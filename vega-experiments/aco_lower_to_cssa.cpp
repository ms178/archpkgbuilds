#include "aco_builder.h"
#include "aco_ir.h"

#include <algorithm>
#include <map>
#include <unordered_map>
#include <vector>

/*
 * This pass lowers SSA-based PHI nodes to parallelcopies at the end of
 * predecessor blocks.
 * The algorithm is based on "Revisiting Out-of-SSA Translation for Correctness,
 * Code Quality, and Efficiency" by Z. Budimlic et al.
 *
 * The paper's algorithm for coalescing is followed, which is to greedily
 * merge variables if they don't interfere.
 *
 * The paper's algorithm for emitting parallelcopies is also followed:
 * 1. build a location-transfer-graph (LTG)
 * 2. emit all copies which are not part of a cycle
 * 3. emit cycles as parallelcopies
 */

namespace aco {
namespace {

typedef std::vector<Temp> merge_set;

struct copy {
   Definition def;
   Operand op;
};

/* single place for “invalid” sentinels */
constexpr uint32_t INVALID_IDX = std::numeric_limits<uint32_t>::max();

/*----------------------------------------------------------------------
 *  node in the Location-Transfer-Graph  (one per outstanding copy)
 *----------------------------------------------------------------------*/
struct ltg_node {
   copy* cp = nullptr;
   uint32_t read_key = INVALID_IDX;
   uint32_t num_uses = 0;

   /* convenient aggregate-style ctor */
   ltg_node(copy* c = nullptr, uint32_t r = INVALID_IDX, uint32_t u = 0)
      : cp(c), read_key(r), num_uses(u)
   {}
};

struct merge_node {
   Operand value = Operand();
   uint32_t index = INVALID_IDX;
   uint32_t defined_at = INVALID_IDX;

   Temp equal_anc_in = Temp();
   Temp equal_anc_out = Temp();
};

struct cssa_ctx {
   Program* program;
   std::vector<std::vector<copy>> parallelcopies;
   std::vector<merge_set> merge_sets;
   std::vector<merge_node> merge_node_table;
};


[[nodiscard]] static inline const merge_node&
get_node_const(const cssa_ctx& ctx, uint32_t id)
{
   assert(id < ctx.merge_node_table.size() && "Query for a non-existent merge node.");
   return ctx.merge_node_table[id];
}

[[nodiscard]] static inline merge_node&
get_node(cssa_ctx& ctx, uint32_t id)
{
   assert(id < ctx.merge_node_table.size() && "Query for a non-existent merge node.");
   return ctx.merge_node_table[id];
}

/* create (virtual) parallelcopies for each phi instruction */
static void
collect_parallelcopies(cssa_ctx& ctx)
{
   ctx.parallelcopies.resize(ctx.program->blocks.size());
   ctx.merge_sets.reserve(ctx.program->blocks.size());

   Builder bld(ctx.program);

   for (Block& block : ctx.program->blocks) {
      for (aco_ptr<Instruction>& phi : block.instructions) {
         if (phi->opcode != aco_opcode::p_phi && phi->opcode != aco_opcode::p_linear_phi)
            break;

         const Definition& def = phi->definitions[0];
         if (!def.isTemp() || def.isKill())
            continue;

         const Block::edge_vec& preds =
            phi->opcode == aco_opcode::p_phi ? block.logical_preds : block.linear_preds;

         const uint32_t set_idx = ctx.merge_sets.size();
         merge_set set;
         set.reserve(phi->operands.size() + 1);

         bool has_preheader_copy = false;

         for (unsigned i = 0; i < phi->operands.size(); ++i) {
            Operand op = phi->operands[i];
            if (op.isUndefined())
               continue;

            if (def.regClass().type() == RegType::sgpr && !op.isTemp()) {
               if (op.isConstant()) {
                  if (ctx.program->gfx_level >= GFX10) {
                     if (op.size() == 1 && !op.isLiteral())
                        continue;
                  } else {
                     bool can_be_inlined = op.isLiteral() || (op.size() == 1 && op.constantValue() >= -16 && op.constantValue() <= 64);
                     if (can_be_inlined)
                        continue;
                  }
               } else {
                  assert(op.isFixed() && op.physReg() == exec);
                  continue;
               }
            }

            assert(preds[i] < ctx.parallelcopies.size());
            if (ctx.parallelcopies[preds[i]].empty())
               ctx.parallelcopies[preds[i]].reserve(4);

            Temp tmp = bld.tmp(def.regClass());
            ctx.parallelcopies[preds[i]].push_back({Definition(tmp), op});

            phi->operands[i] = Operand(tmp);
            phi->operands[i].setKill(true);

            set.emplace_back(tmp);
            if (tmp.id() >= ctx.merge_node_table.size())
               ctx.merge_node_table.resize(tmp.id() + 1);
            ctx.merge_node_table[tmp.id()] = {op, set_idx, preds[i]};

            has_preheader_copy |= (i == 0 && (block.kind & block_kind_loop_header));
         }

         if (set.empty())
            continue;

         if (has_preheader_copy)
            set.emplace(std::next(set.begin()), def.getTemp());
         else if (block.kind & block_kind_loop_header)
            set.emplace(set.begin(), def.getTemp());
         else
            set.emplace_back(def.getTemp());

         if (def.tempId() >= ctx.merge_node_table.size())
            ctx.merge_node_table.resize(def.tempId() + 1);
         ctx.merge_node_table[def.tempId()] = {Operand(def.getTemp()), set_idx, block.index};

         ctx.merge_sets.emplace_back(std::move(set));
      }
   }
}

[[nodiscard]] static inline bool
defined_after(const cssa_ctx& ctx, Temp a, Temp b)
{
   const merge_node& A = get_node_const(ctx, a.id());
   const merge_node& B = get_node_const(ctx, b.id());

   return (A.defined_at == B.defined_at) ? a.id() > b.id() : A.defined_at > B.defined_at;
}

[[nodiscard]] static inline bool
dominates(const cssa_ctx& ctx, Temp a, Temp b)
{
   assert(defined_after(ctx, b, a));
   const Block& parent = ctx.program->blocks[get_node_const(ctx, a.id()).defined_at];
   const Block& child = ctx.program->blocks[get_node_const(ctx, b.id()).defined_at];

   return (b.regClass().type() == RegType::vgpr) ? dominates_logical(parent, child)
                                                 : dominates_linear(parent, child);
}

[[nodiscard]] static inline bool
is_live_out(const cssa_ctx& ctx, Temp var, uint32_t block_idx)
{
   const Block::edge_vec& succs = var.is_linear()
                                     ? ctx.program->blocks[block_idx].linear_succs
                                     : ctx.program->blocks[block_idx].logical_succs;

   return std::any_of(
      succs.begin(), succs.end(),
      [&](unsigned s) { return ctx.program->live.live_in[s].count(var.id()); });
}

[[nodiscard]] static inline bool
intersects(const cssa_ctx& ctx, Temp var, Temp parent)
{
   const merge_node& nv = get_node_const(ctx, var.id());
   const uint32_t blk = nv.defined_at;

   const merge_node& np = get_node_const(ctx, parent.id());
   if (np.defined_at < nv.defined_at && !ctx.program->live.live_in[blk].count(parent.id()))
      return false;

   if (is_live_out(ctx, parent, blk))
      return true;

   bool parent_live = false;
   for (const copy& cp : ctx.parallelcopies[blk]) {
      if (cp.def.getTemp() == var)
         return false;
      if (cp.op.isTemp() && cp.op.getTemp() == parent)
         parent_live = true;
   }
   if (parent_live)
      return true;

   const Block& block = ctx.program->blocks[blk];
   for (auto it = block.instructions.crbegin(); it != block.instructions.crend(); ++it) {
      if (is_phi(it->get()))
         break;

      for (const Definition& d : (*it)->definitions)
         if (d.isTemp() && d.getTemp() == var)
            return false;

      for (const Operand& o : (*it)->operands)
         if (o.isTemp() && o.getTemp() == parent)
            return true;
   }
   return false;
}

[[nodiscard]] static inline bool
interference(cssa_ctx& ctx, Temp var, Temp parent)
{
   assert(var != parent);
   merge_node& nv = get_node(ctx, var.id());
   nv.equal_anc_out = Temp();

   if (nv.index == get_node_const(ctx, parent.id()).index)
      parent = get_node_const(ctx, parent.id()).equal_anc_out;

   for (Temp tmp = parent; tmp != Temp(); tmp = get_node_const(ctx, tmp.id()).equal_anc_in) {

      if (!intersects(ctx, var, tmp))
         continue;

      if (nv.value == get_node_const(ctx, tmp.id()).value) {
         nv.equal_anc_out = tmp;
         return false;
      }
      return true;
   }
   return false;
}

static bool
try_merge_merge_set(cssa_ctx& ctx, Temp dst, merge_set& set_b)
{
   const uint32_t index = get_node_const(ctx, dst.id()).index;
   merge_set& set_a = ctx.merge_sets[index];

   std::vector<Temp> dom_stack;
   merge_set union_set;
   union_set.reserve(set_a.size() + set_b.size());

   size_t i_a = 0, i_b = 0;
   while (i_a < set_a.size() || i_b < set_b.size()) {
      Temp cur;
      if (i_a == set_a.size()) {
         cur = set_b[i_b++];
      } else if (i_b == set_b.size()) {
         cur = set_a[i_a++];
      } else if (defined_after(ctx, set_a[i_a], set_b[i_b])) {
         cur = set_b[i_b++];
      } else {
         cur = set_a[i_a++];
      }

      while (!dom_stack.empty() && !dominates(ctx, dom_stack.back(), cur)) {
         dom_stack.pop_back();
      }

      if (!dom_stack.empty() && interference(ctx, cur, dom_stack.back())) {
         for (Temp t : union_set) {
            get_node(ctx, t.id()).equal_anc_out = Temp();
         }
         return false;
      }

      dom_stack.emplace_back(cur);
      if (cur != dst) {
         union_set.emplace_back(cur);
      }
   }

   for (Temp t : union_set) {
      merge_node& n = get_node(ctx, t.id());
      if (n.equal_anc_in == Temp() ||
          (n.equal_anc_out != Temp() && defined_after(ctx, n.equal_anc_out, n.equal_anc_in))) {
         n.equal_anc_in = n.equal_anc_out;
      }

      n.equal_anc_out = Temp();
      n.index = index;
   }

   set_b.clear();
   ctx.merge_sets[index] = std::move(union_set);
   return true;
}

static bool
try_coalesce_copy(cssa_ctx& ctx, copy cp, uint32_t blk_idx)
{
   if (!cp.op.isTemp() || !cp.op.isKill())
      return false;
   if (cp.op.regClass() != cp.def.regClass())
      return false;

   merge_node& op_node = get_node(ctx, cp.op.tempId());

   if (op_node.defined_at == INVALID_IDX) {
      while (blk_idx != INVALID_IDX &&
             ctx.program->live.live_in[blk_idx].count(cp.op.tempId())) {
         uint32_t idom = (cp.op.regClass().type() == RegType::vgpr)
                            ? ctx.program->blocks[blk_idx].logical_idom
                            : ctx.program->blocks[blk_idx].linear_idom;
         if (idom == blk_idx)
            break;
         blk_idx = idom;
      }
      op_node.defined_at = blk_idx;
      op_node.value = cp.op;
   }

   if (op_node.index == INVALID_IDX) {
      merge_set singleton{cp.op.getTemp()};
      return try_merge_merge_set(ctx, cp.def.getTemp(), singleton);
   }

   const uint32_t def_idx = get_node_const(ctx, cp.def.tempId()).index;
   if (op_node.index == def_idx)
      return true;

   return try_merge_merge_set(ctx, cp.def.getTemp(), ctx.merge_sets[op_node.index]);
}

static void
emit_copies_block(Builder& bld, std::unordered_map<uint32_t, ltg_node>& ltg, RegType type)
{
   RegisterDemand live_changes;
   RegisterDemand reg_demand =
      bld.it->get()->register_demand - get_temp_registers(bld.it->get()) -
      get_live_changes(bld.it->get());

   std::unordered_map<uint32_t, uint32_t> remaining_use_cnt;
   for (const auto& [_, node] : ltg) {
      if (node.cp->op.isTemp()) {
         ++remaining_use_cnt[node.cp->op.tempId()];
      }
   }

   auto is_last_use_and_decrement = [&](Temp t) -> bool {
      auto it = remaining_use_cnt.find(t.id());
      if (it == remaining_use_cnt.end()) {
         return true;
      }
      bool last = it->second == 1;
      --it->second;
      return last;
   };

   std::vector<uint32_t> worklist;
   worklist.reserve(ltg.size());
   std::vector<uint32_t> initial_keys;
   initial_keys.reserve(ltg.size());
   for (const auto& [idx, n] : ltg) {
      if (n.cp->def.regClass().type() == type && n.num_uses == 0) {
         initial_keys.push_back(idx);
      }
   }
   std::sort(initial_keys.begin(), initial_keys.end());
   worklist.insert(worklist.end(), initial_keys.begin(), initial_keys.end());

   auto dec_uses_and_enqueue = [&](uint32_t read_key) {
      if (read_key == INVALID_IDX) {
         return;
      }
      auto it = ltg.find(read_key);
      if (it != ltg.end() && --it->second.num_uses == 0 &&
          it->second.cp->def.regClass().type() == type) {
         worklist.push_back(read_key);
      }
   };

   while (!worklist.empty()) {
      uint32_t write_key = worklist.back();
      worklist.pop_back();

      auto it = ltg.find(write_key);
      assert(it != ltg.end());

      ltg_node node = it->second;
      ltg.erase(it);

      Operand src = node.cp->op;
      if (src.isTemp() && src.isKill()) {
         src.setKill(is_last_use_and_decrement(src.getTemp()));
      }

      dec_uses_and_enqueue(node.read_key);

      Instruction* copy_ins = bld.copy(node.cp->def, src);
      live_changes += get_live_changes(copy_ins);
      copy_ins->register_demand =
         reg_demand + live_changes + get_temp_registers(copy_ins);
   }

   unsigned pc_slots = 0;
   for (const auto& [_, n] : ltg) {
      if (n.cp->def.regClass().type() == type) {
         ++pc_slots;
      }
   }

   if (pc_slots) {
      aco_ptr<Instruction> pc{
         create_instruction(aco_opcode::p_parallelcopy, Format::PSEUDO, pc_slots, pc_slots)};

      unsigned slot = 0;
      std::vector<uint32_t> cycle_keys;
      cycle_keys.reserve(ltg.size());
      for (const auto& [key, val] : ltg) {
          if (val.cp->def.regClass().type() == type) {
              cycle_keys.push_back(key);
          }
      }
      std::sort(cycle_keys.begin(), cycle_keys.end());

      for (uint32_t key : cycle_keys) {
         auto it = ltg.find(key);
         if (it == ltg.end()) continue;

         pc->definitions[slot] = it->second.cp->def;

         Operand src = it->second.cp->op;
         if (src.isTemp() && src.isKill()) {
            src.setKill(is_last_use_and_decrement(src.getTemp()));
         }
         pc->operands[slot] = src;

         dec_uses_and_enqueue(it->second.read_key);
         ltg.erase(it);
         ++slot;
      }
      assert(slot == pc_slots);

      live_changes += get_live_changes(pc.get());
      pc->register_demand = reg_demand + live_changes + get_temp_registers(pc.get());
      bld.insert(std::move(pc));
   }

   if (live_changes.sgpr || live_changes.vgpr) {
      for (auto it = bld.it; it != bld.instructions->end(); ++it) {
         it->get()->register_demand += live_changes;
      }
   }
}

/* either emits or coalesces all parallel-copies in each block and
 * rewrites φ-operands accordingly.                                      */
static void
emit_parallelcopies(cssa_ctx& ctx)
{
   std::unordered_map<uint32_t, Operand> rename_map;

   for (int blk_idx = int(ctx.program->blocks.size()) - 1; blk_idx >= 0; --blk_idx) {
      if (ctx.parallelcopies[blk_idx].empty()) {
         continue;
      }

      std::vector<bool> coalesced(ctx.parallelcopies[blk_idx].size(), false);

      /* ───────────────────────── Stage 1 : coalesce & fix liveness ─────────────────────── */
      for (int reg_type_pass = 0; reg_type_pass < 2; ++reg_type_pass) {
         RegType current_type = (reg_type_pass == 0) ? RegType::vgpr : RegType::sgpr;

         for (unsigned n = 0; n < ctx.parallelcopies[blk_idx].size(); ++n) {
            copy& cp = ctx.parallelcopies[blk_idx][n];
            if (cp.def.regClass().type() != current_type || coalesced[n]) {
               continue;
            }

            if (!try_coalesce_copy(ctx, cp, blk_idx)) {
               continue;
            }

            coalesced[n] = true;
            assert(cp.op.isTemp() && cp.op.isKill());

            for (copy& oth : ctx.parallelcopies[blk_idx]) {
               if (&oth != &cp && oth.op.isTemp() && oth.op.getTemp() == cp.op.getTemp()) {
                  oth.op.setKill(false);
                  oth.op.setFirstKill(false);
               }
            }
            rename_map.emplace(cp.def.tempId(), cp.op);
         }
      }

      /* ───────────────────────── Stage 2 : build LTG (unique key = dst Temp ID) ────────── */
      std::unordered_map<uint32_t, ltg_node> ltg;
      bool has_vgpr = false, has_sgpr = false;

      for (unsigned n = 0; n < ctx.parallelcopies[blk_idx].size(); ++n) {
         if (coalesced[n]) {
            continue;
         }

         copy& cp = ctx.parallelcopies[blk_idx][n];

         if (cp.op.isTemp()) {
            auto it = rename_map.find(cp.op.tempId());
            while (it != rename_map.end()) {
               cp.op = it->second;
               if (!cp.op.isTemp())
                  break;
               it = rename_map.find(cp.op.tempId());
            }
         }

         if (cp.op.isTemp()) {
            bool live_out = is_live_out(ctx, cp.op.getTemp(), blk_idx);
            bool keep_kill = cp.op.isKill() && !live_out;
            cp.op.setKill(keep_kill);
            cp.op.setFirstKill(keep_kill);
         }

         uint32_t dst_key = cp.def.tempId();
         uint32_t read_key = cp.op.isTemp() ? cp.op.tempId() : INVALID_IDX;

         ltg.emplace(dst_key, ltg_node(&cp, read_key));

         bool is_vgpr = cp.def.regClass().type() == RegType::vgpr;
         has_vgpr |= is_vgpr;
         has_sgpr |= !is_vgpr;
      }

      for (auto& [key, node] : ltg) {
         if (node.read_key != INVALID_IDX) {
            auto it = ltg.find(node.read_key);
            if (it != ltg.end()) {
               ++it->second.num_uses;
            }
         }
      }

      /* ───────────────────────── Stage 3 : emit copies ─────────────────────── */
      Builder bld(ctx.program);
      Block& blk = ctx.program->blocks[blk_idx];

      if (has_vgpr) {
         auto lg_end = std::find_if(
            blk.instructions.rbegin(), blk.instructions.rend(),
            [](const aco_ptr<Instruction>& ins) { return ins->opcode == aco_opcode::p_logical_end; });
         auto insert_pt = (lg_end == blk.instructions.rend()) ? std::prev(blk.instructions.end())
                                                              : std::prev(lg_end.base());
         bld.reset(&blk.instructions, insert_pt);
         emit_copies_block(bld, ltg, RegType::vgpr);
      }

      if (has_sgpr) {
         bld.reset(&blk.instructions, std::prev(blk.instructions.end()));
         emit_copies_block(bld, ltg, RegType::sgpr);
      }

      assert(ltg.empty() && "emit_copies_block must consume all LTG nodes");
   }

   /* ───────────────────────── Rename φ operands & update demand ─────────────────────── */
   RegisterDemand programme_demand;
   for (Block& blk : ctx.program->blocks) {
      for (aco_ptr<Instruction>& phi : blk.instructions) {
         if (phi->opcode != aco_opcode::p_phi && phi->opcode != aco_opcode::p_linear_phi) {
            break;
         }

         for (Operand& op : phi->operands) {
            if (op.isTemp()) {
               auto it = rename_map.find(op.tempId());
               while (it != rename_map.end()) {
                  op = it->second;
                  if (!op.isTemp())
                     break;
                  it = rename_map.find(op.tempId());
               }
            }
         }
      }

      blk.register_demand = blk.live_in_demand;
      for (const auto& ins : blk.instructions) {
         blk.register_demand.update(ins->register_demand);
      }

      programme_demand.update(blk.register_demand);
   }
   update_vgpr_sgpr_demand(ctx.program, programme_demand);
}

} // namespace

void
lower_to_cssa(Program* program)
{
   reindex_ssa(program);

   cssa_ctx ctx{program};
   collect_parallelcopies(ctx);
   emit_parallelcopies(ctx);

   if (!validate_live_vars(program)) {
      std::abort();
   }
}

} // namespace aco
