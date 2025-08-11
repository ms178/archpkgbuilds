/*
 * Copyright © 2019 Valve Corporation
 *
 * SPDX-License-Identifier: MIT
 */

#include "aco_builder.h"
#include "aco_ir.h"

#include <algorithm>
#include <map>
#include <unordered_map>
#include <vector>
#include <bitset> // for possible future validity tracking (kept for parity)
#include <limits>
#include <cassert>

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

/* -------------------------------
 * Helper accessors & rename map
 * ------------------------------- */

/* get_node_const - OOB assert in debug; sentinel in release to avoid UB */
[[nodiscard]] static inline const merge_node&
get_node_const(const cssa_ctx& ctx, uint32_t id)
{
   assert(id < ctx.merge_node_table.size() && "merge_node_table OOB: likely undercount in collect_parallelcopies()");
   if (id >= ctx.merge_node_table.size()) {
      static const merge_node sentinel{Operand(), INVALID_IDX, INVALID_IDX, Temp(), Temp()};
      return sentinel;
   }
   return ctx.merge_node_table[id];
}

/* get_node - ensures table is large enough and returns mutable ref */
[[nodiscard]] static inline merge_node&
get_node(cssa_ctx& ctx, uint32_t id)
{
   if (id >= ctx.merge_node_table.size()) {
      /* resize to accommodate: keep semantics stable by initializing with defaults */
      ctx.merge_node_table.resize(id + 1, {Operand(), INVALID_IDX, INVALID_IDX, Temp(), Temp()});
   }
   return ctx.merge_node_table[id];
}

/* Sparse rename map: map from temp id -> resolved Operand.
 * We use unordered_map to avoid allocating a huge vector for programs with sparse/sparse-high temp ids.
 */
using rename_map_t = std::unordered_map<uint32_t, Operand>;

/* resolve_rename: resolves a temp id to its final operand using rename_map and
 * performs path compression where possible. Returns either a non-temp Operand
 * (constant/fixed) or an Operand with a temp that has no further rename.
 */
static Operand
resolve_rename(rename_map_t& rename_map, uint32_t id)
{
   auto it = rename_map.find(id);
   if (it == rename_map.end())
      return Operand();

   Operand cur = it->second;
   if (!cur.isTemp())
      return cur;

   // Walk chain, collecting visited ids for compression
   std::vector<uint32_t> visited;
   visited.reserve(8);
   uint32_t cur_id = cur.getTemp().id();

   while (true) {
      auto it2 = rename_map.find(cur_id);
      if (it2 == rename_map.end()) {
         // reached final operand
         break;
      }
      // avoid pathological infinite loops; if loops exist, break
      if (visited.size() > 2048) {
         break;
      }
      visited.push_back(cur_id);
      Operand next = it2->second;
      if (!next.isTemp()) {
         cur = next;
         break;
      }
      cur = next;
      cur_id = cur.getTemp().id();
   }

   // Path compression: point all visited entries to final operand `cur`.
   for (uint32_t v : visited) {
      rename_map[v] = cur;
   }
   // Also compress the original id
   rename_map[id] = cur;
   return cur;
}

/* Convenience: check if rename exists */
static inline bool
rename_has(const rename_map_t& rename_map, uint32_t id)
{
   return rename_map.find(id) != rename_map.end();
}

/* Set rename mapping */
static inline void
rename_set(rename_map_t& rename_map, uint32_t id, const Operand& op)
{
   // Avoid storing trivial mapping to itself; but op could equal the same temp id -> we store anyway if op isn't same temp.
   if (op.isTemp() && op.getTemp().id() == id)
      return;
   rename_map[id] = op;
}

/* -------------------------------
 * collect_parallelcopies
 * ------------------------------- */

/* create (virtual) parallelcopies for each phi instruction */
static void
collect_parallelcopies(cssa_ctx& ctx)
{
   ctx.parallelcopies.resize(ctx.program->blocks.size());
   ctx.merge_sets.reserve(ctx.program->blocks.size());

   // Pre-count the number of extra temps needed for accurate preallocation
   uint32_t extra_temps = 0;
   uint32_t extra_defs = 0;
   for (const Block& block : ctx.program->blocks) {
      // iterate only over leading phi instructions (stop at first non-phi)
      for (const aco_ptr<Instruction>& phi : block.instructions) {
         if (phi->opcode != aco_opcode::p_phi && phi->opcode != aco_opcode::p_linear_phi) {
            break;
         }

         const Definition& def = phi->definitions[0];
         if (!def.isTemp() || def.isKill())
            continue;

         extra_defs++;

         const Block::edge_vec& preds =
            phi->opcode == aco_opcode::p_phi ? block.logical_preds : block.linear_preds;

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

            extra_temps++;
         }
      }
   }

   // Safe upper bound: existing temps + extra temps for copies + extra for defs + padding for safety
   uint32_t num_temps = ctx.program->temp_rc.size();
   uint32_t max_temp_id = num_temps + extra_temps + extra_defs + ctx.program->blocks.size() * 2u;
   ctx.merge_node_table.reserve(max_temp_id + 1);
   ctx.merge_node_table.resize(max_temp_id + 1, {Operand(), INVALID_IDX, INVALID_IDX, Temp(), Temp()});

   Builder bld(ctx.program);

   for (Block& block : ctx.program->blocks) {
      // iterate only over leading phi instructions (stop at first non-phi)
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

            // preds[i] must exist; assert to catch structural IR issues
            assert(preds.size() > i);
            assert(preds[i] < ctx.parallelcopies.size());
            if (ctx.parallelcopies[preds[i]].empty())
               ctx.parallelcopies[preds[i]].reserve(4);

            Temp tmp = bld.tmp(def.regClass());
            ctx.parallelcopies[preds[i]].push_back({Definition(tmp), op});

            phi->operands[i] = Operand(tmp);
            phi->operands[i].setKill(true);

            set.emplace_back(tmp);

            // ensure merge_node_table size covers tmp.id()
            if (tmp.id() >= ctx.merge_node_table.size()) {
               ctx.merge_node_table.resize(tmp.id() + 1, {Operand(), INVALID_IDX, INVALID_IDX, Temp(), Temp()});
            }
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

         // ensure merge_node_table size covers def.tempId()
         if (def.tempId() >= ctx.merge_node_table.size()) {
            ctx.merge_node_table.resize(def.tempId() + 1, {Operand(), INVALID_IDX, INVALID_IDX, Temp(), Temp()});
         }
         ctx.merge_node_table[def.tempId()] = {Operand(def.getTemp()), set_idx, block.index};

         ctx.merge_sets.emplace_back(std::move(set));
      }
   }
}

/* -------------------------------
 * utility predicates
 * ------------------------------- */

[[nodiscard]] [[gnu::always_inline]] static inline bool
defined_after(const cssa_ctx& ctx, Temp a, Temp b)
{
   const merge_node& A = get_node_const(ctx, a.id());
   const merge_node& B = get_node_const(ctx, b.id());

   // If either node had invalid defined_at we conservatively consider id ordering
   if (A.defined_at == INVALID_IDX || B.defined_at == INVALID_IDX)
      return a.id() > b.id();

   return (A.defined_at == B.defined_at) ? a.id() > b.id() : A.defined_at > B.defined_at;
}

[[nodiscard]] [[gnu::always_inline]] static inline bool
dominates(const cssa_ctx& ctx, Temp a, Temp b)
{
   assert(defined_after(ctx, b, a));
   const merge_node& A = get_node_const(ctx, a.id());
   const merge_node& B = get_node_const(ctx, b.id());
   if (A.defined_at == INVALID_IDX || B.defined_at == INVALID_IDX)
      return false;

   const Block& parent = ctx.program->blocks[A.defined_at];
   const Block& child = ctx.program->blocks[B.defined_at];

   return (b.regClass().type() == RegType::vgpr) ? dominates_logical(parent, child)
                                                 : dominates_linear(parent, child);
}

[[nodiscard]] static inline bool
is_live_out(const cssa_ctx& ctx, Temp var, uint32_t block_idx)
{
   const Block::edge_vec& succs = var.is_linear()
                                     ? ctx.program->blocks[block_idx].linear_succs
                                     : ctx.program->blocks[block_idx].logical_succs;

   for (unsigned s : succs) {
      if (ctx.program->live.live_in[s].count(var.id()))
         return true;
   }
   return false;
}

[[nodiscard]] [[gnu::always_inline]] static inline bool
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

[[nodiscard]] [[gnu::always_inline]] static inline bool
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

/* emit_copies_block: emits single copies and a parallelcopy for cycles.
 * - ltg is block-local vector indexed by local temp ids <= local_max_temp
 * - ltg_valid marks which entries are populated (since we allocate tight).
 */
static void
emit_copies_block(Builder& bld,
                  std::vector<ltg_node>& ltg,
                  std::vector<char>& ltg_valid,
                  std::vector<uint32_t>& active_keys,
                  RegType type,
                  rename_map_t& rename_map)
{
   RegisterDemand live_changes;
   RegisterDemand reg_demand =
      bld.it->get()->register_demand - get_temp_registers(bld.it->get()) -
      get_live_changes(bld.it->get());

   std::unordered_map<uint32_t, uint32_t> remaining_use_cnt;
   for (uint32_t key : active_keys) {
      if (key >= ltg.size()) continue;
      if (!ltg_valid[key]) continue;
      const ltg_node& node = ltg[key];
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
   worklist.reserve(active_keys.size());
   std::vector<uint32_t> initial_keys;
   initial_keys.reserve(active_keys.size());
   for (uint32_t idx : active_keys) {
      if (idx >= ltg.size()) continue;
      if (!ltg_valid[idx]) continue;
      const ltg_node& n = ltg[idx];
      if (n.cp->def.regClass().type() == type && n.num_uses == 0) {
         initial_keys.push_back(idx);
      }
   }
   std::sort(initial_keys.begin(), initial_keys.end()); // Retain for determinism
   worklist.insert(worklist.end(), initial_keys.begin(), initial_keys.end());

   auto dec_uses_and_enqueue = [&](uint32_t read_key) {
      if (read_key == INVALID_IDX) {
         return;
      }
      if (read_key >= ltg.size() || !ltg_valid[read_key]) {
         return;
      }
      ltg_node& node = ltg[read_key];
      if (node.num_uses > 0) {
         --node.num_uses;
         if (node.num_uses == 0 && node.cp->def.regClass().type() == type) {
            worklist.push_back(read_key);
         }
      }
   };

   while (!worklist.empty()) {
      uint32_t write_key = worklist.back();
      worklist.pop_back();

      if (write_key >= ltg.size() || !ltg_valid[write_key]) continue;

      ltg_node node = ltg[write_key];
      // erase
      ltg_valid[write_key] = 0;
      ltg[write_key] = ltg_node();

      Operand src = node.cp->op;
      if (src.isTemp() && src.isKill()) {
         src.setKill(is_last_use_and_decrement(src.getTemp()));
      }

      dec_uses_and_enqueue(node.read_key);

      // If src is a temp and has a rename, resolve it (path compressed)
      if (src.isTemp()) {
         uint32_t rid = src.getTemp().id();
         if (rename_has(rename_map, rid)) {
            Operand resolved = resolve_rename(rename_map, rid);
            if (!resolved.isUndefined())
               src = resolved;
         }
      }

      Instruction* copy_ins = bld.copy(node.cp->def, src);
      live_changes += get_live_changes(copy_ins);
      copy_ins->register_demand =
         reg_demand + live_changes + get_temp_registers(copy_ins);
   }

   unsigned pc_slots = 0;
   for (uint32_t key : active_keys) {
      if (key < ltg.size() && ltg_valid[key] && ltg[key].cp != nullptr && ltg[key].cp->def.regClass().type() == type) {
         ++pc_slots;
      }
   }

   if (pc_slots) {
      aco_ptr<Instruction> pc{
         create_instruction(aco_opcode::p_parallelcopy, Format::PSEUDO, pc_slots, pc_slots)};

      unsigned slot = 0;
      std::vector<uint32_t> cycle_keys;
      cycle_keys.reserve(active_keys.size());
      for (uint32_t key : active_keys) {
         if (key < ltg.size() && ltg_valid[key]) {
            cycle_keys.push_back(key);
         }
      }
      std::sort(cycle_keys.begin(), cycle_keys.end()); // Retain determinism

      for (uint32_t key : cycle_keys) {
         if (key >= ltg.size() || !ltg_valid[key]) continue;

         ltg_node& node = ltg[key];
         if (node.cp == nullptr) continue;

         pc->definitions[slot] = node.cp->def;

         Operand src = node.cp->op;

         // Resolve rename mapping (path compressed)
         if (src.isTemp()) {
            uint32_t rid = src.getTemp().id();
            if (rename_has(rename_map, rid)) {
               Operand resolved = resolve_rename(rename_map, rid);
               if (!resolved.isUndefined())
                  src = resolved;
            }
         }

         if (src.isTemp() && src.isKill()) {
            src.setKill(is_last_use_and_decrement(src.getTemp()));
         }
         pc->operands[slot] = src;

         dec_uses_and_enqueue(node.read_key);
         // erase
         ltg_valid[key] = 0;
         ltg[key] = ltg_node();
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
   const uint32_t max_temp_id = ctx.merge_node_table.size() ? (ctx.merge_node_table.size() - 1) : 0;

   // Use sparse rename_map to avoid allocation of huge vector when temp ids are sparse.
   rename_map_t rename_map;

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

            // record rename as sparse mapping
            if (cp.def.tempId() <= max_temp_id) {
               rename_set(rename_map, cp.def.tempId(), cp.op);
            }
         }
      }

      /* ───────────────────────── Stage 2 : build LTG (unique key = dst Temp ID) ────────── */
      // compute a block-local max temp id to allocate tight ltg arrays
      uint32_t local_max_temp = 0;
      for (unsigned n = 0; n < ctx.parallelcopies[blk_idx].size(); ++n) {
         if (coalesced[n]) continue;
         const copy& cp = ctx.parallelcopies[blk_idx][n];
         uint32_t dst = cp.def.tempId();
         local_max_temp = std::max(local_max_temp, dst);
         if (cp.op.isTemp()) {
            local_max_temp = std::max(local_max_temp, cp.op.tempId());
         }
      }

      // allocate tight LTG and validity bitmap
      std::vector<ltg_node> ltg(local_max_temp + 1, ltg_node());
      std::vector<char> ltg_valid(local_max_temp + 1, 0);
      std::vector<uint32_t> active_keys;
      active_keys.reserve(ctx.parallelcopies[blk_idx].size());
      bool has_vgpr = false, has_sgpr = false;

      for (unsigned n = 0; n < ctx.parallelcopies[blk_idx].size(); ++n) {
         if (coalesced[n]) {
            continue;
         }

         copy& cp = ctx.parallelcopies[blk_idx][n];

         if (cp.op.isTemp()) {
            uint32_t id = cp.op.tempId();
            if (rename_has(rename_map, id)) {
               Operand resolved = resolve_rename(rename_map, id);
               if (!resolved.isUndefined())
                  cp.op = resolved;
               if (!cp.op.isTemp()) {
                  // become immediate/fixed; we keep as-is
               } else {
                  id = cp.op.tempId();
               }
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

         if (dst_key > local_max_temp) continue; // safety bound: should not happen
         ltg[dst_key] = ltg_node(&cp, read_key);
         ltg_valid[dst_key] = 1;
         active_keys.push_back(dst_key);

         bool is_vgpr = cp.def.regClass().type() == RegType::vgpr;
         has_vgpr |= is_vgpr;
         has_sgpr |= !is_vgpr;
      }

      for (uint32_t key : active_keys) {
         if (key < ltg.size() && ltg_valid[key]) {
            ltg_node& node = ltg[key];
            if (node.read_key != INVALID_IDX && node.read_key < ltg.size() && ltg_valid[node.read_key] && ltg[node.read_key].cp != nullptr) {
               ++ltg[node.read_key].num_uses;
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
         emit_copies_block(bld, ltg, ltg_valid, active_keys, RegType::vgpr, rename_map);
      }

      if (has_sgpr) {
         bld.reset(&blk.instructions, std::prev(blk.instructions.end()));
         emit_copies_block(bld, ltg, ltg_valid, active_keys, RegType::sgpr, rename_map);
      }

      // Assert ltg consumed (all sentinels)
      for (uint32_t key : active_keys) {
         if (key < ltg.size() && ltg_valid[key]) {
            // If something remains, it's a logic bug: assert in debug.
            assert(!"emit_copies_block must consume all LTG nodes");
            break;
         }
      }
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
               uint32_t id = op.tempId();
               // Use resolve_rename if mapping exists
               if (rename_has(rename_map, id)) {
                  Operand resolved = resolve_rename(rename_map, id);
                  if (!resolved.isUndefined())
                     op = resolved;
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
