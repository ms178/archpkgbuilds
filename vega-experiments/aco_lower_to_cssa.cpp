/*
 * Copyright © 2019 Valve Corporation
 *
 * SPDX-License-Identifier: MIT
 */

#include "aco_builder.h"
#include "aco_ir.h"

#include <algorithm>
#include <vector>
#include <unordered_map>
#include <limits>
#include <cassert>
#include <cstring>

namespace aco {
namespace {

typedef std::vector<Temp> merge_set;

struct copy {
   Definition def;
   Operand op;
};

/* Single place for invalid sentinels */
constexpr uint32_t INVALID_IDX = std::numeric_limits<uint32_t>::max();
constexpr uint32_t MAX_BLOCK_COUNT = 65536u;  /* Reasonable limit for shader blocks */
constexpr uint32_t MAX_TEMP_COUNT = 1048576u;  /* Reasonable limit for temp IDs */

/*----------------------------------------------------------------------
 * Node in the Location-Transfer-Graph (one per outstanding copy)
 *----------------------------------------------------------------------*/
struct alignas(8) ltg_node {
   copy* cp;
   uint32_t read_key;
   uint32_t num_uses;

   ltg_node() noexcept
      : cp(nullptr), read_key(INVALID_IDX), num_uses(0u)
   {}

   ltg_node(copy* c, uint32_t r, uint32_t u) noexcept
      : cp(c), read_key(r), num_uses(u)
   {}
};

struct alignas(32) merge_node {
   Operand value;
   uint32_t index;
   uint32_t defined_at;
   Temp equal_anc_in;
   Temp equal_anc_out;

   merge_node() noexcept
      : value(Operand()), index(INVALID_IDX), defined_at(INVALID_IDX),
        equal_anc_in(Temp()), equal_anc_out(Temp())
   {}
};

struct cssa_ctx {
   Program* program;
   std::vector<std::vector<copy>> parallelcopies;
   std::vector<merge_set> merge_sets;
   std::vector<merge_node> merge_node_table;

   /* Thread-local cache for live-out queries */
   struct live_out_cache_t {
      struct entry {
         uint32_t var_id;
         uint32_t block_idx;
         bool result;
      };
      static constexpr size_t CACHE_SIZE = 256u;
      entry entries[CACHE_SIZE];
      uint32_t next_slot;

      live_out_cache_t() noexcept : next_slot(0u) {
         std::memset(entries, 0, sizeof(entries));
      }

      bool lookup(uint32_t var_id, uint32_t block_idx, bool& result) const noexcept {
         /* Simple linear probe with wraparound */
         const uint32_t hash = ((var_id * 2654435761u) ^ (block_idx * 1103515245u)) % CACHE_SIZE;
         for (uint32_t i = 0u; i < 4u; ++i) {
            const uint32_t slot = (hash + i) % CACHE_SIZE;
            const entry& e = entries[slot];
            if (e.var_id == var_id && e.block_idx == block_idx && e.var_id != 0u) {
               result = e.result;
               return true;
            }
         }
         return false;
      }

      void insert(uint32_t var_id, uint32_t block_idx, bool result) noexcept {
         if (var_id == 0u) {
            return;
         }
         const uint32_t hash = ((var_id * 2654435761u) ^ (block_idx * 1103515245u)) % CACHE_SIZE;
         const uint32_t slot = hash % CACHE_SIZE;
         entries[slot] = {var_id, block_idx, result};
      }
   };

   mutable live_out_cache_t live_out_cache;
};

/* -------------------------------
 * Helper accessors with bounds checking
 * ------------------------------- */

[[nodiscard]] static inline const merge_node&
get_node_const(const cssa_ctx& ctx, uint32_t id) noexcept
{
   static const merge_node sentinel;
   if (id >= ctx.merge_node_table.size()) {
      assert(!"merge_node_table out of bounds access");
      return sentinel;
   }
   return ctx.merge_node_table[id];
}

[[nodiscard]] static inline merge_node&
get_node(cssa_ctx& ctx, uint32_t id)
{
   if (id >= MAX_TEMP_COUNT) {
      assert(!"Temp ID exceeds maximum allowed");
      static merge_node sentinel;
      return sentinel;
   }

   if (id >= ctx.merge_node_table.size()) {
      /* Grow by power of 2 */
      size_t new_size = ctx.merge_node_table.empty() ? 64u : ctx.merge_node_table.size();
      while (new_size <= id && new_size < MAX_TEMP_COUNT) {
         new_size = new_size * 2u;
      }
      new_size = std::min<size_t>(new_size, MAX_TEMP_COUNT);
      ctx.merge_node_table.resize(new_size);
   }

   return ctx.merge_node_table[id];
}

/* -------------------------------
 * Optimized rename map with path compression
 * ------------------------------- */

class rename_map_t {
private:
   std::unordered_map<uint32_t, Operand> map_;
   mutable std::unordered_map<uint32_t, Operand> cache_;
   static constexpr uint32_t MAX_PATH_LENGTH = 256u;

public:
   [[nodiscard]] bool has(uint32_t id) const noexcept {
      return map_.find(id) != map_.end();
   }

   void set(uint32_t id, const Operand& op) {
      /* Don't store self-mappings */
      if (op.isTemp() && op.getTemp().id() == id) {
         return;
      }
      map_[id] = op;
      cache_.erase(id);
   }

   [[nodiscard]] Operand resolve(uint32_t id) const {
      /* Check cache first */
      auto cache_it = cache_.find(id);
      if (cache_it != cache_.end()) {
         return cache_it->second;
      }

      auto it = map_.find(id);
      if (it == map_.end()) {
         return Operand();
      }

      Operand cur = it->second;
      if (!cur.isTemp()) {
         cache_[id] = cur;
         return cur;
      }

      /* Path compression with cycle detection using Floyd's algorithm */
      std::vector<uint32_t> path;
      path.reserve(16u);

      uint32_t slow = id;
      uint32_t fast = cur.getTemp().id();
      bool has_cycle = false;

      /* Detect cycle */
      for (uint32_t steps = 0u; steps < MAX_PATH_LENGTH; ++steps) {
         auto slow_it = map_.find(slow);
         if (slow_it == map_.end() || !slow_it->second.isTemp()) {
            break;
         }
         slow = slow_it->second.getTemp().id();

         /* Fast pointer moves twice */
         for (int i = 0; i < 2; ++i) {
            auto fast_it = map_.find(fast);
            if (fast_it == map_.end() || !fast_it->second.isTemp()) {
               goto end_cycle_detection;
            }
            fast = fast_it->second.getTemp().id();
         }

         if (slow == fast) {
            has_cycle = true;
            break;
         }
      }
      end_cycle_detection:

      if (has_cycle) {
         /* Return without caching to avoid infinite loop */
         return cur;
      }

      /* Walk path and compress */
      uint32_t cur_id = cur.getTemp().id();
      path.push_back(id);

      for (uint32_t steps = 0u; steps < MAX_PATH_LENGTH; ++steps) {
         auto next_it = map_.find(cur_id);
         if (next_it == map_.end()) {
            break;
         }

         path.push_back(cur_id);

         if (!next_it->second.isTemp()) {
            cur = next_it->second;
            break;
         }

         cur = next_it->second;
         cur_id = cur.getTemp().id();
      }

      /* Update cache for all nodes in path */
      for (uint32_t node_id : path) {
         cache_[node_id] = cur;
      }

      return cur;
   }

   void clear() noexcept {
      map_.clear();
      cache_.clear();
   }
};

/* -------------------------------
 * Parallelcopy collection
 * ------------------------------- */

static void
collect_parallelcopies(cssa_ctx& ctx)
{
   const size_t num_blocks = ctx.program->blocks.size();

   if (num_blocks > MAX_BLOCK_COUNT) {
      assert(!"Block count exceeds maximum");
      return;
   }

   ctx.parallelcopies.resize(num_blocks);
   ctx.merge_sets.reserve(num_blocks * 2u);

   /* Count temps needed */
   uint32_t max_temp_id = ctx.program->temp_rc.size();
   uint32_t extra_temps = 0u;

   for (const Block& block : ctx.program->blocks) {
      for (const aco_ptr<Instruction>& phi : block.instructions) {
         if (phi->opcode != aco_opcode::p_phi &&
             phi->opcode != aco_opcode::p_linear_phi) {
            break;
         }

         const Definition& def = phi->definitions[0];
         if (!def.isTemp() || def.isKill()) {
            continue;
         }

         for (const Operand& op : phi->operands) {
            if (!op.isUndefined()) {
               if (extra_temps < MAX_TEMP_COUNT - 1u) {
                  extra_temps++;
               }
            }
         }
      }
   }

   /* Allocate merge node table with bounds checking */
   const uint32_t total_temps = std::min<uint32_t>(
      max_temp_id + extra_temps + 256u,
      MAX_TEMP_COUNT
   );

   size_t table_size = 64u;
   while (table_size < total_temps && table_size < MAX_TEMP_COUNT) {
      table_size = table_size * 2u;
   }
   table_size = std::min<size_t>(table_size, MAX_TEMP_COUNT);

   ctx.merge_node_table.reserve(table_size);
   ctx.merge_node_table.resize(table_size);

   Builder bld(ctx.program);

   /* Create parallel copies */
   for (Block& block : ctx.program->blocks) {
      const uint32_t block_idx = block.index;

      if (block_idx >= MAX_BLOCK_COUNT) {
         continue;
      }

      for (aco_ptr<Instruction>& phi : block.instructions) {
         if (phi->opcode != aco_opcode::p_phi &&
             phi->opcode != aco_opcode::p_linear_phi) {
            break;
         }

         const Definition& def = phi->definitions[0];
         if (!def.isTemp() || def.isKill()) {
            continue;
         }

         const Block::edge_vec& preds = (phi->opcode == aco_opcode::p_phi)
                                       ? block.logical_preds
                                       : block.linear_preds;

         const uint32_t set_idx = ctx.merge_sets.size();
         merge_set set;
         set.reserve(phi->operands.size() + 1u);

         bool has_preheader_copy = false;
         const bool is_loop_header = (block.kind & block_kind_loop_header) != 0u;

         for (unsigned i = 0u; i < phi->operands.size(); ++i) {
            Operand op = phi->operands[i];
            if (op.isUndefined()) {
               continue;
            }

            /* Check if constant can be inlined */
            if (def.regClass().type() == RegType::sgpr && !op.isTemp()) {
               if (op.isConstant()) {
                  if (ctx.program->gfx_level >= GFX10) {
                     if (op.size() == 1u && !op.isLiteral()) {
                        continue;
                     }
                  } else {
                     const int64_t val = op.constantValue();
                     const bool can_inline = op.isLiteral() ||
                                            (op.size() == 1u && val >= -16 && val <= 64);
                     if (can_inline) {
                        continue;
                     }
                  }
               } else {
                  assert(op.isFixed() && op.physReg() == exec);
                  continue;
               }
            }

            /* Validate predecessor index */
            if (i >= preds.size()) {
               assert(!"Phi operand count mismatch");
               continue;
            }

            const uint32_t pred_idx = preds[i];
            if (pred_idx >= ctx.parallelcopies.size()) {
               assert(!"Invalid predecessor index");
               continue;
            }

            /* Create copy */
            if (ctx.parallelcopies[pred_idx].empty()) {
               ctx.parallelcopies[pred_idx].reserve(8u);
            }

            Temp tmp = bld.tmp(def.regClass());

            /* Validate temp ID */
            if (tmp.id() >= MAX_TEMP_COUNT) {
               assert(!"Temp ID exceeds maximum");
               continue;
            }

            ctx.parallelcopies[pred_idx].push_back({Definition(tmp), op});

            phi->operands[i] = Operand(tmp);
            phi->operands[i].setKill(true);

            set.emplace_back(tmp);

            /* Store merge node */
            merge_node& node = get_node(ctx, tmp.id());
            node.value = op;
            node.index = set_idx;
            node.defined_at = pred_idx;

            has_preheader_copy |= (i == 0u && is_loop_header);
         }

         if (set.empty()) {
            continue;
         }

         /* Insert definition temp */
         if (has_preheader_copy) {
            set.emplace(std::next(set.begin()), def.getTemp());
         } else if (is_loop_header) {
            set.emplace(set.begin(), def.getTemp());
         } else {
            set.emplace_back(def.getTemp());
         }

         /* Store merge node for definition */
         if (def.tempId() < MAX_TEMP_COUNT) {
            merge_node& node = get_node(ctx, def.tempId());
            node.value = Operand(def.getTemp());
            node.index = set_idx;
            node.defined_at = block_idx;
         }

         ctx.merge_sets.emplace_back(std::move(set));
      }
   }
}

/* -------------------------------
 * Utility predicates
 * ------------------------------- */

[[nodiscard]] static inline bool
defined_after(const cssa_ctx& ctx, Temp a, Temp b) noexcept
{
   if (a.id() >= ctx.merge_node_table.size() ||
       b.id() >= ctx.merge_node_table.size()) {
      return a.id() > b.id();
   }

   const merge_node& node_a = ctx.merge_node_table[a.id()];
   const merge_node& node_b = ctx.merge_node_table[b.id()];

   if (node_a.defined_at == INVALID_IDX || node_b.defined_at == INVALID_IDX) {
      return a.id() > b.id();
   }

   return (node_a.defined_at == node_b.defined_at)
          ? (a.id() > b.id())
          : (node_a.defined_at > node_b.defined_at);
}

[[nodiscard]] static inline bool
dominates(const cssa_ctx& ctx, Temp a, Temp b) noexcept
{
   assert(defined_after(ctx, b, a));

   const merge_node& node_a = get_node_const(ctx, a.id());
   const merge_node& node_b = get_node_const(ctx, b.id());

   if (node_a.defined_at == INVALID_IDX ||
       node_b.defined_at == INVALID_IDX ||
       node_a.defined_at >= ctx.program->blocks.size() ||
       node_b.defined_at >= ctx.program->blocks.size()) {
      return false;
   }

   const Block& parent = ctx.program->blocks[node_a.defined_at];
   const Block& child = ctx.program->blocks[node_b.defined_at];

   return (b.regClass().type() == RegType::vgpr)
          ? dominates_logical(parent, child)
          : dominates_linear(parent, child);
}

[[nodiscard]] static bool
is_live_out(const cssa_ctx& ctx, Temp var, uint32_t block_idx) noexcept
{
   if (block_idx >= ctx.program->blocks.size()) {
      return false;
   }

   /* Check cache */
   bool cached_result;
   if (ctx.live_out_cache.lookup(var.id(), block_idx, cached_result)) {
      return cached_result;
   }

   /* Compute result */
   const Block& block = ctx.program->blocks[block_idx];
   const Block::edge_vec& succs = var.is_linear()
                                 ? block.linear_succs
                                 : block.logical_succs;

   bool result = false;
   for (unsigned succ_idx : succs) {
      if (succ_idx < ctx.program->live.live_in.size() &&
          ctx.program->live.live_in[succ_idx].count(var.id())) {
         result = true;
         break;
      }
   }

   /* Update cache */
   ctx.live_out_cache.insert(var.id(), block_idx, result);
   return result;
}

[[nodiscard]] static bool
intersects(const cssa_ctx& ctx, Temp var, Temp parent) noexcept
{
   const merge_node& node_var = get_node_const(ctx, var.id());
   const uint32_t block_idx = node_var.defined_at;

   if (block_idx == INVALID_IDX || block_idx >= ctx.program->blocks.size()) {
      return false;
   }

   const merge_node& node_parent = get_node_const(ctx, parent.id());

   /* Early exit if parent not live at var's definition */
   if (node_parent.defined_at < node_var.defined_at &&
       block_idx < ctx.program->live.live_in.size() &&
       !ctx.program->live.live_in[block_idx].count(parent.id())) {
      return false;
   }

   if (is_live_out(ctx, parent, block_idx)) {
      return true;
   }

   /* Check parallel copies */
   bool parent_live = false;
   if (block_idx < ctx.parallelcopies.size()) {
      for (const copy& cp : ctx.parallelcopies[block_idx]) {
         if (cp.def.getTemp() == var) {
            return false;
         }
         if (cp.op.isTemp() && cp.op.getTemp() == parent) {
            parent_live = true;
         }
      }
   }

   if (parent_live) {
      return true;
   }

   /* Check instructions */
   const Block& block = ctx.program->blocks[block_idx];
   for (auto it = block.instructions.crbegin(); it != block.instructions.crend(); ++it) {
      if (is_phi(it->get())) {
         break;
      }

      for (const Definition& def : (*it)->definitions) {
         if (def.isTemp() && def.getTemp() == var) {
            return false;
         }
      }

      for (const Operand& op : (*it)->operands) {
         if (op.isTemp() && op.getTemp() == parent) {
            return true;
         }
      }
   }

   return false;
}

[[nodiscard]] static bool
interference(cssa_ctx& ctx, Temp var, Temp parent) noexcept
{
   assert(var != parent);

   merge_node& node_var = get_node(ctx, var.id());
   node_var.equal_anc_out = Temp();

   const merge_node& node_parent = get_node_const(ctx, parent.id());

   if (node_var.index == node_parent.index) {
      parent = node_parent.equal_anc_out;
   }

   /* Limit iteration count to prevent infinite loops */
   for (uint32_t iter = 0u; iter < 256u && parent != Temp(); ++iter) {
      const merge_node& node_tmp = get_node_const(ctx, parent.id());

      if (!intersects(ctx, var, parent)) {
         parent = node_tmp.equal_anc_in;
         continue;
      }

      if (node_var.value == node_tmp.value) {
         node_var.equal_anc_out = parent;
         return false;
      }
      return true;
   }

   return false;
}

/* -------------------------------
 * Merge set operations
 * ------------------------------- */

static bool
try_merge_merge_set(cssa_ctx& ctx, Temp dst, merge_set& set_b)
{
   const uint32_t index = get_node_const(ctx, dst.id()).index;

   if (index == INVALID_IDX || index >= ctx.merge_sets.size()) {
      return false;
   }

   merge_set& set_a = ctx.merge_sets[index];

   /* Build union while checking dominance */
   std::vector<Temp> dom_stack;
   dom_stack.reserve(16u);

   std::vector<Temp> union_set;
   union_set.reserve(set_a.size() + set_b.size());

   size_t i_a = 0u;
   size_t i_b = 0u;

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

      /* Pop non-dominators */
      while (!dom_stack.empty() && !dominates(ctx, dom_stack.back(), cur)) {
         dom_stack.pop_back();
      }

      /* Check interference */
      if (!dom_stack.empty() && interference(ctx, cur, dom_stack.back())) {
         /* Rollback */
         for (const Temp& t : union_set) {
            merge_node& node = get_node(ctx, t.id());
            node.equal_anc_out = Temp();
         }
         return false;
      }

      dom_stack.push_back(cur);

      if (cur != dst) {
         union_set.push_back(cur);
      }
   }

   /* Update nodes */
   for (const Temp& t : union_set) {
      merge_node& node = get_node(ctx, t.id());

      if (node.equal_anc_in == Temp() ||
          (node.equal_anc_out != Temp() &&
           defined_after(ctx, node.equal_anc_out, node.equal_anc_in))) {
         node.equal_anc_in = node.equal_anc_out;
      }

      node.equal_anc_out = Temp();
      node.index = index;
   }

   set_b.clear();
   ctx.merge_sets[index] = std::move(union_set);
   return true;
}

static bool
try_coalesce_copy(cssa_ctx& ctx, copy cp, uint32_t block_idx)
{
   if (!cp.op.isTemp() || !cp.op.isKill()) {
      return false;
   }

   if (cp.op.regClass() != cp.def.regClass()) {
      return false;
   }

   if (cp.op.tempId() >= MAX_TEMP_COUNT || cp.def.tempId() >= MAX_TEMP_COUNT) {
      return false;
   }

   merge_node& op_node = get_node(ctx, cp.op.tempId());

   /* Find definition block */
   if (op_node.defined_at == INVALID_IDX) {
      uint32_t def_block = block_idx;

      while (def_block < ctx.program->blocks.size() &&
             ctx.program->live.live_in[def_block].count(cp.op.tempId())) {
         const Block& block = ctx.program->blocks[def_block];
         const uint32_t idom = (cp.op.regClass().type() == RegType::vgpr)
                             ? block.logical_idom
                             : block.linear_idom;

         if (idom == def_block || idom >= ctx.program->blocks.size()) {
            break;
         }
         def_block = idom;
      }

      op_node.defined_at = def_block;
      op_node.value = cp.op;
   }

   /* Try merge */
   if (op_node.index == INVALID_IDX) {
      merge_set singleton{cp.op.getTemp()};
      return try_merge_merge_set(ctx, cp.def.getTemp(), singleton);
   }

   const uint32_t def_idx = get_node_const(ctx, cp.def.tempId()).index;
   if (op_node.index == def_idx) {
      return true;
   }

   if (op_node.index >= ctx.merge_sets.size()) {
      return false;
   }

   return try_merge_merge_set(ctx, cp.def.getTemp(), ctx.merge_sets[op_node.index]);
}

/* -------------------------------
 * Copy emission
 * ------------------------------- */

static void
emit_copies_block(Builder& bld,
                  std::vector<ltg_node>& ltg,
                  std::vector<uint8_t>& ltg_valid,
                  const std::vector<uint32_t>& active_keys,
                  RegType type,
                  rename_map_t& rename_map)
{
   if (active_keys.empty()) {
      return;
   }

   RegisterDemand live_changes;
   RegisterDemand reg_demand = bld.it->get()->register_demand -
                               get_temp_registers(bld.it->get()) -
                               get_live_changes(bld.it->get());

   /* Count uses */
   std::unordered_map<uint32_t, uint32_t> remaining_uses;
   remaining_uses.reserve(active_keys.size());

   for (uint32_t key : active_keys) {
      if (key >= ltg.size() || !ltg_valid[key]) {
         continue;
      }

      const ltg_node& node = ltg[key];
      if (node.cp && node.cp->op.isTemp()) {
         remaining_uses[node.cp->op.tempId()]++;
      }
   }

   /* Lambda for last use check */
   auto is_last_use = [&remaining_uses](uint32_t temp_id) -> bool {
      auto it = remaining_uses.find(temp_id);
      if (it == remaining_uses.end()) {
         return true;
      }
      const bool last = (it->second == 1u);
      if (last) {
         remaining_uses.erase(it);
      } else {
         --it->second;
      }
      return last;
   };

   /* Process ready nodes */
   std::vector<uint32_t> worklist;
   worklist.reserve(active_keys.size());

   for (uint32_t key : active_keys) {
      if (key >= ltg.size() || !ltg_valid[key]) {
         continue;
      }

      const ltg_node& node = ltg[key];
      if (node.cp && node.cp->def.regClass().type() == type && node.num_uses == 0u) {
         worklist.push_back(key);
      }
   }

   while (!worklist.empty()) {
      const uint32_t write_key = worklist.back();
      worklist.pop_back();

      if (write_key >= ltg.size() || !ltg_valid[write_key]) {
         continue;
      }

      ltg_node node = ltg[write_key];
      ltg_valid[write_key] = 0u;

      if (!node.cp) {
         continue;
      }

      Operand src = node.cp->op;

      /* Resolve rename */
      if (src.isTemp()) {
         Operand resolved = rename_map.resolve(src.getTemp().id());
         if (!resolved.isUndefined()) {
            src = resolved;
         }

         if (src.isTemp() && src.isKill()) {
            src.setKill(is_last_use(src.getTemp().id()));
         }
      }

      /* Update dependencies */
      if (node.read_key != INVALID_IDX && node.read_key < ltg.size() && ltg_valid[node.read_key]) {
         ltg_node& read_node = ltg[node.read_key];
         if (read_node.num_uses > 0u) {
            --read_node.num_uses;
            if (read_node.num_uses == 0u && read_node.cp &&
                read_node.cp->def.regClass().type() == type) {
               worklist.push_back(node.read_key);
            }
         }
      }

      /* Emit copy */
      Instruction* copy_ins = bld.copy(node.cp->def, src);
      live_changes += get_live_changes(copy_ins);
      copy_ins->register_demand = reg_demand + live_changes + get_temp_registers(copy_ins);
   }

   /* Handle cycles with parallel copy */
   unsigned pc_slots = 0u;
   for (uint32_t key : active_keys) {
      if (key < ltg.size() && ltg_valid[key] && ltg[key].cp &&
          ltg[key].cp->def.regClass().type() == type) {
         pc_slots++;
      }
   }

   if (pc_slots > 0u) {
      aco_ptr<Instruction> pc{
         create_instruction(aco_opcode::p_parallelcopy, Format::PSEUDO, pc_slots, pc_slots)
      };

      unsigned slot = 0u;
      for (uint32_t key : active_keys) {
         if (key >= ltg.size() || !ltg_valid[key]) {
            continue;
         }

         ltg_node& node = ltg[key];
         if (!node.cp || node.cp->def.regClass().type() != type) {
            continue;
         }

         pc->definitions[slot] = node.cp->def;

         Operand src = node.cp->op;
         if (src.isTemp()) {
            Operand resolved = rename_map.resolve(src.getTemp().id());
            if (!resolved.isUndefined()) {
               src = resolved;
            }

            if (src.isTemp() && src.isKill()) {
               src.setKill(is_last_use(src.getTemp().id()));
            }
         }

         pc->operands[slot] = src;
         ltg_valid[key] = 0u;

         /* Update dependencies */
         if (node.read_key != INVALID_IDX && node.read_key < ltg.size() &&
             ltg_valid[node.read_key] && ltg[node.read_key].num_uses > 0u) {
            --ltg[node.read_key].num_uses;
         }

         ++slot;
      }

      assert(slot == pc_slots);

      live_changes += get_live_changes(pc.get());
      pc->register_demand = reg_demand + live_changes + get_temp_registers(pc.get());
      bld.insert(std::move(pc));
   }

   /* Update register demand */
   if (live_changes.sgpr || live_changes.vgpr) {
      for (auto it = bld.it; it != bld.instructions->end(); ++it) {
         (*it)->register_demand += live_changes;
      }
   }
}

/* -------------------------------
 * Main parallelcopy emission
 * ------------------------------- */

static void
emit_parallelcopies(cssa_ctx& ctx)
{
   rename_map_t rename_map;

   /* Process blocks in reverse */
   for (int block_idx = static_cast<int>(ctx.program->blocks.size()) - 1;
        block_idx >= 0;
        --block_idx) {

      if (static_cast<size_t>(block_idx) >= ctx.parallelcopies.size() ||
          ctx.parallelcopies[block_idx].empty()) {
         continue;
      }

      std::vector<bool> coalesced(ctx.parallelcopies[block_idx].size(), false);

      /* Stage 1: Coalesce */
      for (int type_pass = 0; type_pass < 2; ++type_pass) {
         const RegType current_type = (type_pass == 0) ? RegType::vgpr : RegType::sgpr;

         for (size_t n = 0u; n < ctx.parallelcopies[block_idx].size(); ++n) {
            if (coalesced[n]) {
               continue;
            }

            copy& cp = ctx.parallelcopies[block_idx][n];
            if (cp.def.regClass().type() != current_type) {
               continue;
            }

            if (!try_coalesce_copy(ctx, cp, static_cast<uint32_t>(block_idx))) {
               continue;
            }

            coalesced[n] = true;
            assert(cp.op.isTemp() && cp.op.isKill());

            /* Update kill flags */
            for (copy& other : ctx.parallelcopies[block_idx]) {
               if (&other != &cp && other.op.isTemp() &&
                   other.op.getTemp() == cp.op.getTemp()) {
                  other.op.setKill(false);
                  other.op.setFirstKill(false);
               }
            }

            rename_map.set(cp.def.tempId(), cp.op);
         }
      }

      /* Stage 2: Build LTG */
      uint32_t local_max_temp = 0u;
      for (size_t n = 0u; n < ctx.parallelcopies[block_idx].size(); ++n) {
         if (coalesced[n]) {
            continue;
         }

         const copy& cp = ctx.parallelcopies[block_idx][n];
         local_max_temp = std::max(local_max_temp, cp.def.tempId());
         if (cp.op.isTemp()) {
            local_max_temp = std::max(local_max_temp, cp.op.tempId());
         }
      }

      if (local_max_temp >= MAX_TEMP_COUNT) {
         assert(!"Local temp ID exceeds maximum");
         continue;
      }

      std::vector<ltg_node> ltg(local_max_temp + 1u);
      std::vector<uint8_t> ltg_valid(local_max_temp + 1u, 0u);
      std::vector<uint32_t> active_keys;
      active_keys.reserve(ctx.parallelcopies[block_idx].size());

      bool has_vgpr = false;
      bool has_sgpr = false;

      /* Populate LTG */
      for (size_t n = 0u; n < ctx.parallelcopies[block_idx].size(); ++n) {
         if (coalesced[n]) {
            continue;
         }

         copy& cp = ctx.parallelcopies[block_idx][n];

         /* Apply renames */
         if (cp.op.isTemp()) {
            Operand resolved = rename_map.resolve(cp.op.tempId());
            if (!resolved.isUndefined()) {
               cp.op = resolved;
            }
         }

         /* Update kill flags */
         if (cp.op.isTemp()) {
            const bool live_out = is_live_out(ctx, cp.op.getTemp(),
                                              static_cast<uint32_t>(block_idx));
            const bool keep_kill = cp.op.isKill() && !live_out;
            cp.op.setKill(keep_kill);
            cp.op.setFirstKill(keep_kill);
         }

         const uint32_t dst_key = cp.def.tempId();
         const uint32_t read_key = cp.op.isTemp() ? cp.op.tempId() : INVALID_IDX;

         if (dst_key > local_max_temp) {
            continue;
         }

         ltg[dst_key] = ltg_node(&cp, read_key, 0u);
         ltg_valid[dst_key] = 1u;
         active_keys.push_back(dst_key);

         has_vgpr |= (cp.def.regClass().type() == RegType::vgpr);
         has_sgpr |= (cp.def.regClass().type() == RegType::sgpr);
      }

      /* Count uses */
      for (uint32_t key : active_keys) {
         if (key < ltg.size() && ltg_valid[key]) {
            const uint32_t read_key = ltg[key].read_key;
            if (read_key != INVALID_IDX && read_key < ltg.size() && ltg_valid[read_key]) {
               ltg[read_key].num_uses++;
            }
         }
      }

      /* Stage 3: Emit */
      Builder bld(ctx.program);
      Block& block = ctx.program->blocks[block_idx];

      if (has_vgpr) {
         auto logical_end_it = std::find_if(
            block.instructions.rbegin(),
            block.instructions.rend(),
            [](const aco_ptr<Instruction>& ins) {
               return ins->opcode == aco_opcode::p_logical_end;
            }
         );

         auto insert_pt = (logical_end_it == block.instructions.rend())
                        ? std::prev(block.instructions.end())
                        : std::prev(logical_end_it.base());

         bld.reset(&block.instructions, insert_pt);
         emit_copies_block(bld, ltg, ltg_valid, active_keys, RegType::vgpr, rename_map);
      }

      if (has_sgpr) {
         bld.reset(&block.instructions, std::prev(block.instructions.end()));
         emit_copies_block(bld, ltg, ltg_valid, active_keys, RegType::sgpr, rename_map);
      }
   }

   /* Update phi operands and register demand */
   RegisterDemand program_demand;

   for (Block& block : ctx.program->blocks) {
      for (aco_ptr<Instruction>& phi : block.instructions) {
         if (phi->opcode != aco_opcode::p_phi &&
             phi->opcode != aco_opcode::p_linear_phi) {
            break;
         }

         for (Operand& op : phi->operands) {
            if (op.isTemp()) {
               Operand resolved = rename_map.resolve(op.tempId());
               if (!resolved.isUndefined()) {
                  op = resolved;
               }
            }
         }
      }

      block.register_demand = block.live_in_demand;
      for (const auto& ins : block.instructions) {
         block.register_demand.update(ins->register_demand);
      }

      program_demand.update(block.register_demand);
   }

   update_vgpr_sgpr_demand(ctx.program, program_demand);
}

} // namespace

void
lower_to_cssa(Program* program)
{
   if (!program) {
      assert(!"Null program pointer");
      return;
   }

   reindex_ssa(program);

   cssa_ctx ctx{program};
   collect_parallelcopies(ctx);
   emit_parallelcopies(ctx);

   if (!validate_live_vars(program)) {
      abort();
   }
}

} // namespace aco
