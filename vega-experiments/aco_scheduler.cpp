/*
 * Copyright © 2018 Valve Corporation
 *
 * SPDX-License-Identifier: MIT
 */
#include "aco_builder.h"
#include "aco_ir.h"
#include "aco_util.h"
#include "common/amdgfxregs.h"
#include <algorithm>
#include <vector>
#include <cstring>
#include <climits>
#include <cstdint>
#define SMEM_WINDOW_SIZE    (256 - ctx.occupancy_factor * 16)
#define VMEM_WINDOW_SIZE    (1024 - ctx.occupancy_factor * 64)
#define LDS_WINDOW_SIZE     64
#define POS_EXP_WINDOW_SIZE 512
#define SMEM_MAX_MOVES      (128 - ctx.occupancy_factor * 8)
#define VMEM_MAX_MOVES      (256 - ctx.occupancy_factor * 16)
#define LDSDIR_MAX_MOVES    10
#define LDS_MAX_MOVES       32
#define VMEM_CLAUSE_MAX_GRAB_DIST       (ctx.occupancy_factor * 2)
#define VMEM_STORE_CLAUSE_MAX_GRAB_DIST (ctx.occupancy_factor * 4)
#define POS_EXP_MAX_MOVES               512
namespace aco {
namespace {
enum MoveResult : uint8_t {
   move_success = 0,
   move_fail_ssa,
   move_fail_rar,
   move_fail_pressure,
};
struct DownwardsCursor {
   int source_idx;
   int insert_idx_clause;
   int insert_idx;
   RegisterDemand total_demand;
   RegisterDemand insert_demand;
   explicit DownwardsCursor(int current_idx)
       : source_idx(current_idx - 1), insert_idx_clause(current_idx), insert_idx(current_idx + 1)
   {}
   void verify_invariants([[maybe_unused]] const Block* block) const;
};
struct UpwardsCursor {
   int source_idx;
   int insert_idx;
   RegisterDemand total_demand;
   RegisterDemand insert_demand;
   explicit UpwardsCursor(int source_idx_) : source_idx(source_idx_), insert_idx(-1) {}
   bool has_insert_idx() const { return insert_idx != -1; }
   void verify_invariants([[maybe_unused]] const Block* block) const;
};
struct MoveState {
   monotonic_buffer_resource m;
   RegisterDemand max_registers;
   Block* block;
   Instruction* current;
   bool improved_rar;
   std::vector<uint8_t> depends_on;
   aco::unordered_map<uint32_t, int> rar_dependencies;
   MoveState() : rar_dependencies(m) {}
   DownwardsCursor downwards_init(int current_idx, bool improved_rar);
   MoveResult downwards_check_deps(Instruction* instr, Temp* rar_dep = nullptr);
   MoveResult downwards_move(DownwardsCursor&);
   MoveResult downwards_move_clause(DownwardsCursor&);
   void downwards_skip(DownwardsCursor&);
   UpwardsCursor upwards_init(int source_idx, bool improved_rar);
   bool upwards_check_deps(const UpwardsCursor&) const;
   void upwards_update_insert_idx(UpwardsCursor&);
   MoveResult upwards_move(UpwardsCursor&);
   void upwards_skip(UpwardsCursor&);
};
struct sched_ctx {
   amd_gfx_level gfx_level;
   Program* program;
   int16_t occupancy_factor;
   int16_t last_SMEM_stall;
   int last_SMEM_dep_idx;
   int last_VMEM_store_idx;
   MoveState mv;
   bool schedule_pos_exports = true;
   unsigned schedule_pos_export_div = 1;
};
template <typename It>
static inline void
move_one(It begin_it, size_t idx, size_t before)
{
   if (idx < before) {
      auto first = std::next(begin_it, static_cast<typename std::iterator_traits<It>::difference_type>(idx));
      auto last  = std::next(begin_it, static_cast<typename std::iterator_traits<It>::difference_type>(before - 1));
      auto tmp   = std::move(*first);
      std::move(std::next(first), std::next(first, static_cast<typename std::iterator_traits<It>::difference_type>(before - idx)), first);
      *last = std::move(tmp);
   } else if (idx > before) {
      auto first = std::next(begin_it, static_cast<typename std::iterator_traits<It>::difference_type>(before));
      auto pos   = std::next(begin_it, static_cast<typename std::iterator_traits<It>::difference_type>(idx));
      auto tmp   = std::move(*pos);
      std::move_backward(first, pos, std::next(pos));
      *first = std::move(tmp);
   }
}
template <typename T>
void
move_element(T begin_it, size_t idx, size_t before, int num = 1)
{
   if (idx == before || num <= 0) {
      return;
   }
   if (num == 1) {
      move_one(begin_it, idx, before);
      return;
   }
   if (idx < before) {
      auto begin = std::next(begin_it, static_cast<typename std::iterator_traits<T>::difference_type>(idx));
      auto end = std::next(begin_it, static_cast<typename std::iterator_traits<T>::difference_type>(before));
      std::rotate(begin, begin + num, end);
   } else if (idx > before) {
      auto begin = std::next(begin_it, static_cast<typename std::iterator_traits<T>::difference_type>(before));
      auto end = std::next(begin_it, static_cast<typename std::iterator_traits<T>::difference_type>(idx + 1));
      std::rotate(begin, end - num, end);
   }
}
void
DownwardsCursor::verify_invariants([[maybe_unused]] const Block* block) const
{
   assert(source_idx < insert_idx_clause);
   assert(insert_idx_clause < insert_idx);
#ifndef NDEBUG
   RegisterDemand reference_demand;
   for (int i = source_idx + 1; i < insert_idx_clause; ++i) {
      reference_demand.update(block->instructions[i]->register_demand);
   }
   assert(total_demand == reference_demand);
#endif
}
DownwardsCursor
MoveState::downwards_init(int current_idx, bool improved_rar_)
{
   improved_rar = improved_rar_;
   depends_on.assign(depends_on.size(), 0);
   if (improved_rar)
      rar_dependencies.clear();
   for (const Operand& op : current->operands) {
      if (op.isTemp()) {
         const unsigned id = op.tempId();
         depends_on[id] = 1;
         if (improved_rar && op.isFirstKill())
            rar_dependencies[id] = -1;
      }
   }
   DownwardsCursor cursor(current_idx);
   const RegisterDemand temp = get_temp_registers(block->instructions[cursor.insert_idx - 1].get());
   cursor.insert_demand = block->instructions[cursor.insert_idx - 1]->register_demand - temp;
   cursor.verify_invariants(block);
   return cursor;
}
MoveResult
MoveState::downwards_check_deps(Instruction* instr, Temp* rar_dep)
{
   for (const Definition& def : instr->definitions) {
      if (def.isTemp() && depends_on[def.tempId()])
         return move_fail_ssa;
   }
   for (const Operand& op : instr->operands) {
      if (!op.isTemp() || op.isKill())
         continue;
      if (!improved_rar && depends_on[op.tempId()])
         return move_fail_rar;
      if (improved_rar && rar_dependencies.count(op.tempId())) {
         if (rar_dep && (*rar_dep == Temp() || *rar_dep == op.getTemp()))
            *rar_dep = op.getTemp();
         else
            return move_fail_rar;
      }
   }
   return move_success;
}
MoveResult
MoveState::downwards_move(DownwardsCursor& cursor)
{
   aco_ptr<Instruction>& candidate = block->instructions[cursor.source_idx];
   MoveResult res = downwards_check_deps(candidate.get());
   if (res != move_success)
      return res;
   RegisterDemand register_pressure = cursor.total_demand;
   assert(cursor.insert_idx_clause == (cursor.insert_idx - 1));
   register_pressure.update(block->instructions[cursor.insert_idx_clause]->register_demand);
   const RegisterDemand candidate_diff = get_live_changes(candidate.get());
   if (RegisterDemand(register_pressure - candidate_diff).exceeds(max_registers)) {
      return move_fail_pressure;
   }
   const RegisterDemand temp = get_temp_registers(candidate.get());
   const RegisterDemand insert_demand = cursor.insert_demand;
   const RegisterDemand new_demand = insert_demand + temp;
   if (new_demand.exceeds(max_registers)) {
      return move_fail_pressure;
   }
   move_element(block->instructions.begin(), static_cast<size_t>(cursor.source_idx),
                static_cast<size_t>(cursor.insert_idx));
   cursor.insert_idx--;
   cursor.insert_idx_clause--;
   for (int i = cursor.source_idx; i < cursor.insert_idx; i++) {
      block->instructions[i]->register_demand -= candidate_diff;
   }
   block->instructions[cursor.insert_idx]->register_demand = new_demand;
   if (cursor.source_idx != cursor.insert_idx_clause) {
      cursor.total_demand -= candidate_diff;
   } else {
      assert(cursor.total_demand == RegisterDemand{});
   }
   cursor.insert_demand -= candidate_diff;
   cursor.source_idx--;
   cursor.verify_invariants(block);
   return move_success;
}
MoveResult
MoveState::downwards_move_clause(DownwardsCursor& cursor)
{
   assert(improved_rar);
   if (cursor.source_idx == cursor.insert_idx_clause - 1) {
      cursor.insert_idx_clause--;
      cursor.source_idx--;
      return move_success;
   }
   int clause_begin_idx = cursor.source_idx;
   const int clause_end_idx = cursor.source_idx;
   const int insert_idx = cursor.insert_idx_clause - 1;
   Instruction* instr = block->instructions[cursor.insert_idx_clause].get();
   for (const Operand& op : current->operands) {
      if (op.isTemp() && op.isFirstKill())
         rar_dependencies.erase(op.tempId());
   }
   RegisterDemand max_clause_demand;
   Temp rar_dep = Temp();
   while (should_form_clause(block->instructions[clause_begin_idx].get(), instr)) {
      Instruction* candidate = block->instructions[clause_begin_idx--].get();
      MoveResult res = downwards_check_deps(candidate, &rar_dep);
      if (res != move_success)
         return res;
      max_clause_demand.update(candidate->register_demand);
   }
   const int clause_size = clause_end_idx - clause_begin_idx;
   assert(clause_size > 0);
   instr = block->instructions[clause_begin_idx].get();
   const RegisterDemand clause_begin_demand = instr->register_demand - get_temp_registers(instr);
   instr = block->instructions[clause_end_idx].get();
   const RegisterDemand clause_end_demand = instr->register_demand - get_temp_registers(instr);
   instr = block->instructions[insert_idx].get();
   const RegisterDemand insert_demand = instr->register_demand - get_temp_registers(instr);
   const RegisterDemand clause_diff = clause_end_demand - clause_begin_demand;
   const RegisterDemand insert_diff = insert_demand - clause_end_demand + rar_dep;
   if (RegisterDemand(cursor.total_demand - clause_diff).exceeds(max_registers))
      return move_fail_pressure;
   if (RegisterDemand(max_clause_demand + insert_diff).exceeds(max_registers))
      return move_fail_pressure;
   int rar_index = insert_idx;
   if (rar_dep != Temp()) {
      for (int i = clause_end_idx; i > clause_begin_idx; i--) {
         instr = block->instructions[i].get();
         instr->register_demand -= rar_dep;
         bool first = true;
         for (Operand& op : instr->operands) {
            if (op.isTemp() && op.getTemp() == rar_dep) {
               if (first)
                  instr->register_demand -= get_temp_registers(instr);
               op.setKill(true);
               op.setFirstKill(first);
               first = false;
            }
         }
         if (first == false) {
            instr->register_demand += get_temp_registers(instr);
            break;
         }
      }
      rar_index = cursor.insert_idx + rar_dependencies[rar_dep.id()];
      Instruction* rar_instr = block->instructions[rar_index].get();
      rar_instr->register_demand -= get_temp_registers(rar_instr);
      for (Operand& op : rar_instr->operands) {
         if (op.isTemp() && op.getTemp() == rar_dep && !op.isCopyKill())
            op.setKill(false);
      }
      rar_instr->register_demand += get_temp_registers(rar_instr) + rar_dep;
   }
   for (int i = clause_begin_idx + 1; i <= clause_end_idx; i++)
      block->instructions[i]->register_demand += insert_diff;
   for (int i = clause_end_idx + 1; i <= rar_index; i++)
      block->instructions[i]->register_demand -= clause_diff;
   for (int i = rar_index + 1; i <= insert_idx; i++) {
      block->instructions[i]->register_demand -= clause_diff;
      block->instructions[i]->register_demand += rar_dep;
   }
   move_element(block->instructions.begin(), static_cast<size_t>(clause_begin_idx + 1),
                static_cast<size_t>(cursor.insert_idx_clause), clause_size);
   cursor.source_idx = clause_begin_idx;
   cursor.insert_idx_clause -= clause_size;
   cursor.total_demand -= clause_diff;
   return move_success;
}
void
MoveState::downwards_skip(DownwardsCursor& cursor)
{
   const aco_ptr<Instruction>& instr = block->instructions[cursor.source_idx];
   for (const Operand& op : instr->operands) {
      if (op.isTemp()) {
         const unsigned id = op.tempId();
         depends_on[id] = 1;
         if (improved_rar && op.isFirstKill())
            rar_dependencies[id] = cursor.source_idx - cursor.insert_idx;
      }
   }
   cursor.total_demand.update(instr->register_demand);
   cursor.source_idx--;
   cursor.verify_invariants(block);
}
void
UpwardsCursor::verify_invariants([[maybe_unused]] const Block* block) const
{
#ifndef NDEBUG
   if (!has_insert_idx()) {
      return;
   }
   assert(insert_idx < source_idx);
   RegisterDemand reference_demand;
   for (int i = insert_idx; i < source_idx; ++i) {
      reference_demand.update(block->instructions[i]->register_demand);
   }
   assert(total_demand == reference_demand);
#endif
}
UpwardsCursor
MoveState::upwards_init(int source_idx, bool improved_rar_)
{
   improved_rar = improved_rar_;
   depends_on.assign(depends_on.size(), 0);
   rar_dependencies.clear();
   for (const Definition& def : current->definitions) {
      if (def.isTemp()) {
         depends_on[def.tempId()] = 1;
      }
   }
   return UpwardsCursor(source_idx);
}
bool
MoveState::upwards_check_deps(const UpwardsCursor& cursor) const
{
   const aco_ptr<Instruction>& instr = block->instructions[cursor.source_idx];
   for (const Operand& op : instr->operands) {
      if (op.isTemp() && depends_on[op.tempId()]) {
         return false;
      }
   }
   return true;
}
void
MoveState::upwards_update_insert_idx(UpwardsCursor& cursor)
{
   cursor.insert_idx = cursor.source_idx;
   cursor.total_demand = block->instructions[cursor.insert_idx]->register_demand;
   const RegisterDemand temp = get_temp_registers(block->instructions[cursor.insert_idx - 1].get());
   cursor.insert_demand = block->instructions[cursor.insert_idx - 1]->register_demand - temp;
}
static constexpr inline int
vgpr_headroom(const RegisterDemand& max_regs, const RegisterDemand& demand)
{
   const int margin = max_regs.vgpr - demand.vgpr;
   return margin > 0 ? margin : 0;
}
MoveResult
MoveState::upwards_move(UpwardsCursor& cursor)
{
   assert(cursor.has_insert_idx());
   aco_ptr<Instruction>& instr = block->instructions[cursor.source_idx];
   for (const Operand& op : instr->operands) {
      if (op.isTemp() && depends_on[op.tempId()]) {
         return move_fail_ssa;
      }
   }
   for (const Operand& op : instr->operands) {
      if (op.isTemp() && (!improved_rar || op.isFirstKill()) && rar_dependencies.count(op.tempId())) {
         return move_fail_rar;
      }
   }
   const RegisterDemand candidate_diff = get_live_changes(instr.get());
   const RegisterDemand temp = get_temp_registers(instr.get());
   if (RegisterDemand(cursor.total_demand + candidate_diff).exceeds(max_registers)) {
      return move_fail_pressure;
   }
   const RegisterDemand new_demand = cursor.insert_demand + candidate_diff + temp;
   if (new_demand.exceeds(max_registers)) {
      return move_fail_pressure;
   }
   move_element(block->instructions.begin(), static_cast<size_t>(cursor.source_idx),
                static_cast<size_t>(cursor.insert_idx));
   block->instructions[cursor.insert_idx]->register_demand = new_demand;
   for (int i = cursor.insert_idx + 1; i <= cursor.source_idx; i++) {
      block->instructions[i]->register_demand += candidate_diff;
   }
   cursor.total_demand += candidate_diff;
   cursor.insert_demand += candidate_diff;
   cursor.insert_idx++;
   cursor.source_idx++;
   cursor.verify_invariants(block);
   return move_success;
}
void
MoveState::upwards_skip(UpwardsCursor& cursor)
{
   if (cursor.has_insert_idx()) {
      const aco_ptr<Instruction>& instr = block->instructions[cursor.source_idx];
      for (const Definition& def : instr->definitions) {
         if (def.isTemp()) {
            depends_on[def.tempId()] = 1;
         }
      }
      for (const Operand& op : instr->operands) {
         if (op.isTemp()) {
            rar_dependencies[op.tempId()] = cursor.source_idx - cursor.insert_idx;
         }
      }
      cursor.total_demand.update(instr->register_demand);
   }
   cursor.source_idx++;
   cursor.verify_invariants(block);
}
memory_sync_info
get_sync_info_with_hack(const Instruction* instr)
{
   memory_sync_info sync = get_sync_info(instr);
   if (instr->isSMEM() && !instr->operands.empty() && instr->operands[0].bytes() == 16) {
      sync.storage = static_cast<storage_class>(sync.storage | storage_buffer);
      sync.semantics =
         static_cast<memory_semantics>((sync.semantics | semantic_private) & ~semantic_can_reorder);
   }
   return sync;
}
static constexpr inline bool
is_reorderable(const Instruction* instr)
{
   return instr->opcode != aco_opcode::s_memtime && instr->opcode != aco_opcode::s_memrealtime &&
          instr->opcode != aco_opcode::s_setprio && instr->opcode != aco_opcode::s_getreg_b32 &&
          instr->opcode != aco_opcode::p_shader_cycles_hi_lo_hi &&
          instr->opcode != aco_opcode::p_init_scratch &&
          instr->opcode != aco_opcode::p_jump_to_epilog &&
          instr->opcode != aco_opcode::s_sendmsg_rtn_b32 &&
          instr->opcode != aco_opcode::s_sendmsg_rtn_b64 &&
          instr->opcode != aco_opcode::p_end_with_regs && instr->opcode != aco_opcode::s_nop &&
          instr->opcode != aco_opcode::s_sleep && instr->opcode != aco_opcode::s_trap &&
          instr->opcode != aco_opcode::p_call && instr->opcode != aco_opcode::p_logical_start &&
          instr->opcode != aco_opcode::p_logical_end &&
          instr->opcode != aco_opcode::p_reload_preserved;
}
struct memory_event_set {
   bool has_control_barrier;
   unsigned bar_acquire;
   unsigned bar_release;
   unsigned bar_classes;
   unsigned access_acquire;
   unsigned access_release;
   unsigned access_relaxed;
   unsigned access_atomic;
};
struct hazard_query {
   uint32_t aliasing_storage;
   uint32_t aliasing_storage_smem;
   memory_event_set mem_events;

   Program* program;
   amd_gfx_level gfx_level;
   bool contains_spill;
   bool contains_sendmsg;
   bool uses_exec;
   bool writes_exec;
};
void
init_hazard_query(const sched_ctx& ctx, hazard_query* query)
{
   query->program = ctx.program;
   query->gfx_level = ctx.gfx_level;
   query->contains_spill = false;
   query->contains_sendmsg = false;
   query->uses_exec = false;
   query->writes_exec = false;
   std::memset(&query->mem_events, 0, sizeof(query->mem_events));
   query->aliasing_storage = 0;
   query->aliasing_storage_smem = 0;
}
void
add_memory_event(Program* program, memory_event_set* set, Instruction* instr,
                 memory_sync_info* sync)
{
   if (instr->opcode == aco_opcode::p_barrier) {
      Pseudo_barrier_instruction& bar = instr->barrier();
      if (bar.sync.semantics & semantic_acquire) {
         set->bar_acquire |= bar.sync.storage;
      }
      if (bar.sync.semantics & semantic_release) {
         set->bar_release |= bar.sync.storage;
      }
      set->bar_classes |= bar.sync.storage;
   }
   if (!sync->storage) {
      set->has_control_barrier |=
         is_atomic_or_control_instr(program, instr, *sync, semantic_acquire | semantic_release) !=
         0;
      return;
   }
   if (sync->semantics & semantic_acquire) {
      set->access_acquire |= sync->storage;
   }
   if (sync->semantics & semantic_release) {
      set->access_release |= sync->storage;
   }
   if (!(sync->semantics & semantic_private)) {
      if (sync->semantics & semantic_atomic) {
         set->access_atomic |= sync->storage;
      } else {
         set->access_relaxed |= sync->storage;
      }
   }
}
void
add_to_hazard_query(hazard_query* query, Instruction* instr)
{
   if (instr->opcode == aco_opcode::p_spill || instr->opcode == aco_opcode::p_reload) {
      query->contains_spill = true;
   }
   query->contains_sendmsg |= instr->opcode == aco_opcode::s_sendmsg;
   query->uses_exec |= needs_exec_mask(instr);
   for (const Definition& def : instr->definitions) {
      if (def.isFixed() && def.physReg() == exec) {
         query->writes_exec = true;
      }
   }
   memory_sync_info sync = get_sync_info_with_hack(instr);
   add_memory_event(query->program, &query->mem_events, instr, &sync);
   if (!(sync.semantics & semantic_can_reorder)) {
      unsigned storage = sync.storage;
      if (storage & (storage_buffer | storage_image)) {
         storage |= storage_buffer | storage_image;
      }
      if (instr->isSMEM()) {
         query->aliasing_storage_smem |= storage;
      } else {
         query->aliasing_storage |= storage;
      }
   }
}
enum HazardResult : uint8_t {
   hazard_success,
   hazard_fail_reorder_vmem_smem,
   hazard_fail_reorder_ds,
   hazard_fail_reorder_sendmsg,
   hazard_fail_spill,
   hazard_fail_export,
   hazard_fail_barrier,
   hazard_fail_exec,
   hazard_fail_unreorderable,
};
HazardResult
perform_hazard_query(hazard_query* query, Instruction* instr, bool upwards)
{
   if (!upwards && instr->opcode == aco_opcode::p_exit_early_if_not) {
      return hazard_fail_unreorderable;
   }
   if (upwards) {
      if (instr->opcode == aco_opcode::p_pops_gfx9_add_exiting_wave_id ||
          is_wait_export_ready(query->gfx_level, instr)) {
         return hazard_fail_unreorderable;
      }
   } else {
      if (instr->opcode == aco_opcode::p_pops_gfx9_ordered_section_done) {
         return hazard_fail_unreorderable;
      }
   }
   if (query->uses_exec || query->writes_exec) {
      for (const Definition& def : instr->definitions) {
         if (def.isFixed() && def.physReg() == exec) {
            return hazard_fail_exec;
         }
      }
   }
   if (query->writes_exec && needs_exec_mask(instr)) {
      return hazard_fail_exec;
   }
   if (instr->isEXP() || instr->opcode == aco_opcode::p_dual_src_export_gfx11) {
      return hazard_fail_export;
   }
   memory_event_set instr_set;
   std::memset(&instr_set, 0, sizeof(instr_set));
   memory_sync_info sync = get_sync_info_with_hack(instr);
   add_memory_event(query->program, &instr_set, instr, &sync);
   memory_event_set* first = &instr_set;
   memory_event_set* second = &query->mem_events;
   if (upwards) {
      std::swap(first, second);
   }
   if ((first->has_control_barrier || first->access_atomic) && second->bar_acquire) {
      return hazard_fail_barrier;
   }
   if (((first->access_acquire || first->bar_acquire) && second->bar_classes) ||
       ((first->access_acquire | first->bar_acquire) &
        (second->access_relaxed | second->access_atomic))) {
      return hazard_fail_barrier;
   }
   if (first->bar_release && (second->has_control_barrier || second->access_atomic)) {
      return hazard_fail_barrier;
   }
   if ((first->bar_classes && (second->bar_release || second->access_release)) ||
       ((first->access_relaxed | first->access_atomic) &
        (second->bar_release | second->access_release))) {
      return hazard_fail_barrier;
   }
   if (first->bar_classes && second->bar_classes) {
      return hazard_fail_barrier;
   }
   const unsigned control_classes =
      storage_buffer | storage_image | storage_shared | storage_task_payload;
   if (first->has_control_barrier &&
       ((second->access_atomic | second->access_relaxed) & control_classes)) {
      return hazard_fail_barrier;
   }
   const unsigned aliasing_storage =
      instr->isSMEM() ? query->aliasing_storage_smem : query->aliasing_storage;
   if ((sync.storage & aliasing_storage) && !(sync.semantics & semantic_can_reorder)) {
      const unsigned intersect = sync.storage & aliasing_storage;
      if (intersect & storage_shared) {
         return hazard_fail_reorder_ds;
      }
      return hazard_fail_reorder_vmem_smem;
   }
   if ((instr->opcode == aco_opcode::p_spill || instr->opcode == aco_opcode::p_reload) &&
       query->contains_spill) {
      return hazard_fail_spill;
   }
   if (instr->opcode == aco_opcode::s_sendmsg && query->contains_sendmsg) {
      return hazard_fail_reorder_sendmsg;
   }
   return hazard_success;
}
static constexpr inline unsigned
get_likely_cost(const Instruction* instr)
{
   if (instr->opcode == aco_opcode::p_split_vector ||
       instr->opcode == aco_opcode::p_extract_vector) {
      unsigned cost = 0;
      for (const Definition& def : instr->definitions) {
         if (instr->operands[0].isKill() &&
             def.regClass().type() == instr->operands[0].regClass().type()) {
            continue;
         }
         cost += def.size();
      }
      return cost;
   } else if (instr->opcode == aco_opcode::p_create_vector) {
      unsigned cost = 0;
      for (const Operand& op : instr->operands) {
         if (op.isTemp() && op.isFirstKill() &&
             op.regClass().type() == instr->definitions[0].regClass().type()) {
            continue;
         }
         cost += op.size();
      }
      return cost;
   } else {
      return 1;
   }
}
void
schedule_SMEM(sched_ctx& ctx, Block* block, Instruction* current, int idx)
{
   assert(idx != 0);
   if (current->opcode == aco_opcode::s_memtime ||
       current->opcode == aco_opcode::s_memrealtime ||
       current->opcode == aco_opcode::s_sendmsg_rtn_b32 ||
       current->opcode == aco_opcode::s_sendmsg_rtn_b64) {
      return;
   }
   hazard_query hq;
   init_hazard_query(ctx, &hq);
   add_to_hazard_query(&hq, current);
   DownwardsCursor cursor = ctx.mv.downwards_init(idx, false);
   int window_size = SMEM_WINDOW_SIZE;
   int max_moves = SMEM_MAX_MOVES;
   if (ctx.gfx_level <= GFX9) {
      const int margin = vgpr_headroom(ctx.mv.max_registers, cursor.insert_demand);
      if (margin >= 32) {
         window_size = window_size + window_size / 8;
         max_moves = max_moves + max_moves / 10;
      }
   }
   int16_t k = 0;
   for (int candidate_idx = idx - 1;
        k < max_moves && candidate_idx > static_cast<int>(idx) - window_size;
        candidate_idx--) {
      assert(candidate_idx >= 0);
      assert(candidate_idx == cursor.source_idx);
      aco_ptr<Instruction>& candidate = block->instructions[candidate_idx];
      const bool can_stall_prev_smem =
         idx <= ctx.last_SMEM_dep_idx && candidate_idx < ctx.last_SMEM_dep_idx;
      if (can_stall_prev_smem && ctx.last_SMEM_stall >= 0) {
         break;
      }
      if (!is_reorderable(candidate.get())) {
         break;
      }
      if ((candidate->isVMEM() || candidate->isFlatLike()) &&
          (cursor.insert_idx - cursor.source_idx > (ctx.occupancy_factor * 4) ||
           (!current->operands.empty() && current->operands[0].size() == 4))) {
         break;
      }
      if (candidate->isSMEM() && !candidate->operands.empty() &&
          !current->operands.empty() && current->operands[0].size() == 4 &&
          candidate->operands[0].size() == 2) {
         break;
      }
      bool can_move_down = true;
      HazardResult haz = perform_hazard_query(&hq, candidate.get(), false);
      if (haz == hazard_fail_reorder_ds || haz == hazard_fail_spill ||
          haz == hazard_fail_reorder_sendmsg || haz == hazard_fail_barrier ||
          haz == hazard_fail_export) {
         can_move_down = false;
      } else if (haz != hazard_success) {
         break;
      }
      if (candidate->isDS() || !can_move_down) {
         add_to_hazard_query(&hq, candidate.get());
         ctx.mv.downwards_skip(cursor);
         continue;
      }
      const MoveResult res = ctx.mv.downwards_move(cursor);
      if (res == move_fail_ssa || res == move_fail_rar) {
         add_to_hazard_query(&hq, candidate.get());
         ctx.mv.downwards_skip(cursor);
         continue;
      } else if (res == move_fail_pressure) {
         break;
      }
      if (candidate_idx < ctx.last_SMEM_dep_idx) {
         ctx.last_SMEM_stall++;
      }
      k++;
   }
   UpwardsCursor up_cursor = ctx.mv.upwards_init(idx + 1, false);
   bool found_dependency = false;
   for (int candidate_idx = idx + 1;
        k < max_moves && candidate_idx < static_cast<int>(idx) + window_size;
        candidate_idx++) {
      assert(candidate_idx == up_cursor.source_idx);
      assert(candidate_idx < static_cast<int>(block->instructions.size()));
      aco_ptr<Instruction>& candidate = block->instructions[candidate_idx];
      if (!is_reorderable(candidate.get())) {
         break;
      }
      bool is_dependency = !found_dependency && !ctx.mv.upwards_check_deps(up_cursor);
      if (is_dependency && (candidate->isVMEM() || candidate->isFlatLike())) {
         break;
      }
      if (found_dependency) {
         HazardResult haz = perform_hazard_query(&hq, candidate.get(), true);
         if (haz == hazard_fail_reorder_ds || haz == hazard_fail_spill ||
             haz == hazard_fail_reorder_sendmsg || haz == hazard_fail_barrier ||
             haz == hazard_fail_export) {
            is_dependency = true;
         } else if (haz != hazard_success) {
            break;
         }
      }
      if (is_dependency) {
         if (!found_dependency) {
            ctx.mv.upwards_update_insert_idx(up_cursor);
            init_hazard_query(ctx, &hq);
            found_dependency = true;
         }
      }
      if (is_dependency || !found_dependency) {
         if (found_dependency) {
            add_to_hazard_query(&hq, candidate.get());
         } else {
            k++;
         }
         ctx.mv.upwards_skip(up_cursor);
         continue;
      }
      const MoveResult res = ctx.mv.upwards_move(up_cursor);
      if (res == move_fail_ssa || res == move_fail_rar) {
         if (res == move_fail_ssa && (candidate->isVMEM() || candidate->isFlatLike())) {
            break;
         }
         add_to_hazard_query(&hq, candidate.get());
         ctx.mv.upwards_skip(up_cursor);
         continue;
      } else if (res == move_fail_pressure) {
         break;
      }
      k++;
   }
   ctx.last_SMEM_dep_idx = found_dependency ? up_cursor.insert_idx : 0;
   ctx.last_SMEM_stall = 10 - ctx.occupancy_factor - k;
}
void
schedule_VMEM(sched_ctx& ctx, Block* block, Instruction* current, int idx)
{
   assert(idx != 0);
   int window_size = VMEM_WINDOW_SIZE;
   int max_moves = VMEM_MAX_MOVES;
   int clause_max_grab_dist = VMEM_CLAUSE_MAX_GRAB_DIST;
   bool only_clauses = false;
   int16_t k = 0;
   hazard_query indep_hq;
   hazard_query clause_hq;
   init_hazard_query(ctx, &indep_hq);
   init_hazard_query(ctx, &clause_hq);
   add_to_hazard_query(&indep_hq, current);
   DownwardsCursor cursor = ctx.mv.downwards_init(idx, true);
   if (ctx.gfx_level <= GFX9) {
      const int margin = vgpr_headroom(ctx.mv.max_registers, cursor.insert_demand);
      if (margin >= 32) {
         clause_max_grab_dist = VMEM_CLAUSE_MAX_GRAB_DIST + 8;
         window_size = window_size + window_size / 8;
         max_moves = max_moves + max_moves / 10;
      }
   }
   for (int candidate_idx = idx - 1;
        k < max_moves && candidate_idx > static_cast<int>(idx) - window_size;
        candidate_idx--) {
      assert(candidate_idx == cursor.source_idx);
      assert(candidate_idx >= 0);
      aco_ptr<Instruction>& candidate = block->instructions[candidate_idx];
      const bool is_vmem = candidate->isVMEM() || candidate->isFlatLike();
      if (!is_reorderable(candidate.get())) {
         break;
      }
      if (should_form_clause(current, candidate.get())) {
         const int grab_dist = cursor.insert_idx_clause - candidate_idx;
         if (grab_dist >= clause_max_grab_dist + k) {
            break;
         }
         const HazardResult haz = perform_hazard_query(&clause_hq, candidate.get(), false);
         if (haz == hazard_success) {
            ctx.mv.downwards_move_clause(cursor);
         }
         break;
      }
      const bool can_stall_prev_smem =
         idx <= ctx.last_SMEM_dep_idx && candidate_idx < ctx.last_SMEM_dep_idx;
      if (can_stall_prev_smem && ctx.last_SMEM_stall >= 0) {
         break;
      }
      bool can_move_down = !only_clauses && (!is_vmem || candidate->definitions.empty());
      HazardResult haz = perform_hazard_query(&indep_hq, candidate.get(), false);
      if (haz == hazard_fail_reorder_ds || haz == hazard_fail_spill ||
          haz == hazard_fail_reorder_sendmsg || haz == hazard_fail_barrier ||
          haz == hazard_fail_export) {
         can_move_down = false;
      } else if (haz != hazard_success) {
         break;
      }
      if (!can_move_down) {
         add_to_hazard_query(&indep_hq, candidate.get());
         add_to_hazard_query(&clause_hq, candidate.get());
         ctx.mv.downwards_skip(cursor);
         continue;
      }
      const MoveResult res = ctx.mv.downwards_move(cursor);
      if (res == move_fail_ssa || res == move_fail_rar) {
         add_to_hazard_query(&indep_hq, candidate.get());
         add_to_hazard_query(&clause_hq, candidate.get());
         ctx.mv.downwards_skip(cursor);
         continue;
      } else if (res == move_fail_pressure) {
         only_clauses = true;
         add_to_hazard_query(&indep_hq, candidate.get());
         add_to_hazard_query(&clause_hq, candidate.get());
         ctx.mv.downwards_skip(cursor);
         continue;
      }
      k++;
      if (candidate_idx < ctx.last_SMEM_dep_idx) {
         ctx.last_SMEM_stall++;
      }
   }
   UpwardsCursor up_cursor = ctx.mv.upwards_init(idx + 1, true);
   bool found_dependency = false;
   for (int candidate_idx = idx + 1;
        k < max_moves && candidate_idx < static_cast<int>(idx) + window_size;
        candidate_idx++) {
      assert(candidate_idx == up_cursor.source_idx);
      assert(candidate_idx < static_cast<int>(block->instructions.size()));
      aco_ptr<Instruction>& candidate = block->instructions[candidate_idx];
      const bool is_vmem = candidate->isVMEM() || candidate->isFlatLike();
      if (!is_reorderable(candidate.get())) {
         break;
      }
      bool is_dependency = false;
      if (found_dependency) {
         HazardResult haz = perform_hazard_query(&indep_hq, candidate.get(), true);
         if (haz == hazard_fail_reorder_ds || haz == hazard_fail_spill ||
             haz == hazard_fail_reorder_vmem_smem || haz == hazard_fail_reorder_sendmsg ||
             haz == hazard_fail_barrier || haz == hazard_fail_export) {
            is_dependency = true;
         } else if (haz != hazard_success) {
            break;
         }
      }
      is_dependency |= !found_dependency && !ctx.mv.upwards_check_deps(up_cursor);
      if (is_dependency) {
         if (!found_dependency) {
            ctx.mv.upwards_update_insert_idx(up_cursor);
            init_hazard_query(ctx, &indep_hq);
            found_dependency = true;
         }
      } else if (is_vmem) {
         for (const Definition& def : candidate->definitions) {
            if (def.isTemp()) {
               ctx.mv.depends_on[def.tempId()] = 1;
            }
         }
      }
      if (is_dependency || !found_dependency) {
         if (found_dependency) {
            add_to_hazard_query(&indep_hq, candidate.get());
         } else {
            k++;
         }
         ctx.mv.upwards_skip(up_cursor);
         continue;
      }
      const MoveResult res = ctx.mv.upwards_move(up_cursor);
      if (res == move_fail_ssa || res == move_fail_rar) {
         add_to_hazard_query(&indep_hq, candidate.get());
         ctx.mv.upwards_skip(up_cursor);
         continue;
      } else if (res == move_fail_pressure) {
         break;
      }
      k++;
   }
}
void
schedule_LDS(sched_ctx& ctx, Block* block, Instruction* current, int idx)
{
   assert(idx != 0);
   const int window_size = LDS_WINDOW_SIZE;
   const int max_moves = current->isLDSDIR() ? LDSDIR_MAX_MOVES : LDS_MAX_MOVES;
   int16_t k = 0;
   hazard_query hq;
   init_hazard_query(ctx, &hq);
   add_to_hazard_query(&hq, current);
   DownwardsCursor cursor = ctx.mv.downwards_init(idx, true);
   for (int i = 0; k < max_moves && i < window_size; i++) {
      aco_ptr<Instruction>& candidate = block->instructions[cursor.source_idx];
      const bool is_mem = candidate->isVMEM() || candidate->isFlatLike() || candidate->isSMEM();
      if (!is_reorderable(candidate.get()) || is_mem) {
         break;
      }
      if (candidate->isDS() || candidate->isLDSDIR()) {
         add_to_hazard_query(&hq, candidate.get());
         ctx.mv.downwards_skip(cursor);
         continue;
      }
      HazardResult haz = perform_hazard_query(&hq, candidate.get(), false);
      if (haz != hazard_success || ctx.mv.downwards_move(cursor) != move_success) {
         break;
      }
      k++;
   }
   bool found_dependency = false;
   int i = 0;
   UpwardsCursor up_cursor = ctx.mv.upwards_init(idx + 1, true);
   for (; k < max_moves && i < window_size; i++) {
      aco_ptr<Instruction>& candidate = block->instructions[up_cursor.source_idx];
      const bool is_mem = candidate->isVMEM() || candidate->isFlatLike() || candidate->isSMEM();
      if (!is_reorderable(candidate.get()) || is_mem) {
         break;
      }
      if (!ctx.mv.upwards_check_deps(up_cursor)) {
         init_hazard_query(ctx, &hq);
         add_to_hazard_query(&hq, candidate.get());
         ctx.mv.upwards_update_insert_idx(up_cursor);
         ctx.mv.upwards_skip(up_cursor);
         found_dependency = true;
         i++;
         break;
      }
      ctx.mv.upwards_skip(up_cursor);
   }
   for (; found_dependency && k < max_moves && i < window_size; i++) {
      aco_ptr<Instruction>& candidate = block->instructions[up_cursor.source_idx];
      const bool is_mem = candidate->isVMEM() || candidate->isFlatLike() || candidate->isSMEM();
      if (!is_reorderable(candidate.get()) || is_mem) {
         break;
      }
      HazardResult haz = perform_hazard_query(&hq, candidate.get(), true);
      if (haz == hazard_fail_exec || haz == hazard_fail_unreorderable) {
         break;
      }
      if (haz != hazard_success || ctx.mv.upwards_move(up_cursor) != move_success) {
         add_to_hazard_query(&hq, candidate.get());
         ctx.mv.upwards_skip(up_cursor);
      } else {
         k++;
      }
   }
}
void
schedule_position_export(sched_ctx& ctx, Block* block, Instruction* current, int idx)
{
   assert(idx != 0);
   const int window_size = POS_EXP_WINDOW_SIZE / static_cast<int>(ctx.schedule_pos_export_div);
   const int max_moves = POS_EXP_MAX_MOVES / static_cast<int>(ctx.schedule_pos_export_div);
   int16_t k = 0;
   DownwardsCursor cursor = ctx.mv.downwards_init(idx, true);
   hazard_query hq;
   init_hazard_query(ctx, &hq);
   add_to_hazard_query(&hq, current);
   for (int candidate_idx = idx - 1;
        k < max_moves && candidate_idx > static_cast<int>(idx) - window_size;
        candidate_idx--) {
      assert(candidate_idx >= 0);
      aco_ptr<Instruction>& candidate = block->instructions[candidate_idx];
      if (!is_reorderable(candidate.get())) {
         break;
      }
      if (candidate->isVMEM() || candidate->isSMEM() || candidate->isFlatLike()) {
         break;
      }
      const HazardResult haz = perform_hazard_query(&hq, candidate.get(), false);
      if (haz == hazard_fail_exec || haz == hazard_fail_unreorderable) {
         break;
      }
      if (haz != hazard_success) {
         add_to_hazard_query(&hq, candidate.get());
         ctx.mv.downwards_skip(cursor);
         continue;
      }
      const MoveResult res = ctx.mv.downwards_move(cursor);
      if (res == move_fail_ssa || res == move_fail_rar) {
         add_to_hazard_query(&hq, candidate.get());
         ctx.mv.downwards_skip(cursor);
         continue;
      } else if (res == move_fail_pressure) {
         break;
      }
      k++;
   }
}
void
schedule_VMEM_store(sched_ctx& ctx, Block* block, Instruction* current, int idx)
{
   const int max_distance = ctx.last_VMEM_store_idx + VMEM_STORE_CLAUSE_MAX_GRAB_DIST;
   ctx.last_VMEM_store_idx = idx;
   if (max_distance < idx) {
      return;
   }
   hazard_query hq;
   init_hazard_query(ctx, &hq);
   DownwardsCursor cursor = ctx.mv.downwards_init(idx, true);
   for (int16_t k = 0; k < VMEM_STORE_CLAUSE_MAX_GRAB_DIST;) {
      if (cursor.source_idx < 0) {
         break;
      }
      aco_ptr<Instruction>& candidate = block->instructions[cursor.source_idx];
      if (!is_reorderable(candidate.get())) {
         break;
      }
      if (should_form_clause(current, candidate.get())) {
         const HazardResult haz = perform_hazard_query(&hq, candidate.get(), false);
         if (haz == hazard_success) {
            ctx.mv.downwards_move_clause(cursor);
         }
         break;
      }
      if (candidate->isVMEM() || candidate->isFlatLike()) {
         break;
      }
      add_to_hazard_query(&hq, candidate.get());
      ctx.mv.downwards_skip(cursor);
      k += static_cast<int16_t>(get_likely_cost(candidate.get()));
   }
}
void
schedule_block(sched_ctx& ctx, Program* program, Block* block)
{
   ctx.last_SMEM_dep_idx = 0;
   ctx.last_VMEM_store_idx = INT_MIN;
   ctx.last_SMEM_stall = INT16_MIN;
   ctx.mv.block = block;
   for (unsigned idx = 0; idx < block->instructions.size(); idx++) {
      Instruction* current = block->instructions[idx].get();
      if (current->opcode == aco_opcode::p_logical_end) {
         break;
      }
      if (block->kind & block_kind_export_end && current->isEXP() && ctx.schedule_pos_exports) {
         const unsigned target = current->exp().dest;
         if (target >= V_008DFC_SQ_EXP_POS && target < V_008DFC_SQ_EXP_PRIM) {
            ctx.mv.current = current;
            schedule_position_export(ctx, block, current, static_cast<int>(idx));
         }
      }
      if (current->definitions.empty()) {
         if ((current->isVMEM() || current->isFlatLike()) && program->gfx_level >= GFX11) {
            ctx.mv.current = current;
            schedule_VMEM_store(ctx, block, current, static_cast<int>(idx));
         }
         continue;
      }
      if (current->isVMEM() || current->isFlatLike()) {
         ctx.mv.current = current;
         schedule_VMEM(ctx, block, current, static_cast<int>(idx));
      }
      if (current->isSMEM()) {
         ctx.mv.current = current;
         schedule_SMEM(ctx, block, current, static_cast<int>(idx));
      }
      if (current->isLDSDIR() || (current->isDS() && !current->ds().gds)) {
         ctx.mv.current = current;
         schedule_LDS(ctx, block, current, static_cast<int>(idx));
      }
   }
   block->register_demand = block->live_in_demand;
   for (const aco_ptr<Instruction>& instr : block->instructions) {
      block->register_demand.update(instr->register_demand);
   }
}
}
void
schedule_program(Program* program)
{
   RegisterDemand demand;
   for (Block& block : program->blocks) {
      demand.update(block.register_demand);
   }
   demand.vgpr += program->config->num_shared_vgprs / 2;
   demand.update(program->fixed_reg_demand);
   sched_ctx ctx;
   ctx.gfx_level = program->gfx_level;
   ctx.program = program;
   ctx.mv.depends_on.resize(program->peekAllocationId());
   const int wave_factor = program->gfx_level >= GFX10 ? 2 : 1;
   const int wave_minimum = std::max<int>(program->min_waves, 4 * wave_factor);
   const float reg_file_multiple = static_cast<float>(program->dev.physical_vgprs) / (256.0f * static_cast<float>(wave_factor));
   int vgpr_demand = std::max<int>(24, demand.vgpr) + static_cast<int>(12.0f * reg_file_multiple);
   int target_waves = std::max(wave_minimum, program->dev.physical_vgprs / vgpr_demand);
   target_waves = max_suitable_waves(program, std::min<int>(program->num_waves, target_waves));
   assert(target_waves >= program->min_waves);
   ctx.mv.max_registers = get_addr_regs_from_waves(program, target_waves);
   ctx.mv.max_registers.vgpr -= 2;
   ctx.occupancy_factor = static_cast<int16_t>(target_waves / wave_factor);
   if (program->info.hw_stage == AC_HW_NEXT_GEN_GEOMETRY_SHADER) {
      ctx.schedule_pos_exports = program->info.schedule_ngg_pos_exports;
      ctx.schedule_pos_export_div = 4;
   }
   for (Block& block : program->blocks) {
      schedule_block(ctx, program, &block);
   }
   RegisterDemand new_demand;
   for (Block& block : program->blocks) {
      new_demand.update(block.register_demand);
   }
   new_demand.update(program->fixed_reg_demand);
   assert(!new_demand.exceeds(ctx.mv.max_registers) ||
          !new_demand.exceeds(program->max_reg_demand));
   update_vgpr_sgpr_demand(program, new_demand);
   if (!validate_live_vars(program)) {
      abort();
   }
}
}
