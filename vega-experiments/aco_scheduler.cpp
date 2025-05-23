/*
 * Copyright © 2018 Valve Corporation
 *
 * SPDX-License-Identifier: MIT
 */

#include "aco_builder.h"
#include "aco_ir.h"

#include "common/amdgfxregs.h"

#include <algorithm>
#include <cassert>   // For assert()
#include <cstring>   // For memset
#include <vector>
#include <cmath>     // For std::abs
#include <tuple>     // For std::tuple
#include <initializer_list> // For std::min with initializer list

/* ------------------------------------------------------------------------
 * Forward declarations for helpers that live in other compilation units.
 * ----------------------------------------------------------------------*/
namespace aco {
      // These are assumed to be defined in aco_ir.cpp or similar
      // bool   is_done_sendmsg(amd_gfx_level, const Instruction*);
      // bool   is_pos_prim_export(amd_gfx_level, const Instruction*);
      // memory_sync_info get_sync_info_with_hack(const Instruction*);

      /*  Forward declaration for the generic clause helper that lives in another
       *  compilation unit.  Required so we can call it in our Vega-enhanced
       *  wrapper. */
      bool should_form_clause(const Instruction* a, const Instruction* b);

} // namespace aco


/* ------------------------------------------------------------------------
 *  Tiny helper: move a single element inside a contiguous container while
 *  keeping the relative order of every other element.  Implemented with
 *  std::rotate() which is linear in the distance moved and requires no
 *  additional storage.
 * ----------------------------------------------------------------------*/
namespace aco {

      template<class Iter>
      static void move_element_impl(Iter begin, int old_idx, int new_idx)
      {
            assert(old_idx >= 0 && new_idx >= 0);
            assert(old_idx != new_idx); // Should not be called if no move is needed
            /* std::rotate expects [first, middle, last) – ‘middle’ is the element that
             * will end up at ‘first’.  We therefore pass either (old, old+1, new) or
             * (new,  old,    old+1) depending on direction. */

            // The original code had a check: if (old_idx == new_idx || old_idx + 1 == new_idx) return;
            // The new_idx == old_idx + 1 for downward moves (new_idx > old_idx) means old+1 is the new end,
            // so std::rotate(begin + old_idx, begin + old_idx + 1, begin + old_idx + 1) is a no-op.
            // The assert(old_idx != new_idx) covers the first part.
            // If new_idx == old_idx + 1, the rotate is `std::rotate(it_old, it_old_plus_1, it_old_plus_1)` which is fine.

            if (new_idx > old_idx)       /* move *down* (towards the back) */
                  std::rotate(begin + old_idx, begin + old_idx + 1, begin + new_idx);
            else                         /* move *up* (towards the front)  */
                  std::rotate(begin + new_idx, begin + old_idx,     begin + old_idx + 1);
      }

      /* Small convenience wrapper so the original call-sites do not have to change
       * if we ever replace the implementation. */
      template<class Iter>
      static inline void move_element(Iter b, int from, int to)
      {
            if (from == to) return; // Explicitly handle no-op for the public alias
            move_element_impl(b, from, to);
      }

} // namespace aco


// Using CTX suffix to denote these are context-dependent and need careful use
#define SMEM_WINDOW_SIZE_CTX    (256 - ctx.occupancy_factor * 16)
#define VMEM_WINDOW_SIZE_CTX    (1024 - ctx.occupancy_factor * 64)
#define POS_EXP_WINDOW_SIZE_CTX 512
#define SMEM_MAX_MOVES_CTX      (128 - ctx.occupancy_factor * 8)
#define VMEM_MAX_MOVES_CTX      (256 - ctx.occupancy_factor * 16)
#define LDSDIR_MAX_MOVES_CTX    10
#define LDS_MAX_MOVES_CTX       32
#define VMEM_CLAUSE_MAX_GRAB_DIST_CTX       (ctx.occupancy_factor * 2)
#define VMEM_STORE_CLAUSE_MAX_GRAB_DIST_CTX (ctx.occupancy_factor * 4)
#define POS_EXP_MAX_MOVES_CTX         512


namespace aco {

      // Forward declarations within aco namespace
      class Block;
      class Program;
      class Instruction;
      // struct sched_ctx; // No longer needed here as it's defined before use by other aco:: structs

      /* ------------------------------------------------------------------
       * 1.  Complete helper data-structures for hazard querying
       * ----------------------------------------------------------------*/
      struct memory_event_set {
            bool     has_control_barrier = false;
            unsigned bar_acquire         = 0;
            unsigned bar_release         = 0;
            unsigned bar_classes         = 0;
            unsigned access_acquire      = 0;
            unsigned access_release      = 0;
            unsigned access_relaxed      = 0;
            unsigned access_atomic       = 0;
      };

      struct hazard_query {
            amd_gfx_level   gfx_level       = {};
            bool            contains_spill  = false;
            bool            contains_sendmsg= false;
            bool            uses_exec       = false;
            bool            writes_exec     = false;
            memory_event_set mem_events;
            unsigned        aliasing_storage      = 0;
            unsigned        aliasing_storage_smem = 0;
      };

      enum HazardResult {
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


      // Core scheduling structures and types
      enum MoveResult {
            move_success,
            move_fail_ssa,
            move_fail_rar,
            move_fail_pressure,
      };

      struct DownwardsCursor {
            int source_idx;
            int insert_idx_clause;
            int insert_idx;
            RegisterDemand clause_demand;
            RegisterDemand total_demand;
            RegisterDemand insert_demand_clause;
            RegisterDemand insert_demand;

            DownwardsCursor(int current_idx, RegisterDemand initial_clause_demand)
            : source_idx(current_idx - 1), insert_idx_clause(current_idx), insert_idx(current_idx + 1),
            clause_demand(initial_clause_demand)
            {}

            inline void verify_invariants(const Block*) {}
      };

      struct UpwardsCursor {
            int source_idx;
            int insert_idx;
            RegisterDemand total_demand;
            RegisterDemand insert_demand;

            UpwardsCursor(int source_idx_) : source_idx(source_idx_)
            {
                  insert_idx = -1;
            }

            bool has_insert_idx() const { return insert_idx != -1; }
            inline void verify_invariants(const Block*) {}
      };

      struct MoveState {
            RegisterDemand max_registers;
            Block* block;
            Instruction* current;
            bool improved_rar;
            std::vector<uint8_t> depends_on;
            std::vector<uint8_t> RAR_dependencies;
            std::vector<uint8_t> RAR_dependencies_clause;

            DownwardsCursor downwards_init(int current_idx, bool improved_rar, bool may_form_clauses);
            MoveResult downwards_move(DownwardsCursor&, bool clause);
            void downwards_skip(DownwardsCursor&);
            UpwardsCursor upwards_init(int source_idx, bool improved_rar);
            bool upwards_check_deps(UpwardsCursor&);
            void upwards_update_insert_idx(UpwardsCursor&);
            MoveResult upwards_move(UpwardsCursor&);
            void upwards_skip(UpwardsCursor&);
      };

      struct sched_ctx {
            amd_gfx_level gfx_level;
            int16_t occupancy_factor;
            int16_t last_SMEM_stall;
            int last_SMEM_dep_idx; // Changed from int to int16_t in one version, ensure consistency if this was intended. Original was int.
            MoveState mv;
            bool schedule_pos_exports = true;
            unsigned schedule_pos_export_div = 1;
            float schedule_aggressiveness = 1.0f;
            bool prefer_clauses = true;
            bool prefer_latency_hiding = false;
      };

      struct VegaMemoryScheduler {
            static constexpr int VEGA_VMEM_ISSUE_CYCLES = 4;
            static constexpr int VEGA_L2_LATENCY = 100;
            static constexpr int VEGA_L1_LATENCY = 16;      /* LDS / texture cache */

            static int calculate_vmem_window(const sched_ctx& ctx, int base_window) {
                  int effective_waves = std::min(static_cast<int>(ctx.occupancy_factor), 8);
                  int min_latency_hiding_slots = VEGA_L2_LATENCY / VEGA_VMEM_ISSUE_CYCLES;
                  int wave_scaling_factor = std::max(1, (10 - effective_waves) / 2);

                  /* We use a dual-window model: one window big enough to hide L1 misses,
                   * one for L2 misses and pick the larger of the two, then compare
                   * against the caller supplied base_window.                                                  */
                  int l2_window = min_latency_hiding_slots * wave_scaling_factor;
                  int l1_window = (VEGA_L1_LATENCY / VEGA_VMEM_ISSUE_CYCLES) * wave_scaling_factor;

                  return std::max({base_window, l1_window, l2_window}); // Use initializer list for std::max
            }
      };

      struct MemoryPatternAnalyzer {
            enum AccessPattern {
                  PATTERN_SEQUENTIAL,
                  PATTERN_STRIDED,
                  PATTERN_RANDOM,
                  PATTERN_UNKNOWN
            };

            struct PatternInfo {
                  AccessPattern pattern;
                  int stride;
                  int access_size;
            };

            static PatternInfo analyze_vmem_pattern(const Block* block, int start_idx, int window_depth) {
                  std::vector<std::tuple<unsigned, int64_t, int>> accesses;

                  for (int i = start_idx; i < std::min(static_cast<int>(block->instructions.size()), start_idx + window_depth); i++) {
                        Instruction* instr = block->instructions[i].get();
                        if (instr && instr->isVMEM() && instr->operands.size() > 1 && instr->operands[0].isTemp()) {
                              if (instr->operands[1].isConstant()) {
                                    unsigned desc_id = instr->operands[0].tempId();
                                    int64_t offset = instr->operands[1].constantValue();
                                    int size = 0;
                                    if (!instr->definitions.empty() && instr->definitions[0].isTemp()) {
                                          size = instr->definitions[0].bytes();
                                    } else if (instr->isVMEM() && instr->format == Format::MUBUF) {
                                          if (instr->operands.size() >= 4 && instr->operands[3].isTemp()){
                                                size = instr->operands[3].bytes();
                                          } else if (instr->operands.size() >= 3 && instr->operands[2].isTemp()){
                                                size = instr->operands[2].bytes();
                                          } else if (!instr->operands.empty() && instr->operands.back().isTemp()) {
                                                size = instr->operands.back().bytes();
                                          }
                                    }

                                    if (size > 0)
                                          accesses.push_back({desc_id, offset, size});
                              }
                        }
                  }

                  if (accesses.size() < 2)
                        return {PATTERN_UNKNOWN, 0, 0};

                  bool is_sequential = true;
                  bool is_strided = true;
                  int common_stride = -1;
                  int first_access_size = std::get<2>(accesses[0]);

                  for (size_t i = 1; i < accesses.size(); i++) {
                        if (std::get<0>(accesses[i]) != std::get<0>(accesses[i-1])) {
                              is_sequential = false;
                              is_strided = false;
                              break;
                        }

                        int64_t prev_op_offset = std::get<1>(accesses[i-1]);
                        int prev_op_size = std::get<2>(accesses[i-1]);
                        int64_t prev_op_end = prev_op_offset + prev_op_size;
                        int64_t current_op_start = std::get<1>(accesses[i]);

                        int64_t gap = current_op_start - prev_op_end;

                        if (gap != 0) {
                              is_sequential = false;
                        }

                        if (is_strided) {
                              if (common_stride == -1) {
                                    common_stride = gap;
                              } else if (gap != common_stride) {
                                    is_strided = false;
                              }
                        }
                  }

                  if (is_sequential)
                        return {PATTERN_SEQUENTIAL, 0, first_access_size};
                  else if (is_strided && common_stride >= 0 && common_stride <= 256)
                        return {PATTERN_STRIDED, common_stride, first_access_size};
                  else
                        return {PATTERN_RANDOM, 0, 0};
            }
      };


      // Hazard query helper function implementations
      void
      init_hazard_query(const sched_ctx& ctx, hazard_query* query)
      {
            if (!query) return;
            query->gfx_level            = ctx.gfx_level;
            query->contains_spill       = false;
            query->contains_sendmsg     = false;
            query->uses_exec            = false;
            query->writes_exec          = false;
            std::memset(&query->mem_events, 0, sizeof(query->mem_events));
            query->aliasing_storage      = 0;
            query->aliasing_storage_smem = 0;
      }

      bool is_done_sendmsg(amd_gfx_level gfx_level, const Instruction* instr)
      {
            if (!instr) return false;
            if (gfx_level <= GFX10_3 && instr->opcode == aco_opcode::s_sendmsg)
                  return (instr->salu().imm & sendmsg_id_mask) == sendmsg_gs_done;
            return false;
      }

      bool is_pos_prim_export(amd_gfx_level gfx_level, const Instruction* instr)
      {
            if (!instr) return false;
            return instr->opcode == aco_opcode::exp && instr->exp().dest >= V_008DFC_SQ_EXP_POS &&
            instr->exp().dest <= V_008DFC_SQ_EXP_PRIM && gfx_level >= GFX10;
      }

      memory_sync_info get_sync_info_with_hack(const Instruction* instr)
      {
            if (!instr) return memory_sync_info();
            memory_sync_info sync = get_sync_info(instr);
            if (instr->isSMEM() && !instr->operands.empty() && instr->operands[0].isTemp() && instr->operands[0].bytes() == 16) {
                  sync.storage = (storage_class)(sync.storage | storage_buffer);
                  sync.semantics =
                  (memory_semantics)((sync.semantics | semantic_private) & ~semantic_can_reorder);
            }
            return sync;
      }


      void
      add_memory_event(amd_gfx_level gfx_level, memory_event_set* set, Instruction* instr,
                       memory_sync_info* sync)
      {
            if (!instr || !sync || !set) return;
            set->has_control_barrier |= is_done_sendmsg(gfx_level, instr);
            set->has_control_barrier |= is_pos_prim_export(gfx_level, instr);
            if (instr->opcode == aco_opcode::p_barrier) {
                  Pseudo_barrier_instruction& bar = instr->barrier();
                  if (bar.sync.semantics & semantic_acquire)
                        set->bar_acquire |= bar.sync.storage;
                  if (bar.sync.semantics & semantic_release)
                        set->bar_release |= bar.sync.storage;
                  set->bar_classes |= bar.sync.storage;

                  set->has_control_barrier |= bar.exec_scope > scope_invocation;
            }

            if (!sync->storage)
                  return;

            if (sync->semantics & semantic_acquire)
                  set->access_acquire |= sync->storage;
            if (sync->semantics & semantic_release)
                  set->access_release |= sync->storage;

            if (!(sync->semantics & semantic_private)) {
                  if (sync->semantics & semantic_atomic)
                        set->access_atomic |= sync->storage;
                  else
                        set->access_relaxed |= sync->storage;
            }
      }

      void
      add_to_hazard_query(hazard_query* query, Instruction* instr)
      {
            if (!query || !instr) return;
            if (instr->opcode == aco_opcode::p_spill || instr->opcode == aco_opcode::p_reload)
                  query->contains_spill = true;
            query->contains_sendmsg |= instr->opcode == aco_opcode::s_sendmsg;
            query->uses_exec |= needs_exec_mask(instr);
            for (const Definition& def : instr->definitions) {
                  if (def.isFixed() && def.physReg() == exec)
                        query->writes_exec = true;
            }

            memory_sync_info sync = get_sync_info_with_hack(instr);
            add_memory_event(query->gfx_level, &query->mem_events, instr, &sync);

            if (!(sync.semantics & semantic_can_reorder)) {
                  unsigned storage = sync.storage;
                  if (storage & (storage_buffer | storage_image))
                        storage |= storage_buffer | storage_image;
                  if (instr->isSMEM())
                        query->aliasing_storage_smem |= storage;
                  else
                        query->aliasing_storage |= storage;
            }
      }

      HazardResult
      perform_hazard_query(hazard_query* query, Instruction* instr, bool upwards)
      {
            if(!query || !instr) return hazard_fail_unreorderable;

            if (!upwards && instr->opcode == aco_opcode::p_exit_early_if_not)
                  return hazard_fail_unreorderable;

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
                        if (def.isFixed() && def.physReg() == exec)
                              return hazard_fail_exec;
                  }
            }
            if (query->writes_exec && needs_exec_mask(instr))
                  return hazard_fail_exec;

            if (instr->isEXP() || instr->opcode == aco_opcode::p_dual_src_export_gfx11)
                  return hazard_fail_export;

            if (instr->opcode == aco_opcode::s_memtime || instr->opcode == aco_opcode::s_memrealtime ||
                  instr->opcode == aco_opcode::s_setprio || instr->opcode == aco_opcode::s_getreg_b32 ||
                  instr->opcode == aco_opcode::p_shader_cycles_hi_lo_hi ||
                  instr->opcode == aco_opcode::p_init_scratch ||
                  instr->opcode == aco_opcode::p_jump_to_epilog ||
                  instr->opcode == aco_opcode::s_sendmsg_rtn_b32 ||
                  instr->opcode == aco_opcode::s_sendmsg_rtn_b64 ||
                  instr->opcode == aco_opcode::p_end_with_regs || instr->opcode == aco_opcode::s_nop ||
                  instr->opcode == aco_opcode::s_sleep || instr->opcode == aco_opcode::s_trap)
                  return hazard_fail_unreorderable;

            memory_event_set instr_set_local;
            memset(&instr_set_local, 0, sizeof(instr_set_local));
            memory_sync_info sync = get_sync_info_with_hack(instr);
            add_memory_event(query->gfx_level, &instr_set_local, instr, &sync);

            memory_event_set* first = &instr_set_local;
            memory_event_set* second = &query->mem_events;
            if (upwards)
                  std::swap(first, second);

            if ((first->has_control_barrier || first->access_atomic) && second->bar_acquire)
                  return hazard_fail_barrier;
            if (((first->access_acquire || first->bar_acquire) && second->bar_classes) ||
                  ((first->access_acquire | first->bar_acquire) &
                  (second->access_relaxed | second->access_atomic)))
                  return hazard_fail_barrier;

            if (first->bar_release && (second->has_control_barrier || second->access_atomic))
                  return hazard_fail_barrier;
            if ((first->bar_classes && (second->bar_release || second->access_release)) ||
                  ((first->access_relaxed | first->access_atomic) &
                  (second->bar_release | second->access_release)))
                  return hazard_fail_barrier;

            if (first->bar_classes && second->bar_classes)
                  return hazard_fail_barrier;

            unsigned control_classes =
            storage_buffer | storage_image | storage_shared | storage_task_payload;
            if (first->has_control_barrier &&
                  ((second->access_atomic | second->access_relaxed) & control_classes))
                  return hazard_fail_barrier;

            unsigned current_aliasing_storage =
            instr->isSMEM() ? query->aliasing_storage_smem : query->aliasing_storage;
            if ((sync.storage & current_aliasing_storage) && !(sync.semantics & semantic_can_reorder)) {
                  unsigned intersect = sync.storage & current_aliasing_storage;
                  if (intersect & storage_shared)
                        return hazard_fail_reorder_ds;
                  return hazard_fail_reorder_vmem_smem;
            }

            if ((instr->opcode == aco_opcode::p_spill || instr->opcode == aco_opcode::p_reload) &&
                  query->contains_spill)
                  return hazard_fail_spill;

            if (instr->opcode == aco_opcode::s_sendmsg && query->contains_sendmsg)
                  return hazard_fail_reorder_sendmsg;

            return hazard_success;
      }

      // MoveState methods
      DownwardsCursor
      MoveState::downwards_init(int current_idx, bool improved_rar_, bool may_form_clauses)
      {
            improved_rar = improved_rar_;

            std::fill(depends_on.begin(), depends_on.end(), false);
            if (improved_rar) {
                  std::fill(RAR_dependencies.begin(), RAR_dependencies.end(), false);
                  if (may_form_clauses)
                        std::fill(RAR_dependencies_clause.begin(), RAR_dependencies_clause.end(), false);
            }

            if (current && !current->operands.empty()){
                  for (const Operand& op : current->operands) {
                        if (op.isTemp()) {
                              depends_on[op.tempId()] = true;
                              if (improved_rar && op.isFirstKill())
                                    RAR_dependencies[op.tempId()] = true;
                        }
                  }
            }

            assert(block && current_idx >= 0 && current_idx < (int)block->instructions.size());
            assert(block->instructions[current_idx]);
            DownwardsCursor cursor(current_idx, block->instructions[current_idx]->register_demand);

            assert(cursor.insert_idx -1 >=0 && cursor.insert_idx -1 < (int)block->instructions.size());
            assert(block->instructions[cursor.insert_idx - 1]);
            RegisterDemand temp = get_temp_registers(block->instructions[cursor.insert_idx - 1].get());
            cursor.insert_demand = block->instructions[cursor.insert_idx - 1]->register_demand - temp;

            assert(cursor.insert_idx_clause -1 >=0 && cursor.insert_idx_clause -1 < (int)block->instructions.size());
            assert(block->instructions[cursor.insert_idx_clause - 1]);
            temp = get_temp_registers(block->instructions[cursor.insert_idx_clause - 1].get());
            cursor.insert_demand_clause =
            block->instructions[cursor.insert_idx_clause - 1]->register_demand - temp;

            cursor.verify_invariants(block);
            return cursor;
      }

      MoveResult
      MoveState::downwards_move(DownwardsCursor& cursor, bool add_to_clause)
      {
            if (cursor.source_idx < 0 || cursor.source_idx >= (int)block->instructions.size()) return move_fail_ssa;
            aco_ptr<Instruction>& instr = block->instructions[cursor.source_idx];
            if (!instr) return move_fail_ssa;

            for (const Definition& def : instr->definitions)
                  if (def.isTemp() && depends_on[def.tempId()])
                        return move_fail_ssa;

            auto& RAR_deps =
            improved_rar ? (add_to_clause ? RAR_dependencies_clause : RAR_dependencies) : depends_on;
            for (const Operand& op : instr->operands) {
                  if (op.isTemp() && RAR_deps[op.tempId()]) {
                        return move_fail_rar;
                  }
            }

            if (add_to_clause) {
                  for (const Operand& op : instr->operands) {
                        if (op.isTemp()) {
                              depends_on[op.tempId()] = true;
                              if (op.isFirstKill())
                                    RAR_dependencies[op.tempId()] = true;
                        }
                  }
            }

            const int dest_insert_idx = add_to_clause ? cursor.insert_idx_clause : cursor.insert_idx;
            RegisterDemand register_pressure = cursor.total_demand;
            if (!add_to_clause) {
                  register_pressure.update(cursor.clause_demand);
            }

            const RegisterDemand candidate_diff = get_live_changes(instr.get());
            if (RegisterDemand(register_pressure - candidate_diff).exceeds(max_registers))
                  return move_fail_pressure;

            const RegisterDemand temp_regs = get_temp_registers(instr.get());
            const RegisterDemand current_insert_demand =
            add_to_clause ? cursor.insert_demand_clause : cursor.insert_demand;
            const RegisterDemand new_demand = current_insert_demand + temp_regs;
            if (new_demand.exceeds(max_registers))
                  return move_fail_pressure;

            aco::move_element(block->instructions.begin(), cursor.source_idx, dest_insert_idx);

            for (int i = cursor.source_idx; i < dest_insert_idx - 1; i++) {
                  if (i >=0 && i < (int)block->instructions.size() && block->instructions[i])
                        block->instructions[i]->register_demand -= candidate_diff;
            }
            if (dest_insert_idx -1 >=0 && dest_insert_idx -1 < (int)block->instructions.size() && block->instructions[dest_insert_idx-1])
                  block->instructions[dest_insert_idx - 1]->register_demand = new_demand;

            cursor.insert_idx_clause--;
            if (cursor.source_idx != cursor.insert_idx_clause) {
                  cursor.total_demand -= candidate_diff;
            } else {
                  assert(cursor.total_demand == RegisterDemand{});
            }
            if (add_to_clause) {
                  cursor.clause_demand.update(new_demand);
            } else {
                  cursor.clause_demand -= candidate_diff;
                  cursor.insert_demand -= candidate_diff;
                  cursor.insert_idx--;
            }
            cursor.insert_demand_clause -= candidate_diff;

            cursor.source_idx--;
            if (cursor.source_idx >= -1)
                  cursor.verify_invariants(block);
            return move_success;
      }

      void
      MoveState::downwards_skip(DownwardsCursor& cursor)
      {
            if (cursor.source_idx < 0 || cursor.source_idx >= (int)block->instructions.size()) {
                  cursor.source_idx--;
                  return;
            }
            aco_ptr<Instruction>& instr = block->instructions[cursor.source_idx];
            if (!instr) { cursor.source_idx--; return; }


            for (const Operand& op : instr->operands) {
                  if (op.isTemp()) {
                        depends_on[op.tempId()] = true;
                        if (improved_rar && op.isFirstKill()) {
                              RAR_dependencies[op.tempId()] = true;
                              RAR_dependencies_clause[op.tempId()] = true;
                        }
                  }
            }
            cursor.total_demand.update(instr->register_demand);
            cursor.source_idx--;
            if (cursor.source_idx >= -1)
                  cursor.verify_invariants(block);
      }

      UpwardsCursor
      MoveState::upwards_init(int source_idx, bool improved_rar_)
      {
            improved_rar = improved_rar_;

            std::fill(depends_on.begin(), depends_on.end(), false);
            std::fill(RAR_dependencies.begin(), RAR_dependencies.end(), false);

            if(current){
                  for (const Definition& def : current->definitions) {
                        if (def.isTemp())
                              depends_on[def.tempId()] = true;
                  }
            }

            return UpwardsCursor(source_idx);
      }

      bool
      MoveState::upwards_check_deps(UpwardsCursor& cursor)
      {
            if (cursor.source_idx < 0 || cursor.source_idx >= (int)block->instructions.size()) return false;
            aco_ptr<Instruction>& instr = block->instructions[cursor.source_idx];
            if (!instr) return false;

            for (const Operand& op : instr->operands) {
                  if (op.isTemp() && depends_on[op.tempId()])
                        return false;
            }
            return true;
      }

      void
      MoveState::upwards_update_insert_idx(UpwardsCursor& cursor)
      {
            if (cursor.source_idx < 0 || cursor.source_idx >= (int)block->instructions.size()) return;
            cursor.insert_idx = cursor.source_idx;
            if (!block->instructions[cursor.insert_idx]) return;
            cursor.total_demand = block->instructions[cursor.insert_idx]->register_demand;

            if (cursor.insert_idx -1 < 0 || cursor.insert_idx -1 >= (int)block->instructions.size() || !block->instructions[cursor.insert_idx-1]) return;
            const RegisterDemand temp = get_temp_registers(block->instructions[cursor.insert_idx - 1].get());
            cursor.insert_demand = block->instructions[cursor.insert_idx - 1]->register_demand - temp;
      }

      MoveResult
      MoveState::upwards_move(UpwardsCursor& cursor)
      {
            assert(cursor.has_insert_idx());
            if (cursor.source_idx < 0 || cursor.source_idx >= (int)block->instructions.size() ||
                  cursor.insert_idx < 0 || cursor.insert_idx >= (int)block->instructions.size()) return move_fail_ssa;

            aco_ptr<Instruction>& instr = block->instructions[cursor.source_idx];
            if(!instr) return move_fail_ssa;

            for (const Operand& op : instr->operands) {
                  if (op.isTemp() && depends_on[op.tempId()])
                        return move_fail_ssa;
            }

            for (const Operand& op : instr->operands) {
                  if (op.isTemp() && (!improved_rar || op.isFirstKill()) && RAR_dependencies[op.tempId()])
                        return move_fail_rar;
            }

            const RegisterDemand candidate_diff = get_live_changes(instr.get());
            const RegisterDemand temp_regs = get_temp_registers(instr.get());
            if (RegisterDemand(cursor.total_demand + candidate_diff).exceeds(max_registers))
                  return move_fail_pressure;
            const RegisterDemand new_demand = cursor.insert_demand + candidate_diff + temp_regs;
            if (new_demand.exceeds(max_registers))
                  return move_fail_pressure;

            aco::move_element(block->instructions.begin(), cursor.source_idx, cursor.insert_idx);

            if (block->instructions[cursor.insert_idx])
                  block->instructions[cursor.insert_idx]->register_demand = new_demand;

            for (int i = cursor.insert_idx + 1; i <= cursor.source_idx; i++) {
                  if (i >=0 && i < (int)block->instructions.size() && block->instructions[i])
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
            if (cursor.source_idx < 0 || cursor.source_idx >= (int)block->instructions.size()) {
                  cursor.source_idx++;
                  return;
            }

            if (cursor.has_insert_idx()) {
                  aco_ptr<Instruction>& instr = block->instructions[cursor.source_idx];
                  if (!instr) { cursor.source_idx++; return; }

                  for (const Definition& def : instr->definitions) {
                        if (def.isTemp())
                              depends_on[def.tempId()] = true;
                  }
                  for (const Operand& op : instr->operands) {
                        if (op.isTemp())
                              RAR_dependencies[op.tempId()] = true;
                  }
                  cursor.total_demand.update(instr->register_demand);
            }

            cursor.source_idx++;
            cursor.verify_invariants(block);
      }


      unsigned
      get_likely_cost(Instruction* instr)
      {
            if (!instr) return 1;
            if (instr->opcode == aco_opcode::p_split_vector ||
                  instr->opcode == aco_opcode::p_extract_vector) {
                  unsigned cost = 0;
            for (Definition def : instr->definitions) {
                  if (instr->operands.size() > 0 && instr->operands[0].isKill() &&
                        def.regClass().type() == instr->operands[0].regClass().type())
                        continue;
                  cost += def.size();
            }
            return cost;
                  } else if (instr->opcode == aco_opcode::p_create_vector) {
                        unsigned cost = 0;
                        for (Operand op : instr->operands) {
                              if (instr->definitions.size() > 0 &&
                                    op.isTemp() && op.isFirstKill() &&
                                    op.regClass().type() == instr->definitions[0].regClass().type())
                                    continue;
                              cost += op.size();
                        }
                        return cost;
                  } else {
                        return 1;
                  }
      }

      bool should_form_clause_vega_enhanced(const Instruction* instr_a, const Instruction* instr_b, amd_gfx_level agfx_level, const sched_ctx& actx) {
            if (!instr_a || !instr_b) return false;
            if (!should_form_clause(instr_a, instr_b))
                  return false;

            if (agfx_level == GFX9) {
                  if (instr_a->isVMEM() && instr_b->isVMEM()) {
                        if (instr_a->operands.size() > 0 && instr_b->operands.size() > 0 &&
                              instr_a->operands[0].isTemp() && instr_b->operands[0].isTemp() &&
                              instr_a->operands[0].tempId() == instr_b->operands[0].tempId()) {

                              if (instr_a->operands.size() > 1 && instr_b->operands.size() > 1 &&
                                    instr_a->operands[1].isConstant() && instr_b->operands[1].isConstant()) {
                                    int64_t offset_a = instr_a->operands[1].constantValue();
                              int64_t offset_b = instr_b->operands[1].constantValue();
                        int64_t diff = std::abs(offset_a - offset_b);

                        if (diff <= 256) {
                              unsigned size_instr_a = 0;
                              if (!instr_a->definitions.empty() && instr_a->definitions[0].isTemp())
                                    size_instr_a = instr_a->definitions[0].bytes();
                              else if (instr_a->isVMEM() && instr_a->format == Format::MUBUF) {
                                    if(instr_a->operands.size() >= 4 && instr_a->operands[3].isTemp())
                                          size_instr_a = instr_a->operands[3].bytes();
                                    else if(instr_a->operands.size() >= 3 && instr_a->operands[2].isTemp())
                                          size_instr_a = instr_a->operands[2].bytes();
                                    else if (!instr_a->operands.empty() && instr_a->operands.back().isTemp())
                                          size_instr_a = instr_a->operands.back().bytes();
                              }

                              if (size_instr_a > 0 && offset_b == (offset_a + size_instr_a))
                                    return true;
                              return true;
                        }
                                    }
                                    return true;
                              }
                  }

                  if (instr_a->isFlatLike() && instr_b->isFlatLike()) {
                        if (instr_a->operands.size() > 0 && instr_b->operands.size() > 0 &&
                              instr_a->operands[0].isTemp() && instr_b->operands[0].isTemp() &&
                              instr_a->operands[0].tempId() == instr_b->operands[0].tempId())
                              return true;
                        if (actx.prefer_clauses)
                              return true;
                  }
            }
            return true;
      }


      void
      schedule_SMEM(sched_ctx& ctx, Block* block, Instruction* current, int idx)
      {
            assert(idx != 0 && current && block);
            int base_window_size = SMEM_WINDOW_SIZE_CTX;
            int base_max_moves = SMEM_MAX_MOVES_CTX;
            int16_t k = 0;

            if (ctx.gfx_level == GFX9) {
                  bool is_descriptor_load = false;
                  if (current->opcode == aco_opcode::s_load_dwordx4 ||
                        current->opcode == aco_opcode::s_buffer_load_dwordx4) {
                        is_descriptor_load = true;
                        }

                        if (is_descriptor_load) {
                              base_window_size = base_window_size * 3 / 2;
                              base_max_moves = base_max_moves * 3 / 2;
                        }

                        int recent_scalar_loads = 0;
                  int check_distance = std::min(32, idx);
                  if (idx >= check_distance) {
                        for (int i = idx - check_distance; i < idx; i++) {
                              if (i >= 0 && i < (int)block->instructions.size() && block->instructions[i] && block->instructions[i]->isSMEM()) {
                                    recent_scalar_loads++;
                              }
                        }
                  }

                  if (check_distance > 0 && recent_scalar_loads > check_distance / 4) {
                        base_window_size = base_window_size * 2 / 3;
                        base_max_moves = base_max_moves * 2 / 3;
                  }
            }

            int window_size = static_cast<int>(base_window_size * ctx.schedule_aggressiveness);
            int max_moves = static_cast<int>(base_max_moves * ctx.schedule_aggressiveness);

            if (current->opcode == aco_opcode::s_memtime || current->opcode == aco_opcode::s_memrealtime ||
                  current->opcode == aco_opcode::s_sendmsg_rtn_b32 ||
                  current->opcode == aco_opcode::s_sendmsg_rtn_b64)
                  return;

            hazard_query hq_down;
            init_hazard_query(ctx, &hq_down);
            add_to_hazard_query(&hq_down, current);

            DownwardsCursor cursor = ctx.mv.downwards_init(idx, false, false);

            for (/* k init above */; k < max_moves && cursor.source_idx >= 0 && cursor.source_idx > (int)idx - window_size;
                 /* cursor.source_idx updated by skip/move */ ) {
                  assert(cursor.source_idx >= 0 && cursor.source_idx < (int)block->instructions.size());
                  aco_ptr<Instruction>& candidate = block->instructions[cursor.source_idx];
                  if (!candidate) { ctx.mv.downwards_skip(cursor); continue;}


                  bool can_stall_prev_smem =
                  idx <= ctx.last_SMEM_dep_idx && cursor.source_idx < ctx.last_SMEM_dep_idx;
                  if (can_stall_prev_smem && ctx.last_SMEM_stall >= 0)
                        break;

                  if (candidate->opcode == aco_opcode::p_logical_start)
                        break;

                  bool current_is_16byte_smem = (current->opcode == aco_opcode::s_load_dwordx4 || current->opcode == aco_opcode::s_buffer_load_dwordx4);
                  bool candidate_is_8byte_smem_load = (candidate->opcode == aco_opcode::s_load_dwordx2 || candidate->opcode == aco_opcode::s_buffer_load_dwordx2);

                  if ((candidate->isVMEM() || candidate->isFlatLike()) &&
                        ( ((cursor.insert_idx_clause - 1) - cursor.source_idx) > (ctx.occupancy_factor * 4) || current_is_16byte_smem ))
                        break;
                  if (candidate->isSMEM() && current_is_16byte_smem && candidate_is_8byte_smem_load)
                        break;

                  bool can_move_down = true;
                  HazardResult haz = perform_hazard_query(&hq_down, candidate.get(), false);
                  if (haz == hazard_fail_reorder_ds || haz == hazard_fail_spill ||
                        haz == hazard_fail_reorder_sendmsg || haz == hazard_fail_barrier ||
                        haz == hazard_fail_export)
                        can_move_down = false;
                  else if (haz != hazard_success)
                        break;

                  if (candidate->isDS() || !can_move_down) {
                        add_to_hazard_query(&hq_down, candidate.get());
                        ctx.mv.downwards_skip(cursor);
                        continue;
                  }

                  MoveResult res = ctx.mv.downwards_move(cursor, false);
                  if (res == move_fail_ssa || res == move_fail_rar) {
                        add_to_hazard_query(&hq_down, candidate.get());
                        ctx.mv.downwards_skip(cursor);
                        continue;
                  } else if (res == move_fail_pressure) {
                        break;
                  }

                  if (cursor.source_idx +1 < ctx.last_SMEM_dep_idx)
                        ctx.last_SMEM_stall++;
                  k++;
                 }

                 UpwardsCursor up_cursor = ctx.mv.upwards_init(idx + 1, false);
                 bool found_dependency = false;
                 hazard_query hq_up;
                 init_hazard_query(ctx, &hq_up);
                 add_to_hazard_query(&hq_up, current);

                 for ( /* k already partially used */;
                      k < max_moves && up_cursor.source_idx < (int)block->instructions.size() && up_cursor.source_idx < idx + window_size;
            /* source_idx incremented by skip/move */ ) {
                       assert(up_cursor.source_idx >= 0 && up_cursor.source_idx < (int)block->instructions.size());
                       aco_ptr<Instruction>& candidate = block->instructions[up_cursor.source_idx];
                       if (!candidate) { ctx.mv.upwards_skip(up_cursor); continue;}


                       if (candidate->opcode == aco_opcode::p_logical_end)
                             break;

                       bool is_current_candidate_dependency = !found_dependency && !ctx.mv.upwards_check_deps(up_cursor);
                       if (is_current_candidate_dependency && (candidate->isVMEM() || candidate->isFlatLike()))
                             break;

                       bool general_hazard_as_dependency = false;
                       if (found_dependency) {
                             HazardResult haz = perform_hazard_query(&hq_up, candidate.get(), true);
                             if (haz == hazard_fail_reorder_ds || haz == hazard_fail_spill ||
                                   haz == hazard_fail_reorder_sendmsg || haz == hazard_fail_barrier ||
                                   haz == hazard_fail_export)
                                   general_hazard_as_dependency = true;
                             else if (haz != hazard_success)
                                   break;
                       }

                       if (is_current_candidate_dependency || general_hazard_as_dependency) {
                             if (!found_dependency) {
                                   ctx.mv.upwards_update_insert_idx(up_cursor);
                                   add_to_hazard_query(&hq_up, candidate.get());
                                   found_dependency = true;
                             } else {
                                   add_to_hazard_query(&hq_up, candidate.get());
                             }
                             ctx.mv.upwards_skip(up_cursor);
                             continue;
                       }

                       if (!found_dependency && (candidate->isVMEM() || candidate->isFlatLike())) {
                             ctx.mv.upwards_skip(up_cursor);
                             continue;
                       }

                       MoveResult res = ctx.mv.upwards_move(up_cursor);
                       if (res == move_fail_ssa || res == move_fail_rar) {
                             if (res == move_fail_ssa && (candidate->isVMEM() || candidate->isFlatLike()))
                                   break;
                             add_to_hazard_query(&hq_up, candidate.get());
                             ctx.mv.upwards_skip(up_cursor);
                             continue;
                       } else if (res == move_fail_pressure) {
                             break;
                       }
                       k++;
            }

            ctx.last_SMEM_dep_idx = found_dependency ? up_cursor.insert_idx : 0;
            const int smem_base = ctx.gfx_level == GFX9 ? 9 : 10;
            ctx.last_SMEM_stall = smem_base - static_cast<int16_t>(ctx.occupancy_factor) - k;
      }

      void
      schedule_VMEM(sched_ctx& ctx, Block* block, Instruction* current, int idx)
      {
            assert(idx != 0 && current && block);
            int base_window_size = VMEM_WINDOW_SIZE_CTX;
            if (ctx.gfx_level == GFX9) {
                  base_window_size = VegaMemoryScheduler::calculate_vmem_window(ctx, base_window_size);
            }
            int window_size = static_cast<int>(base_window_size * ctx.schedule_aggressiveness);
            int max_moves = static_cast<int>(VMEM_MAX_MOVES_CTX * ctx.schedule_aggressiveness);

            int base_clause_max_grab_dist = VMEM_CLAUSE_MAX_GRAB_DIST_CTX;
            if (ctx.gfx_level == GFX9 && idx + 1 < (int)block->instructions.size()) {
                  auto pattern_info = MemoryPatternAnalyzer::analyze_vmem_pattern(block, idx + 1, 32);
                  if (pattern_info.pattern == MemoryPatternAnalyzer::PATTERN_SEQUENTIAL) {
                        base_clause_max_grab_dist = VMEM_CLAUSE_MAX_GRAB_DIST_CTX * 2;
                  } else if (pattern_info.pattern == MemoryPatternAnalyzer::PATTERN_STRIDED) {
                        if (pattern_info.stride >=0 && pattern_info.stride <= 64) {
                              base_clause_max_grab_dist = (VMEM_CLAUSE_MAX_GRAB_DIST_CTX * 3) / 2;
                        }
                  }
            }
            int clause_max_grab_dist = static_cast<int>(base_clause_max_grab_dist * (ctx.prefer_clauses ? ctx.schedule_aggressiveness : 1.0f));

            bool only_clauses = false;
            int16_t k = 0;

            hazard_query indep_hq;
            hazard_query clause_hq;
            init_hazard_query(ctx, &indep_hq);
            init_hazard_query(ctx, &clause_hq);
            add_to_hazard_query(&indep_hq, current);

            DownwardsCursor cursor = ctx.mv.downwards_init(idx, true, true);

            for (/* k init */; k < max_moves && cursor.source_idx >=0 && cursor.source_idx > (int)idx - window_size;
                 /* cursor.source_idx updated by skip/move */ ) {
                  assert(cursor.source_idx >= 0 && cursor.source_idx < (int)block->instructions.size());
                  aco_ptr<Instruction>& candidate = block->instructions[cursor.source_idx];
                  if (!candidate) { ctx.mv.downwards_skip(cursor); continue;}


                  bool is_vmem_candidate = candidate->isVMEM() || candidate->isFlatLike();

                  if (candidate->opcode == aco_opcode::p_logical_start)
                        break;

                  bool can_stall_prev_smem =
                  idx <= ctx.last_SMEM_dep_idx && cursor.source_idx < ctx.last_SMEM_dep_idx;
                  if (can_stall_prev_smem && ctx.last_SMEM_stall >= 0)
                        break;

                  bool part_of_clause = false;
                  if ( (current->isVMEM() && candidate->isVMEM()) || (current->isFlatLike() && candidate->isFlatLike()) ) {
                        int grab_dist = (cursor.insert_idx_clause -1) - cursor.source_idx;
                        bool basic_clause_ok = should_form_clause_vega_enhanced(current, candidate.get(), ctx.gfx_level, ctx);
                        part_of_clause = grab_dist < clause_max_grab_dist && basic_clause_ok;
                  }

                  bool can_move_down = !is_vmem_candidate || part_of_clause || candidate->definitions.empty();
                  if (only_clauses) {
                        if (part_of_clause) {
                              int current_clause_size = cursor.insert_idx - cursor.insert_idx_clause;
                              int prev_potential_clause_size = 1;
                              while (cursor.source_idx - prev_potential_clause_size >= 0) {
                                    Instruction* prev_instr = block->instructions[cursor.source_idx - prev_potential_clause_size].get();
                                    if (!prev_instr) break;
                                    bool prev_clause_ok = should_form_clause_vega_enhanced(current, prev_instr, ctx.gfx_level, ctx);
                                    if (!prev_clause_ok) break;
                                    prev_potential_clause_size++;
                              }
                              if (prev_potential_clause_size > current_clause_size + 1)
                                    break;
                        } else {
                              can_move_down = false;
                        }
                  }
                  HazardResult haz =
                  perform_hazard_query(part_of_clause ? &clause_hq : &indep_hq, candidate.get(), false);
                  if (haz == hazard_fail_reorder_ds || haz == hazard_fail_spill ||
                        haz == hazard_fail_reorder_sendmsg || haz == hazard_fail_barrier ||
                        haz == hazard_fail_export)
                        can_move_down = false;
                  else if (haz != hazard_success)
                        break;

                  if (!can_move_down) {
                        if (part_of_clause)
                              add_to_hazard_query(&clause_hq, candidate.get());
                        add_to_hazard_query(&indep_hq, candidate.get());
                        ctx.mv.downwards_skip(cursor);
                        continue;
                  }

                  Instruction* candidate_ptr = candidate.get();
                  MoveResult res = ctx.mv.downwards_move(cursor, part_of_clause);
                  if (res == move_fail_ssa || res == move_fail_rar) {
                        if (part_of_clause)
                              add_to_hazard_query(&clause_hq, candidate.get());
                        add_to_hazard_query(&indep_hq, candidate.get());
                        ctx.mv.downwards_skip(cursor);
                        continue;
                  } else if (res == move_fail_pressure) {
                        only_clauses = true;
                        if (part_of_clause)
                              add_to_hazard_query(&clause_hq, candidate.get());
                        add_to_hazard_query(&indep_hq, candidate.get());
                        ctx.mv.downwards_skip(cursor);
                        continue;
                  }

                  if (part_of_clause) {
                        add_to_hazard_query(&clause_hq, candidate_ptr);
                        add_to_hazard_query(&indep_hq, candidate_ptr);
                  } else {
                        add_to_hazard_query(&indep_hq, candidate_ptr);
                        k++;
                  }
                  if (cursor.source_idx +1 < ctx.last_SMEM_dep_idx)
                        ctx.last_SMEM_stall++;
                 }

                 UpwardsCursor up_cursor = ctx.mv.upwards_init(idx + 1, true);
                 bool found_dependency = false;
                 init_hazard_query(ctx, &indep_hq);
                 add_to_hazard_query(&indep_hq, current);

                 for ( /* k already partially used */ ;
                      k < max_moves && up_cursor.source_idx < (int)block->instructions.size() && up_cursor.source_idx < idx + window_size;
            /* source_idx updated by skip/move */ ) {
                       assert(up_cursor.source_idx >= 0 && up_cursor.source_idx < (int)block->instructions.size());
                       aco_ptr<Instruction>& candidate = block->instructions[up_cursor.source_idx];
                       if (!candidate) { ctx.mv.upwards_skip(up_cursor); continue;}

                       bool is_vmem_candidate = candidate->isVMEM() || candidate->isFlatLike();

                       if (candidate->opcode == aco_opcode::p_logical_end)
                             break;

                       bool is_current_candidate_dependency = !found_dependency && !ctx.mv.upwards_check_deps(up_cursor);

                       bool general_hazard_as_dependency = false;
                       if (found_dependency) {
                             HazardResult haz = perform_hazard_query(&indep_hq, candidate.get(), true);
                             if (haz == hazard_fail_reorder_ds || haz == hazard_fail_spill ||
                                   haz == hazard_fail_reorder_vmem_smem || haz == hazard_fail_reorder_sendmsg ||
                                   haz == hazard_fail_barrier || haz == hazard_fail_export)
                                   general_hazard_as_dependency = true;
                             else if (haz != hazard_success)
                                   break;
                       }

                       if (is_current_candidate_dependency || general_hazard_as_dependency) {
                             if (!found_dependency) {
                                   ctx.mv.upwards_update_insert_idx(up_cursor);
                                   add_to_hazard_query(&indep_hq, candidate.get());
                                   found_dependency = true;
                             } else {
                                   add_to_hazard_query(&indep_hq, candidate.get());
                             }
                             ctx.mv.upwards_skip(up_cursor);
                             continue;
                       }

                       if (!found_dependency && is_vmem_candidate && !ctx.prefer_latency_hiding) {
                             for (const Definition& def : candidate->definitions) {
                                   if (def.isTemp())
                                         ctx.mv.depends_on[def.tempId()] = true;
                             }
                             add_to_hazard_query(&indep_hq, candidate.get());
                             ctx.mv.upwards_skip(up_cursor);
                             continue;
                       }

                       MoveResult res = ctx.mv.upwards_move(up_cursor);
                       if (res == move_fail_ssa || res == move_fail_rar) {
                             add_to_hazard_query(&indep_hq, candidate.get());
                             ctx.mv.upwards_skip(up_cursor);
                             continue;
                       } else if (res == move_fail_pressure) {
                             break;
                       }
                       if (!is_vmem_candidate) k++;
            }
      }

      void
      schedule_LDS(sched_ctx& ctx, Block* block, Instruction* current, int idx)
      {
            assert(idx != 0 && current && block);
            int base_window_size = (ctx.gfx_level == GFX9 ? 80 : 64);
            int base_max_moves = current->isLDSDIR() ? LDSDIR_MAX_MOVES_CTX : LDS_MAX_MOVES_CTX;

            if (ctx.gfx_level == GFX9) {
                  base_max_moves = base_max_moves * 3 / 2;
                  if (ctx.occupancy_factor > 8) {
                        base_max_moves = base_max_moves * 2 / 3;
                  }
            }
            int window_size = static_cast<int>(base_window_size * ctx.schedule_aggressiveness);
            int max_moves = static_cast<int>(base_max_moves * ctx.schedule_aggressiveness);
            int16_t k = 0;

            hazard_query hq_down;
            init_hazard_query(ctx, &hq_down);
            add_to_hazard_query(&hq_down, current);

            DownwardsCursor cursor = ctx.mv.downwards_init(idx, true, false);

            for (int i_down = 0; k < max_moves && i_down < window_size && cursor.source_idx >=0; /* i_down incr in loop */ ) {
                  assert(cursor.source_idx < (int)block->instructions.size());
                  aco_ptr<Instruction>& candidate = block->instructions[cursor.source_idx];
                  if (!candidate) { ctx.mv.downwards_skip(cursor); i_down++; continue; }

                  bool is_mem = candidate->isVMEM() || candidate->isFlatLike() || candidate->isSMEM();
                  if (candidate->opcode == aco_opcode::p_logical_start || is_mem)
                        break;

                  if (candidate->isDS() || candidate->isLDSDIR()) {
                        add_to_hazard_query(&hq_down, candidate.get());
                        ctx.mv.downwards_skip(cursor);
                        i_down++;
                        continue;
                  }

                  if (perform_hazard_query(&hq_down, candidate.get(), false) != hazard_success ||
                        ctx.mv.downwards_move(cursor, false) != move_success)
                        break;

                  k++;
                  i_down++;
            }

            bool found_dependency = false;
            int i_up = 0;
            UpwardsCursor up_cursor = ctx.mv.upwards_init(idx + 1, true);
            hazard_query hq_up;
            init_hazard_query(ctx, &hq_up);
            add_to_hazard_query(&hq_up, current);

            bool first_loop_broke_lds = false;
            for (; k < max_moves && i_up < window_size && up_cursor.source_idx < (int)block->instructions.size(); i_up++) {
                  assert(up_cursor.source_idx >= 0 && up_cursor.source_idx < (int)block->instructions.size());
                  aco_ptr<Instruction>& candidate = block->instructions[up_cursor.source_idx];
                  if (!candidate) { ctx.mv.upwards_skip(up_cursor); continue;}

                  bool is_mem = candidate->isVMEM() || candidate->isFlatLike() || candidate->isSMEM();
                  if (candidate->opcode == aco_opcode::p_logical_end || is_mem) {
                        first_loop_broke_lds = true; break;
                  }

                  if (!ctx.mv.upwards_check_deps(up_cursor)) {
                        add_to_hazard_query(&hq_up, candidate.get());
                        ctx.mv.upwards_update_insert_idx(up_cursor);
                        ctx.mv.upwards_skip(up_cursor);
                        found_dependency = true;
                        first_loop_broke_lds = true; break;
                  }
                  add_to_hazard_query(&hq_up, candidate.get());
                  ctx.mv.upwards_skip(up_cursor);
            }

            if (first_loop_broke_lds && i_up < window_size && up_cursor.source_idx < (int)block->instructions.size() && found_dependency) {
                  // i_up is correct for the next loop
            }


            for (; found_dependency && k < max_moves && i_up < window_size && up_cursor.source_idx < (int)block->instructions.size(); i_up++) {
                  assert(up_cursor.source_idx >=0 && up_cursor.source_idx < (int)block->instructions.size());
                  aco_ptr<Instruction>& candidate = block->instructions[up_cursor.source_idx];
                  if (!candidate) { ctx.mv.upwards_skip(up_cursor); continue;}


                  bool is_mem = candidate->isVMEM() || candidate->isFlatLike() || candidate->isSMEM();
                  if (candidate->opcode == aco_opcode::p_logical_end || is_mem)
                        break;

                  HazardResult haz = perform_hazard_query(&hq_up, candidate.get(), true);
                  if (haz == hazard_fail_exec || haz == hazard_fail_unreorderable)
                        break;

                  if (haz != hazard_success || ctx.mv.upwards_move(up_cursor) != move_success) {
                        add_to_hazard_query(&hq_up, candidate.get());
                        ctx.mv.upwards_skip(up_cursor);
                  } else {
                        k++;
                  }
            }
      }

      void
      schedule_position_export(sched_ctx& ctx, Block* block, Instruction* current, int idx)
      {
            assert(idx != 0 && current && block);
            float gfx9_export_aggressiveness_factor = 1.0f;
            if (ctx.gfx_level == GFX9) {
                  int export_count = 0;
                  int scan_window_half = 32;
                  int start_scan = std::max(0, idx - scan_window_half);
                  int end_scan = std::min(static_cast<int>(block->instructions.size()), idx + scan_window_half);

                  for (int i = start_scan; i < end_scan; i++) {
                        if (i == idx) continue;
                        if (i >= 0 && i < (int)block->instructions.size() && block->instructions[i] && block->instructions[i]->isEXP()) {
                              export_count++;
                        }
                  }
                  if (export_count > 4) {
                        gfx9_export_aggressiveness_factor = 1.5f;
                  } else if (export_count > 0 && export_count <= 2 ) {
                        gfx9_export_aggressiveness_factor = 0.8f;
                  }
            }

            int base_window_size = POS_EXP_WINDOW_SIZE_CTX / ctx.schedule_pos_export_div;
            int base_max_moves = POS_EXP_MAX_MOVES_CTX / ctx.schedule_pos_export_div;

            int window_size = static_cast<int>(base_window_size * ctx.schedule_aggressiveness * gfx9_export_aggressiveness_factor);
            int max_moves = static_cast<int>(base_max_moves * ctx.schedule_aggressiveness * gfx9_export_aggressiveness_factor);
            int16_t k = 0;

            DownwardsCursor cursor = ctx.mv.downwards_init(idx, true, false);
            hazard_query hq;
            init_hazard_query(ctx, &hq);
            add_to_hazard_query(&hq, current);

            for (/* k init */; k < max_moves && cursor.source_idx >= 0 && cursor.source_idx > (int)idx - window_size;
                 /* cursor.source_idx updated by skip/move */ ) {
                  assert(cursor.source_idx >= 0 && cursor.source_idx < (int)block->instructions.size());
                  aco_ptr<Instruction>& candidate = block->instructions[cursor.source_idx];
                  if (!candidate) { ctx.mv.downwards_skip(cursor); continue;}


                  if (candidate->opcode == aco_opcode::p_logical_start)
                        break;
                  if (candidate->isVMEM() || candidate->isSMEM() || candidate->isFlatLike())
                        break;

                  HazardResult haz = perform_hazard_query(&hq, candidate.get(), false);
                  if (haz == hazard_fail_exec || haz == hazard_fail_unreorderable || haz == hazard_fail_export)
                        break;

                  if (haz != hazard_success) {
                        add_to_hazard_query(&hq, candidate.get());
                        ctx.mv.downwards_skip(cursor);
                        continue;
                  }

                  MoveResult res = ctx.mv.downwards_move(cursor, false);
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

      unsigned
      schedule_VMEM_store(sched_ctx& ctx, Block* block, Instruction* current, int idx)
      {
            assert(current && block);
            hazard_query hq;
            init_hazard_query(ctx, &hq);

            DownwardsCursor cursor = ctx.mv.downwards_init(idx, true, true);
            int skip = 0;

            int base_clause_max_grab_dist = VMEM_STORE_CLAUSE_MAX_GRAB_DIST_CTX;
            int clause_max_grab_dist = static_cast<int>(base_clause_max_grab_dist *
            ((ctx.gfx_level == GFX9 && ctx.prefer_clauses) ? ctx.schedule_aggressiveness : 1.0f));

            for (int k_dist = 0; k_dist < clause_max_grab_dist && cursor.source_idx >=0;) {
                  assert(cursor.source_idx < (int)block->instructions.size());
                  aco_ptr<Instruction>& candidate = block->instructions[cursor.source_idx];
                  if (!candidate) { ctx.mv.downwards_skip(cursor); if(cursor.source_idx < 0) break; k_dist = (cursor.insert_idx_clause > cursor.source_idx+1) ? (cursor.insert_idx_clause -1) - cursor.source_idx : 0; continue;}


                  if (candidate->opcode == aco_opcode::p_logical_start)
                        break;

                  bool basic_clause_ok = should_form_clause_vega_enhanced(current, candidate.get(), ctx.gfx_level, ctx);
                  if (!basic_clause_ok) {
                        add_to_hazard_query(&hq, candidate.get());
                        ctx.mv.downwards_skip(cursor);
                        if (cursor.source_idx < 0) break;
                        k_dist = (cursor.insert_idx_clause > cursor.source_idx +1) ? (cursor.insert_idx_clause -1) - cursor.source_idx : 0;
                        continue;
                  }

                  HazardResult haz_res = perform_hazard_query(&hq, candidate.get(), false);
                  if (haz_res != hazard_success){
                        if(haz_res == hazard_fail_exec || haz_res == hazard_fail_unreorderable) break;
                        add_to_hazard_query(&hq, candidate.get());
                        ctx.mv.downwards_skip(cursor);
                        if (cursor.source_idx < 0) break;
                        k_dist = (cursor.insert_idx_clause > cursor.source_idx +1) ? (cursor.insert_idx_clause -1) - cursor.source_idx : 0;
                        continue;
                  }

                  MoveResult move_res = ctx.mv.downwards_move(cursor, true);
                  if (move_res != move_success) {
                        break;
                  }

                  add_to_hazard_query(&hq, candidate.get());
                  skip++;
                  if (cursor.source_idx < 0) break;
                  k_dist = (cursor.insert_idx_clause > cursor.source_idx +1) ? (cursor.insert_idx_clause -1) - cursor.source_idx : 0;
            }
            return skip;
      }


      void
      schedule_block(sched_ctx& ctx, Program* program, Block* block)
      {
            assert(program && block);
            ctx.last_SMEM_dep_idx = 0;
            ctx.last_SMEM_stall = INT16_MIN;
            ctx.mv.block = block;

            unsigned num_stores = 0;
            for (unsigned idx = 0; idx < block->instructions.size(); idx++) {
                  Instruction* current = block->instructions[idx].get();
                  if (!current) continue;

                  if (current->opcode == aco_opcode::p_logical_end)
                        break;

                  if (block->kind & block_kind_export_end && current->isEXP() && ctx.schedule_pos_exports) {
                        unsigned target = current->exp().dest;
                        if (target >= V_008DFC_SQ_EXP_POS && target < V_008DFC_SQ_EXP_PRIM) {
                              ctx.mv.current = current;
                              schedule_position_export(ctx, block, current, idx);
                        }
                  }

                  if (current->definitions.empty()) {
                        num_stores += current->isVMEM() || current->isFlatLike() ? 1 : 0;
                        continue;
                  }

                  if (current->isVMEM() || current->isFlatLike()) {
                        ctx.mv.current = current;
                        schedule_VMEM(ctx, block, current, idx);
                  }

                  if (current->isSMEM()) {
                        ctx.mv.current = current;
                        schedule_SMEM(ctx, block, current, idx);
                  }

                  if (current->isLDSDIR() || (current->isDS() && !current->ds().gds)) {
                        ctx.mv.current = current;
                        schedule_LDS(ctx, block, current, idx);
                  }
            }

            if (num_stores > 1 && (program->gfx_level >= GFX11 || (program->gfx_level == GFX9 && ctx.prefer_clauses))) {
                  for (int idx_store = static_cast<int>(block->instructions.size()) - 1; idx_store >= 0; idx_store--) {
                        Instruction* current_store = block->instructions[idx_store].get();
                        if (!current_store) continue;

                        if (!current_store->definitions.empty() || !(current_store->isVMEM() || current_store->isFlatLike()))
                              continue;

                        ctx.mv.current = current_store;
                        idx_store -= schedule_VMEM_store(ctx, block, current_store, idx_store);
                  }
            }

            block->register_demand = block->live_in_demand;
            for (const aco_ptr<Instruction>& instr : block->instructions) {
                  if (instr)
                        block->register_demand.update(instr->register_demand);
            }
      }

      void
      schedule_program(Program* program)
      {
            if (!program) return;

            unsigned mem_instrs  = 0;
            unsigned alu_instrs  = 0;

            for (const Block& blk : program->blocks) {
                  for (const aco_ptr<Instruction>& ins : blk.instructions) {
                        if (!ins) continue;
                        if (ins->isVMEM() || ins->isFlatLike() || ins->isSMEM()) {
                              ++mem_instrs;
                              continue;
                        }
                        if ((ins->isVALU() || ins->isSALU()) ||
                              ins->isDS()        || ins->isEXP()       ||
                              ins->format == Format::VOP3   || ins->format == Format::VOP3P  ||
                              ins->format == Format::SOPK   || ins->format == Format::SOPC   ||
                              ins->format == Format::SOPP) {
                              ++alu_instrs;
                              }
                  }
            }

            const unsigned total_instrs = mem_instrs + alu_instrs;
            const double mem_ratio = total_instrs ? double(mem_instrs) / double(total_instrs) : 0.0;

            RegisterDemand demand;
            for (Block& block : program->blocks)
                  demand.update(block.register_demand);

            aco::sched_ctx ctx;
            ctx.gfx_level = program->gfx_level;
            ctx.mv.depends_on.resize(program->peekAllocationId());
            ctx.mv.RAR_dependencies.resize(program->peekAllocationId());
            ctx.mv.RAR_dependencies_clause.resize(program->peekAllocationId());

            const int wave_factor = program->gfx_level >= GFX10 ? 2 : 1;
            const float reg_file_multiple = program->dev.physical_vgprs > 0 ? program->dev.physical_vgprs / (256.0f * wave_factor) : 1.0f;
            const int wave_minimum = std::max<int>(program->min_waves, static_cast<int>(4 * wave_factor * reg_file_multiple));


            int vgpr_spare = 12;
            if (program->gfx_level == GFX9) {
                  // Using simplified GFX9 vgpr_spare logic based on OpenAI's patch comment for stability
                  vgpr_spare = mem_ratio > 0.40 ? 8 : (mem_ratio > 0.20 ? 10 : 12);
            } else if (program->gfx_level >= GFX10) {
                  vgpr_spare = 12;
            }

            int vgpr_demand_val = std::max<int>(24, demand.vgpr) + static_cast<int>(vgpr_spare * reg_file_multiple);
            if (vgpr_demand_val <= 0) vgpr_demand_val = program->dev.physical_vgprs > 0 ? program->dev.physical_vgprs : 256;

            const int sgpr_limit    = program->dev.physical_sgprs;
            const int sgpr_gran     = 16;
            int sgpr_per_wave       = demand.sgpr;
            sgpr_per_wave           = (sgpr_per_wave + sgpr_gran - 1) & ~(sgpr_gran - 1);
            int sgpr_based_waves    = sgpr_per_wave > 0 ? (sgpr_limit > 0 ? sgpr_limit / sgpr_per_wave : program->num_waves) : program->num_waves;

            int target_waves_calc_vgpr = program->dev.physical_vgprs > 0 && vgpr_demand_val > 0 ? program->dev.physical_vgprs / vgpr_demand_val : wave_minimum;
            int target_waves = std::max(wave_minimum, target_waves_calc_vgpr);
            target_waves = std::min(target_waves, sgpr_based_waves);
            target_waves = max_suitable_waves(program, std::min<int>(program->num_waves, target_waves));

            assert(target_waves >= wave_minimum);
            assert(target_waves > 0);

            ctx.mv.max_registers = get_addr_regs_from_waves(program, target_waves);
            if (ctx.mv.max_registers.vgpr >= 2)
                  ctx.mv.max_registers.vgpr -= 2;
            else if (ctx.mv.max_registers.vgpr == 1)
                  ctx.mv.max_registers.vgpr = 1;
            else
                  ctx.mv.max_registers.vgpr = 0;


            ctx.occupancy_factor = target_waves > 0 && wave_factor > 0 ? target_waves / wave_factor : 1;
            if (ctx.occupancy_factor == 0 && target_waves > 0) ctx.occupancy_factor = 1;

            if (program->gfx_level == GFX9) {
                  if (target_waves <= 4) {
                        ctx.schedule_aggressiveness = 1.5f;
                        ctx.prefer_clauses = true;
                        ctx.prefer_latency_hiding = true;
                  } else if (target_waves <= 6) {
                        ctx.schedule_aggressiveness = 1.2f;
                        ctx.prefer_clauses = true;
                        ctx.prefer_latency_hiding = true;
                  } else if (target_waves <= 8) {
                        ctx.schedule_aggressiveness = 1.0f;
                        ctx.prefer_clauses = true;
                        ctx.prefer_latency_hiding = false;
                  } else {
                        ctx.schedule_aggressiveness = 0.8f;
                        ctx.prefer_clauses = false;
                        ctx.prefer_latency_hiding = false;
                  }
            } else {
                  ctx.schedule_aggressiveness = 1.0f;
                  ctx.prefer_clauses = true;
                  ctx.prefer_latency_hiding = false;
            }

            if (program->info.hw_stage == AC_HW_NEXT_GEN_GEOMETRY_SHADER) {
                  ctx.schedule_pos_exports    = program->info.schedule_ngg_pos_exports;
                  ctx.schedule_pos_export_div = 4;
            } else {
                  ctx.schedule_pos_exports    = true;
                  ctx.schedule_pos_export_div = 1;
            }

            for (Block& block : program->blocks)
                  schedule_block(ctx, program, &block);

            RegisterDemand new_demand;
            for (Block& block : program->blocks)
                  new_demand.update(block.register_demand);
            update_vgpr_sgpr_demand(program, new_demand);

            if (!validate_live_vars(program))
                  abort();
      }

} // namespace aco
