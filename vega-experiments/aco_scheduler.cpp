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
#include <limits>    // For std::numeric_limits

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
            assert(old_idx != new_idx);

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
            if (from == to) return;
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
      struct sched_ctx;

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
            int last_SMEM_dep_idx;
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

                  return std::max({base_window, l1_window, l2_window});
            }
      };

      static bool
      get_constant_vmem_offset(const Instruction* instr, int64_t& out_off)
      {
            if (!instr)
                  return false;

            /* Most VMEM/FLAT ops: operands[1] holds the immediate offset */
            if (instr->operands.size() > 1 && instr->operands[1].isConstant()) {
                  out_off = instr->operands[1].constantValue();
                  return true;
            }

            /* Some MUBUF variants keep the imm-offset in operand 2 or 3 */
            for (unsigned i = 2; i < instr->operands.size(); ++i) {
                  if (instr->operands[i].isConstant()) {
                        out_off = instr->operands[i].constantValue();
                        return true;
                  }
            }

            return false;
      }

      static bool
      check_l2_page_affinity(const Instruction* i1, const Instruction* i2)
      {
            int64_t off1, off2;
            if (!get_constant_vmem_offset(i1, off1) ||
                  !get_constant_vmem_offset(i2, off2))
                  return false;

            /* 0xFFF = 4095 : mask within 4 KiB page                            */
            return ((off1 ^ off2) & ~int64_t{4095}) == 0;
      }

      static float
      calculate_vega_ilp_boost(const Block* /*blk*/, double mem_ratio)
      {
            float boost =
            getenv("ACO_SCHED_VEGA_ILP_BOOST")
            ? std::clamp(std::atoi(getenv("ACO_SCHED_VEGA_ILP_BOOST")),
                         0, 200) / 100.f
                         : 1.0f;

                         if (mem_ratio > 0.60)      boost *= 1.30f;
                               else if (mem_ratio < 0.20) boost *= 0.90f;

                                     return std::clamp(boost, 0.5f, 2.0f);
      }

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

      bool should_form_clause_vega_enhanced(const Instruction* a,
                                            const Instruction* b,
                                            amd_gfx_level       gfx_level,
                                            const sched_ctx&    ctx)
      {
            /* generic, architecture independent check first */
            if (!a || !b || !should_form_clause(a, b))
                  return false;

            /* ---  Vega / GFX9 specific heuristics  -------------------------------- */
            if (gfx_level == GFX9) {
                  /* --- VMEM vs VMEM --------------------------------------------------- */
                  if (a->isVMEM() && b->isVMEM()) {
                        /* same descriptor?  -> always fine                                 */
                        if (a->operands.size() > 0 && b->operands.size() > 0 &&
                              a->operands[0].isTemp()    && b->operands[0].isTemp() &&
                              a->operands[0].tempId()    == b->operands[0].tempId()) {

                              /* check constant offsets to decide *how* good it is…            */
                              if (a->operands.size() > 1 && b->operands.size() > 1 &&
                                    a->operands[1].isConstant() && b->operands[1].isConstant()) {
                                    int64_t off_a = a->operands[1].constantValue();
                              int64_t off_b = b->operands[1].constantValue();
                        int64_t diff  = std::abs(off_a - off_b);

                        /* keep accesses inside 256-byte window – that maps nicely to
                         * 4 consecutive 64-byte cache lines and is what the hardware
                         * prefetcher looks at.                                       */
                        if (diff <= 256) {
                              /* reward perfect sequential pattern even more */
                              unsigned size_a = 0;
                              if (!a->definitions.empty() && a->definitions[0].isTemp())
                                    size_a = a->definitions[0].bytes();
                              else if (a->isVMEM() && a->format == Format::MUBUF) { // Store heuristic
                                    if(a->operands.size() >= 4 && a->operands[3].isTemp())
                                          size_a = a->operands[3].bytes();
                                    else if(a->operands.size() >= 3 && a->operands[2].isTemp())
                                          size_a = a->operands[2].bytes();
                                    else if (!a->operands.empty() && a->operands.back().isTemp())
                                          size_a = a->operands.back().bytes();
                              }

                              if (size_a > 0 && off_b == off_a + size_a)
                                    return true;                /* perfect seq */
                                    return true;                   /* still good   */
                        }
                                    }
                                    /* same descriptor but unknown offsets – still beneficial        */
                                    return true;
                              }
                  }

                  /* --- FLAT vs FLAT --------------------------------------------------- */
                  if (a->isFlatLike() && b->isFlatLike()) {
                        /* identical base pointer => likely same page                       */
                        if (a->operands.size() > 0 && b->operands.size() > 0 &&
                              a->operands[0].isTemp() && b->operands[0].isTemp() &&
                              a->operands[0].tempId() == b->operands[0].tempId())
                              return true;

                        /* otherwise let ‘prefer_clauses’ policy decide                     */
                        return ctx.prefer_clauses;
                  }
            }

            /* default fall-through for non-Vega chips or instructions that do not
             * match our extra heuristics.                                            */
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
                        (((cursor.insert_idx_clause - 1) - cursor.source_idx) >
                        (ctx.occupancy_factor * 4)) /* avoid stalling VMEM queue   */
                        || current_is_16byte_smem)        /* big vs small load hazard   */
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
      schedule_VMEM(sched_ctx& ctx, Block* blk, Instruction* cur, int idx)
      {
            const float aggr = ctx.schedule_aggressiveness;

            int base_win = VMEM_WINDOW_SIZE_CTX;
            if (ctx.gfx_level == GFX9)
                  base_win = VegaMemoryScheduler::calculate_vmem_window(ctx, base_win);

            const int win_size  = int(base_win           * aggr);
            const int max_moves = int(VMEM_MAX_MOVES_CTX * aggr);

            int base_clause = VMEM_CLAUSE_MAX_GRAB_DIST_CTX;
            int clause_dist = int(base_clause * (ctx.prefer_clauses ? aggr : 1.0f));

            if (ctx.gfx_level == GFX9 && idx + 1 < (int)blk->instructions.size()) {
                  auto pat = MemoryPatternAnalyzer::analyze_vmem_pattern(blk, idx + 1, 64);
                  if (pat.pattern == MemoryPatternAnalyzer::PATTERN_SEQUENTIAL)
                        clause_dist *= 3;                 /* even more aggressive         */
                        else if (pat.pattern == MemoryPatternAnalyzer::PATTERN_STRIDED) {
                              if (pat.stride <= 32)        clause_dist *= 2;
                              else if (pat.stride <= 128)  clause_dist = int(clause_dist * 1.7f);
                              else if (pat.stride <= 4096) clause_dist = int(clause_dist * 1.3f);
                        }
            }

            DownwardsCursor cur_d = ctx.mv.downwards_init(idx, true, true);
            hazard_query indep_q, clause_q;
            init_hazard_query(ctx, &indep_q); add_to_hazard_query(&indep_q, cur);
            init_hazard_query(ctx, &clause_q);

            bool only_clauses = false;
            int16_t k = 0;

            /* ---------------- downwards phase -------------------------- */
            for ( ; k < max_moves &&
                  cur_d.source_idx >= 0 &&
                  cur_d.source_idx > idx - win_size; )
            {
                  auto& cand = blk->instructions[cur_d.source_idx];
                  if (!cand) { ctx.mv.downwards_skip(cur_d); continue; }
                  if (cand->opcode == aco_opcode::p_logical_start) break;

                  bool part_clause = false;
                  if ((cur->isVMEM() && cand->isVMEM()) ||
                        (cur->isFlatLike() && cand->isFlatLike()))
                  {
                        int grab = (cur_d.insert_idx_clause - 1) - cur_d.source_idx;
                        if (grab < clause_dist &&
                              should_form_clause_vega_enhanced(cur, cand.get(),
                                                               ctx.gfx_level, ctx))
                        {
                              /* 128-B super gather & 4 KiB page affinity --------------- */
                              int64_t off_first, off_cand;
                              bool c0 = get_constant_vmem_offset(cur, off_first);
                              bool c1 = get_constant_vmem_offset(cand.get(), off_cand);
                              bool same_desc =
                              cur->operands[0].isTemp() && cand->operands[0].isTemp() &&
                              cur->operands[0].tempId() == cand->operands[0].tempId();

                              bool same128 = c0 && c1 && same_desc &&
                              ((off_first ^ off_cand) & ~127) == 0;
                              bool same4k  = c0 && c1 && same_desc &&
                              ((off_first ^ off_cand) & ~4095) == 0;

                              int extra = same128 ? 8 : (same4k ? 4 : 0);
                              part_clause = grab < clause_dist + extra;
                        }
                  }

                  bool can_move = !cand->isVMEM() || part_clause || cand->definitions.empty();
                  HazardResult hz =
                  perform_hazard_query(part_clause ? &clause_q : &indep_q,
                                       cand.get(), false);
                  if (hz == hazard_fail_reorder_ds || hz == hazard_fail_spill ||
                        hz == hazard_fail_reorder_sendmsg || hz == hazard_fail_barrier ||
                        hz == hazard_fail_export)
                        can_move = false;
                  else if (hz != hazard_success)
                        break;

                  if (!can_move) {
                        if (part_clause) add_to_hazard_query(&clause_q, cand.get());
                        add_to_hazard_query(&indep_q, cand.get());
                        ctx.mv.downwards_skip(cur_d);
                        continue;
                  }

                  MoveResult res = ctx.mv.downwards_move(cur_d, part_clause);
                  if (res != move_success) {
                        if (part_clause) add_to_hazard_query(&clause_q, cand.get());
                        add_to_hazard_query(&indep_q, cand.get());
                        if (res == move_fail_pressure) only_clauses = true;
                        ctx.mv.downwards_skip(cur_d);
                        continue;
                  }

                  if (part_clause) {
                        add_to_hazard_query(&clause_q, cand.get());
                        add_to_hazard_query(&indep_q, cand.get());
                  } else {
                        add_to_hazard_query(&indep_q, cand.get());
                        ++k;
                  }
            }

            /* ---------------- upwards phase (unchanged) ---------------- */
            UpwardsCursor up = ctx.mv.upwards_init(idx + 1, true);
            bool found_dep = false;
            init_hazard_query(ctx, &indep_q); add_to_hazard_query(&indep_q, cur);

            for ( ; k < max_moves &&
                  up.source_idx < (int)blk->instructions.size() &&
                  up.source_idx < idx + win_size; )
            {
                  auto& cand = blk->instructions[up.source_idx];
                  if (!cand) { ctx.mv.upwards_skip(up); continue; }
                  if (cand->opcode == aco_opcode::p_logical_end) break;

                  bool is_vmem = cand->isVMEM() || cand->isFlatLike();
                  bool is_dep  = !found_dep && !ctx.mv.upwards_check_deps(up);
                  bool haz_dep = false;

                  if (found_dep) {
                        HazardResult hz =
                        perform_hazard_query(&indep_q, cand.get(), true);
                        if (hz == hazard_fail_reorder_ds      || hz == hazard_fail_spill ||
                              hz == hazard_fail_reorder_vmem_smem ||
                              hz == hazard_fail_reorder_sendmsg ||
                              hz == hazard_fail_barrier         || hz == hazard_fail_export)
                              haz_dep = true;
                        else if (hz != hazard_success)
                              break;
                  }

                  if (is_dep || haz_dep) {
                        if (!found_dep) {
                              ctx.mv.upwards_update_insert_idx(up);
                              add_to_hazard_query(&indep_q, cand.get());
                              found_dep = true;
                        } else
                              add_to_hazard_query(&indep_q, cand.get());

                        ctx.mv.upwards_skip(up);
                        continue;
                  }

                  if (!found_dep && is_vmem && !ctx.prefer_latency_hiding) {
                        for (const Definition& d : cand->definitions)
                              if (d.isTemp()) ctx.mv.depends_on[d.tempId()] = true;
                              add_to_hazard_query(&indep_q, cand.get());
                        ctx.mv.upwards_skip(up);
                        continue;
                  }

                  MoveResult res = ctx.mv.upwards_move(up);
                  if (res == move_fail_ssa || res == move_fail_rar) {
                        add_to_hazard_query(&indep_q, cand.get());
                        ctx.mv.upwards_skip(up);
                        continue;
                  }
                  if (res == move_fail_pressure) break;

                  if (!is_vmem) ++k;
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
      schedule_position_export(sched_ctx& ctx, Block* blk, Instruction* cur, int idx)
      {
            float gfx9_factor = 1.0f;
            if (ctx.gfx_level == GFX9) {
                  int count = 0, scan = 64;
                  int b = std::max(0, idx - scan / 2);
                  int e = std::min<int>(blk->instructions.size(), idx + scan / 2);
                  for (int i = b; i < e; ++i)
                        if (blk->instructions[i] && blk->instructions[i]->isEXP()) ++count;
                        if (count > 4) gfx9_factor *= 1.5f;
                        else if (count <= 2) gfx9_factor *= 0.8f;
            }

            int win_base = POS_EXP_WINDOW_SIZE_CTX / ctx.schedule_pos_export_div;
            int mov_base = POS_EXP_MAX_MOVES_CTX  / ctx.schedule_pos_export_div;

            int win_size = int(win_base * ctx.schedule_aggressiveness * gfx9_factor);
            int max_mv   = int(mov_base * ctx.schedule_aggressiveness * gfx9_factor);

            DownwardsCursor down = ctx.mv.downwards_init(idx, true, false);
            hazard_query hq; init_hazard_query(ctx, &hq); add_to_hazard_query(&hq, cur);

            int16_t k = 0;
            for (; k < max_mv &&
                  down.source_idx >= 0 &&
                  down.source_idx > idx - win_size; )
            {
                  auto& cand = blk->instructions[down.source_idx];
                  if (!cand) { ctx.mv.downwards_skip(down); continue; }

                  if (cand->opcode == aco_opcode::p_logical_start) break;
                  if (cand->isVMEM() || cand->isSMEM() || cand->isFlatLike()) break;

                  HazardResult hz = perform_hazard_query(&hq, cand.get(), false);
                  if (hz == hazard_fail_exec || hz == hazard_fail_unreorderable ||
                        hz == hazard_fail_export) break;
                  if (hz != hazard_success) {
                        add_to_hazard_query(&hq, cand.get());
                        ctx.mv.downwards_skip(down); continue;
                  }

                  /* SALU bias with VALU-consumer look-ahead ------------------ */
                  unsigned salu_bias = 0;
                  if (cand->isSALU()) {
                        bool feeds_valu = false;
                        for (int look = down.source_idx + 1;
                             look < down.source_idx + 8 && look < (int)blk->instructions.size();
                        ++look)
                             {
                                   const auto& future = blk->instructions[look];
                                   if (!future || !future->isVALU()) continue;
                                   for (const Definition&  def : cand->definitions)
                                         for (const Operand& op : future->operands)
                                               if (def.isTemp() && op.isTemp() &&
                                                     def.tempId() == op.tempId()) { feeds_valu = true; break; }
                                                     if (feeds_valu) break;
                             }
                             salu_bias = feeds_valu ? 12 : 8;
                  }

                  unsigned cost = std::max(1u, get_likely_cost(cand.get()) - salu_bias);

                  MoveResult res = ctx.mv.downwards_move(down, false);
                  if (res == move_fail_ssa || res == move_fail_rar) {
                        add_to_hazard_query(&hq, cand.get());
                        ctx.mv.downwards_skip(down); continue;
                  }
                  if (res == move_fail_pressure) break;

                  k += cost;
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
      schedule_block(sched_ctx& ctx, Program* prog, Block* blk)
      {
            ctx.last_SMEM_dep_idx = 0;
            ctx.last_SMEM_stall   = INT16_MIN;
            ctx.mv.block          = blk;

            /* ---- local ratios ------------------------------------------ */
            unsigned mem_i = 0, alu_i = 0;
            for (const auto& ins : blk->instructions)
                  if (ins)
                        (ins->isVMEM() || ins->isFlatLike() || ins->isSMEM())
                        ? ++mem_i : ++alu_i;

                  const double mem_ratio_blk =
                  double(mem_i) / double(std::max(1u, mem_i + alu_i));

            /* save globals */
            const float aggr_g   = ctx.schedule_aggressiveness;
            const bool  clause_g = ctx.prefer_clauses;
            const bool  lat_g    = ctx.prefer_latency_hiding;

            /* local knobs */
            float boost = calculate_vega_ilp_boost(blk, mem_ratio_blk);
            ctx.schedule_aggressiveness =
            std::clamp(0.5f + 1.0f * float(mem_ratio_blk), 0.5f, 1.5f) * boost;

            ctx.prefer_latency_hiding = mem_ratio_blk > 0.25;
            ctx.prefer_clauses        = mem_ratio_blk > 0.15;

            /* scheduling loop (unchanged except for knob usage) ---------- */
            unsigned num_stores = 0;
            for (unsigned i = 0; i < blk->instructions.size(); ++i) {
                  Instruction* cur = blk->instructions[i].get();
                  if (!cur) continue;
                  if (cur->opcode == aco_opcode::p_logical_end) break;

                  if ((blk->kind & block_kind_export_end) &&
                        cur->isEXP() && ctx.schedule_pos_exports) {
                        unsigned tgt = cur->exp().dest;
                  if (tgt >= V_008DFC_SQ_EXP_POS && tgt < V_008DFC_SQ_EXP_PRIM) {
                        ctx.mv.current = cur;
                        schedule_position_export(ctx, blk, cur, i);
                  }
                        }

                        if (cur->definitions.empty()) {
                              if (cur->isVMEM() || cur->isFlatLike()) ++num_stores;
                              continue;
                        }

                        ctx.mv.current = cur;
                        if (cur->isVMEM() || cur->isFlatLike())
                              schedule_VMEM(ctx, blk, cur, i);
                  else if (cur->isSMEM())
                        schedule_SMEM(ctx, blk, cur, i);
                  else if (cur->isLDSDIR() || (cur->isDS() && !cur->ds().gds))
                        schedule_LDS(ctx, blk, cur, i);
            }

            if (num_stores > 1 &&
                  (prog->gfx_level >= GFX11 ||
                  (prog->gfx_level == GFX9 && ctx.prefer_clauses))) {
                  for (int s = int(blk->instructions.size()) - 1; s >= 0; --s) {
                        Instruction* st = blk->instructions[s].get();
                        if (!st || !st->definitions.empty()) continue;
                        if (!(st->isVMEM() || st->isFlatLike())) continue;
                        ctx.mv.current = st;
                        s -= schedule_VMEM_store(ctx, blk, st, s);
                  }
                  }

                  blk->register_demand = blk->live_in_demand;
                  for (const auto& ins : blk->instructions)
                        if (ins) blk->register_demand.update(ins->register_demand);

                        /* restore globals */
                        ctx.schedule_aggressiveness = aggr_g;
                  ctx.prefer_clauses          = clause_g;
                  ctx.prefer_latency_hiding   = lat_g;
      }

      void
      schedule_program(Program* program)
      {
            if (!program)
                  return;

            /* 1. global ALU/MEM ratio------------------------------------- */
            unsigned mem_i = 0, alu_i = 0;
            for (const Block& b : program->blocks)
                  for (const auto& ins : b.instructions)
                        if (ins)
                              (ins->isVMEM() || ins->isFlatLike() || ins->isSMEM())
                              ? ++mem_i : ++alu_i;

                        const unsigned tot_i = mem_i + alu_i;
                  const double   mem_ratio_gl = tot_i ? double(mem_i) / double(tot_i) : 0.0;

            /* 2. current register demand --------------------------------- */
            RegisterDemand demand;
            for (const Block& b : program->blocks)
                  demand.update(b.register_demand);

            /* 3. context skeleton ---------------------------------------- */
            sched_ctx ctx;
            ctx.gfx_level = program->gfx_level;
            ctx.mv.depends_on.resize            (program->peekAllocationId());
            ctx.mv.RAR_dependencies.resize      (program->peekAllocationId());
            ctx.mv.RAR_dependencies_clause.resize(program->peekAllocationId());

            const int   wave_factor       = program->gfx_level >= GFX10 ? 2 : 1;
            const float reg_file_multiple =
            program->dev.physical_vgprs > 0
            ? program->dev.physical_vgprs / (256.0f * wave_factor) : 1.0f;
            const int wave_minimum =
            std::max<int>(program->min_waves,
                          int(4 * wave_factor * reg_file_multiple));

            /* 4. sophisticated Vega spare solver ------------------------- */
            int vgpr_spare = 12;
            if (program->gfx_level == GFX9) {
                  static constexpr std::array<int,9> vega_thr{24,32,40,48,64,80,128,170,256};
                  const int env_bias = getenv("ACO_SCHED_VEGA_SPARE_BIAS") ?
                  atoi(getenv("ACO_SCHED_VEGA_SPARE_BIAS")) : 0;

                  double best_cost = std::numeric_limits<double>::max();

                  for (int spare = 0; spare <= 16; ++spare) {
                        int needed_vgpr = ((demand.vgpr + spare + 3) / 4) * 4;
                        if (!needed_vgpr)
                              continue;

                        int waves = program->dev.physical_vgprs / needed_vgpr;
                        waves     = std::clamp(waves, 2, 10);
                        if (waves < program->min_waves)
                              continue;

                        /* --- new guard: keep at least 6 waves for FS/CS ---------------- */
                        if ((program->stage == fragment_fs || program->stage == compute_cs) &&
                              waves < 6)
                              continue;

                        /* cost model --------------------------------------------------- */
                        double reg_gain = double(needed_vgpr - demand.vgpr);
                        double occ_pen  = double(10 - waves);
                        double occ_coef = mem_ratio_gl * 1.5 + 0.5;

                        double cost = (10.0 / (reg_gain + 1.0)) + occ_coef * occ_pen;
                        for (int t : vega_thr)
                              if (demand.vgpr <= t && needed_vgpr > t)
                                    cost += (t == 64 ? 4.0 : 2.0);

                        if (cost < best_cost) { best_cost = cost; vgpr_spare = spare; }
                  }
                  vgpr_spare = std::clamp(vgpr_spare + env_bias, 0, 16);
            }

            if (program->gfx_level >= GFX10 && program->gfx_level != GFX9)
                  vgpr_spare = 12;

            /* 5. derive target waves ------------------------------------- */
            int vgpr_need =
            std::max<int>(24, demand.vgpr) + int(vgpr_spare * reg_file_multiple);

            if (vgpr_need <= 0)
                  vgpr_need = program->dev.physical_vgprs ? program->dev.physical_vgprs : 256;

            int waves_vgpr = program->dev.physical_vgprs / vgpr_need;
            int sgpr_per_wave = (demand.sgpr + 15) & ~15;
            int waves_sgpr    = sgpr_per_wave
            ? program->dev.physical_sgprs / sgpr_per_wave
            : program->num_waves;

            int target_waves =
            std::clamp<int>(std::min(waves_vgpr, waves_sgpr),
                            wave_minimum, program->num_waves);
            target_waves = max_suitable_waves(program, target_waves);

            /* 6. finalise ctx -------------------------------------------- */
            ctx.mv.max_registers = get_addr_regs_from_waves(program, target_waves);
            ctx.mv.max_registers.vgpr = std::max(0, ctx.mv.max_registers.vgpr - 2);

            ctx.occupancy_factor        = std::max(1, target_waves / wave_factor);
            ctx.schedule_aggressiveness = 1.0f;
            ctx.prefer_clauses          = true;
            ctx.prefer_latency_hiding   = false;

            if (program->info.hw_stage == AC_HW_NEXT_GEN_GEOMETRY_SHADER) {
                  ctx.schedule_pos_exports    = program->info.schedule_ngg_pos_exports;
                  ctx.schedule_pos_export_div = 4;
            }

            /* 7. walk blocks --------------------------------------------- */
            for (Block& b : program->blocks)
                  schedule_block(ctx, program, &b);

            /* 8. update demand / validate -------------------------------- */
            RegisterDemand new_dem;
            for (Block& b : program->blocks)
                  new_dem.update(b.register_demand);

            update_vgpr_sgpr_demand(program, new_dem);
            if (!validate_live_vars(program))
                  abort();
      }

} // namespace aco
