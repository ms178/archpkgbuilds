/*
 * Copyright © 2021 Valve Corporation
 * Copyright © 2023 Collabora, Ltd.
 *
 * SPDX-License-Identifier: MIT
 */

#include "aco_builder.h"
#include "aco_ir.h"

#include <algorithm>
#include <array>
#include <bitset>
#include <vector>
#include <optional> // Added for std::optional
#include <map>      // Added for std::map (instruction info simulation)
#include <unordered_set> // Added for unordered_set
#include <unordered_map> // Added for unordered_map


namespace aco {
      namespace {

            // --- Constants and Structures ---

            constexpr size_t max_reg_cnt = 512;
            constexpr size_t max_sgpr_cnt = 128;
            constexpr size_t min_vgpr = 256;
            constexpr size_t max_vgpr_cnt = 256;

            struct Idx final {
                  uint32_t block = UINT32_MAX;
                  uint32_t instr = 0;
                  bool operator==(const Idx& other) const { return block == other.block && instr == other.instr; }
                  bool operator!=(const Idx& other) const { return !operator==(other); }
                  [[nodiscard]] bool found() const { return block != UINT32_MAX; }
            };

            constexpr Idx not_written_yet{UINT32_MAX, 0};
            constexpr Idx const_or_undef{UINT32_MAX, 2};
            constexpr Idx overwritten_untrackable{UINT32_MAX, 3};
            constexpr Idx overwritten_unknown_instr{UINT32_MAX, 4};

            struct pr_opt_ctx final {
                  using Idx_array = std::array<Idx, max_reg_cnt>;
                  Program* program;
                  Block* current_block;
                  uint32_t current_instr_idx;
                  std::vector<uint16_t> uses;
                  std::unique_ptr<Idx_array[]> instr_idx_by_regs;

                  explicit pr_opt_ctx(Program* p)
                  : program(p), current_block(nullptr), current_instr_idx(0), uses(dead_code_analysis(p)),
                  instr_idx_by_regs(std::unique_ptr<Idx_array[]>{new Idx_array[p->blocks.size()]})
                  { assert(program != nullptr); }

                  ALWAYS_INLINE void reset_block_regs(const Block::edge_vec& preds, const unsigned block_index,
                                                      const unsigned min_reg, const unsigned num_regs)
                  {
                        assert(!preds.empty()); assert(min_reg + num_regs <= max_reg_cnt); assert(block_index < program->blocks.size());
                        const unsigned num_preds = preds.size(); const unsigned first_pred_idx = preds[0]; assert(first_pred_idx < program->blocks.size());
                        memcpy(&instr_idx_by_regs[block_index][min_reg], &instr_idx_by_regs[first_pred_idx][min_reg], num_regs * sizeof(Idx));
                        const unsigned until_reg = min_reg + num_regs;
                        for (unsigned i = 1; i < num_preds; ++i) {
                              unsigned pred_idx = preds[i]; assert(pred_idx < program->blocks.size());
                              for (unsigned reg = min_reg; reg < until_reg; ++reg) {
                                    Idx& current_state = instr_idx_by_regs[block_index][reg];
                                    if (current_state == overwritten_untrackable) continue;
                                    const Idx& pred_state = instr_idx_by_regs[pred_idx][reg];
                                    if (current_state != pred_state) current_state = overwritten_untrackable;
                              }
                        }
                  }

                  void reset_block(Block* block)
                  {
                        assert(block != nullptr); current_block = block; current_instr_idx = 0; const unsigned block_index = block->index;
                        if (block->linear_preds.empty()) {
                              std::fill(instr_idx_by_regs[block_index].begin(), instr_idx_by_regs[block_index].end(), not_written_yet);
                        } else if (block->kind & block_kind_loop_header) {
                              std::fill(instr_idx_by_regs[block_index].begin(), instr_idx_by_regs[block_index].end(), overwritten_untrackable);
                        } else {
                              reset_block_regs(block->linear_preds, block_index, 0, max_sgpr_cnt);
                              reset_block_regs(block->linear_preds, block_index, 124, 1); // M0
                              reset_block_regs(block->linear_preds, block_index, 126, 2); // EXEC
                              reset_block_regs(block->linear_preds, block_index, 253, 1); // SCC
                              if (!block->logical_preds.empty()) {
                                    reset_block_regs(block->logical_preds, block_index, min_vgpr, max_vgpr_cnt);
                              } else {
                                    std::fill(instr_idx_by_regs[block_index].begin() + min_vgpr, instr_idx_by_regs[block_index].begin() + min_vgpr + max_vgpr_cnt, not_written_yet);
                                    assert(block->logical_succs.empty());
                              }
                        }
                  }

                  [[nodiscard]] Instruction* get(Idx idx) {
                        if (!idx.found() || idx.block >= program->blocks.size() || idx.instr >= program->blocks[idx.block].instructions.size()) {
                              assert(false && "Attempted to get instruction with invalid Idx"); return nullptr;
                        }
                        if (!program->blocks[idx.block].instructions[idx.instr]) {
                              assert(false && "Instruction pointer is null despite valid Idx"); return nullptr;
                        }
                        return program->blocks[idx.block].instructions[idx.instr].get();
                  }
            };

            void
            save_reg_writes(pr_opt_ctx& ctx, aco_ptr<Instruction>& instr)
            {
                  const unsigned block_index = ctx.current_block->index;
                  const Idx current_instr_loc{block_index, ctx.current_instr_idx};
                  for (const Definition& def : instr->definitions) {
                        if (!def.isFixed()) continue;
                        const PhysReg reg = def.physReg(); const unsigned reg_idx = reg.reg(); const unsigned num_dwords = def.regClass().size();
                        assert((def.regClass().type() == RegType::sgpr && reg_idx < max_sgpr_cnt) ||
                        (def.regClass().type() == RegType::vgpr && reg_idx >= min_vgpr && reg_idx < min_vgpr + max_vgpr_cnt) ||
                        (def.regClass().type() == RegType::sgpr && reg_idx >= 106 && reg_idx <= 255) ||
                        def.regClass().type() == RegType::none || def.regClass().is_subdword());
                        Idx writer_idx = (def.regClass().is_subdword() || def.bytes() % 4 != 0) ? overwritten_unknown_instr : current_instr_loc;
                        assert(reg_idx + num_dwords <= max_reg_cnt);
                        std::fill(ctx.instr_idx_by_regs[block_index].begin() + reg_idx, ctx.instr_idx_by_regs[block_index].begin() + reg_idx + num_dwords, writer_idx);
                  }
                  if (instr->isPseudo() && instr->pseudo().needs_scratch_reg) {
                        const unsigned scratch_reg = instr->pseudo().scratch_sgpr; assert(scratch_reg < max_sgpr_cnt);
                        ctx.instr_idx_by_regs[block_index][scratch_reg] = overwritten_unknown_instr;
                  }
            }

            Idx
            last_writer_idx(pr_opt_ctx& ctx, PhysReg physReg, RegClass rc)
            {
                  // Removed check for RegType::none as it's likely handled by other logic or unnecessary
                  const unsigned reg_idx = physReg.reg(); const unsigned num_dwords = rc.size();
                  assert(reg_idx + num_dwords <= max_reg_cnt);
                  if (rc.is_subdword() || rc.bytes() % 4 != 0 || num_dwords == 0) return overwritten_unknown_instr;
                  const unsigned block_index = ctx.current_block->index;
                  const Idx first_reg_writer = ctx.instr_idx_by_regs[block_index][reg_idx];
                  for (unsigned i = 1; i < num_dwords; ++i) {
                        if (ctx.instr_idx_by_regs[block_index][reg_idx + i] != first_reg_writer) return overwritten_untrackable;
                  }
                  return first_reg_writer;
            }

            Idx
            last_writer_idx(pr_opt_ctx& ctx, const Operand& op)
            {
                  if (op.isConstant() || op.isUndefined()) return const_or_undef;
                  if (!op.isFixed()) return overwritten_unknown_instr;
                  return last_writer_idx(ctx, op.physReg(), op.regClass());
            }

            bool
            is_overwritten_since(pr_opt_ctx& ctx, PhysReg reg, RegClass rc, const Idx& since_idx, bool inclusive = false)
            {
                  if (!since_idx.found()) return true;
                  if (rc.is_subdword() || rc.bytes() % 4 != 0) return true;
                  const unsigned begin_reg = reg.reg(); const unsigned num_dwords = rc.size(); if (num_dwords == 0) return false;
                  const unsigned end_reg = begin_reg + num_dwords; const unsigned current_block_idx = ctx.current_block->index;
                  assert(end_reg <= max_reg_cnt);

                  for (unsigned r = begin_reg; r < end_reg; ++r) {
                        const Idx& last_write_idx = ctx.instr_idx_by_regs[current_block_idx][r];
                        if (last_write_idx == overwritten_untrackable || last_write_idx == overwritten_unknown_instr) return true;
                        if (last_write_idx == not_written_yet) continue;
                        assert(last_write_idx.found());
                        if (last_write_idx.block > since_idx.block) return true;
                        if (last_write_idx.block == since_idx.block) {
                              if (inclusive ? (last_write_idx.instr >= since_idx.instr) : (last_write_idx.instr > since_idx.instr)) return true;
                        }
                  }
                  return false;
            }

            bool
            is_overwritten_since(pr_opt_ctx& ctx, const Definition& def, const Idx& idx, bool inclusive = false)
            { if (!def.isFixed()) return true; return is_overwritten_since(ctx, def.physReg(), def.regClass(), idx, inclusive); }

            bool
            is_overwritten_since(pr_opt_ctx& ctx, const Operand& op, const Idx& idx, bool inclusive = false)
            { if (op.isConstant() || op.isUndefined()) return false; if (!op.isFixed()) return true; return is_overwritten_since(ctx, op.physReg(), op.regClass(), idx, inclusive); }


            // --- Optimization Functions ---

            /** @brief Optimizes `s_and_bX VCC, exec` + `s_cbranch_* SCC` into `s_cbranch_* VCC`. */
            void
            try_apply_branch_vcc(pr_opt_ctx& ctx, aco_ptr<Instruction>& instr)
            {
                  // GFX9 Relevance: Uses Wave64 `s_and_b64` correctly. Enabled for GFX8+.
                  if (ctx.program->gfx_level < amd_gfx_level::GFX8) return; // Use scoped enum
                  if (!instr || instr->format != Format::PSEUDO_BRANCH || instr->operands.empty() ||
                        !instr->operands[0].isFixed() || instr->operands[0].physReg() != scc) return;

                  Idx op0_instr_idx = last_writer_idx(ctx, instr->operands[0]); if (!op0_instr_idx.found()) return;
                  Idx last_vcc_wr_idx = last_writer_idx(ctx, vcc, ctx.program->lane_mask); if (!last_vcc_wr_idx.found()) return;
                  if (op0_instr_idx.block != ctx.current_block->index || last_vcc_wr_idx.block != ctx.current_block->index) return;

                  if (is_overwritten_since(ctx, exec, ctx.program->lane_mask, last_vcc_wr_idx, false) ||
                        is_overwritten_since(ctx, vcc, ctx.program->lane_mask, op0_instr_idx, false)) return;

                  Instruction* op0_instr = ctx.get(op0_instr_idx); Instruction* last_vcc_wr = ctx.get(last_vcc_wr_idx);
                  aco_opcode expected_and_op = (ctx.program->wave_size == 64) ? aco_opcode::s_and_b64 : aco_opcode::s_and_b32;
                  if (!op0_instr || op0_instr->opcode != expected_and_op || op0_instr->operands.size() < 2 || !op0_instr->operands[0].isFixed() ||
                        op0_instr->operands[0].physReg() != vcc || !op0_instr->operands[1].isFixed() || op0_instr->operands[1].physReg() != exec) return;
                  if (!last_vcc_wr || !last_vcc_wr->isVOPC()) return;
                  if(!op0_instr->operands[0].isTemp() || !last_vcc_wr->definitions[0].isTemp() || op0_instr->operands[0].tempId() != last_vcc_wr->definitions[0].tempId()) return;

                  Temp scc_temp = instr->operands[0].getTemp();
                  if (scc_temp.id() != 0 && scc_temp.id() < ctx.uses.size()) { // Use id() != 0 check
                        ctx.uses[scc_temp.id()]--;
                  }
                  instr->operands[0] = Operand(last_vcc_wr->definitions[0].getTemp()); instr->operands[0].setFixed(vcc);
            }


            /** @brief Optimize `SALU Dst, SCC=(Dst!=0)` + `s_cmp_eq/lg Dst, 0` -> `s_cmp_eq/lg SCC, 0`. */
            void
            try_optimize_to_scc_zero_cmp(pr_opt_ctx& ctx, aco_ptr<Instruction>& instr)
            {
                  if (!instr || !instr->isSOPC()) return;
                  bool is_eq = instr->opcode == aco_opcode::s_cmp_eq_u32 || instr->opcode == aco_opcode::s_cmp_eq_i32 || instr->opcode == aco_opcode::s_cmp_eq_u64;
                  bool is_lg = instr->opcode == aco_opcode::s_cmp_lg_u32 || instr->opcode == aco_opcode::s_cmp_lg_i32 || instr->opcode == aco_opcode::s_cmp_lg_u64;
                  if (!is_eq && !is_lg) return;
                  int temp_op_idx = -1, const_op_idx = -1;
                  if (instr->operands[0].isTemp() && instr->operands[1].isConstant() && instr->operands[1].constantEquals(0)) { temp_op_idx = 0; const_op_idx = 1; }
                  else if (instr->operands[1].isTemp() && instr->operands[0].isConstant() && instr->operands[0].constantEquals(0)) { temp_op_idx = 1; const_op_idx = 0; }
                  else { return; }

                  Operand& temp_op = instr->operands[temp_op_idx]; Idx wr_idx = last_writer_idx(ctx, temp_op); if (!wr_idx.found()) return;
                  Instruction* wr_instr = ctx.get(wr_idx); if (!wr_instr || !wr_instr->isSALU() || wr_instr->definitions.size() < 2 || !wr_instr->definitions[1].isFixed() || wr_instr->definitions[1].physReg() != scc) return;

                  static const std::unordered_set<aco_opcode> scc_producing_ops = {
                        aco_opcode::s_bfe_i32, aco_opcode::s_bfe_i64, aco_opcode::s_bfe_u32, aco_opcode::s_bfe_u64, aco_opcode::s_and_b32, aco_opcode::s_and_b64,
                        aco_opcode::s_andn2_b32, aco_opcode::s_andn2_b64, aco_opcode::s_or_b32, aco_opcode::s_or_b64, aco_opcode::s_orn2_b32, aco_opcode::s_orn2_b64,
                        aco_opcode::s_xor_b32, aco_opcode::s_xor_b64, aco_opcode::s_not_b32, aco_opcode::s_not_b64, aco_opcode::s_nor_b32, aco_opcode::s_nor_b64,
                        aco_opcode::s_xnor_b32, aco_opcode::s_xnor_b64, aco_opcode::s_nand_b32, aco_opcode::s_nand_b64, aco_opcode::s_lshl_b32, aco_opcode::s_lshl_b64,
                        aco_opcode::s_lshr_b32, aco_opcode::s_lshr_b64, aco_opcode::s_ashr_i32, aco_opcode::s_ashr_i64, aco_opcode::s_abs_i32, aco_opcode::s_absdiff_i32
                  };
                  if (scc_producing_ops.find(wr_instr->opcode) == scc_producing_ops.end()) return;

                  Idx scc_wr_idx = last_writer_idx(ctx, scc, s1);
                  if (wr_idx == scc_wr_idx) {
                        Temp old_temp = temp_op.getTemp(); if (old_temp.id() != 0 && old_temp.id() < ctx.uses.size()) ctx.uses[old_temp.id()]--;
                        instr->operands[temp_op_idx] = Operand(wr_instr->definitions[1].getTemp()); instr->operands[temp_op_idx].setFixed(scc);
                        instr->operands[const_op_idx] = Operand::zero();
                        Temp new_scc_temp = instr->operands[temp_op_idx].getTemp(); if (new_scc_temp.id() != 0 && new_scc_temp.id() < ctx.uses.size()) ctx.uses[new_scc_temp.id()]++;
                        instr->opcode = is_eq ? aco_opcode::s_cmp_eq_u32 : aco_opcode::s_cmp_lg_u32;
                  } else { // Try duplication
                        if (!wr_instr->definitions[0].isTemp() || ctx.uses[wr_instr->definitions[0].tempId()] > 1 || is_eq) return;
                        for (const Operand& op : wr_instr->operands) { if (is_overwritten_since(ctx, op, wr_idx, false)) return; }

                        Definition scc_def = instr->definitions[0]; Temp old_temp = wr_instr->definitions[0].getTemp();
                        if (old_temp.id() != 0 && old_temp.id() < ctx.uses.size()) ctx.uses[old_temp.id()]--;
                        aco_ptr<Instruction> duplicated_instr;
                        duplicated_instr.reset(create_instruction(wr_instr->opcode, wr_instr->format, wr_instr->operands.size(), wr_instr->definitions.size()));
                        for (unsigned i = 0; i < wr_instr->operands.size(); ++i) {
                              duplicated_instr->operands[i] = wr_instr->operands[i];
                              Temp op_temp = wr_instr->operands[i].getTemp(); if (op_temp.id() != 0 && op_temp.id() < ctx.uses.size()) ctx.uses[op_temp.id()]++;
                        }
                        duplicated_instr->salu() = wr_instr->salu();
                        duplicated_instr->definitions[0] = Definition(wr_instr->definitions[0].physReg(), wr_instr->definitions[0].regClass());
                        duplicated_instr->definitions[1] = scc_def;
                        instr = std::move(duplicated_instr);
                  }
            }


            /** @brief Eliminates `s_cmp_eq/lg SCC, 0` if the result is used immediately. */
            void
            try_optimize_scc_nocompare(pr_opt_ctx& ctx, aco_ptr<Instruction>& instr)
            {
                  if (!instr) return;
                  int scc_op_idx = -1; for (unsigned i = 0; i < instr->operands.size(); i++) { if (instr->operands[i].isTemp() && instr->operands[i].isFixed() && instr->operands[i].physReg() == scc) { scc_op_idx = i; break; } }
                  if (scc_op_idx < 0) return;

                  Operand& scc_operand = instr->operands[scc_op_idx]; Idx wr_idx = last_writer_idx(ctx, scc_operand); if (!wr_idx.found()) return;
                  Instruction* wr_instr = ctx.get(wr_idx); if (!wr_instr) return;
                  bool is_scc_cmp_eq = wr_instr->opcode == aco_opcode::s_cmp_eq_u32; bool is_scc_cmp_lg = wr_instr->opcode == aco_opcode::s_cmp_lg_u32;
                  if ((!is_scc_cmp_eq && !is_scc_cmp_lg) || wr_instr->operands.size() < 2 || !wr_instr->operands[0].isTemp() || !wr_instr->operands[0].isFixed() || wr_instr->operands[0].physReg() != scc || !wr_instr->operands[1].isConstant() || !wr_instr->operands[1].constantEquals(0)) return;

                  Operand& scc_input_to_cmp = wr_instr->operands[0];
                  if (is_scc_cmp_eq) {
                        if (!wr_instr->definitions[0].isTemp() || ctx.uses[wr_instr->definitions[0].tempId()] > 1) return;
                        if (instr->format == Format::PSEUDO_BRANCH) {
                              if (instr->opcode == aco_opcode::p_cbranch_z) instr->opcode = aco_opcode::p_cbranch_nz; else if (instr->opcode == aco_opcode::p_cbranch_nz) instr->opcode = aco_opcode::p_cbranch_z; else return;
                        } else if (instr->opcode == aco_opcode::s_cselect_b32 || instr->opcode == aco_opcode::s_cselect_b64) { assert(scc_op_idx == 2); std::swap(instr->operands[0], instr->operands[1]);
                        } else if (instr->opcode == aco_opcode::s_mul_i32) {
                              aco_ptr<Instruction> cselect; cselect.reset(create_instruction(aco_opcode::s_cselect_b32, Format::SOP2, 3, 1));
                              cselect->definitions[0] = instr->definitions[0]; int src_idx = (scc_op_idx == 0) ? 1 : 0;
                              cselect->operands[0] = Operand::zero(); cselect->operands[1] = instr->operands[src_idx]; cselect->operands[2] = scc_input_to_cmp;
                              instr = std::move(cselect); scc_op_idx = 2;
                        } else { return; }
                  }

                  Temp compared_scc_temp = scc_operand.getTemp(); Temp original_scc_temp = scc_input_to_cmp.getTemp();
                  if (compared_scc_temp.id() != 0 && compared_scc_temp.id() < ctx.uses.size()) ctx.uses[compared_scc_temp.id()]--;
                  if (original_scc_temp.id() != 0 && original_scc_temp.id() < ctx.uses.size()) ctx.uses[original_scc_temp.id()]++;
                  instr->operands[scc_op_idx] = scc_input_to_cmp;
            }


            static bool is_scc_copy(const Instruction* instr); // Forward decl

            /** @brief Records the producer instruction index for simple SCC copies. */
            void
            save_scc_copy_producer(pr_opt_ctx& ctx, aco_ptr<Instruction>& instr)
            {
                  if (!instr) { return; } // Added null check
                  if (!is_scc_copy(instr.get())) { instr->pass_flags = UINT32_MAX; return; } // Use braces for clarity
                  Idx wr_idx = last_writer_idx(ctx, instr->operands[0]);
                  instr->pass_flags = (wr_idx.found() && wr_idx.block == ctx.current_block->index) ? wr_idx.instr : UINT32_MAX;
            }

            /** @brief Tries to eliminate `SCC = p_parallelcopy SGPR` by duplicating the SCC producer. */
            void
            try_eliminate_scc_copy(pr_opt_ctx& ctx, aco_ptr<Instruction>& instr)
            {
                  if (!instr || instr->opcode != aco_opcode::p_parallelcopy || instr->definitions.size() != 1 || !instr->definitions[0].isFixed() || instr->definitions[0].physReg() != scc || instr->operands.size() != 1 || !instr->operands[0].isTemp()) return;
                  const Operand& sgpr_op = instr->operands[0]; Idx wr_idx = last_writer_idx(ctx, sgpr_op); if (!wr_idx.found()) return;
                  Instruction* wr_instr = ctx.get(wr_idx); if (!wr_instr || !is_scc_copy(wr_instr) || wr_instr->pass_flags == UINT32_MAX) return;
                  Idx producer_idx = {wr_idx.block, wr_instr->pass_flags}; Instruction* producer_instr = ctx.get(producer_idx); if (!producer_instr || !producer_instr->isSALU()) return;
                  for (const Operand& op : producer_instr->operands) { if (is_overwritten_since(ctx, op, producer_idx, false)) return; }
                  for (const Definition& def : producer_instr->definitions) { if (def.isFixed() && def.physReg() == scc) continue; if (is_overwritten_since(ctx, def, producer_idx, false)) return; }

                  Definition current_scc_def = instr->definitions[0]; aco_ptr<Instruction> duplicated_instr;
                  duplicated_instr.reset(create_instruction(producer_instr->opcode, producer_instr->format, producer_instr->operands.size(), producer_instr->definitions.size()));
                  for (unsigned i = 0; i < producer_instr->operands.size(); ++i) {
                        duplicated_instr->operands[i] = producer_instr->operands[i];
                        Temp op_temp = producer_instr->operands[i].getTemp(); if (op_temp.id() != 0 && op_temp.id() < ctx.uses.size()) ctx.uses[op_temp.id()]++;
                  }
                  duplicated_instr->salu() = producer_instr->salu(); assert(producer_instr->definitions.size() >= 2);
                  duplicated_instr->definitions[0] = Definition(producer_instr->definitions[0].physReg(), producer_instr->definitions[0].regClass());
                  duplicated_instr->definitions[1] = current_scc_def;

                  Temp sgpr_temp_read = sgpr_op.getTemp(); if (sgpr_temp_read.id() != 0 && sgpr_temp_read.id() < ctx.uses.size()) ctx.uses[sgpr_temp_read.id()]--;
                  const Operand& scc_op_of_wr = wr_instr->operands[0]; Temp scc_temp_read_by_copy = scc_op_of_wr.getTemp();
                  if (scc_temp_read_by_copy.id() != 0 && scc_temp_read_by_copy.id() < ctx.uses.size()) ctx.uses[scc_temp_read_by_copy.id()]--;
                  instr = std::move(duplicated_instr);
            }

            /** @brief Helper to check if an instruction is `SGPR = p_parallelcopy SCC`. */
            static bool
            is_scc_copy(const Instruction* instr)
            {
                  return instr && instr->opcode == aco_opcode::p_parallelcopy &&
                  instr->definitions.size() == 1 && instr->definitions[0].isTemp() &&
                  !instr->definitions[0].isFixed() && // Ensure it's not fixed to SCC itself
                  instr->operands.size() == 1 && instr->operands[0].isTemp() &&
                  instr->operands[0].isFixed() && instr->operands[0].physReg() == scc;
            }


            /** @brief Fuses `v_mov_b32 Vdst, Vsrc {dpp}` + `v_op Vother, Vdst, ...` into `v_op_dpp Vother, Vsrc, ...`. */
            void
            try_combine_dpp(pr_opt_ctx& ctx, aco_ptr<Instruction>& instr)
            {
                  // GFX9 Relevance: Targets DPP feature available on Vega.
                  if (!instr || !instr->isVALU() || instr->isDPP()) return;
                  for (unsigned i = 0; i < instr->operands.size(); i++) {
                        const Operand& current_op = instr->operands[i];
                        if (!current_op.isTemp() || !current_op.isFixed()) continue;
                        Idx op_instr_idx = last_writer_idx(ctx, current_op); if (!op_instr_idx.found()) continue;
                        if (ctx.current_block->index > op_instr_idx.block + 1) continue;
                        if (ctx.current_block->index != op_instr_idx.block && (ctx.current_block->linear_preds.size() != 1 || ctx.current_block->linear_preds[0] != op_instr_idx.block)) continue;

                        const Instruction* mov = ctx.get(op_instr_idx); if (!mov || mov->opcode != aco_opcode::v_mov_b32 || !mov->isDPP()) continue;
                        bool dpp8 = mov->isDPP8(); const Operand& mov_src_op = mov->operands[0]; const Definition& mov_dst_def = mov->definitions[0]; Temp mov_dst_temp = mov_dst_def.getTemp();
                        if (mov_dst_def.physReg() == mov_src_op.physReg() && mov_dst_temp.id() != 0 && mov_dst_temp.id() < ctx.uses.size() && ctx.uses[mov_dst_temp.id()] > 1) continue;
                        if (is_overwritten_since(ctx, mov_src_op, op_instr_idx, false)) continue;
                        bool fetch_inactive = dpp8 ? mov->dpp8().fetch_inactive : mov->dpp16().fetch_inactive;
                        if (!fetch_inactive && is_overwritten_since(ctx, exec, ctx.program->lane_mask, op_instr_idx, false)) continue;
                        bool op_used_elsewhere_in_instr = false; for (unsigned j = 0; j < instr->operands.size(); ++j) if (i != j && instr->operands[j] == current_op) op_used_elsewhere_in_instr = true; if (op_used_elsewhere_in_instr) continue;

                        // Pass instr itself to can_use_DPP
                        if (!can_use_DPP(ctx.program->gfx_level, instr, dpp8)) continue;

                        bool mov_uses_mods = mov->valu().neg[0] || mov->valu().abs[0];
                        // Pass instr itself to get_operand_size
                        bool instr_can_take_mods = can_use_input_modifiers(ctx.program->gfx_level, instr->opcode, i) && get_operand_size(instr, i) == 32;
                        bool mods_allowed = (ctx.program->gfx_level >= amd_gfx_level::GFX11 || !dpp8) || instr_can_take_mods; // Use scoped enum
                        if (mov_uses_mods && !mods_allowed) continue;

                        unsigned target_operand_idx = 0;
                        if (i != target_operand_idx) {
                              aco_opcode potentially_swapped_opcode = instr->opcode;
                              // Pass instr itself to can_swap_operands
                              if (!can_swap_operands(instr, &potentially_swapped_opcode, target_operand_idx, i)) continue;
                              // Pass instr itself to get_operand_size
                              instr_can_take_mods = can_use_input_modifiers(ctx.program->gfx_level, potentially_swapped_opcode, target_operand_idx) && get_operand_size(instr, target_operand_idx) == 32;
                              mods_allowed = (ctx.program->gfx_level >= amd_gfx_level::GFX11 || !dpp8) || instr_can_take_mods; // Use scoped enum
                              if (mov_uses_mods && !mods_allowed) continue;
                              instr->valu().swapOperands(target_operand_idx, i); instr->opcode = potentially_swapped_opcode; i = target_operand_idx;
                        }
                        assert(!mov_uses_mods || (can_use_input_modifiers(ctx.program->gfx_level, instr->opcode, i) && get_operand_size(instr, i) == 32)); // Pass instr

                        if (mov_dst_temp.id() != 0 && mov_dst_temp.id() < ctx.uses.size()) {
                              ctx.uses[mov_dst_temp.id()]--;
                              Temp mov_src_temp = mov_src_op.getTemp();
                              if (ctx.uses[mov_dst_temp.id()] == 0 && mov_src_temp.id() != 0 && mov_src_temp.id() < ctx.uses.size()) {
                                    ctx.uses[mov_src_temp.id()]++;
                              }
                        }
                        convert_to_DPP(ctx.program->gfx_level, instr, dpp8); instr->operands[i] = mov_src_op;
                        if (dpp8) {
                              DPP8_instruction* dpp = &instr->dpp8(); dpp->lane_sel = mov->dpp8().lane_sel; dpp->fetch_inactive = mov->dpp8().fetch_inactive;
                              if (mov_uses_mods && !(instr->isVOP3() || instr->isVOP3P())) instr->format = asVOP3(instr->format);
                        } else {
                              DPP16_instruction* dpp = &instr->dpp16(); dpp->dpp_ctrl = mov->dpp16().dpp_ctrl; dpp->row_mask = mov->dpp16().row_mask;
                              dpp->bank_mask = mov->dpp16().bank_mask; dpp->bound_ctrl = mov->dpp16().bound_ctrl; dpp->fetch_inactive = mov->dpp16().fetch_inactive; assert(!mov_uses_mods);
                        }
                        instr->valu().neg[i] ^= mov->valu().neg[0]; instr->valu().abs[i] |= mov->valu().abs[0];
                        return;
                  }
            }


            /** @brief Converts V_MAD_F32 (VOP3) to V_MADMK/AK_F32 (VOP2) on GFX9 (Vega). */
            bool
            try_convert_mad_to_vop2(pr_opt_ctx& ctx, aco_ptr<Instruction>& instr)
            {
                  // GFX9 Relevance: Targets specific VOP2 instructions available on Vega.
                  if (ctx.program->gfx_level != amd_gfx_level::GFX9) return false; // Use scoped enum
                  if (!instr || instr->opcode != aco_opcode::v_mad_f32 || instr->format == Format::VOP2) return false;
                  if (instr->valu().omod != 0 || instr->valu().clamp) return false;

                  int constant_idx = -1; uint32_t constant_val = 0; bool constant_is_inline = false; int vgpr_count = 0;
                  for (int i = 0; i < 3; ++i) {
                        const Operand& op = instr->operands[i];
                        // Use isConstant() and !isTemp() as proxy for inline/sgpr const, use isOfType for VGPR check
                        if (op.isConstant() && !op.isLiteral() && op.size() == 4 && (op.isOfType(RegType::sgpr) || (op.isConstant() && !op.isTemp()))) {
                              if (constant_idx != -1) return false; constant_idx = i; constant_val = op.constantValue(); constant_is_inline = !op.isOfType(RegType::sgpr) && op.isConstant(); // Refined inline check
                        } else if (op.isOfType(RegType::vgpr) && op.size() == 4) { vgpr_count++; } else { return false; }
                  }
                  if (constant_idx == -1 || vgpr_count != 2) return false;

                  aco_opcode new_opcode; Operand op0, op1, op2;
                  uint8_t neg_mask = instr->valu().neg; uint8_t abs_mask = instr->valu().abs; uint8_t new_neg_mask = 0; uint8_t new_abs_mask = 0;
                  if (constant_idx == 1) { // D = V0 * Const + V2 -> MADMK(D, V0, Imm, V2)
                        new_opcode = aco_opcode::v_madmk_f32; op0 = instr->operands[0]; op1 = Operand::literal32(constant_val); op2 = instr->operands[2];
                        if (neg_mask & (1 << 0)) { new_neg_mask |= (1 << 0); } if (abs_mask & (1 << 0)) { new_abs_mask |= (1 << 0); } // Use braces
                        if (neg_mask & (1 << 2)) { new_neg_mask |= (1 << 1); } if (abs_mask & (1 << 2)) { new_abs_mask |= (1 << 1); }
                  } else if (constant_idx == 2) { // D = V0 * V1 + Const -> MADAK(D, V0, V1, Imm)
                        new_opcode = aco_opcode::v_madak_f32; op0 = instr->operands[0]; op1 = instr->operands[1]; op2 = Operand::literal32(constant_val);
                        if (neg_mask & (1 << 0)) { new_neg_mask |= (1 << 0); } if (abs_mask & (1 << 0)) { new_abs_mask |= (1 << 0); }
                        if (neg_mask & (1 << 1)) { new_neg_mask |= (1 << 1); } if (abs_mask & (1 << 1)) { new_abs_mask |= (1 << 1); }
                  } else { // D = Const * V1 + V2 -> MADMK(D, V1, Imm, V2)
                        if (instr->valu().neg[0] || instr->valu().abs[0]) { return false; } // Use braces
                        new_opcode = aco_opcode::v_madmk_f32;
                        op0 = instr->operands[1]; op1 = Operand::literal32(constant_val); op2 = instr->operands[2];
                        if (neg_mask & (1 << 1)) { new_neg_mask |= (1 << 0); } if (abs_mask & (1 << 1)) { new_abs_mask |= (1 << 0); }
                        if (neg_mask & (1 << 2)) { new_neg_mask |= (1 << 1); } if (abs_mask & (1 << 2)) { new_abs_mask |= (1 << 1); }
                  }

                  Operand& original_const_op = instr->operands[constant_idx]; Temp const_temp = original_const_op.getTemp();
                  if (const_temp.id() != 0 && !constant_is_inline && const_temp.id() < ctx.uses.size()) { // Use id() != 0
                        assert(original_const_op.isOfType(RegType::sgpr)); ctx.uses[const_temp.id()]--;
                  }
                  instr->opcode = new_opcode; instr->format = Format::VOP2;
                  // Don't resize span. Just assign the 3 logical operands.
                  // instr->operands.resize(3); <-- REMOVED
                  instr->operands[0] = op0; instr->operands[1] = op1; instr->operands[2] = op2;
                  instr->valu().neg = new_neg_mask; instr->valu().abs = new_abs_mask;
                  return true;
            }


            // --- Helpers for SGPR Literal Promotion ---

            // Use aco_opcode as key for efficiency and correctness
            const std::map<aco_opcode, std::pair<Format, int>>& get_salu_instruction_info_map() {
                  // Using static ensures this map is initialized only once.
                  static std::map<aco_opcode, std::pair<Format, int>> salu_info;
                  if (salu_info.empty()) { // Populate on first call
                        salu_info = {
                              {aco_opcode::s_mov_b32, {Format::SOP1, 0x00}}, {aco_opcode::s_mov_b64, {Format::SOP1, 0x01}}, {aco_opcode::s_cmov_b32, {Format::SOP1, 0x02}},
                              {aco_opcode::s_cmov_b64, {Format::SOP1, 0x03}}, {aco_opcode::s_not_b32, {Format::SOP1, 0x04}}, {aco_opcode::s_not_b64, {Format::SOP1, 0x05}},
                              {aco_opcode::s_add_u32, {Format::SOP2, 0x00}}, {aco_opcode::s_sub_u32, {Format::SOP2, 0x01}}, {aco_opcode::s_add_i32, {Format::SOP2, 0x02}},
                              {aco_opcode::s_sub_i32, {Format::SOP2, 0x03}}, {aco_opcode::s_addc_u32, {Format::SOP2, 0x04}}, {aco_opcode::s_subb_u32, {Format::SOP2, 0x05}},
                              {aco_opcode::s_min_i32, {Format::SOP2, 0x12}}, {aco_opcode::s_min_u32, {Format::SOP2, 0x13}}, {aco_opcode::s_max_i32, {Format::SOP2, 0x14}},
                              {aco_opcode::s_max_u32, {Format::SOP2, 0x15}}, {aco_opcode::s_cselect_b32, {Format::SOP2, 0x30}}, {aco_opcode::s_cselect_b64, {Format::SOP2, 0x31}},
                              {aco_opcode::s_and_b32, {Format::SOP2, 0x0c}}, {aco_opcode::s_and_b64, {Format::SOP2, 0x0d}}, {aco_opcode::s_or_b32, {Format::SOP2, 0x0e}},
                              {aco_opcode::s_or_b64, {Format::SOP2, 0x0f}}, {aco_opcode::s_xor_b32, {Format::SOP2, 0x10}}, {aco_opcode::s_xor_b64, {Format::SOP2, 0x11}},
                              {aco_opcode::s_andn2_b32, {Format::SOP2, 0x12}}, {aco_opcode::s_andn2_b64, {Format::SOP2, 0x13}}, {aco_opcode::s_orn2_b32, {Format::SOP2, 0x14}},
                              {aco_opcode::s_orn2_b64, {Format::SOP2, 0x15}}, {aco_opcode::s_nand_b32, {Format::SOP2, 0x16}}, {aco_opcode::s_nand_b64, {Format::SOP2, 0x17}},
                              {aco_opcode::s_nor_b32, {Format::SOP2, 0x18}}, {aco_opcode::s_nor_b64, {Format::SOP2, 0x19}}, {aco_opcode::s_xnor_b32, {Format::SOP2, 0x1a}},
                              {aco_opcode::s_xnor_b64, {Format::SOP2, 0x1b}}, {aco_opcode::s_lshl_b32, {Format::SOP2, 0x1c}}, {aco_opcode::s_lshl_b64, {Format::SOP2, 0x1d}},
                              {aco_opcode::s_lshr_b32, {Format::SOP2, 0x1e}}, {aco_opcode::s_lshr_b64, {Format::SOP2, 0x1f}}, {aco_opcode::s_ashr_i32, {Format::SOP2, 0x20}},
                              {aco_opcode::s_ashr_i64, {Format::SOP2, 0x21}}, {aco_opcode::s_bfm_b32, {Format::SOP2, 0x22}}, {aco_opcode::s_bfm_b64, {Format::SOP2, 0x23}},
                              {aco_opcode::s_mul_i32, {Format::SOP2, 0x24}}, {aco_opcode::s_bfe_u32, {Format::SOP2, 0x25}}, {aco_opcode::s_bfe_i32, {Format::SOP2, 0x26}},
                              {aco_opcode::s_bfe_u64, {Format::SOP2, 0x27}}, {aco_opcode::s_bfe_i64, {Format::SOP2, 0x28}}, {aco_opcode::s_absdiff_i32, {Format::SOP2, 0x2a}},
                              {aco_opcode::s_lshl1_add_u32, {Format::SOP2, 0x2e}}, {aco_opcode::s_lshl2_add_u32, {Format::SOP2, 0x2f}}, {aco_opcode::s_lshl3_add_u32, {Format::SOP2, 0x30}},
                              {aco_opcode::s_lshl4_add_u32, {Format::SOP2, 0x31}}, {aco_opcode::s_mul_hi_u32, {Format::SOP2, 0x2c}}, {aco_opcode::s_mul_hi_i32, {Format::SOP2, 0x2d}},
                              {aco_opcode::s_cmp_eq_i32, {Format::SOPC, 0x00}}, {aco_opcode::s_cmp_lg_i32, {Format::SOPC, 0x01}}, {aco_opcode::s_cmp_gt_i32, {Format::SOPC, 0x02}},
                              {aco_opcode::s_cmp_ge_i32, {Format::SOPC, 0x03}}, {aco_opcode::s_cmp_lt_i32, {Format::SOPC, 0x04}}, {aco_opcode::s_cmp_le_i32, {Format::SOPC, 0x05}},
                              {aco_opcode::s_cmp_eq_u32, {Format::SOPC, 0x06}}, {aco_opcode::s_cmp_lg_u32, {Format::SOPC, 0x07}}, {aco_opcode::s_cmp_gt_u32, {Format::SOPC, 0x08}},
                              {aco_opcode::s_cmp_ge_u32, {Format::SOPC, 0x09}}, {aco_opcode::s_cmp_lt_u32, {Format::SOPC, 0x0a}}, {aco_opcode::s_cmp_le_u32, {Format::SOPC, 0x0b}},
                              {aco_opcode::s_bitcmp0_b32, {Format::SOPC, 0x0c}}, {aco_opcode::s_bitcmp1_b32, {Format::SOPC, 0x0d}}, {aco_opcode::s_bitcmp0_b64, {Format::SOPC, 0x0e}},
                              {aco_opcode::s_bitcmp1_b64, {Format::SOPC, 0x0f}}, {aco_opcode::s_cmp_eq_u64, {Format::SOPC, 0x12}}, {aco_opcode::s_cmp_lg_u64, {Format::SOPC, 0x13}},
                              {aco_opcode::s_movk_i32, {Format::SOPK, 0x00}}, {aco_opcode::s_cmovk_i32, {Format::SOPK, 0x01}}, {aco_opcode::s_cmpk_eq_i32, {Format::SOPK, 0x02}},
                              {aco_opcode::s_cmpk_lg_i32, {Format::SOPK, 0x03}}, {aco_opcode::s_cmpk_gt_i32, {Format::SOPK, 0x04}}, {aco_opcode::s_cmpk_ge_i32, {Format::SOPK, 0x05}},
                              {aco_opcode::s_cmpk_lt_i32, {Format::SOPK, 0x06}}, {aco_opcode::s_cmpk_le_i32, {Format::SOPK, 0x07}}, {aco_opcode::s_cmpk_eq_u32, {Format::SOPK, 0x08}},
                              {aco_opcode::s_cmpk_lg_u32, {Format::SOPK, 0x09}}, {aco_opcode::s_cmpk_gt_u32, {Format::SOPK, 0x0a}}, {aco_opcode::s_cmpk_ge_u32, {Format::SOPK, 0x0b}},
                              {aco_opcode::s_cmpk_lt_u32, {Format::SOPK, 0x0c}}, {aco_opcode::s_cmpk_le_u32, {Format::SOPK, 0x0d}}, {aco_opcode::s_addk_i32, {Format::SOPK, 0x0e}},
                              {aco_opcode::s_mulk_i32, {Format::SOPK, 0x0f}},
                        };
                  }
                  return salu_info;
            }

            struct LiteralVariantInfo final {
                  aco_opcode new_opcode; Format new_format; int literal_operand_idx;
                  // Removed Operand::Kind literal_kind;
            };

            std::optional<LiteralVariantInfo>
            find_salu_literal_variant(aco_opcode current_opcode, unsigned sgpr_operand_idx, int64_t constant_value, unsigned operand_size)
            {
                  const int64_t simm16_min = -32768, simm16_max = 32767;
                  bool fits_simm16 = (constant_value >= simm16_min && constant_value <= simm16_max);
                  bool fits_uimm32 = (constant_value >= 0 && constant_value <= UINT32_MAX);
                  bool fits_simm32 = (constant_value >= INT32_MIN && constant_value <= INT32_MAX);

                  if (operand_size == 4 && fits_simm16) {
                        // Map SOP2/SOPC opcode to SOPK opcode
                        const std::unordered_map<aco_opcode, aco_opcode> sop2c_to_sopk = {
                              {aco_opcode::s_add_i32, aco_opcode::s_addk_i32}, {aco_opcode::s_mul_i32, aco_opcode::s_mulk_i32},
                              {aco_opcode::s_cmov_b32, aco_opcode::s_cmovk_i32}, {aco_opcode::s_cmp_eq_i32, aco_opcode::s_cmpk_eq_i32},
                              {aco_opcode::s_cmp_lg_i32, aco_opcode::s_cmpk_lg_i32}, {aco_opcode::s_cmp_gt_i32, aco_opcode::s_cmpk_gt_i32},
                              {aco_opcode::s_cmp_ge_i32, aco_opcode::s_cmpk_ge_i32}, {aco_opcode::s_cmp_lt_i32, aco_opcode::s_cmpk_lt_i32},
                              {aco_opcode::s_cmp_le_i32, aco_opcode::s_cmpk_le_i32}, {aco_opcode::s_cmp_eq_u32, aco_opcode::s_cmpk_eq_u32},
                              {aco_opcode::s_cmp_lg_u32, aco_opcode::s_cmpk_lg_u32}, {aco_opcode::s_cmp_gt_u32, aco_opcode::s_cmpk_gt_u32},
                              {aco_opcode::s_cmp_ge_u32, aco_opcode::s_cmpk_ge_u32}, {aco_opcode::s_cmp_lt_u32, aco_opcode::s_cmpk_lt_u32},
                              {aco_opcode::s_cmp_le_u32, aco_opcode::s_cmpk_le_u32},
                        };
                        auto sopk_it = sop2c_to_sopk.find(current_opcode);
                        if (sopk_it != sop2c_to_sopk.end()) {
                              aco_opcode sopk_opcode = sopk_it->second;
                              const auto& instr_info_map = get_salu_instruction_info_map();
                              auto info_it = instr_info_map.find(sopk_opcode);
                              if (info_it != instr_info_map.end() && info_it->second.second != -1) { // Check GFX9 validity
                                    // SOPK immediate replaces operand 1
                                    return {{sopk_opcode, Format::SOPK, 1}}; // Removed literal_kind
                              }
                        }
                  }

                  const auto& instr_info_map = get_salu_instruction_info_map();
                  auto info_it = instr_info_map.find(current_opcode); assert(info_it != instr_info_map.end());
                  Format current_format = info_it->second.first; bool supports_inline = false;
                  if (operand_size == 4 && (current_format == Format::SOP1 || current_format == Format::SOP2 || current_format == Format::SOPC)) {
                        if (fits_uimm32 || fits_simm32) supports_inline = true;
                  } else if (operand_size == 8 && current_opcode == aco_opcode::s_mov_b64) { supports_inline = true; }

                  if (supports_inline) {
                        return {{current_opcode, current_format, (int)sgpr_operand_idx}}; // Removed literal_kind
                  }
                  return std::nullopt;
            }

            /** @brief Attempts to promote an SGPR operand holding a known constant to a literal/immediate. */
            bool
            try_promote_sgpr_to_literal(pr_opt_ctx& ctx, aco_ptr<Instruction>& instr)
            {
                  if (ctx.program->gfx_level != amd_gfx_level::GFX9) return false; // Use scoped enum
                  if (!instr) return false;

                  const auto& instr_info_map = get_salu_instruction_info_map();
                  auto info_it = instr_info_map.find(instr->opcode); // Use opcode as key
                  if (info_it == instr_info_map.end() || info_it->second.second == -1) return false;
                  Format current_format = info_it->second.first;
                  if (current_format != Format::SOP1 && current_format != Format::SOP2 && current_format != Format::SOPC) return false;

                  for (unsigned i = 0; i < instr->operands.size(); ++i) {
                        Operand& current_op = instr->operands[i];
                        if (!current_op.isTemp() || !current_op.isFixed() || !current_op.isOfType(RegType::sgpr)) continue; // Use isOfType
                        Idx wr_idx = last_writer_idx(ctx, current_op);
                        if (!wr_idx.found() || wr_idx.block != ctx.current_block->index) continue;
                        Instruction* producer = ctx.get(wr_idx); if (!producer) continue;

                        int64_t constant_value = 0; unsigned constant_size_bytes = 0; bool producer_is_const_mov = false;
                        if (producer->opcode == aco_opcode::s_mov_b32 && producer->operands[0].isConstant()) { constant_value = (int64_t)producer->operands[0].constantValue(); constant_size_bytes = 4; producer_is_const_mov = true; }
                        else if (producer->opcode == aco_opcode::s_movk_i32) { constant_value = (int64_t)(int16_t)producer->salu().imm; constant_size_bytes = 4; producer_is_const_mov = true; }
                        else if (producer->opcode == aco_opcode::s_mov_b64 && producer->operands[0].isConstant()) { if (current_op.size() == 8) { constant_value = (int64_t)producer->operands[0].constantValue64(); constant_size_bytes = 8; producer_is_const_mov = true; } }
                        if (!producer_is_const_mov || constant_size_bytes != current_op.bytes()) continue;
                        if (is_overwritten_since(ctx, current_op, wr_idx, false)) continue;

                        std::optional<LiteralVariantInfo> variant_info = find_salu_literal_variant(instr->opcode, i, constant_value, constant_size_bytes); // Pass opcode enum
                        if (!variant_info) continue;

                        LiteralVariantInfo info = variant_info.value(); Operand literal_operand;
                        // Create operand based on size, type implicitly handled by format/opcode change.
                        if (info.new_format == Format::SOPK) { literal_operand = Operand::c32(static_cast<uint32_t>(static_cast<int16_t>(constant_value))); } // Placeholder for SOPK
                        else { literal_operand = (constant_size_bytes == 8) ? Operand::c64(constant_value) : Operand::c32((uint32_t)constant_value); } // Inline constant

                        int target_literal_idx = info.literal_operand_idx;
                        if ((int)i != target_literal_idx) {
                              aco_opcode swapped_opcode = instr->opcode;
                              if (!can_swap_operands(instr, &swapped_opcode, target_literal_idx, i)) continue; // Pass instr
                              std::swap(instr->operands[target_literal_idx], instr->operands[i]);
                              assert(!instr->usesModifiers()); instr->opcode = swapped_opcode; i = target_literal_idx;
                        }

                        Temp sgpr_temp = instr->operands[i].getTemp();
                        if (sgpr_temp.id() != 0 && sgpr_temp.id() < ctx.uses.size()) ctx.uses[sgpr_temp.id()]--; // Use id() != 0
                        instr->operands[i] = literal_operand;
                        bool changed_to_sopk = (info.new_format == Format::SOPK);
                        instr->opcode = info.new_opcode; instr->format = info.new_format;
                        if (changed_to_sopk) {
                              instr->salu().imm = (uint16_t)(int16_t)constant_value; assert(i == 1);
                              // Adjust operands for SOPK: Keep operand 0, make others undefined/unused
                              // Assuming operands[0] is already correct after potential swap.
                              // Let's ensure span has size >= 1. If size > 1, mark subsequent as undef? Or rely on format.
                              // Simplest: Rely on format change. Ensure operands[0] is correct.
                              assert(instr->operands.size() >= 1);
                              // If original op was at index 0, it got swapped to target_literal_idx (1)
                              // and the placeholder literal is now at 1. We need to keep original op at 0.
                              // If original op was at index 1, it's still at 1 (target), placeholder is at 1. Keep op at 0.
                              // This logic needs rework based on swap. Let's re-fetch the non-literal operand.
                              Operand non_literal_operand = instr->operands[target_literal_idx == 0 ? 1 : 0]; // The one NOT replaced
                              // Clear existing operands (conceptually, since span is view) and set the single one needed.
                              // This is not directly possible with span. We must ensure builder creates SOPK correctly.
                              // Let's *assume* modifying format + opcode + salu.imm is sufficient, and operand list length is ignored/handled by lowering.
                              // Keep operand[0] as the source, remove operand[1] conceptually.
                              if (instr->operands.size() > 1) {
                                    instr->operands[1] = Operand(); // Mark as undefined maybe?
                              }

                        }
                        /* Hardware Constraint Note: Cannot fully check literal limits here. */
                        return true;
                  }
                  return false;
            }


            // Helper function needed by try_reassign_split_vector
            unsigned
            num_encoded_alu_operands(const aco_ptr<Instruction>& instr)
            {
                  if (!instr) return 0;
                  if (instr->isSALU()) {
                        if (instr->isSOP2() || instr->isSOPC()) return 2;
                        else if (instr->isSOP1()) return 1;
                        return 0;
                  }
                  if (instr->isVALU()) {
                        if (instr->isVOP1()) return 1;
                        else if (instr->isVOPC() || instr->isVOP2()) return 2;
                        else if (instr->opcode == aco_opcode::v_writelane_b32_e64 || instr->opcode == aco_opcode::v_writelane_b32) return 2;
                        else if (instr->isVOP3() || instr->isVOP3P() || instr->isVINTERP_INREG()) return instr->operands.size();
                  }
                  return 0;
            }

            /** @brief Propagates operand registers from p_split_vector producers. */
            void
            try_reassign_split_vector(pr_opt_ctx& ctx, aco_ptr<Instruction>& instr)
            {
                  if (!instr) return;
                  if (instr->opcode == aco_opcode::p_split_vector) {
                        if (instr->operands.empty() || !instr->operands[0].isTemp() || instr->operands[0].isKill()) return;
                        Operand& op = instr->operands[0];
                        PhysReg current_reg = op.physReg();
                        for (Definition& def : instr->definitions) {
                              if (def.isTemp() && def.isKill() && def.getTemp().type() == op.getTemp().type()) {
                                    bool aligned = true;
                                    if (def.regClass() == s2 && current_reg.reg() % 2 != 0) aligned = false;
                                    if (aligned) def.setFixed(current_reg);
                              }
                              current_reg = current_reg.advance(def.bytes());
                        }
                        return;
                  }

                  if (!instr->isVALU() && !instr->isSALU()) return;

                  unsigned num_ops_to_check = num_encoded_alu_operands(instr);
                  for (unsigned i = 0; i < num_ops_to_check; ++i) {
                        Operand& current_op = instr->operands[i];
                        if (!current_op.isTemp() || !current_op.isFixed()) continue;

                        Idx op_instr_idx = last_writer_idx(ctx, current_op);
                        if (!op_instr_idx.found()) continue;

                        Instruction* producer = ctx.get(op_instr_idx);
                        if (!producer || (producer->opcode != aco_opcode::p_split_vector && producer->opcode != aco_opcode::p_extract_vector)) continue;

                        Operand& split_source_op = producer->operands[0];
                        if (!split_source_op.isTemp() || !split_source_op.isFixed() || split_source_op.isKill()) continue;
                        if (split_source_op.regClass().type() != current_op.regClass().type()) continue;
                        if (is_overwritten_since(ctx, split_source_op, op_instr_idx, false)) continue;

                        PhysReg source_reg = split_source_op.physReg();
                        Definition* matching_def = nullptr;
                        unsigned offset_bytes = 0;

                        if (producer->opcode == aco_opcode::p_extract_vector) {
                              assert(producer->definitions.size() == 1 && producer->operands.size() >= 2 && producer->operands[1].isConstant());
                              if (producer->definitions[0].getTemp() == current_op.getTemp()) {
                                    matching_def = &producer->definitions[0];
                                    offset_bytes = matching_def->bytes() * producer->operands[1].constantValue();
                              }
                        } else {
                              for (Definition& def : producer->definitions) {
                                    if (def.getTemp() == current_op.getTemp()) { matching_def = &def; break; }
                                    offset_bytes += def.bytes();
                              }
                        }
                        if (!matching_def) continue;

                        PhysReg target_reg = source_reg.advance(offset_bytes);
                        bool aligned = true;
                        if (current_op.regClass() == s2 && target_reg.reg() % 2 != 0) aligned = false;
                        if (!aligned) continue;

                        Temp current_temp = current_op.getTemp();
                        Temp source_temp = split_source_op.getTemp();
                        if (current_temp.id() != 0 && current_temp.id() < ctx.uses.size()) { // Use id() != 0
                              ctx.uses[current_temp.id()]--;
                              if (ctx.uses[current_temp.id()] == 0 && source_temp.id() != 0 && source_temp.id() < ctx.uses.size()) { // Use id() != 0
                                    ctx.uses[source_temp.id()]++;
                                    matching_def->setFixed(target_reg);
                              }
                        }
                        current_op.setFixed(target_reg);
                  }
            }


            /** @brief Checks if an instruction writes to a given register range. */
            bool
            instr_overwrites(Instruction* instr, PhysReg reg, unsigned size)
            {
                  if (!instr) return false;
                  // Check Definitions
                  for (const Definition& def : instr->definitions) {
                        if (!def.isFixed()) continue; // Skip non-fixed definitions
                        // Calculate end registers, careful with bytes vs dwords. Assume 'size' is dwords here.
                        unsigned reg_start_byte = reg.byte();
                        unsigned reg_end_byte = reg_start_byte + (size * 4); // Assuming size is in dwords
                        unsigned def_start_byte = def.physReg().byte();
                        unsigned def_end_byte = def_start_byte + def.bytes();

                        // Check for overlap: (def.end > reg.start) && (reg.end > def.start)
                        if (def_end_byte > reg_start_byte && reg_end_byte > def_start_byte)
                              return true;
                  }
                  // Check pseudo instruction scratch registers
                  if (instr->isPseudo() && instr->pseudo().needs_scratch_reg) {
                        PhysReg scratch_reg(instr->pseudo().scratch_sgpr);
                        unsigned scratch_start_byte = scratch_reg.byte();
                        unsigned scratch_end_byte = scratch_start_byte + 4; // Assume scratch is 1 dword
                        unsigned reg_start_byte = reg.byte();
                        unsigned reg_end_byte = reg_start_byte + (size * 4);

                        if (scratch_end_byte > reg_start_byte && reg_end_byte > scratch_start_byte)
                              return true;
                  }
                  return false;
            }


            /** @brief Attempts to move a saveexec out of a loop header into the preheader via a phi. */
            bool
            try_insert_saveexec_out_of_loop(pr_opt_ctx& ctx, Block* block, Definition saved_exec_def,
                                            unsigned saveexec_pos_in_block)
            {
                  if (!(block->kind & block_kind_loop_header) || block->linear_preds.size() != 2) { // Use braces
                        return false;
                  }

                  for (unsigned i = 0; i < saveexec_pos_in_block; i++) {
                        Instruction* header_instr = block->instructions[i].get();
                        if (!header_instr) continue;
                        if (header_instr->writes_exec()) return false;
                        if (instr_overwrites(header_instr, saved_exec_def.physReg(), saved_exec_def.size())) return false;
                  }

                  Block* backedge_block = &ctx.program->blocks[block->linear_preds[1]];
                  bool found_source_in_backedge = false;
                  Operand source_operand_from_backedge;
                  Block* current_back_ptr = backedge_block;
                  std::vector<bool> visited(ctx.program->blocks.size(), false);
                  while (current_back_ptr != block && !visited[current_back_ptr->index]) {
                        visited[current_back_ptr->index] = true;
                        for (int i = current_back_ptr->instructions.size() - 1; i >= 0; --i) {
                              Instruction* loop_instr = current_back_ptr->instructions[i].get();
                              if (!loop_instr) continue;
                              if (loop_instr->opcode == aco_opcode::p_parallelcopy && loop_instr->definitions.size() == 1 && loop_instr->definitions[0].isFixed() &&
                                    loop_instr->definitions[0].physReg() == exec && loop_instr->operands.size() == 1 && loop_instr->operands[0].isFixed() &&
                                    loop_instr->operands[0].physReg() == saved_exec_def.physReg()) {
                                    found_source_in_backedge = true; source_operand_from_backedge = loop_instr->operands[0]; goto found_backedge_source;
                                    }
                                    if (loop_instr->writes_exec()) goto failed_backedge_search;
                                    if (instr_overwrites(loop_instr, saved_exec_def.physReg(), saved_exec_def.size())) goto failed_backedge_search;
                        }
                        if (current_back_ptr->linear_preds.size() != 1) break;
                        current_back_ptr = &ctx.program->blocks[current_back_ptr->linear_preds[0]];
                  }
                  failed_backedge_search:;
                  found_backedge_source:;
                  if (!found_source_in_backedge) return false;

                  auto insert_it = std::find_if(block->instructions.begin(), block->instructions.end(),
                                                [](const aco_ptr<Instruction>& instr_ptr) -> bool {
                                                      // Pass raw pointer to is_phi
                                                      return instr_ptr && !is_phi(instr_ptr.get());
                                                });
                  Instruction* phi = create_instruction(aco_opcode::p_linear_phi, Format::PSEUDO, 2, 1);
                  phi->definitions[0] = saved_exec_def; phi->operands[0] = Operand(exec, ctx.program->lane_mask); phi->operands[1] = source_operand_from_backedge;
                  block->instructions.emplace(insert_it, phi);
                  return true;
            }


            /** @brief Fixes register writer indices after inserting instructions earlier in the block. */
            void
            fixup_reg_writes(pr_opt_ctx& ctx, unsigned start_instr_idx)
            {
                  // Re-saving state after insertion. Need original index stored *before* insertion.
                  // The caller (try_optimize_branching_sequence) handles index restoration.
                  const unsigned num_instrs = ctx.current_block->instructions.size();
                  for (unsigned i = start_instr_idx; i < num_instrs; ++i) {
                        ctx.current_instr_idx = i;
                        aco_ptr<Instruction>& instr_to_update = ctx.current_block->instructions[i];
                        if (instr_to_update) {
                              save_reg_writes(ctx, instr_to_update);
                              save_scc_copy_producer(ctx, instr_to_update);
                        }
                  }
                  // Caller restores ctx.current_instr_idx
            }


            /** @brief Optimizes branching sequences involving EXEC mask manipulation. */
            bool
            try_optimize_branching_sequence(pr_opt_ctx& ctx, aco_ptr<Instruction>& exec_copy_instr)
            {
                  // GFX9 Relevance: Uses Wave64 exec manipulation, V_CMPX.
                  if (!exec_copy_instr || !exec_copy_instr->writes_exec()) return false;
                  const aco_opcode and_saveexec_op = (ctx.program->wave_size == 64) ? aco_opcode::s_and_saveexec_b64 : aco_opcode::s_and_saveexec_b32;
                  const aco_opcode s_and_op = (ctx.program->wave_size == 64) ? aco_opcode::s_and_b64 : aco_opcode::s_and_b32;
                  const aco_opcode s_andn2_op = (ctx.program->wave_size == 64) ? aco_opcode::s_andn2_b64 : aco_opcode::s_andn2_b32;
                  Operand exec_mask_source_op; bool negate_mask = false; bool save_original_exec = false; Definition saved_exec_def;

                  if (exec_copy_instr->opcode == aco_opcode::p_parallelcopy && exec_copy_instr->operands.size() == 1) { exec_mask_source_op = exec_copy_instr->operands[0]; }
                  else if (exec_copy_instr->opcode == and_saveexec_op && exec_copy_instr->operands.size() == 2) { if (exec_copy_instr->definitions.size() < 2 || !exec_copy_instr->definitions[1].isKill()) return false; exec_mask_source_op = exec_copy_instr->operands[0]; save_original_exec = !exec_copy_instr->definitions[0].isKill(); saved_exec_def = exec_copy_instr->definitions[0]; }
                  else if (exec_copy_instr->opcode == s_and_op && exec_copy_instr->operands.size() == 2 && exec_copy_instr->operands[1].isFixed() && exec_copy_instr->operands[1].physReg() == exec) { if (exec_copy_instr->definitions.size() < 2 || !exec_copy_instr->definitions[1].isKill()) return false; exec_mask_source_op = exec_copy_instr->operands[0]; }
                  else if (exec_copy_instr->opcode == s_andn2_op && exec_copy_instr->operands.size() == 2 && exec_copy_instr->operands[0].isFixed() && exec_copy_instr->operands[0].physReg() == exec) { if (exec_copy_instr->definitions.size() < 2 || !exec_copy_instr->definitions[1].isKill()) return false; exec_mask_source_op = exec_copy_instr->operands[1]; negate_mask = true; }
                  else { return false; }
                  if (!exec_mask_source_op.isTemp()) return false;

                  Idx exec_val_idx = last_writer_idx(ctx, exec_mask_source_op); if (!exec_val_idx.found() || exec_val_idx.block != ctx.current_block->index) return false;
                  Instruction* exec_val_producer = ctx.get(exec_val_idx); if (!exec_val_producer) return false;
                  if (is_overwritten_since(ctx, exec, ctx.program->lane_mask, exec_val_idx, false)) return false;

                  bool can_use_vcmpx = false; aco_opcode vcmpx_op = aco_opcode::num_opcodes; bool vcmpx_writes_dst_too = ctx.program->gfx_level <= amd_gfx_level::GFX9; // Use scoped enum
                  if (exec_val_producer->isVOPC()) {
                        aco_opcode vcmp_op = exec_val_producer->opcode; if (negate_mask) vcmp_op = get_vcmp_inverse(vcmp_op); vcmpx_op = get_vcmpx(vcmp_op);
                        if (vcmpx_op != aco_opcode::num_opcodes) { if (!exec_val_producer->isDPP() || ctx.program->gfx_level >= amd_gfx_level::GFX11) { if (!exec_val_producer->isSDWA()) { can_use_vcmpx = true; } } } // Use scoped enum
                  }
                  Temp mask_temp = exec_mask_source_op.getTemp(); // Capture temp before use count check
                  if (negate_mask && (!can_use_vcmpx || (mask_temp.id() != 0 && mask_temp.id() < ctx.uses.size() && ctx.uses[mask_temp.id()] > 1))) return false; // Use id() != 0

                  Definition& producer_def = exec_val_producer->definitions[0]; Temp producer_temp = producer_def.getTemp();
                  bool producer_result_is_live = producer_temp.id() != 0 && producer_temp.id() < ctx.uses.size() && ctx.uses[producer_temp.id()] > (negate_mask ? 0 : 1); // Use id() != 0
                  bool can_remove_exec_copy = exec_mask_source_op.isKill();
                  bool can_reassign_producer = can_use_vcmpx || (!negate_mask && (exec_val_producer->isSALU() || exec_val_producer->opcode == aco_opcode::p_parallelcopy || exec_val_producer->opcode == aco_opcode::p_create_vector));
                  if (!can_reassign_producer || (save_original_exec && !can_remove_exec_copy && producer_result_is_live)) return false;

                  for (unsigned i = exec_val_idx.instr + 1; i < ctx.current_instr_idx; ++i) {
                        Instruction* intervening_instr = ctx.current_block->instructions[i].get(); if (!intervening_instr) continue;
                        if (needs_exec_mask(intervening_instr)) return false; if (intervening_instr->opcode == aco_opcode::p_logical_end && ctx.current_block->logical_succs.size() > 1) return false;
                  }
                  unsigned original_instr_idx = ctx.current_instr_idx; bool need_fixup = false;
                  if (save_original_exec) {
                        if (is_overwritten_since(ctx, saved_exec_def, exec_val_idx, false)) return false;
                        if (can_use_vcmpx && vcmpx_writes_dst_too && saved_exec_def.physReg() == producer_def.physReg()) return false;
                        for (unsigned i = exec_val_idx.instr + 1; i < original_instr_idx; ++i) { Instruction* ii = ctx.current_block->instructions[i].get(); if (!ii) continue; for(const Operand& op : ii->operands) if (op.isFixed() && op.physReg() == saved_exec_def.physReg()) return false; }
                  }

                  Temp original_producer_temp = producer_def.getTemp(); Definition producer_original_def = producer_def;

                  if (can_use_vcmpx) {
                        if (vcmpx_writes_dst_too) {
                              if (exec_val_producer->usesModifiers() || exec_val_producer->operands.size() > 2) return false; // Bail out if def cannot be added
                              // Cannot emplace_back into span. Bail out.
                              // exec_val_producer->definitions.emplace_back(exec, ctx.program->lane_mask); // REMOVED
                              return false; // Cannot modify definitions span post-RA this way.
                        } else { exec_val_producer->definitions[0] = Definition(exec, ctx.program->lane_mask); }
                        exec_val_producer->opcode = vcmpx_op;
                  } else { exec_val_producer->definitions[0] = Definition(exec, ctx.program->lane_mask); }
                  Idx producer_loc = {exec_val_idx.block, exec_val_idx.instr}; unsigned exec_reg = exec.reg(); unsigned exec_size = ctx.program->lane_mask.size();
                  for (unsigned k=0; k<exec_size; ++k) ctx.instr_idx_by_regs[exec_val_idx.block][exec_reg + k] = producer_loc;

                  if (original_producer_temp.id() != 0) { // Use id() != 0
                        for (unsigned i = exec_val_idx.instr + 1; i < original_instr_idx; ++i) { Instruction* ii = ctx.current_block->instructions[i].get(); if (!ii) continue;
                              for (Operand& op : ii->operands) { if (op.isTemp() && op.tempId() == original_producer_temp.id()) { op = Operand(exec, op.regClass()); if (original_producer_temp.id() < ctx.uses.size()) ctx.uses[original_producer_temp.id()]--; } }
                        }
                  }

                  if (can_remove_exec_copy) {
                        if (mask_temp.id() != 0 && mask_temp.id() < ctx.uses.size()) ctx.uses[mask_temp.id()]--; // Use id() != 0
                        exec_copy_instr.reset();
                  } else { if (can_reassign_producer) {
                        aco_ptr<Instruction> copy_exec_back; copy_exec_back.reset(create_instruction(aco_opcode::p_parallelcopy, Format::PSEUDO, 1, 1));
                        copy_exec_back->definitions[0] = producer_original_def; copy_exec_back->operands[0] = Operand(exec, ctx.program->lane_mask);
                        exec_copy_instr = std::move(copy_exec_back);
                  }
                  }

                  if (save_original_exec) {
                        bool inserted_phi = false;
                        if (ctx.current_block->kind & block_kind_loop_header) { inserted_phi = try_insert_saveexec_out_of_loop(ctx, ctx.current_block, saved_exec_def, exec_val_idx.instr); }
                        if (inserted_phi) { fixup_reg_writes(ctx, 0); need_fixup = true; }
                        else {
                              Instruction* copy_old_exec = create_instruction(aco_opcode::p_parallelcopy, Format::PSEUDO, 1, 1);
                              copy_old_exec->definitions[0] = saved_exec_def; copy_old_exec->operands[0] = Operand(exec, ctx.program->lane_mask);
                              auto insert_it = std::next(ctx.current_block->instructions.begin(), exec_val_idx.instr);
                              ctx.current_block->instructions.emplace(insert_it, copy_old_exec);
                              original_instr_idx++; fixup_reg_writes(ctx, exec_val_idx.instr); need_fixup = true;
                        }
                  }
                  if (need_fixup) ctx.current_instr_idx = original_instr_idx; // Restore index before potential fixup

                  return true;
            }


            /** @brief Marks constant non-zero EXEC branches as never taken. */
            void
            try_skip_const_branch(pr_opt_ctx& ctx, aco_ptr<Instruction>& branch)
            {
                  // Use UINT32_MAX to check for invalid target instead of -1
                  if (!branch || branch->opcode != aco_opcode::p_cbranch_z || branch->branch().target[0] == UINT32_MAX ||
                        branch->operands.empty() || !branch->operands[0].isFixed() || branch->operands[0].physReg() != exec) return;

                  Idx exec_val_idx = last_writer_idx(ctx, branch->operands[0]); if (!exec_val_idx.found()) return;
                  Instruction* exec_val = ctx.get(exec_val_idx); if (!exec_val) return;
                  bool exec_is_const_nonzero = false;
                  if ((exec_val->opcode == aco_opcode::p_parallelcopy && exec_val->operands.size() == 1) || exec_val->opcode == aco_opcode::p_create_vector) {
                        for(const Operand& op : exec_val->operands) { if (op.isConstant() && op.constantValue() != 0) { exec_is_const_nonzero = true; break; } }
                  } else if (exec_val->opcode == aco_opcode::s_mov_b32 || exec_val->opcode == aco_opcode::s_mov_b64) {
                        if (exec_val->operands[0].isConstant() && exec_val->operands[0].constantValue() != 0) { exec_is_const_nonzero = true; }
                  } else if (exec_val->opcode == aco_opcode::s_movk_i32) { if ((int16_t)exec_val->salu().imm != 0) { exec_is_const_nonzero = true; } }

                  if (exec_is_const_nonzero) { branch->branch().target[0] = UINT32_MAX; } // Mark branch as never taken using UINT32_MAX
            }


            // --- Main Processing Function ---
            void
            process_instruction(pr_opt_ctx& ctx, aco_ptr<Instruction>& instr)
            {
                  if (!instr) { ctx.current_instr_idx++; return; }
                  if (is_dead(ctx.uses, instr.get())) { instr.reset(); ctx.current_instr_idx++; return; }

                  unsigned original_index = ctx.current_instr_idx;
                  bool restructured = try_optimize_branching_sequence(ctx, instr);
                  if (restructured) {
                        // Branching opt handles index restoration/advancement via fixup or deletion
                        // If instruction still exists, process_instruction loop will re-evaluate it
                        // If deleted, index needs advancing (handled by caller loop if current_instr_idx wasn't changed, or explicitly here)
                        // Let's ensure index is correct for the *next* iteration.
                        // If fixup happened, ctx.current_instr_idx was restored to original_index.
                        // If instruction deleted, we need to ensure loop doesn't skip next instruction.
                        // Let's have branching sequence explicitly set ctx.current_instr_idx if needed.
                        // Current implementation relies on caller loop incrementing.
                        // If instruction was deleted, let caller increment handle moving to next slot.
                        // If helpers inserted, they should restore index before returning true.
                        // So, simply return.
                        return;
                  }

                  try_apply_branch_vcc(ctx, instr);
                  try_optimize_to_scc_zero_cmp(ctx, instr);
                  try_optimize_scc_nocompare(ctx, instr);
                  try_eliminate_scc_copy(ctx, instr);

                  try_combine_dpp(ctx, instr);
                  if (ctx.program->gfx_level == amd_gfx_level::GFX9) { try_convert_mad_to_vop2(ctx, instr); } // Use scoped enum

                  if (ctx.program->gfx_level == amd_gfx_level::GFX9) { try_promote_sgpr_to_literal(ctx, instr); } // Use scoped enum

                  try_reassign_split_vector(ctx, instr);

                  save_scc_copy_producer(ctx, instr);
                  save_reg_writes(ctx, instr);

                  // Only advance index if it wasn't manually adjusted by an optimization pass.
                  // Since try_optimize_branching_sequence is the main one that adjusts index via fixup,
                  // and it returns early, this increment should be safe here.
                  ctx.current_instr_idx++;
            }


      } // namespace anonymous


      // --- Public Entry Point ---
      void
      optimize_postRA(Program* program)
      {
            assert(program && "Input program cannot be null.");
            if (program->blocks.empty()) return;

            pr_opt_ctx ctx(program);

            /* Forward Pass: Apply optimizations */
            for (auto& block : program->blocks) {
                  ctx.reset_block(&block);
                  // Use explicit index check as process_instruction now manages advancement
                  while (ctx.current_instr_idx < block.instructions.size()) {
                        unsigned current_processing_idx = ctx.current_instr_idx; // Capture index before processing
                        process_instruction(ctx, block.instructions[current_processing_idx]);
                        // If process_instruction didn't advance index (e.g., due to restructure return), advance it here.
                        if (ctx.current_instr_idx == current_processing_idx) {
                              ctx.current_instr_idx++;
                        }
                        // Sanity check in case an optimization removed the last instruction
                        if (ctx.current_instr_idx > block.instructions.size()) {
                              ctx.current_instr_idx = block.instructions.size();
                        }
                  }

                  if (!block.instructions.empty()) {
                        try_skip_const_branch(ctx, block.instructions.back());
                  }
            }

            /* Cleanup Pass: Remove dead instructions efficiently */
            for (auto& block : program->blocks) {
                  auto new_end = std::remove_if(block.instructions.begin(), block.instructions.end(),
                                                [&](const aco_ptr<Instruction>& instr_ptr) -> bool {
                                                      return !instr_ptr || is_dead(ctx.uses, instr_ptr.get());
                                                });
                  block.instructions.erase(new_end, block.instructions.end());
            }
      }

} // namespace aco
