/*
 * Copyright © 2018 Valve Corporation
 * Copyright © 2018 Google
 *
 * SPDX-License-Identifier: MIT
 */
#include "aco_ir.h"
#include <immintrin.h>
#include <algorithm>
#include <vector>
#include <cstdio>

#define LIKELY(x)   __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)

#ifndef ALWAYS_INLINE
#  if defined(__GNUC__) || defined(__clang__)
#    define ALWAYS_INLINE inline __attribute__((always_inline))
#  elif defined(_MSC_VER)
#    define ALWAYS_INLINE __forceinline
#  else
#    define ALWAYS_INLINE inline
#  endif
#endif

#ifndef ALIGN_NPOT
#define ALIGN_NPOT(x, align_val) (((x) + (align_val) - 1) / (align_val) * (align_val))
#endif
#ifndef DIV_ROUND_UP
#define DIV_ROUND_UP(n, d) (((n) + (d)-1) / (d))
#endif

namespace aco {

      [[nodiscard]] RegisterDemand
      get_live_changes(Instruction* instr)
      {
            RegisterDemand changes;
            const size_t num_defs = instr->definitions.size();
            const size_t num_ops = instr->operands.size();

            if (num_defs == 1) {
                  const Definition& def = instr->definitions[0];
                  if (LIKELY(def.isTemp() && !def.isKill()))
                        changes += def.getTemp();
            } else {
                  for (const Definition& def : instr->definitions) {
                        if (UNLIKELY(!def.isTemp()) || def.isKill())
                              continue;
                        changes += def.getTemp();
                  }
            }

            if (num_ops <= 2) {
                  if (num_ops >= 1 && instr->operands[0].isTemp() && instr->operands[0].isFirstKill())
                        changes -= instr->operands[0].getTemp();
                  if (num_ops == 2 && instr->operands[1].isTemp() && instr->operands[1].isFirstKill())
                        changes -= instr->operands[1].getTemp();
            } else {
                  for (const Operand& op : instr->operands) {
                        if (UNLIKELY(!op.isTemp()) || !op.isFirstKill())
                              continue;
                        changes -= op.getTemp();
                  }
            }
            return changes;
      }

      [[nodiscard]] RegisterDemand
      get_temp_registers(Instruction* instr)
      {
            RegisterDemand demand_before;
            RegisterDemand demand_after;
            bool has_kill_defs = false;
            for (const Definition& def : instr->definitions) {
                  if (UNLIKELY(def.isKill())) {
                        has_kill_defs = true;
                        break;
                  }
            }

            if (LIKELY(!has_kill_defs)) {
                  for (const Definition& def : instr->definitions) {
                        if (LIKELY(def.isTemp()))
                              demand_before -= def.getTemp();
                  }
            } else {
                  for (const Definition& def : instr->definitions) {
                        if (UNLIKELY(def.isKill()))
                              demand_after += def.getTemp();
                        else if (LIKELY(def.isTemp()))
                              demand_before -= def.getTemp();
                  }
            }

            for (const Operand& op : instr->operands) {
                  if (op.isFirstKill() || op.isCopyKill()) {
                        demand_before += op.getTemp();
                        if (UNLIKELY(op.isLateKill()))
                              demand_after += op.getTemp();
                  } else if (UNLIKELY(op.isClobbered() && !op.isKill())) {
                        demand_before += op.getTemp();
                  }
            }
            demand_after.update(demand_before);
            return demand_after;
      }

      [[nodiscard]] RegisterDemand
      get_temp_reg_changes(Instruction* instr)
      {
            RegisterDemand available_def_space;
            for (const Definition& def : instr->definitions) {
                  if (LIKELY(def.isTemp()))
                        available_def_space += def.getTemp();
            }
            for (const Operand& op : instr->operands) {
                  if (op.isFirstKillBeforeDef() || op.isCopyKill())
                        available_def_space -= op.getTemp();
                  else if (UNLIKELY(op.isClobbered() && !op.isKill()))
                        available_def_space -= op.getTemp();
            }
            return available_def_space;
      }

      namespace {

            static constexpr size_t VEGA_CACHE_LINE = 64;
            static constexpr size_t PREFETCH_DISTANCE = 4;
            static constexpr size_t VEGA_WAVE_SIZE = 64; // Added missing constant

            struct alignas(8) InstrCache {
                  uint16_t opcode;
                  uint8_t format_bits;
                  uint8_t vcc_flags;
                  uint8_t num_temp_defs;
                  uint8_t num_temp_ops;
                  uint8_t special_flags;
                  uint8_t padding;
            };

            enum FormatBits : uint8_t {
                  FMT_VOPC = 1 << 0,
                  FMT_VOP2 = 1 << 1,
                  FMT_VOP3 = 1 << 2,
                  FMT_VALU = 1 << 3,
            };

            enum VccFlags : uint8_t {
                  VCC_NEEDS_BASE  = 1 << 0,
                  VCC_HAS_3_OPS   = 1 << 1,
                  VCC_HAS_2_DEFS  = 1 << 2,
                  VCC_OP2_IS_SGPR = 1 << 3,
            };

            enum SpecialFlags : uint8_t {
                  SPECIAL_NONE         = 0,
                  SPECIAL_CARRY_IN     = 1 << 0,
                  SPECIAL_BPERMUTE     = 1 << 1,
                  SPECIAL_WMMA         = 1 << 2,
                  SPECIAL_INTERP_GFX11 = 1 << 3,
                  SPECIAL_INTERP_F32   = 1 << 4,
                  SPECIAL_INIT_SCRATCH = 1 << 5,
            };

            struct live_ctx {
                  monotonic_buffer_resource m;
                  Program* program;
                  int32_t worklist;
                  uint32_t handled_once;
                  InstrCache* instr_cache = nullptr;
                  size_t cache_capacity = 0;
                  uint8_t* temp_type_cache = nullptr;
                  uint8_t* temp_size_cache = nullptr;
                  size_t temp_cache_capacity = 0;
            };

            [[nodiscard]] bool
            instr_needs_vcc(Instruction* instr)
            {
                  if (UNLIKELY(instr->isVOPC()))
                        return true;
                  if (instr->isVOP2() && !instr->isVOP3()) {
                        if (UNLIKELY(instr->operands.size() == 3 && instr->operands[2].isTemp() &&
                              instr->operands[2].regClass().type() == RegType::sgpr))
                              return true;
                        if (UNLIKELY(instr->definitions.size() == 2))
                              return true;
                  }
                  return false;
            }

            [[nodiscard]] ALWAYS_INLINE bool
            instr_needs_vcc_cached(const InstrCache& cache) noexcept
            {
                  if (cache.vcc_flags & VCC_NEEDS_BASE)
                        return true;
                  if ((cache.format_bits & (FMT_VOP2 | FMT_VOP3)) == FMT_VOP2) {
                        if ((cache.vcc_flags & (VCC_HAS_3_OPS | VCC_OP2_IS_SGPR)) ==
                              (VCC_HAS_3_OPS | VCC_OP2_IS_SGPR))
                              return true;
                        if (cache.vcc_flags & VCC_HAS_2_DEFS)
                              return true;
                  }
                  return false;
            }

            ALWAYS_INLINE void
            cache_instruction(InstrCache& cache, const Instruction* insn) noexcept
            {
                  static_assert(static_cast<int>(aco_opcode::num_opcodes) < 65536, "Opcode doesn't fit in uint16_t");
                  cache.opcode = static_cast<uint16_t>(insn->opcode);
                  cache.format_bits = 0;
                  if (insn->isVOPC()) cache.format_bits |= FMT_VOPC;
                  if (insn->isVOP2()) cache.format_bits |= FMT_VOP2;
                  if (insn->isVOP3()) cache.format_bits |= FMT_VOP3;
                  if (insn->isVALU()) cache.format_bits |= FMT_VALU;
                  cache.vcc_flags = 0;
                  if (cache.format_bits & FMT_VOPC)
                        cache.vcc_flags |= VCC_NEEDS_BASE;
                  const size_t num_ops = insn->operands.size();
                  const size_t num_defs = insn->definitions.size();
                  if (num_ops == 3) cache.vcc_flags |= VCC_HAS_3_OPS;
                  if (num_defs == 2) cache.vcc_flags |= VCC_HAS_2_DEFS;
                  if (num_ops == 3 && insn->operands[2].isTemp() &&
                        insn->operands[2].regClass().type() == RegType::sgpr) {
                        cache.vcc_flags |= VCC_OP2_IS_SGPR;
                        }
                        cache.num_temp_defs = 0;
                  cache.num_temp_ops = 0;
                  for (const Definition& def : insn->definitions) {
                        if (def.isTemp()) cache.num_temp_defs++;
                  }
                  for (const Operand& op : insn->operands) {
                        if (op.isTemp()) cache.num_temp_ops++;
                  }
                  cache.special_flags = SPECIAL_NONE;
                  switch (insn->opcode) {
                        case aco_opcode::v_addc_co_u32:
                        case aco_opcode::v_subb_co_u32:
                        case aco_opcode::v_subbrev_co_u32:
                              cache.special_flags |= SPECIAL_CARRY_IN;
                              break;
                        case aco_opcode::p_bpermute_readlane:
                        case aco_opcode::p_bpermute_permlane:
                        case aco_opcode::p_bpermute_shared_vgpr:
                        case aco_opcode::p_dual_src_export_gfx11:
                        case aco_opcode::v_mqsad_u32_u8:
                              cache.special_flags |= SPECIAL_BPERMUTE;
                              break;
                        case aco_opcode::p_interp_gfx11:
                              cache.special_flags |= SPECIAL_INTERP_GFX11;
                              break;
                        case aco_opcode::v_interp_p1_f32:
                              cache.special_flags |= SPECIAL_INTERP_F32;
                              break;
                        case aco_opcode::p_init_scratch:
                              cache.special_flags |= SPECIAL_INIT_SCRATCH;
                              break;
                        default:
                              if (instr_info.classes[static_cast<int>(insn->opcode)] == instr_class::wmma)
                                    cache.special_flags |= SPECIAL_WMMA;
                        break;
                  }
            }

            inline bool
            should_use_temp_cache(const Program* program) noexcept
            {
                  return program->temp_rc.size() > 2000;
            }

            inline void
            init_temp_cache(live_ctx& ctx) noexcept
            {
                  const size_t num_temps = ctx.program->temp_rc.size();
                  if (num_temps > ctx.temp_cache_capacity) {
                        ctx.temp_cache_capacity = (num_temps + (VEGA_CACHE_LINE - 1)) & ~(VEGA_CACHE_LINE - 1);
                        ctx.temp_type_cache = (uint8_t*)ctx.m.allocate(ctx.temp_cache_capacity, VEGA_CACHE_LINE);
                        ctx.temp_size_cache = (uint8_t*)ctx.m.allocate(ctx.temp_cache_capacity, VEGA_CACHE_LINE);

                        if (!ctx.temp_type_cache || !ctx.temp_size_cache) {
                              ctx.temp_type_cache = nullptr;
                              ctx.temp_size_cache = nullptr;
                              ctx.temp_cache_capacity = 0;
                              return;
                        }

                        for (size_t i = 0; i < num_temps; ++i) {
                              ctx.temp_type_cache[i] = static_cast<uint8_t>(ctx.program->temp_rc[i].type());
                              ctx.temp_size_cache[i] = ctx.program->temp_rc[i].size();
                        }
                  }
            }

            [[nodiscard]] IDSet
            compute_live_out(live_ctx& ctx, Block* block)
            {
                  IDSet live(ctx.m);
                  if (LIKELY(block->logical_succs.empty())) {
                        for (unsigned succ_idx : block->linear_succs) {
                              Block& succ_block = ctx.program->blocks[succ_idx];
                              if (LIKELY(succ_block.logical_preds.empty())) {
                                    live.insert(ctx.program->live.live_in[succ_idx]);
                              } else {
                                    for (unsigned t : ctx.program->live.live_in[succ_idx]) {
                                          if (UNLIKELY(ctx.program->temp_rc[t].is_linear()))
                                                live.insert(t);
                                    }
                              }
                        }
                  } else {
                        live = IDSet(ctx.program->live.live_in[block->linear_succs[0]], ctx.m);
                        if (UNLIKELY(block->linear_succs.size() == 2))
                              live.insert(ctx.program->live.live_in[block->linear_succs[1]]);
                        if (UNLIKELY(block->logical_succs.back() != block->linear_succs.back())) {
                              for (unsigned t : ctx.program->live.live_in[block->logical_succs.back()]) {
                                    if (LIKELY(!ctx.program->temp_rc[t].is_linear()))
                                          live.insert(t);
                              }
                        } else {
                              assert(block->logical_succs[0] == block->linear_succs[0]);
                        }
                  }
                  if (block->linear_succs.size() == 1 && block->linear_succs[0] >= ctx.handled_once) {
                        Block& succ_block = ctx.program->blocks[block->linear_succs[0]];
                        auto it = std::find(succ_block.linear_preds.begin(), succ_block.linear_preds.end(), block->index);
                        unsigned op_idx = std::distance(succ_block.linear_preds.begin(), it);
                        for (aco_ptr<Instruction>& phi : succ_block.instructions) {
                              if (UNLIKELY(!is_phi(phi)))
                                    break;
                              if (UNLIKELY(phi->opcode == aco_opcode::p_phi || phi->definitions[0].isKill()))
                                    continue;
                              if (op_idx < phi->operands.size() && LIKELY(phi->operands[op_idx].isTemp()))
                                    live.insert(phi->operands[op_idx].tempId());
                        }
                  }
                  if (block->logical_succs.size() == 1 && block->logical_succs[0] >= ctx.handled_once) {
                        Block& succ_block = ctx.program->blocks[block->logical_succs[0]];
                        auto it = std::find(succ_block.logical_preds.begin(), succ_block.logical_preds.end(), block->index);
                        unsigned op_idx = std::distance(succ_block.logical_preds.begin(), it);
                        for (aco_ptr<Instruction>& phi : succ_block.instructions) {
                              if (UNLIKELY(!is_phi(phi)))
                                    break;
                              if (UNLIKELY(phi->opcode == aco_opcode::p_linear_phi || phi->definitions[0].isKill()))
                                    continue;
                              if (op_idx < phi->operands.size() && LIKELY(phi->operands[op_idx].isTemp()))
                                    live.insert(phi->operands[op_idx].tempId());
                        }
                  }
                  return live;
            }

            void
            process_live_temps_per_block(live_ctx& ctx, Block* block)
            {
                  RegisterDemand new_demand;
                  block->register_demand = RegisterDemand();
                  IDSet live = compute_live_out(ctx, block);

                  const bool use_temp_cache = should_use_temp_cache(ctx.program);
                  if (use_temp_cache) {
                        init_temp_cache(ctx);
                  }

                  if (use_temp_cache && ctx.temp_type_cache && ctx.temp_size_cache) {
                        for (unsigned t : live) {
                              if (t < ctx.temp_cache_capacity) {
                                    if (((t * sizeof(uint8_t)) & (VEGA_CACHE_LINE - 1)) == 0 && (t + VEGA_CACHE_LINE) <= ctx.temp_cache_capacity) {
                                          __builtin_prefetch(&ctx.temp_type_cache[t + VEGA_CACHE_LINE], 0, 1);
                                          __builtin_prefetch(&ctx.temp_size_cache[t + VEGA_CACHE_LINE], 0, 1);
                                    }
                                    const uint8_t type = ctx.temp_type_cache[t];
                                    const uint8_t size_val = ctx.temp_size_cache[t];
                                    new_demand.vgpr += size_val * (type == static_cast<uint8_t>(RegType::vgpr));
                                    new_demand.sgpr += size_val * (type == static_cast<uint8_t>(RegType::sgpr));
                              } else {
                                    new_demand += Temp(t, ctx.program->temp_rc[t]);
                              }
                        }
                  } else {
                        for (unsigned t : live) {
                              new_demand += Temp(t, ctx.program->temp_rc[t]);
                        }
                  }

                  const size_t num_instrs = block->instructions.size();
                  const bool use_instr_cache = num_instrs <= 256;

                  if (use_instr_cache && num_instrs > ctx.cache_capacity) {
                        ctx.cache_capacity = (num_instrs + 15) & ~15;
                        ctx.instr_cache = (InstrCache*)ctx.m.allocate(
                              ctx.cache_capacity * sizeof(InstrCache), alignof(InstrCache));
                  }

                  if (use_instr_cache && ctx.instr_cache) {
                        for (size_t i = 0; i < num_instrs; ++i) {
                              if (i + PREFETCH_DISTANCE < num_instrs) {
                                    __builtin_prefetch(block->instructions[i + PREFETCH_DISTANCE].get(), 0, 1);
                              }
                              cache_instruction(ctx.instr_cache[i], block->instructions[i].get());
                        }
                  }

                  int idx;

                  for (idx = block->instructions.size() - 1; idx >= 0; idx--) {
                        Instruction* insn = block->instructions[idx].get();
                        if (UNLIKELY(is_phi(insn))) {
                              break;
                        }

                        if (idx >= PREFETCH_DISTANCE) {
                              __builtin_prefetch(block->instructions[idx - PREFETCH_DISTANCE].get(), 0, 1);
                        }

                        if (use_instr_cache && ctx.instr_cache) {
                              ctx.program->needs_vcc |= instr_needs_vcc_cached(ctx.instr_cache[idx]);
                        } else {
                              ctx.program->needs_vcc |= instr_needs_vcc(insn);
                        }

                        insn->register_demand = RegisterDemand(new_demand.vgpr, new_demand.sgpr);

                        if (use_instr_cache && ctx.instr_cache &&
                              ctx.instr_cache[idx].num_temp_defs == 0 &&
                              ctx.instr_cache[idx].num_temp_ops == 0) {
                              block->register_demand.update(insn->register_demand);
                        continue;
                              }

                              bool has_vgpr_def = false;

                              for (Definition& definition : insn->definitions) {
                                    has_vgpr_def |= definition.regClass().type() == RegType::vgpr &&
                                    !definition.regClass().is_linear_vgpr();

                                    if (UNLIKELY(!definition.isTemp()))
                                          continue;

                                    if (UNLIKELY(definition.isFixed() && definition.physReg() == vcc))
                                          ctx.program->needs_vcc = true;

                                    const Temp temp = definition.getTemp();
                                    const size_t n = live.erase(temp.id());

                                    if (LIKELY(n)) {
                                          if (use_temp_cache && ctx.temp_type_cache && ctx.temp_size_cache) {
                                                const uint32_t id_val = temp.id();
                                                if (id_val < ctx.temp_cache_capacity) {
                                                      const uint8_t type = ctx.temp_type_cache[id_val];
                                                      const uint8_t size_val = ctx.temp_size_cache[id_val];
                                                      new_demand.vgpr -= size_val * (type == static_cast<uint8_t>(RegType::vgpr));
                                                      new_demand.sgpr -= size_val * (type == static_cast<uint8_t>(RegType::sgpr));
                                                } else { new_demand -= temp;}
                                          } else {
                                                new_demand -= temp;
                                          }
                                          definition.setKill(false);
                                    } else {
                                          insn->register_demand += temp;
                                          definition.setKill(true);
                                    }
                              }

                              bool is_vector_op = false;
                              for (Operand& op : insn->operands) {
                                    op.setKill(false);
                                    bool lateKill = (UNLIKELY(op.hasRegClass() && op.regClass().is_linear_vgpr() &&
                                    !op.isUndefined() && has_vgpr_def));
                                    lateKill |= is_vector_op || op.isVectorAligned();
                                    op.setLateKill(lateKill);
                                    is_vector_op = op.isVectorAligned();
                              }

                              const InstrCache* pCache = (use_instr_cache && ctx.instr_cache) ? &ctx.instr_cache[idx] : nullptr;
                              if (pCache && pCache->special_flags != SPECIAL_NONE) {
                                    if (UNLIKELY(ctx.program->gfx_level >= GFX10 && (pCache->format_bits & FMT_VALU) &&
                                          !insn->definitions.empty() &&
                                          insn->definitions.back().regClass() == s2)) {
                                          bool carry_in = pCache->special_flags & SPECIAL_CARRY_IN;
                                    unsigned limit = carry_in ? std::min(2u, (unsigned)insn->operands.size()) : (unsigned)insn->operands.size();
                                    for (unsigned op_idx_val = 0; op_idx_val < limit; op_idx_val++) {
                                          if (insn->operands[op_idx_val].isOfType(RegType::sgpr))
                                                insn->operands[op_idx_val].setLateKill(true);
                                    }
                                          } else if (pCache->special_flags & SPECIAL_BPERMUTE) {
                                                for (Operand& op : insn->operands)
                                                      op.setLateKill(true);
                                          } else if ((pCache->special_flags & SPECIAL_INTERP_GFX11) && insn->operands.size() == 7) {
                                                insn->operands[5].setLateKill(true);
                                          } else if ((pCache->special_flags & SPECIAL_INTERP_F32) && ctx.program->dev.has_16bank_lds) {
                                                if (!insn->operands.empty()) insn->operands[0].setLateKill(true);
                                          } else if (pCache->special_flags & SPECIAL_INIT_SCRATCH) {
                                                if (!insn->operands.empty()) insn->operands.back().setLateKill(true);
                                          } else if (pCache->special_flags & SPECIAL_WMMA) {
                                                if (insn->operands.size() > 0) insn->operands[0].setLateKill(true);
                                                if (insn->operands.size() > 1) insn->operands[1].setLateKill(true);
                                          }
                              } else {
                                    if (UNLIKELY(ctx.program->gfx_level >= GFX10 && insn->isVALU() &&
                                          !insn->definitions.empty() &&
                                          insn->definitions.back().regClass() == s2)) {
                                          bool carry_in = insn->opcode == aco_opcode::v_addc_co_u32 ||
                                          insn->opcode == aco_opcode::v_subb_co_u32 ||
                                          insn->opcode == aco_opcode::v_subbrev_co_u32;
                                    unsigned limit = carry_in ? std::min(2u, (unsigned)insn->operands.size()) : (unsigned)insn->operands.size();
                                    for (unsigned op_idx_val = 0; op_idx_val < limit; op_idx_val++) {
                                          if (insn->operands[op_idx_val].isOfType(RegType::sgpr))
                                                insn->operands[op_idx_val].setLateKill(true);
                                    }
                                          } else if (UNLIKELY(insn->opcode == aco_opcode::p_bpermute_readlane ||
                                                insn->opcode == aco_opcode::p_bpermute_permlane ||
                                                insn->opcode == aco_opcode::p_bpermute_shared_vgpr ||
                                                insn->opcode == aco_opcode::p_dual_src_export_gfx11 ||
                                                insn->opcode == aco_opcode::v_mqsad_u32_u8)) {
                                                for (Operand& op : insn->operands)
                                                      op.setLateKill(true);
                                                } else if (UNLIKELY(insn->opcode == aco_opcode::p_interp_gfx11 && insn->operands.size() == 7)) {
                                                      insn->operands[5].setLateKill(true);
                                                } else if (UNLIKELY(insn->opcode == aco_opcode::v_interp_p1_f32 && ctx.program->dev.has_16bank_lds)) {
                                                      if (!insn->operands.empty()) insn->operands[0].setLateKill(true);
                                                } else if (UNLIKELY(insn->opcode == aco_opcode::p_init_scratch)) {
                                                      if (!insn->operands.empty()) insn->operands.back().setLateKill(true);
                                                } else if (UNLIKELY(instr_info.classes[static_cast<int>(insn->opcode)] == instr_class::wmma)) {
                                                      if (insn->operands.size() > 0) insn->operands[0].setLateKill(true);
                                                      if (insn->operands.size() > 1) insn->operands[1].setLateKill(true);
                                                }
                              }

                              RegisterDemand operand_demand;
                              auto tied_defs = get_tied_defs(insn);

                              for (auto op_idx_val : tied_defs) {
                                    Temp tmp = insn->operands[op_idx_val].getTemp();
                                    if (UNLIKELY(std::any_of(tied_defs.begin(), tied_defs.end(), [&](uint32_t k) {
                                          return k < op_idx_val && insn->operands[k].getTemp() == tmp;
                                    }))) {
                                          operand_demand += tmp;
                                          insn->operands[op_idx_val].setCopyKill(true);
                                    }
                                    insn->operands[op_idx_val].setClobbered(true);
                              }

                              is_vector_op = false;
                              for (unsigned i = 0; i < insn->operands.size(); ++i) {
                                    Operand& operand = insn->operands[i];
                                    if (UNLIKELY(!operand.isTemp()))
                                          continue;

                                    const Temp temp = operand.getTemp();

                                    if (UNLIKELY(operand.isPrecolored())) {
                                          assert(!operand.isLateKill());
                                          ctx.program->needs_vcc |= operand.physReg() == vcc;

                                          if (UNLIKELY(std::any_of(insn->definitions.begin(), insn->definitions.end(),
                                                [=](const Definition& def) {
                                                      return def.isFixed() &&
                                                      def.physReg() + def.size() > operand.physReg() &&
                                                      operand.physReg() + operand.size() > def.physReg();
                                                })))
                                                operand.setClobbered(true);

                                          for (unsigned j = i + 1; !operand.isCopyKill() && j < insn->operands.size(); ++j) {
                                                if (UNLIKELY(insn->operands[j].isPrecolored() &&
                                                      insn->operands[j].getTemp() == temp)) {
                                                      operand_demand += temp;
                                                insn->operands[j].setCopyKill(true);
                                                      }
                                          }
                                    }

                                    if (is_vector_op || operand.isVectorAligned()) {
                                          bool other_is_vector_op = false;
                                          for (unsigned j = 0; j < i; j++) {
                                                if ((other_is_vector_op || insn->operands[j].isVectorAligned()) &&
                                                      insn->operands[j].getTemp() == temp) {
                                                      operand_demand += temp;
                                                insn->register_demand += temp;
                                                operand.setCopyKill(true);
                                                break;
                                                      }
                                                      other_is_vector_op = insn->operands[j].isVectorAligned();
                                          }
                                    }

                                    if (UNLIKELY(operand.isLateKill())) {
                                          for (unsigned k_op = 0; k_op < insn->operands.size(); ++k_op) {
                                                Operand& other = insn->operands[k_op];
                                                if (other.isTemp() && other.getTemp() == operand.getTemp())
                                                      other.setLateKill(true);
                                          }
                                    }

                                    if (UNLIKELY(operand.isKill()))
                                          continue;

                                    if (LIKELY(live.insert(temp.id()).second)) {
                                          operand.setFirstKill(true);
                                          for (unsigned j = i + 1; j < insn->operands.size(); ++j) {
                                                if (insn->operands[j].isTemp() && insn->operands[j].getTemp() == temp)
                                                      insn->operands[j].setKill(true);
                                          }
                                          if (UNLIKELY(operand.isLateKill()))
                                                insn->register_demand += temp;

                                          if (use_temp_cache && ctx.temp_type_cache && ctx.temp_size_cache) {
                                                const uint32_t id_val = temp.id();
                                                if (id_val < ctx.temp_cache_capacity) {
                                                      const uint8_t type = ctx.temp_type_cache[id_val];
                                                      const uint8_t size_val = ctx.temp_size_cache[id_val];
                                                      new_demand.vgpr += size_val * (type == static_cast<uint8_t>(RegType::vgpr));
                                                      new_demand.sgpr += size_val * (type == static_cast<uint8_t>(RegType::sgpr));
                                                } else { new_demand += temp; }
                                          } else {
                                                new_demand += temp;
                                          }
                                    } else if (UNLIKELY(operand.isClobbered())) {
                                          operand_demand += temp;
                                    }
                                    is_vector_op = operand.isVectorAligned();
                              }

                              operand_demand += new_demand;
                              insn->register_demand.update(operand_demand);
                              block->register_demand.update(insn->register_demand);
                  }

                  for (int phi_idx = 0; phi_idx <= idx; phi_idx++) {
                        Instruction* insn = block->instructions[phi_idx].get();
                        insn->register_demand = new_demand;

                        assert(is_phi(insn) && insn->definitions.size() == 1);
                        if (UNLIKELY(!insn->definitions[0].isTemp())) {
                              assert(insn->definitions[0].isFixed() && insn->definitions[0].physReg() == exec);
                              continue;
                        }

                        Definition& definition = insn->definitions[0];
                        if (UNLIKELY(definition.isFixed() && definition.physReg() == vcc))
                              ctx.program->needs_vcc = true;

                        const size_t n = live.erase(definition.tempId());
                        if (n && (definition.isKill() || ctx.handled_once > block->index)) {
                              Block::edge_vec& preds = insn->opcode == aco_opcode::p_phi ?
                              block->logical_preds :
                              block->linear_preds;
                              for (unsigned k = 0; k < preds.size(); k++) {
                                    if (k < insn->operands.size() && LIKELY(insn->operands[k].isTemp()))
                                          ctx.worklist = std::max<int>(ctx.worklist, preds[k]);
                              }
                        }
                        definition.setKill(!n);
                  }

                  for (int phi_idx = 0; phi_idx <= idx; phi_idx++) {
                        Instruction* insn = block->instructions[phi_idx].get();
                        assert(is_phi(insn));

                        if (UNLIKELY(insn->definitions[0].isKill()))
                              continue;
                        for (Operand& operand : insn->operands) {
                              if (UNLIKELY(!operand.isTemp()))
                                    continue;
                              operand.setKill(!live.count(operand.tempId()));
                        }
                  }

                  if (LIKELY(ctx.program->live.live_in[block->index].insert(live))) {
                        if (LIKELY(block->linear_preds.size())) {
                              assert(block->logical_preds.empty() ||
                              block->logical_preds.back() <= block->linear_preds.back());
                              ctx.worklist = std::max<int>(ctx.worklist, block->linear_preds.back());
                        } else {
                              if (!(aco::debug_flags & DEBUG_NO_VALIDATE)) {
                                    [[maybe_unused]] bool is_valid = validate_ir(ctx.program);
                                    assert(!is_valid && "IR validation failed: block has no linear predecessors but live_in changed");
                              }
                        }
                  }

                  block->live_in_demand = new_demand;
                  block->register_demand.update(block->live_in_demand);
                  ctx.program->max_reg_demand.update(block->register_demand);
                  ctx.handled_once = std::min(ctx.handled_once, block->index);

                  assert(!block->linear_preds.empty() || (new_demand == RegisterDemand() && live.empty()));
            }

            [[nodiscard]] unsigned
            calc_waves_per_workgroup(Program* program)
            {
                  if (!program || program->wave_size == 0)
                        return 1;
                  unsigned workgroup_size = program->workgroup_size == UINT_MAX ? program->wave_size : program->workgroup_size;
                  return align(workgroup_size, program->wave_size) / program->wave_size;
            }

      } // Anonymous namespace ends here

      [[nodiscard]] bool
      uses_scratch(Program* program)
      {
            return UNLIKELY(program->config->scratch_bytes_per_wave || program->stage == raytracing_cs);
      }

      [[nodiscard]] uint16_t
      get_extra_sgprs(Program* program)
      {
            if (UNLIKELY(!program))
                  return 0;
            bool needs_scratch_memory = uses_scratch(program);
            if (UNLIKELY(program->gfx_level < GFX8)) {
                  if (UNLIKELY(needs_scratch_memory)) {
                        return 4;
                  } else if (LIKELY(program->needs_vcc)) {
                        return 2;
                  }
                  return 0;
            } else if (UNLIKELY(program->gfx_level == GFX8)) {
                  if (UNLIKELY(needs_scratch_memory)) {
                        if (UNLIKELY(program->dev.xnack_enabled)) {
                              return 6;
                        } else {
                              return 4;
                        }
                  } else if (UNLIKELY(program->dev.xnack_enabled)) {
                        return 4;
                  } else if (LIKELY(program->needs_vcc)) {
                        return 2;
                  }
                  return 0;
            } else if (UNLIKELY(program->gfx_level == GFX9)) {
                  if (UNLIKELY(needs_scratch_memory)) {
                        return 6;
                  } else if (UNLIKELY(program->dev.xnack_enabled)) {
                        return 4;
                  } else if (LIKELY(program->needs_vcc)) {
                        return 2;
                  }
                  return 0;
            } else {
                  assert(!program->dev.xnack_enabled && "XNACK not supported on GFX10+");
                  if (UNLIKELY(needs_scratch_memory)) {
                        return 2;
                  }
                  return 0;
            }
      }

      [[nodiscard]] uint16_t
      get_sgpr_alloc(Program* program, uint16_t addressable_sgprs)
      {
            uint16_t sgprs = addressable_sgprs + get_extra_sgprs(program);
            uint16_t granule = program->dev.sgpr_alloc_granule;
            return ALIGN_NPOT(std::max(sgprs, granule), granule);
      }

      [[nodiscard]] uint16_t
      get_vgpr_alloc(Program* program, uint16_t addressable_vgprs)
      {
            assert(addressable_vgprs <= program->dev.vgpr_limit);
            uint16_t granule = program->dev.vgpr_alloc_granule;
            return ALIGN_NPOT(std::max(addressable_vgprs, granule), granule);
      }

      [[nodiscard]] unsigned
      round_down(unsigned a, unsigned b)
      {
            return b == 0 ? a : (a - (a % b));
      }

      [[nodiscard]] RegisterDemand
      get_addr_regs_from_waves(Program* program, uint16_t waves)
      {
            if (waves == 0) waves = 1;
            uint16_t sgprs_calc = program->dev.physical_sgprs / waves; // Use intermediate for clarity
            sgprs_calc = std::min<uint16_t>(sgprs_calc, 128);
            sgprs_calc = round_down(sgprs_calc, program->dev.sgpr_alloc_granule) - get_extra_sgprs(program);
            sgprs_calc = std::min(sgprs_calc, program->dev.sgpr_limit);

            uint16_t vgprs_calc = program->dev.physical_vgprs / waves;
            if (program->dev.vgpr_alloc_granule == 0) {
                  vgprs_calc = program->dev.vgpr_limit;
            } else {
                  vgprs_calc = vgprs_calc / program->dev.vgpr_alloc_granule * program->dev.vgpr_alloc_granule;
            }
            vgprs_calc -= program->config->num_shared_vgprs / 2;
            vgprs_calc = std::min(vgprs_calc, program->dev.vgpr_limit);
            return RegisterDemand(vgprs_calc, sgprs_calc);
      }

      void
      calc_min_waves(Program* program)
      {
            unsigned waves_per_workgroup = calc_waves_per_workgroup(program);
            unsigned simd_per_cu_wgp = program->dev.simd_per_cu * (program->wgp_mode ? 2 : 1);
            if (simd_per_cu_wgp == 0) simd_per_cu_wgp = 1;
            program->min_waves = DIV_ROUND_UP(waves_per_workgroup, simd_per_cu_wgp);
            if (program->min_waves == 0) program->min_waves = 1;
      }

      [[nodiscard]] uint16_t
      max_suitable_waves(Program* program, uint16_t waves)
      {
            unsigned num_simd = program->dev.simd_per_cu * (program->wgp_mode ? 2 : 1);
            if (num_simd == 0) num_simd = 1;
            unsigned waves_per_workgroup = calc_waves_per_workgroup(program);
            if (waves_per_workgroup == 0) {
                  waves_per_workgroup = program->wave_size > 0 ? program->wave_size : VEGA_WAVE_SIZE;
            }

            unsigned num_workgroups = waves_per_workgroup ? (waves * num_simd / waves_per_workgroup) : 0;

            unsigned lds_per_workgroup = align(program->config->lds_size * program->dev.lds_encoding_granule, program->dev.lds_alloc_granule);
            if (program->stage == fragment_fs) {
                  unsigned lds_bytes_per_interp = 3 * 16;
                  unsigned lds_param_bytes = lds_bytes_per_interp * program->info.ps.num_inputs;
                  lds_per_workgroup += align(lds_param_bytes, program->dev.lds_alloc_granule);
            }
            unsigned lds_limit = program->wgp_mode ? program->dev.lds_limit * 2 : program->dev.lds_limit;
            if (lds_per_workgroup > 0 && lds_limit > 0)
                  num_workgroups = std::min(num_workgroups, lds_limit / lds_per_workgroup);
            if (waves_per_workgroup > 1)
                  num_workgroups = std::min(num_workgroups, program->wgp_mode ? 32u : 16u);
            unsigned workgroup_waves = num_workgroups * waves_per_workgroup;
            return num_simd ? DIV_ROUND_UP(workgroup_waves, num_simd) : 0;
      }

      void
      update_vgpr_sgpr_demand(Program* program, const RegisterDemand new_demand)
      {
            assert(program->min_waves >= 1);
            RegisterDemand limit = get_addr_regs_from_waves(program, program->min_waves);
            if (UNLIKELY(new_demand.exceeds(limit))) {
                  program->num_waves = 0;
                  program->max_reg_demand = new_demand;
            } else {
                  uint16_t sgpr_alloc_val = get_sgpr_alloc(program, new_demand.sgpr);
                  uint16_t vgpr_demand_val = get_vgpr_alloc(program, new_demand.vgpr) + program->config->num_shared_vgprs / 2;

                  if (sgpr_alloc_val > 0) {
                        program->num_waves = program->dev.physical_sgprs / sgpr_alloc_val;
                  } else {
                        program->num_waves = program->dev.max_waves_per_simd;
                  }

                  if (vgpr_demand_val > 0) {
                        program->num_waves = std::min<uint16_t>(program->num_waves,
                                                                program->dev.physical_vgprs / vgpr_demand_val);
                  }
                  // If vgpr_demand_val is 0, num_waves determined by SGPRs (or max_waves_per_simd) remains.

                  program->num_waves = std::min(program->num_waves, program->dev.max_waves_per_simd);
                  program->num_waves = max_suitable_waves(program, program->num_waves);
                  if (program->num_waves > 0)
                        program->max_reg_demand = get_addr_regs_from_waves(program, program->num_waves);
                  else
                        program->max_reg_demand = new_demand;
            }
      }

      void
      live_var_analysis(Program* program)
      {
            program->live.live_in.clear();
            program->live.memory.release();
            program->live.live_in.resize(program->blocks.size(), IDSet(program->live.memory));
            program->max_reg_demand = RegisterDemand();
            program->needs_vcc = program->gfx_level >= GFX10;
            live_ctx ctx;
            ctx.program = program;
            ctx.worklist = program->blocks.size() - 1;
            ctx.handled_once = program->blocks.size();

            while (LIKELY(ctx.worklist >= 0)) {
                  process_live_temps_per_block(ctx, &program->blocks[ctx.worklist--]);
            }
            if (LIKELY(program->progress < CompilationProgress::after_ra))
                  update_vgpr_sgpr_demand(program, program->max_reg_demand);
      }
} // namespace aco
