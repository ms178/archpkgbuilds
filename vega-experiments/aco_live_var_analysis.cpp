/*
 * Copyright © 2018 Valve Corporation
 * Copyright © 2018 Google
 *
 * SPDX-License-Identifier: MIT
 */
#include "aco_ir.h"
#define LIKELY(x)   __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)

namespace aco {

      [[nodiscard]] RegisterDemand
      get_live_changes(Instruction* instr)
      {
            RegisterDemand changes;
            for (const Definition& def : instr->definitions) {
                  if (UNLIKELY(!def.isTemp()) || def.isKill())
                        continue;
                  changes += def.getTemp();
            }

            for (const Operand& op : instr->operands) {
                  if (UNLIKELY(!op.isTemp()) || !op.isFirstKill())
                        continue;
                  changes -= op.getTemp();
            }

            return changes;
      }

      [[nodiscard]] RegisterDemand
      get_temp_registers(Instruction* instr)
      {
            RegisterDemand demand_before;
            RegisterDemand demand_after;

            for (const Definition& def : instr->definitions) {
                  if (UNLIKELY(def.isKill()))
                        demand_after += def.getTemp();
                  else if (LIKELY(def.isTemp()))
                        demand_before -= def.getTemp();
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

      [[nodiscard]] RegisterDemand get_temp_reg_changes(Instruction* instr)
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

            struct live_ctx {
                  monotonic_buffer_resource m;
                  Program* program;
                  int32_t worklist;
                  uint32_t handled_once;
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

            [[nodiscard]] IDSet
            compute_live_out(live_ctx& ctx, Block* block)
            {
                  IDSet live(ctx.m);

                  if (LIKELY(block->logical_succs.empty())) {
                        for (unsigned succ : block->linear_succs) {
                              if (LIKELY(ctx.program->blocks[succ].logical_preds.empty())) {
                                    live.insert(ctx.program->live.live_in[succ]);
                              } else {
                                    for (unsigned t : ctx.program->live.live_in[succ]) {
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
                        Block& succ = ctx.program->blocks[block->linear_succs[0]];
                        auto it = std::find(succ.linear_preds.begin(), succ.linear_preds.end(), block->index);
                        unsigned op_idx = std::distance(succ.linear_preds.begin(), it);
                        for (aco_ptr<Instruction>& phi : succ.instructions) {
                              if (UNLIKELY(!is_phi(phi)))
                                    break;
                              if (UNLIKELY(phi->opcode == aco_opcode::p_phi || phi->definitions[0].isKill()))
                                    continue;
                              if (LIKELY(phi->operands[op_idx].isTemp()))
                                    live.insert(phi->operands[op_idx].tempId());
                        }
                  }
                  if (block->logical_succs.size() == 1 && block->logical_succs[0] >= ctx.handled_once) {
                        Block& succ = ctx.program->blocks[block->logical_succs[0]];
                        auto it = std::find(succ.logical_preds.begin(), succ.logical_preds.end(), block->index);
                        unsigned op_idx = std::distance(succ.logical_preds.begin(), it);
                        for (aco_ptr<Instruction>& phi : succ.instructions) {
                              if (UNLIKELY(!is_phi(phi)))
                                    break;
                              if (UNLIKELY(phi->opcode == aco_opcode::p_linear_phi || phi->definitions[0].isKill()))
                                    continue;
                              if (LIKELY(phi->operands[op_idx].isTemp()))
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

                  for (unsigned t : live)
                        new_demand += Temp(t, ctx.program->temp_rc[t]);

                  int idx;
                  for (idx = block->instructions.size() - 1; idx >= 0; idx--) {
                        Instruction* insn = block->instructions[idx].get();
                        if (UNLIKELY(is_phi(insn)))
                              break;

                        ctx.program->needs_vcc |= instr_needs_vcc(insn);
                        insn->register_demand = RegisterDemand(new_demand.vgpr, new_demand.sgpr);

                        bool has_vgpr_def = false;

                        for (Definition& definition : insn->definitions) {
                              has_vgpr_def |= definition.regClass().type() == RegType::vgpr &&
                              !definition.regClass().is_linear_vgpr();

                              if (UNLIKELY(!definition.isTemp())) {
                                    continue;
                              }
                              if (UNLIKELY(definition.isFixed() && definition.physReg() == vcc))
                                    ctx.program->needs_vcc = true;

                              const Temp temp = definition.getTemp();
                              const size_t n = live.erase(temp.id());

                              if (LIKELY(n)) {
                                    new_demand -= temp;
                                    definition.setKill(false);
                              } else {
                                    insn->register_demand += temp;
                                    definition.setKill(true);
                              }
                        }

                        bool is_vector_op = false;
                        for (Operand& op : insn->operands) {
                              op.setKill(false);
                              bool lateKill =
                              (UNLIKELY(op.hasRegClass() && op.regClass().is_linear_vgpr() && !op.isUndefined() && has_vgpr_def));
                              lateKill |= is_vector_op || op.isVectorAligned();
                              op.setLateKill(lateKill);
                              is_vector_op = op.isVectorAligned();
                        }

                        if (UNLIKELY(ctx.program->gfx_level >= GFX10 && insn->isVALU() &&
                              insn->definitions.back().regClass() == s2)) {
                              bool carry_in = insn->opcode == aco_opcode::v_addc_co_u32 ||
                              insn->opcode == aco_opcode::v_subb_co_u32 ||
                              insn->opcode == aco_opcode::v_subbrev_co_u32;
                        for (unsigned op_idx = 0; op_idx < (carry_in ? 2 : insn->operands.size()); op_idx++) {
                              if (insn->operands[op_idx].isOfType(RegType::sgpr))
                                    insn->operands[op_idx].setLateKill(true);
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
                                          insn->operands[0].setLateKill(true);
                                    } else if (UNLIKELY(insn->opcode == aco_opcode::p_init_scratch)) {
                                          insn->operands.back().setLateKill(true);
                                    } else if (UNLIKELY(instr_info.classes[(int)insn->opcode] == instr_class::wmma)) {
                                          insn->operands[0].setLateKill(true);
                                          insn->operands[1].setLateKill(true);
                                    }

                                    RegisterDemand operand_demand;
                                    auto tied_defs = get_tied_defs(insn);
                                    for (auto op_idx : tied_defs) {
                                          Temp tmp = insn->operands[op_idx].getTemp();
                                          if (UNLIKELY(std::any_of(tied_defs.begin(), tied_defs.end(), [&](uint32_t i)
                                          { return i < op_idx && insn->operands[i].getTemp() == tmp; }))) {
                                                operand_demand += tmp;
                                                insn->operands[op_idx].setCopyKill(true);
                                          }
                                          insn->operands[op_idx].setClobbered(true);
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
                                                      [=](const Definition& def)
                                                      {
                                                            return def.isFixed() &&
                                                            def.physReg() + def.size() > operand.physReg() &&
                                                            operand.physReg() + operand.size() > def.physReg();
                                                      })))
                                                      operand.setClobbered(true);

                                                for (unsigned j = i + 1; !operand.isCopyKill() && j < insn->operands.size(); ++j) {
                                                      if (UNLIKELY(insn->operands[j].isPrecolored() && insn->operands[j].getTemp() == temp)) {
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
                                                for (Operand& other : insn->operands) {
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
                                                new_demand += temp;
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
                              Block::edge_vec& preds =
                              insn->opcode == aco_opcode::p_phi ? block->logical_preds : block->linear_preds;
                              for (unsigned i = 0; i < preds.size(); i++) {
                                    if (LIKELY(insn->operands[i].isTemp()))
                                          ctx.worklist = std::max<int>(ctx.worklist, preds[i]);
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
                              [[maybe_unused]] ASSERTED bool is_valid = validate_ir(ctx.program);
                              assert(!is_valid);
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
                  unsigned workgroup_size =
                  program->workgroup_size == UINT_MAX ? program->wave_size : program->workgroup_size;

                  return align(workgroup_size, program->wave_size) / program->wave_size;
            }
      }

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
                  }
                  else if (UNLIKELY(program->dev.xnack_enabled)) {
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
            return a - (a % b);
      }

      [[nodiscard]] RegisterDemand
      get_addr_regs_from_waves(Program* program, uint16_t waves)
      {
            uint16_t sgprs = std::min(program->dev.physical_sgprs / waves, 128);
            sgprs = round_down(sgprs, program->dev.sgpr_alloc_granule) - get_extra_sgprs(program);
            sgprs = std::min(sgprs, program->dev.sgpr_limit);

            uint16_t vgprs = program->dev.physical_vgprs / waves;
            vgprs = vgprs / program->dev.vgpr_alloc_granule * program->dev.vgpr_alloc_granule;
            vgprs -= program->config->num_shared_vgprs / 2;
            vgprs = std::min(vgprs, program->dev.vgpr_limit);
            return RegisterDemand(vgprs, sgprs);
      }

      void
      calc_min_waves(Program* program)
      {
            unsigned waves_per_workgroup = calc_waves_per_workgroup(program);
            unsigned simd_per_cu_wgp = program->dev.simd_per_cu * (program->wgp_mode ? 2 : 1);
            program->min_waves = DIV_ROUND_UP(waves_per_workgroup, simd_per_cu_wgp);
      }

      [[nodiscard]] uint16_t
      max_suitable_waves(Program* program, uint16_t waves)
      {
            unsigned num_simd = program->dev.simd_per_cu * (program->wgp_mode ? 2 : 1);
            unsigned waves_per_workgroup = calc_waves_per_workgroup(program);
            unsigned num_workgroups = waves * num_simd / waves_per_workgroup;

            unsigned lds_per_workgroup = align(program->config->lds_size * program->dev.lds_encoding_granule,
                                               program->dev.lds_alloc_granule);

            if (program->stage == fragment_fs) {
                  unsigned lds_bytes_per_interp = 3 * 16;
                  unsigned lds_param_bytes = lds_bytes_per_interp * program->info.ps.num_inputs;
                  lds_per_workgroup += align(lds_param_bytes, program->dev.lds_alloc_granule);
            }
            unsigned lds_limit = program->wgp_mode ? program->dev.lds_limit * 2 : program->dev.lds_limit;
            if (lds_per_workgroup)
                  num_workgroups = std::min(num_workgroups, lds_limit / lds_per_workgroup);

            if (waves_per_workgroup > 1)
                  num_workgroups = std::min(num_workgroups, program->wgp_mode ? 32u : 16u);

            unsigned workgroup_waves = num_workgroups * waves_per_workgroup;
            return DIV_ROUND_UP(workgroup_waves, num_simd);
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
                  program->num_waves = program->dev.physical_sgprs / get_sgpr_alloc(program, new_demand.sgpr);
                  uint16_t vgpr_demand =
                  get_vgpr_alloc(program, new_demand.vgpr) + program->config->num_shared_vgprs / 2;
                  program->num_waves =
                  std::min<uint16_t>(program->num_waves, program->dev.physical_vgprs / vgpr_demand);
                  program->num_waves = std::min(program->num_waves, program->dev.max_waves_per_simd);

                  program->num_waves = max_suitable_waves(program, program->num_waves);
                  program->max_reg_demand = get_addr_regs_from_waves(program, program->num_waves);
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

}
