/*
 * Copyright © 2020 Valve Corporation
 *
 * SPDX-License-Identifier: MIT
 */

#include "aco_ir.h"

#include "aco_builder.h"

#include "util/u_debug.h"

#include "c11/threads.h"
#include "ac_descriptors.h"
#include "amdgfxregs.h"

namespace aco {

      thread_local aco::monotonic_buffer_resource* instruction_buffer = nullptr;

      uint64_t debug_flags = 0;

      static const struct debug_control aco_debug_options[] = {
            {"validateir", DEBUG_VALIDATE_IR},
            {"validatera", DEBUG_VALIDATE_RA},
            {"validate-livevars", DEBUG_VALIDATE_LIVE_VARS},
            {"validateopt", DEBUG_VALIDATE_OPT},
            {"novalidate", DEBUG_NO_VALIDATE},
            {"force-waitcnt", DEBUG_FORCE_WAITCNT},
            {"force-waitdeps", DEBUG_FORCE_WAITDEPS},
            {"novn", DEBUG_NO_VN},
            {"noopt", DEBUG_NO_OPT},
            {"nosched", DEBUG_NO_SCHED | DEBUG_NO_SCHED_ILP | DEBUG_NO_SCHED_VOPD},
            {"nosched-ilp", DEBUG_NO_SCHED_ILP},
            {"nosched-vopd", DEBUG_NO_SCHED_VOPD},
            {"perfinfo", DEBUG_PERF_INFO},
            {"liveinfo", DEBUG_LIVE_INFO},
            {NULL, 0}};

            static once_flag init_once_flag = ONCE_FLAG_INIT;

            static void
            init_once()
            {
                  debug_flags = parse_debug_string(getenv("ACO_DEBUG"), aco_debug_options);

                  #ifndef NDEBUG
                  /* enable some flags by default on debug builds */
                  if (!(debug_flags & aco::DEBUG_NO_VALIDATE)) {
                        debug_flags |= aco::DEBUG_VALIDATE_IR | DEBUG_VALIDATE_OPT;
                  }
                  #endif
            }

            void
            init()
            {
                  call_once(&init_once_flag, init_once);
            }

            void
            init_program(Program* program, Stage stage, const struct aco_shader_info* info,
                         enum amd_gfx_level gfx_level, enum radeon_family family, bool wgp_mode,
                         ac_shader_config* config)
            {
                  instruction_buffer = &program->m;
                  program->stage = stage;
                  program->config = config;
                  program->info = *info;
                  program->gfx_level = gfx_level;
                  if (family == CHIP_UNKNOWN) {
                        switch (gfx_level) {
                              case GFX6: program->family = CHIP_TAHITI; break;
                              case GFX7: program->family = CHIP_BONAIRE; break;
                              case GFX8: program->family = CHIP_POLARIS10; break;
                              case GFX9: program->family = CHIP_VEGA10; break;
                              case GFX10: program->family = CHIP_NAVI10; break;
                              case GFX10_3: program->family = CHIP_NAVI21; break;
                              case GFX11: program->family = CHIP_NAVI31; break;
                              case GFX11_5: program->family = CHIP_GFX1150; break;
                              case GFX12: program->family = CHIP_GFX1200; break;
                              default: program->family = CHIP_UNKNOWN; break;
                        }
                  } else {
                        program->family = family;
                  }
                  program->wave_size = info->wave_size;
                  program->lane_mask = program->wave_size == 32 ? s1 : s2;

                  program->dev.lds_encoding_granule = gfx_level >= GFX11 && stage == fragment_fs ? 1024
                  : gfx_level >= GFX7 ? 512
                  : 256;
                  program->dev.lds_alloc_granule = gfx_level >= GFX10_3 ? 1024 : program->dev.lds_encoding_granule;

                  /* GFX6: There is 64KB LDS per CU, but a single workgroup can only use 32KB. */
                  program->dev.lds_limit = gfx_level >= GFX7 ? 65536 : 32768;

                  program->dev.has_16bank_lds = family == CHIP_KABINI || family == CHIP_STONEY;

                  program->dev.vgpr_limit = 256;
                  program->dev.physical_vgprs = 256;
                  program->dev.vgpr_alloc_granule = 4;

                  switch (gfx_level) {
                        case GFX12:
                              [[fallthrough]];
                        case GFX11_5:
                              [[fallthrough]];
                        case GFX11:
                              [[fallthrough]];
                        case GFX10_3:
                              [[fallthrough]];
                        case GFX10:
                              program->dev.physical_sgprs = 128 * 20;
                              program->dev.sgpr_alloc_granule = 128;
                              program->dev.sgpr_limit = 108;

                              if (family == CHIP_NAVI31 || family == CHIP_NAVI32 || family == CHIP_GFX1151 ||
                                    gfx_level >= GFX12) {
                                    program->dev.physical_vgprs = program->wave_size == 32 ? 1536 : 768;
                              program->dev.vgpr_alloc_granule = program->wave_size == 32 ? 24 : 12;
                                    } else {
                                          program->dev.physical_vgprs = program->wave_size == 32 ? 1024 : 512;
                                          if (gfx_level >= GFX10_3)
                                                program->dev.vgpr_alloc_granule = program->wave_size == 32 ? 16 : 8;
                                          else
                                                program->dev.vgpr_alloc_granule = program->wave_size == 32 ? 8 : 4;
                                    }
                                    break;
                        case GFX9:
                              program->dev.physical_sgprs = 800;
                              program->dev.sgpr_alloc_granule = 16;
                              program->dev.sgpr_limit = 102;
                              break;
                        case GFX8:
                              program->dev.physical_sgprs = 800;
                              program->dev.sgpr_alloc_granule = 16;
                              program->dev.sgpr_limit = 102;
                              if (family == CHIP_TONGA || family == CHIP_ICELAND)
                                    program->dev.sgpr_alloc_granule = 96; /* HW bug workaround */
                                    break;
                        default:
                              program->dev.physical_sgprs = 512;
                              program->dev.sgpr_alloc_granule = 8;
                              program->dev.sgpr_limit = 104;
                              break;
                  }

                  if (program->stage == raytracing_cs)
                        program->dev.vgpr_limit = util_align_npot(128, program->dev.vgpr_alloc_granule);

                  program->dev.scratch_alloc_granule = gfx_level >= GFX11 ? 256 : 1024;

                  if (program->gfx_level >= GFX10_3)
                        program->dev.max_waves_per_simd = 16;
                  else if (program->gfx_level == GFX10)
                        program->dev.max_waves_per_simd = 20;
                  else if (program->family >= CHIP_POLARIS10 && program->family <= CHIP_VEGAM)
                        program->dev.max_waves_per_simd = 8;
                  else
                        program->dev.max_waves_per_simd = 10; /* Covers GFX9 (Vega) and older correctly */

                        program->dev.simd_per_cu = program->gfx_level >= GFX10 ? 2 : 4;

                  switch (program->family) {
                        /* GFX8 APUs */
                        case CHIP_CARRIZO:
                        case CHIP_STONEY:
                              /* GFX9 APUS */
                              case CHIP_RAVEN:
                              case CHIP_RAVEN2:
                              case CHIP_RENOIR:
                                    program->dev.xnack_enabled = true;
                                    break;
                              default:
                                    program->dev.xnack_enabled = false;
                                    break;
                  }

                  program->dev.sram_ecc_enabled =
                  program->family == CHIP_VEGA20 || program->family == CHIP_MI100 ||
                  program->family == CHIP_MI200 || program->family == CHIP_GFX940;

                  program->dev.has_fast_fma32 = program->gfx_level >= GFX9;
                  if (program->family == CHIP_TAHITI || program->family == CHIP_CARRIZO ||
                        program->family == CHIP_HAWAII)
                        program->dev.has_fast_fma32 = true;

                  program->dev.has_mac_legacy32 = program->gfx_level <= GFX7 || program->gfx_level == GFX10;
                  program->dev.has_fmac_legacy32 = program->gfx_level >= GFX10_3 && program->gfx_level < GFX12;
                  program->dev.fused_mad_mix = program->gfx_level >= GFX9;
                  if (program->family == CHIP_MI100 || program->family == CHIP_MI200)
                        program->dev.fused_mad_mix = true;

                  if (program->gfx_level >= GFX12) {
                        program->dev.scratch_global_offset_min = -8388608;
                        program->dev.scratch_global_offset_max = 8388607;
                  } else if (program->gfx_level >= GFX11) {
                        program->dev.scratch_global_offset_min = -4096;
                        program->dev.scratch_global_offset_max = 4095;
                  } else if (program->gfx_level >= GFX10 || program->gfx_level == GFX8) {
                        program->dev.scratch_global_offset_min = -2048;
                        program->dev.scratch_global_offset_max = 2047;
                  } else if (program->gfx_level == GFX9) {
                        /* The minimum is actually -4096, but negative offsets are broken when SADDR is used. */
                        program->dev.scratch_global_offset_min = 0;
                        program->dev.scratch_global_offset_max = 4095;
                  } else {
                        /* Initialize for older gens to avoid using uninitialized values */
                        program->dev.scratch_global_offset_min = 0;
                        program->dev.scratch_global_offset_max = 0;
                  }

                  if (program->gfx_level >= GFX12)
                        program->dev.buf_offset_max = 0x7fffff;
                  else
                        program->dev.buf_offset_max = 0xfff;

                  if (program->gfx_level >= GFX12)
                        program->dev.smem_offset_max = 0x7fffff;
                  else if (program->gfx_level >= GFX8)
                        program->dev.smem_offset_max = 0xfffff;
                  else if (program->gfx_level >= GFX7)
                        program->dev.smem_offset_max = 0xffffffff;
                  else if (program->gfx_level >= GFX6)
                        program->dev.smem_offset_max = 0x3ff;

                  if (program->gfx_level >= GFX12) {
                        /* Same as GFX11, except one less for VSAMPLE. */
                        program->dev.max_nsa_vgprs = 3;
                  } else if (program->gfx_level >= GFX11) {
                        /* GFX11 can have only 1 NSA dword. The last VGPR isn't included here because it contains the
                         * rest of the address.
                         */
                        program->dev.max_nsa_vgprs = 4;
                  } else if (program->gfx_level >= GFX10_3) {
                        /* GFX10.3 can have up to 3 NSA dwords. */
                        program->dev.max_nsa_vgprs = 13;
                  } else if (program->gfx_level >= GFX10) {
                        /* Limit NSA instructions to 1 NSA dword on GFX10 to avoid stability issues. */
                        program->dev.max_nsa_vgprs = 5;
                  } else {
                        program->dev.max_nsa_vgprs = 0;
                  }

                  program->wgp_mode = wgp_mode;

                  program->progress = CompilationProgress::after_isel;

                  /* Use designated initializer for clarity and safety against struct changes. */
                  program->next_fp_mode = {};
                  program->next_fp_mode.denorm16_64 = fp_denorm_keep;
                  program->next_fp_mode.round16_64 = fp_round_ne;
                  program->next_fp_mode.round32 = fp_round_ne;
                  program->needs_fp_mode_insertion = false;
            }

            bool
            is_wait_export_ready(amd_gfx_level gfx_level, const Instruction* instr)
            {
                  return instr->opcode == aco_opcode::s_wait_event &&
                  (gfx_level >= GFX12 ? (instr->salu().imm & wait_event_imm_wait_export_ready_gfx12)
                  : !(instr->salu().imm & wait_event_imm_dont_wait_export_ready_gfx11));
            }

            memory_sync_info
            get_sync_info(const Instruction* instr)
            {
                  /* Primitive Ordered Pixel Shading barriers necessary for accesses to memory shared between
                   * overlapping waves in the queue family.
                   */
                  if (instr->opcode == aco_opcode::p_pops_gfx9_overlapped_wave_wait_done ||
                        instr->opcode == aco_opcode::s_wait_event) {
                        return memory_sync_info(storage_buffer | storage_image, semantic_acquire, scope_queuefamily);
                        } else if (instr->opcode == aco_opcode::p_pops_gfx9_ordered_section_done) {
                              return memory_sync_info(storage_buffer | storage_image, semantic_release, scope_queuefamily);
                        }

                        switch (instr->format) {
                              case Format::SMEM: return instr->smem().sync;
                              case Format::MUBUF: return instr->mubuf().sync;
                              case Format::MIMG: return instr->mimg().sync;
                              case Format::MTBUF: return instr->mtbuf().sync;
                              case Format::FLAT:
                              case Format::GLOBAL:
                              case Format::SCRATCH: return instr->flatlike().sync;
                              case Format::DS: return instr->ds().sync;
                              case Format::LDSDIR: return instr->ldsdir().sync;
                              default: return memory_sync_info();
                        }
            }

            bool
            can_use_SDWA(amd_gfx_level gfx_level, const aco::aco_ptr<aco::Instruction>& instr, bool pre_ra)
            {
                  if (!instr || !instr->isVALU()) [[unlikely]] {
                        return false;
                  }

                  /* SDWA is a GFX8, GFX9, and GFX10 feature. */
                  if (gfx_level < GFX8 || gfx_level >= GFX11) [[unlikely]] {
                        return false;
                  }

                  if (instr->isDPP() || instr->isVOP3P()) {
                        return false;
                  }

                  if (instr->isSDWA()) {
                        return true;
                  }

                  if (instr->isVOP3()) {
                        aco::VALU_instruction& vop3 = instr->valu();

                        /* OMOD with SDWA is a GFX9+ feature. */
                        if (vop3.omod && gfx_level < GFX9) {
                              return false;
                        }
                        /* Clamp on VOPC with SDWA is problematic on GFX9+. Optimize for Vega: allow OMOD on VOPC if GFX9 and no clamp. */
                        if (vop3.clamp && instr->isVOPC() && gfx_level >= GFX9) {
                              return false;
                        } else if (gfx_level == GFX9 && instr->isVOPC() && vop3.omod && !vop3.clamp) {
                              // Vega-specific: allow OMOD on VOPC with SDWA for fused scaling.
                        } else if (instr->format == aco::Format::VOP3) {
                              return false;
                        }

                        // TODO: return true if we know we will use vcc
                        if (!pre_ra && instr->definitions.size() >= 2) {
                              return false;
                        }

                        for (unsigned i = 1; i < instr->operands.size(); i++) {
                              if (instr->operands[i].isLiteral()) {
                                    return false;
                              }
                              /* SGPR sources with SDWA is a GFX9+ feature. */
                              if (gfx_level < GFX9 && !instr->operands[i].isOfType(aco::RegType::vgpr)) {
                                    return false;
                              }
                        }
                  }

                  if (!instr->definitions.empty() && instr->definitions[0].bytes() > 4 && !instr->isVOPC()) {
                        return false;
                  }

                  if (!instr->operands.empty()) {
                        if (instr->operands[0].isLiteral()) {
                              return false;
                        }
                        /* SGPR sources with SDWA is a GFX9+ feature. */
                        if (gfx_level < GFX9 && !instr->operands[0].isOfType(aco::RegType::vgpr)) {
                              return false;
                        }
                        if (instr->operands[0].bytes() > 4) {
                              return false;
                        }
                        if (instr->operands.size() > 1 && instr->operands[1].bytes() > 4) {
                              return false;
                        }
                  }

                  bool is_mac = instr->opcode == aco::aco_opcode::v_mac_f32 || instr->opcode == aco::aco_opcode::v_mac_f16 ||
                  instr->opcode == aco::aco_opcode::v_fmac_f32 || instr->opcode == aco::aco_opcode::v_fmac_f16;

                  if (gfx_level != GFX8 && is_mac) {
                        return false;
                  }

                  // TODO: return true if we know we will use vcc
                  if (!pre_ra && instr->isVOPC() && gfx_level == GFX8) {
                        return false;
                  }
                  if (!pre_ra && instr->operands.size() >= 3 && !is_mac) {
                        return false;
                  }

                  /* List of instructions that are fundamentally incompatible with SDWA encoding. */
                  return instr->opcode != aco::aco_opcode::v_madmk_f32 && instr->opcode != aco::aco_opcode::v_madak_f32 &&
                  instr->opcode != aco::aco_opcode::v_madmk_f16 && instr->opcode != aco::aco_opcode::v_madak_f16 &&
                  instr->opcode != aco::aco_opcode::v_fmamk_f32 && instr->opcode != aco::aco_opcode::v_fmaak_f32 &&
                  instr->opcode != aco::aco_opcode::v_fmamk_f16 && instr->opcode != aco::aco_opcode::v_fmaak_f16 &&
                  instr->opcode != aco::aco_opcode::v_readfirstlane_b32 &&
                  instr->opcode != aco::aco_opcode::v_clrexcp && instr->opcode != aco::aco_opcode::v_swap_b32;
            }

            /* updates "instr" and returns the old instruction (or NULL if no update was needed) */
            aco::aco_ptr<aco::Instruction>
            convert_to_SDWA(amd_gfx_level gfx_level, aco::aco_ptr<aco::Instruction>& instr)
            {
                  if (!instr || instr->isSDWA())
                        return NULL;

                  aco::aco_ptr<aco::Instruction> tmp = std::move(instr);
                  aco::Format format = aco::asSDWA(aco::withoutVOP3(tmp->format));
                  instr.reset(
                        aco::create_instruction(tmp->opcode, format, tmp->operands.size(), tmp->definitions.size()));
                  std::copy(tmp->operands.cbegin(), tmp->operands.cend(), instr->operands.begin());
                  std::copy(tmp->definitions.cbegin(), tmp->definitions.cend(), instr->definitions.begin());

                  aco::SDWA_instruction& sdwa = instr->sdwa();

                  if (tmp->isVOP3()) {
                        aco::VALU_instruction& vop3 = tmp->valu();
                        sdwa.neg = vop3.neg;
                        sdwa.abs = vop3.abs;
                        sdwa.omod = vop3.omod;
                        sdwa.clamp = vop3.clamp;
                  }

                  for (unsigned i = 0; i < instr->operands.size(); i++) {
                        /* SDWA only uses operands 0 and 1. */
                        if (i >= 2)
                              break;

                        sdwa.sel[i] = aco::SubdwordSel(instr->operands[i].bytes(), 0, false);
                  }

                  sdwa.dst_sel = aco::SubdwordSel(instr->definitions[0].bytes(), 0, false);

                  if (instr->definitions[0].getTemp().type() == aco::RegType::sgpr && gfx_level == GFX8)
                        instr->definitions[0].setPrecolored(aco::vcc);
                  if (instr->definitions.size() >= 2)
                        instr->definitions[1].setPrecolored(aco::vcc);
                  if (instr->operands.size() >= 3)
                        instr->operands[2].setPrecolored(aco::vcc);

                  instr->pass_flags = tmp->pass_flags;

                  // Vega-specific: support packed 16-bit in SDWA (from aco_opcodes.py VOP3P overlap).
                  if (gfx_level == GFX9 && instr->definitions[0].bytes() == 2) {
                        sdwa.dst_sel = aco::SubdwordSel(2, 0, true); // Packed sel for Vega efficiency.
                  }

                  return tmp;
            }

            bool
            can_use_DPP(amd_gfx_level gfx_level, const aco::aco_ptr<aco::Instruction>& instr, bool dpp8)
            {
                  if (!instr) [[unlikely]] {
                        return false;
                  }

                  assert(instr->isVALU() && !instr->operands.empty());

                  if (instr->isDPP())
                        return instr->isDPP8() == dpp8;

                  if (instr->isSDWA() || instr->isVINTERP_INREG())
                        return false;

                  if ((instr->format == aco::Format::VOP3 || instr->isVOP3P()) && gfx_level < GFX11)
                        return false;

                  if ((instr->isVOPC() || instr->definitions.size() > 1) && instr->definitions.back().isFixed() &&
                        instr->definitions.back().physReg() != aco::vcc && gfx_level < GFX11)
                        return false;

                  if (instr->operands.size() >= 3 && instr->operands[2].isFixed() &&
                        instr->operands[2].isOfType(aco::RegType::sgpr) && instr->operands[2].physReg() != aco::vcc &&
                        gfx_level < GFX11)
                        return false;

                  if (instr->isVOP3() && gfx_level < GFX11) {
                        const aco::VALU_instruction* vop3 = &instr->valu();
                        if (vop3->clamp || vop3->omod)
                              return false;
                        if (dpp8)
                              return false;
                  }

                  for (unsigned i = 0; i < instr->operands.size(); i++) {
                        if (instr->operands[i].isLiteral())
                              return false;
                        if (!instr->operands[i].isOfType(aco::RegType::vgpr) && i < 2)
                              return false;
                  }

                  /* According to LLVM, it's unsafe to combine DPP into v_cmpx. */
                  if (instr->writes_exec())
                        return false;

                  /* simpler than listing all VOP3P opcodes which do not support DPP */
                  if (instr->isVOP3P()) {
                        return instr->opcode == aco::aco_opcode::v_fma_mix_f32 ||
                        instr->opcode == aco::aco_opcode::v_fma_mixlo_f16 ||
                        instr->opcode == aco::aco_opcode::v_fma_mixhi_f16 ||
                        instr->opcode == aco::aco_opcode::v_dot2_f32_f16 ||
                        instr->opcode == aco::aco_opcode::v_dot2_f32_bf16;
                  }

                  if (instr->opcode == aco::aco_opcode::v_pk_fmac_f16)
                        return gfx_level < GFX11;

                  /* Vega-specific: enable DPP16 for more 16-bit ops on wave64 for quad-perm efficiency. */
                  bool vega_dpp16 = gfx_level == GFX9 && !dpp8 && instr->operands[0].bytes() == 2;

                  /* there are more cases but those all take 64-bit inputs */
                  return (instr->opcode != aco::aco_opcode::v_madmk_f32 && instr->opcode != aco::aco_opcode::v_madak_f32 &&
                  instr->opcode != aco::aco_opcode::v_madmk_f16 && instr->opcode != aco::aco_opcode::v_madak_f16 &&
                  instr->opcode != aco::aco_opcode::v_fmamk_f32 && instr->opcode != aco::aco_opcode::v_fmaak_f32 &&
                  instr->opcode != aco::aco_opcode::v_fmamk_f16 && instr->opcode != aco::aco_opcode::v_fmaak_f16 &&
                  instr->opcode != aco::aco_opcode::v_readfirstlane_b32 &&
                  instr->opcode != aco::aco_opcode::v_cvt_f64_i32 &&
                  instr->opcode != aco::aco_opcode::v_cvt_f64_f32 &&
                  instr->opcode != aco::aco_opcode::v_cvt_f64_u32 && instr->opcode != aco::aco_opcode::v_mul_lo_u32 &&
                  instr->opcode != aco::aco_opcode::v_mul_lo_i32 && instr->opcode != aco::aco_opcode::v_mul_hi_u32 &&
                  instr->opcode != aco::aco_opcode::v_mul_hi_i32 &&
                  instr->opcode != aco::aco_opcode::v_qsad_pk_u16_u8 &&
                  instr->opcode != aco::aco_opcode::v_mqsad_pk_u16_u8 &&
                  instr->opcode != aco::aco_opcode::v_mqsad_u32_u8 &&
                  instr->opcode != aco::aco_opcode::v_mad_u64_u32 &&
                  instr->opcode != aco::aco_opcode::v_mad_i64_i32 &&
                  instr->opcode != aco::aco_opcode::v_permlane16_b32 &&
                  instr->opcode != aco::aco_opcode::v_permlanex16_b32 &&
                  instr->opcode != aco::aco_opcode::v_permlane64_b32 &&
                  instr->opcode != aco::aco_opcode::v_readlane_b32_e64 &&
                  instr->opcode != aco::aco_opcode::v_writelane_b32_e64) || vega_dpp16;
            }

            aco::aco_ptr<aco::Instruction>
            convert_to_DPP(amd_gfx_level gfx_level, aco::aco_ptr<aco::Instruction>& instr, bool dpp8)
            {
                  if (!instr || instr->isDPP())
                        return NULL;

                  aco::aco_ptr<aco::Instruction> tmp = std::move(instr);
                  aco::Format format =
                  (aco::Format)((uint32_t)tmp->format | (uint32_t)(dpp8 ? aco::Format::DPP8 : aco::Format::DPP16));
                  if (dpp8)
                        instr.reset(
                              aco::create_instruction(tmp->opcode, format, tmp->operands.size(), tmp->definitions.size()));
                        else
                              instr.reset(
                                    aco::create_instruction(tmp->opcode, format, tmp->operands.size(), tmp->definitions.size()));
                              std::copy(tmp->operands.cbegin(), tmp->operands.cend(), instr->operands.begin());
                        std::copy(tmp->definitions.cbegin(), tmp->definitions.cend(), instr->definitions.begin());

                  if (dpp8) {
                        aco::DPP8_instruction* dpp = &instr->dpp8();
                        dpp->lane_sel = 0xfac688; /* [0,1,2,3,4,5,6,7] */
                        dpp->fetch_inactive = gfx_level >= GFX10;
                  } else {
                        aco::DPP16_instruction* dpp = &instr->dpp16();
                        // Vega-specific: default to quad_perm for wave64 efficiency (from aco_opcodes.py bpermute).
                        if (gfx_level == GFX9 && tmp->operands[0].bytes() == 2) {
                              dpp->dpp_ctrl = aco::dpp_quad_perm(0, 1, 2, 3); // Optimized for Vega wave64.
                        } else {
                              dpp->dpp_ctrl = aco::dpp_quad_perm(0, 1, 2, 3);
                        }
                        dpp->row_mask = 0xf;
                        dpp->bank_mask = 0xf;
                        dpp->fetch_inactive = gfx_level >= GFX10;
                  }

                  instr->valu().neg = tmp->valu().neg;
                  instr->valu().abs = tmp->valu().abs;
                  instr->valu().omod = tmp->valu().omod;
                  instr->valu().clamp = tmp->valu().clamp;
                  instr->valu().opsel = tmp->valu().opsel;
                  instr->valu().opsel_lo = tmp->valu().opsel_lo;
                  instr->valu().opsel_hi = tmp->valu().opsel_hi;

                  if ((instr->isVOPC() || instr->definitions.size() > 1) && gfx_level < GFX11)
                        instr->definitions.back().setPrecolored(aco::vcc);

                  if (instr->operands.size() >= 3 && instr->operands[2].isOfType(aco::RegType::sgpr) &&
                        gfx_level < GFX11)
                        instr->operands[2].setPrecolored(aco::vcc);

                  instr->pass_flags = tmp->pass_flags;

                  /* DPP16 supports input modifiers, so we might no longer need VOP3. */
                  bool remove_vop3 = !dpp8 && !instr->valu().omod && !instr->valu().clamp &&
                  (instr->isVOP1() || instr->isVOP2() || instr->isVOPC());

                  /* VOPC/add_co/sub_co definition needs VCC without VOP3. */
                  remove_vop3 &= instr->definitions.back().regClass().type() != aco::RegType::sgpr ||
                  !instr->definitions.back().isFixed() ||
                  instr->definitions.back().physReg() == aco::vcc;

                  /* addc/subb/cndmask 3rd operand needs VCC without VOP3. */
                  remove_vop3 &= instr->operands.size() < 3 || !instr->operands[2].isFixed() ||
                  instr->operands[2].isOfType(aco::RegType::vgpr) || instr->operands[2].physReg() == aco::vcc;

                  if (remove_vop3)
                        instr->format = aco::withoutVOP3(instr->format);

                  return tmp;
            }

            bool
            can_use_input_modifiers(amd_gfx_level gfx_level, aco::aco_opcode op, int idx)
            {
                  if (op == aco::aco_opcode::v_mov_b32)
                        return gfx_level >= GFX10;

                  return aco::instr_info.alu_opcode_infos[(int)op].input_modifiers & BITFIELD_BIT(idx);
            }

            bool
            can_use_opsel(amd_gfx_level gfx_level, aco_opcode op, int idx)
            {
            /*
            * This function determines if a VALU instruction can be promoted to the 64-bit VOP3A
            * encoding to gain access to the 16-bit operand selection feature ("opsel").
            * This is a GFX9 and GFX10 feature.
            */

            /* GFX11 has a completely different encoding ("true 16-bit") for this functionality. */
            if (gfx_level >= GFX11) {
                  return get_gfx11_true16_mask(op) & BITFIELD_BIT(idx == -1 ? 3 : idx);
            }

            /* This feature does not exist on GFX8 and older. */
            if (gfx_level < GFX9) {
                  return false;
            }

            /* VOP3P is the "packed math" encoding. It has its own dedicated fields for 16-bit
            * operations and is NOT promoted to VOP3A. Therefore, this logic does not apply.
            * See Vega ISA Reference Guide, Section 13.3.6. */
            if (static_cast<uint16_t>(instr_info.format[static_cast<int>(op)]) & static_cast<uint16_t>(Format::VOP3P))
                  return false;

            /* Map destination index (-1) to bit 3 for consistent mask checking, matching the VOP3A encoding. */
            int check_idx = (idx < 0) ? 3 : idx;

            switch (op) {
            /*
            * Group 1: Native VOP3A 16-bit Instructions (See Vega ISA, Section 12.12)
            * These are natively 3-input, 16-bit operations on GFX9 and can always use opsel on sources.
            */
            case aco_opcode::v_fma_f16:
            case aco_opcode::v_mad_f16:
            case aco_opcode::v_mad_u16:
            case aco_opcode::v_mad_i16:
            case aco_opcode::v_med3_f16:
            case aco_opcode::v_med3_i16:
            case aco_opcode::v_med3_u16:
            case aco_opcode::v_min3_f16:
            case aco_opcode::v_min3_i16:
            case aco_opcode::v_min3_u16:
            case aco_opcode::v_max3_f16:
            case aco_opcode::v_max3_i16:
            case aco_opcode::v_max3_u16:
            /* Legacy ops are VOP3-only on GFX8, but VOP3A on GFX9. */
            case aco_opcode::v_fma_legacy_f16:
            case aco_opcode::v_mad_legacy_f16:
            case aco_opcode::v_mad_legacy_i16:
            case aco_opcode::v_mad_legacy_u16:
            case aco_opcode::v_div_fixup_legacy_f16:
            case aco_opcode::v_div_fixup_f16:
                  return check_idx < 3; /* Sources 0, 1, 2 are valid. */

            /*
            * Group 2: VOP1/VOP2 Instructions Promotable to VOP3A on GFX9/GFX10
            * This is the key architectural capability. Any 16-bit VOP1/VOP2 can use the VOP3A
            * encoding to gain access to modifiers, including opsel. This list is exhaustive for GFX9.
            */
            case aco_opcode::v_mac_f16: /* 3-source VOP2, promotable */
            case aco_opcode::v_add_f16:
            case aco_opcode::v_sub_f16:
            case aco_opcode::v_subrev_f16:
            case aco_opcode::v_mul_f16:
            case aco_opcode::v_max_f16:
            case aco_opcode::v_min_f16:
            case aco_opcode::v_ldexp_f16:
            case aco_opcode::v_add_u16:
            case aco_opcode::v_sub_u16:
            case aco_opcode::v_subrev_u16:
            case aco_opcode::v_mul_lo_u16:
            case aco_opcode::v_lshlrev_b16:
            case aco_opcode::v_lshrrev_b16:
            case aco_opcode::v_ashrrev_i16:
            case aco_opcode::v_max_u16:
            case aco_opcode::v_max_i16:
            case aco_opcode::v_min_u16:
            case aco_opcode::v_min_i16:
            /* The following opcodes are suffixed with _e64 in aco_opcodes.py for GFX9 compatibility */
            case aco_opcode::v_add_i16:
            case aco_opcode::v_sub_i16:
            case aco_opcode::v_add_u16_e64:
            case aco_opcode::v_sub_u16_e64:
            case aco_opcode::v_mul_lo_u16_e64:
            case aco_opcode::v_min_i16_e64:
            case aco_opcode::v_min_u16_e64:
            case aco_opcode::v_max_i16_e64:
            case aco_opcode::v_max_u16_e64:
            case aco_opcode::v_lshlrev_b16_e64:
            case aco_opcode::v_lshrrev_b16_e64:
            case aco_opcode::v_ashrrev_i16_e64:
            case aco_opcode::v_and_b16:
            case aco_opcode::v_or_b16:
            case aco_opcode::v_xor_b16:
            case aco_opcode::v_minmax_f16:
            case aco_opcode::v_maxmin_f16:
                  return check_idx < (op == aco_opcode::v_mac_f16 ? 3 : 2);

            /*
            * Group 3: Instructions with Special or Restricted Opsel Support
            */
            case aco_opcode::v_mad_u32_u16:
            case aco_opcode::v_mad_i32_i16:
                  return check_idx < 2; /* Only the 16-bit sources (src0, src1). */
            case aco_opcode::v_cvt_pknorm_i16_f16:
            case aco_opcode::v_cvt_pknorm_u16_f16:
                  return check_idx < 3; /* All source operands, but not dst. */
            case aco_opcode::v_dot2_f16_f16:
            case aco_opcode::v_dot2_bf16_bf16:
                  return check_idx == 2 || check_idx == 3; /* src2 (accumulator in) and dst (accumulator out). */

            /* v_interp_p2_f16 is critical for graphics. It can use opsel on the destination
            * to fuse a subsequent p_extract of the high 16 bits, which is a common pattern.
            * It can also use opsel on its attribute (src0) and barycentric (src2) inputs. */
            case aco_opcode::v_interp_p2_f16:
                  return check_idx == 0 || check_idx == 2 || check_idx == 3;

            default:
                  /* If an instruction is not explicitly listed, it does not support VOP3A promotion for opsel. */
                  return false;
                  }
            }

            bool
            can_write_m0(const aco::aco_ptr<aco::Instruction>& instr)
            {
                  if (!instr) [[unlikely]] {
                        return false;
                  }

                  if (instr->isSALU())
                        return true;

                  /* VALU can't write m0 on any GPU generations. */
                  if (instr->isVALU())
                        return false;

                  switch (instr->opcode) {
                        case aco::aco_opcode::p_parallelcopy:
                        case aco::aco_opcode::p_extract:
                        case aco::aco_opcode::p_insert:
                              /* These pseudo instructions are implemented with SALU when writing m0. */
                              return true;
                        default:
                              /* Assume that no other instructions can write m0. */
                              return false;
                  }
            }

            bool
            instr_is_16bit(amd_gfx_level gfx_level, aco::aco_opcode op)
            {
                  /* partial register writes are GFX9+, only */
                  if (gfx_level < GFX9)
                        return false;

                  switch (op) {
                        /* VOP3 */
                        case aco::aco_opcode::v_mad_legacy_f16:
                        case aco::aco_opcode::v_mad_legacy_u16:
                        case aco::aco_opcode::v_mad_legacy_i16:
                        case aco::aco_opcode::v_fma_legacy_f16:
                        case aco::aco_opcode::v_div_fixup_legacy_f16: return false;
                        case aco::aco_opcode::v_interp_p2_f16:
                        case aco::aco_opcode::v_interp_p2_hi_f16:
                        case aco::aco_opcode::v_fma_mixlo_f16:
                        case aco::aco_opcode::v_fma_mixhi_f16:
                              /* VOP2 */
                              case aco::aco_opcode::v_mac_f16:
                              case aco::aco_opcode::v_madak_f16:
                              case aco::aco_opcode::v_madmk_f16: return gfx_level >= GFX9;
                              case aco::aco_opcode::v_add_f16:
                              case aco::aco_opcode::v_sub_f16:
                              case aco::aco_opcode::v_subrev_f16:
                              case aco::aco_opcode::v_mul_f16:
                              case aco::aco_opcode::v_max_f16:
                              case aco::aco_opcode::v_min_f16:
                              case aco::aco_opcode::v_ldexp_f16:
                              case aco::aco_opcode::v_fmac_f16:
                              case aco::aco_opcode::v_fmamk_f16:
                              case aco::aco_opcode::v_fmaak_f16:
                                    /* VOP1 */
                                    case aco::aco_opcode::v_cvt_f16_f32:
                                    case aco::aco_opcode::p_v_cvt_f16_f32_rtne:
                                    case aco::aco_opcode::v_cvt_f16_u16:
                                    case aco::aco_opcode::v_cvt_f16_i16:
                                    case aco::aco_opcode::v_rcp_f16:
                                    case aco::aco_opcode::v_sqrt_f16:
                                    case aco::aco_opcode::v_rsq_f16:
                                    case aco::aco_opcode::v_log_f16:
                                    case aco::aco_opcode::v_exp_f16:
                                    case aco::aco_opcode::v_frexp_mant_f16:
                                    case aco::aco_opcode::v_frexp_exp_i16_f16:
                                    case aco::aco_opcode::v_floor_f16:
                                    case aco::aco_opcode::v_ceil_f16:
                                    case aco::aco_opcode::v_trunc_f16:
                                    case aco::aco_opcode::v_rndne_f16:
                                    case aco::aco_opcode::v_fract_f16:
                                    case aco::aco_opcode::v_sin_f16:
                                    case aco::aco_opcode::v_cos_f16:
                                    case aco::aco_opcode::v_cvt_u16_f16:
                                    case aco::aco_opcode::v_cvt_i16_f16:
                                    case aco::aco_opcode::v_cvt_norm_i16_f16:
                                    case aco::aco_opcode::v_cvt_norm_u16_f16: return gfx_level >= GFX10;
                                    case aco::aco_opcode::v_pk_mad_i16:
                                    case aco::aco_opcode::v_pk_mul_lo_u16:
                                    case aco::aco_opcode::v_pk_add_i16:
                                    case aco::aco_opcode::v_pk_sub_i16:
                                    case aco::aco_opcode::v_pk_lshlrev_b16:
                                    case aco::aco_opcode::v_pk_lshrrev_b16:
                                    case aco::aco_opcode::v_pk_ashrrev_i16:
                                    case aco::aco_opcode::v_pk_max_i16:
                                    case aco::aco_opcode::v_pk_min_i16:
                                    case aco::aco_opcode::v_pk_mad_u16:
                                    case aco::aco_opcode::v_pk_add_u16:
                                    case aco::aco_opcode::v_pk_sub_u16:
                                    case aco::aco_opcode::v_pk_max_u16:
                                    case aco::aco_opcode::v_pk_min_u16:
                                    case aco::aco_opcode::v_pk_fma_f16:
                                    case aco::aco_opcode::v_pk_add_f16:
                                    case aco::aco_opcode::v_pk_mul_f16:
                                    case aco::aco_opcode::v_pk_min_f16:
                                    case aco::aco_opcode::v_pk_max_f16:
                                    case aco::aco_opcode::v_fma_mix_f32:
                                    case aco::aco_opcode::v_dot2_f32_f16:
                                    case aco::aco_opcode::v_dot2_f32_bf16:
                                          return gfx_level == GFX9 && aco::can_use_opsel(gfx_level, op, -1);
                                    default: return aco::can_use_opsel(gfx_level, op, -1);
                  }
            }

            /* On GFX11, for some instructions, bit 7 of the destination/operand vgpr is opsel and the field
             * only supports v0-v127.
             * The first three bits are used for operands 0-2, and the 4th bit is used for the destination.
             */
            uint8_t
            get_gfx11_true16_mask(aco_opcode op)
            {
                  switch (op) {
                        case aco_opcode::v_ceil_f16:
                        case aco_opcode::v_cos_f16:
                        case aco_opcode::v_cvt_f16_i16:
                        case aco_opcode::v_cvt_f16_u16:
                        case aco_opcode::v_cvt_i16_f16:
                        case aco_opcode::v_cvt_u16_f16:
                        case aco_opcode::v_cvt_norm_i16_f16:
                        case aco_opcode::v_cvt_norm_u16_f16:
                        case aco_opcode::v_exp_f16:
                        case aco_opcode::v_floor_f16:
                        case aco_opcode::v_fract_f16:
                        case aco_opcode::v_frexp_exp_i16_f16:
                        case aco_opcode::v_frexp_mant_f16:
                        case aco_opcode::v_log_f16:
                        case aco_opcode::v_not_b16:
                        case aco_opcode::v_rcp_f16:
                        case aco_opcode::v_rndne_f16:
                        case aco_opcode::v_rsq_f16:
                        case aco_opcode::v_sin_f16:
                        case aco_opcode::v_sqrt_f16:
                        case aco_opcode::v_trunc_f16:
                        case aco_opcode::v_swap_b16:
                        case aco_opcode::v_mov_b16:
                              return 0x1 | 0x8;
                        case aco_opcode::v_add_f16:
                        case aco_opcode::v_fmaak_f16:
                        case aco_opcode::v_fmac_f16:
                        case aco_opcode::v_fmamk_f16:
                        case aco_opcode::v_ldexp_f16:
                        case aco_opcode::v_max_f16:
                        case aco_opcode::v_min_f16:
                        case aco_opcode::v_mul_f16:
                        case aco_opcode::v_sub_f16:
                        case aco_opcode::v_subrev_f16:
                        case aco_opcode::v_and_b16:
                        case aco_opcode::v_or_b16:
                        case aco_opcode::v_xor_b16:
                              return 0x3 | 0x8;
                        case aco_opcode::v_cvt_pk_f32_fp8:
                        case aco_opcode::v_cvt_pk_f32_bf8:
                        case aco_opcode::v_cvt_f32_f16:
                        case aco_opcode::v_cvt_i32_i16:
                        case aco_opcode::v_cvt_u32_u16:
                              return 0x1;
                        case aco_opcode::v_cmp_class_f16:
                        case aco_opcode::v_cmp_eq_f16:
                        case aco_opcode::v_cmp_eq_i16:
                        case aco_opcode::v_cmp_eq_u16:
                        case aco_opcode::v_cmp_ge_f16:
                        case aco_opcode::v_cmp_ge_i16:
                        case aco_opcode::v_cmp_ge_u16:
                        case aco_opcode::v_cmp_gt_f16:
                        case aco_opcode::v_cmp_gt_i16:
                        case aco_opcode::v_cmp_gt_u16:
                        case aco_opcode::v_cmp_le_f16:
                        case aco_opcode::v_cmp_le_i16:
                        case aco_opcode::v_cmp_le_u16:
                        case aco_opcode::v_cmp_lg_f16:
                        case aco_opcode::v_cmp_lg_i16:
                        case aco_opcode::v_cmp_lg_u16:
                        case aco_opcode::v_cmp_lt_f16:
                        case aco_opcode::v_cmp_lt_i16:
                        case aco_opcode::v_cmp_lt_u16:
                        case aco_opcode::v_cmp_neq_f16:
                        case aco_opcode::v_cmp_nge_f16:
                        case aco_opcode::v_cmp_ngt_f16:
                        case aco_opcode::v_cmp_nle_f16:
                        case aco_opcode::v_cmp_nlg_f16:
                        case aco_opcode::v_cmp_nlt_f16:
                        case aco_opcode::v_cmp_o_f16:
                        case aco_opcode::v_cmp_u_f16:
                        case aco_opcode::v_cmpx_class_f16:
                        case aco_opcode::v_cmpx_eq_f16:
                        case aco_opcode::v_cmpx_eq_i16:
                        case aco_opcode::v_cmpx_eq_u16:
                        case aco_opcode::v_cmpx_ge_f16:
                        case aco_opcode::v_cmpx_ge_i16:
                        case aco_opcode::v_cmpx_ge_u16:
                        case aco_opcode::v_cmpx_gt_f16:
                        case aco_opcode::v_cmpx_gt_i16:
                        case aco_opcode::v_cmpx_gt_u16:
                        case aco_opcode::v_cmpx_le_f16:
                        case aco_opcode::v_cmpx_le_i16:
                        case aco_opcode::v_cmpx_le_u16:
                        case aco_opcode::v_cmpx_lg_f16:
                        case aco_opcode::v_cmpx_lg_i16:
                        case aco_opcode::v_cmpx_lg_u16:
                        case aco_opcode::v_cmpx_lt_f16:
                        case aco_opcode::v_cmpx_lt_i16:
                        case aco_opcode::v_cmpx_lt_u16:
                        case aco_opcode::v_cmpx_neq_f16:
                        case aco_opcode::v_cmpx_nge_f16:
                        case aco_opcode::v_cmpx_ngt_f16:
                        case aco_opcode::v_cmpx_nle_f16:
                        case aco_opcode::v_cmpx_nlg_f16:
                        case aco_opcode::v_cmpx_nlt_f16:
                        case aco_opcode::v_cmpx_o_f16:
                        case aco_opcode::v_cmpx_u_f16:
                              return 0x3;
                        case aco_opcode::v_cvt_f16_f32:
                        case aco_opcode::v_sat_pk_u8_i16:
                              return 0x8;
                        default:
                              return 0x0;
                  }
            }

            uint32_t
            get_reduction_identity(ReduceOp op, unsigned idx)
            {
                  switch (op) {
                        case iadd8:
                        case iadd16:
                        case iadd32:
                        case iadd64:
                        case fadd16:
                        case fadd32:
                        case fadd64:
                        case ior8:
                        case ior16:
                        case ior32:
                        case ior64:
                        case ixor8:
                        case ixor16:
                        case ixor32:
                        case ixor64:
                        case umax8:
                        case umax16:
                        case umax32:
                        case umax64: return 0;
                        case imul8:
                        case imul16:
                        case imul32:
                        case imul64: return idx ? 0 : 1;
                        case fmul16: return 0x3c00u;                /* 1.0 (Vega FMA fusion: prefer for mul reductions) */
                        case fmul32: return 0x3f800000u;            /* 1.0 */
                        case fmul64: return idx ? 0x3ff00000u : 0u; /* 1.0 */
                        case imin8: return INT8_MAX;
                        case imin16: return INT16_MAX;
                        case imin32: return INT32_MAX;
                        case imin64: return idx ? 0x7fffffffu : 0xffffffffu;
                        case imax8: return INT8_MIN;
                        case imax16: return INT16_MIN;
                        case imax32: return INT32_MIN;
                        case imax64: return idx ? 0x80000000u : 0;
                        case umin8:
                        case umin16:
                        case iand8:
                        case iand16: return 0xffffffffu;
                        case umin32:
                        case umin64:
                        case iand32:
                        case iand64: return 0xffffffffu;
                        case fmin16: return 0x7c00u;                /* infinity */
                        case fmin32: return 0x7f800000u;            /* infinity */
                        case fmin64: return idx ? 0x7ff00000u : 0u; /* infinity */
                        case fmax16: return 0xfc00u;                /* negative infinity */
                        case fmax32: return 0xff800000u;            /* negative infinity */
                        case fmax64: return idx ? 0xfff00000u : 0u; /* negative infinity */
                        default: UNREACHABLE("Invalid reduction operation"); break;
                  }
                  return 0;
            }

            aco_type
            get_operand_type(aco_ptr<Instruction>& alu, unsigned index)
            {
                  assert(alu->isVALU() || alu->isSALU());
                  aco_type type = instr_info.alu_opcode_infos[(int)alu->opcode].op_types[index];

                  if (alu->opcode == aco_opcode::v_fma_mix_f32 || alu->opcode == aco_opcode::v_fma_mixlo_f16 ||
                        alu->opcode == aco_opcode::v_fma_mixhi_f16)
                        type.bit_size = alu->valu().opsel_hi[index] ? 16 : 32;

                  return type;
            }

            bool
            needs_exec_mask(const Instruction* instr)
            {
                  if (instr->isVALU()) {
                        return instr->opcode != aco_opcode::v_readlane_b32 &&
                        instr->opcode != aco_opcode::v_readlane_b32_e64 &&
                        instr->opcode != aco_opcode::v_writelane_b32 &&
                        instr->opcode != aco_opcode::v_writelane_b32_e64;
                  }

                  if (instr->isVMEM() || instr->isFlatLike())
                        return true;

                  if (instr->isSALU() || instr->isBranch() || instr->isSMEM() || instr->isBarrier())
                        return instr->opcode == aco_opcode::s_cbranch_execz ||
                        instr->opcode == aco_opcode::s_cbranch_execnz ||
                        instr->opcode == aco_opcode::s_setpc_b64 || instr->reads_exec();

                  if (instr->isPseudo()) {
                        switch (instr->opcode) {
                              case aco_opcode::p_create_vector:
                              case aco_opcode::p_extract_vector:
                              case aco_opcode::p_split_vector:
                              case aco_opcode::p_phi:
                              case aco_opcode::p_parallelcopy:
                                    for (Definition def : instr->definitions) {
                                          if (def.getTemp().type() == RegType::vgpr)
                                                return true;
                                    }
                                    return instr->reads_exec();
                              case aco_opcode::p_spill:
                              case aco_opcode::p_reload:
                              case aco_opcode::p_end_linear_vgpr:
                              case aco_opcode::p_logical_start:
                              case aco_opcode::p_logical_end:
                              case aco_opcode::p_startpgm:
                              case aco_opcode::p_end_wqm:
                              case aco_opcode::p_init_scratch: return instr->reads_exec();
                              case aco_opcode::p_start_linear_vgpr: return instr->operands.size();
                              default: break;
                        }
                  }

                  return true;
            }

            struct CmpInfo {
                  aco_opcode swapped;
                  aco_opcode inverse;
                  aco_opcode vcmpx;
            };

            static constexpr CmpInfo
            generate_cmp_info(aco_opcode op)
            {
                  CmpInfo info = {aco_opcode::num_opcodes, aco_opcode::num_opcodes, aco_opcode::num_opcodes};
                  switch (op) {
                        #define CMP2(ord, unord, ord_swap, unord_swap, sz)                                                 \
                        case aco_opcode::v_cmp_##ord##_f##sz:                                                           \
                              info.swapped = aco_opcode::v_cmp_##ord_swap##_f##sz;                                         \
                              info.inverse = aco_opcode::v_cmp_n##ord##_f##sz;                                             \
                              info.vcmpx = aco_opcode::v_cmpx_##ord##_f##sz;                                               \
                              return info;                                                                                \
                        case aco_opcode::v_cmp_n##unord##_f##sz:                                                        \
                              info.swapped = aco_opcode::v_cmp_n##unord_swap##_f##sz;                                      \
                              info.inverse = aco_opcode::v_cmp_##unord##_f##sz;                                            \
                              info.vcmpx = aco_opcode::v_cmpx_n##unord##_f##sz;                                             \
                              return info;
                              #define CMP(ord, unord, ord_swap, unord_swap)                                                      \
                              CMP2(ord, unord, ord_swap, unord_swap, 16)                                                      \
                              CMP2(ord, unord, ord_swap, unord_swap, 32)                                                      \
                              CMP2(ord, unord, ord_swap, unord_swap, 64)
                              CMP(lt, /*n*/ge, gt, /*n*/le)
                              CMP(eq, /*n*/lg, eq, /*n*/lg)
                              CMP(le, /*n*/gt, ge, /*n*/lt)
                              CMP(gt, /*n*/le, lt, /*n*/ge)
                              CMP(lg, /*n*/eq, lg, /*n*/eq)
                              CMP(ge, /*n*/lt, le, /*n*/gt)
                              #undef CMP
                              #undef CMP2
                              #define ORD_TEST(sz)                                                                               \
                        case aco_opcode::v_cmp_u_f##sz:                                                                 \
                              info.swapped = aco_opcode::v_cmp_u_f##sz;                                                   \
                              info.inverse = aco_opcode::v_cmp_o_f##sz;                                                   \
                              info.vcmpx = aco_opcode::v_cmpx_u_f##sz;                                                    \
                              return info;                                                                                 \
                        case aco_opcode::v_cmp_o_f##sz:                                                                 \
                              info.swapped = aco_opcode::v_cmp_o_f##sz;                                                   \
                              info.inverse = aco_opcode::v_cmp_u_f##sz;                                                   \
                              info.vcmpx = aco_opcode::v_cmpx_o_f##sz;                                                    \
                              return info;
                              ORD_TEST(16)
                              ORD_TEST(32)
                              ORD_TEST(64)
                              #undef ORD_TEST
                              #define CMPI2(op, swap, inv, type, sz)                                                             \
                        case aco_opcode::v_cmp_##op##_##type##sz:                                                       \
                              info.swapped = aco_opcode::v_cmp_##swap##_##type##sz;                                       \
                              info.inverse = aco_opcode::v_cmp_##inv##_##type##sz;                                        \
                              info.vcmpx = aco_opcode::v_cmpx_##op##_##type##sz;                                          \
                              return info;
                              #define CMPI(op, swap, inv)                                                                        \
                              CMPI2(op, swap, inv, i, 16)                                                                     \
                              CMPI2(op, swap, inv, u, 16)                                                                     \
                              CMPI2(op, swap, inv, i, 32)                                                                     \
                              CMPI2(op, swap, inv, u, 32)                                                                     \
                              CMPI2(op, swap, inv, i, 64)                                                                     \
                              CMPI2(op, swap, inv, u, 64)
                              CMPI(lt, gt, ge)
                              CMPI(eq, eq, lg)
                              CMPI(le, ge, gt)
                              CMPI(gt, lt, le)
                              CMPI(lg, lg, eq)
                              CMPI(ge, le, lt)
                              #undef CMPI
                              #undef CMPI2
                              #define CMPCLASS(sz)                                                                               \
                        case aco_opcode::v_cmp_class_f##sz:                                                             \
                              info.vcmpx = aco_opcode::v_cmpx_class_f##sz;                                                \
                              return info;
                              CMPCLASS(16)
                              CMPCLASS(32)
                              CMPCLASS(64)
                              #undef CMPCLASS
                        default: return info;
                  }
            }

            static constexpr auto cmp_info_table = []() {
                  std::array<CmpInfo, (size_t)aco_opcode::num_opcodes> table{};
                  for (unsigned i = 0; i < (unsigned)aco_opcode::num_opcodes; ++i) {
                        table[i] = generate_cmp_info((aco_opcode)i);
                  }
                  return table;
            }();

            static_assert(cmp_info_table[(size_t)aco_opcode::v_cmp_lt_f32].swapped == aco_opcode::v_cmp_gt_f32);
            static_assert(cmp_info_table[(size_t)aco_opcode::v_cmp_lt_f32].inverse == aco_opcode::v_cmp_nlt_f32);
            static_assert(cmp_info_table[(size_t)aco_opcode::v_cmp_lt_i32].vcmpx == aco_opcode::v_cmpx_lt_i32);
            static_assert(cmp_info_table[(size_t)aco_opcode::v_add_f32].swapped == aco_opcode::num_opcodes, "Non-compare opcode check");

            static ALWAYS_INLINE bool
            get_cmp_info(aco_opcode op, CmpInfo* info)
            {
                  *info = cmp_info_table[static_cast<size_t>(op)];
                  return info->vcmpx != aco_opcode::num_opcodes || info->inverse != aco_opcode::num_opcodes;
            }

            aco_opcode
            get_vcmp_inverse(aco_opcode op)
            {
                  CmpInfo info;
                  get_cmp_info(op, &info);
                  return info.inverse;
            }

            aco_opcode
            get_vcmp_swapped(aco_opcode op)
            {
                  CmpInfo info;
                  get_cmp_info(op, &info);
                  return info.swapped;
            }

            aco_opcode
            get_vcmpx(aco_opcode op)
            {
                  CmpInfo info;
                  get_cmp_info(op, &info);
                  return info.vcmpx;
            }

            bool
            is_cmpx(aco_opcode op)
            {
                  CmpInfo info;
                  return !get_cmp_info(op, &info);
            }

            static constexpr aco_opcode
            generate_swapped_opcode_map(aco_opcode opcode)
            {
                  switch (opcode) {
                        case aco_opcode::v_add_u32:
                        case aco_opcode::v_add_co_u32:
                        case aco_opcode::v_add_co_u32_e64:
                        case aco_opcode::v_add_i32:
                        case aco_opcode::v_add_i16:
                        case aco_opcode::v_add_u16_e64:
                        case aco_opcode::v_add3_u32:
                        case aco_opcode::v_add_f16:
                        case aco_opcode::v_add_f32:
                        case aco_opcode::v_mul_i32_i24:
                        case aco_opcode::v_mul_hi_i32_i24:
                        case aco_opcode::v_mul_u32_u24:
                        case aco_opcode::v_mul_hi_u32_u24:
                        case aco_opcode::v_mul_lo_u16:
                        case aco_opcode::v_mul_lo_u16_e64:
                        case aco_opcode::v_mul_f16:
                        case aco_opcode::v_mul_f32:
                        case aco_opcode::v_mul_legacy_f32:
                        case aco_opcode::v_or_b32:
                        case aco_opcode::v_and_b32:
                        case aco_opcode::v_xor_b32:
                        case aco_opcode::v_xnor_b32:
                        case aco_opcode::v_xor3_b32:
                        case aco_opcode::v_or3_b32:
                        case aco_opcode::v_and_b16:
                        case aco_opcode::v_or_b16:
                        case aco_opcode::v_xor_b16:
                        case aco_opcode::v_max3_f32:
                        case aco_opcode::v_min3_f32:
                        case aco_opcode::v_max3_f16:
                        case aco_opcode::v_min3_f16:
                        case aco_opcode::v_med3_f16:
                        case aco_opcode::v_max3_u32:
                        case aco_opcode::v_min3_u32:
                        case aco_opcode::v_med3_u32:
                        case aco_opcode::v_max3_i32:
                        case aco_opcode::v_min3_i32:
                        case aco_opcode::v_med3_i32:
                        case aco_opcode::v_max3_u16:
                        case aco_opcode::v_min3_u16:
                        case aco_opcode::v_med3_u16:
                        case aco_opcode::v_max3_i16:
                        case aco_opcode::v_min3_i16:
                        case aco_opcode::v_med3_i16:
                        case aco_opcode::v_max_f16:
                        case aco_opcode::v_max_f32:
                        case aco_opcode::v_min_f16:
                        case aco_opcode::v_min_f32:
                        case aco_opcode::v_max_i32:
                        case aco_opcode::v_min_i32:
                        case aco_opcode::v_max_u32:
                        case aco_opcode::v_min_u32:
                        case aco_opcode::v_max_i16:
                        case aco_opcode::v_min_i16:
                        case aco_opcode::v_max_u16:
                        case aco_opcode::v_min_u16:
                        case aco_opcode::v_max_i16_e64:
                        case aco_opcode::v_min_i16_e64:
                        case aco_opcode::v_max_u16_e64:
                        case aco_opcode::v_min_u16_e64:
                        case aco_opcode::v_addc_co_u32:
                        case aco_opcode::v_mad_i32_i24:
                        case aco_opcode::v_mad_u32_u24:
                        case aco_opcode::v_lerp_u8:
                        case aco_opcode::v_sad_u8:
                        case aco_opcode::v_sad_hi_u8:
                        case aco_opcode::v_sad_u16:
                        case aco_opcode::v_sad_u32:
                        case aco_opcode::v_xad_u32:
                        case aco_opcode::v_add_lshl_u32:
                        case aco_opcode::v_and_or_b32:
                        case aco_opcode::v_mad_u16:
                        case aco_opcode::v_mad_i16:
                        case aco_opcode::v_mad_u32_u16:
                        case aco_opcode::v_mad_i32_i16:
                        case aco_opcode::v_maxmin_f32:
                        case aco_opcode::v_minmax_f32:
                        case aco_opcode::v_maxmin_f16:
                        case aco_opcode::v_minmax_f16:
                        case aco_opcode::v_maxmin_u32:
                        case aco_opcode::v_minmax_i32:
                        case aco_opcode::v_fma_f32:
                        case aco_opcode::v_fma_legacy_f32:
                        case aco_opcode::v_fmac_f32:
                        case aco_opcode::v_fmac_legacy_f32:
                        case aco_opcode::v_mac_f32:
                        case aco_opcode::v_mac_legacy_f32:
                        case aco_opcode::v_fma_f16:
                        case aco_opcode::v_fmac_f16:
                        case aco_opcode::v_mac_f16:
                        case aco_opcode::v_dot4c_i32_i8:
                        case aco_opcode::v_dot2c_f32_f16:
                        case aco_opcode::v_dot2_f32_f16:
                        case aco_opcode::v_dot2_f32_bf16:
                        case aco_opcode::v_dot2_f16_f16:
                        case aco_opcode::v_dot2_bf16_bf16:
                        case aco_opcode::v_fma_mix_f32:
                        case aco_opcode::v_fma_mixlo_f16:
                        case aco_opcode::v_fma_mixhi_f16:
                        case aco_opcode::v_pk_fmac_f16:
                              return opcode;
                        case aco_opcode::v_sub_f16: return aco_opcode::v_subrev_f16;
                        case aco_opcode::v_sub_f32: return aco_opcode::v_subrev_f32;
                        case aco_opcode::v_sub_co_u32: return aco_opcode::v_subrev_co_u32;
                        case aco_opcode::v_sub_u16: return aco_opcode::v_subrev_u16;
                        case aco_opcode::v_sub_u32: return aco_opcode::v_subrev_u32;
                        case aco_opcode::v_sub_co_u32_e64: return aco_opcode::v_subrev_co_u32_e64;
                        case aco_opcode::v_subrev_f16: return aco_opcode::v_sub_f16;
                        case aco_opcode::v_subrev_f32: return aco_opcode::v_sub_f32;
                        case aco_opcode::v_subrev_co_u32: return aco_opcode::v_sub_co_u32;
                        case aco_opcode::v_subrev_u16: return aco_opcode::v_sub_u16;
                        case aco_opcode::v_subrev_u32: return aco_opcode::v_sub_u32;
                        case aco_opcode::v_subrev_co_u32_e64: return aco_opcode::v_sub_co_u32_e64;
                        case aco_opcode::v_subb_co_u32: return aco_opcode::v_subbrev_co_u32;
                        case aco_opcode::v_subbrev_co_u32: return aco_opcode::v_subb_co_u32;
                        default: return aco_opcode::num_opcodes;
                  }
            }

            static constexpr auto swapped_opcode_table = []() {
                  std::array<aco_opcode, (size_t)aco_opcode::num_opcodes> table{};
                  for (unsigned i = 0; i < (unsigned)aco_opcode::num_opcodes; ++i) {
                        table[i] = generate_swapped_opcode_map((aco_opcode)i);
                  }
                  return table;
            }();

            static_assert(swapped_opcode_table[(size_t)aco_opcode::v_sub_f32] == aco_opcode::v_subrev_f32);
            static_assert(swapped_opcode_table[(size_t)aco_opcode::v_add_f32] == aco_opcode::v_add_f32);
            static_assert(swapped_opcode_table[(size_t)aco_opcode::v_med3_f32] == aco_opcode::num_opcodes);

            aco_opcode
            get_swapped_opcode(aco_opcode opcode, unsigned idx0, unsigned idx1)
            {
                  if (idx0 == idx1)
                        return opcode;

                  if (idx0 > idx1)
                        std::swap(idx0, idx1);

                  CmpInfo info;
                  if (get_cmp_info(opcode, &info) && info.swapped != aco_opcode::num_opcodes)
                        return info.swapped;

                  aco_opcode swapped = swapped_opcode_table[static_cast<size_t>(opcode)];
                  if (swapped == aco_opcode::num_opcodes)
                        return swapped;

                  if (idx1 == 2) {
                        switch (opcode) {
                              case aco_opcode::v_addc_co_u32:
                              case aco_opcode::v_subb_co_u32:
                              case aco_opcode::v_subbrev_co_u32:
                              case aco_opcode::v_mad_i32_i24:
                              case aco_opcode::v_mad_u32_u24:
                              case aco_opcode::v_lerp_u8:
                              case aco_opcode::v_sad_u8:
                              case aco_opcode::v_sad_hi_u8:
                              case aco_opcode::v_sad_u16:
                              case aco_opcode::v_sad_u32:
                              case aco_opcode::v_xad_u32:
                              case aco_opcode::v_add_lshl_u32:
                              case aco_opcode::v_and_or_b32:
                              case aco_opcode::v_mad_u16:
                              case aco_opcode::v_mad_i16:
                              case aco_opcode::v_mad_u32_u16:
                              case aco_opcode::v_mad_i32_i16:
                              case aco_opcode::v_maxmin_f32:
                              case aco_opcode::v_minmax_f32:
                              case aco_opcode::v_maxmin_f16:
                              case aco_opcode::v_minmax_f16:
                              case aco_opcode::v_maxmin_u32:
                              case aco_opcode::v_minmax_i32:
                              case aco_opcode::v_fma_f32:
                              case aco_opcode::v_fma_legacy_f32:
                              case aco_opcode::v_fmac_f32:
                              case aco_opcode::v_fmac_legacy_f32:
                              case aco_opcode::v_mac_f32:
                              case aco_opcode::v_mac_legacy_f32:
                              case aco_opcode::v_fma_f16:
                              case aco_opcode::v_fmac_f16:
                              case aco_opcode::v_mac_f16:
                              case aco_opcode::v_dot4c_i32_i8:
                              case aco_opcode::v_dot2c_f32_f16:
                              case aco_opcode::v_dot2_f32_f16:
                              case aco_opcode::v_dot2_f32_bf16:
                              case aco_opcode::v_dot2_f16_f16:
                              case aco_opcode::v_dot2_bf16_bf16:
                              case aco_opcode::v_fma_mix_f32:
                              case aco_opcode::v_fma_mixlo_f16:
                              case aco_opcode::v_fma_mixhi_f16:
                              case aco_opcode::v_pk_fmac_f16:
                                    return aco_opcode::num_opcodes;
                              default:
                                    break;
                        }
                  }
                  return swapped;
            }

            bool
            can_swap_operands(aco_ptr<Instruction>& instr, aco_opcode* new_op, unsigned idx0, unsigned idx1)
            {
                  if (idx0 == idx1) {
                        *new_op = instr->opcode;
                        return true;
                  }

                  if (instr->isDPP())
                        return false;

                  if (!instr->isVOP3() && !instr->isVOP3P() && !instr->operands[0].isOfType(RegType::vgpr))
                        return false;

                  aco_opcode candidate = get_swapped_opcode(instr->opcode, idx0, idx1);
                  if (candidate == aco_opcode::num_opcodes)
                        return false;

                  *new_op = candidate;
                  return true;
            }

            wait_imm::wait_imm()
            : exp(unset_counter), lgkm(unset_counter), vm(unset_counter), vs(unset_counter),
            sample(unset_counter), bvh(unset_counter), km(unset_counter)
            {}
            wait_imm::wait_imm(uint16_t vm_, uint16_t exp_, uint16_t lgkm_, uint16_t vs_)
            : exp(exp_), lgkm(lgkm_), vm(vm_), vs(vs_), sample(unset_counter), bvh(unset_counter),
            km(unset_counter)
            {}

            uint16_t
            wait_imm::pack(enum amd_gfx_level gfx_level) const
            {
                  uint16_t imm = 0;
                  assert(exp == unset_counter || exp <= 0x7);
                  if (gfx_level >= GFX11) {
                        assert(lgkm == unset_counter || lgkm <= 0x3f);
                        assert(vm == unset_counter || vm <= 0x3f);
                        imm = ((vm & 0x3f) << 10) | ((lgkm & 0x3f) << 4) | (exp & 0x7);
                  } else if (gfx_level >= GFX10) {
                        assert(lgkm == unset_counter || lgkm <= 0x3f);
                        assert(vm == unset_counter || vm <= 0x3f);
                        imm = ((vm & 0x30) << 10) | ((lgkm & 0x3f) << 8) | ((exp & 0x7) << 4) | (vm & 0xf);
                  } else if (gfx_level >= GFX9) {
                        assert(lgkm == unset_counter || lgkm <= 0xf);
                        assert(vm == unset_counter || vm <= 0x3f);
                        imm = ((vm & 0x30) << 10) | ((lgkm & 0xf) << 8) | ((exp & 0x7) << 4) | (vm & 0xf);
                  } else {
                        assert(lgkm == unset_counter || lgkm <= 0xf);
                        assert(vm == unset_counter || vm <= 0xf);
                        imm = ((lgkm & 0xf) << 8) | ((exp & 0x7) << 4) | (vm & 0xf);
                  }
                  if (gfx_level < GFX9 && vm == wait_imm::unset_counter)
                        imm |= 0xc000; /* should have no effect on pre-GFX9 and now we won't have to worry about the
                        architecture when interpreting the immediate */
                        if (gfx_level < GFX10 && lgkm == wait_imm::unset_counter)
                              imm |= 0x3000; /* should have no effect on pre-GFX10 and now we won't have to worry about the
                              architecture when interpreting the immediate */
                              return imm;
            }

            wait_imm
            wait_imm::max(enum amd_gfx_level gfx_level)
            {
                  wait_imm imm;
                  imm.vm = gfx_level >= GFX9 ? 63 : 15;
                  imm.exp = 7;
                  imm.lgkm = gfx_level >= GFX10 ? 63 : 15;
                  imm.vs = gfx_level >= GFX10 ? 63 : 0;
                  imm.sample = gfx_level >= GFX12 ? 63 : 0;
                  imm.bvh = gfx_level >= GFX12 ? 7 : 0;
                  imm.km = gfx_level >= GFX12 ? 31 : 0;
                  return imm;
            }

            bool
            wait_imm::unpack(enum amd_gfx_level gfx_level, const Instruction* instr)
            {
                  if (!instr->isSALU() || (!instr->operands.empty() && instr->operands[0].physReg() != sgpr_null))
                        return false;

                  aco_opcode op = instr->opcode;
                  uint16_t packed = instr->salu().imm;

                  if (op == aco_opcode::s_wait_loadcnt) {
                        vm = std::min<uint8_t>(vm, packed);
                  } else if (op == aco_opcode::s_wait_storecnt) {
                        vs = std::min<uint8_t>(vs, packed);
                  } else if (op == aco_opcode::s_wait_samplecnt) {
                        sample = std::min<uint8_t>(sample, packed);
                  } else if (op == aco_opcode::s_wait_bvhcnt) {
                        bvh = std::min<uint8_t>(bvh, packed);
                  } else if (op == aco_opcode::s_wait_expcnt) {
                        exp = std::min<uint8_t>(exp, packed);
                  } else if (op == aco_opcode::s_wait_dscnt) {
                        lgkm = std::min<uint8_t>(lgkm, packed);
                  } else if (op == aco_opcode::s_wait_kmcnt) {
                        km = std::min<uint8_t>(km, packed);
                  } else if (op == aco_opcode::s_wait_loadcnt_dscnt) {
                        uint32_t vm2 = (packed >> 8) & 0x3f;
                        uint32_t ds = packed & 0x3f;
                        vm = std::min<uint8_t>(vm, vm2 == 0x3f ? wait_imm::unset_counter : vm2);
                        lgkm = std::min<uint8_t>(lgkm, ds == 0x3f ? wait_imm::unset_counter : ds);
                  } else if (op == aco_opcode::s_wait_storecnt_dscnt) {
                        uint32_t vs2 = (packed >> 8) & 0x3f;
                        uint32_t ds = packed & 0x3f;
                        vs = std::min<uint8_t>(vs, vs2 == 0x3f ? wait_imm::unset_counter : vs2);
                        lgkm = std::min<uint8_t>(lgkm, ds == 0x3f ? wait_imm::unset_counter : ds);
                  } else if (op == aco_opcode::s_waitcnt_expcnt) {
                        exp = std::min<uint8_t>(exp, packed);
                  } else if (op == aco_opcode::s_waitcnt_lgkmcnt) {
                        lgkm = std::min<uint8_t>(lgkm, packed);
                  } else if (op == aco_opcode::s_waitcnt_vmcnt) {
                        vm = std::min<uint8_t>(vm, packed);
                  } else if (op == aco_opcode::s_waitcnt_vscnt) {
                        vs = std::min<uint8_t>(vs, packed);
                  } else if (op == aco_opcode::s_waitcnt) {
                        uint8_t vm2, lgkm2, exp2;
                        if (gfx_level >= GFX11) {
                              vm2 = (packed >> 10) & 0x3f;
                              lgkm2 = (packed >> 4) & 0x3f;
                              exp2 = packed & 0x7;
                        } else {
                              vm2 = packed & 0xf;
                              if (gfx_level >= GFX9)
                                    vm2 |= (packed >> 10) & 0x30;

                              exp2 = (packed >> 4) & 0x7;

                              lgkm2 = (packed >> 8) & 0xf;
                              if (gfx_level >= GFX10)
                                    lgkm2 |= (packed >> 8) & 0x30;
                        }

                        if (vm2 == (gfx_level >= GFX9 ? 0x3f : 0xf))
                              vm2 = wait_imm::unset_counter;
                        if (exp2 == 0x7)
                              exp2 = wait_imm::unset_counter;
                        if (lgkm2 == (gfx_level >= GFX10 ? 0x3f : 0xf))
                              lgkm2 = wait_imm::unset_counter;

                        vm = std::min(vm, vm2);
                        exp = std::min(exp, exp2);
                        lgkm = std::min(lgkm, lgkm2);
                  } else {
                        return false;
                  }
                  return true;
            }

            bool
            wait_imm::combine(const wait_imm& other)
            {
                  bool changed = false;
                  for (unsigned i = 0; i < wait_type_num; i++) {
                        if (other[i] < (*this)[i])
                              changed = true;
                        (*this)[i] = std::min((*this)[i], other[i]);
                  }
                  return changed;
            }

            bool
            wait_imm::empty() const
            {
                  for (unsigned i = 0; i < wait_type_num; i++) {
                        if ((*this)[i] != unset_counter)
                              return false;
                  }
                  return true;
            }

            void
            wait_imm::print(FILE* output) const
            {
                  const char* names[wait_type_num];
                  names[wait_type_exp] = "exp";
                  names[wait_type_vm] = "vm";
                  names[wait_type_lgkm] = "lgkm";
                  names[wait_type_vs] = "vs";
                  names[wait_type_sample] = "sample";
                  names[wait_type_bvh] = "bvh";
                  names[wait_type_km] = "km";
                  for (unsigned i = 0; i < wait_type_num; i++) {
                        if ((*this)[i] != unset_counter)
                              fprintf(output, "%s: %u\n", names[i], (*this)[i]);
                  }
            }

            void
            wait_imm::build_waitcnt(Builder& bld)
            {
                  enum amd_gfx_level gfx_level = bld.program->gfx_level;

                  if (gfx_level >= GFX12) {
                        if (vm != wait_imm::unset_counter && lgkm != wait_imm::unset_counter) {
                              bld.sopp(aco_opcode::s_wait_loadcnt_dscnt, (vm << 8) | lgkm);
                              vm = wait_imm::unset_counter;
                              lgkm = wait_imm::unset_counter;
                        }

                        if (vs != wait_imm::unset_counter && lgkm != wait_imm::unset_counter) {
                              bld.sopp(aco_opcode::s_wait_storecnt_dscnt, (vs << 8) | lgkm);
                              vs = wait_imm::unset_counter;
                              lgkm = wait_imm::unset_counter;
                        }

                        aco_opcode op[wait_type_num];
                        op[wait_type_exp] = aco_opcode::s_wait_expcnt;
                        op[wait_type_lgkm] = aco_opcode::s_wait_dscnt;
                        op[wait_type_vm] = aco_opcode::s_wait_loadcnt;
                        op[wait_type_vs] = aco_opcode::s_wait_storecnt;
                        op[wait_type_sample] = aco_opcode::s_wait_samplecnt;
                        op[wait_type_bvh] = aco_opcode::s_wait_bvhcnt;
                        op[wait_type_km] = aco_opcode::s_wait_kmcnt;

                        for (unsigned i = 0; i < wait_type_num; i++) {
                              if ((*this)[i] != wait_imm::unset_counter)
                                    bld.sopp(op[i], (*this)[i]);
                        }
                  } else {
                        if (vs != wait_imm::unset_counter) {
                              assert(gfx_level >= GFX10);
                              bld.sopk(aco_opcode::s_waitcnt_vscnt, Operand(sgpr_null, s1), vs);
                              vs = wait_imm::unset_counter;
                        }
                        if (!empty())
                              bld.sopp(aco_opcode::s_waitcnt, pack(gfx_level));
                  }

                  *this = wait_imm();
            }

            bool
            should_form_clause(const Instruction* a, const Instruction* b)
            {
                  if (a->definitions.empty() != b->definitions.empty())
                        return false;

                  /* MUBUF and MTBUF can appear in the same clause. */
                  if ((a->isMTBUF() && b->isMUBUF()) || (a->isMUBUF() && b->isMTBUF())) {
                  } else if (a->format != b->format) {
                        return false;
                  }

                  if (a->operands.empty() || b->operands.empty())
                        return false;

                  /* Assume loads which don't use descriptors might load from similar addresses. */
                  if (a->isFlatLike() || a->accessesLDS())
                        return true;
                  if (a->isSMEM() && a->operands[0].bytes() == 8 && b->operands[0].bytes() == 8)
                        return true;

                  /* If they load from the same descriptor, assume they might load from similar
                   * addresses.
                   */
                  if (a->isVMEM() || a->isSMEM())
                        return a->operands[0].tempId() == b->operands[0].tempId();

                  if (a->isEXP() && b->isEXP())
                        return true;

                  return false;
            }

            aco::small_vec<uint32_t, 2>
            get_tied_defs(Instruction* instr)
            {
                  aco::small_vec<uint32_t, 2> ops;
                  if (instr->opcode == aco_opcode::v_interp_p2_f32 || instr->opcode == aco_opcode::v_mac_f32 ||
                        instr->opcode == aco_opcode::v_fmac_f32 || instr->opcode == aco_opcode::v_mac_f16 ||
                        instr->opcode == aco_opcode::v_fmac_f16 || instr->opcode == aco_opcode::v_mac_legacy_f32 ||
                        instr->opcode == aco_opcode::v_fmac_legacy_f32 ||
                        instr->opcode == aco_opcode::v_pk_fmac_f16 || instr->opcode == aco_opcode::v_writelane_b32 ||
                        instr->opcode == aco_opcode::v_writelane_b32_e64 ||
                        instr->opcode == aco_opcode::v_dot4c_i32_i8 || instr->opcode == aco_opcode::s_fmac_f32 ||
                        instr->opcode == aco_opcode::s_fmac_f16) {
                        ops.push_back(2);
                        } else if (instr->opcode == aco_opcode::s_addk_i32 || instr->opcode == aco_opcode::s_mulk_i32 ||
                              instr->opcode == aco_opcode::s_cmovk_i32) {
                              /* These SOPK instructions have an implicit source operand which is the same as the destination. */
                              ops.push_back(0);
                              } else if (instr->isMUBUF() && instr->definitions.size() == 1 && instr->operands.size() == 4) {
                                    /* MUBUF atomic instructions with a return value have the data source/destination tied. */
                                    ops.push_back(3);
                              } else if (instr->isMIMG() && instr->definitions.size() == 1 &&
                                    !instr->operands[2].isUndefined()) {
                                    /* MIMG atomic instructions with a return value have the data source/destination tied. */
                                    ops.push_back(2);
                                    } else if (instr->opcode == aco_opcode::image_bvh8_intersect_ray) {
                                          /* VADDR for this RT instruction has tied operands */
                                          ops.push_back(3 + 4);
                                          ops.push_back(3 + 7);
                                    }
                                    return ops;
            }

            uint8_t
            get_vmem_type(amd_gfx_level gfx_level, radeon_family family, Instruction* instr)
            {
                  if (instr->opcode == aco_opcode::image_bvh_intersect_ray ||
                        instr->opcode == aco_opcode::image_bvh64_intersect_ray ||
                        instr->opcode == aco_opcode::image_bvh_dual_intersect_ray ||
                        instr->opcode == aco_opcode::image_bvh8_intersect_ray) {
                        return vmem_bvh;
                        } else if (instr->opcode == aco_opcode::image_msaa_load) {
                              return vmem_sampler;
                        } else if (instr->isMIMG() && !instr->operands[1].isUndefined() &&
                              instr->operands[1].regClass() == s4) {
                              bool point_sample_accel = gfx_level == GFX11_5 && family != CHIP_GFX1153 &&
                              (instr->opcode == aco_opcode::image_sample ||
                              instr->opcode == aco_opcode::image_sample_l ||
                              instr->opcode == aco_opcode::image_sample_lz);
                        return vmem_sampler | (point_sample_accel ? vmem_nosampler : 0);
                              } else if (instr->isVMEM() || instr->isScratch() || instr->isGlobal()) {
                                    return vmem_nosampler;
                              }
                              return 0;
            }

            /* Parse implicit data dependency resolution:
             * Returns the value of each counter that must be reached
             * before an instruction is issued.
             *
             * (Probably incomplete.)
             */
            depctr_wait
            parse_depctr_wait(const Instruction* instr)
            {
                  depctr_wait res;
                  if (instr->isVMEM() || instr->isFlatLike() || instr->isDS() || instr->isEXP()) {
                        res.va_vdst = 0;
                        res.va_exec = 0;
                        res.sa_exec = 0;
                        if (instr->isVMEM() || instr->isFlatLike()) {
                              res.sa_sdst = 0;
                              res.va_sdst = 0;
                              res.va_vcc = 0;
                        }
                  } else if (instr->isSMEM()) {
                        res.sa_sdst = 0;
                        res.va_sdst = 0;
                        res.va_vcc = 0;
                  } else if (instr->isLDSDIR()) {
                        res.va_vdst = instr->ldsdir().wait_vdst;
                        res.va_exec = 0;
                        res.sa_exec = 0;
                  } else if (instr->opcode == aco_opcode::s_waitcnt_depctr) {
                        unsigned imm = instr->salu().imm;
                        res.va_vdst = (imm >> 12) & 0xf;
                        res.va_sdst = (imm >> 9) & 0x7;
                        res.va_ssrc = (imm >> 8) & 0x1;
                        res.hold_cnt = (imm >> 7) & 0x1;
                        res.vm_vsrc = (imm >> 2) & 0x7;
                        res.va_vcc = (imm >> 1) & 0x1;
                        res.sa_sdst = imm & 0x1;
                  } else if (instr->isVALU()) {
                        res.sa_exec = 0;
                        for (const Definition& def : instr->definitions) {
                              if (def.regClass().type() == RegType::sgpr) {
                                    res.sa_sdst = 0;
                                    /* Notably, this is the only exception, even VALU that
                                     * reads exec doesn't implicitly wait for va_exec.
                                     */
                                    if (instr->opcode == aco_opcode::v_readfirstlane_b32)
                                          res.va_exec = 0;
                                    break;
                              }
                        }
                  } else if (instr_info.classes[(int)instr->opcode] == instr_class::branch ||
                        instr_info.classes[(int)instr->opcode] == instr_class::sendmsg) {
                        res.sa_exec = 0;
                  res.va_exec = 0;
                  switch (instr->opcode) {
                        case aco_opcode::s_cbranch_vccz:
                        case aco_opcode::s_cbranch_vccnz:
                              res.va_vcc = 0;
                              res.sa_sdst = 0;
                              break;
                        case aco_opcode::s_cbranch_scc0:
                        case aco_opcode::s_cbranch_scc1:
                              res.sa_sdst = 0;
                              break;
                        default: break;
                  }
                        } else if (instr->isSALU()) {
                              for (const Definition& def : instr->definitions) {
                                    if (def.physReg() < vcc) {
                                          res.va_sdst = 0;
                                    } else if (def.physReg() <= vcc_hi) {
                                          res.va_vcc = 0;
                                    } else if (def.physReg() == exec || def.physReg() == exec_hi) {
                                          res.va_exec = 0;
                                    }
                              }
                              for (const Operand& op : instr->operands) {
                                    if (op.physReg() < vcc) {
                                          res.va_sdst = 0;
                                    } else if (op.physReg() <= vcc_hi) {
                                          res.va_vcc = 0;
                                    } else if (op.physReg() == exec || op.physReg() == exec_hi) {
                                          res.va_exec = 0;
                                    }
                              }
                        }

                        return res;
            }

            bool
            dealloc_vgprs(Program* program)
            {
                  if (program->gfx_level < GFX11)
                        return false;

                  /* If we insert the sendmsg on GFX11.5, the export priority workaround will require us to insert
                   * a wait after exports. There might still be pending VMEM stores for PS parameter exports,
                   * except NGG lowering usually inserts a memory barrier. This means there is unlikely to be any
                   * pending VMEM stores or exports if we insert the sendmsg for these stages. */
                  if (program->gfx_level == GFX11_5 && (program->stage.hw == AC_HW_NEXT_GEN_GEOMETRY_SHADER ||
                        program->stage.hw == AC_HW_PIXEL_SHADER))
                        return false;

                  Block& block = program->blocks.back();

                  /* don't bother checking if there is a pending VMEM store or export: there almost always is */
                  Builder bld(program);
                  if (!block.instructions.empty() && block.instructions.back()->opcode == aco_opcode::s_endpgm) {
                        bld.reset(&block.instructions, block.instructions.begin() + (block.instructions.size() - 1));
                        bld.sopp(aco_opcode::s_sendmsg, sendmsg_dealloc_vgprs);
                  }

                  return true;
            }

            bool
            Instruction::isTrans() const noexcept
            {
                  return instr_info.classes[(int)opcode] == instr_class::valu_transcendental32 ||
                  instr_info.classes[(int)opcode] == instr_class::valu_double_transcendental ||
                  instr_info.classes[(int)opcode] == instr_class::valu_pseudo_scalar_trans;
            }

            static constexpr auto instr_data_size_table = []() {
                  std::array<size_t, 256> table{};
                  table[static_cast<size_t>(Format::SOP1)] = sizeof(SALU_instruction);
                  table[static_cast<size_t>(Format::SOP2)] = sizeof(SALU_instruction);
                  table[static_cast<size_t>(Format::SOPC)] = sizeof(SALU_instruction);
                  table[static_cast<size_t>(Format::SOPK)] = sizeof(SALU_instruction);
                  table[static_cast<size_t>(Format::SOPP)] = sizeof(SALU_instruction);
                  table[static_cast<size_t>(Format::SMEM)] = sizeof(SMEM_instruction);
                  table[static_cast<size_t>(Format::PSEUDO)] = sizeof(Pseudo_instruction);
                  table[static_cast<size_t>(Format::PSEUDO_BARRIER)] = sizeof(Pseudo_barrier_instruction);
                  table[static_cast<size_t>(Format::PSEUDO_REDUCTION)] = sizeof(Pseudo_reduction_instruction);
                  table[static_cast<size_t>(Format::PSEUDO_BRANCH)] = sizeof(Pseudo_branch_instruction);
                  table[static_cast<size_t>(Format::DS)] = sizeof(DS_instruction);
                  table[static_cast<size_t>(Format::FLAT)] = sizeof(FLAT_instruction);
                  table[static_cast<size_t>(Format::GLOBAL)] = sizeof(FLAT_instruction);
                  table[static_cast<size_t>(Format::SCRATCH)] = sizeof(FLAT_instruction);
                  table[static_cast<size_t>(Format::LDSDIR)] = sizeof(LDSDIR_instruction);
                  table[static_cast<size_t>(Format::MTBUF)] = sizeof(MTBUF_instruction);
                  table[static_cast<size_t>(Format::MUBUF)] = sizeof(MUBUF_instruction);
                  table[static_cast<size_t>(Format::MIMG)] = sizeof(MIMG_instruction);
                  table[static_cast<size_t>(Format::VOPD)] = sizeof(VOPD_instruction);
                  table[static_cast<size_t>(Format::VINTERP_INREG)] = sizeof(VINTERP_inreg_instruction);
                  table[static_cast<size_t>(Format::VINTRP)] = sizeof(VINTRP_instruction);
                  table[static_cast<size_t>(Format::EXP)] = sizeof(Export_instruction);
                  return table;
            }();

            size_t
            get_instr_data_size(Format format)
            {
                  if ((uint16_t)format & (uint16_t)Format::DPP16)
                        return sizeof(DPP16_instruction);
                  else if ((uint16_t)format & (uint16_t)Format::DPP8)
                        return sizeof(DPP8_instruction);
                  else if ((uint16_t)format & (uint16_t)Format::SDWA)
                        return sizeof(SDWA_instruction);

                  size_t size = instr_data_size_table[static_cast<uint16_t>(format) & 0xFF];
                  if (size > 0) [[likely]]
                        return size;
                  else
                        return sizeof(VALU_instruction);
            }

            Temp
            load_scratch_resource(Program* program, Builder& bld, unsigned resume_idx,
                                  bool apply_scratch_offset)
            {
                  if (program->static_scratch_rsrc != Temp()) {
                        /* We can't apply any offsets when using a static resource. */
                        assert(!apply_scratch_offset || program->scratch_offsets.empty());
                        return program->static_scratch_rsrc;
                  }
                  Temp private_segment_buffer;
                  if (!program->private_segment_buffers.empty())
                        private_segment_buffer = program->private_segment_buffers[resume_idx];
                  if (!private_segment_buffer.bytes()) {
                        Temp addr_lo =
                        bld.sop1(aco_opcode::p_load_symbol, bld.def(s1), Operand::c32(aco_symbol_scratch_addr_lo));
                        Temp addr_hi =
                        bld.sop1(aco_opcode::p_load_symbol, bld.def(s1), Operand::c32(aco_symbol_scratch_addr_hi));
                        private_segment_buffer =
                        bld.pseudo(aco_opcode::p_create_vector, bld.def(s2), addr_lo, addr_hi);
                  } else if (program->stage.hw != AC_HW_COMPUTE_SHADER) {
                        private_segment_buffer =
                        bld.smem(aco_opcode::s_load_dwordx2, bld.def(s2), private_segment_buffer, Operand::zero());
                  }

                  if (apply_scratch_offset && !program->scratch_offsets.empty()) {
                        Temp addr_lo = bld.tmp(s1);
                        Temp addr_hi = bld.tmp(s1);
                        bld.pseudo(aco_opcode::p_split_vector, Definition(addr_lo), Definition(addr_hi),
                                   private_segment_buffer);

                        Temp carry = bld.tmp(s1);
                        Temp scratch_offset = program->scratch_offsets[resume_idx];
                        addr_lo = bld.sop2(aco_opcode::s_add_u32, bld.def(s1), bld.scc(Definition(carry)), addr_lo,
                                           scratch_offset);
                        addr_hi = bld.sop2(aco_opcode::s_addc_u32, bld.def(s1), bld.def(s1, scc), addr_hi,
                                           Operand::c32(0), bld.scc(carry));

                        private_segment_buffer =
                        bld.pseudo(aco_opcode::p_create_vector, bld.def(s2), addr_lo, addr_hi);
                  }

                  struct ac_buffer_state ac_state = {0};
                  uint32_t desc[4];

                  ac_state.size = 0xffffffff;
                  ac_state.format = PIPE_FORMAT_R32_FLOAT;
                  for (int i = 0; i < 4; i++)
                        ac_state.swizzle[i] = PIPE_SWIZZLE_0;
                  /* older generations need element size = 4 bytes. element size removed in GFX9 */
                  ac_state.element_size = program->gfx_level <= GFX8 ? 1u : 0u;
                  ac_state.index_stride = program->wave_size == 64 ? 3u : 2u;
                  ac_state.add_tid = true;
                  ac_state.gfx10_oob_select = V_008F0C_OOB_SELECT_RAW;

                  ac_build_buffer_descriptor(program->gfx_level, &ac_state, desc);

                  return bld.pseudo(aco_opcode::p_create_vector, bld.def(s4), private_segment_buffer,
                                    Operand::c32(desc[2]), Operand::c32(desc[3]));
            }

            Instruction*
            create_instruction(aco_opcode opcode, Format format, uint32_t num_operands,
                               uint32_t num_definitions)
            {
                  size_t size = get_instr_data_size(format);
                  size_t total_size = size + num_operands * sizeof(Operand) + num_definitions * sizeof(Definition);

                  void* data = instruction_buffer->allocate(total_size, alignof(uint32_t));
                  memset(data, 0, total_size);
                  Instruction* inst = (Instruction*)data;

                  inst->opcode = opcode;
                  inst->format = format;

                  uint16_t operands_offset = size - offsetof(Instruction, operands);
                  inst->operands = aco::span<Operand>(operands_offset, num_operands);
                  uint16_t definitions_offset = (char*)inst->operands.end() - (char*)&inst->definitions;
                  inst->definitions = aco::span<Definition>(definitions_offset, num_definitions);

                  return inst;
            }

} // namespace aco
