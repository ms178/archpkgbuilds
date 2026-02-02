/*
 * Copyright © 2018 Valve Corporation
 *
 * SPDX-License-Identifier: MIT
 */

#include "aco_builder.h"
#include "aco_ir.h"

#include "common/sid.h"

#include "util/memstream.h"

#include "ac_shader_util.h"
#include <algorithm>
#include <cstdint>
#include <unordered_map>
#include <vector>

namespace aco {

struct constaddr_info {
   unsigned getpc_end;
   unsigned add_literal;
};

struct branch_info {
   unsigned pos;
   unsigned target;
};

struct asm_context {
   Program* program;
   enum amd_gfx_level gfx_level;
   std::vector<branch_info> branches;
   std::unordered_map<unsigned, constaddr_info> constaddrs;
   std::unordered_map<unsigned, constaddr_info> resumeaddrs;
   std::vector<struct aco_symbol>* symbols;
   uint32_t loop_header = UINT32_MAX;
   uint32_t loop_exit = UINT32_MAX;
   const int16_t* opcode;
   int subvector_begin_pos = -1;

   asm_context(Program* program_, std::vector<struct aco_symbol>* symbols_)
       : program(program_), gfx_level(program_->gfx_level), symbols(symbols_)
   {
      /* Pre-size hash maps to avoid rehashing during assembly.
       * Values chosen based on typical shader complexity. */
      constaddrs.reserve(256);
      resumeaddrs.reserve(32);
      branches.reserve(512);

      /* Select opcode table based on GFX level.
       * GFX9 (Vega) uses opcode_gfx9. */
      if (gfx_level <= GFX7)
         opcode = &instr_info.opcode_gfx7[0];
      else if (gfx_level <= GFX9)
         opcode = &instr_info.opcode_gfx9[0];
      else if (gfx_level <= GFX10_3)
         opcode = &instr_info.opcode_gfx10[0];
      else if (gfx_level <= GFX11_5)
         opcode = &instr_info.opcode_gfx11[0];
      else
         opcode = &instr_info.opcode_gfx12[0];
   }
};

unsigned
get_mimg_nsa_dwords(const Instruction* instr)
{
   /* MIMG instructions require at least 4 operands:
    * [0] = resource descriptor
    * [1] = sampler descriptor
    * [2] = vdata
    * [3+] = address components
    * Guard against malformed instructions. */
   if (instr->operands.size() < 4) [[unlikely]]
      return 0;

   unsigned addr_dwords = instr->operands.size() - 3;
   for (unsigned i = 3; i < instr->operands.size(); i++) {
      if (instr->operands[i].isVectorAligned())
         addr_dwords--;
   }

   /* Check if address operands are contiguous in register file */
   for (unsigned i = 4; i < instr->operands.size(); i++) {
      const PhysReg expected = instr->operands[i - 1].physReg().advance(
         instr->operands[i - 1].bytes());
      if (instr->operands[i].physReg() != expected)
         return DIV_ROUND_UP(addr_dwords - 1, 4);
   }
   return 0;
}

unsigned
get_vopd_opy_start(const Instruction* instr)
{
   switch (instr->opcode) {
   case aco_opcode::v_dual_fmac_f32:
   case aco_opcode::v_dual_fmaak_f32:
   case aco_opcode::v_dual_fmamk_f32:
   case aco_opcode::v_dual_cndmask_b32:
   case aco_opcode::v_dual_dot2acc_f32_f16:
   case aco_opcode::v_dual_dot2acc_f32_bf16:
      return 3;
   case aco_opcode::v_dual_mov_b32:
      return 1;
   default:
      return 2;
   }
}

/**
 * Encode a physical register for instruction encoding.
 * Handles GFX11+ m0/sgpr_null register swap.
 */
uint32_t
reg(asm_context& ctx, PhysReg r)
{
   /* GFX11+ swaps m0 and sgpr_null encoding positions.
    * For GFX9 (Vega 64), this branch is never taken. */
   if (ctx.gfx_level >= GFX11) [[unlikely]] {
      if (r == m0) [[unlikely]]
         return sgpr_null.reg();
      else if (r == sgpr_null) [[unlikely]]
         return m0.reg();
   }
   return r.reg();
}

/**
 * Encode operand register with optional width masking.
 * width=8 for VGPR/SGPR fields, width=32 for full register.
 */
ALWAYS_INLINE uint32_t
reg(asm_context& ctx, Operand op, unsigned width = 32)
{
   return reg(ctx, op.physReg()) & BITFIELD_MASK(width);
}

/**
 * Encode definition register with optional width masking.
 */
ALWAYS_INLINE uint32_t
reg(asm_context& ctx, Definition def, unsigned width = 32)
{
   return reg(ctx, def.physReg()) & BITFIELD_MASK(width);
}

bool
needs_vop3_gfx11(asm_context& ctx, Instruction* instr)
{
   /* Only applies to GFX11+ true16 mode.
    * Returns false immediately for GFX9. */
   if (ctx.gfx_level <= GFX10_3)
      return false;

   uint8_t mask = get_gfx11_true16_mask(instr->opcode);
   if (!mask)
      return false;

   u_foreach_bit (i, mask & 0x3) {
      if (instr->operands[i].physReg().reg() >= (256 + 128))
         return true;
   }
   if ((mask & 0x8) && instr->definitions[0].physReg().reg() >= (256 + 128))
      return true;
   return false;
}

template <typename T>
uint32_t
get_gfx12_cpol(const T& instr)
{
   uint32_t scope = instr.cache.gfx12.scope;
   uint32_t th = instr.cache.gfx12.temporal_hint;
   return scope | (th << 2);
}

void
emit_sop2_instruction(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[static_cast<int>(instr->opcode)];

   uint32_t encoding = (0b10u << 30);
   encoding |= opcode << 23;
   encoding |= !instr->definitions.empty() ? reg(ctx, instr->definitions[0]) << 16 : 0;
   encoding |= instr->operands.size() >= 2 ? reg(ctx, instr->operands[1]) << 8 : 0;
   encoding |= !instr->operands.empty() ? reg(ctx, instr->operands[0]) : 0;
   out.push_back(encoding);
}

void
emit_sopk_instruction(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[static_cast<int>(instr->opcode)];
   const SALU_instruction& sopk = instr->salu();
   assert(sopk.imm <= UINT16_MAX);
   uint16_t imm = static_cast<uint16_t>(sopk.imm);

   if (instr->opcode == aco_opcode::s_subvector_loop_begin) {
      assert(ctx.gfx_level >= GFX10);
      assert(ctx.subvector_begin_pos == -1);
      ctx.subvector_begin_pos = static_cast<int>(out.size());
   } else if (instr->opcode == aco_opcode::s_subvector_loop_end) {
      assert(ctx.gfx_level >= GFX10);
      assert(ctx.subvector_begin_pos != -1);
      out[static_cast<size_t>(ctx.subvector_begin_pos)] |=
         static_cast<uint32_t>(out.size()) - static_cast<uint32_t>(ctx.subvector_begin_pos);
      imm = static_cast<uint16_t>(ctx.subvector_begin_pos - static_cast<int>(out.size()));
      ctx.subvector_begin_pos = -1;
   }

   uint32_t encoding = (0b1011u << 28);
   encoding |= opcode << 23;
   if (!instr->definitions.empty() && instr->definitions[0].physReg() != scc) {
      encoding |= reg(ctx, instr->definitions[0]) << 16;
   } else if (!instr->operands.empty() && instr->operands[0].physReg() <= 127) {
      encoding |= reg(ctx, instr->operands[0]) << 16;
   }
   encoding |= imm;
   out.push_back(encoding);
}

void
emit_sop1_instruction(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[static_cast<int>(instr->opcode)];

   uint32_t encoding = (0b101111101u << 23);
   encoding |= !instr->definitions.empty() ? reg(ctx, instr->definitions[0]) << 16 : 0;
   encoding |= opcode << 8;
   encoding |= !instr->operands.empty() ? reg(ctx, instr->operands[0]) : 0;
   out.push_back(encoding);
}

void
emit_sopc_instruction(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[static_cast<int>(instr->opcode)];

   uint32_t encoding = (0b101111110u << 23);
   encoding |= opcode << 16;
   encoding |= instr->operands.size() == 2 ? reg(ctx, instr->operands[1]) << 8 : 0;
   encoding |= !instr->operands.empty() ? reg(ctx, instr->operands[0]) : 0;
   out.push_back(encoding);
}

void
emit_sopp_instruction(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr,
                      bool force_imm = false)
{
   const uint32_t opcode = ctx.opcode[static_cast<int>(instr->opcode)];
   const SALU_instruction& sopp = instr->salu();

   uint32_t encoding = (0b101111111u << 23);
   encoding |= opcode << 16;

   if (!force_imm && instr_info.classes[static_cast<int>(instr->opcode)] == instr_class::branch) {
      ctx.branches.push_back({static_cast<unsigned>(out.size()), sopp.imm});
   } else {
      assert(sopp.imm <= UINT16_MAX);
      encoding |= static_cast<uint16_t>(sopp.imm);
   }
   out.push_back(encoding);
}

void
emit_smem_instruction(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[static_cast<int>(instr->opcode)];
   const SMEM_instruction& smem = instr->smem();
   const bool glc = smem.cache.value & ac_glc;
   const bool dlc = smem.cache.value & ac_dlc;
   const enum amd_gfx_level gfx = ctx.gfx_level;

   const bool soe = instr->operands.size() >= (!instr->definitions.empty() ? 3u : 4u);
   const bool is_load = !instr->definitions.empty();
   uint32_t encoding = 0;

   /* GFX6/GFX7 use different encoding format */
   if (gfx <= GFX7) [[unlikely]] {
      encoding = (0b11000u << 27);
      encoding |= opcode << 22;
      encoding |= instr->definitions.size() ? reg(ctx, instr->definitions[0]) << 15 : 0;
      encoding |= instr->operands.size() ? (reg(ctx, instr->operands[0]) >> 1) << 9 : 0;
      if (instr->operands.size() >= 2) {
         if (!instr->operands[1].isConstant()) {
            encoding |= reg(ctx, instr->operands[1]);
         } else if (instr->operands[1].constantValue() >= 1024) {
            encoding |= 255;
         } else {
            encoding |= instr->operands[1].constantValue() >> 2;
            encoding |= 1u << 8;
         }
      }
      out.push_back(encoding);
      if (instr->operands.size() >= 2 && instr->operands[1].isConstant() &&
          instr->operands[1].constantValue() >= 1024)
         out.push_back(instr->operands[1].constantValue() >> 2);
      return;
   }

   /* GFX8/GFX9 encoding - this is the hot path for Vega 64 */
   if (gfx <= GFX9) [[likely]] {
      encoding = (0b110000u << 26);
      assert(!dlc); /* DLC not supported on GFX8/9 */
   } else {
      encoding = (0b111101u << 26);
      if (gfx <= GFX11_5)
         encoding |= dlc ? 1u << (gfx >= GFX11 ? 13 : 14) : 0;
   }

   if (gfx <= GFX11_5) [[likely]] {
      encoding |= opcode << 18;
      encoding |= glc ? 1u << (gfx >= GFX11 ? 14 : 16) : 0;
   } else {
      encoding |= opcode << 13;
      encoding |= get_gfx12_cpol(smem) << 21;
   }

   if (gfx <= GFX9) [[likely]] {
      if (instr->operands.size() >= 2)
         encoding |= instr->operands[1].isConstant() ? 1u << 17 : 0;
   }
   if (gfx == GFX9) [[likely]] {
      encoding |= soe ? 1u << 14 : 0;
   }

   if (is_load || instr->operands.size() >= 3) {
      encoding |= (is_load ? reg(ctx, instr->definitions[0]) : reg(ctx, instr->operands[2])) << 6;
   }
   if (instr->operands.size() >= 1) {
      encoding |= reg(ctx, instr->operands[0]) >> 1;
   }

   out.push_back(encoding);
   encoding = 0;

   int32_t offset = 0;
   uint32_t soffset = gfx >= GFX10 ? reg(ctx, sgpr_null) : 0;

   if (instr->operands.size() >= 2) {
      const Operand& op_off1 = instr->operands[1];
      if (gfx <= GFX9) [[likely]] {
         offset = op_off1.isConstant() ? static_cast<int32_t>(op_off1.constantValue())
                                       : static_cast<int32_t>(reg(ctx, op_off1));
      } else {
         if (op_off1.isConstant()) {
            offset = static_cast<int32_t>(op_off1.constantValue());
         } else {
            soffset = reg(ctx, op_off1);
            assert(!soe);
         }
      }

      if (soe) {
         const Operand& op_off2 = instr->operands.back();
         assert(gfx >= GFX9);
         assert(!op_off2.isConstant());
         soffset = reg(ctx, op_off2);
      }
   }
   encoding |= static_cast<uint32_t>(offset);
   encoding |= soffset << 25;

   out.push_back(encoding);
}

void
emit_vop2_instruction(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[static_cast<int>(instr->opcode)];
   const VALU_instruction& valu = instr->valu();

   uint32_t encoding = 0;
   encoding |= opcode << 25;
   encoding |= reg(ctx, instr->definitions[0], 8) << 17;
   encoding |= (valu.opsel[3] ? 128u : 0u) << 17;
   encoding |= reg(ctx, instr->operands[1], 8) << 9;
   encoding |= (valu.opsel[1] ? 128u : 0u) << 9;
   encoding |= reg(ctx, instr->operands[0]);
   encoding |= valu.opsel[0] ? 128u : 0u;
   out.push_back(encoding);
}

void
emit_vop1_instruction(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[static_cast<int>(instr->opcode)];
   const VALU_instruction& valu = instr->valu();

   uint32_t encoding = (0b0111111u << 25);
   if (!instr->definitions.empty()) {
      encoding |= reg(ctx, instr->definitions[0], 8) << 17;
      encoding |= (valu.opsel[3] ? 128u : 0u) << 17;
   }
   encoding |= opcode << 9;
   if (!instr->operands.empty()) {
      encoding |= reg(ctx, instr->operands[0]);
      encoding |= valu.opsel[0] ? 128u : 0u;
   }
   out.push_back(encoding);
}

void
emit_vopc_instruction(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[static_cast<int>(instr->opcode)];
   const VALU_instruction& valu = instr->valu();

   uint32_t encoding = (0b0111110u << 25);
   encoding |= opcode << 17;
   encoding |= reg(ctx, instr->operands[1], 8) << 9;
   encoding |= (valu.opsel[1] ? 128u : 0u) << 9;
   encoding |= reg(ctx, instr->operands[0]);
   encoding |= valu.opsel[0] ? 128u : 0u;
   out.push_back(encoding);
}

void
emit_vintrp_instruction(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[static_cast<int>(instr->opcode)];
   const VINTRP_instruction& interp = instr->vintrp();
   const enum amd_gfx_level gfx = ctx.gfx_level;

   uint32_t encoding = 0;
   if (instr->opcode == aco_opcode::v_interp_p1ll_f16 ||
       instr->opcode == aco_opcode::v_interp_p1lv_f16 ||
       instr->opcode == aco_opcode::v_interp_p2_legacy_f16 ||
       instr->opcode == aco_opcode::v_interp_p2_f16 ||
       instr->opcode == aco_opcode::v_interp_p2_hi_f16) {
      if (gfx == GFX8 || gfx == GFX9) [[likely]] {
         encoding = (0b110100u << 26);
      } else if (gfx >= GFX10) [[unlikely]] {
         encoding = (0b110101u << 26);
      } else {
         UNREACHABLE("Unknown gfx_level.");
      }

      unsigned opsel = instr->opcode == aco_opcode::v_interp_p2_hi_f16 ? 0x8u : 0u;

      encoding |= opcode << 16;
      encoding |= opsel << 11;
      encoding |= reg(ctx, instr->definitions[0], 8);
      out.push_back(encoding);

      encoding = 0;
      encoding |= interp.attribute;
      encoding |= static_cast<uint32_t>(interp.component) << 6;
      encoding |= interp.high_16bits ? 1u << 8 : 0u;
      encoding |= reg(ctx, instr->operands[0]) << 9;
      if (instr->opcode == aco_opcode::v_interp_p2_f16 ||
          instr->opcode == aco_opcode::v_interp_p2_hi_f16 ||
          instr->opcode == aco_opcode::v_interp_p2_legacy_f16 ||
          instr->opcode == aco_opcode::v_interp_p1lv_f16) {
         encoding |= reg(ctx, instr->operands[2]) << 18;
      }
      out.push_back(encoding);
   } else {
      if (gfx == GFX8 || gfx == GFX9) [[likely]] {
         encoding = (0b110101u << 26);
      } else {
         encoding = (0b110010u << 26);
      }

      assert(encoding);
      encoding |= reg(ctx, instr->definitions[0], 8) << 18;
      encoding |= opcode << 16;
      encoding |= static_cast<uint32_t>(interp.attribute) << 10;
      encoding |= static_cast<uint32_t>(interp.component) << 8;
      if (instr->opcode == aco_opcode::v_interp_mov_f32)
         encoding |= (0x3u & instr->operands[0].constantValue());
      else
         encoding |= reg(ctx, instr->operands[0], 8);
      out.push_back(encoding);
   }
}

void
emit_vinterp_inreg_instruction(asm_context& ctx, std::vector<uint32_t>& out,
                               const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[static_cast<int>(instr->opcode)];
   const VINTERP_inreg_instruction& interp = instr->vinterp_inreg();

   uint32_t encoding = (0b11001101u << 24);
   encoding |= reg(ctx, instr->definitions[0], 8);
   encoding |= static_cast<uint32_t>(interp.wait_exp) << 8;
   encoding |= static_cast<uint32_t>(interp.opsel) << 11;
   encoding |= static_cast<uint32_t>(interp.clamp) << 15;
   encoding |= opcode << 16;
   out.push_back(encoding);

   encoding = 0;
   for (unsigned i = 0; i < instr->operands.size(); i++)
      encoding |= reg(ctx, instr->operands[i]) << (i * 9);
   for (unsigned i = 0; i < 3; i++)
      encoding |= interp.neg[i] ? 1u << (29 + i) : 0u;
   out.push_back(encoding);
}

void
emit_vopd_instruction(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[static_cast<int>(instr->opcode)];
   const VOPD_instruction& vopd = instr->vopd();

   uint32_t encoding = (0b110010u << 26);
   encoding |= reg(ctx, instr->operands[0]);
   if (instr->opcode != aco_opcode::v_dual_mov_b32)
      encoding |= reg(ctx, instr->operands[1], 8) << 9;
   encoding |= static_cast<uint32_t>(ctx.opcode[static_cast<int>(vopd.opy)]) << 17;
   encoding |= opcode << 22;
   out.push_back(encoding);

   const unsigned opy_start = get_vopd_opy_start(instr);

   encoding = reg(ctx, instr->operands[opy_start]);
   if (vopd.opy != aco_opcode::v_dual_mov_b32)
      encoding |= reg(ctx, instr->operands[opy_start + 1], 8) << 9;
   encoding |= (reg(ctx, instr->definitions[1], 8) >> 1) << 17;
   encoding |= reg(ctx, instr->definitions[0], 8) << 24;
   out.push_back(encoding);
}

void
emit_ds_instruction(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[static_cast<int>(instr->opcode)];
   const DS_instruction& ds = instr->ds();
   const enum amd_gfx_level gfx = ctx.gfx_level;

   uint32_t encoding = (0b110110u << 26);
   if (gfx == GFX8 || gfx == GFX9) [[likely]] {
      encoding |= opcode << 17;
      encoding |= (ds.gds ? 1u : 0u) << 16;
   } else {
      encoding |= opcode << 18;
      encoding |= (ds.gds ? 1u : 0u) << 17;
   }
   encoding |= (static_cast<uint32_t>(ds.offset1) & 0xFFu) << 8;
   encoding |= static_cast<uint32_t>(ds.offset0) & 0xFFFFu;
   out.push_back(encoding);

   encoding = 0;
   if (!instr->definitions.empty())
      encoding |= reg(ctx, instr->definitions.back(), 8) << 24;

   unsigned op_idx = 0;
   for (unsigned vector_idx = 0; op_idx < MIN2(instr->operands.size(), 3u); vector_idx++) {
      assert(vector_idx < 3);

      const Operand& op = instr->operands[op_idx];
      if (op.physReg() != m0 && !op.isUndefined())
         encoding |= reg(ctx, op, 8) << (8 * vector_idx);
      while (op_idx < instr->operands.size() && instr->operands[op_idx].isVectorAligned())
         ++op_idx;
      ++op_idx;
   }
   out.push_back(encoding);
}

void
emit_ldsdir_instruction(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[(int)instr->opcode];
   const LDSDIR_instruction& dir = instr->ldsdir();
   const enum amd_gfx_level gfx = ctx.gfx_level;

   uint32_t encoding = (0b11001110 << 24);
   encoding |= opcode << 20;
   encoding |= (uint32_t)dir.wait_vdst << 16;
   if (gfx >= GFX12)
      encoding |= (uint32_t)dir.wait_vsrc << 23;
   encoding |= (uint32_t)dir.attr << 10;
   encoding |= (uint32_t)dir.attr_chan << 8;
   encoding |= reg(ctx, instr->definitions[0], 8);
   out.push_back(encoding);
}

void
emit_mubuf_instruction(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[(int)instr->opcode];
   const MUBUF_instruction& mubuf = instr->mubuf();
   const bool glc = mubuf.cache.value & ac_glc;
   const bool slc = mubuf.cache.value & ac_slc;
   const bool dlc = mubuf.cache.value & ac_dlc;
   const enum amd_gfx_level gfx = ctx.gfx_level;

   uint32_t encoding = (0b111000 << 26);
   encoding |= (mubuf.lds ? 1u : 0u) << 16;
   encoding |= opcode << 18;
   encoding |= (glc ? 1u : 0u) << 14;
   if (gfx <= GFX10_3) [[likely]]
      encoding |= (mubuf.idxen ? 1u : 0u) << 13;
   assert(!mubuf.addr64 || gfx <= GFX7);
   if (gfx == GFX6 || gfx == GFX7)
      encoding |= (mubuf.addr64 ? 1u : 0u) << 15;
   if (gfx <= GFX10_3) [[likely]]
      encoding |= (mubuf.offen ? 1u : 0u) << 12;
   if (gfx == GFX8 || gfx == GFX9) {
      assert(!dlc);
      encoding |= (slc ? 1u : 0u) << 17;
   } else if (gfx >= GFX11) [[unlikely]] {
      encoding |= (slc ? 1u : 0u) << 12;
      encoding |= (dlc ? 1u : 0u) << 13;
   } else if (gfx >= GFX10) [[unlikely]] {
      encoding |= (dlc ? 1u : 0u) << 15;
   }
   encoding |= 0x0FFFu & mubuf.offset;
   out.push_back(encoding);
   encoding = 0;
   if (gfx <= GFX7 || (gfx >= GFX10 && gfx <= GFX10_3)) {
      encoding |= (slc ? 1u : 0u) << 22;
   }
   encoding |= reg(ctx, instr->operands[2]) << 24;
   if (gfx >= GFX11) [[unlikely]] {
      encoding |= (mubuf.tfe ? 1u : 0u) << 21;
      encoding |= (mubuf.offen ? 1u : 0u) << 22;
      encoding |= (mubuf.idxen ? 1u : 0u) << 23;
   } else {
      encoding |= (mubuf.tfe ? 1u : 0u) << 23;
   }
   encoding |= (reg(ctx, instr->operands[0]) >> 2) << 16;
   if (instr->operands.size() > 3 && !mubuf.lds)
      encoding |= reg(ctx, instr->operands[3], 8) << 8;
   else if (!mubuf.lds)
      encoding |= reg(ctx, instr->definitions[0], 8) << 8;
   encoding |= reg(ctx, instr->operands[1], 8);
   out.push_back(encoding);
}

void
emit_mubuf_instruction_gfx12(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[(int)instr->opcode];
   const MUBUF_instruction& mubuf = instr->mubuf();
   assert(!mubuf.lds);

   uint32_t encoding = 0b110001 << 26;
   encoding |= opcode << 14;
   if (instr->operands[2].isConstant()) {
      assert(instr->operands[2].constantValue() == 0);
      encoding |= reg(ctx, sgpr_null);
   } else {
      encoding |= reg(ctx, instr->operands[2]);
   }
   encoding |= (mubuf.tfe ? 1u : 0u) << 22;
   out.push_back(encoding);

   encoding = 0;
   if (instr->operands.size() > 3)
      encoding |= reg(ctx, instr->operands[3], 8);
   else
      encoding |= reg(ctx, instr->definitions[0], 8);
   encoding |= reg(ctx, instr->operands[0]) << 9;
   encoding |= (mubuf.offen ? 1u : 0u) << 30;
   encoding |= (mubuf.idxen ? 1u : 0u) << 31;
   encoding |= get_gfx12_cpol(mubuf) << 18;
   encoding |= 1u << 23;
   out.push_back(encoding);

   encoding = 0;
   if (!instr->operands[1].isUndefined())
      encoding |= reg(ctx, instr->operands[1], 8);
   encoding |= (mubuf.offset & 0x00ffffffu) << 8;
   out.push_back(encoding);
}

void
emit_mtbuf_instruction(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[(int)instr->opcode];
   const MTBUF_instruction& mtbuf = instr->mtbuf();
   const bool glc = mtbuf.cache.value & ac_glc;
   const bool slc = mtbuf.cache.value & ac_slc;
   const bool dlc = mtbuf.cache.value & ac_dlc;
   const enum amd_gfx_level gfx = ctx.gfx_level;
   const uint32_t img_format = ac_get_tbuffer_format(gfx, mtbuf.dfmt, mtbuf.nfmt);
   assert(img_format <= 0x7F);
   assert(!dlc || gfx >= GFX10);

   uint32_t encoding = (0b111010 << 26);
   encoding |= (img_format << 19);
   if (gfx < GFX8) {
      encoding |= opcode << 16;
   } else if (gfx >= GFX10 && gfx < GFX11) {
      encoding |= (opcode & 0x07u) << 16;
      encoding |= (dlc ? 1u : 0u) << 15;
   } else {
      encoding |= opcode << 15;
   }
   encoding |= (glc ? 1u : 0u) << 14;
   if (gfx >= GFX11) [[unlikely]] {
      encoding |= (dlc ? 1u : 0u) << 13;
      encoding |= (slc ? 1u : 0u) << 12;
   } else {
      encoding |= (mtbuf.idxen ? 1u : 0u) << 13;
      encoding |= (mtbuf.offen ? 1u : 0u) << 12;
   }
   encoding |= 0x0FFFu & mtbuf.offset;
   out.push_back(encoding);

   encoding = 0;
   encoding |= reg(ctx, instr->operands[2]) << 24;
   if (gfx >= GFX11) [[unlikely]] {
      encoding |= (mtbuf.idxen ? 1u : 0u) << 23;
      encoding |= (mtbuf.offen ? 1u : 0u) << 22;
      encoding |= (mtbuf.tfe ? 1u : 0u) << 21;
   } else {
      encoding |= (mtbuf.tfe ? 1u : 0u) << 23;
      encoding |= (slc ? 1u : 0u) << 22;
      if (gfx >= GFX10)
         encoding |= (((opcode & 0x08u) >> 3) << 21);
   }
   encoding |= (reg(ctx, instr->operands[0]) >> 2) << 16;
   if (instr->operands.size() > 3)
      encoding |= reg(ctx, instr->operands[3], 8) << 8;
   else
      encoding |= reg(ctx, instr->definitions[0], 8) << 8;
   encoding |= reg(ctx, instr->operands[1], 8);
   out.push_back(encoding);
}

void
emit_mtbuf_instruction_gfx12(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[(int)instr->opcode];
   const MTBUF_instruction& mtbuf = instr->mtbuf();
   const enum amd_gfx_level gfx = ctx.gfx_level;

   const uint32_t img_format = ac_get_tbuffer_format(gfx, mtbuf.dfmt, mtbuf.nfmt);

   uint32_t encoding = 0b110001 << 26;
   encoding |= 0b1000 << 18;
   encoding |= opcode << 14;
   if (instr->operands[2].isConstant()) {
      assert(instr->operands[2].constantValue() == 0);
      encoding |= reg(ctx, sgpr_null);
   } else {
      encoding |= reg(ctx, instr->operands[2]);
   }
   encoding |= (mtbuf.tfe ? 1u : 0u) << 22;
   out.push_back(encoding);

   encoding = 0;
   if (instr->operands.size() > 3)
      encoding |= reg(ctx, instr->operands[3], 8);
   else
      encoding |= reg(ctx, instr->definitions[0], 8);
   encoding |= reg(ctx, instr->operands[0]) << 9;
   encoding |= (mtbuf.offen ? 1u : 0u) << 30;
   encoding |= (mtbuf.idxen ? 1u : 0u) << 31;
   encoding |= get_gfx12_cpol(mtbuf) << 18;
   encoding |= img_format << 23;
   out.push_back(encoding);

   encoding = 0;
   encoding |= reg(ctx, instr->operands[1], 8);
   encoding |= (mtbuf.offset & 0x00ffffffu) << 8;
   out.push_back(encoding);
}

void
emit_mimg_instruction(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[(int)instr->opcode];
   const MIMG_instruction& mimg = instr->mimg();
   const bool glc = mimg.cache.value & ac_glc;
   const bool slc = mimg.cache.value & ac_slc;
   const bool dlc = mimg.cache.value & ac_dlc;
   const enum amd_gfx_level gfx = ctx.gfx_level;

   unsigned nsa_dwords = get_mimg_nsa_dwords(instr);
   assert(!nsa_dwords || gfx >= GFX10);

   uint32_t encoding = (0b111100 << 26);
   if (gfx >= GFX11) [[unlikely]] {
      assert(nsa_dwords <= 1);
      encoding |= nsa_dwords;
      encoding |= mimg.dim << 2;
      encoding |= mimg.unrm ? 1u << 7 : 0u;
      encoding |= (0xFu & mimg.dmask) << 8;
      encoding |= slc ? 1u << 12 : 0u;
      encoding |= dlc ? 1u << 13 : 0u;
      encoding |= glc ? 1u << 14 : 0u;
      encoding |= mimg.r128 ? 1u << 15 : 0u;
      encoding |= mimg.a16 ? 1u << 16 : 0u;
      encoding |= mimg.d16 ? 1u << 17 : 0u;
      encoding |= (opcode & 0xFFu) << 18;
   } else {
      encoding |= slc ? 1u << 25 : 0u;
      encoding |= (opcode & 0x7fu) << 18;
      encoding |= (opcode >> 7) & 1u;
      encoding |= mimg.lwe ? 1u << 17 : 0u;
      encoding |= mimg.tfe ? 1u << 16 : 0u;
      encoding |= glc ? 1u << 13 : 0u;
      encoding |= mimg.unrm ? 1u << 12 : 0u;
      if (gfx <= GFX9) {
         assert(!dlc);
         assert(!mimg.r128);
         encoding |= mimg.a16 ? 1u << 15 : 0u;
         encoding |= mimg.da ? 1u << 14 : 0u;
      } else {
         encoding |= mimg.r128 ? 1u << 15 : 0u;
         encoding |= nsa_dwords << 1;
         encoding |= mimg.dim << 3;
         encoding |= dlc ? 1u << 7 : 0u;
      }
      encoding |= (0xFu & mimg.dmask) << 8;
   }
   out.push_back(encoding);

   encoding = reg(ctx, instr->operands[3], 8);
   if (!instr->definitions.empty()) {
      encoding |= reg(ctx, instr->definitions[0], 8) << 8;
   } else if (!instr->operands[2].isUndefined()) {
      encoding |= reg(ctx, instr->operands[2], 8) << 8;
   }
   encoding |= (0x1Fu & (reg(ctx, instr->operands[0]) >> 2)) << 16;

   assert(!mimg.d16 || gfx >= GFX9);
   if (gfx >= GFX11) [[unlikely]] {
      if (!instr->operands[1].isUndefined())
         encoding |= (0x1Fu & (reg(ctx, instr->operands[1]) >> 2)) << 26;

      encoding |= mimg.tfe ? 1u << 21 : 0u;
      encoding |= mimg.lwe ? 1u << 22 : 0u;
   } else {
      if (!instr->operands[1].isUndefined())
         encoding |= (0x1Fu & (reg(ctx, instr->operands[1]) >> 2)) << 21;

      encoding |= mimg.d16 ? 1u << 31 : 0u;
      if (gfx >= GFX10) {
         encoding |= mimg.a16 ? 1u << 30 : 0u;
      }
   }

   out.push_back(encoding);

   if (nsa_dwords) {
      out.resize(out.size() + nsa_dwords);
      std::vector<uint32_t>::iterator nsa = std::prev(out.end(), nsa_dwords);
      for (unsigned i = 4, k = 0; i < instr->operands.size(); i++) {
         if (instr->operands[i - 1].isVectorAligned())
            continue;
         nsa[k / 4] |= reg(ctx, instr->operands[i], 8) << (k % 4 * 8);
         k++;
      }
   }
}

void
emit_mimg_instruction_gfx12(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[(int)instr->opcode];
   const MIMG_instruction& mimg = instr->mimg();

   bool vsample = !instr->operands[1].isUndefined() || instr->opcode == aco_opcode::image_msaa_load;
   uint32_t encoding = opcode << 14;
   if (vsample) {
      encoding |= 0b111001 << 26;
      encoding |= mimg.tfe << 3;
      encoding |= mimg.unrm << 13;
   } else {
      encoding |= 0b110100 << 26;
   }
   encoding |= mimg.dim;
   encoding |= mimg.r128 << 4;
   encoding |= mimg.d16 << 5;
   encoding |= mimg.a16 << 6;
   encoding |= (mimg.dmask & 0xfu) << 22;
   out.push_back(encoding);

   uint8_t vaddr[5] = {0, 0, 0, 0, 0};
   for (unsigned i = 3, k = 0; i < instr->operands.size(); i++) {
      if (instr->operands[i - 1].isVectorAligned())
         continue;
      vaddr[k++] = reg(ctx, instr->operands[i], 8);
   }
   int num_vaddr = instr->operands.size() - 3;
   for (int i = 0; i < (int)MIN2(instr->operands.back().size() - 1, ARRAY_SIZE(vaddr) - num_vaddr); i++)
      vaddr[num_vaddr + i] = reg(ctx, instr->operands.back(), 8) + i + 1;

   encoding = 0;
   if (!instr->definitions.empty())
      encoding |= reg(ctx, instr->definitions.back(), 8);
   else if (!instr->operands[2].isUndefined())
      encoding |= reg(ctx, instr->operands[2], 8);
   encoding |= reg(ctx, instr->operands[0]) << 9;
   if (vsample) {
      encoding |= mimg.lwe << 8;
      if (instr->opcode != aco_opcode::image_msaa_load)
         encoding |= reg(ctx, instr->operands[1]) << 23;
   } else {
      encoding |= mimg.tfe << 23;
      encoding |= vaddr[4] << 24;
   }
   encoding |= get_gfx12_cpol(mimg) << 18;
   out.push_back(encoding);

   encoding = 0;
   for (unsigned i = 0; i < 4; i++)
      encoding |= vaddr[i] << (i * 8);
   out.push_back(encoding);
}

void
emit_flatlike_instruction(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[(int)instr->opcode];
   const FLAT_instruction& flat = instr->flatlike();
   const bool glc = flat.cache.value & ac_glc;
   const bool slc = flat.cache.value & ac_slc;
   const bool dlc = flat.cache.value & ac_dlc;
   const enum amd_gfx_level gfx = ctx.gfx_level;

   uint32_t encoding = (0b110111 << 26);
   encoding |= opcode << 18;
   if (gfx == GFX9 || gfx >= GFX11) {
      if (instr->isFlat())
         assert(flat.offset <= 0xfff);
      else
         assert(flat.offset >= -4096 && flat.offset < 4096);
      encoding |= flat.offset & 0x1fffu;
   } else if (gfx <= GFX8 || instr->isFlat()) {
      assert(flat.offset == 0);
   } else {
      assert(flat.offset >= -2048 && flat.offset <= 2047);
      encoding |= flat.offset & 0xfffu;
   }
   if (instr->isScratch())
      encoding |= 1u << (gfx >= GFX11 ? 16 : 14);
   else if (instr->isGlobal())
      encoding |= 2u << (gfx >= GFX11 ? 16 : 14);
   encoding |= flat.lds ? 1u << 13 : 0u;
   encoding |= glc ? 1u << (gfx >= GFX11 ? 14 : 16) : 0u;
   encoding |= slc ? 1u << (gfx >= GFX11 ? 15 : 17) : 0u;
   if (gfx >= GFX10) [[unlikely]] {
      assert(!flat.nv);
      encoding |= dlc ? 1u << (gfx >= GFX11 ? 13 : 12) : 0u;
   } else {
      assert(!dlc);
   }
   out.push_back(encoding);
   encoding = reg(ctx, instr->operands[0], 8);
   if (!instr->definitions.empty())
      encoding |= reg(ctx, instr->definitions[0], 8) << 24;
   if (instr->operands.size() >= 3)
      encoding |= reg(ctx, instr->operands[2], 8) << 8;
   if (!instr->operands[1].isUndefined()) {
      assert(gfx >= GFX10 || instr->operands[1].physReg() != 0x7F);
      assert(instr->format != Format::FLAT);
      encoding |= reg(ctx, instr->operands[1], 8) << 16;
   } else if (instr->format != Format::FLAT || gfx >= GFX10) {
      if (gfx <= GFX9 ||
          (instr->isScratch() && instr->operands[0].isUndefined() && gfx < GFX11))
         encoding |= 0x7Fu << 16;
      else
         encoding |= reg(ctx, sgpr_null) << 16;
   }
   if (gfx >= GFX11 && instr->isScratch())
      encoding |= !instr->operands[0].isUndefined() ? 1u << 23 : 0u;
   else
      encoding |= flat.nv ? 1u << 23 : 0u;
   out.push_back(encoding);
}

void
emit_flatlike_instruction_gfx12(asm_context& ctx, std::vector<uint32_t>& out,
                                const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[(int)instr->opcode];
   const FLAT_instruction& flat = instr->flatlike();
   assert(!flat.lds);

   uint32_t encoding = opcode << 14;
   encoding |= 0b111011 << 26;
   if (!instr->operands[1].isUndefined()) {
      assert(!instr->isFlat());
      encoding |= reg(ctx, instr->operands[1]);
   } else {
      encoding |= reg(ctx, sgpr_null);
   }
   if (instr->isScratch())
      encoding |= 1u << 24;
   else if (instr->isGlobal())
      encoding |= 2u << 24;
   out.push_back(encoding);

   encoding = 0;
   if (!instr->definitions.empty())
      encoding |= reg(ctx, instr->definitions[0], 8);
   if (instr->isScratch())
      encoding |= !instr->operands[0].isUndefined() ? 1u << 17 : 0u;
   encoding |= get_gfx12_cpol(flat) << 18;
   if (instr->operands.size() >= 3)
      encoding |= reg(ctx, instr->operands[2], 8) << 23;
   out.push_back(encoding);

   encoding = 0;
   if (!instr->operands[0].isUndefined())
      encoding |= reg(ctx, instr->operands[0], 8);
   encoding |= (flat.offset & 0x00ffffffu) << 8;
   out.push_back(encoding);
}

void
emit_exp_instruction(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   const Export_instruction& exp = instr->exp();
   const enum amd_gfx_level gfx = ctx.gfx_level;
   uint32_t encoding;
   if (gfx == GFX8 || gfx == GFX9) {
      encoding = (0b110001 << 26);
   } else {
      encoding = (0b111110 << 26);
   }

   if (gfx >= GFX11) [[unlikely]] {
      encoding |= exp.row_en ? 0b1u << 13 : 0u;
   } else {
      encoding |= exp.valid_mask ? 0b1u << 12 : 0u;
      encoding |= exp.compressed ? 0b1u << 10 : 0u;
   }
   encoding |= exp.done ? 0b1u << 11 : 0u;
   encoding |= exp.dest << 4;
   encoding |= exp.enabled_mask;

   /* GFX6 (except OLAND and HAINAN) has a bug that it only looks at the X
    * writemask component.
    */
   if (ctx.program->dev.has_gfx6_mrt_export_bug && exp.enabled_mask && exp.dest <= V_008DFC_SQ_EXP_MRTZ) {
      encoding |= 0x1;
   }

   out.push_back(encoding);
   encoding = reg(ctx, exp.operands[0], 8);
   encoding |= reg(ctx, exp.operands[1], 8) << 8;
   encoding |= reg(ctx, exp.operands[2], 8) << 16;
   encoding |= reg(ctx, exp.operands[3], 8) << 24;
   out.push_back(encoding);
}

void emit_instruction(asm_context& ctx, std::vector<uint32_t>& out, Instruction* instr);

void
emit_dpp16_instruction(asm_context& ctx, std::vector<uint32_t>& out, Instruction* instr)
{
   assert(ctx.gfx_level >= GFX8);
   DPP16_instruction& dpp = instr->dpp16();

   Operand dpp_op = instr->operands[0];
   instr->operands[0] = Operand(PhysReg{250}, v1);
   instr->format = (Format)((uint16_t)instr->format & ~(uint16_t)Format::DPP16);
   emit_instruction(ctx, out, instr);
   instr->format = (Format)((uint16_t)instr->format | (uint16_t)Format::DPP16);
   instr->operands[0] = dpp_op;

   uint32_t encoding = (0xFu & dpp.row_mask) << 28;
   encoding |= (0xFu & dpp.bank_mask) << 24;
   encoding |= dpp.abs[1] << 23;
   encoding |= dpp.neg[1] << 22;
   encoding |= dpp.abs[0] << 21;
   encoding |= dpp.neg[0] << 20;
   encoding |= dpp.fetch_inactive << 18;
   encoding |= dpp.bound_ctrl << 19;
   encoding |= dpp.dpp_ctrl << 8;
   encoding |= reg(ctx, dpp_op, 8);
   encoding |= dpp.opsel[0] && !instr->isVOP3() ? 128 : 0;
   out.push_back(encoding);
}

void
emit_dpp8_instruction(asm_context& ctx, std::vector<uint32_t>& out, Instruction* instr)
{
   assert(ctx.gfx_level >= GFX10);
   DPP8_instruction& dpp = instr->dpp8();

   Operand dpp_op = instr->operands[0];
   instr->operands[0] = Operand(PhysReg{233u + dpp.fetch_inactive}, v1);
   instr->format = (Format)((uint16_t)instr->format & ~(uint16_t)Format::DPP8);
   emit_instruction(ctx, out, instr);
   instr->format = (Format)((uint16_t)instr->format | (uint16_t)Format::DPP8);
   instr->operands[0] = dpp_op;

   uint32_t encoding = reg(ctx, dpp_op, 8);
   encoding |= dpp.opsel[0] && !instr->isVOP3() ? 128 : 0;
   encoding |= dpp.lane_sel << 8;
   out.push_back(encoding);
}

void
emit_vop3_instruction(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   uint32_t opcode = ctx.opcode[(int)instr->opcode];
   const VALU_instruction& vop3 = instr->valu();
   const enum amd_gfx_level gfx = ctx.gfx_level;

   /* Adjust opcode based on original instruction format */
   if (instr->isVOP2()) {
      opcode += 0x100;
   } else if (instr->isVOP1()) {
      opcode += (gfx == GFX8 || gfx == GFX9) ? 0x140 : 0x180;
   } else if (instr->isVINTRP()) {
      opcode += 0x270;
   }
   /* VOPC: no adjustment needed (offset is 0) */

   uint32_t encoding;
   if (gfx <= GFX9) {
      encoding = (0b110100 << 26);
   } else if (gfx >= GFX10) {
      encoding = (0b110101 << 26);
   } else {
      UNREACHABLE("Unknown gfx_level.");
   }

   if (gfx <= GFX7) [[unlikely]] {
      encoding |= opcode << 17;
      encoding |= (vop3.clamp ? 1u : 0u) << 11;
   } else {
      encoding |= opcode << 16;
      encoding |= (vop3.clamp ? 1u : 0u) << 15;
   }
   encoding |= vop3.opsel << 11;
   for (unsigned i = 0; i < 3; i++)
      encoding |= vop3.abs[i] << (8 + i);
   if (instr->definitions.size() == 2 && instr->isVOPC())
      assert(gfx <= GFX9 && instr->definitions[1].physReg() == exec);
   else if (instr->definitions.size() == 2 && instr->opcode != aco_opcode::v_swap_b16)
      encoding |= reg(ctx, instr->definitions[1]) << 8;
   encoding |= reg(ctx, instr->definitions[0], 8);
   out.push_back(encoding);

   encoding = 0;
   const unsigned num_ops = instr->opcode == aco_opcode::v_writelane_b32_e64 ? 2u :
                            instr->opcode == aco_opcode::v_swap_b16 ? 1u :
                            static_cast<unsigned>(instr->operands.size());

   for (unsigned i = 0; i < num_ops; i++)
      encoding |= reg(ctx, instr->operands[i]) << (i * 9);

   /* RDNA (GFX10+): encode unused operand slots as inline constant 0 (encoding 128)
    * for decoder performance. GFX9 and earlier leave unused slots as zero. */
   if (gfx >= GFX10) [[unlikely]] {
      for (unsigned i = num_ops; i < 3; i++)
         encoding |= 128u << (i * 9);
   }

   encoding |= vop3.omod << 27;
   for (unsigned i = 0; i < 3; i++)
      encoding |= vop3.neg[i] << (29 + i);
   out.push_back(encoding);
}

void
emit_vop3p_instruction(asm_context& ctx, std::vector<uint32_t>& out, const Instruction* instr)
{
   const uint32_t opcode = ctx.opcode[(int)instr->opcode];
   const VALU_instruction& vop3 = instr->valu();
   const enum amd_gfx_level gfx = ctx.gfx_level;

   uint32_t encoding;
   if (gfx == GFX9) {
      encoding = (0b110100111 << 23);
   } else if (gfx >= GFX10) {
      encoding = (0b110011 << 26);
   } else {
      UNREACHABLE("Unknown gfx_level.");
   }

   encoding |= opcode << 16;
   encoding |= (vop3.clamp ? 1u : 0u) << 15;
   encoding |= vop3.opsel_lo << 11;
   encoding |= ((vop3.opsel_hi & 0x4u) ? 1u : 0u) << 14;
   for (unsigned i = 0; i < 3; i++)
      encoding |= vop3.neg_hi[i] << (8 + i);
   encoding |= reg(ctx, instr->definitions[0], 8);
   out.push_back(encoding);

   encoding = 0;
   const unsigned num_ops = static_cast<unsigned>(instr->operands.size());
   for (unsigned i = 0; i < num_ops; i++)
      encoding |= reg(ctx, instr->operands[i]) << (i * 9);

   /* RDNA (GFX10+): encode unused operand slots as inline constant 0 (encoding 128)
    * for decoder performance. GFX9 and earlier leave unused slots as zero. */
   if (gfx >= GFX10) [[unlikely]] {
      for (unsigned i = num_ops; i < 3; i++)
         encoding |= 128u << (i * 9);
   }

   encoding |= (vop3.opsel_hi & 0x3u) << 27;
   for (unsigned i = 0; i < 3; i++)
      encoding |= vop3.neg_lo[i] << (29 + i);
   out.push_back(encoding);
}

void
emit_sdwa_instruction(asm_context& ctx, std::vector<uint32_t>& out, Instruction* instr)
{
   assert(ctx.gfx_level >= GFX8 && ctx.gfx_level < GFX11);
   SDWA_instruction& sdwa = instr->sdwa();
   const enum amd_gfx_level gfx = ctx.gfx_level;

   Operand sdwa_op = instr->operands[0];
   instr->operands[0] = Operand(PhysReg{249}, v1);
   instr->format = (Format)((uint16_t)instr->format & ~(uint16_t)Format::SDWA);
   emit_instruction(ctx, out, instr);
   instr->format = (Format)((uint16_t)instr->format | (uint16_t)Format::SDWA);
   instr->operands[0] = sdwa_op;

   uint32_t encoding = 0;

   if (instr->isVOPC()) {
      if (instr->definitions[0].physReg() !=
          (gfx >= GFX10 && is_cmpx(instr->opcode) ? exec : vcc)) {
         encoding |= reg(ctx, instr->definitions[0]) << 8;
         encoding |= 1u << 15;
      }
      encoding |= (sdwa.clamp ? 1u : 0u) << 13;
   } else {
      encoding |= sdwa.dst_sel.to_sdwa_sel(instr->definitions[0].physReg().byte()) << 8;
      uint32_t dst_u = sdwa.dst_sel.sign_extend() ? 1u : 0u;
      if (instr->definitions[0].bytes() < 4)
         dst_u = 2u;
      encoding |= dst_u << 11;
      encoding |= (sdwa.clamp ? 1u : 0u) << 13;
      encoding |= sdwa.omod << 14;
   }

   encoding |= sdwa.sel[0].to_sdwa_sel(sdwa_op.physReg().byte()) << 16;
   encoding |= sdwa.sel[0].sign_extend() ? 1u << 19 : 0u;
   encoding |= sdwa.abs[0] << 21;
   encoding |= sdwa.neg[0] << 20;

   if (instr->operands.size() >= 2) {
      encoding |= sdwa.sel[1].to_sdwa_sel(instr->operands[1].physReg().byte()) << 24;
      encoding |= sdwa.sel[1].sign_extend() ? 1u << 27 : 0u;
      encoding |= sdwa.abs[1] << 29;
      encoding |= sdwa.neg[1] << 28;
   }

   encoding |= reg(ctx, sdwa_op, 8);
   encoding |= (sdwa_op.physReg() < 256) << 23;
   if (instr->operands.size() >= 2)
      encoding |= (instr->operands[1].physReg() < 256) << 31;
   out.push_back(encoding);
}

void
emit_instruction(asm_context& ctx, std::vector<uint32_t>& out, Instruction* instr)
{
   if (instr->opcode == aco_opcode::p_constaddr_getpc) {
      ctx.constaddrs[instr->operands[0].constantValue()].getpc_end = out.size() + 1;

      instr->opcode = aco_opcode::s_getpc_b64;
      instr->operands.pop_back();
   } else if (instr->opcode == aco_opcode::p_constaddr_addlo) {
      ctx.constaddrs[instr->operands[2].constantValue()].add_literal = out.size() + 1;

      instr->opcode = aco_opcode::s_add_u32;
      instr->operands.pop_back();
      assert(instr->operands[1].isConstant());
      instr->operands[1] = Operand::literal32(instr->operands[1].constantValue());
   } else if (instr->opcode == aco_opcode::p_resumeaddr_getpc) {
      ctx.resumeaddrs[instr->operands[0].constantValue()].getpc_end = out.size() + 1;

      instr->opcode = aco_opcode::s_getpc_b64;
      instr->operands.pop_back();
   } else if (instr->opcode == aco_opcode::p_resumeaddr_addlo) {
      ctx.resumeaddrs[instr->operands[2].constantValue()].add_literal = out.size() + 1;

      instr->opcode = aco_opcode::s_add_u32;
      instr->operands.pop_back();
      assert(instr->operands[1].isConstant());
      instr->operands[1] = Operand::literal32(instr->operands[1].constantValue());
   } else if (instr->opcode == aco_opcode::p_load_symbol) {
      assert(instr->operands[0].isConstant());
      assert(ctx.symbols);

      ctx.symbols->emplace_back(aco_symbol{
         (enum aco_symbol_id)instr->operands[0].constantValue(),
         static_cast<unsigned>(out.size() + 1)
      });

      instr->opcode = aco_opcode::s_mov_b32;
      instr->operands[0] = Operand::literal32(0);
   } else if (instr->opcode == aco_opcode::p_debug_info) {
      assert(instr->operands[0].isConstant());
      uint32_t index = instr->operands[0].constantValue();
      ctx.program->debug_info[index].offset = static_cast<unsigned>(out.size() * 4u);
      return;
   }

   if ((instr->isVOP1() || instr->isVOP2() || instr->isVOPC()) && !instr->isVOP3() &&
       needs_vop3_gfx11(ctx, instr)) {
      instr->format = asVOP3(instr->format);
      if (instr->opcode == aco_opcode::v_fmaak_f16) {
         instr->opcode = aco_opcode::v_fma_f16;
         instr->format = (Format)((uint32_t)instr->format & ~(uint32_t)Format::VOP2);
      } else if (instr->opcode == aco_opcode::v_fmamk_f16) {
         instr->valu().swapOperands(1, 2);
         instr->opcode = aco_opcode::v_fma_f16;
         instr->format = (Format)((uint32_t)instr->format & ~(uint32_t)Format::VOP2);
      }
   }

   uint32_t opcode = ctx.opcode[(int)instr->opcode];
   if (opcode == (uint32_t)-1) {
      char* outmem;
      size_t outsize;
      struct u_memstream mem;
      u_memstream_open(&mem, &outmem, &outsize);
      FILE* const memf = u_memstream_get(&mem);

      fprintf(memf, "Unsupported opcode: ");
      aco_print_instr(ctx.gfx_level, instr, memf);
      u_memstream_close(&mem);

      aco_err(ctx.program, outmem);
      free(outmem);

      abort();
   }

   switch (instr->format) {
   case Format::SOP2: {
      emit_sop2_instruction(ctx, out, instr);
      break;
   }
   case Format::SOPK: {
      emit_sopk_instruction(ctx, out, instr);
      break;
   }
   case Format::SOP1: {
      emit_sop1_instruction(ctx, out, instr);
      break;
   }
   case Format::SOPC: {
      emit_sopc_instruction(ctx, out, instr);
      break;
   }
   case Format::SOPP: {
      emit_sopp_instruction(ctx, out, instr);
      break;
   }
   case Format::SMEM: {
      emit_smem_instruction(ctx, out, instr);
      return;
   }
   case Format::VOP2: {
      emit_vop2_instruction(ctx, out, instr);
      break;
   }
   case Format::VOP1: {
      emit_vop1_instruction(ctx, out, instr);
      break;
   }
   case Format::VOPC: {
      emit_vopc_instruction(ctx, out, instr);
      break;
   }
   case Format::VINTRP: {
      emit_vintrp_instruction(ctx, out, instr);
      break;
   }
   case Format::VINTERP_INREG: {
      emit_vinterp_inreg_instruction(ctx, out, instr);
      break;
   }
   case Format::VOPD: {
      emit_vopd_instruction(ctx, out, instr);
      break;
   }
   case Format::DS: {
      emit_ds_instruction(ctx, out, instr);
      break;
   }
   case Format::LDSDIR: {
      emit_ldsdir_instruction(ctx, out, instr);
      break;
   }
   case Format::MUBUF: {
      if (ctx.gfx_level >= GFX12)
         emit_mubuf_instruction_gfx12(ctx, out, instr);
      else
         emit_mubuf_instruction(ctx, out, instr);
      break;
   }
   case Format::MTBUF: {
      if (ctx.gfx_level >= GFX12)
         emit_mtbuf_instruction_gfx12(ctx, out, instr);
      else
         emit_mtbuf_instruction(ctx, out, instr);
      break;
   }
   case Format::MIMG: {
      if (ctx.gfx_level >= GFX12)
         emit_mimg_instruction_gfx12(ctx, out, instr);
      else
         emit_mimg_instruction(ctx, out, instr);
      break;
   }
   case Format::FLAT:
   case Format::SCRATCH:
   case Format::GLOBAL: {
      if (ctx.gfx_level >= GFX12)
         emit_flatlike_instruction_gfx12(ctx, out, instr);
      else
         emit_flatlike_instruction(ctx, out, instr);
      break;
   }
   case Format::EXP: {
      emit_exp_instruction(ctx, out, instr);
      break;
   }
   case Format::PSEUDO:
   case Format::PSEUDO_BARRIER:
      if (instr->opcode != aco_opcode::p_unit_test)
         UNREACHABLE("Pseudo instructions should be lowered before assembly.");
      break;
   default:
      if (instr->isDPP16()) {
         emit_dpp16_instruction(ctx, out, instr);
         return;
      } else if (instr->isDPP8()) {
         emit_dpp8_instruction(ctx, out, instr);
         return;
      } else if (instr->isVOP3()) {
         emit_vop3_instruction(ctx, out, instr);
      } else if (instr->isVOP3P()) {
         emit_vop3p_instruction(ctx, out, instr);
      } else if (instr->isSDWA()) {
         emit_sdwa_instruction(ctx, out, instr);
      } else {
         UNREACHABLE("unimplemented instruction format");
      }
      break;
   }

   for (const Operand& op : instr->operands) {
      if (op.isLiteral()) {
         out.push_back(op.constantValue());
         break;
      }
   }
}

void
emit_block(asm_context& ctx, std::vector<uint32_t>& out, Block& block)
{
   for (aco_ptr<Instruction>& instr : block.instructions) {
#if 0
      int start_idx = out.size();
      std::cerr << "Encoding:\t" << std::endl;
      aco_print_instr(&*instr, stderr);
      std::cerr << std::endl;
#endif
      emit_instruction(ctx, out, instr.get());
#if 0
      for (int i = start_idx; i < out.size(); i++)
         std::cerr << "encoding: " << "0x" << std::setfill('0') << std::setw(8) << std::hex << out[i] << std::endl;
#endif
   }
}

void
fix_exports(asm_context& ctx, std::vector<uint32_t>& out, Program* program)
{
   bool exported = false;
   for (Block& block : program->blocks) {
      if (!(block.kind & block_kind_export_end))
         continue;
      std::vector<aco_ptr<Instruction>>::reverse_iterator it = block.instructions.rbegin();
      while (it != block.instructions.rend()) {
         if ((*it)->isEXP()) {
            Export_instruction& exp = (*it)->exp();
            if (program->stage.hw == AC_HW_VERTEX_SHADER ||
                program->stage.hw == AC_HW_NEXT_GEN_GEOMETRY_SHADER) {
               if (exp.dest >= V_008DFC_SQ_EXP_POS && exp.dest <= (V_008DFC_SQ_EXP_POS + 3)) {
                  exp.done = true;
                  exported = true;
                  break;
               }
            } else {
               exp.done = true;
               exp.valid_mask = true;
               exported = true;
               break;
            }
         } else if ((*it)->definitions.size() && (*it)->definitions[0].physReg() == exec) {
            break;
         }
         ++it;
      }
   }

   bool may_skip_export = program->stage.hw == AC_HW_PIXEL_SHADER && program->gfx_level >= GFX10;

   if (!exported && !may_skip_export) {
      bool is_vertex_or_ngg = (program->stage.hw == AC_HW_VERTEX_SHADER ||
                               program->stage.hw == AC_HW_NEXT_GEN_GEOMETRY_SHADER);
      aco_err(program,
              "Missing export in %s shader:", is_vertex_or_ngg ? "vertex or NGG" : "fragment");
      aco_print_program(program, stderr);
      abort();
   }
}

static void
insert_code(asm_context& ctx, std::vector<uint32_t>& out, unsigned insert_before,
            unsigned insert_count, const uint32_t* insert_data)
{
   out.insert(out.begin() + insert_before, insert_data, insert_data + insert_count);

   for (Block& block : ctx.program->blocks) {
      if (block.offset >= insert_before)
         block.offset += insert_count;
   }

   for (branch_info& info : ctx.branches) {
      if (info.pos >= insert_before)
         info.pos += insert_count;
   }

   for (auto& constaddr : ctx.constaddrs) {
      constaddr_info& info = constaddr.second;
      if (info.getpc_end >= insert_before)
         info.getpc_end += insert_count;
      if (info.add_literal >= insert_before)
         info.add_literal += insert_count;
   }
   for (auto& constaddr : ctx.resumeaddrs) {
      constaddr_info& info = constaddr.second;
      if (info.getpc_end >= insert_before)
         info.getpc_end += insert_count;
      if (info.add_literal >= insert_before)
         info.add_literal += insert_count;
   }

   if (ctx.symbols) {
      for (auto& symbol : *ctx.symbols) {
         if (symbol.offset >= insert_before)
            symbol.offset += insert_count;
      }
   }
}

static void
fix_branches_gfx10(asm_context& ctx, std::vector<uint32_t>& out)
{
   bool gfx10_3f_bug = false;

   do {
      auto buggy_branch_it = std::find_if(
         ctx.branches.begin(), ctx.branches.end(), [&](const branch_info& branch) -> bool
         { return ((int)ctx.program->blocks[branch.target].offset - branch.pos - 1) == 0x3f; });
      gfx10_3f_bug = buggy_branch_it != ctx.branches.end();

      if (gfx10_3f_bug) {
         constexpr uint32_t s_nop_0 = 0xbf800000u;
         insert_code(ctx, out, buggy_branch_it->pos + 1, 1, &s_nop_0);
      }
   } while (gfx10_3f_bug);
}

void
chain_branches(asm_context& ctx, std::vector<uint32_t>& out, branch_info& branch)
{
   Block* new_block = ctx.program->create_and_insert_block();
   Builder bld(ctx.program);
   std::vector<uint32_t> code;
   Instruction* branch_instr;

   unsigned target = branch.target;
   branch.target = new_block->index;

   unsigned skip_branch_target = 0;

   const int half_dist = (INT16_MAX - 31) / 2;
   const unsigned upper_start = MIN2(ctx.program->blocks[target].offset, branch.pos) + half_dist;
   const unsigned upper_end = upper_start + half_dist;
   const unsigned lower_end = MAX2(ctx.program->blocks[target].offset, branch.pos) - half_dist;
   const unsigned lower_start = lower_end - half_dist;
   unsigned insert_at = 0;
   for (unsigned i = 0; i < ctx.program->blocks.size() - 1; i++) {
      Block& block = ctx.program->blocks[i];
      Block& next = ctx.program->blocks[i + 1];
      if (next.offset >= lower_end)
         break;
      if (next.offset < upper_start || (next.offset > upper_end && next.offset < lower_start))
         continue;

      if (!block.instructions.empty() &&
          block.instructions.back()->opcode == aco_opcode::s_branch) {
         insert_at = next.offset;
         bld.reset(&block.instructions);
         if (next.offset >= lower_start)
            break;
      }
   }

   if (insert_at == 0) {
      unsigned insertion_block_idx = 0;
      unsigned next_block = 0;
      while (ctx.program->blocks[next_block + 1].offset < upper_end) {
         if (!ctx.program->blocks[next_block].instructions.empty())
            insertion_block_idx = next_block;
         next_block++;
      }

      insert_at = ctx.program->blocks[next_block].offset;
      if (insert_at < upper_start) {
         auto it = ctx.program->blocks[next_block].instructions.begin();
         int skip = 0;
         while (skip-- > 0 || insert_at < upper_start) {
            assert(it != ctx.program->blocks[next_block].instructions.end());
            Instruction* instr = (it++)->get();
            if (instr->isSOPP()) {
               if (instr->opcode == aco_opcode::s_clause)
                  skip = instr->salu().imm + 1;
               else if (instr->opcode == aco_opcode::s_delay_alu)
                  skip = ((instr->salu().imm >> 4) & 0x7) + 1;
               else if (instr->opcode == aco_opcode::s_branch)
                  skip = 1;
               insert_at++;
               continue;
            }
            emit_instruction(ctx, code, instr);
            assert(out[insert_at] == code[0]);
            insert_at += code.size();
            code.clear();
         }

         bld.reset(&ctx.program->blocks[next_block].instructions, it);
         skip_branch_target = next_block;
      } else {
         bld.reset(&ctx.program->blocks[insertion_block_idx].instructions);
         skip_branch_target = next_block;
      }

      if (ctx.program->gfx_level == GFX10) {
         emit_sopk_instruction(
            ctx, code, bld.sopk(aco_opcode::s_waitcnt_vscnt, Operand(sgpr_null, s1), 0).instr);
      }

      branch_instr = bld.sopp(aco_opcode::s_branch, 1).instr;
      emit_sopp_instruction(ctx, code, branch_instr, true);
   }
   assert(insert_at >= upper_start);
   const unsigned block_offset = insert_at + code.size();

   branch_instr = bld.sopp(aco_opcode::s_branch, 0);
   emit_sopp_instruction(ctx, code, branch_instr, true);
   insert_code(ctx, out, insert_at, code.size(), code.data());

   new_block->offset = block_offset;
   if (skip_branch_target) {
      ctx.branches.push_back({block_offset - 1, skip_branch_target});
   }
   ctx.branches.push_back({block_offset, target});
   assert(out[ctx.branches.back().pos] == code.back());
}

void
fix_branches(asm_context& ctx, std::vector<uint32_t>& out)
{
   const size_t max_iterations = 100;
   size_t iteration_count = 0;

   bool repeat = false;
   do {
      repeat = false;

      if (ctx.gfx_level == GFX10)
         fix_branches_gfx10(ctx, out);

      const size_t original_branch_count = ctx.branches.size();

      ctx.branches.reserve(original_branch_count * 3);

      for (size_t i = 0; i < original_branch_count; ++i) {
         branch_info& branch = ctx.branches[i];

         int offset = (int)ctx.program->blocks[branch.target].offset - branch.pos - 1;
         if (offset >= INT16_MIN && offset <= INT16_MAX) {
            out[branch.pos] &= 0xffff0000u;
            out[branch.pos] |= (uint16_t)offset;
         } else {
            chain_branches(ctx, out, branch);
            repeat = true;
            break;
         }
      }

      ++iteration_count;
      if (iteration_count > max_iterations) {
         aco_err(ctx.program, "Branch fixup failed to converge after %zu iterations (infinite loop detected)", max_iterations);
         abort();
      }
   } while (repeat);
}

void
fix_constaddrs(asm_context& ctx, std::vector<uint32_t>& out)
{
   for (auto& constaddr : ctx.constaddrs) {
      constaddr_info& info = constaddr.second;
      out[info.add_literal] += (out.size() - info.getpc_end) * 4u;

      if (ctx.symbols) {
         ctx.symbols->emplace_back(aco_symbol{aco_symbol_const_data_addr, info.add_literal});
      }
   }
   for (auto& addr : ctx.resumeaddrs) {
      constaddr_info& info = addr.second;
      const Block& block = ctx.program->blocks[out[info.add_literal]];
      assert(block.kind & block_kind_resume);
      out[info.add_literal] = (block.offset - info.getpc_end) * 4u;
   }
}

void
align_block(asm_context& ctx, std::vector<uint32_t>& code, Block& block)
{
   if (ctx.loop_header != -1u &&
       block.loop_nest_depth < ctx.program->blocks[ctx.loop_header].loop_nest_depth) {
      assert(ctx.loop_exit != -1u);
      Block& loop_header = ctx.program->blocks[ctx.loop_header];
      Block& loop_exit = ctx.program->blocks[ctx.loop_exit];
      ctx.loop_header = -1u;
      ctx.loop_exit = -1u;
      std::vector<uint32_t> nops;

      const unsigned loop_num_cl = DIV_ROUND_UP(block.offset - loop_header.offset, 16);

      const bool change_prefetch = ctx.program->gfx_level >= GFX10_3 &&
                                   ctx.program->gfx_level <= GFX11 && loop_num_cl > 1 &&
                                   loop_num_cl <= 3;

      if (change_prefetch) {
         Builder bld(ctx.program, &ctx.program->blocks[loop_header.linear_preds[0]]);
         int16_t prefetch_mode = loop_num_cl == 3 ? 0x1 : 0x2;
         Instruction* instr = bld.sopp(aco_opcode::s_inst_prefetch, prefetch_mode);
         emit_instruction(ctx, nops, instr);
         insert_code(ctx, code, loop_header.offset, nops.size(), nops.data());

         bld.reset(&loop_exit.instructions, loop_exit.instructions.begin());
         instr = bld.sopp(aco_opcode::s_inst_prefetch, 0x3);
         if (ctx.loop_exit < block.index) {
            nops.clear();
            emit_instruction(ctx, nops, instr);
            insert_code(ctx, code, loop_exit.offset, nops.size(), nops.data());
         }
      }

      const unsigned loop_start_cl = loop_header.offset >> 4;
      const unsigned loop_end_cl = (block.offset - 1) >> 4;

      const bool align_loop = loop_end_cl - loop_start_cl >= loop_num_cl &&
                              (loop_num_cl == 1 || change_prefetch || loop_header.offset % 16 > 8);

      if (align_loop) {
         nops.clear();
         nops.resize(16 - (loop_header.offset % 16), 0xbf800000u);
         insert_code(ctx, code, loop_header.offset, nops.size(), nops.data());
      }
   }

   if (block.kind & block_kind_loop_header) {
      if (block.linear_preds.size() > 1) {
         ctx.loop_header = block.index;
         ctx.loop_exit = -1u;
      }
   }

   if (ctx.loop_header != -1u && ctx.loop_exit == -1u) {
      for (uint32_t succ_idx : block.linear_succs) {
         Block& succ = ctx.program->blocks[succ_idx];
         if (succ.loop_nest_depth < ctx.program->blocks[ctx.loop_header].loop_nest_depth)
            ctx.loop_exit = succ_idx;
      }
   }

   if (block.kind & block_kind_resume) {
      size_t cache_aligned = align(code.size(), 16);
      code.resize(cache_aligned, 0xbf800000u);
      block.offset = code.size();
   }
}

unsigned
emit_program(Program* program, std::vector<uint32_t>& code, std::vector<struct aco_symbol>* symbols,
             bool append_endpgm)
{
   asm_context ctx(program, symbols);

   size_t estimated_insn_count = 0;
   size_t loop_count = 0;
   size_t branch_count = 0;

   for (const Block& block : program->blocks) {
      estimated_insn_count += block.instructions.size();
      if (block.kind & block_kind_loop_header)
         loop_count++;
      for (const aco_ptr<Instruction>& instr : block.instructions) {
         if (instr_info.classes[(int)instr->opcode] == instr_class::branch)
            branch_count++;
      }
   }

   const size_t base = (estimated_insn_count <= UINT32_MAX / 3)
                        ? (estimated_insn_count * 5) / 2
                        : (estimated_insn_count / 2) * 5;

   const size_t literal_overhead    = base / 5;
   const size_t modifier_overhead   = base / 20;
   const size_t nsa_overhead        = (base * 3) / 100;
   const size_t alignment_padding   = loop_count * 15u;

   const size_t cascade_depth = 3;
   const size_t chains_per_branch = (1u << cascade_depth);
   const size_t dwords_per_chain = 10u;
   const size_t branch_chaining = (branch_count / 10) * chains_per_branch * dwords_per_chain;

   const size_t constant_dwords     = (program->constant_data.size() + 3u) / 4u;
   const size_t endpgm_padding      = append_endpgm ? 5u : 0u;

   const size_t total_estimate = base + literal_overhead + modifier_overhead + nsa_overhead +
                                  alignment_padding + branch_chaining + constant_dwords + endpgm_padding;

   const size_t reserve_size = total_estimate + (total_estimate * 20u) / 100u;

   code.reserve(reserve_size);

   bool is_separately_compiled_ngg_vs_or_es =
      (program->stage.sw == SWStage::VS || program->stage.sw == SWStage::TES) &&
      program->stage.hw == AC_HW_NEXT_GEN_GEOMETRY_SHADER &&
      program->info.merged_shader_compiled_separately;

   if (!program->is_prolog && !program->info.ps.has_epilog &&
       !is_separately_compiled_ngg_vs_or_es &&
       (program->stage.hw == AC_HW_VERTEX_SHADER || program->stage.hw == AC_HW_PIXEL_SHADER ||
        program->stage.hw == AC_HW_NEXT_GEN_GEOMETRY_SHADER))
      fix_exports(ctx, code, program);

   for (Block& block : program->blocks) {
      block.offset = code.size();
      align_block(ctx, code, block);
      emit_block(ctx, code, block);
   }

   fix_branches(ctx, code);

   unsigned exec_size = code.size() * sizeof(uint32_t);

   if (append_endpgm)
      code.resize(code.size() + 5, 0xbf9f0000u);

   fix_constaddrs(ctx, code);

   while (program->constant_data.size() % 4u)
      program->constant_data.push_back(0);
   code.insert(code.end(), (uint32_t*)program->constant_data.data(),
               (uint32_t*)(program->constant_data.data() + program->constant_data.size()));

   program->config->scratch_bytes_per_wave =
      align(program->config->scratch_bytes_per_wave + program->scratch_arg_size,
            program->dev.scratch_alloc_granule);
   program->config->wgp_mode = program->wgp_mode;

   return exec_size;
}

}
