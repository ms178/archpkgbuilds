/*
 * Copyright © 2018 Valve Corporation
 * Copyright © 2018 Google
 *
 * SPDX-License-Identifier: MIT
 */

#include "aco_ir.h"

#include "ac_shader_util.h"

#define LIKELY(x) __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)

#ifndef ALWAYS_INLINE
# if defined(__GNUC__) || defined(__clang__)
# define ALWAYS_INLINE inline __attribute__((always_inline))
# else
# define ALWAYS_INLINE inline
# endif
#endif

static constexpr size_t VEGA_CACHE_LINE = 64;
static constexpr size_t PREFETCH_DISTANCE = 4;

namespace aco {

RegisterDemand
get_live_changes(Instruction* instr)
{
   RegisterDemand changes;
   const auto& defs = instr->definitions;
   const size_t nd = defs.size();
   if (LIKELY(nd == 1)) {
      const Definition& d = defs[0];
      if (LIKELY(d.isTemp() &&!d.isKill()))
         changes += d.getTemp();
   } else if (nd > 1) {
      for (const Definition& d : defs) {
         if (UNLIKELY(!d.isTemp() || d.isKill()))
            continue;
         changes += d.getTemp();
      }
   }

   const auto& ops = instr->operands;
   const size_t no = ops.size();
   if (LIKELY(no <= 2)) {
      if (no >= 1) {
         const Operand& o0 = ops[0];
         if (LIKELY(o0.isTemp() && o0.isFirstKill()))
            changes -= o0.getTemp();
      }
      if (no == 2) {
         const Operand& o1 = ops[1];
         if (LIKELY(o1.isTemp() && o1.isFirstKill()))
            changes -= o1.getTemp();
      }
   } else {
      for (const Operand& o : ops) {
         if (UNLIKELY(!o.isTemp() ||!o.isFirstKill()))
            continue;
         changes -= o.getTemp();
      }
   }
   return changes;
}

RegisterDemand
get_temp_registers(Instruction* instr)
{
   RegisterDemand demand_before;
   RegisterDemand demand_after;
   bool has_kill = false;
   for (const Definition& d : instr->definitions) {
      if (UNLIKELY(d.isKill())) {
         has_kill = true;
         break;
      }
   }
   if (LIKELY(!has_kill)) {
      for (const Definition& d : instr->definitions) {
         if (LIKELY(d.isTemp()))
            demand_before -= d.getTemp();
      }
   } else {
      for (const Definition& d : instr->definitions) {
         if (UNLIKELY(d.isKill()))
            demand_after += d.getTemp();
         else if (LIKELY(d.isTemp()))
            demand_before -= d.getTemp();
      }
   }

   for (const Operand& op : instr->operands) {
      if (LIKELY(op.isFirstKill() || op.isCopyKill())) {
         demand_before += op.getTemp();
         if (UNLIKELY(op.isLateKill()))
            demand_after += op.getTemp();
      } else if (UNLIKELY(op.isClobbered() &&!op.isKill())) {
         demand_before += op.getTemp();
      }
   }

   demand_after.update(demand_before);
   return demand_after;
}

RegisterDemand get_temp_reg_changes(Instruction* instr)
{
   RegisterDemand available_def_space;

   for (const Definition& def : instr->definitions) {
      if (LIKELY(def.isTemp()))
         available_def_space += def.getTemp();
   }

   for (const Operand& op : instr->operands) {
      if (op.isFirstKillBeforeDef() || (op.isCopyKill() &&!op.isLateKill()))
         available_def_space -= op.getTemp();
      else if (UNLIKELY(op.isClobbered() &&!op.isKill()))
         available_def_space -= op.getTemp();
   }

   return available_def_space;
}

namespace {

struct InstrCache {
   uint8_t format;
   uint8_t vcc;
   uint8_t special;
};

struct live_ctx {
   monotonic_buffer_resource m;
   Program* program;
   int32_t worklist;
   uint32_t handled_once;
   InstrCache* instr_cache = nullptr;
   size_t cache_capacity = 0;
};

ALWAYS_INLINE bool
instr_needs_vcc(Instruction* instr)
{
   if (UNLIKELY(instr->isVOPC()))
      return true;
   if (instr->isVOP2() &&!instr->isVOP3()) {
      if (UNLIKELY(instr->operands.size() == 3 && instr->operands[2].isTemp() &&
          instr->operands[2].regClass().type() == RegType::sgpr))
         return true;
      if (UNLIKELY(instr->definitions.size() == 2))
         return true;
   }
   return false;
}

ALWAYS_INLINE bool
instr_needs_vcc_cached(const InstrCache& c)
{
   if (c.vcc & 1)
      return true;
   if ((c.format & 6) == 2) {
      if ((c.vcc & 6) == 6)
         return true;
      if (c.vcc & 4)
         return true;
   }
   return false;
}

ALWAYS_INLINE void
cache_instruction(InstrCache& c, const Instruction* insn)
{
   uint8_t f = 0;
   if (insn->isVOPC()) f |= 1;
   if (insn->isVOP2()) f |= 2;
   if (insn->isVOP3()) f |= 4;
   if (insn->isVALU()) f |= 8;
   c.format = f;
   uint8_t v = 0;
   if (f & 1) v |= 1;
   if (insn->definitions.size() == 2) v |= 4;
   if (insn->operands.size() == 3 && insn->operands[2].isTemp() &&
       insn->operands[2].regClass().type() == RegType::sgpr)
      v |= 2;
   c.vcc = v;
   uint8_t s = 0;
   switch (insn->opcode) {
   case aco_opcode::v_addc_co_u32:
   case aco_opcode::v_subb_co_u32:
   case aco_opcode::v_subbrev_co_u32:
      s = 1; break;
   case aco_opcode::p_bpermute_readlane:
   case aco_opcode::p_bpermute_permlane:
   case aco_opcode::p_bpermute_shared_vgpr:
   case aco_opcode::p_dual_src_export_gfx11:
   case aco_opcode::v_mqsad_u32_u8:
      s = 2; break;
   case aco_opcode::p_interp_gfx11:
      s = 3; break;
   case aco_opcode::v_interp_p1_f32:
      s = 4; break;
   case aco_opcode::p_init_scratch:
   case aco_opcode::p_reload_preserved:
      s = 5; break;
   default:
      if (instr_info.classes[(int)insn->opcode] == instr_class::wmma)
         s = 6;
      break;
   }
   c.special = s;
}

IDSet
compute_live_out(live_ctx& ctx, Block* block)
{
   IDSet live(ctx.m);

   if (LIKELY(block->logical_succs.empty())) {
      for (unsigned succ : block->linear_succs) {
         __builtin_prefetch(&ctx.program->blocks[succ], 0, 1);
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

      if (UNLIKELY(block->logical_succs.back()!= block->linear_succs.back())) {
         for (unsigned t : ctx.program->live.live_in[block->logical_succs.back()]) {
            if (LIKELY(!ctx.program->temp_rc[t].is_linear()))
               live.insert(t);
         }
      } else {
         assert(block->logical_succs[0] == block->linear_succs[0]);
      }
   }

   if (LIKELY(block->linear_succs.size() == 1) && block->linear_succs[0] >= ctx.handled_once) {
      Block& succ = ctx.program->blocks[block->linear_succs[0]];
      auto it = std::find(succ.linear_preds.begin(), succ.linear_preds.end(), block->index);
      unsigned op_idx = std::distance(succ.linear_preds.begin(), it);
      for (aco_ptr<Instruction>& phi : succ.instructions) {
         if (UNLIKELY(!is_phi(phi)))
            break;
         if (UNLIKELY(phi->opcode == aco_opcode::p_phi || phi->definitions[0].isKill()))
            continue;
         if (op_idx < phi->operands.size() && LIKELY(phi->operands[op_idx].isTemp()))
            live.insert(phi->operands[op_idx].tempId());
      }
   }
   if (UNLIKELY(block->logical_succs.size() == 1) && block->logical_succs[0] >= ctx.handled_once) {
      Block& succ = ctx.program->blocks[block->logical_succs[0]];
      auto it = std::find(succ.logical_preds.begin(), succ.logical_preds.end(), block->index);
      unsigned op_idx = std::distance(succ.logical_preds.begin(), it);
      for (aco_ptr<Instruction>& phi : succ.instructions) {
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

template <typename T>
ALWAYS_INLINE RegisterDemand
get_demand_for_reg(live_ctx& ctx, T op_or_def)
{
   if (!op_or_def.isPrecolored())
      return RegisterDemand();

   PhysReg reg = op_or_def.physReg();
   RegType type = op_or_def.regClass().type();

   if (type == RegType::sgpr && reg >= ctx.program->dev.sgpr_limit)
      return RegisterDemand();

   PhysReg max_reg = reg.advance(op_or_def.regClass().bytes());

   if (type == RegType::sgpr)
      return RegisterDemand(0, max_reg);
   else
      return RegisterDemand(max_reg - 256, 0);
}

void
process_live_temps_per_block(live_ctx& ctx, Block* block)
{
   ctx.m.release_reallocate();
   ctx.instr_cache = nullptr;
   ctx.cache_capacity = 0;

   const size_t num_instrs = block->instructions.size();
   if (LIKELY(num_instrs && num_instrs <= 512 && num_instrs > ctx.cache_capacity)) {
      ctx.cache_capacity = (num_instrs + 15) & ~size_t(15);
      ctx.instr_cache = (InstrCache*)ctx.m.allocate(ctx.cache_capacity * sizeof(InstrCache), alignof(InstrCache));
   }
   if (LIKELY(ctx.instr_cache && num_instrs <= 512)) {
      for (size_t i = 0; i < num_instrs; ++i) {
         if (LIKELY(i + PREFETCH_DISTANCE < num_instrs))
            __builtin_prefetch(block->instructions[i + PREFETCH_DISTANCE].get(), 0, 1);
         cache_instruction(ctx.instr_cache[i], block->instructions[i].get());
      }
   }

   RegisterDemand new_demand;
   unsigned num_linear_vgprs = 0;
   block->register_demand = RegisterDemand();
   block->call_spills = RegisterDemand();
   IDSet live = compute_live_out(ctx, block);

   for (unsigned t : live) {
      new_demand += Temp(t, ctx.program->temp_rc[t]);
      if (ctx.program->temp_rc[t].is_linear_vgpr())
         num_linear_vgprs += ctx.program->temp_rc[t].size();
   }

   int idx;
   for (idx = (int)block->instructions.size() - 1; idx >= 0; idx--) {
      if (LIKELY(idx >= (int)PREFETCH_DISTANCE))
         __builtin_prefetch(block->instructions[idx - PREFETCH_DISTANCE].get(), 0, 1);
      Instruction* insn = block->instructions[idx].get();
      if (UNLIKELY(is_phi(insn)))
         break;

      if (UNLIKELY(insn->hasPrecoloredGPRs())) {
         RegisterDemand precolored_demand = RegisterDemand();
         for (Operand op : insn->operands)
            precolored_demand.update(get_demand_for_reg(ctx, op));
         for (Definition def : insn->definitions)
            precolored_demand.update(get_demand_for_reg(ctx, def));
         ctx.program->fixed_reg_demand.update(precolored_demand);
      }

      if (LIKELY(ctx.instr_cache && idx >= 0 && (size_t)idx < ctx.cache_capacity && block->instructions.size() <= 512))
         ctx.program->needs_vcc |= instr_needs_vcc_cached(ctx.instr_cache[idx]);
      else
         ctx.program->needs_vcc |= instr_needs_vcc(insn);
      RegisterDemand demand_after_instr = RegisterDemand(new_demand.vgpr, new_demand.sgpr);
      insn->register_demand = demand_after_instr;

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
            if (temp.regClass().is_linear_vgpr())
               num_linear_vgprs -= temp.size();
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
            op.hasRegClass() && op.regClass().is_linear_vgpr() &&!op.isUndefined() && has_vgpr_def;

         lateKill |= is_vector_op || op.isVectorAligned();
         op.setLateKill(lateKill);
         is_vector_op = op.isVectorAligned();
      }

      if (ctx.program->gfx_level >= GFX10 && insn->isVALU() &&
          insn->definitions.back().regClass() == s2) {
         bool carry_in = insn->opcode == aco_opcode::v_addc_co_u32 ||
                         insn->opcode == aco_opcode::v_subb_co_u32 ||
                         insn->opcode == aco_opcode::v_subbrev_co_u32;
         for (unsigned op_idx = 0; op_idx < (carry_in? 2 : insn->operands.size()); op_idx++) {
            if (insn->operands[op_idx].isOfType(RegType::sgpr))
               insn->operands[op_idx].setLateKill(true);
         }
      } else if (insn->opcode == aco_opcode::p_bpermute_readlane ||
                 insn->opcode == aco_opcode::p_bpermute_permlane ||
                 insn->opcode == aco_opcode::p_bpermute_shared_vgpr ||
                 insn->opcode == aco_opcode::p_dual_src_export_gfx11 ||
                 insn->opcode == aco_opcode::v_mqsad_u32_u8) {
         for (Operand& op : insn->operands)
            op.setLateKill(true);
      } else if (insn->opcode == aco_opcode::p_interp_gfx11 && insn->operands.size() == 7) {
         insn->operands[5].setLateKill(true);
      } else if (insn->opcode == aco_opcode::v_interp_p1_f32 && ctx.program->dev.has_16bank_lds) {
         insn->operands[0].setLateKill(true);
      } else if (insn->opcode == aco_opcode::p_init_scratch ||
                 insn->opcode == aco_opcode::p_reload_preserved) {
         insn->operands.back().setLateKill(true);
      } else if (instr_info.classes[(int)insn->opcode] == instr_class::wmma) {
         insn->operands[0].setLateKill(true);
         insn->operands[1].setLateKill(true);
      }

      RegisterDemand operand_demand;
      auto tied_defs = get_tied_defs(insn);
      for (auto op_idx : tied_defs) {
         Temp tmp = insn->operands[op_idx].getTemp();
         if (std::any_of(tied_defs.begin(), tied_defs.end(), [&](uint32_t i)
                         { return i < op_idx && insn->operands[i].getTemp() == tmp; })) {
            operand_demand += tmp;
            insn->operands[op_idx].setCopyKill(true);
         }
         insn->operands[op_idx].setClobbered(true);

         if (insn->operands[op_idx].isVectorAligned())
            insn->operands[op_idx].setLateKill(false);
         while (insn->operands[op_idx].isVectorAligned()) {
            ++op_idx;
            insn->operands[op_idx].setClobbered(true);
            insn->operands[op_idx].setLateKill(false);
         }
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
                            [=](Definition def)
                            {
                               return def.isFixed() &&
                                      def.physReg() + def.size() > operand.physReg() &&
                                      operand.physReg() + operand.size() > def.physReg();
                            })))
               operand.setClobbered(true);

            for (unsigned j = i + 1;!operand.isCopyKill() && j < insn->operands.size(); ++j) {
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
         is_vector_op = operand.isVectorAligned();

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
            if (temp.regClass().is_linear_vgpr())
               num_linear_vgprs += temp.size();
         } else if (UNLIKELY(operand.isClobbered())) {
            operand_demand += temp;
         }
      }

      if (insn->isCall()) {
         RegisterDemand limit = get_addr_regs_from_waves(ctx.program, ctx.program->min_waves);
         insn->call().callee_preserved_limit = insn->call().abi.numPreserved(limit);

         BITSET_DECLARE(preserved_regs, 512);
         insn->call().abi.preservedRegisters(preserved_regs, limit);

         for (auto& op : insn->operands) {
            if (!op.isTemp() ||!op.isPrecolored() ||!op.isKill())
               continue;

            for (unsigned i = 0; i < op.size(); ++i) {
               if (BITSET_TEST(preserved_regs, op.physReg().reg() + i))
                  insn->call().callee_preserved_limit -= Temp(0, RegClass(op.regClass().type(), 1));
            }
         }

         insn->call().callee_preserved_limit.vgpr =
            MAX2(insn->call().callee_preserved_limit.vgpr - (int16_t)num_linear_vgprs, 0);

         insn->call().caller_preserved_demand = demand_after_instr;
         insn->call().caller_preserved_demand.vgpr -= num_linear_vgprs;

         for (auto& op : insn->operands) {
            if (!op.isTemp() ||!op.isPrecolored() || op.isClobbered() || op.isKill())
               continue;

            for (unsigned i = 0; i < op.size(); ++i) {
               if (!BITSET_TEST(preserved_regs, op.physReg().reg() + i))
                  insn->call().caller_preserved_demand -=
                     Temp(0, RegClass(op.regClass().type(), 1));
            }
         }

         for (unsigned i = 0; i < insn->definitions.size(); ++i) {
            if (!insn->definitions[i].isKill())
               insn->call().caller_preserved_demand -= insn->definitions[i].getTemp();
         }

         block->call_spills.update(insn->call().caller_preserved_demand -
                                   insn->call().callee_preserved_limit);
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
      ctx.program->needs_vcc |= definition.isFixed() && definition.physReg() == vcc;
      const size_t n = live.erase(definition.tempId());
      if (n && (definition.isKill() || ctx.handled_once > block->index)) {
         Block::edge_vec& preds =
            insn->opcode == aco_opcode::p_phi? block->logical_preds : block->linear_preds;
         for (unsigned i = 0; i < preds.size(); i++) {
            if (insn->operands[i].isTemp())
               ctx.worklist = std::max<int>(ctx.worklist, preds[i]);
         }
      }
      definition.setKill(!n);
   }

   for (int phi_idx = 0; phi_idx <= idx; phi_idx++) {
      Instruction* insn = block->instructions[phi_idx].get();
      assert(is_phi(insn));
      if (insn->definitions[0].isKill())
         continue;
      for (Operand& operand : insn->operands) {
         if (!operand.isTemp())
            continue;
         operand.setKill(!live.count(operand.tempId()));
      }
   }

   if (ctx.program->live.live_in[block->index].insert(live)) {
      if (block->linear_preds.size()) {
         assert(block->logical_preds.empty() ||
                block->logical_preds.back() <= block->linear_preds.back());
         ctx.worklist = std::max<int>(ctx.worklist, block->linear_preds.back());
      } else {
         ASSERTED bool is_valid = validate_ir(ctx.program);
         assert(!is_valid);
      }
   }

   block->live_in_demand = new_demand;
   block->register_demand.update(block->live_in_demand);
   ctx.program->max_reg_demand.update(block->register_demand);
   ctx.program->max_call_spills.update(block->call_spills);
   ctx.handled_once = std::min(ctx.handled_once, block->index);

   assert(!block->linear_preds.empty() || (new_demand == RegisterDemand() && live.empty()));
}

unsigned
calc_waves_per_workgroup(Program* program)
{
   unsigned workgroup_size =
      program->workgroup_size == UINT_MAX? program->wave_size : program->workgroup_size;

   return align(workgroup_size, program->wave_size) / program->wave_size;
}
} /* end namespace */

bool
uses_scratch(Program* program)
{
   return program->config->scratch_bytes_per_wave || program->stage == raytracing_cs;
}

uint16_t
get_extra_sgprs(Program* program)
{
   bool needs_flat_scr = uses_scratch(program) && program->gfx_level == GFX9;

   if (program->gfx_level >= GFX10) {
      assert(!program->dev.xnack_enabled);
      return 0;
   } else if (program->gfx_level >= GFX8) {
      if (needs_flat_scr)
         return 6;
      else if (program->dev.xnack_enabled)
         return 4;
      else if (program->needs_vcc)
         return 2;
      else
         return 0;
   } else {
      assert(!program->dev.xnack_enabled);
      if (needs_flat_scr)
         return 4;
      else if (program->needs_vcc)
         return 2;
      else
         return 0;
   }
}

uint16_t
get_sgpr_alloc(Program* program, uint16_t addressable_sgprs)
{
   uint16_t sgprs = addressable_sgprs + get_extra_sgprs(program);
   uint16_t granule = program->dev.sgpr_alloc_granule;
   return ALIGN_NPOT(std::max(sgprs, granule), granule);
}

uint16_t
get_vgpr_alloc(Program* program, uint16_t addressable_vgprs)
{
   assert(addressable_vgprs <= program->dev.vgpr_limit);
   uint16_t granule = program->dev.vgpr_alloc_granule;
   return ALIGN_NPOT(std::max(addressable_vgprs, granule), granule);
}

unsigned
round_down(unsigned a, unsigned b)
{
   return a - (a % b);
}

RegisterDemand
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
   unsigned simd_per_cu_wgp = program->dev.simd_per_cu * (program->wgp_mode? 2 : 1);
   program->min_waves = DIV_ROUND_UP(waves_per_workgroup, simd_per_cu_wgp);
}

uint16_t
max_suitable_waves(Program* program, uint16_t waves)
{
   unsigned num_simd = program->dev.simd_per_cu * (program->wgp_mode? 2 : 1);
   unsigned waves_per_workgroup = calc_waves_per_workgroup(program);
   unsigned num_workgroups = waves * num_simd / waves_per_workgroup;

   unsigned lds_increment = ac_shader_get_lds_alloc_granularity(program->gfx_level);
   unsigned lds_per_workgroup = align(program->config->lds_size, lds_increment);

   if (program->stage == fragment_fs) {
      unsigned lds_bytes_per_interp = 3 * 16;
      unsigned lds_param_bytes = lds_bytes_per_interp * program->info.ps.num_inputs;
      lds_per_workgroup += align(lds_param_bytes, lds_increment);
   }
   unsigned lds_limit = program->wgp_mode? program->dev.lds_limit * 2 : program->dev.lds_limit;
   if (lds_per_workgroup)
      num_workgroups = std::min(num_workgroups, lds_limit / lds_per_workgroup);

   if (waves_per_workgroup > 1)
      num_workgroups = std::min(num_workgroups, program->wgp_mode? 32u : 16u);

   unsigned workgroup_waves = num_workgroups * waves_per_workgroup;
   return DIV_ROUND_UP(workgroup_waves, num_simd);
}

void
update_vgpr_sgpr_demand(Program* program, RegisterDemand new_demand)
{
   assert(program->min_waves >= 1);
   RegisterDemand limit = get_addr_regs_from_waves(program, program->min_waves);

   if (new_demand.exceeds(limit) || program->max_call_spills!= RegisterDemand()) {
      program->num_waves = 0;
      program->max_reg_demand = new_demand;
   } else {
      RegisterDemand temp_demand = new_demand;
      new_demand.update(program->fixed_reg_demand);

      program->num_waves = program->dev.physical_sgprs / get_sgpr_alloc(program, new_demand.sgpr);
      uint16_t vgpr_demand =
         get_vgpr_alloc(program, new_demand.vgpr) + program->config->num_shared_vgprs / 2;
      program->num_waves =
         std::min<uint16_t>(program->num_waves, program->dev.physical_vgprs / vgpr_demand);
      program->num_waves = std::min(program->num_waves, program->dev.max_waves_per_simd);

      program->num_waves = max_suitable_waves(program, program->num_waves);
      if (program->is_callee) {
         std::pair<int, unsigned> best(INT_MIN, program->num_waves);
         for (; program->num_waves > program->min_waves; program->num_waves--) {
            program->max_reg_demand = get_addr_regs_from_waves(program, program->num_waves);
            RegisterDemand clobbered = program->callee_abi.numClobbered(program->max_reg_demand);
            std::pair<int, unsigned> val(MIN2(clobbered.vgpr - temp_demand.vgpr, 0),
                                         program->num_waves);
            if (val > best)
               best = val;
         }
         program->num_waves = best.second;
      }
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
   program->max_call_spills = RegisterDemand();
   program->fixed_reg_demand = RegisterDemand();
   program->needs_vcc = program->gfx_level >= GFX10;

   live_ctx ctx;
   ctx.program = program;
   ctx.worklist = program->blocks.size() - 1;
   ctx.handled_once = program->blocks.size();

   while (LIKELY(ctx.worklist >= 0)) {
      process_live_temps_per_block(ctx, &program->blocks[ctx.worklist--]);
   }

   if (program->progress < CompilationProgress::after_ra)
      update_vgpr_sgpr_demand(program, program->max_reg_demand);
}

} // namespace aco
