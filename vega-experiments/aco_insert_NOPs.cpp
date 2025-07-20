/*
 * Copyright © 2019 Valve Corporation
 *
 * SPDX-License-Identifier: MIT
 */

#include "aco_builder.h"
#include "aco_ir.h"

#include "util/bitset.h"

#include <algorithm>
#include <bitset>
#include <set>
#include <stack>
#include <vector>

namespace aco {
namespace {

struct State {
   Program* program;
   Block* block;
   std::vector<aco_ptr<Instruction>> old_instructions;
};

struct NOP_ctx_gfx6 {
   void join(const NOP_ctx_gfx6& o) {
      set_vskip_mode_then_vector = MAX2(set_vskip_mode_then_vector, o.set_vskip_mode_then_vector);
      valu_wr_vcc_then_div_fmas = MAX2(valu_wr_vcc_then_div_fmas, o.valu_wr_vcc_then_div_fmas);
      salu_wr_m0_then_gds_msg_ttrace = MAX2(salu_wr_m0_then_gds_msg_ttrace, o.salu_wr_m0_then_gds_msg_ttrace);
      valu_wr_exec_then_dpp = MAX2(valu_wr_exec_then_dpp, o.valu_wr_exec_then_dpp);
      salu_wr_m0_then_lds = MAX2(salu_wr_m0_then_lds, o.salu_wr_m0_then_lds);
      salu_wr_m0_then_moverel = MAX2(salu_wr_m0_then_moverel, o.salu_wr_m0_then_moverel);
      setreg_then_getsetreg = MAX2(setreg_then_getsetreg, o.setreg_then_getsetreg);
      vcc_wr_then_branch = MAX2(vcc_wr_then_branch, o.vcc_wr_then_branch);

      vmem_store_then_wr_data |= o.vmem_store_then_wr_data;

      smem_clause |= o.smem_clause;
      smem_write |= o.smem_write;

      for (unsigned i = 0; i < BITSET_WORDS(128); ++i) {
         smem_clause_read_write[i] |= o.smem_clause_read_write[i];
         smem_clause_write[i] |= o.smem_clause_write[i];
      }
   }

   bool operator==(const NOP_ctx_gfx6& o) const {
      return set_vskip_mode_then_vector == o.set_vskip_mode_then_vector &&
             valu_wr_vcc_then_div_fmas == o.valu_wr_vcc_then_div_fmas &&
             salu_wr_m0_then_gds_msg_ttrace == o.salu_wr_m0_then_gds_msg_ttrace &&
             valu_wr_exec_then_dpp == o.valu_wr_exec_then_dpp &&
             salu_wr_m0_then_lds == o.salu_wr_m0_then_lds &&
             salu_wr_m0_then_moverel == o.salu_wr_m0_then_moverel &&
             setreg_then_getsetreg == o.setreg_then_getsetreg &&
             vcc_wr_then_branch == o.vcc_wr_then_branch &&
             vmem_store_then_wr_data == o.vmem_store_then_wr_data &&
             smem_clause == o.smem_clause &&
             smem_write == o.smem_write &&
             BITSET_EQUAL(smem_clause_read_write, o.smem_clause_read_write) &&
             BITSET_EQUAL(smem_clause_write, o.smem_clause_write);
   }

   void add_wait_states(unsigned amount) {
      /* saturating decrement helper – keeps signedness well-defined */
      auto saturating_sub = [amount](int8_t& ctr) noexcept {
         ctr = (ctr > static_cast<int8_t>(amount)) ? static_cast<int8_t>(ctr - amount) : 0;
      };

      saturating_sub(set_vskip_mode_then_vector);
      saturating_sub(valu_wr_vcc_then_div_fmas);
      saturating_sub(salu_wr_m0_then_gds_msg_ttrace);
      saturating_sub(valu_wr_exec_then_dpp);
      saturating_sub(salu_wr_m0_then_lds);
      saturating_sub(salu_wr_m0_then_moverel);
      saturating_sub(setreg_then_getsetreg);
      saturating_sub(vcc_wr_then_branch);

      vmem_store_then_wr_data.reset();
   }

   int8_t set_vskip_mode_then_vector = 0;     /* MODE.vskip → vector op  (2) */
   int8_t valu_wr_vcc_then_div_fmas = 0;      /* VALU→VCC  → v_div_fmas  (4) */
   int8_t salu_wr_m0_then_gds_msg_ttrace = 0; /* M0 write  → GDS/sendmsg (1) */
   int8_t valu_wr_exec_then_dpp = 0;          /* VALU→EXEC → DPP         (5) */
   int8_t salu_wr_m0_then_lds = 0;            /* GFX9+ M0 write → LDS    (1) */
   int8_t salu_wr_m0_then_moverel = 0;        /* GFX9   M0 write → moverel(1)*/
   int8_t setreg_then_getsetreg = 0;          /* s_setreg → s_get/setreg (2) */
   int8_t vcc_wr_then_branch = 0;             /* NEW: SALU→VCC → branch  (2) */
   uint8_t current_vskip = 0;

   std::bitset<256> vmem_store_then_wr_data;

   bool smem_clause = false;
   bool smem_write = false;
   BITSET_DECLARE(smem_clause_read_write, 128) = {0};
   BITSET_DECLARE(smem_clause_write, 128) = {0};
};

struct NOP_ctx_gfx10 {
   bool has_VOPC_write_exec = false;
   bool has_nonVALU_exec_read = false;
   bool has_VMEM = false;
   bool has_branch_after_VMEM = false;
   bool has_DS = false;
   bool has_branch_after_DS = false;
   bool has_NSA_MIMG = false;
   bool has_writelane = false;
   std::bitset<128> sgprs_read_by_VMEM;
   std::bitset<128> sgprs_read_by_VMEM_store;
   std::bitset<128> sgprs_read_by_DS;
   std::bitset<128> sgprs_read_by_SMEM;

   void join(const NOP_ctx_gfx10& other)
   {
      has_VOPC_write_exec |= other.has_VOPC_write_exec;
      has_nonVALU_exec_read |= other.has_nonVALU_exec_read;
      has_VMEM |= other.has_VMEM;
      has_branch_after_VMEM |= other.has_branch_after_VMEM;
      has_DS |= other.has_DS;
      has_branch_after_DS |= other.has_branch_after_DS;
      has_NSA_MIMG |= other.has_NSA_MIMG;
      has_writelane |= other.has_writelane;
      sgprs_read_by_VMEM |= other.sgprs_read_by_VMEM;
      sgprs_read_by_DS |= other.sgprs_read_by_DS;
      sgprs_read_by_VMEM_store |= other.sgprs_read_by_VMEM_store;
      sgprs_read_by_SMEM |= other.sgprs_read_by_SMEM;
   }

   bool operator==(const NOP_ctx_gfx10& other)
   {
      return has_VOPC_write_exec == other.has_VOPC_write_exec &&
             has_nonVALU_exec_read == other.has_nonVALU_exec_read && has_VMEM == other.has_VMEM &&
             has_branch_after_VMEM == other.has_branch_after_VMEM && has_DS == other.has_DS &&
             has_branch_after_DS == other.has_branch_after_DS &&
             has_NSA_MIMG == other.has_NSA_MIMG && has_writelane == other.has_writelane &&
             sgprs_read_by_VMEM == other.sgprs_read_by_VMEM &&
             sgprs_read_by_DS == other.sgprs_read_by_DS &&
             sgprs_read_by_VMEM_store == other.sgprs_read_by_VMEM_store &&
             sgprs_read_by_SMEM == other.sgprs_read_by_SMEM;
   }
};

template <int Max> struct RegCounterMap {
   void inc() { base++; }
   void set(PhysReg reg) { update(reg, 0); }

   uint8_t get(PhysReg reg)
   {
      if (present.test(reg.reg() & 0x7F)) {
         for (entry& e : list) {
            if (e.reg == reg.reg())
               return MIN2(base - e.val, Max);
         }
      }
      return Max;
   }

   void reset()
   {
      present.reset();
      list.clear();
      base = 0;
   }

   bool empty()
   {
      for (entry& e : list) {
         if (base - e.val < Max)
            return false;
      }
      return true;
   }

   void join_min(const RegCounterMap& other)
   {
      for (const entry& e : other.list) {
         int idx = other.base - e.val;
         if (idx >= Max)
            continue;

         update(e.reg, idx);
      }
   }

   void update(uint16_t reg, int idx)
   {
         int16_t val = base - idx;
         uint32_t key = reg & 0x7F;

         /* fast path: already present */
         if (present.test(key)) {
               for (entry& e : list) {
                     if (e.reg == reg) {
                           e.val = MAX2(e.val, val);
                           return;
                     }
               }
         }
         /* slow path: insert */
         list.push_back(entry{reg, val});
         present.set(key);
   }

   bool operator==(const RegCounterMap& other) const
   {
      /* Two maps with different bases could also be equal, but for our use case,
       * i.e. checking for changes at loop headers, this is sufficient since we
       * always join the predecessors into an empty map with base=0.
       */
      return base == other.base && list == other.list;
   }

private:
   struct entry {
      uint16_t reg;
      int16_t val;
      bool operator!=(const entry& other) const { return reg != other.reg || val != other.val; }
   };

   std::bitset<128> present;
   small_vec<entry, 4> list;
   int base = 0;
};

struct NOP_ctx_gfx11 {
   /* VcmpxPermlaneHazard */
   bool has_Vcmpx = false;

   /* LdsDirectVMEMHazard */
   std::bitset<256> vgpr_used_by_vmem_load;
   std::bitset<256> vgpr_used_by_vmem_sample;
   std::bitset<256> vgpr_used_by_vmem_bvh;
   std::bitset<256> vgpr_used_by_vmem_store;
   std::bitset<256> vgpr_used_by_ds;

   /* VALUTransUseHazard */
   RegCounterMap<6> valu_since_wr_by_trans;
   RegCounterMap<2> trans_since_wr_by_trans;

   /* VALUMaskWriteHazard */
   std::bitset<128> sgpr_read_by_valu_as_lanemask;
   std::bitset<128> sgpr_read_by_valu_as_lanemask_then_wr_by_salu;
   std::bitset<128> sgpr_read_by_valu_as_lanemask_then_wr_by_valu;

   /* WMMAHazards */
   std::bitset<256> vgpr_written_by_wmma;

   /* VALUReadSGPRHazard */
   std::bitset<m0.reg() / 2> sgpr_read_by_valu; /* SGPR pairs, excluding null, exec, m0 and scc */
   std::bitset<m0.reg()> sgpr_read_by_valu_then_wr_by_valu;
   RegCounterMap<11> sgpr_read_by_valu_then_wr_by_salu;

   void join(const NOP_ctx_gfx11& other)
   {
      has_Vcmpx |= other.has_Vcmpx;
      vgpr_used_by_vmem_load |= other.vgpr_used_by_vmem_load;
      vgpr_used_by_vmem_sample |= other.vgpr_used_by_vmem_sample;
      vgpr_used_by_vmem_bvh |= other.vgpr_used_by_vmem_bvh;
      vgpr_used_by_vmem_store |= other.vgpr_used_by_vmem_store;
      vgpr_used_by_ds |= other.vgpr_used_by_ds;
      valu_since_wr_by_trans.join_min(other.valu_since_wr_by_trans);
      trans_since_wr_by_trans.join_min(other.trans_since_wr_by_trans);
      sgpr_read_by_valu_as_lanemask |= other.sgpr_read_by_valu_as_lanemask;
      sgpr_read_by_valu_as_lanemask_then_wr_by_salu |=
         other.sgpr_read_by_valu_as_lanemask_then_wr_by_salu;
      sgpr_read_by_valu_as_lanemask_then_wr_by_valu |=
         other.sgpr_read_by_valu_as_lanemask_then_wr_by_valu;
      vgpr_written_by_wmma |= other.vgpr_written_by_wmma;
      sgpr_read_by_valu |= other.sgpr_read_by_valu;
      sgpr_read_by_valu_then_wr_by_valu |= other.sgpr_read_by_valu_then_wr_by_valu;
      sgpr_read_by_valu_then_wr_by_salu.join_min(other.sgpr_read_by_valu_then_wr_by_salu);
   }

   bool operator==(const NOP_ctx_gfx11& other)
   {
      return has_Vcmpx == other.has_Vcmpx &&
             vgpr_used_by_vmem_load == other.vgpr_used_by_vmem_load &&
             vgpr_used_by_vmem_sample == other.vgpr_used_by_vmem_sample &&
             vgpr_used_by_vmem_bvh == other.vgpr_used_by_vmem_bvh &&
             vgpr_used_by_vmem_store == other.vgpr_used_by_vmem_store &&
             vgpr_used_by_ds == other.vgpr_used_by_ds &&
             valu_since_wr_by_trans == other.valu_since_wr_by_trans &&
             trans_since_wr_by_trans == other.trans_since_wr_by_trans &&
             sgpr_read_by_valu_as_lanemask == other.sgpr_read_by_valu_as_lanemask &&
             sgpr_read_by_valu_as_lanemask_then_wr_by_salu ==
                other.sgpr_read_by_valu_as_lanemask_then_wr_by_salu &&
             sgpr_read_by_valu_as_lanemask_then_wr_by_valu ==
                other.sgpr_read_by_valu_as_lanemask_then_wr_by_valu &&
             vgpr_written_by_wmma == other.vgpr_written_by_wmma &&
             sgpr_read_by_valu == other.sgpr_read_by_valu &&
             sgpr_read_by_valu_then_wr_by_salu == other.sgpr_read_by_valu_then_wr_by_salu;
   }
};

int
get_wait_states(aco_ptr<Instruction>& instr)
{
   if (instr->opcode == aco_opcode::s_nop)
      return instr->salu().imm + 1;
   else if (instr->opcode == aco_opcode::p_constaddr)
      return 3; /* lowered to 3 instructions in the assembler */
   else
      return 1;
}

bool
regs_intersect(PhysReg a_reg, unsigned a_size,
               PhysReg b_reg, unsigned b_size)
{
      if (a_reg > b_reg) [[likely]]
            return a_reg - b_reg < b_size;
      else
            return b_reg - a_reg < a_size;
}

static inline bool
is_vcc_branch(const aco_ptr<Instruction>& instr)
{
      switch (instr->opcode) {
            case aco_opcode::s_cbranch_vccz:
            case aco_opcode::s_cbranch_vccnz:
                  return true;
            default:
                  return false;
      }
}

template <typename GlobalState, typename BlockState,
          bool (*block_cb)(GlobalState&, BlockState&, Block*),
          bool (*instr_cb)(GlobalState&, BlockState&, aco_ptr<Instruction>&)>
void
search_backwards_internal(State& state, GlobalState& global_state, BlockState block_state,
                          Block* block, bool start_at_end)
{
   if (block == state.block && start_at_end) {
      /* If it's the current block, block->instructions is incomplete. */
      for (int pred_idx = state.old_instructions.size() - 1; pred_idx >= 0; pred_idx--) {
         aco_ptr<Instruction>& instr = state.old_instructions[pred_idx];
         if (!instr)
            break; /* Instruction has been moved to block->instructions. */
         if (instr_cb(global_state, block_state, instr))
            return;
      }
   }

   for (int pred_idx = block->instructions.size() - 1; pred_idx >= 0; pred_idx--) {
      if (instr_cb(global_state, block_state, block->instructions[pred_idx]))
         return;
   }

   PRAGMA_DIAGNOSTIC_PUSH
   PRAGMA_DIAGNOSTIC_IGNORED(-Waddress)
   if (block_cb != nullptr && !block_cb(global_state, block_state, block))
      return;
   PRAGMA_DIAGNOSTIC_POP

   for (unsigned lin_pred : block->linear_preds) {
      search_backwards_internal<GlobalState, BlockState, block_cb, instr_cb>(
         state, global_state, block_state, &state.program->blocks[lin_pred], true);
   }
}

template <typename GlobalState, typename BlockState,
          bool (*block_cb)(GlobalState&, BlockState&, Block*),
          bool (*instr_cb)(GlobalState&, BlockState&, aco_ptr<Instruction>&)>
void
search_backwards(State& state, GlobalState& global_state, BlockState& block_state)
{
   search_backwards_internal<GlobalState, BlockState, block_cb, instr_cb>(
      state, global_state, block_state, state.block, false);
}

struct HandleRawHazardGlobalState {
   PhysReg reg;
   int nops_needed;
};

struct HandleRawHazardBlockState {
   uint32_t mask;
   int nops_needed;
};

template <bool Valu, bool Vintrp, bool Salu>
static bool
handle_raw_hazard_instr(HandleRawHazardGlobalState& global_state,
                        HandleRawHazardBlockState&  block_state,
                        aco_ptr<Instruction>&       pred)
{
   const unsigned mask_size = util_last_bit(block_state.mask);

   uint32_t writemask = 0u;
   for (const Definition& def : pred->definitions) {
      if (regs_intersect(global_state.reg, mask_size,
                         def.physReg(), def.size()))
      {
         const unsigned start = (def.physReg() > global_state.reg) ?
                                (def.physReg() - global_state.reg) : 0u;
         const unsigned end   = std::min(mask_size, start + def.size());
         writemask           |= u_bit_consecutive(start, end - start);
      }
   }

   const bool relevant_pred =
      ((pred->isVALU()   && Valu)   ||
       (pred->isVINTRP() && Vintrp) ||
       (pred->isSALU()   && Salu));

   /* RAW hazard found – record and stop. */
   if (writemask && relevant_pred) {
      global_state.nops_needed =
         std::max(global_state.nops_needed, block_state.nops_needed);
      return true;                               /* EARLY-EXIT on conflict  */
   }

   /* age the counters for wait-states already consumed by this pred */
   block_state.mask         &= ~writemask;
   block_state.nops_needed   = std::max(
                                 block_state.nops_needed -
                                 static_cast<int>(get_wait_states(pred)), 0);

   /* If nothing left to cover OR we already satisfied wait distance – done */
   if (block_state.mask == 0u || block_state.nops_needed == 0)
      return true;

   /* continue walking */
   return false;
}

template <bool Valu, bool Vintrp, bool Salu>
void
handle_raw_hazard(State& state, int* NOPs, int min_states, Operand op)
{
   if (*NOPs >= min_states)
      return;

   HandleRawHazardGlobalState global = {op.physReg(), 0};
   HandleRawHazardBlockState block = {u_bit_consecutive(0, op.size()), min_states};

   /* Loops require branch instructions, which count towards the wait
    * states. So even with loops this should finish unless nops_needed is some
    * huge value. */
   search_backwards<HandleRawHazardGlobalState, HandleRawHazardBlockState, nullptr,
                    handle_raw_hazard_instr<Valu, Vintrp, Salu>>(state, global, block);

   *NOPs = MAX2(*NOPs, global.nops_needed);
}

static auto handle_valu_then_read_hazard = handle_raw_hazard<true, true, false>;
static auto handle_vintrp_then_read_hazard = handle_raw_hazard<false, true, false>;
static auto handle_valu_salu_then_read_hazard = handle_raw_hazard<true, true, true>;

void
set_bitset_range(BITSET_WORD* words, unsigned start, unsigned size)
{
   unsigned end = start + size - 1;
   unsigned start_mod = start % BITSET_WORDBITS;
   if (start_mod + size <= BITSET_WORDBITS) {
      BITSET_SET_RANGE_INSIDE_WORD(words, start, end);
   } else {
      unsigned first_size = BITSET_WORDBITS - start_mod;
      set_bitset_range(words, start, BITSET_WORDBITS - start_mod);
      set_bitset_range(words, start + first_size, size - first_size);
   }
}

bool
test_bitset_range(BITSET_WORD* words, unsigned start, unsigned size)
{
   unsigned end = start + size - 1;
   unsigned start_mod = start % BITSET_WORDBITS;
   if (start_mod + size <= BITSET_WORDBITS) {
      return BITSET_TEST_RANGE(words, start, end);
   } else {
      unsigned first_size = BITSET_WORDBITS - start_mod;
      return test_bitset_range(words, start, BITSET_WORDBITS - start_mod) ||
             test_bitset_range(words, start + first_size, size - first_size);
   }
}

/* A SMEM clause is any group of consecutive SMEM instructions. The
 * instructions in this group may return out of order and/or may be replayed.
 *
 * To fix this potential hazard correctly, we have to make sure that when a
 * clause has more than one instruction, no instruction in the clause writes
 * to a register that is read by another instruction in the clause (including
 * itself). In this case, we have to break the SMEM clause by inserting non
 * SMEM instructions.
 *
 * SMEM clauses are only present on GFX8+, and only matter when XNACK is set.
 */
static void
handle_smem_clause_hazards(Program*                program,
                           NOP_ctx_gfx6&           ctx,
                           aco_ptr<Instruction>&   instr,
                           int*                    NOPs)
{
      if (!program->dev.xnack_enabled) return;
      assert(NOPs && "caller must pass valid pointer");

      /* Break an outstanding clause only if we *both* currently do not emit
       * another NOP already *and* we are still inside a clause.               */
      if (!*NOPs && (ctx.smem_clause || ctx.smem_write)) {         /* fixed “&&”→“&&” */
            /* Clause must not contain stores or atomics. */
            if (ctx.smem_write || instr->definitions.empty() ||
                  instr_info.is_atomic[static_cast<unsigned>(instr->opcode)])
            {
                  *NOPs = 1;
            }
            /* With XNACK the clause may replay out of order – ensure no RAW. */
            else if (program->dev.xnack_enabled) {
                  for (const Operand& op : instr->operands) {
                        if (!op.isConstant() &&
                              test_bitset_range(ctx.smem_clause_write, op.physReg(), op.size()))
                        {
                              *NOPs = 1;
                              break;
                        }
                  }

                  if (!*NOPs) {
                        const Definition& def = instr->definitions[0];
                        if (test_bitset_range(ctx.smem_clause_read_write,
                              def.physReg(), def.size()))
                              *NOPs = 1;
                  }
            }
      }
}

static bool has_intervening_valu(State& state, Block* start_block) {
      std::stack<Block*> blocks_to_check;
      std::set<unsigned> visited;
      blocks_to_check.push(start_block);

      while (!blocks_to_check.empty()) {
            Block* block = blocks_to_check.top();
            blocks_to_check.pop();

            if (visited.count(block->index)) continue;
            visited.insert(block->index);

            // Search backwards for VALU instructions
            for (auto it = block->instructions.rbegin(); it != block->instructions.rend(); ++it) {
                  if ((*it)->isVALU()) return true;
                  if (is_vcc_branch(*it)) break; // Stop at previous branch
            }

            // Add linear predecessors (to handle loop/control flow)
            for (unsigned pred : block->linear_preds) {
                  blocks_to_check.push(&state.program->blocks[pred]);
            }
      }
      return false;
}

static void
handle_instruction_gfx6(State&                             state,
                        NOP_ctx_gfx6&                      ctx,
                        aco_ptr<Instruction>&              instr,
                        std::vector<aco_ptr<Instruction>>& new_instructions)
{
   /* --------------------------------------------------------------------
    *  1.  Determine required wait-states *before* @instr
    * ------------------------------------------------------------------ */
   int NOPs = 0;

   /* SALU→VCC→branch hazard (Vega = 1 wait-state) */
   if (is_vcc_branch(instr)) {
         if (state.program->gfx_level == GFX9) {
               // Skip NOP if there's a VALU between SALU→VCC and branch
               if (!has_intervening_valu(state, state.block)) {
                     NOPs = std::max(NOPs, static_cast<int>(ctx.vcc_wr_then_branch));
               }
         } else {
               NOPs = std::max(NOPs, static_cast<int>(ctx.vcc_wr_then_branch));
         }
   }

   /* -------------------------  SMEM hazards  --------------------------- */
   if (instr->isSMEM()) {
      if (state.program->gfx_level == GFX6) {
         /* VALU/SALU → SMRD RAW hazards (4 wait-states). */
         for (unsigned i = 0; i < instr->operands.size(); ++i) {
            const Operand& op = instr->operands[i];
            if (op.isConstant())
               continue;

            const bool is_buffer_desc = (i == 0) && op.size() > 2;
            if (is_buffer_desc)
               handle_valu_salu_then_read_hazard(state, &NOPs, 4, op);
            else
               handle_valu_then_read_hazard(state, &NOPs, 4, op);
         }
      }

      /* SMEM-clause hazards (replay/XNACK etc.). */
      handle_smem_clause_hazards(state.program, ctx, instr, &NOPs);
   }

   /* -------------------------  SALU hazards  --------------------------- */
   else if (instr->isSALU()) {
      if (instr->opcode == aco_opcode::s_setreg_b32         ||
          instr->opcode == aco_opcode::s_setreg_imm32_b32   ||
          instr->opcode == aco_opcode::s_getreg_b32)
      {
         NOPs = std::max(NOPs, static_cast<int>(ctx.setreg_then_getsetreg));
      }

      if (state.program->gfx_level == GFX9 &&
          (instr->opcode == aco_opcode::s_movrels_b32 ||
           instr->opcode == aco_opcode::s_movrels_b64 ||
           instr->opcode == aco_opcode::s_movreld_b32 ||
           instr->opcode == aco_opcode::s_movreld_b64))
      {
         NOPs = std::max(NOPs, static_cast<int>(ctx.salu_wr_m0_then_moverel));
      }

      if (instr->opcode == aco_opcode::s_sendmsg ||
          instr->opcode == aco_opcode::s_ttracedata)
      {
         NOPs = std::max(NOPs,
                         static_cast<int>(ctx.salu_wr_m0_then_gds_msg_ttrace));
      }
   }

   /* -------------------------  DS / GDS hazards  ----------------------- */
   else if (instr->isDS() && instr->ds().gds) {
      NOPs = std::max(NOPs,
                      static_cast<int>(ctx.salu_wr_m0_then_gds_msg_ttrace));
   }

   /* -------------------------  VALU / VINTRP  -------------------------- */
   else if (instr->isVALU() || instr->isVINTRP()) {
      if (instr->isDPP()) {
         NOPs = std::max(NOPs,
                         static_cast<int>(ctx.valu_wr_exec_then_dpp));
         handle_valu_then_read_hazard(state, &NOPs, 2, instr->operands[0]);
      }

      /* VMEM-store followed by WRDATA hazard. */
      for (const Definition& def : instr->definitions) {
            if (def.regClass().type() == RegType::sgpr)
                  continue;

            const unsigned base = def.physReg() & 0xff;
            int max_wait = 0;
            for (unsigned i = 0; i < def.size(); ++i) {
                  max_wait = std::max(max_wait, static_cast<int>(ctx.vmem_store_then_wr_data[base + i]));
            }
            if (max_wait > 0) {
                  // Use s_waitcnt for GFX9 if wait ≥3 (more efficient than NOPs)
                  if (state.program->gfx_level == GFX9 && max_wait >= 3) {
                        Builder bld(state.program, &new_instructions);
                        wait_imm imm;
                        imm.lgkm = 0; // Wait for LGKM (LDS/GFX/MEM) operations to complete
                        bld.sopp(aco_opcode::s_waitcnt, imm.pack(state.program->gfx_level));
                        ctx.vmem_store_then_wr_data.reset(); // Clear hazard after wait
                  } else {
                        NOPs = std::max(NOPs, max_wait);
                  }
            }
      }

      /* readlane / writelane hazards (SGPR index is operand[1]). */
      if ((instr->opcode == aco_opcode::v_readlane_b32      ||
           instr->opcode == aco_opcode::v_readlane_b32_e64  ||
           instr->opcode == aco_opcode::v_writelane_b32     ||
           instr->opcode == aco_opcode::v_writelane_b32_e64) &&
          !instr->operands[1].isConstant())
      {
         handle_valu_then_read_hazard(state, &NOPs, 4, instr->operands[1]);
      }

      /* undocumented GFX6 VINTRP → readlane hazard. */
      if (state.program->gfx_level == GFX6 &&
          (instr->opcode == aco_opcode::v_readlane_b32 ||
           instr->opcode == aco_opcode::v_readfirstlane_b32))
      {
         handle_vintrp_then_read_hazard(state, &NOPs, 1, instr->operands[0]);
      }

      /* VALU writes VCC → v_div_fmas reads VCC later. */
      if (instr->opcode == aco_opcode::v_div_fmas_f32 ||
            instr->opcode == aco_opcode::v_div_fmas_f64)
      {
            ctx.valu_wr_vcc_then_div_fmas = (state.program->gfx_level == GFX9) ? 3 : 4;
      }
   }

   /* -------------------------  VMEM / FLAT hazards  -------------------- */
   else if (instr->isVMEM() || instr->isFlatLike()) {
      for (const Operand& op : instr->operands) {
         if (!op.isConstant() && !op.isUndefined() &&
             op.regClass().type() == RegType::sgpr)
         {
            handle_valu_then_read_hazard(state, &NOPs, 5, op);
         }
      }
   }

   /* v_skip wait-state hazard (non-SALU / non-SMEM). */
   if (!instr->isSALU() && instr->format != Format::SMEM)
      NOPs = std::max(NOPs,
                      static_cast<int>(ctx.set_vskip_mode_then_vector));

   /* Extra GFX9 LDS hazards. */
   if (state.program->gfx_level == GFX9) {
      const bool lds_scratch_global =
         (instr->isScratch() || instr->isGlobal()) && instr->flatlike().lds;

      if (instr->isVINTRP() || lds_scratch_global ||
          instr->opcode == aco_opcode::ds_read_addtid_b32  ||
          instr->opcode == aco_opcode::ds_write_addtid_b32 ||
          instr->opcode == aco_opcode::buffer_store_lds_dword)
      {
         NOPs = std::max(NOPs,
                         static_cast<int>(ctx.salu_wr_m0_then_lds));
      }
   }

   /* --------------------------------------------------------------------
    *  2.  Emit NOPs / waitcnt and age live counters
    * ------------------------------------------------------------------ */
   ctx.add_wait_states(static_cast<unsigned>(NOPs) + get_wait_states(instr));

   if (NOPs) {
         Builder bld(state.program, &new_instructions);
         const bool long_wait  = NOPs >= 3;
         const bool mem_hazard = ctx.vmem_store_then_wr_data.any() ||
         ctx.smem_clause || ctx.smem_write;

         if (long_wait && mem_hazard) {
               wait_imm imm;
               imm.vm   = 0;
               imm.lgkm = 0;
               bld.sopp(aco_opcode::s_waitcnt, imm.pack(state.program->gfx_level));
         } else {
               bld.sopp(aco_opcode::s_nop, static_cast<uint16_t>(NOPs - 1));
         }
   }

   /* --------------------------------------------------------------------
    *  3.  Update context (hazards *caused* by @instr)
    * ------------------------------------------------------------------ */

   /* ---------- SMEM clause tracking ----------------------------------- */
   if ((ctx.smem_clause || ctx.smem_write) &&
       (NOPs || instr->format != Format::SMEM))
   {
      ctx.smem_clause = ctx.smem_write = false;
      if (state.program->dev.xnack_enabled) {
         BITSET_ZERO(ctx.smem_clause_read_write);
         BITSET_ZERO(ctx.smem_clause_write);
      }
   }

   if (instr->isSMEM()) {
      if (instr->definitions.empty() ||
          instr_info.is_atomic[static_cast<unsigned>(instr->opcode)])
      {
         ctx.smem_write = true;
      } else {
         ctx.smem_clause = true;

         if (state.program->dev.xnack_enabled) {
            for (const Operand& op : instr->operands)
               if (!op.isConstant())
                  set_bitset_range(ctx.smem_clause_read_write,
                                   op.physReg(), op.size());

            const Definition& def = instr->definitions[0];
            set_bitset_range(ctx.smem_clause_read_write,
                             def.physReg(), def.size());
            set_bitset_range(ctx.smem_clause_write,
                             def.physReg(), def.size());
         }
      }
   }

   /* ---------- VALU post-processing ------------------------------------ */
   else if (instr->isVALU()) {
      for (const Definition& def : instr->definitions) {
         if (def.regClass().type() != RegType::sgpr)
            continue;

         if (def.physReg() == vcc || def.physReg() == vcc_hi) {
            ctx.valu_wr_vcc_then_div_fmas = (state.program->gfx_level == GFX9) ? 3 : 4;
            }
         if (def.physReg() == exec || def.physReg() == exec_hi)
            ctx.valu_wr_exec_then_dpp     = 5;
      }
   }

   /* ---------- SALU post-processing ------------------------------------ */
   else if (instr->isSALU()) {
      if (!instr->definitions.empty()) {
         const Definition& def = instr->definitions[0];
         if (def.physReg() == m0) {
            ctx.salu_wr_m0_then_gds_msg_ttrace = 1;
            ctx.salu_wr_m0_then_lds            = 1;
            ctx.salu_wr_m0_then_moverel        = 1;
         }
      }

      if (instr->opcode == aco_opcode::s_setreg_b32 ||
          instr->opcode == aco_opcode::s_setreg_imm32_b32)
      {
         SALU_instruction& sopk = instr->salu();
         const unsigned offset = (sopk.imm >> 6) & 0x1f;
         const unsigned size   = ((sopk.imm >> 11) & 0x1f) + 1;
         const unsigned reg    =  sopk.imm & 0x3f;

         ctx.setreg_then_getsetreg = 2;

         /* MODE register (SGPR1), bit 28 is v_skip. */
         if (reg == 1u && offset <= 28u && (28u - offset) < size) {
            bool bit_known = false;
            uint32_t src_val = 0u;

            if (instr->opcode == aco_opcode::s_setreg_imm32_b32) {
               src_val  = sopk.imm;
               bit_known = true;
            } else if (sopk.operands[0].isConstant()) {
               src_val  = sopk.operands[0].constantValue();
               bit_known = true;
            }

            if (bit_known) {
               const uint8_t new_bit =
                  static_cast<uint8_t>((src_val >> (28u - offset)) & 1u);
               if (new_bit != ctx.current_vskip) {
                  ctx.set_vskip_mode_then_vector = 2;
                  ctx.current_vskip = new_bit;
               }
            } else {
               /* Unknown value – conservatively assume it toggles. */
               ctx.set_vskip_mode_then_vector = 2;
               ctx.current_vskip ^= 1u;
            }
         }
      }
   }

   /* ---------- VMEM store → WRDATA tracker ---------------------------- */
   else if (instr->isVMEM() || instr->isFlatLike()) {
      const bool consider_buf  = (instr->isMUBUF() || instr->isMTBUF()) &&
                                 instr->operands.size() == 4 &&
                                 instr->operands[3].size() > 2 &&
                                 instr->operands[2].physReg() >= 128;
      const bool consider_mimg = instr->isMIMG() &&
                                 instr->operands[1].regClass().type() == RegType::vgpr &&
                                 instr->operands[1].size() > 2 &&
                                 instr->operands[0].size() == 4;
      const bool consider_flat = instr->isFlatLike() &&
                                 instr->operands.size() == 3 &&
                                 instr->operands[2].size() > 2;

      if (consider_buf || consider_mimg || consider_flat) {
         const unsigned op_idx = consider_flat ? 2 : 3;
         const PhysReg wrdata  = instr->operands[op_idx].physReg();
         const unsigned size   = instr->operands[op_idx].size();

         for (unsigned i = 0; i < size; ++i)
            ctx.vmem_store_then_wr_data[(wrdata & 0xff) + i] = 1u;
      }
   }

   /* ---------- Track SALU writes to VCC for next branch --------------- */
   if (instr->isSALU()) {
      for (const Definition& def : instr->definitions) {
         if (def.physReg() == vcc || def.physReg() == vcc_hi) {
            ctx.vcc_wr_then_branch = 1;  /* Vega: exactly 1 wait-state. */
            break;
         }
      }
   }
}

bool
is_latest_instr_vintrp(bool& global_state, bool& block_state, aco_ptr<Instruction>& pred) {
      if (pred->isVINTRP())
            global_state = true;
      return true;
}

template <bool Salu, bool Sgpr>
bool
handle_wr_hazard_instr(int& global_state, int& block_state, aco_ptr<Instruction>& pred) {
      if (Salu ? pred->isSALU() : (pred->isVALU() || pred->isVINTRP())) {
            for (Definition dst : pred->definitions) {
                  if ((dst.physReg().reg() < 256) == Sgpr) {
                        global_state = MAX2(global_state, block_state);
                        return true;
                  }
            }
      }

      block_state -= get_wait_states(pred);
      return block_state <= 0;
}

template <bool Salu, bool Sgpr>
void
handle_wr_hazard(State& state, int* NOPs, int min_states) {
      if (*NOPs >= min_states)
            return;

      int global = 0;
      int block = min_states;
      search_backwards<int, int, nullptr, handle_wr_hazard_instr<Salu, Sgpr>>(state, global, block);
      *NOPs = MAX2(*NOPs, global);
}

/* ---------------------------------------------------------------------------
 *  Final “resolve all pending hazards” for the current basic block (GFX6-9)
 * ------------------------------------------------------------------------- */
static void
resolve_all_gfx6(State& state, NOP_ctx_gfx6& ctx,
                 std::vector<aco_ptr<Instruction>>& new_instructions) {
   int NOPs = 0;

   /* SGPR→SMEM hazards (only on pre-GFX8) */
   if (state.program->gfx_level == GFX6) {
      handle_wr_hazard<true, true>(state, &NOPs, 4);
      handle_wr_hazard<false, true>(state, &NOPs, 4);
   }

   /* Break SMEM clauses if one is still open. */
   if (ctx.smem_clause || ctx.smem_write)
      NOPs = std::max(NOPs, 1);

   /* SALU / LDS hazards still active? */
   NOPs = std::max(NOPs, static_cast<int>(ctx.setreg_then_getsetreg));
   if (state.program->gfx_level == GFX9)
      NOPs = std::max(NOPs, static_cast<int>(ctx.salu_wr_m0_then_moverel));
   NOPs = std::max(NOPs, static_cast<int>(ctx.salu_wr_m0_then_gds_msg_ttrace));

   /* VALU hazards */
   NOPs = std::max(NOPs, static_cast<int>(ctx.valu_wr_exec_then_dpp));
   if (state.program->gfx_level >= GFX8)
      handle_wr_hazard<false, false>(state, &NOPs, 2); /* VALU→DPP */
   if (ctx.vmem_store_then_wr_data.any())
      NOPs = std::max(NOPs, 1);
   if (state.program->gfx_level == GFX6) {
      bool vintrp = false;
      search_backwards<bool, bool, nullptr, is_latest_instr_vintrp>(
         state, vintrp, vintrp);
      if (vintrp)
         NOPs = std::max(NOPs, 1);
   }
   NOPs = std::max(NOPs, static_cast<int>(ctx.valu_wr_vcc_then_div_fmas));

   /* VALU(sgpr)→VMEM / readlane hazards. */
   handle_wr_hazard<false, true>(state, &NOPs, 5);

   /* MODE.vskip hazard still pending? */
   NOPs = std::max(NOPs, static_cast<int>(ctx.set_vskip_mode_then_vector));

   if (state.program->gfx_level == GFX9)
      NOPs = std::max(NOPs, static_cast<int>(ctx.salu_wr_m0_then_lds));

   /* --------------------------------------------------------------------
    *  Emit final waits and age counters
    * ------------------------------------------------------------------ */
   ctx.add_wait_states(static_cast<unsigned>(NOPs));

   if (NOPs) {
      Builder bld(state.program, &new_instructions);

      const bool long_wait  = NOPs >= 3;
      const bool mem_hazard = ctx.vmem_store_then_wr_data.any() ||
                              ctx.smem_clause || ctx.smem_write;

      if (long_wait && mem_hazard) {
         wait_imm imm;
         imm.vm   = 0;
         imm.lgkm = 0;
         bld.sopp(aco_opcode::s_waitcnt,
                  imm.pack(state.program->gfx_level)); /* <<< FIXED */
      } else {
         bld.sopp(aco_opcode::s_nop,
                  static_cast<uint16_t>(NOPs - 1));
      }
   }
}

template <std::size_t N>
bool
check_written_regs(const aco_ptr<Instruction>& instr, const std::bitset<N>& check_regs) {
      return std::any_of(instr->definitions.begin(), instr->definitions.end(),
                         [&check_regs](const Definition& def) -> bool {
                               bool writes_any = false;
                               for (unsigned i = 0; i < def.size(); i++) {
                                     unsigned def_reg = def.physReg() + i;
                                     writes_any |= def_reg < check_regs.size() && check_regs[def_reg];
                               }
                               return writes_any;
                         });
}

template <std::size_t N>
void
mark_read_regs(const aco_ptr<Instruction>& instr, std::bitset<N>& reg_reads) {
      for (const Operand& op : instr->operands) {
            for (unsigned i = 0; i < op.size(); i++) {
                  unsigned reg = op.physReg() + i;
                  if (reg < reg_reads.size())
                        reg_reads.set(reg);
            }
      }
}

template <std::size_t N>
void
mark_read_regs_exec(State& state, const aco_ptr<Instruction>& instr, std::bitset<N>& reg_reads) {
      mark_read_regs(instr, reg_reads);
      reg_reads.set(exec);
      if (state.program->wave_size == 64)
            reg_reads.set(exec_hi);
}

bool
VALU_writes_sgpr(aco_ptr<Instruction>& instr) {
      if (instr->isVOPC())
            return true;
      if (instr->isVOP3() && instr->definitions.size() == 2)
            return true;
      if (instr->opcode == aco_opcode::v_readfirstlane_b32 ||
            instr->opcode == aco_opcode::v_readlane_b32 ||
            instr->opcode == aco_opcode::v_readlane_b32_e64)
            return true;
      return false;
}

bool
instr_writes_sgpr(const aco_ptr<Instruction>& instr) {
      return std::any_of(instr->definitions.begin(), instr->definitions.end(),
                         [](const Definition& def) -> bool {
                               return def.getTemp().type() == RegType::sgpr;
                         });
}

inline bool
instr_is_branch(const aco_ptr<Instruction>& instr) {
      return instr->opcode == aco_opcode::s_branch || instr->opcode == aco_opcode::s_cbranch_scc0 ||
      instr->opcode == aco_opcode::s_cbranch_scc1 ||
      instr->opcode == aco_opcode::s_cbranch_vccz ||
      instr->opcode == aco_opcode::s_cbranch_vccnz ||
      instr->opcode == aco_opcode::s_cbranch_execz ||
      instr->opcode == aco_opcode::s_cbranch_execnz ||
      instr->opcode == aco_opcode::s_cbranch_cdbgsys ||
      instr->opcode == aco_opcode::s_cbranch_cdbguser ||
      instr->opcode == aco_opcode::s_cbranch_cdbgsys_or_user ||
      instr->opcode == aco_opcode::s_cbranch_cdbgsys_and_user ||
      instr->opcode == aco_opcode::s_subvector_loop_begin ||
      instr->opcode == aco_opcode::s_subvector_loop_end ||
      instr->opcode == aco_opcode::s_setpc_b64 || instr->opcode == aco_opcode::s_swappc_b64 ||
      instr->opcode == aco_opcode::s_getpc_b64 || instr->opcode == aco_opcode::s_call_b64;
}

void
handle_instruction_gfx10(State& state, NOP_ctx_gfx10& ctx, aco_ptr<Instruction>& instr,
                         std::vector<aco_ptr<Instruction>>& new_instructions)
{
   // TODO: s_dcache_inv needs to be in it's own group on GFX10

   Builder bld(state.program, &new_instructions);

   unsigned vm_vsrc = 7;
   unsigned sa_sdst = 1;
   if (debug_flags & DEBUG_FORCE_WAITDEPS) {
      bld.sopp(aco_opcode::s_waitcnt_depctr, 0x0000);
      vm_vsrc = 0;
      sa_sdst = 0;
   } else if (instr->opcode == aco_opcode::s_waitcnt_depctr) {
      vm_vsrc = (instr->salu().imm >> 2) & 0x7;
      sa_sdst = instr->salu().imm & 0x1;
   }

   /* VMEMtoScalarWriteHazard
    * Handle EXEC/M0/SGPR write following a VMEM/DS instruction without a VALU or "waitcnt vmcnt(0)"
    * in-between.
    */
   if (instr->isVMEM() || instr->isFlatLike() || instr->isDS()) {
      /* Remember all SGPRs that are read by the VMEM/DS instruction */
      if (instr->isVMEM() || instr->isFlatLike())
         mark_read_regs_exec(
            state, instr,
            instr->definitions.empty() ? ctx.sgprs_read_by_VMEM_store : ctx.sgprs_read_by_VMEM);
      if (instr->isFlat() || instr->isDS())
         mark_read_regs_exec(state, instr, ctx.sgprs_read_by_DS);
   } else if (instr->isSALU() || instr->isSMEM()) {
      wait_imm imm;
      if (imm.unpack(state.program->gfx_level, instr.get())) {
         if (imm.vm == 0)
            ctx.sgprs_read_by_VMEM.reset();
         if (imm.lgkm == 0)
            ctx.sgprs_read_by_DS.reset();
         if (imm.vs == 0)
            ctx.sgprs_read_by_VMEM_store.reset();
      } else if (vm_vsrc == 0) {
         ctx.sgprs_read_by_VMEM.reset();
         ctx.sgprs_read_by_DS.reset();
         ctx.sgprs_read_by_VMEM_store.reset();
      }

      /* Check if SALU writes an SGPR that was previously read by the VALU */
      if (check_written_regs(instr, ctx.sgprs_read_by_VMEM) ||
          check_written_regs(instr, ctx.sgprs_read_by_DS) ||
          check_written_regs(instr, ctx.sgprs_read_by_VMEM_store)) {
         ctx.sgprs_read_by_VMEM.reset();
         ctx.sgprs_read_by_DS.reset();
         ctx.sgprs_read_by_VMEM_store.reset();

         /* Insert s_waitcnt_depctr instruction with magic imm to mitigate the problem */
         bld.sopp(aco_opcode::s_waitcnt_depctr, 0xffe3);
      }
   } else if (instr->isVALU()) {
      /* Hazard is mitigated by any VALU instruction */
      ctx.sgprs_read_by_VMEM.reset();
      ctx.sgprs_read_by_DS.reset();
      ctx.sgprs_read_by_VMEM_store.reset();
   }

   /* VcmpxPermlaneHazard
    * Handle any permlane following a VOPC instruction writing exec, insert v_mov between them.
    */
   if (instr->isVOPC() && instr->definitions[0].physReg() == exec) {
      /* we only need to check definitions[0] because since GFX10 v_cmpx only writes one dest */
      ctx.has_VOPC_write_exec = true;
   } else if (ctx.has_VOPC_write_exec && (instr->opcode == aco_opcode::v_permlane16_b32 ||
                                          instr->opcode == aco_opcode::v_permlanex16_b32)) {
      ctx.has_VOPC_write_exec = false;

      /* v_nop would be discarded by SQ, so use v_mov with the first operand of the permlane */
      bld.vop1(aco_opcode::v_mov_b32, Definition(instr->operands[0].physReg(), v1),
               Operand(instr->operands[0].physReg(), v1));
   } else if (instr->isVALU() && instr->opcode != aco_opcode::v_nop) {
      ctx.has_VOPC_write_exec = false;
   }

   /* VcmpxExecWARHazard
    * Handle any VALU instruction writing the exec mask after it was read by a non-VALU instruction.
    */
   if (!instr->isVALU() && instr->reads_exec()) {
      ctx.has_nonVALU_exec_read = true;
   } else if (instr->isVALU() && ctx.has_nonVALU_exec_read) {
      if (instr->writes_exec()) {
         ctx.has_nonVALU_exec_read = false;

         /* Insert s_waitcnt_depctr instruction with magic imm to mitigate the problem */
         bld.sopp(aco_opcode::s_waitcnt_depctr, 0xfffe);
      } else if (instr_writes_sgpr(instr)) {
         /* Any VALU instruction that writes an SGPR mitigates the problem */
         ctx.has_nonVALU_exec_read = false;
      }
   } else if (sa_sdst == 0) {
      ctx.has_nonVALU_exec_read = false;
   }

   /* SMEMtoVectorWriteHazard
    * Handle any VALU instruction writing an SGPR after an SMEM reads it.
    */
   if (instr->isSMEM()) {
      /* Remember all SGPRs that are read by the SMEM instruction */
      mark_read_regs(instr, ctx.sgprs_read_by_SMEM);
   } else if (VALU_writes_sgpr(instr)) {
      /* Check if VALU writes an SGPR that was previously read by SMEM */
      if (check_written_regs(instr, ctx.sgprs_read_by_SMEM)) {
         ctx.sgprs_read_by_SMEM.reset();

         /* Insert s_mov to mitigate the problem */
         bld.sop1(aco_opcode::s_mov_b32, Definition(sgpr_null, s1), Operand::zero());
      }
   } else if (instr->isSALU()) {
      wait_imm imm;
      if (imm.unpack(state.program->gfx_level, instr.get()) && imm.lgkm == 0) {
         /* Reducing lgkmcnt count to 0 always mitigates the hazard. */
         ctx.sgprs_read_by_SMEM.reset();
      } else if (instr->format != Format::SOPP && instr->definitions.size()) {
         /* SALU can mitigate the hazard */
         ctx.sgprs_read_by_SMEM.reset();
      }
   }

   /* LdsBranchVmemWARHazard
    * Handle VMEM/GLOBAL/SCRATCH->branch->DS and DS->branch->VMEM/GLOBAL/SCRATCH patterns.
    */
   if (instr->isVMEM() || instr->isGlobal() || instr->isScratch()) {
      if (ctx.has_branch_after_DS)
         bld.sopk(aco_opcode::s_waitcnt_vscnt, Operand(sgpr_null, s1), 0);
      ctx.has_branch_after_VMEM = ctx.has_branch_after_DS = ctx.has_DS = false;
      ctx.has_VMEM = true;
   } else if (instr->isDS()) {
      if (ctx.has_branch_after_VMEM)
         bld.sopk(aco_opcode::s_waitcnt_vscnt, Operand(sgpr_null, s1), 0);
      ctx.has_branch_after_VMEM = ctx.has_branch_after_DS = ctx.has_VMEM = false;
      ctx.has_DS = true;
   } else if (instr_is_branch(instr)) {
      ctx.has_branch_after_VMEM |= ctx.has_VMEM;
      ctx.has_branch_after_DS |= ctx.has_DS;
      ctx.has_VMEM = ctx.has_DS = false;
   } else if (instr->opcode == aco_opcode::s_waitcnt_vscnt) {
      /* Only s_waitcnt_vscnt can mitigate the hazard */
      const SALU_instruction& sopk = instr->salu();
      if (sopk.operands[0].physReg() == sgpr_null && sopk.imm == 0)
         ctx.has_VMEM = ctx.has_branch_after_VMEM = ctx.has_DS = ctx.has_branch_after_DS = false;
   }

   /* NSAToVMEMBug
    * Handles NSA MIMG (4 or more dwords) immediately followed by MUBUF/MTBUF (with offset[2:1] !=
    * 0).
    */
   if (instr->isMIMG() && get_mimg_nsa_dwords(instr.get()) > 1) {
      ctx.has_NSA_MIMG = true;
   } else if (ctx.has_NSA_MIMG) {
      ctx.has_NSA_MIMG = false;

      if (instr->isMUBUF() || instr->isMTBUF()) {
         uint32_t offset = instr->isMUBUF() ? instr->mubuf().offset : instr->mtbuf().offset;
         if (offset & 6)
            bld.sopp(aco_opcode::s_nop, 0);
      }
   }

   /* waNsaCannotFollowWritelane
    * Handles NSA MIMG immediately following a v_writelane_b32.
    */
   if (instr->opcode == aco_opcode::v_writelane_b32_e64) {
      ctx.has_writelane = true;
   } else if (ctx.has_writelane) {
      ctx.has_writelane = false;
      if (instr->isMIMG() && get_mimg_nsa_dwords(instr.get()) > 0)
         bld.sopp(aco_opcode::s_nop, 0);
   }
}

void
resolve_all_gfx10(State& state, NOP_ctx_gfx10& ctx,
                  std::vector<aco_ptr<Instruction>>& new_instructions)
{
   Builder bld(state.program, &new_instructions);

   size_t prev_count = new_instructions.size();

   /* VcmpxPermlaneHazard */
   if (ctx.has_VOPC_write_exec) {
      ctx.has_VOPC_write_exec = false;
      bld.vop1(aco_opcode::v_mov_b32, Definition(PhysReg(256), v1), Operand(PhysReg(256), v1));

      /* VALU mitigates VMEMtoScalarWriteHazard. */
      ctx.sgprs_read_by_VMEM.reset();
      ctx.sgprs_read_by_DS.reset();
      ctx.sgprs_read_by_VMEM_store.reset();
   }

   unsigned waitcnt_depctr = 0xffff;

   /* VMEMtoScalarWriteHazard */
   if (ctx.sgprs_read_by_VMEM.any() || ctx.sgprs_read_by_DS.any() ||
       ctx.sgprs_read_by_VMEM_store.any()) {
      ctx.sgprs_read_by_VMEM.reset();
      ctx.sgprs_read_by_DS.reset();
      ctx.sgprs_read_by_VMEM_store.reset();
      waitcnt_depctr &= 0xffe3;
   }

   /* VcmpxExecWARHazard */
   if (ctx.has_nonVALU_exec_read) {
      ctx.has_nonVALU_exec_read = false;
      waitcnt_depctr &= 0xfffe;
   }

   if (waitcnt_depctr != 0xffff)
      bld.sopp(aco_opcode::s_waitcnt_depctr, waitcnt_depctr);

   /* SMEMtoVectorWriteHazard */
   if (ctx.sgprs_read_by_SMEM.any()) {
      ctx.sgprs_read_by_SMEM.reset();
      bld.sop1(aco_opcode::s_mov_b32, Definition(sgpr_null, s1), Operand::zero());
   }

   /* LdsBranchVmemWARHazard */
   if (ctx.has_VMEM || ctx.has_branch_after_VMEM || ctx.has_DS || ctx.has_branch_after_DS) {
      bld.sopk(aco_opcode::s_waitcnt_vscnt, Operand(sgpr_null, s1), 0);
      ctx.has_VMEM = ctx.has_branch_after_VMEM = ctx.has_DS = ctx.has_branch_after_DS = false;
   }

   /* NSAToVMEMBug/waNsaCannotFollowWritelane */
   if (ctx.has_NSA_MIMG || ctx.has_writelane) {
      ctx.has_NSA_MIMG = ctx.has_writelane = false;
      /* Any instruction resolves these hazards. */
      if (new_instructions.size() == prev_count)
         bld.sopp(aco_opcode::s_nop, 0);
   }
}

void
fill_vgpr_bitset(std::bitset<256>& set, PhysReg reg, unsigned bytes)
{
   if (reg.reg() < 256)
      return;
   for (unsigned i = 0; i < DIV_ROUND_UP(bytes, 4); i++)
      set.set(reg.reg() - 256 + i);
}

bool
test_vgpr_bitset(std::bitset<256>& set, Operand op)
{
   if (op.physReg().reg() < 256)
      return false;
   for (unsigned i = 0; i < op.size(); i++) {
      if (set[op.physReg().reg() - 256 + i])
         return true;
   }
   return false;
}

/* GFX11 */
struct LdsDirectVALUHazardGlobalState {
   unsigned wait_vdst = 15;
   PhysReg vgpr;
   std::set<unsigned> loop_headers_visited;
};

struct LdsDirectVALUHazardBlockState {
   unsigned num_valu = 0;
   bool has_trans = false;

   unsigned num_instrs = 0;
   unsigned num_blocks = 0;
};

bool
handle_lds_direct_valu_hazard_instr(LdsDirectVALUHazardGlobalState& global_state,
                                    LdsDirectVALUHazardBlockState& block_state,
                                    aco_ptr<Instruction>& instr)
{
   if (instr->isVALU()) {
      block_state.has_trans |= instr->isTrans();

      bool uses_vgpr = false;
      for (Definition& def : instr->definitions)
         uses_vgpr |= regs_intersect(def.physReg(), def.size(), global_state.vgpr, 1);
      for (Operand& op : instr->operands) {
         uses_vgpr |=
            !op.isConstant() && regs_intersect(op.physReg(), op.size(), global_state.vgpr, 1);
      }
      if (uses_vgpr) {
         /* Transcendentals execute in parallel to other VALU and va_vdst count becomes unusable */
         global_state.wait_vdst =
            MIN2(global_state.wait_vdst, block_state.has_trans ? 0 : block_state.num_valu);
         return true;
      }

      block_state.num_valu++;
   }

   if (parse_depctr_wait(instr.get()).va_vdst == 0)
      return true;

   block_state.num_instrs++;
   if (block_state.num_instrs > 256 || block_state.num_blocks > 32) {
      /* Exit to limit compile times and set wait_vdst to be safe. */
      global_state.wait_vdst =
         MIN2(global_state.wait_vdst, block_state.has_trans ? 0 : block_state.num_valu);
      return true;
   }

   return block_state.num_valu >= global_state.wait_vdst;
}

bool
handle_lds_direct_valu_hazard_block(LdsDirectVALUHazardGlobalState& global_state,
                                    LdsDirectVALUHazardBlockState& block_state, Block* block)
{
   if (block->kind & block_kind_loop_header) {
      if (global_state.loop_headers_visited.count(block->index))
         return false;
      global_state.loop_headers_visited.insert(block->index);
   }

   block_state.num_blocks++;

   return true;
}

unsigned
handle_lds_direct_valu_hazard(State& state, aco_ptr<Instruction>& instr)
{
   /* LdsDirectVALUHazard
    * Handle LDSDIR writing a VGPR after it's used by a VALU instruction.
    */
   if (instr->ldsdir().wait_vdst == 0)
      return 0; /* early exit */

   LdsDirectVALUHazardGlobalState global_state;
   global_state.wait_vdst = instr->ldsdir().wait_vdst;
   global_state.vgpr = instr->definitions[0].physReg();
   LdsDirectVALUHazardBlockState block_state;
   search_backwards<LdsDirectVALUHazardGlobalState, LdsDirectVALUHazardBlockState,
                    &handle_lds_direct_valu_hazard_block, &handle_lds_direct_valu_hazard_instr>(
      state, global_state, block_state);
   return global_state.wait_vdst;
}

enum VALUPartialForwardingHazardState : uint8_t {
   nothing_written,
   written_after_exec_write,
   exec_written,
};

struct VALUPartialForwardingHazardGlobalState {
   bool hazard_found = false;
   std::set<unsigned> loop_headers_visited;
};

struct VALUPartialForwardingHazardBlockState {
   /* initialized by number of VGPRs read by VALU, decrement when encountered to return early */
   uint8_t num_vgprs_read = 0;
   BITSET_DECLARE(vgprs_read, 256) = {0};
   enum VALUPartialForwardingHazardState state = nothing_written;
   unsigned num_valu_since_read = 0;
   unsigned num_valu_since_write = 0;

   unsigned num_instrs = 0;
   unsigned num_blocks = 0;
};

bool
handle_valu_partial_forwarding_hazard_instr(VALUPartialForwardingHazardGlobalState& global_state,
                                            VALUPartialForwardingHazardBlockState& block_state,
                                            aco_ptr<Instruction>& instr)
{
   /* Check if there is already a hazard found on some other control flow path. */
   if (global_state.hazard_found)
      return true;

   if (instr->isSALU() && !instr->definitions.empty()) {
      if (block_state.state == written_after_exec_write && instr->writes_exec())
         block_state.state = exec_written;
   } else if (instr->isVALU()) {
      bool vgpr_write = false;
      for (Definition& def : instr->definitions) {
         if (def.physReg().reg() < 256)
            continue;

         for (unsigned i = 0; i < def.size(); i++) {
            unsigned reg = def.physReg().reg() - 256 + i;
            if (!BITSET_TEST(block_state.vgprs_read, reg))
               continue;

            if (block_state.state == exec_written && block_state.num_valu_since_write < 3) {
               global_state.hazard_found = true;
               return true;
            }

            BITSET_CLEAR(block_state.vgprs_read, reg);
            block_state.num_vgprs_read--;
            vgpr_write = true;
         }
      }

      if (vgpr_write) {
         /* If the state is nothing_written: the check below should ensure that this write is
          * close enough to the read.
          *
          * If the state is exec_written: the current choice of second write has failed. Reset and
          * try with the current write as the second one, if it's close enough to the read.
          *
          * If the state is written_after_exec_write: a further second write would be better, if
          * it's close enough to the read.
          */
         if (block_state.state == nothing_written || block_state.num_valu_since_read < 5) {
            block_state.state = written_after_exec_write;
            block_state.num_valu_since_write = 0;
         } else {
            block_state.num_valu_since_write++;
         }
      } else {
         block_state.num_valu_since_write++;
      }

      block_state.num_valu_since_read++;
   } else if (parse_depctr_wait(instr.get()).va_vdst == 0) {
      return true;
   }

   if (block_state.num_valu_since_read >= (block_state.state == nothing_written ? 5 : 8))
      return true; /* Hazard not possible at this distance. */
   if (block_state.num_vgprs_read == 0)
      return true; /* All VGPRs have been written and a hazard was never found. */

   block_state.num_instrs++;
   if (block_state.num_instrs > 256 || block_state.num_blocks > 32) {
      /* Exit to limit compile times and set hazard_found=true to be safe. */
      global_state.hazard_found = true;
      return true;
   }

   return false;
}

bool
handle_valu_partial_forwarding_hazard_block(VALUPartialForwardingHazardGlobalState& global_state,
                                            VALUPartialForwardingHazardBlockState& block_state,
                                            Block* block)
{
   if (block->kind & block_kind_loop_header) {
      if (global_state.loop_headers_visited.count(block->index))
         return false;
      global_state.loop_headers_visited.insert(block->index);
   }

   block_state.num_blocks++;

   return true;
}

bool
handle_valu_partial_forwarding_hazard(State& state, aco_ptr<Instruction>& instr)
{
   /* VALUPartialForwardingHazard
    * VALU instruction reads two VGPRs: one written before an exec write by SALU and one after.
    * For the hazard, there must be less than 3 VALU between the first and second VGPR writes.
    * There also must be less than 5 VALU between the second VGPR write and the current instruction.
    */
   if (state.program->wave_size != 64 || !instr->isVALU())
      return false;

   unsigned num_vgprs = 0;
   for (Operand& op : instr->operands)
      num_vgprs += op.physReg().reg() < 256 ? op.size() : 1;
   if (num_vgprs <= 1)
      return false; /* early exit */

   VALUPartialForwardingHazardBlockState block_state;

   for (unsigned i = 0; i < instr->operands.size(); i++) {
      Operand& op = instr->operands[i];
      if (op.physReg().reg() < 256)
         continue;
      for (unsigned j = 0; j < op.size(); j++)
         BITSET_SET(block_state.vgprs_read, op.physReg().reg() - 256 + j);
   }
   block_state.num_vgprs_read = BITSET_COUNT(block_state.vgprs_read);

   if (block_state.num_vgprs_read <= 1)
      return false; /* early exit */

   VALUPartialForwardingHazardGlobalState global_state;
   search_backwards<VALUPartialForwardingHazardGlobalState, VALUPartialForwardingHazardBlockState,
                    &handle_valu_partial_forwarding_hazard_block,
                    &handle_valu_partial_forwarding_hazard_instr>(state, global_state, block_state);
   return global_state.hazard_found;
}

void
handle_instruction_gfx11(State& state, NOP_ctx_gfx11& ctx, aco_ptr<Instruction>& instr,
                         std::vector<aco_ptr<Instruction>>& new_instructions)
{
   Builder bld(state.program, &new_instructions);

   /* Due to a hazard, an s_nop is needed before "s_sendmsg sendmsg_dealloc_vgprs". */
   if (instr->opcode == aco_opcode::s_sendmsg && instr->salu().imm == sendmsg_dealloc_vgprs &&
       (new_instructions.empty() || new_instructions.back()->opcode != aco_opcode::s_nop)) {
      bld.sopp(aco_opcode::s_nop, 0);
   }

   /* VcmpxPermlaneHazard
    * Handle any permlane following a VOPC instruction writing exec, insert v_mov between them.
    */
   if (instr->isVOPC() && instr->definitions[0].physReg() == exec) {
      ctx.has_Vcmpx = true;
   } else if (ctx.has_Vcmpx && (instr->opcode == aco_opcode::v_permlane16_b32 ||
                                instr->opcode == aco_opcode::v_permlanex16_b32 ||
                                instr->opcode == aco_opcode::v_permlane64_b32 ||
                                instr->opcode == aco_opcode::v_permlane16_var_b32 ||
                                instr->opcode == aco_opcode::v_permlanex16_var_b32)) {
      ctx.has_Vcmpx = false;

      /* Unlike on GFX10, v_nop should resolve the hazard on GFX11. */
      bld.vop1(aco_opcode::v_nop);
   } else if (instr->isVALU()) {
      ctx.has_Vcmpx = false;
   }

   depctr_wait wait = parse_depctr_wait(instr.get());
   if (debug_flags & DEBUG_FORCE_WAITDEPS)
      wait = parse_depctr_wait(bld.sopp(aco_opcode::s_waitcnt_depctr, 0x0000));
   else if (instr->isLDSDIR() && state.program->gfx_level >= GFX12)
      wait.vm_vsrc = instr->ldsdir().wait_vsrc ? 7 : 0;

   if (instr->isLDSDIR()) {
      unsigned count = handle_lds_direct_valu_hazard(state, instr);
      LDSDIR_instruction* ldsdir = &instr->ldsdir();
      if (count < wait.va_vdst) {
         ldsdir->wait_vdst = MIN2(ldsdir->wait_vdst, count);
         wait.va_vdst = MIN2(wait.va_vdst, count);
      }
   }

   /* VALUTransUseHazard
    * VALU reads VGPR written by transcendental instruction without 6+ VALU or 2+ transcendental
    * in-between.
    */
   if (state.program->gfx_level < GFX11_5 && wait.va_vdst > 0 && instr->isVALU()) {
      uint8_t num_valu = 15;
      uint8_t num_trans = 15;
      for (Operand& op : instr->operands) {
         if (op.physReg().reg() < 256)
            continue;
         for (unsigned i = 0; i < op.size(); i++) {
            PhysReg reg = op.physReg().advance(i * 4);
            num_valu = std::min(num_valu, ctx.valu_since_wr_by_trans.get(reg));
            num_trans = std::min(num_trans, ctx.trans_since_wr_by_trans.get(reg));
         }
      }
      if (num_trans <= 1 && num_valu <= 5) {
         bld.sopp(aco_opcode::s_waitcnt_depctr, 0x0fff);
         wait.va_vdst = 0;
      }
   }

   if (wait.va_vdst > 0 && state.program->gfx_level < GFX12 &&
       handle_valu_partial_forwarding_hazard(state, instr)) {
      bld.sopp(aco_opcode::s_waitcnt_depctr, 0x0fff);
      wait.va_vdst = 0;
   }

   if (state.program->gfx_level < GFX12) {
      /* VALUMaskWriteHazard
       * VALU reads SGPR as a lane mask and later written by SALU or VALU cannot safely be read by
       * SALU or VALU.
       */
      if (state.program->wave_size == 64 && (instr->isSALU() || instr->isVALU())) {
         uint16_t imm = 0xffff;

         for (Operand op : instr->operands) {
            if (op.physReg() >= state.program->dev.sgpr_limit)
               continue;

            for (unsigned i = 0; i < op.size(); i++) {
               unsigned reg = op.physReg() + i;

               /* s_waitcnt_depctr on sa_sdst */
               if (ctx.sgpr_read_by_valu_as_lanemask_then_wr_by_salu[reg]) {
                  imm &= 0xfffe;
                  wait.sa_sdst = 0;
               }

               /* s_waitcnt_depctr on va_sdst (if non-VCC SGPR) or va_vcc (if VCC SGPR) */
               if (ctx.sgpr_read_by_valu_as_lanemask_then_wr_by_valu[reg]) {
                  bool is_vcc = reg == vcc || reg == vcc_hi;
                  imm &= is_vcc ? 0xfffd : 0xf1ff;
                  if (is_vcc)
                     wait.va_vcc = 0;
                  else
                     wait.va_sdst = 0;
               }
            }
         }

         if (imm != 0xffff)
            bld.sopp(aco_opcode::s_waitcnt_depctr, imm);
      }

      if (wait.va_vdst == 0) {
         ctx.valu_since_wr_by_trans.reset();
         ctx.trans_since_wr_by_trans.reset();
         ctx.sgpr_read_by_valu_as_lanemask_then_wr_by_valu.reset();
      }

      if (wait.sa_sdst == 0)
         ctx.sgpr_read_by_valu_as_lanemask_then_wr_by_salu.reset();

      if (wait.va_sdst == 0) {
         std::bitset<128> old = ctx.sgpr_read_by_valu_as_lanemask_then_wr_by_valu;
         ctx.sgpr_read_by_valu_as_lanemask_then_wr_by_valu.reset();
         ctx.sgpr_read_by_valu_as_lanemask_then_wr_by_valu[vcc] = old[vcc];
         ctx.sgpr_read_by_valu_as_lanemask_then_wr_by_valu[vcc_hi] = old[vcc_hi];
      }

      if (wait.va_vcc == 0) {
         ctx.sgpr_read_by_valu_as_lanemask_then_wr_by_valu[vcc] = false;
         ctx.sgpr_read_by_valu_as_lanemask_then_wr_by_valu[vcc_hi] = false;
      }

      if (state.program->wave_size == 64 && instr->isSALU() &&
          check_written_regs(instr, ctx.sgpr_read_by_valu_as_lanemask)) {
         unsigned reg = instr->definitions[0].physReg().reg();
         for (unsigned i = 0; i < instr->definitions[0].size(); i++)
            ctx.sgpr_read_by_valu_as_lanemask_then_wr_by_salu[reg + i] = 1;
      }

      if (instr->isVALU()) {
         bool is_trans = instr->isTrans();

         ctx.valu_since_wr_by_trans.inc();
         if (is_trans)
            ctx.trans_since_wr_by_trans.inc();

         if (is_trans) {
            for (Definition& def : instr->definitions) {
               for (unsigned i = 0; i < def.size(); i++) {
                  PhysReg reg = def.physReg().advance(i * 4);
                  ctx.valu_since_wr_by_trans.set(reg);
                  ctx.trans_since_wr_by_trans.set(reg);
               }
            }
         }

         if (state.program->wave_size == 64) {
            for (Operand& op : instr->operands) {
               /* This should ignore exec reads */
               if (!op.isConstant() && op.physReg().reg() < 126)
                  ctx.sgpr_read_by_valu_as_lanemask.reset();
            }

            if (!instr->definitions.empty() &&
                instr->definitions.back().getTemp().type() == RegType::sgpr &&
                check_written_regs(instr, ctx.sgpr_read_by_valu_as_lanemask)) {
               unsigned reg = instr->definitions.back().physReg().reg();
               for (unsigned i = 0; i < instr->definitions.back().size(); i++)
                  ctx.sgpr_read_by_valu_as_lanemask_then_wr_by_valu[reg + i] = 1;
            }

            switch (instr->opcode) {
            case aco_opcode::v_addc_co_u32:
            case aco_opcode::v_subb_co_u32:
            case aco_opcode::v_subbrev_co_u32:
            case aco_opcode::v_cndmask_b16:
            case aco_opcode::v_cndmask_b32:
            case aco_opcode::v_div_fmas_f32:
            case aco_opcode::v_div_fmas_f64:
               if (instr->operands.back().physReg() != exec) {
                  ctx.sgpr_read_by_valu_as_lanemask.set(instr->operands.back().physReg().reg());
                  ctx.sgpr_read_by_valu_as_lanemask.set(instr->operands.back().physReg().reg() + 1);
               }
               break;
            default: break;
            }
         }
      }
   } else {
      /* VALUReadSGPRHazard
       * VALU reads SGPR and later written by VALU/SALU cannot safely be read by VALU/SALU.
       */
      if (instr->isVALU() || instr->isSALU()) {
         unsigned expiry_count = instr->isSALU() ? 10 : 11;
         uint16_t imm = 0xffff;

         for (Operand& op : instr->operands) {
            if (op.physReg() >= m0)
               continue;

            for (unsigned i = 0; i < op.size(); i++) {
               PhysReg reg = op.physReg().advance(i * 4);
               if (ctx.sgpr_read_by_valu_then_wr_by_salu.get(reg) < expiry_count) {
                  imm &= 0xfffe;
                  wait.sa_sdst = 0;
               }
               if (instr->isVALU()) {
                  ctx.sgpr_read_by_valu.set(reg / 2);

                  /* s_wait_alu on va_sdst (if non-VCC SGPR) or va_vcc (if VCC SGPR) */
                  if (ctx.sgpr_read_by_valu_then_wr_by_valu[reg]) {
                     bool is_vcc = reg == vcc || reg == vcc_hi;
                     imm &= is_vcc ? 0xfffd : 0xf1ff;
                     if (is_vcc)
                        wait.va_vcc = 0;
                     else
                        wait.va_sdst = 0;
                  }
               }
            }
         }

         if (imm != 0xffff)
            bld.sopp(aco_opcode::s_waitcnt_depctr, imm);
      }

      if (wait.sa_sdst == 0)
         ctx.sgpr_read_by_valu_then_wr_by_salu.reset();
      else if (instr->isSALU() && !instr->isSOPP())
         ctx.sgpr_read_by_valu_then_wr_by_salu.inc();

      if (wait.va_sdst == 0) {
         std::bitset<m0.reg()> old = ctx.sgpr_read_by_valu_then_wr_by_valu;
         ctx.sgpr_read_by_valu_then_wr_by_valu.reset();
         ctx.sgpr_read_by_valu_then_wr_by_valu[vcc] = old[vcc];
         ctx.sgpr_read_by_valu_then_wr_by_valu[vcc_hi] = old[vcc_hi];
      }
      if (wait.va_vcc == 0) {
         ctx.sgpr_read_by_valu_then_wr_by_valu[vcc] = false;
         ctx.sgpr_read_by_valu_then_wr_by_valu[vcc_hi] = false;
      }

      if (instr->isVALU() && !instr->definitions.empty()) {
         PhysReg reg = instr->definitions.back().physReg();
         if (reg < m0 && ctx.sgpr_read_by_valu[reg / 2]) {
            for (unsigned i = 0; i < instr->definitions.back().size(); i++)
               ctx.sgpr_read_by_valu_then_wr_by_valu.set(reg + i);
         }
      } else if (instr->isSALU() && !instr->definitions.empty()) {
         PhysReg reg = instr->definitions[0].physReg();
         if (reg < m0 && ctx.sgpr_read_by_valu[reg / 2]) {
            for (unsigned i = 0; i < instr->definitions[0].size(); i++)
               ctx.sgpr_read_by_valu_then_wr_by_salu.set(reg.advance(i * 4));
         }
      }
   }

   /* LdsDirectVMEMHazard
    * Handle LDSDIR writing a VGPR after it's used by a VMEM/DS instruction.
    */
   if (instr->isVMEM() || instr->isFlatLike()) {
      if (instr->definitions.empty()) {
         for (Operand& op : instr->operands)
            fill_vgpr_bitset(ctx.vgpr_used_by_vmem_store, op.physReg(), op.bytes());
      } else {
         uint8_t vmem_type =
            state.program->gfx_level >= GFX12
               ? get_vmem_type(state.program->gfx_level, state.program->family, instr.get())
               : vmem_nosampler;
         std::bitset<256>* vgprs = &ctx.vgpr_used_by_vmem_load;
         if (vmem_type == vmem_sampler)
            vgprs = &ctx.vgpr_used_by_vmem_sample;
         else if (vmem_type == vmem_bvh)
            vgprs = &ctx.vgpr_used_by_vmem_bvh;

         for (Definition& def : instr->definitions)
            fill_vgpr_bitset(*vgprs, def.physReg(), def.bytes());
         for (Operand& op : instr->operands)
            fill_vgpr_bitset(*vgprs, op.physReg(), op.bytes());
      }
   }
   if (instr->isDS() || instr->isFlat()) {
      for (Definition& def : instr->definitions)
         fill_vgpr_bitset(ctx.vgpr_used_by_ds, def.physReg(), def.bytes());
      for (Operand& op : instr->operands)
         fill_vgpr_bitset(ctx.vgpr_used_by_ds, op.physReg(), op.bytes());
   }
   wait_imm imm;
   if (instr->isVALU() || instr->isEXP() || wait.vm_vsrc == 0) {
      ctx.vgpr_used_by_vmem_load.reset();
      ctx.vgpr_used_by_vmem_sample.reset();
      ctx.vgpr_used_by_vmem_bvh.reset();
      ctx.vgpr_used_by_vmem_store.reset();
      ctx.vgpr_used_by_ds.reset();
   } else if (imm.unpack(state.program->gfx_level, instr.get())) {
      if (imm.vm == 0)
         ctx.vgpr_used_by_vmem_load.reset();
      if (imm.sample == 0)
         ctx.vgpr_used_by_vmem_sample.reset();
      if (imm.bvh == 0)
         ctx.vgpr_used_by_vmem_bvh.reset();
      if (imm.lgkm == 0)
         ctx.vgpr_used_by_ds.reset();
      if (imm.vs == 0)
         ctx.vgpr_used_by_vmem_store.reset();
   }
   if (instr->isLDSDIR()) {
      if (ctx.vgpr_used_by_vmem_load[instr->definitions[0].physReg().reg() - 256] ||
          ctx.vgpr_used_by_vmem_sample[instr->definitions[0].physReg().reg() - 256] ||
          ctx.vgpr_used_by_vmem_bvh[instr->definitions[0].physReg().reg() - 256] ||
          ctx.vgpr_used_by_vmem_store[instr->definitions[0].physReg().reg() - 256] ||
          ctx.vgpr_used_by_ds[instr->definitions[0].physReg().reg() - 256]) {
         if (state.program->gfx_level >= GFX12)
            instr->ldsdir().wait_vsrc = 0;
         else
            bld.sopp(aco_opcode::s_waitcnt_depctr, 0xffe3);
         ctx.vgpr_used_by_vmem_load.reset();
         ctx.vgpr_used_by_vmem_sample.reset();
         ctx.vgpr_used_by_vmem_bvh.reset();
         ctx.vgpr_used_by_vmem_store.reset();
         ctx.vgpr_used_by_ds.reset();
      }
   }

   /* WMMA Hazards */
   if (instr_info.classes[(int)instr->opcode] == instr_class::wmma) {
      assert(instr->operands.back().isConstant() ||
             instr->operands.back().regClass() == instr->definitions[0].regClass());

      bool is_swmma = instr->operands.size() == 4;
      if (test_vgpr_bitset(ctx.vgpr_written_by_wmma, instr->operands[0]) ||
          test_vgpr_bitset(ctx.vgpr_written_by_wmma, instr->operands[1]) ||
          (is_swmma && test_vgpr_bitset(ctx.vgpr_written_by_wmma, instr->operands[2]))) {
         bld.vop1(aco_opcode::v_nop);
      }

      ctx.vgpr_written_by_wmma.reset();
      fill_vgpr_bitset(ctx.vgpr_written_by_wmma, instr->definitions[0].physReg(),
                       instr->definitions[0].bytes());
   } else if (instr->isVALU()) {
      ctx.vgpr_written_by_wmma.reset();
   }
}

bool
has_vdst0_since_valu_instr(bool& global_state, unsigned& block_state, aco_ptr<Instruction>& pred)
{
   if (parse_depctr_wait(pred.get()).va_vdst == 0)
      return true;

   if (--block_state == 0) {
      global_state = false;
      return true;
   }

   if (pred->isVALU()) {
      bool vgpr_rd_or_wr = false;
      for (Definition def : pred->definitions) {
         if (def.physReg().reg() >= 256)
            vgpr_rd_or_wr = true;
      }
      for (Operand op : pred->operands) {
         if (op.physReg().reg() >= 256)
            vgpr_rd_or_wr = true;
      }
      if (vgpr_rd_or_wr) {
         global_state = false;
         return true;
      }
   }

   return false;
}

void
resolve_all_gfx11(State& state, NOP_ctx_gfx11& ctx,
                  std::vector<aco_ptr<Instruction>>& new_instructions)
{
   Builder bld(state.program, &new_instructions);

   unsigned waitcnt_depctr = 0xffff;
   bool valu_read_sgpr = false;

   /* LdsDirectVALUHazard/VALUPartialForwardingHazard/VALUTransUseHazard */
   bool has_vdst0_since_valu = true;
   unsigned depth = 16;
   search_backwards<bool, unsigned, nullptr, has_vdst0_since_valu_instr>(
      state, has_vdst0_since_valu, depth);
   if (!has_vdst0_since_valu) {
      waitcnt_depctr &= 0x0fff;
      ctx.valu_since_wr_by_trans.reset();
      ctx.trans_since_wr_by_trans.reset();
   }

   /* VcmpxPermlaneHazard/WMMAHazards */
   if (ctx.has_Vcmpx || ctx.vgpr_written_by_wmma.any()) {
      ctx.has_Vcmpx = false;
      ctx.vgpr_written_by_wmma.reset();
      bld.vop1(aco_opcode::v_nop);
   }

   /* VALUMaskWriteHazard */
   if (state.program->gfx_level < GFX12 && state.program->wave_size == 64) {
      if (ctx.sgpr_read_by_valu_as_lanemask_then_wr_by_salu.any()) {
         waitcnt_depctr &= 0xfffe;
         ctx.sgpr_read_by_valu_as_lanemask_then_wr_by_salu.reset();
      }
      if (ctx.sgpr_read_by_valu_as_lanemask_then_wr_by_valu[vcc] ||
          ctx.sgpr_read_by_valu_as_lanemask_then_wr_by_valu[vcc_hi]) {
         waitcnt_depctr &= 0xfffd;
         ctx.sgpr_read_by_valu_as_lanemask_then_wr_by_valu[vcc] = false;
         ctx.sgpr_read_by_valu_as_lanemask_then_wr_by_valu[vcc_hi] = false;
      }
      if (ctx.sgpr_read_by_valu_as_lanemask_then_wr_by_valu.any()) {
         waitcnt_depctr &= 0xf1ff;
         ctx.sgpr_read_by_valu_as_lanemask_then_wr_by_valu.reset();
      }
      if (ctx.sgpr_read_by_valu_as_lanemask.any()) {
         valu_read_sgpr = true;
         ctx.sgpr_read_by_valu_as_lanemask.reset();
      }
   }

   /* VALUReadSGPRHazard */
   if (state.program->gfx_level >= GFX12) {
      if (!ctx.sgpr_read_by_valu_then_wr_by_salu.empty())
         waitcnt_depctr &= 0xfffe;

      ctx.sgpr_read_by_valu_then_wr_by_salu.reset();
      if (ctx.sgpr_read_by_valu_then_wr_by_valu[vcc] ||
          ctx.sgpr_read_by_valu_then_wr_by_valu[vcc_hi]) {
         waitcnt_depctr &= 0xfffd;
         ctx.sgpr_read_by_valu_then_wr_by_valu[vcc] = false;
         ctx.sgpr_read_by_valu_then_wr_by_valu[vcc_hi] = false;
      }
      if (ctx.sgpr_read_by_valu_then_wr_by_valu.any()) {
         waitcnt_depctr &= 0xf1ff;
         ctx.sgpr_read_by_valu_then_wr_by_valu.reset();
      }
   }

   /* LdsDirectVMEMHazard */
   if (ctx.vgpr_used_by_vmem_load.any() || ctx.vgpr_used_by_vmem_store.any() ||
       ctx.vgpr_used_by_ds.any() || ctx.vgpr_used_by_vmem_sample.any() ||
       ctx.vgpr_used_by_vmem_bvh.any()) {
      waitcnt_depctr &= 0xffe3;
      ctx.vgpr_used_by_vmem_load.reset();
      ctx.vgpr_used_by_vmem_store.reset();
      ctx.vgpr_used_by_ds.reset();
   }

   if (waitcnt_depctr != 0xffff)
      bld.sopp(aco_opcode::s_waitcnt_depctr, waitcnt_depctr);

   if (valu_read_sgpr) {
      /* This has to be after the s_waitcnt_depctr so that the instruction is not involved in any
       * other hazards. */
      bld.vop3(aco_opcode::v_xor3_b32, Definition(PhysReg(256), v1), Operand(PhysReg(256), v1),
               Operand(PhysReg(0), s1), Operand(PhysReg(0), s1));

      /* workaround possible LdsDirectVALUHazard/VALUPartialForwardingHazard */
      bld.sopp(aco_opcode::s_waitcnt_depctr, 0x0fff);
   }
}

template <typename Ctx>
using HandleInstr = void (*)(State& state, Ctx&, aco_ptr<Instruction>&,
                             std::vector<aco_ptr<Instruction>>&);

template <typename Ctx>
using ResolveAll = void (*)(State& state, Ctx&, std::vector<aco_ptr<Instruction>>&);

template <typename Ctx, HandleInstr<Ctx> Handle, ResolveAll<Ctx> Resolve>
void
handle_block(Program* program, Ctx& ctx, Block& block)
{
   if (block.instructions.empty())
      return;

   State state;
   state.program = program;
   state.block = &block;
   state.old_instructions = std::move(block.instructions);

   block.instructions.clear(); // Silence clang-analyzer-cplusplus.Move warning
   block.instructions.reserve(state.old_instructions.size());

   bool found_end = false;
   for (aco_ptr<Instruction>& instr : state.old_instructions) {
      Handle(state, ctx, instr, block.instructions);

      /* Resolve all possible hazards (we don't know what s_setpc_b64 jumps to). */
      if (instr->opcode == aco_opcode::s_setpc_b64) {
         block.instructions.emplace_back(std::move(instr));

         std::vector<aco_ptr<Instruction>> resolve_instrs;
         Resolve(state, ctx, resolve_instrs);
         block.instructions.insert(std::prev(block.instructions.end()),
                                   std::move_iterator(resolve_instrs.begin()),
                                   std::move_iterator(resolve_instrs.end()));

         found_end = true;
         continue;
      }

      found_end |= instr->opcode == aco_opcode::s_endpgm;
      block.instructions.emplace_back(std::move(instr));
   }

   /* Resolve all possible hazards (we don't know what the shader is concatenated with). */
   if (block.linear_succs.empty() && !found_end)
      Resolve(state, ctx, block.instructions);
}

template <typename Ctx, HandleInstr<Ctx> Handle, ResolveAll<Ctx> Resolve>
void
mitigate_hazards(Program* program, Ctx initial_ctx = Ctx())
{
   std::vector<Ctx> all_ctx(program->blocks.size());
   std::stack<unsigned, std::vector<unsigned>> loop_header_indices;

   for (unsigned i = 0; i < program->blocks.size(); i++) {
      Block& block = program->blocks[i];
      Ctx& ctx = all_ctx[i];

      if (i == 0 || (block.kind & block_kind_resume))
         ctx = initial_ctx;

      if (block.kind & block_kind_loop_header) {
         loop_header_indices.push(i);
      } else if (block.kind & block_kind_loop_exit) {
         /* Go through the whole loop again */
         for (unsigned idx = loop_header_indices.top(); idx < i; idx++) {
            Ctx loop_block_ctx;
            for (unsigned b : program->blocks[idx].linear_preds)
               loop_block_ctx.join(all_ctx[b]);

            handle_block<Ctx, Handle, Resolve>(program, loop_block_ctx, program->blocks[idx]);

            /* We only need to continue if the loop header context changed */
            if (idx == loop_header_indices.top() && loop_block_ctx == all_ctx[idx])
               break;

            all_ctx[idx] = loop_block_ctx;
         }

         loop_header_indices.pop();
      }

      for (unsigned b : block.linear_preds)
         ctx.join(all_ctx[b]);

      handle_block<Ctx, Handle, Resolve>(program, ctx, block);
   }
}

/* FeatureRequiredExportPriority in LLVM */
void
required_export_priority(Program* program)
{
   /* Skip callees, assuming that the caller has already increased the priority. */
   bool increase_priority = !program->is_epilog && !program->info.vs.has_prolog &&
                            (!program->info.merged_shader_compiled_separately ||
                             program->stage.sw == SWStage::VS || program->stage.sw == SWStage::TES);
   increase_priority |= program->is_prolog;

   for (Block& block : program->blocks) {
      std::vector<aco_ptr<Instruction>> new_instructions;
      new_instructions.reserve(block.instructions.size() + 6);

      Builder bld(program, &new_instructions);

      if (increase_priority && block.index == 0) {
         if (!block.instructions.empty() && block.instructions[0]->opcode == aco_opcode::s_setprio)
            block.instructions[0]->salu().imm = MAX2(block.instructions[0]->salu().imm, 2);
         else
            bld.sopp(aco_opcode::s_setprio, 2);
      }

      for (unsigned i = 0; i < block.instructions.size(); i++) {
         Instruction* instr = block.instructions[i].get();
         new_instructions.push_back(std::move(block.instructions[i]));

         if (instr->opcode == aco_opcode::s_setprio) {
            instr->salu().imm = MAX2(instr->salu().imm, 2);
            continue;
         }

         bool end_of_export_sequence = instr->isEXP() && (i == block.instructions.size() - 1 ||
                                                          !block.instructions[i + 1]->isEXP());
         if (!end_of_export_sequence)
            continue;

         bool before_endpgm = false;
         if (i != block.instructions.size() - 1) {
            before_endpgm = block.instructions[i + 1]->opcode == aco_opcode::s_endpgm;
         } else {
            /* Does this fallthrough to a s_endpgm? */
            for (unsigned j = block.index + 1; j < program->blocks.size(); j++) {
               if (program->blocks[j].instructions.size() == 1 &&
                   program->blocks[j].instructions[0]->opcode == aco_opcode::s_endpgm)
                  before_endpgm = true;
               if (!program->blocks[j].instructions.empty())
                  break;
            }
         }

         bld.sopp(aco_opcode::s_setprio, 0);
         if (!before_endpgm)
            bld.sopk(aco_opcode::s_waitcnt_expcnt, Operand(sgpr_null, s1), 0);
         bld.sopp(aco_opcode::s_nop, 0);
         bld.sopp(aco_opcode::s_nop, 0);
         if (!before_endpgm)
            bld.sopp(aco_opcode::s_setprio, 2);
      }

      block.instructions = std::move(new_instructions);
   }
}

} /* end namespace */

void
insert_NOPs(Program* program)
{
   if (program->gfx_level >= GFX11) {
      NOP_ctx_gfx11 initial_ctx;

      bool has_previous_part =
         program->is_epilog || program->info.vs.has_prolog || program->info.ps.has_prolog ||
         (program->info.merged_shader_compiled_separately && program->stage.sw != SWStage::VS &&
          program->stage.sw != SWStage::TES) || program->stage == raytracing_cs;
      if (program->gfx_level >= GFX12 && has_previous_part) {
         /* resolve_all_gfx11 can't resolve VALUReadSGPRHazard entirely. We have to assume that any
          * SGPR might have been read by VALU if there was a previous shader part.
          */
         initial_ctx.sgpr_read_by_valu.flip();
      }

      mitigate_hazards<NOP_ctx_gfx11, handle_instruction_gfx11, resolve_all_gfx11>(program,
                                                                                   initial_ctx);
   } else if (program->gfx_level >= GFX10_3) {
      ; /* no hazards/bugs to mitigate */
   } else if (program->gfx_level >= GFX10) {
      mitigate_hazards<NOP_ctx_gfx10, handle_instruction_gfx10, resolve_all_gfx10>(program);
   } else {
      mitigate_hazards<NOP_ctx_gfx6, handle_instruction_gfx6, resolve_all_gfx6>(program);
   }

   if (program->gfx_level == GFX11_5 && (program->stage.hw == AC_HW_NEXT_GEN_GEOMETRY_SHADER ||
                                         program->stage.hw == AC_HW_PIXEL_SHADER))
      required_export_priority(program);
}

} // namespace aco
