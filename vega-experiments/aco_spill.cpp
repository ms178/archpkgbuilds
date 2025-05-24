/*
 * Copyright © 2018 Valve Corporation
 * Copyright © 2018 Google
 *
 * SPDX-License-Identifier: MIT
 */

#include "aco_builder.h"
#include "aco_ir.h"
#include "aco_util.h"

#include "common/ac_descriptors.h"
#include "common/sid.h"

#include <algorithm>
#include <cstring>
#include <cfloat>
#include <map>
#include <optional>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <limits.h>

#define LIKELY(x)   __builtin_expect(!!(x),1)
#define UNLIKELY(x) __builtin_expect(!!(x),0)

#ifndef ACO_TINY_BITMAP_DEFINED
#define ACO_TINY_BITMAP_DEFINED
struct tiny_bitmap {
      /* packed words – 64 spill-slot flags per entry */
      std::vector<uint64_t> words;

      ALWAYS_INLINE void ensure(unsigned bit)
      {
            if (bit >> 6 >= words.size())
                  words.resize((bit >> 6) + 1, 0ull);
      }

      ALWAYS_INLINE void set(unsigned bit)
      {
            ensure(bit);
            words[bit >> 6] |= 1ull << (bit & 63u);
      }

      ALWAYS_INLINE bool test(unsigned bit) const
      {
            unsigned idx = bit >> 6;
            return idx < words.size() && (words[idx] & (1ull << (bit & 63u)));
      }

      /*  ──►  THIS helper was missing / outside the struct  ◄── */
      ALWAYS_INLINE uint64_t word(unsigned idx) const
      {
            return idx < words.size() ? words[idx] : 0ull;
      }

      ALWAYS_INLINE void clear() { words.clear(); }

      ALWAYS_INLINE unsigned max_marked_plus_one() const
      {
            for (int i = (int)words.size() - 1; i >= 0; --i)
                  if (uint64_t w = words[i])
                        return (i << 6) + 64 - unsigned(__builtin_clzll(w));
            return 0;
      }
};
#endif /* ACO_TINY_BITMAP_DEFINED */

namespace std {
      template <>
      struct hash<aco::Temp> {
            ALWAYS_INLINE size_t operator()(aco::Temp t) const noexcept
            {
                  return std::hash<uint32_t>{}(t.id());
            }
      };
} // namespace std

/* Ensure Temp's size assumption for hashing remains valid. */
static_assert(sizeof(aco::Temp) == sizeof(uint32_t),
              "Temp stopped being 32-bit – revisit hash.");

namespace aco {

      namespace {

            struct remat_info {
                  Instruction* instr;
            };

            struct loop_info {
                  uint32_t index;
                  aco::unordered_map<Temp, uint32_t> spills;
                  IDSet live_in;
            };

            struct use_info {
                  uint32_t num_uses = 0;
                  uint32_t last_use = 0;
                  float precomputed_score = 0.0f;
                  float score()
                  {
                        /* This function is now only called if precomputed_score wasn't calculated,
                         * or as a fallback. Normally, precomputed_score should be used.
                         */
                        if (num_uses == 0)
                              return FLT_MAX; // Or a very large number indicating high spill priority
                              return static_cast<float>(last_use) / static_cast<float>(num_uses);
                  }
            };

            struct spill_ctx {
                  RegisterDemand target_pressure;
                  Program* program;
                  aco::monotonic_buffer_resource memory;

                  std::vector<aco::map<Temp, Temp>> renames;
                  std::vector<aco::unordered_map<Temp, uint32_t>> spills_entry;
                  std::vector<aco::unordered_map<Temp, uint32_t>> spills_exit;

                  std::vector<bool> processed;
                  std::vector<loop_info> loop;

                  std::vector<use_info> ssa_infos;
                  std::vector<std::pair<RegClass, std::vector<uint32_t>>> interferences;
                  std::vector<uint32_t> id2grp;
                  std::vector<std::vector<uint32_t>> affinities;
                  bool interferences_finalized_for_assignment = false;

                  std::vector<bool> is_reloaded;
                  aco::unordered_map<Temp, remat_info> remat;
                  std::set<Instruction*> unused_remats;
                  uint32_t wave_size; // Cached wave_size

                  unsigned sgpr_spill_slots;
                  unsigned vgpr_spill_slots;
                  Temp scratch_rsrc;

                  unsigned resume_idx;
                  std::vector<uint32_t> logical_end_pos;

                  spill_ctx(const RegisterDemand target_pressure_, Program* program_)
                  : target_pressure(target_pressure_), program(program_), memory(),
                  renames(program->blocks.size(), aco::map<Temp, Temp>(memory)),
                  spills_entry(program->blocks.size(), aco::unordered_map<Temp, uint32_t>(memory)),
                  spills_exit(program->blocks.size(), aco::unordered_map<Temp, uint32_t>(memory)),
                  processed(program->blocks.size(), false),
                  loop(),
                  ssa_infos(program->peekAllocationId()),
                  interferences(),
                  id2grp(),
                  affinities(),
                  is_reloaded(), remat(memory),
                  wave_size(program->wave_size), sgpr_spill_slots(0), vgpr_spill_slots(0), resume_idx(0),
                  logical_end_pos(program->blocks.size(), UINT32_MAX)
                              {
                                    for (size_t i = 0; i < program->blocks.size(); ++i) {
                                    spills_entry[i].reserve(8);
                                    spills_exit[i].reserve(8);
                                    }

                                    for (Block& b : program->blocks) {
                                          logical_end_pos[b.index] = UINT32_MAX;
                                          if (b.instructions.empty())
                                                continue;
                                          for (int i = b.instructions.size() - 1; i >= 0; --i) {
                                                if (b.instructions[i]->opcode == aco_opcode::p_logical_end) {
                                                      logical_end_pos[b.index] = i;
                                                      break;
                                                }
                                          }
                                    }
                              }

                              ALWAYS_INLINE std::vector<aco_ptr<Instruction>>::iterator
                              get_insert_point_before_logical_end(Block& blk)
                              {
                                    unsigned k = logical_end_pos[blk.index];
                                    if (k == UINT32_MAX)
                                          k = blk.instructions.size();
                                          auto it = std::next(blk.instructions.begin(), k);
                                          if (logical_end_pos[blk.index] != UINT32_MAX &&
                                              logical_end_pos[blk.index] < blk.instructions.size())
                                              ++logical_end_pos[blk.index];
                                    return it;
                              }


                              void add_affinity(uint32_t a, uint32_t b)
                              {
                                    if (a == b)
                                          return;

                                    uint32_t ga = id2grp[a];
                                    uint32_t gb = id2grp[b];

                                    if (ga == UINT32_MAX && gb == UINT32_MAX) {
                                          affinities.emplace_back();
                                          affinities.back().push_back(a);
                                          affinities.back().push_back(b);
                                          id2grp[a] = id2grp[b] = affinities.size() - 1;
                                          return;
                                    }
                                    if (ga == UINT32_MAX) {
                                          affinities[gb].push_back(a);
                                          id2grp[a] = gb;
                                          return;
                                    }
                                    if (gb == UINT32_MAX) {
                                          affinities[ga].push_back(b);
                                          id2grp[b] = ga;
                                          return;
                                    }
                                    if (ga == gb)
                                          return;

                                    uint32_t dst_grp_idx = affinities[ga].size() < affinities[gb].size() ? gb : ga;
                                    uint32_t src_grp_idx = dst_grp_idx == ga ? gb : ga;

                                    std::vector<uint32_t>& dst_vec = affinities[dst_grp_idx];
                                    std::vector<uint32_t>& src_vec = affinities[src_grp_idx];

                                    dst_vec.reserve(dst_vec.size() + src_vec.size());
                                    for (uint32_t id : src_vec) {
                                          dst_vec.push_back(id);
                                          id2grp[id] = dst_grp_idx;
                                    }
                                    src_vec.clear();
                                    src_vec.shrink_to_fit(); // Optional: if memory for empty groups is a concern
                              }

                              void add_interference(uint32_t first, uint32_t second)
                              {
                                    if (interferences[first].first.type() != interferences[second].first.type())
                                          return;

                                    interferences[first].second.push_back(second);
                                    interferences[second].second.push_back(first);
                              }

                              uint32_t allocate_spill_id(RegClass rc)
                              {
                                    interferences.emplace_back(rc, std::vector<uint32_t>());
                                    interferences.back().second.reserve(16); // Pre-reserve typical interference list size
                                    is_reloaded.push_back(false);
                                    id2grp.push_back(UINT32_MAX);
                                    return next_spill_id++;
                              }

                              uint32_t add_to_spills(Temp to_spill,
                                                     aco::unordered_map<Temp, uint32_t>& spills)
                              {
                                    const uint32_t spill_id = allocate_spill_id(to_spill.regClass());

                                    for (auto [_, id] : spills)
                                          add_interference(spill_id, id);

                                    if (!loop.empty()) {
                                          for (auto [_, id] : loop.back().spills)
                                                add_interference(spill_id, id);
                                    }

                                    spills[to_spill] = spill_id;
                                    return spill_id;
                              }

                              uint32_t next_spill_id = 0;
            };

            void
            gather_ssa_use_info(spill_ctx& ctx)
            {
                  unsigned instruction_idx = 0;
                  for (Block& block : ctx.program->blocks) {
                        for (int i = block.instructions.size() - 1; i >= 0; i--) {
                              aco_ptr<Instruction>& instr = block.instructions[i];
                              for (const Operand& op : instr->operands) {
                                    if (op.isTemp()) {
                                          use_info& info = ctx.ssa_infos[op.tempId()];
                                          info.num_uses++;
                                          info.last_use = std::max(info.last_use, instruction_idx + i);
                                    }
                              }
                        }

                        if (block.kind & block_kind_loop_header) {
                              for (unsigned t : ctx.program->live.live_in[block.index])
                                    ctx.ssa_infos[t].num_uses++;
                        }
                        instruction_idx += block.instructions.size();
                  }

                  for (size_t i = 0; i < ctx.ssa_infos.size(); ++i) {
                        use_info& info = ctx.ssa_infos[i];
                        if (info.num_uses > 0) {
                              info.precomputed_score = static_cast<float>(info.last_use) / static_cast<float>(info.num_uses);
                        } else {
                              info.precomputed_score = FLT_MAX; // Higher score = better spill candidate
                        }
                  }
            }

            bool
            ALWAYS_INLINE should_rematerialize(aco_ptr<Instruction>& instr)
            {
                  if (instr->format != Format::VOP1 && instr->format != Format::SOP1 &&
                        instr->format != Format::PSEUDO && instr->format != Format::SOPK)
                        return false;
                  if (instr->isPseudo() && instr->opcode != aco_opcode::p_create_vector &&
                        instr->opcode != aco_opcode::p_parallelcopy)
                        return false;
                  if (instr->isSOPK() && instr->opcode != aco_opcode::s_movk_i32)
                        return false;

                  for (const Operand& op : instr->operands) {
                        if (!op.isConstant())
                              return false;
                  }

                  if (instr->definitions.size() > 1)
                        return false;

                  return true;
            }

            aco_ptr<Instruction>
            do_reload(spill_ctx& ctx, Temp tmp, Temp new_name, uint32_t spill_id)
            {
                  std::unordered_map<Temp, remat_info>::iterator remat = ctx.remat.find(tmp);
                  if (remat != ctx.remat.end()) {
                        Instruction* instr = remat->second.instr;
                        assert((instr->isVOP1() || instr->isSOP1() || instr->isPseudo() || instr->isSOPK()) &&
                        "unsupported");
                        assert((instr->format != Format::PSEUDO || instr->opcode == aco_opcode::p_create_vector ||
                        instr->opcode == aco_opcode::p_parallelcopy) &&
                        "unsupported");
                        assert(instr->definitions.size() == 1 && "unsupported");

                        aco_ptr<Instruction> res;
                        res.reset(create_instruction(instr->opcode, instr->format, instr->operands.size(),
                                                     instr->definitions.size()));
                        if (instr->isSOPK())
                              res->salu().imm = instr->salu().imm;

                        for (unsigned i = 0; i < instr->operands.size(); i++) {
                              res->operands[i] = instr->operands[i];
                              if (instr->operands[i].isTemp()) {
                                    assert(false && "unsupported");
                                    if (ctx.remat.count(instr->operands[i].getTemp()))
                                          ctx.unused_remats.erase(ctx.remat[instr->operands[i].getTemp()].instr);
                              }
                        }
                        res->definitions[0] = Definition(new_name);
                        return res;
                  } else {
                        aco_ptr<Instruction> reload{create_instruction(aco_opcode::p_reload, Format::PSEUDO, 1, 1)};
                        reload->operands[0] = Operand::c32(spill_id);
                        reload->definitions[0] = Definition(new_name);
                        ctx.is_reloaded[spill_id] = true;
                        return reload;
                  }
            }

            void
            get_rematerialize_info(spill_ctx& ctx)
            {
                  for (Block& block : ctx.program->blocks) {
                        bool logical = false;
                        for (aco_ptr<Instruction>& instr : block.instructions) {
                              if (instr->opcode == aco_opcode::p_logical_start)
                                    logical = true;
                              else if (instr->opcode == aco_opcode::p_logical_end)
                                    logical = false;
                              if (logical && should_rematerialize(instr)) {
                                    for (const Definition& def : instr->definitions) {
                                          if (def.isTemp()) {
                                                ctx.remat[def.getTemp()] = remat_info{instr.get()};
                                                ctx.unused_remats.insert(instr.get());
                                          }
                                    }
                              }
                        }
                  }
            }

            RegisterDemand
            init_live_in_vars(spill_ctx& ctx, Block* block, unsigned block_idx)
            {
                  RegisterDemand spilled_registers;

                  if (block->linear_preds.empty())
                        return {0, 0};

                  const IDSet& live_in = ctx.program->live.live_in[block_idx];

                  if (block->kind & block_kind_loop_header) {
                        assert(block->linear_preds[0] == block_idx - 1);
                        assert(block->logical_preds[0] == block_idx - 1);

                        RegisterDemand reg_pressure = block->live_in_demand;
                        RegisterDemand loop_demand = reg_pressure;
                        unsigned i = block_idx;
                        while (ctx.program->blocks[i].loop_nest_depth >= block->loop_nest_depth)
                              loop_demand.update(ctx.program->blocks[i++].register_demand);

                        for (auto spilled : ctx.spills_exit[block_idx - 1]) {
                              if (!live_in.count(spilled.first.id()))
                                    continue;

                              ctx.spills_entry[block_idx][spilled.first] = spilled.second;
                              spilled_registers += spilled.first;
                              loop_demand -= spilled.first;
                        }
                        if (!ctx.loop.empty()) {
                              for (auto spilled : ctx.loop.back().spills) {
                                    if (live_in.count(spilled.first.id()) &&
                                          ctx.spills_entry[block_idx].insert(spilled).second) {
                                          spilled_registers += spilled.first;
                                    loop_demand -= spilled.first;
                                          }
                              }
                        }

                        RegType type = RegType::vgpr;
                        while (loop_demand.exceeds(ctx.target_pressure)) {
                              if (type == RegType::vgpr && loop_demand.vgpr <= ctx.target_pressure.vgpr)
                                    type = RegType::sgpr;
                              if (type == RegType::sgpr && loop_demand.sgpr <= ctx.target_pressure.sgpr)
                                    break;

                              float score = 0.0;
                              unsigned remat = 0;
                              Temp to_spill;
                              for (unsigned t : live_in) {
                                    Temp var = Temp(t, ctx.program->temp_rc[t]);
                                    if (var.type() != type || ctx.spills_entry[block_idx].count(var) ||
                                          var.regClass().is_linear_vgpr())
                                          continue;

                                    unsigned can_remat = ctx.remat.count(var);
                                    if (can_remat > remat || (can_remat == remat && ctx.ssa_infos[t].precomputed_score > score)) {
                                          to_spill = var;
                                          score = ctx.ssa_infos[t].precomputed_score;
                                          remat = can_remat;
                                    }
                              }

                              if (score == 0.0) {
                                    if (type == RegType::sgpr)
                                          break;
                                    type = RegType::sgpr;
                                    continue;
                              }

                              ctx.add_to_spills(to_spill, ctx.spills_entry[block_idx]);
                              spilled_registers += to_spill;
                              loop_demand -= to_spill;
                        }

                        loop_info info = {block_idx, ctx.spills_entry[block_idx], live_in};
                        ctx.loop.emplace_back(std::move(info));

                        if (!loop_demand.exceeds(ctx.target_pressure))
                              return spilled_registers;

                        reg_pressure -= spilled_registers;

                        while (reg_pressure.exceeds(ctx.target_pressure)) {
                              float score = 0;
                              Temp to_spill = Temp();
                              type = reg_pressure.vgpr > ctx.target_pressure.vgpr ? RegType::vgpr : RegType::sgpr;
                              for (aco_ptr<Instruction>& phi : block->instructions) {
                                    if (UNLIKELY(!is_phi(phi)))
                                          break;
                                    if (!phi->definitions[0].isTemp() || phi->definitions[0].isKill())
                                          continue;
                                    Temp var = phi->definitions[0].getTemp();
                                    if (var.type() == type && !ctx.spills_entry[block_idx].count(var) &&
                                          ctx.ssa_infos[var.id()].precomputed_score > score) {
                                          to_spill = var;
                                    score = ctx.ssa_infos[var.id()].precomputed_score;
                                          }
                              }
                              assert(to_spill != Temp());
                              ctx.add_to_spills(to_spill, ctx.spills_entry[block_idx]);
                              spilled_registers += to_spill;
                              reg_pressure -= to_spill;
                        }

                        return spilled_registers;
                  }

                  if (block->linear_preds.size() == 1 && !(block->kind & block_kind_loop_exit)) {
                        unsigned pred_idx = block->linear_preds[0];
                        for (std::pair<Temp, uint32_t> pair : ctx.spills_exit[pred_idx]) {
                              if (pair.first.type() != RegType::sgpr)
                                    continue;

                              if (live_in.count(pair.first.id())) {
                                    spilled_registers += pair.first;
                                    ctx.spills_entry[block_idx].emplace(pair);
                              }
                        }

                        if (block->logical_preds.empty())
                              return spilled_registers;

                        pred_idx = block->logical_preds[0];
                        for (std::pair<Temp, uint32_t> pair : ctx.spills_exit[pred_idx]) {
                              if (pair.first.type() != RegType::vgpr)
                                    continue;

                              if (live_in.count(pair.first.id())) {
                                    spilled_registers += pair.first;
                                    ctx.spills_entry[block_idx].emplace(pair);
                              }
                        }

                        return spilled_registers;
                  }

                  aco::unordered_map<Temp, bool> partial_spills{ctx.memory};

                  for (unsigned t : live_in) {
                        const RegClass rc = ctx.program->temp_rc[t];
                        Temp var = Temp(t, rc);
                        Block::edge_vec& preds = rc.is_linear() ? block->linear_preds : block->logical_preds;

                        const bool remat = ctx.remat.count(var);
                        const bool avoid_respill = block->loop_nest_depth && ctx.loop.back().spills.count(var);
                        bool spill = true;
                        bool partial_spill = false;
                        uint32_t spill_id = 0;
                        for (unsigned pred_idx : preds) {
                              if (!ctx.spills_exit[pred_idx].count(var)) {
                                    spill = false;
                              } else {
                                    partial_spill = true;
                                    spill_id = ctx.spills_exit[pred_idx][var];
                              }
                        }
                        spill |= (remat && partial_spill);
                        spill |= (avoid_respill && partial_spill);
                        if (spill) {
                              ctx.spills_entry[block_idx][var] = spill_id;
                              partial_spills.erase(var);
                              spilled_registers += var;
                        } else {
                              partial_spills[var] = partial_spill;
                        }
                  }

                  for (aco_ptr<Instruction>& phi : block->instructions) {
                        if (UNLIKELY(!is_phi(phi)))
                              break;
                        if (!phi->definitions[0].isTemp() || phi->definitions[0].isKill())
                              continue;

                        Block::edge_vec& preds =
                        phi->opcode == aco_opcode::p_phi ? block->logical_preds : block->linear_preds;
                        bool is_all_undef = true;
                        bool is_all_spilled = true;
                        bool is_partial_spill = false;
                        for (unsigned i = 0; i < phi->operands.size(); i++) {
                              if (phi->operands[i].isUndefined())
                                    continue;
                              bool spilled = phi->operands[i].isTemp() &&
                              ctx.spills_exit[preds[i]].count(phi->operands[i].getTemp());
                              is_all_spilled &= spilled;
                              is_partial_spill |= spilled;
                              is_all_undef = false;
                        }

                        if (is_all_spilled && !is_all_undef) {
                              ctx.add_to_spills(phi->definitions[0].getTemp(), ctx.spills_entry[block_idx]);
                              spilled_registers += phi->definitions[0].getTemp();
                              partial_spills.erase(phi->definitions[0].getTemp());
                        } else {
                              partial_spills[phi->definitions[0].getTemp()] = is_partial_spill;
                        }
                  }

                  RegisterDemand reg_pressure = block->live_in_demand;
                  reg_pressure -= spilled_registers;

                  while (reg_pressure.exceeds(ctx.target_pressure)) {
                        assert(!partial_spills.empty());
                        auto it = partial_spills.begin();
                        Temp to_spill = Temp();
                        bool is_partial_spill = false;
                        float score = 0.0;
                        RegType type = reg_pressure.vgpr > ctx.target_pressure.vgpr ? RegType::vgpr : RegType::sgpr;

                        while (it != partial_spills.end()) {
                              assert(!ctx.spills_entry[block_idx].count(it->first));

                              if (it->first.type() == type && !it->first.regClass().is_linear_vgpr() &&
                                    ((it->second && !is_partial_spill) ||
                                    (it->second == is_partial_spill &&
                                    ctx.ssa_infos[it->first.id()].precomputed_score > score))) {
                                    score = ctx.ssa_infos[it->first.id()].precomputed_score;
                              to_spill = it->first;
                              is_partial_spill = it->second;
                                    }
                                    ++it;
                        }
                        assert(to_spill != Temp());
                        ctx.add_to_spills(to_spill, ctx.spills_entry[block_idx]);
                        partial_spills.erase(to_spill);
                        spilled_registers += to_spill;
                        reg_pressure -= to_spill;
                  }

                  return spilled_registers;
            }

            void
            add_coupling_code(spill_ctx& ctx, Block* block, IDSet& live_in)
            {
                  const unsigned block_idx = block->index;
                  if (block->linear_preds.empty())
                        return;

                  if (block->linear_preds.size() == 1 &&
                        !(block->kind & (block_kind_loop_exit | block_kind_loop_header))) {
                        assert(ctx.processed[block->linear_preds[0]]);

                  ctx.renames[block_idx] = ctx.renames[block->linear_preds[0]];
                  if (!block->logical_preds.empty() && block->logical_preds[0] != block->linear_preds[0]) {
                        for (auto it : ctx.renames[block->logical_preds[0]]) {
                              if (it.first.type() == RegType::vgpr)
                                    ctx.renames[block_idx].insert_or_assign(it.first, it.second);
                        }
                  }
                  return;
                        }

                        std::vector<aco_ptr<Instruction>> instructions;

                        for (ASSERTED unsigned pred : block->linear_preds)
                              assert(ctx.processed[pred]);

                  for (aco_ptr<Instruction>& phi : block->instructions) {
                        if (UNLIKELY(!is_phi(phi)))
                              break;

                        for (const Operand& op : phi->operands) {
                              if (op.isTemp())
                                    ctx.ssa_infos[op.tempId()].num_uses--;
                        }

                        if (!phi->definitions[0].isTemp() ||
                              !ctx.spills_entry[block_idx].count(phi->definitions[0].getTemp()))
                              continue;

                        Block::edge_vec& preds =
                        phi->opcode == aco_opcode::p_phi ? block->logical_preds : block->linear_preds;
                        uint32_t def_spill_id = ctx.spills_entry[block_idx][phi->definitions[0].getTemp()];
                        phi->definitions[0].setKill(true);

                        for (unsigned i = 0; i < phi->operands.size(); i++) {
                              if (phi->operands[i].isUndefined())
                                    continue;

                              unsigned pred_idx = preds[i];
                              Operand spill_op = phi->operands[i];
                              phi->operands[i] = Operand(phi->definitions[0].regClass());

                              if (spill_op.isTemp()) {
                                    assert(spill_op.isKill());
                                    Temp var = spill_op.getTemp();

                                    std::map<Temp, Temp>::iterator rename_it = ctx.renames[pred_idx].find(var);
                                    if (rename_it == ctx.renames[preds[i]].end() && ctx.remat.count(var))
                                          ctx.unused_remats.erase(ctx.remat[var].instr);

                                    auto spilled = ctx.spills_exit[pred_idx].find(var);
                                    if (spilled != ctx.spills_exit[pred_idx].end()) {
                                          if (spilled->second != def_spill_id)
                                                ctx.add_affinity(def_spill_id, spilled->second);
                                          continue;
                                    }

                                    if (var == phi->definitions[0].getTemp())
                                          ctx.spills_exit[pred_idx][var] = def_spill_id;

                                    if (rename_it != ctx.renames[pred_idx].end()) {
                                          spill_op.setTemp(rename_it->second);
                                          ctx.renames[pred_idx].erase(rename_it);
                                    }
                              }

                              for (std::pair<Temp, uint32_t> pair : ctx.spills_exit[pred_idx])
                                    ctx.add_interference(def_spill_id, pair.second);

                              aco_ptr<Instruction> spill{create_instruction(aco_opcode::p_spill, Format::PSEUDO, 2, 0)};
                              spill->operands[0] = spill_op;
                              spill->operands[1] = Operand::c32(def_spill_id);
                              Block& pred = ctx.program->blocks[pred_idx];
                              std::vector<aco_ptr<Instruction>>::iterator it;
                              if (phi->opcode == aco_opcode::p_phi) {
                                    it = ctx.get_insert_point_before_logical_end(pred);
                              } else {
                                    it = std::prev(pred.instructions.end()); // Linear phis insert before terminator
                              }
                              pred.instructions.insert(it, std::move(spill));
                        }
                  }

                  for (std::pair<Temp, uint32_t> pair : ctx.spills_entry[block_idx]) {
                        if (!live_in.count(pair.first.id()))
                              continue;

                        Block::edge_vec& preds = pair.first.is_linear() ? block->linear_preds : block->logical_preds;
                        for (unsigned pred_idx : preds) {
                              auto spilled = ctx.spills_exit[pred_idx].find(pair.first);
                              if (spilled != ctx.spills_exit[pred_idx].end()) {
                                    if (spilled->second != pair.second)
                                          ctx.add_affinity(pair.second, spilled->second);
                                    continue;
                              }

                              const uint32_t loop_nest_depth = std::min(ctx.program->blocks[pred_idx].loop_nest_depth,
                                                                        ctx.program->blocks[block_idx].loop_nest_depth);
                              if (loop_nest_depth) {
                                    auto spill = ctx.loop[loop_nest_depth - 1].spills.find(pair.first);
                                    if (spill != ctx.loop[loop_nest_depth - 1].spills.end() && spill->second == pair.second)
                                          continue;
                              }

                              for (std::pair<Temp, uint32_t> exit_spill : ctx.spills_exit[pred_idx])
                                    ctx.add_interference(exit_spill.second, pair.second);

                              Temp var = pair.first;
                              std::map<Temp, Temp>::iterator rename_it = ctx.renames[pred_idx].find(var);
                              if (rename_it != ctx.renames[pred_idx].end()) {
                                    var = rename_it->second;
                                    ctx.renames[pred_idx].erase(rename_it);
                              }

                              aco_ptr<Instruction> spill{create_instruction(aco_opcode::p_spill, Format::PSEUDO, 2, 0)};
                              spill->operands[0] = Operand(var);
                              spill->operands[1] = Operand::c32(pair.second);
                              Block& pred = ctx.program->blocks[pred_idx];
                              std::vector<aco_ptr<Instruction>>::iterator it;
                              if (pair.first.type() == RegType::vgpr) {
                                    it = ctx.get_insert_point_before_logical_end(pred);
                              } else {
                                    it = std::prev(pred.instructions.end());
                              }
                              pred.instructions.insert(it, std::move(spill));
                        }
                  }

                  for (aco_ptr<Instruction>& phi : block->instructions) {
                        if (UNLIKELY(!is_phi(phi)))
                              break;
                        if (phi->definitions[0].isKill())
                              continue;

                        assert(!phi->definitions[0].isTemp() ||
                        !ctx.spills_entry[block_idx].count(phi->definitions[0].getTemp()));

                        Block::edge_vec& preds =
                        phi->opcode == aco_opcode::p_phi ? block->logical_preds : block->linear_preds;
                        for (unsigned i = 0; i < phi->operands.size(); i++) {
                              if (!phi->operands[i].isTemp())
                                    continue;
                              unsigned pred_idx = preds[i];

                              if (!ctx.spills_exit[pred_idx].count(phi->operands[i].getTemp())) {
                                    std::map<Temp, Temp>::iterator it =
                                    ctx.renames[pred_idx].find(phi->operands[i].getTemp());
                                    if (it != ctx.renames[pred_idx].end()) {
                                          phi->operands[i].setTemp(it->second);
                                    } else {
                                          auto remat_it = ctx.remat.find(phi->operands[i].getTemp());
                                          if (remat_it != ctx.remat.end()) {
                                                ctx.unused_remats.erase(remat_it->second.instr);
                                          }
                                    }
                                    continue;
                              }

                              Temp tmp = phi->operands[i].getTemp();
                              Temp new_name = ctx.program->allocateTmp(tmp.regClass());
                              Block& pred = ctx.program->blocks[pred_idx];
                              std::vector<aco_ptr<Instruction>>::iterator it;

                              if (phi->opcode == aco_opcode::p_phi) {
                                    it = ctx.get_insert_point_before_logical_end(pred);
                              } else {
                                    it = std::prev(pred.instructions.end());
                              }
                              aco_ptr<Instruction> reload =
                              do_reload(ctx, tmp, new_name, ctx.spills_exit[pred_idx][tmp]);

                              if (!phi->definitions[0].isTemp()) {
                                    assert(phi->definitions[0].isFixed() && phi->definitions[0].physReg() == exec);
                                    reload->definitions[0] = phi->definitions[0];
                                    phi->operands[i] = Operand(exec, ctx.program->lane_mask);
                              } else {
                                    ctx.spills_exit[pred_idx].erase(tmp);
                                    ctx.renames[pred_idx][tmp] = new_name;
                                    phi->operands[i].setTemp(new_name);
                              }
                              pred.instructions.insert(it, std::move(reload));
                        }
                  }

                  for (unsigned t : live_in) {
                        const RegClass rc = ctx.program->temp_rc[t];
                        Temp var = Temp(t, rc);

                        if (ctx.spills_entry[block_idx].count(var))
                              continue;

                        Block::edge_vec& preds = rc.is_linear() ? block->linear_preds : block->logical_preds;
                        for (unsigned pred_idx : preds) {
                              if (!ctx.spills_exit[pred_idx].count(var))
                                    continue;

                              Temp new_name = ctx.program->allocateTmp(rc);
                              Block& pred = ctx.program->blocks[pred_idx];
                              std::vector<aco_ptr<Instruction>>::iterator it;
                              if (rc.type() == RegType::vgpr) {
                                    it = ctx.get_insert_point_before_logical_end(pred);
                              } else {
                                    it = std::prev(pred.instructions.end());
                              }

                              aco_ptr<Instruction> reload =
                              do_reload(ctx, var, new_name, ctx.spills_exit[pred.index][var]);
                              pred.instructions.insert(it, std::move(reload));

                              ctx.spills_exit[pred.index].erase(var);
                              ctx.renames[pred.index][var] = new_name;
                        }

                        Temp rename = Temp();
                        bool is_same = true;
                        for (unsigned pred_idx : preds) {
                              if (!ctx.renames[pred_idx].count(var)) {
                                    if (rename == Temp())
                                          rename = var;
                                    else
                                          is_same = rename == var;
                              } else {
                                    if (rename == Temp())
                                          rename = ctx.renames[pred_idx][var];
                                    else
                                          is_same = rename == ctx.renames[pred_idx][var];
                              }

                              if (!is_same)
                                    break;
                        }

                        if (!is_same) {
                              aco_opcode opcode = rc.is_linear() ? aco_opcode::p_linear_phi : aco_opcode::p_phi;
                              aco_ptr<Instruction> phi{create_instruction(opcode, Format::PSEUDO, preds.size(), 1)};
                              rename = ctx.program->allocateTmp(rc);
                              for (unsigned i = 0; i < phi->operands.size(); i++) {
                                    Temp tmp;
                                    if (ctx.renames[preds[i]].count(var)) {
                                          tmp = ctx.renames[preds[i]][var];
                                    } else if (preds[i] >= block_idx) {
                                          tmp = rename;
                                    } else {
                                          tmp = var;
                                          if (ctx.remat.count(tmp))
                                                ctx.unused_remats.erase(ctx.remat[tmp].instr);
                                    }
                                    phi->operands[i] = Operand(tmp);
                              }
                              phi->definitions[0] = Definition(rename);
                              phi->register_demand = block->live_in_demand;
                              block->instructions.insert(block->instructions.begin(), std::move(phi));
                        }

                        if (!(rename == Temp() || rename == var))
                              ctx.renames[block_idx][var] = rename;
                  }
            }

            void
            process_block(spill_ctx& ctx, unsigned block_idx, Block* block, RegisterDemand spilled_registers)
            {
                  assert(!ctx.processed[block_idx]);

                  std::vector<aco_ptr<Instruction>> instructions;
                  instructions.reserve(block->instructions.size() + 16); // Pre-reserve for spills/reloads
                  unsigned idx = 0;

                  while (block->instructions[idx]->opcode == aco_opcode::p_phi ||
                        block->instructions[idx]->opcode == aco_opcode::p_linear_phi) {
                        const Definition def = block->instructions[idx]->definitions[0];
                  if (def.isTemp() && !def.isKill() && def.tempId() < ctx.ssa_infos.size())
                        ctx.program->live.live_in[block_idx].insert(def.tempId());
                        instructions.emplace_back(std::move(block->instructions[idx++]));
                        }

                        auto& current_spills = ctx.spills_exit[block_idx];

                  while (idx < block->instructions.size()) {
                        aco_ptr<Instruction>& instr = block->instructions[idx];

                        if (instr->opcode == aco_opcode::p_branch) {
                              instructions.emplace_back(std::move(instr));
                              idx++;
                              continue;
                        }

                        std::map<Temp, std::pair<Temp, uint32_t>> reloads;

                        for (Operand& op : instr->operands) {
                              if (UNLIKELY(!op.isTemp()))
                                    continue;

                              if (op.isFirstKill())
                                    ctx.program->live.live_in[block_idx].erase(op.tempId());
                              ctx.ssa_infos[op.tempId()].num_uses--;

                              if (!current_spills.count(op.getTemp()))
                                    continue;

                              Temp new_tmp = ctx.program->allocateTmp(op.regClass());
                              ctx.renames[block_idx][op.getTemp()] = new_tmp;
                              reloads[op.getTemp()] = std::make_pair(new_tmp, current_spills[op.getTemp()]);
                              current_spills.erase(op.getTemp());
                              spilled_registers -= new_tmp;
                        }

                        if (block->register_demand.exceeds(ctx.target_pressure)) {
                              RegisterDemand new_demand = instr->register_demand;
                              std::optional<RegisterDemand> live_changes;

                              while ((new_demand - spilled_registers).exceeds(ctx.target_pressure)) {
                                    float score = 0.0;
                                    Temp to_spill = Temp();
                                    bool spill_is_operand = false;
                                    bool spill_is_clobbered = false;
                                    unsigned respill_slot = -1u;
                                    unsigned do_rematerialize = 0;
                                    unsigned avoid_respill = 0;

                                    RegType type = RegType::sgpr;
                                    if (new_demand.vgpr - spilled_registers.vgpr > ctx.target_pressure.vgpr)
                                          type = RegType::vgpr;

                                    for (unsigned t : ctx.program->live.live_in[block_idx]) {
                                          RegClass rc = ctx.program->temp_rc[t];
                                          Temp var = Temp(t, rc);
                                          if (rc.type() != type || current_spills.count(var) || rc.is_linear_vgpr())
                                                continue;

                                          unsigned can_rematerialize = ctx.remat.count(var);
                                          unsigned loop_variable = block->loop_nest_depth && ctx.loop.back().spills.count(var);
                                          if (avoid_respill > loop_variable || do_rematerialize > can_rematerialize)
                                                continue;

                                          if (can_rematerialize > do_rematerialize || loop_variable > avoid_respill ||
                                                ctx.ssa_infos[t].precomputed_score > score) {
                                                bool is_operand = false;
                                          bool is_clobbered = false;
                                          bool can_spill = true;
                                          for (auto& op : instr->operands) {
                                                if (!op.isTemp() || op.getTemp() != var)
                                                      continue;
                                                if (op.isLateKill() || op.isKill() || op.size() > 1) {
                                                      can_spill = false;
                                                      break;
                                                }

                                                if (!live_changes)
                                                      live_changes = get_temp_reg_changes(instr.get());

                                                if (!op.isClobbered() && RegisterDemand(op.getTemp()).exceeds(*live_changes)) {
                                                      can_spill = false;
                                                      break;
                                                }

                                                is_operand = true;
                                                is_clobbered = op.isClobbered();
                                                break;
                                          }
                                          if (!can_spill)
                                                continue;

                                                bool is_spilled_operand = is_operand && reloads.count(var);

                                          to_spill = var;
                                          score = ctx.ssa_infos[t].precomputed_score;
                                          do_rematerialize = can_rematerialize;
                                          avoid_respill = loop_variable || is_spilled_operand;
                                          spill_is_operand = is_operand;
                                          spill_is_clobbered = is_clobbered;

                                          if (loop_variable)
                                                respill_slot = ctx.loop.back().spills[var];
                                                else if (is_spilled_operand)
                                                      respill_slot = reloads[var].second;
                                                }
                                    }
                                    assert(to_spill != Temp());

                                    if (spill_is_operand) {
                                          if (!spill_is_clobbered)
                                                *live_changes -= to_spill;
                                    }

                                    if (avoid_respill) {
                                          current_spills[to_spill] = respill_slot;
                                          spilled_registers += to_spill;
                                          continue;
                                    }

                                    uint32_t spill_id = ctx.add_to_spills(to_spill, current_spills);
                                    for (std::pair<const Temp, std::pair<Temp, uint32_t>>& pair : reloads)
                                          ctx.add_interference(spill_id, pair.second.second);

                                    spilled_registers += to_spill;

                                    if (ctx.renames[block_idx].count(to_spill)) {
                                          to_spill = ctx.renames[block_idx][to_spill];
                                    }

                                    aco_ptr<Instruction> spill{
                                          create_instruction(aco_opcode::p_spill, Format::PSEUDO, 2, 0)};
                                          spill->operands[0] = Operand(to_spill);
                                          spill->operands[1] = Operand::c32(spill_id);
                                          instructions.emplace_back(std::move(spill));
                              }
                        }

                        for (const Definition& def : instr->definitions) {
                              if (def.isTemp() && !def.isKill())
                                    ctx.program->live.live_in[block_idx].insert(def.tempId());
                        }
                        for (Operand& op : instr->operands) {
                              if (op.isTemp()) {
                                    auto rename_it = ctx.renames[block_idx].find(op.getTemp());
                                    if (rename_it != ctx.renames[block_idx].end()) {
                                          op.setTemp(rename_it->second);
                                    } else {
                                          auto remat_it = ctx.remat.find(op.getTemp());
                                          if (remat_it != ctx.remat.end()) {
                                                ctx.unused_remats.erase(remat_it->second.instr);
                                          }
                                    }
                              }
                        }

                        for (std::pair<const Temp, std::pair<Temp, uint32_t>>& pair : reloads) {
                              aco_ptr<Instruction> reload =
                              do_reload(ctx, pair.first, pair.second.first, pair.second.second);
                              instructions.emplace_back(std::move(reload));
                        }
                        instructions.emplace_back(std::move(instr));
                        idx++;
                  }

                  block->instructions = std::move(instructions);
            }

            void
            spill_block(spill_ctx& ctx, unsigned block_idx)
            {
                  Block* block = &ctx.program->blocks[block_idx];

                  RegisterDemand spilled_registers = init_live_in_vars(ctx, block, block_idx);

                  if (!(block->kind & block_kind_loop_header)) {
                        add_coupling_code(ctx, block, ctx.program->live.live_in[block_idx]);
                  }

                  assert(ctx.spills_exit[block_idx].empty());
                  ctx.spills_exit[block_idx] = ctx.spills_entry[block_idx];
                  process_block(ctx, block_idx, block, spilled_registers);

                  ctx.processed[block_idx] = true;

                  if (block->loop_nest_depth == 0 ||
                        ctx.program->blocks[block_idx + 1].loop_nest_depth >= block->loop_nest_depth)
                        return;

                  uint32_t loop_header_idx = ctx.loop.back().index;

                  aco::map<Temp, Temp> renames = std::move(ctx.renames[loop_header_idx]);

                  for (unsigned t : ctx.loop.back().live_in)
                        ctx.ssa_infos[t].num_uses--;
                  add_coupling_code(ctx, &ctx.program->blocks[loop_header_idx], ctx.loop.back().live_in);
                  renames.swap(ctx.renames[loop_header_idx]);

                  ctx.loop.pop_back();
                  if (renames.empty())
                        return;

                  for (std::pair<Temp, Temp> rename : renames) {
                        for (unsigned idx = loop_header_idx; idx <= block_idx; idx++)
                              ctx.renames[idx].insert(rename);
                  }

                  for (unsigned idx = loop_header_idx; idx <= block_idx; idx++) {
                        Block& current = ctx.program->blocks[idx];
                        for (aco_ptr<Instruction>& instr : current.instructions) {
                              if (idx == loop_header_idx && is_phi(instr))
                                    continue;

                              for (Operand& op : instr->operands) {
                                    if (UNLIKELY(!op.isTemp()))
                                          continue;

                                    auto rename_it = renames.find(op.getTemp());
                                    if (rename_it != renames.end())
                                          op.setTemp(rename_it->second);
                              }
                        }
                  }
            }

            Temp
            load_scratch_resource(spill_ctx& ctx, Builder& bld, bool apply_scratch_offset)
            {
                  Temp private_segment_buffer;
                  if (!ctx.program->private_segment_buffers.empty())
                        private_segment_buffer = ctx.program->private_segment_buffers[ctx.resume_idx];

                  if (!private_segment_buffer.bytes()) {
                        Temp addr_lo =
                        bld.sop1(aco_opcode::p_load_symbol, bld.def(s1), Operand::c32(aco_symbol_scratch_addr_lo));
                        Temp addr_hi =
                        bld.sop1(aco_opcode::p_load_symbol, bld.def(s1), Operand::c32(aco_symbol_scratch_addr_hi));
                        private_segment_buffer =
                        bld.pseudo(aco_opcode::p_create_vector, bld.def(s2), addr_lo, addr_hi);
                  } else if (ctx.program->stage.hw != AC_HW_COMPUTE_SHADER) {
                        private_segment_buffer =
                        bld.smem(aco_opcode::s_load_dwordx2, bld.def(s2), private_segment_buffer, Operand::zero());
                  }

                  if (apply_scratch_offset) {
                        Temp addr_lo = bld.tmp(s1);
                        Temp addr_hi = bld.tmp(s1);
                        bld.pseudo(aco_opcode::p_split_vector, Definition(addr_lo), Definition(addr_hi),
                                   private_segment_buffer);

                        Temp carry = bld.tmp(s1);
                        addr_lo = bld.sop2(aco_opcode::s_add_u32, bld.def(s1), bld.scc(Definition(carry)), addr_lo,
                                           ctx.program->scratch_offsets[ctx.resume_idx]);
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
                  ac_state.element_size = ctx.program->gfx_level <= GFX8 ? 1u : 0u;
                  ac_state.index_stride = ctx.wave_size == 64 ? 3u : 2u;
                  ac_state.add_tid = true;
                  ac_state.gfx10_oob_select = V_008F0C_OOB_SELECT_RAW;

                  ac_build_buffer_descriptor(ctx.program->gfx_level, &ac_state, desc);

                  return bld.pseudo(aco_opcode::p_create_vector, bld.def(s4), private_segment_buffer,
                                    Operand::c32(desc[2]), Operand::c32(desc[3]));
            }

            void
            setup_vgpr_spill_reload(spill_ctx& ctx, Block& block,
                                    std::vector<aco_ptr<Instruction>>& instructions, uint32_t spill_slot,
                                    Temp& scratch_offset, unsigned* offset)
            {
                  uint32_t scratch_size = ctx.program->config->scratch_bytes_per_wave / ctx.wave_size;

                  uint32_t offset_range;
                  if (ctx.program->gfx_level >= GFX9) {
                        offset_range =
                        ctx.program->dev.scratch_global_offset_max - ctx.program->dev.scratch_global_offset_min;
                  } else {
                        if (scratch_size < ctx.program->dev.buf_offset_max)
                              offset_range = ctx.program->dev.buf_offset_max - scratch_size;
                        else
                              offset_range = 0;
                  }

                  bool overflow = (ctx.vgpr_spill_slots - 1) * 4 > offset_range;

                  Builder rsrc_bld(ctx.program);
                  if (block.kind & block_kind_top_level) {
                        rsrc_bld.reset(&instructions);
                  } else if (ctx.scratch_rsrc == Temp() && (!overflow || ctx.program->gfx_level < GFX9)) {
                        Block* tl_block = &block;
                        while (!(tl_block->kind & block_kind_top_level))
                              tl_block = &ctx.program->blocks[tl_block->linear_idom];

                        std::vector<aco_ptr<Instruction>>& prev_instructions = tl_block->instructions;
                        std::vector<aco_ptr<Instruction>>::iterator insert_it =
                        ctx.get_insert_point_before_logical_end(*tl_block);
                        rsrc_bld.reset(&prev_instructions, insert_it);
                  }

                  Builder offset_bld = rsrc_bld;
                  if (overflow)
                        offset_bld.reset(&instructions);

                  *offset = spill_slot * 4;
                  if (ctx.program->gfx_level >= GFX9) {
                        *offset += ctx.program->dev.scratch_global_offset_min;

                        if (ctx.scratch_rsrc == Temp() || overflow) {
                              int32_t saddr = scratch_size - ctx.program->dev.scratch_global_offset_min;
                              if ((int32_t)*offset > (int32_t)ctx.program->dev.scratch_global_offset_max) {
                                    saddr += (int32_t)*offset;
                                    *offset = 0;
                              }
                              ctx.scratch_rsrc = offset_bld.copy(offset_bld.def(s1), Operand::c32(saddr));
                        }
                  } else {
                        if (ctx.scratch_rsrc == Temp())
                              ctx.scratch_rsrc = load_scratch_resource(ctx, rsrc_bld, overflow);

                        if (overflow) {
                              uint32_t soffset =
                              ctx.program->config->scratch_bytes_per_wave + *offset * ctx.wave_size;
                              *offset = 0;
                              scratch_offset = offset_bld.copy(offset_bld.def(s1), Operand::c32(soffset));
                        } else {
                              *offset += scratch_size;
                        }
                  }
            }

            void
            spill_vgpr(spill_ctx& ctx, Block& block, std::vector<aco_ptr<Instruction>>& instructions,
                       aco_ptr<Instruction>& spill, std::vector<uint32_t>& slots)
            {
                  ctx.program->config->spilled_vgprs += spill->operands[0].size();

                  uint32_t spill_id = spill->operands[1].constantValue();
                  uint32_t spill_slot = slots[spill_id];

                  Temp scratch_offset;
                  if (!ctx.program->scratch_offsets.empty())
                        scratch_offset = ctx.program->scratch_offsets[ctx.resume_idx];
                  unsigned offset;
                  setup_vgpr_spill_reload(ctx, block, instructions, spill_slot, scratch_offset, &offset);

                  assert(spill->operands[0].isTemp());
                  Temp temp = spill->operands[0].getTemp();
                  assert(temp.type() == RegType::vgpr && !temp.is_linear());

                  Builder bld(ctx.program, &instructions);
                  if (temp.size() > 1) {
                        Instruction* split{
                              create_instruction(aco_opcode::p_split_vector, Format::PSEUDO, 1, temp.size())};
                              split->operands[0] = Operand(temp);
                              for (unsigned i = 0; i < temp.size(); i++)
                                    split->definitions[i] = bld.def(v1);
                        bld.insert(split);
                        for (unsigned i = 0; i < temp.size(); i++, offset += 4) {
                              Temp elem = split->definitions[i].getTemp();
                              if (ctx.program->gfx_level >= GFX9) {
                                    bld.scratch(aco_opcode::scratch_store_dword, Operand(v1), ctx.scratch_rsrc, elem,
                                                offset, memory_sync_info(storage_vgpr_spill, semantic_private));
                              } else {
                                    Instruction* instr = bld.mubuf(aco_opcode::buffer_store_dword, ctx.scratch_rsrc,
                                                                   Operand(v1), scratch_offset, elem, offset, false);
                                    instr->mubuf().sync = memory_sync_info(storage_vgpr_spill, semantic_private);
                                    instr->mubuf().cache.value = ac_swizzled;
                              }
                        }
                  } else if (ctx.program->gfx_level >= GFX9) {
                        bld.scratch(aco_opcode::scratch_store_dword, Operand(v1), ctx.scratch_rsrc, temp, offset,
                                    memory_sync_info(storage_vgpr_spill, semantic_private));
                  } else {
                        Instruction* instr = bld.mubuf(aco_opcode::buffer_store_dword, ctx.scratch_rsrc, Operand(v1),
                                                       scratch_offset, temp, offset, false);
                        instr->mubuf().sync = memory_sync_info(storage_vgpr_spill, semantic_private);
                        instr->mubuf().cache.value = ac_swizzled;
                  }
            }

            void
            reload_vgpr(spill_ctx& ctx, Block& block, std::vector<aco_ptr<Instruction>>& instructions,
                        aco_ptr<Instruction>& reload, std::vector<uint32_t>& slots)
            {
                  uint32_t spill_id = reload->operands[0].constantValue();
                  uint32_t spill_slot = slots[spill_id];

                  Temp scratch_offset;
                  if (!ctx.program->scratch_offsets.empty())
                        scratch_offset = ctx.program->scratch_offsets[ctx.resume_idx];
                  unsigned offset;
                  setup_vgpr_spill_reload(ctx, block, instructions, spill_slot, scratch_offset, &offset);

                  Definition def = reload->definitions[0];

                  Builder bld(ctx.program, &instructions);
                  if (def.size() > 1) {
                        Instruction* vec{
                              create_instruction(aco_opcode::p_create_vector, Format::PSEUDO, def.size(), 1)};
                              vec->definitions[0] = def;
                              for (unsigned i = 0; i < def.size(); i++, offset += 4) {
                                    Temp tmp = bld.tmp(v1);
                                    vec->operands[i] = Operand(tmp);
                                    if (ctx.program->gfx_level >= GFX9) {
                                          bld.scratch(aco_opcode::scratch_load_dword, Definition(tmp), Operand(v1),
                                                      ctx.scratch_rsrc, offset,
                                                      memory_sync_info(storage_vgpr_spill, semantic_private));
                                    } else {
                                          Instruction* instr =
                                          bld.mubuf(aco_opcode::buffer_load_dword, Definition(tmp), ctx.scratch_rsrc,
                                                    Operand(v1), scratch_offset, offset, false);
                                          instr->mubuf().sync = memory_sync_info(storage_vgpr_spill, semantic_private);
                                          instr->mubuf().cache.value = ac_swizzled;
                                    }
                              }
                              bld.insert(vec);
                  } else if (ctx.program->gfx_level >= GFX9) {
                        bld.scratch(aco_opcode::scratch_load_dword, def, Operand(v1), ctx.scratch_rsrc, offset,
                                    memory_sync_info(storage_vgpr_spill, semantic_private));
                  } else {
                        Instruction* instr = bld.mubuf(aco_opcode::buffer_load_dword, def, ctx.scratch_rsrc,
                                                       Operand(v1), scratch_offset, offset, false);
                        instr->mubuf().sync = memory_sync_info(storage_vgpr_spill, semantic_private);
                        instr->mubuf().cache.value = ac_swizzled;
                  }
            }

            void
            add_interferences_for_slot_assignment(spill_ctx& ctx,
                                                  const std::vector<bool>& is_assigned,
                                                  const std::vector<uint32_t>& slots,
                                                  tiny_bitmap& slots_used,
                                                  unsigned id)
            {
                  for (unsigned other : ctx.interferences[id].second) {
                        if (!is_assigned[other])
                              continue;

                        unsigned base  = slots[other];                      /* first slot */
                        unsigned count = ctx.interferences[other].first.size();   /* #words */

                        for (unsigned i = 0; i < count; ++i)
                              slots_used.set(base + i);        /* mark every lane as used   */
                  }
            }

            static ALWAYS_INLINE unsigned
            find_available_slot(const tiny_bitmap& used,
                                unsigned           wave_size,
                                unsigned           size,
                                bool               is_sgpr)
            {
                  const unsigned wave_mask = wave_size - 1u;
                  unsigned slot = 0;

                  while (true) {
                        /* ───── 1. Guard: SGPR range must not cross the segment ───── */
                        if (is_sgpr && ((slot & wave_mask) + size > wave_size)) {
                              slot = (slot + wave_size) & ~wave_mask;
                              continue;
                        }

                        /* ───── 2. Word-level skip over fully used 64-bit words ───── */
                        unsigned w_idx = slot >> 6;
                        uint64_t word  = used.word(w_idx);

                        /* mask out bits below current slot within this word */
                        word |= (1ull << (slot & 63u)) - 1ull;

                        if (word == ~0ull) {               /* word completely full */
                              slot = (w_idx + 1u) << 6;
                              continue;
                        }

                        /* ───── 3. First zero bit in the current word ───── */
                        slot = (w_idx << 6) + unsigned(__builtin_ctzll(~word));

                        /* ───── 4a.  Fast path: size == 1 ───────────────── */
                        if (size == 1) {
                              if (!used.test(slot))
                                    return slot;
                              ++slot;
                              continue;
                        }

                        /* ───── 4b.  Validate full range for size 2-8 ───── */
                        bool ok = true;
                        for (unsigned off = 0; off < size; ++off) {
                              unsigned bit = slot + off;

                              /* SGPR wrap-around check (only matters when off > 0) */
                              if (is_sgpr && off && !(bit & wave_mask)) { ok = false; break; }

                              if (used.test(bit)) { ok = false; break; }
                        }

                        if (ok)
                              return slot;                     /* found a free range */

                              ++slot;                             /* else continue scan */
                  }
            }

            static void
            assign_spill_slots_helper(spill_ctx&             ctx,
                                      RegType                type,
                                      std::vector<bool>&     is_assigned,
                                      std::vector<uint32_t>& slots,
                                      unsigned*              num_slots)
            {
                  /* Persistent bitmap marks every slot word that is already taken. */
                  tiny_bitmap used;
                  unsigned    max_slot = 0;         /* one past the highest word set */

                  auto mark_range = [&](unsigned base, unsigned cnt)
                  {
                        for (unsigned i = 0; i < cnt; ++i)
                              used.set(base + i);
                        max_slot = std::max(max_slot, base + cnt);
                  };

                  /* Helper: mark all *already-placed* interferers of spill-id `id`. */
                  auto mark_interferers = [&](unsigned id)
                  {
                        for (unsigned other : ctx.interferences[id].second) {
                              if (!is_assigned[other])
                                    continue;
                              unsigned base = slots[other];
                              unsigned cnt  = ctx.interferences[other].first.size();
                              mark_range(base, cnt);
                        }
                  };

                  const bool sgpr_mode = type == RegType::sgpr;

                  /* ===============================================================
                   * (1)  Place affinity groups first
                   * ============================================================= */
                  for (std::vector<uint32_t>& grp : ctx.affinities) {
                        if (grp.empty() || ctx.interferences[grp[0]].first.type() != type)
                              continue;

                        /* mark ranges of ids that might already be assigned (unlikely) */
                        for (unsigned id : grp)
                              if (ctx.is_reloaded[id])
                                    mark_interferers(id);

                        const unsigned range_size = ctx.interferences[grp[0]].first.size();

                        unsigned slot_idx =
                        find_available_slot(used, ctx.wave_size, range_size, sgpr_mode);

                        for (unsigned id : grp) {
                              if (!ctx.is_reloaded[id])
                                    continue;
                              slots[id]       = slot_idx;
                              is_assigned[id] = true;
                        }
                        mark_range(slot_idx, range_size);
                  }

                  /* ===============================================================
                   * (2)  Assign all remaining individual ids
                   * ============================================================= */
                  for (unsigned id = 0; id < ctx.interferences.size(); ++id) {
                        if (is_assigned[id]                         ||
                              !ctx.is_reloaded[id]                    ||
                              ctx.interferences[id].first.type() != type)
                              continue;

                        mark_interferers(id);

                        unsigned range_size = ctx.interferences[id].first.size();
                        unsigned slot_idx   =
                        find_available_slot(used, ctx.wave_size, range_size, sgpr_mode);

                        slots[id]       = slot_idx;
                        is_assigned[id] = true;
                        mark_range(slot_idx, range_size);
                  }

                  *num_slots = max_slot;          /* caller will multiply by word-size */
            }

            void
            end_unused_spill_vgprs(spill_ctx& ctx, Block& block, std::vector<Temp>& vgpr_spill_temps,
                                   const std::vector<uint32_t>& slots,
                                   const aco::unordered_map<Temp, uint32_t>& spills)
            {
                  std::vector<bool> is_used(vgpr_spill_temps.size());
                  for (std::pair<Temp, uint32_t> pair : spills) {
                        if (pair.first.type() == RegType::sgpr && ctx.is_reloaded[pair.second])
                              is_used[slots[pair.second] / ctx.wave_size] = true;
                  }

                  std::vector<Temp> temps;
                  temps.reserve(vgpr_spill_temps.size());
                  for (unsigned i = 0; i < vgpr_spill_temps.size(); i++) {
                        if (vgpr_spill_temps[i].id() && !is_used[i]) {
                              temps.push_back(vgpr_spill_temps[i]);
                              vgpr_spill_temps[i] = Temp();
                        }
                  }
                  if (temps.empty() || block.linear_preds.empty())
                        return;

                  aco_ptr<Instruction> destr{
                        create_instruction(aco_opcode::p_end_linear_vgpr, Format::PSEUDO, temps.size(), 0)};
                        for (unsigned i = 0; i < temps.size(); i++)
                              destr->operands[i] = Operand(temps[i]);

                  std::vector<aco_ptr<Instruction>>::iterator it = block.instructions.begin();
                  while (LIKELY(is_phi(*it)))
                        ++it;
                  block.instructions.insert(it, std::move(destr));
            }

            void
            assign_spill_slots(spill_ctx& ctx, unsigned spills_to_vgpr)
            {
                  if (!ctx.interferences_finalized_for_assignment) {
                        for (auto& pair : ctx.interferences) {
                              std::vector<uint32_t>& vec = pair.second;
                              if (!vec.empty()) {
                                    std::sort(vec.begin(), vec.end());
                                    vec.erase(std::unique(vec.begin(), vec.end()), vec.end());
                              }
                        }
                        ctx.interferences_finalized_for_assignment = true;
                  }

                  std::vector<uint32_t> slots(ctx.interferences.size());
                  std::vector<bool> is_assigned(ctx.interferences.size());

                  for (std::vector<uint32_t>& vec : ctx.affinities) {
                        if (vec.empty()) continue;
                        for (unsigned i = 0; i < vec.size(); i++) {
                              for (unsigned j = i + 1; j < vec.size(); j++) {
                                    assert(vec[i] != vec[j]);
                                    bool reloaded = ctx.is_reloaded[vec[i]] || ctx.is_reloaded[vec[j]];
                                    ctx.is_reloaded[vec[i]] = reloaded;
                                    ctx.is_reloaded[vec[j]] = reloaded;
                              }
                        }
                  }
                  for (ASSERTED uint32_t i = 0; i < ctx.interferences.size(); i++)
                        for (ASSERTED uint32_t id : ctx.interferences[i].second)
                              assert(i != id);

                  assign_spill_slots_helper(ctx, RegType::sgpr, is_assigned, slots, &ctx.sgpr_spill_slots);
                  assign_spill_slots_helper(ctx, RegType::vgpr, is_assigned, slots, &ctx.vgpr_spill_slots);

                  for (unsigned id = 0; id < is_assigned.size(); id++)
                        assert(is_assigned[id] || !ctx.is_reloaded[id]);

                  for (std::vector<uint32_t>& vec : ctx.affinities) {
                        if (vec.empty()) continue;
                        for (unsigned i = 0; i < vec.size(); i++) {
                              for (unsigned j = i + 1; j < vec.size(); j++) {
                                    assert(is_assigned[vec[i]] == is_assigned[vec[j]]);
                                    if (!is_assigned[vec[i]])
                                          continue;
                                    assert(ctx.is_reloaded[vec[i]] == ctx.is_reloaded[vec[j]]);
                                    assert(ctx.interferences[vec[i]].first.type() ==
                                    ctx.interferences[vec[j]].first.type());
                                    assert(slots[vec[i]] == slots[vec[j]]);
                              }
                        }
                  }

                  std::vector<Temp> vgpr_spill_temps((ctx.sgpr_spill_slots + ctx.wave_size - 1) / ctx.wave_size);
                  assert(vgpr_spill_temps.size() <= spills_to_vgpr);

                  unsigned last_top_level_block_idx = 0;
                  for (Block& block : ctx.program->blocks) {

                        if (block.kind & block_kind_top_level) {
                              last_top_level_block_idx = block.index;
                              end_unused_spill_vgprs(ctx, block, vgpr_spill_temps, slots, ctx.spills_entry[block.index]);
                              if (block.linear_preds.empty())
                                    ctx.scratch_rsrc = Temp();
                              if (block.kind & block_kind_resume)
                                    ++ctx.resume_idx;
                        }

                        std::vector<aco_ptr<Instruction>> instructions;
                        instructions.reserve(block.instructions.size() + 16);
                        Builder bld(ctx.program, &instructions);
                        for (auto it = block.instructions.begin(); it != block.instructions.end(); ++it) {

                              if ((*it)->opcode == aco_opcode::p_spill) {
                                    uint32_t spill_id = (*it)->operands[1].constantValue();
                                    if (!ctx.is_reloaded[spill_id]) {
                                    } else if (!is_assigned[spill_id]) {
                                          unreachable("No spill slot assigned for spill id");
                                    } else if (ctx.interferences[spill_id].first.type() == RegType::vgpr) {
                                          spill_vgpr(ctx, block, instructions, *it, slots);
                                    } else {
                                          ctx.program->config->spilled_sgprs += (*it)->operands[0].size();
                                          uint32_t spill_slot = slots[spill_id];
                                          if (vgpr_spill_temps[spill_slot / ctx.wave_size] == Temp()) {
                                                Temp linear_vgpr = ctx.program->allocateTmp(v1.as_linear());
                                                vgpr_spill_temps[spill_slot / ctx.wave_size] = linear_vgpr;
                                                aco_ptr<Instruction> create{
                                                      create_instruction(aco_opcode::p_start_linear_vgpr, Format::PSEUDO, 0, 1)};
                                                      create->definitions[0] = Definition(linear_vgpr);
                                                      if (last_top_level_block_idx == block.index) {
                                                            instructions.emplace_back(std::move(create));
                                                      } else {
                                                            assert(last_top_level_block_idx < block.index);
                                                            std::vector<aco_ptr<Instruction>>& tl_instrs =
                                                            ctx.program->blocks[last_top_level_block_idx].instructions;
                                                            auto insert_point = ctx.get_insert_point_before_logical_end(
                                                                  ctx.program->blocks[last_top_level_block_idx]);
                                                            tl_instrs.insert(insert_point, std::move(create));
                                                      }
                                          }
                                          Instruction* spill = create_instruction(aco_opcode::p_spill, Format::PSEUDO, 3, 0);
                                          spill->operands[0] = Operand(vgpr_spill_temps[spill_slot / ctx.wave_size]);
                                          spill->operands[1] = Operand::c32(spill_slot % ctx.wave_size);
                                          spill->operands[2] = (*it)->operands[0];
                                          instructions.emplace_back(aco_ptr<Instruction>(spill));
                                    }
                              } else if ((*it)->opcode == aco_opcode::p_reload) {
                                    uint32_t spill_id = (*it)->operands[0].constantValue();
                                    assert(ctx.is_reloaded[spill_id]);
                                    if (!is_assigned[spill_id]) {
                                          unreachable("No spill slot assigned for spill id");
                                    } else if (ctx.interferences[spill_id].first.type() == RegType::vgpr) {
                                          reload_vgpr(ctx, block, instructions, *it, slots);
                                    } else {
                                          uint32_t spill_slot = slots[spill_id];
                                          if (vgpr_spill_temps[spill_slot / ctx.wave_size] == Temp()) {
                                                Temp linear_vgpr = ctx.program->allocateTmp(v1.as_linear());
                                                vgpr_spill_temps[spill_slot / ctx.wave_size] = linear_vgpr;
                                                aco_ptr<Instruction> create{
                                                      create_instruction(aco_opcode::p_start_linear_vgpr, Format::PSEUDO, 0, 1)};
                                                      create->definitions[0] = Definition(linear_vgpr);
                                                      if (last_top_level_block_idx == block.index) {
                                                            instructions.emplace_back(std::move(create));
                                                      } else {
                                                            assert(last_top_level_block_idx < block.index);
                                                            std::vector<aco_ptr<Instruction>>& tl_instrs =
                                                            ctx.program->blocks[last_top_level_block_idx].instructions;
                                                            auto insert_point = ctx.get_insert_point_before_logical_end(
                                                                  ctx.program->blocks[last_top_level_block_idx]);
                                                            tl_instrs.insert(insert_point, std::move(create));
                                                      }
                                          }
                                          Instruction* reload = create_instruction(aco_opcode::p_reload, Format::PSEUDO, 2, 1);
                                          reload->operands[0] = Operand(vgpr_spill_temps[spill_slot / ctx.wave_size]);
                                          reload->operands[1] = Operand::c32(spill_slot % ctx.wave_size);
                                          reload->definitions[0] = (*it)->definitions[0];
                                          instructions.emplace_back(aco_ptr<Instruction>(reload));
                                    }
                              } else if (!ctx.unused_remats.count(it->get())) {
                                    instructions.emplace_back(std::move(*it));
                              }
                        }
                        block.instructions = std::move(instructions);
                  }

                  ctx.program->config->scratch_bytes_per_wave += ctx.vgpr_spill_slots * 4 * ctx.wave_size;
            }

      }

      void
      spill(Program* program)
      {
            program->config->spilled_vgprs = 0;
            program->config->spilled_sgprs = 0;

            program->progress = CompilationProgress::after_spilling;

            if (program->num_waves > 0)
                  return;

            lower_to_cssa(program);

            const RegisterDemand demand = program->max_reg_demand;
            const RegisterDemand limit = get_addr_regs_from_waves(program, program->min_waves);
            uint16_t extra_vgprs = 0;
            uint16_t extra_sgprs = 0;

            if (demand.sgpr > limit.sgpr) {
                  unsigned sgpr_spills = demand.sgpr - limit.sgpr;
                  extra_vgprs = DIV_ROUND_UP(sgpr_spills * 2, program->wave_size) + 1;
            }
            if (demand.vgpr + extra_vgprs > limit.vgpr) {
                  if (program->gfx_level >= GFX9)
                        extra_sgprs = 1;
                  else
                        extra_sgprs = 5;
                  if (demand.sgpr + extra_sgprs > limit.sgpr) {
                        unsigned sgpr_spills = demand.sgpr + extra_sgprs - limit.sgpr;
                        extra_vgprs = DIV_ROUND_UP(sgpr_spills * 2, program->wave_size) + 1;
                  }
            }
            const RegisterDemand target(limit.vgpr - extra_vgprs, limit.sgpr - extra_sgprs);

            spill_ctx ctx(target, program);
            gather_ssa_use_info(ctx);
            get_rematerialize_info(ctx);

            for (unsigned i = 0; i < program->blocks.size(); i++)
                  spill_block(ctx, i);

            assign_spill_slots(ctx, extra_vgprs);

            live_var_analysis(program);

            assert(program->num_waves > 0);
      }

} // namespace aco
