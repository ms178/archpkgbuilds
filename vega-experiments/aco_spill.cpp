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
      std::vector<uint64_t> words;

      ALWAYS_INLINE void ensure(unsigned bit)
      {
            const unsigned word_idx = bit >> 6;
            if (LIKELY(word_idx < words.size()))
                  return;

            const size_t new_size = std::max<size_t>(word_idx + 1,
                                                     words.empty() ? 4 : words.size() * 2);
            words.resize(new_size, 0ull);
      }

      ALWAYS_INLINE void set(unsigned bit)
      {
            ensure(bit);
            words[bit >> 6] |= 1ull << (bit & 63u);
      }

      ALWAYS_INLINE bool test(unsigned bit) const
      {
            const unsigned idx = bit >> 6;
            return idx < words.size() &&
            (words[idx] & (1ull << (bit & 63u)));
      }

      ALWAYS_INLINE uint64_t word(unsigned idx) const
      {
            return idx < words.size() ? words[idx] : 0ull;
      }

      ALWAYS_INLINE void clear() { words.clear(); }

      ALWAYS_INLINE unsigned max_marked_plus_one() const
      {
            if (words.empty()) return 0;
            for (int i = (int)words.size() - 1; i >= 0; --i) {
                  const uint64_t w = words[i];
                  if (w) {
                        return ((unsigned)i << 6) + 64u - (unsigned)__builtin_clzll(w);
                  }
            }
            return 0;
      }

      ALWAYS_INLINE void set_range(unsigned start, unsigned count)
      {
            if (!count)
                  return;

            const unsigned end = start + count;
            ensure(end - 1);

            const unsigned start_word = start >> 6;
            const unsigned end_word   = (end - 1) >> 6;

            const unsigned sb = start & 63u;
            const unsigned eb = (end - 1) & 63u;

            if (start_word == end_word) {
                  const uint64_t mask =
                  ((count >= 64) ? ~0ull
                  : ((1ull << count) - 1ull)) << sb;
                  words[start_word] |= mask;
                  return;
            }

            words[start_word] |= ~0ull << sb;
            if (start_word + 1 < end_word) {
                  std::fill(words.begin() + start_word + 1,
                            words.begin() + end_word, ~0ull);
            }
            words[end_word]   |= (eb == 63u) ? ~0ull
            : ((1ull << (eb + 1)) - 1ull);
      }
};
#endif

namespace std {
      template <>
      struct hash<aco::Temp> {
            ALWAYS_INLINE size_t operator()(aco::Temp t) const noexcept
            {
                  return std::hash<uint32_t>{}(t.id());
            }
      };
}

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
                  float score() const
                  {
                        if (UNLIKELY(num_uses == 0))
                              return 0.0f;
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
                  uint32_t wave_size;

                  unsigned sgpr_spill_slots;
                  unsigned vgpr_spill_slots;
                  Temp scratch_rsrc;

                  unsigned resume_idx;

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
                  wave_size(program->wave_size), sgpr_spill_slots(0), vgpr_spill_slots(0), resume_idx(0)
                  {
                        for (size_t i = 0; i < program->blocks.size(); ++i) {
                              spills_entry[i].reserve(8);
                              spills_exit[i].reserve(8);
                        }
                        affinities.reserve(16);
                  }

                  static std::vector<aco_ptr<Instruction>>::iterator
                  get_insert_point_before_logical_end(Block& blk)
                  {
                        for (auto it = blk.instructions.begin(); it != blk.instructions.end(); ++it) {
                              if (UNLIKELY((*it)->opcode == aco_opcode::p_logical_end)) {
                                    return it;
                              }
                        }
                        return blk.instructions.end();
                  }

                  void
                  add_affinity(uint32_t a, uint32_t b)
                  {
                        if (UNLIKELY(a == b)) return;

                        auto ensure = [&](uint32_t id) -> uint32_t& {
                              if (UNLIKELY(id >= id2grp.size()))
                                    id2grp.resize(std::max(id + 1, (uint32_t)id2grp.size() + 16u), UINT32_MAX);
                              return id2grp[id];
                        };

                        uint32_t& ga = ensure(a);
                        uint32_t& gb = ensure(b);

                        if (ga == UINT32_MAX && gb == UINT32_MAX) {
                              affinities.emplace_back();
                              affinities.back().reserve(8);
                              affinities.back().push_back(a);
                              affinities.back().push_back(b);
                              ga = gb = affinities.size() - 1;
                              return;
                        }
                        if (ga == UINT32_MAX) { affinities[gb].push_back(a); ga = gb; return; }
                        if (gb == UINT32_MAX) { affinities[ga].push_back(b); gb = ga; return; }

                        if (UNLIKELY(ga == gb)) return;

                        uint32_t dst_grp_idx = affinities[ga].size() >= affinities[gb].size() ? ga : gb;
                        uint32_t src_grp_idx = dst_grp_idx == ga ? gb : ga;

                        auto& D_vec = affinities[dst_grp_idx];
                        auto& S_vec = affinities[src_grp_idx];

                        if (!S_vec.empty()) {
                              D_vec.reserve(D_vec.size() + S_vec.size());
                              for (uint32_t id_in_S : S_vec) {
                                    D_vec.push_back(id_in_S);
                                    ensure(id_in_S) = dst_grp_idx;
                              }
                              S_vec.clear();
                              S_vec.shrink_to_fit();
                        }
                  }

                  void add_interference(uint32_t first, uint32_t second)
                  {
                        if (UNLIKELY(first >= interferences.size() || second >= interferences.size()))
                              return;
                        if (UNLIKELY(interferences[first].first.type() != interferences[second].first.type()))
                              return;

                        interferences[first].second.push_back(second);
                        interferences[second].second.push_back(first);
                  }

                  uint32_t allocate_spill_id(RegClass rc)
                  {
                        interferences.emplace_back(rc, std::vector<uint32_t>());
                        interferences.back().second.reserve(16);
                        is_reloaded.push_back(false);
                        if (UNLIKELY(next_spill_id >= id2grp.size()))
                              id2grp.resize(std::max(next_spill_id + 1, (uint32_t)id2grp.size() + 16u), UINT32_MAX);
                        id2grp[next_spill_id] = UINT32_MAX;
                        return next_spill_id++;
                  }

                  uint32_t add_to_spills(Temp to_spill,
                                         aco::unordered_map<Temp, uint32_t>& spills)
                  {
                        const uint32_t spill_id = allocate_spill_id(to_spill.regClass());

                        for (auto const& pair_val : spills)
                              add_interference(spill_id, pair_val.second);

                        if (LIKELY(!loop.empty())) {
                              for (auto const& pair_val : loop.back().spills)
                                    add_interference(spill_id, pair_val.second);
                        }

                        spills[to_spill] = spill_id;
                        return spill_id;
                  }
                  uint32_t next_spill_id = 0;
            };

            static void
            gather_ssa_use_info(spill_ctx& ctx)
            {
                  const unsigned ninfos = ctx.ssa_infos.size();
                  unsigned       global_idx = 0;

                  for (const Block& block : ctx.program->blocks) {
                        const unsigned bsz = block.instructions.size();

                        for (unsigned i = 0; i < bsz; ++i) {
                              const Instruction* instr = block.instructions[i].get();
                              const unsigned     gi    = global_idx + i;

                              for (const Operand& op : instr->operands) {
                                    if (!op.isTemp())
                                          continue;

                                    const unsigned id = op.tempId();
                                    if (id >= ninfos)
                                          continue;

                                    use_info& ui = ctx.ssa_infos[id];
                                    ui.num_uses++;
                                    ui.last_use = gi;
                              }
                        }

                        if ((block.kind & block_kind_loop_header) &&
                              block.index < ctx.program->live.live_in.size()) {
                              for (unsigned id : ctx.program->live.live_in[block.index])
                                    if (id < ninfos)
                                          ctx.ssa_infos[id].num_uses++;
                              }

                              global_idx += bsz;
                  }

                  for (use_info& ui : ctx.ssa_infos)
                        ui.precomputed_score =
                        ui.num_uses ? float(ui.last_use) / float(ui.num_uses) : 0.0f;
            }

            static ALWAYS_INLINE bool
            should_rematerialize(const Instruction* instr)
            {
                  if (instr->definitions.size() != 1)
                        return false;

                  switch (instr->format) {
                        case Format::VOP1:
                        case Format::SOP1:
                              break;
                        case Format::PSEUDO:
                              if (instr->opcode != aco_opcode::p_create_vector &&
                                    instr->opcode != aco_opcode::p_parallelcopy)
                                    return false;
                              break;
                        case Format::SOPK:
                              if (instr->opcode != aco_opcode::s_movk_i32)
                                    return false;
                        break;
                        default:
                              return false;
                  }

                  for (const Operand& op : instr->operands)
                        if (!op.isConstant())
                              return false;

                  return true;
            }


            static inline aco_ptr<Instruction>
            do_reload(spill_ctx& ctx, Temp original, Temp new_name, uint32_t spill_id)
            {
                  auto it = ctx.remat.find(original);
                  if (it != ctx.remat.end()) {
                        Instruction* src = it->second.instr;

                        aco_ptr<Instruction> res(
                              create_instruction(src->opcode, src->format,
                                                 src->operands.size(), 1));

                        if (src->isSOPK())
                              res->salu().imm = src->salu().imm;

                        if (!src->operands.empty()) {
                              std::memcpy(res->operands.begin(),
                                          src->operands.begin(),
                                          src->operands.size() * sizeof(Operand));
                        }

                        res->definitions[0] = Definition(new_name);
                        return res;
                  }

                  if (spill_id >= ctx.is_reloaded.size())
                        ctx.is_reloaded.resize(spill_id + 1, false);

                  ctx.is_reloaded[spill_id] = true;

                  aco_ptr<Instruction> reload(
                        create_instruction(aco_opcode::p_reload, Format::PSEUDO, 1, 1));
                  reload->operands[0]  = Operand::c32(spill_id);
                  reload->definitions[0] = Definition(new_name);
                  return reload;
            }

            static void
            get_rematerialize_info(spill_ctx& ctx)
            {
                  for (Block& b : ctx.program->blocks) {
                        bool logical = false;
                        for (aco_ptr<Instruction>& ins : b.instructions) {
                              if (ins->opcode == aco_opcode::p_logical_start) logical = true;
                              else if (ins->opcode == aco_opcode::p_logical_end) logical = false;


                              if (!logical || !should_rematerialize(ins.get()))
                                    continue;

                              for (const Definition& d : ins->definitions)
                                    if (d.isTemp()) {
                                          ctx.remat[d.getTemp()] = {ins.get()};
                                          ctx.unused_remats.insert(ins.get());
                                    }
                        }
                  }
            }

            static Temp
            load_scratch_resource(spill_ctx& ctx, Builder& bld, bool apply_off)
            {
                  if (!apply_off && ctx.scratch_rsrc != Temp())
                        return ctx.scratch_rsrc;

                  Temp psb;
                  if (LIKELY(ctx.resume_idx < ctx.program->private_segment_buffers.size()))
                        psb = ctx.program->private_segment_buffers[ctx.resume_idx];

                  if (!psb.bytes()) {
                        Temp lo = bld.sop1(aco_opcode::p_load_symbol, bld.def(s1),
                                           Operand::c32(aco_symbol_scratch_addr_lo));
                        Temp hi = bld.sop1(aco_opcode::p_load_symbol, bld.def(s1),
                                           Operand::c32(aco_symbol_scratch_addr_hi));
                        psb     = bld.pseudo(aco_opcode::p_create_vector, bld.def(s2), lo, hi);
                  } else if (ctx.program->stage.hw != AC_HW_COMPUTE_SHADER) {
                        psb = bld.smem(aco_opcode::s_load_dwordx2, bld.def(s2), psb, Operand::zero());
                  }

                  Temp final_s2 = psb;
                  if (apply_off) {
                        if (UNLIKELY(ctx.program->scratch_offsets.empty() || ctx.resume_idx >= ctx.program->scratch_offsets.size())) {
                              assert(false && "resume_idx out of bounds for scratch_offsets or scratch_offsets is empty");
                              return Temp();
                        }
                        const Temp off = ctx.program->scratch_offsets[ctx.resume_idx];
                        Temp lo = bld.tmp(s1), hi = bld.tmp(s1);

                        aco_ptr<Instruction> split{create_instruction(aco_opcode::p_split_vector,
                              Format::PSEUDO, 1, 2)};
                              split->operands[0]  = Operand(psb);
                              split->definitions[0] = Definition(lo);
                              split->definitions[1] = Definition(hi);
                              bld.insert(std::move(split));

                              Temp carry = bld.tmp(s1);
                              lo = bld.sop2(aco_opcode::s_add_u32, bld.def(s1),
                                            bld.scc(Definition(carry)), lo, off);
                              hi = bld.sop2(aco_opcode::s_addc_u32, bld.def(s1), bld.def(s1, scc),
                                            hi, Operand::c32(0), bld.scc(carry));
                              final_s2 = bld.pseudo(aco_opcode::p_create_vector, bld.def(s2), lo, hi);
                  }

                  ac_buffer_state st = {};
                  st.size         = 0xffffffff;
                  st.format       = PIPE_FORMAT_R32_FLOAT;
                  st.element_size = ctx.program->gfx_level <= GFX8 ? 1 : 0;
                  st.index_stride = ctx.wave_size == 64 ? 3 : 2;
                  st.add_tid      = true;
                  st.gfx10_oob_select = V_008F0C_OOB_SELECT_RAW;
                  for (unsigned i = 0; i < 4; ++i)
                        st.swizzle[i] = PIPE_SWIZZLE_0;

                  uint32_t desc[4];
                  ac_build_buffer_descriptor(ctx.program->gfx_level, &st, desc);

                  Temp res =
                  bld.pseudo(aco_opcode::p_create_vector, bld.def(s4), final_s2,
                             Operand::c32(desc[2]), Operand::c32(desc[3]));

                  if (!apply_off)
                        ctx.scratch_rsrc = res;
                  return res;
            }

            static RegisterDemand
            init_live_in_vars(spill_ctx& ctx, Block* block, unsigned block_idx)
            {
                  RegisterDemand spilled_registers;

                  if (UNLIKELY(block_idx >= ctx.program->live.live_in.size() || block->linear_preds.empty()))
                        return {0, 0};

                  const IDSet& live_in = ctx.program->live.live_in[block_idx];

                  if (block->kind & block_kind_loop_header) {
                        if (UNLIKELY(block_idx == 0 || block->linear_preds.empty() || block->logical_preds.empty() ||
                              block->linear_preds[0] != block_idx - 1 || block->logical_preds[0] != block_idx - 1 ||
                              (block_idx - 1) >= ctx.spills_exit.size())) {
                              assert(false && "Loop header assumptions violated or out of bounds access");
                        return {0, 0};
                              }

                              RegisterDemand reg_pressure = block->live_in_demand;
                              RegisterDemand loop_demand = reg_pressure;
                              unsigned i = block_idx;
                              while (i < ctx.program->blocks.size() && ctx.program->blocks[i].loop_nest_depth >= block->loop_nest_depth) {
                                    loop_demand.update(ctx.program->blocks[i].register_demand);
                                    i++;
                              }

                              for (auto spilled : ctx.spills_exit[block_idx - 1]) {
                                    if (UNLIKELY(!live_in.count(spilled.first.id())))
                                          continue;
                                    if (LIKELY(block_idx < ctx.spills_entry.size())) {
                                          ctx.spills_entry[block_idx][spilled.first] = spilled.second;
                                          spilled_registers += spilled.first;
                                          loop_demand -= spilled.first;
                                    }
                              }
                              if (LIKELY(!ctx.loop.empty() && block_idx < ctx.spills_entry.size())) {
                                    for (auto spilled : ctx.loop.back().spills) {
                                          if (LIKELY(live_in.count(spilled.first.id())) &&
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

                                    float score = -1.0f;
                                    unsigned remat_val = 0;
                                    Temp to_spill = Temp();
                                    for (unsigned t_id : live_in) {
                                          if (UNLIKELY(t_id >= ctx.program->temp_rc.size() || t_id >= ctx.ssa_infos.size())) continue;
                                          Temp var = Temp(t_id, ctx.program->temp_rc[t_id]);
                                          if (var.type() != type || (LIKELY(block_idx < ctx.spills_entry.size()) && ctx.spills_entry[block_idx].count(var)) ||
                                                var.regClass().is_linear_vgpr())
                                                continue;

                                          unsigned can_remat = ctx.remat.count(var);
                                          if (can_remat > remat_val || (can_remat == remat_val && ctx.ssa_infos[t_id].precomputed_score > score)) {
                                                to_spill = var;
                                                score = ctx.ssa_infos[t_id].precomputed_score;
                                                remat_val = can_remat;
                                          }
                                    }

                                    if (UNLIKELY(to_spill == Temp())) {
                                          if (type == RegType::sgpr || (type == RegType::vgpr && loop_demand.vgpr <= ctx.target_pressure.vgpr))
                                                break;
                                          type = RegType::sgpr;
                                          continue;
                                    }
                                    if (LIKELY(block_idx < ctx.spills_entry.size())) {
                                          ctx.add_to_spills(to_spill, ctx.spills_entry[block_idx]);
                                          spilled_registers += to_spill;
                                          loop_demand -= to_spill;
                                    } else { assert(false); break; }
                              }
                              if (LIKELY(block_idx < ctx.spills_entry.size())) {
                                    loop_info info = {block_idx, ctx.spills_entry[block_idx], live_in};
                                    ctx.loop.emplace_back(std::move(info));
                              }

                              if (UNLIKELY(!loop_demand.exceeds(ctx.target_pressure)))
                                    return spilled_registers;

                        reg_pressure -= spilled_registers;

                        while (reg_pressure.exceeds(ctx.target_pressure)) {
                              float score = -1.0f;
                              Temp to_spill = Temp();
                              type = reg_pressure.vgpr > ctx.target_pressure.vgpr ? RegType::vgpr : RegType::sgpr;
                              for (aco_ptr<Instruction>& phi : block->instructions) {
                                    if (UNLIKELY(!is_phi(phi)))
                                          break;
                                    if (UNLIKELY(!phi->definitions[0].isTemp() || phi->definitions[0].isKill()))
                                          continue;

                                    unsigned def_id = phi->definitions[0].tempId();
                                    if (UNLIKELY(def_id >= ctx.ssa_infos.size())) continue;

                                    Temp var = phi->definitions[0].getTemp();
                                    if (var.type() == type &&
                                          (UNLIKELY(block_idx >= ctx.spills_entry.size()) || !ctx.spills_entry[block_idx].count(var)) &&
                                          ctx.ssa_infos[def_id].precomputed_score > score) {
                                          to_spill = var;
                                    score = ctx.ssa_infos[def_id].precomputed_score;
                                          }
                              }
                              if (UNLIKELY(to_spill == Temp())) break;
                              assert(to_spill != Temp());

                              if (LIKELY(block_idx < ctx.spills_entry.size())) {
                                    ctx.add_to_spills(to_spill, ctx.spills_entry[block_idx]);
                                    spilled_registers += to_spill;
                                    reg_pressure -= to_spill;
                              } else { assert(false); break; }
                        }
                        return spilled_registers;
                  }

                  if (block->linear_preds.size() == 1 && !(block->kind & block_kind_loop_exit)) {
                        unsigned pred_idx = block->linear_preds[0];
                        if (LIKELY(pred_idx < ctx.spills_exit.size() && block_idx < ctx.spills_entry.size())) {
                              for (std::pair<Temp, uint32_t> pair : ctx.spills_exit[pred_idx]) {
                                    if (pair.first.type() != RegType::sgpr)
                                          continue;
                                    if (LIKELY(live_in.count(pair.first.id()))) {
                                          spilled_registers += pair.first;
                                          ctx.spills_entry[block_idx].emplace(pair);
                                    }
                              }
                        }

                        if (UNLIKELY(block->logical_preds.empty()))
                              return spilled_registers;

                        pred_idx = block->logical_preds[0];
                        if (LIKELY(pred_idx < ctx.spills_exit.size() && block_idx < ctx.spills_entry.size())) {
                              for (std::pair<Temp, uint32_t> pair : ctx.spills_exit[pred_idx]) {
                                    if (pair.first.type() != RegType::vgpr)
                                          continue;
                                    if (LIKELY(live_in.count(pair.first.id()))) {
                                          spilled_registers += pair.first;
                                          ctx.spills_entry[block_idx].emplace(pair);
                                    }
                              }
                        }
                        return spilled_registers;
                  }

                  aco::unordered_map<Temp, bool> partial_spills{ctx.memory};

                  for (unsigned t_id : live_in) {
                        if (UNLIKELY(t_id >= ctx.program->temp_rc.size())) continue;
                        const RegClass rc = ctx.program->temp_rc[t_id];
                        Temp var = Temp(t_id, rc);
                        Block::edge_vec& preds = rc.is_linear() ? block->linear_preds : block->logical_preds;

                        const bool remat_val = ctx.remat.count(var);
                        bool avoid_respill_val = false;
                        if (block->loop_nest_depth > 0 && !ctx.loop.empty()) {
                              avoid_respill_val = ctx.loop.back().spills.count(var);
                        }

                        bool should_spill_var = true;
                        bool is_partial_spill_var = false;
                        uint32_t spill_id_val = 0;
                        for (unsigned pred_idx : preds) {
                              if (UNLIKELY(pred_idx >= ctx.spills_exit.size()) || !ctx.spills_exit[pred_idx].count(var)) {
                                    should_spill_var = false;
                              } else {
                                    is_partial_spill_var = true;
                                    spill_id_val = ctx.spills_exit[pred_idx][var];
                              }
                        }
                        should_spill_var |= (remat_val && is_partial_spill_var);
                        should_spill_var |= (avoid_respill_val && is_partial_spill_var);

                        if (should_spill_var && LIKELY(block_idx < ctx.spills_entry.size())) {
                              ctx.spills_entry[block_idx][var] = spill_id_val;
                              partial_spills.erase(var);
                              spilled_registers += var;
                        } else {
                              partial_spills[var] = is_partial_spill_var;
                        }
                  }

                  for (aco_ptr<Instruction>& phi : block->instructions) {
                        if (UNLIKELY(!is_phi(phi)))
                              break;
                        if (UNLIKELY(!phi->definitions[0].isTemp() || phi->definitions[0].isKill()))
                              continue;

                        Block::edge_vec& preds =
                        phi->opcode == aco_opcode::p_phi ? block->logical_preds : block->linear_preds;
                        bool is_all_undef = true;
                        bool is_all_spilled = true;
                        bool is_partial_spill_phi = false;
                        for (unsigned i = 0; i < phi->operands.size() && i < preds.size(); i++) {
                              if (phi->operands[i].isUndefined())
                                    continue;
                              unsigned pred_idx = preds[i];
                              if (UNLIKELY(pred_idx >= ctx.spills_exit.size())) { is_all_spilled = false; continue; }

                              bool spilled_operand = phi->operands[i].isTemp() &&
                              ctx.spills_exit[pred_idx].count(phi->operands[i].getTemp());
                              is_all_spilled &= spilled_operand;
                              is_partial_spill_phi |= spilled_operand;
                              is_all_undef = false;
                        }

                        if (is_all_spilled && !is_all_undef && LIKELY(block_idx < ctx.spills_entry.size())) {
                              ctx.add_to_spills(phi->definitions[0].getTemp(), ctx.spills_entry[block_idx]);
                              spilled_registers += phi->definitions[0].getTemp();
                              partial_spills.erase(phi->definitions[0].getTemp());
                        } else {
                              partial_spills[phi->definitions[0].getTemp()] = is_partial_spill_phi;
                        }
                  }

                  RegisterDemand reg_pressure = block->live_in_demand;
                  reg_pressure -= spilled_registers;

                  while (reg_pressure.exceeds(ctx.target_pressure)) {
                        if (UNLIKELY(partial_spills.empty())) { assert(false && "No partial spills to choose from but pressure exceeds target."); break; }
                        auto it = partial_spills.begin();
                        Temp to_spill = Temp();
                        bool is_partial_spill_candidate = false;
                        float score = -1.0f;
                        RegType type = reg_pressure.vgpr > ctx.target_pressure.vgpr ? RegType::vgpr : RegType::sgpr;

                        while (it != partial_spills.end()) {
                              if (LIKELY(block_idx < ctx.spills_entry.size()) && ctx.spills_entry[block_idx].count(it->first)) {
                                    it = partial_spills.erase(it);
                                    continue;
                              }
                              unsigned temp_id = it->first.id();
                              if (UNLIKELY(temp_id >= ctx.ssa_infos.size())) { ++it; continue; }

                              if (it->first.type() == type && !it->first.regClass().is_linear_vgpr() &&
                                    ((it->second && !is_partial_spill_candidate) ||
                                    (it->second == is_partial_spill_candidate &&
                                    ctx.ssa_infos[temp_id].precomputed_score > score))) {
                                    score = ctx.ssa_infos[temp_id].precomputed_score;
                              to_spill = it->first;
                              is_partial_spill_candidate = it->second;
                                    }
                                    ++it;
                        }
                        if (UNLIKELY(to_spill == Temp())) break;
                        assert(to_spill != Temp());
                        if (LIKELY(block_idx < ctx.spills_entry.size())) {
                              ctx.add_to_spills(to_spill, ctx.spills_entry[block_idx]);
                              partial_spills.erase(to_spill);
                              spilled_registers += to_spill;
                              reg_pressure -= to_spill;
                        } else { assert(false); break; }
                  }
                  return spilled_registers;
            }

            static void
            add_coupling_code(spill_ctx& ctx, Block* block, IDSet& live_in_current_block)
            {
                  const unsigned block_idx = block->index;
                  if (UNLIKELY(block_idx >= ctx.program->blocks.size() || block->linear_preds.empty()))
                        return;

                  if (block->linear_preds.size() == 1 && !(block->kind & (block_kind_loop_exit | block_kind_loop_header))) {
                        unsigned pred_idx = block->linear_preds[0];
                        if (LIKELY(pred_idx < ctx.processed.size() && ctx.processed[pred_idx] &&
                              block_idx < ctx.renames.size() && pred_idx < ctx.renames.size())) {
                              ctx.renames[block_idx] = ctx.renames[pred_idx];
                        if (!block->logical_preds.empty() && LIKELY(block_idx < ctx.program->blocks.size()) &&
                              !ctx.program->blocks[block_idx].logical_preds.empty() &&
                              ctx.program->blocks[block_idx].logical_preds[0] != pred_idx) {
                              unsigned logical_pred_idx = ctx.program->blocks[block_idx].logical_preds[0];
                        if (LIKELY(logical_pred_idx < ctx.renames.size())) {
                              for (auto const& [temp, renamed_temp] : ctx.renames[logical_pred_idx]) {
                                    if (temp.type() == RegType::vgpr)
                                          ctx.renames[block_idx].insert_or_assign(temp, renamed_temp);
                              }
                        }
                              }
                              }
                              return;
                  }

                  for (unsigned pred_idx_check : block->linear_preds) {
                        assert(pred_idx_check < ctx.processed.size() && ctx.processed[pred_idx_check]);
                  }

                  for (aco_ptr<Instruction>& phi_instr : block->instructions) {
                        if (UNLIKELY(!is_phi(phi_instr))) break;

                        for (const Operand& op : phi_instr->operands) {
                              if (op.isTemp() && LIKELY(op.tempId() < ctx.ssa_infos.size()))
                                    ctx.ssa_infos[op.tempId()].num_uses--;
                        }

                        if (UNLIKELY(!phi_instr->definitions[0].isTemp() ||
                              block_idx >= ctx.spills_entry.size() ||
                              !ctx.spills_entry[block_idx].count(phi_instr->definitions[0].getTemp())))
                              continue;

                        Block::edge_vec& preds = phi_instr->opcode == aco_opcode::p_phi ? block->logical_preds : block->linear_preds;
                        uint32_t def_spill_id = ctx.spills_entry[block_idx][phi_instr->definitions[0].getTemp()];
                        phi_instr->definitions[0].setKill(true);

                        for (unsigned i = 0; i < phi_instr->operands.size() && i < preds.size(); i++) {
                              if (phi_instr->operands[i].isUndefined()) continue;

                              unsigned pred_idx = preds[i];
                              if (UNLIKELY(pred_idx >= ctx.program->blocks.size())) continue;

                              Operand spill_op_operand = phi_instr->operands[i];
                              phi_instr->operands[i] = Operand(phi_instr->definitions[0].regClass());

                              if (spill_op_operand.isTemp()) {
                                    assert(spill_op_operand.isKill());
                                    Temp var = spill_op_operand.getTemp();

                                    if (LIKELY(pred_idx < ctx.renames.size())) {
                                          auto rename_it = ctx.renames[pred_idx].find(var);
                                          if (rename_it == ctx.renames[pred_idx].end() && ctx.remat.count(var))
                                                ctx.unused_remats.erase(ctx.remat[var].instr);
                                    }

                                    if (LIKELY(pred_idx < ctx.spills_exit.size())) {
                                          auto spilled_it = ctx.spills_exit[pred_idx].find(var);
                                          if (spilled_it != ctx.spills_exit[pred_idx].end()) {
                                                if (spilled_it->second != def_spill_id)
                                                      ctx.add_affinity(def_spill_id, spilled_it->second);
                                                continue;
                                          }
                                          if (var == phi_instr->definitions[0].getTemp()) {
                                                ctx.spills_exit[pred_idx][var] = def_spill_id;
                                          }
                                    }

                                    if (LIKELY(pred_idx < ctx.renames.size())) {
                                          auto rename_it = ctx.renames[pred_idx].find(var);
                                          if (rename_it != ctx.renames[pred_idx].end()) {
                                                spill_op_operand.setTemp(rename_it->second);
                                                ctx.renames[pred_idx].erase(rename_it);
                                          }
                                    }
                              }

                              if (LIKELY(pred_idx < ctx.spills_exit.size())) {
                                    for (const auto& pair_val : ctx.spills_exit[pred_idx])
                                          ctx.add_interference(def_spill_id, pair_val.second);
                              }

                              aco_ptr<Instruction> spill_instr_phi{create_instruction(aco_opcode::p_spill, Format::PSEUDO, 2, 0)};
                              spill_instr_phi->operands[0] = spill_op_operand;
                              spill_instr_phi->operands[1] = Operand::c32(def_spill_id);
                              Block& pred_block_ref = ctx.program->blocks[pred_idx];
                              auto insert_iterator = pred_block_ref.instructions.empty() ? pred_block_ref.instructions.end() :
                              (phi_instr->opcode == aco_opcode::p_phi ?
                              spill_ctx::get_insert_point_before_logical_end(pred_block_ref) :
                              std::prev(pred_block_ref.instructions.end()));
                              pred_block_ref.instructions.insert(insert_iterator, std::move(spill_instr_phi));
                        }
                  }

                  if (LIKELY(block_idx < ctx.spills_entry.size())) {
                        for (const auto& [temp_to_spill, spill_id_val] : ctx.spills_entry[block_idx]) {
                              if (UNLIKELY(!live_in_current_block.count(temp_to_spill.id()))) continue;

                              Block::edge_vec& preds_for_temp = temp_to_spill.is_linear() ? block->linear_preds : block->logical_preds;
                              for (unsigned pred_idx : preds_for_temp) {
                                    if (UNLIKELY(pred_idx >= ctx.program->blocks.size() || pred_idx >= ctx.spills_exit.size())) continue;

                                    auto spilled_it = ctx.spills_exit[pred_idx].find(temp_to_spill);
                                    if (spilled_it != ctx.spills_exit[pred_idx].end()) {
                                          if (spilled_it->second != spill_id_val)
                                                ctx.add_affinity(spill_id_val, spilled_it->second);
                                          continue;
                                    }

                                    const uint32_t loop_depth = std::min(ctx.program->blocks[pred_idx].loop_nest_depth,
                                                                         ctx.program->blocks[block_idx].loop_nest_depth);
                                    if (loop_depth > 0 && LIKELY((loop_depth - 1) < ctx.loop.size())) {
                                          auto& loop_spills_map = ctx.loop[loop_depth - 1].spills;
                                          auto loop_spill_it = loop_spills_map.find(temp_to_spill);
                                          if (loop_spill_it != loop_spills_map.end() && loop_spill_it->second == spill_id_val)
                                                continue;
                                    }

                                    for (const auto& pair_val : ctx.spills_exit[pred_idx])
                                          ctx.add_interference(pair_val.second, spill_id_val);

                                    Temp var_for_spill_instr = temp_to_spill;
                                    if (LIKELY(pred_idx < ctx.renames.size())) {
                                          auto rename_it = ctx.renames[pred_idx].find(var_for_spill_instr);
                                          if (rename_it != ctx.renames[pred_idx].end()) {
                                                var_for_spill_instr = rename_it->second;
                                                ctx.renames[pred_idx].erase(rename_it);
                                          }
                                    }

                                    aco_ptr<Instruction> spill_instr_live_in{create_instruction(aco_opcode::p_spill, Format::PSEUDO, 2, 0)};
                                    spill_instr_live_in->operands[0] = Operand(var_for_spill_instr);
                                    spill_instr_live_in->operands[1] = Operand::c32(spill_id_val);
                                    Block& pred_block_ref = ctx.program->blocks[pred_idx];
                                    auto insert_iterator = pred_block_ref.instructions.empty() ? pred_block_ref.instructions.end() :
                                    (temp_to_spill.type() == RegType::vgpr ?
                                    spill_ctx::get_insert_point_before_logical_end(pred_block_ref) :
                                    std::prev(pred_block_ref.instructions.end()));
                                    pred_block_ref.instructions.insert(insert_iterator, std::move(spill_instr_live_in));
                              }
                        }
                  }

                  for (aco_ptr<Instruction>& phi_instr : block->instructions) {
                        if (UNLIKELY(!is_phi(phi_instr)) || UNLIKELY(phi_instr->definitions[0].isKill())) continue;

                        assert(block_idx >= ctx.spills_entry.size() || !phi_instr->definitions[0].isTemp() ||
                        !ctx.spills_entry[block_idx].count(phi_instr->definitions[0].getTemp()));

                        Block::edge_vec& preds = phi_instr->opcode == aco_opcode::p_phi ? block->logical_preds : block->linear_preds;
                        for (unsigned i = 0; i < phi_instr->operands.size() && i < preds.size(); i++) {
                              if (UNLIKELY(!phi_instr->operands[i].isTemp())) continue;

                              unsigned pred_idx = preds[i];
                              if (UNLIKELY(pred_idx >= ctx.program->blocks.size() || pred_idx >= ctx.spills_exit.size() || pred_idx >= ctx.renames.size())) continue;

                              if (!ctx.spills_exit[pred_idx].count(phi_instr->operands[i].getTemp())) {
                                    auto rename_it = ctx.renames[pred_idx].find(phi_instr->operands[i].getTemp());
                                    if (rename_it != ctx.renames[pred_idx].end()) {
                                          phi_instr->operands[i].setTemp(rename_it->second);
                                    } else {
                                          auto remat_it = ctx.remat.find(phi_instr->operands[i].getTemp());
                                          if (remat_it != ctx.remat.end()) {
                                                ctx.unused_remats.erase(remat_it->second.instr);
                                          }
                                    }
                                    continue;
                              }

                              Temp original_temp = phi_instr->operands[i].getTemp();
                              Temp new_renamed_temp = ctx.program->allocateTmp(original_temp.regClass());
                              Block& pred_block_ref = ctx.program->blocks[pred_idx];

                              auto insert_iterator = pred_block_ref.instructions.empty() ? pred_block_ref.instructions.end() :
                              (phi_instr->opcode == aco_opcode::p_phi ?
                              spill_ctx::get_insert_point_before_logical_end(pred_block_ref) :
                              std::prev(pred_block_ref.instructions.end()));

                              aco_ptr<Instruction> reload_instr =
                              do_reload(ctx, original_temp, new_renamed_temp, ctx.spills_exit[pred_idx][original_temp]);

                              if (!phi_instr->definitions[0].isTemp()) {
                                    assert(phi_instr->definitions[0].isFixed() && phi_instr->definitions[0].physReg() == exec);
                                    reload_instr->definitions[0] = phi_instr->definitions[0];
                                    phi_instr->operands[i] = Operand(exec, ctx.program->lane_mask);
                              } else {
                                    ctx.spills_exit[pred_idx].erase(original_temp);
                                    ctx.renames[pred_idx][original_temp] = new_renamed_temp;
                                    phi_instr->operands[i].setTemp(new_renamed_temp);
                              }
                              pred_block_ref.instructions.insert(insert_iterator, std::move(reload_instr));
                        }
                  }

                  for (unsigned temp_id : live_in_current_block) {
                        if (UNLIKELY(temp_id >= ctx.program->temp_rc.size())) continue;
                        const RegClass rc = ctx.program->temp_rc[temp_id];
                        Temp var = Temp(temp_id, rc);

                        if (LIKELY(block_idx < ctx.spills_entry.size()) && ctx.spills_entry[block_idx].count(var)) continue;

                        Block::edge_vec& preds = rc.is_linear() ? block->linear_preds : block->logical_preds;
                        bool needs_new_phi = false;
                        Temp rename_target = Temp();
                        bool first_pred = true;

                        for (unsigned pred_idx : preds) {
                              if (UNLIKELY(pred_idx >= ctx.program->blocks.size() || pred_idx >= ctx.spills_exit.size() || pred_idx >= ctx.renames.size())) {
                                    needs_new_phi = true; break;
                              }

                              if (ctx.spills_exit[pred_idx].count(var)) {
                                    Temp new_name_for_reload = ctx.program->allocateTmp(rc);
                                    Block& pred_block_ref = ctx.program->blocks[pred_idx];
                                    auto insert_iterator = pred_block_ref.instructions.empty() ? pred_block_ref.instructions.end() :
                                    (rc.type() == RegType::vgpr ?
                                    spill_ctx::get_insert_point_before_logical_end(pred_block_ref) :
                                    std::prev(pred_block_ref.instructions.end()));

                                    aco_ptr<Instruction> reload_instr =
                                    do_reload(ctx, var, new_name_for_reload, ctx.spills_exit[pred_idx][var]);
                                    pred_block_ref.instructions.insert(insert_iterator, std::move(reload_instr));

                                    ctx.spills_exit[pred_idx].erase(var);
                                    ctx.renames[pred_idx][var] = new_name_for_reload;
                              }

                              Temp current_pred_temp = var;
                              if (ctx.renames[pred_idx].count(var)) {
                                    current_pred_temp = ctx.renames[pred_idx][var];
                              }

                              if (first_pred) {
                                    rename_target = current_pred_temp;
                                    first_pred = false;
                              } else if (rename_target != current_pred_temp) {
                                    needs_new_phi = true;
                              }
                        }
                        if (preds.empty() && rename_target == Temp()) {
                              rename_target = var;
                        }

                        if (needs_new_phi) {
                              aco_opcode new_phi_opcode = rc.is_linear() ? aco_opcode::p_linear_phi : aco_opcode::p_phi;
                              aco_ptr<Instruction> new_phi_instr{create_instruction(new_phi_opcode, Format::PSEUDO, preds.size(), 1)};
                              Temp new_phi_def_temp = ctx.program->allocateTmp(rc);

                              for (unsigned i = 0; i < new_phi_instr->operands.size() && i < preds.size(); i++) {
                                    unsigned pred_idx = preds[i];
                                    Temp operand_temp = var;
                                    if (LIKELY(pred_idx < ctx.renames.size()) && ctx.renames[pred_idx].count(var)) {
                                          operand_temp = ctx.renames[pred_idx][var];
                                    } else if (UNLIKELY(pred_idx >= ctx.program->blocks.size()) || pred_idx >= block_idx) {
                                          operand_temp = new_phi_def_temp;
                                    } else {
                                          if (ctx.remat.count(operand_temp)) {
                                                ctx.unused_remats.erase(ctx.remat[operand_temp].instr);
                                          }
                                    }
                                    new_phi_instr->operands[i] = Operand(operand_temp);
                              }
                              new_phi_instr->definitions[0] = Definition(new_phi_def_temp);
                              if (LIKELY(block_idx < ctx.program->blocks.size())) {
                                    auto& target_block_instructions = ctx.program->blocks[block_idx].instructions;
                                    auto insert_phi_it = target_block_instructions.begin();
                                    while (insert_phi_it != target_block_instructions.end() && is_phi(*insert_phi_it)) {
                                          ++insert_phi_it;
                                    }
                                    target_block_instructions.insert(insert_phi_it, std::move(new_phi_instr));
                              }
                              rename_target = new_phi_def_temp;
                        }

                        if (LIKELY(block_idx < ctx.renames.size()) && rename_target != Temp() && rename_target != var) {
                              ctx.renames[block_idx][var] = rename_target;
                        }
                  }
            }

            static void
            process_block(spill_ctx& ctx, unsigned idx, Block* blk,
                          RegisterDemand spilled)
            {
                  std::vector<aco_ptr<Instruction>> out;
                  out.reserve(blk->instructions.size());

                  unsigned ii = 0;
                  while (ii < blk->instructions.size() &&
                        is_phi(blk->instructions[ii]))
                        out.emplace_back(std::move(blk->instructions[ii++]));

                  auto& cur_spills = ctx.spills_exit[idx];

                  for (; ii < blk->instructions.size(); ++ii) {
                        aco_ptr<Instruction>& ins = blk->instructions[ii];

                        if (ins->opcode == aco_opcode::p_branch) {
                              out.emplace_back(std::move(ins));
                              continue;
                        }

                        std::map<Temp, std::pair<Temp, uint32_t>> reloads;

                        for (Operand& op : ins->operands) {
                              if (!op.isTemp())
                                    continue;

                              unsigned id = op.tempId();
                              if (op.isFirstKill() && LIKELY(idx < ctx.program->live.live_in.size()))
                                    ctx.program->live.live_in[idx].erase(id);
                              if (LIKELY(id < ctx.ssa_infos.size()))
                                    ctx.ssa_infos[id].num_uses--;

                              auto it = cur_spills.find(op.getTemp());
                              if (it == cur_spills.end())
                                    continue;

                              Temp new_tmp = ctx.program->allocateTmp(op.regClass());
                              if (LIKELY(idx < ctx.renames.size()))
                                    ctx.renames[idx][op.getTemp()] = new_tmp;
                              reloads[op.getTemp()] = {new_tmp, it->second};
                              cur_spills.erase(it);
                              spilled -= op.getTemp();
                        }

                        if (blk->register_demand.exceeds(ctx.target_pressure)) {
                              RegisterDemand need = ins->register_demand;

                              while ((need - spilled).exceeds(ctx.target_pressure)) {
                                    Temp cand = Temp();
                                    float best = -1.0f;
                                    RegType t = (need.vgpr - spilled.vgpr > ctx.target_pressure.vgpr) ?
                                    RegType::vgpr : RegType::sgpr;
                                    unsigned can_rematerialize_best = 0;
                                    unsigned is_loop_variable_or_reloaded_operand_best = 0;


                                    if (LIKELY(idx < ctx.program->live.live_in.size())) {
                                          for (unsigned id_live : ctx.program->live.live_in[idx]) {
                                                if (UNLIKELY(id_live >= ctx.program->temp_rc.size() || id_live >= ctx.ssa_infos.size())) continue;
                                                Temp v(id_live, ctx.program->temp_rc[id_live]);
                                                if (v.type() != t || cur_spills.count(v) || v.regClass().is_linear_vgpr())
                                                      continue;

                                                unsigned can_remat = ctx.remat.count(v);
                                                unsigned is_loop_var = 0;
                                                if(blk->loop_nest_depth > 0 && !ctx.loop.empty() && ctx.loop.back().spills.count(v))
                                                      is_loop_var = 1;
                                                bool is_reloaded_op = reloads.count(v);
                                                unsigned current_priority_flag = is_loop_var || is_reloaded_op;

                                                if (is_loop_variable_or_reloaded_operand_best > current_priority_flag || can_rematerialize_best > can_remat) continue;

                                                float s = ctx.ssa_infos[id_live].precomputed_score;
                                                if (can_remat > can_rematerialize_best || current_priority_flag > is_loop_variable_or_reloaded_operand_best ||
                                                      (can_remat == can_rematerialize_best && current_priority_flag == is_loop_variable_or_reloaded_operand_best && s > best)) {
                                                      best = s;
                                                cand = v;
                                                can_rematerialize_best = can_remat;
                                                is_loop_variable_or_reloaded_operand_best = current_priority_flag;
                                                      }
                                          }
                                    }
                                    if (cand == Temp()) break;

                                    if (is_loop_variable_or_reloaded_operand_best) {
                                          unsigned respill_slot_id = UINT32_MAX;
                                          if (blk->loop_nest_depth > 0 && !ctx.loop.empty() && ctx.loop.back().spills.count(cand))
                                                respill_slot_id = ctx.loop.back().spills.at(cand);
                                          else if (reloads.count(cand))
                                                respill_slot_id = reloads.at(cand).second;

                                          if (respill_slot_id != UINT32_MAX) {
                                                cur_spills[cand] = respill_slot_id;
                                                spilled += cand;
                                                continue;
                                          }
                                    }


                                    uint32_t sid = ctx.add_to_spills(cand, cur_spills);
                                    for(const auto& pair_val : reloads)
                                          ctx.add_interference(sid, pair_val.second.second);

                                    spilled += cand;

                                    Temp eff = cand;
                                    if (LIKELY(idx < ctx.renames.size())){
                                          auto rn  = ctx.renames[idx].find(cand);
                                          if (rn != ctx.renames[idx].end())
                                                eff = rn->second;
                                    }


                                    aco_ptr<Instruction> sp{create_instruction(aco_opcode::p_spill,
                                          Format::PSEUDO, 2, 0)};
                                          sp->operands[0] = Operand(eff);
                                          sp->operands[1] = Operand::c32(sid);
                                          out.emplace_back(std::move(sp));
                              }
                        }

                        for (const Definition& d : ins->definitions)
                              if (d.isTemp() && !d.isKill() && LIKELY(idx < ctx.program->live.live_in.size()))
                                    ctx.program->live.live_in[idx].insert(d.tempId());

                        for (Operand& op : ins->operands)
                              if (op.isTemp()) {
                                    auto rn = LIKELY(idx < ctx.renames.size()) ? ctx.renames[idx].find(op.getTemp()) : ctx.renames.back().end();
                                    if (LIKELY(idx < ctx.renames.size()) && rn != ctx.renames[idx].end())
                                          op.setTemp(rn->second);
                                    else if (ctx.remat.count(op.getTemp()))
                                          ctx.unused_remats.erase(ctx.remat[op.getTemp()].instr);
                              }

                              for (auto& p : reloads) {
                                    aco_ptr<Instruction> rl =
                                    do_reload(ctx, p.first, p.second.first, p.second.second);
                                    out.emplace_back(std::move(rl));
                              }

                              out.emplace_back(std::move(ins));
                  }

                  blk->instructions.swap(out);
            }

            static void
            spill_block(spill_ctx& ctx, unsigned idx)
            {
                  Block* blk = &ctx.program->blocks[idx];

                  RegisterDemand spilled = init_live_in_vars(ctx, blk, idx);

                  if (!(blk->kind & block_kind_loop_header) && LIKELY(idx < ctx.program->live.live_in.size()))
                        add_coupling_code(ctx, blk, ctx.program->live.live_in[idx]);

                  if (LIKELY(idx < ctx.spills_entry.size() && idx < ctx.spills_exit.size()))
                        ctx.spills_exit[idx] = ctx.spills_entry[idx];


                  process_block(ctx, idx, blk, spilled);

                  if (LIKELY(idx < ctx.processed.size()))
                        ctx.processed[idx] = true;

                  if (!blk->loop_nest_depth || UNLIKELY((idx + 1) >= ctx.program->blocks.size()) ||
                        ctx.program->blocks[idx + 1].loop_nest_depth >= blk->loop_nest_depth)
                        return;

                  if (UNLIKELY(ctx.loop.empty())) {
                        assert(false && "Loop stack empty an loop exit"); return;
                  }
                  uint32_t head = ctx.loop.back().index;
                  if (UNLIKELY(head >= ctx.renames.size())) {
                        assert(false && "Loop header index out of bounds for renames"); return;
                  }

                  auto ren = std::move(ctx.renames[head]);

                  if (LIKELY(head < ctx.program->live.live_in.size()))
                        add_coupling_code(ctx, &ctx.program->blocks[head], ctx.loop.back().live_in);

                  if (LIKELY(head < ctx.renames.size()))
                        ctx.renames[head].swap(ren);
                  ctx.loop.pop_back();

                  if (ren.empty())
                        return;

                  for (auto p : ren)
                        for (unsigned i = head; i <= idx; ++i)
                              if (LIKELY(i < ctx.renames.size()))
                                    ctx.renames[i].insert(p);

                  for (unsigned i = head; i <= idx; ++i) {
                        if (UNLIKELY(i >= ctx.program->blocks.size())) continue;
                        for (aco_ptr<Instruction>& ins : ctx.program->blocks[i].instructions) {
                              if (i == head && is_phi(ins))
                                    continue;
                              for (Operand& op : ins->operands)
                                    if (op.isTemp()) {
                                          auto it = ren.find(op.getTemp());
                                          if (it != ren.end())
                                                op.setTemp(it->second);
                                    }
                        }
                  }
            }
            static ALWAYS_INLINE unsigned
            find_available_slot_impl(const tiny_bitmap& used,
                                     unsigned           wave_size,
                                     unsigned           size,
                                     bool               is_sgpr)
            {
                  const unsigned wmask = wave_size - 1u;
                  unsigned       slot  = 0;

                  while (true) {
                        if (is_sgpr) {
                              const unsigned lane = slot & wmask;
                              if (lane + size > wave_size)
                                    slot += wave_size - lane;
                        }

                        const unsigned word_idx = slot >> 6;
                        uint64_t       free_bits = ~used.word(word_idx);
                        free_bits &= ~0ull << (slot & 63u);

                        if (!free_bits) {
                              slot = (word_idx + 1) << 6;
                              continue;
                        }

                        slot = (word_idx << 6) + __builtin_ctzll(free_bits);

                        bool ok = true;
                        for (unsigned i = 0; i < size; ++i) {
                              unsigned s = slot + i;
                              if ((is_sgpr && (s & wmask) == 0 && i > 0) || used.test(s)) {
                                    ok = false;
                                    break;
                              }
                        }
                        if (ok)
                              return slot;

                        ++slot;
                  }
            }

            static void
            setup_vgpr_spill_reload(spill_ctx& ctx,
                                    Block&,
                                    std::vector<aco_ptr<Instruction>>& instrs,
                                    uint32_t slot_idx,
                                    Temp& dyn_off,
                                    unsigned* imm_off_bytes)
            {
                  constexpr uint32_t BYTES_PER_LANE = 4u;
                  *imm_off_bytes = slot_idx * BYTES_PER_LANE;
                  dyn_off = Temp();

                  Builder bld(ctx.program, &instrs);

                  if (ctx.scratch_rsrc == Temp())
                        ctx.scratch_rsrc = load_scratch_resource(ctx, bld, false);

                  if (ctx.program->gfx_level >= GFX9) {
                        uint64_t full_off =
                        uint64_t(*imm_off_bytes) + ctx.program->dev.scratch_global_offset_min;
                        const uint32_t max_imm = ctx.program->dev.scratch_global_offset_max;

                        if (full_off <= max_imm) {
                              *imm_off_bytes = (unsigned)full_off;
                              return;
                        }

                        *imm_off_bytes = full_off % (uint64_t(max_imm) + 1u);
                        const uint64_t base_adjustment = full_off - (uint64_t)(*imm_off_bytes);


                        Temp lo = bld.tmp(s1), hi = bld.tmp(s1), d2 = bld.tmp(s1), d3 = bld.tmp(s1);

                        aco_ptr<Instruction> split_s4_instr{create_instruction(aco_opcode::p_split_vector, Format::PSEUDO, 1, 4)};
                        split_s4_instr->operands[0] = Operand(ctx.scratch_rsrc);
                        split_s4_instr->definitions[0] = Definition(lo);
                        split_s4_instr->definitions[1] = Definition(hi);
                        split_s4_instr->definitions[2] = Definition(d2);
                        split_s4_instr->definitions[3] = Definition(d3);
                        bld.insert(std::move(split_s4_instr));


                        Temp new_lo = bld.tmp(s1);
                        Temp new_hi = bld.tmp(s1);
                        Temp carry  = bld.tmp(s1);

                        bld.sop2(aco_opcode::s_add_u32, Definition(new_lo),
                                 bld.scc(Definition(carry)), lo,
                                 Operand::c32(uint32_t(base_adjustment)));
                        bld.sop2(aco_opcode::s_addc_u32, Definition(new_hi),
                                 bld.def(s1, scc), hi,
                                 Operand::c32(uint32_t(base_adjustment >> 32)), bld.scc(carry));

                        ctx.scratch_rsrc =
                        bld.pseudo(aco_opcode::p_create_vector, bld.def(s4),
                                   new_lo, new_hi, d2, d3);
                  } else {
                        const uint32_t max_imm  = ctx.program->dev.buf_offset_max;

                        if (*imm_off_bytes > max_imm) {
                              uint32_t dyn_part_val = *imm_off_bytes - (*imm_off_bytes % (max_imm + 1u));
                              *imm_off_bytes   = *imm_off_bytes % (max_imm + 1u);
                              if (dyn_part_val > 0)
                                    dyn_off = bld.copy(bld.def(s1), Operand::c32(dyn_part_val));
                        }
                  }
            }

            template <bool IsStore>
            static ALWAYS_INLINE void
            emit_scratch_vgpr_io(spill_ctx& ctx, Builder& bld,
                                 Temp value, Temp scratch_rsrc_arg,
                                 Temp dyn_off, unsigned imm_off,
                                 memory_sync_info sync)
            {
                  if (ctx.program->gfx_level >= GFX9) {
                        const aco_opcode op = IsStore
                        ? aco_opcode::scratch_store_dword
                        : aco_opcode::scratch_load_dword;
                        if constexpr(IsStore)
                              bld.scratch(op, Operand(v1), scratch_rsrc_arg, value, imm_off, sync);
                        else
                              bld.scratch(op, Definition(value), Operand(v1), scratch_rsrc_arg,
                                          imm_off, sync);
                  } else {
                        const aco_opcode op = IsStore
                        ? aco_opcode::buffer_store_dword
                        : aco_opcode::buffer_load_dword;
                        if constexpr(IsStore) {
                              Instruction* st = bld.mubuf(op, scratch_rsrc_arg, Operand(v1),
                                                          dyn_off, value, imm_off, false);
                              st->mubuf().sync = sync;
                              st->mubuf().cache.value = ac_swizzled;
                        } else {
                              Instruction* ld = bld.mubuf(op, Definition(value), scratch_rsrc_arg,
                                                          Operand(v1), dyn_off, imm_off, false);
                              ld->mubuf().sync = sync;
                              ld->mubuf().cache.value = ac_swizzled;
                        }
                  }
            }

            static void
            spill_vgpr(spill_ctx& ctx, Block& blk,
                       std::vector<aco_ptr<Instruction>>& instrs,
                       aco_ptr<Instruction>& spill,
                       const std::vector<uint32_t>& slots)
            {
                  ctx.program->config->spilled_vgprs += spill->operands[0].size();

                  const uint32_t id   = spill->operands[1].constantValue();
                  const uint32_t slot_idx = slots[id];

                  Temp dyn_off;
                  unsigned imm_off;
                  setup_vgpr_spill_reload(ctx, blk, instrs, slot_idx, dyn_off, &imm_off);

                  Builder bld(ctx.program, &instrs);
                  Temp val = spill->operands[0].getTemp();
                  if (val.size() > 1) {
                        Instruction* split_instr =
                        create_instruction(aco_opcode::p_split_vector, Format::PSEUDO,
                                           1, val.size());
                        split_instr->operands[0] = Operand(val);
                        for (unsigned i = 0; i < val.size(); ++i)
                              split_instr->definitions[i] = bld.def(v1);
                        bld.insert(split_instr);

                        unsigned current_imm_off = imm_off;
                        for (unsigned i = 0; i < val.size(); ++i, current_imm_off += 4)
                              emit_scratch_vgpr_io<true>(ctx, bld, split_instr->definitions[i].getTemp(),
                                                         ctx.scratch_rsrc, dyn_off, current_imm_off,
                                                         memory_sync_info(storage_vgpr_spill,
                                                                          semantic_private));
                  } else {
                        emit_scratch_vgpr_io<true>(ctx, bld, val, ctx.scratch_rsrc,
                                                   dyn_off, imm_off,
                                                   memory_sync_info(storage_vgpr_spill,
                                                                    semantic_private));
                  }
            }

            static void
            reload_vgpr(spill_ctx& ctx, Block& blk,
                        std::vector<aco_ptr<Instruction>>& instrs,
                        aco_ptr<Instruction>& reload,
                        const std::vector<uint32_t>& slots)
            {
                  const uint32_t id   = reload->operands[0].constantValue();
                  const uint32_t slot_idx = slots[id];

                  Temp dyn_off;
                  unsigned imm_off;
                  setup_vgpr_spill_reload(ctx, blk, instrs, slot_idx, dyn_off, &imm_off);

                  Builder bld(ctx.program, &instrs);
                  Definition def = reload->definitions[0];

                  if (def.size() > 1) {
                        Instruction* vec_instr =
                        create_instruction(aco_opcode::p_create_vector, Format::PSEUDO,
                                           def.size(), 1);
                        vec_instr->definitions[0] = def;

                        unsigned current_imm_off = imm_off;
                        for (unsigned i = 0; i < def.size(); ++i, current_imm_off += 4) {
                              Temp tmp = bld.tmp(v1);
                              vec_instr->operands[i] = Operand(tmp);

                              emit_scratch_vgpr_io<false>(ctx, bld, tmp, ctx.scratch_rsrc,
                                                          dyn_off, current_imm_off,
                                                          memory_sync_info(storage_vgpr_spill,
                                                                           semantic_private));
                        }
                        bld.insert(vec_instr);
                  } else {
                        emit_scratch_vgpr_io<false>(ctx, bld, def.getTemp(), ctx.scratch_rsrc,
                                                    dyn_off, imm_off,
                                                    memory_sync_info(storage_vgpr_spill,
                                                                     semantic_private));
                  }
            }

            static void
            assign_spill_slots_helper(spill_ctx&            ctx,
                                      RegType               type,
                                      std::vector<bool>&    assigned,
                                      std::vector<uint32_t>&slots,
                                      unsigned*             num_slots_out)
            {
                  tiny_bitmap used;
                  unsigned    max_slot = 0;
                  const bool  is_sgpr  = (type == RegType::sgpr);

                  const auto mark_used_globally = [&](unsigned base, unsigned cnt) {
                        if (!cnt)
                              return;
                        used.set_range(base, cnt);
                        max_slot = std::max(max_slot, base + cnt);
                  };

                  auto place_item = [&](uint32_t item_id, const tiny_bitmap& blocked_slots_for_item) {
                        const unsigned sz   = ctx.interferences[item_id].first.size();
                        const unsigned base_slot = find_available_slot_impl(blocked_slots_for_item, ctx.wave_size, sz, is_sgpr);
                        slots[item_id]    = base_slot;
                        assigned[item_id] = true;
                        mark_used_globally(base_slot, sz);
                  };

                  for (const auto& grp : ctx.affinities) {
                        if (grp.empty())
                              continue;

                        const uint32_t first_in_group = grp.front();
                        if (UNLIKELY(first_in_group >= ctx.interferences.size() ||
                              ctx.interferences[first_in_group].first.type() != type))
                              continue;

                        bool group_is_active = false;
                        for (uint32_t id_in_grp : grp)
                              if (LIKELY(id_in_grp < ctx.is_reloaded.size()) && ctx.is_reloaded[id_in_grp]) {
                                    group_is_active = true;
                                    break;
                              }
                              if (!group_is_active)
                                    continue;

                        tiny_bitmap blocked_slots_for_group = used;
                        for (uint32_t id_in_grp : grp) {
                              if (UNLIKELY(id_in_grp >= ctx.interferences.size())) continue;
                              for (uint32_t interfering_spill_id : ctx.interferences[id_in_grp].second) {
                                    if (UNLIKELY(interfering_spill_id >= assigned.size() || !assigned[interfering_spill_id])) continue;
                                    if (UNLIKELY(interfering_spill_id >= ctx.interferences.size())) continue;
                                    if (ctx.interferences[interfering_spill_id].first.type() == type) {
                                          bool part_of_current_grp = false;
                                          for(uint32_t member_in_grp : grp) if(member_in_grp == interfering_spill_id) part_of_current_grp = true;
                                          if(part_of_current_grp) continue;
                                          blocked_slots_for_group.set_range(slots[interfering_spill_id], ctx.interferences[interfering_spill_id].first.size());
                                    }
                              }
                        }


                        place_item(first_in_group, blocked_slots_for_group);
                        for (uint32_t id_in_grp : grp) {
                              if (id_in_grp == first_in_group) continue;
                              if (LIKELY(id_in_grp < assigned.size() && !assigned[id_in_grp])) {
                                    if (LIKELY(id_in_grp < slots.size()))
                                          slots[id_in_grp]    = slots[first_in_group];
                                    assigned[id_in_grp] = true;
                              }
                        }
                  }

                  const unsigned n_interferences = ctx.interferences.size();
                  for (unsigned id = 0; id < n_interferences; ++id) {
                        if (UNLIKELY(id >= ctx.is_reloaded.size() || !ctx.is_reloaded[id] ||
                              id >= assigned.size() || assigned[id] ||
                              id >= ctx.interferences.size() || ctx.interferences[id].first.type() != type))
                              continue;

                        tiny_bitmap blocked_slots_for_single = used;
                        for (uint32_t interfering_spill_id : ctx.interferences[id].second)
                              if (LIKELY(interfering_spill_id < assigned.size() && assigned[interfering_spill_id] &&
                                    LIKELY(interfering_spill_id < ctx.interferences.size()) &&
                                    ctx.interferences[interfering_spill_id].first.type() == type)) {
                                    blocked_slots_for_single.set_range(slots[interfering_spill_id], ctx.interferences[interfering_spill_id].first.size());
                                    }
                                    place_item(id, blocked_slots_for_single);
                  }
                  *num_slots_out = max_slot;
            }


            static void
            end_unused_spill_vgprs(spill_ctx&                  ctx,
                                   Block&                      blk,
                                   std::vector<Temp>&          lin_vgpr_for_sgpr,
                                   const std::vector<uint32_t>&slots,
                                   const aco::unordered_map<Temp, uint32_t>& blk_spills_entry)
            {
                  if (ctx.wave_size == 0 || lin_vgpr_for_sgpr.empty())
                        return;

                  std::vector<uint8_t> used_linear_vgprs(lin_vgpr_for_sgpr.size(), 0);
                  for (auto const& pair_val : blk_spills_entry) {
                        Temp t = pair_val.first;
                        uint32_t id = pair_val.second;

                        if (t.type() != RegType::sgpr)
                              continue;
                        if (UNLIKELY(id >= ctx.is_reloaded.size() || !ctx.is_reloaded[id]))
                              continue;
                        if (UNLIKELY(id >= slots.size()))
                              continue;

                        const unsigned vgpr_array_idx = slots[id] / ctx.wave_size;
                        if (LIKELY(vgpr_array_idx < used_linear_vgprs.size()))
                              used_linear_vgprs[vgpr_array_idx] = 1;
                  }

                  std::vector<Temp> temps_to_end;
                  temps_to_end.reserve(lin_vgpr_for_sgpr.size());
                  for (unsigned i = 0; i < lin_vgpr_for_sgpr.size(); ++i)
                        if (lin_vgpr_for_sgpr[i].id() && !used_linear_vgprs[i]) {
                              temps_to_end.push_back(lin_vgpr_for_sgpr[i]);
                              lin_vgpr_for_sgpr[i] = Temp();
                        }

                        if (temps_to_end.empty() || blk.linear_preds.empty())
                              return;

                  aco_ptr<Instruction> end_instr{
                        create_instruction(aco_opcode::p_end_linear_vgpr, Format::PSEUDO,
                                           temps_to_end.size(), 0)};
                                           for (unsigned i = 0; i < temps_to_end.size(); ++i)
                                                 end_instr->operands[i] = Operand(temps_to_end[i]);

                  auto it = blk.instructions.begin();
                  while (it != blk.instructions.end() && is_phi(*it))
                        ++it;
                  blk.instructions.insert(it, std::move(end_instr));
            }

            static void
            assign_spill_slots(spill_ctx& ctx, unsigned)
            {
                  if (!ctx.interferences_finalized_for_assignment) {
                        for (auto& p : ctx.interferences) {
                              auto& v = p.second;
                              if (!v.empty()) {
                                    std::sort(v.begin(), v.end());
                                    v.erase(std::unique(v.begin(), v.end()), v.end());
                              }
                        }
                        ctx.interferences_finalized_for_assignment = true;
                  }

                  const size_t N = ctx.interferences.size();
                  std::vector<uint32_t> slots   (N, 0);
                  std::vector<bool>     assigned(N, false);

                  for (const auto& grp : ctx.affinities) {
                        if (grp.empty())
                              continue;

                        bool reload = false;
                        for (uint32_t id : grp)
                              if (LIKELY(id < ctx.is_reloaded.size()) && ctx.is_reloaded[id]) {
                                    reload = true;
                                    break;
                              }
                              if (reload)
                                    for (uint32_t id : grp)
                                          if (LIKELY(id < ctx.is_reloaded.size()))
                                                ctx.is_reloaded[id] = true;
                  }

                  assign_spill_slots_helper(ctx, RegType::sgpr, assigned, slots,
                                            &ctx.sgpr_spill_slots);
                  assign_spill_slots_helper(ctx, RegType::vgpr, assigned, slots,
                                            &ctx.vgpr_spill_slots);

                  const unsigned lin_cnt =
                  (ctx.sgpr_spill_slots + ctx.wave_size - 1) / ctx.wave_size;
                  std::vector<Temp> lin_vgpr_for_sgpr(lin_cnt);

                  unsigned last_top = 0;
                  for (const Block& b : ctx.program->blocks)
                        if (b.kind & block_kind_top_level)
                              last_top = b.index;

                  for (Block& blk : ctx.program->blocks) {
                        if (blk.kind & block_kind_top_level) {
                              if (LIKELY(blk.index < ctx.spills_entry.size()))
                                    end_unused_spill_vgprs(ctx, blk, lin_vgpr_for_sgpr,
                                                           slots, ctx.spills_entry[blk.index]);

                                    if (blk.kind & block_kind_resume)
                                          ctx.resume_idx++;

                              if (blk.linear_preds.empty())
                                    ctx.scratch_rsrc = Temp();
                        }

                        std::vector<aco_ptr<Instruction>> out;
                        out.reserve(blk.instructions.size() + 8);

                        for (aco_ptr<Instruction>& ins : blk.instructions) {
                              const bool is_sp  = ins->opcode == aco_opcode::p_spill;
                              const bool is_rl  = ins->opcode == aco_opcode::p_reload;

                              if (!is_sp && !is_rl) {
                                    if (ctx.unused_remats.count(ins.get()) == 0)
                                          out.emplace_back(std::move(ins));
                                    continue;
                              }

                              const uint32_t id = is_sp ? ins->operands[1].constantValue()
                              : ins->operands[0].constantValue();
                              if (UNLIKELY(id >= ctx.is_reloaded.size() || !ctx.is_reloaded[id] ||
                                    id >= ctx.interferences.size()))
                                    continue;

                              const RegClass rc = ctx.interferences[id].first;
                              if (UNLIKELY(id >= assigned.size() || !assigned[id])) {
                                    out.emplace_back(std::move(ins));
                                    continue;
                              }


                              if (rc.type() == RegType::vgpr) {
                                    if (is_sp)
                                          spill_vgpr(ctx, blk, out, ins, slots);
                                    else
                                          reload_vgpr(ctx, blk, out, ins, slots);
                              } else {
                                    const unsigned slot_val = slots[id];
                                    const unsigned v_idx    = slot_val / ctx.wave_size;
                                    const unsigned lane_idx = slot_val % ctx.wave_size;

                                    if (UNLIKELY(v_idx >= lin_vgpr_for_sgpr.size())) {
                                          assert(false && "SGPR spill target linear VGPR index out of bounds.");
                                          out.emplace_back(std::move(ins));
                                          continue;
                                    }

                                    if (!lin_vgpr_for_sgpr[v_idx].id()) {
                                          Temp lin = ctx.program->allocateTmp(
                                                RegClass(RegType::vgpr, ctx.wave_size).as_linear());
                                          lin_vgpr_for_sgpr[v_idx] = lin;

                                          aco_ptr<Instruction> start{
                                                create_instruction(aco_opcode::p_start_linear_vgpr, Format::PSEUDO,
                                                                   0, 1)};
                                                                   start->definitions[0] = Definition(lin);
                                                                   Block& top            = ctx.program->blocks[last_top];
                                                                   top.instructions.insert(
                                                                         spill_ctx::get_insert_point_before_logical_end(top),
                                                                                           std::move(start));
                                    }

                                    if (is_sp) {
                                          aco_ptr<Instruction> sg_sp{
                                                create_instruction(aco_opcode::p_spill, Format::PSEUDO, 3, 0)};
                                                sg_sp->operands[0] = Operand(lin_vgpr_for_sgpr[v_idx]);
                                                sg_sp->operands[1] = Operand::c32(lane_idx);
                                                sg_sp->operands[2] = ins->operands[0];
                                                out.emplace_back(std::move(sg_sp));
                                                ctx.program->config->spilled_sgprs += ins->operands[0].size();
                                    } else {
                                          aco_ptr<Instruction> sg_rl{
                                                create_instruction(aco_opcode::p_reload, Format::PSEUDO, 2, 1)};
                                                sg_rl->operands[0]   = Operand(lin_vgpr_for_sgpr[v_idx]);
                                                sg_rl->operands[1]   = Operand::c32(lane_idx);
                                                sg_rl->definitions[0]= ins->definitions[0];
                                                out.emplace_back(std::move(sg_rl));
                                    }
                              }
                        }
                        blk.instructions.swap(out);
                  }

                  ctx.program->config->scratch_bytes_per_wave +=
                  ctx.vgpr_spill_slots * 4u * ctx.wave_size;
            }

      }

      void spill(Program* program)
      {
            if (!program || !program->wave_size)
                  return;

            program->config->spilled_vgprs = 0;
            program->config->spilled_sgprs = 0;
            program->progress              = CompilationProgress::after_spilling;

            lower_to_cssa(program);

            const RegisterDemand demand = program->max_reg_demand;
            const RegisterDemand limit  = get_addr_regs_from_waves(program,
                                                                   program->min_waves);

            uint16_t extra_vgpr = 0, extra_sgpr = 0;

            if (demand.sgpr > limit.sgpr) {
                  unsigned diff = demand.sgpr - limit.sgpr;
                  extra_vgpr = DIV_ROUND_UP(diff * 2u, program->wave_size) + 1u;
            }
            if (demand.vgpr + extra_vgpr > limit.vgpr) {
                  extra_sgpr = program->gfx_level >= GFX9 ? 1 : 5;
                  if (demand.sgpr + extra_sgpr > limit.sgpr) {
                        unsigned diff = demand.sgpr + extra_sgpr - limit.sgpr;
                        extra_vgpr = DIV_ROUND_UP(diff * 2u, program->wave_size) + 1u;
                  }
            }

            RegisterDemand target(
                  limit.vgpr > extra_vgpr ? limit.vgpr - extra_vgpr : 0,
                  limit.sgpr > extra_sgpr ? limit.sgpr - extra_sgpr : 0);

            spill_ctx ctx(target, program);

            gather_ssa_use_info(ctx);
            get_rematerialize_info(ctx);

            for (unsigned i = 0; i < program->blocks.size(); ++i)
                  spill_block(ctx, i);

            assign_spill_slots(ctx, extra_vgpr);

            live_var_analysis(program);
      }

} // namespace aco
