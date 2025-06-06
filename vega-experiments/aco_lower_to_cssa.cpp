#include "aco_builder.h"
#include "aco_ir.h"

#include <algorithm>
#include <map>
#include <unordered_map>
#include <vector>

/*
 * Implements an algorithm to lower to Conventional SSA Form (CSSA).
 * After "Revisiting Out-of-SSA Translation for Correctness, CodeQuality, and Efficiency"
 * by B. Boissinot, A. Darte, F. Rastello, B. Dupont de Dinechin, C. Guillon,
 *
 * By lowering the IR to CSSA, the insertion of parallelcopies is separated from
 * the register coalescing problem. Additionally, correctness is ensured w.r.t. spilling.
 * The algorithm coalesces non-interfering phi-resources while taking value-equality
 * into account. Re-indexes the SSA-defs.
 */

namespace aco {
      namespace {

            typedef std::vector<Temp> merge_set;

            struct copy {
                  Definition def;
                  Operand op;
            };

            struct merge_node {
                  Operand value = Operand(); /* original value: can be an SSA-def or constant value */
                  uint32_t index = -1u;      /* index into the vector of merge sets */
                  uint32_t defined_at = -1u; /* defining block */

                  Temp equal_anc_in = Temp();  /* within the same merge set */
                  Temp equal_anc_out = Temp(); /* from the other set we're currently trying to merge with */
            };

            struct cssa_ctx {
                  Program* program;
                  std::vector<std::vector<copy>> parallelcopies; /* copies per block */
                  std::vector<merge_set> merge_sets;             /* each vector is one (ordered) merge set */
                  std::unordered_map<uint32_t, merge_node> merge_node_table; /* tempid -> merge node */
            };

            void
            collect_parallelcopies(cssa_ctx& ctx)
            {
                  ctx.parallelcopies.resize(ctx.program->blocks.size());
                  // Heuristic reservation for merge_sets assuming on average up to 1 new set per block with phis.
                  if (ctx.program->blocks.size() > 0) {
                        ctx.merge_sets.reserve(ctx.program->blocks.size());
                  }

                  Builder bld(ctx.program);
                  for (Block& block : ctx.program->blocks) {
                        for (aco_ptr<Instruction>& phi : block.instructions) {
                              if (phi->opcode != aco_opcode::p_phi && phi->opcode != aco_opcode::p_linear_phi) [[unlikely]]
                                    break;

                              const Definition& def = phi->definitions[0];

                              if (!def.isTemp() || def.isKill()) [[unlikely]]
                                    continue;

                              Block::edge_vec& preds =
                              phi->opcode == aco_opcode::p_phi ? block.logical_preds : block.linear_preds;
                              uint32_t index = ctx.merge_sets.size();
                              merge_set set;
                              if (phi->operands.size() > 0) {
                                    set.reserve(phi->operands.size() + 1);
                              }


                              bool has_preheader_copy = false;
                              for (unsigned i = 0; i < phi->operands.size(); i++) {
                                    Operand op = phi->operands[i];
                                    if (op.isUndefined()) [[unlikely]]
                                          continue;

                                    if (def.regClass().type() == RegType::sgpr && !op.isTemp()) {
                                          if (op.isConstant()) {
                                                if (ctx.program->gfx_level >= GFX10)
                                                      continue;
                                                if (op.size() == 1 && !op.isLiteral())
                                                      continue;
                                          } else {
                                                assert(op.isFixed() && op.physReg() == exec);
                                                continue;
                                          }
                                    }

                                    if (preds[i] < ctx.parallelcopies.size() && ctx.parallelcopies[preds[i]].empty()) {
                                          ctx.parallelcopies[preds[i]].reserve(4);
                                    }

                                    Temp tmp = bld.tmp(def.regClass());
                                    ctx.parallelcopies[preds[i]].emplace_back(copy{Definition(tmp), op});
                                    phi->operands[i] = Operand(tmp);
                                    phi->operands[i].setKill(true);

                                    set.emplace_back(tmp);
                                    ctx.merge_node_table[tmp.id()] = {op, index, preds[i]};

                                    has_preheader_copy |= (i == 0 && (block.kind & block_kind_loop_header));
                              }

                              if (set.empty()) [[unlikely]]
                                    continue;

                              if (def.isTemp()) {
                                    if (has_preheader_copy)
                                          set.emplace(std::next(set.begin()), def.getTemp());
                                    else if (block.kind & block_kind_loop_header)
                                          set.emplace(set.begin(), def.getTemp());
                                    else
                                          set.emplace_back(def.getTemp());
                                    ctx.merge_node_table[def.tempId()] = {Operand(def.getTemp()), index, block.index};
                              }
                              ctx.merge_sets.emplace_back(std::move(set));
                        }
                  }
            }

            /* check whether the definition of a comes after b. */
            inline bool
            defined_after(cssa_ctx& ctx, Temp a, Temp b)
            {
                  merge_node& node_a = ctx.merge_node_table[a.id()];
                  merge_node& node_b = ctx.merge_node_table[b.id()];
                  if (node_a.defined_at == node_b.defined_at)
                        return a.id() > b.id();

                  return node_a.defined_at > node_b.defined_at;
            }

            /* check whether a dominates b where b is defined after a */
            inline bool
            dominates(cssa_ctx& ctx, Temp a, Temp b)
            {
                  assert(defined_after(ctx, b, a));
                  Block& parent = ctx.program->blocks[ctx.merge_node_table[a.id()].defined_at];
                  Block& child = ctx.program->blocks[ctx.merge_node_table[b.id()].defined_at];
                  if (b.regClass().type() == RegType::vgpr)
                        return dominates_logical(parent, child);
                  else
                        return dominates_linear(parent, child);
            }

            /* Checks whether some variable is live-out, not considering any phi-uses. */
            inline bool
            is_live_out(cssa_ctx& ctx, Temp var, uint32_t block_idx)
            {
                  Block::edge_vec& succs = var.is_linear() ? ctx.program->blocks[block_idx].linear_succs
                  : ctx.program->blocks[block_idx].logical_succs;

                  return std::any_of(succs.begin(), succs.end(), [&](unsigned succ)
                  { return ctx.program->live.live_in[succ].count(var.id()); });
            }

            /* check intersection between var and parent:
             * We already know that parent dominates var. */
            inline bool
            intersects(cssa_ctx& ctx, Temp var, Temp parent)
            {
                  merge_node& node_var = ctx.merge_node_table[var.id()];
                  merge_node& node_parent = ctx.merge_node_table[parent.id()];
                  assert(node_var.index != node_parent.index);
                  uint32_t block_idx = node_var.defined_at;

                  if (node_parent.defined_at < node_var.defined_at) {
                        if (!ctx.program->live.live_in[block_idx].count(parent.id()))
                              return false;
                  }

                  bool parent_live = is_live_out(ctx, parent, block_idx);
                  if (parent_live)
                        return true;

                  for (const copy& cp : ctx.parallelcopies[block_idx]) {
                        if (cp.def.getTemp() == var)
                              return false;
                        if (cp.op.isTemp() && cp.op.getTemp() == parent)
                              parent_live = true;
                  }
                  if (parent_live)
                        return true;

                  const Block& block = ctx.program->blocks[block_idx];
                  for (auto it = block.instructions.crbegin(); it != block.instructions.crend(); ++it) {
                        if (is_phi(it->get()))
                              break;

                        for (const Definition& def : (*it)->definitions) {
                              if (!def.isTemp())
                                    continue;
                              if (def.getTemp() == var)
                                    return false;
                        }

                        for (const Operand& op : (*it)->operands) {
                              if (!op.isTemp())
                                    continue;
                              if (op.getTemp() == parent)
                                    return true;
                        }
                  }

                  return false;
            }

            /* check interference between var and parent:
             * i.e. they have different values and intersect.
             * If parent and var intersect and share the same value, also updates the equal ancestor. */
            inline bool
            interference(cssa_ctx& ctx, Temp var, Temp parent)
            {
                  assert(var != parent);
                  merge_node& node_var = ctx.merge_node_table[var.id()];
                  node_var.equal_anc_out = Temp();

                  if (node_var.index == ctx.merge_node_table[parent.id()].index) {
                        parent = ctx.merge_node_table[parent.id()].equal_anc_out;
                  }

                  Temp tmp = parent;
                  while (tmp != Temp() && !intersects(ctx, var, tmp)) {
                        merge_node& node_tmp = ctx.merge_node_table[tmp.id()];
                        tmp = node_tmp.equal_anc_in;
                  }

                  if (tmp == Temp())
                        return false;

                  if (node_var.value == ctx.merge_node_table[parent.id()].value) {
                        node_var.equal_anc_out = tmp;
                        return false;
                  }

                  return true;
            }

            /* tries to merge set_b into set_a of given temporary and
             * drops that temporary as it is being coalesced */
            bool
            try_merge_merge_set(cssa_ctx& ctx, Temp dst, merge_set& set_b)
            {
                  auto def_node_it = ctx.merge_node_table.find(dst.id());
                  uint32_t index = def_node_it->second.index;
                  merge_set& set_a = ctx.merge_sets[index];
                  std::vector<Temp> dom;
                  merge_set union_set;
                  // Reserve for union_set: sum of sizes is an upper bound.
                  union_set.reserve(set_a.size() + set_b.size());
                  uint32_t i_a = 0;
                  uint32_t i_b = 0;

                  while (i_a < set_a.size() || i_b < set_b.size()) {
                        Temp current;
                        if (i_a == set_a.size())
                              current = set_b[i_b++];
                        else if (i_b == set_b.size())
                              current = set_a[i_a++];
                        else if (defined_after(ctx, set_a[i_a], set_b[i_b]))
                              current = set_b[i_b++];
                        else
                              current = set_a[i_a++];

                        while (!dom.empty() && !dominates(ctx, dom.back(), current))
                              dom.pop_back();

                        if (!dom.empty() && interference(ctx, current, dom.back())) [[unlikely]] {
                              for (Temp t : union_set)
                                    ctx.merge_node_table[t.id()].equal_anc_out = Temp();
                              return false;
                        }

                        dom.emplace_back(current);
                        if (current != dst)
                              union_set.emplace_back(current);
                  }

                  for (Temp t : union_set) {
                        merge_node& node = ctx.merge_node_table[t.id()];
                        Temp in = node.equal_anc_in;
                        Temp out = node.equal_anc_out;
                        if (in == Temp() || (out != Temp() && defined_after(ctx, out, in)))
                              node.equal_anc_in = out;
                        node.equal_anc_out = Temp();
                        node.index = index;
                  }
                  set_b.clear(); // Clear and shrink if needed, or just let it be reassigned
                  set_b.shrink_to_fit();
                  ctx.merge_sets[index] = std::move(union_set); // Use std::move
                  ctx.merge_node_table.erase(dst.id());

                  return true;
            }

            /* returns true if the copy can safely be omitted */
            bool
            try_coalesce_copy(cssa_ctx& ctx, copy copy, uint32_t block_idx)
            {
                  if (!copy.op.isTemp() || !copy.op.isKill()) [[unlikely]]
                        return false;

                  if (copy.op.regClass() != copy.def.regClass()) [[unlikely]]
                        return false;

                  merge_node& op_node = ctx.merge_node_table[copy.op.tempId()];
                  if (op_node.defined_at == -1u) {
                        while (ctx.program->live.live_in[block_idx].count(copy.op.tempId()))
                              block_idx = copy.op.regClass().type() == RegType::vgpr
                              ? ctx.program->blocks[block_idx].logical_idom
                              : ctx.program->blocks[block_idx].linear_idom;
                        op_node.defined_at = block_idx;
                        op_node.value = copy.op;
                  }

                  if (op_node.index == -1u) {
                        merge_set op_set = merge_set{copy.op.getTemp()};
                        return try_merge_merge_set(ctx, copy.def.getTemp(), op_set);
                  }

                  assert(ctx.merge_node_table.count(copy.def.tempId()));
                  if (op_node.index == ctx.merge_node_table[copy.def.tempId()].index)
                        return true;

                  return try_merge_merge_set(ctx, copy.def.getTemp(), ctx.merge_sets[op_node.index]);
            }

            /* node in the location-transfer-graph */
            struct ltg_node {
                  copy* cp;
                  uint32_t read_idx;
                  uint32_t num_uses = 0;
            };

            /* emit the copies in an order that does not
             * create interferences within a merge-set */
            void
            emit_copies_block(Builder& bld, std::map<uint32_t, ltg_node>& ltg, RegType type,
                              std::unordered_map<uint32_t, uint32_t>& ltg_operand_temp_counts)
            {
                  RegisterDemand live_changes;
                  RegisterDemand reg_demand = bld.it->get()->register_demand - get_temp_registers(bld.it->get()) -
                  get_live_changes(bld.it->get());
                  auto it = ltg.begin();
                  while (it != ltg.end()) {
                        // Original copy data from the ltg_node's pointer.
                        // The `cp` pointer points into `ctx.parallelcopies[i]`.
                        copy* original_cp_ptr = it->second.cp;
                        Definition def_to_emit = original_cp_ptr->def;
                        Operand op_to_emit = original_cp_ptr->op; // Make a working copy of the operand for potential modification

                        if (def_to_emit.regClass().type() != type || it->second.num_uses > 0) {
                              ++it;
                              continue;
                        }

                        // Modify kill flag on op_to_emit based on global LTG usage.
                        // This replicates the original std::any_of logic which effectively checks if the operand
                        // is used by any copy within the current ltg (including potentially itself).
                        if (op_to_emit.isTemp() && op_to_emit.isKill()) {
                              auto count_it = ltg_operand_temp_counts.find(op_to_emit.tempId());
                              if (count_it != ltg_operand_temp_counts.end() && count_it->second > 0) {
                                    op_to_emit.setKill(false);
                              }
                        }

                        uint32_t current_read_idx = it->second.read_idx; // Save before 'it' potentially invalidated by erase

                        /* update the location transfer graph for the dependency */
                        if (current_read_idx != -1u) {
                              auto other_iter = ltg.find(current_read_idx);
                              if (other_iter != ltg.end()) {
                                    other_iter->second.num_uses--;
                              }
                        }

                        // Decrement usage count for the operand of the copy being removed.
                        if (original_cp_ptr->op.isTemp()) {
                              auto count_iter = ltg_operand_temp_counts.find(original_cp_ptr->op.tempId());
                              if (count_iter != ltg_operand_temp_counts.end()) {
                                    if (count_iter->second > 0) { // Should always be true if found and part of ltg
                                          count_iter->second--;
                                    }
                                    // Optional: if (count_iter->second == 0) ltg_operand_temp_counts.erase(count_iter);
                              }
                        }

                        it = ltg.erase(it); // Erase and get next valid iterator

                        /* emit the copy */
                        Instruction* instr = bld.copy(def_to_emit, op_to_emit);
                        live_changes += get_live_changes(instr);
                        RegisterDemand temps = get_temp_registers(instr);
                        instr->register_demand = reg_demand + live_changes + temps;

                        // Restart scan from beginning of map to maintain original processing order (smallest write_idx first)
                        if (!ltg.empty()) { // Check if ltg is not empty before resetting iterator
                              it = ltg.begin();
                        } // If ltg became empty, loop condition `it != ltg.end()` will handle termination.
                  }

                  unsigned num = 0;
                  for (auto const& [_, node_val] : ltg) {
                        if (node_val.cp->def.regClass().type() == type) {
                              num++;
                        }
                  }

                  if (num) [[unlikely]] {
                        aco_ptr<Instruction> par_copy_instr{
                              create_instruction(aco_opcode::p_parallelcopy, Format::PSEUDO, num, num)};

                              auto current_ltg_iter = ltg.begin();
                              for (unsigned i = 0; i < num; i++) {
                                    while(current_ltg_iter != ltg.end() && current_ltg_iter->second.cp->def.regClass().type() != type) {
                                          ++current_ltg_iter;
                                    }
                                    if (current_ltg_iter == ltg.end()) {
                                          assert(false && "Should have found enough nodes for parallelcopy");
                                          break;
                                    }

                                    par_copy_instr->definitions[i] = current_ltg_iter->second.cp->def;
                                    par_copy_instr->operands[i] = current_ltg_iter->second.cp->op;

                                    // Decrement usage count for the operand of the copy being moved to parallelcopy
                                    if (current_ltg_iter->second.cp->op.isTemp()) {
                                          auto count_iter = ltg_operand_temp_counts.find(current_ltg_iter->second.cp->op.tempId());
                                          if (count_iter != ltg_operand_temp_counts.end()) {
                                                if (count_iter->second > 0) {
                                                      count_iter->second--;
                                                }
                                          }
                                    }
                                    current_ltg_iter = ltg.erase(current_ltg_iter); // Erase and advance
                              }
                              live_changes += get_live_changes(par_copy_instr.get());
                              RegisterDemand temps = get_temp_registers(par_copy_instr.get());
                              par_copy_instr->register_demand = reg_demand + live_changes + temps;
                              bld.insert(std::move(par_copy_instr));
                  }

                  for (auto instr_it = bld.it; instr_it != bld.instructions->end(); ++instr_it) {
                        instr_it->get()->register_demand += live_changes;
                  }
            }

            /* either emits or coalesces all parallelcopies and
             * renames the phi-operands accordingly. */
            void
            emit_parallelcopies(cssa_ctx& ctx)
            {
                  std::unordered_map<uint32_t, Operand> renames;

                  for (int i = ctx.program->blocks.size() - 1; i >= 0; i--) {
                        if (ctx.parallelcopies[i].empty()) [[likely]] // Assume many blocks might not have parallel copies
                              continue;

                        std::map<uint32_t, ltg_node> ltg;
                        bool has_vgpr_copy = false;
                        bool has_sgpr_copy = false;

                        for (copy& cp : ctx.parallelcopies[i]) {
                              if (try_coalesce_copy(ctx, cp, i)) [[likely]] { // Assume coalescing is often successful
                                    assert(cp.op.isTemp() && cp.op.isKill());
                                    for (copy& other : ctx.parallelcopies[i]) {
                                          if (&other != &cp && other.op.isTemp() && other.op.getTemp() == cp.op.getTemp())
                                                other.op.setKill(false);
                                    }
                                    renames.emplace(cp.def.tempId(), cp.op);
                              } else {
                                    uint32_t read_idx = -1u;
                                    if (cp.op.isTemp()) {
                                          read_idx = ctx.merge_node_table[cp.op.tempId()].index;
                                          cp.op.setKill(cp.op.isKill() && !is_live_out(ctx, cp.op.getTemp(), i));
                                          cp.op.setFirstKill(cp.op.isKill());
                                    }
                                    uint32_t write_idx = ctx.merge_node_table[cp.def.tempId()].index;
                                    assert(write_idx != -1u);
                                    ltg[write_idx] = {&cp, read_idx};

                                    bool is_vgpr = cp.def.regClass().type() == RegType::vgpr;
                                    has_vgpr_copy |= is_vgpr;
                                    has_sgpr_copy |= !is_vgpr;
                              }
                        }

                        for (auto& pair : ltg) {
                              if (pair.second.read_idx == -1u)
                                    continue;
                              auto it_user = ltg.find(pair.second.read_idx);
                              if (it_user != ltg.end())
                                    it_user->second.num_uses++;
                        }

                        std::unordered_map<uint32_t, uint32_t> ltg_operand_temp_counts;
                        if (!ltg.empty()) {
                              for (auto const& [_, node_val] : ltg) {
                                    if (node_val.cp->op.isTemp()) {
                                          ltg_operand_temp_counts[node_val.cp->op.tempId()]++;
                                    }
                              }
                        }

                        Builder bld(ctx.program);
                        Block& block = ctx.program->blocks[i];

                        if (has_vgpr_copy) {
                              auto IsLogicalEnd = [](const aco_ptr<Instruction>& inst) -> bool
                              { return inst->opcode == aco_opcode::p_logical_end; };
                              auto log_end_it =
                              std::find_if(block.instructions.rbegin(), block.instructions.rend(), IsLogicalEnd);
                              bld.reset(&block.instructions, std::prev(log_end_it.base()));
                              emit_copies_block(bld, ltg, RegType::vgpr, ltg_operand_temp_counts);
                        }

                        if (has_sgpr_copy) {
                              bld.reset(&block.instructions, std::prev(block.instructions.end()));
                              emit_copies_block(bld, ltg, RegType::sgpr, ltg_operand_temp_counts);
                        }
                  }

                  RegisterDemand new_demand;
                  for (Block& block : ctx.program->blocks) {
                        for (aco_ptr<Instruction>& phi : block.instructions) {
                              if (phi->opcode != aco_opcode::p_phi && phi->opcode != aco_opcode::p_linear_phi)
                                    break;

                              for (Operand& op : phi->operands) {
                                    if (!op.isTemp())
                                          continue;
                                    auto rename_it = renames.find(op.tempId());
                                    if (rename_it != renames.end()) [[likely]] {
                                          op = rename_it->second;
                                          renames.erase(rename_it);
                                    }
                              }
                        }

                        block.register_demand = block.live_in_demand;
                        for (const aco_ptr<Instruction>& instr : block.instructions)
                              block.register_demand.update(instr->register_demand);
                        new_demand.update(block.register_demand);
                  }

                  update_vgpr_sgpr_demand(ctx.program, new_demand);

                  assert(renames.empty());
            }

      }

      void
      lower_to_cssa(Program* program)
      {
            reindex_ssa(program);
            cssa_ctx ctx = {program};
            collect_parallelcopies(ctx);
            emit_parallelcopies(ctx);

            if (!validate_live_vars(program)) [[unlikely]]
                  abort();
      }
} // namespace aco
