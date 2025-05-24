#include "aco_builder.h"
#include "aco_ir.h"

#include "common/amdgfxregs.h"

#include <algorithm>
#include <cassert>
#include <cstring>
#include <vector>
#include <cmath>
#include <tuple>
#include <initializer_list>
#include <limits>
#include <memory>
#include <functional>
#include <unordered_set>

namespace aco {
      bool should_form_clause(const Instruction* a, const Instruction* b);

      class InstructionPropertyCache;
      class VegaClauseEvaluator;
      class SimplePatternScheduler;
      struct sched_ctx;
      struct MoveState;
      struct DownwardsCursor;
      struct UpwardsCursor;
      struct memory_event_set;
      struct hazard_query;
      struct MemoryPatternAnalyzer;
      struct VegaMemoryScheduler;
}

namespace aco {

      template<class Iter>
      static void move_element_impl(Iter begin, int old_idx, int new_idx)
      {
            assert(old_idx >= 0 && new_idx >= 0);
            assert(old_idx != new_idx);

            if (new_idx > old_idx)
                  std::rotate(begin + old_idx, begin + old_idx + 1, begin + new_idx);
            else
                  std::rotate(begin + new_idx, begin + old_idx,     begin + old_idx + 1);
      }

      template<class Iter>
      static inline void move_element(Iter b, int from, int to)
      {
            if (from == to) return;
            move_element_impl(b, from, to);
      }

      static bool
      get_constant_vmem_offset(const Instruction* instr, int64_t& out_off)
      {
            if (!instr)
                  return false;

            if (instr->operands.size() > 1 && instr->operands[1].isConstant()) {
                  out_off = instr->operands[1].constantValue();
                  return true;
            }

            for (unsigned i = 2; i < instr->operands.size(); ++i) {
                  if (instr->operands[i].isConstant()) {
                        out_off = instr->operands[i].constantValue();
                        return true;
                  }
            }
            return false;
      }

      static unsigned get_vmem_access_size(const Instruction* instr)
      {
            if (!instr) return 0;

            if (!instr->definitions.empty() && instr->definitions[0].isTemp()) {
                  return instr->definitions[0].bytes();
            }
            else if (instr->isVMEM() && instr->format == Format::MUBUF) {
                  if (instr->operands.size() >= 4 && instr->operands[3].isTemp())
                        return instr->operands[3].bytes();
                  else if (instr->operands.size() >= 3 && instr->operands[2].isTemp())
                        return instr->operands[2].bytes();
                  else if (!instr->operands.empty() && instr->operands.back().isTemp())
                        return instr->operands.back().bytes();
            }
            return 0;
      }

      class InstructionPropertyCache {
      private:
            struct CachedProperties {
                  int64_t constant_offset;
                  uint16_t access_size;
                  uint8_t has_constant_offset : 1;
                  uint8_t access_size_valid : 1;
                  uint8_t reserved : 6;
            };

            static constexpr size_t CACHE_SIZE = 512;
            static constexpr size_t CACHE_MASK = CACHE_SIZE - 1;

            struct Entry {
                  const Instruction* key = nullptr;
                  CachedProperties props = {};
            };

            alignas(64) Entry cache[CACHE_SIZE];

            void reset_entry(Entry& e, const Instruction* new_key) {
                  e.key = new_key;
                  e.props = {};
            }

      public:
            InstructionPropertyCache() {
                  std::memset(cache, 0, sizeof(cache));
            }

            bool get_constant_offset(const Instruction* instr, int64_t& offset) {
                  size_t idx = (reinterpret_cast<uintptr_t>(instr) >> 3) & CACHE_MASK;
                  Entry& e = cache[idx];

                  if (e.key == instr && e.props.has_constant_offset) {
                        offset = e.props.constant_offset;
                        return true;
                  }

                  if (e.key != instr) {
                        reset_entry(e, instr);
                  }

                  bool valid = aco::get_constant_vmem_offset(instr, offset);
                  e.props.constant_offset = offset;
                  e.props.has_constant_offset = valid ? 1 : 0;

                  return valid;
            }

            uint16_t get_access_size(const Instruction* instr) {
                  size_t idx = (reinterpret_cast<uintptr_t>(instr) >> 3) & CACHE_MASK;
                  Entry& e = cache[idx];

                  if (e.key == instr && e.props.access_size_valid) {
                        return e.props.access_size;
                  }

                  if (e.key != instr) {
                        reset_entry(e, instr);
                  }

                  uint16_t size = aco::get_vmem_access_size(instr);
                  e.props.access_size = size;
                  e.props.access_size_valid = 1;

                  return size;
            }

            void prefetch(const Instruction* instr) {
                  if (instr) {
                        size_t idx = (reinterpret_cast<uintptr_t>(instr) >> 3) & CACHE_MASK;
                        __builtin_prefetch(&cache[idx], 0, 1);
                  }
            }
      };

      enum MoveResult {
            move_success,
            move_fail_ssa,
            move_fail_rar,
            move_fail_pressure,
      };

      struct MoveState {
            RegisterDemand max_registers;
            Block* block;
            Instruction* current;
            bool improved_rar;
            std::vector<uint8_t> depends_on;
            std::vector<uint8_t> RAR_dependencies;
            std::vector<uint8_t> RAR_dependencies_clause;

            DownwardsCursor downwards_init(int current_idx, bool improved_rar, bool may_form_clauses);
            MoveResult downwards_move(DownwardsCursor&, bool clause);
            void downwards_skip(DownwardsCursor&);
            UpwardsCursor upwards_init(int source_idx, bool improved_rar);
            bool upwards_check_deps(UpwardsCursor&);
            void upwards_update_insert_idx(UpwardsCursor&);
            MoveResult upwards_move(UpwardsCursor&);
            void upwards_skip(UpwardsCursor&);
      };

      struct sched_ctx {
            amd_gfx_level gfx_level;
            int16_t occupancy_factor;
            int16_t last_SMEM_stall;
            int last_SMEM_dep_idx;
            MoveState mv;
            bool schedule_pos_exports = true;
            unsigned schedule_pos_export_div = 1;
            float schedule_aggressiveness = 1.0f;
            bool prefer_clauses = true;
            bool prefer_latency_hiding = false;

            std::unique_ptr<InstructionPropertyCache> prop_cache;
            std::unique_ptr<VegaClauseEvaluator> clause_eval;
            std::unique_ptr<SimplePatternScheduler> pattern_scheduler;

            void init_caches();
      };


      class VegaClauseEvaluator {
      private:
            InstructionPropertyCache& cache;

            static constexpr int VEGA_HBM2_CHANNELS = 8;
            static constexpr int VEGA_CHANNEL_INTERLEAVE = 256;
            static constexpr int VEGA_L1_LINE_SIZE = 64;
            static constexpr int VEGA_L2_SECTOR_SIZE = 64;
            static constexpr int VEGA_BANK_STRIDE = 4;
            static constexpr int VEGA_BANK_COUNT = 32;

            static int get_hbm2_channel(int64_t addr) {
                  return (addr / VEGA_CHANNEL_INTERLEAVE) % VEGA_HBM2_CHANNELS;
            }

            static int get_l1_bank(int64_t addr) {
                  return (addr / VEGA_BANK_STRIDE) % VEGA_BANK_COUNT;
            }

      public:
            VegaClauseEvaluator(InstructionPropertyCache& c) : cache(c) {}

            enum ClauseScore {
                  CLAUSE_BAD = 0,
                  CLAUSE_NEUTRAL = 1,
                  CLAUSE_GOOD = 2,
                  CLAUSE_EXCELLENT = 3
            };

            ClauseScore evaluate_clause(const Instruction* a, const Instruction* b,
                                        const sched_ctx& ctx) {
                  if (!a || !b || a->operands.empty() || b->operands.empty())
                        return CLAUSE_BAD;

                  if (!a->operands[0].isTemp() || !b->operands[0].isTemp() ||
                        a->operands[0].tempId() != b->operands[0].tempId())
                        return CLAUSE_BAD;

                  int64_t offset_a, offset_b;
                  if (!cache.get_constant_offset(a, offset_a) ||
                        !cache.get_constant_offset(b, offset_b))
                        return CLAUSE_NEUTRAL;

                  int64_t diff = offset_b - offset_a;
                  int64_t abs_diff = std::abs(diff);

                  uint16_t size_a = cache.get_access_size(a);
                  if (diff == size_a && size_a > 0) {
                        return CLAUSE_EXCELLENT;
                  }

                  if (abs_diff < VEGA_L1_LINE_SIZE) {
                        int bank_a = get_l1_bank(offset_a);
                        int bank_b = get_l1_bank(offset_b);
                        if (bank_a != bank_b) {
                              return CLAUSE_EXCELLENT;
                        }
                        if (abs_diff >= VEGA_BANK_STRIDE * 2) {
                              return CLAUSE_GOOD;
                        } else if (abs_diff >= VEGA_BANK_STRIDE) {
                              return CLAUSE_NEUTRAL;
                        } else {
                              return CLAUSE_BAD;
                        }
                  }

                  if (abs_diff < 256) {
                        int chan_a = get_hbm2_channel(offset_a);
                        int chan_b = get_hbm2_channel(offset_b);

                        if (chan_a != chan_b) {
                              return CLAUSE_GOOD;
                        }

                        if (abs_diff < 128) {
                              return CLAUSE_NEUTRAL;
                        }
                  }

                  bool same_page = ((offset_a ^ offset_b) & ~0xFFF) == 0;
                  if (same_page && abs_diff < 2048) {
                        if (ctx.occupancy_factor >= 6) {
                              return CLAUSE_GOOD;
                        } else {
                              if (abs_diff < 512) {
                                    return CLAUSE_NEUTRAL;
                              }
                        }
                  }
                  return CLAUSE_BAD;
                                        }

                                        bool should_form_clause(const Instruction* a, const Instruction* b,
                                                                const sched_ctx& ctx) {
                                              ClauseScore score = evaluate_clause(a, b, ctx);
                                              ClauseScore min_score = ctx.prefer_clauses ? CLAUSE_NEUTRAL : CLAUSE_GOOD;

                                              if (ctx.occupancy_factor >= 8 && ctx.prefer_clauses) {
                                                    min_score = std::max(min_score, CLAUSE_GOOD);
                                              }
                                              return score >= min_score;
                                                                }
      };

      class SimplePatternScheduler {
      public:
            struct PatternInfo {
                  enum Type { SEQUENTIAL, STRIDED, RANDOM };
                  Type type;
                  float confidence;
                  int stride;
                  int sample_count;
            };
      private:
            InstructionPropertyCache& cache;
            VegaClauseEvaluator& evaluator;


            PatternInfo detect_pattern_robust(const Block* block, int center, int window) {
                  PatternInfo info = {PatternInfo::RANDOM, 0.0f, 0, 0};
                  std::vector<std::pair<int, int64_t>> offset_pairs;

                  for (int i = std::max(0, center - window/2);
                       i < std::min((int)block->instructions.size(), center + window/2); i++) {
                        auto* instr = block->instructions[i].get();
                        if (!instr || !instr->isVMEM()) continue;

                        int64_t offset;
                        if (cache.get_constant_offset(instr, offset)) {
                              offset_pairs.push_back({i, offset});
                        }
                       }

                       info.sample_count = offset_pairs.size();
                       if (offset_pairs.size() < 3) return info;

                       std::vector<int64_t> strides;
                  for (size_t i = 1; i < offset_pairs.size(); i++) {
                        strides.push_back(offset_pairs[i].second - offset_pairs[i-1].second);
                  }

                  if (strides.empty()) return info;

                  std::sort(strides.begin(), strides.end());
                  int64_t common_stride = strides[0];
                  int max_count = 1, current_count = 1;

                  for (size_t i = 1; i < strides.size(); i++) {
                        if (strides[i] == strides[i-1]) {
                              current_count++;
                        } else {
                              if (current_count > max_count) {
                                    max_count = current_count;
                                    common_stride = strides[i-1];
                              }
                              current_count = 1;
                        }
                  }
                  if (current_count > max_count) {
                        max_count = current_count;
                        common_stride = strides.back();
                  }


                  info.confidence = float(max_count) / float(strides.size());
                  info.stride = common_stride;

                  if (info.confidence > 0.75f) {
                        uint16_t access_size = 0;
                        if (!offset_pairs.empty()) {
                              access_size = cache.get_access_size(block->instructions[offset_pairs[0].first].get());
                        }
                        if (common_stride > 0 && common_stride == access_size && access_size > 0) {
                              info.type = PatternInfo::SEQUENTIAL;
                        } else if (common_stride > 0 && common_stride <= 64) {
                              info.type = PatternInfo::SEQUENTIAL;
                        } else if (common_stride > 0 && common_stride <= 4096) {
                              info.type = PatternInfo::STRIDED;
                        }
                  }
                  return info;
            }

            float calculate_clause_threshold_adjustment(float clause_bias,
                                                        VegaClauseEvaluator::ClauseScore score) {
                  if (score == VegaClauseEvaluator::CLAUSE_BAD ||
                        score == VegaClauseEvaluator::CLAUSE_EXCELLENT)
                        return 0.0f;

                  if (score == VegaClauseEvaluator::CLAUSE_NEUTRAL) {
                        return clause_bias;
                  } else {
                        return clause_bias * 0.5f;
                  }
                                                        }

                                                        void schedule_VMEM_enhanced_with_pattern_bias(
                                                              sched_ctx& ctx, Block* block, Instruction* current, int idx,
                                                              float clause_bias, const PatternInfo& pattern);

      public:
            SimplePatternScheduler(InstructionPropertyCache& c, VegaClauseEvaluator& e)
            : cache(c), evaluator(e) {}

            void schedule_vmem_adaptive(sched_ctx& ctx, Block* block,
                                        Instruction* current, int idx) {
                  PatternInfo pattern_info = detect_pattern_robust(block, idx, 28);
                  float aggressiveness_factor = 1.0f;
                  float dynamic_clause_bias = 0.0f;

                  if (pattern_info.confidence > 0.6f && pattern_info.sample_count >= 5) {
                        switch (pattern_info.type) {
                              case PatternInfo::SEQUENTIAL:
                                    aggressiveness_factor = 1.0f + 0.3f * pattern_info.confidence;
                                    dynamic_clause_bias = 0.4f * pattern_info.confidence;
                                    break;
                              case PatternInfo::STRIDED:
                                    if (pattern_info.stride <= 256) {
                                          aggressiveness_factor = 1.0f + 0.1f * pattern_info.confidence;
                                          dynamic_clause_bias = 0.2f * pattern_info.confidence;
                                    } else if (pattern_info.stride <= 1024) {
                                          aggressiveness_factor = 1.0f + 0.15f * pattern_info.confidence;
                                          dynamic_clause_bias = 0.0f;
                                    } else {
                                          aggressiveness_factor = 1.0f + 0.2f * pattern_info.confidence;
                                          dynamic_clause_bias = -0.2f * pattern_info.confidence;
                                    }
                                    break;
                              case PatternInfo::RANDOM:
                                    aggressiveness_factor = 1.0f + 0.3f * pattern_info.confidence;
                                    dynamic_clause_bias = -0.3f * pattern_info.confidence;
                                    break;
                        }
                  }

                  float saved_aggressiveness = ctx.schedule_aggressiveness;
                  ctx.schedule_aggressiveness *= aggressiveness_factor;
                  ctx.schedule_aggressiveness = std::clamp(ctx.schedule_aggressiveness, 0.5f, saved_aggressiveness * 1.5f);


                  schedule_VMEM_enhanced_with_pattern_bias(ctx, block, current, idx, dynamic_clause_bias, pattern_info);
                  ctx.schedule_aggressiveness = saved_aggressiveness;
                                        }
      };

      void sched_ctx::init_caches() {
            prop_cache = std::make_unique<InstructionPropertyCache>();
            clause_eval = std::make_unique<VegaClauseEvaluator>(*prop_cache);
            pattern_scheduler = std::make_unique<SimplePatternScheduler>(*prop_cache, *clause_eval);
      }


      #define SMEM_WINDOW_SIZE_CTX    (256 - ctx.occupancy_factor * 16)
      #define VMEM_WINDOW_SIZE_CTX    (1024 - ctx.occupancy_factor * 64)
      #define POS_EXP_WINDOW_SIZE_CTX 512
      #define SMEM_MAX_MOVES_CTX      (128 - ctx.occupancy_factor * 8)
      #define VMEM_MAX_MOVES_CTX      (256 - ctx.occupancy_factor * 16)
      #define LDSDIR_MAX_MOVES_CTX    10
      #define LDS_MAX_MOVES_CTX       32
      #define VMEM_CLAUSE_MAX_GRAB_DIST_CTX       (ctx.occupancy_factor * 2)
      #define VMEM_STORE_CLAUSE_MAX_GRAB_DIST_CTX (ctx.occupancy_factor * 4)
      #define POS_EXP_MAX_MOVES_CTX         512


      struct memory_event_set {
            bool     has_control_barrier = false;
            unsigned bar_acquire         = 0;
            unsigned bar_release         = 0;
            unsigned bar_classes         = 0;
            unsigned access_acquire      = 0;
            unsigned access_release      = 0;
            unsigned access_relaxed      = 0;
            unsigned access_atomic       = 0;
      };

      struct hazard_query {
            amd_gfx_level   gfx_level       = {};
            bool            contains_spill  = false;
            bool            contains_sendmsg= false;
            bool            uses_exec       = false;
            bool            writes_exec     = false;
            memory_event_set mem_events;
            unsigned        aliasing_storage      = 0;
            unsigned        aliasing_storage_smem = 0;
      };

      enum HazardResult {
            hazard_success,
            hazard_fail_reorder_vmem_smem,
            hazard_fail_reorder_ds,
            hazard_fail_reorder_sendmsg,
            hazard_fail_spill,
            hazard_fail_export,
            hazard_fail_barrier,
            hazard_fail_exec,
            hazard_fail_unreorderable,
      };


      struct DownwardsCursor {
            int source_idx;
            int insert_idx_clause;
            int insert_idx;
            RegisterDemand clause_demand;
            RegisterDemand total_demand;
            RegisterDemand insert_demand_clause;
            RegisterDemand insert_demand;

            DownwardsCursor(int current_idx, RegisterDemand initial_clause_demand)
            : source_idx(current_idx - 1), insert_idx_clause(current_idx), insert_idx(current_idx + 1),
            clause_demand(initial_clause_demand)
            { total_demand = RegisterDemand(); insert_demand_clause = RegisterDemand(); insert_demand = RegisterDemand(); }

            inline void verify_invariants(const Block*) {}
      };

      struct UpwardsCursor {
            int source_idx;
            int insert_idx;
            RegisterDemand total_demand;
            RegisterDemand insert_demand;

            UpwardsCursor(int source_idx_) : source_idx(source_idx_)
            {
                  insert_idx = -1;
                  total_demand = RegisterDemand(); insert_demand = RegisterDemand();
            }

            bool has_insert_idx() const { return insert_idx != -1; }
            inline void verify_invariants(const Block*) {}
      };

      struct VegaMemoryScheduler {
            static constexpr int VEGA_VMEM_ISSUE_CYCLES = 4;
            static constexpr int VEGA_L1_LATENCY = 20;
            static constexpr int VEGA_L2_LATENCY = 130;
            static constexpr int VEGA_HBM2_LATENCY = 240;
            static constexpr int VEGA_MAX_WAVES_PER_SIMD = 10;

            static int calculate_vmem_window_enhanced(const sched_ctx& ctx, int base_window,
                                                      double local_mem_ratio) {
                  int effective_waves = std::min(static_cast<int>(ctx.occupancy_factor), VEGA_MAX_WAVES_PER_SIMD);
                  if (effective_waves <= 0) effective_waves = 1;

                  int l1_hiding_slots = (VEGA_L1_LATENCY + VEGA_VMEM_ISSUE_CYCLES -1) / VEGA_VMEM_ISSUE_CYCLES;
                  int l2_hiding_slots = (VEGA_L2_LATENCY + VEGA_VMEM_ISSUE_CYCLES -1) / VEGA_VMEM_ISSUE_CYCLES;
                  int hbm2_hiding_slots = (VEGA_HBM2_LATENCY + VEGA_VMEM_ISSUE_CYCLES -1) / VEGA_VMEM_ISSUE_CYCLES;

                  float wave_scaling = 1.0f + (float(VEGA_MAX_WAVES_PER_SIMD - effective_waves) /
                  float(VEGA_MAX_WAVES_PER_SIMD)) * 1.25f;

                  float mem_scaling = 1.0f + (local_mem_ratio * 0.6f);

                  int l1_window = int(l1_hiding_slots * wave_scaling * mem_scaling);
                  int l2_window = int(l2_hiding_slots * wave_scaling * mem_scaling);
                  int hbm2_window = int(hbm2_hiding_slots * wave_scaling);

                  int calculated_window = std::max({base_window, l1_window, l2_window, hbm2_window});

                  int max_window_multiplier = (effective_waves < 4) ? 4 :
                  (effective_waves < 6) ? 3 : 2;
                  int max_window = base_window * max_window_multiplier;

                  if (local_mem_ratio < 0.3) {
                        max_window = std::min(max_window, int(base_window * 2.0));
                  }

                  return std::clamp(calculated_window, base_window, max_window);
                                                      }
      };


      static double calculate_local_memory_ratio_simple(const Block* blk, int center_idx) {
            if (!blk || center_idx < 0 || center_idx >= (int)blk->instructions.size())
                  return 0.0;

            constexpr int WINDOW_SIZE = 32;
            int start = std::max(0, center_idx - WINDOW_SIZE/2);
            int end = std::min((int)blk->instructions.size(), center_idx + WINDOW_SIZE/2);

            int mem_count = 0, total_count = 0;

            for (int i = start; i < end; i++) {
                  const auto& instr = blk->instructions[i];
                  if (instr) {
                        total_count++;
                        if (instr->isVMEM() || instr->isFlatLike() || instr->isSMEM()) {
                              mem_count++;
                        }
                  }
            }

            return total_count > 0 ? double(mem_count) / double(total_count) : 0.0;
      }

      static bool check_l2_page_affinity(const Instruction* i1, const Instruction* i2)
      {
            if (!i1 || !i2) return false;

            int64_t off1, off2;
            if (!aco::get_constant_vmem_offset(i1, off1) || !aco::get_constant_vmem_offset(i2, off2))
                  return false;

            return ((off1 ^ off2) & ~int64_t(4095)) == 0;
      }

      static float calculate_vega_ilp_boost(const Block* blk, double mem_ratio)
      {
            float base_boost = getenv("ACO_SCHED_VEGA_ILP_BOOST")
            ? std::clamp(std::atoi(getenv("ACO_SCHED_VEGA_ILP_BOOST")), 0, 200) / 100.f
            : 1.0f;

            if (mem_ratio > 0.75) {
                  base_boost *= 1.15f;
            } else if (mem_ratio > 0.60) {
                  base_boost *= 1.08f;
            } else if (mem_ratio > 0.40) {
                  base_boost *= 1.03f;
            } else if (mem_ratio < 0.20) {
                  base_boost *= 0.92f;
            }

            if (blk) {
                  int vmem_count = 0, total_count = 0;
                  for (const auto& instr : blk->instructions) {
                        if (instr) {
                              total_count++;
                              if (instr->isVMEM() || instr->isFlatLike()) {
                                    vmem_count++;
                              }
                        }
                  }

                  if (total_count > 0) {
                        float vmem_density = float(vmem_count) / float(total_count);
                        if (vmem_density > 0.5f) {
                              base_boost *= 1.05f;
                        }
                  }
            }

            return std::clamp(base_boost, 0.7f, 1.4f);
      }

      static void adjust_vega_scheduling_context(sched_ctx& ctx, const Block* blk, double mem_ratio_blk)
      {
            if (ctx.gfx_level != GFX9) return;

            float occupancy_factor_scaled = float(ctx.occupancy_factor) / 10.0f;

            if (occupancy_factor_scaled > 0.7f && mem_ratio_blk > 0.6f) {
                  ctx.schedule_aggressiveness *= 1.05f;
                  ctx.prefer_clauses = true;
                  ctx.prefer_latency_hiding = true;
            }
            else if (occupancy_factor_scaled < 0.4f && mem_ratio_blk < 0.3f) {
                  ctx.schedule_aggressiveness *= 0.9f;
                  ctx.prefer_clauses = false;
            }
            else if (occupancy_factor_scaled >= 0.4f && occupancy_factor_scaled <= 0.7f && mem_ratio_blk > 0.8f) {
                  ctx.prefer_clauses = true;
                  ctx.schedule_aggressiveness *= 1.05f;
            }

            if (blk) {
                  int vmem_count = 0, total_count = 0;
                  for (const auto& instr : blk->instructions) {
                        if (instr) {
                              total_count++;
                              if (instr->isVMEM() || instr->isFlatLike()) {
                                    vmem_count++;
                              }
                        }
                  }

                  if (total_count > 0) {
                        float vmem_density = float(vmem_count) / float(total_count);
                        if (vmem_density > 0.6f) {
                              ctx.schedule_aggressiveness *= 1.04f;
                              ctx.prefer_clauses = true;
                        }
                  }
            }
            ctx.schedule_aggressiveness = std::clamp(ctx.schedule_aggressiveness, 0.5f, 1.6f);
      }

      struct MemoryPatternAnalyzer {
            enum AccessPattern {
                  PATTERN_SEQUENTIAL,
                  PATTERN_STRIDED,
                  PATTERN_RANDOM,
                  PATTERN_UNKNOWN
            };

            struct PatternInfo {
                  AccessPattern pattern;
                  int stride;
                  int access_size;
            };

            static PatternInfo analyze_vmem_pattern(const Block* block, int start_idx, int window_depth) {
                  std::vector<std::tuple<unsigned, int64_t, int>> accesses;

                  for (int i = start_idx; i < std::min(static_cast<int>(block->instructions.size()), start_idx + window_depth); i++) {
                        Instruction* instr = block->instructions[i].get();
                        if (instr && instr->isVMEM() && instr->operands.size() > 1 && instr->operands[0].isTemp()) {
                              if (instr->operands[1].isConstant()) {
                                    unsigned desc_id = instr->operands[0].tempId();
                                    int64_t offset = instr->operands[1].constantValue();
                                    int size = 0;
                                    if (!instr->definitions.empty() && instr->definitions[0].isTemp()) {
                                          size = instr->definitions[0].bytes();
                                    } else if (instr->isVMEM() && instr->format == Format::MUBUF) {
                                          if (instr->operands.size() >= 4 && instr->operands[3].isTemp()){
                                                size = instr->operands[3].bytes();
                                          } else if (instr->operands.size() >= 3 && instr->operands[2].isTemp()){
                                                size = instr->operands[2].bytes();
                                          } else if (!instr->operands.empty() && instr->operands.back().isTemp()) {
                                                size = instr->operands.back().bytes();
                                          }
                                    }

                                    if (size > 0)
                                          accesses.push_back({desc_id, offset, size});
                              }
                        }
                  }

                  if (accesses.size() < 2)
                        return {PATTERN_UNKNOWN, 0, 0};

                  bool is_sequential = true;
                  bool is_strided = true;
                  int common_stride = -1;
                  int first_access_size = std::get<2>(accesses[0]);

                  for (size_t i = 1; i < accesses.size(); i++) {
                        if (std::get<0>(accesses[i]) != std::get<0>(accesses[i-1])) {
                              is_sequential = false;
                              is_strided = false;
                              break;
                        }

                        int64_t prev_op_offset = std::get<1>(accesses[i-1]);
                        int prev_op_size = std::get<2>(accesses[i-1]);
                        int64_t prev_op_end = prev_op_offset + prev_op_size;
                        int64_t current_op_start = std::get<1>(accesses[i]);

                        int64_t gap = current_op_start - prev_op_end;

                        if (gap != 0) {
                              is_sequential = false;
                        }

                        if (is_strided) {
                              if (common_stride == -1) {
                                    common_stride = gap;
                              } else if (gap != common_stride) {
                                    is_strided = false;
                              }
                        }
                  }

                  if (is_sequential)
                        return {PATTERN_SEQUENTIAL, 0, first_access_size};
                  else if (is_strided && common_stride >= 0 && common_stride <= 256)
                        return {PATTERN_STRIDED, common_stride, first_access_size};
                  else
                        return {PATTERN_RANDOM, 0, 0};
            }
      };

      void
      init_hazard_query(const sched_ctx& ctx, hazard_query* query)
      {
            if (!query) return;
            query->gfx_level            = ctx.gfx_level;
            query->contains_spill       = false;
            query->contains_sendmsg     = false;
            query->uses_exec            = false;
            query->writes_exec          = false;
            std::memset(&query->mem_events, 0, sizeof(query->mem_events));
            query->aliasing_storage      = 0;
            query->aliasing_storage_smem = 0;
      }

      bool is_done_sendmsg(amd_gfx_level gfx_level, const Instruction* instr)
      {
            if (!instr) return false;
            if (gfx_level <= GFX10_3 && instr->opcode == aco_opcode::s_sendmsg)
                  return (instr->salu().imm & sendmsg_id_mask) == sendmsg_gs_done;
            return false;
      }

      bool is_pos_prim_export(amd_gfx_level gfx_level, const Instruction* instr)
      {
            if (!instr) return false;
            return instr->opcode == aco_opcode::exp && instr->exp().dest >= V_008DFC_SQ_EXP_POS &&
            instr->exp().dest <= V_008DFC_SQ_EXP_PRIM && gfx_level >= GFX10;
      }

      memory_sync_info get_sync_info_with_hack(const Instruction* instr)
      {
            if (!instr) return memory_sync_info();
            memory_sync_info sync = get_sync_info(instr);
            if (instr->isSMEM() && !instr->operands.empty() && instr->operands[0].isTemp() && instr->operands[0].bytes() == 16) {
                  sync.storage = (storage_class)(sync.storage | storage_buffer);
                  sync.semantics =
                  (memory_semantics)((sync.semantics | semantic_private) & ~semantic_can_reorder);
            }
            return sync;
      }


      void
      add_memory_event(amd_gfx_level gfx_level, memory_event_set* set, Instruction* instr,
                       memory_sync_info* sync)
      {
            if (!instr || !sync || !set) return;
            set->has_control_barrier |= is_done_sendmsg(gfx_level, instr);
            set->has_control_barrier |= is_pos_prim_export(gfx_level, instr);
            if (instr->opcode == aco_opcode::p_barrier) {
                  Pseudo_barrier_instruction& bar = instr->barrier();
                  if (bar.sync.semantics & semantic_acquire)
                        set->bar_acquire |= bar.sync.storage;
                  if (bar.sync.semantics & semantic_release)
                        set->bar_release |= bar.sync.storage;
                  set->bar_classes |= bar.sync.storage;

                  set->has_control_barrier |= bar.exec_scope > scope_invocation;
            }

            if (!sync->storage)
                  return;

            if (sync->semantics & semantic_acquire)
                  set->access_acquire |= sync->storage;
            if (sync->semantics & semantic_release)
                  set->access_release |= sync->storage;

            if (!(sync->semantics & semantic_private)) {
                  if (sync->semantics & semantic_atomic)
                        set->access_atomic |= sync->storage;
                  else
                        set->access_relaxed |= sync->storage;
            }
      }

      void
      add_to_hazard_query(hazard_query* query, Instruction* instr)
      {
            if (!query || !instr) return;
            if (instr->opcode == aco_opcode::p_spill || instr->opcode == aco_opcode::p_reload)
                  query->contains_spill = true;
            query->contains_sendmsg |= instr->opcode == aco_opcode::s_sendmsg;
            query->uses_exec |= needs_exec_mask(instr);
            for (const Definition& def : instr->definitions) {
                  if (def.isFixed() && def.physReg() == exec)
                        query->writes_exec = true;
            }

            memory_sync_info sync = get_sync_info_with_hack(instr);
            add_memory_event(query->gfx_level, &query->mem_events, instr, &sync);

            if (!(sync.semantics & semantic_can_reorder)) {
                  unsigned storage = sync.storage;
                  if (storage & (storage_buffer | storage_image))
                        storage |= storage_buffer | storage_image;
                  if (instr->isSMEM())
                        query->aliasing_storage_smem |= storage;
                  else
                        query->aliasing_storage |= storage;
            }
      }

      HazardResult
      perform_hazard_query(hazard_query* query, Instruction* instr, bool upwards)
      {
            if(!query || !instr) return hazard_fail_unreorderable;

            if (!upwards && instr->opcode == aco_opcode::p_exit_early_if_not)
                  return hazard_fail_unreorderable;

            if (upwards) {
                  if (instr->opcode == aco_opcode::p_pops_gfx9_add_exiting_wave_id ||
                        is_wait_export_ready(query->gfx_level, instr)) {
                        return hazard_fail_unreorderable;
                        }
            } else {
                  if (instr->opcode == aco_opcode::p_pops_gfx9_ordered_section_done) {
                        return hazard_fail_unreorderable;
                  }
            }

            if (query->uses_exec || query->writes_exec) {
                  for (const Definition& def : instr->definitions) {
                        if (def.isFixed() && def.physReg() == exec)
                              return hazard_fail_exec;
                  }
            }
            if (query->writes_exec && needs_exec_mask(instr))
                  return hazard_fail_exec;

            if (instr->isEXP() || instr->opcode == aco_opcode::p_dual_src_export_gfx11)
                  return hazard_fail_export;

            if (instr->opcode == aco_opcode::s_memtime || instr->opcode == aco_opcode::s_memrealtime ||
                  instr->opcode == aco_opcode::s_setprio || instr->opcode == aco_opcode::s_getreg_b32 ||
                  instr->opcode == aco_opcode::p_shader_cycles_hi_lo_hi ||
                  instr->opcode == aco_opcode::p_init_scratch ||
                  instr->opcode == aco_opcode::p_jump_to_epilog ||
                  instr->opcode == aco_opcode::s_sendmsg_rtn_b32 ||
                  instr->opcode == aco_opcode::s_sendmsg_rtn_b64 ||
                  instr->opcode == aco_opcode::p_end_with_regs || instr->opcode == aco_opcode::s_nop ||
                  instr->opcode == aco_opcode::s_sleep || instr->opcode == aco_opcode::s_trap)
                  return hazard_fail_unreorderable;

            memory_event_set instr_set_local;
            memset(&instr_set_local, 0, sizeof(instr_set_local));
            memory_sync_info sync = get_sync_info_with_hack(instr);
            add_memory_event(query->gfx_level, &instr_set_local, instr, &sync);

            memory_event_set* first = &instr_set_local;
            memory_event_set* second = &query->mem_events;
            if (upwards)
                  std::swap(first, second);

            if ((first->has_control_barrier || first->access_atomic) && second->bar_acquire)
                  return hazard_fail_barrier;
            if (((first->access_acquire || first->bar_acquire) && second->bar_classes) ||
                  ((first->access_acquire | first->bar_acquire) &
                  (second->access_relaxed | second->access_atomic)))
                  return hazard_fail_barrier;

            if (first->bar_release && (second->has_control_barrier || second->access_atomic))
                  return hazard_fail_barrier;
            if ((first->bar_classes && (second->bar_release || second->access_release)) ||
                  ((first->access_relaxed | first->access_atomic) &
                  (second->bar_release | second->access_release)))
                  return hazard_fail_barrier;

            if (first->bar_classes && second->bar_classes)
                  return hazard_fail_barrier;

            unsigned control_classes =
            storage_buffer | storage_image | storage_shared | storage_task_payload;
            if (first->has_control_barrier &&
                  ((second->access_atomic | second->access_relaxed) & control_classes))
                  return hazard_fail_barrier;

            unsigned current_aliasing_storage =
            instr->isSMEM() ? query->aliasing_storage_smem : query->aliasing_storage;
            if ((sync.storage & current_aliasing_storage) && !(sync.semantics & semantic_can_reorder)) {
                  unsigned intersect = sync.storage & current_aliasing_storage;
                  if (intersect & storage_shared)
                        return hazard_fail_reorder_ds;
                  return hazard_fail_reorder_vmem_smem;
            }

            if ((instr->opcode == aco_opcode::p_spill || instr->opcode == aco_opcode::p_reload) &&
                  query->contains_spill)
                  return hazard_fail_spill;

            if (instr->opcode == aco_opcode::s_sendmsg && query->contains_sendmsg)
                  return hazard_fail_reorder_sendmsg;

            return hazard_success;
      }

      DownwardsCursor
      MoveState::downwards_init(int current_idx, bool improved_rar_, bool may_form_clauses)
      {
            improved_rar = improved_rar_;

            std::fill(depends_on.begin(), depends_on.end(), false);
            if (improved_rar) {
                  std::fill(RAR_dependencies.begin(), RAR_dependencies.end(), false);
                  if (may_form_clauses)
                        std::fill(RAR_dependencies_clause.begin(), RAR_dependencies_clause.end(), false);
            }

            if (current && !current->operands.empty()){
                  for (const Operand& op : current->operands) {
                        if (op.isTemp()) {
                              depends_on[op.tempId()] = true;
                              if (improved_rar && op.isFirstKill())
                                    RAR_dependencies[op.tempId()] = true;
                        }
                  }
            }

            assert(block && current_idx >= 0 && current_idx < (int)block->instructions.size());
            assert(block->instructions[current_idx]);
            DownwardsCursor cursor(current_idx, block->instructions[current_idx]->register_demand);

            assert(cursor.insert_idx -1 >=0 && cursor.insert_idx -1 < (int)block->instructions.size());
            assert(block->instructions[cursor.insert_idx - 1]);
            RegisterDemand temp = get_temp_registers(block->instructions[cursor.insert_idx - 1].get());
            cursor.insert_demand = block->instructions[cursor.insert_idx - 1]->register_demand - temp;

            assert(cursor.insert_idx_clause -1 >=0 && cursor.insert_idx_clause -1 < (int)block->instructions.size());
            assert(block->instructions[cursor.insert_idx_clause - 1]);
            temp = get_temp_registers(block->instructions[cursor.insert_idx_clause - 1].get());
            cursor.insert_demand_clause =
            block->instructions[cursor.insert_idx_clause - 1]->register_demand - temp;

            cursor.verify_invariants(block);
            return cursor;
      }

      MoveResult
      MoveState::downwards_move(DownwardsCursor& cursor, bool add_to_clause)
      {
            if (cursor.source_idx < 0 || cursor.source_idx >= (int)block->instructions.size()) return move_fail_ssa;
            aco_ptr<Instruction>& instr = block->instructions[cursor.source_idx];
            if (!instr) return move_fail_ssa;

            for (const Definition& def : instr->definitions)
                  if (def.isTemp() && depends_on[def.tempId()])
                        return move_fail_ssa;

            auto& RAR_deps =
            improved_rar ? (add_to_clause ? RAR_dependencies_clause : RAR_dependencies) : depends_on;
            for (const Operand& op : instr->operands) {
                  if (op.isTemp() && RAR_deps[op.tempId()]) {
                        return move_fail_rar;
                  }
            }

            if (add_to_clause) {
                  for (const Operand& op : instr->operands) {
                        if (op.isTemp()) {
                              depends_on[op.tempId()] = true;
                              if (op.isFirstKill())
                                    RAR_dependencies[op.tempId()] = true;
                        }
                  }
            }

            const int dest_insert_idx = add_to_clause ? cursor.insert_idx_clause : cursor.insert_idx;
            RegisterDemand register_pressure = cursor.total_demand;
            if (!add_to_clause) {
                  register_pressure.update(cursor.clause_demand);
            }

            const RegisterDemand candidate_diff = get_live_changes(instr.get());
            if (RegisterDemand(register_pressure - candidate_diff).exceeds(max_registers))
                  return move_fail_pressure;

            const RegisterDemand temp_regs = get_temp_registers(instr.get());
            const RegisterDemand current_insert_demand =
            add_to_clause ? cursor.insert_demand_clause : cursor.insert_demand;
            const RegisterDemand new_demand = current_insert_demand + temp_regs;
            if (new_demand.exceeds(max_registers))
                  return move_fail_pressure;

            aco::move_element(block->instructions.begin(), cursor.source_idx, dest_insert_idx);

            for (int i = cursor.source_idx; i < dest_insert_idx - 1; i++) {
                  if (i >=0 && i < (int)block->instructions.size() && block->instructions[i])
                        block->instructions[i]->register_demand -= candidate_diff;
            }
            if (dest_insert_idx -1 >=0 && dest_insert_idx -1 < (int)block->instructions.size() && block->instructions[dest_insert_idx-1])
                  block->instructions[dest_insert_idx - 1]->register_demand = new_demand;

            cursor.insert_idx_clause--;
            if (cursor.source_idx != cursor.insert_idx_clause) {
                  cursor.total_demand -= candidate_diff;
            } else {
                  assert(cursor.total_demand == RegisterDemand{});
            }
            if (add_to_clause) {
                  cursor.clause_demand.update(new_demand);
            } else {
                  cursor.clause_demand -= candidate_diff;
                  cursor.insert_demand -= candidate_diff;
                  cursor.insert_idx--;
            }
            cursor.insert_demand_clause -= candidate_diff;

            cursor.source_idx--;
            if (cursor.source_idx >= -1)
                  cursor.verify_invariants(block);
            return move_success;
      }

      void
      MoveState::downwards_skip(DownwardsCursor& cursor)
      {
            if (cursor.source_idx < 0 || cursor.source_idx >= (int)block->instructions.size()) {
                  cursor.source_idx--;
                  return;
            }
            aco_ptr<Instruction>& instr = block->instructions[cursor.source_idx];
            if (!instr) { cursor.source_idx--; return; }


            for (const Operand& op : instr->operands) {
                  if (op.isTemp()) {
                        depends_on[op.tempId()] = true;
                        if (improved_rar && op.isFirstKill()) {
                              RAR_dependencies[op.tempId()] = true;
                              RAR_dependencies_clause[op.tempId()] = true;
                        }
                  }
            }
            cursor.total_demand.update(instr->register_demand);
            cursor.source_idx--;
            if (cursor.source_idx >= -1)
                  cursor.verify_invariants(block);
      }

      UpwardsCursor
      MoveState::upwards_init(int source_idx, bool improved_rar_)
      {
            improved_rar = improved_rar_;

            std::fill(depends_on.begin(), depends_on.end(), false);
            std::fill(RAR_dependencies.begin(), RAR_dependencies.end(), false);

            if(current){
                  for (const Definition& def : current->definitions) {
                        if (def.isTemp())
                              depends_on[def.tempId()] = true;
                  }
            }

            return UpwardsCursor(source_idx);
      }

      bool
      MoveState::upwards_check_deps(UpwardsCursor& cursor)
      {
            if (cursor.source_idx < 0 || cursor.source_idx >= (int)block->instructions.size()) return false;
            aco_ptr<Instruction>& instr = block->instructions[cursor.source_idx];
            if (!instr) return false;

            for (const Operand& op : instr->operands) {
                  if (op.isTemp() && depends_on[op.tempId()])
                        return false;
            }
            return true;
      }

      void
      MoveState::upwards_update_insert_idx(UpwardsCursor& cursor)
      {
            if (cursor.source_idx < 0 || cursor.source_idx >= (int)block->instructions.size()) return;
            cursor.insert_idx = cursor.source_idx;
            if (!block->instructions[cursor.insert_idx]) return;
            cursor.total_demand = block->instructions[cursor.insert_idx]->register_demand;

            if (cursor.insert_idx -1 < 0 || cursor.insert_idx -1 >= (int)block->instructions.size() || !block->instructions[cursor.insert_idx-1]) return;
            const RegisterDemand temp = get_temp_registers(block->instructions[cursor.insert_idx - 1].get());
            cursor.insert_demand = block->instructions[cursor.insert_idx - 1]->register_demand - temp;
      }

      MoveResult
      MoveState::upwards_move(UpwardsCursor& cursor)
      {
            assert(cursor.has_insert_idx());
            if (cursor.source_idx < 0 || cursor.source_idx >= (int)block->instructions.size() ||
                  cursor.insert_idx < 0 || cursor.insert_idx >= (int)block->instructions.size()) return move_fail_ssa;

            aco_ptr<Instruction>& instr = block->instructions[cursor.source_idx];
            if(!instr) return move_fail_ssa;

            for (const Operand& op : instr->operands) {
                  if (op.isTemp() && depends_on[op.tempId()])
                        return move_fail_ssa;
            }

            for (const Operand& op : instr->operands) {
                  if (op.isTemp() && (!improved_rar || op.isFirstKill()) && RAR_dependencies[op.tempId()])
                        return move_fail_rar;
            }

            const RegisterDemand candidate_diff = get_live_changes(instr.get());
            const RegisterDemand temp_regs = get_temp_registers(instr.get());
            if (RegisterDemand(cursor.total_demand + candidate_diff).exceeds(max_registers))
                  return move_fail_pressure;
            const RegisterDemand new_demand = cursor.insert_demand + candidate_diff + temp_regs;
            if (new_demand.exceeds(max_registers))
                  return move_fail_pressure;

            aco::move_element(block->instructions.begin(), cursor.source_idx, cursor.insert_idx);

            if (block->instructions[cursor.insert_idx])
                  block->instructions[cursor.insert_idx]->register_demand = new_demand;

            for (int i = cursor.insert_idx + 1; i <= cursor.source_idx; i++) {
                  if (i >=0 && i < (int)block->instructions.size() && block->instructions[i])
                        block->instructions[i]->register_demand += candidate_diff;
            }
            cursor.total_demand += candidate_diff;
            cursor.insert_demand += candidate_diff;

            cursor.insert_idx++;
            cursor.source_idx++;

            cursor.verify_invariants(block);

            return move_success;
      }

      void
      MoveState::upwards_skip(UpwardsCursor& cursor)
      {
            if (cursor.source_idx < 0 || cursor.source_idx >= (int)block->instructions.size()) {
                  cursor.source_idx++;
                  return;
            }

            if (cursor.has_insert_idx()) {
                  aco_ptr<Instruction>& instr = block->instructions[cursor.source_idx];
                  if (!instr) { cursor.source_idx++; return; }

                  for (const Definition& def : instr->definitions) {
                        if (def.isTemp())
                              depends_on[def.tempId()] = true;
                  }
                  for (const Operand& op : instr->operands) {
                        if (op.isTemp())
                              RAR_dependencies[op.tempId()] = true;
                  }
                  cursor.total_demand.update(instr->register_demand);
            }

            cursor.source_idx++;
            cursor.verify_invariants(block);
      }


      unsigned
      get_likely_cost(Instruction* instr)
      {
            if (!instr) return 1;
            if (instr->opcode == aco_opcode::p_split_vector ||
                  instr->opcode == aco_opcode::p_extract_vector) {
                  unsigned cost = 0;
            for (Definition def : instr->definitions) {
                  if (instr->operands.size() > 0 && instr->operands[0].isKill() &&
                        def.regClass().type() == instr->operands[0].regClass().type())
                        continue;
                  cost += def.size();
            }
            return cost;
                  } else if (instr->opcode == aco_opcode::p_create_vector) {
                        unsigned cost = 0;
                        for (Operand op : instr->operands) {
                              if (instr->definitions.size() > 0 &&
                                    op.isTemp() && op.isFirstKill() &&
                                    op.regClass().type() == instr->definitions[0].regClass().type())
                                    continue;
                              cost += op.size();
                        }
                        return cost;
                  } else {
                        return 1;
                  }
      }

      bool should_form_clause_vega_enhanced(const Instruction* a, const Instruction* b,
                                            amd_gfx_level gfx_level, const sched_ctx& ctx)
      {
            if (!ctx.clause_eval) {
                  return should_form_clause(a, b);
            }
            if (gfx_level != GFX9) {
                  return should_form_clause(a,b);
            }
            return ctx.clause_eval->should_form_clause(a, b, ctx);
      }


      void
      schedule_SMEM(sched_ctx& ctx, Block* block, Instruction* current, int idx)
      {
            assert(idx != 0 && current && block);
            int base_window_size = SMEM_WINDOW_SIZE_CTX;
            int base_max_moves = SMEM_MAX_MOVES_CTX;
            int16_t k = 0;
            const int smem_base_latency_hiding_target = (ctx.gfx_level == GFX9 ? 9 : 10);


            if (ctx.gfx_level == GFX9) {
                  bool is_descriptor_load = false;
                  if (current->opcode == aco_opcode::s_load_dwordx4 ||
                        current->opcode == aco_opcode::s_buffer_load_dwordx4) {
                        is_descriptor_load = true;
                        }

                        if (is_descriptor_load) {
                              base_window_size = base_window_size * 3 / 2;
                              base_max_moves = base_max_moves * 3 / 2;
                        }

                        int recent_scalar_loads = 0;
                  int check_distance = std::min(32, idx);
                  if (idx >= check_distance) {
                        for (int i = idx - check_distance; i < idx; i++) {
                              if (i >= 0 && i < (int)block->instructions.size() && block->instructions[i] && block->instructions[i]->isSMEM()) {
                                    recent_scalar_loads++;
                              }
                        }
                  }

                  if (check_distance > 0 && recent_scalar_loads > check_distance / 3) {
                        base_window_size = base_window_size * 3 / 4;
                        base_max_moves = base_max_moves * 3 / 4;
                  }
            }

            int window_size = static_cast<int>(base_window_size * ctx.schedule_aggressiveness);
            int max_moves = static_cast<int>(base_max_moves * ctx.schedule_aggressiveness);

            if (current->opcode == aco_opcode::s_memtime || current->opcode == aco_opcode::s_memrealtime ||
                  current->opcode == aco_opcode::s_sendmsg_rtn_b32 ||
                  current->opcode == aco_opcode::s_sendmsg_rtn_b64)
                  return;

            hazard_query hq_down;
            init_hazard_query(ctx, &hq_down);
            add_to_hazard_query(&hq_down, current);

            DownwardsCursor cursor = ctx.mv.downwards_init(idx, false, false);

            for ( ; k < max_moves && cursor.source_idx >= 0 && cursor.source_idx > (int)idx - window_size;
            ) {
                  assert(cursor.source_idx >= 0 && cursor.source_idx < (int)block->instructions.size());
                  aco_ptr<Instruction>& candidate = block->instructions[cursor.source_idx];
                  if (!candidate) { ctx.mv.downwards_skip(cursor); continue;}


                  bool can_stall_prev_smem =
                  idx <= ctx.last_SMEM_dep_idx && cursor.source_idx < ctx.last_SMEM_dep_idx;
                  if (can_stall_prev_smem && ctx.last_SMEM_stall >= 0)
                        break;

                  if (candidate->opcode == aco_opcode::p_logical_start)
                        break;

                  bool current_is_16byte_smem = (current->opcode == aco_opcode::s_load_dwordx4 || current->opcode == aco_opcode::s_buffer_load_dwordx4);

                  if ((candidate->isVMEM() || candidate->isFlatLike()) &&
                        (((cursor.insert_idx_clause - 1) - cursor.source_idx) > (ctx.occupancy_factor * 6))
                  ) {
                        if (current_is_16byte_smem) break;
                  }


                  bool can_move_down = true;
                  HazardResult haz = perform_hazard_query(&hq_down, candidate.get(), false);
                  if (haz == hazard_fail_reorder_ds || haz == hazard_fail_spill ||
                        haz == hazard_fail_reorder_sendmsg || haz == hazard_fail_barrier ||
                        haz == hazard_fail_export)
                        can_move_down = false;
                  else if (haz != hazard_success)
                        break;

                  if (candidate->isDS() || !can_move_down) {
                        add_to_hazard_query(&hq_down, candidate.get());
                        ctx.mv.downwards_skip(cursor);
                        continue;
                  }

                  MoveResult res = ctx.mv.downwards_move(cursor, false);
                  if (res == move_fail_ssa || res == move_fail_rar) {
                        add_to_hazard_query(&hq_down, candidate.get());
                        ctx.mv.downwards_skip(cursor);
                        continue;
                  } else if (res == move_fail_pressure) {
                        break;
                  }

                  if (cursor.source_idx +1 < ctx.last_SMEM_dep_idx)
                        ctx.last_SMEM_stall++;
                  k++;
            }

            UpwardsCursor up_cursor = ctx.mv.upwards_init(idx + 1, false);
            bool found_dependency = false;
            hazard_query hq_up;
            init_hazard_query(ctx, &hq_up);
            add_to_hazard_query(&hq_up, current);

            for ( ;
                 k < max_moves && up_cursor.source_idx < (int)block->instructions.size() && up_cursor.source_idx < idx + window_size;
            ) {
                  assert(up_cursor.source_idx >= 0 && up_cursor.source_idx < (int)block->instructions.size());
                  aco_ptr<Instruction>& candidate = block->instructions[up_cursor.source_idx];
                  if (!candidate) { ctx.mv.upwards_skip(up_cursor); continue;}


                  if (candidate->opcode == aco_opcode::p_logical_end)
                        break;

                  bool is_current_candidate_dependency = !found_dependency && !ctx.mv.upwards_check_deps(up_cursor);
                  if (is_current_candidate_dependency && (candidate->isVMEM() || candidate->isFlatLike()))
                        break;

                  bool general_hazard_as_dependency = false;
                  if (found_dependency) {
                        HazardResult haz = perform_hazard_query(&hq_up, candidate.get(), true);
                        if (haz == hazard_fail_reorder_ds || haz == hazard_fail_spill ||
                              haz == hazard_fail_reorder_sendmsg || haz == hazard_fail_barrier ||
                              haz == hazard_fail_export)
                              general_hazard_as_dependency = true;
                        else if (haz != hazard_success)
                              break;
                  }

                  if (is_current_candidate_dependency || general_hazard_as_dependency) {
                        if (!found_dependency) {
                              ctx.mv.upwards_update_insert_idx(up_cursor);
                              add_to_hazard_query(&hq_up, candidate.get());
                              found_dependency = true;
                        } else {
                              add_to_hazard_query(&hq_up, candidate.get());
                        }
                        ctx.mv.upwards_skip(up_cursor);
                        continue;
                  }

                  if (!found_dependency && (candidate->isVMEM() || candidate->isFlatLike())) {
                        ctx.mv.upwards_skip(up_cursor);
                        continue;
                  }

                  MoveResult res = ctx.mv.upwards_move(up_cursor);
                  if (res == move_fail_ssa || res == move_fail_rar) {
                        if (res == move_fail_ssa && (candidate->isVMEM() || candidate->isFlatLike()))
                              break;
                        add_to_hazard_query(&hq_up, candidate.get());
                        ctx.mv.upwards_skip(up_cursor);
                        continue;
                  } else if (res == move_fail_pressure) {
                        break;
                  }
                  k++;
            }

            ctx.last_SMEM_dep_idx = found_dependency ? up_cursor.insert_idx : 0;
            ctx.last_SMEM_stall = smem_base_latency_hiding_target - static_cast<int16_t>(ctx.occupancy_factor) - k;
      }

      void SimplePatternScheduler::schedule_VMEM_enhanced_with_pattern_bias(
            sched_ctx& ctx, Block* block, Instruction* current, int idx,
            float clause_bias, const SimplePatternScheduler::PatternInfo& pattern)
      {
            const float aggr = ctx.schedule_aggressiveness;
            int base_win = VMEM_WINDOW_SIZE_CTX;

            if (pattern.confidence > 0.7f) {
                  if (pattern.type == PatternInfo::RANDOM) {
                        base_win = int(base_win * 1.3f);
                  } else if (pattern.type == PatternInfo::SEQUENTIAL) {
                        base_win = int(base_win * 0.9f);
                  }
            }
            if (ctx.gfx_level == GFX9) {
                  double local_mem_ratio = calculate_local_memory_ratio_simple(block, idx);
                  base_win = VegaMemoryScheduler::calculate_vmem_window_enhanced(ctx, base_win, local_mem_ratio);
            }


            const int win_size  = int(base_win * aggr);
            const int max_moves = int(VMEM_MAX_MOVES_CTX * aggr);

            int base_clause = VMEM_CLAUSE_MAX_GRAB_DIST_CTX;
            int clause_dist = int(base_clause * (ctx.prefer_clauses ? aggr : 1.0f));

            if (pattern.confidence > 0.7f && pattern.type == PatternInfo::SEQUENTIAL && ctx.gfx_level == GFX9) {
                  clause_dist = int(clause_dist * 2.0f);
            } else if (ctx.gfx_level == GFX9 && idx + 1 < (int)block->instructions.size()){
                  auto pat_old = MemoryPatternAnalyzer::analyze_vmem_pattern(block, idx + 1, 64);
                  if (pat_old.pattern == MemoryPatternAnalyzer::PATTERN_SEQUENTIAL)
                        clause_dist = int(clause_dist * 2.5f);
                  else if (pat_old.pattern == MemoryPatternAnalyzer::PATTERN_STRIDED) {
                        if (pat_old.stride <= 32) clause_dist = int(clause_dist * 1.75f);
                        else if (pat_old.stride <= 128) clause_dist = int(clause_dist * 1.5f);
                        else if (pat_old.stride <= 4096) clause_dist = int(clause_dist * 1.2f);
                  }
            }


            DownwardsCursor cur_d = ctx.mv.downwards_init(idx, true, true);
            hazard_query indep_q, clause_q_local;
            init_hazard_query(ctx, &indep_q); add_to_hazard_query(&indep_q, current);
            init_hazard_query(ctx, &clause_q_local);

            bool only_clauses = false;
            int16_t k = 0;

            for ( ; k < max_moves &&
                  cur_d.source_idx >= 0 &&
                  cur_d.source_idx > idx - win_size; )
            {
                  auto& cand = block->instructions[cur_d.source_idx];
                  if (!cand) { ctx.mv.downwards_skip(cur_d); continue; }
                  if (cand->opcode == aco_opcode::p_logical_start) break;

                  bool part_clause = false;
                  if ((current->isVMEM() && cand->isVMEM()) ||
                        (current->isFlatLike() && cand->isFlatLike()))
                  {
                        int grab = (cur_d.insert_idx_clause - 1) - cur_d.source_idx;
                        if (grab < clause_dist) {
                              auto score = evaluator.evaluate_clause(current, cand.get(), ctx);
                              float adjustment = calculate_clause_threshold_adjustment(clause_bias, score);
                              if (score == VegaClauseEvaluator::CLAUSE_EXCELLENT) {
                                    part_clause = true;
                              } else if (score == VegaClauseEvaluator::CLAUSE_GOOD) {
                                    part_clause = (adjustment >= -0.1f);
                              } else if (score == VegaClauseEvaluator::CLAUSE_NEUTRAL) {
                                    part_clause = (adjustment > 0.1f);
                              } else {
                                    part_clause = false;
                              }
                        }
                  }

                  bool can_move = !cand->isVMEM() || part_clause || cand->definitions.empty();
                  HazardResult hz = perform_hazard_query(part_clause ? &clause_q_local : &indep_q,
                                                         cand.get(), false);
                  if (hz == hazard_fail_reorder_ds || hz == hazard_fail_spill ||
                        hz == hazard_fail_reorder_sendmsg || hz == hazard_fail_barrier ||
                        hz == hazard_fail_export)
                        can_move = false;
                  else if (hz != hazard_success)
                        break;

                  if (!can_move) {
                        if (part_clause) add_to_hazard_query(&clause_q_local, cand.get());
                        add_to_hazard_query(&indep_q, cand.get());
                        ctx.mv.downwards_skip(cur_d);
                        continue;
                  }

                  MoveResult res = ctx.mv.downwards_move(cur_d, part_clause);
                  if (res != move_success) {
                        if (part_clause) add_to_hazard_query(&clause_q_local, cand.get());
                        add_to_hazard_query(&indep_q, cand.get());
                        if (res == move_fail_pressure) only_clauses = true;
                        ctx.mv.downwards_skip(cur_d);
                        continue;
                  }

                  if (part_clause) {
                        add_to_hazard_query(&clause_q_local, cand.get());
                        add_to_hazard_query(&indep_q, cand.get());
                  } else {
                        add_to_hazard_query(&indep_q, cand.get());
                        ++k;
                  }
            }

            UpwardsCursor up = ctx.mv.upwards_init(idx + 1, true);
            bool found_dep = false;
            init_hazard_query(ctx, &indep_q); add_to_hazard_query(&indep_q, current);

            for ( ; k < max_moves &&
                  up.source_idx < (int)block->instructions.size() &&
                  up.source_idx < idx + win_size; )
            {
                  auto& cand = block->instructions[up.source_idx];
                  if (!cand) { ctx.mv.upwards_skip(up); continue; }
                  if (cand->opcode == aco_opcode::p_logical_end) break;

                  bool is_vmem = cand->isVMEM() || cand->isFlatLike();
                  bool is_dep  = !found_dep && !ctx.mv.upwards_check_deps(up);
                  bool haz_dep = false;

                  if (found_dep) {
                        HazardResult hz = perform_hazard_query(&indep_q, cand.get(), true);
                        if (hz == hazard_fail_reorder_ds      || hz == hazard_fail_spill ||
                              hz == hazard_fail_reorder_vmem_smem ||
                              hz == hazard_fail_reorder_sendmsg ||
                              hz == hazard_fail_barrier         || hz == hazard_fail_export)
                              haz_dep = true;
                        else if (hz != hazard_success)
                              break;
                  }

                  if (is_dep || haz_dep) {
                        if (!found_dep) {
                              ctx.mv.upwards_update_insert_idx(up);
                              add_to_hazard_query(&indep_q, cand.get());
                              found_dep = true;
                        } else
                              add_to_hazard_query(&indep_q, cand.get());

                        ctx.mv.upwards_skip(up);
                        continue;
                  }

                  if (!found_dep && is_vmem && !ctx.prefer_latency_hiding) {
                        for (const Definition& d : cand->definitions)
                              if (d.isTemp()) ctx.mv.depends_on[d.tempId()] = true;
                              add_to_hazard_query(&indep_q, cand.get());
                        ctx.mv.upwards_skip(up);
                        continue;
                  }
                  if (pattern.type == PatternInfo::SEQUENTIAL &&
                        pattern.confidence > 0.8f && k > max_moves / 2) {
                        break;
                        }

                        MoveResult res = ctx.mv.upwards_move(up);
                  if (res == move_fail_ssa || res == move_fail_rar) {
                        add_to_hazard_query(&indep_q, cand.get());
                        ctx.mv.upwards_skip(up);
                        continue;
                  }
                  if (res == move_fail_pressure) break;

                  if (!is_vmem) ++k;
            }
      }
      void
      schedule_LDS(sched_ctx& ctx, Block* block, Instruction* current, int idx)
      {
            assert(idx != 0 && current && block);
            int base_window_size = (ctx.gfx_level == GFX9 ? 80 : 64);
            int base_max_moves = current->isLDSDIR() ? LDSDIR_MAX_MOVES_CTX : LDS_MAX_MOVES_CTX;

            if (ctx.gfx_level == GFX9) {
                  base_max_moves = base_max_moves * 5 / 4;
                  if (ctx.occupancy_factor > 8) {
                        base_max_moves = base_max_moves * 3 / 4;
                  }
            }
            int window_size = static_cast<int>(base_window_size * ctx.schedule_aggressiveness);
            int max_moves = static_cast<int>(base_max_moves * ctx.schedule_aggressiveness);
            int16_t k = 0;

            hazard_query hq_down;
            init_hazard_query(ctx, &hq_down);
            add_to_hazard_query(&hq_down, current);

            DownwardsCursor cursor = ctx.mv.downwards_init(idx, true, false);

            for (int i_down = 0; k < max_moves && cursor.source_idx >=0 && (idx - cursor.source_idx) < window_size;  ) {
                  assert(cursor.source_idx < (int)block->instructions.size());
                  aco_ptr<Instruction>& candidate = block->instructions[cursor.source_idx];
                  if (!candidate) { ctx.mv.downwards_skip(cursor); i_down++; continue; }

                  bool is_mem = candidate->isVMEM() || candidate->isFlatLike() || candidate->isSMEM();
                  if (candidate->opcode == aco_opcode::p_logical_start || is_mem)
                        break;

                  if (candidate->isDS() || candidate->isLDSDIR()) {
                        add_to_hazard_query(&hq_down, candidate.get());
                        ctx.mv.downwards_skip(cursor);
                        i_down++;
                        continue;
                  }
                  HazardResult haz = perform_hazard_query(&hq_down, candidate.get(), false);
                  if (haz != hazard_success) {
                        break;
                  }
                  MoveResult move_res = ctx.mv.downwards_move(cursor, false);
                  if (move_res != move_success) {
                        add_to_hazard_query(&hq_down, candidate.get());
                        ctx.mv.downwards_skip(cursor);
                        if(move_res == move_fail_pressure) break;
                        i_down++;
                        continue;
                  }


                  k++;
                  i_down++;
            }

            bool found_dependency = false;
            int i_up = 0;
            UpwardsCursor up_cursor = ctx.mv.upwards_init(idx + 1, true);
            hazard_query hq_up;
            init_hazard_query(ctx, &hq_up);
            add_to_hazard_query(&hq_up, current);

            for (; k < max_moves && up_cursor.source_idx < (int)block->instructions.size() && i_up < window_size; i_up++) {
                  assert(up_cursor.source_idx >= 0 && up_cursor.source_idx < (int)block->instructions.size());
                  aco_ptr<Instruction>& candidate = block->instructions[up_cursor.source_idx];
                  if (!candidate) { ctx.mv.upwards_skip(up_cursor); continue;}

                  bool is_mem = candidate->isVMEM() || candidate->isFlatLike() || candidate->isSMEM();
                  if (candidate->opcode == aco_opcode::p_logical_end || is_mem) {
                        break;
                  }

                  if (!ctx.mv.upwards_check_deps(up_cursor)) {
                        add_to_hazard_query(&hq_up, candidate.get());
                        ctx.mv.upwards_update_insert_idx(up_cursor);
                        found_dependency = true;
                        ctx.mv.upwards_skip(up_cursor);
                        break;
                  }
                  HazardResult haz = perform_hazard_query(&hq_up, candidate.get(), true);
                  if (haz != hazard_success) {
                        add_to_hazard_query(&hq_up, candidate.get());
                        if(!found_dependency) {
                              ctx.mv.upwards_update_insert_idx(up_cursor);
                        }
                        ctx.mv.upwards_skip(up_cursor);
                        break;
                  }
                  MoveResult res = ctx.mv.upwards_move(up_cursor);
                  if (res != move_success) {
                        add_to_hazard_query(&hq_up, candidate.get());
                        ctx.mv.upwards_skip(up_cursor);
                        if (res == move_fail_pressure) break;
                  } else {
                        k++;
                  }
            }
      }

      void
      schedule_position_export(sched_ctx& ctx, Block* blk, Instruction* cur, int idx)
      {
            float gfx9_factor = 1.0f;
            if (ctx.gfx_level == GFX9) {
                  int count = 0, scan = 64;
                  int b = std::max(0, idx - scan / 2);
                  int e = std::min<int>(blk->instructions.size(), idx + scan / 2);
                  for (int i = b; i < e; ++i)
                        if (blk->instructions[i] && blk->instructions[i]->isEXP()) ++count;
                        if (count > 4) gfx9_factor *= 1.5f;
                        else if (count <= 2) gfx9_factor *= 0.8f;
            }

            int win_base = POS_EXP_WINDOW_SIZE_CTX / ctx.schedule_pos_export_div;
            int mov_base = POS_EXP_MAX_MOVES_CTX  / ctx.schedule_pos_export_div;

            int win_size = int(win_base * ctx.schedule_aggressiveness * gfx9_factor);
            int max_mv   = int(mov_base * ctx.schedule_aggressiveness * gfx9_factor);

            DownwardsCursor down = ctx.mv.downwards_init(idx, true, false);
            hazard_query hq; init_hazard_query(ctx, &hq); add_to_hazard_query(&hq, cur);

            int16_t k = 0;
            for (; k < max_mv &&
                  down.source_idx >= 0 &&
                  down.source_idx > idx - win_size; )
            {
                  auto& cand = blk->instructions[down.source_idx];
                  if (!cand) { ctx.mv.downwards_skip(down); continue; }

                  if (cand->opcode == aco_opcode::p_logical_start) break;
                  if (cand->isVMEM() || cand->isSMEM() || cand->isFlatLike()) break;

                  HazardResult hz = perform_hazard_query(&hq, cand.get(), false);
                  if (hz == hazard_fail_exec || hz == hazard_fail_unreorderable ||
                        hz == hazard_fail_export) break;
                  if (hz != hazard_success) {
                        add_to_hazard_query(&hq, cand.get());
                        ctx.mv.downwards_skip(down); continue;
                  }

                  unsigned salu_bias = 0;
                  if (cand->isSALU()) {
                        bool feeds_valu = false;
                        for (int look = down.source_idx + 1;
                             look < down.source_idx + 8 && look < (int)blk->instructions.size();
                        ++look)
                             {
                                   const auto& future = blk->instructions[look];
                                   if (!future || !future->isVALU()) continue;
                                   for (const Definition&  def : cand->definitions)
                                         for (const Operand& op : future->operands)
                                               if (def.isTemp() && op.isTemp() &&
                                                     def.tempId() == op.tempId()) { feeds_valu = true; break; }
                                                     if (feeds_valu) break;
                             }
                             salu_bias = feeds_valu ? 12 : 8;
                  }

                  unsigned cost = std::max(1u, get_likely_cost(cand.get()) - salu_bias);

                  MoveResult res = ctx.mv.downwards_move(down, false);
                  if (res == move_fail_ssa || res == move_fail_rar) {
                        add_to_hazard_query(&hq, cand.get());
                        ctx.mv.downwards_skip(down); continue;
                  }
                  if (res == move_fail_pressure) break;

                  k += cost;
            }
      }

      unsigned
      schedule_VMEM_store(sched_ctx& ctx, Block* block, Instruction* current, int idx)
      {
            assert(current && block);
            hazard_query hq;
            init_hazard_query(ctx, &hq);

            DownwardsCursor cursor = ctx.mv.downwards_init(idx, true, true);
            int skip = 0;

            int base_clause_max_grab_dist = VMEM_STORE_CLAUSE_MAX_GRAB_DIST_CTX;
            int clause_max_grab_dist = static_cast<int>(base_clause_max_grab_dist *
            ((ctx.gfx_level == GFX9 && ctx.prefer_clauses) ? ctx.schedule_aggressiveness : 1.0f));

            for (int k_dist = 0; k_dist < clause_max_grab_dist && cursor.source_idx >=0;) {
                  assert(cursor.source_idx < (int)block->instructions.size());
                  aco_ptr<Instruction>& candidate = block->instructions[cursor.source_idx];
                  if (!candidate) { ctx.mv.downwards_skip(cursor); if(cursor.source_idx < 0) break; k_dist = (cursor.insert_idx_clause > cursor.source_idx+1) ? (cursor.insert_idx_clause -1) - cursor.source_idx : 0; continue;}


                  if (candidate->opcode == aco_opcode::p_logical_start)
                        break;

                  bool basic_clause_ok = should_form_clause_vega_enhanced(current, candidate.get(), ctx.gfx_level, ctx);
                  if (!basic_clause_ok) {
                        add_to_hazard_query(&hq, candidate.get());
                        ctx.mv.downwards_skip(cursor);
                        if (cursor.source_idx < 0) break;
                        k_dist = (cursor.insert_idx_clause > cursor.source_idx +1) ? (cursor.insert_idx_clause -1) - cursor.source_idx : 0;
                        continue;
                  }

                  HazardResult haz_res = perform_hazard_query(&hq, candidate.get(), false);
                  if (haz_res != hazard_success){
                        if(haz_res == hazard_fail_exec || haz_res == hazard_fail_unreorderable) break;
                        add_to_hazard_query(&hq, candidate.get());
                        ctx.mv.downwards_skip(cursor);
                        if (cursor.source_idx < 0) break;
                        k_dist = (cursor.insert_idx_clause > cursor.source_idx +1) ? (cursor.insert_idx_clause -1) - cursor.source_idx : 0;
                        continue;
                  }

                  MoveResult move_res = ctx.mv.downwards_move(cursor, true);
                  if (move_res != move_success) {
                        break;
                  }

                  add_to_hazard_query(&hq, candidate.get());
                  skip++;
                  if (cursor.source_idx < 0) break;
                  k_dist = (cursor.insert_idx_clause > cursor.source_idx +1) ? (cursor.insert_idx_clause -1) - cursor.source_idx : 0;
            }
            return skip;
      }

      void schedule_block_perfected(sched_ctx& ctx, Program* prog, Block* blk)
      {
            ctx.last_SMEM_dep_idx = 0;
            ctx.last_SMEM_stall   = INT16_MIN;
            ctx.mv.block          = blk;

            unsigned mem_i = 0, alu_i = 0;
            for (const auto& ins : blk->instructions)
                  if (ins)
                        (ins->isVMEM() || ins->isFlatLike() || ins->isSMEM())
                        ? ++mem_i : ++alu_i;

                  const double mem_ratio_blk = double(mem_i) / double(std::max(1u, mem_i + alu_i));

            const float aggr_g   = ctx.schedule_aggressiveness;
            const bool  clause_g = ctx.prefer_clauses;
            const bool  lat_g    = ctx.prefer_latency_hiding;

            float boost = calculate_vega_ilp_boost(blk, mem_ratio_blk);
            ctx.schedule_aggressiveness =
            std::clamp(0.5f + 1.0f * float(mem_ratio_blk), 0.5f, 1.5f) * boost;

            ctx.prefer_latency_hiding = mem_ratio_blk > 0.25;
            ctx.prefer_clauses        = mem_ratio_blk > 0.15;

            if (ctx.gfx_level == GFX9) {
                  adjust_vega_scheduling_context(ctx, blk, mem_ratio_blk);
            }

            unsigned num_stores = 0;
            for (unsigned i = 0; i < blk->instructions.size(); ++i) {
                  Instruction* cur = blk->instructions[i].get();
                  if (!cur) continue;
                  if (cur->opcode == aco_opcode::p_logical_end) break;

                  if ((blk->kind & block_kind_export_end) &&
                        cur->isEXP() && ctx.schedule_pos_exports) {
                        unsigned tgt = cur->exp().dest;
                  if (tgt >= V_008DFC_SQ_EXP_POS && tgt < V_008DFC_SQ_EXP_PRIM) {
                        ctx.mv.current = cur;
                        schedule_position_export(ctx, blk, cur, i);
                  }
                        }

                        if (cur->definitions.empty()) {
                              if (cur->isVMEM() || cur->isFlatLike()) ++num_stores;
                              continue;
                        }

                        ctx.mv.current = cur;
                        if (cur->isVMEM() || cur->isFlatLike()) {
                              assert(ctx.prop_cache && ctx.pattern_scheduler);
                              for (int j = i + 1; j < std::min((int)blk->instructions.size(), (int)i + 8); j++) {
                                    if (blk->instructions[j]) {
                                          ctx.prop_cache->prefetch(blk->instructions[j].get());
                                    }
                              }
                              ctx.pattern_scheduler->schedule_vmem_adaptive(ctx, blk, cur, i);
                        } else if (cur->isSMEM()) {
                              schedule_SMEM(ctx, blk, cur, i);
                        } else if (cur->isLDSDIR() || (cur->isDS() && !cur->ds().gds)) {
                              schedule_LDS(ctx, blk, cur, i);
                        }
            }

            if (num_stores > 1 &&
                  (prog->gfx_level >= GFX11 ||
                  (prog->gfx_level == GFX9 && ctx.prefer_clauses))) {
                  for (int s = int(blk->instructions.size()) - 1; s >= 0; --s) {
                        Instruction* st = blk->instructions[s].get();
                        if (!st || !st->definitions.empty()) continue;
                        if (!(st->isVMEM() || st->isFlatLike())) continue;
                        ctx.mv.current = st;
                        s -= schedule_VMEM_store(ctx, blk, st, s);
                  }
                  }

                  blk->register_demand = blk->live_in_demand;
                  for (const auto& ins : blk->instructions)
                        if (ins) blk->register_demand.update(ins->register_demand);

                        ctx.schedule_aggressiveness = aggr_g;
            ctx.prefer_clauses          = clause_g;
            ctx.prefer_latency_hiding   = lat_g;
      }

      void
      schedule_program(Program* program)
      {
            if (!program)
                  return;

            unsigned mem_i = 0, alu_i = 0;
            for (const Block& b : program->blocks)
                  for (const auto& ins : b.instructions)
                        if (ins)
                              (ins->isVMEM() || ins->isFlatLike() || ins->isSMEM())
                              ? ++mem_i : ++alu_i;

                        const unsigned tot_i = mem_i + alu_i;
                  const double   mem_ratio_gl = tot_i ? double(mem_i) / double(tot_i) : 0.0;

            RegisterDemand demand;
            for (const Block& b : program->blocks)
                  demand.update(b.register_demand);

            sched_ctx ctx;
            ctx.gfx_level = program->gfx_level;
            ctx.mv.depends_on.resize            (program->peekAllocationId());
            ctx.mv.RAR_dependencies.resize      (program->peekAllocationId());
            ctx.mv.RAR_dependencies_clause.resize(program->peekAllocationId());

            ctx.init_caches();

            const int   wave_factor       = program->gfx_level >= GFX10 ? 2 : 1;
            const float reg_file_multiple =
            program->dev.physical_vgprs > 0
            ? program->dev.physical_vgprs / (256.0f * wave_factor) : 1.0f;
            const int wave_minimum =
            std::max<int>(program->min_waves,
                          int(4 * wave_factor * reg_file_multiple));

            int vgpr_spare = 12;
            if (program->gfx_level == GFX9) {
                  static constexpr std::array<int,9> vega_thr{24,32,40,48,64,80,128,170,256};
                  const int env_bias = getenv("ACO_SCHED_VEGA_SPARE_BIAS") ?
                  atoi(getenv("ACO_SCHED_VEGA_SPARE_BIAS")) : 0;

                  double best_cost = std::numeric_limits<double>::max();
                  int best_spare_for_cost = 0;

                  for (int spare_candidate = 0; spare_candidate <= 16; ++spare_candidate) {
                        int needed_vgpr = ((demand.vgpr + spare_candidate + 3) / 4) * 4;
                        if (!needed_vgpr)
                              needed_vgpr = 4;

                        int waves_for_vgpr_config = program->dev.physical_vgprs / needed_vgpr;
                        waves_for_vgpr_config     = std::clamp(waves_for_vgpr_config, 1, 10);

                        if (waves_for_vgpr_config < program->min_waves)
                              continue;

                        if ((program->stage == fragment_fs || program->stage == compute_cs) &&
                              waves_for_vgpr_config < 6 && program->dev.physical_vgprs >=256)
                              continue;

                        double reg_gain_cost = double(needed_vgpr - demand.vgpr);
                        double occ_pen_val  = double(10 - waves_for_vgpr_config);
                        double occ_coef = mem_ratio_gl * 1.0 + 0.5;

                        double current_cost = (10.0 / (reg_gain_cost + 1.0)) + occ_coef * occ_pen_val;
                        for (int t : vega_thr)
                              if (demand.vgpr <= t && needed_vgpr > t)
                                    current_cost += (t == 64 ? 4.0 : 2.0);

                        if (current_cost < best_cost) { best_cost = current_cost; best_spare_for_cost = spare_candidate; }
                  }
                  vgpr_spare = std::clamp(best_spare_for_cost + env_bias, 0, 16);
            } else if (program->gfx_level >= GFX10) {
                  vgpr_spare = 12;
            }


            int vgpr_need_final =
            std::max<int>(24, demand.vgpr) + int(vgpr_spare * reg_file_multiple);

            if (vgpr_need_final <= 0)
                  vgpr_need_final = program->dev.physical_vgprs ? program->dev.physical_vgprs : 256;

            int waves_vgpr = program->dev.physical_vgprs / vgpr_need_final;
            int sgpr_per_wave = (demand.sgpr + 15) & ~15;
            int waves_sgpr    = sgpr_per_wave
            ? program->dev.physical_sgprs / sgpr_per_wave
            : program->num_waves;

            int target_waves =
            std::clamp<int>(std::min(waves_vgpr, waves_sgpr),
                            wave_minimum, program->num_waves);
            target_waves = max_suitable_waves(program, target_waves);

            ctx.mv.max_registers = get_addr_regs_from_waves(program, target_waves);
            ctx.mv.max_registers.vgpr = std::max(0, ctx.mv.max_registers.vgpr - 2);

            ctx.occupancy_factor        = std::max(1, target_waves / wave_factor);
            ctx.schedule_aggressiveness = 1.0f;
            ctx.prefer_clauses          = true;
            ctx.prefer_latency_hiding   = false;

            if (program->info.hw_stage == AC_HW_NEXT_GEN_GEOMETRY_SHADER) {
                  ctx.schedule_pos_exports    = program->info.schedule_ngg_pos_exports;
                  ctx.schedule_pos_export_div = 4;
            }

            for (Block& b : program->blocks)
                  schedule_block_perfected(ctx, program, &b);

            RegisterDemand new_dem;
            for (Block& b : program->blocks)
                  new_dem.update(b.register_demand);

            update_vgpr_sgpr_demand(program, new_dem);
            if (!validate_live_vars(program))
                  abort();
      }

}
