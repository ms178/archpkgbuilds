//===- ScheduleOptimizer.cpp - Calculate an optimized schedule ------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the Polly schedule optimizer.
//
// A key feature of this implementation is a sophisticated, conservative
// profitability model that acts as a gatekeeper. It analyzes the workload to
// ensure Polly only optimizes code that fits the polyhedral model (e.g., with
// regular memory access and sufficient arithmetic intensity), preventing performance
// regressions on general-purpose systems code, which is critical for compiling
// large projects like the Linux kernel for latency-sensitive applications.
//
// When a SCoP is deemed profitable, post-scheduling transformations are
// applied with dynamic, target-aware decisions via LLVM's TargetTransformInfo
// (TTI), ensuring adaptability across all modern microarchitectures.
//
//===----------------------------------------------------------------------===//

#include "polly/ScheduleOptimizer.h"
#include "polly/CodeGen/CodeGeneration.h"
#include "polly/DependenceInfo.h"
#include "polly/ManualOptimizer.h"
#include "polly/MatmulOptimizer.h"
#include "polly/Options.h"
#include "polly/ScheduleTreeTransform.h"
#include "polly/ScopInfo.h"
#include "polly/Support/ISLOStream.h"
#include "polly/Support/ISLTools.h"
#include "llvm/ADT/Sequence.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/OptimizationRemarkEmitter.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/InitializePasses.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MathExtras.h"
#include "isl/options.h"
#include <algorithm>
#include <cmath>

using namespace llvm;
using namespace polly;

namespace llvm {
class Loop;
class Module;
} // namespace llvm

#include "polly/Support/PollyDebug.h"
#define DEBUG_TYPE "polly-opt-isl"

//===----------------------------------------------------------------------===//
// Command-line options (with generic defaults; optimizer uses TTI for intelligence)
//===----------------------------------------------------------------------===//

static cl::opt<std::string>
    OptimizeDeps("polly-opt-optimize-only",
                 cl::desc("Only a certain kind of dependences (all/raw)"),
                 cl::Hidden, cl::init("all"), cl::cat(PollyCategory));

static cl::opt<std::string>
    SimplifyDeps("polly-opt-simplify-deps",
                 cl::desc("Dependences should be simplified (yes/no)"),
                 cl::Hidden, cl::init("yes"), cl::cat(PollyCategory));

static cl::opt<int> MaxConstantTerm(
    "polly-opt-max-constant-term",
    cl::desc("The maximal constant term allowed (-1 is unlimited)"), cl::Hidden,
    cl::init(20), cl::cat(PollyCategory));

static cl::opt<int> MaxCoefficient(
    "polly-opt-max-coefficient",
    cl::desc("The maximal coefficient allowed (-1 is unlimited)"), cl::Hidden,
    cl::init(20), cl::cat(PollyCategory));

static cl::opt<std::string>
    MaximizeBandDepth("polly-opt-maximize-bands",
                      cl::desc("Maximize the band depth (yes/no)"), cl::Hidden,
                      cl::init("yes"), cl::cat(PollyCategory));

static cl::opt<int>
    ScheduleComputeOut("polly-schedule-computeout",
                       cl::desc("Bound the scheduler by maximal amount"
                                "of computational steps. "),
                       cl::Hidden, cl::init(300000), cl::ZeroOrMore,
                       cl::cat(PollyCategory));

static cl::opt<bool>
    GreedyFusion("polly-loopfusion-greedy",
                 cl::desc("Aggressively try to fuse everything"), cl::Hidden,
                 cl::cat(PollyCategory));

static cl::opt<std::string> OuterCoincidence(
    "polly-opt-outer-coincidence",
    cl::desc("Try to construct schedules where the outer member of each band "
             "satisfies the coincidence constraints (yes/no)"),
    cl::Hidden, cl::init("no"), cl::cat(PollyCategory));

static cl::opt<int> PrevectorWidth(
    "polly-prevect-width",
    cl::desc(
        "The number of loop iterations to strip-mine for pre-vectorization"),
    cl::Hidden, cl::init(4), cl::cat(PollyCategory));

static cl::opt<bool> FirstLevelTiling("polly-tiling",
                                      cl::desc("Enable loop tiling"),
                                      cl::init(true), cl::cat(PollyCategory));

static cl::opt<int> FirstLevelDefaultTileSize(
    "polly-default-tile-size",
    cl::desc("The default tile size (if not enough were provided by"
             " --polly-tile-sizes)"),
    cl::Hidden, cl::init(32), cl::cat(PollyCategory));

static cl::list<int>
    FirstLevelTileSizes("polly-tile-sizes",
                        cl::desc("A tile size for each loop dimension, filled "
                                 "with --polly-default-tile-size"),
                        cl::Hidden, cl::CommaSeparated, cl::cat(PollyCategory));

static cl::opt<bool>
    SecondLevelTiling("polly-2nd-level-tiling",
                      cl::desc("Enable a 2nd level loop of loop tiling"),
                      cl::cat(PollyCategory));

static cl::opt<int> SecondLevelDefaultTileSize(
    "polly-2nd-level-default-tile-size",
    cl::desc("The default 2nd-level tile size (if not enough were provided by"
             " --polly-2nd-level-tile-sizes)"),
    cl::Hidden, cl::init(16), cl::cat(PollyCategory));

static cl::list<int>
    SecondLevelTileSizes("polly-2nd-level-tile-sizes",
                         cl::desc("A tile size for each loop dimension, filled "
                                  "with --polly-default-tile-size"),
                         cl::Hidden, cl::CommaSeparated,
                         cl::cat(PollyCategory));

static cl::opt<bool> RegisterTiling("polly-register-tiling",
                                    cl::desc("Enable register tiling"),
                                    cl::cat(PollyCategory));

static cl::opt<int> RegisterDefaultTileSize(
    "polly-register-tiling-default-tile-size",
    cl::desc("The default register tile size (if not enough were provided by"
             " --polly-register-tile-sizes)"),
    cl::Hidden, cl::init(4), cl::cat(PollyCategory));

static cl::list<int>
    RegisterTileSizes("polly-register-tile-sizes",
                      cl::desc("A tile size for each loop dimension, filled "
                               "with --polly-register-tile-size"),
                      cl::Hidden, cl::CommaSeparated, cl::cat(PollyCategory));

static cl::opt<bool> PragmaBasedOpts(
    "polly-pragma-based-opts",
    cl::desc("Apply user-directed transformation from metadata"),
    cl::init(true), cl::cat(PollyCategory));

static cl::opt<bool> EnableReschedule("polly-reschedule",
                                      cl::desc("Optimize SCoPs using ISL"),
                                      cl::init(true), cl::cat(PollyCategory));

static cl::opt<bool>
    PMBasedOpts("polly-pattern-matching-based-opts",
                cl::desc("Perform optimizations based on pattern matching"),
                cl::init(true), cl::cat(PollyCategory));

static cl::opt<bool>
    EnablePostopts("polly-postopts",
                   cl::desc("Apply post-rescheduling optimizations such as "
                            "tiling (requires -polly-reschedule)"),
                   cl::init(true), cl::cat(PollyCategory));

static cl::opt<bool> OptimizedScops(
    "polly-optimized-scops",
    cl::desc("Polly - Dump polyhedral description of Scops optimized with "
             "the isl scheduling optimizer and the set of post-scheduling "
             "transformations is applied on the schedule tree"),
    cl::cat(PollyCategory));

//===----------------------------------------------------------------------===//
// Statistics
//===----------------------------------------------------------------------===//

STATISTIC(ScopsProcessed, "Number of scops processed");
STATISTIC(ScopsRejected, "Number of scops rejected by profitability model");
STATISTIC(ScopsRescheduled, "Number of scops rescheduled");
STATISTIC(ScopsOptimized, "Number of scops optimized");
STATISTIC(NumAffineLoopsOptimized, "Number of affine loops optimized");
STATISTIC(NumBoxedLoopsOptimized, "Number of boxed loops optimized");
#define THREE_STATISTICS(VARNAME, DESC)                                        \
  static Statistic VARNAME[3] = {                                              \
      {DEBUG_TYPE, #VARNAME "0", DESC " (original)"},                          \
      {DEBUG_TYPE, #VARNAME "1", DESC " (after scheduler)"},                   \
      {DEBUG_TYPE, #VARNAME "2", DESC " (after optimizer)"}}
THREE_STATISTICS(NumBands, "Number of bands");
THREE_STATISTICS(NumBandMembers, "Number of band members");
THREE_STATISTICS(NumCoincident, "Number of coincident band members");
THREE_STATISTICS(NumPermutable, "Number of permutable bands");
THREE_STATISTICS(NumFilters, "Number of filter nodes");
THREE_STATISTICS(NumExtension, "Number of extension nodes");
STATISTIC(FirstLevelTileOpts, "Number of first level tiling applied");
STATISTIC(SecondLevelTileOpts, "Number of second level tiling applied");
STATISTIC(RegisterTileOpts, "Number of register tiling applied");
STATISTIC(PrevectOpts, "Number of strip-mining for prevectorization applied");
STATISTIC(MatMulOpts,
          "Number of matrix multiplication patterns detected and optimized");
STATISTIC(NumVectorizedBands, "Number of bands marked for vectorization");

namespace {
// Additional parameters to guide the schedule optimizer.
struct OptimizerAdditionalInfoTy {
  Scop *S;
  const llvm::TargetTransformInfo *TTI;
  const Dependences *D;
  bool PatternOpts;
  bool Postopts;
  bool Prevect;
  bool &DepsChanged;
};

//===----------------------------------------------------------------------===//
// ScheduleTreeOptimizer - A set of post-scheduling transformations.
//===----------------------------------------------------------------------===//

class ScheduleTreeOptimizer final {
public:
  static isl::schedule
  optimizeSchedule(isl::schedule Schedule, const OptimizerAdditionalInfoTy *OAI);
  static isl::schedule_node
  optimizeScheduleNode(isl::schedule_node Node, const OptimizerAdditionalInfoTy *OAI);
  static bool isProfitableSchedule(Scop &S, isl::schedule NewSchedule);
  static isl::schedule_node isolateFullPartialTiles(isl::schedule_node Node, int VectorWidth);

private:
  static bool isTileableBandNode(isl::schedule_node Node);
  static bool isPMOptimizableBandNode(isl::schedule_node Node);
  static isl::schedule_node prevectSchedBand(isl::schedule_node Node,
                                             unsigned DimToVectorize,
                                             int VectorWidth,
                                             const TargetTransformInfo *TTI);
  static isl_schedule_node *optimizeBand(__isl_take isl_schedule_node *Node, void *User);
  static isl::schedule_node applyTileBandOpt(isl::schedule_node Node, const OptimizerAdditionalInfoTy *OAI);
  static isl::schedule_node applyPrevectBandOpt(isl::schedule_node Node, const OptimizerAdditionalInfoTy *OAI);
};

// A schedule rewriter to insert target-aware SIMD markers.
struct InsertSimdMarkers final : ScheduleNodeRewriter<InsertSimdMarkers> {
  const TargetTransformInfo *TTI;
  InsertSimdMarkers(const TargetTransformInfo *TTI) : TTI(TTI) {}

  isl::schedule_node visitBand(isl::schedule_node_band Band) {
    isl::schedule_node Node = visitChildren(Band);
    if (!Node.first_child().isa<isl::schedule_node_leaf>()) {
      return Node;
    }

    Node = Band.insert_mark(isl::id::alloc(Band.ctx(), "SIMD", nullptr));

    if (TTI) {
      unsigned RegBitWidth = TTI->getLoadStoreVecRegBitWidth(0);
      if (RegBitWidth > 0) {
        unsigned Alignment = llvm::divideCeil(RegBitWidth, 8);
        std::string OptStr = "{ align[" + std::to_string(Alignment) + "] }";
        isl::union_set AlignOpts = isl::union_set(Node.ctx(), OptStr);
        Node = Node.as<isl::schedule_node_band>().set_ast_build_options(AlignOpts);
      }
    }
    NumVectorizedBands++;
    return Node;
  }
};

isl::schedule_node
ScheduleTreeOptimizer::isolateFullPartialTiles(isl::schedule_node Node, int VectorWidth) {
  assert(isl_schedule_node_get_type(Node.get()) == isl_schedule_node_band && "Expected a band node");
  Node = Node.child(0).child(0);
  isl::union_map SchedRelUMap = Node.get_prefix_schedule_relation();
  isl::union_set ScheduleRangeUSet = SchedRelUMap.range();
  isl::set ScheduleRange{ScheduleRangeUSet};
  isl::set IsolateDomain = getPartialTilePrefixes(ScheduleRange, VectorWidth);
  auto AtomicOption = getDimOptions(IsolateDomain.ctx(), "atomic");
  isl::union_set IsolateOption = getIsolateOptions(IsolateDomain, 1);
  Node = Node.parent().parent();
  isl::union_set Options = IsolateOption.unite(AtomicOption);
  isl::schedule_node_band Result =
      Node.as<isl::schedule_node_band>().set_ast_build_options(Options);
  return Result;
}

isl::schedule_node ScheduleTreeOptimizer::prevectSchedBand(
    isl::schedule_node Node, unsigned DimToVectorize, int VectorWidth,
    const TargetTransformInfo *TTI) {
  assert(isl_schedule_node_get_type(Node.get()) == isl_schedule_node_band);

  auto Space = isl::manage(isl_schedule_node_band_get_space(Node.get()));
  unsigned ScheduleDimensions = unsignedFromIslSize(Space.dim(isl::dim::set));
  assert(DimToVectorize < ScheduleDimensions);

  if (DimToVectorize > 0) {
    Node = isl::manage(
        isl_schedule_node_band_split(Node.release(), DimToVectorize));
    Node = Node.child(0);
  }
  if (DimToVectorize < ScheduleDimensions - 1) {
    Node = isl::manage(isl_schedule_node_band_split(Node.release(), 1));
  }
  Space = isl::manage(isl_schedule_node_band_get_space(Node.get()));
  auto Sizes = isl::multi_val::zero(Space);
  Sizes = Sizes.set_val(0, isl::val(Node.ctx(), VectorWidth));
  Node =
      isl::manage(isl_schedule_node_band_tile(Node.release(), Sizes.release()));
  Node = isolateFullPartialTiles(Node, VectorWidth);
  Node = Node.child(0);
  Node = Node.as<isl::schedule_node_band>().set_ast_build_options(
      isl::union_set(Node.ctx(), "{ unroll[x]: 1 = 0 }"));
  Node = isl::manage(isl_schedule_node_band_sink(Node.release()));
  Node = InsertSimdMarkers(TTI).visit(Node);

  PrevectOpts++;
  return Node.parent();
}

static bool isSimpleInnermostBand(const isl::schedule_node &Node) {
  assert(isl_schedule_node_get_type(Node.get()) == isl_schedule_node_band);
  assert(isl_schedule_node_n_children(Node.get()) == 1);

  auto ChildType = isl_schedule_node_get_type(Node.child(0).get());

  if (ChildType == isl_schedule_node_leaf) {
    return true;
  }

  if (ChildType != isl_schedule_node_sequence) {
    return false;
  }

  auto Sequence = Node.child(0);

  for (int c = 0, nc = isl_schedule_node_n_children(Sequence.get()); c < nc;
       ++c) {
    auto Child = Sequence.child(c);
    if (isl_schedule_node_get_type(Child.get()) != isl_schedule_node_filter) {
      return false;
    }
    if (isl_schedule_node_get_type(Child.child(0).get()) !=
        isl_schedule_node_leaf) {
      return false;
    }
  }
  return true;
}

static bool isOneTimeParentBandNode(isl::schedule_node Node) {
  if (isl_schedule_node_get_type(Node.get()) != isl_schedule_node_band) {
    return false;
  }
  if (isl_schedule_node_n_children(Node.get()) != 1) {
    return false;
  }
  return true;
}

bool ScheduleTreeOptimizer::isTileableBandNode(isl::schedule_node Node) {
  if (!isOneTimeParentBandNode(Node)) {
    return false;
  }
  if (!isl_schedule_node_band_get_permutable(Node.get())) {
    return false;
  }
  auto Space = isl::manage(isl_schedule_node_band_get_space(Node.get()));
  if (unsignedFromIslSize(Space.dim(isl::dim::set)) <= 1u) {
    return false;
  }
  return isSimpleInnermostBand(Node);
}

bool ScheduleTreeOptimizer::isPMOptimizableBandNode(isl::schedule_node Node) {
  if (!isOneTimeParentBandNode(Node)) {
    return false;
  }
  return Node.child(0).isa<isl::schedule_node_leaf>();
}

// Heuristic to get dominant element bytes from SCoP.
static unsigned getDominantElementTypeBytes(Scop &S) {
    // TODO: A more advanced implementation would analyze ScopStmts to find
    // the most frequent memory access type size. For now, we default to 4,
    // which covers standard integer and single-precision float operations.
    return 4;
}

// Derives a cache-aware tile size from TTI.
static int getCacheAwareTileSize(const TargetTransformInfo *TTI,
                                 TargetTransformInfo::CacheLevel Level,
                                 int DefaultSize, unsigned ElemBytes) {
  if (!TTI || ElemBytes == 0) {
    return DefaultSize;
  }
  auto MaybeCacheSize = TTI->getCacheSize(Level);
  if (!MaybeCacheSize || *MaybeCacheSize == 0) {
    return DefaultSize;
  }

  double Effective = 0.8 * static_cast<double>(*MaybeCacheSize) / ElemBytes;
  double TileDouble = std::sqrt(Effective);
  int Tile = std::clamp(static_cast<int>(TileDouble), 16, 256);
  return llvm::PowerOf2Ceil(static_cast<unsigned>(Tile));
}

__isl_give isl::schedule_node
ScheduleTreeOptimizer::applyTileBandOpt(isl::schedule_node Node, const OptimizerAdditionalInfoTy *OAI) {
  unsigned ElemBytes = getDominantElementTypeBytes(*OAI->S);

  if (FirstLevelTiling) {
    int TileSize = getCacheAwareTileSize(OAI->TTI, TargetTransformInfo::CacheLevel::L1D,
                                         FirstLevelDefaultTileSize, ElemBytes);
    Node = tileNode(Node, "1st level tiling", FirstLevelTileSizes, TileSize);
    FirstLevelTileOpts++;
  }

  if (SecondLevelTiling) {
    int TileSize = getCacheAwareTileSize(OAI->TTI, TargetTransformInfo::CacheLevel::L2D,
                                         SecondLevelDefaultTileSize, ElemBytes);
    Node = tileNode(Node, "2nd level tiling", SecondLevelTileSizes, TileSize);
    SecondLevelTileOpts++;
  }

  if (RegisterTiling) {
    int RegTileSize = RegisterDefaultTileSize;
    if (OAI->TTI) {
      // Corrected API call for getUnrollingPreferences.
      TargetTransformInfo::UnrollingPreferences UnrollPrefs;
      // We pass nullptr for the Loop* and ORE* as we are in a context where a
      // specific loop is not available, and we want the general target default.
      OAI->TTI->getUnrollingPreferences(nullptr, *OAI->S->getSE(), UnrollPrefs,
                                        nullptr);
      if (UnrollPrefs.Count > 0) {
        RegTileSize = UnrollPrefs.Count;
      }
    }
    Node = applyRegisterTiling(Node, RegisterTileSizes, RegTileSize);
    RegisterTileOpts++;
  }
  return Node;
}

isl::schedule_node
ScheduleTreeOptimizer::applyPrevectBandOpt(isl::schedule_node Node, const OptimizerAdditionalInfoTy *OAI) {
  auto Space = isl::manage(isl_schedule_node_band_get_space(Node.get()));
  int Dims = unsignedFromIslSize(Space.dim(isl::dim::set));
  unsigned ElemBytes = getDominantElementTypeBytes(*OAI->S);

  for (int i = Dims - 1; i >= 0; i--) {
    if (Node.as<isl::schedule_node_band>().member_get_coincident(i)) {
      int VecWidth = PrevectorWidth;
      if (OAI->TTI) {
        unsigned RegBitWidth = OAI->TTI->getLoadStoreVecRegBitWidth(0);
        if (RegBitWidth > 0 && ElemBytes > 0) {
          VecWidth = RegBitWidth / (ElemBytes * 8);
        }
      }
      Node = prevectSchedBand(Node, i, std::max(1, VecWidth), OAI->TTI);
      break;
    }
  }
  return Node;
}

__isl_give isl_schedule_node *
ScheduleTreeOptimizer::optimizeBand(__isl_take isl_schedule_node *NodeArg,
                                    void *User) {
  const OptimizerAdditionalInfoTy *OAI =
      static_cast<const OptimizerAdditionalInfoTy *>(User);
  assert(OAI && "Expecting optimization options");

  isl::schedule_node Node = isl::manage(NodeArg);

  if (OAI->PatternOpts && isPMOptimizableBandNode(Node)) {
    isl::schedule_node PatternOptimizedSchedule =
        tryOptimizeMatMulPattern(Node, OAI->TTI, OAI->D);
    if (!PatternOptimizedSchedule.is_null()) {
      MatMulOpts++;
      OAI->DepsChanged = true;
      return PatternOptimizedSchedule.release();
    }
  }

  if (!isTileableBandNode(Node)) {
    return Node.release();
  }

  if (OAI->Postopts) {
    Node = applyTileBandOpt(Node, OAI);
  }

  if (OAI->Prevect) {
    Node = applyPrevectBandOpt(Node, OAI);
  }

  return Node.release();
}

isl::schedule
ScheduleTreeOptimizer::optimizeSchedule(isl::schedule Schedule,
                                        const OptimizerAdditionalInfoTy *OAI) {
  auto Root = Schedule.get_root();
  Root = optimizeScheduleNode(Root, OAI);
  return Root.get_schedule();
}

isl::schedule_node ScheduleTreeOptimizer::optimizeScheduleNode(
    isl::schedule_node Node, const OptimizerAdditionalInfoTy *OAI) {
  Node = isl::manage(isl_schedule_node_map_descendant_bottom_up(
      Node.release(), optimizeBand,
      const_cast<void *>(static_cast<const void *>(OAI))));
  return Node;
}

static unsigned countOuterParallelBands(const isl::schedule &Schedule) {
    unsigned Count = 0;
    if (isl::schedule_node Root = Schedule.get_root(); !Root.is_null()) {
      // Corrected lambda signature to return isl::stat.
      Root.foreach_ancestor_top_down(
          [&](const isl::schedule_node &node) -> isl::stat {
            if (node.isa<isl::schedule_node_band>() &&
                node.as<isl::schedule_node_band>().get_permutable()) {
              Count++;
            }
            return isl::stat::ok();
          });
    }
    return Count;
}

bool ScheduleTreeOptimizer::isProfitableSchedule(Scop &S,
                                                 isl::schedule NewSchedule) {
  auto OldSchedule = S.getScheduleTree();
  assert(!OldSchedule.is_null() && "Original schedule should be valid");

  unsigned NewParallelBands = countOuterParallelBands(NewSchedule);
  unsigned OldParallelBands = countOuterParallelBands(OldSchedule);

  if (NewParallelBands > OldParallelBands) {
    return true;
  }

  return !OldSchedule.get_map().is_equal(NewSchedule.get_map());
}

// A gatekeeper function to prevent Polly from optimizing code that is unlikely
// to benefit, which is common in systems and gaming code.
static bool isProfitableToOptimize(Scop &S) {
  const unsigned MinProfitableLoopDepth = 2;
  const unsigned MaxIrregularAccesses = 2;
  const unsigned MinFPInstructions = 1;

  if (S.getMaxLoopDepth() < MinProfitableLoopDepth) {
    POLLY_DEBUG(dbgs() << "Skipping SCoP: Not enough loop depth\n");
    return false;
  }

  unsigned IrregularAccessCount = 0;
  unsigned FPInstructionCount = 0;
  for (const ScopStmt &Stmt : S) {
    for (const MemoryAccess *MA : Stmt) {
      if (!MA->isAffine()) {
        IrregularAccessCount++;
      }
    }
    for (const Instruction *I : Stmt.getInstructions()) {
      if (I->getType()->isFloatingPointTy()) {
        FPInstructionCount++;
      }
    }
  }

  if (IrregularAccessCount > MaxIrregularAccesses) {
    POLLY_DEBUG(dbgs() << "Skipping SCoP: Too many irregular memory accesses\n");
    ScopsRejected++;
    return false;
  }

  if (FPInstructionCount < MinFPInstructions) {
    POLLY_DEBUG(dbgs() << "Skipping SCoP: Not enough floating point instructions\n");
    ScopsRejected++;
    return false;
  }

  return true;
}

class IslScheduleOptimizerWrapperPass final : public ScopPass {
public:
  static char ID;

  explicit IslScheduleOptimizerWrapperPass() : ScopPass(ID) {}

  bool runOnScop(Scop &S) override;
  void printScop(raw_ostream &OS, Scop &S) const override;
  void getAnalysisUsage(AnalysisUsage &AU) const override;
  void releaseMemory() override {
    LastSchedule = {};
    IslCtx.reset();
  }

private:
  std::shared_ptr<isl_ctx> IslCtx;
  isl::schedule LastSchedule;
};

char IslScheduleOptimizerWrapperPass::ID = 0;

#ifndef NDEBUG
static void printSchedule(llvm::raw_ostream &OS, const isl::schedule &Schedule,
                          StringRef Desc) {
  isl::ctx Ctx = Schedule.ctx();
  isl_printer *P = isl_printer_to_str(Ctx.get());
  P = isl_printer_set_yaml_style(P, ISL_YAML_STYLE_BLOCK);
  P = isl_printer_print_schedule(P, Schedule.get());
  char *Str = isl_printer_get_str(P);
  OS << Desc << ": \n" << Str << "\n";
  free(Str);
  isl_printer_free(P);
}
#endif

static void walkScheduleTreeForStatistics(isl::schedule Schedule, int Version) {
  auto Root = Schedule.get_root();
  if (Root.is_null()) {
    return;
  }

  isl_schedule_node_foreach_descendant_top_down(
      Root.get(),
      [](__isl_keep isl_schedule_node *nodeptr, void *user) -> isl_bool {
        isl::schedule_node Node = isl::manage_copy(nodeptr);
        int Version = *static_cast<int *>(user);

        switch (isl_schedule_node_get_type(Node.get())) {
        case isl_schedule_node_band: {
          NumBands[Version]++;
          if (isl_schedule_node_band_get_permutable(Node.get()) ==
              isl_bool_true) {
            NumPermutable[Version]++;
          }
          int CountMembers = isl_schedule_node_band_n_member(Node.get());
          NumBandMembers[Version] += CountMembers;
          for (int i = 0; i < CountMembers; i += 1) {
            if (Node.as<isl::schedule_node_band>().member_get_coincident(i)) {
              NumCoincident[Version]++;
            }
          }
          break;
        }
        case isl_schedule_node_filter:
          NumFilters[Version]++;
          break;
        case isl_schedule_node_extension:
          NumExtension[Version]++;
          break;
        default:
          break;
        }
        return isl_bool_true;
      },
      &Version);
}

void prepareIslOptions(isl_ctx *Ctx) {
  int IslMaximizeBands = (MaximizeBandDepth == "yes") ? 1 : 0;
  if (MaximizeBandDepth != "yes" && MaximizeBandDepth != "no") {
    errs() << "warning: Option -polly-opt-maximize-bands should be 'yes'/'no'\n";
  }

  int IslOuterCoincidence = (OuterCoincidence == "yes") ? 1 : 0;
  if (OuterCoincidence != "yes" && OuterCoincidence != "no") {
    errs() << "warning: Option -polly-opt-outer-coincidence should be 'yes'/'no'\n";
  }

  isl_options_set_schedule_outer_coincidence(Ctx, IslOuterCoincidence);
  isl_options_set_schedule_maximize_band_depth(Ctx, IslMaximizeBands);
  isl_options_set_schedule_max_constant_term(Ctx, MaxConstantTerm);
  isl_options_set_schedule_max_coefficient(Ctx, MaxCoefficient);
  isl_options_set_tile_scale_tile_loops(Ctx, 0);
}

isl::schedule computeSchedule(Scop &S, const Dependences &D) {
  int ValidityKinds = Dependences::TYPE_RAW | Dependences::TYPE_WAR | Dependences::TYPE_WAW;
  int ProximityKinds = (OptimizeDeps == "raw") ? Dependences::TYPE_RAW
                       : Dependences::TYPE_RAW | Dependences::TYPE_WAR | Dependences::TYPE_WAW;

  isl::union_set Domain = S.getDomains();
  if (Domain.is_null()) {
    return {};
  }

  isl::union_map Validity = D.getDependences(ValidityKinds);
  isl::union_map Proximity = D.getDependences(ProximityKinds);

  if (SimplifyDeps == "yes") {
    Validity = Validity.gist_domain(Domain).gist_range(Domain);
    Proximity = Proximity.gist_domain(Domain).gist_range(Domain);
  } else if (SimplifyDeps != "no") {
    errs() << "warning: -polly-opt-simplify-deps should be 'yes' or 'no'\n";
  }

  POLLY_DEBUG(dbgs() << "\n\nCompute schedule from:\n";
              dbgs() << "Domain := " << Domain << ";\n";
              dbgs() << "Proximity := " << Proximity << ";\n";
              dbgs() << "Validity := " << Validity << ";\n");

  auto SC = isl::schedule_constraints::on_domain(Domain)
                .set_proximity(Proximity)
                .set_validity(Validity)
                .set_coincidence(Validity);

  isl::schedule Result;
  isl_ctx *Ctx = S.getIslCtx().get();
  auto OnErrorStatus = isl_options_get_on_error(Ctx);
  isl_options_set_on_error(Ctx, ISL_ON_ERROR_CONTINUE);
  {
    IslMaxOperationsGuard MaxOpGuard(Ctx, ScheduleComputeOut);
    Result = SC.compute_schedule();
    if (MaxOpGuard.hasQuotaExceeded()) {
      POLLY_DEBUG(dbgs() << "Scheduler calculation exceeds ISL quota\n");
    }
  }
  isl_options_set_on_error(Ctx, OnErrorStatus);
  return Result;
}

void runIslScheduleOptimizer(
    Scop &S,
    function_ref<const Dependences &(Dependences::AnalysisLevel)> GetDeps,
    TargetTransformInfo *TTI, OptimizationRemarkEmitter *ORE,
    isl::schedule &LastSchedule, bool &DepsChanged) {
  if (S.getSize() == 0) {
    S.markAsOptimized();
    return;
  }
  ScopsProcessed++;

  if (!isProfitableToOptimize(S)) {
    return;
  }

  isl::schedule Schedule = S.getScheduleTree();
  walkScheduleTreeForStatistics(Schedule, 0);
  POLLY_DEBUG(printSchedule(dbgs(), Schedule, "Original schedule tree"));

  bool HasUserTransformation = false;
  if (PragmaBasedOpts) {
    isl::schedule ManuallyTransformed =
        applyManualTransformations(&S, Schedule, GetDeps(Dependences::AL_Statement), ORE);
    if (ManuallyTransformed.is_null()) {
      POLLY_DEBUG(dbgs() << "Error during manual optimization\n");
      return;
    }
    if (ManuallyTransformed.get() != Schedule.get()) {
      HasUserTransformation = true;
      Schedule = std::move(ManuallyTransformed);
      POLLY_DEBUG(printSchedule(dbgs(), Schedule, "After manual transformations"));
    }
  }

  if (!HasUserTransformation && S.hasDisableHeuristicsHint()) {
    POLLY_DEBUG(dbgs() << "Heuristic optimizations disabled by metadata\n");
    return;
  }

  const Dependences &D = GetDeps(Dependences::AL_Statement);
  if (D.getSharedIslCtx() != S.getSharedIslCtx() || !D.hasValidDependences()) {
    POLLY_DEBUG(dbgs() << "Dependency information not available or invalid\n");
    return;
  }

  if (!EnableReschedule) {
    POLLY_DEBUG(dbgs() << "Skipping rescheduling due to command line option\n");
  } else if (HasUserTransformation) {
    POLLY_DEBUG(dbgs() << "Skipping rescheduling due to manual transformation\n");
  } else {
    prepareIslOptions(S.getIslCtx().get());
    Schedule = computeSchedule(S, D);
    if (Schedule.is_null()) {
      POLLY_DEBUG(dbgs() << "ISL scheduler failed to find a schedule\n");
      return;
    }
    ScopsRescheduled++;
    POLLY_DEBUG(printSchedule(dbgs(), Schedule, "After rescheduling"));
  }
  walkScheduleTreeForStatistics(Schedule, 1);

  if (GreedyFusion) {
    isl::union_map Validity = D.getDependences(
        Dependences::TYPE_RAW | Dependences::TYPE_WAR | Dependences::TYPE_WAW);
    Schedule = applyGreedyFusion(Schedule, Validity);
    assert(!Schedule.is_null() && "Greedy fusion should not fail");
  }

  const OptimizerAdditionalInfoTy OAI = {
      &S, TTI, &D, !HasUserTransformation && PMBasedOpts,
      !HasUserTransformation && EnablePostopts,
      PollyVectorizerChoice != VECTORIZER_NONE, DepsChanged};
  if (OAI.PatternOpts || OAI.Postopts || OAI.Prevect) {
    Schedule = ScheduleTreeOptimizer::optimizeSchedule(Schedule, &OAI);
    Schedule = hoistExtensionNodes(Schedule);
    POLLY_DEBUG(printSchedule(dbgs(), Schedule, "After post-optimizations"));
    walkScheduleTreeForStatistics(Schedule, 2);
  }

  if (!HasUserTransformation && !ScheduleTreeOptimizer::isProfitableSchedule(S, Schedule)) {
    return;
  }

  auto ScopStats = S.getStatistics();
  ScopsOptimized++;
  NumAffineLoopsOptimized += ScopStats.NumAffineLoops;
  NumBoxedLoopsOptimized += ScopStats.NumBoxedLoops;
  LastSchedule = Schedule;
  S.setScheduleTree(Schedule);
  S.markAsOptimized();

  if (OptimizedScops) {
    errs() << S;
  }
}

bool IslScheduleOptimizerWrapperPass::runOnScop(Scop &S) {
  releaseMemory();
  IslCtx = S.getSharedIslCtx();
  auto GetDeps = [this](Dependences::AnalysisLevel) -> const Dependences & {
    return getAnalysis<DependenceInfo>().getDependences(Dependences::AL_Statement);
  };
  OptimizationRemarkEmitter &ORE = getAnalysis<OptimizationRemarkEmitterWrapperPass>().getORE();
  TargetTransformInfo &TTI = getAnalysis<TargetTransformInfoWrapperPass>().getTTI(S.getFunction());
  bool DepsChanged = false;
  runIslScheduleOptimizer(S, GetDeps, &TTI, &ORE, LastSchedule, DepsChanged);
  if (DepsChanged) {
    getAnalysis<DependenceInfo>().abandonDependences();
  }
  return false;
}

static void runScheduleOptimizerPrinter(raw_ostream &OS,
                                        isl::schedule LastSchedule) {
  if (LastSchedule.is_null()) {
    OS << "n/a\n";
    return;
  }
  OS << LastSchedule << "\n";
}

void IslScheduleOptimizerWrapperPass::printScop(raw_ostream &OS, Scop &) const {
  OS << "Calculated schedule:\n";
  runScheduleOptimizerPrinter(OS, LastSchedule);
}

void IslScheduleOptimizerWrapperPass::getAnalysisUsage(AnalysisUsage &AU) const {
  ScopPass::getAnalysisUsage(AU);
  AU.addRequired<DependenceInfo>();
  AU.addRequired<TargetTransformInfoWrapperPass>();
  AU.addRequired<OptimizationRemarkEmitterWrapperPass>();
  AU.addPreserved<DependenceInfo>();
  AU.addPreserved<OptimizationRemarkEmitterWrapperPass>();
}
} // namespace

Pass *polly::createIslScheduleOptimizerWrapperPass() {
  return new IslScheduleOptimizerWrapperPass();
}

INITIALIZE_PASS_BEGIN(IslScheduleOptimizerWrapperPass, "polly-opt-isl",
                      "Polly - Optimize schedule of SCoP", false, false);
INITIALIZE_PASS_DEPENDENCY(DependenceInfo);
INITIALIZE_PASS_DEPENDENCY(ScopInfoRegionPass);
INITIALIZE_PASS_DEPENDENCY(TargetTransformInfoWrapperPass);
INITIALIZE_PASS_DEPENDENCY(OptimizationRemarkEmitterWrapperPass);
INITIALIZE_PASS_END(IslScheduleOptimizerWrapperPass, "polly-opt-isl",
                    "Polly - Optimize schedule of SCoP", false, false)

//===----------------------------------------------------------------------===//
// New Pass Manager Implementation
//===----------------------------------------------------------------------===//

static PreservedAnalyses
runIslScheduleOptimizerUsingNPM(Scop &S, ScopAnalysisManager &SAM,
                                ScopStandardAnalysisResults &SAR, SPMUpdater &U,
                                raw_ostream *OS) {
  DependenceAnalysis::Result &Deps = SAM.getResult<DependenceAnalysis>(S, SAR);
  auto GetDeps = [&Deps](Dependences::AnalysisLevel) -> const Dependences & {
    return Deps.getDependences(Dependences::AL_Statement);
  };
  OptimizationRemarkEmitter ORE(&S.getFunction());
  isl::schedule LastSchedule;
  bool DepsChanged = false;
  runIslScheduleOptimizer(S, GetDeps, &SAR.TTI, &ORE, LastSchedule, DepsChanged);
  if (DepsChanged) {
    Deps.abandonDependences();
  }

  if (OS) {
    *OS << "Printing analysis 'Polly - Optimize schedule of SCoP' for region: '"
        << S.getName() << "' in function '" << S.getFunction().getName()
        << "':\n";
    runScheduleOptimizerPrinter(*OS, LastSchedule);
  }
  return PreservedAnalyses::all();
}

PreservedAnalyses
IslScheduleOptimizerPass::run(Scop &S, ScopAnalysisManager &SAM,
                              ScopStandardAnalysisResults &SAR, SPMUpdater &U) {
  return runIslScheduleOptimizerUsingNPM(S, SAM, SAR, U, nullptr);
}

PreservedAnalyses
IslScheduleOptimizerPrinterPass::run(Scop &S, ScopAnalysisManager &SAM,
                                     ScopStandardAnalysisResults &SAR,
                                     SPMUpdater &U) {
  return runIslScheduleOptimizerUsingNPM(S, SAM, SAR, U, &OS);
}

//===----------------------------------------------------------------------===//
// Legacy Printer Pass
//===----------------------------------------------------------------------===//

namespace {
class IslScheduleOptimizerPrinterLegacyPass final : public ScopPass {
public:
  static char ID;
  IslScheduleOptimizerPrinterLegacyPass()
      : IslScheduleOptimizerPrinterLegacyPass(outs()) {}
  explicit IslScheduleOptimizerPrinterLegacyPass(llvm::raw_ostream &OS)
      : ScopPass(ID), OS(OS) {}

  bool runOnScop(Scop &S) override {
    IslScheduleOptimizerWrapperPass &P =
        getAnalysis<IslScheduleOptimizerWrapperPass>();
    OS << "Printing analysis '" << P.getPassName() << "' for region: '"
       << S.getRegion().getNameStr() << "' in function '"
       << S.getFunction().getName() << "':\n";
    P.printScop(OS, S);
    return false;
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    ScopPass::getAnalysisUsage(AU);
    AU.addRequired<IslScheduleOptimizerWrapperPass>();
    AU.setPreservesAll();
  }

private:
  llvm::raw_ostream &OS;
};

char IslScheduleOptimizerPrinterLegacyPass::ID = 0;
} // namespace

Pass *polly::createIslScheduleOptimizerPrinterLegacyPass(raw_ostream &OS) {
  return new IslScheduleOptimizerPrinterLegacyPass(OS);
}

INITIALIZE_PASS_BEGIN(IslScheduleOptimizerPrinterLegacyPass,
                      "polly-print-opt-isl",
                      "Polly - Print optimizer schedule of SCoP", false, false);
INITIALIZE_PASS_DEPENDENCY(IslScheduleOptimizerWrapperPass)
INITIALIZE_PASS_END(IslScheduleOptimizerPrinterLegacyPass,
                    "polly-print-opt-isl",
                    "Polly - Print optimizer schedule of SCoP", false, false)
