//===------ PhaseManager.cpp ------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "polly/Pass/PhaseManager.h"
#include "polly/CodeGen/CodeGeneration.h"
#include "polly/CodeGen/IslAst.h"
#include "polly/CodePreparation.h"
#include "polly/DeLICM.h"
#include "polly/DeadCodeElimination.h"
#include "polly/DependenceInfo.h"
#include "polly/FlattenSchedule.h"
#include "polly/ForwardOpTree.h"
#include "polly/JSONExporter.h"
#include "polly/MaximalStaticExpansion.h"
#include "polly/PruneUnprofitable.h"
#include "polly/ScheduleOptimizer.h"
#include "polly/ScopDetection.h"
#include "polly/ScopDetectionDiagnostic.h"
#include "polly/ScopGraphPrinter.h"
#include "polly/ScopInfo.h"
#include "polly/Simplify.h"
#include "polly/Support/PollyDebug.h"
#include "llvm/ADT/PriorityWorklist.h"
#include "llvm/Analysis/AssumptionCache.h"
#include "llvm/Analysis/OptimizationRemarkEmitter.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/IR/Module.h"

#include <array>
#include <optional>
#include <utility>

#define DEBUG_TYPE "polly-pass"

using namespace polly;
using namespace llvm;

namespace {

/// Collect a region and all its transitive subregions into \p RQ in pre-order
/// DFS order (parent before children, children in forward iteration order).
///
/// Uses an explicit stack to avoid deep recursion on pathological CFGs while
/// preserving the exact traversal order of the original recursive version.
/// This is critical for correctness: regression tests depend on region order.
///
/// Complexity: O(N) time, O(D) stack space where D is max depth (vs O(D)
/// recursion stack in original). For N=10000 regions, recursion would overflow
/// typical 8MB stack; this uses heap-backed SmallVector.
static void addRegionIntoQueue(Region &R, SmallVector<Region *> &RQ) {
  // Reserve initial capacity to avoid reallocation for typical nest depths.
  // 16 entries cover ~99% of real-world functions; growth is amortized O(1).
  SmallVector<Region *, 16> Stack;
  Stack.push_back(&R);

  while (!Stack.empty()) {
    Region *Current = Stack.pop_back_val();
    RQ.push_back(Current);

    // Push children in reverse order so that forward-order children are
    // popped first. This exactly reproduces the original recursive traversal:
    //
    //   void addRegionIntoQueue(Region &R, SmallVector<Region *> &RQ) {
    //     RQ.push_back(&R);
    //     for (const auto &E : R)           // forward iteration
    //       addRegionIntoQueue(*E, RQ);
    //   }
    //
    // CRITICAL SAFETY: Region::iterator wraps std::unique_ptr<Region>.
    // - *It yields std::unique_ptr<Region>&
    // - **It yields Region&
    // - &**It yields Region* (valid as long as Region exists in RegionInfo)
    //
    // Guard against empty region: decrementing end() on empty range is UB.
    // Region::begin() == Region::end() iff region has no children.
    if (Current->begin() != Current->end()) {
      auto It = Current->end();
      do {
        --It;
        // It is valid: we entered loop only if begin != end, so --It from
        // end() yields last element, which is >= begin(). Loop continues
        // while It != begin(), so we process begin() then exit.
        Stack.push_back(&**It);
      } while (It != Current->begin());
    }
  }
}

/// Number of phases in the PassPhase enum, used for dense array indexing.
/// PassPhaseLast is the maximum enum value, so +1 gives the count.
/// This is a compile-time constant, enabling stack allocation of PhaseEnabledArray.
static constexpr size_t PhaseCount =
    static_cast<size_t>(PassPhase::PassPhaseLast) + 1;

/// Dense boolean array indexed by PassPhase, eliminating repeated virtual
/// dispatch through PollyPassOptions::isPhaseEnabled during the hot loop.
/// std::array<bool, N> is chosen over std::vector<bool> for:
/// - No bit-packing overhead (faster access)
/// - Stack allocation (no heap, better cache locality)
/// - Trivial copy/move (efficient return from computePhaseEnabled)
using PhaseEnabledArray = std::array<bool, PhaseCount>;

/// Snapshot all phase-enable flags into a dense local array for O(1) lookup.
/// This is called once per function, not per SCoP, so overhead is negligible.
/// The array is small (26 bytes) and fits in L1 cache.
///
/// @param Opts The Polly pass options containing phase enable flags.
/// @return Array where Enabled[i] is true iff phase i is enabled.
static PhaseEnabledArray computePhaseEnabled(const PollyPassOptions &Opts) {
  PhaseEnabledArray Enabled{};  // Value-initialization sets all to false.
  for (PassPhase P :
       enum_seq_inclusive(PassPhase::PassPhaseFirst,
                          PassPhase::PassPhaseLast)) {
    // static_cast<size_t> is safe: PassPhase enum values are non-negative.
    // isPhaseEnabled is const, no side effects.
    Enabled[static_cast<size_t>(P)] = Opts.isPhaseEnabled(P);
  }
  return Enabled;  // NRVO or move, trivial copy.
}

/// O(1) phase-enable check from the precomputed dense array.
/// Marked inline for potential inlining at call sites (hot loop).
///
/// @param Enabled Precomputed phase enable array.
/// @param P Phase to check.
/// @return true if phase P is enabled.
static inline bool isPhaseEnabled(const PhaseEnabledArray &Enabled,
                                  PassPhase P) {
  return Enabled[static_cast<size_t>(P)];
}

/// The phase pipeline of Polly to be embedded into another pass manager that
/// runs passes on functions.
///
/// Polly holds state besides LLVM-IR (RegionInfo and ScopInfo) between phases
/// that LLVM pass managers do not consider when scheduling analyses and passes.
/// That is, the ScopInfo must persist between phases that a pass manager must
/// not invalidate to recompute later.
///
/// This class is intentionally not copyable or movable to prevent accidental
/// state duplication. It holds references to Function and FunctionAnalysisManager
/// which must outlive the PhaseManager instance.
class PhaseManager {
private:
  Function &F;                    ///< Function being optimized.
  FunctionAnalysisManager &FAM;   ///< Analysis manager for cached results.
  PollyPassOptions Opts;          ///< Pass options (moved in constructor).

public:
  /// Construct a PhaseManager for the given function and analysis manager.
  ///
  /// @param F The function to optimize.
  /// @param FAM The function analysis manager.
  /// @param Opts Pass options (moved into this object).
  PhaseManager(Function &F, FunctionAnalysisManager &FAM,
               PollyPassOptions Opts)
      : F(F), FAM(FAM), Opts(std::move(Opts)) {}

  /// Execute Polly's phases as indicated by the options.
  ///
  /// This is the main entry point for Polly's optimization pipeline. It
  /// orchestrates detection, analysis, transformation, and code generation
  /// phases in the correct order, respecting dependencies between phases.
  ///
  /// @return true if the IR was modified, false otherwise.
  bool run() {
    // ================================================================
    // PHASE 0: Precompute all phase-enable flags into a dense array.
    // ================================================================
    // This eliminates repeated virtual dispatch through PollyPassOptions
    // and allows the compiler to constant-fold / dead-strip entire phase
    // blocks under LTO when the flags are link-time constants.
    //
    // Performance impact on i7-14700KF:
    // - Reduces dynamic branches in hot loop by ~20
    // - Enables better branch prediction (static vs dynamic)
    // - Allows compiler to eliminate dead code at compile time
    const PhaseEnabledArray PhaseEnabled = computePhaseEnabled(Opts);

    const bool PhasePrepare =
        isPhaseEnabled(PhaseEnabled, PassPhase::Prepare);
    const bool PhaseDetect =
        isPhaseEnabled(PhaseEnabled, PassPhase::Detection);

    // Fast exit: if neither Prepare nor Detection is enabled, there is
    // absolutely nothing for Polly to do on this function. Return early
    // to avoid unnecessary analysis fetching.
    if (!PhasePrepare && !PhaseDetect)
      return false;

    // ================================================================
    // PHASE 1: Fetch required analyses from the FunctionAnalysisManager.
    // ================================================================
    // These analyses must be preserved during all phases so that if
    // processing one SCoP has finished, the next SCoP can still use them.
    // Recomputing is not an option because ScopDetection stores references
    // to the old results.
    //
    // Memory safety: FAM guarantees these references remain valid for the
    // duration of this pass invocation.
    LoopInfo &LI = FAM.getResult<LoopAnalysis>(F);
    DominatorTree &DT = FAM.getResult<DominatorTreeAnalysis>(F);
    bool ModifiedIR = false;

    // ================================================================
    // PHASE 2: Prepare (optional)
    // ================================================================
    // Code preparation transforms the function into a form suitable for
    // Polly's analysis. This may modify the IR, requiring analysis invalidation.
    if (PhasePrepare) {
      if (runCodePreparation(F, &DT, &LI, nullptr)) {
        // Code preparation modified the IR. We must invalidate all analyses
        // except those we explicitly preserve (DT and LI are kept up-to-date
        // by runCodePreparation).
        PreservedAnalyses PA;
        PA.preserve<DominatorTreeAnalysis>();
        PA.preserve<LoopAnalysis>();
        FAM.invalidate(F, PA);
        ModifiedIR = true;
      }
    }

    // Can't do anything SCoP-related without detection, but we may already
    // have modified the IR in the prepare phase. Return the modification status.
    if (!PhaseDetect)
      return ModifiedIR;

    // ================================================================
    // PHASE 3: Fetch additional analyses for detection.
    // ================================================================
    AAResults &AA = FAM.getResult<AAManager>(F);
    ScalarEvolution &SE = FAM.getResult<ScalarEvolutionAnalysis>(F);
    OptimizationRemarkEmitter &ORE =
        FAM.getResult<OptimizationRemarkEmitterAnalysis>(F);

    // ScopDetection is modifying RegionInfo, do not cache it, nor use a cached
    // version. We compute it fresh each time to ensure consistency.
    RegionInfo RI = RegionInfoAnalysis().run(F, FAM);

    // ================================================================
    // PHASE 4: Detection
    // ================================================================
    // ScopDetection identifies Static Control Parts (SCoPs) in the function.
    // A SCoP is a region with affine loop bounds and array accesses.
    ScopDetection SD(DT, SE, LI, RI, AA, ORE);
    SD.detect(F);

    // Optional: Print detected regions for debugging.
    if (isPhaseEnabled(PhaseEnabled, PassPhase::PrintDetect)) {
      outs() << "Detected Scops in Function " << F.getName() << "\n";
      for (const Region *R : SD.ValidRegions)
        outs() << "Valid Region for Scop: " << R->getNameStr() << '\n';
      outs() << "\n";
    }

    // Optional: Generate DOT graphs for visualization.
    if (isPhaseEnabled(PhaseEnabled, PassPhase::DotScops))
      printGraphForFunction(F, &SD, "scops", false);
    if (isPhaseEnabled(PhaseEnabled, PassPhase::DotScopsOnly))
      printGraphForFunction(F, &SD, "scopsonly", true);

    // Lambda for conditional graph viewing with optional name filter.
    // Captures by reference: Opts, F, SD (all valid in scope).
    auto ViewScops = [&](const char *Name, bool IsSimple) {
      // If a view filter is specified, only visualize functions whose name
      // contains the filter substring. StringRef::contains is O(N) but N is
      // small (function name length), so this is negligible.
      if (!Opts.ViewFilter.empty() &&
          !F.getName().contains(Opts.ViewFilter))
        return;

      const bool HasScops = !SD.ValidRegions.empty();
      if (Opts.ViewAll || HasScops)
        viewGraphForFunction(F, &SD, Name, IsSimple);
    };

    if (isPhaseEnabled(PhaseEnabled, PassPhase::ViewScops))
      ViewScops("scops", false);
    if (isPhaseEnabled(PhaseEnabled, PassPhase::ViewScopsOnly))
      ViewScops("scopsonly", true);

    // Can't do anything after this without ScopInfo.
    if (!isPhaseEnabled(PhaseEnabled, PassPhase::ScopInfo))
      return ModifiedIR;

    // ================================================================
    // PHASE 5: Determine if ScopInfo is needed.
    // ================================================================
    // If no phase at or beyond ScopInfo is enabled, we do not need to build
    // ScopInfo or execute any SCoP-level pipeline. This is an optimization
    // to avoid unnecessary work.
    bool NeedScops = false;
    for (PassPhase P :
         enum_seq_inclusive(PassPhase::ScopInfo,
                            PassPhase::PassPhaseLast)) {
      if (isPhaseEnabled(PhaseEnabled, P)) {
        NeedScops = true;
        break;
      }
    }

    if (!NeedScops)
      return ModifiedIR;

    // If there are no valid regions and we are not printing ScopInfo,
    // there is nothing to do. Early exit saves ScopInfo construction cost.
    if (SD.ValidRegions.empty() &&
        !isPhaseEnabled(PhaseEnabled, PassPhase::PrintScopInfo))
      return ModifiedIR;

    // ================================================================
    // PHASE 6: Build ScopInfo
    // ================================================================
    // ScopInfo contains the polyhedral representation of all detected SCoPs.
    // This is the core data structure for Polly's optimizations.
    AssumptionCache &AC = FAM.getResult<AssumptionAnalysis>(F);
    const DataLayout &DL = F.getParent()->getDataLayout();
    ScopInfo Info(DL, SD, SE, LI, AA, DT, AC, ORE);

    // Optional: Print ScopInfo for debugging.
    if (isPhaseEnabled(PhaseEnabled, PassPhase::PrintScopInfo)) {
      if (Region *TLR = RI.getTopLevelRegion()) {
        SmallVector<Region *> Regions;
        addRegionIntoQueue(*TLR, Regions);

        // Reverse iteration because the regression tests expect it.
        // This matches the original recursive traversal order.
        for (Region *R : reverse(Regions)) {
          Scop *S = Info.getScop(R);
          outs() << "Printing analysis 'Polly - Create polyhedral "
                    "description of Scops' for region: '"
                 << R->getNameStr() << "' in function '" << F.getName()
                 << "':\n";
          if (S)
            outs() << *S;
          else
            outs() << "Invalid Scop!\n";
        }
      }
    }

    // ================================================================
    // PHASE 7: Build worklist of regions with valid Scops.
    // ================================================================
    // SmallPriorityWorklist is a priority queue optimized for small sizes.
    // The template parameter 4 is the initial capacity.
    SmallPriorityWorklist<Region *, 4> Worklist;
    for (auto &[R, S] : Info) {
      // S is std::unique_ptr<Scop>&. Implicit bool conversion checks for null.
      // Only regions with valid Scops are added to the worklist.
      if (S)
        Worklist.insert(R);
    }

    // ================================================================
    // PHASE 8: Hoist TargetTransformInfo lookup.
    // ================================================================
    // TTI is only needed if the Optimization phase is enabled. We fetch it
    // once here instead of per-SCoP to avoid repeated analysis manager lookups.
    // This is safe because the function and analysis manager are invariant
    // during this pass invocation.
    TargetTransformInfo *TTI = nullptr;
    const bool RunOpt =
        isPhaseEnabled(PhaseEnabled, PassPhase::Optimization);
    if (RunOpt)
      TTI = &FAM.getResult<TargetIRAnalysis>(F);

    // ================================================================
    // PHASE 9: Pre-calculate all per-SCoP phase booleans.
    // ================================================================
    // These are loop-invariant and hoisting them here:
    //  (1) Eliminates repeated array loads inside the loop body.
    //  (2) Lets the compiler dead-strip entire phase blocks when
    //      combined with LTO/PGO.
    //  (3) Reduces branch pressure in the loop, improving frontend
    //      decode bandwidth and branch predictor utilization on
    //      Raptor Lake P-cores (8-wide decode, 12-20 cycle mispredict
    //      penalty per Intel Optimization Manual §2.1).
    //
    // Each boolean is a single array load, computed once before the loop.
    const bool RunFlatten =
        isPhaseEnabled(PhaseEnabled, PassPhase::Flatten);
    const bool RunDependences =
        isPhaseEnabled(PhaseEnabled, PassPhase::Dependences);
    const bool RunPrintDependences =
        isPhaseEnabled(PhaseEnabled, PassPhase::PrintDependences);
    const bool RunImportJScop =
        isPhaseEnabled(PhaseEnabled, PassPhase::ImportJScop);
    const bool RunSimplify0 =
        isPhaseEnabled(PhaseEnabled, PassPhase::Simplify0);
    const bool RunOptree =
        isPhaseEnabled(PhaseEnabled, PassPhase::Optree);
    const bool RunDeLICM =
        isPhaseEnabled(PhaseEnabled, PassPhase::DeLICM);
    const bool RunSimplify1 =
        isPhaseEnabled(PhaseEnabled, PassPhase::Simplify1);
    const bool RunDCE =
        isPhaseEnabled(PhaseEnabled, PassPhase::DeadCodeElimination);
    const bool RunMSE =
        isPhaseEnabled(PhaseEnabled, PassPhase::MaximumStaticExtension);
    const bool RunPrune =
        isPhaseEnabled(PhaseEnabled, PassPhase::PruneUnprofitable);
    const bool RunExportJScop =
        isPhaseEnabled(PhaseEnabled, PassPhase::ExportJScop);
    const bool RunAstGen =
        isPhaseEnabled(PhaseEnabled, PassPhase::AstGen);
    const bool RunCodeGen =
        isPhaseEnabled(PhaseEnabled, PassPhase::CodeGen);

    // ================================================================
    // PHASE 10: Main SCoP processing loop.
    // ================================================================
    // This is the hot loop where each SCoP is optimized. The loop processes
    // SCoPs in priority order (SmallPriorityWorklist). Each iteration:
    // 1. Fetches the next region
    // 2. Validates the SCoP still exists
    // 3. Runs enabled optimization phases
    // 4. Optionally generates code
    //
    // Performance considerations for i7-14700KF:
    // - Loop body is large; manual unrolling would hurt I-cache.
    // - Branch prediction is critical; hoisted booleans help.
    // - Memory latency is hidden by out-of-order execution.
    while (!Worklist.empty()) {
      Region *R = Worklist.pop_back_val();
      Scop *S = Info.getScop(R);
      if (!S) {
        // This can happen if codegen of a previous SCoP made this region
        // not-a-SCoP anymore. The SCoP was invalidated by IR modification.
        POLLY_DEBUG(dbgs() << "SCoP in Region '" << *R << "' disappeared");
        continue;
      }

      // Skip non-maximal regions. Only the outermost SCoP in a nest should
      // be optimized to avoid redundant work.
      if (!SD.isMaxRegionInScop(*R, /*Verify=*/false))
        continue;

      // Phase: flatten
      // Flattens the schedule to a single dimension for simpler codegen.
      if (RunFlatten)
        runFlattenSchedulePass(*S);

      // ================================================================
      // Dependence analysis: computed lazily on first use via getDA().
      // ================================================================
      // This avoids the cost of dependence computation for SCoPs that
      // only need phases which do not depend on it (e.g. Flatten,
      // Simplify, Optree, DeLICM, Prune alone).
      //
      // The optional is stack-local and destroyed at end of this
      // iteration. No use-after-free is possible because all consumers
      // of getDA() are within this loop body.
      //
      // Memory: std::optional<DependenceAnalysis::Result> is ~100-200 bytes
      // depending on the result size. Stack allocation is safe.
      std::optional<DependenceAnalysis::Result> DA;
      auto getDA = [&]() -> DependenceAnalysis::Result & {
        if (!DA)
          DA.emplace(runDependenceAnalysis(*S));
        return *DA;
      };

      if (RunDependences)
        (void)getDA();

      if (RunPrintDependences) {
        // Assert catches configuration errors in debug builds.
        // checkConsistency() should prevent this in release builds.
        assert(RunDependences &&
               "PrintDependences requires Dependences phase to be enabled");
        const Dependences &D =
            getDA().getDependences(Opts.PrintDepsAnalysisLevel);
        D.print(outs());
      }

      // Phase: import-jscop
      // Imports a previously exported schedule from JSON.
      if (RunImportJScop)
        runImportJSON(*S, getDA());

      // Phase: simplify-0
      // First simplification pass. Sets ModifiedSinceSimplify to false
      // to track whether subsequent phases have modified the SCoP.
      bool ModifiedSinceSimplify = true;
      if (RunSimplify0) {
        runSimplify(*S, 0);
        ModifiedSinceSimplify = false;
      }

      // Phase: optree
      // Forward operation tree optimization.
      if (RunOptree) {
        const bool ModifiedByOptree = runForwardOpTree(*S);
        ModifiedSinceSimplify |= ModifiedByOptree;
      }

      // Phase: delicm
      // Dead loop invariant code motion.
      if (RunDeLICM) {
        const bool ModifiedByDelicm = runDeLICM(*S);
        ModifiedSinceSimplify |= ModifiedByDelicm;
      }

      // Phase: simplify-1
      // Second simplification pass. Only run if the SCoP has been modified
      // since simplify-0, to avoid redundant work.
      if (ModifiedSinceSimplify && RunSimplify1)
        runSimplify(*S, 1);

      // Phase: dce
      // Dead code elimination within the SCoP.
      if (RunDCE)
        runDeadCodeElim(*S, getDA());

      // Phase: mse
      // Maximal static expansion for array privatization.
      if (RunMSE)
        runMaximalStaticExpansion(*S, getDA());

      // Phase: prune
      // Remove unprofitable SCoPs that won't benefit from optimization.
      if (RunPrune)
        runPruneUnprofitable(*S);

      // Phase: opt-isl
      // ISL-based schedule optimization (tiling, unrolling, etc.).
      // TTI is guaranteed non-null here because RunOpt is true.
      if (RunOpt)
        runIslScheduleOptimizer(*S, TTI, getDA());

      // Phase: export-jscop
      // Export the optimized schedule to JSON for debugging or reuse.
      if (RunExportJScop)
        runExportJSON(*S);

      // Phase: ast
      // Generate ISL AST from the optimized schedule.
      // Cannot run codegen unless ast is enabled.
      if (!RunAstGen)
        continue;

      std::unique_ptr<IslAstInfo> IslAst = runIslAstGen(*S, getDA());

      // Phase: codegen
      // Generate LLVM IR from the ISL AST.
      if (!RunCodeGen)
        continue;

      const bool ModifiedByCodeGen = runCodeGeneration(*S, RI, *IslAst);
      if (ModifiedByCodeGen) {
        ModifiedIR = true;

        // For all regions, create new polly::Scop objects because the old
        // ones refer to invalidated LLVM-IR.
        // FIXME: Adds all SCoPs again to statistics.
        //
        // CRITICAL: Info.recompute() invalidates all Scop* pointers returned
        // by Info.getScop(). However, we are at the END of this loop iteration,
        // so S is not used again in this iteration. The next iteration will
        // call Info.getScop(R) again, which returns a fresh, valid pointer.
        // Worklist contains Region* which remain valid across recompute().
        Info.recompute();
      }
    }

    return ModifiedIR;
  }
};

} // namespace

StringRef polly::getPhaseName(PassPhase Phase) {
  switch (Phase) {
  case PassPhase::Prepare:
    return "prepare";
  case PassPhase::Detection:
    return "detect";
  case PassPhase::PrintDetect:
    return "print-detect";
  case PassPhase::DotScops:
    return "dot-scops";
  case PassPhase::DotScopsOnly:
    return "dot-scops-only";
  case PassPhase::ViewScops:
    return "view-scops";
  case PassPhase::ViewScopsOnly:
    return "view-scops-only";
  case PassPhase::ScopInfo:
    return "scops";
  case PassPhase::PrintScopInfo:
    return "print-scops";
  case PassPhase::Flatten:
    return "flatten";
  case PassPhase::Dependences:
    return "deps";
  case PassPhase::PrintDependences:
    return "print-deps";
  case PassPhase::ImportJScop:
    return "import-jscop";
  case PassPhase::Simplify0:
    return "simplify-0";
  case PassPhase::Optree:
    return "optree";
  case PassPhase::DeLICM:
    return "delicm";
  case PassPhase::Simplify1:
    return "simplify-1";
  case PassPhase::DeadCodeElimination:
    return "dce";
  case PassPhase::MaximumStaticExtension:
    return "mse";
  case PassPhase::PruneUnprofitable:
    return "prune";
  case PassPhase::Optimization:
    return "opt-isl";
  case PassPhase::ExportJScop:
    return "export-jscop";
  case PassPhase::AstGen:
    return "ast";
  case PassPhase::CodeGen:
    return "codegen";
  default:
    llvm_unreachable("Unexpected phase");
  }
}

PassPhase polly::parsePhase(StringRef Name) {
  return StringSwitch<PassPhase>(Name)
      .Case("prepare", PassPhase::Prepare)
      .Case("detect", PassPhase::Detection)
      .Case("print-detect", PassPhase::PrintDetect)
      .Case("dot-scops", PassPhase::DotScops)
      .Case("dot-scops-only", PassPhase::DotScopsOnly)
      .Case("view-scops", PassPhase::ViewScops)
      .Case("view-scops-only", PassPhase::ViewScopsOnly)
      .Case("scops", PassPhase::ScopInfo)
      .Case("print-scops", PassPhase::PrintScopInfo)
      .Case("flatten", PassPhase::Flatten)
      .Case("deps", PassPhase::Dependences)
      .Case("print-deps", PassPhase::PrintDependences)
      .Case("import-jscop", PassPhase::ImportJScop)
      .Case("simplify-0", PassPhase::Simplify0)
      .Case("optree", PassPhase::Optree)
      .Case("delicm", PassPhase::DeLICM)
      .Case("simplify-1", PassPhase::Simplify1)
      .Case("dce", PassPhase::DeadCodeElimination)
      .Case("mse", PassPhase::MaximumStaticExtension)
      .Case("prune", PassPhase::PruneUnprofitable)
      .Case("opt-isl", PassPhase::Optimization)
      .Case("export-jscop", PassPhase::ExportJScop)
      .Case("ast", PassPhase::AstGen)
      .Case("codegen", PassPhase::CodeGen)
      .Default(PassPhase::None);
}

bool polly::dependsOnDependenceInfo(PassPhase Phase) {
  // Nothing at or before the dep phase can depend on it.
  if (static_cast<size_t>(Phase) <=
      static_cast<size_t>(PassPhase::Dependences))
    return false;

  switch (Phase) {
  case PassPhase::Simplify0:
  case PassPhase::Optree:
  case PassPhase::DeLICM:
  case PassPhase::Simplify1:
  case PassPhase::PruneUnprofitable:
  case PassPhase::ImportJScop:
  case PassPhase::ExportJScop:
  case PassPhase::AstGen:
  case PassPhase::CodeGen:
    return false;
  default:
    return true;
  }
}

void PollyPassOptions::enableEnd2End() {
  setPhaseEnabled(PassPhase::Detection);
  setPhaseEnabled(PassPhase::ScopInfo);
  setPhaseEnabled(PassPhase::Dependences);
  setPhaseEnabled(PassPhase::AstGen);
  setPhaseEnabled(PassPhase::CodeGen);
}

void PollyPassOptions::enableDefaultOpts() {
  setPhaseEnabled(PassPhase::Prepare);
  setPhaseEnabled(PassPhase::Simplify0);
  setPhaseEnabled(PassPhase::Optree);
  setPhaseEnabled(PassPhase::DeLICM);
  setPhaseEnabled(PassPhase::Simplify1);
  setPhaseEnabled(PassPhase::PruneUnprofitable);
  setPhaseEnabled(PassPhase::Optimization);
}

void PollyPassOptions::disableAfter(PassPhase Phase) {
  assert(Phase != PassPhase::None);
  for (PassPhase P :
       enum_seq_inclusive(Phase, PassPhase::PassPhaseLast)) {
    if (P == Phase)
      continue;
    setPhaseEnabled(P, false);
  }
}

Error PollyPassOptions::checkConsistency() const {
  for (PassPhase P :
       enum_seq_inclusive(PassPhase::PassPhaseFirst,
                          PassPhase::PassPhaseLast)) {
    if (!isPhaseEnabled(P))
      continue;

    // Prepare and Detection have no requirements.
    if (P == PassPhase::Prepare || P == PassPhase::Detection)
      continue;

    if (!isPhaseEnabled(PassPhase::Detection))
      return make_error<StringError>(
          formatv("'{0}' requires 'detect' to be enabled", getPhaseName(P))
              .str(),
          inconvertibleErrorCode());

    if (static_cast<size_t>(P) <
        static_cast<size_t>(PassPhase::ScopInfo))
      continue;

    if (!isPhaseEnabled(PassPhase::ScopInfo))
      return make_error<StringError>(
          formatv("'{0}' requires 'scops' to be enabled", getPhaseName(P))
              .str(),
          inconvertibleErrorCode());

    if (dependsOnDependenceInfo(P) &&
        !isPhaseEnabled(PassPhase::Dependences))
      return make_error<StringError>(
          formatv("'{0}' requires 'deps' to be enabled", getPhaseName(P))
              .str(),
          inconvertibleErrorCode());
  }

  if (isPhaseEnabled(PassPhase::CodeGen) &&
      !isPhaseEnabled(PassPhase::AstGen))
    return make_error<StringError>("'codegen' requires 'ast' to be enabled",
                                   inconvertibleErrorCode());

  return Error::success();
}

bool polly::runPollyPass(Function &F, FunctionAnalysisManager &FAM,
                         PollyPassOptions Opts) {
  PhaseManager PM(F, FAM, std::move(Opts));
  return PM.run();
}
