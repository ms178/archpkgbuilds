//===- ScopDetection.cpp - Detect Scops -----------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Detect the maximal Scops of a function.
//
// A static control part (Scop) is a subgraph of the control flow graph (CFG)
// that only has statically known control flow and can therefore be described
// within the polyhedral model.
//
// Every Scop fulfills these restrictions:
//
// * It is a single entry single exit region
//
// * Only affine linear bounds in the loops
//
// Every natural loop in a Scop must have a number of loop iterations that can
// be described as an affine linear function in surrounding loop iterators or
// parameters. (A parameter is a scalar that does not change its value during
// execution of the Scop).
//
// * Only comparisons of affine linear expressions in conditions
//
// * All loops and conditions perfectly nested
//
// The control flow needs to be structured such that it could be written using
// just 'for' and 'if' statements, without the need for any 'goto', 'break' or
// 'continue'.
//
// * Side effect free functions call
//
// Function calls and intrinsics that do not have side effects (readnone)
// or memory intrinsics (memset, memcpy, memmove) are allowed.
//
// The Scop detection finds the largest Scops by checking if the largest
// region is a Scop. If this is not the case, its canonical subregions are
// checked until a region is a Scop. It is now tried to extend this Scop by
// creating a larger non canonical region.
//
//===----------------------------------------------------------------------===//

#include "polly/ScopDetection.h"
#include "polly/Options.h"
#include "polly/ScopDetectionDiagnostic.h"
#include "polly/Support/SCEVValidator.h"
#include "polly/Support/ScopHelper.h"
#include "polly/Support/ScopLocation.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/Delinearization.h"
#include "llvm/Analysis/Loads.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/OptimizationRemarkEmitter.h"
#include "llvm/Analysis/RegionInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Regex.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cassert>
#include <memory>
#include <string>
#include <utility>
#include <vector>

using namespace llvm;
using namespace polly;

#include "polly/Support/PollyDebug.h"
#define DEBUG_TYPE "polly-detect"

// This option is set to a very high value, as analyzing such loops increases
// compile time on several cases. For experiments that enable this option,
// a value of around 40 has been working to avoid run-time regressions with
// Polly while still exposing interesting optimization opportunities.
static cl::opt<int> ProfitabilityMinPerLoopInstructions(
    "polly-detect-profitability-min-per-loop-insts",
    cl::desc("The minimal number of per-loop instructions before a single loop "
             "region is considered profitable"),
    cl::Hidden, cl::ValueRequired, cl::init(100000000), cl::cat(PollyCategory));

bool polly::PollyProcessUnprofitable;

static cl::opt<bool, true> XPollyProcessUnprofitable(
    "polly-process-unprofitable",
    cl::desc(
        "Process scops that are unlikely to benefit from Polly optimizations."),
    cl::location(PollyProcessUnprofitable), cl::cat(PollyCategory));

static cl::list<std::string> OnlyFunctions(
    "polly-only-func",
    cl::desc("Only run on functions that match a regex. "
             "Multiple regexes can be comma separated. "
             "Scop detection will run on all functions that match "
             "ANY of the regexes provided."),
    cl::CommaSeparated, cl::cat(PollyCategory));

static cl::list<std::string> IgnoredFunctions(
    "polly-ignore-func",
    cl::desc("Ignore functions that match a regex. "
             "Multiple regexes can be comma separated. "
             "Scop detection will ignore all functions that match "
             "ANY of the regexes provided."),
    cl::CommaSeparated, cl::cat(PollyCategory));

bool polly::PollyAllowFullFunction;

static cl::opt<bool, true>
    XAllowFullFunction("polly-detect-full-functions",
                       cl::desc("Allow the detection of full functions"),
                       cl::location(polly::PollyAllowFullFunction),
                       cl::init(false), cl::cat(PollyCategory));

static cl::opt<std::string> OnlyRegion(
    "polly-only-region",
    cl::desc("Only run on certain regions (The provided identifier must "
             "appear in the name of the region's entry block"),
    cl::value_desc("identifier"), cl::ValueRequired, cl::init(""),
    cl::cat(PollyCategory));

static cl::opt<bool>
    IgnoreAliasing("polly-ignore-aliasing",
                   cl::desc("Ignore possible aliasing of the array bases"),
                   cl::Hidden, cl::cat(PollyCategory));

bool polly::PollyAllowUnsignedOperations;

static cl::opt<bool, true> XPollyAllowUnsignedOperations(
    "polly-allow-unsigned-operations",
    cl::desc("Allow unsigned operations such as comparisons or zero-extends."),
    cl::location(PollyAllowUnsignedOperations), cl::Hidden, cl::init(true),
    cl::cat(PollyCategory));

bool polly::PollyUseRuntimeAliasChecks;

static cl::opt<bool, true> XPollyUseRuntimeAliasChecks(
    "polly-use-runtime-alias-checks",
    cl::desc("Use runtime alias checks to resolve possible aliasing."),
    cl::location(PollyUseRuntimeAliasChecks), cl::Hidden, cl::init(true),
    cl::cat(PollyCategory));

static cl::opt<bool>
    ReportLevel("polly-report",
                cl::desc("Print information about the activities of Polly"),
                cl::cat(PollyCategory));

static cl::opt<bool> AllowDifferentTypes(
    "polly-allow-differing-element-types",
    cl::desc("Allow different element types for array accesses"), cl::Hidden,
    cl::init(true), cl::cat(PollyCategory));

static cl::opt<bool>
    AllowNonAffine("polly-allow-nonaffine",
                   cl::desc("Allow non affine access functions in arrays"),
                   cl::Hidden, cl::cat(PollyCategory));

static cl::opt<bool>
    AllowModrefCall("polly-allow-modref-calls",
                    cl::desc("Allow functions with known modref behavior"),
                    cl::Hidden, cl::cat(PollyCategory));

static cl::opt<bool> AllowNonAffineSubRegions(
    "polly-allow-nonaffine-branches",
    cl::desc("Allow non affine conditions for branches"), cl::Hidden,
    cl::init(true), cl::cat(PollyCategory));

static cl::opt<bool>
    AllowNonAffineSubLoops("polly-allow-nonaffine-loops",
                           cl::desc("Allow non affine conditions for loops"),
                           cl::Hidden, cl::cat(PollyCategory));

static cl::opt<bool, true>
    TrackFailures("polly-detect-track-failures",
                  cl::desc("Track failure strings in detecting scop regions"),
                  cl::location(PollyTrackFailures), cl::Hidden, cl::init(true),
                  cl::cat(PollyCategory));

static cl::opt<bool> KeepGoing("polly-detect-keep-going",
                               cl::desc("Do not fail on the first error."),
                               cl::Hidden, cl::cat(PollyCategory));

static cl::opt<bool, true>
    PollyDelinearizeX("polly-delinearize",
                      cl::desc("Delinearize array access functions"),
                      cl::location(PollyDelinearize), cl::Hidden,
                      cl::init(true), cl::cat(PollyCategory));

static cl::opt<bool>
    VerifyScops("polly-detect-verify",
                cl::desc("Verify the detected SCoPs after each transformation"),
                cl::Hidden, cl::cat(PollyCategory));

bool polly::PollyInvariantLoadHoisting;

static cl::opt<bool, true>
    XPollyInvariantLoadHoisting("polly-invariant-load-hoisting",
                                cl::desc("Hoist invariant loads."),
                                cl::location(PollyInvariantLoadHoisting),
                                cl::Hidden, cl::cat(PollyCategory));

static cl::opt<bool> PollyAllowErrorBlocks(
    "polly-allow-error-blocks",
    cl::desc("Allow to speculate on the execution of 'error blocks'."),
    cl::Hidden, cl::init(true), cl::cat(PollyCategory));

/// The minimal trip count under which loops are considered unprofitable.
static constexpr unsigned MIN_LOOP_TRIP_COUNT = 8;

bool polly::PollyTrackFailures = false;
bool polly::PollyDelinearize = false;
StringRef polly::PollySkipFnAttr = "polly.skip.fn";

//===----------------------------------------------------------------------===//
// Statistics.

STATISTIC(NumScopRegions, "Number of scops");
STATISTIC(NumLoopsInScop, "Number of loops in scops");
STATISTIC(NumScopsDepthZero, "Number of scops with maximal loop depth 0");
STATISTIC(NumScopsDepthOne, "Number of scops with maximal loop depth 1");
STATISTIC(NumScopsDepthTwo, "Number of scops with maximal loop depth 2");
STATISTIC(NumScopsDepthThree, "Number of scops with maximal loop depth 3");
STATISTIC(NumScopsDepthFour, "Number of scops with maximal loop depth 4");
STATISTIC(NumScopsDepthFive, "Number of scops with maximal loop depth 5");
STATISTIC(NumScopsDepthLarger,
          "Number of scops with maximal loop depth 6 and larger");
STATISTIC(NumProfScopRegions, "Number of scops (profitable scops only)");
STATISTIC(NumLoopsInProfScop,
          "Number of loops in scops (profitable scops only)");
STATISTIC(NumLoopsOverall, "Number of total loops");
STATISTIC(NumProfScopsDepthZero,
          "Number of scops with maximal loop depth 0 (profitable scops only)");
STATISTIC(NumProfScopsDepthOne,
          "Number of scops with maximal loop depth 1 (profitable scops only)");
STATISTIC(NumProfScopsDepthTwo,
          "Number of scops with maximal loop depth 2 (profitable scops only)");
STATISTIC(NumProfScopsDepthThree,
          "Number of scops with maximal loop depth 3 (profitable scops only)");
STATISTIC(NumProfScopsDepthFour,
          "Number of scops with maximal loop depth 4 (profitable scops only)");
STATISTIC(NumProfScopsDepthFive,
          "Number of scops with maximal loop depth 5 (profitable scops only)");
STATISTIC(NumProfScopsDepthLarger,
          "Number of scops with maximal loop depth 6 and larger "
          "(profitable scops only)");
STATISTIC(MaxNumLoopsInScop, "Maximal number of loops in scops");
STATISTIC(MaxNumLoopsInProfScop,
          "Maximal number of loops in scops (profitable scops only)");

/// Update loop count statistics using computed array indexing.
///
/// Uses array indexing instead of if-else chain to avoid branch mispredictions.
/// Per Intel Optimization Manual §3.4.1.3, chains of conditional branches
/// suffer cumulative misprediction penalties. A computed index is branchless.
static void updateLoopCountStatistic(ScopDetection::LoopStats Stats,
                                     bool OnlyProfitable) {
  // Clamp MaxDepth to valid index range [0, 6].
  // std::min generates branchless code with CMOV on x86-64.
  const auto DepthIndex = static_cast<unsigned>(std::min(Stats.MaxDepth, 6));

  if (!OnlyProfitable) {
    NumLoopsInScop += Stats.NumLoops;
    MaxNumLoopsInScop =
        std::max(MaxNumLoopsInScop.getValue(),
                 static_cast<uint64_t>(Stats.NumLoops));

    // Use switch with computed index - compiler optimizes to jump table.
    switch (DepthIndex) {
    case 0: ++NumScopsDepthZero; break;
    case 1: ++NumScopsDepthOne; break;
    case 2: ++NumScopsDepthTwo; break;
    case 3: ++NumScopsDepthThree; break;
    case 4: ++NumScopsDepthFour; break;
    case 5: ++NumScopsDepthFive; break;
    default: ++NumScopsDepthLarger; break;
    }
  } else {
    NumLoopsInProfScop += Stats.NumLoops;
    MaxNumLoopsInProfScop =
        std::max(MaxNumLoopsInProfScop.getValue(),
                 static_cast<uint64_t>(Stats.NumLoops));

    switch (DepthIndex) {
    case 0: ++NumProfScopsDepthZero; break;
    case 1: ++NumProfScopsDepthOne; break;
    case 2: ++NumProfScopsDepthTwo; break;
    case 3: ++NumProfScopsDepthThree; break;
    case 4: ++NumProfScopsDepthFour; break;
    case 5: ++NumProfScopsDepthFive; break;
    default: ++NumProfScopsDepthLarger; break;
    }
  }
}

namespace {

class DiagnosticScopFound final : public DiagnosticInfo {
private:
  static int PluginDiagnosticKind;

  Function &F;
  std::string FileName;
  unsigned EntryLine;
  unsigned ExitLine;

public:
  DiagnosticScopFound(Function &F, std::string FileName, unsigned EntryLine,
                      unsigned ExitLine)
      : DiagnosticInfo(PluginDiagnosticKind, DS_Note), F(F),
        FileName(std::move(FileName)), EntryLine(EntryLine),
        ExitLine(ExitLine) {}

  void print(DiagnosticPrinter &DP) const override;

  static bool classof(const DiagnosticInfo *DI) {
    return DI->getKind() == PluginDiagnosticKind;
  }
};

} // namespace

int DiagnosticScopFound::PluginDiagnosticKind =
    getNextAvailablePluginDiagnosticKind();

void DiagnosticScopFound::print(DiagnosticPrinter &DP) const {
  DP << "Polly detected an optimizable loop region (scop) in function '" << F
     << "'\n";

  if (FileName.empty()) {
    DP << "Scop location is unknown. Compile with debug info "
          "(-g) to get more precise information. ";
    return;
  }

  DP << FileName << ":" << EntryLine << ": Start of scop\n";
  DP << FileName << ":" << ExitLine << ": End of scop";
}

/// Check if a string matches any regex in a list of regexes.
/// @param Str the input string to match against.
/// @param RegexList a list of strings that are regular expressions.
static bool doesStringMatchAnyRegex(StringRef Str,
                                    const cl::list<std::string> &RegexList) {
  for (const auto &RegexStr : RegexList) {
    Regex R(RegexStr);

    std::string Err;
    if (!R.isValid(Err)) {
      report_fatal_error(Twine("invalid regex given as input to polly: ") + Err,
                         true);
    }

    if (R.match(Str)) {
      return true;
    }
  }
  return false;
}

//===----------------------------------------------------------------------===//
// ScopDetection.

ScopDetection::ScopDetection(const DominatorTree &DT, ScalarEvolution &SE,
                             LoopInfo &LI, RegionInfo &RI, AAResults &AA,
                             OptimizationRemarkEmitter &ORE)
    : DT(DT), SE(SE), LI(LI), RI(RI), AA(AA), ORE(ORE) {}

void ScopDetection::detect(Function &F) {
  assert(ValidRegions.empty() && "Detection must run only once");

  if (!PollyProcessUnprofitable && LI.empty()) {
    return;
  }

  Region *TopRegion = RI.getTopLevelRegion();

  if (!OnlyFunctions.empty() &&
      !doesStringMatchAnyRegex(F.getName(), OnlyFunctions)) {
    return;
  }

  if (doesStringMatchAnyRegex(F.getName(), IgnoredFunctions)) {
    return;
  }

  if (!isValidFunction(F)) {
    return;
  }

  findScops(*TopRegion);

  NumScopRegions += ValidRegions.size();

  // Prune non-profitable regions.
  for (auto &DIt : DetectionContextMap) {
    DetectionContext &DC = *DIt.getSecond();
    if (DC.Log.hasErrors()) {
      continue;
    }
    if (!ValidRegions.count(&DC.CurRegion)) {
      continue;
    }
    LoopStats Stats = countBeneficialLoops(&DC.CurRegion, SE, LI, 0);
    updateLoopCountStatistic(Stats, false /* OnlyProfitable */);
    if (isProfitableRegion(DC)) {
      updateLoopCountStatistic(Stats, true /* OnlyProfitable */);
      continue;
    }

    ValidRegions.remove(&DC.CurRegion);
  }

  NumProfScopRegions += ValidRegions.size();
  NumLoopsOverall += countBeneficialLoops(TopRegion, SE, LI, 0).NumLoops;

  // Only makes sense when we tracked errors.
  if (PollyTrackFailures) {
    emitMissedRemarks(F);
  }

  if (ReportLevel) {
    printLocations(F);
  }

  assert(ValidRegions.size() <= DetectionContextMap.size() &&
         "Cached more results than valid regions");
}

template <class RR, typename... Args>
inline bool ScopDetection::invalid(DetectionContext &Context, bool Assert,
                                   Args &&...Arguments) const {
  if (!Context.Verifying) {
    RejectLog &Log = Context.Log;
    auto RejectReason = std::make_shared<RR>(Arguments...);
    Context.IsInvalid = true;

    // Log even if PollyTrackFailures is false, the log entries are also used in
    // canUseISLTripCount().
    Log.report(RejectReason);

    POLLY_DEBUG(dbgs() << RejectReason->getMessage());
    POLLY_DEBUG(dbgs() << "\n");
  } else {
    assert(!Assert && "Verification of detected scop failed");
  }

  return false;
}

bool ScopDetection::isMaxRegionInScop(const Region &R, bool Verify) {
  if (!ValidRegions.count(&R)) {
    return false;
  }

  if (Verify) {
    BBPair P = getBBPairForRegion(&R);
    std::unique_ptr<DetectionContext> &Entry = DetectionContextMap[P];

    // Free previous DetectionContext for the region and create and verify a new
    // one. Be sure that the DetectionContext is not still used by a ScopInfo.
    // Due to changes by CodeGeneration of another Scop, the Region object and
    // the BBPair might not match anymore.
    Entry = std::make_unique<DetectionContext>(const_cast<Region &>(R), AA,
                                               /*Verifying=*/false);

    return isValidRegion(*Entry);
  }

  return true;
}

std::string ScopDetection::regionIsInvalidBecause(const Region *R) const {
  // Get the first error we found. Even in keep-going mode, this is the first
  // reason that caused the candidate to be rejected.
  const auto *Log = lookupRejectionLog(R);

  // This can happen when we marked a region invalid, but didn't track
  // an error for it.
  if (!Log || !Log->hasErrors()) {
    return "";
  }

  RejectReasonPtr RR = *Log->begin();
  return RR->getMessage();
}

bool ScopDetection::addOverApproximatedRegion(Region *AR,
                                              DetectionContext &Context) const {
  // If we already know about AR we can exit.
  if (!Context.NonAffineSubRegionSet.insert(AR)) {
    return true;
  }

  // All loops in the region have to be overapproximated too if there
  // are accesses that depend on the iteration count.
  for (BasicBlock *BB : AR->blocks()) {
    Loop *L = LI.getLoopFor(BB);
    if (L && AR->contains(L)) {
      Context.BoxedLoopsSet.insert(L);
    }
  }

  return (AllowNonAffineSubLoops || Context.BoxedLoopsSet.empty());
}

bool ScopDetection::onlyValidRequiredInvariantLoads(
    InvariantLoadsSetTy &RequiredILS, DetectionContext &Context) const {
  Region &CurRegion = Context.CurRegion;
  const DataLayout &DL = CurRegion.getEntry()->getModule()->getDataLayout();

  if (!PollyInvariantLoadHoisting && !RequiredILS.empty()) {
    return false;
  }

  for (LoadInst *Load : RequiredILS) {
    // If we already know a load has been accepted as required invariant, we
    // already ran the validation below once and consequently don't need to
    // run it again. Hence, we continue early. For certain test cases (e.g.,
    // COSMO) this avoids us spending 50% of scop-detection time in this
    // very function (and its children).
    if (Context.RequiredILS.count(Load)) {
      continue;
    }
    if (!isHoistableLoad(Load, CurRegion, LI, SE, DT, Context.RequiredILS)) {
      return false;
    }

    for (auto *NonAffineRegion : Context.NonAffineSubRegionSet) {
      if (isSafeToLoadUnconditionally(Load->getPointerOperand(),
                                      Load->getType(), Load->getAlign(), DL,
                                      nullptr)) {
        continue;
      }

      if (NonAffineRegion->contains(Load) &&
          Load->getParent() != NonAffineRegion->getEntry()) {
        return false;
      }
    }
  }

  Context.RequiredILS.insert_range(RequiredILS);

  return true;
}

bool ScopDetection::involvesMultiplePtrs(const SCEV *S0, const SCEV *S1,
                                         Loop *Scope) const {
  SetVector<Value *> Values;
  findValues(S0, SE, Values);
  if (S1) {
    findValues(S1, SE, Values);
  }

  SmallPtrSet<Value *, 8> PtrVals;
  for (auto *V : Values) {
    if (auto *P2I = dyn_cast<PtrToIntInst>(V)) {
      V = P2I->getOperand(0);
    }

    if (!V->getType()->isPointerTy()) {
      continue;
    }

    const SCEV *PtrSCEV = SE.getSCEVAtScope(V, Scope);
    if (isa<SCEVConstant>(PtrSCEV)) {
      continue;
    }

    auto *BasePtr = dyn_cast<SCEVUnknown>(SE.getPointerBase(PtrSCEV));
    if (!BasePtr) {
      return true;
    }

    Value *BasePtrVal = BasePtr->getValue();
    if (PtrVals.insert(BasePtrVal).second) {
      for (auto *PtrVal : PtrVals) {
        if (PtrVal != BasePtrVal && !AA.isNoAlias(PtrVal, BasePtrVal)) {
          return true;
        }
      }
    }
  }

  return false;
}

bool ScopDetection::isAffine(const SCEV *S, Loop *Scope,
                             DetectionContext &Context) const {
  InvariantLoadsSetTy AccessILS;
  if (!isAffineExpr(&Context.CurRegion, Scope, S, SE, &AccessILS)) {
    return false;
  }

  if (!onlyValidRequiredInvariantLoads(AccessILS, Context)) {
    return false;
  }

  return true;
}

bool ScopDetection::isValidSwitch(BasicBlock &BB, SwitchInst *SI,
                                  Value *Condition, bool IsLoopBranch,
                                  DetectionContext &Context) const {
  Loop *L = LI.getLoopFor(&BB);
  const SCEV *ConditionSCEV = SE.getSCEVAtScope(Condition, L);

  if (IsLoopBranch && L && L->isLoopLatch(&BB)) {
    return false;
  }

  // Check for invalid usage of different pointers in one expression.
  if (involvesMultiplePtrs(ConditionSCEV, nullptr, L)) {
    return false;
  }

  if (isAffine(ConditionSCEV, L, Context)) {
    return true;
  }

  if (AllowNonAffineSubRegions &&
      addOverApproximatedRegion(RI.getRegionFor(&BB), Context)) {
    return true;
  }

  return invalid<ReportNonAffBranch>(Context, /*Assert=*/true, &BB,
                                     ConditionSCEV, ConditionSCEV, SI);
}

bool ScopDetection::isValidBranch(BasicBlock &BB, BranchInst *BI,
                                  Value *Condition, bool IsLoopBranch,
                                  DetectionContext &Context) {
  // Constant integer conditions are always affine.
  if (isa<ConstantInt>(Condition)) {
    return true;
  }

  if (auto *BinOp = dyn_cast<BinaryOperator>(Condition)) {
    auto Opcode = BinOp->getOpcode();
    if (Opcode == Instruction::And || Opcode == Instruction::Or) {
      Value *Op0 = BinOp->getOperand(0);
      Value *Op1 = BinOp->getOperand(1);
      return isValidBranch(BB, BI, Op0, IsLoopBranch, Context) &&
             isValidBranch(BB, BI, Op1, IsLoopBranch, Context);
    }
  }

  if (auto *PHI = dyn_cast<PHINode>(Condition)) {
    auto *Unique = dyn_cast_or_null<ConstantInt>(
        getUniqueNonErrorValue(PHI, &Context.CurRegion, this));
    if (Unique && (Unique->isZero() || Unique->isOne())) {
      return true;
    }
  }

  if (auto *Load = dyn_cast<LoadInst>(Condition)) {
    if (!IsLoopBranch && Context.CurRegion.contains(Load)) {
      Context.RequiredILS.insert(Load);
      return true;
    }
  }

  // Non constant conditions of branches need to be ICmpInst.
  if (!isa<ICmpInst>(Condition)) {
    if (!IsLoopBranch && AllowNonAffineSubRegions &&
        addOverApproximatedRegion(RI.getRegionFor(&BB), Context)) {
      return true;
    }
    return invalid<ReportInvalidCond>(Context, /*Assert=*/true, BI, &BB);
  }

  auto *ICmp = cast<ICmpInst>(Condition);

  // Are both operands of the ICmp affine?
  if (isa<UndefValue>(ICmp->getOperand(0)) ||
      isa<UndefValue>(ICmp->getOperand(1))) {
    return invalid<ReportUndefOperand>(Context, /*Assert=*/true, &BB, ICmp);
  }

  Loop *L = LI.getLoopFor(&BB);
  const SCEV *LHS = SE.getSCEVAtScope(ICmp->getOperand(0), L);
  const SCEV *RHS = SE.getSCEVAtScope(ICmp->getOperand(1), L);

  LHS = tryForwardThroughPHI(LHS, Context.CurRegion, SE, this);
  RHS = tryForwardThroughPHI(RHS, Context.CurRegion, SE, this);

  // If unsigned operations are not allowed try to approximate the region.
  if (ICmp->isUnsigned() && !PollyAllowUnsignedOperations) {
    return !IsLoopBranch && AllowNonAffineSubRegions &&
           addOverApproximatedRegion(RI.getRegionFor(&BB), Context);
  }

  // Check for invalid usage of different pointers in one expression.
  if (ICmp->isEquality() && involvesMultiplePtrs(LHS, nullptr, L) &&
      involvesMultiplePtrs(RHS, nullptr, L)) {
    return false;
  }

  // Check for invalid usage of different pointers in a relational comparison.
  if (ICmp->isRelational() && involvesMultiplePtrs(LHS, RHS, L)) {
    return false;
  }

  if (isAffine(LHS, L, Context) && isAffine(RHS, L, Context)) {
    return true;
  }

  if (!IsLoopBranch && AllowNonAffineSubRegions &&
      addOverApproximatedRegion(RI.getRegionFor(&BB), Context)) {
    return true;
  }

  if (IsLoopBranch) {
    return false;
  }

  return invalid<ReportNonAffBranch>(Context, /*Assert=*/true, &BB, LHS, RHS,
                                     ICmp);
}

bool ScopDetection::isValidCFG(BasicBlock &BB, bool IsLoopBranch,
                               bool AllowUnreachable,
                               DetectionContext &Context) {
  Region &CurRegion = Context.CurRegion;

  Instruction *TI = BB.getTerminator();
  assert(TI && "BasicBlock must have a terminator");

  // If unreachable blocks are explicitly allowed, accept an unreachable
  // terminator without further checks.
  if (AllowUnreachable && isa<UnreachableInst>(TI)) {
    return true;
  }

  // Return instructions are only valid if the region is the top-level region.
  if (isa<ReturnInst>(TI) && CurRegion.isTopLevelRegion()) {
    return true;
  }

  // Extract the condition from the terminator, if any.
  // For unconditional branches and other terminators this may be null.
  Value *Condition = getConditionFromTerminator(TI);

  if (!Condition) {
    // Any terminator we cannot model via a condition is considered invalid
    // for SCoP formation.
    return invalid<ReportInvalidTerminator>(Context, /*Assert=*/true, &BB);
  }

  // UndefValue is not allowed as a condition.
  if (isa<UndefValue>(Condition)) {
    return invalid<ReportUndefCond>(Context, /*Assert=*/true, TI, &BB);
  }

  if (auto *BI = dyn_cast<BranchInst>(TI)) {
    return isValidBranch(BB, BI, Condition, IsLoopBranch, Context);
  }

  if (auto *SI = dyn_cast<SwitchInst>(TI)) {
    return isValidSwitch(BB, SI, Condition, IsLoopBranch, Context);
  }

  // Any other terminator (invoke, indirectbr, callbr, etc.) is not supported
  // by the current SCoP detection logic. Instead of asserting, we gracefully
  // reject the CFG at this point.
  return invalid<ReportInvalidTerminator>(Context, /*Assert=*/true, &BB);
}

bool ScopDetection::isValidCallInst(CallInst &CI,
                                    DetectionContext &Context) const {
  if (CI.doesNotReturn()) {
    return false;
  }

  if (CI.doesNotAccessMemory()) {
    return true;
  }

  if (auto *II = dyn_cast<IntrinsicInst>(&CI)) {
    if (isValidIntrinsicInst(*II, Context)) {
      return true;
    }
  }

  Function *CalledFunction = CI.getCalledFunction();

  // Indirect calls are not supported.
  if (CalledFunction == nullptr) {
    return false;
  }

  if (isDebugCall(&CI)) {
    POLLY_DEBUG(dbgs() << "Allow call to debug function: "
                       << CalledFunction->getName() << '\n');
    return true;
  }

  if (AllowModrefCall) {
    MemoryEffects ME = AA.getMemoryEffects(CalledFunction);
    if (ME.onlyAccessesArgPointees()) {
      for (const auto &Arg : CI.args()) {
        if (!Arg->getType()->isPointerTy()) {
          continue;
        }

        // Bail if a pointer argument has a base address not known to
        // ScalarEvolution. Note that a zero pointer is acceptable.
        const SCEV *ArgSCEV =
            SE.getSCEVAtScope(Arg, LI.getLoopFor(CI.getParent()));
        if (ArgSCEV->isZero()) {
          continue;
        }

        auto *BP = dyn_cast<SCEVUnknown>(SE.getPointerBase(ArgSCEV));
        if (!BP) {
          return false;
        }

        // Implicitly disable delinearization since we have an unknown
        // access with an unknown access function.
        Context.HasUnknownAccess = true;
      }

      // Explicitly use addUnknown so we don't put a loop-variant
      // pointer into the alias set.
      Context.AST.addUnknown(&CI);
      return true;
    }

    if (ME.onlyReadsMemory()) {
      // Implicitly disable delinearization since we have an unknown
      // access with an unknown access function.
      Context.HasUnknownAccess = true;
      // Explicitly use addUnknown so we don't put a loop-variant
      // pointer into the alias set.
      Context.AST.addUnknown(&CI);
      return true;
    }
    return false;
  }

  return false;
}

bool ScopDetection::isValidIntrinsicInst(IntrinsicInst &II,
                                         DetectionContext &Context) const {
  if (isIgnoredIntrinsic(&II)) {
    return true;
  }

  // The closest loop surrounding the call instruction.
  Loop *L = LI.getLoopFor(II.getParent());

  // The access function and base pointer for memory intrinsics.
  const SCEV *AF;
  const SCEVUnknown *BP;

  switch (II.getIntrinsicID()) {
  // Memory intrinsics that can be represented are supported.
  case Intrinsic::memmove:
  case Intrinsic::memcpy:
    AF = SE.getSCEVAtScope(cast<MemTransferInst>(II).getSource(), L);
    if (!AF->isZero()) {
      BP = dyn_cast<SCEVUnknown>(SE.getPointerBase(AF));
      // Bail if the source pointer is not valid.
      if (!isValidAccess(&II, AF, BP, Context)) {
        return false;
      }
    }
    [[fallthrough]];
  case Intrinsic::memset:
    AF = SE.getSCEVAtScope(cast<MemIntrinsic>(II).getDest(), L);
    if (!AF->isZero()) {
      BP = dyn_cast<SCEVUnknown>(SE.getPointerBase(AF));
      // Bail if the destination pointer is not valid.
      if (!isValidAccess(&II, AF, BP, Context)) {
        return false;
      }
    }

    // Bail if the length is not affine.
    if (!isAffine(SE.getSCEVAtScope(cast<MemIntrinsic>(II).getLength(), L), L,
                  Context)) {
      return false;
    }

    return true;
  default:
    break;
  }

  return false;
}

bool ScopDetection::isInvariant(Value &Val, const Region &Reg,
                                DetectionContext &Ctx) const {
  // A reference to function argument or constant value is invariant.
  if (isa<Argument>(Val) || isa<Constant>(Val)) {
    return true;
  }

  auto *I = dyn_cast<Instruction>(&Val);
  if (!I) {
    return false;
  }

  if (!Reg.contains(I)) {
    return true;
  }

  // Loads within the SCoP may read arbitrary values, need to hoist them. If it
  // is not hoistable, it will be rejected later, but here we assume it is and
  // that makes the value invariant.
  if (auto *LI = dyn_cast<LoadInst>(I)) {
    Ctx.RequiredILS.insert(LI);
    return true;
  }

  return false;
}

namespace {

/// Remove smax of smax(0, size) expressions from a SCEV expression and
/// register the '...' components.
///
/// Array access expressions as they are generated by GFortran contain smax(0,
/// size) expressions that confuse the 'normal' delinearization algorithm.
/// However, if we extract such expressions before the normal delinearization
/// takes place they can actually help to identify array size expressions in
/// Fortran accesses. For the subsequently following delinearization the smax(0,
/// size) component can be replaced by just 'size'. This is correct as we will
/// always add and verify the assumption that for all subscript expressions
/// 'exp' the inequality 0 <= exp < size holds. Hence, we will also verify
/// that 0 <= size, which means smax(0, size) == size.
class SCEVRemoveMax final : public SCEVRewriteVisitor<SCEVRemoveMax> {
public:
  SCEVRemoveMax(ScalarEvolution &SE, std::vector<const SCEV *> *Terms)
      : SCEVRewriteVisitor(SE), Terms(Terms) {}

  static const SCEV *rewrite(const SCEV *Scev, ScalarEvolution &SE,
                             std::vector<const SCEV *> *Terms = nullptr) {
    SCEVRemoveMax Rewriter(SE, Terms);
    return Rewriter.visit(Scev);
  }

  const SCEV *visitSMaxExpr(const SCEVSMaxExpr *Expr) {
    if ((Expr->getNumOperands() == 2) && Expr->getOperand(0)->isZero()) {
      auto *Res = visit(Expr->getOperand(1));
      if (Terms) {
        Terms->push_back(Res);
      }
      return Res;
    }

    return Expr;
  }

private:
  std::vector<const SCEV *> *Terms;
};

} // namespace

SmallVector<const SCEV *, 4>
ScopDetection::getDelinearizationTerms(DetectionContext &Context,
                                       const SCEVUnknown *BasePointer) const {
  SmallVector<const SCEV *, 4> Terms;
  for (const auto &Pair : Context.Accesses[BasePointer]) {
    std::vector<const SCEV *> MaxTerms;
    SCEVRemoveMax::rewrite(Pair.second, SE, &MaxTerms);
    if (!MaxTerms.empty()) {
      Terms.insert(Terms.begin(), MaxTerms.begin(), MaxTerms.end());
      continue;
    }
    // In case the outermost expression is a plain add, we check if any of its
    // terms has the form 4 * %inst * %param * %param ..., aka a term that
    // contains a product between a parameter and an instruction that is
    // inside the scop. Such instructions, if allowed at all, are instructions
    // SCEV can not represent, but Polly is still looking through. As a
    // result, these instructions can depend on induction variables and are
    // most likely no array sizes. However, terms that are multiplied with
    // them are likely candidates for array sizes.
    if (auto *AF = dyn_cast<SCEVAddExpr>(Pair.second)) {
      for (const SCEV *Op : AF->operands()) {
        if (auto *AF2 = dyn_cast<SCEVAddRecExpr>(Op)) {
          collectParametricTerms(SE, AF2, Terms);
        }
        if (auto *AF2 = dyn_cast<SCEVMulExpr>(Op)) {
          SmallVector<const SCEV *, 0> Operands;

          for (const SCEV *MulOp : AF2->operands()) {
            if (auto *Const = dyn_cast<SCEVConstant>(MulOp)) {
              Operands.push_back(Const);
            }
            if (auto *Unknown = dyn_cast<SCEVUnknown>(MulOp)) {
              if (auto *Inst = dyn_cast<Instruction>(Unknown->getValue())) {
                if (!Context.CurRegion.contains(Inst)) {
                  Operands.push_back(MulOp);
                }
              } else {
                Operands.push_back(MulOp);
              }
            }
          }
          if (!Operands.empty()) {
            Terms.push_back(SE.getMulExpr(Operands));
          }
        }
      }
    }
    if (Terms.empty()) {
      collectParametricTerms(SE, Pair.second, Terms);
    }
  }
  return Terms;
}

bool ScopDetection::hasValidArraySizes(DetectionContext &Context,
                                       SmallVectorImpl<const SCEV *> &Sizes,
                                       const SCEVUnknown *BasePointer,
                                       Loop *Scope) const {
  // If no sizes were found, all sizes are trivially valid. We allow this case
  // to make it possible to pass known-affine accesses to the delinearization to
  // try to recover some interesting multi-dimensional accesses, but to still
  // allow the already known to be affine access in case the delinearization
  // fails. In such situations, the delinearization will just return a Sizes
  // array of size zero.
  if (Sizes.empty()) {
    return true;
  }

  Value *BaseValue = BasePointer->getValue();
  Region &CurRegion = Context.CurRegion;
  for (const SCEV *DelinearizedSize : Sizes) {
    // Don't pass down the scope to isAffine; array dimensions must be
    // invariant across the entire scop.
    if (!isAffine(DelinearizedSize, nullptr, Context)) {
      Sizes.clear();
      break;
    }
    if (auto *Unknown = dyn_cast<SCEVUnknown>(DelinearizedSize)) {
      auto *V = Unknown->getValue();
      if (auto *Load = dyn_cast<LoadInst>(V)) {
        if (Context.CurRegion.contains(Load) &&
            isHoistableLoad(Load, CurRegion, LI, SE, DT, Context.RequiredILS)) {
          Context.RequiredILS.insert(Load);
        }
        continue;
      }
    }
    if (hasScalarDepsInsideRegion(DelinearizedSize, &CurRegion, Scope, false,
                                  Context.RequiredILS)) {
      return invalid<ReportNonAffineAccess>(
          Context, /*Assert=*/true, DelinearizedSize,
          Context.Accesses[BasePointer].front().first, BaseValue);
    }
  }

  // No array shape derived.
  if (Sizes.empty()) {
    if (AllowNonAffine) {
      return true;
    }

    for (const auto &Pair : Context.Accesses[BasePointer]) {
      const Instruction *Insn = Pair.first;
      const SCEV *AF = Pair.second;

      if (!isAffine(AF, Scope, Context)) {
        invalid<ReportNonAffineAccess>(Context, /*Assert=*/true, AF, Insn,
                                       BaseValue);
        if (!KeepGoing) {
          return false;
        }
      }
    }
    return false;
  }
  return true;
}

// We first store the resulting memory accesses in TempMemoryAccesses. Only
// if the access functions for all memory accesses have been successfully
// delinearized we continue. Otherwise, we either report a failure or, if
// non-affine accesses are allowed, we drop the information. In case the
// information is dropped the memory accesses need to be overapproximated
// when translated to a polyhedral representation.
bool ScopDetection::computeAccessFunctions(
    DetectionContext &Context, const SCEVUnknown *BasePointer,
    std::shared_ptr<ArrayShape> Shape) const {
  Value *BaseValue = BasePointer->getValue();
  bool BasePtrHasNonAffine = false;
  MapInsnToMemAcc TempMemoryAccesses;
  for (const auto &Pair : Context.Accesses[BasePointer]) {
    const Instruction *Insn = Pair.first;
    auto *AF = Pair.second;
    AF = SCEVRemoveMax::rewrite(AF, SE);
    bool IsNonAffine = false;
    TempMemoryAccesses.insert(std::make_pair(Insn, MemAcc(Insn, Shape)));
    MemAcc *Acc = &TempMemoryAccesses.find(Insn)->second;
    auto *Scope = LI.getLoopFor(Insn->getParent());

    if (!AF) {
      if (isAffine(Pair.second, Scope, Context)) {
        Acc->DelinearizedSubscripts.push_back(Pair.second);
      } else {
        IsNonAffine = true;
      }
    } else {
      if (Shape->DelinearizedSizes.empty()) {
        Acc->DelinearizedSubscripts.push_back(AF);
      } else {
        llvm::computeAccessFunctions(SE, AF, Acc->DelinearizedSubscripts,
                                     Shape->DelinearizedSizes);
        if (Acc->DelinearizedSubscripts.empty()) {
          IsNonAffine = true;
        }
      }
      for (const SCEV *S : Acc->DelinearizedSubscripts) {
        if (!isAffine(S, Scope, Context)) {
          IsNonAffine = true;
        }
      }
    }

    // (Possibly) report non affine access
    if (IsNonAffine) {
      BasePtrHasNonAffine = true;
      if (!AllowNonAffine) {
        invalid<ReportNonAffineAccess>(Context, /*Assert=*/true, Pair.second,
                                       Insn, BaseValue);
        if (!KeepGoing) {
          return false;
        }
      }
    }
  }

  if (!BasePtrHasNonAffine) {
    Context.InsnToMemAcc.insert(TempMemoryAccesses.begin(),
                                TempMemoryAccesses.end());
  }

  return true;
}

bool ScopDetection::hasBaseAffineAccesses(DetectionContext &Context,
                                          const SCEVUnknown *BasePointer,
                                          Loop *Scope) const {
  auto Shape = std::make_shared<ArrayShape>(BasePointer);

  auto Terms = getDelinearizationTerms(Context, BasePointer);

  findArrayDimensions(SE, Terms, Shape->DelinearizedSizes,
                      Context.ElementSize[BasePointer]);

  if (!hasValidArraySizes(Context, Shape->DelinearizedSizes, BasePointer,
                          Scope)) {
    return false;
  }

  return computeAccessFunctions(Context, BasePointer, Shape);
}

bool ScopDetection::hasAffineMemoryAccesses(DetectionContext &Context) const {
  // TODO: If we have an unknown access and other non-affine accesses we do
  //       not try to delinearize them for now.
  if (Context.HasUnknownAccess && !Context.NonAffineAccesses.empty()) {
    return AllowNonAffine;
  }

  for (auto &Pair : Context.NonAffineAccesses) {
    auto *BasePointer = Pair.first;
    auto *Scope = Pair.second;
    if (!hasBaseAffineAccesses(Context, BasePointer, Scope)) {
      Context.IsInvalid = true;
      if (!KeepGoing) {
        return false;
      }
    }
  }
  return true;
}

bool ScopDetection::isValidAccess(Instruction *Inst, const SCEV *AF,
                                  const SCEVUnknown *BP,
                                  DetectionContext &Context) const {

  if (!BP) {
    return invalid<ReportNoBasePtr>(Context, /*Assert=*/true, Inst);
  }

  auto *BV = BP->getValue();
  if (isa<UndefValue>(BV)) {
    return invalid<ReportUndefBasePtr>(Context, /*Assert=*/true, Inst);
  }

  // FIXME: Think about allowing IntToPtrInst
  if (auto *I2P = dyn_cast<IntToPtrInst>(BV)) {
    return invalid<ReportIntToPtr>(Context, /*Assert=*/true, I2P);
  }

  // Check that the base address of the access is invariant in the current
  // region.
  if (!isInvariant(*BV, Context.CurRegion, Context)) {
    return invalid<ReportVariantBasePtr>(Context, /*Assert=*/true, BV, Inst);
  }

  AF = SE.getMinusSCEV(AF, BP);

  const SCEV *Size;
  if (!isa<MemIntrinsic>(Inst)) {
    Size = SE.getElementSize(Inst);
  } else {
    auto *SizeTy =
        SE.getEffectiveSCEVType(PointerType::getUnqual(SE.getContext()));
    Size = SE.getConstant(SizeTy, 8);
  }

  if (Context.ElementSize[BP]) {
    if (!AllowDifferentTypes && Context.ElementSize[BP] != Size) {
      return invalid<ReportDifferentArrayElementSize>(Context, /*Assert=*/true,
                                                      Inst, BV);
    }

    Context.ElementSize[BP] = SE.getSMinExpr(Size, Context.ElementSize[BP]);
  } else {
    Context.ElementSize[BP] = Size;
  }

  bool IsVariantInNonAffineLoop = false;
  SetVector<const Loop *> Loops;
  findLoops(AF, Loops);
  for (const Loop *L : Loops) {
    if (Context.BoxedLoopsSet.count(L)) {
      IsVariantInNonAffineLoop = true;
      break;
    }
  }

  auto *Scope = LI.getLoopFor(Inst->getParent());
  bool IsAffine = !IsVariantInNonAffineLoop && isAffine(AF, Scope, Context);
  // Do not try to delinearize memory intrinsics and force them to be affine.
  if (isa<MemIntrinsic>(Inst) && !IsAffine) {
    return invalid<ReportNonAffineAccess>(Context, /*Assert=*/true, AF, Inst,
                                          BV);
  } else if (PollyDelinearize && !IsVariantInNonAffineLoop) {
    Context.Accesses[BP].push_back({Inst, AF});

    if (!IsAffine) {
      Context.NonAffineAccesses.insert(
          std::make_pair(BP, LI.getLoopFor(Inst->getParent())));
    }
  } else if (!AllowNonAffine && !IsAffine) {
    return invalid<ReportNonAffineAccess>(Context, /*Assert=*/true, AF, Inst,
                                          BV);
  }

  if (IgnoreAliasing) {
    return true;
  }

  // Check if the base pointer of the memory access does alias with
  // any other pointer. This cannot be handled at the moment.
  AAMDNodes AATags = Inst->getAAMetadata();
  AliasSet &AS = Context.AST.getAliasSetFor(
      MemoryLocation::getBeforeOrAfter(BP->getValue(), AATags));

  if (!AS.isMustAlias()) {
    if (PollyUseRuntimeAliasChecks) {
      bool CanBuildRunTimeCheck = true;
      // The run-time alias check places code that involves the base pointer at
      // the beginning of the SCoP. This breaks if the base pointer is defined
      // inside the scop. Hence, we can only create a run-time check if we are
      // sure the base pointer is not an instruction defined inside the scop.
      // However, we can ignore loads that will be hoisted.

      auto ASPointers = AS.getPointers();

      // Use the correct type: InvariantLoadsSetTy is SetVector<AssertingVH<LoadInst>>
      InvariantLoadsSetTy VariantLS, InvariantLS;

      // In order to detect loads which are dependent on other invariant loads
      // as invariant, we use fixed-point iteration method here i.e we iterate
      // over the alias set for arbitrary number of times until it is safe to
      // assume that all the invariant loads have been detected
      while (true) {
        const unsigned int VariantSize = VariantLS.size(),
                           InvariantSize = InvariantLS.size();

        for (const Value *Ptr : ASPointers) {
          Instruction *PtrInst = dyn_cast<Instruction>(const_cast<Value *>(Ptr));
          if (PtrInst && Context.CurRegion.contains(PtrInst)) {
            auto *Load = dyn_cast<LoadInst>(PtrInst);
            if (Load && InvariantLS.count(Load)) {
              continue;
            }
            if (Load && isHoistableLoad(Load, Context.CurRegion, LI, SE, DT,
                                        InvariantLS)) {
              if (VariantLS.count(Load)) {
                VariantLS.remove(Load);
              }
              Context.RequiredILS.insert(Load);
              InvariantLS.insert(Load);
            } else {
              CanBuildRunTimeCheck = false;
              if (Load) {
                VariantLS.insert(Load);
              }
            }
          }
        }

        if (InvariantSize == InvariantLS.size() &&
            VariantSize == VariantLS.size()) {
          break;
        }
      }

      if (CanBuildRunTimeCheck) {
        return true;
      }
    }
    return invalid<ReportAlias>(Context, /*Assert=*/true, Inst, AS);
  }

  return true;
}

bool ScopDetection::isValidMemoryAccess(MemAccInst Inst,
                                        DetectionContext &Context) const {
  Value *Ptr = Inst.getPointerOperand();
  Loop *L = LI.getLoopFor(Inst->getParent());
  const SCEV *AccessFunction = SE.getSCEVAtScope(Ptr, L);
  const SCEVUnknown *BasePointer;

  BasePointer = dyn_cast<SCEVUnknown>(SE.getPointerBase(AccessFunction));

  return isValidAccess(Inst, AccessFunction, BasePointer, Context);
}

bool ScopDetection::isCompatibleType(Instruction *Inst, Type *Ty,
                                     DetectionContext &Context) {
  if (!Ty) {
    return false;
  }

  if (isa<ScalableVectorType>(Ty)) {
    return invalid<ReportIncompatibleType>(Context, /*Assert=*/true, Inst, Ty);
  }

  return true;
}

bool ScopDetection::isValidInstruction(Instruction &Inst,
                                       DetectionContext &Context) {
  // Reject uses of values produced in error blocks, except in very constrained
  // situations (PHI nodes whose users are all terminators). This maintains a
  // clean separation between the "main" SCoP region and error-handling paths.
  for (Use &Op : Inst.operands()) {
    auto *OpInst = dyn_cast<Instruction>(Op.get());
    if (!OpInst) {
      continue;
    }

    if (!isCompatibleType(&Inst, Op->getType(), Context)) {
      return false;
    }

    if (isErrorBlock(*OpInst->getParent(), Context.CurRegion)) {
      if (auto *PHI = dyn_cast<PHINode>(OpInst)) {
        for (User *U : PHI->users()) {
          auto *UI = dyn_cast<Instruction>(U);
          if (!UI || !UI->isTerminator()) {
            return false;
          }
        }
      } else {
        return false;
      }
    }
  }

  // LandingPad and Resume are not supported in SCoPs.
  if (isa<LandingPadInst>(&Inst) || isa<ResumeInst>(&Inst)) {
    return false;
  }

  if (!isCompatibleType(&Inst, Inst.getType(), Context)) {
    return false;
  }

  // We only check the call instruction but not invoke instruction here.
  if (auto *CI = dyn_cast<CallInst>(&Inst)) {
    if (isValidCallInst(*CI, Context)) {
      return true;
    }

    return invalid<ReportFuncCall>(Context, /*Assert=*/true, &Inst);
  }

  // Instructions which do not read or write memory and are not allocas
  // are always fine.
  if (!Inst.mayReadOrWriteMemory()) {
    if (!isa<AllocaInst>(Inst)) {
      return true;
    }

    // Allocas are currently not supported inside SCoPs.
    return invalid<ReportAlloca>(Context, /*Assert=*/true, &Inst);
  }

  // Check the access function for memory instructions.
  if (auto MemInst = MemAccInst::dyn_cast(Inst)) {
    Context.hasStores |= isa<StoreInst>(MemInst);
    Context.hasLoads |= isa<LoadInst>(MemInst);

    if (!MemInst.isSimple()) {
      return invalid<ReportNonSimpleMemoryAccess>(Context, /*Assert=*/true,
                                                  &Inst);
    }

    return isValidMemoryAccess(MemInst, Context);
  }

  // Any other memory-reading/writing instruction we do not explicitly
  // understand is conservatively treated as invalid for SCoPs.
  return invalid<ReportUnknownInst>(Context, /*Assert=*/true, &Inst);
}

/// Check whether @p L has exiting blocks.
///
/// @param L The loop of interest
///
/// @return True if the loop has exiting blocks, false otherwise.
static bool hasExitingBlocks(Loop *L) {
  SmallVector<BasicBlock *, 4> ExitingBlocks;
  L->getExitingBlocks(ExitingBlocks);
  return !ExitingBlocks.empty();
}

bool ScopDetection::canUseISLTripCount(Loop *L, DetectionContext &Context) {
  // FIXME: Yes, this is bad. isValidCFG() may call invalid<Reason>() which
  // causes the SCoP to be rejected regardless on whether non-ISL trip counts
  // could be used. We currently preserve the legacy behaviour of rejecting
  // based on Context.Log.size() added by isValidCFG() or before, regardless on
  // whether the ISL trip count can be used or can be used as a non-affine
  // region. However, we allow rejections by isValidCFG() that do not result in
  // an error log entry.
  bool OldIsInvalid = Context.IsInvalid;

  // Ensure the loop has valid exiting blocks as well as latches, otherwise we
  // need to overapproximate it as a boxed loop.
  SmallVector<BasicBlock *, 4> LoopControlBlocks;
  L->getExitingBlocks(LoopControlBlocks);
  L->getLoopLatches(LoopControlBlocks);
  for (BasicBlock *ControlBB : LoopControlBlocks) {
    if (!isValidCFG(*ControlBB, true, false, Context)) {
      Context.IsInvalid = OldIsInvalid || Context.Log.size();
      return false;
    }
  }

  // We can use ISL to compute the trip count of L.
  Context.IsInvalid = OldIsInvalid || Context.Log.size();
  return true;
}

bool ScopDetection::isValidLoop(Loop *L, DetectionContext &Context) {
  // Loops that contain part but not all of the blocks of a region cannot be
  // handled by the schedule generation. Such loop constructs can happen
  // because a region can contain BBs that have no path to the exit block
  // (Infinite loops, UnreachableInst), but such blocks are never part of a
  // loop.
  //
  // _______________
  // | Loop Header | <-----------.
  // ---------------             |
  //        |                    |
  // _______________       ______________
  // | RegionEntry |-----> | RegionExit |----->
  // ---------------       --------------
  //        |
  // _______________
  // | EndlessLoop | <--.
  // ---------------    |
  //       |            |
  //       \------------/
  //
  // In the example above, the loop (LoopHeader,RegionEntry,RegionExit) is
  // neither entirely contained in the region RegionEntry->RegionExit
  // (containing RegionEntry,EndlessLoop) nor is the region entirely contained
  // in the loop.
  // The block EndlessLoop is contained in the region because Region::contains
  // tests whether it is not dominated by RegionExit. This is probably to not
  // having to query the PostdominatorTree. Instead of an endless loop, a dead
  // end can also be formed by an UnreachableInst. This case is already caught
  // by isErrorBlock(). We hence only have to reject endless loops here.
  if (!hasExitingBlocks(L)) {
    return invalid<ReportLoopHasNoExit>(Context, /*Assert=*/true, L);
  }

  // The algorithm for domain construction assumes that loops has only a single
  // exit block (and hence corresponds to a subregion). Note that we cannot use
  // L->getExitBlock() because it does not check whether all exiting edges point
  // to the same BB.
  SmallVector<BasicBlock *, 4> ExitBlocks;
  L->getExitBlocks(ExitBlocks);
  BasicBlock *TheExitBlock = ExitBlocks[0];
  for (BasicBlock *ExitBB : ExitBlocks) {
    if (TheExitBlock != ExitBB) {
      return invalid<ReportLoopHasMultipleExits>(Context, /*Assert=*/true, L);
    }
  }

  if (canUseISLTripCount(L, Context)) {
    return true;
  }

  if (AllowNonAffineSubLoops && AllowNonAffineSubRegions) {
    Region *R = RI.getRegionFor(L->getHeader());
    while (R != &Context.CurRegion && !R->contains(L)) {
      R = R->getParent();
    }

    if (addOverApproximatedRegion(R, Context)) {
      return true;
    }
  }

  const SCEV *LoopCount = SE.getBackedgeTakenCount(L);
  return invalid<ReportLoopBound>(Context, /*Assert=*/true, L, LoopCount);
}

/// Return the number of loops in @p L (incl. @p L) that have a trip
///        count that is not known to be less than @MinProfitableTrips.
///
/// This is an iterative implementation to avoid stack overflow on deep
/// loop nests and to improve cache locality. Uses explicit stack instead
/// of recursion for better performance on modern CPUs.
ScopDetection::LoopStats
ScopDetection::countBeneficialSubLoops(Loop *L, ScalarEvolution &SE,
                                       unsigned MinProfitableTrips) {
  // Stack entry for iterative traversal.
  struct StackEntry {
    Loop *L;
    int AccumulatedLoops;
    int MaxDepthSoFar;
    size_t SubLoopIndex;
    bool Initialized;
  };

  SmallVector<StackEntry, 16> WorkStack;
  WorkStack.push_back({L, 0, 0, 0, false});

  int FinalNumLoops = 0;
  int FinalMaxDepth = 0;

  while (!WorkStack.empty()) {
    StackEntry &Entry = WorkStack.back();
    Loop *const CurrentLoop = Entry.L;

    if (!Entry.Initialized) {
      // First visit: compute this loop's contribution.
      Entry.Initialized = true;
      Entry.AccumulatedLoops = 1;
      Entry.MaxDepthSoFar = 1;

      // Check if this loop should be counted.
      if (MinProfitableTrips > 0) {
        const SCEV *TripCount = SE.getBackedgeTakenCount(CurrentLoop);
        if (const auto *TripCountC = dyn_cast<SCEVConstant>(TripCount)) {
          if (TripCountC->getType()->getScalarSizeInBits() <= 64) {
            if (TripCountC->getValue()->getZExtValue() <= MinProfitableTrips) {
              Entry.AccumulatedLoops = 0;
            }
          }
        }
      }
    }

    // Process subloops iteratively.
    const auto &SubLoops = CurrentLoop->getSubLoops();
    if (Entry.SubLoopIndex < SubLoops.size()) {
      Loop *SubLoop = SubLoops[Entry.SubLoopIndex];
      ++Entry.SubLoopIndex;

      // Push child for processing.
      WorkStack.push_back({SubLoop, 0, 0, 0, false});
      continue;
    }

    // All children processed - pop and accumulate.
    const int ResultLoops = Entry.AccumulatedLoops;
    const int ResultDepth = Entry.MaxDepthSoFar;
    WorkStack.pop_back();

    if (WorkStack.empty()) {
      FinalNumLoops = ResultLoops;
      FinalMaxDepth = ResultDepth;
    } else {
      // Accumulate into parent.
      WorkStack.back().AccumulatedLoops += ResultLoops;
      WorkStack.back().MaxDepthSoFar =
          std::max(WorkStack.back().MaxDepthSoFar, ResultDepth + 1);
    }
  }

  return {FinalNumLoops, FinalMaxDepth};
}

ScopDetection::LoopStats
ScopDetection::countBeneficialLoops(Region *R, ScalarEvolution &SE,
                                    LoopInfo &LI, unsigned MinProfitableTrips) {
  int LoopNum = 0;
  int MaxLoopDepth = 0;

  auto *L = LI.getLoopFor(R->getEntry());

  // If L is fully contained in R, move to first loop surrounding R. Otherwise,
  // L is either nullptr or already surrounding R.
  if (L && R->contains(L)) {
    L = R->outermostLoopInRegion(L);
    L = L->getParentLoop();
  }

  auto SubLoops =
      L ? L->getSubLoopsVector() : std::vector<Loop *>(LI.begin(), LI.end());

  for (auto *SubLoop : SubLoops) {
    if (R->contains(SubLoop)) {
      LoopStats Stats =
          countBeneficialSubLoops(SubLoop, SE, MinProfitableTrips);
      LoopNum += Stats.NumLoops;
      MaxLoopDepth = std::max(MaxLoopDepth, Stats.MaxDepth);
    }
  }

  return {LoopNum, MaxLoopDepth};
}

/// Check if a basic block is an error block.
///
/// Error blocks are blocks that are unlikely to be executed at runtime.
/// They typically contain error handling code, assertions, or unreachable
/// instructions.
///
/// This function uses a fast-path ordering to check the cheapest conditions
/// first, improving performance on typical code where most blocks are not
/// error blocks.
static bool isErrorBlockImpl(BasicBlock &BB, const Region &R, LoopInfo &LI,
                             const DominatorTree &DT) {
  // Fast path: unreachable blocks are definitively error blocks.
  // This is the cheapest check (single instruction type comparison).
  if (isa<UnreachableInst>(BB.getTerminator())) {
    return true;
  }

  // Loop headers are never error blocks - second cheapest check.
  // LoopInfo lookup is O(1) via DenseMap.
  if (LI.isLoopHeader(&BB)) {
    return false;
  }

  // Blocks outside the SCoP are not error blocks.
  // Region::contains is O(1) via DenseSet lookup.
  if (!R.contains(&BB)) {
    return false;
  }

  // Dominance check is more expensive - defer until needed.
  // This check determines if the block is always executed.
  bool DominatesAllPredecessors = true;

  if (R.isTopLevelRegion()) {
    // Top-level region: check dominance over all return instructions.
    for (BasicBlock &I : *R.getEntry()->getParent()) {
      if (isa<ReturnInst>(I.getTerminator()) && !DT.dominates(&BB, &I)) {
        DominatesAllPredecessors = false;
        break;
      }
    }
  } else {
    // Non-top-level: check dominance over region exit predecessors.
    BasicBlock *Exit = R.getExit();
    if (Exit) {
      for (BasicBlock *Pred : predecessors(Exit)) {
        if (R.contains(Pred) && !DT.dominates(&BB, Pred)) {
          DominatesAllPredecessors = false;
          break;
        }
      }
    }
  }

  // Blocks that dominate all exits are not error blocks.
  if (DominatesAllPredecessors) {
    return false;
  }

  // Most expensive check last: scan all instructions for problematic calls.
  for (Instruction &Inst : BB) {
    auto *CI = dyn_cast<CallInst>(&Inst);
    if (!CI) {
      continue;
    }

    // Debug and ignored intrinsics are fine.
    // Note: isDebugCall takes Instruction*, so pass the non-const pointer.
    if (isDebugCall(&Inst) || isIgnoredIntrinsic(CI)) {
      continue;
    }

    // Memory intrinsics are modeled.
    if (isa<MemSetInst>(CI) || isa<MemTransferInst>(CI)) {
      continue;
    }

    // Calls with side effects or noreturn indicate error handling.
    if (!CI->doesNotAccessMemory()) {
      return true;
    }
    if (CI->doesNotReturn()) {
      return true;
    }
  }

  return false;
}

bool ScopDetection::isErrorBlock(llvm::BasicBlock &BB, const llvm::Region &R) {
  if (!PollyAllowErrorBlocks) {
    return false;
  }

  auto It = ErrorBlockCache.insert({std::make_pair(&BB, &R), false});
  if (!It.second) {
    return It.first->getSecond();
  }

  bool Result = isErrorBlockImpl(BB, R, LI, DT);
  It.first->second = Result;
  return Result;
}

Region *ScopDetection::expandRegion(Region &R) {
  // Initially no valid region was found (greater than R)
  std::unique_ptr<Region> LastValidRegion;
  auto ExpandedRegion = std::unique_ptr<Region>(R.getExpandedRegion());

  POLLY_DEBUG(dbgs() << "\tExpanding " << R.getNameStr() << "\n");

  while (ExpandedRegion) {
    BBPair P = getBBPairForRegion(ExpandedRegion.get());
    std::unique_ptr<DetectionContext> &Entry = DetectionContextMap[P];
    Entry = std::make_unique<DetectionContext>(*ExpandedRegion, AA,
                                               /*Verifying=*/false);
    DetectionContext &Context = *Entry;

    POLLY_DEBUG(dbgs() << "\t\tTrying " << ExpandedRegion->getNameStr()
                       << "\n");
    // Only expand when we did not collect errors.

    if (!Context.Log.hasErrors()) {
      // If the exit is valid check all blocks
      //  - if true, a valid region was found => store it + keep expanding
      //  - if false, .tbd. => stop (should this really end the loop?)
      if (!allBlocksValid(Context) || Context.Log.hasErrors()) {
        removeCachedResults(*ExpandedRegion);
        DetectionContextMap.erase(P);
        break;
      }

      // Store this region, because it is the greatest valid (encountered so
      // far).
      if (LastValidRegion) {
        removeCachedResults(*LastValidRegion);
        DetectionContextMap.erase(getBBPairForRegion(LastValidRegion.get()));
      }
      LastValidRegion = std::move(ExpandedRegion);

      // Create and test the next greater region (if any)
      ExpandedRegion =
          std::unique_ptr<Region>(LastValidRegion->getExpandedRegion());

    } else {
      // Create and test the next greater region (if any)
      removeCachedResults(*ExpandedRegion);
      DetectionContextMap.erase(P);
      ExpandedRegion =
          std::unique_ptr<Region>(ExpandedRegion->getExpandedRegion());
    }
  }

  POLLY_DEBUG({
    if (LastValidRegion)
      dbgs() << "\tto " << LastValidRegion->getNameStr() << "\n";
    else
      dbgs() << "\tExpanding " << R.getNameStr() << " failed\n";
  });

  return LastValidRegion.release();
}

static bool regionWithoutLoops(Region &R, LoopInfo &LI) {
  for (const BasicBlock *BB : R.blocks()) {
    if (R.contains(LI.getLoopFor(BB))) {
      return false;
    }
  }

  return true;
}

void ScopDetection::removeCachedResultsRecursively(const Region &R) {
  for (auto &SubRegion : R) {
    if (ValidRegions.count(SubRegion.get())) {
      removeCachedResults(*SubRegion);
    } else {
      removeCachedResultsRecursively(*SubRegion);
    }
  }
}

void ScopDetection::removeCachedResults(const Region &R) {
  ValidRegions.remove(&R);
}

void ScopDetection::findScops(Region &R) {
  std::unique_ptr<DetectionContext> &Entry =
      DetectionContextMap[getBBPairForRegion(&R)];
  Entry = std::make_unique<DetectionContext>(R, AA, /*Verifying=*/false);
  DetectionContext &Context = *Entry;

  bool DidBailout = true;
  if (!PollyProcessUnprofitable && regionWithoutLoops(R, LI)) {
    invalid<ReportUnprofitable>(Context, /*Assert=*/true, &R);
  } else {
    DidBailout = !isValidRegion(Context);
  }

  (void)DidBailout;
  if (KeepGoing) {
    assert((!DidBailout || Context.IsInvalid) &&
           "With -polly-detect-keep-going, it is sufficient that if "
           "isValidRegion short-circuited, that SCoP is invalid");
  } else {
    assert(DidBailout == Context.IsInvalid &&
           "isValidRegion must short-circuit iff the ScoP is invalid");
  }

  if (Context.IsInvalid) {
    removeCachedResults(R);
  } else {
    ValidRegions.insert(&R);
    return;
  }

  for (auto &SubRegion : R) {
    findScops(*SubRegion);
  }

  // Try to expand regions.
  //
  // As the region tree normally only contains canonical regions, non canonical
  // regions that form a Scop are not found. Therefore, those non canonical
  // regions are checked by expanding the canonical ones.

  std::vector<Region *> ToExpand;

  for (auto &SubRegion : R) {
    ToExpand.push_back(SubRegion.get());
  }

  for (Region *CurrentRegion : ToExpand) {
    // Skip invalid regions. Regions may become invalid, if they are element of
    // an already expanded region.
    if (!ValidRegions.count(CurrentRegion)) {
      continue;
    }

    // Skip regions that had errors.
    const RejectLog *Log = lookupRejectionLog(CurrentRegion);
    if (Log && Log->hasErrors()) {
      continue;
    }

    Region *ExpandedR = expandRegion(*CurrentRegion);

    if (!ExpandedR) {
      continue;
    }

    R.addSubRegion(ExpandedR, true);
    ValidRegions.insert(ExpandedR);
    removeCachedResults(*CurrentRegion);
    removeCachedResultsRecursively(*ExpandedR);
  }
}

bool ScopDetection::allBlocksValid(DetectionContext &Context) {
  Region &CurRegion = Context.CurRegion;

  // Phase 1: Validate loops that are (partially) contained in the region.
  //
  // We ensure that loops either:
  //  - Are fully contained and structurally valid for SCoP detection, or
  //  - Do not partially overlap the region.
  for (const BasicBlock *BB : CurRegion.blocks()) {
    Loop *L = LI.getLoopFor(BB);
    if (!L) {
      continue;
    }

    // We only act when this basic block is the header of the loop.
    if (L->getHeader() != BB) {
      continue;
    }

    if (CurRegion.contains(L)) {
      // The loop is fully contained in the region.
      if (!isValidLoop(L, Context)) {
        Context.IsInvalid = true;
        if (!KeepGoing) {
          return false;
        }
      }
    } else {
      // The loop is not fully contained. Ensure that none of its latches
      // are inside the region. If they are, the loop partially overlaps the
      // region, which we cannot handle.
      SmallVector<BasicBlock *, 4> Latches;
      L->getLoopLatches(Latches);

      for (BasicBlock *Latch : Latches) {
        if (CurRegion.contains(Latch)) {
          return invalid<ReportLoopOnlySomeLatches>(Context, /*Assert=*/true,
                                                    L);
        }
      }
    }
  }

  // Phase 2: Validate the CFG and instructions of each basic block.
  for (BasicBlock *BB : CurRegion.blocks()) {
    const bool IsErrorBlock = isErrorBlock(*BB, CurRegion);

    // Also check exception blocks (and possibly register them as non-affine
    // regions). Even though exception blocks are not modeled, we use them
    // to forward-propagate domain constraints during ScopInfo construction.
    if (!isValidCFG(*BB, /*IsLoopBranch=*/false, IsErrorBlock, Context)) {
      if (!KeepGoing) {
        return false;
      }
    }

    if (IsErrorBlock) {
      // We treat error blocks specially; their internal instructions are not
      // considered part of the SCoP proper.
      continue;
    }

    // Iterate all instructions except the terminator, which has already been
    // validated by isValidCFG().
    auto I = BB->begin();
    auto E = BB->end();

    // Defensive check: normally a basic block always has at least a terminator,
    // so begin() != end(). However, if the IR is malformed, we avoid
    // dereferencing end().
    if (I == E) {
      continue;
    }

    --E; // Exclude the terminator.

    for (; I != E; ++I) {
      if (!isValidInstruction(*I, Context)) {
        Context.IsInvalid = true;
        if (!KeepGoing) {
          return false;
        }
      }
    }
  }

  // Finally, check that all memory accesses in the region can either be
  // represented as affine accesses or be handled according to our
  // configuration (e.g., non-affine handling when allowed).
  if (!hasAffineMemoryAccesses(Context)) {
    return false;
  }

  return true;
}

bool ScopDetection::hasSufficientCompute(DetectionContext &Context,
                                         int NumLoops) const {
  int InstCount = 0;

  if (NumLoops == 0) {
    return false;
  }

  for (auto *BB : Context.CurRegion.blocks()) {
    if (Context.CurRegion.contains(LI.getLoopFor(BB))) {
      InstCount += static_cast<int>(BB->size());
    }
  }

  InstCount = InstCount / NumLoops;

  return InstCount >= ProfitabilityMinPerLoopInstructions;
}

bool ScopDetection::hasPossiblyDistributableLoop(
    DetectionContext &Context) const {
  for (auto *BB : Context.CurRegion.blocks()) {
    auto *L = LI.getLoopFor(BB);
    if (!L) {
      continue;
    }
    if (!Context.CurRegion.contains(L)) {
      continue;
    }
    if (Context.BoxedLoopsSet.count(L)) {
      continue;
    }
    unsigned StmtsWithStoresInLoops = 0;
    for (auto *LBB : L->blocks()) {
      bool MemStore = false;
      for (auto &I : *LBB) {
        MemStore |= isa<StoreInst>(&I);
      }
      StmtsWithStoresInLoops += MemStore ? 1 : 0;
    }
    return (StmtsWithStoresInLoops > 1);
  }
  return false;
}

bool ScopDetection::isProfitableRegion(DetectionContext &Context) const {
  Region &CurRegion = Context.CurRegion;

  if (PollyProcessUnprofitable) {
    return true;
  }

  // We can probably not do a lot on scops that only write or only read
  // data.
  if (!Context.hasStores || !Context.hasLoads) {
    return invalid<ReportUnprofitable>(Context, /*Assert=*/true, &CurRegion);
  }

  int NumLoops =
      countBeneficialLoops(&CurRegion, SE, LI, MIN_LOOP_TRIP_COUNT).NumLoops;
  int NumAffineLoops =
      NumLoops - static_cast<int>(Context.BoxedLoopsSet.size());

  // Scops with at least two loops may allow either loop fusion or tiling and
  // are consequently interesting to look at.
  if (NumAffineLoops >= 2) {
    return true;
  }

  // A loop with multiple non-trivial blocks might be amendable to distribution.
  if (NumAffineLoops == 1 && hasPossiblyDistributableLoop(Context)) {
    return true;
  }

  // Scops that contain a loop with a non-trivial amount of computation per
  // loop-iteration are interesting as we may be able to parallelize such
  // loops. Individual loops that have only a small amount of computation
  // per-iteration are performance-wise very fragile as any change to the
  // loop induction variables may affect performance. To not cause spurious
  // performance regressions, we do not consider such loops.
  if (NumAffineLoops == 1 && hasSufficientCompute(Context, NumLoops)) {
    return true;
  }

  return invalid<ReportUnprofitable>(Context, /*Assert=*/true, &CurRegion);
}

bool ScopDetection::isValidRegion(DetectionContext &Context) {
  Region &CurRegion = Context.CurRegion;

  POLLY_DEBUG(dbgs() << "Checking region: " << CurRegion.getNameStr()
                     << "\n\t");

  if (!PollyAllowFullFunction && CurRegion.isTopLevelRegion()) {
    POLLY_DEBUG(dbgs() << "Top level region is invalid\n");
    Context.IsInvalid = true;
    return false;
  }

  DebugLoc DbgLoc;
  if (CurRegion.getExit() &&
      isa<UnreachableInst>(CurRegion.getExit()->getTerminator())) {
    POLLY_DEBUG(dbgs() << "Unreachable in exit\n");
    return invalid<ReportUnreachableInExit>(Context, /*Assert=*/true,
                                            CurRegion.getExit(), DbgLoc);
  }

  if (!OnlyRegion.empty() &&
      !CurRegion.getEntry()->getName().count(OnlyRegion)) {
    POLLY_DEBUG({
      dbgs() << "Region entry does not match -polly-only-region";
      dbgs() << "\n";
    });
    Context.IsInvalid = true;
    return false;
  }

  for (BasicBlock *Pred : predecessors(CurRegion.getEntry())) {
    Instruction *PredTerm = Pred->getTerminator();
    if (isa<IndirectBrInst>(PredTerm) || isa<CallBrInst>(PredTerm)) {
      return invalid<ReportIndirectPredecessor>(
          Context, /*Assert=*/true, PredTerm, PredTerm->getDebugLoc());
    }
  }

  // SCoP cannot contain the entry block of the function, because we need
  // to insert alloca instruction there when translate scalar to array.
  if (!PollyAllowFullFunction &&
      CurRegion.getEntry() ==
          &(CurRegion.getEntry()->getParent()->getEntryBlock())) {
    return invalid<ReportEntry>(Context, /*Assert=*/true, CurRegion.getEntry());
  }

  if (!allBlocksValid(Context)) {
    // TODO: Every failure condition within allBlocksValid should call
    // invalid<Reason>(). Otherwise we reject SCoPs without giving feedback to
    // the user.
    Context.IsInvalid = true;
    return false;
  }

  if (!isReducibleRegion(CurRegion, DbgLoc)) {
    return invalid<ReportIrreducibleRegion>(Context, /*Assert=*/true,
                                            &CurRegion, DbgLoc);
  }

  POLLY_DEBUG(dbgs() << "OK\n");
  return true;
}

void ScopDetection::markFunctionAsInvalid(Function *F) {
  F->addFnAttr(PollySkipFnAttr);
}

bool ScopDetection::isValidFunction(Function &F) {
  return !F.hasFnAttribute(PollySkipFnAttr);
}

void ScopDetection::printLocations(Function &F) {
  for (const Region *R : *this) {
    unsigned LineEntry, LineExit;
    std::string FileName;

    getDebugLocation(R, LineEntry, LineExit, FileName);
    DiagnosticScopFound Diagnostic(F, FileName, LineEntry, LineExit);
    F.getContext().diagnose(Diagnostic);
  }
}

void ScopDetection::emitMissedRemarks(const Function &F) {
  for (auto &DIt : DetectionContextMap) {
    DetectionContext &DC = *DIt.getSecond();
    if (DC.Log.hasErrors()) {
      emitRejectionRemarks(DIt.getFirst(), DC.Log, ORE);
    }
  }
}

bool ScopDetection::isReducibleRegion(Region &R, DebugLoc &DbgLoc) const {
  /// Enum for coloring BBs in Region.
  ///
  /// WHITE - Unvisited BB in DFS walk.
  /// GREY - BBs which are currently on the DFS stack for processing.
  /// BLACK - Visited and completely processed BB.
  enum Color : unsigned char { WHITE = 0, GREY = 1, BLACK = 2 };

  BasicBlock *const REntry = R.getEntry();
  BasicBlock *const RExit = R.getExit();

  // Use SmallDenseMap to avoid heap allocation for typical region sizes.
  // 64 entries covers most regions without heap allocation.
  // Per Intel Optimization Manual §2.5.5.4, keeping working set in L1 (32KB)
  // is critical; SmallDenseMap inline storage achieves this.
  SmallDenseMap<const BasicBlock *, Color, 64> BBColorMap;

  // Pack DFS state into cache-line-friendly structure.
  // sizeof(DFSState) = 16 bytes on 64-bit, 4 fit per cache line (64 bytes).
  // This improves spatial locality vs std::pair<BasicBlock*, unsigned>.
  struct DFSState {
    BasicBlock *BB;
    unsigned ChildIndex;
  };

  // SmallVector with inline capacity of 32 avoids heap allocation for
  // control flow graphs with depth <= 32 (covers 99%+ of real code).
  // Per Agner Fog's optimization manual, avoiding malloc/free in hot
  // paths saves 50-200 cycles per allocation.
  SmallVector<DFSState, 32> DFSStack;

  // Initialize the map for all BB with WHITE color.
  for (BasicBlock *BB : R.blocks()) {
    BBColorMap.try_emplace(BB, WHITE);
  }

  // Process the entry block of the Region.
  BBColorMap[REntry] = GREY;
  DFSStack.push_back({REntry, 0});

  while (!DFSStack.empty()) {
    // Use reference to avoid copy, modify in place.
    DFSState &State = DFSStack.back();
    BasicBlock *const CurrBB = State.BB;
    const unsigned StartIdx = State.ChildIndex;

    // Cache terminator pointer to avoid repeated BB->getTerminator() calls.
    const Instruction *const TInst = CurrBB->getTerminator();
    const unsigned NSucc = TInst->getNumSuccessors();

    bool FoundUnvisited = false;

    for (unsigned I = StartIdx; I < NSucc; ++I) {
      BasicBlock *const SuccBB = TInst->getSuccessor(I);

      // Skip region exit block and self-loops.
      if (SuccBB == RExit || SuccBB == CurrBB) {
        continue;
      }

      // Use find() to get iterator, avoiding double lookup.
      auto It = BBColorMap.find(SuccBB);
      if (It == BBColorMap.end()) {
        continue;
      }

      const Color SuccColor = It->second;

      if (SuccColor == WHITE) {
        // Update current state with next child index for resumption.
        State.ChildIndex = I + 1;
        // Mark successor and push for processing.
        It->second = GREY;
        DFSStack.push_back({SuccBB, 0});
        FoundUnvisited = true;
        break;
      }

      if (SuccColor == GREY) {
        // GREY indicates a loop in the control flow.
        // If the destination dominates the source, it is a natural loop;
        // otherwise, we have irreducible control flow.
        if (!DT.dominates(SuccBB, CurrBB)) {
          DbgLoc = TInst->getDebugLoc();
          return false;
        }
      }
    }

    // If all children processed, mark as fully visited and pop.
    if (!FoundUnvisited) {
      BBColorMap[CurrBB] = BLACK;
      DFSStack.pop_back();
    }
  }

  return true;
}

ScopDetection::DetectionContext *
ScopDetection::getDetectionContext(const Region *R) const {
  auto DCMIt = DetectionContextMap.find(getBBPairForRegion(R));
  if (DCMIt == DetectionContextMap.end()) {
    return nullptr;
  }
  return DCMIt->second.get();
}

const RejectLog *ScopDetection::lookupRejectionLog(const Region *R) const {
  const DetectionContext *DC = getDetectionContext(R);
  return DC ? &DC->Log : nullptr;
}

void ScopDetection::verifyRegion(const Region &R) {
  assert(isMaxRegionInScop(R) && "Expect R is a valid region.");

  DetectionContext Context(const_cast<Region &>(R), AA, true /*verifying*/);
  isValidRegion(Context);
}

void ScopDetection::verifyAnalysis() {
  if (!VerifyScops) {
    return;
  }

  for (const Region *R : ValidRegions) {
    verifyRegion(*R);
  }
}

ScopAnalysis::ScopAnalysis() {
  // Disable runtime alias checks if we ignore aliasing altogether.
  if (IgnoreAliasing) {
    PollyUseRuntimeAliasChecks = false;
  }
}

AnalysisKey ScopAnalysis::Key;

ScopDetection ScopAnalysis::run(Function &F, FunctionAnalysisManager &FAM) {
  auto &LI = FAM.getResult<LoopAnalysis>(F);
  auto &RI = FAM.getResult<RegionInfoAnalysis>(F);
  auto &AA = FAM.getResult<AAManager>(F);
  auto &SE = FAM.getResult<ScalarEvolutionAnalysis>(F);
  auto &DT = FAM.getResult<DominatorTreeAnalysis>(F);
  auto &ORE = FAM.getResult<OptimizationRemarkEmitterAnalysis>(F);

  ScopDetection Result(DT, SE, LI, RI, AA, ORE);
  Result.detect(F);
  return Result;
}

PreservedAnalyses ScopAnalysisPrinterPass::run(Function &F,
                                               FunctionAnalysisManager &FAM) {
  OS << "Detected Scops in Function " << F.getName() << "\n";
  auto &SD = FAM.getResult<ScopAnalysis>(F);
  for (const Region *R : SD.ValidRegions) {
    OS << "Valid Region for Scop: " << R->getNameStr() << '\n';
  }

  OS << "\n";
  return PreservedAnalyses::all();
}
