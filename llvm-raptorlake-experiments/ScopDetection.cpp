//===- ScopDetection.cpp - Detect Scops -----------------------------------===//
//
// Part of the the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// Detect the maximal Scops of a function.
//
//===----------------------------------------------------------------------===//

#include "polly/ScopDetection.h"
#include "polly/LinkAllPasses.h"
#include "polly/Options.h"
#include "polly/ScopDetectionDiagnostic.h"
#include "polly/Support/SCEVValidator.h"
#include "polly/Support/ScopHelper.h"
#include "polly/Support/ScopLocation.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/Delinearization.h"
#include "llvm/Analysis/Loads.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/OptimizationRemarkEmitter.h"
#include "llvm/Analysis/RegionInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#include "llvm/IR/Attributes.h"
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
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Value.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
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

static cl::opt<int> ProfitabilityMinPerLoopInstructions(
    "polly-detect-profitability-min-per-loop-insts",
    cl::desc("The minimal number of per-loop instructions before a single loop "
             "region is considered profitable"),
    cl::Hidden, cl::ValueRequired, cl::init(100000000), cl::cat(PollyCategory));

static cl::opt<unsigned> PollyScopsMaxBlocks(
    "polly-scops-max-blocks",
    cl::desc("Maximum number of basic blocks in a function for Scop detection"),
    cl::Hidden, cl::init(20000), cl::cat(PollyCategory));

static cl::opt<unsigned> PollyMaxSubRegions(
    "polly-max-subregions",
    cl::desc("Maximum number of sub-regions considered recursively"),
    cl::Hidden, cl::init(512), cl::cat(PollyCategory));

static cl::opt<unsigned> PollyMinMemops(
    "polly-min-memops",
    cl::desc("Minimal count of memory operations in a single-loop Scop"),
    cl::Hidden, cl::ValueRequired, cl::init(8), cl::cat(PollyCategory));

static cl::opt<unsigned> PollyMaxSwitchCases(
    "polly-max-switch-cases",
    cl::desc("Maximum number of cases in a switch to be considered"),
    cl::Hidden, cl::init(32), cl::cat(PollyCategory));

bool polly::PollyProcessUnprofitable;

static cl::opt<bool, true> XPollyProcessUnprofitable(
    "polly-process-unprofitable",
    cl::desc("Process scops that are unlikely to benefit from Polly optimizations."),
    cl::location(PollyProcessUnprofitable), cl::cat(PollyCategory));

static cl::list<std::string> OnlyFunctions(
    "polly-only-func",
    cl::desc("Only run on functions that match a regex. Multiple regexes can be comma separated. Scop detection will run on all functions that match ANY of the regexes provided."),
    cl::CommaSeparated, cl::cat(PollyCategory));

static cl::list<std::string> IgnoredFunctions(
    "polly-ignore-func",
    cl::desc("Ignore functions that match a regex. Multiple regexes can be comma separated. Scop detection will ignore all functions that match ANY of the regexes provided."),
    cl::CommaSeparated, cl::cat(PollyCategory));

bool polly::PollyAllowFullFunction;

static cl::opt<bool, true>
    XAllowFullFunction("polly-detect-full-functions",
                       cl::desc("Allow the detection of full functions"),
                       cl::location(polly::PollyAllowFullFunction),
                       cl::init(false), cl::cat(PollyCategory));

static cl::opt<std::string> OnlyRegion(
    "polly-only-region",
    cl::desc("Only run on certain regions (identifier must appear in the region entry block name)"),
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
    cl::desc("Allow different element types for array accesses"),
    cl::Hidden, cl::init(true), cl::cat(PollyCategory));

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
    cl::desc("Allow non affine conditions for branches"),
    cl::Hidden, cl::init(true), cl::cat(PollyCategory));

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

static const unsigned MIN_LOOP_TRIP_COUNT = 8;

bool polly::PollyTrackFailures = false;
bool polly::PollyDelinearize = false;
StringRef polly::PollySkipFnAttr = "polly.skip.fn";

STATISTIC(NumScopRegions, "Number of scops");
STATISTIC(NumLoopsInScop, "Number of loops in scops");
STATISTIC(NumScopsDepthZero, "Number of scops with maximal loop depth 0");
STATISTIC(NumScopsDepthOne, "Number of scops with maximal loop depth 1");
STATISTIC(NumScopsDepthTwo, "Number of scops with maximal loop depth 2");
STATISTIC(NumScopsDepthThree, "Number of scops with maximal loop depth 3");
STATISTIC(NumScopsDepthFour, "Number of scops with maximal loop depth 4");
STATISTIC(NumScopsDepthFive, "Number of scops with maximal loop depth 5");
STATISTIC(NumScopsDepthLarger, "Number of scops with maximal loop depth 6 and larger");
STATISTIC(NumProfScopRegions, "Number of scops (profitable scops only)");
STATISTIC(NumLoopsInProfScop, "Number of loops in scops (profitable scops only)");
STATISTIC(NumLoopsOverall, "Number of total loops");
STATISTIC(NumProfScopsDepthZero, "Number of scops with maximal loop depth 0 (profitable scops only)");
STATISTIC(NumProfScopsDepthOne, "Number of scops with maximal loop depth 1 (profitable scops only)");
STATISTIC(NumProfScopsDepthTwo, "Number of scops with maximal loop depth 2 (profitable scops only)");
STATISTIC(NumProfScopsDepthThree, "Number of scops with maximal loop depth 3 (profitable scops only)");
STATISTIC(NumProfScopsDepthFour, "Number of scops with maximal loop depth 4 (profitable scops only)");
STATISTIC(NumProfScopsDepthFive, "Number of scops with maximal loop depth 5 (profitable scops only)");
STATISTIC(NumProfScopsDepthLarger, "Number of scops with maximal loop depth 6 and larger (profitable scops only)");
STATISTIC(MaxNumLoopsInScop, "Maximal number of loops in scops");
STATISTIC(MaxNumLoopsInProfScop, "Maximal number of loops in scops (profitable scops only)");

static void updateLoopCountStatistic(ScopDetection::LoopStats Stats, bool OnlyProfitable);

namespace {

class DiagnosticScopFound final : public DiagnosticInfo {
private:
  static int PluginDiagnosticKind;

  Function &F;
  std::string FileName;
  unsigned EntryLine, ExitLine;

public:
  DiagnosticScopFound(Function &F, std::string FileName, unsigned EntryLine, unsigned ExitLine)
      : DiagnosticInfo(PluginDiagnosticKind, DS_Note), F(F), FileName(FileName), EntryLine(EntryLine),
        ExitLine(ExitLine) {}

  void print(DiagnosticPrinter &DP) const override;

  static bool classof(const DiagnosticInfo *DI) { return DI->getKind() == PluginDiagnosticKind; }
};

} // namespace

int DiagnosticScopFound::PluginDiagnosticKind = getNextAvailablePluginDiagnosticKind();

void DiagnosticScopFound::print(DiagnosticPrinter &DP) const {
  DP << "Polly detected an optimizable loop region (scop) in function '" << F << "'\n";
  if (FileName.empty()) {
    DP << "Scop location is unknown. Compile with debug info (-g).";
    return;
  }
  DP << FileName << ":" << EntryLine << ": Start of scop\n";
  DP << FileName << ":" << ExitLine << ": End of scop";
}

static bool doesStringMatchAnyRegex(StringRef Str, const cl::list<std::string> &RegexList) {
  using RegexVec = std::vector<Regex>;
  static thread_local DenseMap<const void *, RegexVec> Cache;

  auto It = Cache.find(&RegexList);
  if (It == Cache.end()) {
    RegexVec RV;
    RV.reserve(RegexList.size());
    for (const auto &RegexStr : RegexList) {
      Regex R(RegexStr);
      std::string Err;
      if (!R.isValid(Err)) {
        report_fatal_error(Twine("invalid regex given as input to polly: ") + Err, true);
      }
      RV.push_back(std::move(R));
    }
    It = Cache.insert({&RegexList, std::move(RV)}).first;
  }

  for (const Regex &R : It->second) {
    if (R.match(Str)) {
      return true;
    }
  }
  return false;
}

ScopDetection::ScopDetection(const DominatorTree &DT, ScalarEvolution &SE, LoopInfo &LI, RegionInfo &RI,
                             AAResults &AA, OptimizationRemarkEmitter &ORE)
    : DT(DT), SE(SE), LI(LI), RI(RI), AA(AA), ORE(ORE) {}

void ScopDetection::detect(Function &F) {
  assert(ValidRegions.empty() && "Detection must run only once");

  if (!PollyProcessUnprofitable && LI.empty()) {
    return;
  }

  if (!OnlyFunctions.empty() && !doesStringMatchAnyRegex(F.getName(), OnlyFunctions)) {
    return;
  }

  if (doesStringMatchAnyRegex(F.getName(), IgnoredFunctions)) {
    return;
  }

  if (!isValidFunction(F)) {
    return;
  }

  Region *TopRegion = RI.getTopLevelRegion();
  findScops(*TopRegion);

  NumScopRegions += ValidRegions.size();

  for (auto &DIt : DetectionContextMap) {
    DetectionContext &DC = *DIt.getSecond();
    if (DC.Log.hasErrors()) {
      continue;
    }
    if (!ValidRegions.count(&DC.CurRegion)) {
      continue;
    }
    LoopStats Stats = countBeneficialLoops(&DC.CurRegion, SE, LI, 0);
    updateLoopCountStatistic(Stats, false);
    if (isProfitableRegion(DC)) {
      updateLoopCountStatistic(Stats, true);
      continue;
    }
    ValidRegions.remove(&DC.CurRegion);
  }

  NumProfScopRegions += ValidRegions.size();
  NumLoopsOverall += countBeneficialLoops(TopRegion, SE, LI, 0).NumLoops;

  if (PollyTrackFailures) {
    emitMissedRemarks(F);
  }
  if (ReportLevel) {
    printLocations(F);
  }

  assert(ValidRegions.size() <= DetectionContextMap.size() && "Cached more results than valid regions");
}

template <class RR, typename... Args>
inline bool ScopDetection::invalid(DetectionContext &Context, bool Assert, Args &&...Arguments) const {
  if (!Context.Verifying) {
    RejectLog &Log = Context.Log;
    std::shared_ptr<RR> RejectReason = std::make_shared<RR>(Arguments...);
    Context.IsInvalid = true;
    Log.report(RejectReason);
    POLLY_DEBUG(dbgs() << RejectReason->getMessage() << "\n");
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
    Entry = std::make_unique<DetectionContext>(const_cast<Region &>(R), AA, false);
    return isValidRegion(*Entry);
  }
  return true;
}

std::string ScopDetection::regionIsInvalidBecause(const Region *R) const {
  const RejectLog *Log = lookupRejectionLog(R);
  if (!Log || !Log->hasErrors()) {
    return "";
  }
  RejectReasonPtr RR = *Log->begin();
  return RR->getMessage();
}

bool ScopDetection::addOverApproximatedRegion(Region *AR, DetectionContext &Context) const {
  if (!Context.NonAffineSubRegionSet.insert(AR)) {
    return true;
  }

  for (BasicBlock *BB : AR->blocks()) {
    Loop *L = LI.getLoopFor(BB);
    if (AR->contains(L)) {
      Context.BoxedLoopsSet.insert(L);
    }
  }

  return (AllowNonAffineSubLoops || Context.BoxedLoopsSet.empty());
}

bool ScopDetection::onlyValidRequiredInvariantLoads(InvariantLoadsSetTy &RequiredILS,
                                                    DetectionContext &Context) const {
  Region &CurRegion = Context.CurRegion;
  const DataLayout &DL = CurRegion.getEntry()->getModule()->getDataLayout();

  if (!PollyInvariantLoadHoisting && !RequiredILS.empty()) {
    return false;
  }

  for (LoadInst *Load : RequiredILS) {
    if (Context.RequiredILS.count(Load)) {
      continue;
    }
    if (!isHoistableLoad(Load, CurRegion, LI, SE, DT, Context.RequiredILS)) {
      return false;
    }

    for (auto NonAffineRegion : Context.NonAffineSubRegionSet) {
      if (isSafeToLoadUnconditionally(Load->getPointerOperand(), Load->getType(), Load->getAlign(), DL, nullptr)) {
        continue;
      }
      if (NonAffineRegion->contains(Load) && Load->getParent() != NonAffineRegion->getEntry()) {
        return false;
      }
    }
  }

  Context.RequiredILS.insert_range(RequiredILS);
  return true;
}

bool ScopDetection::involvesMultiplePtrs(const SCEV *S0, const SCEV *S1, Loop *Scope) const {
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

bool ScopDetection::isAffine(const SCEV *S, Loop *Scope, DetectionContext &Context) const {
  InvariantLoadsSetTy AccessILS;
  if (!isAffineExpr(&Context.CurRegion, Scope, S, SE, &AccessILS)) {
    return false;
  }
  if (!onlyValidRequiredInvariantLoads(AccessILS, Context)) {
    return false;
  }
  return true;
}

bool ScopDetection::isValidSwitch(BasicBlock &BB, SwitchInst *SI, Value *Condition, bool IsLoopBranch,
                                  DetectionContext &Context) const {
  if (SI->getNumCases() > PollyMaxSwitchCases) {
    if (AllowNonAffineSubRegions && addOverApproximatedRegion(RI.getRegionFor(&BB), Context)) {
      return true;
    }
    Loop *L = LI.getLoopFor(&BB);
    const SCEV *CondS = SE.getSCEVAtScope(Condition, L);
    return invalid<ReportNonAffBranch>(Context, true, &BB, CondS, CondS, SI);
  }

  Loop *L = LI.getLoopFor(&BB);
  const SCEV *ConditionSCEV = SE.getSCEVAtScope(Condition, L);

  if (IsLoopBranch && L->isLoopLatch(&BB)) {
    return false;
  }

  if (involvesMultiplePtrs(ConditionSCEV, nullptr, L)) {
    return false;
  }

  if (isAffine(ConditionSCEV, L, Context)) {
    return true;
  }

  if (AllowNonAffineSubRegions && addOverApproximatedRegion(RI.getRegionFor(&BB), Context)) {
    return true;
  }

  return invalid<ReportNonAffBranch>(Context, true, &BB, ConditionSCEV, ConditionSCEV, SI);
}

bool ScopDetection::isValidBranch(BasicBlock &BB, BranchInst *BI, Value *Condition, bool IsLoopBranch,
                                  DetectionContext &Context) {
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
    auto *Unique = dyn_cast_or_null<ConstantInt>(getUniqueNonErrorValue(PHI, &Context.CurRegion, this));
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

  if (!isa<ICmpInst>(Condition)) {
    if (!IsLoopBranch && AllowNonAffineSubRegions && addOverApproximatedRegion(RI.getRegionFor(&BB), Context)) {
      return true;
    }
    return invalid<ReportInvalidCond>(Context, true, BI, &BB);
  }

  auto *ICmp = cast<ICmpInst>(Condition);

  if (isa<UndefValue>(ICmp->getOperand(0)) || isa<UndefValue>(ICmp->getOperand(1))) {
    return invalid<ReportUndefOperand>(Context, true, &BB, ICmp);
  }

  Loop *L = LI.getLoopFor(&BB);
  const SCEV *LHS = SE.getSCEVAtScope(ICmp->getOperand(0), L);
  const SCEV *RHS = SE.getSCEVAtScope(ICmp->getOperand(1), L);

  LHS = tryForwardThroughPHI(LHS, Context.CurRegion, SE, this);
  RHS = tryForwardThroughPHI(RHS, Context.CurRegion, SE, this);

  if (ICmp->isUnsigned() && !PollyAllowUnsignedOperations) {
    return !IsLoopBranch && AllowNonAffineSubRegions && addOverApproximatedRegion(RI.getRegionFor(&BB), Context);
  }

  if (ICmp->isEquality() && involvesMultiplePtrs(LHS, nullptr, L) && involvesMultiplePtrs(RHS, nullptr, L)) {
    return false;
  }

  if (ICmp->isRelational() && involvesMultiplePtrs(LHS, RHS, L)) {
    return false;
  }

  if (isAffine(LHS, L, Context) && isAffine(RHS, L, Context)) {
    return true;
  }

  if (!IsLoopBranch && AllowNonAffineSubRegions && addOverApproximatedRegion(RI.getRegionFor(&BB), Context)) {
    return true;
  }

  if (IsLoopBranch) {
    return false;
  }

  return invalid<ReportNonAffBranch>(Context, true, &BB, LHS, RHS, ICmp);
}

bool ScopDetection::isValidCFG(BasicBlock &BB, bool IsLoopBranch, bool AllowUnreachable,
                               DetectionContext &Context) {
  Region &CurRegion = Context.CurRegion;
  Instruction *TI = BB.getTerminator();

  if (AllowUnreachable && isa<UnreachableInst>(TI)) {
    return true;
  }

  if (isa<ReturnInst>(TI) && CurRegion.isTopLevelRegion()) {
    return true;
  }

  Value *Condition = getConditionFromTerminator(TI);
  if (!Condition) {
    return invalid<ReportInvalidTerminator>(Context, true, &BB);
  }

  if (isa<UndefValue>(Condition)) {
    return invalid<ReportUndefCond>(Context, true, TI, &BB);
  }

  if (auto *BI = dyn_cast<BranchInst>(TI)) {
    return isValidBranch(BB, BI, Condition, IsLoopBranch, Context);
  }

  auto *SI = dyn_cast<SwitchInst>(TI);
  assert(SI && "Terminator was neither branch nor switch");
  return isValidSwitch(BB, SI, Condition, IsLoopBranch, Context);
}

bool ScopDetection::isValidCallInst(CallInst &CI, DetectionContext &Context) const {
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
  if (CalledFunction == nullptr) {
    return false;
  }

  if (isDebugCall(&CI)) {
    POLLY_DEBUG(dbgs() << "Allow call to debug function: " << CalledFunction->getName() << '\n');
    return true;
  }

  if (AllowModrefCall) {
    MemoryEffects ME = AA.getMemoryEffects(CalledFunction);
    if (ME.onlyAccessesArgPointees()) {
      for (const auto &Arg : CI.args()) {
        if (!Arg->getType()->isPointerTy()) {
          continue;
        }
        const SCEV *ArgSCEV = SE.getSCEVAtScope(Arg, LI.getLoopFor(CI.getParent()));
        if (ArgSCEV->isZero()) {
          continue;
        }
        auto *BP = dyn_cast<SCEVUnknown>(SE.getPointerBase(ArgSCEV));
        if (!BP) {
          return false;
        }
        Context.HasUnknownAccess = true;
      }
      Context.AST.addUnknown(&CI);
      return true;
    }

    if (ME.onlyReadsMemory()) {
      Context.HasUnknownAccess = true;
      Context.AST.addUnknown(&CI);
      return true;
    }
    return false;
  }

  return false;
}

bool ScopDetection::isValidIntrinsicInst(IntrinsicInst &II, DetectionContext &Context) const {
  if (isIgnoredIntrinsic(&II)) {
    return true;
  }

  Loop *L = LI.getLoopFor(II.getParent());
  const SCEV *AF;
  const SCEVUnknown *BP;

  switch (II.getIntrinsicID()) {
  case Intrinsic::memmove:
  case Intrinsic::memcpy:
    AF = SE.getSCEVAtScope(cast<MemTransferInst>(II).getSource(), L);
    if (!AF->isZero()) {
      BP = dyn_cast<SCEVUnknown>(SE.getPointerBase(AF));
      if (!isValidAccess(&II, AF, BP, Context)) {
        return false;
      }
    }
    [[fallthrough]];
  case Intrinsic::memset:
    AF = SE.getSCEVAtScope(cast<MemIntrinsic>(II).getDest(), L);
    if (!AF->isZero()) {
      BP = dyn_cast<SCEVUnknown>(SE.getPointerBase(AF));
      if (!isValidAccess(&II, AF, BP, Context)) {
        return false;
      }
    }
    if (!isAffine(SE.getSCEVAtScope(cast<MemIntrinsic>(II).getLength(), L), L, Context)) {
      return false;
    }
    return true;
  default:
    break;
  }

  return false;
}

bool ScopDetection::isInvariant(Value &Val, const Region &Reg, DetectionContext &Ctx) const {
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

  if (auto *LI_ = dyn_cast<LoadInst>(I)) {
    Ctx.RequiredILS.insert(LI_);
    return true;
  }

  return false;
}

namespace {

class SCEVRemoveMax final : public SCEVRewriteVisitor<SCEVRemoveMax> {
public:
  SCEVRemoveMax(ScalarEvolution &SE, std::vector<const SCEV *> *Terms) : SCEVRewriteVisitor(SE), Terms(Terms) {}

  static const SCEV *rewrite(const SCEV *Scev, ScalarEvolution &SE, std::vector<const SCEV *> *Terms = nullptr) {
    SCEVRemoveMax Rewriter(SE, Terms);
    return Rewriter.visit(Scev);
  }

  const SCEV *visitSMaxExpr(const SCEVSMaxExpr *Expr) {
    if ((Expr->getNumOperands() == 2) && Expr->getOperand(0)->isZero()) {
      auto Res = visit(Expr->getOperand(1));
      if (Terms) {
        (*Terms).push_back(Res);
      }
      return Res;
    }
    return Expr;
  }

private:
  std::vector<const SCEV *> *Terms;
};

} // namespace

SmallVector<const SCEV *, 4> ScopDetection::getDelinearizationTerms(DetectionContext &Context,
                                                                    const SCEVUnknown *BasePointer) const {
  SmallVector<const SCEV *, 4> Terms;
  for (const auto &Pair : Context.Accesses[BasePointer]) {
    std::vector<const SCEV *> MaxTerms;
    SCEVRemoveMax::rewrite(Pair.second, SE, &MaxTerms);
    if (!MaxTerms.empty()) {
      Terms.insert(Terms.begin(), MaxTerms.begin(), MaxTerms.end());
      continue;
    }
    if (auto *AF = dyn_cast<SCEVAddExpr>(Pair.second)) {
      for (auto Op : AF->operands()) {
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

bool ScopDetection::hasValidArraySizes(DetectionContext &Context, SmallVectorImpl<const SCEV *> &Sizes,
                                       const SCEVUnknown *BasePointer, Loop *Scope) const {
  if (Sizes.empty()) {
    return true;
  }

  Value *BaseValue = BasePointer->getValue();
  Region &CurRegion = Context.CurRegion;
  for (const SCEV *DelinearizedSize : Sizes) {
    if (!isAffine(DelinearizedSize, nullptr, Context)) {
      Sizes.clear();
      break;
    }
    if (auto *Unknown = dyn_cast<SCEVUnknown>(DelinearizedSize)) {
      auto *V = dyn_cast<Value>(Unknown->getValue());
      if (auto *Load = dyn_cast<LoadInst>(V)) {
        if (Context.CurRegion.contains(Load) && isHoistableLoad(Load, CurRegion, LI, SE, DT, Context.RequiredILS)) {
          Context.RequiredILS.insert(Load);
        }
        continue;
      }
    }
    if (hasScalarDepsInsideRegion(DelinearizedSize, &CurRegion, Scope, false, Context.RequiredILS)) {
      return invalid<ReportNonAffineAccess>(Context, true, DelinearizedSize,
                                            Context.Accesses[BasePointer].front().first, BaseValue);
    }
  }

  if (Sizes.empty()) {
    if (AllowNonAffine) {
      return true;
    }

    for (const auto &Pair : Context.Accesses[BasePointer]) {
      const Instruction *Insn = Pair.first;
      const SCEV *AF = Pair.second;
      if (!isAffine(AF, Scope, Context)) {
        invalid<ReportNonAffineAccess>(Context, true, AF, Insn, BaseValue);
        if (!KeepGoing) {
          return false;
        }
      }
    }
    return false;
  }
  return true;
}

bool ScopDetection::computeAccessFunctions(DetectionContext &Context, const SCEVUnknown *BasePointer,
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
        llvm::computeAccessFunctions(SE, AF, Acc->DelinearizedSubscripts, Shape->DelinearizedSizes);
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

    if (IsNonAffine) {
      BasePtrHasNonAffine = true;
      if (!AllowNonAffine) {
        invalid<ReportNonAffineAccess>(Context, true, Pair.second, Insn, BaseValue);
        if (!KeepGoing) {
          return false;
        }
      }
    }
  }

  if (!BasePtrHasNonAffine) {
    Context.InsnToMemAcc.insert(TempMemoryAccesses.begin(), TempMemoryAccesses.end());
  }

  return true;
}

bool ScopDetection::hasBaseAffineAccesses(DetectionContext &Context, const SCEVUnknown *BasePointer,
                                          Loop *Scope) const {
  auto Shape = std::shared_ptr<ArrayShape>(new ArrayShape(BasePointer));
  auto Terms = getDelinearizationTerms(Context, BasePointer);

  findArrayDimensions(SE, Terms, Shape->DelinearizedSizes, Context.ElementSize[BasePointer]);

  if (!hasValidArraySizes(Context, Shape->DelinearizedSizes, BasePointer, Scope)) {
    return false;
  }

  return computeAccessFunctions(Context, BasePointer, Shape);
}

bool ScopDetection::hasAffineMemoryAccesses(DetectionContext &Context) const {
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

bool ScopDetection::isValidAccess(Instruction *Inst, const SCEV *AF, const SCEVUnknown *BP,
                                  DetectionContext &Context) const {
  if (!BP) {
    return invalid<ReportNoBasePtr>(Context, true, Inst);
  }

  auto *BV = BP->getValue();
  if (isa<UndefValue>(BV)) {
    return invalid<ReportUndefBasePtr>(Context, true, Inst);
  }

  if (auto *ITP = dyn_cast<IntToPtrInst>(BV)) {
    return invalid<ReportIntToPtr>(Context, true, ITP);
  }

  if (!isInvariant(*BV, Context.CurRegion, Context)) {
    return invalid<ReportVariantBasePtr>(Context, true, BV, Inst);
  }

  AF = SE.getMinusSCEV(AF, BP);

  const SCEV *Size;
  if (!isa<MemIntrinsic>(Inst)) {
    Size = SE.getElementSize(Inst);
  } else {
    auto *SizeTy = SE.getEffectiveSCEVType(PointerType::getUnqual(SE.getContext()));
    Size = SE.getConstant(SizeTy, 8);
  }

  if (Context.ElementSize[BP]) {
    if (!AllowDifferentTypes && Context.ElementSize[BP] != Size) {
      return invalid<ReportDifferentArrayElementSize>(Context, true, Inst, BV);
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
    }
  }

  auto *Scope = LI.getLoopFor(Inst->getParent());
  bool IsAffine = !IsVariantInNonAffineLoop && isAffine(AF, Scope, Context);

  if (isa<MemIntrinsic>(Inst) && !IsAffine) {
    return invalid<ReportNonAffineAccess>(Context, true, AF, Inst, BV);
  } else if (PollyDelinearize && !IsVariantInNonAffineLoop) {
    Context.Accesses[BP].push_back({Inst, AF});
    if (!IsAffine) {
      Context.NonAffineAccesses.insert(std::make_pair(BP, LI.getLoopFor(Inst->getParent())));
    }
  } else if (!AllowNonAffine && !IsAffine) {
    return invalid<ReportNonAffineAccess>(Context, true, AF, Inst, BV);
  }

  if (IgnoreAliasing) {
    return true;
  }

  AAMDNodes AATags = Inst->getAAMetadata();
  AliasSet &AS =
      Context.AST.getAliasSetFor(MemoryLocation::getBeforeOrAfter(BP->getValue(), AATags));

  if (!AS.isMustAlias()) {
    if (PollyUseRuntimeAliasChecks) {
      bool CanBuildRunTimeCheck = true;
      auto ASPointers = AS.getPointers();

      InvariantLoadsSetTy VariantLS, InvariantLS;
      while (true) {
        const unsigned int VariantSize = VariantLS.size(), InvariantSize = InvariantLS.size();

        for (const Value *Ptr : ASPointers) {
          Instruction *PtrInst = dyn_cast<Instruction>(const_cast<Value *>(Ptr));
          if (PtrInst && Context.CurRegion.contains(PtrInst)) {
            auto *Load = dyn_cast<LoadInst>(PtrInst);
            if (Load && InvariantLS.count(Load)) {
              continue;
            }
            if (Load && isHoistableLoad(Load, Context.CurRegion, LI, SE, DT, InvariantLS)) {
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

        if (InvariantSize == InvariantLS.size() && VariantSize == VariantLS.size()) {
          break;
        }
      }

      if (CanBuildRunTimeCheck) {
        return true;
      }
    }
    return invalid<ReportAlias>(Context, true, Inst, AS);
  }

  return true;
}

bool ScopDetection::isValidMemoryAccess(MemAccInst Inst, DetectionContext &Context) const {
  Value *Ptr = Inst.getPointerOperand();
  Loop *L = LI.getLoopFor(Inst->getParent());
  const SCEV *AccessFunction = SE.getSCEVAtScope(Ptr, L);
  const SCEVUnknown *BasePointer = dyn_cast<SCEVUnknown>(SE.getPointerBase(AccessFunction));
  return isValidAccess(Inst, AccessFunction, BasePointer, Context);
}

bool ScopDetection::isValidInstruction(Instruction &Inst, DetectionContext &Context) {
  for (auto &Op : Inst.operands()) {
    Value *OpV = Op.get();
    auto *OpInst = dyn_cast<Instruction>(OpV);
    if (!OpInst) {
      continue;
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

  if (isa<LandingPadInst>(&Inst) || isa<ResumeInst>(&Inst)) {
    return false;
  }

  if (auto *CI = dyn_cast<CallInst>(&Inst)) {
    if (isValidCallInst(*CI, Context)) {
      return true;
    }
    return invalid<ReportFuncCall>(Context, true, &Inst);
  }

  if (!Inst.mayReadOrWriteMemory()) {
    if (!isa<AllocaInst>(Inst)) {
      return true;
    }
    return invalid<ReportAlloca>(Context, true, &Inst);
  }

  if (auto MemInst = MemAccInst::dyn_cast(Inst)) {
    Context.hasStores |= isa<StoreInst>(MemInst);
    Context.hasLoads |= isa<LoadInst>(MemInst);
    if (!MemInst.isSimple()) {
      return invalid<ReportNonSimpleMemoryAccess>(Context, true, &Inst);
    }
    return isValidMemoryAccess(MemInst, Context);
  }

  return invalid<ReportUnknownInst>(Context, true, &Inst);
}

static bool hasExitingBlocks(Loop *L) {
  SmallVector<BasicBlock *, 4> ExitingBlocks;
  L->getExitingBlocks(ExitingBlocks);
  return !ExitingBlocks.empty();
}

bool ScopDetection::canUseISLTripCount(Loop *L, DetectionContext &Context) {
  bool OldIsInvalid = Context.IsInvalid;

  SmallVector<BasicBlock *, 4> LoopControlBlocks;
  L->getExitingBlocks(LoopControlBlocks);
  L->getLoopLatches(LoopControlBlocks);
  for (BasicBlock *ControlBB : LoopControlBlocks) {
    if (!isValidCFG(*ControlBB, true, false, Context)) {
      Context.IsInvalid = OldIsInvalid || Context.Log.size();
      return false;
    }
  }

  Context.IsInvalid = OldIsInvalid || Context.Log.size();
  return true;
}

bool ScopDetection::isValidLoop(Loop *L, DetectionContext &Context) {
  if (!hasExitingBlocks(L)) {
    return invalid<ReportLoopHasNoExit>(Context, true, L);
  }

  SmallVector<BasicBlock *, 4> ExitBlocks;
  L->getExitBlocks(ExitBlocks);
  BasicBlock *HotExit = nullptr;

  for (BasicBlock *ExitBB : ExitBlocks) {
    if (PollyAllowErrorBlocks && isErrorBlock(*ExitBB, Context.CurRegion)) {
      continue;
    }
    if (!HotExit) {
      HotExit = ExitBB;
      continue;
    }
    if (HotExit != ExitBB) {
      return invalid<ReportLoopHasMultipleExits>(Context, true, L);
    }
  }

  if (!HotExit) {
    return invalid<ReportLoopHasMultipleExits>(Context, true, L);
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
  return invalid<ReportLoopBound>(Context, true, L, LoopCount);
}

ScopDetection::LoopStats ScopDetection::countBeneficialSubLoops(Loop *L, ScalarEvolution &SE,
                                                                unsigned MinProfitableTrips) {
  const SCEV *TripCount = SE.getBackedgeTakenCount(L);

  int NumLoops = 1;
  int MaxLoopDepth = 1;
  if (MinProfitableTrips > 0) {
    if (auto *TripCountC = dyn_cast<SCEVConstant>(TripCount)) {
      if (TripCountC->getType()->getScalarSizeInBits() <= 64) {
        if (TripCountC->getValue()->getZExtValue() <= MinProfitableTrips) {
          NumLoops -= 1;
        }
      }
    }
  }

  for (auto &SubLoop : *L) {
    LoopStats Stats = countBeneficialSubLoops(SubLoop, SE, MinProfitableTrips);
    NumLoops += Stats.NumLoops;
    MaxLoopDepth = std::max(MaxLoopDepth, Stats.MaxDepth + 1);
  }

  return {NumLoops, MaxLoopDepth};
}

ScopDetection::LoopStats ScopDetection::countBeneficialLoops(Region *R, ScalarEvolution &SE, LoopInfo &LI,
                                                             unsigned MinProfitableTrips) {
  int LoopNum = 0;
  int MaxLoopDepth = 0;

  auto *L = LI.getLoopFor(R->getEntry());
  if (L && R->contains(L)) {
    L = R->outermostLoopInRegion(L);
    L = L->getParentLoop();
  }

  auto SubLoops = L ? L->getSubLoopsVector() : std::vector<Loop *>(LI.begin(), LI.end());

  for (auto &SubLoop : SubLoops) {
    if (R->contains(SubLoop)) {
      LoopStats Stats = countBeneficialSubLoops(SubLoop, SE, MinProfitableTrips);
      LoopNum += Stats.NumLoops;
      MaxLoopDepth = std::max(MaxLoopDepth, Stats.MaxDepth);
    }
  }

  return {LoopNum, MaxLoopDepth};
}

static bool isErrorBlockImpl(BasicBlock &BB, const Region &R, LoopInfo &LI, const DominatorTree &DT) {
  if (isa<UnreachableInst>(BB.getTerminator())) {
    return true;
  }

  if (LI.isLoopHeader(&BB)) {
    return false;
  }

  if (!R.contains(&BB)) {
    return false;
  }

  bool DominatesAllPredecessors = true;
  if (R.isTopLevelRegion()) {
    for (BasicBlock &I : *R.getEntry()->getParent()) {
      if (isa<ReturnInst>(I.getTerminator()) && !DT.dominates(&BB, &I)) {
        DominatesAllPredecessors = false;
        break;
      }
    }
  } else {
    for (auto *Pred : predecessors(R.getExit())) {
      if (R.contains(Pred) && !DT.dominates(&BB, Pred)) {
        DominatesAllPredecessors = false;
        break;
      }
    }
  }

  if (DominatesAllPredecessors) {
    return false;
  }

  for (Instruction &Inst : BB) {
    if (auto *CI = dyn_cast<CallInst>(&Inst)) {
      if (isDebugCall(CI)) {
        continue;
      }
      if (isIgnoredIntrinsic(CI)) {
        continue;
      }
      if (isa<MemSetInst>(CI) || isa<MemTransferInst>(CI)) {
        continue;
      }
      if (!CI->doesNotAccessMemory()) {
        return true;
      }
      if (CI->doesNotReturn()) {
        return true;
      }
    }
  }

  return false;
}

bool ScopDetection::isErrorBlock(llvm::BasicBlock &BB, const llvm::Region &R) {
  if (!PollyAllowErrorBlocks) {
    return false;
  }

  auto InsertResult = ErrorBlockCache.insert({std::make_pair(&BB, &R), false});
  if (!InsertResult.second) {
    return InsertResult.first->second;
  }

  bool Result = isErrorBlockImpl(BB, R, LI, DT);
  InsertResult.first->second = Result;
  return Result;
}

Region *ScopDetection::expandRegion(Region &R) {
  std::unique_ptr<Region> LastValidRegion;
  auto ExpandedRegion = std::unique_ptr<Region>(R.getExpandedRegion());

  POLLY_DEBUG(dbgs() << "\tExpanding " << R.getNameStr() << "\n");

  while (ExpandedRegion) {
    BBPair P = getBBPairForRegion(ExpandedRegion.get());
    std::unique_ptr<DetectionContext> &Entry = DetectionContextMap[P];
    Entry = std::make_unique<DetectionContext>(*ExpandedRegion, AA, false);
    DetectionContext &Context = *Entry;

    POLLY_DEBUG(dbgs() << "\t\tTrying " << ExpandedRegion->getNameStr() << "\n");

    if (!Context.Log.hasErrors()) {
      if (!allBlocksValid(Context) || Context.Log.hasErrors()) {
        removeCachedResults(*ExpandedRegion);
        DetectionContextMap.erase(P);
        break;
      }

      if (LastValidRegion) {
        removeCachedResults(*LastValidRegion);
        DetectionContextMap.erase(P);
      }
      LastValidRegion = std::move(ExpandedRegion);
      ExpandedRegion = std::unique_ptr<Region>(LastValidRegion->getExpandedRegion());

    } else {
      removeCachedResults(*ExpandedRegion);
      DetectionContextMap.erase(P);
      ExpandedRegion = std::unique_ptr<Region>(ExpandedRegion->getExpandedRegion());
    }
  }

  POLLY_DEBUG({
    if (LastValidRegion) {
      dbgs() << "\tto " << LastValidRegion->getNameStr() << "\n";
    } else {
      dbgs() << "\tExpanding " << R.getNameStr() << " failed\n";
    }
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

void ScopDetection::removeCachedResults(const Region &R) { ValidRegions.remove(&R); }

void ScopDetection::findScops(Region &R) {
  std::unique_ptr<DetectionContext> &Entry = DetectionContextMap[getBBPairForRegion(&R)];
  Entry = std::make_unique<DetectionContext>(R, AA, false);
  DetectionContext &Context = *Entry;

  bool DidBailout = true;
  if (!PollyProcessUnprofitable && regionWithoutLoops(R, LI)) {
    invalid<ReportUnprofitable>(Context, true, &R);
  } else {
    DidBailout = !isValidRegion(Context);
  }

  (void)DidBailout;
  if (KeepGoing) {
    assert((!DidBailout || Context.IsInvalid) &&
           "With -polly-detect-keep-going, it is sufficient that if isValidRegion short-circuited, that SCoP is invalid");
  } else {
    assert(DidBailout == Context.IsInvalid && "isValidRegion must short-circuit iff the ScoP is invalid");
  }

  if (!Context.IsInvalid) {
    ValidRegions.insert(&R);
    return;
  }

  removeCachedResults(R);

  std::vector<Region *> RegionsToExpand;
  unsigned SubRegionCounter = 0;

  for (auto &SubRegion : R) {
    if (++SubRegionCounter > PollyMaxSubRegions) {
      POLLY_DEBUG(dbgs() << "Polly: Region '" << R.getNameStr() << "' has too many sub-regions (>" << PollyMaxSubRegions
                         << "). Skipping recursion.\n");
      return;
    }
    findScops(*SubRegion);
    RegionsToExpand.push_back(SubRegion.get());
  }

  for (Region *CurrentRegion : RegionsToExpand) {
    if (!ValidRegions.count(CurrentRegion)) {
      continue;
    }
    if (lookupRejectionLog(CurrentRegion)->hasErrors()) {
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

  for (const BasicBlock *BB : CurRegion.blocks()) {
    Loop *L = LI.getLoopFor(BB);
    if (L && L->getHeader() == BB) {
      if (CurRegion.contains(L)) {
        if (!isValidLoop(L, Context)) {
          Context.IsInvalid = true;
          if (!KeepGoing) {
            return false;
          }
        }
      } else {
        SmallVector<BasicBlock *, 1> Latches;
        L->getLoopLatches(Latches);
        for (BasicBlock *Latch : Latches) {
          if (CurRegion.contains(Latch)) {
            return invalid<ReportLoopOnlySomeLatches>(Context, true, L);
          }
        }
      }
    }
  }

  for (BasicBlock *BB : CurRegion.blocks()) {
    bool IsErrorBlock = isErrorBlock(*BB, CurRegion);

    if (!isValidCFG(*BB, false, IsErrorBlock, Context) && !KeepGoing) {
      return false;
    }

    if (IsErrorBlock) {
      continue;
    }

    for (BasicBlock::iterator I = BB->begin(), E = --BB->end(); I != E; ++I) {
      if (!isValidInstruction(*I, Context)) {
        Context.IsInvalid = true;
        if (!KeepGoing) {
          return false;
        }
      }
    }
  }

  if (!hasAffineMemoryAccesses(Context)) {
    return false;
  }

  return true;
}

bool ScopDetection::hasSufficientCompute(DetectionContext &Context, int NumLoops) const {
  int InstCount = 0;
  if (NumLoops == 0) {
    return false;
  }

  for (auto *BB : Context.CurRegion.blocks()) {
    if (Context.CurRegion.contains(LI.getLoopFor(BB))) {
      InstCount += BB->size();
    }
  }

  InstCount = InstCount / NumLoops;
  return InstCount >= ProfitabilityMinPerLoopInstructions;
}

bool ScopDetection::hasPossiblyDistributableLoop(DetectionContext &Context) const {
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
      StmtsWithStoresInLoops += MemStore;
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

  if (!Context.hasStores && !Context.hasLoads) {
    return invalid<ReportUnprofitable>(Context, true, &CurRegion);
  }

  int NumLoops = countBeneficialLoops(&CurRegion, SE, LI, MIN_LOOP_TRIP_COUNT).NumLoops;
  int NumAffineLoops = NumLoops - Context.BoxedLoopsSet.size();

  if (NumAffineLoops >= 2) {
    return true;
  }

  if (NumAffineLoops == 1) {
    if (hasPossiblyDistributableLoop(Context)) {
      return true;
    }
    if (!Context.hasStores) {
      return invalid<ReportUnprofitable>(Context, true, &CurRegion);
    }

    unsigned MemOpCount = 0;
    for (BasicBlock *BB : CurRegion.blocks()) {
      const Loop *L = LI.getLoopFor(BB);
      if (L && CurRegion.contains(L) && !Context.BoxedLoopsSet.count(L)) {
        for (Instruction &Inst : *BB) {
          if (Inst.mayReadOrWriteMemory()) {
            if (isa<AllocaInst>(Inst) || isDebugCall(&Inst)) {
              continue;
            }
            MemOpCount++;
          }
        }
      }
    }
    if (MemOpCount >= PollyMinMemops) {
      return true;
    }
  }

  return invalid<ReportUnprofitable>(Context, true, &CurRegion);
}

bool ScopDetection::isValidRegion(DetectionContext &Context) {
  Region &CurRegion = Context.CurRegion;

  POLLY_DEBUG(dbgs() << "Checking region: " << CurRegion.getNameStr() << "\n\t");

  if (!PollyAllowFullFunction && CurRegion.isTopLevelRegion()) {
    POLLY_DEBUG(dbgs() << "Top level region is invalid\n");
    Context.IsInvalid = true;
    return false;
  }

  DebugLoc DbgLoc;
  if (CurRegion.getExit() && isa<UnreachableInst>(CurRegion.getExit()->getTerminator())) {
    POLLY_DEBUG(dbgs() << "Unreachable in exit\n");
    return invalid<ReportUnreachableInExit>(Context, true, CurRegion.getExit(), DbgLoc);
  }

  if (!OnlyRegion.empty() && !CurRegion.getEntry()->getName().count(OnlyRegion)) {
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
      return invalid<ReportIndirectPredecessor>(Context, true, PredTerm, PredTerm->getDebugLoc());
    }
  }

  if (!PollyAllowFullFunction &&
      CurRegion.getEntry() == &(CurRegion.getEntry()->getParent()->getEntryBlock())) {
    return invalid<ReportEntry>(Context, true, CurRegion.getEntry());
  }

  if (!allBlocksValid(Context)) {
    Context.IsInvalid = true;
    return false;
  }

  if (!isReducibleRegion(CurRegion, DbgLoc)) {
    return invalid<ReportIrreducibleRegion>(Context, true, &CurRegion, DbgLoc);
  }

  POLLY_DEBUG(dbgs() << "OK\n");
  return true;
}

void ScopDetection::markFunctionAsInvalid(Function *F) { F->addFnAttr(PollySkipFnAttr); }

bool ScopDetection::isValidFunction(Function &F) {
  if (F.hasFnAttribute(PollySkipFnAttr) || F.hasFnAttribute(Attribute::OptimizeNone)) {
    return false;
  }

  for (BasicBlock &BB : F) {
    const Instruction *Terminator = BB.getTerminator();
    if (isa<InvokeInst>(Terminator) || isa<CatchSwitchInst>(Terminator) || isa<CatchReturnInst>(Terminator) ||
        isa<CleanupReturnInst>(Terminator)) {
      return false;
    }
    if (const auto *SI = dyn_cast<SwitchInst>(Terminator)) {
      if (SI->getNumCases() > PollyMaxSwitchCases) {
        return false;
      }
    }
  }

  return true;
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
  enum Color { WHITE, GREY, BLACK };

  BasicBlock *REntry = R.getEntry();
  BasicBlock *RExit = R.getExit();

  DenseMap<const BasicBlock *, Color> BBColorMap;
  for (auto *BB : R.blocks()) {
    BBColorMap[BB] = WHITE;
  }

  using StackElem = std::pair<BasicBlock *, unsigned>;
  SmallVector<StackElem, 32> DFSStack;

  BasicBlock *CurrBB = REntry;
  BBColorMap[CurrBB] = GREY;
  DFSStack.push_back({CurrBB, 0});

  while (!DFSStack.empty()) {
    auto [BB, NextIdx] = DFSStack.pop_back_val();
    CurrBB = BB;
    unsigned AdjacentBlockIndex = NextIdx;

    const Instruction *TInst = CurrBB->getTerminator();
    unsigned NSucc = TInst->getNumSuccessors();

    for (unsigned I = AdjacentBlockIndex; I < NSucc; ++I, ++AdjacentBlockIndex) {
      BasicBlock *SuccBB = TInst->getSuccessor(I);

      if (SuccBB == RExit || SuccBB == CurrBB) {
        continue;
      }

      Color &SuccColor = BBColorMap[SuccBB];
      if (SuccColor == WHITE) {
        DFSStack.push_back({CurrBB, I + 1});
        DFSStack.push_back({SuccBB, 0});
        SuccColor = GREY;
        goto NextIteration;
      } else if (SuccColor == GREY) {
        if (!DT.dominates(SuccBB, CurrBB)) {
          DbgLoc = TInst->getDebugLoc();
          return false;
        }
      }
    }

    BBColorMap[CurrBB] = BLACK;
  NextIteration:
    (void)0;
  }

  return true;
}

static void updateLoopCountStatistic(ScopDetection::LoopStats Stats, bool OnlyProfitable) {
  if (!OnlyProfitable) {
    NumLoopsInScop += Stats.NumLoops;
    MaxNumLoopsInScop = std::max(MaxNumLoopsInScop.getValue(), (uint64_t)Stats.NumLoops);
    if (Stats.MaxDepth == 0) {
      NumScopsDepthZero++;
    } else if (Stats.MaxDepth == 1) {
      NumScopsDepthOne++;
    } else if (Stats.MaxDepth == 2) {
      NumScopsDepthTwo++;
    } else if (Stats.MaxDepth == 3) {
      NumScopsDepthThree++;
    } else if (Stats.MaxDepth == 4) {
      NumScopsDepthFour++;
    } else if (Stats.MaxDepth == 5) {
      NumScopsDepthFive++;
    } else {
      NumScopsDepthLarger++;
    }
  } else {
    NumLoopsInProfScop += Stats.NumLoops;
    MaxNumLoopsInProfScop = std::max(MaxNumLoopsInProfScop.getValue(), (uint64_t)Stats.NumLoops);
    if (Stats.MaxDepth == 0) {
      NumProfScopsDepthZero++;
    } else if (Stats.MaxDepth == 1) {
      NumProfScopsDepthOne++;
    } else if (Stats.MaxDepth == 2) {
      NumProfScopsDepthTwo++;
    } else if (Stats.MaxDepth == 3) {
      NumProfScopsDepthThree++;
    } else if (Stats.MaxDepth == 4) {
      NumProfScopsDepthFour++;
    } else if (Stats.MaxDepth == 5) {
      NumProfScopsDepthFive++;
    } else {
      NumProfScopsDepthLarger++;
    }
  }
}

ScopDetection::DetectionContext *ScopDetection::getDetectionContext(const Region *R) const {
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
  DetectionContext Context(const_cast<Region &>(R), AA, true);
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

bool ScopDetectionWrapperPass::runOnFunction(Function &F) {
  auto &LI = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
  auto &RI = getAnalysis<RegionInfoPass>().getRegionInfo();
  auto &AA = getAnalysis<AAResultsWrapperPass>().getAAResults();
  auto &SE = getAnalysis<ScalarEvolutionWrapperPass>().getSE();
  auto &DT = getAnalysis<DominatorTreeWrapperPass>().getDomTree();
  auto &ORE = getAnalysis<OptimizationRemarkEmitterWrapperPass>().getORE();

  if (F.size() > PollyScopsMaxBlocks) {
    LLVM_DEBUG(dbgs() << "Polly: Function '" << F.getName() << "' is too large (" << F.size() << " basic blocks > "
                      << PollyScopsMaxBlocks << ") for Scop analysis. Skipping.\n");
    ORE.emit([&]() {
      return OptimizationRemarkMissed("polly-detect", "FunctionTooLarge", &F)
             << "Polly analysis skipped: function is too large ("
             << ore::NV("NumBlocks", F.size()) << " basic blocks, limit is "
             << ore::NV("BlockLimit", PollyScopsMaxBlocks) << ").";
    });
    return false;
  }

  Result = std::make_unique<ScopDetection>(DT, SE, LI, RI, AA, ORE);
  Result->detect(F);
  return false;
}

void ScopDetectionWrapperPass::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<LoopInfoWrapperPass>();
  AU.addRequiredTransitive<ScalarEvolutionWrapperPass>();
  AU.addRequired<DominatorTreeWrapperPass>();
  AU.addRequired<OptimizationRemarkEmitterWrapperPass>();
  AU.addRequiredTransitive<AAResultsWrapperPass>();
  AU.addRequiredTransitive<RegionInfoPass>();
  AU.setPreservesAll();
}

void ScopDetectionWrapperPass::print(raw_ostream &OS, const Module *) const {
  for (const Region *R : Result->ValidRegions) {
    OS << "Valid Region for Scop: " << R->getNameStr() << '\n';
  }
  OS << "\n";
}

ScopDetectionWrapperPass::ScopDetectionWrapperPass() : FunctionPass(ID) {
  if (IgnoreAliasing) {
    PollyUseRuntimeAliasChecks = false;
  }
}

ScopAnalysis::ScopAnalysis() {
  if (IgnoreAliasing) {
    PollyUseRuntimeAliasChecks = false;
  }
}

void ScopDetectionWrapperPass::releaseMemory() { Result.reset(); }

char ScopDetectionWrapperPass::ID;

AnalysisKey ScopAnalysis::Key;

ScopDetection ScopAnalysis::run(Function &F, FunctionAnalysisManager &FAM) {
  auto &LI = FAM.getResult<LoopAnalysis>(F);
  auto &RI = FAM.getResult<RegionInfoAnalysis>(F);
  auto &AA = FAM.getResult<AAManager>(F);
  auto &SE = FAM.getResult<ScalarEvolutionAnalysis>(F);
  auto &DT = FAM.getResult<DominatorTreeAnalysis>(F);
  auto &ORE = FAM.getResult<OptimizationRemarkEmitterAnalysis>(F);

  if (F.size() > PollyScopsMaxBlocks) {
    LLVM_DEBUG(dbgs() << "Polly: Function '" << F.getName() << "' is too large (" << F.size() << " basic blocks > "
                      << PollyScopsMaxBlocks << ") for Scop analysis. Skipping.\n");
    ORE.emit([&]() {
      return OptimizationRemarkMissed("polly-detect", "FunctionTooLarge", &F)
             << "Polly analysis skipped: function is too large ("
             << ore::NV("NumBlocks", F.size()) << " basic blocks, limit is "
             << ore::NV("BlockLimit", PollyScopsMaxBlocks) << ").";
    });
    return ScopDetection(DT, SE, LI, RI, AA, ORE);
  }

  ScopDetection Result(DT, SE, LI, RI, AA, ORE);
  Result.detect(F);
  return Result;
}

PreservedAnalyses ScopAnalysisPrinterPass::run(Function &F, FunctionAnalysisManager &FAM) {
  OS << "Detected Scops in Function " << F.getName() << "\n";
  auto &SD = FAM.getResult<ScopAnalysis>(F);
  for (const Region *R : SD.ValidRegions) {
    OS << "Valid Region for Scop: " << R->getNameStr() << '\n';
  }
  OS << "\n";
  return PreservedAnalyses::all();
}

Pass *polly::createScopDetectionWrapperPassPass() { return new ScopDetectionWrapperPass(); }

INITIALIZE_PASS_BEGIN(ScopDetectionWrapperPass, "polly-detect",
                      "Polly - Detect static control parts (SCoPs)", false, false);
INITIALIZE_PASS_DEPENDENCY(AAResultsWrapperPass);
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass);
INITIALIZE_PASS_DEPENDENCY(RegionInfoPass);
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass);
INITIALIZE_PASS_DEPENDENCY(ScalarEvolutionWrapperPass);
INITIALIZE_PASS_DEPENDENCY(OptimizationRemarkEmitterWrapperPass);
INITIALIZE_PASS_END(ScopDetectionWrapperPass, "polly-detect",
                    "Polly - Detect static control parts (SCoPs)", false, false)

namespace {

class ScopDetectionPrinterLegacyPass final : public FunctionPass {
public:
  static char ID;

  ScopDetectionPrinterLegacyPass() : ScopDetectionPrinterLegacyPass(outs()) {}
  explicit ScopDetectionPrinterLegacyPass(llvm::raw_ostream &OS) : FunctionPass(ID), OS(OS) {}

  bool runOnFunction(Function &F) override {
    ScopDetectionWrapperPass &P = getAnalysis<ScopDetectionWrapperPass>();

    OS << "Printing analysis '" << P.getPassName() << "' for function '" << F.getName() << "':\n";
    P.print(OS);
    return false;
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    FunctionPass::getAnalysisUsage(AU);
    AU.addRequired<ScopDetectionWrapperPass>();
    AU.setPreservesAll();
  }

private:
  llvm::raw_ostream &OS;
};

char ScopDetectionPrinterLegacyPass::ID = 0;

} // namespace

Pass *polly::createScopDetectionPrinterLegacyPass(raw_ostream &OS) {
  return new ScopDetectionPrinterLegacyPass(OS);
}

INITIALIZE_PASS_BEGIN(ScopDetectionPrinterLegacyPass, "polly-print-detect",
                      "Polly - Print static control parts (SCoPs)", false, false);
INITIALIZE_PASS_DEPENDENCY(ScopDetectionWrapperPass);
INITIALIZE_PASS_END(ScopDetectionPrinterLegacyPass, "polly-print-detect",
                    "Polly - Print static control parts (SCoPs)", false, false)
