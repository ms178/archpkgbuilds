//===- ForwardOpTree.h ------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// Move instructions between statements.
//
//===----------------------------------------------------------------------===//

#include "polly/ForwardOpTree.h"
#include "polly/Options.h"
#include "polly/ScopBuilder.h"
#include "polly/ScopInfo.h"
#include "polly/ScopPass.h"
#include "polly/Support/GICHelper.h"
#include "polly/Support/ISLOStream.h"
#include "polly/Support/ISLTools.h"
#include "polly/Support/VirtualInstruction.h"
#include "polly/ZoneAlgo.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"
#include "llvm/InitializePasses.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "isl/ctx.h"
#include "isl/isl-noexceptions.h"
#include <cassert>
#include <functional>
#include <memory>

#include "polly/Support/PollyDebug.h"
#define DEBUG_TYPE "polly-optree"

using namespace llvm;
using namespace polly;

static cl::opt<bool>
    AnalyzeKnown("polly-optree-analyze-known",
                 cl::desc("Analyze array contents for load forwarding"),
                 cl::cat(PollyCategory), cl::init(true), cl::Hidden);

static cl::opt<bool>
    NormalizePHIs("polly-optree-normalize-phi",
                  cl::desc("Replace PHIs by their incoming values"),
                  cl::cat(PollyCategory), cl::init(false), cl::Hidden);

static cl::opt<unsigned>
    MaxOps("polly-optree-max-ops",
           cl::desc("Maximum number of ISL operations to invest for known "
                    "analysis; 0=no limit"),
           cl::init(1000000), cl::cat(PollyCategory), cl::Hidden);

STATISTIC(KnownAnalyzed, "Number of successfully analyzed SCoPs");
STATISTIC(KnownOutOfQuota,
          "Analyses aborted because max_operations was reached");

STATISTIC(TotalInstructionsCopied, "Number of copied instructions");
STATISTIC(TotalKnownLoadsForwarded,
          "Number of forwarded loads because their value was known");
STATISTIC(TotalReloads, "Number of reloaded values");
STATISTIC(TotalReadOnlyCopied, "Number of copied read-only accesses");
STATISTIC(TotalForwardedTrees, "Number of forwarded operand trees");
STATISTIC(TotalModifiedStmts,
          "Number of statements with at least one forwarded tree");

STATISTIC(ScopsModified, "Number of SCoPs with at least one forwarded tree");

STATISTIC(NumValueWrites, "Number of scalar value writes after OpTree");
STATISTIC(NumValueWritesInLoops,
          "Number of scalar value writes nested in affine loops after OpTree");
STATISTIC(NumPHIWrites, "Number of scalar phi writes after OpTree");
STATISTIC(NumPHIWritesInLoops,
          "Number of scalar phi writes nested in affine loops after OpTree");
STATISTIC(NumSingletonWrites, "Number of singleton writes after OpTree");
STATISTIC(NumSingletonWritesInLoops,
          "Number of singleton writes nested in affine loops after OpTree");

namespace {

enum ForwardingDecision {
  FD_Unknown,
  FD_CannotForward,
  FD_CanForwardLeaf,
  FD_CanForwardProfitably,
  FD_NotApplicable
};

struct ForwardingAction {
  using KeyTy = std::pair<Value *, ScopStmt *>;

  ForwardingDecision Decision = FD_Unknown;

  std::function<bool()> Execute = []() -> bool {
    llvm_unreachable("unspecified how to forward");
  };

  SmallVector<KeyTy, 4> Depends;

  static ForwardingAction notApplicable() {
    ForwardingAction Result;
    Result.Decision = FD_NotApplicable;
    return Result;
  }

  static ForwardingAction cannotForward() {
    ForwardingAction Result;
    Result.Decision = FD_CannotForward;
    return Result;
  }

  static ForwardingAction triviallyForwardable(bool IsProfitable, Value *Val) {
    ForwardingAction Result;
    Result.Decision =
        IsProfitable ? FD_CanForwardProfitably : FD_CanForwardLeaf;
    Result.Execute = [=]() {
      POLLY_DEBUG(dbgs() << "    trivially forwarded: " << *Val << "\n");
      return true;
    };
    return Result;
  }

  static ForwardingAction canForward(std::function<bool()> Execute,
                                     ArrayRef<KeyTy> Depends,
                                     bool IsProfitable) {
    ForwardingAction Result;
    Result.Decision =
        IsProfitable ? FD_CanForwardProfitably : FD_CanForwardLeaf;
    Result.Execute = std::move(Execute);
    Result.Depends.append(Depends.begin(), Depends.end());
    return Result;
  }
};

class ForwardOpTreeImpl final : ZoneAlgorithm {
private:
  using MemoizationTy = DenseMap<ForwardingAction::KeyTy, ForwardingAction>;

  IslMaxOperationsGuard &MaxOpGuard;

  int NumInstructionsCopied = 0;
  int NumKnownLoadsForwarded = 0;
  int NumReloads = 0;
  int NumReadOnlyCopied = 0;
  int NumForwardedTrees = 0;
  int NumModifiedStmts = 0;
  bool Modified = false;

  MemoizationTy ForwardingActions;

  isl::union_map Known;
  isl::union_map Translator;

  isl::union_map findSameContentElements(isl::union_map ValInst) {
    assert(!ValInst.is_single_valued().is_false());

    isl::union_set Domain = ValInst.domain();
    isl::union_map Schedule = getScatterFor(Domain);

    isl::union_map MustKnownCurried =
        convertZoneToTimepoints(Known, isl::dim::in, false, true).curry();

    isl::union_map DomValSched = ValInst.domain_map().apply_range(Schedule);

    isl::union_map SchedValDomVal =
        DomValSched.range_product(ValInst.range_map()).reverse();

    isl::union_map MustKnownInst = MustKnownCurried.apply_range(SchedValDomVal);

    isl::union_map MustKnownMap =
        MustKnownInst.uncurry().domain().unwrap().reverse();
    simplify(MustKnownMap);
    return MustKnownMap;
  }

  isl::map singleLocation(isl::union_map MustKnown, isl::set Domain) {
    isl::map Result;
    Domain = Domain.intersect_params(S->getContext());

    for (isl::map Map : MustKnown.get_map_list()) {
      isl::id ArrayId = Map.get_tuple_id(isl::dim::out);
      ScopArrayInfo *SAI = static_cast<ScopArrayInfo *>(ArrayId.get_user());

      if (SAI->getBasePtrOriginSAI())
        continue;

      isl::set MapDom = Map.domain();
      if (!Domain.is_subset(MapDom).is_true())
        continue;

      Result = Map.lexmin();
      break;
    }

    return Result;
  }

public:
  ForwardOpTreeImpl(Scop *S, LoopInfo *LI, IslMaxOperationsGuard &MaxOpGuard)
      : ZoneAlgorithm("polly-optree", S, LI), MaxOpGuard(MaxOpGuard) {}

  bool computeKnownValues() {
    collectCompatibleElts();

    {
      IslQuotaScope QuotaScope = MaxOpGuard.enter();

      computeCommon();
      if (NormalizePHIs)
        computeNormalizedPHIs();
      Known = computeKnown(true, true);

      Translator = makeIdentityMap(Known.range(), false);
    }

    if (Known.is_null() || Translator.is_null() || NormalizeMap.is_null()) {
      assert(isl_ctx_last_error(IslCtx.get()) == isl_error_quota);
      Known = {};
      Translator = {};
      NormalizeMap = {};
      POLLY_DEBUG(dbgs() << "Known analysis exceeded max_operations\n");
      return false;
    }

    KnownAnalyzed++;
    POLLY_DEBUG(dbgs() << "All known: " << Known << "\n");
    return true;
  }

  void printStatistics(raw_ostream &OS, int Indent = 0) {
    OS.indent(Indent) << "Statistics {\n";
    OS.indent(Indent + 4) << "Instructions copied: " << NumInstructionsCopied
                          << '\n';
    OS.indent(Indent + 4) << "Known loads forwarded: " << NumKnownLoadsForwarded
                          << '\n';
    OS.indent(Indent + 4) << "Reloads: " << NumReloads << '\n';
    OS.indent(Indent + 4) << "Read-only accesses copied: " << NumReadOnlyCopied
                          << '\n';
    OS.indent(Indent + 4) << "Operand trees forwarded: " << NumForwardedTrees
                          << '\n';
    OS.indent(Indent + 4) << "Statements with forwarded operand trees: "
                          << NumModifiedStmts << '\n';
    OS.indent(Indent) << "}\n";
  }

  void printStatements(raw_ostream &OS, int Indent = 0) const {
    OS.indent(Indent) << "After statements {\n";
    for (auto &Stmt : *S) {
      OS.indent(Indent + 4) << Stmt.getBaseName() << "\n";
      for (auto *MA : Stmt)
        MA->print(OS);

      OS.indent(Indent + 12);
      Stmt.printInstructions(OS);
    }
    OS.indent(Indent) << "}\n";
  }

  MemoryAccess *makeReadArrayAccess(ScopStmt *Stmt, LoadInst *LI,
                                    isl::map AccessRelation) {
    isl::id ArrayId = AccessRelation.get_tuple_id(isl::dim::out);
    ScopArrayInfo *SAI = reinterpret_cast<ScopArrayInfo *>(ArrayId.get_user());

    SmallVector<const SCEV *, 4> Sizes;
    Sizes.reserve(SAI->getNumberOfDimensions());
    SmallVector<const SCEV *, 4> Subscripts;
    Subscripts.reserve(SAI->getNumberOfDimensions());
    for (unsigned i = 0; i < SAI->getNumberOfDimensions(); i += 1) {
      Sizes.push_back(SAI->getDimensionSize(i));
      Subscripts.push_back(nullptr);
    }

    MemoryAccess *Access =
        new MemoryAccess(Stmt, LI, MemoryAccess::READ, SAI->getBasePtr(),
                         LI->getType(), true, {}, Sizes, LI, MemoryKind::Array);
    S->addAccessFunction(Access);
    Stmt->addAccess(Access, true);

    Access->setNewAccessRelation(AccessRelation);
    return Access;
  }

  ForwardingAction forwardKnownLoad(ScopStmt *TargetStmt, Instruction *Inst,
                                    ScopStmt *UseStmt, Loop *UseLoop,
                                    ScopStmt *DefStmt, Loop *DefLoop) {
    if (Known.is_null() || Translator.is_null() || MaxOpGuard.hasQuotaExceeded())
      return ForwardingAction::notApplicable();

    LoadInst *LI = dyn_cast<LoadInst>(Inst);
    if (!LI)
      return ForwardingAction::notApplicable();

    ForwardingDecision OpDecision =
        forwardTree(TargetStmt, LI->getPointerOperand(), DefStmt, DefLoop);
    switch (OpDecision) {
    case FD_CanForwardProfitably:
    case FD_CanForwardLeaf:
      break;
    case FD_CannotForward:
      return ForwardingAction::cannotForward();
    default:
      llvm_unreachable("Shouldn't return this");
    }

    MemoryAccess *Access = TargetStmt->getArrayAccessOrNULLFor(LI);
    if (Access) {
      auto ExecAction = [this, TargetStmt, LI, Access]() -> bool {
        TargetStmt->prependInstruction(LI);
        POLLY_DEBUG(dbgs() << "    forwarded known load with preexisting MemoryAccess" << Access << "\n");
        (void)Access;
        NumKnownLoadsForwarded++;
        TotalKnownLoadsForwarded++;
        return true;
      };
      return ForwardingAction::canForward(
          ExecAction, {{LI->getPointerOperand(), DefStmt}}, true);
    }

    IslQuotaScope QuotaScope = MaxOpGuard.enter();

    isl::map ExpectedVal = makeValInst(Inst, UseStmt, UseLoop);
    assert(!isNormalized(ExpectedVal).is_false() && "LoadInsts are always normalized");

    isl::map UseToTarget = getDefToTarget(UseStmt, TargetStmt);
    isl::map TargetExpectedVal = ExpectedVal.apply_domain(UseToTarget);
    isl::union_map TranslatedExpectedVal =
        isl::union_map(TargetExpectedVal).apply_range(Translator);

    isl::union_map Candidates = findSameContentElements(TranslatedExpectedVal);
    isl::map SameVal = singleLocation(Candidates, getDomainFor(TargetStmt));
    if (SameVal.is_null())
      return ForwardingAction::notApplicable();

    POLLY_DEBUG(dbgs() << "      expected values where " << TargetExpectedVal << "\n");
    POLLY_DEBUG(dbgs() << "      candidate elements where " << Candidates << "\n");

    isl::space ValInstSpace = ExpectedVal.get_space().range();

    isl::map LocalTranslator;
    if (!ValInstSpace.is_wrapping().is_false()) {
      isl::map ValInsts = ExpectedVal.range().unwrap();
      isl::set DefDomain = ValInsts.domain();
      (void)DefDomain;

      isl::space ValSpace = ValInstSpace.unwrap().range();
      isl::map ValToVal =
          isl::map::identity(ValSpace.map_from_domain_and_range(ValSpace));

      isl::map DefToTarget = getDefToTarget(DefStmt, TargetStmt);
      LocalTranslator = DefToTarget.reverse().product(ValToVal);
      POLLY_DEBUG(dbgs() << "      local translator is " << LocalTranslator << "\n");

      if (LocalTranslator.is_null())
        return ForwardingAction::notApplicable();
    }

    auto ExecAction = [this, TargetStmt, LI, SameVal, LocalTranslator]() -> bool {
      TargetStmt->prependInstruction(LI);
      MemoryAccess *Access = makeReadArrayAccess(TargetStmt, LI, SameVal);
      POLLY_DEBUG(dbgs() << "    forwarded known load with new MemoryAccess" << Access << "\n");
      (void)Access;

      if (!LocalTranslator.is_null())
        Translator = Translator.unite(LocalTranslator);

      NumKnownLoadsForwarded++;
      TotalKnownLoadsForwarded++;
      return true;
    };
    return ForwardingAction::canForward(
        ExecAction, {{LI->getPointerOperand(), DefStmt}}, true);
  }

  ForwardingAction reloadKnownContent(ScopStmt *TargetStmt, Instruction *Inst,
                                      ScopStmt *UseStmt, Loop *UseLoop,
                                      ScopStmt *DefStmt, Loop *DefLoop) {
    if (Known.is_null() || Translator.is_null() || MaxOpGuard.hasQuotaExceeded())
      return ForwardingAction::notApplicable();

    IslQuotaScope QuotaScope = MaxOpGuard.enter();

    isl::union_map ExpectedVal = makeNormalizedValInst(Inst, UseStmt, UseLoop);
    isl::map UseToTarget = getDefToTarget(UseStmt, TargetStmt);

    isl::union_map TargetExpectedVal = ExpectedVal.apply_domain(UseToTarget);
    isl::union_map TranslatedExpectedVal =
        TargetExpectedVal.apply_range(Translator);

    isl::union_map Candidates = findSameContentElements(TranslatedExpectedVal);

    isl::map SameVal = singleLocation(Candidates, getDomainFor(TargetStmt));
    simplify(SameVal);
    if (SameVal.is_null())
      return ForwardingAction::notApplicable();

    auto ExecAction = [this, TargetStmt, Inst, SameVal]() {
      MemoryAccess *Access = TargetStmt->lookupInputAccessOf(Inst);
      if (!Access)
        Access = TargetStmt->ensureValueRead(Inst);
      Access->setNewAccessRelation(SameVal);

      POLLY_DEBUG(dbgs() << "    forwarded known content of " << *Inst << " which is " << SameVal << "\n");
      TotalReloads++;
      NumReloads++;
      return false;
    };

    return ForwardingAction::canForward(ExecAction, {}, true);
  }

  ForwardingAction forwardSpeculatable(ScopStmt *TargetStmt, Instruction *UseInst, ScopStmt *DefStmt,
                                       Loop *DefLoop) {
    if (isa<PHINode>(UseInst))
      return ForwardingAction::notApplicable();

    if (mayHaveNonDefUseDependency(*UseInst))
      return ForwardingAction::notApplicable();

    SmallVector<ForwardingAction::KeyTy, 4> Depends;
    Depends.reserve(UseInst->getNumOperands());
    for (Value *OpVal : UseInst->operand_values()) {
      ForwardingDecision OpDecision = forwardTree(TargetStmt, OpVal, DefStmt, DefLoop);
      switch (OpDecision) {
      case FD_CannotForward:
        return ForwardingAction::cannotForward();
      case FD_CanForwardLeaf:
      case FD_CanForwardProfitably:
        Depends.emplace_back(OpVal, DefStmt);
        break;
      case FD_NotApplicable:
      case FD_Unknown:
        llvm_unreachable("forwardTree should never return FD_NotApplicable/FD_Unknown");
      }
    }

    auto ExecAction = [this, TargetStmt, UseInst]() {
      TargetStmt->prependInstruction(UseInst);
      POLLY_DEBUG(dbgs() << "    forwarded speculable instruction: " << *UseInst << "\n");
      NumInstructionsCopied++;
      TotalInstructionsCopied++;
      return true;
    };
    return ForwardingAction::canForward(ExecAction, Depends, true);
  }

  ForwardingAction forwardTreeImpl(ScopStmt *TargetStmt, Value *UseVal, ScopStmt *UseStmt, Loop *UseLoop) {
    ScopStmt *DefStmt = nullptr;
    Loop *DefLoop = nullptr;

    isl::map DefToTarget;

    VirtualUse VUse = VirtualUse::create(UseStmt, UseLoop, UseVal, true);
    switch (VUse.getKind()) {
    case VirtualUse::Constant:
    case VirtualUse::Block:
    case VirtualUse::Hoisted:
      return ForwardingAction::triviallyForwardable(false, UseVal);

    case VirtualUse::Synthesizable: {
      VirtualUse TargetUse =
          VirtualUse::create(S, TargetStmt, TargetStmt->getSurroundingLoop(), UseVal, true);
      if (TargetUse.getKind() == VirtualUse::Synthesizable)
        return ForwardingAction::triviallyForwardable(false, UseVal);

      POLLY_DEBUG(dbgs() << "    Synthesizable would not be synthesizable anymore: " << *UseVal << "\n");
      return ForwardingAction::cannotForward();
    }

    case VirtualUse::ReadOnly: {
      if (!ModelReadOnlyScalars)
        return ForwardingAction::triviallyForwardable(false, UseVal);

      auto ExecAction = [this, TargetStmt, UseVal]() {
        TargetStmt->ensureValueRead(UseVal);
        POLLY_DEBUG(dbgs() << "    forwarded read-only value " << *UseVal << "\n");
        NumReadOnlyCopied++;
        TotalReadOnlyCopied++;
        return false;
      };
      return ForwardingAction::canForward(ExecAction, {}, false);
    }

    case VirtualUse::Intra:
      DefStmt = UseStmt;
      [[fallthrough]];

    case VirtualUse::Inter: {
      Instruction *Inst = cast<Instruction>(UseVal);

      if (!DefStmt) {
        DefStmt = S->getStmtFor(Inst);
        if (!DefStmt)
          return ForwardingAction::cannotForward();
      }

      DefLoop = LI->getLoopFor(Inst->getParent());

      ForwardingAction SpeculativeResult = forwardSpeculatable(TargetStmt, Inst, DefStmt, DefLoop);
      if (SpeculativeResult.Decision != FD_NotApplicable)
        return SpeculativeResult;

      ForwardingAction KnownResult =
          forwardKnownLoad(TargetStmt, Inst, UseStmt, UseLoop, DefStmt, DefLoop);
      if (KnownResult.Decision != FD_NotApplicable)
        return KnownResult;

      ForwardingAction ReloadResult =
          reloadKnownContent(TargetStmt, Inst, UseStmt, UseLoop, DefStmt, DefLoop);
      if (ReloadResult.Decision != FD_NotApplicable)
        return ReloadResult;

      POLLY_DEBUG(dbgs() << "    Cannot forward instruction: " << *Inst << "\n");
      return ForwardingAction::cannotForward();
    }
    }

    llvm_unreachable("Case unhandled");
  }

  ForwardingDecision forwardTree(ScopStmt *TargetStmt, Value *UseVal, ScopStmt *UseStmt, Loop *UseLoop) {
    auto It = ForwardingActions.find({UseVal, UseStmt});
    if (It != ForwardingActions.end())
      return It->second.Decision;

    ForwardingAction Action = forwardTreeImpl(TargetStmt, UseVal, UseStmt, UseLoop);
    ForwardingDecision Result = Action.Decision;

    assert(!ForwardingActions.count({UseVal, UseStmt}) && "circular dependency?");
    ForwardingActions.insert({{UseVal, UseStmt}, std::move(Action)});
    return Result;
  }

  void applyForwardingActions(ScopStmt *Stmt, Value *UseVal, MemoryAccess *RA) {
    using ChildItTy = decltype(std::declval<ForwardingAction>().Depends.begin());
    using EdgeTy = std::pair<ForwardingAction *, ChildItTy>;

    DenseSet<ForwardingAction::KeyTy> Visited;
    SmallVector<EdgeTy, 32> Stack;
    SmallVector<ForwardingAction *, 32> Ordered;

    assert(ForwardingActions.count({UseVal, Stmt}));
    ForwardingAction *RootAction = &ForwardingActions[{UseVal, Stmt}];
    Stack.emplace_back(RootAction, RootAction->Depends.begin());

    while (!Stack.empty()) {
      EdgeTy &Top = Stack.back();
      ForwardingAction *TopAction = Top.first;
      ChildItTy &TopEdge = Top.second;

      if (TopEdge == TopAction->Depends.end()) {
        Ordered.push_back(TopAction);
        Stack.pop_back();
        continue;
      }

      ForwardingAction::KeyTy Key = *TopEdge;
      ++TopEdge;

      auto VisitIt = Visited.insert(Key);
      if (!VisitIt.second)
        continue;

      assert(ForwardingActions.count(Key) && "Must not insert new actions during execution phase");
      ForwardingAction *ChildAction = &ForwardingActions[Key];
      Stack.emplace_back(ChildAction, ChildAction->Depends.begin());
    }

    assert(Ordered.back() == RootAction);
    if (RootAction->Execute())
      Stmt->removeSingleMemoryAccess(RA);
    Ordered.pop_back();
    for (auto DepAction : reverse(Ordered)) {
      assert(DepAction->Decision != FD_Unknown && DepAction->Decision != FD_CannotForward);
      assert(DepAction != RootAction);
      DepAction->Execute();
    }
  }

  bool tryForwardTree(MemoryAccess *RA) {
    assert(RA->isLatestScalarKind());
    POLLY_DEBUG(dbgs() << "Trying to forward operand tree " << RA << "...\n");

    ScopStmt *Stmt = RA->getStatement();
    Loop *InLoop = Stmt->getSurroundingLoop();
    (void)InLoop;

    isl::map TargetToUse;
    if (!Known.is_null()) {
      isl::space DomSpace = Stmt->getDomainSpace();
      TargetToUse = isl::map::identity(DomSpace.map_from_domain_and_range(DomSpace));
    }

    ForwardingDecision Assessment = forwardTree(Stmt, RA->getAccessValue(), Stmt, InLoop);

    bool Changed = false;
    if (Assessment == FD_CanForwardProfitably) {
      applyForwardingActions(Stmt, RA->getAccessValue(), RA);
      Changed = true;
    }

    ForwardingActions.clear();
    return Changed;
  }

  Scop *getScop() const { return S; }

  bool forwardOperandTrees() {
    for (ScopStmt &Stmt : *S) {
      bool StmtModified = false;

      SmallVector<MemoryAccess *, 16> Accs(Stmt.begin(), Stmt.end());

      for (MemoryAccess *RA : Accs) {
        if (!RA->isRead())
          continue;
        if (!RA->isLatestScalarKind())
          continue;

        if (tryForwardTree(RA)) {
          Modified = true;
          StmtModified = true;
          NumForwardedTrees++;
          TotalForwardedTrees++;
        }
      }

      if (StmtModified) {
        NumModifiedStmts++;
        TotalModifiedStmts++;
      }
    }

    if (Modified) {
      ScopsModified++;
      S->realignParams();
    }
    return Modified;
  }

  void print(raw_ostream &OS, int Indent = 0) {
    printStatistics(OS, Indent);

    if (!Modified) {
      OS << "ForwardOpTree executed, but did not modify anything\n";
      return;
    }

    printStatements(OS, Indent);
  }

  bool isModified() const { return Modified; }
};

static std::unique_ptr<ForwardOpTreeImpl> runForwardOpTree(Scop &S, LoopInfo &LI) {
  std::unique_ptr<ForwardOpTreeImpl> Impl;
  {
    IslMaxOperationsGuard MaxOpGuard(S.getIslCtx().get(), MaxOps, false);
    Impl = std::make_unique<ForwardOpTreeImpl>(&S, &LI, MaxOpGuard);

    if (AnalyzeKnown) {
      POLLY_DEBUG(dbgs() << "Prepare forwarders...\n");
      Impl->computeKnownValues();
    }

    POLLY_DEBUG(dbgs() << "Forwarding operand trees...\n");
    Impl->forwardOperandTrees();

    if (MaxOpGuard.hasQuotaExceeded()) {
      POLLY_DEBUG(dbgs() << "Not all operations completed because of max_operations exceeded\n");
      KnownOutOfQuota++;
    }
  }

  POLLY_DEBUG(dbgs() << "\nFinal Scop:\n");
  POLLY_DEBUG(dbgs() << S);

  Scop::ScopStatistics ScopStats = S.getStatistics();
  NumValueWrites += ScopStats.NumValueWrites;
  NumValueWritesInLoops += ScopStats.NumValueWritesInLoops;
  NumPHIWrites += ScopStats.NumPHIWrites;
  NumPHIWritesInLoops += ScopStats.NumPHIWritesInLoops;
  NumSingletonWrites += ScopStats.NumSingletonWrites;
  NumSingletonWritesInLoops += ScopStats.NumSingletonWritesInLoops;

  return Impl;
}

static PreservedAnalyses runForwardOpTreeUsingNPM(Scop &S, ScopAnalysisManager &SAM,
                                                  ScopStandardAnalysisResults &SAR, SPMUpdater &U,
                                                  raw_ostream *OS) {
  LoopInfo &LI = SAR.LI;

  std::unique_ptr<ForwardOpTreeImpl> Impl = runForwardOpTree(S, LI);
  if (OS) {
    *OS << "Printing analysis 'Polly - Forward operand tree' for region: '" << S.getName() << "' in function '"
        << S.getFunction().getName() << "':\n";
    if (Impl) {
      assert(Impl->getScop() == &S);
      Impl->print(*OS);
    }
  }

  if (!Impl->isModified())
    return PreservedAnalyses::all();

  PreservedAnalyses PA;
  PA.preserveSet<AllAnalysesOn<Module>>();
  PA.preserveSet<AllAnalysesOn<Function>>();
  PA.preserveSet<AllAnalysesOn<Loop>>();
  return PA;
}

class ForwardOpTreeWrapperPass final : public ScopPass {
private:
  std::unique_ptr<ForwardOpTreeImpl> Impl;

public:
  static char ID;

  explicit ForwardOpTreeWrapperPass() : ScopPass(ID) {}
  ForwardOpTreeWrapperPass(const ForwardOpTreeWrapperPass &) = delete;
  ForwardOpTreeWrapperPass &operator=(const ForwardOpTreeWrapperPass &) = delete;

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequiredTransitive<ScopInfoRegionPass>();
    AU.addRequired<LoopInfoWrapperPass>();
    AU.setPreservesAll();
  }

  bool runOnScop(Scop &S) override {
    releaseMemory();
    LoopInfo &LI = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
    Impl = runForwardOpTree(S, LI);
    return false;
  }

  void printScop(raw_ostream &OS, Scop &S) const override {
    if (!Impl)
      return;
    assert(Impl->getScop() == &S);
    Impl->print(OS);
  }

  void releaseMemory() override { Impl.reset(); }
};

char ForwardOpTreeWrapperPass::ID;

class ForwardOpTreePrinterLegacyPass final : public ScopPass {
public:
  static char ID;

  ForwardOpTreePrinterLegacyPass() : ForwardOpTreePrinterLegacyPass(outs()) {}
  explicit ForwardOpTreePrinterLegacyPass(llvm::raw_ostream &OS) : ScopPass(ID), OS(OS) {}

  bool runOnScop(Scop &S) override {
    ForwardOpTreeWrapperPass &P = getAnalysis<ForwardOpTreeWrapperPass>();
    OS << "Printing analysis '" << P.getPassName() << "' for region: '" << S.getRegion().getNameStr()
       << "' in function '" << S.getFunction().getName() << "':\n";
    P.printScop(OS, S);
    return false;
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    ScopPass::getAnalysisUsage(AU);
    AU.addRequired<ForwardOpTreeWrapperPass>();
    AU.setPreservesAll();
  }

private:
  llvm::raw_ostream &OS;
};

char ForwardOpTreePrinterLegacyPass::ID = 0;
} // namespace

Pass *polly::createForwardOpTreeWrapperPass() { return new ForwardOpTreeWrapperPass(); }

Pass *polly::createForwardOpTreePrinterLegacyPass(llvm::raw_ostream &OS) {
  return new ForwardOpTreePrinterLegacyPass(OS);
}

llvm::PreservedAnalyses ForwardOpTreePass::run(Scop &S, ScopAnalysisManager &SAM,
                                               ScopStandardAnalysisResults &SAR, SPMUpdater &U) {
  return runForwardOpTreeUsingNPM(S, SAM, SAR, U, nullptr);
}

llvm::PreservedAnalyses ForwardOpTreePrinterPass::run(Scop &S, ScopAnalysisManager &SAM,
                                                      ScopStandardAnalysisResults &SAR, SPMUpdater &U) {
  return runForwardOpTreeUsingNPM(S, SAM, SAR, U, &OS);
}

INITIALIZE_PASS_BEGIN(ForwardOpTreeWrapperPass, "polly-optree", "Polly - Forward operand tree", false, false)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_END(ForwardOpTreeWrapperPass, "polly-optree", "Polly - Forward operand tree", false, false)

INITIALIZE_PASS_BEGIN(ForwardOpTreePrinterLegacyPass, "polly-print-optree",
                      "Polly - Print forward operand tree result", false, false)
INITIALIZE_PASS_DEPENDENCY(ForwardOpTreeWrapperPass)
INITIALIZE_PASS_END(ForwardOpTreePrinterLegacyPass, "polly-print-optree",
                    "Polly - Print forward operand tree result", false, false)
