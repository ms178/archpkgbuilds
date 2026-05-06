//===- DFAJumpThreading.cpp - Threads a switch statement inside a loop ----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Transform each threading path to effectively jump thread the DFA. For
// example, the CFG below could be transformed as follows, where the cloned
// blocks unconditionally branch to the next correct case based on what is
// identified in the analysis.
//
//          sw.bb                        sw.bb
//        /   |   \                    /   |   \
//   case1  case2  case3          case1  case2  case3
//        \   |   /                 |      |      |
//       determinator            det.2   det.3  det.1
//        br sw.bb                /        |        \
//                          sw.bb.2     sw.bb.3     sw.bb.1
//                           br case2    br case3    br case1
//
// Definitions and Terminology:
//
// * Threading path:
//   a list of basic blocks, the exit state, and the block that determines
//   the next state, for which the following notation will be used:
//   < path of BBs that form a cycle > [ state, determinator ]
//
// * Predictable switch:
//   The switch variable is always a known constant so that all conditional
//   jumps based on switch variable can be converted to unconditional jump.
//
// * Determinator:
//   The basic block that determines the next state of the DFA.
//
// Representing the optimization in C-like pseudocode: the code pattern on the
// left could functionally be transformed to the right pattern if the switch
// condition is predictable.
//
//  X = A                       goto A
//  for (...)                   A:
//    switch (X)                  ...
//      case A                    goto B
//        X = B                 B:
//      case B                    ...
//        X = C                   goto C
//
// The pass first checks that switch variable X is decided by the control flow
// path taken in the loop; for example, in case B, the next value of X is
// decided to be C. It then enumerates through all paths in the loop and labels
// the basic blocks where the next state is decided.
//
// Using this information it creates new paths that unconditionally branch to
// the next case. This involves cloning code, so it only gets triggered if the
// amount of code duplicated is below a threshold.
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Scalar/DFAJumpThreading.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Analysis/AssumptionCache.h"
#include "llvm/Analysis/BlockFrequencyInfo.h"
#include "llvm/Analysis/BranchProbabilityInfo.h"
#include "llvm/Analysis/CodeMetrics.h"
#include "llvm/Analysis/DomTreeUpdater.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/OptimizationRemarkEmitter.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/SSAUpdaterBulk.h"
#include "llvm/Transforms/Utils/ValueMapper.h"
#include <cmath>
#include <deque>
#include <limits>
#include <utility>
#include <vector>

#ifdef EXPENSIVE_CHECKS
#include "llvm/IR/Verifier.h"
#endif

using namespace llvm;

#define DEBUG_TYPE "dfa-jump-threading"

STATISTIC(NumTransforms, "Number of transformations done");
STATISTIC(NumCloned, "Number of blocks cloned");
STATISTIC(NumPaths, "Number of individual paths threaded");
STATISTIC(NumHotPathsPruned,
          "Number of threading paths pruned by hot-path budget");

namespace llvm {
static cl::opt<bool>
    ClViewCfgBefore("dfa-jump-view-cfg-before",
                    cl::desc("View the CFG before DFA Jump Threading"),
                    cl::Hidden, cl::init(false));

static cl::opt<bool> EarlyExitHeuristic(
    "dfa-early-exit-heuristic",
    cl::desc("Exit early if an unpredictable value come from the same loop"),
    cl::Hidden, cl::init(true));

static cl::opt<unsigned> MaxPathLength(
    "dfa-max-path-length",
    cl::desc("Max number of blocks searched to find a threading path"),
    cl::Hidden, cl::init(20));

static cl::opt<unsigned> MaxNumVisitiedPaths(
    "dfa-max-num-visited-paths",
    cl::desc(
        "Max number of blocks visited while enumerating paths around a switch"),
    cl::Hidden, cl::init(2500));

static cl::opt<unsigned>
    MaxNumPaths("dfa-max-num-paths",
                cl::desc("Max number of paths enumerated around a switch"),
                cl::Hidden, cl::init(200));

static cl::opt<unsigned>
    CostThreshold("dfa-cost-threshold",
                  cl::desc("Maximum cost accepted for the transformation"),
                  cl::Hidden, cl::init(50));

static cl::opt<double> MaxClonedRate(
    "dfa-max-cloned-rate",
    cl::desc(
        "Maximum cloned instructions rate accepted for the transformation"),
    cl::Hidden, cl::init(7.5));

static cl::opt<unsigned>
    MaxOuterUseBlocks("dfa-max-out-use-blocks",
                      cl::desc("Maximum unduplicated blocks with outer uses "
                               "accepted for the transformation"),
                      cl::Hidden, cl::init(40));

static cl::opt<bool> HotPathHeuristic(
    "dfa-hot-path-heuristic",
    cl::desc("Prioritize and prune DFA threading paths by block frequency"),
    cl::Hidden, cl::init(true));

static cl::opt<unsigned> HotPathCloneBudgetInst(
    "dfa-hot-path-clone-budget-inst",
    cl::desc("Instruction clone budget used by hot-path pruning (0 = unlimited)"),
    cl::Hidden, cl::init(250));

static cl::opt<bool> TracePathHeuristic(
    "dfa-trace-path-heuristic",
    cl::desc("Prefer trace-like threading paths (fewer side exits/header crossings)"),
    cl::Hidden, cl::init(true));

static cl::opt<bool> EntropyProfitabilityHeuristic(
    "dfa-entropy-profitability",
    cl::desc("Adjust DFA profitability threshold by switch branch entropy"),
    cl::Hidden, cl::init(true));

static cl::opt<unsigned> RegionCloneBudgetInst(
    "dfa-region-clone-budget-inst",
    cl::desc("Absolute max instructions cloned per DFA transform (0 = unlimited)"),
    cl::Hidden, cl::init(1200));

static cl::opt<double> MaxLoopCloneRate(
    "dfa-max-loop-clone-rate",
    cl::desc("Max cloned/original instruction ratio within containing loop"),
    cl::Hidden, cl::init(2.0));

extern cl::opt<bool> ProfcheckDisableMetadataFixes;
} // namespace llvm

namespace {

class SelectInstToUnfold {
  SelectInst *SI;
  PHINode *SIUse;

public:
  SelectInstToUnfold(SelectInst *SI, PHINode *SIUse) : SI(SI), SIUse(SIUse) {}

  SelectInst *getInst() const { return SI; }
  PHINode *getUse() const { return SIUse; }

  explicit operator bool() const { return SI && SIUse; }
};

class DFAJumpThreading {
public:
  DFAJumpThreading(AssumptionCache *AC, DomTreeUpdater *DTU, LoopInfo *LI,
                   TargetTransformInfo *TTI, OptimizationRemarkEmitter *ORE,
                   BlockFrequencyInfo *BFI, BranchProbabilityInfo *BPI)
      : AC(AC), DTU(DTU), LI(LI), TTI(TTI), ORE(ORE), BFI(BFI), BPI(BPI) {}

  bool run(Function &F);
  bool LoopInfoBroken;

private:
  void unfoldSelectInstrs(
      const SmallVector<SelectInstToUnfold, 4> &SelectInsts) {
    SmallVector<SelectInstToUnfold, 16> Stack;
    Stack.reserve(SelectInsts.size() + 16);
    llvm::append_range(Stack, SelectInsts);

    while (!Stack.empty()) {
      SelectInstToUnfold SIToUnfold = Stack.pop_back_val();
      if (!SIToUnfold)
        continue;

      SmallVector<SelectInstToUnfold, 4> NewSIsToUnfold;
      SmallVector<BasicBlock *, 4> NewBBs;
      unfold(DTU, LI, SIToUnfold, NewSIsToUnfold, NewBBs);

      llvm::append_range(Stack, NewSIsToUnfold);
    }
  }

  static void unfold(DomTreeUpdater *DTU, LoopInfo *LI,
                     SelectInstToUnfold SIToUnfold,
                     SmallVectorImpl<SelectInstToUnfold> &NewSIsToUnfold,
                     SmallVectorImpl<BasicBlock *> &NewBBs);

  AssumptionCache *AC;
  DomTreeUpdater *DTU;
  LoopInfo *LI;
  TargetTransformInfo *TTI;
  OptimizationRemarkEmitter *ORE;
  BlockFrequencyInfo *BFI;
  BranchProbabilityInfo *BPI;
};
} // namespace

void DFAJumpThreading::unfold(
    DomTreeUpdater *DTU, LoopInfo *LI, SelectInstToUnfold SIToUnfold,
    SmallVectorImpl<SelectInstToUnfold> &NewSIsToUnfold,
    SmallVectorImpl<BasicBlock *> &NewBBs) {
  SelectInst *SI = SIToUnfold.getInst();
  PHINode *SIUse = SIToUnfold.getUse();
  assert(SI->hasOneUse() && "Select must have exactly one use");

  BasicBlock *StartBlock = SIUse->getIncomingBlock(*SI->use_begin());
  Instruction *StartTerm = StartBlock->getTerminator();

  if (auto *StartBlockTerm = dyn_cast<UncondBrInst>(StartTerm)) {
    BasicBlock *EndBlock = StartBlock->getUniqueSuccessor();
    BasicBlock *NewBlock = BasicBlock::Create(
        SI->getContext(), Twine(SI->getName(), ".si.unfold.false"),
        EndBlock->getParent(), EndBlock);
    NewBBs.push_back(NewBlock);
    UncondBrInst::Create(EndBlock, NewBlock);
    DTU->applyUpdates({{DominatorTree::Insert, NewBlock, EndBlock}});

    Value *SIOp1 = SI->getTrueValue();
    Value *SIOp2 = SI->getFalseValue();

    PHINode *NewPhi = PHINode::Create(
        SIUse->getType(), 1, Twine(SIOp2->getName(), ".si.unfold.phi"),
        NewBlock->getFirstInsertionPt());
    NewPhi->addIncoming(SIOp2, StartBlock);

    for (PHINode &Phi : EndBlock->phis()) {
      if (SIUse == &Phi)
        continue;
      int Idx = Phi.getBasicBlockIndex(StartBlock);
      if (Idx >= 0)
        Phi.addIncoming(Phi.getIncomingValue(Idx), NewBlock);
    }

    if (EndBlock == SIUse->getParent()) {
      SIUse->addIncoming(NewPhi, NewBlock);
      SIUse->replaceUsesOfWith(SI, SIOp1);
    } else {
      PHINode *EndPhi = PHINode::Create(
          SIUse->getType(), pred_size(EndBlock),
          Twine(SI->getName(), ".si.unfold.phi"),
          EndBlock->getFirstInsertionPt());

      for (BasicBlock *Pred : predecessors(EndBlock)) {
        if (Pred != StartBlock && Pred != NewBlock)
          EndPhi->addIncoming(EndPhi, Pred);
      }

      EndPhi->addIncoming(SIOp1, StartBlock);
      EndPhi->addIncoming(NewPhi, NewBlock);
      SIUse->replaceUsesOfWith(SI, EndPhi);
      SIUse = EndPhi;
    }

    if (auto *OpSi = dyn_cast<SelectInst>(SIOp1))
      NewSIsToUnfold.push_back(SelectInstToUnfold(OpSi, SIUse));
    if (auto *OpSi = dyn_cast<SelectInst>(SIOp2))
      NewSIsToUnfold.push_back(SelectInstToUnfold(OpSi, NewPhi));

    StartBlockTerm->eraseFromParent();
    auto *BI =
        CondBrInst::Create(SI->getCondition(), EndBlock, NewBlock, StartBlock);
    if (!ProfcheckDisableMetadataFixes)
      BI->setMetadata(LLVMContext::MD_prof,
                      SI->getMetadata(LLVMContext::MD_prof));
    DTU->applyUpdates({{DominatorTree::Insert, StartBlock, NewBlock}});

  } else if (auto *CondBr = dyn_cast<CondBrInst>(StartTerm)) {
    BasicBlock *EndBlock = SIUse->getParent();

    unsigned NumEdgesToEndBlock = 0;
    for (unsigned I = 0, E = CondBr->getNumSuccessors(); I != E; ++I)
      if (CondBr->getSuccessor(I) == EndBlock)
        ++NumEdgesToEndBlock;

    if (NumEdgesToEndBlock != 1)
      return;

    BasicBlock *NewBlockT = BasicBlock::Create(
        SI->getContext(), Twine(SI->getName(), ".si.unfold.true"),
        EndBlock->getParent(), EndBlock);
    BasicBlock *NewBlockF = BasicBlock::Create(
        SI->getContext(), Twine(SI->getName(), ".si.unfold.false"),
        EndBlock->getParent(), EndBlock);

    NewBBs.push_back(NewBlockT);
    NewBBs.push_back(NewBlockF);

    UncondBrInst::Create(EndBlock, NewBlockF);
    auto *BI =
        CondBrInst::Create(SI->getCondition(), EndBlock, NewBlockF, NewBlockT);
    if (!ProfcheckDisableMetadataFixes)
      BI->setMetadata(LLVMContext::MD_prof,
                      SI->getMetadata(LLVMContext::MD_prof));
    DTU->applyUpdates({{DominatorTree::Insert, NewBlockT, NewBlockF},
                       {DominatorTree::Insert, NewBlockT, EndBlock},
                       {DominatorTree::Insert, NewBlockF, EndBlock}});

    Value *TrueVal = SI->getTrueValue();
    Value *FalseVal = SI->getFalseValue();

    PHINode *NewPhiT = PHINode::Create(
        SIUse->getType(), 1, Twine(TrueVal->getName(), ".si.unfold.phi"),
        NewBlockT->getFirstInsertionPt());
    PHINode *NewPhiF = PHINode::Create(
        SIUse->getType(), 1, Twine(FalseVal->getName(), ".si.unfold.phi"),
        NewBlockF->getFirstInsertionPt());
    NewPhiT->addIncoming(TrueVal, StartBlock);
    NewPhiF->addIncoming(FalseVal, NewBlockT);

    if (auto *TrueSI = dyn_cast<SelectInst>(TrueVal))
      NewSIsToUnfold.push_back(SelectInstToUnfold(TrueSI, NewPhiT));
    if (auto *FalseSi = dyn_cast<SelectInst>(FalseVal))
      NewSIsToUnfold.push_back(SelectInstToUnfold(FalseSi, NewPhiF));

    SIUse->addIncoming(NewPhiT, NewBlockT);
    SIUse->addIncoming(NewPhiF, NewBlockF);

    while (SIUse->getBasicBlockIndex(StartBlock) >= 0)
      SIUse->removeIncomingValue(StartBlock, /*DeletePHIIfEmpty=*/false);

    for (PHINode &Phi : EndBlock->phis()) {
      if (SIUse == &Phi)
        continue;
      int Idx = Phi.getBasicBlockIndex(StartBlock);
      if (Idx >= 0) {
        Value *OldVal = Phi.getIncomingValue(Idx);
        Phi.addIncoming(OldVal, NewBlockT);
        Phi.addIncoming(OldVal, NewBlockF);
        while (Phi.getBasicBlockIndex(StartBlock) >= 0)
          Phi.removeIncomingValue(StartBlock, /*DeletePHIIfEmpty=*/false);
      }
    }

    unsigned SuccNum = CondBr->getSuccessor(1) == EndBlock ? 1 : 0;
    CondBr->setSuccessor(SuccNum, NewBlockT);
    DTU->applyUpdates({{DominatorTree::Delete, StartBlock, EndBlock},
                       {DominatorTree::Insert, StartBlock, NewBlockT}});
  } else {
    return;
  }

  if (Loop *L = LI->getLoopFor(StartBlock))
    for (BasicBlock *NewBB : NewBBs)
      L->addBasicBlockToLoop(NewBB, *LI);

  assert(SI->use_empty() && "Select must be dead now");
  SI->eraseFromParent();
}

namespace {
struct ClonedBlock {
  BasicBlock *BB;
  APInt State;
};
} // namespace

typedef std::deque<BasicBlock *> PathType;
typedef std::vector<PathType> PathsType;
typedef SmallPtrSet<const BasicBlock *, 8> VisitedBlocks;
typedef std::vector<ClonedBlock> CloneList;
typedef DenseMap<BasicBlock *, CloneList> DuplicateBlockMap;
typedef MapVector<Instruction *, std::vector<Instruction *>> DefMap;

inline raw_ostream &operator<<(raw_ostream &OS, const PathType &Path) {
  auto BBNames = llvm::map_range(
      Path, [](const BasicBlock *BB) { return BB->getNameOrAsOperand(); });
  OS << "< " << llvm::join(BBNames, ", ") << " >";
  return OS;
}

namespace {
struct ThreadingPath {
  APInt getExitValue() const { return ExitVal; }
  void setExitValue(const ConstantInt *V) {
    ExitVal = V->getValue();
    IsExitValSet = true;
  }
  void setExitValue(const APInt &V) {
    ExitVal = V;
    IsExitValSet = true;
  }
  bool isExitValueSet() const { return IsExitValSet; }

  const BasicBlock *getDeterminatorBB() const { return DBB; }
  void setDeterminator(const BasicBlock *BB) { DBB = BB; }

  const PathType &getPath() const { return Path; }
  void setPath(const PathType &NewPath) { Path = NewPath; }
  void push_back(BasicBlock *BB) { Path.push_back(BB); }
  void push_front(BasicBlock *BB) { Path.push_front(BB); }
  void appendExcludingFirst(const PathType &OtherPath) {
    llvm::append_range(Path, llvm::drop_begin(OtherPath));
  }

  void print(raw_ostream &OS) const {
    OS << Path << " [ " << ExitVal << ", " << DBB->getNameOrAsOperand()
       << " ]";
  }

private:
  PathType Path;
  APInt ExitVal;
  const BasicBlock *DBB = nullptr;
  bool IsExitValSet = false;
};

#ifndef NDEBUG
inline raw_ostream &operator<<(raw_ostream &OS, const ThreadingPath &TPath) {
  TPath.print(OS);
  return OS;
}
#endif

struct MainSwitch {
  MainSwitch(SwitchInst *SI, LoopInfo *LI, OptimizationRemarkEmitter *ORE)
      : LI(LI) {
    if (isCandidate(SI)) {
      Instr = SI;
    } else {
      ORE->emit([&]() {
        return OptimizationRemarkMissed(DEBUG_TYPE, "SwitchNotPredictable", SI)
               << "Switch instruction is not predictable.";
      });
    }
  }

  ~MainSwitch() = default;

  SwitchInst *getInstr() const { return Instr; }
  const SmallVector<SelectInstToUnfold, 4> &getSelectInsts() const {
    return SelectInsts;
  }

private:
  bool isCandidate(const SwitchInst *SI) {
    std::deque<std::pair<Value *, BasicBlock *>> Q;
    SmallPtrSet<Value *, 16> SeenValues;
    SelectInsts.clear();

    Value *SICond = SI->getCondition();
    LLVM_DEBUG(dbgs() << "\tSICond: " << *SICond << "\n");
    if (!isa<PHINode>(SICond))
      return false;

    const Loop *L = LI->getLoopFor(SI->getParent());
    if (!L)
      return false;

    addToQueue(SICond, nullptr, Q, SeenValues);

    while (!Q.empty()) {
      Value *Current = Q.front().first;
      BasicBlock *CurrentIncomingBB = Q.front().second;
      Q.pop_front();

      if (auto *Phi = dyn_cast<PHINode>(Current)) {
        for (BasicBlock *IncomingBB : Phi->blocks()) {
          Value *Incoming = Phi->getIncomingValueForBlock(IncomingBB);
          addToQueue(Incoming, IncomingBB, Q, SeenValues);
        }
        LLVM_DEBUG(dbgs() << "\tphi: " << *Phi << "\n");
      } else if (SelectInst *SelI = dyn_cast<SelectInst>(Current)) {
        if (!isValidSelectInst(SelI))
          return false;
        addToQueue(SelI->getTrueValue(), CurrentIncomingBB, Q, SeenValues);
        addToQueue(SelI->getFalseValue(), CurrentIncomingBB, Q, SeenValues);
        LLVM_DEBUG(dbgs() << "\tselect: " << *SelI << "\n");
        if (auto *SelIUse = dyn_cast<PHINode>(SelI->user_back()))
          SelectInsts.push_back(SelectInstToUnfold(SelI, SelIUse));
      } else if (isa<Constant>(Current)) {
        LLVM_DEBUG(dbgs() << "\tconst: " << *Current << "\n");
        continue;
      } else {
        LLVM_DEBUG(dbgs() << "\tother: " << *Current << "\n");
        if (EarlyExitHeuristic &&
            L->contains(LI->getLoopFor(CurrentIncomingBB))) {
          LLVM_DEBUG(dbgs()
                     << "\tExiting early due to unpredictability heuristic.\n");
          return false;
        }
        continue;
      }
    }

    return true;
  }

  static void addToQueue(Value *Val, BasicBlock *BB,
                         std::deque<std::pair<Value *, BasicBlock *>> &Q,
                         SmallPtrSet<Value *, 16> &SeenValues) {
    if (SeenValues.insert(Val).second)
      Q.push_back({Val, BB});
  }

  bool isValidSelectInst(SelectInst *SI) {
    if (!SI->hasOneUse())
      return false;

    Instruction *SIUse = SI->user_back();
    if (!isa<PHINode, SelectInst>(SIUse))
      return false;

    BasicBlock *SIBB = SI->getParent();
    UncondBrInst *SITerm = dyn_cast<UncondBrInst>(SIBB->getTerminator());
    if (!SITerm)
      return false;

    PHINode *PHIUser = dyn_cast<PHINode>(SIUse);
    if (PHIUser && PHIUser->getIncomingBlock(*SI->use_begin()) != SIBB)
      return false;

    for (const SelectInstToUnfold &SIToUnfold : SelectInsts) {
      SelectInst *PrevSI = SIToUnfold.getInst();
      if (PrevSI->getTrueValue() != SI && PrevSI->getFalseValue() != SI &&
          PrevSI->getParent() == SI->getParent())
        return false;
    }

    return true;
  }

  LoopInfo *LI;
  SwitchInst *Instr = nullptr;
  SmallVector<SelectInstToUnfold, 4> SelectInsts;
};

struct AllSwitchPaths {
  AllSwitchPaths(const MainSwitch *MSwitch, OptimizationRemarkEmitter *ORE,
                 LoopInfo *LI, Loop *L)
      : Switch(MSwitch->getInstr()), SwitchBlock(Switch->getParent()),
        ORE(ORE), LI(LI), SwitchOuterLoop(L) {}

  std::vector<ThreadingPath> &getThreadingPaths() { return TPaths; }
  unsigned getNumThreadingPaths() { return TPaths.size(); }
  SwitchInst *getSwitchInst() { return Switch; }
  BasicBlock *getSwitchBlock() { return SwitchBlock; }

  void run() {
    findTPaths();
    unifyTPaths();
  }

  bool destinationDistinguishesSwitchEdges(BasicBlock *Dest) const {
    for (PHINode &Phi : Dest->phis()) {
      Value *FirstValue = nullptr;
      bool SawSwitchEdge = false;

      for (unsigned I = 0, E = Phi.getNumIncomingValues(); I != E; ++I) {
        if (Phi.getIncomingBlock(I) != SwitchBlock)
          continue;

        Value *Incoming = Phi.getIncomingValue(I);
        if (!SawSwitchEdge) {
          FirstValue = Incoming;
          SawSwitchEdge = true;
          continue;
        }

        if (Incoming != FirstValue)
          return true;
      }
    }

    return false;
  }

private:
  typedef DenseMap<const BasicBlock *, const PHINode *> StateDefMap;

  std::vector<ThreadingPath> getPathsFromStateDefMap(StateDefMap &StateDef,
                                                     PHINode *Phi,
                                                     VisitedBlocks &VB,
                                                     unsigned PathsLimit) {
    std::vector<ThreadingPath> Res;
    auto *PhiBB = Phi->getParent();
    VB.insert(PhiBB);

    VisitedBlocks UniqueBlocks;
    for (auto *IncomingBB : Phi->blocks()) {
      if (Res.size() >= PathsLimit)
        break;
      if (!UniqueBlocks.insert(IncomingBB).second)
        continue;
      if (!SwitchOuterLoop->contains(IncomingBB))
        continue;

      Value *IncomingValue = Phi->getIncomingValueForBlock(IncomingBB);
      if (auto *C = dyn_cast<ConstantInt>(IncomingValue)) {
        if (PhiBB == SwitchBlock &&
            SwitchBlock != cast<PHINode>(Switch->getOperand(0))->getParent())
          continue;
        ThreadingPath NewPath;
        NewPath.setDeterminator(PhiBB);
        NewPath.setExitValue(C);
        if (IncomingBB != SwitchBlock) {
          if (VB.contains(IncomingBB))
            continue;
          NewPath.push_back(IncomingBB);
        }
        NewPath.push_back(PhiBB);
        Res.push_back(NewPath);
        continue;
      }

      if (VB.contains(IncomingBB) || IncomingBB == SwitchBlock)
        continue;

      auto *IncomingPhi = dyn_cast<PHINode>(IncomingValue);
      if (!IncomingPhi)
        continue;
      auto *IncomingPhiDefBB = IncomingPhi->getParent();
      if (!StateDef.contains(IncomingPhiDefBB))
        continue;

      if (IncomingPhiDefBB == IncomingBB) {
        assert(PathsLimit > Res.size());
        std::vector<ThreadingPath> PredPaths = getPathsFromStateDefMap(
            StateDef, IncomingPhi, VB, PathsLimit - Res.size());
        for (ThreadingPath &Path : PredPaths) {
          Path.push_back(PhiBB);
          Res.push_back(std::move(Path));
        }
        continue;
      }

      if (VB.contains(IncomingPhiDefBB))
        continue;

      PathsType IntermediatePaths;
      assert(PathsLimit > Res.size());
      auto InterPathLimit = PathsLimit - Res.size();
      IntermediatePaths = paths(IncomingPhiDefBB, IncomingBB, VB,
                                /* PathDepth = */ 1, InterPathLimit);
      if (IntermediatePaths.empty())
        continue;

      assert(InterPathLimit >= IntermediatePaths.size());
      auto PredPathLimit = InterPathLimit / IntermediatePaths.size();
      std::vector<ThreadingPath> PredPaths =
          getPathsFromStateDefMap(StateDef, IncomingPhi, VB, PredPathLimit);
      for (const ThreadingPath &Path : PredPaths) {
        for (const PathType &IPath : IntermediatePaths) {
          ThreadingPath NewPath(Path);
          NewPath.appendExcludingFirst(IPath);
          NewPath.push_back(PhiBB);
          Res.push_back(NewPath);
        }
      }
    }

    VB.erase(PhiBB);
    return Res;
  }

  PathsType paths(BasicBlock *BB, BasicBlock *ToBB, VisitedBlocks &Visited,
                  unsigned PathDepth, unsigned PathsLimit) {
    PathsType Res;

    if (PathDepth > MaxPathLength) {
      ORE->emit([&]() {
        return OptimizationRemarkAnalysis(DEBUG_TYPE, "MaxPathLengthReached",
                                          Switch)
               << "Exploration stopped after visiting MaxPathLength="
               << ore::NV("MaxPathLength", MaxPathLength) << " blocks.";
      });
      return Res;
    }

    const bool Inserted = Visited.insert(BB).second;

    struct EraseIfInserted {
      VisitedBlocks &Visited;
      BasicBlock *BB;
      bool Inserted;

      ~EraseIfInserted() {
        if (Inserted)
          Visited.erase(BB);
      }
    } Guard{Visited, BB, Inserted};

    if (++NumVisited > MaxNumVisitiedPaths)
      return Res;

    if (!SwitchOuterLoop->contains(BB))
      return Res;

    Loop *CurrLoop = LI->getLoopFor(BB);
    if (!CurrLoop)
      return Res;

    SmallPtrSet<BasicBlock *, 4> UniqueSuccessors;
    for (BasicBlock *Succ : successors(BB)) {
      if (Res.size() >= PathsLimit)
        break;

      if (!UniqueSuccessors.insert(Succ).second)
        continue;

      if (Succ == ToBB) {
        Res.push_back({BB, ToBB});
        continue;
      }

      if (Visited.contains(Succ))
        continue;

      if (Succ == CurrLoop->getHeader())
        continue;

      if (LI->getLoopFor(Succ) != CurrLoop)
        continue;

      assert(PathsLimit > Res.size());
      PathsType SuccPaths =
          paths(Succ, ToBB, Visited, PathDepth + 1, PathsLimit - Res.size());

      for (PathType &Path : SuccPaths) {
        Path.push_front(BB);
        Res.push_back(std::move(Path));
      }
    }

    return Res;
  }

  StateDefMap getStateDefMap() const {
    StateDefMap Res;
    DenseSet<const BasicBlock *> MultipleDefBBs;
    PHINode *FirstDef = dyn_cast<PHINode>(Switch->getOperand(0));
    if (!FirstDef)
      return Res;

    SmallVector<PHINode *, 8> Stack;
    Stack.push_back(FirstDef);
    SmallPtrSet<Value *, 16> SeenValues;
    SeenValues.insert(FirstDef);

    while (!Stack.empty()) {
      PHINode *CurPhi = Stack.pop_back_val();
      BasicBlock *CurDefBlock = CurPhi->getParent();

      auto [It, Inserted] = Res.try_emplace(CurDefBlock, CurPhi);
      if (!Inserted && It->second != CurPhi)
        MultipleDefBBs.insert(CurDefBlock);

      for (BasicBlock *IncomingBB : CurPhi->blocks()) {
        PHINode *IncomingPhi =
            dyn_cast<PHINode>(CurPhi->getIncomingValueForBlock(IncomingBB));
        if (!IncomingPhi)
          continue;
        bool IsOutsideLoops = !SwitchOuterLoop->contains(IncomingBB);
        if (IsOutsideLoops || !SeenValues.insert(IncomingPhi).second)
          continue;

        Stack.push_back(IncomingPhi);
      }
    }

    for (const BasicBlock *BB : MultipleDefBBs) {
      LLVM_DEBUG(dbgs() << "Not a state-defining block: Multiple defs in "
                        << BB->getNameOrAsOperand() << "\n");
      Res.erase(BB);
    }
    return Res;
  }

  void findTPaths() {
    StateDefMap StateDef = getStateDefMap();
    if (StateDef.empty()) {
      ORE->emit([&]() {
        return OptimizationRemarkMissed(DEBUG_TYPE, "SwitchNotPredictable",
                                        Switch)
               << "Switch instruction is not predictable.";
      });
      return;
    }

    auto *SwitchPhi = cast<PHINode>(Switch->getOperand(0));
    auto *SwitchPhiDefBB = SwitchPhi->getParent();
    VisitedBlocks VB;
    std::vector<ThreadingPath> PathsToPhiDef =
        getPathsFromStateDefMap(StateDef, SwitchPhi, VB, MaxNumPaths);
    if (SwitchPhiDefBB == SwitchBlock || PathsToPhiDef.empty()) {
      TPaths = std::move(PathsToPhiDef);
      return;
    }

    assert(MaxNumPaths >= PathsToPhiDef.size() && !PathsToPhiDef.empty());
    auto PathsLimit = MaxNumPaths / PathsToPhiDef.size();
    PathsType PathsToSwitchBB =
        paths(SwitchPhiDefBB, SwitchBlock, VB, /* PathDepth = */ 1, PathsLimit);
    if (PathsToSwitchBB.empty())
      return;

    std::vector<ThreadingPath> TempList;
    for (const ThreadingPath &Path : PathsToPhiDef) {
      SmallPtrSet<BasicBlock *, 32> PathSet(Path.getPath().begin(),
                                            Path.getPath().end());
      for (const PathType &PathToSw : PathsToSwitchBB) {
        if (any_of(llvm::drop_begin(PathToSw),
                   [&](const BasicBlock *BB) { return PathSet.contains(BB); }))
          continue;
        ThreadingPath PathCopy(Path);
        PathCopy.appendExcludingFirst(PathToSw);
        TempList.push_back(PathCopy);
      }
    }
    TPaths = std::move(TempList);
  }

  BasicBlock *getNextCaseSuccessor(const APInt &NextState) {
    if (CaseValToDest.empty()) {
      for (auto Case : Switch->cases()) {
        APInt CaseVal = Case.getCaseValue()->getValue();
        CaseValToDest[CaseVal] = Case.getCaseSuccessor();
      }
    }

    auto SuccIt = CaseValToDest.find(NextState);
    return SuccIt == CaseValToDest.end() ? Switch->getDefaultDest()
                                         : SuccIt->second;
  }

  void unifyTPaths() {
    SmallDenseMap<BasicBlock *, APInt, 16> DestToState;
    SmallDenseMap<BasicBlock *, bool, 16> DestDistinguishes;

    for (ThreadingPath &Path : TPaths) {
      APInt NextState = Path.getExitValue();
      BasicBlock *Dest = getNextCaseSuccessor(NextState);

      auto [StateIt, Inserted] = DestToState.try_emplace(Dest, NextState);
      if (Inserted)
        continue;

      if (NextState == StateIt->second)
        continue;

      auto [DistIt, DistInserted] =
          DestDistinguishes.try_emplace(Dest, false);
      if (DistInserted)
        DistIt->second = destinationDistinguishesSwitchEdges(Dest);

      if (DistIt->second)
        continue;

      LLVM_DEBUG(dbgs() << "Next state in ";
                 Path.print(dbgs());
                 dbgs() << " is equivalent to " << StateIt->second << "\n");

      Path.setExitValue(StateIt->second);
    }
  }

  unsigned NumVisited = 0;
  SwitchInst *Switch;
  BasicBlock *SwitchBlock;
  OptimizationRemarkEmitter *ORE;
  std::vector<ThreadingPath> TPaths;
  DenseMap<APInt, BasicBlock *> CaseValToDest;
  LoopInfo *LI;
  Loop *SwitchOuterLoop;
};

struct TransformDFA {
  TransformDFA(AllSwitchPaths *SwitchPaths, DomTreeUpdater *DTU,
               AssumptionCache *AC, TargetTransformInfo *TTI,
               OptimizationRemarkEmitter *ORE,
               SmallPtrSet<const Value *, 32> EphValues,
               BlockFrequencyInfo *BFI, BranchProbabilityInfo *BPI,
               LoopInfo *LI)
      : SwitchPaths(SwitchPaths), DTU(DTU), AC(AC), TTI(TTI), ORE(ORE),
        EphValues(std::move(EphValues)), BFI(BFI), BPI(BPI), LI(LI) {}

  bool run() {
    NumHotPathsPruned += prunePathsByHotness();

    if (SwitchPaths->getThreadingPaths().empty())
      return false;

    if (isLegalAndProfitableToTransform()) {
      createAllExitPaths();
      NumTransforms++;
      return true;
    }
    return false;
  }

private:
  using SeenStateMap = DenseMap<BasicBlock *, SmallVector<APInt, 4>>;

  static bool stateSeen(const SeenStateMap &Seen, BasicBlock *BB,
                        const APInt &State) {
    SeenStateMap::const_iterator It = Seen.find(BB);
    if (It == Seen.end())
      return false;

    for (const APInt &SeenState : It->second)
      if (SeenState == State)
        return true;

    return false;
  }

  static void markStateSeen(SeenStateMap &Seen, BasicBlock *BB,
                            const APInt &State) {
    Seen[BB].push_back(State);
  }

  struct PathScore {
    unsigned Index = 0;
    uint64_t MinFreq = 0;
    uint64_t SumFreq = 0;
    unsigned SideExits = 0;
    unsigned HeaderCrossings = 0;
    unsigned Length = 0;
  };

  static uint64_t saturatingAdd(uint64_t A, uint64_t B) {
    if (A > std::numeric_limits<uint64_t>::max() - B)
      return std::numeric_limits<uint64_t>::max();
    return A + B;
  }

  PathScore computePathScore(const ThreadingPath &TPath,
                             unsigned Index) const {
    PathScore S;
    S.Index = Index;

    const PathType &PathBBs = TPath.getPath();
    auto DetIt = llvm::find(PathBBs, TPath.getDeterminatorBB());
    if (DetIt == PathBBs.end())
      return S;

    size_t DetIdx = std::distance(PathBBs.begin(), DetIt);
    size_t BeginIdx = (DetIdx == 0) ? 1 : DetIdx;
    if (BeginIdx >= PathBBs.size())
      return S;

    SmallPtrSet<const BasicBlock *, 32> PathSet(PathBBs.begin(),
                                                 PathBBs.end());

    uint64_t MinF = std::numeric_limits<uint64_t>::max();
    uint64_t SumF = 0;
    unsigned SideExitCount = 0;
    unsigned HeaderCount = 0;
    unsigned Len = 0;

    for (size_t I = BeginIdx, E = PathBBs.size(); I < E; ++I) {
      BasicBlock *BB = PathBBs[I];
      ++Len;

      if (BFI) {
        uint64_t F = BFI->getBlockFreq(BB).getFrequency();
        MinF = std::min(MinF, F);
        SumF = saturatingAdd(SumF, F);
      }

      if (TracePathHeuristic) {
        if (LI && LI->isLoopHeader(BB))
          ++HeaderCount;

        for (BasicBlock *Succ : successors(BB))
          if (!PathSet.contains(Succ))
            ++SideExitCount;
      }
    }

    if (MinF == std::numeric_limits<uint64_t>::max())
      MinF = 0;

    S.MinFreq = MinF;
    S.SumFreq = SumF;
    S.SideExits = SideExitCount;
    S.HeaderCrossings = HeaderCount;
    S.Length = Len;
    return S;
  }

  uint64_t estimateIncrementalCloneInsts(const ThreadingPath &TPath,
                                         SeenStateMap &Seen) {
    const APInt &State = TPath.getExitValue();
    uint64_t Cost = 0;

    BasicBlock *SwitchBlock = SwitchPaths->getSwitchBlock();
    if (!stateSeen(Seen, SwitchBlock, State)) {
      Cost += SwitchBlock->size();
      markStateSeen(Seen, SwitchBlock, State);
    }

    const PathType &PathBBs = TPath.getPath();
    auto DetIt = llvm::find(PathBBs, TPath.getDeterminatorBB());
    if (DetIt == PathBBs.end())
      return Cost;

    size_t DetIdx = std::distance(PathBBs.begin(), DetIt);
    size_t BeginIdx = (DetIdx == 0) ? 1 : DetIdx;
    for (size_t I = BeginIdx, E = PathBBs.size(); I < E; ++I) {
      BasicBlock *BB = PathBBs[I];
      if (stateSeen(Seen, BB, State))
        continue;
      Cost += BB->size();
      markStateSeen(Seen, BB, State);
    }

    return Cost;
  }

  unsigned prunePathsByHotness() {
    if (!HotPathHeuristic)
      return 0;

    std::vector<ThreadingPath> &Paths = SwitchPaths->getThreadingPaths();
    const size_t OrigSize = Paths.size();
    if (OrigSize <= 1)
      return 0;

    SmallVector<PathScore, 32> Ranked;
    Ranked.reserve(OrigSize);
    for (unsigned I = 0, E = static_cast<unsigned>(OrigSize); I != E; ++I)
      Ranked.push_back(computePathScore(Paths[I], I));

    llvm::stable_sort(Ranked, [](const PathScore &A, const PathScore &B) {
      if (A.MinFreq != B.MinFreq)
        return A.MinFreq > B.MinFreq;
      if (A.SideExits != B.SideExits)
        return A.SideExits < B.SideExits;
      if (A.HeaderCrossings != B.HeaderCrossings)
        return A.HeaderCrossings < B.HeaderCrossings;
      if (A.SumFreq != B.SumFreq)
        return A.SumFreq > B.SumFreq;
      if (A.Length != B.Length)
        return A.Length < B.Length;
      return A.Index < B.Index;
    });

    const uint64_t Budget = HotPathCloneBudgetInst.getValue();
    const bool Unlimited = (Budget == 0);

    SeenStateMap SeenPairs;
    SmallVector<ThreadingPath, 32> Selected;
    Selected.reserve(OrigSize);
    uint64_t UsedBudget = 0;

    for (const PathScore &S : Ranked) {
      ThreadingPath &Path = Paths[S.Index];

      SeenStateMap SeenTmp = SeenPairs;
      const uint64_t Inc = estimateIncrementalCloneInsts(Path, SeenTmp);

      if (!Unlimited && (Inc > Budget || UsedBudget > Budget - Inc))
        continue;

      (void)estimateIncrementalCloneInsts(Path, SeenPairs);
      UsedBudget += Inc;
      Selected.push_back(std::move(Path));
    }

    if (Selected.empty())
      Selected.push_back(std::move(Paths[Ranked.front().Index]));

    if (Selected.size() == OrigSize)
      return 0;

    const unsigned Removed = static_cast<unsigned>(OrigSize - Selected.size());
    // Keeping paths in Ranked order allows the hottest paths to dictate
    // unifications and clone reuse priorities.
    Paths.assign(std::make_move_iterator(Selected.begin()),
                 std::make_move_iterator(Selected.end()));
    return Removed;
  }

  uint64_t getContainingLoopInstCount() const {
    if (!LI)
      return 0;
    BasicBlock *SwitchBB = SwitchPaths->getSwitchBlock();
    Loop *L = LI->getLoopFor(SwitchBB);
    if (!L)
      return 0;

    uint64_t Count = 0;
    for (BasicBlock *BB : L->blocks())
      Count += BB->size();
    return Count;
  }

  unsigned getEntropyAdjustedThreshold(const SwitchInst *Switch) const {
    if (!EntropyProfitabilityHeuristic || !BPI)
      return CostThreshold;

    const BasicBlock *SwitchBB = Switch->getParent();
    unsigned NSucc = Switch->getNumSuccessors();
    if (NSucc <= 1)
      return CostThreshold;

    double Entropy = 0.0;
    double MaxP = 0.0;

    for (unsigned I = 0; I < NSucc; ++I) {
      BranchProbability P = BPI->getEdgeProbability(SwitchBB, I);
      double Pd = static_cast<double>(P.getNumerator()) /
                  static_cast<double>(P.getDenominator());
      MaxP = std::max(MaxP, Pd);
      if (Pd > 0.0)
        Entropy -= Pd * std::log2(Pd);
    }

    double MaxEntropy = std::log2(static_cast<double>(NSucc));
    double NormEntropy = (MaxEntropy > 0.0) ? (Entropy / MaxEntropy) : 0.0;
    NormEntropy = std::max(0.0, std::min(1.0, NormEntropy));

    int EntropyScale =
        900 + static_cast<int>(std::llround(350.0 * NormEntropy));
    int DominancePenalty = 0;
    if (MaxP > 0.70)
      DominancePenalty = static_cast<int>(
          std::llround(((MaxP - 0.70) / 0.30) * 200.0));

    int ScalePermille =
        std::max(700, std::min(1300, EntropyScale - DominancePenalty));
    uint64_t Adjusted =
        (static_cast<uint64_t>(CostThreshold) *
             static_cast<uint64_t>(ScalePermille) +
         999) /
        1000;

    return static_cast<unsigned>(std::max<uint64_t>(1, Adjusted));
  }

  bool destinationHasDistinctSwitchPhiValues(BasicBlock *Dest) const {
    BasicBlock *SwitchBlock = SwitchPaths->getSwitchBlock();

    for (PHINode &Phi : Dest->phis()) {
      Value *FirstValue = nullptr;
      bool SawSwitchIncoming = false;

      for (unsigned I = 0, E = Phi.getNumIncomingValues(); I != E; ++I) {
        if (Phi.getIncomingBlock(I) != SwitchBlock)
          continue;

        Value *Incoming = Phi.getIncomingValue(I);
        if (!SawSwitchIncoming) {
          FirstValue = Incoming;
          SawSwitchIncoming = true;
          continue;
        }

        if (Incoming != FirstValue)
          return true;
      }
    }

    return false;
  }

  bool isLegalAndProfitableToTransform() {
    CodeMetrics Metrics;
    uint64_t NumClonedInst = 0;
    SwitchInst *Switch = SwitchPaths->getSwitchInst();

    if (Switch->getNumSuccessors() <= 1)
      return false;

    for (BasicBlock *Succ : successors(Switch->getParent())) {
      if (destinationHasDistinctSwitchPhiValues(Succ)) {
        LLVM_DEBUG(dbgs()
                   << "DFA Jump Threading: not threading switch because "
                      "successor PHIs distinguish multiple switch edges.\n");
        ORE->emit([&]() {
          return OptimizationRemarkMissed(DEBUG_TYPE,
                                          "StateDependentDestinationPhi",
                                          Switch)
                 << "Switch successor PHIs distinguish multiple switch edges.";
        });
        return false;
      }
    }

    DuplicateBlockMap DuplicateMap;
    for (const ThreadingPath &TPath : SwitchPaths->getThreadingPaths()) {
      const PathType &PathBBs = TPath.getPath();
      APInt NextState = TPath.getExitValue();
      const BasicBlock *Determinator = TPath.getDeterminatorBB();

      BasicBlock *BB = SwitchPaths->getSwitchBlock();
      BasicBlock *VisitedBB = getClonedBB(BB, NextState, DuplicateMap);
      if (!VisitedBB) {
        Metrics.analyzeBasicBlock(BB, *TTI, EphValues);
        NumClonedInst += BB->size();
        DuplicateMap[BB].push_back({BB, NextState});
      }

      auto DetIt = llvm::find(PathBBs, Determinator);
      if (DetIt != PathBBs.end()) {
        size_t DetIdx = std::distance(PathBBs.begin(), DetIt);
        size_t BeginIdx = (DetIdx == 0) ? 1 : DetIdx;
        for (size_t I = BeginIdx, E = PathBBs.size(); I < E; ++I) {
          BB = PathBBs[I];
          VisitedBB = getClonedBB(BB, NextState, DuplicateMap);
          if (VisitedBB)
            continue;
          Metrics.analyzeBasicBlock(BB, *TTI, EphValues);
          NumClonedInst += BB->size();
          DuplicateMap[BB].push_back({BB, NextState});
        }
      }

      if (Metrics.notDuplicatable) {
        LLVM_DEBUG(dbgs() << "DFA Jump Threading: Not jump threading, contains "
                          << "non-duplicatable instructions.\n");
        ORE->emit([&]() {
          return OptimizationRemarkMissed(DEBUG_TYPE, "NonDuplicatableInst",
                                          Switch)
                 << "Contains non-duplicatable instructions.";
        });
        return false;
      }

      if (Metrics.Convergence != ConvergenceKind::None) {
        LLVM_DEBUG(dbgs() << "DFA Jump Threading: Not jump threading, contains "
                          << "convergent instructions.\n");
        ORE->emit([&]() {
          return OptimizationRemarkMissed(DEBUG_TYPE, "ConvergentInst", Switch)
                 << "Contains convergent instructions.";
        });
        return false;
      }

      if (!Metrics.NumInsts.isValid()) {
        LLVM_DEBUG(dbgs() << "DFA Jump Threading: Not jump threading, contains "
                          << "instructions with invalid cost.\n");
        ORE->emit([&]() {
          return OptimizationRemarkMissed(DEBUG_TYPE, "InvalidCost", Switch)
                 << "Contains instructions with invalid cost.";
        });
        return false;
      }
    }

    uint64_t NumOrigInst = 0;
    uint64_t NumOuterUseBlock = 0;
    for (BasicBlock *BB : DuplicateMap.keys()) {
      NumOrigInst += BB->size();
      for (BasicBlock *Succ : successors(BB))
        if (!DuplicateMap.count(Succ) && Succ->getSinglePredecessor())
          ++NumOuterUseBlock;
    }

    if (NumOrigInst == 0)
      return false;

    if (RegionCloneBudgetInst != 0 && NumClonedInst > RegionCloneBudgetInst) {
      ORE->emit([&]() {
        return OptimizationRemarkMissed(DEBUG_TYPE, "NotProfitable", Switch)
               << "Cloned instructions exceed region budget.";
      });
      return false;
    }

    uint64_t LoopInstCount = getContainingLoopInstCount();
    if (LoopInstCount > 0) {
      double LoopCloneRate = static_cast<double>(NumClonedInst) /
                             static_cast<double>(LoopInstCount);
      if (LoopCloneRate > MaxLoopCloneRate) {
        ORE->emit([&]() {
          return OptimizationRemarkMissed(DEBUG_TYPE, "NotProfitable", Switch)
                 << "Cloned instructions exceed max loop clone rate.";
        });
        return false;
      }
    }

    if (double(NumClonedInst) / double(NumOrigInst) > MaxClonedRate) {
      LLVM_DEBUG(dbgs() << "DFA Jump Threading: Not jump threading, too much "
                           "instructions wll be cloned\n");
      ORE->emit([&]() {
        return OptimizationRemarkMissed(DEBUG_TYPE, "NotProfitable", Switch)
               << "Too much instructions will be cloned.";
      });
      return false;
    }

    if (NumOuterUseBlock > MaxOuterUseBlocks) {
      LLVM_DEBUG(dbgs() << "DFA Jump Threading: Not jump threading, too much "
                           "blocks with outer uses\n");
      ORE->emit([&]() {
        return OptimizationRemarkMissed(DEBUG_TYPE, "NotProfitable", Switch)
               << "Too much blocks with outer uses.";
      });
      return false;
    }

    InstructionCost DuplicationCost = 0;

    unsigned JumpTableSize = 0;
    TTI->getEstimatedNumberOfCaseClusters(*Switch, JumpTableSize, nullptr,
                                          nullptr);
    if (JumpTableSize == 0) {
      unsigned CondBranches =
          APInt(32, Switch->getNumSuccessors()).ceilLogBase2();
      assert(CondBranches > 0 &&
             "The threaded switch must have multiple branches");
      DuplicationCost = Metrics.NumInsts / CondBranches;
    } else {
      DuplicationCost = Metrics.NumInsts / JumpTableSize;
    }

    const unsigned EffectiveThreshold = getEntropyAdjustedThreshold(Switch);

    LLVM_DEBUG(dbgs() << "\nDFA Jump Threading: Cost to jump thread block "
                      << SwitchPaths->getSwitchBlock()->getName()
                      << " is: " << DuplicationCost
                      << ", threshold: " << EffectiveThreshold << "\n\n");

    if (DuplicationCost > EffectiveThreshold) {
      LLVM_DEBUG(dbgs() << "Not jump threading, duplication cost exceeds the "
                        << "cost threshold.\n");
      ORE->emit([&]() {
        return OptimizationRemarkMissed(DEBUG_TYPE, "NotProfitable", Switch)
               << "Duplication cost exceeds the effective threshold (cost="
               << ore::NV("Cost", DuplicationCost)
               << ", threshold=" << ore::NV("Threshold", EffectiveThreshold)
               << ").";
      });
      return false;
    }

    ORE->emit([&]() {
      return OptimizationRemark(DEBUG_TYPE, "JumpThreaded", Switch)
             << "Switch statement jump-threaded.";
    });

    return true;
  }

  void createAllExitPaths() {
    DefMap NewDefs;
    DuplicateBlockMap DuplicateMap;

    BasicBlock *SwitchBlock = SwitchPaths->getSwitchBlock();

    SmallPtrSet<BasicBlock *, 32> BlocksToClean;
    BlocksToClean.insert_range(successors(SwitchBlock));
    BlocksToClean.insert(SwitchBlock);

    for (const ThreadingPath &TPath : SwitchPaths->getThreadingPaths()) {
      createExitPath(NewDefs, TPath, DuplicateMap, BlocksToClean, DTU);
      ++NumPaths;
    }

    for (const ThreadingPath &TPath : SwitchPaths->getThreadingPaths())
      updateLastSuccessor(TPath, DuplicateMap, DTU);

    for (BasicBlock *BB : BlocksToClean)
      cleanPhiNodes(BB);

    updateSSA(NewDefs);
  }

  static bool hasCFGEdge(BasicBlock *From, BasicBlock *To) {
    Instruction *TI = From->getTerminator();

    for (unsigned I = 0, E = TI->getNumSuccessors(); I != E; ++I)
      if (TI->getSuccessor(I) == To)
        return true;

    return false;
  }

  void createExitPath(DefMap &NewDefs, const ThreadingPath &Path,
                      DuplicateBlockMap &DuplicateMap,
                      SmallPtrSetImpl<BasicBlock *> &BlocksToClean,
                      DomTreeUpdater *DTU) {
    const PathType &PathBBs = Path.getPath();
    const BasicBlock *Determinator = Path.getDeterminatorBB();
    auto DetIt = llvm::find(PathBBs, Determinator);
    if (DetIt == PathBBs.end())
      return;

    size_t DetIdx = std::distance(PathBBs.begin(), DetIt);
    size_t BeginIdx = (DetIdx == 0) ? 1 : DetIdx;
    if (BeginIdx >= PathBBs.size())
      return;

    BasicBlock *PrevBB = PathBBs[BeginIdx - 1];

    for (size_t I = BeginIdx, E = PathBBs.size(); I < E; ++I) {
      BasicBlock *BB = PathBBs[I];
      BlocksToClean.insert(BB);

      if (BasicBlock *NextBB =
              getClonedBB(BB, Path.getExitValue(), DuplicateMap)) {
        if (hasCFGEdge(PrevBB, BB))
          updatePredecessor(PrevBB, BB, NextBB, DTU);
        PrevBB = NextBB;
        continue;
      }

      if (!hasCFGEdge(PrevBB, BB))
        return;

      BasicBlock *NewBB = cloneBlockAndUpdatePredecessor(
          BB, PrevBB, Path.getExitValue(), DuplicateMap, NewDefs, DTU);
      DuplicateMap[BB].push_back({NewBB, Path.getExitValue()});
      BlocksToClean.insert(NewBB);
      PrevBB = NewBB;
    }
  }

  void updateSSA(DefMap &NewDefs) {
    SSAUpdaterBulk SSAUpdate;
    SmallVector<Use *, 16> UsesToRename;

    for (const auto &KV : NewDefs) {
      Instruction *I = KV.first;
      BasicBlock *BB = I->getParent();
      const std::vector<Instruction *> &Cloned = KV.second;

      for (Use &U : I->uses()) {
        Instruction *User = cast<Instruction>(U.getUser());
        if (PHINode *UserPN = dyn_cast<PHINode>(User)) {
          if (UserPN->getIncomingBlock(U) == BB)
            continue;
        } else if (User->getParent() == BB) {
          continue;
        }

        UsesToRename.push_back(&U);
      }

      if (UsesToRename.empty())
        continue;
      LLVM_DEBUG(dbgs() << "DFA-JT: Renaming non-local uses of: " << *I
                        << "\n");

      unsigned VarNum = SSAUpdate.AddVariable(I->getName(), I->getType());
      SSAUpdate.AddAvailableValue(VarNum, BB, I);
      for (Instruction *New : Cloned)
        SSAUpdate.AddAvailableValue(VarNum, New->getParent(), New);

      while (!UsesToRename.empty())
        SSAUpdate.AddUse(VarNum, UsesToRename.pop_back_val());

      LLVM_DEBUG(dbgs() << "\n");
    }
    SSAUpdate.RewriteAllUses(&DTU->getDomTree());
  }

  BasicBlock *getNextCaseSuccessor(const APInt &NextState) {
    if (CaseValToDestCache.empty()) {
      SwitchInst *Switch = SwitchPaths->getSwitchInst();
      for (auto Case : Switch->cases())
        CaseValToDestCache[Case.getCaseValue()->getValue()] =
            Case.getCaseSuccessor();
      DefaultDestCache = Switch->getDefaultDest();
    }

    auto It = CaseValToDestCache.find(NextState);
    return It == CaseValToDestCache.end() ? DefaultDestCache : It->second;
  }

  BasicBlock *cloneBlockAndUpdatePredecessor(BasicBlock *BB, BasicBlock *PrevBB,
                                             const APInt &NextState,
                                             DuplicateBlockMap &DuplicateMap,
                                             DefMap &NewDefs,
                                             DomTreeUpdater *DTU) {
    ValueToValueMapTy VMap;
    BasicBlock *NewBB = CloneBasicBlock(
        BB, VMap, ".jt" + std::to_string(NextState.getLimitedValue()),
        BB->getParent());
    NewBB->moveAfter(BB);
    ++NumCloned;

    for (Instruction &I : *NewBB) {
      if (isa<PHINode>(&I))
        continue;
      RemapInstruction(&I, VMap,
                       RF_IgnoreMissingLocals | RF_NoModuleLevelChanges);
      if (auto *II = dyn_cast<AssumeInst>(&I))
        AC->registerAssumption(II);
    }

    updateSuccessorPhis(BB, NewBB, NextState, VMap, DuplicateMap);
    updatePredecessor(PrevBB, BB, NewBB, DTU);
    cleanPhiNodes(NewBB);
    updateDefMap(NewDefs, VMap);

    SmallPtrSet<BasicBlock *, 4> SuccSet;
    for (BasicBlock *SuccBB : successors(NewBB))
      if (SuccSet.insert(SuccBB).second)
        DTU->applyUpdates({{DominatorTree::Insert, NewBB, SuccBB}});

    return NewBB;
  }

  void updateSuccessorPhis(BasicBlock *BB, BasicBlock *ClonedBB,
                           const APInt &NextState,
                           ValueToValueMapTy &VMap,
                           DuplicateBlockMap &DuplicateMap) {
    (void)NextState;
    (void)DuplicateMap;

    auto MapValue = [&](Value *V) -> Value * {
      if (isa<Constant>(V))
        return V;

      auto It = VMap.find(V);
      if (It != VMap.end() && It->second)
        return cast<Value>(It->second);

      return V;
    };

    SmallPtrSet<BasicBlock *, 8> SeenSuccs;
    Instruction *TI = ClonedBB->getTerminator();

    for (BasicBlock *Succ : successors(ClonedBB)) {
      if (!SeenSuccs.insert(Succ).second)
        continue;

      unsigned EdgeMultiplicity = 0;
      for (unsigned I = 0, E = TI->getNumSuccessors(); I != E; ++I)
        if (TI->getSuccessor(I) == Succ)
          ++EdgeMultiplicity;

      for (PHINode &Phi : Succ->phis()) {
        SmallVector<Value *, 4> IncomingVals;

        for (unsigned I = 0, E = Phi.getNumIncomingValues(); I != E; ++I)
          if (Phi.getIncomingBlock(I) == BB)
            IncomingVals.push_back(Phi.getIncomingValue(I));

        assert(IncomingVals.size() >= EdgeMultiplicity &&
               "PHI edge multiplicity mismatch while cloning");

        if (!IncomingVals.empty()) {
          for (unsigned I = 0; I != EdgeMultiplicity; ++I)
            Phi.addIncoming(MapValue(IncomingVals[I % IncomingVals.size()]),
                            ClonedBB);
        }
      }
    }
  }

  void updatePredecessor(BasicBlock *PrevBB,
                         BasicBlock *OldBB,
                         BasicBlock *NewBB,
                         DomTreeUpdater *DTU) {
    Instruction *PrevTerm = PrevBB->getTerminator();

    SmallVector<unsigned, 4> EdgeIdxs;

    for (unsigned I = 0, E = PrevTerm->getNumSuccessors(); I != E; ++I)
      if (PrevTerm->getSuccessor(I) == OldBB)
        EdgeIdxs.push_back(I);

    if (EdgeIdxs.empty())
      return;

    const unsigned RedirectedEdges = EdgeIdxs.size();

    auto OldRange = OldBB->phis();
    auto NewRange = NewBB->phis();

    auto OldIt = OldRange.begin();
    auto NewIt = NewRange.begin();

    for (; OldIt != OldRange.end() && NewIt != NewRange.end();
         ++OldIt, ++NewIt) {
      PHINode &OldPhi = *OldIt;
      PHINode &NewPhi = *NewIt;

      SmallVector<Value *, 4> IncomingVals;

      for (unsigned I = 0, E = OldPhi.getNumIncomingValues(); I != E; ++I)
        if (OldPhi.getIncomingBlock(I) == PrevBB)
          IncomingVals.push_back(OldPhi.getIncomingValue(I));

      unsigned Existing = 0;

      for (unsigned I = 0, E = NewPhi.getNumIncomingValues(); I != E; ++I)
        if (NewPhi.getIncomingBlock(I) == PrevBB)
          ++Existing;

      if (!IncomingVals.empty()) {
        for (unsigned I = Existing; I < RedirectedEdges; ++I) {
          Value *V = IncomingVals[I % IncomingVals.size()];
          NewPhi.addIncoming(V, PrevBB);
        }
      }
    }

#ifndef NDEBUG
    assert(OldIt == OldRange.end() && NewIt == NewRange.end() &&
           "clone must preserve PHI structure");
#endif

    for (unsigned Idx : EdgeIdxs)
      PrevTerm->setSuccessor(Idx, NewBB);

    for (PHINode &Phi : OldBB->phis()) {
      unsigned Removed = 0;

      for (int I = static_cast<int>(Phi.getNumIncomingValues()) - 1;
           I >= 0 && Removed < RedirectedEdges; --I) {
        if (Phi.getIncomingBlock(I) != PrevBB)
          continue;

        Phi.removeIncomingValue(I, /*DeletePHIIfEmpty=*/false);
        ++Removed;
      }
    }

    DTU->applyUpdates({{DominatorTree::Delete, PrevBB, OldBB},
                       {DominatorTree::Insert, PrevBB, NewBB}});
  }

  void updateDefMap(DefMap &NewDefs, ValueToValueMapTy &VMap) {
    SmallVector<std::pair<Instruction *, Instruction *>> NewDefsVector;
    NewDefsVector.reserve(VMap.size());

    for (const auto &Entry : VMap) {
      Instruction *Inst =
          dyn_cast<Instruction>(const_cast<Value *>(Entry.first));
      if (!Inst || !Entry.second ||
          isa<UncondBrInst, CondBrInst, SwitchInst>(Inst))
        continue;

      Instruction *Cloned = dyn_cast<Instruction>(Entry.second);
      if (!Cloned)
        continue;

      NewDefsVector.push_back({Inst, Cloned});
    }

    sort(NewDefsVector, [](const auto &LHS, const auto &RHS) {
      if (LHS.first == RHS.first)
        return LHS.second->comesBefore(RHS.second);
      return LHS.first->comesBefore(RHS.first);
    });

    for (const auto &KV : NewDefsVector)
      NewDefs[KV.first].push_back(KV.second);
  }

  void updateLastSuccessor(const ThreadingPath &TPath,
                           DuplicateBlockMap &DuplicateMap,
                           DomTreeUpdater *DTU) {
    if (TPath.getPath().empty())
      return;

    APInt NextState = TPath.getExitValue();
    BasicBlock *BB = TPath.getPath().back();
    BasicBlock *LastBlock = getClonedBB(BB, NextState, DuplicateMap);

    if (!LastBlock)
      return;

    auto *Switch = dyn_cast<SwitchInst>(LastBlock->getTerminator());
    if (!Switch)
      return;

    BasicBlock *NextCase = getNextCaseSuccessor(NextState);

    std::vector<DominatorTree::UpdateType> DTUpdates;
    SmallPtrSet<BasicBlock *, 8> SuccSet;

    for (BasicBlock *Succ : successors(LastBlock))
      if (Succ != NextCase && SuccSet.insert(Succ).second)
        DTUpdates.push_back({DominatorTree::Delete, LastBlock, Succ});

    Switch->eraseFromParent();
    BranchInst::Create(NextCase, LastBlock);

    DTU->applyUpdates(DTUpdates);
  }

  void cleanPhiNodes(BasicBlock *BB) {
    if (pred_empty(BB)) {
      for (PHINode &PN : make_early_inc_range(BB->phis())) {
        PN.replaceAllUsesWith(PoisonValue::get(PN.getType()));
        PN.eraseFromParent();
      }
      return;
    }

    SmallDenseMap<BasicBlock *, unsigned, 8> PredCounts;

    for (BasicBlock *Pred : predecessors(BB))
      ++PredCounts[Pred];

    for (PHINode &Phi : BB->phis()) {
      SmallDenseMap<BasicBlock *, unsigned, 8> Seen;

      for (unsigned I = 0; I < Phi.getNumIncomingValues();) {
        BasicBlock *IncomingBB = Phi.getIncomingBlock(I);

        unsigned Allowed = PredCounts.lookup(IncomingBB);
        if (Allowed == 0) {
          Phi.removeIncomingValue(I, false);
          continue;
        }

        if (++Seen[IncomingBB] > Allowed) {
          Phi.removeIncomingValue(I, false);
          continue;
        }

        ++I;
      }

#ifndef NDEBUG
      SmallDenseMap<BasicBlock *, unsigned, 8> FinalCounts;

      for (unsigned I = 0, E = Phi.getNumIncomingValues(); I != E; ++I) {
        BasicBlock *IncomingBB = Phi.getIncomingBlock(I);

        assert(PredCounts.count(IncomingBB) &&
               "PHI contains non-predecessor");

        ++FinalCounts[IncomingBB];
      }

      for (const auto &KV : FinalCounts)
        assert(KV.second <= PredCounts.lookup(KV.first) &&
               "PHI has too many incoming edges");
#endif
    }
  }

  BasicBlock *getClonedBB(BasicBlock *BB, const APInt &NextState,
                          DuplicateBlockMap &DuplicateMap) {
    auto MIt = DuplicateMap.find(BB);
    if (MIt == DuplicateMap.end())
      return nullptr;

    const CloneList &ClonedBBs = MIt->second;
    auto It = llvm::find_if(ClonedBBs, [&](const ClonedBlock &C) {
      return C.State == NextState;
    });
    return It != ClonedBBs.end() ? It->BB : nullptr;
  }

  AllSwitchPaths *SwitchPaths;
  DomTreeUpdater *DTU;
  AssumptionCache *AC;
  TargetTransformInfo *TTI;
  OptimizationRemarkEmitter *ORE;
  SmallPtrSet<const Value *, 32> EphValues;
  DenseMap<APInt, BasicBlock *> CaseValToDestCache;
  BasicBlock *DefaultDestCache = nullptr;
  BlockFrequencyInfo *BFI;
  BranchProbabilityInfo *BPI;
  LoopInfo *LI;
};
} // namespace

bool DFAJumpThreading::run(Function &F) {
  LLVM_DEBUG(dbgs() << "\nDFA Jump threading: " << F.getName() << "\n");

  if (F.hasOptSize()) {
    LLVM_DEBUG(dbgs() << "Skipping due to the 'minsize' attribute\n");
    return false;
  }

  if (ClViewCfgBefore)
    F.viewCFG();

  SmallVector<AllSwitchPaths, 2> ThreadableLoops;
  bool MadeChanges = false;
  LoopInfoBroken = false;

  for (BasicBlock &BB : F) {
    auto *SI = dyn_cast<SwitchInst>(BB.getTerminator());
    if (!SI)
      continue;

    LLVM_DEBUG(dbgs() << "\nCheck if SwitchInst in BB " << BB.getName()
                      << " is a candidate\n");
    MainSwitch Switch(SI, LI, ORE);

    if (!Switch.getInstr()) {
      LLVM_DEBUG(dbgs() << "\nSwitchInst in BB " << BB.getName() << " is not a "
                        << "candidate for jump threading\n");
      continue;
    }

    LLVM_DEBUG(dbgs() << "\nSwitchInst in BB " << BB.getName() << " is a "
                      << "candidate for jump threading\n");
    LLVM_DEBUG(SI->dump());

    unfoldSelectInstrs(Switch.getSelectInsts());
    if (!Switch.getSelectInsts().empty())
      MadeChanges = true;

    AllSwitchPaths SwitchPaths(&Switch, ORE, LI,
                               LI->getLoopFor(&BB)->getOutermostLoop());
    SwitchPaths.run();

    if (SwitchPaths.getNumThreadingPaths() > 0) {
      ThreadableLoops.push_back(std::move(SwitchPaths));
      break;
    }
  }

#ifdef NDEBUG
  LI->verify(DTU->getDomTree());
#endif

  SmallPtrSet<const Value *, 32> EphValues;
  if (!ThreadableLoops.empty())
    CodeMetrics::collectEphemeralValues(&F, AC, EphValues);

  for (AllSwitchPaths &SwitchPaths : ThreadableLoops) {
    TransformDFA Transform(&SwitchPaths, DTU, AC, TTI, ORE, EphValues,
                           BFI, BPI, LI);
    if (Transform.run())
      MadeChanges = LoopInfoBroken = true;
  }

  DTU->flush();

#ifdef EXPENSIVE_CHECKS
  verifyFunction(F, &dbgs());
#endif

  if (MadeChanges && VerifyDomInfo)
    assert(DTU->getDomTree().verify(DominatorTree::VerificationLevel::Full) &&
           "Failed to maintain validity of domtree!");

  return MadeChanges;
}

PreservedAnalyses DFAJumpThreadingPass::run(Function &F,
                                            FunctionAnalysisManager &AM) {
  AssumptionCache &AC = AM.getResult<AssumptionAnalysis>(F);
  DominatorTree &DT = AM.getResult<DominatorTreeAnalysis>(F);
  LoopInfo &LI = AM.getResult<LoopAnalysis>(F);
  TargetTransformInfo &TTI = AM.getResult<TargetIRAnalysis>(F);
  BlockFrequencyInfo &BFI = AM.getResult<BlockFrequencyAnalysis>(F);
  BranchProbabilityInfo &BPI = AM.getResult<BranchProbabilityAnalysis>(F);
  OptimizationRemarkEmitter ORE(&F);

  DomTreeUpdater DTU(DT, DomTreeUpdater::UpdateStrategy::Lazy);
  DFAJumpThreading ThreadImpl(&AC, &DTU, &LI, &TTI, &ORE, &BFI, &BPI);
  if (!ThreadImpl.run(F))
    return PreservedAnalyses::all();

  PreservedAnalyses PA;
  PA.preserve<DominatorTreeAnalysis>();
  if (!ThreadImpl.LoopInfoBroken)
    PA.preserve<LoopAnalysis>();
  return PA;
}
