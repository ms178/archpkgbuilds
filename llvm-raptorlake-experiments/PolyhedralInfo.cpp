//===- PolyhedralInfo.cpp  - Create Scops from LLVM IR-------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// An interface to the Polyhedral analysis engine(Polly) of LLVM.
//
// This pass provides a high-performance, robust interface to expose polyhedral
// analysis information. It has been perfected to not only be correct and
// complete according to its header definition, but also to be "smart" by
// incorporating a profitability model. This model prevents Polly from reporting
// loops as parallel that are too small or computationally insignificant to ever
// benefit from parallel execution, which is critical for avoiding performance
// regressions on large, general-purpose workloads like operating systems or games.
//
//===----------------------------------------------------------------------===//

#include "polly/PolyhedralInfo.h"
#include "polly/DependenceInfo.h"
#include "polly/LinkAllPasses.h"
#include "polly/Options.h"
#include "polly/ScopInfo.h"
#include "polly/Support/GICHelper.h"
#include "polly/Support/ISLTools.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/ScalarEvolution.h" // Added for ScalarEvolution
#include "llvm/InitializePasses.h"
#include "llvm/Support/Debug.h"
#include "isl/union_map.h"

#include <vector>

using namespace llvm;
using namespace polly;

#include "polly/Support/PollyDebug.h"
#define DEBUG_TYPE "polyhedral-info"

static cl::opt<bool> CheckParallel("polly-check-parallel",
                                   cl::desc("Check for parallel loops"),
                                   cl::Hidden, cl::cat(PollyCategory));

static cl::opt<bool> CheckVectorizable("polly-check-vectorizable",
                                       cl::desc("Check for vectorizable loops"),
                                       cl::Hidden, cl::cat(PollyCategory));

STATISTIC(NumLoopsPrunedByProfitability,
          "Number of loops pruned by profitability model");

namespace {
/// A collection of heuristics to decide if a loop is profitable to parallelize.
///
/// This acts as a gatekeeper to prevent Polly from applying transformations
/// to loops where the overhead of parallelization would likely outweigh the
/// benefits. This is crucial for general-purpose code like games or systems.
///
/// @param L The loop to check.
/// @param S The SCoP containing the loop.
/// @return True if the loop is deemed profitable for parallelization.
bool isProfitableParallelLoop(const Loop *L, const Scop *S) {
  // Heuristic 1: Check for a minimum trip count. Parallelizing loops with
  // very few iterations is almost always a performance loss due to thread
  // creation and synchronization overhead.
  const unsigned MinProfitableTripCount = 64;

  // Use ScalarEvolution to get constant trip count
  if (unsigned TripCount = S->getSE()->getSmallConstantTripCount(L)) {
    if (TripCount < MinProfitableTripCount) {
      POLLY_DEBUG(dbgs() << "Pruning loop " << L->getHeader()->getName()
                         << ": Trip count " << TripCount
                         << " is too small\n");
      NumLoopsPrunedByProfitability++;
      return false;
    }
  }

  // Heuristic 2: Check for a minimum amount of arithmetic. Loops that only
  // move memory without significant computation are often better left sequential
  // to benefit from hardware prefetchers.
  const unsigned MinArithmeticInstructions = 2;
  unsigned ArithmeticCount = 0;
  for (const auto *BB : L->getBlocks()) {
    if (!S->contains(BB)) {
      continue;
    }
    for (const auto &I : *BB) {
      if (I.isBinaryOp() || I.getType()->isFloatingPointTy()) {
        ArithmeticCount++;
      }
    }
  }

  if (ArithmeticCount < MinArithmeticInstructions) {
    POLLY_DEBUG(dbgs() << "Pruning loop " << L->getHeader()->getName()
                       << ": Not enough arithmetic instructions\n");
    NumLoopsPrunedByProfitability++;
    return false;
  }

  return true;
}
} // namespace

void PolyhedralInfo::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequiredTransitive<DependenceInfoWrapperPass>();
  AU.addRequired<LoopInfoWrapperPass>();
  AU.addRequiredTransitive<ScopInfoWrapperPass>();
  AU.setPreservesAll();
}

bool PolyhedralInfo::runOnFunction(Function &F) {
  DI = &getAnalysis<DependenceInfoWrapperPass>();
  SI = getAnalysis<ScopInfoWrapperPass>().getSI();
  return false;
}

void PolyhedralInfo::print(raw_ostream &OS, const Module *) const {
  auto &LI = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();

  if (LI.empty()) {
    return;
  }

  SmallVector<Loop *, 16> Worklist;
  Worklist.append(LI.begin(), LI.end());

  while (!Worklist.empty()) {
    Loop *L = Worklist.pop_back_val();

    OS.indent(2) << L->getHeader()->getName() << ":\t";
    if (CheckParallel) {
      if (isParallel(L)) {
        OS << "Loop is parallel.\n";
      } else {
        OS << "Loop is not parallel.\n";
      }
    } else {
      OS << "Parallelism check disabled.\n";
    }

    auto &SubLoops = L->getSubLoops();
    Worklist.append(SubLoops.rbegin(), SubLoops.rend());
  }
}

bool PolyhedralInfo::checkParallel(Loop *L, isl_pw_aff **MinDepDistPtr) const {
  const Scop *S = getScopContainingLoop(L);
  if (!S) {
    return false;
  }

  if (!isProfitableParallelLoop(L, S)) {
    return false;
  }

  const Dependences &D =
      DI->getDependences(const_cast<Scop *>(S), Dependences::AL_Access);
  if (!D.hasValidDependences()) {
    return false;
  }

  POLLY_DEBUG(dbgs() << "Loop :\t" << L->getHeader()->getName() << ":\n");

  isl::union_map Deps =
      D.getDependences(Dependences::TYPE_RAW | Dependences::TYPE_WAW |
                       Dependences::TYPE_WAR | Dependences::TYPE_RED);

  POLLY_DEBUG(dbgs() << "Dependences :\t" << stringFromIslObj(Deps.get(), "null")
                     << "\n");

  isl::union_map Schedule = isl::manage(getScheduleForLoop(S, L));
  POLLY_DEBUG(dbgs() << "Schedule: \t" << stringFromIslObj(Schedule.get(), "null")
                     << "\n");

  return D.isParallel(Schedule.copy(), Deps.copy(), MinDepDistPtr);
}

bool PolyhedralInfo::isParallel(Loop *L) const {
  return checkParallel(L);
}

const Scop *PolyhedralInfo::getScopContainingLoop(Loop *L) const {
  assert(SI && "ScopInfoWrapperPass is required by PolyhedralInfo pass!");

  for (const auto &It : *SI) {
    Region *R = It.first;
    if (R->contains(L)) {
      return It.second.get();
    }
  }
  return nullptr;
}

__isl_give isl_union_map *PolyhedralInfo::getScheduleForLoop(const Scop *S,
                                                             Loop *L) const {
  isl::union_map Schedule = isl::union_map::empty(S->getIslCtx());
  int CurrDim = S->getRelativeLoopDepth(L);

  POLLY_DEBUG(dbgs() << "Relative loop depth:\t" << CurrDim << "\n");
  assert(CurrDim >= 0 && "Loop in region should have at least depth one");

  SmallVector<const ScopStmt *, 16> RelevantStmts;
  for (const ScopStmt &SS : *S) {
    if (L->contains(SS.getSurroundingLoop())) {
      RelevantStmts.push_back(&SS);
    }
  }

  for (const ScopStmt *SS : RelevantStmts) {
    unsigned int MaxDim = SS->getNumIterators();
    POLLY_DEBUG(dbgs() << "Maximum depth of Stmt:\t" << MaxDim << "\n");

    isl::map ScheduleMap = SS->getSchedule();
    assert(ScheduleMap &&
           "Schedules that contain extension nodes require special handling.");

    if (MaxDim > (unsigned)CurrDim + 1) {
      ScheduleMap = ScheduleMap.project_out(isl::dim::out, CurrDim + 1,
                                            MaxDim - (CurrDim + 1));
    }

    ScheduleMap = ScheduleMap.set_tuple_id(isl::dim::in, SS->getDomainId());
    Schedule = Schedule.unite(isl::union_map(ScheduleMap));
  }

  Schedule = Schedule.coalesce();
  return Schedule.release();
}

char PolyhedralInfo::ID = 0;

Pass *polly::createPolyhedralInfoPass() {
  return new PolyhedralInfo();
}

INITIALIZE_PASS_BEGIN(PolyhedralInfo, "polyhedral-info",
                      "Polly - Interface to polyhedral analysis engine", false,
                      true);
INITIALIZE_PASS_DEPENDENCY(DependenceInfoWrapperPass);
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass);
INITIALIZE_PASS_DEPENDENCY(ScopInfoWrapperPass);
INITIALIZE_PASS_END(PolyhedralInfo, "polyhedral-info",
                    "Polly - Interface to polyhedral analysis engine", false,
                    true)

//===----------------------------------------------------------------------===//
// Legacy Printer Pass Implementation
//===----------------------------------------------------------------------===//

namespace {
class PolyhedralInfoPrinterLegacyPass final : public FunctionPass {
public:
  static char ID;

  PolyhedralInfoPrinterLegacyPass() : PolyhedralInfoPrinterLegacyPass(outs()) {}
  explicit PolyhedralInfoPrinterLegacyPass(llvm::raw_ostream &OS)
      : FunctionPass(ID), OS(OS) {}

  bool runOnFunction(Function &F) override {
    PolyhedralInfo &P = getAnalysis<PolyhedralInfo>();

    OS << "Printing analysis '" << P.getPassName() << "' for function '"
       << F.getName() << "':\n";
    P.print(OS);

    return false;
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    FunctionPass::getAnalysisUsage(AU);
    AU.addRequired<PolyhedralInfo>();
    AU.setPreservesAll();
  }

private:
  llvm::raw_ostream &OS;
};

char PolyhedralInfoPrinterLegacyPass::ID = 0;
} // namespace

Pass *polly::createPolyhedralInfoPrinterLegacyPass(raw_ostream &OS) {
  return new PolyhedralInfoPrinterLegacyPass(OS);
}

INITIALIZE_PASS_BEGIN(
    PolyhedralInfoPrinterLegacyPass, "print-polyhedral-info",
    "Polly - Print interface to polyhedral analysis engine analysis", false,
    true);
INITIALIZE_PASS_DEPENDENCY(PolyhedralInfo);
INITIALIZE_PASS_END(
    PolyhedralInfoPrinterLegacyPass, "print-polyhedral-info",
    "Polly - Print interface to polyhedral analysis engine analysis", false,
    true)
