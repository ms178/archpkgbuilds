//===------ DeLICM.cpp -----------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// Undo the effect of Loop Invariant Code Motion (LICM) and GVN PRE on SCoP-level.
//
// Remove register/scalar dependencies by mapping them back to array elements.
//
//===----------------------------------------------------------------------===//

#include "polly/DeLICM.h"
#include "polly/LinkAllPasses.h"
#include "polly/Options.h"
#include "polly/ScopInfo.h"
#include "polly/ScopPass.h"
#include "polly/Support/GICHelper.h"
#include "polly/Support/ISLOStream.h"
#include "polly/Support/ISLTools.h"
#include "polly/ZoneAlgo.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/OptimizationRemarkEmitter.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/Module.h"
#include "llvm/InitializePasses.h"

#include "polly/Support/PollyDebug.h"
#define DEBUG_TYPE "polly-delicm"

using namespace polly;
using namespace llvm;

namespace {

cl::opt<int> DelicmMaxOps(
    "polly-delicm-max-ops",
    cl::desc("Maximum number of isl operations to invest for lifetime analysis; 0=no limit"),
    cl::init(1000000), cl::cat(PollyCategory));

cl::opt<bool> DelicmOverapproximateWrites(
    "polly-delicm-overapproximate-writes",
    cl::desc("Do more PHI writes than necessary in order to avoid partial accesses"),
    cl::init(false), cl::Hidden, cl::cat(PollyCategory));

cl::opt<bool> DelicmPartialWrites("polly-delicm-partial-writes",
                                  cl::desc("Allow partial writes"),
                                  cl::init(true), cl::Hidden,
                                  cl::cat(PollyCategory));

cl::opt<bool> DelicmComputeKnown(
    "polly-delicm-compute-known",
    cl::desc("Compute known content of array elements"),
    cl::init(true), cl::Hidden, cl::cat(PollyCategory));

STATISTIC(DeLICMAnalyzed, "Number of successfully analyzed SCoPs");
STATISTIC(DeLICMOutOfQuota, "Analyses aborted because max_operations was reached");
STATISTIC(MappedValueScalars, "Number of mapped Value scalars");
STATISTIC(MappedPHIScalars, "Number of mapped PHI scalars");
STATISTIC(TargetsMapped, "Number of stores used for at least one mapping");
STATISTIC(DeLICMScopsModified, "Number of SCoPs optimized");

STATISTIC(NumValueWrites, "Number of scalar value writes after DeLICM");
STATISTIC(NumValueWritesInLoops, "Number of scalar value writes nested in affine loops after DeLICM");
STATISTIC(NumPHIWrites, "Number of scalar phi writes after DeLICM");
STATISTIC(NumPHIWritesInLoops, "Number of scalar phi writes nested in affine loops after DeLICM");
STATISTIC(NumSingletonWrites, "Number of singleton writes after DeLICM");
STATISTIC(NumSingletonWritesInLoops, "Number of singleton writes nested in affine loops after DeLICM");

isl::union_map computeReachingOverwrite(isl::union_map Schedule, isl::union_map Writes,
                                        bool InclPrevWrite, bool InclOverwrite) {
  return computeReachingWrite(Schedule, Writes, true, InclPrevWrite, InclOverwrite);
}

isl::union_map computeScalarReachingOverwrite(isl::union_map Schedule, isl::union_set Writes,
                                              bool InclPrevWrite, bool InclOverwrite) {
  auto WritesMap = isl::union_map::from_domain(Writes);
  auto Result = computeReachingOverwrite(std::move(Schedule), std::move(WritesMap), InclPrevWrite, InclOverwrite);
  return Result.domain_factor_range();
}

isl::map computeScalarReachingOverwrite(isl::union_map Schedule, isl::set Writes,
                                        bool InclPrevWrite, bool InclOverwrite) {
  isl::space ScatterSpace = getScatterSpace(Schedule);
  isl::space DomSpace = Writes.get_space();
  isl::union_map ReachOverwrite =
      computeScalarReachingOverwrite(std::move(Schedule), isl::union_set(Writes), InclPrevWrite, InclOverwrite);
  isl::space ResultSpace = ScatterSpace.map_from_domain_and_range(DomSpace);
  return singleton(std::move(ReachOverwrite), ResultSpace);
}

isl::union_map expandMapping(isl::union_map Relevant, isl::union_set Universe) {
  Relevant = Relevant.coalesce();
  isl::union_set RelevantDomain = Relevant.domain();
  isl::union_map Simplified = Relevant.gist_domain(RelevantDomain);
  Simplified = Simplified.coalesce();
  return Simplified.intersect_domain(Universe);
}

class Knowledge final {
private:
  isl::union_set Occupied;
  isl::union_set Unused;
  isl::union_map Known;
  isl::union_map Written;

  void checkConsistency() const {
#ifndef NDEBUG
    if (Occupied.is_null() && Unused.is_null() && Known.is_null() && Written.is_null())
      return;

    assert((!Occupied.is_null() || !Unused.is_null()) && "One of Occupied/Unused must be implicit");
    assert(!Known.is_null() && !Written.is_null());

    if (Occupied.is_null() || Unused.is_null())
      return;

    assert(Occupied.is_disjoint(Unused));
    isl::union_set Universe = Occupied.unite(Unused);

    assert(!Known.domain().is_subset(Universe).is_false());
    assert(!Written.domain().is_subset(Universe).is_false());
#endif
  }

public:
  Knowledge() {}

  Knowledge(isl::union_set Occupied, isl::union_set Unused, isl::union_map Known, isl::union_map Written)
      : Occupied(std::move(Occupied)), Unused(std::move(Unused)), Known(std::move(Known)),
        Written(std::move(Written)) {
    checkConsistency();
  }

  bool isUsable() const { return (Occupied.is_null() || Unused.is_null()) && !Known.is_null() && !Written.is_null(); }

  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const {
    if (isUsable()) {
      if (!Occupied.is_null())
        OS.indent(Indent) << "Occupied: " << Occupied << "\n";
      else
        OS.indent(Indent) << "Occupied: <Everything else not in Unused>\n";
      if (!Unused.is_null())
        OS.indent(Indent) << "Unused:   " << Unused << "\n";
      else
        OS.indent(Indent) << "Unused:   <Everything else not in Occupied>\n";
      OS.indent(Indent) << "Known:    " << Known << "\n";
      OS.indent(Indent) << "Written : " << Written << '\n';
    } else {
      OS.indent(Indent) << "Invalid knowledge\n";
    }
  }

  void learnFrom(Knowledge That) {
    assert(!isConflicting(*this, That));
    assert(!Unused.is_null() && !That.Occupied.is_null());
    assert(That.Unused.is_null());
    assert(Occupied.is_null());

    Unused = Unused.subtract(That.Occupied);
    Known = Known.unite(That.Known);
    Written = Written.unite(That.Written);

    checkConsistency();
  }

  static bool isConflicting(const Knowledge &Existing, const Knowledge &Proposed, llvm::raw_ostream *OS = nullptr,
                            unsigned Indent = 0) {
    assert(!Existing.Unused.is_null());
    assert(!Proposed.Occupied.is_null());

#ifndef NDEBUG
    if (!Existing.Occupied.is_null() && !Proposed.Unused.is_null()) {
      auto ExistingUniverse = Existing.Occupied.unite(Existing.Unused);
      auto ProposedUniverse = Proposed.Occupied.unite(Proposed.Unused);
      assert(ExistingUniverse.is_equal(ProposedUniverse) &&
             "Both inputs' Knowledges must be over the same universe");
    }
#endif

    isl::union_map ProposedOccupiedAnyVal = makeUnknownForDomain(Proposed.Occupied);
    isl::union_map ProposedValues = Proposed.Known.unite(ProposedOccupiedAnyVal);

    isl::union_map ExistingUnusedAnyVal = makeUnknownForDomain(Existing.Unused);
    isl::union_map ExistingValues = Existing.Known.unite(ExistingUnusedAnyVal);

    isl::union_map MatchingVals = ExistingValues.intersect(ProposedValues);
    isl::union_set Matches = MatchingVals.domain();

    if (!Proposed.Occupied.is_subset(Matches)) {
      if (OS) {
        isl::union_set Conflicting = Proposed.Occupied.subtract(Matches);
        isl::union_map ExistingConflictingKnown = Existing.Known.intersect_domain(Conflicting);
        isl::union_map ProposedConflictingKnown = Proposed.Known.intersect_domain(Conflicting);

        OS->indent(Indent) << "Proposed lifetime conflicting with Existing's\n";
        OS->indent(Indent) << "Conflicting occupied: " << Conflicting << "\n";
        if (!ExistingConflictingKnown.is_empty())
          OS->indent(Indent) << "Existing Known:       " << ExistingConflictingKnown << "\n";
        if (!ProposedConflictingKnown.is_empty())
          OS->indent(Indent) << "Proposed Known:       " << ProposedConflictingKnown << "\n";
      }
      return true;
    }

    isl::union_map ProposedFixedDefs = convertZoneToTimepoints(Proposed.Occupied, true, false);
    isl::union_map ProposedFixedKnown = convertZoneToTimepoints(Proposed.Known, isl::dim::in, true, false);

    isl::union_map ExistingConflictingWrites = Existing.Written.intersect_domain(ProposedFixedDefs);
    isl::union_set ExistingConflictingWritesDomain = ExistingConflictingWrites.domain();

    isl::union_map CommonWrittenVal = ProposedFixedKnown.intersect(ExistingConflictingWrites);
    isl::union_set CommonWrittenValDomain = CommonWrittenVal.domain();

    if (!ExistingConflictingWritesDomain.is_subset(CommonWrittenValDomain)) {
      if (OS) {
        isl::union_map ExistingConflictingWritten =
            ExistingConflictingWrites.subtract_domain(CommonWrittenValDomain);
        isl::union_map ProposedConflictingKnown = ProposedFixedKnown.subtract_domain(
            ExistingConflictingWritten.domain());

        OS->indent(Indent) << "Proposed a lifetime where there is an Existing write into it\n";
        OS->indent(Indent) << "Existing conflicting writes: " << ExistingConflictingWritten << "\n";
        if (!ProposedConflictingKnown.is_empty())
          OS->indent(Indent) << "Proposed conflicting known:  " << ProposedConflictingKnown << "\n";
      }
      return true;
    }

    isl::union_map ExistingAvailableDefs = convertZoneToTimepoints(Existing.Unused, true, false);
    isl::union_map ExistingKnownDefs = convertZoneToTimepoints(Existing.Known, isl::dim::in, true, false);

    isl::union_set ProposedWrittenDomain = Proposed.Written.domain();
    isl::union_map KnownIdentical = ExistingKnownDefs.intersect(Proposed.Written);
    isl::union_set IdenticalOrUnused = ExistingAvailableDefs.unite(KnownIdentical.domain());
    if (!ProposedWrittenDomain.is_subset(IdenticalOrUnused)) {
      if (OS) {
        isl::union_set Conflicting = ProposedWrittenDomain.subtract(IdenticalOrUnused);
        isl::union_map ExistingConflictingKnown = ExistingKnownDefs.intersect_domain(Conflicting);
        isl::union_map ProposedConflictingWritten = Proposed.Written.intersect_domain(Conflicting);

        OS->indent(Indent) << "Proposed writes into range used by Existing\n";
        OS->indent(Indent) << "Proposed conflicting writes: " << ProposedConflictingWritten << "\n";
        if (!ExistingConflictingKnown.is_empty())
          OS->indent(Indent) << "Existing conflicting known: " << ExistingConflictingKnown << "\n";
      }
      return true;
    }

    isl::union_set BothWritten = Existing.Written.domain().intersect(Proposed.Written.domain());
    isl::union_map ExistingKnownWritten = filterKnownValInst(Existing.Written);
    isl::union_map ProposedKnownWritten = filterKnownValInst(Proposed.Written);
    isl::union_set CommonWritten = ExistingKnownWritten.intersect(ProposedKnownWritten).domain();

    if (!BothWritten.is_subset(CommonWritten)) {
      if (OS) {
        isl::union_set Conflicting = BothWritten.subtract(CommonWritten);
        isl::union_map ExistingConflictingWritten = Existing.Written.intersect_domain(Conflicting);
        isl::union_map ProposedConflictingWritten = Proposed.Written.intersect_domain(Conflicting);

        OS->indent(Indent) << "Proposed writes at the same time as an already Existing write\n";
        OS->indent(Indent) << "Conflicting writes: " << Conflicting << "\n";
        if (!ExistingConflictingWritten.is_empty())
          OS->indent(Indent) << "Exiting write:      " << ExistingConflictingWritten << "\n";
        if (!ProposedConflictingWritten.is_empty())
          OS->indent(Indent) << "Proposed write:     " << ProposedConflictingWritten << "\n";
      }
      return true;
    }

    return false;
  }
};

class DeLICMImpl final : public ZoneAlgorithm {
private:
  Knowledge OriginalZone;
  Knowledge Zone;

  int NumberOfCompatibleTargets = 0;
  int NumberOfTargetsMapped = 0;
  int NumberOfMappedValueScalars = 0;
  int NumberOfMappedPHIScalars = 0;

  bool isConflicting(const Knowledge &Proposed) {
    raw_ostream *OS = nullptr;
    POLLY_DEBUG(OS = &llvm::dbgs());
    return Knowledge::isConflicting(Zone, Proposed, OS, 4);
  }

  bool isMappable(const ScopArrayInfo *SAI) {
    assert(SAI);

    if (SAI->isValueKind()) {
      auto *MA = S->getValueDef(SAI);
      if (!MA) {
        POLLY_DEBUG(dbgs() << "    Reject because value is read-only within the scop\n");
        return false;
      }
      auto Inst = MA->getAccessInstruction();
      for (auto User : Inst->users()) {
        if (!isa<Instruction>(User))
          return false;
        auto *UserInst = cast<Instruction>(User);
        if (!S->contains(UserInst)) {
          POLLY_DEBUG(dbgs() << "    Reject because value is escaping\n");
          return false;
        }
      }
      return true;
    }

    if (SAI->isPHIKind()) {
      auto *MA = S->getPHIRead(SAI);
      assert(MA);
      auto *PHI = cast<PHINode>(MA->getAccessInstruction());
      for (auto *Incoming : PHI->blocks()) {
        if (!S->contains(Incoming)) {
          POLLY_DEBUG(dbgs() << "    Reject because at least one incoming block is not in the scop region\n");
          return false;
        }
      }
      return true;
    }

    POLLY_DEBUG(dbgs() << "    Reject ExitPHI or other non-value\n");
    return false;
  }

  std::tuple<isl::union_map, isl::map> computeValueUses(const ScopArrayInfo *SAI) {
    assert(SAI->isValueKind());

    isl::union_set Reads = makeEmptyUnionSet();
    for (auto *MA : S->getValueUses(SAI))
      Reads = Reads.unite(getDomainFor(MA));

    isl::union_map ReadSchedule = getScatterFor(Reads);

    auto *DefMA = S->getValueDef(SAI);
    assert(DefMA);

    isl::union_set Writes = getDomainFor(DefMA);
    isl::union_map WriteScatter = getScatterFor(Writes);

    isl::union_map ReachDef = getScalarReachingDefinition(DefMA->getStatement());

    isl::union_map Uses =
        isl::union_map(ReachDef.reverse().range_map()).apply_range(ReadSchedule.reverse());

    isl::map UseScatter = singleton(Uses.domain().unwrap(),
                                    Writes.get_space().map_from_domain_and_range(ScatterSpace));

    isl::map Lifetime = betweenScatter(WriteScatter, UseScatter, false, true);
    isl::union_map DefUses = Uses.domain_factor_domain();

    return std::make_pair(DefUses, Lifetime);
  }

  bool tryMapValue(const ScopArrayInfo *SAI, isl::map TargetElt) {
    assert(SAI->isValueKind());

    auto *DefMA = S->getValueDef(SAI);
    assert(DefMA->isValueKind() && DefMA->isMustWrite());
    auto *V = DefMA->getAccessValue();
    auto *DefInst = DefMA->getAccessInstruction();

    if (!DefMA->getLatestScopArrayInfo()->isValueKind())
      return false;

    isl::union_map DefSched = getScatterFor(DefMA);

    isl::map DefTarget = TargetElt.apply_domain(DefSched.reverse());
    simplify(DefTarget);
    POLLY_DEBUG(dbgs() << "    Def Mapping: " << DefTarget << '\n');

    isl::union_set OrigDomain = getDomainFor(DefMA);
    isl::set MappedDomain = DefTarget.domain();
    if (!OrigDomain.is_subset(MappedDomain)) {
      POLLY_DEBUG(dbgs() << "    Reject because mapping does not encompass all instances\n");
      return false;
    }

    isl::map Lifetime;
    isl::union_map DefUses;
    std::tie(DefUses, Lifetime) = computeValueUses(SAI);
    POLLY_DEBUG(dbgs() << "    Lifetime: " << Lifetime << '\n');

    isl::union_set EltZone = Lifetime.apply_domain(DefTarget).wrap();
    simplify(EltZone);

    isl::map ValInst = DelicmComputeKnown
                           ? makeValInst(V, DefMA->getStatement(), LI->getLoopFor(DefInst->getParent()))
                           : makeUnknownForDomain(DefMA->getStatement());

    isl::map EltKnownTranslator = DefTarget.range_product(Lifetime);
    isl::union_map EltKnown = ValInst.apply_domain(EltKnownTranslator);
    simplify(EltKnown);

    isl::union_map WrittenTranslator = DefTarget.range_product(DefSched);
    isl::union_map DefEltSched = ValInst.apply_domain(WrittenTranslator);
    simplify(DefEltSched);

    Knowledge Proposed(EltZone, {}, filterKnownValInst(EltKnown), DefEltSched);
    if (isConflicting(Proposed))
      return false;

    isl::union_map UseTarget = DefUses.reverse().apply_range(DefTarget);
    mapValue(SAI, std::move(DefTarget), std::move(UseTarget), std::move(Lifetime), std::move(Proposed));
    return true;
  }

  void applyLifetime(Knowledge Proposed) { Zone.learnFrom(std::move(Proposed)); }

  void mapValue(const ScopArrayInfo *SAI, isl::map DefTarget, isl::union_map UseTarget, isl::map Lifetime,
                Knowledge Proposed) {
    for (auto *MA : S->getValueUses(SAI)) {
      isl::union_set Domain = getDomainFor(MA);
      isl::union_map NewAccRel = UseTarget.intersect_domain(Domain);
      simplify(NewAccRel);
      assert(isl_union_map_n_map(NewAccRel.get()) == 1);
      MA->setNewAccessRelation(isl::map::from_union_map(NewAccRel));
    }

    auto *WA = S->getValueDef(SAI);
    WA->setNewAccessRelation(DefTarget);
    applyLifetime(std::move(Proposed));

    MappedValueScalars++;
    NumberOfMappedValueScalars++;
  }

  isl::map makeValInst(Value *Val, ScopStmt *UserStmt, Loop *Scope, bool IsCertain = true) {
    if (!DelicmComputeKnown)
      return makeUnknownForDomain(UserStmt);
    return ZoneAlgorithm::makeValInst(Val, UserStmt, Scope, IsCertain);
  }

  isl::union_map determinePHIWrittenValues(const ScopArrayInfo *SAI) {
    isl::union_map Result = makeEmptyUnionMap();

    for (auto *MA : S->getPHIIncomings(SAI)) {
      isl::union_map ValInst;
      auto *WriteStmt = MA->getStatement();

      auto Incoming = MA->getIncoming();
      assert(!Incoming.empty());
      if (Incoming.size() == 1) {
        ValInst = makeValInst(Incoming[0].second, WriteStmt, LI->getLoopFor(Incoming[0].first));
      } else {
        ValInst = makeUnknownForDomain(WriteStmt);
      }

      Result = Result.unite(ValInst);
    }

    assert(Result.is_single_valued() && "Cannot have multiple incoming values for same incoming statement");
    return Result;
  }

  bool tryMapPHI(const ScopArrayInfo *SAI, isl::map TargetElt) {
    auto *PHIRead = S->getPHIRead(SAI);
    assert(PHIRead->isPHIKind() && PHIRead->isRead());

    if (!PHIRead->getLatestScopArrayInfo()->isPHIKind())
      return false;

    isl::union_map PHISched = getScatterFor(PHIRead);
    isl::map PHITarget = PHISched.apply_range(TargetElt);
    simplify(PHITarget);
    POLLY_DEBUG(dbgs() << "    Mapping: " << PHITarget << '\n');

    isl::union_set OrigDomain = getDomainFor(PHIRead);
    isl::set MappedDomain = PHITarget.domain();
    if (!OrigDomain.is_subset(MappedDomain)) {
      POLLY_DEBUG(dbgs() << "    Reject because mapping does not encompass all instances\n");
      return false;
    }

    isl::union_map PerPHIWrites = computePerPHI(SAI);
    if (PerPHIWrites.is_null()) {
      POLLY_DEBUG(dbgs() << "    Reject because cannot determine incoming values\n");
      return false;
    }

    isl::union_map WritesTarget = PerPHIWrites.apply_domain(PHITarget).reverse();
    simplify(WritesTarget);

    isl::union_set UniverseWritesDom = isl::union_set::empty(ParamSpace.ctx());
    for (auto *MA : S->getPHIIncomings(SAI))
      UniverseWritesDom = UniverseWritesDom.unite(getDomainFor(MA));

    isl::union_map RelevantWritesTarget = WritesTarget;
    if (DelicmOverapproximateWrites)
      WritesTarget = expandMapping(WritesTarget, UniverseWritesDom);

    isl::union_set ExpandedWritesDom = WritesTarget.domain();
    if (!DelicmPartialWrites && !UniverseWritesDom.is_subset(ExpandedWritesDom)) {
      POLLY_DEBUG(dbgs() << "    Reject because did not find PHI write mapping for all instances\n");
      if (DelicmOverapproximateWrites)
        POLLY_DEBUG(dbgs() << "      Relevant Mapping:    " << RelevantWritesTarget << '\n');
      POLLY_DEBUG(dbgs() << "      Deduced Mapping:     " << WritesTarget << '\n');
      POLLY_DEBUG(dbgs() << "      Missing instances:    "
                         << UniverseWritesDom.subtract(ExpandedWritesDom) << '\n');
      return false;
    }

    isl::union_map PerPHIWriteScatterUmap = PerPHIWrites.apply_range(Schedule);
    isl::map PerPHIWriteScatter = singleton(PerPHIWriteScatterUmap, PHISched.get_space());

    isl::map Lifetime = betweenScatter(PerPHIWriteScatter, PHISched, false, true);
    simplify(Lifetime);
    POLLY_DEBUG(dbgs() << "    Lifetime: " << Lifetime << "\n");

    isl::union_map WriteLifetime = isl::union_map(Lifetime).apply_domain(PerPHIWrites);
    isl::union_map WrittenValue = determinePHIWrittenValues(SAI);

    isl::union_map WrittenTranslator = WritesTarget.range_product(Schedule);
    isl::union_map Written = WrittenValue.apply_domain(WrittenTranslator);
    simplify(Written);

    isl::union_map LifetimeTranslator = WritesTarget.range_product(WriteLifetime);

    isl::union_map WrittenKnownValue = filterKnownValInst(WrittenValue);
    isl::union_map EltLifetimeInst = WrittenKnownValue.apply_domain(LifetimeTranslator);
    simplify(EltLifetimeInst);

    isl::union_set Occupied = LifetimeTranslator.range();
    simplify(Occupied);

    Knowledge Proposed(Occupied, {}, EltLifetimeInst, Written);
    if (isConflicting(Proposed))
      return false;

    mapPHI(SAI, std::move(PHITarget), std::move(WritesTarget), std::move(Lifetime), std::move(Proposed));
    return true;
  }

  void mapPHI(const ScopArrayInfo *SAI, isl::map ReadTarget, isl::union_map WriteTarget, isl::map Lifetime,
              Knowledge Proposed) {
    isl::space ElementSpace = ReadTarget.get_space().range();

    for (auto *MA : S->getPHIIncomings(SAI)) {
      isl::union_set Domain = getDomainFor(MA);
      isl::union_map NewAccRel = WriteTarget.intersect_domain(Domain);
      simplify(NewAccRel);

      isl::space NewAccRelSpace = Domain.get_space().map_from_domain_and_range(ElementSpace);
      isl::map NewAccRelMap = singleton(NewAccRel, NewAccRelSpace);
      MA->setNewAccessRelation(NewAccRelMap);
    }

    auto *PHIRead = S->getPHIRead(SAI);
    PHIRead->setNewAccessRelation(ReadTarget);
    applyLifetime(std::move(Proposed));

    MappedPHIScalars++;
    NumberOfMappedPHIScalars++;
  }

  bool collapseScalarsToStore(MemoryAccess *TargetStoreMA) {
    assert(TargetStoreMA->isLatestArrayKind() && TargetStoreMA->isMustWrite());

    auto *TargetStmt = TargetStoreMA->getStatement();

    isl::union_set TargetDom = getDomainFor(TargetStmt);
    isl::union_map TargetAccRel = getAccessRelationFor(TargetStoreMA);

    isl::union_map Target =
        computeScalarReachingOverwrite(Schedule, TargetDom, false, true);
    isl::union_map EltTarget = Target.apply_range(TargetAccRel);
    simplify(EltTarget);
    POLLY_DEBUG(dbgs() << "    Target mapping is " << EltTarget << '\n');

    SmallVector<MemoryAccess *, 16> Worklist;
    SmallPtrSet<const ScopArrayInfo *, 16> Closed;

    auto ProcessAllIncoming = [&](ScopStmt *Stmt) {
      for (auto *MA : *Stmt) {
        if (!MA->isLatestScalarKind())
          continue;
        if (!MA->isRead())
          continue;
        Worklist.push_back(MA);
      }
    };

    auto *WrittenVal = TargetStoreMA->getAccessInstruction()->getOperand(0);
    if (auto *WrittenValInputMA = TargetStmt->lookupInputAccessOf(WrittenVal))
      Worklist.push_back(WrittenValInputMA);
    else
      ProcessAllIncoming(TargetStmt);

    bool AnyMapped = false;
    const DataLayout &DL = S->getRegion().getEntry()->getModule()->getDataLayout();
    uint64_t StoreSize = DL.getTypeAllocSize(TargetStoreMA->getAccessValue()->getType());

    while (!Worklist.empty()) {
      auto *MA = Worklist.pop_back_val();
      const ScopArrayInfo *SAI = MA->getScopArrayInfo();
      if (Closed.count(SAI))
        continue;
      Closed.insert(SAI);
      POLLY_DEBUG(dbgs() << "\n    Trying to map " << MA << " (SAI: " << SAI << ")\n");

      if (!isMappable(SAI))
        continue;

      uint64_t MASize = DL.getTypeAllocSize(MA->getAccessValue()->getType());
      if (MASize > StoreSize) {
        POLLY_DEBUG(dbgs() << "    Reject because storage size is insufficient\n");
        continue;
      }

      if (SAI->isValueKind()) {
        if (!tryMapValue(SAI, EltTarget))
          continue;

        auto *DefAcc = S->getValueDef(SAI);
        ProcessAllIncoming(DefAcc->getStatement());

        AnyMapped = true;
        continue;
      }

      if (SAI->isPHIKind()) {
        if (!tryMapPHI(SAI, EltTarget))
          continue;

        for (auto *PHIWrite : S->getPHIIncomings(SAI)) {
          auto *PHIWriteStmt = PHIWrite->getStatement();
          bool FoundAny = false;
          for (auto Incoming : PHIWrite->getIncoming()) {
            if (auto *IncomingInputMA = PHIWriteStmt->lookupInputAccessOf(Incoming.second)) {
              Worklist.push_back(IncomingInputMA);
              FoundAny = true;
            }
          }

          if (!FoundAny)
            ProcessAllIncoming(PHIWrite->getStatement());
        }

        AnyMapped = true;
        continue;
      }
    }

    if (AnyMapped) {
      TargetsMapped++;
      NumberOfTargetsMapped++;
    }
    return AnyMapped;
  }

  isl::union_set computeLifetime() const {
    isl::union_map ArrayUnused =
        computeArrayUnused(Schedule, AllMustWrites, AllReads, false, false, true);
    isl::union_set Result = ArrayUnused.wrap();
    simplify(Result);
    return Result;
  }

  isl::union_map computeWritten() const {
    isl::union_map EltWritten = applyDomainRange(AllWriteValInst, Schedule);
    simplify(EltWritten);
    return EltWritten;
  }

  bool isScalarAccess(MemoryAccess *MA) {
    isl::union_map Map = getAccessRelationFor(MA);
    isl::union_set Set = Map.range();
    return Set.is_singleton();
  }

  void printStatistics(llvm::raw_ostream &OS, int Indent = 0) const {
    OS.indent(Indent) << "Statistics {\n";
    OS.indent(Indent + 4) << "Compatible overwrites: " << NumberOfCompatibleTargets << "\n";
    OS.indent(Indent + 4) << "Overwrites mapped to:  " << NumberOfTargetsMapped << '\n';
    OS.indent(Indent + 4) << "Value scalars mapped:  " << NumberOfMappedValueScalars << '\n';
    OS.indent(Indent + 4) << "PHI scalars mapped:    " << NumberOfMappedPHIScalars << '\n';
    OS.indent(Indent) << "}\n";
  }

public:
  DeLICMImpl(Scop *S, LoopInfo *LI) : ZoneAlgorithm("polly-delicm", S, LI) {}

  bool computeZone() {
    collectCompatibleElts();

    isl::union_set EltUnused;
    isl::union_map EltKnown, EltWritten;

    {
      IslMaxOperationsGuard MaxOpGuard(IslCtx.get(), DelicmMaxOps);

      computeCommon();
      EltUnused = computeLifetime();
      EltKnown = computeKnown(true, false);
      EltWritten = computeWritten();
    }

    if (EltUnused.is_null() || EltKnown.is_null() || EltWritten.is_null()) {
      assert(isl_ctx_last_error(IslCtx.get()) == isl_error_quota);
      DeLICMOutOfQuota++;
      POLLY_DEBUG(dbgs() << "DeLICM analysis exceeded max_operations\n");
      DebugLoc Begin, End;
      getDebugLocations(getBBPairForRegion(&S->getRegion()), Begin, End);
      OptimizationRemarkAnalysis R(DEBUG_TYPE, "OutOfQuota", Begin, S->getEntry());
      R << "maximal number of operations exceeded during zone analysis";
      S->getFunction().getContext().diagnose(R);
      return false;
    }

    Zone = OriginalZone = Knowledge({}, EltUnused, EltKnown, EltWritten);
    POLLY_DEBUG(dbgs() << "Computed Zone:\n"; OriginalZone.print(dbgs(), 4));

    DeLICMAnalyzed++;
    assert(Zone.isUsable() && OriginalZone.isUsable());
    return true;
  }

  void greedyCollapse() {
    bool Modified = false;

    for (auto &Stmt : *S) {
      for (auto *MA : Stmt) {
        if (!MA->isLatestArrayKind())
          continue;
        if (!MA->isWrite())
          continue;

        if (MA->isMayWrite()) {
          POLLY_DEBUG(dbgs() << "Access " << MA << " pruned because it is a MAY_WRITE\n");
          OptimizationRemarkMissed R(DEBUG_TYPE, "TargetMayWrite", MA->getAccessInstruction());
          R << "Skipped possible mapping target because it is not an unconditional overwrite";
          S->getFunction().getContext().diagnose(R);
          continue;
        }

        if (Stmt.getNumIterators() == 0) {
          POLLY_DEBUG(dbgs() << "Access " << MA << " pruned because it is not in a loop\n");
          OptimizationRemarkMissed R(DEBUG_TYPE, "WriteNotInLoop", MA->getAccessInstruction());
          R << "skipped possible mapping target because it is not in a loop";
          S->getFunction().getContext().diagnose(R);
          continue;
        }

        if (isScalarAccess(MA)) {
          POLLY_DEBUG(dbgs() << "Access " << MA << " pruned because it writes only a single element\n");
          OptimizationRemarkMissed R(DEBUG_TYPE, "ScalarWrite", MA->getAccessInstruction());
          R << "skipped possible mapping target because the memory location written to does not depend on its outer loop";
          S->getFunction().getContext().diagnose(R);
          continue;
        }

        if (!isa<StoreInst>(MA->getAccessInstruction())) {
          POLLY_DEBUG(dbgs() << "Access " << MA << " pruned because it is not a StoreInst\n");
          OptimizationRemarkMissed R(DEBUG_TYPE, "NotAStore", MA->getAccessInstruction());
          R << "skipped possible mapping target because non-store instructions are not supported";
          S->getFunction().getContext().diagnose(R);
          continue;
        }

        isl::union_map AccRel = MA->getLatestAccessRelation();
        if (!AccRel.is_single_valued().is_true()) {
          POLLY_DEBUG(dbgs() << "Access " << MA
                             << " is incompatible because it writes multiple elements per instance\n");
          OptimizationRemarkMissed R(DEBUG_TYPE, "NonFunctionalAccRel", MA->getAccessInstruction());
          R << "skipped possible mapping target because it writes more than one element";
          S->getFunction().getContext().diagnose(R);
          continue;
        }

        isl::union_set TouchedElts = AccRel.range();
        if (!TouchedElts.is_subset(CompatibleElts)) {
          POLLY_DEBUG(dbgs() << "Access " << MA
                             << " is incompatible because it touches incompatible elements\n");
          OptimizationRemarkMissed R(DEBUG_TYPE, "IncompatibleElts", MA->getAccessInstruction());
          R << "skipped possible mapping target because a target location cannot be reliably analyzed";
          S->getFunction().getContext().diagnose(R);
          continue;
        }

        assert(isCompatibleAccess(MA));
        NumberOfCompatibleTargets++;
        POLLY_DEBUG(dbgs() << "Analyzing target access " << MA << "\n");
        if (collapseScalarsToStore(MA))
          Modified = true;
      }
    }

    if (Modified)
      DeLICMScopsModified++;
  }

  void print(llvm::raw_ostream &OS, int Indent = 0) {
    if (!Zone.isUsable()) {
      OS.indent(Indent) << "Zone not computed\n";
      return;
    }

    printStatistics(OS, Indent);
    if (!isModified()) {
      OS.indent(Indent) << "No modification has been made\n";
      return;
    }
    printAccesses(OS, Indent);
  }

  bool isModified() const { return NumberOfTargetsMapped > 0; }
};

static std::unique_ptr<DeLICMImpl> collapseToUnused(Scop &S, LoopInfo &LI) {
  std::unique_ptr<DeLICMImpl> Impl = std::make_unique<DeLICMImpl>(&S, &LI);

  if (!Impl->computeZone()) {
    POLLY_DEBUG(dbgs() << "Abort because cannot reliably compute lifetimes\n");
    return Impl;
  }

  POLLY_DEBUG(dbgs() << "Collapsing scalars to unused array elements...\n");
  Impl->greedyCollapse();

  POLLY_DEBUG(dbgs() << "\nFinal Scop:\n");
  POLLY_DEBUG(dbgs() << S);

  return Impl;
}

static std::unique_ptr<DeLICMImpl> runDeLICM(Scop &S, LoopInfo &LI) {
  std::unique_ptr<DeLICMImpl> Impl = collapseToUnused(S, LI);

  Scop::ScopStatistics ScopStats = S.getStatistics();
  NumValueWrites += ScopStats.NumValueWrites;
  NumValueWritesInLoops += ScopStats.NumValueWritesInLoops;
  NumPHIWrites += ScopStats.NumPHIWrites;
  NumPHIWritesInLoops += ScopStats.NumPHIWritesInLoops;
  NumSingletonWrites += ScopStats.NumSingletonWrites;
  NumSingletonWritesInLoops += ScopStats.NumSingletonWritesInLoops;

  return Impl;
}

static PreservedAnalyses runDeLICMUsingNPM(Scop &S, ScopAnalysisManager &SAM, ScopStandardAnalysisResults &SAR,
                                           SPMUpdater &U, raw_ostream *OS) {
  LoopInfo &LI = SAR.LI;
  std::unique_ptr<DeLICMImpl> Impl = runDeLICM(S, LI);

  if (OS) {
    *OS << "Printing analysis 'Polly - DeLICM/DePRE' for region: '" << S.getName() << "' in function '"
        << S.getFunction().getName() << "':\n";
    if (Impl) {
      assert(Impl->getScop() == &S);
      *OS << "DeLICM result:\n";
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

class DeLICMWrapperPass final : public ScopPass {
private:
  DeLICMWrapperPass(const DeLICMWrapperPass &) = delete;
  const DeLICMWrapperPass &operator=(const DeLICMWrapperPass &) = delete;

  std::unique_ptr<DeLICMImpl> Impl;

public:
  static char ID;
  explicit DeLICMWrapperPass() : ScopPass(ID) {}

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequiredTransitive<ScopInfoRegionPass>();
    AU.addRequired<LoopInfoWrapperPass>();
    AU.setPreservesAll();
  }

  bool runOnScop(Scop &S) override {
    releaseMemory();
    auto &LI = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
    Impl = runDeLICM(S, LI);
    return Impl->isModified();
  }

  void printScop(raw_ostream &OS, Scop &S) const override {
    if (!Impl)
      return;
    assert(Impl->getScop() == &S);
    OS << "DeLICM result:\n";
    Impl->print(OS);
  }

  void releaseMemory() override { Impl.reset(); }
};

char DeLICMWrapperPass::ID = 0;

class DeLICMPrinterLegacyPass final : public ScopPass {
public:
  static char ID;

  DeLICMPrinterLegacyPass() : DeLICMPrinterLegacyPass(outs()) {}
  explicit DeLICMPrinterLegacyPass(llvm::raw_ostream &OS) : ScopPass(ID), OS(OS) {}

  bool runOnScop(Scop &S) override {
    DeLICMWrapperPass &P = getAnalysis<DeLICMWrapperPass>();

    OS << "Printing analysis '" << P.getPassName() << "' for region: '" << S.getRegion().getNameStr()
       << "' in function '" << S.getFunction().getName() << "':\n";
    P.printScop(OS, S);

    return false;
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    ScopPass::getAnalysisUsage(AU);
    AU.addRequired<DeLICMWrapperPass>();
    AU.setPreservesAll();
  }

private:
  llvm::raw_ostream &OS;
};

char DeLICMPrinterLegacyPass::ID = 0;

} // anonymous namespace

Pass *polly::createDeLICMWrapperPass() { return new DeLICMWrapperPass(); }

llvm::Pass *polly::createDeLICMPrinterLegacyPass(llvm::raw_ostream &OS) {
  return new DeLICMPrinterLegacyPass(OS);
}

llvm::PreservedAnalyses polly::DeLICMPass::run(Scop &S, ScopAnalysisManager &SAM, ScopStandardAnalysisResults &SAR,
                                               SPMUpdater &U) {
  return runDeLICMUsingNPM(S, SAM, SAR, U, nullptr);
}

llvm::PreservedAnalyses polly::DeLICMPrinterPass::run(Scop &S, ScopAnalysisManager &SAM,
                                                      ScopStandardAnalysisResults &SAR, SPMUpdater &U) {
  return runDeLICMUsingNPM(S, SAM, SAR, U, &OS);
}

bool polly::isConflicting(isl::union_set ExistingOccupied, isl::union_set ExistingUnused,
                          isl::union_map ExistingKnown, isl::union_map ExistingWrites,
                          isl::union_set ProposedOccupied, isl::union_set ProposedUnused,
                          isl::union_map ProposedKnown, isl::union_map ProposedWrites, llvm::raw_ostream *OS,
                          unsigned Indent) {
  Knowledge Existing(std::move(ExistingOccupied), std::move(ExistingUnused), std::move(ExistingKnown),
                     std::move(ExistingWrites));
  Knowledge Proposed(std::move(ProposedOccupied), std::move(ProposedUnused), std::move(ProposedKnown),
                     std::move(ProposedWrites));

  return Knowledge::isConflicting(Existing, Proposed, OS, Indent);
}

INITIALIZE_PASS_BEGIN(DeLICMWrapperPass, "polly-delicm", "Polly - DeLICM/DePRE", false, false)
INITIALIZE_PASS_DEPENDENCY(ScopInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_END(DeLICMWrapperPass, "polly-delicm", "Polly - DeLICM/DePRE", false, false)

INITIALIZE_PASS_BEGIN(DeLICMPrinterLegacyPass, "polly-print-delicm", "Polly - Print DeLICM/DePRE", false, false)
INITIALIZE_PASS_DEPENDENCY(ScopInfoWrapperPass)
INITIALIZE_PASS_END(DeLICMPrinterLegacyPass, "polly-print-delicm", "Polly - Print DeLICM/DePRE", false, false)
