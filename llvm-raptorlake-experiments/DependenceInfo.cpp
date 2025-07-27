//===- DependenceInfo.cpp - Calculate dependency information for a Scop. --===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Calculate the data dependency relations for a Scop using ISL.
//
// The integer set library (ISL) from Sven, has a integrated dependency analysis
// to calculate data dependences. This pass takes advantage of this and
// calculate those dependences a Scop.
//
// The dependences in this pass are exact in terms that for a specific read
// statement instance only the last write statement instance is returned. In
// case of may writes a set of possible write instances is returned. This
// analysis will never produce redundant dependences.
//
//===----------------------------------------------------------------------===//
//
#include "polly/DependenceInfo.h"
#include "polly/LinkAllPasses.h"
#include "polly/Options.h"
#include "polly/ScopInfo.h"
#include "polly/Support/GICHelper.h"
#include "polly/Support/ISLTools.h"
#include "llvm/ADT/Sequence.h"
#include "llvm/Support/Debug.h"
#include "isl/aff.h"
#include "isl/ctx.h"
#include "isl/flow.h"
#include "isl/map.h"
#include "isl/options.h"
#include "isl/schedule.h"
#include "isl/set.h"
#include "isl/union_map.h"
#include "isl/union_set.h"

#include <future>

using namespace polly;
using namespace llvm;

#include "polly/Support/PollyDebug.h"
#define DEBUG_TYPE "polly-dependence"

static cl::opt<int> OptComputeOut(
    "polly-dependences-computeout",
    cl::desc("Bound the dependence analysis by a maximal amount of "
             "computational steps (0 means no bound)"),
    cl::Hidden, cl::init(500000), cl::cat(PollyCategory));

static cl::opt<bool>
    LegalityCheckDisabled("disable-polly-legality",
                          cl::desc("Disable polly legality check"), cl::Hidden,
                          cl::cat(PollyCategory));

static cl::opt<bool>
    UseReductions("polly-dependences-use-reductions",
                  cl::desc("Exploit reductions in dependence analysis"),
                  cl::Hidden, cl::init(true), cl::cat(PollyCategory));

enum AnalysisType { VALUE_BASED_ANALYSIS, MEMORY_BASED_ANALYSIS };

static cl::opt<enum AnalysisType> OptAnalysisType(
    "polly-dependences-analysis-type",
    cl::desc("The kind of dependence analysis to use"),
    cl::values(clEnumValN(VALUE_BASED_ANALYSIS, "value-based",
                          "Exact dependences without transitive dependences"),
               clEnumValN(MEMORY_BASED_ANALYSIS, "memory-based",
                          "Overapproximation of dependences")),
    cl::Hidden, cl::init(VALUE_BASED_ANALYSIS), cl::cat(PollyCategory));

static cl::opt<Dependences::AnalysisLevel> OptAnalysisLevel(
    "polly-dependences-analysis-level",
    cl::desc("The level of dependence analysis"),
    cl::values(clEnumValN(Dependences::AL_Statement, "statement-wise",
                          "Statement-level analysis"),
               clEnumValN(Dependences::AL_Reference, "reference-wise",
                          "Memory reference level analysis that distinguish"
                          " accessed references in the same statement"),
               clEnumValN(Dependences::AL_Access, "access-wise",
                          "Memory reference level analysis that distinguish"
                          " access instructions in the same statement")),
    cl::Hidden, cl::init(Dependences::AL_Statement), cl::cat(PollyCategory));

//===----------------------------------------------------------------------===//

/// Tag the @p Relation domain with @p TagId
static __isl_give isl_map *tag(__isl_take isl_map *Relation,
                               __isl_take isl_id *TagId) {
  isl_space *Space = isl_map_get_space(Relation);
  Space = isl_space_drop_dims(Space, isl_dim_out, 0,
                              isl_map_dim(Relation, isl_dim_out));
  Space = isl_space_set_tuple_id(Space, isl_dim_out, TagId);
  isl_multi_aff *Tag = isl_multi_aff_domain_map(Space);
  Relation = isl_map_preimage_domain_multi_aff(Relation, Tag);
  return Relation;
}

/// Tag the @p Relation domain with either MA->getArrayId() or
///        MA->getId() based on @p TagLevel
static __isl_give isl_map *tag(__isl_take isl_map *Relation, MemoryAccess *MA,
                               Dependences::AnalysisLevel TagLevel) {
  if (TagLevel == Dependences::AL_Reference)
    return tag(Relation, MA->getArrayId().release());

  if (TagLevel == Dependences::AL_Access)
    return tag(Relation, MA->getId().release());

  // No need to tag at the statement level.
  return Relation;
}

/// Collect information about the SCoP @p S.
static void collectInfo(Scop &S, isl_union_map *&Read,
                        isl_union_map *&MustWrite, isl_union_map *&MayWrite,
                        isl_union_map *&ReductionTagMap,
                        isl_union_set *&TaggedStmtDomain,
                        Dependences::AnalysisLevel Level) {
  isl_space *Space = S.getParamSpace().release();
  Read = isl_union_map_empty(isl_space_copy(Space));
  MustWrite = isl_union_map_empty(isl_space_copy(Space));
  MayWrite = isl_union_map_empty(isl_space_copy(Space));
  ReductionTagMap = isl_union_map_empty(isl_space_copy(Space));
  isl_union_map *StmtSchedule = isl_union_map_empty(Space);

  SmallPtrSet<const ScopArrayInfo *, 8> ReductionArrays;
  if (UseReductions)
    for (ScopStmt &Stmt : S)
      for (MemoryAccess *MA : Stmt)
        if (MA->isReductionLike())
          ReductionArrays.insert(MA->getScopArrayInfo());

  for (ScopStmt &Stmt : S) {
    for (MemoryAccess *MA : Stmt) {
      isl_set *domcp = Stmt.getDomain().release();
      isl_map *accdom = MA->getAccessRelation().release();

      accdom = isl_map_intersect_domain(accdom, domcp);

      if (ReductionArrays.count(MA->getScopArrayInfo())) {
        ReductionTagMap =
            isl_union_map_add_map(ReductionTagMap, isl_map_copy(accdom));
        accdom = isl_map_range_map(accdom);
      } else {
        accdom = tag(accdom, MA, Level);
        if (Level > Dependences::AL_Statement) {
          isl_map *StmtScheduleMap = Stmt.getSchedule().release();
          assert(StmtScheduleMap &&
                 "Schedules that contain extension nodes require special "
                 "handling.");
          isl_map *Schedule = tag(StmtScheduleMap, MA, Level);
          StmtSchedule = isl_union_map_add_map(StmtSchedule, Schedule);
        }
      }

      if (MA->isRead())
        Read = isl_union_map_add_map(Read, accdom);
      else if (MA->isMayWrite())
        MayWrite = isl_union_map_add_map(MayWrite, accdom);
      else
        MustWrite = isl_union_map_add_map(MustWrite, accdom);
    }

    if (!ReductionArrays.empty() && Level == Dependences::AL_Statement)
      StmtSchedule =
          isl_union_map_add_map(StmtSchedule, Stmt.getSchedule().release());
  }

  StmtSchedule = isl_union_map_intersect_params(
      StmtSchedule, S.getAssumedContext().release());
  TaggedStmtDomain = isl_union_map_domain(StmtSchedule);

  ReductionTagMap = isl_union_map_coalesce(ReductionTagMap);
  Read = isl_union_map_coalesce(Read);
  MustWrite = isl_union_map_coalesce(MustWrite);
  MayWrite = isl_union_map_coalesce(MayWrite);
}

/// Fix all dimension of @p Zero to 0 and add it to @p user
static void fixSetToZero(isl::set Zero, isl::union_set *User) {
  for (auto i : rangeIslSize(0, Zero.tuple_dim()))
    Zero = Zero.fix_si(isl::dim::set, i, 0);
  *User = User->unite(Zero);
}

void Dependences::addPrivatizationDependences(isl_union_map *TC) {
  TC_RED = TC;

  isl_union_set *UDeltas = isl_union_map_deltas(isl_union_map_copy(TC_RED));
  isl_union_set *Universe = isl_union_set_universe(isl_union_set_copy(UDeltas));

  isl::union_set Zero =
  isl::manage(isl_union_set_empty(isl_union_set_get_space(Universe)));
  for (isl::set Set : isl::manage_copy(Universe).get_set_list()) {
    fixSetToZero(Set, &Zero);
  }

  isl_union_map *NonPositive =
  isl_union_set_lex_le_union_set(UDeltas, Zero.release());

  TC_RED = isl_union_map_subtract(TC_RED, NonPositive);
  TC_RED = isl_union_map_coalesce(TC_RED);

  if (isl_union_map_is_empty(TC_RED)) {
    isl_union_set_free(Universe);
    return;
  }

  isl_union_map **Maps[] = {&RAW, &WAW, &WAR};
  for (unsigned u = 0; u < 3; u++) {
    isl_union_map **Map = Maps[u];
    isl_union_map *PrivMap;

    PrivMap = isl_union_map_apply_range(isl_union_map_copy(TC_RED),
                                        isl_union_map_copy(*Map));

    isl_union_map *Map_o_TC =
    isl_union_map_apply_range(isl_union_map_copy(*Map),
                              isl_union_map_copy(TC_RED));

    PrivMap = isl_union_map_union(PrivMap, Map_o_TC);
    *Map = isl_union_map_union(*Map, PrivMap);
  }

  isl_union_set_free(Universe);
}

static __isl_give isl_union_flow *buildFlow(__isl_keep isl_union_map *Snk,
                                            __isl_keep isl_union_map *Src,
                                            __isl_keep isl_union_map *MaySrc,
                                            __isl_keep isl_union_map *Kill,
                                            __isl_keep isl_schedule *Schedule) {
  isl_union_access_info *AI;

  AI = isl_union_access_info_from_sink(isl_union_map_copy(Snk));
  if (MaySrc)
    AI = isl_union_access_info_set_may_source(AI, isl_union_map_copy(MaySrc));
  if (Src)
    AI = isl_union_access_info_set_must_source(AI, isl_union_map_copy(Src));
  if (Kill)
    AI = isl_union_access_info_set_kill(AI, isl_union_map_copy(Kill));
  AI = isl_union_access_info_set_schedule(AI, isl_schedule_copy(Schedule));
  auto Flow = isl_union_access_info_compute_flow(AI);
  POLLY_DEBUG(if (!Flow) dbgs()
                  << "last error: "
                  << isl_ctx_last_error(isl_schedule_get_ctx(Schedule))
                  << '\n';);
  return Flow;
}

void Dependences::calculateDependences(Scop &S) {
  isl_union_map *Read, *MustWrite, *MayWrite, *ReductionTagMap;
  isl_schedule *Schedule;
  isl_union_set *TaggedStmtDomain;

  POLLY_DEBUG(dbgs() << "Scop: \n" << S << "\n");

  collectInfo(S, Read, MustWrite, MayWrite, ReductionTagMap, TaggedStmtDomain,
              Level);

  // OPTIMIZATION: Add fast paths for trivial cases.
  isl_union_map *AllWrites = isl_union_map_union(isl_union_map_copy(MustWrite),
                                                 isl_union_map_copy(MayWrite));
  if (isl_union_map_is_empty(AllWrites)) {
    isl_space *Space = S.getParamSpace().release();
    RAW = isl_union_map_empty(isl_space_copy(Space));
    WAR = isl_union_map_empty(isl_space_copy(Space));
    WAW = isl_union_map_empty(isl_space_copy(Space));
    RED = isl_union_map_empty(isl_space_copy(Space));
    TC_RED = isl_union_map_empty(Space);
    isl_union_map_free(AllWrites);
    isl_union_map_free(Read);
    isl_union_map_free(MustWrite);
    isl_union_map_free(MayWrite);
    isl_union_map_free(ReductionTagMap);
    isl_union_set_free(TaggedStmtDomain);
    return;
  }
  if (isl_union_map_is_empty(Read)) {
    isl_space *Space = S.getParamSpace().release();
    RAW = isl_union_map_empty(isl_space_copy(Space));
    WAR = isl_union_map_empty(Space);
    // Fall through to calculate WAW dependencies.
  }
  isl_union_map_free(AllWrites);

  bool HasRedructions =
  UseReductions && !isl_union_map_is_empty(ReductionTagMap);

  POLLY_DEBUG(dbgs() << "Read: " << Read << '\n';
  dbgs() << "MustWrite: " << MustWrite << '\n';
  dbgs() << "MayWrite: " << MayWrite << '\n';
  dbgs() << "ReductionTagMap: " << ReductionTagMap << '\n';
  dbgs() << "TaggedStmtDomain: " << TaggedStmtDomain << '\n';);

  Schedule = S.getScheduleTree().release();

  if (!HasRedructions) {
    isl_union_map_free(ReductionTagMap);
    if (Level > AL_Statement) {
      auto TaggedMap =
      isl_union_set_unwrap(isl_union_set_copy(TaggedStmtDomain));
      auto Tags = isl_union_map_domain_map_union_pw_multi_aff(TaggedMap);
      Schedule = isl_schedule_pullback_union_pw_multi_aff(Schedule, Tags);
    }
  } else {
    isl_union_map *IdentityMap;
    isl_union_pw_multi_aff *ReductionTags, *IdentityTags, *Tags;
    ReductionTags =
    isl_union_map_domain_map_union_pw_multi_aff(ReductionTagMap);
    IdentityMap = isl_union_set_identity(isl_union_set_copy(TaggedStmtDomain));
    IdentityTags = isl_union_pw_multi_aff_from_union_map(IdentityMap);
    Tags = isl_union_pw_multi_aff_union_add(ReductionTags, IdentityTags);
    Schedule = isl_schedule_pullback_union_pw_multi_aff(Schedule, Tags);
  }

  POLLY_DEBUG(dbgs() << "Read: " << Read << "\n";
  dbgs() << "MustWrite: " << MustWrite << "\n";
  dbgs() << "MayWrite: " << MayWrite << "\n";
  dbgs() << "Schedule: " << Schedule << "\n");

  isl_union_map *StrictWAW = nullptr;
  {
    IslMaxOperationsGuard MaxOpGuard(getIslCtx(), OptComputeOut);

    if (!RAW) { // If not handled by a fast path
      isl_union_map *Write = isl_union_map_union(isl_union_map_copy(MustWrite),
                                                 isl_union_map_copy(MayWrite));

      isl_union_flow *Flow = buildFlow(Write, Write, Read, nullptr, Schedule);
      StrictWAW = isl_union_flow_get_must_dependence(Flow);
      isl_union_flow_free(Flow);

      if (OptAnalysisType == VALUE_BASED_ANALYSIS) {
        Flow = buildFlow(Read, MustWrite, MayWrite, nullptr, Schedule);
        RAW = isl_union_flow_get_may_dependence(Flow);
        isl_union_flow_free(Flow);

        Flow = buildFlow(Write, nullptr, Read, MustWrite, Schedule);
        WAR = isl_union_flow_get_may_dependence(Flow);
        isl_union_flow_free(Flow);
      } else {
        Flow = buildFlow(Read, nullptr, Write, nullptr, Schedule);
        RAW = isl_union_flow_get_may_dependence(Flow);
        isl_union_flow_free(Flow);

        Flow = buildFlow(Write, nullptr, Read, nullptr, Schedule);
        WAR = isl_union_flow_get_may_dependence(Flow);
        isl_union_flow_free(Flow);
      }
      Flow = buildFlow(Write, MustWrite, MayWrite, nullptr, Schedule);
      WAW = isl_union_flow_get_may_dependence(Flow);
      isl_union_flow_free(Flow);

      isl_union_map_free(Write);
    }

    isl_union_map_free(MustWrite);
    isl_union_map_free(MayWrite);
    isl_union_map_free(Read);
    isl_schedule_free(Schedule);

    if (isl_ctx_last_error(getIslCtx()) == isl_error_quota) {
      isl_union_map_free(RAW);
      isl_union_map_free(WAW);
      isl_union_map_free(WAR);
      isl_union_map_free(StrictWAW);
      RAW = WAW = WAR = StrictWAW = nullptr;
      isl_ctx_reset_error(getIslCtx());
    }

    if (hasValidDependences()) {
      RAW = isl_union_map_coalesce(RAW);
      WAW = isl_union_map_coalesce(WAW);
      WAR = isl_union_map_coalesce(WAR);
    }
  }

  if (!hasValidDependences()) {
    isl_union_set_free(TaggedStmtDomain);
    if (HasRedructions)
      isl_union_map_free(ReductionTagMap);
    RED = TC_RED = nullptr;
    return;
  }

  if (!HasRedructions && Level == AL_Statement) {
    RED = isl_union_map_empty(isl_union_map_get_space(RAW));
    TC_RED = isl_union_map_empty(isl_union_set_get_space(TaggedStmtDomain));
    isl_union_set_free(TaggedStmtDomain);
    isl_union_map_free(StrictWAW);
    return;
  }

  isl_union_map *STMT_RAW, *STMT_WAW, *STMT_WAR;
  STMT_RAW = isl_union_map_intersect_domain(
    isl_union_map_copy(RAW), isl_union_set_copy(TaggedStmtDomain));
  STMT_WAW = isl_union_map_intersect_domain(
    isl_union_map_copy(WAW), isl_union_set_copy(TaggedStmtDomain));
  STMT_WAR =
  isl_union_map_intersect_domain(isl_union_map_copy(WAR), TaggedStmtDomain);
  POLLY_DEBUG({
    dbgs() << "Wrapped Dependences:\n";
    dump();
    dbgs() << "\n";
  });

  RED = isl_union_map_empty(isl_union_map_get_space(RAW));
  if (HasRedructions) {
    for (ScopStmt &Stmt : S) {
      for (MemoryAccess *MA : Stmt) {
        if (!MA->isReductionLike())
          continue;
        isl_set *AccDomW = isl_map_wrap(MA->getAccessRelation().release());
        isl_map *Identity =
        isl_map_from_domain_and_range(isl_set_copy(AccDomW), AccDomW);
        RED = isl_union_map_add_map(RED, Identity);
      }
    }
    RED = isl_union_map_intersect(RED, isl_union_map_copy(RAW));
    RED = isl_union_map_intersect(RED, StrictWAW);
  }

  if (HasRedructions && !isl_union_map_is_empty(RED)) {
    isl_bool exact;
    isl_union_map *Current_TC_RED =
    isl_union_map_transitive_closure(isl_union_map_copy(RED), &exact);

    if (exact == isl_bool_false) {
      POLLY_DEBUG(dbgs() << "Transitive closure was not exact. "
      << "Proceeding with over-approximated privatization.\n");
    }

    RAW = isl_union_map_subtract(RAW, isl_union_map_copy(RED));
    WAW = isl_union_map_subtract(WAW, isl_union_map_copy(RED));
    WAR = isl_union_map_subtract(WAR, isl_union_map_copy(RED));
    addPrivatizationDependences(Current_TC_RED);
  } else {
    TC_RED = isl_union_map_empty(isl_union_map_get_space(RAW));
  }

  POLLY_DEBUG({
    dbgs() << "Final Wrapped Dependences:\n";
    dump();
    dbgs() << "\n";
  });

  if (TC_RED && !isl_union_map_is_empty(TC_RED)) {
    for (ScopStmt &Stmt : S) {
      for (MemoryAccess *MA : Stmt) {
        if (!MA->isReductionLike())
          continue;

        isl_set *AccDomW = isl_map_wrap(MA->getAccessRelation().release());
        isl_union_map *AccRedDepU = isl_union_map_intersect_domain(
          isl_union_map_copy(TC_RED), isl_union_set_from_set(AccDomW));
        if (isl_union_map_is_empty(AccRedDepU)) {
          isl_union_map_free(AccRedDepU);
          continue;
        }

        isl_map *AccRedDep = isl_map_from_union_map(AccRedDepU);
        AccRedDep = isl_map_zip(AccRedDep);
        AccRedDep = isl_set_unwrap(isl_map_domain(AccRedDep));
        setReductionDependences(MA, AccRedDep);
      }
    }
  }

  RAW = isl_union_map_zip(RAW);
  WAW = isl_union_map_zip(WAW);
  WAR = isl_union_map_zip(WAR);
  RED = isl_union_map_zip(RED);
  if (TC_RED)
    TC_RED = isl_union_map_zip(TC_RED);

  RAW = isl_union_set_unwrap(isl_union_map_domain(RAW));
  WAW = isl_union_set_unwrap(isl_union_map_domain(WAW));
  WAR = isl_union_set_unwrap(isl_union_map_domain(WAR));
  RED = isl_union_set_unwrap(isl_union_map_domain(RED));
  if (TC_RED)
    TC_RED = isl_union_set_unwrap(isl_union_map_domain(TC_RED));

  RAW = isl_union_map_union(RAW, STMT_RAW);
  WAW = isl_union_map_union(WAW, STMT_WAW);
  WAR = isl_union_map_union(WAR, STMT_WAR);

  RAW = isl_union_map_coalesce(RAW);
  WAW = isl_union_map_coalesce(WAW);
  WAR = isl_union_map_coalesce(WAR);
  RED = isl_union_map_coalesce(RED);
  if (TC_RED)
    TC_RED = isl_union_map_coalesce(TC_RED);

  POLLY_DEBUG(dump());
}

bool Dependences::isValidSchedule(Scop &S, isl::schedule NewSched) const {
  StatementToIslMapTy NewSchedules;
  for (auto NewMap : NewSched.get_map().get_map_list()) {
    auto Stmt = reinterpret_cast<ScopStmt *>(
        NewMap.get_tuple_id(isl::dim::in).get_user());
    NewSchedules[Stmt] = NewMap;
  }
  return isValidSchedule(S, NewSchedules);
}

bool Dependences::isValidSchedule(
    Scop &S, const StatementToIslMapTy &NewSchedule) const {
    if (LegalityCheckDisabled) {
        return true;
    }

    isl::union_map Dependences = getDependences(TYPE_RAW | TYPE_WAW | TYPE_WAR);
    if (Dependences.is_empty()) {
        return true;
    }

    isl::union_map Schedule = isl::union_map::empty(S.getIslCtx());
    isl::space TimeSpace;

    for (ScopStmt &Stmt : S) {
        isl::map StmtScat;
        auto Lookup = NewSchedule.find(&Stmt);
        if (Lookup == NewSchedule.end()) {
            StmtScat = Stmt.getSchedule();
        } else {
            StmtScat = Lookup->second;
        }
        assert(!StmtScat.is_null() &&
               "Schedules that contain extension nodes require special handling.");

        if (TimeSpace.is_null() && !StmtScat.is_empty()) {
            TimeSpace = StmtScat.get_space().range();
        }

        Schedule = Schedule.unite(StmtScat);
    }

    Dependences = Dependences.apply_domain(Schedule).apply_range(Schedule);
    if (Dependences.is_empty()) {
        return true;
    }

    if (TimeSpace.is_null()) {
        return true;
    }
    isl::map LexGT_Relation = isl::map::lex_gt(TimeSpace);

    for (isl::map DepMap : Dependences.get_map_list()) {
        if (DepMap.is_empty()) {
            continue;
        }

        isl::map Violations = DepMap.intersect(LexGT_Relation);

        if (!Violations.is_empty()) {
            return false;
        }
    }

    return true;
}

bool Dependences::isParallel(__isl_keep isl_union_map *Schedule,
                             __isl_take isl_union_map *Deps,
                             __isl_give isl_pw_aff **MinDistancePtr) const {
  isl_set *Deltas, *Distance;
  isl_map *ScheduleDeps;
  unsigned Dimension;
  bool IsParallel;

  Deps = isl_union_map_apply_range(Deps, isl_union_map_copy(Schedule));
  Deps = isl_union_map_apply_domain(Deps, isl_union_map_copy(Schedule));

  if (isl_union_map_is_empty(Deps)) {
    isl_union_map_free(Deps);
    return true;
  }

  ScheduleDeps = isl_map_from_union_map(Deps);
  Dimension = isl_map_dim(ScheduleDeps, isl_dim_out) - 1;

  for (unsigned i = 0; i < Dimension; i++)
    ScheduleDeps = isl_map_equate(ScheduleDeps, isl_dim_out, i, isl_dim_in, i);

  Deltas = isl_map_deltas(ScheduleDeps);
  Distance = isl_set_universe(isl_set_get_space(Deltas));

  // A loop-carried dependence at the current dimension has a distance vector
  // of the form [0, ..., 0, d] where d >= 1.
  for (unsigned i = 0; i < Dimension; i++)
    Distance = isl_set_fix_si(Distance, isl_dim_set, i, 0);

  Distance = isl_set_lower_bound_si(Distance, isl_dim_set, Dimension, 1);
  Distance = isl_set_intersect(Distance, Deltas);

  IsParallel = isl_set_is_empty(Distance);
  if (IsParallel || !MinDistancePtr) {
    isl_set_free(Distance);
    return IsParallel;
  }

  Distance = isl_set_project_out(Distance, isl_dim_set, 0, Dimension);
  Distance = isl_set_coalesce(Distance);

  *MinDistancePtr = isl_pw_aff_coalesce(isl_set_dim_min(Distance, 0));

  return false;
}

static void printDependencyMap(raw_ostream &OS, __isl_keep isl_union_map *DM) {
  if (DM)
    OS << DM << "\n";
  else
    OS << "n/a\n";
}

void Dependences::print(raw_ostream &OS) const {
  OS << "\tRAW dependences:\n\t\t";
  printDependencyMap(OS, RAW);
  OS << "\tWAR dependences:\n\t\t";
  printDependencyMap(OS, WAR);
  OS << "\tWAW dependences:\n\t\t";
  printDependencyMap(OS, WAW);
  OS << "\tReduction dependences:\n\t\t";
  printDependencyMap(OS, RED);
  OS << "\tTransitive closure of reduction dependences:\n\t\t";
  printDependencyMap(OS, TC_RED);
}

void Dependences::dump() const { print(dbgs()); }

void Dependences::releaseMemory() {
  isl_union_map_free(RAW);
  isl_union_map_free(WAR);
  isl_union_map_free(WAW);
  isl_union_map_free(RED);
  isl_union_map_free(TC_RED);

  RED = RAW = WAR = WAW = TC_RED = nullptr;

  for (auto &ReductionDeps : ReductionDependences)
    isl_map_free(ReductionDeps.second);
  ReductionDependences.clear();
}

isl::union_map Dependences::getDependences(int Kinds) const {
  assert(hasValidDependences() && "No valid dependences available");
  isl::space Space = isl::manage_copy(RAW).get_space();
  isl::union_map Deps = isl::union_map::empty(Space.ctx());

  if (Kinds & TYPE_RAW)
    Deps = Deps.unite(isl::manage_copy(RAW));

  if (Kinds & TYPE_WAR)
    Deps = Deps.unite(isl::manage_copy(WAR));

  if (Kinds & TYPE_WAW)
    Deps = Deps.unite(isl::manage_copy(WAW));

  if (Kinds & TYPE_RED)
    Deps = Deps.unite(isl::manage_copy(RED));

  if (Kinds & TYPE_TC_RED)
    Deps = Deps.unite(isl::manage_copy(TC_RED));

  Deps = Deps.coalesce();
  Deps = Deps.detect_equalities();
  return Deps;
}

bool Dependences::hasValidDependences() const {
  return (RAW != nullptr) && (WAR != nullptr) && (WAW != nullptr);
}

__isl_give isl_map *
Dependences::getReductionDependences(MemoryAccess *MA) const {
  return isl_map_copy(ReductionDependences.lookup(MA));
}

void Dependences::setReductionDependences(MemoryAccess *MA,
                                          __isl_take isl_map *D) {
  assert(ReductionDependences.count(MA) == 0 &&
         "Reduction dependences set twice!");
  ReductionDependences[MA] = D;
}

const Dependences &
DependenceAnalysis::Result::getDependences(Dependences::AnalysisLevel Level) {
  if (Dependences *d = D[Level].get())
    return *d;

  return recomputeDependences(Level);
}

const Dependences &
DependenceAnalysis::Result::recomputeDependences(
  Dependences::AnalysisLevel Level) {
  D[Level].reset(new Dependences(S.getSharedIslCtx(), Level));
  D[Level]->calculateDependences(S);
  return *D[Level];
}

void DependenceAnalysis::Result::abandonDependences() {
  for (std::unique_ptr<Dependences> &Deps : D)
    Deps.release();
}

DependenceAnalysis::Result
DependenceAnalysis::run(Scop &S, ScopAnalysisManager &SAM,
                        ScopStandardAnalysisResults &SAR) {
  return {S, {}};
}

AnalysisKey DependenceAnalysis::Key;

PreservedAnalyses
DependenceInfoPrinterPass::run(Scop &S, ScopAnalysisManager &SAM,
                               ScopStandardAnalysisResults &SAR,
                               SPMUpdater &U) {
  auto &DI = SAM.getResult<DependenceAnalysis>(S, SAR);

  if (auto d = DI.D[OptAnalysisLevel].get()) {
    d->print(OS);
    return PreservedAnalyses::all();
  }

  Dependences D(S.getSharedIslCtx(), OptAnalysisLevel);
  D.calculateDependences(S);
  D.print(OS);

  return PreservedAnalyses::all();
}

const Dependences &
DependenceInfo::getDependences(Dependences::AnalysisLevel Level) {
  if (Dependences *d = D[Level].get())
    return *d;

  return recomputeDependences(Level);
}

const Dependences &
DependenceInfo::recomputeDependences(Dependences::AnalysisLevel Level) {
  D[Level].reset(new Dependences(S->getSharedIslCtx(), Level));
  D[Level]->calculateDependences(*S);
  return *D[Level];
}

void DependenceInfo::abandonDependences() {
  for (std::unique_ptr<Dependences> &Deps : D)
    Deps.release();
}

bool DependenceInfo::runOnScop(Scop &ScopVar) {
  S = &ScopVar;
  return false;
}

void polly::DependenceInfo::printScop(raw_ostream &OS, Scop &S) const {
  if (auto d = D[OptAnalysisLevel].get()) {
    d->print(OS);
    return;
  }

  Dependences D(S.getSharedIslCtx(), OptAnalysisLevel);
  D.calculateDependences(S);
  D.print(OS);
}

void DependenceInfo::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequiredTransitive<ScopInfoRegionPass>();
  AU.setPreservesAll();
}

char DependenceInfo::ID = 0;

Pass *polly::createDependenceInfoPass() { return new DependenceInfo(); }

INITIALIZE_PASS_BEGIN(DependenceInfo, "polly-dependences",
                      "Polly - Calculate dependences", false, false);
INITIALIZE_PASS_DEPENDENCY(ScopInfoRegionPass);
INITIALIZE_PASS_END(DependenceInfo, "polly-dependences",
                    "Polly - Calculate dependences", false, false)

//===----------------------------------------------------------------------===//

namespace {
class DependenceInfoPrinterLegacyPass final : public ScopPass {
public:
  static char ID;
  DependenceInfoPrinterLegacyPass() : DependenceInfoPrinterLegacyPass(outs()) {}
  explicit DependenceInfoPrinterLegacyPass(llvm::raw_ostream &OS)
      : ScopPass(ID), OS(OS) {}

  bool runOnScop(Scop &S) override {
    DependenceInfo &P = getAnalysis<DependenceInfo>();
    OS << "Printing analysis '" << P.getPassName() << "' for "
       << "region: '" << S.getRegion().getNameStr() << "' in function '"
       << S.getFunction().getName() << "':\n";
    P.printScop(OS, S);
    return false;
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    ScopPass::getAnalysisUsage(AU);
    AU.addRequired<DependenceInfo>();
    AU.setPreservesAll();
  }

private:
  llvm::raw_ostream &OS;
};
char DependenceInfoPrinterLegacyPass::ID = 0;
} // namespace

Pass *polly::createDependenceInfoPrinterLegacyPass(raw_ostream &OS) {
  return new DependenceInfoPrinterLegacyPass(OS);
}

INITIALIZE_PASS_BEGIN(DependenceInfoPrinterLegacyPass,
                      "polly-print-dependences", "Polly - Print dependences",
                      false, false);
INITIALIZE_PASS_DEPENDENCY(DependenceInfo);
INITIALIZE_PASS_END(DependenceInfoPrinterLegacyPass, "polly-print-dependences",
                    "Polly - Print dependences", false, false)

//===----------------------------------------------------------------------===//

const Dependences &
DependenceInfoWrapperPass::getDependences(Scop *S,
                                          Dependences::AnalysisLevel Level) {
  auto It = ScopToDepsMap.find(S);
  if (It != ScopToDepsMap.end())
    if (It->second) {
      if (It->second->getDependenceLevel() == Level)
        return *It->second;
    }
  return recomputeDependences(S, Level);
}

const Dependences &DependenceInfoWrapperPass::recomputeDependences(
    Scop *S, Dependences::AnalysisLevel Level) {
  std::unique_ptr<Dependences> D(new Dependences(S->getSharedIslCtx(), Level));
  D->calculateDependences(*S);
  auto Inserted = ScopToDepsMap.insert(std::make_pair(S, std::move(D)));
  return *Inserted.first->second;
}

bool DependenceInfoWrapperPass::runOnFunction(Function &F) {
  auto &SI = *getAnalysis<ScopInfoWrapperPass>().getSI();
  for (auto &It : SI) {
    assert(It.second && "Invalid SCoP object!");
    recomputeDependences(It.second.get(), Dependences::AL_Access);
  }
  return false;
}

void DependenceInfoWrapperPass::print(raw_ostream &OS, const Module *M) const {
  for (auto &It : ScopToDepsMap) {
    assert((It.first && It.second) && "Invalid Scop or Dependence object!\n");
    It.second->print(OS);
  }
}

void DependenceInfoWrapperPass::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequiredTransitive<ScopInfoWrapperPass>();
  AU.setPreservesAll();
}

char DependenceInfoWrapperPass::ID = 0;

Pass *polly::createDependenceInfoWrapperPassPass() {
  return new DependenceInfoWrapperPass();
}

INITIALIZE_PASS_BEGIN(
    DependenceInfoWrapperPass, "polly-function-dependences",
    "Polly - Calculate dependences for all the SCoPs of a function", false,
    false)
INITIALIZE_PASS_DEPENDENCY(ScopInfoWrapperPass);
INITIALIZE_PASS_END(
    DependenceInfoWrapperPass, "polly-function-dependences",
    "Polly - Calculate dependences for all the SCoPs of a function", false,
    false)

//===----------------------------------------------------------------------===//

namespace {
class DependenceInfoPrinterLegacyFunctionPass final : public FunctionPass {
public:
  static char ID;
  DependenceInfoPrinterLegacyFunctionPass()
      : DependenceInfoPrinterLegacyFunctionPass(outs()) {}
  explicit DependenceInfoPrinterLegacyFunctionPass(llvm::raw_ostream &OS)
      : FunctionPass(ID), OS(OS) {}

  bool runOnFunction(Function &F) override {
    DependenceInfoWrapperPass &P = getAnalysis<DependenceInfoWrapperPass>();
    OS << "Printing analysis '" << P.getPassName() << "' for function '"
       << F.getName() << "':\n";
    P.print(OS);
    return false;
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    FunctionPass::getAnalysisUsage(AU);
    AU.addRequired<DependenceInfoWrapperPass>();
    AU.setPreservesAll();
  }

private:
  llvm::raw_ostream &OS;
};
char DependenceInfoPrinterLegacyFunctionPass::ID = 0;
} // namespace

Pass *polly::createDependenceInfoPrinterLegacyFunctionPass(raw_ostream &OS) {
  return new DependenceInfoPrinterLegacyFunctionPass(OS);
}

INITIALIZE_PASS_BEGIN(
    DependenceInfoPrinterLegacyFunctionPass, "polly-print-function-dependences",
    "Polly - Print dependences for all the SCoPs of a function", false, false);
INITIALIZE_PASS_DEPENDENCY(DependenceInfoWrapperPass);
INITIALIZE_PASS_END(DependenceInfoPrinterLegacyFunctionPass,
                    "polly-print-function-dependences",
                    "Polly - Print dependences for all the SCoPs of a function",
                    false, false)
