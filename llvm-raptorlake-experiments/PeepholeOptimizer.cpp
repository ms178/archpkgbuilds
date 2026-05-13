//===- PeepholeOptimizer.cpp - Peephole Optimizations ---------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Perform peephole optimizations on the machine code:
//
// - Optimize Extensions
//
//     Optimization of sign / zero extension instructions. It may be extended to
//     handle other instructions with similar properties.
//
//     On some targets, some instructions, e.g. X86 sign / zero extension, may
//     leave the source value in the lower part of the result. This optimization
//     will replace some uses of the pre-extension value with uses of the
//     sub-register of the results.
//
// - Optimize Comparisons
//
//     Optimization of comparison instructions. For instance, in this code:
//
//       sub r1, 1
//       cmp r1, 0
//       bz  L1
//
//     If the "sub" instruction already sets (or could be modified to set) the
//     same flag that the "cmp" instruction sets and that "bz" uses, then we can
//     eliminate the "cmp" instruction.
//
//     Another instance, in this code:
//
//       sub r1, r3 | sub r1, imm
//       cmp r3, r1 or cmp r1, r3 | cmp r1, imm
//       bge L1
//
//     If the branch instruction can use flag from "sub", then we can replace
//     "sub" with "subs" and eliminate the "cmp" instruction.
//
// - Optimize Loads:
//
//     Loads that can be folded into a later instruction. A load is foldable
//     if it loads to virtual registers and the virtual register defined has
//     a single use.
//
// - Optimize Copies and Bitcast (more generally, target specific copies):
//
//     Rewrite copies and bitcasts to avoid cross register bank copies
//     when possible.
//     E.g., Consider the following example, where capital and lower
//     letters denote different register file:
//     b = copy A <-- cross-bank copy
//     C = copy b <-- cross-bank copy
//   =>
//     b = copy A <-- cross-bank copy
//     C = copy A <-- same-bank copy
//
//     E.g., for bitcast:
//     b = bitcast A <-- cross-bank copy
//     C = bitcast b <-- cross-bank copy
//   =>
//     b = bitcast A <-- cross-bank copy
//     C = copy A    <-- same-bank copy
//===----------------------------------------------------------------------===//

#include "llvm/CodeGen/PeepholeOptimizer.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/MachinePassManager.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetOpcodes.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/InitializePasses.h"
#include "llvm/MC/LaneBitmask.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>
#include <cstdint>
#include <optional>
#include <utility>

using namespace llvm;
using RegSubRegPair = TargetInstrInfo::RegSubRegPair;
using RegSubRegPairAndIdx = TargetInstrInfo::RegSubRegPairAndIdx;

#define DEBUG_TYPE "peephole-opt"

// Optimize Extensions
static cl::opt<bool> Aggressive("aggressive-ext-opt", cl::Hidden,
                                cl::desc("Aggressive extension optimization"));

static cl::opt<bool>
    DisablePeephole("disable-peephole", cl::Hidden, cl::init(false),
                    cl::desc("Disable the peephole optimizer"));

/// Specify whether or not the value tracking looks through
/// complex instructions. When this is true, the value tracker
/// bails on everything that is not a copy or a bitcast.
static cl::opt<bool>
    DisableAdvCopyOpt("disable-adv-copy-opt", cl::Hidden, cl::init(false),
                      cl::desc("Disable advanced copy optimization"));

static cl::opt<bool> DisableNAPhysCopyOpt(
    "disable-non-allocatable-phys-copy-opt", cl::Hidden, cl::init(false),
    cl::desc("Disable non-allocatable physical register copy optimization"));

// Limit the number of PHI instructions to process
// in PeepholeOptimizer::getNextSource.
static cl::opt<unsigned>
    RewritePHILimit("rewrite-phi-limit", cl::Hidden, cl::init(10),
                    cl::desc("Limit the length of PHI chains to lookup"));

// Limit the length of recurrence chain when evaluating the benefit of
// commuting operands.
static cl::opt<unsigned> MaxRecurrenceChain(
    "recurrence-chain-limit", cl::Hidden, cl::init(3),
    cl::desc("Maximum length of recurrence chain when evaluating the benefit "
             "of commuting operands"));

STATISTIC(NumReuse, "Number of extension results reused");
STATISTIC(NumCmps, "Number of compares eliminated");
STATISTIC(NumImmFold, "Number of move immediate folded");
STATISTIC(NumLoadFold, "Number of loads folded");
STATISTIC(NumSelects, "Number of selects optimized");
STATISTIC(NumUncoalescableCopies, "Number of uncoalescable copies optimized");
STATISTIC(NumRewrittenCopies, "Number of copies rewritten");
STATISTIC(NumNAPhysCopies, "Number of non-allocatable physical copies removed");

namespace {

class ValueTrackerResult;
class RecurrenceInstr;

/// Interface to query instructions amenable to copy rewriting.
class Rewriter {
protected:
  MachineInstr &CopyLike;
  int CurrentSrcIdx = 0; ///< The index of the source being rewritten.

public:
  Rewriter(MachineInstr &CopyLike) : CopyLike(CopyLike) {}
  virtual ~Rewriter() = default;

  /// Get the next rewritable source (SrcReg, SrcSubReg) and
  /// the related value that it affects (DstReg, DstSubReg).
  /// A source is considered rewritable if its register class and the
  /// register class of the related DstReg may not be register
  /// coalescer friendly. In other words, given a copy-like instruction
  /// not all the arguments may be returned as rewritable source, since
  /// some arguments may already be register coalescer friendly.
  ///
  /// Each call of this method moves the current source to the next
  /// rewritable source.
  virtual bool getNextRewritableSource(RegSubRegPair &Src,
                                       RegSubRegPair &Dst) = 0;

  /// Rewrite the current source with \p NewReg and \p NewSubReg if possible.
  /// \return True if the rewriting was possible, false otherwise.
  virtual bool RewriteCurrentSource(Register NewReg, unsigned NewSubReg) = 0;
};

/// Rewriter for COPY instructions.
class CopyRewriter : public Rewriter {
public:
  CopyRewriter(MachineInstr &MI) : Rewriter(MI) {
    assert(MI.isCopy() && "Expected copy instruction");
  }
  ~CopyRewriter() override = default;

  bool getNextRewritableSource(RegSubRegPair &Src,
                               RegSubRegPair &Dst) override {
    if (++CurrentSrcIdx > 1)
      return false;

    const MachineOperand &MOSrc = CopyLike.getOperand(CurrentSrcIdx);
    Src = RegSubRegPair(MOSrc.getReg(), MOSrc.getSubReg());

    const MachineOperand &MODef = CopyLike.getOperand(0);
    Dst = RegSubRegPair(MODef.getReg(), MODef.getSubReg());
    return true;
  }

  bool RewriteCurrentSource(Register NewReg, unsigned NewSubReg) override {
    MachineOperand &MOSrc = CopyLike.getOperand(CurrentSrcIdx);
    MOSrc.setReg(NewReg);
    MOSrc.setSubReg(NewSubReg);
    return true;
  }
};

/// Helper class to rewrite uncoalescable copy-like instructions
/// into new COPY (coalescer-friendly) instructions.
class UncoalescableRewriter : public Rewriter {
  int NumDefs; ///< Number of defs in the bitcast-like instruction.

public:
  UncoalescableRewriter(MachineInstr &MI) : Rewriter(MI) {
    NumDefs = MI.getDesc().getNumDefs();
  }

  bool getNextRewritableSource(RegSubRegPair &Src,
                               RegSubRegPair &Dst) override {
    if (CurrentSrcIdx == NumDefs)
      return false;

    while (CopyLike.getOperand(CurrentSrcIdx).isDead()) {
      ++CurrentSrcIdx;
      if (CurrentSrcIdx == NumDefs)
        return false;
    }

    Src = RegSubRegPair(0, 0);
    const MachineOperand &MODef = CopyLike.getOperand(CurrentSrcIdx);
    Dst = RegSubRegPair(MODef.getReg(), MODef.getSubReg());

    ++CurrentSrcIdx;
    return true;
  }

  bool RewriteCurrentSource(Register NewReg, unsigned NewSubReg) override {
    (void)NewReg;
    (void)NewSubReg;
    return false;
  }
};

/// Specialized rewriter for INSERT_SUBREG instruction.
class InsertSubregRewriter : public Rewriter {
public:
  InsertSubregRewriter(MachineInstr &MI) : Rewriter(MI) {
    assert(MI.isInsertSubreg() && "Invalid instruction");
  }

  bool getNextRewritableSource(RegSubRegPair &Src,
                               RegSubRegPair &Dst) override {
    if (CurrentSrcIdx == 2)
      return false;

    CurrentSrcIdx = 2;
    const MachineOperand &MOInsertedReg = CopyLike.getOperand(2);
    Src = RegSubRegPair(MOInsertedReg.getReg(), MOInsertedReg.getSubReg());
    const MachineOperand &MODef = CopyLike.getOperand(0);

    if (MODef.getSubReg())
      return false;

    Dst = RegSubRegPair(MODef.getReg(),
                        static_cast<unsigned>(CopyLike.getOperand(3).getImm()));
    return true;
  }

  bool RewriteCurrentSource(Register NewReg, unsigned NewSubReg) override {
    if (CurrentSrcIdx != 2)
      return false;

    MachineOperand &MO = CopyLike.getOperand(CurrentSrcIdx);
    MO.setReg(NewReg);
    MO.setSubReg(NewSubReg);
    return true;
  }
};

/// Specialized rewriter for EXTRACT_SUBREG instruction.
class ExtractSubregRewriter : public Rewriter {
  const TargetInstrInfo &TII;

public:
  ExtractSubregRewriter(MachineInstr &MI, const TargetInstrInfo &TII)
      : Rewriter(MI), TII(TII) {
    assert(MI.isExtractSubreg() && "Invalid instruction");
  }

  bool getNextRewritableSource(RegSubRegPair &Src,
                               RegSubRegPair &Dst) override {
    if (CurrentSrcIdx == 1)
      return false;

    CurrentSrcIdx = 1;
    const MachineOperand &MOExtractedReg = CopyLike.getOperand(1);
    if (MOExtractedReg.getSubReg())
      return false;

    Src =
        RegSubRegPair(MOExtractedReg.getReg(), CopyLike.getOperand(2).getImm());

    const MachineOperand &MODef = CopyLike.getOperand(0);
    Dst = RegSubRegPair(MODef.getReg(), MODef.getSubReg());
    return true;
  }

  bool RewriteCurrentSource(Register NewReg, unsigned NewSubReg) override {
    if (CurrentSrcIdx != 1)
      return false;

    CopyLike.getOperand(CurrentSrcIdx).setReg(NewReg);

    if (!NewSubReg) {
      CurrentSrcIdx = -1;
      CopyLike.removeOperand(2);
      CopyLike.setDesc(TII.get(TargetOpcode::COPY));
      return true;
    }

    CopyLike.getOperand(CurrentSrcIdx + 1).setImm(NewSubReg);
    return true;
  }
};

/// Specialized rewriter for REG_SEQUENCE instruction.
class RegSequenceRewriter : public Rewriter {
public:
  RegSequenceRewriter(MachineInstr &MI) : Rewriter(MI) {
    assert(MI.isRegSequence() && "Invalid instruction");
    CurrentSrcIdx = -1;
  }

  bool getNextRewritableSource(RegSubRegPair &Src,
                               RegSubRegPair &Dst) override {
    CurrentSrcIdx += 2;
    if (static_cast<unsigned>(CurrentSrcIdx) >= CopyLike.getNumOperands())
      return false;

    const MachineOperand &MOInsertedReg = CopyLike.getOperand(CurrentSrcIdx);
    Src.Reg = MOInsertedReg.getReg();
    Src.SubReg = MOInsertedReg.getSubReg();

    Dst.SubReg = CopyLike.getOperand(CurrentSrcIdx + 1).getImm();

    const MachineOperand &MODef = CopyLike.getOperand(0);
    Dst.Reg = MODef.getReg();
    assert(MODef.getSubReg() == 0 && "cannot have subregister def in SSA");
    return true;
  }

  bool RewriteCurrentSource(Register NewReg, unsigned NewSubReg) override {
    MachineOperand &MO = CopyLike.getOperand(CurrentSrcIdx);
    MO.setReg(NewReg);
    MO.setSubReg(NewSubReg);
    return true;
  }
};

class PeepholeOptimizer : private MachineFunction::Delegate {
  const TargetInstrInfo *TII = nullptr;
  const TargetRegisterInfo *TRI = nullptr;
  MachineRegisterInfo *MRI = nullptr;
  MachineDominatorTree *DT = nullptr;
  MachineLoopInfo *MLI = nullptr;

public:
  PeepholeOptimizer(MachineDominatorTree *DT, MachineLoopInfo *MLI)
      : DT(DT), MLI(MLI) {}

  bool run(MachineFunction &MF);

  using RewriteMapTy = SmallDenseMap<RegSubRegPair, ValueTrackerResult>;
  using RecurrenceCycle = SmallVector<RecurrenceInstr, 4>;

private:
  bool optimizeCmpInstr(MachineInstr &MI);
  bool optimizeCmpInstr(MachineInstr &MI, Register SrcReg, Register SrcReg2,
                        int64_t CmpMask, int64_t CmpValue);

  bool optimizeExtInstr(MachineInstr &MI, MachineBasicBlock &MBB,
                        SmallPtrSetImpl<MachineInstr *> &LocalMIs);
  bool optimizeSelect(MachineInstr &MI,
                      SmallPtrSetImpl<MachineInstr *> &LocalMIs);
  bool optimizeCondBranch(MachineInstr &MI);

  bool optimizeCoalescableCopyImpl(Rewriter &&CpyRewriter);
  bool optimizeCoalescableCopy(MachineInstr &MI);
  bool optimizeUncoalescableCopy(MachineInstr &MI,
                                 SmallPtrSetImpl<MachineInstr *> &LocalMIs);
  bool optimizeRecurrence(MachineInstr &PHI);

  bool findNextSource(const TargetRegisterClass *DefRC, unsigned DefSubReg,
                      RegSubRegPair RegSubReg, RewriteMapTy &RewriteMap);

  bool isMoveImmediate(MachineInstr &MI, SmallSet<Register, 4> &ImmDefRegs,
                       DenseMap<Register, MachineInstr *> &ImmDefMIs);

  bool foldImmediate(MachineInstr &MI, SmallSet<Register, 4> &ImmDefRegs,
                     DenseMap<Register, MachineInstr *> &ImmDefMIs,
                     bool &Deleted);

  bool findTargetRecurrence(Register Reg,
                            const SmallSet<Register, 2> &TargetReg,
                            RecurrenceCycle &RC);

  bool foldRedundantCopy(MachineInstr &MI);
  bool isNAPhysCopy(Register Reg);
  bool foldRedundantNAPhysCopy(
      MachineInstr &MI, DenseMap<Register, MachineInstr *> &NAPhysToVirtMIs);

  bool isLoadFoldable(MachineInstr &MI,
                      SmallSet<Register, 16> &FoldAsLoadDefCandidates);

  /// Try to fold the load defined by \p FoldReg into \p MI using
  /// TII->optimizeLoadInstr. On success, updates \p LocalMIs, erases the old
  /// instructions, and returns the replacement; returns nullptr otherwise.
  MachineInstr *foldLoadInto(MachineFunction &MF, MachineInstr &MI,
                             Register FoldReg,
                             SmallPtrSet<MachineInstr *, 16> &LocalMIs);

  bool tryFoldLoadAfterCompareElim(Register CmpSrcReg,
                                   SmallPtrSetImpl<MachineInstr *> &LocalMIs);

  static bool isCoalescableCopy(const MachineInstr &MI) {
    return MI.isCopy() ||
           (!DisableAdvCopyOpt && (MI.isRegSequence() || MI.isInsertSubreg() ||
                                   MI.isExtractSubreg()));
  }

  static bool isUncoalescableCopy(const MachineInstr &MI) {
    return MI.isBitcast() || (!DisableAdvCopyOpt && (MI.isRegSequenceLike() ||
                                                     MI.isInsertSubregLike() ||
                                                     MI.isExtractSubregLike()));
  }

  MachineInstr &rewriteSource(MachineInstr &CopyLike, RegSubRegPair Def,
                              RewriteMapTy &RewriteMap);

  DenseMap<RegSubRegPair, MachineInstr *> CopySrcMIs;

  void MF_HandleInsertion(MachineInstr &MI) override { (void)MI; }

  bool getCopySrc(MachineInstr &MI, RegSubRegPair &SrcPair) {
    if (!MI.isCopy())
      return false;

    Register SrcReg = MI.getOperand(1).getReg();
    unsigned SrcSubReg = MI.getOperand(1).getSubReg();
    if (!SrcReg.isVirtual() && !MRI->isConstantPhysReg(SrcReg))
      return false;

    SrcPair = RegSubRegPair(SrcReg, SrcSubReg);
    return true;
  }

  void deleteChangedCopy(MachineInstr &MI) {
    RegSubRegPair SrcPair;
    if (!getCopySrc(MI, SrcPair))
      return;

    auto It = CopySrcMIs.find(SrcPair);
    if (It != CopySrcMIs.end() && It->second == &MI)
      CopySrcMIs.erase(It);
  }

  void MF_HandleRemoval(MachineInstr &MI) override { deleteChangedCopy(MI); }

  void MF_HandleChangeDesc(MachineInstr &MI, const MCInstrDesc &TID) override {
    (void)TID;
    deleteChangedCopy(MI);
  }
};

class PeepholeOptimizerLegacy : public MachineFunctionPass {
public:
  static char ID;

  PeepholeOptimizerLegacy() : MachineFunctionPass(ID) {}

  bool runOnMachineFunction(MachineFunction &MF) override;

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    MachineFunctionPass::getAnalysisUsage(AU);
    AU.addRequired<MachineLoopInfoWrapperPass>();
    AU.addPreserved<MachineLoopInfoWrapperPass>();
    if (Aggressive) {
      AU.addRequired<MachineDominatorTreeWrapperPass>();
      AU.addPreserved<MachineDominatorTreeWrapperPass>();
    }
  }

  MachineFunctionProperties getRequiredProperties() const override {
    return MachineFunctionProperties().setIsSSA();
  }
};

class RecurrenceInstr {
public:
  using IndexPair = std::pair<unsigned, unsigned>;

  RecurrenceInstr(MachineInstr *MI) : MI(MI) {}
  RecurrenceInstr(MachineInstr *MI, unsigned Idx1, unsigned Idx2)
      : MI(MI), CommutePair(std::make_pair(Idx1, Idx2)) {}

  MachineInstr *getMI() const { return MI; }
  std::optional<IndexPair> getCommutePair() const { return CommutePair; }

private:
  MachineInstr *MI;
  std::optional<IndexPair> CommutePair;
};

class ValueTrackerResult {
private:
  SmallVector<RegSubRegPair, 2> RegSrcs;
  const MachineInstr *Inst = nullptr;

public:
  ValueTrackerResult() = default;
  ValueTrackerResult(Register Reg, unsigned SubReg) { addSource(Reg, SubReg); }

  bool isValid() const { return getNumSources() > 0; }

  void setInst(const MachineInstr *I) { Inst = I; }
  const MachineInstr *getInst() const { return Inst; }

  void clear() {
    RegSrcs.clear();
    Inst = nullptr;
  }

  void addSource(Register SrcReg, unsigned SrcSubReg) {
    RegSrcs.push_back(RegSubRegPair(SrcReg, SrcSubReg));
  }

  void setSource(int Idx, Register SrcReg, unsigned SrcSubReg) {
    assert(Idx < getNumSources() && "Reg pair source out of index");
    RegSrcs[Idx] = RegSubRegPair(SrcReg, SrcSubReg);
  }

  int getNumSources() const { return RegSrcs.size(); }

  RegSubRegPair getSrc(int Idx) const { return RegSrcs[Idx]; }

  Register getSrcReg(int Idx) const {
    assert(Idx < getNumSources() && "Reg source out of index");
    return RegSrcs[Idx].Reg;
  }

  unsigned getSrcSubReg(int Idx) const {
    assert(Idx < getNumSources() && "SubReg source out of index");
    return RegSrcs[Idx].SubReg;
  }

  bool operator==(const ValueTrackerResult &Other) const {
    if (Other.getInst() != getInst())
      return false;
    if (Other.getNumSources() != getNumSources())
      return false;

    for (int i = 0, e = Other.getNumSources(); i != e; ++i)
      if (Other.getSrcReg(i) != getSrcReg(i) ||
          Other.getSrcSubReg(i) != getSrcSubReg(i))
        return false;
    return true;
  }
};

class ValueTracker {
private:
  const MachineInstr *Def = nullptr;
  unsigned DefIdx = 0;
  unsigned DefSubReg;
  Register Reg;
  const MachineRegisterInfo &MRI;
  const TargetInstrInfo *TII;

  ValueTrackerResult getNextSourceImpl();
  ValueTrackerResult getNextSourceFromCopy();
  ValueTrackerResult getNextSourceFromBitcast();
  ValueTrackerResult getNextSourceFromRegSequence();
  ValueTrackerResult getNextSourceFromInsertSubreg();
  ValueTrackerResult getNextSourceFromExtractSubreg();
  ValueTrackerResult getNextSourceFromSubregToReg();
  ValueTrackerResult getNextSourceFromPHI();

public:
  ValueTracker(Register Reg, unsigned DefSubReg, const MachineRegisterInfo &MRI,
               const TargetInstrInfo *TII = nullptr)
      : DefSubReg(DefSubReg), Reg(Reg), MRI(MRI), TII(TII) {
    if (!Reg.isPhysical()) {
      Def = MRI.getVRegDef(Reg);
      DefIdx = MRI.def_begin(Reg).getOperandNo();
    }
  }

  ValueTrackerResult getNextSource();
};

} // end anonymous namespace

char PeepholeOptimizerLegacy::ID = 0;
char &llvm::PeepholeOptimizerLegacyID = PeepholeOptimizerLegacy::ID;

INITIALIZE_PASS_BEGIN(PeepholeOptimizerLegacy, DEBUG_TYPE,
                      "Peephole Optimizations", false, false)
INITIALIZE_PASS_DEPENDENCY(MachineDominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(MachineLoopInfoWrapperPass)
INITIALIZE_PASS_END(PeepholeOptimizerLegacy, DEBUG_TYPE,
                    "Peephole Optimizations", false, false)

bool PeepholeOptimizer::optimizeExtInstr(
    MachineInstr &MI, MachineBasicBlock &MBB,
    SmallPtrSetImpl<MachineInstr *> &LocalMIs) {
  Register SrcReg, DstReg;
  unsigned SubIdx;
  if (!TII->isCoalescableExtInstr(MI, SrcReg, DstReg, SubIdx))
    return false;

  if (DstReg.isPhysical() || SrcReg.isPhysical())
    return false;

  if (MRI->hasOneNonDBGUse(SrcReg))
    return false;

  const TargetRegisterClass *DstRC = MRI->getRegClass(DstReg);
  DstRC = TRI->getSubClassWithSubReg(DstRC, SubIdx);
  if (!DstRC)
    return false;

  bool UseSrcSubIdx =
      TRI->getSubClassWithSubReg(MRI->getRegClass(SrcReg), SubIdx) != nullptr;

  SmallPtrSet<MachineBasicBlock *, 4> ReachedBBs;
  for (MachineInstr &UI : MRI->use_nodbg_instructions(DstReg))
    ReachedBBs.insert(UI.getParent());

  SmallVector<MachineOperand *, 8> Uses;
  SmallVector<MachineOperand *, 8> ExtendedUses;

  bool ExtendLife = true;
  for (MachineOperand &UseMO : MRI->use_nodbg_operands(SrcReg)) {
    MachineInstr *UseMI = UseMO.getParent();
    if (UseMI == &MI)
      continue;

    if (UseMI->isPHI()) {
      ExtendLife = false;
      continue;
    }

    if (UseSrcSubIdx && UseMO.getSubReg() != SubIdx)
      continue;

    if (UseMI->getOpcode() == TargetOpcode::SUBREG_TO_REG)
      continue;

    MachineBasicBlock *UseMBB = UseMI->getParent();
    if (UseMBB == &MBB) {
      if (!LocalMIs.count(UseMI))
        Uses.push_back(&UseMO);
    } else if (ReachedBBs.count(UseMBB)) {
      Uses.push_back(&UseMO);
    } else if (Aggressive && DT->dominates(&MBB, UseMBB)) {
      ExtendedUses.push_back(&UseMO);
    } else {
      ExtendLife = false;
      break;
    }
  }

  if (ExtendLife && !ExtendedUses.empty())
    Uses.append(ExtendedUses.begin(), ExtendedUses.end());

  bool Changed = false;
  if (!Uses.empty()) {
    SmallPtrSet<MachineBasicBlock *, 4> PHIBBs;
    for (MachineInstr &UI : MRI->use_nodbg_instructions(DstReg))
      if (UI.isPHI())
        PHIBBs.insert(UI.getParent());

    const TargetRegisterClass *RC = MRI->getRegClass(SrcReg);
    for (MachineOperand *UseMO : Uses) {
      MachineInstr *UseMI = UseMO->getParent();
      MachineBasicBlock *UseMBB = UseMI->getParent();
      if (PHIBBs.count(UseMBB))
        continue;

      if (!Changed) {
        MRI->clearKillFlags(DstReg);
        MRI->constrainRegClass(DstReg, DstRC);
      }

      if (UseSrcSubIdx)
        RC = MRI->getRegClass(UseMI->getOperand(0).getReg());

      Register NewVR = MRI->createVirtualRegister(RC);
      BuildMI(*UseMBB, UseMI, UseMI->getDebugLoc(),
              TII->get(TargetOpcode::COPY), NewVR)
          .addReg(DstReg, {}, SubIdx);

      if (UseSrcSubIdx)
        UseMO->setSubReg(0);

      UseMO->setReg(NewVR);
      ++NumReuse;
      Changed = true;
    }
  }

  return Changed;
}

bool PeepholeOptimizer::optimizeCmpInstr(MachineInstr &MI) {
  Register SrcReg, SrcReg2;
  int64_t CmpMask = 0;
  int64_t CmpValue = 0;
  if (!TII->analyzeCompare(MI, SrcReg, SrcReg2, CmpMask, CmpValue))
    return false;
  return optimizeCmpInstr(MI, SrcReg, SrcReg2, CmpMask, CmpValue);
}

bool PeepholeOptimizer::optimizeCmpInstr(MachineInstr &MI, Register SrcReg,
                                         Register SrcReg2, int64_t CmpMask,
                                         int64_t CmpValue) {
  if (SrcReg.isPhysical() || SrcReg2.isPhysical())
    return false;

  LLVM_DEBUG(dbgs() << "Attempting to optimize compare: " << MI);
  if (TII->optimizeCompareInstr(MI, SrcReg, SrcReg2, CmpMask, CmpValue, MRI)) {
    LLVM_DEBUG(dbgs() << "  -> Successfully optimized compare!\n");
    ++NumCmps;
    return true;
  }

  return false;
}

bool PeepholeOptimizer::optimizeSelect(
    MachineInstr &MI, SmallPtrSetImpl<MachineInstr *> &LocalMIs) {
  assert(MI.isSelect() && "Should only be called when MI->isSelect() is true");
  if (!TII->optimizeSelect(MI, LocalMIs))
    return false;
  LLVM_DEBUG(dbgs() << "Deleting select: " << MI);
  MI.eraseFromParent();
  ++NumSelects;
  return true;
}

bool PeepholeOptimizer::optimizeCondBranch(MachineInstr &MI) {
  return TII->optimizeCondBranch(MI);
}

bool PeepholeOptimizer::findNextSource(const TargetRegisterClass *DefRC,
                                       unsigned DefSubReg,
                                       RegSubRegPair RegSubReg,
                                       RewriteMapTy &RewriteMap) {
  Register Reg = RegSubReg.Reg;
  RegSubRegPair CurSrcPair = RegSubReg;
  SmallVector<RegSubRegPair, 4> SrcToLook = {CurSrcPair};

  unsigned PHICount = 0;
  do {
    CurSrcPair = SrcToLook.pop_back_val();
    if (CurSrcPair.Reg.isPhysical())
      return false;

    ValueTracker ValTracker(CurSrcPair.Reg, CurSrcPair.SubReg, *MRI, TII);

    while (true) {
      ValueTrackerResult Res = ValTracker.getNextSource();
      if (!Res.isValid())
        return false;

      auto [InsertPt, WasInserted] = RewriteMap.try_emplace(CurSrcPair, Res);
      if (!WasInserted) {
        const ValueTrackerResult &CurSrcRes = InsertPt->second;
        assert(CurSrcRes == Res && "ValueTrackerResult found must match");
        if (CurSrcRes.getNumSources() > 1) {
          LLVM_DEBUG(dbgs()
                     << "findNextSource: found PHI cycle, aborting...\n");
          return false;
        }
        break;
      }

      unsigned NumSrcs = Res.getNumSources();
      if (NumSrcs > 1) {
        ++PHICount;
        if (PHICount >= RewritePHILimit) {
          LLVM_DEBUG(dbgs() << "findNextSource: PHI limit reached\n");
          return false;
        }

        for (unsigned i = 0; i < NumSrcs; ++i)
          SrcToLook.push_back(Res.getSrc(i));
        break;
      }

      CurSrcPair = Res.getSrc(0);
      if (CurSrcPair.Reg.isPhysical())
        return false;

      const TargetRegisterClass *SrcRC = MRI->getRegClass(CurSrcPair.Reg);
      if (!TRI->shouldRewriteCopySrc(DefRC, DefSubReg, SrcRC,
                                     CurSrcPair.SubReg))
        continue;

      if (PHICount > 0 && CurSrcPair.SubReg != 0)
        continue;

      break;
    }
  } while (!SrcToLook.empty());

  return CurSrcPair.Reg != Reg;
}

static MachineInstr &insertPHI(MachineRegisterInfo &MRI,
                               const TargetInstrInfo &TII,
                               const SmallVectorImpl<RegSubRegPair> &SrcRegs,
                               MachineInstr &OrigPHI) {
  assert(!SrcRegs.empty() && "No sources to create a PHI instruction?");

  const TargetRegisterClass *NewRC = MRI.getRegClass(SrcRegs[0].Reg);
  assert(SrcRegs[0].SubReg == 0 && "should not have subreg operand");
  Register NewVR = MRI.createVirtualRegister(NewRC);
  MachineBasicBlock *MBB = OrigPHI.getParent();
  MachineInstrBuilder MIB = BuildMI(*MBB, &OrigPHI, OrigPHI.getDebugLoc(),
                                    TII.get(TargetOpcode::PHI), NewVR);

  unsigned MBBOpIdx = 2;
  for (const RegSubRegPair &RegPair : SrcRegs) {
    MIB.addReg(RegPair.Reg, {}, RegPair.SubReg);
    MIB.addMBB(OrigPHI.getOperand(MBBOpIdx).getMBB());
    MRI.clearKillFlags(RegPair.Reg);
    MBBOpIdx += 2;
  }

  return *MIB;
}

static RegSubRegPair
getNewSource(MachineRegisterInfo *MRI, const TargetInstrInfo *TII,
             RegSubRegPair Def,
             const PeepholeOptimizer::RewriteMapTy &RewriteMap,
             bool HandleMultipleSources = true) {
  RegSubRegPair LookupSrc(Def.Reg, Def.SubReg);
  while (true) {
    ValueTrackerResult Res = RewriteMap.lookup(LookupSrc);
    if (!Res.isValid())
      return LookupSrc;

    unsigned NumSrcs = Res.getNumSources();
    if (NumSrcs == 1) {
      LookupSrc.Reg = Res.getSrcReg(0);
      LookupSrc.SubReg = Res.getSrcSubReg(0);
      continue;
    }

    if (!HandleMultipleSources)
      break;

    SmallVector<RegSubRegPair, 4> NewPHISrcs;
    for (unsigned i = 0; i < NumSrcs; ++i) {
      RegSubRegPair PHISrc(Res.getSrcReg(i), Res.getSrcSubReg(i));
      NewPHISrcs.push_back(
          getNewSource(MRI, TII, PHISrc, RewriteMap, HandleMultipleSources));
    }

    MachineInstr &OrigPHI = const_cast<MachineInstr &>(*Res.getInst());
    MachineInstr &NewPHI = insertPHI(*MRI, *TII, NewPHISrcs, OrigPHI);
    LLVM_DEBUG(dbgs() << "-- getNewSource\n");
    LLVM_DEBUG(dbgs() << "   Replacing: " << OrigPHI);
    LLVM_DEBUG(dbgs() << "        With: " << NewPHI);
    const MachineOperand &MODef = NewPHI.getOperand(0);
    return RegSubRegPair(MODef.getReg(), MODef.getSubReg());
  }

  return RegSubRegPair(0, 0);
}

bool PeepholeOptimizer::optimizeCoalescableCopyImpl(Rewriter &&CpyRewriter) {
  bool Changed = false;
  RegSubRegPair Dst;
  RegSubRegPair TrackPair;
  while (CpyRewriter.getNextRewritableSource(TrackPair, Dst)) {
    if (Dst.Reg.isPhysical())
      continue;

    const TargetRegisterClass *DefRC = MRI->getRegClass(Dst.Reg);

    RewriteMapTy RewriteMap;
    if (!findNextSource(DefRC, Dst.SubReg, TrackPair, RewriteMap))
      continue;

    RegSubRegPair NewSrc = getNewSource(MRI, TII, TrackPair, RewriteMap,
                                        /*HandleMultipleSources=*/false);
    assert(TrackPair.Reg != NewSrc.Reg &&
           "should not rewrite source to original value");
    if (!NewSrc.Reg)
      continue;

    if (NewSrc.SubReg) {
      const TargetRegisterClass *RC = MRI->getRegClass(NewSrc.Reg);
      const TargetRegisterClass *WithSubRC =
          TRI->getSubClassWithSubReg(RC, NewSrc.SubReg);
      if (!WithSubRC || !MRI->constrainRegClass(NewSrc.Reg, WithSubRC))
        continue;
      Changed = true;
    }

    if (CpyRewriter.RewriteCurrentSource(NewSrc.Reg, NewSrc.SubReg)) {
      MRI->clearKillFlags(NewSrc.Reg);
      Changed = true;
    }
  }

  NumRewrittenCopies += Changed;
  return Changed;
}

bool PeepholeOptimizer::optimizeCoalescableCopy(MachineInstr &MI) {
  assert(isCoalescableCopy(MI) && "Invalid argument");
  assert(MI.getDesc().getNumDefs() == 1 &&
         "Coalescer can understand multiple defs?!");
  const MachineOperand &MODef = MI.getOperand(0);
  if (MODef.getReg().isPhysical())
    return false;

  switch (MI.getOpcode()) {
  case TargetOpcode::COPY:
    return optimizeCoalescableCopyImpl(CopyRewriter(MI));
  case TargetOpcode::INSERT_SUBREG:
    return optimizeCoalescableCopyImpl(InsertSubregRewriter(MI));
  case TargetOpcode::EXTRACT_SUBREG:
    return optimizeCoalescableCopyImpl(ExtractSubregRewriter(MI, *TII));
  case TargetOpcode::REG_SEQUENCE:
    return optimizeCoalescableCopyImpl(RegSequenceRewriter(MI));
  default:
    if (MI.isBitcast() || MI.isRegSequenceLike() || MI.isInsertSubregLike() ||
        MI.isExtractSubregLike())
      return optimizeCoalescableCopyImpl(UncoalescableRewriter(MI));
    return false;
  }
}

MachineInstr &PeepholeOptimizer::rewriteSource(MachineInstr &CopyLike,
                                               RegSubRegPair Def,
                                               RewriteMapTy &RewriteMap) {
  assert(!Def.Reg.isPhysical() && "We do not rewrite physical registers");

  RegSubRegPair NewSrc = getNewSource(MRI, TII, Def, RewriteMap);

  const TargetRegisterClass *DefRC = MRI->getRegClass(Def.Reg);
  Register NewVReg = MRI->createVirtualRegister(DefRC);

  if (NewSrc.SubReg) {
    const TargetRegisterClass *NewSrcRC = MRI->getRegClass(NewSrc.Reg);
    const TargetRegisterClass *WithSubRC =
        TRI->getSubClassWithSubReg(NewSrcRC, NewSrc.SubReg);
    if (!WithSubRC || !MRI->constrainRegClass(NewSrc.Reg, WithSubRC))
      llvm_unreachable("replacement register cannot support subregister");
  }

  MachineInstr *NewCopy =
      BuildMI(*CopyLike.getParent(), &CopyLike, CopyLike.getDebugLoc(),
              TII->get(TargetOpcode::COPY), NewVReg)
          .addReg(NewSrc.Reg, {}, NewSrc.SubReg);

  if (Def.SubReg) {
    NewCopy->getOperand(0).setSubReg(Def.SubReg);
    NewCopy->getOperand(0).setIsUndef();
  }

  LLVM_DEBUG(dbgs() << "-- RewriteSource\n");
  LLVM_DEBUG(dbgs() << "   Replacing: " << CopyLike);
  LLVM_DEBUG(dbgs() << "        With: " << *NewCopy);

  MRI->replaceRegWith(Def.Reg, NewVReg);
  MRI->clearKillFlags(NewVReg);
  MRI->clearKillFlags(NewSrc.Reg);

  return *NewCopy;
}

bool PeepholeOptimizer::optimizeUncoalescableCopy(
    MachineInstr &MI, SmallPtrSetImpl<MachineInstr *> &LocalMIs) {
  assert(isUncoalescableCopy(MI) && "Invalid argument");
  UncoalescableRewriter CpyRewriter(MI);

  RewriteMapTy RewriteMap;
  RegSubRegPair Src;
  RegSubRegPair Def;
  SmallVector<RegSubRegPair, 4> RewritePairs;
  while (CpyRewriter.getNextRewritableSource(Src, Def)) {
    (void)Src;
    if (Def.Reg.isPhysical())
      return false;

    const TargetRegisterClass *DefRC = MRI->getRegClass(Def.Reg);
    if (!findNextSource(DefRC, Def.SubReg, Def, RewriteMap))
      return false;

    RewritePairs.push_back(Def);
  }

  for (const RegSubRegPair &DefPair : RewritePairs) {
    MachineInstr &NewCopy = rewriteSource(MI, DefPair, RewriteMap);
    LocalMIs.insert(&NewCopy);
  }

  LLVM_DEBUG(dbgs() << "Deleting uncoalescable copy: " << MI);
  MI.eraseFromParent();
  ++NumUncoalescableCopies;
  return true;
}

bool PeepholeOptimizer::isLoadFoldable(
    MachineInstr &MI, SmallSet<Register, 16> &FoldAsLoadDefCandidates) {
  if (!MI.canFoldAsLoad() || !MI.mayLoad())
    return false;

  const MCInstrDesc &MCID = MI.getDesc();
  if (MCID.getNumDefs() != 1)
    return false;

  Register Reg = MI.getOperand(0).getReg();
  if (Reg.isVirtual() && !MI.getOperand(0).getSubReg() &&
      MRI->hasOneNonDBGUser(Reg)) {
    FoldAsLoadDefCandidates.insert(Reg);
    return true;
  }

  return false;
}

MachineInstr *
PeepholeOptimizer::foldLoadInto(MachineFunction &MF, MachineInstr &MI,
                                Register FoldReg,
                                SmallPtrSetImpl<MachineInstr *> &LocalMIs) {
  Register Reg = FoldReg;
  MachineInstr *DefMI = nullptr;
  MachineInstr *CopyMI = nullptr;

  MachineInstr *FoldMI = TII->optimizeLoadInstr(MI, MRI, Reg, DefMI, CopyMI);
  if (!FoldMI)
    return nullptr;

  LLVM_DEBUG(dbgs() << "Replacing: " << MI << "     With: " << *FoldMI);
  LocalMIs.erase(&MI);
  LocalMIs.erase(DefMI);
  LocalMIs.insert(FoldMI);
  if (CopyMI)
    LocalMIs.insert(CopyMI);

  if (MI.shouldUpdateAdditionalCallInfo())
    MF.moveAdditionalCallInfo(&MI, FoldMI);
  MI.eraseFromParent();
  DefMI->eraseFromParent();

  MRI->markUsesInDebugValueAsUndef(FoldReg);
  ++NumLoadFold;
  return FoldMI;
}

bool PeepholeOptimizer::tryFoldLoadAfterCompareElim(
    Register CmpSrcReg, SmallPtrSetImpl<MachineInstr *> &LocalMIs) {
  if (!CmpSrcReg.isVirtual() || !MRI->hasOneNonDBGUser(CmpSrcReg))
    return false;

  auto UI = MRI->use_nodbg_begin(CmpSrcReg);
  if (UI == MRI->use_nodbg_end())
    return false;

  MachineInstr *FlagProducer = UI->getParent();
  MachineInstr *LoadMI = MRI->getVRegDef(CmpSrcReg);
  if (!FlagProducer || !LoadMI || FlagProducer == LoadMI)
    return false;

  if (!LocalMIs.count(FlagProducer) || !LocalMIs.count(LoadMI))
    return false;

  if (!LoadMI->canFoldAsLoad() || !LoadMI->mayLoad())
    return false;

  return foldLoadInto(*FlagProducer->getMF(), *FlagProducer, CmpSrcReg,
                      LocalMIs) != nullptr;
}

bool PeepholeOptimizer::isMoveImmediate(
    MachineInstr &MI, SmallSet<Register, 4> &ImmDefRegs,
    DenseMap<Register, MachineInstr *> &ImmDefMIs) {
  const MCInstrDesc &MCID = MI.getDesc();
  if (MCID.getNumDefs() != 1 || !MI.getOperand(0).isReg())
    return false;

  Register Reg = MI.getOperand(0).getReg();
  if (!Reg.isVirtual())
    return false;

  int64_t ImmVal;
  if (!MI.isMoveImmediate() && !TII->getConstValDefinedInReg(MI, Reg, ImmVal))
    return false;

  ImmDefMIs.insert(std::make_pair(Reg, &MI));
  ImmDefRegs.insert(Reg);
  return true;
}

bool PeepholeOptimizer::foldImmediate(
    MachineInstr &MI, SmallSet<Register, 4> &ImmDefRegs,
    DenseMap<Register, MachineInstr *> &ImmDefMIs, bool &Deleted) {
  Deleted = false;
  for (unsigned i = 0, e = MI.getDesc().getNumOperands(); i != e; ++i) {
    MachineOperand &MO = MI.getOperand(i);
    if (!MO.isReg() || MO.isDef())
      continue;

    Register Reg = MO.getReg();
    if (!Reg.isVirtual())
      continue;

    if (ImmDefRegs.count(Reg) == 0)
      continue;

    auto II = ImmDefMIs.find(Reg);
    assert(II != ImmDefMIs.end() && "couldn't find immediate definition");
    if (TII->foldImmediate(MI, *II->second, Reg, MRI)) {
      ++NumImmFold;

      if (MRI->getVRegDef(Reg) &&
          MI.isIdenticalTo(*II->second, MachineInstr::IgnoreVRegDefs)) {
        Register DstReg = MI.getOperand(0).getReg();
        if (DstReg.isVirtual() &&
            MRI->getRegClass(DstReg) == MRI->getRegClass(Reg)) {
          MRI->replaceRegWith(DstReg, Reg);
          MRI->clearKillFlags(Reg);
          MI.eraseFromParent();
          Deleted = true;
        }
      }
      return true;
    }
  }

  return false;
}

bool PeepholeOptimizer::foldRedundantCopy(MachineInstr &MI) {
  assert(MI.isCopy() && "expected a COPY machine instruction");

  RegSubRegPair SrcPair;
  if (!getCopySrc(MI, SrcPair))
    return false;

  Register DstReg = MI.getOperand(0).getReg();
  if (!DstReg.isVirtual())
    return false;

  if (CopySrcMIs.insert(std::make_pair(SrcPair, &MI)).second)
    return false;

  MachineInstr *PrevCopy = CopySrcMIs.find(SrcPair)->second;

  assert(SrcPair.SubReg == PrevCopy->getOperand(1).getSubReg() &&
         "Unexpected mismatching subreg!");

  Register PrevDstReg = PrevCopy->getOperand(0).getReg();
  if (MRI->getRegClass(DstReg) != MRI->getRegClass(PrevDstReg))
    return false;

  MRI->replaceRegWith(DstReg, PrevDstReg);
  MRI->clearKillFlags(PrevDstReg);
  return true;
}

bool PeepholeOptimizer::isNAPhysCopy(Register Reg) {
  return Reg.isPhysical() && !MRI->isAllocatable(Reg);
}

bool PeepholeOptimizer::foldRedundantNAPhysCopy(
    MachineInstr &MI, DenseMap<Register, MachineInstr *> &NAPhysToVirtMIs) {
  assert(MI.isCopy() && "expected a COPY machine instruction");

  if (DisableNAPhysCopyOpt)
    return false;

  Register DstReg = MI.getOperand(0).getReg();
  Register SrcReg = MI.getOperand(1).getReg();

  if (isNAPhysCopy(SrcReg) && DstReg.isVirtual()) {
    NAPhysToVirtMIs.insert_or_assign(SrcReg, &MI);
    return false;
  }

  if (!(SrcReg.isVirtual() && isNAPhysCopy(DstReg)))
    return false;

  auto PrevCopy = NAPhysToVirtMIs.find(DstReg);
  if (PrevCopy == NAPhysToVirtMIs.end()) {
    LLVM_DEBUG(dbgs() << "NAPhysCopy: intervening clobber forbids erasing "
                      << MI);
    return false;
  }

  Register PrevDstReg = PrevCopy->second->getOperand(0).getReg();
  if (PrevDstReg == SrcReg) {
    LLVM_DEBUG(dbgs() << "NAPhysCopy: erasing " << MI);
    ++NumNAPhysCopies;
    return true;
  }

  LLVM_DEBUG(dbgs() << "NAPhysCopy: missed opportunity " << MI);
  NAPhysToVirtMIs.erase(PrevCopy);
  return false;
}

/// \brief Returns true if \p MO is a virtual register operand.
static bool isVirtualRegisterOperand(const MachineOperand &MO) {
  return MO.isReg() && MO.getReg().isVirtual();
}

bool PeepholeOptimizer::findTargetRecurrence(
    Register Reg, const SmallSet<Register, 2> &TargetRegs,
    RecurrenceCycle &RC) {
  if (TargetRegs.count(Reg))
    return true;

  if (!MRI->hasOneNonDBGUse(Reg))
    return false;

  if (RC.size() >= MaxRecurrenceChain)
    return false;

  MachineInstr &MI = *(MRI->use_instr_nodbg_begin(Reg));
  unsigned Idx = MI.findRegisterUseOperandIdx(Reg, /*TRI=*/nullptr);
  if (Idx == static_cast<unsigned>(-1))
    return false;

  if (MI.getDesc().getNumDefs() != 1)
    return false;

  MachineOperand &DefOp = MI.getOperand(0);
  if (!isVirtualRegisterOperand(DefOp))
    return false;

  unsigned TiedUseIdx;
  if (!MI.isRegTiedToUseOperand(0, &TiedUseIdx))
    return false;

  if (Idx == TiedUseIdx) {
    RC.push_back(RecurrenceInstr(&MI));
    return findTargetRecurrence(DefOp.getReg(), TargetRegs, RC);
  }

  unsigned CommIdx = TargetInstrInfo::CommuteAnyOperandIndex;
  if (TII->findCommutedOpIndices(MI, Idx, CommIdx) && CommIdx == TiedUseIdx) {
    RC.push_back(RecurrenceInstr(&MI, Idx, CommIdx));
    return findTargetRecurrence(DefOp.getReg(), TargetRegs, RC);
  }

  return false;
}

bool PeepholeOptimizer::optimizeRecurrence(MachineInstr &PHI) {
  SmallSet<Register, 2> TargetRegs;
  for (unsigned Idx = 1; Idx < PHI.getNumOperands(); Idx += 2) {
    const MachineOperand &MO = PHI.getOperand(Idx);
    assert(isVirtualRegisterOperand(MO) && "Invalid PHI instruction");
    TargetRegs.insert(MO.getReg());
  }

  bool Changed = false;
  RecurrenceCycle RC;
  if (findTargetRecurrence(PHI.getOperand(0).getReg(), TargetRegs, RC)) {
    LLVM_DEBUG(dbgs() << "Optimize recurrence chain from " << PHI);
    for (auto &RI : RC) {
      LLVM_DEBUG(dbgs() << "\tInst: " << *(RI.getMI()));
      auto CP = RI.getCommutePair();
      if (CP) {
        Changed = true;
        TII->commuteInstruction(*(RI.getMI()), false, (*CP).first,
                                (*CP).second);
        LLVM_DEBUG(dbgs() << "\t\tCommuted: " << *(RI.getMI()));
      }
    }
  }

  return Changed;
}

PreservedAnalyses
PeepholeOptimizerPass::run(MachineFunction &MF,
                           MachineFunctionAnalysisManager &MFAM) {
  MFPropsModifier _(*this, MF);
  auto *DT =
      Aggressive ? &MFAM.getResult<MachineDominatorTreeAnalysis>(MF) : nullptr;
  auto *MLI = &MFAM.getResult<MachineLoopAnalysis>(MF);
  PeepholeOptimizer Impl(DT, MLI);
  bool Changed = Impl.run(MF);
  if (!Changed)
    return PreservedAnalyses::all();

  auto PA = getMachineFunctionPassPreservedAnalyses();
  PA.preserve<MachineDominatorTreeAnalysis>();
  PA.preserve<MachineLoopAnalysis>();
  PA.preserveSet<CFGAnalyses>();
  return PA;
}

bool PeepholeOptimizerLegacy::runOnMachineFunction(MachineFunction &MF) {
  if (skipFunction(MF.getFunction()))
    return false;
  auto *DT = Aggressive
                 ? &getAnalysis<MachineDominatorTreeWrapperPass>().getDomTree()
                 : nullptr;
  auto *MLI = &getAnalysis<MachineLoopInfoWrapperPass>().getLI();
  PeepholeOptimizer Impl(DT, MLI);
  return Impl.run(MF);
}

bool PeepholeOptimizer::run(MachineFunction &MF) {
  LLVM_DEBUG(dbgs() << "********** PEEPHOLE OPTIMIZER **********\n");
  LLVM_DEBUG(dbgs() << "********** Function: " << MF.getName() << '\n');

  if (DisablePeephole)
    return false;

  TII = MF.getSubtarget().getInstrInfo();
  TRI = MF.getSubtarget().getRegisterInfo();
  MRI = &MF.getRegInfo();
  MF.setDelegate(this);

  bool Changed = false;

  for (MachineBasicBlock &MBB : MF) {
    bool SeenMoveImm = false;

    // #1 Capacity planning for hot per-MBB containers.
    SmallPtrSet<MachineInstr *, 16> LocalMIs;
    LocalMIs.reserve(MBB.size());

    SmallSet<Register, 4> ImmDefRegs;
    DenseMap<Register, MachineInstr *> ImmDefMIs;
    ImmDefMIs.reserve(MBB.size());

    SmallSet<Register, 16> FoldAsLoadDefCandidates;

    DenseMap<Register, MachineInstr *> NAPhysToVirtMIs;
    NAPhysToVirtMIs.reserve(8);

    CopySrcMIs.clear();

    bool IsLoopHeader = MLI->isLoopHeader(&MBB);

    for (MachineBasicBlock::iterator MII = MBB.begin(), MIE = MBB.end();
         MII != MIE;) {
      MachineInstr *MI = &*MII;
      ++MII;
      LocalMIs.insert(MI);

      if (MI->isDebugInstr() || MI->isPosition())
        continue;

      if (IsLoopHeader && MI->isPHI()) {
        if (optimizeRecurrence(*MI)) {
          Changed = true;
          continue;
        }
      }

      if (!MI->isCopy()) {
        for (const MachineOperand &MO : MI->operands()) {
          if (MO.isReg()) {
            Register Reg = MO.getReg();
            if (MO.isDef() && isNAPhysCopy(Reg)) {
              auto Def = NAPhysToVirtMIs.find(Reg);
              if (Def != NAPhysToVirtMIs.end()) {
                LLVM_DEBUG(dbgs()
                           << "NAPhysCopy: invalidating because of " << *MI);
                NAPhysToVirtMIs.erase(Def);
              }
            }
          } else if (MO.isRegMask()) {
            const uint32_t *RegMask = MO.getRegMask();
            // #3 Two-step erase for map iteration safety/predictability.
            SmallVector<Register, 4> ClobberedRegs;
            for (const auto &RegMI : NAPhysToVirtMIs) {
              Register DefReg = RegMI.first;
              if (MachineOperand::clobbersPhysReg(RegMask, DefReg))
                ClobberedRegs.push_back(DefReg);
            }
            for (Register DefReg : ClobberedRegs) {
              LLVM_DEBUG(dbgs()
                         << "NAPhysCopy: invalidating because of " << *MI);
              NAPhysToVirtMIs.erase(DefReg);
            }
          }
        }
      }

      if (MI->isImplicitDef() || MI->isKill())
        continue;

      if (MI->isInlineAsm() || MI->hasUnmodeledSideEffects()) {
        LLVM_DEBUG(dbgs() << "NAPhysCopy: blowing away all info due to "
                          << *MI);
        NAPhysToVirtMIs.clear();
      }

      const bool IsCompare = MI->isCompare();
      Register CmpSrcReg;
      Register CmpSrcReg2;
      int64_t CmpMask = 0;
      int64_t CmpValue = 0;
      bool HasCmpInfo = false;

      if (IsCompare) {
        HasCmpInfo =
            TII->analyzeCompare(*MI, CmpSrcReg, CmpSrcReg2, CmpMask, CmpValue) &&
            !CmpSrcReg.isPhysical() && !CmpSrcReg2.isPhysical();
      }

      if ((isUncoalescableCopy(*MI) &&
           optimizeUncoalescableCopy(*MI, LocalMIs)) ||
          (IsCompare && HasCmpInfo &&
           optimizeCmpInstr(*MI, CmpSrcReg, CmpSrcReg2, CmpMask, CmpValue)) ||
          (MI->isSelect() && optimizeSelect(*MI, LocalMIs))) {
        LocalMIs.erase(MI);
        Changed = true;

        if (IsCompare && HasCmpInfo)
          (void)tryFoldLoadAfterCompareElim(CmpSrcReg, LocalMIs);

        continue;
      }

      if (MI->isConditionalBranch() && optimizeCondBranch(*MI)) {
        Changed = true;
        continue;
      }

      if (isCoalescableCopy(*MI) && optimizeCoalescableCopy(*MI)) {
        Changed = true;
        continue;
      }

      if (MI->isCopy() && (foldRedundantCopy(*MI) ||
                           foldRedundantNAPhysCopy(*MI, NAPhysToVirtMIs))) {
        LocalMIs.erase(MI);
        LLVM_DEBUG(dbgs() << "Deleting redundant copy: " << *MI << "\n");
        MI->eraseFromParent();
        Changed = true;
        continue;
      }

      if (isMoveImmediate(*MI, ImmDefRegs, ImmDefMIs)) {
        SeenMoveImm = true;
      } else {
        Changed |= optimizeExtInstr(*MI, MBB, LocalMIs);

        MII = MI;
        ++MII;

        if (SeenMoveImm) {
          bool Deleted = false;
          Changed |= foldImmediate(*MI, ImmDefRegs, ImmDefMIs, Deleted);
          if (Deleted) {
            LocalMIs.erase(MI);
            continue;
          }
        }
      }

      if (!isLoadFoldable(*MI, FoldAsLoadDefCandidates) &&
          !FoldAsLoadDefCandidates.empty()) {
        for (unsigned i = MI->getDesc().getNumDefs(); i != MI->getNumOperands();
             ++i) {
          const MachineOperand &MOp = MI->getOperand(i);
          if (!MOp.isReg())
            continue;

          Register FoldAsLoadDefReg = MOp.getReg();
          if (!FoldAsLoadDefCandidates.count(FoldAsLoadDefReg))
            continue;

          // #2 Single helper for repeated fold+bookkeeping mechanics.
          if (MachineInstr *FoldMI =
                  foldLoadInto(MF, *MI, FoldAsLoadDefReg, LocalMIs)) {
            FoldAsLoadDefCandidates.erase(FoldAsLoadDefReg);
            Changed = true;
            MI = FoldMI;
          }
        }
      }

      if (MI->isLoadFoldBarrier()) {
        LLVM_DEBUG(dbgs() << "Encountered load fold barrier on " << *MI);
        FoldAsLoadDefCandidates.clear();
      }
    }
  }

  MF.resetDelegate(this);
  return Changed;
}

ValueTrackerResult ValueTracker::getNextSourceFromCopy() {
  assert(Def->isCopy() && "Invalid definition");
  assert(Def->getNumOperands() - Def->getNumImplicitOperands() == 2 &&
         "Invalid number of operands");
  assert(!Def->hasImplicitDef() && "Only implicit uses are allowed");
  assert(!Def->getOperand(DefIdx).getSubReg() && "no subregister defs in SSA");

  const MachineOperand &Src = Def->getOperand(1);
  if (Src.isUndef())
    return ValueTrackerResult();

  Register SrcReg = Src.getReg();
  unsigned SubReg = Src.getSubReg();
  if (DefSubReg) {
    const TargetRegisterInfo *TRI = MRI.getTargetRegisterInfo();
    SubReg = TRI->composeSubRegIndices(SubReg, DefSubReg);

    if (SrcReg.isVirtual()) {
      const TargetRegisterClass *RegRC = MRI.getRegClass(SrcReg);
      if (!TRI->isSubRegValidForRegClass(RegRC, SubReg))
        return ValueTrackerResult();
    } else if (!TRI->getSubReg(SrcReg, SubReg)) {
      return ValueTrackerResult();
    }
  }

  return ValueTrackerResult(SrcReg, SubReg);
}

ValueTrackerResult ValueTracker::getNextSourceFromBitcast() {
  assert(Def->isBitcast() && "Invalid definition");

  if (Def->mayRaiseFPException() || Def->hasUnmodeledSideEffects())
    return ValueTrackerResult();

  if (Def->getDesc().getNumDefs() != 1)
    return ValueTrackerResult();

  assert(!Def->getOperand(DefIdx).getSubReg() && "no subregister defs in SSA");

  unsigned SrcIdx = Def->getNumOperands();
  for (unsigned OpIdx = DefIdx + 1, EndOpIdx = SrcIdx; OpIdx != EndOpIdx;
       ++OpIdx) {
    const MachineOperand &MO = Def->getOperand(OpIdx);
    if (!MO.isReg() || !MO.getReg())
      continue;
    if (MO.isImplicit() && MO.isDead())
      continue;
    assert(!MO.isDef() && "We should have skipped all the definitions by now");
    if (SrcIdx != EndOpIdx)
      return ValueTrackerResult();
    SrcIdx = OpIdx;
  }

  if (SrcIdx >= Def->getNumOperands())
    return ValueTrackerResult();

  const MachineOperand &DefOp = Def->getOperand(DefIdx);
  for (const MachineInstr &UseMI : MRI.use_nodbg_instructions(DefOp.getReg())) {
    if (UseMI.isSubregToReg())
      return ValueTrackerResult();
  }

  const MachineOperand &Src = Def->getOperand(SrcIdx);
  if (Src.isUndef())
    return ValueTrackerResult();

  return ValueTrackerResult(Src.getReg(), Src.getSubReg());
}

ValueTrackerResult ValueTracker::getNextSourceFromRegSequence() {
  assert((Def->isRegSequence() || Def->isRegSequenceLike()) &&
         "Invalid definition");
  assert(!Def->getOperand(DefIdx).getSubReg() && "illegal subregister def");

  SmallVector<RegSubRegPairAndIdx, 8> RegSeqInputRegs;
  if (!TII->getRegSequenceInputs(*Def, DefIdx, RegSeqInputRegs))
    return ValueTrackerResult();

  for (const RegSubRegPairAndIdx &RegSeqInput : RegSeqInputRegs) {
    if (RegSeqInput.SubIdx == DefSubReg)
      return ValueTrackerResult(RegSeqInput.Reg, RegSeqInput.SubReg);
  }

  const TargetRegisterInfo *TRI = MRI.getTargetRegisterInfo();
  for (const RegSubRegPairAndIdx &RegSeqInput : RegSeqInputRegs) {
    LaneBitmask DefMask = TRI->getSubRegIndexLaneMask(DefSubReg);
    LaneBitmask ThisOpRegMask = TRI->getSubRegIndexLaneMask(RegSeqInput.SubIdx);

    if ((DefMask & ThisOpRegMask) != DefMask)
      continue;

    unsigned ReverseDefCompose =
        TRI->reverseComposeSubRegIndices(RegSeqInput.SubIdx, DefSubReg);
    if (!ReverseDefCompose)
      continue;

    unsigned ComposedDefInSrcReg1 =
        TRI->composeSubRegIndices(RegSeqInput.SubReg, ReverseDefCompose);

    const TargetRegisterClass *SrcRC = MRI.getRegClass(RegSeqInput.Reg);
    if (!TRI->isSubRegValidForRegClass(SrcRC, ComposedDefInSrcReg1))
      return ValueTrackerResult();

    return ValueTrackerResult(RegSeqInput.Reg, ComposedDefInSrcReg1);
  }

  return ValueTrackerResult();
}

ValueTrackerResult ValueTracker::getNextSourceFromInsertSubreg() {
  assert((Def->isInsertSubreg() || Def->isInsertSubregLike()) &&
         "Invalid definition");
  assert(!Def->getOperand(DefIdx).getSubReg() && "no subreg defs in SSA");

  RegSubRegPair BaseReg;
  RegSubRegPairAndIdx InsertedReg;
  if (!TII->getInsertSubregInputs(*Def, DefIdx, BaseReg, InsertedReg))
    return ValueTrackerResult();

  if (InsertedReg.SubIdx == DefSubReg)
    return ValueTrackerResult(InsertedReg.Reg, InsertedReg.SubReg);

  const MachineOperand &MODef = Def->getOperand(DefIdx);
  if (MRI.getRegClass(MODef.getReg()) != MRI.getRegClass(BaseReg.Reg) ||
      BaseReg.SubReg)
    return ValueTrackerResult();

  const TargetRegisterInfo *TRI = MRI.getTargetRegisterInfo();
  if ((TRI->getSubRegIndexLaneMask(DefSubReg) &
       TRI->getSubRegIndexLaneMask(InsertedReg.SubIdx))
          .any())
    return ValueTrackerResult();

  return ValueTrackerResult(BaseReg.Reg, DefSubReg);
}

ValueTrackerResult ValueTracker::getNextSourceFromExtractSubreg() {
  assert((Def->isExtractSubreg() || Def->isExtractSubregLike()) &&
         "Invalid definition");

  if (DefSubReg)
    return ValueTrackerResult();

  RegSubRegPairAndIdx ExtractSubregInputReg;
  if (!TII->getExtractSubregInputs(*Def, DefIdx, ExtractSubregInputReg))
    return ValueTrackerResult();

  if (ExtractSubregInputReg.SubReg)
    return ValueTrackerResult();

  return ValueTrackerResult(ExtractSubregInputReg.Reg,
                            ExtractSubregInputReg.SubIdx);
}

ValueTrackerResult ValueTracker::getNextSourceFromSubregToReg() {
  assert(Def->isSubregToReg() && "Invalid definition");

  if (DefSubReg != Def->getOperand(2).getImm())
    return ValueTrackerResult();

  if (Def->getOperand(1).getSubReg())
    return ValueTrackerResult();

  return ValueTrackerResult(Def->getOperand(1).getReg(),
                            Def->getOperand(2).getImm());
}

ValueTrackerResult ValueTracker::getNextSourceFromPHI() {
  assert(Def->isPHI() && "Invalid definition");
  ValueTrackerResult Res;

  for (unsigned i = 1, e = Def->getNumOperands(); i < e; i += 2) {
    const MachineOperand &MO = Def->getOperand(i);
    assert(MO.isReg() && "Invalid PHI instruction");
    if (MO.isUndef())
      return ValueTrackerResult();
    Res.addSource(MO.getReg(), MO.getSubReg());
  }

  return Res;
}

ValueTrackerResult ValueTracker::getNextSourceImpl() {
  assert(Def && "This method needs a valid definition");
  assert(((Def->getOperand(DefIdx).isDef() &&
           (DefIdx < Def->getDesc().getNumDefs() ||
            Def->getDesc().isVariadic())) ||
          Def->getOperand(DefIdx).isImplicit()) &&
         "Invalid DefIdx");

  if (Def->isCopy())
    return getNextSourceFromCopy();
  if (Def->isBitcast())
    return getNextSourceFromBitcast();

  if (DisableAdvCopyOpt)
    return ValueTrackerResult();
  if (Def->isRegSequence() || Def->isRegSequenceLike())
    return getNextSourceFromRegSequence();
  if (Def->isInsertSubreg() || Def->isInsertSubregLike())
    return getNextSourceFromInsertSubreg();
  if (Def->isExtractSubreg() || Def->isExtractSubregLike())
    return getNextSourceFromExtractSubreg();
  if (Def->isSubregToReg())
    return getNextSourceFromSubregToReg();
  if (Def->isPHI())
    return getNextSourceFromPHI();
  return ValueTrackerResult();
}

ValueTrackerResult ValueTracker::getNextSource() {
  if (!Def)
    return ValueTrackerResult();

  ValueTrackerResult Res = getNextSourceImpl();
  if (Res.isValid()) {
    bool OneRegSrc = Res.getNumSources() == 1;
    if (OneRegSrc)
      Reg = Res.getSrcReg(0);

    Res.setInst(Def);

    if (!Reg.isPhysical() && OneRegSrc) {
      MachineRegisterInfo::def_iterator DI = MRI.def_begin(Reg);
      if (DI != MRI.def_end()) {
        Def = DI->getParent();
        DefIdx = DI.getOperandNo();
        DefSubReg = Res.getSrcSubReg(0);
      } else {
        Def = nullptr;
      }
      return Res;
    }
  }

  Def = nullptr;
  return Res;
}
