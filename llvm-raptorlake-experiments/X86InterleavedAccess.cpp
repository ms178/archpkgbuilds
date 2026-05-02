//===- X86InterleavedAccess.cpp -------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
/// \file
/// This file contains the X86 implementation of the interleaved accesses
/// optimization generating X86-specific instructions/intrinsics for
/// interleaved access groups.
//
//===----------------------------------------------------------------------===//

#include "X86ISelLowering.h"
#include "X86Subtarget.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/VectorUtils.h"
#include "llvm/CodeGenTypes/MachineValueType.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include <algorithm>
#include <cassert>
#include <cstdint>

using namespace llvm;

namespace {

class X86InterleavedAccessGroup {
  Instruction *const Inst;
  ArrayRef<ShuffleVectorInst *> Shuffles;
  ArrayRef<unsigned> Indices;
  const unsigned Factor;
  const X86Subtarget &Subtarget;
  const DataLayout &DL;
  IRBuilder<> &Builder;

  void decompose(Instruction *Inst, unsigned NumSubVectors, FixedVectorType *T,
                 SmallVectorImpl<Instruction *> &DecomposedVectors);
  void transpose_4x4(ArrayRef<Instruction *> InputVectors,
                     SmallVectorImpl<Value *> &TransposedMatrix);
  void interleave8bitStride4(ArrayRef<Instruction *> InputVectors,
                             SmallVectorImpl<Value *> &TransposedMatrix,
                             unsigned NumSubVecElems);
  void interleave8bitStride4VF8(ArrayRef<Instruction *> InputVectors,
                                SmallVectorImpl<Value *> &TransposedMatrix);
  void interleave8bitStride3(ArrayRef<Instruction *> InputVectors,
                             SmallVectorImpl<Value *> &TransposedMatrix,
                             unsigned NumSubVecElems);
  void deinterleave8bitStride3(ArrayRef<Instruction *> InputVectors,
                               SmallVectorImpl<Value *> &TransposedMatrix,
                               unsigned NumSubVecElems);

public:
  explicit X86InterleavedAccessGroup(Instruction *I,
                                     ArrayRef<ShuffleVectorInst *> Shuffs,
                                     ArrayRef<unsigned> Ind, const unsigned F,
                                     const X86Subtarget &STarget,
                                     IRBuilder<> &B)
      : Inst(I), Shuffles(Shuffs), Indices(Ind), Factor(F), Subtarget(STarget),
        DL(Inst->getDataLayout()), Builder(B) {}

  bool isSupported() const;
  bool lowerIntoOptimizedSequence();
};

} // end anonymous namespace

static MVT scaleVectorType(MVT VT) {
  unsigned ScalarSize = VT.getVectorElementType().getScalarSizeInBits() * 2;
  return MVT::getVectorVT(MVT::getIntegerVT(ScalarSize),
                          VT.getVectorNumElements() / 2);
}

static constexpr int Concat[] = {
    0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15,
    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63};

static void genShuffleBland(MVT VT, ArrayRef<int> Mask,
                            SmallVectorImpl<int> &Out, int LowOffset,
                            int HighOffset) {
  assert(VT.getSizeInBits() >= 256 &&
         "This function doesn't accept width smaller then 256");
  unsigned NumOfElm = VT.getVectorNumElements();
  for (int I : Mask)
    Out.push_back(I + LowOffset);
  for (int I : Mask)
    Out.push_back(I + HighOffset + static_cast<int>(NumOfElm));
}

static void reorderSubVector(MVT VT, SmallVectorImpl<Value *> &TransposedMatrix,
                             ArrayRef<Value *> Vec, ArrayRef<int> VPShuf,
                             unsigned VecElems, unsigned Stride,
                             IRBuilder<> &Builder) {
  if (VecElems == 16) {
    for (unsigned i = 0; i < Stride; ++i)
      TransposedMatrix[i] = Builder.CreateShuffleVector(Vec[i], VPShuf);
    return;
  }

  SmallVector<int, 32> OptimizeShuf;
  Value *Temp[8];

  for (unsigned i = 0; i < (VecElems / 16) * Stride; i += 2) {
    genShuffleBland(VT, VPShuf, OptimizeShuf, (i / Stride) * 16,
                    ((i + 1) / Stride) * 16);
    Temp[i / 2] = Builder.CreateShuffleVector(
        Vec[i % Stride], Vec[(i + 1) % Stride], OptimizeShuf);
    OptimizeShuf.clear();
  }

  if (VecElems == 32) {
    std::copy(Temp, Temp + Stride, TransposedMatrix.begin());
    return;
  }

  for (unsigned i = 0; i < Stride; ++i)
    TransposedMatrix[i] =
        Builder.CreateShuffleVector(Temp[2 * i], Temp[2 * i + 1], Concat);
}

static void createShuffleStride(MVT VT, unsigned Stride,
                                SmallVectorImpl<int> &Mask) {
  uint64_t VectorSize = VT.getSizeInBits();
  unsigned VF = VT.getVectorNumElements();
  unsigned LaneCount =
      static_cast<unsigned>(std::max<uint64_t>(VectorSize / 128, 1));
  for (unsigned Lane = 0; Lane < LaneCount; ++Lane) {
    unsigned LaneSize = VF / LaneCount;
    for (unsigned i = 0; i != LaneSize; ++i)
      Mask.push_back((i * Stride) % LaneSize + LaneSize * Lane);
  }
}

static void setGroupSize(MVT VT, SmallVectorImpl<int> &SizeInfo) {
  uint64_t VectorSize = VT.getSizeInBits();
  unsigned VF = VT.getVectorNumElements() /
                static_cast<unsigned>(std::max<uint64_t>(VectorSize / 128, 1));
  unsigned FirstGroupElement = 0;
  for (unsigned i = 0; i < 3; ++i) {
    unsigned GroupSize = (VF - FirstGroupElement + 2) / 3;
    SizeInfo.push_back(static_cast<int>(GroupSize));
    FirstGroupElement = (GroupSize * 3 + FirstGroupElement) % VF;
  }
}

static void DecodePALIGNRMask(MVT VT, unsigned Imm,
                              SmallVectorImpl<int> &ShuffleMask,
                              bool AlignDirection = true,
                              bool Unary = false) {
  unsigned NumElts = VT.getVectorNumElements();
  unsigned NumLanes =
      static_cast<unsigned>(std::max<uint64_t>(VT.getSizeInBits() / 128, 1));
  unsigned NumLaneElts = NumElts / NumLanes;

  Imm = AlignDirection ? Imm : (NumLaneElts - Imm);
  unsigned Offset = Imm * (VT.getScalarSizeInBits() / 8);

  for (unsigned l = 0; l != NumElts; l += NumLaneElts) {
    for (unsigned i = 0; i != NumLaneElts; ++i) {
      unsigned Base = i + Offset;
      if (Base >= NumLaneElts)
        Base = Unary ? (Base % NumLaneElts) : (Base + NumElts - NumLaneElts);
      ShuffleMask.push_back(static_cast<int>(Base + l));
    }
  }
}

static void concatSubVector(Value **Vec, ArrayRef<Instruction *> InVec,
                            unsigned VecElems, IRBuilder<> &Builder) {
  if (VecElems == 16) {
    for (int i = 0; i < 3; ++i)
      Vec[i] = InVec[i];
    return;
  }

  for (unsigned j = 0; j < VecElems / 32; ++j)
    for (int i = 0; i < 3; ++i)
      Vec[i + j * 3] = Builder.CreateShuffleVector(
          InVec[j * 6 + i], InVec[j * 6 + i + 3], ArrayRef(Concat, 32));

  if (VecElems == 32)
    return;

  for (int i = 0; i < 3; ++i)
    Vec[i] = Builder.CreateShuffleVector(Vec[i], Vec[i + 3], Concat);
}

bool X86InterleavedAccessGroup::isSupported() const {
  VectorType *ShuffleVecTy = Shuffles[0]->getType();
  Type *ShuffleEltTy = ShuffleVecTy->getElementType();
  unsigned ShuffleElemSize = DL.getTypeSizeInBits(ShuffleEltTy);
  unsigned WideInstSize;

  if (!Subtarget.hasAVX() || (Factor != 4 && Factor != 3))
    return false;

  if (auto *LI = dyn_cast<LoadInst>(Inst)) {
    if (!LI->isSimple())
      return false;
    WideInstSize = DL.getTypeSizeInBits(Inst->getType());
    if (LI->getPointerAddressSpace())
      return false;
  } else if (auto *SI = dyn_cast<StoreInst>(Inst)) {
    if (!SI->isSimple())
      return false;
    WideInstSize = DL.getTypeSizeInBits(Shuffles[0]->getType());
  } else {
    return false;
  }

  if (ShuffleElemSize == 64 && WideInstSize == 1024 && Factor == 4)
    return true;

  // General stride-4 transpose case: 16 total elements (4 x 4 matrix) for any
  // legal element type. This maps directly to transpose_4x4 in both load/store
  // paths and enables more non-byte interleaved patterns.
  if (Factor == 4 && ShuffleElemSize >= 8 &&
      WideInstSize % ShuffleElemSize == 0) {
    unsigned NumVecElts = WideInstSize / ShuffleElemSize;
    if (NumVecElts == 16 && (Subtarget.hasAVX2() || WideInstSize <= 256))
      return true;
  }

  if (ShuffleElemSize == 8 && isa<StoreInst>(Inst) && Factor == 4 &&
      (WideInstSize == 256 || WideInstSize == 512 || WideInstSize == 1024 ||
       WideInstSize == 2048))
    return true;

  if (ShuffleElemSize == 8 && Factor == 3 &&
      (WideInstSize == 384 || WideInstSize == 768 || WideInstSize == 1536))
    return true;

  return false;
}

void X86InterleavedAccessGroup::decompose(
    Instruction *VecInst, unsigned NumSubVectors, FixedVectorType *SubVecTy,
    SmallVectorImpl<Instruction *> &DecomposedVectors) {
  assert((isa<LoadInst>(VecInst) || isa<ShuffleVectorInst>(VecInst)) &&
         "Expected Load or Shuffle");

  Type *VecWidth = VecInst->getType();
  assert(VecWidth->isVectorTy() &&
         DL.getTypeSizeInBits(VecWidth) >=
             DL.getTypeSizeInBits(SubVecTy) * NumSubVectors &&
         "Invalid Inst-size!!!");

  if (auto *SVI = dyn_cast<ShuffleVectorInst>(VecInst)) {
    Value *Op0 = SVI->getOperand(0);
    Value *Op1 = SVI->getOperand(1);
    for (unsigned i = 0; i < NumSubVectors; ++i)
      DecomposedVectors.push_back(
          cast<ShuffleVectorInst>(Builder.CreateShuffleVector(
              Op0, Op1,
              createSequentialMask(Indices[i], SubVecTy->getNumElements(),
                                   0))));
    return;
  }

  LoadInst *LI = cast<LoadInst>(VecInst);
  Type *VecBaseTy;
  unsigned NumLoads = NumSubVectors;
  unsigned VecLength = DL.getTypeSizeInBits(VecWidth);
  Value *VecBasePtr = LI->getPointerOperand();

  if (VecLength == 768 || VecLength == 1536) {
    VecBaseTy = FixedVectorType::get(Type::getInt8Ty(LI->getContext()), 16);
    NumLoads = NumSubVectors * (VecLength / 384);
  } else {
    VecBaseTy = SubVecTy;
  }

  assert(VecBaseTy->getPrimitiveSizeInBits().isKnownMultipleOf(8) &&
         "VecBaseTy's size must be a multiple of 8");

  const Align FirstAlignment = LI->getAlign();
  const Align SubsequentAlignment = commonAlignment(
      FirstAlignment, VecBaseTy->getPrimitiveSizeInBits().getFixedValue() / 8);
  Align Alignment = FirstAlignment;

  for (unsigned i = 0; i < NumLoads; ++i) {
    Value *NewBasePtr =
        Builder.CreateGEP(VecBaseTy, VecBasePtr, Builder.getInt32(i));
    auto *NewLoad = cast<LoadInst>(
        Builder.CreateAlignedLoad(VecBaseTy, NewBasePtr, Alignment));
    NewLoad->copyMetadata(*LI);
    DecomposedVectors.push_back(NewLoad);
    Alignment = SubsequentAlignment;
  }
}

void X86InterleavedAccessGroup::transpose_4x4(
    ArrayRef<Instruction *> Matrix,
    SmallVectorImpl<Value *> &TransposedMatrix) {
  assert(Matrix.size() == 4 && "Invalid matrix size");
  TransposedMatrix.resize(4);

  static constexpr int IntMask1[] = {0, 1, 4, 5};
  ArrayRef<int> Mask = ArrayRef(IntMask1, 4);
  Value *IntrVec1 = Builder.CreateShuffleVector(Matrix[0], Matrix[2], Mask);
  Value *IntrVec2 = Builder.CreateShuffleVector(Matrix[1], Matrix[3], Mask);

  static constexpr int IntMask2[] = {2, 3, 6, 7};
  Mask = ArrayRef(IntMask2, 4);
  Value *IntrVec3 = Builder.CreateShuffleVector(Matrix[0], Matrix[2], Mask);
  Value *IntrVec4 = Builder.CreateShuffleVector(Matrix[1], Matrix[3], Mask);

  static constexpr int IntMask3[] = {0, 4, 2, 6};
  Mask = ArrayRef(IntMask3, 4);
  TransposedMatrix[0] = Builder.CreateShuffleVector(IntrVec1, IntrVec2, Mask);
  TransposedMatrix[2] = Builder.CreateShuffleVector(IntrVec3, IntrVec4, Mask);

  static constexpr int IntMask4[] = {1, 5, 3, 7};
  Mask = ArrayRef(IntMask4, 4);
  TransposedMatrix[1] = Builder.CreateShuffleVector(IntrVec1, IntrVec2, Mask);
  TransposedMatrix[3] = Builder.CreateShuffleVector(IntrVec3, IntrVec4, Mask);
}

void X86InterleavedAccessGroup::interleave8bitStride4VF8(
    ArrayRef<Instruction *> Matrix,
    SmallVectorImpl<Value *> &TransposedMatrix) {
  MVT VT = MVT::v8i16;
  TransposedMatrix.resize(2);
  SmallVector<int, 16> MaskLow;
  SmallVector<int, 32> MaskLowTemp1, MaskLowWord;
  SmallVector<int, 32> MaskHighTemp1, MaskHighWord;

  for (unsigned i = 0; i < 8; ++i) {
    MaskLow.push_back(static_cast<int>(i));
    MaskLow.push_back(static_cast<int>(i + 8));
  }

  createUnpackShuffleMask(VT, MaskLowTemp1, true, false);
  createUnpackShuffleMask(VT, MaskHighTemp1, false, false);
  narrowShuffleMaskElts(2, MaskHighTemp1, MaskHighWord);
  narrowShuffleMaskElts(2, MaskLowTemp1, MaskLowWord);

  Value *IntrVec1Low =
      Builder.CreateShuffleVector(Matrix[0], Matrix[1], MaskLow);
  Value *IntrVec2Low =
      Builder.CreateShuffleVector(Matrix[2], Matrix[3], MaskLow);

  TransposedMatrix[0] =
      Builder.CreateShuffleVector(IntrVec1Low, IntrVec2Low, MaskLowWord);
  TransposedMatrix[1] =
      Builder.CreateShuffleVector(IntrVec1Low, IntrVec2Low, MaskHighWord);
}

void X86InterleavedAccessGroup::interleave8bitStride4(
    ArrayRef<Instruction *> Matrix, SmallVectorImpl<Value *> &TransposedMatrix,
    unsigned NumOfElm) {
  MVT VT = MVT::getVectorVT(MVT::i8, NumOfElm);
  MVT HalfVT = scaleVectorType(VT);

  TransposedMatrix.resize(4);
  SmallVector<int, 32> MaskHigh;
  SmallVector<int, 32> MaskLow;
  SmallVector<int, 32> LowHighMask[2];
  SmallVector<int, 32> MaskHighTemp;
  SmallVector<int, 32> MaskLowTemp;

  createUnpackShuffleMask(VT, MaskLow, true, false);
  createUnpackShuffleMask(VT, MaskHigh, false, false);

  createUnpackShuffleMask(HalfVT, MaskLowTemp, true, false);
  createUnpackShuffleMask(HalfVT, MaskHighTemp, false, false);
  narrowShuffleMaskElts(2, MaskLowTemp, LowHighMask[0]);
  narrowShuffleMaskElts(2, MaskHighTemp, LowHighMask[1]);

  Value *IntrVec[4];
  IntrVec[0] = Builder.CreateShuffleVector(Matrix[0], Matrix[1], MaskLow);
  IntrVec[1] = Builder.CreateShuffleVector(Matrix[0], Matrix[1], MaskHigh);
  IntrVec[2] = Builder.CreateShuffleVector(Matrix[2], Matrix[3], MaskLow);
  IntrVec[3] = Builder.CreateShuffleVector(Matrix[2], Matrix[3], MaskHigh);

  Value *VecOut[4];
  for (int i = 0; i < 4; ++i)
    VecOut[i] = Builder.CreateShuffleVector(IntrVec[i / 2], IntrVec[i / 2 + 2],
                                            LowHighMask[i % 2]);

  if (VT == MVT::v16i8) {
    std::copy(VecOut, VecOut + 4, TransposedMatrix.begin());
    return;
  }

  reorderSubVector(VT, TransposedMatrix, VecOut, ArrayRef(Concat, 16), NumOfElm,
                   4, Builder);
}

void X86InterleavedAccessGroup::deinterleave8bitStride3(
    ArrayRef<Instruction *> InVec, SmallVectorImpl<Value *> &TransposedMatrix,
    unsigned VecElems) {
  TransposedMatrix.resize(3);
  SmallVector<int, 32> VPShuf;
  SmallVector<int, 32> VPAlign[2];
  SmallVector<int, 32> VPAlign2;
  SmallVector<int, 32> VPAlign3;
  SmallVector<int, 3> GroupSize;
  Value *Vec[6], *TempVector[3];

  MVT VT = MVT::getVT(Shuffles[0]->getType());

  createShuffleStride(VT, 3, VPShuf);
  setGroupSize(VT, GroupSize);

  for (int i = 0; i < 2; ++i)
    DecodePALIGNRMask(VT, GroupSize[2 - i], VPAlign[i], false);

  DecodePALIGNRMask(VT, GroupSize[2] + GroupSize[1], VPAlign2, true, true);
  DecodePALIGNRMask(VT, GroupSize[1], VPAlign3, true, true);

  concatSubVector(Vec, InVec, VecElems, Builder);

  for (int i = 0; i < 3; ++i)
    Vec[i] = Builder.CreateShuffleVector(Vec[i], VPShuf);

  for (int i = 0; i < 3; ++i)
    TempVector[i] =
        Builder.CreateShuffleVector(Vec[(i + 2) % 3], Vec[i], VPAlign[0]);

  for (int i = 0; i < 3; ++i)
    Vec[i] = Builder.CreateShuffleVector(TempVector[(i + 1) % 3], TempVector[i],
                                         VPAlign[1]);

  Value *TempVec = Builder.CreateShuffleVector(Vec[1], VPAlign3);
  TransposedMatrix[0] = Builder.CreateShuffleVector(Vec[0], VPAlign2);
  TransposedMatrix[1] = VecElems == 8 ? Vec[2] : TempVec;
  TransposedMatrix[2] = VecElems == 8 ? TempVec : Vec[2];
}

static void group2Shuffle(MVT VT, SmallVectorImpl<int> &Mask,
                          SmallVectorImpl<int> &Output) {
  int IndexGroup[3] = {0, 0, 0};
  int Index = 0;
  int VectorWidth = VT.getSizeInBits();
  int VF = VT.getVectorNumElements();
  int Lane = (VectorWidth / 128 > 0) ? VectorWidth / 128 : 1;
  for (int i = 0; i < 3; ++i) {
    IndexGroup[(Index * 3) % (VF / Lane)] = Index;
    Index += Mask[i];
  }
  for (int i = 0; i < VF / Lane; ++i) {
    Output.push_back(IndexGroup[i % 3]);
    IndexGroup[i % 3]++;
  }
}

void X86InterleavedAccessGroup::interleave8bitStride3(
    ArrayRef<Instruction *> InVec, SmallVectorImpl<Value *> &TransposedMatrix,
    unsigned VecElems) {
  TransposedMatrix.resize(3);
  SmallVector<int, 3> GroupSize;
  SmallVector<int, 32> VPShuf;
  SmallVector<int, 32> VPAlign[3];
  SmallVector<int, 32> VPAlign2;
  SmallVector<int, 32> VPAlign3;

  Value *Vec[3], *TempVector[3];
  MVT VT = MVT::getVectorVT(MVT::i8, VecElems);

  setGroupSize(VT, GroupSize);

  for (int i = 0; i < 3; ++i)
    DecodePALIGNRMask(VT, GroupSize[i], VPAlign[i]);

  DecodePALIGNRMask(VT, GroupSize[1] + GroupSize[2], VPAlign2, false, true);
  DecodePALIGNRMask(VT, GroupSize[1], VPAlign3, false, true);

  Vec[0] = Builder.CreateShuffleVector(InVec[0], VPAlign2);
  Vec[1] = Builder.CreateShuffleVector(InVec[1], VPAlign3);
  Vec[2] = InVec[2];

  for (int i = 0; i < 3; ++i)
    TempVector[i] =
        Builder.CreateShuffleVector(Vec[i], Vec[(i + 2) % 3], VPAlign[1]);

  for (int i = 0; i < 3; ++i)
    Vec[i] = Builder.CreateShuffleVector(TempVector[i], TempVector[(i + 1) % 3],
                                         VPAlign[2]);

  unsigned NumOfElm = VT.getVectorNumElements();
  group2Shuffle(VT, GroupSize, VPShuf);
  reorderSubVector(VT, TransposedMatrix, Vec, VPShuf, NumOfElm, 3, Builder);
}

bool X86InterleavedAccessGroup::lowerIntoOptimizedSequence() {
  SmallVector<Instruction *, 4> DecomposedVectors;
  SmallVector<Value *, 4> TransposedVectors;
  auto *ShuffleTy = cast<FixedVectorType>(Shuffles[0]->getType());

  if (isa<LoadInst>(Inst)) {
    auto *ShuffleEltTy = cast<FixedVectorType>(Inst->getType());
    unsigned NumSubVecElems = ShuffleEltTy->getNumElements() / Factor;
    switch (NumSubVecElems) {
    default:
      return false;
    case 4:
    case 8:
    case 16:
    case 32:
    case 64:
      if (ShuffleTy->getNumElements() != NumSubVecElems)
        return false;
      break;
    }

    decompose(Inst, Factor, ShuffleTy, DecomposedVectors);

    if (NumSubVecElems == 4)
      transpose_4x4(DecomposedVectors, TransposedVectors);
    else
      deinterleave8bitStride3(DecomposedVectors, TransposedVectors,
                              NumSubVecElems);

    for (unsigned i = 0, e = Shuffles.size(); i < e; ++i)
      Shuffles[i]->replaceAllUsesWith(TransposedVectors[Indices[i]]);

    return true;
  }

  Type *ShuffleEltTy = ShuffleTy->getElementType();
  unsigned NumSubVecElems = ShuffleTy->getNumElements() / Factor;

  decompose(Shuffles[0], Factor,
            FixedVectorType::get(ShuffleEltTy, NumSubVecElems),
            DecomposedVectors);

  switch (NumSubVecElems) {
  case 4:
    transpose_4x4(DecomposedVectors, TransposedVectors);
    break;
  case 8:
    interleave8bitStride4VF8(DecomposedVectors, TransposedVectors);
    break;
  case 16:
  case 32:
  case 64:
    if (Factor == 4)
      interleave8bitStride4(DecomposedVectors, TransposedVectors,
                            NumSubVecElems);
    if (Factor == 3)
      interleave8bitStride3(DecomposedVectors, TransposedVectors,
                            NumSubVecElems);
    break;
  default:
    return false;
  }

  Value *WideVec = concatenateVectors(Builder, TransposedVectors);
  StoreInst *SI = cast<StoreInst>(Inst);
  auto *NewStore =
      Builder.CreateAlignedStore(WideVec, SI->getPointerOperand(), SI->getAlign());
  NewStore->copyMetadata(*SI);

  return true;
}

bool X86TargetLowering::lowerInterleavedLoad(
    Instruction *Load, Value *Mask, ArrayRef<ShuffleVectorInst *> Shuffles,
    ArrayRef<unsigned> Indices, unsigned Factor, const APInt &GapMask) const {
  assert(Factor >= 2 && Factor <= getMaxSupportedInterleaveFactor() &&
         "Invalid interleave factor");
  assert(!Shuffles.empty() && "Empty shufflevector input");
  assert(Shuffles.size() == Indices.size() &&
         "Unmatched number of shufflevectors and indices");

  auto *LI = dyn_cast<LoadInst>(Load);
  if (!LI)
    return false;
  assert(!Mask && GapMask.popcount() == Factor && "Unexpected mask on a load");

  IRBuilder<> Builder(LI);
  X86InterleavedAccessGroup Grp(LI, Shuffles, Indices, Factor, Subtarget,
                                Builder);

  return Grp.isSupported() && Grp.lowerIntoOptimizedSequence();
}

bool X86TargetLowering::lowerInterleavedStore(Instruction *Store,
                                              Value *LaneMask,
                                              ShuffleVectorInst *SVI,
                                              unsigned Factor,
                                              const APInt &GapMask) const {
  assert(Factor >= 2 && Factor <= getMaxSupportedInterleaveFactor() &&
         "Invalid interleave factor");
  assert(cast<FixedVectorType>(SVI->getType())->getNumElements() % Factor == 0 &&
         "Invalid interleaved store");

  auto *SI = dyn_cast<StoreInst>(Store);
  if (!SI)
    return false;
  assert(!LaneMask && GapMask.popcount() == Factor &&
         "Unexpected mask on store");

  auto Mask = SVI->getShuffleMask();
  SmallVector<unsigned, 4> Indices(Mask.take_front(Factor));

  ArrayRef<ShuffleVectorInst *> Shuffles = ArrayRef(SVI);

  IRBuilder<> Builder(SI);
  X86InterleavedAccessGroup Grp(SI, Shuffles, Indices, Factor, Subtarget,
                                Builder);

  return Grp.isSupported() && Grp.lowerIntoOptimizedSequence();
}
