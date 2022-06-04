#!/bin/bash

export TOPLEV=~/toolchain/gcc
mkdir ${TOPLEV}
cd ${TOPLEV}

mkdir -p ${TOPLEV}/bolt-gcc/intrumentdata
mkdir -p ${TOPLEV}/bolt-gcc/intrumentdata/cc1
mkdir -p ${TOPLEV}/bolt-gcc/intrumentdata/cc1plus
cd ${TOPLEV}/bolt-gcc
GCCPATH=/usr/lib/gcc/x86_64-pc-linux-gnu/13
GCCINSTPATH=${TOPLEV}/bolt-gcc
BOLTPATH=~/toolchain/llvm/stage1/bin


echo "Instrument clang with llvm-bolt"

${BOLTPATH}/llvm-bolt \
    --instrument \
    --instrumentation-file-append-pid \
    --instrumentation-file=${TOPLEV}/bolt-gcc/intrumentdata/cc1/cc1.fdata \
    ${GCCPATH}/cc1 \
    -o ${GCCINSTPATH}/cc1.inst

echo "mooving instrumented binary"
sudo mv ${GCCPATH}/cc1 ${GCCPATH}/cc1.org
sudo mv ${GCCINSTPATH}/cc1.inst ${GCCPATH}/cc1

${BOLTPATH}/llvm-bolt \
    --instrument \
    --instrumentation-file-append-pid \
    --instrumentation-file=${TOPLEV}/bolt-gcc/intrumentdata/cc1plus/cc1plus.fdata \
    ${GCCPATH}/cc1plus \
    -o ${GCCINSTPATH}/cc1plus.inst

sudo mv ${GCCPATH}/cc1plus ${GCCPATH}/cc1plus.org
sudo mv ${GCCINSTPATH}/cc1plus.inst ${GCCPATH}/cc1plus

echo "now do some instrument compiles for example compiling a kernel or GCC"
