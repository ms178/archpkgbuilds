/* SPDX-License-Identifier: GPL-2.0 */
/*
 * memcpy() – tuned for Intel® Raptor-Lake-R (i7-14700KF)
 *
 *  rdi = dst
 *  rsi = src
 *  rdx = count
 *
 *  rax = dst  (SysV ABI)
 *
 *  On CPUs with FSRM (Fast-Short-REP-Movs) we execute a plain
 *     rep movsb
 *  via alternatives.  On older parts we fall back to memcpy_orig,
 *  now modernised to use rep movsq in both directions.
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/cfi_types.h>
#include <asm/errno.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>

        .section .noinstr.text,"ax"

/* ==============================================================
 *  Public entry – alternatives patches out the JMP on FSRM parts
 * ============================================================== */
        .p2align 5
SYM_TYPED_FUNC_START(__memcpy)
        /* RL-fix: flag was FSRS → FSRM */
        ALTERNATIVE "jmp memcpy_orig", "", X86_FEATURE_FSRM

        movq    %rdi, %rax
        movq    %rdx, %rcx
        rep     movsb
        RET
SYM_FUNC_END(__memcpy)
EXPORT_SYMBOL(__memcpy)

SYM_FUNC_ALIAS_MEMFUNC(memcpy, __memcpy)
EXPORT_SYMBOL(memcpy)

/* ==============================================================
 *  memcpy_orig – fallback when FSRM unavailable
 * ============================================================== */
        .p2align 4
SYM_FUNC_START_LOCAL(memcpy_orig)
        movq    %rdi, %rax            /* preserve dst for retval */

        /* ------------------------------------------------------
         * Decide copy direction to avoid destructive overlap.
         * (kernel memcpy _does not_ guarantee memmove semantics
         *  but we keep the legacy behaviour.)
         * ---------------------------------------------------- */
        cmpq    $0x20, %rdx
        jb      .Lhandle_tail

        lea     -1(%rdi,%rdx), %r8    /* dst_end */
        lea     -1(%rsi,%rdx), %r9    /* src_end */

        /* dst < src || dst_end < src  → safe forward copy */
        cmpq    %rsi, %rdi
        jb      .Lforward
        cmpq    %r9, %rdi
        jbe     .Lforward

/* ----------------------------------------------------------------
 *  Backward copy  (overlap & dst>src)
 * -------------------------------------------------------------- */
.Lbackward:
        std                         /* set DF=1, count down   */
        movq    %rdx, %rcx
        rep     movsq               /* move RCX q-words       */
        cld                         /* restore DF=0           */
        jmp     .Ldone

/* ----------------------------------------------------------------
 *  Forward copy  (no harmful overlap)
 * -------------------------------------------------------------- */
.Lforward:
        cld                         /* ensure DF=0            */

        /* For large sizes use rep movsq, otherwise tiny-loop */
        cmpq    $128, %rdx
        jb      .Ltiny_fwd

        /* RCX = q-words, RDX = bytes%8  */
        movq    %rdx, %rcx
        shrq    $3,  %rcx
        rep     movsq
        movq    %rdx, %rcx           /* low 3 bits still in RDX */
        andq    $7,  %rcx
        rep     movsb
        jmp     .Ldone

/* --------- old 4×8-byte unroll kept for <128 B forward copies ---- */
.Ltiny_fwd:
        subq    $0x20, %rdx
.Lfwd_loop:
        subq    $0x20, %rdx
        movq    0(%rsi),  %r8
        movq    8(%rsi),  %r9
        movq    16(%rsi), %r10
        movq    24(%rsi), %r11
        addq    $0x20,  %rsi
        movq    %r8,  0(%rdi)
        movq    %r9,  8(%rdi)
        movq    %r10, 16(%rdi)
        movq    %r11, 24(%rdi)
        addq    $0x20,  %rdi
        jae     .Lfwd_loop
        addq    $0x20,  %rdx         /* correct over-step      */
/* -------------- fall into tail ------------------------------- */

/* =================================================================
 *  Handle <32-byte tails for either direction
 * ============================================================== */
.Lhandle_tail:
        cmpq    $16, %rdx
        jb      .Ltail_lt16

        /* 16 … 31 bytes */
        movq    (%rsi),      %r8
        movq    8(%rsi),     %r9
        movq    -16(%rsi,%rdx), %r10
        movq    -8(%rsi,%rdx),  %r11
        movq    %r8,  (%rdi)
        movq    %r9,  8(%rdi)
        movq    %r10, -16(%rdi,%rdx)
        movq    %r11, -8(%rdi,%rdx)
        jmp     .Ldone

.Ltail_lt16:
        cmpq    $8, %rdx
        jb      .Ltail_lt8

        /* 8 … 15 bytes */
        movq    (%rsi), %r8
        movq    -8(%rsi,%rdx), %r9
        movq    %r8, (%rdi)
        movq    %r9, -8(%rdi,%rdx)
        jmp     .Ldone

.Ltail_lt8:
        cmpq    $4, %rdx
        jb      .Ltail_lt4

        /* 4 … 7 bytes */
        movl    (%rsi), %ecx
        movl    -4(%rsi,%rdx), %r8d
        movl    %ecx, (%rdi)
        movl    %r8d, -4(%rdi,%rdx)
        jmp     .Ldone

.Ltail_lt4:
        testq   %rdx, %rdx
        je      .Ldone
        mov     %rdx, %rcx
        rep     movsb

.Ldone:
        RET
SYM_FUNC_END(memcpy_orig)
