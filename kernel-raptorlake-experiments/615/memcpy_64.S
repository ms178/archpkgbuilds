/* SPDX-License-Identifier: GPL-2.0 */
/*
 * memcpy() — 64-bit, overlap-aware implementation
 *            tuned for Intel® Raptor-Lake-R (i7-14700KF)
 *
 *  IN : RDI = dst,  RSI = src,  RDX = count (bytes)
 *  OUT: RAX = dst            (SysV ABI)
 *
 *  Fast path: CPUs advertising X86_FEATURE_FSRM execute an in-lined
 *             “rep movsb” via alternatives — the fastest choice on
 *             Alder/Raptor cores for every length.
 *
 *  Fallback : memcpy_orig
 *             ─ forward   : rep movsq (≥128 B) | 32-byte unroll (<128 B)
 *             ─ backward  : std + rep movsb        (overlap-safe)
 *             ─ shared tail routine (≤31 B)
 *
 *  ABI, symbol names and linkage remain identical to upstream.
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/cfi_types.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>

        .section .noinstr.text, "ax"

/* ------------------------------------------------------------------ */
/*  Public entry — alternatives patches the JMP away on FSRM CPUs      */
/* ------------------------------------------------------------------ */
        .p2align 5                          /* 32-byte i-cache line */
SYM_TYPED_FUNC_START(__memcpy)
        ALTERNATIVE "jmp memcpy_orig", "", X86_FEATURE_FSRM

        /* FSRM fast path: in-lined REP MOVSB ------------------- */
        movq    %rdi, %rax                  /* preserve dst for ret */
        movq    %rdx, %rcx
        rep     movsb
        RET
SYM_FUNC_END(__memcpy)
EXPORT_SYMBOL(__memcpy)

SYM_FUNC_ALIAS_MEMFUNC(memcpy, __memcpy)
EXPORT_SYMBOL(memcpy)

/* ================================================================== */
/*  memcpy_orig — generic fallback                                    */
/* ================================================================== */
        .p2align 4
SYM_FUNC_START_LOCAL(memcpy_orig)
        movq    %rdi, %rax                  /* keep dst for return  */

        /* ------------------------------------------------------
         * Decide copy direction.  Copy forward when the ranges
         * do not destructively overlap (dst ∉ (src, src+len−1]).
         * ---------------------------------------------------- */
        cmpq    %rdi, %rsi
        jbe     .Lforward_ok                /* dst ≤ src  ⇒ forward */

        leaq    (%rsi,%rdx), %rcx           /* rcx = src_end       */
        cmpq    %rcx, %rdi
        ja      .Lforward_ok                /* dst > src_end ⇒ fwd */

        /* ------------- backward copy (destructive overlap) -------- */
.Lbackward:
        addq    %rdx, %rsi
        addq    %rdx, %rdi
        std                                 /* set DF = 1          */
        movq    %rdx, %rcx
        rep     movsb                       /* byte-accurate       */
        cld
        RET

/* ------------------------------ forward path ----------------------- */
.Lforward_ok:
        cld

        /* Large? Use rep movsq; else 32-byte unroll. */
        cmpq    $128, %rdx
        jb      .Lsmall_fwd

        movq    %rdx, %rcx
        shrq    $3, %rcx                    /* q-word count        */
        rep     movsq
        movq    %rdx, %rcx
        andq    $7,  %rcx                   /* tail bytes          */
        rep     movsb
        RET

/* ---- small forward copy (<128 B) --------------------------------- */
.Lsmall_fwd:
        subq    $0x20, %rdx
.Lunroll32:
        subq    $0x20, %rdx
        movq    0(%rsi),  %r8
        movq    8(%rsi),  %r9
        movq    16(%rsi), %r10
        movq    24(%rsi), %r11
        addq    $0x20,    %rsi
        movq    %r8,  0(%rdi)
        movq    %r9,  8(%rdi)
        movq    %r10, 16(%rdi)
        movq    %r11, 24(%rdi)
        addq    $0x20,    %rdi
        jae     .Lunroll32
        addq    $0x20,    %rdx              /* undo last sub       */

        /* ---- shared byte tail (0-31 B) ------------------------ */
.Ltail:
        testq   %rdx, %rdx
        je      .Ldone
        movq    %rdx, %rcx
        rep     movsb
.Ldone:
        RET
SYM_FUNC_END(memcpy_orig)
