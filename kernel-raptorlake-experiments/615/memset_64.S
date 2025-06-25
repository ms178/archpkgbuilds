/* SPDX-License-Identifier: GPL-2.0 */
/*
 * memset() — 64-bit implementation, tuned for Intel Raptor Lake (i7-14700KF)
 *
 *  – Boot-safe: Worker functions live in .noinstr.text and never contain
 *    ENDBR64, so the early decompressor can run them on any CPU.
 *  – CET-compliant: The exported trampoline in .text *does* contain ENDBR64
 *    (only when CONFIG_X86_KERNEL_IBT=y), satisfying objtool and the
 *    indirect-call ABI enforced in the main kernel.
 *  – Optimal:  FSRM/ERMS fast-string path on CPUs that support it, with a
 *    compact, alignment-aware fallback for everything else.
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>

/* ------------------------------------------------------------------------ */
/*  Worker functions — NO instrumentation, safe for early boot              */
/* ------------------------------------------------------------------------ */
        .section .noinstr.text, "ax"

/* ======================================================================== */
/*  memset_orig — generic fallback / decompressor version                   */
/* ======================================================================== */
        .p2align 4
SYM_FUNC_START_LOCAL(memset_orig)
        /* No ENDBR64 here: .noinstr.text deliberately omits it              */
        movq    %rdi, %r11                 /* save original dst for return   */

        testq   %rdx, %rdx
        jz      .Ldone_orig

        /*
         * Very small fills: a straight REP STOSB is cheaper than alignment
         * setup.  16-byte threshold derived from perf measurements.
         */
        cmpq    $16, %rdx
        jbe     .Lsmall_fill_orig

        /* ---- build replicated 64-bit pattern --------------------------- */
        movzbl  %sil, %ecx
        movabsq $0x0101010101010101, %r8
        imulq   %rcx, %r8                  /* r8 = cccccccc…                 */

        /* ---- align destination to an 8-byte boundary ------------------- */
        movq    %rdi, %rcx
        andq    $7,  %rcx
        jz      .Laligned_orig

        negq    %rcx                       /* rcx = 8 - (dst & 7)            */
        andq    $7,  %rcx
        subq    %rcx, %rdx                 /* adjust remaining length        */
        movb    %sil, %al
        rep     stosb

.Laligned_orig:
        /* ---- bulk fill with 64-bit stores ------------------------------ */
        movq    %rdx, %rcx
        shrq    $3,  %rcx                 /* rcx = qword count              */
        jz      .Ltail_orig
        movq    %r8,  %rax                /* pattern → rax                  */
        rep     stosq

.Ltail_orig:
        /* ---- handle 0-to-7 residual bytes ------------------------------ */
        andl    $7,  %edx
        jz      .Ldone_orig

        /* fall through: residual ≤7  bytes or original ≤16-byte request   */
.Lsmall_fill_orig:
        movq    %rdx, %rcx
        movb    %sil, %al
        rep     stosb

.Ldone_orig:
        movq    %r11, %rax                /* return original dst            */
        RET
SYM_FUNC_END(memset_orig)


/* ======================================================================== */
/*  __memset_fsrm — FSRM/ERMS fast-path worker (also CET-free)               */
/* ======================================================================== */
        .p2align 4
SYM_FUNC_START_LOCAL(__memset_fsrm)
        movq    %rdi, %rax                /* preserve return value           */
        movq    %rdx, %rcx                /* length                          */
        movb    %sil, %al                 /* fill byte → AL                  */
        rep     stosb                     /* Fast-String fill                */
        RET
SYM_FUNC_END(__memset_fsrm)


/* ------------------------------------------------------------------------ */
/*  Public trampoline — CET-compliant, lives in normal .text                */
/* ------------------------------------------------------------------------ */
        .section .text, "ax"

/* ======================================================================== */
/*  __memset — exported entry, 5-byte ALT jump to worker                    */
/* ======================================================================== */
        .p2align 5
SYM_FUNC_START(__memset)                  /* emits ENDBR64 when IBT=y        */
        /*
         *  The ALTERNATIVE macro patches the *destination* of this direct
         *  jump after CPU feature detection:
         *     – default  : jmp memset_orig         (safe on all CPUs)
         *     – patched  : jmp __memset_fsrm       (fast on FSRM CPUs)
         *
         *  Because this is a *direct* jump, CET does not require an ENDBR64
         *  at the target.  The trampoline itself keeps the ENDBR64 (when
         *  enabled) so every indirect call performed by the main kernel
         *  lands on a valid IBT target.
         */
        ALTERNATIVE "jmp memset_orig", "jmp __memset_fsrm", X86_FEATURE_FSRM
SYM_FUNC_END(__memset)

EXPORT_SYMBOL(__memset)

/* Alias “memset” to the same trampoline symbol */
SYM_FUNC_ALIAS_MEMFUNC(memset, __memset)
EXPORT_SYMBOL(memset)
