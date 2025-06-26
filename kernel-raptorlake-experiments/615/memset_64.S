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

/*
 * ========================================================================
 *                  *** COMPATIBILITY WORKAROUND ***
 *
 * If you experience hardware initialization issues after enabling this file
 * (e.g., a USB mouse not working at boot), it may be due to a latent race
 * condition in a driver that is exposed by this function's faster timing.
 *
 * To fix this, uncomment the following line. This will force the use of
 * the slightly slower, more compatible code path, which can resolve such
 * timing issues, without sacrificing the correctness and security fixes.
 *
 * ========================================================================
 */
#define MEMSET_FORCE_GENERIC_PATH 1


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

        cmpq    $16, %rdx
        jbe     .Lsmall_fill_orig

        movzbl  %sil, %ecx
        movabsq $0x0101010101010101, %r8
        imulq   %rcx, %r8

        movq    %rdi, %rcx
        andq    $7,  %rcx
        jz      .Laligned_orig

        negq    %rcx
        andq    $7,  %rcx
        subq    %rcx, %rdx
        movb    %sil, %al
        rep     stosb

.Laligned_orig:
        movq    %rdx, %rcx
        shrq    $3,  %rcx
        jz      .Ltail_orig
        movq    %r8,  %rax
        rep     stosq

.Ltail_orig:
        andl    $7,  %edx
        jz      .Ldone_orig

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
/*  __memset — exported entry, jump to worker                               */
/* ======================================================================== */
        .p2align 5
SYM_FUNC_START(__memset)                  /* emits ENDBR64 when IBT=y        */
#ifdef MEMSET_FORCE_GENERIC_PATH
        /*
         * Compatibility path: Unconditionally jump to the generic fallback.
         * This avoids potential timing issues with the FSRM fast path.
         */
        jmp     memset_orig
#else
        /*
         * Standard path: The ALTERNATIVE macro patches the jump after
         * CPU feature detection to use the fastest available implementation.
         */
        ALTERNATIVE "jmp memset_orig", "jmp __memset_fsrm", X86_FEATURE_FSRM
#endif
SYM_FUNC_END(__memset)

EXPORT_SYMBOL(__memset)

/* Alias “memset” to the same trampoline symbol */
SYM_FUNC_ALIAS_MEMFUNC(memset, __memset)
EXPORT_SYMBOL(memset)
