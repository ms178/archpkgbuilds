/* SPDX-License-Identifier: GPL-2.0 */
/*
 * memset() – tuned for Intel® Raptor-Lake-R (i7-14700KF)
 *
 *   rdi = dst
 *   rsi = byte value (low-8 bits)
 *   rdx = count (bytes)
 *
 *   rax = dst    (SysV ABI)
 *
 * Fast-string path
 * ----------------
 *   If the CPU advertises FSRS (Fast-Short-REP-STOS) the alternatives
 *   framework removes a 5-byte JMP and we fall directly into a plain
 *   ‘rep stosb’.  That is the fastest variant on Alder/Raptor cores.
 *
 * Fallback (memset_orig)
 * ----------------------
 *   • Byte-align dst to 8 B, fill head.
 *   • Use rep stosq for the bulk, rep stosb for the tail.
 *   • 4×8-byte unroll for <128 B copies to hide REP start-up on
 *     very small buffers.
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/cfi_types.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>

        .section .noinstr.text,"ax"

/* =================================================================
 *  Public entry – alternatives replaces JMP with NOPs on FSRS CPUs
 * ============================================================== */
        .p2align 5
SYM_TYPED_FUNC_START(__memset)
        /* RL-fix: FSRS → FSRM */
        ALTERNATIVE "jmp memset_orig", "", X86_FEATURE_FSRM

        movq    %rdi, %rax
        movzx   %sil, %eax
        movq    %rdx, %rcx
        rep     stosb
        RET
SYM_FUNC_END(__memset)
EXPORT_SYMBOL(__memset)

SYM_FUNC_ALIAS_MEMFUNC(memset, __memset)
EXPORT_SYMBOL(memset)

/* =================================================================
 *  Fallback – for CPUs without FSRS
 * ============================================================== */
        .p2align 4
SYM_FUNC_START_LOCAL(memset_orig)
        /* ---------- save return pointer & build 64-bit pattern -------- */
        movq    %rdi, %r11            /* save dst for return          */
        movzx   %sil, %eax            /* AL = fill byte               */
        movabs  $0x0101010101010101, %rcx
        imulq   %rcx, %rax            /* RAX = 8×byte pattern         */

        /* ---------- <8 B ? → byte tail only --------------------------- */
        cmpq    $8, %rdx
        jb      .Ltail_bytes

        /* ---------- align destination to 8 B -------------------------- */
        movq    %rdi, %rcx
        andq    $7, %rcx              /* rcx = misalignment (0-7)      */
        jz      .Laligned

        mov     %rcx, %r8             /* r8  = head count             */
        movq    %r8,  %rcx
        rep     stosb                 /* fill head one byte at a time */
        subq    %r8,  %rdx            /* adjust remaining             */

.Laligned:
        /* ---------- choose bulk strategy ----------------------------- */
        cmpq    $128, %rdx
        jb      .Lsmall_bulk

        /* ---- large: rep stosq -------------------------------------- */
        movq    %rdx, %rcx
        shrq    $3,  %rcx             /* q-word count                 */
        rep     stosq
        movq    %rdx, %rcx
        andq    $7, %rcx              /* tail 0-7                     */
        rep     stosb
        jmp     .Ldone

/* ---------- <128 B forward – 4×8-byte unroll --------------------- */
.Lsmall_bulk:
        subq    $0x20, %rdx
.Lunroll32:
        subq    $0x20, %rdx
        movq    %rax,   0(%rdi)
        movq    %rax,   8(%rdi)
        movq    %rax,  16(%rdi)
        movq    %rax,  24(%rdi)
        addq    $0x20, %rdi
        jae     .Lunroll32
        addq    $0x20, %rdx           /* fix over-step */

.Ltail_bytes:
        testq   %rdx, %rdx
        je      .Ldone
        movq    %rdx, %rcx
        rep     stosb

.Ldone:
        movq    %r11, %rax            /* return original dst */
        RET
SYM_FUNC_END(memset_orig)
