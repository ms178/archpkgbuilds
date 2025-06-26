/* SPDX-License-Identifier: GPL-2.0 */
/*
 *  Fast 4-KiB page copy, tuned for Intel® Core i7-14700KF (Raptor-Lake-R)
 *
 *  – Uses ‘rep movsb’ on REP_GOOD parts, otherwise a 64×64-byte
 *    hand-unrolled loop.
 *  – No extra CLD: DF is already guaranteed clear; a redundant CLD would
 *    trigger an objtool warning.
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>

        .section .noinstr.text, "ax"

        .p2align 5
SYM_FUNC_START(copy_page)
        /* On REP_GOOD CPUs the next instruction is patched to NOPs,
         * falling through into the rep-movsb fast path.  Otherwise we
         * branch to the legacy implementation below.
         */
        ALTERNATIVE "jmp copy_page_regs", "", X86_FEATURE_REP_GOOD

        movl    $4096, %ecx          /* ECX = 4096 bytes              */
        rep     movsb
        RET
SYM_FUNC_END(copy_page)
EXPORT_SYMBOL(copy_page)

        /* -------------------------------------------------------------- */
        /* Legacy path: 64×64-byte unrolled copy                          */
        /* -------------------------------------------------------------- */
        .p2align 4
SYM_FUNC_START_LOCAL(copy_page_regs)
        pushq   %rbx
        pushq   %r12

        movl    $64, %ecx
.Lloop64:
        prefetcht0 128(%rsi)         /* harmless on modern cores       */

        movq    0(%rsi),  %rax
        movq    8(%rsi),  %rbx
        movq    16(%rsi), %rdx
        movq    24(%rsi), %r8
        movq    32(%rsi), %r9
        movq    40(%rsi), %r10
        movq    48(%rsi), %r11
        movq    56(%rsi), %r12

        movq    %rax,  0(%rdi)
        movq    %rbx,  8(%rdi)
        movq    %rdx,  16(%rdi)
        movq    %r8,   24(%rdi)
        movq    %r9,   32(%rdi)
        movq    %r10,  40(%rdi)
        movq    %r11,  48(%rdi)
        movq    %r12,  56(%rdi)

        addq    $64, %rsi
        addq    $64, %rdi
        dec     %ecx
        jne     .Lloop64

        popq    %r12
        popq    %rbx
        RET
SYM_FUNC_END(copy_page_regs)
