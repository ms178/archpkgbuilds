/* SPDX-License-Identifier: GPL-2.0 */
/*
 * copy_page - 4 KiB page copy
 *
 * Tuned for Intel Raptor Lake (i7-14700KF, 8P+8E)
 *  - Fast path: rep movsb via X86_FEATURE_REP_GOOD (FSRM/ERMS)
 *    -> 17c startup, then ~64 bytes/cycle, no FPU state, safe in any context
 *  - Legacy path: 64×64-byte unrolled, no stack ops, no software prefetch
 *    -> uses only caller-saved regs (rax, rdx, r8, r9), loads interleaved
 *       with stores to keep store buffer draining on Gracemont E-cores
 *  - DF=0 guaranteed by kernel entry; no cld needed (matches upstream)
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>

        .section .noinstr.text, "ax"

        .p2align 5
SYM_FUNC_START(copy_page)
        ANNOTATE_NOENDBR
        /* On 14700KF this jmp is patched to 5 NOPs at boot */
        ALTERNATIVE "jmp copy_page_regs", "", X86_FEATURE_REP_GOOD

        movl    $4096, %ecx
        rep     movsb
        RET
SYM_FUNC_END(copy_page)
EXPORT_SYMBOL(copy_page)

        /* -------------------------------------------------------------- */
        /* Legacy path for CPUs without REP_GOOD                          */
        /* -------------------------------------------------------------- */
        .p2align 6
SYM_FUNC_START_LOCAL(copy_page_regs)
        movl    $64, %ecx
1:
        movq    0(%rsi), %rax
        movq    8(%rsi), %rdx
        movq    16(%rsi), %r8
        movq    24(%rsi), %r9
        movq    %rax, 0(%rdi)
        movq    %rdx, 8(%rdi)
        movq    %r8, 16(%rdi)
        movq    %r9, 24(%rdi)

        movq    32(%rsi), %rax
        movq    40(%rsi), %rdx
        movq    48(%rsi), %r8
        movq    56(%rsi), %r9
        movq    %rax, 32(%rdi)
        movq    %rdx, 40(%rdi)
        movq    %r8, 48(%rdi)
        movq    %r9, 56(%rdi)

        addq    $64, %rsi
        addq    $64, %rdi
        decl    %ecx
        jnz     1b
        RET
SYM_FUNC_END(copy_page_regs)
