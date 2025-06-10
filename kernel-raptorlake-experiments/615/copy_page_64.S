/* SPDX-License-Identifier: GPL-2.0 */
#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/cfi_types.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>

/* Fast-string page copy – RL-tune revision 2 (feature flag fix)     */

        .p2align 5
SYM_TYPED_FUNC_START(copy_page)            /* public entry */
        ALTERNATIVE "jmp copy_page_regs", "", X86_FEATURE_REP_GOOD

        /* REP MOVSB (ERMS + FSRM) path -------------------------- */
        xorl    %eax, %eax
        movl    $4096, %ecx
        rep     movsb
        RET
SYM_FUNC_END(copy_page)
EXPORT_SYMBOL(copy_page)

        .p2align 4
SYM_FUNC_START_LOCAL(copy_page_regs)       /* fallback */
        pushq   %rbx
        CFI_PUSH(%rbx)
        pushq   %r12
        CFI_PUSH(%r12)

        movl    $64, %ecx                 /* 64 × 64 B */
.Lloop64:
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
        CFI_POP()
        popq    %rbx
        CFI_POP()
        RET
SYM_FUNC_END(copy_page_regs)
