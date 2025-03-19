/* SPDX-License-Identifier: GPL-2.0 */
/* Written 2003 by Andi Kleen, based on a kernel by Evandro Menezes */
/* Optimized for Intel Raptor Lake using Intel optimization guidelines */

#include <linux/export.h>
#include <linux/linkage.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>

/*
 * Optimized page copy implementation for Raptor Lake:
 * 1. REP MOVSQ path (efficient on modern Intel CPUs)
 * 2. Standard register-based fallback with optimized prefetching
 */
        ALIGN
SYM_FUNC_START(copy_page)
        ALTERNATIVE "jmp copy_page_regs", "", X86_FEATURE_REP_GOOD
        movl    $4096/8, %ecx
        cld             /* Keep CLD for safety despite warning */
        rep     movsq
        RET
SYM_FUNC_END(copy_page)
EXPORT_SYMBOL(copy_page)

/*
 * Optimized register-based implementation for CPUs without REP_GOOD
 */
        .p2align 4
SYM_FUNC_START_LOCAL(copy_page_regs)
        /* Setup proper stack frame */
        pushq   %rbp
        movq    %rsp, %rbp

        /* Save preserved registers */
        pushq   %rbx
        pushq   %r12

        /* Process 4096 bytes in 64-byte chunks */
        movl    $4096/64, %ecx
        .p2align 4
.Loop64:
        /* Prefetch optimized for Raptor Lake's improved prefetchers */
        prefetcht0      6*64(%rsi)    /* ~384 bytes ahead - better for Raptor Lake */

        /* Load and store 64 bytes into registers, interleaved for ILP */
        movq    0*8(%rsi), %rax
        movq    1*8(%rsi), %rbx
        movq    2*8(%rsi), %rdx
        movq    3*8(%rsi), %r8
        movq    4*8(%rsi), %r9
        movq    5*8(%rsi), %r10
        movq    6*8(%rsi), %r11
        movq    7*8(%rsi), %r12

        movq    %rax, 0*8(%rdi)
        movq    %rbx, 1*8(%rdi)
        movq    %rdx, 2*8(%rdi)
        movq    %r8, 3*8(%rdi)
        movq    %r9, 4*8(%rdi)
        movq    %r10, 5*8(%rdi)
        movq    %r11, 6*8(%rdi)
        movq    %r12, 7*8(%rdi)

        /* Update pointers */
        leaq    64(%rsi), %rsi
        leaq    64(%rdi), %rdi

        /* Loop control */
        decl    %ecx
        jnz     .Loop64

        /* Restore preserved registers - must be in reverse order */
        popq    %r12
        popq    %rbx

        /* Restore stack frame */
        popq    %rbp
        RET
SYM_FUNC_END(copy_page_regs)
