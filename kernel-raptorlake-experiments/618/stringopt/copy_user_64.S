/* SPDX-License-Identifier: GPL-2.0 */
/*
 * rep_movs_alternative - fast user copy with precise exceptions
 *
 * Tuned for Intel Raptor Lake (i7-14700KF, 8P+8E, ERMS+FSRM)
 *  - Input:  rdi=dst, rsi=src, rcx=len  (rcx returned = bytes not copied)
 *  - Gaming workload: 70% of calls are 16-128B (amdgpu command buffers)
 *  - No FPU/SIMD: runs in atomic context, must not corrupt XMM/YMM
 *  - DF=0 on entry/exit (kernel ABI)
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/objtool.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>
#include <asm/asm.h>

        .p2align 5
SYM_FUNC_START(rep_movs_alternative)
        ANNOTATE_NOENDBR

        cmpq    $64, %rcx
        jae     .Llarge

        /* ---- 8..63 bytes: 2x unrolled qword loop ---- */
        cmpq    $8, %rcx
        jb      .Ltail

        .p2align 6
.Lqword2:
0:      movq    (%rsi), %rax
1:      movq    8(%rsi), %rdx
2:      movq    %rax, (%rdi)
3:      movq    %rdx, 8(%rdi)
        addq    $16, %rsi
        addq    $16, %rdi
        subq    $16, %rcx
        cmpq    $16, %rcx
        jae     .Lqword2

        /* handle remaining 8..15 */
        cmpq    $8, %rcx
        jb      .Ltail
4:      movq    (%rsi), %rax
5:      movq    %rax, (%rdi)
        addq    $8, %rsi
        addq    $8, %rdi
        subq    $8, %rcx

        /* ---- 0..7 bytes ---- */
.Ltail:
        testq   %rcx, %rcx
        jz      .Lexit
6:      movb    (%rsi), %al
7:      movb    %al, (%rdi)
        incq    %rsi
        incq    %rdi
        decq    %rcx
        jne     6b
.Lexit:
        RET

        _ASM_EXTABLE_UA(0b, .Ltail)
        _ASM_EXTABLE_UA(1b, .Ltail)
        _ASM_EXTABLE_UA(2b, .Ltail)
        _ASM_EXTABLE_UA(3b, .Ltail)
        _ASM_EXTABLE_UA(4b, .Ltail)
        _ASM_EXTABLE_UA(5b, .Ltail)
        _ASM_EXTABLE_UA(6b, .Lexit)
        _ASM_EXTABLE_UA(7b, .Lexit)

        /* ---- >=64 bytes ---- */
        .p2align 6
.Llarge:
        /* On 14700KF this is patched to 'rep movsb' at boot */
0:      ALTERNATIVE "jmp .Llarge_movsq", "rep movsb", X86_FEATURE_ERMS
1:      RET
        _ASM_EXTABLE_UA(0b, 1b)

.Llarge_movsq:
        /* Copy first qword (may be unaligned), then align dst */
0:      movq    (%rsi), %rax
1:      movq    %rax, (%rdi)
        _ASM_EXTABLE_UA(0b, .Ltail)
        _ASM_EXTABLE_UA(1b, .Ltail)

        addq    $8, %rsi
        addq    $8, %rdi
        subq    $8, %rcx

        /* Align destination to 8 bytes for rep movsq */
        movq    %rdi, %rax
        negq    %rax
        andq    $7, %rax
        addq    %rax, %rdi
        addq    %rax, %rsi
        subq    %rax, %rcx

        movq    %rcx, %rdx
        shrq    $3, %rcx
        andl    $7, %edx
0:      rep movsq
        movl    %edx, %ecx
        testq   %rcx, %rcx
        jne     .Ltail
        RET
1:      leaq    (%rdx,%rcx,8), %rcx
        jmp     .Ltail
        _ASM_EXTABLE_UA(0b, 1b)

SYM_FUNC_END(rep_movs_alternative)
EXPORT_SYMBOL(rep_movs_alternative)
