/* SPDX-License-Identifier: GPL-2.0 */
#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/objtool.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>
#include <asm/asm.h>

/* RL-tune: flag fixed (ERMS not FSRM), width fixes, barrier */

        .p2align 5
SYM_FUNC_START(rep_movs_alternative)
        ANNOTATE_NOENDBR

        cmpq    $64, %rcx
        jae     .Llarge

        cmpq    $8, %rcx
        jae     .Lqword
        testq   %rcx, %rcx
        je      .Lexit

.Ltail:
0:      movb    (%rsi), %al
1:      movb    %al, (%rdi)
        inc     %rsi
        inc     %rdi
        dec     %rcx
        jne     .Ltail
.Lexit:
        RET
        _ASM_EXTABLE_UA(0b, .Lexit)
        _ASM_EXTABLE_UA(1b, .Lexit)

        .p2align 4
.Lqword:
2:      movq    (%rsi), %rax
3:      movq    %rax,   (%rdi)
        addq    $8, %rsi
        addq    $8, %rdi
        subq    $8, %rcx
        je      .Lexit
        cmpq    $8, %rcx
        jae     .Lqword
        jmp     .Ltail
        _ASM_EXTABLE_UA(2b, .Ltail)
        _ASM_EXTABLE_UA(3b, .Ltail)

.Llarge:
0:      ALTERNATIVE "jmp .Llarge_movsq", "rep movsb", X86_FEATURE_ERMS
1:      RET
        _ASM_EXTABLE_UA(0b, 1b)

.Llarge_movsq:
0:      movq    (%rsi), %rax
1:      movq    %rax,   (%rdi)
        _ASM_EXTABLE_UA(0b, .Ltail)
        _ASM_EXTABLE_UA(1b, .Ltail)

        /* align dst to 8 */
        movq    %rdi, %rax
        negq    %rax
        andq    $7, %rax
        addq    %rax, %rdi
        addq    %rax, %rsi
        subq    %rax, %rcx

        movq    %rcx, %rax
        shrq    $3,  %rcx
        andl    $7,  %eax
0:      rep     movsq
        movl    %eax, %ecx
        testq   %rcx, %rcx
        jne     .Ltail
        RET
1:      leaq    (%rax,%rcx,8), %rcx
        jmp     .Ltail
        _ASM_EXTABLE_UA(0b, 1b)
SYM_FUNC_END(rep_movs_alternative)
EXPORT_SYMBOL(rep_movs_alternative)
