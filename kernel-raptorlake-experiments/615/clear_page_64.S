/* SPDX-License-Identifier: GPL-2.0-only */
#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/cfi_types.h>
#include <linux/objtool.h>
#include <asm/asm.h>

/* ----------------------------------------------------------------- */
/* ERMS – fastest on Alder/Raptor-Lake                               */
/* ----------------------------------------------------------------- */
        .p2align 5                         /* RL-tune: cache-line    */
SYM_TYPED_FUNC_START(clear_page_erms)
        xorl    %eax, %eax
        movl    $4096, %ecx
        rep     stosb
        RET
SYM_FUNC_END(clear_page_erms)
EXPORT_SYMBOL_GPL(clear_page_erms)

/* ----------------------------------------------------------------- */
SYM_TYPED_FUNC_START(clear_page_rep)       /* legacy REP STOSQ        */
        xorl    %eax, %eax
        movl    $4096/8, %ecx              /* 512 q-words            */
        rep     stosq
        RET
SYM_FUNC_END(clear_page_rep)
EXPORT_SYMBOL_GPL(clear_page_rep)

/* ----------------------------------------------------------------- */
SYM_TYPED_FUNC_START(clear_page_orig)      /* hand-unrolled fallback  */
        xorl    %eax, %eax
        movl    $4096/64, %ecx             /* 64 × 64 B              */
.Lloop:
        movq    %rax,   0(%rdi)
        movq    %rax,   8(%rdi)
        movq    %rax,  16(%rdi)
        movq    %rax,  24(%rdi)
        movq    %rax,  32(%rdi)
        movq    %rax,  40(%rdi)
        movq    %rax,  48(%rdi)
        movq    %rax,  56(%rdi)
        addq    $64,   %rdi
        decl    %ecx
        jne     .Lloop
        RET
SYM_FUNC_END(clear_page_orig)
EXPORT_SYMBOL_GPL(clear_page_orig)

/* ==================================================================
 * rep_stos_alternative  —  clear_user() with fault accounting
 *   in : RDI = dst , RCX = count , RAX = 0
 *   out: RCX = 0 on success | remaining bytes on fault
 * ================================================================= */
SYM_FUNC_START(rep_stos_alternative)
        ANNOTATE_NOENDBR

        cmpq    $64, %rcx
        jae     .Lbulk64

        cmpq    $8, %rcx
        jae     .Lqword

        testq   %rcx, %rcx
        je      .Lexit
/* --- sub-8 B tail ---------------------------------------------- */
.Ltail:
0:      movb    %al, (%rdi)
        inc     %rdi
        dec     %rcx
        jne     .Ltail
.Lexit:
        RET
        _ASM_EXTABLE_UA(0b, .Lexit)

/* --- 8-B loop --------------------------------------------------- */
.Lqword:
1:      movq    %rax, (%rdi)
        addq    $8, %rdi
        subq    $8, %rcx
        je      .Lexit
        cmpq    $8, %rcx
        jae     .Lqword
        jmp     .Ltail
        _ASM_EXTABLE_UA(1b, .Ltail)

/* --- 64-B unrolled loop ---------------------------------------- */
        .p2align 4
.Lbulk64:
10:     movq    %rax,   0(%rdi)
11:     movq    %rax,   8(%rdi)
12:     movq    %rax,  16(%rdi)
13:     movq    %rax,  24(%rdi)
14:     movq    %rax,  32(%rdi)
15:     movq    %rax,  40(%rdi)
16:     movq    %rax,  48(%rdi)
17:     movq    %rax,  56(%rdi)
        addq    $64, %rdi
        subq    $64, %rcx
        cmpq    $64, %rcx
        jae     .Lbulk64
        cmpq    $8, %rcx
        jae     .Lqword
        testq   %rcx, %rcx
        jne     .Ltail
        RET

        _ASM_EXTABLE_UA(10b, .Ltail)
        _ASM_EXTABLE_UA(11b, .Ltail)
        _ASM_EXTABLE_UA(12b, .Ltail)
        _ASM_EXTABLE_UA(13b, .Ltail)
        _ASM_EXTABLE_UA(14b, .Ltail)
        _ASM_EXTABLE_UA(15b, .Ltail)
        _ASM_EXTABLE_UA(16b, .Ltail)
        _ASM_EXTABLE_UA(17b, .Ltail)
SYM_FUNC_END(rep_stos_alternative)
EXPORT_SYMBOL(rep_stos_alternative)
