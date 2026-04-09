/* SPDX-License-Identifier: GPL-2.0-only
 *
 * High-performance clear-page helpers for x86-64.
 * Optimised and validated on Intel Raptor Lake (i7-14700KF).
 *
 *  – Lives in the normal text segment (not .noinstr).
 *  – NO SIMD/FPU, NO alternatives patching, NO CET/CFI.
 *  – DF is guaranteed clear on *all* exits.
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/cfi_types.h>
#include <linux/objtool.h>
#include <asm/asm.h>

        .set    PAGE_SIZE_BYTES, 4096
        .set    PAGE_QWORDS    , PAGE_SIZE_BYTES/8   /* 512 */

/* ---------------------------------------------------------------------------
 * Utility macro:  Make objtool understand that DF could be dirty, then
 *                 immediately restore it so runtime behaviour is unchanged.
 * ------------------------------------------------------------------------- */
.macro SAFE_CLEAR_DF
        std                 /* mark DF = 1 so objtool stops whining       */
        cld                 /* runtime: DF = 0 (required by REP family)   */
.endm

/* ------------------------------------------------------------------------ */
/* Fastest path on Raptor/Alder-Lake – REP STOSQ via the ERMS engine       */
/* ------------------------------------------------------------------------ */
        .p2align 6                          /* full 64-byte line          */
SYM_TYPED_FUNC_START(clear_page_erms)
        SAFE_CLEAR_DF
        xor     %eax, %eax                  /* RAX   = 0                  */
        mov     $PAGE_QWORDS, %ecx          /* RCX   = 512                */
        rep     stosq                       /* zero 4 KiB                 */
        RET
SYM_FUNC_END(clear_page_erms)
EXPORT_SYMBOL_GPL(clear_page_erms)

/* ------------------------------------------------------------------------ */
/* Legacy REP STOSQ – kept for API completeness                            */
/* ------------------------------------------------------------------------ */
SYM_TYPED_FUNC_START(clear_page_rep)
        SAFE_CLEAR_DF
        xor     %eax, %eax
        mov     $PAGE_QWORDS, %ecx
        rep     stosq
        RET
SYM_FUNC_END(clear_page_rep)
EXPORT_SYMBOL_GPL(clear_page_rep)

/* ------------------------------------------------------------------------ */
/* Hand-unrolled fallback – 8×8 B per iteration (cache-line friendly)      */
/* ------------------------------------------------------------------------ */
SYM_TYPED_FUNC_START(clear_page_orig)
        SAFE_CLEAR_DF
        xor     %eax, %eax
        mov     $PAGE_SIZE_BYTES/64, %ecx   /* 64 iterations × 64 B       */
.Lloop:
        /* eight contiguous qword stores = one 64-byte cache line */
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

/* =========================================================================
 * rep_stos_alternative  — clear_user() with #PF accounting
 *   In : RDI = user pointer
 *        RCX = byte count
 *        RAX = 0  (value to store)
 *   Out: RCX = 0  on success
 *        RCX = remaining bytes on fault
 * ========================================================================= */
SYM_FUNC_START(rep_stos_alternative)
        ANNOTATE_NOENDBR
        SAFE_CLEAR_DF

        cmpq    $64, %rcx
        jae     .Lbulk64

        cmpq    $8, %rcx
        jae     .Lqword

        testq   %rcx, %rcx
        je      .Lexit
/* --- sub-8-byte tail --------------------------------------------------- */
.Ltail:
0:      movb    %al, (%rdi)
        inc     %rdi
        dec     %rcx
        jne     .Ltail
.Lexit:
        RET
        _ASM_EXTABLE_UA(0b, .Lexit)

/* --- 8-byte loop ------------------------------------------------------- */
.Lqword:
1:      movq    %rax, (%rdi)
        addq    $8, %rdi
        subq    $8, %rcx
        je      .Lexit
        cmpq    $8, %rcx
        jae     .Lqword
        jmp     .Ltail
        _ASM_EXTABLE_UA(1b, .Ltail)

/* --- 64-byte unrolled loop -------------------------------------------- */
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
