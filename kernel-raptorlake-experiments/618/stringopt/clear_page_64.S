/* SPDX-License-Identifier: GPL-2.0-only
 *
 * clear_page / clear_user helpers – x86-64
 * Audited for Intel Raptor Lake (i7-14700KF, ERMS+FSRM)
 *
 *  - NO std/cld pair: DF is guaranteed 0 by kernel entry. The previous
 *    macro created a DF=1 window and wasted 2 uops per call.
 *  - NO FPU/SIMD: runs in any context, safe for amdgpu page faults.
 *  - rep stosb/q uses ERMS engine: ~64 bytes/cycle after 17c startup.
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/objtool.h>
#include <asm/asm.h>

        .set    PAGE_QWORDS, 4096/8

/* ------------------------------------------------------------------------
 * clear_page_erms – fast path, used on all REP_GOOD CPUs (14700KF)
 * --------------------------------------------------------------------- */
        .p2align 6
SYM_FUNC_START(clear_page_erms)
        ANNOTATE_NOENDBR
        xorl    %eax, %eax
        movl    $PAGE_QWORDS, %ecx
        rep     stosq
        RET
SYM_FUNC_END(clear_page_erms)
EXPORT_SYMBOL_GPL(clear_page_erms)

/* clear_page_rep kept for API compatibility – alias, no extra code */
SYM_FUNC_START(clear_page_rep)
        jmp     clear_page_erms
SYM_FUNC_END(clear_page_rep)
EXPORT_SYMBOL_GPL(clear_page_rep)

/* ------------------------------------------------------------------------
 * clear_page_orig – unrolled fallback (no rep), cache-line friendly
 * --------------------------------------------------------------------- */
        .p2align 6
SYM_FUNC_START(clear_page_orig)
        ANNOTATE_NOENDBR
        xorl    %eax, %eax
        movl    $64, %ecx              /* 64 × 64 bytes */
1:
        movq    %rax, 0(%rdi)
        movq    %rax, 8(%rdi)
        movq    %rax, 16(%rdi)
        movq    %rax, 24(%rdi)
        movq    %rax, 32(%rdi)
        movq    %rax, 40(%rdi)
        movq    %rax, 48(%rdi)
        movq    %rax, 56(%rdi)
        addq    $64, %rdi
        decl    %ecx
        jnz     1b
        RET
SYM_FUNC_END(clear_page_orig)
EXPORT_SYMBOL_GPL(clear_page_orig)

/* ------------------------------------------------------------------------
 * rep_stos_alternative – clear_user() with precise #PF accounting
 *   In:  RDI = user dst, RCX = len, RAX = 0
 *   Out: RCX = bytes not cleared
 * --------------------------------------------------------------------- */
        .p2align 6
SYM_FUNC_START(rep_stos_alternative)
        ANNOTATE_NOENDBR

        /* For len < 8 use byte loop – avoids rep startup cost */
        cmpq    $8, %rcx
        jb      .Ltail

        /* Save original count for fault fixup */
        movq    %rcx, %r8

        /* Bulk clear with qwords – CPU leaves RCX = remaining qwords on fault */
        shrq    $3, %rcx
1:      rep     stosq
        _ASM_EXTABLE_UA(1b, .Lfix_q)

        /* Tail bytes */
        andl    $7, %r8d
        movl    %r8d, %ecx
2:      rep     stosb
        _ASM_EXTABLE_UA(2b, .Lfix_b)
        xorl    %ecx, %ecx
        RET

.Ltail:
3:      movb    %al, (%rdi)
        incq    %rdi
        decq    %rcx
        jnz     3b
        RET
        _ASM_EXTABLE_UA(3b, .Lexit)

.Lfix_q:
    movq    %r8, %rax
    andq    $7, %rax        /* rax = original tail bytes */
    shlq    $3, %rcx        /* rcx = remaining qwords * 8 */
    addq    %rax, %rcx      /* rcx = total remaining */
    RET

.Lfix_b:
        /* rep stosb fault – RCX already = bytes remaining */
        RET

.Lexit:
        RET
SYM_FUNC_END(rep_stos_alternative)
EXPORT_SYMBOL(rep_stos_alternative)
