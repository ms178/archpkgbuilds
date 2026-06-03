/* SPDX-License-Identifier: GPL-2.0-only */
#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/cfi_types.h>
#include <linux/objtool.h>
#include <asm/asm.h>

/*
 * x86-64 page clearing and clear_user backend.
 *
 * Raptor Lake final notes:
 *  - clear_page_erms remains REP STOSB for the ERMS fast-string path.
 *  - clear_page_rep remains REP STOSQ for the legacy fast-string selector.
 *  - clear_page_orig is stackless and uses only caller-saved registers.
 *  - rep_stos_alternative intentionally keeps the vanilla low-latency scalar
 *    logic.  Multiple measured custom attempts regressed E-core short clears;
 *    the best production optimization is therefore to leave this sensitive
 *    clear_user residual-accounting helper on the measured no-regression path.
 */

/*
 * Zero a page.
 * %rdi - page
 */
SYM_TYPED_FUNC_START(clear_page_rep)
	movl	$4096/8, %ecx
	xorl	%eax, %eax
	rep stosq
	RET
SYM_FUNC_END(clear_page_rep)
EXPORT_SYMBOL_GPL(clear_page_rep)

SYM_TYPED_FUNC_START(clear_page_orig)
	xorl	%eax, %eax
	movl	$4096/64, %ecx
	.p2align 4
.Lclear_page_orig_loop:
	movq	%rax, 0(%rdi)
	movq	%rax, 8(%rdi)
	movq	%rax, 16(%rdi)
	movq	%rax, 24(%rdi)
	movq	%rax, 32(%rdi)
	movq	%rax, 40(%rdi)
	movq	%rax, 48(%rdi)
	movq	%rax, 56(%rdi)
	addq	$64, %rdi
	decl	%ecx
	jnz	.Lclear_page_orig_loop
	RET
SYM_FUNC_END(clear_page_orig)
EXPORT_SYMBOL_GPL(clear_page_orig)

SYM_TYPED_FUNC_START(clear_page_erms)
	movl	$4096, %ecx
	xorl	%eax, %eax
	rep stosb
	RET
SYM_FUNC_END(clear_page_erms)
EXPORT_SYMBOL_GPL(clear_page_erms)

/*
 * Default clear user-space.
 * Input:
 * rdi destination
 * rcx count
 * rax is zero
 *
 * Output:
 * rcx: uncleared bytes or 0 if successful.
 */
SYM_FUNC_START(rep_stos_alternative)
	ANNOTATE_NOENDBR
	cmpq	$64, %rcx
	jae	.Lunrolled

	cmp	$8, %ecx
	jae	.Lword

	testl	%ecx, %ecx
	je	.Lexit

.Lclear_user_tail:
0:	movb	%al, (%rdi)
	incq	%rdi
	decq	%rcx
	jnz	.Lclear_user_tail
.Lexit:
	RET

	_ASM_EXTABLE_UA(0b, .Lexit)

.Lword:
1:	movq	%rax, (%rdi)
	addq	$8, %rdi
	sub	$8, %ecx
	je	.Lexit
	cmp	$8, %ecx
	jae	.Lword
	jmp	.Lclear_user_tail

	.p2align 4
.Lunrolled:
10:	movq	%rax, (%rdi)
11:	movq	%rax, 8(%rdi)
12:	movq	%rax, 16(%rdi)
13:	movq	%rax, 24(%rdi)
14:	movq	%rax, 32(%rdi)
15:	movq	%rax, 40(%rdi)
16:	movq	%rax, 48(%rdi)
17:	movq	%rax, 56(%rdi)
	addq	$64, %rdi
	subq	$64, %rcx
	cmpq	$64, %rcx
	jae	.Lunrolled
	cmpl	$8, %ecx
	jae	.Lword
	testl	%ecx, %ecx
	jne	.Lclear_user_tail
	RET

	/*
	 * If we take an exception on any of the word stores, we know that
	 * %rcx is non-zero, so fall back to byte clearing for the exact count.
	 * The unrolled case may clear some bytes twice; this is the original
	 * helper's documented/accepted behavior.
	 */
	_ASM_EXTABLE_UA(1b, .Lclear_user_tail)
	_ASM_EXTABLE_UA(10b, .Lclear_user_tail)
	_ASM_EXTABLE_UA(11b, .Lclear_user_tail)
	_ASM_EXTABLE_UA(12b, .Lclear_user_tail)
	_ASM_EXTABLE_UA(13b, .Lclear_user_tail)
	_ASM_EXTABLE_UA(14b, .Lclear_user_tail)
	_ASM_EXTABLE_UA(15b, .Lclear_user_tail)
	_ASM_EXTABLE_UA(16b, .Lclear_user_tail)
	_ASM_EXTABLE_UA(17b, .Lclear_user_tail)
SYM_FUNC_END(rep_stos_alternative)
EXPORT_SYMBOL(rep_stos_alternative)
