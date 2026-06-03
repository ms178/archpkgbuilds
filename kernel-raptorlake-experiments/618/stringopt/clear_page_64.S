/* SPDX-License-Identifier: GPL-2.0-only */
#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/cfi_types.h>
#include <linux/objtool.h>
#include <asm/asm.h>

/*
 * x86-64 page clearing and clear_user backend.
 *
 * Raptor Lake notes:
 *  - clear_page_erms remains REP STOSB for the ERMS fast-string path.
 *  - clear_page_rep remains REP STOSQ for the legacy fast-string selector.
 *  - clear_page_orig is stackless and uses only caller-saved registers.
 *  - rep_stos_alternative keeps precise clear_user residual semantics while
 *    reducing branch overhead for the common 16..63 byte range with a safe
 *    two-qword loop.  Fault fixups intentionally fall back to byte probing;
 *    bytes may be cleared twice, but RCX is precise on exit.
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

	cmpq	$8, %rcx
	jb	.Lmaybe_tail
	cmpq	$16, %rcx
	jae	.Lword2
	jmp	.Lword

.Lmaybe_tail:
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

	.p2align 4
.Lword2:
2:	movq	%rax, (%rdi)
3:	movq	%rax, 8(%rdi)
	addq	$16, %rdi
	subq	$16, %rcx
	cmpq	$16, %rcx
	jae	.Lword2
	cmpq	$8, %rcx
	jae	.Lword
	testl	%ecx, %ecx
	jne	.Lclear_user_tail
	RET

.Lword:
1:	movq	%rax, (%rdi)
	addq	$8, %rdi
	subq	$8, %rcx
	je	.Lexit
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
	cmpq	$16, %rcx
	jae	.Lword2
	cmpq	$8, %rcx
	jae	.Lword
	testl	%ecx, %ecx
	jne	.Lclear_user_tail
	RET

	/*
	 * If we take an exception on any of the word stores, RCX still
	 * describes the not-yet-accounted range for the current chunk.  Fall
	 * back to byte probing for an exact residual.  Some already-cleared
	 * bytes can be stored twice; this matches the original semantics.
	 */
	_ASM_EXTABLE_UA(1b, .Lclear_user_tail)
	_ASM_EXTABLE_UA(2b, .Lclear_user_tail)
	_ASM_EXTABLE_UA(3b, .Lclear_user_tail)
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
