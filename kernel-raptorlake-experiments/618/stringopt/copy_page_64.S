/* SPDX-License-Identifier: GPL-2.0 */
/*
 * copy_page - 4 KiB page copy
 *
 * Raptor Lake v5 policy:
 *  - Public fast path uses REP MOVSQ when X86_FEATURE_REP_GOOD patches out
 *    the fallback branch.  User P/E-core measurements did not show a win for
 *    changing the vanilla page-copy fast path to REP MOVSB.
 *  - The scalar fallback is only for CPUs without REP_GOOD.  It avoids stack
 *    traffic and callee-saved registers, using only caller-saved temporaries.
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/cfi_types.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>

	.section .noinstr.text, "ax"

	ALIGN
SYM_TYPED_FUNC_START(copy_page)
	ALTERNATIVE "jmp copy_page_regs", "", X86_FEATURE_REP_GOOD
	movl	$4096/8, %ecx
	rep	movsq
	RET
SYM_FUNC_END(copy_page)
EXPORT_SYMBOL(copy_page)

SYM_FUNC_START_LOCAL(copy_page_regs)
	movl	$4096/64, %ecx
	.p2align 4
.Loop64:
	movq	0(%rsi), %rax
	movq	8(%rsi), %rdx
	movq	16(%rsi), %r8
	movq	24(%rsi), %r9
	movq	%rax, 0(%rdi)
	movq	%rdx, 8(%rdi)
	movq	%r8, 16(%rdi)
	movq	%r9, 24(%rdi)

	movq	32(%rsi), %rax
	movq	40(%rsi), %rdx
	movq	48(%rsi), %r8
	movq	56(%rsi), %r9
	movq	%rax, 32(%rdi)
	movq	%rdx, 40(%rdi)
	movq	%r8, 48(%rdi)
	movq	%r9, 56(%rdi)

	addq	$64, %rsi
	addq	$64, %rdi
	decl	%ecx
	jnz	.Loop64
	RET
SYM_FUNC_END(copy_page_regs)
