/* SPDX-License-Identifier: GPL-2.0 */
/*
 * memmove - overlap-safe copy for x86-64
 *
 * Raptor Lake policy:
 *  - No SIMD/FPU state, no stack, no callee-saved registers.
 *  - Non-overlap and dst<src use a forward path; >16 bytes relies on REP
 *    MOVSB, which is the right large-copy primitive on ERMS/FSRM systems.
 *  - Overlap with dst>src copies backward.  Tiny copies are register-buffered
 *    before any store so dst=src+1 cannot corrupt the source.  Larger copies
 *    use DF=1 REP MOVSB and always restore DF before return.
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/cfi_types.h>
#include <linux/objtool.h>
#include <asm/asm.h>

	.section .noinstr.text, "ax", @progbits

/* void *memmove(void *dst, const void *src, size_t len) */
SYM_TYPED_FUNC_START(__memmove)
	ANNOTATE_NOENDBR
	movq	%rdi, %rax		/* return value */

	testq	%rdx, %rdx
	jz	.Ldone
	cmpq	%rsi, %rdi
	je	.Ldone

	/* forward if dst < src or if the ranges do not overlap */
	movq	%rdi, %rcx
	subq	%rsi, %rcx		/* unsigned dst-src */
	cmpq	%rsi, %rdi
	jb	.Lforward
	cmpq	%rdx, %rcx
	jae	.Lforward

	/* ---------- backward: overlap with dst > src ---------- */
	cmpq	$16, %rdx
	ja	.Lbackward_rep

	cmpq	$8, %rdx
	jb	.Lbwd_lt8
	movq	(%rsi), %rcx
	movq	-8(%rsi,%rdx), %r8
	movq	%r8, -8(%rdi,%rdx)
	movq	%rcx, (%rdi)
	jmp	.Ldone

.Lbwd_lt8:
	cmpq	$4, %rdx
	jb	.Lbwd_lt4
	movl	(%rsi), %ecx
	movl	-4(%rsi,%rdx), %r8d
	movl	%r8d, -4(%rdi,%rdx)
	movl	%ecx, (%rdi)
	jmp	.Ldone

.Lbwd_lt4:
	cmpq	$2, %rdx
	jb	.Lbwd_1
	movw	(%rsi), %cx
	movw	-2(%rsi,%rdx), %r8w
	movw	%r8w, -2(%rdi,%rdx)
	movw	%cx, (%rdi)
	jmp	.Ldone

.Lbwd_1:
	movb	(%rsi), %cl
	movb	%cl, (%rdi)
	jmp	.Ldone

	.p2align 4
.Lbackward_rep:
	leaq	-1(%rsi,%rdx), %rsi
	leaq	-1(%rdi,%rdx), %rdi
	std
	movq	%rdx, %rcx
	rep	movsb
	cld
	jmp	.Ldone

	/* ---------- forward: dst < src or non-overlapping ---------- */
	.p2align 4
.Lforward:
	cmpq	$16, %rdx
	ja	.Lforward_rep

	cmpq	$8, %rdx
	jb	.Lfwd_lt8
	movq	(%rsi), %rcx
	movq	-8(%rsi,%rdx), %r8
	movq	%rcx, (%rdi)
	movq	%r8, -8(%rdi,%rdx)
	jmp	.Ldone

.Lfwd_lt8:
	cmpq	$4, %rdx
	jb	.Lfwd_lt4
	movl	(%rsi), %ecx
	movl	-4(%rsi,%rdx), %r8d
	movl	%ecx, (%rdi)
	movl	%r8d, -4(%rdi,%rdx)
	jmp	.Ldone

.Lfwd_lt4:
	cmpq	$2, %rdx
	jb	.Lfwd_1
	movw	(%rsi), %cx
	movw	-2(%rsi,%rdx), %r8w
	movw	%cx, (%rdi)
	movw	%r8w, -2(%rdi,%rdx)
	jmp	.Ldone

.Lfwd_1:
	movb	(%rsi), %cl
	movb	%cl, (%rdi)
	jmp	.Ldone

	.p2align 4
.Lforward_rep:
	movq	%rdx, %rcx
	rep	movsb

.Ldone:
	RET
SYM_FUNC_END(__memmove)
EXPORT_SYMBOL(__memmove)

SYM_FUNC_ALIAS_MEMFUNC(memmove, __memmove)
EXPORT_SYMBOL(memmove)
