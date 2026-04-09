/* SPDX-License-Identifier: GPL-2.0-only */
/* Copyright 2002 Andi Kleen, audited for Intel Raptor Lake */

#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/cfi_types.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>

.section .noinstr.text, "ax"

/*
 * void *memcpy(void *dst, const void *src, size_t len)
 *   rdi = dst, rsi = src, rdx = len, returns rax = dst
 */
SYM_TYPED_FUNC_START(__memcpy)
	ANNOTATE_NOENDBR
	ALTERNATIVE "jmp memcpy_orig", "", X86_FEATURE_FSRM

	movq	%rdi, %rax
	movq	%rdx, %rcx
	rep	movsb
	RET
SYM_FUNC_END(__memcpy)
EXPORT_SYMBOL(__memcpy)

SYM_FUNC_ALIAS_MEMFUNC(memcpy, __memcpy)
SYM_PIC_ALIAS(memcpy)
EXPORT_SYMBOL(memcpy)

SYM_FUNC_START_LOCAL(memcpy_orig)
	ANNOTATE_NOENDBR
	movq	%rdi, %rax

	cmpq	$32, %rdx
	jb	.Ltail

	.p2align 6
.Lloop32:
	movq	0(%rsi), %r8
	movq	8(%rsi), %r9
	movq	16(%rsi), %r10
	movq	24(%rsi), %r11
	movq	%r8, 0(%rdi)
	movq	%r9, 8(%rdi)
	movq	%r10, 16(%rdi)
	movq	%r11, 24(%rdi)
	addq	$32, %rsi
	addq	$32, %rdi
	subq	$32, %rdx
	cmpq	$32, %rdx
	jae	.Lloop32

.Ltail:
	cmpq	$16, %rdx
	jb	.Llt16
	movq	0(%rsi), %r8
	movq	8(%rsi), %r9
	movq	-16(%rsi,%rdx), %r10
	movq	-8(%rsi,%rdx), %r11
	movq	%r8, 0(%rdi)
	movq	%r9, 8(%rdi)
	movq	%r10, -16(%rdi,%rdx)
	movq	%r11, -8(%rdi,%rdx)
	RET

	.p2align 4
.Llt16:
	cmpq	$8, %rdx
	jb	.Llt8
	movq	0(%rsi), %r8
	movq	-8(%rsi,%rdx), %r9
	movq	%r8, 0(%rdi)
	movq	%r9, -8(%rdi,%rdx)
	RET

	.p2align 4
.Llt8:
	cmpq	$4, %rdx
	jb	.Llt4
	movl	0(%rsi), %ecx
	movl	-4(%rsi,%rdx), %r8d
	movl	%ecx, 0(%rdi)
	movl	%r8d, -4(%rdi,%rdx)
	RET

	.p2align 4
.Llt4:
	testq	%rdx, %rdx
	je	.Ldone
	cmpq	$2, %rdx
	jb	.L1
	movw	0(%rsi), %cx
	movw	-2(%rsi,%rdx), %r8w
	movw	%cx, 0(%rdi)
	movw	%r8w, -2(%rdi,%rdx)
	cmpq	$3, %rdx
	jne	.Ldone
	movb	2(%rsi), %cl
	movb	%cl, 2(%rdi)
	RET
.L1:
	movb	0(%rsi), %cl
	movb	%cl, 0(%rdi)
.Ldone:
	RET
SYM_FUNC_END(memcpy_orig)
