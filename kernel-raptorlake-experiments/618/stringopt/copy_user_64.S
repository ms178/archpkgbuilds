/* SPDX-License-Identifier: GPL-2.0-only */
/*
 * Copyright 2008 Vitaly Mayatskikh <vmayatsk@redhat.com>
 * Copyright 2002 Andi Kleen, SuSE Labs.
 *
 * Functions to copy from and to user space.
 *
 * Raptor Lake notes:
 *  - 0..15 bytes are laid out as the vanilla tiny path: the 8..15 byte
 *    qword copy is fall-through after the size tests, avoiding the extra
 *    taken branch that showed up as an 8-byte regression on E-cores.
 *  - 16..63 bytes use a correct 2xqword loop.  The steady-state loop keeps
 *    pointer/count state precise after each committed 16-byte chunk.
 *  - >=64 bytes uses the existing ERMS alternative and is patched to
 *    REP MOVSB on the i7-14700KF.
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/cfi_types.h>
#include <linux/objtool.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>
#include <asm/asm.h>

/*
 * rep_movs_alternative - memory copy with exception handling.
 * This version is for CPUs that don't have FSRM (Fast Short Rep Movs)
 *
 * Input:
 * rdi destination
 * rsi source
 * rcx count
 *
 * Output:
 * rcx uncopied bytes or 0 if successful.
 *
 * NOTE! The calling convention is very intentionally the same as
 * for 'rep movs', so that we can rewrite the function call with
 * just a plain 'rep movs' on machines that have FSRM.  But to make
 * it simpler for us, we can clobber rsi/rdi and rax freely.
 */
SYM_FUNC_START(rep_movs_alternative)
	ANNOTATE_NOENDBR
	cmp	$8, %ecx
	jb	.Llt8
	cmpq	$16, %rcx
	jae	.Lge16

	/* 8..15 bytes: vanilla-shaped qword + byte tail path. */
6:	movq	(%rsi), %rax
7:	movq	%rax, (%rdi)
	addq	$8, %rsi
	addq	$8, %rdi
	subq	$8, %rcx
	je	.Lexit

.Lcopy_user_tail:
4:	movb	(%rsi), %al
5:	movb	%al, (%rdi)
	incq	%rdi
	incq	%rsi
	decq	%rcx
	jne	.Lcopy_user_tail
.Lexit:
	RET

.Llt8:
	testl	%ecx, %ecx
	jne	.Lcopy_user_tail
	RET

	.p2align 4
.Lge16:
	cmpq	$64, %rcx
	jae	.Llarge

.Lqword2:
0:	movq	(%rsi), %rax
1:	movq	%rax, (%rdi)
2:	movq	8(%rsi), %rax
3:	movq	%rax, 8(%rdi)
	addq	$16, %rsi
	addq	$16, %rdi
	subq	$16, %rcx
	cmpq	$16, %rcx
	jae	.Lqword2
	cmpq	$8, %rcx
	jae	6b
	testl	%ecx, %ecx
	jne	.Lcopy_user_tail
	RET

	_ASM_EXTABLE_UA(0b, .Lcopy_user_tail)
	_ASM_EXTABLE_UA(1b, .Lcopy_user_tail)
	_ASM_EXTABLE_UA(2b, .Lcopy_user_tail)
	_ASM_EXTABLE_UA(3b, .Lcopy_user_tail)
	_ASM_EXTABLE_UA(4b, .Lexit)
	_ASM_EXTABLE_UA(5b, .Lexit)
	_ASM_EXTABLE_UA(6b, .Lcopy_user_tail)
	_ASM_EXTABLE_UA(7b, .Lcopy_user_tail)

.Llarge:
0:	ALTERNATIVE "jmp .Llarge_movsq", "rep movsb", X86_FEATURE_ERMS
1:	RET

	_ASM_EXTABLE_UA(0b, 1b)

.Llarge_movsq:
	/* Do the first possibly unaligned word */
0:	movq	(%rsi), %rax
1:	movq	%rax, (%rdi)

	_ASM_EXTABLE_UA(0b, .Lcopy_user_tail)
	_ASM_EXTABLE_UA(1b, .Lcopy_user_tail)

	/* What would be the offset to the aligned destination? */
	leaq	8(%rdi), %rax
	andq	$-8, %rax
	subq	%rdi, %rax

	/* .. and update pointers and count to match */
	addq	%rax, %rdi
	addq	%rax, %rsi
	subq	%rax, %rcx

	/* make %rcx contain the number of words, %rax the remainder */
	movq	%rcx, %rax
	shrq	$3, %rcx
	andl	$7, %eax
0:	rep movsq
	movl	%eax, %ecx
	testl	%ecx, %ecx
	jne	.Lcopy_user_tail
	RET

1:	leaq	(%rax,%rcx,8), %rcx
	jmp	.Lcopy_user_tail

	_ASM_EXTABLE_UA(0b, 1b)
SYM_FUNC_END(rep_movs_alternative)
EXPORT_SYMBOL(rep_movs_alternative)
