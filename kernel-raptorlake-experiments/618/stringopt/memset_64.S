/* SPDX-License-Identifier: GPL-2.0 */
/*
 * memset() - 64-bit implementation for x86-64
 *
 * Raptor Lake policy:
 *  - The FSRS path stays inline at the public symbol after alternatives
 *    patching; there is no extra trampoline branch on the hot path.
 *  - <=64 bytes avoid REP STOSB startup by using first/last scalar stores.
 *  - >64 bytes use REP STOSB and only require AL, avoiding unnecessary
 *    64-bit pattern construction in the large path.
 *  - The fallback path is scalar, stackless, and safe before alternatives.
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/cfi_types.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>

.section .noinstr.text, "ax"

/*
 * ISO C memset - set a memory block to a byte value.
 *
 * rdi   destination
 * rsi   value (char)
 * rdx   count (bytes)
 *
 * rax   original destination
 */
SYM_TYPED_FUNC_START(__memset)
	ALTERNATIVE "jmp memset_orig", "", X86_FEATURE_FSRS

	movq	%rdi, %r9
	testq	%rdx, %rdx
	jz	.Ldone_fsrs

	movzbl	%sil, %eax
	cmpq	$64, %rdx
	ja	.Lrep_stosb

	/* Build the repeated-byte scalar pattern only for the scalar path. */
	imull	$0x01010101, %eax, %eax
	movl	%eax, %ecx
	shlq	$32, %rcx
	orq	%rcx, %rax
	movq	%rdx, %rcx

	cmpq	$32, %rcx
	ja	.L32_64
	cmpq	$16, %rcx
	ja	.L16_32
	cmpq	$8, %rcx
	ja	.L8_16
	cmpq	$4, %rcx
	ja	.L4_8
	cmpq	$2, %rcx
	ja	.L3_4
	je	.L2
	movb	%al, (%rdi)
	jmp	.Ldone_fsrs

.L2:
	movw	%ax, (%rdi)
	jmp	.Ldone_fsrs

.L3_4:
	movw	%ax, (%rdi)
	movw	%ax, -2(%rdi,%rcx)
	jmp	.Ldone_fsrs

.L4_8:
	movl	%eax, (%rdi)
	movl	%eax, -4(%rdi,%rcx)
	jmp	.Ldone_fsrs

.L8_16:
	movq	%rax, (%rdi)
	movq	%rax, -8(%rdi,%rcx)
	jmp	.Ldone_fsrs

.L16_32:
	movq	%rax, (%rdi)
	movq	%rax, 8(%rdi)
	movq	%rax, -16(%rdi,%rcx)
	movq	%rax, -8(%rdi,%rcx)
	jmp	.Ldone_fsrs

.L32_64:
	movq	%rax, (%rdi)
	movq	%rax, 8(%rdi)
	movq	%rax, 16(%rdi)
	movq	%rax, 24(%rdi)
	movq	%rax, -32(%rdi,%rcx)
	movq	%rax, -24(%rdi,%rcx)
	movq	%rax, -16(%rdi,%rcx)
	movq	%rax, -8(%rdi,%rcx)
	jmp	.Ldone_fsrs

.Lrep_stosb:
	movq	%rdx, %rcx
	rep	stosb

.Ldone_fsrs:
	movq	%r9, %rax
	RET
SYM_FUNC_END(__memset)
EXPORT_SYMBOL(__memset)

SYM_FUNC_ALIAS_MEMFUNC(memset, __memset)
SYM_PIC_ALIAS(memset)
EXPORT_SYMBOL(memset)

SYM_FUNC_START_LOCAL(memset_orig)
	movq	%rdi, %r10

	testq	%rdx, %rdx
	jz	.Ldone_orig

	/* expand byte value */
	movzbl	%sil, %ecx
	movabs	$0x0101010101010101, %rax
	imulq	%rcx, %rax

	/* align dst */
	movl	%edi, %r9d
	andl	$7, %r9d
	jnz	.Lbad_alignment
.Lafter_bad_alignment:

	movq	%rdx, %rcx
	shrq	$6, %rcx
	jz	.Lhandle_tail

	.p2align 4
.Lloop_64:
	decq	%rcx
	movq	%rax, (%rdi)
	movq	%rax, 8(%rdi)
	movq	%rax, 16(%rdi)
	movq	%rax, 24(%rdi)
	movq	%rax, 32(%rdi)
	movq	%rax, 40(%rdi)
	movq	%rax, 48(%rdi)
	movq	%rax, 56(%rdi)
	leaq	64(%rdi), %rdi
	jnz	.Lloop_64

	/* Handle tail in loops. The loops should be faster than hard
	 * to predict jump tables.
	 */
	.p2align 4
.Lhandle_tail:
	movl	%edx, %ecx
	andl	$63&(~7), %ecx
	jz	.Lhandle_7
	shrl	$3, %ecx
	.p2align 4
.Lloop_8:
	decl	%ecx
	movq	%rax, (%rdi)
	leaq	8(%rdi), %rdi
	jnz	.Lloop_8

.Lhandle_7:
	andl	$7, %edx
	jz	.Ldone_orig
	.p2align 4
.Lloop_1:
	decl	%edx
	movb	%al, (%rdi)
	leaq	1(%rdi), %rdi
	jnz	.Lloop_1

.Ldone_orig:
	movq	%r10, %rax
	RET

.Lbad_alignment:
	cmpq	$7, %rdx
	jbe	.Lhandle_7
	movq	%rax, (%rdi)	/* unaligned store */
	movq	$8, %r8
	subq	%r9, %r8
	addq	%r8, %rdi
	subq	%r8, %rdx
	jmp	.Lafter_bad_alignment
SYM_FUNC_END(memset_orig)
