/* SPDX-License-Identifier: GPL-2.0 */
/*
 * csum_partial_copy_generic()
 *
 *  – Copy user → kernel buffer while computing a 64-bit ones-complement sum
 *  – Fully fault-tolerant: src may #PF, dst may #MC
 *
 *  Input :
 *      RDI : src  (user pointer)
 *      RSI : dst  (kernel pointer)
 *      EDX : len  (bytes, 32-bit)
 *
 *  Output:
 *      RAX : 64-bit checksum (undefined on exception)
 *
 *  Clobbers: RBX, RCX, RDX, R8-R15, RFLAGS
 */

#include <linux/linkage.h>
#include <asm/errno.h>
#include <asm/asm.h>

/* ----------------------------------------------------------------- */
/*  Macros to attach extable entries                                 */
/* ----------------------------------------------------------------- */
.macro source
10:
	_ASM_EXTABLE_UA(10b, .Lfault)
.endm

.macro dest
20:
	_ASM_EXTABLE_UA(20b, .Lfault)
.endm

/* ----------------------------------------------------------------- */
SYM_FUNC_START(csum_partial_copy_generic)
	/* Spill callee-saved that we clobber */
	subq	$5*8, %rsp
	movq	%rbx, 0*8(%rsp)
	movq	%r12, 1*8(%rsp)
	movq	%r14, 2*8(%rsp)
	movq	%r13, 3*8(%rsp)
	movq	%r15, 4*8(%rsp)

	movl	$-1, %eax	/* preset carry (for first ADC)   */
	xorl	%r9d, %r9d	/* r9 = 0 (used for final adc)    */
	movl	%edx, %ecx
	cmpl	$8, %ecx
	jb	.Lshort

	/* ------------------------------------------------------------- */
	/*  Check destination alignment                                  */
	/* ------------------------------------------------------------- */
	testb	$7, %sil
	jne	.Lunaligned

/* ---------- fast aligned path ------------------------------------ */
.Laligned:
	movl	%ecx, %r12d	/* r12d = len                    */
	shrq	$6,  %r12	/* number of 64-byte blocks      */
	jz	.Lhandle_tail

	clc				/* clear CF before adc chain     */
	.p2align 4
.Lloop_64:
	/* --- load 8×8 bytes from user -------------------------------- */
	source
	movq	 0(%rdi),  %rbx
	source
	movq	 8(%rdi),  %r8
	source
	movq	16(%rdi), %r11
	source
	movq	24(%rdi), %rdx
	source
	movq	32(%rdi), %r10
	source
	movq	40(%rdi), %r15
	source
	movq	48(%rdi), %r14
	source
	movq	56(%rdi), %r13

30:
	/* Prefetch next lines – tolerate fault */
	_ASM_EXTABLE(30b, 2f)
	prefetcht0 5*64(%rdi)
2:
	/* --- accumulate checksum (carry-prop chain) ---------------- */
	adcq	%rbx, %rax
	adcq	%r8,  %rax
	adcq	%r11, %rax
	adcq	%rdx, %rax
	adcq	%r10, %rax
	adcq	%r15, %rax
	adcq	%r14, %rax
	adcq	%r13, %rax

	decl	 %r12d          /* one 64-byte block done        */

	/* --- store to kernel dst  ----------------------------------- */
	dest
	movq	%rbx,  0(%rsi)
	dest
	movq	%r8,   8(%rsi)
	dest
	movq	%r11, 16(%rsi)
	dest
	movq	%rdx, 24(%rsi)
	dest
	movq	%r10, 32(%rsi)
	dest
	movq	%r15, 40(%rsi)
	dest
	movq	%r14, 48(%rsi)
	dest
	movq	%r13, 56(%rsi)

	leaq	64(%rdi), %rdi
	leaq	64(%rsi), %rsi
	jnz	.Lloop_64

	adcq	%r9, %rax	/* add final carry              */

	/* ------------------------------------------------------------- */
	/*  Handle tail: 0-56 bytes remain                               */
	/* ------------------------------------------------------------- */
.Lhandle_tail:
	movq	%rcx, %r10		/* keep original len           */
	andl	$63, %ecx
	shrl	$3, %ecx
	jz	.Lfold

	clc
	.p2align 4
.Lloop_8:
	source
	movq	(%rdi), %rbx
	adcq	%rbx, %rax
	decl	%ecx
	dest
	movq	%rbx, (%rsi)
	addq	$8, %rdi
	addq	$8, %rsi
	jnz	.Lloop_8
	adcq	%r9, %rax

/* ---------- fold 64→32 ------------------------------------------- */
.Lfold:
	movl	%eax, %ebx
	shrq	$32, %rax
	addl	%ebx, %eax
	adcl	%r9d, %eax

/* ---------- final 0-6 byte handler ------------------------------- */
.Lhandle_7:
	movl	%r10d, %ecx
	andl	$7, %ecx

.Ltail_words:
/* Two-byte chunks */
	shrl	$1, %ecx
	jz	.Lhandle_1
	movl	$2, %edx
	xorl	%ebx, %ebx
	clc
	.p2align 4
.Lloop_2:
	source
	movw	(%rdi), %bx
	adcl	%ebx, %eax
	decl	%ecx
	dest
	movw	%bx, (%rsi)
	addq	$2, %rdi
	addq	$2, %rsi
	jnz	.Lloop_2
	adcl	%r9d, %eax

/* ---------- last odd byte ---------------------------------------- */
.Lhandle_1:
	testb	$1, %r10b
	jz	.Lfinish
	xorl	%ebx, %ebx
	source
	movb	(%rdi), %bl
	dest
	movb	%bl, (%rsi)
	addl	%ebx, %eax
	adcl	%r9d, %eax

.Lfinish:
	testq	%r10, %r10
	js	.Lwas_odd

.Lout:
	/* restore spilled regs */
	movq	0*8(%rsp), %rbx
	movq	1*8(%rsp), %r12
	movq	2*8(%rsp), %r14
	movq	3*8(%rsp), %r13
	movq	4*8(%rsp), %r15
	addq	$5*8, %rsp
	RET

/* ================================================================
 *  Short (<8) initial length fast-path
 * ============================================================== */
.Lshort:
	movl	%ecx, %r10d
	jmp	.Ltail_words

/* ================================================================
 *  Destination not 8-byte aligned  (head fix-up)
 * ============================================================== */
.Lunaligned:
	xorl	%ebx, %ebx
	testb	$1, %sil
	je	.Lalign_word

	/* --- handle leading single byte ------------------------- */
	test	%edx, %edx
	je	.Lfinish
	source
	movb	(%rdi), %bl
	dest
	movb	%bl, (%rsi)
	incq	%rdi
	incq	%rsi
	decq	%rdx
	movl	%edx, %ecx
	jmp	.Laligned	/* now aligned or larger fix-up */

.Lalign_word:
	testb	$2, %sil
	je	.Lalign_long
	cmp	$2, %edx
	jb	.Lhandle_7
	/* 2-byte fix-up */
	source
	movw	(%rdi), %bx
	dest
	movw	%bx, (%rsi)
	addq	$2, %rdi
	addq	$2, %rsi
	subq	$2, %rdx
	movl	%edx, %ecx
	jmp	.Laligned

.Lalign_long:
	testb	$4, %sil
	je	.Laligned
	cmp	$4, %edx
	jb	.Lhandle_7

	/* 4-byte fix-up */
	source
	movl	(%rdi), %ebx
	dest
	movl	%ebx, (%rsi)
	addq	$4, %rdi
	addq	$4, %rsi
	subq	$4, %rdx
	movl	%edx, %ecx
	jmp	.Laligned

/* ================================================================
 *  Helpers: odd-length checksum rotation, fault handler
 * ============================================================== */
.Lwas_odd:
	roll	$8, %eax
	jmp	.Lout

.Lfault:
	xorl	%eax, %eax
	jmp	.Lout

SYM_FUNC_END(csum_partial_copy_generic)
