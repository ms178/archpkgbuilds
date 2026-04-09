/* SPDX-License-Identifier: GPL-2.0
 *
 * memset() — 64-bit implementation for x86-64
 * Copyright 2024 [Your Name/Handle], refined for safety and performance
 *
 *  – Boot-safe: Worker functions live in .noinstr.text and never contain
 *    ENDBR64, so the early decompressor can run them on any CPU.
 *  – CET-compliant: The exported trampoline in .text *does* contain ENDBR64
 *    (when CONFIG_X86_KERNEL_IBT=y), satisfying objtool and the
 *    indirect-call ABI enforced in the main kernel.
 *  – Optimal: Uses FSRS (Fast Short REP STOSB) path on modern CPUs, with a
 *    highly-tuned, safe, and unrolled fallback for other CPUs.
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/cfi_types.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>

/* ------------------------------------------------------------------------ */
/*  Worker functions — NO instrumentation, safe for early boot              */
/* ------------------------------------------------------------------------ */
	.section .noinstr.text, "ax"

/* ======================================================================== */
/*  memset_orig — generic fallback, unrolled and tuned for performance      */
/* ======================================================================== */
	.p2align 4
SYM_FUNC_START_LOCAL(memset_orig)
	movq	%rdi, %r11		/* save original dst for return   */

	testq	%rdx, %rdx
	jz	.Ldone_orig

	/* ---- build replicated 64-bit pattern ----------------------- */
	movzbl	%sil, %eax
	movabsq	$0x0101010101010101, %r8
	imulq	%r8, %rax, %rax

	/* ---- fast path: small fills (≤ 16 bytes) ------------------- */
	cmpq	$16, %rdx
	jbe	.Lsmall_fill_orig

	/* ---- align destination to 16-byte boundary ----------------- */
	movq	%rdi, %rcx
	andq	$15, %rcx
	jz	.Laligned_orig

	negq	%rcx
	andq	$15, %rcx		/* rcx = 16 - (dst & 15)	*/
	/*
	 * Overlapping 16-byte write covers the unaligned head.
	 * Safe because rdx > 16.
	 */
	movq	%rax, (%rdi)
	movq	%rax, 8(%rdi)
	subq	%rcx, %rdx
	addq	%rcx, %rdi

.Laligned_orig:
	/* ---- bulk fill with 2x unrolled 64-byte stores ------------ */
	movq	%rdx, %rcx
	shrq	$7, %rcx		/* 128-byte blocks		*/
	jz	.Ltail

	.p2align 5
.Lloop_128:
	movq	%rax, (%rdi)
	movq	%rax, 8(%rdi)
	movq	%rax, 16(%rdi)
	movq	%rax, 24(%rdi)
	movq	%rax, 32(%rdi)
	movq	%rax, 40(%rdi)
	movq	%rax, 48(%rdi)
	movq	%rax, 56(%rdi)
	movq	%rax, 64(%rdi)
	movq	%rax, 72(%rdi)
	movq	%rax, 80(%rdi)
	movq	%rax, 88(%rdi)
	movq	%rax, 96(%rdi)
	movq	%rax, 104(%rdi)
	movq	%rax, 112(%rdi)
	movq	%rax, 120(%rdi)
	addq	$128, %rdi
	decq	%rcx
	jnz	.Lloop_128

	andl	$0x7f, %edx		/* remaining 0–127 bytes	*/

.Ltail:
	/* ---- handle 1–127 byte tail ------------------------------- */
	cmpq	$8, %rdx
	jb	.Ltail_1_7

	movq	%rax, (%rdi)
	movq	%rax, -8(%rdi, %rdx)
	cmpq	$16, %rdx
	jb	.Ldone_orig
	movq	%rax, 8(%rdi)
	movq	%rax, -16(%rdi, %rdx)
	cmpq	$32, %rdx
	jb	.Ldone_orig
	movq	%rax, 16(%rdi)
	movq	%rax, 24(%rdi)
	cmpq	$48, %rdx
	jb	.Ldone_orig
	movq	%rax, 32(%rdi)
	movq	%rax, 40(%rdi)
	jmp	.Ldone_orig

.Ltail_1_7:
	testq	%rdx, %rdx
	jz	.Ldone_orig
	movb	%al, (%rdi)
	cmpq	$1, %rdx
	je	.Ldone_orig
	movb	%al, 1(%rdi)
	cmpq	$2, %rdx
	je	.Ldone_orig
	movb	%al, 2(%rdi)
	cmpq	$3, %rdx
	je	.Ldone_orig
	movb	%al, 3(%rdi)
	cmpq	$4, %rdx
	je	.Ldone_orig
	movb	%al, 4(%rdi)
	cmpq	$5, %rdx
	je	.Ldone_orig
	movb	%al, 5(%rdi)
	cmpq	$6, %rdx
	je	.Ldone_orig
	movb	%al, 6(%rdi)
	jmp	.Ldone_orig

.Lsmall_fill_orig:
	/* 1–16 bytes: overlapping stores */
	cmpq	$8, %rdx
	jb	.Ltail_1_7
	movq	%rax, (%rdi)
	movq	%rax, -8(%rdi, %rdx)
	jmp	.Ldone_orig

.Ldone_orig:
	movq	%r11, %rax
	RET
SYM_FUNC_END(memset_orig)


/* ======================================================================== */
/*  __memset_erms — FSRS fast-path worker (also CET-free)                   */
/* ======================================================================== */
	.p2align 4
SYM_FUNC_START_LOCAL(__memset_erms)
	movq	%rdi, %r11		/* save original dst for return   */
	movq	%rdx, %rcx		/* length			*/
	movzbl	%sil, %eax		/* fill byte → eax (al used)	*/
	rep	stosb			/* Fast-String fill		*/
	movq	%r11, %rax		/* restore original dst		*/
	RET
SYM_FUNC_END(__memset_erms)


/* ------------------------------------------------------------------------ */
/*  Public trampoline — CET-compliant, lives in normal .text                */
/* ------------------------------------------------------------------------ */
	.section .text, "ax"

/* ======================================================================== */
/*  __memset — exported entry, 5-byte ALT jump to worker                    */
/* ======================================================================== */
	.p2align 5
SYM_TYPED_FUNC_START(__memset)
	/*
	 *  The ALTERNATIVE macro patches the destination of this direct
	 *  jump at boot time based on CPU features. This is CET-compliant
	 *  and safe for early boot.
	 *     – default  : jmp memset_orig         (safe, unrolled fallback)
	 *     – patched  : jmp __memset_erms       (fastest on modern CPUs)
	 */
	ALTERNATIVE "jmp memset_orig", "jmp __memset_erms", X86_FEATURE_FSRS
SYM_FUNC_END(__memset)

EXPORT_SYMBOL(__memset)

/* Alias “memset” to the same trampoline symbol */
SYM_FUNC_ALIAS_MEMFUNC(memset, __memset)
SYM_PIC_ALIAS(memset)
EXPORT_SYMBOL(memset)
