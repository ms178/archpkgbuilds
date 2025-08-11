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
	/* No ENDBR64 here: .noinstr.text deliberately omits it              */
	movq	%rdi, %r11		/* save original dst for return   */

	testq	%rdx, %rdx
	jz	.Ldone_orig

	/*
	 * For very small fills, a straight REP STOSB is best.
	 * The 16-byte threshold is a proven heuristic.
	 */
	cmpq	$16, %rdx
	jbe	.Lsmall_fill_orig

	/* ---- build replicated 64-bit pattern (FIXED) ------------------- */
	/* Standard, correct sequence for creating a 64-bit pattern. */
	movabsq $0x0101010101010101, %r8
	movzbl	%sil, %eax
	imulq	%rax, %r8
	movq	%r8, %rax

	/* ---- align destination to an 8-byte boundary ------------------- */
	movq	%rdi, %rcx
	andq	$7,  %rcx
	jz	.Laligned_orig

	/*
	 * Use REP STOSB for the head alignment. It is provably safe as it
	 * will not write past the end of the buffer.
	 */
	negq	%rcx			/* rcx = 8 - (dst & 7)		*/
	andq	$7,  %rcx
	subq	%rcx, %rdx		/* adjust remaining length	*/
	rep	stosb			/* Uses %al, which holds pattern byte */

.Laligned_orig:
	/* ---- bulk fill with unrolled 64-byte stores -------------------- */
	movq	%rdx, %rcx
	shrq	$6,  %rcx		/* rcx = number of 64-byte blocks */
	jz	.Ltail_8_byte_chunks

	.p2align 4
.Lloop_64:
	movq	%rax, (%rdi)
	movq	%rax, 8(%rdi)
	movq	%rax, 16(%rdi)
	movq	%rax, 24(%rdi)
	movq	%rax, 32(%rdi)
	movq	%rax, 40(%rdi)
	movq	%rax, 48(%rdi)
	movq	%rax, 56(%rdi)
	addq	$64, %rdi
	decq	%rcx
	jnz	.Lloop_64

.Ltail_8_byte_chunks:
	/* ---- handle remaining 8-to-56 byte chunks ---------------------- */
	movl	%edx, %ecx
	andl	$0x38, %ecx		/* mask for 56, 48, ..., 8 */
	jz	.Ltail_orig
	shrq	$3, %rcx		/* rcx = number of 8-byte chunks */
	rep	stosq			/* REP STOSQ is efficient for this */

.Ltail_orig:
	/* ---- handle 0-to-7 residual bytes with one overlapping write --- */
	andl	$7,  %edx
	jz	.Ldone_orig
	addq	%rdx, %rdi		/* rdi -> end of buffer		*/
	movq	%rax, -8(%rdi)		/* write one qword at end-8	*/
	jmp	.Ldone_orig

.Lsmall_fill_orig:
	/* This path is jumped to directly, so it needs to setup its own registers. */
	movq	%rdx, %rcx
	movb	%sil, %al		/* Setup %al for rep stosb	*/
	rep	stosb

.Ldone_orig:
	movq	%r11, %rax		/* return original dst		*/
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
