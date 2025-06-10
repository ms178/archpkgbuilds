/* SPDX-License-Identifier: GPL-2.0 */
/*
 * memmove() — overlap-safe copy, tuned for Intel® Raptor-Lake-R
 *
 *  rdi = dst , rsi = src , rdx = len
 *  rax = dst  (SysV ABI)
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/cfi_types.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>

	.text

/* =================================================================== */
/*  Public entry — FSRM CPUs are patched to inline REP MOVSB            */
/* =================================================================== */
	.p2align 5
SYM_TYPED_FUNC_START(__memmove)
	ALTERNATIVE "jmp memmove_orig", "", X86_FEATURE_FSRM

	/* Fast path: REP MOVSB (FSRM) */
	movq	%rdi, %rax
	movq	%rdx, %rcx
	rep	movsb
	RET
SYM_FUNC_END(__memmove)
EXPORT_SYMBOL(__memmove)
SYM_FUNC_ALIAS_MEMFUNC(memmove, __memmove)
EXPORT_SYMBOL(memmove)

/* =================================================================== */
/*  Fallback implementation                                            */
/* =================================================================== */
	.p2align 4
SYM_FUNC_START_LOCAL(memmove_orig)
	movq	%rdi, %rax		/* preserve dst for return */

/* ------------ choose copy direction -------------------------------- */
	cmpq	%rdi, %rsi
	jbe	.Lforward_entry

	leaq	(%rsi,%rdx), %rcx
	cmpq	%rcx, %rdi
	jbe	.Lforward_entry		/* dst ≥ src_end → forward */

	/* ---------------- backward copy (overlap) -------------------- */
.Lbackward_entry:
	addq	%rdx, %rsi
	addq	%rdx, %rdi
	std					/* DF = 1 */
	movq	%rdx, %rcx
	rep	movsb
	cld					/* restore DF */
	RET

/* ---------------- forward copy paths ------------------------------- */
.Lforward_entry:
	/* Large?  Use rep movsq; else small unroll */
	cmpq	$128, %rdx
	jb	.Lsmall_fwd

/* ---------- .Lforward_main: NO redundant cld ---------------------- */
.Lforward_main:
	movq	%rdx, %rcx
	shrq	$3, %rcx
	rep	movsq
	movq	%rdx, %rcx
	andq	$7, %rcx
	rep	movsb
	RET

/* ---------- small forward (<128 B) -------------------------------- */
.Lsmall_fwd:
	subq	$0x20, %rdx
.Lunroll32:
	subq	$0x20, %rdx
	movq	0(%rsi),  %r8
	movq	8(%rsi),  %r9
	movq	16(%rsi), %r10
	movq	24(%rsi), %r11
	addq	$0x20,    %rsi
	movq	%r8,  0(%rdi)
	movq	%r9,  8(%rdi)
	movq	%r10, 16(%rdi)
	movq	%r11, 24(%rdi)
	addq	$0x20,    %rdi
	jae	.Lunroll32
	addq	$0x20,    %rdx		/* correct overshoot */

	/* tail 0-31 B */
	testq	%rdx, %rdx
	je	.Lret
	movq	%rdx, %rcx
	rep	movsb

.Lret:
	RET
SYM_FUNC_END(memmove_orig)
