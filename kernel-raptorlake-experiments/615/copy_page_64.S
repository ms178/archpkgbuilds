/*
 *  Fast 4-KiB page copy
 *    – REP-GOOD (ERMS / FSRM) CPUs: patched to  rep movsb
 *    – legacy fallback           : 64 × 64-byte unrolled loop
 *
 *  Clobbers:  RAX, RBX, RDX, R8-R12, RFLAGS
 */

#include <linux/linkage.h>
#include <linux/export.h>
#include <asm/asm.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>

	.section .noinstr.text, "ax"

/* ================================================================
 *  Fast-string path (patched in when REP_GOOD is present)
 * ============================================================== */
	.p2align 5
SYM_FUNC_START(copy_page)
	.cfi_startproc

	ALTERNATIVE "jmp copy_page_regs", "", X86_FEATURE_REP_GOOD

	movl	$4096, %ecx
	rep	movsb
	RET

	.cfi_endproc
SYM_FUNC_END(copy_page)
EXPORT_SYMBOL(copy_page)

/* ================================================================
 *  Hand-unrolled fallback
 * ============================================================== */
	.p2align 4
SYM_FUNC_START_LOCAL(copy_page_regs)
	.cfi_startproc

	/* --- prologue: save RBX, R12 --- */
	pushq	%rbx
	.cfi_adjust_cfa_offset 8
	.cfi_offset %rbx, -8

	pushq	%r12
	.cfi_adjust_cfa_offset 8
	.cfi_offset %r12, -16

	/* 64 iterations × 64 B = 4096 B */
	movl	$64, %ecx

.Lloop64:
	/* loads */
	movq	0(%rsi),  %rax
	movq	8(%rsi),  %rbx
	movq	16(%rsi), %rdx
	movq	24(%rsi), %r8
	movq	32(%rsi), %r9
	movq	40(%rsi), %r10
	movq	48(%rsi), %r11
	movq	56(%rsi), %r12

	/* stores */
	movq	%rax,  0(%rdi)
	movq	%rbx,  8(%rdi)
	movq	%rdx,  16(%rdi)
	movq	%r8,   24(%rdi)
	movq	%r9,   32(%rdi)
	movq	%r10,  40(%rdi)
	movq	%r11,  48(%rdi)
	movq	%r12,  56(%rdi)

	addq	$64, %rsi
	addq	$64, %rdi
	dec	%ecx
	jne	.Lloop64

	/* --- epilogue --- */
	popq	%r12
	.cfi_adjust_cfa_offset -8
	popq	%rbx
	.cfi_adjust_cfa_offset -8
	RET

	.cfi_endproc
SYM_FUNC_END(copy_page_regs)
