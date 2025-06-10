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
	ALTERNATIVE "jmp copy_page_regs", "", X86_FEATURE_REP_GOOD

	movl	$4096, %ecx
	rep	movsb
	RET
SYM_FUNC_END(copy_page)
EXPORT_SYMBOL(copy_page)

/* ================================================================
 *  Hand-unrolled fallback
 * ============================================================== */
	.p2align 4
SYM_FUNC_START(copy_page_regs)
	/* Save callee-saved registers */
	pushq	%rbx
	pushq	%r12

	/* 64 iterations × 64 B = 4096 B */
	movl	$64, %ecx

.Lloop64:
	/* Load 64 bytes from source */
	movq	0(%rsi),  %rax
	movq	8(%rsi),  %rbx
	movq	16(%rsi), %rdx
	movq	24(%rsi), %r8
	movq	32(%rsi), %r9
	movq	40(%rsi), %r10
	movq	48(%rsi), %r11
	movq	56(%rsi), %r12

	/* Store 64 bytes to destination */
	movq	%rax,  0(%rdi)
	movq	%rbx,  8(%rdi)
	movq	%rdx,  16(%rdi)
	movq	%r8,   24(%rdi)
	movq	%r9,   32(%rdi)
	movq	%r10,  40(%rdi)
	movq	%r11,  48(%rdi)
	movq	%r12,  56(%rdi)

	/* Advance pointers */
	addq	$64, %rsi
	addq	$64, %rdi
	dec	%ecx
	jne	.Lloop64

	/* Restore registers and return */
	popq	%r12
	popq	%rbx
	RET
SYM_FUNC_END(copy_page_regs)
