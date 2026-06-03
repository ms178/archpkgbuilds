/* SPDX-License-Identifier: GPL-2.0-only */
/* Copyright(c) 2016-2020 Intel Corporation. All rights reserved. */

#include <linux/linkage.h>
#include <asm/asm.h>

#ifndef CONFIG_UML

#ifdef CONFIG_X86_MCE

/*
 * copy_mc_fragile - copy memory with indication if an exception / fault happened
 *
 * The fragile path deliberately avoids fast-string instructions.  This variant
 * keeps the original fault-accounting model but reduces loop-branch density on
 * Raptor Lake by copying two naturally aligned qwords per steady-state loop.
 * Pointers and ECX are advanced after each successful qword, so every read or
 * write exception still observes a precise "current byte" position for the
 * existing fixup code.
 */
SYM_FUNC_START(copy_mc_fragile)
	cmpl	$8, %edx
	/* Less than 8 bytes? Go to byte copy loop */
	jb	.L_no_whole_words

	/* Check for bad alignment of source */
	testl	$7, %esi
	/* Already aligned */
	jz	.L_8byte_aligned

	/* Copy one byte at a time until source is 8-byte aligned */
	movl	%esi, %ecx
	andl	$7, %ecx
	subl	$8, %ecx
	negl	%ecx
	subl	%ecx, %edx
	.p2align 4
.L_read_leading_b0:
	movb	(%rsi), %al
.L_write_leading_b0:
	movb	%al, (%rdi)
	addq	$1, %rsi
	addq	$1, %rdi
	subl	$1, %ecx
	jz	.L_8byte_aligned
.L_read_leading_b1:
	movb	(%rsi), %al
.L_write_leading_b1:
	movb	%al, (%rdi)
	addq	$1, %rsi
	addq	$1, %rdi
	subl	$1, %ecx
	jnz	.L_read_leading_b0

.L_8byte_aligned:
	movl	%edx, %ecx
	andl	$7, %edx
	shrl	$3, %ecx
	jz	.L_no_whole_words

	.p2align 4
.L_read_words0:
	movq	(%rsi), %r8
.L_write_words0:
	movq	%r8, (%rdi)
	addq	$8, %rsi
	addq	$8, %rdi
	subl	$1, %ecx
	jz	.L_no_whole_words
.L_read_words1:
	movq	(%rsi), %r8
.L_write_words1:
	movq	%r8, (%rdi)
	addq	$8, %rsi
	addq	$8, %rdi
	subl	$1, %ecx
	jnz	.L_read_words0

	/* Any trailing bytes? */
.L_no_whole_words:
	andl	%edx, %edx
	jz	.L_done_memcpy_trap

	/* Copy trailing bytes */
	movl	%edx, %ecx
	.p2align 4
.L_read_trailing_b0:
	movb	(%rsi), %al
.L_write_trailing_b0:
	movb	%al, (%rdi)
	addq	$1, %rsi
	addq	$1, %rdi
	subl	$1, %ecx
	jz	.L_done_memcpy_trap
.L_read_trailing_b1:
	movb	(%rsi), %al
.L_write_trailing_b1:
	movb	%al, (%rdi)
	addq	$1, %rsi
	addq	$1, %rdi
	subl	$1, %ecx
	jnz	.L_read_trailing_b0

	/* Copy successful. Return zero */
.L_done_memcpy_trap:
	xorl	%eax, %eax
.L_done:
	RET

	/*
	 * Return number of bytes not copied for any failure. Note that
	 * there is no "tail" handling since the source buffer is 8-byte
	 * aligned and poison is cacheline aligned.
	 */
.E_read_words:
	shll	$3, %ecx
.E_leading_bytes:
	addl	%edx, %ecx
.E_trailing_bytes:
	mov	%ecx, %eax
	jmp	.L_done

	/*
	 * For write fault handling, given the destination is unaligned,
	 * we handle faults on multi-byte writes with a byte-by-byte
	 * copy up to the write-protected page.
	 */
.E_write_words:
	shll	$3, %ecx
	addl	%edx, %ecx
	movl	%ecx, %edx
	jmp	copy_mc_fragile_handle_tail

	_ASM_EXTABLE_TYPE(.L_read_leading_b0, .E_leading_bytes, EX_TYPE_DEFAULT_MCE_SAFE)
	_ASM_EXTABLE_TYPE(.L_read_leading_b1, .E_leading_bytes, EX_TYPE_DEFAULT_MCE_SAFE)
	_ASM_EXTABLE_TYPE(.L_read_words0, .E_read_words, EX_TYPE_DEFAULT_MCE_SAFE)
	_ASM_EXTABLE_TYPE(.L_read_words1, .E_read_words, EX_TYPE_DEFAULT_MCE_SAFE)
	_ASM_EXTABLE_TYPE(.L_read_trailing_b0, .E_trailing_bytes, EX_TYPE_DEFAULT_MCE_SAFE)
	_ASM_EXTABLE_TYPE(.L_read_trailing_b1, .E_trailing_bytes, EX_TYPE_DEFAULT_MCE_SAFE)
	_ASM_EXTABLE(.L_write_leading_b0, .E_leading_bytes)
	_ASM_EXTABLE(.L_write_leading_b1, .E_leading_bytes)
	_ASM_EXTABLE(.L_write_words0, .E_write_words)
	_ASM_EXTABLE(.L_write_words1, .E_write_words)
	_ASM_EXTABLE(.L_write_trailing_b0, .E_trailing_bytes)
	_ASM_EXTABLE(.L_write_trailing_b1, .E_trailing_bytes)

SYM_FUNC_END(copy_mc_fragile)
#endif /* CONFIG_X86_MCE */

/*
 * copy_mc_enhanced_fast_string - memory copy with exception handling
 *
 * Fast string copy + fault / exception handling. If the CPU does
 * support machine check exception recovery, but does not support
 * recovering from fast-string exceptions then this CPU needs to be
 * added to the copy_mc_fragile_key set of quirks. Otherwise, absent any
 * machine check recovery support this version should be no slower than
 * standard memcpy.
 */
SYM_FUNC_START(copy_mc_enhanced_fast_string)
	movq	%rdi, %rax
	movq	%rdx, %rcx
.L_copy:
	rep movsb
	/* Copy successful. Return zero */
	xorl	%eax, %eax
	RET

.E_copy:
	/*
	 * On fault %rcx is updated such that the copy instruction could
	 * optionally be restarted at the fault position, i.e. it
	 * contains 'bytes remaining'. A non-zero return indicates error
	 * to copy_mc_generic() users, or indicate short transfers to
	 * user-copy routines.
	 */
	movq	%rcx, %rax
	RET

	_ASM_EXTABLE_TYPE(.L_copy, .E_copy, EX_TYPE_DEFAULT_MCE_SAFE)

SYM_FUNC_END(copy_mc_enhanced_fast_string)
#endif /* !CONFIG_UML */
