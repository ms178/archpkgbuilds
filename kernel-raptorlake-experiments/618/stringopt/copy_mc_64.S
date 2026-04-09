/* SPDX-License-Identifier: GPL-2.0-only */
/* Copyright(c) 2016-2020 Intel Corporation. All rights reserved. */

#include <linux/linkage.h>
#include <asm/asm.h>

#ifndef CONFIG_UML

#ifdef CONFIG_X86_MCE

/*
 * copy_mc_fragile - copy memory with indication if an exception / fault happened
 *
 * The 'fragile' version is opted into by platform quirks and takes
 * pains to avoid unrecoverable corner cases like 'fast-string'
 * instruction sequences, and consuming poison across a cacheline
 * boundary. The non-fragile version is equivalent to memcpy()
 * regardless of CPU machine-check-recovery capability.
 */
SYM_FUNC_START(copy_mc_fragile)
	cmpl $8, %edx
	/* Less than 8 bytes? Go to byte copy loop */
	jb .L_no_whole_words

	/* Check for bad alignment of source */
	testl $7, %esi
	/* Already aligned */
	jz .L_8byte_aligned

	/* Copy one byte at a time until source is 8-byte aligned */
	movl %esi, %ecx
	andl $7, %ecx
	subl $8, %ecx
	negl %ecx
	subl %ecx, %edx

	.p2align 5
.L_read_b0:
	movb (%rsi), %al
.L_write_b0:
	movb %al, (%rdi)
	subl $1, %ecx
	jz .L_8byte_aligned
	movb 1(%rsi), %al
.L_write_b1:
	movb %al, 1(%rdi)
	subl $1, %ecx
	jz .L_8byte_aligned_1
	movb 2(%rsi), %al
.L_write_b2:
	movb %al, 2(%rdi)
	subl $1, %ecx
	jz .L_8byte_aligned_2
	movb 3(%rsi), %al
.L_write_b3:
	movb %al, 3(%rdi)
	addq $4, %rsi
	addq $4, %rdi
	subl $1, %ecx
	jnz .L_read_b0
	jmp .L_8byte_aligned

.L_8byte_aligned_1:
	addq $1, %rsi
	addq $1, %rdi
	jmp .L_8byte_aligned

.L_8byte_aligned_2:
	addq $2, %rsi
	addq $2, %rdi
	jmp .L_8byte_aligned

.L_8byte_aligned:
	movl %edx, %ecx
	andl $7, %edx
	shrl $3, %ecx
	jz .L_no_whole_words

	.p2align 5
.L_read_w0:
	movq (%rsi), %r8
.L_write_w0:
	movq %r8, (%rdi)
	subl $1, %ecx
	jz .L_no_whole_words
	movq 8(%rsi), %r8
.L_write_w1:
	movq %r8, 8(%rdi)
	subl $1, %ecx
	jz .L_no_whole_words_8
	movq 16(%rsi), %r8
.L_write_w2:
	movq %r8, 16(%rdi)
	subl $1, %ecx
	jz .L_no_whole_words_16
	movq 24(%rsi), %r8
.L_write_w3:
	movq %r8, 24(%rdi)
	addq $32, %rsi
	addq $32, %rdi
	subl $1, %ecx
	jnz .L_read_w0
	jmp .L_no_whole_words

.L_no_whole_words_8:
	addq $8, %rsi
	addq $8, %rdi
	jmp .L_no_whole_words

.L_no_whole_words_16:
	addq $16, %rsi
	addq $16, %rdi
	jmp .L_no_whole_words

	/* Any trailing bytes? */
.L_no_whole_words:
	andl %edx, %edx
	jz .L_done_memcpy_trap

	/* Copy trailing bytes */
	movl %edx, %ecx

	.p2align 5
.L_read_t0:
	movb (%rsi), %al
.L_write_t0:
	movb %al, (%rdi)
	subl $1, %ecx
	jz .L_done_memcpy_trap
	movb 1(%rsi), %al
.L_write_t1:
	movb %al, 1(%rdi)
	subl $1, %ecx
	jz .L_done_memcpy_trap_1
	movb 2(%rsi), %al
.L_write_t2:
	movb %al, 2(%rdi)
	subl $1, %ecx
	jz .L_done_memcpy_trap_2
	movb 3(%rsi), %al
.L_write_t3:
	movb %al, 3(%rdi)
	addq $4, %rsi
	addq $4, %rdi
	subl $1, %ecx
	jnz .L_read_t0
	jmp .L_done_memcpy_trap

.L_done_memcpy_trap_1:
	addq $1, %rsi
	addq $1, %rdi
	jmp .L_done_memcpy_trap

.L_done_memcpy_trap_2:
	addq $2, %rsi
	addq $2, %rdi
	jmp .L_done_memcpy_trap

	/* Copy successful. Return zero */
.L_done_memcpy_trap:
	xorl %eax, %eax
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
	jmp copy_mc_fragile_handle_tail

	_ASM_EXTABLE_TYPE(.L_read_b0, .E_leading_bytes, EX_TYPE_DEFAULT_MCE_SAFE)
	_ASM_EXTABLE_TYPE(.L_read_b1, .E_leading_bytes, EX_TYPE_DEFAULT_MCE_SAFE)
	_ASM_EXTABLE_TYPE(.L_read_b2, .E_leading_bytes, EX_TYPE_DEFAULT_MCE_SAFE)
	_ASM_EXTABLE_TYPE(.L_read_b3, .E_leading_bytes, EX_TYPE_DEFAULT_MCE_SAFE)

	_ASM_EXTABLE_TYPE(.L_read_w0, .E_read_words, EX_TYPE_DEFAULT_MCE_SAFE)
	_ASM_EXTABLE_TYPE(.L_read_w1, .E_read_words, EX_TYPE_DEFAULT_MCE_SAFE)
	_ASM_EXTABLE_TYPE(.L_read_w2, .E_read_words, EX_TYPE_DEFAULT_MCE_SAFE)
	_ASM_EXTABLE_TYPE(.L_read_w3, .E_read_words, EX_TYPE_DEFAULT_MCE_SAFE)

	_ASM_EXTABLE_TYPE(.L_read_t0, .E_trailing_bytes, EX_TYPE_DEFAULT_MCE_SAFE)
	_ASM_EXTABLE_TYPE(.L_read_t1, .E_trailing_bytes, EX_TYPE_DEFAULT_MCE_SAFE)
	_ASM_EXTABLE_TYPE(.L_read_t2, .E_trailing_bytes, EX_TYPE_DEFAULT_MCE_SAFE)
	_ASM_EXTABLE_TYPE(.L_read_t3, .E_trailing_bytes, EX_TYPE_DEFAULT_MCE_SAFE)

	_ASM_EXTABLE(.L_write_b0, .E_leading_bytes)
	_ASM_EXTABLE(.L_write_b1, .E_leading_bytes)
	_ASM_EXTABLE(.L_write_b2, .E_leading_bytes)
	_ASM_EXTABLE(.L_write_b3, .E_leading_bytes)

	_ASM_EXTABLE(.L_write_w0, .E_write_words)
	_ASM_EXTABLE(.L_write_w1, .E_write_words)
	_ASM_EXTABLE(.L_write_w2, .E_write_words)
	_ASM_EXTABLE(.L_write_w3, .E_write_words)

	_ASM_EXTABLE(.L_write_t0, .E_trailing_bytes)
	_ASM_EXTABLE(.L_write_t1, .E_trailing_bytes)
	_ASM_EXTABLE(.L_write_t2, .E_trailing_bytes)
	_ASM_EXTABLE(.L_write_t3, .E_trailing_bytes)

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
	movq %rdi, %rax
	movq %rdx, %rcx
	.p2align 4
.L_copy:
	rep movsb
	/* Copy successful. Return zero */
	xorl %eax, %eax
	RET

.E_copy:
	/*
	 * On fault %rcx is updated such that the copy instruction could
	 * optionally be restarted at the fault position, i.e. it
	 * contains 'bytes remaining'. A non-zero return indicates error
	 * to copy_mc_generic() users, or indicate short transfers to
	 * user-copy routines.
	 */
	movq %rcx, %rax
	RET

	_ASM_EXTABLE_TYPE(.L_copy, .E_copy, EX_TYPE_DEFAULT_MCE_SAFE)

SYM_FUNC_END(copy_mc_enhanced_fast_string)
#endif /* !CONFIG_UML */
