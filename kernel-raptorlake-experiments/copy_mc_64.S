/* SPDX-License-Identifier: GPL-2.0-only */
/* Copyright(c) 2016-2020 Intel Corporation. All rights reserved. */
/* Optimized for Intel Raptor Lake by [Your Name], 2025 */

#include <linux/linkage.h>
#include <asm/asm.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>

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
 *
 * Optimized for Intel Raptor Lake with wider chunk copies and larger
 * copy operations, conditional on CPU feature checks.
 */
SYM_FUNC_START(copy_mc_fragile)
        /* Save callee-saved registers we'll use */
        pushq %rbx
        pushq %r12
        pushq %r13
        pushq %r14
        pushq %r15

        /* Save original destination for return */
        movq %rdi, %r12

        cmpl $8, %edx
        /* Less than 8 bytes? Go to byte copy loop */
        jb .L_no_whole_words

        /* Check for bad alignment of source */
        movl %esi, %ecx
        andl $7, %ecx
        jz .L_8byte_aligned

        /* Calculate bytes to copy to reach 8-byte alignment */
        negl %ecx
        addl $8, %ecx

        /* Byte-by-byte copy for alignment is safest approach */
        subl %ecx, %edx
.L_read_leading_bytes:
        movb (%rsi), %al
.L_write_leading_bytes:
        movb %al, (%rdi)
        addq $1, %rsi
        addq $1, %rdi
        subl $1, %ecx
        jnz .L_read_leading_bytes

.L_8byte_aligned:
        movl %edx, %ecx
        /* Optimize for large copies on Raptor Lake: Copy 32-byte chunks */
        cmpl $64, %ecx  /* Threshold for large copy optimization */
        jb .L_handle_small_copy
        /* Use ALTERNATIVE to enable optimized path only if FSRM is supported */
        ALTERNATIVE "jmp .L_handle_small_copy", "", X86_FEATURE_FSRM
        movl %edx, %ecx
        andl $31, %edx  /* Save remainder for trailing bytes */
        shrl $5, %ecx   /* Divide by 32 for 32-byte chunks */
        jz .L_handle_remainder

.L_read_32bytes:
        /* Optimize for Raptor Lake pipeline with separate registers */
        movq (%rsi), %r8
        movq 8(%rsi), %r9
        movq 16(%rsi), %r10
        movq 24(%rsi), %r11
.L_write_32bytes_1:
        movq %r8, (%rdi)
.L_write_32bytes_2:
        movq %r9, 8(%rdi)
.L_write_32bytes_3:
        movq %r10, 16(%rdi)
.L_write_32bytes_4:
        movq %r11, 24(%rdi)
        addq $32, %rsi
        addq $32, %rdi
        subl $1, %ecx
        jnz .L_read_32bytes

.L_handle_remainder:
        /* Handle remaining bytes < 32 */
        movl %edx, %ecx
        andl $7, %edx   /* Save remainder for trailing bytes < 8 */
        shrl $3, %ecx   /* Divide by 8 for 8-byte chunks */
        jz .L_no_whole_words

.L_read_words:
        movq (%rsi), %r8
.L_write_words:
        movq %r8, (%rdi)
        addq $8, %rsi
        addq $8, %rdi
        subl $1, %ecx
        jnz .L_read_words
        jmp .L_no_whole_words

.L_handle_small_copy:
        /* Original 8-byte copy loop for small copies or non-Raptor Lake CPUs */
        movl %edx, %ecx
        andl $7, %edx   /* Save remainder for trailing bytes */
        shrl $3, %ecx   /* Divide by 8 for 8-byte chunks */
        jz .L_no_whole_words

.L_read_words_small:
        movq (%rsi), %r8
.L_write_words_small:
        movq %r8, (%rdi)
        addq $8, %rsi
        addq $8, %rdi
        subl $1, %ecx
        jnz .L_read_words_small

        /* Any trailing bytes? */
.L_no_whole_words:
        andl %edx, %edx
        jz .L_done_memcpy_trap

        /* Copy trailing bytes */
        movl %edx, %ecx
.L_read_trailing_bytes:
        movb (%rsi), %al
.L_write_trailing_bytes:
        movb %al, (%rdi)
        addq $1, %rsi
        addq $1, %rdi
        subl $1, %ecx
        jnz .L_read_trailing_bytes

        /* Copy successful. Return zero */
.L_done_memcpy_trap:
        xorl %eax, %eax
.L_done:
        /* Return original destination pointer */
        movq %r12, %rdi
        /* Restore callee-saved registers */
        popq %r15
        popq %r14
        popq %r13
        popq %r12
        popq %rbx
        RET

        /*
         * Return number of bytes not copied for any failure.
         */
.E_read_words:
        shll    $3, %ecx
.E_leading_bytes:
        addl    %edx, %ecx
.E_trailing_bytes:
        movl    %ecx, %eax
        jmp     .L_done

        /*
         * For write fault handling, given the destination is unaligned,
         * we handle faults on multi-byte writes with remaining bytes count.
         */
.E_write_words:
        shll    $3, %ecx
        addl    %edx, %ecx
        movl    %ecx, %eax
        jmp     .L_done

        /* Exception table entries for reads - machine check recoverable */
        _ASM_EXTABLE_TYPE(.L_read_leading_bytes, .E_leading_bytes, EX_TYPE_DEFAULT_MCE_SAFE)
        _ASM_EXTABLE_TYPE(.L_read_words, .E_read_words, EX_TYPE_DEFAULT_MCE_SAFE)
        _ASM_EXTABLE_TYPE(.L_read_words_small, .E_read_words, EX_TYPE_DEFAULT_MCE_SAFE)
        _ASM_EXTABLE_TYPE(.L_read_trailing_bytes, .E_trailing_bytes, EX_TYPE_DEFAULT_MCE_SAFE)
        _ASM_EXTABLE_TYPE(.L_read_32bytes, .E_read_words, EX_TYPE_DEFAULT_MCE_SAFE)

        /* Exception table entries for writes - standard handling */
        _ASM_EXTABLE(.L_write_leading_bytes, .E_leading_bytes)
        _ASM_EXTABLE(.L_write_words, .E_write_words)
        _ASM_EXTABLE(.L_write_words_small, .E_write_words)
        _ASM_EXTABLE(.L_write_trailing_bytes, .E_trailing_bytes)
        _ASM_EXTABLE(.L_write_32bytes_1, .E_write_words)
        _ASM_EXTABLE(.L_write_32bytes_2, .E_write_words)
        _ASM_EXTABLE(.L_write_32bytes_3, .E_write_words)
        _ASM_EXTABLE(.L_write_32bytes_4, .E_write_words)
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
 *
 * Optimized for Intel Raptor Lake with a fast path for small copies.
 */
SYM_FUNC_START(copy_mc_enhanced_fast_string)
        /* Save callee-saved registers we'll use */
        pushq %r12
        pushq %r13
        pushq %r14

        /* Save original destination for return */
        movq %rdi, %r12

        movq %rdx, %rcx
        cmpl $16, %ecx
        jb .L_small_copy

.L_copy:
        rep movsb
        /* Copy successful. Return zero */
        xorl %eax, %eax
        jmp .L_exit_restore

.L_small_copy:
        /* Handle small copies with discrete movq */
        cmpl $8, %ecx
        jb .L_small_copy_bytes
        /* 8-byte read for small copy optimization */
        movq (%rsi), %r13
.L_small_copy_write_8:
        /* 8-byte write */
        movq %r13, (%rdi)
        addq $8, %rsi
        addq $8, %rdi
        subl $8, %ecx
        jz .L_small_copy_done
.L_small_copy_bytes:
        /* Handle remaining bytes with rep movsb */
        rep movsb
.L_small_copy_done:
        xorl %eax, %eax

.L_exit_restore:
        /* Return original destination pointer */
        movq %r12, %rax
        /* Restore callee-saved registers */
        popq %r14
        popq %r13
        popq %r12
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
        jmp .L_exit_restore

        /* Exception table entries for fast string function */
        _ASM_EXTABLE_TYPE(.L_copy, .E_copy, EX_TYPE_DEFAULT_MCE_SAFE)
        _ASM_EXTABLE_TYPE(.L_small_copy_bytes, .E_copy, EX_TYPE_DEFAULT_MCE_SAFE)
        _ASM_EXTABLE_TYPE(.L_small_copy, .E_copy, EX_TYPE_DEFAULT_MCE_SAFE)
        _ASM_EXTABLE(.L_small_copy_write_8, .E_copy)
SYM_FUNC_END(copy_mc_enhanced_fast_string)
EXPORT_SYMBOL(copy_mc_enhanced_fast_string)
#endif /* !CONFIG_UML */
