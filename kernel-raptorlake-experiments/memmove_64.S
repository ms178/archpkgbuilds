/* SPDX-License-Identifier: GPL-2.0 */
/*
 * Normally compiler builtins are used, but sometimes the compiler calls out
 * of line code. Based on asm-i386/string.h.
 *
 * This assembly file is re-written from memmove_64.c file.
 *      - Copyright 2011 Fenghua Yu <fenghua.yu@intel.com>
 */
#include <linux/export.h>
#include <linux/linkage.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>

#undef memmove

.section .noinstr.text, "ax"

/*
 * Implement memmove(). This can handle overlap between src and dst.
 *
 * Input:
 * rdi: dest
 * rsi: src
 * rdx: count
 *
 * Output:
 * rax: dest
 */
SYM_FUNC_START(__memmove)

        mov %rdi, %rax
        test %rdx, %rdx
        jz 13f
# Early exit for zero-length copies

        # Decide forward/backward copy mode
        cmp %rdi, %rsi
        jge .Lmemmove_begin_forward
        mov %rsi, %r8
        add %rdx, %r8
        cmp %rdi, %r8
        jg 2f
        jmp .Lmemmove_begin_forward
# No overlap, use forward copy

#define CHECK_LEN       cmp $0x20, %rdx; jb 1f
#define MEMMOVE_BYTES   movq %rdx, %rcx; rep movsb; RET

.Lmemmove_begin_forward:
        ALTERNATIVE_2 __stringify(CHECK_LEN), \
                      __stringify(CHECK_LEN; MEMMOVE_BYTES), X86_FEATURE_ERMS, \
                      __stringify(MEMMOVE_BYTES), X86_FEATURE_FSRM

        # Save callee-saved registers
        pushq %r12
        pushq %r13
        pushq %r14
        pushq %r15

        # Use rep movsb for aligned copies on Raptor Lake, leveraging ERMS/FSRM.
        testb $63, %dil
        jnz 3f
# Check if destination is 64-byte aligned
# If not, use register-based loop
        testb $63, %sil
        jnz 3f
# Check if source is 64-byte aligned
# If not, use register-based loop
        jmp 4f
# Both aligned, use rep movsb
3:
        # We gobble 64 bytes forward in each loop, matching Raptor Lake cache line size.
5:
        cmpq $0x40, %rdx         # Check if we have at least 64 bytes left
        jb 1f                    # If not, handle remainder

        sub $0x40, %rdx          # Decrement remaining count
        movq 0*8(%rsi), %r11
        movq %r11, 0*8(%rdi)
        movq 1*8(%rsi), %r10
        movq %r10, 1*8(%rdi)
        movq 2*8(%rsi), %r9
        movq %r9, 2*8(%rdi)
        movq 3*8(%rsi), %r8
        movq %r8, 3*8(%rdi)
        movq 4*8(%rsi), %r12
        movq %r12, 4*8(%rdi)
        movq 5*8(%rsi), %r13
        movq %r13, 5*8(%rdi)
        movq 6*8(%rsi), %r14
        movq %r14, 6*8(%rdi)
        movq 7*8(%rsi), %r15
        movq %r15, 7*8(%rdi)
        leaq 8*8(%rsi), %rsi
        leaq 8*8(%rdi), %rdi
        jmp 5b
        # Handle data forward by movsb for aligned copies.
        .p2align 4
4:
        # Restore callee-saved registers - not needed here since rep movsb doesn't use them
        popq %r15
        popq %r14
        popq %r13
        popq %r12

        movq %rdx, %rcx
        rep movsb
        jmp 13f
.Lmemmove_end_forward:

        # Handle data backward by movsb, leveraging ERMS/FSRM.
        .p2align 4
7:
        # Restore callee-saved registers
        popq %r15
        popq %r14
        popq %r13
        popq %r12

        movq %rdx, %rcx
        movq (%rsi), %r11
        movq %rdi, %r10
        addq %rdx, %rsi
        addq %rdx, %rdi
        decq %rsi
        decq %rdi
        std
        rep movsb
        cld
        movq %r11, (%r10)
        jmp 13f

        # Start to prepare for backward copy.
        .p2align 4
2:
        # Save callee-saved registers
        pushq %r12
        pushq %r13
        pushq %r14
        pushq %r15

        cmp $0x20, %rdx
        jb 1f
        testb $63, %dil
        jnz 6f
# Check if destination is 64-byte aligned
# If not, use register-based loop
        testb $63, %sil
        jnz 6f
# Check if source is 64-byte aligned
# If not, use register-based loop
        jmp 7b
# Both aligned, use rep movsb
6:
        # Calculate copy position to tail.
        addq %rdx, %rsi
        addq %rdx, %rdi

        # We gobble 64 bytes backward in each loop, matching Raptor Lake cache line size.
8:
        cmpq $0x40, %rdx         # Check if we have at least 64 bytes left
        jb 1f                    # If not, handle remainder

        subq $0x40, %rdx         # Decrement remaining count
        movq -1*8(%rsi), %r11
        movq %r11, -1*8(%rdi)
        movq -2*8(%rsi), %r10
        movq %r10, -2*8(%rdi)
        movq -3*8(%rsi), %r9
        movq %r9, -3*8(%rdi)
        movq -4*8(%rsi), %r8
        movq %r8, -4*8(%rdi)
        movq -5*8(%rsi), %r12
        movq %r12, -5*8(%rdi)
        movq -6*8(%rsi), %r13
        movq %r13, -6*8(%rdi)
        movq -7*8(%rsi), %r14
        movq %r14, -7*8(%rdi)
        movq -8*8(%rsi), %r15
        movq %r15, -8*8(%rdi)
        leaq -8*8(%rsi), %rsi
        leaq -8*8(%rdi), %rdi
        jmp 8b
        # Calculate copy position to head.
        # This will be reached when rdx < 0x40
1:
        # Restore callee-saved registers
        popq %r15
        popq %r14
        popq %r13
        popq %r12

        # Handle any remaining bytes
        cmpq $16, %rdx
        jb 9f
        # Move data from 16 bytes to 31 bytes using rep movsb.
        movq %rdx, %rcx
        rep movsb
        jmp 13f
        .p2align 4
9:
        cmpq $8, %rdx
        jb 10f
        # Move data from 8 bytes to 15 bytes using rep movsb.
        movq %rdx, %rcx
        rep movsb
        jmp 13f
10:
        cmpq $4, %rdx
        jb 11f
        # Move data from 4 bytes to 7 bytes using rep movsb.
        movq %rdx, %rcx
        rep movsb
        jmp 13f
11:
        cmp $2, %rdx
        jb 12f
        # Move data from 2 bytes to 3 bytes.
        movw (%rsi), %r11w
        movw %r11w, (%rdi)
        movw -2(%rsi, %rdx), %r10w
        movw %r10w, -2(%rdi, %rdx)
        jmp 13f
12:
        cmp $1, %rdx
        jb 13f
        # Move data for 1 byte.
        movb (%rsi), %r11b
        movb %r11b, (%rdi)
13:
        RET
SYM_FUNC_END(__memmove)
EXPORT_SYMBOL(__memmove)

SYM_FUNC_ALIAS_MEMFUNC(memmove, __memmove)
EXPORT_SYMBOL(memmove)
