/* SPDX-License-Identifier: GPL-2.0-only */
/*
 * Copyright 2008 Vitaly Mayatskikh <vmayatsk@redhat.com>
 * Copyright 2002 Andi Kleen, SuSE Labs.
 * Optimized for Intel Raptor Lake by Claude, 2025
 *
 * Functions to copy from and to user space.
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>
#include <asm/asm.h>

/*
 * rep_movs_alternative - memory copy with exception handling.
 * This version is for CPUs that don't have FSRM (Fast Short Rep Movs)
 * Optimized for Intel Raptor Lake CPUs.
 *
 * Input:
 * rdi destination
 * rsi source
 * rcx count
 *
 * Output:
 * rcx uncopied bytes or 0 if successful.
 */
SYM_FUNC_START(rep_movs_alternative)
        /* Check for zero length */
        testq %rcx,%rcx
        jz .Lexit

        /* Classify transfer size */
        cmpq $128,%rcx
        jae .Llarge
        cmpq $32,%rcx
        jae .Lmedium
        cmpq $8,%rcx
        jae .Lword

        /* 1-7 bytes: byte-by-byte copy */
.Lcopy_user_tail:
0:      movb (%rsi),%al
1:      movb %al,(%rdi)
        inc %rdi
        inc %rsi
        dec %rcx
        jnz .Lcopy_user_tail
.Lexit:
        RET

        _ASM_EXTABLE_UA( 0b, .Lexit)
        _ASM_EXTABLE_UA( 1b, .Lexit)

        /* 8-31 bytes: word-sized copy */
        .p2align 4
.Lword:
2:      movq (%rsi),%rax
3:      movq %rax,(%rdi)
        addq $8,%rsi
        addq $8,%rdi
        subq $8,%rcx
        jz .Lexit            /* Exactly 8 bytes copied */
        cmpq $8,%rcx         /* 8 or more bytes left? */
        jae .Lword           /* Yes, continue with words */
        jmp .Lcopy_user_tail /* Handle 1-7 remaining bytes */

        _ASM_EXTABLE_UA( 2b, .Lcopy_user_tail)
        _ASM_EXTABLE_UA( 3b, .Lcopy_user_tail)

        /* 32-127 bytes: simpler two-phase approach for medium transfers */
        .p2align 4
.Lmedium:
        /* Handle first 32 bytes in a single go */
4:      movq 0*8(%rsi),%rax
5:      movq 1*8(%rsi),%rdx
6:      movq 2*8(%rsi),%r8
7:      movq 3*8(%rsi),%r9

8:      movq %rax,0*8(%rdi)
9:      movq %rdx,1*8(%rdi)
10:     movq %r8,2*8(%rdi)
11:     movq %r9,3*8(%rdi)

        /* Update pointers and count */
        addq $32,%rsi
        addq $32,%rdi
        subq $32,%rcx        /* Adjust for first 32 bytes */

        /* Handle remaining bytes */
        cmpq $32,%rcx        /* 32 or more bytes left? */
        jae .Lmedium         /* Yes, process another 32-byte chunk */
        cmpq $8,%rcx         /* 8 or more bytes left? */
        jae .Lword           /* Yes, switch to word-based copy */
        testq %rcx,%rcx      /* Any bytes left? */
        jnz .Lcopy_user_tail /* Yes, handle remaining 1-7 bytes */
        RET                  /* No, we're done */

        /* Exception table for medium path */
        _ASM_EXTABLE_UA( 4b, .Lmedium_fault)
        _ASM_EXTABLE_UA( 5b, .Lmedium_fault)
        _ASM_EXTABLE_UA( 6b, .Lmedium_fault)
        _ASM_EXTABLE_UA( 7b, .Lmedium_fault)
        _ASM_EXTABLE_UA( 8b, .Lmedium_fault)
        _ASM_EXTABLE_UA( 9b, .Lmedium_fault)
        _ASM_EXTABLE_UA(10b, .Lmedium_fault)
        _ASM_EXTABLE_UA(11b, .Lmedium_fault)

.Lmedium_fault:
        /* Simply return current rcx value (remaining bytes) */
        RET

        /* 128+ bytes: large transfers with alignment and/or rep movsb */
.Llarge:
0:      ALTERNATIVE "jmp .Llarge_movsq", "rep movsb", X86_FEATURE_ERMS
1:      RET

        _ASM_EXTABLE_UA( 0b, 1b)

.Llarge_movsq:
        /* Save original count */
        movq %rcx,%r10

        /* Optional alignment to 64-byte boundary (cache line) */
        movq %rdi,%rax
        andl $7,%eax         /* Check 8-byte alignment first */
        jz .Lmovsq_aligned

        /* Handle up to 7 bytes for quadword alignment */
        movl $8,%r8d
        subl %eax,%r8d       /* r8d = bytes to 8-byte alignment */
        movq %r8,%rcx

12:     rep movsb

        /* Update remaining bytes */
        subq %r8,%r10
        movq %r10,%rcx
        testq %rcx,%rcx
        jz .Lexit

.Lmovsq_aligned:
        /* Prefetch for large transfers (128+ bytes away) */
13:     prefetcht0 128(%rsi)

        /* Calculate number of 8-byte chunks and remainder */
        movq %rcx,%rax       /* Save original count */
        shrq $3,%rcx         /* rcx = number of 8-byte chunks */
        andl $7,%eax         /* eax = remainder bytes */

        testq %rcx,%rcx
        jz .Lmovsq_tail

        /* Copy 8-byte aligned chunks */
14:     rep movsq

.Lmovsq_tail:
        /* Handle remainder bytes (0-7) */
        movl %eax,%ecx
        testl %ecx,%ecx
        jz .Lexit

15:     rep movsb
        xorl %ecx,%ecx       /* Set rcx to 0 (success) */
        RET

        /* Exception handling for large transfers */
16:     /* Calculate remaining bytes for movsq fault */
        leaq (%rax,%rcx,8),%rcx
        jmp .Lcopy_user_tail

        _ASM_EXTABLE_UA(12b, .Llarge_fault_align)
        _ASM_EXTABLE_UA(13b, .Lmovsq_fault_prefetch)
        _ASM_EXTABLE_UA(14b, 16b)
        _ASM_EXTABLE_UA(15b, .Lexit)

.Llarge_fault_align:
        /* On fault during alignment - return remaining */
        movq %r10,%rcx
        addq %r8,%rcx        /* Add alignment bytes */
        subq %rax,%rcx       /* Subtract processed bytes */
        RET

.Lmovsq_fault_prefetch:
        /* On fault during prefetch - whole count remains */
        movq %r10,%rcx
        RET

SYM_FUNC_END(rep_movs_alternative)
EXPORT_SYMBOL(rep_movs_alternative)
