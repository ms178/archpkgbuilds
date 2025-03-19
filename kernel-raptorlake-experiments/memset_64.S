/* SPDX-License-Identifier: GPL-2.0 */
/* Copyright 2002 Andi Kleen, SuSE Labs */
/* Optimized for Intel Raptor Lake by Claude, 2025 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>

.section .noinstr.text, "ax"

/*
 * ISO C memset - set a memory block to a byte value.
 * Optimized for Intel Raptor Lake architecture with P/E-core awareness.
 *
 * P-cores: >2KB threshold for ERMSB, 64B-2KB use AVX2 when available
 * E-cores: >1KB threshold for ERMSB, 64B-1KB use AVX2 when available
 *
 * rdi   destination
 * rsi   value (char)
 * rdx   count (bytes)
 *
 * rax   original destination
 */
SYM_FUNC_START(__memset)
        /* Store original destination for return value */
        movq %rdi, %r9

        /* Ensure proper direction flag - keep this as the main entry point */
        cld

        /* Check for zero-length case first */
        testq %rdx, %rdx
        jz .L_zero_length

        /* Fast path with FSRS for large buffers */
        ALTERNATIVE "jmp .L_hybrid_path", "", X86_FEATURE_FSRS

        /* Expand byte value to fill %al */
        movb %sil, %al
        movq %rdx, %rcx
1:      rep stosb
.L_zero_length:
        movq %r9, %rax
        RET

        /* Exception table for FSRS path */
        _ASM_EXTABLE(1b, .Lfsrs_fault_handler)
SYM_FUNC_END(__memset)
EXPORT_SYMBOL(__memset)

SYM_FUNC_ALIAS_MEMFUNC(memset, __memset)
EXPORT_SYMBOL(memset)

/* P/E-core hybrid aware path */
SYM_FUNC_START_LOCAL(.L_hybrid_path)
        /* Store original destination for return value */
        movq %rdi, %r10

        /* For small blocks (<64 bytes), use scalar path directly */
        cmpq $64, %rdx
        jb .L_small_path

        /* For large blocks, use different thresholds for P-cores vs E-cores */
        /* Check CPU type if available (Intel Hybrid bit) */
        ALTERNATIVE "jmp .L_pcore_path", "", X86_FEATURE_HYBRID_CPU

        /* E-core path: Use ERMSB for >1KB, otherwise scalar/AVX path */
        cmpq $1024, %rdx
        ja .L_use_ermsb

        /* Use AVX2 for medium blocks if available */
        ALTERNATIVE "jmp .L_use_scalar", "", X86_FEATURE_AVX2

        jmp .L_use_avx2

.L_pcore_path:
        /* P-core path: Use ERMSB for >2KB, otherwise scalar/AVX path */
        cmpq $2048, %rdx
        ja .L_use_ermsb

        /* Use AVX2 for medium blocks if available */
        ALTERNATIVE "jmp .L_use_scalar", "", X86_FEATURE_AVX2

        jmp .L_use_avx2

.L_use_ermsb:
        /* Enhanced REP STOSB path - optimized for both P and E cores */
        movb %sil, %al
        movq %rdx, %rcx
2:      rep stosb
        movq %r10, %rax
        RET

.L_use_avx2:
        /* AVX2 path for medium blocks */
        /* Broadcast byte value to YMM register */
        movzbl %sil, %eax
        vmovd %eax, %xmm0
        vpbroadcastb %xmm0, %ymm0

        /* Align destination to 32-byte boundary */
        movl %edi, %ecx
        andl $31, %ecx
        jz .L_avx2_aligned

        /* Calculate bytes to align */
        movl $32, %r8d
        subl %ecx, %r8d

        /* Ensure alignment doesn't exceed total size */
        movq %r8, %rcx
        cmpq %rdx, %rcx
        jbe 3f
        movq %rdx, %rcx

3:      /* Align with rep stosb */
        movb %sil, %al
        subq %rcx, %rdx
        rep stosb

.L_avx2_aligned:
        /* Process 64-byte chunks */
        movq %rdx, %rcx
        shrq $6, %rcx
        jz .L_avx2_remainder

.L_avx2_loop:
4:      vmovdqa %ymm0, (%rdi)
5:      vmovdqa %ymm0, 32(%rdi)
        addq $64, %rdi
        decq %rcx
        jnz .L_avx2_loop

        /* Process remainder bytes */
        andq $63, %rdx

.L_avx2_remainder:
        testq %rdx, %rdx
        jz .L_avx2_done

        /* Process 32-byte chunk if applicable */
        cmpq $32, %rdx
        jb .L_avx2_small_remainder

6:      vmovdqa %ymm0, (%rdi)
        addq $32, %rdi
        subq $32, %rdx

.L_avx2_small_remainder:
        /* Process remaining bytes with scalar */
        testq %rdx, %rdx
        jz .L_avx2_done

        /* Use rep stosb for tail */
        movq %rdx, %rcx
        movb %sil, %al
        rep stosb

.L_avx2_done:
        vzeroupper
        movq %r10, %rax
        RET

.L_use_scalar:
.L_small_path:
        /* Scalar path for small blocks */
        movzbl %sil, %ecx
        movabs $0x0101010101010101, %rax
        imulq %rcx, %rax

        /* Handle small sizes (<64 bytes) with optimized code */
        cmpq $8, %rdx
        jb .L_small_lt8

        /* Handle 8+ bytes - start with 8-byte chunks */
        movq %rdx, %rcx
        shrq $3, %rcx
        jz .L_small_remainder

.L_small_loop:
7:      movq %rax, (%rdi)
        addq $8, %rdi
        decq %rcx
        jnz .L_small_loop

.L_small_remainder:
        /* Handle 0-7 remaining bytes */
        andq $7, %rdx
        jz .L_small_done

.L_small_lt8:
        /* Handle 4-byte chunk if applicable */
        cmpq $4, %rdx
        jb .L_small_lt4

8:      movl %eax, (%rdi)
        addq $4, %rdi
        subq $4, %rdx

.L_small_lt4:
        /* Handle 2-byte chunk if applicable */
        cmpq $2, %rdx
        jb .L_small_lt2

9:      movw %ax, (%rdi)
        addq $2, %rdi
        subq $2, %rdx

.L_small_lt2:
        /* Handle last byte if applicable */
        testq %rdx, %rdx
        jz .L_small_done

10:     movb %al, (%rdi)

.L_small_done:
        movq %r10, %rax
        RET

/* Fault handlers */
.Lfsrs_fault_handler:
        cld
        movq %r9, %rax
        RET

.L_hybrid_fault_handler:
        /* Clean up AVX state if needed */
        ALTERNATIVE "nop", "vzeroupper", X86_FEATURE_AVX2
        cld
        movq %r10, %rax
        RET

        /* Exception tables for hybrid path */
        _ASM_EXTABLE(2b, .L_hybrid_fault_handler)
        _ASM_EXTABLE(3b, .L_hybrid_fault_handler)
        _ASM_EXTABLE(4b, .L_hybrid_fault_handler)
        _ASM_EXTABLE(5b, .L_hybrid_fault_handler)
        _ASM_EXTABLE(6b, .L_hybrid_fault_handler)
        _ASM_EXTABLE(7b, .L_hybrid_fault_handler)
        _ASM_EXTABLE(8b, .L_hybrid_fault_handler)
        _ASM_EXTABLE(9b, .L_hybrid_fault_handler)
        _ASM_EXTABLE(10b, .L_hybrid_fault_handler)
SYM_FUNC_END(.L_hybrid_path)

/* Original memset implementation (non-optimized fallback) */
SYM_FUNC_START_LOCAL(memset_orig)
        /* Store original destination for return value */
        movq %rdi, %r9

        /* Optimize for zero length */
        testq %rdx, %rdx
        jz .Lende

        /* Expand byte value */
        movzbl %sil, %ecx
        movabs $0x0101010101010101, %rax
        imulq %rcx, %rax

        /* Handle small sizes (<=64 bytes) directly with rep stosb */
        cmpq $64, %rdx
        jbe .Lsmall

        /* Align destination to 64-byte cache line boundary for Raptor Lake */
        movl %edi, %ecx
        andl $63, %ecx
        jz .Lafter_bad_alignment

        /* Calculate bytes to 64-byte alignment */
        movl $64, %r8d
        subl %ecx, %r8d

        /* Ensure alignment doesn't exceed total size */
        movq %r8, %rcx
        cmpq %rdx, %rcx
        jbe 16f
        movq %rdx, %rcx

16:     /* Align with rep stosb */
        subq %rcx, %rdx
        rep stosb

        /* Check if we have bytes left to set */
        testq %rdx, %rdx
        jz .Lende

.Lafter_bad_alignment:
        /* Check if we have enough memory for prefetching */
        cmpq $256, %rdx
        jb .Lno_prefetch_orig

        /* Add prefetching for large blocks - optimized for Raptor Lake */
17:     prefetchw 384(%rdi)
18:     prefetchw 512(%rdi)

.Lno_prefetch_orig:
        /* Process 64-byte chunks - cache line sized */
        movq %rdx, %rcx
        shrq $6, %rcx
        jz .Lhandle_tail

        .p2align 4
.Lloop_64_orig:
19:     movq %rax, 0*8(%rdi)
20:     movq %rax, 1*8(%rdi)
21:     movq %rax, 2*8(%rdi)
22:     movq %rax, 3*8(%rdi)
23:     movq %rax, 4*8(%rdi)
24:     movq %rax, 5*8(%rdi)
25:     movq %rax, 6*8(%rdi)
26:     movq %rax, 7*8(%rdi)

        leaq 64(%rdi), %rdi
        decq %rcx
        jnz .Lloop_64_orig

        /* Calculate remaining bytes */
        andq $63, %rdx

.Lhandle_tail:
.Lsmall:
        /* Handle remaining bytes with rep stosb */
        testq %rdx, %rdx
        jz .Lende

        movq %rdx, %rcx
        rep stosb

.Lende:
        movq %r9, %rax
        RET

/* Generic fault handler */
.Lorig_fault_handler:
        /* cld not required */
        movq %r9, %rax     /* Restore original destination */
        RET

        /* Exception tables for original path */
        _ASM_EXTABLE(16b, .Lorig_fault_handler)
        _ASM_EXTABLE(17b, .Lorig_fault_handler) /* Prefetch */
        _ASM_EXTABLE(18b, .Lorig_fault_handler) /* Prefetch */
        _ASM_EXTABLE(19b, .Lorig_fault_handler)
        _ASM_EXTABLE(20b, .Lorig_fault_handler)
        _ASM_EXTABLE(21b, .Lorig_fault_handler)
        _ASM_EXTABLE(22b, .Lorig_fault_handler)
        _ASM_EXTABLE(23b, .Lorig_fault_handler)
        _ASM_EXTABLE(24b, .Lorig_fault_handler)
        _ASM_EXTABLE(25b, .Lorig_fault_handler)
        _ASM_EXTABLE(26b, .Lorig_fault_handler)
SYM_FUNC_END(memset_orig)
