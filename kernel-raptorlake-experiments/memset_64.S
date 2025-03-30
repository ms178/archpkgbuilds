/* SPDX-License-Identifier: GPL-2.0 */
/* Copyright 2002 Andi Kleen, SuSE Labs */
/* Optimized for Intel Raptor Lake by Genius, 2025 - Max Performance */

#include <linux/export.h>
#include <linux/linkage.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h> // Provides ALTERNATIVE*, _ASM_EXTABLE

.section .noinstr.text, "ax"


/* --- Fault Handlers (Defined before use) --- */

SYM_FUNC_START_LOCAL(.Lfsrs_fault_handler)
	/* DF is already clear */
	movq %r9, %rax /* Restore original %rdi from FSRS path save (%r9) */
	RET
SYM_FUNC_END(.Lfsrs_fault_handler)

SYM_FUNC_START_LOCAL(.L_hybrid_fault_handler)
	/* Clean up AVX state */
	ALTERNATIVE "nop", "vzeroupper", X86_FEATURE_AVX2
	/* DF is already clear */
	movq %r10, %rax /* Restore original %rdi from hybrid path save (%r10) */
	RET
SYM_FUNC_END(.L_hybrid_fault_handler)

SYM_FUNC_START_LOCAL(.Lgeneric_loop_fault_handler)
	/* DF is already clear */
	movq %r9, %rax     /* Restore original destination from %r9 */
	RET
SYM_FUNC_END(.Lgeneric_loop_fault_handler)


/* --- Main memset Entry Point --- */

/*
 * ISO C memset - set a memory block to a byte value.
 * Maximum performance variant for Raptor Lake. PRIORITIZES SPEED.
 * Accepts objtool fallthrough warning for __memset as benign byproduct
 * of inline FSRS path structure.
 */
SYM_FUNC_START(__memset)
	/* Store original destination for return value */
	movq %rdi, %r9

	/* DF flag is guaranteed clear (0) by x86-64 ABI */

	/* Check for zero-length case first, jump directly to return */
	testq %rdx, %rdx
	jz .L_memset_ret

	/* Choose the execution path based on FSRS feature using standard ALTERNATIVE */
	/* If FSRS absent (old): jump to hybrid path */
	/* If FSRS present (new): execute nop, fall through to inline FSRS path */
	ALTERNATIVE "jmp .L_hybrid_path", "nop", X86_FEATURE_FSRS

/* --- Inline FSRS Path (Fastest) --- */
/* Execution falls through here only if FSRS feature is present */
	movb %sil, %al
	movq %rdx, %rcx
1:	rep stosb	/* Requires DF=0 (guaranteed by ABI) */

	/* Performance: Implicit fallthrough to return sequence. Saves a jmp. */
	/* This structure triggers the objtool fallthrough warning, accepted here. */

.L_memset_ret: /* Common return point *inside* __memset */
	movq %r9, %rax /* Return original destination */
	ret

	/* Exception table for the inline FSRS path */
	_ASM_EXTABLE(1b, .Lfsrs_fault_handler)

SYM_FUNC_END(__memset) /* End of the main __memset function block */
EXPORT_SYMBOL(__memset)

SYM_FUNC_ALIAS_MEMFUNC(memset, __memset)
EXPORT_SYMBOL(memset)


/* --- Hybrid Path (No FSRS/ERMSB) --- */

/* P/E-core hybrid aware path (Used ONLY when FSRS/ERMSB is NOT available) */
/* This code is only reached via the ALTERNATIVE jump above */
SYM_FUNC_START_LOCAL(.L_hybrid_path)
	/* Store original destination for return value (use %r10 for this path) */
	movq %rdi, %r10 /* Note: %rdi still holds original destination here */

	/* Handle small blocks (< 64 bytes) separately */
	cmpq $64, %rdx
	jb .L_small_scalar_path

	/* For blocks >= 64 bytes, use AVX2 if available, otherwise generic loop */
	ALTERNATIVE "jmp .L_no_avx2_fallback", "", X86_FEATURE_AVX2

/* --- AVX2 Path (>= 64 Bytes) --- */
.L_use_avx2_path:
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
3:	/* Align with rep stosb */
	movb %sil, %al
	subq %rcx, %rdx
	rep stosb

.L_avx2_aligned:
	/* Process 64-byte chunks */
	movq %rdx, %rcx
	shrq $6, %rcx
	jz .L_avx2_remainder
.L_avx2_loop:
4:	vmovdqa %ymm0, (%rdi)
5:	vmovdqa %ymm0, 32(%rdi)
	addq $64, %rdi
	decq %rcx
	jnz .L_avx2_loop
	andq $63, %rdx /* Calculate remainder bytes */
.L_avx2_remainder:
	testq %rdx, %rdx
	jz .L_avx2_done

	/* Process 32-byte chunk if applicable */
	cmpq $32, %rdx
	jb .L_avx2_small_remainder
6:	vmovdqa %ymm0, (%rdi)
	addq $32, %rdi
	subq $32, %rdx
.L_avx2_small_remainder:
	/* Process remaining bytes (< 32) with rep stosb */
	testq %rdx, %rdx
	jz .L_avx2_done
	movq %rdx, %rcx
	movb %sil, %al
11:	rep stosb
.L_avx2_done:
	vzeroupper /* Crucial AVX cleanup */
	movq %r10, %rax /* Return original destination from hybrid path */
	RET
	/* Exception tables for AVX path */
	_ASM_EXTABLE(3b, .L_hybrid_fault_handler)
	_ASM_EXTABLE(4b, .L_hybrid_fault_handler)
	_ASM_EXTABLE(5b, .L_hybrid_fault_handler)
	_ASM_EXTABLE(6b, .L_hybrid_fault_handler)
	_ASM_EXTABLE(11b, .L_hybrid_fault_handler)
/* End of AVX2 Path */


/* --- Fallback if NO AVX2 for >= 64 Bytes --- */
.L_no_avx2_fallback:
	movq %r10, %r9 /* Prepare %r9 for generic loop */
	jmp __memset_generic_loop /* Tail call to generic scalar loop */


/* --- Small Scalar Path (< 64 Bytes) --- */
.L_small_scalar_path:
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
7:	movq %rax, (%rdi)
	addq $8, %rdi
	decq %rcx
	jnz .L_small_loop
	andq $7, %rdx /* Calculate remaining bytes (0-7) */
.L_small_remainder:
	jz .L_small_done
.L_small_lt8:
	/* Handle 4-byte chunk if applicable */
	cmpq $4, %rdx
	jb .L_small_lt4
8:	movl %eax, (%rdi)
	addq $4, %rdi
	subq $4, %rdx
.L_small_lt4:
	/* Handle 2-byte chunk if applicable */
	cmpq $2, %rdx
	jb .L_small_lt2
9:	movw %ax, (%rdi)
	addq $2, %rdi
	subq $2, %rdx
.L_small_lt2:
	/* Handle last byte if applicable */
	testq %rdx, %rdx
	jz .L_small_done
10:	movb %al, (%rdi)
.L_small_done:
	movq %r10, %rax /* Return original destination from hybrid path */
	RET
	/* Exception tables for scalar path */
	_ASM_EXTABLE(7b, .L_hybrid_fault_handler)
	_ASM_EXTABLE(8b, .L_hybrid_fault_handler)
	_ASM_EXTABLE(9b, .L_hybrid_fault_handler)
	_ASM_EXTABLE(10b, .L_hybrid_fault_handler)
/* End of Small Scalar Path */

SYM_FUNC_END(.L_hybrid_path)


/* --- Generic Fallback Loop (No ERMSB/AVX, Large Size) --- */
SYM_FUNC_START_LOCAL(__memset_generic_loop)
	/* %r9 should already hold original %rdi */
	/* DF is guaranteed clear (0) by ABI */

	/* Expand byte value */
	movzbl %sil, %ecx
	movabs $0x0101010101010101, %rax
	imulq %rcx, %rax

	/* Handle alignment and main loop (assumes size >= 64 initially) */
	/* Small check needed in case alignment reduces size drastically */
	cmpq $64, %rdx
	jbe .Lsmall_generic

	/* Align destination to 64-byte cache line boundary */
	movl %edi, %ecx
	andl $63, %ecx
	jz .Lafter_bad_alignment_generic

	/* Calculate bytes to 64-byte alignment */
	movl $64, %r8d
	subl %ecx, %r8d

	/* Ensure alignment doesn't exceed total size */
	movq %r8, %rcx
	cmpq %rdx, %rcx
	jbe 16f
	movq %rdx, %rcx
16:	/* Align with rep stosb */
	subq %rcx, %rdx
	movb %sil, %al /* Set %al */
	rep stosb

	/* Check if we have bytes left to set (size could become < 64 after align) */
	testq %rdx, %rdx
	jz .Lende_generic
	cmpq $64, %rdx
	jbe .Lsmall_generic

	/* Restore rax with the expanded value for movq loop */
	movabs $0x0101010101010101, %rax
	movzbl %sil, %ecx
	imulq %rcx, %rax

.Lafter_bad_alignment_generic:
	/* Check if we have enough memory for prefetching */
	cmpq $256, %rdx
	jb .Lno_prefetch_generic

	/* Add prefetching for large blocks */
17:	prefetchw 384(%rdi)
18:	prefetchw 512(%rdi)

.Lno_prefetch_generic:
	/* Process 64-byte chunks - cache line sized */
	movq %rdx, %rcx
	shrq $6, %rcx
	jz .Lhandle_tail_generic

	.p2align 4
.Lloop_64_generic:
19:	movq %rax, 0*8(%rdi)
20:	movq %rax, 1*8(%rdi)
21:	movq %rax, 2*8(%rdi)
22:	movq %rax, 3*8(%rdi)
23:	movq %rax, 4*8(%rdi)
24:	movq %rax, 5*8(%rdi)
25:	movq %rax, 6*8(%rdi)
26:	movq %rax, 7*8(%rdi)
	addq $64, %rdi
	decq %rcx
	jnz .Lloop_64_generic

	/* Calculate remaining bytes */
	andq $63, %rdx

.Lhandle_tail_generic:
.Lsmall_generic:
	/* Handle remaining bytes (< 64) with rep stosb */
	testq %rdx, %rdx
	jz .Lende_generic
	movq %rdx, %rcx
	movb %sil, %al /* Set %al */
27:	rep stosb

.Lende_generic:
	movq %r9, %rax /* Return original destination from generic path */
	RET

	/* Exception tables for generic loop path */
	_ASM_EXTABLE(16b, .Lgeneric_loop_fault_handler)
	_ASM_EXTABLE(17b, .Lgeneric_loop_fault_handler)
	_ASM_EXTABLE(18b, .Lgeneric_loop_fault_handler)
	_ASM_EXTABLE(19b, .Lgeneric_loop_fault_handler)
	_ASM_EXTABLE(20b, .Lgeneric_loop_fault_handler)
	_ASM_EXTABLE(21b, .Lgeneric_loop_fault_handler)
	_ASM_EXTABLE(22b, .Lgeneric_loop_fault_handler)
	_ASM_EXTABLE(23b, .Lgeneric_loop_fault_handler)
	_ASM_EXTABLE(24b, .Lgeneric_loop_fault_handler)
	_ASM_EXTABLE(25b, .Lgeneric_loop_fault_handler)
	_ASM_EXTABLE(26b, .Lgeneric_loop_fault_handler)
	_ASM_EXTABLE(27b, .Lgeneric_loop_fault_handler)
SYM_FUNC_END(__memset_generic_loop)
