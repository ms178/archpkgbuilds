/* SPDX-License-Identifier: GPL-2.0 */
/*
 * __get_user functions.
 *
 * (C) Copyright 1998 Linus Torvalds
 * (C) Copyright 2005 Andi Kleen
 * (C) Copyright 2008 Glauber Costa
 *
 * These functions have a non-standard call interface
 * to make them more efficient, especially as they
 * return an error value in addition to the "real"
 * return value.
 *
 * Optimized for Intel Raptor Lake processors.
 */

/*
 * __get_user_X
 *
 * Inputs:	%[r|e]ax contains the address.
 *
 * Outputs:	%[r|e]ax is error code (0 or -EFAULT)
 *		%[r|e]dx contains zero-extended value
 *		%ecx contains the high half for 32-bit __get_user_8
 *              For 64-bit __get_user_16: %rdx = low 64 bits, %rcx = high 64 bits.
 *
 * These functions should not modify any other registers,
 * as they get called from within inline assembly.
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <asm/page_types.h>
#include <asm/errno.h>
#include <asm/asm-offsets.h>
#include <asm/thread_info.h>
#include <asm/asm.h>
#include <asm/smap.h>

/* Original speculative execution barrier */
#define ASM_BARRIER_NOSPEC ALTERNATIVE "", "lfence", X86_FEATURE_LFENCE_RDTSC

/*
 * Improved range check using conditional move (better for Raptor Lake)
 * Uses branchless design to avoid misprediction penalties
 */
.macro check_range size:req
.if IS_ENABLED(CONFIG_X86_64)
	movq $0x0123456789abcdef,%rdx /* Sentinel for USER_PTR_MAX comparison */
  1:
  /*
   * This creates a PC-relative reference to USER_PTR_MAX.
   * The runtime_ptr_patch() will replace the .long with the actual
   * PC-relative offset to USER_PTR_MAX.
   * At runtime, before the cmp, %rdx will be loaded with USER_PTR_MAX.
   * movabs $USER_PTR_MAX, %rdx would be simpler but is a 10-byte instruction.
   * This sequence is more compact for frequent use.
   */
  .pushsection runtime_ptr_USER_PTR_MAX,"a"
	.long 1b - 8 - . /* Placeholder for PC-relative offset, 8 bytes before the cmp */
  .popsection
	cmp %rdx, %rax   /* Compare user address (%rax) with USER_PTR_MAX */
	cmova %rdx, %rax /* If %rax > USER_PTR_MAX, %rax becomes USER_PTR_MAX (will fault) */
.else /* !CONFIG_X86_64 */
	cmp $TASK_SIZE_MAX-\size+1, %eax /* Check if addr + size - 1 is within bounds */
	jae .Lbad_get_user               /* If out of bounds, jump to error */
	/*
	 * array_index_mask_nospec(): If the previous check passed (carry clear),
	 * sbb %edx, %edx results in %edx = 0.
	 * If it failed (carry set, though 'jae' means we shouldn't hit this path often here),
	 * sbb %edx, %edx results in %edx = -1 (all bits set).
	 * This is a speculative execution hardening technique. Since 'jae' handles
	 * the out-of-bounds case directly, this sbb/and sequence here will always
	 * result in %eax remaining unchanged if the bounds check passed.
	 */
	sbb %edx, %edx
	and %edx, %eax
.endif
.endm

/*
 * Enhanced UACCESS macro with proper exception handling
 * Ensures consistent behavior across all access sizes
 */
.macro UACCESS op src dst
1:	\op \src,\dst
	/*
	 * _ASM_EXTABLE_UA informs the kernel that the instruction at label 1b
	 * can fault due to user access, and if it does, execution should
	 * jump to __get_user_handle_exception.
	 */
	_ASM_EXTABLE_UA(1b, __get_user_handle_exception)
.endm

/*
 * Cache-optimized layout: Group related functions in 64-byte cache lines
 * where possible, with 32-byte alignment for individual functions
 */

	.text
	/*
	 * Cache line #1: 1-byte and 2-byte accessors
	 * Aligned to 64-byte boundary for optimal cache usage
	 */
	.p2align 6 /* Align to 2^6 = 64 bytes */
SYM_FUNC_START(__get_user_1)
	check_range size=1
	ASM_STAC /* Set AC flag for user access (SMAP protection) */
	UACCESS movzbl (%_ASM_AX),%edx /* Zero-extend byte from user addr in %rax to %edx */
	xor %eax,%eax /* Success: %rax = 0 */
	ASM_CLAC /* Clear AC flag */
	RET
SYM_FUNC_END(__get_user_1)
EXPORT_SYMBOL(__get_user_1)

	.p2align 5 /* Align to 2^5 = 32 bytes */
SYM_FUNC_START(__get_user_nocheck_1)
	ASM_STAC
	ASM_BARRIER_NOSPEC /* Mitigate speculative execution issues */
	UACCESS movzbl (%_ASM_AX),%edx
	xor %eax,%eax
	ASM_CLAC
	RET
SYM_FUNC_END(__get_user_nocheck_1)
EXPORT_SYMBOL(__get_user_nocheck_1)

	/*
	 * Cache line #2: 2-byte accessors
	 * Aligned to 64-byte boundary for optimal cache usage
	 */
	.p2align 6
SYM_FUNC_START(__get_user_2)
	check_range size=2
	ASM_STAC
	UACCESS movzwl (%_ASM_AX),%edx /* Zero-extend word */
	xor %eax,%eax
	ASM_CLAC
	RET
SYM_FUNC_END(__get_user_2)
EXPORT_SYMBOL(__get_user_2)

	.p2align 5
SYM_FUNC_START(__get_user_nocheck_2)
	ASM_STAC
	ASM_BARRIER_NOSPEC
	UACCESS movzwl (%_ASM_AX),%edx
	xor %eax,%eax
	ASM_CLAC
	RET
SYM_FUNC_END(__get_user_nocheck_2)
EXPORT_SYMBOL(__get_user_nocheck_2)

	/*
	 * Cache line #3: 4-byte accessors
	 * Aligned to 64-byte boundary for optimal cache usage
	 */
	.p2align 6
SYM_FUNC_START(__get_user_4)
	check_range size=4
	ASM_STAC
	UACCESS movl (%_ASM_AX),%edx /* Load dword */
	xor %eax,%eax
	ASM_CLAC
	RET
SYM_FUNC_END(__get_user_4)
EXPORT_SYMBOL(__get_user_4)

	.p2align 5
SYM_FUNC_START(__get_user_nocheck_4)
	ASM_STAC
	ASM_BARRIER_NOSPEC
	UACCESS movl (%_ASM_AX),%edx
	xor %eax,%eax
	ASM_CLAC
	RET
SYM_FUNC_END(__get_user_nocheck_4)
EXPORT_SYMBOL(__get_user_nocheck_4)

	/*
	 * Cache line #4: 8-byte accessors with optimized paths
	 * Aligned to 64-byte boundary for optimal cache usage
	 */
	.p2align 6
SYM_FUNC_START(__get_user_8)
#ifndef CONFIG_X86_64
	/* For 32-bit, output is %edx (low 32 bits) and %ecx (high 32 bits) */
	xor %ecx,%ecx /* Clear high part initially */
#endif
	check_range size=8
	ASM_STAC
#ifdef CONFIG_X86_64
	UACCESS movq (%_ASM_AX),%rdx /* Load qword */
#else
	/* Enhanced 32-bit 8-byte read with alignment optimization */
	test $3, %eax /* Check if address in %eax is 4-byte aligned */
	jnz 1f        /* If not aligned, jump to unaligned/split read */
	/* Try aligned 8-byte read using MMX if CPU supports it. */
	/* ALTERNATIVE will patch this to the MMX sequence at boot if X86_FEATURE_MMX. */
	/* If no MMX, it executes the first (empty) part and falls through to 1f. */
	ALTERNATIVE "", "2:\n\tmovq (%_ASM_AX), %mm0\n\tmovd %mm0, %edx\n\tpsrlq $32, %mm0\n\tmovd %mm0, %ecx\n\temms\n\t_ASM_EXTABLE_UA(2b,__get_user_handle_exception)", X86_FEATURE_MMX
	jmp 3f /* If MMX path taken and successful, skip to end */
1:
	/* Fallback to standard two 32-bit reads, prefetch if available. */
	/* This prefetch is a hint and might not always be beneficial for short reads. */
	ALTERNATIVE "", "prefetcht0 4(%_ASM_AX)", X86_FEATURE_PREFETCHW
	UACCESS movl (%_ASM_AX),%edx /* Load low 32 bits */
	UACCESS movl 4(%_ASM_AX),%ecx /* Load high 32 bits */
3:  /* Target for successful MMX path or fall-through from unaligned path */
#endif
	xor %eax,%eax /* Success: %rax = 0 */
	ASM_CLAC
	RET
SYM_FUNC_END(__get_user_8)
EXPORT_SYMBOL(__get_user_8)

	.p2align 5
SYM_FUNC_START(__get_user_nocheck_8)
	ASM_STAC
	ASM_BARRIER_NOSPEC
#ifdef CONFIG_X86_64
	UACCESS movq (%_ASM_AX),%rdx
#else
	/* Enhanced 32-bit 8-byte read with alignment optimization */
	xor %ecx,%ecx /* Clear high part initially */
	test $3, %eax /* Check alignment */
	jnz 1f        /* If not aligned, jump */
	ALTERNATIVE "", "2:\n\tmovq (%_ASM_AX), %mm0\n\tmovd %mm0, %edx\n\tpsrlq $32, %mm0\n\tmovd %mm0, %ecx\n\temms\n\t_ASM_EXTABLE_UA(2b,__get_user_handle_exception)", X86_FEATURE_MMX
	jmp 3f
1:
	ALTERNATIVE "", "prefetcht0 4(%_ASM_AX)", X86_FEATURE_PREFETCHW
	UACCESS movl (%_ASM_AX),%edx
	UACCESS movl 4(%_ASM_AX),%ecx
3:
#endif
	xor %eax,%eax
	ASM_CLAC
	RET
SYM_FUNC_END(__get_user_nocheck_8)
EXPORT_SYMBOL(__get_user_nocheck_8)

/*
 * For 16-byte access, using two 8-byte GPR loads is safe, simple,
 * and avoids FPU/AVX state management complexities in this low-level path.
 * The potential performance gain of a single 16-byte AVX load is likely
 * offset by FPU management overhead for such a small operation.
 */
#ifdef CONFIG_X86_64
	.p2align 6
SYM_FUNC_START(__get_user_16)
	/* %rax = user address, outputs: %rax = 0 or -EFAULT, %rdx = low64, %rcx = high64 */
	check_range size=16
	ASM_STAC
	ASM_BARRIER_NOSPEC
	/* GPR-only path for 16-byte read: */
1:	movq (%_ASM_AX),%rdx  /* Load low 64 bits */
2:	movq 8(%_ASM_AX),%rcx /* Load high 64 bits */
	/* Exception table entries point to a specific handler for 16-byte faults */
	_ASM_EXTABLE_UA(1b, __get_user_handle_exception_16)
	_ASM_EXTABLE_UA(2b, __get_user_handle_exception_16)
	xor %eax,%eax /* Success: %rax = 0 */
	ASM_CLAC
	RET
SYM_FUNC_END(__get_user_16)
EXPORT_SYMBOL(__get_user_16)

	.p2align 5
SYM_FUNC_START(__get_user_nocheck_16)
	ASM_STAC
	ASM_BARRIER_NOSPEC
	/* GPR-only path for 16-byte read: */
1:	movq (%_ASM_AX),%rdx
2:	movq 8(%_ASM_AX),%rcx
	_ASM_EXTABLE_UA(1b, __get_user_handle_exception_16)
	_ASM_EXTABLE_UA(2b, __get_user_handle_exception_16)
	xor %eax,%eax
	ASM_CLAC
	RET
SYM_FUNC_END(__get_user_nocheck_16)
EXPORT_SYMBOL(__get_user_nocheck_16)
#endif

/*
 * Generic Error handling path
 * Aligned to 16-byte boundary for efficient execution
 */
	.p2align 4 /* Align to 2^4 = 16 bytes */
SYM_CODE_START_LOCAL(__get_user_handle_exception)
	ASM_CLAC /* Clear AC flag first thing on fault */
.Lbad_get_user: /* Common entry point for bad access after range check */
	xor %edx,%edx /* Zero out low part of data return register */
#ifndef CONFIG_X86_64
	/* For 32-bit __get_user_8, also clear %ecx (high part of 64-bit value) */
	xor %ecx,%ecx
	/* No VZEROUPPER on 32-bit as AVX isn't typically used for these get_user sizes */
#else /* CONFIG_X86_64 */
	/*
	 * General FPU cleanup if AVX was used by *any* __get_user variant
	 * that might have faulted (e.g., a hypothetical future AVX __get_user_8/other).
	 * This is a safety net. For the current GPR-only _16, it's not strictly needed
	 * if this handler is *only* jumped to from GPR paths, but harmless.
	 * It will only execute if X86_FEATURE_AVX is present on the CPU.
	 */
	ALTERNATIVE "", "vzeroupper", X86_FEATURE_AVX
#endif
	mov $(-EFAULT),%_ASM_AX /* Set error code in %rax */
	RET
SYM_CODE_END(__get_user_handle_exception)

/*
 * Specific fault handler for 16-byte GPR operations (64-bit only).
 * Ensures both %rdx and %rcx (the 128-bit value pair) are cleared on fault.
 * The generic handler doesn't necessarily clear %rcx on 64-bit.
 */
#ifdef CONFIG_X86_64
.p2align 4
SYM_CODE_START_LOCAL(__get_user_handle_exception_16)
	ASM_CLAC
	xor %edx,%edx   /* Zero %rdx (low 64 bits of the 128-bit value) */
	xor %ecx,%ecx   /* Zero %rcx (high 64 bits of the 128-bit value) */
	/* No VZEROUPPER needed here as this handler is specifically for GPR paths */
	mov $(-EFAULT),%_ASM_AX
	RET
SYM_CODE_END(__get_user_handle_exception_16)
#endif /* CONFIG_X86_64 */
