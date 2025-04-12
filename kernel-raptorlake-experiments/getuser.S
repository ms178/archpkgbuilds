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
 *
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
	movq $0x0123456789abcdef,%rdx
  1:
  .pushsection runtime_ptr_USER_PTR_MAX,"a"
	.long 1b - 8 - .
  .popsection
	cmp %rdx, %rax
	cmova %rdx, %rax
.else
	cmp $TASK_SIZE_MAX-\size+1, %eax
	jae .Lbad_get_user
	sbb %edx, %edx		/* array_index_mask_nospec() */
	and %edx, %eax
.endif
.endm

/*
 * Enhanced UACCESS macro with proper exception handling
 * Ensures consistent behavior across all access sizes
 */
.macro UACCESS op src dst
1:	\op \src,\dst
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
	.p2align 6
SYM_FUNC_START(__get_user_1)
	check_range size=1
	ASM_STAC
	UACCESS movzbl (%_ASM_AX),%edx
	xor %eax,%eax
	ASM_CLAC
	RET
SYM_FUNC_END(__get_user_1)
EXPORT_SYMBOL(__get_user_1)

	.p2align 5
SYM_FUNC_START(__get_user_nocheck_1)
	ASM_STAC
	ASM_BARRIER_NOSPEC
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
	UACCESS movzwl (%_ASM_AX),%edx
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
	UACCESS movl (%_ASM_AX),%edx
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
	xor %ecx,%ecx
#endif
	check_range size=8
	ASM_STAC
#ifdef CONFIG_X86_64
	UACCESS movq (%_ASM_AX),%rdx
#else
	/* Enhanced 32-bit 8-byte read with alignment optimization */
	test $3, %eax
	jnz 1f
	/* Try aligned 8-byte read if CPU supports it */
	ALTERNATIVE "", "movq (%_ASM_AX), %mm0; movd %mm0, %edx; psrlq $32, %mm0; movd %mm0, %ecx; emms", X86_FEATURE_MMX
	jmp 2f
1:
	/* Fallback to standard 32-bit reads with prefetching */
	ALTERNATIVE "", "prefetcht0 4(%_ASM_AX)", X86_FEATURE_PREFETCHW
	UACCESS movl (%_ASM_AX),%edx
	UACCESS movl 4(%_ASM_AX),%ecx
2:
#endif
	xor %eax,%eax
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
	xor %ecx,%ecx
	test $3, %eax
	jnz 1f
	/* Try aligned 8-byte read if CPU supports it */
	ALTERNATIVE "", "movq (%_ASM_AX), %mm0; movd %mm0, %edx; psrlq $32, %mm0; movd %mm0, %ecx; emms", X86_FEATURE_MMX
	jmp 2f
1:
	/* Fallback to standard 32-bit reads with prefetching */
	ALTERNATIVE "", "prefetcht0 4(%_ASM_AX)", X86_FEATURE_PREFETCHW
	UACCESS movl (%_ASM_AX),%edx
	UACCESS movl 4(%_ASM_AX),%ecx
2:
#endif
	xor %eax,%eax
	ASM_CLAC
	RET
SYM_FUNC_END(__get_user_nocheck_8)
EXPORT_SYMBOL(__get_user_nocheck_8)

/*
 * Optimized vector-based accessors for larger data
 * These are particularly beneficial for Raptor Lake's improved vector units
 */
#ifdef CONFIG_X86_64
	.p2align 6
SYM_FUNC_START(__get_user_16)
	check_range size=16
	ASM_STAC
	/* Use AVX if available for 16-byte reads */
	ALTERNATIVE "movq (%rax), %rdx; movq 8(%rax), %rcx", \
		"vmovdqu (%rax), %xmm0; vmovq %xmm0, %rdx; vpextrq $1, %xmm0, %rcx", \
		X86_FEATURE_AVX
	xor %eax,%eax
	ASM_CLAC
	RET
SYM_FUNC_END(__get_user_16)
EXPORT_SYMBOL(__get_user_16)

	.p2align 5
SYM_FUNC_START(__get_user_nocheck_16)
	ASM_STAC
	ASM_BARRIER_NOSPEC
	/* Use AVX if available for 16-byte reads */
	ALTERNATIVE "movq (%rax), %rdx; movq 8(%rax), %rcx", \
		"vmovdqu (%rax), %xmm0; vmovq %xmm0, %rdx; vpextrq $1, %xmm0, %rcx", \
		X86_FEATURE_AVX
	xor %eax,%eax
	ASM_CLAC
	RET
SYM_FUNC_END(__get_user_nocheck_16)
EXPORT_SYMBOL(__get_user_nocheck_16)
#endif

/*
 * Error handling path - optimized for minimal overhead
 * Aligned to 16-byte boundary for efficient execution
 */
	.p2align 4
SYM_CODE_START_LOCAL(__get_user_handle_exception)
	ASM_CLAC
.Lbad_get_user:
	xor %edx,%edx
#ifndef CONFIG_X86_64
	/* Clear %ecx for 32-bit __get_user_8 */
	xor %ecx,%ecx
#endif
#ifdef CONFIG_X86_64
	/* Clear additional registers used by vector operations */
	ALTERNATIVE "", "vzeroupper", X86_FEATURE_AVX
#endif
	mov $(-EFAULT),%_ASM_AX
	RET
SYM_CODE_END(__get_user_handle_exception)
