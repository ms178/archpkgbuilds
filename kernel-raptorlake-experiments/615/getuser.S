/* SPDX-License-Identifier: GPL-2.0 */
/*
 * __get_user helpers – fault-tolerant loads from userspace
 *
 *  Input :   %rax  = user pointer
 *  Output:   %rax  = 0 / -EFAULT
 *            %rdx  = zero-extended value
 *            (plus %ecx for 32-bit build when size==8)
 *
 *  The code below is ABI-identical to upstream.  Only style-polish
 *  and correct, balanced pre-processor guards were applied to fix
 *  build errors (#else / #endif mismatches).
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/objtool.h>
#include <asm/page_types.h>
#include <asm/errno.h>
#include <asm/asm-offsets.h>
#include <asm/thread_info.h>
#include <asm/asm.h>
#include <asm/smap.h>
#include <asm/runtime-const.h>

#define ASM_BARRIER_NOSPEC \
	ALTERNATIVE "", "lfence", X86_FEATURE_LFENCE_RDTSC

/* ------------------------------------------------------------------ */
/*  Range-check macro – blocks speculation past USER_PTR_MAX          */
/* ------------------------------------------------------------------ */
.macro check_range size:req
.if IS_ENABLED(CONFIG_X86_64)
	RUNTIME_CONST_PTR USER_PTR_MAX, rdx
	cmp	%rdx, %rax
	cmova	%rdx, %rax
.else
	cmp	$TASK_SIZE_MAX-\size+1, %eax
	jae	.Lbad_get_user
	sbb	%edx, %edx	/* array_index_mask_nospec() */
	and	%edx, %eax
.endif
.endm

/* ------------------------------------------------------------------ */
/*  Fault-tolerant memory op helper                                   */
/* ------------------------------------------------------------------ */
.macro UACCESS op src dst
1:	\op \src, \dst
	_ASM_EXTABLE_UA(1b, __get_user_handle_exception)
.endm

	.text
/* ================================================================ */
SYM_FUNC_START(__get_user_1)
	ANNOTATE_NOENDBR
	check_range size=1
	ASM_STAC
	UACCESS movzbl (%_ASM_AX), %edx
	xor	%eax, %eax
	ASM_CLAC
	RET
SYM_FUNC_END(__get_user_1)
EXPORT_SYMBOL(__get_user_1)

/* ================================================================ */
SYM_FUNC_START(__get_user_2)
	ANNOTATE_NOENDBR
	check_range size=2
	ASM_STAC
	UACCESS movzwl (%_ASM_AX), %edx
	xor	%eax, %eax
	ASM_CLAC
	RET
SYM_FUNC_END(__get_user_2)
EXPORT_SYMBOL(__get_user_2)

/* ================================================================ */
SYM_FUNC_START(__get_user_4)
	ANNOTATE_NOENDBR
	check_range size=4
	ASM_STAC
	UACCESS movl (%_ASM_AX), %edx
	xor	%eax, %eax
	ASM_CLAC
	RET
SYM_FUNC_END(__get_user_4)
EXPORT_SYMBOL(__get_user_4)

/* ================================================================ */
SYM_FUNC_START(__get_user_8)
	ANNOTATE_NOENDBR
#ifndef CONFIG_X86_64
	xor	%ecx, %ecx
#endif
	check_range size=8
	ASM_STAC
#ifdef CONFIG_X86_64
	UACCESS movq (%_ASM_AX), %rdx
#else /* 32-bit build */
	UACCESS movl (%_ASM_AX), %edx
	UACCESS movl 4(%_ASM_AX), %ecx
#endif
	xor	%eax, %eax
	ASM_CLAC
	RET
SYM_FUNC_END(__get_user_8)
EXPORT_SYMBOL(__get_user_8)

/* ------------------------------------------------------------------ *
 *                nocheck variants (skip range check)                 *
 * ------------------------------------------------------------------ */
#define GEN_NOCHECK(sz, suf, insn)			\
		.p2align 5				; \
SYM_FUNC_START(__get_user_nocheck_##suf)		; \
		ANNOTATE_NOENDBR			; \
		ASM_STAC				; \
		ASM_BARRIER_NOSPEC			; \
		UACCESS insn (%_ASM_AX), %edx		; \
		xor	%eax, %eax			; \
		ASM_CLAC				; \
		RET					; \
SYM_FUNC_END(__get_user_nocheck_##suf)		; \
EXPORT_SYMBOL(__get_user_nocheck_##suf)

GEN_NOCHECK(1, 1, movzbl)
GEN_NOCHECK(2, 2, movzwl)
GEN_NOCHECK(4, 4, movl)

	.p2align 5
SYM_FUNC_START(__get_user_nocheck_8)
	ANNOTATE_NOENDBR
	ASM_STAC
	ASM_BARRIER_NOSPEC
#ifdef CONFIG_X86_64
	UACCESS movq (%_ASM_AX), %rdx
#else
	xor	%ecx, %ecx
	UACCESS movl (%_ASM_AX), %edx
	UACCESS movl 4(%_ASM_AX), %ecx
#endif
	xor	%eax, %eax
	ASM_CLAC
	RET
SYM_FUNC_END(__get_user_nocheck_8)
EXPORT_SYMBOL(__get_user_nocheck_8)

/* ------------------------------------------------------------------ */
/*  Fault handler                                                      */
/* ------------------------------------------------------------------ */
SYM_CODE_START_LOCAL(__get_user_handle_exception)
	ASM_CLAC
.Lbad_get_user:
	xor	%edx, %edx
	mov	$(-EFAULT), %_ASM_AX
	RET
SYM_CODE_END(__get_user_handle_exception)
