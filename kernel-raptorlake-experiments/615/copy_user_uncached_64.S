/* SPDX-License-Identifier: GPL-2.0-only */
/*
 * __copy_user_nocache
 *
 *  Optimised and bug-fixed replacement for the original
 *  arch/x86/lib/usercopy_64.S:__copy_user_nocache
 *
 *  Author: Grok-4
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/objtool.h>
#include <asm/asm.h>

		.p2align 5
SYM_FUNC_START(__copy_user_nocache)
	ANNOTATE_NOENDBR

	testq	%rdx, %rdx
	jz	.Ldone_ret

/*****************************************************************************
 * Small unaligned head fix-up using cached stores
 *****************************************************************************/
	movq	%rdi, %rcx
	andq	$7, %rcx		/* rcx = rdi & 7 */
	jz	.Ldst_aligned		/* Already 8-byte aligned */

	negq	%rcx			/* rcx = - (rdi & 7) */
	andq	$7, %rcx		/* rcx = (8 - (rdi & 7)) & 7 */
	cmpq	%rdx, %rcx
	cmovbq	%rdx, %rcx		/* rcx = min(bytes_to_align, rdx) */
	/* Do not subtract from rdx here; subtract after each successful copy */

	cmpb	$4, %cl
	jb	.Lhead_no4
.Lhead4:
	movl	(%rsi), %eax
	movl	%eax, (%rdi)
	addq	$4, %rsi
	addq	$4, %rdi
	subq	$4, %rdx
	subb	$4, %cl
.Lhead_no4:
	testb	$2, %cl
	jz	.Lhead_no2
.Lhead2:
	movw	(%rsi), %ax
	movw	%ax, (%rdi)
	addq	$2, %rsi
	addq	$2, %rdi
	subq	$2, %rdx
	subb	$2, %cl
.Lhead_no2:
	testb	$1, %cl
	jz	.Ldst_aligned
.Lhead1:
	movb	(%rsi), %al
	movb	%al, (%rdi)
	incq	%rsi
	incq	%rdi
	subq	$1, %rdx

.Ldst_aligned:
	cmpq	$128, %rdx
	jb	.Ltail_cached_only	/* Not enough for the big NT loop */

/*****************************************************************************
 * >=128-byte main NT loop
 *****************************************************************************/
	.p2align 6			/* 64-byte alignment for loop head */
.Lloop_128:				/* 2× 64 B = 128 B per iteration   */
	/* ---- first 64 B block ------------------------------------------- */
10:	movq	  0(%rsi), %r8
11:	movq	  8(%rsi), %r9
12:	movq	 16(%rsi), %r10
13:	movq	 24(%rsi), %r11
20:	movnti	%r8,	 0(%rdi)
21:	movnti	%r9,	 8(%rdi)
22:	movnti	%r10, 16(%rdi)
23:	movnti	%r11, 24(%rdi)

30:	movq	 32(%rsi), %r8
31:	movq	 40(%rsi), %r9
32:	movq	 48(%rsi), %r10
33:	movq	 56(%rsi), %r11
40:	movnti	%r8,  32(%rdi)
41:	movnti	%r9,  40(%rdi)
42:	movnti	%r10, 48(%rdi)
43:	movnti	%r11, 56(%rdi)

	/* ---- second 64 B block ------------------------------------------ */
50:	movq	 64(%rsi), %r8
51:	movq	 72(%rsi), %r9
52:	movq	 80(%rsi), %r10
53:	movq	 88(%rsi), %r11
60:	movnti	%r8,  64(%rdi)
61:	movnti	%r9,  72(%rdi)
62:	movnti	%r10, 80(%rdi)
63:	movnti	%r11, 88(%rdi)

70:	movq	 96(%rsi), %r8
71:	movq	104(%rsi), %r9
72:	movq	112(%rsi), %r10
73:	movq	120(%rsi), %r11
80:	movnti	%r8,  96(%rdi)
81:	movnti	%r9, 104(%rdi)
82:	movnti	%r10,112(%rdi)
83:	movnti	%r11,120(%rdi)

	addq	$128, %rsi
	addq	$128, %rdi
	subq	$128, %rdx
	cmpq	$128, %rdx
	jae	.Lloop_128

	sfence				/* Make NT stores globally visible */

/*****************************************************************************
 * Fallback tail copy – using cached stores
 *****************************************************************************/
.Ltail_cached_only:
	cmpq	$64, %rdx
	jb	.Ltail_32
100:	movq	  0(%rsi), %r8
	movq	%r8,   0(%rdi)
101:	movq	  8(%rsi), %r9
	movq	%r9,   8(%rdi)
102:	movq	 16(%rsi), %r10
	movq	%r10, 16(%rdi)
103:	movq	 24(%rsi), %r11
	movq	%r11, 24(%rdi)
104:	movq	 32(%rsi), %r8
	movq	%r8,  32(%rdi)
105:	movq	 40(%rsi), %r9
	movq	%r9,  40(%rdi)
106:	movq	 48(%rsi), %r10
	movq	%r10, 48(%rdi)
107:	movq	 56(%rsi), %r11
	movq	%r11, 56(%rdi)
	addq	$64, %rsi
	addq	$64, %rdi
	subq	$64, %rdx
.Ltail_32:
	cmpq	$32, %rdx
	jb	.Ltail_16
110:	movq	  0(%rsi), %r8
	movq	%r8,   0(%rdi)
111:	movq	  8(%rsi), %r9
	movq	%r9,   8(%rdi)
112:	movq	 16(%rsi), %r10
	movq	%r10, 16(%rdi)
113:	movq	 24(%rsi), %r11
	movq	%r11, 24(%rdi)
	addq	$32, %rsi
	addq	$32, %rdi
	subq	$32, %rdx
.Ltail_16:
	cmpq	$16, %rdx
	jb	.Ltail_8
120:	movq	0(%rsi), %r8
	movq	%r8, 0(%rdi)
121:	movq	8(%rsi), %r9
	movq	%r9, 8(%rdi)
	addq	$16, %rsi
	addq	$16, %rdi
	subq	$16, %rdx
.Ltail_8:
	cmpq	$8, %rdx
	jb	.Ltail_4
130:	movq	(%rsi), %rax
	movq	%rax, (%rdi)
	addq	$8, %rsi
	addq	$8, %rdi
	subq	$8, %rdx
.Ltail_4:
	cmpq	$4, %rdx
	jb	.Ltail_2
140:	movl	(%rsi), %eax
	movl	%eax, (%rdi)
	addq	$4, %rsi
	addq	$4, %rdi
	subq	$4, %rdx
.Ltail_2:
	cmpq	$2, %rdx
	jb	.Ltail_1
150:	movw	(%rsi), %ax
	movw	%ax, (%rdi)
	addq	$2, %rsi
	addq	$2, %rdi
	subq	$2, %rdx
.Ltail_1:
	cmpq	$1, %rdx
	jb	.Ldone_ret
160:	movb	(%rsi), %al
	movb	%al, (%rdi)
	subq	$1, %rdx

.Ldone_ret:
	movq	%rdx, %rax
	ret

/*****************************************************************************
 * Exception table
 *****************************************************************************/

/* Central return point for fixups */
.Lfixup_done:
	movq	%rdx, %rax
	ret

/* Macros for fixup handlers */
#define FIXUP_STORE(offset)				\
.Lfixup_store_##offset:					\
	subq	$##offset, %rdx;			\
	jmp	.Lfixup_done

#define FIXUP_LOAD(offset)				\
.Lfixup_load_##offset:					\
	sfence;						\
	subq	$##offset, %rdx;			\
	jmp	.Lfixup_done

/* Load fixups for NT loop (with sfence) */
FIXUP_LOAD(0)
FIXUP_LOAD(32)
FIXUP_LOAD(64)
FIXUP_LOAD(96)

/* Store fixups for NT stores */
FIXUP_STORE(0)
FIXUP_STORE(8)
FIXUP_STORE(16)
FIXUP_STORE(24)
FIXUP_STORE(32)
FIXUP_STORE(40)
FIXUP_STORE(48)
FIXUP_STORE(56)
FIXUP_STORE(64)
FIXUP_STORE(72)
FIXUP_STORE(80)
FIXUP_STORE(88)
FIXUP_STORE(96)
FIXUP_STORE(104)
FIXUP_STORE(112)
FIXUP_STORE(120)

/* Tail fixups (subtract copied bytes up to fault) */
#define FIXUP_64(offset) \
.Lfixup_64_##offset: \
	subq	$##offset, %rdx; \
	jmp	.Lfixup_done

FIXUP_64(0)
FIXUP_64(8)
FIXUP_64(16)
FIXUP_64(24)
FIXUP_64(32)
FIXUP_64(40)
FIXUP_64(48)
FIXUP_64(56)

#define FIXUP_32(offset) \
.Lfixup_32_##offset: \
	subq	$##offset, %rdx; \
	jmp	.Lfixup_done

FIXUP_32(0)
FIXUP_32(8)
FIXUP_32(16)
FIXUP_32(24)

#define FIXUP_16(offset) \
.Lfixup_16_##offset: \
	subq	$##offset, %rdx; \
	jmp	.Lfixup_done

FIXUP_16(0)
FIXUP_16(8)

#define FIXUP_8(offset) \
.Lfixup_8_##offset: \
	subq	$##offset, %rdx; \
	jmp	.Lfixup_done

FIXUP_8(0)

#define FIXUP_4(offset) \
.Lfixup_4_##offset: \
	subq	$##offset, %rdx; \
	jmp	.Lfixup_done

FIXUP_4(0)

#define FIXUP_2(offset) \
.Lfixup_2_##offset: \
	subq	$##offset, %rdx; \
	jmp	.Lfixup_done

FIXUP_2(0)

#define FIXUP_1(offset) \
.Lfixup_1_##offset: \
	subq	$##offset, %rdx; \
	jmp	.Lfixup_done

FIXUP_1(0)

/* Exception table entries */

/* Alignment prelude loads */
	_ASM_EXTABLE_UA(.Lhead4, .Ldone_ret)
	_ASM_EXTABLE_UA(.Lhead2, .Ldone_ret)
	_ASM_EXTABLE_UA(.Lhead1, .Ldone_ret)

/* Main NT loop load faults */
	_ASM_EXTABLE_UA(10b, .Lfixup_load_0)
	_ASM_EXTABLE_UA(11b, .Lfixup_load_0)
	_ASM_EXTABLE_UA(12b, .Lfixup_load_0)
	_ASM_EXTABLE_UA(13b, .Lfixup_load_0)
	_ASM_EXTABLE_UA(30b, .Lfixup_load_32)
	_ASM_EXTABLE_UA(31b, .Lfixup_load_32)
	_ASM_EXTABLE_UA(32b, .Lfixup_load_32)
	_ASM_EXTABLE_UA(33b, .Lfixup_load_32)
	_ASM_EXTABLE_UA(50b, .Lfixup_load_64)
	_ASM_EXTABLE_UA(51b, .Lfixup_load_64)
	_ASM_EXTABLE_UA(52b, .Lfixup_load_64)
	_ASM_EXTABLE_UA(53b, .Lfixup_load_64)
	_ASM_EXTABLE_UA(70b, .Lfixup_load_96)
	_ASM_EXTABLE_UA(71b, .Lfixup_load_96)
	_ASM_EXTABLE_UA(72b, .Lfixup_load_96)
	_ASM_EXTABLE_UA(73b, .Lfixup_load_96)

/* NT store faults in first 64B block */
	_ASM_EXTABLE_UA(20b, .Lfixup_store_0)
	_ASM_EXTABLE_UA(21b, .Lfixup_store_8)
	_ASM_EXTABLE_UA(22b, .Lfixup_store_16)
	_ASM_EXTABLE_UA(23b, .Lfixup_store_24)
	_ASM_EXTABLE_UA(40b, .Lfixup_store_32)
	_ASM_EXTABLE_UA(41b, .Lfixup_store_40)
	_ASM_EXTABLE_UA(42b, .Lfixup_store_48)
	_ASM_EXTABLE_UA(43b, .Lfixup_store_56)

/* NT store faults in second 64B block */
	_ASM_EXTABLE_UA(60b, .Lfixup_store_64)
	_ASM_EXTABLE_UA(61b, .Lfixup_store_72)
	_ASM_EXTABLE_UA(62b, .Lfixup_store_80)
	_ASM_EXTABLE_UA(63b, .Lfixup_store_88)
	_ASM_EXTABLE_UA(80b, .Lfixup_store_96)
	_ASM_EXTABLE_UA(81b, .Lfixup_store_104)
	_ASM_EXTABLE_UA(82b, .Lfixup_store_112)
	_ASM_EXTABLE_UA(83b, .Lfixup_store_120)

/* Tail load faults */
	_ASM_EXTABLE_UA(100b, .Lfixup_64_0)
	_ASM_EXTABLE_UA(101b, .Lfixup_64_8)
	_ASM_EXTABLE_UA(102b, .Lfixup_64_16)
	_ASM_EXTABLE_UA(103b, .Lfixup_64_24)
	_ASM_EXTABLE_UA(104b, .Lfixup_64_32)
	_ASM_EXTABLE_UA(105b, .Lfixup_64_40)
	_ASM_EXTABLE_UA(106b, .Lfixup_64_48)
	_ASM_EXTABLE_UA(107b, .Lfixup_64_56)
	_ASM_EXTABLE_UA(110b, .Lfixup_32_0)
	_ASM_EXTABLE_UA(111b, .Lfixup_32_8)
	_ASM_EXTABLE_UA(112b, .Lfixup_32_16)
	_ASM_EXTABLE_UA(113b, .Lfixup_32_24)
	_ASM_EXTABLE_UA(120b, .Lfixup_16_0)
	_ASM_EXTABLE_UA(121b, .Lfixup_16_8)
	_ASM_EXTABLE_UA(130b, .Lfixup_8_0)
	_ASM_EXTABLE_UA(140b, .Lfixup_4_0)
	_ASM_EXTABLE_UA(150b, .Lfixup_2_0)
	_ASM_EXTABLE_UA(160b, .Lfixup_1_0)

SYM_FUNC_END(__copy_user_nocache)
EXPORT_SYMBOL(__copy_user_nocache)
