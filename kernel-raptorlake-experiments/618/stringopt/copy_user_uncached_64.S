/* SPDX-License-Identifier: GPL-2.0-only */
/*
 * __copy_user_nocache - uncached copy from user to kernel
 *
 * Copyright 2023 Linus Torvalds
 *
 *
 * Input:  rdi=dst (kernel), rsi=src (user), edx=len
 * Output: rax=bytes not copied
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/objtool.h>
#include <asm/asm.h>

SYM_FUNC_START(__copy_user_nocache)
        ANNOTATE_NOENDBR

        testb   $7, %dil
        jne     .Lalign

.Lis_aligned:
        cmpl    $64, %edx
        jb      .Lquadwords

        .p2align 4, 0x90
.Lunrolled:
10:     movq    (%rsi), %r8
11:     movq    8(%rsi), %r9
12:     movq    16(%rsi), %r10
13:     movq    24(%rsi), %r11
20:     movnti  %r8, (%rdi)
21:     movnti  %r9, 8(%rdi)
22:     movnti  %r10, 16(%rdi)
23:     movnti  %r11, 24(%rdi)
30:     movq    32(%rsi), %r8
31:     movq    40(%rsi), %r9
32:     movq    48(%rsi), %r10
33:     movq    56(%rsi), %r11
40:     movnti  %r8, 32(%rdi)
41:     movnti  %r9, 40(%rdi)
42:     movnti  %r10, 48(%rdi)
43:     movnti  %r11, 56(%rdi)

        addq    $64, %rsi
        addq    $64, %rdi
        subl    $64, %edx
        cmpl    $64, %edx
        jae     .Lunrolled

_ASM_EXTABLE_UA(10b, .Lquadwords)
_ASM_EXTABLE_UA(11b, .Lquadwords)
_ASM_EXTABLE_UA(12b, .Lquadwords)
_ASM_EXTABLE_UA(13b, .Lquadwords)
_ASM_EXTABLE_UA(30b, .Lfixup32)
_ASM_EXTABLE_UA(31b, .Lfixup32)
_ASM_EXTABLE_UA(32b, .Lfixup32)
_ASM_EXTABLE_UA(33b, .Lfixup32)
_ASM_EXTABLE_UA(20b, .Ldone0)
_ASM_EXTABLE_UA(21b, .Ldone8)
_ASM_EXTABLE_UA(22b, .Ldone16)
_ASM_EXTABLE_UA(23b, .Ldone24)
_ASM_EXTABLE_UA(40b, .Ldone32)
_ASM_EXTABLE_UA(41b, .Ldone40)
_ASM_EXTABLE_UA(42b, .Ldone48)
_ASM_EXTABLE_UA(43b, .Ldone56)

.Lquadwords:
        cmpl    $8, %edx
        jb      .Llong
50:     movq    (%rsi), %rax
51:     movnti  %rax, (%rdi)
        addq    $8, %rsi
        addq    $8, %rdi
        subl    $8, %edx
        jmp     .Lquadwords

_ASM_EXTABLE_UA(50b, .Llast4)
_ASM_EXTABLE_UA(51b, .Ldone0)

.Llong:
        testb   $4, %dl
        je      .Lword
60:     movl    (%rsi), %eax
61:     movnti  %eax, (%rdi)
        addq    $4, %rsi
        addq    $4, %rdi
        subl    $4, %edx
.Lword:
        sfence
        testb   $2, %dl
        je      .Lbyte
70:     movw    (%rsi), %ax
71:     movw    %ax, (%rdi)
        addq    $2, %rsi
        addq    $2, %rdi
        subl    $2, %edx
.Lbyte:
        testb   $1, %dl
        je      .Ldone
80:     movb    (%rsi), %al
81:     movb    %al, (%rdi)
        decl    %edx
.Ldone:
        movl    %edx, %eax
        RET

_ASM_EXTABLE_UA(60b, .Ldone)
_ASM_EXTABLE_UA(61b, .Ldone)
_ASM_EXTABLE_UA(70b, .Ldone)
_ASM_EXTABLE_UA(71b, .Ldone)
_ASM_EXTABLE_UA(80b, .Ldone)
_ASM_EXTABLE_UA(81b, .Ldone)

.Lalign:
        testb   $1, %dil
        je      .Lalign_word
        testl   %edx, %edx
        je      .Ldone
90:     movb    (%rsi), %al
91:     movb    %al, (%rdi)
        incq    %rsi
        incq    %rdi
        decl    %edx
.Lalign_word:
        testb   $2, %dil
        je      .Lalign_long
        cmpl    $2, %edx
        jb      .Lbyte
92:     movw    (%rsi), %ax
93:     movw    %ax, (%rdi)
        addq    $2, %rsi
        addq    $2, %rdi
        subl    $2, %edx
.Lalign_long:
        testb   $4, %dil
        je      .Lis_aligned
        cmpl    $4, %edx
        jb      .Lword
94:     movl    (%rsi), %eax
95:     movnti  %eax, (%rdi)
        addq    $4, %rsi
        addq    $4, %rdi
        subl    $4, %edx
        jmp     .Lis_aligned

_ASM_EXTABLE_UA(90b, .Ldone)
_ASM_EXTABLE_UA(91b, .Ldone)
_ASM_EXTABLE_UA(92b, .Ldone)
_ASM_EXTABLE_UA(93b, .Ldone)
_ASM_EXTABLE_UA(94b, .Ldone)
_ASM_EXTABLE_UA(95b, .Ldone)

.Ldone56: subl $8, %edx
.Ldone48: subl $8, %edx
.Ldone40: subl $8, %edx
.Ldone32: subl $8, %edx
.Ldone24: subl $8, %edx
.Ldone16: subl $8, %edx
.Ldone8:  subl $8, %edx
.Ldone0:
        movl    %edx, %eax
        RET

.Lfixup32:
        addq    $32, %rsi
        addq    $32, %rdi
        subl    $32, %edx
        jmp     .Lquadwords

.Llast4:
52:     movl    (%rsi), %eax
53:     movnti  %eax, (%rdi)
        sfence
        subl    $4, %edx
        movl    %edx, %eax
        RET
_ASM_EXTABLE_UA(52b, .Ldone0)
_ASM_EXTABLE_UA(53b, .Ldone0)

SYM_FUNC_END(__copy_user_nocache)
EXPORT_SYMBOL(__copy_user_nocache)
