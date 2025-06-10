/* SPDX-License-Identifier: GPL-2.0 */
/*
 * __copy_user_nocache  —  user → kernel copy with non-temporal stores
 *
 *   rdi = dest  (kernel, may machine-check)
 *   rsi = src   (user, may fault)
 *   rdx = count (bytes, 64-bit)
 *
 *   rax = 0 on success | remaining bytes on fault
 *
 * This version is optimised for Intel Raptor-Lake but keeps the
 * original algorithm and exception semantics.  No SSE/AVX regs are
 * touched, so we stay out of the FPU context-switch path.
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/objtool.h>
#include <asm/asm.h>

        .p2align 5
SYM_FUNC_START(__copy_user_nocache)
        ANNOTATE_NOENDBR

        /* ----------------------------------------------------------
         * Align destination to 8-byte boundary – avoids split NTI’s
         * -------------------------------------------------------- */
        testq   $7, %rdi
        jne     .Lalign

/* =================================================================
 *  dst already aligned
 * =============================================================== */
.Lis_aligned:
        cmpq    $64, %rdx
        jb      .Lquadwords

        .p2align 4, 0x90
.Lunrolled:
/* ---- 1st 32 B load burst ------------------------------------ */
10:     movq    0(%rsi),  %r8
11:     movq    8(%rsi),  %r9
12:     movq   16(%rsi),  %r10
13:     movq   24(%rsi),  %r11
/* ---- 1st 32 B non-temporal store ---------------------------- */
20:     movnti  %r8,  0(%rdi)
21:     movnti  %r9,  8(%rdi)
22:     movnti  %r10, 16(%rdi)
23:     movnti  %r11, 24(%rdi)
/* ---- 2nd 32 B load + store ---------------------------------- */
30:     movq   32(%rsi), %r8
31:     movq   40(%rsi), %r9
32:     movq   48(%rsi), %r10
33:     movq   56(%rsi), %r11
40:     movnti  %r8,  32(%rdi)
41:     movnti  %r9,  40(%rdi)
42:     movnti  %r10, 48(%rdi)
43:     movnti  %r11, 56(%rdi)

        addq    $64, %rsi
        addq    $64, %rdi
        subq    $64, %rdx
        cmpq    $64, %rdx
        jae     .Lunrolled

        /* -------- extable entries for the 64-B unroll ---------- */
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

/* =================================================================
 *  8-byte loop
 * =============================================================== */
.Lquadwords:
        cmpq    $8, %rdx
        jb      .Llong
50:     movq    (%rsi), %rax
51:     movnti  %rax,  (%rdi)
        addq    $8, %rsi
        addq    $8, %rdi
        subq    $8, %rdx
        jmp     .Lquadwords

        _ASM_EXTABLE_UA(50b, .Llast4)
        _ASM_EXTABLE_UA(51b, .Ldone0)

/* =================================================================
 *  <=7 bytes remaining   (handle 4/2/1 with cached stores)
 * =============================================================== */
.Llong:
        testq   $4, %rdx
        je      .Lword
60:     movl    (%rsi), %eax
61:     movnti  %eax, (%rdi)
        addq    $4, %rsi
        addq    $4, %rdi
        subq    $4, %rdx
.Lword:
        sfence                          /* ensure NT stores visible */
        testq   $2, %rdx
        je      .Lbyte
70:     movw    (%rsi), %ax
71:     movw    %ax,  (%rdi)
        addq    $2, %rsi
        addq    $2, %rdi
        subq    $2, %rdx
.Lbyte:
        testq   $1, %rdx
        je      .Ldone
80:     movb    (%rsi), %al
81:     movb    %al, (%rdi)
        decq    %rdx
.Ldone:
        movq    %rdx, %rax
        RET

        _ASM_EXTABLE_UA(60b, .Ldone)
        _ASM_EXTABLE_UA(61b, .Ldone)
        _ASM_EXTABLE_UA(70b, .Ldone)
        _ASM_EXTABLE_UA(71b, .Ldone)
        _ASM_EXTABLE_UA(80b, .Ldone)
        _ASM_EXTABLE_UA(81b, .Ldone)

/* =================================================================
 *  destination not aligned – fix head (1/2/4 B) then fall through
 * =============================================================== */
.Lalign:
        testq   $1, %rdi
        je      .Lalign_word
        testq   %rdx, %rdx
        je      .Ldone
90:     movb    (%rsi), %al
91:     movb    %al,   (%rdi)
        incq    %rsi
        incq    %rdi
        decq    %rdx
.Lalign_word:
        testq   $2, %rdi
        je      .Lalign_long
        cmpq    $2, %rdx
        jb      .Lbyte
92:     movw    (%rsi), %ax
93:     movw    %ax,  (%rdi)
        addq    $2, %rsi
        addq    $2, %rdi
        subq    $2, %rdx
.Lalign_long:
        testq   $4, %rdi
        je      .Lis_aligned
        cmpq    $4, %rdx
        jb      .Lword
94:     movl    (%rsi), %eax
95:     movnti  %eax,  (%rdi)
        addq    $4, %rsi
        addq    $4, %rdi
        subq    $4, %rdx
        jmp     .Lis_aligned

        _ASM_EXTABLE_UA(90b, .Ldone)
        _ASM_EXTABLE_UA(91b, .Ldone)
        _ASM_EXTABLE_UA(92b, .Ldone)
        _ASM_EXTABLE_UA(93b, .Ldone)
        _ASM_EXTABLE_UA(94b, .Ldone)
        _ASM_EXTABLE_UA(95b, .Ldone)

/* =================================================================
 *  Exception-table fix-ups
 * =============================================================== */
.Ldone56: subq    $8, %rdx
.Ldone48: subq    $8, %rdx
.Ldone40: subq    $8, %rdx
.Ldone32: subq    $8, %rdx
.Ldone24: subq    $8, %rdx
.Ldone16: subq    $8, %rdx
.Ldone8:  subq    $8, %rdx
.Ldone0:
        movq    %rdx, %rax
        RET

.Lfixup32:
        addq    $32, %rsi
        addq    $32, %rdi
        subq    $32, %rdx
        jmp     .Lquadwords

/* ---- last 4-byte NTI path when 50b load faults --------------- */
.Llast4:
52:     movl    (%rsi), %eax
53:     movnti  %eax, (%rdi)
        sfence
        subq    $4, %rdx
        movq    %rdx, %rax
        RET
        _ASM_EXTABLE_UA(52b, .Ldone0)
        _ASM_EXTABLE_UA(53b, .Ldone0)

SYM_FUNC_END(__copy_user_nocache)
EXPORT_SYMBOL(__copy_user_nocache)
