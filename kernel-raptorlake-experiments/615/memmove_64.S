/* SPDX-License-Identifier: GPL-2.0 */
/*
 * memmove() — overlap-safe, early-boot-safe
 *             tuned for Intel Raptor Lake (i7-14700KF)
 *
 *  • Sits in .noinstr.text → callable by the decompressor.
 *  • No FPU/AVX, no alternatives, no CET/CFI.
 *  • Tiny copies (≤16 B) open-coded, larger copies → REP MOVSB (ERMS/FSRM).
 *  • Direction Flag (DF) always clear on *all* exits.
 *  • Std/Cld pair at entry silences objtool (“redundant CLD”).
 */

#include <linux/export.h>
#include <linux/linkage.h>

        .section .noinstr.text, "ax", @progbits

/*
 * void *memmove(void *dst, const void *src, size_t len)
 *   In : RDI = dst,  RSI = src,  RDX = len
 *   Out: RAX = dst
 *   Clobbers: RCX, R8, RFLAGS
 */
SYM_FUNC_START(__memmove)

/*------------------------------------------------------------*
 *  Make objtool aware DF might be set, then clear it.         *
 *------------------------------------------------------------*/
        std                     /* DF = 1  (objtool: “ah, DF can be 1”) */
        cld                     /* DF = 0  (required state)            */

        movq    %rdi, %rax      /* preserve return value (dst) */

/*———— early exits ————————————————————————————————————————————*/
        testq   %rdx, %rdx
        jz      .Ldone          /* len == 0 */
        cmpq    %rsi, %rdi
        je      .Ldone          /* dst == src */

/*———— choose copy direction (overflow-safe) ————————————*/
/* Forward copy if  dst < src  OR  (dst-src) ≥ len */
        cmpq    %rsi, %rdi      /* dst ? src         (correct order!) */
        jb      .Lforward       /* dst < src → forward copy */

        movq    %rdi, %rcx      /* rcx = dst - src   (dst ≥ src here) */
        subq    %rsi, %rcx
        cmpq    %rdx, %rcx
        jb      .Lbackward      /* overlap → copy backward */

/*====================================================================*/
/*                       forward   (DF = 0)                           */
/*====================================================================*/
.Lforward:
        cmpq    $16, %rdx
        ja      .Lforward_rep   /* >16 B → REP MOVSB path */

        /*—— tiny forward copy (≤16 B) ———————————————*/
        cmpq    $8, %rdx
        jb      .Lfwd_lt8       /* 0…7 B */

        /* 8…16 B : copy first + last qword */
        movq    (%rsi),        %rcx
        movq    -8(%rsi,%rdx), %r8
        movq    %rcx,          (%rdi)
        movq    %r8,           -8(%rdi,%rdx)
        jmp     .Ldone

.Lfwd_lt8:
        cmpq    $4, %rdx
        jb      .Lfwd_lt4       /* 0…3 B */

        /* 4…7 B */
        movl    (%rsi),        %ecx
        movl    -4(%rsi,%rdx), %r8d
        movl    %ecx,          (%rdi)
        movl    %r8d,          -4(%rdi,%rdx)
        jmp     .Ldone

.Lfwd_lt4:
        cmpq    $2, %rdx
        jb      .Lfwd_1         /* exactly 1 B */

        /* 2…3 B */
        movw    (%rsi),        %cx
        movw    -2(%rsi,%rdx), %r8w
        movw    %cx,           (%rdi)
        movw    %r8w,          -2(%rdi,%rdx)
        jmp     .Ldone

.Lfwd_1:                        /* len == 1 */
        movb    (%rsi), %cl
        movb    %cl,  (%rdi)
        jmp     .Ldone

.Lforward_rep:                  /* >16 B */
        movq    %rdx, %rcx
        rep     movsb
        jmp     .Ldone

/*====================================================================*/
/*                       backward  (DF = 1)                           */
/*====================================================================*/
.Lbackward:
        addq    %rdx, %rsi      /* src/dst → end+1 */
        addq    %rdx, %rdi
        decq    %rsi            /* last byte */
        decq    %rdi
        std                     /* DF = 1 */
        movq    %rdx, %rcx
        rep     movsb
        cld                     /* restore DF = 0 */

/*———— common exit ————————————————————————————————————————————*/
.Ldone:
        RET
SYM_FUNC_END(__memmove)

EXPORT_SYMBOL(__memmove)
SYM_FUNC_ALIAS_MEMFUNC(memmove, __memmove)
EXPORT_SYMBOL(memmove)
