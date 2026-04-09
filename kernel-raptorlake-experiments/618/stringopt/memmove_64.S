/* SPDX-License-Identifier: GPL-2.0 */
/*
 * memmove – overlap-safe copy, Raptor Lake tuned
 *
 *  • Lives in .noinstr.text (early-boot safe)
 *  • No FPU/AVX, no alternatives patching
 *  • len ≤16: open-coded first+last copy (forward only)
 *  • len >16: rep movsb (ERMS/FSRM on i7-14700KF → ~64 B/cycle)
 *  • DF is 0 on entry and on every exit
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/objtool.h>
#include <asm/asm.h>

        .section .noinstr.text, "ax", @progbits

/* void *memmove(void *dst, const void *src, size_t len) */
SYM_FUNC_START(__memmove)
        ANNOTATE_NOENDBR

        movq    %rdi, %rax              /* return value */

        testq   %rdx, %rdx
        jz      .Ldone
        cmpq    %rdi, %rsi
        je      .Ldone

        /* unsigned diff = dst - src ; forward if dst<src or diff>=len */
        movq    %rdi, %rcx
        subq    %rsi, %rcx
        cmpq    %rsi, %rdi
        jb      .Lforward               /* dst < src */
        cmpq    %rdx, %rcx
        jae     .Lforward               /* no overlap */

        /* ---------- backward (overlap, dst > src) ---------- */
        /* Use rep movsb for all backward sizes – the previous
         * tiny backward optimization corrupted data for
         * dst = src+1, len ≤16 by overwriting source before reading. */
        jmp     .Lbackward_rep

        .p2align 6
.Lbackward_rep:
        leaq    -1(%rsi,%rdx), %rsi
        leaq    -1(%rdi,%rdx), %rdi
        std
        movq    %rdx, %rcx
        rep     movsb
        cld
        jmp     .Ldone

        /* ---------- forward (dst <= src or no overlap) ---------- */
        .p2align 6
.Lforward:
        cmpq    $16, %rdx
        ja      .Lforward_rep

        cmpq    $8, %rdx
        jb      .Lfwd_lt8
        movq    (%rsi), %rcx
        movq    -8(%rsi,%rdx), %r8
        movq    %rcx, (%rdi)
        movq    %r8, -8(%rdi,%rdx)
        jmp     .Ldone

.Lfwd_lt8:
        cmpq    $4, %rdx
        jb      .Lfwd_lt4
        movl    (%rsi), %ecx
        movl    -4(%rsi,%rdx), %r8d
        movl    %ecx, (%rdi)
        movl    %r8d, -4(%rdi,%rdx)
        jmp     .Ldone

.Lfwd_lt4:
        cmpq    $2, %rdx
        jb      .Lfwd_1
        movw    (%rsi), %cx
        movw    -2(%rsi,%rdx), %r8w
        movw    %cx, (%rdi)
        movw    %r8w, -2(%rdi,%rdx)
        jmp     .Ldone

.Lfwd_1:
        movb    (%rsi), %cl
        movb    %cl, (%rdi)
        jmp     .Ldone

        .p2align 6
.Lforward_rep:
        movq    %rdx, %rcx
        rep     movsb

.Ldone:
        RET
SYM_FUNC_END(__memmove)

EXPORT_SYMBOL(__memmove)
SYM_FUNC_ALIAS_MEMFUNC(memmove, __memmove)
EXPORT_SYMBOL(memmove)
