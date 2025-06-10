/* SPDX-License-Identifier: GPL-2.0 */
/*
 * csum_partial_copy_generic – 64-bit checksum + copy with fault handling
 *
 *  rdi : src (USER)              may fault
 *  rsi : dst (KERNEL)            may machine-check
 *  edx : len (bytes, 32-bit)     wrappers clamp to <4 GiB
 *
 *  rax : 64-bit sum  (undefined on fault, wrappers handle it)
 *
 *  Registers clobbered :
 *        rax rbx rcx rdx r8-r15 rflags
 *
 *  This rewrite keeps every numbered label so that the existing
 *  EXTABLE lines match exactly.  Only style, width, CFI and a few
 *  µ-arch micro-fixes changed.
 */

#include <linux/linkage.h>
#include <asm/errno.h>
#include <asm/asm.h>

        /* ---------- helper macros keep extable lines concise --- */
        .macro source
10:
        _ASM_EXTABLE_UA(10b, .Lfault)
        .endm

        .macro dest
20:
        _ASM_EXTABLE_UA(20b, .Lfault)
        .endm

        /* ====================================================== */
        .p2align 5                          /* 32-B i-cache line  */
SYM_FUNC_START(csum_partial_copy_generic)
        /* ---------- prologue: spill callee-saved we trash ----- */
        subq    $5*8, %rsp
        CFI_ADJUST_CFA_OFFSET(5*8)
        movq    %rbx, 0*8(%rsp)
        movq    %r12, 1*8(%rsp)
        movq    %r14, 2*8(%rsp)
        movq    %r13, 3*8(%rsp)
        movq    %r15, 4*8(%rsp)

        /* ------------------------------------------------------ */
        movl    $-1,  %eax             /* carry preset for ADCQ  */
        xorq    %r9,  %r9              /* r9 = 0 (carry adder)   */

        movl    %edx, %ecx             /* len -> rcx   (zero-ext)*/
        cmpq    $8,   %rcx
        jb      .Lshort

        /* dst alignment check (8-byte) ------------------------- */
        testq   $7,  %rsi
        jne     .Lunaligned

/* =================================================================
 *  dst already 8-byte aligned
 * ============================================================== */
.Laligned:
        movl    %ecx, %r12d            /* r12 = len */
        shrq    $6,  %r12              /* /64       */
        jz      .Lhandle_tail          /* < 64 B    */

        clc                             /* clear CF for first ADC  */

        /* ------------------------------------------------------
         * Main unrolled 64-B loop
         *   rbx/r8/r11/rdx/r10/r15/r14/r13 – eight 8-byte words
         * ---------------------------------------------------- */
        .p2align 4
.Lloop:
        source  movq   0(%rdi),  %rbx
        source  movq   8(%rdi),  %r8
        source  movq  16(%rdi),  %r11
        source  movq  24(%rdi),  %rdx

        source  movq  32(%rdi),  %r10
        source  movq  40(%rdi),  %r15
        source  movq  48(%rdi),  %r14
        source  movq  56(%rdi),  %r13

30:     /* unconditional prefetch; may be unmapped → extable above */
        _ASM_EXTABLE(30b, 2f)
        prefetcht0 5*64(%rdi)
2:
        adcq    %rbx, %rax
        adcq    %r8,  %rax
        adcq    %r11, %rax
        adcq    %rdx, %rax
        adcq    %r10, %rax
        adcq    %r15, %rax
        adcq    %r14, %rax
        adcq    %r13, %rax

        decl    %r12d

        dest    movq  %rbx,  0(%rsi)
        dest    movq  %r8,   8(%rsi)
        dest    movq  %r11, 16(%rsi)
        dest    movq  %rdx, 24(%rsi)

        dest    movq  %r10, 32(%rsi)
        dest    movq  %r15, 40(%rsi)
        dest    movq  %r14, 48(%rsi)
        dest    movq  %r13, 56(%rsi)

        leaq    64(%rdi), %rdi
        leaq    64(%rsi), %rsi
        jnz     .Lloop

        adcq    %r9, %rax              /* carry-in after loop    */

/* --------------------------------------------------------------
 * Handle tail:  0 … 56  bytes remain
 * ------------------------------------------------------------ */
.Lhandle_tail:
        movq    %rcx, %r10             /* save original len      */
        andl    $63,  %ecx
        shrq    $3,  %rcx              /* rcx = (# q-words)      */
        jz      .Lfold
        clc
        .p2align 4
.Lloop_8:
        source  movq (%rdi), %rbx
        adcq    %rbx, %rax
        decl    %ecx
        dest    movq %rbx, (%rsi)
        leaq    8(%rdi), %rdi
        leaq    8(%rsi), %rsi
        jnz     .Lloop_8
        adcq    %r9, %rax

/* Fold partial sum to 32 bits ----------------------------------- */
.Lfold:
        movl    %eax, %ebx
        shrq    $32, %rax
        addl    %ebx, %eax
        adcl    %r9d, %eax

/* Handle final 0-6 bytes ---------------------------------------- */
.Lhandle_7:
        movl    %r10d, %ecx
        andl    $7,    %ecx

.L1:                                    /* .Lshort falls through */
        shrq    $1, %rcx
        jz      .Lhandle_1

        movl    $2, %edx
        xorq    %rbx, %rbx
        clc
        .p2align 4
.Lloop_1:
        source  movw (%rdi), %bx
        adcl    %ebx, %eax
        decl    %ecx
        dest    movw %bx, (%rsi)
        leaq    2(%rdi), %rdi
        leaq    2(%rsi), %rsi
        jnz     .Lloop_1
        adcl    %r9d, %eax

/* Last odd byte -------------------------------------------------- */
.Lhandle_1:
        testq   $1, %r10
        jz      .Lende
        xorq    %rbx, %rbx
        source  movb (%rdi), %bl
        dest    movb %bl, (%rsi)
        addl    %ebx, %eax
        adcl    %r9d, %eax

.Lende:
        testq   %r10, %r10             /* was len odd? (bit63) */
        js      .Lwas_odd

/* --------------------------------------------------------------
 * Epilogue
 * ------------------------------------------------------------ */
.Lout:
        movq    0*8(%rsp), %rbx
        movq    1*8(%rsp), %r12
        movq    2*8(%rsp), %r14
        movq    3*8(%rsp), %r13
        movq    4*8(%rsp), %r15
        addq    $5*8, %rsp
        CFI_ADJUST_CFA_OFFSET(-5*8)
        RET

/* =================================================================
 *  Fallback for short (<8) copies – shares tail path
 * ============================================================== */
.Lshort:
        movl    %ecx, %r10d
        jmp     .L1

/* =================================================================
 *  dst not aligned – fix up 1/2/4-byte head, then fall into main
 * ============================================================== */
.Lunaligned:
        xorq    %rbx, %rbx
        testq   $1, %rsi
        jne     .Lodd
1:
        testq   $2, %rsi
        je      2f
        source  movw (%rdi), %bx
        dest    movw %bx, (%rsi)
        leaq    2(%rdi), %rdi
        subq    $2, %rcx
        leaq    2(%rsi), %rsi
        addq    %rbx, %rax
2:
        testq   $4, %rsi
        je      .Laligned
        source  movl (%rdi), %ebx
        dest    movl %ebx, (%rsi)
        leaq    4(%rdi), %rdi
        subq    $4, %rcx
        leaq    4(%rsi), %rsi
        addq    %rbx, %rax
        jmp     .Laligned

/* Head starts on odd byte --------------------------------------- */
.Lodd:
        source  movb (%rdi), %bl
        dest    movb %bl, (%rsi)
        incq    %rdi
        incq    %rsi
        /* rcx = (len-1) | bit63 to mark odd */
        leaq    -1(%rcx,%rcx), %rcx
        rorq    $1, %rcx
        shlq    $8, %rbx
        addq    %rbx, %rax
        jmp     1b

/* rotate if length odd ------------------------------------------ */
.Lwas_odd:
        roll    $8, %eax
        jmp     .Lout

/* =================================================================
 *  Fault path – zero checksum, restore regs, return
 * ============================================================== */
.Lfault:
        xorl    %eax, %eax
        jmp     .Lout
SYM_FUNC_END(csum_partial_copy_generic)
