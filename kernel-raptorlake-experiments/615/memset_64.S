/* SPDX-License-Identifier: GPL-2.0 */
/* =======================================================================
 *   High-performance memset, tuned for Intel Core-i7-14700KF
 * =======================================================================
 *
 *   – lives entirely in .noinstr.text → safe during very early boot
 *   – three run-time paths
 *        • FSRM/ERMS   (fast REP STOSB)
 *        • Hybrid      (AVX2 when available, scalar otherwise)
 *        • Generic     (large scalar loop)
 *   – __memset is placed LAST in the section; nothing can fall through
 *     after it, so objtool no longer warns.
 * --------------------------------------------------------------------- */

#include <linux/export.h>
#include <linux/linkage.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>          /* ALTERNATIVE*, _ASM_EXTABLE */

/* ---- temporary compatibility with old out-of-tree “FSRS” spelling ---- */
#ifndef X86_FEATURE_FSRS
# define X86_FEATURE_FSRS   X86_FEATURE_FSRM
#endif

        .section .noinstr.text, "ax"

/* ===================================================================== */
/*                           Fault handlers                              */
/* ===================================================================== */
SYM_FUNC_START_LOCAL(.Lfsrm_fault_handler)
        /* DF = 0 by the x86-64 ABI */
        movq    %r9, %rax
        RET
SYM_FUNC_END(.Lfsrm_fault_handler)

SYM_FUNC_START_LOCAL(.L_hybrid_fault_handler)
        ALTERNATIVE "nop", "vzeroupper", X86_FEATURE_AVX2
        movq    %r10, %rax
        RET
SYM_FUNC_END(.L_hybrid_fault_handler)

SYM_FUNC_START_LOCAL(.Lgeneric_loop_fault_handler)
        movq    %r9, %rax
        RET
SYM_FUNC_END(.Lgeneric_loop_fault_handler)

/* ===================================================================== */
/*        Generic scalar big-block helper  –  __memset_generic_loop       */
/* ===================================================================== */
SYM_FUNC_START_LOCAL(__memset_generic_loop)
        /* r9 already carries the original dst pointer                    */

        /* build replicated 0x010101.. pattern -------------------------- */
        movzbl  %sil, %ecx
        movabs  $0x0101010101010101, %rax
        imulq   %rcx, %rax

        cmpq    $64, %rdx
        jbe     .Lsmall_generic

        /* ---------- align destination to 64-byte cache line ---------- */
        movl    %edi, %ecx
        andl    $63, %ecx
        jz      .Lafter_align_generic

        mov     $64, %r8d
        subl    %ecx, %r8d                  /* r8 = bytes to alignment   */
        cmpq    %rdx, %r8
        cmovaq  %rdx, %r8                  /* clamp                     */

        movb    %sil, %al
        subq    %r8, %rdx
        movq    %r8, %rcx
        rep     stosb

        testq   %rdx, %rdx
        jz      .Lend_generic
        cmpq    $64, %rdx
        jbe     .Lsmall_generic

.Lafter_align_generic:
        /* -------- optional prefetch for very large ranges ------------ */
        cmpq    $256, %rdx
        jb      .Lno_prefetch_generic
17:     prefetchw 384(%rdi)
18:     prefetchw 512(%rdi)
.Lno_prefetch_generic:

        /* 64-byte chunks ---------------------------------------------- */
        movq    %rdx, %rcx
        shrq    $6,  %rcx
        jz      .Lhandle_tail_generic
        .p2align 4
.Lloop_64_generic:
19:     movq    %rax, 0*8(%rdi)
20:     movq    %rax, 1*8(%rdi)
21:     movq    %rax, 2*8(%rdi)
22:     movq    %rax, 3*8(%rdi)
23:     movq    %rax, 4*8(%rdi)
24:     movq    %rax, 5*8(%rdi)
25:     movq    %rax, 6*8(%rdi)
26:     movq    %rax, 7*8(%rdi)
        addq    $64,  %rdi
        decq    %rcx
        jnz     .Lloop_64_generic
        andq    $63,  %rdx

.Lhandle_tail_generic:
.Lsmall_generic:
        testq   %rdx, %rdx
        jz      .Lend_generic
        movq    %rdx, %rcx
        movb    %sil, %al
27:     rep     stosb

.Lend_generic:
        movq    %r9, %rax
        RET

        /* fault-table coverage ---------------------------------------- */
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

/* ===================================================================== */
/*            Hybrid path – used when FSRM/ERMSB is absent               */
/* ===================================================================== */
SYM_FUNC_START_LOCAL(.L_hybrid_path)
        movq    %rdi, %r10                     /* save dst for return   */

        /* small (<64B) ------------------------------------------------- */
        cmpq    $64, %rdx
        jb      .L_small_scalar_path

        /* ---------- AVX2 fast path  (with run-time TS guard) ---------- */
        ALTERNATIVE "jmp .L_no_avx2_fallback", "", X86_FEATURE_AVX2

        /*   CR0.TS set ?  (early boot before FPU enabled)               */
        mov     %cr0, %r11
        test    $0x8, %r11
        jnz     .L_no_avx2_fallback

        /* broadcast byte into YMM0 ------------------------------------ */
        movzbl  %sil, %eax
        vmovd   %eax, %xmm0
        vpbroadcastb %xmm0, %ymm0

        /* align dst to 32 bytes --------------------------------------- */
        movl    %edi, %ecx
        andl    $31, %ecx
        jz      .L_avx2_aligned

        mov     $32, %r8d
        subl    %ecx, %r8d
        cmpq    %rdx, %r8
        cmovaq  %rdx, %r8

        movb    %sil, %al
        subq    %r8, %rdx
        movq    %r8, %rcx
        rep     stosb

.L_avx2_aligned:
        movq    %rdx, %rcx
        shrq    $6,  %rcx
        jz      .L_avx2_remainder
        .p2align 4
.L_avx2_loop:
4:      vmovdqa %ymm0,   (%rdi)
5:      vmovdqa %ymm0, 32(%rdi)
        addq    $64, %rdi
        decq    %rcx
        jnz     .L_avx2_loop
        andq    $63, %rdx

.L_avx2_remainder:
        testq   %rdx, %rdx
        jz      .L_avx2_done
        cmpq    $32, %rdx
        jb      .L_avx2_small_rem
6:      vmovdqa %ymm0, (%rdi)
        addq    $32, %rdi
        subq    $32, %rdx
.L_avx2_small_rem:
        testq   %rdx, %rdx
        jz      .L_avx2_done
        movq    %rdx, %rcx
        movb    %sil, %al
11:     rep     stosb
.L_avx2_done:
        vzeroupper
        movq    %r10, %rax
        RET

        _ASM_EXTABLE(4b,  .L_hybrid_fault_handler)
        _ASM_EXTABLE(5b,  .L_hybrid_fault_handler)
        _ASM_EXTABLE(6b,  .L_hybrid_fault_handler)
        _ASM_EXTABLE(11b, .L_hybrid_fault_handler)

        /* ---------- no-AVX2 fall-back: jump BACK to generic loop ----- */
.L_no_avx2_fallback:
        movq    %r10, %r9                 /* generic loop expects r9  */
        jmp     __memset_generic_loop     /* backwards → objtool OK   */

/* -------------------- <64B scalar fast path -------------------------- */
.L_small_scalar_path:
        movzbl  %sil, %ecx
        movabs  $0x0101010101010101, %rax
        imulq   %rcx, %rax

        cmpq    $8, %rdx
        jb      .L_small_lt8
        movq    %rdx, %rcx
        shrq    $3,  %rcx
        jz      .L_small_remainder
7:      movq    %rax, (%rdi)
        addq    $8, %rdi
        decq    %rcx
        jnz     7b
        andq    $7, %rdx
.L_small_remainder:
        jz      .L_small_done
.L_small_lt8:
        cmpq    $4, %rdx
        jb      .L_small_lt4
8:      movl    %eax, (%rdi)
        addq    $4,  %rdi
        subq    $4,  %rdx
.L_small_lt4:
        cmpq    $2, %rdx
        jb      .L_small_lt2
9:      movw    %ax,  (%rdi)
        addq    $2, %rdi
        subq    $2, %rdx
.L_small_lt2:
        testq   %rdx, %rdx
        jz      .L_small_done
10:     movb    %al, (%rdi)
.L_small_done:
        movq    %r10, %rax
        RET

        _ASM_EXTABLE(7b,  .L_hybrid_fault_handler)
        _ASM_EXTABLE(8b,  .L_hybrid_fault_handler)
        _ASM_EXTABLE(9b,  .L_hybrid_fault_handler)
        _ASM_EXTABLE(10b, .L_hybrid_fault_handler)
SYM_FUNC_END(.L_hybrid_path)

/* ===================================================================== */
/*                           PUBLIC ENTRY                                */
/* ===================================================================== */
SYM_FUNC_START(__memset)                    /* *** LAST in section *** */
        movq    %rdi, %r9                  /* preserve dst for return  */

        testq   %rdx, %rdx                 /* len == 0 ?               */
        jz      .L_ret

        /* If FSRM absent → jump forward-patched to hybrid path          */
        ALTERNATIVE "jmp .L_hybrid_path", "nop", X86_FEATURE_FSRS

        /* ------------ inline FSRM fast REP-string path --------------- */
        movb    %sil, %al
        movq    %rdx, %rcx
1:      rep     stosb

.L_ret:
        movq    %r9, %rax
        ret

        _ASM_EXTABLE(1b, .Lfsrm_fault_handler)
SYM_FUNC_END(__memset)

/* --------------------------------------------------------------------- */
/*                      Symbol exports / aliases                         */
/* --------------------------------------------------------------------- */
EXPORT_SYMBOL(__memset)
SYM_FUNC_ALIAS_MEMFUNC(memset, __memset)
EXPORT_SYMBOL(memset)
