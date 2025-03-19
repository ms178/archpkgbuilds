/* SPDX-License-Identifier: GPL-2.0-only */
/*
 * Copyright 2002 Andi Kleen
 * Optimized for Intel Raptor Lake by [Your Name], 2025
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/cfi_types.h>
#include <asm/errno.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>

.section .noinstr.text, "ax"

/*
 * memcpy - Copy a memory block.
 * Optimized for Intel Raptor Lake architecture with enhanced vectorization.
 *
 * Input:
 *  rdi destination
 *  rsi source
 *  rdx count
 *
 * Output:
 * rax original destination
 */
SYM_TYPED_FUNC_START(__memcpy)
        /* Store return value (original destination) */
        movq %rdi, %rax

        /* Optimize for zero-length copy first */
        testq %rdx, %rdx
        jz .L_return

        /* Ensure proper direction flag at the start */
        cld          /* Keep only this CLD at the main entry point */

        /* Fast path with FSRM - use rep movsb for all sizes */
        ALTERNATIVE "jmp memcpy_orig", "", X86_FEATURE_FSRM

        movq %rdx, %rcx
1:      rep movsb
.L_return:
        RET

        /* Exception handler for FSRM path */
        _ASM_EXTABLE(1b, .Lfsrm_fault_handler)
SYM_FUNC_END(__memcpy)
EXPORT_SYMBOL(__memcpy)

SYM_FUNC_ALIAS_MEMFUNC(memcpy, __memcpy)
EXPORT_SYMBOL(memcpy)

/* Fault handler for FSRM path */
.Lfsrm_fault_handler:
        cld                /* Ensure direction flag is cleared */
        RET                /* Return directly for consistency */

/* Optimized path for P-cores with AVX2 support for large copies */
SYM_FUNC_START_LOCAL(memcpy_pcore_path)
        /* Save return value */
        movq %rdi, %rax

        /* Optimize for zero-length copy first */
        testq %rdx, %rdx
        jz .Lpcore_done

        /* Preserve callee-saved registers we'll use */
        pushq %rbp
        pushq %rbx
        pushq %r12
        pushq %r13
        pushq %r14
        pushq %r15

        /* Skip to regular path for small copies */
        cmpq $256, %rdx
        jb .Lrestore_and_jump_to_orig

        /* Check if AVX2 is available */
        ALTERNATIVE "jmp .Lrestore_and_jump_to_orig", "", X86_FEATURE_AVX2

        /* Check if kernel allows AVX usage */
        ALTERNATIVE "jmp .Lrestore_and_jump_to_orig", "", X86_FEATURE_OSXSAVE

        /* Check for alignment */
        movl %edi, %ecx
        andl $63, %ecx    /* Check 64-byte alignment for Raptor Lake */
        jz .Lavx_aligned_copy

        /* Calculate bytes needed to align destination to 64-byte boundary */
        movl $64, %r8d
        subl %ecx, %r8d

        /* Ensure alignment doesn't exceed total size */
        movq %r8, %rcx
        cmpq %rdx, %rcx
        jbe 2f
        movq %rdx, %rcx

2:      /* Copy bytes to align */
        subq %rcx, %rdx     /* Adjust remaining count */
        rep movsb           /* Use rep movsb for alignment */

        /* Skip AVX if no bytes remain */
        testq %rdx, %rdx
        jz .Lexit_pcore

.Lavx_aligned_copy:
        /* For very large transfers (>1MB), use non-temporal stores */
        cmpq $1048576, %rdx
        jae .Lavx_nt_copy

        /* Set up for 128-byte chunk copies */
        movq %rdx, %rcx
        shrq $7, %rcx     /* Divide by 128 */
        movq %rcx, %r8    /* Save loop count for fault handling */
        jz .Lavx_remainder

        /* Add prefetching for large copies (>1KB) */
        cmpq $1024, %rdx
        jl .Lavx_loop

        /* Prefetch ahead with optimized distances for Raptor Lake */
3:      prefetcht0 384(%rsi)  /* L1 cache */
4:      prefetcht1 512(%rsi)  /* L2 cache */
5:      prefetcht2 1024(%rsi) /* L3 cache */

        /* Main AVX2 copy loop - 128 bytes per iteration */
.Lavx_loop:
        /* Use vmovdqu for unaligned source */
6:      vmovdqu 0*32(%rsi), %ymm0
7:      vmovdqu 1*32(%rsi), %ymm1
8:      vmovdqu 2*32(%rsi), %ymm2
9:      vmovdqu 3*32(%rsi), %ymm3

        /* Add periodic prefetching for very large transfers */
        testq $0x7, %rcx
        jnz 10f
        cmpq $16, %rcx
        jl 10f
11:     prefetcht0 384(%rsi)  /* L1 cache */
12:     prefetcht1 512(%rsi)  /* L2 cache */

10:     vmovdqa %ymm0, 0*32(%rdi)
13:     vmovdqa %ymm1, 1*32(%rdi)
14:     vmovdqa %ymm2, 2*32(%rdi)
15:     vmovdqa %ymm3, 3*32(%rdi)

        addq $128, %rsi
        addq $128, %rdi
        decq %rcx
        jnz .Lavx_loop

        /* Calculate remaining bytes */
        andq $127, %rdx
        jmp .Lavx_remainder

        /* Non-temporal copy for very large buffers */
.Lavx_nt_copy:
        /* Set up for 128-byte chunk copies */
        movq %rdx, %rcx
        shrq $7, %rcx     /* Divide by 128 */
        movq %rcx, %r8    /* Save loop count for fault handling */
        andq $127, %rdx   /* Save remainder */

        /* Main AVX2 non-temporal copy loop - minimize cache pollution */
.Lavx_nt_loop:
        /* Pre-load with aggressive prefetch */
16:     prefetcht0 512(%rsi)

        /* Use vmovdqu for unaligned source */
17:     vmovdqu 0*32(%rsi), %ymm0
18:     vmovdqu 1*32(%rsi), %ymm1
19:     vmovdqu 2*32(%rsi), %ymm2
20:     vmovdqu 3*32(%rsi), %ymm3

        vmovntdq %ymm0, 0*32(%rdi)
21:     vmovntdq %ymm1, 1*32(%rdi)
22:     vmovntdq %ymm2, 2*32(%rdi)
23:     vmovntdq %ymm3, 3*32(%rdi)

        addq $128, %rsi
        addq $128, %rdi
        decq %rcx
        jnz .Lavx_nt_loop

        /* Ensure non-temporal stores are visible */
        sfence

.Lavx_remainder:
        /* Clear AVX state to avoid penalties */
        vzeroupper

        /* Handle remaining bytes (<128) */
        testq %rdx, %rdx
        jz .Lexit_pcore

        /* Use rep movsb for tail */
        movq %rdx, %rcx
        rep movsb

.Lexit_pcore:
        /* Restore saved registers and return */
        popq %r15
        popq %r14
        popq %r13
        popq %r12
        popq %rbx
        popq %rbp
.Lpcore_done:
        RET

.Lrestore_and_jump_to_orig:
        /* Restore registers before jumping to regular path */
        popq %r15
        popq %r14
        popq %r13
        popq %r12
        popq %rbx
        popq %rbp
        jmp memcpy_orig

.Lavx_fault_handler:
        /* Clean up AVX state, clear direction flag, and return original destination */
        vzeroupper
        cld                /* Ensure direction flag is cleared */
        popq %r15
        popq %r14
        popq %r13
        popq %r12
        popq %rbx
        popq %rbp
        RET

        _ASM_EXTABLE(2b, .Lavx_fault_handler)
        _ASM_EXTABLE(3b, .Lavx_fault_handler)  /* Prefetch */
        _ASM_EXTABLE(4b, .Lavx_fault_handler)  /* Prefetch */
        _ASM_EXTABLE(5b, .Lavx_fault_handler)  /* Prefetch */
        _ASM_EXTABLE(6b, .Lavx_fault_handler)
        _ASM_EXTABLE(7b, .Lavx_fault_handler)
        _ASM_EXTABLE(8b, .Lavx_fault_handler)
        _ASM_EXTABLE(9b, .Lavx_fault_handler)
        _ASM_EXTABLE(10b, .Lavx_fault_handler)
        _ASM_EXTABLE(11b, .Lavx_fault_handler) /* Prefetch */
        _ASM_EXTABLE(12b, .Lavx_fault_handler) /* Prefetch */
        _ASM_EXTABLE(13b, .Lavx_fault_handler)
        _ASM_EXTABLE(14b, .Lavx_fault_handler)
        _ASM_EXTABLE(15b, .Lavx_fault_handler)
        _ASM_EXTABLE(16b, .Lavx_fault_handler) /* Prefetch */
        _ASM_EXTABLE(17b, .Lavx_fault_handler)
        _ASM_EXTABLE(18b, .Lavx_fault_handler)
        _ASM_EXTABLE(19b, .Lavx_fault_handler)
        _ASM_EXTABLE(20b, .Lavx_fault_handler)
        _ASM_EXTABLE(21b, .Lavx_fault_handler)
        _ASM_EXTABLE(22b, .Lavx_fault_handler)
        _ASM_EXTABLE(23b, .Lavx_fault_handler)
SYM_FUNC_END(memcpy_pcore_path)

/* Original path with Raptor Lake optimizations */
SYM_FUNC_START_LOCAL(memcpy_orig)
        /* Store return value */
        movq %rdi, %rax

        /* Optimize for zero-length copy first */
        testq %rdx, %rdx
        jz .Lorig_done

        /* Preserve registers we'll be using */
        pushq %rbp
        pushq %rbx
        pushq %r12
        pushq %r13
        pushq %r14
        pushq %r15

        /* Simple overlap check - if dst > src, use backward copy */
        cmpq %rsi, %rdi
        ja .Lcopy_backward_entry

        /* Adjust size threshold to 64 bytes for Raptor Lake cache lines */
        cmpq $0x40, %rdx
        jb .Lmedium_copy

        /* Align destination to cache line if copy is large enough */
        movl %edi, %ecx
        andl $0x3F, %ecx
        jz .Laligned_forward_copy

        /* Only align if copy is large (>128 bytes) */
        cmpq $0x80, %rdx
        jb .Lmedium_copy_align

        /* Calculate bytes to align */
        movl $64, %r8d
        subl %ecx, %r8d
        movq %r8, %rcx

        /* Ensure we don't over-copy */
        cmpq %rdx, %rcx
        jbe 24f
        movq %rdx, %rcx

24:     /* Adjust remaining count and align */
        subq %rcx, %rdx
        rep movsb

        /* Check if we have bytes left to copy */
        testq %rdx, %rdx
        jz .Lexit_with_restore

.Lmedium_copy_align:
        /* Align medium-sized copies (64-128 bytes) to 64 bytes for Raptor Lake */
        cmpq $128, %rdx
        ja .Laligned_forward_copy
        cmpq $64, %rdx
        jb .Lhandle_tail
        movl %edi, %ecx
        andl $0x3F, %ecx  /* Check 64-byte alignment */
        jz .Laligned_forward_copy
        movl $64, %r8d
        subl %ecx, %r8d
        movq %r8, %rcx
        subq %rcx, %rdx
25:     rep movsb
        testq %rdx, %rdx
        jz .Lexit_with_restore

.Laligned_forward_copy:
        /* Skip small copy path for larger copies */
        cmpq $0x40, %rdx
        jb .Lhandle_tail

        /* Prefetch for large copies on Raptor Lake */
        cmpq $512, %rdx
        jb .Lno_prefetch

26:     prefetcht0 384(%rsi)  /* Optimized for Raptor Lake L1 */
27:     prefetcht1 512(%rsi)
.Lno_prefetch:
        /* Use 64-byte chunks for Raptor Lake's cache line size */
        movq %rdx, %rcx
        shrq $6, %rcx     /* Divide by 64 */
        movq %rcx, %r8    /* Save loop count for fault handling */
        andq $63, %rdx    /* Save remainder */

        testq %rcx, %rcx
        jz .Lhandle_tail

.Lcopy_forward_loop:
        /* Copy 64 bytes (full cache line) at a time */
28:     movq 0*8(%rsi), %r8
29:     movq 1*8(%rsi), %r9
30:     movq 2*8(%rsi), %r10
31:     movq 3*8(%rsi), %r11
32:     movq 4*8(%rsi), %r12
33:     movq 5*8(%rsi), %r13
34:     movq 6*8(%rsi), %r14
35:     movq 7*8(%rsi), %r15

        /* Add periodic prefetching for large transfers */
        testq $0x3, %rcx
        jnz .Lno_extra_prefetch_fwd
        cmpq $8, %rcx
        jl .Lno_extra_prefetch_fwd
36:     prefetcht0 384(%rsi)  /* Optimized for Raptor Lake */
37:     prefetcht1 512(%rsi)

.Lno_extra_prefetch_fwd:
38:     movq %r8,  0*8(%rdi)
39:     movq %r9,  1*8(%rdi)
40:     movq %r10, 2*8(%rdi)
41:     movq %r11, 3*8(%rdi)
42:     movq %r12, 4*8(%rdi)
43:     movq %r13, 5*8(%rdi)
44:     movq %r14, 6*8(%rdi)
45:     movq %r15, 7*8(%rdi)

        leaq 8*8(%rsi), %rsi
        leaq 8*8(%rdi), %rdi

        decq %rcx
        jnz .Lcopy_forward_loop

.Lhandle_tail:
        /* Nothing to copy */
        testq %rdx, %rdx
        jz .Lexit_with_restore

.Lmedium_copy:
        /* Adjusted thresholds for medium copies */
        cmpq $32, %rdx
        jb .Lless_32bytes

        /* Specialized handling for 32-64 bytes */
        cmpq $48, %rdx
        jb .Lcopy_32_to_48

        /* Copy 48-64 bytes with unrolled movq */
46:     movq 0*8(%rsi), %r8
47:     movq 1*8(%rsi), %r9
48:     movq 2*8(%rsi), %r10
49:     movq 3*8(%rsi), %r11

        /* For safety, check if rdx >= 32 before using negative offsets */
        movq %rdx, %rcx
        subq $32, %rcx    /* rcx = rdx - 32 */

50:     movq (%rsi,%rcx), %r12
51:     movq 8(%rsi,%rcx), %r13
52:     movq 16(%rsi,%rcx), %r14
53:     movq 24(%rsi,%rcx), %r15

54:     movq %r8,  0*8(%rdi)
55:     movq %r9,  1*8(%rdi)
56:     movq %r10, 2*8(%rdi)
57:     movq %r11, 3*8(%rdi)
58:     movq %r12, (%rdi,%rcx)
59:     movq %r13, 8(%rdi,%rcx)
60:     movq %r14, 16(%rdi,%rcx)
61:     movq %r15, 24(%rdi,%rcx)

        jmp .Lexit_with_restore

.Lcopy_32_to_48:
        /* Copy 32-48 bytes with unrolled movq */
62:     movq 0*8(%rsi), %r8
63:     movq 1*8(%rsi), %r9
64:     movq 2*8(%rsi), %r10
65:     movq 3*8(%rsi), %r11

        /* Calculate remaining bytes safer */
        movq %rdx, %rcx
        subq $32, %rcx
        testq %rcx, %rcx
        jz .Lcopy_exactly_32

        /* Copy remaining bytes (up to 16) */
66:     movq (%rsi,%rcx), %r12
67:     movq 8(%rsi,%rcx), %r13

68:     movq %r8,  0*8(%rdi)
69:     movq %r9,  1*8(%rdi)
70:     movq %r10, 2*8(%rdi)
71:     movq %r11, 3*8(%rdi)
72:     movq %r12, (%rdi,%rcx)
73:     movq %r13, 8(%rdi,%rcx)

        jmp .Lexit_with_restore

.Lcopy_exactly_32:
        /* Exactly 32 bytes */
74:     movq %r8,  0*8(%rdi)
75:     movq %r9,  1*8(%rdi)
76:     movq %r10, 2*8(%rdi)
77:     movq %r11, 3*8(%rdi)
        jmp .Lexit_with_restore

.Lless_32bytes:
        cmpq $16, %rdx
        jb .Lless_16bytes

        /* Copy 16-32 bytes */
78:     movq 0*8(%rsi), %r8
79:     movq 1*8(%rsi), %r9

        /* Calculate remaining bytes safer */
        movq %rdx, %rcx
        subq $16, %rcx
        testq %rcx, %rcx
        jz .Lcopy_exactly_16

        /* Copy remaining bytes (up to 16) */
80:     movq (%rsi,%rcx), %r10
81:     movq 8(%rsi,%rcx), %r11

82:     movq %r8, 0*8(%rdi)
83:     movq %r9, 1*8(%rdi)
84:     movq %r10, (%rdi,%rcx)
85:     movq %r11, 8(%rdi,%rcx)

        jmp .Lexit_with_restore

.Lcopy_exactly_16:
        /* Exactly 16 bytes */
86:     movq %r8, 0*8(%rdi)
87:     movq %r9, 1*8(%rdi)
        jmp .Lexit_with_restore

.Lless_16bytes:
        /* Add special case for 12 bytes (3 integers) */
        cmpq $12, %rdx
        je .Lspecial_12bytes

        cmpq $8, %rdx
        jb .Lless_8bytes

        /* Copy 8-12 bytes */
88:     movq (%rsi), %r8

        /* Handle any remaining bytes (1-8) */
        subq $8, %rdx
        jz .Lcopy_exactly_8

        /* For 9-11 bytes */
89:     movl (%rsi,%rdx), %ecx
90:     movq %r8, (%rdi)
91:     movl %ecx, (%rdi,%rdx)
        jmp .Lexit_with_restore

.Lcopy_exactly_8:
        /* Exactly 8 bytes */
92:     movq %r8, (%rdi)
        jmp .Lexit_with_restore

.Lspecial_12bytes:
        /* Optimized path for exactly 12 bytes (common case) */
93:     movq (%rsi), %r8
94:     movl 8(%rsi), %ecx

95:     movq %r8, (%rdi)
96:     movl %ecx, 8(%rdi)
        jmp .Lexit_with_restore

.Lless_8bytes:
        cmpq $4, %rdx
        jb .Lless_4bytes

        /* Copy 4-7 bytes */
97:     movl (%rsi), %ecx

        /* For 5-7 bytes */
        subq $4, %rdx
        jz .Lcopy_exactly_4

        /* Save temp in r10 to avoid overwriting rsi */
98:     movl (%rsi,%rdx), %r10d
99:     movl %ecx, (%rdi)
100:    movl %r10d, (%rdi,%rdx)
        jmp .Lexit_with_restore

.Lcopy_exactly_4:
        /* Exactly 4 bytes */
101:    movl %ecx, (%rdi)
        jmp .Lexit_with_restore

.Lless_4bytes:
        /* Safe copy for 1-3 bytes */
        cmpq $0, %rdx
        je .Lexit_with_restore

        /* First byte */
102:    movzbl (%rsi), %ecx
103:    movb %cl, (%rdi)

        cmpq $1, %rdx
        je .Lexit_with_restore

        /* Second byte */
104:    movzbl 1(%rsi), %ecx
105:    movb %cl, 1(%rdi)

        cmpq $2, %rdx
        je .Lexit_with_restore

        /* Third byte */
106:    movzbl 2(%rsi), %ecx
107:    movb %cl, 2(%rdi)

.Lexit_with_restore:
        /* Restore preserved registers */
        popq %r15
        popq %r14
        popq %r13
        popq %r12
        popq %rbx
        popq %rbp
.Lorig_done:
        RET

/*
 * Handle backward copy for overlapping regions
 * dst > src
 */
.Lcopy_backward_entry:
        /* Jump to AVX path if large enough and AVX2 is available */
        cmpq $256, %rdx
        jb .Lcopy_backward
        ALTERNATIVE "jmp .Lcopy_backward", "", X86_FEATURE_AVX2
        ALTERNATIVE "jmp .Lcopy_backward", "", X86_FEATURE_OSXSAVE
        jmp memcpy_pcore_path  /* Use AVX2 path for large backward copies */

.Lcopy_backward:
        /* Check for zero length */
        testq %rdx, %rdx
        jz .Lbackward_done_no_restore

        /* Set direction flag for backward copy */
        std

        /* Move pointers to the end of buffers */
        leaq -1(%rsi,%rdx), %rsi
        leaq -1(%rdi,%rdx), %rdi

        /* Optimize for small backward copies */
        cmpq $64, %rdx
        jae .Lcopy_backward_large

        /* Use rep movsb for small backward copies */
        movq %rdx, %rcx
108:    rep movsb

        /* Clear direction flag before returning */
        cld
        jmp .Lexit_with_restore

.Lcopy_backward_large:
        /* Process 64-byte chunks - optimized for Raptor Lake cache line size */
        movq %rdx, %rcx
        shrq $6, %rcx
        movq %rcx, %r8    /* Save loop count for fault handling */
        jz .Lbackward_handle_tail

        /* Save remainder bytes */
        andq $63, %rdx

.Lbackward_loop_64:
        /* Move pointers back 64 bytes, then load/store */
        subq $64, %rsi
        subq $64, %rdi

109:    movq 56(%rsi), %r8
110:    movq 48(%rsi), %r9
111:    movq 40(%rsi), %r10
112:    movq 32(%rsi), %r11
113:    movq 24(%rsi), %r12
114:    movq 16(%rsi), %r13
115:    movq 8(%rsi), %r14
116:    movq (%rsi), %r15

117:    movq %r8, 56(%rdi)
118:    movq %r9, 48(%rdi)
119:    movq %r10, 40(%rdi)
120:    movq %r11, 32(%rdi)
121:    movq %r12, 24(%rdi)
122:    movq %r13, 16(%rdi)
123:    movq %r14, 8(%rdi)
124:    movq %r15, (%rdi)

        decq %rcx
        jnz .Lbackward_loop_64

.Lbackward_handle_tail:
        /* Handle remaining bytes */
        testq %rdx, %rdx
        jz .Lbackward_done

        /* Handle remaining bytes with rep movsb */
        movq %rdx, %rcx
125:    rep movsb

.Lbackward_done:
        /* Restore registers and clear direction flag */
        cld
        popq %r15
        popq %r14
        popq %r13
        popq %r12
        popq %rbx
        popq %rbp
.Lbackward_done_no_restore:
        RET

.Lforward_read_fault:
        /* Calculate remaining bytes for read fault */
        shlq $6, %r8     /* Convert back to bytes */
        addq %rdx, %r8   /* Add remainder bytes */
        movq %r8, %rcx   /* Return uncopied bytes */
        jmp .Lcopy_fault_handler

.Lforward_write_fault:
        /* Calculate remaining bytes for write fault */
        shlq $6, %r8     /* Convert back to bytes */
        addq %rdx, %r8   /* Add remainder bytes */
        movq %r8, %rcx   /* Return uncopied bytes */
        jmp .Lcopy_fault_handler

.Lbackward_read_fault:
        /* Calculate remaining bytes for read fault */
        shlq $6, %r8     /* Convert back to bytes */
        addq %rdx, %r8   /* Add remainder bytes */
        movq %r8, %rcx   /* Return uncopied bytes */
        jmp .Lcopy_backward_fault_handler

.Lbackward_write_fault:
        /* Calculate remaining bytes for write fault */
        shlq $6, %r8     /* Convert back to bytes */
        addq %rdx, %r8   /* Add remainder bytes */
        movq %r8, %rcx   /* Return uncopied bytes */
        jmp .Lcopy_backward_fault_handler

/* Exception handlers for regular copy */
.Lcopy_fault_handler:
        /* Restore registers, clear direction flag, and return original destination */
        popq %r15
        popq %r14
        popq %r13
        popq %r12
        popq %rbx
        popq %rbp
        cld  /* Ensure direction flag is cleared */
        RET

.Lcopy_backward_fault_handler:
        /* Restore registers, clear direction flag, and return original destination */
        cld
        popq %r15
        popq %r14
        popq %r13
        popq %r12
        popq %rbx
        popq %rbp
        RET

        /* Exception table for forward copy (regular path) */
        _ASM_EXTABLE(24b, .Lcopy_fault_handler)
        _ASM_EXTABLE(25b, .Lcopy_fault_handler)
        _ASM_EXTABLE(26b, .Lforward_read_fault) /* Prefetch */
        _ASM_EXTABLE(27b, .Lforward_read_fault) /* Prefetch */
        _ASM_EXTABLE(28b, .Lforward_read_fault)
        _ASM_EXTABLE(29b, .Lforward_read_fault)
        _ASM_EXTABLE(30b, .Lforward_read_fault)
        _ASM_EXTABLE(31b, .Lforward_read_fault)
        _ASM_EXTABLE(32b, .Lforward_read_fault)
        _ASM_EXTABLE(33b, .Lforward_read_fault)
        _ASM_EXTABLE(34b, .Lforward_read_fault)
        _ASM_EXTABLE(35b, .Lforward_read_fault)
        _ASM_EXTABLE(36b, .Lforward_read_fault) /* Prefetch */
        _ASM_EXTABLE(37b, .Lforward_read_fault) /* Prefetch */
        _ASM_EXTABLE(38b, .Lforward_write_fault)
        _ASM_EXTABLE(39b, .Lforward_write_fault)
        _ASM_EXTABLE(40b, .Lforward_write_fault)
        _ASM_EXTABLE(41b, .Lforward_write_fault)
        _ASM_EXTABLE(42b, .Lforward_write_fault)
        _ASM_EXTABLE(43b, .Lforward_write_fault)
        _ASM_EXTABLE(44b, .Lforward_write_fault)
        _ASM_EXTABLE(45b, .Lforward_write_fault)
        _ASM_EXTABLE(46b, .Lforward_read_fault)
        _ASM_EXTABLE(47b, .Lforward_read_fault)
        _ASM_EXTABLE(48b, .Lforward_read_fault)
        _ASM_EXTABLE(49b, .Lforward_read_fault)
        _ASM_EXTABLE(50b, .Lforward_read_fault)
        _ASM_EXTABLE(51b, .Lforward_read_fault)
        _ASM_EXTABLE(52b, .Lforward_read_fault)
        _ASM_EXTABLE(53b, .Lforward_read_fault)
        _ASM_EXTABLE(54b, .Lforward_write_fault)
        _ASM_EXTABLE(55b, .Lforward_write_fault)
        _ASM_EXTABLE(56b, .Lforward_write_fault)
        _ASM_EXTABLE(57b, .Lforward_write_fault)
        _ASM_EXTABLE(58b, .Lforward_write_fault)
        _ASM_EXTABLE(59b, .Lforward_write_fault)
        _ASM_EXTABLE(60b, .Lforward_write_fault)
        _ASM_EXTABLE(61b, .Lforward_write_fault)
        _ASM_EXTABLE(62b, .Lforward_read_fault)
        _ASM_EXTABLE(63b, .Lforward_read_fault)
_ASM_EXTABLE(64b, .Lforward_read_fault)
        _ASM_EXTABLE(65b, .Lforward_read_fault)
        _ASM_EXTABLE(66b, .Lforward_read_fault)
        _ASM_EXTABLE(67b, .Lforward_read_fault)
        _ASM_EXTABLE(68b, .Lforward_write_fault)
        _ASM_EXTABLE(69b, .Lforward_write_fault)
        _ASM_EXTABLE(70b, .Lforward_write_fault)
        _ASM_EXTABLE(71b, .Lforward_write_fault)
        _ASM_EXTABLE(72b, .Lforward_write_fault)
        _ASM_EXTABLE(73b, .Lforward_write_fault)
        _ASM_EXTABLE(74b, .Lforward_write_fault)
        _ASM_EXTABLE(75b, .Lforward_write_fault)
        _ASM_EXTABLE(76b, .Lforward_write_fault)
        _ASM_EXTABLE(77b, .Lforward_write_fault)
        _ASM_EXTABLE(78b, .Lforward_read_fault)
        _ASM_EXTABLE(79b, .Lforward_read_fault)
        _ASM_EXTABLE(80b, .Lforward_read_fault)
        _ASM_EXTABLE(81b, .Lforward_read_fault)
        _ASM_EXTABLE(82b, .Lforward_write_fault)
        _ASM_EXTABLE(83b, .Lforward_write_fault)
        _ASM_EXTABLE(84b, .Lforward_write_fault)
        _ASM_EXTABLE(85b, .Lforward_write_fault)
        _ASM_EXTABLE(86b, .Lforward_write_fault)
        _ASM_EXTABLE(87b, .Lforward_write_fault)
        _ASM_EXTABLE(88b, .Lforward_read_fault)
        _ASM_EXTABLE(89b, .Lforward_read_fault)
        _ASM_EXTABLE(90b, .Lforward_write_fault)
        _ASM_EXTABLE(91b, .Lforward_write_fault)
        _ASM_EXTABLE(92b, .Lforward_write_fault)
        _ASM_EXTABLE(93b, .Lforward_read_fault)
        _ASM_EXTABLE(94b, .Lforward_read_fault)
        _ASM_EXTABLE(95b, .Lforward_write_fault)
        _ASM_EXTABLE(96b, .Lforward_write_fault)
        _ASM_EXTABLE(97b, .Lforward_read_fault)
        _ASM_EXTABLE(98b, .Lforward_read_fault)
        _ASM_EXTABLE(99b, .Lforward_write_fault)
        _ASM_EXTABLE(100b, .Lforward_write_fault)
        _ASM_EXTABLE(101b, .Lforward_write_fault)
        _ASM_EXTABLE(102b, .Lforward_read_fault)
        _ASM_EXTABLE(103b, .Lforward_read_fault)
        _ASM_EXTABLE(104b, .Lforward_write_fault)
        _ASM_EXTABLE(105b, .Lforward_write_fault)
        _ASM_EXTABLE(106b, .Lforward_read_fault)
        _ASM_EXTABLE(107b, .Lforward_write_fault)

        /* Exception table for backward copy */
        _ASM_EXTABLE(108b, .Lcopy_backward_fault_handler)
        _ASM_EXTABLE(109b, .Lbackward_read_fault)
        _ASM_EXTABLE(110b, .Lbackward_read_fault)
        _ASM_EXTABLE(111b, .Lbackward_read_fault)
        _ASM_EXTABLE(112b, .Lbackward_read_fault)
        _ASM_EXTABLE(113b, .Lbackward_read_fault)
        _ASM_EXTABLE(114b, .Lbackward_read_fault)
        _ASM_EXTABLE(115b, .Lbackward_read_fault)
        _ASM_EXTABLE(116b, .Lbackward_read_fault)
        _ASM_EXTABLE(117b, .Lbackward_write_fault)
        _ASM_EXTABLE(118b, .Lbackward_write_fault)
        _ASM_EXTABLE(119b, .Lbackward_write_fault)
        _ASM_EXTABLE(120b, .Lbackward_write_fault)
        _ASM_EXTABLE(121b, .Lbackward_write_fault)
        _ASM_EXTABLE(122b, .Lbackward_write_fault)
        _ASM_EXTABLE(123b, .Lbackward_write_fault)
        _ASM_EXTABLE(124b, .Lbackward_write_fault)
        _ASM_EXTABLE(125b, .Lcopy_backward_fault_handler)

SYM_FUNC_END(memcpy_orig)
