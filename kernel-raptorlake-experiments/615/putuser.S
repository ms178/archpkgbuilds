/* SPDX-License-Identifier: GPL-2.0 */
/*
 * __put_user_*  —  fault-tolerant user memory stores
 *
 *  IN  : %eax[:%edx]  = value
 *        %ecx/%rcx    = user pointer
 *  OUT : %ecx         = 0 / –EFAULT
 *
 *  Only %ebx is clobbered as scratch (matches inline-asm contract).
 */

#include <linux/export.h>
#include <linux/linkage.h>
#include <linux/objtool.h>
#include <asm/thread_info.h>
#include <asm/errno.h>
#include <asm/asm.h>
#include <asm/smap.h>
#include <asm/cpufeatures.h>

/* ------------------------------------------------------------------ */
/*  Speculation barrier macro – only emitted when CPU advertises it   */
/* ------------------------------------------------------------------ */
#define ASM_BARRIER_NOSPEC  ALTERNATIVE "", "lfence", X86_FEATURE_LFENCE_RDTSC

/* ------------------------------------------------------------------ */
/*  Range check helper                                                */
/* ------------------------------------------------------------------ */
.macro check_range size:req
.if IS_ENABLED(CONFIG_X86_64)
        mov     %rcx, %rbx       /* rbx = pointer sign bits   */
        sarq    $63,  %rbx
        orq     %rbx, %rcx       /* canonicalise; >MAX → -1   */
.else
        cmp     $TASK_SIZE_MAX-\size+1, %ecx
        jae     .Lbad_put_user
.endif
.endm

/* ------------------------------------------------------------------ */
/*  1-byte store                                                      */
/* ------------------------------------------------------------------ */
        .p2align 5
SYM_FUNC_START(__put_user_1)
        ANNOTATE_NOENDBR
        check_range size=1
        ASM_STAC
1:      movb    %al, (%_ASM_CX)
        xorl    %ecx, %ecx
        ASM_CLAC
        RET
SYM_FUNC_END(__put_user_1)
EXPORT_SYMBOL(__put_user_1)

/* nocheck --------------------------------------------------------- */
        .p2align 5
SYM_FUNC_START(__put_user_nocheck_1)
        ANNOTATE_NOENDBR
        ASM_BARRIER_NOSPEC
        ASM_STAC
2:      movb    %al,(%_ASM_CX)
        xorl    %ecx,%ecx
        ASM_CLAC
        RET
SYM_FUNC_END(__put_user_nocheck_1)
EXPORT_SYMBOL(__put_user_nocheck_1)

/* ------------------------------------------------------------------ */
/*  2-byte store                                                      */
/* ------------------------------------------------------------------ */
        .p2align 5
SYM_FUNC_START(__put_user_2)
        ANNOTATE_NOENDBR
        check_range size=2
        ASM_STAC
3:      movw    %ax, (%_ASM_CX)
        xorl    %ecx, %ecx
        ASM_CLAC
        RET
SYM_FUNC_END(__put_user_2)
EXPORT_SYMBOL(__put_user_2)

/* nocheck --------------------------------------------------------- */
        .p2align 5
SYM_FUNC_START(__put_user_nocheck_2)
        ANNOTATE_NOENDBR
        ASM_STAC
        ASM_BARRIER_NOSPEC
4:      movw    %ax, (%_ASM_CX)
        xorl    %ecx, %ecx
        ASM_CLAC
        RET
SYM_FUNC_END(__put_user_nocheck_2)
EXPORT_SYMBOL(__put_user_nocheck_2)

/* ------------------------------------------------------------------ */
/*  4-byte store                                                      */
/* ------------------------------------------------------------------ */
        .p2align 5
SYM_FUNC_START(__put_user_4)
        ANNOTATE_NOENDBR
        check_range size=4
        ASM_STAC
5:      movl    %eax, (%_ASM_CX)
        xorl    %ecx, %ecx
        ASM_CLAC
        RET
SYM_FUNC_END(__put_user_4)
EXPORT_SYMBOL(__put_user_4)

/* nocheck --------------------------------------------------------- */
        .p2align 5
SYM_FUNC_START(__put_user_nocheck_4)
        ANNOTATE_NOENDBR
        ASM_STAC
        ASM_BARRIER_NOSPEC
6:      movl    %eax, (%_ASM_CX)
        xorl    %ecx, %ecx
        ASM_CLAC
        RET
SYM_FUNC_END(__put_user_nocheck_4)
EXPORT_SYMBOL(__put_user_nocheck_4)

/* ------------------------------------------------------------------ */
/*  8-byte store (64-bit only)                                        */
/* ------------------------------------------------------------------ */
        .p2align 5
SYM_FUNC_START(__put_user_8)
        ANNOTATE_NOENDBR
        check_range size=8
        ASM_STAC
7:      movq    %_ASM_AX, (%_ASM_CX)
#ifdef CONFIG_X86_32
8:      movl    %edx, 4(%_ASM_CX)
#endif
        xorl    %ecx, %ecx
        ASM_CLAC
        RET
SYM_FUNC_END(__put_user_8)
EXPORT_SYMBOL(__put_user_8)

/* nocheck --------------------------------------------------------- */
        .p2align 5
SYM_FUNC_START(__put_user_nocheck_8)
        ANNOTATE_NOENDBR
        ASM_STAC
        ASM_BARRIER_NOSPEC
9:      movq    %_ASM_AX, (%_ASM_CX)
#ifdef CONFIG_X86_32
10:     movl    %edx, 4(%_ASM_CX)
#endif
        xorl    %ecx, %ecx
        ASM_CLAC
        RET
SYM_FUNC_END(__put_user_nocheck_8)
EXPORT_SYMBOL(__put_user_nocheck_8)

/* ------------------------------------------------------------------ */
/*  Fault handler                                                     */
/* ------------------------------------------------------------------ */
SYM_CODE_START_LOCAL(__put_user_handle_exception)
        ASM_CLAC
.Lbad_put_user:
        movl    $-EFAULT, %ecx
        RET
SYM_CODE_END(__put_user_handle_exception)

/* extable entries – keep byte-exact addresses -------------------- */
        _ASM_EXTABLE_UA(1b,  __put_user_handle_exception)
        _ASM_EXTABLE_UA(2b,  __put_user_handle_exception)
        _ASM_EXTABLE_UA(3b,  __put_user_handle_exception)
        _ASM_EXTABLE_UA(4b,  __put_user_handle_exception)
        _ASM_EXTABLE_UA(5b,  __put_user_handle_exception)
        _ASM_EXTABLE_UA(6b,  __put_user_handle_exception)
        _ASM_EXTABLE_UA(7b,  __put_user_handle_exception)
        _ASM_EXTABLE_UA(9b,  __put_user_handle_exception)
#ifdef CONFIG_X86_32
        _ASM_EXTABLE_UA(8b,  __put_user_handle_exception)
        _ASM_EXTABLE_UA(10b, __put_user_handle_exception)
#endif
