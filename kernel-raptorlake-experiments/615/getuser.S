/* SPDX-License-Identifier: GPL-2.0 */
/*
 * __get_user_*  –  fast, fault-tolerant user-memory loads
 *
 *  Input :  %rax = user pointer
 *  Output:  %rax = 0 / –EFAULT
 *           %rdx = zero-extended value
 *           (on 32-bit + __get_user_8, high dword in %ecx)
 *
 *  Registers preserved aside from the ones above.
 *
 *  Tuning notes for Raptor-Lake:
 *   • 32-byte i-cache alignment avoids split fetch on entry.
 *   • CMOV for range check is fine (no µ-op penalty, all ports).
 *   • Add LFENCE (via alternatives) for speculative attack hardening,
 *     but only when CPU advertises LFENCE_RDTSC.
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

#define ASM_BARRIER_NOSPEC  ALTERNATIVE "", "lfence", X86_FEATURE_LFENCE_RDTSC

/* -------------------------------------------------------------- */
/*  Range-check macro (avoids speculation past USER_PTR_MAX)      */
/* -------------------------------------------------------------- */
.macro check_range size:req
.if IS_ENABLED(CONFIG_X86_64)
        RUNTIME_CONST_PTR USER_PTR_MAX, rdx   /* rd  = USER_PTR_MAX */
        cmpq    %rdx, %rax
        cmovaq  %rdx, %rax                    /* if >MAX → fault addr */
.else
        cmp     $TASK_SIZE_MAX-\size+1, %eax
        jae     .Lbad_get_user
        sbbb    %edx, %edx                    /* array_index_mask_nospec */
        andl    %edx, %eax
.endif
.endm

/* -------------------------------------------------------------- */
/*  Fault-tolerant memory op helper                               */
/* -------------------------------------------------------------- */
.macro UACCESS op src dst
1:      \op \src, \dst
        _ASM_EXTABLE_UA(1b, __get_user_handle_exception)
.endm

/* ==============================================================
 *  1-byte get_user
 * ============================================================== */
        .p2align 5
SYM_FUNC_START(__get_user_1)
        ANNOTATE_NOENDBR
        check_range size=1
        ASM_STAC
        UACCESS  movzbl (%_ASM_AX), %edx
        xorl     %eax, %eax
        ASM_CLAC
        RET
SYM_FUNC_END(__get_user_1)
EXPORT_SYMBOL(__get_user_1)

/* ==============================================================
 *  2-byte get_user
 * ============================================================== */
        .p2align 5
SYM_FUNC_START(__get_user_2)
        ANNOTATE_NOENDBR
        check_range size=2
        ASM_STAC
        UACCESS  movzwl (%_ASM_AX), %edx
        xorl     %eax, %eax
        ASM_CLAC
        RET
SYM_FUNC_END(__get_user_2)
EXPORT_SYMBOL(__get_user_2)

/* ==============================================================
 *  4-byte get_user
 * ============================================================== */
        .p2align 5
SYM_FUNC_START(__get_user_4)
        ANNOTATE_NOENDBR
        check_range size=4
        ASM_STAC
        UACCESS  movl (%_ASM_AX), %edx
        xorl     %eax, %eax
        ASM_CLAC
        RET
SYM_FUNC_END(__get_user_4)
EXPORT_SYMBOL(__get_user_4)

/* ==============================================================
 *  8-byte get_user
 * ============================================================== */
        .p2align 5
SYM_FUNC_START(__get_user_8)
        ANNOTATE_NOENDBR
.ifndef CONFIG_X86_64
        xorl     %ecx, %ecx
.endif
        check_range size=8
        ASM_STAC
.if IS_ENABLED(CONFIG_X86_64)
        UACCESS  movq (%_ASM_AX), %rdx
#else
        UACCESS  movl (%_ASM_AX), %edx
        UACCESS  movl 4(%_ASM_AX), %ecx
#endif
        xorl     %eax, %eax
        ASM_CLAC
        RET
SYM_FUNC_END(__get_user_8)
EXPORT_SYMBOL(__get_user_8)

/* =================================================================
 *  *_nocheck variants – skip range bounds, keep speculation barrier
 * ============================================================== */
#define GEN_GET_USER_NOCHECK(size,suffix,load)            \
        .p2align 5                                        ;\
SYM_FUNC_START(__get_user_nocheck_##suffix)               ;\
        ANNOTATE_NOENDBR                                  ;\
        ASM_STAC                                          ;\
        ASM_BARRIER_NOSPEC                                ;\
        UACCESS  load (%_ASM_AX), %edx                    ;\
        xorl     %eax, %eax                               ;\
        ASM_CLAC                                          ;\
        RET                                              ;\
SYM_FUNC_END(__get_user_nocheck_##suffix)                 ;\
EXPORT_SYMBOL(__get_user_nocheck_##suffix)

GEN_GET_USER_NOCHECK(1, 1,  movzbl)
GEN_GET_USER_NOCHECK(2, 2,  movzwl)
GEN_GET_USER_NOCHECK(4, 4,  movl)

        .p2align 5
SYM_FUNC_START(__get_user_nocheck_8)
        ANNOTATE_NOENDBR
        ASM_STAC
        ASM_BARRIER_NOSPEC
.if IS_ENABLED(CONFIG_X86_64)
        UACCESS  movq (%_ASM_AX), %rdx
#else
        xorl     %ecx, %ecx
        UACCESS  movl (%_ASM_AX), %edx
        UACCESS  movl 4(%_ASM_AX), %ecx
#endif
        xorl     %eax, %eax
        ASM_CLAC
        RET
SYM_FUNC_END(__get_user_nocheck_8)
EXPORT_SYMBOL(__get_user_nocheck_8)

/* ==============================================================
 *  Exception handler – zero value, return -EFAULT
 * ============================================================== */
SYM_CODE_START_LOCAL(__get_user_handle_exception)
        ASM_CLAC
.Lbad_get_user:
        xorl     %edx, %edx
        movl     $(-EFAULT), %eax
        RET
SYM_CODE_END(__get_user_handle_exception)
