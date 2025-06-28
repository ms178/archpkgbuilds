/* __pthread_spin_lock – TTAS with capped exponential back-off
   for x86-64 (Intel Raptor-Lake / AMD Zen3+).
   Copyright (C) 2003-2025 Free Software Foundation, Inc.
   SPDX-License-Identifier: LGPL-2.1-or-later                               */

#include <sysdep.h>
#include <shlib-compat.h>

        .p2align 6
ENTRY(__pthread_spin_lock)
        xor     %eax, %eax                /* success return value is always 0 */

.L_try_acquire:
        /* Fast path: atomically attempt to acquire the lock. */
        LOCK
        decl    (%rdi)                    /* 1→0 on the lock value grabs it. */
        jne     .L_contended_path
        ret

        .p2align 5                        /* Align hot loop for LSD/uop-cache. */
.L_contended_path:
        mov     $1, %edx                  /* Initial back-off: 1 pause cycle. */

.L_spin_loop:
        /* Test-and-Test-and-Set: cheap read before expensive atomic write. */
        cmpl    %eax, (%rdi)              /* Is lock value > 0 (free)? */
        jg      .L_try_acquire            /* Looks free -> retry the fast path. */

        /* Inner PAUSE loop: yields CPU resources to sibling hyper-thread
           and saves power while waiting for the lock. */
        mov     %edx, %ecx
.L_pause_loop:
        rep nop                           /* This is the PAUSE instruction. */
        dec     %ecx
        jne     .L_pause_loop

        /* Exponentially increase back-off duration for the next spin. */
        add     %edx, %edx                /* back-off *= 2 */
        cmp     $256, %edx                /* Cap at 256 to bound latency. */
        jbe     .L_spin_loop
        mov     $256, %edx
        jmp     .L_spin_loop
END(__pthread_spin_lock)

versioned_symbol (libc, __pthread_spin_lock, pthread_spin_lock, GLIBC_2_34)
#if OTHER_SHLIB_COMPAT (libpthread, GLIBC_2_2, GLIBC_2_34)
compat_symbol (libpthread, __pthread_spin_lock, pthread_spin_lock, GLIBC_2_2)
#endif
