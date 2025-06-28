/* Copyright (C) 2012-2025 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <https://www.gnu.org/licenses/>.  */

#include <sysdep.h>
#include <shlib-compat.h>

        .p2align 6                      /*  64-byte cache-line       */
ENTRY(__pthread_spin_lock)
        xor     %eax, %eax              /* return value 0            */

.L_try_acquire:
        movl    0(%rdi), %edx           /* optimistic read           */
        test    %edx, %edx              /* <=0 ⇒ locked              */
        jle     .L_wait

        /* — attempt to take the lock — */
        LOCK
        decl    0(%rdi)                 /* 1 → 0 : success           */
        jne     .L_wait                 /* someone beat us           */
        ret                             /* fast-path acquired        */

/* ------------------------------------------------------------------ */
/* Contended slow path                                                 */
/* ------------------------------------------------------------------ */
        .p2align 4
.L_wait:
        mov     $1, %ecx                /* initial pause count       */

.L_pause_loop:
        rep nop                         /* architectural “pause”     */
        dec     %ecx
        jnz     .L_pause_loop           /* burn ECX cycles           */

        /* double (capped @256) for next round                         */
        lea     (%rcx,%rcx), %ecx
        cmp     $256, %ecx
        jbe     .L_check
        mov     $256, %ecx

.L_check:
        cmpl    %eax, 0(%rdi)           /* is lock positive (=free)? */
        jle     .L_wait

        /* — try again — */
        LOCK
        decl    0(%rdi)
        jne     .L_wait
        ret
END(__pthread_spin_lock)

/* Symbol versioning unchanged -------------------------------------- */
versioned_symbol (libc, __pthread_spin_lock, pthread_spin_lock, GLIBC_2_34)

#if OTHER_SHLIB_COMPAT (libpthread, GLIBC_2_2, GLIBC_2_34)
compat_symbol (libpthread, __pthread_spin_lock, pthread_spin_lock, GLIBC_2_2)
#endif
