/*  blake3_hash_many_avx2_stub.S
 *  A minimal veneer that preserves ABI / symbol visibility for
 *  _blake3_hash_many_avx2  /  blake3_hash_many_avx2
 *  and forwards unconditionally to the production-quality C
 *  implementation `blake3_hash_many_avx2_c` (compiled from
 *  blake3_avx2.c).
 */

#include "mesa_blake3_visibility.h"

#if defined(__ELF__) && defined(__linux__)
/* mark stack as non-executable */
.section .note.GNU-stack,"",%progbits
#endif

/* ------------------------------------------------------------------ */
/*  IBT / CET support (GNU/LLVM define _CET_ENDBR to `endbr64`)       */
#if defined(__ELF__) && defined(__CET__) && defined(__has_include)
#  if __has_include(<cet.h>)
#    include <cet.h>
#  endif
#endif
#ifndef _CET_ENDBR
#  define _CET_ENDBR                /* expands to nothing if CET absent */
#endif

.intel_syntax noprefix

/* ------------------------------------------------------------------ */
/*  External symbol implemented in C                                  */
.extern blake3_hash_many_avx2_c

/*  Public + hidden aliases expected by upper layers                  */
HIDDEN _blake3_hash_many_avx2
HIDDEN blake3_hash_many_avx2
.global _blake3_hash_many_avx2
.global blake3_hash_many_avx2

/* ------------------------------------------------------------------ */
#ifdef __APPLE__
.text
#else
.section .text
#endif
.p2align 4

/*  Both decorated and undecorated names point to the same stub.      */
_blake3_hash_many_avx2:
blake3_hash_many_avx2:
    _CET_ENDBR                    /* emits ENDBR64 if CET is enabled  */
    jmp blake3_hash_many_avx2_c   /* tail-call into the C implementation */

#if defined(__ELF__)
/* Emit a size directive for ELF; harmless elsewhere but wrapped for
   safety because Mach-O and COFF tool-chains reject `.size`.          */
.size blake3_hash_many_avx2, .-blake3_hash_many_avx2
#endif
