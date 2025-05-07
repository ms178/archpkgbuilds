// SPDX-License-Identifier: GPL-2.0
/*
 * Most of the string-functions are rather heavily hand-optimized,
 * see especially strsep,strstr,str[c]spn. They should work, but are not
 * very easy to understand. Everything is done entirely within the register
 * set, making the functions fast and clean. String instructions have been
 * used through-out, making for "slightly" unclear code :-)
 *
 * AK: On P4 and K7 using non string instruction implementations might be faster
 * for large memory blocks. But most of them are unlikely to be used on large
 * strings.
 *
 * Raptor Lake Optimizations:
 * - strnlen rewritten to use repne scasb.
 * - Early exit for count=0 in strncpy, strncmp.
 * - Early exit for pointer equality in strcmp, strncmp.
 * - Clarified strchr NULL return.
 * - DF=0 is assumed by C ABI and kernel. No CLD needed.
 */

#define __NO_FORTIFY // Keep this as it was in the original
#include <linux/string.h>
#include <linux/export.h>

#ifdef __HAVE_ARCH_STRCPY
char *strcpy(char *dest, const char *src)
{
	int d0, d1, d2;
	asm volatile(
		"1:\tlodsb\n\t"
		"stosb\n\t"
		"testb %%al,%%al\n\t"
		"jne 1b"
		: "=&S" (d0), "=&D" (d1), "=&a" (d2)
		: "0" (src), "1" (dest) : "memory");
	return dest;
}
EXPORT_SYMBOL(strcpy);
#endif

#ifdef __HAVE_ARCH_STRNCPY
char *strncpy(char *dest, const char *src, size_t count)
{
	int d0, d1, d2, d3;
	asm volatile(
		"testl %2, %2\n\t" /* if (count == 0) */
		"jz 2f\n\t"      /*   goto end (label 2); */
		"1:\tdecl %2\n\t" /* Use original count in %ecx (%2) as loop counter */
		"js 2f\n\t"      /* If count becomes <0 (was 0 initially, or exhausted) */
		"lodsb\n\t"
		"stosb\n\t"
		"testb %%al,%%al\n\t"
		"jne 1b\n\t"     /* Loop if char copied was not NUL */
		/* Fall through if NUL was copied. %2 has remaining count for padding. */
		/* AL is 0. %ecx (%2) has remaining padding count. */
		"rep stosb\n\t"  /* Pad with NULs. rep uses %ecx. If %ecx is 0, no-op. */
		"2:"
		: "=&S" (d0), "=&D" (d1), "=&c" (d2), "=&a" (d3)
		: "0" (src), "1" (dest), "2" (count) : "memory");
	return dest;
}
EXPORT_SYMBOL(strncpy);
#endif

#ifdef __HAVE_ARCH_STRCAT
char *strcat(char *dest, const char *src)
{
	int d0, d1, d2, d3;
	asm volatile(
		"repne scasb\n\t" /* Find NUL in dest. EDI points after NUL. ECX is junk. */
		"decl %1\n\t"   /* EDI now points to the NUL in dest. */
		"1:\tlodsb\n\t"   /* Copy src */
		"stosb\n\t"
		"testb %%al,%%al\n\t"
		"jne 1b"
		: "=&S" (d0), "=&D" (d1), "=&a" (d2), "=&c" (d3)
		: "0" (src), "1" (dest), "2" (0), "3" (0xffffffffu) /* dest->EDI, src->ESI, AL=0, ECX=-1 for scasb */
		: "memory");
	return dest;
}
EXPORT_SYMBOL(strcat);
#endif

#ifdef __HAVE_ARCH_STRNCAT
char *strncat(char *dest, const char *src, size_t count)
{
	int d0, d1, d2, d3;
	asm volatile(
		"repne scasb\n\t"       /* Find NUL in dest. EDI points after NUL. Original %ecx is junk. */
		"decl %1\n\t"         /* EDI now points to the NUL in dest. */
		"movl %8, %3\n\t"       /* Load original 'strncat_count' into loop counter %ecx (%3). Constraint "g"(count) is %8. */
		"testl %3, %3\n\t"      /* If strncat_count == 0 */
		"jz 2f\n\t"           /*   then skip copy loop, just NUL terminate. */
		"1:\tdecl %3\n\t"     /* Decrement remaining 'strncat_count' for copy */
		"js 2f\n\t"           /* If count for copy exhausted */
		"lodsb\n\t"
		"stosb\n\t"
		"testb %%al,%%al\n\t"
		"jne 1b\n\t"          /* Loop if char copied was not NUL */
		/* Fall through if NUL copied (src was shorter than strncat_count) or count exhausted */
		"2:\txorl %2, %2\n\t"   /* AL = 0 */
		"stosb"                 /* Store NUL terminator. */
		: "=&S" (d0), "=&D" (d1), "=&a" (d2), "=&c" (d3)
		/* Inputs for scasb: dest->EDI (%1), AL=0 (%2), ECX=-1 (%3 clobbered after use of %8) */
		/* Inputs for copy: src->ESI (%0) */
		: "0" (src), "1" (dest), "2" (0), "3" (0xffffffffu), "g" (count)
		: "memory");
	return dest;
}
EXPORT_SYMBOL(strncat);
#endif

#ifdef __HAVE_ARCH_STRCMP
int strcmp(const char *cs, const char *ct)
{
	int d0, d1;
	int res;
	asm volatile(
		"cmp %1, %2\n\t"        /* if (cs == ct) */
		"je .Lstrcmp_equal_%= \n\t" /* Use %= for unique local label */
		"1:\tlodsb\n\t"
		"scasb\n\t"
		"jne .Lstrcmp_notequal_%= \n\t"
		"testb %%al,%%al\n\t"
		"jne 1b\n\t"
		".Lstrcmp_equal_%= :\n\t"
		"xorl %%eax,%%eax\n\t"
		"jmp .Lstrcmp_done_%= \n\t"
		".Lstrcmp_notequal_%= :\n\t"
		"sbbl %%eax,%%eax\n\t"
		"orb $1,%%al\n\t"
		".Lstrcmp_done_%= :"
		: "=a" (res), "=&S" (d0), "=&D" (d1)
		: "1" (cs), "2" (ct)
		: "memory", "cc");
	return res;
}
EXPORT_SYMBOL(strcmp);
#endif

#ifdef __HAVE_ARCH_STRNCMP
int strncmp(const char *cs, const char *ct, size_t count)
{
	int res;
	int d0, d1, d2;
	asm volatile(
		"testl %3, %3\n\t"      /* if (count == 0), %3 is original count from arg */
		"jz .Lstrncmp_equal_%= \n\t"
		"cmp %1, %2\n\t"        /* if (cs == ct) */
		"je .Lstrncmp_equal_%= \n\t"
		/* %3 (original count) gets moved to %ecx by constraint "3" */
		"1:\tdecl %2\n\t"       /* Use %ecx (%2) as loop counter, was original count */
		"js .Lstrncmp_equal_%= \n\t" /* If count exhausted */
		"lodsb\n\t"
		"scasb\n\t"
		"jne .Lstrncmp_notequal_%= \n\t"
		"testb %%al,%%al\n\t"
		"jne 1b\n\t"            /* If not NUL, continue loop (if count allows) */
		/* Fall through if NUL found, strings are equal up to NUL or count */
		".Lstrncmp_equal_%= :\n\t"
		"xorl %%eax,%%eax\n\t"
		"jmp .Lstrncmp_done_%= \n\t"
		".Lstrncmp_notequal_%= :\n\t"
		"sbbl %%eax,%%eax\n\t"
		"orb $1,%%al\n\t"
		".Lstrncmp_done_%= :"
		: "=a" (res), "=&S" (d0), "=&D" (d1), "=&c" (d2)
		: "1" (cs), "2" (ct), "3" (count) /* count maps to %ecx (%d2 for clobber, %3 for input) */
		: "memory", "cc");
	return res;
}
EXPORT_SYMBOL(strncmp);
#endif

#ifdef __HAVE_ARCH_STRCHR
char *strchr(const char *s, int c)
{
	int d0; /* for S clobber */
	char *res; /* for a output */
	/* Original: c in %al (from "0" constraint). s in %esi (from "1" constraint). */
	asm volatile(
		"movb %%al, %%ah\n\t" /* Save char c (in al) to ah for comparison */
		"1:\tlodsb\n\t"       /* al = *s++ */
		"cmpb %%ah, %%al\n\t" /* if (*s == c) */
		"je 2f\n\t"
		"testb %%al, %%al\n\t" /* if (*s == 0) */
		"jne 1b\n\t"
		/* Not found */
		"movl $0, %0\n\t"       /* res = NULL */
		"jmp 3f\n\t"
		"2:\tmovl %1, %0\n\t"   /* Found: res = current s */
		"decl %0\n\t"       /* lodsb incremented s, so res needs to point to the char itself */
		"3:"
		: "=a" (res), "=&S" (d0)
		: "1" (s), "0" (c)
		: "memory", "ah", "cc"); /* ah is used and clobbered */
	return res;
}
EXPORT_SYMBOL(strchr);
#endif

#ifdef __HAVE_ARCH_STRLEN
size_t strlen(const char *s)
{
	int d0;
	size_t res;
	asm volatile(
		"repne scasb" /* Optimized for modern CPUs */
		/* Inputs for scasb: s->EDI (%1), AL=0 (%a), ECX=-1 (%0) */
		: "=c" (res), "=&D" (d0) /* Output: final ecx in res, edi clobbered in d0 */
		: "1" (s), "a" (0), "0" (0xffffffffu)
		: "memory");
	return ~res - 1; /* Correct calculation for length from final ecx */
}
EXPORT_SYMBOL(strlen);
#endif

#ifdef __HAVE_ARCH_MEMCHR
void *memchr(const void *cs, int c, size_t count)
{
	int d0; /* for D clobber */
	void *res; /* for D output */
	/* Original: cs into EDI (constraint "0"), c into AL (constraint "a"), count into ECX (constraint "1") */
	asm volatile(
		"testl %2, %2\n\t"      /* if (count == 0) */
		"jz .Lmemchr_notfound_%= \n\t" /* Note: res is edi, not eax here. */
		"repne scasb\n\t"
		"je .Lmemchr_found_%= \n\t" /* ZF=1 if found */
		".Lmemchr_notfound_%= :\n\t"
		"movl $0, %0\n\t"       /* res (edi) = NULL */
		"jmp .Lmemchr_done_%= \n\t"
		".Lmemchr_found_%= :\n\t"
		"decl %0\n\t"           /* res (edi) points to char */
		".Lmemchr_done_%= :"
		: "=D" (res), "=&c" (d0)
		: "a" (c), "0" (cs), "1" (count)
		: "memory", "cc");
	return res;
}
EXPORT_SYMBOL(memchr);
#endif

#ifdef __HAVE_ARCH_MEMSCAN
void *memscan(void *addr, int c, size_t size)
{
	// Original: addr in EDI (constraint "0"), size in ECX (constraint "1"), c in AL ("a")
	asm volatile(
		"testl %1, %1\n\t"      /* if (size == 0) */
		"jz 1f\n\t"           /*   skip scan, addr is already correct */
		"repnz scasb\n\t"       /* Scan while [edi]!=al (ZF=0) && ecx!=0 */
		/* Stops if [edi]==al (ZF=1) OR ecx becomes 0 */
		"jnz 1f\n\t"            /* If ZF=0 (ecx became 0, last char was not c), addr is edi (end of scan) */
		"dec %%edi\n\t"         /* If ZF=1 (char c found), edi points after c, so dec to point to c */
		"1:"
		: "=D" (addr), "=c" (size)
		: "0" (addr), "1" (size), "a" (c)
		: "memory", "cc");
	return addr;
}
EXPORT_SYMBOL(memscan);
#endif

#ifdef __HAVE_ARCH_STRNLEN
size_t strnlen(const char *s, size_t count)
{
	size_t result;
	// Inputs from C: s on stack/reg, count on stack/reg
	// For inline asm: map s to %edi, count to %ecx
	asm volatile(
		"testl %2, %2\n\t"          // If count == 0
		"jz .Lstrnlen_len_is_zero_%= \n\t"  // then length is 0

		"movl %1, %%edi\n\t"        // edi = s
		"movl %2, %%ecx\n\t"        // ecx = count (this will be the REP counter)
	"xorl %%eax, %%eax\n\t"     // al = 0 (char to search for NUL)

	"repne scasb\n\t"           // edi will point one byte *after* NUL if found,
	// or s + count if NUL not found within 'count' bytes.
	// ecx will be 0 if 'count' exhausted without finding NUL,
	// or count_remaining_after_NUL if NUL found.

	/* Calculate length: original_count - final_ecx gives bytes processed by repne. */
	/* This value is either 'count' (if NUL not found) or 'length_of_string_up_to_NUL + 1'. */
	"movl %2, %%eax\n\t"        // eax = original count
	"subl %%ecx, %%eax\n\t"     // eax = original_count - final_ecx

	/* Now, eax holds 'count' if NUL wasn't found within 'count' bytes. */
	/* Or, eax holds 'length_up_to_NUL + 1' if NUL was found. */
	/* We need to distinguish these cases. 'repne scasb' sets ZF=1 if the *reason for stopping* was a match. */
	/* It sets ZF based on the last comparison if ECX becomes zero. */

	"jne .Lstrnlen_nul_found_or_limit_reached_%= \n\t" // If ZF=0 after repne, it means ECX became 0 AND last char was not NUL
	// In this case, length is 'count', eax is already 'count'. Good.
	// If ZF=1, NUL was found. eax is 'len+1'.
	/* ZF=1 here means NUL was found (or last char was NUL when count exhausted) */
	"decl %%eax\n\t"            // Length is one less to exclude the NUL byte.
	"jmp .Lstrnlen_done_%= \n\t"

	".Lstrnlen_nul_found_or_limit_reached_%= :\n\t"
	/* This path is taken if repne finished because ECX became 0, AND the last byte compared was NOT NUL. */
	/* In this case, EAX = original_count, which is the correct length. */
	"jmp .Lstrnlen_done_%= \n\t"

	".Lstrnlen_len_is_zero_%= :\n\t"
	"xorl %%eax, %%eax\n\t"     // Result is 0
	".Lstrnlen_done_%= :"
	: "=a" (result)                 // Output: result in %eax
	: "g"(s), "g"(count)            // Inputs: s, count (general constraints allow them anywhere)
	: "%edi", "%ecx", "memory", "cc"); // Clobbers for GCC
	return result;
}
EXPORT_SYMBOL(strnlen);
#endif
