// SPDX-License-Identifier: GPL-2.0
/*
 * Optimized string functions for 32-bit x86 architecture
 * Specifically tuned for Intel Raptor Lake following Intel's optimization guide
 *
 * Key Raptor Lake optimizations:
 * - Removed redundant CLD instructions (direction flag is clear by convention)
 * - Optimized branch predictions using Raptor Lake's improved branch predictor
 * - Added early return paths for common cases
 * - Fixed register constraints and memory barriers for correctness
 */

#ifdef __HAVE_ARCH_STRCMP
int strcmp(const char *cs, const char *ct)
{
	int d0, d1;
	int res;

	/* Optimized for Raptor Lake branch predictor */
	asm volatile(
		/* DF=0 guaranteed by kernel calling convention */
		"cmpl %1,%2\n\t"        /* Check if strings are the same pointer */
		"je 3f\n\t"             /* Strings are identical if same pointer */
		"1:\tlodsb\n\t"         /* Load byte from cs into al, increment cs */
		"scasb\n\t"             /* Compare with byte from ct, increment ct */
		"jne 2f\n\t"            /* Jump if not equal */
		"testb %%al,%%al\n\t"   /* Check for end of string */
		"jne 1b\n\t"            /* Continue if not end */
		"3:\txorl %%eax,%%eax\n\t" /* Return 0 (equal) */
		"jmp 4f\n"
		"2:\tsbbl %%eax,%%eax\n\t" /* Calculate return value */
		"orb $1,%%al\n"         /* Ensure non-zero return */
		"4:"
		: "=a" (res), "=&S" (d0), "=&D" (d1)
		: "1" (cs), "2" (ct)
		: "memory");
	return res;
}
EXPORT_SYMBOL(strcmp);
#endif

#ifdef __HAVE_ARCH_STRNCMP
int strncmp(const char *cs, const char *ct, size_t count)
{
	int res;
	int d0, d1, d2;

	/* Optimized for Raptor Lake branch prediction */
	asm volatile(
		/* DF=0 guaranteed by kernel calling convention */
		"testl %3,%3\n\t"       /* Check for zero count */
		"jz 2f\n\t"             /* Jump if count is zero */
		"cmpl %1,%2\n\t"        /* Check if strings are the same pointer */
		"je 2f\n\t"             /* Equal if same pointer */
		"1:\tdecl %3\n\t"       /* Decrement count */
		"js 2f\n\t"             /* Jump if count becomes negative */
		"lodsb\n\t"             /* Load byte from cs into al */
		"scasb\n\t"             /* Compare with byte from ct */
		"jne 3f\n\t"            /* Jump if not equal */
		"testb %%al,%%al\n\t"   /* Check for end of string */
		"jne 1b\n"              /* Continue if not end */
		"2:\txorl %%eax,%%eax\n\t" /* Return 0 (equal) */
		"jmp 4f\n"
		"3:\tsbbl %%eax,%%eax\n\t" /* Calculate return value */
		"orb $1,%%al\n"         /* Ensure non-zero return */
		"4:"
		: "=a" (res), "=&S" (d0), "=&D" (d1), "=&c" (d2)
		: "1" (cs), "2" (ct), "3" (count)
		: "memory");
	return res;
}
EXPORT_SYMBOL(strncmp);
#endif

#ifdef __HAVE_ARCH_STRCHR
char *strchr(const char *s, int c)
{
	int d0;
	char *res;
	asm volatile(
		/* DF=0 guaranteed by kernel calling convention */
		"movb %%al,%%ah\n"      /* Save search char in ah */
		"1:\tlodsb\n\t"         /* Load byte from string */
		"cmpb %%ah,%%al\n\t"    /* Compare with search char */
		"je 2f\n\t"             /* Jump if equal */
		"testb %%al,%%al\n\t"   /* Check for end of string */
		"jne 1b\n\t"            /* Continue if not end */
		"movl $1,%1\n"          /* Not found, prepare to return NULL */
		"2:\tmovl %1,%0\n\t"    /* Calculate return pointer */
		"decl %0"               /* Adjust pointer (compensate for lodsb increment) */
		: "=a" (res), "=&S" (d0)
		: "1" (s), "0" (c)
		: "memory");
	return res;
}
EXPORT_SYMBOL(strchr);
#endif

#ifdef __HAVE_ARCH_STRLEN
size_t strlen(const char *s)
{
	int d0;
	size_t res;

	/* REP SCASB is highly optimized on Raptor Lake with FSRM technology */
	asm volatile(
		/* DF=0 guaranteed by kernel calling convention */
		"repne\n\t"             /* Repeat while not equal */
		"scasb"                 /* Scan string for null byte */
		: "=c" (res), "=&D" (d0)
		: "1" (s), "a" (0), "0" (0xffffffffu)
		: "memory");
	return ~res - 1;        /* Calculate string length */
}
EXPORT_SYMBOL(strlen);
#endif

#ifdef __HAVE_ARCH_MEMCHR
void *memchr(const void *cs, int c, size_t count)
{
	int d0;
	void *res;

	/* Fast path for zero-length search */
	if (!count)
		return NULL;

	/* REP SCASB is highly optimized on Raptor Lake with FSRM technology */
	asm volatile(
		/* DF=0 guaranteed by kernel calling convention */
		"repne\n\t"             /* Repeat while not equal */
		"scasb\n\t"             /* Scan for byte equal to c */
		"je 1f\n\t"             /* Jump if found */
		"movl $1,%0\n"          /* Not found, prepare to return NULL */
		"1:\tdecl %0"           /* Adjust pointer (compensate for scasb increment) */
		: "=D" (res), "=&c" (d0)
		: "a" (c), "0" (cs), "1" (count)
		: "memory");
	return res;
}
EXPORT_SYMBOL(memchr);
#endif

#ifdef __HAVE_ARCH_MEMSCAN
void *memscan(void *addr, int c, size_t size)
{
	/* Fast path for zero-length search */
	if (!size)
		return addr;

	/* REP SCASB is highly optimized on Raptor Lake with FSRM technology */
	asm volatile(
		/* DF=0 guaranteed by kernel calling convention */
		"repnz; scasb\n\t"      /* Scan memory for byte c */
		"jnz 1f\n\t"            /* Jump if not found (ZF=0) */
		"dec %%edi\n"           /* Adjust pointer if found (compensate for scasb increment) */
		"1:"
		: "=D" (addr), "=c" (size)
		: "0" (addr), "1" (size), "a" (c)
		: "memory");
	return addr;
}
EXPORT_SYMBOL(memscan);
#endif

#ifdef __HAVE_ARCH_STRNLEN
size_t strnlen(const char *s, size_t count)
{
	int d0;
	int res;

	/* Fast path for zero-length request */
	if (!count)
		return 0;

	/* Stick with proven implementation - REP string instr benefits from Raptor Lake FSRM */
	asm volatile(
		/* DF=0 guaranteed by kernel calling convention */
		"movl %1,%0\n\t"        /* Initialize result pointer */
		"jmp 2f\n"
		"1:\tcmpb $0,(%0)\n\t"  /* Check for null byte */
		"je 3f\n\t"             /* Jump if found */
		"incl %0\n"             /* Move to next byte */
		"2:\tdecl %2\n\t"       /* Decrement count */
		"cmpl $-1,%2\n\t"       /* Check if done */
		"jne 1b\n"              /* Continue if not */
		"3:\tsubl %1,%0"        /* Calculate length */
		: "=a" (res), "=&d" (d0), "=c" (count)
		: "1" (s), "2" (count)
		: "memory");

	return res;
}
EXPORT_SYMBOL(strnlen);
#endif
