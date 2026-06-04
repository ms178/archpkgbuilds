// SPDX-License-Identifier: GPL-2.0
/*
 *  linux/lib/string.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 */

/*
 * This file should be used only for "library" routines that may have
 * alternative implementations on specific architectures (generally
 * found in <asm-xx/string.h>), or get overloaded by FORTIFY_SOURCE.
 * (Specifically, this file is built with __NO_FORTIFY.)
 *
 * Other helper functions should live in string_helpers.c.
 */

#define __NO_FORTIFY
#include <linux/bits.h>
#include <linux/bug.h>
#include <linux/ctype.h>
#include <linux/errno.h>
#include <linux/limits.h>
#include <linux/linkage.h>
#include <linux/stddef.h>
#include <linux/string.h>
#include <linux/types.h>

#include <asm/page.h>
#include <asm/rwonce.h>
#include <linux/unaligned.h>
#include <asm/word-at-a-time.h>


#if BITS_PER_LONG == 64 && defined(CONFIG_HAVE_EFFICIENT_UNALIGNED_ACCESS)
#define STRINGOP_WORD_AT_A_TIME 1

static __always_inline unsigned long stringop_find_diff_byte(unsigned long a, unsigned long b)
{
	return __ffs(a ^ b) >> 3;
}

static __always_inline int stringop_word_cmp(unsigned long a, unsigned long b)
{
	unsigned long idx = stringop_find_diff_byte(a, b);
	unsigned long shift = idx * 8;
	return (int)((a >> shift) & 0xff) - (int)((b >> shift) & 0xff);
}
#endif

#ifndef __HAVE_ARCH_STRNCASECMP
/**
 * strncasecmp - Case insensitive, length-limited string comparison
 * @s1: One string
 * @s2: The other string
 * @len: the maximum number of characters to compare
 */
int strncasecmp(const char *s1, const char *s2, size_t len)
{
	/* Yes, Virginia, it had better be unsigned */
	unsigned char c1, c2;

	if (!len)
		return 0;

	do {
		c1 = *s1++;
		c2 = *s2++;
		if (!c1 || !c2)
			break;
		if (c1 == c2)
			continue;
		c1 = tolower(c1);
		c2 = tolower(c2);
		if (c1 != c2)
			break;
	} while (--len);
	return (int)c1 - (int)c2;
}
EXPORT_SYMBOL(strncasecmp);
#endif

#ifndef __HAVE_ARCH_STRCASECMP
int strcasecmp(const char *s1, const char *s2)
{
	int c1, c2;

	do {
		c1 = tolower(*s1++);
		c2 = tolower(*s2++);
	} while (c1 == c2 && c1 != 0);
	return c1 - c2;
}
EXPORT_SYMBOL(strcasecmp);
#endif

#ifndef __HAVE_ARCH_STRCPY
char *strcpy(char *dest, const char *src)
{
	char *tmp = dest;

	while ((*dest++ = *src++) != '\0')
		/* nothing */;
	return tmp;
}
EXPORT_SYMBOL(strcpy);
#endif

#ifndef __HAVE_ARCH_STRNCPY
char *strncpy(char *dest, const char *src, size_t count)
{
	char *tmp = dest;

	while (count) {
		if ((*tmp = *src) != 0)
			src++;
		tmp++;
		count--;
	}
	return dest;
}
EXPORT_SYMBOL(strncpy);
#endif

#ifdef __BIG_ENDIAN
# define ALLBUTLAST_BYTE_MASK (~255ul)
#else
# define ALLBUTLAST_BYTE_MASK (~0ul >> 8)
#endif

ssize_t sized_strscpy(char *dest, const char *src, size_t count)
{
	const struct word_at_a_time constants = WORD_AT_A_TIME_CONSTANTS;
	size_t max = count;
	long res = 0;

	if (count == 0 || WARN_ON_ONCE(count > INT_MAX))
		return -E2BIG;

#ifndef CONFIG_DCACHE_WORD_ACCESS
#ifdef CONFIG_HAVE_EFFICIENT_UNALIGNED_ACCESS
	/*
	 * If src is unaligned, don't cross a page boundary,
	 * since we don't know if the next page is mapped.
	 */
	if ((long)src & (sizeof(long) - 1)) {
		size_t limit = PAGE_SIZE - ((long)src & (PAGE_SIZE - 1));
		if (limit < max)
			max = limit;
	}
#else
	/* If src or dest is unaligned, don't do word-at-a-time. */
	if (((long) dest | (long) src) & (sizeof(long) - 1))
		max = 0;
#endif
#endif

	/*
	 * load_unaligned_zeropad() or read_word_at_a_time() below may read
	 * uninitialized bytes after the trailing zero and use them in
	 * comparisons. Disable this optimization under KMSAN to prevent
	 * false positive reports.
	 */
	if (IS_ENABLED(CONFIG_KMSAN))
		max = 0;

	while (max >= sizeof(unsigned long)) {
		unsigned long c, data;

#ifdef CONFIG_DCACHE_WORD_ACCESS
		c = load_unaligned_zeropad(src+res);
#else
		c = read_word_at_a_time(src+res);
#endif
		if (has_zero(c, &data, &constants)) {
			data = prep_zero_mask(c, data, &constants);
			data = create_zero_mask(data);
			*(unsigned long *)(dest+res) = c & zero_bytemask(data);
			return res + find_zero(data);
		}
		count -= sizeof(unsigned long);
		if (unlikely(!count)) {
			c &= ALLBUTLAST_BYTE_MASK;
			*(unsigned long *)(dest+res) = c;
			return -E2BIG;
		}
		*(unsigned long *)(dest+res) = c;
		res += sizeof(unsigned long);
		max -= sizeof(unsigned long);
	}

	while (count > 1) {
		char c;

		c = src[res];
		dest[res] = c;
		if (!c)
			return res;
		res++;
		count--;
	}

	/* Force NUL-termination. */
	dest[res] = '\0';

	/* Return E2BIG if the source didn't stop */
	return src[res] ? -E2BIG : res;
}
EXPORT_SYMBOL(sized_strscpy);

/**
 * stpcpy - copy a string from src to dest returning a pointer to the new end
 *          of dest, including src's %NUL-terminator. May overrun dest.
 * @dest: pointer to end of string being copied into. Must be large enough
 *        to receive copy.
 * @src: pointer to the beginning of string being copied from. Must not overlap
 *       dest.
 *
 * stpcpy differs from strcpy in a key way: the return value is a pointer
 * to the new %NUL-terminating character in @dest. (For strcpy, the return
 * value is a pointer to the start of @dest). This interface is considered
 * unsafe as it doesn't perform bounds checking of the inputs. As such it's
 * not recommended for usage. Instead, its definition is provided in case
 * the compiler lowers other libcalls to stpcpy.
 */
char *stpcpy(char *__restrict__ dest, const char *__restrict__ src);
char *stpcpy(char *__restrict__ dest, const char *__restrict__ src)
{
	while ((*dest++ = *src++) != '\0')
		/* nothing */;
	return --dest;
}
EXPORT_SYMBOL(stpcpy);

#ifndef __HAVE_ARCH_STRCAT
char *strcat(char *dest, const char *src)
{
	char *tmp = dest;

	while (*dest)
		dest++;
	while ((*dest++ = *src++) != '\0')
		;
	return tmp;
}
EXPORT_SYMBOL(strcat);
#endif

#ifndef __HAVE_ARCH_STRNCAT
char *strncat(char *dest, const char *src, size_t count)
{
	char *tmp = dest;

	if (count) {
		while (*dest)
			dest++;
		while ((*dest++ = *src++) != 0) {
			if (--count == 0) {
				*dest = '\0';
				break;
			}
		}
	}
	return tmp;
}
EXPORT_SYMBOL(strncat);
#endif

#ifndef __HAVE_ARCH_STRLCAT
size_t strlcat(char *dest, const char *src, size_t count)
{
	size_t dsize = strlen(dest);
	size_t len = strlen(src);
	size_t res = dsize + len;

	/* This would be a bug */
	BUG_ON(dsize >= count);

	dest += dsize;
	count -= dsize;
	if (len >= count)
		len = count-1;
	__builtin_memcpy(dest, src, len);
	dest[len] = 0;
	return res;
}
EXPORT_SYMBOL(strlcat);
#endif

#ifndef __HAVE_ARCH_STRCMP
int strcmp(const char *cs, const char *ct)
{
	unsigned char c1 = *cs;
	unsigned char c2 = *ct;

	if (c1 != c2)
		return c1 < c2 ? -1 : 1;
	if (!c1)
		return 0;

#if defined(STRINGOP_WORD_AT_A_TIME)
	const struct word_at_a_time constants = WORD_AT_A_TIME_CONSTANTS;

	for (;;) {
		unsigned long a = load_unaligned_zeropad(cs);
		unsigned long b = load_unaligned_zeropad(ct);
		unsigned long bits;

		if (a != b || has_zero(a, &bits, &constants) ||
		    has_zero(b, &bits, &constants)) {
			for (;;) {
				unsigned char c1 = *cs++;
				unsigned char c2 = *ct++;

				if (c1 != c2)
					return c1 < c2 ? -1 : 1;
				if (!c1)
					return 0;
			}
		}
		cs += sizeof(unsigned long);
		ct += sizeof(unsigned long);
	}
#else
	while (1) {
		c1 = *cs++;
		c2 = *ct++;
		if (c1 != c2)
			return c1 < c2 ? -1 : 1;
		if (!c1)
			break;
	}
	return 0;
#endif
}
EXPORT_SYMBOL(strcmp);
#endif

#ifndef __HAVE_ARCH_STRNCMP
/**
 * strncmp - Compare two length-limited strings
 * @cs: One string
 * @ct: Another string
 * @count: The maximum number of bytes to compare
 */
int strncmp(const char *cs, const char *ct, size_t count)
{
#if defined(STRINGOP_WORD_AT_A_TIME)
	const struct word_at_a_time constants = WORD_AT_A_TIME_CONSTANTS;

	if (count < 16)
		goto byte_loop;
	if (*cs != *ct)
		return *(const unsigned char *)cs < *(const unsigned char *)ct ? -1 : 1;
	if (!*cs)
		return 0;
	while (count >= sizeof(unsigned long)) {
		unsigned long a = get_unaligned((const unsigned long *)cs);
		unsigned long b = get_unaligned((const unsigned long *)ct);
		unsigned long bits;

		if (a != b || has_zero(a, &bits, &constants) ||
		    has_zero(b, &bits, &constants))
			break;
		cs += sizeof(unsigned long);
		ct += sizeof(unsigned long);
		count -= sizeof(unsigned long);
	}
byte_loop:
#endif
	while (count) {
		unsigned char c1 = *cs++;
		unsigned char c2 = *ct++;

		if (c1 != c2)
			return c1 < c2 ? -1 : 1;
		if (!c1)
			break;
		count--;
	}
	return 0;
}
EXPORT_SYMBOL(strncmp);
#endif

#ifndef __HAVE_ARCH_STRCHR
/**
 * strchr - Find the first occurrence of a character in a string
 * @s: The string to be searched
 * @c: The character to search for
 *
 * Note that the %NUL-terminator is considered part of the string, and can
 * be searched for.
 */
char *strchr(const char *s, int c)
{
	if (*s == (char)c)
		return (char *)s;
	if (!*s)
		return NULL;

#if defined(STRINGOP_WORD_AT_A_TIME)
	const struct word_at_a_time constants = WORD_AT_A_TIME_CONSTANTS;
	unsigned long repeated = REPEAT_BYTE((unsigned char)c);

	for (;;) {
		unsigned long word = load_unaligned_zeropad(s);
		unsigned long mbits, zbits, bits;

		has_zero(word ^ repeated, &mbits, &constants);
		has_zero(word, &zbits, &constants);
		bits = mbits | zbits;
		if (bits) {
			unsigned long idx = find_zero(bits);
			return s[idx] == (char)c ? (char *)s + idx : NULL;
		}
		s += sizeof(unsigned long);
	}
#else
	for (; *s != (char)c; ++s)
		if (*s == '\0')
			return NULL;
	return (char *)s;
#endif
}
EXPORT_SYMBOL(strchr);
#endif

#ifndef __HAVE_ARCH_STRCHRNUL
/**
 * strchrnul - Find and return a character in a string, or end of string
 * @s: The string to be searched
 * @c: The character to search for
 *
 * Returns pointer to first occurrence of 'c' in s. If c is not found, then
 * return a pointer to the null byte at the end of s.
 */
char *strchrnul(const char *s, int c)
{
	if (*s == (char)c || !*s)
		return (char *)s;

#if defined(STRINGOP_WORD_AT_A_TIME)
	const struct word_at_a_time constants = WORD_AT_A_TIME_CONSTANTS;
	unsigned long repeated = REPEAT_BYTE((unsigned char)c);

	for (;;) {
		unsigned long word = load_unaligned_zeropad(s);
		unsigned long mbits, zbits, bits;

		has_zero(word ^ repeated, &mbits, &constants);
		has_zero(word, &zbits, &constants);
		bits = mbits | zbits;
		if (bits)
			return (char *)s + find_zero(bits);
		s += sizeof(unsigned long);
	}
#else
	while (*s && *s != (char)c)
		s++;
	return (char *)s;
#endif
}
EXPORT_SYMBOL(strchrnul);
#endif

/**
 * strnchrnul - Find and return a character in a length limited string,
 * or end of string
 * @s: The string to be searched
 * @count: The number of characters to be searched
 * @c: The character to search for
 *
 * Returns pointer to the first occurrence of 'c' in s. If c is not found,
 * then return a pointer to the last character of the string.
 */
char *strnchrnul(const char *s, size_t count, int c)
{
	while (count-- && *s && *s != (char)c)
		s++;
	return (char *)s;
}

#ifndef __HAVE_ARCH_STRRCHR
/**
 * strrchr - Find the last occurrence of a character in a string
 * @s: The string to be searched
 * @c: The character to search for
 */
char *strrchr(const char *s, int c)
{
	const char *last = NULL;
	do {
		if (*s == (char)c)
			last = s;
	} while (*s++);
	return (char *)last;
}
EXPORT_SYMBOL(strrchr);
#endif

#ifndef __HAVE_ARCH_STRNCHR
/**
 * strnchr - Find a character in a length limited string
 * @s: The string to be searched
 * @count: The number of characters to be searched
 * @c: The character to search for
 *
 * Note that the %NUL-terminator is considered part of the string, and can
 * be searched for.
 */
char *strnchr(const char *s, size_t count, int c)
{
#if defined(STRINGOP_WORD_AT_A_TIME)
	const struct word_at_a_time constants = WORD_AT_A_TIME_CONSTANTS;
	unsigned long repeated = REPEAT_BYTE((unsigned char)c);

	if (!count)
		return NULL;
	if (*s == (char)c)
		return (char *)s;
	if (!*s)
		return NULL;
	if (count < 16)
		goto byte_tail;
	while (count >= sizeof(unsigned long)) {
		unsigned long word = get_unaligned((const unsigned long *)s);
		unsigned long mbits, zbits, bits;

		has_zero(word ^ repeated, &mbits, &constants);
		has_zero(word, &zbits, &constants);
		bits = mbits | zbits;
		if (bits) {
			unsigned long idx = find_zero(bits);
			return s[idx] == (char)c ? (char *)s + idx : NULL;
		}
		s += sizeof(unsigned long);
		count -= sizeof(unsigned long);
	}
byte_tail:
#endif
	while (count--) {
		if (*s == (char)c)
			return (char *)s;
		if (*s++ == '\0')
			break;
	}
	return NULL;
}
EXPORT_SYMBOL(strnchr);
#endif

#ifndef __HAVE_ARCH_STRLEN
size_t strlen(const char *s)
{
#if defined(STRINGOP_WORD_AT_A_TIME)
	const struct word_at_a_time constants = WORD_AT_A_TIME_CONSTANTS;
	const char *p = s;

	for (;;) {
		unsigned long word = load_unaligned_zeropad(p);
		unsigned long bits;

		if (has_zero(word, &bits, &constants))
			return p - s + find_zero(bits);
		p += sizeof(unsigned long);
	}
#else
	const char *sc;

	for (sc = s; *sc != '\0'; ++sc)
		/* nothing */;
	return sc - s;
#endif
}
EXPORT_SYMBOL(strlen);
#endif

#ifndef __HAVE_ARCH_STRNLEN
size_t strnlen(const char *s, size_t count)
{
#if defined(STRINGOP_WORD_AT_A_TIME)
	const struct word_at_a_time constants = WORD_AT_A_TIME_CONSTANTS;
	const char *p = s;

	if (count < sizeof(unsigned long))
		goto byte_tail;
	while (count >= sizeof(unsigned long)) {
		unsigned long word = get_unaligned((const unsigned long *)p);
		unsigned long bits;

		if (has_zero(word, &bits, &constants))
			return p - s + find_zero(bits);
		p += sizeof(unsigned long);
		count -= sizeof(unsigned long);
	}
byte_tail:
	while (count && *p) {
		p++;
		count--;
	}
	return p - s;
#else
	const char *sc;

	for (sc = s; count-- && *sc != '\0'; ++sc)
		/* nothing */;
	return sc - s;
#endif
}
EXPORT_SYMBOL(strnlen);
#endif

#ifndef __HAVE_ARCH_STRSPN
/**
 * strspn - Calculate the length of the initial substring of @s which only contain letters in @accept
 * @s: The string to be searched
 * @accept: The string to search for
 */
size_t strspn(const char *s, const char *accept)
{
	const char *p;

	for (p = s; *p != '\0'; ++p) {
		if (!strchr(accept, *p))
			break;
	}
	return p - s;
}
EXPORT_SYMBOL(strspn);
#endif

#ifndef __HAVE_ARCH_STRCSPN
/**
 * strcspn - Calculate the length of the initial substring of @s which does not contain letters in @reject
 * @s: The string to be searched
 * @reject: The string to avoid
 */
size_t strcspn(const char *s, const char *reject)
{
	const char *p;

	for (p = s; *p != '\0'; ++p) {
		if (strchr(reject, *p))
			break;
	}
	return p - s;
}
EXPORT_SYMBOL(strcspn);
#endif

#ifndef __HAVE_ARCH_STRPBRK
/**
 * strpbrk - Find the first occurrence of a set of characters
 * @cs: The string to be searched
 * @ct: The characters to search for
 */
char *strpbrk(const char *cs, const char *ct)
{
	const char *sc;

	for (sc = cs; *sc != '\0'; ++sc) {
		if (strchr(ct, *sc))
			return (char *)sc;
	}
	return NULL;
}
EXPORT_SYMBOL(strpbrk);
#endif

#ifndef __HAVE_ARCH_STRSEP
/**
 * strsep - Split a string into tokens
 * @s: The string to be searched
 * @ct: The characters to search for
 *
 * strsep() updates @s to point after the token, ready for the next call.
 *
 * It returns empty tokens, too, behaving exactly like the libc function
 * of that name. In fact, it was stolen from glibc2 and de-fancy-fied.
 * Same semantics, slimmer shape. ;)
 */
char *strsep(char **s, const char *ct)
{
	char *sbegin = *s;
	char *end;

	if (sbegin == NULL)
		return NULL;

	end = strpbrk(sbegin, ct);
	if (end)
		*end++ = '\0';
	*s = end;
	return sbegin;
}
EXPORT_SYMBOL(strsep);
#endif

#ifndef __HAVE_ARCH_MEMSET
/**
 * memset - Fill a region of memory with the given value
 * @s: Pointer to the start of the area.
 * @c: The byte to fill the area with
 * @count: The size of the area.
 *
 * Do not use memset() to access IO space, use memset_io() instead.
 */
void *memset(void *s, int c, size_t count)
{
	char *xs = s;

	while (count--)
		*xs++ = c;
	return s;
}
EXPORT_SYMBOL(memset);
#endif

#ifndef __HAVE_ARCH_MEMSET16
/**
 * memset16() - Fill a memory area with a uint16_t
 * @s: Pointer to the start of the area.
 * @v: The value to fill the area with
 * @count: The number of values to store
 *
 * Differs from memset() in that it fills with a uint16_t instead
 * of a byte.  Remember that @count is the number of uint16_ts to
 * store, not the number of bytes.
 */
void *memset16(uint16_t *s, uint16_t v, size_t count)
{
	uint16_t *xs = s;

	while (count--)
		*xs++ = v;
	return s;
}
EXPORT_SYMBOL(memset16);
#endif

#ifndef __HAVE_ARCH_MEMSET32
/**
 * memset32() - Fill a memory area with a uint32_t
 * @s: Pointer to the start of the area.
 * @v: The value to fill the area with
 * @count: The number of values to store
 *
 * Differs from memset() in that it fills with a uint32_t instead
 * of a byte.  Remember that @count is the number of uint32_ts to
 * store, not the number of bytes.
 */
void *memset32(uint32_t *s, uint32_t v, size_t count)
{
	uint32_t *xs = s;

	while (count--)
		*xs++ = v;
	return s;
}
EXPORT_SYMBOL(memset32);
#endif

#ifndef __HAVE_ARCH_MEMSET64
/**
 * memset64() - Fill a memory area with a uint64_t
 * @s: Pointer to the start of the area.
 * @v: The value to fill the area with
 * @count: The number of values to store
 *
 * Differs from memset() in that it fills with a uint64_t instead
 * of a byte.  Remember that @count is the number of uint64_ts to
 * store, not the number of bytes.
 */
void *memset64(uint64_t *s, uint64_t v, size_t count)
{
	uint64_t *xs = s;

	while (count--)
		*xs++ = v;
	return s;
}
EXPORT_SYMBOL(memset64);
#endif

#ifndef __HAVE_ARCH_MEMCPY
/**
 * memcpy - Copy one area of memory to another
 * @dest: Where to copy to
 * @src: Where to copy from
 * @count: The size of the area.
 *
 * You should not use this function to access IO space, use memcpy_toio()
 * or memcpy_fromio() instead.
 */
void *memcpy(void *dest, const void *src, size_t count)
{
	char *tmp = dest;
	const char *s = src;

	while (count--)
		*tmp++ = *s++;
	return dest;
}
EXPORT_SYMBOL(memcpy);
#endif

#ifndef __HAVE_ARCH_MEMMOVE
/**
 * memmove - Copy one area of memory to another
 * @dest: Where to copy to
 * @src: Where to copy from
 * @count: The size of the area.
 *
 * Unlike memcpy(), memmove() copes with overlapping areas.
 */
void *memmove(void *dest, const void *src, size_t count)
{
	char *tmp;
	const char *s;

	if (dest <= src) {
		tmp = dest;
		s = src;
		while (count--)
			*tmp++ = *s++;
	} else {
		tmp = dest;
		tmp += count;
		s = src;
		s += count;
		while (count--)
			*--tmp = *--s;
	}
	return dest;
}
EXPORT_SYMBOL(memmove);
#endif

#ifndef __HAVE_ARCH_MEMCMP
/**
 * memcmp - Compare two areas of memory
 * @cs: One area of memory
 * @ct: Another area of memory
 * @count: The size of the area.
 */
#undef memcmp
__visible int memcmp(const void *cs, const void *ct, size_t count)
{
	const unsigned char *su1 = cs, *su2 = ct;

#if defined(STRINGOP_WORD_AT_A_TIME)
	if (!count)
		return 0;
	if (*su1 != *su2)
		return *su1 - *su2;
	if (count < sizeof(unsigned long))
		goto byte_tail;
	while (count >= sizeof(unsigned long)) {
		unsigned long a = get_unaligned((const unsigned long *)su1);
		unsigned long b = get_unaligned((const unsigned long *)su2);

		if (a != b)
			return stringop_word_cmp(a, b);
		su1 += sizeof(unsigned long);
		su2 += sizeof(unsigned long);
		count -= sizeof(unsigned long);
	}
byte_tail:
#endif
	while (count) {
		int res = *su1++ - *su2++;

		if (res)
			return res;
		count--;
	}
	return 0;
}
EXPORT_SYMBOL(memcmp);
#endif

#ifndef __HAVE_ARCH_BCMP
/**
 * bcmp - returns 0 if and only if the buffers have identical contents.
 * @a: pointer to first buffer.
 * @b: pointer to second buffer.
 * @len: size of buffers.
 *
 * The sign or magnitude of a non-zero return value has no particular
 * meaning, and architectures may implement their own more efficient bcmp().
 */
int bcmp(const void *a, const void *b, size_t len)
{
#if defined(STRINGOP_WORD_AT_A_TIME)
	const unsigned char *p1 = a, *p2 = b;

	if (!len)
		return 0;
	if (*p1 != *p2)
		return 1;
	if (len < sizeof(unsigned long))
		goto byte_tail;
	while (len >= sizeof(unsigned long)) {
		if (get_unaligned((const unsigned long *)p1) !=
		    get_unaligned((const unsigned long *)p2))
			return 1;
		p1 += sizeof(unsigned long);
		p2 += sizeof(unsigned long);
		len -= sizeof(unsigned long);
	}
byte_tail:
	while (len) {
		if (*p1++ != *p2++)
			return 1;
		len--;
	}
	return 0;
#else
	return memcmp(a, b, len);
#endif
}
EXPORT_SYMBOL(bcmp);
#endif

#ifndef __HAVE_ARCH_MEMSCAN
/**
 * memscan - Find a character in an area of memory.
 * @addr: The memory area
 * @c: The byte to search for
 * @size: The size of the area.
 *
 * returns the address of the first occurrence of @c, or 1 byte past
 * the area if @c is not found
 */
void *memscan(void *addr, int c, size_t size)
{
	unsigned char *p = addr;

#if defined(STRINGOP_WORD_AT_A_TIME)
	const struct word_at_a_time constants = WORD_AT_A_TIME_CONSTANTS;
	unsigned long repeated = REPEAT_BYTE((unsigned char)c);

	if (!size)
		return p;
	if (*p == (unsigned char)c)
		return p;
	if (size < sizeof(unsigned long)) {
		p++;
		size--;
		goto byte_tail;
	}
	while (size >= sizeof(unsigned long)) {
		unsigned long word = get_unaligned((const unsigned long *)p);
		unsigned long bits;

		if (has_zero(word ^ repeated, &bits, &constants))
			return p + find_zero(bits);
		p += sizeof(unsigned long);
		size -= sizeof(unsigned long);
	}
byte_tail:
#endif
	while (size) {
		if (*p == (unsigned char)c)
			return (void *)p;
		p++;
		size--;
	}
	return (void *)p;
}
EXPORT_SYMBOL(memscan);
#endif

#ifndef __HAVE_ARCH_STRSTR
/**
 * strstr - Find the first substring in a %NUL terminated string
 * @s1: The string to be searched
 * @s2: The string to search for
 */
char *strstr(const char *s1, const char *s2)
{
	size_t l1, l2;

	l2 = strlen(s2);
	if (!l2)
		return (char *)s1;
	l1 = strlen(s1);
	while (l1 >= l2) {
		l1--;
		if (!memcmp(s1, s2, l2))
			return (char *)s1;
		s1++;
	}
	return NULL;
}
EXPORT_SYMBOL(strstr);
#endif

#ifndef __HAVE_ARCH_STRNSTR
/**
 * strnstr - Find the first substring in a length-limited string
 * @s1: The string to be searched
 * @s2: The string to search for
 * @len: the maximum number of characters to search
 */
char *strnstr(const char *s1, const char *s2, size_t len)
{
	size_t l2;

	l2 = strlen(s2);
	if (!l2)
		return (char *)s1;
	while (len >= l2) {
		len--;
		if (!memcmp(s1, s2, l2))
			return (char *)s1;
		s1++;
	}
	return NULL;
}
EXPORT_SYMBOL(strnstr);
#endif

#ifndef __HAVE_ARCH_MEMCHR
/**
 * memchr - Find a character in an area of memory.
 * @s: The memory area
 * @c: The byte to search for
 * @n: The size of the area.
 *
 * returns the address of the first occurrence of @c, or %NULL
 * if @c is not found
 */
void *memchr(const void *s, int c, size_t n)
{
	const unsigned char *p = s;

#if defined(STRINGOP_WORD_AT_A_TIME)
	const struct word_at_a_time constants = WORD_AT_A_TIME_CONSTANTS;
	unsigned long repeated = REPEAT_BYTE((unsigned char)c);

	if (!n)
		return NULL;
	if (*p == (unsigned char)c)
		return (void *)p;
	if (n < sizeof(unsigned long)) {
		p++;
		n--;
		goto byte_tail;
	}
	while (n >= sizeof(unsigned long)) {
		unsigned long word = get_unaligned((const unsigned long *)p);
		unsigned long bits;

		if (has_zero(word ^ repeated, &bits, &constants))
			return (void *)(p + find_zero(bits));
		p += sizeof(unsigned long);
		n -= sizeof(unsigned long);
	}
byte_tail:
#endif
	while (n-- != 0) {
		if ((unsigned char)c == *p++)
			return (void *)(p - 1);
	}
	return NULL;
}
EXPORT_SYMBOL(memchr);
#endif

static void *check_bytes8(const u8 *start, u8 value, unsigned int bytes)
{
	while (bytes) {
		if (*start != value)
			return (void *)start;
		start++;
		bytes--;
	}
	return NULL;
}

/**
 * memchr_inv - Find an unmatching character in an area of memory.
 * @start: The memory area
 * @c: Find a character other than c
 * @bytes: The size of the area.
 *
 * returns the address of the first character other than @c, or %NULL
 * if the whole buffer contains just @c.
 */
void *memchr_inv(const void *start, int c, size_t bytes)
{
	u8 value = c;
	u64 value64;
	unsigned int words, prefix;

	if (bytes <= 16)
		return check_bytes8(start, value, bytes);

	value64 = value;
#if defined(CONFIG_ARCH_HAS_FAST_MULTIPLIER) && BITS_PER_LONG == 64
	value64 *= 0x0101010101010101ULL;
#elif defined(CONFIG_ARCH_HAS_FAST_MULTIPLIER)
	value64 *= 0x01010101;
	value64 |= value64 << 32;
#else
	value64 |= value64 << 8;
	value64 |= value64 << 16;
	value64 |= value64 << 32;
#endif

	prefix = (unsigned long)start % 8;
	if (prefix) {
		u8 *r;

		prefix = 8 - prefix;
		r = check_bytes8(start, value, prefix);
		if (r)
			return r;
		start += prefix;
		bytes -= prefix;
	}

	words = bytes / 8;

	while (words) {
		if (*(u64 *)start != value64)
			return check_bytes8(start, value, 8);
		start += 8;
		words--;
	}

	return check_bytes8(start, value, bytes % 8);
}
EXPORT_SYMBOL(memchr_inv);
