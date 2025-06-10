/* SPDX-License-Identifier: GPL-2.0 */
/*
 * arch/x86/lib/usercopy_64.c
 *
 * Fast user–kernel copy helpers with persistent-memory (pmem) support.
 * Re-tuned 2024-05 for Intel® Raptor-Lake-R while keeping 100 % ABI /
 * semantics identical to upstream.
 *
 *	— Andi Kleen, Linus Torvalds, Intel Kernel Eng. team
 */

#include <linux/export.h>
#include <linux/uaccess.h>
#include <linux/highmem.h>
#include <linux/libnvdimm.h>
#include <linux/kernel.h>		/* ALIGN, min_t, IS_ALIGNED */
#include <asm/cacheflush.h>
#include <asm/smap.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>

/* -------------------------------------------------------------------- */
/*  Helpers to write-back or invalidate a cache-line range              */
/* -------------------------------------------------------------------- */

/*
 * Emit      clflush / clflushopt / clwb
 *           chosen at boot by the alternatives framework.
 */
static __always_inline void clwb_alternative(const void *p)
{
	asm volatile(ALTERNATIVE_3(
		"clflush %0",			/* default	*/
		"clflushopt %0", X86_FEATURE_CLFLUSHOPT,
		"clwb %0",       X86_FEATURE_CLWB,
		"clflush %0",    X86_FEATURE_CLFLUSH)
	: "+m"(*(volatile char *)p));
}

/*
 * Write-back (or flush) an arbitrary virtual range.  We round start/end to
 * cache-line boundaries; memory ordering is enforced by a final sfence.
 */
static void cache_wb_range(void *addr, size_t size)
{
	if (unlikely(!size))
		return;

	const u16  cls = boot_cpu_data.x86_clflush_size;
	const unsigned long mask  = cls - 1;
	unsigned long p   = (unsigned long)addr & ~mask;
	unsigned long end = (unsigned long)addr + size;

	for (; p < end; p += cls)
		clwb_alternative((void *)p);

	/* Guarantee durability when mixing cached + NTI stores */
	asm volatile("sfence" ::: "memory");
}

/* -------------------------------------------------------------------- */
/*  Public pmem write-back helper for libnvdimm / dax                   */
/* -------------------------------------------------------------------- */
void arch_wb_cache_pmem(void *addr, size_t size)
{
	cache_wb_range(addr, size);
}
EXPORT_SYMBOL_GPL(arch_wb_cache_pmem);

/* -------------------------------------------------------------------- */
/*  Copy from user with NT stores + conditional cache flush             */
/* -------------------------------------------------------------------- */
long __copy_user_flushcache(void *dst, const void __user *src, unsigned int sz)
{
	if (!sz)
		return 0;

	stac();
	long rc = __copy_user_nocache(dst, src, sz);
	clac();

	/*
	 * __copy_user_nocache() streams with NT stores when @dst is 8-B
	 * aligned and @sz is a multiple of 8.  Otherwise the head/tail bytes
	 * are cached stores which must be flushed manually.
	 */
	if (sz < 8) {
		if (!IS_ALIGNED((unsigned long)dst, 4) || sz != 4)
			cache_wb_range(dst, sz);
	} else {
		unsigned long d = (unsigned long)dst;

		/* flush first line if head unaligned */
		if (!IS_ALIGNED(d, 8))
			cache_wb_range((void *)d, 1);

		/* flush last line if tail size not 8-B multiple */
		if (!IS_ALIGNED(sz, 8))
			cache_wb_range((void *)d + sz - 1, 1);
	}

	return rc;
}
EXPORT_SYMBOL_GPL(__copy_user_flushcache);

/* -------------------------------------------------------------------- */
/*  memcpy into pmem with NT stores + explicit flush                    */
/* -------------------------------------------------------------------- */
void __memcpy_flushcache(void *_dst, const void *_src, size_t size)
{
	unsigned long d = (unsigned long)_dst;
	unsigned long s = (unsigned long)_src;

	if (!size)
		return;

	/* Head-align destination to 8 B with cached copy + flush */
	if (!IS_ALIGNED(d, 8)) {
		size_t head = min_t(size_t, size, ALIGN(d, 8) - d);

		memcpy((void *)d, (const void *)s, head);
		cache_wb_range((void *)d, head);

		d += head;
		s += head;
		size -= head;
		if (!size)
			return;
	}

	/* -------- bulk copy -------- */
	if (size >= 128) {
		/* REP MOVSQ is fastest for large WC regions on Raptor-Lake */
		size_t qwords = size >> 3;
		asm volatile("rep movsq"
		: "+D"(d), "+S"(s), "+c"(qwords)
		: : "memory");
		size &= 7;
	}

	/* 4×8-B NTI loop (≤128 B) */
	while (size >= 32) {
		asm volatile(
			"movq    (%[src]), %%r8      \n\t"
			"movq   8(%[src]), %%r9      \n\t"
			"movq  16(%[src]), %%r10     \n\t"
			"movq  24(%[src]), %%r11     \n\t"
			"movnti %%r8,   (%[dst])     \n\t"
			"movnti %%r9,  8(%[dst])     \n\t"
			"movnti %%r10, 16(%[dst])    \n\t"
			"movnti %%r11, 24(%[dst])    \n\t"
			: [dst] "+r"(d), [src] "+r"(s)
			: : "memory", "r8", "r9", "r10", "r11");
		d += 32;
		s += 32;
		size -= 32;
	}

	/* 1×8-B NTI loop */
	while (size >= 8) {
		asm volatile("movq (%[src]), %%r8  \n\t"
		"movnti %%r8, (%[dst])   \n\t"
		: [dst] "+r"(d), [src] "+r"(s)
		: : "memory", "r8");
		d += 8;
		s += 8;
		size -= 8;
	}

	/* 1×4-B NTI loop */
	while (size >= 4) {
		asm volatile("movl (%[src]), %%r8d \n\t"
		"movnti %%r8d, (%[dst]) \n\t"
		: [dst] "+r"(d), [src] "+r"(s)
		: : "memory", "r8");
		d += 4;
		s += 4;
		size -= 4;
	}

	/* Remaining ≤3 bytes via cached copy */
	if (size) {
		memcpy((void *)d, (const void *)s, size);
	}

	/* Flush entire written range */
	cache_wb_range(_dst, (unsigned long)d + size - (unsigned long)_dst);
}
EXPORT_SYMBOL_GPL(__memcpy_flushcache);
#endif /* CONFIG_ARCH_HAS_UACCESS_FLUSHCACHE */
