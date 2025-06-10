// SPDX-License-Identifier: GPL-2.0
/*
 * arch/x86/lib/usercopy_64.c
 *
 * Fast user↔kernel copy helpers with persistent-memory support.
 * Modernised for Intel® Raptor-Lake-R while keeping original ABI.
 */

#include <linux/export.h>
#include <linux/uaccess.h>
#include <linux/highmem.h>
#include <linux/libnvdimm.h>
#include <asm/cacheflush.h>
#include <asm/smap.h>
#include <asm/cpufeatures.h>
#include <asm/alternative.h>

#ifdef CONFIG_ARCH_HAS_UACCESS_FLUSHCACHE
/* --------------------------------------------------------------------
 *  Cache-flush helpers
 * ------------------------------------------------------------------ */
static __always_inline void clwb_alternative(const void *p)
{
	asm volatile(ALTERNATIVE_3("clflush %0",
							   "clflushopt %0", X86_FEATURE_CLFLUSHOPT,
							"clwb %0",       X86_FEATURE_CLWB,
							"clflush %0",    X86_FEATURE_CLFLUSH)
	: "+m"(*(volatile char *)p));
}

static void cache_wb_range(void *addr, size_t size)
{
	if (!size)
		return;

	const unsigned int cls  = boot_cpu_data.x86_clflush_size;
	const unsigned long msk = cls - 1;
	unsigned long p   = (unsigned long)addr & ~msk;
	unsigned long end = (unsigned long)addr + size;

	for (; p < end; p += cls)
		clwb_alternative((void *)p);

	asm volatile("sfence" ::: "memory");	/* durability barrier */
}

/* -------------------------------------------------------------------- */
void arch_wb_cache_pmem(void *addr, size_t size)
{
	cache_wb_range(addr, size);
}
EXPORT_SYMBOL_GPL(arch_wb_cache_pmem);

/* --------------------------------------------------------------------
 *  copy_user with flushcache
 * ------------------------------------------------------------------ */
long __copy_user_flushcache(void *dst, const void __user *src, unsigned int sz)
{
	if (!sz)
		return 0;

	stac();
	long rc = __copy_user_nocache(dst, src, sz);
	clac();

	if (rc || !sz)
		return rc;

	if (sz < 8) {
		if (!IS_ALIGNED((unsigned long)dst, 4) || sz != 4)
			cache_wb_range(dst, sz);
	} else {
		unsigned long d = (unsigned long)dst;

		if (!IS_ALIGNED(d, 8))
			cache_wb_range((void *)d, 1);

		if (!IS_ALIGNED(sz, 8))
			cache_wb_range((void *)(d + sz - 1), 1);
	}
	return rc;
}
EXPORT_SYMBOL_GPL(__copy_user_flushcache);

/* --------------------------------------------------------------------
 *  memcpy into pmem with NT stores + explicit flush
 * ------------------------------------------------------------------ */
void __memcpy_flushcache(void *dst, const void *src, size_t size)
{
	unsigned long d = (unsigned long)dst;
	unsigned long s = (unsigned long)src;

	if (!size)
		return;

	/* align destination to 8 */
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

	/* large copy via REP MOVSQ */
	if (size >= 128) {
		size_t qwords = size >> 3;
		asm volatile("rep movsq"
		: "+D"(d), "+S"(s), "+c"(qwords)
		: : "memory");
		size &= 7;
	}

	/* NTI loops */
	while (size >= 32) {
		asm volatile(
			"movq    (%[s]), %%r8\n\t"
			"movq   8(%[s]), %%r9\n\t"
			"movq  16(%[s]), %%r10\n\t"
			"movq  24(%[s]), %%r11\n\t"
			"movnti %%r8,   (%[d])\n\t"
			"movnti %%r9,  8(%[d])\n\t"
			"movnti %%r10, 16(%[d])\n\t"
			"movnti %%r11, 24(%[d])"
			: [d] "+r"(d), [s] "+r"(s)
			: : "memory", "r8", "r9", "r10", "r11");
		d += 32; s += 32; size -= 32;
	}
	while (size >= 8) {
		asm volatile("movq (%[s]), %%r8 ; movnti %%r8,(%[d])"
		: [d] "+r"(d), [s] "+r"(s)
		: : "memory", "r8");
		d += 8; s += 8; size -= 8;
	}
	while (size >= 4) {
		asm volatile("movl (%[s]), %%r8d ; movnti %%r8d,(%[d])"
		: [d] "+r"(d), [s] "+r"(s)
		: : "memory", "r8");
		d += 4; s += 4; size -= 4;
	}

	if (size) {
		memcpy((void *)d, (const void *)s, size);
	}

	cache_wb_range(dst, (unsigned long)d + size - (unsigned long)dst);
}
EXPORT_SYMBOL_GPL(__memcpy_flushcache);

#endif /* CONFIG_ARCH_HAS_UACCESS_FLUSHCACHE */
