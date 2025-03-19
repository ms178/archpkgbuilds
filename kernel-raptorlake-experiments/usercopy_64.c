// SPDX-License-Identifier: GPL-2.0-only
/*
 * User address space access functions.
 * Optimized for Intel Raptor Lake architecture.
 */
#include <linux/export.h>
#include <linux/uaccess.h>
#include <linux/highmem.h>
#include <linux/libnvdimm.h>
#include <asm/cpufeature.h>
#include <asm/processor.h>

// Function Prototypes (Declarations)
static inline void clean_cache_range(void *addr, size_t size);
static inline void __memcpy_flushcache_avx2(void *dst, const void *src, size_t size);
static inline void __memcpy_flushcache_std(void *dst, const void *src, size_t size);
void arch_wb_cache_pmem(void *addr, size_t size);
long __copy_user_flushcache(void *dst, const void __user *src, unsigned size);
void __memcpy_flushcache(void *dst, const void *src, size_t size);

#ifdef CONFIG_ARCH_HAS_UACCESS_FLUSHCACHE

static inline void clean_cache_range(void *addr, size_t size)
{
	u16 x86_clflush_size = boot_cpu_data.x86_clflush_size;
	unsigned long clflush_mask = x86_clflush_size - 1;
	void *vend = addr + size;
	void *p;

	p = (void *)((unsigned long)addr & ~clflush_mask);

	while (likely(p + 4 * x86_clflush_size <= vend)) {
		clwb(p);
		clwb(p + x86_clflush_size);
		clwb(p + 2 * x86_clflush_size);
		clwb(p + 3 * x86_clflush_size);
		p += 4 * x86_clflush_size;
	}

	while (unlikely(p < vend)) {
		clwb(p);
		p += x86_clflush_size;
	}
}

void arch_wb_cache_pmem(void *addr, size_t size)
{
	clean_cache_range(addr, size);
}
EXPORT_SYMBOL_GPL(arch_wb_cache_pmem);

long __copy_user_flushcache(void *dst, const void __user *src, unsigned size)
{
	unsigned long flushed, dest = (unsigned long) dst;
	long rc;
	u16 x86_clflush_size = boot_cpu_data.x86_clflush_size;

	stac();
	rc = __copy_user_nocache(dst, src, size);
	clac();

	if (size < 8) {
		if (!IS_ALIGNED(dest, 4) || size != 4)
			clean_cache_range(dst, size);
	} else {
		if (!IS_ALIGNED(dest, 8)) {
			unsigned long next_aligned = ALIGN(dest, x86_clflush_size);
			clean_cache_range(dst, next_aligned - dest);
			dest = next_aligned;
		}

		flushed = dest - (unsigned long) dst;
		if (size > flushed && !IS_ALIGNED(size - flushed, 8)) {
			unsigned long end = (unsigned long)dst + size;
			unsigned long prev_aligned = end & ~(x86_clflush_size - 1);
			clean_cache_range((void*)prev_aligned, end - prev_aligned);

		}
	}

	return rc;
}

static inline void __memcpy_flushcache_avx2(void *dst, const void *src, size_t size)
{
	unsigned long dest = (unsigned long) dst;
	unsigned long source = (unsigned long) src;
	size_t len = size;

	if (size < 128) {
		__memcpy_flushcache_std(dst, src, size);
		return;
	}

	if (!IS_ALIGNED(dest, 32)) {
		size_t headLen = ALIGN(dest, 32) - dest;
		memcpy((void *)dest, (void *)source, headLen);
		clean_cache_range((void *)dest, headLen);
		dest += headLen;
		source += headLen;
		len -= headLen;
	}

	while (likely(len >= 128)) {
		prefetch((const void *)source + 512);

		asm volatile(
			"vmovdqa    0(%0), %%ymm0\n"
			"vmovdqa   32(%0), %%ymm1\n"
			"vmovdqa   64(%0), %%ymm2\n"
			"vmovdqa   96(%0), %%ymm3\n"

			"vmovntdq %%ymm0,    0(%1)\n"
			"vmovntdq %%ymm1,   32(%1)\n"
			"vmovntdq %%ymm2,   64(%1)\n"
			"vmovntdq %%ymm3,   96(%1)\n"
			:: "r"(source), "r"(dest)
			: "memory", "ymm0", "ymm1", "ymm2", "ymm3"
		);

		source += 128;
		dest += 128;
		len -= 128;
	}

	while (likely(len >= 32)) {
		asm volatile(
			"vmovdqa  (%0), %%ymm0\n"
			"vmovntdq %%ymm0, (%1)\n"
			:: "r"(source), "r"(dest)
			: "memory", "ymm0"
		);

		source += 32;
		dest += 32;
		len -= 32;
	}

	asm volatile("sfence" ::: "memory");
	asm volatile("vzeroupper" ::: "memory");

	if (len > 0) {
		memcpy((void *)dest, (void *)source, len);
		clean_cache_range((void *)dest, len);
	}
}

static inline void __memcpy_flushcache_std(void *dst, const void *src, size_t size)
{
	unsigned long dest = (unsigned long) dst;
	unsigned long source = (unsigned long) src;
	size_t len = size;

	if (!IS_ALIGNED(dest, 8)) {
		size_t headLen = ALIGN(dest, 8) - dest;
		memcpy((void *)dest, (void *)source, headLen);
		clean_cache_range((void *)dest, headLen);
		dest += headLen;
		source += headLen;
		len -= headLen;
		if (!len)
			return;
	}

	while (likely(len >= 64)) {
		prefetch((const void *)source + 512);

		asm volatile(
			"movq    0(%0), %%r8\n"
			"movq    8(%0), %%r9\n"
			"movq   16(%0), %%r10\n"
			"movq   24(%0), %%r11\n"
			"movnti %%r8,    0(%1)\n"
			"movnti %%r9,    8(%1)\n"
			"movnti %%r10,  16(%1)\n"
			"movnti %%r11,  24(%1)\n"

			"movq   32(%0), %%r8\n"
			"movq   40(%0), %%r9\n"
			"movq   48(%0), %%r10\n"
			"movq   56(%0), %%r11\n"
			"movnti %%r8,   32(%1)\n"
			"movnti %%r9,   40(%1)\n"
			"movnti %%r10,  48(%1)\n"
			"movnti %%r11,  56(%1)\n"
			:: "r"(source), "r"(dest)
			: "memory", "r8", "r9", "r10", "r11"
		);

		source += 64;
		dest += 64;
		len -= 64;
	}

	while (likely(len >= 32)) {
		asm volatile(
			"movq    0(%0), %%r8\n"
			"movq    8(%0), %%r9\n"
			"movq   16(%0), %%r10\n"
			"movq   24(%0), %%r11\n"
			"movnti %%r8,    0(%1)\n"
			"movnti %%r9,    8(%1)\n"
			"movnti %%r10,  16(%1)\n"
			"movnti %%r11,  24(%1)\n"
			:: "r"(source), "r"(dest)
			: "memory", "r8", "r9", "r10", "r11"
		);

		source += 32;
		dest += 32;
		len -= 32;
	}

	while (likely(len >= 8)) {
		asm volatile(
			"movq   (%0), %%r8\n"
			"movnti %%r8,  (%1)\n"
			:: "r"(source), "r"(dest)
			: "memory", "r8"
		);

		source += 8;
		dest += 8;
		len -= 8;
	}

	while (likely(len >= 4)) {
		asm volatile(
			"movl   (%0), %%r8d\n"
			"movnti %%r8d, (%1)\n"
			:: "r"(source), "r"(dest)
			: "memory", "r8"
		);

		source += 4;
		dest += 4;
		len -= 4;
	}

	asm volatile("sfence" ::: "memory");
	asm volatile("vzeroupper" ::: "memory");

	if (len > 0) {
		memcpy((void *)dest, (void *)source, len);
		clean_cache_range((void *)dest, len);
	}
}

void __memcpy_flushcache(void *dst, const void *src, size_t size)
{
	asm goto(ALTERNATIVE("jmp %l[std_path]", "jmp %l[avx2_path]", X86_FEATURE_AVX2)
	:::: std_path, avx2_path);

	avx2_path:
	__memcpy_flushcache_avx2(dst, src, size);
	return;

	std_path:
	__memcpy_flushcache_std(dst, src, size);
}
EXPORT_SYMBOL_GPL(__memcpy_flushcache);
#endif
