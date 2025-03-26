// SPDX-License-Identifier: GPL-2.0-only
/*
 * User address space access functions.
 * Optimized for Intel Raptor Lake architecture.
 * Provides copy functions that ensure data is flushed for persistence (e.g., PMEM).
 */
#include <linux/export.h>
#include <linux/uaccess.h>
#include <linux/highmem.h>
#include <linux/libnvdimm.h>
#include <linux/string.h>
#include <linux/kernel.h>
#include <asm/cpufeature.h>
#include <asm/processor.h>
#include <asm/cacheflush.h>
#include <asm/fpu/api.h>
#include <asm/alternative.h>
#include <asm/barrier.h> // Include for sfence() macro potentially used elsewhere, though we use inline asm here.

// Function Prototypes
static inline void clean_cache_range(void *addr, size_t size);
static inline void __memcpy_flushcache_avx2(void *dst, const void *src, size_t size);
static inline void __memcpy_flushcache_std(void *dst, const void *src, size_t size);
void arch_wb_cache_pmem(void *addr, size_t size);
long __copy_user_flushcache(void *dst, const void __user *src, unsigned size);
void __memcpy_flushcache(void *dst, const void *src, size_t size);


#ifdef CONFIG_ARCH_HAS_UACCESS_FLUSHCACHE

/**
 * clean_cache_range - write back and invalidate cache range with CLWB + sfence
 * @addr:	start address
 * @size:	number of bytes
 */
static inline void clean_cache_range(void *addr, size_t size)
{
	if (unlikely(size == 0))
		return;

	if (!static_cpu_has(X86_FEATURE_CLWB)) {
		WARN_ONCE(1, FW_BUG "CLWB feature missing despite CONFIG_ARCH_HAS_UACCESS_FLUSHCACHE");
		return;
	}

	u16 clflush_size = boot_cpu_data.x86_clflush_size;
	if (unlikely(!clflush_size || !is_power_of_2(clflush_size))) {
		WARN_ONCE(1, "Invalid x86_clflush_size: %u", clflush_size);
		clflush_size = 64;
	}
	unsigned long clflush_mask = clflush_size - 1;
	char *p = (char *)((unsigned long)addr & ~clflush_mask);
	char *vend = (char *)addr + size;

	while (likely(p + 4 * clflush_size <= vend)) {
		clwb(p);
		clwb(p + clflush_size);
		clwb(p + 2 * clflush_size);
		clwb(p + 3 * clflush_size);
		p += 4 * clflush_size;
	}
	if (p + 2 * clflush_size <= vend) {
		clwb(p);
		clwb(p + clflush_size);
		p += 2 * clflush_size;
	}
	if (p < vend) {
		clwb(p);
	}

	// SFENCE: Ensure CLWB operations complete for persistence.
	asm volatile("sfence" ::: "memory");
}

/**
 * arch_wb_cache_pmem - Write back cache lines for persistent memory
 * @addr:	start address
 * @size:	number of bytes
 */
void arch_wb_cache_pmem(void *addr, size_t size)
{
	clean_cache_range(addr, size);
}
EXPORT_SYMBOL_GPL(arch_wb_cache_pmem);

/**
 * __copy_user_flushcache - Copy from user, ensuring data hits persistence.
 * @dst:   Destination address (kernel space, PMEM).
 * @src:   Source address (user space).
 * @size:  Number of bytes to copy.
 *
 * Returns: Number of bytes NOT copied (0 on success).
 */
long __copy_user_flushcache(void *dst, const void __user *src, unsigned size)
{
	long rc;

	if (unlikely(size == 0))
		return 0;

	// Perform copy from user; __copy_user_nocache handles access_ok & faults
	stac();
	rc = __copy_user_nocache(dst, src, size);
	clac();

	unsigned long bytes_copied = size - rc;

	// Flush the destination range that was successfully copied
	if (likely(bytes_copied > 0)) {
		clean_cache_range(dst, bytes_copied); // Includes sfence
	}

	return rc;
}

/**
 * __memcpy_flushcache_avx2 - memcpy using AVX2 NT stores + flush.
 * @dst:   Destination address. Requires 32B alignment for main loops.
 * @src:   Source address. Can be unaligned.
 * @size:  Number of bytes to copy.
 */
static inline void __memcpy_flushcache_avx2(void *dst, const void *src, size_t size)
{
	char *d = (char *)dst;
	const char *s = (const char *)src;
	size_t len = size;

	// Fallback for smaller sizes
	if (len < 256) {
		__memcpy_flushcache_std(dst, src, size);
		return;
	}

	kernel_fpu_begin();

	// Align destination to 32 bytes for vmovntdq
	if (unlikely(!IS_ALIGNED((unsigned long)d, 32))) {
		size_t head_len = ALIGN((unsigned long)d, 32) - (unsigned long)d;
		head_len = min(head_len, len);
		memcpy(d, s, head_len);
		clean_cache_range(d, head_len);
		d += head_len;
		s += head_len;
		len -= head_len;
		if (unlikely(len == 0))
			goto end_avx;
	}

	// Main loop: process 256 bytes (8x YMM) per iteration
	while (likely(len >= 256)) {
		prefetch((const void *)(s + 768));

		asm volatile(
			"vmovdqu    0(%[src]), %%ymm0\n"
			"vmovdqu   32(%[src]), %%ymm1\n"
			"vmovdqu   64(%[src]), %%ymm2\n"
			"vmovdqu   96(%[src]), %%ymm3\n"
			"vmovdqu  128(%[src]), %%ymm4\n"
			"vmovdqu  160(%[src]), %%ymm5\n"
			"vmovdqu  192(%[src]), %%ymm6\n"
			"vmovdqu  224(%[src]), %%ymm7\n"
			"vmovntdq %%ymm0,    0(%[dst])\n"
			"vmovntdq %%ymm1,   32(%[dst])\n"
			"vmovntdq %%ymm2,   64(%[dst])\n"
			"vmovntdq %%ymm3,   96(%[dst])\n"
			"vmovntdq %%ymm4,  128(%[dst])\n"
			"vmovntdq %%ymm5,  160(%[dst])\n"
			"vmovntdq %%ymm6,  192(%[dst])\n"
			"vmovntdq %%ymm7,  224(%[dst])\n"
			: : [src]"r"(s), [dst]"r"(d)
			: "memory", "ymm0", "ymm1", "ymm2", "ymm3", "ymm4", "ymm5", "ymm6", "ymm7"
		);

		s += 256;
		d += 256;
		len -= 256;
	}

	// Handle remaining 128B chunks
	if (len >= 128) {
		asm volatile(
			"vmovdqu    0(%[src]), %%ymm0\n"
			"vmovdqu   32(%[src]), %%ymm1\n"
			"vmovdqu   64(%[src]), %%ymm2\n"
			"vmovdqu   96(%[src]), %%ymm3\n"
			"vmovntdq %%ymm0,    0(%[dst])\n"
			"vmovntdq %%ymm1,   32(%[dst])\n"
			"vmovntdq %%ymm2,   64(%[dst])\n"
			"vmovntdq %%ymm3,   96(%[dst])\n"
			: : [src]"r"(s), [dst]"r"(d)
			: "memory", "ymm0", "ymm1", "ymm2", "ymm3"
		);
		s += 128;
		d += 128;
		len -= 128;
	}

	// Handle remaining 64B chunks
	if (len >= 64) {
		asm volatile(
			"vmovdqu    0(%[src]), %%ymm0\n"
			"vmovdqu   32(%[src]), %%ymm1\n"
			"vmovntdq %%ymm0,    0(%[dst])\n"
			"vmovntdq %%ymm1,   32(%[dst])\n"
			: : [src]"r"(s), [dst]"r"(d)
			: "memory", "ymm0", "ymm1"
		);
		s += 64;
		d += 64;
		len -= 64;
	}

	// Handle remaining 32B chunks
	if (len >= 32) {
		asm volatile(
			"vmovdqu  (%[src]), %%ymm0\n"
			"vmovntdq %%ymm0, (%[dst])\n"
			: : [src]"r"(s), [dst]"r"(d)
			: "memory", "ymm0"
		);
		s += 32;
		d += 32;
		len -= 32;
	}

	// Ensure NT stores complete
	asm volatile("sfence" ::: "memory");

	end_avx:
	// Avoid AVX-SSE transition penalty
	asm volatile("vzeroupper" ::: "memory");
	kernel_fpu_end();

	// Handle final tail (< 32 bytes)
	if (len > 0) {
		memcpy(d, s, len);
		clean_cache_range(d, len); // Includes sfence
	}
}


/**
 * __memcpy_flushcache_std - memcpy using GPR NT stores + flush (No AVX2).
 * @dst:   Destination address. Requires 8B alignment for main loops.
 * @src:   Source address.
 * @size:  Number of bytes to copy.
 */
static inline void __memcpy_flushcache_std(void *dst, const void *src, size_t size)
{
	char *d = (char *)dst;
	const char *s = (const char *)src;
	size_t len = size;

	// Align destination to 8 bytes for movnti
	if (unlikely(!IS_ALIGNED((unsigned long)d, 8))) {
		size_t head_len = ALIGN((unsigned long)d, 8) - (unsigned long)d;
		head_len = min(head_len, len);
		memcpy(d, s, head_len);
		clean_cache_range(d, head_len); // Includes sfence
		d += head_len;
		s += head_len;
		len -= head_len;
		if (unlikely(len == 0))
			return;
	}

	// Process 64 bytes (8 qwords) per iteration
	while (likely(len >= 64)) {
		prefetch((const void *)(s + 512));

		asm volatile(
			"movq    0(%[src]), %%r8\n"
			"movq    8(%[src]), %%r9\n"
			"movq   16(%[src]), %%r10\n"
			"movq   24(%[src]), %%r11\n"
			"movnti %%r8,    0(%[dst])\n"
			"movnti %%r9,    8(%[dst])\n"
			"movnti %%r10,  16(%[dst])\n"
			"movnti %%r11,  24(%[dst])\n"
			"movq   32(%[src]), %%r12\n"
			"movq   40(%[src]), %%r13\n"
			"movq   48(%[src]), %%r14\n"
			"movq   56(%[src]), %%r15\n"
			"movnti %%r12,  32(%[dst])\n"
			"movnti %%r13,  40(%[dst])\n"
			"movnti %%r14,  48(%[dst])\n"
			"movnti %%r15,  56(%[dst])\n"
			: : [src]"r"(s), [dst]"r"(d)
			: "memory", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"
		);

		s += 64;
		d += 64;
		len -= 64;
	}

	// Handle remaining 32B chunks
	if (len >= 32) {
		asm volatile(
			"movq    0(%[src]), %%r8\n"
			"movq    8(%[src]), %%r9\n"
			"movq   16(%[src]), %%r10\n"
			"movq   24(%[src]), %%r11\n"
			"movnti %%r8,    0(%[dst])\n"
			"movnti %%r9,    8(%[dst])\n"
			"movnti %%r10,  16(%[dst])\n"
			"movnti %%r11,  24(%[dst])\n"
			: : [src]"r"(s), [dst]"r"(d)
			: "memory", "r8", "r9", "r10", "r11"
		);
		s += 32;
		d += 32;
		len -= 32;
	}

	// Handle remaining 16B chunks
	if (len >= 16) {
		asm volatile(
			"movq    0(%[src]), %%r8\n"
			"movq    8(%[src]), %%r9\n"
			"movnti %%r8,    0(%[dst])\n"
			"movnti %%r9,    8(%[dst])\n"
			: : [src]"r"(s), [dst]"r"(d)
			: "memory", "r8", "r9"
		);
		s += 16;
		d += 16;
		len -= 16;
	}

	// Handle remaining 8B chunk
	if (len >= 8) {
		asm volatile(
			"movq   (%[src]), %%r8\n"
			"movnti %%r8,  (%[dst])\n"
			: : [src]"r"(s), [dst]"r"(d)
			: "memory", "r8"
		);
		s += 8;
		d += 8;
		len -= 8;
	}

	// Ensure NT stores complete
	asm volatile("sfence" ::: "memory");

	// Handle final tail (< 8 bytes)
	if (len > 0) {
		memcpy(d, s, len);
		clean_cache_range(d, len); // Includes sfence
	}
}

/**
 * __memcpy_flushcache - memcpy wrapper selecting AVX2 or standard GPR path.
 * @dst:   Destination address (kernel space, likely PMEM).
 * @src:   Source address (kernel space).
 * @size:  Number of bytes to copy.
 */
void __memcpy_flushcache(void *dst, const void *src, size_t size)
{
	if (unlikely(size == 0))
		return;

	// Select implementation based on AVX2 feature
	asm goto(ALTERNATIVE("jmp %l[std_path]",
						 "jmp %l[avx2_path]",
					  X86_FEATURE_AVX2)
	: : : : std_path, avx2_path);

	avx2_path:
	__memcpy_flushcache_avx2(dst, src, size);
	return;

	std_path:
	__memcpy_flushcache_std(dst, src, size);
	return;
}
EXPORT_SYMBOL_GPL(__memcpy_flushcache);

#endif /* CONFIG_ARCH_HAS_UACCESS_FLUSHCACHE */
