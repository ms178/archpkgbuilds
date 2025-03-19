/* SPDX-License-Identifier: GPL-2.0 */
#ifndef _ASM_X86_UACCESS_64_H
#define _ASM_X86_UACCESS_64_H

/*
 * User space memory access functions
 * Optimized for Intel Raptor Lake (AVX2)
 */
#include <linux/compiler.h>
#include <linux/lockdep.h>
#include <linux/kasan-checks.h>
#include <linux/jump_label.h>
#include <asm/alternative.h>
#include <asm/cpufeatures.h>
#include <asm/page.h>
#include <asm/percpu.h>
#include <asm/runtime-const.h>
#include <linux/prefetch.h>

/*
 * Virtual variable: there's no actual backing store for this,
 * it can purely be used as 'runtime_const_ptr(USER_PTR_MAX)'
 */
extern unsigned long USER_PTR_MAX;

/* Set all feature flags to FALSE by default */
static DEFINE_STATIC_KEY_FALSE(fsrm_enabled_key);
static DEFINE_STATIC_KEY_FALSE(fsrs_enabled_key);
static DEFINE_STATIC_KEY_FALSE(avx2_enabled_key);
static DEFINE_STATIC_KEY_TRUE(user_ptr_max_fixed_key);
static DEFINE_STATIC_KEY_FALSE(lam_enabled_key);
static DEFINE_STATIC_KEY_FALSE(features_initialized_key);

static inline void init_lam_feature(void)
{
	if (cpu_feature_enabled(X86_FEATURE_LAM))
		static_branch_enable(&lam_enabled_key);
}

static inline void init_fsrm_feature(void)
{
	if (cpu_feature_enabled(X86_FEATURE_FSRM))
		static_branch_enable(&fsrm_enabled_key);
}

static inline void init_fsrs_feature(void)
{
	if (cpu_feature_enabled(X86_FEATURE_FSRS))
		static_branch_enable(&fsrs_enabled_key);
}

static inline void init_avx2_feature(void)
{
	if (cpu_feature_enabled(X86_FEATURE_AVX2))
		static_branch_enable(&avx2_enabled_key);
}

static inline void init_user_ptr_max(void)
{
	if (runtime_const_ptr(USER_PTR_MAX) != 0x00007fffffffffffUL)
		static_branch_disable(&user_ptr_max_fixed_key);
}

static inline void mark_uaccess_features_initialized(void)
{
	/* Ensure full memory barrier before enabling features */
	smp_mb();
	static_branch_enable(&features_initialized_key);
}

/* Read features with memory barrier for consistency */
static inline bool are_uaccess_features_initialized(void)
{
	bool initialized = static_branch_likely(&features_initialized_key);
	/* Ensure memory barrier when checking feature initialization */
	if (initialized)
		smp_rmb();
	return initialized;
}

#ifdef CONFIG_ADDRESS_MASKING
/*
 * Mask out tag bits from the address.
 */
static inline unsigned long __untagged_addr(unsigned long addr)
{
	unsigned long mask;
	/* Only use LAM if features are initialized */
	if (are_uaccess_features_initialized() &&
		static_branch_likely(&lam_enabled_key)) {
		asm_inline (
			"movq " __percpu_arg([mask]) ", %[mask]\n\t"
			"and %[mask], %[addr]"
			: [addr] "+r" (addr), [mask] "=r" (mask)
			: [mask] "m" (__my_cpu_var(tlbstate_untag_mask)));
		}
		return addr;
}

#define untagged_addr(addr)     ({                                      \
unsigned long __addr = (__force unsigned long)(addr);           \
(__force __typeof__(addr))__untagged_addr(__addr);              \
})

static inline unsigned long __untagged_addr_remote(struct mm_struct *mm,
												   unsigned long addr)
{
	mmap_assert_locked(mm);
	return addr & (mm)->context.untag_mask;
}

#define untagged_addr_remote(mm, addr)  ({                              \
unsigned long __addr = (__force unsigned long)(addr);           \
(__force __typeof__(addr))__untagged_addr_remote(mm, __addr);   \
})

#endif

#define valid_user_address(x) ({                                \
unsigned long __addr = (__force unsigned long)(x);          \
unsigned long __max;                                        \
unsigned long __valid;                                      \
if (static_branch_likely(&user_ptr_max_fixed_key)) {        \
	__max = 0x00007fffffffffffUL;                           \
} else {                                                    \
	__max = runtime_const_ptr(USER_PTR_MAX);                \
}                                                           \
asm_inline ("cmpq %1, %2\n\t"                               \
"setbe %b0"                                     \
: "=q" (__valid)                                \
: "r" (__max), "r" (__addr));                   \
__valid;                                                    \
})

/*
 * User pointers can have tag bits on x86-64. This scheme tolerates
 * arbitrary values in those bits rather then masking them off.
 *
 * Enforce two rules:
 * 1. 'ptr' must be in the user part of the address space
 * 2. 'ptr+size' must not overflow into kernel addresses
 */
static inline bool __access_ok(const void __user *ptr, unsigned long size)
{
	if (unlikely(size == 0))
		return true;

	if (__builtin_constant_p(size <= PAGE_SIZE) && size <= PAGE_SIZE) {
		return valid_user_address(ptr);
	} else {
		unsigned long addr = (__force unsigned long)ptr;
		unsigned long sum = addr + size;
		unsigned long max = runtime_const_ptr(USER_PTR_MAX);
		bool valid;
		asm_inline (
			"cmpq %2, %1\n\t" /* Compare sum with max */
			"setbe %b0\n\t"   /* Set valid if sum <= max */
			"cmpq %1, %3\n\t" /* Compare sum with addr */
			"andb $1, %b0"    /* AND with valid if sum >= addr */
			: "=q" (valid)
			: "r" (sum), "r" (max), "r" (addr));
		return valid;
	}
}
#define __access_ok __access_ok

static inline void __user *mask_user_address(const void __user *ptr)
{
	void __user *ret;
	asm("cmp %1,%0\n\t"
	"cmova %1,%0"
	:"=r" (ret)
	:"r" (runtime_const_ptr(USER_PTR_MAX)),
		"0" (ptr));
	return ret;
}
#define masked_user_access_begin(x) ({                          \
__auto_type __masked_ptr = (x);                         \
__masked_ptr = mask_user_address(__masked_ptr);         \
__uaccess_begin(); __masked_ptr; })

/* Helper function for small, constant-size transfers */
static __always_inline __must_check unsigned long
__copy_user_inline(void *to, const void *from, unsigned long len)
{
	/* Small, constant-size transfer: use inline moves */
	kasan_check_write(to, len);
	kasan_check_read(from, len);

	switch (len) {
		case 0: return 0;
		case 1: *(char *)to = *(char *)from; return 0;
		case 2: *(short *)to = *(short *)from; return 0;
		case 4: *(int *)to = *(int *)from; return 0;
		case 8: *(long *)to = *(long *)from; return 0;
		case 16: {
			/* Optimized 16-byte copy using XMM register */
			asm volatile(
				"movups (%1), %%xmm0\n\t"
				"movups %%xmm0, (%0)\n\t"
				:
				: "r" (to), "r" (from)
				: "memory", "xmm0");
			return 0;
		}
		default:
			return len; /* Fall back to caller for non-constant size */
	}
}

/* Adaptive prefetch for user-to-kernel copies based on transfer size */
static inline void user_access_prefetch(const void *addr, unsigned long len)
{
	/* Only prefetch if we have a significant amount of data */
	if (len < 256)
		return;

	unsigned long prefetch_distance;
	unsigned long prefetch_step;

	if (len >= 8192) {
		/* For very large transfers, use aggressive prefetching */
		prefetch_distance = 512;
		prefetch_step = 128;
	} else if (len >= 1024) {
		/* For medium transfers */
		prefetch_distance = 256;
		prefetch_step = 64;
	} else {
		/* For smaller transfers */
		prefetch_distance = 128;
		prefetch_step = 64;
	}

	/* Prefetch with temporal locality for normal copies */
	unsigned long i;
	for (i = 0; i < len && i < prefetch_distance; i += prefetch_step) {
		asm volatile("prefetcht0 %0" : : "m" (*(const char *)(addr + i)));
	}
}

/* Handle early returns for zero-length operations */
static __always_inline __must_check unsigned long
copy_user_generic(void *to, const void *from, unsigned long len)
{
	/* Early return for zero-length copy */
	if (unlikely(len == 0))
		return 0;

	unsigned long orig_len = len;
	unsigned long bytes_copied = 0;
	char *current_dst = to;
	const char *current_src = from;

	/* Validate with KASAN for kernel side */
	kasan_check_write(to, len);
	kasan_check_read(from, len);

	/* For constant small sizes, try to handle without UACCESS state changes */
	if (__builtin_constant_p(len) && len <= 16) {
		unsigned long inline_ret = __copy_user_inline(to, from, len);
		if (inline_ret == 0)
			return 0;
	}

	stac();

	/* Only use optimized path if features are initialized */
	if (are_uaccess_features_initialized() &&
		static_branch_likely(&fsrm_enabled_key)) {

		if (__builtin_constant_p(len) && len <= 16) {
			/* Already tried inline above, use rep movsb with exception handling */
			unsigned long rem = len;
			asm volatile(
				"1:\n\t"
				"rep movsb\n\t"
				"2:\n\t"
				_ASM_EXTABLE_UA(1b, 2b)
				: "+c" (rem), "+D" (current_dst), "+S" (current_src)
				: : "memory");
			bytes_copied = len - rem;
		} else if (len <= 16) {
			/* Small transfer with exception handling */
			unsigned long rem = len;
			asm volatile(
				"1:\n\t"
				"rep movsb\n\t"
				"2:\n\t"
				_ASM_EXTABLE_UA(1b, 2b)
				: "+c" (rem), "+D" (current_dst), "+S" (current_src)
				: : "memory");
			bytes_copied = len - rem;
		} else if (len >= 4096) {
			/* Large transfer with prefetching and exception handling */
			user_access_prefetch(current_src, len);

			if (len >= 64 && !((unsigned long)current_dst & 7) && !((unsigned long)current_src & 7)) {
				/* Aligned large transfer: use rep movsq + remainder */
				unsigned long qwords = len >> 3;
				unsigned long remainder = len & 7;
				unsigned long orig_qwords = qwords;

				asm volatile(
					"1:\n\t"
					"rep movsq\n\t"
					"2:\n\t"
					_ASM_EXTABLE_UA(1b, 2b)
					: "+c" (qwords), "+D" (current_dst), "+S" (current_src)
					: : "memory");

				/* Calculate bytes moved */
				unsigned long qword_bytes = (orig_qwords - qwords) << 3;
				bytes_copied = qword_bytes;

				/* Only process remainder if all qwords were copied */
				if (likely(qwords == 0) && remainder > 0) {
					/* Handle remainder bytes with exception handling */
					unsigned long rem = remainder;
					asm volatile(
						"1:\n\t"
						"rep movsb\n\t"
						"2:\n\t"
						_ASM_EXTABLE_UA(1b, 2b)
						: "+c" (rem), "+D" (current_dst), "+S" (current_src)
						: : "memory");
					bytes_copied += (remainder - rem);
				}
			} else {
				/* Unaligned large transfer: use rep movsb with exception handling */
				unsigned long rem = len;
				asm volatile(
					"1:\n\t"
					"rep movsb\n\t"
					"2:\n\t"
					_ASM_EXTABLE_UA(1b, 2b)
					: "+c" (rem), "+D" (current_dst), "+S" (current_src)
					: : "memory");
				bytes_copied = len - rem;
			}
		} else if (len >= 64 && !((unsigned long)current_dst & 7) && !((unsigned long)current_src & 7)) {
			/* Aligned medium transfer: use rep movsq with exception handling */
			unsigned long qwords = len >> 3;
			unsigned long remainder = len & 7;
			unsigned long orig_qwords = qwords;

			asm volatile(
				"1:\n\t"
				"rep movsq\n\t"
				"2:\n\t"
				_ASM_EXTABLE_UA(1b, 2b)
				: "+c" (qwords), "+D" (current_dst), "+S" (current_src)
				: : "memory");

			/* Calculate bytes moved */
			unsigned long qword_bytes = (orig_qwords - qwords) << 3;
			bytes_copied = qword_bytes;

			/* Only process remainder if all qwords were copied */
			if (likely(qwords == 0) && remainder > 0) {
				/* Handle remainder with exception handling */
				unsigned long rem = remainder;
				asm volatile(
					"1:\n\t"
					"rep movsb\n\t"
					"2:\n\t"
					_ASM_EXTABLE_UA(1b, 2b)
					: "+c" (rem), "+D" (current_dst), "+S" (current_src)
					: : "memory");
				bytes_copied += (remainder - rem);
			}
		} else {
			/* Unaligned medium transfer: use rep movsb with exception handling */
			unsigned long rem = len;
			asm volatile(
				"1:\n\t"
				"rep movsb\n\t"
				"2:\n\t"
				_ASM_EXTABLE_UA(1b, 2b)
				: "+c" (rem), "+D" (current_dst), "+S" (current_src)
				: : "memory");
			bytes_copied = len - rem;
		}
		} else {
			/* Non-FSRM fallback */
			unsigned long ret = len;
			asm volatile(
				"1:\n\t"
				"call rep_movs_alternative\n\t"
				"2:\n"
				_ASM_EXTABLE_UA(1b, 2b)
				: "+c" (ret), "+D" (current_dst), "+S" (current_src), ASM_CALL_CONSTRAINT
				: : "memory", "rax", "rdx", "r8");
			bytes_copied = orig_len - ret;
		}

		clac();
		return orig_len - bytes_copied; /* Return bytes not copied */
}

static __always_inline __must_check unsigned long
raw_copy_from_user(void *dst, const void __user *src, unsigned long size)
{
	if (unlikely(!__access_ok(src, size)))
		return size;
	return copy_user_generic(dst, (__force void *)src, size);
}

static __always_inline __must_check unsigned long
raw_copy_to_user(void __user *dst, const void *src, unsigned long size)
{
	if (unlikely(!__access_ok(dst, size)))
		return size;
	return copy_user_generic((__force void *)dst, src, size);
}

extern long __copy_user_nocache(void *dst, const void __user *src, unsigned size);
extern long __copy_user_flushcache(void *dst, const void __user *src, unsigned size);

/* Optimized non-temporal copy with AVX2 */
static inline int
__copy_from_user_inatomic_nocache(void *dst, const void __user *src,
								  unsigned size)
{
	/* Early return for zero-length copy */
	if (unlikely(size == 0))
		return 0;

	/* Validate the user pointer */
	if (unlikely(!__access_ok(src, size)))
		return size;

	unsigned long bytes_copied = 0;
	kasan_check_write(dst, size);

	/* Only use AVX2 if features are initialized and conditions are right */
	if (are_uaccess_features_initialized() &&
		static_branch_likely(&avx2_enabled_key) &&
		size >= 32 &&
		!((unsigned long)src & 31) &&
		!((unsigned long)dst & 31)) {

		/* Aligned, large transfer: use AVX2 */
		unsigned long vector_chunks = size >> 5; /* 32-byte chunks */
		unsigned long remainder = size & 31;

	stac();

	if (vector_chunks) {
		/* Use AVX2 for 32-byte chunks */
		unsigned long avx_chunks_left = vector_chunks;
		char *avx_dst = (char *)dst;
		const char __user *avx_src = src;

		asm volatile(
			"1:\n\t"
			"vmovdqa (%1), %%ymm0\n\t"      /* Load 32 bytes from src */
			"vmovntdq %%ymm0, (%0)\n\t"     /* Non-temporal store to dst */
			"add $32, %0\n\t"
			"add $32, %1\n\t"
			"dec %2\n\t"
			"jnz 1b\n\t"
			"2:\n\t"
			_ASM_EXTABLE_UA(1b, 2b)
			: "+r" (avx_dst), "+r" (avx_src), "+r" (avx_chunks_left)
			: : "memory", "ymm0");

		/* Clean up AVX state */
		asm volatile("vzeroupper" ::: "memory");

		/* Calculate bytes copied with AVX2 */
		bytes_copied = (vector_chunks - avx_chunks_left) << 5;

		/* If all chunks were processed and we have remainder */
		if (likely(avx_chunks_left == 0) && remainder > 0) {
			/* Recalculate correct pointers for remainder handling */
			void *rem_dst = (char *)dst + bytes_copied;
			const void __user *rem_src = (const char __user *)src + bytes_copied;

			/* Handle remainder */
			clac();  /* Disable user access before calling function */
			long ret = __copy_user_nocache(rem_dst, rem_src, remainder);

			if (ret == 0) {
				/* All remainder bytes copied */
				bytes_copied += remainder;
			} else {
				/* Some remainder bytes not copied */
				bytes_copied += remainder - ret;
			}
			return size - bytes_copied;
		}
	} else if (remainder) {
		/* No full 32-byte chunks, just handle remainder */
		clac();
		long ret = __copy_user_nocache(dst, src, remainder);

		if (ret == 0) {
			bytes_copied = remainder;
		} else {
			bytes_copied = remainder - ret;
		}
		return size - bytes_copied;
	}

	clac();
	return size - bytes_copied;
		} else {
			/* Non-AVX2 path - straight to __copy_user_nocache */
			return __copy_user_nocache(dst, src, size);
		}
}

static inline int
__copy_from_user_flushcache(void *dst, const void __user *src, unsigned size)
{
	if (unlikely(!__access_ok(src, size)))
		return size;
	kasan_check_write(dst, size);
	return __copy_user_flushcache(dst, src, size);
}

/*
 * Zero Userspace.
 */

__must_check unsigned long
rep_stos_alternative(void __user *addr, unsigned long len);

/* Fixed __clear_user function */
static __always_inline __must_check unsigned long
__clear_user(void __user *addr, unsigned long size)
{
	/* Early return for zero-length clear */
	if (unlikely(size == 0))
		return 0;

	unsigned long orig_size = size;
	unsigned long bytes_cleared = 0;
	void __user *current_addr = addr;

	might_fault();
	stac();

	/* Only use optimized path if features are initialized */
	if (are_uaccess_features_initialized() &&
		static_branch_likely(&fsrs_enabled_key) &&
		size >= 64 &&
		!((unsigned long)addr & 7)) {

		/* Aligned, large clear: use rep stosq */
		unsigned long qwords = size >> 3;
	unsigned long remainder = size & 7;

	if (likely(qwords > 0)) {
		/* Clear qwords first */
		unsigned long qwords_left = qwords;

		asm volatile(
			"1:\n\t"
			"rep stosq\n\t"
			"2:\n\t"
			_ASM_EXTABLE_UA(1b, 2b)
			: "+c" (qwords_left), "+D" (current_addr)
			: "a" (0)
			: "memory");

		/* Calculate bytes cleared */
		unsigned long qword_bytes = (qwords - qwords_left) << 3;
		bytes_cleared = qword_bytes;

		/* Only process remainder if all qwords were cleared */
		if (likely(qwords_left == 0) && remainder > 0) {
			/* Clear remainder bytes */
			unsigned long rem = remainder;
			asm volatile(
				"1:\n\t"
				"rep stosb\n\t"
				"2:\n\t"
				_ASM_EXTABLE_UA(1b, 2b)
				: "+c" (rem), "+D" (current_addr)
				: "a" (0)
				: "memory");

			bytes_cleared += (remainder - rem);
		}
	} else {
		/* No qwords, just clear bytes */
		unsigned long rem = size;
		asm volatile(
			"1:\n\t"
			"rep stosb\n\t"
			"2:\n\t"
			_ASM_EXTABLE_UA(1b, 2b)
			: "+c" (rem), "+D" (current_addr)
			: "a" (0)
			: "memory");

		bytes_cleared = size - rem;
	}
		} else {
			/* Unaligned or small clear: use rep_stos_alternative */
			unsigned long rem = size;

			asm volatile(
				"1:\n\t"
				"call rep_stos_alternative\n\t"
				"2:\n"
				_ASM_EXTABLE_UA(1b, 2b)
				: "+c" (rem), "+D" (current_addr), ASM_CALL_CONSTRAINT
				: "a" (0)
				: "memory");

			bytes_cleared = size - rem;
		}

		clac();

		/* Return number of bytes not cleared */
		return orig_size - bytes_cleared;
}

static __always_inline unsigned long clear_user(void __user *to, unsigned long n)
{
	if (unlikely(!__access_ok(to, n)))
		return n;
	return __clear_user(to, n);
}
#endif /* _ASM_X86_UACCESS_64_H */
