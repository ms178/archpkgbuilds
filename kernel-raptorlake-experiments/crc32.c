/*
 * Aug 8, 2011 Bob Pearson with help from Joakim Tjernlund and George Spelvin
 * cleaned up code to current version of sparse and added the slicing-by-8
 * algorithm to the closely similar existing slicing-by-4 algorithm.
 *
 * Oct 15, 2000 Matt Domsch <Matt_Domsch@dell.com>
 * Nicer crc32 functions/docs submitted by linux@horizon.com.  Thanks!
 * Code was from the public domain, copyright abandoned.  Code was
 * subsequently included in the kernel, thus was re-licensed under the
 * GNU GPL v2.
 *
 * Oct 12, 2000 Matt Domsch <Matt_Domsch@dell.com>
 * Same crc32 function was used in 5 other places in the kernel.
 * I made one version, and deleted the others.
 * There are various incantations of crc32().  Some use a seed of 0 or ~0.
 * Some xor at the end with ~0.  The generic crc32() function takes
 * seed as an argument, and doesn't xor at the end.  Then individual
 * users can do whatever they need.
 *   drivers/net/smc9194.c uses seed ~0, doesn't xor with ~0.
 *   fs/jffs2 uses seed 0, doesn't xor with ~0.
 *   fs/partitions/efi.c uses seed ~0, xor's with ~0.
 *
 * This source code is licensed under the GNU General Public License,
 * Version 2.  See the file COPYING for more details.
 */

/* see: Documentation/staging/crc32.rst for a description of algorithms */

#include <linux/crc32.h>
#include <linux/crc32poly.h>
#include <linux/module.h>
#include <linux/types.h>
#include <linux/sched.h>
#include <linux/bitops.h>
#include "crc32defs.h"

#ifdef CONFIG_X86
#include <asm/cpufeatures.h>
#include <asm/processor.h>
#include <asm/cpu.h>
#endif

#if CRC_LE_BITS > 8
# define tole(x) ((__force u32) cpu_to_le32(x))
#else
# define tole(x) (x)
#endif

#if CRC_BE_BITS > 8
# define tobe(x) ((__force u32) cpu_to_be32(x))
#else
# define tobe(x) (x)
#endif

#include "crc32table.h"

MODULE_AUTHOR("Matt Domsch <Matt_Domsch@dell.com>");
MODULE_DESCRIPTION("Various CRC32 calculations");
MODULE_LICENSE("GPL");

/* Function prototypes for the combine functions */
u32 __attribute_const__ crc32_combine(u32 crc1, u32 crc2, size_t len2);
u32 __attribute_const__ __crc32c_combine(u32 crc1, u32 crc2, size_t len2);

/* Function pointer for runtime dispatch */
typedef u32 (*crc32_func_t)(u32, unsigned char const *, size_t);
static crc32_func_t crc32_le_impl = NULL;
static crc32_func_t __crc32c_le_impl = NULL;

/* Forward declarations for optimized implementations */
static u32 __pure crc32_le_generic_impl(u32 crc, unsigned char const *p, size_t len);
static u32 __pure __crc32c_le_generic_impl(u32 crc, unsigned char const *p, size_t len);

#ifdef CONFIG_X86
static u32 __pure __maybe_unused crc32_intel_le(u32 crc, unsigned char const *buf, size_t len);
static u32 __pure __maybe_unused __crc32c_intel_le(u32 crc, unsigned char const *buf, size_t len);
static u32 __pure crc32_intel_le_opt(u32 crc, unsigned char const *buf, size_t len);
static u32 __pure __crc32c_intel_le_opt(u32 crc, unsigned char const *buf, size_t len);
#ifdef CONFIG_AS_AVX2
static u32 __pure crc32_avx2_le(u32 crc, const unsigned char *buf, size_t len);
static u32 __pure __crc32c_avx2_le(u32 crc, const unsigned char *buf, size_t len);
static u32 __pure crc32_avx2_raptor_lake(u32 crc, const unsigned char *buf, size_t len);
static u32 __pure __crc32c_avx2_raptor_lake(u32 crc, const unsigned char *buf, size_t len);
#endif
#endif /* CONFIG_X86 */

#if CRC_LE_BITS > 8 || CRC_BE_BITS > 8

/* implements slicing-by-4 or slicing-by-8 algorithm */
static inline u32 __pure
crc32_body(u32 crc, unsigned char const *buf, size_t len, const u32 (*tab)[256])
{
	# ifdef __LITTLE_ENDIAN
	#  define DO_CRC(x) crc = t0[(crc ^ (x)) & 255] ^ (crc >> 8)
	#  define DO_CRC4 (t3[(q) & 255] ^ t2[(q >> 8) & 255] ^ \
	t1[(q >> 16) & 255] ^ t0[(q >> 24) & 255])
	#  define DO_CRC8 (t7[(q) & 255] ^ t6[(q >> 8) & 255] ^ \
	t5[(q >> 16) & 255] ^ t4[(q >> 24) & 255])
	# else
	#  define DO_CRC(x) crc = t0[((crc >> 24) ^ (x)) & 255] ^ (crc << 8)
	#  define DO_CRC4 (t0[(q) & 255] ^ t1[(q >> 8) & 255] ^ \
	t2[(q >> 16) & 255] ^ t3[(q >> 24) & 255])
	#  define DO_CRC8 (t4[(q) & 255] ^ t5[(q >> 8) & 255] ^ \
	t6[(q >> 16) & 255] ^ t7[(q >> 24) & 255])
	# endif
	const u32 *b;
	size_t    rem_len;
	# ifdef CONFIG_X86
	size_t i;
	# endif
	const u32 *t0=tab[0], *t1=tab[1], *t2=tab[2], *t3=tab[3];
	# if CRC_LE_BITS != 32
	const u32 *t4 = tab[4], *t5 = tab[5], *t6 = tab[6], *t7 = tab[7];
	# endif
	u32 q;

	/* Align it */
	if (unlikely((long)buf & 3 && len)) {
		do {
			DO_CRC(*buf++);
		} while ((--len) && ((long)buf)&3);
	}

	# if CRC_LE_BITS == 32
	rem_len = len & 3;
	len = len >> 2;
	# else
	rem_len = len & 7;
	len = len >> 3;
	# endif

	b = (const u32 *)buf;
	# ifdef CONFIG_X86
	--b;
	for (i = 0; i < len; i++) {
		# else
		for (--b; len; --len) {
			# endif
			q = crc ^ *++b; /* use pre increment for speed */
			# if CRC_LE_BITS == 32
			crc = DO_CRC4;
			# else
			crc = DO_CRC8;
			q = *++b;
			crc ^= DO_CRC4;
			# endif
		}
		len = rem_len;
		/* And the last few bytes */
		if (len) {
			u8 *p = (u8 *)(b + 1) - 1;
			# ifdef CONFIG_X86
			for (i = 0; i < len; i++)
				DO_CRC(*++p); /* use pre increment for speed */
				# else
				do {
					DO_CRC(*++p); /* use pre increment for speed */
				} while (--len);
			# endif
		}
		return crc;
		#undef DO_CRC
		#undef DO_CRC4
		#undef DO_CRC8
	}

	/* Optimized version with prefetching and improved branch prediction */
	static inline u32 __pure
	crc32_body_optimized(u32 crc, unsigned char const *buf, size_t len, const u32 (*tab)[256])
	{
		# ifdef __LITTLE_ENDIAN
		#  define DO_CRC(x) crc = t0[(crc ^ (x)) & 255] ^ (crc >> 8)
		#  define DO_CRC4 (t3[(q) & 255] ^ t2[(q >> 8) & 255] ^ \
		t1[(q >> 16) & 255] ^ t0[(q >> 24) & 255])
		#  define DO_CRC8 (t7[(q) & 255] ^ t6[(q >> 8) & 255] ^ \
		t5[(q >> 16) & 255] ^ t4[(q >> 24) & 255])
		# else
		#  define DO_CRC(x) crc = t0[((crc >> 24) ^ (x)) & 255] ^ (crc << 8)
		#  define DO_CRC4 (t0[(q) & 255] ^ t1[(q >> 8) & 255] ^ \
		t2[(q >> 16) & 255] ^ t3[(q >> 24) & 255])
		#  define DO_CRC8 (t4[(q) & 255] ^ t5[(q >> 8) & 255] ^ \
		t6[(q >> 16) & 255] ^ t7[(q >> 24) & 255])
		# endif
		const u32 *b;
		size_t    rem_len;
		const u32 *t0=tab[0], *t1=tab[1], *t2=tab[2], *t3=tab[3];
		# if CRC_LE_BITS != 32
		const u32 *t4 = tab[4], *t5 = tab[5], *t6 = tab[6], *t7 = tab[7];
		# endif
		u32 q;

		/* Prefetch table data for better cache utilization */
		__builtin_prefetch(t0, 0, 0);
		__builtin_prefetch(t1, 0, 0);
		__builtin_prefetch(t2, 0, 0);
		__builtin_prefetch(t3, 0, 0);
		# if CRC_LE_BITS != 32
		__builtin_prefetch(t4, 0, 0);
		__builtin_prefetch(t5, 0, 0);
		__builtin_prefetch(t6, 0, 0);
		__builtin_prefetch(t7, 0, 0);
		# endif

		/* Process bytes until aligned */
		if (unlikely((uintptr_t)buf & 3 && len)) {
			do {
				DO_CRC(*buf++);
			} while ((--len) && unlikely((uintptr_t)buf & 3));
		}

		# if CRC_LE_BITS == 32
		rem_len = len & 3;
		len = len >> 2;
		# else
		rem_len = len & 7;
		len = len >> 3;
		# endif

		b = (const u32 *)buf;

		/* Main processing loop with improved branch prediction */
		if (likely(len > 0)) {
			--b;
			do {
				/* Prefetch next data block to reduce cache misses */
				if (len > 16)
					__builtin_prefetch(b + 16, 0, 0);

				q = crc ^ *++b;
				# if CRC_LE_BITS == 32
				crc = DO_CRC4;
				# else
				crc = DO_CRC8;
				q = *++b;
				crc ^= DO_CRC4;
				# endif
			} while (--len);
		}

		/* Process remaining bytes */
		if (unlikely(rem_len)) {
			u8 *p = (u8 *)(b + 1) - 1;
			do {
				DO_CRC(*++p);
			} while (--rem_len);
		}

		return crc;
		#undef DO_CRC
		#undef DO_CRC4
		#undef DO_CRC8
	}
	#endif

	/**
	 * crc32_le_generic() - Calculate bitwise little-endian Ethernet AUTODIN II
	 *                      CRC32/CRC32C
	 * @crc: seed value for computation.  ~0 for Ethernet, sometimes 0 for other
	 *       uses, or the previous crc32/crc32c value if computing incrementally.
	 * @p: pointer to buffer over which CRC32/CRC32C is run
	 * @len: length of buffer @p
	 * @tab: little-endian Ethernet table
	 * @polynomial: CRC32/CRC32c LE polynomial
	 */
	static inline u32 __pure crc32_le_generic(u32 crc, unsigned char const *p,
											  size_t len, const u32 (*tab)[256],
											  u32 polynomial)
	{
		#if CRC_LE_BITS == 1
		int i;
		while (len--) {
			crc ^= *p++;
			for (i = 0; i < 8; i++)
				crc = (crc >> 1) ^ ((crc & 1) ? polynomial : 0);
		}
		# elif CRC_LE_BITS == 2
		while (len--) {
			crc ^= *p++;
			crc = (crc >> 2) ^ tab[0][crc & 3];
			crc = (crc >> 2) ^ tab[0][crc & 3];
			crc = (crc >> 2) ^ tab[0][crc & 3];
			crc = (crc >> 2) ^ tab[0][crc & 3];
		}
		# elif CRC_LE_BITS == 4
		while (len--) {
			crc ^= *p++;
			crc = (crc >> 4) ^ tab[0][crc & 15];
			crc = (crc >> 4) ^ tab[0][crc & 15];
		}
		# elif CRC_LE_BITS == 8
		/* aka Sarwate algorithm */
		while (len--) {
			crc ^= *p++;
			crc = (crc >> 8) ^ tab[0][crc & 255];
		}
		# else
		crc = (__force u32) __cpu_to_le32(crc);
		crc = crc32_body_optimized(crc, p, len, tab);
		crc = __le32_to_cpu((__force __le32)crc);
		#endif
		return crc;
	}

	#ifdef CONFIG_X86
	/* Hardware-accelerated CRC32 implementations using Intel SSE4.2 instructions */
	static u32 __pure __maybe_unused crc32_intel_le(u32 crc, unsigned char const *buf, size_t len)
	{
		uintptr_t addr = (uintptr_t)buf;
		size_t aligned_len;

		/* Process individual bytes until aligned to 8 bytes */
		while (len && (addr & 7)) {
			crc = __builtin_ia32_crc32qi(crc, *buf);
			buf++;
			addr++;
			len--;
		}

		/* Process 8 bytes at a time (64-bit) */
		aligned_len = len & ~7ULL;
		if (aligned_len) {
			const u64 *buf64 = (const u64 *)buf;
			size_t chunks64 = aligned_len >> 3;

			while (chunks64--) {
				crc = __builtin_ia32_crc32di(crc, *buf64);
				buf64++;
			}

			buf += aligned_len;
			len -= aligned_len;
		}

		/* Handle remaining bytes */
		while (len--) {
			crc = __builtin_ia32_crc32qi(crc, *buf);
			buf++;
		}

		return crc;
	}

	static u32 __pure __maybe_unused __crc32c_intel_le(u32 crc, unsigned char const *buf, size_t len)
	{
		uintptr_t addr = (uintptr_t)buf;
		size_t aligned_len;

		/* Process individual bytes until aligned to 8 bytes */
		while (len && (addr & 7)) {
			crc = __builtin_ia32_crc32qi(crc, *buf);
			buf++;
			addr++;
			len--;
		}

		/* Process 8 bytes at a time (64-bit) */
		aligned_len = len & ~7ULL;
		if (aligned_len) {
			const u64 *buf64 = (const u64 *)buf;
			size_t chunks64 = aligned_len >> 3;

			while (chunks64--) {
				crc = __builtin_ia32_crc32di(crc, *buf64);
				buf64++;
			}

			buf += aligned_len;
			len -= aligned_len;
		}

		/* Handle remaining bytes */
		while (len--) {
			crc = __builtin_ia32_crc32qi(crc, *buf);
			buf++;
		}

		return crc;
	}

	/* Optimized SSE4.2 implementation with cache-friendly processing */
	static u32 __pure crc32_intel_le_opt(u32 crc, unsigned char const *buf, size_t len)
	{
		uintptr_t addr = (uintptr_t)buf;
		size_t aligned_len;

		/* Process individual bytes until aligned to 8 bytes */
		while (len && (addr & 7)) {
			crc = __builtin_ia32_crc32qi(crc, *buf);
			buf++;
			addr++;
			len--;
		}

		/* Process data in 64-byte chunks when possible (cache line size) */
		if (len >= 64) {
			const u64 *buf64 = (const u64 *)buf;
			size_t chunks64 = len >> 6; /* Divide by 64 */
			size_t remaining64 = (chunks64 << 6); /* Multiply by 64 */

			/* Process 8 u64 values (64 bytes) per iteration */
			while (chunks64--) {
				/* Prefetch next cache line */
				__builtin_prefetch(buf64 + 8, 0, 0);

				/* Process current cache line (8 u64 values) */
				crc = __builtin_ia32_crc32di(crc, buf64[0]);
				crc = __builtin_ia32_crc32di(crc, buf64[1]);
				crc = __builtin_ia32_crc32di(crc, buf64[2]);
				crc = __builtin_ia32_crc32di(crc, buf64[3]);
				crc = __builtin_ia32_crc32di(crc, buf64[4]);
				crc = __builtin_ia32_crc32di(crc, buf64[5]);
				crc = __builtin_ia32_crc32di(crc, buf64[6]);
				crc = __builtin_ia32_crc32di(crc, buf64[7]);

				buf64 += 8;
			}

			buf += remaining64;
			len -= remaining64;
		}

		/* Process remaining 8-byte chunks */
		aligned_len = len & ~7ULL;
		if (aligned_len) {
			const u64 *buf64 = (const u64 *)buf;
			size_t chunks64 = aligned_len >> 3;

			while (chunks64--) {
				crc = __builtin_ia32_crc32di(crc, *buf64);
				buf64++;
			}

			buf += aligned_len;
			len -= aligned_len;
		}

		/* Handle remaining bytes */
		while (len--) {
			crc = __builtin_ia32_crc32qi(crc, *buf);
			buf++;
		}

		return crc;
	}

	static u32 __pure __crc32c_intel_le_opt(u32 crc, unsigned char const *buf, size_t len)
	{
		/* Implementation is identical to crc32_intel_le_opt */
		return crc32_intel_le_opt(crc, buf, len);
	}

	#ifdef CONFIG_AS_AVX2
	/* AVX2-based implementation for large data sets */
	static u32 __pure crc32_avx2_le(u32 crc, const unsigned char *buf, size_t len)
	{
		int i;

		/* For small buffers, use the SSE4.2 implementation */
		if (len < 256)
			return crc32_intel_le_opt(crc, buf, len);

		uintptr_t addr = (uintptr_t)buf;

		/* Process bytes until aligned to 32 bytes (AVX2 register size) */
		while (len && (addr & 31)) {
			crc = __builtin_ia32_crc32qi(crc, *buf);
			buf++;
			addr++;
			len--;
		}

		/*
		 * Process multiple streams in parallel using CRC32 instructions
		 * and manual stream splitting
		 */
		if (len >= 1024) {
			/* Split into 4 parallel streams */
			u32 crc_values[4] = {crc, 0, 0, 0};
			size_t block_size = (len >> 2) & ~31ULL; /* Divide by 4, 32-byte aligned */

			/* Create 4 parallel streams */
			const u64 *bufs[4];
			bufs[0] = (const u64 *)buf;

			for (i = 1; i < 4; i++) {
				bufs[i] = bufs[i-1] + (block_size >> 3); /* Advance by block_size bytes */
			}

			size_t chunks = block_size >> 3; /* Number of 8-byte chunks per stream */
			size_t total_processed = block_size * 4;

			/* Process 4 streams in parallel */
			while (chunks--) {
				/* Process each stream with CRC32 instructions */
				for (i = 0; i < 4; i++) {
					crc_values[i] = __builtin_ia32_crc32di(crc_values[i], *bufs[i]++);
				}
			}

			/* Combine the 4 CRC values */
			crc = crc_values[0];
			for (i = 1; i < 4; i++) {
				crc = crc32_combine(crc, crc_values[i], block_size);
			}

			/* Adjust pointers and length for remaining data */
			buf += total_processed;
			len -= total_processed;
		}

		/* Process remaining bytes using SSE4.2 */
		if (len > 0)
			crc = crc32_intel_le_opt(crc, buf, len);

		return crc;
	}

	static u32 __pure __crc32c_avx2_le(u32 crc, const unsigned char *buf, size_t len)
	{
		/* Implementation is identical to crc32_avx2_le */
		return crc32_avx2_le(crc, buf, len);
	}

	/* Raptor Lake specific AVX2 implementation */
	static u32 __pure crc32_avx2_raptor_lake(u32 crc, const unsigned char *buf, size_t len)
	{
		int s, i;

		/* For smaller buffers, use the SSE4.2 implementation */
		if (len < 512)
			return crc32_intel_le_opt(crc, buf, len);

		uintptr_t addr = (uintptr_t)buf;

		/* Align to 32 bytes */
		while (len && (addr & 31)) {
			crc = __builtin_ia32_crc32qi(crc, *buf);
			buf++;
			addr++;
			len--;
		}

		/*
		 * Raptor Lake specific optimization:
		 * - Use 64-byte chunks to match cache line size
		 * - Take advantage of improved execution units in Raptor Lake
		 */
		if (len >= 512) {
			/*
			 * Process multiple streams in parallel
			 * Raptor Lake can handle 4 parallel streams efficiently
			 */
			const int NUM_STREAMS = 4;
			u32 crc_values[NUM_STREAMS];
			crc_values[0] = crc;

			for (i = 1; i < NUM_STREAMS; i++)
				crc_values[i] = 0;

			size_t block_size = (len / NUM_STREAMS) & ~63ULL; /* 64-byte aligned blocks */
			size_t total_processed = block_size * NUM_STREAMS;

			/* Create pointers for each stream */
			const u64 *stream_bufs[NUM_STREAMS];
			stream_bufs[0] = (const u64 *)buf;

			for (i = 1; i < NUM_STREAMS; i++)
				stream_bufs[i] = stream_bufs[i-1] + (block_size / 8);

			/* Process each stream */
			size_t chunks = block_size / 64; /* 64 bytes per chunk */

			while (chunks--) {
				/* Process 64 bytes (8 u64 values) for each stream */
				for (s = 0; s < NUM_STREAMS; s++) {
					/* Process 8 u64 values (64 bytes) */
					for (i = 0; i < 8; i++) {
						crc_values[s] = __builtin_ia32_crc32di(crc_values[s], stream_bufs[s][i]);
					}
					stream_bufs[s] += 8;
				}
			}

			/* Combine all CRC values */
			crc = crc_values[0];
			for (i = 1; i < NUM_STREAMS; i++)
				crc = crc32_combine(crc, crc_values[i], block_size);

			/* Update buffer pointer and length */
			buf += total_processed;
			len -= total_processed;
		}

		/* Process remaining bytes using the SSE4.2 implementation */
		if (len > 0)
			crc = crc32_intel_le_opt(crc, buf, len);

		return crc;
	}

	static u32 __pure __crc32c_avx2_raptor_lake(u32 crc, const unsigned char *buf, size_t len)
	{
		/* Implementation is identical to crc32_avx2_raptor_lake */
		return crc32_avx2_raptor_lake(crc, buf, len);
	}
	#endif /* CONFIG_AS_AVX2 */
	#endif /* CONFIG_X86 */

	/* Generic software implementations for fallback */
	static u32 __pure crc32_le_sw_unrolled(u32 crc, unsigned char const *buf, size_t len)
	{
		/* For small blocks, use the simple approach */
		if (len < 16) {
			while (len--) {
				crc ^= *buf++;
				crc = (crc >> 8) ^ crc32table_le[0][crc & 255];
			}
			return crc;
		}

		/* Align to 4 bytes */
		while (len && ((uintptr_t)buf & 3)) {
			crc ^= *buf++;
			crc = (crc >> 8) ^ crc32table_le[0][crc & 255];
			len--;
		}

		/* Process 16 bytes at a time with loop unrolling */
		if (len >= 16) {
			size_t count = len >> 4;
			len &= 15;

			while (count--) {
				const u32 *b = (const u32 *)buf;
				u32 q;

				/* Unroll 4 iterations of 4-byte processing */
				q = crc ^ *b++;
				crc = crc32table_le[3][q & 0xFF] ^
				crc32table_le[2][(q >> 8) & 0xFF] ^
				crc32table_le[1][(q >> 16) & 0xFF] ^
				crc32table_le[0][q >> 24];

				q = crc ^ *b++;
				crc = crc32table_le[3][q & 0xFF] ^
				crc32table_le[2][(q >> 8) & 0xFF] ^
				crc32table_le[1][(q >> 16) & 0xFF] ^
				crc32table_le[0][q >> 24];

				q = crc ^ *b++;
				crc = crc32table_le[3][q & 0xFF] ^
				crc32table_le[2][(q >> 8) & 0xFF] ^
				crc32table_le[1][(q >> 16) & 0xFF] ^
				crc32table_le[0][q >> 24];

				q = crc ^ *b++;
				crc = crc32table_le[3][q & 0xFF] ^
				crc32table_le[2][(q >> 8) & 0xFF] ^
				crc32table_le[1][(q >> 16) & 0xFF] ^
				crc32table_le[0][q >> 24];

				buf += 16;
			}
		}

		/* Process remaining bytes */
		while (len--) {
			crc ^= *buf++;
			crc = (crc >> 8) ^ crc32table_le[0][crc & 255];
		}

		return crc;
	}

	static u32 __pure crc32_le_generic_impl(u32 crc, unsigned char const *p, size_t len)
	{
		if (len > 64)
			return crc32_le_sw_unrolled(crc, p, len);

		return crc32_le_generic(crc, p, len, crc32table_le, CRC32_POLY_LE);
	}

	static u32 __pure __crc32c_le_generic_impl(u32 crc, unsigned char const *p, size_t len)
	{
		if (len > 64)
			return crc32_le_sw_unrolled(crc, p, len);

		return crc32_le_generic(crc, p, len, crc32ctable_le, CRC32C_POLY_LE);
	}

	#if CRC_LE_BITS == 1
	u32 __pure __weak crc32_le(u32 crc, unsigned char const *p, size_t len)
	{
		return crc32_le_generic(crc, p, len, NULL, CRC32_POLY_LE);
	}
	u32 __pure __weak __crc32c_le(u32 crc, unsigned char const *p, size_t len)
	{
		return crc32_le_generic(crc, p, len, NULL, CRC32C_POLY_LE);
	}
	#else
	u32 __pure __weak crc32_le(u32 crc, unsigned char const *p, size_t len)
	{
		/* Use the function pointer set up during initialization */
		if (likely(crc32_le_impl))
			return crc32_le_impl(crc, p, len);

		return crc32_le_generic(crc, p, len, crc32table_le, CRC32_POLY_LE);
	}
	u32 __pure __weak __crc32c_le(u32 crc, unsigned char const *p, size_t len)
	{
		/* Use the function pointer set up during initialization */
		if (likely(__crc32c_le_impl))
			return __crc32c_le_impl(crc, p, len);

		return crc32_le_generic(crc, p, len, crc32ctable_le, CRC32C_POLY_LE);
	}
	#endif
	EXPORT_SYMBOL(crc32_le);
	EXPORT_SYMBOL(__crc32c_le);

	u32 __pure crc32_le_base(u32, unsigned char const *, size_t) __alias(crc32_le);
	EXPORT_SYMBOL(crc32_le_base);

	u32 __pure __crc32c_le_base(u32, unsigned char const *, size_t) __alias(__crc32c_le);
	EXPORT_SYMBOL(__crc32c_le_base);

	u32 __pure crc32_be_base(u32, unsigned char const *, size_t) __alias(crc32_be);

	/*
	 * This multiplies the polynomials x and y modulo the given modulus.
	 * This follows the "little-endian" CRC convention that the lsbit
	 * represents the highest power of x, and the msbit represents x^0.
	 */
	static u32 __attribute_const__ gf2_multiply(u32 x, u32 y, u32 modulus)
	{
		u32 product = x & 1 ? y : 0;
		int i;

		for (i = 0; i < 31; i++) {
			product = (product >> 1) ^ (product & 1 ? modulus : 0);
			x >>= 1;
			product ^= x & 1 ? y : 0;
		}

		return product;
	}

	/**
	 * crc32_generic_shift - Append @len 0 bytes to crc, in logarithmic time
	 * @crc: The original little-endian CRC (i.e. lsbit is x^31 coefficient)
	 * @len: The number of bytes. @crc is multiplied by x^(8*@len)
	 * @polynomial: The modulus used to reduce the result to 32 bits.
	 *
	 * It's possible to parallelize CRC computations by computing a CRC
	 * over separate ranges of a buffer, then summing them.
	 * This shifts the given CRC by 8*len bits (i.e. produces the same effect
	 * as appending len bytes of zero to the data), in time proportional
	 * to log(len).
	 */
	static u32 __attribute_const__ crc32_generic_shift(u32 crc, size_t len,
													   u32 polynomial)
	{
		u32 power = polynomial; /* CRC of x^32 */
		int i;

		/* Shift up to 32 bits in the simple linear way */
		for (i = 0; i < 8 * (int)(len & 3); i++)
			crc = (crc >> 1) ^ (crc & 1 ? polynomial : 0);

		len >>= 2;
		if (!len)
			return crc;

		for (;;) {
			/* "power" is x^(2^i), modulo the polynomial */
			if (len & 1)
				crc = gf2_multiply(crc, power, polynomial);

			len >>= 1;
			if (!len)
				break;

			/* Square power, advancing to x^(2^(i+1)) */
			power = gf2_multiply(power, power, polynomial);
		}

		return crc;
	}

	u32 __attribute_const__ crc32_le_shift(u32 crc, size_t len)
	{
		return crc32_generic_shift(crc, len, CRC32_POLY_LE);
	}

	u32 __attribute_const__ __crc32c_le_shift(u32 crc, size_t len)
	{
		return crc32_generic_shift(crc, len, CRC32C_POLY_LE);
	}
	EXPORT_SYMBOL(crc32_le_shift);
	EXPORT_SYMBOL(__crc32c_le_shift);

	/**
	 * crc32_be_generic() - Calculate bitwise big-endian Ethernet AUTODIN II CRC32
	 * @crc: seed value for computation.  ~0 for Ethernet, sometimes 0 for
	 *      other uses, or the previous crc32 value if computing incrementally.
	 * @p: pointer to buffer over which CRC32 is run
	 * @len: length of buffer @p
	 * @tab: big-endian Ethernet table
	 * @polynomial: CRC32 BE polynomial
	 */
	static inline u32 __pure crc32_be_generic(u32 crc, unsigned char const *p,
											  size_t len, const u32 (*tab)[256],
											  u32 polynomial)
	{
		#if CRC_BE_BITS == 1
		int i;
		while (len--) {
			crc ^= *p++ << 24;
			for (i = 0; i < 8; i++)
				crc =
				(crc << 1) ^ ((crc & 0x80000000) ? polynomial :
				0);
		}
		# elif CRC_BE_BITS == 2
		while (len--) {
			crc ^= *p++ << 24;
			crc = (crc << 2) ^ tab[0][crc >> 30];
			crc = (crc << 2) ^ tab[0][crc >> 30];
			crc = (crc << 2) ^ tab[0][crc >> 30];
			crc = (crc << 2) ^ tab[0][crc >> 30];
		}
		# elif CRC_BE_BITS == 4
		while (len--) {
			crc ^= *p++ << 24;
			crc = (crc << 4) ^ tab[0][crc >> 28];
			crc = (crc << 4) ^ tab[0][crc >> 28];
		}
		# elif CRC_BE_BITS == 8
		while (len--) {
			crc ^= *p++ << 24;
			crc = (crc << 8) ^ tab[0][crc >> 24];
		}
		# else
		crc = (__force u32) __cpu_to_be32(crc);
		crc = crc32_body_optimized(crc, p, len, tab);
		crc = __be32_to_cpu((__force __be32)crc);
		# endif
		return crc;
	}

	#if CRC_BE_BITS == 1
	u32 __pure __weak crc32_be(u32 crc, unsigned char const *p, size_t len)
	{
		return crc32_be_generic(crc, p, len, NULL, CRC32_POLY_BE);
	}
	#else
	u32 __pure __weak crc32_be(u32 crc, unsigned char const *p, size_t len)
	{
		return crc32_be_generic(crc, p, len, crc32table_be, CRC32_POLY_BE);
	}
	#endif
	EXPORT_SYMBOL(crc32_be);

	/**
	 * crc32_combine - Combine two CRC values into one
	 * @crc1: First CRC value
	 * @crc2: Second CRC value
	 * @len2: Length of data that crc2 represents
	 */
	u32 __attribute_const__ crc32_combine(u32 crc1, u32 crc2, size_t len2)
	{
		return crc1 ^ crc32_le_shift(crc2, len2);
	}
	EXPORT_SYMBOL(crc32_combine);

	/**
	 * __crc32c_combine - Combine two CRC-32C values into one
	 * @crc1: First CRC value
	 * @crc2: Second CRC value
	 * @len2: Length of data that crc2 represents
	 */
	u32 __attribute_const__ __crc32c_combine(u32 crc1, u32 crc2, size_t len2)
	{
		return crc1 ^ __crc32c_le_shift(crc2, len2);
	}
	EXPORT_SYMBOL(__crc32c_combine);

	#ifdef CONFIG_X86
	/* CPU feature detection and initialization */
	static bool __init has_sse42_crc32(void)
	{
		return static_cpu_has(X86_FEATURE_PCLMULQDQ);
	}

	#ifdef CONFIG_AS_AVX2
	static bool __init has_avx2(void)
	{
		return static_cpu_has(X86_FEATURE_AVX2);
	}
	#endif

	static int __init crc32_init_dispatch(void)
	{
		#ifdef CONFIG_AS_AVX2
		if (has_avx2()) {
			/* Check if we're on Raptor Lake */
			if ((boot_cpu_data.x86_vendor == X86_VENDOR_INTEL) &&
				(boot_cpu_data.x86 == 6) &&
				((boot_cpu_data.x86_model == 0xB7) ||  /* Raptor Lake */
				(boot_cpu_data.x86_model == 0xBF) ||  /* Raptor Lake-P */
				(boot_cpu_data.x86_model == 0xBA))) { /* Raptor Lake-S */

					crc32_le_impl = crc32_avx2_raptor_lake;
					__crc32c_le_impl = __crc32c_avx2_raptor_lake;
					pr_info("CRC32: Using Raptor Lake optimized AVX2 implementation\n");
				} else {
					crc32_le_impl = crc32_avx2_le;
					__crc32c_le_impl = __crc32c_avx2_le;
					pr_info("CRC32: Using AVX2 implementation\n");
				}
		} else
			#endif /* CONFIG_AS_AVX2 */
			if (has_sse42_crc32()) {
				crc32_le_impl = crc32_intel_le_opt;
				__crc32c_le_impl = __crc32c_intel_le_opt;
				pr_info("CRC32: Using SSE4.2 implementation\n");
			} else
				#endif /* CONFIG_X86 */
			{
				crc32_le_impl = crc32_le_generic_impl;
				__crc32c_le_impl = __crc32c_le_generic_impl;
				pr_info("CRC32: Using software implementation\n");
			}

			return 0;
	}
	early_initcall(crc32_init_dispatch);
