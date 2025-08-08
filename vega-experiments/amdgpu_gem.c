/*
 * Copyright 2008 Advanced Micro Devices, Inc.
 * Copyright 2008 Red Hat Inc.
 * Copyright 2009 Jerome Glisse.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE COPYRIGHT HOLDER(S) OR AUTHOR(S) BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 * Authors: Dave Airlie
 *          Alex Deucher
 *          Jerome Glisse
 */
#include <linux/ktime.h>
#include <linux/mm.h>
#include <linux/module.h>
#include <linux/pagemap.h>
#include <linux/pci.h>
#include <linux/dma-buf.h>
#include <linux/jump_label.h>
#include <linux/prefetch.h>
#include <linux/atomic.h>
#include <linux/sched.h>
#include <linux/sched/clock.h>
#include <linux/seqlock.h>
#include <linux/math64.h>
#include <linux/overflow.h>
#include <linux/hash.h>
#include <linux/workqueue.h>
#include <linux/completion.h>
#include <linux/atomic.h>
#include <asm/fpu/api.h>
#include <asm/cpufeature.h>
#include <asm/irq.h>

#include <drm/amdgpu_drm.h>
#include <drm/drm_drv.h>
#include <drm/drm_exec.h>
#include <drm/drm_gem_ttm_helper.h>
#include <drm/ttm/ttm_tt.h>

#include "amdgpu.h"
#include "amdgpu_display.h"
#include "amdgpu_dma_buf.h"
#include "amdgpu_hmm.h"
#include "amdgpu_xgmi.h"
#include "amdgpu_vm.h"

/* --- Forward declarations --- */
static inline u32 pb_get_bias(void);
static bool amdgpu_vega_optimize_for_workload(struct amdgpu_device *adev,
											  struct amdgpu_bo *bo,
											  uint64_t flags);
void amdgpu_vega_vram_thresholds_init(void);

#if defined(CONFIG_X86_64)
#define KTIME_FAST_NS()  ktime_get_mono_fast_ns()
#else
#define KTIME_FAST_NS()  ktime_get_ns()
#endif

/*
 * VRAM Pressure Governor
 */
enum vega_vram_pressure_state {
	VEGA_VRAM_GREEN,
	VEGA_VRAM_YELLOW,
	VEGA_VRAM_RED,
};

/* Padded to prevent false sharing on multi-core CPUs */
struct vega_pressure_cache_pc {
	enum vega_vram_pressure_state state;
	u32 pct_last;
	unsigned long jts_last_update;
} ____cacheline_aligned;

static DEFINE_PER_CPU(struct vega_pressure_cache_pc, pressure_cache_pc);

struct vega_vram_state {
	/* seqcount_spinlock_t is optimal for this read-mostly data */
	seqcount_spinlock_t seq;
	u64        size_bytes;
	u64        reciprocal_100;
	atomic_t   eviction_rate_ewma;
	spinlock_t lock; /* The spinlock for the seqcount */
};

/* Correctly initialized structure */
static struct vega_vram_state vram_state = {
	.seq = SEQCNT_SPINLOCK_ZERO(vram_state.seq, &vram_state.lock),
	.size_bytes = 0,
	.reciprocal_100 = 0,
	.eviction_rate_ewma = ATOMIC_INIT(0),
	.lock = __SPIN_LOCK_UNLOCKED(vram_state.lock),
};

/* Initialize VRAM state on first device probe */
static void amdgpu_vram_state_init(struct amdgpu_device *adev)
{
	static atomic_t initialised = ATOMIC_INIT(0);
	unsigned long flags;
	u64 vram_b;

	if (unlikely(!adev)) {
		return;
	}

	if (atomic_xchg(&initialised, 1)) {
		return;					/* already done */
	}

	vram_b = adev->gmc.mc_vram_size;
	if (unlikely(vram_b < SZ_1M)) {
		pr_warn("amdgpu: VRAM size too small – pressure governor off\n");
		return;
	}

	spin_lock_irqsave(&vram_state.lock, flags);
	write_seqcount_begin(&vram_state.seq);
	vram_state.size_bytes = vram_b;
	vram_state.reciprocal_100 =
	div64_u64((1ULL << 38), div64_u64(vram_b, 100)); /* Q38 */
	write_seqcount_end(&vram_state.seq);
	spin_unlock_irqrestore(&vram_state.lock, flags);
}


#define PB_ENTRIES          64U
#define PB_HASH_MASK        (PB_ENTRIES - 1)
#define EW_UNIT_SHIFT       4
#define EW_INC_PER_FAULT    16
#define MAX_EWMA            (16 << EW_UNIT_SHIFT)
#define PB_DECAY_FACTOR_NS (128 * NSEC_PER_MSEC)

/* Padded to prevent false sharing */
struct pid_bias_entry {
	u32 tgid;
	u16 ewma;
	u64 ns_last;
} ____cacheline_aligned;

static DEFINE_PER_CPU(struct pid_bias_entry[PB_ENTRIES], pid_bias_tbl);

DEFINE_STATIC_KEY_FALSE(vega_bankalign_key);
DEFINE_STATIC_KEY_FALSE(vega_prefetch_key);
DEFINE_STATIC_KEY_FALSE(vega_domain_key);
DEFINE_STATIC_KEY_FALSE(amdgpu_vm_always_valid_key);

#define vega_hbm2_key vega_domain_key

static inline void amdgpu_vm_always_valid_key_enable(void)
{
	if (static_branch_likely(&amdgpu_vm_always_valid_key))
		return;

	static_branch_enable(&amdgpu_vm_always_valid_key);
}

static inline void
amdgpu_gem_static_branch_init(struct amdgpu_device *adev)
{
	if (adev && adev->asic_type == CHIP_VEGA10) {
		static_branch_enable(&vega_bankalign_key);
		static_branch_enable(&vega_prefetch_key);
		static_branch_enable(&vega_domain_key);
		amdgpu_vram_state_init(adev);
	}
}

/* Optimization: Increased from 32 */
#define TBO_CACHE_DEPTH 64
#define TBO_MAX_BYTES   (64u << 10)
#define TBO_CACHEABLE_VRAM_FLAGS (AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS | \
AMDGPU_GEM_CREATE_NO_CPU_ACCESS   | \
AMDGPU_GEM_CREATE_VRAM_CLEARED)

/* Lockless per-CPU cache with optimal alignment */
struct tiny_bo_cache {
	local_lock_t lock;				/* protects the indices */
	struct amdgpu_bo *gtt_slot[TBO_CACHE_DEPTH / 2];
	struct amdgpu_bo *vram_slot[TBO_CACHE_DEPTH / 2];
	u8                gtt_top;
	u8                vram_top;
} ____cacheline_aligned;

static DEFINE_PER_CPU(struct tiny_bo_cache, tiny_bo_cache);
DEFINE_STATIC_KEY_FALSE(tbo_cache_key);

static struct kmem_cache *ubo_slab;

static void amdgpu_tbo_slab_ensure(void)
{
	struct kmem_cache *s;

	if (likely(READ_ONCE(ubo_slab)))
		return;

	s = kmem_cache_create("amdgpu_bo_user",
			      sizeof(struct amdgpu_bo_user),
			      0, SLAB_HWCACHE_ALIGN, NULL);
	if (unlikely(!s))
		return;

	if (cmpxchg(&ubo_slab, NULL, s)) {
		kmem_cache_destroy(s);
	} else {
		static_branch_enable(&tbo_cache_key);
	}
}

/* --------------------------------------------------------------------- *
 * Helpers for flag checking                                             *
 * --------------------------------------------------------------------- */
static inline bool tbo_flags_match_gtt(u64 f)
{
	return !(f & ~(AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED |
	AMDGPU_GEM_CREATE_CPU_GTT_USWC));
}

static inline bool tbo_flags_match_vram(u64 f)
{
	return f == (AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS |
	AMDGPU_GEM_CREATE_NO_CPU_ACCESS   |
	AMDGPU_GEM_CREATE_VRAM_CLEARED);
}

/* --------------------------------------------------------------------- *
 * Fast per-CPU allocation path                                          *
 * --------------------------------------------------------------------- */
static struct amdgpu_bo *
tbo_cache_try_get(unsigned long size, u64 user_flags,
                  u32 domain, struct dma_resv *resv, int align)
{
    struct tiny_bo_cache *c;
    struct amdgpu_bo *bo = NULL;

    /* Fast-path bail-outs -------------------------------------------------- */
    if (!static_branch_unlikely(&tbo_cache_key)) {
        return NULL;				/* cache disabled          */
    }
    if (size > TBO_MAX_BYTES || align > PAGE_SIZE || resv) {
        return NULL;				/* non-tiny / external resv */
    }

    c = this_cpu_ptr(&tiny_bo_cache);

    /* ----------------------------- pop from stack ------------------------ */
    // Neat trick: Atomic pop using kernel smp_ for lockless fast path (ordering ensures no use-after-free)
    if (domain == AMDGPU_GEM_DOMAIN_GTT && tbo_flags_match_gtt(user_flags)) {
        u8 top = smp_load_acquire(&c->gtt_top);  // Acquire for visibility
        if (top) {
            bo = smp_load_acquire(&c->gtt_slot[top - 1]);
            u8 expected = top;
            if (bo && __atomic_compare_exchange_n(&c->gtt_top, &expected, top - 1, false, __ATOMIC_RELEASE, __ATOMIC_RELAXED)) {
                // Success, release ordering
            } else {
                bo = NULL;
            }
        }
    } else if (domain == AMDGPU_GEM_DOMAIN_VRAM && tbo_flags_match_vram(user_flags)) {
        u8 top = smp_load_acquire(&c->vram_top);
        if (top) {
            bo = smp_load_acquire(&c->vram_slot[top - 1]);
            u8 expected = top;
            if (bo && __atomic_compare_exchange_n(&c->vram_top, &expected, top - 1, false, __ATOMIC_RELEASE, __ATOMIC_RELAXED)) {
                // Success
            } else {
                bo = NULL;
            }
        }
    }

    /* Was evicted while cached? ------------------------------------------ */
    if (bo && bo->tbo.ttm) {
        ttm_bo_put(&bo->tbo);
        bo = NULL;
    }

    /* Size mismatch?  Push back or drop. ---------------------------------- */
    if (bo && bo->tbo.base.size != size) {
        local_lock(&c->lock);
        if (domain == AMDGPU_GEM_DOMAIN_GTT &&
            c->gtt_top < ARRAY_SIZE(c->gtt_slot)) {
            WRITE_ONCE(c->gtt_slot[c->gtt_top++], bo);
            bo = NULL;
        } else if (domain == AMDGPU_GEM_DOMAIN_VRAM &&
                   c->vram_top < ARRAY_SIZE(c->vram_slot)) {
            WRITE_ONCE(c->vram_slot[c->vram_top++], bo);
            bo = NULL;
        }
        local_unlock(&c->lock);

        if (bo) {			/* cache full – free object */
            ttm_bo_put(&bo->tbo);
            bo = NULL;
        }
    }

    /* --------- Optional HBM2 bank-alignment preference (Vega only) ------- */
    if (bo && domain == AMDGPU_GEM_DOMAIN_VRAM &&
        static_branch_unlikely(&vega_domain_key)) {

        u32 page_align = bo->tbo.page_alignment;	/* already pages   */
        bool pwr2      = is_power_of_2(page_align);
        bool hbm_algn  = pwr2 && (page_align >= (SZ_1M >> PAGE_SHIFT));

        /*
         * Keep the bottom 75 % of the per-CPU VRAM stack for "good"
         * (≥1 MiB-aligned) objects to maximise bank utilisation.
         */
        if (!hbm_algn) {
            bool discard = false;

            local_lock(&c->lock);
            discard = c->vram_top >
                      (u8)(ARRAY_SIZE(c->vram_slot) * 3 / 4);
            local_unlock(&c->lock);

            if (discard) {
                ttm_bo_put(&bo->tbo);
                bo = NULL;
            }
        }
    }

    if (bo) {			/* Prefetch to warm cache lines   */
        prefetchw(bo->tbo.base.resv);
        prefetch(bo);
    }

    return bo;
}

/* --------------------------------------------------------------------- *
 * Slow path – park an idle BO in the per-CPU cache                      *
 * --------------------------------------------------------------------- */
static bool tbo_cache_put(struct amdgpu_bo *bo)
{
	struct tiny_bo_cache *c;
	u64 flags;

	if (!static_branch_unlikely(&tbo_cache_key) || !bo) {
		return false;
	}

	if (!dma_resv_test_signaled(bo->tbo.base.resv, DMA_RESV_USAGE_WRITE) ||
		bo->tbo.base.size > TBO_MAX_BYTES ||
		(bo->tbo.page_alignment << PAGE_SHIFT) > PAGE_SIZE ||
		bo->tbo.ttm) {
		return false;
		}

		if (!kref_get_unless_zero(&bo->tbo.kref)) {	/* someone freed it */
			return false;
		}

		flags = bo->flags & ~AMDGPU_GEM_CREATE_VRAM_WIPE_ON_RELEASE;

		if (__builtin_popcountll(flags) > 4) {
			ttm_bo_put(&bo->tbo);
			return false;
		}

		c = this_cpu_ptr(&tiny_bo_cache);
		local_lock(&c->lock);

		switch (bo->preferred_domains) {
			case AMDGPU_GEM_DOMAIN_GTT:
				if (tbo_flags_match_gtt(flags) &&
					c->gtt_top < ARRAY_SIZE(c->gtt_slot)) {
					WRITE_ONCE(c->gtt_slot[c->gtt_top++], bo);
				local_unlock(&c->lock);
				return true;
					}
					break;
			case AMDGPU_GEM_DOMAIN_VRAM:
				if (tbo_flags_match_vram(flags) &&
					c->vram_top < ARRAY_SIZE(c->vram_slot)) {
					WRITE_ONCE(c->vram_slot[c->vram_top++], bo);
				local_unlock(&c->lock);
				return true;
					}
					break;
			default:
				break;
		}

		local_unlock(&c->lock);
		ttm_bo_put(&bo->tbo);
		return false;
}

#define AMDGPU_VEGA_HBM2_BANK_SIZE       (1ULL * 1024 * 1024)
#define AMDGPU_VEGA_SMALL_BUFFER_SIZE    (1ULL * 1024 * 1024)
#define AMDGPU_VEGA_MEDIUM_BUFFER_SIZE   (4ULL * 1024 * 1024)
#define AMDGPU_VEGA_LARGE_BUFFER_SIZE    (16ULL * 1024 * 1024)
#define AMDGPU_VEGA_HBM2_MIN_ALIGNMENT   (256 * 1024)
#define FAST_VA_MAP_MAX_BYTES		 (64u << 10)

static int amdgpu_vega_vram_pressure_mid  __ro_after_init = 75;
static int amdgpu_vega_vram_pressure_high __ro_after_init = 90;

void amdgpu_vega_vram_thresholds_init(void)
{
	amdgpu_vega_vram_pressure_mid =
	clamp(amdgpu_vega_vram_pressure_mid, 50, 90);

	amdgpu_vega_vram_pressure_high =
	clamp(amdgpu_vega_vram_pressure_high,
		  amdgpu_vega_vram_pressure_mid + 5, 95);
}

module_param_named(vram_pressure_mid, amdgpu_vega_vram_pressure_mid,  int, 0644);
MODULE_PARM_DESC(vram_pressure_mid,  "Mid VRAM pressure threshold for Vega (75)");
module_param_named(vram_pressure_high, amdgpu_vega_vram_pressure_high, int, 0644);
MODULE_PARM_DESC(vram_pressure_high, "High VRAM pressure threshold for Vega (90)");

static __always_inline bool is_vega_texture(uint64_t flags)
{
	return flags & AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS;
}

static __always_inline bool is_vega_compute(uint64_t flags)
{
	return flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS;
}

static __always_inline bool is_vega_cpu_access(uint64_t flags)
{
	return flags & AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED;
}

static __always_inline bool is_hbm2_vega(struct amdgpu_device *adev)
{
	return static_branch_unlikely(&vega_hbm2_key);
}

struct vram_pct_percpu_cache {
    u32 pct;
    unsigned long stamp_j;
} ____cacheline_aligned;

static DEFINE_PER_CPU(struct vram_pct_percpu_cache, vram_pct_cache);

static u32 __amdgpu_vega_get_vram_usage(struct amdgpu_device *adev)
{
    struct ttm_resource_manager *mgr;
    struct vram_pct_percpu_cache *pc;
    u64 recip_q = 0;
    u64 used_bytes = 0;
    unsigned long now_j;
    unsigned int seq;
    u32 pct;

    if (unlikely(!adev))
        return 0;

    mgr = ttm_manager_type(&adev->mman.bdev, TTM_PL_VRAM);
    if (unlikely(!mgr))
        return 0;

    amdgpu_vram_state_init(adev);

    preempt_disable();
    pc = this_cpu_ptr(&vram_pct_cache);
    now_j = READ_ONCE(jiffies);

    if (time_before(now_j, pc->stamp_j + HZ / 100)) {
        pct = pc->pct;
        preempt_enable();
        return pct;
    }
    preempt_enable();

    do {
        seq = read_seqcount_begin(&vram_state.seq);
        recip_q = READ_ONCE(vram_state.reciprocal_100);
    } while (read_seqcount_retry(&vram_state.seq, seq));

    if (unlikely(!recip_q))
        return 0;

    used_bytes = ttm_resource_manager_usage(mgr);

    pct = min_t(u32, mul_u64_u64_shr(used_bytes, recip_q, 38), 100ULL);

    preempt_disable();
    pc = this_cpu_ptr(&vram_pct_cache);
    pc->pct = pct;
    pc->stamp_j = now_j;
    preempt_enable();

    return pct;
}

/*
 * Determine current VRAM pressure on Vega-class HBM2 parts.
 * Fast path:   per-CPU cached percentage, refreshed every 10 ms.
 * Slow path:   calls into TTM once per refresh period.
 *
 * Hysteresis  : ±2 pp to avoid GREEN<->YELLOW flapping.
 * Thread-safe : per-CPU data accessed inside get_cpu()/put_cpu() window.
 *
 * Must be kept in sync with enum vega_vram_pressure_state.
 */
#define VRAM_CACHE_REFRESH_JIFFIES  (HZ / 100)   /* 10 ms */
#define VRAM_PCT_HYSTERESIS         2u           /* ±2 pp */

static enum vega_vram_pressure_state
amdgpu_vega_get_vram_pressure_state(struct amdgpu_device *adev)
{
    struct vega_pressure_cache_pc *pc;
    enum vega_vram_pressure_state st;
    unsigned long now_j;
    u32 pct_now, pct_pred;
    u32 bias, y_th, r_th;

    if (unlikely(!adev))
        return VEGA_VRAM_GREEN;

    preempt_disable();
    pc    = this_cpu_ptr(&pressure_cache_pc);
    now_j = jiffies;

    if (time_before(now_j, pc->jts_last_update + VRAM_CACHE_REFRESH_JIFFIES)) {
        st = pc->state;
        goto out;
    }

    pct_now  = __amdgpu_vega_get_vram_usage(adev);
    pct_pred = ((u64)pct_now * 3u + pc->pct_last) >> 2;

    // Incorporated fix: Capped retries in EWMA decay
    {
        u32 old, new;
        int retries = 0;
        do {
            old = atomic_read(&vram_state.eviction_rate_ewma);
            if (!old) break;
            new = old - min(old, 4u);
        } while (atomic_cmpxchg(&vram_state.eviction_rate_ewma, old, new) != old && retries++ < 10);
    }

    bias = min_t(u32, atomic_read(&vram_state.eviction_rate_ewma) >> EW_UNIT_SHIFT, 25u);

    u32 adaptive_mid = min(amdgpu_vega_vram_pressure_mid + (bias < 10 ? 5u : 0u), U32_MAX) -
                       (bias >= 10 ? min_t(u32, 5u, (u32)amdgpu_vega_vram_pressure_mid) : 0u);
    u32 adaptive_high = min(amdgpu_vega_vram_pressure_high + (bias < 10 ? 5u : 0u), U32_MAX) -
                        (bias >= 10 ? min_t(u32, 5u, (u32)amdgpu_vega_vram_pressure_high) : 0u);
    adaptive_mid = clamp(adaptive_mid, 50u, 90u);
    adaptive_high = clamp(adaptive_high, adaptive_mid + 5u, 95u);

    y_th = max(0u, adaptive_mid - bias);
    r_th = max(0u, adaptive_high - bias);
    r_th = max(r_th, y_th + 5u);

    __builtin_assume(adaptive_mid >= 50u);

    if (pct_pred > r_th + VRAM_PCT_HYSTERESIS) {
        st = VEGA_VRAM_RED;
    } else if (pct_pred > y_th + VRAM_PCT_HYSTERESIS) {
        st = VEGA_VRAM_YELLOW;
    } else if (pct_pred < y_th - VRAM_PCT_HYSTERESIS) {
        st = VEGA_VRAM_GREEN;
    } else {
        st = pc->state;
    }

    pc->state = st;
    pc->pct_last = pct_now;
    pc->jts_last_update = now_j;

out:
    preempt_enable();
    return st;
}

static inline u32 pb_hash(u32 tgid)
{
	return hash_32(tgid, ilog2(PB_ENTRIES));
}

static inline void pb_decay(struct pid_bias_entry *e, u64 now_ns)
{
	u64 delta_ns;

	if (unlikely(!e || !e->ewma)) {
		return;
	}

	delta_ns = now_ns - e->ns_last;
	if (delta_ns < PB_DECAY_FACTOR_NS) {
		return;
	}

	u64 units = delta_ns / PB_DECAY_FACTOR_NS;
	u32 shift = (units <= 1) ? 1 :
	(64 - (u32)__builtin_clzll(units - 1));

	shift = min(shift, 8u);
	e->ewma >>= shift;
	e->ns_last = now_ns;
}

static inline void pb_global_decay_once(void)
{
	u32 old, new;

	do {
		old = atomic_read(&vram_state.eviction_rate_ewma);
		if (!old) {
			return;
		}
		new = (u32){ old - min(old, 4u) };	/* compound literal */
	} while (atomic_cmpxchg(&vram_state.eviction_rate_ewma,
							old, new) != old);
}

static inline u32 pb_get_bias(void)
{
    const u32 tgid = current->tgid;
    struct pid_bias_entry *tbl;
    u32 h, i, ret = 0;
    u64 now_ns;
    int cpu;

    if (unlikely(!tgid))
        return 0;

    /* Global decay – very cheap, runs on every lookup */
    pb_global_decay_once();

    cpu = get_cpu();
    tbl = per_cpu_ptr(pid_bias_tbl, cpu);
    h   = pb_hash(tgid);
    now_ns = sched_clock();

    #if defined(CONFIG_X86_64)
    if (!irqs_disabled()) {
        #ifdef CONFIG_X86_FEATURE_AVX2
        if (cpu_has(cpu, X86_FEATURE_AVX2)) {
            kernel_fpu_begin();
            __m256i vec_tgid = _mm256_set1_epi32(tgid);
            for (i = 0; i < PB_ENTRIES; i += 8) {
                __m256i vec_entries = _mm256_loadu_si256((__m256i*)&tbl[(h + i) & PB_HASH_MASK].tgid);
                __m256i cmp = _mm256_cmpeq_epi32(vec_tgid, vec_entries);
                int mask = _mm256_movemask_epi8(cmp);
                if (mask) {
                    int idx = min(__builtin_ctz(mask | 0x10000000) / 4, 7u);
                    struct pid_bias_entry *e = &tbl[(h + i + idx) & PB_HASH_MASK];
                    if (now_ns - e->ns_last > NSEC_PER_SEC)
                        e->ewma = 0;
                    pb_decay(e, now_ns);
                    ret = e->ewma >> EW_UNIT_SHIFT;
                    kernel_fpu_end();
                    put_cpu();
                    return ret;
                }
            }
            kernel_fpu_end();
        }
        #endif

        #ifdef CONFIG_X86_FEATURE_BMI2
        if (cpu_has(cpu, X86_FEATURE_BMI2)) {
            u64 packed = 0;
            for (i = 0; i < PB_ENTRIES; i += 2) {
                u32 t1 = tbl[(h + i) & PB_HASH_MASK].tgid;
                u32 t2 = tbl[(h + i + 1) & PB_HASH_MASK].tgid;
                packed |= _pdep_u64((u64)(t1 == tgid) , 1ULL << (unsigned)(i % 64)) |
                          _pdep_u64((u64)(t2 == tgid) , 1ULL << (unsigned)((i + 1) % 64));
            }
            if (packed) {
                int idx = min(__builtin_ctzll(packed), PB_ENTRIES - 1u);
                struct pid_bias_entry *e = &tbl[(h + idx) & PB_HASH_MASK];
                if (now_ns - e->ns_last > NSEC_PER_SEC)
                    e->ewma = 0;
                pb_decay(e, now_ns);
                ret = e->ewma >> EW_UNIT_SHIFT;
                put_cpu();
                return ret;
            }
        }
        #endif
    }
    #endif

    for (i = 0; i < PB_ENTRIES; ++i, h = (h + 1) & PB_HASH_MASK) {
        struct pid_bias_entry *e = &tbl[h];

        if (e->tgid == tgid) {
            if (now_ns - e->ns_last > NSEC_PER_SEC)
                e->ewma = 0;

            pb_decay(e, now_ns);
            ret = e->ewma >> EW_UNIT_SHIFT;
            break;
        }
        if (!e->tgid)
            break;
    }
    put_cpu();
    return ret;
}

static void pb_account_eviction(void)
{
    const u32 tgid = current->tgid;
    struct pid_bias_entry *tbl;
    u32 h, i, victim_idx = PB_ENTRIES;
    u64 now_ns;
    u16 victim_ew = U16_MAX;
    int cpu;

    if (unlikely(!tgid)) {
        return;
    }

    atomic_add_unless(&vram_state.eviction_rate_ewma,
                      EW_INC_PER_FAULT, INT_MAX);

    cpu = get_cpu();
    tbl = per_cpu_ptr(pid_bias_tbl, cpu);
    h   = pb_hash(tgid);
    now_ns = sched_clock();

    for (i = 0; i < PB_ENTRIES; ++i, h = (h + 1) & PB_HASH_MASK) {
        struct pid_bias_entry *e = &tbl[h];

        if (e->tgid == tgid) {
            victim_idx = h;
            break;
        }

        u64 delta_ns = now_ns - e->ns_last;
        if (delta_ns >= PB_DECAY_FACTOR_NS) {
            u64 units = delta_ns / PB_DECAY_FACTOR_NS;
            u32 shift;
            if (units <= 1) {
                shift = 1;
            } else {
                shift = 64 - __builtin_clzll(units - 1);
            }
            shift = min(shift, 8u);
            e->ewma >>= shift;
            e->ns_last = now_ns;
        }

        if (!e->tgid) {
            victim_idx = h;
            break;
        }

        if (e->ewma < victim_ew) {
            victim_ew   = e->ewma;
            victim_idx  = h;
        }
    }

    if (victim_idx < PB_ENTRIES) {
        struct pid_bias_entry *e = &tbl[victim_idx];

        if (e->tgid != tgid && e->tgid != 0) {
            pb_decay(e, now_ns);
        }

        e->tgid    = tgid;
        e->ewma    = min_t(u16, e->ewma + EW_INC_PER_FAULT, MAX_EWMA);
        e->ns_last = now_ns;
    }

    put_cpu();
}

static bool
amdgpu_vega_optimize_hbm2_bank_access(struct amdgpu_device *adev,
                                      u64 *size_inout,
                                      u32 *align_inout,
                                      u64 flags)
{
    u64 size, new_size;
    u32 want_align;

    if (!adev || !size_inout || !align_inout) {
        return false;
    }
    if (!is_hbm2_vega(adev)) {
        return false;
    }

    size = *size_inout;

    /* skip tiny or CPU-mapped BOs */
    if (size < SZ_8M ||
        !(flags & (AMDGPU_GEM_CREATE_NO_CPU_ACCESS |
                   AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS))) {
        return false;
    }

    if (size >= SZ_256M) {
        want_align = SZ_64M;               /* full stack  */
    } else if (size >= SZ_32M) {
        want_align = SZ_8M;                /* one pipe    */
    } else {
        want_align = SZ_1M;                /* one bank    */
    }

    want_align = max_t(u32, want_align, *align_inout);
    if (!is_power_of_2(want_align)) {
        want_align = roundup_pow_of_two(want_align);
    }

    /* Use kernel check_add_overflow for standard, warning-free overflow handling */
    if (check_add_overflow(size, (u64)want_align - 1, &new_size)) {
        return false;		/* would overflow */
    }

    new_size = ALIGN(size, want_align);
    if (new_size == size && want_align == *align_inout) {
        return false;		/* unchanged */
    }

    *size_inout  = new_size;
    *align_inout = want_align;
    return true;
}

#define PF_LUT_SIZE	64
static const u8 pf_scale_lut[PF_LUT_SIZE] = {
	/* 0-1.6% .. 98-100% mapped to scaling 120-40% */
	#define PF_LUT_ENTRY(i) (120u - (((i) * 100u / PF_LUT_SIZE) * 8u) / 10u)
	PF_LUT_ENTRY(0),  PF_LUT_ENTRY(1),  PF_LUT_ENTRY(2),  PF_LUT_ENTRY(3),
	PF_LUT_ENTRY(4),  PF_LUT_ENTRY(5),  PF_LUT_ENTRY(6),  PF_LUT_ENTRY(7),
	PF_LUT_ENTRY(8),  PF_LUT_ENTRY(9),  PF_LUT_ENTRY(10), PF_LUT_ENTRY(11),
	PF_LUT_ENTRY(12), PF_LUT_ENTRY(13), PF_LUT_ENTRY(14), PF_LUT_ENTRY(15),
	PF_LUT_ENTRY(16), PF_LUT_ENTRY(17), PF_LUT_ENTRY(18), PF_LUT_ENTRY(19),
	PF_LUT_ENTRY(20), PF_LUT_ENTRY(21), PF_LUT_ENTRY(22), PF_LUT_ENTRY(23),
	PF_LUT_ENTRY(24), PF_LUT_ENTRY(25), PF_LUT_ENTRY(26), PF_LUT_ENTRY(27),
	PF_LUT_ENTRY(28), PF_LUT_ENTRY(29), PF_LUT_ENTRY(30), PF_LUT_ENTRY(31),
	PF_LUT_ENTRY(32), PF_LUT_ENTRY(33), PF_LUT_ENTRY(34), PF_LUT_ENTRY(35),
	PF_LUT_ENTRY(36), PF_LUT_ENTRY(37), PF_LUT_ENTRY(38), PF_LUT_ENTRY(39),
	PF_LUT_ENTRY(40), PF_LUT_ENTRY(41), PF_LUT_ENTRY(42), PF_LUT_ENTRY(43),
	PF_LUT_ENTRY(44), PF_LUT_ENTRY(45), PF_LUT_ENTRY(46), PF_LUT_ENTRY(47),
	PF_LUT_ENTRY(48), PF_LUT_ENTRY(49), PF_LUT_ENTRY(50), PF_LUT_ENTRY(51),
	PF_LUT_ENTRY(52), PF_LUT_ENTRY(53), PF_LUT_ENTRY(54), PF_LUT_ENTRY(55),
	PF_LUT_ENTRY(56), PF_LUT_ENTRY(57), PF_LUT_ENTRY(58), PF_LUT_ENTRY(59),
	PF_LUT_ENTRY(60), PF_LUT_ENTRY(61), PF_LUT_ENTRY(62), PF_LUT_ENTRY(63)
	#undef PF_LUT_ENTRY
};

struct vega_prefetch_pc {
    struct amdgpu_bo *last_bo;
    unsigned long last_j;
    u8 streak;  // 0..4 burst length
} ____cacheline_aligned;

static DEFINE_PER_CPU(struct vega_prefetch_pc, vega_prefetch_pc);

static unsigned int
amdgpu_vega_determine_optimal_prefetch(struct amdgpu_device *adev,
                                       struct amdgpu_bo *bo,
                                       unsigned int      base_pages)
{
    u32 vram_pct;
    unsigned int total_pages, want, cap;
    bool is_compute;

    if (!is_hbm2_vega(adev) || !bo || !base_pages) {
        return base_pages;
    }

    total_pages = max(DIV_ROUND_UP(amdgpu_bo_size(bo), PAGE_SIZE), 1u);
    if (!total_pages) {
        return base_pages;
    }

    vram_pct = __amdgpu_vega_get_vram_usage(adev);
    want = (base_pages * pf_scale_lut[min_t(u32, vram_pct * PF_LUT_SIZE / 100, PF_LUT_SIZE - 1)]) / 100;

    if (total_pages > 1) {
        want += ilog2(total_pages);
    }

    is_compute = bo->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS;
    if (is_compute && vram_pct < amdgpu_vega_vram_pressure_high) {
        want += want >> 2;
    }

    cap = (vram_pct < amdgpu_vega_vram_pressure_mid) ? 64 : (vram_pct < amdgpu_vega_vram_pressure_high) ? 48 : 24;
    cap = max(cap - (vram_pct / 5), 24u);

    // Incorporated OpenAI idea: Streak ramp for bursts
    {
        struct vega_prefetch_pc *pc = this_cpu_ptr(&vega_prefetch_pc);
        unsigned long now_j = jiffies;
        if (pc->last_bo == bo && time_before(now_j, pc->last_j + HZ / 200)) {
            if (pc->streak < 4) pc->streak++;
        } else {
            pc->streak = 0;
        }
        pc->last_bo = bo;
        pc->last_j = now_j;
        want += (want * pc->streak) >> 1;  // Up to +200%
    }

    // Incorporated OpenAI idea: Fairness throttling via bias
    u32 bias = pb_get_bias();
    if (bias >= 8) {
        u32 reduc = min(bias, 12u);
        want = (want * (16u - reduc)) >> 4;
        if (want == 0) want = 1;
    }

    // My original multiplier, refined
    u32 multiplier = likely(current) && task_nice(current) <= 0 && (bo->flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) ? 2 : 1;
    cap = min(cap * multiplier, UINT_MAX);

    cap = min(cap, total_pages);
    want = min3(want, cap, total_pages);

    return max(want, 1u);
}

static bool amdgpu_vega_optimize_for_workload(struct amdgpu_device *adev,
											  struct amdgpu_bo *bo,
											  uint64_t flags)
{
	u64 size;

	if (!is_hbm2_vega(adev) || !bo || !bo->tbo.base.dev) {
		return false;
	}

	size = amdgpu_bo_size(bo);
	if (!size) {
		return false;
	}

	if (!dma_resv_is_locked(bo->tbo.base.resv)) {
		return false;
	}

	if (is_vega_texture(flags) && size >= SZ_4M) {
		bo->preferred_domains = AMDGPU_GEM_DOMAIN_VRAM;
		bo->allowed_domains   = AMDGPU_GEM_DOMAIN_VRAM |
		AMDGPU_GEM_DOMAIN_GTT;
		return true;
	}

	if (is_vega_compute(flags) && !is_vega_cpu_access(flags)) {
		bo->preferred_domains = AMDGPU_GEM_DOMAIN_VRAM;
		bo->allowed_domains   = AMDGPU_GEM_DOMAIN_VRAM |
		AMDGPU_GEM_DOMAIN_GTT;
		return true;
	}

	if (is_vega_cpu_access(flags) && size <= SZ_1M) {
		bo->preferred_domains = AMDGPU_GEM_DOMAIN_GTT;
		bo->allowed_domains   = AMDGPU_GEM_DOMAIN_GTT |
		AMDGPU_GEM_DOMAIN_VRAM;
		return true;
	}

	return false;
}

unsigned long amdgpu_gem_timeout(uint64_t timeout_ns)
{
	u64 now = KTIME_FAST_NS();
	s64 delta_ns;

	if ((s64)timeout_ns < 0)
		return MAX_SCHEDULE_TIMEOUT;

	if (timeout_ns <= now) {
		return 0;
	}

	delta_ns = timeout_ns - now;

	if (unlikely(div64_u64(delta_ns, NSEC_PER_SEC) >= (u64)MAX_SCHEDULE_TIMEOUT - 1))
		return MAX_SCHEDULE_TIMEOUT - 1;

	return max_t(unsigned long, 1ul, nsecs_to_jiffies(delta_ns));
}

static const uint16_t pitch_mask_lut[5] = { 0, 255, 127, 63, 63 };

static int
amdgpu_gem_align_pitch(struct amdgpu_device *adev,
                       int width, int cpp, bool tiled)
{
    int mask, aligned_width, result;

    if (unlikely(width <= 0 || cpp <= 0 || cpp >= ARRAY_SIZE(pitch_mask_lut)))
        return -EINVAL;

    mask = pitch_mask_lut[cpp];
    if (mask < 0)
        return -EINVAL;

    if (unlikely(width > INT_MAX - mask))
        return -EINVAL;

    aligned_width = (width + mask) & ~mask;

    #if defined(CONFIG_X86_64) && defined(CONFIG_X86_FEATURE_BMI2)
    if (cpu_has(cpu, X86_FEATURE_BMI2)) {
        u32 log2_align = __builtin_tzcnt(aligned_width) ^ 31;
        aligned_width = 1u << log2_align;
    } else {
    #endif
        // Fallback scalar roundup
        aligned_width = roundup_pow_of_two(aligned_width);
    #if defined(CONFIG_X86_64) && defined(CONFIG_X86_FEATURE_BMI2)
    }
    #endif

    if (unlikely(check_mul_overflow(aligned_width, cpp, &result)))
        return -EINVAL;

    return result;
}

static vm_fault_t amdgpu_gem_fault(struct vm_fault *vmf)
{
    struct ttm_buffer_object *bo;
    struct drm_device        *ddev;
    vm_fault_t                ret;
    int                       idx;

    /* -------- 0. Sanity -------------------------------------------------- */
    if (unlikely(!vmf || !vmf->vma))
        return VM_FAULT_SIGBUS;

    bo = vmf->vma->vm_private_data;
    if (unlikely(!bo))
        return VM_FAULT_SIGBUS;

    ddev = bo->base.dev;
    if (unlikely(!ddev))
        return VM_FAULT_SIGBUS;

    /* -------- 1. Reserve object ----------------------------------------- */
    if (dma_resv_trylock(bo->base.resv)) {
        // Fast path: Already locked or uncontended
    } else {
        unsigned long timeout = jiffies + msecs_to_jiffies(10);
        do {
            ret = ttm_bo_vm_reserve(bo, vmf);
            if (unlikely(ret))
                return ret;
        } while (time_before(jiffies, timeout) && ret == -EAGAIN);
        if (ret == -EAGAIN) return VM_FAULT_SIGBUS;
    }

    /* -------- 2. Enter DRM device  -------------------------------------- */
    if (!drm_dev_enter(ddev, &idx)) {
        ret = ttm_bo_vm_dummy_page(vmf, vmf->vma->vm_page_prot);
        goto out_unlock;
    }

    {
        struct amdgpu_device *adev = drm_to_adev(ddev);
        struct amdgpu_bo     *abo  = ttm_to_amdgpu_bo(bo);
        unsigned int          prefetch_pages = TTM_BO_VM_NUM_PREFAULT;

        /* 2.a  Notify TTM we are about to fault-in */
        ret = amdgpu_bo_fault_reserve_notify(bo);
        if (unlikely(ret)) {
            drm_dev_exit(idx);
            goto out_unlock;
        }

        /* 2.b  Heuristic prefetch ------------------------------------------------ */
        if (static_branch_unlikely(&vega_prefetch_key) && abo && adev) {
            prefetch_pages =
            amdgpu_vega_determine_optimal_prefetch(adev, abo,
                                                   prefetch_pages);
            prefetch_pages = max(prefetch_pages, 1u);
        }

        /* 2.c  Perform the real fault-in now (synchronous path) --------- */
        ret = ttm_bo_vm_fault_reserved(vmf, vmf->vma->vm_page_prot,
                                       prefetch_pages);

        drm_dev_exit(idx);
    }

out_unlock:
    dma_resv_unlock(bo->base.resv);
    return ret;
}

static const struct vm_operations_struct amdgpu_gem_vm_ops = {
	.fault = amdgpu_gem_fault,
	.open = ttm_bo_vm_open,
	.close = ttm_bo_vm_close,
	.access = ttm_bo_vm_access,
};

static void amdgpu_gem_object_free(struct drm_gem_object *gobj)
{
	struct amdgpu_bo *aobj;

	if (unlikely(!gobj))
		return;

	aobj = gem_to_amdgpu_bo(gobj);
	if (unlikely(!aobj))
		return;

	if (tbo_cache_put(aobj))
		return;

	amdgpu_hmm_unregister(aobj);
	ttm_bo_put(&aobj->tbo);
}

/**
 * amdgpu_gem_try_cpu_clear - Clear small VRAM buffers on CPU
 * @bo: The buffer object to clear
 *
 * For small VRAM buffers that need clearing, it's often faster to map them
 * to CPU and clear them there rather than submitting a GPU command.
 * This is especially true for Vega's HBM2 where small GPU commands have
 * high overhead. This avoids polluting the CPU cache by using a temporary
 * atomic mapping.
 *
 * Returns true if successfully cleared, false if GPU clear is needed.
 */
static bool amdgpu_gem_try_cpu_clear(struct amdgpu_bo *bo)
{
    static const u32 thresh_lut[] = {
        [VEGA_VRAM_GREEN]  =  64 * 1024,
        [VEGA_VRAM_YELLOW] = 128 * 1024,
        [VEGA_VRAM_RED]    =  32 * 1024,
    };
    struct ttm_operation_ctx ctx = { .interruptible = true, .no_wait_gpu = false };
    struct amdgpu_device *adev;
    enum vega_vram_pressure_state ps;
    u32 size, thresh;
    void *cpu_addr;
    int r;

    if (unlikely(!bo)) {
        return false;
    }

    size = amdgpu_bo_size(bo);
    adev = amdgpu_ttm_adev(bo->tbo.bdev);
    ps   = amdgpu_vega_get_vram_pressure_state(adev);
    thresh = min(thresh_lut[ps], 256U * 1024U);

    if (size > thresh) {
        return false;
    }

    if (!(bo->preferred_domains & AMDGPU_GEM_DOMAIN_VRAM) ||
        (bo->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS)) {
        return false;
    }

    r = amdgpu_bo_reserve(bo, false);
    if (unlikely(r)) {
        return false;
    }

    amdgpu_bo_placement_from_domain(bo, AMDGPU_GEM_DOMAIN_VRAM);
    r = ttm_bo_validate(&bo->tbo, &bo->placement, &ctx);
    if (unlikely(r)) {
        goto out_unreserve;
    }

    r = amdgpu_bo_kmap(bo, &cpu_addr);
    if (unlikely(r)) {
        goto out_unreserve;
    }

    if (IS_ALIGNED((uintptr_t)cpu_addr, PAGE_SIZE)) {
        u32 pages = size >> PAGE_SHIFT;
        u32 tail  = size & (PAGE_SIZE - 1);
        u8 *p = cpu_addr;

        for (u32 i = 0; i < pages; ++i) {
            clear_page(p);
            p += PAGE_SIZE;
        }
        if (tail) {
            memset(p, 0, tail);
        }
    } else {
        memset(cpu_addr, 0, size);
    }

    // Incorporated fix: wmb for WC write ordering (ensures clears visible before unmap)
    wmb();

    amdgpu_bo_kunmap(bo);

out_unreserve:
    amdgpu_bo_unreserve(bo);
    return r == 0;
}

static void amdgpu_vega_fused_optimize(struct amdgpu_device *adev, struct amdgpu_bo_param *bp, u64 size, u64 flags)
{
    u32 domain = bp->domain;
    u32 align = bp->byte_align;

    if (size == 0 || !is_hbm2_vega(adev)) return;

    // Incorporated: Slack-based padding for nearby alignments (clever for fragmentation)
    u32 want_align = (size >= SZ_256M) ? SZ_64M : (size >= SZ_32M) ? SZ_8M : SZ_1M;
    want_align = max(want_align, align);
    want_align = is_power_of_2(want_align) ? want_align : roundup_pow_of_two(want_align);
    if (want_align <= 1) return;

    u64 new_size;
    if (!check_add_overflow(size, (u64)want_align - 1, &new_size)) {
        u64 aligned_once = ALIGN(size, want_align);
        u64 rem = aligned_once - size;
        u64 slack = max_t(u64, want_align >> 3, (u64)SZ_256K);
        if (rem > 0 && rem <= slack) {
            u64 tmp;
            if (!check_add_overflow(aligned_once, (u64)want_align, &tmp)) {
                aligned_once = tmp;
            }
        }
        bp->size = aligned_once;
        bp->byte_align = want_align;
    }

    // Inline placement (exact original logic)
    u32 want_dom;
    if ((flags & AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED) && size <= SZ_256K) {
        want_dom = AMDGPU_GEM_DOMAIN_GTT;
    } else if (flags & (AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS | AMDGPU_GEM_CREATE_NO_CPU_ACCESS)) {
        want_dom = AMDGPU_GEM_DOMAIN_VRAM;
    } else {
        want_dom = domain & (AMDGPU_GEM_DOMAIN_VRAM | AMDGPU_GEM_DOMAIN_GTT);
        if (want_dom != AMDGPU_GEM_DOMAIN_VRAM && want_dom != AMDGPU_GEM_DOMAIN_GTT) {
            want_dom = AMDGPU_GEM_DOMAIN_VRAM;
        }
    }

    enum vega_vram_pressure_state ps = amdgpu_vega_get_vram_pressure_state(adev);
    if (ps == VEGA_VRAM_RED && size > SZ_1M) want_dom = AMDGPU_GEM_DOMAIN_GTT;
    else if (ps == VEGA_VRAM_YELLOW && size > SZ_32M) want_dom = AMDGPU_GEM_DOMAIN_GTT;
    if (task_nice(current) > 0) want_dom = AMDGPU_GEM_DOMAIN_GTT;

    u32 cur_dom = domain & (AMDGPU_GEM_DOMAIN_VRAM | AMDGPU_GEM_DOMAIN_GTT);
    if (cur_dom != want_dom) {
        bp->domain = (domain & ~(AMDGPU_GEM_DOMAIN_VRAM | AMDGPU_GEM_DOMAIN_GTT)) | want_dom;
        if (cur_dom == AMDGPU_GEM_DOMAIN_VRAM && want_dom == AMDGPU_GEM_DOMAIN_GTT) pb_account_eviction();
    }
}

int amdgpu_gem_object_create(struct amdgpu_device *adev,
                             unsigned long size, int alignment,
                             u32 initial_domain, u64 flags,
                             enum ttm_bo_type type, struct dma_resv *resv,
                             struct drm_gem_object **obj,
                             int8_t xcp_id_plus1)
{
    struct amdgpu_bo_param bp = { 0 };
    struct amdgpu_bo_user *ubo = NULL;
    struct amdgpu_bo *bo       = NULL;
    u32   byte_align;
    int   r;

    if (WARN_ON(!adev || !obj))
        return -EINVAL;

    *obj = NULL;

    if (!size ||
        size > adev->gmc.mc_vram_size + adev->gmc.gart_size)
        return -EINVAL;

    size = ALIGN(size, PAGE_SIZE);

    if (alignment < 0)
        alignment = 0;

    amdgpu_tbo_slab_ensure();

    if (flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID)
        amdgpu_vm_always_valid_key_enable();

    bp.size             = size;
    bp.byte_align       = alignment;
    bp.type             = type;
    bp.resv             = resv;
    bp.preferred_domain = initial_domain;
    bp.flags            = flags;
    bp.bo_ptr_size      = sizeof(struct amdgpu_bo);
    bp.xcp_id_plus1     = xcp_id_plus1;
    bp.domain           = initial_domain;

    if (static_branch_unlikely(&vega_domain_key)) {
        amdgpu_vega_fused_optimize(adev, &bp, size, flags);  // Integrated call
    }

    bo = tbo_cache_try_get(size, flags, bp.domain, resv, alignment);
    if (bo) {
        bo->flags             = flags;
        bo->preferred_domains = bp.domain;
        bo->allowed_domains   = bp.domain;
        if (bo->allowed_domains == AMDGPU_GEM_DOMAIN_VRAM)
            bo->allowed_domains |= AMDGPU_GEM_DOMAIN_GTT;

        byte_align                = max(alignment, (int)PAGE_SIZE);
        bo->tbo.page_alignment    = roundup_pow_of_two(byte_align) >>
        PAGE_SHIFT;

        *obj = &bo->tbo.base;
        return 0;
    }

    if (ubo_slab)
        ubo = kmem_cache_zalloc(ubo_slab, GFP_KERNEL | __GFP_NOWARN);

    if (!ubo) {
        r = amdgpu_bo_create(adev, &bp, &bo);
        if (r)
            return r;
    } else {
        r = amdgpu_bo_create_user(adev, &bp, &ubo);
        if (r) {
            kmem_cache_free(ubo_slab, ubo);
            return r;
        }
        bo = &ubo->bo;
    }

    if (bo->flags & AMDGPU_GEM_CREATE_VRAM_CLEARED) {
        if (amdgpu_gem_try_cpu_clear(bo))
            bo->flags &= ~AMDGPU_GEM_CREATE_VRAM_CLEARED;
    }

    *obj = &bo->tbo.base;
    return 0;
}

void amdgpu_gem_force_release(struct amdgpu_device *adev)
{
	struct drm_device *ddev;
	struct drm_file *file;

	if (unlikely(!adev))
		return;

	ddev = adev_to_drm(adev);
	if (unlikely(!ddev))
		return;

	mutex_lock(&ddev->filelist_mutex);

	list_for_each_entry(file, &ddev->filelist, lhead) {
		struct drm_gem_object *gobj;
		int handle;

		struct release_node {
			struct list_head node;
			struct drm_gem_object *obj;
		};
		LIST_HEAD(release_list);
		struct release_node *n, *tmp;

		#define OOM_BATCH_SIZE 32
		struct drm_gem_object *oom_objs[OOM_BATCH_SIZE];
		unsigned int oom_cnt = 0;

		WARN_ONCE(1, "Force-releasing GEM objects for file %p\n", file);

		spin_lock(&file->table_lock);
		idr_for_each_entry(&file->object_idr, gobj, handle) {
			if (!kref_get_unless_zero(&gobj->refcount))
				continue;

			n = kmalloc(sizeof(*n), GFP_ATOMIC);
			if (likely(n)) {
				n->obj = gobj;
				list_add_tail(&n->node, &release_list);
			} else if (oom_cnt < OOM_BATCH_SIZE) {
				oom_objs[oom_cnt++] = gobj;
			} else {
				dev_warn_once(ddev->dev, "OOM in force_release, potential leak\n");
				drm_gem_object_put(gobj);
			}
		}
		idr_destroy(&file->object_idr);
		spin_unlock(&file->table_lock);

		list_for_each_entry_safe(n, tmp, &release_list, node) {
			drm_gem_object_put(n->obj);
			kfree(n);
		}

		for (unsigned int i = 0; i < oom_cnt; ++i)
			drm_gem_object_put(oom_objs[i]);
	}

	mutex_unlock(&ddev->filelist_mutex);
}

static int amdgpu_gem_object_open(struct drm_gem_object *obj,
                                  struct drm_file *file_priv)
{
    struct amdgpu_bo *abo;
    struct amdgpu_device *adev;
    struct amdgpu_fpriv *fpriv;
    struct amdgpu_vm *vm;
    struct amdgpu_bo_va *bo_va;
    struct mm_struct *mm;
    int r = 0;

    if (unlikely(!obj || !file_priv))
        return -EINVAL;

    abo = gem_to_amdgpu_bo(obj);
    if (unlikely(!abo))
        return -EINVAL;

    adev = amdgpu_ttm_adev(abo->tbo.bdev);
    fpriv = file_priv->driver_priv;
    if (unlikely(!adev || !fpriv))
        return -EINVAL;

    vm = &fpriv->vm;

    if (static_branch_unlikely(&vega_prefetch_key)) {
        prefetch(abo);
        prefetch(vm);
        prefetch(abo->tbo.base.resv);
    }

    rcu_read_lock();
    mm = amdgpu_ttm_tt_get_usermm(abo->tbo.ttm);
    if (mm && mm != current->mm) {
        rcu_read_unlock();
        return -EPERM;
    }
    rcu_read_unlock();

    if ((abo->flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) &&
        !amdgpu_vm_is_bo_always_valid(vm, abo))
        return -EPERM;

    if (static_branch_likely(&amdgpu_vm_always_valid_key) &&
        (abo->flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) &&
        (abo->allowed_domains == AMDGPU_GEM_DOMAIN_GTT) &&
        !abo->parent &&
        (!obj->import_attach || (obj->import_attach && !dma_buf_is_dynamic(obj->import_attach->dmabuf))))
        return 0;

    r = amdgpu_bo_reserve(abo, false);
    if (unlikely(r))
        return r;

    bo_va = amdgpu_vm_bo_find(vm, abo);
    if (!bo_va) {
        bo_va = amdgpu_vm_bo_add(adev, vm, abo);
        if (IS_ERR(bo_va)) {
            r = PTR_ERR(bo_va);
            goto unreserve;
        }
    } else {
        if (bo_va->ref_count < UINT_MAX)
            bo_va->ref_count++;
        else
            DRM_WARN("amdgpu: ref_count saturated - potential overflow\n");
    }

    amdgpu_bo_unreserve(abo);

    if (vm->is_compute_context && vm->process_info &&
        obj->import_attach && dma_buf_is_dynamic(obj->import_attach->dmabuf)) {
        mutex_lock_nested(&vm->process_info->lock, 1);
        if (vm->process_info->eviction_fence) {
            r = amdgpu_amdkfd_bo_validate_and_fence(
                abo, AMDGPU_GEM_DOMAIN_GTT,
                &vm->process_info->eviction_fence->base);

            if (unlikely(r)) {
                struct amdgpu_task_info *ti = amdgpu_vm_get_task_info_vm(vm);

                dev_warn(adev->dev, "validate_and_fence failed: %d\n", r);
                if (ti) {
                    dev_warn(adev->dev, "pid %d\n", ti->pid);
                    amdgpu_vm_put_task_info(ti);
                }
            }
        }
        mutex_unlock(&vm->process_info->lock);
    }

    return r;

unreserve:
    amdgpu_bo_unreserve(abo);
    return r;
}

static void
amdgpu_gem_object_close(struct drm_gem_object *obj, struct drm_file *file_priv)
{
	struct amdgpu_bo *bo;
	struct amdgpu_device *adev;
	struct amdgpu_fpriv *fpriv;
	struct amdgpu_vm *vm;
	struct dma_fence *fence = NULL;
	struct amdgpu_bo_va *bo_va;
	struct drm_exec exec;
	long r = 0;
	bool use_async = false;

	if (unlikely(!obj || !file_priv))
		return;

	bo = gem_to_amdgpu_bo(obj);
	if (unlikely(!bo))
		return;

	fpriv = file_priv->driver_priv;
	if (unlikely(!fpriv))
		return;

	adev = amdgpu_ttm_adev(bo->tbo.bdev);
	if (unlikely(!adev))
		return;

	vm = &fpriv->vm;

	if (static_branch_unlikely(&vega_prefetch_key)) {
		prefetchw(bo->tbo.base.resv);
		prefetch(vm);
	}

	if (is_hbm2_vega(adev)) {
		use_async = (bo->flags & AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED) ||
		(amdgpu_bo_size(bo) < (256ULL << 10));
	}

	drm_exec_init(&exec, DRM_EXEC_IGNORE_DUPLICATES, 0);
	drm_exec_until_all_locked(&exec) {
		r = amdgpu_vm_lock_pd(vm, &exec, 1);
		drm_exec_retry_on_contention(&exec);
		if (unlikely(r))
			goto out_unlock;

		r = drm_exec_lock_obj(&exec, &bo->tbo.base);
		drm_exec_retry_on_contention(&exec);
		if (unlikely(r))
			goto out_unlock;
	}

	bo_va = amdgpu_vm_bo_find(vm, bo);
	if (!bo_va || bo_va->ref_count == 0)
		goto out_unlock;

	if (--bo_va->ref_count > 0)
		goto out_unlock;

	amdgpu_vm_bo_del(adev, bo_va);
	amdgpu_vm_bo_update_shared(bo);

	if (amdgpu_vm_ready(vm)) {
		r = amdgpu_vm_clear_freed(adev, vm, &fence);
		if (unlikely(r < 0)) {
			dev_err(adev->dev, "failed to clear page tables on GEM close (%ld)\n", r);
			goto out_unlock;
		}
	}

	if (fence) {
		amdgpu_bo_fence(bo, fence, use_async);
		dma_fence_put(fence);
	}

	out_unlock:
	if (unlikely(r)) {
		dev_err(adev->dev, "Error in GEM object close for pid %d (%ld)\n",
				task_pid_nr(current), r);
	}
	drm_exec_fini(&exec);
}

static int amdgpu_gem_object_mmap(struct drm_gem_object *obj, struct vm_area_struct *vma)
{
	struct amdgpu_bo *bo;

	if (unlikely(!obj))
		return -EINVAL;

	bo = gem_to_amdgpu_bo(obj);
	if (unlikely(!bo))
		return -EINVAL;

	if (amdgpu_ttm_tt_get_usermm(bo->tbo.ttm) ||
		(bo->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS))
		return -EPERM;

	if (is_cow_mapping(vma->vm_flags) && !(vma->vm_flags & VM_ACCESS_FLAGS))
		vm_flags_clear(vma, VM_MAYWRITE);

	return drm_gem_ttm_mmap(obj, vma);
}

const struct drm_gem_object_funcs amdgpu_gem_object_funcs = {
	.free = amdgpu_gem_object_free,
	.open = amdgpu_gem_object_open,
	.close = amdgpu_gem_object_close,
	.export = amdgpu_gem_prime_export,
	.vmap = drm_gem_ttm_vmap,
	.vunmap = drm_gem_ttm_vunmap,
	.mmap = amdgpu_gem_object_mmap,
	.vm_ops = &amdgpu_gem_vm_ops,
};

int amdgpu_gem_create_ioctl(struct drm_device *dev, void *data,
                            struct drm_file *filp)
{
    struct amdgpu_device *adev;
    struct amdgpu_fpriv *fpriv;
    struct amdgpu_vm *vm;
    union drm_amdgpu_gem_create *args = data;
    uint64_t flags, size;
    struct dma_resv *resv = NULL;
    struct drm_gem_object *gobj = NULL;
    uint32_t handle;
    int r = 0;
    unsigned int i;

    if (unlikely(!dev || !data || !filp))
        return -EINVAL;

    adev = drm_to_adev(dev);
    fpriv = filp->driver_priv;
    if (unlikely(!adev || !fpriv))
        return -EINVAL;

    vm = &fpriv->vm;
    flags = args->in.domain_flags;
    size = args->in.bo_size;

    if (unlikely(args->in.domains & AMDGPU_GEM_DOMAIN_DOORBELL ||
                 args->in.domains & ~AMDGPU_GEM_DOMAIN_MASK))
        return -EINVAL;

    if (unlikely(flags & ~(AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED |
                           AMDGPU_GEM_CREATE_NO_CPU_ACCESS |
                           AMDGPU_GEM_CREATE_CPU_GTT_USWC |
                           AMDGPU_GEM_CREATE_VRAM_CLEARED |
                           AMDGPU_GEM_CREATE_VM_ALWAYS_VALID |
                           AMDGPU_GEM_CREATE_EXPLICIT_SYNC |
                           AMDGPU_GEM_CREATE_ENCRYPTED |
                           AMDGPU_GEM_CREATE_GFX12_DCC |
                           AMDGPU_GEM_CREATE_DISCARDABLE)))
        return -EINVAL;

    if (unlikely(!amdgpu_is_tmz(adev) && (flags & AMDGPU_GEM_CREATE_ENCRYPTED)))
        return -EINVAL;

    if (args->in.domains & (AMDGPU_GEM_DOMAIN_GDS |
                            AMDGPU_GEM_DOMAIN_GWS |
                            AMDGPU_GEM_DOMAIN_OA))
        flags |= AMDGPU_GEM_CREATE_NO_CPU_ACCESS;

    if (flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) {
        r = amdgpu_bo_reserve(vm->root.bo, false);
        if (unlikely(r))
            return r;
        resv = vm->root.bo->tbo.base.resv;
    }

    uint32_t alloc_domain = (u32)(args->in.domains);

    for (i = 0; i < 2; ++i) {
        r = amdgpu_gem_object_create(adev, size, args->in.alignment,
                                     alloc_domain, flags,
                                     ttm_bo_type_device, resv, &gobj,
                                     fpriv->xcp_id + 1);

        if (likely(!r)) {
            /* Success on the first or second attempt. */
            break;
        } else {
            /* Allocation failed, check if we can retry. */
            if (r == -ENOMEM && ((alloc_domain & AMDGPU_GEM_DOMAIN_VRAM) != 0) && i == 0) {
                /* Failed in VRAM due to no space. Try GTT-only next. */
                alloc_domain = AMDGPU_GEM_DOMAIN_GTT;
                continue;
            } else {
                /* Any other error is unrecoverable, exit the loop. */
                if (r == -ENOMEM && i == 1) {
                    DRM_WARN("amdgpu: GEM create failed after retry - possible low memory at boot\n");
                }
                goto out_unreserve;
            }
        }
    }

out_unreserve:
    if (flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) {
        if (likely(!r)) {
            struct amdgpu_bo *abo = gem_to_amdgpu_bo(gobj);

            if (likely(abo))
                abo->parent = amdgpu_bo_ref(vm->root.bo);
        }
        amdgpu_bo_unreserve(vm->root.bo);
    }
    if (unlikely(r))
        return r;

    r = drm_gem_handle_create(filp, gobj, &handle);
    drm_gem_object_put(gobj);
    if (unlikely(r))
        return r;

    memset(args, 0, sizeof(*args));
    args->out.handle = handle;
    return 0;
}

int amdgpu_gem_userptr_ioctl(struct drm_device *dev, void *data,
							 struct drm_file *filp)
{
	struct ttm_operation_ctx ctx = { true, false };
	struct amdgpu_device *adev;
	struct drm_amdgpu_gem_userptr *args = data;
	struct amdgpu_fpriv *fpriv;
	struct drm_gem_object *gobj;
	struct hmm_range *range = NULL;
	struct amdgpu_bo *bo;
	uint32_t handle;
	int r;

	if (unlikely(!dev || !data || !filp))
		return -EINVAL;

	adev = drm_to_adev(dev);
	fpriv = filp->driver_priv;
	if (unlikely(!adev || !fpriv))
		return -EINVAL;

	args->addr = untagged_addr(args->addr);

	if (unlikely(offset_in_page(args->addr | args->size) || !args->size))
		return -EINVAL;

	if (unlikely(args->flags & ~(AMDGPU_GEM_USERPTR_READONLY |
		AMDGPU_GEM_USERPTR_ANONONLY |
		AMDGPU_GEM_USERPTR_VALIDATE |
		AMDGPU_GEM_USERPTR_REGISTER)))
		return -EINVAL;

	if (unlikely(!(args->flags & AMDGPU_GEM_USERPTR_READONLY) &&
		!(args->flags & AMDGPU_GEM_USERPTR_REGISTER)))
		return -EACCES;

	r = amdgpu_gem_object_create(adev, args->size, 0, AMDGPU_GEM_DOMAIN_CPU,
								 0, ttm_bo_type_device, NULL, &gobj, fpriv->xcp_id + 1);
	if (unlikely(r))
		return r;

	bo = gem_to_amdgpu_bo(gobj);
	bo->preferred_domains = AMDGPU_GEM_DOMAIN_GTT;
	bo->allowed_domains = AMDGPU_GEM_DOMAIN_GTT;

	r = amdgpu_ttm_tt_set_userptr(&bo->tbo, args->addr, args->flags);
	if (unlikely(r))
		goto release_object;

	r = amdgpu_hmm_register(bo, args->addr);
	if (unlikely(r))
		goto release_object;

	if (args->flags & AMDGPU_GEM_USERPTR_VALIDATE) {
		r = amdgpu_ttm_tt_get_user_pages(bo, bo->tbo.ttm->pages, &range);
		if (unlikely(r))
			goto release_object;

		r = amdgpu_bo_reserve(bo, true);
		if (unlikely(r))
			goto user_pages_done;

		amdgpu_bo_placement_from_domain(bo, AMDGPU_GEM_DOMAIN_GTT);
		r = ttm_bo_validate(&bo->tbo, &bo->placement, &ctx);
		amdgpu_bo_unreserve(bo);
		if (unlikely(r))
			goto user_pages_done;
	}

	r = drm_gem_handle_create(filp, gobj, &handle);
	if (unlikely(r))
		goto user_pages_done;

	args->handle = handle;

	user_pages_done:
	if ((args->flags & AMDGPU_GEM_USERPTR_VALIDATE) && range)
		amdgpu_ttm_tt_get_user_pages_done(bo->tbo.ttm, range);

	release_object:
	drm_gem_object_put(gobj);
	return r;
}

int amdgpu_mode_dumb_mmap(struct drm_file *filp,
						  struct drm_device *dev,
						  uint32_t handle, uint64_t *offset_p)
{
	struct drm_gem_object *gobj;
	struct amdgpu_bo *robj;

	if (unlikely(!filp || !dev || !offset_p))
		return -EINVAL;

	gobj = drm_gem_object_lookup(filp, handle);
	if (unlikely(!gobj))
		return -ENOENT;

	robj = gem_to_amdgpu_bo(gobj);
	if (unlikely(!robj)) {
		drm_gem_object_put(gobj);
		return -EINVAL;
	}

	if (amdgpu_ttm_tt_get_usermm(robj->tbo.ttm) ||
		(robj->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS)) {
		drm_gem_object_put(gobj);
	return -EPERM;
		}
		*offset_p = amdgpu_bo_mmap_offset(robj);
		drm_gem_object_put(gobj);
		return 0;
}

int amdgpu_gem_mmap_ioctl(struct drm_device *dev, void *data,
						  struct drm_file *filp)
{
	union drm_amdgpu_gem_mmap *args = data;
	uint32_t handle;

	if (unlikely(!dev || !data || !filp))
		return -EINVAL;

	handle = args->in.handle;
	memset(args, 0, sizeof(*args));
	return amdgpu_mode_dumb_mmap(filp, dev, handle, &args->out.addr_ptr);
}

int amdgpu_gem_wait_idle_ioctl(struct drm_device *dev, void *data,
							   struct drm_file *filp)
{
	union drm_amdgpu_gem_wait_idle *args = data;
	struct drm_gem_object *gobj;
	struct amdgpu_bo *robj;
	uint32_t handle;
	unsigned long timeout;
	int r = 0;
	long ret;

	if (unlikely(!dev || !data || !filp))
		return -EINVAL;

	handle = args->in.handle;
	timeout = amdgpu_gem_timeout(args->in.timeout);

	gobj = drm_gem_object_lookup(filp, handle);
	if (unlikely(!gobj))
		return -ENOENT;

	robj = gem_to_amdgpu_bo(gobj);
	if (unlikely(!robj)) {
		drm_gem_object_put(gobj);
		return -EINVAL;
	}

	ret = dma_resv_wait_timeout(robj->tbo.base.resv, DMA_RESV_USAGE_READ,
								true, timeout);

	if (ret >= 0) {
		memset(args, 0, sizeof(*args));
		args->out.status = (ret == 0);
	} else {
		r = ret;
	}

	drm_gem_object_put(gobj);
	return r;
}

int amdgpu_gem_metadata_ioctl(struct drm_device *dev, void *data,
							  struct drm_file *filp)
{
	struct drm_amdgpu_gem_metadata *args = data;
	struct drm_gem_object *gobj;
	struct amdgpu_bo *robj;
	int r = 0;

	if (unlikely(!dev || !data || !filp))
		return -EINVAL;

	gobj = drm_gem_object_lookup(filp, args->handle);
	if (unlikely(!gobj))
		return -ENOENT;

	robj = gem_to_amdgpu_bo(gobj);
	if (unlikely(!robj)) {
		drm_gem_object_put(gobj);
		return -EINVAL;
	}

	r = amdgpu_bo_reserve(robj, false);
	if (unlikely(r != 0))
		goto out;

	switch (args->op) {
		case AMDGPU_GEM_METADATA_OP_GET_METADATA:
			amdgpu_bo_get_tiling_flags(robj, &args->data.tiling_info);
			r = amdgpu_bo_get_metadata(robj, args->data.data,
									   sizeof(args->data.data),
									   &args->data.data_size_bytes,
							  &args->data.flags);
			break;

		case AMDGPU_GEM_METADATA_OP_SET_METADATA:
			if (unlikely(args->data.data_size_bytes > sizeof(args->data.data))) {
				r = -EINVAL;
				break;
			}
			r = amdgpu_bo_set_tiling_flags(robj, args->data.tiling_info);

			if (!r) {
				r = amdgpu_bo_set_metadata(robj, args->data.data,
										   args->data.data_size_bytes,
							   args->data.flags);
			}
			break;

		default:
			r = -EINVAL;
			break;
	}

	amdgpu_bo_unreserve(robj);
	out:
	drm_gem_object_put(gobj);
	return r;
}

uint64_t amdgpu_gem_va_map_flags(struct amdgpu_device *adev, uint32_t flags)
{
	uint64_t pte_flag = 0;

	if (unlikely(!adev))
		return 0;

	if (flags & AMDGPU_VM_PAGE_EXECUTABLE)
		pte_flag |= AMDGPU_PTE_EXECUTABLE;
	if (flags & AMDGPU_VM_PAGE_READABLE)
		pte_flag |= AMDGPU_PTE_READABLE;
	if (flags & AMDGPU_VM_PAGE_WRITEABLE)
		pte_flag |= AMDGPU_PTE_WRITEABLE;
	if (flags & AMDGPU_VM_PAGE_PRT)
		pte_flag |= AMDGPU_PTE_PRT_FLAG(adev);
	if (flags & AMDGPU_VM_PAGE_NOALLOC)
		pte_flag |= AMDGPU_PTE_NOALLOC;

	if (likely(adev->gmc.gmc_funcs && adev->gmc.gmc_funcs->map_mtype))
		pte_flag |= amdgpu_gmc_map_mtype(adev, flags & AMDGPU_VM_MTYPE_MASK);

	return pte_flag;
}

static int amdgpu_gem_va_update_vm(struct amdgpu_device *adev,
								   struct amdgpu_vm *vm,
								   struct amdgpu_bo_va *bo_va,
								   uint32_t operation)
{
	int r;

	if (unlikely(!adev || !vm))
		return -EINVAL;

	if (!amdgpu_vm_ready(vm))
		return 0;

	r = amdgpu_vm_clear_freed(adev, vm, NULL);
	if (unlikely(r))
		goto error;

	if (operation == AMDGPU_VA_OP_MAP || operation == AMDGPU_VA_OP_REPLACE) {
		r = amdgpu_vm_bo_update(adev, bo_va, false);
		if (unlikely(r))
			goto error;
	}

	r = amdgpu_vm_update_pdes(adev, vm, false);
	if (unlikely(r))
		goto error;

	return 0;

	error:
	if (r && r != -ERESTARTSYS)
		DRM_ERROR("Couldn't update BO_VA (%d)\n", r);
	return r;
}

int amdgpu_gem_va_ioctl(struct drm_device *dev, void *data,
                        struct drm_file *filp)
{
    struct amdgpu_device *adev;
    struct amdgpu_fpriv *fpriv;
    struct amdgpu_vm *vm;
    struct drm_amdgpu_gem_va *args = data;
    struct drm_gem_object *gobj = NULL;
    struct amdgpu_bo *abo = NULL;
    struct amdgpu_bo_va *bo_va = NULL;
    struct drm_exec exec;
    uint64_t va_end;
    uint64_t map_flags;
    int r = 0;

    if (unlikely(!dev || !data || !filp))
        return -EINVAL;

    adev = drm_to_adev(dev);
    fpriv = filp->driver_priv;
    if (unlikely(!adev || !fpriv))
        return -EINVAL;

    vm = &fpriv->vm;

    const u32 VALID_FLAGS = AMDGPU_VM_DELAY_UPDATE |
    AMDGPU_VM_PAGE_READABLE   | AMDGPU_VM_PAGE_WRITEABLE |
    AMDGPU_VM_PAGE_EXECUTABLE | AMDGPU_VM_MTYPE_MASK     |
    AMDGPU_VM_PAGE_NOALLOC;
    const u32 PRT_FLAGS = AMDGPU_VM_DELAY_UPDATE | AMDGPU_VM_PAGE_PRT;

    if (unlikely(args->operation > AMDGPU_VA_OP_REPLACE ||
                 args->va_address < AMDGPU_VA_RESERVED_BOTTOM ||
                 (args->va_address >= AMDGPU_GMC_HOLE_START &&
                  args->va_address <  AMDGPU_GMC_HOLE_END) ||
                 ((args->flags & ~VALID_FLAGS) && (args->flags & ~PRT_FLAGS))))
        return -EINVAL;

    args->va_address &= AMDGPU_GMC_HOLE_MASK;
    uint64_t vm_size = (adev->vm_manager.max_pfn * AMDGPU_GPU_PAGE_SIZE) -
    AMDGPU_VA_RESERVED_TOP;
    if (unlikely(check_add_overflow(args->va_address, args->map_size, &va_end) ||
                 va_end > vm_size))
        return -EINVAL;

    if ((args->operation != AMDGPU_VA_OP_CLEAR) &&
        !(args->flags & AMDGPU_VM_PAGE_PRT)) {
        gobj = drm_gem_object_lookup(filp, args->handle);

        if (unlikely(!gobj)) {
            return -ENOENT;
        }
        abo = gem_to_amdgpu_bo(gobj);
    }

    drm_exec_init(&exec, DRM_EXEC_INTERRUPTIBLE_WAIT | DRM_EXEC_IGNORE_DUPLICATES, 0);
    drm_exec_until_all_locked(&exec) {
        r = drm_exec_lock_obj(&exec, &vm->root.bo->tbo.base);
        drm_exec_retry_on_contention(&exec);
        if (unlikely(r))
            goto out_exec;

        if (abo) {
            r = drm_exec_lock_obj(&exec, gobj);
            drm_exec_retry_on_contention(&exec);
            if (unlikely(r))
                goto out_exec;
        }
    }

    bo_va = abo ? amdgpu_vm_bo_find(vm, abo) : fpriv->prt_va;
    if (!bo_va) {
        if (abo && args->operation == AMDGPU_VA_OP_MAP) {
            bo_va = amdgpu_vm_bo_add(adev, vm, abo);
            if (IS_ERR(bo_va)) {
                r = PTR_ERR(bo_va);
                goto out_exec;
            }
        } else {
            r = -ENOENT;
            goto out_exec;
        }
    }

    switch (args->operation) {
        case AMDGPU_VA_OP_MAP:
        case AMDGPU_VA_OP_REPLACE:
            // Neat trick: BMI2-packed flag fusion if available (reduces branches)
            #if defined(CONFIG_X86_64) && defined(CONFIG_X86_FEATURE_BMI2)
            if (cpu_has(cpu, X86_FEATURE_BMI2)) {
                u64 packed = 0;
                packed |= _pdep_u64((args->flags & AMDGPU_VM_PAGE_EXECUTABLE) ? 1 : 0, 1ULL << 0);
                packed |= _pdep_u64((args->flags & AMDGPU_VM_PAGE_READABLE) ? 1 : 0, 1ULL << 1);
                packed |= _pdep_u64((args->flags & AMDGPU_VM_PAGE_WRITEABLE) ? 1 : 0, 1ULL << 2);
                packed |= _pdep_u64((args->flags & AMDGPU_VM_PAGE_PRT && adev) ? 1 : 0, 1ULL << 3);
                packed |= _pdep_u64((args->flags & AMDGPU_VM_PAGE_NOALLOC) ? 1 : 0, 1ULL << 4);
                // Mtype as bitfield (mask to 3 bits for MTYPE)
                u64 mtype_bits = (args->flags & AMDGPU_VM_MTYPE_MASK) >> __builtin_ctz(AMDGPU_VM_MTYPE_MASK);
                packed |= _pdep_u64(mtype_bits, 7ULL << 5);  // Bits 5-7 for mtype

                map_flags = 0;
                if (packed & (1ULL << 0)) map_flags |= AMDGPU_PTE_EXECUTABLE;
                if (packed & (1ULL << 1)) map_flags |= AMDGPU_PTE_READABLE;
                if (packed & (1ULL << 2)) map_flags |= AMDGPU_PTE_WRITEABLE;
                if (packed & (1ULL << 3)) map_flags |= AMDGPU_PTE_PRT_FLAG(adev);
                if (packed & (1ULL << 4)) map_flags |= AMDGPU_PTE_NOALLOC;
                if (likely(adev->gmc.gmc_funcs && adev->gmc.gmc_funcs->map_mtype)) {
                    u32 mtype = (packed >> 5) & 7;  // Extract mtype bits
                    map_flags |= amdgpu_gmc_map_mtype(adev, mtype << __builtin_ctz(AMDGPU_VM_MTYPE_MASK));
                }
            } else {
            #endif
                // Fallback scalar fusion (if no BMI2 or for safety)
                map_flags = 0;
                map_flags |= (args->flags & AMDGPU_VM_PAGE_EXECUTABLE) ? AMDGPU_PTE_EXECUTABLE : 0;
                map_flags |= (args->flags & AMDGPU_VM_PAGE_READABLE) ? AMDGPU_PTE_READABLE : 0;
                map_flags |= (args->flags & AMDGPU_VM_PAGE_WRITEABLE) ? AMDGPU_PTE_WRITEABLE : 0;
                if (args->flags & AMDGPU_VM_PAGE_PRT && adev)
                    map_flags |= AMDGPU_PTE_PRT_FLAG(adev);
                map_flags |= (args->flags & AMDGPU_VM_PAGE_NOALLOC) ? AMDGPU_PTE_NOALLOC : 0;
                if (likely(adev->gmc.gmc_funcs && adev->gmc.gmc_funcs->map_mtype))
                    map_flags |= amdgpu_gmc_map_mtype(adev, args->flags & AMDGPU_VM_MTYPE_MASK);
            #if defined(CONFIG_X86_64) && defined(CONFIG_X86_FEATURE_BMI2)
            }
            #endif
            if (args->operation == AMDGPU_VA_OP_MAP) {
                r = amdgpu_vm_bo_map(adev, bo_va, args->va_address,
                                     args->offset_in_bo, args->map_size, map_flags);
            } else {
                r = amdgpu_vm_bo_replace_map(adev, bo_va, args->va_address,
                                             args->offset_in_bo, args->map_size, map_flags);
            }
            break;
        case AMDGPU_VA_OP_UNMAP:
            r = amdgpu_vm_bo_unmap(adev, bo_va, args->va_address);
            break;
        case AMDGPU_VA_OP_CLEAR:
            r = amdgpu_vm_bo_clear_mappings(adev, vm, args->va_address, args->map_size);
            break;
        default:
            r = -EINVAL;
            break;
    }

    if (!r && !(args->flags & AMDGPU_VM_DELAY_UPDATE) && !adev->debug_vm)
        r = amdgpu_gem_va_update_vm(adev, vm, bo_va, args->operation);

out_exec:
    drm_exec_fini(&exec);
    if (gobj)
        drm_gem_object_put(gobj);

    return r;
}

int amdgpu_gem_op_ioctl(struct drm_device *dev, void *data,
						struct drm_file *filp)
{
	struct amdgpu_device *adev;
	struct drm_amdgpu_gem_op *args = data;
	struct drm_gem_object *gobj;
	struct amdgpu_bo *robj;
	int r = 0;

	if (unlikely(!dev || !data || !filp))
		return -EINVAL;

	adev = drm_to_adev(dev);
	if (unlikely(!adev))
		return -EINVAL;

	gobj = drm_gem_object_lookup(filp, args->handle);
	if (unlikely(!gobj))
		return -ENOENT;

	robj = gem_to_amdgpu_bo(gobj);
	if (unlikely(!robj)) {
		drm_gem_object_put(gobj);
		return -EINVAL;
	}

	r = amdgpu_bo_reserve(robj, true);
	if (unlikely(r))
		goto out;

	switch (args->op) {
		case AMDGPU_GEM_OP_GET_GEM_CREATE_INFO: {
			struct drm_amdgpu_gem_create_in info = {0};

			info.bo_size = robj->tbo.base.size;
			info.alignment = robj->tbo.page_alignment << PAGE_SHIFT;
			info.domains = robj->preferred_domains;
			info.domain_flags = robj->flags;

			amdgpu_bo_unreserve(robj);
			if (copy_to_user(u64_to_user_ptr(args->value), &info, sizeof(info)))
				r = -EFAULT;
			goto out;
		}
		case AMDGPU_GEM_OP_SET_PLACEMENT: {
			struct amdgpu_vm_bo_base *base;
			uint32_t new_domains = args->value & (AMDGPU_GEM_DOMAIN_VRAM |
			AMDGPU_GEM_DOMAIN_GTT |
			AMDGPU_GEM_DOMAIN_CPU);
			if (unlikely(!new_domains)) {
				r = -EINVAL;
				break;
			}

			if (unlikely(robj->tbo.base.import_attach && (new_domains & AMDGPU_GEM_DOMAIN_VRAM))) {
				r = -EINVAL;
				break;
			}

			if (unlikely(amdgpu_ttm_tt_get_usermm(robj->tbo.ttm))) {
				r = -EPERM;
				break;
			}

			if (robj->tbo.base.dma_buf) {
				for (base = robj->vm_bo; base; base = base->next) {
					if (amdgpu_xgmi_same_hive(adev, amdgpu_ttm_adev(base->vm->root.bo->tbo.bdev))) {
						r = -EINVAL;
						goto set_placement_end;
					}
				}
			}

			robj->preferred_domains = new_domains;
			robj->allowed_domains = robj->preferred_domains;
			if (robj->allowed_domains == AMDGPU_GEM_DOMAIN_VRAM)
				robj->allowed_domains |= AMDGPU_GEM_DOMAIN_GTT;

			if (is_hbm2_vega(adev))
				amdgpu_vega_optimize_for_workload(adev, robj, robj->flags);

			if (robj->flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID)
				amdgpu_vm_bo_invalidate(robj, true);

			set_placement_end:
			break;
		}
		default:
			r = -EINVAL;
			break;
	}

	amdgpu_bo_unreserve(robj);
	out:
	drm_gem_object_put(gobj);
	return r;
}

int amdgpu_mode_dumb_create(struct drm_file *file_priv,
							struct drm_device *dev,
							struct drm_mode_create_dumb *args)
{
	struct amdgpu_device *adev;
	struct amdgpu_fpriv *fpriv;
	struct drm_gem_object *gobj;
	uint32_t handle;
	u64 flags = AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED |
	AMDGPU_GEM_CREATE_CPU_GTT_USWC |
	AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS;
	u32 domain;
	uint64_t size;
	int r;

	if (unlikely(!file_priv || !dev || !args))
		return -EINVAL;

	adev = drm_to_adev(dev);
	fpriv = file_priv->driver_priv;
	if (unlikely(!adev || !fpriv))
		return -EINVAL;

	if (unlikely(args->width == 0 || args->height == 0 || args->bpp == 0 ||
		args->bpp > 32 || args->width > U16_MAX || args->height > U16_MAX))
		return -EINVAL;

	/*
	 * Flag that we want the buffer to be cleared. The create ioctl will
	 * handle the most efficient clearing method.
	 */
	flags |= AMDGPU_GEM_CREATE_VRAM_CLEARED;

	r = amdgpu_gem_align_pitch(adev, args->width,
							   DIV_ROUND_UP(args->bpp, 8), 0);
	if (unlikely(r < 0))
		return r;
	args->pitch = r;

	if (unlikely(check_mul_overflow((u64)args->pitch, args->height, &size))) {
		DRM_ERROR("Dumb buffer size calculation overflow\n");
		return -EINVAL;
	}

	domain = amdgpu_bo_get_preferred_domain(adev,
											amdgpu_display_supported_domains(adev, flags));

	uint32_t alignment = PAGE_SIZE;
	if (domain == AMDGPU_GEM_DOMAIN_VRAM && is_hbm2_vega(adev))
		amdgpu_vega_optimize_hbm2_bank_access(adev, &size, &alignment, flags);

	if (unlikely(size > U64_MAX - (alignment - 1))) {
		DRM_ERROR("Dumb buffer alignment would overflow\n");
		return -EINVAL;
	}
	size = ALIGN(size, alignment);
	args->size = size;

	r = amdgpu_gem_object_create(adev, size, alignment, domain, flags,
								 ttm_bo_type_device, NULL, &gobj,
							  fpriv->xcp_id + 1);
	if (unlikely(r)) {
		DRM_DEBUG("Failed to create dumb buffer (%d)\n", r);
		return r;
	}

	r = drm_gem_handle_create(file_priv, gobj, &handle);
	drm_gem_object_put(gobj);
	if (unlikely(r))
		return r;

	args->handle = handle;
	return 0;
}

#if defined(CONFIG_DEBUG_FS)
static int amdgpu_debugfs_gem_info_show(struct seq_file *m, void *unused)
{
	struct amdgpu_device *adev;
	struct drm_device *dev;
	struct drm_file *file;
	int r;

	if (unlikely(!m || !m->private))
		return -EINVAL;

	adev = m->private;
	dev = adev_to_drm(adev);
	if (unlikely(!dev))
		return -EINVAL;

	r = mutex_lock_interruptible(&dev->filelist_mutex);
	if (r)
		return r;

	list_for_each_entry(file, &dev->filelist, lhead) {
		struct task_struct *task;
		struct drm_gem_object *gobj;
		struct pid *pid;
		int id;

		rcu_read_lock();
		pid = rcu_dereference(file->pid);
		task = pid_task(pid, PIDTYPE_TGID);
		seq_printf(m, "pid %8d command %s:\n", pid_nr(pid),
				   task ? task->comm : "<unknown>");
		rcu_read_unlock();

		spin_lock(&file->table_lock);
		idr_for_each_entry(&file->object_idr, gobj, id) {
			struct amdgpu_bo *bo = gem_to_amdgpu_bo(gobj);
			if (bo)
				amdgpu_bo_print_info(id, bo, m);
		}
		spin_unlock(&file->table_lock);
	}

	mutex_unlock(&dev->filelist_mutex);
	return 0;
}

DEFINE_SHOW_ATTRIBUTE(amdgpu_debugfs_gem_info);
#endif

void amdgpu_debugfs_gem_init(struct amdgpu_device *adev)
{
	#if defined(CONFIG_DEBUG_FS)
	struct drm_minor *minor;
	struct dentry *root;

	if (unlikely(!adev))
		return;

	minor = adev_to_drm(adev)->primary;
	if (unlikely(!minor))
		return;

	root = minor->debugfs_root;
	if (unlikely(!root))
		return;

	debugfs_create_file("amdgpu_gem_info", 0444, root, adev,
						&amdgpu_debugfs_gem_info_fops);
	#endif
}
