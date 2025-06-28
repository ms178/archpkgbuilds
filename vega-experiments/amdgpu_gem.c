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
#include <linux/seqlock.h>
#include <linux/math64.h>
#include <linux/overflow.h>
#include <linux/hash.h>
#include <linux/workqueue.h>
#include <linux/completion.h>
#include <linux/atomic.h>

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
	static atomic_t initialized = ATOMIC_INIT(0);
	unsigned long flags;
	u64 size;

	if (unlikely(!adev) || atomic_read(&initialized))
		return;

	if (atomic_inc_return(&initialized) > 1)
		return;

	size = adev->gmc.mc_vram_size;
	if (unlikely(!size || size < 100))
		return;

	spin_lock_irqsave(&vram_state.lock, flags);
	write_seqcount_begin(&vram_state.seq);
	vram_state.size_bytes = size;
	/* Calculate reciprocal for fast division: (1<<38) / (size/100) */
	vram_state.reciprocal_100 = div64_u64((1ULL << 38), div64_u64(size, 100));
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

#define TBO_CACHE_DEPTH 32
#define TBO_MAX_BYTES   (64u << 10)
#define TBO_CACHEABLE_VRAM_FLAGS (AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS | \
AMDGPU_GEM_CREATE_NO_CPU_ACCESS   | \
AMDGPU_GEM_CREATE_VRAM_CLEARED)

/* Lockless per-CPU cache with optimal alignment */
struct tiny_bo_cache {
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

static struct amdgpu_bo *
tbo_cache_try_get(unsigned long size, u64 user_flags,
				  u32 domain, struct dma_resv *resv, int align)
{
	struct amdgpu_bo       *bo = NULL;
	struct tiny_bo_cache   *c;
	int                     cpu;

	if (!static_branch_unlikely(&tbo_cache_key))
		return NULL;

	if (unlikely(size > TBO_MAX_BYTES || resv || align > PAGE_SIZE))
		return NULL;

	/* guarantee pointer stability */
	cpu = get_cpu();
	c   = per_cpu_ptr(&tiny_bo_cache, cpu);

	if (domain == AMDGPU_GEM_DOMAIN_GTT) {
		if ((user_flags & ~(AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED |
			AMDGPU_GEM_CREATE_CPU_GTT_USWC)) == 0 &&
			c->gtt_top > 0) {
			bo = c->gtt_slot[--c->gtt_top];
		c->gtt_slot[c->gtt_top] = NULL;
			}
	} else if (domain == AMDGPU_GEM_DOMAIN_VRAM) {
		if ((user_flags & ~TBO_CACHEABLE_VRAM_FLAGS) == 0 &&
			c->vram_top > 0) {
			bo = c->vram_slot[--c->vram_top];
		c->vram_slot[c->vram_top] = NULL;
			}
	}

	put_cpu();

	/* the object was swapped-out while cached – fall back to slow path */
	if (unlikely(!bo || bo->tbo.ttm))
		return NULL;

	prefetchw(bo->tbo.base.resv);
	prefetch(bo);
	return bo;
}

static bool tbo_cache_put(struct amdgpu_bo *bo)
{
	struct tiny_bo_cache *c;
	u64                   flags;
	int                   cpu;

	if (unlikely(!static_branch_unlikely(&tbo_cache_key) || !bo))
		return false;

	if (unlikely(bo->tbo.base.size > TBO_MAX_BYTES ||
		(bo->tbo.page_alignment << PAGE_SHIFT) > PAGE_SIZE ||
		bo->tbo.ttm))
		return false;

	flags = bo->flags & ~AMDGPU_GEM_CREATE_VRAM_WIPE_ON_RELEASE;

	cpu = get_cpu();
	c   = per_cpu_ptr(&tiny_bo_cache, cpu);

	switch (bo->preferred_domains) {
		case AMDGPU_GEM_DOMAIN_GTT:
			/* STRICT: No other flags than these are allowed. */
			if ((flags & ~(AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED |
				AMDGPU_GEM_CREATE_CPU_GTT_USWC)) == 0 &&
				c->gtt_top < ARRAY_SIZE(c->gtt_slot)) {
				c->gtt_slot[c->gtt_top++] = bo;
			put_cpu();
			return true;
				}
				break;
		case AMDGPU_GEM_DOMAIN_VRAM:
			/* STRICT: flags must exactly match cacheable flags. */
			if (flags == TBO_CACHEABLE_VRAM_FLAGS &&
				c->vram_top < ARRAY_SIZE(c->vram_slot)) {
				c->vram_slot[c->vram_top++] = bo;
			put_cpu();
			return true;
				}
				break;
		default:
			break;
	}

	put_cpu();
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
	amdgpu_vega_vram_pressure_mid  = clamp(amdgpu_vega_vram_pressure_mid,  50, 90);
	amdgpu_vega_vram_pressure_high = clamp(amdgpu_vega_vram_pressure_high, 70, 95);

	if (amdgpu_vega_vram_pressure_high <= amdgpu_vega_vram_pressure_mid)
		amdgpu_vega_vram_pressure_high = amdgpu_vega_vram_pressure_mid + 5;
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

static u32 __amdgpu_vega_get_vram_usage(struct amdgpu_device *adev)
{
	struct ttm_resource_manager *mgr;
	u64 used, size, recip;
	unsigned int seq;

	if (unlikely(!adev))
		return 0;

	mgr = ttm_manager_type(&adev->mman.bdev, TTM_PL_VRAM);
	if (unlikely(!mgr))
		return 0;

	used = ttm_resource_manager_usage(mgr);
	size = READ_ONCE(adev->gmc.mc_vram_size);
	if (unlikely(!size))
		return 0;

	static DEFINE_PER_CPU(u64, recip_q38);
	static DEFINE_PER_CPU(unsigned long, stamp);
	unsigned long *stamp_ptr = this_cpu_ptr(&stamp);
	unsigned long j = jiffies;

	/* Correctly initialize per-CPU timestamp on first use */
	if (unlikely(*stamp_ptr == 0))
		*stamp_ptr = j;

	recip = this_cpu_read(recip_q38);

	if (unlikely(time_after(j, *stamp_ptr + (HZ / 250)))) {
		do {
			seq = read_seqcount_begin(&vram_state.seq);
			recip = READ_ONCE(vram_state.reciprocal_100);
		} while (read_seqcount_retry(&vram_state.seq, seq));

		if (unlikely(!recip))
			return 0;

		this_cpu_write(recip_q38, recip);
		*stamp_ptr = j;
	}

	u32 pct = mul_u64_u64_shr(used, recip, 38);
	return min(pct, 100u);
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
#define VRAM_PCT_HYSTERESIS         2u           /* 2 percentage points */

static enum vega_vram_pressure_state
amdgpu_vega_get_vram_pressure_state(struct amdgpu_device *adev)
{
	struct vega_pressure_cache_pc *cache_pc;
	unsigned long  now_j;
	u32            pct_now, bias, yellow_th, red_th;
	enum vega_vram_pressure_state new_state;
	int            cpu;

	/* Invalid device?  Assume low pressure so we do not throttle. */
	if (unlikely(!adev))
		return VEGA_VRAM_GREEN;

	/* -------- 1. per-CPU fast-path -------- */
	now_j   = jiffies;
	cpu     = get_cpu();                             /* disable pre-emption   */
	cache_pc = per_cpu_ptr(&pressure_cache_pc, cpu);

	if (likely(time_before(now_j,
		cache_pc->jts_last_update + VRAM_CACHE_REFRESH_JIFFIES))) {
		new_state = cache_pc->state;
	put_cpu();
	return new_state;
		}

		/* -------- 2. Slow-path : recalc -------- */

		/* a) decay global eviction EWMA (~ every 250 ms system-wide, race-free) */
		static unsigned long last_decay_j;
		unsigned long last_j = READ_ONCE(last_decay_j);

		if (time_after(now_j, last_j + HZ / 4)) {
			if (cmpxchg(&last_decay_j, last_j, now_j) == last_j) {
				int old, new;

				do {
					old = atomic_read(&vram_state.eviction_rate_ewma);
					new = max(0, old - ((old + 15) >> 4)); /* 1/16 decay */
				} while (atomic_cmpxchg(&vram_state.eviction_rate_ewma,
										old, new) != old);
			}
		}

		/* b) fresh VRAM-usage percentage */
		pct_now = __amdgpu_vega_get_vram_usage(adev);

		/* c) dynamic bias from recent eviction activity (0-25 pp) */
		bias = min_t(u32,
					 atomic_read(&vram_state.eviction_rate_ewma) >> EW_UNIT_SHIFT,
					 25u);

		/* thresholds after subtracting bias */
		yellow_th = max_t(u32, amdgpu_vega_vram_pressure_mid  - bias, 50u);
		red_th    = max_t(u32, amdgpu_vega_vram_pressure_high - bias, 75u);

		/* -------- 3. Finite-state machine with hysteresis -------- */
		if (pct_now > red_th + VRAM_PCT_HYSTERESIS) {
			new_state = VEGA_VRAM_RED;
		} else if (pct_now > yellow_th + VRAM_PCT_HYSTERESIS) {
			new_state = VEGA_VRAM_YELLOW;
		} else if (pct_now < yellow_th - VRAM_PCT_HYSTERESIS) {
			new_state = VEGA_VRAM_GREEN;
		} else {
			/* remain in previous state to avoid oscillation */
			new_state = cache_pc->state;
		}

		/* -------- 4. Publish to per-CPU cache -------- */
		cache_pc->state           = new_state;
		cache_pc->pct_last        = pct_now;
		cache_pc->jts_last_update = now_j;

		put_cpu();   /* re-enable pre-emption */
		return new_state;
}

static inline u32 pb_hash(u32 tgid)
{
	return hash_32(tgid, ilog2(PB_ENTRIES));
}

static inline void pb_decay(struct pid_bias_entry *e, u64 now_ns)
{
	u64 delta_ns;
	u32 shifts;

	if (unlikely(!e || !e->ewma))
		return;

	delta_ns = now_ns - e->ns_last;
	if (unlikely(delta_ns == 0 || PB_DECAY_FACTOR_NS == 0))
		return;

	shifts = div64_u64(delta_ns, PB_DECAY_FACTOR_NS);
	if (shifts) {
		if (shifts >= 16)
			e->ewma = 0;
		else
			e->ewma >>= min(shifts, 8u);
		e->ns_last = now_ns;
	}
}

static inline u32 pb_get_bias(void)
{
	const u32 tgid = current->tgid;
	struct pid_bias_entry *tbl;
	u32 h, i, ret = 0;
	u64 now;
	int cpu;

	if (unlikely(!tgid))
		return 0;

	cpu = get_cpu();
	tbl = per_cpu_ptr(pid_bias_tbl, cpu);
	h   = pb_hash(tgid);
	now = ktime_get_coarse_ns();

	for (i = 0; i < PB_ENTRIES; ++i, h = (h + 1) & PB_HASH_MASK) {
		struct pid_bias_entry *e = &tbl[h];

		if (e->tgid == tgid) {
			if (now - e->ns_last > NSEC_PER_SEC)
				e->ewma = 0;

			pb_decay(e, now);
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
	u32 h, i, victim = PB_ENTRIES;
	u64 now_ns;
	u16 victim_ew = U16_MAX;
	int cpu;

	if (unlikely(!tgid))
		return;

	atomic_add_unless(&vram_state.eviction_rate_ewma,
					  EW_INC_PER_FAULT, INT_MAX);

	cpu = get_cpu();
	tbl = per_cpu_ptr(pid_bias_tbl, cpu);
	h   = pb_hash(tgid);
	now_ns = ktime_get_coarse_ns();

	for (i = 0; i < PB_ENTRIES; ++i, h = (h + 1) & PB_HASH_MASK) {
		struct pid_bias_entry *e = &tbl[h];

		if (e->tgid == tgid) {
			victim = h;
			break;
		}
		pb_decay(e, now_ns);
		if (!e->tgid) {
			victim = h;
			break;
		}
		if (e->ewma < victim_ew) {
			victim_ew = e->ewma;
			victim    = h;
		}
	}

	if (victim < PB_ENTRIES) {
		struct pid_bias_entry *e = &tbl[victim];

		if (e->tgid != tgid && e->tgid != 0)
			pb_decay(e, now_ns);

		e->tgid    = tgid;
		e->ewma    = min_t(u16, e->ewma + EW_INC_PER_FAULT, MAX_EWMA);
		e->ns_last = now_ns;
	}

	put_cpu();
}

static bool
amdgpu_vega_optimize_hbm2_bank_access(struct amdgpu_device *adev,
									  u64 *size_in_out,
									  u32 *align_in_out)
{
	u32 want;
	u64 sz;

	if (unlikely(!adev || !size_in_out || !align_in_out))
		return false;

	if (!is_hbm2_vega(adev) ||
		!static_branch_unlikely(&vega_bankalign_key) ||
		!*size_in_out)
		return false;

	if (amdgpu_vega_get_vram_pressure_state(adev) != VEGA_VRAM_GREEN)
		return false;

	want = *align_in_out;
	sz = *size_in_out;

	if (sz >= (128ULL << 20))
		want = max(want, 2u << 20);
	else if (sz >= (8ULL << 20))
		want = max(want, 512u << 10);
	else
		return false;

	if (!is_power_of_2(want) || want == *align_in_out)
		return false;

	/* Add overflow protection before calling ALIGN */
	if (unlikely(sz > U64_MAX - (want - 1)))
		return false;

	*align_in_out = want;
	*size_in_out = ALIGN(sz, want);
	return true;
}

static unsigned int
amdgpu_vega_determine_optimal_prefetch(struct amdgpu_device *adev,
									   struct amdgpu_bo     *bo,
									   unsigned int          base_pages)
{
	unsigned int pages_total, want;
	enum vega_vram_pressure_state st;

	/* Vega 64 specific tuning: Be more conservative with HBM2 */
	#define VEGA64_PREFETCH_MAX  16U
	#define VEGA64_PREFETCH_MIN   1U

	if (unlikely(!is_hbm2_vega(adev) || !bo || !base_pages))
		return base_pages;

	pages_total = DIV_ROUND_UP(amdgpu_bo_size(bo), PAGE_SIZE);
	if (!pages_total)
		return base_pages;

	/* ------------------------------------------------------------------
	 * 1.  Base scaling based on VRAM pressure
	 * ------------------------------------------------------------------ */
	st = amdgpu_vega_get_vram_pressure_state(adev);

	/*
	 * More conservative scaling for Vega 64.
	 * GREEN  : 100 % of base_pages
	 * YELLOW :  40 %
	 * RED    :  20 %
	 */
	static const u8 scale_pct[3] = { 100, 40, 20 };
	want = (base_pages * scale_pct[st]) / 100;

	/* ------------------------------------------------------------------
	 * 2.  Domain-dependent fine-tuning
	 * ------------------------------------------------------------------ */
	if (bo->preferred_domains & AMDGPU_GEM_DOMAIN_GTT)
		want = max(want >> 1, VEGA64_PREFETCH_MIN);

	/* Compute-only VRAM buffers can tolerate a bit more */
	if ((bo->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS) &&
		pages_total > 256 && st == VEGA_VRAM_GREEN)
		want = min(want * 2, VEGA64_PREFETCH_MAX);

	/* ------------------------------------------------------------------
	 * 3.  Clamp to sane, Vega 64-tuned limits
	 * ------------------------------------------------------------------ */
	want = clamp(want, VEGA64_PREFETCH_MIN,
				 min(pages_total, VEGA64_PREFETCH_MAX));

	return want;
}

static bool
amdgpu_vega_optimize_buffer_placement(struct amdgpu_device *adev,
									  struct amdgpu_bo *bo, /* nullable */
									  u64 size, u64 flags, u32 *domain)
{
	const bool cpu_acc = is_vega_cpu_access(flags);

	if (unlikely(!is_hbm2_vega(adev) || !domain))
		return false;

	if (size <= (64 << 10) && cpu_acc) {
		u32 cur_dom = *domain & (AMDGPU_GEM_DOMAIN_VRAM | AMDGPU_GEM_DOMAIN_GTT);

		if (cur_dom == AMDGPU_GEM_DOMAIN_GTT) {
			return false;
		} else {
			*domain = (*domain & ~AMDGPU_GEM_DOMAIN_VRAM) | AMDGPU_GEM_DOMAIN_GTT;
			return true;
		}
	}

	const bool is_tex = is_vega_texture(flags);
	const bool is_compute = is_vega_compute(flags);
	int score = 0;
	u32 cur_dom, want_dom;
	static DEFINE_PER_CPU(int, last_score);
	int *lastp = this_cpu_ptr(&last_score);
	enum vega_vram_pressure_state pst;
	static const int press_tbl[3] = { 0, 8, 24 };

	if (size > (32ULL << 20)) { score -= 16; }
	else if (size > (4ULL << 20)) { score -= 8; }
	else if (size > (1ULL << 20)) { score += 4; }
	else { score += 12; }

	if (is_tex) { score += 25; }
	if (is_compute) { score += 15; }
	if (cpu_acc) { score -= 35; }

	pst = amdgpu_vega_get_vram_pressure_state(adev);
	score -= press_tbl[pst];

	score -= (int)pb_get_bias() * 2;
	if (current->policy == SCHED_FIFO || current->policy == SCHED_RR)
		score += 8;

	if (abs(score - *lastp) < 10)
		score = *lastp;
	*lastp = score;

	cur_dom  = *domain & (AMDGPU_GEM_DOMAIN_VRAM | AMDGPU_GEM_DOMAIN_GTT);
	want_dom = (score >= 0) ? AMDGPU_GEM_DOMAIN_VRAM : AMDGPU_GEM_DOMAIN_GTT;

	if (cur_dom == want_dom) {
		return false;
	} else {
		*domain &= ~(AMDGPU_GEM_DOMAIN_VRAM | AMDGPU_GEM_DOMAIN_GTT);
		*domain |=  want_dom;

		if (want_dom == AMDGPU_GEM_DOMAIN_GTT && size > (256ULL << 10))
			pb_account_eviction();
		return true;
	}
}

static bool amdgpu_vega_optimize_for_workload(struct amdgpu_device *adev,
											  struct amdgpu_bo *bo,
											  uint64_t flags)
{
	uint64_t size;

	if (unlikely(!is_hbm2_vega(adev) || !bo || !bo->tbo.base.dev))
		return false;

	size = amdgpu_bo_size(bo);
	if (unlikely(!size))
		return false;

	if (unlikely(!dma_resv_is_locked(bo->tbo.base.resv)))
		return false;

	if (is_vega_texture(flags) && size >= AMDGPU_VEGA_MEDIUM_BUFFER_SIZE) {
		bo->preferred_domains = AMDGPU_GEM_DOMAIN_VRAM;
		bo->allowed_domains = AMDGPU_GEM_DOMAIN_VRAM | AMDGPU_GEM_DOMAIN_GTT;
		return true;
	}

	if (is_vega_compute(flags) && !is_vega_cpu_access(flags)) {
		bo->preferred_domains = AMDGPU_GEM_DOMAIN_VRAM;
		bo->allowed_domains = AMDGPU_GEM_DOMAIN_VRAM | AMDGPU_GEM_DOMAIN_GTT;
		return true;
	}

	if (is_vega_cpu_access(flags) && size <= AMDGPU_VEGA_SMALL_BUFFER_SIZE) {
		bo->preferred_domains = AMDGPU_GEM_DOMAIN_GTT;
		bo->allowed_domains = AMDGPU_GEM_DOMAIN_GTT | AMDGPU_GEM_DOMAIN_VRAM;
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

	if (unlikely(width > INT_MAX - mask))
		return -EINVAL;

	aligned_width = (width + mask) & ~mask;

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
	ret = ttm_bo_vm_reserve(bo, vmf);
	if (unlikely(ret))
		return ret;

	/* -------- 2. Enter DRM device  -------------------------------------- */
	if (!drm_dev_enter(ddev, &idx)) {
		ret = ttm_bo_vm_dummy_page(vmf, vmf->vma->vm_page_prot);
		goto out_unlock;
	}

	/* -------------------------------------------------------------------- */
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

int amdgpu_gem_object_create(struct amdgpu_device *adev,
							 unsigned long size,
							 int alignment,
							 u32 initial_domain,
							 u64 flags,
							 enum ttm_bo_type type,
							 struct dma_resv *resv,
							 struct drm_gem_object **obj,
							 int8_t xcp_id_plus1)
{
	struct amdgpu_bo_user *ubo = NULL;
	struct amdgpu_bo *bo;
	struct amdgpu_bo_param bp;
	int r;

	if (unlikely(!adev || !obj))
		return -EINVAL;

	*obj = NULL;

	if (unlikely(!size || size > adev->gmc.mc_vram_size + adev->gmc.gart_size))
		return -EINVAL;

	amdgpu_tbo_slab_ensure();

	size = ALIGN(size, PAGE_SIZE);

	if (flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID)
		amdgpu_vm_always_valid_key_enable();

	bo = tbo_cache_try_get(size, flags, initial_domain, resv, alignment);
	if (bo) {
		*obj = &bo->tbo.base;
		return 0;
	}

	memset(&bp, 0, sizeof(bp));
	bp.size = size;
	bp.byte_align = alignment;
	bp.type = type;
	bp.resv = resv;
	bp.preferred_domain = initial_domain;
	bp.domain = initial_domain;
	bp.flags = flags;
	bp.bo_ptr_size = sizeof(struct amdgpu_bo);
	bp.xcp_id_plus1 = xcp_id_plus1;

	if (static_branch_unlikely(&vega_domain_key))
		amdgpu_vega_optimize_buffer_placement(adev, NULL, size, flags, &bp.domain);

	if (ubo_slab)
		ubo = kmem_cache_zalloc(ubo_slab, GFP_KERNEL | __GFP_NOWARN);

	if (unlikely(!ubo)) {
		/* Fallback to regular allocation if slab allocation fails */
		r = amdgpu_bo_create(adev, &bp, &bo);
		if (unlikely(r))
			return r;
	} else {
		/* Proceed with the user/slab allocation path */
		r = amdgpu_bo_create_user(adev, &bp, &ubo);
		if (unlikely(r)) {
			kmem_cache_free(ubo_slab, ubo);
			return r;
		}
		bo = &ubo->bo;
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
		(!obj->import_attach || !dma_buf_is_dynamic(obj->import_attach->dmabuf)))
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

	flags |= AMDGPU_GEM_CREATE_VRAM_CLEARED;
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
			if (r == -ENOMEM && (alloc_domain & AMDGPU_GEM_DOMAIN_VRAM)) {
				/* Failed in VRAM due to no space. Try GTT-only next. */
				alloc_domain = AMDGPU_GEM_DOMAIN_GTT;
				continue;
			} else {
				/* Any other error is unrecoverable, exit the loop. */
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
				map_flags = amdgpu_gem_va_map_flags(adev, args->flags);
				r = amdgpu_vm_bo_map(adev, bo_va, args->va_address,
									 args->offset_in_bo, args->map_size, map_flags);
				break;
			case AMDGPU_VA_OP_UNMAP:
				r = amdgpu_vm_bo_unmap(adev, bo_va, args->va_address);
				break;
			case AMDGPU_VA_OP_CLEAR:
				r = amdgpu_vm_bo_clear_mappings(adev, vm, args->va_address, args->map_size);
				break;
			case AMDGPU_VA_OP_REPLACE:
				map_flags = amdgpu_gem_va_map_flags(adev, args->flags);
				r = amdgpu_vm_bo_replace_map(adev, bo_va, args->va_address,
											 args->offset_in_bo, args->map_size, map_flags);
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

	if (adev->mman.buffer_funcs_enabled)
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
		amdgpu_vega_optimize_hbm2_bank_access(adev, &size, &alignment);

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
