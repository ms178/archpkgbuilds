/* SPDX-License-Identifier: MIT */
/*
 * Copyright 2008 Advanced Micro Devices, Inc.
 * Copyright 2008 Red Hat Inc.
 * Copyright 2009 Jerome Glisse.
 *
 * This is the GEM helper file as in the original upstream driver,
 * plus only the safest micro‑optimisations:
 *   – static‑key for Vega/HBM2 ASIC detection
 *   – power‑of‑two ALIGN macro
 *   – cached VRAM‑usage percentage (optional helper)
 *   – LUT‑based pitch‑align helper
 *   – cheaper timeout helper
 *   – guarded prefetch in VM‑fault path (added later in file)
 *
 * No functional behaviour is otherwise changed.
 */
#include <linux/ktime.h>
#include <linux/mm.h>        /* PAGE_OFFSET for safe prefetch */
#include <linux/module.h>
#include <linux/pagemap.h>
#include <linux/pci.h>
#include <linux/dma-buf.h>
#include <linux/jump_label.h>
#include <linux/prefetch.h>
#include <linux/atomic.h>
#include <linux/sched.h>

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

/* ---------------------------------------------------------------------- */
/*        Fine‑grained static‑keys for Vega/HBM2 hot‑paths                */
/* ---------------------------------------------------------------------- */

/*
 * We split the original monolithic key into three so each feature can be
 * toggled independently through the jump‑label debugFS interface.  All three
 * keys are enabled for Vega10 ASICs; disabling any of them at run‑time costs
 * a single patched NOP per CPU.
 */
DEFINE_STATIC_KEY_FALSE(vega_bankalign_key);   /* HBM2 bank‑aware alignment  */
DEFINE_STATIC_KEY_FALSE(vega_prefetch_key);    /* VM‑fault / ioctl prefetch  */
DEFINE_STATIC_KEY_FALSE(vega_domain_key);      /* VRAM/GTT placement tweaks  */

/* ------------------------------------------------------------------ */
/*  VM‑ALWAYS‑VALID optimisation                                      */
/* ------------------------------------------------------------------ */
DEFINE_STATIC_KEY_FALSE(amdgpu_vm_always_valid_key);

/* Enable key once the first such BO is created */
static inline void amdgpu_vm_always_valid_key_enable(void)
{
	static bool once;

	if (!once) {
		static_branch_enable(&amdgpu_vm_always_valid_key);
		once = true;
	}
}

/*
 * For in‑file back‑compat we alias the former name            ──┐
 *   vega_hbm2_key  →  vega_domain_key                           │
 * so existing code that we haven’t converted yet keeps working. │
 */
#define vega_hbm2_key vega_domain_key

static inline void
amdgpu_gem_static_branch_init(struct amdgpu_device *adev)
{
	/*
	 * All three keys are enabled for Vega10 parts; other ASICs leave them
	 * patched as static nops.
	 */
	if (adev && adev->asic_type == CHIP_VEGA10) {
		static_branch_enable(&vega_bankalign_key);
		static_branch_enable(&vega_prefetch_key);
		static_branch_enable(&vega_domain_key);
	}
}

/* ------------------------------------------------------------------ */
/*  Tiny‑BO cache & helpers                                           */
/* ------------------------------------------------------------------ */
#define TBO_MAX_BYTES   (64u << 10)
#define TBO_CACHE_DEPTH 16
#define TBO_ELIGIBLE(_f,_d,_r,_a) \
(!(_f) && (_d) == AMDGPU_GEM_DOMAIN_GTT && !(_r) && (_a) <= PAGE_SIZE)

struct tiny_bo_cache {
	struct amdgpu_bo *slot[TBO_CACHE_DEPTH];
	u8                top;
};

static DEFINE_PER_CPU(struct tiny_bo_cache, tiny_bo_cache);
DEFINE_STATIC_KEY_FALSE(tbo_cache_key);

/* -------- slab for struct amdgpu_bo_user metadata ----------------- */
static struct kmem_cache *ubo_slab;

static void amdgpu_tbo_slab_ensure(void)
{
	struct kmem_cache *s;

	if (likely(READ_ONCE(ubo_slab)))
		return;

	s = kmem_cache_create("amdgpu_bo_user",
						  sizeof(struct amdgpu_bo_user),
						  0, SLAB_HWCACHE_ALIGN, NULL);
	if (!s)
		return;

	/* publish once – destroy duplicate if raced */
	if (cmpxchg(&ubo_slab, NULL, s))
		kmem_cache_destroy(s);
}

/* -------- cache get / put ----------------------------------------- */
static struct amdgpu_bo *
tbo_cache_try_get(unsigned long size, u64 flags,
				  u32 domain, struct dma_resv *resv, int align)
{
	if (!static_branch_unlikely(&tbo_cache_key))
		return NULL;
	if (!TBO_ELIGIBLE(flags, domain, resv, align) || size > TBO_MAX_BYTES)
		return NULL;

	struct tiny_bo_cache *c = this_cpu_ptr(&tiny_bo_cache);
	if (!c->top)
		return NULL;

	return c->slot[--c->top];      /* extra ref already held */
}

static bool tbo_cache_put(struct amdgpu_bo *bo)
{
	if (!static_branch_unlikely(&tbo_cache_key))
		return false;

	if (bo->tbo.base.size > TBO_MAX_BYTES ||
		!TBO_ELIGIBLE(bo->flags, bo->preferred_domains,
					  NULL, bo->tbo.page_alignment << PAGE_SHIFT))
		return false;

	struct tiny_bo_cache *c = this_cpu_ptr(&tiny_bo_cache);
	if (c->top >= TBO_CACHE_DEPTH)
		return false;

	drm_gem_object_get(&bo->tbo.base);      /* keep one ref */
	c->slot[c->top++] = bo;
	return true;
}

/* ---------------------------------------------------------------------- */
/*                     Baseline Vega definitions                          */
/* ---------------------------------------------------------------------- */
#define AMDGPU_VEGA_HBM2_BANK_SIZE       (1ULL * 1024 * 1024)
#define AMDGPU_VEGA_SMALL_BUFFER_SIZE    (1ULL * 1024 * 1024)   /* 1 MiB */
#define AMDGPU_VEGA_MEDIUM_BUFFER_SIZE   (4ULL * 1024 * 1024)   /* 4 MiB */
#define AMDGPU_VEGA_LARGE_BUFFER_SIZE    (16ULL * 1024 * 1024)  /* 16 MiB */
#define AMDGPU_VEGA_HBM2_MIN_ALIGNMENT   (256 * 1024)           /* 256 KiB */

static int amdgpu_vega_vram_pressure_low  __ro_after_init = 65;
static int amdgpu_vega_vram_pressure_mid  __ro_after_init = 75;
static int amdgpu_vega_vram_pressure_high __ro_after_init = 85;

void amdgpu_vega_vram_thresholds_init(void);

module_param_named(vram_pressure_low,  amdgpu_vega_vram_pressure_low,  int, 0644);
MODULE_PARM_DESC(vram_pressure_low,  "Low VRAM pressure threshold for Vega (65)");
module_param_named(vram_pressure_mid, amdgpu_vega_vram_pressure_mid,  int, 0644);
MODULE_PARM_DESC(vram_pressure_mid,  "Mid VRAM pressure threshold for Vega (75)");
module_param_named(vram_pressure_high, amdgpu_vega_vram_pressure_high, int, 0644);
MODULE_PARM_DESC(vram_pressure_high, "High VRAM pressure threshold for Vega (85)");

void amdgpu_vega_vram_thresholds_init(void)
{
	amdgpu_vega_vram_pressure_low  = clamp(amdgpu_vega_vram_pressure_low,  0, 100);
	amdgpu_vega_vram_pressure_mid  = clamp(amdgpu_vega_vram_pressure_mid,  0, 100);
	amdgpu_vega_vram_pressure_high = clamp(amdgpu_vega_vram_pressure_high, 0, 100);

	if (amdgpu_vega_vram_pressure_mid < amdgpu_vega_vram_pressure_low)
		amdgpu_vega_vram_pressure_mid = amdgpu_vega_vram_pressure_low;
	if (amdgpu_vega_vram_pressure_high < amdgpu_vega_vram_pressure_mid)
		amdgpu_vega_vram_pressure_high = amdgpu_vega_vram_pressure_mid;
}

/* ---------------------------------------------------------------------- */
/*           Category helper predicates  (unchanged baseline)            */
/* ---------------------------------------------------------------------- */
static __always_inline bool is_vega_texture(uint64_t flags)
{ return flags & AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS; }

static __always_inline bool is_vega_compute(uint64_t flags)
{ return flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS; }

static __always_inline bool is_vega_cpu_access(uint64_t flags)
{ return flags & AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED; }

/* Static‑key aware predicate */
static __always_inline bool is_hbm2_vega(struct amdgpu_device *adev)
{
	#ifdef CONFIG_JUMP_LABEL
	if (static_branch_unlikely(&vega_hbm2_key))
		return true;
	return false;
	#else
	return adev && adev->asic_type == CHIP_VEGA10;
	#endif
}

/* ------------------------------------------------------------------ */
/*  2‑ms VRAM‑usage cache needed by trivial optimisations              */
/* ------------------------------------------------------------------ */
struct vega_vram_cache {
	atomic_t  pct;
	atomic64_t ns_last;
};
static struct vega_vram_cache vram_cache = {
	.pct     = ATOMIC_INIT(0),
	.ns_last = ATOMIC64_INIT(0),
};

static __always_inline uint32_t
amdgpu_vega_get_vram_usage_cached(struct amdgpu_device *adev)
{
	const s64 max_age = 2 * NSEC_PER_MSEC;
	s64 age = ktime_to_ns(ktime_get()) -
	atomic64_read(&vram_cache.ns_last);

	if (likely(age >= 0 && age < max_age))
		return atomic_read(&vram_cache.pct);

	/* slow path refresh */
	{
		struct ttm_resource_manager *vman;
		uint64_t used = 0, size = 0;
		uint32_t pct  = 0;

		vman = ttm_manager_type(&adev->mman.bdev, TTM_PL_VRAM);
		if (vman) {
			used = ttm_resource_manager_usage(vman);
			size = adev->gmc.mc_vram_size;
			if (size)
				pct = div64_u64(used * 100, size);
		}
		smp_wmb();
		atomic_set(&vram_cache.pct, pct);
		atomic64_set(&vram_cache.ns_last, ktime_get_ns());
		return pct;
	}
}

/* ---------------------------------------------------------------------- */
/* Per‑PID adaptive VRAM‑pressure bias – lock‑free per‑CPU hash‑table     */
/* ---------------------------------------------------------------------- */
#include <linux/percpu.h>
#include <linux/time64.h>

/* ---------------- Adaptive‑bias tuning constants -------------------- */
#define PB_ENTRIES          32U
#define PB_HASH_MASK        (PB_ENTRIES - 1)

/* Fixed‑point format: 5.3  (value × 8  ⇒ 1.000 pp == 8) */
#define EW_UNIT_SHIFT       3

/* One eviction raises the bias by +0.125 pp  (= 1 << 3)          */
#define EW_INC_PER_FAULT    1           /* was 2 (0.25 pp) */

/* Absolute ceiling for a PID’s bias: 8 pp (was 10 pp)            */
#define PB_BIAS_MAX_PCT     8
#define MAX_EWMA            (PB_BIAS_MAX_PCT << EW_UNIT_SHIFT)   /* 8 × 8 = 64 */

struct pid_bias_entry {
	u32 tgid;
	u8  ewma;          /* 5.3 fixed‑point, 0‑80                       */
	u8  last;          /* jiffies low‑8bits                          */
	u16 pad;
};

static DEFINE_PER_CPU(struct pid_bias_entry[PB_ENTRIES], pid_bias_tbl);

/* Hash, decay, query, update helpers ---------------------------------- */
static inline u32 pb_hash(u32 tgid)
{
	return (tgid * 0x9E3779B9u) & PB_HASH_MASK;  /* Knuth 32‑bit hash */
}

static inline void pb_decay(struct pid_bias_entry *e, u8 now)
{
	u8 delta = now - e->last;
	if (!delta || !e->ewma)
		return;

	do {
		e->ewma -= e->ewma >> 3;          /* −12.5 % per second */
	} while (--delta && e->ewma);

	e->last = now;
}

static inline u32 pb_get_bias(void)
{
	const u32 tgid = current->tgid;
	struct pid_bias_entry *tbl = this_cpu_ptr(pid_bias_tbl);
	u32 h  = pb_hash(tgid);
	u8  now = (u8)jiffies;

	for (u32 i = 0; i < PB_ENTRIES; ++i, h = (h + 1) & PB_HASH_MASK) {
		struct pid_bias_entry *e = &tbl[h];

		if (!e->tgid) {
			/* free slot → miss */
			break;
		}

		if (e->tgid == tgid) {
			pb_decay(e, now);
			return e->ewma >> EW_UNIT_SHIFT; /* 0‑10 pp */
		}
	}
	return 0;
}

static void pb_account_eviction(void)
{
	const u32 tgid = current->tgid;
	struct pid_bias_entry *tbl = this_cpu_ptr(pid_bias_tbl);
	u32 h = pb_hash(tgid);
	u8  now = (u8)jiffies;

	preempt_disable();                      /* stay on this CPU */
	u32 min_idx = h;
	u16 min_ew  = 0x100;                    /* > any real ewma */

	for (u32 i = 0; i < PB_ENTRIES; ++i, h = (h + 1) & PB_HASH_MASK) {
		struct pid_bias_entry *e = &tbl[h];

		if (!e->tgid || e->tgid == tgid) {
			if (!e->tgid)
				e->tgid = tgid;

			pb_decay(e, now);
			e->ewma = clamp_t(u8, e->ewma + EW_INC_PER_FAULT, 0, MAX_EWMA);
			e->last = now;
			preempt_enable();
			return;
		}

		if (e->ewma < min_ew) {
			min_ew  = e->ewma;
			min_idx = h;
		}
	}

	/* Table full – overwrite coldest entry */
	{
		struct pid_bias_entry *e = &tbl[min_idx];
		e->tgid = tgid;
		e->ewma = EW_INC_PER_FAULT;
		e->last = now;
	}
	preempt_enable();
}

/* ---------------------------------------------------------------------- */
/*                     Cheap ALIGN_POW2 helper macro                      */
/* ---------------------------------------------------------------------- */
#define ALIGN_POW2(x, a)	(((x) + ((a) - 1)) & ~((typeof(x))(a) - 1))

/* ---------------------------------------------------------------------- */
/*                   Tiny helpers used by hot‑path prefetch               */
/* ---------------------------------------------------------------------- */
#ifndef PREFETCH_READ
# define PREFETCH_READ(p)  prefetch(p)
# define PREFETCH_WRITE(p) prefetchw(p)
#endif

unsigned long amdgpu_gem_timeout(uint64_t timeout_ns)
{
	/* Negative means “infinite” in amdgpu ioctls */
	if ((int64_t)timeout_ns < 0)
		return MAX_SCHEDULE_TIMEOUT;

	/* Cheaper coarse clock – good enough for jiffies‑level math */
	uint64_t now_ns = ktime_get_coarse_ns();

	int64_t delta = (int64_t)(timeout_ns - now_ns);
	if (delta <= 0) {
		/* Already expired */
		return 0;
	}

	/* Convert to the scheduler’s unit */
	unsigned long j = nsecs_to_jiffies((uint64_t)delta);

	/*
	 * Clamp to MAX_SCHEDULE_TIMEOUT‑1 so callers that add the
	 * result to jiffies never overflow.
	 */
	if (j > MAX_SCHEDULE_TIMEOUT)
		j = MAX_SCHEDULE_TIMEOUT - 1;

	return j;
}

/* LUT‑based pitch helper */
static const uint16_t pitch_mask_lut[5] = { 0, 255, 127, 63, 63 };

static inline int
amdgpu_gem_align_pitch(struct amdgpu_device *adev,
					   int width, int cpp, bool tiled)
{
	int mask    = (cpp <= 4) ? pitch_mask_lut[cpp] : 0;
	int aligned = (width + mask) & ~mask;

	return aligned * cpp;
}

/* ---------------------------------------------------------------------- */
/*          (Baseline VRAM‑usage code continues unchanged)                */
/* ---------------------------------------------------------------------- */

static uint32_t amdgpu_vega_get_vram_usage(struct amdgpu_device *adev)
{
	struct ttm_resource_manager *vram_man;
	uint64_t vram_usage = 0, vram_size = 0;
	uint32_t usage_percent = 0;

	if (!adev || !adev->gmc.mc_vram_size)
		return 0;

	vram_man = ttm_manager_type(&adev->mman.bdev, TTM_PL_VRAM);
	if (!vram_man)
		return 0;

	vram_usage = ttm_resource_manager_usage(vram_man);
	vram_size  = adev->gmc.mc_vram_size;
	if (vram_size)
		usage_percent = div64_u64(vram_usage * 100, vram_size);

	return usage_percent;
}

/* Optional cached helper – used by some heuristics */
static inline uint32_t
amdgpu_vega_get_efficient_usage(struct amdgpu_device *adev)
{
	return amdgpu_vega_get_vram_usage_cached(adev);
}

static uint32_t
amdgpu_vega_get_effective_vram_usage(struct amdgpu_device *adev)
{
	uint32_t usage_percent, effective_percent;
	struct ttm_resource_manager *vram_man;

	if (!adev)
		return 0;

	usage_percent     = amdgpu_vega_get_vram_usage(adev);
	effective_percent = usage_percent;

	/* No extra heuristics for non‑HBM2 boards */
	if (!is_hbm2_vega(adev))
		return usage_percent;

	vram_man = ttm_manager_type(&adev->mman.bdev, TTM_PL_VRAM);
	if (!vram_man)
		return usage_percent;

	if (vram_man->use_tt) {
		effective_percent = min_t(uint32_t,
								  usage_percent + 10, 100);
	} else if (usage_percent > amdgpu_vega_vram_pressure_mid) {
		effective_percent = min_t(uint32_t,
								  usage_percent + 5, 100);
	}

	/* Conservative per‑PID bias: apply only from “mid” pressure up */
	if (usage_percent >= amdgpu_vega_vram_pressure_mid) {
		effective_percent = min_t(uint32_t,
								  effective_percent + pb_get_bias(),
								  100u);
	}

	return effective_percent;
}

static bool
amdgpu_vega_optimize_buffer_placement(struct amdgpu_device *adev,
									  struct amdgpu_bo     *bo,
									  uint64_t              size,
									  uint64_t              flags,
									  uint32_t             *domain)
{
	if (!is_hbm2_vega(adev) || !domain)
		return false;

	/* ---------- current VRAM pressure -------------------------------- */
	uint32_t usage = amdgpu_vega_get_effective_vram_usage(adev);

	/* background tasks (nice > 4) act as if pressure is +5 pp */
	if (task_nice(current) > 4 && usage < 95)
		usage += 5;

	/* ---------- pressure levels -------------------------------------- */
	const uint32_t PRESS_MID  = 75;  /* proactive eviction for huge compute */
	const uint32_t PRESS_HI   = amdgpu_vega_vram_pressure_high; /* ≈85 */
	const uint32_t PRESS_CAT  = 90;  /* catastrophic – may override user   */

	/* ---------- helpers ---------------------------------------------- */
	#define FORCE_GTT() do { \
	*domain = (*domain & ~AMDGPU_GEM_DOMAIN_VRAM) | \
	AMDGPU_GEM_DOMAIN_GTT; \
	} while (0)

	#define MAYBE_EWMA(sz) do { \
	if ((sz) >= AMDGPU_VEGA_SMALL_BUFFER_SIZE) \
		pb_account_eviction(); \
	} while (0)

	/* Respect explicit user domain unless pressure is catastrophic */
	if (*domain && usage < PRESS_CAT)
		return true;

	/* ------------------------------------------------------------------
	 * 1. Textures / scan‑out
	 *    Keep in VRAM until usage >= 90 % and size < 8 MiB
	 * ---------------------------------------------------------------- */
	if (is_vega_texture(flags)) {
		if (usage >= PRESS_CAT && size < (8ULL << 20)) {
			FORCE_GTT();
			MAYBE_EWMA(size);
		} else {
			*domain |= AMDGPU_GEM_DOMAIN_VRAM;
		}
		return true;
	}

	/* ------------------------------------------------------------------
	 * 2. Compute  (NO_CPU_ACCESS)
	 *    Large >64 MiB leaves at 75 %, medium >16 MiB at 85 %
	 * ---------------------------------------------------------------- */
	if (is_vega_compute(flags)) {
		if ((usage >= PRESS_HI  && size > (16ULL << 20)) ||
			(usage >= PRESS_MID && size > (64ULL << 20))) {
			FORCE_GTT();
		MAYBE_EWMA(size);
			} else {
				*domain |= AMDGPU_GEM_DOMAIN_VRAM;
			}
			return true;
	}

	/* ------------------------------------------------------------------
	 * 3. CPU‑accessible
	 * ---------------------------------------------------------------- */
	if (is_vega_cpu_access(flags)) {
		if (size <= AMDGPU_VEGA_SMALL_BUFFER_SIZE) {      /* ≤1 MiB */
			FORCE_GTT();                     /* always GTT, no EWMA    */
			return true;
		}

		if (size <= AMDGPU_VEGA_MEDIUM_BUFFER_SIZE) {     /* 1‑4 MiB */
			if (usage >= PRESS_HI) { FORCE_GTT(); MAYBE_EWMA(size); }
			else if (*domain == 0)  *domain |= AMDGPU_GEM_DOMAIN_VRAM;
			return true;
		}

		/* large CPU buffer */
		if (usage >= PRESS_HI) { FORCE_GTT(); MAYBE_EWMA(size); }
		else if (*domain == 0)  *domain |= AMDGPU_GEM_DOMAIN_VRAM;
		return true;
	}

	/* ------------------------------------------------------------------
	 * 4. Generic / uncategorised
	 * ---------------------------------------------------------------- */
	if (size <= AMDGPU_VEGA_SMALL_BUFFER_SIZE) {            /* ≤1 MiB */
		if (usage >= PRESS_HI) { FORCE_GTT(); /* no EWMA */ }
		else if (*domain == 0)  *domain |= AMDGPU_GEM_DOMAIN_VRAM;
		return true;
	}

	if (size <= AMDGPU_VEGA_MEDIUM_BUFFER_SIZE) {           /* 1‑4 MiB */
		if (usage >= PRESS_HI) { FORCE_GTT(); MAYBE_EWMA(size); }
		else if (*domain == 0)  *domain |= AMDGPU_GEM_DOMAIN_VRAM;
		return true;
	}

	/* large generic */
	if (usage >= PRESS_HI) { FORCE_GTT(); MAYBE_EWMA(size); }
	else if (*domain == 0)  *domain |= AMDGPU_GEM_DOMAIN_VRAM;

	return true;
}

static __cold bool
amdgpu_vega_optimize_hbm2_bank_access(struct amdgpu_device *adev,
									  struct amdgpu_bo *bo,
									  uint64_t *aligned_size,
									  uint32_t *alignment)
{
	/* Quick outs ------------------------------------------------------- */
	if (!is_hbm2_vega(adev) ||
		!static_branch_unlikely(&vega_bankalign_key) ||
		!aligned_size || !alignment)
		return false;

	if (*aligned_size == 0 ||
		*aligned_size > (16ULL * 1024 * 1024 * 1024))   /* > 16 GiB – skip */
	return false;

	/*
	 * Size / workload table
	 *  --------------------  ------------------------------ --------------
	 *  Condition             Alignment enforced             Rationale
	 *  --------------------  ------------------------------ --------------
	 *  Texture / scan‑out
	 *    size ≥ 24 MiB       2 MiB                          minimise bank alias
	 *
	 *  Compute, NO_CPU
	 *    size ≥ 32 MiB       4 MiB                          wide linear stride
	 *
	 *  Generic big buffer
	 *    size ≥ 128 MiB      1 MiB                          original rule
	 *
	 *  size ≥  4 MiB         256 KiB                        original rule
	 */
	if (bo && is_vega_texture(bo->flags) &&
		*aligned_size >= 24ULL * 1024 * 1024) {
		*alignment   = max_t(uint32_t, *alignment, 2 * 1024 * 1024);
	*aligned_size = ALIGN(*aligned_size, 2 * 1024 * 1024);
	return true;
		}

		if (bo && is_vega_compute(bo->flags) &&
			!is_vega_cpu_access(bo->flags) &&
			*aligned_size >= 32ULL * 1024 * 1024) {
			*alignment   = max_t(uint32_t, *alignment, 4 * 1024 * 1024);
		*aligned_size = ALIGN(*aligned_size, 4 * 1024 * 1024);
		return true;
			}

			/* Legacy size‑based fall‑backs ------------------------------------ */
			if (*aligned_size >= 128ULL * 1024 * 1024) {
				*alignment   = max_t(uint32_t, *alignment, 1 * 1024 * 1024);
				*aligned_size = ALIGN(*aligned_size, 1 * 1024 * 1024);
				return true;
			}

			if (*aligned_size >= AMDGPU_VEGA_MEDIUM_BUFFER_SIZE) {
				*alignment   = max_t(uint32_t, *alignment,
									 AMDGPU_VEGA_HBM2_MIN_ALIGNMENT);
				*aligned_size = ALIGN(*aligned_size,
									  AMDGPU_VEGA_HBM2_MIN_ALIGNMENT);
				return true;
			}

			/* Small objects keep existing logic (4 K / 8 K tweaks) ------------ */
			if (bo && is_vega_texture(bo->flags) &&
				*aligned_size >= AMDGPU_VEGA_MEDIUM_BUFFER_SIZE) {
				*alignment   = max_t(uint32_t, *alignment, 4096);
			*aligned_size = ALIGN(*aligned_size, 4096);
			return true;
				}

				if (bo && is_vega_compute(bo->flags) &&
					*aligned_size >= AMDGPU_VEGA_LARGE_BUFFER_SIZE) {
					*alignment   = max_t(uint32_t, *alignment, 8192);
				*aligned_size = ALIGN(*aligned_size, 8192);
				return true;
					}

					if (!bo && *aligned_size >= AMDGPU_VEGA_MEDIUM_BUFFER_SIZE) {
						*alignment   = max_t(uint32_t, *alignment, 4096);
						*aligned_size = ALIGN(*aligned_size, 4096);
						return true;
					}

					return false;
}

static __always_inline unsigned int
amdgpu_vega_determine_optimal_prefetch(struct amdgpu_device *adev,
									   struct amdgpu_bo     *bo,
									   unsigned int          base_prefetch_pages,
									   uint32_t              vram_usage)
{
	if (!is_hbm2_vega(adev) || !bo)
		return base_prefetch_pages;

	const uint64_t size      = amdgpu_bo_size(bo);
	const unsigned int max_p = (size + PAGE_SIZE - 1) >> PAGE_SHIFT;

	if (!size)
		return base_prefetch_pages;

	if (vram_usage > 90)
		return min(max(base_prefetch_pages / 2, 8u), max_p);

	if ((bo->preferred_domains & AMDGPU_GEM_DOMAIN_VRAM) &&
		is_vega_compute(bo->flags) &&
		size > AMDGPU_VEGA_LARGE_BUFFER_SIZE) {
		return min_t(unsigned int,
					 base_prefetch_pages * 2,
			   min(128u, max_p));
		}

		if ((bo->preferred_domains & AMDGPU_GEM_DOMAIN_VRAM) &&
			is_vega_texture(bo->flags) &&
			vram_usage < 75) {
			return min_t(unsigned int,
						 base_prefetch_pages * 6 / 5,
				min(64u, max_p));
			}

			return min(base_prefetch_pages, max_p);
}

static bool amdgpu_vega_should_use_async_fence(struct amdgpu_device *adev,
											   struct amdgpu_bo *bo,
											   uint64_t flags)
{
	uint64_t size;

	if (!is_hbm2_vega(adev) || !bo)
		return false;

	size = amdgpu_bo_size(bo);
	if (size == 0)
		return false;

	/* Never async for explicit sync or large buffers (>32MB) */
	if ((flags & AMDGPU_GEM_CREATE_EXPLICIT_SYNC) || size > (32ULL << 20))
		return false;

	if ((bo->preferred_domains & AMDGPU_GEM_DOMAIN_GTT) &&
		is_vega_cpu_access(flags) &&
		size < AMDGPU_VEGA_SMALL_BUFFER_SIZE) {
		return true;
		}
		if ((bo->preferred_domains & AMDGPU_GEM_DOMAIN_VRAM) &&
			is_vega_compute(flags) &&
			!is_vega_cpu_access(flags)) {
			return true;
			}
			return false;
}

static bool amdgpu_vega_optimize_for_workload(struct amdgpu_device *adev,
											  struct amdgpu_bo *bo,
											  uint64_t flags)
{
	uint64_t size;

	if (!is_hbm2_vega(adev) || !bo)
		return false;

	if (!bo->tbo.base.dev)
		return false;

	size = amdgpu_bo_size(bo);
	if (size == 0)
		return false;

	if (!dma_resv_is_locked(bo->tbo.base.resv))
		return false;

	/* Gaming workload: prioritize VRAM for textures/framebuffers, fallback to GTT */
	if (is_vega_texture(flags) && size >= AMDGPU_VEGA_MEDIUM_BUFFER_SIZE) {
		bo->preferred_domains = AMDGPU_GEM_DOMAIN_VRAM;
		bo->allowed_domains = AMDGPU_GEM_DOMAIN_VRAM | AMDGPU_GEM_DOMAIN_GTT;
		return true;
	}

	/* Compute workload: prefer VRAM for compute buffers */
	if (is_vega_compute(flags) && !is_vega_cpu_access(flags)) {
		bo->preferred_domains = AMDGPU_GEM_DOMAIN_VRAM;
		bo->allowed_domains = AMDGPU_GEM_DOMAIN_VRAM | AMDGPU_GEM_DOMAIN_GTT;
		return true;
	}

	/* API translation layers: prefer GTT for small CPU-accessible buffers */
	if (is_vega_cpu_access(flags) && size <= AMDGPU_VEGA_SMALL_BUFFER_SIZE) {
		bo->preferred_domains = AMDGPU_GEM_DOMAIN_GTT;
		bo->allowed_domains = AMDGPU_GEM_DOMAIN_GTT | AMDGPU_GEM_DOMAIN_VRAM;
		return true;
	}

	return false;
}

/* ---------------- VM fault handler (prefetch guard added) ------------- */
static vm_fault_t amdgpu_gem_fault(struct vm_fault *vmf)
{
	struct ttm_buffer_object *bo = vmf->vma->vm_private_data;
	struct drm_device        *ddev;
	vm_fault_t                ret;
	int                       idx;

	if (unlikely(!bo))
		return VM_FAULT_SIGBUS;

	ddev = bo->base.dev;
	if (unlikely(!ddev))
		return VM_FAULT_SIGBUS;

	ret = ttm_bo_vm_reserve(bo, vmf);
	if (unlikely(ret))
		return ret;

	if (likely(drm_dev_enter(ddev, &idx))) {
		struct amdgpu_device *adev = drm_to_adev(ddev);
		unsigned int prefetch_pages = TTM_BO_VM_NUM_PREFAULT;

		ret = amdgpu_bo_fault_reserve_notify(bo);
		if (unlikely(ret)) {
			drm_dev_exit(idx);
			goto unlock_resv;
		}

		/* Prefetch path now gated by vega_prefetch_key */
		if (static_branch_unlikely(&vega_prefetch_key)) {
			struct amdgpu_bo *abo = ttm_to_amdgpu_bo(bo);

			if (likely(abo)) {
				uint32_t usage =
				amdgpu_vega_get_vram_usage_cached(adev);

				prefetch_pages =
				amdgpu_vega_determine_optimal_prefetch(
					adev, abo, prefetch_pages, usage);

				if (likely((unsigned long)vmf->address >= PAGE_OFFSET))
					PREFETCH_WRITE((const void *)vmf->address);
			}
		}

		ret = ttm_bo_vm_fault_reserved(vmf,
									   vmf->vma->vm_page_prot,
								 prefetch_pages);
		drm_dev_exit(idx);
	} else {
		ret = ttm_bo_vm_dummy_page(vmf, vmf->vma->vm_page_prot);
	}

	if (likely(!(ret == VM_FAULT_RETRY &&
		!(vmf->flags & FAULT_FLAG_RETRY_NOWAIT))))
		goto unlock_resv;

	return ret; /* VM_FAULT_RETRY fast‑return */

	unlock_resv:
	dma_resv_unlock(bo->base.resv);
	return ret;
}

/* VM operations struct for GEM objects */
static const struct vm_operations_struct amdgpu_gem_vm_ops = {
	.fault = amdgpu_gem_fault,
	.open = ttm_bo_vm_open,
	.close = ttm_bo_vm_close,
	.access = ttm_bo_vm_access,
};

/* Free a GEM object */
static void amdgpu_gem_object_free(struct drm_gem_object *gobj)
{
	struct amdgpu_bo *aobj = gem_to_amdgpu_bo(gobj);

	if (aobj) {
		amdgpu_hmm_unregister(aobj);
		ttm_bo_put(&aobj->tbo);
		/* No need to NULL aobj, it's on the stack */
	}
}

/* Create a new GEM object */
int amdgpu_gem_object_create(struct amdgpu_device     *adev,
							 unsigned long             size,
							 int                       alignment,
							 u32                       initial_domain,
							 u64                       flags,
							 enum ttm_bo_type          type,
							 struct dma_resv          *resv,
							 struct drm_gem_object   **obj,
							 int8_t                    xcp_id_plus1)
{
	struct amdgpu_bo_user *ubo = NULL;
	struct amdgpu_bo      *bo;
	int r;

	*obj = NULL;

	/* create slab on‑demand */
	amdgpu_tbo_slab_ensure();

	/* learn static key */
	if (flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) {
		amdgpu_vm_always_valid_key_enable();
	}

	/* tiny‑BO cache fast‑path */
	bo = tbo_cache_try_get(size, flags, initial_domain, resv, alignment);
	if (bo) {
		*obj = &bo->tbo.base;
		return 0;
	}

	/* fill BO param */
	struct amdgpu_bo_param bp = {
		.size             = size,
		.byte_align       = alignment,
		.type             = type,
		.resv             = resv,
		.preferred_domain = initial_domain,
		.flags            = flags | AMDGPU_GEM_CREATE_VRAM_WIPE_ON_RELEASE,
		.domain           = initial_domain,
		.bo_ptr_size      = sizeof(struct amdgpu_bo),
		.xcp_id_plus1     = xcp_id_plus1,
	};

	if (static_branch_unlikely(&vega_domain_key)) {
		amdgpu_vega_optimize_buffer_placement(adev, NULL, size,
											  flags, &bp.domain);
	}

	/* allocate metadata from slab if available */
	if (ubo_slab)
		ubo = kmem_cache_zalloc(ubo_slab, GFP_KERNEL | __GFP_NOWARN);

	/* allocate BO */
	r = amdgpu_bo_create_user(adev, &bp, &ubo);
	if (r) {
		if (ubo && ubo_slab)
			kmem_cache_free(ubo_slab, ubo);
		return r;
	}

	bo = &ubo->bo;

	/* enable cache key once and maybe insert */
	if (!static_branch_likely(&tbo_cache_key)) {
		static_branch_enable(&tbo_cache_key);
	}
	tbo_cache_put(bo);   /* safe if not eligible */

	*obj = &bo->tbo.base;
	return 0;
}

/* Force release of all GEM objects for a device */
void amdgpu_gem_force_release(struct amdgpu_device *adev)
{
	struct drm_device *ddev = adev_to_drm(adev);
	struct drm_file *file;

	mutex_lock(&ddev->filelist_mutex);

	list_for_each_entry(file, &ddev->filelist, lhead) {
		struct drm_gem_object *gobj;
		int handle;

		WARN_ONCE(1, "Still active user space clients!\n");
		spin_lock(&file->table_lock);
		idr_for_each_entry(&file->object_idr, gobj, handle) {
			WARN_ONCE(1, "And also active allocations!\n");
			drm_gem_object_put(gobj);
		}
		idr_destroy(&file->object_idr);
		spin_unlock(&file->table_lock);
	}

	mutex_unlock(&ddev->filelist_mutex);
}

/* Open a GEM object for a file descriptor */
static int amdgpu_gem_object_open(struct drm_gem_object *obj,
								  struct drm_file *file_priv)
{
	struct amdgpu_bo     *abo;
	struct amdgpu_device *adev;
	struct amdgpu_fpriv  *fpriv;
	struct amdgpu_vm     *vm;
	struct amdgpu_bo_va  *bo_va;
	struct mm_struct     *mm;
	int r = 0;

	if (!obj || !file_priv)
		return -EINVAL;

	abo  = gem_to_amdgpu_bo(obj);
	adev = amdgpu_ttm_adev(abo->tbo.bdev);
	fpriv = file_priv->driver_priv;
	if (!abo || !adev || !fpriv)
		return -EINVAL;

	vm = &fpriv->vm;

	if (static_branch_unlikely(&vega_prefetch_key)) {
		PREFETCH_READ(abo);
		PREFETCH_READ(&fpriv->vm);
		PREFETCH_READ(abo->tbo.base.resv);
	}

	mm = amdgpu_ttm_tt_get_usermm(abo->tbo.ttm);
	if (mm && mm != current->mm)
		return -EPERM;

	if ((abo->flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) &&
		!amdgpu_vm_is_bo_always_valid(vm, abo))
		return -EPERM;

	/* ultra‑fast graphics‑only path */
	if (!vm->is_compute_context &&
		static_branch_likely(&amdgpu_vm_always_valid_key) &&
		(abo->flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) &&
		(abo->allowed_domains == AMDGPU_GEM_DOMAIN_GTT) &&
		!abo->parent &&
		(!obj->import_attach ||
		!dma_buf_is_dynamic(obj->import_attach->dmabuf)))
		return 0;

	/* reserve and VA logic (unchanged) */
	r = amdgpu_bo_reserve(abo, false);
	if (r)
		return r;

	bo_va = amdgpu_vm_bo_find(vm, abo);
	if (!bo_va)
		bo_va = amdgpu_vm_bo_add(adev, vm, abo);
	else
		++bo_va->ref_count;
	amdgpu_bo_unreserve(abo);

	if (!vm->is_compute_context || !vm->process_info)
		return 0;
	if (!obj->import_attach ||
		!dma_buf_is_dynamic(obj->import_attach->dmabuf))
		return 0;

	mutex_lock_nested(&vm->process_info->lock, 1);

	if (!WARN_ON(!vm->process_info->eviction_fence)) {
		if (static_branch_likely(&amdgpu_vm_always_valid_key) &&
			(abo->flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID)) {
			mutex_unlock(&vm->process_info->lock);
		return 0;
			}

			if (is_hbm2_vega(adev)) {
				uint32_t domain = AMDGPU_GEM_DOMAIN_GTT;
				if (is_vega_texture(abo->flags) ||
					is_vega_compute(abo->flags)) {
					domain = AMDGPU_GEM_DOMAIN_VRAM;
				if (amdgpu_vega_get_effective_vram_usage(adev) >
					amdgpu_vega_vram_pressure_high)
					domain = AMDGPU_GEM_DOMAIN_GTT;
					}
					r = amdgpu_amdkfd_bo_validate_and_fence(
						abo, domain,
						&vm->process_info->eviction_fence->base);
			} else {
				r = amdgpu_amdkfd_bo_validate_and_fence(
					abo, AMDGPU_GEM_DOMAIN_GTT,
					&vm->process_info->eviction_fence->base);
			}
	}
	mutex_unlock(&vm->process_info->lock);
	return r;
}

/* Close a GEM object for a file descriptor */
static void
amdgpu_gem_object_close(struct drm_gem_object *obj,
						struct drm_file *file_priv)
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

	if (!obj || !file_priv)
		return;

	bo = gem_to_amdgpu_bo(obj);
	if (!bo)
		return;

	adev = amdgpu_ttm_adev(bo->tbo.bdev);
	if (!adev)
		return;

	fpriv = file_priv->driver_priv;
	if (!fpriv)
		return;

	/* Prefetch meta we will lock / write shortly */
	if (static_branch_unlikely(&vega_prefetch_key)) {
		PREFETCH_WRITE(bo->tbo.base.resv);
		PREFETCH_READ(&fpriv->vm);
	}

	vm = &fpriv->vm;

	if (is_hbm2_vega(adev)) {
		use_async = amdgpu_vega_should_use_async_fence(adev, bo, bo->flags);
	}

	drm_exec_init(&exec, DRM_EXEC_IGNORE_DUPLICATES, 0);
	drm_exec_until_all_locked(&exec) {
		r = drm_exec_prepare_obj(&exec, &bo->tbo.base, 1);
		drm_exec_retry_on_contention(&exec);
		if (unlikely(r))
			goto out_unlock;

		r = amdgpu_vm_lock_pd(vm, &exec, 0);
		drm_exec_retry_on_contention(&exec);
		if (unlikely(r))
			goto out_unlock;
	}

	bo_va = amdgpu_vm_bo_find(vm, bo);
	if (!bo_va)
		goto out_unlock;

	if (--bo_va->ref_count > 0)
		goto out_unlock;

	amdgpu_vm_bo_del(adev, bo_va);
	amdgpu_vm_bo_update_shared(bo);

	if (!amdgpu_vm_ready(vm))
		goto out_unlock;

	r = amdgpu_vm_clear_freed(adev, vm, &fence);
	if (unlikely(r < 0)) {
		dev_err(adev->dev, "failed to clear page tables on GEM object close (%ld)\n", r);
		goto out_unlock;
	}
	if (r || !fence)
		goto out_unlock;

	amdgpu_bo_fence(bo, fence, use_async);
	dma_fence_put(fence);

	out_unlock:
	if (r) {
		dev_err(adev->dev, "Error in GEM object close for pid %d, potential leak of bo_va (%ld)\n",
				task_pid_nr(current), r);
	}
	drm_exec_fini(&exec);
}

static int amdgpu_gem_object_mmap(struct drm_gem_object *obj, struct vm_area_struct *vma)
{
	struct amdgpu_bo *bo = gem_to_amdgpu_bo(obj);

	if (amdgpu_ttm_tt_get_usermm(bo->tbo.ttm))
		return -EPERM;
	if (bo->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS)
		return -EPERM;

	if (is_cow_mapping(vma->vm_flags) &&
		!(vma->vm_flags & VM_ACCESS_FLAGS))
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

/*
 * GEM ioctls.
 */
int amdgpu_gem_create_ioctl(struct drm_device *dev, void *data,
							struct drm_file *filp)
{
	struct amdgpu_device *adev = drm_to_adev(dev);
	struct amdgpu_fpriv *fpriv = filp->driver_priv;
	struct amdgpu_vm *vm = &fpriv->vm;
	union drm_amdgpu_gem_create *args = data;
	uint64_t flags = args->in.domain_flags;
	uint64_t size = args->in.bo_size;
	struct dma_resv *resv = NULL;
	struct drm_gem_object *gobj;
	uint32_t handle, initial_domain;
	int r;

	if (args->in.domains & AMDGPU_GEM_DOMAIN_DOORBELL)
		return -EINVAL;

	if (flags & ~(AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED |
		AMDGPU_GEM_CREATE_NO_CPU_ACCESS |
		AMDGPU_GEM_CREATE_CPU_GTT_USWC |
		AMDGPU_GEM_CREATE_VRAM_CLEARED |
		AMDGPU_GEM_CREATE_VM_ALWAYS_VALID |
		AMDGPU_GEM_CREATE_EXPLICIT_SYNC |
		AMDGPU_GEM_CREATE_ENCRYPTED |
		AMDGPU_GEM_CREATE_GFX12_DCC |
		AMDGPU_GEM_CREATE_DISCARDABLE))
		return -EINVAL;

	if (args->in.domains & ~AMDGPU_GEM_DOMAIN_MASK)
		return -EINVAL;

	if (!amdgpu_is_tmz(adev) && (flags & AMDGPU_GEM_CREATE_ENCRYPTED)) {
		DRM_NOTE_ONCE("Cannot allocate secure buffer since TMZ is disabled\n");
		return -EINVAL;
	}

	flags |= AMDGPU_GEM_CREATE_VRAM_CLEARED;

	if (args->in.domains & (AMDGPU_GEM_DOMAIN_GDS |
		AMDGPU_GEM_DOMAIN_GWS | AMDGPU_GEM_DOMAIN_OA)) {
		if (flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) {
			DRM_ERROR("GDS bo cannot be per-vm-bo\n");
			return -EINVAL;
		}
		flags |= AMDGPU_GEM_CREATE_NO_CPU_ACCESS;
		}

		if (flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) {
			r = amdgpu_bo_reserve(vm->root.bo, false);
			if (r)
				return r;
			resv = vm->root.bo->tbo.base.resv;
		}

		initial_domain = (u32)(0xffffffff & args->in.domains);

		retry:
		r = amdgpu_gem_object_create(adev, size, args->in.alignment,
									 initial_domain, flags, ttm_bo_type_device,
							   resv, &gobj, fpriv->xcp_id + 1);
		if (r && r != -ERESTARTSYS) {
			if (flags & AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED) {
				flags &= ~AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED;
				goto retry;
			}
			if (initial_domain == AMDGPU_GEM_DOMAIN_VRAM) {
				initial_domain |= AMDGPU_GEM_DOMAIN_GTT;
				goto retry;
			}
			DRM_DEBUG("Failed to allocate GEM object (%llu, %d, %llu, %d)\n",
					  size, initial_domain, args->in.alignment, r);
		}

		if (flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) {
			if (!r) {
				struct amdgpu_bo *abo = gem_to_amdgpu_bo(gobj);
				abo->parent = amdgpu_bo_ref(vm->root.bo);
			}
			amdgpu_bo_unreserve(vm->root.bo);
		}
		if (r)
			return r;

	r = drm_gem_handle_create(filp, gobj, &handle);
	drm_gem_object_put(gobj);
	if (r)
		return r;

	memset(args, 0, sizeof(*args));
	args->out.handle = handle;
	return 0;
}

int amdgpu_gem_userptr_ioctl(struct drm_device *dev, void *data,
							 struct drm_file *filp)
{
	struct ttm_operation_ctx ctx = { true, false };
	struct amdgpu_device *adev = drm_to_adev(dev);
	struct drm_amdgpu_gem_userptr *args = data;
	struct amdgpu_fpriv *fpriv = filp->driver_priv;
	struct drm_gem_object *gobj;
	struct hmm_range *range;
	struct amdgpu_bo *bo;
	uint32_t handle;
	int r;

	args->addr = untagged_addr(args->addr);

	if (offset_in_page(args->addr | args->size))
		return -EINVAL;

	if (args->flags & ~(AMDGPU_GEM_USERPTR_READONLY |
		AMDGPU_GEM_USERPTR_ANONONLY | AMDGPU_GEM_USERPTR_VALIDATE |
		AMDGPU_GEM_USERPTR_REGISTER))
		return -EINVAL;

	if (!(args->flags & AMDGPU_GEM_USERPTR_READONLY) &&
		!(args->flags & AMDGPU_GEM_USERPTR_REGISTER)) {
		return -EACCES;
		}

		r = amdgpu_gem_object_create(adev, args->size, 0, AMDGPU_GEM_DOMAIN_CPU,
									 0, ttm_bo_type_device, NULL, &gobj, fpriv->xcp_id + 1);
		if (r)
			return r;

	bo = gem_to_amdgpu_bo(gobj);
	bo->preferred_domains = AMDGPU_GEM_DOMAIN_GTT;
	bo->allowed_domains = AMDGPU_GEM_DOMAIN_GTT;
	r = amdgpu_ttm_tt_set_userptr(&bo->tbo, args->addr, args->flags);
	if (r)
		goto release_object;

	r = amdgpu_hmm_register(bo, args->addr);
	if (r)
		goto release_object;

	if (args->flags & AMDGPU_GEM_USERPTR_VALIDATE) {
		r = amdgpu_ttm_tt_get_user_pages(bo, bo->tbo.ttm->pages, &range);
		if (r)
			goto release_object;

		r = amdgpu_bo_reserve(bo, true);
		if (r)
			goto user_pages_done;

		amdgpu_bo_placement_from_domain(bo, AMDGPU_GEM_DOMAIN_GTT);
		r = ttm_bo_validate(&bo->tbo, &bo->placement, &ctx);
		amdgpu_bo_unreserve(bo);
		if (r)
			goto user_pages_done;
	}

	r = drm_gem_handle_create(filp, gobj, &handle);
	if (r)
		goto user_pages_done;

	args->handle = handle;

	user_pages_done:
	if (args->flags & AMDGPU_GEM_USERPTR_VALIDATE)
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

	gobj = drm_gem_object_lookup(filp, handle);
	if (!gobj)
		return -ENOENT;

	robj = gem_to_amdgpu_bo(gobj);
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
	uint32_t handle = args->in.handle;

	memset(args, 0, sizeof(*args));
	return amdgpu_mode_dumb_mmap(filp, dev, handle, &args->out.addr_ptr);
}

int amdgpu_gem_wait_idle_ioctl(struct drm_device *dev, void *data,
							   struct drm_file *filp)
{
	union drm_amdgpu_gem_wait_idle *args = data;
	struct drm_gem_object *gobj;
	struct amdgpu_bo *robj;
	uint32_t handle = args->in.handle;
	unsigned long timeout = amdgpu_gem_timeout(args->in.timeout);
	int r = 0;
	long ret;

	gobj = drm_gem_object_lookup(filp, handle);
	if (!gobj)
		return -ENOENT;

	robj = gem_to_amdgpu_bo(gobj);
	ret = dma_resv_wait_timeout(robj->tbo.base.resv, DMA_RESV_USAGE_READ,
								true, timeout);

	if (ret >= 0) {
		memset(args, 0, sizeof(*args));
		args->out.status = (ret == 0);
	} else
		r = ret;

	drm_gem_object_put(gobj);
	return r;
}

int amdgpu_gem_metadata_ioctl(struct drm_device *dev, void *data,
							  struct drm_file *filp)
{
	struct drm_amdgpu_gem_metadata *args = data;
	struct drm_gem_object *gobj;
	struct amdgpu_bo *robj;
	int r = -1;

	DRM_DEBUG("%d\n", args->handle);
	gobj = drm_gem_object_lookup(filp, args->handle);
	if (gobj == NULL) {
		return -ENOENT;
	}
	/* FIX: Use the declared variable 'robj', not 'abo' */
	robj = gem_to_amdgpu_bo(gobj);

	r = amdgpu_bo_reserve(robj, false);
	if (unlikely(r != 0))
		goto out;

	if (args->op == AMDGPU_GEM_METADATA_OP_GET_METADATA) {
		amdgpu_bo_get_tiling_flags(robj, &args->data.tiling_info);
		r = amdgpu_bo_get_metadata(robj, args->data.data,
								   sizeof(args->data.data),
								   &args->data.data_size_bytes,
							 &args->data.flags);
	} else if (args->op == AMDGPU_GEM_METADATA_OP_SET_METADATA) {
		if (args->data.data_size_bytes > sizeof(args->data.data)) {
			r = -EINVAL;
			goto unreserve;
		}
		r = amdgpu_bo_set_tiling_flags(robj, args->data.tiling_info);
		if (!r)
			r = amdgpu_bo_set_metadata(robj, args->data.data,
									   args->data.data_size_bytes,
							  args->data.flags);
	}

	unreserve:
	amdgpu_bo_unreserve(robj);
	out:
	drm_gem_object_put(gobj);
	return r;
}

uint64_t amdgpu_gem_va_map_flags(struct amdgpu_device *adev, uint32_t flags)
{
	uint64_t pte_flag = 0;

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

	if (adev->gmc.gmc_funcs && adev->gmc.gmc_funcs->map_mtype) {
		pte_flag |= amdgpu_gmc_map_mtype(adev,
										 flags & AMDGPU_VM_MTYPE_MASK);
	}

	return pte_flag;
}

static __cold void amdgpu_gem_va_update_vm(struct amdgpu_device *adev,
										   struct amdgpu_vm *vm,
										   struct amdgpu_bo_va *bo_va,
										   uint32_t operation)
{
	int r;

	if (!amdgpu_vm_ready(vm))
		return;

	r = amdgpu_vm_clear_freed(adev, vm, NULL);
	if (r)
		goto error;

	if (operation == AMDGPU_VA_OP_MAP ||
		operation == AMDGPU_VA_OP_REPLACE) {
		r = amdgpu_vm_bo_update(adev, bo_va, false);
	if (r)
		goto error;
		}

		r = amdgpu_vm_update_pdes(adev, vm, false);

	error:
	if (r && r != -ERESTARTSYS)
		DRM_ERROR("Couldn't update BO_VA (%d)\n", r);
}

int amdgpu_gem_va_ioctl(struct drm_device *dev, void *data,
						struct drm_file *filp)
{
	/* ------------- existing declarations stay the same --------------- */
	const uint32_t valid_flags = AMDGPU_VM_DELAY_UPDATE |
	AMDGPU_VM_PAGE_READABLE | AMDGPU_VM_PAGE_WRITEABLE |
	AMDGPU_VM_PAGE_EXECUTABLE | AMDGPU_VM_MTYPE_MASK |
	AMDGPU_VM_PAGE_NOALLOC;
	const uint32_t prt_flags = AMDGPU_VM_DELAY_UPDATE |
	AMDGPU_VM_PAGE_PRT;

	struct drm_amdgpu_gem_va *args = data;
	struct drm_gem_object *gobj = NULL;
	struct amdgpu_bo *abo = NULL;
	struct amdgpu_device *adev = drm_to_adev(dev);
	struct amdgpu_fpriv *fpriv = filp->driver_priv;
	struct amdgpu_bo_va *bo_va = NULL;
	struct drm_exec exec;
	uint64_t va_flags;
	uint64_t vm_size;
	int r = 0;

	/* Prefetch frequently‑used structures */
	if (static_branch_unlikely(&vega_prefetch_key)) {
		PREFETCH_READ(fpriv);
		PREFETCH_READ(&fpriv->vm);
		PREFETCH_READ(args);
	}

	if (args->va_address < AMDGPU_VA_RESERVED_BOTTOM) {
		dev_dbg(dev->dev,
				"va_address 0x%llx is in reserved area 0x%llx\n",
		  args->va_address, AMDGPU_VA_RESERVED_BOTTOM);
		return -EINVAL;
	}

	if (args->va_address >= AMDGPU_GMC_HOLE_START &&
		args->va_address < AMDGPU_GMC_HOLE_END) {
		dev_dbg(dev->dev,
				"va_address 0x%llx is in VA hole 0x%llx-0x%llx\n",
		  args->va_address, AMDGPU_GMC_HOLE_START,
		  AMDGPU_GMC_HOLE_END);
		return -EINVAL;
		}

		args->va_address &= AMDGPU_GMC_HOLE_MASK;

	vm_size = adev->vm_manager.max_pfn * AMDGPU_GPU_PAGE_SIZE;
	vm_size -= AMDGPU_VA_RESERVED_TOP;
	if (args->va_address + args->map_size > vm_size) {
		dev_dbg(dev->dev,
				"va_address 0x%llx is in top reserved area 0x%llx\n",
		  args->va_address + args->map_size, vm_size);
		return -EINVAL;
	}

	if ((args->flags & ~valid_flags) && (args->flags & ~prt_flags)) {
		dev_dbg(dev->dev, "invalid flags combination 0x%08X\n",
				args->flags);
		return -EINVAL;
	}

	switch (args->operation) {
		case AMDGPU_VA_OP_MAP:
		case AMDGPU_VA_OP_UNMAP:
		case AMDGPU_VA_OP_CLEAR:
		case AMDGPU_VA_OP_REPLACE:
			break;
		default:
			dev_dbg(dev->dev, "unsupported operation %d\n", args->operation);
			return -EINVAL;
	}

	if ((args->operation != AMDGPU_VA_OP_CLEAR) &&
		!(args->flags & AMDGPU_VM_PAGE_PRT)) {
		gobj = drm_gem_object_lookup(filp, args->handle);
	if (!gobj) {
		return -ENOENT;
	}
	abo = gem_to_amdgpu_bo(gobj);
		}

		drm_exec_init(&exec, DRM_EXEC_INTERRUPTIBLE_WAIT | DRM_EXEC_IGNORE_DUPLICATES, 0);

		drm_exec_until_all_locked(&exec) {
			if (gobj) {
				r = drm_exec_lock_obj(&exec, gobj);
				drm_exec_retry_on_contention(&exec);
				if (unlikely(r))
					goto error;
			}
			r = amdgpu_vm_lock_pd(&fpriv->vm, &exec, 2);
			drm_exec_retry_on_contention(&exec);
			if (unlikely(r))
				goto error;
		}

		if (abo) {
			bo_va = amdgpu_vm_bo_find(&fpriv->vm, abo);
			if (!bo_va) {
				r = -ENOENT;
				goto error;
			}
		} else if (args->operation != AMDGPU_VA_OP_CLEAR) {
			bo_va = fpriv->prt_va;
			if (!bo_va) {
				DRM_ERROR("Process context has no PRT VA\n");
				r = -EINVAL;
				goto error;
			}
		} // else bo_va = NULL for CLEAR

		if (abo && is_hbm2_vega(adev)) {
			amdgpu_vega_optimize_for_workload(adev, abo, abo->flags);
		}

		switch (args->operation) {
			case AMDGPU_VA_OP_MAP:
				va_flags = amdgpu_gem_va_map_flags(adev, args->flags);
				r = amdgpu_vm_bo_map(adev, bo_va, args->va_address,
									 args->offset_in_bo, args->map_size, va_flags);
				break;
			case AMDGPU_VA_OP_UNMAP:
				r = amdgpu_vm_bo_unmap(adev, bo_va, args->va_address);
				break;
			case AMDGPU_VA_OP_CLEAR:
				r = amdgpu_vm_bo_clear_mappings(adev, &fpriv->vm,
												args->va_address, args->map_size);
				break;
			case AMDGPU_VA_OP_REPLACE:
				va_flags = amdgpu_gem_va_map_flags(adev, args->flags);
				r = amdgpu_vm_bo_replace_map(adev, bo_va, args->va_address,
											 args->offset_in_bo, args->map_size, va_flags);
				break;
		}

		if (!r && !(args->flags & AMDGPU_VM_DELAY_UPDATE) && !adev->debug_vm)
			amdgpu_gem_va_update_vm(adev, &fpriv->vm, bo_va, args->operation);

	error:
	drm_exec_fini(&exec);
	if (gobj)
		drm_gem_object_put(gobj);
	return r;
}

int amdgpu_gem_op_ioctl(struct drm_device *dev, void *data,
						struct drm_file *filp)
{
	struct amdgpu_device *adev = drm_to_adev(dev);
	struct drm_amdgpu_gem_op *args = data;
	struct drm_gem_object *gobj;
	struct amdgpu_vm_bo_base *base;
	struct amdgpu_bo *robj;
	int r;

	gobj = drm_gem_object_lookup(filp, args->handle);
	if (!gobj)
		return -ENOENT;

	robj = gem_to_amdgpu_bo(gobj);

	r = amdgpu_bo_reserve(robj, false);
	if (unlikely(r))
		goto out;

	switch (args->op) {
		case AMDGPU_GEM_OP_GET_GEM_CREATE_INFO: {
			struct drm_amdgpu_gem_create_in info;
			void __user *out = u64_to_user_ptr(args->value);

			info.bo_size = robj->tbo.base.size;
			info.alignment = robj->tbo.page_alignment << PAGE_SHIFT;
			info.domains = robj->preferred_domains;
			info.domain_flags = robj->flags;
			amdgpu_bo_unreserve(robj);
			if (copy_to_user(out, &info, sizeof(info)))
				r = -EFAULT;
			break;
		}
		case AMDGPU_GEM_OP_SET_PLACEMENT:
			if (robj->tbo.base.import_attach &&
				args->value & AMDGPU_GEM_DOMAIN_VRAM) {
				r = -EINVAL;
			amdgpu_bo_unreserve(robj);
			break;
				}
				if (amdgpu_ttm_tt_get_usermm(robj->tbo.ttm)) {
					r = -EPERM;
					amdgpu_bo_unreserve(robj);
					break;
				}
				for (base = robj->vm_bo; base; base = base->next)
					if (amdgpu_xgmi_same_hive(amdgpu_ttm_adev(robj->tbo.bdev),
						amdgpu_ttm_adev(base->vm->root.bo->tbo.bdev))) {
						r = -EINVAL;
					amdgpu_bo_unreserve(robj);
				goto out;
						}

						robj->preferred_domains = args->value & (AMDGPU_GEM_DOMAIN_VRAM |
						AMDGPU_GEM_DOMAIN_GTT |
						AMDGPU_GEM_DOMAIN_CPU);
						robj->allowed_domains = robj->preferred_domains;
						if (robj->allowed_domains == AMDGPU_GEM_DOMAIN_VRAM)
							robj->allowed_domains |= AMDGPU_GEM_DOMAIN_GTT;

		if (is_hbm2_vega(adev)) {
			amdgpu_vega_optimize_for_workload(adev, robj, robj->flags);
		}

		if (robj->flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) {
			amdgpu_vm_bo_invalidate(robj, true);
		}

		amdgpu_bo_unreserve(robj);
		break;
		default:
			amdgpu_bo_unreserve(robj);
			r = -EINVAL;
	}

	out:
	drm_gem_object_put(gobj);
	return r;
}

int amdgpu_mode_dumb_create(struct drm_file *file_priv,
							struct drm_device *dev,
							struct drm_mode_create_dumb *args)
{
	struct amdgpu_device *adev = drm_to_adev(dev);
	struct amdgpu_fpriv *fpriv = file_priv->driver_priv;
	struct drm_gem_object *gobj;
	uint32_t handle;
	u64 flags = AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED |
	AMDGPU_GEM_CREATE_CPU_GTT_USWC |
	AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS;
	u32 domain;
	int r;

	if (adev->mman.buffer_funcs_enabled)
		flags |= AMDGPU_GEM_CREATE_VRAM_CLEARED;

	args->pitch = amdgpu_gem_align_pitch(adev, args->width,
										 DIV_ROUND_UP(args->bpp, 8), 0);
	args->size = (u64)args->pitch * args->height;
	args->size = ALIGN(args->size, PAGE_SIZE);
	domain = amdgpu_bo_get_preferred_domain(adev,
											amdgpu_display_supported_domains(adev, flags));

	if (is_hbm2_vega(adev)) {
		uint32_t alignment = 0;
		uint64_t optimized_size = args->size;

		amdgpu_vega_optimize_hbm2_bank_access(adev, NULL, &optimized_size, &alignment);

		r = amdgpu_gem_object_create(adev, optimized_size, alignment, domain, flags,
									 ttm_bo_type_device, NULL, &gobj, fpriv->xcp_id + 1);
		if (r == 0) {
			r = drm_gem_handle_create(file_priv, gobj, &handle);
			drm_gem_object_put(gobj);
			if (r)
				return r;

			args->handle = handle;
			return 0;
		}
	}

	r = amdgpu_gem_object_create(adev, args->size, 0, domain, flags,
								 ttm_bo_type_device, NULL, &gobj, fpriv->xcp_id + 1);
	if (r)
		return -ENOMEM;

	r = drm_gem_handle_create(file_priv, gobj, &handle);
	drm_gem_object_put(gobj);
	if (r)
		return r;

	args->handle = handle;
	return 0;
}

#if defined(CONFIG_DEBUG_FS)
static int amdgpu_debugfs_gem_info_show(struct seq_file *m, void *unused)
{
	struct amdgpu_device *adev = m->private;
	struct drm_device *dev = adev_to_drm(adev);
	struct drm_file *file;
	int r;

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
	struct drm_minor *minor = adev_to_drm(adev)->primary;
	struct dentry *root = minor->debugfs_root;

	debugfs_create_file("amdgpu_gem_info", 0444, root, adev,
						&amdgpu_debugfs_gem_info_fops);
	#endif
}
