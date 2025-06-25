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

/* --- Forward declarations to silence compiler warnings --- */
static inline u32 pb_get_bias(void);
static bool amdgpu_vega_optimize_for_workload(struct amdgpu_device *adev,
											  struct amdgpu_bo *bo,
											  uint64_t flags);
void amdgpu_vega_vram_thresholds_init(void);

#if defined(CONFIG_X86_TSC)
# define KTIME_FAST_NS()  ktime_get_mono_fast_ns()
#else
# define KTIME_FAST_NS()  ktime_get_ns()
#endif

/*
 * VRAM Pressure Governor - A resilient, state-aware management system.
 */
enum vega_vram_pressure_state {
	VEGA_VRAM_GREEN,
	VEGA_VRAM_YELLOW,
	VEGA_VRAM_RED,
};

struct vega_pressure_cache_pc {
	enum vega_vram_pressure_state state;
	u32 pct_last;
	unsigned long jts_last_update;
};
static DEFINE_PER_CPU(struct vega_pressure_cache_pc, pressure_cache_pc);

struct vega_vram_state {
	seqcount_t seq;
	u64        size_bytes;
	u64        reciprocal_100;
	atomic_t   eviction_rate_ewma;
};
static struct vega_vram_state vram_state;
static DEFINE_MUTEX(vram_reciprocal_lock);

#define PB_ENTRIES          32U
#define PB_HASH_MASK        (PB_ENTRIES - 1)
#define EW_UNIT_SHIFT       4
#define EW_INC_PER_FAULT    16
#define MAX_EWMA            (16 << EW_UNIT_SHIFT)
#define PB_DECAY_FACTOR_NS (128 * NSEC_PER_MSEC)

struct pid_bias_entry {
	u32 tgid;
	u16 ewma;
	u64 ns_last;
};
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
	}
}

#define TBO_CACHE_DEPTH 32
#define TBO_MAX_BYTES   (64u << 10)
#define TBO_CACHEABLE_VRAM_FLAGS (AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS | \
AMDGPU_GEM_CREATE_NO_CPU_ACCESS   | \
AMDGPU_GEM_CREATE_VRAM_CLEARED)

struct tiny_bo_cache {
	struct amdgpu_bo *slot[TBO_CACHE_DEPTH];
	u8                top;
};

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
	if (!s)
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
	struct tiny_bo_cache *c;
	struct amdgpu_bo *bo;

	if (!static_branch_unlikely(&tbo_cache_key))
		return NULL;

	if (size > TBO_MAX_BYTES || resv || align > PAGE_SIZE)
		return NULL;

	if (domain == AMDGPU_GEM_DOMAIN_GTT) {
		if (user_flags & ~(AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED |
			AMDGPU_GEM_CREATE_CPU_GTT_USWC))
			return NULL;
	} else if (domain == AMDGPU_GEM_DOMAIN_VRAM) {
		if (user_flags & ~TBO_CACHEABLE_VRAM_FLAGS)
			return NULL;
	} else {
		return NULL;
	}

	c = this_cpu_ptr(&tiny_bo_cache);
	if (!c->top)
		return NULL;

	bo = c->slot[--c->top];
	if (!bo || bo->tbo.ttm) {   /* migration in progress → skip */
		c->top++;
		return NULL;
	}

	/* Intel uArch: tell HW we will write the reservation soon */
	prefetchw(bo->tbo.base.resv);
	#ifdef CONFIG_X86
	if (domain == AMDGPU_GEM_DOMAIN_VRAM)
		clflushopt(bo);          /* 1st line: hide later WB stall */
		#endif
		prefetch(bo);
	return bo;
}

static bool tbo_cache_put(struct amdgpu_bo *bo)
{
	struct tiny_bo_cache *c;
	u64 flags;

	if (unlikely(!static_branch_unlikely(&tbo_cache_key) || !bo))
		return false;

	if (unlikely(bo->tbo.base.size > TBO_MAX_BYTES) ||
		unlikely((bo->tbo.page_alignment << PAGE_SHIFT) > PAGE_SIZE) ||
		unlikely(bo->tbo.ttm))
		return false;

	flags = bo->flags & ~AMDGPU_GEM_CREATE_VRAM_WIPE_ON_RELEASE;

	switch (bo->preferred_domains) {
		case AMDGPU_GEM_DOMAIN_GTT:
			if (flags & ~(AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED |
				AMDGPU_GEM_CREATE_CPU_GTT_USWC))
				return false;
			break;
		case AMDGPU_GEM_DOMAIN_VRAM:
			if ((flags & TBO_CACHEABLE_VRAM_FLAGS) != TBO_CACHEABLE_VRAM_FLAGS)
				return false;
		break;
		default:
			return false;
	}

	c = this_cpu_ptr(&tiny_bo_cache);
	if (unlikely(c->top >= TBO_CACHE_DEPTH))
		return false;

	c->slot[c->top++] = bo;
	return true;
}

#define AMDGPU_VEGA_HBM2_BANK_SIZE       (1ULL * 1024 * 1024)
#define AMDGPU_VEGA_SMALL_BUFFER_SIZE    (1ULL * 1024 * 1024)
#define AMDGPU_VEGA_MEDIUM_BUFFER_SIZE   (4ULL * 1024 * 1024)
#define AMDGPU_VEGA_LARGE_BUFFER_SIZE    (16ULL * 1024 * 1024)
#define AMDGPU_VEGA_HBM2_MIN_ALIGNMENT   (256 * 1024)
#define FAST_VA_MAP_MAX_BYTES	(64u << 10)

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
	struct ttm_resource_manager *vram_man;
	static DEFINE_PER_CPU(u64, recip_cache);
	static DEFINE_PER_CPU(unsigned long, recip_jiffies);

	u64 used, size, recip;
	unsigned long now_j = READ_ONCE(jiffies);
	unsigned int seq;

	vram_man = ttm_manager_type(&adev->mman.bdev, TTM_PL_VRAM);
	if (unlikely(!vram_man))
		return 0;

	used = ttm_resource_manager_usage(vram_man);
	size = READ_ONCE(adev->gmc.mc_vram_size);
	if (unlikely(!size))
		return 0;

	recip = this_cpu_read(recip_cache);
	if (unlikely(now_j - this_cpu_read(recip_jiffies) > HZ / 250)) {
		retry:
		seq = read_seqcount_begin(&vram_state.seq);
		recip = READ_ONCE(vram_state.reciprocal_100);
		if (read_seqcount_retry(&vram_state.seq, seq))
			goto retry;

		this_cpu_write(recip_cache,   recip);
		this_cpu_write(recip_jiffies, now_j);
	}

	return min_t(u32, mul_u64_u64_shr(used, recip, 38), 100);
}

static __always_inline enum vega_vram_pressure_state
amdgpu_vega_get_vram_pressure_state(struct amdgpu_device *adev)
{
	struct vega_pressure_cache_pc *c = this_cpu_ptr(&pressure_cache_pc);
	unsigned long now_jts = jiffies;

	if (likely(time_before(now_jts, c->jts_last_update + HZ / 100)))
		return c->state;

	static unsigned long last_decay_j;
	if (time_after_eq(now_jts, last_decay_j + HZ / 4)) {
		int ev = atomic_read(&vram_state.eviction_rate_ewma);
		ev -= (ev + 15) >> 4;
		if (ev < 0)
			ev = 0;
		atomic_set(&vram_state.eviction_rate_ewma, ev);
		last_decay_j = now_jts;
	}

	u32 pct_now   = __amdgpu_vega_get_vram_usage(adev);
	u32 bias      = atomic_read(&vram_state.eviction_rate_ewma) >> EW_UNIT_SHIFT;
	u32 yellow_th = max_t(u32, amdgpu_vega_vram_pressure_mid  - bias, 50);
	u32 red_th    = max_t(u32, amdgpu_vega_vram_pressure_high - bias, 75);

	enum vega_vram_pressure_state new_state;

	if (pct_now > red_th)
		new_state = VEGA_VRAM_RED;
	else if (pct_now > yellow_th)
		new_state = VEGA_VRAM_YELLOW;
	else
		new_state = VEGA_VRAM_GREEN;

	if (new_state != c->state) {
		s32 delta = (s32)pct_now - (s32)c->pct_last;
		if (abs(delta) < 3)
			new_state = c->state;
	}

	c->state          = new_state;
	c->pct_last       = pct_now;
	c->jts_last_update = now_jts;
	return new_state;
}

static inline u32 pb_hash(u32 tgid)
{
	return (tgid * 0x9E3779B9u) & PB_HASH_MASK;
}

static inline void pb_decay(struct pid_bias_entry *e, u64 now_ns)
{
	u64 delta_ns;

	if (!e->ewma) {
		e->ns_last = now_ns;
		return;
	}

	delta_ns = now_ns - e->ns_last;
	if (!delta_ns)
		return;

	u32 shifts = div64_u64(delta_ns, PB_DECAY_FACTOR_NS);
	if (shifts) {
		if (shifts >= 8)
			e->ewma = 0;
		else
			e->ewma >>= shifts;
		e->ns_last = now_ns;
	}
}

static inline u32 pb_get_bias(void)
{
	const u32 tgid = current->tgid;
	struct pid_bias_entry *tbl = this_cpu_ptr(pid_bias_tbl);
	u32 h = pb_hash(tgid);
	u64 now = ktime_get_coarse_ns();

	for (u32 i = 0; i < PB_ENTRIES; ++i, h = (h + 1) & PB_HASH_MASK) {
		struct pid_bias_entry *e = &tbl[h];

		if (e->tgid == tgid) {
			if (now - e->ns_last > NSEC_PER_SEC)
				e->ewma = 0;

			pb_decay(e, now);
			return e->ewma >> EW_UNIT_SHIFT;
		}
		if (!e->tgid)
			break;
	}
	return 0;
}

static void pb_account_eviction(void)
{
	const u32 tgid = current->tgid;
	struct pid_bias_entry *tbl = this_cpu_ptr(pid_bias_tbl);
	u32 h = pb_hash(tgid);
	u64 now_ns = ktime_get_coarse_ns();
	u32 victim = PB_ENTRIES;
	u16 victim_ew = U16_MAX;

	atomic_add_unless(&vram_state.eviction_rate_ewma,
					  EW_INC_PER_FAULT, INT_MAX);

	for (u32 i = 0; i < PB_ENTRIES; ++i, h = (h + 1) & PB_HASH_MASK) {
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

	preempt_disable();
	{
		struct pid_bias_entry *e = &tbl[victim];

		if (e->tgid != tgid && e->tgid != 0)
			pb_decay(e, now_ns);

		e->tgid   = tgid;
		e->ewma   = min_t(u16, e->ewma + EW_INC_PER_FAULT, MAX_EWMA);
		e->ns_last = now_ns;
	}
	preempt_enable();
}

static __cold bool
amdgpu_vega_optimize_hbm2_bank_access(struct amdgpu_device *adev,
									  struct amdgpu_bo     *bo,
									  u64                  *aligned_size,
									  u32                  *alignment)
{
	enum vega_vram_pressure_state state;
	u32 new_align;
	u64 sz;

	if (!adev || !aligned_size || !alignment)
		return false;

	if (!is_hbm2_vega(adev) ||
		!static_branch_unlikely(&vega_bankalign_key) ||
		!*aligned_size)
		return false;

	state = amdgpu_vega_get_vram_pressure_state(adev);
	if (state != VEGA_VRAM_GREEN)
		return false;

	new_align = *alignment;
	sz        = *aligned_size;

	if (sz >= (64ULL << 20))
		new_align = max_t(u32, new_align, 2u << 20);
	else if (sz >= AMDGPU_VEGA_MEDIUM_BUFFER_SIZE)
		new_align = max_t(u32, new_align, 512u << 10);
	else
		return false;

	if (unlikely(!is_power_of_2(new_align)))
		return false;

	if (new_align <= *alignment)
		return false;

	*alignment    = new_align;
	*aligned_size = ALIGN(sz, new_align);
	return true;
}

static __always_inline unsigned int
amdgpu_vega_determine_optimal_prefetch(struct amdgpu_device *adev,
									   struct amdgpu_bo *bo,
									   unsigned int base_pages,
									   u32 _unused)
{
	enum vega_vram_pressure_state st;
	unsigned int pages;

	if (!is_hbm2_vega(adev) || !bo || !base_pages)
		return base_pages;

	pages = DIV_ROUND_UP(amdgpu_bo_size(bo), PAGE_SIZE);
	if (!pages)
		return base_pages;

	st = amdgpu_vega_get_vram_pressure_state(adev);

	/* cubic [1.0, 0.5, 0.06] scaling table */
	static const unsigned int pct[3] = { 100, 50, 6 };
	unsigned int factor = pct[st];

	if (!bo->preferred_domains ||
		bo->preferred_domains & AMDGPU_GEM_DOMAIN_VRAM) {
		unsigned int want = max(1u, (base_pages * factor) / 100);
	return min(want, pages);
		}

		/* GTT: conservative */
		return min_t(unsigned int, max(1u, base_pages / 2), pages);
}

static bool
amdgpu_vega_optimize_buffer_placement(struct amdgpu_device *adev,
									  struct amdgpu_bo     *bo,
									  u64                   size,
									  u64                   flags,
									  u32                  *domain)
{
	enum vega_vram_pressure_state st;
	bool tex, compute, cpu_acc;
	u32 cur_dom, new_dom;

	if (!is_hbm2_vega(adev) || !domain)
		return false;

	st = amdgpu_vega_get_vram_pressure_state(adev);
	tex      = is_vega_texture(flags);
	compute  = is_vega_compute(flags);
	cpu_acc  = is_vega_cpu_access(flags);

	cur_dom = *domain & (AMDGPU_GEM_DOMAIN_VRAM | AMDGPU_GEM_DOMAIN_GTT);
	new_dom = cur_dom;

	switch (st) {
		case VEGA_VRAM_RED:
			if (tex ||
				(compute && size > AMDGPU_VEGA_LARGE_BUFFER_SIZE))
				new_dom = AMDGPU_GEM_DOMAIN_VRAM;
			else
				new_dom = AMDGPU_GEM_DOMAIN_GTT;
		break;

		case VEGA_VRAM_YELLOW:
			if (tex || compute)
				new_dom = AMDGPU_GEM_DOMAIN_VRAM;
		else
			new_dom = AMDGPU_GEM_DOMAIN_GTT;
		break;

		case VEGA_VRAM_GREEN:
		default:
			if (cpu_acc && size < (256u << 10))
				new_dom = AMDGPU_GEM_DOMAIN_GTT;
		else
			new_dom = AMDGPU_GEM_DOMAIN_VRAM;
		break;
	}

	if (new_dom == cur_dom)
		return false;

	*domain &= ~(AMDGPU_GEM_DOMAIN_VRAM | AMDGPU_GEM_DOMAIN_GTT);
	*domain |= new_dom;

	if (new_dom == AMDGPU_GEM_DOMAIN_GTT && size > (1ULL << 20))
		pb_account_eviction();

	return true;
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
	s64 delta;

	if ((s64)timeout_ns < 0)
		return MAX_SCHEDULE_TIMEOUT;

	delta = (s64)(timeout_ns - KTIME_FAST_NS());
	if (delta <= 0)
		return 0;

	return min_t(unsigned long, nsecs_to_jiffies((u64)delta),
				 MAX_SCHEDULE_TIMEOUT - 1);
}

static const uint16_t pitch_mask_lut[5] = { 0, 255, 127, 63, 63 };

static inline int
amdgpu_gem_align_pitch(struct amdgpu_device *adev,
					   int width, int cpp, bool tiled)
{
	int mask;
	int aligned_width;

	if (unlikely(width <= 0 || cpp <= 0 || cpp > 4))
		return -EINVAL;

	mask = pitch_mask_lut[cpp];
	aligned_width = (width + mask) & ~mask;

	if (unlikely(aligned_width > S32_MAX / cpp))
		return -EINVAL;

	return aligned_width * cpp;
}

static vm_fault_t amdgpu_gem_fault(struct vm_fault *vmf)
{
	struct ttm_buffer_object *bo   = vmf->vma->vm_private_data;
	struct drm_device        *ddev;
	struct amdgpu_device     *adev;
	struct amdgpu_bo         *abo;
	vm_fault_t  ret;
	unsigned int prefetch_pages = TTM_BO_VM_NUM_PREFAULT;
	unsigned long page_off;
	u64 bytes_left;
	int idx;

	if (unlikely(!bo)) {
		return VM_FAULT_SIGBUS;
	}

	ddev = bo->base.dev;
	if (unlikely(!ddev)) {
		return VM_FAULT_SIGBUS;
	}

	/*
	 * Reserve the BO.  We always do this – even if the reservation has
	 * no active fences – because amdgpu_bo_fault_reserve_notify() and
	 * TTM expect it to be held.
	 */
	ret = ttm_bo_vm_reserve(bo, vmf);
	if (unlikely(ret)) {
		return ret;
	}

	/* Enter the real device; if it’s gone, serve dummy page. */
	if (!drm_dev_enter(ddev, &idx)) {
		ret = ttm_bo_vm_dummy_page(vmf, vmf->vma->vm_page_prot);
		goto out_unlock;
	}

	abo  = ttm_to_amdgpu_bo(bo);
	adev = drm_to_adev(ddev);

	/* ---------------- Prefetch window decision -------------------- */
	if (static_branch_unlikely(&vega_prefetch_key) && abo && adev) {
		/* Bring reservation line into M-state early (Intel guide). */
		prefetchw(bo->base.resv);

		page_off   = (vmf->address - vmf->vma->vm_start) >> PAGE_SHIFT;
		bytes_left = bo->base.size -
		((u64)page_off << PAGE_SHIFT);

		if ((abo->preferred_domains & AMDGPU_GEM_DOMAIN_VRAM) &&
			amdgpu_vega_get_vram_pressure_state(adev) == VEGA_VRAM_GREEN &&
			bytes_left <= (256ULL << PAGE_SHIFT)) {
			/* Map remainder in one go (≤1 MiB) */
			prefetch_pages = DIV_ROUND_UP(bytes_left, PAGE_SIZE);
			} else {
				prefetch_pages =
				amdgpu_vega_determine_optimal_prefetch(
					adev, abo, prefetch_pages, 0);

				/* Don’t prefetch beyond buffer end */
				prefetch_pages = min(prefetch_pages,
									 (unsigned int)
									 DIV_ROUND_UP(bytes_left, PAGE_SIZE));
			}
	}

	/* ---------------- Actual fault handling ----------------------- */
	ret = amdgpu_bo_fault_reserve_notify(bo);
	if (!ret) {
		ret = ttm_bo_vm_fault_reserved(vmf, vmf->vma->vm_page_prot,
									   prefetch_pages);
	}

	/* leave device context */
	drm_dev_exit(idx);

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
	struct amdgpu_bo *aobj = gem_to_amdgpu_bo(gobj);

	if (!aobj)
		return;

	if (tbo_cache_put(aobj))
		return;

	amdgpu_hmm_unregister(aobj);
	ttm_bo_put(&aobj->tbo);
}

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
	struct amdgpu_bo_param bp;
	int r;

	*obj = NULL;
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
	bp.size             = size;
	bp.byte_align       = alignment;
	bp.type             = type;
	bp.resv             = resv;
	bp.preferred_domain = initial_domain;
	bp.domain           = initial_domain;
	bp.flags            = flags;
	bp.bo_ptr_size      = sizeof(struct amdgpu_bo);
	bp.xcp_id_plus1     = xcp_id_plus1;

	if (static_branch_unlikely(&vega_domain_key)) {
		amdgpu_vega_optimize_buffer_placement(adev, NULL, size,
											  flags, &bp.domain);
	}

	if (ubo_slab) {
		ubo = kmem_cache_zalloc(ubo_slab,
								GFP_KERNEL | __GFP_NOWARN);
	}

	r = amdgpu_bo_create_user(adev, &bp, &ubo);
	if (r) {
		if (ubo && ubo_slab)
			kmem_cache_free(ubo_slab, ubo);
		return r;
	}

	bo   = &ubo->bo;
	*obj = &bo->tbo.base;
	return 0;
}

void amdgpu_gem_force_release(struct amdgpu_device *adev)
{
	struct drm_device *ddev = adev_to_drm(adev);
	struct drm_file *file;

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

		WARN_ONCE(1, "Force-releasing GEM objects for an active file.\n");

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
				dev_warn_once(ddev->dev, "OOM in force_release, leaking GEM object\n");
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

	abo = gem_to_amdgpu_bo(obj);
	adev = amdgpu_ttm_adev(abo->tbo.bdev);
	fpriv = file_priv->driver_priv;
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
		(!obj->import_attach ||
		!dma_buf_is_dynamic(obj->import_attach->dmabuf)))
		return 0;

	r = amdgpu_bo_reserve(abo, false);
	if (r)
		return r;

	bo_va = amdgpu_vm_bo_find(vm, abo);
	if (!bo_va) {
		bo_va = amdgpu_vm_bo_add(adev, vm, abo);
		if (IS_ERR(bo_va)) {
			r = PTR_ERR(bo_va);
			amdgpu_bo_unreserve(abo);
			return r;
		}
	} else {
		bo_va->ref_count++;
	}
	amdgpu_bo_unreserve(abo);

	if (!vm->is_compute_context || !vm->process_info ||
		!obj->import_attach ||
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
				u32 domain = AMDGPU_GEM_DOMAIN_GTT;
				if (is_vega_texture(abo->flags) || is_vega_compute(abo->flags)) {
					if (amdgpu_vega_get_vram_pressure_state(adev) != VEGA_VRAM_RED)
						domain = AMDGPU_GEM_DOMAIN_VRAM;
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
	fpriv = file_priv->driver_priv;
	if (unlikely(!bo || !fpriv))
		return;

	adev = amdgpu_ttm_adev(bo->tbo.bdev);
	if (unlikely(!adev))
		return;

	if (static_branch_unlikely(&vega_prefetch_key)) {
		prefetchw(bo->tbo.base.resv);
		prefetch(&fpriv->vm);
	}
	vm = &fpriv->vm;

	if (is_hbm2_vega(adev))
		use_async = (amdgpu_bo_size(bo) < (64ULL << 10)) ||
		(is_vega_compute(bo->flags) &&
		amdgpu_bo_size(bo) < (8ULL << 20));

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
	if (!bo_va || bo_va->ref_count == 0)
		goto out_unlock;

	if (--bo_va->ref_count > 0)
		goto out_unlock;

	amdgpu_vm_bo_del(adev, bo_va);
	amdgpu_vm_bo_update_shared(bo);

	if (!amdgpu_vm_ready(vm))
		goto out_unlock;

	r = amdgpu_vm_clear_freed(adev, vm, &fence);
	if (unlikely(r < 0)) {
		dev_err(adev->dev, "failed to clear page tables on GEM close (%ld)\n", r);
		goto out_unlock;
	}

	if (fence) {
		amdgpu_bo_fence(bo, fence, use_async);
		dma_fence_put(fence);
	}

	out_unlock:
	if (r)
		dev_err(adev->dev, "Error in GEM object close for pid %d (%ld)\n",
				task_pid_nr(current), r);
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
		AMDGPU_GEM_DOMAIN_GWS | AMDGPU_GEM_DOMAIN_OA))
		flags |= AMDGPU_GEM_CREATE_NO_CPU_ACCESS;

	if (flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) {
		r = amdgpu_bo_reserve(vm->root.bo, false);
		if (r)
			return r;
		resv = vm->root.bo->tbo.base.resv;
	}

	initial_domain = (u32)(args->in.domains);

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
		!(args->flags & AMDGPU_GEM_USERPTR_REGISTER))
		return -EACCES;

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
	int r = -1;

	DRM_DEBUG("%d\n", args->handle);
	gobj = drm_gem_object_lookup(filp, args->handle);
	if (!gobj)
		return -ENOENT;

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

static int amdgpu_gem_va_update_vm(struct amdgpu_device *adev,
								   struct amdgpu_vm *vm,
								   struct amdgpu_bo_va *bo_va,
								   uint32_t operation)
{
	int r;

	if (!amdgpu_vm_ready(vm))
		return 0;

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
	if (r)
		goto error;

	return 0;

	error:
	if (r && r != -ERESTARTSYS)
		DRM_ERROR("Couldn't update BO_VA (%d)\n", r);
	return r;
}

static bool bo_va_mapping_present(struct amdgpu_bo_va *bo_va,
								  uint64_t va, uint64_t size, uint64_t offset)
{
	const uint64_t first_pfn = va               >> AMDGPU_GPU_PAGE_SHIFT;
	const uint64_t last_pfn  = (va + size - 1) >> AMDGPU_GPU_PAGE_SHIFT;
	struct amdgpu_bo_va_mapping *m;

	list_for_each_entry(m, &bo_va->valids, list) {
		if (m->start  == first_pfn &&
			m->last   == last_pfn  &&
			m->offset == offset) {
			return true;        /* exact match already present */
			}
	}
	return false;
}

int amdgpu_gem_va_ioctl(struct drm_device *dev, void *data,
			struct drm_file *filp)
{
	struct amdgpu_device *adev  = drm_to_adev(dev);
	struct amdgpu_fpriv  *fpriv = filp->driver_priv;
	struct amdgpu_vm     *vm    = &fpriv->vm;
	struct drm_amdgpu_gem_va *args = data;

	struct drm_gem_object *gobj = NULL;
	struct amdgpu_bo      *abo  = NULL;
	struct amdgpu_bo_va   *bo_va = NULL;
	struct drm_exec exec;
	uint64_t vm_size, map_flags;
	int r = 0;

	/* ------------- constants for flag validation ------------------ */
	const u32 VALID = AMDGPU_VM_DELAY_UPDATE |
		AMDGPU_VM_PAGE_READABLE   | AMDGPU_VM_PAGE_WRITEABLE |
		AMDGPU_VM_PAGE_EXECUTABLE | AMDGPU_VM_MTYPE_MASK     |
		AMDGPU_VM_PAGE_NOALLOC;
	const u32 PRT   = AMDGPU_VM_DELAY_UPDATE | AMDGPU_VM_PAGE_PRT;

	const bool op_map   = args->operation == AMDGPU_VA_OP_MAP;
	const bool op_unmap = args->operation == AMDGPU_VA_OP_UNMAP;

	/* ------------- fast reject cluster --------------------------- */
	if (unlikely(!fpriv)) {
		return -EINVAL;
	}
	if (unlikely(args->operation > AMDGPU_VA_OP_REPLACE)) {
		return -EINVAL;
	}
	if (unlikely(args->va_address < AMDGPU_VA_RESERVED_BOTTOM)) {
		return -EINVAL;
	}
	if (unlikely(args->va_address >= AMDGPU_GMC_HOLE_START &&
		     args->va_address <  AMDGPU_GMC_HOLE_END)) {
		return -EINVAL;
	}
	if (unlikely((args->flags & ~VALID) && (args->flags & ~PRT))) {
		return -EINVAL;
	}

	args->va_address &= AMDGPU_GMC_HOLE_MASK;

	vm_size  = (adev->vm_manager.max_pfn * AMDGPU_GPU_PAGE_SIZE) -
		   AMDGPU_VA_RESERVED_TOP;
	if (unlikely(args->va_address + args->map_size > vm_size)) {
		return -EINVAL;
	}

	/* ------------- tiny MAP / UNMAP speculative path -------------- */
	if ((op_map || op_unmap) &&
	    args->map_size <= (96u << 10) &&
	    !(args->flags & AMDGPU_VM_PAGE_PRT)) {

		gobj = drm_gem_object_lookup(filp, args->handle);
		if (!gobj) {
			return -ENOENT;
		}
		abo = gem_to_amdgpu_bo(gobj);

		if (abo &&
		    abo->preferred_domains == AMDGPU_GEM_DOMAIN_GTT &&
		    dma_resv_trylock(vm->root.bo->tbo.base.resv)) {

			if (dma_resv_trylock(abo->tbo.base.resv)) {

				bo_va = amdgpu_vm_bo_find(vm, abo);
				if (!bo_va && op_map) {
					bo_va = amdgpu_vm_bo_add(adev, vm, abo);
				}

				if (!IS_ERR(bo_va)) {
					bool present = bo_va_mapping_present(
						bo_va, args->va_address,
						args->map_size,
						args->offset_in_bo);

					if (op_map) {
						if (!present) {
							map_flags =
							  amdgpu_gem_va_map_flags(
								adev, args->flags);
							r = amdgpu_vm_bo_map(
								adev, bo_va,
								args->va_address,
								args->offset_in_bo,
								args->map_size,
								map_flags);
						}
					} else { /* UNMAP */
						if (present) {
							r = amdgpu_vm_bo_unmap(
								adev, bo_va,
								args->va_address);
						}
					}
				} else {
					r = PTR_ERR(bo_va);
				}
				dma_resv_unlock(abo->tbo.base.resv);
			} else {
				r = -EAGAIN; /* BO contended */
			}
			dma_resv_unlock(vm->root.bo->tbo.base.resv);
		} else {
			r = -EAGAIN;     /* PD busy or wrong domain */
		}

		drm_gem_object_put(gobj);

		if (r != -EAGAIN) {
			return r;          /* success or fatal error */
		}

		/* contention → slow path */
		gobj = NULL;
		abo  = NULL;
	}

	/* ------------- canonical slow path via drm_exec --------------- */
	if ((args->operation != AMDGPU_VA_OP_CLEAR) &&
	    !(args->flags & AMDGPU_VM_PAGE_PRT)) {
		gobj = drm_gem_object_lookup(filp, args->handle);
		if (!gobj) {
			return -ENOENT;
		}
		abo = gem_to_amdgpu_bo(gobj);
	}

	drm_exec_init(&exec,
		      DRM_EXEC_INTERRUPTIBLE_WAIT | DRM_EXEC_IGNORE_DUPLICATES,
		      0);

	drm_exec_until_all_locked(&exec) {
		r = drm_exec_lock_obj(&exec, &vm->root.bo->tbo.base);
		drm_exec_retry_on_contention(&exec);
		if (r) {
			goto out_exec;
		}
		if (abo) {
			r = drm_exec_lock_obj(&exec, gobj);
			drm_exec_retry_on_contention(&exec);
			if (r) {
				goto out_exec;
			}
		}
	}

	bo_va = abo ? amdgpu_vm_bo_find(vm, abo) : fpriv->prt_va;
	if (!bo_va) {
		r = -ENOENT;
		goto out_exec;
	}

	switch (args->operation) {
	case AMDGPU_VA_OP_MAP:
		map_flags = amdgpu_gem_va_map_flags(adev, args->flags);
		r = amdgpu_vm_bo_map(adev, bo_va, args->va_address,
				     args->offset_in_bo, args->map_size,
				     map_flags);
		break;
	case AMDGPU_VA_OP_UNMAP:
		r = amdgpu_vm_bo_unmap(adev, bo_va, args->va_address);
		break;
	case AMDGPU_VA_OP_CLEAR:
		r = amdgpu_vm_bo_clear_mappings(adev, vm,
						args->va_address,
						args->map_size);
		break;
	case AMDGPU_VA_OP_REPLACE:
		map_flags = amdgpu_gem_va_map_flags(adev, args->flags);
		r = amdgpu_vm_bo_replace_map(adev, bo_va, args->va_address,
					     args->offset_in_bo,
					     args->map_size, map_flags);
		break;
	}

	if (!r &&
	    !(args->flags & AMDGPU_VM_DELAY_UPDATE) &&
	    !adev->debug_vm) {
		r = amdgpu_gem_va_update_vm(adev, vm, bo_va,
					    args->operation);
	}

out_exec:
	drm_exec_fini(&exec);
	if (gobj) {
		drm_gem_object_put(gobj);
	}
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
				for (base = robj->vm_bo; base; base = base->next) {
					if (amdgpu_xgmi_same_hive(amdgpu_ttm_adev(robj->tbo.bdev),
						amdgpu_ttm_adev(base->vm->root.bo->tbo.bdev))) {
						r = -EINVAL;
					amdgpu_bo_unreserve(robj);
					goto out;
						}
				}

				robj->preferred_domains = args->value & (AMDGPU_GEM_DOMAIN_VRAM |
				AMDGPU_GEM_DOMAIN_GTT |
				AMDGPU_GEM_DOMAIN_CPU);
				robj->allowed_domains = robj->preferred_domains;
				if (robj->allowed_domains == AMDGPU_GEM_DOMAIN_VRAM)
					robj->allowed_domains |= AMDGPU_GEM_DOMAIN_GTT;

		if (is_hbm2_vega(adev))
			amdgpu_vega_optimize_for_workload(adev, robj, robj->flags);

		if (robj->flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID)
			amdgpu_vm_bo_invalidate(robj, true);

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
	u32 domain, alignment;
	uint64_t size;
	int r;

	if (unlikely(args->width == 0 || args->height == 0 || args->bpp == 0))
		return -EINVAL;

	if (adev->mman.buffer_funcs_enabled)
		flags |= AMDGPU_GEM_CREATE_VRAM_CLEARED;

	args->pitch = amdgpu_gem_align_pitch(adev, args->width,
										 DIV_ROUND_UP(args->bpp, 8), 0);

	if (unlikely(__builtin_mul_overflow(args->pitch, args->height, &size))) {
		DRM_ERROR("Dumb buffer size calculation would overflow\n");
		return -EINVAL;
	}
	size = ALIGN(size, PAGE_SIZE);

	if (unlikely(size == 0)) {
		DRM_ERROR("Zero-sized dumb buffer: pitch=%u height=%u bpp=%u\n",
				  args->pitch, args->height, args->bpp);
		return -EINVAL;
	}

	domain = amdgpu_bo_get_preferred_domain(adev,
											amdgpu_display_supported_domains(adev, flags));

	alignment = 0;

	if (is_hbm2_vega(adev)) {
		amdgpu_vega_optimize_hbm2_bank_access(adev, NULL, &size,
											  &alignment);
	}
	if (alignment == 0)
		alignment = PAGE_SIZE;

	if (unlikely(size > U64_MAX - (alignment - 1))) {
		DRM_ERROR("Final size 0x%llx with alignment 0x%x would overflow\n",
				  size, alignment);
		return -EINVAL;
	}
	size = ALIGN(size, alignment);

	r = amdgpu_gem_object_create(adev, size, alignment, domain, flags,
								 ttm_bo_type_device, NULL, &gobj,
							  fpriv->xcp_id + 1);
	if (r) {
		DRM_DEBUG("Buffer object creation failed (%d)\n", r);
		return r;
	}

	args->size = size;

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
