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

#if defined(CONFIG_X86_TSC)
# define KTIME_FAST_NS()  ktime_get_mono_fast_ns()
#else
# define KTIME_FAST_NS()  ktime_get_ns()
#endif

/*
 * Per-CPU cache of the most recently observed VRAM-usage percentage.
 * The values are only advisory; a maximum age of 2 ms is enforced.
 */
struct vega_vram_cache_pc {
	u32 pct;      /* cached VRAM usage in % (0-100)  */
	u64 ns_last;  /* timestamp (ns) of last refresh  */
} __aligned(L1_CACHE_BYTES);

static DEFINE_PER_CPU(struct vega_vram_cache_pc, vram_cache_pc);

static inline void vega_force_gtt(uint32_t *domain)
{
	*domain &= ~AMDGPU_GEM_DOMAIN_VRAM;
	*domain |=  AMDGPU_GEM_DOMAIN_GTT;
}

#define VEGA10_HBM2_GPU_PAGE_SIZE (256 * 1024)
DEFINE_STATIC_KEY_FALSE(vega_bankalign_key);
DEFINE_STATIC_KEY_FALSE(vega_prefetch_key);
DEFINE_STATIC_KEY_FALSE(vega_domain_key);

DEFINE_STATIC_KEY_FALSE(amdgpu_vm_always_valid_key);

static inline void amdgpu_vm_always_valid_key_enable(void)
{
	static atomic_t initialized = ATOMIC_INIT(0);

	/* Ensure the static branch is enabled only once with atomic operations. */
	if (atomic_cmpxchg(&initialized, 0, 1) == 0) {
		static_branch_enable(&amdgpu_vm_always_valid_key);
	}
}

#define vega_hbm2_key vega_domain_key

static inline void
amdgpu_gem_static_branch_init(struct amdgpu_device *adev)
{
	if (adev && adev->asic_type == CHIP_VEGA10) {
		static_branch_enable(&vega_bankalign_key);
		static_branch_enable(&vega_prefetch_key);
		static_branch_enable(&vega_domain_key);
	}
}

#define TBO_MAX_BYTES   (64u << 10)
#define TBO_CACHE_DEPTH 16
#define TBO_CACHEABLE_USER_FLAGS (AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED | \
AMDGPU_GEM_CREATE_CPU_GTT_USWC)

struct tiny_bo_cache {
	struct amdgpu_bo *slot[TBO_CACHE_DEPTH];
	u8                top;
} __aligned(L1_CACHE_BYTES);

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
		/* Somebody else installed it first */
		kmem_cache_destroy(s);
	} else {
		/* Slab is live → the tiny-BO cache can be considered ready */
		static_branch_enable(&tbo_cache_key);
	}
}

static struct amdgpu_bo *
tbo_cache_try_get(unsigned long size, u64 user_flags,
				  u32 domain, struct dma_resv *resv, int align)
{
	struct tiny_bo_cache *c;
	struct amdgpu_bo *bo = NULL;
	unsigned int cpu;

	if (unlikely(!static_branch_unlikely(&tbo_cache_key))           ||
		unlikely(size                         > TBO_MAX_BYTES)      ||
		unlikely(domain                       != AMDGPU_GEM_DOMAIN_GTT) ||
		unlikely((user_flags & ~TBO_CACHEABLE_USER_FLAGS) != 0)     ||
		unlikely(resv != NULL)                                      ||
		unlikely(align                        > PAGE_SIZE))
		return NULL;

	cpu = get_cpu();				/* pin task to CPU   */
	c   = per_cpu_ptr(&tiny_bo_cache, cpu);

	if (likely(c->top)) {
		bo = c->slot[--c->top];
		prefetch(bo);
	}
	put_cpu();

	return bo;
}

static bool tbo_cache_put(struct amdgpu_bo *bo)
{
	struct tiny_bo_cache *c;
	u64 flags;
	unsigned int cpu;

	if (unlikely(!bo) ||
		unlikely(!static_branch_unlikely(&tbo_cache_key))          ||
		unlikely(bo->tbo.base.size            > TBO_MAX_BYTES)     ||
		unlikely(bo->preferred_domains        != AMDGPU_GEM_DOMAIN_GTT) ||
		unlikely((bo->tbo.page_alignment << PAGE_SHIFT) > PAGE_SIZE))
		return false;

	flags = bo->flags & ~AMDGPU_GEM_CREATE_VRAM_WIPE_ON_RELEASE;
	if (unlikely(flags & ~TBO_CACHEABLE_USER_FLAGS))
		return false;

	cpu = get_cpu();
	c   = per_cpu_ptr(&tiny_bo_cache, cpu);

	if (unlikely(c->top >= TBO_CACHE_DEPTH)) {
		put_cpu();
		return false;
	}

	/* resurrect: cache now owns the single reference */
	kref_init(&bo->tbo.base.refcount);
	c->slot[c->top++] = bo;
	put_cpu();
	return true;
}

#define AMDGPU_VEGA_HBM2_BANK_SIZE       (1ULL * 1024 * 1024)
#define AMDGPU_VEGA_SMALL_BUFFER_SIZE    (1ULL * 1024 * 1024)
#define AMDGPU_VEGA_MEDIUM_BUFFER_SIZE   (4ULL * 1024 * 1024)
#define AMDGPU_VEGA_LARGE_BUFFER_SIZE    (16ULL * 1024 * 1024)
#define AMDGPU_VEGA_HBM2_MIN_ALIGNMENT   (256 * 1024)

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

static __always_inline bool is_vega_texture(uint64_t flags)
{ return flags & AMDGPU_GEM_CREATE_VRAM_CONTIGUOUS; }

static __always_inline bool is_vega_compute(uint64_t flags)
{ return flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS; }

static __always_inline bool is_vega_cpu_access(uint64_t flags)
{ return flags & AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED; }

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

static uint32_t __amdgpu_vega_get_vram_usage(struct amdgpu_device *adev)
{
	struct ttm_resource_manager *vram_man;
	uint64_t vram_usage, vram_size;
	uint32_t pct = 0;

	if (unlikely(!adev))
		return 0;

	vram_man = ttm_manager_type(&adev->mman.bdev, TTM_PL_VRAM);
	if (unlikely(!vram_man))
		return 0;

	vram_usage = ttm_resource_manager_usage(vram_man);
	vram_size  = READ_ONCE(adev->gmc.mc_vram_size);

	if (likely(vram_size)) {
		/*
		 * Fast path if the product fits into 128 bit and thus the
		 * high-precision shortcut used by div64_u64() is available.
		 */
		if (likely(vram_usage <= 0x1999999999999999ULL)) {
			pct = div64_u64(vram_usage * 100, vram_size);
		} else {
			uint64_t divisor = div64_u64(vram_size, 100);
			pct = div64_u64(vram_usage, max_t(uint64_t, divisor, 1));
		}
	}

	return min(pct, 100u);
}

static __always_inline u32
amdgpu_vega_get_vram_usage_cached(struct amdgpu_device *adev)
{
	const s64 max_age_ns = 2 * NSEC_PER_MSEC;
	unsigned int cpu;
	struct vega_vram_cache_pc *c;
	u64 now;
	u32 pct;

	if (unlikely(!adev))
		return 0;

	cpu = get_cpu();
	c   = per_cpu_ptr(&vram_cache_pc, cpu);
	now = KTIME_FAST_NS();

	if (likely(now - c->ns_last < max_age_ns)) {
		pct = READ_ONCE(c->pct);
		put_cpu();
		return pct;
	}
	put_cpu();

	/* slow-path: refresh */
	pct = __amdgpu_vega_get_vram_usage(adev);

	cpu = get_cpu();
	c   = per_cpu_ptr(&vram_cache_pc, cpu);
	smp_store_release(&c->pct, pct); /* publish value */
	WRITE_ONCE(c->ns_last, now);     /* then timestamp */
	put_cpu();

	return pct;
}

#define ALIGN_POW2(x, a)	(((x) + ((a) - 1)) & ~((typeof(x))(a) - 1))

#ifndef PREFETCH_READ
# define PREFETCH_READ(p)  prefetch(p)
# define PREFETCH_WRITE(p) prefetchw(p)
#endif

unsigned long amdgpu_gem_timeout(uint64_t timeout_ns)
{
	if ((s64)timeout_ns < 0)
		return MAX_SCHEDULE_TIMEOUT;

	/* Monotonic fast clock – see helper macro above */
	u64 now_ns = KTIME_FAST_NS();
	s64 delta  = (s64)(timeout_ns - now_ns);

	if (delta <= 0)
		return 0;

	/* Guaranteed positive, so the cast is safe */
	unsigned long j = nsecs_to_jiffies((u64)delta);

	if (j >= MAX_SCHEDULE_TIMEOUT)
		j = MAX_SCHEDULE_TIMEOUT - 1;

	return j;
}

static const uint16_t pitch_mask_lut[5] = { 0, 255, 127, 63, 63 };

static inline int
amdgpu_gem_align_pitch(struct amdgpu_device *adev,
					   int width, int cpp, bool tiled)
{
	int mask    = (cpp <= 4) ? pitch_mask_lut[cpp] : 0;
	int aligned = (width + mask) & ~mask;

	return aligned * cpp;
}

static inline uint32_t
amdgpu_vega_get_efficient_usage(struct amdgpu_device *adev)
{
	return amdgpu_vega_get_vram_usage_cached(adev);
}

static uint32_t
amdgpu_vega_get_effective_vram_usage(struct amdgpu_device *adev)
{
	struct ttm_resource_manager *vram_man;
	uint32_t usage_percent, effective_percent;

	if (unlikely(!adev)) {
		return 0;
	}

	usage_percent = amdgpu_vega_get_vram_usage_cached(adev);
	effective_percent = usage_percent;

	if (unlikely(!is_hbm2_vega(adev))) {
		return usage_percent;
	}

	vram_man = ttm_manager_type(&adev->mman.bdev, TTM_PL_VRAM);
	if (vram_man && vram_man->use_tt) {
		effective_percent = min_t(uint32_t, usage_percent + 10, 100u);
	} else if (usage_percent > amdgpu_vega_vram_pressure_mid) {
		effective_percent = min_t(uint32_t, usage_percent + 5, 100u);
	}

	return effective_percent;
}

static bool
amdgpu_vega_optimize_buffer_placement(struct amdgpu_device *adev,
									  u64 size, u64 flags, u32 *domain)
{
	u32 orig = *domain;
	u32 pressure;

	if (unlikely(!is_hbm2_vega(adev) || !domain))
		return false;

	/* CPU-visible buffers → force GTT */
	if (is_vega_cpu_access(flags)) {
		vega_force_gtt(domain);
		return *domain != orig;
	}

	/* Only GPU-only buffers below are considered                            */
	if (!(is_vega_texture(flags) || is_vega_compute(flags))) {
		return false;
	}

	pressure = amdgpu_vega_get_vram_usage_cached(adev);

	if (pressure > amdgpu_vega_vram_pressure_mid ||
		(size > (64ULL << 20) &&
		pressure > amdgpu_vega_vram_pressure_low)) {
		if (size <= AMDGPU_VEGA_MEDIUM_BUFFER_SIZE &&
			pressure < amdgpu_vega_vram_pressure_high) {
			*domain = (*domain & ~AMDGPU_GEM_DOMAIN_GTT) |
			AMDGPU_GEM_DOMAIN_VRAM;
			} else {
				vega_force_gtt(domain);
			}
		} else {
			*domain = (*domain & ~AMDGPU_GEM_DOMAIN_GTT) |
			AMDGPU_GEM_DOMAIN_VRAM;
		}

		return *domain != orig;
}

static __cold bool
amdgpu_vega_optimize_hbm2_bank_access(struct amdgpu_device *adev,
									  struct amdgpu_bo     *__maybe_unused bo,
									  uint64_t             *aligned_size,
									  uint32_t             *alignment)
{
	uint32_t new_align;
	uint64_t size, aligned_size_new;

	if (unlikely(!adev || !aligned_size || !alignment))
		return false;

	if (unlikely(!is_hbm2_vega(adev) ||
		!static_branch_unlikely(&vega_bankalign_key) ||
		!*aligned_size))
		return false;

	size = *aligned_size;
	new_align = *alignment;

	if (size >= (128ULL << 20))
		new_align = max_t(uint32_t, new_align, 2u << 20);
	else if (size >= AMDGPU_VEGA_MEDIUM_BUFFER_SIZE)
		new_align = max_t(uint32_t, new_align, 256u << 10);
	else
		return false;

	if (unlikely(new_align == 0 || !is_power_of_2(new_align))) {
		DRM_ERROR("Invalid alignment %u in HBM2 optimization\n",
				  new_align);
		return false;
	}

	if (new_align > *alignment) {
		aligned_size_new = ALIGN(size, new_align);

		if (unlikely(aligned_size_new < size)) {
			DRM_ERROR("ALIGN resulted in smaller size: %llu -> %llu\n",
					  size, aligned_size_new);
			return false;
		}

		*alignment = new_align;
		*aligned_size = aligned_size_new;
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
	uint64_t size;
	unsigned int pages;

	if (unlikely(!is_hbm2_vega(adev) || !bo))
		return base_prefetch_pages;

	size = amdgpu_bo_size(bo);
	if (unlikely(!size))
		return base_prefetch_pages;

	pages = DIV_ROUND_UP(size, PAGE_SIZE);
	if (!pages)
		return base_prefetch_pages;

	/* imported PRIME BOs: treat preferred_domains==0 as VRAM */
	if (!bo->preferred_domains ||
		(bo->preferred_domains & AMDGPU_GEM_DOMAIN_VRAM)) {
		/* VRAM target: aggressive unless pressure is extreme */
		if (vram_usage < amdgpu_vega_vram_pressure_high) {
			return min_t(unsigned int,
						 min_t(unsigned int,
							   base_prefetch_pages * 2U, 128U),
				pages);
		} else {
			return min_t(unsigned int,
						 min_t(unsigned int,
							   base_prefetch_pages, 32U),
				pages);
		}
		}

		/* GTT target: conservative to protect PCIe bus */
		return max_t(unsigned int, 1U,
					 min_t(unsigned int,
						   min_t(unsigned int,
								 base_prefetch_pages / 2U, 16U),
			pages));
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

static vm_fault_t amdgpu_gem_fault(struct vm_fault *vmf)
{
	struct ttm_buffer_object *bo = vmf->vma->vm_private_data;
	struct drm_device *ddev;
	vm_fault_t ret;
	int idx;

	if (unlikely(!bo)) {
		return VM_FAULT_SIGBUS;
	}

	ddev = bo->base.dev;
	if (unlikely(!ddev)) {
		return VM_FAULT_SIGBUS;
	}

	ret = ttm_bo_vm_reserve(bo, vmf);
	if (unlikely(ret)) {
		return ret;
	}

	if (likely(drm_dev_enter(ddev, &idx))) {
		struct amdgpu_device *adev = drm_to_adev(ddev);
		unsigned int prefetch_pages = TTM_BO_VM_NUM_PREFAULT;

		ret = amdgpu_bo_fault_reserve_notify(bo);
		if (unlikely(ret)) {
			drm_dev_exit(idx);
			goto unlock_resv;
		}

		if (static_branch_unlikely(&vega_prefetch_key)) {
			struct amdgpu_bo *abo = ttm_to_amdgpu_bo(bo);

			if (likely(abo)) {
				uint32_t usage;

				prefetch(abo);
				if (likely(adev))
					prefetch(&adev->mman);

				usage = amdgpu_vega_get_vram_usage_cached(adev);
				prefetch_pages =
				amdgpu_vega_determine_optimal_prefetch(
					adev, abo, prefetch_pages, usage);
			}
		}

		ret = ttm_bo_vm_fault_reserved(vmf, vmf->vma->vm_page_prot,
									   prefetch_pages);
		drm_dev_exit(idx);
	} else {
		ret = ttm_bo_vm_dummy_page(vmf, vmf->vma->vm_page_prot);
	}

	/* On VM_FAULT_RETRY, the mm core expects the reservation to be kept. */
	if (ret == VM_FAULT_RETRY && !(vmf->flags & FAULT_FLAG_RETRY_NOWAIT)) {
		return ret;
	}

	unlock_resv:
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

	/* If per-CPU cache accepts it, keep the extra ref inside cache */
	if (tbo_cache_put(aobj))
		return;

	/* Otherwise destroy for real */
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
	int r;

	if (WARN_ON(!adev || !obj)) {
		return -EINVAL;
	}

	*obj = NULL;
	amdgpu_tbo_slab_ensure();

	if (flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) {
		amdgpu_vm_always_valid_key_enable();
	}

	bo = tbo_cache_try_get(size, flags, initial_domain, resv, alignment);
	if (bo) {
		*obj = &bo->tbo.base;
		return 0;
	}

	{
		struct amdgpu_bo_param bp = {
			.size             = size,
			.byte_align       = alignment,
			.type             = type,
			.resv             = resv,
			.preferred_domain = initial_domain,
			.flags            = flags,
			.domain           = initial_domain,
			.bo_ptr_size      = sizeof(struct amdgpu_bo),
			.xcp_id_plus1     = xcp_id_plus1,
		};

		if (static_branch_unlikely(&vega_domain_key)) {
			amdgpu_vega_optimize_buffer_placement(adev, size,
												  flags, &bp.domain);
		}

		if (ubo_slab) {
			ubo = kmem_cache_zalloc(ubo_slab,
									GFP_KERNEL | __GFP_NOWARN);
		}

		r = amdgpu_bo_create_user(adev, &bp, &ubo);
		if (r) {
			if (ubo && ubo_slab) {
				kmem_cache_free(ubo_slab, ubo);
			}
			return r;
		}
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

static int amdgpu_gem_object_open(struct drm_gem_object *obj,
								  struct drm_file *file_priv)
{
	struct amdgpu_bo       *abo;
	struct amdgpu_fpriv    *fpriv;
	struct amdgpu_device   *adev;
	struct amdgpu_vm       *vm;
	struct amdgpu_bo_va    *bo_va;
	struct mm_struct       *mm;
	int                     r;

	if (unlikely(!obj || !file_priv))
		return -EINVAL;

	abo   = gem_to_amdgpu_bo(obj);
	fpriv = file_priv->driver_priv;
	if (unlikely(!abo || !fpriv))
		return -EINVAL;

	adev = amdgpu_ttm_adev(abo->tbo.bdev);
	if (unlikely(!adev))
		return -EINVAL;

	vm = &fpriv->vm;

	/* ------------------------------------------------------------------
	 * Fast-exit paths for “always-valid” BOs
	 * ------------------------------------------------------------------
	 */
	if ((abo->flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) &&
		!amdgpu_vm_is_bo_always_valid(vm, abo))
		return -EPERM;

	if (!vm->is_compute_context &&
		static_branch_likely(&amdgpu_vm_always_valid_key) &&
		(abo->flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID) &&
		(abo->allowed_domains == AMDGPU_GEM_DOMAIN_GTT) &&
		!abo->parent &&
		(!obj->import_attach ||
		!dma_buf_is_dynamic(obj->import_attach->dmabuf)))
		return 0;

	/* ------------------------------------------------------------------
	 * Sanity-check current->mm for userptr BOs
	 * ------------------------------------------------------------------
	 */
	rcu_read_lock();
	mm = amdgpu_ttm_tt_get_usermm(abo->tbo.ttm);
	if (mm) {
		if (!mmget_not_zero(mm)) {
			rcu_read_unlock();
			return -EPERM;
		}

		if (mm != current->mm) {
			mmput(mm);
			rcu_read_unlock();
			return -EPERM;
		}
		mmput(mm);
	}
	rcu_read_unlock();

	/* ------------------------------------------------------------------
	 * Attach BO to VM
	 * ------------------------------------------------------------------
	 */
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

	/* ------------------------------------------------------------------
	 * kFD eviction-fence handling (GPUVM compute contexts)
	 * ------------------------------------------------------------------
	 */
	if (!vm->is_compute_context || !vm->process_info)
		return 0;

	if (!obj->import_attach ||
		!dma_buf_is_dynamic(obj->import_attach->dmabuf))
		return 0;

	mutex_lock_nested(&vm->process_info->lock, 1);

	if (vm->process_info->eviction_fence &&
		!(static_branch_likely(&amdgpu_vm_always_valid_key) &&
		(abo->flags & AMDGPU_GEM_CREATE_VM_ALWAYS_VALID))) {

		uint32_t domain = AMDGPU_GEM_DOMAIN_GTT;

	if (is_hbm2_vega(adev)) {
		if (is_vega_texture(abo->flags) ||
			is_vega_compute(abo->flags))
			domain = AMDGPU_GEM_DOMAIN_VRAM;

		if (amdgpu_vega_get_effective_vram_usage(adev) >
			amdgpu_vega_vram_pressure_high)
			domain = AMDGPU_GEM_DOMAIN_GTT;
	}

	r = amdgpu_amdkfd_bo_validate_and_fence(
		abo, domain,
		&vm->process_info->eviction_fence->base);
		}

		mutex_unlock(&vm->process_info->lock);
		return r;
}

static void amdgpu_gem_object_close(struct drm_gem_object *obj,
									struct drm_file *file_priv)
{
	struct amdgpu_bo      *bo;
	struct amdgpu_device  *adev;
	struct amdgpu_fpriv   *fpriv;
	struct amdgpu_vm      *vm;
	struct dma_fence      *fence = NULL;
	struct amdgpu_bo_va   *bo_va;
	struct drm_exec        exec;
	long                   r = 0;
	bool                   use_async = false;

	if (unlikely(!obj || !file_priv))
		return;

	bo     = gem_to_amdgpu_bo(obj);
	fpriv  = file_priv->driver_priv;
	if (unlikely(!bo || !fpriv))
		return;

	adev = amdgpu_ttm_adev(bo->tbo.bdev);
	if (unlikely(!adev))
		return;

	if (static_branch_unlikely(&vega_prefetch_key)) {
		PREFETCH_WRITE(bo->tbo.base.resv);
		PREFETCH_READ(&fpriv->vm);
	}

	vm = &fpriv->vm;

	if (is_hbm2_vega(adev))
		use_async = amdgpu_vega_should_use_async_fence(adev, bo, bo->flags);

	drm_exec_init(&exec, DRM_EXEC_IGNORE_DUPLICATES, 0);

	/* ------------------------------------------------------------------
	 * Lock BO and its VM page-directory
	 * ------------------------------------------------------------------
	 */
	drm_exec_until_all_locked(&exec) {
		r = drm_exec_prepare_obj(&exec, &bo->tbo.base, 1);
		drm_exec_retry_on_contention(&exec);
		if (r)
			goto out_unlock;

		r = amdgpu_vm_lock_pd(vm, &exec, 0);
		drm_exec_retry_on_contention(&exec);
		if (r)
			goto out_unlock;
	}

	/* ------------------------------------------------------------------
	 * Remove BO-VA reference
	 * ------------------------------------------------------------------
	 */
	bo_va = amdgpu_vm_bo_find(vm, bo);
	if (!bo_va)			/* Nothing to do */
		goto out_unlock;

	if (WARN_ON(!bo_va->ref_count)) {
		dev_warn(adev->dev,
				 "Attempted to close BO with zero ref_count\n");
		goto out_unlock;
	}

	if (--bo_va->ref_count)		/* Still referenced elsewhere */
		goto out_unlock;

	amdgpu_vm_bo_del(adev, bo_va);
	amdgpu_vm_bo_update_shared(bo);

	if (!amdgpu_vm_ready(vm))
		goto out_unlock;

	r = amdgpu_vm_clear_freed(adev, vm, &fence);
	if (r < 0) {
		dev_err(adev->dev,
				"failed to clear page tables on GEM close (%ld)\n",
				r);
		goto out_unlock;
	}

	/* amdgpu_vm_clear_freed() may legitimately return r>0 w/ fence */
	if (!fence)
		goto out_unlock;

	amdgpu_bo_fence(bo, fence, use_async);

	out_unlock:
	dma_fence_put(fence);
	if (r)
		dev_err(adev->dev,
				"Error in GEM object close for pid %d (%ld)\n",
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
	struct amdgpu_bo      *robj;
	int                    r = 0;

	gobj = drm_gem_object_lookup(filp, handle);
	if (!gobj)
		return -ENOENT;

	robj = gem_to_amdgpu_bo(gobj);

	if (amdgpu_ttm_tt_get_usermm(robj->tbo.ttm) ||
		(robj->flags & AMDGPU_GEM_CREATE_NO_CPU_ACCESS)) {
		r = -EPERM;
	goto out_put;
		}

		*offset_p = amdgpu_bo_mmap_offset(robj);

		out_put:
		drm_gem_object_put(gobj);
		return r;
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

static __cold int amdgpu_gem_va_update_vm(struct amdgpu_device *adev,
										  struct amdgpu_vm *vm,
										  struct amdgpu_bo_va *bo_va,
										  uint32_t operation)
{
	int r;

	if (!amdgpu_vm_ready(vm)) {
		return 0;
	}

	r = amdgpu_vm_clear_freed(adev, vm, NULL);
	if (r) {
		goto error;
	}

	if (operation == AMDGPU_VA_OP_MAP ||
		operation == AMDGPU_VA_OP_REPLACE) {
		r = amdgpu_vm_bo_update(adev, bo_va, false);
	if (r) {
		goto error;
	}
		}

		r = amdgpu_vm_update_pdes(adev, vm, false);
		if (r) {
			goto error;
		}

		return 0;

		error:
		if (r && r != -ERESTARTSYS) {
			DRM_ERROR("Couldn't update BO_VA (%d)\n", r);
		}
		return r;
}

int amdgpu_gem_va_ioctl(struct drm_device *dev, void *data,
						struct drm_file *filp)
{
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

	if (unlikely(!fpriv)) {
		return -EINVAL;
	}

	if (static_branch_unlikely(&vega_prefetch_key)) {
		prefetch(fpriv);
		prefetch(&fpriv->vm);
		prefetch(args);
	}

	if (unlikely(args->va_address < AMDGPU_VA_RESERVED_BOTTOM)) {
		dev_dbg(dev->dev,
				"va_address 0x%llx is in reserved area 0x%llx\n",
		  args->va_address, AMDGPU_VA_RESERVED_BOTTOM);
		return -EINVAL;
	}

	if (unlikely(args->va_address >= AMDGPU_GMC_HOLE_START &&
		args->va_address < AMDGPU_GMC_HOLE_END)) {
		dev_dbg(dev->dev,
				"va_address 0x%llx is in VA hole 0x%llx-0x%llx\n",
		  args->va_address, AMDGPU_GMC_HOLE_START,
		  AMDGPU_GMC_HOLE_END);
		return -EINVAL;
		}

		args->va_address &= AMDGPU_GMC_HOLE_MASK;

	vm_size = adev->vm_manager.max_pfn * AMDGPU_GPU_PAGE_SIZE;
	vm_size -= AMDGPU_VA_RESERVED_TOP;
	if (unlikely(args->va_address + args->map_size > vm_size)) {
		dev_dbg(dev->dev,
				"va_address 0x%llx is in top reserved area 0x%llx\n",
		  args->va_address + args->map_size, vm_size);
		return -EINVAL;
	}

	if (unlikely((args->flags & ~valid_flags) && (args->flags & ~prt_flags))) {
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
				if (unlikely(r)) {
					goto error;
				}
			}
			r = amdgpu_vm_lock_pd(&fpriv->vm, &exec, 2);
			drm_exec_retry_on_contention(&exec);
			if (unlikely(r)) {
				goto error;
			}
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
		}

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
											 args->offset_in_bo, args->map_size,
								 va_flags);
				break;
		}

		/* Propagate errors from the critical VM update path to the caller. */
		if (!r && !(args->flags & AMDGPU_VM_DELAY_UPDATE) && !adev->debug_vm) {
			int update_r;

			update_r = amdgpu_gem_va_update_vm(adev, &fpriv->vm, bo_va,
											   args->operation);
			if (update_r) {
				r = update_r;
			}
		}

		error:
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
