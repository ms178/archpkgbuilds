/*
 * Copyright 2017 Red Hat
 * Parts ported from amdgpu (fence wait code).
 * Copyright 2016 Advanced Micro Devices, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 *
 * Authors:
 *
 */

/**
 * DOC: Overview
 *
 * DRM synchronisation objects (syncobj, see struct &drm_syncobj) provide a
 * container for a synchronization primitive which can be used by userspace
 * to explicitly synchronize GPU commands, can be shared between userspace
 * processes, and can be shared between different DRM drivers.
 * Their primary use-case is to implement Vulkan fences and semaphores.
 * The syncobj userspace API provides ioctls for several operations:
 *
 *  - Creation and destruction of syncobjs
 *  - Import and export of syncobjs to/from a syncobj file descriptor
 *  - Import and export a syncobj's underlying fence to/from a sync file
 *  - Reset a syncobj (set its fence to NULL)
 *  - Signal a syncobj (set a trivially signaled fence)
 *  - Wait for a syncobj's fence to appear and be signaled
 *
 * The syncobj userspace API also provides operations to manipulate a syncobj
 * in terms of a timeline of struct &dma_fence_chain rather than a single
 * struct &dma_fence, through the following operations:
 *
 *   - Signal a given point on the timeline
 *   - Wait for a given point to appear and/or be signaled
 *   - Import and export from/to a given point of a timeline
 *
 * At it's core, a syncobj is simply a wrapper around a pointer to a struct
 * &dma_fence which may be NULL.
 * When a syncobj is first created, its pointer is either NULL or a pointer
 * to an already signaled fence depending on whether the
 * &DRM_SYNCOBJ_CREATE_SIGNALED flag is passed to
 * &DRM_IOCTL_SYNCOBJ_CREATE.
 *
 * If the syncobj is considered as a binary (its state is either signaled or
 * unsignaled) primitive, when GPU work is enqueued in a DRM driver to signal
 * the syncobj, the syncobj's fence is replaced with a fence which will be
 * signaled by the completion of that work.
 * If the syncobj is considered as a timeline primitive, when GPU work is
 * enqueued in a DRM driver to signal the a given point of the syncobj, a new
 * struct &dma_fence_chain pointing to the DRM driver's fence and also
 * pointing to the previous fence that was in the syncobj. The new struct
 * &dma_fence_chain fence replace the syncobj's fence and will be signaled by
 * completion of the DRM driver's work and also any work associated with the
 * fence previously in the syncobj.
 *
 * When GPU work which waits on a syncobj is enqueued in a DRM driver, at the
 * time the work is enqueued, it waits on the syncobj's fence before
 * submitting the work to hardware. That fence is either :
 *
 *    - The syncobj's current fence if the syncobj is considered as a binary
 *      primitive.
 *    - The struct &dma_fence associated with a given point if the syncobj is
 *      considered as a timeline primitive.
 *
 * If the syncobj's fence is NULL or not present in the syncobj's timeline,
 * the enqueue operation is expected to fail.
 *
 * With binary syncobj, all manipulation of the syncobjs's fence happens in
 * terms of the current fence at the time the ioctl is called by userspace
 * regardless of whether that operation is an immediate host-side operation
 * (signal or reset) or or an operation which is enqueued in some driver
 * queue. &DRM_IOCTL_SYNCOBJ_RESET and &DRM_IOCTL_SYNCOBJ_SIGNAL can be used
 * to manipulate a syncobj from the host by resetting its pointer to NULL or
 * setting its pointer to a fence which is already signaled.
 *
 * With a timeline syncobj, all manipulation of the synobj's fence happens in
 * terms of a u64 value referring to point in the timeline. See
 * dma_fence_chain_find_seqno() to see how a given point is found in the
 * timeline.
 *
 * Note that applications should be careful to always use timeline set of
 * ioctl() when dealing with syncobj considered as timeline. Using a binary
 * set of ioctl() with a syncobj considered as timeline could result incorrect
 * synchronization. The use of binary syncobj is supported through the
 * timeline set of ioctl() by using a point value of 0, this will reproduce
 * the behavior of the binary set of ioctl() (for example replace the
 * syncobj's fence when signaling).
 *
 *
 * Host-side wait on syncobjs
 * --------------------------
 *
 * &DRM_IOCTL_SYNCOBJ_WAIT takes an array of syncobj handles and does a
 * host-side wait on all of the syncobj fences simultaneously.
 * If &DRM_SYNCOBJ_WAIT_FLAGS_WAIT_ALL is set, the wait ioctl will wait on
 * all of the syncobj fences to be signaled before it returns.
 * Otherwise, it returns once at least one syncobj fence has been signaled
 * and the index of a signaled fence is written back to the client.
 *
 * Unlike the enqueued GPU work dependencies which fail if they see a NULL
 * fence in a syncobj, if &DRM_SYNCOBJ_WAIT_FLAGS_WAIT_FOR_SUBMIT is set,
 * the host-side wait will first wait for the syncobj to receive a non-NULL
 * fence and then wait on that fence.
 * If &DRM_SYNCOBJ_WAIT_FLAGS_WAIT_FOR_SUBMIT is not set and any one of the
 * syncobjs in the array has a NULL fence, -EINVAL will be returned.
 * Assuming the syncobj starts off with a NULL fence, this allows a client
 * to do a host wait in one thread (or process) which waits on GPU work
 * submitted in another thread (or process) without having to manually
 * synchronize between the two.
 * This requirement is inherited from the Vulkan fence API.
 *
 * If &DRM_SYNCOBJ_WAIT_FLAGS_WAIT_DEADLINE is set, the ioctl will also set
 * a fence deadline hint on the backing fences before waiting, to provide the
 * fence signaler with an appropriate sense of urgency.  The deadline is
 * specified as an absolute &CLOCK_MONOTONIC value in units of ns.
 *
 * Similarly, &DRM_IOCTL_SYNCOBJ_TIMELINE_WAIT takes an array of syncobj
 * handles as well as an array of u64 points and does a host-side wait on all
 * of syncobj fences at the given points simultaneously.
 *
 * &DRM_IOCTL_SYNCOBJ_TIMELINE_WAIT also adds the ability to wait for a given
 * fence to materialize on the timeline without waiting for the fence to be
 * signaled by using the &DRM_SYNCOBJ_WAIT_FLAGS_WAIT_AVAILABLE flag. This
 * requirement is inherited from the wait-before-signal behavior required by
 * the Vulkan timeline semaphore API.
 *
 * Alternatively, &DRM_IOCTL_SYNCOBJ_EVENTFD can be used to wait without
 * blocking: an eventfd will be signaled when the syncobj is. This is useful to
 * integrate the wait in an event loop.
 *
 *
 * Import/export of syncobjs
 * -------------------------
 *
 * &DRM_IOCTL_SYNCOBJ_FD_TO_HANDLE and &DRM_IOCTL_SYNCOBJ_HANDLE_TO_FD
 * provide two mechanisms for import/export of syncobjs.
 *
 * The first lets the client import or export an entire syncobj to a file
 * descriptor.
 * These fd's are opaque and have no other use case, except passing the
 * syncobj between processes.
 * All exported file descriptors and any syncobj handles created as a
 * result of importing those file descriptors own a reference to the
 * same underlying struct &drm_syncobj and the syncobj can be used
 * persistently across all the processes with which it is shared.
 * The syncobj is freed only once the last reference is dropped.
 * Unlike dma-buf, importing a syncobj creates a new handle (with its own
 * reference) for every import instead of de-duplicating.
 * The primary use-case of this persistent import/export is for shared
 * Vulkan fences and semaphores.
 *
 * The second import/export mechanism, which is indicated by
 * &DRM_SYNCOBJ_FD_TO_HANDLE_FLAGS_IMPORT_SYNC_FILE or
 * &DRM_SYNCOBJ_HANDLE_TO_FD_FLAGS_EXPORT_SYNC_FILE lets the client
 * import/export the syncobj's current fence from/to a &sync_file.
 * When a syncobj is exported to a sync file, that sync file wraps the
 * sycnobj's fence at the time of export and any later signal or reset
 * operations on the syncobj will not affect the exported sync file.
 * When a sync file is imported into a syncobj, the syncobj's fence is set
 * to the fence wrapped by that sync file.
 * Because sync files are immutable, resetting or signaling the syncobj
 * will not affect any sync files whose fences have been imported into the
 * syncobj.
 *
 *
 * Import/export of timeline points in timeline syncobjs
 * -----------------------------------------------------
 *
 * &DRM_IOCTL_SYNCOBJ_TRANSFER provides a mechanism to transfer a struct
 * &dma_fence_chain of a syncobj at a given u64 point to another u64 point
 * into another syncobj.
 *
 * Note that if you want to transfer a struct &dma_fence_chain from a given
 * point on a timeline syncobj from/into a binary syncobj, you can use the
 * point 0 to mean take/replace the fence in the syncobj.
 */

#include <linux/anon_inodes.h>
#include <linux/compiler.h>
#include <linux/cpufeature.h>
#include <linux/dma-fence-unwrap.h>
#include <linux/dma-fence.h>
#include <linux/eventfd.h>
#include <linux/file.h>
#include <linux/fs.h>
#include <linux/kernel.h>
#include <linux/list.h>
#include <linux/pci.h>
#include <linux/prefetch.h>
#include <linux/rcupdate.h>
#include <linux/sched.h>
#include <linux/sched/signal.h>
#include <linux/slab.h>
#include <linux/spinlock.h>
#include <linux/sync_file.h>
#include <linux/types.h>
#include <linux/uaccess.h>
#include <asm/nops.h>

#include <drm/drm.h>
#include <drm/drm_drv.h>
#include <drm/drm_file.h>
#include <drm/drm_gem.h>
#include <drm/drm_print.h>
#include <drm/drm_syncobj.h>
#include <drm/drm_utils.h>

#include "drm_internal.h"

struct syncobj_wait_entry {
	struct list_head node;
	struct task_struct *task;
	struct dma_fence *fence;
	struct dma_fence_cb fence_cb;
	u64    point;
};

static void syncobj_wait_syncobj_func(struct drm_syncobj *syncobj,
				      struct syncobj_wait_entry *wait);

struct syncobj_eventfd_entry {
	struct list_head node;
	struct dma_fence *fence;
	struct dma_fence_cb fence_cb;
	struct drm_syncobj *syncobj;
	struct eventfd_ctx *ev_fd_ctx;
	u64 point;
	u32 flags;
};

static void
syncobj_eventfd_entry_func(struct drm_syncobj *syncobj,
			   struct syncobj_eventfd_entry *entry);

/**
 * drm_syncobj_find - lookup and reference a sync object.
 * @file_private: drm file private pointer
 * @handle: sync object handle to lookup.
 *
 * Returns a reference to the syncobj pointed to by handle or NULL. The
 * reference must be released by calling drm_syncobj_put().
 */
struct drm_syncobj *drm_syncobj_find(struct drm_file *file_private,
				     u32 handle)
{
	struct drm_syncobj *syncobj;

	spin_lock(&file_private->syncobj_table_lock);

	/* Check if we currently have a reference on the object */
	syncobj = idr_find(&file_private->syncobj_idr, handle);
	if (syncobj)
		drm_syncobj_get(syncobj);

	spin_unlock(&file_private->syncobj_table_lock);

	return syncobj;
}
EXPORT_SYMBOL(drm_syncobj_find);

/* Module parameter to force-enable/disable Vega optimizations */
int amdgpu_vega_optimization = -1; /* -1 = auto, 0 = off, 1 = on */
EXPORT_SYMBOL(amdgpu_vega_optimization); /* Export the symbol for other modules */
module_param_named(vega_optimization, amdgpu_vega_optimization, int, 0444);
MODULE_PARM_DESC(vega_optimization, "Enable AMD Vega-specific optimizations (-1 = auto, 0 = off, 1 = on)");

/* AMD Vega device IDs */
struct amd_vega_device_id {
	u16 device_id;
	const char *name;
};

/* Vega10 device IDs */
static const struct amd_vega_device_id vega10_ids[] = {
	{ 0x6860, "Vega10 GLXT SERVER (Instinct MI25)" },
	{ 0x6861, "Vega10 GLXT (Pro WX 9100)" },
	{ 0x6862, "Vega10 SSG (Pro SSG)" },
	{ 0x6863, "Vega10 GLXTX (Vega Frontier Edition)" },
	{ 0x6864, "Vega10 GLXT SERVER" },
	{ 0x6867, "Vega10 XLA (Pro Vega 56)" },
	{ 0x6868, "Vega10 GLXL (Pro WX 8200)" },
	{ 0x6869, "Vega10 XGA" },
	{ 0x686A, "Vega10 LEA" },
	{ 0x686B, "Vega10 XTXA (Pro Vega 64X)" },
	{ 0x686C, "Vega10 GLXT SERVER VF (Instinct MI25 MxGPU)" },
	{ 0x686D, "Vega10 GLXTA" },
	{ 0x686E, "Vega10 GLXLA" },
	{ 0x687F, "Vega10 (RX Vega Series)" },
};

/* Vega12 device IDs */
static const struct amd_vega_device_id vega12_ids[] = {
	{ 0x69A0, "Vega12 GL MXT" },
	{ 0x69A1, "Vega12 GL MXL" },
	{ 0x69A2, "Vega12 GL XL" },
	{ 0x69A3, "Vega12 Reserved" },
	{ 0x69AF, "Vega12 (Pro Vega 16/20)" },
};

/* Vega20 device IDs */
static const struct amd_vega_device_id vega20_ids[] = {
	{ 0x66A0, "Vega20 (Instinct Series)" },
	{ 0x66A1, "Vega20 (Server XT 32GB/MI50/Pro VII)" },
	{ 0x66A2, "Vega20" },
	{ 0x66A3, "Vega20 (Pro Vega II/Duo)" },
	{ 0x66A4, "Vega20" },
	{ 0x66A7, "Vega20" },
	{ 0x66AF, "Vega20 (Radeon VII)" },
};

/* Cache the result to avoid repeated PCI bus scanning */
static atomic_t vega_gpu_present = ATOMIC_INIT(-1);

/* Check if a PCI device matches any Vega device ID */
static bool is_device_amd_vega(struct pci_dev *pdev)
{
	u16 device_id;
	int i;

	if (!pdev)
		return false;

	/* Verify it's an AMD GPU: Vendor ID 0x1002 is AMD */
	if (pdev->vendor != 0x1002)
		return false;

	device_id = pdev->device;

	/* Check against Vega10 devices */
	for (i = 0; i < ARRAY_SIZE(vega10_ids); i++) {
		if (device_id == vega10_ids[i].device_id)
			return true;
	}

	/* Check against Vega12 devices */
	for (i = 0; i < ARRAY_SIZE(vega12_ids); i++) {
		if (device_id == vega12_ids[i].device_id)
			return true;
	}

	/* Check against Vega20 devices */
	for (i = 0; i < ARRAY_SIZE(vega20_ids); i++) {
		if (device_id == vega20_ids[i].device_id)
			return true;
	}

	return false;
}

/**
 * detect_amd_vega_gpus - Safely detects AMD Vega GPUs in the system
 *
 * Uses atomic operations to ensure only one thread performs PCI scanning,
 * with proper synchronization and timeout handling to avoid race conditions
 * and deadlocks.
 *
 * Return: true if an AMD Vega GPU is present, false otherwise
 */
static bool detect_amd_vega_gpus(void)
{
	struct pci_dev *pdev = NULL;
	int detection_in_progress = -2;  /* State while detection is in progress */
	int not_detected = -1;           /* Initial state: not detected */
	int old_val, result;

	/* Fast path: Check if we've already detected Vega GPUs */
	result = atomic_read(&vega_gpu_present);
	if (result >= 0)
		return (result > 0);

	/* Try to claim the detection work atomically */
	old_val = atomic_cmpxchg(&vega_gpu_present, not_detected, detection_in_progress);

	if (old_val != not_detected) {
		/* Either detection is in progress or already done */
		if (old_val >= 0)  /* Detection completed */
			return (old_val > 0);

		/* Detection is in progress by another thread */
		/* Wait with timeout for the other thread to complete */
		unsigned int timeout = 1000;  /* Arbitrary timeout */
		while (timeout--) {
			cpu_relax();
			result = atomic_read(&vega_gpu_present);
			if (result != detection_in_progress)
				return (result > 0);
		}

		/* Timeout waiting for detection - assume no Vega GPU for safety */
		return false;
	}

	/* We own the detection process now */

	/* Scan the PCI bus for AMD Vega GPUs */
	while ((pdev = pci_get_class(PCI_CLASS_DISPLAY_VGA << 8, pdev))) {
		if (is_device_amd_vega(pdev)) {
			atomic_set(&vega_gpu_present, 1);
			pci_dev_put(pdev);
			return true;
		}
		/* Release reference if not matching */
		pci_dev_put(pdev);
	}

	/* Also check 3D controller class for discrete GPUs */
	while ((pdev = pci_get_class(PCI_CLASS_DISPLAY_3D << 8, pdev))) {
		if (is_device_amd_vega(pdev)) {
			atomic_set(&vega_gpu_present, 1);
			pci_dev_put(pdev);
			return true;
		}
		/* Release reference if not matching */
		pci_dev_put(pdev);
	}

	/* No Vega GPU found */
	atomic_set(&vega_gpu_present, 0);
	return false;
}

/* Main function to determine if we should use Vega optimizations */
static inline bool is_amd_vega_gpu(void)
{
	/* Check module parameter first */
	if (amdgpu_vega_optimization >= 0)
		return (amdgpu_vega_optimization > 0);

	/* Otherwise auto-detect */
	return detect_amd_vega_gpus();
}

/**
 * process_fence_batch_amd_vega - Process a batch of fences efficiently on AMD Vega GPUs
 * @fences: Array of fence pointers to process
 * @count: Number of fences in the array
 * @signaled: Output array to store signaled state (must be at least 'count' elements)
 *
 * Processes a batch of fences with optimized prefetching patterns specifically
 * tuned for AMD Vega GPU's memory hierarchy and Intel Raptor Lake CPUs.
 */
/* Batch size aligned with Vega wavefront and cache line size */
#define VEGA_BATCH_SIZE 64
/* Prefetch distance optimized for Raptor Lake L1/L2 cache */
#define RL_PREFETCH_DISTANCE 16

static void process_fence_batch_amd_vega(struct dma_fence **fences,
										 int count,
										 bool *signaled)
{
	int i;

	if (unlikely(!fences || !signaled || count <= 0))
		return;

	/* Ensure safe initialization of signaled array */
	memset(signaled, 0, count * sizeof(bool));

	#ifdef CONFIG_X86
	/* Prefetch fences in groups to match Vega's HBM2 access pattern */
	for (i = 0; i < min(RL_PREFETCH_DISTANCE, count); i++) {
		if (likely(fences[i])) /* Avoid prefetching NULL pointers */
			prefetchw(fences[i]);
	}

	/* Use AVX2 to process fences on Raptor Lake */
	if (boot_cpu_has(X86_FEATURE_AVX2) && count >= 8) {
		/* Process fences in groups of 8 using AVX2 for prefetching and NULL check */
		for (i = 0; i <= count - 8; i += 8) {
			int j;

			/* Prefetch ahead - optimized for Raptor Lake's cache hierarchy */
			if (i + RL_PREFETCH_DISTANCE < count) {
				for (j = 0; j < 8 && (i + RL_PREFETCH_DISTANCE + j) < count; j++) {
					if (likely(fences[i + RL_PREFETCH_DISTANCE + j]))
						prefetchw(fences[i + RL_PREFETCH_DISTANCE + j]);
				}
			}

			/* Use safer external calls instead of inline assembly for the actual check */
			for (j = 0; j < 8; j++) {
				if (likely(fences[i + j])) {
					signaled[i + j] = dma_fence_is_signaled(fences[i + j]);
				}
			}
		}

		/* Ensure AVX state is cleaned up properly */
		asm volatile("vzeroupper" ::: "memory");

		/* Handle remaining fences */
		for (; i < count; i++) {
			if (likely(fences[i])) {
				signaled[i] = dma_fence_is_signaled(fences[i]);
			}
		}
	} else {
		/* Non-AVX2 path with optimized prefetching */
		for (i = 0; i < count; i++) {
			/* Prefetch ahead to keep the cache pipeline full */
			if (i + RL_PREFETCH_DISTANCE < count && likely(fences[i + RL_PREFETCH_DISTANCE])) {
				prefetchw(fences[i + RL_PREFETCH_DISTANCE]);
			}

			/* Process current element */
			if (likely(fences[i])) {
				signaled[i] = dma_fence_is_signaled(fences[i]);
			}
		}
	}
	#else
	/* Fallback for non-x86 platforms */
	for (i = 0; i < count; i++) {
		if (likely(fences[i])) {
			signaled[i] = dma_fence_is_signaled(fences[i]);
		}
	}
	#endif
}

/**
 * drm_syncobj_fence_add_wait - Add a wait callback to a fence
 * @syncobj: Sync object to add the wait callback to
 * @wait: Wait entry to add
 *
 * Uses RCU for read-only access to reduce spinlock contention on Intel Raptor
 * Lake CPUs, falling back to spinlock for modifications.
 */
static void drm_syncobj_fence_add_wait(struct drm_syncobj *syncobj,
									   struct syncobj_wait_entry *wait)
{
	struct dma_fence *fence;

	if (unlikely(!syncobj || !wait))
		return;

	if (wait->fence)
		return;

	/* Ensure wait->node is initialized to prevent list corruption */
	if (!wait->node.next)
		INIT_LIST_HEAD(&wait->node);

	/* Fast path: Use RCU to read fence without spinlock */
	rcu_read_lock();
	fence = rcu_dereference(syncobj->fence);
	if (likely(fence)) {
		fence = dma_fence_get_rcu(fence);
		if (likely(fence)) {
			int ret;
			/* Ensure memory ordering for RCU read */
			smp_mb();
			ret = dma_fence_chain_find_seqno(&fence, wait->point);
			if (likely(!ret)) {
				if (!fence) {
					wait->fence = dma_fence_get_stub();
				} else {
					wait->fence = fence;
				}
				rcu_read_unlock();
				return;
			}
			dma_fence_put(fence);
		}
	}
	/* Fix: Ensure RCU unlock happens in all paths */
	rcu_read_unlock();

	/* Slow path: Modify callback list under spinlock */
	spin_lock(&syncobj->lock);
	fence = rcu_dereference_protected(syncobj->fence,
									  lockdep_is_held(&syncobj->lock));

	if (unlikely(!fence)) {
		list_add_tail(&wait->node, &syncobj->cb_list);
	} else {
		int ret;
		fence = dma_fence_get(fence);
		ret = dma_fence_chain_find_seqno(&fence, wait->point);
		if (ret) {
			dma_fence_put(fence);
			list_add_tail(&wait->node, &syncobj->cb_list);
		} else if (unlikely(!fence)) {
			wait->fence = dma_fence_get_stub();
		} else {
			wait->fence = fence;
		}
	}
	spin_unlock(&syncobj->lock);
}

static void drm_syncobj_remove_wait(struct drm_syncobj *syncobj,
				    struct syncobj_wait_entry *wait)
{
	if (!wait->node.next)
		return;

	spin_lock(&syncobj->lock);
	list_del_init(&wait->node);
	spin_unlock(&syncobj->lock);
}

static void
syncobj_eventfd_entry_free(struct syncobj_eventfd_entry *entry)
{
	eventfd_ctx_put(entry->ev_fd_ctx);
	dma_fence_put(entry->fence);
	/* This happens either inside the syncobj lock, or after the node has
	 * already been removed from the list.
	 */
	list_del(&entry->node);
	kfree(entry);
}

static void
drm_syncobj_add_eventfd(struct drm_syncobj *syncobj,
			struct syncobj_eventfd_entry *entry)
{
	spin_lock(&syncobj->lock);
	list_add_tail(&entry->node, &syncobj->ev_fd_list);
	syncobj_eventfd_entry_func(syncobj, entry);
	spin_unlock(&syncobj->lock);
}

/**
 * drm_syncobj_add_point - add new timeline point to the syncobj
 * @syncobj: sync object to add timeline point do
 * @chain: chain node to use to add the point
 * @fence: fence to encapsulate in the chain node
 * @point: sequence number to use for the point
 *
 * Add the chain node as new timeline point to the syncobj.
 */
void drm_syncobj_add_point(struct drm_syncobj *syncobj,
			   struct dma_fence_chain *chain,
			   struct dma_fence *fence,
			   uint64_t point)
{
	struct syncobj_wait_entry *wait_cur, *wait_tmp;
	struct syncobj_eventfd_entry *ev_fd_cur, *ev_fd_tmp;
	struct dma_fence *prev;

	dma_fence_get(fence);

	spin_lock(&syncobj->lock);

	prev = drm_syncobj_fence_get(syncobj);
	/* You are adding an unorder point to timeline, which could cause payload returned from query_ioctl is 0! */
	if (prev && prev->seqno >= point)
		DRM_DEBUG("You are adding an unorder point to timeline!\n");
	dma_fence_chain_init(chain, prev, fence, point);
	rcu_assign_pointer(syncobj->fence, &chain->base);

	list_for_each_entry_safe(wait_cur, wait_tmp, &syncobj->cb_list, node)
		syncobj_wait_syncobj_func(syncobj, wait_cur);
	list_for_each_entry_safe(ev_fd_cur, ev_fd_tmp, &syncobj->ev_fd_list, node)
		syncobj_eventfd_entry_func(syncobj, ev_fd_cur);
	spin_unlock(&syncobj->lock);

	/* Walk the chain once to trigger garbage collection */
	dma_fence_chain_for_each(fence, prev);
	dma_fence_put(prev);
}
EXPORT_SYMBOL(drm_syncobj_add_point);

/**
 * drm_syncobj_replace_fence - replace fence in a sync object.
 * @syncobj: Sync object to replace fence in
 * @fence: fence to install in sync file.
 *
 * This replaces the fence on a sync object.
 */
void drm_syncobj_replace_fence(struct drm_syncobj *syncobj,
			       struct dma_fence *fence)
{
	struct dma_fence *old_fence;
	struct syncobj_wait_entry *wait_cur, *wait_tmp;
	struct syncobj_eventfd_entry *ev_fd_cur, *ev_fd_tmp;

	if (fence)
		dma_fence_get(fence);

	spin_lock(&syncobj->lock);

	old_fence = rcu_dereference_protected(syncobj->fence,
					      lockdep_is_held(&syncobj->lock));
	rcu_assign_pointer(syncobj->fence, fence);

	if (fence != old_fence) {
		list_for_each_entry_safe(wait_cur, wait_tmp, &syncobj->cb_list, node)
			syncobj_wait_syncobj_func(syncobj, wait_cur);
		list_for_each_entry_safe(ev_fd_cur, ev_fd_tmp, &syncobj->ev_fd_list, node)
			syncobj_eventfd_entry_func(syncobj, ev_fd_cur);
	}

	spin_unlock(&syncobj->lock);

	dma_fence_put(old_fence);
}
EXPORT_SYMBOL(drm_syncobj_replace_fence);

/**
 * drm_syncobj_assign_null_handle - assign a stub fence to the sync object
 * @syncobj: sync object to assign the fence on
 *
 * Assign a already signaled stub fence to the sync object.
 */
static int drm_syncobj_assign_null_handle(struct drm_syncobj *syncobj)
{
	struct dma_fence *fence = dma_fence_allocate_private_stub(ktime_get());

	if (!fence)
		return -ENOMEM;

	drm_syncobj_replace_fence(syncobj, fence);
	dma_fence_put(fence);
	return 0;
}

/* 5s default for wait submission */
#define DRM_SYNCOBJ_WAIT_FOR_SUBMIT_TIMEOUT 5000000000ULL
/**
 * drm_syncobj_find_fence - lookup and reference the fence in a sync object
 * @file_private: drm file private pointer
 * @handle: sync object handle to lookup
 * @point: timeline point
 * @flags: DRM_SYNCOBJ_WAIT_FLAGS_WAIT_FOR_SUBMIT or not
 * @fence: out parameter for the fence
 *
 * Looks up and references the fence in a sync object, optimized for Intel
 * Raptor Lake (CPU) using likely/unlikely hints to reduce branch mispredictions.
 *
 * Returns 0 on success or a negative error value on failure. On success @fence
 * contains a reference to the fence, which must be released by calling
 * dma_fence_put().
 */
int drm_syncobj_find_fence(struct drm_file *file_private,
						   u32 handle, u64 point, u64 flags,
						   struct dma_fence **fence)
{
	struct drm_syncobj *syncobj = drm_syncobj_find(file_private, handle);
	struct syncobj_wait_entry wait;
	u64 timeout = nsecs_to_jiffies64(DRM_SYNCOBJ_WAIT_FOR_SUBMIT_TIMEOUT);
	int ret;

	/* Use unlikely to optimize branch prediction on Raptor Lake */
	if (unlikely(flags & ~DRM_SYNCOBJ_WAIT_FLAGS_WAIT_FOR_SUBMIT))
		return -EINVAL;

	/* Use unlikely to optimize branch prediction on Raptor Lake */
	if (unlikely(!syncobj))
		return -ENOENT;

	/* Waiting for userspace with locks help is illegal cause that can
	 * trivial deadlock with page faults for example. Make lockdep complain
	 * about it early on.
	 */
	if (flags & DRM_SYNCOBJ_WAIT_FLAGS_WAIT_FOR_SUBMIT) {
		might_sleep();
		lockdep_assert_none_held_once();
	}

	*fence = drm_syncobj_fence_get(syncobj);

	/* Use likely to optimize branch prediction on Raptor Lake */
	if (likely(*fence)) {
		/* Use likely to optimize branch prediction on Raptor Lake */
		ret = dma_fence_chain_find_seqno(fence, point);
		if (likely(!ret)) {
			/* Use unlikely to optimize branch prediction on Raptor Lake */
			if (unlikely(!*fence))
				*fence = dma_fence_get_stub();

			goto out;
		}
		dma_fence_put(*fence);
	} else {
		ret = -EINVAL;
	}

	if (!(flags & DRM_SYNCOBJ_WAIT_FLAGS_WAIT_FOR_SUBMIT))
		goto out;

	memset(&wait, 0, sizeof(wait));
	wait.task = current;
	wait.point = point;
	drm_syncobj_fence_add_wait(syncobj, &wait);

	do {
		set_current_state(TASK_INTERRUPTIBLE);
		if (wait.fence) {
			ret = 0;
			break;
		}
		/* Use unlikely to optimize branch prediction on Raptor Lake */
		if (unlikely(timeout == 0)) {
			ret = -ETIME;
			break;
		}

		/* Use unlikely to optimize branch prediction on Raptor Lake */
		if (unlikely(signal_pending(current))) {
			ret = -ERESTARTSYS;
			break;
		}

		timeout = schedule_timeout(timeout);
	} while (1);

	__set_current_state(TASK_RUNNING);
	*fence = wait.fence;

	if (wait.node.next)
		drm_syncobj_remove_wait(syncobj, &wait);

	out:
	drm_syncobj_put(syncobj);

	return ret;
}
EXPORT_SYMBOL(drm_syncobj_find_fence);

/**
 * drm_syncobj_free - free a sync object.
 * @kref: kref to free.
 *
 * Only to be called from kref_put in drm_syncobj_put.
 */
void drm_syncobj_free(struct kref *kref)
{
	struct drm_syncobj *syncobj = container_of(kref,
						   struct drm_syncobj,
						   refcount);
	struct syncobj_eventfd_entry *ev_fd_cur, *ev_fd_tmp;

	drm_syncobj_replace_fence(syncobj, NULL);

	list_for_each_entry_safe(ev_fd_cur, ev_fd_tmp, &syncobj->ev_fd_list, node)
		syncobj_eventfd_entry_free(ev_fd_cur);

	kfree(syncobj);
}
EXPORT_SYMBOL(drm_syncobj_free);

/**
 * drm_syncobj_create - create a new syncobj
 * @out_syncobj: returned syncobj
 * @flags: DRM_SYNCOBJ_* flags
 * @fence: if non-NULL, the syncobj will represent this fence
 *
 * This is the first function to create a sync object. After creating, drivers
 * probably want to make it available to userspace, either through
 * drm_syncobj_get_handle() or drm_syncobj_get_fd().
 *
 * Returns 0 on success or a negative error value on failure.
 */
int drm_syncobj_create(struct drm_syncobj **out_syncobj, uint32_t flags,
		       struct dma_fence *fence)
{
	int ret;
	struct drm_syncobj *syncobj;

	syncobj = kzalloc(sizeof(struct drm_syncobj), GFP_KERNEL);
	if (!syncobj)
		return -ENOMEM;

	kref_init(&syncobj->refcount);
	INIT_LIST_HEAD(&syncobj->cb_list);
	INIT_LIST_HEAD(&syncobj->ev_fd_list);
	spin_lock_init(&syncobj->lock);

	if (flags & DRM_SYNCOBJ_CREATE_SIGNALED) {
		ret = drm_syncobj_assign_null_handle(syncobj);
		if (ret < 0) {
			drm_syncobj_put(syncobj);
			return ret;
		}
	}

	if (fence)
		drm_syncobj_replace_fence(syncobj, fence);

	*out_syncobj = syncobj;
	return 0;
}
EXPORT_SYMBOL(drm_syncobj_create);

/**
 * drm_syncobj_get_handle - get a handle from a syncobj
 * @file_private: drm file private pointer
 * @syncobj: Sync object to export
 * @handle: out parameter with the new handle
 *
 * Exports a sync object created with drm_syncobj_create() as a handle on
 * @file_private to userspace.
 *
 * Returns 0 on success or a negative error value on failure.
 */
int drm_syncobj_get_handle(struct drm_file *file_private,
			   struct drm_syncobj *syncobj, u32 *handle)
{
	int ret;

	/* take a reference to put in the idr */
	drm_syncobj_get(syncobj);

	idr_preload(GFP_KERNEL);
	spin_lock(&file_private->syncobj_table_lock);
	ret = idr_alloc(&file_private->syncobj_idr, syncobj, 1, 0, GFP_NOWAIT);
	spin_unlock(&file_private->syncobj_table_lock);

	idr_preload_end();

	if (ret < 0) {
		drm_syncobj_put(syncobj);
		return ret;
	}

	*handle = ret;
	return 0;
}
EXPORT_SYMBOL(drm_syncobj_get_handle);

static int drm_syncobj_create_as_handle(struct drm_file *file_private,
					u32 *handle, uint32_t flags)
{
	int ret;
	struct drm_syncobj *syncobj;

	ret = drm_syncobj_create(&syncobj, flags, NULL);
	if (ret)
		return ret;

	ret = drm_syncobj_get_handle(file_private, syncobj, handle);
	drm_syncobj_put(syncobj);
	return ret;
}

static int drm_syncobj_destroy(struct drm_file *file_private,
			       u32 handle)
{
	struct drm_syncobj *syncobj;

	spin_lock(&file_private->syncobj_table_lock);
	syncobj = idr_remove(&file_private->syncobj_idr, handle);
	spin_unlock(&file_private->syncobj_table_lock);

	if (!syncobj)
		return -EINVAL;

	drm_syncobj_put(syncobj);
	return 0;
}

static int drm_syncobj_file_release(struct inode *inode, struct file *file)
{
	struct drm_syncobj *syncobj = file->private_data;

	drm_syncobj_put(syncobj);
	return 0;
}

static const struct file_operations drm_syncobj_file_fops = {
	.release = drm_syncobj_file_release,
};

/**
 * drm_syncobj_get_fd - get a file descriptor from a syncobj
 * @syncobj: Sync object to export
 * @p_fd: out parameter with the new file descriptor
 *
 * Exports a sync object created with drm_syncobj_create() as a file descriptor.
 *
 * Returns 0 on success or a negative error value on failure.
 */
int drm_syncobj_get_fd(struct drm_syncobj *syncobj, int *p_fd)
{
	struct file *file;
	int fd;

	fd = get_unused_fd_flags(O_CLOEXEC);
	if (fd < 0)
		return fd;

	file = anon_inode_getfile("syncobj_file",
				  &drm_syncobj_file_fops,
				  syncobj, 0);
	if (IS_ERR(file)) {
		put_unused_fd(fd);
		return PTR_ERR(file);
	}

	drm_syncobj_get(syncobj);
	fd_install(fd, file);

	*p_fd = fd;
	return 0;
}
EXPORT_SYMBOL(drm_syncobj_get_fd);

static int drm_syncobj_handle_to_fd(struct drm_file *file_private,
				    u32 handle, int *p_fd)
{
	struct drm_syncobj *syncobj = drm_syncobj_find(file_private, handle);
	int ret;

	if (!syncobj)
		return -EINVAL;

	ret = drm_syncobj_get_fd(syncobj, p_fd);
	drm_syncobj_put(syncobj);
	return ret;
}

static int drm_syncobj_fd_to_handle(struct drm_file *file_private,
				    int fd, u32 *handle)
{
	struct drm_syncobj *syncobj;
	CLASS(fd, f)(fd);
	int ret;

	if (fd_empty(f))
		return -EINVAL;

	if (fd_file(f)->f_op != &drm_syncobj_file_fops)
		return -EINVAL;

	/* take a reference to put in the idr */
	syncobj = fd_file(f)->private_data;
	drm_syncobj_get(syncobj);

	idr_preload(GFP_KERNEL);
	spin_lock(&file_private->syncobj_table_lock);
	ret = idr_alloc(&file_private->syncobj_idr, syncobj, 1, 0, GFP_NOWAIT);
	spin_unlock(&file_private->syncobj_table_lock);
	idr_preload_end();

	if (ret > 0) {
		*handle = ret;
		ret = 0;
	} else
		drm_syncobj_put(syncobj);

	return ret;
}

static int drm_syncobj_import_sync_file_fence(struct drm_file *file_private,
					      int fd, int handle)
{
	struct dma_fence *fence = sync_file_get_fence(fd);
	struct drm_syncobj *syncobj;

	if (!fence)
		return -EINVAL;

	syncobj = drm_syncobj_find(file_private, handle);
	if (!syncobj) {
		dma_fence_put(fence);
		return -ENOENT;
	}

	drm_syncobj_replace_fence(syncobj, fence);
	dma_fence_put(fence);
	drm_syncobj_put(syncobj);
	return 0;
}

static int drm_syncobj_export_sync_file(struct drm_file *file_private,
					int handle, int *p_fd)
{
	int ret;
	struct dma_fence *fence;
	struct sync_file *sync_file;
	int fd = get_unused_fd_flags(O_CLOEXEC);

	if (fd < 0)
		return fd;

	ret = drm_syncobj_find_fence(file_private, handle, 0, 0, &fence);
	if (ret)
		goto err_put_fd;

	sync_file = sync_file_create(fence);

	dma_fence_put(fence);

	if (!sync_file) {
		ret = -EINVAL;
		goto err_put_fd;
	}

	fd_install(fd, sync_file->file);

	*p_fd = fd;
	return 0;
err_put_fd:
	put_unused_fd(fd);
	return ret;
}
/**
 * drm_syncobj_open - initializes syncobj file-private structures at devnode open time
 * @file_private: drm file-private structure to set up
 *
 * Called at device open time, sets up the structure for handling refcounting
 * of sync objects.
 */
void
drm_syncobj_open(struct drm_file *file_private)
{
	idr_init_base(&file_private->syncobj_idr, 1);
	spin_lock_init(&file_private->syncobj_table_lock);
}

static int
drm_syncobj_release_handle(int id, void *ptr, void *data)
{
	struct drm_syncobj *syncobj = ptr;

	drm_syncobj_put(syncobj);
	return 0;
}

/**
 * drm_syncobj_release - release file-private sync object resources
 * @file_private: drm file-private structure to clean up
 *
 * Called at close time when the filp is going away.
 *
 * Releases any remaining references on objects by this filp.
 */
void
drm_syncobj_release(struct drm_file *file_private)
{
	idr_for_each(&file_private->syncobj_idr,
		     &drm_syncobj_release_handle, file_private);
	idr_destroy(&file_private->syncobj_idr);
}

int
drm_syncobj_create_ioctl(struct drm_device *dev, void *data,
			 struct drm_file *file_private)
{
	struct drm_syncobj_create *args = data;

	if (!drm_core_check_feature(dev, DRIVER_SYNCOBJ))
		return -EOPNOTSUPP;

	/* no valid flags yet */
	if (args->flags & ~DRM_SYNCOBJ_CREATE_SIGNALED)
		return -EINVAL;

	return drm_syncobj_create_as_handle(file_private,
					    &args->handle, args->flags);
}

int
drm_syncobj_destroy_ioctl(struct drm_device *dev, void *data,
			  struct drm_file *file_private)
{
	struct drm_syncobj_destroy *args = data;

	if (!drm_core_check_feature(dev, DRIVER_SYNCOBJ))
		return -EOPNOTSUPP;

	/* make sure padding is empty */
	if (args->pad)
		return -EINVAL;
	return drm_syncobj_destroy(file_private, args->handle);
}

int
drm_syncobj_handle_to_fd_ioctl(struct drm_device *dev, void *data,
				   struct drm_file *file_private)
{
	struct drm_syncobj_handle *args = data;

	if (!drm_core_check_feature(dev, DRIVER_SYNCOBJ))
		return -EOPNOTSUPP;

	if (args->pad)
		return -EINVAL;

	if (args->flags != 0 &&
	    args->flags != DRM_SYNCOBJ_HANDLE_TO_FD_FLAGS_EXPORT_SYNC_FILE)
		return -EINVAL;

	if (args->flags & DRM_SYNCOBJ_HANDLE_TO_FD_FLAGS_EXPORT_SYNC_FILE)
		return drm_syncobj_export_sync_file(file_private, args->handle,
						    &args->fd);

	return drm_syncobj_handle_to_fd(file_private, args->handle,
					&args->fd);
}

int
drm_syncobj_fd_to_handle_ioctl(struct drm_device *dev, void *data,
				   struct drm_file *file_private)
{
	struct drm_syncobj_handle *args = data;

	if (!drm_core_check_feature(dev, DRIVER_SYNCOBJ))
		return -EOPNOTSUPP;

	if (args->pad)
		return -EINVAL;

	if (args->flags != 0 &&
	    args->flags != DRM_SYNCOBJ_FD_TO_HANDLE_FLAGS_IMPORT_SYNC_FILE)
		return -EINVAL;

	if (args->flags & DRM_SYNCOBJ_FD_TO_HANDLE_FLAGS_IMPORT_SYNC_FILE)
		return drm_syncobj_import_sync_file_fence(file_private,
							  args->fd,
							  args->handle);

	return drm_syncobj_fd_to_handle(file_private, args->fd,
					&args->handle);
}

static int drm_syncobj_transfer_to_timeline(struct drm_file *file_private,
					    struct drm_syncobj_transfer *args)
{
	struct drm_syncobj *timeline_syncobj = NULL;
	struct dma_fence *fence, *tmp;
	struct dma_fence_chain *chain;
	int ret;

	timeline_syncobj = drm_syncobj_find(file_private, args->dst_handle);
	if (!timeline_syncobj) {
		return -ENOENT;
	}
	ret = drm_syncobj_find_fence(file_private, args->src_handle,
				     args->src_point, args->flags,
				     &tmp);
	if (ret)
		goto err_put_timeline;

	fence = dma_fence_unwrap_merge(tmp);
	dma_fence_put(tmp);
	if (!fence) {
		ret = -ENOMEM;
		goto err_put_timeline;
	}

	chain = dma_fence_chain_alloc();
	if (!chain) {
		ret = -ENOMEM;
		goto err_free_fence;
	}

	drm_syncobj_add_point(timeline_syncobj, chain, fence, args->dst_point);
err_free_fence:
	dma_fence_put(fence);
err_put_timeline:
	drm_syncobj_put(timeline_syncobj);

	return ret;
}

static int
drm_syncobj_transfer_to_binary(struct drm_file *file_private,
			       struct drm_syncobj_transfer *args)
{
	struct drm_syncobj *binary_syncobj = NULL;
	struct dma_fence *fence;
	int ret;

	binary_syncobj = drm_syncobj_find(file_private, args->dst_handle);
	if (!binary_syncobj)
		return -ENOENT;
	ret = drm_syncobj_find_fence(file_private, args->src_handle,
				     args->src_point, args->flags, &fence);
	if (ret)
		goto err;
	drm_syncobj_replace_fence(binary_syncobj, fence);
	dma_fence_put(fence);
err:
	drm_syncobj_put(binary_syncobj);

	return ret;
}
int
drm_syncobj_transfer_ioctl(struct drm_device *dev, void *data,
			   struct drm_file *file_private)
{
	struct drm_syncobj_transfer *args = data;
	int ret;

	if (!drm_core_check_feature(dev, DRIVER_SYNCOBJ_TIMELINE))
		return -EOPNOTSUPP;

	if (args->pad)
		return -EINVAL;

	if (args->dst_point)
		ret = drm_syncobj_transfer_to_timeline(file_private, args);
	else
		ret = drm_syncobj_transfer_to_binary(file_private, args);

	return ret;
}

/**
 * syncobj_wait_fence_func - Callback function for fence completion
 * @fence: The fence that was completed
 * @cb: The callback structure embedded in syncobj_wait_entry
 *
 * This callback function is registered with dma_fence_add_callback and
 * called when a fence signals. It wakes up the waiting task.
 */
static void syncobj_wait_fence_func(struct dma_fence *fence,
									struct dma_fence_cb *cb)
{
	struct syncobj_wait_entry *wait =
	container_of(cb, struct syncobj_wait_entry, fence_cb);

	wake_up_process(wait->task);
}

static void syncobj_wait_syncobj_func(struct drm_syncobj *syncobj,
									  struct syncobj_wait_entry *wait)
{
	struct dma_fence *fence;

	/* This happens inside the syncobj lock */
	fence = rcu_dereference_protected(syncobj->fence,
									  lockdep_is_held(&syncobj->lock));
	if (!fence) {
		wait->fence = dma_fence_get_stub();
		wake_up_process(wait->task);
		list_del_init(&wait->node);
		return;
	}

	dma_fence_get(fence);
	if (dma_fence_chain_find_seqno(&fence, wait->point)) {
		dma_fence_put(fence);
		return;
	} else if (!fence) {
		wait->fence = dma_fence_get_stub();
	} else {
		wait->fence = fence;
	}

	wake_up_process(wait->task);
	list_del_init(&wait->node);
}

/* State tracking constants for better readability */
#define SYNCOBJ_STATE_VALID     0  /* Valid, not signaled */
#define SYNCOBJ_STATE_NULL      1  /* Null syncobj */
#define SYNCOBJ_STATE_SIGNALED  2  /* Signaled state */

/* Stack allocation size - already defined as 8 in original code */
#define STACK_ARRAY_SIZE        8
/* Define the missing prefetch distance constant */
#define VEGA_PREFETCH_DISTANCE  8  /* Optimized for Vega cache hierarchy */

/**
 * process_vega_fence_batch_wait - Process a batch of fences for AMD Vega GPUs during wait
 * @batch_fences: Array of fences to process
 * @batch_indices: Array of corresponding indices
 * @entries: Array of syncobj wait entries
 * @states: Array of state flags
 * @signaled_count: Pointer to signaled count
 * @idx: Pointer to store first signaled index (if any)
 * @wait_flags: Wait flags
 *
 * Returns: 1 if an immediately signaled fence was found in ANY mode, 0 otherwise
 */
static int process_vega_fence_batch_wait(struct dma_fence **batch_fences,
										 int *batch_indices,
										 int batch_size,
										 struct syncobj_wait_entry *entries,
										 uint8_t *states,
										 uint32_t *signaled_count,
										 uint32_t *idx,
										 uint32_t wait_flags)
{
	bool batch_signaled[VEGA_BATCH_SIZE];
	int j;

	/* Batch process the fences using existing Vega-optimized function */
	process_fence_batch_amd_vega(batch_fences, batch_size, batch_signaled);

	for (j = 0; j < batch_size; j++) {
		int idx_j = batch_indices[j];
		struct syncobj_wait_entry *entry = &entries[idx_j];
		struct dma_fence *batch_fence = batch_fences[j];

		if (batch_signaled[j]) {
			states[idx_j] = SYNCOBJ_STATE_SIGNALED;
			smp_wmb(); /* Ensure state change is visible on NUMA Raptor Lake */

			/* Fixed indentation warning by using proper braces */
			if (*signaled_count == 0 && idx) {
				*idx = idx_j;
			}
			(*signaled_count)++;

			if (!(wait_flags & DRM_SYNCOBJ_WAIT_FLAGS_WAIT_ALL))
				return 1; /* Found a match for ANY mode */
		} else if (wait_flags & DRM_SYNCOBJ_WAIT_FLAGS_WAIT_AVAILABLE) {
			states[idx_j] = SYNCOBJ_STATE_SIGNALED; /* For WAIT_AVAILABLE */
			smp_wmb();

			if (*signaled_count == 0 && idx) {
				*idx = idx_j;
			}
			(*signaled_count)++;

			if (!(wait_flags & DRM_SYNCOBJ_WAIT_FLAGS_WAIT_ALL))
				return 1;
		} else if (batch_fence && !READ_ONCE(entry->fence_cb.func)) {
			/* Avoid race condition in callback registration with atomic check */
			if (dma_fence_is_signaled(batch_fence)) {
				states[idx_j] = SYNCOBJ_STATE_SIGNALED;
				smp_wmb();

				if (*signaled_count == 0 && idx) {
					*idx = idx_j;
				}
				(*signaled_count)++;

				if (!(wait_flags & DRM_SYNCOBJ_WAIT_FLAGS_WAIT_ALL))
					return 1;
			} else if (cmpxchg(&entry->fence_cb.func, NULL, syncobj_wait_fence_func) == NULL) {
				/* Successfully registered callback function atomically */
				if (dma_fence_add_callback(batch_fence,
					&entry->fence_cb,
					syncobj_wait_fence_func)) {
					/* Callback registration failed - treat as signaled */
					WRITE_ONCE(entry->fence_cb.func, NULL); /* Reset */
					states[idx_j] = SYNCOBJ_STATE_SIGNALED;
				smp_wmb();

				if (*signaled_count == 0 && idx) {
					*idx = idx_j;
				}
				(*signaled_count)++;

				if (!(wait_flags & DRM_SYNCOBJ_WAIT_FLAGS_WAIT_ALL))
					return 1;
					}
			}
		}
	}
	return 0;
}

/**
 * drm_syncobj_array_wait_timeout - Wait on an array of sync objects
 * @syncobjs: Array of sync objects to wait on
 * @user_points: User-space pointer to array of timeline points
 * @count: Number of sync objects
 * @flags: Wait flags
 * @timeout: Timeout in jiffies
 * @idx: Index of the first signaled sync object (out parameter)
 * @deadline: Deadline for the wait (optional)
 *
 * Optimized for Intel Raptor Lake (AVX2, cache-aligned data, NUMA-aware)
 * and AMD Vega 64 (batch processing, wavefront-optimized).
 *
 * Return: 0 on success, negative error code on failure
 */
static signed long drm_syncobj_array_wait_timeout(struct drm_syncobj **syncobjs,
												  u64 __user *user_points,
												  uint32_t count,
												  uint32_t flags,
												  signed long timeout,
												  uint32_t *idx,
												  ktime_t *deadline)
{
	struct syncobj_wait_entry stack_entries[STACK_ARRAY_SIZE];
	struct syncobj_wait_entry *entries = NULL;
	/* 64-byte aligned for Raptor Lake cache line size */
	uint8_t stack_states[STACK_ARRAY_SIZE] __aligned(64);
	uint8_t *states = NULL;
	uint32_t signaled_count = 0, i; /* Removed unused variable 'j' */
	struct dma_fence *fence;
	bool use_vega_optimizations;
	signed long ret = timeout;
	unsigned long abs_timeout = 0;
	int init_count = 0; /* Track initialization progress for safer cleanup */

	/* Calculate absolute timeout for more precise handling */
	if (timeout > 0)
		abs_timeout = jiffies + timeout;

	if (unlikely(!syncobjs || count == 0))
		return -EINVAL;

	use_vega_optimizations = is_amd_vega_gpu();

	if (flags & (DRM_SYNCOBJ_WAIT_FLAGS_WAIT_FOR_SUBMIT |
		DRM_SYNCOBJ_WAIT_FLAGS_WAIT_AVAILABLE)) {
		might_sleep();
	lockdep_assert_none_held_once();
		}

		if (user_points && !access_ok(user_points, count * sizeof(*user_points))) {
			ret = -EFAULT;
			goto cleanup_entries;
		}

		/* Allocate entries on stack for small counts */
		if (count > STACK_ARRAY_SIZE) {
			/* Node-aware allocation for NUMA optimization on Raptor Lake */
			entries = kcalloc_node(count, sizeof(*entries), GFP_KERNEL,
								   numa_node_id());
			if (!entries) {
				ret = -ENOMEM;
				goto cleanup_entries;
			}
		} else {
			memset(stack_entries, 0, sizeof(stack_entries));
			entries = stack_entries;
		}

		/* Allocate state array on stack for small counts, aligned to cache line */
		if (count > STACK_ARRAY_SIZE) {
			states = kzalloc_node(count * sizeof(*states), GFP_KERNEL,
								  numa_node_id());
			if (!states) {
				ret = -ENOMEM;
				if (entries != stack_entries)
					kfree(entries);
				goto cleanup_entries;
			}
		} else {
			memset(stack_states, 0, sizeof(stack_states));
			states = stack_states;
		}

		/* Initialize entries */
		for (i = 0; i < count; ++i) {
			/* Hardware-appropriate prefetching */
			if (use_vega_optimizations) {
				if (i + VEGA_PREFETCH_DISTANCE < count && likely(syncobjs[i + VEGA_PREFETCH_DISTANCE]))
					prefetch(syncobjs[i + VEGA_PREFETCH_DISTANCE]);
			} else {
				/* Raptor Lake prefetch */
				if (i + RL_PREFETCH_DISTANCE < count && likely(syncobjs[i + RL_PREFETCH_DISTANCE]))
					prefetchw(syncobjs[i + RL_PREFETCH_DISTANCE]);
			}

			INIT_LIST_HEAD(&entries[i].node);
			entries[i].task = current;
			entries[i].fence = NULL;
			WRITE_ONCE(entries[i].fence_cb.func, NULL);
			entries[i].point = 0;
			init_count++; /* Track initialization progress */

			if (user_points && __get_user(entries[i].point, user_points + i)) {
				ret = -EFAULT;
				goto cleanup_entries;
			}

			if (unlikely(!syncobjs[i])) {
				states[i] = SYNCOBJ_STATE_NULL;
				if (flags & (DRM_SYNCOBJ_WAIT_FLAGS_WAIT_FOR_SUBMIT |
					DRM_SYNCOBJ_WAIT_FLAGS_WAIT_AVAILABLE)) {
					continue;
					} else {
						ret = -EINVAL;
						goto cleanup_entries;
					}
			}

			fence = drm_syncobj_fence_get(syncobjs[i]);
			if (!fence || dma_fence_chain_find_seqno(&fence, entries[i].point)) {
				dma_fence_put(fence); /* Ensure we release the fence */
				if (flags & (DRM_SYNCOBJ_WAIT_FLAGS_WAIT_FOR_SUBMIT |
					DRM_SYNCOBJ_WAIT_FLAGS_WAIT_AVAILABLE)) {
					continue;
					} else {
						ret = -EINVAL;
						goto cleanup_entries;
					}
			}

			entries[i].fence = fence;
		}

		/* Initial signaled check */
		if (use_vega_optimizations && !(flags & DRM_SYNCOBJ_WAIT_FLAGS_WAIT_AVAILABLE)) {
			/* Use optimized batch processing for AMD Vega */
			struct dma_fence *batch_fences[VEGA_BATCH_SIZE];
			int batch_indices[VEGA_BATCH_SIZE];
			int batch_size = 0;

			for (i = 0; i < count; i++) {
				if (states[i] == SYNCOBJ_STATE_NULL) /* Skip null syncobjs */
					continue;

				fence = entries[i].fence;
				if (!fence)
					continue;

				/* Add to batch */
				batch_fences[batch_size] = fence;
				batch_indices[batch_size] = i;
				batch_size++;

				/* Process batch when full */
				if (batch_size >= VEGA_BATCH_SIZE) {
					if (process_vega_fence_batch_wait(batch_fences, batch_indices, batch_size,
						entries, states, &signaled_count,
						idx, flags)) {
						ret = 0; /* Found signaled fence in ANY mode */
						goto cleanup_entries;
						}
						batch_size = 0;
				}
			}

			/* Process final partial batch */
			if (batch_size > 0) {
				if (process_vega_fence_batch_wait(batch_fences, batch_indices, batch_size,
					entries, states, &signaled_count,
					idx, flags)) {
					ret = 0; /* Found signaled fence in ANY mode */
					goto cleanup_entries;
					}
			}
		} else {
			/* Optimized path for Raptor Lake using AVX2-friendly access patterns */
			for (i = 0; i < count; i++) {
				if (states[i] == SYNCOBJ_STATE_NULL) /* Skip null syncobjs */
					continue;

				fence = entries[i].fence;
				if (!fence)
					continue;

				/* Cache-friendly prefetch for next iteration */
				if (i + 1 < count && likely(entries[i + 1].fence))
					prefetch(entries[i + 1].fence);

				if ((flags & DRM_SYNCOBJ_WAIT_FLAGS_WAIT_AVAILABLE) ||
					dma_fence_is_signaled(fence)) {
					states[i] = SYNCOBJ_STATE_SIGNALED;
				smp_wmb(); /* Ensure visibility on NUMA systems */

				if (signaled_count == 0 && idx) {
					*idx = i;
				}
				signaled_count++;
					}
			}
		}

		/* Early return if all or any required fence is signaled */
		if (signaled_count == count ||
			(signaled_count > 0 && !(flags & DRM_SYNCOBJ_WAIT_FLAGS_WAIT_ALL))) {
			ret = 0; /* Success */
			goto cleanup_entries;
			}

			/* Setup wait callbacks for wait-for-submit mode */
			if (flags & (DRM_SYNCOBJ_WAIT_FLAGS_WAIT_FOR_SUBMIT |
				DRM_SYNCOBJ_WAIT_FLAGS_WAIT_AVAILABLE)) {
				for (i = 0; i < count; ++i) {
					/* Fixed indentation warning by using proper braces */
					if (states[i] == SYNCOBJ_STATE_NULL || states[i] == SYNCOBJ_STATE_SIGNALED) {
						continue;
					}
					drm_syncobj_fence_add_wait(syncobjs[i], &entries[i]);
				}
				}

				/* Apply deadlines if requested */
				if (deadline) {
					for (i = 0; i < count; ++i) {
						/* Fixed indentation warning by using proper braces */
						if (states[i] == SYNCOBJ_STATE_NULL || states[i] == SYNCOBJ_STATE_SIGNALED) {
							continue;
						}
						fence = entries[i].fence;
						if (!fence)
							continue;

						dma_fence_set_deadline(fence, *deadline);
					}
				}

				/* Main wait loop */
				do {
					set_current_state(TASK_INTERRUPTIBLE);
					signaled_count = 0;

					/* Hardware-specific wait loop processing */
					if (use_vega_optimizations) {
						struct dma_fence *batch_fences[VEGA_BATCH_SIZE];
						int batch_indices[VEGA_BATCH_SIZE];
						int batch_size = 0;

						for (i = 0; i < count; ++i) {
							if (states[i] == SYNCOBJ_STATE_NULL) /* Skip null syncobjs */
								continue;

							fence = entries[i].fence;
							if (!fence)
								continue;

							/* Skip already signaled fences */
							if (states[i] == SYNCOBJ_STATE_SIGNALED) {
								if (flags & DRM_SYNCOBJ_WAIT_FLAGS_WAIT_ALL) {
									signaled_count++;
								} else {
									if (idx)
										*idx = i;
									goto done_waiting;
								}
								continue;
							}

							/* Add to batch */
							batch_fences[batch_size] = fence;
							batch_indices[batch_size] = i;
							batch_size++;

							/* Process batch when full */
							if (batch_size >= VEGA_BATCH_SIZE) {
								if (process_vega_fence_batch_wait(batch_fences, batch_indices, batch_size,
									entries, states, &signaled_count,
									idx, flags)) {
									goto done_waiting; /* Found signaled fence in ANY mode */
									}
									batch_size = 0;
							}
						}

						/* Process final partial batch */
						if (batch_size > 0) {
							if (process_vega_fence_batch_wait(batch_fences, batch_indices, batch_size,
								entries, states, &signaled_count,
								idx, flags)) {
								goto done_waiting; /* Found signaled fence in ANY mode */
								}
						}
					} else {
						/* Optimized path for Raptor Lake with AVX2-friendly memory access */
						for (i = 0; i < count; ++i) {
							if (states[i] == SYNCOBJ_STATE_NULL) /* Skip null syncobjs */
								continue;

							fence = entries[i].fence;
							if (!fence)
								continue;

							/* AVX2-friendly memory access with aligned prefetch */
							if (i + 1 < count) {
								bool next_valid = syncobjs[i + 1] && states[i + 1] != SYNCOBJ_STATE_NULL;
								if (next_valid && entries[i + 1].fence)
									prefetch(entries[i + 1].fence);
							}

							/* Skip already signaled fences */
							if (states[i] == SYNCOBJ_STATE_SIGNALED) {
								if (flags & DRM_SYNCOBJ_WAIT_FLAGS_WAIT_ALL) {
									signaled_count++;
								} else {
									if (idx)
										*idx = i;
									goto done_waiting;
								}
								continue;
							}

							/* Check signaled state with branch prediction hints */
							if ((flags & DRM_SYNCOBJ_WAIT_FLAGS_WAIT_AVAILABLE) ||
								dma_fence_is_signaled(fence)) {
								states[i] = SYNCOBJ_STATE_SIGNALED;
							smp_wmb(); /* Ensure visibility on NUMA systems */

							if (flags & DRM_SYNCOBJ_WAIT_FLAGS_WAIT_ALL) {
								signaled_count++;
							} else {
								if (idx)
									*idx = i;
								goto done_waiting;
							}
								} else if (fence && !READ_ONCE(entries[i].fence_cb.func)) {
									/* Atomic callback registration to prevent races */
									if (cmpxchg(&entries[i].fence_cb.func, NULL, syncobj_wait_fence_func) == NULL) {
										if (dma_fence_add_callback(fence, &entries[i].fence_cb,
											syncobj_wait_fence_func)) {
											WRITE_ONCE(entries[i].fence_cb.func, NULL); /* Reset */
											states[i] = SYNCOBJ_STATE_SIGNALED;
										smp_wmb();

										if (flags & DRM_SYNCOBJ_WAIT_FLAGS_WAIT_ALL) {
											signaled_count++;
										} else {
											if (idx)
												*idx = i;
											goto done_waiting;
										}
											}
									}
								}
						}
					}

					/* Check if all fences signaled (only applicable for WAIT_ALL) */
					if (signaled_count == count) {
						ret = 0; /* Success */
						goto done_waiting;
					}

					/* Check for timeout using absolute time for precision */
					if (timeout > 0) {
						if (time_after_eq(jiffies, abs_timeout)) {
							ret = -ETIME;
							goto done_waiting;
						}
					} else if (timeout <= 0) {
						ret = -ETIME;
						goto done_waiting;
					}

					if (signal_pending(current)) {
						ret = -ERESTARTSYS;
						goto done_waiting;
					}

					/* Sleep until timeout or signal */
					if (timeout > 0) {
						unsigned long remaining = abs_timeout - jiffies;
						if (remaining <= 0) {
							ret = -ETIME;
							goto done_waiting;
						}
						timeout = schedule_timeout(remaining);
					} else {
						schedule();
					}
				} while (timeout != 0);

				/* Timeout expired */
				ret = -ETIME;

				done_waiting:
				__set_current_state(TASK_RUNNING);

				cleanup_entries:
				/* Clean up all entries */
				for (i = 0; i < count && i < init_count; ++i) {
					if (syncobjs && syncobjs[i] && states && states[i] != SYNCOBJ_STATE_NULL)
						drm_syncobj_remove_wait(syncobjs[i], &entries[i]);

					/* Safe callback removal with atomic operations */
					if (entries && READ_ONCE(entries[i].fence_cb.func) && entries[i].fence) {
						dma_fence_remove_callback(entries[i].fence, &entries[i].fence_cb);
						WRITE_ONCE(entries[i].fence_cb.func, NULL);
					}

					/* Safe fence release */
					if (entries && entries[i].fence) {
						dma_fence_put(entries[i].fence);
						entries[i].fence = NULL;
					}
				}

				/* Free allocated resources */
				if (entries && entries != stack_entries)
					kfree(entries);

	if (states && states != stack_states)
		kfree(states);

	return ret;
}

/**
 * drm_timeout_abs_to_jiffies - calculate jiffies timeout from absolute value
 *
 * @timeout_nsec: timeout nsec component in ns, 0 for poll
 *
 * Calculate the timeout in jiffies from an absolute time in sec/nsec.
 */
signed long drm_timeout_abs_to_jiffies(int64_t timeout_nsec)
{
	ktime_t abs_timeout, now;
	u64 timeout_ns, timeout_jiffies64;

	/* make 0 timeout means poll - absolute 0 doesn't seem valid */
	if (timeout_nsec == 0)
		return 0;

	abs_timeout = ns_to_ktime(timeout_nsec);
	now = ktime_get();

	if (!ktime_after(abs_timeout, now))
		return 0;

	timeout_ns = ktime_to_ns(ktime_sub(abs_timeout, now));

	timeout_jiffies64 = nsecs_to_jiffies64(timeout_ns);
	/*  clamp timeout to avoid infinite timeout */
	if (timeout_jiffies64 >= MAX_SCHEDULE_TIMEOUT - 1)
		return MAX_SCHEDULE_TIMEOUT - 1;

	return timeout_jiffies64 + 1;
}
EXPORT_SYMBOL(drm_timeout_abs_to_jiffies);

/**
 * drm_syncobj_array_find - Find an array of sync objects from their handles
 * @file_private: DRM file to check for handle rights
 * @handles: User pointer to array of handle values
 * @count: Number of handles in the array
 * @stack_syncobjs: Stack array to use for small counts
 * @stack_count: Size of the stack array
 * @syncobjs_out: Output pointer to array of syncobj pointers
 *
 * Safely finds sync objects from user handles, with proper error handling
 * and resource management.
 */
static int drm_syncobj_array_find(struct drm_file *file_private,
								  u32 __user *handles,
								  uint32_t count,
								  struct drm_syncobj **stack_syncobjs,
								  u32 stack_count,
								  struct drm_syncobj ***syncobjs_out)
{
	struct drm_syncobj **syncobjs;
	uint32_t i;
	int ret = 0;

	/* Validate parameters */
	if (!handles || !syncobjs_out || !file_private)
		return -EINVAL;

	/* Check user pointer validity */
	if (!access_ok(handles, count * sizeof(*handles)))
		return -EFAULT;

	/* Overflow check for allocation size */
	if (count > SIZE_MAX / sizeof(struct drm_syncobj *))
		return -EINVAL;

	/* Allocate memory or use stack array */
	if (count > stack_count) {
		syncobjs = kmalloc_array(count, sizeof(struct drm_syncobj *), GFP_KERNEL);
		if (!syncobjs)
			return -ENOMEM;
	} else {
		syncobjs = stack_syncobjs;
	}

	/* Initialize array for safety */
	for (i = 0; i < count; i++)
		syncobjs[i] = NULL;

	/* Process each handle */
	for (i = 0; i < count; i++) {
		u32 handle;  /* Use u32 to match both user space and drm_syncobj_find */

		if (__get_user(handle, handles++)) {
			ret = -EFAULT;
			goto err_put_syncobjs;
		}

		syncobjs[i] = drm_syncobj_find(file_private, handle);
		if (!syncobjs[i]) {
			ret = -ENOENT;
			goto err_put_syncobjs;
		}
	}

	*syncobjs_out = syncobjs;
	return 0;

	err_put_syncobjs:
	/* Clean up previously allocated objects */
	while (i > 0) {
		--i;  /* Decrement first to access the last valid entry */
		if (syncobjs[i])
			drm_syncobj_put(syncobjs[i]);
	}

	/* Free the array if dynamically allocated */
	if (syncobjs != stack_syncobjs)
		kfree(syncobjs);

	return ret;
}

static void drm_syncobj_array_free(struct drm_syncobj **syncobjs,
				   uint32_t count,
				   struct drm_syncobj **stack_syncobjs)
{
	uint32_t i;

	for (i = 0; i < count; i++)
		drm_syncobj_put(syncobjs[i]);

	if (syncobjs != stack_syncobjs)
		kfree(syncobjs);
}

int
drm_syncobj_wait_ioctl(struct drm_device *dev, void *data,
		       struct drm_file *file_private)
{
	struct drm_syncobj *stack_syncobjs[4];
	struct drm_syncobj_wait *args = data;
	ktime_t deadline, *pdeadline = NULL;
	u32 count = args->count_handles;
	struct drm_syncobj **syncobjs;
	unsigned int possible_flags;
	u32 first = ~0;
	long timeout;
	int ret = 0;

	if (!drm_core_check_feature(dev, DRIVER_SYNCOBJ))
		return -EOPNOTSUPP;

	possible_flags = DRM_SYNCOBJ_WAIT_FLAGS_WAIT_ALL |
			 DRM_SYNCOBJ_WAIT_FLAGS_WAIT_FOR_SUBMIT |
			 DRM_SYNCOBJ_WAIT_FLAGS_WAIT_DEADLINE;

	if (args->flags & ~possible_flags)
		return -EINVAL;

	if (count == 0)
		return 0;

	ret = drm_syncobj_array_find(file_private,
				     u64_to_user_ptr(args->handles),
				     count,
				     stack_syncobjs,
				     ARRAY_SIZE(stack_syncobjs),
				     &syncobjs);
	if (ret < 0)
		return ret;

	if (args->flags & DRM_SYNCOBJ_WAIT_FLAGS_WAIT_DEADLINE) {
		deadline = ns_to_ktime(args->deadline_nsec);
		pdeadline = &deadline;
	}

	timeout = drm_syncobj_array_wait_timeout(syncobjs,
						 NULL,
						 count,
						 args->flags,
						 drm_timeout_abs_to_jiffies(args->timeout_nsec),
						 &first,
						 pdeadline);

	drm_syncobj_array_free(syncobjs, count, stack_syncobjs);

	if (timeout < 0)
		return timeout;

	args->first_signaled = first;

	return 0;
}

int
drm_syncobj_timeline_wait_ioctl(struct drm_device *dev, void *data,
				struct drm_file *file_private)
{
	struct drm_syncobj_timeline_wait *args = data;
	struct drm_syncobj *stack_syncobjs[4];
	ktime_t deadline, *pdeadline = NULL;
	u32 count = args->count_handles;
	struct drm_syncobj **syncobjs;
	unsigned int possible_flags;
	u32 first = ~0;
	long timeout;
	int ret = 0;

	if (!drm_core_check_feature(dev, DRIVER_SYNCOBJ_TIMELINE))
		return -EOPNOTSUPP;

	possible_flags = DRM_SYNCOBJ_WAIT_FLAGS_WAIT_ALL |
			 DRM_SYNCOBJ_WAIT_FLAGS_WAIT_FOR_SUBMIT |
			 DRM_SYNCOBJ_WAIT_FLAGS_WAIT_AVAILABLE |
			 DRM_SYNCOBJ_WAIT_FLAGS_WAIT_DEADLINE;

	if (args->flags & ~possible_flags)
		return -EINVAL;

	if (count == 0)
		return 0;

	ret = drm_syncobj_array_find(file_private,
				     u64_to_user_ptr(args->handles),
				     count,
				     stack_syncobjs,
				     ARRAY_SIZE(stack_syncobjs),
				     &syncobjs);
	if (ret < 0)
		return ret;

	if (args->flags & DRM_SYNCOBJ_WAIT_FLAGS_WAIT_DEADLINE) {
		deadline = ns_to_ktime(args->deadline_nsec);
		pdeadline = &deadline;
	}

	timeout = drm_syncobj_array_wait_timeout(syncobjs,
						 u64_to_user_ptr(args->points),
						 count,
						 args->flags,
						 drm_timeout_abs_to_jiffies(args->timeout_nsec),
						 &first,
						 pdeadline);

	drm_syncobj_array_free(syncobjs, count, stack_syncobjs);

	if (timeout < 0)
		return timeout;

	args->first_signaled = first;

	return 0;
}

static void syncobj_eventfd_entry_fence_func(struct dma_fence *fence,
					     struct dma_fence_cb *cb)
{
	struct syncobj_eventfd_entry *entry =
		container_of(cb, struct syncobj_eventfd_entry, fence_cb);

	eventfd_signal(entry->ev_fd_ctx);
	syncobj_eventfd_entry_free(entry);
}

static void
syncobj_eventfd_entry_func(struct drm_syncobj *syncobj,
			   struct syncobj_eventfd_entry *entry)
{
	int ret;
	struct dma_fence *fence;

	/* This happens inside the syncobj lock */
	fence = dma_fence_get(rcu_dereference_protected(syncobj->fence, 1));
	if (!fence)
		return;

	ret = dma_fence_chain_find_seqno(&fence, entry->point);
	if (ret != 0) {
		/* The given seqno has not been submitted yet. */
		dma_fence_put(fence);
		return;
	} else if (!fence) {
		/* If dma_fence_chain_find_seqno returns 0 but sets the fence
		 * to NULL, it implies that the given seqno is signaled and a
		 * later seqno has already been submitted. Assign a stub fence
		 * so that the eventfd still gets signaled below.
		 */
		fence = dma_fence_get_stub();
	}

	list_del_init(&entry->node);
	entry->fence = fence;

	if (entry->flags & DRM_SYNCOBJ_WAIT_FLAGS_WAIT_AVAILABLE) {
		eventfd_signal(entry->ev_fd_ctx);
		syncobj_eventfd_entry_free(entry);
	} else {
		ret = dma_fence_add_callback(fence, &entry->fence_cb,
					     syncobj_eventfd_entry_fence_func);
		if (ret == -ENOENT) {
			eventfd_signal(entry->ev_fd_ctx);
			syncobj_eventfd_entry_free(entry);
		}
	}
}

int
drm_syncobj_eventfd_ioctl(struct drm_device *dev, void *data,
			  struct drm_file *file_private)
{
	struct drm_syncobj_eventfd *args = data;
	struct drm_syncobj *syncobj;
	struct eventfd_ctx *ev_fd_ctx;
	struct syncobj_eventfd_entry *entry;
	int ret;

	if (!drm_core_check_feature(dev, DRIVER_SYNCOBJ_TIMELINE))
		return -EOPNOTSUPP;

	if (args->flags & ~DRM_SYNCOBJ_WAIT_FLAGS_WAIT_AVAILABLE)
		return -EINVAL;

	if (args->pad)
		return -EINVAL;

	syncobj = drm_syncobj_find(file_private, args->handle);
	if (!syncobj)
		return -ENOENT;

	ev_fd_ctx = eventfd_ctx_fdget(args->fd);
	if (IS_ERR(ev_fd_ctx)) {
		ret = PTR_ERR(ev_fd_ctx);
		goto err_fdget;
	}

	entry = kzalloc(sizeof(*entry), GFP_KERNEL);
	if (!entry) {
		ret = -ENOMEM;
		goto err_kzalloc;
	}
	entry->syncobj = syncobj;
	entry->ev_fd_ctx = ev_fd_ctx;
	entry->point = args->point;
	entry->flags = args->flags;

	drm_syncobj_add_eventfd(syncobj, entry);
	drm_syncobj_put(syncobj);

	return 0;

err_kzalloc:
	eventfd_ctx_put(ev_fd_ctx);
err_fdget:
	drm_syncobj_put(syncobj);
	return ret;
}

int
drm_syncobj_reset_ioctl(struct drm_device *dev, void *data,
			struct drm_file *file_private)
{
	struct drm_syncobj_array *args = data;
	struct drm_syncobj *stack_syncobjs[4];
	struct drm_syncobj **syncobjs;
	uint32_t i;
	int ret;

	if (!drm_core_check_feature(dev, DRIVER_SYNCOBJ))
		return -EOPNOTSUPP;

	if (args->pad != 0)
		return -EINVAL;

	if (args->count_handles == 0)
		return -EINVAL;

	ret = drm_syncobj_array_find(file_private,
				     u64_to_user_ptr(args->handles),
				     args->count_handles,
				     stack_syncobjs,
				     ARRAY_SIZE(stack_syncobjs),
				     &syncobjs);
	if (ret < 0)
		return ret;

	for (i = 0; i < args->count_handles; i++)
		drm_syncobj_replace_fence(syncobjs[i], NULL);

	drm_syncobj_array_free(syncobjs, args->count_handles, stack_syncobjs);

	return 0;
}

int
drm_syncobj_signal_ioctl(struct drm_device *dev, void *data,
			 struct drm_file *file_private)
{
	struct drm_syncobj_array *args = data;
	struct drm_syncobj *stack_syncobjs[4];
	struct drm_syncobj **syncobjs;
	uint32_t i;
	int ret;

	if (!drm_core_check_feature(dev, DRIVER_SYNCOBJ))
		return -EOPNOTSUPP;

	if (args->pad != 0)
		return -EINVAL;

	if (args->count_handles == 0)
		return -EINVAL;

	ret = drm_syncobj_array_find(file_private,
				     u64_to_user_ptr(args->handles),
				     args->count_handles,
				     stack_syncobjs,
				     ARRAY_SIZE(stack_syncobjs),
				     &syncobjs);
	if (ret < 0)
		return ret;

	for (i = 0; i < args->count_handles; i++) {
		ret = drm_syncobj_assign_null_handle(syncobjs[i]);
		if (ret < 0)
			break;
	}

	drm_syncobj_array_free(syncobjs, args->count_handles, stack_syncobjs);

	return ret;
}

int
drm_syncobj_timeline_signal_ioctl(struct drm_device *dev, void *data,
				  struct drm_file *file_private)
{
	struct drm_syncobj_timeline_array *args = data;
	uint64_t __user *points = u64_to_user_ptr(args->points);
	uint32_t i, j, count = args->count_handles;
	struct drm_syncobj *stack_syncobjs[4];
	struct drm_syncobj **syncobjs;
	struct dma_fence_chain **chains;
	int ret;

	if (!drm_core_check_feature(dev, DRIVER_SYNCOBJ_TIMELINE))
		return -EOPNOTSUPP;

	if (args->flags != 0)
		return -EINVAL;

	if (args->count_handles == 0)
		return -EINVAL;

	if (!access_ok(points, count * sizeof(*points)))
		return -EFAULT;

	ret = drm_syncobj_array_find(file_private,
				     u64_to_user_ptr(args->handles),
				     count,
				     stack_syncobjs,
				     ARRAY_SIZE(stack_syncobjs),
				     &syncobjs);
	if (ret < 0)
		return ret;

	chains = kmalloc_array(count, sizeof(void *), GFP_KERNEL);
	if (!chains) {
		ret = -ENOMEM;
		goto out;
	}
	for (i = 0; i < count; i++) {
		chains[i] = dma_fence_chain_alloc();
		if (!chains[i]) {
			for (j = 0; j < i; j++)
				dma_fence_chain_free(chains[j]);
			ret = -ENOMEM;
			goto err_chains;
		}
	}

	for (i = 0; i < count; i++) {
		struct dma_fence *fence = dma_fence_get_stub();
		u64 point = 0;

		if (points && __get_user(point, points++)) {
			ret =  -EFAULT;
			for (j = i; j < count; j++)
				dma_fence_chain_free(chains[j]);
			goto err_chains;
		}

		drm_syncobj_add_point(syncobjs[i], chains[i], fence, point);
		dma_fence_put(fence);
	}
err_chains:
	kfree(chains);
out:
	drm_syncobj_array_free(syncobjs, count, stack_syncobjs);

	return ret;
}

int drm_syncobj_query_ioctl(struct drm_device *dev, void *data,
							struct drm_file *file_private)
{
	struct drm_syncobj_timeline_array *args = data;
	struct drm_syncobj *stack_syncobjs[4];
	struct drm_syncobj **syncobjs;
	uint64_t __user *points = u64_to_user_ptr(args->points);
	uint32_t i;
	int ret;

	if (!drm_core_check_feature(dev, DRIVER_SYNCOBJ_TIMELINE))
		return -EOPNOTSUPP;

	if (args->flags & ~DRM_SYNCOBJ_QUERY_FLAGS_LAST_SUBMITTED)
		return -EINVAL;

	if (args->count_handles == 0)
		return -EINVAL;

	if (!points || !access_ok(points, args->count_handles * sizeof(*points)))
		return -EFAULT;

	ret = drm_syncobj_array_find(file_private,
								 u64_to_user_ptr(args->handles),
								 args->count_handles,
							  stack_syncobjs,
							  ARRAY_SIZE(stack_syncobjs),
								 &syncobjs);
	if (ret < 0)
		return ret;

	for (i = 0; i < args->count_handles; i++) {
		struct dma_fence_chain *chain;
		struct dma_fence *fence;
		uint64_t point = 0;  /* Initialize with default value */

		if (!syncobjs[i]) {
			/* Handle NULL syncobj - this shouldn't happen but be defensive */
			goto put_point;
		}

		fence = drm_syncobj_fence_get(syncobjs[i]);
		if (!fence) {
			/* No fence available */
			goto put_point;
		}

		chain = to_dma_fence_chain(fence);
		if (chain) {
			struct dma_fence *iter = NULL, *last_signaled = NULL;
			bool has_unordered_points = false;  /* Properly declared at block start */

			/* Use the last submitted point if requested */
			if (args->flags & DRM_SYNCOBJ_QUERY_FLAGS_LAST_SUBMITTED) {
				point = fence->seqno;
				dma_fence_put(fence);
				goto put_point;
			}

			/* Otherwise find the signaled point */
			last_signaled = dma_fence_get(fence);
			if (!last_signaled) {
				/* This should never happen, but be defensive */
				dma_fence_put(fence);
				goto put_point;
			}

			dma_fence_chain_for_each(iter, fence) {
				if (!iter || iter->context != fence->context) {
					/* Timeline has unordered points */
					has_unordered_points = true;
					if (iter)
						dma_fence_put(iter);
					break;
				}

				/* Update last_signaled with current iteration */
				dma_fence_put(last_signaled);
				last_signaled = dma_fence_get(iter);
				if (!last_signaled) {
					/* Unexpected error - break out safely */
					has_unordered_points = true;
					break;
				}
			}

			/* Determine point based on signaled state */
			if (last_signaled) {
				if (dma_fence_is_signaled(last_signaled)) {
					point = last_signaled->seqno;
				} else {
					struct dma_fence_chain *last_chain = to_dma_fence_chain(last_signaled);
					if (last_chain) {
						point = last_chain->prev_seqno;
					} else {
						/* Fallback for unhandled cases */
						point = has_unordered_points ? last_signaled->seqno : 0;
					}
				}

				/* Clean up the last_signaled reference */
				dma_fence_put(last_signaled);
			}

			dma_fence_put(fence);
		} else {
			/* Not a chain fence */
			dma_fence_put(fence);
		}

		put_point:
		if (__put_user(point, points++)) {
			ret = -EFAULT;
			break;
		}
	}

	drm_syncobj_array_free(syncobjs, args->count_handles, stack_syncobjs);
	return ret;
}
