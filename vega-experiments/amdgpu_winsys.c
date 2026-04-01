/*
 * Copyright © 2009 Corbin Simpson <MostAwesomeDude@gmail.com>
 * Copyright © 2009 Joakim Sindholt <opensource@zhasha.com>
 * Copyright © 2011 Marek Olšák <maraeo@gmail.com>
 * Copyright © 2015 Advanced Micro Devices, Inc.
 *
 * SPDX-License-Identifier: MIT
 */

#include "amdgpu_cs.h"

#include "util/os_drm.h"
#include "util/os_file.h"
#include "util/os_misc.h"
#include "util/u_cpu_detect.h"
#include "util/u_hash_table.h"
#include "util/hash_table.h"
#include "util/log.h"
#include "util/thread_sched.h"
#include "util/xmlconfig.h"
#include "drm-uapi/amdgpu_drm.h"
#include <xf86drm.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "sid.h"

char amdgpu_userq_str[AMDGPU_MAX_QUEUES][8] = {
   "gfx",
   "gfx_hi",
   "comp",
   "sdma"
};

static struct hash_table *dev_tab = NULL;
static simple_mtx_t dev_tab_mutex = SIMPLE_MTX_INITIALIZER;

#if MESA_DEBUG
DEBUG_GET_ONCE_BOOL_OPTION(all_bos, "RADEON_ALL_BOS", false)
#endif

static void
amdgpu_destroy_kms_handles(struct amdgpu_screen_winsys *sws)
{
   if (!sws || !sws->kms_handles)
      return;

   if (sws->fd >= 0) {
      struct drm_gem_close args = {0};

      hash_table_foreach(sws->kms_handles, entry) {
         args.handle = (uint32_t)(uintptr_t)entry->data;
         (void)drm_ioctl(sws->fd, DRM_IOCTL_GEM_CLOSE, &args);
      }
   }

   _mesa_hash_table_destroy(sws->kms_handles, NULL);
   sws->kms_handles = NULL;
}

static void
amdgpu_screen_winsys_remove_locked(struct amdgpu_screen_winsys *sws)
{
   struct amdgpu_screen_winsys **sws_iter = &sws->aws->sws_list;

   while (*sws_iter) {
      if (*sws_iter == sws) {
         *sws_iter = sws->next;
         sws->next = NULL;
         return;
      }

      sws_iter = &(*sws_iter)->next;
   }
}

static bool
amdgpu_device_initialize_preferred_fd(int fd, bool is_virtio,
                                      uint32_t *drm_major, uint32_t *drm_minor,
                                      ac_drm_device **dev)
{
   int init_result;

   if (fd < 0 || !drm_major || !drm_minor || !dev)
      return false;

   *dev = NULL;

   if (drmGetNodeTypeFromFd(fd) != DRM_NODE_RENDER) {
      char *render_device = drmGetRenderDeviceNameFromFd(fd);

      if (render_device) {
         const int render_fd = open(render_device, O_RDWR | O_CLOEXEC);

         free(render_device);

         if (render_fd >= 0) {
            init_result = ac_drm_device_initialize(render_fd, is_virtio,
                                                   drm_major, drm_minor, dev);
            if (init_result == 0) {
               const int device_fd = ac_drm_device_get_fd(*dev);

               /* If libdrm reused an existing device handle whose internal FD
                * is not our temporary render-node FD, we still own render_fd
                * and must close it here.
                */
               if (device_fd != render_fd)
                  close(render_fd);

               return true;
            }

            mesa_logd("amdgpu: amd%s_device_initialize failed for render device.\n",
                      is_virtio ? "vgpu" : "gpu");
            close(render_fd);
         }
      }
   }

   init_result = ac_drm_device_initialize(fd, is_virtio, drm_major, drm_minor, dev);
   if (init_result != 0) {
      mesa_loge("amdgpu: amd%s_device_initialize failed.\n",
                is_virtio ? "vgpu" : "gpu");
      return false;
   }

   return true;
}

static void
amdgpu_winsys_early_cleanup(struct amdgpu_winsys *aws)
{
   if (!aws)
      return;

   if (aws->vm_timeline_syncobj && aws->dev)
      ac_drm_cs_destroy_syncobj(aws->dev, aws->vm_timeline_syncobj);

   if (aws->addrlib) {
      ac_addrlib_destroy(aws->addrlib);
      aws->addrlib = NULL;
   }

   if (aws->bo_export_table) {
      _mesa_hash_table_destroy(aws->bo_export_table, NULL);
      aws->bo_export_table = NULL;
   }

   simple_mtx_destroy(&aws->sws_list_lock);
#if MESA_DEBUG
   simple_mtx_destroy(&aws->global_bo_list_lock);
#endif
   simple_mtx_destroy(&aws->bo_fence_lock);
   simple_mtx_destroy(&aws->stats_lock);
   simple_mtx_destroy(&aws->bo_export_table_lock);
   simple_mtx_destroy(&aws->vm_ioctl_lock);

   if (aws->dev) {
      ac_drm_device_deinitialize(aws->dev);
      aws->dev = NULL;
   }

   FREE(aws);
}

/* Helper function to do the ioctls needed for setup and init. */
static bool
do_winsys_init(struct amdgpu_winsys *aws,
               const struct pipe_screen_config *config,
               int fd)
{
   const char *const r600_debug = debug_get_option("R600_DEBUG", "");
   const char *const amd_debug = debug_get_option("AMD_DEBUG", "");

   if (ac_query_gpu_info(fd, aws->dev, &aws->info, false) != AC_QUERY_GPU_INFO_SUCCESS) {
      mesa_loge("amdgpu: ac_query_gpu_info failed.\n");
      goto fail;
   }

   aws->addrlib = ac_addrlib_create(&aws->info, &aws->info.max_alignment);
   if (!aws->addrlib) {
      mesa_loge("amdgpu: Cannot create addrlib.\n");
      goto fail;
   }

   aws->check_vm = strstr(r600_debug, "check_vm") != NULL ||
                   strstr(amd_debug, "check_vm") != NULL;
   aws->noop_cs = aws->info.family_overridden ||
                  debug_get_bool_option("RADEON_NOOP", false);
#if MESA_DEBUG
   aws->debug_all_bos = debug_get_option_all_bos();
#endif
   aws->reserve_vmid = strstr(r600_debug, "reserve_vmid") != NULL ||
                       strstr(amd_debug, "reserve_vmid") != NULL ||
                       strstr(amd_debug, "sqtt") != NULL;
   aws->zero_all_vram_allocs = strstr(r600_debug, "zerovram") != NULL ||
                               driQueryOptionb(config->options, "radeonsi_zerovram");
   aws->userq_job_log = strstr(amd_debug, "userqjoblog") != NULL;

   for (unsigned i = 0; i < ARRAY_SIZE(aws->queues); i++)
      simple_mtx_init(&aws->queues[i].userq.lock, mtx_plain);

   /* TODO: Enable this once the kernel handles it efficiently. */
   if (!aws->info.userq_ip_mask)
      aws->info.has_vm_always_valid = false;

   if (aws->userq_job_log)
      amdgpu_userq_start_job_log_thread(aws);

   return true;

fail:
   if (aws->addrlib) {
      ac_addrlib_destroy(aws->addrlib);
      aws->addrlib = NULL;
   }

   return false;
}

static void
do_winsys_deinit(struct amdgpu_winsys *aws)
{
   if (aws->reserve_vmid && aws->dev)
      ac_drm_vm_unreserve_vmid(aws->dev, 0);

   if (aws->userq_job_log) {
      aws->userq_job_log = false;
      pthread_join(aws->userq_job_log_thread, NULL);
   }

   for (unsigned i = 0; i < ARRAY_SIZE(aws->queues); i++) {
      for (unsigned j = 0; j < ARRAY_SIZE(aws->queues[i].fences); j++)
         amdgpu_fence_reference(&aws->queues[i].fences[j], NULL);

      amdgpu_userq_deinit(aws, &aws->queues[i].userq);
      simple_mtx_destroy(&aws->queues[i].userq.lock);

      amdgpu_ctx_reference(&aws->queues[i].last_ctx, NULL);
   }

   if (util_queue_is_initialized(&aws->cs_queue))
      util_queue_destroy(&aws->cs_queue);

   if (aws->bo_slabs.groups)
      pb_slabs_deinit(&aws->bo_slabs);
   pb_cache_deinit(&aws->bo_cache);

   if (aws->bo_export_table)
      _mesa_hash_table_destroy(aws->bo_export_table, NULL);

   simple_mtx_destroy(&aws->sws_list_lock);
#if MESA_DEBUG
   simple_mtx_destroy(&aws->global_bo_list_lock);
#endif
   simple_mtx_destroy(&aws->bo_export_table_lock);

   if (aws->addrlib)
      ac_addrlib_destroy(aws->addrlib);

   if (aws->vm_timeline_syncobj && aws->dev)
      ac_drm_cs_destroy_syncobj(aws->dev, aws->vm_timeline_syncobj);

   if (aws->dev)
      ac_drm_device_deinitialize(aws->dev);

   simple_mtx_destroy(&aws->vm_ioctl_lock);
   simple_mtx_destroy(&aws->bo_fence_lock);
   simple_mtx_destroy(&aws->stats_lock);

   FREE(aws);
}

static void
amdgpu_winsys_destroy_locked(struct radeon_winsys *rws, bool locked)
{
   struct amdgpu_screen_winsys *sws = amdgpu_screen_winsys(rws);
   struct amdgpu_winsys *aws = sws->aws;
   bool destroy;

   /* When the reference counter drops to zero, remove the device pointer
    * from the table.
    * This must happen while the mutex is locked, so that
    * amdgpu_winsys_create in another thread doesn't get the winsys
    * from the table when the counter drops to 0.
    */
   if (!locked)
      simple_mtx_lock(&dev_tab_mutex);

   destroy = pipe_reference(&aws->reference, NULL);
   if (destroy && dev_tab) {
      if (aws->dev)
         _mesa_hash_table_remove_key(dev_tab,
                                     (void *)ac_drm_device_get_cookie(aws->dev));

      if (_mesa_hash_table_num_entries(dev_tab) == 0) {
         _mesa_hash_table_destroy(dev_tab, NULL);
         dev_tab = NULL;
      }
   }

   if (!locked)
      simple_mtx_unlock(&dev_tab_mutex);

   if (sws->fd >= 0 && sws->fd != aws->fd)
      close(sws->fd);

   if (destroy)
      do_winsys_deinit(aws);

   FREE(rws);
}

static void amdgpu_winsys_destroy(struct radeon_winsys *rws)
{
   amdgpu_winsys_destroy_locked(rws, false);
}

static void amdgpu_winsys_query_info(struct radeon_winsys *rws, struct radeon_info *info)
{
   struct amdgpu_winsys *aws = amdgpu_winsys(rws);

   *info = aws->info;
}

static bool amdgpu_cs_request_feature(struct radeon_cmdbuf *rcs,
                                      enum radeon_feature_id fid,
                                      bool enable)
{
   (void)rcs;
   (void)fid;
   (void)enable;
   return false;
}

static uint64_t amdgpu_query_value(struct radeon_winsys *rws,
                                   enum radeon_value_id value)
{
   struct amdgpu_winsys *aws = amdgpu_winsys(rws);
   struct amdgpu_heap_info heap = {0};
   uint64_t retval = 0;

   switch (value) {
   case RADEON_REQUESTED_VRAM_MEMORY:
      return aws->allocated_vram;
   case RADEON_REQUESTED_GTT_MEMORY:
      return aws->allocated_gtt;
   case RADEON_MAPPED_VRAM:
      return aws->mapped_vram;
   case RADEON_MAPPED_GTT:
      return aws->mapped_gtt;
   case RADEON_SLAB_WASTED_VRAM:
      return aws->slab_wasted_vram;
   case RADEON_SLAB_WASTED_GTT:
      return aws->slab_wasted_gtt;
   case RADEON_BUFFER_WAIT_TIME_NS:
      return aws->buffer_wait_time;
   case RADEON_NUM_MAPPED_BUFFERS:
      return aws->num_mapped_buffers;
   case RADEON_TIMESTAMP:
      ac_drm_query_info(aws->dev, AMDGPU_INFO_TIMESTAMP, 8, &retval);
      return retval;
   case RADEON_NUM_GFX_IBS:
      return aws->num_gfx_IBs;
   case RADEON_NUM_SDMA_IBS:
      return aws->num_sdma_IBs;
   case RADEON_GFX_BO_LIST_COUNTER:
      return aws->gfx_bo_list_counter;
   case RADEON_GFX_IB_SIZE_COUNTER:
      return aws->gfx_ib_size_counter;
   case RADEON_NUM_BYTES_MOVED:
      ac_drm_query_info(aws->dev, AMDGPU_INFO_NUM_BYTES_MOVED, 8, &retval);
      return retval;
   case RADEON_NUM_EVICTIONS:
      ac_drm_query_info(aws->dev, AMDGPU_INFO_NUM_EVICTIONS, 8, &retval);
      return retval;
   case RADEON_NUM_VRAM_CPU_PAGE_FAULTS:
      ac_drm_query_info(aws->dev, AMDGPU_INFO_NUM_VRAM_CPU_PAGE_FAULTS, 8, &retval);
      return retval;
   case RADEON_VRAM_USAGE:
      ac_drm_query_heap_info(aws->dev, AMDGPU_GEM_DOMAIN_VRAM, 0, &heap);
      return heap.heap_usage;
   case RADEON_VRAM_VIS_USAGE:
      ac_drm_query_heap_info(aws->dev, AMDGPU_GEM_DOMAIN_VRAM,
                             AMDGPU_GEM_CREATE_CPU_ACCESS_REQUIRED, &heap);
      return heap.heap_usage;
   case RADEON_GTT_USAGE:
      ac_drm_query_heap_info(aws->dev, AMDGPU_GEM_DOMAIN_GTT, 0, &heap);
      return heap.heap_usage;
   case RADEON_GPU_TEMPERATURE:
      ac_drm_query_sensor_info(aws->dev, AMDGPU_INFO_SENSOR_GPU_TEMP, 4, &retval);
      return retval;
   case RADEON_CURRENT_SCLK:
      ac_drm_query_sensor_info(aws->dev, AMDGPU_INFO_SENSOR_GFX_SCLK, 4, &retval);
      return retval;
   case RADEON_CURRENT_MCLK:
      ac_drm_query_sensor_info(aws->dev, AMDGPU_INFO_SENSOR_GFX_MCLK, 4, &retval);
      return retval;
   case RADEON_CS_THREAD_TIME:
      return util_queue_get_thread_time_nano(&aws->cs_queue, 0);
   }
   return 0;
}

static bool amdgpu_read_registers(struct radeon_winsys *rws,
                                  unsigned reg_offset,
                                  unsigned num_registers, uint32_t *out)
{
   struct amdgpu_winsys *aws = amdgpu_winsys(rws);

   return ac_drm_read_mm_registers(aws->dev, reg_offset / 4, num_registers,
                                   0xffffffff, 0, out) == 0;
}

static bool amdgpu_winsys_unref(struct radeon_winsys *rws)
{
   struct amdgpu_screen_winsys *sws = amdgpu_screen_winsys(rws);
   struct amdgpu_winsys *aws = sws->aws;
   bool ret;

   simple_mtx_lock(&aws->sws_list_lock);

   ret = pipe_reference(&sws->reference, NULL);
   if (ret)
      amdgpu_screen_winsys_remove_locked(sws);

   simple_mtx_unlock(&aws->sws_list_lock);

   if (ret)
      amdgpu_destroy_kms_handles(sws);

   return ret;
}

static void amdgpu_pin_threads_to_L3_cache(struct radeon_winsys *rws,
                                           unsigned cpu)
{
   struct amdgpu_winsys *aws = amdgpu_winsys(rws);

   util_thread_sched_apply_policy(aws->cs_queue.threads[0],
                                  UTIL_THREAD_DRIVER_SUBMIT, cpu, NULL);
}

static uint32_t kms_handle_hash(const void *key)
{
   const struct amdgpu_bo_real *bo = key;

   return bo->kms_handle;
}

static bool kms_handle_equals(const void *a, const void *b)
{
   return a == b;
}

static bool amdgpu_cs_is_secure(struct radeon_cmdbuf *rcs)
{
   struct amdgpu_cs *acs = amdgpu_cs(rcs);
   return amdgpu_csc_get_current(acs)->secure;
}

static uint32_t
radeon_to_amdgpu_pstate(enum radeon_ctx_pstate pstate)
{
   switch (pstate) {
   case RADEON_CTX_PSTATE_NONE:
      return AMDGPU_CTX_STABLE_PSTATE_NONE;
   case RADEON_CTX_PSTATE_STANDARD:
      return AMDGPU_CTX_STABLE_PSTATE_STANDARD;
   case RADEON_CTX_PSTATE_MIN_SCLK:
      return AMDGPU_CTX_STABLE_PSTATE_MIN_SCLK;
   case RADEON_CTX_PSTATE_MIN_MCLK:
      return AMDGPU_CTX_STABLE_PSTATE_MIN_MCLK;
   case RADEON_CTX_PSTATE_PEAK:
      return AMDGPU_CTX_STABLE_PSTATE_PEAK;
   default:
      UNREACHABLE("Invalid pstate");
   }
}

static bool
amdgpu_cs_set_pstate(struct radeon_cmdbuf *rcs, enum radeon_ctx_pstate pstate)
{
   struct amdgpu_cs *acs = amdgpu_cs(rcs);

   uint32_t amdgpu_pstate = radeon_to_amdgpu_pstate(pstate);
   return ac_drm_cs_ctx_stable_pstate(acs->aws->dev, acs->ctx->ctx_handle,
      AMDGPU_CTX_OP_SET_STABLE_PSTATE, amdgpu_pstate, NULL) == 0;
}

static bool
are_file_descriptions_equal(int fd1, int fd2)
{
   static atomic_flag logged = ATOMIC_FLAG_INIT;

   if (fd1 < 0 || fd2 < 0)
      return false;

   if (fd1 == fd2)
      return true;

   const int r = os_same_file_description(fd1, fd2);

   if (r == 0)
      return true;

   if (r < 0) {
      if (!atomic_flag_test_and_set_explicit(&logged, memory_order_relaxed)) {
         os_log_message("amdgpu: os_same_file_description couldn't "
                        "determine if two DRM fds reference the same "
                        "file description.\n"
                        "If they do, bad things may happen!\n");
      }
   }

   return false;
}

static int
amdgpu_drm_winsys_get_fd(struct radeon_winsys *rws)
{
   struct amdgpu_screen_winsys *sws = amdgpu_screen_winsys(rws);

   return sws->fd;
}

PUBLIC struct radeon_winsys *
amdgpu_winsys_create(int fd, const struct pipe_screen_config *config,
                     radeon_screen_create_t screen_create, bool is_virtio)
{
   struct amdgpu_screen_winsys *sws;
   struct amdgpu_winsys *aws;
   ac_drm_device *dev = NULL;
   uint32_t drm_major = 0;
   uint32_t drm_minor = 0;
   int r = 0;

   sws = CALLOC_STRUCT(amdgpu_screen_winsys);
   if (!sws)
      return NULL;

   pipe_reference_init(&sws->reference, 1);
   sws->fd = -1;

   /* Look up the winsys from the dev table. */
   simple_mtx_lock(&dev_tab_mutex);
   if (!dev_tab) {
      dev_tab = util_hash_table_create_ptr_keys();
      if (!dev_tab)
         goto fail_locked_sws;
   }

   /* Initialize the amdgpu device. This should always return the same pointer
    * for the same fd. Prefer a render node when possible to avoid issues with
    * unauthenticated card-node FDs.
    */
   if (!amdgpu_device_initialize_preferred_fd(fd, is_virtio,
                                              &drm_major, &drm_minor, &dev))
      goto fail_locked_sws;

   /* Lookup a winsys if we have already created one for this device. */
   aws = util_hash_table_get(dev_tab, (void *)ac_drm_device_get_cookie(dev));
   if (aws) {
      struct amdgpu_screen_winsys *sws_iter;

      /* Release the device handle, because we don't need it anymore.
       * This function is returning an existing winsys instance, which
       * has its own device handle.
       */
      ac_drm_device_deinitialize((void *)dev);
      dev = NULL;

      simple_mtx_lock(&aws->sws_list_lock);
      for (sws_iter = aws->sws_list; sws_iter; sws_iter = sws_iter->next) {
         if (are_file_descriptions_equal(sws_iter->fd, fd)) {
            FREE(sws);
            sws = sws_iter;
            pipe_reference(NULL, &sws->reference);
            simple_mtx_unlock(&aws->sws_list_lock);
            goto unlock;
         }
      }
      simple_mtx_unlock(&aws->sws_list_lock);

      sws->kms_handles = _mesa_hash_table_create(NULL, kms_handle_hash,
                                                 kms_handle_equals);
      if (!sws->kms_handles)
         goto fail_locked_sws;

      pipe_reference(NULL, &aws->reference);
      sws->aws = aws;
   } else {
      /* Create a new winsys. */
      aws = CALLOC_STRUCT(amdgpu_winsys);
      if (!aws)
         goto fail_locked_dev;

      sws->aws = aws;
      aws->dev = dev;

      pipe_reference_init(&aws->reference, 1);
#if MESA_DEBUG
      list_inithead(&aws->global_bo_list);
#endif
      (void)simple_mtx_init(&aws->sws_list_lock, mtx_plain);
#if MESA_DEBUG
      (void)simple_mtx_init(&aws->global_bo_list_lock, mtx_plain);
#endif
      (void)simple_mtx_init(&aws->bo_fence_lock, mtx_plain);
      (void)simple_mtx_init(&aws->stats_lock, mtx_plain);
      (void)simple_mtx_init(&aws->bo_export_table_lock, mtx_plain);
      (void)simple_mtx_init(&aws->vm_ioctl_lock, mtx_plain);

      aws->bo_export_table = util_hash_table_create_ptr_keys();
      if (!aws->bo_export_table)
         goto fail_locked_new_aws;

      /* The device fd might be different from the one we passed because of
       * libdrm_amdgpu device dedup logic. This can happen if radv is initialized
       * first.
       * Get the correct fd or the buffer sharing will not work (see #3424).
       */
      aws->fd = ac_drm_device_get_fd(dev);
      if (!are_file_descriptions_equal(aws->fd, fd)) {
         sws->kms_handles = _mesa_hash_table_create(NULL, kms_handle_hash,
                                                    kms_handle_equals);
         if (!sws->kms_handles)
            goto fail_locked_new_aws;
      } else {
         sws->fd = aws->fd;
      }

      aws->info.drm_major = drm_major;
      aws->info.drm_minor = drm_minor;
      aws->info.is_virtio = is_virtio;

      if (ac_drm_cs_create_syncobj2(aws->dev, 0, &aws->vm_timeline_syncobj))
         goto fail_locked_new_aws;

      /* Only aws and buffer functions are used. */
      aws->dummy_sws.aws = aws;
      amdgpu_bo_init_functions(&aws->dummy_sws);

      if (!do_winsys_init(aws, config, fd))
         goto fail_locked_new_aws;

      /* Create managers. */
      pb_cache_init(&aws->bo_cache, RADEON_NUM_HEAPS,
                    500000, aws->check_vm ? 1.0f : 1.5f, 0,
                    ((uint64_t)aws->info.vram_size_kb + aws->info.gart_size_kb) * 1024 / 8,
                    offsetof(struct amdgpu_bo_real_reusable, cache_entry), aws,
                    /* Cast to void* because one of the function parameters
                     * is a struct pointer instead of void*. */
                    (void *)amdgpu_bo_destroy, (void *)amdgpu_bo_can_reclaim);

      if (!pb_slabs_init(&aws->bo_slabs,
                         8,  /* min slab entry size: 256 bytes */
                         20, /* max slab entry size: 1 MB (slab size = 2 MB) */
                         RADEON_NUM_HEAPS, true,
                         aws,
                         amdgpu_bo_can_reclaim_slab,
                         amdgpu_bo_slab_alloc,
                         /* Cast to void* because one of the function parameters
                          * is a struct pointer instead of void*. */
                         (void *)amdgpu_bo_slab_free)) {
         amdgpu_destroy_kms_handles(sws);
         amdgpu_winsys_destroy_locked(&sws->base, true);
         simple_mtx_unlock(&dev_tab_mutex);
         return NULL;
      }

      aws->info.min_alloc_size = 1u << aws->bo_slabs.min_order;

      if (!util_queue_init(&aws->cs_queue, "cs", 8, 1,
                           UTIL_QUEUE_INIT_RESIZE_IF_FULL, NULL)) {
         amdgpu_destroy_kms_handles(sws);
         amdgpu_winsys_destroy_locked(&sws->base, true);
         simple_mtx_unlock(&dev_tab_mutex);
         return NULL;
      }

      if (!_mesa_hash_table_insert(dev_tab,
                                   (void *)ac_drm_device_get_cookie(dev), aws)) {
         amdgpu_destroy_kms_handles(sws);
         amdgpu_winsys_destroy_locked(&sws->base, true);
         simple_mtx_unlock(&dev_tab_mutex);
         return NULL;
      }

      if (aws->reserve_vmid) {
         r = ac_drm_vm_reserve_vmid(aws->dev, 0);
         if (r) {
            amdgpu_destroy_kms_handles(sws);
            amdgpu_winsys_destroy_locked(&sws->base, true);
            simple_mtx_unlock(&dev_tab_mutex);
            return NULL;
         }
      }
   }

   if (sws->fd < 0) {
      sws->fd = os_dupfd_cloexec(fd);
      if (sws->fd < 0) {
         amdgpu_destroy_kms_handles(sws);
         amdgpu_winsys_destroy_locked(&sws->base, true);
         simple_mtx_unlock(&dev_tab_mutex);
         return NULL;
      }
   }

   sws->aws = aws;

   /* Set functions. */
   sws->base.unref = amdgpu_winsys_unref;
   sws->base.destroy = amdgpu_winsys_destroy;
   sws->base.get_fd = amdgpu_drm_winsys_get_fd;
   sws->base.query_info = amdgpu_winsys_query_info;
   sws->base.cs_request_feature = amdgpu_cs_request_feature;
   sws->base.query_value = amdgpu_query_value;
   sws->base.read_registers = amdgpu_read_registers;
   sws->base.pin_threads_to_L3_cache = amdgpu_pin_threads_to_L3_cache;
   sws->base.cs_is_secure = amdgpu_cs_is_secure;
   sws->base.cs_set_pstate = amdgpu_cs_set_pstate;

   amdgpu_bo_init_functions(sws);
   amdgpu_cs_init_functions(sws);
   amdgpu_userq_init_functions(sws);
   amdgpu_surface_init_functions(sws);

   simple_mtx_lock(&aws->sws_list_lock);
   sws->next = aws->sws_list;
   aws->sws_list = sws;
   simple_mtx_unlock(&aws->sws_list_lock);

   /* Create the screen at the end. The winsys must be initialized
    * completely.
    *
    * Alternatively, we could create the screen based on "ws->gen"
    * and link all drivers into one binary blob. */
   sws->base.screen = screen_create(&sws->base, config);
   if (!sws->base.screen) {
      const bool last_sws_ref = amdgpu_winsys_unref(&sws->base);

      if (last_sws_ref) {
         amdgpu_winsys_destroy_locked(&sws->base, true);
      } else {
         /* This should never happen, but don't abort the process in production.
          * Detach the broken winsys so it can't be reused, then leak it rather
          * than crashing the game.
          */
         simple_mtx_lock(&aws->sws_list_lock);
         amdgpu_screen_winsys_remove_locked(sws);
         simple_mtx_unlock(&aws->sws_list_lock);

         os_log_message("amdgpu: screen_create failed while winsys still had "
                        "unexpected extra references; detached leaked winsys "
                        "instance.\n");
      }

      simple_mtx_unlock(&dev_tab_mutex);
      return NULL;
   }

unlock:
   /* We must unlock the mutex once the winsys is fully initialized, so that
    * other threads attempting to create the winsys from the same fd will
    * get a fully initialized winsys and not just half-way initialized. */
   simple_mtx_unlock(&dev_tab_mutex);

   return &sws->base;

fail_locked_new_aws:
   amdgpu_destroy_kms_handles(sws);
   amdgpu_winsys_early_cleanup(aws);
   sws->aws = NULL;
   goto fail_locked_sws;

fail_locked_dev:
   if (dev)
      ac_drm_device_deinitialize((void *)dev);

fail_locked_sws:
   if (dev_tab && _mesa_hash_table_num_entries(dev_tab) == 0) {
      _mesa_hash_table_destroy(dev_tab, NULL);
      dev_tab = NULL;
   }

   FREE(sws);
   simple_mtx_unlock(&dev_tab_mutex);
   return NULL;
}
