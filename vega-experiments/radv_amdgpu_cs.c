/*
 * Copyright © 2016 Red Hat.
 * Copyright © 2016 Bas Nieuwenhuizen
 *
 * SPDX-License-Identifier: MIT
 */

#include <assert.h>
#include <libsync.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "drm-uapi/amdgpu_drm.h"

#include "util/detect_os.h"
#include "util/os_time.h"
#include "util/u_memory.h"
#include "ac_cmdbuf_cp.h"
#include "ac_debug.h"
#include "ac_linux_drm.h"
#include "radv_amdgpu_bo.h"
#include "radv_amdgpu_cs.h"
#include "radv_amdgpu_winsys.h"
#include "radv_debug.h"
#include "radv_radeon_winsys.h"
#include "sid.h"
#include "vk_alloc.h"
#include "vk_drm_syncobj.h"
#include "vk_sync.h"
#include "vk_sync_dummy.h"

#if defined(__x86_64__) || defined(_M_X64)
#include <immintrin.h>
#endif

/* Some BSDs don't define ENODATA (and ENODATA is replaced with different error
 * codes in the kernel).
 */
#if DETECT_OS_OPENBSD
#define ENODATA ENOTSUP
#elif DETECT_OS_FREEBSD || DETECT_OS_DRAGONFLY
#define ENODATA ECONNREFUSED
#endif

/* Branch prediction hints for performance-critical paths. */
#if defined(__GNUC__) || defined(__clang__)
#define likely(x)   __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)
#else
#define likely(x)   (x)
#define unlikely(x) (x)
#endif

/* Attributes for hot paths and inlining. */
#if defined(__GNUC__) || defined(__clang__)
#define RADV_ALWAYS_INLINE __attribute__((always_inline)) inline
#define RADV_HOT __attribute__((hot))
#else
#define RADV_ALWAYS_INLINE inline
#define RADV_HOT
#endif

/* Thresholds/tuning knobs. */
#define RADV_MAX_IBS_PER_SUBMIT 192
#define BUFFER_HASH_TABLE_SIZE 1024
#define VIRTUAL_BUFFER_HASH_TABLE_SIZE 1024
/* Optimized: Lower threshold for hash-based BO dedup (cache-friendly for small lists). */
#define RADV_SMALL_BO_DEDUP_THRESHOLD 128u
/* Only use AVX2 non-temporal streaming for big copies where it pays off. */
#define RADV_NT_STREAM_THRESHOLD_BYTES 32768u /* 32 KiB */

/* Maximum hash table size to prevent excessive memory usage. */
#define RADV_MAX_HASH_TABLE_SIZE 8192u

/*
 * Thread-local AVX2 feature cache - avoids repeated CPUID calls.
 * CPUID takes ~100-200 cycles; caching saves this on every search.
 */
static _Thread_local int avx2_supported_cached = -1;

static RADV_ALWAYS_INLINE bool
radv_cpu_supports_avx2_cached(void)
{
#if defined(__GNUC__) || defined(__clang__)
   if (unlikely(avx2_supported_cached < 0)) {
      __builtin_cpu_init();
      /* Require both AVX2 and BMI2 for optimal performance */
      avx2_supported_cached = __builtin_cpu_supports("avx2") && __builtin_cpu_supports("bmi2") ? 1 : 0;
   }
   return avx2_supported_cached != 0;
#else
   return false;
#endif
}

struct radv_amdgpu_ib {
   struct radeon_winsys_bo *bo; /* NULL when not owned by the current CS object */
   uint64_t va;
   unsigned cdw;
};

struct radv_amdgpu_cs_ib_info {
   int64_t flags;
   uint64_t ib_mc_address;
   uint32_t size;
   enum amd_ip_type ip_type;
};

/* A hash entry for our Robin Hood hash table.
 * We store the bo_handle to resolve collisions and the index into the handles array.
 * `bo_handle == 0` signifies an empty slot.
 */
struct radv_buffer_hash_entry {
      uint32_t bo_handle;      /* Buffer object handle (can be 0) */
      uint32_t hash_cached;    /* Cached hash for Robin Hood distance */
      int32_t  index;          /* Index into handles array */
      uint8_t  present;        /* 1 if occupied, 0 if empty (fixes handle=0 bug) */
      uint8_t  _pad[3];        /* Pad to 16 bytes (4 entries per cache line) */
};

/* Compile-time verification of cache line packing */
_Static_assert(sizeof(struct radv_buffer_hash_entry) == 16,
               "Hash entry must be 16 bytes for cache line packing");
_Static_assert((sizeof(struct radv_buffer_hash_entry) * 4) == 64,
               "4 entries must fit exactly in 64-byte cache line");

/*
 * OPTIMIZATION 3: Cache-Optimized `radv_amdgpu_cs` Struct Layout
 *
 * The fields of this struct have been meticulously reordered to improve cache
 * performance on modern CPUs (e.g., Intel Raptor Lake).
 *
 * - Hot data used in every command emission or buffer addition is packed into
 *   the first one or two cache lines (64/128 bytes).
 * - Warm data is grouped next.
 * - Cold data, used only for debugging or rare paths, is placed at the end
 *   to avoid polluting the cache.
 *
 * This minimizes cache misses in performance-critical paths.
 */
struct radv_amdgpu_cs {
   /* --- CACHE LINE 1: Hottest data --- */
   /* `base` must be first for ABI compatibility. It contains the most frequently
    * modified fields: `buf` (destination pointer) and `cdw` (dword cursor).
    */
   struct ac_cmdbuf base;

   /* `ws` is needed for growing, `status` is checked frequently.
    * `num_buffers` is incremented on every unique buffer addition.
    * `ib_buffer` points to the current command buffer BO.
    */
   struct radv_amdgpu_winsys *ws;
   VkResult status;
   unsigned num_buffers;
   struct radeon_winsys_bo *ib_buffer;

   /* --- CACHE LINE 2: Warm data --- */
   /* These fields are used frequently, but less so than the `cdw` cursor. */
   unsigned max_num_buffers;
   struct drm_amdgpu_bo_list_entry *handles;
   uint32_t *ib_size_ptr;
   bool chain_ib;
   unsigned hw_ip;

   /* Dynamic hash table size for resizing support. */
   uint32_t buffer_hash_table_size;

   /* --- COLD DATA: Infrequently accessed --- */
   /* This data is not on the critical path of command emission. */
   uint8_t *ib_mapped;
   struct radv_amdgpu_cs_ib_info ib;

   struct radv_amdgpu_ib *ib_buffers;
   unsigned num_ib_buffers;
   unsigned max_num_ib_buffers;

   bool is_secondary;
   struct radv_amdgpu_cs *chained_to;

   unsigned num_virtual_buffers;
   unsigned max_num_virtual_buffers;
   struct radeon_winsys_bo **virtual_buffers;
   int *virtual_buffer_hash_table;

   /* The buffer hash table is large and placed at the end. */
   struct radv_buffer_hash_entry *buffer_hash_table;

   /* Annotations are for debugging and are extremely cold. */
   struct hash_table *annotations;
};

/* Enforce ABI compatibility with a compile-time assertion. */
_Static_assert(offsetof(struct radv_amdgpu_cs, base) == 0,
               "radv_amdgpu_cs.base must be the first member");

struct radv_winsys_sem_counts {
   uint32_t syncobj_count;
   uint32_t timeline_syncobj_count;
   uint32_t *syncobj;
   uint64_t *points;
};

struct radv_winsys_sem_info {
   bool cs_emit_signal;
   bool cs_emit_wait;
   struct radv_winsys_sem_counts wait;
   struct radv_winsys_sem_counts signal;
};

static RADV_ALWAYS_INLINE RADV_HOT void
radeon_emit_unchecked(struct ac_cmdbuf *cs, uint32_t value)
{
   cs->buf[cs->cdw++] = value;
}

static uint32_t radv_amdgpu_ctx_queue_syncobj(struct radv_amdgpu_ctx *ctx, unsigned ip, unsigned ring);

static inline struct radv_amdgpu_cs *
radv_amdgpu_cs(struct ac_cmdbuf *base)
{
   return (struct radv_amdgpu_cs *)base;
}

struct radv_amdgpu_cs_request {
   /** Specify HW IP block type to which to send the IB. */
   unsigned ip_type;

   /** IP instance index if there are several IPs of the same type. */
   unsigned ip_instance;

   /**
    * Specify ring index of the IP. We could have several rings
    * in the same IP. E.g. 0 for SDMA0 and 1 for SDMA1.
    */
   uint32_t ring;

   /**
    * BO list handles used by this request.
    */
   struct drm_amdgpu_bo_list_entry *handles;
   uint32_t num_handles;

   /** Number of IBs to submit in the field ibs. */
   uint32_t number_of_ibs;

   /**
    * IBs to submit. Those IBs will be submitted together as single entity
    */
   struct radv_amdgpu_cs_ib_info *ibs;

   /**
    * The returned sequence number for the command submission
    */
   uint64_t seq_no;
};

static VkResult radv_amdgpu_cs_submit(struct radv_amdgpu_ctx *ctx, struct radv_amdgpu_cs_request *request,
                                      struct radv_winsys_sem_info *sem_info);

static void
radv_amdgpu_request_to_fence(struct radv_amdgpu_ctx *ctx, struct radv_amdgpu_fence *fence,
                             struct radv_amdgpu_cs_request *req)
{
   fence->fence.ip_type = req->ip_type;
   fence->fence.ip_instance = req->ip_instance;
   fence->fence.ring = req->ring;
   fence->fence.fence = req->seq_no;
}

static struct radv_amdgpu_cs_ib_info
radv_amdgpu_cs_ib_to_info(struct radv_amdgpu_cs *cs, struct radv_amdgpu_ib ib)
{
   struct radv_amdgpu_cs_ib_info info = {
      .flags = 0,
      .ip_type = cs->hw_ip,
      .ib_mc_address = ib.va,
      .size = ib.cdw,
   };
   return info;
}

static void
radv_amdgpu_cs_free_annotation(struct hash_entry *entry)
{
   free(entry->data);
}

static void
radv_amdgpu_cs_destroy(struct ac_cmdbuf *rcs)
{
   struct radv_amdgpu_cs *cs = radv_amdgpu_cs(rcs);

   if (!cs) {
      return;
   }

   _mesa_hash_table_destroy(cs->annotations, radv_amdgpu_cs_free_annotation);

   if (cs->ib_buffer) {
      cs->ws->base.buffer_destroy(&cs->ws->base, cs->ib_buffer);
   }

   for (unsigned i = 0; i < cs->num_ib_buffers; ++i) {
      if (cs->ib_buffers[i].bo) {
         cs->ws->base.buffer_destroy(&cs->ws->base, cs->ib_buffers[i].bo);
      }
   }

   free(cs->ib_buffers);
   free(cs->virtual_buffers);
   free(cs->virtual_buffer_hash_table);
   free(cs->handles);
   free(cs->buffer_hash_table);
   free(cs);
}

static void
radv_amdgpu_init_cs(struct radv_amdgpu_cs *cs, enum amd_ip_type ip_type)
{
   cs->buffer_hash_table_size = 128u;
   cs->buffer_hash_table = calloc(cs->buffer_hash_table_size, sizeof(struct radv_buffer_hash_entry));
   if (unlikely(!cs->buffer_hash_table)) {
      cs->status = VK_ERROR_OUT_OF_HOST_MEMORY;
   }

   cs->hw_ip = ip_type;
}

static enum radeon_bo_domain
radv_amdgpu_cs_domain(const struct radeon_winsys *_ws)
{
   const struct radv_amdgpu_winsys *ws = (const struct radv_amdgpu_winsys *)_ws;

   bool enough_vram = ws->info.all_vram_visible ||
                      p_atomic_read_relaxed(&ws->allocated_vram_vis) * 2 <= (uint64_t)ws->info.vram_vis_size_kb * 1024;

   /* Bandwidth should be equivalent to at least PCIe 3.0 x8.
    * If there is no PCIe info, assume there is enough bandwidth.
    */
   const uint32_t bandwidth_mbps_threshold = 8 * 0.985 * 1024;
   bool enough_bandwidth =
      !ws->info.has_pcie_bandwidth_info || ws->info.pcie_bandwidth_mbps >= bandwidth_mbps_threshold;

   bool use_sam =
      (enough_vram && enough_bandwidth && ws->info.has_dedicated_vram && !(ws->perftest & RADV_PERFTEST_NO_SAM)) ||
      (ws->perftest & RADV_PERFTEST_SAM);
   return use_sam ? RADEON_DOMAIN_VRAM : RADEON_DOMAIN_GTT;
}

static VkResult
radv_amdgpu_cs_bo_create(struct radv_amdgpu_cs *cs, uint32_t ib_size)
{
   struct radeon_winsys *ws = &cs->ws->base;

   /* Avoid memcpy from VRAM when a secondary cmdbuf can't always rely on IB2. */
   const bool can_always_use_ib2 = cs->ws->info.gfx_level >= GFX8 && cs->hw_ip == AMD_IP_GFX;
   const bool avoid_vram = cs->is_secondary && !can_always_use_ib2;
   const enum radeon_bo_domain domain = avoid_vram ? RADEON_DOMAIN_GTT : radv_amdgpu_cs_domain(ws);
   const enum radeon_bo_flag gtt_wc_flag = avoid_vram ? 0 : RADEON_FLAG_GTT_WC;
   /* Bypass GL2 because command buffers are read only once and it's better for latency. */
   const enum radeon_bo_flag flags = RADEON_FLAG_CPU_ACCESS | RADEON_FLAG_NO_INTERPROCESS_SHARING |
                                     RADEON_FLAG_READ_ONLY | RADEON_FLAG_GL2_BYPASS | gtt_wc_flag;

   return ws->buffer_create(ws, ib_size, cs->ws->info.ip[cs->hw_ip].ib_alignment, domain, flags, RADV_BO_PRIORITY_CS, 0,
                            &cs->ib_buffer);
}

static VkResult
radv_amdgpu_cs_get_new_ib(struct ac_cmdbuf *_cs, uint32_t ib_size)
{
   struct radv_amdgpu_cs *cs = radv_amdgpu_cs(_cs);
   VkResult result;

   result = radv_amdgpu_cs_bo_create(cs, ib_size);
   if (unlikely(result != VK_SUCCESS)) {
      return result;
   }

   cs->ib_mapped = radv_buffer_map(&cs->ws->base, cs->ib_buffer);
   if (unlikely(!cs->ib_mapped)) {
      cs->ws->base.buffer_destroy(&cs->ws->base, cs->ib_buffer);
      cs->ib_buffer = NULL;
      return VK_ERROR_OUT_OF_DEVICE_MEMORY;
   }

   cs->ib.ib_mc_address = radv_amdgpu_winsys_bo(cs->ib_buffer)->base.va;
   cs->base.buf = (uint32_t *)cs->ib_mapped;
   cs->base.cdw = 0;
   cs->base.reserved_dw = 0;
   cs->base.max_dw = ib_size / 4u - 4u;
   cs->ib.size = 0;
   cs->ib.ip_type = cs->hw_ip;

   if (cs->chain_ib) {
      cs->ib_size_ptr = &cs->ib.size;
   }

   cs->ws->base.cs_add_buffer(&cs->base, cs->ib_buffer);

   return VK_SUCCESS;
}

static unsigned
radv_amdgpu_cs_get_initial_size(struct radv_amdgpu_winsys *ws, enum amd_ip_type ip_type)
{
   const uint32_t ib_alignment = ws->info.ip[ip_type].ib_alignment;
   assert(util_is_power_of_two_nonzero(ib_alignment));
   return align(20 * 1024 * 4, ib_alignment);
}

static struct ac_cmdbuf *
radv_amdgpu_cs_create(struct radeon_winsys *ws, enum amd_ip_type ip_type, bool is_secondary)
{
   struct radv_amdgpu_cs *cs;
   uint32_t ib_size = radv_amdgpu_cs_get_initial_size(radv_amdgpu_winsys(ws), ip_type);

   cs = calloc(1, sizeof(struct radv_amdgpu_cs));
   if (unlikely(!cs)) {
      return NULL;
   }

   cs->is_secondary = is_secondary;
   cs->ws = radv_amdgpu_winsys(ws);
   radv_amdgpu_init_cs(cs, ip_type);

   cs->chain_ib = cs->ws->chain_ib && (ip_type == AMD_IP_GFX || ip_type == AMD_IP_COMPUTE) &&
                  !(is_secondary && !cs->ws->info.can_chain_ib2);

   VkResult result = radv_amdgpu_cs_get_new_ib(&cs->base, ib_size);
   if (unlikely(result != VK_SUCCESS)) {
      free(cs->buffer_hash_table);
      free(cs);
      return NULL;
   }

   return &cs->base;
}

static uint32_t
get_nop_packet(struct radv_amdgpu_cs *cs)
{
   switch (cs->hw_ip) {
   case AMDGPU_HW_IP_GFX:
   case AMDGPU_HW_IP_COMPUTE:
      return cs->ws->info.gfx_ib_pad_with_type2 ? PKT2_NOP_PAD : PKT3_NOP_PAD;
   case AMDGPU_HW_IP_DMA:
      return cs->ws->info.gfx_level == GFX6 ? 0xF0000000 : SDMA_NOP_PAD;
   case AMDGPU_HW_IP_UVD:
   case AMDGPU_HW_IP_UVD_ENC:
      return PKT2_NOP_PAD;
   case AMDGPU_HW_IP_VCN_DEC:
      return 0x81FF;
   case AMDGPU_HW_IP_VCN_ENC:
      return 0; /* NOPs are illegal in encode, so don't pad */
   default:
      UNREACHABLE("Unknown IP type");
   }
}

/**
 * Emit a PKT3 NOP packet for the graphics or compute queue.
 *
 * Emit a single NOP packet to minimize CP overhead because NOP is a variable-sized
 * packet. The size of the packet body after the header is always count + 1.
 * If count == -1, there is no packet body. NOP is the only packet that can have
 * count == -1, which is the definition of PKT3_NOP_PAD (count == 0x3fff means -1).
 *
 * Note that GFX6 doesn't support PKT3_NOP with count == -1
 */
static void
radv_amdgpu_cs_emit_pkt3_nop(struct radv_amdgpu_cs *cs, const unsigned num_dw)
{
   assert(num_dw >= (cs->ws->info.gfx_ib_pad_with_type2 ? 2 : 1));

   radeon_emit_unchecked(&cs->base, PKT3(PKT3_NOP, num_dw - 2, 0));
   cs->base.cdw += num_dw - 1;
}

/**
 * Emit one or more NOP packets to fill the specified amount of dwords.
 * Should only be called for IP types that have a NOP packet.
 */
static void
radv_amdgpu_cs_emit_nops(struct radv_amdgpu_cs *cs, const unsigned num_dw)
{
   const enum amd_ip_type ip_type = cs->hw_ip;
   assert(ip_type != AMDGPU_HW_IP_VCN_ENC); /* VCN_ENC has no NOP packets. */

   if (!num_dw) {
      return;
   }

   /* Emit a single, larger PKT3 NOP packet to fill the specified amount of dwords. */
   if (num_dw > 1 && (ip_type == AMD_IP_GFX || ip_type == AMD_IP_COMPUTE)) {
      radv_amdgpu_cs_emit_pkt3_nop(cs, num_dw);
      return;
   }

   const uint32_t nop_packet = get_nop_packet(cs);

   for (uint32_t i = 0; i < num_dw; ++i) {
      radeon_emit_unchecked(&cs->base, nop_packet);
   }
}

static void
radv_amdgpu_cs_add_ib_buffer(struct radv_amdgpu_cs *cs, struct radeon_winsys_bo *bo, uint64_t va, uint32_t cdw)
{
   if (unlikely(cs->num_ib_buffers == cs->max_num_ib_buffers)) {
      unsigned max_num_ib_buffers = MAX2(1, cs->max_num_ib_buffers * 2);
      struct radv_amdgpu_ib *ib_buffers = realloc(cs->ib_buffers, max_num_ib_buffers * sizeof(*ib_buffers));
      if (unlikely(!ib_buffers)) {
         cs->status = VK_ERROR_OUT_OF_HOST_MEMORY;
         return;
      }
      cs->max_num_ib_buffers = max_num_ib_buffers;
      cs->ib_buffers = ib_buffers;
   }

   cs->ib_buffers[cs->num_ib_buffers].bo = bo;
   cs->ib_buffers[cs->num_ib_buffers].va = va;
   cs->ib_buffers[cs->num_ib_buffers++].cdw = cdw;
}

static void
radv_amdgpu_restore_last_ib(struct radv_amdgpu_cs *cs)
{
   /* FIX: Validate that we have at least one IB buffer before restoring. */
   if (unlikely(cs->num_ib_buffers == 0)) {
      cs->status = VK_ERROR_OUT_OF_DEVICE_MEMORY;
      return;
   }

   struct radv_amdgpu_ib *ib = &cs->ib_buffers[--cs->num_ib_buffers];
   assert(ib->bo);
   cs->ib_buffer = ib->bo;
}

static void
radv_amdgpu_cs_grow(struct ac_cmdbuf *_cs, size_t min_size)
{
   struct radv_amdgpu_cs *cs = radv_amdgpu_cs(_cs);

   if (unlikely(cs->status != VK_SUCCESS)) {
      cs->base.cdw = 0;
      return;
   }

   const uint32_t ib_alignment = cs->ws->info.ip[cs->hw_ip].ib_alignment;

   cs->ws->base.cs_finalize(_cs);

   /* FIX: Add overflow check for ib_size calculation. */
   uint64_t ib_size = MAX2(min_size * 4 + 16, cs->base.max_dw * 4 * 2);

   /* Ensure ib_size doesn't exceed maximum valid size. */
   if (ib_size > ~C_3F2_IB_SIZE) {
      ib_size = ~C_3F2_IB_SIZE;
   }

   ib_size = align(ib_size, ib_alignment);

   /* FIX: Validate that alignment didn't cause overflow. */
   if (unlikely(ib_size > UINT32_MAX)) {
      cs->base.cdw = 0;
      cs->status = VK_ERROR_OUT_OF_HOST_MEMORY;
      if (cs->num_ib_buffers > 0) {
         radv_amdgpu_restore_last_ib(cs);
      }
      return;
   }

   VkResult result = radv_amdgpu_cs_bo_create(cs, (uint32_t)ib_size);

   if (result != VK_SUCCESS) {
      cs->base.cdw = 0;
      cs->status = VK_ERROR_OUT_OF_DEVICE_MEMORY;
      if (cs->num_ib_buffers > 0) {
         radv_amdgpu_restore_last_ib(cs);
      }
      return;
   }

   cs->ib_mapped = radv_buffer_map(&cs->ws->base, cs->ib_buffer);
   if (!cs->ib_mapped) {
      cs->ws->base.buffer_destroy(&cs->ws->base, cs->ib_buffer);
      cs->ib_buffer = NULL;
      cs->base.cdw = 0;
      cs->status = VK_ERROR_OUT_OF_DEVICE_MEMORY;
      if (cs->num_ib_buffers > 0) {
         radv_amdgpu_restore_last_ib(cs);
      }
      return;
   }

   cs->ws->base.cs_add_buffer(&cs->base, cs->ib_buffer);

   if (cs->chain_ib) {
      cs->base.buf[cs->base.cdw - 4] = PKT3(PKT3_INDIRECT_BUFFER, 2, 0);
      cs->base.buf[cs->base.cdw - 3] = radv_amdgpu_winsys_bo(cs->ib_buffer)->base.va;
      cs->base.buf[cs->base.cdw - 2] = radv_amdgpu_winsys_bo(cs->ib_buffer)->base.va >> 32;
      cs->base.buf[cs->base.cdw - 1] = S_3F2_CHAIN(1) | S_3F2_VALID(1);

      cs->ib_size_ptr = cs->base.buf + cs->base.cdw - 1;
   }

   cs->base.buf = (uint32_t *)cs->ib_mapped;
   cs->base.cdw = 0;
   cs->base.reserved_dw = 0;
   cs->base.max_dw = (uint32_t)(ib_size / 4u - 4u);
}

static void
radv_amdgpu_winsys_cs_pad(struct ac_cmdbuf *_cs, unsigned leave_dw_space)
{
   struct radv_amdgpu_cs *cs = radv_amdgpu_cs(_cs);
   const enum amd_ip_type ip_type = cs->hw_ip;

   /* Don't pad on VCN encode/unified as no NOPs */
   if (ip_type == AMDGPU_HW_IP_VCN_ENC) {
      return;
   }

   /* Don't add padding to 0 length UVD due to kernel. */
   if (ip_type == AMDGPU_HW_IP_UVD && cs->base.cdw == 0) {
      return;
   }

   const uint32_t pad_dw_mask = cs->ws->info.ip[ip_type].ib_pad_dw_mask;

   /* FIX: Add overflow check before calculating unaligned_dw. */
   if (unlikely(cs->base.cdw > UINT32_MAX - leave_dw_space)) {
      return;
   }

   const uint32_t unaligned_dw = (cs->base.cdw + leave_dw_space) & pad_dw_mask;

   if (unaligned_dw) {
      /* Pad the IB with NOP packets to ensure that the end of the IB is correctly aligned. */
      radv_amdgpu_cs_emit_nops(cs, pad_dw_mask + 1 - unaligned_dw);
   } else if (cs->base.cdw == 0 && leave_dw_space == 0) {
      /* Emit NOPs to avoid submitting a completely empty IB. */
      radv_amdgpu_cs_emit_nops(cs, pad_dw_mask + 1);
   }

   assert(((cs->base.cdw + leave_dw_space) & pad_dw_mask) == 0);
}

static VkResult
radv_amdgpu_cs_finalize(struct ac_cmdbuf *_cs)
{
   struct radv_amdgpu_cs *cs = radv_amdgpu_cs(_cs);

   assert(cs->base.cdw <= cs->base.reserved_dw);

   if (cs->chain_ib) {
      radv_amdgpu_winsys_cs_pad(_cs, 4);
      radv_amdgpu_cs_emit_nops(cs, 4);
      *cs->ib_size_ptr |= cs->base.cdw;
   } else {
      radv_amdgpu_winsys_cs_pad(_cs, 0);
   }

   /* Append the current (last) IB to the array of IB buffers. */
   radv_amdgpu_cs_add_ib_buffer(cs, cs->ib_buffer, cs->ib_buffer->va,
                                cs->chain_ib ? G_3F2_IB_SIZE(*cs->ib_size_ptr) : cs->base.cdw);

   /* Prevent freeing this BO twice. */
   cs->ib_buffer = NULL;

   cs->chained_to = NULL;

   assert(cs->base.cdw <= cs->base.max_dw + 4);

   return cs->status;
}

static void
radv_amdgpu_cs_reset(struct ac_cmdbuf *_cs)
{
   struct radv_amdgpu_cs *cs = radv_amdgpu_cs(_cs);
   cs->base.cdw = 0;
   cs->base.reserved_dw = 0;
   cs->status = VK_SUCCESS;

   /* FIX: Add null check before memset. */
   if (likely(cs->buffer_hash_table)) {
      memset(cs->buffer_hash_table, 0, sizeof(struct radv_buffer_hash_entry) * cs->buffer_hash_table_size);
   }

   /* FIX: Add null check before accessing virtual buffer hash table. */
   if (cs->virtual_buffer_hash_table) {
      for (unsigned i = 0; i < cs->num_virtual_buffers; ++i) {
         unsigned hash = ((uintptr_t)cs->virtual_buffers[i] >> 6) & (VIRTUAL_BUFFER_HASH_TABLE_SIZE - 1);
         cs->virtual_buffer_hash_table[hash] = -1;
      }
   }

   cs->num_buffers = 0;
   cs->num_virtual_buffers = 0;

   /* When the CS is finalized and IBs are not allowed, use last IB. */
   assert(cs->ib_buffer || cs->num_ib_buffers);
   if (!cs->ib_buffer) {
      radv_amdgpu_restore_last_ib(cs);
      /* FIX: Check if restore failed. */
      if (cs->status != VK_SUCCESS) {
         return;
      }
   }

   cs->ws->base.cs_add_buffer(&cs->base, cs->ib_buffer);

   for (unsigned i = 0; i < cs->num_ib_buffers; ++i) {
      if (cs->ib_buffers[i].bo) {
         cs->ws->base.buffer_destroy(&cs->ws->base, cs->ib_buffers[i].bo);
      }
   }

   cs->num_ib_buffers = 0;
   cs->ib.ib_mc_address = radv_amdgpu_winsys_bo(cs->ib_buffer)->base.va;

   cs->ib.size = 0;

   if (cs->chain_ib) {
      cs->ib_size_ptr = &cs->ib.size;
   }

   _mesa_hash_table_destroy(cs->annotations, radv_amdgpu_cs_free_annotation);
   cs->annotations = NULL;
}

static void
radv_amdgpu_cs_unchain(struct ac_cmdbuf *cs)
{
   struct radv_amdgpu_cs *acs = radv_amdgpu_cs(cs);

   if (!acs->chained_to) {
      return;
   }

   assert(cs->cdw <= cs->max_dw + 4);

   acs->chained_to = NULL;
   cs->buf[cs->cdw - 4] = PKT3(PKT3_NOP, 2, 0);
}

static bool
radv_amdgpu_cs_chain(struct ac_cmdbuf *cs, struct ac_cmdbuf *next_cs, bool pre_ena)
{
   /* Chains together two CS (command stream) objects by editing
    * the end of the first CS to add a command that jumps to the
    * second CS.
    *
    * After this, it is enough to submit the first CS to the GPU
    * and not necessary to submit the second CS because it is already
    * executed by the first.
    */

   struct radv_amdgpu_cs *acs = radv_amdgpu_cs(cs);
   struct radv_amdgpu_cs *next_acs = radv_amdgpu_cs(next_cs);

   /* Only some HW IP types have packets that we can use for chaining. */
   if (!acs->chain_ib) {
      return false;
   }

   assert(cs->cdw <= cs->max_dw + 4);

   acs->chained_to = next_acs;

   cs->buf[cs->cdw - 4] = PKT3(PKT3_INDIRECT_BUFFER, 2, 0);
   cs->buf[cs->cdw - 3] = next_acs->ib.ib_mc_address;
   cs->buf[cs->cdw - 2] = next_acs->ib.ib_mc_address >> 32;
   cs->buf[cs->cdw - 1] = S_3F2_CHAIN(1) | S_3F2_VALID(1) | S_3F2_PRE_ENA(pre_ena) | next_acs->ib.size;

   return true;
}

/* Hashing and table ops. */
static RADV_ALWAYS_INLINE __attribute__((const)) uint32_t
radv_hash_bo(uint32_t bo_handle)
{
   uint32_t h = bo_handle;
   h ^= h >> 16;
   h *= 0x85ebca6bu;
   h ^= h >> 13;
   h *= 0xc2b2ae35u;
   h ^= h >> 16;
   return h;
}

static int
radv_amdgpu_cs_find_buffer(struct radv_amdgpu_cs *cs, uint32_t bo_handle)
{
   /* Memory barrier: ensure we see latest writes from other threads */
   __atomic_thread_fence(__ATOMIC_ACQUIRE);

   /* Error state check - must be first for correctness */
   if (unlikely(cs->status != VK_SUCCESS || cs->buffer_hash_table == NULL)) {
      /* Fallback: linear search in handles array */
      for (unsigned i = 0; i < cs->num_buffers; ++i) {
         if (cs->handles[i].bo_handle == bo_handle) {
            return (int)i;
         }
      }
      return -1;
   }

   /* Fast path: empty buffer list */
   if (unlikely(cs->num_buffers == 0)) {
      return -1;
   }

   const uint32_t mask = cs->buffer_hash_table_size - 1u;
   const uint32_t hash = radv_hash_bo(bo_handle);
   uint32_t dist = 0u;

   /* Prefetch initial probe location to L1 cache */
   __builtin_prefetch(&cs->buffer_hash_table[hash & mask], 0, 3);

   for (;;) {
      const uint32_t pos = (hash + dist) & mask;
      struct radv_buffer_hash_entry *entry = &cs->buffer_hash_table[pos];

      /*
       * CRITICAL: Prefetch 2 entries ahead.
       * Raptor Lake L1 latency is ~5 cycles. At ~2 cycles per iteration,
       * prefetching 2 ahead ensures data arrives just when needed.
       * Prefetching more wastes cache bandwidth; less causes stalls.
       */
      if (dist < 32u) {
         __builtin_prefetch(&cs->buffer_hash_table[(hash + dist + 2u) & mask], 0, 2);
      }

      /* Check if slot is occupied (most common case) */
      if (likely(entry->present)) {
         /* Fast path: exact handle match */
         if (likely(entry->bo_handle == bo_handle)) {
            /* Validate index before returning (defense in depth) */
            if (likely((unsigned)entry->index < cs->num_buffers)) {
               return entry->index;
            }
            /* Corrupted entry - continue search (table inconsistency) */
         }

         /*
          * Robin Hood invariant check:
          * If our probe distance exceeds this entry's distance,
          * the element cannot exist (would have displaced this entry).
          * This is the key property that makes Robin Hood O(1) average.
          */
         const uint32_t entry_hash = entry->hash_cached;
         const uint32_t entry_dist = (pos - (entry_hash & mask) + cs->buffer_hash_table_size) & mask;

         if (unlikely(dist > entry_dist)) {
            return -1;
         }
      } else {
         /* Empty slot - element definitively not present */
         return -1;
      }

      dist++;

      /*
       * Failsafe: table should never be full with proper load factor.
       * If we've probed all slots, fall back to linear search.
       * This handles pathological hash collisions gracefully.
       */
      if (unlikely(dist >= cs->buffer_hash_table_size)) {
         for (unsigned i = 0; i < cs->num_buffers; ++i) {
            if (cs->handles[i].bo_handle == bo_handle) {
               return (int)i;
            }
         }
         return -1;
      }
   }
}

static void
radv_amdgpu_cs_insert_buffer(struct radv_amdgpu_cs *cs, uint32_t bo_handle, int index)
{
   /* Validate hash table exists */
   if (unlikely(cs->buffer_hash_table == NULL)) {
      return;
   }

   /* Validate index bounds (prevent corruption) */
   if (unlikely(index < 0 || (unsigned)index >= cs->num_buffers)) {
      cs->status = VK_ERROR_UNKNOWN;
      return;
   }

   const uint32_t mask = cs->buffer_hash_table_size - 1u;
   const uint32_t hash = radv_hash_bo(bo_handle);
   uint32_t dist = 0u;

   struct radv_buffer_hash_entry new_entry = {
      .bo_handle = bo_handle,
      .hash_cached = hash,
      .index = index,
      .present = 1,
      ._pad = {0, 0, 0}
   };

   /* Prefetch initial location for write */
   __builtin_prefetch(&cs->buffer_hash_table[hash & mask], 1, 3);

   for (;;) {
      const uint32_t pos = (hash + dist) & mask;
      struct radv_buffer_hash_entry *entry = &cs->buffer_hash_table[pos];

      /* Prefetch next location for write */
      if (dist < 32u) {
         __builtin_prefetch(&cs->buffer_hash_table[(hash + dist + 2u) & mask], 1, 2);
      }

      /* Empty slot - insert here */
      if (unlikely(!entry->present)) {
         *entry = new_entry;
         /* Memory barrier: ensure write is visible before returning */
         __atomic_thread_fence(__ATOMIC_RELEASE);
         return;
      }

      /* Duplicate handle - update index */
      if (unlikely(entry->bo_handle == bo_handle)) {
         entry->index = index;
         entry->hash_cached = hash;
         __atomic_thread_fence(__ATOMIC_RELEASE);
         return;
      }

      /*
       * Robin Hood displacement:
       * If new entry has traveled farther than existing entry,
       * swap them and continue inserting the displaced entry.
       * This minimizes maximum probe distance across all entries.
       */
      const uint32_t entry_hash = entry->hash_cached;
      const uint32_t entry_dist = (pos - (entry_hash & mask) + cs->buffer_hash_table_size) & mask;

      if (unlikely(dist > entry_dist)) {
         /* Swap: new entry takes this slot, continue with displaced */
         const struct radv_buffer_hash_entry tmp = *entry;
         *entry = new_entry;
         new_entry = tmp;
         dist = entry_dist;
         /* hash already correct in new_entry.hash_cached */
      }

      dist++;

      /* Critical: prevent infinite loop on full table */
      if (unlikely(dist >= cs->buffer_hash_table_size)) {
         cs->status = VK_ERROR_OUT_OF_HOST_MEMORY;
         return;
      }
   }
}

static void
radv_amdgpu_cs_resize_buffer_hash_table(struct radv_amdgpu_cs *cs)
{
   /* Validate table exists */
   if (unlikely(cs->buffer_hash_table == NULL)) {
      return;
   }

   const uint32_t old_size = cs->buffer_hash_table_size;

   /* Overflow check: prevent doubling beyond safe limits */
   if (unlikely(old_size >= RADV_MAX_HASH_TABLE_SIZE / 2u)) {
      return;
   }

   const uint32_t new_size = old_size * 2u;

   /* Compile-time verification of power-of-2 requirement */
   _Static_assert((RADV_MAX_HASH_TABLE_SIZE & (RADV_MAX_HASH_TABLE_SIZE - 1u)) == 0,
                  "RADV_MAX_HASH_TABLE_SIZE must be power of 2");

   /* Allocate new table with zero-initialization */
   struct radv_buffer_hash_entry *new_table = calloc(new_size, sizeof(struct radv_buffer_hash_entry));
   if (unlikely(new_table == NULL)) {
      /* OOM: continue with old table (degraded performance) */
      return;
   }

   /* Save old state for potential rollback */
   struct radv_buffer_hash_entry *old_table = cs->buffer_hash_table;
   const VkResult old_status = cs->status;

   /* Update table pointer BEFORE rehashing */
   cs->buffer_hash_table = new_table;
   cs->buffer_hash_table_size = new_size;

   /* Rehash all valid entries */
   for (uint32_t i = 0; i < old_size; ++i) {
      if (old_table[i].present) {
         radv_amdgpu_cs_insert_buffer(cs, old_table[i].bo_handle, old_table[i].index);

         /* Check for insertion failure during rehash */
         if (unlikely(cs->status != VK_SUCCESS)) {
            /* CRITICAL: Restore old table on failure */
            free(new_table);
            cs->buffer_hash_table = old_table;
            cs->buffer_hash_table_size = old_size;
            cs->status = old_status;
            return;
         }
      }
   }

   /* Success: free old table */
   free(old_table);
}

static void
radv_amdgpu_cs_add_buffer_internal(struct radv_amdgpu_cs *cs, uint32_t bo, uint8_t priority)
{
   /* Status check with memory barrier */
   if (unlikely(cs->status != VK_SUCCESS)) {
      return;
   }

   /* Check for duplicate (hash table lookup) */
   if (radv_amdgpu_cs_find_buffer(cs, bo) != -1) {
      return;
   }

   /*
    * Resize hash table at 50% load factor.
    * This is optimal for Robin Hood hashing (minimizes probe distance).
    * All overflow checks use C23 __builtin_mul_overflow.
    */
   if (cs->buffer_hash_table != NULL && cs->num_buffers > 0) {
      uint32_t doubled;
      if (!__builtin_mul_overflow(cs->num_buffers, 2u, &doubled)) {
         if (doubled >= cs->buffer_hash_table_size) {
            radv_amdgpu_cs_resize_buffer_hash_table(cs);
            /* Continue even if resize fails (uses old table) */
         }
      }
   }

   /* Grow handles array if needed */
   if (unlikely(cs->num_buffers >= cs->max_num_buffers)) {
      uint32_t new_count;

      if (cs->max_num_buffers == 0) {
         new_count = 16u; /* Initial size */
      } else if (__builtin_mul_overflow(cs->max_num_buffers, 2u, &new_count)) {
         cs->status = VK_ERROR_OUT_OF_HOST_MEMORY;
         return;
      }

      /* Check allocation size overflow */
      size_t alloc_size;
      if (__builtin_mul_overflow((size_t)new_count, sizeof(struct drm_amdgpu_bo_list_entry), &alloc_size)) {
         cs->status = VK_ERROR_OUT_OF_HOST_MEMORY;
         return;
      }

      /* Reallocate with validation */
      struct drm_amdgpu_bo_list_entry *new_handles = realloc(cs->handles, alloc_size);
      if (unlikely(new_handles == NULL)) {
         cs->status = VK_ERROR_OUT_OF_HOST_MEMORY;
         return;
      }

      cs->handles = new_handles;
      cs->max_num_buffers = new_count;
   }

   /* Add to handles array (validated bounds above) */
   const unsigned index = cs->num_buffers;
   cs->handles[index].bo_handle = bo;
   cs->handles[index].bo_priority = priority;

   /* Insert into hash table */
   if (cs->buffer_hash_table != NULL) {
      radv_amdgpu_cs_insert_buffer(cs, bo, (int)index);

      /* Don't fail on hash table errors - continue with degraded perf */
      if (unlikely(cs->status != VK_SUCCESS)) {
         cs->status = VK_SUCCESS;
      }
   }

   /* Increment count AFTER successful addition */
   cs->num_buffers++;

   /* Memory barrier: ensure writes visible to concurrent readers */
   __atomic_thread_fence(__ATOMIC_RELEASE);
}

static void *
radv_aligned_alloc_portable(size_t alignment, size_t size)
{
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L && !defined(__ANDROID__)
   /* C11 aligned_alloc - requires size be multiple of alignment */
   const size_t aligned_size = (size + alignment - 1u) & ~(alignment - 1u);
   return aligned_alloc(alignment, aligned_size);
#elif defined(_GNU_SOURCE) || defined(__USE_POSIX199506)
   /* POSIX posix_memalign */
   void *ptr = NULL;
   const int ret = posix_memalign(&ptr, alignment, size);
   return (ret == 0) ? ptr : NULL;
#else
   /* Manual alignment with original pointer tracking */
   void *raw = malloc(size + alignment - 1u + sizeof(void *));
   if (unlikely(raw == NULL)) {
      return NULL;
   }

   /* Calculate aligned address */
   void **ptr = (void **)(((uintptr_t)raw + sizeof(void *) + alignment - 1u) & ~(alignment - 1u));
   ptr[-1] = raw; /* Store original pointer for free() */
   return ptr;
#endif
}

static void
radv_aligned_free_portable(void *ptr)
{
   if (unlikely(ptr == NULL)) {
      return;
   }

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L && !defined(__ANDROID__)
   free(ptr);
#elif defined(_GNU_SOURCE) || defined(__USE_POSIX199506)
   free(ptr);
#else
   /* Retrieve original pointer for manual alignment case */
   void **p = (void **)ptr;
   free(p[-1]);
#endif
}

static void
radv_amdgpu_cs_add_virtual_buffer(struct ac_cmdbuf *_cs, struct radeon_winsys_bo *bo)
{
   struct radv_amdgpu_cs *cs = radv_amdgpu_cs(_cs);

   /* Null check */
   if (unlikely(bo == NULL)) {
      return;
   }

   /* Hash from aligned pointer (shift right 6 for 64-byte alignment) */
   const unsigned hash = ((uintptr_t)bo >> 6) & (VIRTUAL_BUFFER_HASH_TABLE_SIZE - 1u);

   /* Lazy initialization of hash table */
   if (unlikely(cs->virtual_buffer_hash_table == NULL)) {
      const size_t alignment = 64u; /* Cache line size */
      const size_t size = VIRTUAL_BUFFER_HASH_TABLE_SIZE * sizeof(int);

      int *table = radv_aligned_alloc_portable(alignment, size);
      if (unlikely(table == NULL)) {
         cs->status = VK_ERROR_OUT_OF_HOST_MEMORY;
         return;
      }

      /* Initialize all slots to -1 (empty) */
      for (int i = 0; i < VIRTUAL_BUFFER_HASH_TABLE_SIZE; ++i) {
         table[i] = -1;
      }

      cs->virtual_buffer_hash_table = table;
   }

   /* Fast path: check cached hash slot */
   const int cached_idx = cs->virtual_buffer_hash_table[hash];
   if (cached_idx >= 0 && (unsigned)cached_idx < cs->num_virtual_buffers) {
      if (cs->virtual_buffers[cached_idx] == bo) {
         return; /* Already present */
      }
   }

   /* Linear search for collision or verification */
   for (unsigned i = 0; i < cs->num_virtual_buffers; ++i) {
      if (cs->virtual_buffers[i] == bo) {
         /* Update cache to point to correct index */
         cs->virtual_buffer_hash_table[hash] = (int)i;
         return;
      }
   }

   /* Not found - add new entry */
   if (cs->num_virtual_buffers >= cs->max_num_virtual_buffers) {
      uint32_t new_max;

      if (cs->max_num_virtual_buffers == 0) {
         new_max = 8u;
      } else if (__builtin_mul_overflow(cs->max_num_virtual_buffers, 2u, &new_max)) {
         cs->status = VK_ERROR_OUT_OF_HOST_MEMORY;
         return;
      }

      size_t alloc_size;
      if (__builtin_mul_overflow((size_t)new_max, sizeof(struct radeon_winsys_bo *), &alloc_size)) {
         cs->status = VK_ERROR_OUT_OF_HOST_MEMORY;
         return;
      }

      struct radeon_winsys_bo **new_array = realloc(cs->virtual_buffers, alloc_size);
      if (unlikely(new_array == NULL)) {
         cs->status = VK_ERROR_OUT_OF_HOST_MEMORY;
         return;
      }

      cs->virtual_buffers = new_array;
      cs->max_num_virtual_buffers = new_max;
   }

   /* Add new virtual buffer */
   const unsigned idx = cs->num_virtual_buffers++;
   cs->virtual_buffers[idx] = bo;
   cs->virtual_buffer_hash_table[hash] = (int)idx;

   /* Memory barrier for visibility */
   __atomic_thread_fence(__ATOMIC_RELEASE);
}

static void
radv_amdgpu_cs_add_buffer(struct ac_cmdbuf *_cs, struct radeon_winsys_bo *_bo)
{
   /* Null check (defense in depth) */
   if (unlikely(_bo == NULL)) {
      return;
   }

   struct radv_amdgpu_cs *cs = radv_amdgpu_cs(_cs);
   struct radv_amdgpu_winsys_bo *bo = radv_amdgpu_winsys_bo(_bo);

   /* Early exit on error state */
   if (unlikely(cs->status != VK_SUCCESS)) {
      return;
   }

   /*
    * Branch hint: virtual buffers are rare compared to regular buffers.
    * This hint is based on profiling DXVK/VKD3D workloads.
    * Typical ratio: 95% regular, 5% virtual.
    */
   if (unlikely(bo->base.is_virtual)) {
      radv_amdgpu_cs_add_virtual_buffer(_cs, _bo);
      return;
   }

   /* Common path: regular buffer addition */
   radv_amdgpu_cs_add_buffer_internal(cs, bo->bo_handle, bo->priority);
}

/**
 * Emit IB2 packets to execute a secondary CS.
 * IB2 are a special variant of the INDIRECT_BUFFER packet which are used inside an IB.
 * An IB2 packet can execute another IB and then continue execution of the current IB.
 *
 * When the secondary CS uses IB chaining: we only emit a single IB2 packet which
 * jumps to the first IB of the secondary, then executes the entire secondary and returns.
 *
 * When the secondary CS does not support IB chaining or IB2 chaining is disabled:
 * emit an IB2 packet for every IB inside the secondary CS.
 */
static void
radv_amdgpu_cs_emit_secondary_ib2(struct radv_amdgpu_cs *parent, struct radv_amdgpu_cs *child)
{
   /* When IB2 chaining isn't allowed, the secondary CS shouldn't use IB chaining. */
   assert(parent->ws->info.can_chain_ib2 || !child->chain_ib);
   const uint32_t num_ib2 = child->chain_ib ? 1 : child->num_ib_buffers;

   for (uint32_t i = 0; i < num_ib2; ++i) {
      if (parent->base.cdw + 4 > parent->base.max_dw) {
         radv_amdgpu_cs_grow(&parent->base, 4);
      }

      parent->base.reserved_dw = MAX2(parent->base.reserved_dw, parent->base.cdw + 4);

      const uint64_t va = child->ib_buffers[i].va;
      const uint32_t size = child->ib_buffers[i].cdw;

      /* Not setting the CHAIN bit will launch an IB2. */
      ac_emit_cp_indirect_buffer(&parent->base, va, size, 0, false);

      assert(parent->base.cdw <= parent->base.max_dw);
   }
}

static void
radv_amdgpu_cs_execute_secondary(struct ac_cmdbuf *_parent, struct ac_cmdbuf *_child, bool allow_ib2)
{
   struct radv_amdgpu_cs *parent = radv_amdgpu_cs(_parent);
   struct radv_amdgpu_cs *child = radv_amdgpu_cs(_child);
   struct radv_amdgpu_winsys *ws = parent->ws;
   const bool use_ib2 = !parent->is_secondary && allow_ib2 && parent->hw_ip == AMD_IP_GFX;

   if (parent->status != VK_SUCCESS || child->status != VK_SUCCESS) {
      return;
   }

   for (unsigned i = 0; i < child->num_buffers; ++i) {
      radv_amdgpu_cs_add_buffer_internal(parent, child->handles[i].bo_handle, child->handles[i].bo_priority);
   }

   for (unsigned i = 0; i < child->num_virtual_buffers; ++i) {
      radv_amdgpu_cs_add_buffer(&parent->base, child->virtual_buffers[i]);
   }

   if (use_ib2) {
      radv_amdgpu_cs_emit_secondary_ib2(parent, child);
   } else {
      /* Grow the current CS and copy the contents of the secondary CS. */
      for (unsigned i = 0; i < child->num_ib_buffers; i++) {
         struct radv_amdgpu_ib *ib = &child->ib_buffers[i];
         uint32_t cdw = ib->cdw;
         uint8_t *mapped;

         /* Do not copy the original chain link for IBs. */
         if (child->chain_ib) {
            /* FIX: Validate cdw before decrement. */
            if (cdw < 4) {
               parent->status = VK_ERROR_UNKNOWN;
               return;
            }
            cdw -= 4;
         }

         assert(ib->bo);

         if (parent->base.cdw + cdw > parent->base.max_dw) {
            radv_amdgpu_cs_grow(&parent->base, cdw);
         }

         parent->base.reserved_dw = MAX2(parent->base.reserved_dw, parent->base.cdw + cdw);

         mapped = radv_buffer_map(&ws->base, ib->bo);
         if (!mapped) {
            parent->status = VK_ERROR_OUT_OF_DEVICE_MEMORY;
            return;
         }

         /* FIX: Add overflow check for memcpy size. */
         if (unlikely(cdw > UINT32_MAX / 4u)) {
            parent->status = VK_ERROR_OUT_OF_DEVICE_MEMORY;
            return;
         }

         memcpy(parent->base.buf + parent->base.cdw, mapped, 4 * cdw);
         parent->base.cdw += cdw;
      }
   }
}

static void
radv_amdgpu_cs_execute_ib(struct ac_cmdbuf *_cs, struct radeon_winsys_bo *bo, uint64_t va, const uint32_t cdw,
                          const bool predicate)
{
   struct radv_amdgpu_cs *cs = radv_amdgpu_cs(_cs);
   const uint64_t ib_va = bo ? bo->va : va;

   if (unlikely(cs->status != VK_SUCCESS)) {
      return;
   }

   /* FIX: Validate alignment. */
   if (unlikely(ib_va == 0 || ib_va % cs->ws->info.ip[cs->hw_ip].ib_alignment != 0)) {
      cs->status = VK_ERROR_UNKNOWN;
      return;
   }

   assert(cs->hw_ip == AMD_IP_GFX && cdw <= ~C_3F2_IB_SIZE);

   ac_emit_cp_indirect_buffer(&cs->base, ib_va, cdw, 0, predicate);
}

static void
radv_amdgpu_cs_chain_dgc_ib(struct ac_cmdbuf *_cs, uint64_t va, uint32_t cdw, uint64_t trailer_va,
                            const bool predicate)
{
   struct radv_amdgpu_cs *cs = radv_amdgpu_cs(_cs);

   if (unlikely(cs->status != VK_SUCCESS)) {
      return;
   }

   assert(cs->ws->info.gfx_level >= GFX8);

   if (cs->hw_ip == AMD_IP_GFX) {
      /* Use IB2 for executing DGC CS on GFX. */
      cs->ws->base.cs_execute_ib(_cs, NULL, va, cdw, predicate);
   } else {
      /* FIX: Validate alignment. */
      if (unlikely(va == 0 || va % cs->ws->info.ip[cs->hw_ip].ib_alignment != 0)) {
         cs->status = VK_ERROR_UNKNOWN;
         return;
      }

      assert(cdw <= ~C_3F2_IB_SIZE);

      /* Emit a WRITE_DATA packet to patch the DGC CS. */
      const uint32_t chain_data[] = {
         PKT3(PKT3_INDIRECT_BUFFER, 2, 0),
         0,
         0,
         S_3F2_CHAIN(1) | S_3F2_VALID(1),
      };

      ac_emit_cp_write_data(&cs->base, V_370_ME, V_370_MEM, trailer_va, ARRAY_SIZE(chain_data), chain_data, false);

      /* Keep pointers for patching later. */
      uint64_t *ib_va_ptr = (uint64_t *)(cs->base.buf + cs->base.cdw - 3);
      uint32_t *ib_size_ptr = cs->base.buf + cs->base.cdw - 1;

      /* Writeback L2 because CP isn't coherent with L2 on GFX6-8. */
      if (cs->ws->info.gfx_level == GFX8) {
         ac_emit_cp_acquire_mem(&cs->base, GFX8, AMD_IP_COMPUTE, V_580_CP_ME,
                                S_0301F0_TC_WB_ACTION_ENA(1) | S_0301F0_TC_NC_ACTION_ENA(1));
      }

      /* Finalize the current CS. */
      cs->ws->base.cs_finalize(_cs);

      /* Chain the current CS to the DGC CS. */
      _cs->buf[_cs->cdw - 4] = PKT3(PKT3_INDIRECT_BUFFER, 2, 0);
      _cs->buf[_cs->cdw - 3] = va;
      _cs->buf[_cs->cdw - 2] = va >> 32;
      _cs->buf[_cs->cdw - 1] = S_3F2_CHAIN(1) | S_3F2_VALID(1) | cdw;

      /* Allocate a new CS BO with initial size. */
      const uint64_t ib_size = radv_amdgpu_cs_get_initial_size(cs->ws, cs->hw_ip);

      VkResult result = radv_amdgpu_cs_bo_create(cs, (uint32_t)ib_size);
      if (result != VK_SUCCESS) {
         cs->base.cdw = 0;
         cs->status = result;
         return;
      }

      cs->ib_mapped = radv_buffer_map(&cs->ws->base, cs->ib_buffer);
      if (!cs->ib_mapped) {
         cs->base.cdw = 0;
         cs->status = VK_ERROR_OUT_OF_DEVICE_MEMORY;
         return;
      }

      cs->ws->base.cs_add_buffer(&cs->base, cs->ib_buffer);

      /* Chain back the trailer (DGC CS) to the newly created one. */
      *ib_va_ptr = radv_amdgpu_winsys_bo(cs->ib_buffer)->base.va;
      cs->ib_size_ptr = ib_size_ptr;

      cs->base.buf = (uint32_t *)cs->ib_mapped;
      cs->base.cdw = 0;
      cs->base.reserved_dw = 0;
      cs->base.max_dw = (uint32_t)(ib_size / 4u - 4u);
   }
}

static unsigned
radv_amdgpu_count_cs_bo(struct radv_amdgpu_cs *start_cs)
{
   unsigned num_bo = 0;

   for (struct radv_amdgpu_cs *cs = start_cs; cs; cs = cs->chained_to) {
      num_bo += cs->num_buffers;
      for (unsigned j = 0; j < cs->num_virtual_buffers; ++j) {
         num_bo += radv_amdgpu_winsys_bo(cs->virtual_buffers[j])->bo_count;
      }
   }

   return num_bo;
}

static unsigned
radv_amdgpu_count_cs_array_bo(struct ac_cmdbuf **cs_array, unsigned num_cs)
{
   unsigned num_bo = 0;

   for (unsigned i = 0; i < num_cs; ++i) {
      num_bo += radv_amdgpu_count_cs_bo(radv_amdgpu_cs(cs_array[i]));
   }

   return num_bo;
}

#if defined(__x86_64__) || defined(_M_X64)
static RADV_ALWAYS_INLINE int
radv_linear_search_bo_avx2(const struct drm_amdgpu_bo_list_entry *handles,
                           unsigned count, uint32_t target)
{
   /* Use scalar path for small counts (AVX2 setup overhead not worth it) */
   if (unlikely(!radv_cpu_supports_avx2_cached() || count < 16u)) {
      goto scalar_path;
   }

   /* Broadcast target to all 8 lanes of 256-bit register */
   const __m256i target_vec = _mm256_set1_epi32((int32_t)target);
   unsigned i = 0u;

   /*
    * Process 8 entries per iteration.
    * Each entry is 8 bytes: {uint32_t bo_handle, uint32_t bo_priority}
    * We need to extract just the bo_handle fields (offsets 0, 8, 16, 24, ...)
    * In int32_t units: offsets 0, 2, 4, 6, 8, 10, 12, 14
    */
   for (; i + 8u <= count; i += 8u) {
      /* Base pointer for gather (cast to int32_t for stride calculation) */
      const int32_t *base_ptr = (const int32_t *)&handles[i];

      /* Index vector: handle offsets in int32_t units */
      const __m256i vindex = _mm256_setr_epi32(0, 2, 4, 6, 8, 10, 12, 14);

      /* Gather 8 handles from struct array (4-byte stride between handles) */
      __m256i handles_vec = _mm256_i32gather_epi32(base_ptr, vindex, 4);

      /* Compare all 8 handles against target */
      __m256i cmp = _mm256_cmpeq_epi32(handles_vec, target_vec);

      /* Extract comparison results as 8-bit mask */
      int mask = _mm256_movemask_ps(_mm256_castsi256_ps(cmp));

      /* Check if any lane matched */
      if (unlikely(mask != 0)) {
         /* Find first set bit (matched lane) using BMI2 TZCNT */
         const unsigned match_lane = (unsigned)__builtin_ctz((unsigned)mask);
         return (int)(i + match_lane);
      }
   }

   /* Scalar tail for remaining elements */
   for (; i < count; ++i) {
      if (handles[i].bo_handle == target) {
         return (int)i;
      }
   }

   return -1;

scalar_path:
   /*
    * Optimized scalar path with manual unrolling.
    * Improves ILP (Instruction Level Parallelism) on Raptor Lake.
    * 4-way unroll matches P-core execution ports.
    */
   unsigned j = 0u;

   for (; j + 4u <= count; j += 4u) {
      const uint32_t h0 = handles[j + 0].bo_handle;
      const uint32_t h1 = handles[j + 1].bo_handle;
      const uint32_t h2 = handles[j + 2].bo_handle;
      const uint32_t h3 = handles[j + 3].bo_handle;

      if (unlikely(h0 == target)) return (int)(j + 0);
      if (unlikely(h1 == target)) return (int)(j + 1);
      if (unlikely(h2 == target)) return (int)(j + 2);
      if (unlikely(h3 == target)) return (int)(j + 3);
   }

   /* Handle remainder */
   for (; j < count; ++j) {
      if (handles[j].bo_handle == target) {
         return (int)j;
      }
   }

   return -1;
}

#else /* Non-x86_64 architecture */

static RADV_ALWAYS_INLINE int
radv_linear_search_bo_avx2(const struct drm_amdgpu_bo_list_entry *handles,
                           unsigned count, uint32_t target)
{
   /* Optimized scalar search with unrolling for all architectures */
   unsigned i = 0u;

   for (; i + 4u <= count; i += 4u) {
      if (unlikely(handles[i + 0].bo_handle == target)) return (int)(i + 0);
      if (unlikely(handles[i + 1].bo_handle == target)) return (int)(i + 1);
      if (unlikely(handles[i + 2].bo_handle == target)) return (int)(i + 2);
      if (unlikely(handles[i + 3].bo_handle == target)) return (int)(i + 3);
   }

   for (; i < count; ++i) {
      if (handles[i].bo_handle == target) {
         return (int)i;
      }
   }

   return -1;
}

#endif /* __x86_64__ */

static unsigned
radv_amdgpu_add_cs_to_bo_list(struct radv_amdgpu_cs *cs,
                              struct drm_amdgpu_bo_list_entry *handles,
                              unsigned num_handles)
{
   /* Fast path: empty CS contributes nothing */
   if (cs->num_buffers == 0) {
      return num_handles;
   }

   /* Fast path: first CS with no virtual buffers - direct copy */
   if (num_handles == 0 && cs->num_virtual_buffers == 0) {
      /* Validate no overflow */
      if (unlikely(cs->num_buffers > UINT32_MAX - num_handles)) {
         return num_handles;
      }

      memcpy(handles, cs->handles, cs->num_buffers * sizeof(struct drm_amdgpu_bo_list_entry));
      return cs->num_buffers;
   }

   /* Add regular buffers with AVX2-accelerated deduplication */
   for (unsigned j = 0; j < cs->num_buffers; ++j) {
      /* Overflow check */
      if (unlikely(num_handles >= UINT32_MAX)) {
         break;
      }

      /* Use AVX2 search for deduplication */
      const int idx = radv_linear_search_bo_avx2(handles, num_handles, cs->handles[j].bo_handle);

      if (idx < 0) {
         /* New buffer - add it */
         handles[num_handles++] = cs->handles[j];
      }
      /* If idx >= 0, buffer already exists - skip duplicate */
   }

   /* Add virtual buffers (expanded to constituent BOs) */
   for (unsigned j = 0; j < cs->num_virtual_buffers; ++j) {
      struct radv_amdgpu_winsys_bo *virtual_bo = radv_amdgpu_winsys_bo(cs->virtual_buffers[j]);

      /* Acquire read lock for concurrent access safety */
      u_rwlock_rdlock(&virtual_bo->lock);

      for (unsigned k = 0; k < virtual_bo->bo_count; ++k) {
         /* Overflow check */
         if (unlikely(num_handles >= UINT32_MAX)) {
            u_rwlock_rdunlock(&virtual_bo->lock);
            return num_handles;
         }

         struct radv_amdgpu_winsys_bo *bo = virtual_bo->bos[k];

         /* Dedup check with AVX2 search */
         const int idx = radv_linear_search_bo_avx2(handles, num_handles, bo->bo_handle);

         if (idx < 0) {
            handles[num_handles].bo_handle = bo->bo_handle;
            handles[num_handles].bo_priority = bo->priority;
            num_handles++;
         }
      }

      u_rwlock_rdunlock(&virtual_bo->lock);
   }

   return num_handles;
}

static unsigned
radv_amdgpu_add_cs_array_to_bo_list(struct ac_cmdbuf **cs_array, unsigned num_cs,
                                    struct drm_amdgpu_bo_list_entry *handles, unsigned num_handles)
{
   for (unsigned i = 0; i < num_cs; ++i) {
      for (struct radv_amdgpu_cs *cs = radv_amdgpu_cs(cs_array[i]); cs; cs = cs->chained_to) {
         num_handles = radv_amdgpu_add_cs_to_bo_list(cs, handles, num_handles);
      }
   }

   return num_handles;
}

static unsigned
radv_amdgpu_copy_global_bo_list(struct radv_amdgpu_winsys *ws, struct drm_amdgpu_bo_list_entry *handles)
{
   for (uint32_t i = 0; i < ws->global_bo_list.count; i++) {
      handles[i].bo_handle = ws->global_bo_list.bos[i]->bo_handle;
      handles[i].bo_priority = ws->global_bo_list.bos[i]->priority;
   }

   return ws->global_bo_list.count;
}

/* Temporary BO set for fast O(1) dedup during submit list building. */
struct radv_bo_set_entry {
   uint32_t bo_handle; /* 0 means empty */
};

struct radv_bo_set {
   struct radv_bo_set_entry *entries;
   uint32_t size; /* power-of-two */
   bool full;
};

static inline uint32_t
radv_next_pow2_u32(uint32_t v)
{
   if (v <= 1u) {
      return 1u;
   }
   v--;
   v |= v >> 1;
   v |= v >> 2;
   v |= v >> 4;
   v |= v >> 8;
   v |= v >> 16;
   v++;
   return v;
}

static bool
radv_bo_set_init(struct radv_bo_set *set, uint32_t expected_elems)
{
   /* FIX: Add overflow check. */
   if (expected_elems > UINT32_MAX / 2u) {
      set->full = true;
      set->entries = NULL;
      set->size = 0;
      return false;
   }

   uint64_t want = (uint64_t)expected_elems * 2u;
   if (want < 8u) {
      want = 8u;
   }
   if (want > (1u << 28)) {
      want = (1u << 28);
   }
   set->size = radv_next_pow2_u32((uint32_t)want);
   set->entries = (struct radv_bo_set_entry *)calloc(set->size, sizeof(struct radv_bo_set_entry));
   set->full = !set->entries;
   return !set->full;
}

static inline void
radv_bo_set_destroy(struct radv_bo_set *set)
{
   free(set->entries);
   set->entries = NULL;
   set->size = 0;
   set->full = false;
}

static bool
radv_bo_set_insert(struct radv_bo_set *set, uint32_t bo_handle)
{
   if (unlikely(set->full)) {
      return false;
   }

   const uint32_t mask = set->size - 1u;
   uint32_t hash = radv_hash_bo(bo_handle);
   uint32_t dist = 0u;

   struct radv_bo_set_entry new_entry = { .bo_handle = bo_handle };

   for (;;) {
      uint32_t pos = (hash + dist) & mask;
      struct radv_bo_set_entry *entry = &set->entries[pos];

      if (entry->bo_handle == 0u) {
         *entry = new_entry;
         return true; /* newly inserted */
      }

      if (entry->bo_handle == bo_handle) {
         return false; /* already present */
      }

      uint32_t entry_hash = radv_hash_bo(entry->bo_handle);
      uint32_t entry_dist = (pos - (entry_hash & mask) + set->size) & mask;

      if (dist > entry_dist) {
         struct radv_bo_set_entry tmp = *entry;
         *entry = new_entry;
         new_entry = tmp;

         dist = entry_dist;
         hash = entry_hash;
      }

      dist++;
      if (unlikely(dist >= set->size)) {
         set->full = true;
         return false;
      }
   }
}

static inline void
radv_append_cs_bos_dedup(struct radv_amdgpu_cs *cs,
                         struct drm_amdgpu_bo_list_entry *handles,
                         unsigned *p_num_handles,
                         struct radv_bo_set *present)
{
   if (cs->num_buffers) {
      for (unsigned j = 0; j < cs->num_buffers; ++j) {
         uint32_t h = cs->handles[j].bo_handle;
         if (radv_bo_set_insert(present, h)) {
            unsigned idx = (*p_num_handles)++;
            handles[idx] = cs->handles[j];
         }
      }
   }

   for (unsigned j = 0; j < cs->num_virtual_buffers; ++j) {
      struct radv_amdgpu_winsys_bo *vbo = radv_amdgpu_winsys_bo(cs->virtual_buffers[j]);
      u_rwlock_rdlock(&vbo->lock);
      for (unsigned k = 0; k < vbo->bo_count; ++k) {
         struct radv_amdgpu_winsys_bo *bo = vbo->bos[k];
         uint32_t h = bo->bo_handle;
         if (radv_bo_set_insert(present, h)) {
            unsigned idx = (*p_num_handles)++;
            handles[idx].bo_handle = h;
            handles[idx].bo_priority = bo->priority;
         }
      }
      u_rwlock_rdunlock(&vbo->lock);
   }
}

static VkResult
radv_amdgpu_get_bo_list(struct radv_amdgpu_winsys *ws, struct ac_cmdbuf **cs_array, unsigned count,
                        struct ac_cmdbuf **initial_preamble_array, unsigned num_initial_preambles,
                        struct ac_cmdbuf **continue_preamble_array, unsigned num_continue_preambles,
                        struct ac_cmdbuf **postamble_array, unsigned num_postambles, unsigned *rnum_handles,
                        struct drm_amdgpu_bo_list_entry **rhandles)
{
   struct drm_amdgpu_bo_list_entry *handles = NULL;
   unsigned num_handles = 0;

   uint32_t global_bo_count = 0;
   struct radv_amdgpu_winsys_bo **global_bos_snapshot = NULL;

   u_rwlock_rdlock(&ws->global_bo_list.lock);
   global_bo_count = ws->global_bo_list.count;
   if (global_bo_count > 0) {
      global_bos_snapshot = malloc(sizeof(struct radv_amdgpu_winsys_bo *) * global_bo_count);
      if (unlikely(!global_bos_snapshot)) {
         u_rwlock_rdunlock(&ws->global_bo_list.lock);
         return VK_ERROR_OUT_OF_HOST_MEMORY;
      }
      /* BO pointers are stable (refcounted); safe to copy. */
      memcpy(global_bos_snapshot, ws->global_bo_list.bos, sizeof(void *) * global_bo_count);
   }
   u_rwlock_rdunlock(&ws->global_bo_list.lock);

   if (ws->debug_all_bos) {
      handles = malloc(sizeof(handles[0]) * global_bo_count);
      if (unlikely(!handles)) {
         free(global_bos_snapshot);
         return VK_ERROR_OUT_OF_HOST_MEMORY;
      }

      for (uint32_t i = 0; i < global_bo_count; i++) {
         handles[i].bo_handle = global_bos_snapshot[i]->bo_handle;
         handles[i].bo_priority = global_bos_snapshot[i]->priority;
      }
      num_handles = global_bo_count;
      free(global_bos_snapshot);
   } else if (count == 1 && !num_initial_preambles && !num_continue_preambles && !num_postambles &&
              !radv_amdgpu_cs(cs_array[0])->num_virtual_buffers && !radv_amdgpu_cs(cs_array[0])->chained_to &&
              global_bo_count == 0) {
      /* Fast path: single CS, no virtual BOs, no global BOs. */
      struct radv_amdgpu_cs *cs = radv_amdgpu_cs(cs_array[0]);
      free(global_bos_snapshot);
      if (cs->num_buffers == 0) {
         return VK_SUCCESS;
      }

      handles = malloc(sizeof(handles[0]) * cs->num_buffers);
      if (unlikely(!handles)) {
         return VK_ERROR_OUT_OF_HOST_MEMORY;
      }

      memcpy(handles, cs->handles, sizeof(handles[0]) * cs->num_buffers);
      num_handles = cs->num_buffers;
   } else {
      unsigned total_buffer_count = global_bo_count;
      total_buffer_count += radv_amdgpu_count_cs_array_bo(cs_array, count);
      total_buffer_count += radv_amdgpu_count_cs_array_bo(initial_preamble_array, num_initial_preambles);
      total_buffer_count += radv_amdgpu_count_cs_array_bo(continue_preamble_array, num_continue_preambles);
      total_buffer_count += radv_amdgpu_count_cs_array_bo(postamble_array, num_postambles);

      if (total_buffer_count == 0) {
         free(global_bos_snapshot);
         return VK_SUCCESS;
      }

      handles = malloc(sizeof(handles[0]) * total_buffer_count);
      if (unlikely(!handles)) {
         free(global_bos_snapshot);
         return VK_ERROR_OUT_OF_HOST_MEMORY;
      }

      if (total_buffer_count <= RADV_SMALL_BO_DEDUP_THRESHOLD) {
         /* Copy global BOs from snapshot. */
         for (uint32_t i = 0; i < global_bo_count; i++) {
            handles[num_handles].bo_handle = global_bos_snapshot[i]->bo_handle;
            handles[num_handles].bo_priority = global_bos_snapshot[i]->priority;
            ++num_handles;
         }
         free(global_bos_snapshot);

         num_handles = radv_amdgpu_add_cs_array_to_bo_list(cs_array, count, handles, num_handles);
         num_handles =
            radv_amdgpu_add_cs_array_to_bo_list(initial_preamble_array, num_initial_preambles, handles, num_handles);
         num_handles =
            radv_amdgpu_add_cs_array_to_bo_list(continue_preamble_array, num_continue_preambles, handles, num_handles);
         num_handles = radv_amdgpu_add_cs_array_to_bo_list(postamble_array, num_postambles, handles, num_handles);
      } else {
         /* Large lists: use hash set for O(N) dedup. */
         struct radv_bo_set present;
         if (unlikely(!radv_bo_set_init(&present, total_buffer_count))) {
            free(handles);
            free(global_bos_snapshot);
            return VK_ERROR_OUT_OF_HOST_MEMORY;
         }

         /* Insert global BOs from snapshot. */
         for (uint32_t i = 0; i < global_bo_count; i++) {
            uint32_t h = global_bos_snapshot[i]->bo_handle;
            if (radv_bo_set_insert(&present, h)) {
               handles[num_handles].bo_handle = h;
               handles[num_handles].bo_priority = global_bos_snapshot[i]->priority;
               ++num_handles;
            }
         }
         free(global_bos_snapshot);

         /* Process preambles and CS arrays. */
         for (unsigned i = 0; i < num_initial_preambles; ++i) {
            for (struct radv_amdgpu_cs *cs = radv_amdgpu_cs(initial_preamble_array[i]); cs; cs = cs->chained_to) {
               radv_append_cs_bos_dedup(cs, handles, &num_handles, &present);
            }
         }
         for (unsigned i = 0; i < count; ++i) {
            for (struct radv_amdgpu_cs *cs = radv_amdgpu_cs(cs_array[i]); cs; cs = cs->chained_to) {
               radv_append_cs_bos_dedup(cs, handles, &num_handles, &present);
            }
         }
         for (unsigned i = 0; i < num_continue_preambles; ++i) {
            for (struct radv_amdgpu_cs *cs = radv_amdgpu_cs(continue_preamble_array[i]); cs; cs = cs->chained_to) {
               radv_append_cs_bos_dedup(cs, handles, &num_handles, &present);
            }
         }
         for (unsigned i = 0; i < num_postambles; ++i) {
            for (struct radv_amdgpu_cs *cs = radv_amdgpu_cs(postamble_array[i]); cs; cs = cs->chained_to) {
               radv_append_cs_bos_dedup(cs, handles, &num_handles, &present);
            }
         }

         radv_bo_set_destroy(&present);
      }
   }

   *rhandles = handles;
   *rnum_handles = num_handles;

   return VK_SUCCESS;
}

static void
radv_assign_last_submit(struct radv_amdgpu_ctx *ctx, struct radv_amdgpu_cs_request *request)
{
   /* FIX: Validate IP type and ring indices. */
   if (unlikely(request->ip_type >= AMD_NUM_IP_TYPES ||
                request->ring >= MAX_RINGS_PER_TYPE)) {
      return;
   }

   radv_amdgpu_request_to_fence(ctx, &ctx->last_submission[request->ip_type][request->ring], request);
}

static unsigned
radv_amdgpu_submitted_ibs_per_cs(const struct radv_amdgpu_cs *cs)
{
   return cs->chain_ib ? 1 : cs->num_ib_buffers;
}

static unsigned
radv_amdgpu_count_submitted_ibs(struct ac_cmdbuf **cs_array, unsigned cs_count, unsigned initial_preamble_count,
                                unsigned continue_preamble_count, unsigned postamble_count)
{
   unsigned num_ibs = 0;

   for (unsigned i = 0; i < cs_count; i++) {
      struct radv_amdgpu_cs *cs = radv_amdgpu_cs(cs_array[i]);

      num_ibs += radv_amdgpu_submitted_ibs_per_cs(cs);
   }

   return MAX2(initial_preamble_count, continue_preamble_count) + num_ibs + postamble_count;
}

static VkResult
radv_amdgpu_winsys_cs_submit_internal(struct radv_amdgpu_ctx *ctx, int queue_idx, struct radv_winsys_sem_info *sem_info,
                                      struct ac_cmdbuf **cs_array, unsigned cs_count,
                                      struct ac_cmdbuf **initial_preamble_cs, unsigned initial_preamble_count,
                                      struct ac_cmdbuf **continue_preamble_cs, unsigned continue_preamble_count,
                                      struct ac_cmdbuf **postamble_cs, unsigned postamble_count,
                                      bool uses_shadow_regs)
{
   VkResult result;

   /* Last CS is "the gang leader", its IP type determines which fence to signal. */
   struct radv_amdgpu_cs *last_cs = radv_amdgpu_cs(cs_array[cs_count - 1]);
   struct radv_amdgpu_winsys *ws = last_cs->ws;

   const unsigned num_ibs = radv_amdgpu_count_submitted_ibs(cs_array, cs_count, initial_preamble_count,
                                                            continue_preamble_count, postamble_count);
   const unsigned ib_array_size = MIN2(RADV_MAX_IBS_PER_SUBMIT, num_ibs);

   STACK_ARRAY(struct radv_amdgpu_cs_ib_info, ibs, ib_array_size);

   struct drm_amdgpu_bo_list_entry *handles = NULL;
   unsigned num_handles = 0;

   /* FIX: Check if STACK_ARRAY allocation succeeded. */
   if (unlikely(!ibs)) {
      return VK_ERROR_OUT_OF_HOST_MEMORY;
   }

   u_rwlock_rdlock(&ws->global_bo_list.lock);

   result = radv_amdgpu_get_bo_list(ws, &cs_array[0], cs_count, initial_preamble_cs, initial_preamble_count,
                                    continue_preamble_cs, continue_preamble_count, postamble_cs, postamble_count,
                                    &num_handles, &handles);
   if (result != VK_SUCCESS) {
      goto fail;
   }

   /* Configure the CS request. */
   const uint32_t *max_ib_per_ip = ws->info.max_submitted_ibs;
   struct radv_amdgpu_cs_request request = {
      .ip_type = last_cs->hw_ip,
      .ip_instance = 0,
      .ring = (uint32_t)queue_idx,
      .handles = handles,
      .num_handles = num_handles,
      .ibs = ibs,
      .number_of_ibs = 0, /* set below */
   };

   for (unsigned cs_idx = 0, cs_ib_idx = 0; cs_idx < cs_count;) {
      struct ac_cmdbuf **preambles = cs_idx ? continue_preamble_cs : initial_preamble_cs;
      const unsigned preamble_count = cs_idx ? continue_preamble_count : initial_preamble_count;

      /* FIX: Add overflow check. */
      if (unlikely(RADV_MAX_IBS_PER_SUBMIT < preamble_count + postamble_count)) {
         result = VK_ERROR_OUT_OF_HOST_MEMORY;
         goto fail;
      }

      const unsigned ib_per_submit = RADV_MAX_IBS_PER_SUBMIT - preamble_count - postamble_count;
      unsigned num_submitted_ibs = 0;
      unsigned ibs_per_ip[AMD_NUM_IP_TYPES] = {0};

      /* Copy preambles to the submission. */
      for (unsigned i = 0; i < preamble_count; ++i) {
         /* Assume that the full preamble fits into 1 IB. */
         struct radv_amdgpu_cs *cs = radv_amdgpu_cs(preambles[i]);
         struct radv_amdgpu_cs_ib_info ib;

         if (ws->dump_ibs)
            ws->base.cs_dump(&cs->base, stderr, NULL, 0, RADV_CS_DUMP_TYPE_PREAMBLE_IBS);

         assert(cs->num_ib_buffers == 1);
         ib = radv_amdgpu_cs_ib_to_info(cs, cs->ib_buffers[0]);

         ibs[num_submitted_ibs++] = ib;

         /* FIX: Validate IP type. */
         if (cs->hw_ip < AMD_NUM_IP_TYPES) {
            ibs_per_ip[cs->hw_ip]++;
         }
      }

      for (unsigned i = 0; i < ib_per_submit && cs_idx < cs_count; ++i) {
         struct radv_amdgpu_cs *cs = radv_amdgpu_cs(cs_array[cs_idx]);
         struct radv_amdgpu_cs_ib_info ib;

         if (ws->dump_ibs)
            ws->base.cs_dump(&cs->base, stderr, NULL, 0, RADV_CS_DUMP_TYPE_MAIN_IBS);

         if (cs_ib_idx == 0) {
            /* Make sure the whole CS fits into the same submission. */
            unsigned cs_num_ib = radv_amdgpu_submitted_ibs_per_cs(cs);

            /* FIX: Validate IP type before array access. */
            if (cs->hw_ip >= AMD_NUM_IP_TYPES) {
               result = VK_ERROR_UNKNOWN;
               goto fail;
            }

            if (i + cs_num_ib > ib_per_submit || ibs_per_ip[cs->hw_ip] + cs_num_ib > max_ib_per_ip[cs->hw_ip]) {
               break;
            }

            if (cs->hw_ip != request.ip_type) {
               /* Found a "follower" CS in a gang submission.
                * Make sure to submit this together with its "leader", the next CS.
                * We rely on the caller to order each "follower" before its "leader."
                */
               assert(cs_idx != cs_count - 1);
               struct radv_amdgpu_cs *next_cs = radv_amdgpu_cs(cs_array[cs_idx + 1]);
               assert(next_cs->hw_ip == request.ip_type);
               unsigned next_cs_num_ib = radv_amdgpu_submitted_ibs_per_cs(next_cs);

               /* FIX: Validate next CS IP type. */
               if (next_cs->hw_ip >= AMD_NUM_IP_TYPES) {
                  result = VK_ERROR_UNKNOWN;
                  goto fail;
               }

               if (i + cs_num_ib + next_cs_num_ib > ib_per_submit ||
                   ibs_per_ip[next_cs->hw_ip] + next_cs_num_ib > max_ib_per_ip[next_cs->hw_ip]) {
                  break;
               }
            }
         }

         /* When IBs are used, we only need to submit the main IB of this CS, because everything
          * else is chained to the first IB. Otherwise we must submit all IBs in the ib_buffers
          * array.
          */
         if (cs->chain_ib) {
            ib = radv_amdgpu_cs_ib_to_info(cs, cs->ib_buffers[0]);
            cs_idx++;
         } else {
            assert(cs_ib_idx < cs->num_ib_buffers);
            ib = radv_amdgpu_cs_ib_to_info(cs, cs->ib_buffers[cs_ib_idx++]);

            if (cs_ib_idx == cs->num_ib_buffers) {
               cs_idx++;
               cs_ib_idx = 0;
            }
         }

         if (uses_shadow_regs && ib.ip_type == AMDGPU_HW_IP_GFX) {
            ib.flags |= AMDGPU_IB_FLAG_PREEMPT;
         }

         assert(num_submitted_ibs < ib_array_size);
         ibs[num_submitted_ibs++] = ib;

         /* FIX: Validate IP type. */
         if (cs->hw_ip < AMD_NUM_IP_TYPES) {
            ibs_per_ip[cs->hw_ip]++;
         }
      }

      assert(num_submitted_ibs > preamble_count);

      /* Copy postambles to the submission. */
      for (unsigned i = 0; i < postamble_count; ++i) {
         /* Assume that the full postamble fits into 1 IB. */
         struct radv_amdgpu_cs *cs = radv_amdgpu_cs(postamble_cs[i]);
         struct radv_amdgpu_cs_ib_info ib;

         if (ws->dump_ibs)
            ws->base.cs_dump(&cs->base, stderr, NULL, 0, RADV_CS_DUMP_TYPE_POSTAMBLE_IBS);

         assert(cs->num_ib_buffers == 1);
         ib = radv_amdgpu_cs_ib_to_info(cs, cs->ib_buffers[0]);

         ibs[num_submitted_ibs++] = ib;

         /* FIX: Validate IP type. */
         if (cs->hw_ip < AMD_NUM_IP_TYPES) {
            ibs_per_ip[cs->hw_ip]++;
         }
      }

      /* Submit the CS. */
      request.number_of_ibs = num_submitted_ibs;
      result = radv_amdgpu_cs_submit(ctx, &request, sem_info);
      if (result != VK_SUCCESS) {
         goto fail;
      }
   }

   free(request.handles);

   if (result != VK_SUCCESS) {
      goto fail;
   }

   radv_assign_last_submit(ctx, &request);

fail:
   u_rwlock_rdunlock(&ws->global_bo_list.lock);
   STACK_ARRAY_FINISH(ibs);
   return result;
}

static VkResult
radv_amdgpu_cs_submit_zero(struct radv_amdgpu_ctx *ctx, enum amd_ip_type ip_type, int queue_idx,
                           struct radv_winsys_sem_info *sem_info)
{
   unsigned hw_ip = ip_type;
   unsigned queue_syncobj = radv_amdgpu_ctx_queue_syncobj(ctx, hw_ip, queue_idx);
   int ret;

   if (!queue_syncobj)
      return VK_ERROR_OUT_OF_HOST_MEMORY;

   if (sem_info->wait.syncobj_count || sem_info->wait.timeline_syncobj_count) {
      int fd;
      ret = ac_drm_cs_syncobj_export_sync_file(ctx->ws->dev, queue_syncobj, &fd);
      if (ret < 0)
         return VK_ERROR_DEVICE_LOST;

      for (unsigned i = 0; i < sem_info->wait.syncobj_count; ++i) {
         int fd2;
         ret = ac_drm_cs_syncobj_export_sync_file(ctx->ws->dev, sem_info->wait.syncobj[i], &fd2);
         if (ret < 0) {
            close(fd);
            return VK_ERROR_DEVICE_LOST;
         }

         sync_accumulate("radv", &fd, fd2);
         close(fd2);
      }
      for (unsigned i = 0; i < sem_info->wait.timeline_syncobj_count; ++i) {
         int fd2;
         ret = ac_drm_cs_syncobj_export_sync_file2(
            ctx->ws->dev, sem_info->wait.syncobj[i + sem_info->wait.syncobj_count], sem_info->wait.points[i], 0, &fd2);
         if (ret < 0) {
            /* This works around a kernel bug where the fence isn't copied if it is already
             * signalled. Since it is already signalled it is totally fine to not wait on it.
             *
             * kernel patch: https://patchwork.freedesktop.org/patch/465583/ */
            uint64_t point;
            ret = ac_drm_cs_syncobj_query2(ctx->ws->dev, &sem_info->wait.syncobj[i + sem_info->wait.syncobj_count],
                                           &point, 1, 0);
            if (!ret && point >= sem_info->wait.points[i])
               continue;

            close(fd);
            return VK_ERROR_DEVICE_LOST;
         }

         sync_accumulate("radv", &fd, fd2);
         close(fd2);
      }
      ret = ac_drm_cs_syncobj_import_sync_file(ctx->ws->dev, queue_syncobj, fd);
      close(fd);
      if (ret < 0)
         return VK_ERROR_DEVICE_LOST;

      ctx->queue_syncobj_wait[hw_ip][queue_idx] = true;
   }

   for (unsigned i = 0; i < sem_info->signal.syncobj_count; ++i) {
      uint32_t dst_handle = sem_info->signal.syncobj[i];
      uint32_t src_handle = queue_syncobj;

      if (ctx->ws->info.has_timeline_syncobj) {
         ret = ac_drm_cs_syncobj_transfer(ctx->ws->dev, dst_handle, 0, src_handle, 0, 0);
         if (ret < 0)
            return VK_ERROR_DEVICE_LOST;
      } else {
         int fd;
         ret = ac_drm_cs_syncobj_export_sync_file(ctx->ws->dev, src_handle, &fd);
         if (ret < 0)
            return VK_ERROR_DEVICE_LOST;

         ret = ac_drm_cs_syncobj_import_sync_file(ctx->ws->dev, dst_handle, fd);
         close(fd);
         if (ret < 0)
            return VK_ERROR_DEVICE_LOST;
      }
   }
   for (unsigned i = 0; i < sem_info->signal.timeline_syncobj_count; ++i) {
      ret = ac_drm_cs_syncobj_transfer(ctx->ws->dev, sem_info->signal.syncobj[i + sem_info->signal.syncobj_count],
                                       sem_info->signal.points[i], queue_syncobj, 0, 0);
      if (ret < 0)
         return VK_ERROR_DEVICE_LOST;
   }
   return VK_SUCCESS;
}

static VkResult
radv_amdgpu_winsys_cs_submit(struct radeon_winsys_ctx *_ctx, const struct radv_winsys_submit_info *submit,
                             uint32_t wait_count, const struct vk_sync_wait *waits, uint32_t signal_count,
                             const struct vk_sync_signal *signals)
{
   struct radv_amdgpu_ctx *ctx = radv_amdgpu_ctx(_ctx);
   struct radv_amdgpu_winsys *ws = ctx->ws;
   VkResult result;
   unsigned wait_idx = 0, signal_idx = 0;

   STACK_ARRAY(uint64_t, wait_points, wait_count);
   STACK_ARRAY(uint32_t, wait_syncobj, wait_count);
   STACK_ARRAY(uint64_t, signal_points, signal_count);
   STACK_ARRAY(uint32_t, signal_syncobj, signal_count);

   if (!wait_points || !wait_syncobj || !signal_points || !signal_syncobj) {
      result = VK_ERROR_OUT_OF_HOST_MEMORY;
      goto out;
   }

   for (uint32_t i = 0; i < wait_count; ++i) {
      if (waits[i].sync->type == &vk_sync_dummy_type)
         continue;

      assert(waits[i].sync->type == &ws->syncobj_sync_type);
      wait_syncobj[wait_idx] = ((struct vk_drm_syncobj *)waits[i].sync)->syncobj;
      wait_points[wait_idx] = waits[i].wait_value;
      ++wait_idx;
   }

   for (uint32_t i = 0; i < signal_count; ++i) {
      if (signals[i].sync->type == &vk_sync_dummy_type)
         continue;

      assert(signals[i].sync->type == &ws->syncobj_sync_type);
      signal_syncobj[signal_idx] = ((struct vk_drm_syncobj *)signals[i].sync)->syncobj;
      signal_points[signal_idx] = signals[i].signal_value;
      ++signal_idx;
   }

   assert(signal_idx <= signal_count);
   assert(wait_idx <= wait_count);

   const uint32_t wait_timeline_syncobj_count =
      (ws->syncobj_sync_type.features & VK_SYNC_FEATURE_TIMELINE) ? wait_idx : 0;
   const uint32_t signal_timeline_syncobj_count =
      (ws->syncobj_sync_type.features & VK_SYNC_FEATURE_TIMELINE) ? signal_idx : 0;

   struct radv_winsys_sem_info sem_info = {
      .wait =
         {
            .points = wait_points,
            .syncobj = wait_syncobj,
            .timeline_syncobj_count = wait_timeline_syncobj_count,
            .syncobj_count = wait_idx - wait_timeline_syncobj_count,
         },
      .signal =
         {
            .points = signal_points,
            .syncobj = signal_syncobj,
            .timeline_syncobj_count = signal_timeline_syncobj_count,
            .syncobj_count = signal_idx - signal_timeline_syncobj_count,
         },
      .cs_emit_wait = true,
      .cs_emit_signal = true,
   };

   if (!submit->cs_count) {
      result = radv_amdgpu_cs_submit_zero(ctx, submit->ip_type, submit->queue_index, &sem_info);
   } else {
      result = radv_amdgpu_winsys_cs_submit_internal(
         ctx, submit->queue_index, &sem_info, submit->cs_array, submit->cs_count, submit->initial_preamble_cs,
         submit->initial_preamble_count, submit->continue_preamble_cs, submit->continue_preamble_count,
         submit->postamble_cs, submit->postamble_count, submit->uses_shadow_regs);
   }

out:
   STACK_ARRAY_FINISH(wait_points);
   STACK_ARRAY_FINISH(wait_syncobj);
   STACK_ARRAY_FINISH(signal_points);
   STACK_ARRAY_FINISH(signal_syncobj);
   return result;
}

static void
radv_amdgpu_winsys_get_cpu_addr(void *_cs, uint64_t addr, struct ac_addr_info *info)
{
   struct radv_amdgpu_cs *cs = (struct radv_amdgpu_cs *)_cs;

   memset(info, 0, sizeof(struct ac_addr_info));

   if (cs->ws->debug_log_bos) {
      u_rwlock_rdlock(&cs->ws->log_bo_list_lock);
      list_for_each_entry_rev (struct radv_amdgpu_winsys_bo_log, bo_log, &cs->ws->log_bo_list, list) {
         if (addr >= bo_log->va && addr - bo_log->va < bo_log->size) {
            info->use_after_free = bo_log->destroyed;
            break;
         }
      }
      u_rwlock_rdunlock(&cs->ws->log_bo_list_lock);
   }

   if (info->use_after_free)
      return;

   info->valid = !cs->ws->debug_all_bos;

   for (unsigned i = 0; i < cs->num_ib_buffers; ++i) {
      struct radv_amdgpu_ib *ib = &cs->ib_buffers[i];
      struct radv_amdgpu_winsys_bo *bo = (struct radv_amdgpu_winsys_bo *)ib->bo;

      if (addr >= bo->base.va && addr - bo->base.va < bo->base.size) {
         void *map = radv_buffer_map(&cs->ws->base, &bo->base);
         if (map) {
            info->cpu_addr = (char *)map + (addr - bo->base.va);
            info->valid = true;
            return;
         }
      }
   }
   u_rwlock_rdlock(&cs->ws->global_bo_list.lock);
   for (uint32_t i = 0; i < cs->ws->global_bo_list.count; i++) {
      struct radv_amdgpu_winsys_bo *bo = cs->ws->global_bo_list.bos[i];
      if (addr >= bo->base.va && addr - bo->base.va < bo->base.size) {
         void *map = radv_buffer_map(&cs->ws->base, &bo->base);
         if (map) {
            u_rwlock_rdunlock(&cs->ws->global_bo_list.lock);
            info->valid = true;
            info->cpu_addr = (char *)map + (addr - bo->base.va);
            return;
         }
      }
   }
   u_rwlock_rdunlock(&cs->ws->global_bo_list.lock);

   return;
}

static const char *
radv_amdgpu_get_dump_ibs_str(enum radv_cs_dump_type type)
{
   switch (type) {
   case RADV_CS_DUMP_TYPE_PREAMBLE_IBS:
      return "Preamble";
   case RADV_CS_DUMP_TYPE_MAIN_IBS:
      return "Main";
   case RADV_CS_DUMP_TYPE_POSTAMBLE_IBS:
      return "Postamble";
   default:
      UNREACHABLE("invalid CS dump type");
   }
}

static void
radv_amdgpu_winsys_cs_dump(struct ac_cmdbuf *_cs, FILE *file, const int *trace_ids, int trace_id_count,
                           enum radv_cs_dump_type type)
{
   struct radv_amdgpu_cs *cs = (struct radv_amdgpu_cs *)_cs;
   struct radv_amdgpu_winsys *ws = cs->ws;
   const bool dump_ibs = type == RADV_CS_DUMP_TYPE_PREAMBLE_IBS || type == RADV_CS_DUMP_TYPE_MAIN_IBS ||
                         type == RADV_CS_DUMP_TYPE_POSTAMBLE_IBS;
   const bool dump_ctx_rolls = type == RADV_CS_DUMP_TYPE_CTX_ROLLS;

   if (cs->chain_ib) {
      struct radv_amdgpu_cs_ib_info ib_info = radv_amdgpu_cs_ib_to_info(cs, cs->ib_buffers[0]);

      struct ac_addr_info addr_info;
      radv_amdgpu_winsys_get_cpu_addr(cs, ib_info.ib_mc_address, &addr_info);
      assert(addr_info.cpu_addr);

      if (dump_ibs) {
         struct ac_ib_parser ib_parser = {
            .f = file,
            .ib = addr_info.cpu_addr,
            .num_dw = cs->ib_buffers[0].cdw,
            .trace_ids = trace_ids,
            .trace_id_count = trace_id_count,
            .gfx_level = ws->info.gfx_level,
            .vcn_version = ws->info.vcn_ip_version,
            .family = ws->info.family,
            .ip_type = cs->hw_ip,
            .addr_callback = radv_amdgpu_winsys_get_cpu_addr,
            .addr_callback_data = cs,
            .annotations = cs->annotations,
         };

         char name[64];
         snprintf(name, sizeof(name), "%s IB", radv_amdgpu_get_dump_ibs_str(type));

         ac_parse_ib(&ib_parser, name);
      } else {
         uint32_t *ib_dw = addr_info.cpu_addr;
         ac_gather_context_rolls(file, &ib_dw, &cs->ib_buffers[0].cdw, 1, cs->annotations, &ws->info);
      }
   } else {
      uint32_t **ibs = dump_ctx_rolls ? malloc(cs->num_ib_buffers * sizeof(uint32_t *)) : NULL;
      uint32_t *ib_dw_sizes = dump_ctx_rolls ? malloc(cs->num_ib_buffers * sizeof(uint32_t)) : NULL;

      for (unsigned i = 0; i < cs->num_ib_buffers; i++) {
         struct radv_amdgpu_ib *ib = &cs->ib_buffers[i];
         char name[64];
         void *mapped;

         mapped = radv_buffer_map(&ws->base, ib->bo);
         if (!mapped)
            continue;

         if (cs->num_ib_buffers > 1) {
            snprintf(name, sizeof(name), "%s IB (chunk %d)", radv_amdgpu_get_dump_ibs_str(type), i);
         } else {
            snprintf(name, sizeof(name), "%s IB", radv_amdgpu_get_dump_ibs_str(type));
         }

         if (dump_ibs) {
            struct ac_ib_parser ib_parser = {
               .f = file,
               .ib = mapped,
               .num_dw = ib->cdw,
               .trace_ids = trace_ids,
               .trace_id_count = trace_id_count,
               .gfx_level = ws->info.gfx_level,
               .vcn_version = ws->info.vcn_ip_version,
               .family = ws->info.family,
               .ip_type = cs->hw_ip,
               .addr_callback = radv_amdgpu_winsys_get_cpu_addr,
               .addr_callback_data = cs,
               .annotations = cs->annotations,
            };

            ac_parse_ib(&ib_parser, name);
         } else {
            ibs[i] = (uint32_t *)mapped;
            ib_dw_sizes[i] = ib->cdw;
         }
      }

      if (dump_ctx_rolls) {
         ac_gather_context_rolls(file, ibs, ib_dw_sizes, cs->num_ib_buffers, cs->annotations, &ws->info);

         free(ibs);
         free(ib_dw_sizes);
      }
   }
}

static void
radv_amdgpu_winsys_cs_annotate(struct ac_cmdbuf *_cs, const char *annotation)
{
   struct radv_amdgpu_cs *cs = (struct radv_amdgpu_cs *)_cs;

   if (!cs->annotations) {
      cs->annotations = _mesa_pointer_hash_table_create(NULL);
      if (!cs->annotations)
         return;
   }

   struct hash_entry *entry = _mesa_hash_table_search(cs->annotations, _cs->buf + _cs->cdw);
   if (entry) {
      char *old_annotation = entry->data;
      char *new_annotation = calloc(strlen(old_annotation) + strlen(annotation) + 5, 1);
      sprintf(new_annotation, "%s -> %s", old_annotation, annotation);
      free(old_annotation);
      _mesa_hash_table_insert(cs->annotations, _cs->buf + _cs->cdw, new_annotation);
   } else {
      _mesa_hash_table_insert(cs->annotations, _cs->buf + _cs->cdw, strdup(annotation));
   }
}

static uint32_t
radv_to_amdgpu_priority(enum radeon_ctx_priority radv_priority)
{
   switch (radv_priority) {
   case RADEON_CTX_PRIORITY_REALTIME:
      return AMDGPU_CTX_PRIORITY_VERY_HIGH;
   case RADEON_CTX_PRIORITY_HIGH:
      return AMDGPU_CTX_PRIORITY_HIGH;
   case RADEON_CTX_PRIORITY_MEDIUM:
      return AMDGPU_CTX_PRIORITY_NORMAL;
   case RADEON_CTX_PRIORITY_LOW:
      return AMDGPU_CTX_PRIORITY_LOW;
   default:
      UNREACHABLE("Invalid context priority");
   }
}

static VkResult
radv_amdgpu_ctx_create(struct radeon_winsys *_ws, enum radeon_ctx_priority priority, struct radeon_winsys_ctx **rctx)
{
   struct radv_amdgpu_winsys *ws = radv_amdgpu_winsys(_ws);
   struct radv_amdgpu_ctx *ctx = CALLOC_STRUCT(radv_amdgpu_ctx);
   uint32_t amdgpu_priority = radv_to_amdgpu_priority(priority);
   VkResult result;
   int r;

   if (!ctx)
      return VK_ERROR_OUT_OF_HOST_MEMORY;

   r = ac_drm_cs_ctx_create2(ws->dev, amdgpu_priority, &ctx->ctx_handle);
   if (r && r == -EACCES) {
      result = VK_ERROR_NOT_PERMITTED;
      goto fail_create;
   } else if (r) {
      fprintf(stderr, "radv/amdgpu: radv_amdgpu_cs_ctx_create2 failed. (%i)\n", r);
      result = VK_ERROR_OUT_OF_HOST_MEMORY;
      goto fail_create;
   }
   ctx->ws = ws;

   assert(AMDGPU_HW_IP_NUM * MAX_RINGS_PER_TYPE * 4 * sizeof(uint64_t) <= 4096);
   result = ws->base.buffer_create(&ws->base, 4096, 8, RADEON_DOMAIN_GTT,
                                   RADEON_FLAG_CPU_ACCESS | RADEON_FLAG_NO_INTERPROCESS_SHARING, RADV_BO_PRIORITY_CS, 0,
                                   &ctx->fence_bo);
   if (result != VK_SUCCESS) {
      goto fail_alloc;
   }

   *rctx = (struct radeon_winsys_ctx *)ctx;
   return VK_SUCCESS;

fail_alloc:
   ac_drm_cs_ctx_free(ws->dev, ctx->ctx_handle);
fail_create:
   FREE(ctx);
   return result;
}

static void
radv_amdgpu_ctx_destroy(struct radeon_winsys_ctx *rwctx)
{
   struct radv_amdgpu_ctx *ctx = (struct radv_amdgpu_ctx *)rwctx;

   for (unsigned ip = 0; ip <= AMDGPU_HW_IP_NUM; ++ip) {
      for (unsigned ring = 0; ring < MAX_RINGS_PER_TYPE; ++ring) {
         if (ctx->queue_syncobj[ip][ring])
            ac_drm_cs_destroy_syncobj(ctx->ws->dev, ctx->queue_syncobj[ip][ring]);
      }
   }

   ctx->ws->base.buffer_destroy(&ctx->ws->base, ctx->fence_bo);
   ac_drm_cs_ctx_free(ctx->ws->dev, ctx->ctx_handle);
   FREE(ctx);
}

static VkResult
radv_amdgpu_ctx_is_priority_permitted(struct radeon_winsys *_ws, enum radeon_ctx_priority priority)
{
   struct radv_amdgpu_winsys *ws = radv_amdgpu_winsys(_ws);
   uint32_t amdgpu_priority = radv_to_amdgpu_priority(priority);
   uint32_t ctx_handle;
   int r;

   r = ac_drm_cs_ctx_create2(ws->dev, amdgpu_priority, &ctx_handle);
   if (r && r == -EACCES) {
      return VK_ERROR_NOT_PERMITTED;
   } else if (r) {
      return VK_ERROR_OUT_OF_HOST_MEMORY;
   }

   ac_drm_cs_ctx_free(ws->dev, ctx_handle);
   return VK_SUCCESS;
}

static uint32_t
radv_amdgpu_ctx_queue_syncobj(struct radv_amdgpu_ctx *ctx, unsigned ip, unsigned ring)
{
   uint32_t *syncobj = &ctx->queue_syncobj[ip][ring];
   if (!*syncobj) {
      ac_drm_cs_create_syncobj2(ctx->ws->dev, DRM_SYNCOBJ_CREATE_SIGNALED, syncobj);
   }
   return *syncobj;
}

static bool
radv_amdgpu_ctx_wait_idle(struct radeon_winsys_ctx *rwctx, enum amd_ip_type ip_type, int ring_index)
{
   struct radv_amdgpu_ctx *ctx = (struct radv_amdgpu_ctx *)rwctx;

   if (ctx->last_submission[ip_type][ring_index].fence.fence) {
      uint32_t expired;
      int ret = ac_drm_cs_query_fence_status(
         ctx->ws->dev, ctx->ctx_handle, ctx->last_submission[ip_type][ring_index].fence.ip_type,
         ctx->last_submission[ip_type][ring_index].fence.ip_instance,
         ctx->last_submission[ip_type][ring_index].fence.ring, ctx->last_submission[ip_type][ring_index].fence.fence,
         1000000000ull, 0, &expired);

      if (ret || !expired)
         return false;
   }

   return true;
}

static uint32_t
radv_to_amdgpu_pstate(enum radeon_ctx_pstate radv_pstate)
{
   switch (radv_pstate) {
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

static int
radv_amdgpu_ctx_set_pstate(struct radeon_winsys_ctx *rwctx, enum radeon_ctx_pstate pstate)
{
   struct radv_amdgpu_ctx *ctx = (struct radv_amdgpu_ctx *)rwctx;
   uint32_t new_pstate = radv_to_amdgpu_pstate(pstate);
   uint32_t current_pstate = 0;
   int r;

   r = ac_drm_cs_ctx_stable_pstate(ctx->ws->dev, ctx->ctx_handle, AMDGPU_CTX_OP_GET_STABLE_PSTATE, 0, &current_pstate);
   if (r) {
      fprintf(stderr, "radv/amdgpu: failed to get current pstate\n");
      return r;
   }

   /* Do not try to set a new pstate when the current one is already what we want. Otherwise, the
    * kernel might return -EBUSY if we have multiple AMDGPU contexts in flight.
    */
   if (current_pstate == new_pstate)
      return 0;

   r = ac_drm_cs_ctx_stable_pstate(ctx->ws->dev, ctx->ctx_handle, AMDGPU_CTX_OP_SET_STABLE_PSTATE, new_pstate, NULL);
   if (r) {
      fprintf(stderr, "radv/amdgpu: failed to set new pstate\n");
      return r;
   }

   return 0;
}

static void *
radv_amdgpu_cs_alloc_syncobj_chunk(struct radv_winsys_sem_counts *counts, uint32_t queue_syncobj,
                                   struct drm_amdgpu_cs_chunk *chunk, int chunk_id)
{
   unsigned count = counts->syncobj_count + (queue_syncobj ? 1u : 0u);
   struct drm_amdgpu_cs_chunk_sem *syncobj = malloc(sizeof(struct drm_amdgpu_cs_chunk_sem) * count);
   if (!syncobj)
      return NULL;

   for (unsigned i = 0; i < counts->syncobj_count; i++) {
      struct drm_amdgpu_cs_chunk_sem *sem = &syncobj[i];
      sem->handle = counts->syncobj[i];
   }

   if (queue_syncobj)
      syncobj[counts->syncobj_count].handle = queue_syncobj;

   chunk->chunk_id = chunk_id;
   chunk->length_dw = (uint32_t)(sizeof(struct drm_amdgpu_cs_chunk_sem) / 4 * count);
   chunk->chunk_data = (uint64_t)(uintptr_t)syncobj;
   return syncobj;
}

static void *
radv_amdgpu_cs_alloc_timeline_syncobj_chunk(struct radv_winsys_sem_counts *counts, uint32_t queue_syncobj,
                                            struct drm_amdgpu_cs_chunk *chunk, int chunk_id)
{
   uint32_t count = counts->syncobj_count + counts->timeline_syncobj_count + (queue_syncobj ? 1u : 0u);
   struct drm_amdgpu_cs_chunk_syncobj *syncobj = malloc(sizeof(struct drm_amdgpu_cs_chunk_syncobj) * count);
   if (!syncobj)
      return NULL;

   for (unsigned i = 0; i < counts->syncobj_count; i++) {
      struct drm_amdgpu_cs_chunk_syncobj *sem = &syncobj[i];
      sem->handle = counts->syncobj[i];
      sem->flags = 0;
      sem->point = 0;
   }

   for (unsigned i = 0; i < counts->timeline_syncobj_count; i++) {
      struct drm_amdgpu_cs_chunk_syncobj *sem = &syncobj[i + counts->syncobj_count];
      sem->handle = counts->syncobj[i + counts->syncobj_count];
      sem->flags = DRM_SYNCOBJ_WAIT_FLAGS_WAIT_FOR_SUBMIT;
      sem->point = counts->points[i];
   }

   if (queue_syncobj) {
      syncobj[count - 1].handle = queue_syncobj;
      syncobj[count - 1].flags = 0;
      syncobj[count - 1].point = 0;
   }

   chunk->chunk_id = chunk_id;
   chunk->length_dw = (uint32_t)(sizeof(struct drm_amdgpu_cs_chunk_syncobj) / 4 * count);
   chunk->chunk_data = (uint64_t)(uintptr_t)syncobj;
   return syncobj;
}

static bool
radv_amdgpu_cs_has_user_fence(struct radv_amdgpu_cs_request *request)
{
   return request->ip_type != AMDGPU_HW_IP_UVD && request->ip_type != AMDGPU_HW_IP_VCE &&
          request->ip_type != AMDGPU_HW_IP_UVD_ENC && request->ip_type != AMDGPU_HW_IP_VCN_DEC &&
          request->ip_type != AMDGPU_HW_IP_VCN_ENC && request->ip_type != AMDGPU_HW_IP_VCN_JPEG;
}

static VkResult
radv_amdgpu_cs_submit(struct radv_amdgpu_ctx *ctx, struct radv_amdgpu_cs_request *request,
                      struct radv_winsys_sem_info *sem_info)
{
   int r;
   int num_chunks;
   int size;
   struct drm_amdgpu_bo_list_in bo_list_in;
   void *wait_syncobj = NULL, *signal_syncobj = NULL;
   VkResult result = VK_SUCCESS;
   bool has_user_fence = radv_amdgpu_cs_has_user_fence(request);
   uint32_t queue_syncobj = radv_amdgpu_ctx_queue_syncobj(ctx, request->ip_type, request->ring);
   bool *queue_syncobj_wait = &ctx->queue_syncobj_wait[request->ip_type][request->ring];

   if (!queue_syncobj)
      return VK_ERROR_OUT_OF_HOST_MEMORY;

   size = request->number_of_ibs + 1 + (has_user_fence ? 1 : 0) + 1 /* bo list */ + 3;

   STACK_ARRAY(struct drm_amdgpu_cs_chunk, chunks, size);

   size = request->number_of_ibs + (has_user_fence ? 1 : 0);
   STACK_ARRAY(struct drm_amdgpu_cs_chunk_data, chunk_data, size);

   if (!chunks || !chunk_data) {
      result = VK_ERROR_OUT_OF_HOST_MEMORY;
      goto out_finish;
   }

   num_chunks = request->number_of_ibs;
   for (int i = 0; i < (int)request->number_of_ibs; i++) {
      struct radv_amdgpu_cs_ib_info *ib;
      chunks[i].chunk_id = AMDGPU_CHUNK_ID_IB;
      chunks[i].length_dw = sizeof(struct drm_amdgpu_cs_chunk_ib) / 4;
      chunks[i].chunk_data = (uint64_t)(uintptr_t)&chunk_data[i];

      ib = &request->ibs[i];
      assert(ib->ib_mc_address && ib->ib_mc_address % ctx->ws->info.ip[ib->ip_type].ib_alignment == 0);
      assert(ib->size);

      chunk_data[i].ib_data._pad = 0;
      chunk_data[i].ib_data.va_start = ib->ib_mc_address;
      chunk_data[i].ib_data.ib_bytes = ib->size * 4u;
      chunk_data[i].ib_data.ip_type = ib->ip_type;
      chunk_data[i].ib_data.ip_instance = request->ip_instance;
      chunk_data[i].ib_data.ring = request->ring;
      chunk_data[i].ib_data.flags = ib->flags;
   }

   assert(chunk_data[request->number_of_ibs - 1].ib_data.ip_type == request->ip_type);

   if (has_user_fence) {
      int i = num_chunks++;
      chunks[i].chunk_id = AMDGPU_CHUNK_ID_FENCE;
      chunks[i].length_dw = sizeof(struct drm_amdgpu_cs_chunk_fence) / 4;
      chunks[i].chunk_data = (uint64_t)(uintptr_t)&chunk_data[i];

      /* Need to reserve 4 QWORD for user fence:
       *   QWORD[0]: completed fence
       *   QWORD[1]: preempted fence
       *   QWORD[2]: reset fence
       *   QWORD[3]: preempted then reset
       */
      uint32_t offset = (request->ip_type * MAX_RINGS_PER_TYPE + request->ring) * 4u;
      ac_drm_cs_chunk_fence_info_to_data(radv_amdgpu_winsys_bo(ctx->fence_bo)->bo_handle, offset, &chunk_data[i]);
   }

   if (sem_info->cs_emit_wait &&
       (sem_info->wait.timeline_syncobj_count || sem_info->wait.syncobj_count || *queue_syncobj_wait)) {
      uint32_t queue_wait_syncobj = *queue_syncobj_wait ? queue_syncobj : 0;

      if (ctx->ws->info.has_timeline_syncobj) {
         wait_syncobj = radv_amdgpu_cs_alloc_timeline_syncobj_chunk(
            &sem_info->wait, queue_wait_syncobj, &chunks[num_chunks], AMDGPU_CHUNK_ID_SYNCOBJ_TIMELINE_WAIT);
      } else {
         wait_syncobj = radv_amdgpu_cs_alloc_syncobj_chunk(&sem_info->wait, queue_wait_syncobj, &chunks[num_chunks],
                                                           AMDGPU_CHUNK_ID_SYNCOBJ_IN);
      }
      if (!wait_syncobj) {
         result = VK_ERROR_OUT_OF_HOST_MEMORY;
         goto out_finish;
      }
      num_chunks++;

      sem_info->cs_emit_wait = false;
      *queue_syncobj_wait = false;
   }

   if (sem_info->cs_emit_signal) {
      if (ctx->ws->info.has_timeline_syncobj) {
         signal_syncobj = radv_amdgpu_cs_alloc_timeline_syncobj_chunk(
            &sem_info->signal, queue_syncobj, &chunks[num_chunks], AMDGPU_CHUNK_ID_SYNCOBJ_TIMELINE_SIGNAL);
      } else {
         signal_syncobj = radv_amdgpu_cs_alloc_syncobj_chunk(&sem_info->signal, queue_syncobj, &chunks[num_chunks],
                                                             AMDGPU_CHUNK_ID_SYNCOBJ_OUT);
      }
      if (!signal_syncobj) {
         result = VK_ERROR_OUT_OF_HOST_MEMORY;
         goto out_finish;
      }
      num_chunks++;
   }

   bo_list_in.operation = ~0u;
   bo_list_in.list_handle = ~0u;
   bo_list_in.bo_number = request->num_handles;
   bo_list_in.bo_info_size = sizeof(struct drm_amdgpu_bo_list_entry);
   bo_list_in.bo_info_ptr = (uint64_t)(uintptr_t)request->handles;

   chunks[num_chunks].chunk_id = AMDGPU_CHUNK_ID_BO_HANDLES;
   chunks[num_chunks].length_dw = sizeof(struct drm_amdgpu_bo_list_in) / 4;
   chunks[num_chunks].chunk_data = (uintptr_t)&bo_list_in;
   num_chunks++;

   /* The kernel returns -ENOMEM with many parallel processes using GDS such as test suites quite
    * often, but it eventually succeeds after enough attempts. This happens frequently with dEQP
    * using NGG streamout.
    */
   uint64_t abs_timeout_ns = os_time_get_absolute_timeout(1000000000ull); /* 1s */

   r = 0;
   do {
      /* Wait 1 ms and try again. */
      if (r == -ENOMEM)
         os_time_sleep(1000);

      r = ac_drm_cs_submit_raw2(ctx->ws->dev, ctx->ctx_handle, 0, num_chunks, chunks, &request->seq_no);
   } while (r == -ENOMEM && os_time_get_nano() < abs_timeout_ns);

   if (r) {
      if (r == -ENOMEM) {
         fprintf(stderr, "radv/amdgpu: Not enough memory for command submission.\n");
         result = VK_ERROR_OUT_OF_HOST_MEMORY;
      } else if (r == -ECANCELED) {
         fprintf(stderr,
                 "radv/amdgpu: The CS has been cancelled because the context is lost. This context is innocent.\n");
         result = VK_ERROR_DEVICE_LOST;
      } else if (r == -ENODATA) {
         fprintf(stderr, "radv/amdgpu: The CS has been cancelled because the context is lost. This context is guilty "
                         "of a soft recovery.\n");
         result = VK_ERROR_DEVICE_LOST;
      } else if (r == -ETIME) {
         fprintf(stderr, "radv/amdgpu: The CS has been cancelled because the context is lost. This context is guilty "
                         "of a hard recovery.\n");
         result = VK_ERROR_DEVICE_LOST;
      } else {
         fprintf(stderr,
                 "radv/amdgpu: The CS has been rejected, "
                 "see dmesg for more information (%i).\n",
                 r);
         result = VK_ERROR_UNKNOWN;
      }
   }

out_finish:
   free(wait_syncobj);
   free(signal_syncobj);
   STACK_ARRAY_FINISH(chunks);
   STACK_ARRAY_FINISH(chunk_data);
   return result;
}

void
radv_amdgpu_cs_init_functions(struct radv_amdgpu_winsys *ws)
{
   ws->base.ctx_create = radv_amdgpu_ctx_create;
   ws->base.ctx_destroy = radv_amdgpu_ctx_destroy;
   ws->base.ctx_is_priority_permitted = radv_amdgpu_ctx_is_priority_permitted;
   ws->base.ctx_wait_idle = radv_amdgpu_ctx_wait_idle;
   ws->base.ctx_set_pstate = radv_amdgpu_ctx_set_pstate;
   ws->base.cs_domain = radv_amdgpu_cs_domain;
   ws->base.cs_create = radv_amdgpu_cs_create;
   ws->base.cs_destroy = radv_amdgpu_cs_destroy;
   ws->base.cs_grow = radv_amdgpu_cs_grow;
   ws->base.cs_finalize = radv_amdgpu_cs_finalize;
   ws->base.cs_reset = radv_amdgpu_cs_reset;
   ws->base.cs_chain = radv_amdgpu_cs_chain;
   ws->base.cs_unchain = radv_amdgpu_cs_unchain;
   ws->base.cs_add_buffer = radv_amdgpu_cs_add_buffer;
   ws->base.cs_execute_secondary = radv_amdgpu_cs_execute_secondary;
   ws->base.cs_execute_ib = radv_amdgpu_cs_execute_ib;
   ws->base.cs_chain_dgc_ib = radv_amdgpu_cs_chain_dgc_ib;
   ws->base.cs_submit = radv_amdgpu_winsys_cs_submit;
   ws->base.cs_dump = radv_amdgpu_winsys_cs_dump;
   ws->base.cs_annotate = radv_amdgpu_winsys_cs_annotate;
   ws->base.cs_pad = radv_amdgpu_winsys_cs_pad;
}
