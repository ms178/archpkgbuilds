/*
 * Copyright © 2016 Red Hat.
 * Copyright © 2016 Bas Nieuwenhuizen
 *
 * based on si_state.c
 * Copyright © 2015 Advanced Micro Devices, Inc.
 *
 * SPDX-License-Identifier: MIT
 */

#include "radv_cp_dma.h"
#include "radv_buffer.h"
#include "radv_cs.h"
#include "radv_debug.h"
#include "radv_shader.h"
#include "radv_sqtt.h"
#include "sid.h"

#include <assert.h>
#include <stdint.h>

/* Set this if you want the 3D engine to wait until CP DMA is done.
 * It should be set on the last CP DMA packet. */
#define CP_DMA_SYNC (1 << 0)

/* Set this if the source data was used as a destination in a previous CP DMA
 * packet. It's for preventing a read-after-write (RAW) hazard between two
 * CP DMA packets. */
#define CP_DMA_RAW_WAIT (1 << 1)
#define CP_DMA_USE_L2   (1 << 2)
#define CP_DMA_CLEAR    (1 << 3)

/* Alignment for optimal performance. */
#define SI_CPDMA_ALIGNMENT 32

_Static_assert((SI_CPDMA_ALIGNMENT & (SI_CPDMA_ALIGNMENT - 1)) == 0,
               "SI_CPDMA_ALIGNMENT must be power of 2");

/* The max number of bytes that can be copied per packet. */
static inline unsigned
cp_dma_max_byte_count(enum amd_gfx_level gfx_level)
{
   unsigned max = gfx_level >= GFX11 ? 32767u : gfx_level >= GFX9 ? S_506_BYTE_COUNT(~0u) : S_415_BYTE_COUNT(~0u);

   /* Make it aligned for optimal performance. */
   return max & ~(SI_CPDMA_ALIGNMENT - 1u);
}

/* Emit a CP DMA packet to do a copy from one buffer to another, or to clear
 * a buffer. The size must fit in bits [20:0]. If CP_DMA_CLEAR is set, src_va is a 32-bit
 * clear value.
 */
static void
radv_cs_emit_cp_dma(struct radv_device *device, struct radv_cmd_stream *cs, bool predicating, uint64_t dst_va,
                    uint64_t src_va, unsigned size, unsigned flags)
{
   const struct radv_physical_device *pdev = radv_device_physical(device);
   const enum amd_gfx_level gfx_level = pdev->info.gfx_level;
   const bool cp_dma_use_l2 = __builtin_expect((flags & CP_DMA_USE_L2) && pdev->info.cp_dma_use_L2, 1);
   const bool cp_dma_use_mall = gfx_level == GFX12;
   /* GFX12: TC_L2 means MALL, which should always be set. */
   const bool cp_dma_tc_l2_flag = cp_dma_use_l2 || cp_dma_use_mall;
   uint32_t header = 0, command = 0;

   assert(size <= cp_dma_max_byte_count(gfx_level));

   radeon_check_space(device->ws, cs->b, 9);
   if (__builtin_expect(gfx_level >= GFX9, 1))
      command |= S_506_BYTE_COUNT(size);
   else
      command |= S_415_BYTE_COUNT(size);

   /* Sync flags. Only present for PFP/ME. MEC always sync. */
   if (__builtin_expect((flags & CP_DMA_SYNC) && cs->hw_ip == AMD_IP_GFX, 0))
      header |= S_501_CP_SYNC(1);

   if (__builtin_expect(flags & CP_DMA_RAW_WAIT, 0))
      command |= S_506_RAW_WAIT(1);

   /* Src and dst flags. */
   if (cp_dma_tc_l2_flag)
      header |= S_501_DST_SEL(V_501_DST_ADDR_USING_L2);

   if (__builtin_expect(flags & CP_DMA_CLEAR, 0))
      header |= S_501_SRC_SEL(V_501_DATA);
   else if (cp_dma_tc_l2_flag)
      header |= S_501_SRC_SEL(V_501_SRC_ADDR_USING_L2);

   radeon_begin(cs);
   if (__builtin_expect(gfx_level >= GFX7, 1)) {
      radeon_emit(PKT3(PKT3_DMA_DATA, 5, predicating));
      radeon_emit(header);
      radeon_emit((uint32_t)src_va);         /* SRC_ADDR_LO [31:0] */
      radeon_emit((uint32_t)(src_va >> 32)); /* SRC_ADDR_HI [31:0] */
      radeon_emit((uint32_t)dst_va);         /* DST_ADDR_LO [31:0] */
      radeon_emit((uint32_t)(dst_va >> 32)); /* DST_ADDR_HI [31:0] */
      radeon_emit(command);
   } else {
      assert(!cp_dma_tc_l2_flag);
      header |= S_412_SRC_ADDR_HI((uint32_t)(src_va >> 32));
      radeon_emit(PKT3(PKT3_CP_DMA, 4, predicating));
      radeon_emit((uint32_t)src_va);                  /* SRC_ADDR_LO [31:0] */
      radeon_emit(header);                            /* SRC_ADDR_HI [15:0] + flags. */
      radeon_emit((uint32_t)dst_va);                  /* DST_ADDR_LO [31:0] */
      radeon_emit((uint32_t)((dst_va >> 32) & 0xffffu)); /* DST_ADDR_HI [15:0] */
      radeon_emit(command);
   }
   radeon_end();
}

static void
radv_emit_cp_dma(struct radv_cmd_buffer *cmd_buffer, uint64_t dst_va, uint64_t src_va, unsigned size, unsigned flags)
{
   struct radv_device *device = radv_cmd_buffer_device(cmd_buffer);
   const struct radv_cond_render_state *cond_render = &cmd_buffer->state.cond_render;
   struct radv_cmd_stream *cs = cmd_buffer->cs;

   radv_cs_emit_cp_dma(device, cs, cond_render->enabled, dst_va, src_va, size, flags);

   /* CP DMA is executed in ME, but index buffers are read by PFP.
    * This ensures that ME (CP DMA) is idle before PFP starts fetching
    * indices. If we wanted to execute CP DMA in PFP, this packet
    * should precede it.
    */
   if (flags & CP_DMA_SYNC) {
      if (cmd_buffer->qf == RADV_QUEUE_GENERAL)
         ac_emit_cp_pfp_sync_me(cs->b, cond_render->enabled);

      /* CP will see the sync flag and wait for all DMAs to complete. */
      cmd_buffer->state.dma_is_busy = false;
   }

   if (radv_device_fault_detection_enabled(device))
      radv_cmd_buffer_trace_emit(cmd_buffer);
}

/* Emit a CP DMA prefetch.
 * This is useful for warming up caches before draw commands,
 * for example we use it to load shader binaries and VBO descriptors into the cache.
 * Implemented by starting a CP DMA copy where the source and destination are the same.
 *
 * On GPUs where CP DMA uses L2, it loads binaries into L2.
 * On GPUs that have MALL (infinity cache), it loads binaries into MALL.
 *
 * More specifically:
 *
 * | GPU generation  | CP DMA L2 | MALL | Prefetch location |
 * | --------------- | --------- | ---- | ----------------- |
 * | GFX6            | -         | -    | -                 |
 * | GFX7 - 10       | yes       | -    | L2                |
 * | GFX10.3 - 11.5  | yes       | yes  | L2, MALL          |
 * | GFX12           | -         | yes  | MALL              |
 *
 */
void
radv_cs_cp_dma_prefetch(const struct radv_device *device, struct radv_cmd_stream *cs, uint64_t va, unsigned size,
                        bool predicating)
{
   const struct radv_physical_device *pdev = radv_device_physical(device);
   const enum amd_gfx_level gfx_level = pdev->info.gfx_level;

   if (gfx_level < GFX7)
      return;

   if (!pdev->info.cp_dma_use_L2 && gfx_level < GFX12)
      return;

   if (__builtin_expect(gfx_level >= GFX11, 0)) {
      const unsigned gfx11_max = 32768u - SI_CPDMA_ALIGNMENT;
      size = size < gfx11_max ? size : gfx11_max;
   }

   assert(size <= cp_dma_max_byte_count(gfx_level));

   radeon_check_space(device->ws, cs->b, 9);

   const uint64_t aligned_va = va & ~(uint64_t)(SI_CPDMA_ALIGNMENT - 1);
   const uint64_t end_va = va + size;
   const uint64_t aligned_end = (end_va + SI_CPDMA_ALIGNMENT - 1u) & ~(uint64_t)(SI_CPDMA_ALIGNMENT - 1);
   uint64_t aligned_size = aligned_end - aligned_va;

   const unsigned hw_max = cp_dma_max_byte_count(gfx_level);
   if (__builtin_expect(aligned_size > hw_max, 0))
      aligned_size = hw_max;

   uint32_t header, command;

   if (__builtin_expect(gfx_level >= GFX9, 1)) {
      command = S_506_BYTE_COUNT((unsigned)aligned_size) | S_506_DISABLE_WR_CONFIRM(1);
      header = S_501_DST_SEL(V_501_DST_NOWHERE) | S_501_SRC_SEL(V_501_SRC_ADDR_USING_L2);
   } else {
      command = S_415_BYTE_COUNT((unsigned)aligned_size) | S_415_DISABLE_WR_CONFIRM(1);
      header = S_501_DST_SEL(V_501_DST_ADDR_USING_L2) | S_501_SRC_SEL(V_501_SRC_ADDR_USING_L2);
   }

   radeon_begin(cs);
   radeon_emit(PKT3(PKT3_DMA_DATA, 5, predicating));
   radeon_emit(header);
   radeon_emit((uint32_t)aligned_va);         /* SRC_ADDR_LO [31:0] */
   radeon_emit((uint32_t)(aligned_va >> 32)); /* SRC_ADDR_HI [31:0] */
   radeon_emit((uint32_t)aligned_va);         /* DST_ADDR_LO [31:0] */
   radeon_emit((uint32_t)(aligned_va >> 32)); /* DST_ADDR_HI [31:0] */
   radeon_emit(command);
   radeon_end();
}

void
radv_cp_dma_prefetch(struct radv_cmd_buffer *cmd_buffer, uint64_t va, unsigned size)
{
   struct radv_device *device = radv_cmd_buffer_device(cmd_buffer);
   const struct radv_cond_render_state *cond_render = &cmd_buffer->state.cond_render;

   radv_cs_cp_dma_prefetch(device, cmd_buffer->cs, va, size, cond_render->enabled);

   if (radv_device_fault_detection_enabled(device))
      radv_cmd_buffer_trace_emit(cmd_buffer);
}

static void
radv_cp_dma_prepare(struct radv_cmd_buffer *cmd_buffer, unsigned byte_count, uint64_t remaining_size, unsigned *flags)
{
   /* Flush the caches for the first copy only.
    * Also wait for the previous CP DMA operations.
    */
   if (cmd_buffer->state.flush_bits) {
      radv_emit_cache_flush(cmd_buffer);
      *flags |= CP_DMA_RAW_WAIT;
   }

   /* Do the synchronization after the last dma, so that all data
    * is written to memory.
    */
   if (byte_count == remaining_size)
      *flags |= CP_DMA_SYNC;
}

static void
radv_cp_dma_realign_engine(struct radv_cmd_buffer *cmd_buffer, unsigned size)
{
   uint64_t va;
   uint32_t offset;
   unsigned dma_flags = 0;
   const unsigned buf_size = SI_CPDMA_ALIGNMENT * 2;
   void *ptr;

   assert(size < SI_CPDMA_ALIGNMENT);

   radv_cmd_buffer_upload_alloc(cmd_buffer, buf_size, &offset, &ptr);

   va = radv_buffer_get_va(cmd_buffer->upload.upload_bo);
   va += offset;

   radv_cp_dma_prepare(cmd_buffer, size, size, &dma_flags);

   radv_emit_cp_dma(cmd_buffer, va, va + SI_CPDMA_ALIGNMENT, size, dma_flags);
}

void
radv_cp_dma_copy_memory(struct radv_cmd_buffer *cmd_buffer, uint64_t src_va, uint64_t dest_va, uint64_t size)
{
   struct radv_device *device = radv_cmd_buffer_device(cmd_buffer);
   const struct radv_physical_device *pdev = radv_device_physical(device);
   const enum amd_gfx_level gfx_level = pdev->info.gfx_level;
   const unsigned max_byte_count = cp_dma_max_byte_count(gfx_level);

   if (__builtin_expect(!(pdev->info.cp_dma_use_L2 && gfx_level >= GFX9), 0)) {
      /* Invalidate L2 in case "src_va" or "dest_va" were previously written through L2. */
      cmd_buffer->state.flush_bits |= RADV_CMD_FLAG_INV_L2;
   }

   /* Assume that we are not going to sync after the last DMA operation. */
   cmd_buffer->state.dma_is_busy = true;

   /* Fast path for GFX9+ (Vega, Navi, RDNA, etc.): No alignment workarounds needed. */
   if (__builtin_expect(gfx_level >= GFX9, 1)) {
      while (size) {
         const unsigned byte_count = size < max_byte_count ? (unsigned)size : max_byte_count;
         unsigned dma_flags = CP_DMA_USE_L2;

         radv_cp_dma_prepare(cmd_buffer, byte_count, size, &dma_flags);
         radv_emit_cp_dma(cmd_buffer, dest_va, src_va, byte_count, dma_flags);

         size -= byte_count;
         src_va += byte_count;
         dest_va += byte_count;
      }
      return;
   }

   /* Legacy path for GFX6-GFX8 (pre-Vega): Alignment workarounds required for Carrizo/Stoney. */
   uint64_t main_src_va, main_dest_va;
   uint64_t skipped_size = 0, realign_size = 0;

   if (pdev->info.family <= CHIP_CARRIZO || pdev->info.family == CHIP_STONEY) {
      if (size % SI_CPDMA_ALIGNMENT)
         realign_size = SI_CPDMA_ALIGNMENT - (size % SI_CPDMA_ALIGNMENT);

      if (src_va % SI_CPDMA_ALIGNMENT) {
         skipped_size = SI_CPDMA_ALIGNMENT - (src_va % SI_CPDMA_ALIGNMENT);
         skipped_size = skipped_size < size ? skipped_size : size;
         size -= skipped_size;
      }
   }

   main_src_va = src_va + skipped_size;
   main_dest_va = dest_va + skipped_size;

   while (size) {
      const unsigned byte_count = size < max_byte_count ? (unsigned)size : max_byte_count;
      unsigned dma_flags = 0;

      radv_cp_dma_prepare(cmd_buffer, byte_count, size + skipped_size + realign_size, &dma_flags);
      dma_flags &= ~CP_DMA_SYNC;
      radv_emit_cp_dma(cmd_buffer, main_dest_va, main_src_va, byte_count, dma_flags);

      size -= byte_count;
      main_src_va += byte_count;
      main_dest_va += byte_count;
   }

   if (skipped_size) {
      unsigned dma_flags = 0;

      radv_cp_dma_prepare(cmd_buffer, (unsigned)skipped_size, skipped_size + realign_size, &dma_flags);
      radv_emit_cp_dma(cmd_buffer, dest_va, src_va, (unsigned)skipped_size, dma_flags);
   }

   if (realign_size)
      radv_cp_dma_realign_engine(cmd_buffer, (unsigned)realign_size);
}

void
radv_cp_dma_fill_memory(struct radv_cmd_buffer *cmd_buffer, uint64_t va, uint64_t size, unsigned value)
{
   /* Early return for zero-size fills. */
   if (__builtin_expect(!size, 0))
      return;

   struct radv_device *device = radv_cmd_buffer_device(cmd_buffer);
   const struct radv_physical_device *pdev = radv_device_physical(device);
   const enum amd_gfx_level gfx_level = pdev->info.gfx_level;
   const unsigned max_byte_count = cp_dma_max_byte_count(gfx_level);
   const bool use_l2 = __builtin_expect(gfx_level >= GFX9, 1);

   if (__builtin_expect(!use_l2, 0)) {
      /* Invalidate L2 in case "va" was previously written through L2. */
      cmd_buffer->state.flush_bits |= RADV_CMD_FLAG_INV_L2;
   }

   assert((va % 4) == 0 && (size % 4) == 0);

   /* Assume that we are not going to sync after the last DMA operation. */
   cmd_buffer->state.dma_is_busy = true;

   while (size) {
      const unsigned byte_count = size < max_byte_count ? (unsigned)size : max_byte_count;
      unsigned dma_flags = CP_DMA_CLEAR | (use_l2 ? CP_DMA_USE_L2 : 0u);

      radv_cp_dma_prepare(cmd_buffer, byte_count, size, &dma_flags);

      /* Emit the clear packet. */
      radv_emit_cp_dma(cmd_buffer, va, value, byte_count, dma_flags);

      size -= byte_count;
      va += byte_count;
   }
}

void
radv_cp_dma_wait_for_idle(struct radv_cmd_buffer *cmd_buffer)
{
   struct radv_device *device = radv_cmd_buffer_device(cmd_buffer);
   const struct radv_physical_device *pdev = radv_device_physical(device);

   if (pdev->info.gfx_level < GFX7)
      return;

   if (!cmd_buffer->state.dma_is_busy)
      return;

   /* Issue a dummy DMA that copies zero bytes.
    *
    * The DMA engine will see that there's no work to do and skip this
    * DMA request, however, the CP will see the sync flag and still wait
    * for all DMAs to complete.
    */
   radv_emit_cp_dma(cmd_buffer, 0, 0, 0, CP_DMA_SYNC);

   cmd_buffer->state.dma_is_busy = false;
}
