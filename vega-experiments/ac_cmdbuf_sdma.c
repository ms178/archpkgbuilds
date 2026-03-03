/*
 * Copyright 2012 Advanced Micro Devices, Inc.
 * Copyright 2024-2026 Valve Corporation
 *
 * SPDX-License-Identifier: MIT
 *
 */

#include "ac_cmdbuf.h"
#include "ac_cmdbuf_sdma.h"
#include "ac_formats.h"
#include "ac_surface.h"
#include "sid.h"

#include "util/u_math.h"

/*
 * Vega 64 branch prediction hints (SDMA 4.0 is the dominant gaming workload).
 * Reduces misprediction penalties on 14700KF.
 */
#if defined(__GNUC__) || defined(__clang__)
   #define SDMA_LIKELY(x)        __builtin_expect(!!(x), 1)
   #define SDMA_UNLIKELY(x)      __builtin_expect(!!(x), 0)
   #define SDMA_FORCE_INLINE     static inline __attribute__((always_inline))
#else
   #define SDMA_LIKELY(x)        (x)
   #define SDMA_UNLIKELY(x)      (x)
   #define SDMA_FORCE_INLINE     static inline
#endif

_Static_assert(sizeof(uint32_t) == 4, "uint32_t must be exactly 4 bytes");
_Static_assert(sizeof(uint64_t) == 8, "uint64_t must be exactly 8 bytes");

/* ==================== BASIC PACKETS ==================== */

void
ac_emit_sdma_nop(struct ac_cmdbuf *cs)
{
   ac_cmdbuf_begin(cs);
   ac_cmdbuf_emit(SDMA_PACKET(SDMA_OPCODE_NOP, 0, 0));
   ac_cmdbuf_end();
}

void
ac_emit_sdma_write_timestamp(struct ac_cmdbuf *cs, uint64_t va)
{
   ac_cmdbuf_begin(cs);
   ac_cmdbuf_emit(SDMA_PACKET(SDMA_OPCODE_TIMESTAMP, SDMA_TS_SUB_OPCODE_GET_GLOBAL_TIMESTAMP, 0));
   ac_cmdbuf_emit((uint32_t)va);
   ac_cmdbuf_emit((uint32_t)(va >> 32));
   ac_cmdbuf_end();
}

void
ac_emit_sdma_fence(struct ac_cmdbuf *cs, uint64_t va, uint32_t fence)
{
   ac_cmdbuf_begin(cs);
   ac_cmdbuf_emit(SDMA_PACKET(SDMA_OPCODE_FENCE, 0, SDMA_FENCE_MTYPE_UC));
   ac_cmdbuf_emit((uint32_t)va);
   ac_cmdbuf_emit((uint32_t)(va >> 32));
   ac_cmdbuf_emit(fence);
   ac_cmdbuf_end();
}

void
ac_emit_sdma_wait_mem(struct ac_cmdbuf *cs, uint32_t op, uint64_t va,
                      uint32_t ref, uint32_t mask)
{
   assert(op <= 0xFu);

   ac_cmdbuf_begin(cs);
   ac_cmdbuf_emit(SDMA_PACKET(SDMA_OPCODE_POLL_REGMEM, 0, 0) |
                  (op << 28) | SDMA_POLL_MEM);
   ac_cmdbuf_emit((uint32_t)va);
   ac_cmdbuf_emit((uint32_t)(va >> 32));
   ac_cmdbuf_emit(ref);
   ac_cmdbuf_emit(mask);
   ac_cmdbuf_emit(SDMA_POLL_INTERVAL_160_CLK | (SDMA_POLL_RETRY_INDEFINITELY << 16));
   ac_cmdbuf_end();
}

void
ac_emit_sdma_write_data_head(struct ac_cmdbuf *cs, uint64_t va, uint32_t count)
{
   assert(count >= 1);

   ac_cmdbuf_begin(cs);
   ac_cmdbuf_emit(SDMA_PACKET(SDMA_OPCODE_WRITE, SDMA_WRITE_SUB_OPCODE_LINEAR, 0));
   ac_cmdbuf_emit((uint32_t)va);
   ac_cmdbuf_emit((uint32_t)(va >> 32));
   ac_cmdbuf_emit(count - 1);
   ac_cmdbuf_end();
}

/* ==================== CONSTANT FILL & LINEAR COPY (Vega 64 optimized) ==================== */

uint64_t
ac_emit_sdma_constant_fill(struct ac_cmdbuf *cs, enum sdma_version sdma_ip_version,
                           uint64_t va, uint64_t size, uint32_t value)
{
   const uint32_t fill_size_encoding = 2;

   assert(sdma_ip_version >= SDMA_2_4);
   assert(size > 0);

   if (SDMA_UNLIKELY(size < 4))
      return 0;

   const unsigned size_field_bits = SDMA_UNLIKELY(sdma_ip_version >= SDMA_6_0) ? 30 : 22;
   const uint64_t max_fill_size = (UINT64_C(1) << size_field_bits) - UINT64_C(4);
   const uint64_t aligned_size = size & ~UINT64_C(3);
   const uint64_t bytes_written = MIN2(aligned_size, max_fill_size);

   assert(bytes_written >= 4);

   ac_cmdbuf_begin(cs);
   ac_cmdbuf_emit(SDMA_PACKET(SDMA_OPCODE_CONSTANT_FILL, 0, 0) | (fill_size_encoding << 30));
   ac_cmdbuf_emit((uint32_t)va);
   ac_cmdbuf_emit((uint32_t)(va >> 32));
   ac_cmdbuf_emit(value);
   ac_cmdbuf_emit((uint32_t)(bytes_written - 1));
   ac_cmdbuf_end();

   return bytes_written;
}

uint64_t
ac_emit_sdma_copy_linear(struct ac_cmdbuf *cs, enum sdma_version sdma_ip_version,
                         uint64_t src_va, uint64_t dst_va, uint64_t size,
                         bool tmz)
{
   assert(sdma_ip_version >= SDMA_2_0);
   assert(size >= 1);

   const uint64_t max_size = SDMA_UNLIKELY(sdma_ip_version >= SDMA_5_2)
      ? SDMA_V5_2_COPY_MAX_BYTES
      : SDMA_V2_0_COPY_MAX_BYTES;

   const uint64_t alignment_mask = UINT64_C(3);
   const bool addrs_aligned = ((src_va | dst_va) & alignment_mask) == 0;
   const bool size_unaligned = (size & alignment_mask) != 0;
   const bool should_round = addrs_aligned && size_unaligned && size > 4;

   const uint64_t copy_size = should_round ? (size & ~alignment_mask) : size;
   const uint64_t bytes_written = MIN2(copy_size, max_size);

   const uint32_t size_field = SDMA_LIKELY(sdma_ip_version >= SDMA_4_0)
      ? (uint32_t)(bytes_written - 1)
      : (uint32_t)bytes_written;

   ac_cmdbuf_begin(cs);
   ac_cmdbuf_emit(SDMA_PACKET(SDMA_OPCODE_COPY, SDMA_COPY_SUB_OPCODE_LINEAR, (tmz ? 4 : 0)));
   ac_cmdbuf_emit(size_field);
   ac_cmdbuf_emit(0);
   ac_cmdbuf_emit((uint32_t)src_va);
   ac_cmdbuf_emit((uint32_t)(src_va >> 32));
   ac_cmdbuf_emit((uint32_t)dst_va);
   ac_cmdbuf_emit((uint32_t)(dst_va >> 32));
   ac_cmdbuf_end();

   return bytes_written;
}

/* ==================== PITCH VALIDATION ==================== */

SDMA_FORCE_INLINE void
ac_sdma_check_pitches(enum sdma_version sdma_ip_version, uint32_t pitch,
                      uint32_t slice_pitch, uint32_t bpp, bool uses_depth)
{
   assert(bpp >= 1 && bpp <= 16);
   assert(util_is_power_of_two_nonzero(bpp));

   ASSERTED const uint32_t pitch_alignment = MAX2(1u, 4u / bpp);
   assert(pitch >= 1);
   assert(pitch <= (1u << (sdma_ip_version >= SDMA_7_0 ? 16 : 14)));
   assert((pitch % pitch_alignment) == 0);

   if (uses_depth) {
      assert(slice_pitch >= 1);
      assert(slice_pitch <= (1u << 28));
      assert((slice_pitch % 4) == 0);
   }
}

/* ==================== LINEAR SUB-WINDOW (matches header exactly) ==================== */

void
ac_emit_sdma_copy_linear_sub_window(struct ac_cmdbuf *cs, enum sdma_version sdma_ip_version,
                                    const struct ac_sdma_surf *src,
                                    const struct ac_sdma_surf *dst,
                                    uint32_t width, uint32_t height, uint32_t depth)
{
   assert(width >= 1);
   assert(height >= 1);
   assert(depth >= 1);
   assert(src->bpp == dst->bpp);
   assert(util_is_power_of_two_nonzero(src->bpp));

   const bool use_pitch_shift_16 = (sdma_ip_version >= SDMA_7_0) ||
                                   (sdma_ip_version < SDMA_4_0);
   const uint32_t pitch_shift = use_pitch_shift_16 ? 16u : 13u;
   const uint32_t max_offset_z = (1u << pitch_shift) - 1u;

   assert(src->offset.z <= max_offset_z);
   assert(dst->offset.z <= max_offset_z);
   assert(src->offset.x <= UINT16_MAX);
   assert(src->offset.y <= UINT16_MAX);
   assert(dst->offset.x <= UINT16_MAX);
   assert(dst->offset.y <= UINT16_MAX);

   ac_sdma_check_pitches(sdma_ip_version, src->pitch, src->slice_pitch, src->bpp, false);
   ac_sdma_check_pitches(sdma_ip_version, dst->pitch, dst->slice_pitch, dst->bpp, false);

   const uint32_t log2_bpp = util_logbase2(src->bpp);
   const uint32_t src_pitch_encoded = (src->pitch - 1) << pitch_shift;
   const uint32_t dst_pitch_encoded = (dst->pitch - 1) << pitch_shift;

   ac_cmdbuf_begin(cs);
   ac_cmdbuf_emit(SDMA_PACKET(SDMA_OPCODE_COPY, SDMA_COPY_SUB_OPCODE_LINEAR_SUB_WINDOW, 0) |
                  (log2_bpp << 29));
   ac_cmdbuf_emit((uint32_t)src->va);
   ac_cmdbuf_emit((uint32_t)(src->va >> 32));
   ac_cmdbuf_emit(src->offset.x | (src->offset.y << 16));
   ac_cmdbuf_emit(src->offset.z | src_pitch_encoded);
   ac_cmdbuf_emit(src->slice_pitch - 1);
   ac_cmdbuf_emit((uint32_t)dst->va);
   ac_cmdbuf_emit((uint32_t)(dst->va >> 32));
   ac_cmdbuf_emit(dst->offset.x | (dst->offset.y << 16));
   ac_cmdbuf_emit(dst->offset.z | dst_pitch_encoded);
   ac_cmdbuf_emit(dst->slice_pitch - 1);

   if (SDMA_UNLIKELY(sdma_ip_version == SDMA_2_0)) {
      ac_cmdbuf_emit(width | (height << 16));
      ac_cmdbuf_emit(depth);
   } else {
      ac_cmdbuf_emit((width - 1) | ((height - 1) << 16));
      ac_cmdbuf_emit(depth - 1);
   }

   ac_cmdbuf_end();
}

/* ==================== TILED HELPERS (unified struct) ==================== */

SDMA_FORCE_INLINE uint32_t
ac_sdma_get_tiled_header_dword(enum sdma_version sdma_ip_version,
                               const struct ac_sdma_surf *tiled)
{
   if (SDMA_LIKELY(sdma_ip_version >= SDMA_4_0) &&
       SDMA_LIKELY(sdma_ip_version < SDMA_5_0)) {
      const uint32_t mip_max = MAX2(tiled->num_levels, 1u);
      const uint32_t mip_id = tiled->first_level;

      assert(mip_max >= 1 && mip_max <= 16);
      assert(mip_id <= 15);

      return ((mip_max - 1) << 20) | (mip_id << 24);
   }
   return 0;
}

SDMA_FORCE_INLINE enum gfx9_resource_type
ac_sdma_get_tiled_resource_dim(enum sdma_version sdma_ip_version,
                               const struct ac_sdma_surf *tiled)
{
   if (SDMA_UNLIKELY(sdma_ip_version >= SDMA_5_0)) {
      const enum gfx9_resource_type res_type = tiled->surf->u.gfx9.resource_type;
      const unsigned micro_mode = tiled->surf->micro_tile_mode;

      if ((res_type == RADEON_RESOURCE_1D || res_type == RADEON_RESOURCE_3D) &&
          (micro_mode == RADEON_MICRO_MODE_RENDER ||
           micro_mode == RADEON_MICRO_MODE_DEPTH))
         return RADEON_RESOURCE_2D;
   }
   return tiled->surf->u.gfx9.resource_type;
}

static uint32_t
ac_sdma_get_tiled_info_dword(const struct radeon_info *info,
                             const struct ac_sdma_surf *tiled)
{
   const uint32_t element_size = util_logbase2(tiled->bpp);
   const bool has_stencil = tiled->is_stencil;
   const uint32_t swizzle_mode = has_stencil
      ? tiled->surf->u.gfx9.zs.stencil_swizzle_mode
      : tiled->surf->u.gfx9.swizzle_mode;

   assert(element_size <= 4);

   /* Vega 64 (SDMA 4.0) hot path */
   if (SDMA_LIKELY(info->sdma_ip_version >= SDMA_4_0) &&
       SDMA_LIKELY(info->sdma_ip_version < SDMA_5_0)) {
      const enum gfx9_resource_type dimension = tiled->surf->u.gfx9.resource_type;
      const uint32_t epitch = has_stencil
         ? tiled->surf->u.gfx9.zs.stencil_epitch
         : tiled->surf->u.gfx9.epitch;

      assert(epitch <= UINT16_MAX);

      return element_size |
             (swizzle_mode << 3) |
             ((uint32_t)dimension << 9) |
             (epitch << 16);
   }

   if (SDMA_UNLIKELY(info->sdma_ip_version >= SDMA_7_0)) {
      const uint32_t mip_max = MAX2(tiled->num_levels, 1u);
      const uint32_t mip_id = tiled->first_level;
      return element_size | (swizzle_mode << 3) | ((mip_max - 1) << 16) | (mip_id << 24);
   }

   if (SDMA_UNLIKELY(info->sdma_ip_version >= SDMA_5_0)) {
      const enum gfx9_resource_type dimension =
         ac_sdma_get_tiled_resource_dim(info->sdma_ip_version, tiled);
      const uint32_t mip_max = MAX2(tiled->num_levels, 1u);
      const uint32_t mip_id = tiled->first_level;

      return element_size |
             (swizzle_mode << 3) |
             ((uint32_t)dimension << 9) |
             ((mip_max - 1) << 16) |
             (mip_id << 20);
   }

   /* Legacy path (pre-GFX9) */
   {
      const uint32_t tile_index = tiled->surf->u.legacy.tiling_index[0];
      const uint32_t macro_tile_index = tiled->surf->u.legacy.macro_tile_index;
      const uint32_t tile_mode = info->si_tile_mode_array[tile_index];
      const uint32_t macro_tile_mode = info->cik_macrotile_mode_array[macro_tile_index];
      const uint32_t tile_split = tiled->surf->u.legacy.tile_split;
      const uint32_t tile_split_enc = (tile_split > 0) ? util_logbase2(tile_split >> 6) : 0;

      return element_size |
             (G_009910_ARRAY_MODE(tile_mode) << 3) |
             (G_009910_MICRO_TILE_MODE_NEW(tile_mode) << 8) |
             (tile_split_enc << 11) |
             (G_009990_BANK_WIDTH(macro_tile_mode) << 15) |
             (G_009990_BANK_HEIGHT(macro_tile_mode) << 18) |
             (G_009990_NUM_BANKS(macro_tile_mode) << 21) |
             (G_009990_MACRO_TILE_ASPECT(macro_tile_mode) << 24) |
             (G_009910_PIPE_CONFIG(tile_mode) << 26);
   }
}

static uint32_t
ac_sdma7_get_metadata_config(const struct radeon_info *info,
                             const struct ac_sdma_surf *src,
                             const struct ac_sdma_surf *dst)
{
   uint32_t meta_config = 0;

   if (src->is_compressed) {
      meta_config |= SDMA7_DCC_READ_CM(2);
   }

   if (dst->is_compressed) {
      const uint32_t data_format = ac_get_cb_format(info->gfx_level, dst->format);
      const uint32_t number_type = ac_get_cb_number_type(dst->format);
      const uint32_t dcc_max = dst->surf->u.gfx9.color.dcc.max_compressed_block_size;

      meta_config |= SDMA7_DCC_DATA_FORMAT(data_format) |
                     SDMA7_DCC_NUM_TYPE(number_type) |
                     SDMA7_DCC_MAX_COM(dcc_max) |
                     SDMA7_DCC_MAX_UCOM(1) |
                     SDMA7_DCC_WRITE_CM(1);
   }

   return meta_config;
}

static uint32_t
ac_sdma5_get_metadata_config(const struct radeon_info *info,
                             const struct ac_sdma_surf *tiled,
                             bool detile, bool tmz)
{
   const uint32_t data_format = ac_get_cb_format(info->gfx_level, tiled->format);
   const uint32_t number_type = ac_get_cb_number_type(tiled->format);
   const bool alpha_msb = ac_alpha_is_on_msb(info, tiled->format);
   const uint32_t dcc_max = tiled->surf->u.gfx9.color.dcc.max_compressed_block_size;
   const bool pipe_aligned = tiled->htile_enabled ||
                             tiled->surf->u.gfx9.color.dcc.pipe_aligned;
   const bool write_compress = !detile && !tiled->htile_enabled;

   return SDMA5_DCC_DATA_FORMAT(data_format) |
          SDMA5_DCC_ALPHA_IS_ON_MSB(alpha_msb) |
          SDMA5_DCC_NUM_TYPE(number_type) |
          SDMA5_DCC_SURF_TYPE(tiled->surf_type) |
          SDMA5_DCC_MAX_COM(dcc_max) |
          SDMA5_DCC_PIPE_ALIGNED(pipe_aligned) |
          SDMA5_DCC_MAX_UCOM(V_028C78_MAX_BLOCK_SIZE_256B) |
          SDMA5_DCC_WRITE_COMPRESS(write_compress) |
          SDMA5_DCC_TMZ(tmz);
}

/* ==================== TILED SUB-WINDOW ==================== */

void
ac_emit_sdma_copy_tiled_sub_window(struct ac_cmdbuf *cs, const struct radeon_info *info,
                                   const struct ac_sdma_surf *linear,
                                   const struct ac_sdma_surf *tiled,
                                   bool detile, uint32_t width, uint32_t height,
                                   uint32_t depth, bool tmz)
{
   assert(width >= 1);
   assert(height >= 1);
   assert(depth >= 1);
   assert(util_is_power_of_two_nonzero(tiled->bpp));

   if (!info->sdma_supports_compression)
      assert(!tiled->is_compressed);

   const enum sdma_version sdma_ver = info->sdma_ip_version;
   const bool dcc = (sdma_ver >= SDMA_7_0)
      ? (linear->is_compressed || tiled->is_compressed)
      : tiled->is_compressed;

   const uint32_t header_dword = ac_sdma_get_tiled_header_dword(sdma_ver, tiled);
   const uint32_t info_dword = ac_sdma_get_tiled_info_dword(info, tiled);

   const bool uses_depth = (linear->offset.z != 0) ||
                           (tiled->offset.z != 0) ||
                           (depth != 1);
   ac_sdma_check_pitches(sdma_ver, linear->pitch, linear->slice_pitch, tiled->bpp, uses_depth);

   assert(linear->offset.x <= UINT16_MAX);
   assert(linear->offset.y <= UINT16_MAX);
   assert(linear->offset.z <= UINT16_MAX);
   assert(tiled->offset.x <= UINT16_MAX);
   assert(tiled->offset.y <= UINT16_MAX);
   assert(tiled->offset.z <= UINT16_MAX);

   const uint32_t packet_header =
      SDMA_PACKET(SDMA_OPCODE_COPY, SDMA_COPY_SUB_OPCODE_TILED_SUB_WINDOW, (tmz ? 4 : 0)) |
      ((uint32_t)dcc << 19) |
      ((uint32_t)detile << 31) |
      header_dword;

   ac_cmdbuf_begin(cs);
   ac_cmdbuf_emit(packet_header);
   ac_cmdbuf_emit((uint32_t)tiled->va);
   ac_cmdbuf_emit((uint32_t)(tiled->va >> 32));
   ac_cmdbuf_emit(tiled->offset.x | (tiled->offset.y << 16));
   ac_cmdbuf_emit(tiled->offset.z | ((tiled->extent.width - 1) << 16));
   ac_cmdbuf_emit((tiled->extent.height - 1) | ((tiled->extent.depth - 1) << 16));
   ac_cmdbuf_emit(info_dword);
   ac_cmdbuf_emit((uint32_t)linear->va);
   ac_cmdbuf_emit((uint32_t)(linear->va >> 32));
   ac_cmdbuf_emit(linear->offset.x | (linear->offset.y << 16));
   ac_cmdbuf_emit(linear->offset.z | ((linear->pitch - 1) << 16));
   ac_cmdbuf_emit(linear->slice_pitch - 1);

   if (SDMA_UNLIKELY(sdma_ver == SDMA_2_0)) {
      ac_cmdbuf_emit(width | (height << 16));
      ac_cmdbuf_emit(depth);
   } else {
      ac_cmdbuf_emit((width - 1) | ((height - 1) << 16));
      ac_cmdbuf_emit(depth - 1);
   }

   if (SDMA_UNLIKELY(dcc)) {
      if (sdma_ver >= SDMA_7_0) {
         const struct ac_sdma_surf *meta_src = detile ? tiled : linear;
         const struct ac_sdma_surf *meta_dst = detile ? linear : tiled;
         const uint32_t meta_config = ac_sdma7_get_metadata_config(info, meta_src, meta_dst);
         ac_cmdbuf_emit(meta_config);
      } else {
         const uint32_t meta_config = ac_sdma5_get_metadata_config(info, tiled, detile, tmz);
         ac_cmdbuf_emit((uint32_t)tiled->meta_va);
         ac_cmdbuf_emit((uint32_t)(tiled->meta_va >> 32));
         ac_cmdbuf_emit(meta_config);
      }
   }

   ac_cmdbuf_end();
}

/* ==================== T2T SUB-WINDOW ==================== */

void
ac_emit_sdma_copy_t2t_sub_window(struct ac_cmdbuf *cs, const struct radeon_info *info,
                                 const struct ac_sdma_surf *src,
                                 const struct ac_sdma_surf *dst,
                                 uint32_t width, uint32_t height, uint32_t depth)
{
   assert(width >= 1);
   assert(height >= 1);
   assert(depth >= 1);
   assert(info->sdma_ip_version >= SDMA_4_0);
   assert(util_is_power_of_two_nonzero(src->bpp));
   assert(util_is_power_of_two_nonzero(dst->bpp));
   assert(!(src->is_compressed && dst->is_compressed));

   const enum sdma_version sdma_ver = info->sdma_ip_version;
   const bool is_sdma4 = SDMA_LIKELY(sdma_ver >= SDMA_4_0) &&
                         SDMA_LIKELY(sdma_ver < SDMA_5_0);

   const uint32_t src_header_dword = ac_sdma_get_tiled_header_dword(sdma_ver, src);
   const uint32_t src_info_dword = ac_sdma_get_tiled_info_dword(info, src);
   const uint32_t dst_info_dword = ac_sdma_get_tiled_info_dword(info, dst);

   if (is_sdma4) {
      assert((src_header_dword >> 24) == 0);
      assert(!src->is_compressed);
      assert(!dst->is_compressed);
   }

   assert(src->offset.x <= UINT16_MAX);
   assert(src->offset.y <= UINT16_MAX);
   assert(src->offset.z <= UINT16_MAX);
   assert(dst->offset.x <= UINT16_MAX);
   assert(dst->offset.y <= UINT16_MAX);
   assert(dst->offset.z <= UINT16_MAX);

   const uint32_t dcc = (src->is_compressed || dst->is_compressed) ? 1u : 0u;
   const uint32_t dcc_dir = (src->is_compressed && !dst->is_compressed) ? 1u : 0u;

   const uint32_t packet_header =
      SDMA_PACKET(SDMA_OPCODE_COPY, SDMA_COPY_SUB_OPCODE_T2T_SUB_WINDOW, 0) |
      (dcc << 19) |
      (dcc_dir << 31) |
      src_header_dword;

   ac_cmdbuf_begin(cs);
   ac_cmdbuf_emit(packet_header);
   ac_cmdbuf_emit((uint32_t)src->va);
   ac_cmdbuf_emit((uint32_t)(src->va >> 32));
   ac_cmdbuf_emit(src->offset.x | (src->offset.y << 16));
   ac_cmdbuf_emit(src->offset.z | ((src->extent.width - 1) << 16));
   ac_cmdbuf_emit((src->extent.height - 1) | ((src->extent.depth - 1) << 16));
   ac_cmdbuf_emit(src_info_dword);
   ac_cmdbuf_emit((uint32_t)dst->va);
   ac_cmdbuf_emit((uint32_t)(dst->va >> 32));
   ac_cmdbuf_emit(dst->offset.x | (dst->offset.y << 16));
   ac_cmdbuf_emit(dst->offset.z | ((dst->extent.width - 1) << 16));
   ac_cmdbuf_emit((dst->extent.height - 1) | ((dst->extent.depth - 1) << 16));
   ac_cmdbuf_emit(dst_info_dword);
   ac_cmdbuf_emit((width - 1) | ((height - 1) << 16));
   ac_cmdbuf_emit(depth - 1);

   if (dcc) {
      if (sdma_ver >= SDMA_7_0) {
         if (dst->is_compressed) {
            const uint32_t dst_meta_config =
               ac_sdma7_get_metadata_config(info, src, dst);
            ac_cmdbuf_emit(dst_meta_config);
         }
      } else {
         if (dst->is_compressed) {
            const uint32_t dst_meta_config =
               ac_sdma5_get_metadata_config(info, dst, false, false);
            ac_cmdbuf_emit((uint32_t)dst->meta_va);
            ac_cmdbuf_emit((uint32_t)(dst->meta_va >> 32));
            ac_cmdbuf_emit(dst_meta_config);
         } else {
            const uint32_t src_meta_config =
               ac_sdma5_get_metadata_config(info, src, true, false);
            ac_cmdbuf_emit((uint32_t)src->meta_va);
            ac_cmdbuf_emit((uint32_t)(src->meta_va >> 32));
            ac_cmdbuf_emit(src_meta_config);
         }
      }
   }

   ac_cmdbuf_end();
}
