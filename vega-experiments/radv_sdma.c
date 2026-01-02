/*
 * Copyright 2010 Jerome Glisse <glisse@freedesktop.org>
 * Copyright 2015-2021 Advanced Micro Devices, Inc.
 * Copyright 2023 Valve Corporation
 * All Rights Reserved.
 *
 * SPDX-License-Identifier: MIT
 */

#include "radv_sdma.h"
#include "util/macros.h"
#include "util/u_memory.h"
#include "radv_cs.h"
#include "radv_formats.h"

#include "ac_cmdbuf_sdma.h"
#include "ac_formats.h"

struct radv_sdma_chunked_copy_info {
   unsigned extent_horizontal_blocks;
   unsigned extent_vertical_blocks;
   unsigned aligned_row_pitch;
   unsigned num_rows_per_copy;
};

static const VkExtent3D radv_sdma_t2t_alignment_2d_and_planar[] = {
   {16, 16, 1}, /* 1 bpp */
   {16, 8, 1},  /* 2 bpp */
   {8, 8, 1},   /* 4 bpp */
   {8, 4, 1},   /* 8 bpp */
   {4, 4, 1},   /* 16 bpp */
};

static const VkExtent3D radv_sdma_t2t_alignment_3d[] = {
   {8, 4, 8}, /* 1 bpp */
   {4, 4, 8}, /* 2 bpp */
   {4, 4, 4}, /* 4 bpp */
   {4, 2, 4}, /* 8 bpp */
   {2, 2, 4}, /* 16 bpp */
};

ALWAYS_INLINE static unsigned
radv_sdma_pitch_alignment(const struct radv_device *device, const unsigned bpp)
{
   const struct radv_physical_device *pdev = radv_device_physical(device);

   if (pdev->info.sdma_ip_version >= SDMA_5_0)
      return MAX2(1, 4 / bpp);

   return 4;
}

ALWAYS_INLINE static uint32_t
radv_sdma_surface_type_from_aspect_mask(const VkImageAspectFlags aspectMask)
{
   if (aspectMask & VK_IMAGE_ASPECT_DEPTH_BIT)
      return 1;
   else if (aspectMask & VK_IMAGE_ASPECT_STENCIL_BIT)
      return 2;

   return 0;
}

ALWAYS_INLINE static VkExtent3D
radv_sdma_pixel_extent_to_blocks(const VkExtent3D extent, const unsigned blk_w, const unsigned blk_h)
{
   const VkExtent3D r = {
      .width = DIV_ROUND_UP(extent.width, blk_w),
      .height = DIV_ROUND_UP(extent.height, blk_h),
      .depth = extent.depth,
   };

   return r;
}

ALWAYS_INLINE static VkOffset3D
radv_sdma_pixel_offset_to_blocks(const VkOffset3D offset, const unsigned blk_w, const unsigned blk_h)
{
   /* Use integer division (truncation toward zero) for offsets.
    * For Vulkan-valid inputs, offsets are always non-negative and block-aligned
    * for compressed formats, so this produces correct floor division results.
    * The explicit casts ensure signed division semantics.
    */
   assert(blk_w > 0 && blk_h > 0);

   const VkOffset3D r = {
      .x = offset.x / (int32_t)blk_w,
      .y = offset.y / (int32_t)blk_h,
      .z = offset.z,
   };

   return r;
}

ALWAYS_INLINE static unsigned
radv_sdma_pixels_to_blocks(const unsigned linear_pitch, const unsigned blk_w)
{
   return DIV_ROUND_UP(linear_pitch, blk_w);
}

ALWAYS_INLINE static unsigned
radv_sdma_pixel_area_to_blocks(const unsigned linear_slice_pitch, const unsigned blk_w, const unsigned blk_h)
{
   return DIV_ROUND_UP(DIV_ROUND_UP(linear_slice_pitch, blk_w), blk_h);
}

static struct radv_sdma_chunked_copy_info
radv_sdma_get_chunked_copy_info(const struct radv_device *const device, const struct radv_sdma_surf *const img,
                                const VkExtent3D extent)
{
   const unsigned extent_horizontal_blocks = DIV_ROUND_UP(extent.width * img->texel_scale, img->blk_w);
   const unsigned extent_vertical_blocks = DIV_ROUND_UP(extent.height, img->blk_h);
   const unsigned aligned_row_pitch = align(extent_horizontal_blocks, 4);
   const unsigned aligned_row_bytes = aligned_row_pitch * img->bpp;

   /* Assume that we can always copy at least one full row at a time. */
   const unsigned max_num_rows_per_copy = MIN2(RADV_SDMA_TRANSFER_TEMP_BYTES / aligned_row_bytes, extent.height);
   assert(max_num_rows_per_copy);

   /* Ensure that the number of rows copied at a time is a power of two. */
   const unsigned num_rows_per_copy = MAX2(1, util_next_power_of_two(max_num_rows_per_copy + 1) / 2);

   const struct radv_sdma_chunked_copy_info r = {
      .extent_horizontal_blocks = extent_horizontal_blocks,
      .extent_vertical_blocks = extent_vertical_blocks,
      .aligned_row_pitch = aligned_row_pitch,
      .num_rows_per_copy = num_rows_per_copy,
   };

   return r;
}

static uint32_t
radv_sdma_get_bpe(const struct radv_image *const image, VkImageAspectFlags aspect_mask)
{
   const unsigned plane_idx = radv_plane_from_aspect(aspect_mask);
   const struct radeon_surf *surf = &image->planes[plane_idx].surface;
   const bool is_stencil_only = aspect_mask == VK_IMAGE_ASPECT_STENCIL_BIT;

   if (is_stencil_only) {
      return 1;
   } else if (vk_format_is_96bit(image->vk.format)) {
      /* Adjust the bpp for 96-bits formats because SDMA expects a power of two. */
      return 4;
   } else {
      return surf->bpe;
   }
}

static uint32_t
radv_sdma_get_texel_scale(const struct radv_image *const image)
{
   if (vk_format_is_96bit(image->vk.format)) {
      return 3;
   } else {
      return 1;
   }
}

struct radv_sdma_surf
radv_sdma_get_buf_surf(uint64_t buffer_va, const struct radv_image *const image, const VkBufferImageCopy2 *const region)
{
   assert(util_bitcount(region->imageSubresource.aspectMask) == 1);

   const uint32_t texel_scale = radv_sdma_get_texel_scale(image);
   const unsigned pitch = (region->bufferRowLength ? region->bufferRowLength : region->imageExtent.width) * texel_scale;
   const unsigned slice_pitch =
      (region->bufferImageHeight ? region->bufferImageHeight : region->imageExtent.height) * pitch;

   const unsigned plane_idx = radv_plane_from_aspect(region->imageSubresource.aspectMask);
   const struct radeon_surf *surf = &image->planes[plane_idx].surface;
   const uint32_t bpe = radv_sdma_get_bpe(image, region->imageSubresource.aspectMask);

   const struct radv_sdma_surf info = {
      .va = buffer_va + region->bufferOffset,
      .pitch = pitch,
      .slice_pitch = slice_pitch,
      .bpp = bpe,
      .blk_w = surf->blk_w,
      .blk_h = surf->blk_h,
      .texel_scale = texel_scale,
      .is_linear = true,
   };

   return info;
}

struct radv_sdma_surf
radv_sdma_get_surf(const struct radv_device *const device, const struct radv_image *const image,
                   const VkImageSubresourceLayers subresource, const VkOffset3D offset)
{
   assert(util_bitcount(subresource.aspectMask) == 1);

   const struct radv_physical_device *pdev = radv_device_physical(device);
   const unsigned plane_idx = radv_plane_from_aspect(subresource.aspectMask);
   const unsigned binding_idx = image->disjoint ? plane_idx : 0;
   const struct radeon_surf *const surf = &image->planes[plane_idx].surface;
   const struct radv_image_binding *binding = &image->bindings[binding_idx];
   const uint64_t va = binding->addr;
   const uint32_t bpe = radv_sdma_get_bpe(image, subresource.aspectMask);
   struct radv_sdma_surf info = {
      .surf = surf,
      .format = image->vk.format,
      .aspect_format = vk_format_get_aspect_format(image->vk.format, subresource.aspectMask),
      .extent =
         {
            .width = vk_format_get_plane_width(image->vk.format, plane_idx, image->vk.extent.width),
            .height = vk_format_get_plane_height(image->vk.format, plane_idx, image->vk.extent.height),
            .depth = image->vk.image_type == VK_IMAGE_TYPE_3D ? image->vk.extent.depth : image->vk.array_layers,
         },
      .offset =
         {
            .x = offset.x,
            .y = offset.y,
            .z = image->vk.image_type == VK_IMAGE_TYPE_3D ? offset.z : subresource.baseArrayLayer,
         },
      .bpp = bpe,
      .blk_w = surf->blk_w,
      .blk_h = surf->blk_h,
      .first_level = subresource.mipLevel,
      .mip_levels = image->vk.mip_levels,
      .micro_tile_mode = surf->micro_tile_mode,
      .texel_scale = radv_sdma_get_texel_scale(image),
      .is_linear = surf->is_linear,
      .is_3d = surf->u.gfx9.resource_type == RADEON_RESOURCE_3D,
   };

   const uint64_t surf_offset = (subresource.aspectMask == VK_IMAGE_ASPECT_STENCIL_BIT) ? surf->u.gfx9.zs.stencil_offset
                                                                                        : surf->u.gfx9.surf_offset;

   if (surf->is_linear) {
      info.va = va + surf_offset + surf->u.gfx9.offset[subresource.mipLevel];
      info.pitch = surf->u.gfx9.pitch[subresource.mipLevel];
      info.slice_pitch = surf->blk_w * surf->blk_h * surf->u.gfx9.surf_slice_size / bpe;
   } else {
      const bool htile_enabled = radv_htile_enabled(image, subresource.mipLevel);

      /* 1D resources should be linear. */
      assert(surf->u.gfx9.resource_type != RADEON_RESOURCE_1D);

      info.va = (va + surf_offset) | surf->tile_swizzle << 8;

      if (pdev->info.gfx_level >= GFX12) {
         info.is_compressed = binding->bo && binding->bo->gfx12_allow_dcc;
      } else if (pdev->info.sdma_supports_compression &&
                 (radv_dcc_enabled(image, subresource.mipLevel) || htile_enabled)) {
         info.is_compressed = true;
      }

      if (info.is_compressed) {
         info.meta_va = va + surf->meta_offset;
         info.surface_type = radv_sdma_surface_type_from_aspect_mask(subresource.aspectMask);
         info.htile_enabled = htile_enabled;
      }
   }

   return info;
}

void
radv_sdma_emit_nop(const struct radv_device *device, struct radv_cmd_stream *cs)
{
   radeon_check_space(device->ws, cs->b, 1);
   ac_emit_sdma_nop(cs->b);
}

void
radv_sdma_copy_memory(const struct radv_device *device, struct radv_cmd_stream *cs, uint64_t src_va, uint64_t dst_va,
                      uint64_t size)
{
   const struct radv_physical_device *pdev = radv_device_physical(device);

   while (size > 0) {
      radeon_check_space(device->ws, cs->b, 7);
      uint64_t bytes_written = ac_emit_sdma_copy_linear(cs->b, pdev->info.sdma_ip_version, src_va, dst_va, size, false);

      size -= bytes_written;
      src_va += bytes_written;
      dst_va += bytes_written;
   }
}

void
radv_sdma_fill_memory(const struct radv_device *device, struct radv_cmd_stream *cs,
                      uint64_t va, uint64_t size, const uint32_t value)
{
   /* Early return for zero-size fills - no work needed. */
   if (size == 0)
      return;

   /*
    * SDMA constant fill operates in DWORD (4-byte) units.
    * Per Vulkan spec (vkCmdFillBuffer):
    *   - dstOffset must be a multiple of 4
    *   - size must be a multiple of 4, or VK_WHOLE_SIZE
    *
    * These preconditions are validated here to catch internal driver bugs.
    */
   assert(util_is_aligned(va, 4));
   assert(util_is_aligned(size, 4));

   const struct radv_physical_device *pdev = radv_device_physical(device);
   const enum sdma_version ver = pdev->info.sdma_ip_version;

   while (size > 0) {
      radeon_check_space(device->ws, cs->b, 5);

      const uint64_t bytes_written = ac_emit_sdma_constant_fill(cs->b, ver, va, size, value);

      /*
       * For properly aligned inputs with size >= 4, bytes_written is guaranteed
       * to be non-zero. This safety check prevents an infinite loop if the
       * invariant is ever violated due to a bug in ac_emit_sdma_constant_fill
       * or misuse of this function.
       *
       * In debug builds, we assert to catch the bug immediately.
       * In release builds, we break to prevent a GPU hang.
       */
      if (bytes_written == 0) {
         assert(!"ac_emit_sdma_constant_fill returned 0 unexpectedly");
         break;
      }

      /*
       * Sanity check: bytes_written should never exceed remaining size,
       * and should always be DWORD-aligned.
       */
      assert(bytes_written <= size);
      assert(util_is_aligned(bytes_written, 4));

      size -= bytes_written;
      va += bytes_written;
   }
}

static void
radv_sdma_emit_copy_linear_sub_window(const struct radv_device *device, struct radv_cmd_stream *cs,
                                      const struct radv_sdma_surf *const src, const struct radv_sdma_surf *const dst,
                                      const VkExtent3D pix_extent)
{
   const struct radv_physical_device *pdev = radv_device_physical(device);

   /* Validate block sizes to prevent division by zero. */
   assert(src->blk_w > 0 && src->blk_h > 0);
   assert(dst->blk_w > 0 && dst->blk_h > 0);

   VkOffset3D src_off = radv_sdma_pixel_offset_to_blocks(src->offset, src->blk_w, src->blk_h);
   VkOffset3D dst_off = radv_sdma_pixel_offset_to_blocks(dst->offset, dst->blk_w, dst->blk_h);
   VkExtent3D ext = radv_sdma_pixel_extent_to_blocks(pix_extent, src->blk_w, src->blk_h);
   const unsigned src_pitch = radv_sdma_pixels_to_blocks(src->pitch, src->blk_w);
   const unsigned dst_pitch = radv_sdma_pixels_to_blocks(dst->pitch, dst->blk_w);
   const unsigned src_slice_pitch = radv_sdma_pixel_area_to_blocks(src->slice_pitch, src->blk_w, src->blk_h);
   const unsigned dst_slice_pitch = radv_sdma_pixel_area_to_blocks(dst->slice_pitch, dst->blk_w, dst->blk_h);

   /* Adjust offset/extent for 96-bit formats because SDMA expects a power of two bpp. */
   const uint32_t texel_scale = src->texel_scale ? src->texel_scale : dst->texel_scale;
   assert(texel_scale > 0);

   /* Safe multiplication - texel_scale is 1 or 3, offsets are reasonably bounded. */
   src_off.x *= (int)texel_scale;
   dst_off.x *= (int)texel_scale;
   ext.width *= texel_scale;

   /* Validate offsets are non-negative after conversion for SDMA packet. */
   assert(src_off.x >= 0 && src_off.y >= 0 && src_off.z >= 0);
   assert(dst_off.x >= 0 && dst_off.y >= 0 && dst_off.z >= 0);

   const struct ac_sdma_surf_linear surf_src = {
      .va = src->va,
      .offset =
         {
            .x = (uint32_t)src_off.x,
            .y = (uint32_t)src_off.y,
            .z = (uint32_t)src_off.z,
         },
      .bpp = src->bpp,
      .pitch = src_pitch,
      .slice_pitch = src_slice_pitch,
   };

   const struct ac_sdma_surf_linear surf_dst = {
      .va = dst->va,
      .offset =
         {
            .x = (uint32_t)dst_off.x,
            .y = (uint32_t)dst_off.y,
            .z = (uint32_t)dst_off.z,
         },
      .bpp = dst->bpp,
      .pitch = dst_pitch,
      .slice_pitch = dst_slice_pitch,
   };

   radeon_check_space(device->ws, cs->b, 13);
   ac_emit_sdma_copy_linear_sub_window(cs->b, pdev->info.sdma_ip_version, &surf_src, &surf_dst,
                                       ext.width, ext.height, ext.depth);
}

static void
radv_sdma_emit_copy_tiled_sub_window(const struct radv_device *device, struct radv_cmd_stream *cs,
                                     const struct radv_sdma_surf *const tiled,
                                     const struct radv_sdma_surf *const linear,
                                     const VkExtent3D pix_extent, const bool detile)
{
   const struct radv_physical_device *pdev = radv_device_physical(device);

   /* Validate block sizes to prevent division by zero. */
   assert(tiled->blk_w > 0 && tiled->blk_h > 0);
   assert(linear->blk_w > 0 && linear->blk_h > 0);

   /* For tiled operations, the surface descriptor must be valid. */
   assert(tiled->surf != NULL);

   const VkOffset3D linear_off = radv_sdma_pixel_offset_to_blocks(linear->offset, linear->blk_w, linear->blk_h);
   const VkOffset3D tiled_off = radv_sdma_pixel_offset_to_blocks(tiled->offset, tiled->blk_w, tiled->blk_h);
   const VkExtent3D tiled_ext = radv_sdma_pixel_extent_to_blocks(tiled->extent, tiled->blk_w, tiled->blk_h);
   const VkExtent3D ext = radv_sdma_pixel_extent_to_blocks(pix_extent, tiled->blk_w, tiled->blk_h);
   const unsigned linear_pitch = radv_sdma_pixels_to_blocks(linear->pitch, tiled->blk_w);
   const unsigned linear_slice_pitch = radv_sdma_pixel_area_to_blocks(linear->slice_pitch,
                                                                      tiled->blk_w, tiled->blk_h);

   /* Validate offsets are non-negative for SDMA packet. */
   assert(linear_off.x >= 0 && linear_off.y >= 0 && linear_off.z >= 0);
   assert(tiled_off.x >= 0 && tiled_off.y >= 0 && tiled_off.z >= 0);

   const struct ac_sdma_surf_linear surf_linear = {
      .va = linear->va,
      .offset =
         {
            .x = (uint32_t)linear_off.x,
            .y = (uint32_t)linear_off.y,
            .z = (uint32_t)linear_off.z,
         },
      .pitch = linear_pitch,
      .slice_pitch = linear_slice_pitch,
   };

   const struct ac_sdma_surf_tiled surf_tiled = {
      .surf = tiled->surf,
      .va = tiled->va,
      .format = radv_format_to_pipe_format(tiled->aspect_format),
      .bpp = tiled->bpp,
      .offset =
         {
            .x = (uint32_t)tiled_off.x,
            .y = (uint32_t)tiled_off.y,
            .z = (uint32_t)tiled_off.z,
         },
      .extent =
         {
            .width = tiled_ext.width,
            .height = tiled_ext.height,
            .depth = tiled_ext.depth,
         },
      .first_level = tiled->first_level,
      .num_levels = tiled->mip_levels,
      .is_compressed = tiled->is_compressed,
      .surf_type = tiled->surface_type,
      .meta_va = tiled->meta_va,
      .htile_enabled = tiled->htile_enabled,
   };

   radeon_check_space(device->ws, cs->b, 17);
   ac_emit_sdma_copy_tiled_sub_window(cs->b, &pdev->info, &surf_linear, &surf_tiled, detile,
                                      ext.width, ext.height, ext.depth, false);
}

static void
radv_sdma_emit_copy_t2t_sub_window(const struct radv_device *device, struct radv_cmd_stream *cs,
                                   const struct radv_sdma_surf *const src,
                                   const struct radv_sdma_surf *const dst,
                                   const VkExtent3D px_extent)
{
   const struct radv_physical_device *pdev = radv_device_physical(device);

   /* Validate block sizes to prevent division by zero. */
   assert(src->blk_w > 0 && src->blk_h > 0);
   assert(dst->blk_w > 0 && dst->blk_h > 0);

   /* For tiled operations, the surface descriptors must be valid. */
   assert(src->surf != NULL);
   assert(dst->surf != NULL);

   const VkOffset3D src_off = radv_sdma_pixel_offset_to_blocks(src->offset, src->blk_w, src->blk_h);
   const VkOffset3D dst_off = radv_sdma_pixel_offset_to_blocks(dst->offset, dst->blk_w, dst->blk_h);
   const VkExtent3D src_ext = radv_sdma_pixel_extent_to_blocks(src->extent, src->blk_w, src->blk_h);
   const VkExtent3D dst_ext = radv_sdma_pixel_extent_to_blocks(dst->extent, dst->blk_w, dst->blk_h);
   const VkExtent3D ext = radv_sdma_pixel_extent_to_blocks(px_extent, src->blk_w, src->blk_h);

   /* Validate offsets are non-negative for SDMA packet. */
   assert(src_off.x >= 0 && src_off.y >= 0 && src_off.z >= 0);
   assert(dst_off.x >= 0 && dst_off.y >= 0 && dst_off.z >= 0);

   const struct ac_sdma_surf_tiled surf_src = {
      .surf = src->surf,
      .va = src->va,
      .format = radv_format_to_pipe_format(src->aspect_format),
      .bpp = src->bpp,
      .offset =
         {
            .x = (uint32_t)src_off.x,
            .y = (uint32_t)src_off.y,
            .z = (uint32_t)src_off.z,
         },
      .extent =
         {
            .width = src_ext.width,
            .height = src_ext.height,
            .depth = src_ext.depth,
         },
      .first_level = src->first_level,
      .num_levels = src->mip_levels,
      .is_compressed = src->is_compressed,
      .surf_type = src->surface_type,
      .meta_va = src->meta_va,
      .htile_enabled = src->htile_enabled,
   };

   const struct ac_sdma_surf_tiled surf_dst = {
      .surf = dst->surf,
      .va = dst->va,
      .format = radv_format_to_pipe_format(dst->aspect_format),
      .bpp = dst->bpp,
      .offset =
         {
            .x = (uint32_t)dst_off.x,
            .y = (uint32_t)dst_off.y,
            .z = (uint32_t)dst_off.z,
         },
      .extent =
         {
            .width = dst_ext.width,
            .height = dst_ext.height,
            .depth = dst_ext.depth,
         },
      .first_level = dst->first_level,
      .num_levels = dst->mip_levels,
      .is_compressed = dst->is_compressed,
      .surf_type = dst->surface_type,
      .meta_va = dst->meta_va,
      .htile_enabled = dst->htile_enabled,
   };

   radeon_check_space(device->ws, cs->b, 18);
   ac_emit_sdma_copy_t2t_sub_window(cs->b, &pdev->info, &surf_src, &surf_dst,
                                    ext.width, ext.height, ext.depth);
}

void
radv_sdma_copy_buffer_image(const struct radv_device *device, struct radv_cmd_stream *cs,
                            const struct radv_sdma_surf *buf, const struct radv_sdma_surf *img,
                            const VkExtent3D extent, bool to_image)
{
   /* Early return for empty copies - prevents underflow in SDMA packet fields
    * where hardware expects (dimension - 1) encoding.
    */
   if (extent.width == 0 || extent.height == 0 || extent.depth == 0)
      return;

   if (img->is_linear) {
      if (to_image)
         radv_sdma_emit_copy_linear_sub_window(device, cs, buf, img, extent);
      else
         radv_sdma_emit_copy_linear_sub_window(device, cs, img, buf, extent);
   } else {
      radv_sdma_emit_copy_tiled_sub_window(device, cs, img, buf, extent, !to_image);
   }
}

bool
radv_sdma_use_unaligned_buffer_image_copy(const struct radv_device *device, const struct radv_sdma_surf *buf,
                                          const struct radv_sdma_surf *img, const VkExtent3D ext)
{
   const unsigned pitch_blocks = radv_sdma_pixels_to_blocks(buf->pitch, img->blk_w);
   if (!util_is_aligned(pitch_blocks, radv_sdma_pitch_alignment(device, img->bpp)))
      return true;

   const bool uses_depth = img->offset.z != 0 || ext.depth != 1;
   if (!img->is_linear && uses_depth) {
      const unsigned slice_pitch_blocks = radv_sdma_pixel_area_to_blocks(buf->slice_pitch, img->blk_w, img->blk_h);
      if (!util_is_aligned(slice_pitch_blocks, 4))
         return true;
   }

   return false;
}

void
radv_sdma_copy_buffer_image_unaligned(const struct radv_device *device, struct radv_cmd_stream *cs,
                                      const struct radv_sdma_surf *buf, const struct radv_sdma_surf *img_in,
                                      const VkExtent3D base_extent, struct radeon_winsys_bo *temp_bo, bool to_image)
{
   /* Early return for empty copies - avoids unnecessary work. */
   if (base_extent.width == 0 || base_extent.height == 0 || base_extent.depth == 0)
      return;

   const struct radv_sdma_chunked_copy_info info = radv_sdma_get_chunked_copy_info(device, img_in, base_extent);
   struct radv_sdma_surf img = *img_in;
   struct radv_sdma_surf tmp = {
      .va = radv_buffer_get_va(temp_bo),
      .bpp = img.bpp,
      .blk_w = img.blk_w,
      .blk_h = img.blk_h,
      .pitch = info.aligned_row_pitch * img.blk_w,
      .slice_pitch = info.aligned_row_pitch * img.blk_w * info.extent_vertical_blocks * img.blk_h,
      .texel_scale = buf->texel_scale,
   };

   VkExtent3D extent = base_extent;
   const unsigned buf_pitch_blocks = DIV_ROUND_UP(buf->pitch, img.blk_w);
   const unsigned buf_slice_pitch_blocks = DIV_ROUND_UP(DIV_ROUND_UP(buf->slice_pitch, img.blk_w), img.blk_h);
   assert(buf_pitch_blocks);
   assert(buf_slice_pitch_blocks);
   extent.depth = 1;

   for (unsigned slice = 0; slice < base_extent.depth; ++slice) {
      for (unsigned row = 0; row < info.extent_vertical_blocks; row += info.num_rows_per_copy) {
         const unsigned rows = MIN2(info.extent_vertical_blocks - row, info.num_rows_per_copy);

         img.offset.y = img_in->offset.y + row * img.blk_h;
         img.offset.z = img_in->offset.z + slice;
         extent.height = rows * img.blk_h;
         tmp.slice_pitch = tmp.pitch * rows * img.blk_h;

         if (!to_image) {
            /* Copy the rows from the source image to the temporary buffer. */
            if (img.is_linear)
               radv_sdma_emit_copy_linear_sub_window(device, cs, &img, &tmp, extent);
            else
               radv_sdma_emit_copy_tiled_sub_window(device, cs, &img, &tmp, extent, true);

            /* Wait for the copy to finish. */
            radv_sdma_emit_nop(device, cs);
         }

         /* buffer to image: copy each row from source buffer to temporary buffer.
          * image to buffer: copy each row from temporary buffer to destination buffer.
          */
         for (unsigned r = 0; r < rows; ++r) {
            const uint64_t buf_va =
               buf->va + slice * buf_slice_pitch_blocks * img.bpp + (row + r) * buf_pitch_blocks * img.bpp;
            const uint64_t tmp_va = tmp.va + r * info.aligned_row_pitch * img.bpp;
            radv_sdma_copy_memory(device, cs, to_image ? buf_va : tmp_va, to_image ? tmp_va : buf_va,
                                  info.extent_horizontal_blocks * img.bpp);
         }

         /* Wait for the copy to finish. */
         radv_sdma_emit_nop(device, cs);

         if (to_image) {
            /* Copy the rows from the temporary buffer to the destination image. */
            if (img.is_linear)
               radv_sdma_emit_copy_linear_sub_window(device, cs, &tmp, &img, extent);
            else
               radv_sdma_emit_copy_tiled_sub_window(device, cs, &img, &tmp, extent, false);

            /* Wait for the copy to finish. */
            radv_sdma_emit_nop(device, cs);
         }
      }
   }
}

void
radv_sdma_copy_image(const struct radv_device *device, struct radv_cmd_stream *cs,
                     const struct radv_sdma_surf *src, const struct radv_sdma_surf *dst,
                     const VkExtent3D extent)
{
   /* Early return for empty copies - prevents underflow in SDMA packet fields
    * where hardware expects (dimension - 1) encoding.
    */
   if (extent.width == 0 || extent.height == 0 || extent.depth == 0)
      return;

   if (src->is_linear) {
      if (dst->is_linear) {
         radv_sdma_emit_copy_linear_sub_window(device, cs, src, dst, extent);
      } else {
         radv_sdma_emit_copy_tiled_sub_window(device, cs, dst, src, extent, false);
      }
   } else {
      if (dst->is_linear) {
         radv_sdma_emit_copy_tiled_sub_window(device, cs, src, dst, extent, true);
      } else {
         radv_sdma_emit_copy_t2t_sub_window(device, cs, src, dst, extent);
      }
   }
}

bool
radv_sdma_use_t2t_scanline_copy(const struct radv_device *device, const struct radv_sdma_surf *src,
                                const struct radv_sdma_surf *dst, const VkExtent3D extent)
{
   /* These need a linear-to-linear / linear-to-tiled copy. */
   if (src->is_linear || dst->is_linear)
      return false;

   /* SDMA can't do format conversion. */
   assert(src->bpp == dst->bpp);

   /* SDMA only supports power-of-two bpp up to 16 bytes.
    * Reject unsupported bpp to prevent array out-of-bounds access below.
    */
   if (!util_is_power_of_two_nonzero(src->bpp) || src->bpp > 16)
      return true;

   const struct radv_physical_device *pdev = radv_device_physical(device);
   const enum sdma_version ver = pdev->info.sdma_ip_version;

   if (ver < SDMA_5_0) {
      /* SDMA v4.x and older doesn't support proper mip level selection in T2T packets.
       * For Vega 64 (SDMA 4.0), we must use scanline copy for any multi-mip image.
       */
      if (src->mip_levels > 1 || dst->mip_levels > 1)
         return true;
   }

   /* The two images can have a different block size,
    * but must have the same swizzle mode.
    */
   if (src->micro_tile_mode != dst->micro_tile_mode)
      return true;

   /* The T2T subwindow copy packet only has fields for one metadata configuration.
    * It can either compress or decompress, or copy uncompressed images, but it
    * can't copy from a compressed image to another.
    */
   if (src->is_compressed && dst->is_compressed)
      return true;

   const bool needs_3d_alignment = src->is_3d && (src->micro_tile_mode == RADEON_MICRO_MODE_DISPLAY ||
                                                  src->micro_tile_mode == RADEON_MICRO_MODE_STANDARD);
   const unsigned log2bpp = util_logbase2(src->bpp);

   /* log2bpp is guaranteed to be 0-4 due to the bpp validation above. */
   assert(log2bpp < ARRAY_SIZE(radv_sdma_t2t_alignment_2d_and_planar));
   assert(log2bpp < ARRAY_SIZE(radv_sdma_t2t_alignment_3d));

   const VkExtent3D *const alignment =
      needs_3d_alignment ? &radv_sdma_t2t_alignment_3d[log2bpp]
                         : &radv_sdma_t2t_alignment_2d_and_planar[log2bpp];

   const VkExtent3D copy_extent_blk = radv_sdma_pixel_extent_to_blocks(extent, src->blk_w, src->blk_h);
   const VkOffset3D src_offset_blk = radv_sdma_pixel_offset_to_blocks(src->offset, src->blk_w, src->blk_h);
   const VkOffset3D dst_offset_blk = radv_sdma_pixel_offset_to_blocks(dst->offset, dst->blk_w, dst->blk_h);

   if (!util_is_aligned(copy_extent_blk.width, alignment->width) ||
       !util_is_aligned(copy_extent_blk.height, alignment->height) ||
       !util_is_aligned(copy_extent_blk.depth, alignment->depth))
      return true;

   if (!util_is_aligned(src_offset_blk.x, alignment->width) ||
       !util_is_aligned(src_offset_blk.y, alignment->height) ||
       !util_is_aligned(src_offset_blk.z, alignment->depth))
      return true;

   if (!util_is_aligned(dst_offset_blk.x, alignment->width) ||
       !util_is_aligned(dst_offset_blk.y, alignment->height) ||
       !util_is_aligned(dst_offset_blk.z, alignment->depth))
      return true;

   if (ver < SDMA_6_0 &&
       ((src->format == VK_FORMAT_S8_UINT && vk_format_is_color(dst->format)) ||
        (vk_format_is_color(src->format) && dst->format == VK_FORMAT_S8_UINT))) {
      /* For weird reasons, color<->stencil only T2T subwindow copies on SDMA4-5 don't work as
       * expected, and the driver needs to fallback to scanline copies to workaround them.
       * This affects Vega 64 (SDMA 4.0) and Navi 1x/2x (SDMA 5.x).
       */
      return true;
   }

   return false;
}

void
radv_sdma_copy_image_t2t_scanline(const struct radv_device *device, struct radv_cmd_stream *cs,
                                  const struct radv_sdma_surf *src, const struct radv_sdma_surf *dst,
                                  const VkExtent3D extent, struct radeon_winsys_bo *temp_bo)
{
   assert(device);
   assert(cs);
   assert(src);
   assert(dst);
   assert(temp_bo);

   /* Early return for empty copies - avoids underflow in SDMA packet width/height/depth fields
    * where the hardware expects (dimension - 1) encoding.
    */
   if (extent.width == 0 || extent.height == 0 || extent.depth == 0)
      return;

   /* SDMA cannot perform format conversion. Block dimensions and bytes-per-pixel must match
    * between source and destination for the scanline copy algorithm to work correctly.
    */
   assert(src->blk_w == dst->blk_w);
   assert(src->blk_h == dst->blk_h);
   assert(src->bpp == dst->bpp);

   const struct radv_sdma_chunked_copy_info info = radv_sdma_get_chunked_copy_info(device, src, extent);
   const uint64_t temp_va = radv_buffer_get_va(temp_bo);

   struct radv_sdma_surf t2l_src = *src;
   struct radv_sdma_surf t2l_dst = {
      .va = temp_va,
      .bpp = src->bpp,
      .blk_w = src->blk_w,
      .blk_h = src->blk_h,
      .pitch = info.aligned_row_pitch * src->blk_w,
      .texel_scale = 1,
      .is_linear = true,
   };

   struct radv_sdma_surf l2t_dst = *dst;
   struct radv_sdma_surf l2t_src = {
      .va = temp_va,
      .bpp = dst->bpp,
      .blk_w = dst->blk_w,
      .blk_h = dst->blk_h,
      .pitch = info.aligned_row_pitch * dst->blk_w,
      .texel_scale = 1,
      .is_linear = true,
   };

   for (unsigned slice = 0; slice < extent.depth; ++slice) {
      for (unsigned row = 0; row < info.extent_vertical_blocks; row += info.num_rows_per_copy) {
         const unsigned rows = MIN2(info.extent_vertical_blocks - row, info.num_rows_per_copy);

         /* Phase 1: Detile - copy from source tiled image to temporary linear buffer. */
         const VkExtent3D t2l_extent = {
            .width = info.extent_horizontal_blocks * src->blk_w,
            .height = rows * src->blk_h,
            .depth = 1,
         };

         t2l_src.offset.y = src->offset.y + row * src->blk_h;
         t2l_src.offset.z = src->offset.z + slice;
         t2l_dst.slice_pitch = t2l_dst.pitch * t2l_extent.height;

         radv_sdma_emit_copy_tiled_sub_window(device, cs, &t2l_src, &t2l_dst, t2l_extent, true);

         /* SDMA NOP acts as a fence - wait for detile to complete before retiling
          * to avoid RAW hazard on the temporary buffer.
          */
         radv_sdma_emit_nop(device, cs);

         /* Phase 2: Retile - copy from temporary linear buffer to destination tiled image. */
         const VkExtent3D l2t_extent = {
            .width = info.extent_horizontal_blocks * dst->blk_w,
            .height = rows * dst->blk_h,
            .depth = 1,
         };

         l2t_dst.offset.y = dst->offset.y + row * dst->blk_h;
         l2t_dst.offset.z = dst->offset.z + slice;
         l2t_src.slice_pitch = l2t_src.pitch * l2t_extent.height;

         radv_sdma_emit_copy_tiled_sub_window(device, cs, &l2t_dst, &l2t_src, l2t_extent, false);

         /* Wait for retile to complete before reusing temp buffer in next iteration
          * to avoid WAW hazard.
          */
         radv_sdma_emit_nop(device, cs);
      }
   }
}

bool
radv_sdma_supports_image(const struct radv_device *device, const struct radv_image *image)
{
   assert(device);
   assert(image);

   const struct radv_physical_device *pdev = radv_device_physical(device);

   /* Emulated formats require shader-based copies, not SDMA. */
   if (radv_is_format_emulated(pdev, image->vk.format))
      return false;

   /* Sparse residency requires hardware support for proper page fault handling. */
   if (!pdev->info.sdma_supports_sparse &&
       (image->vk.create_flags & VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT))
      return false;

   /* SDMA copy packets don't support MSAA sample patterns. */
   if (image->vk.samples != VK_SAMPLE_COUNT_1_BIT)
      return false;

   return true;
}
