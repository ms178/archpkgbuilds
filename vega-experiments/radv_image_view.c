/*
 * Copyright © 2016 Red Hat.
 * Copyright © 2016 Bas Nieuwenhuizen
 *
 * based in part on anv driver which is:
 * Copyright © 2015 Intel Corporation
 *
 * SPDX-License-Identifier: MIT
 */

#include "vk_log.h"

#include "radv_buffer.h"
#include "radv_buffer_view.h"
#include "radv_debug.h"
#include "radv_entrypoints.h"
#include "radv_formats.h"
#include "radv_image.h"
#include "radv_image_view.h"

#include "ac_descriptors.h"
#include "ac_formats.h"
#include "gfx10_format_table.h"

/* GFX9 ISA: V_008F1C_SQ_RSRC_IMG_* constants */

static ALWAYS_INLINE unsigned
radv_tex_dim(VkImageType image_type, VkImageViewType view_type, unsigned nr_layers, unsigned nr_samples,
             bool is_storage_image, bool gfx9)
{
   if (unlikely(view_type == VK_IMAGE_VIEW_TYPE_CUBE || view_type == VK_IMAGE_VIEW_TYPE_CUBE_ARRAY))
      return is_storage_image ? V_008F1C_SQ_RSRC_IMG_2D_ARRAY : V_008F1C_SQ_RSRC_IMG_CUBE;

   /* GFX9 allocates 1D textures as 2D. Optimized check. */
   if (likely(gfx9) && image_type == VK_IMAGE_TYPE_1D)
      image_type = VK_IMAGE_TYPE_2D;

   switch (image_type) {
   case VK_IMAGE_TYPE_1D:
      return nr_layers > 1 ? V_008F1C_SQ_RSRC_IMG_1D_ARRAY : V_008F1C_SQ_RSRC_IMG_1D;
   case VK_IMAGE_TYPE_2D:
      if (nr_samples > 1)
         return nr_layers > 1 ? V_008F1C_SQ_RSRC_IMG_2D_MSAA_ARRAY : V_008F1C_SQ_RSRC_IMG_2D_MSAA;
      else
         return nr_layers > 1 ? V_008F1C_SQ_RSRC_IMG_2D_ARRAY : V_008F1C_SQ_RSRC_IMG_2D;
   case VK_IMAGE_TYPE_3D:
      if (view_type == VK_IMAGE_VIEW_TYPE_3D)
         return V_008F1C_SQ_RSRC_IMG_3D;
      else
         return V_008F1C_SQ_RSRC_IMG_2D_ARRAY;
   default:
      UNREACHABLE("illegal image type");
   }
}

static ALWAYS_INLINE enum pipe_format
radv_normalize_texture_format(const struct radv_image *image, VkFormat vk_format)
{
   enum pipe_format format = radv_format_to_pipe_format(vk_format);

   if (unlikely(image->vk.format == VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK &&
                format == PIPE_FORMAT_R8G8B8A8_UNORM))
      return PIPE_FORMAT_R8G8B8X8_UNORM;

   if (unlikely(image->vk.format == VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK &&
                format == PIPE_FORMAT_R8G8B8A8_SRGB))
      return PIPE_FORMAT_R8G8B8X8_SRGB;

   return format;
}

void
radv_set_mutable_tex_desc_fields(struct radv_device *device, struct radv_image *image,
                                 const struct legacy_surf_level *base_level_info, unsigned plane_id,
                                 unsigned base_level, unsigned first_level, unsigned block_width, bool is_stencil,
                                 bool is_storage_image, bool disable_compression, bool enable_write_compression,
                                 uint32_t *state, const struct ac_surf_nbc_view *nbc_view, uint64_t offset)
{
   const struct radv_physical_device *pdev = radv_device_physical(device);
   struct radv_image_plane *plane = &image->planes[plane_id];
   const uint32_t bind_idx = image->disjoint ? plane_id : 0;
   const struct radv_image_binding *binding = &image->bindings[bind_idx];
   const bool dcc_enabled = pdev->info.gfx_level >= GFX12 || radv_dcc_enabled(image, first_level);
   const bool tc_compat_htile_enabled =
      !disable_compression && radv_tc_compat_htile_enabled(image, first_level);
   const bool iterate_256 =
      pdev->info.gfx_level >= GFX10 && radv_image_get_iterate256(device, image);
   const uint64_t gpu_address = binding->bo ? binding->addr + offset : 0;

   const struct ac_mutable_tex_state ac_state = {
      .surf = &plane->surface,
      .va = gpu_address,
      .gfx10 =
         {
            .nbc_view = nbc_view,
            .write_compress_enable = dcc_enabled && is_storage_image && enable_write_compression,
            .iterate_256 = iterate_256,
         },
      .gfx6 =
         {
            .base_level_info = base_level_info,
            .base_level = base_level,
            .block_width = block_width,
         },
      .is_stencil = is_stencil,
      .dcc_enabled = !disable_compression && dcc_enabled,
      .tc_compat_htile_enabled = tc_compat_htile_enabled,
   };

   ac_set_mutable_tex_desc_fields(&pdev->info, &ac_state, state);
}

/*
 * GFX10+ Specific Descriptor Build
 * Marked noinline to keep it out of the GFX9 hot path I-Cache if possible
 */
static void __attribute__((noinline))
gfx10_make_texture_descriptor(struct radv_device *device, struct radv_image *image, bool is_storage_image,
                              VkImageViewType view_type, VkFormat vk_format, const VkComponentMapping *mapping,
                              unsigned first_level, unsigned last_level, unsigned first_layer, unsigned last_layer,
                              unsigned width, unsigned height, unsigned depth, float min_lod, uint32_t *state,
                              uint32_t *fmask_state, const struct ac_surf_nbc_view *nbc_view,
                              const VkImageViewSlicedCreateInfoEXT *sliced_3d)
{
   const struct radv_physical_device *pdev = radv_device_physical(device);
   const bool create_2d_view_of_3d =
      (image->vk.create_flags & VK_IMAGE_CREATE_2D_VIEW_COMPATIBLE_BIT_EXT) &&
      view_type == VK_IMAGE_VIEW_TYPE_2D;
   enum pipe_format format = radv_normalize_texture_format(image, vk_format);
   const enum pipe_format img_format = radv_format_to_pipe_format(image->vk.format);
   const struct util_format_description *desc = util_format_description(format);
   enum pipe_swizzle swizzle[4];
   unsigned array_pitch = 0;
   unsigned type;
   const bool dcc_enabled = radv_dcc_enabled(image, first_level);
   const bool tc_compat_htile_enabled = radv_tc_compat_htile_enabled(image, first_level);
   const bool has_fmask = fmask_state != NULL && radv_image_has_fmask(image);

   radv_compose_swizzle(desc, mapping, swizzle);

   if (create_2d_view_of_3d) {
      type = V_008F1C_SQ_RSRC_IMG_3D;
   } else {
      type = radv_tex_dim(image->vk.image_type, view_type, image->vk.array_layers, image->vk.samples,
                          is_storage_image, false);
   }

   if (type == V_008F1C_SQ_RSRC_IMG_1D_ARRAY) {
      height = 1;
      depth = image->vk.array_layers;
   } else if (type == V_008F1C_SQ_RSRC_IMG_2D_ARRAY || type == V_008F1C_SQ_RSRC_IMG_2D_MSAA_ARRAY) {
      if (view_type != VK_IMAGE_VIEW_TYPE_3D)
         depth = image->vk.array_layers;
   } else if (type == V_008F1C_SQ_RSRC_IMG_CUBE) {
      depth = image->vk.array_layers / 6;
   }

   if (create_2d_view_of_3d) {
      depth = !is_storage_image ? depth : u_minify(depth, first_level);
      array_pitch = is_storage_image ? 1u : 0u;
   } else if (sliced_3d) {
      const unsigned total = u_minify(depth, first_level);
      const unsigned slice_count = sliced_3d->sliceCount == VK_REMAINING_3D_SLICES_EXT
                                      ? MAX2(1, total - sliced_3d->sliceOffset)
                                      : sliced_3d->sliceCount;
      first_layer = sliced_3d->sliceOffset;
      depth = sliced_3d->sliceOffset + slice_count;
      array_pitch = 1;
   }

   const struct ac_texture_state tex_state = {
      .surf = &image->planes[0].surface,
      .format = format,
      .img_format = img_format,
      .width = width,
      .height = height,
      .depth = type == V_008F1C_SQ_RSRC_IMG_3D ? depth - 1 : last_layer,
      .type = type,
      .swizzle = {swizzle[0], swizzle[1], swizzle[2], swizzle[3]},
      .num_samples = image->vk.samples,
      .num_storage_samples = image->vk.samples,
      .first_level = first_level,
      .last_level = last_level,
      .num_levels = image->vk.mip_levels,
      .first_layer = first_layer,
      .last_layer = last_layer,
      .min_lod = min_lod,
      .gfx10 = {.nbc_view = nbc_view, .uav3d = array_pitch},
      .dcc_enabled = dcc_enabled,
      .tc_compat_htile_enabled = tc_compat_htile_enabled,
   };

   ac_build_texture_descriptor(&pdev->info, &tex_state, &state[0]);

   if (fmask_state) {
      if (has_fmask) {
         const struct ac_fmask_state ac_state = {
            .surf = &image->planes[0].surface,
            .va = image->bindings[0].addr,
            .width = width,
            .height = height,
            .depth = depth,
            .type = radv_tex_dim(image->vk.image_type, view_type, image->vk.array_layers, 0, false, false),
            .first_layer = first_layer,
            .last_layer = last_layer,
            .num_samples = image->vk.samples,
            .num_storage_samples = image->vk.samples,
            .tc_compat_cmask = radv_image_is_tc_compat_cmask(image),
         };

         ac_build_fmask_descriptor(pdev->info.gfx_level, &ac_state, &fmask_state[0]);
      } else {
         memset(fmask_state, 0, 8 * sizeof(fmask_state[0]));
      }
   }
}

/**
 * Build the sampler view descriptor for a texture (SI-GFX9)
 * Inline this for Vega/GFX9 performance.
 */
static ALWAYS_INLINE void
gfx6_make_texture_descriptor(struct radv_device *device, struct radv_image *image, bool is_storage_image,
                             VkImageViewType view_type, VkFormat vk_format, const VkComponentMapping *mapping,
                             unsigned first_level, unsigned last_level, unsigned first_layer, unsigned last_layer,
                             unsigned width, unsigned height, unsigned depth, float min_lod, uint32_t *state,
                             uint32_t *fmask_state)
{
   const struct radv_physical_device *pdev = radv_device_physical(device);
   const struct radv_instance *instance = radv_physical_device_instance(pdev);
   const bool is_gfx9 = pdev->info.gfx_level == GFX9;
   const bool create_2d_view_of_3d =
      (image->vk.create_flags & VK_IMAGE_CREATE_2D_VIEW_COMPATIBLE_BIT_EXT) &&
      view_type == VK_IMAGE_VIEW_TYPE_2D;
   enum pipe_format format = radv_normalize_texture_format(image, vk_format);
   const enum pipe_format img_format = radv_format_to_pipe_format(image->vk.format);
   const struct util_format_description *desc = util_format_description(format);
   enum pipe_swizzle swizzle[4];
   unsigned type;
   const bool dcc_enabled = radv_dcc_enabled(image, first_level);
   const bool tc_compat_htile_enabled = radv_tc_compat_htile_enabled(image, first_level);
   const bool has_fmask = fmask_state != NULL && radv_image_has_fmask(image);

   radv_compose_swizzle(desc, mapping, swizzle);

   if (likely(is_gfx9) && create_2d_view_of_3d) {
      type = V_008F1C_SQ_RSRC_IMG_3D;
   } else {
      type = radv_tex_dim(image->vk.image_type, view_type, image->vk.array_layers, image->vk.samples,
                          is_storage_image, is_gfx9);
   }

   if (type == V_008F1C_SQ_RSRC_IMG_1D_ARRAY) {
      height = 1;
      depth = image->vk.array_layers;
   } else if (type == V_008F1C_SQ_RSRC_IMG_2D_ARRAY || type == V_008F1C_SQ_RSRC_IMG_2D_MSAA_ARRAY) {
      if (view_type != VK_IMAGE_VIEW_TYPE_3D)
         depth = image->vk.array_layers;
   } else if (type == V_008F1C_SQ_RSRC_IMG_CUBE) {
      depth = image->vk.array_layers / 6;
   }

   const struct ac_texture_state tex_state = {
      .surf = &image->planes[0].surface,
      .format = format,
      .img_format = img_format,
      .width = width,
      .height = height,
      .depth = depth,
      .type = type,
      .swizzle = {swizzle[0], swizzle[1], swizzle[2], swizzle[3]},
      .num_samples = image->vk.samples,
      .num_storage_samples = image->vk.samples,
      .first_level = first_level,
      .last_level = last_level,
      .num_levels = image->vk.mip_levels,
      .first_layer = first_layer,
      .last_layer = last_layer,
      .min_lod = min_lod,
      .dcc_enabled = dcc_enabled,
      .tc_compat_htile_enabled = tc_compat_htile_enabled,
      .aniso_single_level = !instance->drirc.debug.disable_aniso_single_level,
   };

   ac_build_texture_descriptor(&pdev->info, &tex_state, &state[0]);

   if (fmask_state) {
      if (has_fmask) {
         const struct ac_fmask_state ac_fmask_state = {
            .surf = &image->planes[0].surface,
            .va = image->bindings[0].addr,
            .width = width,
            .height = height,
            .depth = depth,
            .type = radv_tex_dim(image->vk.image_type, view_type, image->vk.array_layers, 0, false, false),
            .first_layer = first_layer,
            .last_layer = last_layer,
            .num_samples = image->vk.samples,
            .num_storage_samples = image->vk.samples,
            .tc_compat_cmask = radv_image_is_tc_compat_cmask(image),
         };

         ac_build_fmask_descriptor(pdev->info.gfx_level, &ac_fmask_state, &fmask_state[0]);
      } else {
         memset(fmask_state, 0, 8 * sizeof(fmask_state[0]));
      }
   }
}

/* Dispatcher - optimize for branch prediction */
void
radv_make_texture_descriptor(struct radv_device *device, struct radv_image *image, bool is_storage_image,
                             VkImageViewType view_type, VkFormat vk_format, const VkComponentMapping *mapping,
                             unsigned first_level, unsigned last_level, unsigned first_layer, unsigned last_layer,
                             unsigned width, unsigned height, unsigned depth, float min_lod, uint32_t *state,
                             uint32_t *fmask_state, const struct ac_surf_nbc_view *nbc_view,
                             const VkImageViewSlicedCreateInfoEXT *sliced_3d)
{
   const struct radv_physical_device *pdev = radv_device_physical(device);

   if (unlikely(pdev->info.gfx_level >= GFX10)) {
      gfx10_make_texture_descriptor(device, image, is_storage_image, view_type, vk_format, mapping, first_level,
                                    last_level, first_layer, last_layer, width, height, depth, min_lod, state,
                                    fmask_state, nbc_view, sliced_3d);
   } else {
      /* Hot path for GFX9 */
      gfx6_make_texture_descriptor(device, image, is_storage_image, view_type, vk_format, mapping, first_level,
                                   last_level, first_layer, last_layer, width, height, depth, min_lod, state,
                                   fmask_state);
   }
}

static inline void
compute_non_block_compressed_view(struct radv_device *device, const struct radv_image_view *iview,
                                  struct ac_surf_nbc_view *nbc_view)
{
   const struct radv_physical_device *pdev = radv_device_physical(device);
   const struct radv_image *image = iview->image;
   const struct radeon_surf *surf = &image->planes[0].surface;
   struct ac_surf_info surf_info = radv_get_ac_surf_info(device, image);

   ac_surface_compute_nbc_view(pdev->addrlib, &pdev->info, surf, &surf_info, iview->vk.base_mip_level,
                               iview->vk.base_array_layer, nbc_view);
}

static void
radv_image_view_make_descriptor(struct radv_image_view *iview, struct radv_device *device, VkFormat vk_format,
                                const VkComponentMapping *components, bool is_storage_image, bool disable_compression,
                                bool enable_compression, unsigned plane_id, unsigned descriptor_plane_id,
                                const VkImageViewSlicedCreateInfoEXT *sliced_3d)
{
   const struct radv_physical_device *pdev = radv_device_physical(device);
   struct radv_image *image = iview->image;
   const struct radv_image_plane *plane = &image->planes[plane_id];
   const bool is_gfx9_plus = pdev->info.gfx_level >= GFX9;
   const bool is_stencil = iview->vk.aspects == VK_IMAGE_ASPECT_STENCIL_BIT;
   const bool video_decode_dst = (image->vk.usage & VK_IMAGE_USAGE_VIDEO_DECODE_DST_BIT_KHR) != 0;
   unsigned first_layer = iview->vk.base_array_layer;
   uint32_t blk_w = plane->surface.blk_w;
   union radv_descriptor *const descriptor = is_storage_image ? &iview->storage_descriptor : &iview->descriptor;
   uint32_t *const tex_desc = descriptor->plane_descriptors[descriptor_plane_id];
   uint32_t *const fmask_desc = (!descriptor_plane_id && !is_storage_image) ? descriptor->fmask_descriptor : NULL;
   unsigned hw_level = iview->vk.base_mip_level;
   bool force_zero_base_mip = false;
   uint64_t offset = 0;
   VkExtent3D extent = {
      .width = iview->extent.width,
      .height = iview->extent.height,
      .depth = iview->extent.depth,
   };
   const unsigned plane_bw = vk_format_get_blockwidth(plane->format);
   const unsigned view_bw = vk_format_get_blockwidth(vk_format);

   if (unlikely(view_bw != plane_bw))
      blk_w = (blk_w / plane_bw) * view_bw;

   if (likely(is_gfx9_plus)) {
      if (unlikely(iview->nbc_view.valid)) {
         hw_level = iview->nbc_view.level;
         first_layer = 0;
      } else if (unlikely(video_decode_dst && plane->surface.u.gfx9.swizzle_mode == 0)) {
         offset = (uint64_t)first_layer * plane->surface.u.gfx9.surf_slice_size;
         first_layer = 0;
      }
   } else {
      if (is_storage_image || vk_format_is_block_compressed(plane->format) ||
          (iview->vk.aspects == VK_IMAGE_ASPECT_DEPTH_BIT &&
           iview->image->vk.aspects == (VK_IMAGE_ASPECT_DEPTH_BIT | VK_IMAGE_ASPECT_STENCIL_BIT)) ||
          (iview->image->vk.create_flags & VK_IMAGE_CREATE_2D_VIEW_COMPATIBLE_BIT_EXT &&
           iview->vk.view_type == VK_IMAGE_VIEW_TYPE_2D)) {
         force_zero_base_mip = true;
      }

      if (force_zero_base_mip) {
         hw_level = 0;
      } else {
         extent = image->vk.extent;
      }

      if (video_decode_dst) {
         offset = (uint64_t)first_layer * plane->surface.u.legacy.level[0].slice_size_dw * 4u;
         first_layer = 0;
      }
   }

   if (unlikely(vk_format_is_96bit(plane->format) && !vk_format_is_96bit(vk_format))) {
      extent.width *= 3u;
      blk_w *= 3u;
   }

   const unsigned last_level = hw_level + iview->vk.level_count - 1;
   const unsigned last_layer = iview->vk.base_array_layer + iview->vk.layer_count - 1;
   const unsigned plane_width = vk_format_get_plane_width(image->vk.format, plane_id, extent.width);
   const unsigned plane_height = vk_format_get_plane_height(image->vk.format, plane_id, extent.height);

   radv_make_texture_descriptor(device, image, is_storage_image, iview->vk.view_type, vk_format, components,
                                hw_level, last_level, first_layer, last_layer, plane_width, plane_height,
                                extent.depth, iview->vk.min_lod, tex_desc, fmask_desc, &iview->nbc_view, sliced_3d);

   const struct legacy_surf_level *base_level_info = NULL;
   if (unlikely(!is_gfx9_plus)) {
      const unsigned idx = force_zero_base_mip ? iview->vk.base_mip_level : 0;
      base_level_info = is_stencil ? &plane->surface.u.legacy.zs.stencil_level[idx]
                                   : &plane->surface.u.legacy.level[idx];
   }

   bool enable_write_compression = false;
   bool decompress_htile_on_image_stores = false;

   if (is_storage_image) {
      enable_write_compression = radv_image_compress_dcc_on_image_stores(device, image);
      decompress_htile_on_image_stores = radv_image_decompress_htile_on_image_stores(device, image);

      if (!(enable_write_compression || enable_compression || decompress_htile_on_image_stores))
         disable_compression = true;
   }

   radv_set_mutable_tex_desc_fields(device, image, base_level_info, plane_id,
                                    force_zero_base_mip ? iview->vk.base_mip_level : 0,
                                    iview->vk.base_mip_level, blk_w, is_stencil, is_storage_image,
                                    disable_compression, enable_write_compression, tex_desc, &iview->nbc_view, offset);
}

static bool
radv_image_view_can_fast_clear(const struct radv_device *device, const struct radv_image_view *iview)
{
   struct radv_image *image;
   if (!iview) return false;
   image = iview->image;

   /* Fast path checks */
   if (!radv_image_can_fast_clear(device, image)) return false;

   const bool all_slices = iview->vk.base_array_layer == 0 && iview->vk.layer_count == image->vk.array_layers;
   if (!all_slices && !image->support_comp_to_single) return false;

   if (!radv_image_extent_compare(image, &iview->extent)) return false;

   return true;
}

static uint32_t
radv_surface_max_layer_count(struct radv_image_view *iview)
{
   return iview->vk.view_type == VK_IMAGE_VIEW_TYPE_3D ? iview->extent.depth
                                                       : (iview->vk.base_array_layer + iview->vk.layer_count);
}

static void
radv_initialise_color_surface(struct radv_device *device, struct radv_color_buffer_info *cb,
                              struct radv_image_view *iview)
{
   const struct radv_physical_device *pdev = radv_device_physical(device);
   const struct radv_instance *instance = radv_physical_device_instance(pdev);
   const struct radv_image *image = iview->image;
   const struct radv_image_plane *plane = &image->planes[iview->plane_id];
   const struct radeon_surf *surf = &plane->surface;
   struct ac_surf_nbc_view *nbc_view = iview->nbc_view.valid ? &iview->nbc_view : NULL;
   const unsigned num_layers =
      image->vk.image_type == VK_IMAGE_TYPE_3D ? (iview->extent.depth - 1) : (image->vk.array_layers - 1);
   const uint32_t bind_plane_id = image->disjoint ? iview->plane_id : 0;
   const uint64_t va = image->bindings[bind_plane_id].addr;
   const bool has_fmask = radv_image_has_fmask(image);
   const bool has_cmask = radv_image_has_cmask(image);
   const bool tc_compat_cmask_enabled = has_cmask && radv_image_is_tc_compat_cmask(image);
   const bool dcc_enabled = radv_dcc_enabled(image, iview->vk.base_mip_level) &&
                            (pdev->info.gfx_level >= GFX11 || !iview->disable_dcc_mrt);
   const bool fast_clear_enabled = !(instance->debug_flags & RADV_DEBUG_NO_FAST_CLEARS);

   memset(cb, 0, sizeof(*cb));

   const struct ac_cb_state cb_state = {
      .surf = surf,
      .format = radv_format_to_pipe_format(iview->vk.format),
      .width = vk_format_get_plane_width(image->vk.format, iview->plane_id, iview->extent.width),
      .height = vk_format_get_plane_height(image->vk.format, iview->plane_id, iview->extent.height),
      .first_layer = iview->vk.base_array_layer,
      .last_layer = radv_surface_max_layer_count(iview) - 1,
      .num_layers = num_layers,
      .num_samples = image->vk.samples,
      .num_storage_samples = image->vk.samples,
      .base_level = iview->vk.base_mip_level,
      .num_levels = image->vk.mip_levels,
      .gfx10 = {.nbc_view = nbc_view},
   };

   ac_init_cb_surface(&pdev->info, &cb_state, &cb->ac);

   const struct ac_mutable_cb_state mutable_cb_state = {
      .surf = surf,
      .cb = &cb->ac,
      .va = va,
      .base_level = iview->vk.base_mip_level,
      .num_samples = image->vk.samples,
      .fmask_enabled = has_fmask,
      .cmask_enabled = has_cmask,
      .fast_clear_enabled = fast_clear_enabled,
      .tc_compat_cmask_enabled = tc_compat_cmask_enabled,
      .dcc_enabled = dcc_enabled,
      .gfx10 = {.nbc_view = nbc_view},
   };

   ac_set_mutable_cb_surface_fields(&pdev->info, &mutable_cb_state, &cb->ac);
}

static void
radv_initialise_ds_surface(const struct radv_device *device, struct radv_ds_buffer_info *ds,
                           struct radv_image_view *iview, VkImageAspectFlags ds_aspects, bool depth_compress_disable,
                           bool stencil_compress_disable)
{
   const struct radv_physical_device *pdev = radv_device_physical(device);
   const struct radv_image *image = iview->image;
   const struct radeon_surf *surf = &image->planes[0].surface;
   const unsigned level = iview->vk.base_mip_level;
   const uint32_t max_slice = radv_surface_max_layer_count(iview) - 1;
   const enum pipe_format format = radv_format_to_pipe_format(image->vk.format);
   const bool stencil_only = image->vk.format == VK_FORMAT_S8_UINT;
   const bool htile_enabled = radv_htile_enabled(image, level);
   const bool htile_stencil_disabled = htile_enabled && radv_image_tile_stencil_disabled(device, image);
   const bool vrs_enabled = htile_enabled && radv_image_has_vrs_htile(device, image);
   const bool tc_compat_htile_enabled = htile_enabled && radv_tc_compat_htile_enabled(image, level);

   memset(ds, 0, sizeof(*ds));

   ds->db_render_override2 = S_028010_DECOMPRESS_Z_ON_FLUSH(image->vk.samples >= 4) |
                             S_028010_CENTROID_COMPUTATION_MODE(pdev->info.gfx_level >= GFX10_3);

   const struct ac_ds_state ds_state = {
      .surf = surf,
      .va = image->bindings[0].addr,
      .format = format,
      .width = image->vk.extent.width,
      .height = image->vk.extent.height,
      .level = level,
      .num_levels = image->vk.mip_levels,
      .num_samples = image->vk.samples,
      .first_layer = iview->vk.base_array_layer,
      .last_layer = max_slice,
      .stencil_only = stencil_only,
      .z_read_only = !(ds_aspects & VK_IMAGE_ASPECT_DEPTH_BIT),
      .stencil_read_only = !(ds_aspects & VK_IMAGE_ASPECT_STENCIL_BIT),
      .htile_enabled = htile_enabled,
      .htile_stencil_disabled = htile_stencil_disabled,
      .vrs_enabled = vrs_enabled,
   };

   ac_init_ds_surface(&pdev->info, &ds_state, &ds->ac);

   const struct ac_mutable_ds_state mutable_ds_state = {
      .ds = &ds->ac,
      .format = format,
      .tc_compat_htile_enabled = tc_compat_htile_enabled,
      .zrange_precision = true,
      .no_d16_compression = true,
   };

   ac_set_mutable_ds_surface_fields(&pdev->info, &mutable_ds_state, &ds->ac);

   if (unlikely(pdev->info.gfx_level >= GFX11))
      radv_gfx11_set_db_render_control(device, image->vk.samples, &ds->db_render_control);

   ds->db_render_control |= S_028000_DEPTH_COMPRESS_DISABLE(depth_compress_disable) |
                            S_028000_STENCIL_COMPRESS_DISABLE(stencil_compress_disable);
}

void
radv_initialise_vrs_surface(struct radv_image *image, struct radv_buffer *htile_buffer, struct radv_ds_buffer_info *ds)
{
   const struct radeon_surf *surf = &image->planes[0].surface;
   memset(ds, 0, sizeof(*ds));

   ds->ac.db_z_info = S_028038_FORMAT(V_028040_Z_16) | S_028038_SW_MODE(surf->u.gfx9.swizzle_mode) |
                      S_028038_ZRANGE_PRECISION(1) | S_028038_TILE_SURFACE_ENABLE(1);
   ds->ac.db_stencil_info = S_02803C_FORMAT(V_028044_STENCIL_INVALID);
   ds->ac.db_depth_size = S_02801C_X_MAX(image->vk.extent.width - 1) | S_02801C_Y_MAX(image->vk.extent.height - 1);
   ds->ac.u.gfx6.db_htile_data_base = radv_buffer_get_va(htile_buffer->bo) >> 8;
   ds->ac.u.gfx6.db_htile_surface =
      S_028ABC_FULL_CACHE(1) | S_028ABC_PIPE_ALIGNED(1) | S_028ABC_VRS_HTILE_ENCODING(V_028ABC_VRS_HTILE_4BIT_ENCODING);
}

void
radv_image_view_init(struct radv_image_view *iview, struct radv_device *device,
                     const VkImageViewCreateInfo *pCreateInfo,
                     const struct radv_image_view_extra_create_info *extra_create_info)
{
   VK_FROM_HANDLE(radv_image, image, pCreateInfo->image);
   const struct radv_physical_device *pdev = radv_device_physical(device);
   const bool gfx9_plus = pdev->info.gfx_level >= GFX9;
   const uint32_t image_plane_count = vk_format_get_plane_count(image->vk.format);
   uint32_t plane_count = 1;

   const struct VkImageViewSlicedCreateInfoEXT *sliced_3d =
      vk_find_struct_const(pCreateInfo->pNext, IMAGE_VIEW_SLICED_CREATE_INFO_EXT);

   if (!extra_create_info || !extra_create_info->from_client)
      assert(pCreateInfo->flags & VK_IMAGE_VIEW_CREATE_DRIVER_INTERNAL_BIT_MESA);

   memset(iview, 0, sizeof(*iview));
   vk_image_view_init(&device->vk, &iview->vk, pCreateInfo);

   iview->image = image;
   iview->plane_id = radv_plane_from_aspect(pCreateInfo->subresourceRange.aspectMask);
   iview->nbc_view.valid = false;

   if (iview->vk.format == VK_FORMAT_UNDEFINED) {
      iview->vk.format = image->vk.format;
      iview->vk.view_format = image->vk.format;
   }

   if (iview->vk.aspects == VK_IMAGE_ASPECT_STENCIL_BIT) {
      if (vk_format_has_stencil(iview->vk.view_format))
         iview->vk.view_format = vk_format_stencil_only(iview->vk.view_format);
   } else if (iview->vk.aspects == VK_IMAGE_ASPECT_DEPTH_BIT) {
      if (vk_format_has_depth(iview->vk.view_format))
         iview->vk.view_format = vk_format_depth_only(iview->vk.view_format);
   }

   if (image_plane_count > 1 && pCreateInfo->subresourceRange.aspectMask == VK_IMAGE_ASPECT_COLOR_BIT)
      plane_count = vk_format_get_plane_count(iview->vk.format);

   if (unlikely(radv_is_format_emulated(pdev, iview->vk.format))) {
      iview->plane_id = 1;
      iview->vk.view_format = image->planes[iview->plane_id].format;
      iview->vk.format = image->planes[iview->plane_id].format;
      plane_count = 1;
   }

   if (likely(gfx9_plus)) {
      iview->extent = (VkExtent3D){
         .width = image->vk.extent.width,
         .height = image->vk.extent.height,
         .depth = image->vk.extent.depth,
      };
   } else {
      iview->extent = vk_image_mip_level_extent(&image->vk, iview->vk.base_mip_level);
   }

   if (iview->vk.format != image->planes[iview->plane_id].format) {
      const struct radv_image_plane *plane = &image->planes[iview->plane_id];
      const unsigned view_bw = vk_format_get_blockwidth(iview->vk.format);
      const unsigned view_bh = vk_format_get_blockheight(iview->vk.format);
      const unsigned plane_bw = vk_format_get_blockwidth(plane->format);
      const unsigned plane_bh = vk_format_get_blockheight(plane->format);
      const bool plane_is_bc = vk_format_is_block_compressed(plane->format);
      const bool view_is_bc = vk_format_is_block_compressed(iview->vk.format);

      iview->extent.width = DIV_ROUND_UP(iview->extent.width * view_bw, plane_bw);
      iview->extent.height = DIV_ROUND_UP(iview->extent.height * view_bh, plane_bh);

      if (likely(gfx9_plus) && plane_is_bc && !view_is_bc) {
         if (iview->vk.level_count > 1) {
            iview->extent.width = plane->surface.u.gfx9.base_mip_width;
            iview->extent.height = plane->surface.u.gfx9.base_mip_height;
         } else {
            unsigned lvl_width = u_minify(image->vk.extent.width, iview->vk.base_mip_level);
            unsigned lvl_height = u_minify(image->vk.extent.height, iview->vk.base_mip_level);

            lvl_width = DIV_ROUND_UP(lvl_width * view_bw, plane_bw);
            lvl_height = DIV_ROUND_UP(lvl_height * view_bh, plane_bh);

            iview->extent.width = CLAMP(lvl_width << iview->vk.base_mip_level, iview->extent.width,
                                        plane->surface.u.gfx9.base_mip_width);
            iview->extent.height = CLAMP(lvl_height << iview->vk.base_mip_level, iview->extent.height,
                                         plane->surface.u.gfx9.base_mip_height);

            if (pdev->info.gfx_level >= GFX10 &&
                (u_minify(iview->extent.width, iview->vk.base_mip_level) < lvl_width ||
                 u_minify(iview->extent.height, iview->vk.base_mip_level) < lvl_height) &&
                iview->vk.layer_count == 1) {
               compute_non_block_compressed_view(device, iview, &iview->nbc_view);
               if (iview->nbc_view.valid) {
                  iview->extent.width = iview->nbc_view.width;
                  iview->extent.height = iview->nbc_view.height;
               }
            }
         }
      }
   }

   iview->support_fast_clear = radv_image_view_can_fast_clear(device, iview);
   iview->disable_dcc_mrt = extra_create_info ? extra_create_info->disable_dcc_mrt : false;
   iview->disable_tc_compat_cmask_mrt = extra_create_info ? extra_create_info->disable_tc_compat_cmask_mrt : false;

   const bool depth_compress_disable = extra_create_info ? extra_create_info->depth_compress_disable : false;
   const bool stencil_compress_disable = extra_create_info ? extra_create_info->stencil_compress_disable : false;
   bool disable_compression = extra_create_info ? extra_create_info->disable_compression : false;
   bool enable_compression = extra_create_info ? extra_create_info->enable_compression : false;

   const VkImageUsageFlags usage = iview->vk.usage;
   const bool needs_sampled = (usage & (VK_IMAGE_USAGE_SAMPLED_BIT | VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT)) != 0;
   const bool needs_storage = (usage & VK_IMAGE_USAGE_STORAGE_BIT) != 0;
   const bool needs_descriptors = needs_sampled || needs_storage;
   const bool needs_color_attachment = (usage & VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) != 0;
   const bool needs_ds_attachment = (usage & VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT) != 0;
   const bool image_has_depth = vk_format_has_depth(image->vk.format);
   const bool image_has_stencil = vk_format_has_stencil(image->vk.format);
   const VkComponentMapping *const components = &pCreateInfo->components;

   if (needs_descriptors) {
      if (likely(plane_count == 1)) {
         const VkFormat format = vk_format_get_plane_format(iview->vk.view_format, 0);

         if (needs_sampled) {
            radv_image_view_make_descriptor(iview, device, format, components, false, disable_compression,
                                            enable_compression, iview->plane_id, 0, NULL);
         }
         if (needs_storage) {
            radv_image_view_make_descriptor(iview, device, format, components, true, disable_compression,
                                            enable_compression, iview->plane_id, 0, sliced_3d);
         }
      } else {
         for (uint32_t i = 0; i < plane_count; ++i) {
            const VkFormat format = vk_format_get_plane_format(iview->vk.view_format, i);

            if (needs_sampled) {
               radv_image_view_make_descriptor(iview, device, format, components, false, disable_compression,
                                               enable_compression, iview->plane_id + i, i, NULL);
            }
            if (needs_storage) {
               radv_image_view_make_descriptor(iview, device, format, components, true, disable_compression,
                                               enable_compression, iview->plane_id + i, i, sliced_3d);
            }
         }
      }
   }

   if (needs_color_attachment)
      radv_initialise_color_surface(device, &iview->color_desc, iview);

   if (needs_ds_attachment) {
      if (image_has_depth && image_has_stencil) {
         radv_initialise_ds_surface(device, &iview->depth_stencil_desc, iview,
                                    VK_IMAGE_ASPECT_DEPTH_BIT | VK_IMAGE_ASPECT_STENCIL_BIT,
                                    depth_compress_disable, stencil_compress_disable);
      }
      if (image_has_depth) {
         radv_initialise_ds_surface(device, &iview->depth_only_desc, iview, VK_IMAGE_ASPECT_DEPTH_BIT,
                                    depth_compress_disable, stencil_compress_disable);
      }
      if (image_has_stencil) {
         radv_initialise_ds_surface(device, &iview->stencil_only_desc, iview, VK_IMAGE_ASPECT_STENCIL_BIT,
                                    depth_compress_disable, stencil_compress_disable);
      }
   }
}

void
radv_hiz_image_view_init(struct radv_image_view *iview, struct radv_device *device,
                         const VkImageViewCreateInfo *pCreateInfo)
{
   VK_FROM_HANDLE(radv_image, image, pCreateInfo->image);
   memset(iview, 0, sizeof(*iview));
   vk_image_view_init(&device->vk, &iview->vk, pCreateInfo);

   assert(iview->vk.aspects == VK_IMAGE_ASPECT_DEPTH_BIT);

   iview->image = image;

   const uint32_t type =
      radv_tex_dim(image->vk.image_type, iview->vk.view_type, image->vk.array_layers, image->vk.samples, true, false);

   const struct ac_gfx12_hiz_state hiz_state = {
      .surf = &image->planes[0].surface,
      .va = image->bindings[0].addr,
      .type = type,
      .num_samples = image->vk.samples,
      .first_level = iview->vk.base_mip_level,
      .last_level = iview->vk.base_mip_level + iview->vk.level_count - 1,
      .num_levels = image->vk.mip_levels,
      .first_layer = iview->vk.base_array_layer,
      .last_layer = iview->vk.base_array_layer + iview->vk.layer_count - 1,
   };

   ac_build_gfx12_hiz_descriptor(&hiz_state, iview->storage_descriptor.plane_descriptors[0]);
}

void
radv_image_view_finish(struct radv_image_view *iview)
{
   vk_image_view_finish(&iview->vk);
}

VKAPI_ATTR VkResult VKAPI_CALL
radv_CreateImageView(VkDevice _device, const VkImageViewCreateInfo *pCreateInfo,
                     const VkAllocationCallbacks *pAllocator, VkImageView *pView)
{
   VK_FROM_HANDLE(radv_device, device, _device);
   struct radv_image_view *view;

   view = vk_alloc2(&device->vk.alloc, pAllocator, sizeof(*view), 8, VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
   if (unlikely(view == NULL))
      return vk_error(device, VK_ERROR_OUT_OF_HOST_MEMORY);

   radv_image_view_init(view, device, pCreateInfo, &(struct radv_image_view_extra_create_info){.from_client = true});

   *pView = radv_image_view_to_handle(view);

   return VK_SUCCESS;
}

VKAPI_ATTR void VKAPI_CALL
radv_DestroyImageView(VkDevice _device, VkImageView _iview, const VkAllocationCallbacks *pAllocator)
{
   VK_FROM_HANDLE(radv_device, device, _device);
   VK_FROM_HANDLE(radv_image_view, iview, _iview);

   if (!iview) return;

   radv_image_view_finish(iview);
   vk_free2(&device->vk.alloc, pAllocator, iview);
}
