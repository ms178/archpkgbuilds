/*
 * Copyright © 2016 Red Hat.
 * Copyright © 2016 Bas Nieuwenhuizen
 *
 * based in part on anv driver which is:
 * Copyright © 2015 Intel Corporation
 *
 * SPDX-License-Identifier: MIT
 */

#ifndef RADV_IMAGE_VIEW_H
#define RADV_IMAGE_VIEW_H

#include "ac_surface.h"
#include "radv_image.h"
#include "util/macros.h"  /* For ALWAYS_INLINE */
#include <stdbool.h>

/* Compile-time assertions */
#if defined(__cplusplus)
#  define RADV_CT_ASSERT(cond, msg) static_assert(cond, msg)
#else
#  define RADV_CT_ASSERT(cond, msg) _Static_assert(cond, msg)
#endif

/* Branch prediction hints - only define if not already defined */
#ifndef likely
#  ifdef __GNUC__
#    define likely(x)   __builtin_expect(!!(x), 1)
#    define unlikely(x) __builtin_expect(!!(x), 0)
#  else
#    define likely(x)   (x)
#    define unlikely(x) (x)
#  endif
#endif

RADV_CT_ASSERT(sizeof(uint32_t) == 4, "uint32_t must be 4 bytes");
RADV_CT_ASSERT(sizeof(uint64_t) == 8, "uint64_t must be 8 bytes");

/*
 * GFX9/Vega Optimization:
 * The Image Resource Descriptor (T#) is 8 dwords (32 bytes).
 * The Sampler Descriptor (S#) is 4 dwords (16 bytes).
 * Vega's SQ (Sequencer) L1 cache lines are 64 bytes.
 * We align the union to 64 bytes to ensure that fetching a descriptor
 * never straddles two cache lines, minimizing memory latency.
 */
union radv_descriptor {
   struct {
      uint32_t plane0_descriptor[8];
      uint32_t fmask_descriptor[8];
   };
   struct {
      uint32_t plane_descriptors[3][8];
   };
};

RADV_CT_ASSERT(sizeof(union radv_descriptor) == 96, "radv_descriptor size must be 96 bytes");

struct radv_color_buffer_info {
   struct ac_cb_surface ac;
};

struct radv_ds_buffer_info {
   struct ac_ds_surface ac;

   uint32_t db_render_override2;
   uint32_t db_render_control;
};

struct radv_image_view {
   struct vk_image_view vk;
   struct radv_image *image; /**< VkImageViewCreateInfo::image */

   /* Hot fields accessed during vkCmdBindDescriptorSets */
   unsigned plane_id;

   /* Cache-aligned Descriptors */
   /* Align to 64B to keep plane0 + fmask in a single cache line for Vega */
   _Alignas(64) union radv_descriptor descriptor;

   /* Descriptor for use as a storage image (UAV). */
   _Alignas(64) union radv_descriptor storage_descriptor;

   /* Less frequently accessed fields moved to the end to pack hot data */
   VkExtent3D extent; /**< Extent of VkImageViewCreateInfo::baseMipLevel. */

   bool support_fast_clear;
   bool disable_dcc_mrt;
   bool disable_tc_compat_cmask_mrt;

   /* Block-compressed image views on GFX10+. GFX9 doesn't use this directly but struct layout is shared. */
   struct ac_surf_nbc_view nbc_view;

   union {
      struct radv_color_buffer_info color_desc;
      struct {
         struct radv_ds_buffer_info depth_stencil_desc;
         struct radv_ds_buffer_info depth_only_desc;
         struct radv_ds_buffer_info stencil_only_desc;
      };
   };
};

VK_DEFINE_NONDISP_HANDLE_CASTS(radv_image_view, vk.base, VkImageView, VK_OBJECT_TYPE_IMAGE_VIEW);

struct radv_image_view_extra_create_info {
   bool disable_compression;
   bool enable_compression;
   bool disable_dcc_mrt;
   bool disable_tc_compat_cmask_mrt;
   bool depth_compress_disable;
   bool stencil_compress_disable;
   bool from_client; /**< Set only if this came from vkCreateImage */
};

void radv_image_view_init(struct radv_image_view *view, struct radv_device *device,
                          const VkImageViewCreateInfo *pCreateInfo,
                          const struct radv_image_view_extra_create_info *extra_create_info);
void radv_image_view_finish(struct radv_image_view *iview);

void radv_hiz_image_view_init(struct radv_image_view *iview, struct radv_device *device,
                              const VkImageViewCreateInfo *pCreateInfo);

void radv_set_mutable_tex_desc_fields(struct radv_device *device, struct radv_image *image,
                                      const struct legacy_surf_level *base_level_info, unsigned plane_id,
                                      unsigned base_level, unsigned first_level, unsigned block_width, bool is_stencil,
                                      bool is_storage_image, bool disable_compression, bool enable_write_compression,
                                      uint32_t *state, const struct ac_surf_nbc_view *nbc_view, uint64_t offset);

void radv_make_texture_descriptor(struct radv_device *device, struct radv_image *image, bool is_storage_image,
                                  VkImageViewType view_type, VkFormat vk_format, const VkComponentMapping *mapping,
                                  unsigned first_level, unsigned last_level, unsigned first_layer, unsigned last_layer,
                                  unsigned width, unsigned height, unsigned depth, float min_lod, uint32_t *state,
                                  uint32_t *fmask_state, const struct ac_surf_nbc_view *nbc_view,
                                  const VkImageViewSlicedCreateInfoEXT *sliced_3d);

void radv_initialise_vrs_surface(struct radv_image *image, struct radv_buffer *htile_buffer,
                                 struct radv_ds_buffer_info *ds);

#endif /* RADV_IMAGE_VIEW_H */
