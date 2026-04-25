/*
 * Copyright © 2016 Red Hat.
 * Copyright © 2016 Bas Nieuwenhuizen
 *
 * SPDX-License-Identifier: MIT
 */

#include "radv_descriptor_set.h"
#include "radv_cmd_buffer.h"
#include "radv_descriptor_pool.h"
#include "radv_descriptors.h"
#include "radv_entrypoints.h"
#include "radv_sampler.h"
#include "vk_descriptors.h"

static inline void
radv_build_layout_binding_meta(const VkDescriptorSetLayoutCreateInfo *create_info,
                               const VkDescriptorSetLayoutBindingFlagsCreateInfo *flags_info,
                               const VkMutableDescriptorTypeCreateInfoEXT *mutable_info,
                               VkDescriptorBindingFlags *binding_flags_by_binding,
                               const VkMutableDescriptorTypeListEXT **mutable_list_by_binding,
                               uint32_t num_bindings)
{
   for (uint32_t i = 0; i < num_bindings; i++) {
      binding_flags_by_binding[i] = 0;
      mutable_list_by_binding[i] = NULL;
   }

   for (uint32_t i = 0; i < create_info->bindingCount; i++) {
      const uint32_t b = create_info->pBindings[i].binding;

      if (flags_info && i < flags_info->bindingCount && flags_info->pBindingFlags)
         binding_flags_by_binding[b] = flags_info->pBindingFlags[i];

      if (mutable_info && i < mutable_info->mutableDescriptorTypeListCount &&
          mutable_info->pMutableDescriptorTypeLists)
         mutable_list_by_binding[b] = &mutable_info->pMutableDescriptorTypeLists[i];
   }
}

VKAPI_ATTR VkResult VKAPI_CALL
radv_CreateDescriptorSetLayout(VkDevice _device, const VkDescriptorSetLayoutCreateInfo *pCreateInfo,
                               const VkAllocationCallbacks *pAllocator, VkDescriptorSetLayout *pSetLayout)
{
   VK_FROM_HANDLE(radv_device, device, _device);
   const struct radv_physical_device *pdev = radv_device_physical(device);
   struct radv_descriptor_set_layout *set_layout;

   (void)pAllocator;
   assert(pCreateInfo->sType == VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO);

   const VkDescriptorSetLayoutBindingFlagsCreateInfo *variable_flags =
      vk_find_struct_const(pCreateInfo->pNext, DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO);
   const VkMutableDescriptorTypeCreateInfoEXT *mutable_info =
      vk_find_struct_const(pCreateInfo->pNext, MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_EXT);

   uint32_t num_bindings = 0;
   uint32_t immutable_sampler_count = 0;
   uint32_t ycbcr_sampler_count = 0;

   for (uint32_t j = 0; j < pCreateInfo->bindingCount; j++) {
      const VkDescriptorSetLayoutBinding *b = &pCreateInfo->pBindings[j];
      num_bindings = MAX2(num_bindings, b->binding + 1);

      if ((b->descriptorType == VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER ||
           b->descriptorType == VK_DESCRIPTOR_TYPE_SAMPLER) &&
          b->pImmutableSamplers) {
         immutable_sampler_count += b->descriptorCount;

         bool has_ycbcr_sampler = false;
         for (uint32_t i = 0; i < b->descriptorCount; i++) {
            struct radv_sampler *sampler = radv_sampler_from_handle(b->pImmutableSamplers[i]);
            if (sampler && sampler->vk.ycbcr_conversion) {
               has_ycbcr_sampler = true;
               break;
            }
         }

         if (has_ycbcr_sampler)
            ycbcr_sampler_count += b->descriptorCount;
      }
   }

   uint32_t samplers_offset = offsetof(struct radv_descriptor_set_layout, binding[num_bindings]);
   size_t size = samplers_offset + (size_t)immutable_sampler_count * 4u * sizeof(uint32_t);

   if (ycbcr_sampler_count > 0) {
      size += (size_t)num_bindings * sizeof(uint32_t);
      size = align_uintptr(size, alignof(struct vk_ycbcr_conversion_state));
      size += (size_t)ycbcr_sampler_count * sizeof(struct vk_ycbcr_conversion_state);
   }

   set_layout = vk_descriptor_set_layout_zalloc(&device->vk, size, pCreateInfo);
   if (!set_layout)
      return vk_error(device, VK_ERROR_OUT_OF_HOST_MEMORY);

   VkDescriptorSetLayoutBinding *bindings = NULL;
   VkResult result =
      vk_create_sorted_bindings(pCreateInfo->pBindings, pCreateInfo->bindingCount, &bindings, NULL, NULL);
   if (result != VK_SUCCESS) {
      vk_descriptor_set_layout_unref(&device->vk, &set_layout->vk);
      return vk_error(device, result);
   }

   VkDescriptorBindingFlags *binding_flags_by_binding = NULL;
   const VkMutableDescriptorTypeListEXT **mutable_list_by_binding = NULL;
   if (num_bindings) {
      binding_flags_by_binding = calloc(num_bindings, sizeof(*binding_flags_by_binding));
      mutable_list_by_binding = calloc(num_bindings, sizeof(*mutable_list_by_binding));
      if (!binding_flags_by_binding || !mutable_list_by_binding) {
         free(mutable_list_by_binding);
         free(binding_flags_by_binding);
         free(bindings);
         vk_descriptor_set_layout_unref(&device->vk, &set_layout->vk);
         return vk_error(device, VK_ERROR_OUT_OF_HOST_MEMORY);
      }

      radv_build_layout_binding_meta(pCreateInfo, variable_flags, mutable_info,
                                     binding_flags_by_binding, mutable_list_by_binding, num_bindings);
   }

   set_layout->flags = pCreateInfo->flags;
   set_layout->binding_count = num_bindings;
   set_layout->dynamic_shader_stages = 0;
   set_layout->has_immutable_samplers = false;
   set_layout->has_variable_descriptors = false;
   set_layout->size = 0;

   uint32_t *samplers = (uint32_t *)&set_layout->binding[num_bindings];
   struct vk_ycbcr_conversion_state *ycbcr_samplers = NULL;
   uint32_t *ycbcr_sampler_offsets = NULL;

   if (ycbcr_sampler_count > 0) {
      ycbcr_sampler_offsets = samplers + 4 * immutable_sampler_count;
      set_layout->ycbcr_sampler_offsets_offset = (uint32_t)((char *)ycbcr_sampler_offsets - (char *)set_layout);

      uintptr_t first_ycbcr_sampler_offset =
         (uintptr_t)ycbcr_sampler_offsets + (size_t)num_bindings * sizeof(uint32_t);
      first_ycbcr_sampler_offset =
         align_uintptr(first_ycbcr_sampler_offset, alignof(struct vk_ycbcr_conversion_state));
      ycbcr_samplers = (struct vk_ycbcr_conversion_state *)first_ycbcr_sampler_offset;
   } else {
      set_layout->ycbcr_sampler_offsets_offset = 0;
   }

   uint32_t dynamic_offset_count = 0;
   uint32_t first_alignment = 32;

   if (pCreateInfo->bindingCount > 0) {
      const VkDescriptorSetLayoutBinding *last = &bindings[pCreateInfo->bindingCount - 1];
      uint32_t last_alignment = radv_descriptor_alignment(last->descriptorType);

      if (last->descriptorType == VK_DESCRIPTOR_TYPE_MUTABLE_EXT) {
         uint64_t mutable_size = 0, mutable_align = 0;
         const VkMutableDescriptorTypeListEXT *list =
            (last->binding < num_bindings) ? mutable_list_by_binding[last->binding] : NULL;

         if (!list || !radv_mutable_descriptor_type_size_alignment(device, list, &mutable_size, &mutable_align)) {
            free(mutable_list_by_binding);
            free(binding_flags_by_binding);
            free(bindings);
            vk_descriptor_set_layout_unref(&device->vk, &set_layout->vk);
            return vk_error(device, VK_ERROR_UNKNOWN);
         }
         last_alignment = (uint32_t)mutable_align;
      }

      first_alignment = (last_alignment == 32) ? 16 : 32;
   }

   for (unsigned pass = 0; pass < 2; pass++) {
      for (uint32_t j = 0; j < pCreateInfo->bindingCount; j++) {
         const VkDescriptorSetLayoutBinding *binding = &bindings[j];
         const uint32_t b = binding->binding;
         uint32_t alignment = radv_descriptor_alignment(binding->descriptorType);
         uint32_t descriptor_count = binding->descriptorCount;
         uint32_t max_sampled_image_descriptors = 1;
         bool has_ycbcr_sampler = false;

         if (binding->descriptorType == VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER &&
             binding->pImmutableSamplers) {
            for (uint32_t i = 0; i < binding->descriptorCount; i++) {
               struct radv_sampler *sampler = radv_sampler_from_handle(binding->pImmutableSamplers[i]);
               struct vk_ycbcr_conversion *conversion = sampler ? sampler->vk.ycbcr_conversion : NULL;
               if (conversion) {
                  has_ycbcr_sampler = true;
                  max_sampled_image_descriptors =
                     MAX2(max_sampled_image_descriptors,
                          vk_format_get_plane_count(conversion->state.format));
               }
            }
         }

         switch (binding->descriptorType) {
         case VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC:
         case VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC:
            assert(!(pCreateInfo->flags & VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT));
            set_layout->binding[b].dynamic_offset_count = 1;
            set_layout->dynamic_shader_stages |= binding->stageFlags;
            if (binding->stageFlags & RADV_RT_STAGE_BITS)
               set_layout->dynamic_shader_stages |= VK_SHADER_STAGE_COMPUTE_BIT;
            set_layout->binding[b].size = 0;
            break;
         case VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER:
         case VK_DESCRIPTOR_TYPE_STORAGE_BUFFER:
         case VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER:
         case VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER:
            set_layout->binding[b].size = RADV_BUFFER_DESC_SIZE;
            break;
         case VK_DESCRIPTOR_TYPE_STORAGE_IMAGE:
            set_layout->binding[b].size = RADV_STORAGE_IMAGE_DESC_SIZE;
            break;
         case VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE:
         case VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT:
            set_layout->binding[b].size = radv_get_sampled_image_desc_size(pdev);
            break;
         case VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER:
            set_layout->binding[b].size =
               max_sampled_image_descriptors * radv_get_combined_image_sampler_desc_size(pdev);
            break;
         case VK_DESCRIPTOR_TYPE_SAMPLER:
            set_layout->binding[b].size = RADV_SAMPLER_DESC_SIZE;
            break;
         case VK_DESCRIPTOR_TYPE_MUTABLE_EXT: {
            uint64_t mutable_size = 0, mutable_align = 0;
            const VkMutableDescriptorTypeListEXT *list =
               (b < num_bindings) ? mutable_list_by_binding[b] : NULL;

            if (!list || !radv_mutable_descriptor_type_size_alignment(device, list, &mutable_size, &mutable_align)) {
               free(mutable_list_by_binding);
               free(binding_flags_by_binding);
               free(bindings);
               vk_descriptor_set_layout_unref(&device->vk, &set_layout->vk);
               return vk_error(device, VK_ERROR_UNKNOWN);
            }

            set_layout->binding[b].size = (uint32_t)mutable_size;
            alignment = (uint32_t)mutable_align;
            break;
         }
         case VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK:
            set_layout->binding[b].size = descriptor_count;
            descriptor_count = 1;
            break;
         case VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR:
            set_layout->binding[b].size = RADV_ACCEL_STRUCT_DESC_SIZE;
            break;
         default:
            break;
         }

         if ((pass == 0 && alignment != first_alignment) ||
             (pass == 1 && alignment == first_alignment))
            continue;

         set_layout->size = align(set_layout->size, alignment);
         set_layout->binding[b].type = binding->descriptorType;
         set_layout->binding[b].array_size = descriptor_count;
         set_layout->binding[b].offset = set_layout->size;
         set_layout->binding[b].dynamic_offset_offset = dynamic_offset_count;
         set_layout->binding[b].has_ycbcr_sampler = has_ycbcr_sampler;

         const VkDescriptorBindingFlags bflags =
            (b < num_bindings) ? binding_flags_by_binding[b] : 0;
         if (bflags & VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT) {
            assert(!binding->pImmutableSamplers);
            assert(binding->binding == num_bindings - 1);
            set_layout->has_variable_descriptors = true;
         }

         if ((binding->descriptorType == VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER ||
              binding->descriptorType == VK_DESCRIPTOR_TYPE_SAMPLER) &&
             binding->pImmutableSamplers) {
            set_layout->binding[b].immutable_samplers_offset = samplers_offset;
            set_layout->has_immutable_samplers = true;

            for (uint32_t i = 0; i < binding->descriptorCount; i++) {
               struct radv_sampler *sampler = radv_sampler_from_handle(binding->pImmutableSamplers[i]);
               memcpy(samplers + 4 * i, &sampler->state, RADV_SAMPLER_DESC_SIZE);
            }

            samplers += 4 * binding->descriptorCount;
            samplers_offset += 4 * sizeof(uint32_t) * binding->descriptorCount;

            if (has_ycbcr_sampler) {
               ycbcr_sampler_offsets[b] = (uint32_t)((const char *)ycbcr_samplers - (const char *)set_layout);
               for (uint32_t i = 0; i < binding->descriptorCount; i++) {
                  struct radv_sampler *sampler = radv_sampler_from_handle(binding->pImmutableSamplers[i]);
                  if (sampler && sampler->vk.ycbcr_conversion)
                     ycbcr_samplers[i] = sampler->vk.ycbcr_conversion->state;
                  else
                     ycbcr_samplers[i].format = VK_FORMAT_UNDEFINED;
               }
               ycbcr_samplers += binding->descriptorCount;
            }
         }

         set_layout->size += descriptor_count * set_layout->binding[b].size;
         dynamic_offset_count += descriptor_count * set_layout->binding[b].dynamic_offset_count;
      }
   }

   set_layout->dynamic_offset_count = dynamic_offset_count;

   const uint32_t hash_offset = offsetof(struct radv_descriptor_set_layout, hash) + sizeof(set_layout->hash);
   _mesa_blake3_compute((const char *)set_layout + hash_offset, size - hash_offset, set_layout->hash);

   free(mutable_list_by_binding);
   free(binding_flags_by_binding);
   free(bindings);

   *pSetLayout = radv_descriptor_set_layout_to_handle(set_layout);
   return VK_SUCCESS;
}

VKAPI_ATTR void VKAPI_CALL
radv_GetDescriptorSetLayoutSupport(VkDevice _device, const VkDescriptorSetLayoutCreateInfo *pCreateInfo,
                                   VkDescriptorSetLayoutSupport *pSupport)
{
   VK_FROM_HANDLE(radv_device, device, _device);
   const struct radv_physical_device *pdev = radv_device_physical(device);

   VkDescriptorSetLayoutBinding *bindings = NULL;
   VkResult result =
      vk_create_sorted_bindings(pCreateInfo->pBindings, pCreateInfo->bindingCount, &bindings, NULL, NULL);
   if (result != VK_SUCCESS) {
      pSupport->supported = false;
      return;
   }

   const VkDescriptorSetLayoutBindingFlagsCreateInfo *variable_flags =
      vk_find_struct_const(pCreateInfo->pNext, DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO);
   const VkMutableDescriptorTypeCreateInfoEXT *mutable_info =
      vk_find_struct_const(pCreateInfo->pNext, MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_EXT);

   VkDescriptorSetVariableDescriptorCountLayoutSupport *variable_count =
      vk_find_struct(pSupport->pNext, DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT);
   if (variable_count)
      variable_count->maxVariableDescriptorCount = 0;

   uint32_t num_bindings = 0;
   for (uint32_t i = 0; i < pCreateInfo->bindingCount; i++)
      num_bindings = MAX2(num_bindings, pCreateInfo->pBindings[i].binding + 1);

   VkDescriptorBindingFlags *binding_flags_by_binding = NULL;
   const VkMutableDescriptorTypeListEXT **mutable_list_by_binding = NULL;

   if (num_bindings) {
      binding_flags_by_binding = calloc(num_bindings, sizeof(*binding_flags_by_binding));
      mutable_list_by_binding = calloc(num_bindings, sizeof(*mutable_list_by_binding));
      if (!binding_flags_by_binding || !mutable_list_by_binding) {
         free(mutable_list_by_binding);
         free(binding_flags_by_binding);
         free(bindings);
         pSupport->supported = false;
         return;
      }

      radv_build_layout_binding_meta(pCreateInfo, variable_flags, mutable_info,
                                     binding_flags_by_binding, mutable_list_by_binding, num_bindings);
   }

   uint32_t first_alignment = 32;
   if (pCreateInfo->bindingCount > 0) {
      const VkDescriptorSetLayoutBinding *last = &bindings[pCreateInfo->bindingCount - 1];
      uint32_t last_alignment = radv_descriptor_alignment(last->descriptorType);

      if (last->descriptorType == VK_DESCRIPTOR_TYPE_MUTABLE_EXT) {
         uint64_t mutable_size = 0, mutable_align = 0;
         const VkMutableDescriptorTypeListEXT *list =
            (last->binding < num_bindings) ? mutable_list_by_binding[last->binding] : NULL;
         if (!list || !radv_mutable_descriptor_type_size_alignment(device, list, &mutable_size, &mutable_align)) {
            free(mutable_list_by_binding);
            free(binding_flags_by_binding);
            free(bindings);
            pSupport->supported = false;
            return;
         }
         last_alignment = (uint32_t)mutable_align;
      }

      first_alignment = (last_alignment == 32) ? 16 : 32;
   }

   bool supported = true;
   uint64_t size = 0;

   for (unsigned pass = 0; pass < 2; pass++) {
      for (uint32_t i = 0; i < pCreateInfo->bindingCount; i++) {
         const VkDescriptorSetLayoutBinding *binding = &bindings[i];

         uint64_t descriptor_size = 0;
         uint64_t descriptor_alignment = radv_descriptor_alignment(binding->descriptorType);
         uint32_t descriptor_count = binding->descriptorCount;

         switch (binding->descriptorType) {
         case VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC:
         case VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC:
            break;
         case VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER:
         case VK_DESCRIPTOR_TYPE_STORAGE_BUFFER:
         case VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER:
         case VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER:
            descriptor_size = RADV_BUFFER_DESC_SIZE;
            break;
         case VK_DESCRIPTOR_TYPE_STORAGE_IMAGE:
            descriptor_size = RADV_STORAGE_IMAGE_DESC_SIZE;
            break;
         case VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE:
         case VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT:
            descriptor_size = radv_get_sampled_image_desc_size(pdev);
            break;
         case VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER:
            descriptor_size = radv_get_combined_image_sampler_desc_size(pdev);
            break;
         case VK_DESCRIPTOR_TYPE_SAMPLER:
            descriptor_size = RADV_SAMPLER_DESC_SIZE;
            break;
         case VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK:
            descriptor_size = descriptor_count;
            descriptor_count = 1;
            break;
         case VK_DESCRIPTOR_TYPE_MUTABLE_EXT: {
            const VkMutableDescriptorTypeListEXT *list =
               (binding->binding < num_bindings) ? mutable_list_by_binding[binding->binding] : NULL;
            if (!list ||
                !radv_mutable_descriptor_type_size_alignment(device, list, &descriptor_size, &descriptor_alignment))
               supported = false;
            break;
         }
         case VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR:
            descriptor_size = RADV_ACCEL_STRUCT_DESC_SIZE;
            break;
         default:
            break;
         }

         if ((pass == 0 && descriptor_alignment != first_alignment) ||
             (pass == 1 && descriptor_alignment == first_alignment))
            continue;

         if (size && !align64(size, descriptor_alignment))
            supported = false;
         size = align64(size, descriptor_alignment);

         uint64_t max_count = INT32_MAX;
         if (binding->descriptorType == VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK)
            max_count = MAX_INLINE_UNIFORM_BLOCK_SIZE - size;
         else if (descriptor_size)
            max_count = (INT32_MAX - size) / descriptor_size;

         if (max_count < descriptor_count)
            supported = false;

         const VkDescriptorBindingFlags bflags =
            (binding->binding < num_bindings) ? binding_flags_by_binding[binding->binding] : 0;

         if ((bflags & VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT) && variable_count)
            variable_count->maxVariableDescriptorCount = MIN2(UINT32_MAX, max_count);

         size += descriptor_count * descriptor_size;
      }
   }

   free(mutable_list_by_binding);
   free(binding_flags_by_binding);
   free(bindings);

   pSupport->supported = supported;
}

static VkResult
radv_alloc_descriptor_pool_entry(struct radv_device *device, struct radv_descriptor_pool *pool,
                                 struct radv_descriptor_set *set)
{
   uint64_t current_offset = pool->current_offset;

   if (!pool->host_memory_base) {
      if (set->header.size) {
         uint64_t pool_vma_offset = util_vma_heap_alloc(&pool->bo_heap, set->header.size, 32);
         if (!pool_vma_offset)
            return VK_ERROR_FRAGMENTED_POOL;

         assert(pool_vma_offset >= RADV_POOL_HEAP_OFFSET && pool_vma_offset <= pool->size + RADV_POOL_HEAP_OFFSET);
         set->header.offset = pool_vma_offset - RADV_POOL_HEAP_OFFSET;
         current_offset = set->header.offset;
      }
   } else {
      if (current_offset + set->header.size > pool->size)
         return VK_ERROR_OUT_OF_POOL_MEMORY;

      pool->current_offset += set->header.size;
   }

   set->header.bo = pool->bo;
   set->header.mapped_ptr = (uint32_t *)(pool->mapped_ptr + current_offset);
   set->header.va = pool->bo ? (radv_buffer_get_va(set->header.bo) + current_offset) : 0;

   list_addtail(&set->link, &pool->sets);
   pool->entry_count++;
   return VK_SUCCESS;
}

static VkResult
radv_descriptor_set_create(struct radv_device *device, struct radv_descriptor_pool *pool,
                           struct radv_descriptor_set_layout *layout, const uint32_t variable_count,
                           struct radv_descriptor_set **out_set)
{
   const struct radv_physical_device *pdev = radv_device_physical(device);
   VkResult result;

   if (pool->entry_count == pool->max_entry_count)
      return VK_ERROR_OUT_OF_POOL_MEMORY;

   struct radv_descriptor_set *set;
   unsigned range_offset = sizeof(struct radv_descriptor_set);
   const unsigned dynamic_offset_count = layout->dynamic_offset_count;
   unsigned mem_size = range_offset + sizeof(struct radv_descriptor_range) * dynamic_offset_count;

   if (pool->host_memory_base) {
      if (pool->host_memory_end - pool->host_memory_ptr < mem_size)
         return VK_ERROR_OUT_OF_POOL_MEMORY;

      set = (struct radv_descriptor_set *)pool->host_memory_ptr;
      pool->host_memory_ptr += mem_size;
   } else {
      set = vk_alloc2(&device->vk.alloc, NULL, mem_size, 8, VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);

      if (!set)
         return vk_error(device, VK_ERROR_OUT_OF_HOST_MEMORY);
   }

   memset(set, 0, mem_size);

   vk_object_base_init(&device->vk, &set->header.base, VK_OBJECT_TYPE_DESCRIPTOR_SET);

   if (dynamic_offset_count) {
      set->header.dynamic_descriptors = (struct radv_descriptor_range *)((uint8_t *)set + range_offset);
   }

   set->header.layout = layout;
   uint32_t layout_size = layout->size;
   if (variable_count) {
      uint32_t stride = layout->binding[layout->binding_count - 1].size;
      if (layout->binding[layout->binding_count - 1].type == VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK)
         stride = 1;

      layout_size = layout->binding[layout->binding_count - 1].offset + variable_count * stride;
   }

   set->header.size = align(layout_size, 32);

   result = radv_alloc_descriptor_pool_entry(device, pool, set);
   if (result != VK_SUCCESS)
      return result;

   if (layout->has_immutable_samplers) {
      for (unsigned i = 0; i < layout->binding_count; ++i) {
         if (!layout->binding[i].immutable_samplers_offset)
            continue;

         unsigned offset = layout->binding[i].offset / 4;
         if (layout->binding[i].type == VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER)
            offset += radv_get_combined_image_sampler_offset(pdev) / 4;

         const uint32_t *samplers =
            (const uint32_t *)((const char *)layout + layout->binding[i].immutable_samplers_offset);
         for (unsigned j = 0; j < layout->binding[i].array_size; ++j) {
            memcpy(set->header.mapped_ptr + offset, samplers + 4 * j, RADV_SAMPLER_DESC_SIZE);
            offset += layout->binding[i].size / 4;
         }
      }
   }

   vk_descriptor_set_layout_ref(&layout->vk);
   *out_set = set;
   return VK_SUCCESS;
}

void
radv_descriptor_set_destroy(struct radv_device *device, struct radv_descriptor_pool *pool,
                            struct radv_descriptor_set *set)
{
   assert(!pool->host_memory_base);

   list_del(&set->link);
   vk_descriptor_set_layout_unref(&device->vk, &set->header.layout->vk);

   if (set->header.size)
      util_vma_heap_free(&pool->bo_heap, (uint64_t)set->header.offset + RADV_POOL_HEAP_OFFSET, set->header.size);
   pool->entry_count--;

   vk_object_base_finish(&set->header.base);
   vk_free2(&device->vk.alloc, NULL, set);
}

VKAPI_ATTR VkResult VKAPI_CALL
radv_AllocateDescriptorSets(VkDevice _device, const VkDescriptorSetAllocateInfo *pAllocateInfo,
                            VkDescriptorSet *pDescriptorSets)
{
   VK_FROM_HANDLE(radv_device, device, _device);
   VK_FROM_HANDLE(radv_descriptor_pool, pool, pAllocateInfo->descriptorPool);

   VkResult result = VK_SUCCESS;
   uint32_t i;
   struct radv_descriptor_set *set = NULL;

   const VkDescriptorSetVariableDescriptorCountAllocateInfo *variable_counts =
      vk_find_struct_const(pAllocateInfo->pNext, DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO);

   /* allocate a set of buffers for each shader to contain descriptors */
   for (i = 0; i < pAllocateInfo->descriptorSetCount; i++) {
      VK_FROM_HANDLE(radv_descriptor_set_layout, layout, pAllocateInfo->pSetLayouts[i]);

      uint32_t variable_count =
         layout->has_variable_descriptors && variable_counts && i < variable_counts->descriptorSetCount
            ? variable_counts->pDescriptorCounts[i]
            : 0;

      assert(!(layout->flags & VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT));

      result = radv_descriptor_set_create(device, pool, layout, variable_count, &set);
      if (result != VK_SUCCESS)
         break;

      pDescriptorSets[i] = radv_descriptor_set_to_handle(set);
   }

   if (result != VK_SUCCESS) {
      radv_FreeDescriptorSets(_device, pAllocateInfo->descriptorPool, i, pDescriptorSets);
      for (i = 0; i < pAllocateInfo->descriptorSetCount; i++) {
         pDescriptorSets[i] = VK_NULL_HANDLE;
      }
   }
   return result;
}

VKAPI_ATTR VkResult VKAPI_CALL
radv_FreeDescriptorSets(VkDevice _device, VkDescriptorPool descriptorPool, uint32_t count,
                        const VkDescriptorSet *pDescriptorSets)
{
   VK_FROM_HANDLE(radv_device, device, _device);
   VK_FROM_HANDLE(radv_descriptor_pool, pool, descriptorPool);

   for (uint32_t i = 0; i < count; i++) {
      VK_FROM_HANDLE(radv_descriptor_set, set, pDescriptorSets[i]);

      if (set && !pool->host_memory_base)
         radv_descriptor_set_destroy(device, pool, set);
   }
   return VK_SUCCESS;
}

static ALWAYS_INLINE void
radv_update_descriptor_sets_impl(struct radv_device *device, struct radv_cmd_buffer *cmd_buffer,
                                 VkDescriptorSet dstSetOverride, uint32_t descriptorWriteCount,
                                 const VkWriteDescriptorSet *pDescriptorWrites, uint32_t descriptorCopyCount,
                                 const VkCopyDescriptorSet *pDescriptorCopies)
{
   const struct radv_physical_device *pdev = radv_device_physical(device);
   const uint32_t sampled_desc_size = radv_get_sampled_image_desc_size(pdev);
   const uint32_t combined_sampler_offset = radv_get_combined_image_sampler_offset(pdev);

   for (uint32_t i = 0; i < descriptorWriteCount; i++) {
      const VkWriteDescriptorSet *writeset = &pDescriptorWrites[i];
      VK_FROM_HANDLE(radv_descriptor_set, set, dstSetOverride ? dstSetOverride : writeset->dstSet);
      const struct radv_descriptor_set_binding_layout *binding_layout =
         set->header.layout->binding + writeset->dstBinding;
      const uint32_t desc_size_dw = binding_layout->size / 4;

      uint32_t *ptr = set->header.mapped_ptr + binding_layout->offset / 4;
      const bool copy_immutable_samplers = cmd_buffer && binding_layout->immutable_samplers_offset;
      const uint32_t *samplers = radv_immutable_samplers(set->header.layout, binding_layout);
      const VkWriteDescriptorSetAccelerationStructureKHR *accel_structs = NULL;

      if (writeset->descriptorType == VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK) {
         radv_write_block_descriptor((uint8_t *)ptr + writeset->dstArrayElement, writeset);
         continue;
      } else if (writeset->descriptorType == VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR) {
         accel_structs = vk_find_struct_const(writeset->pNext, WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR);
      }

      ptr += desc_size_dw * writeset->dstArrayElement;

      for (uint32_t j = 0; j < writeset->descriptorCount; j++) {
         switch (writeset->descriptorType) {
         case VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC:
         case VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC: {
            const unsigned idx =
               binding_layout->dynamic_offset_offset + writeset->dstArrayElement + j;
            assert(!(set->header.layout->flags & VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT));
            radv_write_dynamic_buffer_descriptor(set->header.dynamic_descriptors + idx, writeset->pBufferInfo + j);
            break;
         }
         case VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER:
         case VK_DESCRIPTOR_TYPE_STORAGE_BUFFER:
            radv_write_buffer_descriptor_impl(device, ptr, writeset->pBufferInfo + j);
            break;
         case VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER:
         case VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER:
            radv_write_texel_buffer_descriptor(ptr, writeset->pTexelBufferView[j]);
            break;
         case VK_DESCRIPTOR_TYPE_STORAGE_IMAGE:
            radv_write_image_descriptor(ptr, RADV_STORAGE_IMAGE_DESC_SIZE, writeset->descriptorType,
                                        writeset->pImageInfo + j);
            break;
         case VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE:
         case VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT:
            radv_write_image_descriptor(ptr, sampled_desc_size, writeset->descriptorType,
                                        writeset->pImageInfo + j);
            break;
         case VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER:
            if (binding_layout->has_ycbcr_sampler) {
               radv_write_image_descriptor_ycbcr(device, ptr, writeset->pImageInfo + j, true);
            } else {
               radv_write_combined_image_sampler_descriptor(device, ptr, writeset->descriptorType,
                                                            writeset->pImageInfo + j,
                                                            !binding_layout->immutable_samplers_offset);
            }

            if (copy_immutable_samplers) {
               const unsigned idx = writeset->dstArrayElement + j;
               memcpy((char *)ptr + combined_sampler_offset, samplers + 4 * idx, RADV_SAMPLER_DESC_SIZE);
            }
            break;
         case VK_DESCRIPTOR_TYPE_SAMPLER:
            if (!binding_layout->immutable_samplers_offset) {
               radv_write_sampler_descriptor(ptr, writeset->pImageInfo[j].sampler);
            } else if (copy_immutable_samplers) {
               const unsigned idx = writeset->dstArrayElement + j;
               memcpy(ptr, samplers + 4 * idx, RADV_SAMPLER_DESC_SIZE);
            }
            break;
         case VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR: {
            VK_FROM_HANDLE(vk_acceleration_structure, accel_struct,
                           accel_structs->pAccelerationStructures[j]);
            radv_write_accel_struct_descriptor(device, ptr,
                                               accel_struct ? vk_acceleration_structure_get_va(accel_struct) : 0);
            break;
         }
         default:
            break;
         }

         ptr += desc_size_dw;
      }
   }

   for (uint32_t i = 0; i < descriptorCopyCount; i++) {
      const VkCopyDescriptorSet *copyset = &pDescriptorCopies[i];
      VK_FROM_HANDLE(radv_descriptor_set, src_set, copyset->srcSet);
      VK_FROM_HANDLE(radv_descriptor_set, dst_set, copyset->dstSet);

      const struct radv_descriptor_set_binding_layout *src_binding_layout =
         src_set->header.layout->binding + copyset->srcBinding;
      const struct radv_descriptor_set_binding_layout *dst_binding_layout =
         dst_set->header.layout->binding + copyset->dstBinding;

      if (src_binding_layout->type == VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK) {
         uint8_t *src_bytes = (uint8_t *)src_set->header.mapped_ptr +
                              src_binding_layout->offset + copyset->srcArrayElement;
         uint8_t *dst_bytes = (uint8_t *)dst_set->header.mapped_ptr +
                              dst_binding_layout->offset + copyset->dstArrayElement;
         memcpy(dst_bytes, src_bytes, copyset->descriptorCount);
         continue;
      }

      const uint32_t src_desc_dw = src_binding_layout->size / 4;
      const uint32_t dst_desc_dw = dst_binding_layout->size / 4;
      uint32_t *src_ptr = src_set->header.mapped_ptr + src_binding_layout->offset / 4 +
                          src_desc_dw * copyset->srcArrayElement;
      uint32_t *dst_ptr = dst_set->header.mapped_ptr + dst_binding_layout->offset / 4 +
                          dst_desc_dw * copyset->dstArrayElement;

      const bool src_is_dynamic = vk_descriptor_type_is_dynamic(src_binding_layout->type);
      const size_t copy_size = MIN2(src_binding_layout->size, dst_binding_layout->size);

      for (uint32_t j = 0; j < copyset->descriptorCount; j++) {
         if (src_is_dynamic) {
            const unsigned src_idx =
               src_binding_layout->dynamic_offset_offset + copyset->srcArrayElement + j;
            const unsigned dst_idx =
               dst_binding_layout->dynamic_offset_offset + copyset->dstArrayElement + j;
            dst_set->header.dynamic_descriptors[dst_idx] =
               src_set->header.dynamic_descriptors[src_idx];
         } else {
            memcpy(dst_ptr, src_ptr, copy_size);
         }

         src_ptr += src_desc_dw;
         dst_ptr += dst_desc_dw;
      }
   }
}

VKAPI_ATTR void VKAPI_CALL
radv_UpdateDescriptorSets(VkDevice _device, uint32_t descriptorWriteCount,
                          const VkWriteDescriptorSet *pDescriptorWrites, uint32_t descriptorCopyCount,
                          const VkCopyDescriptorSet *pDescriptorCopies)
{
   VK_FROM_HANDLE(radv_device, device, _device);

   radv_update_descriptor_sets_impl(device, NULL, VK_NULL_HANDLE, descriptorWriteCount, pDescriptorWrites,
                                    descriptorCopyCount, pDescriptorCopies);
}

void
radv_cmd_update_descriptor_sets(struct radv_device *device, struct radv_cmd_buffer *cmd_buffer,
                                VkDescriptorSet dstSetOverride, uint32_t descriptorWriteCount,
                                const VkWriteDescriptorSet *pDescriptorWrites, uint32_t descriptorCopyCount,
                                const VkCopyDescriptorSet *pDescriptorCopies)
{
   /* Assume cmd_buffer != NULL to optimize out cmd_buffer checks in generic code above. */
   assume(cmd_buffer != NULL);
   radv_update_descriptor_sets_impl(device, cmd_buffer, dstSetOverride, descriptorWriteCount, pDescriptorWrites,
                                    descriptorCopyCount, pDescriptorCopies);
}
