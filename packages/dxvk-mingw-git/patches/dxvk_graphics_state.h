#pragma once

#include "dxvk_format.h"
#include "dxvk_limits.h"

#include <array>
#include <atomic>
#include <cstring>
#include <optional>
#include <utility>

namespace dxvk {

  class DxvkIaInfo {

  public:

    DxvkIaInfo() = default;

    DxvkIaInfo(
            VkPrimitiveTopology primitiveTopology,
            VkBool32            primitiveRestart,
            uint32_t            patchVertexCount)
    : m_primitiveTopology (uint16_t(primitiveTopology)),
      m_primitiveRestart  (uint16_t(primitiveRestart)),
      m_patchVertexCount  (uint16_t(patchVertexCount)),
      m_reserved          (0) { }

    VkPrimitiveTopology primitiveTopology() const {
      return m_primitiveTopology <= VK_PRIMITIVE_TOPOLOGY_PATCH_LIST
        ? VkPrimitiveTopology(m_primitiveTopology)
        : VK_PRIMITIVE_TOPOLOGY_MAX_ENUM;
    }

    VkBool32 primitiveRestart() const {
      return VkBool32(m_primitiveRestart);
    }

    uint32_t patchVertexCount() const {
      return m_patchVertexCount;
    }

  private:

    uint16_t m_primitiveTopology      : 4;
    uint16_t m_primitiveRestart       : 1;
    uint16_t m_patchVertexCount       : 6;
    uint16_t m_reserved               : 5;

  };


  class DxvkIlInfo {

  public:

    DxvkIlInfo() = default;

    DxvkIlInfo(
            uint32_t        attributeCount,
            uint32_t        bindingCount)
    : m_attributeCount(uint8_t(attributeCount)),
      m_bindingCount  (uint8_t(bindingCount)) { }

    uint32_t attributeCount() const {
      return m_attributeCount;
    }

    uint32_t bindingCount() const {
      return m_bindingCount;
    }

  private:

    uint8_t m_attributeCount;
    uint8_t m_bindingCount;

  };


  class DxvkIlAttribute {

  public:

    DxvkIlAttribute() = default;

    DxvkIlAttribute(
            uint32_t                        location,
            uint32_t                        binding,
            VkFormat                        format,
            uint32_t                        offset)
    : m_location(uint32_t(location)),
      m_binding (uint32_t(binding)),
      m_format  (uint32_t(format)),
      m_offset  (uint32_t(offset)),
      m_reserved(0) { }

    uint32_t location() const {
      return m_location;
    }

    uint32_t binding() const {
      return m_binding;
    }

    VkFormat format() const {
      return VkFormat(m_format);
    }

    uint32_t offset() const {
      return m_offset;
    }

    VkVertexInputAttributeDescription description() const {
      VkVertexInputAttributeDescription result;
      result.location = m_location;
      result.binding  = m_binding;
      result.format   = VkFormat(m_format);
      result.offset   = m_offset;
      return result;
    }

  private:

    uint32_t m_location               : 5;
    uint32_t m_binding                : 5;
    uint32_t m_format                 : 7;
    uint32_t m_offset                 : 11;
    uint32_t m_reserved               : 4;

  };


  class DxvkIlBinding {

  public:

    DxvkIlBinding() = default;

    DxvkIlBinding(
            uint32_t                        binding,
            uint32_t                        stride,
            VkVertexInputRate               inputRate,
            uint32_t                        divisor)
    : m_binding   (uint32_t(binding)),
      m_stride    (uint32_t(stride)),
      m_inputRate (uint32_t(inputRate)),
      m_divisor   (uint32_t(divisor < (1u << 14) ? divisor : 0u)) { }

    uint32_t binding() const {
      return m_binding;
    }

    uint32_t stride() const {
      return m_stride;
    }

    VkVertexInputRate inputRate() const {
      return VkVertexInputRate(m_inputRate);
    }

    uint32_t divisor() const {
      return m_divisor;
    }

    VkVertexInputBindingDescription description() const {
      VkVertexInputBindingDescription result;
      result.binding = m_binding;
      result.stride  = m_stride;
      result.inputRate = VkVertexInputRate(m_inputRate);
      return result;
    }

    void setStride(uint32_t stride) {
      m_stride = stride;
    }

  private:

    uint32_t m_binding                : 5;
    uint32_t m_stride                 : 12;
    uint32_t m_inputRate              : 1;
    uint32_t m_divisor                : 14;

  };


  class DxvkRsInfo {

  public:

    DxvkRsInfo() = default;

    DxvkRsInfo(
            VkBool32              depthClipEnable,
            VkPolygonMode         polygonMode,
            VkSampleCountFlags    sampleCount,
            VkConservativeRasterizationModeEXT conservativeMode,
            VkBool32              flatShading,
            VkLineRasterizationModeEXT lineMode)
    : m_depthClipEnable (uint16_t(depthClipEnable)),
      m_polygonMode     (uint16_t(polygonMode)),
      m_sampleCount     (uint16_t(sampleCount)),
      m_conservativeMode(uint16_t(conservativeMode)),
      m_flatShading     (uint16_t(flatShading)),
      m_lineMode        (uint16_t(lineMode)),
      m_reserved        (0) { }

    VkBool32 depthClipEnable() const {
      return VkBool32(m_depthClipEnable);
    }

    VkPolygonMode polygonMode() const {
      return VkPolygonMode(m_polygonMode);
    }

    VkSampleCountFlags sampleCount() const {
      return VkSampleCountFlags(m_sampleCount);
    }

    VkConservativeRasterizationModeEXT conservativeMode() const {
      return VkConservativeRasterizationModeEXT(m_conservativeMode);
    }

    VkBool32 flatShading() const {
      return VkBool32(m_flatShading);
    }

    VkLineRasterizationModeEXT lineMode() const {
      return VkLineRasterizationModeEXT(m_lineMode);
    }

    bool eq(const DxvkRsInfo& other) const {
      return m_depthClipEnable  == other.m_depthClipEnable
          && m_polygonMode      == other.m_polygonMode
          && m_sampleCount      == other.m_sampleCount
          && m_conservativeMode == other.m_conservativeMode
          && m_flatShading      == other.m_flatShading
          && m_lineMode         == other.m_lineMode;
    }

  private:

    uint16_t m_depthClipEnable        : 1;
    uint16_t m_polygonMode            : 2;
    uint16_t m_sampleCount            : 5;
    uint16_t m_conservativeMode       : 2;
    uint16_t m_flatShading            : 1;
    uint16_t m_lineMode               : 2;
    uint16_t m_reserved               : 3;

  };


  class DxvkMsInfo {

  public:

    DxvkMsInfo() = default;

    DxvkMsInfo(
            VkSampleCountFlags      sampleCount,
            uint32_t                sampleMask,
            VkBool32                enableAlphaToCoverage)
    : m_sampleCount           (uint16_t(sampleCount)),
      m_enableAlphaToCoverage (uint16_t(enableAlphaToCoverage)),
      m_reserved              (0),
      m_sampleMask            (uint16_t(sampleMask)) { }

    VkSampleCountFlags sampleCount() const {
      return VkSampleCountFlags(m_sampleCount);
    }

    uint32_t sampleMask() const {
      return m_sampleMask;
    }

    VkBool32 enableAlphaToCoverage() const {
      return VkBool32(m_enableAlphaToCoverage);
    }

    void setSampleCount(VkSampleCountFlags sampleCount) {
      m_sampleCount = uint16_t(sampleCount);
    }

  private:

    uint16_t m_sampleCount            : 5;
    uint16_t m_enableAlphaToCoverage  : 1;
    uint16_t m_reserved               : 10;
    uint16_t m_sampleMask;

  };


  class DxvkOmInfo {

  public:

    DxvkOmInfo() = default;

    DxvkOmInfo(
            VkBool32           enableLogicOp,
            VkLogicOp          logicOp,
            VkImageAspectFlags feedbackLoop)
    : m_enableLogicOp (uint16_t(enableLogicOp)),
      m_logicOp       (uint16_t(logicOp)),
      m_feedbackLoop  (uint16_t(feedbackLoop)),
      m_reserved      (0) { }

    VkBool32 enableLogicOp() const {
      return VkBool32(m_enableLogicOp);
    }

    VkLogicOp logicOp() const {
      return VkLogicOp(m_logicOp);
    }

    VkImageAspectFlags feedbackLoop() const {
      return VkImageAspectFlags(m_feedbackLoop);
    }

    void setFeedbackLoop(VkImageAspectFlags feedbackLoop) {
      m_feedbackLoop = uint16_t(feedbackLoop);
    }

  private:

    uint16_t m_enableLogicOp          : 1;
    uint16_t m_logicOp                : 4;
    uint16_t m_feedbackLoop           : 2;
    uint16_t m_reserved               : 9;

  };


  class DxvkRtInfo {

  public:

    DxvkRtInfo() = default;

    DxvkRtInfo(
            uint32_t            colorFormatCount,
      const VkFormat*           colorFormats,
            VkFormat            depthStencilFormat,
            VkImageAspectFlags  depthStencilReadOnlyAspects)
    : m_packedData(0ull) {
      m_packedData |= encodeDepthStencilFormat(depthStencilFormat);
      m_packedData |= encodeDepthStencilAspects(depthStencilReadOnlyAspects);

      for (uint32_t i = 0; i < colorFormatCount; i++)
        m_packedData |= encodeColorFormat(colorFormats[i], i);
    }

    VkFormat getColorFormat(uint32_t index) const {
      return decodeColorFormat(m_packedData, index);
    }

    VkFormat getDepthStencilFormat() const {
      return decodeDepthStencilFormat(m_packedData);
    }

    VkImageAspectFlags getDepthStencilReadOnlyAspects() const {
      return decodeDepthStencilAspects(m_packedData);
    }

  private:

    static constexpr uint32_t Range0Start = uint32_t(VK_FORMAT_UNDEFINED);
    static constexpr uint32_t Range0End   = uint32_t(VK_FORMAT_E5B9G9R9_UFLOAT_PACK32);
    static constexpr uint32_t Range1Start = uint32_t(VK_FORMAT_A4R4G4B4_UNORM_PACK16);
    static constexpr uint32_t Range1End   = uint32_t(VK_FORMAT_A4B4G4R4_UNORM_PACK16);
    static constexpr uint32_t Range2Start = uint32_t(VK_FORMAT_A1B5G5R5_UNORM_PACK16_KHR);
    static constexpr uint32_t Range2End   = uint32_t(VK_FORMAT_A8_UNORM_KHR);

    static constexpr uint32_t Range0Size  = Range0End - Range0Start + 1u;
    static constexpr uint32_t Range1Size  = Range1End - Range1Start + 1u;
    static constexpr uint32_t Range2Size  = Range2End - Range2Start + 1u;

    uint64_t m_packedData;

    static uint64_t encodeDepthStencilAspects(VkImageAspectFlags aspects) {
      return uint64_t(aspects) << 61;
    }

    static uint64_t encodeDepthStencilFormat(VkFormat format) {
      return format
        ? (uint64_t(format) - uint64_t(VK_FORMAT_E5B9G9R9_UFLOAT_PACK32)) << 56
        : uint64_t(0);
    }

    static uint64_t encodeColorFormat(VkFormat format, uint32_t index) {
      const uint32_t f = uint32_t(format);
      uint32_t value = 0u;

      if (f <= Range0End)
        value = f - Range0Start;
      else if (f >= Range1Start && f <= Range1End)
        value = Range0Size + (f - Range1Start);
      else if (f >= Range2Start && f <= Range2End)
        value = Range0Size + Range1Size + (f - Range2Start);
      else
        value = 0u;

      return uint64_t(value) << (7u * index);
    }

    static VkImageAspectFlags decodeDepthStencilAspects(uint64_t value) {
      return VkImageAspectFlags((value >> 61) & (VK_IMAGE_ASPECT_DEPTH_BIT | VK_IMAGE_ASPECT_STENCIL_BIT));
    }

    static VkFormat decodeDepthStencilFormat(uint64_t value) {
      value = (value >> 56) & 0x1Fu;

      return value
        ? VkFormat(value + uint64_t(VK_FORMAT_E5B9G9R9_UFLOAT_PACK32))
        : VkFormat(VK_FORMAT_UNDEFINED);
    }

    static VkFormat decodeColorFormat(uint64_t value, uint32_t index) {
      uint32_t code = uint32_t((value >> (7u * index)) & 0x7Fu);

      if (code < Range0Size)
        return VkFormat(Range0Start + code);

      code -= Range0Size;

      if (code < Range1Size)
        return VkFormat(Range1Start + code);

      code -= Range1Size;

      if (code < Range2Size)
        return VkFormat(Range2Start + code);

      return VK_FORMAT_UNDEFINED;
    }

  };


  class DxvkOmAttachmentBlend {

  public:

    DxvkOmAttachmentBlend() = default;

    DxvkOmAttachmentBlend(
            VkBool32                    blendEnable,
            VkBlendFactor               srcColorBlendFactor,
            VkBlendFactor               dstColorBlendFactor,
            VkBlendOp                   colorBlendOp,
            VkBlendFactor               srcAlphaBlendFactor,
            VkBlendFactor               dstAlphaBlendFactor,
            VkBlendOp                   alphaBlendOp,
            VkColorComponentFlags       colorWriteMask)
    : m_blendEnable         (uint32_t(blendEnable)),
      m_srcColorBlendFactor (uint32_t(srcColorBlendFactor)),
      m_dstColorBlendFactor (uint32_t(dstColorBlendFactor)),
      m_colorBlendOp        (uint32_t(colorBlendOp)),
      m_srcAlphaBlendFactor (uint32_t(srcAlphaBlendFactor)),
      m_dstAlphaBlendFactor (uint32_t(dstAlphaBlendFactor)),
      m_alphaBlendOp        (uint32_t(alphaBlendOp)),
      m_colorWriteMask      (uint32_t(colorWriteMask)),
      m_reserved            (0) { }

    VkBool32 blendEnable() const {
      return m_blendEnable;
    }

    VkBlendFactor srcColorBlendFactor() const {
      return VkBlendFactor(m_srcColorBlendFactor);
    }

    VkBlendFactor dstColorBlendFactor() const {
      return VkBlendFactor(m_dstColorBlendFactor);
    }

    VkBlendOp colorBlendOp() const {
      return VkBlendOp(m_colorBlendOp);
    }

    VkBlendFactor srcAlphaBlendFactor() const {
      return VkBlendFactor(m_srcAlphaBlendFactor);
    }

    VkBlendFactor dstAlphaBlendFactor() const {
      return VkBlendFactor(m_dstAlphaBlendFactor);
    }

    VkBlendOp alphaBlendOp() const {
      return VkBlendOp(m_alphaBlendOp);
    }

    VkColorComponentFlags colorWriteMask() const {
      return VkColorComponentFlags(m_colorWriteMask);
    }

    VkPipelineColorBlendAttachmentState state() const {
      VkPipelineColorBlendAttachmentState result;
      result.blendEnable         = VkBool32(m_blendEnable);
      result.srcColorBlendFactor = VkBlendFactor(m_srcColorBlendFactor);
      result.dstColorBlendFactor = VkBlendFactor(m_dstColorBlendFactor);
      result.colorBlendOp        = VkBlendOp(m_colorBlendOp);
      result.srcAlphaBlendFactor = VkBlendFactor(m_srcAlphaBlendFactor);
      result.dstAlphaBlendFactor = VkBlendFactor(m_dstAlphaBlendFactor);
      result.alphaBlendOp        = VkBlendOp(m_alphaBlendOp);
      result.colorWriteMask      = VkColorComponentFlags(m_colorWriteMask);
      return result;
    }

  private:

    uint32_t m_blendEnable            : 1;
    uint32_t m_srcColorBlendFactor    : 5;
    uint32_t m_dstColorBlendFactor    : 5;
    uint32_t m_colorBlendOp           : 3;
    uint32_t m_srcAlphaBlendFactor    : 5;
    uint32_t m_dstAlphaBlendFactor    : 5;
    uint32_t m_alphaBlendOp           : 3;
    uint32_t m_colorWriteMask         : 4;
    uint32_t m_reserved               : 1;

  };


  class DxvkOmAttachmentSwizzle {

  public:

    DxvkOmAttachmentSwizzle() = default;

    DxvkOmAttachmentSwizzle(VkComponentMapping mapping)
    : m_r(util::getComponentIndex(mapping.r, 0)),
      m_g(util::getComponentIndex(mapping.g, 1)),
      m_b(util::getComponentIndex(mapping.b, 2)),
      m_a(util::getComponentIndex(mapping.a, 3)) { }

    uint32_t rIndex() const { return m_r; }
    uint32_t gIndex() const { return m_g; }
    uint32_t bIndex() const { return m_b; }
    uint32_t aIndex() const { return m_a; }

    VkComponentMapping mapping() const {
      VkComponentMapping result;
      result.r = decodeSwizzle(m_r);
      result.g = decodeSwizzle(m_g);
      result.b = decodeSwizzle(m_b);
      result.a = decodeSwizzle(m_a);
      return result;
    }

  private:

    uint8_t m_r : 2;
    uint8_t m_g : 2;
    uint8_t m_b : 2;
    uint8_t m_a : 2;

    static VkComponentSwizzle decodeSwizzle(uint8_t swizzle) {
      return VkComponentSwizzle(uint32_t(swizzle) + uint32_t(VK_COMPONENT_SWIZZLE_R));
    }

  };


  struct DxvkScInfo {
    uint32_t specConstants[DxvkLimits::MaxNumSpecConstants];
  };


  struct alignas(32) DxvkGraphicsPipelineStateInfo {
    DxvkGraphicsPipelineStateInfo() {
      std::memset(this, 0, sizeof(*this));
    }

    DxvkGraphicsPipelineStateInfo(const DxvkGraphicsPipelineStateInfo& other) {
      std::memcpy(this, &other, sizeof(*this));
    }

    DxvkGraphicsPipelineStateInfo& operator = (const DxvkGraphicsPipelineStateInfo& other) {
      std::memcpy(this, &other, sizeof(*this));
      return *this;
    }

    bool eq(const DxvkGraphicsPipelineStateInfo& other) const {
      return bit::bcmpeq(this, &other);
    }

    size_t hash() const {
      auto src = reinterpret_cast<const unsigned char*>(this);
      return size_t(bit::fnv1a_hash(src, sizeof(*this)));
    }

    bool useDynamicDepthTest() const {
      return rt.getDepthStencilFormat();
    }

    bool useDynamicDepthBounds() const {
      return rt.getDepthStencilFormat();
    }

    bool useDynamicStencilTest() const {
      const VkFormat format = rt.getDepthStencilFormat();
      return format && (lookupFormatInfo(format)->aspectMask & VK_IMAGE_ASPECT_STENCIL_BIT);
    }

    bool useDynamicVertexStrides() const {
      const uint32_t count = il.bindingCount();

      if (!count)
        return false;

      for (uint32_t i = 0u; i < count; i++) {
        if (ilBindings[i].stride())
          return false;
      }

      return true;
    }

    bool useDynamicBlendConstants() const {
      for (uint32_t i = 0u; i < MaxNumRenderTargets; i++) {
        if (!rt.getColorFormat(i))
          continue;

        const auto& blend = omBlend[i];

        if (!blend.blendEnable())
          continue;

        if (util::isBlendConstantBlendFactor(blend.srcColorBlendFactor())
         || util::isBlendConstantBlendFactor(blend.dstColorBlendFactor())
         || util::isBlendConstantBlendFactor(blend.srcAlphaBlendFactor())
         || util::isBlendConstantBlendFactor(blend.dstAlphaBlendFactor()))
          return true;
      }

      return false;
    }

    bool useDualSourceBlending() const {
      return omBlend[0].blendEnable() && (
        util::isDualSourceBlendFactor(omBlend[0].srcColorBlendFactor()) ||
        util::isDualSourceBlendFactor(omBlend[0].dstColorBlendFactor()) ||
        util::isDualSourceBlendFactor(omBlend[0].srcAlphaBlendFactor()) ||
        util::isDualSourceBlendFactor(omBlend[0].dstAlphaBlendFactor()));
    }

    bool useSampleLocations() const {
      return ms.sampleCount() != VK_SAMPLE_COUNT_1_BIT
          && rs.sampleCount() == VK_SAMPLE_COUNT_1_BIT;
    }

    bool writesRenderTarget(uint32_t target) const {
      if (unlikely(target >= MaxNumRenderTargets))
        return false;

      if (!omBlend[target].colorWriteMask())
        return false;

      return rt.getColorFormat(target) != VK_FORMAT_UNDEFINED;
    }


    DxvkIaInfo              ia;
    DxvkIlInfo              il;
    DxvkRsInfo              rs;
    DxvkMsInfo              ms;
    DxvkOmInfo              om;
    DxvkRtInfo              rt;
    DxvkScInfo              sc;
    DxvkOmAttachmentSwizzle omSwizzle         [DxvkLimits::MaxNumRenderTargets];
    DxvkOmAttachmentBlend   omBlend           [DxvkLimits::MaxNumRenderTargets];
    DxvkIlAttribute         ilAttributes      [DxvkLimits::MaxNumVertexAttributes];
    DxvkIlBinding           ilBindings        [DxvkLimits::MaxNumVertexBindings];
  };


  struct alignas(32) DxvkComputePipelineStateInfo {
    DxvkComputePipelineStateInfo() {
      std::memset(this, 0, sizeof(*this));
    }

    DxvkComputePipelineStateInfo(const DxvkComputePipelineStateInfo& other) {
      std::memcpy(this, &other, sizeof(*this));
    }

    DxvkComputePipelineStateInfo& operator = (const DxvkComputePipelineStateInfo& other) {
      std::memcpy(this, &other, sizeof(*this));
      return *this;
    }

    bool eq(const DxvkComputePipelineStateInfo& other) const {
      return bit::bcmpeq(this, &other);
    }

    size_t hash() const {
      auto src = reinterpret_cast<const unsigned char*>(this);
      return size_t(bit::fnv1a_hash(src, sizeof(*this)));
    }

    DxvkScInfo              sc;
  };


  template<typename K, typename V>
  class DxvkPipelineVariantTable {
    static constexpr size_t LayerBits = 5u;
    static constexpr size_t LayerSize = 1u << LayerBits;

    static constexpr uint32_t HashThreshold = 4u;
  public:

    ~DxvkPipelineVariantTable() {
      iter(m_table, [] (Entry* e) { delete e; });
    }

    V* find(const K& k) const {
      uint32_t mask = m_table.mask.load(std::memory_order_acquire);

      bool useSimple = !(mask & (mask - 1u));

      if (!useSimple)
        useSimple = bit::popcnt(mask) < HashThreshold;

      if (likely(useSimple)) {
        for (auto index : bit::BitMask(mask)) {
          Entry* e = m_table.entries[index].load(std::memory_order_acquire);

          if (!e)
            continue;

          useSimple = useSimple && !e->table.mask.load(std::memory_order_relaxed);

          while (e) {
            if (e->key.eq(k))
              return &e->value;

            e = e->next.load(std::memory_order_acquire);
          }
        }

        if (likely(useSimple))
          return nullptr;
      }

      const size_t hash = k.hash();
      size_t shift = 0u;

      const Table* table = &m_table;

      while (true) {
        const size_t index = computeListIndex(hash, shift);
        shift += LayerBits;

        Entry* e = table->entries[index].load(std::memory_order_acquire);

        if (!e)
          break;

        table = &e->table;

        if (e->hash != hash)
          continue;

        while (e) {
          if (e->key.eq(k))
            return &e->value;

          e = e->next.load(std::memory_order_acquire);
        }
      }

      return nullptr;
    }

    template<typename... Args>
    V* add(const K& k, Args&&... args) {
      const size_t hash = k.hash();

      Entry* entry = new Entry(k, hash, std::forward<Args>(args)...);
      Table* table = &m_table;
      Entry* target = nullptr;

      size_t index = 0u;
      size_t shift = 0u;

      while (!target) {
        index = computeListIndex(hash, shift);

        if (table->entries[index].compare_exchange_strong(target, entry,
            std::memory_order_release, std::memory_order_acquire))
          break;

        if (target->hash == hash)
          break;

        table = &target->table;
        target = nullptr;

        shift += LayerBits;
      }

      if (target) {
        while (true) {
          Entry* next = nullptr;

          if (target->next.compare_exchange_strong(next, entry,
              std::memory_order_release, std::memory_order_acquire))
            break;

          target = next;
        }
      } else {
        table->mask.fetch_or(1u << index, std::memory_order_release);
      }

      return &entry->value;
    }

    template<typename Fn>
    void forEach(const Fn& fn) const {
      iter(m_table, [&] (Entry* e) { fn(e->value); });
    }

  private:

    struct Entry;

    struct Table {
      std::array<std::atomic<Entry*>, LayerSize> entries = { };
      std::atomic<uint32_t> mask = { 0u };
    };

    struct Entry {
      template<typename... Args>
      Entry(const K& k, size_t h, Args&&... args)
      : key(k), hash(h), value(std::forward<Args>(args)...) { }

      K       key   = { };
      size_t  hash  = 0u;
      V       value = { };
      Table   table = { };

      std::atomic<Entry*> next = { nullptr };
    };

    Table m_table;

    template<typename Fn>
    static void iter(const Table& table, const Fn& fn) {
      const uint32_t mask = table.mask.load(std::memory_order_acquire);

      for (auto index : bit::BitMask(mask)) {
        Entry* e = table.entries[index].load(std::memory_order_relaxed);

        if (!e)
          continue;

        if (e->table.mask.load(std::memory_order_relaxed))
          iter(e->table, fn);

        while (e) {
          Entry* next = e->next.load(std::memory_order_acquire);
          fn(e);
          e = next;
        }
      }
    }

    static size_t computeListIndex(size_t hash, size_t shift) {
      if constexpr (sizeof(size_t) == sizeof(uint64_t)) {
        const uint64_t x = uint64_t(hash);
        const uint64_t y = __builtin_bswap64(x) + x;
        return size_t((y >> shift) & uint64_t(LayerSize - 1u));
      } else {
        const uint32_t x = uint32_t(hash);
        const uint32_t y = __builtin_bswap32(x) + x;
        return size_t((y >> shift) & uint32_t(LayerSize - 1u));
      }
    }

  };

}
