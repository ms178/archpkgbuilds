#pragma once

#include <ir/ir_builder.h>
#include <ir/ir_utils.h>

#include <spirv/spirv_builder.h>
#include <spirv/spirv_mapping.h>

#include "dxvk_device.h"
#include "dxvk_util.h"

using namespace dxbc_spv;

namespace dxvk {

  /**
   * \brief Built-in vertex shader info
   */
  struct DxvkBuiltInVertexShader {
    ir::SsaDef vertexIndex = { };
    ir::SsaDef instanceIndex = { };
    ir::SsaDef coord = { };
  };

  /**
   * \brief Built-in pixel shader info
   */
  struct DxvkBuiltInPixelShader {
    ir::SsaDef sampleId = { };
  };

  /**
   * \brief Built-in compute shader info
   */
  struct DxvkBuiltInComputeShader {
    ir::SsaDef globalId = { };
    ir::SsaDef groupId = { };
    ir::SsaDef localId = { };
    ir::SsaDef localIndex = { };
  };

  /**
   * \brief Trivial resource mapping for built-in shaders
   */
  class DxvkBuiltInResourceMapping : public dxbc_spv::spirv::ResourceMapping {
  public:
    explicit DxvkBuiltInResourceMapping(const DxvkPipelineLayout* layout);
    ~DxvkBuiltInResourceMapping();

    dxbc_spv::spirv::DescriptorBinding mapDescriptor(
            ir::ScalarType          type,
            uint32_t                regSpace,
            uint32_t                regIndex) override;

    uint32_t mapPushData(ir::ShaderStageMask stages) override;

  private:
    uint32_t m_setIndex = 0u;
  };

  /**
   * \brief Built-in shader builder
   *
   * Provides common functionality to dynamically build dxbc-spirv IR
   * shaders for built-in operations.
   *
   * Shaders must be built in such a way that no legalization passes need
   * to be run, i.e. must be in SSA form and not use scoped control flow.
   * It is however allowed to use vectorized arithmetic since the builder
   * will run the scalarization pass. This is done for convenience.
   */
  class DxvkBuiltInShader {
  public:
    DxvkBuiltInShader(
            DxvkDevice*           device,
      const DxvkPipelineLayout*   layout,
      const std::string&          name);
    ~DxvkBuiltInShader();

    DxvkBuiltInComputeShader buildComputeShader(
            ir::Builder&          builder,
            VkExtent3D            groupSize);

    DxvkBuiltInPixelShader buildPixelShader(
            ir::Builder&          builder);

    DxvkBuiltInVertexShader buildFullscreenVertexShader(
            ir::Builder&          builder);

    ir::SsaDef declareImageSrv(
            ir::Builder&          builder,
            uint32_t              binding,
      const char*                 name,
            VkImageViewType       viewType,
            VkFormat              viewFormat,
            VkImageAspectFlagBits viewAspect,
            VkSampleCountFlagBits samples);

    ir::SsaDef declareTexelBufferSrv(
            ir::Builder&          builder,
            uint32_t              binding,
      const char*                 name,
            VkFormat              viewFormat);

    ir::SsaDef declareBufferSrv(
            ir::Builder&          builder,
            uint32_t              binding,
      const char*                 name,
            ir::BasicType         elementType);

    ir::SsaDef declareImageUav(
            ir::Builder&          builder,
            uint32_t              binding,
      const char*                 name,
            VkImageViewType       viewType,
            VkFormat              viewFormat);

    ir::SsaDef declareTexelBufferUav(
            ir::Builder&          builder,
            uint32_t              binding,
      const char*                 name,
            VkFormat              viewFormat);

    ir::SsaDef declareBufferUav(
            ir::Builder&          builder,
            uint32_t              binding,
      const char*                 name,
            ir::BasicType         elementType);

    ir::SsaDef declareSampler(
            ir::Builder&          builder,
            uint32_t              pushDataOffset,
      const char*                 name);

    ir::SsaDef declareInputTarget(
            ir::Builder&          builder,
            uint32_t              binding,
      const char*                 name,
            uint32_t              attachment,
            VkFormat              format,
            VkSampleCountFlagBits samples);

    ir::SsaDef declarePushData(
            ir::Builder&          builder,
            ir::BasicType         type,
            uint32_t              pushDataOffset,
      const char*                 name);

    ir::SsaDef declareBuiltInInput(
            ir::Builder&          builder,
            ir::BasicType         type,
            ir::BuiltIn           builtIn,
            ir::InterpolationModes interpolation = ir::InterpolationModes());

    ir::SsaDef declareInput(
            ir::Builder&          builder,
            ir::BasicType         type,
            uint32_t              location,
      const char*                 name,
            ir::InterpolationModes interpolation = ir::InterpolationModes());

    void exportBuiltIn(
            ir::Builder&          builder,
            ir::BuiltIn           builtIn,
            ir::SsaDef            value);

    void exportOutput(
            ir::Builder&          builder,
            uint32_t              location,
            ir::SsaDef            value,
      const char*                 name);

    ir::SsaDef emitBoundCheck(
            ir::Builder&          builder,
            ir::SsaDef            coord,
            ir::SsaDef            size,
            uint32_t              dims);

    ir::SsaDef emitConditionalBlock(
            ir::Builder&          builder,
            ir::SsaDef            cond);

    ir::SsaDef emitExtractVector(
            ir::Builder&          builder,
            ir::SsaDef            vector,
            uint32_t              first,
            uint32_t              count);

    ir::SsaDef emitConcatVector(
            ir::Builder&          builder,
            ir::SsaDef            a,
            ir::SsaDef            b);

    ir::SsaDef emitReplicateScalar(
            ir::Builder&          builder,
            ir::BasicType         type,
            ir::SsaDef            value);

    ir::SsaDef emitFormatVector(
            ir::Builder&          builder,
            VkFormat              format,
            ir::SsaDef            a);

    ir::SsaDef buildLinearToSrgbFn(
            ir::Builder&          builder,
      const ir::Type&             type);

    void printShader(
            LogLevel              level,
            ir::Builder&          builder);

    std::vector<uint32_t> buildShader(
            ir::Builder&          builder);

    ir::ScalarType determineSampledType(
            VkFormat              format,
            VkImageAspectFlagBits aspect);

    ir::ResourceKind determineResourceKind(
            VkImageViewType       viewType,
            VkSampleCountFlagBits samples);

  private:
    DxvkShaderOptions m_options = { };
    DxvkBuiltInResourceMapping m_resourceMapping;
    std::string m_name;

    ir::SsaDef findEntryPoint(
            ir::Builder&          builder);
    ir::SsaDef findEntryPointFunction(
            ir::Builder&          builder);
    ir::SsaDef declareEntryPoint(
            ir::Builder&          builder,
            ir::ShaderStage       stage);
    ir::SsaDef splitLoad(
            ir::Builder&          builder,
            ir::OpCode            opCode,
            ir::SsaDef            def);
    ir::SsaDef makeConstantVector(
            ir::Builder&          builder,
            ir::SsaDef            constant,
            ir::BasicType         type);
    void splitStore(
            ir::Builder&          builder,
            ir::OpCode            opCode,
            ir::SsaDef            var,
            ir::SsaDef            value);
    void dumpShader(
            size_t                size,
      const uint32_t*             dwords);
  };

}
