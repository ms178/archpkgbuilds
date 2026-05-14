#include <cstring>
#include <iomanip>
#include <sstream>

#include "../util/util_time.h"

#include "dxvk_compute.h"
#include "dxvk_device.h"
#include "dxvk_graphics.h"
#include "dxvk_pipemanager.h"

namespace dxvk {
  
  DxvkComputePipeline::DxvkComputePipeline(
          DxvkDevice*                 device,
          DxvkPipelineManager*        pipeMgr,
          DxvkComputePipelineShaders  shaders,
          DxvkShaderPipelineLibrary*  library)
  : m_device        (device),
    m_stats         (&pipeMgr->m_stats),
    m_library       (library),
    m_shaders       (std::move(shaders)),
    m_layout        (device, pipeMgr, m_shaders.cs->getLayout()),
    m_debugName     (createDebugName()) {

  }
  
  
  DxvkComputePipeline::~DxvkComputePipeline() {
    m_library->releasePipelineHandle();

    m_pipelines.forEach([this] (const DxvkComputePipelineInstance& instance) {
      this->destroyPipeline(instance.handle);
    });
  }
  
  
  VkPipeline DxvkComputePipeline::getPipelineHandle(
    const DxvkComputePipelineStateInfo& state) {
    if (!m_libraryHandle) {
      // Retrieve actual pipeline handle on first use. This
      // may wait for an ongoing compile job to finish, or
      // compile the pipeline immediately on the calling thread.
      m_libraryHandle = m_library->acquirePipelineHandle().handle;
    }

    // Compute pipelines without spec constants are pre-compiled, so the
    // common case is a non-null library handle. Mark this branch as the
    // expected one so the compiler keeps the spec-constant slow path out
    // of the hot basic block (better I-cache density on Raptor Lake's
    // small 32 KB L1i; the µop cache is also line-granular so cold code
    // moved out of the function reduces front-end pressure).
    if (likely(*m_libraryHandle)) {
      return *m_libraryHandle;
    } else {
      // Slow path for compute shaders that do use spec constants
      DxvkComputePipelineInstance* instance = this->findInstance(state);

      if (unlikely(!instance)) {
        std::lock_guard<dxvk::mutex> lock(m_mutex);
        instance = this->findInstance(state);

        if (!instance)
          instance = this->createInstance(state);
      }

      return instance->handle;
    }
  }


  void DxvkComputePipeline::compilePipeline(
    const DxvkComputePipelineStateInfo& state) {
    // For every compute pipeline created with a backing shader-pipeline
    // library (the standard path on Vulkan 1.3 + RADV/ACO, i.e. essentially
    // all pipelines on Vega/RADV), m_library is non-null and this entire
    // function is a no-op. The pipeline-precompile worker invokes this for
    // every cached pipeline during shader warmup, so the hint avoids any
    // mispredict on the function-prologue branch.
    if (unlikely(!m_library)) {
      std::lock_guard<dxvk::mutex> lock(m_mutex);

      if (!this->findInstance(state))
        this->createInstance(state);
    }
  }
  
  
  DxvkComputePipelineInstance* DxvkComputePipeline::createInstance(
    const DxvkComputePipelineStateInfo& state) {
    VkPipeline newPipelineHandle = this->createPipeline(state);

    m_stats->numComputePipelines += 1;
    return m_pipelines.add(state, newPipelineHandle);
  }

  
  DxvkComputePipelineInstance* DxvkComputePipeline::findInstance(
    const DxvkComputePipelineStateInfo& state) {
    return m_pipelines.find(state);
  }
  
  
  VkPipeline DxvkComputePipeline::createPipeline(
    const DxvkComputePipelineStateInfo& state) const {
    auto vk = m_device->vkd();
    auto layout = m_layout.getLayout(DxvkPipelineLayoutType::Merged);

    DxvkPipelineSpecConstantState scState(m_shaders.cs->metadata().specConstantMask, state.sc);
    
    DxvkShaderStageInfo stageInfo(m_device, layout);
    stageInfo.addStage(VK_SHADER_STAGE_COMPUTE_BIT, 
      m_shaders.cs->getCode(m_layout.getBindingMap(DxvkPipelineLayoutType::Merged), nullptr),
      &scState.scInfo);

    VkPipelineCreateFlags2CreateInfo flags = { VK_STRUCTURE_TYPE_PIPELINE_CREATE_FLAGS_2_CREATE_INFO };

    if (m_device->canUseDescriptorHeap())
      flags.flags |= VK_PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT;

    if (m_device->canUseDescriptorBuffer())
      flags.flags |= VK_PIPELINE_CREATE_2_DESCRIPTOR_BUFFER_BIT_EXT;

    VkComputePipelineCreateInfo info = { VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO };
    info.stage                = *stageInfo.getStageInfos();
    info.layout               = layout->getPipelineLayout();
    info.basePipelineIndex    = -1;

    if (flags.flags)
      flags.pNext = std::exchange(info.pNext, &flags);

    VkPipeline pipeline = VK_NULL_HANDLE;
    VkResult vr = vk->vkCreateComputePipelines(vk->device(),
          VK_NULL_HANDLE, 1, &info, nullptr, &pipeline);

    if (vr != VK_SUCCESS) {
      Logger::err(str::format("DxvkComputePipeline: Failed to compile pipeline: ", vr));
      this->logPipelineState(LogLevel::Error, state);
      return VK_NULL_HANDLE;
    }

    return pipeline;
  }


  void DxvkComputePipeline::destroyPipeline(VkPipeline pipeline) {
    auto vk = m_device->vkd();

    vk->vkDestroyPipeline(vk->device(), pipeline, nullptr);
  }
  
  
  void DxvkComputePipeline::logPipelineState(
          LogLevel                      level,
    const DxvkComputePipelineStateInfo& state) const {
    std::stringstream sstr;
    sstr << "  cs  : " << m_shaders.cs->debugName() << std::endl;

    bool hasSpecConstants = false;

    for (uint32_t i = 0; i < MaxNumSpecConstants; i++) {
      if (state.sc.specConstants[i]) {
        if (!hasSpecConstants) {
          sstr << "Specialization constants:" << std::endl;
          hasSpecConstants = true;
        }

        sstr << "  " << i << ": 0x" << std::hex << std::setw(8) << std::setfill('0') << state.sc.specConstants[i] << std::dec << std::endl;
      }
    }

    Logger::log(level, sstr.str());
  }


  std::string DxvkComputePipeline::createDebugName() const {
    // Build the bracketed debug name in a single allocation. The original
    // form did three: debugName() returned a string by value, substr() cut
    // a copy, and str::format produced the final result. The shader debug
    // name is bounded to 10 characters here, so the final string is at
    // most 12 bytes including the brackets -- comfortably inside libstdc++'s
    // small-string-optimisation buffer (15 bytes), so the result allocates
    // zero heap when the SSO threshold isn't crossed.
    const std::string shaderName = m_shaders.cs->debugName();
    const size_t len = std::min(shaderName.size(), size_t(10));

    std::string result;
    result.reserve(len + 2u);
    result.push_back('[');
    result.append(shaderName, 0, len);
    result.push_back(']');
    return result;
  }

}
