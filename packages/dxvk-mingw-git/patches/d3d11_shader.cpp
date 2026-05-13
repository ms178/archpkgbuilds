#include <dxbc/dxbc_container.h>
#include <dxbc/dxbc_interface.h>
#include <dxbc/dxbc_parser.h>

#include "d3d11_device.h"
#include "d3d11_shader.h"

namespace dxvk {

  class D3D11ShaderConverter : public DxvkIrShaderConverter {

  public:

    D3D11ShaderConverter(
      const DxvkShaderHash&         ShaderKey,
      const DxvkIrShaderCreateInfo& ModuleInfo,
      const void*                   pShaderBytecode,
            size_t                  BytecodeLength,
            bool                    LowerIcb)
    : m_key(ShaderKey), m_info(ModuleInfo), m_lowerIcb(LowerIcb) {
      // The DXBC bytecode pointer is owned by the caller and may be freed
      // immediately after Create*Shader returns (per D3D11 spec). The
      // converter is invoked asynchronously on a pipeline-compile worker,
      // so we must take a private copy here. assign() from an iterator
      // pair avoids the spurious zero-init that resize() + memcpy would do
      // for a trivially default-constructible element type on libstdc++.
      const auto* begin = reinterpret_cast<const uint8_t*>(pShaderBytecode);
      m_dxbc.assign(begin, begin + BytecodeLength);
    }

    ~D3D11ShaderConverter() = default;

    void convertShader(dxbc_spv::ir::Builder& builder) {
      const std::string shaderName = m_key.toString();

      dxbc_spv::dxbc::Converter::Options options = { };
      options.name = shaderName.c_str();
      options.includeDebugNames = true;
      options.boundCheckShaderIo = true;
      options.lowerIcb = m_lowerIcb;
      options.icbRegisterSpace = 0u;
      options.icbRegisterIndex = D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT;
      options.classInstanceRegisterSpace = 0u;
      options.classInstanceRegisterIndex = D3D11_COMMONSHADER_CONSTANT_BUFFER_API_SLOT_COUNT + 1u;
      options.limitTessFactor = true;

      dxbc_spv::dxbc::Container container(m_dxbc.data(), m_dxbc.size());

      dxbc_spv::dxbc::ShaderInfo shaderInfo =
        dxbc_spv::dxbc::Parser(container.getCodeChunk()).getShaderInfo();

      dxbc_spv::dxbc::Converter converter(std::move(container), options);

      // Determine whether to create a regular shader or a pass-through GS
      const bool dstIsGs = m_key.stage() == VK_SHADER_STAGE_GEOMETRY_BIT;
      const bool srcIsGs = shaderInfo.getType() == dxbc_spv::dxbc::ShaderType::eGeometry;

      if (dstIsGs && !srcIsGs) {
        if (!converter.createPassthroughGs(builder))
          throw DxvkError(str::format("Failed to create pass-through geometry shader: ", shaderName));
      } else {
        if (!converter.convertShader(builder))
          throw DxvkError(str::format("Failed to convert shader: ", shaderName));
      }
    }

    uint32_t determineResourceIndex(
            dxbc_spv::ir::ShaderStage stage,
            dxbc_spv::ir::ScalarType  type,
            uint32_t                  regSpace,
            uint32_t                  regIndex) const {
      (void)regSpace;

      switch (type) {
        case dxbc_spv::ir::ScalarType::eSampler:
          return D3D11ShaderResourceMapping::computeSamplerBinding(stage, regIndex);
        case dxbc_spv::ir::ScalarType::eCbv:
          return D3D11ShaderResourceMapping::computeCbvBinding(stage, regIndex);
        case dxbc_spv::ir::ScalarType::eSrv:
          return D3D11ShaderResourceMapping::computeSrvBinding(stage, regIndex);
        case dxbc_spv::ir::ScalarType::eUav:
          return D3D11ShaderResourceMapping::computeUavBinding(stage, regIndex);
        case dxbc_spv::ir::ScalarType::eUavCounter:
          return D3D11ShaderResourceMapping::computeUavCounterBinding(stage, regIndex);
        default:
          return 0xffffffffu;
      }
    }

    void dumpSource(const std::string& path) const {
      const auto filePath =
        str::topath(str::format(path, "/", m_key.toString(), ".dxbc").c_str());
      std::ofstream file(filePath.c_str(), std::ios_base::trunc | std::ios_base::binary);
      file.write(reinterpret_cast<const char*>(m_dxbc.data()),
                 static_cast<std::streamsize>(m_dxbc.size()));
    }

    std::string getDebugName() const {
      return m_key.toString();
    }

  private:

    std::vector<uint8_t>    m_dxbc;

    DxvkShaderHash          m_key;
    DxvkIrShaderCreateInfo  m_info;

    bool                    m_lowerIcb = false;

  };


  D3D11CommonShader:: D3D11CommonShader() = default;
  D3D11CommonShader::~D3D11CommonShader() = default;


  D3D11CommonShader::D3D11CommonShader(
          D3D11Device*            pDevice,
          D3D11ClassLinkage*      pLinkage,
    const DxvkShaderHash&         ShaderKey,
    const DxvkIrShaderCreateInfo& ModuleInfo,
    const void*                   pShaderBytecode,
          size_t                  BytecodeLength,
    const D3D11ShaderIcbInfo&     Icb,
    const D3D11BindingMask&       BindingMask)
  : m_bindings(BindingMask) {
    if (Logger::logLevel() <= LogLevel::Debug)
      Logger::debug(str::format("Compiling shader ", ShaderKey.toString()));

    if (pLinkage)
      GatherInterefaceInfo(pLinkage, pShaderBytecode, BytecodeLength);

    CreateIrShader(pDevice, ShaderKey, ModuleInfo, pShaderBytecode, BytecodeLength, Icb);
    pDevice->GetDXVKDevice()->registerShader(m_shader);
  }


  void D3D11CommonShader::CreateIrShader(
          D3D11Device*            pDevice,
    const DxvkShaderHash&         ShaderKey,
    const DxvkIrShaderCreateInfo& ModuleInfo,
    const void*                   pShaderBytecode,
          size_t                  BytecodeLength,
    const D3D11ShaderIcbInfo&     Icb) {
    constexpr size_t MaxEmbeddedIcbSize = 64u;

    // Create icb if lowering is required. sizeof(*Icb.data) is used (rather
    // than a hard-coded uint32_t) so the calculation tracks the type of the
    // ICB element if it is ever changed in the header.
    const size_t icbSizeInBytes = Icb.size * sizeof(*Icb.data);

    if (ModuleInfo.options.flags.test(DxvkShaderCompileFlag::LowerConstantArrays)
     && icbSizeInBytes > MaxEmbeddedIcbSize) {
      DxvkBufferCreateInfo info = { };
      info.size   = align(icbSizeInBytes, 256u);
      info.usage  = VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
                  | VK_BUFFER_USAGE_TRANSFER_SRC_BIT
                  | VK_BUFFER_USAGE_TRANSFER_DST_BIT;
      info.stages = VK_PIPELINE_STAGE_2_TRANSFER_BIT
                  | util::pipelineStages(ShaderKey.stage());
      info.access = VK_ACCESS_UNIFORM_READ_BIT
                  | VK_ACCESS_TRANSFER_READ_BIT
                  | VK_ACCESS_TRANSFER_WRITE_BIT;
      info.debugName = "Icb";

      m_buffer = pDevice->GetDXVKDevice()->createBuffer(info, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);

      pDevice->InitShaderIcb(this, icbSizeInBytes, Icb.data);
    }

    // Materialise the debug name once and reuse it for both the lookup and
    // the (possible) insert path. createCachedShader takes the name by
    // const std::string&, so a single std::string covers both calls and
    // avoids one heap allocation + one hex-format pass per cache miss.
    const std::string shaderName = ShaderKey.toString();

    m_shader = pDevice->GetDXVKDevice()->createCachedShader(
      shaderName, ModuleInfo, nullptr);

    if (m_shader == nullptr) {
      Rc<D3D11ShaderConverter> converter = new D3D11ShaderConverter(
        ShaderKey, ModuleInfo, pShaderBytecode, BytecodeLength, m_buffer != nullptr);

      m_shader = pDevice->GetDXVKDevice()->createCachedShader(
        shaderName, ModuleInfo, std::move(converter));
    }
  }


  void D3D11CommonShader::GatherInterefaceInfo(
          D3D11ClassLinkage*      pLinkage,
    const void*                   pShaderBytecode,
          size_t                  BytecodeLength) {
    dxbc_spv::dxbc::Container container(pShaderBytecode, BytecodeLength);
    dxbc_spv::util::ByteReader ifaceChunk(container.getInterfaceChunk());

    if (!ifaceChunk)
      return;

    dxbc_spv::dxbc::InterfaceChunk ifaceInfo(ifaceChunk);

    if (!ifaceInfo)
      return;

    auto typeInfos = ifaceInfo.getClassTypes();
    auto slotInfos = ifaceInfo.getInterfaceSlots();

    for (auto i = typeInfos.first; i != typeInfos.second; i++) {
      const char* typeName = i->name.c_str();
      m_interfaces.AddType(i->id, typeName);
      pLinkage->AddType(typeName, i->cbSize, i->srvCount, i->samplerCount);
    }

    for (auto i = slotInfos.first; i != slotInfos.second; i++) {
      const uint32_t slotIndex = i->index;
      const uint32_t slotCount = i->count;
      for (const auto& e : i->entries)
        m_interfaces.AddSlotInfo(slotIndex, slotCount, e.typeId, e.tableId);
    }

    auto instances = ifaceInfo.getClassInstances();

    for (auto i = instances.first; i != instances.second; i++) {
      D3D11_CLASS_INSTANCE_DESC desc = { };
      desc.ConstantBuffer           = i->cbvIndex;
      desc.BaseConstantBufferOffset = i->cbvOffset;
      desc.BaseTexture              = i->srvIndex & 0x7fu;
      desc.BaseSampler              = i->samplerIndex & 0xfu;

      auto typeName = m_interfaces.GetTypeName(i->typeId);

      if (typeName)
        pLinkage->AddInstance(&desc, typeName, i->name.c_str());
    }
  }


  D3D11ShaderModuleSet:: D3D11ShaderModuleSet() = default;
  D3D11ShaderModuleSet::~D3D11ShaderModuleSet() = default;


  HRESULT D3D11ShaderModuleSet::GetShaderModule(
          D3D11Device*            pDevice,
          D3D11ClassLinkage*      pLinkage,
    const DxvkShaderHash&         ShaderKey,
    const DxvkIrShaderCreateInfo& ModuleInfo,
    const void*                   pShaderBytecode,
          size_t                  BytecodeLength,
    const D3D11ShaderIcbInfo&     Icb,
    const D3D11BindingMask&       BindingMask,
          D3D11CommonShader*      pShader) {
    // Fast path: lookup under the shared cache lock.
    { std::lock_guard<dxvk::mutex> lock(m_mutex);

      auto entry = m_modules.find(ShaderKey);
      if (entry != m_modules.end()) {
        *pShader = entry->second;
        return S_OK;
      }
    }

    // Slow path: this shader has not been compiled yet. Compile outside the
    // lock so other threads can keep using the cache during the (potentially
    // long) DXBC -> IR translation.
    D3D11CommonShader module;

    try {
      module = D3D11CommonShader(pDevice, pLinkage, ShaderKey,
        ModuleInfo, pShaderBytecode, BytecodeLength, Icb, BindingMask);
    } catch (const DxvkError& e) {
      Logger::err(e.message());
      return E_INVALIDARG;
    }

    // Insert the new module. If another thread compiled the same shader
    // in the meantime we discard ours and return theirs. try_emplace is
    // guaranteed not to move-from the argument when the key is already
    // present, so the local 'module' remains valid (and is harmlessly
    // destroyed at scope exit) in the duplicate-compile case.
    { std::lock_guard<dxvk::mutex> lock(m_mutex);

      auto status = m_modules.try_emplace(ShaderKey, std::move(module));
      *pShader = status.first->second;
      return S_OK;
    }
  }

}
