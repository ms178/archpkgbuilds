#pragma once

#include <array>
#include <cstdint>

#include "dxvk_graphics_state.h"
#include "dxvk_hash.h"

namespace dxvk {

  struct DxvkGplAsyncShaderKey {
    std::array<uint8_t, 20> hash = { };

    bool eq(const DxvkGplAsyncShaderKey& other) const {
      return hash == other.hash;
    }

    size_t hashValue() const {
      DxvkHashState state;

      for (uint32_t i = 0; i < hash.size(); i += 4u) {
        uint32_t value = uint32_t(hash[i + 0u])
                       | uint32_t(hash[i + 1u]) << 8u
                       | uint32_t(hash[i + 2u]) << 16u
                       | uint32_t(hash[i + 3u]) << 24u;
        state.add(value);
      }

      return state;
    }

    bool isNull() const {
      return *this == DxvkGplAsyncShaderKey();
    }

    bool operator == (const DxvkGplAsyncShaderKey& other) const {
      return eq(other);
    }
  };


  struct DxvkGplAsyncPipelineKey {
    DxvkGplAsyncShaderKey vs;
    DxvkGplAsyncShaderKey tcs;
    DxvkGplAsyncShaderKey tes;
    DxvkGplAsyncShaderKey gs;
    DxvkGplAsyncShaderKey fs;

    bool eq(const DxvkGplAsyncPipelineKey& other) const {
      return vs.eq(other.vs)
          && tcs.eq(other.tcs)
          && tes.eq(other.tes)
          && gs.eq(other.gs)
          && fs.eq(other.fs);
    }

    size_t hash() const {
      DxvkHashState state;
      state.add(vs.hashValue());
      state.add(tcs.hashValue());
      state.add(tes.hashValue());
      state.add(gs.hashValue());
      state.add(fs.hashValue());
      return state;
    }

    bool operator == (const DxvkGplAsyncPipelineKey& other) const {
      return eq(other);
    }
  };


  struct DxvkGplAsyncCacheEntry {
    DxvkGplAsyncPipelineKey       shaders = { };
    DxvkGraphicsPipelineStateInfo state = { };
  };


  struct DxvkGplAsyncCacheHeader {
    char     magic[4]  = { 'D', 'X', 'G', 'A' };
    uint32_t version   = 1u;
    uint32_t keySize   = sizeof(DxvkGplAsyncPipelineKey);
    uint32_t stateSize = sizeof(DxvkGraphicsPipelineStateInfo);
  };


  struct DxvkGplAsyncCacheRecordHeader {
    uint32_t dataSize = sizeof(DxvkGplAsyncCacheEntry);
    std::array<uint8_t, 20> hash = { };
  };

}
