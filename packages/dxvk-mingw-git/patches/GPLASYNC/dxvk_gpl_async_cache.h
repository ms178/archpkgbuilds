#pragma once

#include <atomic>
#include <condition_variable>
#include <fstream>
#include <mutex>
#include <queue>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "dxvk_gpl_async_cache_types.h"
#include "dxvk_graphics.h"

#include "../util/thread.h"

namespace dxvk {

  class DxvkDevice;
  class DxvkPipelineManager;
  class DxvkPipelineWorkers;

  class DxvkGplAsyncCache {

  public:

    DxvkGplAsyncCache(
            DxvkDevice*           device,
            DxvkPipelineManager*  pipeManager,
            DxvkPipelineWorkers*  pipeWorkers);

    ~DxvkGplAsyncCache();

    void addGraphicsPipeline(
      const DxvkGraphicsPipelineShaders&   shaders,
      const DxvkGraphicsPipelineStateInfo& state);

    void registerShader(
      const Rc<DxvkShader>&                shader);

    void stopWorkers();

  private:

    struct WorkerItem {
      DxvkGplAsyncPipelineKey key = { };
      DxvkGraphicsPipelineShaders shaders = { };
    };

    struct KeyHash {
      size_t operator () (const DxvkGplAsyncPipelineKey& key) const {
        return key.hash();
      }
    };

    struct KeyEq {
      bool operator () (const DxvkGplAsyncPipelineKey& a, const DxvkGplAsyncPipelineKey& b) const {
        return a.eq(b);
      }
    };

    struct ShaderKeyHash {
      size_t operator () (const DxvkGplAsyncShaderKey& key) const {
        return key.hashValue();
      }
    };

    struct ShaderKeyEq {
      bool operator () (const DxvkGplAsyncShaderKey& a, const DxvkGplAsyncShaderKey& b) const {
        return a.eq(b);
      }
    };

    DxvkDevice*                         m_device      = nullptr;
    DxvkPipelineManager*                m_pipeManager = nullptr;
    DxvkPipelineWorkers*                m_pipeWorkers = nullptr;
    bool                                m_enable      = false;

    dxvk::mutex                         m_entryLock;
    std::vector<DxvkGplAsyncCacheEntry> m_entries;

    std::unordered_multimap<DxvkGplAsyncPipelineKey, size_t, KeyHash, KeyEq> m_entryMap;
    std::unordered_multimap<DxvkGplAsyncShaderKey, DxvkGplAsyncPipelineKey, ShaderKeyHash, ShaderKeyEq> m_pipelineMap;
    std::unordered_map<DxvkGplAsyncShaderKey, Rc<DxvkShader>, ShaderKeyHash, ShaderKeyEq> m_shaderMap;
    std::unordered_set<DxvkGplAsyncPipelineKey, KeyHash, KeyEq> m_queuedPipelines;

    dxvk::mutex                         m_workerLock;
    dxvk::condition_variable            m_workerCond;
    std::queue<WorkerItem>              m_workerQueue;
    dxvk::thread                        m_workerThread;

    dxvk::mutex                         m_writerLock;
    dxvk::condition_variable            m_writerCond;
    std::queue<DxvkGplAsyncCacheEntry>  m_writerQueue;
    dxvk::thread                        m_writerThread;

    std::atomic<bool>                   m_stopThreads = { false };

    DxvkGplAsyncPipelineKey getPipelineKey(const DxvkGraphicsPipelineShaders& shaders) const;
    DxvkGplAsyncShaderKey getShaderKey(const Rc<DxvkShader>& shader) const;
    bool getShaderByKey(const DxvkGplAsyncShaderKey& key, Rc<DxvkShader>& shader) const;

    void mapEntryLocked(const DxvkGplAsyncCacheEntry& entry, size_t entryId);
    void mapShaderToPipelineLocked(const DxvkGplAsyncShaderKey& shader, const DxvkGplAsyncPipelineKey& key);
    void queuePipelineLocked(const DxvkGplAsyncPipelineKey& key, const DxvkGraphicsPipelineShaders& shaders);
    void compilePipelines(const WorkerItem& item);

    bool readCacheFile();
    bool readCacheHeader(std::istream& stream, DxvkGplAsyncCacheHeader& header) const;
    bool readCacheEntry(std::istream& stream, DxvkGplAsyncCacheEntry& entry) const;
    void writeCacheEntry(std::ostream& stream, const DxvkGplAsyncCacheEntry& entry) const;

    void workerFunc();
    void writerFunc();
    void createWorker();
    void createWriter();

    str::path_string getCacheFileName() const;
    std::ifstream openCacheFileForRead() const;
    std::ofstream openCacheFileForWrite(bool recreate) const;
    std::string getCacheDir() const;

  };

}
