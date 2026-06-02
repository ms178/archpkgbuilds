#include "dxvk_device.h"
#include "dxvk_gpl_async_cache.h"
#include "dxvk_pipemanager.h"

#include "../util/sha1/sha1_util.h"
#include "../util/util_env.h"

#include <cstring>

namespace dxvk {

  static const DxvkGplAsyncShaderKey g_nullShaderKey = { };


  DxvkGplAsyncCache::DxvkGplAsyncCache(
          DxvkDevice*           device,
          DxvkPipelineManager*  pipeManager,
          DxvkPipelineWorkers*  pipeWorkers)
  : m_device      (device),
    m_pipeManager (pipeManager),
    m_pipeWorkers (pipeWorkers) {
    std::string useCache = env::getEnvVar("DXVK_GPL_ASYNC_CACHE");

    if (useCache.empty())
      useCache = env::getEnvVar("DXVK_GPLASYNCCACHE");

    m_enable = useCache != "0"
            && useCache != "disable"
            && device->config().gplAsyncCache;

    if (!m_enable)
      return;

    bool newFile = useCache == "reset" || !readCacheFile();

    if (newFile) {
      auto file = openCacheFileForWrite(true);

      for (const auto& entry : m_entries)
        writeCacheEntry(file, entry);
    }
  }


  DxvkGplAsyncCache::~DxvkGplAsyncCache() {
    stopWorkers();
  }


  void DxvkGplAsyncCache::addGraphicsPipeline(
    const DxvkGraphicsPipelineShaders&   shaders,
    const DxvkGraphicsPipelineStateInfo& state) {
    if (!m_enable)
      return;

    DxvkGplAsyncPipelineKey key = getPipelineKey(shaders);

    if (key.vs.isNull())
      return;

    { std::lock_guard<dxvk::mutex> lock(m_entryLock);
      auto range = m_entryMap.equal_range(key);

      for (auto i = range.first; i != range.second; i++) {
        const auto& entry = m_entries[i->second];

        if (entry.state.eq(state))
          return;
      }
    }

    DxvkGplAsyncCacheEntry entry = { };
    entry.shaders = key;
    entry.state = state;

    { std::lock_guard<dxvk::mutex> lock(m_writerLock);
      m_writerQueue.push(entry);
      m_writerCond.notify_one();
      createWriter();
    }
  }


  void DxvkGplAsyncCache::registerShader(const Rc<DxvkShader>& shader) {
    if (!m_enable || shader == nullptr)
      return;

    DxvkGplAsyncShaderKey shaderKey = getShaderKey(shader);

    if (shaderKey.isNull())
      return;

    std::unique_lock<dxvk::mutex> entryLock(m_entryLock);
    m_shaderMap.insert_or_assign(shaderKey, shader);

    std::unique_lock<dxvk::mutex> workerLock;
    auto range = m_pipelineMap.equal_range(shaderKey);

    for (auto i = range.first; i != range.second; i++) {
      DxvkGraphicsPipelineShaders shaders;

      if (!getShaderByKey(i->second.vs,  shaders.vs)
       || !getShaderByKey(i->second.tcs, shaders.tcs)
       || !getShaderByKey(i->second.tes, shaders.tes)
       || !getShaderByKey(i->second.gs,  shaders.gs)
       || !getShaderByKey(i->second.fs,  shaders.fs))
        continue;

      if (!workerLock)
        workerLock = std::unique_lock<dxvk::mutex>(m_workerLock);

      queuePipelineLocked(i->second, shaders);
    }

    if (workerLock) {
      m_workerCond.notify_all();
      createWorker();
    }
  }


  void DxvkGplAsyncCache::stopWorkers() {
    { std::lock_guard<dxvk::mutex> workerLock(m_workerLock);
      std::lock_guard<dxvk::mutex> writerLock(m_writerLock);

      if (m_stopThreads.exchange(true))
        return;

      m_workerCond.notify_all();
      m_writerCond.notify_all();
    }

    if (m_workerThread.joinable())
      m_workerThread.join();

    if (m_writerThread.joinable())
      m_writerThread.join();
  }


  DxvkGplAsyncPipelineKey DxvkGplAsyncCache::getPipelineKey(
    const DxvkGraphicsPipelineShaders& shaders) const {
    DxvkGplAsyncPipelineKey key = { };
    key.vs  = getShaderKey(shaders.vs);
    key.tcs = getShaderKey(shaders.tcs);
    key.tes = getShaderKey(shaders.tes);
    key.gs  = getShaderKey(shaders.gs);
    key.fs  = getShaderKey(shaders.fs);
    return key;
  }


  DxvkGplAsyncShaderKey DxvkGplAsyncCache::getShaderKey(
    const Rc<DxvkShader>& shader) const {
    DxvkGplAsyncShaderKey key = { };

    if (shader == nullptr)
      return key;

    std::string name = shader->debugName();

    if (name.empty())
      return key;

    Sha1Hash hash = Sha1Hash::compute(name.data(), name.size());
    std::memcpy(key.hash.data(), hash.digest(), key.hash.size());
    return key;
  }


  bool DxvkGplAsyncCache::getShaderByKey(
    const DxvkGplAsyncShaderKey& key,
          Rc<DxvkShader>&        shader) const {
    if (key.isNull())
      return true;

    auto entry = m_shaderMap.find(key);

    if (entry == m_shaderMap.end())
      return false;

    shader = entry->second;
    return true;
  }


  void DxvkGplAsyncCache::mapEntryLocked(
    const DxvkGplAsyncCacheEntry& entry,
          size_t                  entryId) {
    m_entryMap.insert({ entry.shaders, entryId });

    mapShaderToPipelineLocked(entry.shaders.vs,  entry.shaders);
    mapShaderToPipelineLocked(entry.shaders.tcs, entry.shaders);
    mapShaderToPipelineLocked(entry.shaders.tes, entry.shaders);
    mapShaderToPipelineLocked(entry.shaders.gs,  entry.shaders);
    mapShaderToPipelineLocked(entry.shaders.fs,  entry.shaders);
  }


  void DxvkGplAsyncCache::mapShaderToPipelineLocked(
    const DxvkGplAsyncShaderKey&   shader,
    const DxvkGplAsyncPipelineKey& key) {
    if (!shader.isNull())
      m_pipelineMap.insert({ shader, key });
  }


  void DxvkGplAsyncCache::queuePipelineLocked(
    const DxvkGplAsyncPipelineKey&     key,
    const DxvkGraphicsPipelineShaders& shaders) {
    if (!m_queuedPipelines.insert(key).second)
      return;

    m_workerQueue.push({ key, shaders });
  }


  void DxvkGplAsyncCache::compilePipelines(const WorkerItem& item) {
    DxvkGraphicsPipeline* pipeline = nullptr;

    std::vector<DxvkGraphicsPipelineStateInfo> states;

    { std::lock_guard<dxvk::mutex> lock(m_entryLock);
      auto range = m_entryMap.equal_range(item.key);

      for (auto i = range.first; i != range.second; i++)
        states.push_back(m_entries[i->second].state);
    }

    for (const auto& state : states) {
      if (!pipeline)
        pipeline = m_pipeManager->createGraphicsPipeline(item.shaders);

      if (pipeline)
        m_pipeWorkers->compileGraphicsPipeline(pipeline, state, DxvkPipelinePriority::Low);
    }
  }


  bool DxvkGplAsyncCache::readCacheFile() {
    std::ifstream file = openCacheFileForRead();

    if (!file)
      return true;

    DxvkGplAsyncCacheHeader header;

    if (!readCacheHeader(file, header)) {
      Logger::warn("DXVK: Failed to read GPL async cache header");
      return false;
    }

    uint32_t invalidEntries = 0u;

    while (file) {
      DxvkGplAsyncCacheEntry entry;

      if (readCacheEntry(file, entry)) {
        std::lock_guard<dxvk::mutex> lock(m_entryLock);
        size_t entryId = m_entries.size();
        m_entries.push_back(entry);
        mapEntryLocked(entry, entryId);
      } else if (file) {
        invalidEntries += 1u;
      }
    }

    Logger::info(str::format("DXVK: Read ", m_entries.size(), " valid GPL async cache entries"));

    if (invalidEntries) {
      Logger::warn(str::format("DXVK: Skipped ", invalidEntries, " invalid GPL async cache entries"));
      return false;
    }

    return true;
  }


  bool DxvkGplAsyncCache::readCacheHeader(
          std::istream&            stream,
          DxvkGplAsyncCacheHeader& header) const {
    DxvkGplAsyncCacheHeader expected;

    if (!stream.read(reinterpret_cast<char*>(&header), sizeof(header)))
      return false;

    return std::memcmp(header.magic, expected.magic, sizeof(header.magic)) == 0
        && header.version == expected.version
        && header.keySize == expected.keySize
        && header.stateSize == expected.stateSize;
  }


  bool DxvkGplAsyncCache::readCacheEntry(
          std::istream&             stream,
          DxvkGplAsyncCacheEntry&   entry) const {
    DxvkGplAsyncCacheRecordHeader header;

    if (!stream.read(reinterpret_cast<char*>(&header), sizeof(header)))
      return false;

    if (header.dataSize != sizeof(entry)) {
      if (header.dataSize > (1u << 20u)) {
        stream.setstate(std::ios_base::failbit);
      } else {
        stream.seekg(header.dataSize, std::ios_base::cur);
      }

      return false;
    }

    if (!stream.read(reinterpret_cast<char*>(&entry), sizeof(entry)))
      return false;

    Sha1Hash hash = Sha1Hash::compute(entry);
    return std::memcmp(header.hash.data(), hash.digest(), header.hash.size()) == 0
        && !entry.shaders.vs.isNull();
  }


  void DxvkGplAsyncCache::writeCacheEntry(
          std::ostream&                stream,
    const DxvkGplAsyncCacheEntry&      entry) const {
    DxvkGplAsyncCacheRecordHeader header;
    Sha1Hash hash = Sha1Hash::compute(entry);
    std::memcpy(header.hash.data(), hash.digest(), header.hash.size());

    stream.write(reinterpret_cast<const char*>(&header), sizeof(header));
    stream.write(reinterpret_cast<const char*>(&entry), sizeof(entry));
    stream.flush();
  }


  void DxvkGplAsyncCache::workerFunc() {
    env::setThreadName("dxvk-gplcache");

    while (!m_stopThreads.load()) {
      WorkerItem item;

      { std::unique_lock<dxvk::mutex> lock(m_workerLock);
        m_workerCond.wait(lock, [this] {
          return !m_workerQueue.empty() || m_stopThreads.load();
        });

        if (m_workerQueue.empty())
          break;

        item = std::move(m_workerQueue.front());
        m_workerQueue.pop();
      }

      compilePipelines(item);
    }
  }


  void DxvkGplAsyncCache::writerFunc() {
    env::setThreadName("dxvk-gplwrite");

    std::ofstream file;

    while (!m_stopThreads.load()) {
      DxvkGplAsyncCacheEntry entry;

      { std::unique_lock<dxvk::mutex> lock(m_writerLock);
        m_writerCond.wait(lock, [this] {
          return !m_writerQueue.empty() || m_stopThreads.load();
        });

        if (m_writerQueue.empty())
          break;

        entry = m_writerQueue.front();
        m_writerQueue.pop();
      }

      { std::lock_guard<dxvk::mutex> lock(m_entryLock);
        auto range = m_entryMap.equal_range(entry.shaders);
        bool exists = false;

        for (auto i = range.first; i != range.second && !exists; i++)
          exists = m_entries[i->second].state.eq(entry.state);

        if (exists)
          continue;

        size_t entryId = m_entries.size();
        m_entries.push_back(entry);
        mapEntryLocked(entry, entryId);
      }

      if (!file.is_open())
        file = openCacheFileForWrite(false);

      if (file)
        writeCacheEntry(file, entry);
    }
  }


  void DxvkGplAsyncCache::createWorker() {
    if (!m_workerThread.joinable())
      m_workerThread = dxvk::thread([this] { workerFunc(); });
  }


  void DxvkGplAsyncCache::createWriter() {
    if (!m_writerThread.joinable())
      m_writerThread = dxvk::thread([this] { writerFunc(); });
  }


  str::path_string DxvkGplAsyncCache::getCacheFileName() const {
    std::string path = getCacheDir();

    if (!path.empty() && *path.rbegin() != '/')
      path += '/';

    path += env::getExeBaseName() + ".dxvk-gplasync-cache";
    return str::topath(path.c_str());
  }


  std::ifstream DxvkGplAsyncCache::openCacheFileForRead() const {
    return std::ifstream(getCacheFileName().c_str(), std::ios_base::binary);
  }


  std::ofstream DxvkGplAsyncCache::openCacheFileForWrite(bool recreate) const {
    std::ofstream file;

    if (!recreate)
      recreate = !openCacheFileForRead();

    if (recreate) {
      file = std::ofstream(getCacheFileName().c_str(), std::ios_base::binary | std::ios_base::trunc);

      if (!file && env::createDirectory(getCacheDir()))
        file = std::ofstream(getCacheFileName().c_str(), std::ios_base::binary | std::ios_base::trunc);
    } else {
      file = std::ofstream(getCacheFileName().c_str(), std::ios_base::binary | std::ios_base::app);
    }

    if (file && recreate) {
      DxvkGplAsyncCacheHeader header;
      file.write(reinterpret_cast<const char*>(&header), sizeof(header));
    }

    return file;
  }


  std::string DxvkGplAsyncCache::getCacheDir() const {
    std::string result = env::getEnvVar("DXVK_GPL_ASYNC_CACHE_PATH");

    if (result.empty())
      result = env::getEnvVar("DXVK_STATE_CACHE_PATH");

    return result;
  }

}
