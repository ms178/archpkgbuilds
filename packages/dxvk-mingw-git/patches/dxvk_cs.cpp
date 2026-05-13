#include "dxvk_cs.h"

namespace dxvk {

  DxvkCsChunk::DxvkCsChunk() {

  }


  DxvkCsChunk::~DxvkCsChunk() {
    this->reset();
  }


  void DxvkCsChunk::init(DxvkCsChunkFlags flags) {
    m_flags = flags;
  }


  void DxvkCsChunk::executeAll(DxvkContext* ctx) {
    auto cmd = m_head;

    if (m_flags.test(DxvkCsChunkFlag::SingleUse)) {
      // The chunk is logically empty as soon as we start consuming it.
      // Reset the offset up front so reuse paths see a clean state.
      m_commandOffset = 0;

      while (cmd != nullptr) {
        // Capture the next pointer before exec(): in the SingleUse path
        // we destroy the current command afterwards, so its m_next field
        // is no longer accessible. Issue a software prefetch for the next
        // command's first cache line (vtable + next pointer) while the
        // current command's exec() body runs -- this overlaps the cold
        // L2/L3/DRAM miss with useful work and consistently reduces the
        // indirect-call vtable-load latency on Raptor Lake.
        auto next = cmd->next();

        if (next != nullptr)
          __builtin_prefetch(next, 0, 1);

        cmd->exec(ctx);
        cmd->~DxvkCsCmd();
        cmd = next;
      }

      m_head = nullptr;
      m_next = &m_head;
    } else {
      // Replay path (deferred-context command lists). The chunk is reused,
      // so we do not destroy commands here. Hoist the next-pointer load
      // above exec() so we can prefetch it; this is safe because exec()
      // does not modify the command's m_next field.
      while (cmd != nullptr) {
        auto next = cmd->next();

        if (next != nullptr)
          __builtin_prefetch(next, 0, 1);

        cmd->exec(ctx);
        cmd = next;
      }
    }
  }


  void DxvkCsChunk::reset() {
    auto cmd = m_head;

    while (cmd != nullptr) {
      auto next = cmd->next();
      cmd->~DxvkCsCmd();
      cmd = next;
    }

    m_head = nullptr;
    m_next = &m_head;

    m_commandOffset = 0;
  }


  DxvkCsChunkPool::DxvkCsChunkPool() {

  }


  DxvkCsChunkPool::~DxvkCsChunkPool() {
    for (DxvkCsChunk* chunk : m_chunks)
      delete chunk;
  }


  DxvkCsChunk* DxvkCsChunkPool::allocChunk(DxvkCsChunkFlags flags) {
    DxvkCsChunk* chunk = nullptr;

    { std::lock_guard<dxvk::mutex> lock(m_mutex);

      if (m_chunks.size() != 0) {
        chunk = m_chunks.back();
        m_chunks.pop_back();
      }
    }

    if (!chunk)
      chunk = new DxvkCsChunk();

    chunk->init(flags);
    return chunk;
  }


  void DxvkCsChunkPool::freeChunk(DxvkCsChunk* chunk) {
    chunk->reset();

    std::lock_guard<dxvk::mutex> lock(m_mutex);
    m_chunks.push_back(chunk);
  }


  DxvkCsThread::DxvkCsThread(
    const Rc<DxvkDevice>&   device,
    const Rc<DxvkContext>&  context)
  : m_device(device), m_context(context),
    m_thread([this] { threadFunc(); }) {

  }


  DxvkCsThread::~DxvkCsThread() {
    { std::unique_lock<dxvk::mutex> lock(m_mutex);
      m_stopped.store(true);
    }

    m_condOnAdd.notify_one();
    m_thread.join();
  }


  uint64_t DxvkCsThread::dispatchChunk(DxvkCsChunkRef&& chunk) {
    uint64_t seq;

    { std::unique_lock<dxvk::mutex> lock(m_mutex);
      seq = ++m_queueOrdered.seqDispatch;

      auto& entry = m_queueOrdered.queue.emplace_back();
      entry.chunk = std::move(chunk);
      entry.seq = seq;
    }

    // Signal the worker after releasing the queue mutex. Notifying under
    // the lock causes the woken thread to immediately block again on the
    // same mutex (lost-wakeup avoidance is preserved by the fact that the
    // queue mutation above happened under the same mutex the waiter uses
    // to evaluate its predicate).
    m_condOnAdd.notify_one();

    return seq;
  }


  void DxvkCsThread::injectChunk(DxvkCsQueue queue, DxvkCsChunkRef&& chunk, bool synchronize) {
    uint64_t timeline = 0u;

    { std::unique_lock<dxvk::mutex> lock(m_mutex);
      auto& q = getQueue(queue);

      if (synchronize)
        timeline = ++q.seqDispatch;

      auto& entry = q.queue.emplace_back();
      entry.chunk = std::move(chunk);
      entry.seq = timeline;

      if (queue == DxvkCsQueue::HighPriority) {
        // Worker will check this flag after executing any
        // chunk without causing additional lock contention
        m_hasHighPrio.store(true, std::memory_order_release);
      }
    }

    // See dispatchChunk() for the rationale on notifying outside the lock.
    m_condOnAdd.notify_one();

    if (synchronize) {
      std::unique_lock<dxvk::mutex> lock(m_counterMutex);

      m_condOnSync.wait(lock, [this, queue, timeline] {
        return getCounter(queue).load(std::memory_order_acquire) >= timeline;
      });
    }
  }


  void DxvkCsThread::synchronize(uint64_t seq) {
    // Avoid locking if we know the sync is a no-op, may
    // reduce overhead if this is being called frequently
    if (seq > m_seqOrdered.load(std::memory_order_acquire)) {
      // We don't need to lock the queue here, if synchronization
      // happens while another thread is submitting then there is
      // an inherent race anyway
      if (seq == SynchronizeAll)
        seq = m_queueOrdered.seqDispatch;

      auto t0 = dxvk::high_resolution_clock::now();

      { std::unique_lock<dxvk::mutex> lock(m_counterMutex);
        m_condOnSync.wait(lock, [this, seq] {
          return m_seqOrdered.load(std::memory_order_acquire) >= seq;
        });
      }

      auto t1 = dxvk::high_resolution_clock::now();
      auto ticks = std::chrono::duration_cast<std::chrono::microseconds>(t1 - t0);

      m_device->addStatCtr(DxvkStatCounter::CsSyncCount, 1);
      m_device->addStatCtr(DxvkStatCounter::CsSyncTicks, ticks.count());
    }
  }


  void DxvkCsThread::threadFunc() {
    env::setThreadName("dxvk-cs");

    // Local chunk queues, we use two queues and swap between
    // them in order to potentially reduce lock contention.
    std::vector<DxvkCsQueuedChunk> ordered;
    std::vector<DxvkCsQueuedChunk> highPrio;

    try {
      while (!m_stopped.load()) {
        { std::unique_lock<dxvk::mutex> lock(m_mutex);

          auto pred = [this] { return
              !m_queueOrdered.queue.empty()
              || !m_queueHighPrio.queue.empty()
              || m_stopped.load();
          };

          if (unlikely(!pred())) {
            auto t0 = dxvk::high_resolution_clock::now();

            m_condOnAdd.wait(lock, [&] {
              return pred();
            });

            auto t1 = dxvk::high_resolution_clock::now();
            m_device->addStatCtr(DxvkStatCounter::CsIdleTicks, std::chrono::duration_cast<std::chrono::microseconds>(t1 - t0).count());
          }

          std::swap(ordered, m_queueOrdered.queue);
          std::swap(highPrio, m_queueHighPrio.queue);

          m_hasHighPrio.store(false, std::memory_order_release);
        }

        size_t orderedIndex = 0u;
        size_t highPrioIndex = 0u;

        while (highPrioIndex < highPrio.size() || orderedIndex < ordered.size()) {
          // Re-fill local high-priority queue if the app has queued anything up
          // in the meantime, we want to reduce possible synchronization delays.
          if (highPrioIndex >= highPrio.size() && m_hasHighPrio.load(std::memory_order_acquire)) {
            highPrio.clear();
            highPrioIndex = 0u;

            std::unique_lock<dxvk::mutex> lock(m_mutex);
            std::swap(highPrio, m_queueHighPrio.queue);

            m_hasHighPrio.store(false, std::memory_order_release);
          }

          // Drain high-priority queue first
          bool isHighPrio = highPrioIndex < highPrio.size();
          auto& entry = isHighPrio ? highPrio[highPrioIndex++] : ordered[orderedIndex++];

          m_context->addStatCtr(DxvkStatCounter::CsChunkCount, 1);

          entry.chunk->executeAll(m_context.ptr());

          if (entry.seq) {
            // Use a separate mutex for the chunk counter, this will only
            // ever be contested if synchronization is actually necessary.
            // Publish the new counter under the mutex, then notify after
            // releasing it so the woken waiter does not immediately block
            // again on the same mutex.
            {
              std::lock_guard lock(m_counterMutex);

              auto& counter = isHighPrio ? m_seqHighPrio : m_seqOrdered;
              counter.store(entry.seq, std::memory_order_release);
            }

            m_condOnSync.notify_one();
          }

          // Immediately free the chunk to release
          // references to any resources held by it
          entry.chunk = DxvkCsChunkRef();
        }

        ordered.clear();
        highPrio.clear();
      }
    } catch (const DxvkError& e) {
      Logger::err("Exception on CS thread!");
      Logger::err(e.message());
    }
  }

}
