#include <atomic>
#include <limits>
#include <system_error>
#include <vector>

#include "thread.h"
#include "util_bit.h"
#include "util_likely.h"

#ifdef _WIN32

#ifndef SYSTEM_CPU_SET_INFORMATION_ALLOCATED_FLAG
#define SYSTEM_CPU_SET_INFORMATION_ALLOCATED_FLAG 0x2
#endif
#ifndef SYSTEM_CPU_SET_INFORMATION_PARKED_FLAG
#define SYSTEM_CPU_SET_INFORMATION_PARKED_FLAG    0x1
#endif

namespace dxvk {

  namespace {

    constexpr SIZE_T DefaultStackReserve = 1u << 20;

    struct CpuSetEntry {
      ULONG id = 0u;
      BYTE  efficiencyClass = 0u;
    };

    struct CpuSetDatabase {
      std::vector<ULONG> perfSets;
      std::vector<ULONG> effSets;

      using PFN_GetCpuInfo = BOOL (WINAPI*)(PSYSTEM_CPU_SET_INFORMATION, ULONG, PULONG, HANDLE, ULONG);
      using PFN_SetThreadSets = BOOL (WINAPI*)(HANDLE, const ULONG*, ULONG);

      PFN_GetCpuInfo    pGetInfo = nullptr;
      PFN_SetThreadSets pSetSets = nullptr;
      bool              available = false;

      CpuSetDatabase() {
        init();
      }

    private:

      void init() {
        HMODULE k32 = ::GetModuleHandleW(L"kernel32.dll");
        if (!k32)
          return;

        pGetInfo = reinterpret_cast<PFN_GetCpuInfo>(
          ::GetProcAddress(k32, "GetSystemCpuSetInformation"));
        pSetSets = reinterpret_cast<PFN_SetThreadSets>(
          ::GetProcAddress(k32, "SetThreadSelectedCpuSets"));

        if (!pGetInfo || !pSetSets)
          return;

        ULONG len = 0;
        if (!pGetInfo(nullptr, 0u, &len, ::GetCurrentProcess(), 0u)) {
          if (::GetLastError() != ERROR_INSUFFICIENT_BUFFER)
            return;
        }

        if (!len)
          return;

        std::vector<char> buf(len);
        if (!pGetInfo(reinterpret_cast<PSYSTEM_CPU_SET_INFORMATION>(buf.data()),
                      len, &len, ::GetCurrentProcess(), 0u))
          return;

        std::vector<CpuSetEntry> usable;
        usable.reserve(static_cast<size_t>(len) / sizeof(SYSTEM_CPU_SET_INFORMATION));

        ULONG off = 0;
        BYTE minClass = std::numeric_limits<BYTE>::max();
        BYTE maxClass = std::numeric_limits<BYTE>::min();

        while (off < len) {
          auto* info = reinterpret_cast<PSYSTEM_CPU_SET_INFORMATION>(buf.data() + off);

          if (!info->Size || info->Size > len - off)
            break;

          if (info->Type == CpuSetInformation) {
            const auto& cs = info->CpuSet;

            const bool isUsable =
                 (cs.AllFlags & SYSTEM_CPU_SET_INFORMATION_ALLOCATED_FLAG)
              && !(cs.AllFlags & SYSTEM_CPU_SET_INFORMATION_PARKED_FLAG);

            if (isUsable) {
              CpuSetEntry e;
              e.id = cs.Id;
              e.efficiencyClass = cs.EfficiencyClass;
              usable.push_back(e);

              minClass = std::min(minClass, e.efficiencyClass);
              maxClass = std::max(maxClass, e.efficiencyClass);
            }
          }

          off += info->Size;
        }

        if (usable.empty())
          return;

        perfSets.reserve(usable.size());
        effSets.reserve(usable.size());

        // Windows docs: higher EfficiencyClass => higher performance.
        for (const auto& e : usable) {
          if (e.efficiencyClass == maxClass)
            perfSets.push_back(e.id);
          if (e.efficiencyClass == minClass)
            effSets.push_back(e.id);
        }

        // Homogeneous fallback: keep one populated vector.
        if (perfSets.empty() && !effSets.empty())
          perfSets = effSets;
        if (effSets.empty() && !perfSets.empty())
          effSets = perfSets;

        available = !perfSets.empty() || !effSets.empty();
      }
    };

    CpuSetDatabase& cpuDb() {
      static CpuSetDatabase db;
      return db;
    }

    void applyCpuSetAffinity(HANDLE threadHandle, ThreadPriority prio) {
      static CpuSetDatabase& db = cpuDb();

      if (!db.available)
        return;

      const std::vector<ULONG>& preferred =
        (prio == ThreadPriority::Lowest) ? db.effSets : db.perfSets;
      const std::vector<ULONG>& fallback =
        (prio == ThreadPriority::Lowest) ? db.perfSets : db.effSets;

      if (!preferred.empty()) {
        db.pSetSets(threadHandle, preferred.data(), static_cast<ULONG>(preferred.size()));
      } else if (!fallback.empty()) {
        db.pSetSets(threadHandle, fallback.data(), static_cast<ULONG>(fallback.size()));
      }
    }

  }


  thread::thread(ThreadProc&& proc)
  : thread(std::move(proc), ThreadPriority::Normal) {
  }


  thread::thread(ThreadProc&& proc, ThreadPriority prio)
  : m_data(nullptr) {
    if (this_thread::isInModuleDetachment()) {
      throw std::system_error(
        std::make_error_code(std::errc::operation_canceled),
        "DXVK: thread creation during module detach");
    }

    m_data = new ThreadData(std::move(proc));
    m_data->priority.store(prio, std::memory_order_relaxed);

    m_data->handle = ::CreateThread(
      nullptr,
      DefaultStackReserve,
      thread::threadProc,
      m_data,
      CREATE_SUSPENDED | STACK_SIZE_PARAM_IS_A_RESERVATION,
      &m_data->id);

    if (!m_data->handle) {
      delete m_data;
      m_data = nullptr;

      throw std::system_error(
        std::make_error_code(std::errc::resource_unavailable_try_again),
        "DXVK: CreateThread failed");
    }

    const int winPrio = (prio == ThreadPriority::Lowest)
      ? THREAD_PRIORITY_LOWEST
      : THREAD_PRIORITY_NORMAL;

    ::SetThreadPriority(m_data->handle, winPrio);
    applyCpuSetAffinity(m_data->handle, prio);
    ::ResumeThread(m_data->handle);
  }


  thread::~thread() {
    if (joinable())
      std::terminate();
  }


  void thread::detach() {
    if (!joinable()) {
      throw std::system_error(
        std::make_error_code(std::errc::invalid_argument),
        "DXVK: detach on non-joinable thread");
    }

    m_data->decRef();
    m_data = nullptr;
  }


  void thread::join() {
    if (!joinable()) {
      throw std::system_error(
        std::make_error_code(std::errc::invalid_argument),
        "DXVK: join on non-joinable thread");
    }

    if (get_id() == this_thread::get_id()) {
      throw std::system_error(
        std::make_error_code(std::errc::resource_deadlock_would_occur),
        "DXVK: cannot join current thread");
    }

    if (::WaitForSingleObjectEx(m_data->handle, INFINITE, FALSE) == WAIT_FAILED) {
      throw std::system_error(
        std::make_error_code(std::errc::io_error),
        "DXVK: WaitForSingleObjectEx failed");
    }

    detach();
  }


  void thread::set_priority(ThreadPriority prio) {
    if (!joinable())
      return;

    ThreadPriority old = m_data->priority.load(std::memory_order_relaxed);
    while (old != prio) {
      if (m_data->priority.compare_exchange_weak(
            old, prio, std::memory_order_relaxed, std::memory_order_relaxed)) {
        const int winPrio = (prio == ThreadPriority::Lowest)
          ? THREAD_PRIORITY_LOWEST
          : THREAD_PRIORITY_NORMAL;

        ::SetThreadPriority(m_data->handle, winPrio);
        applyCpuSetAffinity(m_data->handle, prio);
        return;
      }
    }
  }


  uint32_t thread::hardware_concurrency() {
    static std::atomic<uint32_t> cached { 0u };

    uint32_t cores = cached.load(std::memory_order_relaxed);
    if (cores)
      return cores;

    using GLPIEX_t = BOOL (WINAPI*)(LOGICAL_PROCESSOR_RELATIONSHIP,
                                    PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX,
                                    PDWORD);

    static GLPIEX_t pGLPIEX = reinterpret_cast<GLPIEX_t>(
      ::GetProcAddress(::GetModuleHandleW(L"kernel32.dll"),
                       "GetLogicalProcessorInformationEx"));

    if (pGLPIEX) {
      DWORD len = 0;
      pGLPIEX(RelationProcessorCore, nullptr, &len);

      if (len) {
        constexpr size_t StackBufSize = 2048u;
        char stackBuf[StackBufSize];
        std::vector<char> heapBuf;
        char* buf = nullptr;

        if (len <= StackBufSize) {
          buf = stackBuf;
        } else {
          heapBuf.resize(len);
          buf = heapBuf.data();
        }

        if (pGLPIEX(RelationProcessorCore,
            reinterpret_cast<PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX>(buf), &len)) {
          DWORD off = 0;
          BYTE minClass = std::numeric_limits<BYTE>::max();
          BYTE maxClass = std::numeric_limits<BYTE>::min();
          uint32_t topClassThreads = 0u;
          bool haveEntries = false;

          while (off < len) {
            auto* info = reinterpret_cast<const SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX*>(buf + off);

            if (!info->Size || info->Size > len - off)
              break;

            if (info->Relationship == RelationProcessorCore) {
              const BYTE cls = info->Processor.EfficiencyClass;
              haveEntries = true;
              minClass = std::min(minClass, cls);
              maxClass = std::max(maxClass, cls);
            }

            off += info->Size;
          }

          if (haveEntries && minClass != maxClass) {
            off = 0;

            while (off < len) {
              auto* info = reinterpret_cast<const SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX*>(buf + off);

              if (!info->Size || info->Size > len - off)
                break;

              if (info->Relationship == RelationProcessorCore
               && info->Processor.EfficiencyClass == maxClass) {
                for (WORD g = 0; g < info->Processor.GroupCount; g++) {
                  const KAFFINITY mask = info->Processor.GroupMask[g].Mask;
                  topClassThreads += bit::popcnt(static_cast<uint64_t>(mask));
                }
              }

              off += info->Size;
            }

            if (topClassThreads)
              cores = topClassThreads;
          }
        }
      }
    }

    if (!cores)
      cores = ::GetActiveProcessorCount(ALL_PROCESSOR_GROUPS);

    if (!cores)
      cores = 1u;

    cached.store(cores, std::memory_order_relaxed);
    return cores;
  }


  DWORD WINAPI thread::threadProc(void* arg) noexcept {
    auto* data = reinterpret_cast<ThreadData*>(arg);
    DWORD rc = 0u;

    try {
      data->proc();
    } catch (...) {
      rc = 1u;
    }

    data->decRef();
    return rc;
  }


  namespace this_thread {

    bool isInModuleDetachment() noexcept {
      using Fn = BOOLEAN (NTAPI*)();

      static Fn fn = reinterpret_cast<Fn>(
        ::GetProcAddress(::GetModuleHandleW(L"ntdll.dll"),
                         "RtlDllShutdownInProgress"));

      return fn ? (fn() != 0) : false;
    }

  }

}

#else

namespace dxvk { }

#endif
