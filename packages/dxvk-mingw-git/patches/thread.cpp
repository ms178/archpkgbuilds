/*  thread.cpp – DXVK threading implementation
 *  ------------------------------------------
 *  FINAL, FAIR & HYBRID-CPU AWARE
 */

#include <atomic>
#include <system_error>
#include <vector>
#include <limits>

#include "thread.h"
#include "util_bit.h"
#include "util_likely.h"

#ifdef DXVK_ARCH_X86
#include <x86intrin.h>
#endif

/* ===================================================================== */
/*  Windows implementation                                               */
/* ===================================================================== */
#ifdef _WIN32

/*  Older SDK compatibility – PRS                                          */
#ifndef SYSTEM_CPU_SET_INFORMATION_ALLOCATED_FLAG
# define SYSTEM_CPU_SET_INFORMATION_ALLOCATED_FLAG 0x2
#endif
#ifndef SYSTEM_CPU_SET_INFORMATION_PARKED_FLAG
# define SYSTEM_CPU_SET_INFORMATION_PARKED_FLAG    0x1
#endif

namespace dxvk {

  /* --------------------------------------------------------------------- */
  /*  CPU-set discovery & steering (shared)                                */
  /* --------------------------------------------------------------------- */
  namespace {

    struct CpuSetDatabase {
      std::vector<ULONG> pSets;
      std::vector<ULONG> eSets;

      using PFN_GetCpuInfo = BOOL (WINAPI*)(PSYSTEM_CPU_SET_INFORMATION, ULONG,
                                            PULONG, HANDLE, ULONG);
      using PFN_SetThreadSets = BOOL (WINAPI*)(HANDLE, const ULONG*, ULONG);

      PFN_GetCpuInfo     pGetInfo = nullptr;
      PFN_SetThreadSets  pSetSets = nullptr;
      bool available = false;

      CpuSetDatabase() {
        init();
      }

    private:
      void init() {
        HMODULE k32 = ::GetModuleHandleW(L"kernel32.dll");
        if (!k32) {
          return;
        }

        pGetInfo = reinterpret_cast<PFN_GetCpuInfo>(
          ::GetProcAddress(k32, "GetSystemCpuSetInformation"));
        pSetSets = reinterpret_cast<PFN_SetThreadSets>(
          ::GetProcAddress(k32, "SetThreadSelectedCpuSets"));
        if (!pGetInfo || !pSetSets) {
          return;
        }

        ULONG len = 0;
        if (!pGetInfo(nullptr, 0, &len, ::GetCurrentProcess(), 0) &&
            ::GetLastError() != ERROR_INSUFFICIENT_BUFFER) {
          return;
        }
        if (!len) {
          return;
        }

        std::vector<char> buf(len);
        if (!pGetInfo(reinterpret_cast<PSYSTEM_CPU_SET_INFORMATION>(buf.data()),
                      len, &len, ::GetCurrentProcess(), 0)) {
          return;
        }

        // Approximate reservation to avoid reallocations while parsing
        const size_t approx = static_cast<size_t>(len) / sizeof(SYSTEM_CPU_SET_INFORMATION);
        pSets.reserve(approx);
        eSets.reserve(approx);

        ULONG off = 0;
        while (off < len) {
          auto* info = reinterpret_cast<PSYSTEM_CPU_SET_INFORMATION>(buf.data() + off);
          if (info->Type == CpuSetInformation) {
            const auto& cs = info->CpuSet;
            if (!(cs.AllFlags & SYSTEM_CPU_SET_INFORMATION_ALLOCATED_FLAG) ||
                (cs.AllFlags & SYSTEM_CPU_SET_INFORMATION_PARKED_FLAG)) {
              // Unusable
            } else if (cs.EfficiencyClass == 0) {
              pSets.push_back(cs.Id);
            } else {
              eSets.push_back(cs.Id);
            }
          }
          off += info->Size;
        }

        available = !pSets.empty() || !eSets.empty();
      }
    };

    CpuSetDatabase& cpuDb() {
      static CpuSetDatabase db;
      return db;
    }

    void applyCpuSetAffinity(HANDLE thr, ThreadPriority prio) {
      auto& db = cpuDb();
      if (!db.available) {
        return;
      }

      const std::vector<ULONG>& tgt =
        (prio == ThreadPriority::Lowest) ? db.eSets : db.pSets;

      if (!tgt.empty()) {
        const ULONG count = (tgt.size() >= static_cast<size_t>(std::numeric_limits<ULONG>::max()))
          ? std::numeric_limits<ULONG>::max()
          : static_cast<ULONG>(tgt.size());
        db.pSetSets(thr, tgt.data(), count);
      }
    }

    // Futex-style wait/wake in user mode via Win32 (WaitOnAddress/WakeByAddressSingle)
    struct WaitAddressApi {
      using PFN_WaitOnAddress       = BOOL (WINAPI*)(volatile VOID*, PVOID, SIZE_T, DWORD);
      using PFN_WakeByAddressSingle = VOID (WINAPI*)(PVOID);

      PFN_WaitOnAddress       pWait = nullptr;
      PFN_WakeByAddressSingle pWake = nullptr;
      bool available = false;

      WaitAddressApi() {
        HMODULE k32 = ::GetModuleHandleW(L"kernel32.dll");
        if (!k32) {
          return;
        }

        pWait = reinterpret_cast<PFN_WaitOnAddress>(
          ::GetProcAddress(k32, "WaitOnAddress"));
        pWake = reinterpret_cast<PFN_WakeByAddressSingle>(
          ::GetProcAddress(k32, "WakeByAddressSingle"));
        available = (pWait != nullptr) && (pWake != nullptr);
      }
    };

    WaitAddressApi& waitApi() {
      static WaitAddressApi api;
      return api;
    }

  } /* anonymous namespace */

  /* --------------------------------------------------------------------- */
  /*  fast_mutex – Pillar 1 – Ticket lock                                  */
  /* --------------------------------------------------------------------- */

  // The counter layout is:
  //   low  32 bits: head (next ticket to serve)
  //   high 32 bits: tail (next ticket to take)
  static_assert(sizeof(std::atomic<uint64_t>) == 8, "fast_mutex requires 8-byte atomic");

  DXVK_FORCE_INLINE
  void fast_mutex::lock() noexcept {
    // Take a ticket by bumping the tail (upper 32 bits). Relaxed is sufficient.
    const uint64_t prev = m_ctr.fetch_add(1ull << 32, std::memory_order_relaxed);
    const uint32_t my   = static_cast<uint32_t>(prev >> 32);

    // We will watch the head (low 32 bits). Use a short spin to preserve fast path,
    // then park with WaitOnAddress to avoid burning CPU under contention.
    WaitAddressApi& wa = waitApi();
    uint32_t spin = 0;

    uint32_t* const headAddr32 = reinterpret_cast<uint32_t*>(&m_ctr);

    for (;;) {
      const uint32_t head = static_cast<uint32_t>(m_ctr.load(std::memory_order_acquire));
      if (head == my) {
        return;
      }

#ifdef DXVK_ARCH_X86
      if (spin < 64) {
        _mm_pause();
        ++spin;
        continue;
      }
#endif

      if (wa.available) {
        uint32_t expected = head;
        wa.pWait(reinterpret_cast<volatile VOID*>(headAddr32),
                 &expected, sizeof(expected), INFINITE);
        spin = 0; // after wake, retry with fresh acquire load
      } else {
        this_thread::yield();
        spin = 0;
      }
    }
  }

  DXVK_FORCE_INLINE
  void fast_mutex::unlock() noexcept {
    // Increment head (release) to publish critical-section writes.
    m_ctr.fetch_add(1u, std::memory_order_release);

    // Wake exactly one waiter if available.
    WaitAddressApi& wa = waitApi();
    if (wa.available) {
      uint32_t* const headAddr32 = reinterpret_cast<uint32_t*>(&m_ctr);
      wa.pWake(reinterpret_cast<PVOID>(headAddr32));
    }
  }

  DXVK_FORCE_INLINE
  bool fast_mutex::try_lock() noexcept {
    // Check current head/tail snapshot; relaxed is fine for the initial read.
    uint64_t cur = m_ctr.load(std::memory_order_relaxed);
    const uint32_t head = static_cast<uint32_t>(cur);
    const uint32_t tail = static_cast<uint32_t>(cur >> 32);
    if (head != tail) {
      return false; // busy
    }

    // Acquire on success to pair with unlock's release.
    return m_ctr.compare_exchange_strong(cur,
                                         cur + (1ull << 32),  // ++tail
                                         std::memory_order_acquire,
                                         std::memory_order_relaxed);
  }

  /* --------------------------------------------------------------------- */
  /*  thread constructors / dtor                                           */
  /* --------------------------------------------------------------------- */
  thread::thread(ThreadProc&& proc)
  : thread(std::move(proc), ThreadPriority::Normal) { }

  thread::thread(ThreadProc&& proc, ThreadPriority prio)
  : m_data(new ThreadData(std::move(proc))) {
    if (this_thread::isInModuleDetachment()) {
      delete m_data;
      m_data = nullptr;
      throw std::system_error(
        std::make_error_code(std::errc::operation_canceled),
        "DXVK: thread creation during module detach");
    }

    m_data->handle = ::CreateThread(
      nullptr,
      1u << 20,                          /* reserve 1 MiB */
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

    /* Pillar 3 – proactive affinity & priority */
    ::SetThreadPriority(
      m_data->handle,
      (prio == ThreadPriority::Lowest)
        ? THREAD_PRIORITY_LOWEST
        : THREAD_PRIORITY_NORMAL);

    applyCpuSetAffinity(m_data->handle, prio);

    ::ResumeThread(m_data->handle);
  }

  thread::~thread() {
    if (joinable()) {
      std::terminate();
    }
  }

  /* --------------------------------------------------------------------- */
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

  /* --------------------------------------------------------------------- */
  void thread::set_priority(ThreadPriority prio) {
    if (!joinable()) {
      return;
    }

    ::SetThreadPriority(
      m_data->handle,
      (prio == ThreadPriority::Lowest)
        ? THREAD_PRIORITY_LOWEST
        : THREAD_PRIORITY_NORMAL);

    applyCpuSetAffinity(m_data->handle, prio);
  }

  /* --------------------------------------------------------------------- */
  uint32_t thread::hardware_concurrency() {
    static std::atomic<uint32_t> cached{ 0 };
    uint32_t v = cached.load(std::memory_order_acquire);
    if (v) {
      return v;
    }

    using GLPIEX_t = BOOL (WINAPI*)(LOGICAL_PROCESSOR_RELATIONSHIP,
                                    PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX,
                                    PDWORD);
    static auto pGLPIEX = reinterpret_cast<GLPIEX_t>(
      ::GetProcAddress(::GetModuleHandleW(L"kernel32.dll"),
                       "GetLogicalProcessorInformationEx"));

    uint32_t result = 0;

    if (pGLPIEX) {
      DWORD len = 0;
      pGLPIEX(RelationProcessorCore, nullptr, &len);
      if (len) {
        std::vector<char> buf(len);
        if (pGLPIEX(RelationProcessorCore,
                    reinterpret_cast<PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX>(buf.data()), &len)) {
          uint32_t pLogical = 0;
          bool hasE = false;
          DWORD off = 0;

          while (off < len) {
            auto* info = reinterpret_cast<const SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX*>(buf.data() + off);
            if (info->Relationship == RelationProcessorCore) {
              for (WORD g = 0; g < info->Processor.GroupCount; ++g) {
                UINT64 mask = info->Processor.GroupMask[g].Mask;
                if (info->Processor.EfficiencyClass == 0) {
                  pLogical += dxvk::bit::popcnt(mask);
                } else {
                  hasE = true;
                }
              }
            }
            off += info->Size;
          }

          if (hasE && pLogical) {
            result = pLogical; // prefer P-core logicals on hybrid
          }
        }
      }
    }

    if (!result) {
      SYSTEM_INFO si{};
      ::GetSystemInfo(&si);
      result = si.dwNumberOfProcessors;
    }

    cached.store(result, std::memory_order_release);
    return result;
  }

  /* --------------------------------------------------------------------- */
  DWORD WINAPI thread::threadProc(void* arg) noexcept {
    auto* d = reinterpret_cast<ThreadData*>(arg);
    DWORD rc = 0;
    try {
      d->proc();
    } catch (...) {
      rc = 1;
    }
    d->decRef();
    return rc;
  }

  /* --------------------------------------------------------------------- */
  namespace this_thread {
    bool isInModuleDetachment() noexcept {
      using Fn = BOOLEAN (WINAPI*)();
      static Fn fn = reinterpret_cast<Fn>(
        ::GetProcAddress(::GetModuleHandleW(L"ntdll.dll"),
                         "RtlDllShutdownInProgress"));
      return fn && fn();
    }
  } /* namespace this_thread */

} /* namespace dxvk */

#else  /* =============================================================  */
namespace dxvk { /* POSIX implementation is header-only */ }
#endif
