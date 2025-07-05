#include <atomic>

#include "thread.h"
#include "util_likely.h"

#ifdef DXVK_ARCH_X86
#include <intrin.h>
#endif

#ifdef _WIN32

namespace dxvk {

  thread::thread(ThreadProc&& proc)
  : m_data(new ThreadData(std::move(proc))) {
    m_data->handle = ::CreateThread(nullptr, 0x100000,
                                    thread::threadProc, m_data, STACK_SIZE_PARAM_IS_A_RESERVATION,
                                    &m_data->id);

    if (!m_data->handle) {
      delete m_data;
      throw std::system_error(std::make_error_code(std::errc::resource_unavailable_try_again), "Failed to create thread");
    }
  }


  thread::~thread() {
    if (joinable())
      std::terminate();
  }


  void thread::join() {
    if (!joinable())
      throw std::system_error(std::make_error_code(std::errc::invalid_argument), "Thread not joinable");

    if (get_id() == this_thread::get_id())
      throw std::system_error(std::make_error_code(std::errc::resource_deadlock_would_occur), "Cannot join current thread");

    if(::WaitForSingleObjectEx(m_data->handle, INFINITE, FALSE) == WAIT_FAILED)
      throw std::system_error(std::make_error_code(std::errc::invalid_argument), "Joining thread failed");

    detach();
  }


  void thread::set_priority(ThreadPriority priority) {
    int32_t value;
    switch (priority) {
      default:
      case ThreadPriority::Normal: value = THREAD_PRIORITY_NORMAL; break;
      case ThreadPriority::Lowest: value = THREAD_PRIORITY_LOWEST; break;
    }

    if (m_data)
      ::SetThreadPriority(m_data->handle, value);
  }


  uint32_t thread::hardware_concurrency() {
    static uint32_t s_concurrency = 0;

    if (likely(s_concurrency != 0))
      return s_concurrency;

    SYSTEM_INFO info = { };
    ::GetSystemInfo(&info);

    DWORD logicalProcessors = info.dwNumberOfProcessors;

    #ifdef DXVK_ARCH_X86
    int cpuInfo[4];
    __cpuid(cpuInfo, 0x0B);

    if (cpuInfo[1] & 0xFFFF) {
      uint32_t threadsPerCore = cpuInfo[1] & 0xFFFF;
      s_concurrency = logicalProcessors / threadsPerCore;
    } else
      #endif
    {
      s_concurrency = (logicalProcessors > 4) ? (logicalProcessors / 2) : logicalProcessors;
    }

    return s_concurrency;
  }


  DWORD WINAPI thread::threadProc(void* arg) {
    auto data = reinterpret_cast<ThreadData*>(arg);
    DWORD exitCode = 0;

    try {
      data->proc();
    } catch (...) {
      exitCode = 1;
    }

    data->decRef();
    return exitCode;
  }

}


namespace dxvk::this_thread {

  bool isInModuleDetachment() {
    using PFN_RtlDllShutdownInProgress = BOOLEAN (WINAPI *)();

    static auto RtlDllShutdownInProgress = reinterpret_cast<PFN_RtlDllShutdownInProgress>(
      ::GetProcAddress(::GetModuleHandleW(L"ntdll.dll"), "RtlDllShutdownInProgress"));

    return RtlDllShutdownInProgress && RtlDllShutdownInProgress();
  }

}

#else

namespace dxvk::this_thread {

  static std::atomic<uint32_t> g_threadCtr = { 0u };
  static thread_local uint32_t g_threadId  = 0u;

  uint32_t get_id() {
    if (likely(g_threadId != 0))
      return g_threadId;

    g_threadId = g_threadCtr.fetch_add(1, std::memory_order_relaxed) + 1;
    return g_threadId;
  }

}

#endif
