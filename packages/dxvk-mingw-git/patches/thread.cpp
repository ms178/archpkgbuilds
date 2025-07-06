#include <atomic>
#include "thread.h"
#include "util_likely.h"

#ifdef _WIN32
namespace dxvk {

  /*---------------------------------------------------------------
   * Thread constructor – avoids spawn during DLL detach
   *-------------------------------------------------------------*/
  thread::thread(ThreadProc&& proc)
  : m_data(nullptr) {
    if (this_thread::isInModuleDetachment())
      throw std::system_error(std::make_error_code(std::errc::operation_canceled),
                              "Cannot create thread during module detach");

      m_data = new ThreadData(std::move(proc));

    m_data->handle =
    ::CreateThread(nullptr,
                   0x100000,                      /* reserve 1 MiB (same as earlier)  */
                   thread::threadProc,
                   m_data,
                   STACK_SIZE_PARAM_IS_A_RESERVATION,
                   &m_data->id);

    if (!m_data->handle) {
      delete m_data;
      m_data = nullptr;
      throw std::system_error(std::make_error_code(std::errc::resource_unavailable_try_again),
                              "Failed to create thread");
    }
  }

  thread::~thread() {
    if (joinable())
      std::terminate();   /* contract identical to std::thread */
  }

  void thread::join() {
    if (!joinable())
      throw std::system_error(std::make_error_code(std::errc::invalid_argument),
                              "Thread not joinable");

      if (get_id() == this_thread::get_id())
        throw std::system_error(std::make_error_code(std::errc::resource_deadlock_would_occur),
                                "Cannot join current thread");

        if (::WaitForSingleObjectEx(m_data->handle, INFINITE, FALSE) == WAIT_FAILED)
          throw std::system_error(std::make_error_code(std::errc::io_error),
                                  "WaitForSingleObjectEx failed");

          detach();
  }

  void thread::set_priority(ThreadPriority prio) {
    int value;
    switch (prio) {
      default:
      case ThreadPriority::Normal: value = THREAD_PRIORITY_NORMAL; break;
      case ThreadPriority::Lowest: value = THREAD_PRIORITY_LOWEST; break;
    }
    if (m_data)
      ::SetThreadPriority(m_data->handle, value);
  }

  uint32_t thread::hardware_concurrency() {
    /* Windows 7 SP1+ – supports per-group queries */
    using GetActiveProcessorCount_t = DWORD (WINAPI*)(WORD);
    static auto pGetActiveProcessorCount =
    reinterpret_cast<GetActiveProcessorCount_t>(
      ::GetProcAddress(::GetModuleHandleW(L"kernel32.dll"),
                       "GetActiveProcessorCount"));

    if (pGetActiveProcessorCount)
      return pGetActiveProcessorCount(ALL_PROCESSOR_GROUPS);

    SYSTEM_INFO info {};
    ::GetSystemInfo(&info);
    return info.dwNumberOfProcessors;
  }

  /*---------------------------------------------------------------
   * _beginthreadex alternative avoided – keep RTL neutral
   *-------------------------------------------------------------*/
  DWORD WINAPI thread::threadProc(void* arg) {
    auto* data = reinterpret_cast<ThreadData*>(arg);
    DWORD exitCode = 0;
    try {
      data->proc();
    } catch (...) {
      exitCode = 1;
    }
    data->decRef();
    return exitCode;
  }

} /* namespace dxvk */

namespace dxvk::this_thread {

  bool isInModuleDetachment() {
    using Fn = BOOLEAN (WINAPI*)();
    static auto RtlDllShutdownInProgress =
    reinterpret_cast<Fn>(
      ::GetProcAddress(::GetModuleHandleW(L"ntdll.dll"),
                       "RtlDllShutdownInProgress"));
    return RtlDllShutdownInProgress && RtlDllShutdownInProgress();
  }

} /* namespace dxvk::this_thread */

#else  /* ----------------------------- POSIX ---------------------------- */
namespace dxvk::this_thread {
  /* definition in header – nothing more to do */
}
#endif /* _WIN32 */
