#include <atomic>
#include <vector>

#include "thread.h"
#include "util_bit.h"
#include "util_likely.h"

#ifdef _WIN32
namespace dxvk {

  /*---------------------- constructor ------------------------------------*/
  thread::thread(ThreadProc&& proc) : m_data(new ThreadData(std::move(proc))) {
    /* forbid thread creation during DLL detach (prevents GPU hang crash) */
    if (this_thread::isInModuleDetachment()) {
      delete m_data; m_data = nullptr;
      throw std::system_error(std::make_error_code(std::errc::operation_canceled),
                              "DXVK: thread creation during module detach");
    }

    m_data->handle = ::CreateThread(nullptr,
                                    1 << 20,        /* 1 MiB reserve */
                                    threadProc,
                                    m_data,
                                    STACK_SIZE_PARAM_IS_A_RESERVATION,
                                    &m_data->id);

    if (!m_data->handle) {
      delete m_data; m_data = nullptr;
      throw std::system_error(
        std::make_error_code(std::errc::resource_unavailable_try_again),
                              "DXVK: CreateThread failed");
    }
  }

  /*---------------------- destructor -------------------------------------*/
  thread::~thread() {
    if (joinable()) std::terminate();   /* std::thread semantics */
  }

  /*---------------------- detach -----------------------------------------*/
  void thread::detach() {
    if (!joinable())
      throw std::system_error(std::make_error_code(std::errc::invalid_argument),
                              "DXVK: detach on non-joinable thread");
      m_data->decRef();
    m_data = nullptr;
  }

  /*---------------------- join -------------------------------------------*/
  void thread::join() {
    if (!joinable())
      throw std::system_error(std::make_error_code(std::errc::invalid_argument),
                              "DXVK: join on non-joinable thread");
      if (get_id() == this_thread::get_id())
        throw std::system_error(std::make_error_code(
          std::errc::resource_deadlock_would_occur),
          "DXVK: cannot join current thread");

        if (::WaitForSingleObjectEx(m_data->handle, INFINITE, FALSE) == WAIT_FAILED)
          throw std::system_error(std::make_error_code(std::errc::io_error),
                                  "DXVK: WaitForSingleObjectEx failed");
          detach();
  }

  /*---------------------- set_priority -----------------------------------*/
  void thread::set_priority(ThreadPriority p) {
    if (!joinable()) return;
    int val = (p == ThreadPriority::Lowest)
    ? THREAD_PRIORITY_LOWEST : THREAD_PRIORITY_NORMAL;
    ::SetThreadPriority(m_data->handle, val);
  }

  /*---------------------- hardware_concurrency ---------------------------*/
  uint32_t thread::hardware_concurrency() {
    using GLPIEX_t = BOOL (WINAPI*)(LOGICAL_PROCESSOR_RELATIONSHIP,
                                    PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX,
                                    PDWORD);
    static GLPIEX_t pGLPIEX =
    reinterpret_cast<GLPIEX_t>(
      ::GetProcAddress(::GetModuleHandleW(L"kernel32.dll"),
                       "GetLogicalProcessorInformationEx"));

    if (pGLPIEX) {
      DWORD len = 0;
      pGLPIEX(RelationProcessorCore, nullptr, &len);
      if (len) {
        std::vector<char> buf(len);
        if (pGLPIEX(RelationProcessorCore,
          reinterpret_cast<PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX>(buf.data()),
                    &len)) {
          DWORD off = 0, pcores = 0;
        while (off < len) {
          auto *info = reinterpret_cast<PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX>(buf.data()+off);
          pcores += dxvk::bit::popcnt(info->Processor.GroupMask[0].Mask);
          off += info->Size;
        }
        if (pcores) return pcores;
                    }
      }
    }

    SYSTEM_INFO si{};
    ::GetSystemInfo(&si);
    return si.dwNumberOfProcessors;
  }

  /*---------------------- thread entry proc ------------------------------*/
  DWORD WINAPI thread::threadProc(void* arg) {
    auto* d = reinterpret_cast<ThreadData*>(arg);
    DWORD rc = 0;
    try { d->proc(); }
    catch (...) { rc = 1; }
    d->decRef();
    return rc;
  }

} /* dxvk */

/*---------------- this_thread helpers ----------------------------------*/
namespace dxvk::this_thread {
  bool isInModuleDetachment() {
    using Fn = BOOLEAN (WINAPI*)();
    static Fn fn = reinterpret_cast<Fn>(
      ::GetProcAddress(::GetModuleHandleW(L"ntdll.dll"),
                       "RtlDllShutdownInProgress"));
    return fn && fn();
  }
} /* dxvk::this_thread */

#else /* =============================== POSIX ========================== */

namespace dxvk::this_thread {

  /* get_id defined inline in header; nothing extra needed here */

} /* dxvk::this_thread */

#endif /* _WIN32 */
