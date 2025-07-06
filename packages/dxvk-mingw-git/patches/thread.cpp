#include <atomic>
#include <vector>

#include "thread.h"
#include "util_bit.h"
#include "util_likely.h"

#ifdef _WIN32

namespace dxvk {

  thread::thread(ThreadProc&& proc)
  : m_data(new ThreadData(std::move(proc))) {
    // Check if we're in DLL detachment
    if (this_thread::isInModuleDetachment()) {
      delete m_data;
      m_data = nullptr;
      throw std::system_error(std::make_error_code(std::errc::operation_canceled),
                              "Cannot create thread during module detachment");
    }

    m_data->handle = ::CreateThread(nullptr, 0x100000,
                                    thread::threadProc, m_data, STACK_SIZE_PARAM_IS_A_RESERVATION,
                                    &m_data->id);

    if (!m_data->handle) {
      delete m_data;
      m_data = nullptr;
      throw std::system_error(std::make_error_code(std::errc::resource_unavailable_try_again), "Failed to create thread");
    }
  }


  thread::~thread() {
    if (joinable()) {
      std::terminate();
    }
  }


  void thread::join() {
    if (!joinable()) {
      throw std::system_error(std::make_error_code(std::errc::invalid_argument), "Thread not joinable");
    }

    if (get_id() == this_thread::get_id()) {
      throw std::system_error(std::make_error_code(std::errc::resource_deadlock_would_occur), "Cannot join current thread");
    }

    if(::WaitForSingleObjectEx(m_data->handle, INFINITE, FALSE) == WAIT_FAILED) {
      throw std::system_error(std::make_error_code(std::errc::invalid_argument), "Joining thread failed");
    }

    detach();
  }


  void thread::set_priority(ThreadPriority priority) {
    if (!joinable()) {
      return;
    }

    int32_t value;
    switch (priority) {
      default:
      case ThreadPriority::Normal: value = THREAD_PRIORITY_NORMAL; break;
      case ThreadPriority::Lowest: value = THREAD_PRIORITY_LOWEST; break;
    }

    if (m_data) {
      ::SetThreadPriority(m_data->handle, value);
    }
  }


  uint32_t thread::hardware_concurrency() {
    using GetLogicalProcessorInformationEx_t = BOOL (WINAPI *)(LOGICAL_PROCESSOR_RELATIONSHIP, PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX, PDWORD);
    static auto pGetLogicalProcessorInformationEx = reinterpret_cast<GetLogicalProcessorInformationEx_t>(
      ::GetProcAddress(::GetModuleHandleW(L"kernel32.dll"), "GetLogicalProcessorInformationEx"));

    if (pGetLogicalProcessorInformationEx) {
      DWORD length = 0;
      pGetLogicalProcessorInformationEx(RelationProcessorCore, nullptr, &length);

      if (length > 0) {
        std::vector<char> buffer(length);
        auto info = reinterpret_cast<PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX>(buffer.data());

        if (pGetLogicalProcessorInformationEx(RelationProcessorCore, info, &length)) {
          uint32_t pCoreCount = 0;
          uint32_t totalLogicalCoreCount = 0;
          DWORD offset = 0;

          while (offset < length) {
            auto currentInfo = reinterpret_cast<PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX>(buffer.data() + offset);

            uint32_t logicalProcessorsInCore = dxvk::bit::popcnt(currentInfo->Processor.GroupMask[0].Mask);
            totalLogicalCoreCount += logicalProcessorsInCore;

            if (currentInfo->Processor.EfficiencyClass == 0) {
              pCoreCount += logicalProcessorsInCore;
            }
            offset += currentInfo->Size;
          }

          if (pCoreCount > 0 && pCoreCount < totalLogicalCoreCount) {
            return pCoreCount;
          }
        }
      }
    }

    SYSTEM_INFO info = { };
    ::GetSystemInfo(&info);
    return info.dwNumberOfProcessors;
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

#if defined(__linux__)
#include <unistd.h>
#endif

namespace dxvk::this_thread {

  uint32_t get_id() {
    static thread_local uint32_t t_id = 0u;

    if (likely(t_id != 0u)) {
      return t_id;
    }

    #if defined(__linux__)
    t_id = static_cast<uint32_t>(::gettid());
    #else
    static std::atomic<uint32_t> g_threadCtr = { 0u };
    t_id = ++g_threadCtr;
    #endif
    return t_id;
  }

}

#endif
