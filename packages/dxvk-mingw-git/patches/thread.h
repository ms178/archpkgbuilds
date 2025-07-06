#pragma once

#include <chrono>
#include <condition_variable>
#include <functional>
#include <mutex>
#include <thread>
#include <utility>

#include "util_error.h"

#include "./com/com_include.h"

#include "./rc/util_rc.h"
#include "./rc/util_rc_ptr.h"

// Force inline macro for hot paths
#if defined(_MSC_VER)
#define DXVK_FORCE_INLINE __forceinline
#elif defined(__GNUC__) || defined(__clang__)
#define DXVK_FORCE_INLINE inline __attribute__((always_inline))
#else
#define DXVK_FORCE_INLINE inline
#endif

namespace dxvk {

  /**
   * \brief Thread priority
   */
  enum class ThreadPriority : int32_t {
    Normal,
    Lowest,
  };

  #ifdef _WIN32

  using ThreadProc = std::function<void()>;


  /**
   * \brief Thread object
   */
  struct ThreadData {
    ThreadData(ThreadProc&& proc_)
    : proc(std::move(proc_)) { }

    ~ThreadData() {
      if (handle) {
        CloseHandle(handle);
      }
    }

    HANDLE                handle = nullptr;
    DWORD                 id     = 0;
    std::atomic<uint32_t> refs   = { 2u };
    ThreadProc            proc;

    void decRef() {
      if (refs.fetch_sub(1, std::memory_order_release) == 1) {
        std::atomic_thread_fence(std::memory_order_acquire);
        delete this;
      }
    }
  };


  /**
   * \brief Thread wrapper
   *
   * Drop-in replacement for std::thread
   * using plain win32 threads.
   */
  class thread {

  public:

    using id = uint32_t;
    using native_handle_type = HANDLE;

    thread() { }

    explicit thread(ThreadProc&& proc);

    ~thread();

    thread(thread&& other) noexcept
    : m_data(std::exchange(other.m_data, nullptr)) { }

    thread& operator = (thread&& other) noexcept {
      if (joinable()) {
        std::terminate();
      }

      if (m_data) {
        m_data->decRef();
      }

      m_data = std::exchange(other.m_data, nullptr);
      return *this;
    }

    void detach() {
      if (!joinable()) {
        throw std::system_error(std::make_error_code(std::errc::invalid_argument), "Thread not detachable");
      }

      m_data->decRef();
      m_data = nullptr;
    }

    DXVK_FORCE_INLINE bool joinable() const {
      return m_data != nullptr;
    }

    DXVK_FORCE_INLINE id get_id() const {
      return joinable() ? m_data->id : id();
    }

    DXVK_FORCE_INLINE native_handle_type native_handle() const {
      return joinable() ? m_data->handle : native_handle_type();
    }

    void swap(thread& other) noexcept {
      std::swap(m_data, other.m_data);
    }

    void join();

    void set_priority(ThreadPriority priority);

    static uint32_t hardware_concurrency();

  private:

    ThreadData* m_data = nullptr;

    static DWORD WINAPI threadProc(void* arg);

  };


  namespace this_thread {
    DXVK_FORCE_INLINE void yield() {
      SwitchToThread();
    }

    DXVK_FORCE_INLINE thread::id get_id() {
      return thread::id(GetCurrentThreadId());
    }

    bool isInModuleDetachment();
  }


  /**
   * \brief SRW-based mutex implementation
   *
   * Drop-in replacement for \c std::mutex that uses Win32
   * SRW locks, which are implemented with \c futex in wine.
   */
  class mutex {

  public:

    using native_handle_type = PSRWLOCK;

    mutex() { }

    mutex(const mutex&) = delete;
    mutex& operator = (const mutex&) = delete;

    DXVK_FORCE_INLINE void lock() noexcept {
      AcquireSRWLockExclusive(&m_lock);
    }

    DXVK_FORCE_INLINE void unlock() noexcept {
      ReleaseSRWLockExclusive(&m_lock);
    }

    DXVK_FORCE_INLINE bool try_lock() noexcept {
      return TryAcquireSRWLockExclusive(&m_lock);
    }

    DXVK_FORCE_INLINE native_handle_type native_handle() noexcept {
      return &m_lock;
    }

  private:

    SRWLOCK m_lock = SRWLOCK_INIT;

  };


  /**
   * \brief Fast mutex with adaptive spinning
   *
   * Spins briefly before falling back to SRW lock to reduce
   * context switch overhead for short critical sections.
   */
  class fast_mutex {
  public:
    using native_handle_type = PSRWLOCK;

    fast_mutex() { }

    fast_mutex(const fast_mutex&) = delete;
    fast_mutex& operator = (const fast_mutex&) = delete;

    DXVK_FORCE_INLINE void lock() noexcept {
      if (likely(try_lock())) {
        return;
      }

      // Phase 1: Aggressive spin for ultra-short contention.
      for (uint32_t i = 0; i < 64; i++) {
        #if defined(DXVK_ARCH_X86)
        #if defined(_MSC_VER)
        _mm_pause();
        #elif defined(__GNUC__) || defined(__clang__)
        __builtin_ia32_pause();
        #endif
        #endif
        if (try_lock()) {
          return;
        }
      }

      // Phase 2: Yielding spin for short-term contention.
      for (uint32_t i = 0; i < 16; i++) {
        dxvk::this_thread::yield();
        if (try_lock()) {
          return;
        }
      }

      // Phase 3: Fall back to a blocking OS call.
      AcquireSRWLockExclusive(&m_lock);
    }

    DXVK_FORCE_INLINE void unlock() noexcept {
      ReleaseSRWLockExclusive(&m_lock);
    }

    DXVK_FORCE_INLINE bool try_lock() noexcept {
      return TryAcquireSRWLockExclusive(&m_lock);
    }

    DXVK_FORCE_INLINE native_handle_type native_handle() noexcept {
      return &m_lock;
    }

  private:
    SRWLOCK m_lock = SRWLOCK_INIT;
  };

  // Alias for compatibility
  using spin_mutex = fast_mutex;


  /**
   * \brief Recursive mutex implementation
   *
   * Drop-in replacement for \c std::recursive_mutex that
   * uses Win32 critical sections.
   */
  class recursive_mutex {

  public:

    using native_handle_type = PCRITICAL_SECTION;

    recursive_mutex() {
      InitializeCriticalSection(&m_lock);
    }

    ~recursive_mutex() {
      DeleteCriticalSection(&m_lock);
    }

    recursive_mutex(const recursive_mutex&) = delete;
    recursive_mutex& operator = (const recursive_mutex&) = delete;

    DXVK_FORCE_INLINE void lock() noexcept {
      EnterCriticalSection(&m_lock);
    }

    DXVK_FORCE_INLINE void unlock() noexcept {
      LeaveCriticalSection(&m_lock);
    }

    DXVK_FORCE_INLINE bool try_lock() noexcept {
      return TryEnterCriticalSection(&m_lock);
    }

    DXVK_FORCE_INLINE native_handle_type native_handle() noexcept {
      return &m_lock;
    }

  private:

    CRITICAL_SECTION m_lock;

  };


  /**
   * \brief SRW-based condition variable implementation
   *
   * Drop-in replacement for \c std::condition_variable that
   * uses Win32 condition variables on SRW locks.
   */
  class condition_variable {

  public:

    using native_handle_type = PCONDITION_VARIABLE;

    condition_variable() {
      InitializeConditionVariable(&m_cond);
    }

    condition_variable(condition_variable&) = delete;

    condition_variable& operator = (condition_variable&) = delete;

    DXVK_FORCE_INLINE void notify_one() noexcept {
      WakeConditionVariable(&m_cond);
    }

    DXVK_FORCE_INLINE void notify_all() noexcept {
      WakeAllConditionVariable(&m_cond);
    }

    void wait(std::unique_lock<dxvk::mutex>& lock) {
      auto srw = lock.mutex()->native_handle();
      SleepConditionVariableSRW(&m_cond, srw, INFINITE, 0);
    }

    template<typename Predicate>
    void wait(std::unique_lock<dxvk::mutex>& lock, Predicate pred) {
      while (!pred()) {
        wait(lock);
      }
    }

    template<typename Clock, typename Duration>
    std::cv_status wait_until(std::unique_lock<dxvk::mutex>& lock, const std::chrono::time_point<Clock, Duration>& time) {
      auto now = Clock::now();
      return (now < time)
      ? wait_for(lock, time - now)
      : std::cv_status::timeout;
    }

    template<typename Clock, typename Duration, typename Predicate>
    bool wait_until(std::unique_lock<dxvk::mutex>& lock, const std::chrono::time_point<Clock, Duration>& time, Predicate pred) {
      while (!pred()) {
        if (wait_until(lock, time) == std::cv_status::timeout) {
          return pred();
        }
      }
      return true;
    }

    template<typename Rep, typename Period>
    std::cv_status wait_for(std::unique_lock<dxvk::mutex>& lock, const std::chrono::duration<Rep, Period>& timeout) {
      auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(timeout);
      auto srw = lock.mutex()->native_handle();

      if (ms.count() < 0) {
        ms = std::chrono::milliseconds(0);
      }

      return SleepConditionVariableSRW(&m_cond, srw, DWORD(ms.count()), 0)
      ? std::cv_status::no_timeout
      : std::cv_status::timeout;
    }

    template<typename Rep, typename Period, typename Predicate>
    bool wait_for(std::unique_lock<dxvk::mutex>& lock, const std::chrono::duration<Rep, Period>& timeout, Predicate pred) {
      auto end_time = std::chrono::steady_clock::now() + timeout;
      while (!pred()) {
        if (wait_for(lock, end_time - std::chrono::steady_clock::now()) == std::cv_status::timeout) {
          return pred();
        }
      }
      return true;
    }

    DXVK_FORCE_INLINE native_handle_type native_handle() noexcept {
      return &m_cond;
    }

  private:

    CONDITION_VARIABLE m_cond;

  };

  #else
  #include <sched.h>
  class thread : public std::thread {
  public:
    using std::thread::thread;

    void set_priority(ThreadPriority priority) {
      sched_param param = {};
      int policy;
      switch (priority) {
        default:
        case ThreadPriority::Normal: policy = SCHED_OTHER; break;
        #ifndef __linux__
        case ThreadPriority::Lowest: policy = SCHED_OTHER; break;
        #else
        case ThreadPriority::Lowest: policy = SCHED_IDLE;  break;
        #endif
      }
      pthread_setschedparam(this->native_handle(), policy, ¶m);
    }
  };

  using mutex              = std::mutex;
  using recursive_mutex    = std::recursive_mutex;
  using condition_variable = std::condition_variable;
  using fast_mutex         = std::mutex;
  using spin_mutex         = std::mutex;

  namespace this_thread {
    DXVK_FORCE_INLINE void yield() {
      std::this_thread::yield();
    }

    uint32_t get_id();

    inline bool isInModuleDetachment() {
      return false;
    }
  }
  #endif

}
