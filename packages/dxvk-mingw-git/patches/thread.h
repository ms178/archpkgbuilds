#pragma once

#include <atomic>
#include <chrono>
#include <condition_variable>
#include <cstdint>
#include <functional>
#include <limits>
#include <mutex>
#include <shared_mutex>
#include <thread>
#include <utility>

#include "util_error.h"
#include "util_math.h"
#include "util_likely.h"
#include "./com/com_include.h"
#include "./rc/util_rc.h"
#include "./rc/util_rc_ptr.h"

#ifndef CACHE_LINE_SIZE
#define CACHE_LINE_SIZE 64
#endif

#if defined(_MSC_VER)
#define DXVK_FORCE_INLINE __forceinline
#elif defined(__GNUC__) || defined(__clang__)
#define DXVK_FORCE_INLINE inline __attribute__((always_inline))
#else
#define DXVK_FORCE_INLINE inline
#endif

namespace dxvk {

  enum class ThreadPriority : int32_t { Normal, Lowest };

#ifdef _WIN32

  using ThreadProc = std::function<void()>;

  struct alignas(CACHE_LINE_SIZE) ThreadData {
    std::atomic<uint32_t>      refs;
    HANDLE                     handle;
    DWORD                      id;
    std::atomic<ThreadPriority> priority;
    ThreadProc                 proc;

    explicit ThreadData(ThreadProc&& p)
    : refs(2u)
    , handle(nullptr)
    , id(0u)
    , priority(ThreadPriority::Normal)
    , proc(std::move(p)) {
    }

    ~ThreadData() {
      if (handle) {
        ::CloseHandle(handle);
        handle = nullptr;
      }
    }

    DXVK_FORCE_INLINE void decRef() noexcept {
      const uint32_t old = refs.fetch_sub(1u, std::memory_order_release);
      if (old == 1u) {
        std::atomic_thread_fence(std::memory_order_acquire);
        delete this;
      }
    }
  };

  static_assert(alignof(ThreadData) == CACHE_LINE_SIZE,
    "ThreadData must be cache-line aligned");
  static_assert(sizeof(ThreadData) >= CACHE_LINE_SIZE,
    "ThreadData should span at least one cache line");

  class thread {
  public:
    using id                 = uint32_t;
    using native_handle_type = HANDLE;

    thread() = default;
    explicit thread(ThreadProc&& proc);
    thread(ThreadProc&& proc, ThreadPriority prio);
    ~thread();

    thread(const thread&) = delete;
    thread& operator=(const thread&) = delete;

    thread(thread&& rhs) noexcept
    : m_data(std::exchange(rhs.m_data, nullptr)) {
    }

    thread& operator=(thread&& rhs) noexcept {
      if (joinable())
        std::terminate();

      m_data = std::exchange(rhs.m_data, nullptr);
      return *this;
    }

    DXVK_FORCE_INLINE bool joinable() const noexcept {
      return m_data != nullptr;
    }

    void detach();
    void join();
    void set_priority(ThreadPriority prio);

    DXVK_FORCE_INLINE id get_id() const noexcept {
      return joinable() ? m_data->id : id();
    }

    DXVK_FORCE_INLINE native_handle_type native_handle() const noexcept {
      return joinable() ? m_data->handle : native_handle_type();
    }

    DXVK_FORCE_INLINE void swap(thread& other) noexcept {
      std::swap(m_data, other.m_data);
    }

    static uint32_t hardware_concurrency();

  private:
    ThreadData* m_data = nullptr;

    static DWORD WINAPI threadProc(void* arg) noexcept;
  };

  namespace this_thread {
    DXVK_FORCE_INLINE void yield() noexcept {
      ::SwitchToThread();
    }

    DXVK_FORCE_INLINE thread::id get_id() noexcept {
      return ::GetCurrentThreadId();
    }

    bool isInModuleDetachment() noexcept;
  }

  class mutex {
  public:
    using native_handle_type = PSRWLOCK;

    constexpr mutex() noexcept = default;
    mutex(const mutex&) = delete;
    mutex& operator=(const mutex&) = delete;

    DXVK_FORCE_INLINE void lock() noexcept {
      ::AcquireSRWLockExclusive(&m_lock);
    }

    DXVK_FORCE_INLINE void unlock() noexcept {
      ::ReleaseSRWLockExclusive(&m_lock);
    }

    DXVK_FORCE_INLINE bool try_lock() noexcept {
      return ::TryAcquireSRWLockExclusive(&m_lock) != 0;
    }

    DXVK_FORCE_INLINE native_handle_type native_handle() noexcept {
      return &m_lock;
    }

  private:
    SRWLOCK m_lock = SRWLOCK_INIT;
  };

  class shared_mutex {
  public:
    using native_handle_type = PSRWLOCK;

    shared_mutex() noexcept = default;
    shared_mutex(const shared_mutex&) = delete;
    shared_mutex& operator=(const shared_mutex&) = delete;

    DXVK_FORCE_INLINE void lock() noexcept {
      ::AcquireSRWLockExclusive(&m_lock);
    }

    DXVK_FORCE_INLINE void lock_shared() noexcept {
      ::AcquireSRWLockShared(&m_lock);
    }

    DXVK_FORCE_INLINE void unlock() noexcept {
      ::ReleaseSRWLockExclusive(&m_lock);
    }

    DXVK_FORCE_INLINE void unlock_shared() noexcept {
      ::ReleaseSRWLockShared(&m_lock);
    }

    DXVK_FORCE_INLINE bool try_lock() noexcept {
      return ::TryAcquireSRWLockExclusive(&m_lock) != 0;
    }

    DXVK_FORCE_INLINE bool try_lock_shared() noexcept {
      return ::TryAcquireSRWLockShared(&m_lock) != 0;
    }

    DXVK_FORCE_INLINE native_handle_type native_handle() noexcept {
      return &m_lock;
    }

  private:
    SRWLOCK m_lock = SRWLOCK_INIT;
  };

  class fast_mutex {
  public:
    fast_mutex() {
      ::InitializeCriticalSectionAndSpinCount(&m_lock, 4000u);
    }

    ~fast_mutex() {
      ::DeleteCriticalSection(&m_lock);
    }

    fast_mutex(const fast_mutex&) = delete;
    fast_mutex& operator=(const fast_mutex&) = delete;

    DXVK_FORCE_INLINE void lock() noexcept {
      ::EnterCriticalSection(&m_lock);
    }

    DXVK_FORCE_INLINE void unlock() noexcept {
      ::LeaveCriticalSection(&m_lock);
    }

    DXVK_FORCE_INLINE bool try_lock() noexcept {
      return ::TryEnterCriticalSection(&m_lock) != 0;
    }

  private:
    CRITICAL_SECTION m_lock;
  };

  using spin_mutex = fast_mutex;

  class recursive_mutex {
  public:
    using native_handle_type = PCRITICAL_SECTION;

    recursive_mutex() {
      ::InitializeCriticalSection(&m_lock);
    }

    ~recursive_mutex() {
      ::DeleteCriticalSection(&m_lock);
    }

    recursive_mutex(const recursive_mutex&) = delete;
    recursive_mutex& operator=(const recursive_mutex&) = delete;

    DXVK_FORCE_INLINE void lock() noexcept {
      ::EnterCriticalSection(&m_lock);
    }

    DXVK_FORCE_INLINE void unlock() noexcept {
      ::LeaveCriticalSection(&m_lock);
    }

    DXVK_FORCE_INLINE bool try_lock() noexcept {
      return ::TryEnterCriticalSection(&m_lock) != 0;
    }

    DXVK_FORCE_INLINE native_handle_type native_handle() noexcept {
      return &m_lock;
    }

  private:
    CRITICAL_SECTION m_lock;
  };

  class condition_variable {
  public:
    using native_handle_type = PCONDITION_VARIABLE;

    condition_variable() {
      ::InitializeConditionVariable(&m_cond);
    }

    condition_variable(const condition_variable&) = delete;
    condition_variable& operator=(const condition_variable&) = delete;

    DXVK_FORCE_INLINE void notify_one() noexcept {
      ::WakeConditionVariable(&m_cond);
    }

    DXVK_FORCE_INLINE void notify_all() noexcept {
      ::WakeAllConditionVariable(&m_cond);
    }

    void wait(std::unique_lock<dxvk::mutex>& lock) {
      ::SleepConditionVariableSRW(&m_cond, lock.mutex()->native_handle(), INFINITE, 0u);
    }

    template<typename Pred>
    void wait(std::unique_lock<dxvk::mutex>& lock, Pred pred) {
      while (!pred())
        wait(lock);
    }

    template<class Rep, class Period>
    std::cv_status wait_for(
            std::unique_lock<dxvk::mutex>&         lock,
      const std::chrono::duration<Rep, Period>&    duration) {
      auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(duration);

      if (ms < std::chrono::milliseconds::zero())
        ms = std::chrono::milliseconds::zero();

      const long long cnt = ms.count();
      const DWORD timeout = (cnt <= 0LL)
        ? 0u
        : (cnt >= static_cast<long long>(std::numeric_limits<DWORD>::max())
          ? std::numeric_limits<DWORD>::max()
          : static_cast<DWORD>(cnt));

      const BOOL ok = ::SleepConditionVariableSRW(
        &m_cond, lock.mutex()->native_handle(), timeout, 0u);
      return ok ? std::cv_status::no_timeout : std::cv_status::timeout;
    }

    template<class Rep, class Period, class Pred>
    bool wait_for(
            std::unique_lock<dxvk::mutex>&         lock,
      const std::chrono::duration<Rep, Period>&    duration,
            Pred                                   pred) {
      const auto deadline = std::chrono::steady_clock::now() + duration;

      while (!pred()) {
        if (wait_for(lock, deadline - std::chrono::steady_clock::now()) == std::cv_status::timeout)
          return pred();
      }

      return true;
    }

    template<class Clock, class Duration>
    std::cv_status wait_until(
            std::unique_lock<dxvk::mutex>&          lock,
      const std::chrono::time_point<Clock, Duration>& tp) {
      const auto now = Clock::now();
      return tp <= now ? std::cv_status::timeout : wait_for(lock, tp - now);
    }

    template<class Clock, class Duration, class Pred>
    bool wait_until(
            std::unique_lock<dxvk::mutex>&          lock,
      const std::chrono::time_point<Clock, Duration>& tp,
            Pred                                    pred) {
      while (!pred()) {
        if (wait_until(lock, tp) == std::cv_status::timeout)
          return pred();
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

  #include <pthread.h>
  #include <sched.h>
  #if defined(__linux__)
    #include <unistd.h>
    #include <sys/syscall.h>
  #endif

  class thread : public std::thread {
    using base = std::thread;
  public:
    using base::thread;

    void set_priority(ThreadPriority) {
    }
  };

  using mutex              = std::mutex;
  using shared_mutex       = std::shared_mutex;
  using recursive_mutex    = std::recursive_mutex;
  using condition_variable = std::condition_variable;
  using fast_mutex         = std::mutex;
  using spin_mutex         = std::mutex;

  namespace this_thread {
    DXVK_FORCE_INLINE void yield() noexcept {
      std::this_thread::yield();
    }

    inline uint32_t get_id() {
    #if defined(__linux__)
      return static_cast<uint32_t>(::syscall(SYS_gettid));
    #else
      static std::atomic<uint32_t> ctr { 0u };
      thread_local uint32_t id = 0u;

      if (!id)
        id = ctr.fetch_add(1u, std::memory_order_relaxed) + 1u;

      return id;
    #endif
    }

    DXVK_FORCE_INLINE bool isInModuleDetachment() noexcept {
      return false;
    }
  }

#endif

}
