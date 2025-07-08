#pragma once
/*  thread.h – DXVK threading utilities
 *  ------------------------------------
 *  FINAL, FAIR & HYBRID-CPU AWARE
 */

#include <atomic>
#include <chrono>
#include <condition_variable>
#include <cstdint>
#include <functional>
#include <mutex>
#include <thread>
#include <utility>

/*  DXVK helpers  */
#include "util_error.h"
#include "util_math.h"
#include "util_likely.h"
#include "./com/com_include.h"
#include "./rc/util_rc.h"
#include "./rc/util_rc_ptr.h"

/* --------------------------------------------------------------------- */
#ifndef CACHE_LINE_SIZE
# define CACHE_LINE_SIZE 64
#endif

#if defined(_MSC_VER)
# define DXVK_FORCE_INLINE __forceinline
#elif defined(__GNUC__) || defined(__clang__)
# define DXVK_FORCE_INLINE inline __attribute__((always_inline))
#else
# define DXVK_FORCE_INLINE inline
#endif

/*  Detect x86 for pause / SIMD helpers                                   */
#if defined(__x86_64__) || defined(_M_X64) || \
    defined(__i386__)   || defined(_M_IX86)
# define DXVK_ARCH_X86
#endif

namespace dxvk {

/* ===================================================================== */
/*  Common enum / helpers                                                */
/* ===================================================================== */
enum class ThreadPriority : int32_t { Normal, Lowest };

/* ===================================================================== */
/*  Windows implementation                                               */
/* ===================================================================== */
#ifdef _WIN32

using ThreadProc = std::function<void()>;

struct ThreadData {
  explicit ThreadData(ThreadProc&& p) : proc(std::move(p)) {}
  ~ThreadData() { if (handle) ::CloseHandle(handle); }

  HANDLE                handle = nullptr;
  DWORD                 id     = 0;
  std::atomic<uint32_t> refs   { 2u }; /* owner + thread */
  ThreadProc            proc;

  DXVK_FORCE_INLINE void decRef() {
    if (refs.fetch_sub(1, std::memory_order_release) == 1) {
      std::atomic_thread_fence(std::memory_order_acquire);
      delete this;
    }
  }
};

/* --------------------------------------------------------------------- */
/*  dxvk::thread                                                         */
/* --------------------------------------------------------------------- */
class thread {
public:
  using id                 = uint32_t;
  using native_handle_type = HANDLE;

  thread() = default;

  /*  Classic constructor – starts immediately on default core           */
  explicit thread(ThreadProc&& proc);

  /*  New constructor – steers thread onto P/E cores *before* first instr */
  thread(ThreadProc&& proc, ThreadPriority prio);

  ~thread();

  thread            (const thread&)            = delete;
  thread& operator= (const thread&)            = delete;

  thread            (thread&& rhs) noexcept
  : m_data(std::exchange(rhs.m_data, nullptr)) {}

  thread& operator= (thread&& rhs) noexcept {
    if (joinable()) std::terminate();
    if (m_data)     m_data->decRef();
    m_data = std::exchange(rhs.m_data, nullptr);
    return *this;
  }

  /* ------------------------------------------------------------------ */
  DXVK_FORCE_INLINE bool     joinable()      const { return m_data; }
  void detach();
  void join();
  void set_priority(ThreadPriority);

  DXVK_FORCE_INLINE id                  get_id()        const {
    return joinable() ? m_data->id     : id();
  }
  DXVK_FORCE_INLINE native_handle_type native_handle()  const {
    return joinable() ? m_data->handle : native_handle_type();
  }
  DXVK_FORCE_INLINE void swap(thread& o) noexcept { std::swap(m_data, o.m_data); }

  static uint32_t hardware_concurrency();

private:
  ThreadData*                m_data = nullptr;

  static DWORD  WINAPI threadProc(void*) noexcept;
};

/* --------------------------------------------------------------------- */
/*  this_thread helpers                                                  */
/* --------------------------------------------------------------------- */
namespace this_thread {
  DXVK_FORCE_INLINE void       yield()   { ::SwitchToThread(); }
  DXVK_FORCE_INLINE thread::id get_id()  { return ::GetCurrentThreadId(); }
  bool isInModuleDetachment() noexcept;
}

/* --------------------------------------------------------------------- */
/*  Mutex primitives                                                     */
/* --------------------------------------------------------------------- */
class mutex {
public:
  using native_handle_type = PSRWLOCK;

  constexpr mutex() noexcept = default;
  mutex            (const mutex&)            = delete;
  mutex& operator= (const mutex&)            = delete;

  DXVK_FORCE_INLINE void lock()      noexcept { ::AcquireSRWLockExclusive(&m_lock); }
  DXVK_FORCE_INLINE void unlock()    noexcept { ::ReleaseSRWLockExclusive(&m_lock); }
  DXVK_FORCE_INLINE bool try_lock()  noexcept { return ::TryAcquireSRWLockExclusive(&m_lock) != 0; }
  DXVK_FORCE_INLINE native_handle_type native_handle() noexcept { return &m_lock; }
private:
  SRWLOCK m_lock = SRWLOCK_INIT;
};

/* -----------------------  Pillar 1 — Ticket-Lock ---------------------- */
class fast_mutex {
public:
  fast_mutex()  = default;
  fast_mutex            (const fast_mutex&)            = delete;
  fast_mutex& operator= (const fast_mutex&)            = delete;

  /* FIFO-fair locking */
  void lock() noexcept;
  void unlock() noexcept;
  bool try_lock() noexcept;

private:
  /* 64-bit counter :  [63..32] tail  |  [31..0] head */
  alignas(CACHE_LINE_SIZE) std::atomic<uint64_t> m_ctr { 0 };
};

using spin_mutex = fast_mutex;

/* --------------------------------------------------------------------- */
class recursive_mutex {
public:
  using native_handle_type = PCRITICAL_SECTION;

  recursive_mutex()  { ::InitializeCriticalSection(&m_lock); }
  ~recursive_mutex() { ::DeleteCriticalSection(&m_lock); }

  recursive_mutex            (const recursive_mutex&)            = delete;
  recursive_mutex& operator= (const recursive_mutex&)            = delete;

  DXVK_FORCE_INLINE void lock()      noexcept { ::EnterCriticalSection(&m_lock); }
  DXVK_FORCE_INLINE void unlock()    noexcept { ::LeaveCriticalSection(&m_lock); }
  DXVK_FORCE_INLINE bool try_lock()  noexcept { return ::TryEnterCriticalSection(&m_lock) != 0; }
  DXVK_FORCE_INLINE native_handle_type native_handle() noexcept { return &m_lock; }

private:
  CRITICAL_SECTION m_lock;
};

/* --------------------------------------------------------------------- */
class condition_variable {
public:
  using native_handle_type = PCONDITION_VARIABLE;

  condition_variable()                              { ::InitializeConditionVariable(&m_cond); }
  condition_variable            (const condition_variable&)            = delete;
  condition_variable& operator= (const condition_variable&)            = delete;

  DXVK_FORCE_INLINE void notify_one() noexcept { ::WakeConditionVariable(&m_cond); }
  DXVK_FORCE_INLINE void notify_all() noexcept { ::WakeAllConditionVariable(&m_cond); }

  void wait(std::unique_lock<dxvk::mutex>& lk) {
    ::SleepConditionVariableSRW(&m_cond, lk.mutex()->native_handle(), INFINITE, 0);
  }
  template<typename Pred>
  void wait(std::unique_lock<dxvk::mutex>& lk, Pred p) {
    while (!p()) wait(lk);
  }

  template<class Rep, class Period>
  std::cv_status wait_for(std::unique_lock<dxvk::mutex>& lk,
                          const std::chrono::duration<Rep, Period>& d) {
    auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(d);
    if (ms < std::chrono::milliseconds::zero())
      ms = std::chrono::milliseconds::zero();

    BOOL ok = ::SleepConditionVariableSRW(&m_cond, lk.mutex()->native_handle(),
                                          static_cast<DWORD>(ms.count()), 0);
    return ok ? std::cv_status::no_timeout : std::cv_status::timeout;
  }

  template<class Rep, class Period, class Pred>
  bool wait_for(std::unique_lock<dxvk::mutex>& lk,
                const std::chrono::duration<Rep, Period>& d,
                Pred p) {
    auto deadline = std::chrono::steady_clock::now() + d;
    while (!p()) {
      if (wait_for(lk, deadline - std::chrono::steady_clock::now()) ==
          std::cv_status::timeout)
        return p();
    }
    return true;
  }

  /* … wait_until wrappers identical – omitted for brevity … */
  template<class Clock, class Duration>
  std::cv_status wait_until(std::unique_lock<dxvk::mutex>& lk,
                            const std::chrono::time_point<Clock, Duration>& tp) {
    auto now = Clock::now();
    return tp <= now ? std::cv_status::timeout : wait_for(lk, tp - now);
  }
  template<class Clock, class Duration, class Pred>
  bool wait_until(std::unique_lock<dxvk::mutex>& lk,
                  const std::chrono::time_point<Clock, Duration>& tp,
                  Pred p) {
    while (!p()) {
      if (wait_until(lk, tp) == std::cv_status::timeout)
        return p();
    }
    return true;
  }

  DXVK_FORCE_INLINE native_handle_type native_handle() noexcept { return &m_cond; }

private:
  CONDITION_VARIABLE m_cond;
};

/* ===================================================================== */
/*  POSIX side – unchanged: aliases                                       */
/* ===================================================================== */
#else   /* !_WIN32 */

#include <pthread.h>
#include <sched.h>
#if defined(__linux__)
# include <unistd.h>
#endif

class thread : public std::thread {
  using base = std::thread;
public:
  using base::thread;
  void set_priority(ThreadPriority prio);
};

using mutex              = std::mutex;
using recursive_mutex    = std::recursive_mutex;
using condition_variable = std::condition_variable;
using fast_mutex         = std::mutex;
using spin_mutex         = std::mutex;

namespace this_thread {
  DXVK_FORCE_INLINE void yield() { std::this_thread::yield(); }

  inline uint32_t get_id() {
#   if defined(__linux__)
    return static_cast<uint32_t>(::gettid());
#   else
    static std::atomic<uint32_t> ctr{0};
    thread_local uint32_t id = 0;
    if (!id) id = ++ctr;
    return id;
#   endif
  }
  DXVK_FORCE_INLINE bool isInModuleDetachment() noexcept { return false; }
}

#endif /* _WIN32 */

} /* namespace dxvk */
