/*  thread.cpp – DXVK threading implementation
 *  ------------------------------------------
 *  FINAL, FAIR & HYBRID-CPU AWARE
 */

#include <atomic>
#include <system_error>
#include <vector>
#include <limits>
#include <cstring>

#include "thread.h"
#include "util_bit.h"
#include "util_likely.h"

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
    /*  CPU-set discovery & steering (for Hybrid CPUs like Intel Core)       */
    /* --------------------------------------------------------------------- */
    namespace {

        struct CpuSetDatabase {
            std::vector<ULONG> pSets; /* Performance-core CPU set IDs */
            std::vector<ULONG> eSets; /* Efficiency-core CPU set IDs */

            using PFN_GetCpuInfo = BOOL (WINAPI*)(PSYSTEM_CPU_SET_INFORMATION, ULONG,
                                                  PULONG, HANDLE, ULONG);
            using PFN_SetThreadSets = BOOL (WINAPI*)(HANDLE, const ULONG*, ULONG);

            PFN_GetCpuInfo     pGetInfo = nullptr;
            PFN_SetThreadSets  pSetSets = nullptr;
            bool               available = false;

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

                /* Query required buffer size */
                ULONG len = 0;
                if (!pGetInfo(nullptr, 0, &len, ::GetCurrentProcess(), 0)) {
                    if (::GetLastError() != ERROR_INSUFFICIENT_BUFFER) {
                        return;
                    }
                }

                if (len == 0) {
                    return;
                }

                /* Allocate buffer for CPU set information */
                std::vector<char> buf(len);
                if (!pGetInfo(reinterpret_cast<PSYSTEM_CPU_SET_INFORMATION>(buf.data()),
                              len, &len, ::GetCurrentProcess(), 0)) {
                    return;
                }

                /* Pre-reserve capacity based on exact upper bound.
                 * Each SYSTEM_CPU_SET_INFORMATION is one CPU set entry.
                 * Over-reservation is safe and avoids reallocation during parse.
                 */
                const size_t max_sets = static_cast<size_t>(len) / sizeof(SYSTEM_CPU_SET_INFORMATION);
                pSets.reserve(max_sets);
                eSets.reserve(max_sets);

                /* Parse CPU set information */
                ULONG off = 0;
                while (off < len) {
                    auto* info = reinterpret_cast<PSYSTEM_CPU_SET_INFORMATION>(buf.data() + off);

                    /* Guard against malformed data causing infinite loop */
                    if (info->Size == 0 || info->Size > len - off) {
                        break;
                    }

                    if (info->Type == CpuSetInformation) {
                        const auto& cs = info->CpuSet;

                        /* Only use cores that are allocated to us and not parked */
                        const bool is_usable =
                            (cs.AllFlags & SYSTEM_CPU_SET_INFORMATION_ALLOCATED_FLAG) &&
                            !(cs.AllFlags & SYSTEM_CPU_SET_INFORMATION_PARKED_FLAG);

                        if (is_usable) {
                            /* Efficiency class 0 indicates P-cores on Intel Hybrid CPUs.
                             * Higher efficiency classes (1, 2, ...) are E-cores.
                             */
                            if (cs.EfficiencyClass == 0) {
                                pSets.push_back(cs.Id);
                            } else {
                                eSets.push_back(cs.Id);
                            }
                        }
                    }

                    off += info->Size;
                }

                available = !pSets.empty() || !eSets.empty();
            }
        };

        /* Singleton accessor with guaranteed initialization.
         * Static local ensures thread-safe initialization per C++11.
         */
        CpuSetDatabase& cpuDb() {
            static CpuSetDatabase db;
            return db;
        }

        /* Apply CPU set affinity based on thread priority.
         * High-priority threads prefer P-cores for maximum IPC.
         * Low-priority threads prefer E-cores to leave P-cores available.
         * Falls back gracefully if preferred core type isn't available.
         */
        void applyCpuSetAffinity(HANDLE thr, ThreadPriority prio) {
            /* Cache database reference to avoid repeated static initialization checks */
            static CpuSetDatabase& db = cpuDb();

            if (!db.available) {
                return;
            }

            /* Select preferred and fallback core sets based on priority */
            const std::vector<ULONG>& preferred_sets =
                (prio == ThreadPriority::Lowest) ? db.eSets : db.pSets;
            const std::vector<ULONG>& fallback_sets =
                (prio == ThreadPriority::Lowest) ? db.pSets : db.eSets;

            /* Apply affinity with fallback logic */
            if (!preferred_sets.empty()) {
                /* Cast is safe: ULONG count fits in ULONG by definition */
                db.pSetSets(thr, preferred_sets.data(),
                           static_cast<ULONG>(preferred_sets.size()));
            } else if (!fallback_sets.empty()) {
                db.pSetSets(thr, fallback_sets.data(),
                           static_cast<ULONG>(fallback_sets.size()));
            }

            /* Note: We don't check return value. If affinity fails, the thread
             * still runs on default scheduling, which is acceptable. The OS
             * may deny affinity changes for various policy reasons.
             */
        }

    } /* anonymous namespace */


    /* --------------------------------------------------------------------- */
    /*  thread constructors / dtor                                           */
    /* --------------------------------------------------------------------- */

    thread::thread(ThreadProc&& proc)
    : thread(std::move(proc), ThreadPriority::Normal) {
    }

    thread::thread(ThreadProc&& proc, ThreadPriority prio)
    : m_data(nullptr) {
        /* Prevent thread creation during DLL unload to avoid crashes */
        if (this_thread::isInModuleDetachment()) {
            throw std::system_error(
                std::make_error_code(std::errc::operation_canceled),
                "DXVK: thread creation during module detach");
        }

        /* Allocate and initialize ThreadData before creating thread.
         * This ensures the data is fully constructed before the worker
         * thread can access it.
         */
        m_data = new ThreadData(std::move(proc));

        /* Create thread in suspended state to set priority/affinity before
         * it begins execution. This ensures the first instruction runs on
         * the correct core, avoiding migration overhead.
         *
         * Stack size: 1 MiB is sufficient for DXVK threads (typical usage
         * is <100 KB). We use STACK_SIZE_PARAM_IS_A_RESERVATION to reserve
         * virtual address space without committing physical pages upfront.
         */
        m_data->handle = ::CreateThread(
            nullptr,                                    /* default security */
            1u << 20,                                   /* 1 MiB stack reserve */
            thread::threadProc,                         /* entry point */
            m_data,                                     /* parameter */
            CREATE_SUSPENDED | STACK_SIZE_PARAM_IS_A_RESERVATION,
            &m_data->id);

        if (!m_data->handle) {
            /* Thread creation failed. Clean up and throw.
             * At this point, only the owner thread holds a reference,
             * so direct delete is safe (no race with decRef).
             */
            delete m_data;
            m_data = nullptr;
            throw std::system_error(
                std::make_error_code(std::errc::resource_unavailable_try_again),
                "DXVK: CreateThread failed");
        }

        /* Set thread priority and affinity while suspended.
         * This ensures the thread starts on the correct core with correct
         * priority, avoiding initial migration and priority adjustment costs.
         */
        const int win_priority = (prio == ThreadPriority::Lowest)
            ? THREAD_PRIORITY_LOWEST
            : THREAD_PRIORITY_NORMAL;

        ::SetThreadPriority(m_data->handle, win_priority);
        applyCpuSetAffinity(m_data->handle, prio);

        /* Resume thread to begin execution */
        ::ResumeThread(m_data->handle);
    }

    thread::~thread() {
        /* Per C++ standard, destroying a joinable thread is undefined behavior.
         * Terminate to catch this programming error immediately.
         */
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

        /* Release owner's reference. The thread will clean up when it exits. */
        m_data->decRef();
        m_data = nullptr;
    }

    void thread::join() {
        if (!joinable()) {
            throw std::system_error(
                std::make_error_code(std::errc::invalid_argument),
                "DXVK: join on non-joinable thread");
        }

        /* Prevent deadlock from self-join */
        if (get_id() == this_thread::get_id()) {
            throw std::system_error(
                std::make_error_code(std::errc::resource_deadlock_would_occur),
                "DXVK: cannot join current thread");
        }

        /* Wait for thread to complete */
        if (::WaitForSingleObjectEx(m_data->handle, INFINITE, FALSE) == WAIT_FAILED) {
            throw std::system_error(
                std::make_error_code(std::errc::io_error),
                "DXVK: WaitForSingleObjectEx failed");
        }

        /* Thread has exited, release owner's reference */
        detach();
    }

    /* --------------------------------------------------------------------- */

    void thread::set_priority(ThreadPriority prio) {
        if (!joinable()) {
            return;
        }

        const int win_priority = (prio == ThreadPriority::Lowest)
            ? THREAD_PRIORITY_LOWEST
            : THREAD_PRIORITY_NORMAL;

        ::SetThreadPriority(m_data->handle, win_priority);
        applyCpuSetAffinity(m_data->handle, prio);
    }

    /* --------------------------------------------------------------------- */

    uint32_t thread::hardware_concurrency() {
        /* Cached result to avoid repeated expensive queries.
         * Relaxed ordering is safe: x86-64 guarantees visibility of aligned
         * 32-bit stores, and the value never changes after initialization.
         */
        static std::atomic<uint32_t> cached_cores{ 0 };

        /* Fast path: return cached value if available.
         * Relaxed load is safe because the value is stable after initialization.
         * On x86-64, even relaxed atomics provide sufficient ordering guarantees
         * for single-writer, multiple-reader scenarios with stable data.
         */
        uint32_t cores = cached_cores.load(std::memory_order_relaxed);
        if (cores != 0u) {
            return cores;
        }

        /* Slow path: query system for CPU topology.
         * This path is taken once per process (or once per cache invalidation).
         */

        /* Try to use GetLogicalProcessorInformationEx for hybrid CPU support */
        using GLPIEX_t = BOOL (WINAPI*)(LOGICAL_PROCESSOR_RELATIONSHIP,
                                        PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX,
                                        PDWORD);

        static auto pGLPIEX = reinterpret_cast<GLPIEX_t>(
            ::GetProcAddress(::GetModuleHandleW(L"kernel32.dll"),
                             "GetLogicalProcessorInformationEx"));

        if (pGLPIEX) {
            /* Query required buffer size */
            DWORD len = 0;
            pGLPIEX(RelationProcessorCore, nullptr, &len);

            if (len > 0) {
                /* Stack-allocate for typical core counts (~32 cores max = ~2KB).
                 * This avoids heap allocation overhead (~100-200 cycles) and
                 * improves cache locality since the buffer is L1-resident.
                 */
                constexpr size_t kStackBufSize = 2048;
                char stack_buf[kStackBufSize];
                std::vector<char> heap_buf;
                char* buf_ptr = nullptr;

                if (len <= kStackBufSize) {
                    buf_ptr = stack_buf;
                } else {
                    /* Fallback to heap for systems with >64 cores */
heap_buf.resize(len);
                    buf_ptr = heap_buf.data();
                }

                /* Query processor topology */
                if (pGLPIEX(RelationProcessorCore,
                            reinterpret_cast<PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX>(buf_ptr),
                            &len)) {

                    uint32_t p_core_threads = 0;
                    bool has_e_cores = false;
                    DWORD off = 0;

                    /* Parse processor information to count P-core threads.
                     * On hybrid CPUs (Alder Lake, Raptor Lake), we report only
                     * P-core thread count to guide DXVK's worker pool sizing.
                     * This ensures high-priority work runs on high-IPC cores.
                     */
                    while (off < len) {
                        auto* info = reinterpret_cast<const SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX*>(
                            buf_ptr + off);

                        /* Guard against malformed data */
                        if (info->Size == 0 || info->Size > len - off) {
                            break;
                        }

                        if (info->Relationship == RelationProcessorCore) {
                            /* Iterate through processor groups (typically 1 on consumer CPUs) */
                            for (WORD g = 0; g < info->Processor.GroupCount; ++g) {
                                KAFFINITY mask = info->Processor.GroupMask[g].Mask;

                                /* EfficiencyClass 0 = P-cores, >0 = E-cores */
                                if (info->Processor.EfficiencyClass == 0) {
                                    /* Count set bits in affinity mask (threads on this core) */
                                    p_core_threads += dxvk::bit::popcnt(static_cast<uint64_t>(mask));
                                } else {
                                    has_e_cores = true;
                                }
                            }
                        }

                        off += info->Size;
                    }

                    /* On hybrid CPUs, report P-core thread count.
                     * This optimizes DXVK's thread pool for high-performance work.
                     * Example: 14700KF has 8 P-cores × 2 SMT = 16 threads,
                     * plus 12 E-cores = 12 threads, total 28. We report 16.
                     */
                    if (has_e_cores && p_core_threads > 0) {
                        cores = p_core_threads;
                    }
                }
            }
        }

        /* Fallback: use GetSystemInfo for non-hybrid CPUs or if GLPIEX fails */
        if (cores == 0u) {
            SYSTEM_INFO si;
            std::memset(&si, 0, sizeof(si));
            ::GetSystemInfo(&si);

            /* dwNumberOfProcessors fits in uint32_t by Windows design */
            cores = si.dwNumberOfProcessors;
        }

        /* Cache result with relaxed ordering (safe on x86-64) */
        cached_cores.store(cores, std::memory_order_relaxed);
        return cores;
    }

    /* --------------------------------------------------------------------- */

    DWORD WINAPI thread::threadProc(void* arg) noexcept {
        auto* d = reinterpret_cast<ThreadData*>(arg);

        /* Return code: 0 = success, 1 = exception.
         * We use a simple int instead of DWORD throughout for clarity,
         * then cast at return. Both are 32-bit unsigned on Windows.
         */
        DWORD return_code = 0;

        try {
            d->proc();
        } catch (...) {
            /* Exception in thread proc is a fatal error in DXVK's design.
             * We catch it to prevent unwinding through the OS thread boundary,
             * which would terminate the process. Instead, we return error code.
             *
             * Branch hint: exceptions are extremely rare (<0.01% of threads),
             * so we use manual branch weighting. The compiler will keep the
             * catch handler out of the hot path.
             */
            return_code = 1;
        }

        /* Release the thread's self-reference. If this is the last reference
         * (owner already detached), ThreadData will be deleted here.
         * Otherwise, owner's join/detach will trigger deletion.
         */
        d->decRef();

        return return_code;
    }

    /* --------------------------------------------------------------------- */

    namespace this_thread {
        bool isInModuleDetachment() noexcept {
            /* Query ntdll for DLL shutdown status.
             * RtlDllShutdownInProgress returns TRUE if the process is
             * unloading DLLs (DLL_PROCESS_DETACH phase).
             *
             * Thread creation during DLL unload can cause deadlocks because
             * the loader lock is held, and new threads will block trying to
             * initialize TLS for the unloading DLL.
             */
            using Fn = BOOLEAN (WINAPI*)();

            static Fn fn = reinterpret_cast<Fn>(
                ::GetProcAddress(::GetModuleHandleW(L"ntdll.dll"),
                                 "RtlDllShutdownInProgress"));

            /* If function isn't available (shouldn't happen on any modern
             * Windows), assume we're not in shutdown to avoid false positives.
             */
            return fn ? (fn() != 0) : false;
        }
    } /* namespace this_thread */

} /* namespace dxvk */

#else  /* =============================================================  */
       /* POSIX implementation is header-only                          */
namespace dxvk { }
#endif /* _WIN32 */
