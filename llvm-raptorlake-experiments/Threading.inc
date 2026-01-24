//===- Unix/Threading.inc - Unix Threading Implementation ----- -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Unix threading + (Linux) smart ThreadPool worker pinning:
//   - Respects the effective affinity mask (cpusets/cgroups/taskset).
//   - Ranks CPUs by Intel CPUID core type (P-core vs E-core) when available.
//   - Spreads across physical cores first, then SMT siblings.
//   - Best-effort: if any step fails, falls back to "do nothing".
//
//===----------------------------------------------------------------------===//

#include "Unix.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/ScopeExit.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/thread.h"

#include <algorithm>
#include <atomic>
#include <cstdlib>
#include <cstring>
#include <mutex>
#include <optional>
#include <pthread.h>
#include <thread>
#include <utility>

#if defined(__APPLE__)
#include <mach/mach_init.h>
#include <mach/mach_port.h>
#include <pthread/qos.h>
#include <sys/sysctl.h>
#include <sys/types.h>
#endif

#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__DragonFly__)
#include <pthread_np.h> // pthread_getthreadid_np() / pthread_set_name_np()
#endif

#if defined(__FreeBSD__) || defined(__FreeBSD_kernel__)
#include <errno.h>
#include <sys/cpuset.h>
#include <sys/sysctl.h>
#include <sys/user.h>
#include <unistd.h>
#endif

#if defined(__NetBSD__)
#include <lwp.h> // _lwp_self()
#endif

#if defined(__OpenBSD__)
#include <unistd.h> // getthrid()
#endif

#if defined(__linux__)
#include <sched.h>       // sched_getaffinity/sched_setaffinity
#include <sys/syscall.h> // SYS_gettid / __NR_gettid
#include <unistd.h>      // sysconf
#if defined(__i386__) || defined(__x86_64__)
#include <cpuid.h>
#endif
#endif

#if defined(__CYGWIN__)
#include <sys/cpuset.h>
#endif

#if defined(__HAIKU__)
#include <OS.h> // B_OS_NAME_LENGTH
#endif

namespace llvm {

  pthread_t
  llvm_execute_on_thread_impl(void *(*ThreadFunc)(void *), void *Arg,
                              std::optional<unsigned> StackSizeInBytes) {
    int errnum = 0;

    pthread_attr_t Attr;
    if ((errnum = ::pthread_attr_init(&Attr)) != 0)
      ReportErrnumFatal("pthread_attr_init failed", errnum);

    llvm::scope_exit AttrGuard([&Attr, &errnum]() noexcept {
      if ((errnum = ::pthread_attr_destroy(&Attr)) != 0)
        ReportErrnumFatal("pthread_attr_destroy failed", errnum);
    });

    if (StackSizeInBytes) {
      if ((errnum = ::pthread_attr_setstacksize(&Attr, *StackSizeInBytes)) != 0)
        ReportErrnumFatal("pthread_attr_setstacksize failed", errnum);
    }

    pthread_t Thread;
    if ((errnum = ::pthread_create(&Thread, &Attr, ThreadFunc, Arg)) != 0)
      ReportErrnumFatal("pthread_create failed", errnum);

    return Thread;
                              }

                              void llvm_thread_detach_impl(pthread_t Thread) {
                                const int errnum = ::pthread_detach(Thread);
                                if (errnum != 0)
                                  ReportErrnumFatal("pthread_detach failed", errnum);
                              }

                              void llvm_thread_join_impl(pthread_t Thread) {
                                const int errnum = ::pthread_join(Thread, nullptr);
                                if (errnum != 0)
                                  ReportErrnumFatal("pthread_join failed", errnum);
                              }

                              llvm::thread::id llvm_thread_get_id_impl(pthread_t Thread) {
                                #if defined(__MVS__)
                                return Thread.__;
                                #else
                                return Thread;
                                #endif
                              }

                              llvm::thread::id llvm_thread_get_current_id_impl() {
                                return llvm_thread_get_id_impl(::pthread_self());
                              }

} // namespace llvm

uint64_t llvm::get_threadid() {
  #if defined(__APPLE__)
  uint64_t TID = 0;
  if (::pthread_threadid_np(nullptr, &TID) == 0)
    return TID;

  const thread_port_t Self = mach_thread_self();
  mach_port_deallocate(mach_task_self(), Self);
  return static_cast<uint64_t>(Self);

  #elif defined(__FreeBSD__) || defined(__DragonFly__)
  return static_cast<uint64_t>(pthread_getthreadid_np());
  #elif defined(__NetBSD__)
  return static_cast<uint64_t>(_lwp_self());
  #elif defined(__OpenBSD__)
  return static_cast<uint64_t>(getthrid());
  #elif defined(__ANDROID__)
  return static_cast<uint64_t>(gettid());
  #elif defined(__linux__)
  #if defined(SYS_gettid)
  return static_cast<uint64_t>(syscall(SYS_gettid));
  #else
  return static_cast<uint64_t>(syscall(__NR_gettid));
  #endif
  #elif defined(_AIX)
  return static_cast<uint64_t>(thread_self());
  #elif defined(__MVS__)
  return static_cast<uint64_t>(llvm_thread_get_id_impl(pthread_self()));
  #else
  return static_cast<uint64_t>(pthread_self());
  #endif
}

static constexpr uint32_t get_max_thread_name_length_impl() {
  #if defined(PTHREAD_MAX_NAMELEN_NP)
  return PTHREAD_MAX_NAMELEN_NP;
  #elif defined(__HAIKU__)
  return B_OS_NAME_LENGTH;
  #elif defined(__APPLE__)
  return 64;
  #elif defined(__sun__) && defined(__svr4__)
  return 31;
  #elif defined(__linux__) && HAVE_PTHREAD_SETNAME_NP
  return 16;
  #elif defined(__FreeBSD__) || defined(__FreeBSD_kernel__) || \
  defined(__DragonFly__)
  return 16;
  #elif defined(__OpenBSD__)
  return 24;
  #elif defined(__CYGWIN__)
  return 16;
  #else
  return 0;
  #endif
}

uint32_t llvm::get_max_thread_name_length() {
  return get_max_thread_name_length_impl();
}

void llvm::set_thread_name(const Twine &Name) {
  SmallString<64> Storage;
  llvm::StringRef NameStr = Name.toNullTerminatedStringRef(Storage);

  constexpr uint32_t MaxLen = get_max_thread_name_length_impl();
  if (MaxLen > 0)
    NameStr = NameStr.take_back(MaxLen - 1);

  #if defined(HAVE_PTHREAD_SET_NAME_NP) && HAVE_PTHREAD_SET_NAME_NP
  ::pthread_set_name_np(::pthread_self(), NameStr.data());
  #elif defined(HAVE_PTHREAD_SETNAME_NP) && HAVE_PTHREAD_SETNAME_NP
  #if defined(__NetBSD__)
  ::pthread_setname_np(::pthread_self(), "%s",
                       const_cast<char *>(NameStr.data()));
  #elif defined(__APPLE__)
  ::pthread_setname_np(NameStr.data());
  #else
  ::pthread_setname_np(::pthread_self(), NameStr.data());
  #endif
  #endif
}

void llvm::get_thread_name(SmallVectorImpl<char> &Name) {
  Name.clear();

  #if defined(__FreeBSD__) || defined(__FreeBSD_kernel__)
  const int pid = ::getpid();
  const uint64_t tid = get_threadid();

  struct kinfo_proc *kp = nullptr;
  size_t len = 0;
  int ctl[4] = {CTL_KERN, KERN_PROC, KERN_PROC_PID | KERN_PROC_INC_THREAD,
    pid};

    for (;;) {
      int error = sysctl(ctl, 4, kp, &len, nullptr, 0);
      if (kp == nullptr || (error != 0 && errno == ENOMEM)) {
        len += sizeof(*kp) + len / 10;
        struct kinfo_proc *nkp =
        static_cast<struct kinfo_proc *>(::realloc(kp, len));
        if (nkp == nullptr) {
          ::free(kp);
          return;
        }
        kp = nkp;
        continue;
      }
      if (error != 0)
        len = 0;
      break;
    }

    const size_t count = len / sizeof(*kp);
    for (size_t i = 0; i < count; ++i) {
      if (kp[i].ki_tid == static_cast<lwpid_t>(tid)) {
        const size_t nameLen = std::strlen(kp[i].ki_tdname);
        Name.append(kp[i].ki_tdname, kp[i].ki_tdname + nameLen);
        break;
      }
    }
    ::free(kp);

    #elif (defined(__linux__) || defined(__CYGWIN__)) && HAVE_PTHREAD_GETNAME_NP
    constexpr uint32_t Len = get_max_thread_name_length_impl();
    char Buffer[Len] = {};
    if (::pthread_getname_np(::pthread_self(), Buffer, Len) == 0) {
      const size_t ActualLen = std::strlen(Buffer);
      Name.append(Buffer, Buffer + ActualLen);
    }

    #elif defined(HAVE_PTHREAD_GET_NAME_NP) && HAVE_PTHREAD_GET_NAME_NP
    constexpr uint32_t Len = get_max_thread_name_length_impl();
    char Buffer[Len] = {};
    ::pthread_get_name_np(::pthread_self(), Buffer, Len);
    const size_t ActualLen = std::strlen(Buffer);
    Name.append(Buffer, Buffer + ActualLen);

    #elif defined(HAVE_PTHREAD_GETNAME_NP) && HAVE_PTHREAD_GETNAME_NP
    constexpr uint32_t Len = get_max_thread_name_length_impl();
    char Buffer[Len] = {};
    ::pthread_getname_np(::pthread_self(), Buffer, Len);
    const size_t ActualLen = std::strlen(Buffer);
    Name.append(Buffer, Buffer + ActualLen);
    #endif
}

llvm::SetThreadPriorityResult
llvm::set_thread_priority(ThreadPriority Priority) {
  #if (defined(__linux__) || defined(__CYGWIN__)) && defined(SCHED_IDLE)
  sched_param Param = {};
  Param.sched_priority = 0;

  int Policy;
  switch (Priority) {
    case ThreadPriority::Background:
      Policy = SCHED_IDLE;
      break;
    case ThreadPriority::Low:
      #if defined(SCHED_BATCH)
      Policy = SCHED_BATCH;
      #else
      Policy = SCHED_OTHER;
      #endif
      break;
    case ThreadPriority::Default:
      Policy = SCHED_OTHER;
      break;
  }

  return ::pthread_setschedparam(::pthread_self(), Policy, &Param) == 0
  ? SetThreadPriorityResult::SUCCESS
  : SetThreadPriorityResult::FAILURE;

  #elif defined(__APPLE__)
  qos_class_t QosClass;
  switch (Priority) {
    case ThreadPriority::Background:
      QosClass = QOS_CLASS_BACKGROUND;
      break;
    case ThreadPriority::Low:
      QosClass = QOS_CLASS_UTILITY;
      break;
    case ThreadPriority::Default:
      QosClass = QOS_CLASS_DEFAULT;
      break;
  }
  return ::pthread_set_qos_class_self_np(QosClass, 0) == 0
  ? SetThreadPriorityResult::SUCCESS
  : SetThreadPriorityResult::FAILURE;
  #else
  (void)Priority;
  return SetThreadPriorityResult::FAILURE;
  #endif
}

static int computeHostNumHardwareThreads() {
  #if defined(__FreeBSD__)
  cpuset_t Mask;
  CPU_ZERO(&Mask);
  if (cpuset_getaffinity(CPU_LEVEL_WHICH, CPU_WHICH_TID, -1, sizeof(Mask),
    &Mask) == 0)
    return CPU_COUNT(&Mask);

  #elif defined(__linux__) || defined(__CYGWIN__)
  cpu_set_t Set;
  CPU_ZERO(&Set);
  if (sched_getaffinity(0, sizeof(Set), &Set) == 0)
    return CPU_COUNT(&Set);

  const long NumConf = sysconf(_SC_NPROCESSORS_CONF);
  if (NumConf > 0) {
    const size_t Count = static_cast<size_t>(NumConf);
    const size_t Bytes = CPU_ALLOC_SIZE(Count);
    cpu_set_t *DynSet = CPU_ALLOC(Count);
    if (DynSet != nullptr) {
      CPU_ZERO_S(Bytes, DynSet);
      if (sched_getaffinity(0, Bytes, DynSet) == 0) {
        const int Num = CPU_COUNT_S(Bytes, DynSet);
        CPU_FREE(DynSet);
        return Num;
      }
      CPU_FREE(DynSet);
    }
  }
  #endif

  const unsigned Val = std::thread::hardware_concurrency();
  return Val > 0 ? static_cast<int>(Val) : 1;
}

llvm::BitVector llvm::get_thread_affinity_mask() {
  #if defined(__linux__) || defined(__CYGWIN__)
  cpu_set_t Set;
  CPU_ZERO(&Set);
  if (sched_getaffinity(0, sizeof(Set), &Set) == 0) {
    llvm::BitVector BV(CPU_SETSIZE, false);
    for (int i = 0; i < CPU_SETSIZE; ++i)
      if (CPU_ISSET(i, &Set))
        BV.set(static_cast<unsigned>(i));

    const int Last = BV.find_last();
    if (Last >= 0)
      BV.resize(static_cast<unsigned>(Last + 1));
    else
      BV.clear();
    return BV;
  }

  const long NumConf = sysconf(_SC_NPROCESSORS_CONF);
  if (NumConf > 0) {
    const size_t Count = static_cast<size_t>(NumConf);
    const size_t Bytes = CPU_ALLOC_SIZE(Count);
    cpu_set_t *DynSet = CPU_ALLOC(Count);
    if (DynSet != nullptr) {
      CPU_ZERO_S(Bytes, DynSet);
      if (sched_getaffinity(0, Bytes, DynSet) == 0) {
        llvm::BitVector BV(static_cast<unsigned>(Count), false);
        for (size_t i = 0; i < Count; ++i)
          if (CPU_ISSET_S(i, Bytes, DynSet))
            BV.set(static_cast<unsigned>(i));

        CPU_FREE(DynSet);

        const int Last = BV.find_last();
        if (Last >= 0)
          BV.resize(static_cast<unsigned>(Last + 1));
        else
          BV.clear();
        return BV;
      }
      CPU_FREE(DynSet);
    }
  }
  #endif
  return llvm::BitVector();
}

unsigned llvm::get_cpus() {
  const int HW = computeHostNumHardwareThreads();
  return HW > 0 ? static_cast<unsigned>(HW) : 1;
}

//===----------------------------------------------------------------------===//
// Linux smart worker placement (CPUID core type + physical-core-first).
//===----------------------------------------------------------------------===//

#if defined(__linux__)
namespace {

  static std::optional<unsigned> readSysfsUInt(unsigned Cpu, llvm::StringRef Leaf) {
    llvm::SmallString<128> Path;
    llvm::raw_svector_ostream OS(Path);
    OS << "/sys/devices/system/cpu/cpu" << Cpu << "/topology/" << Leaf;

    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> MB =
    llvm::MemoryBuffer::getFileAsStream(OS.str());
    if (!MB)
      return std::nullopt;

    llvm::StringRef S = (*MB)->getBuffer().trim();
    unsigned V = 0;
    if (S.getAsInteger(10, V))
      return std::nullopt;
    return V;
  }

  static uint32_t normalizeIntelCoreType(uint32_t T) {
    // Intel CPUID 0x1A encodes:
    //   0x20 = Atom (E-core), 0x40 = Core (P-core)
    if (T == 0x20 || T == 32 || T == 1)
      return 0x20;
    if (T == 0x40 || T == 64 || T == 2)
      return 0x40;
    return 0;
  }

  static int coreTypeRank(uint32_t NormType) {
    // Lower rank = preferred.
    if (NormType == 0x40) // P-core
      return 0;
    if (NormType == 0x20) // E-core
      return 1;
    return 2; // unknown
  }

  #if defined(__i386__) || defined(__x86_64__)
  static uint32_t cpuidCoreTypeOnCurrentCPU() {
    unsigned MaxLeaf = __get_cpuid_max(0, nullptr);
    if (MaxLeaf < 0x1A)
      return 0;

    unsigned EAX = 0, EBX = 0, ECX = 0, EDX = 0;
    __cpuid_count(0x1A, 0, EAX, EBX, ECX, EDX);
    return (EAX >> 24) & 0xFF;
  }
  #endif

  static bool setCurrentThreadAffinitySingleCPU(unsigned Cpu, size_t Count) {
    if (Count == 0)
      return false;
    const size_t Bytes = CPU_ALLOC_SIZE(Count);
    cpu_set_t *Set = CPU_ALLOC(Count);
    if (!Set)
      return false;

    CPU_ZERO_S(Bytes, Set);
    CPU_SET_S(Cpu, Bytes, Set);
    const int RC = sched_setaffinity(0, Bytes, Set);
    CPU_FREE(Set);
    return RC == 0;
  }

  static bool restoreCurrentThreadAffinity(const llvm::BitVector &Allowed) {
    if (Allowed.empty())
      return false;

    const size_t Count = static_cast<size_t>(Allowed.size());
    const size_t Bytes = CPU_ALLOC_SIZE(Count);
    cpu_set_t *Set = CPU_ALLOC(Count);
    if (!Set)
      return false;

    CPU_ZERO_S(Bytes, Set);
    for (int I = Allowed.find_first(); I >= 0; I = Allowed.find_next(I))
      CPU_SET_S(static_cast<size_t>(I), Bytes, Set);

    const int RC = sched_setaffinity(0, Bytes, Set);
    CPU_FREE(Set);
    return RC == 0;
  }

  struct CoreGroup {
    uint64_t Key;                         // (pkg<<32)|core_id
    uint32_t NormType;                    // 0x40 P, 0x20 E, 0 unknown
    llvm::SmallVector<unsigned, 2> CPUs;  // SMT siblings
  };

  static llvm::SmallVector<unsigned, 64>
  computePreferredCPUOrder(const llvm::BitVector &Allowed) {
    llvm::SmallVector<unsigned, 64> Order;
    if (Allowed.empty())
      return Order;

    llvm::SmallVector<unsigned, 64> CPUs;
    for (int I = Allowed.find_first(); I >= 0; I = Allowed.find_next(I))
      CPUs.push_back(static_cast<unsigned>(I));
    if (CPUs.size() <= 1) {
      Order = CPUs;
      return Order;
    }

    const size_t AffCount = static_cast<size_t>(Allowed.size());
    llvm::SmallVector<CoreGroup, 64> Groups;
    Groups.reserve(CPUs.size());

    auto findOrCreateGroup = [&](uint64_t Key) -> CoreGroup & {
      for (auto &G : Groups)
        if (G.Key == Key)
          return G;
      Groups.push_back(CoreGroup{Key, 0, {}});
      return Groups.back();
    };

    for (unsigned Cpu : CPUs) {
      unsigned Pkg = 0, Core = Cpu;

      if (auto V = readSysfsUInt(Cpu, "physical_package_id"))
        Pkg = *V;
      if (auto V = readSysfsUInt(Cpu, "core_id"))
        Core = *V;

      uint64_t Key = (static_cast<uint64_t>(Pkg) << 32) | static_cast<uint64_t>(Core);
      CoreGroup &G = findOrCreateGroup(Key);
      G.CPUs.push_back(Cpu);

      uint32_t NormType = 0;
      if (auto T = readSysfsUInt(Cpu, "core_type"))
        NormType = normalizeIntelCoreType(*T);
      #if defined(__i386__) || defined(__x86_64__)
      if (NormType == 0) {
        // Best-effort probe: pin to Cpu (within Allowed), CPUID, restore later.
        if (setCurrentThreadAffinitySingleCPU(Cpu, AffCount))
          NormType = normalizeIntelCoreType(cpuidCoreTypeOnCurrentCPU());
      }
      #endif

      if (coreTypeRank(NormType) < coreTypeRank(G.NormType))
        G.NormType = NormType;
      if (G.NormType == 0 && NormType != 0)
        G.NormType = NormType;
    }

    // Restore affinity after probing (best-effort).
    (void)restoreCurrentThreadAffinity(Allowed);

    // Sort SMT siblings within each core: lowest CPU id first.
    for (auto &G : Groups)
      std::sort(G.CPUs.begin(), G.CPUs.end());

    // Sort cores by (core_type_rank, Key) so P-core physical cores come first.
    std::sort(Groups.begin(), Groups.end(), [](const CoreGroup &A, const CoreGroup &B) {
      const int RA = coreTypeRank(A.NormType);
      const int RB = coreTypeRank(B.NormType);
      if (RA != RB)
        return RA < RB;
      return A.Key < B.Key;
    });

    // Physical cores first: emit one CPU per core.
    for (const auto &G : Groups)
      if (!G.CPUs.empty())
        Order.push_back(G.CPUs.front());

    // Then SMT siblings.
    for (const auto &G : Groups)
      for (size_t i = 1; i < G.CPUs.size(); ++i)
        Order.push_back(G.CPUs[i]);

    return Order;
  }

  struct CpuOrderCache {
    llvm::BitVector Mask;
    llvm::SmallVector<unsigned, 64> Order;
    bool Valid = false;
  };

  static std::mutex CpuCacheMu;
  static CpuOrderCache CpuCache;

  static llvm::SmallVector<unsigned, 64>
  getCachedPreferredCPUOrder(const llvm::BitVector &Allowed) {
    std::lock_guard<std::mutex> Lock(CpuCacheMu);
    if (!CpuCache.Valid || CpuCache.Mask != Allowed) {
      CpuCache.Mask = Allowed;
      CpuCache.Order = computePreferredCPUOrder(Allowed);
      CpuCache.Valid = true;
    }
    return CpuCache.Order;
  }

  static void pinCurrentThreadToCPU(unsigned Cpu) {
    // Fast path: cpu_set_t supports up to CPU_SETSIZE.
    if (Cpu < CPU_SETSIZE) {
      cpu_set_t Set;
      CPU_ZERO(&Set);
      CPU_SET(Cpu, &Set);
      (void)sched_setaffinity(0, sizeof(Set), &Set);
      return;
    }

    // Dynamic fallback.
    const size_t Count = static_cast<size_t>(Cpu) + 1;
    const size_t Bytes = CPU_ALLOC_SIZE(Count);
    cpu_set_t *Set = CPU_ALLOC(Count);
    if (!Set)
      return;
    CPU_ZERO_S(Bytes, Set);
    CPU_SET_S(static_cast<size_t>(Cpu), Bytes, Set);
    (void)sched_setaffinity(0, Bytes, Set);
    CPU_FREE(Set);
  }

} // namespace
#endif // __linux__

void llvm::ThreadPoolStrategy::apply_thread_strategy(unsigned ThreadPoolNum) const {
  #if defined(__linux__)
  // Respect the effective affinity mask (container/cgroup/taskset/cpuset).
  const llvm::BitVector Allowed = llvm::get_thread_affinity_mask();
  if (Allowed.empty() || Allowed.count() <= 1)
    return;

  // Compute preferred CPU order within Allowed.
  const llvm::SmallVector<unsigned, 64> Order = getCachedPreferredCPUOrder(Allowed);
  if (Order.empty())
    return;

  // Use ThreadPoolNum as the stable "worker index", but salt by strategy address
  // so multiple pools don't deterministically collide on CPU 0..N.
  const uintptr_t Salt = (reinterpret_cast<uintptr_t>(this) >> 4);
  const unsigned Index = ThreadPoolNum + static_cast<unsigned>(Salt);

  const unsigned Cpu = Order[Index % Order.size()];
  // Best-effort: if this fails due to permissions, we simply don't pin.
  pinCurrentThreadToCPU(Cpu);
  #else
  (void)ThreadPoolNum;
  #endif
}

#if (defined(__linux__) || defined(__CYGWIN__)) && \
(defined(__i386__) || defined(__x86_64__))

static int computeHostNumPhysicalCores() {
  const llvm::BitVector Allowed = llvm::get_thread_affinity_mask();
  if (Allowed.empty())
    return computeHostNumHardwareThreads();

  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> Text =
  llvm::MemoryBuffer::getFileAsStream("/proc/cpuinfo");
  if (!Text)
    return static_cast<int>(Allowed.count());

  llvm::SmallVector<llvm::StringRef, 8> Lines;
  (*Text)->getBuffer().split(Lines, "\n", /*MaxSplit=*/-1,
                             /*KeepEmpty=*/false);

  int CurProcessor = -1;
  int CurPhysicalId = -1;
  int CurCoreId = -1;

  llvm::DenseSet<uint64_t> EnabledCores;

  for (const llvm::StringRef &Line : Lines) {
    const std::pair<llvm::StringRef, llvm::StringRef> KV = Line.split(':');
    const llvm::StringRef FieldName = KV.first.trim();
    const llvm::StringRef FieldVal = KV.second.trim();
    if (FieldName.empty())
      continue;

    const char FirstChar = FieldName.front();
    if (FirstChar == 'p') {
      if (FieldName == "processor") {
        FieldVal.getAsInteger(10, CurProcessor);
        CurPhysicalId = -1;
        CurCoreId = -1;
      } else if (FieldName == "physical id") {
        FieldVal.getAsInteger(10, CurPhysicalId);
      }
    } else if (FirstChar == 'c') {
      if (FieldName == "core id") {
        FieldVal.getAsInteger(10, CurCoreId);

        if (CurProcessor >= 0 && CurPhysicalId >= 0 && CurCoreId >= 0) {
          const unsigned CPU = static_cast<unsigned>(CurProcessor);
          if (CPU < Allowed.size() && Allowed.test(CPU)) {
            const uint64_t Key =
            (static_cast<uint64_t>(static_cast<uint32_t>(CurPhysicalId))
            << 32) |
            static_cast<uint32_t>(CurCoreId);
            EnabledCores.insert(Key);
          }
        }
      }
    }
  }

  const int Count = static_cast<int>(EnabledCores.size());
  if (Count > 0)
    return Count;

  return static_cast<int>(Allowed.count());
}

#elif (defined(__linux__) && defined(__s390x__)) || defined(_AIX)

static int computeHostNumPhysicalCores() {
  const long Cores = sysconf(_SC_NPROCESSORS_ONLN);
  return Cores > 0 ? static_cast<int>(Cores) : -1;
}

#elif defined(__linux__)

static int computeHostNumPhysicalCores() {
  cpu_set_t Affinity;
  CPU_ZERO(&Affinity);
  if (sched_getaffinity(0, sizeof(Affinity), &Affinity) == 0)
    return CPU_COUNT(&Affinity);

  cpu_set_t *DynAffinity = CPU_ALLOC(2048);
  if (DynAffinity == nullptr)
    return -1;

  const size_t Bytes = CPU_ALLOC_SIZE(2048);
  CPU_ZERO_S(Bytes, DynAffinity);

  if (sched_getaffinity(0, Bytes, DynAffinity) == 0) {
    const int NumCPUs = CPU_COUNT_S(Bytes, DynAffinity);
    CPU_FREE(DynAffinity);
    return NumCPUs;
  }

  CPU_FREE(DynAffinity);
  return -1;
}

#elif defined(__APPLE__)

static int computeHostNumPhysicalCores() {
  uint32_t Count = 0;
  size_t Len = sizeof(Count);

  if (sysctlbyname("hw.physicalcpu", &Count, &Len, nullptr, 0) == 0 &&
    Count > 0)
    return static_cast<int>(Count);

  int Mib[2] = {CTL_HW, HW_AVAILCPU};
  if (sysctl(Mib, 2, &Count, &Len, nullptr, 0) == 0 && Count > 0)
    return static_cast<int>(Count);

  return -1;
}

#elif defined(__MVS__)

static int computeHostNumPhysicalCores() {
  enum : int {
    FLCCVT = 16,
    CVTCSD = 660,
    CSD_NUMBER_ONLINE_STANDARD_CPS = 264,
  };

  const char *PSA = nullptr;
  const char *CVT = reinterpret_cast<const char *>(static_cast<uintptr_t>(
    reinterpret_cast<const unsigned int &>(PSA[FLCCVT])));
  const char *CSD = reinterpret_cast<const char *>(static_cast<uintptr_t>(
    reinterpret_cast<const unsigned int &>(CVT[CVTCSD])));
  return reinterpret_cast<const int &>(CSD[CSD_NUMBER_ONLINE_STANDARD_CPS]);
}

#else

static int computeHostNumPhysicalCores() {
  return -1;
}

#endif

int llvm::get_physical_cores() {
  static const int NumCores = computeHostNumPhysicalCores();
  return NumCores;
}
