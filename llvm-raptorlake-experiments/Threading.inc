//===- Unix/Threading.inc - Unix Threading Implementation ----- -*- C++ -*-===//
//
// Part of the the LLVM Project, under the Apache License v2.0 with LLVM
// Exceptions. See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file provides the Unix specific implementation of Threading functions.
//
//===----------------------------------------------------------------------===//

#include "Unix.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/ScopeExit.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

#if defined(__APPLE__)
#include <mach/mach_init.h>
#include <mach/mach_port.h>
#include <pthread/qos.h>
#include <sys/sysctl.h>
#include <sys/types.h>
#endif

#include <pthread.h>
#include <cstring> // for strlen

#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__DragonFly__)
#include <pthread_np.h> // For pthread_getthreadid_np() / pthread_set_name_np()
#endif

// Must be included after Threading.inc to provide definition for llvm::thread
// because FreeBSD's condvar.h (included by user.h) misuses the "thread"
// keyword.
#ifndef __FreeBSD__
#include "llvm/Support/thread.h"
#endif

#if defined(__FreeBSD__) || defined(__FreeBSD_kernel__)
#include <errno.h>
#include <sys/cpuset.h>
#include <sys/sysctl.h>
#include <sys/user.h>
#include <unistd.h>
#endif

#if defined(__NetBSD__)
#include <lwp.h> // For _lwp_self()
#endif

#if defined(__OpenBSD__)
#include <unistd.h> // For getthrid()
#endif

#if defined(__linux__)
#include <sched.h>       // For sched_getaffinity
#include <sys/syscall.h> // For syscall codes
#include <unistd.h>      // For syscall(), sysconf()
#endif

#if defined(__CYGWIN__)
#include <sys/cpuset.h>
#endif

#if defined(__HAIKU__)
#include <OS.h> // For B_OS_NAME_LENGTH
#endif

namespace llvm {
pthread_t
llvm_execute_on_thread_impl(void *(*ThreadFunc)(void *), void *Arg,
                            std::optional<unsigned> StackSizeInBytes) {
  int errnum;

  // Construct the attributes object.
  pthread_attr_t Attr;
  if ((errnum = ::pthread_attr_init(&Attr)) != 0) {
    ReportErrnumFatal("pthread_attr_init failed", errnum);
  }

  auto AttrGuard = llvm::make_scope_exit([&] {
    if ((errnum = ::pthread_attr_destroy(&Attr)) != 0) {
      ReportErrnumFatal("pthread_attr_destroy failed", errnum);
    }
  });

  // Set the requested stack size, if given.
  if (StackSizeInBytes) {
    if ((errnum = ::pthread_attr_setstacksize(&Attr, *StackSizeInBytes)) != 0) {
      ReportErrnumFatal("pthread_attr_setstacksize failed", errnum);
    }
  }

  // Construct and execute the thread.
  pthread_t Thread;
  if ((errnum = ::pthread_create(&Thread, &Attr, ThreadFunc, Arg)) != 0)
    ReportErrnumFatal("pthread_create failed", errnum);

  return Thread;
}

void llvm_thread_detach_impl(pthread_t Thread) {
  int errnum;

  if ((errnum = ::pthread_detach(Thread)) != 0) {
    ReportErrnumFatal("pthread_detach failed", errnum);
  }
}

void llvm_thread_join_impl(pthread_t Thread) {
  int errnum;

  if ((errnum = ::pthread_join(Thread, nullptr)) != 0) {
    ReportErrnumFatal("pthread_join failed", errnum);
  }
}

pthread_t llvm_thread_get_id_impl(pthread_t Thread) { return Thread; }

pthread_t llvm_thread_get_current_id_impl() { return ::pthread_self(); }

} // namespace llvm

uint64_t llvm::get_threadid() {
#if defined(__APPLE__)
  // Calling "mach_thread_self()" bumps the reference count on the thread
  // port, so we need to deallocate it when no longer needed. Here we cache
  // the port for the lifetime of the thread.
  static thread_local thread_port_t Self = [] {
    thread_port_t InitSelf = mach_thread_self();
    // Note: we intentionally do not deallocate InitSelf here, as we keep it
    // cached for the thread lifetime. The OS will reclaim it when the thread
    // exits.
    return InitSelf;
  }();
  return Self;
#elif defined(__FreeBSD__) || defined(__DragonFly__)
  return uint64_t(pthread_getthreadid_np());
#elif defined(__NetBSD__)
  return uint64_t(_lwp_self());
#elif defined(__OpenBSD__)
  return uint64_t(getthrid());
#elif defined(__ANDROID__)
  return uint64_t(gettid());
#elif defined(__linux__)
  return uint64_t(syscall(__NR_gettid));
#elif defined(_AIX)
  return uint64_t(thread_self());
#else
  return uint64_t(pthread_self());
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
#elif defined(__FreeBSD__) || defined(__FreeBSD_kernel__) ||                   \
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
  // Make sure the input is null terminated.
  SmallString<64> Storage;
  StringRef NameStr = Name.toNullTerminatedStringRef(Storage);

  // Truncate from the beginning, not the end, if the specified name is too
  // long. For one, this ensures that the resulting string is still null
  // terminated, but additionally the end of a long thread name will usually
  // be more unique than the beginning, since a common pattern is for similar
  // threads to share a common prefix.
  // Note that the name length includes the null terminator.
  if (get_max_thread_name_length() > 0)
    NameStr = NameStr.take_back(get_max_thread_name_length() - 1);
  (void)NameStr;
#if defined(HAVE_PTHREAD_SET_NAME_NP)
  ::pthread_set_name_np(::pthread_self(), NameStr.data());
#elif defined(HAVE_PTHREAD_SETNAME_NP)
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
  int pid = ::getpid();
  uint64_t tid = get_threadid();

  struct kinfo_proc *kp = nullptr, *nkp;
  size_t len = 0;
  int error;
  int ctl[4] = {CTL_KERN, KERN_PROC, KERN_PROC_PID | KERN_PROC_INC_THREAD,
                (int)pid};

  while (1) {
    error = sysctl(ctl, 4, kp, &len, nullptr, 0);
    if (kp == nullptr || (error != 0 && errno == ENOMEM)) {
      // Add extra space in case threads are added before next call.
      len += sizeof(*kp) + len / 10;
      nkp = (struct kinfo_proc *)::realloc(kp, len);
      if (nkp == nullptr) {
        free(kp);
        return;
      }
      kp = nkp;
      continue;
    }
    if (error != 0)
      len = 0;
    break;
  }

  for (size_t i = 0; i < len / sizeof(*kp); i++) {
    if (kp[i].ki_tid == (lwpid_t)tid) {
      Name.append(kp[i].ki_tdname, kp[i].ki_tdname + strlen(kp[i].ki_tdname));
      break;
    }
  }
  free(kp);
  return;
#elif (defined(__linux__) || defined(__CYGWIN__)) && HAVE_PTHREAD_GETNAME_NP
  constexpr uint32_t len = get_max_thread_name_length_impl();
  char Buffer[len] = {'\0'}; // FIXME: working around MSan false positive.
  if (0 == ::pthread_getname_np(::pthread_self(), Buffer, len))
    Name.append(Buffer, Buffer + strlen(Buffer));
#elif defined(HAVE_PTHREAD_GET_NAME_NP) && HAVE_PTHREAD_GET_NAME_NP
  constexpr uint32_t len = get_max_thread_name_length_impl();
  char buf[len];
  ::pthread_get_name_np(::pthread_self(), buf, len);

  Name.append(buf, buf + strlen(buf));

#elif defined(HAVE_PTHREAD_GETNAME_NP) && HAVE_PTHREAD_GETNAME_NP
  constexpr uint32_t len = get_max_thread_name_length_impl();
  char buf[len];
  ::pthread_getname_np(::pthread_self(), buf, len);

  Name.append(buf, buf + strlen(buf));
#endif
}

SetThreadPriorityResult llvm::set_thread_priority(ThreadPriority Priority) {
#if (defined(__linux__) || defined(__CYGWIN__)) && defined(SCHED_IDLE)
  // Some very old glibcs may be missing SCHED_IDLE.
  // https://man7.org/linux/man-pages/man3/pthread_setschedparam.3.html
  // https://man7.org/linux/man-pages/man2/sched_setscheduler.2.html
  sched_param Param;
  Param.sched_priority = 0;

  int Policy = SCHED_OTHER;
  switch (Priority) {
  case ThreadPriority::Background:
    // True background work. Lowest CPU pressure; won't interfere with foreground.
    Policy = SCHED_IDLE;
    break;
  case ThreadPriority::Low:
#if defined(SCHED_BATCH)
    // Deprioritized but does not starve; ideal for auxiliary work.
    Policy = SCHED_BATCH;
#else
    Policy = SCHED_OTHER;
#endif
    break;
  case ThreadPriority::Default:
    Policy = SCHED_OTHER;
    break;
  }

  return !pthread_setschedparam(pthread_self(), Policy, &Param)
             ? SetThreadPriorityResult::SUCCESS
             : SetThreadPriorityResult::FAILURE;
#elif defined(__APPLE__)
  // https://developer.apple.com/documentation/apple-silicon/tuning-your-code-s-performance-for-apple-silicon
  const auto qosClass = [&]() {
    switch (Priority) {
    case ThreadPriority::Background:
      return QOS_CLASS_BACKGROUND;
    case ThreadPriority::Low:
      return QOS_CLASS_UTILITY;
    case ThreadPriority::Default:
      return QOS_CLASS_DEFAULT;
    }
  }();
  return !pthread_set_qos_class_self_np(qosClass, 0)
             ? SetThreadPriorityResult::SUCCESS
             : SetThreadPriorityResult::FAILURE;
#endif
  return SetThreadPriorityResult::FAILURE;
}

#include <thread>

static int computeHostNumHardwareThreads() {
#if defined(__FreeBSD__)
  cpuset_t mask;
  CPU_ZERO(&mask);
  if (cpuset_getaffinity(CPU_LEVEL_WHICH, CPU_WHICH_TID, -1, sizeof(mask),
                         &mask) == 0)
    return CPU_COUNT(&mask);
#elif (defined(__linux__) || defined(__CYGWIN__))
  // Try fixed-size affinity mask first.
  cpu_set_t Set;
  CPU_ZERO(&Set);
  if (sched_getaffinity(0, sizeof(Set), &Set) == 0)
    return CPU_COUNT(&Set);

  // Fallback: dynamic mask for systems with many CPUs or where fixed-size fails.
  long N = sysconf(_SC_NPROCESSORS_CONF);
  if (N > 0) {
    size_t Count = static_cast<size_t>(N);
    size_t Bytes = CPU_ALLOC_SIZE(Count);
    cpu_set_t *DynSet = CPU_ALLOC(Count);
    if (DynSet) {
      CPU_ZERO_S(Bytes, DynSet);
      if (sched_getaffinity(0, Bytes, DynSet) == 0) {
        int Num = CPU_COUNT_S(Bytes, DynSet);
        CPU_FREE(DynSet);
        return Num;
      }
      CPU_FREE(DynSet);
    }
  }
#endif
  // Guard against std::thread::hardware_concurrency() returning 0.
  if (unsigned Val = std::thread::hardware_concurrency())
    return static_cast<int>(Val);
  return 1;
}

void llvm::ThreadPoolStrategy::apply_thread_strategy(
    unsigned ThreadPoolNum) const {
  (void)ThreadPoolNum;
}

llvm::BitVector llvm::get_thread_affinity_mask() {
#if defined(__linux__) || defined(__CYGWIN__)
  // Try fixed-size affinity mask first.
  cpu_set_t Set;
  CPU_ZERO(&Set);
  if (sched_getaffinity(0, sizeof(Set), &Set) == 0) {
    llvm::BitVector BV(CPU_SETSIZE, false);
    for (int i = 0; i < CPU_SETSIZE; ++i) {
      if (CPU_ISSET(i, &Set))
        BV.set(static_cast<unsigned>(i));
    }
    int Last = BV.find_last();
    if (Last >= 0)
      BV.resize(static_cast<unsigned>(Last + 1));
    else
      BV.clear();
    return BV;
  }

  // Fallback: dynamic mask sized to configured processors.
  long N = sysconf(_SC_NPROCESSORS_CONF);
  if (N > 0) {
    size_t Count = static_cast<size_t>(N);
    size_t Bytes = CPU_ALLOC_SIZE(Count);
    cpu_set_t *DynSet = CPU_ALLOC(Count);
    if (DynSet) {
      CPU_ZERO_S(Bytes, DynSet);
      if (sched_getaffinity(0, Bytes, DynSet) == 0) {
        llvm::BitVector BV(static_cast<unsigned>(Count), false);
        for (size_t i = 0; i < Count; ++i) {
          if (CPU_ISSET_S(i, Bytes, DynSet))
            BV.set(static_cast<unsigned>(i));
        }
        CPU_FREE(DynSet);
        int Last = BV.find_last();
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
  // Unknown/unsupported: return an empty mask.
  return llvm::BitVector();
}

unsigned llvm::get_cpus() {
  // Prefer physical cores: better throughput/cache locality on Raptor Lake.
  int Physical = llvm::get_physical_cores();
  if (Physical > 0)
    return static_cast<unsigned>(Physical);

  // Fall back to available hardware threads (respecting affinity/cgroups).
  int HW = computeHostNumHardwareThreads();
  if (HW > 0)
    return static_cast<unsigned>(HW);

  return 1;
}

#if (defined(__linux__) || defined(__CYGWIN__)) &&                             \
    (defined(__i386__) || defined(__x86_64__))
// On Linux, the number of physical cores can be computed from /proc/cpuinfo,
// using the number of unique (physical id, core id) pairs among the CPUs
// enabled by the process's affinity mask.
static int computeHostNumPhysicalCores() {
  // Query current affinity (fixed-size).
  cpu_set_t Affinity;
  CPU_ZERO(&Affinity);
  if (sched_getaffinity(0, sizeof(Affinity), &Affinity) != 0)
    return -1;

  // Read /proc/cpuinfo as a stream (size appears as 0; mmapping is not appropriate).
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> Text =
      llvm::MemoryBuffer::getFileAsStream("/proc/cpuinfo");
  if (std::error_code EC = Text.getError()) {
    // Fallback: available hardware threads (respecting affinity).
    cpu_set_t Set;
    CPU_ZERO(&Set);
    if (sched_getaffinity(0, sizeof(Set), &Set) == 0)
      return CPU_COUNT(&Set);
    return 1; // Last-resort default.
  }

  SmallVector<StringRef, 8> Lines;
  (*Text)->getBuffer().split(Lines, "\n", /*MaxSplit=*/-1, /*KeepEmpty=*/false);

  // Track current logical CPU record state while parsing.
  int CurProcessor = -1;
  int CurPhysicalId = -1;
  int CurSiblings = -1;
  int CurCoreId = -1;

  // Enabled bitset for unique (package, core) indices; maps (physical_id, core_id)
  // to an integer index computed as (physical_id * siblings + core_id).
  cpu_set_t Enabled;
  CPU_ZERO(&Enabled);

  for (StringRef Line : Lines) {
    auto KV = Line.split(':');
    StringRef Name = KV.first.trim();
    StringRef Val = KV.second.trim();

    if (Name == "processor") {
      Val.getAsInteger(10, CurProcessor);
      // Reset per-CPU fields for the new logical CPU.
      CurPhysicalId = -1;
      CurSiblings = -1;
      CurCoreId = -1;
      continue;
    }

    if (Name == "physical id") {
      Val.getAsInteger(10, CurPhysicalId);
      continue;
    }

    if (Name == "siblings") {
      Val.getAsInteger(10, CurSiblings);
      continue;
    }

    if (Name == "core id") {
      Val.getAsInteger(10, CurCoreId);

      // Only record when all fields are valid and this logical CPU is enabled
      // by the current process affinity mask.
      if (CurProcessor >= 0 && CurPhysicalId >= 0 && CurCoreId >= 0 &&
          CurSiblings > 0 && CPU_ISSET(CurProcessor, &Affinity)) {
        // Build a unique index per (physical_id, core_id) within the package.
        // Using 'siblings' as the package stride is robust across typical x86 cpuinfo layouts.
        int Index = CurPhysicalId * CurSiblings + CurCoreId;
        if (Index >= 0 && Index < CPU_SETSIZE)
          CPU_SET(Index, &Enabled);
      }
      continue;
    }
  }

  int Count = CPU_COUNT(&Enabled);
  if (Count > 0)
    return Count;

  // Fallback if parsing produced no result: return available hardware threads.
  cpu_set_t Set;
  CPU_ZERO(&Set);
  if (sched_getaffinity(0, sizeof(Set), &Set) == 0)
    return CPU_COUNT(&Set);

  return 1;
}
#elif (defined(__linux__) && defined(__s390x__)) || defined(_AIX)
static int computeHostNumPhysicalCores() {
  return sysconf(_SC_NPROCESSORS_ONLN);
}
#elif defined(__linux__)
static int computeHostNumPhysicalCores() {
  cpu_set_t Affinity;
  if (sched_getaffinity(0, sizeof(Affinity), &Affinity) == 0)
    return CPU_COUNT(&Affinity);

  // The call to sched_getaffinity() may have failed because the Affinity
  // mask is too small for the number of CPU's on the system (i.e. the
  // system has more than 1024 CPUs). Allocate a mask large enough for
  // twice as many CPUs.
  cpu_set_t *DynAffinity;
  DynAffinity = CPU_ALLOC(2048);
  if (sched_getaffinity(0, CPU_ALLOC_SIZE(2048), DynAffinity) == 0) {
    int NumCPUs = CPU_COUNT(DynAffinity);
    CPU_FREE(DynAffinity);
    return NumCPUs;
  }
  return -1;
}
#elif defined(__APPLE__)
// Gets the number of *physical cores* on the machine.
static int computeHostNumPhysicalCores() {
  uint32_t count;
  size_t len = sizeof(count);
  sysctlbyname("hw.physicalcpu", &count, &len, NULL, 0);
  if (count < 1) {
    int nm[2];
    nm[0] = CTL_HW;
    nm[1] = HW_AVAILCPU;
    sysctl(nm, 2, &count, &len, NULL, 0);
    if (count < 1)
      return -1;
  }
  return count;
}
#elif defined(__MVS__)
static int computeHostNumPhysicalCores() {
  enum {
    // Byte offset of the pointer to the Communications Vector Table (CVT) in
    // the Prefixed Save Area (PSA). The table entry is a 31-bit pointer and
    // will be zero-extended to uintptr_t.
    FLCCVT = 16,
    // Byte offset of the pointer to the Common System Data Area (CSD) in the
    // CVT. The table entry is a 31-bit pointer and will be zero-extended to
    // uintptr_t.
    CVTCSD = 660,
    // Byte offset to the number of live CPs in the LPAR, stored as a signed
    // 32-bit value in the table.
    CSD_NUMBER_ONLINE_STANDARD_CPS = 264,
  };
  char *PSA = 0;
  char *CVT = reinterpret_cast<char *>(
      static_cast<uintptr_t>(reinterpret_cast<unsigned int &>(PSA[FLCCVT])));
  char *CSD = reinterpret_cast<char *>(
      static_cast<uintptr_t>(reinterpret_cast<unsigned int &>(CVT[CVTCSD])));
  return reinterpret_cast<int &>(CSD[CSD_NUMBER_ONLINE_STANDARD_CPS]);
}
#else
// On other systems, return -1 to indicate unknown.
static int computeHostNumPhysicalCores() { return -1; }
#endif

int llvm::get_physical_cores() {
  static int NumCores = computeHostNumPhysicalCores();
  return NumCores;
}
