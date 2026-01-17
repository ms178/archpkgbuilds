//===- Unix/Threading.inc - Unix Threading Implementation ----- -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
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
#include <cstring>

#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__DragonFly__)
#include <pthread_np.h>
#endif

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
#include <lwp.h>
#endif

#if defined(__OpenBSD__)
#include <unistd.h>
#endif

#if defined(__linux__)
#include <sched.h>
#include <sys/syscall.h>
#include <unistd.h>
#endif

#if defined(__CYGWIN__)
#include <sys/cpuset.h>
#endif

#if defined(__HAIKU__)
#include <OS.h>
#endif

#include <thread>

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
  int errnum = ::pthread_detach(Thread);
  if (errnum != 0)
    ReportErrnumFatal("pthread_detach failed", errnum);
}

void llvm_thread_join_impl(pthread_t Thread) {
  int errnum = ::pthread_join(Thread, nullptr);
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
  static thread_local const thread_port_t Self = mach_thread_self();
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
  return static_cast<uint64_t>(syscall(__NR_gettid));
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
  StringRef NameStr = Name.toNullTerminatedStringRef(Storage);

  constexpr uint32_t MaxLen = get_max_thread_name_length_impl();
  if (MaxLen > 0)
    NameStr = NameStr.take_back(MaxLen - 1);

  (void)NameStr;
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

SetThreadPriorityResult llvm::set_thread_priority(ThreadPriority Priority) {
#if (defined(__linux__) || defined(__CYGWIN__)) && defined(SCHED_IDLE)
  sched_param Param = {};

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

  return pthread_setschedparam(pthread_self(), Policy, &Param) == 0
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
  return pthread_set_qos_class_self_np(QosClass, 0) == 0
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
    const auto Count = static_cast<size_t>(NumConf);
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

void llvm::ThreadPoolStrategy::apply_thread_strategy(
    unsigned ThreadPoolNum) const {
  (void)ThreadPoolNum;
}

llvm::BitVector llvm::get_thread_affinity_mask() {
#if defined(__linux__) || defined(__CYGWIN__)
  cpu_set_t Set;
  CPU_ZERO(&Set);
  if (sched_getaffinity(0, sizeof(Set), &Set) == 0) {
    llvm::BitVector BV(CPU_SETSIZE, false);
    for (int i = 0; i < CPU_SETSIZE; ++i) {
      if (CPU_ISSET(i, &Set))
        BV.set(static_cast<unsigned>(i));
    }
    const int Last = BV.find_last();
    if (Last >= 0)
      BV.resize(static_cast<unsigned>(Last + 1));
    else
      BV.clear();
    return BV;
  }

  const long NumConf = sysconf(_SC_NPROCESSORS_CONF);
  if (NumConf > 0) {
    const auto Count = static_cast<size_t>(NumConf);
    const size_t Bytes = CPU_ALLOC_SIZE(Count);
    cpu_set_t *DynSet = CPU_ALLOC(Count);
    if (DynSet != nullptr) {
      CPU_ZERO_S(Bytes, DynSet);
      if (sched_getaffinity(0, Bytes, DynSet) == 0) {
        llvm::BitVector BV(static_cast<unsigned>(Count), false);
        for (size_t i = 0; i < Count; ++i) {
          if (CPU_ISSET_S(i, Bytes, DynSet))
            BV.set(static_cast<unsigned>(i));
        }
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

#if (defined(__linux__) || defined(__CYGWIN__)) && \
    (defined(__i386__) || defined(__x86_64__))

static int computeHostNumPhysicalCores() {
  cpu_set_t Affinity;
  CPU_ZERO(&Affinity);
  if (sched_getaffinity(0, sizeof(Affinity), &Affinity) != 0)
    return -1;

  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> Text =
      llvm::MemoryBuffer::getFileAsStream("/proc/cpuinfo");
  if (!Text) {
    cpu_set_t Set;
    CPU_ZERO(&Set);
    if (sched_getaffinity(0, sizeof(Set), &Set) == 0)
      return CPU_COUNT(&Set);
    return -1;
  }

  SmallVector<StringRef, 8> Lines;
  (*Text)->getBuffer().split(Lines, "\n", -1, false);

  int CurProcessor = -1;
  int CurPhysicalId = -1;
  int CurSiblings = -1;
  int CurCoreId = -1;

  cpu_set_t Enabled;
  CPU_ZERO(&Enabled);

  for (const StringRef &Line : Lines) {
    const std::pair<StringRef, StringRef> KV = Line.split(':');
    const StringRef FieldName = KV.first.trim();
    const StringRef FieldVal = KV.second.trim();

    if (FieldName.empty())
      continue;

    const char FirstChar = FieldName.front();

    if (FirstChar == 'p') {
      if (FieldName == "processor") {
        FieldVal.getAsInteger(10, CurProcessor);
        CurPhysicalId = -1;
        CurSiblings = -1;
        CurCoreId = -1;
      } else if (FieldName == "physical id") {
        FieldVal.getAsInteger(10, CurPhysicalId);
      }
    } else if (FirstChar == 's') {
      if (FieldName == "siblings") {
        FieldVal.getAsInteger(10, CurSiblings);
      }
    } else if (FirstChar == 'c') {
      if (FieldName == "core id") {
        FieldVal.getAsInteger(10, CurCoreId);

        if (CurProcessor >= 0 && CurProcessor < CPU_SETSIZE &&
            CurPhysicalId >= 0 && CurCoreId >= 0 && CurSiblings > 0 &&
            CPU_ISSET(CurProcessor, &Affinity)) {
          const int Index = CurPhysicalId * CurSiblings + CurCoreId;
          if (Index >= 0 && Index < CPU_SETSIZE)
            CPU_SET(Index, &Enabled);
        }
      }
    }
  }

  const int Count = CPU_COUNT(&Enabled);
  if (Count > 0)
    return Count;

  return CPU_COUNT(&Affinity);
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
