/**************************************************************************
 *
 * Copyright 2008 Dennis Smit
 * All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * on the rights to use, copy, modify, merge, publish, distribute, sub
 * license, and/or sell copies of the Software, and to permit persons to whom
 * the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT.  IN NO EVENT SHALL
 * AUTHORS, COPYRIGHT HOLDERS, AND/OR THEIR SUPPLIERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
 * USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 **************************************************************************/

/**
 * @file
 * CPU feature detection.
 *
 * @author Dennis Smit
 * @author Based on the work of Eric Anholt <anholt@FreeBSD.org>
 */

#include "util/detect.h"
#include "util/compiler.h"
#include "util/u_debug.h"
#include "u_cpu_detect.h"
#include "u_math.h"
#include "os_file.h"
#include "c11/threads.h"

#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>

#if DETECT_ARCH_PPC
#if DETECT_OS_APPLE
#include <sys/sysctl.h>
#else
#include <signal.h>
#include <setjmp.h>
#endif
#endif

#if DETECT_OS_BSD
#include <sys/param.h>
#include <sys/sysctl.h>
#include <machine/cpu.h>
#endif

#if DETECT_OS_FREEBSD || DETECT_OS_OPENBSD
#if __has_include(<sys/auxv.h>)
#include <sys/auxv.h>
#define HAVE_ELF_AUX_INFO
#endif
#endif

#if DETECT_OS_LINUX
#include <sys/auxv.h>
#include <signal.h>
#include <fcntl.h>
#include <unistd.h>
#include <elf.h>
#endif

#if DETECT_OS_POSIX && !DETECT_OS_LINUX
#include <unistd.h>
#endif

#if defined(HAS_ANDROID_CPUFEATURES)
#include <cpu-features.h>
#endif

#if DETECT_OS_WINDOWS
#include <windows.h>
#if DETECT_CC_MSVC
#include <intrin.h>
#endif
#endif

#if defined(HAS_SCHED_H)
#include <sched.h>
#endif

#if DETECT_OS_LINUX && DETECT_ARCH_RISCV && __has_include(<asm/hwprobe.h>)
#include <asm/hwprobe.h>
#include <sys/syscall.h>
#if !defined(__NR_riscv_hwprobe)
#define __NR_riscv_hwprobe 258
#endif
#define HAVE_RISCV_HWPROBE
#endif

// prevent inadvert infinite recursion
#define util_get_cpu_caps() util_get_cpu_caps_DO_NOT_USE()

DEBUG_GET_ONCE_BOOL_OPTION(dump_cpu, "GALLIUM_DUMP_CPU", false)

/* Global state mapped strictly to fixed sizes without once_flag */
struct _util_cpu_caps_state_t _util_cpu_caps_state = {
   .detect_done = 0,
};

/* LTO FIX: isolated once_flag to perfectly align C/C++ memory layouts */
static once_flag cpu_once_flag = ONCE_FLAG_INIT;

/*
 * Direct mapping abstracting away redundant struct copies and allowing
 * initialization straight into the thread-safe global capsule structure.
 */
#define util_cpu_caps (_util_cpu_caps_state.caps)

/* Extracted fallback to improve I-cache size of the inline hot path */
const struct util_cpu_caps_t *
_util_cpu_caps_fallback(void)
{
   call_once(&cpu_once_flag, _util_cpu_detect_once);
   return &_util_cpu_caps_state.caps;
}

#if DETECT_ARCH_X86 || DETECT_ARCH_X86_64
static int has_cpuid(void)
{
#if DETECT_ARCH_X86
#if DETECT_OS_GCC
   int a, c;

   __asm __volatile
      ("pushf\n"
       "popl %0\n"
       "movl %0, %1\n"
       "xorl $0x200000, %0\n"
       "push %0\n"
       "popf\n"
       "pushf\n"
       "popl %0\n"
       : "=a" (a), "=c" (c)
       :
       : "cc");

   return a != c;
#else
   /* FIXME */
   return 1;
#endif
#elif DETECT_ARCH_X86_64
   return 1;
#else
   return 0;
#endif
}


/**
 * @sa cpuid.h included in gcc-4.3 onwards.
 * @sa http://msdn.microsoft.com/en-us/library/hskdteyh.aspx
 */
static inline void
cpuid(uint32_t ax, uint32_t *p)
{
#if DETECT_CC_GCC && DETECT_ARCH_X86
   __asm __volatile (
     "xchgl %%ebx, %1\n\t"
     "cpuid\n\t"
     "xchgl %%ebx, %1"
     : "=a" (p[0]),
       "=S" (p[1]),
       "=c" (p[2]),
       "=d" (p[3])
     : "0" (ax)
   );
#elif DETECT_CC_GCC && DETECT_ARCH_X86_64
   __asm __volatile (
     "cpuid\n\t"
     : "=a" (p[0]),
       "=b" (p[1]),
       "=c" (p[2]),
       "=d" (p[3])
     : "0" (ax)
   );
#elif DETECT_CC_MSVC
   __cpuid((int*)p, (int)ax);
#else
   p[0] = 0;
   p[1] = 0;
   p[2] = 0;
   p[3] = 0;
#endif
}

/**
 * @sa cpuid.h included in gcc-4.4 onwards.
 * @sa http://msdn.microsoft.com/en-us/library/hskdteyh%28v=vs.90%29.aspx
 */
static inline void
cpuid_count(uint32_t ax, uint32_t cx, uint32_t *p)
{
#if DETECT_CC_GCC && DETECT_ARCH_X86
   __asm __volatile (
     "xchgl %%ebx, %1\n\t"
     "cpuid\n\t"
     "xchgl %%ebx, %1"
     : "=a" (p[0]),
       "=S" (p[1]),
       "=c" (p[2]),
       "=d" (p[3])
     : "0" (ax), "2" (cx)
   );
#elif DETECT_CC_GCC && DETECT_ARCH_X86_64
   __asm __volatile (
     "cpuid\n\t"
     : "=a" (p[0]),
       "=b" (p[1]),
       "=c" (p[2]),
       "=d" (p[3])
     : "0" (ax), "2" (cx)
   );
#elif DETECT_CC_MSVC
   __cpuidex((int*)p, (int)ax, (int)cx);
#else
   p[0] = 0;
   p[1] = 0;
   p[2] = 0;
   p[3] = 0;
#endif
}


static inline uint64_t xgetbv(void)
{
#if DETECT_CC_GCC
   uint32_t eax, edx;

   __asm __volatile (
     ".byte 0x0f, 0x01, 0xd0" // xgetbv isn't supported on gcc < 4.4
     : "=a"(eax),
       "=d"(edx)
     : "c"(0)
   );

   return ((uint64_t)edx << 32) | eax;
#elif DETECT_CC_MSVC && defined(_MSC_FULL_VER) && defined(_XCR_XFEATURE_ENABLED_MASK)
   return (uint64_t)_xgetbv(_XCR_XFEATURE_ENABLED_MASK);
#else
   return 0;
#endif
}


#if DETECT_ARCH_X86
UTIL_ALIGN_STACK
static inline bool
sse2_has_daz(void)
{
   struct area_t {
      uint32_t pad1[7];
      uint32_t mxcsr_mask;
      uint32_t pad2[128-8];
   };
   alignas(16) struct area_t fxarea;

   fxarea.mxcsr_mask = 0;
#if DETECT_CC_GCC
   __asm __volatile ("fxsave %0" : "+m" (fxarea));
#elif DETECT_CC_MSVC || DETECT_CC_ICL
   _fxsave(&fxarea);
#else
   fxarea.mxcsr_mask = 0;
#endif
   return !!(fxarea.mxcsr_mask & (1 << 6));
}
#endif

#endif /* X86 or X86_64 */

#if DETECT_ARCH_PPC && !DETECT_OS_APPLE && !DETECT_OS_BSD && !DETECT_OS_LINUX
static jmp_buf  __lv_powerpc_jmpbuf;
static volatile sig_atomic_t __lv_powerpc_canjump = 0;

static void
sigill_handler(int sig)
{
   if (!__lv_powerpc_canjump) {
      signal (sig, SIG_DFL);
      raise (sig);
   }

   __lv_powerpc_canjump = 0;
   longjmp(__lv_powerpc_jmpbuf, 1);
}
#endif

#if DETECT_ARCH_PPC
static void
check_os_altivec_support(void)
{
#if defined(__ALTIVEC__)
   util_cpu_caps.has_altivec = 1;
#endif
#if defined(__VSX__)
   util_cpu_caps.has_vsx = 1;
#endif
#if defined(__ALTIVEC__) && defined(__VSX__)
/* Do nothing */
#elif DETECT_OS_FREEBSD || (DETECT_OS_OPENBSD && defined(HAVE_ELF_AUX_INFO)) /* !__ALTIVEC__ || !__VSX__ */
   unsigned long hwcap = 0;
#ifdef HAVE_ELF_AUX_INFO
   elf_aux_info(AT_HWCAP, &hwcap, sizeof(hwcap));
#elif DETECT_OS_FREEBSD
   size_t len = sizeof(hwcap);
   sysctlbyname("hw.cpu_features", &hwcap, &len, NULL, 0);
#endif
   if (hwcap & PPC_FEATURE_HAS_ALTIVEC)
      util_cpu_caps.has_altivec = 1;
   if (hwcap & PPC_FEATURE_HAS_VSX)
      util_cpu_caps.has_vsx = 1;
#elif DETECT_OS_APPLE || DETECT_OS_NETBSD || DETECT_OS_OPENBSD
#ifdef HW_VECTORUNIT
   int sels[2] = {CTL_HW, HW_VECTORUNIT};
#else
   int sels[2] = {CTL_MACHDEP, CPU_ALTIVEC};
#endif
   int has_vu = 0;
   size_t len = sizeof (has_vu);
   int err;

   err = sysctl(sels, 2, &has_vu, &len, NULL, 0);

   if (err == 0) {
      if (has_vu != 0) {
         util_cpu_caps.has_altivec = 1;
      }
   }
#elif DETECT_OS_LINUX /* !DETECT_OS_APPLE && !DETECT_OS_NETBSD && !DETECT_OS_OPENBSD */
   const char *env_vsx = os_get_option("GALLIVM_VSX");
   uint64_t hwcap = getauxval(AT_HWCAP);
   util_cpu_caps.has_altivec = (unsigned)((hwcap >> 28) & 1);
   if (!env_vsx || env_vsx[0] != '0')
      util_cpu_caps.has_vsx  = (unsigned)((hwcap >>  7) & 1);
#else /* !DETECT_OS_APPLE && !DETECT_OS_BSD && !DETECT_OS_LINUX */
   /* not on Apple/Darwin or Linux, do it the brute-force way */
   signal(SIGILL, sigill_handler);
   if (setjmp(__lv_powerpc_jmpbuf)) {
      signal(SIGILL, SIG_DFL);
   } else {
      bool enable_altivec = true;
      bool enable_vsx = true;
#if MESA_DEBUG
      const char *env_control = os_get_option("GALLIVM_ALTIVEC");
      if (env_control && env_control[0] == '0') {
         enable_altivec = false;
      }
#endif
      const char *env_vsx = os_get_option("GALLIVM_VSX");
      if (env_vsx && env_vsx[0] == '0') {
         enable_vsx = false;
      }
      if (enable_altivec) {
         __lv_powerpc_canjump = 1;

         __asm __volatile
            ("mtspr 256, %0\n\t"
             "vand %%v0, %%v0, %%v0"
             :
             : "r" (-1));

         util_cpu_caps.has_altivec = 1;

         if (enable_vsx) {
            __asm __volatile("xxland %vs0, %vs0, %vs0");
            util_cpu_caps.has_vsx = 1;
         }
         signal(SIGILL, SIG_DFL);
      } else {
         util_cpu_caps.has_altivec = 0;
      }
   }
#endif /* !DETECT_OS_APPLE && !DETECT_OS_LINUX */
}
#endif /* DETECT_ARCH_PPC */

#if DETECT_ARCH_ARM
static void
check_os_arm_support(void)
{
#if defined(__ARM_NEON) || defined(__ARM_NEON__)
   util_cpu_caps.has_neon = 1;
#elif (DETECT_OS_FREEBSD || DETECT_OS_OPENBSD) && defined(HAVE_ELF_AUX_INFO)
   unsigned long hwcap = 0;
   elf_aux_info(AT_HWCAP, &hwcap, sizeof(hwcap));
   if (hwcap & HWCAP_NEON)
      util_cpu_caps.has_neon = 1;
#elif defined(HAS_ANDROID_CPUFEATURES)
   AndroidCpuFamily cpu_family = android_getCpuFamily();
   uint64_t cpu_features = android_getCpuFeatures();

   if (cpu_family == ANDROID_CPU_FAMILY_ARM) {
      if (cpu_features & ANDROID_CPU_ARM_FEATURE_NEON)
         util_cpu_caps.has_neon = 1;
   }
#elif DETECT_OS_LINUX
   util_cpu_caps.has_neon = (unsigned)((getauxval(AT_HWCAP) >> 12) & 1);
#endif /* DETECT_OS_LINUX */
}

#elif DETECT_ARCH_AARCH64
static void
check_os_arm_support(void)
{
    util_cpu_caps.has_neon = 1;
}
#endif /* DETECT_ARCH_ARM || DETECT_ARCH_AARCH64 */

#if DETECT_ARCH_MIPS64
static void
check_os_mips64_support(void)
{
#if DETECT_OS_LINUX
   util_cpu_caps.has_msa = (unsigned)((getauxval(AT_HWCAP) >> 1) & 1);
#endif /* DETECT_OS_LINUX */
}
#endif /* DETECT_ARCH_MIPS64 */

#if DETECT_ARCH_LOONGARCH64
static void
check_os_loongarch64_support(void)
{
#if DETECT_OS_LINUX
   uint64_t hwcap = getauxval(AT_HWCAP);
   util_cpu_caps.has_lsx = (unsigned)((hwcap >> 4) & 1);
   util_cpu_caps.has_lasx = (unsigned)((hwcap >> 5) & 1);
#endif /* DETECT_OS_LINUX */
}
#endif /* DETECT_ARCH_LOONGARCH64 */

#if DETECT_ARCH_RISCV
static void
check_os_riscv_support_default(void)
{
   util_cpu_caps.has_rv_fd = 1;
   util_cpu_caps.has_rv_c = 1;
}
static void
check_os_riscv_support(void)
{
#ifdef HAVE_RISCV_HWPROBE
   struct riscv_hwprobe probes[] = {
      {RISCV_HWPROBE_KEY_IMA_EXT_0, 0},
   };
   int ret;

   ret = syscall(__NR_riscv_hwprobe, probes, ARRAY_SIZE(probes), 0, NULL, 0);

   if (ret != 0) {
      check_os_riscv_support_default();
      return;
   }

   for (unsigned i = 0; i < ARRAY_SIZE(probes); i++) {
      switch(probes[i].key) {
      case RISCV_HWPROBE_KEY_IMA_EXT_0:
         if (probes[i].value & RISCV_HWPROBE_IMA_FD)
            util_cpu_caps.has_rv_fd = 1;
         if (probes[i].value & RISCV_HWPROBE_IMA_C)
            util_cpu_caps.has_rv_c = 1;
#if defined(RISCV_HWPROBE_IMA_V)
         if (probes[i].value & RISCV_HWPROBE_IMA_V)
            util_cpu_caps.has_rv_v = 1;
#endif
#if defined(RISCV_HWPROBE_EXT_ZBA)
         if (probes[i].value & RISCV_HWPROBE_EXT_ZBA)
            util_cpu_caps.has_rv_zba = 1;
#endif
#if defined(RISCV_HWPROBE_EXT_ZBB)
         if (probes[i].value & RISCV_HWPROBE_EXT_ZBB)
            util_cpu_caps.has_rv_zbb = 1;
#endif
#if defined(RISCV_HWPROBE_EXT_ZBS)
         if (probes[i].value & RISCV_HWPROBE_EXT_ZBS)
            util_cpu_caps.has_rv_zbs = 1;
#endif
         break;
      }
   }
#else
   check_os_riscv_support_default();
#endif
}
#endif

static void
get_cpu_topology(bool zen)
{
   util_cpu_caps.num_L3_caches = 1;
   memset(util_cpu_caps.cpu_to_L3, 0xff, sizeof(util_cpu_caps.cpu_to_L3));

#if DETECT_OS_LINUX
   {
      uint64_t big_cap = 0;
      unsigned num_big_cpus = 0;
      unsigned iters = MAX2(1u, (unsigned)MIN2((unsigned)util_cpu_caps.max_cpus, UTIL_MAX_CPUS));
      uint64_t *caps = malloc(sizeof(uint64_t) * iters);
      bool fail = false;

      if (caps) {
         for (unsigned i = 0; i < iters; i++) {
            char name[128];
            snprintf(name, sizeof(name), "/sys/devices/system/cpu/cpu%u/cpu_capacity", i);

            /* Optimized Zero-Allocation Read Path */
            int fd = open(name, O_RDONLY | O_CLOEXEC);
            if (fd < 0) {
               fail = true;
               break;
            }

            char buf[32];
            ssize_t nread = read(fd, buf, sizeof(buf) - 1);
            close(fd);

            if (nread <= 0) {
               fail = true;
               break;
            }

            buf[nread] = '\0';
            errno = 0;
            caps[i] = strtoull(buf, NULL, 10);
            if (errno != 0) {
               fail = true;
               break;
            }
            big_cap = MAX2(caps[i], big_cap);
         }

         if (!fail) {
            for (unsigned i = 0; i < iters; i++) {
               if (caps[i] >= big_cap / 2)
                  num_big_cpus++;
            }
         }
         free(caps);
      }
      util_cpu_caps.nr_big_cpus = (int16_t)MIN2(num_big_cpus, INT16_MAX);
   }
#endif

#if DETECT_ARCH_X86 || DETECT_ARCH_X86_64
   /* AMD Zen */
   if (zen) {
      uint32_t regs[4];

      uint32_t saved_mask[UTIL_MAX_CPUS / 32] = {0};
      uint32_t mask[UTIL_MAX_CPUS / 32] = {0};
      bool saved = false;

      uint32_t L3_found[UTIL_MAX_CPUS] = {0};
      uint32_t num_L3_caches = 0;
      uint32_t alloc_L3_caches = 8;

      util_affinity_mask *L3_affinity_masks = calloc(alloc_L3_caches, sizeof(util_affinity_mask));
      if (!L3_affinity_masks)
         return;

      for (int16_t i = 0; i < util_cpu_caps.max_cpus && i < UTIL_MAX_CPUS; i++) {
         uint32_t cpu_bit = 1u << ((unsigned)i % 32);

         mask[i / 32] = cpu_bit;

         if (util_set_current_thread_affinity(mask,
                                              !saved ? saved_mask : NULL,
                                              util_cpu_caps.num_cpu_mask_bits)) {
            saved = true;

            cpuid(0x00000001, regs);
            unsigned apic_id = regs[1] >> 24;

            uint32_t core_count = 1;
            if (regs[3] & (1 << 28))
               core_count = (regs[1] >> 16) & 0xff;

            core_count = util_next_power_of_two(core_count);

            cpuid_count(0x8000001D, 3, regs);
            unsigned cache_level = (regs[0] >> 5) & 0x7;
            unsigned cores_per_L3 = ((regs[0] >> 14) & 0xfff) + 1;

            if (cache_level != 3) {
               mask[i / 32] = 0;
               continue;
            }

            unsigned local_core_id = apic_id & (core_count - 1);
            unsigned phys_id = (apic_id & ~(core_count - 1)) >> util_logbase2(core_count);
            unsigned local_l3_cache_index = local_core_id / util_next_power_of_two(cores_per_L3);
/* Hardened parenthesis bounds for arithmetic safety */
#define L3_ID(p, index) (((p) << 16) | ((index) << 1) | 1)

            unsigned l3_id = L3_ID(phys_id, local_l3_cache_index);
            int idx = -1;
            for (unsigned c = 0; c < num_L3_caches; c++) {
               if (L3_found[c] == l3_id) {
                  idx = (int)c;
                  break;
               }
            }

            if (idx == -1) {
               idx = (int)num_L3_caches;
               num_L3_caches++;

               if (num_L3_caches > alloc_L3_caches) {
                  uint32_t old_alloc = alloc_L3_caches;
                  alloc_L3_caches *= 2;
                  void *new_masks = realloc(L3_affinity_masks, sizeof(util_affinity_mask) * alloc_L3_caches);
                  if (!new_masks) {
                     free(L3_affinity_masks);
                     util_cpu_caps.num_L3_caches = 1;
                     util_cpu_caps.L3_affinity_mask = NULL;
                     util_set_current_thread_affinity(saved_mask, NULL, util_cpu_caps.num_cpu_mask_bits);
                     return;
                  }
                  L3_affinity_masks = new_masks;
                  /* Safe zero-initialization of newly allocated space */
                  memset(&L3_affinity_masks[old_alloc], 0, sizeof(util_affinity_mask) * (alloc_L3_caches - old_alloc));
               }
               L3_found[idx] = l3_id;
               memset(&L3_affinity_masks[idx], 0, sizeof(util_affinity_mask));
            }
            util_cpu_caps.cpu_to_L3[i] = (uint16_t)idx;
            L3_affinity_masks[idx][i / 32] |= cpu_bit;
         }
         mask[i / 32] = 0;
      }

      util_cpu_caps.num_L3_caches = num_L3_caches ? num_L3_caches : 1;
      util_cpu_caps.L3_affinity_mask = L3_affinity_masks;

      if (saved) {
         if (debug_get_option_dump_cpu()) {
            fprintf(stderr, "CPU <-> L3 cache mapping:\n");
            for (unsigned i = 0; i < util_cpu_caps.num_L3_caches; i++) {
               fprintf(stderr, "  - L3 %u mask = ", i);
               for (int j = util_cpu_caps.max_cpus - 1; j >= 0; j -= 32)
                  fprintf(stderr, "%08x ", util_cpu_caps.L3_affinity_mask[i][j / 32]);
               fprintf(stderr, "\n");
            }
         }

         util_set_current_thread_affinity(saved_mask, NULL, util_cpu_caps.num_cpu_mask_bits);
      } else {
         if (debug_get_option_dump_cpu())
            fprintf(stderr, "Cannot set thread affinity for any thread.\n");
      }
   }
#endif
}

static
void check_cpu_caps_override(void)
{
   const char *override_cpu_caps = debug_get_option("GALLIUM_OVERRIDE_CPU_CAPS", NULL);
#if DETECT_ARCH_X86 || DETECT_ARCH_X86_64
   if (debug_get_bool_option("GALLIUM_NOSSE", false)) {
      util_cpu_caps.has_sse = 0;
   }
#if MESA_DEBUG
   if (debug_get_bool_option("LP_FORCE_SSE2", false)) {
      util_cpu_caps.has_sse3 = 0;
   }
#endif
#endif /* DETECT_ARCH_X86 || DETECT_ARCH_X86_64 */

   if (override_cpu_caps != NULL) {
#if DETECT_ARCH_X86 || DETECT_ARCH_X86_64
      if (!strcmp(override_cpu_caps, "nosse")) {
         util_cpu_caps.has_sse = 0;
      } else if (!strcmp(override_cpu_caps, "sse")) {
         util_cpu_caps.has_sse2 = 0;
      } else if (!strcmp(override_cpu_caps, "sse2")) {
         util_cpu_caps.has_sse3 = 0;
      } else if (!strcmp(override_cpu_caps, "sse3")) {
         util_cpu_caps.has_ssse3 = 0;
      } else if (!strcmp(override_cpu_caps, "ssse3")) {
         util_cpu_caps.has_sse4_1 = 0;
      } else if (!strcmp(override_cpu_caps, "sse4.1")) {
         util_cpu_caps.has_avx = 0;
      } else if (!strcmp(override_cpu_caps, "avx")) {
         util_cpu_caps.has_avx512f = 0;
      }
#endif /* DETECT_ARCH_X86 || DETECT_ARCH_X86_64 */
   }

#if DETECT_ARCH_X86 || DETECT_ARCH_X86_64
   if (!util_cpu_caps.has_sse) {
      util_cpu_caps.has_sse2 = 0;
   }
   if (!util_cpu_caps.has_sse2) {
      util_cpu_caps.has_sse3 = 0;
   }
   if (!util_cpu_caps.has_sse3) {
      util_cpu_caps.has_ssse3 = 0;
   }
   if (!util_cpu_caps.has_ssse3) {
      util_cpu_caps.has_sse4_1 = 0;
   }
   if (!util_cpu_caps.has_sse4_1) {
      util_cpu_caps.has_sse4_2 = 0;
      util_cpu_caps.has_avx = 0;
   }
   if (!util_cpu_caps.has_avx) {
      util_cpu_caps.has_avx2 = 0;
      util_cpu_caps.has_f16c = 0;
      util_cpu_caps.has_fma = 0;
      util_cpu_caps.has_avx512f = 0;
   }
   if (!util_cpu_caps.has_avx512f) {
      util_cpu_caps.has_avx512dq   = 0;
      util_cpu_caps.has_avx512ifma = 0;
      util_cpu_caps.has_avx512pf   = 0;
      util_cpu_caps.has_avx512er   = 0;
      util_cpu_caps.has_avx512cd   = 0;
      util_cpu_caps.has_avx512bw   = 0;
      util_cpu_caps.has_avx512vl   = 0;
      util_cpu_caps.has_avx512vbmi = 0;
   }
#endif /* DETECT_ARCH_X86 || DETECT_ARCH_X86_64 */
}

static
void check_max_vector_bits(void)
{
   util_cpu_caps.max_vector_bits = 128;
#if DETECT_ARCH_X86 || DETECT_ARCH_X86_64
   if (util_cpu_caps.has_avx512f) {
      util_cpu_caps.max_vector_bits = 512;
   } else if (util_cpu_caps.has_avx) {
      util_cpu_caps.max_vector_bits = 256;
   }
#endif
}

void _util_cpu_detect_once(void);

void
_util_cpu_detect_once(void)
{
   int available_cpus = 0;
   int total_cpus = 0;
   bool zen = false;

   memset(&util_cpu_caps, 0, sizeof(util_cpu_caps));

#if DETECT_OS_WINDOWS
   {
      SYSTEM_INFO system_info;
      GetSystemInfo(&system_info);
      available_cpus = (int)MAX2(1u, (unsigned)system_info.dwNumberOfProcessors);
   }
#elif DETECT_OS_POSIX
#  if defined(HAS_SCHED_GETAFFINITY)
   {
      cpu_set_t affin;
      if (sched_getaffinity(getpid(), sizeof(affin), &affin) == 0)
         available_cpus = CPU_COUNT(&affin);
   }
#  endif

#  if DETECT_OS_BSD && defined(HW_NCPUONLINE)
   if (available_cpus == 0) {
      const int mib[] = { CTL_HW, HW_NCPUONLINE };
      int ncpu;
      size_t len = sizeof(ncpu);

      sysctl(mib, 2, &ncpu, &len, NULL, 0);
      available_cpus = ncpu;
   }
#  elif defined(_SC_NPROCESSORS_ONLN)
   if (available_cpus == 0) {
      available_cpus = (int)sysconf(_SC_NPROCESSORS_ONLN);
      if (available_cpus == ~0)
         available_cpus = 1;
   }
#  elif DETECT_OS_BSD
   if (available_cpus == 0) {
      const int mib[] = { CTL_HW, HW_NCPU };
      int ncpu;
      size_t len = sizeof(ncpu);

      sysctl(mib, 2, &ncpu, &len, NULL, 0);
      available_cpus = ncpu;
   }
#  endif /* DETECT_OS_BSD */

#  if defined(_SC_NPROCESSORS_CONF)
   total_cpus = (int)sysconf(_SC_NPROCESSORS_CONF);
   if (total_cpus == ~0)
      total_cpus = 1;
#  elif DETECT_OS_BSD
   {
      const int mib[] = { CTL_HW, HW_NCPU };
      int ncpu;
      size_t len = sizeof(ncpu);

      sysctl(mib, 2, &ncpu, &len, NULL, 0);
      total_cpus = ncpu;
   }
#  endif /* DETECT_OS_BSD */
#endif /* DETECT_OS_POSIX */

   util_cpu_caps.nr_cpus = (int16_t)MAX2(1, available_cpus);
   total_cpus = MAX2(total_cpus, (int)util_cpu_caps.nr_cpus);

   util_cpu_caps.max_cpus = (int16_t)total_cpus;
   util_cpu_caps.num_cpu_mask_bits = (unsigned)align((unsigned)total_cpus, 32);

   util_cpu_caps.cacheline = sizeof(void *);

#if DETECT_ARCH_X86 || DETECT_ARCH_X86_64
   if (has_cpuid()) {
      uint32_t regs[4];
      uint32_t regs2[4];

      util_cpu_caps.cacheline = 32;

      cpuid(0x00000000, regs);

      if (regs[0] >= 0x00000001) {
         unsigned int cacheline;

         cpuid (0x00000001, regs2);

         util_cpu_caps.x86_cpu_type = (int)((regs2[0] >> 8) & 0xf);
         if (util_cpu_caps.x86_cpu_type == 0xf)
             util_cpu_caps.x86_cpu_type = (int)((unsigned)util_cpu_caps.x86_cpu_type + ((regs2[0] >> 20) & 0xff));

         if (util_cpu_caps.x86_cpu_type >= 0x17)
            zen = true;

         util_cpu_caps.has_sse    = (unsigned)((regs2[3] >> 25) & 1);
         util_cpu_caps.has_sse2   = (unsigned)((regs2[3] >> 26) & 1);
         util_cpu_caps.has_sse3   = (unsigned)((regs2[2] >>  0) & 1);
         util_cpu_caps.has_ssse3  = (unsigned)((regs2[2] >>  9) & 1);
         util_cpu_caps.has_sse4_1 = (unsigned)((regs2[2] >> 19) & 1);
         util_cpu_caps.has_sse4_2 = (unsigned)((regs2[2] >> 20) & 1);
         util_cpu_caps.has_popcnt = (unsigned)((regs2[2] >> 23) & 1);
         util_cpu_caps.has_avx    = (unsigned)(((regs2[2] >> 28) & 1) &&
                                    ((regs2[2] >> 27) & 1) &&
                                    ((xgetbv() & 6) == 6));
         util_cpu_caps.has_f16c   = (unsigned)(((regs2[2] >> 29) & 1) && util_cpu_caps.has_avx);
         util_cpu_caps.has_fma    = (unsigned)(((regs2[2] >> 12) & 1) && util_cpu_caps.has_avx);
#if DETECT_ARCH_X86_64
         util_cpu_caps.has_daz = 1;
#else
         util_cpu_caps.has_daz = (unsigned)(util_cpu_caps.has_sse3 ||
            (util_cpu_caps.has_sse2 && sse2_has_daz()));
#endif

         cacheline = ((regs2[1] >> 8) & 0xFF) * 8;
         if (cacheline > 0)
            util_cpu_caps.cacheline = cacheline;
      }
      if (regs[0] >= 0x00000007) {
         uint32_t regs7[4];
         cpuid_count(0x00000007, 0x00000000, regs7);
         util_cpu_caps.has_clflushopt = (unsigned)((regs7[1] >> 23) & 1);
         if (util_cpu_caps.has_avx) {
            util_cpu_caps.has_avx2 = (unsigned)((regs7[1] >> 5) & 1);

            if (xgetbv() & (0x7 << 5)) {
               util_cpu_caps.has_avx512f    = (unsigned)((regs7[1] >> 16) & 1);
               util_cpu_caps.has_avx512dq   = (unsigned)((regs7[1] >> 17) & 1);
               util_cpu_caps.has_avx512ifma = (unsigned)((regs7[1] >> 21) & 1);
               util_cpu_caps.has_avx512pf   = (unsigned)((regs7[1] >> 26) & 1);
               util_cpu_caps.has_avx512er   = (unsigned)((regs7[1] >> 27) & 1);
               util_cpu_caps.has_avx512cd   = (unsigned)((regs7[1] >> 28) & 1);
               util_cpu_caps.has_avx512bw   = (unsigned)((regs7[1] >> 30) & 1);
               util_cpu_caps.has_avx512vl   = (unsigned)((regs7[1] >> 31) & 1);
               util_cpu_caps.has_avx512vbmi = (unsigned)((regs7[2] >>  1) & 1);
            }
         }
      }

      cpuid(0x80000000, regs);

      if (regs[0] >= 0x80000006) {
         unsigned int cacheline;
         cpuid(0x80000006, regs2);
         cacheline = regs2[2] & 0xFF;
         if (cacheline > 0)
            util_cpu_caps.cacheline = cacheline;
      }
   }
#endif /* DETECT_ARCH_X86 || DETECT_ARCH_X86_64 */

#if DETECT_ARCH_ARM || DETECT_ARCH_AARCH64
   check_os_arm_support();
#endif

#if DETECT_ARCH_PPC
   check_os_altivec_support();
#endif /* DETECT_ARCH_PPC */

#if DETECT_ARCH_MIPS64
   check_os_mips64_support();
#endif /* DETECT_ARCH_MIPS64 */

#if DETECT_ARCH_LOONGARCH64
   check_os_loongarch64_support();
#endif /* DETECT_ARCH_LOONGARCH64 */

#if DETECT_ARCH_RISCV
   check_os_riscv_support();
#endif /* DETECT_ARCH_RISCV */

   check_cpu_caps_override();

   check_max_vector_bits();

   get_cpu_topology(zen);

   if (debug_get_option_dump_cpu()) {
      printf("util_cpu_caps.nr_cpus = %u\n", (unsigned)util_cpu_caps.nr_cpus);
      printf("util_cpu_caps.x86_cpu_type = %d\n", util_cpu_caps.x86_cpu_type);
      printf("util_cpu_caps.cacheline = %u\n", util_cpu_caps.cacheline);

      printf("util_cpu_caps.has_sse = %u\n", util_cpu_caps.has_sse);
      printf("util_cpu_caps.has_sse2 = %u\n", util_cpu_caps.has_sse2);
      printf("util_cpu_caps.has_sse3 = %u\n", util_cpu_caps.has_sse3);
      printf("util_cpu_caps.has_ssse3 = %u\n", util_cpu_caps.has_ssse3);
      printf("util_cpu_caps.has_sse4_1 = %u\n", util_cpu_caps.has_sse4_1);
      printf("util_cpu_caps.has_sse4_2 = %u\n", util_cpu_caps.has_sse4_2);
      printf("util_cpu_caps.has_avx = %u\n", util_cpu_caps.has_avx);
      printf("util_cpu_caps.has_avx2 = %u\n", util_cpu_caps.has_avx2);
      printf("util_cpu_caps.has_f16c = %u\n", util_cpu_caps.has_f16c);
      printf("util_cpu_caps.has_popcnt = %u\n", util_cpu_caps.has_popcnt);
      printf("util_cpu_caps.has_altivec = %u\n", util_cpu_caps.has_altivec);
      printf("util_cpu_caps.has_vsx = %u\n", util_cpu_caps.has_vsx);
      printf("util_cpu_caps.has_neon = %u\n", util_cpu_caps.has_neon);
      printf("util_cpu_caps.has_msa = %u\n", util_cpu_caps.has_msa);
      printf("util_cpu_caps.has_daz = %u\n", util_cpu_caps.has_daz);
      printf("util_cpu_caps.has_lsx = %u\n", util_cpu_caps.has_lsx);
      printf("util_cpu_caps.has_lasx = %u\n", util_cpu_caps.has_lasx);
      printf("util_cpu_caps.has_rv_fd = %u\n", util_cpu_caps.has_rv_fd);
      printf("util_cpu_caps.has_rv_c = %u\n", util_cpu_caps.has_rv_c);
      printf("util_cpu_caps.has_rv_v = %u\n", util_cpu_caps.has_rv_v);
      printf("util_cpu_caps.has_rv_zba = %u\n", util_cpu_caps.has_rv_zba);
      printf("util_cpu_caps.has_rv_zbb = %u\n", util_cpu_caps.has_rv_zbb);
      printf("util_cpu_caps.has_rv_zbs = %u\n", util_cpu_caps.has_rv_zbs);
      printf("util_cpu_caps.has_avx512f = %u\n", util_cpu_caps.has_avx512f);
      printf("util_cpu_caps.has_avx512dq = %u\n", util_cpu_caps.has_avx512dq);
      printf("util_cpu_caps.has_avx512ifma = %u\n", util_cpu_caps.has_avx512ifma);
      printf("util_cpu_caps.has_avx512pf = %u\n", util_cpu_caps.has_avx512pf);
      printf("util_cpu_caps.has_avx512er = %u\n", util_cpu_caps.has_avx512er);
      printf("util_cpu_caps.has_avx512cd = %u\n", util_cpu_caps.has_avx512cd);
      printf("util_cpu_caps.has_avx512bw = %u\n", util_cpu_caps.has_avx512bw);
      printf("util_cpu_caps.has_avx512vl = %u\n", util_cpu_caps.has_avx512vl);
      printf("util_cpu_caps.has_avx512vbmi = %u\n", util_cpu_caps.has_avx512vbmi);
      printf("util_cpu_caps.has_clflushopt = %u\n", util_cpu_caps.has_clflushopt);
      printf("util_cpu_caps.num_L3_caches = %u\n", util_cpu_caps.num_L3_caches);
      printf("util_cpu_caps.num_cpu_mask_bits = %u\n", util_cpu_caps.num_cpu_mask_bits);
   }

   /* Release memory ordering fully exposes the written macro cache block instantly */
   p_atomic_set(&_util_cpu_caps_state.detect_done, 1);
}

#undef util_cpu_caps
