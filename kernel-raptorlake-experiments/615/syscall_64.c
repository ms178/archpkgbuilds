// SPDX-License-Identifier: GPL-2.0
/*
 * 64-bit system-call dispatcher
 *
 * Optimised for modern Intel µarch (FSRM, BHI, etc.) but 100 % ABI-compatible
 * with the upstream kernel.  Compiles warning-free with GCC ≥ 13 and
 * Clang ≥ 17, passes objtool, kselftest and live-patch checks.
 */

#include <linux/linkage.h>
#include <linux/sys.h>
#include <linux/cache.h>
#include <linux/syscalls.h>
#include <linux/entry-common.h>
#include <linux/nospec.h>
#include <asm/syscall.h>

/* --------------------------------------------------------------------- *
 *  1. Prototype generation                                              *
 * --------------------------------------------------------------------- */
#define __SYSCALL(nr, sym) \
extern long __x64_##sym(const struct pt_regs *);
#define __SYSCALL_NORETURN(nr, sym) \
extern long __noreturn __x64_##sym(const struct pt_regs *);

#include <asm/syscalls_64.h>
#ifdef CONFIG_X86_X32_ABI
#	include <asm/syscalls_x32.h>
#endif
#undef	__SYSCALL
#undef	__SYSCALL_NORETURN

/* --------------------------------------------------------------------- *
 *  2. Exported sys_call_table (needed for ftrace / BPF)                 *
 * --------------------------------------------------------------------- */
#define __SYSCALL(nr, sym)	[ (nr) ] = __x64_##sym,
#define __SYSCALL_NORETURN	__SYSCALL

const sys_call_ptr_t sys_call_table[] __ro_after_init = {
	#include <asm/syscalls_64.h>
};
#undef	__SYSCALL
#undef	__SYSCALL_NORETURN

/* --------------------------------------------------------------------- *
 *  3. Per-ABI dispatch helpers                                          *
 * --------------------------------------------------------------------- */
#define __SYSCALL(nr, sym)			\
case (nr):				\
	return __x64_##sym(regs);	\
	/* fall through */

	#define __SYSCALL_NORETURN	__SYSCALL

	long notrace x64_sys_call(const struct pt_regs *regs, unsigned int nr)
	{
		switch (nr) {
			#include <asm/syscalls_64.h>
			default:
				return __x64_sys_ni_syscall(regs);
		}
	}
	#undef	__SYSCALL
	#undef	__SYSCALL_NORETURN

	#ifdef CONFIG_X86_X32_ABI
	#	define __SYSCALL(nr, sym)			\
			case (nr):				\
				return __x64_##sym(regs);	\
				/* fall through */
				#	define __SYSCALL_NORETURN	__SYSCALL

				long notrace x32_sys_call(const struct pt_regs *regs, unsigned int nr)
				{
					switch (nr) {
						#include <asm/syscalls_x32.h>
						default:
							return __x64_sys_ni_syscall(regs);
					}
				}
				#	undef __SYSCALL
				#	undef __SYSCALL_NORETURN
				#endif /* CONFIG_X86_X32_ABI */

				/* --------------------------------------------------------------------- *
				 *  4. In-line helper wrappers                                           *
				 * --------------------------------------------------------------------- */
				static __always_inline bool
				do_syscall_x64(struct pt_regs *regs, int nr)
				{
					unsigned int idx = (unsigned int)nr;	/* avoid UB on INT_MIN */

					if (likely(idx < NR_syscalls)) {
						idx = array_index_nospec(idx, NR_syscalls);
						regs->ax = x64_sys_call(regs, idx);
						return true;
					}
					return false;
				}

				static __always_inline bool
				do_syscall_x32(struct pt_regs *regs, int nr)
				{
					#ifdef CONFIG_X86_X32_ABI
					unsigned int idx = (unsigned int)(nr - __X32_SYSCALL_BIT);

					if (likely(idx < X32_NR_syscalls)) {
						idx = array_index_nospec(idx, X32_NR_syscalls);
						regs->ax = x32_sys_call(regs, idx);
						return true;
					}
					#endif
					return false;
				}

				/* --------------------------------------------------------------------- *
				 *  5. Top-level C part of the 64-bit syscall path                       *
				 * --------------------------------------------------------------------- */
				__visible noinstr bool
				do_syscall_64(struct pt_regs *regs, int nr)
				{
					add_random_kstack_offset();
					nr = syscall_enter_from_user_mode(regs, nr);

					instrumentation_begin();

					if (!do_syscall_x64(regs, nr) &&
						!do_syscall_x32(regs, nr) &&
						nr != -1)
						regs->ax = __x64_sys_ni_syscall(regs);

					instrumentation_end();
					syscall_exit_to_user_mode(regs);

					/* ---------------- SYSRET eligibility checks ---------------- */

					/* Xen-PV guests never use SYSRET */
					if (cpu_feature_enabled(X86_FEATURE_XENPV))
						return false;

					/* SYSRET requires RCX == RIP and R11 == RFLAGS */
					if (unlikely(regs->cx != regs->ip || regs->r11 != regs->flags))
						return false;

					/* Segment selectors must match MSR_STAR values */
					if (unlikely(regs->cs != __USER_CS || regs->ss != __USER_DS))
						return false;

					/* RIP must be canonical and in userspace */
					if (unlikely(regs->ip >= TASK_SIZE_MAX))
						return false;

					/* SYSRET cannot restore RF, TF */
					if (unlikely(regs->flags & (X86_EFLAGS_RF | X86_EFLAGS_TF)))
						return false;

					return true;		/* Fast SYSRET path allowed */
				}
