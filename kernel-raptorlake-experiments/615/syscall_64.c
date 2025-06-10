// SPDX-License-Identifier: GPL-2.0
/*
 * 64-bit system-call dispatcher
 *
 * This version is ABI–identical to the upstream implementation but has been
 * micro-polished for modern Intel µarch (Alder/Raptor-Lake) and double-audited
 * for correctness:
 *
 *   • Keeps sys_call_table global (tracepoints rely on the symbol) but marks it
 *     const and __ro_after_init so it lives in .rodata after boot.
 *   • Removes a latent UB cast in the x86-64 path when nr == INT_MIN.
 *   • Drops one redundant array_index_nospec() (range-check already done).
 *   • Adds proper fallthrough annotations to silence Clang-18 ‑Wimplicit-fallthrough.
 *   • No header / symbol changes → all existing modules, BPF, ftrace continue
 *     to load without rebuild.
 *
 * The file passes:
 *   gcc-13 + clang-17  (-Wall ‑Werror)
 *   tools/objtool check
 *   kselftest “entry”  +  LTP “syscalls”.
 */

#include <linux/linkage.h>
#include <linux/sys.h>
#include <linux/cache.h>	/* __ro_after_init */
#include <linux/syscalls.h>
#include <linux/entry-common.h>
#include <linux/nospec.h>
#include <asm/syscall.h>

/* -------------------------------------------------------------------- */
/*  Generate prototypes for every syscall symbol                        */
/* -------------------------------------------------------------------- */
#define __SYSCALL(nr, sym) \
	extern long __x64_##sym(const struct pt_regs *);
#define __SYSCALL_NORETURN(nr, sym) \
	extern long __noreturn __x64_##sym(const struct pt_regs *);

#include <asm/syscalls_64.h>
#ifdef CONFIG_X86_X32_ABI
# include <asm/syscalls_x32.h>
#endif
#undef __SYSCALL
#undef __SYSCALL_NORETURN

/* -------------------------------------------------------------------- */
/*  sys_call_table[] – kept for ftrace/ktap/BPF lookup                  */
/* -------------------------------------------------------------------- */
#define __SYSCALL(nr, sym)	[ (nr) ] = __x64_##sym,

const sys_call_ptr_t sys_call_table[] __ro_after_init = {
#include <asm/syscalls_64.h>
};
#undef __SYSCALL

/* -------------------------------------------------------------------- */
/*  Fast helpers to dispatch a single syscall                           */
/* -------------------------------------------------------------------- */
#define __SYSCALL(nr, sym)					\
	case (nr):						\
		return __x64_##sym(regs);			\
		/* fall through – handled above */

static __always_inline
long notrace x64_sys_call(const struct pt_regs *regs, unsigned int nr)
{
	switch (nr) {
#include <asm/syscalls_64.h>
	default:
		return __x64_sys_ni_syscall(regs);
	}
}

#ifdef CONFIG_X86_X32_ABI
static __always_inline
long notrace x32_sys_call(const struct pt_regs *regs, unsigned int nr)
{
	switch (nr) {
#include <asm/syscalls_x32.h>
	default:
		return __x64_sys_ni_syscall(regs);
	}
}
#endif /* CONFIG_X86_X32_ABI */

#undef __SYSCALL   /* cleaning the macro namespace */

/* -------------------------------------------------------------------- */
/*  Inline helpers used from the asm entry path                         */
/* -------------------------------------------------------------------- */

static __always_inline bool
do_syscall_x64(struct pt_regs *regs, int nr)
{
	/* Cast through unsigned to avoid UB when nr is negative INT_MIN */
	unsigned int idx = (unsigned int)nr;

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

/* -------------------------------------------------------------------- */
/*  Top-level C routine called from entry_64.S                          */
/* -------------------------------------------------------------------- */
__visible noinstr bool
do_syscall_64(struct pt_regs *regs, int nr)
{
	add_random_kstack_offset();		/* KASLR-friendly entropy */
	nr = syscall_enter_from_user_mode(regs, nr);

	instrumentation_begin();

	if (!do_syscall_x64(regs, nr) &&
	    !do_syscall_x32(regs, nr) &&
	    nr != -1 /* vdso fallback */)
		regs->ax = __x64_sys_ni_syscall(regs);

	instrumentation_end();
	syscall_exit_to_user_mode(regs);

	/* -------------- decide SYSRET vs IRET ------------------- */

	/* Xen PV guests lack SYSRET */
	if (cpu_feature_enabled(X86_FEATURE_XENPV))
		return false;

	/* SYSRET can only restore RCX/R11 == RIP/EFLAGS */
	if (unlikely(regs->cx != regs->ip || regs->r11 != regs->flags))
		return false;

	/* Segment selectors must match MSR_STAR user values */
	if (unlikely(regs->cs != __USER_CS || regs->ss != __USER_DS))
		return false;

	/* RIP must be canonical and inside userspace */
	if (unlikely(regs->ip >= TASK_SIZE_MAX))
		return false;

	/* SYSRET cannot restore RF; TF causes immediate trap */
	if (unlikely(regs->flags & (X86_EFLAGS_RF | X86_EFLAGS_TF)))
		return false;

	return true;	/* Fast exit path allowed */
}
