// SPDX-License-Identifier: GPL-2.0-only
/*
 * Generic helpers for smp ipi calls
 *
 * (C) Jens Axboe <jens.axboe@oracle.com> 2008
 */

#define pr_fmt(fmt) KBUILD_MODNAME ": " fmt

#include <linux/irq_work.h>
#include <linux/rcupdate.h>
#include <linux/rculist.h>
#include <linux/kernel.h>
#include <linux/export.h>
#include <linux/percpu.h>
#include <linux/init.h>
#include <linux/interrupt.h>
#include <linux/gfp.h>
#include <linux/smp.h>
#include <linux/cpu.h>
#include <linux/sched.h>
#include <linux/sched/idle.h>
#include <linux/hypervisor.h>
#include <linux/sched/clock.h>
#include <linux/nmi.h>
#include <linux/sched/debug.h>
#include <linux/jump_label.h>
#include <linux/string_choices.h>
#include <linux/cache.h>

#include <trace/events/ipi.h>
#define CREATE_TRACE_POINTS
#include <trace/events/csd.h>
#undef CREATE_TRACE_POINTS

#include "smpboot.h"
#include "sched/smp.h"

#ifdef CONFIG_X86_64
#include <asm/topology.h>
#include <asm/processor.h>
#endif

#define CSD_TYPE(_csd)	((_csd)->node.u_flags & CSD_FLAG_TYPE_MASK)

struct call_function_data {
	call_single_data_t	__percpu *csd;
	cpumask_var_t		cpumask;
	cpumask_var_t		cpumask_ipi;
};

static DEFINE_PER_CPU_ALIGNED(struct call_function_data, cfd_data);

static DEFINE_PER_CPU_SHARED_ALIGNED(struct llist_head, call_single_queue);

static DEFINE_PER_CPU(atomic_t, trigger_backtrace) = ATOMIC_INIT(1);

static void __flush_smp_call_function_queue(bool warn_cpu_offline);

int smpcfd_prepare_cpu(unsigned int cpu)
{
	struct call_function_data *cfd = &per_cpu(cfd_data, cpu);
	int node = cpu_to_node(cpu);

	if (unlikely(!zalloc_cpumask_var_node(&cfd->cpumask, GFP_KERNEL, node)))
		return -ENOMEM;

	if (unlikely(!zalloc_cpumask_var_node(&cfd->cpumask_ipi, GFP_KERNEL, node))) {
		free_cpumask_var(cfd->cpumask);
		return -ENOMEM;
	}

	cfd->csd = __alloc_percpu(sizeof(call_single_data_t), L1_CACHE_BYTES);
	if (unlikely(!cfd->csd)) {
		free_cpumask_var(cfd->cpumask);
		free_cpumask_var(cfd->cpumask_ipi);
		return -ENOMEM;
	}

	return 0;
}

int smpcfd_dead_cpu(unsigned int cpu)
{
	struct call_function_data *cfd = &per_cpu(cfd_data, cpu);

	free_cpumask_var(cfd->cpumask);
	free_cpumask_var(cfd->cpumask_ipi);
	free_percpu(cfd->csd);
	return 0;
}

int smpcfd_dying_cpu(unsigned int cpu)
{
	__flush_smp_call_function_queue(false);
	irq_work_run();
	return 0;
}

void __init call_function_init(void)
{
	int i;

	for_each_possible_cpu(i)
		init_llist_head(&per_cpu(call_single_queue, i));

	smpcfd_prepare_cpu(smp_processor_id());
}

static __always_inline void
send_call_function_single_ipi(int cpu)
{
	if (call_function_single_prep_ipi(cpu)) {
		trace_ipi_send_cpu(cpu, _RET_IP_,
				   generic_smp_call_function_single_interrupt);
		arch_send_call_function_single_ipi(cpu);
	}
}

static __always_inline void
send_call_function_ipi_mask(struct cpumask *mask)
{
	trace_ipi_send_cpumask(mask, _RET_IP_,
			       generic_smp_call_function_single_interrupt);
	arch_send_call_function_ipi_mask(mask);
}

static __always_inline void
csd_do_func(smp_call_func_t func, void *info, call_single_data_t *csd)
{
	trace_csd_function_entry(func, csd);
	func(info);
	trace_csd_function_exit(func, csd);
}

#ifdef CONFIG_CSD_LOCK_WAIT_DEBUG

static DEFINE_STATIC_KEY_MAYBE(CONFIG_CSD_LOCK_WAIT_DEBUG_DEFAULT, csdlock_debug_enabled);

static int __init csdlock_debug(char *str)
{
	int ret;
	unsigned int val = 0;

	ret = get_option(&str, &val);
	if (ret) {
		if (val)
			static_branch_enable(&csdlock_debug_enabled);
		else
			static_branch_disable(&csdlock_debug_enabled);
	}

	return 1;
}
__setup("csdlock_debug=", csdlock_debug);

static DEFINE_PER_CPU(call_single_data_t *, cur_csd);
static DEFINE_PER_CPU(smp_call_func_t, cur_csd_func);
static DEFINE_PER_CPU(void *, cur_csd_info);

static ulong csd_lock_timeout = 5000;
module_param(csd_lock_timeout, ulong, 0644);
static int panic_on_ipistall;
module_param(panic_on_ipistall, int, 0644);

static atomic_t csd_bug_count = ATOMIC_INIT(0);

static void __csd_lock_record(call_single_data_t *csd)
{
	if (!csd) {
		smp_mb();
		__this_cpu_write(cur_csd, NULL);
		return;
	}
	__this_cpu_write(cur_csd_func, csd->func);
	__this_cpu_write(cur_csd_info, csd->info);
	smp_wmb();
	__this_cpu_write(cur_csd, csd);
	smp_mb();
}

static __always_inline void csd_lock_record(call_single_data_t *csd)
{
	if (static_branch_unlikely(&csdlock_debug_enabled))
		__csd_lock_record(csd);
}

static int csd_lock_wait_getcpu(call_single_data_t *csd)
{
	unsigned int csd_type;

	csd_type = CSD_TYPE(csd);
	if (csd_type == CSD_TYPE_ASYNC || csd_type == CSD_TYPE_SYNC)
		return csd->node.dst;
	return -1;
}

static atomic_t n_csd_lock_stuck;

bool csd_lock_is_stuck(void)
{
	return !!atomic_read(&n_csd_lock_stuck);
}

static bool csd_lock_wait_toolong(call_single_data_t *csd, u64 ts0, u64 *ts1,
				  int *bug_id, unsigned long *nmessages)
{
	int cpu = -1;
	int cpux;
	bool firsttime;
	u64 ts2, ts_delta;
	call_single_data_t *cpu_cur_csd;
	unsigned int flags = READ_ONCE(csd->node.u_flags);
	unsigned long long csd_lock_timeout_ns = csd_lock_timeout * NSEC_PER_MSEC;

	if (!(flags & CSD_FLAG_LOCK)) {
		if (!unlikely(*bug_id))
			return true;
		cpu = csd_lock_wait_getcpu(csd);
		pr_alert("csd: CSD lock (#%d) got unstuck on CPU#%02d, CPU#%02d released the lock.\n",
			 *bug_id, raw_smp_processor_id(), cpu);
		atomic_dec(&n_csd_lock_stuck);
		return true;
	}

	ts2 = ktime_get_mono_fast_ns();
	ts_delta = ts2 - *ts1;
	if (likely(ts_delta <= csd_lock_timeout_ns * (*nmessages + 1) *
			       (!*nmessages ? 1 : (ilog2(num_online_cpus()) / 2 + 1)) ||
		   csd_lock_timeout_ns == 0))
		return false;

	if (ts0 > ts2) {
		ts_delta = ts0 - ts2;
		pr_alert("sched_clock on CPU %d went backward by %llu ns\n",
			 raw_smp_processor_id(), ts_delta);
		*ts1 = ts2;
		return false;
	}

	firsttime = !*bug_id;
	if (firsttime)
		*bug_id = atomic_inc_return(&csd_bug_count);
	cpu = csd_lock_wait_getcpu(csd);
	if (WARN_ONCE(cpu < 0 || cpu >= nr_cpu_ids, "%s: cpu = %d\n", __func__, cpu))
		cpux = 0;
	else
		cpux = cpu;
	cpu_cur_csd = smp_load_acquire(&per_cpu(cur_csd, cpux));
	ts_delta = ts2 - ts0;
	pr_alert("csd: %s non-responsive CSD lock (#%d) on CPU#%d, waiting %lld ns for CPU#%02d %pS(%ps).\n",
		 firsttime ? "Detected" : "Continued", *bug_id, raw_smp_processor_id(),
		 (s64)ts_delta, cpu, csd->func, csd->info);
	(*nmessages)++;
	if (firsttime)
		atomic_inc(&n_csd_lock_stuck);

	BUG_ON(panic_on_ipistall > 0 &&
	       (s64)ts_delta > ((s64)panic_on_ipistall * NSEC_PER_MSEC));

	if (cpu_cur_csd && csd != cpu_cur_csd) {
		pr_alert("\tcsd: CSD lock (#%d) handling prior %pS(%ps) request.\n",
			 *bug_id, READ_ONCE(per_cpu(cur_csd_func, cpux)),
			 READ_ONCE(per_cpu(cur_csd_info, cpux)));
	} else {
		pr_alert("\tcsd: CSD lock (#%d) %s.\n",
			 *bug_id, !cpu_cur_csd ? "unresponsive" : "handling this request");
	}
	if (cpu >= 0) {
		if (atomic_cmpxchg_acquire(&per_cpu(trigger_backtrace, cpu), 1, 0))
			dump_cpu_task(cpu);
		if (!cpu_cur_csd) {
			pr_alert("csd: Re-sending CSD lock (#%d) IPI from CPU#%02d to CPU#%02d\n",
				 *bug_id, raw_smp_processor_id(), cpu);
			arch_send_call_function_single_ipi(cpu);
		}
	}
	if (firsttime)
		dump_stack();
	*ts1 = ts2;

	return false;
}

static void __csd_lock_wait(call_single_data_t *csd)
{
	unsigned long nmessages = 0;
	int bug_id = 0;
	u64 ts0, ts1;

	ts1 = ts0 = ktime_get_mono_fast_ns();
	for (;;) {
		if (csd_lock_wait_toolong(csd, ts0, &ts1, &bug_id, &nmessages))
			break;
		cpu_relax();
	}
	smp_acquire__after_ctrl_dep();
}

static __always_inline void csd_lock_wait(call_single_data_t *csd)
{
	if (static_branch_unlikely(&csdlock_debug_enabled)) {
		__csd_lock_wait(csd);
		return;
	}

	smp_cond_load_acquire(&csd->node.u_flags, !(VAL & CSD_FLAG_LOCK));
}
#else
static void csd_lock_record(call_single_data_t *csd)
{
}

static __always_inline void csd_lock_wait(call_single_data_t *csd)
{
	smp_cond_load_acquire(&csd->node.u_flags, !(VAL & CSD_FLAG_LOCK));
}
#endif

static __always_inline void csd_lock(call_single_data_t *csd)
{
	csd_lock_wait(csd);
	csd->node.u_flags |= CSD_FLAG_LOCK;

	smp_wmb();
}

static __always_inline void csd_unlock(call_single_data_t *csd)
{
	WARN_ON(!(csd->node.u_flags & CSD_FLAG_LOCK));

	smp_store_release(&csd->node.u_flags, 0);
}

static DEFINE_PER_CPU_SHARED_ALIGNED(call_single_data_t, csd_data);

void __smp_call_single_queue(int cpu, struct llist_node *node)
{
	if (unlikely(trace_csd_queue_cpu_enabled())) {
		call_single_data_t *csd;
		smp_call_func_t func;

		csd = container_of(node, call_single_data_t, node.llist);
		func = CSD_TYPE(csd) == CSD_TYPE_TTWU ?
			sched_ttwu_pending : csd->func;

		trace_csd_queue_cpu(cpu, _RET_IP_, func, csd);
	}

	if (likely(llist_add(node, &per_cpu(call_single_queue, cpu))))
		send_call_function_single_ipi(cpu);
}

static int generic_exec_single(int cpu, call_single_data_t *csd)
{
	if (cpu == smp_processor_id()) {
		smp_call_func_t func = csd->func;
		void *info = csd->info;
		unsigned long flags;

		csd_lock_record(csd);
		csd_unlock(csd);
		local_irq_save(flags);
		csd_do_func(func, info, NULL);
		csd_lock_record(NULL);
		local_irq_restore(flags);
		return 0;
	}

	if (unlikely((unsigned int)cpu >= nr_cpu_ids || !cpu_online(cpu))) {
		csd_unlock(csd);
		return -ENXIO;
	}

	__smp_call_single_queue(cpu, &csd->node.llist);

	return 0;
}

void generic_smp_call_function_single_interrupt(void)
{
	__flush_smp_call_function_queue(true);
}

static void __flush_smp_call_function_queue(bool warn_cpu_offline)
{
	call_single_data_t *csd, *csd_next;
	struct llist_node *entry, *prev;
	struct llist_head *head;
	static bool warned;
	atomic_t *tbt;

	lockdep_assert_irqs_disabled();

	tbt = this_cpu_ptr(&trigger_backtrace);
	atomic_set_release(tbt, 1);

	head = this_cpu_ptr(&call_single_queue);
	entry = llist_del_all(head);

	if (unlikely(!entry))
		return;

	entry = llist_reverse_order(entry);

	if (unlikely(warn_cpu_offline && !cpu_online(smp_processor_id()) &&
		     !warned && entry != NULL)) {
		warned = true;
		WARN(1, "IPI on offline CPU %d\n", smp_processor_id());

		llist_for_each_entry(csd, entry, node.llist) {
			switch (CSD_TYPE(csd)) {
			case CSD_TYPE_ASYNC:
			case CSD_TYPE_SYNC:
			case CSD_TYPE_IRQ_WORK:
				pr_warn("IPI callback %pS sent to offline CPU\n",
					csd->func);
				break;

			case CSD_TYPE_TTWU:
				pr_warn("IPI task-wakeup sent to offline CPU\n");
				break;

			default:
				pr_warn("IPI callback, unknown type %d, sent to offline CPU\n",
					CSD_TYPE(csd));
				break;
			}
		}
	}

	prev = NULL;
	llist_for_each_entry_safe(csd, csd_next, entry, node.llist) {
		if (CSD_TYPE(csd) == CSD_TYPE_SYNC) {
			smp_call_func_t func = csd->func;
			void *info = csd->info;

			if (prev)
				prev->next = &csd_next->node.llist;
			else
				entry = &csd_next->node.llist;

			csd_lock_record(csd);
			csd_do_func(func, info, csd);
			csd_unlock(csd);
			csd_lock_record(NULL);
		} else {
			prev = &csd->node.llist;
		}
	}

	if (!entry)
		return;

	prev = NULL;
	llist_for_each_entry_safe(csd, csd_next, entry, node.llist) {
		int type = CSD_TYPE(csd);

		if (likely(type != CSD_TYPE_TTWU)) {
			if (prev)
				prev->next = &csd_next->node.llist;
			else
				entry = &csd_next->node.llist;

			if (type == CSD_TYPE_ASYNC) {
				smp_call_func_t func = csd->func;
				void *info = csd->info;

				csd_lock_record(csd);
				csd_unlock(csd);
				csd_do_func(func, info, csd);
				csd_lock_record(NULL);
			} else if (type == CSD_TYPE_IRQ_WORK) {
				irq_work_single(csd);
			}

		} else {
			prev = &csd->node.llist;
		}
	}

	if (entry) {
		csd = llist_entry(entry, typeof(*csd), node.llist);
		csd_do_func(sched_ttwu_pending, entry, csd);
	}
}

void flush_smp_call_function_queue(void)
{
	unsigned int was_pending;
	unsigned long flags;

	if (llist_empty(this_cpu_ptr(&call_single_queue)))
		return;

	local_irq_save(flags);
	was_pending = local_softirq_pending();
	__flush_smp_call_function_queue(true);
	if (local_softirq_pending())
		do_softirq_post_smp_call_flush(was_pending);

	local_irq_restore(flags);
}

int smp_call_function_single(int cpu, smp_call_func_t func, void *info,
			     int wait)
{
	call_single_data_t *csd;
	call_single_data_t csd_stack = {
		.node = { .u_flags = CSD_FLAG_LOCK | CSD_TYPE_SYNC, },
	};
	int this_cpu;
	int err;

	this_cpu = get_cpu();

	WARN_ON_ONCE(cpu_online(this_cpu) && irqs_disabled()
		     && !oops_in_progress);

	WARN_ON_ONCE(!in_task());

	csd = &csd_stack;
	if (!wait) {
		csd = this_cpu_ptr(&csd_data);
		csd_lock(csd);
	}

	csd->func = func;
	csd->info = info;
#ifdef CONFIG_CSD_LOCK_WAIT_DEBUG
	csd->node.src = smp_processor_id();
	csd->node.dst = cpu;
#endif

	err = generic_exec_single(cpu, csd);

	if (wait)
		csd_lock_wait(csd);

	put_cpu();

	return err;
}
EXPORT_SYMBOL(smp_call_function_single);

int smp_call_function_single_async(int cpu, call_single_data_t *csd)
{
	int err = 0;

	preempt_disable();

	if (unlikely(csd->node.u_flags & CSD_FLAG_LOCK)) {
		err = -EBUSY;
		goto out;
	}

	csd->node.u_flags = CSD_FLAG_LOCK;
	smp_wmb();

	err = generic_exec_single(cpu, csd);

out:
	preempt_enable();

	return err;
}
EXPORT_SYMBOL_GPL(smp_call_function_single_async);

#if defined(CONFIG_X86_64) && defined(CONFIG_SMP)
static __always_inline bool cpu_is_performance_core(unsigned int cpu)
{
	if (unlikely(cpu >= nr_cpu_ids))
		return true;

	return get_topology_cpu_type(&cpu_data(cpu)) != TOPO_CPU_TYPE_EFFICIENCY;
}

static unsigned int find_pcore_in_mask(const struct cpumask *mask,
				       unsigned int local_cpu)
{
	unsigned int cpu;
	int local_node = cpu_to_node(local_cpu);
	unsigned int best_pcore_local = nr_cpu_ids;
	unsigned int best_pcore_remote = nr_cpu_ids;
	unsigned int best_ecore = nr_cpu_ids;

	if (cpumask_test_cpu(local_cpu, mask) && cpu_online(local_cpu)) {
		if (cpu_is_performance_core(local_cpu))
			return local_cpu;
		best_ecore = local_cpu;
	}

	for_each_cpu(cpu, mask) {
		if (!cpu_online(cpu) || cpu == local_cpu)
			continue;

		if (cpu_is_performance_core(cpu)) {
			if (cpu_to_node(cpu) == local_node)
				return cpu;
			if (best_pcore_remote == nr_cpu_ids)
				best_pcore_remote = cpu;
		} else if (best_ecore == nr_cpu_ids) {
			best_ecore = cpu;
		}
	}

	if (best_pcore_local != nr_cpu_ids)
		return best_pcore_local;
	if (best_pcore_remote != nr_cpu_ids)
		return best_pcore_remote;

	return best_ecore;
}
#else
static unsigned int find_pcore_in_mask(const struct cpumask *mask,
				       unsigned int local_cpu)
{
	if (cpumask_test_cpu(local_cpu, mask))
		return local_cpu;
	return sched_numa_find_nth_cpu(mask, 0, cpu_to_node(local_cpu));
}
#endif

int smp_call_function_any(const struct cpumask *mask,
			  smp_call_func_t func, void *info, int wait)
{
	unsigned int cpu;
	unsigned int local_cpu;
	int ret;

	local_cpu = get_cpu();

	cpu = find_pcore_in_mask(mask, local_cpu);

	if (unlikely(cpu >= nr_cpu_ids)) {
		put_cpu();
		return -EINVAL;
	}

	ret = smp_call_function_single(cpu, func, info, wait);
	put_cpu();
	return ret;
}
EXPORT_SYMBOL_GPL(smp_call_function_any);

#define SCF_WAIT	(1U << 0)
#define SCF_RUN_LOCAL	(1U << 1)

static void smp_call_function_many_cond(const struct cpumask *mask,
					smp_call_func_t func, void *info,
					unsigned int scf_flags,
					smp_cond_func_t cond_func)
{
	int cpu, last_cpu, this_cpu = smp_processor_id();
	struct call_function_data *cfd;
	bool wait = scf_flags & SCF_WAIT;
	int nr_cpus = 0;
	bool run_remote = false;

	lockdep_assert_preemption_disabled();

	if (likely(cpu_online(this_cpu)) && !oops_in_progress &&
	    !early_boot_irqs_disabled)
		lockdep_assert_irqs_enabled();

	WARN_ON_ONCE(!in_task());

	if (cpumask_any_and_but(mask, cpu_online_mask, this_cpu) < nr_cpu_ids) {
		cfd = this_cpu_ptr(&cfd_data);
		cpumask_and(cfd->cpumask, mask, cpu_online_mask);
		__cpumask_clear_cpu(this_cpu, cfd->cpumask);

		cpumask_clear(cfd->cpumask_ipi);
		for_each_cpu(cpu, cfd->cpumask) {
			call_single_data_t *csd = per_cpu_ptr(cfd->csd, cpu);

			if (cond_func && !cond_func(cpu, info)) {
				__cpumask_clear_cpu(cpu, cfd->cpumask);
				continue;
			}

			run_remote = true;

			csd_lock(csd);
			if (wait)
				csd->node.u_flags |= CSD_TYPE_SYNC;
			csd->func = func;
			csd->info = info;
#ifdef CONFIG_CSD_LOCK_WAIT_DEBUG
			csd->node.src = smp_processor_id();
			csd->node.dst = cpu;
#endif
			trace_csd_queue_cpu(cpu, _RET_IP_, func, csd);

			if (llist_add(&csd->node.llist, &per_cpu(call_single_queue, cpu))) {
				__cpumask_set_cpu(cpu, cfd->cpumask_ipi);
				nr_cpus++;
				last_cpu = cpu;
			}
		}

		if (nr_cpus == 1)
			send_call_function_single_ipi(last_cpu);
		else if (likely(nr_cpus > 1))
			send_call_function_ipi_mask(cfd->cpumask_ipi);
	}

	if ((scf_flags & SCF_RUN_LOCAL) && cpumask_test_cpu(this_cpu, mask) &&
	    (!cond_func || cond_func(this_cpu, info))) {
		unsigned long flags;

		local_irq_save(flags);
		csd_do_func(func, info, NULL);
		local_irq_restore(flags);
	}

	if (run_remote && wait) {
		for_each_cpu(cpu, cfd->cpumask) {
			call_single_data_t *csd;

			csd = per_cpu_ptr(cfd->csd, cpu);
			csd_lock_wait(csd);
		}
	}
}

void smp_call_function_many(const struct cpumask *mask,
			    smp_call_func_t func, void *info, bool wait)
{
	smp_call_function_many_cond(mask, func, info, wait * SCF_WAIT, NULL);
}
EXPORT_SYMBOL(smp_call_function_many);

void smp_call_function(smp_call_func_t func, void *info, int wait)
{
	preempt_disable();
	smp_call_function_many(cpu_online_mask, func, info, wait);
	preempt_enable();
}
EXPORT_SYMBOL(smp_call_function);

unsigned int setup_max_cpus = NR_CPUS;
EXPORT_SYMBOL(setup_max_cpus);

void __weak __init arch_disable_smp_support(void) { }

static int __init nosmp(char *str)
{
	setup_max_cpus = 0;
	arch_disable_smp_support();

	return 0;
}

early_param("nosmp", nosmp);

static int __init nrcpus(char *str)
{
	int nr_cpus;

	if (get_option(&str, &nr_cpus) && nr_cpus > 0 && nr_cpus < nr_cpu_ids)
		set_nr_cpu_ids(nr_cpus);

	return 0;
}

early_param("nr_cpus", nrcpus);

static int __init maxcpus(char *str)
{
	get_option(&str, &setup_max_cpus);
	if (setup_max_cpus == 0)
		arch_disable_smp_support();

	return 0;
}

early_param("maxcpus", maxcpus);

#if (NR_CPUS > 1) && !defined(CONFIG_FORCE_NR_CPUS)
unsigned int nr_cpu_ids __read_mostly = NR_CPUS;
EXPORT_SYMBOL(nr_cpu_ids);
#endif

void __init setup_nr_cpu_ids(void)
{
	set_nr_cpu_ids(find_last_bit(cpumask_bits(cpu_possible_mask), NR_CPUS) + 1);
}

void __init smp_init(void)
{
	int num_nodes, num_cpus;

	idle_threads_init();
	cpuhp_threads_init();

	pr_info("Bringing up secondary CPUs ...\n");

	bringup_nonboot_cpus(setup_max_cpus);

	num_nodes = num_online_nodes();
	num_cpus  = num_online_cpus();
	pr_info("Brought up %d node%s, %d CPU%s\n",
		num_nodes, str_plural(num_nodes), num_cpus, str_plural(num_cpus));

	smp_cpus_done(setup_max_cpus);
}

void on_each_cpu_cond_mask(smp_cond_func_t cond_func, smp_call_func_t func,
			   void *info, bool wait, const struct cpumask *mask)
{
	unsigned int scf_flags = SCF_RUN_LOCAL;

	if (wait)
		scf_flags |= SCF_WAIT;

	preempt_disable();
	smp_call_function_many_cond(mask, func, info, scf_flags, cond_func);
	preempt_enable();
}
EXPORT_SYMBOL(on_each_cpu_cond_mask);

static void do_nothing(void *unused)
{
}

void kick_all_cpus_sync(void)
{
	smp_mb();
	smp_call_function(do_nothing, NULL, 1);
}
EXPORT_SYMBOL_GPL(kick_all_cpus_sync);

void wake_up_all_idle_cpus(void)
{
	int cpu;

	for_each_possible_cpu(cpu) {
		preempt_disable();
		if (cpu != smp_processor_id() && cpu_online(cpu))
			wake_up_if_idle(cpu);
		preempt_enable();
	}
}
EXPORT_SYMBOL_GPL(wake_up_all_idle_cpus);

struct smp_call_on_cpu_struct {
	struct work_struct	work;
	struct completion	done;
	int			(*func)(void *);
	void			*data;
	int			ret;
	int			cpu;
};

static void smp_call_on_cpu_callback(struct work_struct *work)
{
	struct smp_call_on_cpu_struct *sscs;

	sscs = container_of(work, struct smp_call_on_cpu_struct, work);
	if (sscs->cpu >= 0)
		hypervisor_pin_vcpu(sscs->cpu);
	sscs->ret = sscs->func(sscs->data);
	if (sscs->cpu >= 0)
		hypervisor_pin_vcpu(-1);

	complete(&sscs->done);
}

int smp_call_on_cpu(unsigned int cpu, int (*func)(void *), void *par, bool phys)
{
	struct smp_call_on_cpu_struct sscs = {
		.done = COMPLETION_INITIALIZER_ONSTACK(sscs.done),
		.func = func,
		.data = par,
		.cpu  = phys ? cpu : -1,
	};

	INIT_WORK_ONSTACK(&sscs.work, smp_call_on_cpu_callback);

	if (cpu >= nr_cpu_ids || !cpu_online(cpu))
		return -ENXIO;

	queue_work_on(cpu, system_wq, &sscs.work);
	wait_for_completion(&sscs.done);
	destroy_work_on_stack(&sscs.work);

	return sscs.ret;
}
EXPORT_SYMBOL_GPL(smp_call_on_cpu);
