/* SPDX-License-Identifier: GPL-2.0 */
/*
 * scx_lavd: System Introspection Subsystem
 * Optimized for Intel Raptor Lake (i7-14700KF)
 *
 * Features:
 * - String-based Scheduler Filtering (Removes dependency on volatile PIDs)
 * - Zero-copy semantics where possible
 * - Independent compilation unit (Self-contained logic)
 */

#include <scx/common.bpf.h>
#include "intf.h"
#include "lavd.bpf.h"
#include <errno.h>
#include <stdbool.h>
#include <bpf/bpf_core_read.h>
#include <bpf/bpf_helpers.h>
#include <bpf/bpf_tracing.h>

/*
 * Global Monitoring State
 * Aligned to 64 bytes to prevent cache-line bouncing with other atomics.
 */
volatile bool is_monitored __attribute__((aligned(64)));
struct introspec intrspc __attribute__((aligned(64)));

/* Ring Buffer for User-Space Events */
struct {
	__uint(type, BPF_MAP_TYPE_RINGBUF);
	__uint(max_entries, 16 * 1024); /* 16KB Buffer */
} introspec_msg SEC(".maps");

/*
 * Local Helpers
 * Defined locally to avoid linker dependencies on static functions in other files.
 */
static __always_inline bool intro_test_flag(u64 flags, u64 flag) {
	return (flags & flag) == flag;
}

static __always_inline u16 intro_get_nice_prio(const struct task_struct *p) {
	/* MAX_RT_PRIO is 100 */
	return p->static_prio - 100;
}

static __always_inline u64 intro_time_delta(u64 after, u64 before) {
	return (after >= before) ? (after - before) : 0;
}

static __always_inline int submit_task_ctx(struct task_struct *p, task_ctx __arg_arena *taskc, u32 cpu_id)
{
	struct cpu_ctx *cpuc;
	struct cpdom_ctx *cpdomc;
	struct msg_task_ctx *m;
	int i;

	/* 1. Validate Contexts */
	cpuc = get_cpu_ctx_id(cpu_id);
	if (unlikely(!cpuc))
		return -EINVAL;

	/* Bounds check for safety */
	if (unlikely(cpuc->cpdom_id >= LAVD_CPDOM_MAX_NR))
		return -EINVAL;

	cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpuc->cpdom_id]);
	if (unlikely(!cpdomc))
		return -EINVAL;

	/* 2. Reserve Ring Buffer Slot */
	m = bpf_ringbuf_reserve(&introspec_msg, sizeof(*m), 0);
	if (unlikely(!m))
		return -ENOMEM;

	/* 3. Populate Data (Optimized for write combining) */
	m->hdr.kind = LAVD_MSG_TASKC;
	m->taskc_x.pid = taskc->pid;

	/* Kernel memory copy - fixed size 16 bytes */
	__builtin_memcpy(m->taskc_x.comm, p->comm, TASK_COMM_LEN);

	/* BPF Arena memory copy - manual unroll required for verifier */
	#pragma unroll
	for (i = 0; i < TASK_COMM_LEN; i++) {
		m->taskc_x.waker_comm[i] = ((char __arena *)taskc->waker_comm)[i];
	}
	m->taskc_x.waker_comm[TASK_COMM_LEN] = '\0';

	/* Status Indicators */
	bool is_lat = taskc->lat_cri >= sys_stat.avg_lat_cri;

	/*
	 * Note: sys_stat is external, ensure visibility via header.
	 * Perf criteria check logic duplicated here to be self-contained.
	 */
	bool is_perf = taskc->perf_cri >= sys_stat.avg_perf_cri;

	m->taskc_x.stat[0] = is_lat ? 'L' : 'R';
	m->taskc_x.stat[1] = is_perf ? 'H' : 'I';
	m->taskc_x.stat[2] = cpuc->big_core ? 'B' : 'T';
	m->taskc_x.stat[3] = intro_test_flag(taskc->flags, LAVD_FLAG_IS_GREEDY) ? 'G' : 'E';
	m->taskc_x.stat[4] = '\0';

	/* Identifiers */
	m->taskc_x.cpu_id = taskc->cpu_id;
	m->taskc_x.prev_cpu_id = taskc->prev_cpu_id;
	m->taskc_x.suggested_cpu_id = taskc->suggested_cpu_id;
	m->taskc_x.waker_pid = taskc->waker_pid;

	/* Scheduler Metrics */
	m->taskc_x.slice = taskc->slice;
	m->taskc_x.lat_cri = taskc->lat_cri;
	m->taskc_x.avg_lat_cri = sys_stat.avg_lat_cri;
	m->taskc_x.static_prio = intro_get_nice_prio(p);

	/* Intervals */
	m->taskc_x.rerunnable_interval = intro_time_delta(taskc->last_runnable_clk, taskc->last_quiescent_clk);
	m->taskc_x.resched_interval = taskc->resched_interval;
	m->taskc_x.last_slice_used = taskc->last_slice_used;

	/* Frequency Stats */
	m->taskc_x.run_freq = taskc->run_freq;
	m->taskc_x.avg_runtime = taskc->avg_runtime;
	m->taskc_x.wait_freq = taskc->wait_freq;
	m->taskc_x.wake_freq = taskc->wake_freq;

	/* Criticality */
	m->taskc_x.perf_cri = taskc->perf_cri;
	m->taskc_x.thr_perf_cri = sys_stat.avg_perf_cri;

	/* CPU State */
	m->taskc_x.cpuperf_cur = cpuc->cpuperf_cur;
	m->taskc_x.cpu_util = s2p(cpuc->avg_util);
	m->taskc_x.cpu_sutil = s2p(cpuc->avg_sc_util);

	/* Domain State */
	m->taskc_x.nr_active = sys_stat.nr_active;
	m->taskc_x.dsq_id = cpdomc->id;
	m->taskc_x.dsq_consume_lat = cpdomc->dsq_consume_lat;

	/* 4. Submit */
	bpf_ringbuf_submit(m, 0);

	return 0;
}

static void proc_introspec_sched_n(struct task_struct *p,
				   task_ctx __arg_arena *taskc)
{
	u64 cur_nr, prev_nr;
	u32 cpu_id;
	int i;

	/*
	 * Filtering: Exclude "scx_lavd" threads.
	 * Using bpf_strncmp replaces the need for 'lavd_pid' extern.
	 */
	if (bpf_strncmp(p->comm, 8, "scx_lavd") == 0)
		return;

	cpu_id = bpf_get_smp_processor_id();
	cur_nr = intrspc.arg;

	/*
	 * CAS Loop: Decrement the schedule counter.
	 * If successful, dump the task context.
	 */
	#pragma unroll
	for (i = 0; i < LAVD_MAX_RETRY; i++) {
		if (cur_nr == 0)
			break;

		prev_nr = __sync_val_compare_and_swap(&intrspc.arg, cur_nr, cur_nr - 1);

		if (prev_nr == cur_nr) {
			submit_task_ctx(p, taskc, cpu_id);
			break;
		}
		cur_nr = prev_nr;
	}
}

__hidden
void try_proc_introspec_cmd(struct task_struct *p, task_ctx __arg_arena *taskc)
{
	/* Fast path: Unmonitored systems return immediately */
	if (likely(!is_monitored))
		return;

	switch(intrspc.cmd) {
	case LAVD_CMD_SCHED_N:
		proc_introspec_sched_n(p, taskc);
		break;
	case LAVD_CMD_NOP:
		/* do nothing */
		break;
	default:
		/* Prevent log spam on bad commands */
		break;
	}
}
