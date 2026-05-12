/* SPDX-License-Identifier: GPL-2.0 */
/*
 * scx_lavd: System Introspection Subsystem
 */

#include <scx/common.bpf.h>
#include "intf.h"
#include "lavd.bpf.h"
#include "power.bpf.h"
#include <errno.h>
#include <stdbool.h>
#include <bpf/bpf_core_read.h>
#include <bpf/bpf_helpers.h>
#include <bpf/bpf_tracing.h>

/*
 * Global Monitoring State
 * Aligned to 64 bytes to prevent false sharing
 */
volatile bool is_monitored __attribute__((aligned(64)));
struct introspec intrspc __attribute__((aligned(64)));

/* Ring Buffer for User-Space Events */
struct {
	__uint(type, BPF_MAP_TYPE_RINGBUF);
	__uint(max_entries, 16 * 1024);
} introspec_msg SEC(".maps");

/*
 * Local Helpers
 * Defined locally to avoid linker dependencies on static functions in other files.
 */
static __always_inline u16 intro_get_nice_prio(const struct task_struct *p) {
	/* MAX_RT_PRIO is 100 */
	return p->static_prio - 100;
}

static __always_inline u64 intro_time_delta(u64 after, u64 before) {
	return (after >= before) ? (after - before) : 0;
}

static __always_inline int submit_task_ctx(struct task_struct *p,
					   task_ctx __arg_arena *taskc,
					   u32 cpu_id)
{
	struct cpu_ctx *cpuc;
	struct cpdom_ctx *cpdomc;
	struct msg_task_ctx *m;
	int i;

	if (unlikely(!p || !taskc || cpu_id >= LAVD_CPU_ID_MAX))
		return -EINVAL;

	cpuc = get_cpu_ctx_id(cpu_id);
	if (unlikely(!cpuc || cpuc->cpdom_id >= LAVD_CPDOM_MAX_NR))
		return -EINVAL;

	cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpuc->cpdom_id]);
	if (unlikely(!cpdomc))
		return -EINVAL;

	m = bpf_ringbuf_reserve(&introspec_msg, sizeof(*m), 0);
	if (unlikely(!m))
		return -ENOMEM;

	m->hdr.kind = LAVD_MSG_TASKC;
	m->taskc_x.pid = taskc->pid;
	__builtin_memcpy(m->taskc_x.comm, p->comm, TASK_COMM_LEN);
	m->taskc_x.comm[TASK_COMM_LEN - 1] = '\0';

	#pragma unroll
	for (i = 0; i < TASK_COMM_LEN; i++) {
		char c = ((char __arena *)taskc->waker_comm)[i];
		m->taskc_x.waker_comm[i] = c;
		if (c == '\0')
			break;
	}
	m->taskc_x.waker_comm[TASK_COMM_LEN] = '\0';

	m->taskc_x.stat[0] = (taskc->lat_cri >= sys_stat.avg_lat_cri) ? 'L' : 'R';
	m->taskc_x.stat[1] = (taskc->perf_cri >= sys_stat.avg_perf_cri) ? 'H' : 'I';
	m->taskc_x.stat[2] = cpuc->big_core ? 'B' : 'T';
	m->taskc_x.stat[3] = test_task_flag(taskc, LAVD_FLAG_IS_GREEDY) ? 'G' : 'E';
	m->taskc_x.stat[4] = '\0';

	m->taskc_x.cpu_id = taskc->cpu_id;
	m->taskc_x.prev_cpu_id = taskc->prev_cpu_id;
	m->taskc_x.suggested_cpu_id = taskc->suggested_cpu_id;
	m->taskc_x.waker_pid = taskc->waker_pid;

	m->taskc_x.slice = taskc->slice;
	m->taskc_x.lat_cri = taskc->lat_cri;
	m->taskc_x.avg_lat_cri = sys_stat.avg_lat_cri;
	m->taskc_x.static_prio = intro_get_nice_prio(p);

	m->taskc_x.rerunnable_interval =
		intro_time_delta(taskc->last_runnable_clk, taskc->last_quiescent_clk);
	m->taskc_x.resched_interval = taskc->resched_interval;
	m->taskc_x.last_slice_used = taskc->last_slice_used;

	m->taskc_x.run_freq = taskc->run_freq;
	m->taskc_x.avg_runtime = taskc->avg_runtime;
	m->taskc_x.wait_freq = taskc->wait_freq;
	m->taskc_x.wake_freq = taskc->wake_freq;

	m->taskc_x.perf_cri = taskc->perf_cri;
	m->taskc_x.thr_perf_cri = sys_stat.avg_perf_cri;

	m->taskc_x.cpuperf_cur = cpuc->cpuperf_cur;
	m->taskc_x.cpu_util = s2p(cpuc->avg_util);
	m->taskc_x.cpu_sutil = s2p(cpuc->avg_sc_util);

	m->taskc_x.nr_active = sys_stat.nr_active;
	m->taskc_x.dsq_id = cpdomc->id;
	m->taskc_x.dsq_consume_lat = cpdomc->dsq_consume_lat;

	bpf_ringbuf_submit(m, 0);
	return 0;
}

static void proc_introspec_sched_n(struct task_struct *p,
				   task_ctx __arg_arena *taskc)
{
	u64 cur_nr, prev_nr;
	u32 cpu_id;

	if (bpf_strncmp(p->comm, 8, "scx_lavd") == 0)
		return;

	cpu_id = bpf_get_smp_processor_id();
	cur_nr = READ_ONCE(intrspc.arg);

	#pragma unroll
	for (int i = 0; i < LAVD_MAX_RETRY; i++) {
		if (!cur_nr)
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
	if (!READ_ONCE(is_monitored) || unlikely(!p || !taskc))
		return;

	switch (READ_ONCE(intrspc.cmd)) {
	case LAVD_CMD_SCHED_N:
		proc_introspec_sched_n(p, taskc);
		break;
	default:
		break;
	}
}
