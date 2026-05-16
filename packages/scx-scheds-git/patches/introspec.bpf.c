/* SPDX-License-Identifier: GPL-2.0 */
/*
 * Copyright (c) 2023, 2024 Valve Corporation.
 * Author: Changwoo Min <changwoo@igalia.com>
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
 * Flag to represent whether the scheduler is being monitored or not.
 */
volatile bool is_monitored;

/*
 * Introspection commands
 */
struct introspec intrspc;

struct {
	__uint(type, BPF_MAP_TYPE_RINGBUF);
	__uint(max_entries, 16 * 1024 /* 16 KB */);
} introspec_msg SEC(".maps");

static __always_inline
int submit_task_ctx(struct task_struct *p, task_ctx __arg_arena *taskc, u32 cpu_id)
{
	struct cpu_ctx *cpuc;
	struct cpdom_ctx *cpdomc;
	struct msg_task_ctx *m;
	u32 cpdom_id;
	int i;

	/*
	 * Bound cpu_id by the static cap before invoking the per-cpu map
	 * helper. This is an introspection (cold) path; the early-out
	 * avoids the kernel-side -EINVAL slow-path and also tightens the
	 * verifier's range tracking on the subsequent lookup. The branch
	 * is taken almost-never (cpu_id originates from
	 * bpf_get_smp_processor_id() which is bounded by nr_cpu_ids).
	 */
	if (unlikely(cpu_id >= LAVD_CPU_ID_MAX))
		return -EINVAL;

	cpuc = get_cpu_ctx_id((s32)cpu_id);
	if (unlikely(!cpuc))
		return -EINVAL;

	/*
	 * Snapshot cpdom_id and bound-check it once before MEMBER_VPTR's
	 * verifier asm sequence runs. cpdom_id is u8 (max 255) and the
	 * static cap is 128, so an OOB value here would normally produce
	 * a NULL from MEMBER_VPTR; the explicit pre-check shortens that
	 * path by ~3 BPF insns and improves predictability.
	 */
	cpdom_id = (u32)cpuc->cpdom_id;
	if (unlikely(cpdom_id >= LAVD_CPDOM_MAX_NR))
		return -EINVAL;

	cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);
	if (unlikely(!cpdomc))
		return -EINVAL;

	m = bpf_ringbuf_reserve(&introspec_msg, sizeof(*m), 0);
	if (unlikely(!m))
		return -ENOMEM;

	/*
	 * Snapshot hot fields that participate in BOTH the stat[] gating
	 * AND a later field assignment. Without this, each is_lat_cri() /
	 * is_perf_cri() / test_task_flag() call would re-load through the
	 * arena pointer (4 redundant 2-byte / 8-byte loads), and the
	 * subsequent m->taskc_x.lat_cri = taskc->lat_cri would re-load
	 * lat_cri yet again. Single snapshot point also guarantees the
	 * stat[] string and the reported lat_cri / perf_cri are derived
	 * from a self-consistent sample of the task state — useful when
	 * an updater is racing on a different CPU (the report is an
	 * intentional snapshot, but inconsistency within the same report
	 * is undesirable).
	 */
	u32 lat_cri_s     = (u32)taskc->lat_cri;
	u32 perf_cri_s    = (u32)taskc->perf_cri;
	u32 norm_lat_s    = (u32)taskc->normalized_lat_cri;
	u32 avg_lat_cri_s = sys_stat.avg_lat_cri;
	u32 thr_perf_s    = sys_stat.thr_perf_cri;
	u8  big_core_s    = cpuc->big_core;
	u64 flags_s       = taskc->flags;

	/* Paired-load locality for the time_delta operands. */
	u64 last_qs       = taskc->last_quiescent_clk;
	u64 last_rs       = taskc->last_runnable_clk;

	m->hdr.kind = LAVD_MSG_TASKC;

	/*
	 * Field assignments are grouped by source struct for L1d cache
	 * locality on Raptor Lake (64-byte lines, Intel Optimization
	 * Manual §2.4.5): all taskc-sourced fields first, then cpuc,
	 * then cpdomc, then sys_stat. This keeps each source's hot
	 * cachelines resident across consecutive accesses instead of
	 * thrashing four lines in interleaved order. No semantic change:
	 * each store target in m->taskc_x is an independent memory
	 * location, and the few volatile sources (cpuc->avg_util_*) are
	 * sampled in a single contiguous burst rather than scattered
	 * across the function.
	 */

	/* ----- Header / identity ----- */
	m->taskc_x.pid              = taskc->pid;
	__builtin_memcpy_inline(m->taskc_x.comm, p->comm, TASK_COMM_LEN);
	m->taskc_x.static_prio      = get_nice_prio(p);

	/* ----- stat[] string built from snapshots ----- */
	m->taskc_x.stat[0] = (lat_cri_s  >= avg_lat_cri_s) ? 'L' : 'R';
	m->taskc_x.stat[1] = (perf_cri_s >= thr_perf_s)    ? 'H' : 'I';
	m->taskc_x.stat[2] = big_core_s                    ? 'B' : 'T';
	m->taskc_x.stat[3] = ((flags_s & LAVD_FLAG_IS_GREEDY) == LAVD_FLAG_IS_GREEDY) ? 'G' : 'E';
	m->taskc_x.stat[4] = '\0';

	/* ----- taskc-sourced fields (CL-hot cluster) ----- */
	m->taskc_x.cpu_id                   = taskc->cpu_id;
	m->taskc_x.prev_cpu_id              = taskc->prev_cpu_id;
	m->taskc_x.suggested_cpu_id         = taskc->suggested_cpu_id;
	m->taskc_x.waker_pid                = taskc->waker_pid;

	/*
	 * Per-byte arena copy of waker_comm. This is the established
	 * LAVD idiom for arena→stack copies (see main.bpf.c at the
	 * waker_comm and full-taskc copies). __builtin_memcpy across an
	 * arena boundary is rejected by the BPF verifier; the explicit
	 * loop with `can_loop` proves termination. Do NOT replace with
	 * memcpy.
	 */
	for (i = 0; i < sizeof(m->taskc_x.waker_comm) && can_loop; i++)
		((char *)m->taskc_x.waker_comm)[i] = ((char __arena *)taskc->waker_comm)[i];

	m->taskc_x.slice_wall               = taskc->slice_wall;
	m->taskc_x.lat_cri                  = lat_cri_s;
	m->taskc_x.rerunnable_interval_wall = (u64)time_delta(last_qs, last_rs);
	m->taskc_x.resched_interval_wall    = taskc->resched_interval_wall;
	m->taskc_x.last_slice_used_wall     = taskc->last_slice_used_wall;
	m->taskc_x.run_freq                 = taskc->run_freq;
	m->taskc_x.avg_runtime_wall         = taskc->avg_runtime_wall;
	m->taskc_x.wait_freq                = taskc->wait_freq;
	m->taskc_x.wake_freq                = taskc->wake_freq;
	m->taskc_x.perf_cri                 = perf_cri_s;
	m->taskc_x.task_util_est            = taskc->util_est;
	m->taskc_x.norm_lat_cri             = (u16)norm_lat_s;

	/* ----- cpuc-sourced fields (CL-hot cluster) ----- */
	m->taskc_x.cpuperf_cur              = cpuc->cpuperf_cur;
	m->taskc_x.cpu_util_wall            = s2p(cpuc->avg_util_wall);
	m->taskc_x.cpu_util_invr            = s2p(cpuc->avg_util_invr);
	m->taskc_x.steal_util_wall          = s2p(cpuc->avg_steal_util_wall);
	m->taskc_x.steal_util_invr          = s2p(cpuc->avg_steal_util_invr);
	m->taskc_x.dom_pinned_util_wall     = s2p(cpuc->avg_dom_pinned_util_wall);
	m->taskc_x.dom_pinned_util_invr     = s2p(cpuc->avg_dom_pinned_util_invr);
	m->taskc_x.lat_headroom             = cpuc->lat_headroom;

	/* ----- cpdomc-sourced fields (CL-hot cluster) ----- */
	m->taskc_x.dsq_id                   = cpdomc->id;
	m->taskc_x.dsq_consume_lat          = cpdomc->dsq_consume_lat;
	m->taskc_x.vuln_thresh              = cpdomc->vuln_thresh;

	/* ----- sys_stat-sourced fields ----- */
	m->taskc_x.avg_lat_cri              = avg_lat_cri_s;
	m->taskc_x.thr_perf_cri             = thr_perf_s;
	m->taskc_x.nr_active                = sys_stat.nr_active;

	bpf_ringbuf_submit(m, 0);

	return 0;
}

static void proc_introspec_sched_n(struct task_struct *p,
				   task_ctx __arg_arena *taskc)
{
	u64 cur_nr, prev_nr;
	u32 cpu_id;
	int i;

	/* do not introspect itself */
	if (bpf_strncmp(p->comm, 8, "scx_lavd") == 0)
		return;

	/* introspec_arg is the number of schedules remaining */
	cpu_id = bpf_get_smp_processor_id();
	cur_nr = intrspc.arg;

	/*
	 * Note that the bounded retry (@LAVD_MAX_RETRY) does *not* guarantee
	 * to decrement introspec_arg. However, it is unlikely to happen. Even
	 * if it happens, it is nothing but a matter of delaying a message
	 * delivery. That's because other threads will try and succeed the CAS
	 * operation eventually. So this is good enough. ;-)
	 */
	for (i = 0; cur_nr > 0 && i < LAVD_MAX_RETRY; i++) {
		prev_nr = __sync_val_compare_and_swap(
				&intrspc.arg, cur_nr, cur_nr - 1);
		/* CAS success: submit a message and done */
		if (prev_nr == cur_nr) {
			submit_task_ctx(p, taskc, cpu_id);
			break;
		}
		/* CAS failure: retry */
		cur_nr = prev_nr;
	}
}

__hidden
void try_proc_introspec_cmd(struct task_struct *p, task_ctx __arg_arena *taskc)
{
	/*
	 * is_monitored is declared `volatile` so this read is already
	 * a fresh load; READ_ONCE() would be redundant. The branch is
	 * biased heavily toward "not monitored" in normal operation
	 * (only set when the user-space tool is actively introspecting),
	 * so the unlikely() hint keeps the cold path off the BPU's hot
	 * straight-line layout — important because this is called on
	 * every scheduling tick (Intel Optimization Manual §3.4.1).
	 */
	if (likely(!is_monitored))
		return;

	switch (intrspc.cmd) {
	case LAVD_CMD_SCHED_N:
		proc_introspec_sched_n(p, taskc);
		break;
	case LAVD_CMD_NOP:
		/* do nothing */
		break;
	default:
		scx_bpf_error("Unknown introspec command: %d", intrspc.cmd);
		break;
	}
}
