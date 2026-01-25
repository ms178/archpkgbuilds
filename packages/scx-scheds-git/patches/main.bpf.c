/* SPDX-License-Identifier: GPL-2.0 */
/*
 * scx_lavd: Latency-criticality Aware Virtual Deadline (LAVD) scheduler
 * Copyright (c) 2023, 2024 Valve Corporation.
 * Author: Changwoo Min <changwoo@igalia.com>
 */
#include <scx/common.bpf.h>
#include <scx/bpf_arena_common.bpf.h>
#include "intf.h"
#include "lavd.bpf.h"
#include "util.bpf.h"
#include "power.bpf.h"
#include <errno.h>
#include <stdbool.h>
#include <bpf/bpf_core_read.h>
#include <bpf/bpf_helpers.h>
#include <bpf/bpf_tracing.h>
#include <lib/cgroup.h>

char _license[] SEC("license") = "GPL";

u64 cur_logical_clk = LAVD_DL_COMPETE_WINDOW;

static u64 cur_svc_time;

const volatile u64 slice_min_ns = LAVD_SLICE_MIN_NS_DFL;
const volatile u64 slice_max_ns = LAVD_SLICE_MAX_NS_DFL;
const volatile u8 mig_delta_pct = 0;
const volatile u64 pinned_slice_ns = 0;

static volatile u64 nr_cpus_big;
static pid_t lavd_pid;

#define LAVD_SMOOTH_SHIFT_UP	1U
#define LAVD_SMOOTH_SHIFT_DOWN	0U

static const u8 bore_log2_lut[256] = {
      0,   1,   3,   4,   6,   7,   9,  10,  11,  13,  14,  16,  17,  18,  20,  21,
     22,  24,  25,  26,  28,  29,  30,  32,  33,  34,  35,  37,  38,  39,  40,  42,
     43,  44,  45,  47,  48,  49,  50,  51,  53,  54,  55,  56,  57,  58,  60,  61,
     62,  63,  64,  65,  66,  68,  69,  70,  71,  72,  73,  74,  75,  76,  78,  79,
     80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
     96,  97,  98,  99, 100, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
    113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
    129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 141, 142, 143,
    144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 156, 157, 158,
    159, 160, 161, 162, 163, 164, 165, 165, 166, 167, 168, 169, 170, 171, 172, 173,
    173, 174, 175, 176, 177, 178, 179, 179, 180, 181, 182, 183, 184, 185, 185, 186,
    187, 188, 189, 190, 190, 191, 192, 193, 194, 195, 195, 196, 197, 198, 199, 199,
    200, 201, 202, 203, 203, 204, 205, 206, 207, 207, 208, 209, 210, 211, 211, 212,
    213, 214, 214, 215, 216, 217, 218, 218, 219, 220, 221, 221, 222, 223, 224, 224,
    225, 226, 227, 227, 228, 229, 230, 230, 231, 232, 233, 233, 234, 235, 236, 236,
    237, 238, 239, 239, 240, 241, 241, 242, 243, 244, 244, 245, 246, 247, 247, 248
};

#define BORE_BURST_OFFSET     20
#define BORE_BURST_SCALE      1280
#define BORE_MAX_PENALTY      100

static __always_inline u32 bore_calc_burst_penalty(u64 runtime_ns)
{
	u32 exp, frac_idx, log2_q8, delta_q8, penalty;
	u64 norm, frac;

	if (unlikely(runtime_ns == 0))
		return 0;

	exp = 63 - (u32)__builtin_clzll(runtime_ns);

	if (likely(exp < BORE_BURST_OFFSET))
		return 0;

	norm = runtime_ns << (63 - exp);
	frac = norm << 1;
	frac_idx = (u32)(frac >> 56) & 0xFF;

	log2_q8 = (exp << 8) | bore_log2_lut[frac_idx];
	delta_q8 = log2_q8 - (BORE_BURST_OFFSET << 8);
	penalty = (delta_q8 * BORE_BURST_SCALE) >> 16;

	return (penalty > BORE_MAX_PENALTY) ? BORE_MAX_PENALTY : penalty;
}

#define CAKE_STARVATION_PERIOD     16
#define CAKE_STARVATION_MASK       (CAKE_STARVATION_PERIOD - 1)

struct cake_starvation_state {
	u32 dispatch_count;
	u32 _pad;
};

struct {
	__uint(type, BPF_MAP_TYPE_PERCPU_ARRAY);
	__uint(max_entries, 1);
	__type(key, u32);
	__type(value, struct cake_starvation_state);
} cake_starvation_map SEC(".maps");

static __always_inline struct cake_starvation_state *get_starvation_state(void)
{
	u32 key = 0;
	return bpf_map_lookup_elem(&cake_starvation_map, &key);
}

#define BBR_STABLE_THRESHOLD       4
#define BBR_LAT_CRI_DELTA_THRESH   10

static __always_inline bool bbr_is_lat_cri_stable(u16 cur, u16 prev)
{
	u16 delta = (cur >= prev) ? (cur - prev) : (prev - cur);
	return delta <= BBR_LAT_CRI_DELTA_THRESH;
}

static __always_inline bool bbr_is_runtime_stable(u64 cur, u64 avg)
{
	u64 delta, threshold;

	if (unlikely(avg == 0))
		return false;

	threshold = avg >> 2;
	delta = (cur >= avg) ? (cur - avg) : (avg - cur);
	return delta <= threshold;
}

static __always_inline void bbr_update_stability(task_ctx *taskc)
{
	u16 cur_lat_cri, prev_lat_cri;
	u8 rounds;
	bool lat_stable, runtime_stable;

	if (unlikely(!taskc))
		return;

	cur_lat_cri = taskc->lat_cri;
	prev_lat_cri = taskc->prev_lat_cri;

	if (prev_lat_cri == 0 && taskc->stable_rounds == 0) {
		taskc->prev_lat_cri = cur_lat_cri ? cur_lat_cri : 1;
		return;
	}

	lat_stable = bbr_is_lat_cri_stable(cur_lat_cri, prev_lat_cri);
	runtime_stable = bbr_is_runtime_stable(taskc->acc_runtime, taskc->avg_runtime);

	if (lat_stable && runtime_stable) {
		rounds = taskc->stable_rounds;
		if (rounds < 255)
			rounds++;
		taskc->stable_rounds = rounds;

		if (rounds >= BBR_STABLE_THRESHOLD)
			taskc->try_fast_path = 1;
	} else {
		taskc->stable_rounds = 0;
		taskc->try_fast_path = 0;
	}

	taskc->prev_lat_cri = cur_lat_cri ? cur_lat_cri : 1;
}

static __always_inline bool bbr_can_use_fast_path(task_ctx *taskc,
						  struct cpu_ctx *cpuc)
{
	if (unlikely(!taskc || !cpuc))
		return false;

	if (!taskc->try_fast_path)
		return false;

	if (taskc->cpu_id != cpuc->cpu_id) {
		taskc->try_fast_path = 0;
		taskc->stable_rounds = 0;
		return false;
	}

	if ((u32)taskc->cpdom_id != (u32)cpuc->cpdom_id) {
		taskc->try_fast_path = 0;
		taskc->stable_rounds = 0;
		return false;
	}

	return true;
}

static __always_inline void bbr_reset_fast_path(task_ctx *taskc)
{
	if (likely(taskc)) {
		taskc->try_fast_path = 0;
		taskc->stable_rounds = 0;
	}
}

static __always_inline u64 calc_avg_smooth(u64 old_val, u64 new_val)
{
	u64 delta, step, result;

	if (old_val == new_val)
		return old_val;

	if (new_val > old_val) {
		delta = new_val - old_val;
		step = delta >> LAVD_SMOOTH_SHIFT_UP;
		step |= (u64)(step == 0);
		result = old_val + step;
		return (result > new_val) ? new_val : result;
	}

	delta = old_val - new_val;
	step = delta >> LAVD_SMOOTH_SHIFT_DOWN;
	step |= (u64)(step == 0);
	result = old_val - step;
	return (result < new_val) ? new_val : result;
}

static __always_inline void prefetch_task_hot(task_ctx *taskc)
{
	if (likely(taskc != NULL)) {
		__builtin_prefetch(taskc, 0, 3);
		__builtin_prefetch((const char *)taskc + 64, 0, 2);
	}
}

static __always_inline void prefetch_cpu_hot(struct cpu_ctx *cpuc)
{
	if (likely(cpuc != NULL)) {
		__builtin_prefetch(cpuc, 0, 3);
		__builtin_prefetch((const char *)cpuc + 64, 0, 2);
	}
}

static __always_inline void advance_cur_logical_clk(struct task_struct *p, u64 now)
{
	u64 vlc, clc, nr_queued;
	int i;

	if (unlikely(!p))
		return;

	vlc = READ_ONCE(p->scx.dsq_vtime);
	clc = READ_ONCE(cur_logical_clk);

	if (vlc <= clc)
		return;

	nr_queued = READ_ONCE(sys_stat.nr_queued_task);
	nr_queued = nr_queued ? nr_queued : 1;

	bpf_for(i, 0, LAVD_MAX_RETRY) {
		u64 diff, delta, new_clk, ret_clc;

		if (vlc <= clc)
			return;

		diff = vlc - clc;

		if (nr_queued == 1) {
			delta = diff;
		} else if (diff < nr_queued) {
			const u64 rl_mask = 0x3fffull;
			if ((now & rl_mask) != 0)
				return;
			delta = 1;
		} else {
			delta = diff / nr_queued;
		}

		new_clk = clc + delta;
		if (new_clk > vlc)
			new_clk = vlc;

		ret_clc = __sync_val_compare_and_swap(&cur_logical_clk, clc, new_clk);
		if (ret_clc == clc)
			return;

		clc = ret_clc;
	}
}

static u64 calc_time_slice(task_ctx *taskc, struct cpu_ctx *cpuc)
{
	u64 slice, base_slice, avg_runtime, max_slice, min_slice;
	u64 pinned;
	u32 pinned_tasks;
	bool boosted = false;

	if (unlikely(!taskc || !cpuc))
		return LAVD_SLICE_MAX_NS_DFL;

	base_slice = READ_ONCE(sys_stat.slice);
	avg_runtime = READ_ONCE(taskc->avg_runtime);
	max_slice = READ_ONCE(slice_max_ns);
	min_slice = READ_ONCE(slice_min_ns);

	pinned = READ_ONCE(pinned_slice_ns);
	pinned_tasks = READ_ONCE(cpuc->nr_pinned_tasks);

	if (unlikely(pinned)) {
		if (pinned_waiting_enabled()) {
			if (READ_ONCE(cpuc->nr_pinned_waiting) != 0) {
				slice = (pinned < base_slice) ? pinned : base_slice;
				taskc->slice = slice;
				reset_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);
				return slice;
			}
		} else {
			if (pinned_tasks != 0) {
				slice = (pinned < base_slice) ? pinned : base_slice;
				taskc->slice = slice;
				reset_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);
				return slice;
			}
		}
	}

	slice = base_slice;

	if (likely(avg_runtime < base_slice)) {
		if (have_turbo_core && likely(cpuc->big_core)) {
			u32 avg_perf_cri = READ_ONCE(sys_stat.avg_perf_cri);
			bool is_sparse;

			if (avg_runtime < (1ULL << BORE_BURST_OFFSET)) {
				is_sparse = true;
			} else {
				u32 burst_penalty = bore_calc_burst_penalty(avg_runtime);
				is_sparse = (burst_penalty < 20);
			}

			if (likely(taskc->perf_cri > avg_perf_cri) || is_sparse) {
				slice = base_slice + (base_slice >> 2);
				if (slice > max_slice)
					slice = max_slice;
				boosted = true;
			}
		}
		goto out;
	}

	if (!no_slice_boost && pinned_tasks == 0) {
		if (can_boost_slice()) {
			slice = avg_runtime + LAVD_SLICE_BOOST_BONUS;
			slice = clamp(slice, min_slice, (u64)LAVD_SLICE_BOOST_MAX);
			boosted = true;
		} else {
			u32 avg_lat_cri = READ_ONCE(sys_stat.avg_lat_cri);

			if (taskc->lat_cri > avg_lat_cri) {
				const u64 denom = (u64)avg_lat_cri + 1;
				const u64 lat_bonus = (base_slice * (u64)taskc->lat_cri) / denom;

				slice = base_slice + lat_bonus;
				slice = clamp(slice, min_slice, min(avg_runtime, base_slice << 1));
				boosted = true;
			}
		}
	}

out:
	taskc->slice = slice;
	if (boosted)
		set_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);
	else
		reset_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);

	return slice;
}

static void update_stat_for_running(struct task_struct *p,
				    task_ctx *taskc,
				    struct cpu_ctx *cpuc, u64 now)
{
	u64 wait_period, interval;
	struct cpu_ctx *prev_cpuc;

	if (unlikely(!p || !taskc || !cpuc))
		return;

	if (have_scheduled(taskc)) {
		wait_period = time_delta(now, taskc->last_quiescent_clk);
		interval = taskc->avg_runtime + wait_period;
		if (likely(interval > 0))
			taskc->run_freq = calc_avg_freq(taskc->run_freq, interval);
	}

	if (unlikely(is_monitored))
		taskc->resched_interval = time_delta(now, taskc->last_running_clk);

	taskc->prev_cpu_id = taskc->cpu_id;
	taskc->cpu_id = cpuc->cpu_id;

	reset_task_flag(taskc, LAVD_FLAG_IS_WAKEUP | LAVD_FLAG_IS_SYNC_WAKEUP);
	taskc->last_running_clk = now;
	taskc->last_measured_clk = now;
	taskc->last_sum_exec_clk = task_exec_time(p);

	reset_lock_futex_boost(taskc, cpuc);

	{
		u32 lat_cri = taskc->lat_cri;

		if (lat_cri > cpuc->max_lat_cri)
			cpuc->max_lat_cri = lat_cri;
		cpuc->sum_lat_cri += (u64)lat_cri;
		cpuc->nr_sched++;

		if (unlikely(have_little_core)) {
			u32 perf_cri = taskc->perf_cri;

			if (perf_cri > cpuc->max_perf_cri)
				cpuc->max_perf_cri = perf_cri;
			if (perf_cri < cpuc->min_perf_cri)
				cpuc->min_perf_cri = perf_cri;
			cpuc->sum_perf_cri += (u64)perf_cri;
		}
	}

	cpuc->flags = taskc->flags;
	cpuc->lat_cri = taskc->lat_cri;
	cpuc->running_clk = now;
	cpuc->est_stopping_clk = get_est_stopping_clk(taskc, now);

	if (is_lat_cri(taskc))
		cpuc->nr_lat_cri++;

	if (is_perf_cri(taskc))
		cpuc->nr_perf_cri++;

	prev_cpuc = get_cpu_ctx_id(taskc->prev_cpu_id);
	if (prev_cpuc && prev_cpuc->cpdom_id != cpuc->cpdom_id)
		cpuc->nr_x_migration++;

	reset_suspended_duration(cpuc);
}

static void account_task_runtime(struct task_struct *p,
				 task_ctx *taskc,
				 struct cpu_ctx *cpuc,
				 u64 now)
{
	u64 sus_dur, runtime, svc_time, sc_time, task_time, exec_delta;
	u32 weight;

	if (unlikely(!p || !taskc || !cpuc))
		return;

	sus_dur = get_suspended_duration_and_reset(cpuc);
	runtime = time_delta(now, taskc->last_measured_clk + sus_dur);

	task_time = task_exec_time(p);
	exec_delta = time_delta(task_time, taskc->last_sum_exec_clk);

	if (runtime > exec_delta)
		cpuc->stolen_time_est += runtime - exec_delta;

	runtime = exec_delta;
	if (runtime == 0) {
		taskc->last_measured_clk = now;
		taskc->last_sum_exec_clk = task_time;
		return;
	}

	weight = p->scx.weight;
	weight = weight ? weight : 1;

	svc_time = runtime / (u64)weight;
	sc_time = scale_cap_freq(runtime, cpuc);

	WRITE_ONCE(cpuc->tot_task_time, READ_ONCE(cpuc->tot_task_time) + runtime);
	WRITE_ONCE(cpuc->tot_svc_time,  READ_ONCE(cpuc->tot_svc_time)  + svc_time);
	WRITE_ONCE(cpuc->tot_sc_time,   READ_ONCE(cpuc->tot_sc_time)   + sc_time);

	taskc->acc_runtime += runtime;
	taskc->svc_time += svc_time;
	taskc->last_measured_clk = now;
	taskc->last_sum_exec_clk = task_time;

	if (enable_cpu_bw && (p->pid != lavd_pid)) {
		struct cgroup *cgrp = bpf_cgroup_from_id(taskc->cgrp_id);

		if (cgrp) {
			scx_cgroup_bw_consume(cgrp, runtime);
			bpf_cgroup_release(cgrp);
		}
	}
}

static void update_stat_for_stopping(struct task_struct *p,
				     task_ctx *taskc,
				     struct cpu_ctx *cpuc)
{
	u64 now;

	if (unlikely(!p || !taskc || !cpuc))
		return;

	now = scx_bpf_now();

	account_task_runtime(p, taskc, cpuc, now);

	taskc->avg_runtime = calc_avg_smooth(taskc->avg_runtime, taskc->acc_runtime);
	taskc->last_stopping_clk = now;
	taskc->last_slice_used = time_delta(now, taskc->last_running_clk);

	if (READ_ONCE(cur_svc_time) < taskc->svc_time)
		WRITE_ONCE(cur_svc_time, taskc->svc_time);

	bbr_update_stability(taskc);

	reset_lock_futex_boost(taskc, cpuc);
}

static void update_stat_for_refill(struct task_struct *p,
				   task_ctx *taskc,
				   struct cpu_ctx *cpuc)
{
	u64 now;

	if (unlikely(!p || !taskc || !cpuc))
		return;

	now = scx_bpf_now();

	account_task_runtime(p, taskc, cpuc, now);
	taskc->avg_runtime = calc_avg_smooth(taskc->avg_runtime, taskc->acc_runtime);
}

static __always_inline s32 try_select_idle_sibling(struct pick_ctx *ictx,
						   s32 prev_cpu)
{
	struct cpu_ctx *cpuc_prev, *cpuc_cand;
	u32 prev_cpu_u, cand_cpu_u, cluster_base_u, sibling_u;
	const volatile u32 *sibling_ptr;
	u8 prev_llc;
	u32 nr_cpu = READ_ONCE(nr_cpu_ids);
	bool task_stable = false;

	if (prev_cpu < 0)
		return -ENOENT;

	prev_cpu_u = (u32)prev_cpu;
	if (prev_cpu_u >= nr_cpu || prev_cpu_u >= LAVD_CPU_ID_MAX)
		return -ENOENT;

	cpuc_prev = get_cpu_ctx_id(prev_cpu);
	if (!cpuc_prev || !cpuc_prev->is_online)
		return -ENOENT;

	if (ictx && ictx->taskc)
		task_stable = ictx->taskc->try_fast_path;

	if (task_stable && READ_ONCE(cpuc_prev->idle_start_clk) != 0)
		return prev_cpu;

	if (is_smt_active && likely(cpuc_prev->big_core)) {
		sibling_ptr = MEMBER_VPTR(cpu_sibling, [prev_cpu_u]);
		if (sibling_ptr) {
			sibling_u = READ_ONCE(*sibling_ptr);
			if (likely(sibling_u < LAVD_CPU_ID_MAX &&
				   sibling_u < nr_cpu &&
				   sibling_u != prev_cpu_u)) {
				cpuc_cand = get_cpu_ctx_id((s32)sibling_u);
				if (likely(cpuc_cand && cpuc_cand->is_online &&
					   READ_ONCE(cpuc_cand->idle_start_clk) != 0))
					return (s32)sibling_u;
			}
		}
	}

	if (likely(have_turbo_core && ictx && ictx->taskc)) {
		u32 task_perf_cri = READ_ONCE(ictx->taskc->perf_cri);
		u32 avg_perf_cri = READ_ONCE(sys_stat.avg_perf_cri);
		bool is_sparse;

		u64 avg_runtime = READ_ONCE(ictx->taskc->avg_runtime);
		if (task_stable) {
			is_sparse = (avg_runtime < (1ULL << BORE_BURST_OFFSET));
		} else if (avg_runtime < (1ULL << BORE_BURST_OFFSET)) {
			is_sparse = true;
		} else {
			u32 burst_penalty = bore_calc_burst_penalty(avg_runtime);
			is_sparse = (burst_penalty < 20);
		}

		if (likely(task_perf_cri > avg_perf_cri) || is_sparse) {
			u64 nr_big_raw = READ_ONCE(nr_cpus_big);
			u32 nr_scan = (u32)((nr_big_raw > 16) ? 16 : nr_big_raw);

			bpf_for(cand_cpu_u, 0, nr_scan) {
				if (cand_cpu_u >= nr_cpu || cand_cpu_u >= LAVD_CPU_ID_MAX)
					break;

				cpuc_cand = get_cpu_ctx_id((s32)cand_cpu_u);
				if (likely(cpuc_cand && cpuc_cand->is_online &&
					   cpuc_cand->turbo_core &&
					   READ_ONCE(cpuc_cand->idle_start_clk) != 0))
					return (s32)cand_cpu_u;
			}
		}
	}

	if (likely(!cpuc_prev->big_core)) {
		cluster_base_u = prev_cpu_u & ~3U;
		prev_llc = cpuc_prev->llc_id;

		bpf_for(cand_cpu_u, cluster_base_u, cluster_base_u + 4) {
			if (cand_cpu_u >= LAVD_CPU_ID_MAX ||
			    cand_cpu_u >= nr_cpu ||
			    cand_cpu_u == prev_cpu_u)
				continue;

			cpuc_cand = get_cpu_ctx_id((s32)cand_cpu_u);
			if (likely(cpuc_cand && cpuc_cand->is_online &&
				   !cpuc_cand->big_core &&
				   cpuc_cand->llc_id == prev_llc &&
				   READ_ONCE(cpuc_cand->idle_start_clk) != 0))
				return (s32)cand_cpu_u;
		}
	}

	return -ENOENT;
}

s32 BPF_STRUCT_OPS(lavd_select_cpu, struct task_struct *p, s32 prev_cpu,
		   u64 wake_flags)
{
	struct pick_ctx ictx = {
		.p = p,
		.taskc = NULL,
		.prev_cpu = prev_cpu,
		.cpuc_cur = NULL,
		.wake_flags = wake_flags,
	};
	struct cpu_ctx *cpuc_prev = NULL;
	struct cpu_ctx *cpuc_new = NULL;
	task_ctx *taskc;
	struct cpu_ctx *cpuc_cur;
	bool found_idle = false;
	s32 cpu_id = -ENOENT;
	u32 nrcpu;

	if (unlikely(!p))
		return prev_cpu;

	taskc = get_task_ctx(p);
	cpuc_cur = get_cpu_ctx();
	if (unlikely(!taskc || !cpuc_cur))
		return prev_cpu;

	ictx.taskc = taskc;
	ictx.cpuc_cur = cpuc_cur;

	prefetch_task_hot(taskc);

	if (wake_flags & SCX_WAKE_SYNC)
		set_task_flag(taskc, LAVD_FLAG_IS_SYNC_WAKEUP);
	else
		reset_task_flag(taskc, LAVD_FLAG_IS_SYNC_WAKEUP);

	nrcpu = READ_ONCE(nr_cpu_ids);

	if (prev_cpu >= 0 && (u32)prev_cpu < nrcpu && prev_cpu < LAVD_CPU_ID_MAX) {
		cpuc_prev = get_cpu_ctx_id(prev_cpu);
		if (cpuc_prev && cpuc_prev->is_online &&
		    READ_ONCE(cpuc_prev->idle_start_clk) != 0) {
			u64 now = scx_bpf_now();
			u64 last_stop = READ_ONCE(taskc->last_stopping_clk);

			if (time_delta(now, last_stop) < LAVD_SLICE_MAX_NS_DFL) {
				cpu_id = prev_cpu;
				found_idle = true;
				goto apply_policy;
			}
		}
	}

	cpu_id = try_select_idle_sibling(&ictx, prev_cpu);
	if (cpu_id >= 0 && (u32)cpu_id < nrcpu) {
		found_idle = true;
		goto apply_policy;
	}

	cpu_id = pick_idle_cpu(&ictx, &found_idle);

	if (cpu_id < 0 || (u32)cpu_id >= nrcpu)
		cpu_id = (prev_cpu >= 0 && (u32)prev_cpu < nrcpu) ? prev_cpu : (s32)cpuc_cur->cpu_id;

apply_policy:
	if (have_little_core && cpu_id >= 0 && (u32)cpu_id < nrcpu &&
	    cpuc_prev && cpuc_prev->big_core && cpu_id != prev_cpu) {
		u32 avg_perf_cri = READ_ONCE(sys_stat.avg_perf_cri);

		if (taskc->perf_cri > avg_perf_cri) {
			cpuc_new = get_cpu_ctx_id(cpu_id);
			if (cpuc_new && !cpuc_new->big_core && cpuc_prev->is_online) {
				cpu_id = prev_cpu;
				found_idle = (READ_ONCE(cpuc_prev->idle_start_clk) != 0);
			}
		}
	}

	WRITE_ONCE(taskc->suggested_cpu_id, (u32)cpu_id);

	if (found_idle) {
		struct cpu_ctx *cpuc;

		set_task_flag(taskc, LAVD_FLAG_IDLE_CPU_PICKED);

		if (cpu_id < 0 || cpu_id >= LAVD_CPU_ID_MAX)
			goto out;

		if (cpu_id == prev_cpu && cpuc_prev)
			cpuc = cpuc_prev;
		else
			cpuc = get_cpu_ctx_id(cpu_id);

		if (!cpuc) {
			scx_bpf_error("Failed to lookup cpu_ctx: %d", cpu_id);
			goto out;
		}

		if (likely(!queued_on_cpu(cpuc))) {
			p->scx.dsq_vtime = calc_when_to_run(p, taskc);
			p->scx.slice = LAVD_SLICE_MAX_NS_DFL;
			scx_bpf_dsq_insert(p, SCX_DSQ_LOCAL, p->scx.slice, 0);
			goto out;
		}
	} else {
		reset_task_flag(taskc, LAVD_FLAG_IDLE_CPU_PICKED);
	}

out:
	return cpu_id;
}

static int cgroup_throttled(struct task_struct *p, task_ctx *taskc, bool put_aside)
{
	struct cgroup *cgrp;
	int ret, ret2;

	if (unlikely(!p || !taskc))
		return -EINVAL;

	cgrp = bpf_cgroup_from_id(taskc->cgrp_id);
	if (!cgrp) {
		debugln("Failed to lookup a cgroup: %llu", taskc->cgrp_id);
		return -ESRCH;
	}

	ret = scx_cgroup_bw_throttled(cgrp);
	if ((ret == -EAGAIN) && put_aside) {
		ret2 = scx_cgroup_bw_put_aside(p, (u64)taskc, p->scx.dsq_vtime, cgrp);
		if (ret2) {
			bpf_cgroup_release(cgrp);
			return ret2;
		}
	}

	bpf_cgroup_release(cgrp);
	return ret;
}

void BPF_STRUCT_OPS(lavd_enqueue, struct task_struct *p, u64 enq_flags)
{
	struct cpu_ctx *cpuc, *cpuc_cur;
	s32 task_cpu, cpu = -ENOENT;
	bool is_idle = false;
	task_ctx *taskc;
	u64 dsq_id;

	if (unlikely(!p))
		return;

	taskc = get_task_ctx(p);
	cpuc_cur = get_cpu_ctx();
	if (unlikely(!taskc || !cpuc_cur)) {
		scx_bpf_error("Failed to lookup contexts in enqueue");
		return;
	}

	if (!(enq_flags & SCX_ENQ_REENQ)) {
		if (enq_flags & SCX_ENQ_WAKEUP)
			set_task_flag(taskc, LAVD_FLAG_IS_WAKEUP);
		else
			reset_task_flag(taskc, LAVD_FLAG_IS_WAKEUP);

		p->scx.dsq_vtime = calc_when_to_run(p, taskc);
	}

	p->scx.slice = LAVD_SLICE_MIN_NS_DFL;

	if (enable_cpu_bw && (p->pid != lavd_pid)) {
		if (cgroup_throttled(p, taskc, true) == -EAGAIN) {
			reset_task_flag(taskc, LAVD_FLAG_IDLE_CPU_PICKED);
			return;
		}
	}

	task_cpu = scx_bpf_task_cpu(p);
	if (!__COMPAT_is_enq_cpu_selected(enq_flags)) {
		struct pick_ctx ictx = {
			.p = p,
			.taskc = taskc,
			.prev_cpu = task_cpu,
			.cpuc_cur = cpuc_cur,
			.wake_flags = 0,
		};

		cpu = pick_idle_cpu(&ictx, &is_idle);
		if (cpu < 0 || (u32)cpu >= nr_cpu_ids) {
			cpu = (task_cpu >= 0 && (u32)task_cpu < nr_cpu_ids) ?
			      task_cpu : (s32)cpuc_cur->cpu_id;
			is_idle = false;
		}
	} else {
		cpu = task_cpu;
		is_idle = test_task_flag(taskc, LAVD_FLAG_IDLE_CPU_PICKED);
		reset_task_flag(taskc, LAVD_FLAG_IDLE_CPU_PICKED);
	}

	cpuc = get_cpu_ctx_id(cpu);
	if (unlikely(!cpuc)) {
		cpu = (s32)cpuc_cur->cpu_id;
		cpuc = cpuc_cur;
	}

	WRITE_ONCE(taskc->suggested_cpu_id, (u32)cpu);
	WRITE_ONCE(taskc->cpdom_id, (u32)cpuc->cpdom_id);

	if (is_pinned(p) && (READ_ONCE(taskc->pinned_cpu_id) == -ENOENT)) {
		WRITE_ONCE(taskc->pinned_cpu_id, cpu);
		__sync_fetch_and_add(&cpuc->nr_pinned_tasks, 1);
	}

	maybe_inc_pinned_waiting(taskc, cpuc, p);

	if (is_idle && !queued_on_cpu(cpuc)) {
		scx_bpf_dsq_insert(p, SCX_DSQ_LOCAL_ON | (u64)(u32)cpu, p->scx.slice, enq_flags);
	} else {
		dsq_id = get_target_dsq_id(p, cpuc);
		scx_bpf_dsq_insert_vtime(p, dsq_id, p->scx.slice, p->scx.dsq_vtime, enq_flags);
	}

	if (is_idle) {
		scx_bpf_kick_cpu(cpu, SCX_KICK_IDLE);
		return;
	}

	if (!no_preemption)
		try_find_and_kick_victim_cpu(p, taskc, cpu, cpdom_to_dsq(cpuc->cpdom_id));
}

static int enqueue_cb(struct task_struct __arg_trusted *p)
{
	struct cpu_ctx *cpuc;
	task_ctx *taskc;
	u64 dsq_id;
	s32 cpu;

	if (unlikely(!p))
		return 0;

	taskc = get_task_ctx(p);
	if (!taskc) {
		scx_bpf_error("Failed to lookup a task context: %d", p->pid);
		return 0;
	}

	p->scx.dsq_vtime = calc_when_to_run(p, taskc);

	cpu = (s32)taskc->suggested_cpu_id;
	if (cpu < 0 || cpu >= LAVD_CPU_ID_MAX) {
		scx_bpf_error("Invalid suggested_cpu_id %d", cpu);
		return 0;
	}

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return 0;
	}

	if (is_pinned(p) && (READ_ONCE(taskc->pinned_cpu_id) == -ENOENT)) {
		WRITE_ONCE(taskc->pinned_cpu_id, cpu);
		__sync_fetch_and_add(&cpuc->nr_pinned_tasks, 1);
	}

	maybe_inc_pinned_waiting(taskc, cpuc, p);

	dsq_id = get_target_dsq_id(p, cpuc);
	scx_bpf_dsq_insert_vtime(p, dsq_id, p->scx.slice, p->scx.dsq_vtime, 0);

	return 0;
}

void BPF_STRUCT_OPS(lavd_dequeue, struct task_struct *p, u64 deq_flags)
{
	task_ctx *taskc;
	int ret;

	(void)deq_flags;

	if (!enable_cpu_bw)
		return;

	if (unlikely(!p))
		return;

	taskc = get_task_ctx(p);
	if (!taskc) {
		debugln("Failed to lookup task_ctx for task %d", p->pid);
		return;
	}

	if ((ret = scx_cgroup_bw_cancel((u64)taskc)))
		debugln("Failed to cancel task %d with %d", p->pid, ret);
}

static __always_inline void consume_prev_task(struct task_struct *prev,
					      task_ctx *taskc_prev,
					      struct cpu_ctx *cpuc)
{
	if (!prev || !cpuc)
		return;
	if (!(prev->scx.flags & SCX_TASK_QUEUED))
		return;

	if (!taskc_prev)
		taskc_prev = get_task_ctx(prev);
	if (!taskc_prev)
		return;

	update_stat_for_refill(prev, taskc_prev, cpuc);

	if (enable_cpu_bw && (prev->pid != lavd_pid) &&
	    (cgroup_throttled(prev, taskc_prev, false) == -EAGAIN))
		return;

	prev->scx.slice = calc_time_slice(taskc_prev, cpuc);

	if (is_lock_holder_running(cpuc))
		reset_lock_futex_boost(taskc_prev, cpuc);

	cpuc->flags = taskc_prev->flags;
}

void BPF_STRUCT_OPS(lavd_dispatch, s32 cpu, struct task_struct *prev)
{
	struct bpf_cpumask *active, *ovrflw;
	u64 cpu_dsq_id, cpdom_dsq_id;
	task_ctx *taskc_prev = NULL;
	bool try_consume = false;
	struct cpu_ctx *cpuc;
	struct task_struct *iter_p;
	int ret;
	const bool prev_queued = prev && (prev->scx.flags & SCX_TASK_QUEUED);

	if (cpu < 0 || (u32)cpu >= nr_cpu_ids || cpu >= LAVD_CPU_ID_MAX)
		return;

	cpuc = get_cpu_ctx_id(cpu);
	if (unlikely(!cpuc)) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}

	prefetch_cpu_hot(cpuc);

	if (prev_queued)
		prefetch_task_hot(get_task_ctx(prev));

	cpu_dsq_id = cpu_to_dsq((u32)cpu);
	cpdom_dsq_id = cpdom_to_dsq(cpuc->cpdom_id);

	{
		struct cake_starvation_state *starv = get_starvation_state();
		if (starv) {
			u32 cnt = starv->dispatch_count + 1;
			starv->dispatch_count = cnt;

			if ((cnt & CAKE_STARVATION_MASK) == 0) {
				if (use_cpdom_dsq() &&
				    scx_bpf_dsq_nr_queued(cpdom_dsq_id) > 0) {
					if (scx_bpf_dsq_move_to_local(cpdom_dsq_id))
						return;
				}
			}
		}
	}

	if (enable_cpu_bw && (ret = scx_cgroup_bw_reenqueue()))
		scx_bpf_error("Failed to reenqueue backlogged tasks: %d", ret);

	if (prev_queued) {
		const u64 cpu_q = scx_bpf_dsq_nr_queued(cpu_dsq_id);
		u64 dom_q = 0;

		if (use_cpdom_dsq())
			dom_q = scx_bpf_dsq_nr_queued(cpdom_dsq_id);

		if (cpu_q == 0 && (!use_cpdom_dsq() || dom_q == 0)) {
			consume_prev_task(prev, NULL, cpuc);
			return;
		}
	}

	if (prev_queued && is_lock_holder_running(cpuc)) {
		consume_prev_task(prev, NULL, cpuc);
		return;
	}

	if (use_full_cpus())
		goto consume_out;

	bpf_rcu_read_lock();

	active = active_cpumask;
	ovrflw = ovrflw_cpumask;
	if (unlikely(!active || !ovrflw)) {
		scx_bpf_error("Failed to prepare cpumasks.");
		bpf_rcu_read_unlock();
		return;
	}

	if (bpf_cpumask_test_cpu((u32)cpu, cast_mask(active)) ||
	    bpf_cpumask_test_cpu((u32)cpu, cast_mask(ovrflw))) {
		bpf_rcu_read_unlock();
		goto consume_out;
	}

	if (use_per_cpu_dsq() && scx_bpf_dsq_nr_queued(cpu_dsq_id)) {
		bpf_cpumask_set_cpu((u32)cpu, ovrflw);
		bpf_rcu_read_unlock();
		goto consume_out;
	}

	if (prev) {
		if (is_pinned(prev)) {
			bpf_cpumask_set_cpu((u32)cpu, ovrflw);
			bpf_rcu_read_unlock();
			goto consume_out;
		} else if (is_migration_disabled(prev)) {
			bpf_rcu_read_unlock();
			goto consume_out;
		}

		taskc_prev = get_task_ctx(prev);
		if (taskc_prev &&
		    test_task_flag(taskc_prev, LAVD_FLAG_IS_AFFINITIZED) &&
		    bpf_cpumask_test_cpu((u32)cpu, prev->cpus_ptr) &&
		    !bpf_cpumask_intersects(cast_mask(active), prev->cpus_ptr) &&
		    !bpf_cpumask_intersects(cast_mask(ovrflw), prev->cpus_ptr)) {
			bpf_cpumask_set_cpu((u32)cpu, ovrflw);
			bpf_rcu_read_unlock();
			goto consume_out;
		}
	}

	if (!use_cpdom_dsq()) {
		bpf_rcu_read_unlock();
		return;
	}

	bpf_for_each(scx_dsq, iter_p, cpdom_dsq_id, 0) {
		task_ctx *taskc;
		struct task_struct *task_ref;
		s32 new_cpu;

		task_ref = bpf_task_from_pid(iter_p->pid);
		if (!task_ref)
			break;

		if (is_pinned(task_ref)) {
			new_cpu = scx_bpf_task_cpu(task_ref);
			if (new_cpu == cpu) {
				bpf_cpumask_set_cpu((u32)new_cpu, ovrflw);
				bpf_task_release(task_ref);
				try_consume = true;
				break;
			}
			if (new_cpu >= 0 && (u32)new_cpu < nr_cpu_ids &&
			    !bpf_cpumask_test_and_set_cpu((u32)new_cpu, ovrflw))
				scx_bpf_kick_cpu(new_cpu, SCX_KICK_IDLE);
			bpf_task_release(task_ref);
			continue;
		} else if (is_migration_disabled(task_ref)) {
			new_cpu = scx_bpf_task_cpu(task_ref);
			if (new_cpu == cpu) {
				bpf_task_release(task_ref);
				try_consume = true;
				break;
			}
			bpf_task_release(task_ref);
			continue;
		}

		taskc = get_task_ctx(task_ref);
		if (taskc &&
		    (!test_task_flag(taskc, LAVD_FLAG_IS_AFFINITIZED) ||
		     bpf_cpumask_intersects(cast_mask(active), task_ref->cpus_ptr) ||
		     bpf_cpumask_intersects(cast_mask(ovrflw), task_ref->cpus_ptr))) {
			bpf_task_release(task_ref);
			continue;
		}

		new_cpu = find_cpu_in(task_ref->cpus_ptr, cpuc);
		if (new_cpu >= 0 && (u32)new_cpu < nr_cpu_ids) {
			if (new_cpu == cpu) {
				bpf_cpumask_set_cpu((u32)new_cpu, ovrflw);
				bpf_task_release(task_ref);
				try_consume = true;
				break;
			} else if (!bpf_cpumask_test_and_set_cpu((u32)new_cpu, ovrflw)) {
				scx_bpf_kick_cpu(new_cpu, SCX_KICK_IDLE);
			}
		}

		bpf_task_release(task_ref);
	}

	bpf_rcu_read_unlock();

	if (!try_consume)
		return;

consume_out:
	if (consume_task(cpu_dsq_id, cpdom_dsq_id))
		return;

	consume_prev_task(prev, taskc_prev, cpuc);
}

void BPF_STRUCT_OPS(lavd_runnable, struct task_struct *p, u64 enq_flags)
{
	struct task_struct *waker;
	task_ctx *p_taskc, *waker_taskc;
	u64 now, interval;

	if (unlikely(!p))
		return;

	p_taskc = get_task_ctx(p);
	if (!p_taskc) {
		scx_bpf_error("Failed to lookup task_ctx for task %d", p->pid);
		return;
	}
	p_taskc->acc_runtime = 0;

	if (!(enq_flags & SCX_ENQ_WAKEUP))
		return;

	if (enq_flags & (SCX_ENQ_PREEMPT | SCX_ENQ_REENQ | SCX_ENQ_LAST))
		return;

	waker = bpf_get_current_task_btf();
	if (!waker)
		return;

	if ((p->real_parent != waker->real_parent))
		return;

	if (is_kernel_task(p) != is_kernel_task(waker))
		return;

	if (rt_or_dl_task(waker))
		set_task_flag(p_taskc, LAVD_FLAG_WOKEN_BY_RT_DL);
	else
		reset_task_flag(p_taskc, LAVD_FLAG_WOKEN_BY_RT_DL);

	waker_taskc = get_task_ctx(waker);
	if (!waker_taskc)
		return;

	now = scx_bpf_now();
	interval = time_delta(now, READ_ONCE(waker_taskc->last_runnable_clk));
	if (interval >= LAVD_LC_WAKE_INTERVAL_MIN) {
		WRITE_ONCE(waker_taskc->wake_freq,
			   calc_avg_freq(waker_taskc->wake_freq, interval));
		WRITE_ONCE(waker_taskc->last_runnable_clk, now);
	}

	p_taskc->lat_cri_waker = waker_taskc->lat_cri;
	if (waker_taskc->lat_cri_wakee < p_taskc->lat_cri)
		waker_taskc->lat_cri_wakee = p_taskc->lat_cri;

	if (is_monitored) {
		char comm_buf[TASK_COMM_LEN];
		int i;

		p_taskc->waker_pid = waker->pid;
		bpf_probe_read_kernel(comm_buf, sizeof(comm_buf), waker->comm);

		for (i = 0; i < TASK_COMM_LEN && can_loop; i++)
			p_taskc->waker_comm[i] = comm_buf[i];
	}
}

void BPF_STRUCT_OPS(lavd_running, struct task_struct *p)
{
	struct cpu_ctx *cpuc;
	task_ctx *taskc;
	u64 now;
	bool fast_path;

	if (unlikely(!p))
		return;

	now = scx_bpf_now();

	cpuc = get_cpu_ctx_task(p);
	taskc = get_task_ctx(p);
	if (!cpuc || !taskc) {
		scx_bpf_error("Failed to lookup context for task %d", p->pid);
		return;
	}

	maybe_dec_pinned_waiting(taskc, cpuc);

	if (p->scx.slice == SCX_SLICE_DFL)
		p->scx.dsq_vtime = READ_ONCE(cur_logical_clk);

	fast_path = bbr_can_use_fast_path(taskc, cpuc);

	if (fast_path) {
		u64 cached_slice = taskc->slice;
		u64 min_slice = READ_ONCE(slice_min_ns);
		u64 max_slice = READ_ONCE(slice_max_ns);

		if (cached_slice >= min_slice && cached_slice <= max_slice) {
			p->scx.slice = cached_slice;
		} else {
			fast_path = false;
			p->scx.slice = calc_time_slice(taskc, cpuc);
		}
	} else {
		p->scx.slice = calc_time_slice(taskc, cpuc);
	}

	advance_cur_logical_clk(p, now);

	update_stat_for_running(p, taskc, cpuc, now);

	if (!fast_path || unlikely((now & 0xFFF) == 0))
		update_cpuperf_target(cpuc);

	try_proc_introspec_cmd(p, taskc);
}

void BPF_STRUCT_OPS(lavd_tick, struct task_struct *p)
{
	struct cpu_ctx *cpuc;
	task_ctx *taskc;
	u64 now;

	if (unlikely(!p))
		return;

	cpuc = get_cpu_ctx_task(p);
	taskc = get_task_ctx(p);
	if (unlikely(!cpuc || !taskc)) {
		scx_bpf_error("Failed to lookup context for task %d", p->pid);
		return;
	}

	now = scx_bpf_now();
	account_task_runtime(p, taskc, cpuc, now);

	if (enable_cpu_bw && (cgroup_throttled(p, taskc, false) == -EAGAIN)) {
		preempt_at_tick(p, cpuc);
		return;
	}

	if (READ_ONCE(cpuc->nr_pinned_tasks) != 0)
		shrink_slice_at_tick(p, cpuc, now);
}

void BPF_STRUCT_OPS(lavd_stopping, struct task_struct *p, bool runnable)
{
	struct cpu_ctx *cpuc;
	task_ctx *taskc;

	(void)runnable;

	if (unlikely(!p))
		return;

	cpuc = get_cpu_ctx_task(p);
	taskc = get_task_ctx(p);
	if (!cpuc || !taskc) {
		scx_bpf_error("Failed to lookup context for task %d", p->pid);
		return;
	}

	update_stat_for_stopping(p, taskc, cpuc);
}

void BPF_STRUCT_OPS(lavd_quiescent, struct task_struct *p, u64 deq_flags)
{
	struct cpu_ctx *cpuc;
	task_ctx *taskc;
	u64 now, interval;
	s32 pin_cpu;

	if (unlikely(!p))
		return;

	cpuc = get_cpu_ctx_task(p);
	taskc = get_task_ctx(p);
	if (!cpuc || !taskc) {
		scx_bpf_error("Failed to lookup context for task %d", p->pid);
		return;
	}
	cpuc->flags = 0;

	maybe_dec_pinned_waiting(taskc, cpuc);

	pin_cpu = READ_ONCE(taskc->pinned_cpu_id);
	if (pin_cpu != -ENOENT) {
		struct cpu_ctx *pinc = NULL;

		if (pin_cpu >= 0 && pin_cpu < LAVD_CPU_ID_MAX)
			pinc = get_cpu_ctx_id(pin_cpu);

		if (pinc)
			__sync_fetch_and_sub(&pinc->nr_pinned_tasks, 1);
		else
			__sync_fetch_and_sub(&cpuc->nr_pinned_tasks, 1);

		WRITE_ONCE(taskc->pinned_cpu_id, -ENOENT);
	}

	if (!(deq_flags & SCX_DEQ_SLEEP))
		return;

	now = scx_bpf_now();
	interval = time_delta(now, taskc->last_quiescent_clk);
	if (interval > 0) {
		taskc->wait_freq = calc_avg_freq(taskc->wait_freq, interval);
		taskc->last_quiescent_clk = now;
	}
}

static void cpu_ctx_init_online(struct cpu_ctx *cpuc, u32 cpu_id, u64 now)
{
	struct bpf_cpumask *cd_cpumask;

	if (unlikely(!cpuc))
		return;

	bpf_rcu_read_lock();
	if (cpuc->cpdom_id < LAVD_CPDOM_MAX_NR) {
		cd_cpumask = MEMBER_VPTR(cpdom_cpumask, [cpuc->cpdom_id]);
		if (cd_cpumask)
			bpf_cpumask_set_cpu(cpu_id, cd_cpumask);
	}
	bpf_rcu_read_unlock();

	cpuc->flags = 0;
	cpuc->idle_start_clk = 0;
	cpuc->lat_cri = 0;
	cpuc->running_clk = 0;
	cpuc->est_stopping_clk = SCX_SLICE_INF;
	WRITE_ONCE(cpuc->online_clk, now);
	barrier();

	cpuc->is_online = true;
}

static void cpu_ctx_init_offline(struct cpu_ctx *cpuc, u32 cpu_id, u64 now)
{
	struct bpf_cpumask *cd_cpumask;

	if (unlikely(!cpuc))
		return;

	bpf_rcu_read_lock();
	if (cpuc->cpdom_id < LAVD_CPDOM_MAX_NR) {
		cd_cpumask = MEMBER_VPTR(cpdom_cpumask, [cpuc->cpdom_id]);
		if (cd_cpumask)
			bpf_cpumask_clear_cpu(cpu_id, cd_cpumask);
	}
	bpf_rcu_read_unlock();

	cpuc->flags = 0;
	cpuc->idle_start_clk = 0;
	WRITE_ONCE(cpuc->offline_clk, now);
	cpuc->is_online = false;
	barrier();

	cpuc->lat_cri = 0;
	cpuc->running_clk = 0;
	cpuc->est_stopping_clk = SCX_SLICE_INF;
}

void BPF_STRUCT_OPS(lavd_cpu_online, s32 cpu)
{
	u64 now = scx_bpf_now();
	struct cpu_ctx *cpuc;

	if (cpu < 0 || cpu >= LAVD_CPU_ID_MAX)
		return;

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}

	cpu_ctx_init_online(cpuc, (u32)cpu, now);

	__sync_fetch_and_add(&nr_cpus_onln, 1);
	__sync_fetch_and_add(&total_max_capacity, cpuc->max_capacity);
	update_autopilot_high_cap();
	update_sys_stat();
}

void BPF_STRUCT_OPS(lavd_cpu_offline, s32 cpu)
{
	u64 now = scx_bpf_now();
	struct cpu_ctx *cpuc;

	if (cpu < 0 || cpu >= LAVD_CPU_ID_MAX)
		return;

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}

	cpu_ctx_init_offline(cpuc, (u32)cpu, now);

	__sync_fetch_and_sub(&nr_cpus_onln, 1);
	__sync_fetch_and_sub(&total_max_capacity, cpuc->max_capacity);
	update_autopilot_high_cap();
	update_sys_stat();
}

void BPF_STRUCT_OPS(lavd_update_idle, s32 cpu, bool idle)
{
	struct cpu_ctx *cpuc;
	u64 now = scx_bpf_now();
	int i;

	if (cpu < 0 || cpu >= LAVD_CPU_ID_MAX)
		return;

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}

	if (idle) {
		WRITE_ONCE(cpuc->idle_start_clk, now);
		reset_cpu_preemption_info(cpuc, false);
	} else {
		bpf_for(i, 0, LAVD_MAX_RETRY) {
			u64 old_clk = READ_ONCE(cpuc->idle_start_clk);

			if (old_clk == 0)
				break;

			if (__sync_bool_compare_and_swap(&cpuc->idle_start_clk, old_clk, 0)) {
				if (time_after(old_clk, now))
					break;

				__sync_fetch_and_add(&cpuc->idle_total, time_delta(now, old_clk));
				break;
			}
		}
	}
}

void BPF_STRUCT_OPS(lavd_set_cpumask, struct task_struct *p,
		    const struct cpumask *cpumask)
{
	task_ctx *taskc;
	s32 pin_cpu;

	if (unlikely(!p))
		return;

	taskc = get_task_ctx(p);
	if (!taskc) {
		scx_bpf_error("task_ctx_stor first lookup failed");
		return;
	}

	pin_cpu = READ_ONCE(taskc->pinned_cpu_id);
	if (pin_cpu != -ENOENT && !is_pinned(p)) {
		struct cpu_ctx *pinc = NULL;

		if (pin_cpu >= 0 && pin_cpu < LAVD_CPU_ID_MAX)
			pinc = get_cpu_ctx_id(pin_cpu);

		if (pinc)
			__sync_fetch_and_sub(&pinc->nr_pinned_tasks, 1);

		WRITE_ONCE(taskc->pinned_cpu_id, -ENOENT);
	}

	bbr_reset_fast_path(taskc);

	if (bpf_cpumask_weight(p->cpus_ptr) != nr_cpu_ids)
		set_task_flag(taskc, LAVD_FLAG_IS_AFFINITIZED);
	else
		reset_task_flag(taskc, LAVD_FLAG_IS_AFFINITIZED);

	set_on_core_type(taskc, cpumask);
}

void BPF_STRUCT_OPS(lavd_cpu_acquire, s32 cpu,
		    struct scx_cpu_acquire_args *args)
{
	struct cpu_ctx *cpuc;
	u64 dur, scaled_dur;

	(void)args;

	if (cpu < 0 || cpu >= LAVD_CPU_ID_MAX)
		return;

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}

	dur = time_delta(scx_bpf_now(), cpuc->cpu_release_clk);
	scaled_dur = scale_cap_freq(dur, cpuc);
	cpuc->tot_sc_time += scaled_dur;
	cpuc->cpuperf_cur = scx_bpf_cpuperf_cur(cpu);
}

void BPF_STRUCT_OPS(lavd_cpu_release, s32 cpu,
		    struct scx_cpu_release_args *args)
{
	struct cpu_ctx *cpuc;

	(void)args;

	if (cpu < 0 || cpu >= LAVD_CPU_ID_MAX)
		return;

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}
	cpuc->flags = 0;

	reset_cpu_preemption_info(cpuc, true);
	scx_bpf_reenqueue_local();
	reset_cpuperf_target(cpuc);
	cpuc->cpu_release_clk = scx_bpf_now();
}

void BPF_STRUCT_OPS(lavd_enable, struct task_struct *p)
{
	task_ctx *taskc;

	if (unlikely(!p))
		return;

	taskc = get_task_ctx(p);
	if (!taskc) {
		scx_bpf_error("task_ctx_stor first lookup failed");
		return;
	}

	taskc->svc_time = READ_ONCE(cur_svc_time);
}

s32 BPF_STRUCT_OPS_SLEEPABLE(lavd_init_task, struct task_struct *p,
			     struct scx_init_task_args *args)
{
	task_ctx *taskc;
	u64 now;
	int i;

	if (!p) {
		scx_bpf_error("NULL task_struct pointer received");
		return -ESRCH;
	}

	taskc = scx_task_alloc(p);
	if (!taskc) {
		scx_bpf_error("task_ctx_stor first lookup failed");
		return -ENOMEM;
	}

	bpf_for(i, 0, (int)sizeof(*taskc)) {
		if (!can_loop)
			break;
		((char __arena *)taskc)[i] = 0;
	}

	bpf_rcu_read_lock();
	if (bpf_cpumask_weight(p->cpus_ptr) != nr_cpu_ids)
		set_task_flag(taskc, LAVD_FLAG_IS_AFFINITIZED);
	else
		reset_task_flag(taskc, LAVD_FLAG_IS_AFFINITIZED);
	bpf_rcu_read_unlock();

	if (is_ksoftirqd(p))
		set_task_flag(taskc, LAVD_FLAG_KSOFTIRQD);
	else
		reset_task_flag(taskc, LAVD_FLAG_KSOFTIRQD);

	now = scx_bpf_now();
	taskc->last_runnable_clk = now;
	taskc->last_running_clk = now;
	taskc->last_stopping_clk = now;
	taskc->last_quiescent_clk = now;
	taskc->avg_runtime = sys_stat.slice;
	taskc->svc_time = sys_stat.avg_svc_time;
	taskc->pinned_cpu_id = -ENOENT;
	taskc->pid = p->pid;

	taskc->stable_rounds = 0;
	taskc->try_fast_path = 0;
	taskc->prev_lat_cri = 0;

	if (args && args->cgroup && args->cgroup->kn)
		taskc->cgrp_id = args->cgroup->kn->id;
	else
		taskc->cgrp_id = 0;

	bpf_rcu_read_lock();
	set_on_core_type(taskc, p->cpus_ptr);
	bpf_rcu_read_unlock();

	return 0;
}

s32 BPF_STRUCT_OPS(lavd_exit_task, struct task_struct *p,
		   struct scx_exit_task_args *args)
{
	(void)args;

	if (likely(p))
		scx_task_free(p);
	return 0;
}

static s32 init_cpdoms(u64 now)
{
	struct cpdom_ctx *cpdomc;
	int err;
	int i;

	(void)now;

	bpf_for(i, 0, LAVD_CPDOM_MAX_NR) {
		cpdomc = MEMBER_VPTR(cpdom_ctxs, [i]);
		if (!cpdomc) {
			scx_bpf_error("Failed to lookup cpdom_ctx for %d", i);
			return -ESRCH;
		}
		if (!cpdomc->is_valid)
			continue;

		if (use_cpdom_dsq()) {
			err = scx_bpf_create_dsq(cpdom_to_dsq(cpdomc->id), cpdomc->numa_id);
			if (err) {
				scx_bpf_error("Failed to create DSQ for cpdom %llu", cpdomc->id);
				return err;
			}
		}

		nr_cpdoms = i + 1;
	}

	return 0;
}

static int calloc_cpumask(struct bpf_cpumask **p_cpumask)
{
	struct bpf_cpumask *cpumask;

	if (unlikely(!p_cpumask))
		return -EINVAL;

	cpumask = bpf_cpumask_create();
	if (!cpumask)
		return -ENOMEM;

	cpumask = bpf_kptr_xchg(p_cpumask, cpumask);
	if (cpumask)
		bpf_cpumask_release(cpumask);

	return 0;
}

static int init_cpumasks(void)
{
	const struct cpumask *online_cpumask;
	struct bpf_cpumask *active;
	int err = 0;

	bpf_rcu_read_lock();
	err = calloc_cpumask(&active_cpumask);
	active = active_cpumask;
	if (err || !active)
		goto out;

	online_cpumask = scx_bpf_get_online_cpumask();
	nr_cpus_onln = bpf_cpumask_weight(online_cpumask);
	bpf_cpumask_copy(active, online_cpumask);
	scx_bpf_put_cpumask(online_cpumask);

	err = calloc_cpumask(&ovrflw_cpumask);
	if (err)
		goto out;

	err = calloc_cpumask(&turbo_cpumask);
	if (err)
		goto out;

	err = calloc_cpumask(&big_cpumask);
	if (err)
		goto out;

out:
	bpf_rcu_read_unlock();
	return err;
}

static s32 init_per_cpu_ctx(u64 now)
{
	struct cpu_ctx *cpuc;
	struct bpf_cpumask *turbo, *big, *active, *ovrflw, *cd_cpumask;
	const struct cpumask *online_cpumask;
	struct cpdom_ctx *cpdomc;
	int cpu, i, j, k, err = 0;
	u64 cpdom_id;
	u32 sum_capacity = 0, big_capacity = 0;

	bpf_rcu_read_lock();
	online_cpumask = scx_bpf_get_online_cpumask();

	turbo = turbo_cpumask;
	big = big_cpumask;
	active = active_cpumask;
	ovrflw = ovrflw_cpumask;
	if (!turbo || !big || !active || !ovrflw) {
		scx_bpf_error("Failed to prepare cpumasks.");
		err = -ENOMEM;
		goto unlock_out;
	}

	one_little_max_capacity = LAVD_SCALE;

	bpf_for(cpu, 0, nr_cpu_ids) {
		if (cpu >= LAVD_CPU_ID_MAX)
			break;

		cpuc = get_cpu_ctx_id(cpu);
		if (!cpuc) {
			scx_bpf_error("Failed to lookup cpu_ctx: %d", cpu);
			err = -ESRCH;
			goto unlock_out;
		}

		err = calloc_cpumask(&cpuc->tmp_a_mask);
		if (err)
			goto unlock_out;
		err = calloc_cpumask(&cpuc->tmp_o_mask);
		if (err)
			goto unlock_out;
		err = calloc_cpumask(&cpuc->tmp_l_mask);
		if (err)
			goto unlock_out;
		err = calloc_cpumask(&cpuc->tmp_i_mask);
		if (err)
			goto unlock_out;
		err = calloc_cpumask(&cpuc->tmp_t_mask);
		if (err)
			goto unlock_out;
		err = calloc_cpumask(&cpuc->tmp_t2_mask);
		if (err)
			goto unlock_out;
		err = calloc_cpumask(&cpuc->tmp_t3_mask);
		if (err)
			goto unlock_out;

		cpuc->cpu_id = (u16)cpu;
		cpuc->idle_start_clk = 0;
		cpuc->idle_total = 0;
		cpuc->lat_cri = 0;
		cpuc->running_clk = 0;
		cpuc->est_stopping_clk = SCX_SLICE_INF;
		cpuc->online_clk = now;
		cpuc->offline_clk = now;
		cpuc->cpu_release_clk = now;
		cpuc->is_online = bpf_cpumask_test_cpu((u32)cpu, online_cpumask);

		cpuc->max_capacity = cpu_capacity[cpu];
		cpuc->effective_capacity = cpuc->max_capacity;

		cpuc->big_core = cpu_big[cpu];
		cpuc->turbo_core = cpu_turbo[cpu];

		cpuc->min_perf_cri = LAVD_SCALE;
		cpuc->max_perf_cri = 0;
		cpuc->sum_perf_cri = 0;

		cpuc->max_lat_cri = 0;
		cpuc->sum_lat_cri = 0;

		cpuc->nr_sched = 0;
		cpuc->nr_preempt = 0;
		cpuc->nr_x_migration = 0;
		cpuc->nr_perf_cri = 0;
		cpuc->nr_lat_cri = 0;
		cpuc->nr_pinned_tasks = 0;
		cpuc->nr_pinned_waiting = 0;

		cpuc->tot_task_time = 0;
		cpuc->tot_svc_time = 0;
		cpuc->tot_sc_time = 0;

		cpuc->avg_util = 0;
		cpuc->cur_util = 0;
		cpuc->avg_sc_util = 0;
		cpuc->cur_sc_util = 0;

		cpuc->avg_stolen_est = 0;
		cpuc->cur_stolen_est = 0;
		cpuc->stolen_time_est = 0;

		cpuc->effective_capacity = cpuc->max_capacity;
		cpuc->max_freq = LAVD_SCALE;
		cpuc->max_freq_observed = 0;

		cpuc->futex_op = LAVD_FUTEX_OP_INVALID;

		sum_capacity += (u32)cpuc->max_capacity;

		if (cpuc->big_core) {
			nr_cpus_big++;
			big_capacity += (u32)cpuc->max_capacity;
			bpf_cpumask_set_cpu((u32)cpu, big);
		} else {
			have_little_core = true;
		}

		if (cpuc->turbo_core) {
			bpf_cpumask_set_cpu((u32)cpu, turbo);
			have_turbo_core = true;
		}

		if (cpuc->max_capacity < one_little_max_capacity)
			one_little_max_capacity = cpuc->max_capacity;
	}

	if (sum_capacity == 0) {
		err = -EINVAL;
		goto unlock_out;
	}

	default_big_core_scale = ((u64)big_capacity << LAVD_SHIFT) / (u64)sum_capacity;
	total_max_capacity = sum_capacity;

	bpf_for(cpdom_id, 0, nr_cpdoms) {
		if (cpdom_id >= LAVD_CPDOM_MAX_NR)
			break;

		cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);
		cd_cpumask = MEMBER_VPTR(cpdom_cpumask, [cpdom_id]);
		if (!cpdomc || !cd_cpumask) {
			scx_bpf_error("Failed to lookup cpdom_ctx for %llu", cpdom_id);
			err = -ESRCH;
			goto unlock_out;
		}
		if (!cpdomc->is_valid)
			continue;

		bpf_for(i, 0, LAVD_CPU_ID_MAX / 64) {
			u64 cpumask_val = cpdomc->__cpumask[i];

			bpf_for(k, 0, 64) {
				j = cpumask_next_set_bit(&cpumask_val);
				if (j < 0)
					break;

				cpu = (i * 64) + j;
				if (cpu >= LAVD_CPU_ID_MAX || (u32)cpu >= nr_cpu_ids)
					continue;

				cpuc = get_cpu_ctx_id(cpu);
				if (!cpuc) {
					scx_bpf_error("Failed to lookup cpu_ctx: %d", cpu);
					err = -ESRCH;
					goto unlock_out;
				}

				cpuc->llc_id = cpdomc->llc_id;
				cpuc->cpdom_id = (u8)cpdomc->id;
				cpuc->cpdom_alt_id = (u8)cpdomc->alt_id;

				if (bpf_cpumask_test_cpu((u32)cpu, online_cpumask)) {
					bpf_cpumask_set_cpu((u32)cpu, cd_cpumask);
					cpdomc->nr_active_cpus++;
					cpdomc->cap_sum_active_cpus += (u32)cpuc->effective_capacity;
				}
			}
		}
	}

unlock_out:
	scx_bpf_put_cpumask(online_cpumask);
	bpf_rcu_read_unlock();
	return err;
}

static int init_per_cpu_dsqs(void)
{
	struct cpdom_ctx *cpdomc;
	struct cpu_ctx *cpuc;
	int cpu, err;

	bpf_for(cpu, 0, nr_cpu_ids) {
		if (cpu >= LAVD_CPU_ID_MAX)
			break;

		cpuc = get_cpu_ctx_id(cpu);
		if (!cpuc) {
			scx_bpf_error("Failed to lookup cpu_ctx: %d", cpu);
			return -ESRCH;
		}

		if (cpuc->cpdom_id >= LAVD_CPDOM_MAX_NR) {
			scx_bpf_error("Invalid cpdom_id %hhu for cpu %d", cpuc->cpdom_id, cpu);
			return -EINVAL;
		}

		cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpuc->cpdom_id]);
		if (!cpdomc) {
			scx_bpf_error("Failed to lookup cpdom_ctx for %hhu", cpuc->cpdom_id);
			return -ESRCH;
		}

		if (is_smt_active && (cpu != get_primary_cpu(cpu)))
			continue;

		err = scx_bpf_create_dsq(cpu_to_dsq((u32)cpu), cpdomc->numa_id);
		if (err) {
			scx_bpf_error("Failed to create DSQ for cpu %d", cpu);
			return err;
		}
	}

	return 0;
}

/* ================================================================== */
/*                    Cgroup Support                                  */
/* ================================================================== */

s32 BPF_STRUCT_OPS_SLEEPABLE(lavd_cgroup_init, struct cgroup *cgrp,
			     struct scx_cgroup_init_args *args)
{
	int ret;

	if (!enable_cpu_bw)
		return 0;

	if (unlikely(!cgrp))
		return -EINVAL;

	ret = scx_cgroup_bw_init(cgrp, args);
	if (ret)
		scx_bpf_error("Failed to init a cgroup: %d", ret);
	return ret;
}

void BPF_STRUCT_OPS(lavd_cgroup_exit, struct cgroup *cgrp)
{
	int ret;

	if (!enable_cpu_bw)
		return;

	if (unlikely(!cgrp))
		return;

	ret = scx_cgroup_bw_exit(cgrp);
	if (ret)
		scx_bpf_error("Failed to exit a cgroup: %d", ret);
}

void BPF_STRUCT_OPS(lavd_cgroup_move, struct task_struct *p,
		    struct cgroup *from, struct cgroup *to)
{
	task_ctx *taskc;

	(void)from;

	if (unlikely(!p))
		return;

	taskc = get_task_ctx(p);
	if (!taskc) {
		scx_bpf_error("Failed to get a task context: %d", p->pid);
		return;
	}

	if (to && to->kn)
		taskc->cgrp_id = to->kn->id;
	else
		taskc->cgrp_id = 0;
}

void BPF_STRUCT_OPS(lavd_cgroup_set_bandwidth, struct cgroup *cgrp,
		    u64 period_us, u64 quota_us, u64 burst_us)
{
	int ret;

	if (!enable_cpu_bw)
		return;

	if (unlikely(!cgrp))
		return;

	ret = scx_cgroup_bw_set(cgrp, period_us, quota_us, burst_us);
	if (ret)
		scx_bpf_error("Failed to set bandwidth of a cgroup: %d", ret);
}

int lavd_enqueue_cb(u64 ctx)
{
	task_ctx *taskc = (task_ctx *)ctx;
	struct task_struct *p;

	if (!enable_cpu_bw)
		return 0;

	if (unlikely(!taskc))
		return 0;

	if ((p = bpf_task_from_pid(taskc->pid))) {
		enqueue_cb(p);
		bpf_task_release(p);
	}

	return 0;
}
REGISTER_SCX_CGROUP_BW_ENQUEUE_CB(lavd_enqueue_cb);

/* ================================================================== */
/*                    Main Init / Exit                                */
/* ================================================================== */

s32 BPF_STRUCT_OPS_SLEEPABLE(lavd_init)
{
	u64 now = scx_bpf_now();
	int err;

	err = init_cpdoms(now);
	if (err)
		return err;

	err = init_cpumasks();
	if (err)
		return err;

	err = init_per_cpu_ctx(now);
	if (err)
		return err;

	if (use_per_cpu_dsq()) {
		err = init_per_cpu_dsqs();
		if (err)
			return err;
	}

	err = init_sys_stat(now);
	if (err)
		return err;

	init_autopilot_caps();

	WRITE_ONCE(cur_logical_clk, 0);
	WRITE_ONCE(cur_svc_time, 0);

	if (enable_cpu_bw) {
		struct scx_cgroup_bw_config bw_config = {
			.verbose = verbose > 2,
		};

		err = scx_cgroup_bw_lib_init(&bw_config);
		if (err)
			return err;
	}

	lavd_pid = (u32)bpf_get_current_pid_tgid();

	return 0;
}

void BPF_STRUCT_OPS(lavd_exit, struct scx_exit_info *ei)
{
	UEI_RECORD(uei, ei);
}

SCX_OPS_DEFINE(lavd_ops,
	       .select_cpu		= (void *)lavd_select_cpu,
	       .enqueue			= (void *)lavd_enqueue,
	       .dequeue			= (void *)lavd_dequeue,
	       .dispatch		= (void *)lavd_dispatch,
	       .runnable		= (void *)lavd_runnable,
	       .running			= (void *)lavd_running,
	       .tick			= (void *)lavd_tick,
	       .stopping		= (void *)lavd_stopping,
	       .quiescent		= (void *)lavd_quiescent,
	       .cpu_online		= (void *)lavd_cpu_online,
	       .cpu_offline		= (void *)lavd_cpu_offline,
	       .update_idle		= (void *)lavd_update_idle,
	       .set_cpumask		= (void *)lavd_set_cpumask,
	       .cpu_acquire		= (void *)lavd_cpu_acquire,
	       .cpu_release		= (void *)lavd_cpu_release,
	       .enable			= (void *)lavd_enable,
	       .init_task		= (void *)lavd_init_task,
	       .exit_task		= (void *)lavd_exit_task,
	       .cgroup_init		= (void *)lavd_cgroup_init,
	       .cgroup_exit		= (void *)lavd_cgroup_exit,
	       .cgroup_move		= (void *)lavd_cgroup_move,
	       .cgroup_set_bandwidth	= (void *)lavd_cgroup_set_bandwidth,
	       .init			= (void *)lavd_init,
	       .exit			= (void *)lavd_exit,
	       .timeout_ms		= 30000U,
	       .name			= "lavd");
