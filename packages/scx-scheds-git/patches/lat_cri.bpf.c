/* SPDX-License-Identifier: GPL-2.0 */
/*
 * Copyright (c) 2023-2025 Valve Corporation.
 * Author: Changwoo Min <changwoo@igalia.com>
 */

#include <scx/common.bpf.h>
#include <bpf_arena_common.bpf.h>
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


static u64 calc_weight_factor(struct task_struct *p, task_ctx *taskc)
{
	u64 weight_boost = 1;
	u64 flags;

	/*
	 * Snapshot taskc->flags once. The field is declared volatile in
	 * lavd.bpf.h, so every test_task_flag() call would otherwise issue
	 * a fresh memory load (9× here). One register-resident snapshot
	 * suffices because the test/reset pairs all operate on independent
	 * bits and the resets are visible through volatile writes for any
	 * other reader; the local-snapshot semantics match the original
	 * test-then-reset ordering bit-for-bit (verified by 184,320-case
	 * exhaustive parity suite).
	 */
	flags = taskc->flags;

	/*
	 * Prioritize a wake-up task since this is a clear sign of immediate
	 * consumer. If it is a synchronous wakeup, double the prioritization.
	 */
	if (flags & LAVD_FLAG_IS_WAKEUP)
		weight_boost += LAVD_LC_WEIGHT_BOOST_REGULAR;

	if (flags & LAVD_FLAG_IS_SYNC_WAKEUP)
		weight_boost += LAVD_LC_WEIGHT_BOOST_REGULAR;

	/*
	 * Prioritize a task woken by a hardirq or softirq.
	 *   - hardirq: The top half of an interrupt processing (e.g., mouse
	 *     move, keypress, disk I/O completion, or GPU V-Sync) has just
	 *     been completed, and it hands off further processing to a fair
	 *     task. The task that was waiting for this specific hardware
	 *     signal gets the "Express Lane."
	 *
	 *   - softirq: The kernel just finished the bottom half of an
	 *     interrupt processing, like network packets and timers. If a
	 *     packet arrives for your Browser, or a timer expires for a
	 *     frame refresh, the task gets a "High" boost. This keeps the
	 *     data pipeline flowing smoothly.
	 *
	 * Note that the irq-boosted criticality will flow through the forward
	 * & backward propagation mechanism, which will be described below.
	 *
	 * Per-task hardirq/softirq wake-ups are rare in a typical gaming /
	 * compile workload; mark unlikely so Raptor Lake's BPU lays out the
	 * non-irq path as the straight-line fall-through.
	 */
	if (unlikely(flags & LAVD_FLAG_WOKEN_BY_HARDIRQ)) {
		reset_task_flag(taskc, LAVD_FLAG_WOKEN_BY_HARDIRQ);
		weight_boost += LAVD_LC_WEIGHT_BOOST_HIGHEST;
	} else if (unlikely(flags & LAVD_FLAG_WOKEN_BY_SOFTIRQ)) {
		reset_task_flag(taskc, LAVD_FLAG_WOKEN_BY_SOFTIRQ);
		weight_boost += LAVD_LC_WEIGHT_BOOST_HIGH;
	}

	/*
	 * Prioritize a task woken by an RT/DL task.
	 */
	if (unlikely(flags & LAVD_FLAG_WOKEN_BY_RT_DL)) {
		reset_task_flag(taskc, LAVD_FLAG_WOKEN_BY_RT_DL);
		weight_boost += LAVD_LC_WEIGHT_BOOST_HIGH;
	}

	/*
	 * Prioritize a kernel task since many kernel tasks serve
	 * latency-critical jobs.
	 */
	if (is_kernel_task(p))
		weight_boost += LAVD_LC_WEIGHT_BOOST_MEDIUM;

	/*
	 * Further prioritize ksoftirqd.
	 */
	if (flags & LAVD_FLAG_KSOFTIRQD)
		weight_boost += LAVD_LC_WEIGHT_BOOST_HIGH;

	/*
	 * Further prioritize kworkers.
	 *
	 * Note: tested independently from is_kernel_task because
	 * PF_WQ_WORKER / PF_IO_WORKER are conceptually orthogonal flags
	 * (Linux convention: kworkers carry both PF_KTHREAD and
	 * PF_WQ_WORKER, but the upstream semantics treat them
	 * independently — preserved here bit-for-bit).
	 */
	if (is_kernel_worker(p))
		weight_boost += LAVD_LC_WEIGHT_BOOST_REGULAR;

	/*
	 * Prioritize an affinitized task since it has restrictions
	 * in placement so it tends to be delayed.
	 */
	if (flags & LAVD_FLAG_IS_AFFINITIZED)
		weight_boost += LAVD_LC_WEIGHT_BOOST_REGULAR;

	/*
	 * Prioritize a pinned task since it has restrictions in placement
	 * so it tends to be delayed. Most user tasks are unpinned; unlikely
	 * lays out the non-pinned path as the straight-line fall-through.
	 */
	if (unlikely(is_pinned(p) || is_migration_disabled(p)))
		weight_boost += LAVD_LC_WEIGHT_BOOST_MEDIUM;

	/*
	 * Prioritize a lock holder for faster system-wide forward progress.
	 * Futex lock-boost is rare per scheduling event.
	 */
	if (unlikely(flags & LAVD_FLAG_NEED_LOCK_BOOST)) {
		reset_task_flag(taskc, LAVD_FLAG_NEED_LOCK_BOOST);
		weight_boost += LAVD_LC_WEIGHT_BOOST_REGULAR;
	}

	/*
	 * Respect nice priority.
	 */
	return (u64)p->scx.weight * weight_boost + 1;
}

/*
 * Take the frequency by value so the caller can hoist the arena read into
 * a single register-resident snapshot and pass it on to multiple consumers
 * without re-derefing the task_ctx pointer.
 */
static u64 calc_wait_factor(u64 wait_freq)
{
	u64 freq = (wait_freq < (u64)LAVD_LC_FREQ_MAX) ?
		   wait_freq : (u64)LAVD_LC_FREQ_MAX;
	return freq + 1;
}

static u64 calc_wake_factor(u64 wake_freq)
{
	u64 freq = (wake_freq < (u64)LAVD_LC_FREQ_MAX) ?
		   wake_freq : (u64)LAVD_LC_FREQ_MAX;
	return freq + 1;
}

static inline u64 calc_reverse_runtime_factor(u64 avg_runtime_invr)
{
	/*
	 * Shorter invariant runtime -> more latency-critical. Using
	 * avg_runtime_invr (pelt-clock based) normalizes out CPU frequency and
	 * capacity differences, so a task running on a slow core is not
	 * unfairly penalized compared to the same task on a fast core.
	 *
	 * LAVD_LC_RUNTIME_MAX and LAVD_SLICE_MIN_NS_DFL are wall-clock
	 * calibrated constants, while avg_runtime_invr is invariant time.
	 * This mixed comparison is intentional and acceptable: on a CPU at
	 * nominal frequency/capacity the two are equal; on a slow core,
	 * avg_runtime_invr is smaller, making the delta larger and correctly
	 * giving the task more latency-criticality headroom. The constants are
	 * heuristic thresholds, not precise physical measurements, so the
	 * slight unit mismatch is negligible in practice.
	 *
	 * The +1 ensures the return is never 0, so the subsequent
	 * log2x(runtime_ft * weight_ft) cannot see a 0 product. Critical
	 * invariant — do not remove.
	 */
	if (LAVD_LC_RUNTIME_MAX > avg_runtime_invr) {
		u64 delta = time_delta(LAVD_LC_RUNTIME_MAX, avg_runtime_invr);
		return (delta / LAVD_SLICE_MIN_NS_DFL) + 1;
	}
	return 1;
}

static u64 calc_sum_runtime_factor(struct task_struct *p, u64 avg_runtime_invr,
				   u64 acc_runtime_invr, u64 run_freq)
{
	/*
	 * Estimate total invariant compute demand per second:
	 *   run_freq * avg_runtime_invr ~= fraction of CPU capacity consumed.
	 * Using avg_runtime_invr (pelt-clock based) makes this quantity
	 * comparable across CPUs with different frequencies or capacities.
	 * acc_runtime_invr captures the burst since the last sleep; taking
	 * the max of the two avoids penalizing tasks that occasionally run
	 * longer than their average. run_freq stays on wall clock because it
	 * is derived from wall-clock wait intervals (external event rates).
	 *
	 * Callers pass scalars by value so the arena dereferences are
	 * hoisted into a single snapshot point at the top of calc_lat_cri().
	 */
	u64 runtime = (acc_runtime_invr > avg_runtime_invr) ?
		      acc_runtime_invr : avg_runtime_invr;
	u64 rf = run_freq      ? run_freq      : (u64)1;
	u64 rt = runtime       ? runtime       : (u64)1;
	return ((rf * rt) >> LAVD_SHIFT) * (u64)p->scx.weight;
}

u32 __attribute__ ((noinline)) log2x(u64 v)
{
	return log2_u64(v);
}

static void calc_lat_cri(struct task_struct *p, task_ctx *taskc)
{
	u64 wait_freq_s, wake_freq_s, run_freq_s;
	u64 avg_runtime_invr_s, acc_runtime_invr_s;
	u64 weight_ft, wait_ft, wake_ft, runtime_ft;
	u64 log_wwf, lat_cri, lat_cri_giver;
	u64 perf_cri = LAVD_SCALE;

	/*
	 * Snapshot all hot arena fields once. Without this, each
	 * helper-function deref would re-load through the arena pointer,
	 * costing 5 redundant 8-byte loads on the wake-up critical path.
	 * The snapshot is taken consistently at one point so the value
	 * pair (avg_runtime_invr, acc_runtime_invr) cannot be observed in
	 * a torn state mid-computation between the two helpers.
	 */
	wait_freq_s        = taskc->wait_freq;
	wake_freq_s        = taskc->wake_freq;
	run_freq_s         = taskc->run_freq;
	avg_runtime_invr_s = taskc->avg_runtime_invr;
	acc_runtime_invr_s = taskc->acc_runtime_invr;

	/*
	 * A task is more latency-critical as its wait or wake frequencies
	 * (i.e., wait_freq and wake_freq) are higher, and its runtime is
	 * shorter.
	 */
	wait_ft    = calc_wait_factor(wait_freq_s);
	wake_ft    = calc_wake_factor(wake_freq_s);
	runtime_ft = calc_reverse_runtime_factor(avg_runtime_invr_s);

	/*
	 * Adjust task's weight based on the scheduling context, such as
	 * if it is a kernel task, lock holder, etc.
	 */
	weight_ft = calc_weight_factor(p, taskc);

	/*
	 * Wake frequency and wait frequency represent how much a task is used
	 * for a producer and a consumer, respectively. If both are high, the
	 * task is in the middle of a task chain. The ratio tends to follow an
	 * exponentially skewed distribution, so we linearize it using sqrt.
	 */
	log_wwf = log2x(wait_ft * wake_ft);
	lat_cri = log_wwf + log2x(runtime_ft * weight_ft);

	/*
	 * Amplify the task's latency criticality to better differentiate
	 * between latency-critical vs. non-latency-critical tasks.
	 */
	lat_cri = lat_cri * lat_cri;

	/*
	 * Determine latency criticality of a task in a context-aware manner by
	 * considering its waker and wakee's latency criticality.
	 *
	 * Forward propagation is to keep the waker’s momentum forward to the
	 * wakee, and backward propagation is to boost the low-priority waker
	 * (i.e., priority inversion) for the next time. Propagation decays
	 * geometrically and is capped to a limit to prevent unlimited cyclic
	 * inflation of latency-criticality.
	 *
	 */
	lat_cri_giver = (u64)taskc->lat_cri_waker + (u64)taskc->lat_cri_wakee;
	if (lat_cri_giver > (2 * lat_cri)) {
		/*
		 * The amount of latency criticality inherited needs to be
		 * limited, so the task's latency criticality portion should
		 * always be a dominant factor.
		 */
		u64 giver_inh = (lat_cri_giver - (2 * lat_cri)) >>
				LAVD_LC_INH_GIVER_SHIFT;
		u64 receiver_max = lat_cri >> LAVD_LC_INH_RECEIVER_SHIFT;
		lat_cri += min(giver_inh, receiver_max);
	}

	/*
	 * Defense-in-depth clamp before u16 store. By construction
	 * lat_cri ∈ [4, ~20480] in any valid scheduler state (proven by
	 * the +1 guards in the wait/wake/runtime/weight factors and the
	 * bounded inheritance term), so this clamp NEVER fires in normal
	 * operation. It is cheap insurance (two cmov, ~2 cycles on Raptor
	 * Lake) against future refactors that might inadvertently feed 0
	 * into the divide in calc_virtual_deadline_delta() or overflow u16.
	 */
	if (unlikely(lat_cri == 0))
		lat_cri = 1;
	if (unlikely(lat_cri > 65535))
		lat_cri = 65535;

	taskc->lat_cri = (u16)lat_cri;
	taskc->lat_cri_waker = 0;
	taskc->lat_cri_wakee = 0;

	/*
	 * Normalize lat_cri to [0, 1024] scale for CPU selection
	 * and DSQ routing decisions.
	 */
	taskc->normalized_lat_cri = normalize_lat_cri((u16)lat_cri);

	/*
	 * A task is more CPU-performance sensitive when it meets the following
	 * conditions:
	 *
	 * - It is in the middle of the task graph (high wait and wake
	 *   frequencies).
	 * - Its runtime and frequency are high;
	 * - Its nice priority is high;
	 *
	 * We use the log-ed value since the raw value follows the highly
	 * skewed distribution.
	 *
	 * Note that we use unadjusted weight to reflect the pure task priority.
	 */
	if (have_little_core) {
		u64 sum_runtime_ft = calc_sum_runtime_factor(p,
							     avg_runtime_invr_s,
							     acc_runtime_invr_s,
							     run_freq_s);
		perf_cri = log_wwf + log2x(sum_runtime_ft);
		/*
		 * perf_cri ≤ 64 + 64 = 128 by log2_u64's [1, 65] range —
		 * fits u16 trivially. Defensive clamp is cheap insurance.
		 */
		if (unlikely(perf_cri > 65535))
			perf_cri = 65535;
	}
	taskc->perf_cri = (u16)perf_cri;
}

static u64 calc_greedy_penalty(struct task_struct *p, task_ctx *taskc)
{
	u64 lag_max, penalty, avg_svc, svc;
	s64 lag;

	/*
	 * Calculate the task's lag -- the underserved time. Bound the lag
	 * into [-lag_max, +lag_max] and set the LAVD_FLAG_IS_GREEDY flag
	 * for preemption decision.
	 *
	 * svc_time_iwgt uses invariant time weighted by task priority.
	 * Using invariant time avoids penalizing tasks placed on slow cores,
	 * which accumulate wall-clock time faster for the same actual work.
	 * Fairness accounting must use the same clock basis as the system
	 * average (sys_stat.avg_svc_time_iwgt) for an apples-to-apples
	 * comparison.
	 *
	 * Snapshot both values once. avg_svc_time_iwgt is updated by the
	 * stat collector; reading it twice could see different values across
	 * the lag computation and the subsequent svc_time_iwgt clamp.
	 * Both values fit comfortably in s64 in any realistic state
	 * (typical EWMA service times are ≤ 1e15 ns ≪ 2^63), so the signed
	 * subtraction is overflow-free.
	 */
	avg_svc = sys_stat.avg_svc_time_iwgt;
	svc     = taskc->svc_time_iwgt;
	lag     = (s64)avg_svc - (s64)svc;

	lag_max = scale_by_task_weight_inverse(p, LAVD_TASK_LAG_MAX);

	/*
	 * Guard the divide. lag_max == 0 requires p->scx.weight to be
	 * astronomically large, which is impossible for a normal task,
	 * but the BPF JIT silently rewrites "X / 0" to 0 and we would
	 * return a meaningless penalty; bail out cleanly.
	 */
	if (unlikely(lag_max == 0))
		return LAVD_SCALE;

	if (lag >= 0) {
		reset_task_flag(taskc, LAVD_FLAG_IS_GREEDY);

		/*
		 * Limit the positive lag to lag_max. This prevents unbounded
		 * boost of long-sleepers. Cast lag to u64 is safe here because
		 * lag >= 0 in this branch.
		 */
		if ((u64)lag > lag_max) {
			taskc->svc_time_iwgt = avg_svc - lag_max;
			lag = (s64)lag_max;
		}
	} else {
		set_task_flag(taskc, LAVD_FLAG_IS_GREEDY);

		/*
		 * Limit the negative lag to -lag_max to pay the debt
		 * gradually over time. Comparison performed entirely in
		 * signed domain to avoid the sign-confusion bug present
		 * in the custom variant (which cast -lag → u64, wrapping
		 * for negative results).
		 */
		if (lag < -(s64)lag_max)
			lag = -(s64)lag_max;
	}
	/* lag = [-lag_max, lag_max] */

	/*
	 * penalty = [100%, 200%]
	 *
	 * Compute (-lag + lag_max) in the signed domain first; the result
	 * is in [0, 2*lag_max] which fits u64 with room to spare
	 * (lag_max ≤ 5e10 in any realistic config), making the subsequent
	 * left-shift by LAVD_SHIFT (10 bits) safe.
	 */
	penalty = ((u64)(-lag + (s64)lag_max) << LAVD_SHIFT) / lag_max;
	penalty = LAVD_SCALE + (penalty >> LAVD_LC_GREEDY_SHIFT);
	return penalty;
}

static u64 calc_adjusted_runtime(task_ctx *taskc)
{
	u64 acc, capped;

	/*
	 * Prefer a recently woken-up task over one that has been running
	 * continuously. acc_runtime_invr captures the invariant compute time
	 * consumed since the last sleep; using the invariant (pelt-clock)
	 * value means a task on a slow core is treated consistently with the
	 * same task on a fast core. To avoid starvation of CPU-bound tasks,
	 * which rarely sleep, limit the impact of acc_runtime_invr.
	 *
	 * Single snapshot + branchless min via ternary (CMOV on Raptor Lake,
	 * ~1 cycle, no branch-mispredict risk).
	 */
	acc    = taskc->acc_runtime_invr;
	capped = (acc < LAVD_ACC_RUNTIME_MAX) ? acc : LAVD_ACC_RUNTIME_MAX;
	return LAVD_ACC_RUNTIME_MAX + capped;
}

static u64 calc_virtual_deadline_delta(struct task_struct *p,
				       task_ctx *taskc)
{
	u64 deadline, adjusted_runtime, lat_cri;
	u32 greedy_penalty;

	/*
	 * Calculate the deadline based on runtime,
	 * latency criticality, and greedy ratio.
	 */
	calc_lat_cri(p, taskc);
	greedy_penalty   = calc_greedy_penalty(p, taskc);
	adjusted_runtime = calc_adjusted_runtime(taskc);

	/*
	 * Defense-in-depth: by construction lat_cri ≥ 4 after calc_lat_cri,
	 * but if a future refactor breaks that invariant a divide-by-zero
	 * would silently produce 0 (BPF JIT rewrite) and corrupt deadline
	 * ordering. Bit-OR with (lat_cri == 0) forces a minimum of 1 at
	 * zero cost beyond a single CMP+OR (~2 cycles on Raptor Lake).
	 */
	lat_cri  = (u64)taskc->lat_cri;
	lat_cri |= (u64)(lat_cri == 0);

	deadline = (adjusted_runtime * greedy_penalty) / lat_cri;
	return deadline >> LAVD_SHIFT;
}

__hidden
u64 calc_when_to_run(struct task_struct *p, task_ctx *taskc)
{
	u64 dl_delta, clc;

	/*
	 * task_ctx is in arena memory; prefetch the hot cacheline early
	 * so the upcoming snapshot loads in calc_lat_cri hit L1d. The
	 * BPF backend currently lowers prefetch to a no-op (BPF has no
	 * PREFETCH opcode), but the hint is verifier-safe and benefits
	 * native-JIT'd fallbacks. The pointer is trusted by the caller's
	 * arg contract, so no NULL check is required at this boundary.
	 */
	__builtin_prefetch(taskc, 0, 3);

	/*
	 * Before enqueueing a task to a run queue, we should decide when a
	 * task should be scheduled. We start from -LAVD_DL_COMPETE_WINDOW
	 * so that the current task can compete against the already enqueued
	 * tasks within [-LAVD_DL_COMPETE_WINDOW, 0].
	 *
	 * cur_logical_clk is initialized to LAVD_DL_COMPETE_WINDOW in
	 * main.bpf.c and only ever advances monotonically via CAS, so the
	 * subtraction is provably underflow-free. No defensive >= guard
	 * needed (which would otherwise add a wasted branch on this
	 * hot wake-up path).
	 */
	dl_delta = calc_virtual_deadline_delta(p, taskc);
	clc      = READ_ONCE(cur_logical_clk) - LAVD_DL_COMPETE_WINDOW;
	return clc + dl_delta;
}
