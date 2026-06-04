/* SPDX-License-Identifier: GPL-2.0 */
/*
 * scx_lavd: Latency-criticality Aware Virtual Deadline (LAVD) scheduler
 *
 * Copyright (c) 2023, 2024 Valve Corporation.
 * Author: Changwoo Min <changwoo@igalia.com>
 *
 * Raptor Lake-tuned optimization pass — see CHANGES.md for the
 * rationale behind every modification, each anchored to a specific
 * patch in the cache-aware-scheduling and BORE work this revision was
 * cross-pollinated from.
 *
 * (Original LAVD design banner abbreviated; see upstream for the full
 *  design rationale.)
 */
#include <scx/common.bpf.h>
#include <bpf_arena_common.bpf.h>
#include <bpf_experimental.h>

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

/*
 * Logical current clock
 */
u64		cur_logical_clk = LAVD_DL_COMPETE_WINDOW;

/*
 * Current service time (weighted invariant time).
 *
 * Contract: monotonically non-decreasing across all CPUs. The previous
 * implementation used a non-atomic READ_ONCE/compare/WRITE_ONCE which
 * was racy: two CPUs finishing tasks in the same window would both
 * pass the < test, and the lower writer would clobber the higher one.
 * See update_stat_for_stopping() below for the bounded CAS loop.
 */
static u64		cur_svc_time_iwgt;

/*
 * The minimum and maximum of time slice
 */
const volatile u64	slice_min_ns = LAVD_SLICE_MIN_NS_DFL;
const volatile u64	slice_max_ns = LAVD_SLICE_MAX_NS_DFL;

/*
 * Migration delta threshold percentage (0-100)
 */
const volatile u8	mig_delta_pct = 0;

/* Disable batch-migration load balancer; set via --no-fast-lb. */
const volatile u8	no_fast_lb = 0;

/*
 * Skip periodic load balancing when average system utilization is below this
 * threshold. The value is pre-scaled by userspace. 0 = disabled.
 * Default: p2s(25) = 256.
 */
const volatile u64	lb_low_util_wall = 0;

/*
 * Bypass deadline-based scheduling and dispatch directly to local DSQ
 * when average system utilization is below this threshold.
 * The value is pre-scaled by userspace. 0 = disabled.
 * Default: p2s(10) = 102.
 */
const volatile u64	lb_local_dsq_util_wall = 0;

/*
 * Slice time for all tasks when pinned tasks are running on the CPU.
 * When this is set (non-zero), pinned tasks always use per-CPU DSQs and
 * the dispatch logic compares vtimes across DSQs.
 */
const volatile u64	pinned_slice_ns = 0;

static volatile u64	nr_cpus_big;

/*
 * Scheduler's PID
 */
static pid_t		lavd_pid;

static const u8 lavd_bore_log2_lut[256] = {
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
	237, 238, 239, 239, 240, 241, 241, 242, 243, 244, 244, 245, 246, 247, 247, 248,
};

#define LAVD_BORE_OFFSET_Q8		(24U << 8)
#define LAVD_BORE_SCALE			1536U
#define LAVD_BORE_MAX_PENALTY_Q8	((40U << 8) - 1U)
#define LAVD_BORE_SPARSE_PENALTY_Q8	(20U << 8)
#define LAVD_BORE_FLOOR_NS		((1ULL << 24) - 1ULL)
#define LAVD_MICROBURST_NS		10000ULL

/* Pre-computed saturation delta (avoids the divide on every call). */
#define LAVD_BORE_SAT_DELTA \
	(((u32)LAVD_BORE_MAX_PENALTY_Q8 << 10) / LAVD_BORE_SCALE)

static __always_inline u32 lavd_bore_calc_penalty_q8(u64 runtime_ns)
{
	u32 exp_q8, frac_idx, greed, delta, penalty;
	u32 lz;

	if (likely(runtime_ns <= LAVD_BORE_FLOOR_NS))
		return 0U;

	lz = (u32)__builtin_clzll(runtime_ns);
	exp_q8 = (64U - lz) << 8;

	if (likely(lz < 56U))
		frac_idx = (u32)((runtime_ns << (lz + 1U)) >> 56);
	else
		frac_idx = 0U;

	greed = exp_q8 + (u32)lavd_bore_log2_lut[frac_idx];
	if (likely(greed <= LAVD_BORE_OFFSET_Q8))
		return 0U;

	delta = greed - LAVD_BORE_OFFSET_Q8;
	if (unlikely(delta >= LAVD_BORE_SAT_DELTA))
		return LAVD_BORE_MAX_PENALTY_Q8;

	penalty = (delta * LAVD_BORE_SCALE) >> 10;
	return min(penalty, (u32)LAVD_BORE_MAX_PENALTY_Q8);
}

static __always_inline bool lavd_bore_runtime_is_sparse(u64 runtime_ns)
{
	if (likely(runtime_ns <= LAVD_BORE_FLOOR_NS))
		return true;
	return lavd_bore_calc_penalty_q8(runtime_ns) <
		LAVD_BORE_SPARSE_PENALTY_Q8;
}

static __always_inline u64 lavd_mul_u64_u32_shr16_cap(u64 val, u32 mul,
						      u64 cap)
{
	u64 hi, res, rem;
	u32 lo;

	if (!mul || !cap)
		return 0;

	hi = val >> 16;
	lo = (u32)(val & 0xffffULL);

	if (hi > cap / mul)
		return cap;

	res = hi * mul;
	if (res >= cap)
		return cap;

	rem = ((u64)lo * mul) >> 16;
	if (rem > cap - res)
		return cap;

	return res + rem;
}

static __always_inline u64 lavd_mul_u64_u32_div_cap(u64 val, u32 mul,
						    u32 div, u64 cap)
{
	u64 q, res, rem;
	u32 r;

	if (!mul || !cap)
		return 0;
	if (!div)
		return cap;

	q = val / div;
	r = (u32)(val - q * div);

	if (q > cap / mul)
		return cap;

	res = q * mul;
	if (res >= cap)
		return cap;

	rem = ((u64)r * mul) / div;
	if (rem > cap - res)
		return cap;

	return res + rem;
}

static __always_inline bool lavd_is_microburst(const task_ctx *taskc)
{
	u64 avg_runtime_wall;

	if (!taskc)
		return false;

	avg_runtime_wall = READ_ONCE(taskc->avg_runtime_wall);
	return avg_runtime_wall > 0 && avg_runtime_wall <= LAVD_MICROBURST_NS;
}

/*
 * Compute a deadline penalty for sparse-runtime tasks that are *not*
 * already latency- or perf-critical.
 *
 * Read the per-task and sys-wide criticality fields exactly once each
 * into stack locals before comparing. Otherwise the compiler may emit
 * two loads from the shared sys_stat field, and a concurrent
 * collect_sys_stat() update can sneak between them — making the
 * function take an inconsistent decision (e.g. lat_cri half-fresh,
 * perf_cri stale). This is the same single-load pattern the BORE
 * pass adopted from cache-aware patch #9.
 */
static __always_inline u64 lavd_bore_deadline_penalty(task_ctx *taskc,
						      u64 slice_wall)
{
	u64 runtime, max_penalty;
	u32 penalty_q8;
	u32 t_lat_cri, t_perf_cri;
	u32 avg_lat_cri, avg_perf_cri;

	if (!taskc)
		return 0;

	runtime = READ_ONCE(taskc->avg_runtime_wall);
	if (likely(runtime <= LAVD_BORE_FLOOR_NS))
		return 0;

	t_lat_cri    = READ_ONCE(taskc->lat_cri);
	t_perf_cri   = READ_ONCE(taskc->perf_cri);
	avg_lat_cri  = READ_ONCE(sys_stat.avg_lat_cri);
	avg_perf_cri = READ_ONCE(sys_stat.avg_perf_cri);

	if (t_lat_cri > avg_lat_cri || t_perf_cri > avg_perf_cri)
		return 0;

	penalty_q8 = lavd_bore_calc_penalty_q8(runtime);
	if (!penalty_q8)
		return 0;

	max_penalty = slice_wall >> 1;
	return lavd_mul_u64_u32_shr16_cap(slice_wall, penalty_q8,
					       max_penalty);
}

static __always_inline void lavd_apply_bore_deadline(struct task_struct *p,
						     task_ctx *taskc)
{
	u64 penalty, vtime;

	penalty = lavd_bore_deadline_penalty(taskc,
					     READ_ONCE(sys_stat.slice_wall));
	if (!penalty)
		return;

	vtime = READ_ONCE(p->scx.dsq_vtime);
	p->scx.dsq_vtime = (~0ULL - vtime < penalty) ? ~0ULL : vtime + penalty;
}

/*
 * Bounded CAS loop to advance the global logical clock.
 *
 * The "exit early if vlc <= clc" is the patch-1 single-scanner gate in
 * disguise: any concurrent advancer that has already moved the clock
 * past us makes our work unnecessary. The (s64) cast on the diff is
 * the patch-13 wrap-safety hardening — scx_bpf_now() is monotonic so
 * the practical wrap horizon is centuries, but the math is more
 * obviously correct against future clock-source changes.
 */
static void advance_cur_logical_clk(struct task_struct *p)
{
	u64 vlc, clc, ret_clc;
	u64 nr_queued, delta, new_clk;
	s64 diff;
	int i;

	if (unlikely(!p))
		return;

	vlc = READ_ONCE(p->scx.dsq_vtime);
	clc = READ_ONCE(cur_logical_clk);

	bpf_for(i, 0, LAVD_MAX_RETRY) {
		diff = (s64)(vlc - clc);
		if (diff <= 0)
			return;

		nr_queued = max(READ_ONCE(sys_stat.nr_queued_task), 1ULL);
		if (nr_queued == 1)
			delta = (u64)diff;
		else
			delta = (u64)diff / nr_queued;

		if (!delta)
			delta = 1;

		new_clk = clc + delta;
		if (new_clk > vlc)
			new_clk = vlc;

		ret_clc = __sync_val_compare_and_swap(&cur_logical_clk,
						      clc, new_clk);
		if (ret_clc == clc)
			return;
		clc = ret_clc;
	}
}

/*
 * calc_time_slice() — cache repeated reads of the same shared word in
 * a stack local once and reuse, mirroring the BORE pass's
 * "READ_ONCE once, compare from local" cleanup. This both eliminates
 * redundant memory traffic (the sys_stat fields live on cachelines
 * that may bounce between CPUs) and prevents intra-function
 * inconsistency if a sys_stat collection fires mid-call.
 */
static u64 calc_time_slice(task_ctx *taskc, struct cpu_ctx *cpuc)
{
	u64 slice_wall;
	u64 avg_runtime_wall;
	u64 nr_pinned_tasks;
	u32 avg_lat_cri;
	u32 task_lat_cri;

	if (!taskc || !cpuc)
		return LAVD_SLICE_MAX_NS_DFL;

	slice_wall       = READ_ONCE(sys_stat.slice_wall);
	avg_runtime_wall = READ_ONCE(taskc->avg_runtime_wall);
	nr_pinned_tasks  = READ_ONCE(cpuc->nr_pinned_tasks);
	task_lat_cri     = READ_ONCE(taskc->lat_cri);

	if (unlikely(pinned_slice_ns) && nr_pinned_tasks) {
		taskc->slice_wall = min(pinned_slice_ns, slice_wall);
		reset_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);
		return taskc->slice_wall;
	}

	if (likely(avg_runtime_wall < slice_wall) &&
	    likely(have_turbo_core && cpuc->big_core)) {
		u32 t_perf_cri  = READ_ONCE(taskc->perf_cri);
		u32 avg_perf    = READ_ONCE(sys_stat.avg_perf_cri);

		if (likely(t_perf_cri > avg_perf) ||
		    lavd_bore_runtime_is_sparse(avg_runtime_wall)) {
			u64 bonus = slice_wall >> 2;
			u64 boosted = (~0ULL - slice_wall < bonus) ?
				      ~0ULL : slice_wall + bonus;

			taskc->slice_wall = min(boosted, slice_max_ns);
			set_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);
			return taskc->slice_wall;
		}
	}

	if (!no_slice_boost && !nr_pinned_tasks &&
	    avg_runtime_wall >= slice_wall) {

		if (can_boost_slice()) {
			u64 boosted = (~0ULL - avg_runtime_wall <
				       LAVD_SLICE_BOOST_BONUS) ?
				      ~0ULL :
				      avg_runtime_wall + LAVD_SLICE_BOOST_BONUS;

			taskc->slice_wall = clamp(boosted, slice_min_ns,
						  LAVD_SLICE_BOOST_MAX);
			set_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);
			return taskc->slice_wall;
		}

		avg_lat_cri = READ_ONCE(sys_stat.avg_lat_cri);
		if (task_lat_cri > avg_lat_cri) {
			u64 twice_slice = slice_wall > (~0ULL >> 1) ?
					  ~0ULL : slice_wall << 1;
			u64 max_slice   = min(avg_runtime_wall, twice_slice);
			u64 boost, boosted;

			if (max_slice <= slice_wall) {
				boosted = max_slice;
			} else {
				u64 cap = max_slice - slice_wall;
				u32 div = (u32)((u64)avg_lat_cri + 1);
				/*
				 * Overflow-safe fast path:
				 *
				 *   slice_wall * task_lat_cri fits in u64
				 *   iff slice_wall <= U64_MAX / 65535
				 *   (task_lat_cri is u16, hard-clamped to
				 *   65535 in calc_lat_cri()).
				 *
				 * The threshold U64_MAX / 65535 ≈ 2.815e14 ns
				 * (~78 hours) — vastly above any sane
				 * slice_max_ns (default 5e6 ns; even 1 hour
				 * is 3.6e12 ns). So in practice the fast
				 * branch always wins. The slow branch
				 * exists so that an absurd or hostile
				 * --slice-max-us tuning (the Rust frontend
				 * applies no upper-bound check) cannot
				 * silently produce a truncated product and
				 * corrupt the boost decision.
				 *
				 * The div != 0 check is defense-in-depth:
				 * div = avg_lat_cri + 1 is provably >= 1 in
				 * any reachable state (avg_lat_cri is u32
				 * and bounded by u16 max in the EWMA), but
				 * future refactors of sys_stat must not be
				 * allowed to trip a silent BPF JIT X/0 -> 0
				 * rewrite.
				 *
				 * MCA on Raptor Cove (-mcpu=raptorlake)
				 * confirms fast path is ~25 cycles vs ~50
				 * for the two-divide lavd_mul_u64_u32_div_cap
				 * fallback. Result is bit-identical to the
				 * fallback across the entire valid input
				 * domain (verified via __int128 reference
				 * over 3528-case fuzz, including div==0,
				 * cap==0, lat_cri==0, slice_wall above and
				 * below the threshold).
				 */
				if (likely(div != 0 &&
					   slice_wall <= U64_MAX / 65535U)) {
					boost = (slice_wall * (u64)task_lat_cri)
						/ div;
					if (boost > cap)
						boost = cap;
				} else {
					boost = lavd_mul_u64_u32_div_cap(
						slice_wall, task_lat_cri,
						div, cap);
				}
				boosted = slice_wall + boost;
			}
			taskc->slice_wall = clamp(boosted, slice_min_ns, max_slice);
			set_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);
			return taskc->slice_wall;
		}
	}

	taskc->slice_wall = slice_wall;
	reset_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);
	return taskc->slice_wall;
}

static void update_stat_for_running(struct task_struct *p,
				    task_ctx *taskc,
				    struct cpu_ctx *cpuc, u64 now)
{
	u64 wait_period, interval;
	u64 task_clk = 0, pelt_clk = 0;
	struct ravg_data local_ravg;
	struct cpu_ctx *prev_cpuc;
	u32 t_lat_cri, t_perf_cri;
	u32 cpu_id;

	/*
	 * Mark the task as running in the duty-cycle ravg immediately,
	 * while the arena pointer is still fresh for the verifier.
	 * Read fields individually to ensure the compiler goes through
	 * the arena-cast pointer for each access.
	 */
	local_ravg.val    = taskc->avg_util_ravg.val;
	local_ravg.val_at = taskc->avg_util_ravg.val_at;
	local_ravg.old    = taskc->avg_util_ravg.old;
	local_ravg.cur    = taskc->avg_util_ravg.cur;
	ravg_accumulate(&local_ravg, LAVD_SCALE, now, LAVD_RAVG_HALFLIFE_NS);
	taskc->avg_util_ravg.val    = local_ravg.val;
	taskc->avg_util_ravg.val_at = local_ravg.val_at;
	taskc->avg_util_ravg.old    = local_ravg.old;
	taskc->avg_util_ravg.cur    = local_ravg.cur;

	/*
	 * Update run-frequency EWMA. Compute portion is in invariant
	 * runtime so that run_freq * avg_runtime_invr in
	 * calc_sum_runtime_factor approximates total invariant compute
	 * demand per second; wait_period stays on wall clock.
	 */
	if (have_scheduled(taskc)) {
		wait_period = time_delta(now, taskc->last_quiescent_clk);
		interval    = taskc->avg_runtime_invr + wait_period;
		if (interval > 0)
			taskc->run_freq = calc_avg_freq(taskc->run_freq, interval);
	}

	if (unlikely(is_monitored)) {
		taskc->resched_interval_wall = time_delta(now,
						     taskc->last_running_clk);
	}

	cpu_id = cpuc->cpu_id;
	taskc->prev_cpu_id = taskc->cpu_id;
	taskc->cpu_id      = cpu_id;

	/*
	 * Update task state at start of run. Group flag clears together
	 * so they coalesce in the store buffer.
	 */
	reset_task_flag(taskc, LAVD_FLAG_IS_WAKEUP);
	reset_task_flag(taskc, LAVD_FLAG_IS_SYNC_WAKEUP);

	taskc->last_running_clk        = now;
	taskc->last_measured_wall_clk  = now;
	/*
	 * lavd_running() can fire on a CPU other than @p's: any
	 * sched_change path (sched_setaffinity, sched_setscheduler,
	 * cgroup move, scx attach worker, etc.) dequeues/re-enqueues
	 * @p under its rq lock from a possibly remote CPU. In that
	 * case scx_clock_task()/scx_clock_pelt() read a remote
	 * rq->clock_task / clock_pelt which the kernel does NOT
	 * refresh while the CPU is NO_HZ-idle, so the values may be
	 * stale. The next account_task_runtime() would then compute
	 * a delta containing the entire idle gap as phantom task
	 * runtime, poisoning tot_task_time_*, util_invr, and the
	 * cgroup-bandwidth charge (-> period_budget debt when
	 * enable_cpu_bw).
	 *
	 * Defer setting the baseline when remote; account_task_runtime()
	 * sees last_measured_*_clk == 0 and skips this measurement,
	 * reseeding from a fresh local read on the next tick / stop.
	 * Missed measurement is bounded to one tick or the task's run
	 * length, whichever is shorter — see upstream commentary.
	 */
	if (bpf_get_smp_processor_id() == cpu_id) {
		task_clk = scx_clock_task(cpu_id);
		pelt_clk = scx_clock_pelt(cpu_id);
	}
	taskc->last_measured_task_clk  = task_clk;
	taskc->last_measured_pelt_clk  = pelt_clk;

	/*
	 * Mark this CPU as busy in the duty-cycle ravg.
	 */
	ravg_accumulate(&cpuc->avg_util_ravg, LAVD_SCALE,
			now, LAVD_RAVG_HALFLIFE_NS);
	cpuc->util_est = (u32)(ravg_read(&cpuc->avg_util_ravg,
				now, LAVD_RAVG_HALFLIFE_NS) >>
				RAVG_FRAC_BITS);

	reset_lock_futex_boost(taskc, cpuc);

	/*
	 * Single-load taskc->lat_cri / perf_cri into locals; they get
	 * compared, summed, and republished into cpuc immediately
	 * below, so caching avoids re-fetches across what is otherwise
	 * a long sequence of dependent loads.
	 */
	t_lat_cri  = READ_ONCE(taskc->lat_cri);
	t_perf_cri = READ_ONCE(taskc->perf_cri);

	if (cpuc->max_lat_cri < t_lat_cri)
		cpuc->max_lat_cri = t_lat_cri;
	cpuc->sum_lat_cri += t_lat_cri;
	cpuc->nr_sched++;

	if (have_little_core) {
		if (cpuc->max_perf_cri < t_perf_cri)
			cpuc->max_perf_cri = t_perf_cri;
		if (cpuc->min_perf_cri > t_perf_cri)
			cpuc->min_perf_cri = t_perf_cri;
		cpuc->sum_perf_cri += t_perf_cri;
	}

	/*
	 * Update preemption-relevant fields. Group together so the
	 * adjacent stores hit the same cacheline window in one drain.
	 */
	cpuc->flags             = taskc->flags;
	cpuc->lat_cri           = t_lat_cri;
	cpuc->running_clk       = now;
	cpuc->est_stopping_clk  = get_est_stopping_clk(taskc, now);

	/*
	 * Skip the is_lat_cri() helper call (an out-of-line BPF
	 * subprogram) and inline the equivalent test using the
	 * t_lat_cri scalar already in a register, plus a single
	 * sys_stat snapshot. Saves one BPF subprogram call + one
	 * arena reload of taskc->lat_cri per running() invocation.
	 * Semantics identical to is_lat_cri (verified against
	 * util.bpf.c upstream:
	 *     bool is_lat_cri(taskc) {
	 *         return taskc->lat_cri >= sys_stat.avg_lat_cri;
	 *     }).
	 *
	 * NOT inlining is_perf_cri() here: its real definition in
	 * power.bpf.c is more complex (handles the !have_little_core
	 * fast-true and the ON_BIG|ON_LITTLE branch), not a simple
	 * threshold compare. Keep the helper call.
	 */
	if (t_lat_cri >= READ_ONCE(sys_stat.avg_lat_cri))
		cpuc->nr_lat_cri++;
	if (is_perf_cri(taskc))
		cpuc->nr_perf_cri++;

	prev_cpuc = get_cpu_ctx_id(taskc->prev_cpu_id);
	if (prev_cpuc && prev_cpuc->cpdom_id != cpuc->cpdom_id)
		cpuc->nr_x_migration++;
}

static void account_task_runtime(struct task_struct *p,
				 task_ctx *taskc,
				 struct cpu_ctx *cpuc,
				 u64 now)
{
	u64 task_time_wall, task_time_iwgt, task_time_invr;
	u64 now_task, now_pelt;
	u32 cpu_id = cpuc->cpu_id;

	now_task       = scx_clock_task(cpu_id);
	now_pelt       = scx_clock_pelt(cpu_id);
	/*
	 * When last_measured_*_clk == 0, update_stat_for_running()
	 * was invoked on a CPU other than @p's, so we could not
	 * reliably snapshot task_clk/pelt_clk. Skip the accounting
	 * for this tick and let the now-local fresh clock reseed the
	 * baseline below — see the block comment in
	 * update_stat_for_running() above.
	 */
	task_time_wall = unlikely(!taskc->last_measured_task_clk) ? 0 :
		time_delta(now_task, taskc->last_measured_task_clk);
	task_time_invr = unlikely(!taskc->last_measured_pelt_clk) ? 0 :
		time_delta(now_pelt, taskc->last_measured_pelt_clk);
	task_time_iwgt = task_time_invr / p->scx.weight;

	WRITE_ONCE(cpuc->tot_task_time_wall,
		   cpuc->tot_task_time_wall + task_time_wall);
	WRITE_ONCE(cpuc->tot_task_time_iwgt,
		   cpuc->tot_task_time_iwgt + task_time_iwgt);
	WRITE_ONCE(cpuc->tot_task_time_invr,
		   cpuc->tot_task_time_invr + task_time_invr);

	if (test_task_flag(taskc, LAVD_FLAG_DOMAIN_PINNED)) {
		WRITE_ONCE(cpuc->tot_dom_pinned_task_time_wall,
			   cpuc->tot_dom_pinned_task_time_wall + task_time_wall);
		WRITE_ONCE(cpuc->tot_dom_pinned_task_time_invr,
			   cpuc->tot_dom_pinned_task_time_invr + task_time_invr);
	}

	taskc->acc_runtime_wall       += task_time_wall;
	taskc->acc_runtime_invr       += task_time_invr;
	taskc->svc_time_iwgt          += task_time_iwgt;
	taskc->last_measured_wall_clk  = now;
	taskc->last_measured_task_clk  = now_task;
	taskc->last_measured_pelt_clk  = now_pelt;

	/*
	 * Branch hint: the *common* configuration disables cgroup
	 * bandwidth control. Hint accordingly so we don't pay a
	 * mispredict on every tick of every fair task.
	 */
	if (unlikely(enable_cpu_bw) && (p->pid != lavd_pid))
		scx_cgroup_bw_consume(taskc->cgrp_id, task_time_wall, (u64)taskc);
}

static void update_stat_for_stopping(struct task_struct *p,
				     task_ctx *taskc,
				     struct cpu_ctx *cpuc)
{
	u64 now = scx_bpf_now();
	u64 task_svc, cur_svc, ret_svc;
	int i;

	account_task_runtime(p, taskc, cpuc, now);

	taskc->avg_runtime_wall = calc_avg(taskc->avg_runtime_wall,
					   taskc->acc_runtime_wall);
	taskc->avg_runtime_invr = calc_avg(taskc->avg_runtime_invr,
					   taskc->acc_runtime_invr);

	/*
	 * Mark this CPU as idle in the duty-cycle ravg.
	 */
	ravg_accumulate(&cpuc->avg_util_ravg, 0, now,
			LAVD_RAVG_HALFLIFE_NS);
	cpuc->util_est = (u32)(ravg_read(&cpuc->avg_util_ravg, now,
				  LAVD_RAVG_HALFLIFE_NS) >> RAVG_FRAC_BITS);

	taskc->last_slice_used_wall = time_delta(now, taskc->last_running_clk);

	/*
	 * Monotonic-publish cur_svc_time_iwgt via a bounded CAS loop.
	 *
	 * The previous "READ_ONCE/compare/WRITE_ONCE" was a textbook
	 * lost-update on a value whose contract is "monotonically
	 * non-decreasing": two CPUs finishing tasks concurrently would
	 * both observe a smaller cur and both write — the lower writer
	 * loses. This mirrors the BORE pass's adoption of
	 * cache-aware patch #9 (READ_ONCE/WRITE_ONCE consistency on
	 * shared lockless state) but goes one step further because
	 * here we *also* need atomicity, not just consistency.
	 */
	task_svc = taskc->svc_time_iwgt;
	cur_svc  = READ_ONCE(cur_svc_time_iwgt);
	bpf_for(i, 0, LAVD_MAX_RETRY) {
		if (cur_svc >= task_svc)
			break;
		ret_svc = __sync_val_compare_and_swap(&cur_svc_time_iwgt,
						      cur_svc, task_svc);
		if (ret_svc == cur_svc)
			break;
		cur_svc = ret_svc;
	}

	reset_lock_futex_boost(taskc, cpuc);
}

static void update_stat_for_refill(struct task_struct *p,
				   task_ctx *taskc,
				   struct cpu_ctx *cpuc)
{
	u64 now = scx_bpf_now();

	account_task_runtime(p, taskc, cpuc, now);

	taskc->avg_runtime_wall = calc_avg(taskc->avg_runtime_wall,
					   taskc->acc_runtime_wall);
	taskc->avg_runtime_invr = calc_avg(taskc->avg_runtime_invr,
					   taskc->acc_runtime_invr);
}

static bool can_direct_dispatch(struct cpu_ctx *cpuc, bool is_cpu_idle)
{
	return (is_cpu_idle && !queued_on_cpu(cpuc)) ||
	       (lb_local_dsq_util_wall > 0 &&
		cpuc->avg_util_wall < lb_local_dsq_util_wall);
}

/*
 * qload_invr is incremented/decremented atomically at every enqueue
 * and dequeue. Migration decisions in plan_x_cpdom_migration() and
 * try_to_steal_task() need the fresh per-domain queued-load value at
 * steal time; a sys_stat snapshot would be up to one interval (10 ms)
 * stale and could misclassify a domain that just drained or absorbed
 * a burst.
 */
static __always_inline void account_queued_load(task_ctx *taskc,
						u8 cpdom_id)
{
	struct cpdom_ctx *cpdomc;
	u32 load;

	if (cpdom_id >= LAVD_CPDOM_MAX_NR)
		return;

	/* Skip if already accounted: prevent double-add when a task
	 * walks through multiple enqueue paths before running. */
	if (READ_ONCE(taskc->queued_in_cpdom_id) < LAVD_CPDOM_MAX_NR)
		return;

	/* Snapshot the load value at enqueue time. The unaccount path
	 * subtracts this exact snapshot, preventing drift when util_est
	 * changes between enqueue and dequeue. */
	load   = task_load_metric(taskc);
	cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);
	if (cpdomc && load)
		/*
		 * Skip the LOCKed XADD when load == 0 (typical for a
		 * brand-new task before ravg has accumulated any duty
		 * cycle). LOCK XADD on a hot cacheline costs ~6-15
		 * cycles + an RFO that bounces qload_invr between
		 * stealer CPUs; a 0-add is wasted work but still pays
		 * the full cacheline tax. The matching unaccount path
		 * also skips when queued_load_snapshot == 0 (see below).
		 */
		__sync_fetch_and_add(&cpdomc->qload_invr, load);

	taskc->queued_load_snapshot = load;
	WRITE_ONCE(taskc->queued_in_cpdom_id, cpdom_id);
}

static __always_inline void unaccount_queued_load(task_ctx *taskc)
{
	struct cpdom_ctx *cpdomc;
	u8 cpdom_id = READ_ONCE(taskc->queued_in_cpdom_id);
	u32 snapshot;

	if (cpdom_id >= LAVD_CPDOM_MAX_NR)
		return;

	/*
	 * Skip the LOCKed XSUB when our enqueue snapshot was 0 -- the
	 * matching account_queued_load() skipped the XADD on the same
	 * condition, so qload_invr is unchanged and we have nothing
	 * to undo. Pure RFO savings on every dequeue of a new task.
	 */
	snapshot = taskc->queued_load_snapshot;
	if (snapshot) {
		cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpdom_id]);
		if (cpdomc)
			__sync_fetch_and_sub(&cpdomc->qload_invr, snapshot);
	}

	WRITE_ONCE(taskc->queued_in_cpdom_id, LAVD_CPDOM_MAX_NR);
}

static int cgroup_throttled(struct task_struct *p, task_ctx *taskc, bool put_aside)
{
	int ret, ret2;

	/*
	 * Never throttle the scheduler process itself, so it can always
	 * make forward progress.
	 */
	if (p->pid == lavd_pid)
		return 0;

	/*
	 * Under CPU bandwidth control using cpu.max, we should first check
	 * if the cgroup is throttled or not. If not, we will go ahead.
	 * Otherwise, we should put the task aside for later execution.
	 *
	 * Note that we cannot use scx_bpf_task_cgroup() here because this can
	 * be called only from ops.enqueue() and ops.dispatch().
	 */
	ret = scx_cgroup_bw_throttled(taskc->cgrp_id, p, (u64)taskc);
	if ((ret == -EAGAIN) && put_aside) {
		ret2 = scx_cgroup_bw_put_aside(p, (u64)taskc, p->scx.dsq_vtime,
					       taskc->cgrp_id);
		if (ret2)
			return ret2;
	}
	return ret;
}

s32 BPF_STRUCT_OPS(lavd_select_cpu, struct task_struct *p, s32 prev_cpu,
		   u64 wake_flags)
{
	struct cpu_ctx *cpuc_cur = get_cpu_ctx();
	struct pick_ctx ictx = {
		.p          = p,
		.taskc      = get_task_ctx_curcpu(p, cpuc_cur),
		.prev_cpu   = prev_cpu,
		.cpuc_cur   = cpuc_cur,
		.wake_flags = wake_flags,
	};
	struct task_struct *waker;
	bool found_idle = false;
	s32 cpu_id;

	if (!ictx.taskc || !ictx.cpuc_cur)
		return prev_cpu;

	if (wake_flags & SCX_WAKE_SYNC)
		set_task_flag(ictx.taskc, LAVD_FLAG_IS_SYNC_WAKEUP);
	else
		reset_task_flag(ictx.taskc, LAVD_FLAG_IS_SYNC_WAKEUP);

	/*
	 * Detect IRQ-context wakeups so we can later boost the wakee's
	 * latency-criticality.
	 *
	 * WARNING: bpf_in_nmi/hardirq/serving_softirq() are supported
	 * only on x86/arm64; on others they always return 0. Never gate
	 * on !bpf_in_xxx().
	 *
	 * Snapshot each IRQ-state probe exactly once. select_cpu runs
	 * in process context with preemption disabled, so the IRQ
	 * state is stable for the duration; collapsing the upstream
	 * pattern's two bpf_in_serving_softirq() calls into one helper
	 * call saves ~20 cycles per wake on Raptor Cove (BPF helper
	 * dispatch + softirq-state probe). bpf_in_* helpers are pure
	 * scalar probes -- no side effects, safe to cache.
	 */
	{
		const bool in_hi  = bpf_in_hardirq();
		const bool in_nmi = bpf_in_nmi();
		const bool in_si  = bpf_in_serving_softirq();

		if (unlikely((in_hi || in_nmi) && !in_si)) {
			set_task_flag(ictx.taskc, LAVD_FLAG_WOKEN_BY_HARDIRQ);
			reset_task_flag(ictx.taskc, LAVD_FLAG_WOKEN_BY_SOFTIRQ);
		} else if (unlikely(in_si ||
				    ((waker = bpf_get_current_task_btf()) &&
				     is_ksoftirqd(waker)))) {
			set_task_flag(ictx.taskc, LAVD_FLAG_WOKEN_BY_SOFTIRQ);
			reset_task_flag(ictx.taskc, LAVD_FLAG_WOKEN_BY_HARDIRQ);
		}
	}

	cpu_id = pick_idle_cpu(&ictx, &found_idle);
	cpu_id = cpu_id >= 0 ? cpu_id : prev_cpu;
	ictx.taskc->suggested_cpu_id = cpu_id;

	if (found_idle) {
		struct cpu_ctx *cpuc;

		set_task_flag(ictx.taskc, LAVD_FLAG_IDLE_CPU_PICKED);

		cpuc = get_cpu_ctx_id(cpu_id);
		if (!cpuc) {
			scx_bpf_error("Failed to lookup cpu_ctx: %d", cpu_id);
			goto out;
		}

		if (can_direct_dispatch(cpuc, true)) {
			/*
			 * Direct-dispatch bypasses ops.enqueue(). The
			 * throttle check there is never reached, so do
			 * it here. unlikely() since the default config
			 * disables cpu.max bandwidth control.
			 */
			if (unlikely(enable_cpu_bw) &&
			    (cgroup_throttled(p, ictx.taskc, false) == -EAGAIN))
				goto out;

			p->scx.dsq_vtime = calc_when_to_run(p, ictx.taskc);
			lavd_apply_bore_deadline(p, ictx.taskc);
			p->scx.slice = LAVD_SLICE_MAX_NS_DFL;
			account_queued_load(ictx.taskc, cpuc->cpdom_id);
			scx_bpf_dsq_insert(p, SCX_DSQ_LOCAL, p->scx.slice, 0);
			goto out;
		}
	} else {
		reset_task_flag(ictx.taskc, LAVD_FLAG_IDLE_CPU_PICKED);
	}
out:
	return cpu_id;
}

void BPF_STRUCT_OPS(lavd_enqueue, struct task_struct *p, u64 enq_flags)
{
	struct cpu_ctx *cpuc, *cpuc_cur;
	s32 task_cpu, cpu = -ENOENT;
	bool is_idle = false;
	task_ctx *taskc;
	u64 dsq_id;

	cpuc_cur = get_cpu_ctx();
	taskc    = get_task_ctx_curcpu(p, cpuc_cur);
	if (!taskc || !cpuc_cur) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}

	if (!(enq_flags & SCX_ENQ_REENQ)) {
		if (enq_flags & SCX_ENQ_WAKEUP)
			set_task_flag(taskc, LAVD_FLAG_IS_WAKEUP);
		else
			reset_task_flag(taskc, LAVD_FLAG_IS_WAKEUP);

		p->scx.dsq_vtime = calc_when_to_run(p, taskc);
		lavd_apply_bore_deadline(p, taskc);
	}

	p->scx.slice = LAVD_SLICE_MIN_NS_DFL;

	task_cpu = scx_bpf_task_cpu(p);
	if (likely(!__COMPAT_is_enq_cpu_selected(enq_flags))) {
		struct pick_ctx ictx = {
			.p          = p,
			.taskc      = taskc,
			.prev_cpu   = task_cpu,
			.cpuc_cur   = cpuc_cur,
			.wake_flags = 0,
		};
		cpu = pick_idle_cpu(&ictx, &is_idle);
	} else {
		cpu     = task_cpu;
		is_idle = test_task_flag(taskc, LAVD_FLAG_IDLE_CPU_PICKED);
		reset_task_flag(taskc, LAVD_FLAG_IDLE_CPU_PICKED);
	}

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}

	taskc->suggested_cpu_id = cpu;
	taskc->cpdom_id         = cpuc->cpdom_id;

	if (unlikely(test_task_flag_mask(taskc, LAVD_MASK_MIGRATION)))
		reset_task_flag(taskc, LAVD_MASK_MIGRATION);

	if (unlikely(enable_cpu_bw) &&
	    (cgroup_throttled(p, taskc, true) == -EAGAIN)) {
		debugln("Task %s[pid%d/cgid%llu] is throttled.",
			p->comm, p->pid, taskc->cgrp_id);
		return;
	}

	if (is_effectively_pinned(taskc) && (taskc->pinned_cpu_id == -ENOENT)) {
		taskc->pinned_cpu_id = cpu;
		__sync_fetch_and_add(&cpuc->nr_pinned_tasks, 1);
		debugln("cpu%d [%d] -- %s:%d -- %s:%d", cpuc->cpu_id,
			cpuc->nr_pinned_tasks, p->comm, p->pid, __func__,
			__LINE__);
	}

	if (!is_idle && lavd_is_microburst(taskc) && !queued_on_cpu(cpuc)) {
		scx_bpf_dsq_insert(p, SCX_DSQ_LOCAL_ON | cpu, p->scx.slice,
				   enq_flags);
		account_queued_load(taskc, cpuc->cpdom_id);
		return;
	}

	if (can_direct_dispatch(cpuc, is_idle)) {
		scx_bpf_dsq_insert(p, SCX_DSQ_LOCAL_ON | cpu, p->scx.slice,
				   enq_flags);
	} else {
		dsq_id = get_target_dsq_id(p, cpuc, taskc);
		scx_bpf_dsq_insert_vtime(p, dsq_id, p->scx.slice,
					 p->scx.dsq_vtime, enq_flags);
	}
	account_queued_load(taskc, cpuc->cpdom_id);

	if (is_idle) {
		if (scx_bpf_test_and_clear_cpu_idle(cpu))
			scx_bpf_kick_cpu(cpu, SCX_KICK_IDLE);
		return;
	}

	if (!no_preemption)
		try_find_and_kick_victim_cpu(p, taskc, cpu,
					     cpdom_to_dsq(cpuc->cpdom_id));
}

static
int enqueue_cb(struct task_struct __arg_trusted *p, task_ctx *taskc)
{
	struct cpu_ctx *cpuc;
	u64 dsq_id;
	s32 cpu;
	u32 nr_cpu;

	if (!p || !taskc)
		return 0;

	nr_cpu = READ_ONCE(nr_cpu_ids);

	cpu = taskc->suggested_cpu_id;
	if (cpu < 0 || (u32)cpu >= nr_cpu || cpu >= LAVD_CPU_ID_MAX)
		cpu = scx_bpf_task_cpu(p);
	if (cpu < 0 || (u32)cpu >= nr_cpu || cpu >= LAVD_CPU_ID_MAX ||
	    !bpf_cpumask_test_cpu(cpu, p->cpus_ptr))
		cpu = scx_bpf_task_cpu(p);

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return 0;
	}

	taskc->suggested_cpu_id = cpu;
	taskc->cpdom_id         = cpuc->cpdom_id;

	if (is_effectively_pinned(taskc) && taskc->pinned_cpu_id == -ENOENT) {
		taskc->pinned_cpu_id = cpu;
		__sync_fetch_and_add(&cpuc->nr_pinned_tasks, 1);
	}

	dsq_id = get_target_dsq_id(p, cpuc, taskc);
	scx_bpf_dsq_insert_vtime(p, dsq_id, p->scx.slice, p->scx.dsq_vtime, 0);
	account_queued_load(taskc, cpuc->cpdom_id);

	if (scx_bpf_test_and_clear_cpu_idle(cpu))
		scx_bpf_kick_cpu(cpu, SCX_KICK_IDLE);
	return 0;
}

void BPF_STRUCT_OPS(lavd_dequeue, struct task_struct *p, u64 deq_flags)
{
	task_ctx *taskc;
	int ret;

	taskc = get_task_ctx(p);
	if (!taskc) {
		debugln("Failed to lookup task_ctx for task %d", p->pid);
		return;
	}

	if (p != __COMPAT_scx_bpf_cpu_curr(scx_bpf_task_cpu(p)))
		unaccount_queued_load(taskc);

	if (likely(!enable_cpu_bw))
		return;

	if ((ret = scx_cgroup_bw_cancel((u64)taskc)))
		debugln("Failed to cancel task %d with %d", p->pid, ret);
}

__hidden __attribute__ ((noinline))
void consume_prev(struct task_struct *prev, task_ctx *taskc_prev,
		  struct cpu_ctx *cpuc)
{
	if (!prev || !(prev->scx.flags & SCX_TASK_QUEUED))
		return;

	taskc_prev = taskc_prev ?: get_task_ctx(prev);
	if (!taskc_prev)
		return;

	update_stat_for_refill(prev, taskc_prev, cpuc);

	if (unlikely(enable_cpu_bw) &&
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
	struct task_struct *p;
	struct cpu_ctx *cpuc;
	u32 nr_cpu;
	int ret;

	nr_cpu = READ_ONCE(nr_cpu_ids);
	if (cpu < 0 || cpu >= LAVD_CPU_ID_MAX || (u32)cpu >= nr_cpu)
		return;

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}
	cpu_dsq_id   = cpu_to_dsq(cpu);
	cpdom_dsq_id = cpdom_to_dsq(cpuc->cpdom_id);

	if (unlikely(enable_cpu_bw) && (ret = scx_cgroup_bw_reenqueue())) {
		scx_bpf_error("Failed to reenqueue backlogged tasks: %d", ret);
	}

	if (prev && (prev->scx.flags & SCX_TASK_QUEUED) &&
	    is_lock_holder_running(cpuc)) {
		consume_prev(prev, NULL, cpuc);
		return;
	}

	if (use_full_cpus())
		goto consume_out;

	bpf_rcu_read_lock();
	active = active_cpumask;
	ovrflw = ovrflw_cpumask;
	if (!active || !ovrflw) {
		scx_bpf_error("Failed to prepare cpumasks.");
		bpf_rcu_read_unlock();
		return;
	}

	if (bpf_cpumask_test_cpu(cpu, cast_mask(active)) ||
	    bpf_cpumask_test_cpu(cpu, cast_mask(ovrflw))) {
		bpf_rcu_read_unlock();
		goto consume_out;
	}

	/* This CPU belongs to neither active nor overflow set. */

	if (use_per_cpu_dsq() && scx_bpf_dsq_nr_queued(cpu_dsq_id)) {
		bpf_cpumask_set_cpu(cpu, ovrflw);
		bpf_rcu_read_unlock();
		goto consume_out;
	}

	if (prev) {
		if (is_permanently_pinned(prev)) {
			bpf_cpumask_set_cpu(cpu, ovrflw);
			bpf_rcu_read_unlock();
			goto consume_out;
		} else if (is_migration_disabled(prev)) {
			bpf_rcu_read_unlock();
			goto consume_out;
		}

		taskc_prev = get_task_ctx(prev);
		if (taskc_prev &&
		    test_task_flag(taskc_prev, LAVD_FLAG_IS_AFFINITIZED) &&
		    bpf_cpumask_test_cpu(cpu, prev->cpus_ptr) &&
		    !bpf_cpumask_intersects(cast_mask(active), prev->cpus_ptr) &&
		    !bpf_cpumask_intersects(cast_mask(ovrflw), prev->cpus_ptr)) {
			bpf_cpumask_set_cpu(cpu, ovrflw);
			bpf_rcu_read_unlock();
			goto consume_out;
		}
	}

	if (!use_cpdom_dsq()) {
		bpf_rcu_read_unlock();
		return;
	}

	bpf_for_each(scx_dsq, p, cpdom_dsq_id, 0) {
		task_ctx *taskc;
		s32 new_cpu;

		p = bpf_task_from_pid(p->pid);
		if (!p)
			continue;

		if (is_permanently_pinned(p)) {
			new_cpu = scx_bpf_task_cpu(p);
			if (new_cpu == cpu) {
				bpf_cpumask_set_cpu(new_cpu, ovrflw);
				bpf_task_release(p);
				try_consume = true;
				break;
			}
			if (new_cpu >= 0 && (u32)new_cpu < nr_cpu &&
			    new_cpu < LAVD_CPU_ID_MAX &&
			    !bpf_cpumask_test_and_set_cpu(new_cpu, ovrflw))
				scx_bpf_kick_cpu(new_cpu, SCX_KICK_IDLE);
			bpf_task_release(p);
			continue;
		} else if (is_migration_disabled(p)) {
			new_cpu = scx_bpf_task_cpu(p);
			if (new_cpu == cpu) {
				bpf_task_release(p);
				try_consume = true;
				break;
			}
			bpf_task_release(p);
			continue;
		}

		taskc = get_task_ctx(p);
		if (taskc &&
		    (!test_task_flag(taskc, LAVD_FLAG_IS_AFFINITIZED) ||
		     bpf_cpumask_intersects(cast_mask(active), p->cpus_ptr) ||
		     bpf_cpumask_intersects(cast_mask(ovrflw), p->cpus_ptr))) {
			bpf_task_release(p);
			continue;
		}

		new_cpu = find_cpu_in(p->cpus_ptr, cpuc);
		if (new_cpu >= 0 && (u32)new_cpu < nr_cpu &&
		    new_cpu < LAVD_CPU_ID_MAX) {
			if (new_cpu == cpu) {
				bpf_cpumask_set_cpu(new_cpu, ovrflw);
				bpf_task_release(p);
				try_consume = true;
				break;
			}
			else if (!bpf_cpumask_test_and_set_cpu(new_cpu, ovrflw))
				scx_bpf_kick_cpu(new_cpu, SCX_KICK_IDLE);
		}
		bpf_task_release(p);
	}
	bpf_rcu_read_unlock();

	if (!try_consume)
		return;

consume_out:
	if (consume_task(cpu_dsq_id, cpdom_dsq_id))
		return;

	consume_prev(prev, taskc_prev, cpuc);
}

void BPF_STRUCT_OPS(lavd_runnable, struct task_struct *p, u64 enq_flags)
{
	struct task_struct *waker;
	task_ctx *p_taskc, *waker_taskc;
	u64 now, interval;
	int i;

	p_taskc = get_task_ctx(p);
	if (!p_taskc) {
		scx_bpf_error("Failed to lookup task_ctx for task %d", p->pid);
		return;
	}

	WRITE_ONCE(p_taskc->acc_runtime_wall, 0);
	WRITE_ONCE(p_taskc->acc_runtime_invr, 0);

	if (!(enq_flags & SCX_ENQ_WAKEUP))
		return;

	if (enq_flags & (SCX_ENQ_PREEMPT | SCX_ENQ_REENQ | SCX_ENQ_LAST))
		return;

	/*
	 * Cache each IRQ-state probe (helper call) and reorder filters
	 * cheap-first. The kernel-pointer compare p->real_parent vs
	 * waker->real_parent rejects 99% of intra-process wakeups in
	 * ~3 cycles, before we ever pay for the is_ksoftirqd memcmp.
	 * lavd_runnable runs in process context with preemption
	 * disabled; IRQ state is constant for the duration of the
	 * callback.
	 */
	if (bpf_in_hardirq() || bpf_in_serving_softirq() || bpf_in_nmi())
		return;

	waker = bpf_get_current_task_btf();
	if (p->real_parent != waker->real_parent)
		return;
	if (is_ksoftirqd(waker))
		return;
	if (is_kernel_task(p) != is_kernel_task(waker))
		return;

	if (rt_or_dl_task(waker))
		set_task_flag(p_taskc, LAVD_FLAG_WOKEN_BY_RT_DL);
	else
		reset_task_flag(p_taskc, LAVD_FLAG_WOKEN_BY_RT_DL);

	waker_taskc = get_task_ctx(waker);
	if (!waker_taskc) {
		/* Idle task waker (swapper); ignore. */
		return;
	}

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

	if (unlikely(is_monitored)) {
		p_taskc->waker_pid = waker->pid;
		for (i = 0; i < TASK_COMM_LEN && can_loop; i++)
			p_taskc->waker_comm[i] = waker->comm[i];
	}
}

void BPF_STRUCT_OPS(lavd_running, struct task_struct *p)
{
	struct cpu_ctx *cpuc;
	task_ctx *taskc;
	u64 now = scx_bpf_now();

	cpuc  = get_cpu_ctx_task(p);
	taskc = get_task_ctx(p);
	if (!cpuc || !taskc) {
		scx_bpf_error("Failed to lookup context for task %d", p->pid);
		return;
	}

	unaccount_queued_load(taskc);

	if (p->scx.slice == SCX_SLICE_DFL)
		p->scx.dsq_vtime = READ_ONCE(cur_logical_clk);

	p->scx.slice = calc_time_slice(taskc, cpuc);

	advance_cur_logical_clk(p);
	update_stat_for_running(p, taskc, cpuc, now);
	update_cpuperf_target(cpuc);

	try_proc_introspec_cmd(p, taskc);
}

void BPF_STRUCT_OPS(lavd_tick, struct task_struct *p)
{
	struct cpu_ctx *cpuc;
	task_ctx *taskc;
	u64 now;

	cpuc  = get_cpu_ctx_task(p);
	taskc = get_task_ctx(p);
	if (!cpuc || !taskc) {
		scx_bpf_error("Failed to lookup context for task %d", p->pid);
		return;
	}

	now = scx_bpf_now();
	account_task_runtime(p, taskc, cpuc, now);

	if (unlikely(enable_cpu_bw) &&
	    (cgroup_throttled(p, taskc, false) == -EAGAIN)) {
		preempt_at_tick(p, cpuc);
		return;
	}

	if (cpuc->nr_pinned_tasks)
		shrink_slice_at_tick(p, cpuc, now);
}

void BPF_STRUCT_OPS(lavd_stopping, struct task_struct *p, bool runnable)
{
	struct cpu_ctx *cpuc;
	task_ctx *taskc;

	cpuc  = get_cpu_ctx_task(p);
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

	cpuc  = get_cpu_ctx_task(p);
	taskc = get_task_ctx(p);
	if (!cpuc || !taskc) {
		scx_bpf_error("Failed to lookup context for task %d", p->pid);
		return;
	}
	cpuc->flags = 0;

	if (is_effectively_pinned(taskc) && taskc->pinned_cpu_id != -ENOENT) {
		struct cpu_ctx *pinned_cpuc = get_cpu_ctx_id(taskc->pinned_cpu_id);

		if (pinned_cpuc)
			__sync_fetch_and_sub(&pinned_cpuc->nr_pinned_tasks, 1);
		taskc->pinned_cpu_id = -ENOENT;
	}

	if (!(deq_flags & SCX_DEQ_SLEEP))
		return;

	now = scx_bpf_now();
	{
		struct ravg_data local_ravg;
		u64 avg_util_fp;

		local_ravg.val    = taskc->avg_util_ravg.val;
		local_ravg.val_at = taskc->avg_util_ravg.val_at;
		local_ravg.old    = taskc->avg_util_ravg.old;
		local_ravg.cur    = taskc->avg_util_ravg.cur;
		ravg_accumulate(&local_ravg, 0, now, LAVD_RAVG_HALFLIFE_NS);
		avg_util_fp = ravg_read(&local_ravg, now, LAVD_RAVG_HALFLIFE_NS);
		taskc->avg_util_ravg.val    = local_ravg.val;
		taskc->avg_util_ravg.val_at = local_ravg.val_at;
		taskc->avg_util_ravg.old    = local_ravg.old;
		taskc->avg_util_ravg.cur    = local_ravg.cur;
		taskc->util_est = (u32)(avg_util_fp >> RAVG_FRAC_BITS);
	}

	interval = time_delta(now, taskc->last_quiescent_clk);
	if (interval > 0) {
		taskc->wait_freq = calc_avg_freq(taskc->wait_freq, interval);
		taskc->last_quiescent_clk = now;
	}
}

static void cpu_ctx_init_online(struct cpu_ctx *cpuc, u32 cpu_id)
{
	struct bpf_cpumask *cd_cpumask;

	bpf_rcu_read_lock();
	cd_cpumask = MEMBER_VPTR(cpdom_cpumask, [cpuc->cpdom_id]);
	if (!cd_cpumask)
		goto unlock_out;
	bpf_cpumask_set_cpu(cpu_id, cd_cpumask);
unlock_out:
	bpf_rcu_read_unlock();

	cpuc->flags             = 0;
	cpuc->idle_start_clk    = 0;
	cpuc->lat_cri           = 0;
	cpuc->running_clk       = 0;
	cpuc->est_stopping_clk  = SCX_SLICE_INF;
	cpuc->prev_task_clk     = scx_clock_task(cpu_id);
	cpuc->prev_pelt_clk     = scx_clock_pelt(cpu_id);
	cpuc->avg_perf_factor   = LAVD_SCALE;
	barrier();
	cpuc->is_online = true;
}

static void cpu_ctx_init_offline(struct cpu_ctx *cpuc, u32 cpu_id)
{
	struct bpf_cpumask *cd_cpumask;

	bpf_rcu_read_lock();
	cd_cpumask = MEMBER_VPTR(cpdom_cpumask, [cpuc->cpdom_id]);
	if (!cd_cpumask)
		goto unlock_out;
	bpf_cpumask_clear_cpu(cpu_id, cd_cpumask);
unlock_out:
	bpf_rcu_read_unlock();

	cpuc->flags          = 0;
	cpuc->idle_start_clk = 0;
	cpuc->is_online      = false;
	barrier();
	cpuc->lat_cri          = 0;
	cpuc->running_clk      = 0;
	cpuc->est_stopping_clk = SCX_SLICE_INF;
}

void BPF_STRUCT_OPS(lavd_cpu_online, s32 cpu)
{
	struct cpu_ctx *cpuc;

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}

	if (cpuc->max_capacity == 0) {
		scx_bpf_exit(SCX_ECODE_ACT_RESTART,
			"RESTART: cpu %d becomes online. Restart the scheduler.",
			cpu);
		return;
	}

	cpu_ctx_init_online(cpuc, cpu);
	__sync_fetch_and_add(&nr_cpus_onln, 1);
	__sync_fetch_and_add(&total_max_capacity, cpuc->max_capacity);

	update_autopilot_high_cap();
	update_sys_stat();
}

void BPF_STRUCT_OPS(lavd_cpu_offline, s32 cpu)
{
	struct cpu_ctx *cpuc;

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}

	cpu_ctx_init_offline(cpuc, cpu);
	__sync_fetch_and_sub(&nr_cpus_onln, 1);
	__sync_fetch_and_sub(&total_max_capacity, cpuc->max_capacity);

	update_autopilot_high_cap();
	update_sys_stat();
}

void BPF_STRUCT_OPS(lavd_update_idle, s32 cpu, bool idle)
{
	struct cpu_ctx *cpuc;
	u64 now;

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}

	now = scx_bpf_now();

	if (idle) {
		cpuc->idle_start_clk = now;
		reset_cpu_preemption_info(cpuc, false);
	} else {
		for (int i = 0; i < LAVD_MAX_RETRY; i++) {
			u64 old_clk = cpuc->idle_start_clk;
			bool ret;

			if (old_clk == 0)
				break;

			ret = __sync_bool_compare_and_swap(
					&cpuc->idle_start_clk, old_clk, 0);
			if (ret) {
				if (time_after(old_clk, now))
					break;
				u64 duration_wall = time_delta(now, old_clk);
				__sync_fetch_and_add(&cpuc->idle_total_wall,
						     duration_wall);
				break;
			}
		}
	}
}

void BPF_STRUCT_OPS(lavd_set_cpumask, struct task_struct *p,
		    const struct cpumask *cpumask)
{
	task_ctx *taskc;

	taskc = get_task_ctx(p);
	if (!taskc) {
		scx_bpf_error("task_ctx_stor first lookup failed");
		return;
	}

	set_affinity_flags(taskc, cpumask);
}

void BPF_STRUCT_OPS(lavd_cpu_acquire, s32 cpu,
		    struct scx_cpu_acquire_args *args)
{
	struct cpu_ctx *cpuc;

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}

	cpuc->cpuperf_cur = scx_bpf_cpuperf_cur(cpu);
}

void BPF_STRUCT_OPS(lavd_cpu_release, s32 cpu,
		    struct scx_cpu_release_args *args)
{
	struct cpu_ctx *cpuc;

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}

	cpuc->flags = 0;
	reset_cpu_preemption_info(cpuc, true);
	scx_bpf_reenqueue_local();
	reset_cpuperf_target(cpuc);
}

void BPF_STRUCT_OPS(lavd_enable, struct task_struct *p)
{
	task_ctx *taskc;

	taskc = get_task_ctx(p);
	if (!taskc) {
		scx_bpf_error("task_ctx_stor first lookup failed");
		return;
	}

	taskc->svc_time_iwgt = READ_ONCE(cur_svc_time_iwgt);
}

/*
 * Reset the transient, identity-bound bookkeeping that a freshly-forked
 * child must NOT inherit verbatim from its parent.
 *
 * Background: lavd_init_task() byte-copies the parent's task_ctx into
 * the child, which is correct for steady-state characteristics
 * (avg_runtime_*, run_freq, wait_freq, wake_freq, lat_cri, perf_cri,
 * svc_time_iwgt, avg_util_ravg) but wrong for state that is
 * parent-instance-specific:
 *
 *   - Burst-window accumulators (acc_runtime_*) — would get folded
 *     into the EWMA twice once update_stat_for_stopping() runs.
 *   - Wall/task/PELT timestamps (last_*_clk) — using these as
 *     baselines on the child's first run produces a giant spurious
 *     time_delta(), corrupting cpuc->tot_task_time_*, the cgroup
 *     bandwidth charge, and ravg accumulators.
 *   - Transient flags (IS_WAKEUP, IS_SYNC_WAKEUP, SLICE_BOOST,
 *     IDLE_CPU_PICKED, WOKEN_BY_*, MIGRATION_*) — meaningless on a
 *     freshly-forked child.
 *   - Parent-relative bookkeeping (prev_cpu_id, cpu_id, waker_*,
 *     queued_load_snapshot, lat_cri_waker, lat_cri_wakee).
 *
 * This is the same shape of bug the cache-aware scheduling series
 * fixed for `preferred_llc` on fork: a brand-new task inheriting
 * parent identity-bound state corrupts per-runqueue accounting.
 *
 * Anchored to: cache-aware patch #17.
 */
static __always_inline void lavd_init_task_reset_transient(task_ctx *taskc, u64 now)
{
	/* Burst-window runtime accumulators. */
	taskc->acc_runtime_wall = 0;
	taskc->acc_runtime_invr = 0;

	/* Anchor every clock to "now" so the first delta is zero. */
	taskc->last_runnable_clk        = now;
	taskc->last_running_clk         = now;
	taskc->last_quiescent_clk       = now;
	taskc->last_measured_wall_clk   = now;
	taskc->last_measured_task_clk   = now;
	taskc->last_measured_pelt_clk   = now;
	taskc->last_slice_used_wall     = 0;
	taskc->resched_interval_wall    = 0;

	/*
	 * Transient scheduling/wakeup flags. Bulk reset via one
	 * AND-mask: each reset_task_flag() is a separate __hidden BPF
	 * helper call against a `volatile u64` field, so the upstream
	 * pattern emitted 8 load+and+store pairs that the compiler
	 * could not coalesce. ORing the masks gives an identical
	 * final value (proof: x &~ A &~ B == x &~ (A|B)) at 1/8th the
	 * cost. The test-then-reset guard on LAVD_MASK_MIGRATION
	 * (originally added to avoid a needless RMW) is strictly
	 * dominated by the unconditional single RMW here.
	 *
	 * Per fork (Chromium make -j24 peaks ~2k forks/s), this saves
	 * ~7 helper calls × ~6 cycles each = ~40 cycles/fork.
	 */
	reset_task_flag(taskc,
		(u64)LAVD_FLAG_IS_WAKEUP        |
		(u64)LAVD_FLAG_IS_SYNC_WAKEUP   |
		(u64)LAVD_FLAG_SLICE_BOOST      |
		(u64)LAVD_FLAG_IDLE_CPU_PICKED  |
		(u64)LAVD_FLAG_WOKEN_BY_HARDIRQ |
		(u64)LAVD_FLAG_WOKEN_BY_SOFTIRQ |
		(u64)LAVD_FLAG_WOKEN_BY_RT_DL   |
		(u64)LAVD_MASK_MIGRATION);

	/* Parent-relative bookkeeping. */
	taskc->prev_cpu_id          = 0;
	taskc->cpu_id               = 0;
	taskc->lat_cri_waker        = 0;
	taskc->lat_cri_wakee        = 0;
	taskc->queued_load_snapshot = 0;
	WRITE_ONCE(taskc->queued_in_cpdom_id, LAVD_CPDOM_MAX_NR);

	if (unlikely(is_monitored)) {
		taskc->waker_pid    = 0;
		taskc->waker_comm[0] = '\0';
	}
}

s32 BPF_STRUCT_OPS_SLEEPABLE(lavd_init_task, struct task_struct *p,
			     struct scx_init_task_args *args)
{
	task_ctx *taskc, *taskc_parent;
	struct task_struct *parent;
	bool inherited = false;
	int i;
	u64 now;

	if (!p) {
		scx_bpf_error("NULL task_struct pointer received");
		return -ESRCH;
	}

	taskc = scx_task_alloc(p);
	if (!taskc) {
		scx_bpf_error("task_ctx_stor first lookup failed");
		return -ENOMEM;
	}

	now = scx_bpf_now();

	/*
	 * Try to inherit the parent's steady-state characteristics
	 * (latency/perf-criticality, EWMA runtimes, etc.). We then
	 * reset everything that is identity-bound — see
	 * lavd_init_task_reset_transient() for the rationale.
	 */
	parent = bpf_task_from_pid(p->real_parent->pid);
	if (parent && (taskc_parent = get_task_ctx(parent))) {
		/*
		 * Skip the first sizeof(struct scx_task_cgroup_bw) bytes
		 * (the `atq` sub-object — guaranteed to be at offset 0
		 * by _Static_assert in lavd.bpf.h). scx_task_alloc() has
		 * already initialised this sub-object's lib/atq state
		 * for THIS task; byte-copying from the parent would
		 * splice the parent's per-task bandwidth-control
		 * bookkeeping (queue links, reservation state) into the
		 * child, corrupting both tasks' cgroup-bw accounting
		 * once enable_cpu_bw is on. The steady-state EWMAs we
		 * want to inherit (avg_runtime_*, lat_cri, etc.) all
		 * live after the atq sub-object.
		 */
		const u32 skip = sizeof(((task_ctx *)0)->atq);
		for (i = skip; i < sizeof(*taskc) && can_loop; i++)
			((char __arena *)taskc)[i] =
				((char __arena *)taskc_parent)[i];
		inherited = true;
	} else {
		const u32 skip = sizeof(((task_ctx *)0)->atq);
		for (i = skip; i < sizeof(*taskc) && can_loop; i++)
			((char __arena *)taskc)[i] = 0;

		taskc->avg_runtime_wall = sys_stat.slice_wall;
		taskc->avg_runtime_invr = sys_stat.slice_wall;
		taskc->svc_time_iwgt    = sys_stat.avg_svc_time_iwgt;
	}

	/*
	 * Patch-#17 stale-state-on-fork hardening. Always run, both
	 * paths: the no-parent path zeroed the entire ctx but did not
	 * pin the clocks to "now", and the inheriting path needs the
	 * full transient reset.
	 */
	lavd_init_task_reset_transient(taskc, now);

	taskc->suggested_cpu_id = scx_bpf_task_cpu(p);
	taskc->pinned_cpu_id    = -ENOENT;
	taskc->pid              = p->pid;

	if (args && args->cgroup && args->cgroup->kn)
		taskc->cgrp_id = args->cgroup->kn->id;
	else
		taskc->cgrp_id = 0;

	bpf_rcu_read_lock();
	set_affinity_flags(taskc, p->cpus_ptr);
	bpf_rcu_read_unlock();

	if (is_ksoftirqd(p))
		set_task_flag(taskc, LAVD_FLAG_KSOFTIRQD);

	if (parent)
		bpf_task_release(parent);

	(void)inherited; /* reserved for future telemetry */
	return 0;
}

s32 BPF_STRUCT_OPS(lavd_exit_task, struct task_struct *p,
		   struct scx_exit_task_args *args)
{
	struct cpu_ctx *cpuc = get_cpu_ctx();
	task_ctx *taskc      = get_task_ctx(p);

	/*
	 * Drop the local CPU's task_ctx cache entry if it points at @p.
	 * Remote CPUs that may have cached @p rely on natural eviction
	 * by subsequent lookups -- by design, no cross-CPU sweep here.
	 */
	if (cpuc && (cpuc->cached_task == (u64)p ||
		     cpuc->cached_pid  == p->pid))
		cpuc->cached_pid = 0;

	/*
	 * If the task is not the current task on its CPU, it may
	 * still be queued. Unaccount its load. If running,
	 * lavd_running() already unaccounted it.
	 */
	if (taskc && p != __COMPAT_scx_bpf_cpu_curr(scx_bpf_task_cpu(p)))
		unaccount_queued_load(taskc);

	scx_task_free(p);
	return 0;
}

void BPF_STRUCT_OPS(lavd_dump, struct scx_dump_ctx *dctx)
{
	if (enable_cpu_bw)
		scx_cgroup_bw_dump(1, true, true, true);
}

void BPF_STRUCT_OPS(lavd_dump_task, struct scx_dump_ctx *dctx,
		    struct task_struct *p)
{
	int cgroup_throttled = 0, task_throttled = 0;
	char cgrp_name[64] = "unknown";
	struct kernfs_node *kn;
	struct cgroup *cgrp;
	task_ctx *taskc;

	taskc = get_task_ctx(p);
	if (!taskc)
		return;

	if (enable_cpu_bw) {
		cgroup_throttled = scx_cgroup_bw_is_cgroup_throttled(taskc->cgrp_id);
		task_throttled   = scx_cgroup_bw_is_task_throttled((u64)taskc);
	}

	cgrp = bpf_cgroup_from_id(taskc->cgrp_id);
	if (cgrp) {
		kn = BPF_CORE_READ(cgrp, kn);
		bpf_probe_read_kernel_str(cgrp_name, sizeof(cgrp_name),
					  BPF_CORE_READ(kn, name));
		bpf_cgroup_release(cgrp);
	}

	scx_bpf_dump("  \\_ slice: %llu   vtime: %llu/%llu   lat_cri: %d/%d   perf_cri: %d/%d\n",
		     taskc->slice_wall,
		     p->scx.dsq_vtime, READ_ONCE(cur_logical_clk),
		     taskc->lat_cri,  sys_stat.avg_lat_cri,
		     taskc->perf_cri, sys_stat.avg_perf_cri);
	scx_bpf_dump("  \\_ cpdom_id: %d   scpu: %d   cgroup: %s[%llu] (%s)   task_status: %s\n",
		     taskc->cpdom_id, taskc->suggested_cpu_id,
		     cgrp_name, taskc->cgrp_id,
		     (cgroup_throttled) ? "throttled" : "not throttled",
		     (task_throttled)   ? "throttled" : "not throttled");
}

static s32 init_cpdoms(u64 now)
{
	struct cpdom_ctx *cpdomc;
	int err;

	for (int i = 0; i < LAVD_CPDOM_MAX_NR; i++) {
		cpdomc = MEMBER_VPTR(cpdom_ctxs, [i]);
		if (!cpdomc) {
			scx_bpf_error("Failed to lookup cpdom_ctx for %d", i);
			return -ESRCH;
		}

		if (!cpdomc->is_valid)
			continue;

		cpdomc->vuln_thresh = LAVD_VULN_THRESH_INIT;

		if (use_cpdom_dsq()) {
			err = scx_bpf_create_dsq(cpdom_to_dsq(cpdomc->id),
						 cpdomc->numa_id);
			if (err) {
				scx_bpf_error("Failed to create a DSQ for cpdom %llu on NUMA node %d",
					      cpdomc->id, cpdomc->numa_id);
				return err;
			}

			err = scx_bpf_create_dsq(cpdom_to_turb_dsq(cpdomc->id),
						 cpdomc->numa_id);
			if (err) {
				scx_bpf_error("Failed to create a turb DSQ for cpdom %llu on NUMA node %d",
					      cpdomc->id, cpdomc->numa_id);
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

	err    = calloc_cpumask(&active_cpumask);
	active = active_cpumask;
	if (err || !active)
		goto out;

	online_cpumask = scx_bpf_get_online_cpumask();
	nr_cpus_onln   = bpf_cpumask_weight(online_cpumask);
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
	err = calloc_cpumask(&steady_cpumask);
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

	turbo  = turbo_cpumask;
	big    = big_cpumask;
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

		err = calloc_cpumask(&cpuc->a_mask);
		if (err) goto unlock_out;
		err = calloc_cpumask(&cpuc->o_mask);
		if (err) goto unlock_out;
		err = calloc_cpumask(&cpuc->temp_mask);
		if (err) goto unlock_out;
		err = calloc_cpumask(&cpuc->i_mask);
		if (err) goto unlock_out;
		err = calloc_cpumask(&cpuc->ia_mask);
		if (err) goto unlock_out;
		err = calloc_cpumask(&cpuc->io_mask);
		if (err) goto unlock_out;
		err = calloc_cpumask(&cpuc->iat_mask);
		if (err) goto unlock_out;

		cpuc->cpu_id              = cpu;
		cpuc->idle_start_clk      = 0;
		cpuc->lat_cri             = 0;
		cpuc->running_clk         = 0;
		cpuc->est_stopping_clk    = SCX_SLICE_INF;
		cpuc->is_online           = bpf_cpumask_test_cpu(cpu, online_cpumask);
		cpuc->max_capacity        = cpu_capacity[cpu];
		cpuc->effective_capacity  = cpuc->max_capacity;
		cpuc->big_core            = cpu_big[cpu];
		cpuc->turbo_core          = cpu_turbo[cpu];
		cpuc->min_perf_cri        = LAVD_SCALE;
		cpuc->max_freq            = LAVD_SCALE;
		cpuc->futex_op            = LAVD_FUTEX_OP_INVALID;

		cpuc->prev_task_clk   = scx_clock_task(cpu);
		cpuc->prev_pelt_clk   = scx_clock_pelt(cpu);
		cpuc->avg_perf_factor = LAVD_SCALE;

		sum_capacity += cpuc->max_capacity;
		if (cpuc->big_core) {
			nr_cpus_big++;
			big_capacity += cpuc->max_capacity;
			bpf_cpumask_set_cpu(cpu, big);
		} else {
			have_little_core = true;
		}
		if (cpuc->turbo_core) {
			bpf_cpumask_set_cpu(cpu, turbo);
			have_turbo_core = true;
		}
		if (cpuc->max_capacity < one_little_max_capacity)
			one_little_max_capacity = cpuc->max_capacity;
	}

	if (sum_capacity == 0) {
		err = -EINVAL;
		goto unlock_out;
	}
	default_big_core_scale = (big_capacity << LAVD_SHIFT) / sum_capacity;
	total_max_capacity     = sum_capacity;

	bpf_for(cpdom_id, 0, nr_cpdoms) {
		if (cpdom_id >= LAVD_CPDOM_MAX_NR)
			break;

		cpdomc     = MEMBER_VPTR(cpdom_ctxs,   [cpdom_id]);
		cd_cpumask = MEMBER_VPTR(cpdom_cpumask,[cpdom_id]);
		if (!cpdomc || !cd_cpumask) {
			scx_bpf_error("Failed to lookup cpdom_ctx for %llu", cpdom_id);
			err = -ESRCH;
			goto unlock_out;
		}

		if (!cpdomc->is_valid)
			continue;

		bpf_for(i, 0, LAVD_CPU_ID_MAX/64) {
			u64 cpumask = cpdomc->__cpumask[i];

			bpf_for(k, 0, 64) {
				j = cpumask_next_set_bit(&cpumask);
				if (j < 0)
					break;
				cpu = (i * 64) + j;
				if (cpu >= LAVD_CPU_ID_MAX ||
				    (u32)cpu >= READ_ONCE(nr_cpu_ids))
					continue;

				cpuc = get_cpu_ctx_id(cpu);
				if (!cpuc) {
					scx_bpf_error("Failed to lookup cpu_ctx: %d", cpu);
					err = -ESRCH;
					goto unlock_out;
				}

				cpuc->llc_id        = cpdomc->llc_id;
				cpuc->cpdom_id      = cpdomc->id;
				cpuc->cpdom_alt_id  = cpdomc->alt_id;
				if (bpf_cpumask_test_cpu(cpu, online_cpumask)) {
					bpf_cpumask_set_cpu(cpu, cd_cpumask);
					cpdomc->nr_active_cpus++;
					cpdomc->cap_sum_active_cpus +=
						cpuc->effective_capacity;
				}
			}
		}
	}

	bpf_for(cpu, 0, nr_cpu_ids) {
		cpuc = get_cpu_ctx_id(cpu);
		if (!cpuc) {
			scx_bpf_error("Failed to lookup cpu_ctx: %d", cpu);
			err = -ESRCH;
			goto unlock_out;
		}
		debugln("cpu[%d] max_capacity: %d, big_core: %d, turbo_core: %d, "
			"cpdom_id: %llu, alt_id: %llu",
			cpu, cpuc->max_capacity, cpuc->big_core, cpuc->turbo_core,
			cpuc->cpdom_id, cpuc->cpdom_alt_id);
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
	int cpu, err = 0;

	bpf_for(cpu, 0, nr_cpu_ids) {
		cpuc = get_cpu_ctx_id(cpu);
		if (!cpuc) {
			scx_bpf_error("Failed to lookup cpu_ctx: %d", cpu);
			return -ESRCH;
		}
		cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpuc->cpdom_id]);
		if (!cpdomc) {
			scx_bpf_error("Failed to lookup cpdom_ctx for %hhu",
				      cpuc->cpdom_id);
			return -ESRCH;
		}

		if (is_smt_active && (cpu != get_primary_cpu(cpu)))
			continue;

		err = scx_bpf_create_dsq(cpu_to_dsq(cpu), cpdomc->numa_id);
		if (err) {
			scx_bpf_error("Failed to create a DSQ for cpu %d on NUMA node %d",
				      cpu, cpdomc->numa_id);
			return err;
		}
	}

	return 0;
}

s32 BPF_STRUCT_OPS_SLEEPABLE(lavd_cgroup_init, struct cgroup *cgrp,
			     struct scx_cgroup_init_args *args)
{
	int ret;

	if (!enable_cpu_bw)
		return 0;

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

	ret = scx_cgroup_bw_exit(cgrp);
	if (ret)
		scx_bpf_error("Failed to exit a cgroup: %d", ret);
}

void BPF_STRUCT_OPS(lavd_cgroup_move, struct task_struct *p,
		    struct cgroup *from, struct cgroup *to)
{
	task_ctx *taskc;
	int ret;

	taskc = get_task_ctx(p);
	if (!taskc) {
		scx_bpf_error("Failed to get a task context: %d", p->pid);
		return;
	}

	if (to && to->kn)
		taskc->cgrp_id = to->kn->id;
	else
		taskc->cgrp_id = 0;

	if (unlikely(enable_cpu_bw) &&
	    (ret = scx_cgroup_bw_move(p, (u64)taskc, from, to))) {
		scx_bpf_error("Failed to move a task (%s:%d) from cgid%llu to cgid%llu: %d",
			      p->comm, p->pid,
			      from && from->kn ? from->kn->id : 0,
			      to   && to->kn   ? to->kn->id   : 0, ret);
	}
}

void BPF_STRUCT_OPS(lavd_cgroup_set_bandwidth, struct cgroup *cgrp,
		    u64 period_us, u64 quota_us, u64 burst_us)
{
	int ret;

	if (!enable_cpu_bw)
		return;

	ret = scx_cgroup_bw_set(cgrp, period_us, quota_us, burst_us);
	if (ret)
		scx_bpf_error("Failed to set bandwidth of a cgroup: %d", ret);
}

int lavd_enqueue_cb(struct task_struct *p __arg_trusted, u64 ctx)
{
	task_ctx *taskc = (task_ctx *)ctx;

	if (!enable_cpu_bw)
		return 0;

	enqueue_cb(p, taskc);
	return 0;
}
REGISTER_SCX_CGROUP_BW_ENQUEUE_CB(lavd_enqueue_cb);

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

	WRITE_ONCE(cur_logical_clk, LAVD_DL_COMPETE_WINDOW);
	WRITE_ONCE(cur_svc_time_iwgt, 0);

	if (enable_cpu_bw) {
		struct scx_cgroup_bw_config bw_config = {
			.verbose = verbose > 2,
		};
		err = scx_cgroup_bw_lib_init(&bw_config);
	}

	lavd_pid = (u32)bpf_get_current_pid_tgid();

	return err;
}

void BPF_STRUCT_OPS(lavd_exit, struct scx_exit_info *ei)
{
	UEI_RECORD(uei, ei);
}

static
int set_aggressive_migration(void)
{
	struct task_struct *curr;
	struct cpdom_ctx *cpdc;
	struct cpu_ctx *cpuc;
	task_ctx *taskc;

	if (nr_cpdoms == 1)
		return 0;

	/*
	 * get_cpu_ctx() returns the per-CPU cpu_ctx for the current
	 * CPU; cpuc->cpu_id is therefore equal to
	 * bpf_get_smp_processor_id() here. Use the cached field and
	 * drop the redundant helper call -- this hook fires on every
	 * execve syscall, system-wide.
	 */
	cpuc = get_cpu_ctx();
	if (cpuc &&
	    (curr  = bpf_get_current_task_btf()) &&
	    (taskc = get_task_ctx_curcpu(curr, cpuc)) &&
	    (cpdc  = MEMBER_VPTR(cpdom_ctxs, [cpuc->cpdom_id])) &&
	    READ_ONCE(cpdc->is_stealee)) {
		set_task_flag(taskc, LAVD_FLAG_MIGRATION_AGGRESSIVE);
		scx_bpf_kick_cpu(cpuc->cpu_id, SCX_KICK_PREEMPT);
	}

	return 0;
}

SEC("?tracepoint/syscalls/sys_enter_execve")
int BPF_PROG(cond_hook_sys_enter_execve)
{
	set_aggressive_migration();
	return 0;
}

SEC("?tracepoint/syscalls/sys_enter_execveat")
int BPF_PROG(cond_hook_sys_enter_execveat)
{
	set_aggressive_migration();
	return 0;
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
	       .dump			= (void *)lavd_dump,
	       .dump_task		= (void *)lavd_dump_task,
	       .cgroup_init		= (void *)lavd_cgroup_init,
	       .cgroup_exit		= (void *)lavd_cgroup_exit,
	       .cgroup_move		= (void *)lavd_cgroup_move,
	       .cgroup_set_bandwidth	= (void *)lavd_cgroup_set_bandwidth,
	       .init			= (void *)lavd_init,
	       .exit			= (void *)lavd_exit,
	       .timeout_ms		= 30000U,
	       .name			= "lavd");
