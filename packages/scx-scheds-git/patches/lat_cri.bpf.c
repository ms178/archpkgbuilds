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

/* Fast-path safety tolerance for cached lat_cri vs prev_lat_cri (u16). */
#define LAVD_FASTPATH_LAT_CRI_DELTA_THRESH	64U

/*
 * BORE log2 fractional lookup: log2(1 + i/256) * 256
 * (Q8.8 fractional part).
 *
 * This is used to build a fast rounded log2(x) with better accuracy than plain
 * floor(log2) while keeping the SAME SCALE as the original log2_u64() usage.
 */
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

/*
 * Compute log2(v) in Q8.8 (log2 * 256).
 *
 * CAKE/BORE optimized pattern:
 * - fast exponent via clz
 * - fractional via LUT on next 8 bits after MSB
 */
static __always_inline u32 bore_log2_q8(u64 v)
{
	u32 exp, frac_idx;
	u64 norm, frac;

	if (unlikely(v == 0))
		return 0;

	exp = 63U - (u32)__builtin_clzll(v);

	norm = v << (63U - exp);
	frac = norm << 1;
	frac_idx = (u32)(frac >> 56) & 0xFFU;

	return (exp << 8) | (u32)bore_log2_lut[frac_idx];
}

/*
 * Return a rounded integer log2(v) using Q8.8, preserving the scale used by
 * the original code (log2_u64()).
 *
 * This avoids the earlier catastrophic scaling mistake where we shifted by 4
 * and then squared, saturating lat_cri.
 */
static __always_inline u32 bore_log2_u32_rounded(u64 v)
{
	/* Round to nearest integer: (q8 + 0.5) >> 8 */
	return (bore_log2_q8(v) + 128U) >> 8;
}

/*
 * Prefetch hot task_ctx cachelines (CAKE/BORE pattern).
 * Safe in BPF: lowers to no-op or supported prefetch depending on backend.
 */
static __always_inline void prefetch_task_ctx(task_ctx *taskc)
{
	if (likely(taskc != NULL)) {
		__builtin_prefetch(taskc, 0, 3);
		__builtin_prefetch((const char *)taskc + 64, 0, 2);
	}
}

/*
 * Fast-path invalidation / reset.
 *
 * This file DOES NOT set try_fast_path/stable_rounds; it only consumes and
 * resets them when it observes signals proving cached lat_cri is stale.
 */
static __always_inline void bbr_reset_fast_path(task_ctx *taskc)
{
	if (likely(taskc)) {
		taskc->try_fast_path = 0;
		taskc->stable_rounds = 0;
	}
}

static __always_inline bool bbr_lat_cri_delta_ok(u16 cur, u16 prev)
{
	u16 d = (cur >= prev) ? (cur - prev) : (prev - cur);
	return d <= (u16)LAVD_FASTPATH_LAT_CRI_DELTA_THRESH;
}

/*
 * CAKE-inspired "fast path gating":
 * - require stable flag AND cached values consistent with prev_lat_cri
 * - invalidate on inheritance signals (graph changed)
 */
static __always_inline bool bbr_validate_fast_path(task_ctx *taskc)
{
	u16 lat_cri, prev_lat_cri;

	/*
	 * If waker/wakee propagation is pending, cached lat_cri is incomplete.
	 * Force recompute and reset fast path.
	 */
	if (taskc->lat_cri_waker != 0 || taskc->lat_cri_wakee != 0) {
		bbr_reset_fast_path(taskc);
		return false;
	}

	if (!taskc->try_fast_path)
		return false;

	lat_cri = taskc->lat_cri;
	if (unlikely(lat_cri == 0))
		return false;

	prev_lat_cri = taskc->prev_lat_cri;
	if (unlikely(prev_lat_cri == 0))
		return false;

	/*
	 * Extra safety: if cached lat_cri drifted from prev_lat_cri, don't trust it.
	 * Mirrors CAKE's "penalty_delta <= tolerance" guard.
	 */
	if (!bbr_lat_cri_delta_ok(lat_cri, prev_lat_cri))
		return false;

	return true;
}

static __always_inline u64 calc_weight_factor(struct task_struct *p, task_ctx *taskc)
{
	u64 weight_boost = 1;
	u64 flags;
	u64 w;

	if (unlikely(!taskc))
		return (u64)p->scx.weight + 1;

	/*
	 * Snapshot flags once (CAKE/BORE MLP style).
	 * test_task_flag() would re-read atomics repeatedly.
	 */
	flags = READ_ONCE(taskc->flags);
	w = (u64)p->scx.weight;

	if (flags & LAVD_FLAG_IS_WAKEUP)
		weight_boost += LAVD_LC_WEIGHT_BOOST_REGULAR;

	if (flags & LAVD_FLAG_IS_SYNC_WAKEUP)
		weight_boost += LAVD_LC_WEIGHT_BOOST_REGULAR;

	if (unlikely(flags & LAVD_FLAG_WOKEN_BY_HARDIRQ)) {
		reset_task_flag(taskc, LAVD_FLAG_WOKEN_BY_HARDIRQ);
		weight_boost += LAVD_LC_WEIGHT_BOOST_HIGHEST;
	} else if (unlikely(flags & LAVD_FLAG_WOKEN_BY_SOFTIRQ)) {
		reset_task_flag(taskc, LAVD_FLAG_WOKEN_BY_SOFTIRQ);
		weight_boost += LAVD_LC_WEIGHT_BOOST_HIGH;
	}

	if (unlikely(flags & LAVD_FLAG_WOKEN_BY_RT_DL)) {
		reset_task_flag(taskc, LAVD_FLAG_WOKEN_BY_RT_DL);
		weight_boost += LAVD_LC_WEIGHT_BOOST_HIGH;
	}

	if (unlikely(is_kernel_task(p))) {
		weight_boost += LAVD_LC_WEIGHT_BOOST_MEDIUM;

		if (flags & LAVD_FLAG_KSOFTIRQD)
			weight_boost += LAVD_LC_WEIGHT_BOOST_HIGH;

		if (is_kernel_worker(p))
			weight_boost += LAVD_LC_WEIGHT_BOOST_REGULAR;
	}

	if (flags & LAVD_FLAG_IS_AFFINITIZED)
		weight_boost += LAVD_LC_WEIGHT_BOOST_REGULAR;

	if (unlikely(is_pinned(p) || is_migration_disabled(p)))
		weight_boost += LAVD_LC_WEIGHT_BOOST_MEDIUM;

	if (unlikely(flags & LAVD_FLAG_NEED_LOCK_BOOST)) {
		reset_task_flag(taskc, LAVD_FLAG_NEED_LOCK_BOOST);
		weight_boost += LAVD_LC_WEIGHT_BOOST_REGULAR;
	}

	return w * weight_boost + 1;
}

static __always_inline u64 calc_wait_factor(u64 wait_freq)
{
	u64 freq = (wait_freq < (u64)LAVD_LC_FREQ_MAX) ? wait_freq : (u64)LAVD_LC_FREQ_MAX;
	return freq + 1;
}

static __always_inline u64 calc_wake_factor(u64 wake_freq)
{
	u64 freq = (wake_freq < (u64)LAVD_LC_FREQ_MAX) ? wake_freq : (u64)LAVD_LC_FREQ_MAX;
	return freq + 1;
}

static __always_inline u64 calc_reverse_runtime_factor(u64 avg_runtime)
{
	if (likely(LAVD_LC_RUNTIME_MAX > avg_runtime)) {
		return (LAVD_LC_RUNTIME_MAX - avg_runtime) / LAVD_SLICE_MIN_NS_DFL;
	}
	return 1;
}

static __always_inline u64 calc_sum_runtime_factor(struct task_struct *p,
						   u64 avg_runtime,
						   u64 acc_runtime,
						   u64 run_freq)
{
	u64 runtime = (acc_runtime > avg_runtime) ? acc_runtime : avg_runtime;

	run_freq |= (u64)(run_freq == 0);
	runtime  |= (u64)(runtime == 0);

	return ((run_freq * runtime) >> LAVD_SHIFT) * (u64)p->scx.weight;
}

/*
 * Keep log2x() for compatibility (external symbol). Not used internally anymore.
 * This avoids inlining log2_u64 into hot paths and keeps verifier happier.
 */
u32 __attribute__((noinline)) log2x(u64 v)
{
	return log2_u64(v);
}

static void calc_lat_cri(struct task_struct *p, task_ctx *taskc)
{
	/*
	 * CAKE-style MLP snapshot: load everything once.
	 * This reduces repeated memory reads and helps the CPU overlap loads.
	 */
	u64 wait_freq   = READ_ONCE(taskc->wait_freq);
	u64 wake_freq   = READ_ONCE(taskc->wake_freq);
	u64 avg_runtime = READ_ONCE(taskc->avg_runtime);
	u64 run_freq    = READ_ONCE(taskc->run_freq);
	u64 acc_runtime = READ_ONCE(taskc->acc_runtime);

	u64 wait_ft     = calc_wait_factor(wait_freq);
	u64 wake_ft     = calc_wake_factor(wake_freq);
	u64 runtime_ft  = calc_reverse_runtime_factor(avg_runtime);
	u64 weight_ft   = calc_weight_factor(p, taskc);

	u32 log_wwf;
	u64 lat_cri;
	u64 lat_cri_giver;

	/*
	 * Avoid overflow with bounded inputs:
	 * - wait_ft,wake_ft <= 400001
	 * - product <= ~1.6e11 fits in u64 comfortably
	 */
	log_wwf = bore_log2_u32_rounded(wait_ft * wake_ft);

	lat_cri = (u64)log_wwf +
		  (u64)bore_log2_u32_rounded(runtime_ft * weight_ft);

	/* Amplify differences (as upstream) */
	lat_cri = lat_cri * lat_cri;

	/*
	 * Context-aware inheritance.
	 * Note: This must happen on slow path; fast path is gated to ensure
	 * lat_cri_waker/wakee are zero.
	 */
	lat_cri_giver = (u64)taskc->lat_cri_waker + (u64)taskc->lat_cri_wakee;
	if (lat_cri_giver > (2ULL * lat_cri)) {
		u64 giver_inh = (lat_cri_giver - (2ULL * lat_cri)) >> LAVD_LC_INH_GIVER_SHIFT;
		u64 receiver_max = lat_cri >> LAVD_LC_INH_RECEIVER_SHIFT;
		lat_cri += (giver_inh < receiver_max) ? giver_inh : receiver_max;
	}

	/* Clamp to u16 range and ensure non-zero */
	if (lat_cri == 0)
		lat_cri = 1;
	if (lat_cri > 65535)
		lat_cri = 65535;

	taskc->lat_cri = (u16)lat_cri;
	taskc->lat_cri_waker = 0;
	taskc->lat_cri_wakee = 0;

	/*
	 * Performance criticality (hybrid-aware). Preserve upstream logic,
	 * just using the faster rounded log2.
	 */
	if (have_little_core) {
		u64 sum_runtime_ft = calc_sum_runtime_factor(p, avg_runtime, acc_runtime, run_freq);
		u64 perf_cri = (u64)log_wwf + (u64)bore_log2_u32_rounded(sum_runtime_ft);

		if (perf_cri > 65535)
			perf_cri = 65535;

		taskc->perf_cri = (u16)perf_cri;
	} else {
		taskc->perf_cri = (u16)LAVD_SCALE;
	}
}

static u64 calc_greedy_penalty(struct task_struct *p, task_ctx *taskc)
{
	u64 lag_max, penalty;
	u64 avg_svc, svc_time;
	s64 lag;

	avg_svc = READ_ONCE(sys_stat.avg_svc_time);
	svc_time = READ_ONCE(taskc->svc_time);

	/* Correct signed diff (avoid unsigned underflow before cast). */
	lag = (s64)avg_svc - (s64)svc_time;

	lag_max = scale_by_task_weight_inverse(p, LAVD_TASK_LAG_MAX);
	if (unlikely(lag_max == 0))
		return LAVD_SCALE;

	if (lag >= 0) {
		reset_task_flag(taskc, LAVD_FLAG_IS_GREEDY);

		if ((u64)lag > lag_max) {
			taskc->svc_time = avg_svc - lag_max;
			lag = (s64)lag_max;
		}
	} else {
		set_task_flag(taskc, LAVD_FLAG_IS_GREEDY);

		if (lag < -(s64)lag_max)
			lag = -(s64)lag_max;
	}

	/*
	 * penalty_base_q10 = ((-lag + lag_max) << LAVD_SHIFT) / lag_max
	 * penalty = LAVD_SCALE + (penalty_base_q10 >> LAVD_LC_GREEDY_SHIFT)
	 */
	penalty = (((u64)(-lag) + lag_max) << LAVD_SHIFT) / lag_max;
	penalty = LAVD_SCALE + (penalty >> LAVD_LC_GREEDY_SHIFT);

	return penalty;
}

static u64 calc_adjusted_runtime(task_ctx *taskc)
{
	u64 acc = READ_ONCE(taskc->acc_runtime);
	u64 capped = (acc < LAVD_ACC_RUNTIME_MAX) ? acc : LAVD_ACC_RUNTIME_MAX;
	return LAVD_ACC_RUNTIME_MAX + capped;
}

static u64 calc_virtual_deadline_delta(struct task_struct *p, task_ctx *taskc, bool use_fast_path)
{
	u64 deadline, adjusted_runtime, lat_cri, greedy_penalty;

	if (!use_fast_path)
		calc_lat_cri(p, taskc);

	lat_cri = (u64)taskc->lat_cri;
	lat_cri |= (u64)(lat_cri == 0);

	greedy_penalty = calc_greedy_penalty(p, taskc);
	adjusted_runtime = calc_adjusted_runtime(taskc);

	deadline = (adjusted_runtime * greedy_penalty) / lat_cri;
	return deadline >> LAVD_SHIFT;
}

__hidden
u64 calc_when_to_run(struct task_struct *p, task_ctx *taskc)
{
	u64 dl_delta, clc;
	bool use_fast_path;

	if (unlikely(!p || !taskc))
		return READ_ONCE(cur_logical_clk);

	/* CAKE/BORE pattern: prefetch early */
	prefetch_task_ctx(taskc);

	use_fast_path = bbr_validate_fast_path(taskc);
	if (!use_fast_path) {
		/*
		 * If we are forced slow path, keep fast-path state conservative
		 * when we see structural change signals (inheritance handled in
		 * bbr_validate_fast_path) or we recompute lat_cri.
		 * We deliberately do NOT "build" stability here.
		 */
	}

	dl_delta = calc_virtual_deadline_delta(p, taskc, use_fast_path);

	clc = READ_ONCE(cur_logical_clk);
	if (likely(clc >= LAVD_DL_COMPETE_WINDOW))
		clc -= LAVD_DL_COMPETE_WINDOW;

	return clc + dl_delta;
}
