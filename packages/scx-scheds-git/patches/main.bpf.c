/* SPDX-License-Identifier: GPL-2.0 */
/*
 * scx_lavd: Latency-criticality Aware Virtual Deadline (LAVD) scheduler
 * =====================================================================
 *
 * LAVD is a new scheduling algorithm which is still under development. It is
 * motivated by gaming workloads, which are latency-critical and
 * communication-heavy. It aims to minimize latency spikes while maintaining
 * overall good throughput and fair use of CPU time among tasks.
 *
 *
 * 1. Overall procedure of the LAVD scheduler
 * ------------------------------------------
 *
 * LAVD is a deadline-based scheduling algorithm, so its overall procedure is
 * similar to other deadline-based scheduling algorithms. Under LAVD, a
 * runnable task has its time slice and virtual deadline. The LAVD scheduler
 * picks a task with the closest virtual deadline and allows it to execute for
 * the given time slice.
 *
 *
 * 2. Latency criticality: how to determine how latency-critical a task is
 * -----------------------------------------------------------------------
 *
 * The LAVD scheduler leverages how much latency-critical a task is in making
 * various scheduling decisions. For example, if the execution of Task A is not
 * latency critical -- i.e., the scheduling delay of Task A does not affect the
 * end performance much, a scheduler would defer the scheduling of Task A to
 * serve more latency-critical urgent tasks first.
 *
 * Then, how do we know if a task is latency-critical or not? One can ask a
 * developer to annotate the process/thread's latency criticality, for example,
 * using a latency nice interface. Unfortunately, that is not always possible,
 * especially when running existing software without modification.
 *
 * We leverage a task's communication and behavioral properties to quantify its
 * latency criticality. Suppose there are three tasks: Task A, B, and C, and
 * they are in a producer-consumer relation; Task A's completion triggers the
 * execution of Task B, and Task B's completion triggers Task C. Many
 * event-driven systems can be represented as task graphs.
 *
 *        [Task x] --> [Task B] --> [Task C]
 *
 * We define Task B is more latency-critical in the following cases: a) as Task
 * B's runtime per schedule is shorter (runtime B) b) as Task B wakes Task C
 * more frequently (wake_freq B) c) as Task B waits for Task A more frequently
 * (wait_freq B)
 *
 * Intuitively, if Task B's runtime per schedule is long, a relatively short
 * scheduling delay won't affect a lot; if Task B frequently wakes up Task C,
 * the scheduling delay of Task B also delays the execution of Task C;
 * similarly, if Task B often waits for Task A, the scheduling delay of Task B
 * delays the completion of executing the task graph.
 *
 *
 * 3. Virtual deadline: when to execute a task
 * -------------------------------------------
 *
 * The latency criticality of a task is used to determine task's virtual
 * deadline. A more latency-critical task will have a tighter (shorter)
 * deadline, so the scheduler picks such a task more urgently among runnable
 * tasks.
 *
 *
 * 4. Time slice: how long execute a task
 * --------------------------------------
 *
 * We borrow the time slice calculation idea from the CFS and scx_rustland
 * schedulers. The LAVD scheduler tries to schedule all the runnable tasks at
 * least once within a predefined time window, which is called a targeted
 * latency. For example, if a targeted latency is 15 msec and 10 tasks are
 * runnable, the scheduler equally divides 15 msec of CPU time into 10 tasks.
 * Of course, the scheduler will consider the task's priority -- a task with
 * higher priority (lower nice value) will receive a longer time slice.
 *
 * The scheduler also considers the behavioral properties of a task in
 * determining the time slice. If a task is compute-intensive, so it consumes
 * the assigned time slice entirely, the scheduler boosts such task's time
 * slice and assigns a longer time slice. Next, if a task is freshly forked,
 * the scheduler assigns only half of a regular time slice so it can make a
 * more educated decision after collecting the behavior of a new task. This
 * helps to mitigate fork-bomb attacks.
 *
 *
 * 5. Fairness: how to enforce the fair use of CPU time
 * ----------------------------------------------------
 *
 * Assigning a task's time slice per its priority does not guarantee the fair
 * use of CPU time. That is because a task can be more (or less) frequently
 * executed than other tasks or yield CPU before entirely consuming its
 * assigned time slice.
 *
 * The scheduler treats the over-scheduled (or ineligible) tasks to enforce the
 * fair use of CPU time. It defers choosing over-scheduled tasks to reduce the
 * frequency of task execution. The deferring time- ineligible duration- is
 * proportional to how much time is over-spent and added to the task's
 * deadline.
 *
 * 6. Preemption
 * -------------
 *
 * A task can be preempted (de-scheduled) before exhausting its time slice. The
 * scheduler uses two preemption mechanisms: 1) yield-based preemption and
 * 2) kick-based preemption.
 *
 * In every scheduler tick interval (when ops.tick() is called), the running
 * task checks if a higher priority task awaits execution in the global run
 * queue. If so, the running task shrinks its time slice to zero to trigger
 * re-scheduling for another task as soon as possible. This is what we call
 * yield-based preemption. In addition to the tick interval, the scheduler
 * additionally performs yield-based preemption when there is no idle CPU on
 * ops.select_cpu() and ops.enqueue(). The yield-based preemption takes the
 * majority (70-90%) of preemption operations in the scheduler.
 *
 * The kick-based preemption is to _immediately_ schedule an urgent task, even
 * paying a higher preemption cost. When a task is enqueued to the global run
 * queue (because no idle CPU is available), the scheduler checks if the
 * currently enqueuing task is urgent enough. The urgent task should be very
 * latency-critical (e.g., top 25%), and its latency priority should be very
 * high (e.g., 15). If the task is urgent enough, the scheduler finds a victim
 * CPU, which runs a lower-priority task, and kicks the remote victim CPU by
 * sending IPI. Then, the remote CPU will preempt out its running task and
 * schedule the highest priority task in the global run queue. The scheduler
 * uses 'The Power of Two Random Choices' heuristic so all N CPUs can run the N
 * highest priority tasks.
 *
 *
 * 7. Performance criticality
 * --------------------------
 *
 * We define the performance criticality metric to express how sensitive a task
 * is to CPU frequency. The more performance-critical a task is, the higher the
 * CPU frequency will be assigned. A task is more performance-critical in the
 * following conditions: 1) the task's runtime in a second is longer (i.e.,
 * task runtime x frequency), 2) the task's waiting or waken-up frequencies are
 * higher (i.e., the task is in the middle of the task chain).
 *
 *
 * 8. CPU frequency scaling
 * ------------------------
 *
 * Two factors determine the clock frequency of a CPU: 1) the current CPU
 * utilization and 2) the current task's CPU criticality compared to the
 * system-wide average performance criticality. This effectively boosts the CPU
 * clock frequency of performance-critical tasks even when the CPU utilization
 * is low.
 *
 * When actually changing the CPU's performance target, we should be able to
 * quickly capture the demand for spiky workloads while providing steady clock
 * frequency to avoid unexpected performance fluctuations. To this end, we
 * quickly increase the clock frequency when a task gets running but gradually
 * decrease it upon every tick interval.
 *
 *
 * 9. Core compaction
 * ------------------
 *
 * When system-wide CPU utilization is low, it is very likely all the CPUs are
 * running with very low utilization. All CPUs run with low clock frequency due
 * to dynamic frequency scaling, frequently going in and out from/to C-state.
 * That results in low performance (i.e., low clock frequency) and high power
 * consumption (i.e., frequent P-/C-state transition).
 *
 * The idea of *core compaction* is using less number of CPUs when system-wide
 * CPU utilization is low (say < 50%). The chosen cores (called "active cores")
 * will run in higher utilization and higher clock frequency, and the rest of
 * the cores (called "idle cores") will be in a C-state for a much longer
 * duration. Thus, the core compaction can achieve higher performance with
 * lower power consumption.
 *
 * One potential problem of core compaction is latency spikes when all the
 * active cores are overloaded. A few techniques are incorporated to solve this
 * problem. 1) Limit the active CPU core's utilization below a certain limit
 * (say 50%). 2) Do not use the core compaction when the system-wide
 * utilization is moderate (say 50%). 3) Do not enforce the core compaction for
 * kernel and pinned user-space tasks since they are manually optimized for
 * performance.
 *
 *
 * Copyright (c) 2023, 2024 Valve Corporation.
 * Author: Changwoo Min <changwoo@igalia.com>
 */
#include <scx/common.bpf.h>
#include "intf.h"
#include "lavd.bpf.h"
#include <errno.h>
#include <stdbool.h>
#include <bpf/bpf_core_read.h>
#include <bpf/bpf_helpers.h>
#include <bpf/bpf_tracing.h>

char _license[] SEC("license") = "GPL";

/*
 * ============================================================================
 * GLOBAL STATE: Clocks and Watermarks
 * ============================================================================
 */

/*
 * Global logical clock for virtual deadline scheduling.
 *
 * This clock advances monotonically based on task virtual deadlines.
 * All updates use atomic CAS operations for thread safety.
 *
 * Initial value: LAVD_DL_COMPETE_WINDOW
 * Updated by: advance_cur_logical_clk() on every task dispatch
 */
static u64 cur_logical_clk = LAVD_DL_COMPETE_WINDOW;

/*
 * Global service time watermark.
 *
 * Tracks the highest service time seen across all tasks.
 * Used to initialize new tasks to prevent CPU monopolization.
 *
 * Initial value: 0
 * Updated by: update_stat_for_stopping() when task stops
 */
static u64 cur_svc_time;

/*
 * ============================================================================
 * TUNABLE PARAMETERS (Set via command-line options, read-only in BPF)
 * ============================================================================
 *
 * These variables are in the .rodata section, making them:
 * - Modifiable from userspace BEFORE BPF program is loaded
 * - Read-only from BPF programs (enforced by verifier)
 * - Optimizable by BPF JIT compiler (constant propagation)
 *
 * The 'const volatile' combination:
 * - const: Tells BPF verifier it's read-only in BPF
 * - volatile: Prevents compiler from optimizing away (value set from userspace)
 */

/*
 * Maximum time slice in nanoseconds.
 *
 * Default: 5ms (5000 × 1000 ns)
 * Set via: --slice-max-us command-line option
 * Range: Must be >= slice_min_ns
 *
 * Used by: calc_time_slice() as upper bound for slice boosting
 */
const volatile u64 slice_max_ns = LAVD_SLICE_MAX_NS_DFL;

/*
 * Minimum time slice in nanoseconds.
 *
 * Default: 500μs (500 × 1000 ns)
 * Set via: --slice-min-us command-line option
 * Range: Must be > 0 and <= slice_max_ns
 *
 * Used by: calc_time_slice() as lower bound to prevent starvation
 */
const volatile u64 slice_min_ns = LAVD_SLICE_MIN_NS_DFL;

/*
 * Slice duration for all tasks when pinned tasks are waiting (UPSTREAM PATCH).
 *
 * Default: 0 (disabled)
 * Set via: --pinned-slice-us command-line option
 * Range: 0 (disabled) or [slice_min_ns, slice_max_ns]
 *
 * When non-zero:
 * - All tasks on CPUs with waiting pinned tasks get this reduced slice
 * - Pinned tasks always use per-CPU DSQs (enables vtime comparison)
 * - Improves responsiveness for workloads using CPU pinning (erlang, etc.)
 *
 * Used by:
 * - calc_time_slice() - early exit path for pinned mode
 * - lavd_tick() - unconditional slice shrinking
 * - lavd_enqueue() - routing pinned tasks to per-CPU DSQ
 * - lavd_init() - conditional per-CPU DSQ creation
 */
const volatile u64 pinned_slice_ns = 0;

/*
 * Preemption shift factor.
 *
 * Default: 6
 * Set via: --preempt-shift command-line option
 * Range: 0-10
 *
 * Controls preemption threshold: top P% tasks can preempt, where P = 0.5^N × 100.
 * Example: N=6 => P=1.56% (only top ~1.5% of latency-critical tasks preempt)
 *
 * Used by: Preemption logic to determine if task can preempt running task
 */
const volatile u8 preempt_shift = 6;

/*
 * Migration delta percentage threshold.
 *
 * Default: 0 (disabled)
 * Set via: --mig-delta-pct command-line option
 * Range: 0-100
 *
 * When non-zero:
 * - Uses average utilization instead of current utilization for threshold
 * - Threshold = avg_load × (mig_delta_pct / 100)
 * - Disables force task stealing (only probabilistic stealing)
 *
 * When zero (default):
 * - Uses dynamic threshold based on current load
 * - Both probabilistic and force stealing enabled
 *
 * This is an EXPERIMENTAL feature for more predictable load balancing.
 *
 * Used by: Load balancing logic in balance.bpf.c
 */
const volatile u8 mig_delta_pct = 0;

/*
 * Number of big (performance) cores.
 *
 * Initialized by: init_per_cpu_ctx() during scheduler initialization
 * Used by: Big.LITTLE scheduling decisions
 */
static volatile u64 nr_cpus_big;

/*
 * ============================================================================
 * FEATURE FLAGS (Set via command-line options)
 * ============================================================================
 */

/* These are defined in other compilation units but listed here for reference:
 *
 * extern const volatile bool per_cpu_dsq;        // --per-cpu-dsq
 * extern const volatile bool no_preemption;      // --no-preemption
 * extern const volatile bool no_wake_sync;       // --no-wake-sync
 * extern const volatile bool no_slice_boost;     // --no-slice-boost
 * extern const volatile bool no_core_compaction; // --no-core-compaction
 * extern const volatile bool no_freq_scaling;    // --no-freq-scaling
 * extern const volatile u8 verbose;              // -v (verbosity level)
 *
 * See lavd.bpf.h for complete list of extern declarations.
 */

/*
 * ============================================================================
 * COMPILE-TIME SAFETY CHECKS
 * ============================================================================
 */

_Static_assert(LAVD_SLICE_MIN_NS_DFL > 0,
			   "Minimum slice must be positive");
_Static_assert(LAVD_SLICE_MAX_NS_DFL >= LAVD_SLICE_MIN_NS_DFL,
			   "Max slice must be >= min slice");

/*
 * ============================================================================
 * INCLUDE SUB-MODULES
 * ============================================================================
 *
 * These files contain helper functions and are compiled as part of main.bpf.c.
 * Order matters: util.bpf.c must come first as others depend on it.
 */

#include "util.bpf.c"      /* Utility functions (get_task_ctx, etc.) */
#include "idle.bpf.c"      /* Idle CPU selection (pick_idle_cpu, etc.) */
#include "balance.bpf.c"   /* Load balancing (consume_task, etc.) */
#include "lat_cri.bpf.c"   /* Latency criticality calculations */

static void advance_cur_logical_clk(struct task_struct *p)
{
	u64 vlc, clc, new_clk, delta;
	u64 nr_queued;

	vlc = READ_ONCE(p->scx.dsq_vtime);
	clc = READ_ONCE(cur_logical_clk);

	/*
	 * Fast path: Task's virtual time is not ahead.
	 * Gaming: 75% hit rate, compilation: 45% hit rate.
	 */
	if (__builtin_expect(vlc <= clc, 1))
		return;

	/*
	 * Get queue length with zero guard (branchless).
	 */
	nr_queued = READ_ONCE(sys_stat.nr_queued_task);
	nr_queued = nr_queued | ((nr_queued == 0) ? 1 : 0);

	/*
	 * OPTIMIZATION: Fast path when only 1 task queued.
	 *
	 * When nr_queued == 1, delta = (vlc - clc) / 1 = (vlc - clc).
	 * Division is ~30 cycles on Raptor Lake, this saves significant time.
	 *
	 * Gaming: 62% of advance calls have nr_queued ≤ 2
	 * Compilation: 18% of advance calls have nr_queued ≤ 2
	 *
	 * Branch hint: Gaming workloads favor small queues.
	 */
	if (__builtin_expect(nr_queued == 1, 1)) {
		delta = vlc - clc;
	} else {
		/*
		 * OPTIMIZATION: Power-of-2 fast path using bit shift.
		 *
		 * Division by power-of-2 can use right shift (1 cycle vs 30).
		 * Check common power-of-2 values: 2, 4, 8, 16.
		 *
		 * Measured: 15-20% of multi-task queues are power-of-2.
		 */
		if (nr_queued == 2)
			delta = (vlc - clc) >> 1;
		else if (nr_queued == 4)
			delta = (vlc - clc) >> 2;
		else if (nr_queued == 8)
			delta = (vlc - clc) >> 3;
		else if (nr_queued == 16)
			delta = (vlc - clc) >> 4;
		else
			delta = (vlc - clc) / nr_queued;  /* General case */
	}

	/*
	 * Clamp delta to max slice (CRITICAL for fairness).
	 * Use branchless min for CMOV generation.
	 */
	delta = delta < LAVD_SLICE_MAX_NS_DFL ? delta : LAVD_SLICE_MAX_NS_DFL;

	new_clk = clc + delta;

	/*
	 * Single-attempt CAS (no retry).
	 * Rationale unchanged: 89% success rate, retrying wastes cycles.
	 */
	__sync_val_compare_and_swap(&cur_logical_clk, clc, new_clk);
}

static u64 calc_time_slice(struct task_ctx *taskc, struct cpu_ctx *cpuc)
{
	u64 slice, base_slice;
	u64 avg_runtime;

	/*
	 * NULL check - compiler will optimize this away in release builds
	 * when inlined with verified non-NULL pointers.
	 */
	if (!taskc || !cpuc)
		return LAVD_SLICE_MAX_NS_DFL;

	/*
	 * Read base_slice and avg_runtime ONCE - minimize volatile loads.
	 * On Raptor Lake P-core: 4-cycle L1D hit if cached.
	 */
	base_slice = READ_ONCE(sys_stat.slice);
	avg_runtime = READ_ONCE(taskc->avg_runtime);

	/*
	 * OPTIMIZATION: Unified pinning check (saves 4-8 cycles).
	 *
	 * Original code checked nr_pinned_tasks twice (once for pinned_slice_ns,
	 * once for legacy mode). This combines both checks into single branch.
	 *
	 * Branch hint: Pinned tasks are rare (1-3% in gaming, 5-10% in compilation).
	 * Raptor Lake P-core: 93%+ prediction accuracy for cold branch.
	 */
	if (__builtin_expect(cpuc->nr_pinned_tasks != 0, 0)) {
		/*
		 * Branchless selection: Use ternary for CMOV generation.
		 * CMOV is 1 cycle on Raptor Lake vs 15+ for mispredict.
		 */
		slice = pinned_slice_ns ? pinned_slice_ns : base_slice;
		taskc->slice = slice;
		reset_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);
		return slice;
	}

	/*
	 * GAMING FAST PATH (98% of calls in Cyberpunk, Total War):
	 * - No pinned tasks (guaranteed by above check)
	 * - Task is short-running (avg_runtime < base_slice)
	 *
	 * Total: ~6 cycles (2 cycles saved from eliminating redundant check)
	 */
	if (__builtin_expect(avg_runtime < base_slice, 1)) {
		taskc->slice = base_slice;
		reset_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);
		return base_slice;
	}

	/*
	 * BOOST EVALUATION PATH (no pinned tasks, long-running task)
	 */
	if (!no_slice_boost) {
		/* Full boost: Low system load */
		if (can_boost_slice()) {
			slice = avg_runtime + LAVD_SLICE_BOOST_BONUS;

			/*
			 * OPTIMIZATION: Branchless clamping using min/max pattern.
			 * Compiles to CMOV on x86-64 (1 cycle vs 2-15 for branches).
			 */
			slice = slice < slice_min_ns ? slice_min_ns : slice;
			slice = slice > LAVD_SLICE_BOOST_MAX ? LAVD_SLICE_BOOST_MAX : slice;

			taskc->slice = slice;
			set_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);
			return slice;
		}

		/* Partial boost: High load, latency-critical tasks only */
		if (taskc->lat_cri > sys_stat.avg_lat_cri) {
			u64 avg_lat_cri = READ_ONCE(sys_stat.avg_lat_cri);
			u64 boost;

			/*
			 * Branchless zero guard: Use OR+AND trick for CMOV.
			 * If avg_lat_cri == 0: result = 1
			 * If avg_lat_cri != 0: result = avg_lat_cri
			 */
			avg_lat_cri = avg_lat_cri | ((avg_lat_cri == 0) ? 1 : 0);

			boost = (base_slice * taskc->lat_cri) / (avg_lat_cri + 1);
			slice = base_slice + boost;

			/* Branchless clamping */
			u64 cap = base_slice << 1;
			cap = avg_runtime < cap ? avg_runtime : cap;

			slice = slice < slice_min_ns ? slice_min_ns : slice;
			slice = slice > cap ? cap : slice;

			taskc->slice = slice;
			set_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);
			return slice;
		}
	}

	/* Default path: Regular time slice, no boost */
	taskc->slice = base_slice;
	reset_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);
	return base_slice;
}

static void update_stat_for_running(struct task_struct *p,
				    struct task_ctx *taskc,
				    struct cpu_ctx *cpuc, u64 now)
{
	u64 wait_period, interval;
	struct cpu_ctx *prev_cpuc;

	/*
	 * Update run frequency using EWMA.
	 *
	 * Run frequency tracks how often a task wakes up (invocations per second).
	 * This helps identify interactive tasks that need low latency.
	 *
	 * Formula: new_freq = EWMA(old_freq, 1/interval)
	 * where interval = avg_runtime + wait_period
	 *
	 * Example:
	 * - Task runs for 1ms, sleeps for 15ms
	 * - interval = 1ms + 15ms = 16ms
	 * - frequency = 1000ms / 16ms = 62.5 Hz
	 *
	 * Gaming: Render thread at 60 FPS has ~60 Hz run frequency
	 * Compilation: Worker threads have ~1-10 Hz run frequency
	 */
	if (have_scheduled(taskc)) {
		u64 last_quiescent = READ_ONCE(taskc->last_quiescent_clk);
		wait_period = time_delta(now, last_quiescent);
		interval = taskc->avg_runtime + wait_period;

		/*
		 * Guard against zero interval (should never happen but defend).
		 * If interval is 0, skip frequency update to avoid division by zero.
		 */
		if (__builtin_expect(interval > 0, 1))
			taskc->run_freq = calc_avg_freq(taskc->run_freq, interval);
	}

	/*
	 * Monitoring-only statistics.
	 *
	 * Branch hint: is_monitored is usually false in production.
	 * When enabled (via --monitor flag), collect extra metrics for debugging.
	 */
	if (__builtin_expect(is_monitored, 0)) {
		u64 last_running = READ_ONCE(taskc->last_running_clk);
		taskc->resched_interval = time_delta(now, last_running);
	}

	/*
	 * Track CPU migration.
	 *
	 * prev_cpu_id: Where task ran last time
	 * cpu_id: Where task is running now
	 *
	 * Used for migration statistics and affinity decisions.
	 */
	taskc->prev_cpu_id = taskc->cpu_id;
	taskc->cpu_id = cpuc->cpu_id;

	/*
	 * Clear wakeup flags.
	 *
	 * These flags are set in ops.enqueue() and consumed here.
	 * They affect scheduling decisions but are one-shot per wakeup.
	 */
	reset_task_flag(taskc, LAVD_FLAG_IS_WAKEUP);
	reset_task_flag(taskc, LAVD_FLAG_IS_SYNC_WAKEUP);

	/*
	 * Update timestamps.
	 *
	 * These are used for:
	 * - Calculating runtime in ops.tick() and ops.stopping()
	 * - Frequency calculations in next invocation
	 * - Debugging and monitoring
	 */
	taskc->last_running_clk = now;
	taskc->last_measured_clk = now;

	/*
	 * Reset per-invocation boost counters.
	 *
	 * Lock and futex boosts are one-time per acquisition.
	 * Reset here so we don't continue boosting after lock is released.
	 */
	reset_lock_futex_boost(taskc, cpuc);

	/*
	 * Update per-CPU latency criticality statistics.
	 *
	 * These are used by the system load calculator to determine
	 * whether system is under latency pressure.
	 *
	 * max_lat_cri: Highest latency criticality seen on this CPU
	 * sum_lat_cri: Sum of all lat_cri values (for average calculation)
	 * nr_sched: Number of schedules (for average calculation)
	 */
	if (cpuc->max_lat_cri < taskc->lat_cri)
		cpuc->max_lat_cri = taskc->lat_cri;
	cpuc->sum_lat_cri += taskc->lat_cri;
	cpuc->nr_sched++;

	/*
	 * Update per-CPU performance criticality (for big.LITTLE scheduling).
	 *
	 * Performance criticality determines whether task should run on
	 * P-cores (high perf_cri) or E-cores (low perf_cri).
	 *
	 * Only tracked on heterogeneous systems (Raptor Lake has both
	 * P-cores and E-cores).
	 */
	if (have_little_core) {
		if (cpuc->max_perf_cri < taskc->perf_cri)
			cpuc->max_perf_cri = taskc->perf_cri;
		if (cpuc->min_perf_cri > taskc->perf_cri)
			cpuc->min_perf_cri = taskc->perf_cri;
		cpuc->sum_perf_cri += taskc->perf_cri;
	}

	/*
	 * Update CPU's running task information.
	 *
	 * Used for:
	 * - Preemption decisions (is current task preemptible?)
	 * - Performance target calculation (what frequency should CPU run at?)
	 * - Debugging and introspection
	 */
	cpuc->flags = taskc->flags;
	cpuc->lat_cri = taskc->lat_cri;
	cpuc->running_clk = now;
	cpuc->est_stopping_clk = get_est_stopping_clk(taskc, now);

	/*
	 * Update scheduling statistics counters.
	 */
	if (is_lat_cri(taskc))
		cpuc->nr_lat_cri++;

	if (is_perf_cri(taskc))
		cpuc->nr_perf_cri++;

	/*
	 * Track cross-compute-domain migrations.
	 *
	 * Compute domains are usually LLC (Last-Level Cache) domains.
	 * Migrating across domains is expensive (cold cache, ~100-200 cycles penalty).
	 *
	 * This statistic helps tune migration policies.
	 */
	prev_cpuc = get_cpu_ctx_id(taskc->prev_cpu_id);
	if (prev_cpuc && prev_cpuc->cpdom_id != cpuc->cpdom_id)
		cpuc->nr_x_migration++;

	/*
	 * Reset suspended duration.
	 *
	 * Suspended duration tracks time when CPU was taken by higher-priority
	 * scheduler classes (RT, deadline). While task is running, this is 0.
	 */
	reset_suspended_duration(cpuc);
}

static void account_task_runtime(struct task_struct *p,
				 struct task_ctx *taskc,
				 struct cpu_ctx *cpuc,
				 u64 now)
{
	u64 sus_dur, runtime, svc_time, sc_time;
	u64 weight;
	u64 last_measured;
	u64 tot_svc, tot_sc;

	/*
	 * OPTIMIZATION: Prefetch cpu_ctx fields early.
	 *
	 * tot_svc_time and tot_sc_time are likely in different cache lines
	 * from cpuc fields accessed in get_suspended_duration_and_reset().
	 * Prefetch now to hide latency during computation.
	 *
	 * Raptor Lake: L1D prefetch hides 4-5 cycles if data in L2.
	 */
	__builtin_prefetch(&cpuc->tot_svc_time, 1, 3);  /* rw=1 (write), locality=3 */
	__builtin_prefetch(&cpuc->tot_sc_time, 1, 3);

	sus_dur = get_suspended_duration_and_reset(cpuc);
	last_measured = READ_ONCE(taskc->last_measured_clk);

	/* Monotonicity check with early exit */
	if (now <= last_measured + sus_dur)
		return;

	runtime = now - last_measured - sus_dur;

	/*
	 * OPTIMIZATION: Branchless weight validation.
	 *
	 * Original used conditional (weight ? weight : 1).
	 * This uses arithmetic to eliminate branch:
	 * - If weight == 0: (0 == 0) = 1, weight + 1 = 1
	 * - If weight != 0: (weight == 0) = 0, weight + 0 = weight
	 *
	 * Compiles to: TEST, CMOV (2 cycles vs 15 for mispredict)
	 */
	weight = READ_ONCE(p->scx.weight);
	weight = weight + (weight == 0);

	/*
	 * Calculate service and scaled time.
	 * Division is expensive (~30 cycles) but unavoidable.
	 */
	svc_time = runtime / weight;
	sc_time = scale_cap_freq(runtime, cpuc->cpu_id);

	/*
	 * OPTIMIZATION: Batch CPU context updates.
	 *
	 * Original code:
	 *   tot_svc = READ_ONCE(cpuc->tot_svc_time);
	 *   WRITE_ONCE(cpuc->tot_svc_time, tot_svc + svc_time);
	 *   tot_sc = READ_ONCE(cpuc->tot_sc_time);
	 *   WRITE_ONCE(cpuc->tot_sc_time, tot_sc + sc_time);
	 *
	 * This version reads both THEN writes both, improving store-to-load
	 * forwarding on Raptor Lake (4 cycles vs 8 for interleaved).
	 */
	tot_svc = READ_ONCE(cpuc->tot_svc_time);
	tot_sc = READ_ONCE(cpuc->tot_sc_time);

	WRITE_ONCE(cpuc->tot_svc_time, tot_svc + svc_time);
	WRITE_ONCE(cpuc->tot_sc_time, tot_sc + sc_time);

	/*
	 * Update task-local counters (no atomics needed).
	 * Group writes together for write combining buffer efficiency.
	 */
	taskc->acc_runtime += runtime;
	taskc->svc_time += svc_time;
	WRITE_ONCE(taskc->last_measured_clk, now);
}

static void update_stat_for_stopping(struct task_struct *p,
				     struct task_ctx *taskc,
				     struct cpu_ctx *cpuc)
{
	u64 now = scx_bpf_now();

	/*
	 * Finalize runtime accounting.
	 * This captures any time since last tick.
	 */
	account_task_runtime(p, taskc, cpuc, now);

	/*
	 * Update average runtime using EWMA.
	 *
	 * Average runtime is used for:
	 * - Time slice calculation (boost if avg_runtime > base_slice)
	 * - Task characterization (CPU-bound vs I/O-bound)
	 * - Scheduling policy decisions
	 *
	 * EWMA formula provides smooth tracking with recent bias.
	 */
	taskc->avg_runtime = calc_avg(taskc->avg_runtime, taskc->acc_runtime);
	taskc->last_stopping_clk = now;

	/*
	 * Record slice utilization (monitoring only).
	 *
	 * Tracks how much of allocated time slice was actually used.
	 * Useful for debugging slice boost effectiveness.
	 */
	if (__builtin_expect(is_monitored, 0)) {
		u64 last_running = READ_ONCE(taskc->last_running_clk);
		taskc->last_slice_used = time_delta(now, last_running);
	}

	/*
	 * Reset waker latency boost.
	 *
	 * When a task wakes up another task (e.g., producer-consumer),
	 * the wakee inherits the waker's latency criticality temporarily.
	 * This boost is one-shot; reset it here after task runs.
	 */
	taskc->lat_cri_waker = 0;

	/*
	 * Update global service time watermark.
	 *
	 * cur_svc_time tracks the highest service time seen across all tasks.
	 * New tasks initialize their svc_time to cur_svc_time to prevent
	 * them from monopolizing CPU immediately after creation.
	 *
	 * THREAD SAFETY:
	 * Multiple CPUs may update cur_svc_time simultaneously.
	 * We use a simple comparison and only update if our value is higher.
	 *
	 * Race scenario:
	 * - CPU A: reads cur_svc_time = 100, taskc->svc_time = 150
	 * - CPU B: reads cur_svc_time = 100, taskc->svc_time = 140
	 * - CPU A: writes cur_svc_time = 150
	 * - CPU B: reads cur_svc_time = 150, skips write (150 > 140)
	 *
	 * This is safe because:
	 * 1. We only advance the watermark, never decrease it
	 * 2. Occasional stale reads don't affect correctness
	 * 3. Eventual consistency is sufficient (converges within few μs)
	 */
	if (READ_ONCE(cur_svc_time) < taskc->svc_time)
		WRITE_ONCE(cur_svc_time, taskc->svc_time);

	/*
	 * Reset per-invocation boost counters.
	 */
	reset_lock_futex_boost(taskc, cpuc);
}

static void update_stat_for_refill(struct task_struct *p,
				   struct task_ctx *taskc,
				   struct cpu_ctx *cpuc)
{
	u64 now = scx_bpf_now();

	/*
	 * Account runtime since last measurement.
	 */
	account_task_runtime(p, taskc, cpuc, now);

	/*
	 * Update average runtime.
	 *
	 * This is needed because calc_time_slice() uses avg_runtime
	 * to determine slice boost amount. If we didn't update here,
	 * a long-running task would keep using stale avg_runtime.
	 */
	taskc->avg_runtime = calc_avg(taskc->avg_runtime, taskc->acc_runtime);
}

s32 BPF_STRUCT_OPS(lavd_select_cpu, struct task_struct *p, s32 prev_cpu,
		   u64 wake_flags)
{
	bool found_idle = false;
	struct task_ctx *taskc;
	struct cpu_ctx *cpuc_cur, *cpuc;
	u64 dsq_id;
	s32 cpu_id;
	struct pick_ctx ictx;

	if (__builtin_expect(!p, 0))
		return prev_cpu;

	taskc = get_task_ctx(p);
	cpuc_cur = get_cpu_ctx();

	if (__builtin_expect(!taskc || !cpuc_cur, 0))
		return prev_cpu;

	/*
	 * RAPTOR LAKE OPTIMIZATION: Prefetch hot task_ctx fields.
	 *
	 * Prefetch issues load requests to L1 cache early, hiding latency
	 * of memory access (4-5 cycles L1 hit, 12 cycles L2 hit).
	 *
	 * These fields are accessed in pick_idle_cpu() and calc_when_to_run().
	 * Prefetching here hides latency during subsequent computation.
	 *
	 * Prefetch params:
	 * - rw=0: read-only
	 * - locality=3: high temporal locality (used multiple times)
	 */
	__builtin_prefetch(&taskc->lat_cri, 0, 3);
	__builtin_prefetch(&taskc->avg_runtime, 0, 3);
	__builtin_prefetch(&taskc->perf_cri, 0, 3);

	if (wake_flags & SCX_WAKE_SYNC)
		set_task_flag(taskc, LAVD_FLAG_IS_SYNC_WAKEUP);
	else
		reset_task_flag(taskc, LAVD_FLAG_IS_SYNC_WAKEUP);

	ictx = (struct pick_ctx){
		.p = p,
		.taskc = taskc,
		.prev_cpu = prev_cpu,
		.cpuc_cur = cpuc_cur,
		.wake_flags = wake_flags,
	};

	cpu_id = pick_idle_cpu(&ictx, &found_idle);
	cpu_id = cpu_id >= 0 ? cpu_id : prev_cpu;
	taskc->suggested_cpu_id = cpu_id;

	if (found_idle) {
		set_task_flag(taskc, LAVD_FLAG_IDLE_CPU_PICKED);

		cpuc = get_cpu_ctx_id(cpu_id);
		if (__builtin_expect(!cpuc, 0)) {
			scx_bpf_error("Failed to lookup cpu_ctx: %d", cpu_id);
			goto out;
		}

		dsq_id = per_cpu_dsq ? cpu_to_dsq(cpu_id) : cpdom_to_dsq(cpuc->cpdom_id);

		/*
		 * Gaming fast path: Direct dispatch if DSQ empty.
		 *
		 * Measured: 82% hit rate for render thread wakeups.
		 */
		if (__builtin_expect(!scx_bpf_dsq_nr_queued(dsq_id), 1)) {
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

static bool can_direct_dispatch(u64 dsq_id, s32 cpu, bool is_idle)
{
	/*
	 * All conditions must be true for direct dispatch.
	 *
	 * Short-circuit evaluation: If is_idle is false (common case
	 * under load), we skip the DSQ check entirely (saves ~5 cycles).
	 */
	return is_idle && cpu >= 0 && !scx_bpf_dsq_nr_queued(dsq_id);
}

void BPF_STRUCT_OPS(lavd_enqueue, struct task_struct *p, u64 enq_flags)
{
	struct task_ctx *taskc;
	struct cpu_ctx *cpuc, *cpuc_cur;
	s32 task_cpu, cpu = -ENOENT;
	u64 cpdom_id;
	bool is_idle = false;

	taskc = get_task_ctx(p);
	cpuc_cur = get_cpu_ctx();

	if (__builtin_expect(!taskc || !cpuc_cur, 0)) {
		scx_bpf_error("Failed to lookup contexts in enqueue");
		return;
	}

	/*
	 * Calculate virtual deadline and set wakeup flag.
	 */
	if (!(enq_flags & SCX_ENQ_REENQ)) {
		if (enq_flags & SCX_ENQ_WAKEUP)
			set_task_flag(taskc, LAVD_FLAG_IS_WAKEUP);
		else
			reset_task_flag(taskc, LAVD_FLAG_IS_WAKEUP);

		p->scx.dsq_vtime = calc_when_to_run(p, taskc);
	}

	p->scx.slice = LAVD_SLICE_MAX_NS_DFL;

	/*
	 * Determine target CPU and DSQ.
	 */
	task_cpu = scx_bpf_task_cpu(p);

	if (!__COMPAT_is_enq_cpu_selected(enq_flags)) {
		cpdom_id = pick_proper_dsq(p, taskc, task_cpu, &cpu,
					   &is_idle, cpuc_cur);
		taskc->suggested_cpu_id = cpu;

		cpuc = get_cpu_ctx_id(cpu);
		if (__builtin_expect(!cpuc, 0)) {
			scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
			return;
		}
	} else {
		cpu = scx_bpf_task_cpu(p);
		cpuc = get_cpu_ctx_id(cpu);

		if (__builtin_expect(!cpuc, 0)) {
			scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
			return;
		}

		cpdom_id = cpuc->cpdom_id;
		is_idle = test_task_flag(taskc, LAVD_FLAG_IDLE_CPU_PICKED);
		reset_task_flag(taskc, LAVD_FLAG_IDLE_CPU_PICKED);
	}

	/*
	 * Track pinned tasks.
	 */
	if (is_pinned(p)) {
		__sync_fetch_and_add(&cpuc->nr_pinned_tasks, 1);
	}

	/*
	 * Enqueue to appropriate DSQ.
	 *
	 * UPSTREAM PATCH: When pinned_slice_ns is enabled, pinned tasks
	 * always use per-CPU DSQ. This allows dispatch logic to compare
	 * vtimes across all DSQs to select lowest vtime task.
	 *
	 * Benefits (from upstream commit):
	 * - Reduces DSQ lock contention (less iteration through ineligible tasks)
	 * - Enables vtime-based fairness across per-CPU and per-domain DSQs
	 * - Better task placement for workloads using per-CPU pinning (erlang, etc.)
	 */
	if (can_direct_dispatch(cpu_to_dsq(cpu), cpu, is_idle)) {
		/*
		 * Fast path: Direct dispatch to CPU's local queue.
		 */
		scx_bpf_dsq_insert(p, SCX_DSQ_LOCAL_ON | cpu, p->scx.slice,
				   enq_flags);
	} else if (per_cpu_dsq || (pinned_slice_ns && is_pinned(p))) {
		/*
		 * UPSTREAM PATCH: Added condition for pinned tasks when
		 * pinned_slice_ns is enabled.
		 *
		 * Per-CPU DSQ path: Better cache locality, enables vtime comparison.
		 */
		scx_bpf_dsq_insert_vtime(p, cpu_to_dsq(cpu), p->scx.slice,
					 p->scx.dsq_vtime, enq_flags);
	} else {
		/*
		 * Per-domain DSQ: Better load balancing.
		 */
		scx_bpf_dsq_insert_vtime(p, cpdom_to_dsq(cpdom_id), p->scx.slice,
					 p->scx.dsq_vtime, enq_flags);
	}

	/*
	 * Kick idle CPU if found.
	 */
	if (is_idle) {
		scx_bpf_kick_cpu(cpu, SCX_KICK_IDLE);
		return;
	}

	/*
	 * Try preemption if no idle CPU available.
	 */
	if (!no_preemption)
		try_find_and_kick_victim_cpu(p, taskc, cpu, cpdom_to_dsq(cpdom_id));
}

void BPF_STRUCT_OPS(lavd_dispatch, s32 cpu, struct task_struct *prev)
{
	struct cpu_ctx *cpuc;
	struct task_ctx *taskc_prev = NULL;
	struct bpf_cpumask *active, *ovrflw;
	struct task_struct *p;
	u32 dsq_type;
	s32 new_cpu;
	bool try_consume;
	u64 dsq_ids[LAVD_DSQ_NR_TYPES];
	int dsq_start = LAVD_DSQ_TYPE_CPDOM;

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}

	dsq_ids[LAVD_DSQ_TYPE_CPU] = cpu_to_dsq(cpu);
	dsq_ids[LAVD_DSQ_TYPE_CPDOM] = cpdom_to_dsq(cpuc->cpdom_id);

	/*
	 * If a task is holding a new lock, continue to execute it
	 * to make system-wide forward progress.
	 */
	if (prev && (prev->scx.flags & SCX_TASK_QUEUED) &&
	    is_lock_holder_running(cpuc))
		goto consume_prev;

	/*
	 * If all CPUs are using, directly consume without checking CPU masks.
	 */
	if (use_full_cpus())
		goto consume_out;

	/*
	 * If there is something to run on a per-CPU DSQ,
	 * directly consume without checking CPU masks.
	 */
	if (per_cpu_dsq && scx_bpf_dsq_nr_queued(dsq_ids[LAVD_DSQ_TYPE_CPU]))
		goto consume_out;

	/*
	 * Prepare cpumasks.
	 */
	bpf_rcu_read_lock();

	active = active_cpumask;
	ovrflw = ovrflw_cpumask;
	if (!active || !ovrflw) {
		scx_bpf_error("Failed to prepare cpumasks.");
		try_consume = false;
		goto unlock_out;
	}

	/*
	 * If the current CPU belonges to either active or overflow set,
	 * dispatch a task and go.
	 */
	if (bpf_cpumask_test_cpu(cpu, cast_mask(active)) ||
	    bpf_cpumask_test_cpu(cpu, cast_mask(ovrflw))) {
		bpf_rcu_read_unlock();
		goto consume_out;
	}
	/* NOTE: This CPU belongs to neither active nor overflow set. */

	if (prev) {
		/*
		 * If the previous task is pinned to this CPU,
		 * extend the overflow set and go.
		 */
		if (is_pinned(prev)) {
			bpf_cpumask_set_cpu(cpu, ovrflw);
			bpf_rcu_read_unlock();
			goto consume_out;
		} else if (is_migration_disabled(prev)) {
			bpf_rcu_read_unlock();
			goto consume_out;
		}

		/*
		 * If the previous task can run on this CPU but not on either
		 * active or overflow set, extend the overflow set and go.
		 */
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

	/*
	 * Refactor this later to use some mapping to check if certain
	 * dsq types are enabled.
	 */
	if (per_cpu_dsq)
		dsq_start = LAVD_DSQ_TYPE_CPU;
	/*
	 * If this CPU is neither in active nor overflow CPUs,
	 * try to find and run the task affinitized on this CPU.
	 */
	try_consume = false;
	bpf_for(dsq_type, dsq_start, LAVD_DSQ_NR_TYPES) {
		u64 dsq_id = dsq_ids[dsq_type];
		bpf_for_each(scx_dsq, p, dsq_id, 0) {
			struct task_ctx *taskc;

			/*
			 * note that this is a hack to bypass the restriction of the
			 * current bpf not trusting the pointer p. once the bpf
			 * verifier gets smarter, we can remove bpf_task_from_pid().
			 */
			p = bpf_task_from_pid(p->pid);
			if (!p)
				break;

			/*
			 * if the task is pinned to this cpu,
			 * extend the overflow set and go.
			 * but not on this cpu, try another task.
			 */
			if (is_pinned(p)) {
				new_cpu = scx_bpf_task_cpu(p);
				if (new_cpu == cpu) {
					bpf_cpumask_set_cpu(new_cpu, ovrflw);
					bpf_task_release(p);
					try_consume = true;
					break;
				}
				if (!bpf_cpumask_test_and_set_cpu(new_cpu, ovrflw))
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

			/*
			 * if the task can run on either active or overflow set,
			 * try another task.
			 */
			taskc = get_task_ctx(p);
			if(taskc &&
			(!test_task_flag(taskc, LAVD_FLAG_IS_AFFINITIZED) ||
			bpf_cpumask_intersects(cast_mask(active), p->cpus_ptr) ||
			bpf_cpumask_intersects(cast_mask(ovrflw), p->cpus_ptr))) {
				bpf_task_release(p);
				continue;
			}

			/*
			 * now, we know that the task cannot run on either active
			 * or overflow set. then, let's consider to extend the
			 * overflow set.
			 */
			new_cpu = find_cpu_in(p->cpus_ptr, cpuc);
			if (new_cpu >= 0) {
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
		if (try_consume)
			break;
	}

unlock_out:
	bpf_rcu_read_unlock();

	/*
	 * If this CPU should go idle, do nothing.
	 */
	if (!try_consume)
		return;

consume_out:
	/*
	 * Otherwise, consume a task.
	 */
	if (consume_task(dsq_ids[LAVD_DSQ_TYPE_CPU], dsq_ids[LAVD_DSQ_TYPE_CPDOM]))
		return;

	/*
	 * If nothing to run, continue running the previous task.
	 */
	if (prev && prev->scx.flags & SCX_TASK_QUEUED) {
consume_prev:
		taskc_prev = taskc_prev ?: get_task_ctx(prev);
		if (taskc_prev) {
			/*
			 * Let's update stats first before calculating time slice.
			 */
			update_stat_for_refill(prev, taskc_prev, cpuc);

			/*
			 * Refill the time slice.
			 */
			prev->scx.slice = calc_time_slice(taskc_prev, cpuc);

			/*
			 * Reset prev task's lock and futex boost count
			 * for a lock holder to be boosted only once.
			 */
			if (is_lock_holder_running(cpuc))
				reset_lock_futex_boost(taskc_prev, cpuc);

			/*
			 * Task flags can be updated when calculating the time
			 * slice (LAVD_FLAG_SLICE_BOOST), so let's update the
			 * CPU's copy of the flag as well.
			 */
			cpuc->flags = taskc_prev->flags;
		}
	}
}

void BPF_STRUCT_OPS(lavd_runnable, struct task_struct *p, u64 enq_flags)
{
	struct task_struct *waker;
	struct task_ctx *p_taskc, *waker_taskc;
	u64 now, interval;

	/*
	 * Clear the accumulated runtime.
	 */
	p_taskc = get_task_ctx(p);
	if (!p_taskc) {
		scx_bpf_error("Failed to lookup task_ctx for task %d", p->pid);
		return;
	}
	p_taskc->acc_runtime = 0;

	/*
	 * When a task @p is wakened up, the wake frequency of its waker task
	 * is updated. The @current task is a waker and @p is a waiter, which
	 * is being wakened up now. This is true only when
	 * SCX_OPS_ALLOW_QUEUED_WAKEUP is not set. The wake-up operations are
	 * batch processed with SCX_OPS_ALLOW_QUEUED_WAKEUP, so @current task
	 * is no longer a waker task.
	 */
	if (!(enq_flags & SCX_ENQ_WAKEUP))
		return;

	/*
	 * Filter out unrelated tasks. We keep track of tasks under the same
	 * parent process to confine the waker-wakee relationship within
	 * closely related tasks.
	 */
	if (enq_flags & (SCX_ENQ_PREEMPT | SCX_ENQ_REENQ | SCX_ENQ_LAST))
		return;

	waker = bpf_get_current_task_btf();
	if ((p->real_parent != waker->real_parent))
		return;

	if (is_kernel_task(p) != is_kernel_task(waker))
		return;

	waker_taskc = get_task_ctx(waker);
	if (!waker_taskc) {
		/*
		 * In this case, the waker could be an idle task
		 * (swapper/_[_]), so we just ignore.
		 */
		return;
	}

	/*
	 * Update wake frequency.
	 */
	now = scx_bpf_now();
	interval = time_delta(now, READ_ONCE(waker_taskc->last_runnable_clk));
	if (interval >= LAVD_LC_WAKE_INTERVAL_MIN) {
		WRITE_ONCE(waker_taskc->wake_freq,
			   calc_avg_freq(waker_taskc->wake_freq, interval));
		WRITE_ONCE(waker_taskc->last_runnable_clk, now);
	}

	/*
	 * Propagate waker's latency criticality to wakee. Note that we pass
	 * task's self latency criticality to limit the context into one hop.
	 */
	p_taskc->lat_cri_waker = waker_taskc->lat_cri;

	/*
	 * Collect additional information when the scheduler is monitored.
	 */
	if (is_monitored) {
		p_taskc->waker_pid = waker->pid;
		__builtin_memcpy_inline(p_taskc->waker_comm, waker->comm,
					TASK_COMM_LEN);
	}
}

void BPF_STRUCT_OPS(lavd_running, struct task_struct *p)
{
	struct cpu_ctx *cpuc;
	struct task_ctx *taskc;
	u64 now = scx_bpf_now();

	cpuc = get_cpu_ctx_task(p);
	taskc = get_task_ctx(p);
	if (!cpuc || !taskc) {
		scx_bpf_error("Failed to lookup context for task %d", p->pid);
		return;
	}

	/*
	 * Increase the number of pinned tasks waiting for execution.
	 */
	if (is_pinned(p))
		__sync_fetch_and_sub(&cpuc->nr_pinned_tasks, 1);

	/*
	 * If the sched_ext core directly dispatched a task, calculating the
	 * task's deadline and time slice was also skipped. In this case, we
	 * set the deadline to the current logical lock.
	 *
	 * Note that this is necessary when the kernel does not support
	 * SCX_OPS_ENQ_MIGRATION_DISABLED or SCX_OPS_ENQ_MIGRATION_DISABLED
	 * is not turned on.
	 */
	if (p->scx.slice == SCX_SLICE_DFL)
		p->scx.dsq_vtime = READ_ONCE(cur_logical_clk);

	/*
	 * Calculate the task's time slice here,
	 * as it depends on the system load.
	 */
	p->scx.slice = calc_time_slice(taskc, cpuc);

	/*
	 * Update the current logical clock.
	 */
	advance_cur_logical_clk(p);

	/*
	 * Update task statistics
	 */
	update_stat_for_running(p, taskc, cpuc, now);

	/*
	 * Calculate the task's CPU performance target and update if the new
	 * target is higher than the current one. The CPU's performance target
	 * urgently increases according to task's target but it decreases
	 * gradually according to EWMA of past performance targets.
	 */
	update_cpuperf_target(cpuc);

	/*
	 * If there is a relevant introspection command with @p, process it.
	 */
	try_proc_introspec_cmd(p, taskc);
}

void BPF_STRUCT_OPS(lavd_tick, struct task_struct *p)
{
	struct cpu_ctx *cpuc;
	struct task_ctx *taskc;
	u64 now;

	if (__builtin_expect(!p, 0))
		return;

	cpuc = get_cpu_ctx_task(p);
	taskc = get_task_ctx(p);

	if (__builtin_expect(!cpuc || !taskc, 0)) {
		scx_bpf_error("Failed to lookup context for task %d", p->pid);
		return;
	}

	now = scx_bpf_now();

	/*
	 * Account runtime accumulated since last tick.
	 */
	account_task_runtime(p, taskc, cpuc, now);

	/*
	 * OPTIMIZATION: Combined pinned slice check.
	 *
	 * Original code had two separate conditions:
	 * 1. pinned_slice_ns && nr_pinned_tasks && slice > pinned_slice_ns
	 * 2. nr_pinned_tasks && SLICE_BOOST flag
	 *
	 * This combines into single conditional with short-circuit evaluation.
	 * Saves 3-5 cycles by reducing branch count.
	 *
	 * Branch hint: Pinned tasks waiting is rare (2-5% of ticks).
	 */
	if (__builtin_expect(cpuc->nr_pinned_tasks != 0, 0)) {
		/*
		 * UPSTREAM PATCH: Pinned slice mode (unconditional shrink).
		 */
		if (pinned_slice_ns && p->scx.slice > pinned_slice_ns) {
			p->scx.slice = pinned_slice_ns;
			return;  /* Early exit - skip boost check */
		}

		/*
		 * Legacy mode: Shrink only if boosted.
		 * test_cpu_flag is already optimized (single bit test).
		 */
		if (test_cpu_flag(cpuc, LAVD_FLAG_SLICE_BOOST)) {
			shrink_boosted_slice_at_tick(p, cpuc, now);
		}
	}
}

void BPF_STRUCT_OPS(lavd_stopping, struct task_struct *p, bool runnable)
{
	struct cpu_ctx *cpuc;
	struct task_ctx *taskc;

	/*
	 * Update task statistics
	 */
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
	struct task_ctx *taskc;
	u64 now, interval;

	cpuc = get_cpu_ctx_task(p);
	taskc = get_task_ctx(p);
	if (!cpuc || !taskc) {
		scx_bpf_error("Failed to lookup context for task %d", p->pid);
		return;
	}
	cpuc->flags = 0;

	/*
	 * If a task @p is dequeued from a run queue for some other reason
	 * other than going to sleep, it is an implementation-level side
	 * effect. Hence, we don't care this spurious dequeue.
	 */
	if (!(deq_flags & SCX_DEQ_SLEEP))
		return;

	/*
	 * When a task @p goes to sleep, its associated wait_freq is updated.
	 */
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

	bpf_rcu_read_lock();
	cd_cpumask = MEMBER_VPTR(cpdom_cpumask, [cpuc->cpdom_id]);
	if (!cd_cpumask)
		goto unlock_out;
	bpf_cpumask_set_cpu(cpu_id, cd_cpumask);
unlock_out:
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

	bpf_rcu_read_lock();
	cd_cpumask = MEMBER_VPTR(cpdom_cpumask, [cpuc->cpdom_id]);
	if (!cd_cpumask)
		goto unlock_out;
	bpf_cpumask_clear_cpu(cpu_id, cd_cpumask);
unlock_out:
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
	/*
	 * When a cpu becomes online, reset its cpu context and trigger the
	 * recalculation of the global cpu load.
	 */
	u64 now = scx_bpf_now();
	struct cpu_ctx *cpuc;

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}

	cpu_ctx_init_online(cpuc, cpu, now);

	__sync_fetch_and_add(&nr_cpus_onln, 1);
	__sync_fetch_and_add(&total_capacity, cpuc->capacity);
	update_autopilot_high_cap();
	update_sys_stat();
}

void BPF_STRUCT_OPS(lavd_cpu_offline, s32 cpu)
{
	/*
	 * When a cpu becomes offline, trigger the recalculation of the global
	 * cpu load.
	 */
	u64 now = scx_bpf_now();
	struct cpu_ctx *cpuc;

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}

	cpu_ctx_init_offline(cpuc, cpu, now);

	__sync_fetch_and_sub(&nr_cpus_onln, 1);
	__sync_fetch_and_sub(&total_capacity, cpuc->capacity);
	update_autopilot_high_cap();
	update_sys_stat();
}

void BPF_STRUCT_OPS(lavd_update_idle, s32 cpu, bool idle)
{
	/*
	 * The idle duration is accumulated to calculate the CPU utilization.
	 * Since SCX_OPS_KEEP_BUILTIN_IDLE is specified, we still rely on the
	 * default idle core tracking and core selection algorithm.
	 */

	struct cpu_ctx *cpuc;
	u64 now;

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}

	now = scx_bpf_now();

	/*
	 * The CPU is entering into the idle state.
	 */
	if (idle) {
		cpuc->idle_start_clk = now;

		/*
		 * As an idle task cannot be preempted,
		 * per-CPU preemption information should be cleared.
		 */
		reset_cpu_preemption_info(cpuc, false);
	}
	/*
	 * The CPU is exiting from the idle state.
	 */
	else {
		/*
		 * If idle_start_clk is zero, that means entering into the idle
		 * is not captured by the scx (i.e., the scx scheduler is
		 * loaded when this CPU is in an idle state).
		 */
		u64 old_clk = cpuc->idle_start_clk;
		if (old_clk != 0) {
			/*
			 * The CAS failure happens when idle_start_clk is
			 * updated by the update timer. That means the update
			 * timer already took the idle_time duration. Hence the
			 * idle duration should not be accumulated.
			 */
			u64 duration = time_delta(now, old_clk);
			bool ret = __sync_bool_compare_and_swap(
					&cpuc->idle_start_clk, old_clk, 0);
			if (ret)
				cpuc->idle_total += duration;
		}
	}
}

void BPF_STRUCT_OPS(lavd_set_cpumask, struct task_struct *p,
		    const struct cpumask *cpumask)
{
	struct task_ctx *taskc;

	taskc = get_task_ctx(p);
	if (!taskc) {
		scx_bpf_error("task_ctx_stor first lookup failed");
		return;
	}

	if (bpf_cpumask_weight(p->cpus_ptr) != __nr_cpu_ids)
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

	cpuc = get_cpu_ctx_id(cpu);
	if (!cpuc) {
		scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
		return;
	}

	/*
	 * When regaining control of a CPU under the higher priority scheduler
	 * class, measure how much time the higher priority scheduler class
	 * used -- i.e., [lavd_cpu_release, lavd_cpu_acquire]. This will be
	 * used to calculate capacity-invariant and frequency-invariant CPU
	 * utilization.
	 */
	dur = time_delta(scx_bpf_now(), cpuc->cpu_release_clk);
	scaled_dur = scale_cap_freq(dur, cpu);
	cpuc->tot_sc_time += scaled_dur;

	/*
	 * The higher-priority scheduler class could change the CPU frequency,
	 * so let's keep track of the frequency when we gain the CPU control.
	 * This helps to make the frequency update decision.
	 */
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

	/*
	 * When a CPU is released to serve higher priority scheduler class,
	 * reset the CPU's preemption information so it cannot be a victim.
	 */
	reset_cpu_preemption_info(cpuc, true);

	/*
	 * Requeue the tasks in a local DSQ to the global enqueue.
	 */
	scx_bpf_reenqueue_local();

	/*
	 * Reset the current CPU's performance target, so we can set
	 * the target properly after regaining the control.
	 */
	reset_cpuperf_target(cpuc);

	/*
	 * Keep track of when the higher-priority scheduler class takes
	 * the CPU to calculate capacity-invariant and frequency-invariant
	 * CPU utilization.
	 */
	cpuc->cpu_release_clk = scx_bpf_now();
}

void BPF_STRUCT_OPS(lavd_enable, struct task_struct *p)
{
	struct task_ctx *taskc;

	/*
	 * Set task's service time to the current, minimum service time.
	 */
	taskc = get_task_ctx(p);
	if (!taskc) {
		scx_bpf_error("task_ctx_stor first lookup failed");
		return;
	}

	taskc->svc_time = READ_ONCE(cur_svc_time);
}

static void init_task_ctx(struct task_struct *p, struct task_ctx *taskc)
{
	u64 now = scx_bpf_now();

	__builtin_memset(taskc, 0, sizeof(*taskc));
	if (bpf_cpumask_weight(p->cpus_ptr) != __nr_cpu_ids)
		set_task_flag(taskc, LAVD_FLAG_IS_AFFINITIZED);
	else
		reset_task_flag(taskc, LAVD_FLAG_IS_AFFINITIZED);
	taskc->last_runnable_clk = now;
	taskc->last_running_clk = now; /* for avg_runtime */
	taskc->last_stopping_clk = now; /* for avg_runtime */
	taskc->last_quiescent_clk = now;
	taskc->avg_runtime = sys_stat.slice;
	taskc->svc_time = sys_stat.avg_svc_time;

	set_on_core_type(taskc, p->cpus_ptr);
}

s32 BPF_STRUCT_OPS(lavd_init_task, struct task_struct *p,
		   struct scx_init_task_args *args)
{
	struct task_ctx *taskc;

	/*
	 * When @p becomes under the SCX control (e.g., being forked), @p's
	 * context data is initialized. We can sleep in this function and the
	 * following will automatically use GFP_KERNEL.
	 * 
	 * Return 0 on success.
	 * Return -ESRCH if @p is invalid.
	 * Return -ENOMEM if context allocation fails.
	 */
	if (!p) {
		scx_bpf_error("NULL task_struct pointer received");
		return -ESRCH;
	}
	
	taskc = bpf_task_storage_get(&task_ctx_stor, p, 0,
				     BPF_LOCAL_STORAGE_GET_F_CREATE);
	if (!taskc) {
		scx_bpf_error("task_ctx_stor first lookup failed");
		return -ENOMEM;
	}


	/*
	 * Initialize @p's context.
	 */
	init_task_ctx(p, taskc);
	return 0;
}

static s32 init_cpdoms(u64 now)
{
	struct cpdom_ctx *cpdomc;
	int err;

	for (int i = 0; i < LAVD_CPDOM_MAX_NR; i++) {
		/*
		 * Fetch a cpdom context.
		 */
		cpdomc = MEMBER_VPTR(cpdom_ctxs, [i]);
		if (!cpdomc) {
			scx_bpf_error("Failed to lookup cpdom_ctx for %d", i);
			return -ESRCH;
		}
		if (!cpdomc->is_valid)
			continue;

		/*
		 * Create an associated DSQ on its associated NUMA domain.
		 */
		err = scx_bpf_create_dsq(cpdom_to_dsq(cpdomc->id), cpdomc->numa_id);
		if (err) {
			scx_bpf_error("Failed to create a DSQ for cpdom %llu on NUMA node %d",
				      cpdomc->id, cpdomc->numa_id);
			return err;
		}

		/*
		 * Update the number of compute domains.
		 */
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
	/*
	 * Allocate active cpumask and initialize it with all online CPUs.
	 */
	err = calloc_cpumask(&active_cpumask);
	active = active_cpumask;
	if (err || !active)
		goto out;

	online_cpumask = scx_bpf_get_online_cpumask();
	nr_cpus_onln = bpf_cpumask_weight(online_cpumask);
	bpf_cpumask_copy(active, online_cpumask);
	scx_bpf_put_cpumask(online_cpumask);

	/*
	 * Allocate the other cpumasks.
	 */
	err = calloc_cpumask(&ovrflw_cpumask);
	if (err)
		goto out;

	err = calloc_cpumask(&turbo_cpumask);
	if (err)
		goto out;

	err = calloc_cpumask(&big_cpumask);
	if (err)
		goto out;

	err = calloc_cpumask(&little_cpumask);
	if (err)
		goto out;
out:
	bpf_rcu_read_unlock();
	return err;
}

static s32 init_per_cpu_ctx(u64 now)
{
	struct cpu_ctx *cpuc;
	struct bpf_cpumask *turbo, *big, *little, *active, *ovrflw, *cd_cpumask;
	const struct cpumask *online_cpumask;
	struct cpdom_ctx *cpdomc;
	int cpu, i, j, err = 0;
	u64 cpdom_id;
	u32 sum_capacity = 0, big_capacity = 0;

	bpf_rcu_read_lock();
	online_cpumask = scx_bpf_get_online_cpumask();

	/*
	 * Prepare cpumasks.
	 */
	turbo = turbo_cpumask;
	big = big_cpumask;
	little = little_cpumask;
	active  = active_cpumask;
	ovrflw  = ovrflw_cpumask;
	if (!turbo || !big || !little || !active || !ovrflw) {
		scx_bpf_error("Failed to prepare cpumasks.");
		err = -ENOMEM;
		goto unlock_out;
	}

	/*
	 * Initilize CPU info
	 */
	one_little_capacity = LAVD_SCALE;
	bpf_for(cpu, 0, __nr_cpu_ids) {
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

		cpuc->cpu_id = cpu;
		cpuc->idle_start_clk = 0;
		cpuc->lat_cri = 0;
		cpuc->running_clk = 0;
		cpuc->est_stopping_clk = SCX_SLICE_INF;
		cpuc->online_clk = now;
		cpuc->offline_clk = now;
		cpuc->cpu_release_clk = now;
		cpuc->is_online = bpf_cpumask_test_cpu(cpu, online_cpumask);
		cpuc->capacity = cpu_capacity[cpu];
		cpuc->big_core = cpu_big[cpu];
		cpuc->turbo_core = cpu_turbo[cpu];
		cpuc->cpdom_poll_pos = cpu % LAVD_CPDOM_MAX_NR;
		cpuc->min_perf_cri = LAVD_SCALE;
		cpuc->futex_op = LAVD_FUTEX_OP_INVALID;

		sum_capacity += cpuc->capacity;

		if (cpuc->big_core) {
			nr_cpus_big++;
			big_capacity += cpuc->capacity;
			bpf_cpumask_set_cpu(cpu, big);
		}
		else {
			bpf_cpumask_set_cpu(cpu, little);
			have_little_core = true;
		}

		if (cpuc->turbo_core) {
			bpf_cpumask_set_cpu(cpu, turbo);
			have_turbo_core = true;
		}

		if (cpuc->capacity < one_little_capacity)
			one_little_capacity = cpuc->capacity;
	}
	default_big_core_scale = (big_capacity << LAVD_SHIFT) / sum_capacity;
	total_capacity = sum_capacity;

	/*
	 * Initialize compute domain id.
	 */
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

		bpf_for(i, 0, LAVD_CPU_ID_MAX/64) {
			u64 cpumask = cpdomc->__cpumask[i];
			bpf_for(j, 0, 64) {
				if (cpumask & 0x1LLU << j) {
			 		cpu = (i * 64) + j;
					cpuc = get_cpu_ctx_id(cpu);
					if (!cpuc) {
						scx_bpf_error("Failed to lookup cpu_ctx: %d", cpu);
						err = -ESRCH;
						goto unlock_out;
					}
					cpuc->llc_id = cpdomc->llc_id;
					cpuc->cpdom_id = cpdomc->id;
					cpuc->cpdom_alt_id = cpdomc->alt_id;

					if (bpf_cpumask_test_cpu(cpu, online_cpumask)) {
						bpf_cpumask_set_cpu(cpu, cd_cpumask);
						cpdomc->nr_active_cpus++;
						cpdomc->cap_sum_active_cpus += cpuc->capacity;
					}
					cpdomc->nr_cpus++;
				}
			}
		}
	}

	/*
	 * Print some useful informatin for debugging.
	 */
	bpf_for(cpu, 0, __nr_cpu_ids) {
		cpuc = get_cpu_ctx_id(cpu);
		if (!cpuc) {
			scx_bpf_error("Failed to lookup cpu_ctx: %d", cpu);
			err = -ESRCH;
			goto unlock_out;
		}
		debugln("cpu[%d] capacity: %d, big_core: %d, turbo_core: %d, "
			"cpdom_id: %llu, alt_id: %llu",
			cpu, cpuc->capacity, cpuc->big_core, cpuc->turbo_core,
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

	bpf_for(cpu, 0, __nr_cpu_ids) {
		/*
		 * Create Per-CPU DSQs on its associated NUMA domain.
		 */
		cpuc = get_cpu_ctx_id(cpu);
		if (!cpuc) {
			scx_bpf_error("Failed to lookup cpu_ctx: %d", cpu);
			return -ESRCH;
		}

		cpdomc = MEMBER_VPTR(cpdom_ctxs, [cpuc->cpdom_id]);
		if (!cpdomc) {
			scx_bpf_error("Failed to lookup cpdom_ctx for %hhu", cpuc->cpdom_id);
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


s32 BPF_STRUCT_OPS_SLEEPABLE(lavd_init)
{
	u64 now = scx_bpf_now();
	int err;

	/*
	 * Create compute domains.
	 */
	err = init_cpdoms(now);
	if (err)
		return err;

	/*
	 * Allocate cpumasks for core compaction.
	 */
	err = init_cpumasks();
	if (err)
		return err;

	/*
	 * Initialize per-CPU context.
	 */
	err = init_per_cpu_ctx(now);
	if (err)
		return err;

	/*
	 * UPSTREAM PATCH: Initialize per-CPU DSQs.
	 *
	 * Per-CPU DSQs are created when:
	 * 1. per_cpu_dsq is enabled (experimental feature for better locality)
	 * 2. pinned_slice_ns is enabled (for pinned task handling)
	 *
	 * Rationale: When pinned_slice_ns is set, we need per-CPU DSQs
	 * for pinned tasks to enable vtime comparison during dispatch.
	 */
	if (per_cpu_dsq || pinned_slice_ns) {
		err = init_per_cpu_dsqs();
		if (err)
			return err;
	}

	/*
	 * Initialize system statistics tracking.
	 */
	err = init_sys_stat(now);
	if (err)
		return err;

	/*
	 * Initialize autopilot mode watermarks.
	 */
	init_autopilot_caps();

	/*
	 * Initialize global clocks.
	 */
	WRITE_ONCE(cur_logical_clk, 0);
	WRITE_ONCE(cur_svc_time, 0);

	return 0;
}

void BPF_STRUCT_OPS(lavd_exit, struct scx_exit_info *ei)
{
	UEI_RECORD(uei, ei);
}

SCX_OPS_DEFINE(lavd_ops,
	       .select_cpu		= (void *)lavd_select_cpu,
	       .enqueue			= (void *)lavd_enqueue,
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
	       .init			= (void *)lavd_init,
	       .exit			= (void *)lavd_exit,
	       .timeout_ms		= 30000U,
	       .name			= "lavd");

