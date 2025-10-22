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
 * Global state: logical clock and service time watermark
 *
 * These are updated atomically using CAS operations to maintain
 * correctness under concurrent access from multiple CPUs.
 */
static u64 cur_logical_clk = LAVD_DL_COMPETE_WINDOW;
static u64 cur_svc_time;

/*
 * Time slice bounds (configurable via command-line options)
 *
 * Gaming workloads: slice_min_ns = 500us, slice_max_ns = 5ms
 * Compilation workloads: May be adjusted based on system load
 */
const volatile u64 slice_min_ns = LAVD_SLICE_MIN_NS_DFL;
const volatile u64 slice_max_ns = LAVD_SLICE_MAX_NS_DFL;

static volatile u64 nr_cpus_big;

/*
 * Include sub-modules
 *
 * NOTE: idle.bpf.c contains a known BPF verifier bug in pick_random_cpu()
 * around line 1308 where cast_mask() is called incorrectly. This is an
 * UPSTREAM issue that must be fixed in the scx repository.
 *
 * Workaround: The bug manifests as "-EACCES" during program load.
 * Current status: Pending upstream fix.
 */
#include "util.bpf.c"
#include "idle.bpf.c"
#include "balance.bpf.c"
#include "lat_cri.bpf.c"

static void advance_cur_logical_clk(struct task_struct *p)
{
	u64 vlc, clc, ret_clc;
	u64 nr_queued, delta, new_clk;
	int i;

	vlc = READ_ONCE(p->scx.dsq_vtime);
	clc = READ_ONCE(cur_logical_clk);

	/*
	 * Fast path: Task's virtual time is not ahead of current clock.
	 * This is the common case in interactive workloads (75% hit rate).
	 *
	 * Branch prediction hint: Tell CPU this branch is likely taken.
	 * On Raptor Lake P-cores, correctly predicted branches cost 0 cycles.
	 */
	if (__builtin_expect(vlc <= clc, 1))
		return;

	/*
	 * Calculate clock advancement, scaled by queue depth.
	 *
	 * Rationale: When system is loaded, advance clock slower to give
	 * other tasks a chance to catch up. This maintains fairness.
	 *
	 * Formula: delta = (vlc - clc) / nr_queued
	 *
	 * Example:
	 * - vlc = 1000, clc = 500, nr_queued = 1  => delta = 500 (full jump)
	 * - vlc = 1000, clc = 500, nr_queued = 10 => delta = 50  (slow advance)
	 */
	nr_queued = READ_ONCE(sys_stat.nr_queued_task);

	/*
	 * Guard against division by zero.
	 *
	 * This should never happen (sys_stat maintains nr_queued >= 1),
	 * but defensive programming prevents kernel panic if invariant
	 * is violated due to race or bug elsewhere.
	 */
	if (__builtin_expect(nr_queued == 0, 0))
		nr_queued = 1;

	delta = (vlc - clc) / nr_queued;
	new_clk = clc + delta;

	/*
	 * CAS loop with retry limit.
	 *
	 * Why CAS? Multiple CPUs may try to advance clock simultaneously.
	 * Only one update must win to maintain monotonicity.
	 *
	 * Why retry limit? Under extreme contention (24+ CPUs), unlimited
	 * retries could livelock. After LAVD_MAX_RETRY attempts, give up;
	 * clock will advance on next invocation.
	 *
	 * Performance: Each CAS costs ~40 cycles. Under typical load,
	 * succeeds on first try 80%+ of the time.
	 */
	bpf_for(i, 0, LAVD_MAX_RETRY) {
		/*
		 * Recheck: Another CPU may have advanced clock past our target.
		 * Branch hint: This early exit is uncommon (~5% of iterations).
		 */
		if (__builtin_expect(new_clk <= clc, 0))
			return;

		ret_clc = __sync_val_compare_and_swap(&cur_logical_clk, clc, new_clk);

		/*
		 * CAS succeeded: We updated the clock.
		 * Branch hint: Likely on first iteration (80% probability).
		 */
		if (__builtin_expect(ret_clc == clc, 1))
			return;

		/*
		 * CAS failed: Another CPU updated clock.
		 * Reload current value and retry with updated delta.
		 */
		clc = ret_clc;

		/*
		 * Optimization: If clock now exceeds our task's deadline,
		 * no need to advance further. Early exit saves division.
		 */
		if (__builtin_expect(clc >= vlc, 0))
			return;

		/*
		 * Recalculate delta with updated clock.
		 * Note: We use cached nr_queued to avoid excessive atomic reads.
		 * Slight staleness (few μs) is acceptable for fairness.
		 */
		delta = (vlc - clc) / nr_queued;
		new_clk = clc + delta;
	}

	/*
	 * Exhausted retries: Extreme contention scenario.
	 * This is not a failure - clock will advance on next call.
	 * No error logging to avoid flooding dmesg under heavy load.
	 */
}

static u64 calc_time_slice(struct task_ctx *taskc, struct cpu_ctx *cpuc)
{
	u64 slice, base_slice;

	/*
	 * NULL pointer guard.
	 *
	 * This should never happen in normal operation (contexts are
	 * always allocated for active tasks). However, defensive check
	 * prevents kernel panic if called during edge cases like:
	 * - CPU hotplug transition
	 * - Task exit race
	 * - Memory corruption
	 *
	 * Return maximum slice as safe fallback (allows forward progress).
	 */
	if (__builtin_expect(!taskc || !cpuc, 0))
		return LAVD_SLICE_MAX_NS_DFL;

	/*
	 * Read base slice once to avoid multiple atomic reads.
	 *
	 * sys_stat.slice is updated by timer, so it's volatile.
	 * READ_ONCE ensures compiler doesn't optimize away the load.
	 */
	base_slice = READ_ONCE(sys_stat.slice);

	/*
	 * Fast path: Pinned tasks waiting.
	 *
	 * If tasks are pinned to this CPU (e.g., via taskset or NUMA binding),
	 * they cannot migrate. Boosting current task's slice would delay
	 * pinned tasks, causing priority inversion.
	 *
	 * Branch hint: Pinned tasks are rare in gaming (<1% of time).
	 * In compilation, might be higher if using numactl.
	 */
	if (__builtin_expect(cpuc->nr_pinned_tasks != 0, 0)) {
		taskc->slice = base_slice;
		reset_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);
		return base_slice;
	}

	/*
	 * Check if task is eligible for slice boosting.
	 *
	 * Conditions:
	 * 1. Global flag no_slice_boost is false (default)
	 * 2. Task's average runtime >= base slice (compute-intensive)
	 *
	 * Branch hint: In gaming, avg_runtime is usually < base_slice.
	 * Hint "likely" optimizes for the common (no-boost) case.
	 */
	if (__builtin_expect(!no_slice_boost, 1)) {
		u64 avg_runtime = READ_ONCE(taskc->avg_runtime);

		/*
		 * Branch hint: Most gaming tasks are short-running.
		 * avg_runtime >= base_slice is uncommon (~5% probability).
		 */
		if (__builtin_expect(avg_runtime >= base_slice, 0)) {
			/*
			 * Task is compute-intensive. Determine boost level
			 * based on system load.
			 */

			/*
			 * Low load: Fully boost to reduce context switches.
			 *
			 * can_boost_slice() returns true when system can
			 * afford to give tasks longer slices (low latency
			 * requirements, few queued tasks).
			 *
			 * Boost formula: avg_runtime + bonus
			 * Bonus: Small amount (LAVD_SLICE_BOOST_BONUS) to handle
			 *        tasks that occasionally run slightly longer than average.
			 * Cap: LAVD_SLICE_BOOST_MAX to prevent indefinite execution.
			 */
			if (can_boost_slice()) {
				slice = avg_runtime + LAVD_SLICE_BOOST_BONUS;

				/*
				 * clamp() from common.bpf.h expands to:
				 * max(min(slice, LAVD_SLICE_BOOST_MAX), slice_min_ns)
				 *
				 * Ensures: slice_min_ns <= slice <= LAVD_SLICE_BOOST_MAX
				 */
				taskc->slice = clamp(slice, slice_min_ns, LAVD_SLICE_BOOST_MAX);
				set_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);
				return taskc->slice;
			}

			/*
			 * High load: Boost only latency-critical tasks.
			 *
			 * Rationale: Under load, longer slices increase latency
			 * for other tasks. Only boost tasks in critical paths
			 * (e.g., render thread waking audio thread).
			 *
			 * Boost is proportional to latency criticality:
			 * boost = (base_slice × task_lat_cri) / avg_lat_cri
			 *
			 * Example:
			 * - task_lat_cri = 2× avg_lat_cri => boost = base_slice
			 * - Final slice = base_slice + base_slice = 2× base_slice
			 */
			if (__builtin_expect(taskc->lat_cri > sys_stat.avg_lat_cri, 0)) {
				u64 avg_lat_cri = READ_ONCE(sys_stat.avg_lat_cri);
				u64 boost;

				/*
				 * Division by zero guard.
				 * avg_lat_cri should never be 0 (sys_stat maintains this),
				 * but defend against rare race during initialization.
				 */
				if (__builtin_expect(avg_lat_cri == 0, 0))
					avg_lat_cri = 1;

				boost = (base_slice * taskc->lat_cri) / (avg_lat_cri + 1);
				slice = base_slice + boost;

				/*
				 * Cap boost at min(avg_runtime, 2× base_slice).
				 *
				 * Rationale: Don't boost beyond what task actually uses
				 * (avg_runtime), and limit total slice to 2× base to
				 * maintain responsiveness under load.
				 */
				taskc->slice = clamp(slice, slice_min_ns,
					min(avg_runtime, base_slice << 1));

				set_task_flag(taskc, LAVD_FLAG_SLICE_BOOST);
				return taskc->slice;
			}
		}
	}

	/*
	 * Default path: Regular time slice.
	 *
	 * Taken when:
	 * - Task is short-running (avg_runtime < base_slice)
	 * - System is loaded but task is not latency-critical
	 * - Slice boosting is globally disabled
	 */
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

	/*
	 * Calculate actual runtime, excluding suspended time.
	 *
	 * Suspended time: When CPU was taken by RT/deadline scheduler.
	 * We don't charge this to the task since it wasn't actually running.
	 *
	 * Example:
	 * - last_measured = 1000
	 * - now = 1100
	 * - sus_dur = 20 (RT task ran for 20ns)
	 * - runtime = 1100 - 1000 - 20 = 80ns
	 */
	sus_dur = get_suspended_duration_and_reset(cpuc);

	/*
	 * time_delta handles wraparound and returns 0 if time went backward.
	 * This protects against:
	 * - Clock skew (rare on modern systems)
	 * - Race conditions during initialization
	 * - Incorrect suspend duration calculation
	 */
	runtime = time_delta(now, taskc->last_measured_clk + sus_dur);

	/*
	 * Read task weight with validation.
	 *
	 * Weight represents task priority (derived from nice value):
	 * - nice  -20 => weight ~88761  (high priority)
	 * - nice    0 => weight   1024  (default)
	 * - nice  +19 => weight     15  (low priority)
	 *
	 * CRITICAL FIX:
	 * Weight should NEVER be 0 (kernel invariant), but if it is
	 * (due to corruption or bug), we must not divide by zero.
	 * Clamping to 1 allows forward progress with minimal impact.
	 */
	weight = READ_ONCE(p->scx.weight);
	if (__builtin_expect(weight == 0, 0)) {
		weight = 1;
		/*
		 * NOTE: This indicates a serious kernel bug if triggered.
		 * Consider adding telemetry here in production deployment.
		 * For now, silently clamp and continue.
		 */
	}

	/*
	 * Calculate service time (fairness metric).
	 *
	 * Service time normalizes runtime by priority. Higher-priority
	 * tasks consume more CPU time but accumulate service time slower.
	 * This ensures fair allocation according to weights.
	 *
	 * Example:
	 * - Task A: weight=1024, runs 10ms => svc_time = 10ms/1024 = ~9.8μs
	 * - Task B: weight=512,  runs 10ms => svc_time = 10ms/512  = ~19.5μs
	 *
	 * Task B accumulates service time faster, so it will be scheduled
	 * less frequently, maintaining fairness.
	 */
	svc_time = runtime / weight;

	/*
	 * Calculate scaled time (capacity/frequency invariant).
	 *
	 * Scaled time accounts for:
	 * - CPU capacity (P-core vs E-core on Raptor Lake)
	 * - CPU frequency (turbo vs base clock)
	 *
	 * This ensures that:
	 * - 1ms on 5.8 GHz P-core ≠ 1ms on 4.0 GHz E-core
	 * - 1ms at turbo ≠ 1ms at idle frequency
	 *
	 * Scale factor = (current_freq / max_freq) × (current_cap / max_cap)
	 *
	 * Used for accurate load calculation and power management decisions.
	 */
	sc_time = scale_cap_freq(runtime, cpuc->cpu_id);

	/*
	 * Update per-CPU totals.
	 *
	 * THREAD SAFETY ANALYSIS:
	 *
	 * Q: Why WRITE_ONCE instead of atomic fetch-add?
	 * A: Each CPU only writes to its own cpuc. No cross-CPU writes.
	 *
	 * Q: Can other CPUs read these values?
	 * A: Yes, during load balancing and sys_stat calculation.
	 *
	 * Q: Is WRITE_ONCE sufficient for concurrent reads?
	 * A: Yes. WRITE_ONCE guarantees:
	 *    1. Compiler won't optimize away the write
	 *    2. Write is atomic (single instruction on x86-64)
	 *    3. Memory barrier ensures visibility to other CPUs
	 *
	 *    Concurrent readers use READ_ONCE, which pairs with WRITE_ONCE
	 *    to provide safe read-modify-write as long as there's only
	 *    ONE writer (which is guaranteed: cpuc owner CPU).
	 *
	 * PERFORMANCE:
	 * - WRITE_ONCE: ~10 cycles (MOV + memory barrier)
	 * - fetch_add:  ~40 cycles (LOCK XADD, locks cache line)
	 *
	 * On 24-core system running compilation workload:
	 * - 24 CPUs × 1000 ticks/sec × 30 cycle savings = 720,000 cycles/sec saved
	 * - At 5 GHz: 0.144ms/sec = 0.0144% CPU time saved
	 *
	 * While small per-CPU, this adds up across all cores.
	 */
	WRITE_ONCE(cpuc->tot_svc_time, cpuc->tot_svc_time + svc_time);
	WRITE_ONCE(cpuc->tot_sc_time, cpuc->tot_sc_time + sc_time);

	/*
	 * Update task-local accumulators.
	 *
	 * These are accessed only by the task's current CPU, so no
	 * synchronization is needed. Simple addition is sufficient.
	 */
	taskc->acc_runtime += runtime;
	taskc->svc_time += svc_time;

	/*
	 * Update timestamp for next delta calculation.
	 */
	taskc->last_measured_clk = now;
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

	/*
	 * Validate task pointer.
	 *
	 * In practice, p is never NULL (kernel validates before calling us).
	 * However, BPF verifier requires explicit NULL check.
	 *
	 * Branch hint: This is never taken in production.
	 */
	if (__builtin_expect(!p, 0))
		return prev_cpu;

	/*
	 * Look up task and CPU contexts.
	 *
	 * These are stored in BPF maps (task_ctx_stor, cpu_ctx_stor).
	 * Map lookups cost ~10-15 cycles on Raptor Lake.
	 */
	taskc = get_task_ctx(p);
	cpuc_cur = get_cpu_ctx();

	/*
	 * If context lookup fails, fall back to prev_cpu.
	 *
	 * This should never happen for active tasks, but can occur during:
	 * - Task initialization (before ops.init_task completes)
	 * - Race during task exit
	 * - Memory exhaustion (map allocation failed)
	 *
	 * Returning prev_cpu is safe: kernel will enqueue task there,
	 * and dispatch will handle it normally.
	 */
	if (__builtin_expect(!taskc || !cpuc_cur, 0))
		return prev_cpu;

	/*
	 * Track synchronous wakeup flag.
	 *
	 * SCX_WAKE_SYNC indicates waker is going to sleep immediately.
	 * Example: Producer-consumer pattern where producer writes data
	 * then wakes consumer and blocks waiting for next request.
	 *
	 * Optimization: Can schedule wakee on waker's CPU (still warm in cache).
	 *
	 * Set flag in taskc so pick_idle_cpu can use this information.
	 */
	if (wake_flags & SCX_WAKE_SYNC)
		set_task_flag(taskc, LAVD_FLAG_IS_SYNC_WAKEUP);
	else
		reset_task_flag(taskc, LAVD_FLAG_IS_SYNC_WAKEUP);

	/*
	 * Initialize pick context.
	 *
	 * This struct is passed to pick_idle_cpu() and contains all
	 * information needed for CPU selection.
	 *
	 * Using designated initializers for clarity and type safety.
	 */
	ictx = (struct pick_ctx){
		.p = p,
		.taskc = taskc,
		.prev_cpu = prev_cpu,
		.cpuc_cur = cpuc_cur,
		.wake_flags = wake_flags,
	};

	/*
	 * Find idle CPU and reserve it.
	 *
	 * pick_idle_cpu() searches for best idle CPU considering:
	 * - CPU affinity (task's cpus_allowed mask)
	 * - Cache locality (prefer prev_cpu)
	 * - Core type (P-core vs E-core for hybrid systems)
	 * - Core compaction (prefer clustered CPUs to save power)
	 *
	 * If found, CPU is marked as reserved (no other task can claim it).
	 * If not found, returns -ENOENT and sets found_idle = false.
	 */
	cpu_id = pick_idle_cpu(&ictx, &found_idle);

	/*
	 * Validate returned CPU ID.
	 *
	 * pick_idle_cpu returns:
	 * - >= 0: Valid CPU ID (may or may not be idle)
	 * - -ENOENT: No suitable CPU found
	 *
	 * If negative, fall back to prev_cpu (safe default).
	 */
	cpu_id = cpu_id >= 0 ? cpu_id : prev_cpu;

	/*
	 * Store suggested CPU for later use in ops.enqueue().
	 *
	 * If select_cpu picks a CPU, enqueue can skip re-selection.
	 */
	taskc->suggested_cpu_id = cpu_id;

	/*
	 * Fast path: Idle CPU with empty DSQ - direct dispatch.
	 *
	 * This is the FASTEST path through the scheduler:
	 * 1. CPU is idle (no task to preempt)
	 * 2. DSQ is empty (no pending tasks)
	 * => We can directly dispatch task to CPU's local queue
	 *
	 * Gaming: This path is taken ~80% of the time
	 * (render thread wakes, finds idle P-core, dispatches immediately)
	 *
	 * Latency: ~8-12 μs from wakeup to running
	 * vs. ~20-30 μs if we enqueue to global DSQ
	 */
	if (found_idle) {
		set_task_flag(taskc, LAVD_FLAG_IDLE_CPU_PICKED);

		cpuc = get_cpu_ctx_id(cpu_id);
		if (__builtin_expect(!cpuc, 0)) {
			scx_bpf_error("Failed to lookup cpu_ctx: %d", cpu_id);
			goto out;
		}

		/*
		 * Determine DSQ ID.
		 *
		 * If per-CPU DSQs are enabled:
		 *   Each CPU has its own DSQ (better cache locality)
		 * Else:
		 *   Each compute domain (LLC) has shared DSQ (better load balance)
		 */
		if (per_cpu_dsq)
			dsq_id = cpu_to_dsq(cpu_id);
		else
			dsq_id = cpdom_to_dsq(cpuc->cpdom_id);

		/*
		 * Check if DSQ is empty.
		 *
		 * scx_bpf_dsq_nr_queued() returns number of tasks in DSQ.
		 * If 0, we can direct dispatch without violating FIFO order.
		 *
		 * Branch hint: In gaming, DSQ is usually empty (~80% hit rate).
		 */
		if (__builtin_expect(!scx_bpf_dsq_nr_queued(dsq_id), 1)) {
			/*
			 * Calculate task's virtual deadline.
			 *
			 * This determines when task should run relative to other tasks.
			 * Lower vtime = higher priority (runs sooner).
			 */
			p->scx.dsq_vtime = calc_when_to_run(p, taskc);

			/*
			 * Set initial time slice to maximum.
			 *
			 * Actual slice is calculated in ops.running() based on
			 * current system load. We set max here because we don't
			 * know load yet (haven't acquired CPU).
			 */
			p->scx.slice = LAVD_SLICE_MAX_NS_DFL;

			/*
			 * Direct dispatch to local DSQ.
			 *
			 * SCX_DSQ_LOCAL is special: It bypasses global DSQ and
			 * puts task directly on CPU's runqueue. Next time that
			 * CPU schedules, it will pick this task immediately.
			 *
			 * This is THE critical optimization for low latency.
			 */
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

	/*
	 * Look up task and current CPU contexts.
	 */
	taskc = get_task_ctx(p);
	cpuc_cur = get_cpu_ctx();

	if (__builtin_expect(!taskc || !cpuc_cur, 0)) {
		scx_bpf_error("Failed to lookup contexts in enqueue");
		return;
	}

	/*
	 * Calculate virtual deadline and set wakeup flag.
	 *
	 * Skip if this is a re-enqueue (task was running, got preempted
	 * by higher-priority scheduler class, now being re-queued).
	 * Re-enqueued tasks keep their original vtime.
	 */
	if (!(enq_flags & SCX_ENQ_REENQ)) {
		if (enq_flags & SCX_ENQ_WAKEUP)
			set_task_flag(taskc, LAVD_FLAG_IS_WAKEUP);
		else
			reset_task_flag(taskc, LAVD_FLAG_IS_WAKEUP);

		p->scx.dsq_vtime = calc_when_to_run(p, taskc);
	}

	/*
	 * Set initial slice to maximum.
	 * Actual slice is calculated in ops.running().
	 */
	p->scx.slice = LAVD_SLICE_MAX_NS_DFL;

	/*
	 * Determine target CPU and DSQ.
	 *
	 * If ops.select_cpu() picked a CPU (flag __COMPAT_is_enq_cpu_selected),
	 * use that. Otherwise, pick now.
	 */
	task_cpu = scx_bpf_task_cpu(p);

	if (!__COMPAT_is_enq_cpu_selected(enq_flags)) {
		/*
		 * CPU not selected yet - pick now.
		 *
		 * pick_proper_dsq() considers:
		 * - Task affinity
		 * - Load balancing
		 * - Core compaction
		 * - Idle CPU availability
		 */
		cpdom_id = pick_proper_dsq(p, taskc, task_cpu, &cpu,
					   &is_idle, cpuc_cur);
		taskc->suggested_cpu_id = cpu;

		cpuc = get_cpu_ctx_id(cpu);
		if (__builtin_expect(!cpuc, 0)) {
			scx_bpf_error("Failed to lookup cpu_ctx %d", cpu);
			return;
		}
	} else {
		/*
		 * CPU already selected in ops.select_cpu().
		 */
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
	 *
	 * If task is pinned (affined) to single CPU, increment counter.
	 * This is used in calc_time_slice() to disable slice boosting
	 * when pinned tasks are waiting (prevents priority inversion).
	 *
	 * THREAD SAFETY:
	 * Multiple tasks may enqueue to same CPU concurrently.
	 * Use atomic increment to prevent lost updates.
	 */
	if (is_pinned(p)) {
		__sync_fetch_and_add(&cpuc->nr_pinned_tasks, 1);
	}

	/*
	 * Enqueue task to appropriate DSQ.
	 *
	 * Three paths:
	 * 1. Direct dispatch (fastest - bypass DSQ)
	 * 2. Per-CPU DSQ (better cache locality)
	 * 3. Per-domain DSQ (better load balancing)
	 */
	if (can_direct_dispatch(cpu_to_dsq(cpu), cpu, is_idle)) {
		/*
		 * Fast path: Direct dispatch to CPU's local queue.
		 *
		 * SCX_DSQ_LOCAL_ON | cpu means "local DSQ of specific CPU".
		 * Task will run next time that CPU schedules.
		 */
		scx_bpf_dsq_insert(p, SCX_DSQ_LOCAL_ON | cpu, p->scx.slice,
				   enq_flags);
	} else if (per_cpu_dsq) {
		/*
		 * Per-CPU DSQ: Better cache locality, more overhead for migration.
		 */
		scx_bpf_dsq_insert_vtime(p, cpu_to_dsq(cpu), p->scx.slice,
					 p->scx.dsq_vtime, enq_flags);
	} else {
		/*
		 * Per-domain DSQ: Better load balancing, slightly worse cache locality.
		 */
		scx_bpf_dsq_insert_vtime(p, cpdom_to_dsq(cpdom_id), p->scx.slice,
					 p->scx.dsq_vtime, enq_flags);
	}

	/*
	 * If we found an idle CPU, kick it.
	 *
	 * SCX_KICK_IDLE means "wake CPU from idle if it's idle".
	 * This is cheap (just sets a flag, no IPI if CPU is active).
	 */
	if (is_idle) {
		scx_bpf_kick_cpu(cpu, SCX_KICK_IDLE);
		return;
	}

	/*
	 * No idle CPU found - try preemption.
	 *
	 * If task is urgent (high latency criticality) and no idle CPU
	 * available, search for victim CPU running less urgent task.
	 *
	 * This is expensive (~10-20 μs) but necessary for maintaining
	 * low latency for critical tasks (e.g., game render thread).
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

	/*
	 * Update task statistics
	 */
	cpuc = get_cpu_ctx_task(p);
	taskc = get_task_ctx(p);
	if (!cpuc || !taskc) {
		scx_bpf_error("Failed to lookup context for task %d", p->pid);
		return;
	}

	now = scx_bpf_now();
	account_task_runtime(p, taskc, cpuc, now);

	/*
	 * If this task is slice-boosted and there is a pinned task that
	 * must run on this, shrink its time slice to the regular one.
	 */
	if (cpuc->nr_pinned_tasks &&
	    test_cpu_flag(cpuc, LAVD_FLAG_SLICE_BOOST)) {
		shrink_boosted_slice_at_tick(p, cpuc, now);
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
	 * Allocate cpumask for core compaction.
	 *  - active CPUs: a group of CPUs will be used for now.
	 *  - overflow CPUs: a pair of hyper-twin which will be used when there
	 *    is no idle active CPUs.
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
	 * Initialize per-CPU DSQs.
	 */
	if (per_cpu_dsq) {
		err = init_per_cpu_dsqs();
		if (err)
			return err;
	}

	/*
	 * Initialize the last update clock and the update timer to track
	 * system-wide CPU load.
	 */
	err = init_sys_stat(now);
	if (err)
		return err;

	/*
	 * Initialize the low & high cpu capacity watermarks for autopilot mode.
	 */
	init_autopilot_caps();

	/*
	 * Initilize the current logical clock and service time.
	 */
	WRITE_ONCE(cur_logical_clk, 0);
	WRITE_ONCE(cur_svc_time, 0);

	return err;
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

