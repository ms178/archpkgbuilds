// SPDX-License-Identifier: GPL-2.0
//
// Copyright (c) 2024 Valve Corporation.
// Author: Changwoo Min <changwoo@igalia.com>

// This software may be used and distributed according to the terms of the
// GNU General Public License version 2.

mod bpf_skel;
pub use bpf_skel::*;
pub mod bpf_intf;
pub use bpf_intf::*;

mod cpu_order;
use scx_utils::init_libbpf_logging;
mod stats;
use std::ffi::c_int;
use std::ffi::CStr;
use std::mem::MaybeUninit;
use std::str;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::thread::ThreadId;
use std::time::Duration;

use anyhow::Context;
use anyhow::Result;
use clap::Parser;
use clap_num::number_range;
use cpu_order::CpuOrder;
use cpu_order::PerfCpuOrder;
use crossbeam::channel;
use crossbeam::channel::Receiver;
use crossbeam::channel::RecvTimeoutError;
use crossbeam::channel::Sender;
use crossbeam::channel::TrySendError;
use libbpf_rs::OpenObject;
use libbpf_rs::PrintLevel;
use libbpf_rs::ProgramInput;
use libc::c_char;
use log::debug;
use log::info;
use log::warn;
use plain::Plain;
use scx_stats::prelude::*;
use scx_utils::autopower::{fetch_power_profile, PowerProfile};
use scx_utils::build_id;
use scx_utils::compat;
use scx_utils::libbpf_clap_opts::LibbpfOpts;
use scx_utils::scx_ops_attach;
use scx_utils::scx_ops_load;
use scx_utils::scx_ops_open;
use scx_utils::try_set_rlimit_infinity;
use scx_utils::uei_exited;
use scx_utils::uei_report;
use scx_utils::EnergyModel;
use scx_utils::TopologyArgs;
use scx_utils::UserExitInfo;
use scx_utils::NR_CPU_IDS;
use stats::SchedSample;
use stats::SchedSamples;
use stats::StatsReq;
use stats::StatsRes;
use stats::SysStats;

const SCHEDULER_NAME: &str = "scx_lavd";

/// scx_lavd: Latency-criticality Aware Virtual Deadline (LAVD) scheduler
///
/// The rust part is minimal. It processes command line options and logs out
/// scheduling statistics. The BPF part makes all the scheduling decisions.
/// See the more detailed overview of the LAVD design at main.bpf.c.
#[derive(Debug, Parser)]
struct Opts {
    /// Automatically decide the scheduler's power mode (performance vs.
    /// powersave vs. balanced), CPU preference order, etc, based on system
    /// load. The options affecting the power mode and the use of core compaction
    /// (--autopower, --performance, --powersave, --balanced,
    /// --no-core-compaction) cannot be used with this option. When no option
    /// is specified, this is a default mode.
    #[clap(long = "autopilot", action = clap::ArgAction::SetTrue)]
    autopilot: bool,

    /// Automatically decide the scheduler's power mode (performance vs.
    /// powersave vs. balanced) based on the system's active power profile.
    /// The scheduler's power mode decides the CPU preference order and the use
    /// of core compaction, so the options affecting these (--autopilot,
    /// --performance, --powersave, --balanced, --no-core-compaction) cannot
    /// be used with this option.
    #[clap(long = "autopower", action = clap::ArgAction::SetTrue)]
    autopower: bool,

    /// Run the scheduler in performance mode to get maximum performance.
    /// This option cannot be used with other conflicting options (--autopilot,
    /// --autopower, --balanced, --powersave, --no-core-compaction)
    /// affecting the use of core compaction.
    #[clap(long = "performance", action = clap::ArgAction::SetTrue)]
    performance: bool,

    /// Run the scheduler in powersave mode to minimize power consumption.
    /// This option cannot be used with other conflicting options (--autopilot,
    /// --autopower, --performance, --balanced, --no-core-compaction)
    /// affecting the use of core compaction.
    #[clap(long = "powersave", action = clap::ArgAction::SetTrue)]
    powersave: bool,

    /// Run the scheduler in balanced mode aiming for sweetspot between power
    /// and performance. This option cannot be used with other conflicting
    /// options (--autopilot, --autopower, --performance, --powersave,
    /// --no-core-compaction) affecting the use of core compaction.
    #[clap(long = "balanced", action = clap::ArgAction::SetTrue)]
    balanced: bool,

    /// Maximum scheduling slice duration in microseconds.
    #[clap(long = "slice-max-us", default_value = "5000")]
    slice_max_us: u64,

    /// Minimum scheduling slice duration in microseconds.
    #[clap(long = "slice-min-us", default_value = "500")]
    slice_min_us: u64,

    /// Migration delta threshold percentage (0-100). When set to a non-zero value,
    /// uses average utilization for threshold calculation instead of current
    /// utilization, and the threshold is calculated as: avg_load * (mig-delta-pct / 100).
    /// Additionally, disables force task stealing in the consume path, relying only
    /// on the is_stealer/is_stealee thresholds for more predictable load balancing.
    /// Default is 0 (disabled, uses dynamic threshold based on load with both
    /// probabilistic and force task stealing enabled). This is an experimental feature.
    #[clap(long = "mig-delta-pct", default_value = "0", value_parser=Opts::mig_delta_pct_range)]
    mig_delta_pct: u8,

    /// Slice duration in microseconds to use for all tasks when pinned tasks
    /// are running on a CPU. Must be between slice-min-us and slice-max-us.
    /// When this option is enabled, pinned tasks are always enqueued to per-CPU DSQs
    /// and the dispatch logic compares vtimes across all DSQs to select the lowest
    /// vtime task. This helps improve responsiveness for pinned tasks.
    #[clap(long = "pinned-slice-us")]
    pinned_slice_us: Option<u64>,

    /// Limit the ratio of preemption to the roughly top P% of latency-critical
    /// tasks. When N is given as an argument, P is 0.5^N * 100. The default
    /// value is 6, which limits the preemption for the top 1.56% of
    /// latency-critical tasks.
    #[clap(long = "preempt-shift", default_value = "6", value_parser=Opts::preempt_shift_range)]
    preempt_shift: u8,

    /// List of CPUs in preferred order (e.g., "0-3,7,6,5,4"). The scheduler
    /// uses the CPU preference mode only when the core compaction is enabled
    /// (i.e., balanced or powersave mode is specified as an option or chosen
    /// in the autopilot or autopower mode). When "--cpu-pref-order" is given,
    /// it implies "--no-use-em".
    #[clap(long = "cpu-pref-order", default_value = "")]
    cpu_pref_order: String,

    /// Do not use the energy model in making CPU preference order decisions.
    #[clap(long = "no-use-em", action = clap::ArgAction::SetTrue)]
    no_use_em: bool,

    /// Do not boost futex holders.
    #[clap(long = "no-futex-boost", action = clap::ArgAction::SetTrue)]
    no_futex_boost: bool,

    /// Disable preemption.
    #[clap(long = "no-preemption", action = clap::ArgAction::SetTrue)]
    no_preemption: bool,

    /// Disable an optimization for synchronous wake-up.
    #[clap(long = "no-wake-sync", action = clap::ArgAction::SetTrue)]
    no_wake_sync: bool,

    /// Disable dynamic slice boost for long-running tasks.
    #[clap(long = "no-slice-boost", action = clap::ArgAction::SetTrue)]
    no_slice_boost: bool,

    /// Enables DSQs per CPU, this enables task queuing and dispatching
    /// from CPU specific DSQs. This generally increases L1/L2 cache
    /// locality for tasks and lowers lock contention compared to shared DSQs,
    /// but at the cost of higher load balancing complexity. This is a
    /// highly experimental feature.
    #[clap(long = "per-cpu-dsq", action = clap::ArgAction::SetTrue)]
    per_cpu_dsq: bool,

    /// Disable core compaction so the scheduler uses all the online CPUs.
    /// The core compaction attempts to minimize the number of actively used
    /// CPUs for unaffinitized tasks, respecting the CPU preference order.
    /// Normally, the core compaction is enabled by the power mode (i.e.,
    /// balanced or powersave mode is specified as an option or chosen in
    /// the autopilot or autopower mode). This option cannot be used with the
    /// other options that control the core compaction (--autopilot,
    /// --autopower, --performance, --balanced, --powersave).
    #[clap(long = "no-core-compaction", action = clap::ArgAction::SetTrue)]
    no_core_compaction: bool,

    /// Disable controlling the CPU frequency.
    #[clap(long = "no-freq-scaling", action = clap::ArgAction::SetTrue)]
    no_freq_scaling: bool,

    /// Enable stats monitoring with the specified interval.
    #[clap(long)]
    stats: Option<f64>,

    /// Run in stats monitoring mode with the specified interval. Scheduler is not launched.
    #[clap(long)]
    monitor: Option<f64>,

    /// Run in monitoring mode. Show the specified number of scheduling
    /// samples every second.
    #[clap(long)]
    monitor_sched_samples: Option<u64>,

    /// Enable verbose output, including libbpf details. Specify multiple
    /// times to increase verbosity.
    #[clap(short = 'v', long, action = clap::ArgAction::Count)]
    verbose: u8,

    /// Print scheduler version and exit.
    #[clap(short = 'V', long, action = clap::ArgAction::SetTrue)]
    version: bool,

    /// Show descriptions for statistics.
    #[clap(long)]
    help_stats: bool,

    #[clap(flatten, next_help_heading = "Libbpf Options")]
    pub libbpf: LibbpfOpts,

    /// Topology configuration options
    #[clap(flatten)]
    topology: Option<TopologyArgs>,
}

impl Opts {
    /// Check if autopilot mode can be enabled
    ///
    #[inline(always)]
    const fn can_autopilot(&self) -> bool {
        !self.autopower
        && !self.performance
        && !self.powersave
        && !self.balanced
        && !self.no_core_compaction
    }

    /// Check if autopower mode can be enabled
    #[inline(always)]
    const fn can_autopower(&self) -> bool {
        !self.autopilot
        && !self.performance
        && !self.powersave
        && !self.balanced
        && !self.no_core_compaction
    }

    /// Check if performance mode can be enabled
    #[inline(always)]
    const fn can_performance(&self) -> bool {
        !self.autopilot && !self.autopower && !self.powersave && !self.balanced
    }

    /// Check if balanced mode can be enabled
    #[inline(always)]
    const fn can_balanced(&self) -> bool {
        !self.autopilot
        && !self.autopower
        && !self.performance
        && !self.powersave
        && !self.no_core_compaction
    }

    /// Check if powersave mode can be enabled
    #[inline(always)]
    const fn can_powersave(&self) -> bool {
        !self.autopilot
        && !self.autopower
        && !self.performance
        && !self.balanced
        && !self.no_core_compaction
    }

    /// Process and validate options
    ///
    fn proc(&mut self) -> Option<&mut Self> {
        // Enable autopilot if no other mode specified
        if !self.autopilot {
            self.autopilot = self.can_autopilot();
        }

        // Validate autopilot mode
        if self.autopilot && !self.can_autopilot() {
            info!("Autopilot mode cannot be used with conflicting options.");
            return None;
        }

        // Validate autopower mode
        if self.autopower {
            if !self.can_autopower() {
                info!("Autopower mode cannot be used with conflicting options.");
                return None;
            }
            info!("Autopower mode is enabled.");
        }

        // Validate and configure performance mode
        if self.performance {
            if !self.can_performance() {
                info!("Performance mode cannot be used with conflicting options.");
                return None;
            }
            info!("Performance mode is enabled.");
            self.no_core_compaction = true;
        }

        // Validate and configure powersave mode
        if self.powersave {
            if !self.can_powersave() {
                info!("Powersave mode cannot be used with conflicting options.");
                return None;
            }
            info!("Powersave mode is enabled.");
            self.no_core_compaction = false;
        }

        // Validate and configure balanced mode
        if self.balanced {
            if !self.can_balanced() {
                info!("Balanced mode cannot be used with conflicting options.");
                return None;
            }
            info!("Balanced mode is enabled.");
            self.no_core_compaction = false;
        }

        // Configure energy model usage
        if !EnergyModel::has_energy_model() || !self.cpu_pref_order.is_empty() {
            self.no_use_em = true;
            info!("Energy model won't be used for CPU preference order.");
        }

        // Validate pinned slice configuration
        if let Some(pinned_slice) = self.pinned_slice_us {
            if pinned_slice < self.slice_min_us || pinned_slice > self.slice_max_us {
                info!(
                    "pinned-slice-us ({}) must be between slice-min-us ({}) and slice-max-us ({})",
                      pinned_slice, self.slice_min_us, self.slice_max_us
                );
                return None;
            }
            info!(
                "Pinned task slice mode enabled ({} μs). Pinned tasks use per-CPU DSQs.",
                  pinned_slice
            );
        }

        // Log autopilot status if enabled
        if self.autopilot {
            info!("Autopilot mode is enabled.");
        }

        Some(self)
    }

    /// Validate preempt shift range (0-10)
    #[inline]
    fn preempt_shift_range(s: &str) -> Result<u8, String> {
        number_range(s, 0, 10)
    }

    /// Validate migration delta percentage (0-100)
    #[inline]
    fn mig_delta_pct_range(s: &str) -> Result<u8, String> {
        number_range(s, 0, 100)
    }
}

unsafe impl Plain for msg_task_ctx {}

impl msg_task_ctx {
    /// Convert bytes to msg_task_ctx
    ///
    /// CRITICAL FIX: Use Result instead of panicking
    /// SAFETY: Caller must ensure buffer is properly sized and aligned
    fn from_bytes(buf: &[u8]) -> Result<&msg_task_ctx> {
        plain::from_bytes(buf)
            .map_err(|e| anyhow::anyhow!("Failed to parse msg_task_ctx: {:?}", e))
    }
}

impl introspec {
    /// Create new introspec instance
    ///
    #[inline]
    fn new() -> Self {
        Self {
            cmd: LAVD_CMD_NOP,
            arg: 0,
        }
    }
}

/// Message sequence ID generator
///
static MSG_SEQ_ID: AtomicU64 = AtomicU64::new(0);

/// Global constants for pre-allocated durations
///
const STATS_TIMEOUT: Duration = Duration::from_secs(1);
const RINGBUF_POLL_TIMEOUT: Duration = Duration::from_millis(100);

struct Scheduler<'a> {
    skel: BpfSkel<'a>,
    struct_ops: Option<libbpf_rs::Link>,
    rb_mgr: libbpf_rs::RingBuffer<'static>,
    intrspc: introspec,
    intrspc_rx: Receiver<SchedSample>,
    monitor_tid: Option<ThreadId>,
    stats_server: StatsServer<StatsReq, StatsRes>,
}

impl<'a> Scheduler<'a> {
    fn init(opts: &'a Opts, open_object: &'a mut MaybeUninit<OpenObject>) -> Result<Self> {
        if *NR_CPU_IDS > LAVD_CPU_ID_MAX as usize {
            anyhow::bail!(
                "Num possible CPU IDs ({}) exceeds maximum of ({})",
                *NR_CPU_IDS,
                LAVD_CPU_ID_MAX
            );
        }

        try_set_rlimit_infinity();

        // Open the BPF prog first for verification.
        let mut skel_builder = BpfSkelBuilder::default();
        skel_builder.obj_builder.debug(opts.verbose > 0);
        init_libbpf_logging(Some(PrintLevel::Debug));

        let open_opts = opts.libbpf.clone().into_bpf_open_opts();
        let mut skel = scx_ops_open!(skel_builder, open_object, lavd_ops, open_opts)?;

        // Enable futex tracing using ftrace if available. If the ftrace is not
        // available, use tracepoint, which is known to be slower than ftrace.
        if !opts.no_futex_boost {
            if !Self::attach_futex_ftraces(&mut skel)? {
                info!("Failed to attach futex ftraces. Trying tracepoints.");
                if !Self::attach_futex_tracepoints(&mut skel)? {
                    warn!("Failed to attach futex tracepoints. Futex boosting disabled.");
                }
            }
        }

        // Initialize CPU topology with CLI arguments
        let order = CpuOrder::new(opts.topology.as_ref())?;
        Self::init_cpus(&mut skel, &order);
        Self::init_cpdoms(&mut skel, &order);

        // Initialize skel according to @opts.
        Self::init_globals(&mut skel, opts, &order);

        // Attach.
        let mut skel = scx_ops_load!(skel, lavd_ops, uei)?;
        let struct_ops = Some(scx_ops_attach!(skel, lavd_ops)?);
        let stats_server = StatsServer::new(stats::server_data(*NR_CPU_IDS as u64)).launch()?;

        // Build a ring buffer for instrumentation
        let (intrspc_tx, intrspc_rx) = channel::bounded(65536);
        let rb_map = &mut skel.maps.introspec_msg;
        let mut builder = libbpf_rs::RingBufferBuilder::new();
        builder.add(rb_map, move |data| {
            Scheduler::relay_introspec(data, &intrspc_tx)
        })?;
        let rb_mgr = builder.build()?;

        Ok(Self {
            skel,
            struct_ops,
            rb_mgr,
            intrspc: introspec::new(),
            intrspc_rx,
            monitor_tid: None,
            stats_server,
        })
    }

    fn attach_futex_ftraces(skel: &mut OpenBpfSkel) -> Result<bool> {
        let ftraces = vec![
            ("__futex_wait", &skel.progs.fexit___futex_wait),
            ("futex_wait_multiple", &skel.progs.fexit_futex_wait_multiple),
            (
                "futex_wait_requeue_pi",
                &skel.progs.fexit_futex_wait_requeue_pi,
            ),
            ("futex_wake", &skel.progs.fexit_futex_wake),
            ("futex_wake_op", &skel.progs.fexit_futex_wake_op),
            ("futex_lock_pi", &skel.progs.fexit_futex_lock_pi),
            ("futex_unlock_pi", &skel.progs.fexit_futex_unlock_pi),
        ];

        compat::cond_kprobes_enable(ftraces)
    }

    fn attach_futex_tracepoints(skel: &mut OpenBpfSkel) -> Result<bool> {
        let tracepoints = vec![
            ("syscalls:sys_enter_futex", &skel.progs.rtp_sys_enter_futex),
            ("syscalls:sys_exit_futex", &skel.progs.rtp_sys_exit_futex),
            (
                "syscalls:sys_exit_futex_wait",
                &skel.progs.rtp_sys_exit_futex_wait,
            ),
            (
                "syscalls:sys_exit_futex_waitv",
                &skel.progs.rtp_sys_exit_futex_waitv,
            ),
            (
                "syscalls:sys_exit_futex_wake",
                &skel.progs.rtp_sys_exit_futex_wake,
            ),
        ];

        compat::cond_tracepoints_enable(tracepoints)
    }

    /// Initialize CPU capacity and topology information
    ///
    fn init_cpus(skel: &mut OpenBpfSkel, order: &CpuOrder) {
        debug!("{:#?}", order);

        // Validate complexity early
        let nr_pco_states = order.perf_cpu_order.len() as u8;
        if nr_pco_states > LAVD_PCO_STATE_MAX as u8 {
            panic!(
                "Generated performance vs. CPU order states ({}) exceed maximum ({})",
                   nr_pco_states,
                   LAVD_PCO_STATE_MAX
            );
        }

        let rodata = skel
        .maps
        .rodata_data
        .as_mut()
        .expect("rodata not available");

        // Initialize CPU capacity and topology
        for cpu in order.cpuids.iter() {
            rodata.cpu_capacity[cpu.cpu_adx] = cpu.cpu_cap as u16;
            rodata.cpu_big[cpu.cpu_adx] = cpu.big_core as u8;
            rodata.cpu_turbo[cpu.cpu_adx] = cpu.turbo_core as u8;
            rodata.cpu_sibling[cpu.cpu_adx] = cpu.cpu_sibling as u32;
        }

        // Initialize performance vs. CPU order table
        rodata.nr_pco_states = nr_pco_states;

        // Process active performance states
        for (i, (_, pco)) in order.perf_cpu_order.iter().enumerate() {
            let cpus_perf = pco.cpus_perf.borrow();
            let cpus_ovflw = pco.cpus_ovflw.borrow();
            let pco_nr_primary = cpus_perf.len();

            rodata.pco_bounds[i] = pco.perf_cap as u32;
            rodata.pco_nr_primary[i] = pco_nr_primary as u16;

            for (j, &cpu_adx) in cpus_perf.iter().enumerate() {
                rodata.pco_table[i][j] = cpu_adx as u16;
            }

            for (j, &cpu_adx) in cpus_ovflw.iter().enumerate() {
                let k = j + pco_nr_primary;
                rodata.pco_table[i][k] = cpu_adx as u16;
            }

            info!("{:#}", pco);
        }

        // Fill remaining slots with last state (cold path)
        if let Some((_, last_pco)) = order.perf_cpu_order.last_key_value() {
            let cpus_perf = last_pco.cpus_perf.borrow();
            let cpus_ovflw = last_pco.cpus_ovflw.borrow();
            let pco_nr_primary = cpus_perf.len();

            for i in nr_pco_states..LAVD_PCO_STATE_MAX as u8 {
                let idx = i as usize;

                rodata.pco_bounds[idx] = last_pco.perf_cap as u32;
                rodata.pco_nr_primary[idx] = pco_nr_primary as u16;

                for (j, &cpu_adx) in cpus_perf.iter().enumerate() {
                    rodata.pco_table[idx][j] = cpu_adx as u16;
                }

                for (j, &cpu_adx) in cpus_ovflw.iter().enumerate() {
                    let k = j + pco_nr_primary;
                    rodata.pco_table[idx][k] = cpu_adx as u16;
                }
            }
        }
    }

    /// Initialize compute domain contexts
    ///
    fn init_cpdoms(skel: &mut OpenBpfSkel, order: &CpuOrder) {
        let bss_data = skel.maps.bss_data.as_mut().expect("bss_data not available");

        for (k, v) in order.cpdom_map.iter() {
            let cpdom = &mut bss_data.cpdom_ctxs[v.cpdom_id];

            // Basic domain identification
            cpdom.id = v.cpdom_id as u64;
            cpdom.alt_id = v.cpdom_alt_id.get() as u64;
            cpdom.numa_id = k.numa_adx as u8;
            cpdom.llc_id = k.llc_adx as u8;
            cpdom.is_big = k.is_big as u8;
            cpdom.is_valid = 1;

            for &cpu_id in v.cpu_ids.iter() {
                let word_idx = (cpu_id / 64) as usize;
                let bit_idx = cpu_id % 64;

                cpdom.__cpumask[word_idx] |= 1u64 << bit_idx;
            }

            // Validate topology complexity (must fit in BPF arrays)
            let neighbor_count = v.neighbor_map.borrow().len();
            if neighbor_count > LAVD_CPDOM_MAX_DIST as usize {
                panic!(
                    "Processor topology too complex: {} neighbor distances (max {})",
                       neighbor_count, LAVD_CPDOM_MAX_DIST
                );
            }

            // Build neighbor bitmasks for each distance level
            //
            // neighbor_bits[k] is a u64 bitmask where bit N is set if
            // compute domain N is a neighbor at distance k.
            //
            // Example: If domain 3 is a neighbor, set bit 3:
            // neighbor_bits[k] |= 1u64 << 3
            // Result: 0x0000000000000008 (bit 3 set)
            for (dist_idx, (_distance, neighbors)) in v.neighbor_map.borrow().iter().enumerate() {
                let neighbor_list = neighbors.borrow();
                let nr_neighbors = neighbor_list.len() as u8;

                if nr_neighbors > LAVD_CPDOM_MAX_NR as u8 {
                    panic!(
                        "Too many neighbor domains: {} (max {})",
                           nr_neighbors, LAVD_CPDOM_MAX_NR
                    );
                }

                cpdom.nr_neighbors[dist_idx] = nr_neighbors;

                let mut neighbor_bits = 0u64;
                for &neighbor_id in neighbor_list.iter() {
                    neighbor_bits |= 1u64 << neighbor_id;
                }
                cpdom.neighbor_bits[dist_idx] = neighbor_bits;
            }
        }
    }

    /// Initialize global BPF variables
    ///
    fn init_globals(skel: &mut OpenBpfSkel, opts: &Opts, order: &CpuOrder) {
        let bss_data = skel.maps.bss_data.as_mut().expect("bss_data not available");
        bss_data.no_preemption = opts.no_preemption;
        bss_data.no_core_compaction = opts.no_core_compaction;
        bss_data.no_freq_scaling = opts.no_freq_scaling;
        bss_data.is_powersave_mode = opts.powersave;

        let rodata = skel.maps.rodata_data.as_mut().expect("rodata not available");
        rodata.nr_llcs = order.nr_llcs as u64;
        rodata.__nr_cpu_ids = *NR_CPU_IDS as u64;
        rodata.is_smt_active = order.smt_enabled;
        rodata.is_autopilot_on = opts.autopilot;
        rodata.verbose = opts.verbose;
        rodata.slice_max_ns = opts.slice_max_us * 1000;
        rodata.slice_min_ns = opts.slice_min_us * 1000;
        rodata.pinned_slice_ns = opts.pinned_slice_us.map(|v| v * 1000).unwrap_or(0);
        rodata.preempt_shift = opts.preempt_shift;
        rodata.mig_delta_pct = opts.mig_delta_pct;
        rodata.no_use_em = opts.no_use_em as u8;
        rodata.no_wake_sync = opts.no_wake_sync;
        rodata.no_slice_boost = opts.no_slice_boost;
        rodata.per_cpu_dsq = opts.per_cpu_dsq;

        skel.struct_ops.lavd_ops_mut().flags = *compat::SCX_OPS_ENQ_EXITING
            | *compat::SCX_OPS_ENQ_LAST
            | *compat::SCX_OPS_ENQ_MIGRATION_DISABLED
            | *compat::SCX_OPS_KEEP_BUILTIN_IDLE;
    }

    /// Get next message sequence ID
    ///
    #[inline(always)]
    fn get_msg_seq_id() -> u64 {
        MSG_SEQ_ID.fetch_add(1, Ordering::Relaxed)
    }

    /// Relay introspection data from BPF ring buffer to channel
    ///
    fn relay_introspec(data: &[u8], intrspc_tx: &Sender<SchedSample>) -> i32 {
        let mt = match msg_task_ctx::from_bytes(data) {
            Ok(mt) => mt,
            Err(e) => {
                // Use static counter to avoid spamming logs
                static PARSE_ERROR_COUNT: AtomicU64 = AtomicU64::new(0);
                let count = PARSE_ERROR_COUNT.fetch_add(1, Ordering::Relaxed);
                if count % 1000 == 0 {
                    warn!("Failed to parse msg_task_ctx (count: {}): {:?}", count, e);
                }
                return -1;
            }
        };

        let tx = &mt.taskc_x;
        let tc = &mt.taskc;

        if mt.hdr.kind != LAVD_MSG_TASKC {
            return 0;
        }

        let mseq = MSG_SEQ_ID.fetch_add(1, Ordering::Relaxed);

        let tx_comm = unsafe {
            CStr::from_ptr(tx.comm.as_ptr() as *const c_char)
            .to_string_lossy()
            .into_owned()  // Convert to String for storage
        };

        let waker_comm = unsafe {
            CStr::from_ptr(tc.waker_comm.as_ptr() as *const c_char)
            .to_string_lossy()
            .into_owned()
        };

        let tx_stat = unsafe {
            CStr::from_ptr(tx.stat.as_ptr() as *const c_char)
            .to_string_lossy()
            .into_owned()
        };

        match intrspc_tx.try_send(SchedSample {
            mseq,
            pid: tx.pid,
            comm: tx_comm,
            stat: tx_stat,
            cpu_id: tc.cpu_id,
            prev_cpu_id: tc.prev_cpu_id,
            suggested_cpu_id: tc.suggested_cpu_id,
            waker_pid: tc.waker_pid,
            waker_comm,
            slice: tc.slice,
            lat_cri: tc.lat_cri,
            avg_lat_cri: tx.avg_lat_cri,
            static_prio: tx.static_prio,
            rerunnable_interval: tx.rerunnable_interval,
            resched_interval: tc.resched_interval,
            run_freq: tc.run_freq,
            avg_runtime: tc.avg_runtime,
            wait_freq: tc.wait_freq,
            wake_freq: tc.wake_freq,
            perf_cri: tc.perf_cri,
            thr_perf_cri: tx.thr_perf_cri,
            cpuperf_cur: tx.cpuperf_cur,
            cpu_util: tx.cpu_util,
            cpu_sutil: tx.cpu_sutil,
            nr_active: tx.nr_active,
            dsq_id: tx.dsq_id,
            dsq_consume_lat: tx.dsq_consume_lat,
            slice_used: tc.last_slice_used,
        }) {
            Ok(()) => 0,
            Err(TrySendError::Full(_)) => {
                static DROP_COUNT: AtomicU64 = AtomicU64::new(0);
                let count = DROP_COUNT.fetch_add(1, Ordering::Relaxed);
                if count % 10000 == 0 {
                    warn!("Sample channel full, dropped {} samples", count);
                }
                0  // Return success to continue processing
            }
            Err(TrySendError::Disconnected(_)) => {
                // Channel closed - receiver dropped
                -1
            }
        }
    }

    /// Prepare introspection state for BPF
    #[inline(always)]
    fn prep_introspec(&mut self) {
        let bss_data = self.skel.maps.bss_data.as_mut().expect("bss_data not available");
        if !bss_data.is_monitored {
            bss_data.is_monitored = true;
        }
        bss_data.intrspc.cmd = self.intrspc.cmd;
        bss_data.intrspc.arg = self.intrspc.arg;
    }

    /// Clean up introspection state
    #[inline(always)]
    fn cleanup_introspec(&mut self) {
        if let Some(bss_data) = self.skel.maps.bss_data.as_mut() {
            bss_data.intrspc.cmd = LAVD_CMD_NOP;
        }
    }

    /// Calculate percentage
    ///
    #[inline(always)]
    fn get_pc(x: u64, y: u64) -> f64 {
        if y == 0 {
            0.0
        } else {
            // OPTIMIZATION: Use mul_add for FMA instruction on Raptor Lake
            // FMA: single cycle latency vs 3 cycles (mul + add)
            (x as f64).mul_add(100.0, 0.0) / (y as f64)
        }
    }

    /// Get power mode name
    ///
    #[inline(always)]
    fn get_power_mode(power_mode: i32) -> &'static str {
        match power_mode as u32 {
            LAVD_PM_PERFORMANCE => "performance",
            LAVD_PM_BALANCED => "balanced",
            LAVD_PM_POWERSAVE => "powersave",
            _ => "unknown",
        }
    }

    /// Process stats request and generate response
    ///
    fn stats_req_to_res(&mut self, req: &StatsReq) -> Result<StatsRes> {
        Ok(match req {
            StatsReq::NewSampler(tid) => {
                // CRITICAL FIX: Handle ring buffer errors instead of unwrap
                self.rb_mgr
                .consume()
                .context("Failed to consume ring buffer")?;
                self.monitor_tid = Some(*tid);
                StatsRes::Ack
            }
            StatsReq::SysStatsReq { tid } => {
                if Some(*tid) != self.monitor_tid {
                    return Ok(StatsRes::Bye);
                }

                let bss_data = self
                .skel
                .maps
                .bss_data
                .as_ref()
                .expect("bss_data not available");
                let st = &bss_data.sys_stat;

                let nr_queued_task = st.nr_queued_task;
                let nr_active = st.nr_active;
                let nr_sched = st.nr_sched;
                let nr_preempt = st.nr_preempt;
                let nr_stealee = st.nr_stealee;
                let nr_big = st.nr_big;

                let pc_pc = Self::get_pc(st.nr_perf_cri, nr_sched);
                let pc_lc = Self::get_pc(st.nr_lat_cri, nr_sched);
                let pc_x_migration = Self::get_pc(st.nr_x_migration, nr_sched);
                let pc_big = Self::get_pc(nr_big, nr_sched);
                let pc_pc_on_big = Self::get_pc(st.nr_pc_on_big, nr_big);
                let pc_lc_on_big = Self::get_pc(st.nr_lc_on_big, nr_big);

                let power_mode = Self::get_power_mode(bss_data.power_mode);
                let total_time = bss_data.performance_mode_ns
                + bss_data.balanced_mode_ns
                + bss_data.powersave_mode_ns;
                let pc_performance = Self::get_pc(bss_data.performance_mode_ns, total_time);
                let pc_balanced = Self::get_pc(bss_data.balanced_mode_ns, total_time);
                let pc_powersave = Self::get_pc(bss_data.powersave_mode_ns, total_time);

                StatsRes::SysStats(SysStats {
                    mseq: MSG_SEQ_ID.load(Ordering::Relaxed),
                                   nr_queued_task,
                                   nr_active,
                                   nr_sched,
                                   nr_preempt,
                                   pc_pc,
                                   pc_lc,
                                   pc_x_migration,
                                   nr_stealee,
                                   pc_big,
                                   pc_pc_on_big,
                                   pc_lc_on_big,
                                   power_mode: power_mode.to_string(),
                                   pc_performance,
                                   pc_balanced,
                                   pc_powersave,
                })
            }
            StatsReq::SchedSamplesNr {
                tid,
                nr_samples,
                interval_ms,
            } => {
                if Some(*tid) != self.monitor_tid {
                    return Ok(StatsRes::Bye);
                }

                self.intrspc.cmd = LAVD_CMD_SCHED_N;
                self.intrspc.arg = *nr_samples;
                self.prep_introspec();
                std::thread::sleep(Duration::from_millis(*interval_ms));

                self.rb_mgr
                .poll(RINGBUF_POLL_TIMEOUT)
                .context("Failed to poll ring buffer")?;

                let mut samples = Vec::with_capacity(*nr_samples as usize);

                samples.extend(self.intrspc_rx.try_iter());

                self.cleanup_introspec();

                StatsRes::SchedSamples(SchedSamples { samples })
            }
        })
    }

    /// Stop monitoring mode
    #[inline(always)]
    fn stop_monitoring(&mut self) {
        if let Some(bss_data) = self.skel.maps.bss_data.as_mut() {
            if bss_data.is_monitored {
                bss_data.is_monitored = false;
            }
        }
    }

    pub fn exited(&mut self) -> bool {
        uei_exited!(&self.skel, uei)
    }

    fn set_power_profile(&mut self, mode: u32) -> Result<(), u32> {
        let prog = &mut self.skel.progs.set_power_profile;
        let mut args = power_arg {
            power_mode: mode as c_int,
        };
        let input = ProgramInput {
            context_in: Some(unsafe {
                std::slice::from_raw_parts_mut(
                    &mut args as *mut _ as *mut u8,
                    std::mem::size_of_val(&args),
                )
            }),
            ..Default::default()
        };
        let out = prog.test_run(input).unwrap();
        if out.return_value != 0 {
            return Err(out.return_value);
        }

        Ok(())
    }

    fn update_power_profile(&mut self, prev_profile: PowerProfile) -> (bool, PowerProfile) {
        let profile = fetch_power_profile(false);
        if profile == prev_profile {
            // If the profile is the same, skip updaring the profile for BPF.
            return (true, profile);
        }

        let _ = match profile {
            PowerProfile::Performance => self.set_power_profile(LAVD_PM_PERFORMANCE),
            PowerProfile::Balanced { .. } => self.set_power_profile(LAVD_PM_BALANCED),
            PowerProfile::Powersave => self.set_power_profile(LAVD_PM_POWERSAVE),
            PowerProfile::Unknown => {
                // We don't know how to handle an unknown energy profile,
                // so we just give up updating the profile from now on.
                return (false, profile);
            }
        };

        info!("Set the scheduler's power profile to {profile} mode.");
        (true, profile)
    }

    fn run(&mut self, opts: &Opts, shutdown: Arc<AtomicBool>) -> Result<UserExitInfo> {
        let (res_ch, req_ch) = self.stats_server.channels();
        let mut autopower = opts.autopower;
        let mut profile = PowerProfile::Unknown;

        if opts.performance {
            let _ = self.set_power_profile(LAVD_PM_PERFORMANCE);
        } else if opts.powersave {
            let _ = self.set_power_profile(LAVD_PM_POWERSAVE);
        } else {
            let _ = self.set_power_profile(LAVD_PM_BALANCED);
        }

        while !shutdown.load(Ordering::Relaxed) && !self.exited() {
            if autopower {
                (autopower, profile) = self.update_power_profile(profile);
            }

            match req_ch.recv_timeout(Duration::from_secs(1)) {
                Ok(req) => {
                    let res = self.stats_req_to_res(&req)?;
                    res_ch.send(res)?;
                }
                Err(RecvTimeoutError::Timeout) => {
                    self.stop_monitoring();
                }
                Err(e) => {
                    self.stop_monitoring();
                    Err(e)?
                }
            }
            self.cleanup_introspec();
        }
        self.rb_mgr.consume().unwrap();

        let _ = self.struct_ops.take();
        uei_report!(&self.skel, uei)
    }
}

impl Drop for Scheduler<'_> {
    fn drop(&mut self) {
        info!("Unregister {SCHEDULER_NAME} scheduler");

        if let Some(struct_ops) = self.struct_ops.take() {
            drop(struct_ops);
        }
    }
}

fn init_log(opts: &Opts) {
    let llv = match opts.verbose {
        0 => simplelog::LevelFilter::Info,
        1 => simplelog::LevelFilter::Debug,
        _ => simplelog::LevelFilter::Trace,
    };
    let mut lcfg = simplelog::ConfigBuilder::new();
    lcfg.set_time_offset_to_local()
        .expect("Failed to set local time offset")
        .set_time_level(simplelog::LevelFilter::Error)
        .set_location_level(simplelog::LevelFilter::Off)
        .set_target_level(simplelog::LevelFilter::Off)
        .set_thread_level(simplelog::LevelFilter::Off);
    simplelog::TermLogger::init(
        llv,
        lcfg.build(),
        simplelog::TerminalMode::Stderr,
        simplelog::ColorChoice::Auto,
    )
    .unwrap();
}

fn main() -> Result<()> {
    let mut opts = Opts::parse();

    if opts.version {
        println!(
            "scx_lavd {}",
            build_id::full_version(env!("CARGO_PKG_VERSION"))
        );
        return Ok(());
    }

    if opts.help_stats {
        let sys_stats_meta_name = SysStats::meta().name;
        let sched_sample_meta_name = SchedSample::meta().name;
        let stats_meta_names: &[&str] = &[
            sys_stats_meta_name.as_str(),
            sched_sample_meta_name.as_str(),
        ];
        stats::server_data(0).describe_meta(&mut std::io::stdout(), Some(&stats_meta_names))?;
        return Ok(());
    }

    init_log(&opts);

    if opts.monitor.is_none() && opts.monitor_sched_samples.is_none() {
        opts.proc().unwrap();
        info!("{:#?}", opts);
    }

    let shutdown = Arc::new(AtomicBool::new(false));
    let shutdown_clone = shutdown.clone();
    ctrlc::set_handler(move || {
        shutdown_clone.store(true, Ordering::Relaxed);
    })
    .context("Error setting Ctrl-C handler")?;

    if let Some(nr_samples) = opts.monitor_sched_samples {
        let shutdown_copy = shutdown.clone();
        let jh = std::thread::spawn(move || {
            stats::monitor_sched_samples(nr_samples, shutdown_copy).unwrap()
        });
        let _ = jh.join();
        return Ok(());
    }

    if let Some(intv) = opts.monitor.or(opts.stats) {
        let shutdown_copy = shutdown.clone();
        let jh = std::thread::spawn(move || {
            stats::monitor(Duration::from_secs_f64(intv), shutdown_copy).unwrap()
        });
        if opts.monitor.is_some() {
            let _ = jh.join();
            return Ok(());
        }
    }

    let mut open_object = MaybeUninit::uninit();
    loop {
        let mut sched = Scheduler::init(&opts, &mut open_object)?;
        info!(
            "scx_lavd scheduler is initialized (build ID: {})",
            build_id::full_version(env!("CARGO_PKG_VERSION"))
        );
        info!("scx_lavd scheduler starts running.");
        if !sched.run(&opts, shutdown.clone())?.should_restart() {
            break;
        }
    }

    Ok(())
}
