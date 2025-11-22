// SPDX-License-Identifier: GPL-2.0
//
// Copyright (c) 2024 Valve Corporation.
// Author: Changwoo Min <changwoo@igalia.com>

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
use libbpf_rs::skel::Skel;
use libbpf_rs::OpenObject;
use libbpf_rs::PrintLevel;
use libbpf_rs::ProgramInput;
use libc::c_char;
use plain::Plain;
use scx_arena::ArenaLib;
use scx_stats::prelude::*;
use scx_utils::autopower::{fetch_power_profile, PowerProfile};
use scx_utils::build_id;
use scx_utils::compat;
use scx_utils::ksym_exists;
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
use tracing::{debug, info, warn};
use tracing_subscriber::filter::EnvFilter;

const SCHEDULER_NAME: &str = "scx_lavd";

const STATS_TIMEOUT: Duration = Duration::from_secs(1);
const RINGBUF_POLL_TIMEOUT: Duration = Duration::from_millis(100);

#[derive(Debug, Parser)]
struct Opts {
    #[clap(short = 'v', long, action = clap::ArgAction::Count)]
    verbose: u8,

    #[clap(long = "autopilot", action = clap::ArgAction::SetTrue)]
    autopilot: bool,

    #[clap(long = "autopower", action = clap::ArgAction::SetTrue)]
    autopower: bool,

    #[clap(long = "performance", action = clap::ArgAction::SetTrue)]
    performance: bool,

    #[clap(long = "powersave", action = clap::ArgAction::SetTrue)]
    powersave: bool,

    #[clap(long = "balanced", action = clap::ArgAction::SetTrue)]
    balanced: bool,

    #[clap(long = "slice-max-us", default_value = "5000")]
    slice_max_us: u64,

    #[clap(long = "slice-min-us", default_value = "500")]
    slice_min_us: u64,

    #[clap(long = "mig-delta-pct", default_value = "0", value_parser=Opts::mig_delta_pct_range)]
    mig_delta_pct: u8,

    #[clap(long = "pinned-slice-us")]
    pinned_slice_us: Option<u64>,

    #[clap(long = "preempt-shift", default_value = "6", value_parser=Opts::preempt_shift_range)]
    preempt_shift: u8,

    #[clap(long = "cpu-pref-order", default_value = "")]
    cpu_pref_order: String,

    #[clap(long = "no-use-em", action = clap::ArgAction::SetTrue)]
    no_use_em: bool,

    #[clap(long = "no-futex-boost", action = clap::ArgAction::SetTrue)]
    no_futex_boost: bool,

    #[clap(long = "no-preemption", action = clap::ArgAction::SetTrue)]
    no_preemption: bool,

    #[clap(long = "no-wake-sync", action = clap::ArgAction::SetTrue)]
    no_wake_sync: bool,

    #[clap(long = "no-slice-boost", action = clap::ArgAction::SetTrue)]
    no_slice_boost: bool,

    #[clap(long = "per-cpu-dsq", action = clap::ArgAction::SetTrue)]
    per_cpu_dsq: bool,

    #[clap(long = "enable-cpu-bw", action = clap::ArgAction::SetTrue)]
    enable_cpu_bw: bool,

    #[clap(long = "no-core-compaction", action = clap::ArgAction::SetTrue)]
    no_core_compaction: bool,

    #[clap(long = "no-freq-scaling", action = clap::ArgAction::SetTrue)]
    no_freq_scaling: bool,

    #[clap(long)]
    stats: Option<f64>,

    #[clap(long)]
    monitor: Option<f64>,

    #[clap(long)]
    monitor_sched_samples: Option<u64>,

    #[clap(long, default_value = "info")]
    log_level: String,

    #[clap(short = 'V', long, action = clap::ArgAction::SetTrue)]
    version: bool,

    #[clap(long)]
    run_id: Option<u64>,

    #[clap(long)]
    help_stats: bool,

    #[clap(flatten, next_help_heading = "Libbpf Options")]
    pub libbpf: LibbpfOpts,

    #[clap(flatten)]
    topology: Option<TopologyArgs>,
}

impl Opts {
    #[inline(always)]
    const fn can_autopilot(&self) -> bool {
        !self.autopower
            && !self.performance
            && !self.powersave
            && !self.balanced
            && !self.no_core_compaction
    }

    #[inline(always)]
    const fn can_autopower(&self) -> bool {
        !self.autopilot
            && !self.performance
            && !self.powersave
            && !self.balanced
            && !self.no_core_compaction
    }

    #[inline(always)]
    const fn can_performance(&self) -> bool {
        !self.autopilot && !self.autopower && !self.powersave && !self.balanced
    }

    #[inline(always)]
    const fn can_balanced(&self) -> bool {
        !self.autopilot
            && !self.autopower
            && !self.performance
            && !self.powersave
            && !self.no_core_compaction
    }

    #[inline(always)]
    const fn can_powersave(&self) -> bool {
        !self.autopilot
            && !self.autopower
            && !self.performance
            && !self.balanced
            && !self.no_core_compaction
    }

    #[inline]
    fn validate_slice_window(&self) -> bool {
        if self.slice_min_us == 0 || self.slice_min_us > self.slice_max_us {
            info!(
                "slice-min-us ({}) must be > 0 and <= slice-max-us ({})",
                self.slice_min_us, self.slice_max_us
            );
            return false;
        }
        true
    }

    fn proc(&mut self) -> Option<&mut Self> {
        if !self.validate_slice_window() {
            return None;
        }

        if !self.autopilot {
            self.autopilot = self.can_autopilot();
        }

        if self.autopilot && !self.can_autopilot() {
            info!("Autopilot mode cannot be used with conflicting options.");
            return None;
        }

        if self.autopower {
            if !self.can_autopower() {
                info!("Autopower mode cannot be used with conflicting options.");
                return None;
            }
            info!("Autopower mode is enabled.");
        }

        if self.performance {
            if !self.can_performance() {
                info!("Performance mode cannot be used with conflicting options.");
                return None;
            }
            info!("Performance mode is enabled.");
            self.no_core_compaction = true;
        }

        if self.powersave {
            if !self.can_powersave() {
                info!("Powersave mode cannot be used with conflicting options.");
                return None;
            }
            info!("Powersave mode is enabled.");
            self.no_core_compaction = false;
        }

        if self.balanced {
            if !self.can_balanced() {
                info!("Balanced mode cannot be used with conflicting options.");
                return None;
            }
            info!("Balanced mode is enabled.");
            self.no_core_compaction = false;
        }

        if !EnergyModel::has_energy_model() || !self.cpu_pref_order.is_empty() {
            self.no_use_em = true;
            info!("Energy model won't be used for CPU preference order.");
        }

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

        if self.autopilot {
            info!("Autopilot mode is enabled.");
        }

        Some(self)
    }

    #[inline]
    fn preempt_shift_range(s: &str) -> Result<u8, String> {
        number_range(s, 0, 10)
    }

    #[inline]
    fn mig_delta_pct_range(s: &str) -> Result<u8, String> {
        number_range(s, 0, 100)
    }
}

unsafe impl Plain for msg_task_ctx {}

impl msg_task_ctx {
    #[inline]
    fn from_bytes(buf: &[u8]) -> Result<&msg_task_ctx> {
        plain::from_bytes(buf)
            .map_err(|e| anyhow::anyhow!("Failed to parse msg_task_ctx: {:?}", e))
    }
}

impl introspec {
    #[inline]
    const fn new() -> Self {
        Self {
            cmd: LAVD_CMD_NOP,
            arg: 0,
        }
    }
}

#[repr(align(64))]
struct AlignedAtomicU64(AtomicU64);

static MSG_SEQ_ID: AlignedAtomicU64 = AlignedAtomicU64(AtomicU64::new(0));

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

        let debug_level = if opts.log_level.contains("trace") {
            2
        } else if opts.log_level.contains("debug") {
            1
        } else if opts.verbose > 1 {
            2
        } else if opts.verbose > 0 {
            1
        } else {
            0
        };

        let mut skel_builder = BpfSkelBuilder::default();
        skel_builder.obj_builder.debug(debug_level > 1);
        let log_level = if debug_level > 0 {
            Some(PrintLevel::Debug)
        } else {
            None
        };
        init_libbpf_logging(log_level);

        let open_opts = opts.libbpf.clone().into_bpf_open_opts();
        let mut skel = scx_ops_open!(skel_builder, open_object, lavd_ops, open_opts)?;

        if !opts.no_futex_boost {
            if !Self::attach_futex_ftraces(&mut skel)? {
                info!("Failed to attach futex ftraces. Trying tracepoints.");
                if !Self::attach_futex_tracepoints(&mut skel)? {
                    warn!("Failed to attach futex tracepoints. Futex boosting disabled.");
                }
            }
        }

        let order = CpuOrder::new(opts.topology.as_ref())?;
        Self::init_cpus(&mut skel, &order);
        Self::init_cpdoms(&mut skel, &order);
        Self::init_globals(&mut skel, opts, &order, debug_level);

        let mut skel = scx_ops_load!(skel, lavd_ops, uei)?;
        let task_size = std::mem::size_of::<types::task_ctx>();
        let arenalib = ArenaLib::init(skel.object_mut(), task_size, *NR_CPU_IDS)?;
        arenalib.setup()?;

        let struct_ops = Some(scx_ops_attach!(skel, lavd_ops)?);
        let stats_server = StatsServer::new(stats::server_data(*NR_CPU_IDS as u64)).launch()?;

        let (intrspc_tx, intrspc_rx) = channel::bounded(65536);
        let rb_map = &mut skel.maps.introspec_msg;
        let mut builder = libbpf_rs::RingBufferBuilder::new();
        builder.add(rb_map, move |data| Scheduler::relay_introspec(data, &intrspc_tx))?;
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

        if !compat::tracer_available("function")? {
            info!("Ftrace is not enabled in the kernel.");
            return Ok(false);
        }

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

    fn init_cpus(skel: &mut OpenBpfSkel, order: &CpuOrder) {
        debug!("{:#?}", order);

        let nr_pco_states = order.perf_cpu_order.len() as u8;
        if nr_pco_states > LAVD_PCO_STATE_MAX as u8 {
            panic!(
                "Generated performance vs. CPU order states ({}) exceed maximum ({})",
                nr_pco_states, LAVD_PCO_STATE_MAX
            );
        }

        let rodata = skel
            .maps
            .rodata_data
            .as_mut()
            .expect("rodata not available");

        for cpu in order.cpuids.iter() {
            rodata.cpu_capacity[cpu.cpu_adx] = cpu.cpu_cap as u16;
            rodata.cpu_big[cpu.cpu_adx] = cpu.big_core as u8;
            rodata.cpu_turbo[cpu.cpu_adx] = cpu.turbo_core as u8;
            rodata.cpu_sibling[cpu.cpu_adx] = cpu.cpu_sibling as u32;
        }

        rodata.nr_pco_states = nr_pco_states;

        for (i, (_, pco)) in order.perf_cpu_order.iter().enumerate() {
            Self::init_pco_tuple(skel, i, pco);
            info!("{:#}", pco);
        }

        if let Some((_, last_pco)) = order.perf_cpu_order.last_key_value() {
            for i in nr_pco_states..LAVD_PCO_STATE_MAX as u8 {
                Self::init_pco_tuple(skel, i as usize, last_pco);
            }
        }
    }

    #[inline]
    fn init_pco_tuple(skel: &mut OpenBpfSkel, i: usize, pco: &PerfCpuOrder) {
        let rodata = skel
            .maps
            .rodata_data
            .as_mut()
            .expect("rodata not available");

        // Direct access instead of borrow()
        let cpus_perf = &pco.cpus_perf;
        let cpus_ovflw = &pco.cpus_ovflw;
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
    }

    fn init_cpdoms(skel: &mut OpenBpfSkel, order: &CpuOrder) {
        let bss_data = skel.maps.bss_data.as_mut().expect("bss_data not available");

        for (k, v) in order.cpdom_map.iter() {
            let cpdom = &mut bss_data.cpdom_ctxs[v.cpdom_id];

            cpdom.id = v.cpdom_id as u64;
            // Direct access, no get()
            cpdom.alt_id = v.cpdom_alt_id as u64;
            cpdom.numa_id = k.numa_adx as u8;
            cpdom.llc_id = k.llc_adx as u8;
            cpdom.is_big = k.is_big as u8;
            cpdom.is_valid = 1;

            for &cpu_id in v.cpu_ids.iter() {
                let word_idx = (cpu_id / 64) as usize;
                let bit_idx = cpu_id % 64;
                cpdom.__cpumask[word_idx] |= 1u64 << bit_idx;
            }

            // Direct access, no borrow()
            let neighbor_count = v.neighbor_map.len();
            if neighbor_count > LAVD_CPDOM_MAX_DIST as usize {
                panic!(
                    "Processor topology too complex: {} neighbor distances (max {})",
                    neighbor_count, LAVD_CPDOM_MAX_DIST
                );
            }

            for (dist_idx, (_distance, neighbors)) in v.neighbor_map.iter().enumerate() {
                let nr_neighbors = neighbors.len() as u8;

                if nr_neighbors > LAVD_CPDOM_MAX_NR as u8 {
                    panic!(
                        "Too many neighbor domains: {} (max {})",
                        nr_neighbors, LAVD_CPDOM_MAX_NR
                    );
                }

                cpdom.nr_neighbors[dist_idx] = nr_neighbors;

                // Flatten neighbor IDs into the array
                for (i, &neighbor_id) in neighbors.iter().enumerate() {
                    let idx = (dist_idx * LAVD_CPDOM_MAX_NR as usize) + i;
                    cpdom.neighbor_ids[idx] = neighbor_id as u8;
                }
            }
        }
    }

    fn init_globals(
        skel: &mut OpenBpfSkel,
        opts: &Opts,
        order: &CpuOrder,
        debug_level: u8,
    ) {
        let bss_data = skel.maps.bss_data.as_mut().expect("bss_data not available");
        bss_data.no_preemption = opts.no_preemption;
        bss_data.no_core_compaction = opts.no_core_compaction;
        bss_data.no_freq_scaling = opts.no_freq_scaling;
        bss_data.is_powersave_mode = opts.powersave;

        let rodata = skel
            .maps
            .rodata_data
            .as_mut()
            .expect("rodata not available");
        rodata.nr_llcs = order.nr_llcs as u64;
        rodata.nr_cpu_ids = *NR_CPU_IDS as u32;
        rodata.is_smt_active = order.smt_enabled;
        rodata.is_autopilot_on = opts.autopilot;
        rodata.verbose = debug_level;
        rodata.slice_max_ns = opts.slice_max_us * 1000;
        rodata.slice_min_ns = opts.slice_min_us * 1000;
        rodata.pinned_slice_ns = opts.pinned_slice_us.map(|v| v * 1000).unwrap_or(0);
        rodata.preempt_shift = opts.preempt_shift;
        rodata.mig_delta_pct = opts.mig_delta_pct;
        rodata.no_use_em = opts.no_use_em as u8;
        rodata.no_wake_sync = opts.no_wake_sync;
        rodata.no_slice_boost = opts.no_slice_boost;
        rodata.per_cpu_dsq = opts.per_cpu_dsq;
        rodata.enable_cpu_bw = opts.enable_cpu_bw;

        if !ksym_exists("scx_cgroup_set_bandwidth").unwrap() {
            skel.struct_ops.lavd_ops_mut().cgroup_set_bandwidth = std::ptr::null_mut();
            warn!("Kernel does not support ops.cgroup_set_bandwidth(), so disable it.");
        }

        skel.struct_ops.lavd_ops_mut().flags = *compat::SCX_OPS_ENQ_EXITING
            | *compat::SCX_OPS_ENQ_LAST
            | *compat::SCX_OPS_ENQ_MIGRATION_DISABLED
            | *compat::SCX_OPS_KEEP_BUILTIN_IDLE;
    }

    #[inline(always)]
    fn get_msg_seq_id() -> u64 {
        MSG_SEQ_ID.0.fetch_add(1, Ordering::Relaxed)
    }

    fn relay_introspec(data: &[u8], intrspc_tx: &Sender<SchedSample>) -> i32 {
        let mt = match msg_task_ctx::from_bytes(data) {
            Ok(mt) => mt,
            Err(e) => return Self::handle_parse_error(&e),
        };

        if mt.hdr.kind != LAVD_MSG_TASKC {
            return 0;
        }

        let tx = &mt.taskc_x;
        // taskc is removed from msg_task_ctx as per patch
        // All fields are now in taskc_x (tx)

        let mseq = Self::get_msg_seq_id();

        let tx_comm = unsafe {
            CStr::from_ptr(tx.comm.as_ptr() as *const c_char)
                .to_string_lossy()
                .into_owned()
        };

        let waker_comm = unsafe {
            CStr::from_ptr(tx.waker_comm.as_ptr() as *const c_char)
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
            cpu_id: tx.cpu_id,
            prev_cpu_id: tx.prev_cpu_id,
            suggested_cpu_id: tx.suggested_cpu_id,
            waker_pid: tx.waker_pid,
            waker_comm,
            slice: tx.slice,
            lat_cri: tx.lat_cri,
            avg_lat_cri: tx.avg_lat_cri,
            static_prio: tx.static_prio,
            rerunnable_interval: tx.rerunnable_interval,
            resched_interval: tx.resched_interval,
            run_freq: tx.run_freq,
            avg_runtime: tx.avg_runtime,
            wait_freq: tx.wait_freq,
            wake_freq: tx.wake_freq,
            perf_cri: tx.perf_cri,
            thr_perf_cri: tx.thr_perf_cri,
            cpuperf_cur: tx.cpuperf_cur,
            cpu_util: tx.cpu_util,
            cpu_sutil: tx.cpu_sutil,
            nr_active: tx.nr_active,
            dsq_id: tx.dsq_id,
            dsq_consume_lat: tx.dsq_consume_lat,
            slice_used: tx.last_slice_used,
        }) {
            Ok(()) => 0,
            Err(TrySendError::Full(_)) => Self::handle_channel_full(),
            Err(TrySendError::Disconnected(_)) => -1,
        }
    }

    #[cold]
    #[inline(never)]
    fn handle_parse_error(e: &anyhow::Error) -> i32 {
        static PARSE_ERROR_COUNT: AtomicU64 = AtomicU64::new(0);
        let count = PARSE_ERROR_COUNT.fetch_add(1, Ordering::Relaxed);
        if count % 1000 == 0 {
            warn!("Failed to parse msg_task_ctx (count: {}): {:?}", count, e);
        }
        -1
    }

    #[cold]
    #[inline(never)]
    fn handle_channel_full() -> i32 {
        static DROP_COUNT: AtomicU64 = AtomicU64::new(0);
        let count = DROP_COUNT.fetch_add(1, Ordering::Relaxed);
        if count % 10000 == 0 {
            warn!("Sample channel full, dropped {} samples", count);
        }
        0
    }

    #[inline]
    fn prep_introspec(&mut self) {
        let bss_data = self
            .skel
            .maps
            .bss_data
            .as_mut()
            .expect("bss_data not available");
        if !bss_data.is_monitored {
            bss_data.is_monitored = true;
        }
        bss_data.intrspc.cmd = self.intrspc.cmd;
        bss_data.intrspc.arg = self.intrspc.arg;
    }

    #[inline]
    fn cleanup_introspec(&mut self) {
        if let Some(bss_data) = self.skel.maps.bss_data.as_mut() {
            bss_data.intrspc.cmd = LAVD_CMD_NOP;
        }
    }

    #[inline(always)]
    fn get_pc(x: u64, y: u64) -> f64 {
        if y == 0 {
            0.0
        } else {
            (x as f64).mul_add(100.0, 0.0) / (y as f64)
        }
    }

    #[inline(always)]
    fn get_power_mode(power_mode: i32) -> &'static str {
        match power_mode as u32 {
            LAVD_PM_PERFORMANCE => "performance",
            LAVD_PM_BALANCED => "balanced",
            LAVD_PM_POWERSAVE => "powersave",
            _ => "unknown",
        }
    }

    fn stats_req_to_res(&mut self, req: &StatsReq) -> Result<StatsRes> {
        Ok(match req {
            StatsReq::NewSampler(tid) => {
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

                let mseq = Self::get_msg_seq_id();
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
                    mseq,
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

    #[inline]
    fn stop_monitoring(&mut self) {
        if let Some(bss_data) = self.skel.maps.bss_data.as_mut() {
            if bss_data.is_monitored {
                bss_data.is_monitored = false;
            }
        }
    }

    #[inline]
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
        let out = prog.test_run(input).map_err(|_| u32::MAX)?;
        if out.return_value != 0 {
            return Err(out.return_value);
        }

        Ok(())
    }

    fn update_power_profile(&mut self, prev_profile: PowerProfile) -> (bool, PowerProfile) {
        let profile = fetch_power_profile(false);
        if profile == prev_profile {
            return (true, profile);
        }

        let set_result = match profile {
            PowerProfile::Performance => self.set_power_profile(LAVD_PM_PERFORMANCE),
            PowerProfile::Balanced { .. } => self.set_power_profile(LAVD_PM_BALANCED),
            PowerProfile::Powersave => self.set_power_profile(LAVD_PM_POWERSAVE),
            PowerProfile::Unknown => {
                return (false, profile);
            }
        };

        match set_result {
            Ok(_) => {
                info!("Set the scheduler's power profile to {profile} mode.");
                (true, profile)
            }
            Err(code) => {
                warn!(
                    "Failed to update power profile to {profile:?} (ret={}). Disabling autopower.",
                    code
                );
                (false, prev_profile)
            }
        }
    }

    fn run(&mut self, opts: &Opts, shutdown: Arc<AtomicBool>) -> Result<UserExitInfo> {
        let (res_ch, req_ch) = self.stats_server.channels();
        let mut autopower_active = opts.autopower;
        let mut profile = PowerProfile::Unknown;

        if opts.performance {
            if let Err(e) = self.set_power_profile(LAVD_PM_PERFORMANCE) {
                warn!("Failed to set initial performance profile: {}", e);
            }
        } else if opts.powersave {
            if let Err(e) = self.set_power_profile(LAVD_PM_POWERSAVE) {
                warn!("Failed to set initial powersave profile: {}", e);
            }
        } else if let Err(e) = self.set_power_profile(LAVD_PM_BALANCED) {
            warn!("Failed to set initial balanced profile: {}", e);
        }

        while !shutdown.load(Ordering::Acquire) && !self.exited() {
            if autopower_active {
                (autopower_active, profile) = self.update_power_profile(profile);
            }

            match req_ch.recv_timeout(STATS_TIMEOUT) {
                Ok(req) => {
                    let res = self.stats_req_to_res(&req)?;
                    res_ch.send(res)?;
                }
                Err(RecvTimeoutError::Timeout) => {
                    self.stop_monitoring();
                }
                Err(e) => {
                    self.stop_monitoring();
                    Err(e)?;
                }
            }
            self.cleanup_introspec();
        }
        self.rb_mgr
            .consume()
            .context("Failed to flush ring buffer before exit")?;

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
    let env_filter = EnvFilter::try_from_default_env()
        .or_else(|_| match EnvFilter::try_new(&opts.log_level) {
            Ok(filter) => Ok(filter),
            Err(e) => {
                eprintln!(
                    "invalid log envvar: {}, using info, err is: {}",
                    opts.log_level, e
                );
                EnvFilter::try_new("info")
            }
        })
        .unwrap_or_else(|_| EnvFilter::new("info"));

    match tracing_subscriber::fmt()
        .with_env_filter(env_filter)
        .with_target(true)
        .with_thread_ids(true)
        .with_file(true)
        .with_line_number(true)
        .try_init()
    {
        Ok(()) => {}
        Err(e) => eprintln!("failed to init logger: {}", e),
    }
}

#[clap_main::clap_main]
fn main(mut opts: Opts) -> Result<()> {
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

    if opts.verbose > 0 {
        warn!("Setting verbose via -v is deprecated and will be an error in future releases.");
    }

    if let Some(run_id) = opts.run_id {
        info!("scx_lavd run_id: {}", run_id);
    }

    if opts.monitor.is_none() && opts.monitor_sched_samples.is_none() {
        if opts.proc().is_none() {
            return Ok(());
        }
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
