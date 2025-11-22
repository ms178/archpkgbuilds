// SPDX-License-Identifier: GPL-2.0
//
// Copyright (c) 2025 Valve Corporation.
// Author: Changwoo Min <changwoo@igalia.com>

use anyhow::Result;
use combinations::Combinations;
use scx_utils::CoreType;
use scx_utils::Cpumask;
use scx_utils::EnergyModel;
use scx_utils::PerfDomain;
use scx_utils::PerfState;
use scx_utils::Topology;
use scx_utils::NR_CPU_IDS;
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashSet;
use std::fmt;
use std::hash::{Hash, Hasher};
use tracing::{debug, warn};

#[derive(Debug, Clone)]
pub struct CpuId {
    pub numa_adx: usize,
    pub pd_adx: usize,
    pub llc_adx: usize,
    pub llc_rdx: usize,
    pub llc_kernel_id: usize,
    pub core_rdx: usize,
    pub cpu_rdx: usize,
    pub cpu_adx: usize,
    pub smt_level: usize,
    pub cache_size: usize,
    pub cpu_cap: usize,
    pub big_core: bool,
    pub turbo_core: bool,
    pub cpu_sibling: usize,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone)]
pub struct ComputeDomainId {
    pub numa_adx: usize,
    pub llc_adx: usize,
    pub llc_rdx: usize,
    pub llc_kernel_id: usize,
    pub is_big: bool,
}

#[derive(Debug, Clone)]
pub struct ComputeDomain {
    pub cpdom_id: usize,
    pub cpdom_alt_id: usize,
    pub cpu_ids: Vec<usize>,
    pub neighbor_map: BTreeMap<usize, Vec<usize>>,
}

#[derive(Debug, Clone)]
pub struct PerfCpuOrder {
    pub perf_cap: usize,
    pub perf_util: f32,
    pub cpus_perf: Vec<usize>,
    pub cpus_ovflw: Vec<usize>,
}

#[derive(Debug)]
pub struct CpuOrder {
    pub all_cpus_mask: Cpumask,
    pub cpuids: Vec<CpuId>,
    pub perf_cpu_order: BTreeMap<usize, PerfCpuOrder>,
    pub cpdom_map: BTreeMap<ComputeDomainId, ComputeDomain>,
    pub nr_cpus: usize,
    pub nr_cores: usize,
    pub nr_cpdoms: usize,
    pub nr_llcs: usize,
    pub nr_numa: usize,
    pub smt_enabled: bool,
    pub has_biglittle: bool,
    pub has_energy_model: bool,
}

impl CpuOrder {
    pub fn new(topology_args: Option<&scx_utils::TopologyArgs>) -> Result<CpuOrder> {
        let ctx = CpuOrderCtx::new(topology_args)?;
        let cpus_pf = ctx.build_topo_order(false);
        let cpus_ps = ctx.build_topo_order(true);
        let cpdom_map = ctx.build_cpdom(&cpus_pf);

        let perf_cpu_order = if let Ok(em) = &ctx.em {
            // Safety cap: if too many PDs, combinatorial logic explodes O(2^N).
            // 16 is a safe upper bound for reasonable init time.
            if em.perf_doms.len() <= 16 {
                EnergyModelOptimizer::get_perf_cpu_order_table(em, &cpus_pf)
            } else {
                warn!(
                    "Too many performance domains ({}), utilizing topological fallback",
                    em.perf_doms.len()
                );
                EnergyModelOptimizer::get_fake_perf_cpu_order_table(&cpus_pf, &cpus_ps)
            }
        } else {
            EnergyModelOptimizer::get_fake_perf_cpu_order_table(&cpus_pf, &cpus_ps)
        };

        let nr_cpdoms = cpdom_map.len();
        Ok(CpuOrder {
            all_cpus_mask: ctx.topo.span.clone(),
            cpuids: cpus_pf,
            perf_cpu_order,
            cpdom_map,
            nr_cpus: ctx.topo.all_cpus.len(),
            nr_cores: ctx.topo.all_cores.len(),
            nr_cpdoms,
            nr_llcs: ctx.topo.all_llcs.len(),
            nr_numa: ctx.topo.nodes.len(),
            smt_enabled: ctx.smt_enabled,
            has_biglittle: ctx.has_biglittle,
            has_energy_model: ctx.has_energy_model,
        })
    }
}

struct CpuOrderCtx {
    topo: Topology,
    em: Result<EnergyModel>,
    smt_enabled: bool,
    has_biglittle: bool,
    has_energy_model: bool,
}

impl CpuOrderCtx {
    fn new(topology_args: Option<&scx_utils::TopologyArgs>) -> Result<Self> {
        let topo = match topology_args {
            Some(args) => Topology::with_args(args)?,
            None => Topology::new()?,
        };

        let em = EnergyModel::new();
        let smt_enabled = topo.smt_enabled;
        let has_biglittle = topo.has_little_cores();
        let has_energy_model = em.is_ok();

        debug!(
            "Topology: {} CPUs, Big.Little: {}",
            topo.all_cpus.len(),
            has_biglittle
        );

        Ok(CpuOrderCtx {
            topo,
            em,
            smt_enabled,
            has_biglittle,
            has_energy_model,
        })
    }

    /// Raptor Lake Ranking: Physical P-Core (0) > E-Core (1) > SMT (2)
    fn rank_cpu_performance(&self, cpu: &CpuId) -> u64 {
        let core_type_score = if cpu.big_core { 0 } else { 1 };
        let smt_score = if cpu.smt_level == 0 { 0 } else { 2 };

        // Invert capacity/cache so higher values = lower score (better)
        let cap_inv = (1024 - cpu.cpu_cap.min(1024)) as u64;
        let cache_inv = (usize::MAX - cpu.cache_size) as u64;

        ((smt_score as u64) << 56)
            | ((core_type_score as u64) << 52)
            | ((cpu.numa_adx as u64) << 48)
            | (cap_inv << 32)
            | (cache_inv << 16)
            | (cpu.cpu_adx as u64)
    }

    fn rank_cpu_powersave(&self, cpu: &CpuId) -> u64 {
        let core_type_score = if !cpu.big_core { 0 } else { 1 };
        let cap_score = cpu.cpu_cap as u64;

        ((core_type_score as u64) << 60) | (cap_score << 40) | (cpu.cpu_adx as u64)
    }

    fn build_topo_order(&self, prefer_powersave: bool) -> Vec<CpuId> {
        let mut cpu_ids = Vec::with_capacity(self.topo.all_cpus.len());
        let smt_siblings = self.topo.sibling_cpus();

        for (&numa_adx, node) in self.topo.nodes.iter() {
            for (&llc_adx, llc) in node.llcs.iter() {
                for (core_rdx, (_core_adx, core)) in llc.cores.iter().enumerate() {
                    for (cpu_rdx, (cpu_adx, cpu)) in core.cpus.iter().enumerate() {
                        let cpu_adx = *cpu_adx;
                        let pd_adx = Self::get_pd_id(&self.em, cpu_adx, llc_adx);
                        let cpu_id = CpuId {
                            numa_adx,
                            pd_adx,
                            llc_adx,
                            llc_rdx: 0,
                            llc_kernel_id: llc.kernel_id,
                            core_rdx,
                            cpu_rdx,
                            cpu_adx,
                            smt_level: cpu.smt_level,
                            cache_size: cpu.cache_size,
                            cpu_cap: cpu.cpu_capacity,
                            big_core: cpu.core_type != CoreType::Little,
                            turbo_core: cpu.core_type == CoreType::Big { turbo: true },
                            cpu_sibling: smt_siblings[cpu_adx] as usize,
                        };
                        cpu_ids.push(cpu_id);
                    }
                }
            }
        }

        if prefer_powersave {
            cpu_ids.sort_by_key(|cpu| self.rank_cpu_powersave(cpu));
        } else {
            cpu_ids.sort_by_key(|cpu| self.rank_cpu_performance(cpu));
        }

        cpu_ids
    }

    fn build_cpdom(&self, cpu_ids: &Vec<CpuId>) -> BTreeMap<ComputeDomainId, ComputeDomain> {
        let mut cpdom_map: BTreeMap<ComputeDomainId, ComputeDomain> = BTreeMap::new();
        let mut cpdom_types: BTreeMap<usize, bool> = BTreeMap::new();

        let mut next_cpdom_id = 0;
        for cpu_id in cpu_ids {
            let key = ComputeDomainId {
                numa_adx: cpu_id.numa_adx,
                llc_adx: cpu_id.llc_adx,
                llc_rdx: cpu_id.llc_rdx,
                llc_kernel_id: cpu_id.llc_kernel_id,
                is_big: cpu_id.big_core,
            };

            let entry = cpdom_map.entry(key.clone()).or_insert_with(|| {
                let id = next_cpdom_id;
                next_cpdom_id += 1;
                cpdom_types.insert(id, key.is_big);
                ComputeDomain {
                    cpdom_id: id,
                    cpdom_alt_id: id,
                    cpu_ids: Vec::new(),
                    neighbor_map: BTreeMap::new(),
                }
            });
            entry.cpu_ids.push(cpu_id.cpu_adx);
        }

        // Separate lookup to avoid immutable borrow during mutable iteration
        let lookup: BTreeMap<ComputeDomainId, usize> = cpdom_map.iter()
            .map(|(k, v)| (k.clone(), v.cpdom_id))
            .collect();

        let keys: Vec<ComputeDomainId> = cpdom_map.keys().cloned().collect();
        let mut neighbors: BTreeMap<usize, BTreeMap<usize, Vec<usize>>> = BTreeMap::new();

        for from_k in &keys {
            let from_id = lookup[from_k];
            for to_k in &keys {
                if from_k == to_k {
                    continue;
                }
                let dist = Self::dist(from_k, to_k);
                let to_id = lookup[to_k];
                neighbors
                    .entry(from_id)
                    .or_default()
                    .entry(dist)
                    .or_default()
                    .push(to_id);
            }
        }

        for (cpdom_id, dist_map) in neighbors {
            if let Some(domain) = cpdom_map.values_mut().find(|d| d.cpdom_id == cpdom_id) {
                for (dist, n_list) in dist_map {
                    let sorted = Self::circular_sort(domain.cpdom_id, &n_list);
                    domain.neighbor_map.insert(dist, sorted);
                }
            }
        }

        for (k, v) in cpdom_map.iter_mut() {
            let mut alt_k = k.clone();
            alt_k.is_big = !k.is_big;

            if let Some(&alt_id) = lookup.get(&alt_k) {
                v.cpdom_alt_id = alt_id;
            } else {
                'search: for n_list in v.neighbor_map.values() {
                    for &nid in n_list {
                        if let Some(&is_big) = cpdom_types.get(&nid) {
                            if is_big == alt_k.is_big {
                                v.cpdom_alt_id = nid;
                                break 'search;
                            }
                        }
                    }
                }
            }
        }

        cpdom_map
    }

    fn circular_sort(start: usize, the_rest: &Vec<usize>) -> Vec<usize> {
        let mut list = the_rest.clone();
        list.push(start);
        list.sort_unstable();

        if let Ok(s) = list.binary_search(&start) {
            list.rotate_left(s);
            // After rotation, 'start' is at index 0. Remove it to return only neighbors.
            list.remove(0);
        }
        list
    }

    fn get_pd_id(em: &Result<EnergyModel>, cpu_adx: usize, llc_adx: usize) -> usize {
        match em {
            Ok(em) => em
                .get_pd_by_cpu_id(cpu_adx)
                .map(|pd| pd.id)
                .unwrap_or(llc_adx),
            Err(_) => llc_adx,
        }
    }

    fn dist(from: &ComputeDomainId, to: &ComputeDomainId) -> usize {
        let mut d = 0;
        if from.is_big != to.is_big {
            d += 100;
        }
        if from.numa_adx != to.numa_adx {
            d += 10;
        } else {
            if from.llc_rdx != to.llc_rdx {
                d += 1;
            }
            if from.llc_kernel_id != to.llc_kernel_id {
                d += 1;
            }
        }
        d
    }
}

#[derive(Debug)]
struct EnergyModelOptimizer<'a> {
    em: &'a EnergyModel,
    pd_cpu_order: BTreeMap<usize, Vec<usize>>,
    cpus_topological_order: Vec<usize>,
    tot_perf: usize,
    pdss_infos: BTreeMap<usize, HashSet<PDSetInfo<'a>>>,
    perf_pdsi: BTreeMap<usize, PDSetInfo<'a>>,
    perf_cpu_order: BTreeMap<usize, PerfCpuOrder>,
}

#[derive(Debug, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
struct PDS<'a> {
    pd: &'a PerfDomain,
    ps: &'a PerfState,
}

#[derive(Debug, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
struct PDCpu<'a> {
    pd: &'a PerfDomain,
    cpu_vid: usize,
}

#[derive(Debug, Clone, Eq)]
struct PDSetInfo<'a> {
    performance: usize,
    power: usize,
    pdcpu_set: BTreeSet<PDCpu<'a>>,
    pd_id_set: BTreeSet<usize>,
}

const PD_UNIT: usize = 100_000_000;
const CPU_UNIT: usize = 100_000;
const LOOKAHEAD_CNT: usize = 10;

impl<'a> EnergyModelOptimizer<'a> {
    fn get_fake_perf_cpu_order_table(
        cpus_pf: &'a Vec<CpuId>,
        cpus_ps: &'a Vec<CpuId>,
    ) -> BTreeMap<usize, PerfCpuOrder> {
        let tot_perf: usize = cpus_pf.iter().map(|cpuid| cpuid.cpu_cap).sum();
        let pco_pf = Self::fake_pco(tot_perf, cpus_pf, false);
        let pco_ps = Self::fake_pco(tot_perf, cpus_ps, true);

        let mut map = BTreeMap::new();
        map.insert(pco_pf.perf_cap, pco_pf);
        map.insert(pco_ps.perf_cap, pco_ps);
        map
    }

    fn fake_pco(tot_perf: usize, cpuids: &'a Vec<CpuId>, powersave: bool) -> PerfCpuOrder {
        let perf_cap = if powersave { cpuids[0].cpu_cap } else { tot_perf };
        let perf_util = (perf_cap as f32) / (tot_perf.max(1) as f32);

        let cpus: Vec<usize> = cpuids.iter().map(|c| c.cpu_adx).collect();
        let (primary, overflow) = if powersave {
            let split = 1.min(cpus.len());
            (cpus[..split].to_vec(), cpus[split..].to_vec())
        } else {
            (cpus.clone(), Vec::new())
        };

        PerfCpuOrder {
            perf_cap,
            perf_util,
            cpus_perf: primary,
            cpus_ovflw: overflow,
        }
    }

    fn get_perf_cpu_order_table(
        em: &'a EnergyModel,
        cpus_pf: &'a Vec<CpuId>,
    ) -> BTreeMap<usize, PerfCpuOrder> {
        let mut emo = Self::new(em, cpus_pf);
        emo.gen_perf_cpu_order_table();
        emo.perf_cpu_order
    }

    fn new(em: &'a EnergyModel, cpus_pf: &'a Vec<CpuId>) -> Self {
        let mut pd_cpu_order = BTreeMap::new();
        let mut cpus_topological_order = Vec::with_capacity(cpus_pf.len());

        for cpuid in cpus_pf {
            pd_cpu_order
                .entry(cpuid.pd_adx)
                .or_insert_with(Vec::new)
                .push(cpuid.cpu_adx);
            cpus_topological_order.push(cpuid.cpu_adx);
        }

        EnergyModelOptimizer {
            em,
            pd_cpu_order,
            cpus_topological_order,
            tot_perf: em.perf_total(),
            pdss_infos: BTreeMap::new(),
            perf_pdsi: BTreeMap::new(),
            perf_cpu_order: BTreeMap::new(),
        }
    }

    fn gen_perf_cpu_order_table(&mut self) {
        self.gen_all_pds_combinations();
        self.gen_perf_pds_table();
        self.assign_cpu_vids();
    }

    fn gen_all_pds_combinations(&mut self) {
        let pdsi_0 = self.gen_pds_combinations(0.0);
        self.insert_pds_combinations(pdsi_0);

        let pdsi_100 = self.gen_pds_combinations(100.0);
        self.insert_pds_combinations(pdsi_100);

        self.gen_perf_cpuset_table_range(0, 100);
    }

    fn gen_perf_cpuset_table_range(&mut self, low: isize, high: isize) {
        if low > high {
            return;
        }
        let mid = low + (high - low) / 2;
        let combinations = self.gen_pds_combinations(mid as f32);
        if self.insert_pds_combinations(combinations) {
            self.gen_perf_cpuset_table_range(mid + 1, high);
            self.gen_perf_cpuset_table_range(low, mid - 1);
        }
    }

    fn gen_pds_combinations(&self, util: f32) -> Vec<PDSetInfo<'a>> {
        let pds_set = self.gen_pds_set(util);
        let n = pds_set.len();

        if n > 12 {
            return vec![PDSetInfo::new(pds_set)];
        }

        let mut results = Vec::new();
        for k in 1..n {
            results.extend(Combinations::new(pds_set.clone(), k).map(PDSetInfo::new));
        }
        results.push(PDSetInfo::new(pds_set));
        results
    }

    fn gen_pds_set(&self, util: f32) -> Vec<PDS<'a>> {
        let mut pds_set = Vec::new();
        for pd in self.em.perf_doms.values() {
            if let Some(ps) = pd.select_perf_state(util) {
                let weight = pd.span.weight();
                for _ in 0..weight {
                    pds_set.push(PDS { pd, ps });
                }
            }
        }
        pds_set.sort_unstable();
        pds_set
    }

    fn insert_pds_combinations(&mut self, new_pdsis: Vec<PDSetInfo<'a>>) -> bool {
        let mut found = false;
        for item in new_pdsis {
            let entry = self.pdss_infos.entry(item.performance).or_default();

            if entry.is_empty() {
                entry.insert(item);
                found = true;
                continue;
            }

            let current_best = entry.iter().next().unwrap().power;
            match item.power.cmp(&current_best) {
                Ordering::Less => {
                    entry.clear();
                    entry.insert(item);
                    found = true;
                }
                Ordering::Equal => {
                    if entry.insert(item) {
                        found = true;
                    }
                }
                Ordering::Greater => {}
            }
        }
        found
    }

    fn gen_perf_pds_table(&mut self) {
        let utils = [0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0];

        for &util in &utils {
            let base = self.perf_pdsi.last_key_value().map(|(_, v)| v.clone());
            let best = self.find_perf_pds_for(util, base.as_ref());

            if let Some(mut best) = best {
                if let Some(base) = base {
                    if best.pdcpu_set.is_subset(&base.pdcpu_set) {
                        best.pdcpu_set = base.pdcpu_set;
                        best.pd_id_set = base.pd_id_set;
                        self.perf_pdsi.remove(&base.performance);
                    }
                }
                self.perf_pdsi.insert(best.performance, best);
            }
        }
    }

    fn find_perf_pds_for(
        &self,
        util: f32,
        base: Option<&PDSetInfo<'a>>,
    ) -> Option<PDSetInfo<'a>> {
        let target_perf = (util * self.tot_perf as f32) as usize;
        let mut best_pdsi = None;
        let mut min_dist = usize::MAX;
        let mut lookahead = 0;

        for (&perf, set) in &self.pdss_infos {
            if perf >= target_perf {
                for pdsi in set {
                    let dist = pdsi.dist(base);
                    if dist < min_dist {
                        min_dist = dist;
                        best_pdsi = Some(pdsi.clone());
                    }
                }
                lookahead += 1;
                if lookahead >= LOOKAHEAD_CNT {
                    break;
                }
            }
        }
        best_pdsi
    }

    fn assign_cpu_vids(&mut self) {
        for (&perf_cap, pdsi) in &self.perf_pdsi {
            let mut cpus_perf = Vec::new();

            for pdcpu in &pdsi.pdcpu_set {
                if let Some(order) = self.pd_cpu_order.get(&pdcpu.pd.id) {
                    if pdcpu.cpu_vid < order.len() {
                        cpus_perf.push(order[pdcpu.cpu_vid]);
                    }
                }
            }

            cpus_perf.sort_by_key(|&id| {
                self.cpus_topological_order
                    .iter()
                    .position(|&x| x == id)
                    .unwrap_or(usize::MAX)
            });

            let perf_util = (perf_cap as f32) / (self.tot_perf as f32);

            self.perf_cpu_order.insert(
                perf_cap,
                PerfCpuOrder {
                    perf_cap,
                    perf_util,
                    cpus_perf,
                    cpus_ovflw: Vec::new(),
                },
            );
        }

        let keys: Vec<usize> = self.perf_cpu_order.keys().cloned().collect();
        if keys.is_empty() {
            return;
        }

        for i in 1..keys.len() {
            let current_cap = keys[i - 1];

            let used_cpus: HashSet<usize> = self.perf_cpu_order[&current_cap]
                .cpus_perf
                .iter()
                .cloned()
                .collect();

            let mut overflow = Vec::new();
            for &cap in &keys[i..] {
                for &cpu in &self.perf_cpu_order[&cap].cpus_perf {
                    if !used_cpus.contains(&cpu) && !overflow.contains(&cpu) {
                        overflow.push(cpu);
                    }
                }
            }

            self.perf_cpu_order
                .get_mut(&current_cap)
                .unwrap()
                .cpus_ovflw = overflow;
        }
    }
}

impl<'a> PDSetInfo<'a> {
    fn new(pds_set: Vec<PDS<'a>>) -> Self {
        let mut performance = 0;
        let mut power = 0;
        let mut pd_id_set = BTreeSet::new();
        let mut pds_groups: BTreeMap<&PDS<'a>, usize> = BTreeMap::new();

        for pds in &pds_set {
            performance += pds.ps.performance;
            power += pds.ps.power;
            pd_id_set.insert(pds.pd.id);
            *pds_groups.entry(pds).or_default() += 1;
        }

        let mut pdcpu_set = BTreeSet::new();
        for (pds, count) in pds_groups {
            for vid in 0..count {
                pdcpu_set.insert(PDCpu {
                    pd: pds.pd,
                    cpu_vid: vid,
                });
            }
        }

        PDSetInfo {
            performance,
            power,
            pdcpu_set,
            pd_id_set,
        }
    }

    fn dist(&self, base: Option<&PDSetInfo<'a>>) -> usize {
        let nr_pds = self.pd_id_set.len();
        let nr_cpus = self.pdcpu_set.len();

        let overlap = base
            .map(|b| self.pd_id_set.intersection(&b.pd_id_set).count())
            .unwrap_or(0);

        ((nr_pds - overlap) * PD_UNIT)
            + ((*NR_CPU_IDS - nr_cpus) * CPU_UNIT)
            + (*NR_CPU_IDS
                - self
                    .pd_id_set
                    .first()
                    .cloned()
                    .unwrap_or(0))
    }
}

impl PartialEq for PDSetInfo<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.performance == other.performance
            && self.power == other.power
            && self.pdcpu_set == other.pdcpu_set
    }
}

impl Hash for PDSetInfo<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.pdcpu_set.hash(state);
    }
}

impl PartialEq for PerfCpuOrder {
    fn eq(&self, other: &Self) -> bool {
        self.perf_cap == other.perf_cap
    }
}

impl fmt::Display for PerfCpuOrder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "capacity bound:  {} ({}%)\n",
            self.perf_cap,
            self.perf_util * 100.0
        )?;
        write!(f, "  primary CPUs:  {:?}\n", self.cpus_perf)?;
        write!(f, "  overflow CPUs: {:?}", self.cpus_ovflw)?;
        Ok(())
    }
}
