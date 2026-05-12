/*
 *  Burst-Oriented Response Enhancer (BORE) CPU Scheduler
 *  Copyright (C) 2021-2025 Masahito Suzuki <firelzrd@gmail.com>
 */

#include <linux/cpuset.h>
#include <linux/kernel.h>
#include <linux/limits.h>
#include <linux/percpu.h>
#include <linux/sched/bore.h>
#include <linux/sched/task.h>
#include <linux/workqueue.h>
#include "sched.h"

#ifdef CONFIG_X86
#include <asm/cpufeature.h>
#include <asm/msr.h>
#include <asm/msr-index.h>
#endif

#ifdef CONFIG_SCHED_BORE

DEFINE_STATIC_KEY_TRUE(sched_bore_key);
u8 __read_mostly sched_bore = 1;
u8 __read_mostly sched_burst_inherit_type = 2;
u8 __read_mostly sched_burst_smoothness = 1;
u8 __read_mostly sched_burst_penalty_offset = 24;
uint __read_mostly sched_burst_penalty_scale = 1536;
uint __read_mostly sched_burst_cache_lifetime = 75000000;

static int __maybe_unused maxval_6_bits = 63;
static int __maybe_unused maxval_12_bits = 4095;
static int __maybe_unused maxval_100 = 100;

#define MAX_BURST_PENALTY        ((40U << 8) - 1U)
#define BURST_CACHE_SAMPLE_LIMIT 63U
#define BURST_CACHE_SCAN_LIMIT   (BURST_CACHE_SAMPLE_LIMIT * 2U)

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
	237, 238, 239, 239, 240, 241, 241, 242, 243, 244, 244, 245, 246, 247, 247, 248,
};

static u32 bore_reciprocal_lut[BURST_CACHE_SAMPLE_LIMIT + 1U];

DEFINE_STATIC_KEY_TRUE(sched_burst_inherit_key);
DEFINE_STATIC_KEY_TRUE(sched_burst_ancestor_key);

struct bore_penalty_params {
	u16 offset_q8;
	u16 scale;
	u16 exp_floor_q8;
	u16 sat_delta;
	u64 burst_floor;
	u8 gen;
};

static DEFINE_PER_CPU(struct bore_penalty_params, bore_pparams);
static atomic_t bore_pparam_gen = ATOMIC_INIT(1);
static u8 __read_mostly bore_pparam_gen_shadow = 1;

#ifdef CONFIG_X86
static u8 __read_mostly sched_burst_itd_enable = 1;
static u32 __read_mostly sched_burst_itd_async_interval_ms = 10;
static u32 __read_mostly sched_burst_itd_cap_pct = 60;

#define BORE_ITD_MAX_CLASSES 8

static u32 __read_mostly sched_burst_itd_bias_pcore_pct[BORE_ITD_MAX_CLASSES] = {
	[0] = 0, [1] = 5, [2] = 45, [3] = 90,
	[4 ... BORE_ITD_MAX_CLASSES - 1] = 0,
};

static u32 __read_mostly sched_burst_itd_bias_ecore_pct[BORE_ITD_MAX_CLASSES] = {
	[0] = 0, [1] = 10, [2] = 50, [3] = 100,
	[4 ... BORE_ITD_MAX_CLASSES - 1] = 0,
};

static const u16 bore_pct_to_q8_lut[101] = {
	[0 ... 100] = 0,
	[1] = 2, [2] = 5, [3] = 7, [4] = 10, [5] = 12,
	[6] = 15, [7] = 17, [8] = 20, [9] = 23, [10] = 25,
	[11] = 28, [12] = 30, [13] = 33, [14] = 35, [15] = 38,
	[16] = 40, [17] = 43, [18] = 46, [19] = 48, [20] = 51,
	[21] = 53, [22] = 56, [23] = 58, [24] = 61, [25] = 64,
	[26] = 66, [27] = 69, [28] = 71, [29] = 74, [30] = 76,
	[31] = 79, [32] = 81, [33] = 84, [34] = 87, [35] = 89,
	[36] = 92, [37] = 94, [38] = 97, [39] = 99, [40] = 102,
	[41] = 104, [42] = 107, [43] = 110, [44] = 112, [45] = 115,
	[46] = 117, [47] = 120, [48] = 122, [49] = 125, [50] = 128,
	[51] = 130, [52] = 133, [53] = 135, [54] = 138, [55] = 140,
	[56] = 143, [57] = 145, [58] = 148, [59] = 151, [60] = 153,
	[61] = 156, [62] = 158, [63] = 161, [64] = 163, [65] = 166,
	[66] = 168, [67] = 171, [68] = 174, [69] = 176, [70] = 179,
	[71] = 181, [72] = 184, [73] = 186, [74] = 189, [75] = 192,
	[76] = 194, [77] = 197, [78] = 199, [79] = 202, [80] = 204,
	[81] = 207, [82] = 209, [83] = 212, [84] = 215, [85] = 217,
	[86] = 220, [87] = 222, [88] = 225, [89] = 227, [90] = 230,
	[91] = 232, [92] = 235, [93] = 238, [94] = 240, [95] = 243,
	[96] = 245, [97] = 248, [98] = 250, [99] = 253, [100] = 256,
};

struct bore_itd_state {
	u8 cached_class;
	u8 cached_valid;
	u8 inited;
	u8 is_pcore;
	u16 cached_q8;
	u8 _pad1[58];
	u64 last_sample_ns;
	u8 _pad2[56];
} ____cacheline_aligned_in_smp;

static DEFINE_PER_CPU(struct bore_itd_state, bore_itd_state);
static DEFINE_PER_CPU(struct delayed_work, bore_itd_sample_work);
DEFINE_STATIC_KEY_FALSE(bore_itd_key);

#ifndef MSR_IA32_HW_FEEDBACK_CHAR
#define MSR_IA32_HW_FEEDBACK_CHAR 0x17f
#endif
#endif

static __always_inline void bore_bump_pparam_gen(void)
{
	WRITE_ONCE(bore_pparam_gen_shadow, (u8)atomic_inc_return(&bore_pparam_gen));
}

static __always_inline const struct bore_penalty_params *bore_get_params(void)
{
	struct bore_penalty_params *pp;
	u8 gen_now;
	u8 floor_msb;

	pp = this_cpu_ptr(&bore_pparams);
	gen_now = READ_ONCE(bore_pparam_gen_shadow);

	if (likely(READ_ONCE(pp->gen) == gen_now))
		return pp;

	pp->offset_q8 = (u16)READ_ONCE(sched_burst_penalty_offset) << 8;
	pp->scale = max_t(u16, (u16)READ_ONCE(sched_burst_penalty_scale), 1U);
	pp->exp_floor_q8 = (pp->offset_q8 > 248U) ? (u16)(pp->offset_q8 - 248U) : 0U;
	pp->sat_delta = min_t(u32,
			      DIV_ROUND_UP((u32)MAX_BURST_PENALTY << 10, (u32)pp->scale),
			      U16_MAX);

	floor_msb = (u8)(pp->exp_floor_q8 >> 8);
	if (floor_msb >= 63U)
		pp->burst_floor = U64_MAX;
	else
		pp->burst_floor = (1ULL << (floor_msb + 1U)) - 1ULL;

	smp_wmb();
	WRITE_ONCE(pp->gen, gen_now);

	return pp;
}

#ifdef CONFIG_X86
static __always_inline bool bore_itd_is_available(void)
{
#ifdef X86_FEATURE_HFI
	return READ_ONCE(sched_burst_itd_enable) && boot_cpu_has(X86_FEATURE_HFI);
#else
	return false;
#endif
}

static void bore_itd_sample_work_fn(struct work_struct *work)
{
	struct bore_itd_state *st;
	u64 msr_val;
	u32 delay_ms;
	int cpu;

	(void)work;

	if (!READ_ONCE(sched_burst_itd_enable))
		return;

	cpu = smp_processor_id();
	st = this_cpu_ptr(&bore_itd_state);

	if (rdmsrq_safe(MSR_IA32_HW_FEEDBACK_CHAR, &msr_val)) {
		WRITE_ONCE(st->cached_valid, 0);
	} else {
		u8 valid = (u8)((msr_val >> 63) & 0x1);

		if (valid) {
			u8 classid = (u8)(msr_val & 0xff);
			u32 pct;
			u32 cap;
			u32 eff_pct;
			u32 delta_q8;
			u16 q8 = 256U;

			if (classid < BORE_ITD_MAX_CLASSES) {
				pct = READ_ONCE(st->is_pcore) ?
					READ_ONCE(sched_burst_itd_bias_pcore_pct[classid]) :
					READ_ONCE(sched_burst_itd_bias_ecore_pct[classid]);

				cap = clamp_t(u32, READ_ONCE(sched_burst_itd_cap_pct), 0U, 100U);
				pct = clamp_t(u32, pct, 0U, 100U);
				eff_pct = min(cap, pct);

				if (eff_pct) {
					delta_q8 = bore_pct_to_q8_lut[eff_pct];
					if (READ_ONCE(st->is_pcore))
						q8 = (delta_q8 >= 256U) ? 0U : (u16)(256U - delta_q8);
					else
						q8 = (u16)min_t(u32, 256U + delta_q8, 1024U);
				}
			}

			WRITE_ONCE(st->cached_class, classid);
			WRITE_ONCE(st->cached_q8, q8);
			smp_wmb();
			WRITE_ONCE(st->cached_valid, 1);
		} else {
			WRITE_ONCE(st->cached_valid, 0);
		}
	}

	delay_ms = max_t(u32, 1U, READ_ONCE(sched_burst_itd_async_interval_ms));
	schedule_delayed_work_on(cpu, this_cpu_ptr(&bore_itd_sample_work), msecs_to_jiffies(delay_ms));
}

static __always_inline bool bore_itd_read_q8_fast(u16 *q8)
{
	struct bore_itd_state *st;

	st = this_cpu_ptr(&bore_itd_state);
	if (!READ_ONCE(st->cached_valid))
		return false;

	smp_rmb();
	*q8 = READ_ONCE(st->cached_q8);
	return true;
}

static __always_inline void bore_itd_init_this_cpu_if_needed(void)
{
	struct bore_itd_state *st;
	u32 delay_ms;
	int cpu;

	if (unlikely(system_state < SYSTEM_RUNNING))
		return;

	st = this_cpu_ptr(&bore_itd_state);
	if (READ_ONCE(st->inited))
		return;

	cpu = raw_smp_processor_id();
	WRITE_ONCE(st->is_pcore, arch_asym_cpu_priority(cpu) >= 768);
	WRITE_ONCE(st->cached_q8, 256U);
	WRITE_ONCE(st->cached_valid, 0);

	INIT_DELAYED_WORK(this_cpu_ptr(&bore_itd_sample_work), bore_itd_sample_work_fn);
	delay_ms = max_t(u32, 1U, READ_ONCE(sched_burst_itd_async_interval_ms));
	schedule_delayed_work_on(cpu, this_cpu_ptr(&bore_itd_sample_work), msecs_to_jiffies(delay_ms));

	WRITE_ONCE(st->inited, 1);
}

static void bore_itd_cancel_all_works(void)
{
	int cpu;

	for_each_online_cpu(cpu)
		cancel_delayed_work_sync(per_cpu_ptr(&bore_itd_sample_work, cpu));
}

static __always_inline u32 bore_apply_itd_bias(u32 penalty)
{
	u16 q8;
	u64 prod;

	if (unlikely(!penalty) || !static_branch_unlikely(&bore_itd_key))
		return penalty;

	bore_itd_init_this_cpu_if_needed();

	if (!bore_itd_read_q8_fast(&q8))
		return penalty;

	if (likely(q8 == 256U))
		return penalty;

	prod = (u64)penalty * (u64)q8;
	return min_t(u32, (u32)(prod >> 8), MAX_BURST_PENALTY);
}

static void bore_itd_update_key_handler(void)
{
	if (bore_itd_is_available())
		static_branch_enable(&bore_itd_key);
	else
		static_branch_disable(&bore_itd_key);
}
#else
static __always_inline u32 bore_apply_itd_bias(u32 penalty)
{
	return penalty;
}
#endif

static __always_inline u32 calc_burst_penalty(u64 burst_time)
{
	const struct bore_penalty_params *pp;
	u32 offset_q8;
	u32 exp_q8;
	u32 greed;
	u32 delta;
	u32 scaled_penalty;
	u32 frac_idx;
	int lz;

	if (unlikely(!burst_time))
		return 0U;

	pp = bore_get_params();

	if (likely(burst_time <= pp->burst_floor))
		return 0U;

	offset_q8 = pp->offset_q8;

	lz = __builtin_clzll(burst_time);
	exp_q8 = (64U - (u32)lz) << 8;

	if (likely(exp_q8 <= pp->exp_floor_q8))
		return 0U;

	if (likely(lz < 56))
		frac_idx = (u32)((burst_time << (lz + 1)) >> 56);
	else
		frac_idx = 0U;

	greed = exp_q8 + (u32)bore_log2_lut[frac_idx];
	if (likely(greed <= offset_q8))
		return 0U;

	delta = greed - offset_q8;
	if (unlikely(delta >= pp->sat_delta))
		return MAX_BURST_PENALTY;

	scaled_penalty = (delta * pp->scale) >> 10;
	return min_t(u32, scaled_penalty, MAX_BURST_PENALTY);
}

static __always_inline u64 rescale_slice(u64 delta, u8 old_prio, u8 new_prio)
{
	u64 unscaled;

	unscaled = mul_u64_u32_shr(delta, sched_prio_to_weight[old_prio], 10);
	return mul_u64_u32_shr(unscaled, sched_prio_to_wmult[new_prio], 22);
}

static __always_inline u32 binary_smooth(u32 new_val, u32 old_val)
{
	u32 shift;

	if (new_val <= old_val)
		return new_val;

	shift = min_t(u32, READ_ONCE(sched_burst_smoothness), 31U);
	if (!shift)
		return new_val;

	return old_val + DIV_ROUND_UP(new_val - old_val, 1U << shift);
}

static __always_inline u64 bore_u64_add_sat(u64 a, u64 b)
{
	u64 s = a + b;

	if (unlikely(s < a))
		return U64_MAX;
	return s;
}

static __always_inline u8 bore_penalty_to_score(u16 penalty)
{
	u32 score = (u32)penalty >> 8;

	if (unlikely(score > 39U))
		score = 39U;
	return (u8)score;
}

static void reweight_task_by_prio(struct task_struct *p, int prio)
{
	struct sched_entity *se;
	unsigned long weight;

	if (task_has_idle_policy(p))
		return;

	prio = clamp_t(int, prio, 0, NICE_WIDTH - 1);
	se = &p->se;
	weight = scale_load(sched_prio_to_weight[prio]);

	if (se->on_rq) {
		p->bore.stop_update = true;
		reweight_entity(cfs_rq_of(se), se, weight);
		p->bore.stop_update = false;
	} else {
		se->load.weight = weight;
	}

	se->load.inv_weight = sched_prio_to_wmult[prio];
}

u8 effective_prio_bore(struct task_struct *p)
{
	int prio;

	prio = p->static_prio - MAX_RT_PRIO;
	if (static_branch_likely(&sched_bore_key))
		prio += p->bore.score;

	return (u8)clamp_t(int, prio, 0, 39);
}

static void update_penalty(struct task_struct *p)
{
	struct bore_ctx *ctx;
	u16 new_penalty;
	u8 old_score;
	u8 new_score;
	int base_prio;
	int old_prio;
	int new_prio;

	ctx = &p->bore;

	new_penalty = max_t(u16, ctx->curr_penalty, ctx->prev_penalty);
	if (unlikely(p->flags & PF_KTHREAD))
		new_penalty = 0;

	if (READ_ONCE(ctx->penalty) != new_penalty)
		WRITE_ONCE(ctx->penalty, new_penalty);

	old_score = READ_ONCE(ctx->score);
	new_score = bore_penalty_to_score(new_penalty);

	if (likely(new_score == old_score))
		return;

	WRITE_ONCE(ctx->score, new_score);

	base_prio = clamp_t(int, p->static_prio - MAX_RT_PRIO, 0, 39);
	old_prio = clamp_t(int, base_prio + old_score, 0, 39);
	new_prio = clamp_t(int, base_prio + new_score, 0, 39);

	if (likely(new_prio != old_prio))
		reweight_task_by_prio(p, new_prio);
}

void update_curr_bore(struct task_struct *p, u64 delta_exec)
{
	struct bore_ctx *ctx;
	u32 curr_penalty;
	u16 old_curr_penalty;
	u16 prev_penalty;
	u64 burst_time;

	if (!static_branch_likely(&sched_bore_key))
		return;

	ctx = &p->bore;
	if (unlikely(ctx->stop_update) || unlikely(!delta_exec))
		return;

	burst_time = bore_u64_add_sat(ctx->burst_time, delta_exec);
	ctx->burst_time = burst_time;

	curr_penalty = calc_burst_penalty(burst_time);
	curr_penalty = bore_apply_itd_bias(curr_penalty);

	old_curr_penalty = READ_ONCE(ctx->curr_penalty);
	if (likely(curr_penalty == old_curr_penalty))
		return;

	ctx->curr_penalty = (u16)curr_penalty;
	if (likely(curr_penalty <= old_curr_penalty))
		return;

	prev_penalty = READ_ONCE(ctx->prev_penalty);
	if (likely(curr_penalty <= prev_penalty))
		return;

	update_penalty(p);
}

void restart_burst_bore(struct task_struct *p)
{
	struct bore_ctx *ctx;
	u32 smoothed;

	ctx = &p->bore;
	smoothed = binary_smooth(ctx->curr_penalty, ctx->prev_penalty);

	ctx->prev_penalty = (u16)smoothed;
	ctx->curr_penalty = 0;
	ctx->burst_time = 0;

	update_penalty(p);
}

void restart_burst_rescale_deadline_bore(struct task_struct *p)
{
	struct sched_entity *se;
	s64 vremain;
	s64 vscaled;
	u64 abs_vremain;
	u64 scaled_u;
	u8 old_prio;
	u8 new_prio;

	se = &p->se;
	vremain = se->deadline - se->vruntime;

	old_prio = effective_prio_bore(p);
	restart_burst_bore(p);
	new_prio = effective_prio_bore(p);

	if (old_prio <= new_prio)
		return;

	if (unlikely(vremain < 0))
		abs_vremain = (u64)(-(vremain + 1)) + 1ULL;
	else
		abs_vremain = (u64)vremain;

	scaled_u = rescale_slice(abs_vremain, old_prio, new_prio);

	if (unlikely(vremain < 0)) {
		if (unlikely(scaled_u >= (u64)S64_MAX + 1ULL))
			vscaled = S64_MIN;
		else
			vscaled = -(s64)scaled_u;
	} else {
		if (unlikely(scaled_u > (u64)S64_MAX))
			vscaled = S64_MAX;
		else
			vscaled = (s64)scaled_u;
	}

	if (unlikely(vscaled > 0 && se->vruntime > S64_MAX - vscaled))
		se->deadline = S64_MAX;
	else if (unlikely(vscaled < 0 && se->vruntime < S64_MIN - vscaled))
		se->deadline = S64_MIN;
	else
		se->deadline = se->vruntime + vscaled;
}

static __always_inline bool task_is_bore_eligible(struct task_struct *p)
{
	return p && p->sched_class == &fair_sched_class && !p->exit_state;
}

#ifndef for_each_child_task
#define for_each_child_task(p, t) \
	list_for_each_entry_rcu(t, &(p)->children, sibling)
#endif

static __always_inline u32 count_children_upto2(struct task_struct *p)
{
	struct list_head *head;
	struct list_head *first;

	head = &p->children;
	first = READ_ONCE(head->next);
	if (first == head)
		return 0U;

	return 1U + (READ_ONCE(first->next) != head);
}

static __always_inline bool burst_cache_try_read(struct bore_bc *bc, u64 now, u32 *penalty)
{
	struct bore_bc bc_val;
	u64 timestamp;
	u64 lifetime;

	bc_val.value = READ_ONCE(bc->value);
	timestamp = (u64)bc_val.timestamp << BORE_BC_TIMESTAMP_SHIFT;

	if (unlikely(now > timestamp)) {
		lifetime = (u64)READ_ONCE(sched_burst_cache_lifetime);
		if (now - timestamp > lifetime)
			return false;
	}

	*penalty = (u32)bc_val.penalty;
	return true;
}

static u32 update_burst_cache(struct bore_bc *bc, struct task_struct *owner,
			      u32 count, u64 total, u64 now)
{
	struct bore_bc new_bc;
	u32 average;

	if (!count) {
		average = 0U;
	} else if (count == 1U) {
		average = (u32)min_t(u64, total, U32_MAX);
	} else {
		average = (u32)((total * bore_reciprocal_lut[count]) >> 32);
	}

	new_bc.penalty = max_t(u32, average, owner->bore.penalty);
	new_bc.timestamp = now >> BORE_BC_TIMESTAMP_SHIFT;
	WRITE_ONCE(bc->value, new_bc.value);

	return (u32)new_bc.penalty;
}

static u32 inherit_from_parent(struct task_struct *parent, u64 clone_flags, u64 now)
{
	struct bore_bc *bc;
	u32 cached_penalty;
	struct task_struct *child;
	u32 count = 0;
	u32 scan_count = 0;
	u64 total = 0;

	if (clone_flags & CLONE_PARENT)
		parent = rcu_dereference(parent->real_parent);

	if (unlikely(!parent))
		return 0U;

	bc = &parent->bore.subtree;
	if (likely(burst_cache_try_read(bc, now, &cached_penalty)))
		return cached_penalty;

	for_each_child_task(parent, child) {
		if (count >= BURST_CACHE_SAMPLE_LIMIT || scan_count++ >= BURST_CACHE_SCAN_LIMIT)
			break;

		if (!task_is_bore_eligible(child))
			continue;

		count++;
		total += READ_ONCE(child->bore.penalty);
	}

	return update_burst_cache(bc, parent, count, total, now);
}

static u32 inherit_from_ancestor_hub(struct task_struct *parent, u64 clone_flags, u64 now)
{
	struct task_struct *ancestor;
	u32 sole_child_count = 0;
	u32 cached_penalty;
	struct task_struct *direct_child;
	u32 count = 0;
	u32 scan_count = 0;
	u64 total = 0;

	ancestor = parent;
	if (clone_flags & CLONE_PARENT) {
		ancestor = rcu_dereference(ancestor->real_parent);
		sole_child_count = 1;
	}

	if (unlikely(!ancestor))
		return 0U;

	while (true) {
		struct task_struct *next;

		next = rcu_dereference(ancestor->real_parent);
		if (!next || next == ancestor)
			break;

		if (count_children_upto2(ancestor) > sole_child_count)
			break;

		ancestor = next;
		sole_child_count = 1;
	}

	if (likely(burst_cache_try_read(&ancestor->bore.subtree, now, &cached_penalty)))
		return cached_penalty;

	for_each_child_task(ancestor, direct_child) {
		struct task_struct *descendant;

		if (count >= BURST_CACHE_SAMPLE_LIMIT || scan_count++ >= BURST_CACHE_SCAN_LIMIT)
			break;

		descendant = direct_child;
		while (count_children_upto2(descendant) == 1U) {
			struct task_struct *next_descendant;

			next_descendant = list_first_or_null_rcu(&descendant->children,
								 struct task_struct, sibling);
			if (!next_descendant)
				break;

			descendant = next_descendant;
		}

		if (!task_is_bore_eligible(descendant))
			continue;

		count++;
		total += READ_ONCE(descendant->bore.penalty);
	}

	return update_burst_cache(&ancestor->bore.subtree, ancestor, count, total, now);
}

static u32 inherit_from_thread_group(struct task_struct *p, u64 now)
{
	struct task_struct *leader;
	struct bore_bc *bc;
	u32 cached_penalty;
	struct task_struct *sibling;
	u32 count = 0;
	u32 scan_count = 0;
	u64 total = 0;

	leader = READ_ONCE(p->group_leader);
	if (unlikely(!leader))
		return 0U;

	bc = &leader->bore.group;
	if (likely(burst_cache_try_read(bc, now, &cached_penalty)))
		return cached_penalty;

	for_each_thread(leader, sibling) {
		if (count >= BURST_CACHE_SAMPLE_LIMIT || scan_count++ >= BURST_CACHE_SCAN_LIMIT)
			break;

		if (!task_is_bore_eligible(sibling))
			continue;

		count++;
		total += READ_ONCE(sibling->bore.penalty);
	}

	return update_burst_cache(bc, leader, count, total, now);
}

void task_fork_bore(struct task_struct *p, struct task_struct *parent, u64 clone_flags, u64 now)
{
	struct bore_ctx *ctx;
	u32 inherited_penalty = 0U;

	ctx = &p->bore;
	ctx->burst_time = 0;
	ctx->prev_penalty = 0;
	ctx->curr_penalty = 0;
	ctx->penalty = 0;
	ctx->stop_update = false;
	ctx->futex_waiting = false;
	ctx->subtree.value = 0;
	ctx->group.value = 0;

	if (unlikely(!parent))
		return;

	if (!static_branch_likely(&sched_bore_key) || !task_is_bore_eligible(p))
		return;

	rcu_read_lock();

	if (clone_flags & CLONE_THREAD)
		inherited_penalty = inherit_from_thread_group(parent, now);
	else if (static_branch_likely(&sched_burst_inherit_key))
		inherited_penalty = static_branch_likely(&sched_burst_ancestor_key) ?
			inherit_from_ancestor_hub(parent, clone_flags, now) :
			inherit_from_parent(parent, clone_flags, now);

	rcu_read_unlock();

	ctx->prev_penalty = (u16)inherited_penalty;
	ctx->penalty = (u16)inherited_penalty;

	update_penalty(p);
}

void reset_task_bore(struct task_struct *p)
{
	memset(&p->bore, 0, sizeof(struct bore_ctx));
}

static void update_inherit_type(void)
{
	switch (READ_ONCE(sched_burst_inherit_type)) {
	case 1:
		static_branch_enable(&sched_burst_inherit_key);
		static_branch_disable(&sched_burst_ancestor_key);
		break;
	case 2:
		static_branch_enable(&sched_burst_inherit_key);
		static_branch_enable(&sched_burst_ancestor_key);
		break;
	default:
		static_branch_disable(&sched_burst_inherit_key);
		static_branch_disable(&sched_burst_ancestor_key);
		break;
	}
}

void __init sched_init_bore(void)
{
	u32 i;

	pr_info("%s %s by %s\n", SCHED_BORE_PROGNAME, SCHED_BORE_VERSION, SCHED_BORE_AUTHOR);

	for (i = 1U; i <= BURST_CACHE_SAMPLE_LIMIT; i++)
		bore_reciprocal_lut[i] = (u32)div64_u64(0xffffffffULL + i, i);

	reset_task_bore(&init_task);
	update_inherit_type();

#ifdef CONFIG_X86
	bore_itd_update_key_handler();
#endif
}

static void readjust_all_task_weights(void)
{
	struct task_struct *g;
	struct task_struct *t;

	rcu_read_lock();
	for_each_process(g) {
		for_each_thread(g, t) {
			struct rq *rq;
			struct rq_flags rf;

			if (!task_is_bore_eligible(t))
				continue;

			if (!tryget_task_struct(t))
				continue;

			rq = task_rq_lock(t, &rf);
			update_rq_clock(rq);
			reweight_task_by_prio(t, effective_prio_bore(t));
			task_rq_unlock(rq, t, &rf);
			put_task_struct(t);
		}
	}
	rcu_read_unlock();
}

int sched_bore_update_handler(const struct ctl_table *table,
			      int write, void __user *buffer,
			      size_t *lenp, loff_t *ppos)
{
	int ret;

	ret = proc_dou8vec_minmax(table, write, buffer, lenp, ppos);
	if (ret || !write)
		return ret;

	if (sched_bore)
		static_branch_enable(&sched_bore_key);
	else
		static_branch_disable(&sched_bore_key);

	readjust_all_task_weights();
	return 0;
}

int sched_burst_inherit_type_update_handler(const struct ctl_table *table,
					    int write, void __user *buffer,
					    size_t *lenp, loff_t *ppos)
{
	int ret;

	ret = proc_dou8vec_minmax(table, write, buffer, lenp, ppos);
	if (ret || !write)
		return ret;

	update_inherit_type();
	return 0;
}

static int sched_burst_penalty_offset_update_handler(const struct ctl_table *table,
						     int write, void __user *buffer,
						     size_t *lenp, loff_t *ppos)
{
	u8 old;
	int ret;

	old = READ_ONCE(sched_burst_penalty_offset);
	ret = proc_dou8vec_minmax(table, write, buffer, lenp, ppos);

	if (!ret && write && old != READ_ONCE(sched_burst_penalty_offset))
		bore_bump_pparam_gen();

	return ret;
}

static int sched_burst_penalty_scale_update_handler(const struct ctl_table *table,
						    int write, void __user *buffer,
						    size_t *lenp, loff_t *ppos)
{
	uint old;
	int ret;

	old = READ_ONCE(sched_burst_penalty_scale);
	ret = proc_douintvec_minmax(table, write, buffer, lenp, ppos);

	if (!ret && write && old != READ_ONCE(sched_burst_penalty_scale))
		bore_bump_pparam_gen();

	return ret;
}

#ifdef CONFIG_X86
static int sched_burst_itd_enable_update_handler(const struct ctl_table *table,
						 int write, void __user *buffer,
						 size_t *lenp, loff_t *ppos)
{
	u8 old;
	int ret;

	old = READ_ONCE(sched_burst_itd_enable);
	ret = proc_dou8vec_minmax(table, write, buffer, lenp, ppos);

	if (!ret && write && old != READ_ONCE(sched_burst_itd_enable)) {
		if (READ_ONCE(sched_burst_itd_enable))
			bore_itd_update_key_handler();
		else {
			static_branch_disable(&bore_itd_key);
			bore_itd_cancel_all_works();
		}
	}

	return ret;
}
#endif

#ifdef CONFIG_SYSCTL
static struct ctl_table sched_bore_sysctls[] = {
	{
		.procname = "sched_bore",
		.data = &sched_bore,
		.maxlen = sizeof(u8),
		.mode = 0644,
		.proc_handler = sched_bore_update_handler,
		.extra1 = SYSCTL_ZERO,
		.extra2 = SYSCTL_ONE,
	},
	{
		.procname = "sched_burst_inherit_type",
		.data = &sched_burst_inherit_type,
		.maxlen = sizeof(u8),
		.mode = 0644,
		.proc_handler = sched_burst_inherit_type_update_handler,
		.extra1 = SYSCTL_ZERO,
		.extra2 = SYSCTL_TWO,
	},
	{
		.procname = "sched_burst_smoothness",
		.data = &sched_burst_smoothness,
		.maxlen = sizeof(u8),
		.mode = 0644,
		.proc_handler = proc_dou8vec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = SYSCTL_THREE,
	},
	{
		.procname = "sched_burst_penalty_offset",
		.data = &sched_burst_penalty_offset,
		.maxlen = sizeof(u8),
		.mode = 0644,
		.proc_handler = sched_burst_penalty_offset_update_handler,
		.extra1 = SYSCTL_ZERO,
		.extra2 = &maxval_6_bits,
	},
	{
		.procname = "sched_burst_penalty_scale",
		.data = &sched_burst_penalty_scale,
		.maxlen = sizeof(uint),
		.mode = 0644,
		.proc_handler = sched_burst_penalty_scale_update_handler,
		.extra1 = SYSCTL_ZERO,
		.extra2 = &maxval_12_bits,
	},
	{
		.procname = "sched_burst_cache_lifetime",
		.data = &sched_burst_cache_lifetime,
		.maxlen = sizeof(uint),
		.mode = 0644,
		.proc_handler = proc_douintvec,
	},
#ifdef CONFIG_X86
	{
		.procname = "sched_burst_itd_enable",
		.data = &sched_burst_itd_enable,
		.maxlen = sizeof(u8),
		.mode = 0644,
		.proc_handler = sched_burst_itd_enable_update_handler,
		.extra1 = SYSCTL_ZERO,
		.extra2 = SYSCTL_ONE,
	},
	{
		.procname = "sched_burst_itd_async_interval_ms",
		.data = &sched_burst_itd_async_interval_ms,
		.maxlen = sizeof(u32),
		.mode = 0644,
		.proc_handler = proc_douintvec,
	},
	{
		.procname = "sched_burst_itd_cap_pct",
		.data = &sched_burst_itd_cap_pct,
		.maxlen = sizeof(u32),
		.mode = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = &maxval_100,
	},
	{
		.procname = "sched_burst_itd_bias_pcore_pct_c0",
		.data = &sched_burst_itd_bias_pcore_pct[0],
		.maxlen = sizeof(u32),
		.mode = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = &maxval_100,
	},
	{
		.procname = "sched_burst_itd_bias_pcore_pct_c1",
		.data = &sched_burst_itd_bias_pcore_pct[1],
		.maxlen = sizeof(u32),
		.mode = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = &maxval_100,
	},
	{
		.procname = "sched_burst_itd_bias_pcore_pct_c2",
		.data = &sched_burst_itd_bias_pcore_pct[2],
		.maxlen = sizeof(u32),
		.mode = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = &maxval_100,
	},
	{
		.procname = "sched_burst_itd_bias_pcore_pct_c3",
		.data = &sched_burst_itd_bias_pcore_pct[3],
		.maxlen = sizeof(u32),
		.mode = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = &maxval_100,
	},
	{
		.procname = "sched_burst_itd_bias_ecore_pct_c0",
		.data = &sched_burst_itd_bias_ecore_pct[0],
		.maxlen = sizeof(u32),
		.mode = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = &maxval_100,
	},
	{
		.procname = "sched_burst_itd_bias_ecore_pct_c1",
		.data = &sched_burst_itd_bias_ecore_pct[1],
		.maxlen = sizeof(u32),
		.mode = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = &maxval_100,
	},
	{
		.procname = "sched_burst_itd_bias_ecore_pct_c2",
		.data = &sched_burst_itd_bias_ecore_pct[2],
		.maxlen = sizeof(u32),
		.mode = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = &maxval_100,
	},
	{
		.procname = "sched_burst_itd_bias_ecore_pct_c3",
		.data = &sched_burst_itd_bias_ecore_pct[3],
		.maxlen = sizeof(u32),
		.mode = 0644,
		.proc_handler = proc_douintvec_minmax,
		.extra1 = SYSCTL_ZERO,
		.extra2 = &maxval_100,
	},
#endif
};

static int __init sched_bore_sysctl_init(void)
{
	register_sysctl_init("kernel", sched_bore_sysctls);
#ifdef CONFIG_X86
	bore_itd_update_key_handler();
#endif
	return 0;
}
late_initcall(sched_bore_sysctl_init);
#endif

#endif /* CONFIG_SCHED_BORE */
