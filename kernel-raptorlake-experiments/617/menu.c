// SPDX-License-Identifier: GPL-2.0-only
/*
 * menu.c - the menu cpuidle governor
 *
 * Copyright (C) 2006-2007 Adam Belay <abelay@novell.com>
 * Copyright (C) 2009 Intel Corporation
 * Author: Arjan van de Ven <arjan@linux.intel.com>
 *
 * Optimized for Intel Raptor Lake / AMD Vega64 gaming workloads.
 */

#include <linux/kernel.h>
#include <linux/cpuidle.h>
#include <linux/time.h>
#include <linux/ktime.h>
#include <linux/hrtimer.h>
#include <linux/tick.h>
#include <linux/sched/stat.h>
#include <linux/math64.h>

#include "gov.h"

#define BUCKETS 6
#define INTERVAL_SHIFT 3
#define INTERVALS (1U << INTERVAL_SHIFT)
#define INTERVAL_MASK (INTERVALS - 1U)
#define RESOLUTION 1024U
#define DECAY 8U
#define MAX_INTERESTING (50000ULL * NSEC_PER_USEC)
#define PREDICTION_FACTOR_DIVISOR (RESOLUTION * DECAY * NSEC_PER_USEC)

struct menu_device {
	int		needs_update;
	int		tick_wakeup;
	u64		next_timer_ns;
	unsigned int	bucket;
	unsigned int	correction_factor[BUCKETS];
	unsigned int	intervals[INTERVALS];
	int		interval_ptr;
};

static DEFINE_PER_CPU(struct menu_device, menu_devices);

static __always_inline int which_bucket(u64 duration_ns)
{
	return (int)((duration_ns >= 10ULL * NSEC_PER_USEC) +
		     (duration_ns >= 100ULL * NSEC_PER_USEC) +
		     (duration_ns >= 1000ULL * NSEC_PER_USEC) +
		     (duration_ns >= 10000ULL * NSEC_PER_USEC) +
		     (duration_ns >= 100000ULL * NSEC_PER_USEC));
}

static __always_inline void menu_update_intervals(struct menu_device *data,
						  unsigned int interval_us)
{
	unsigned int ptr = (unsigned int)data->interval_ptr;

	data->intervals[ptr] = interval_us;
	data->interval_ptr = (int)((ptr + 1U) & INTERVAL_MASK);
}

static void menu_update(struct cpuidle_driver *drv, struct cpuidle_device *dev);

static unsigned int get_typical_interval(struct menu_device *data)
{
	const unsigned int * const intervals = data->intervals;
	s64 min_thresh = -1;
	s64 max_thresh = (s64)UINT_MAX;
	unsigned int max, min, divisor;
	u64 avg, variance, avg_sq;
	unsigned int i;

again:
	max = 0;
	min = UINT_MAX;
	avg = 0;
	variance = 0;
	divisor = 0;

	for (i = 0; i < INTERVALS; i++) {
		unsigned int raw = intervals[i];
		s64 value = (s64)raw;

		if (value <= min_thresh || value >= max_thresh)
			continue;

		divisor++;
		avg += raw;
		variance += (u64)raw * raw;

		if (raw > max)
			max = raw;
		if (raw < min)
			min = raw;
	}

	if (max == 0)
		return UINT_MAX;

	if (divisor == INTERVALS) {
		avg >>= INTERVAL_SHIFT;
		variance >>= INTERVAL_SHIFT;
	} else {
		do_div(avg, divisor);
		do_div(variance, divisor);
	}

	avg_sq = avg * avg;
	if (variance >= avg_sq)
		variance -= avg_sq;
	else
		variance = 0;

	if (likely(variance <= U64_MAX / 36)) {
		if ((avg_sq > variance * 36 && divisor * 4 >= INTERVALS * 3) ||
		    variance <= 400)
			return (unsigned int)avg;
	}

	if (divisor * 4 <= INTERVALS * 3)
		return UINT_MAX;

	if (avg - min > max - avg)
		min_thresh = (s64)min;
	else
		max_thresh = (s64)max;

	goto again;
}

static int menu_select(struct cpuidle_driver *drv, struct cpuidle_device *dev,
		       bool *stop_tick)
{
	struct menu_device *data = this_cpu_ptr(&menu_devices);
	s64 latency_req = cpuidle_governor_latency_req(dev->cpu);
	unsigned int typical_us;
	u64 next_timer_ns;
	u64 predicted_ns;
	unsigned int bucket;
	ktime_t delta_tick;
	int i, idx;

	if (data->needs_update) {
		menu_update(drv, dev);
		data->needs_update = 0;
	} else if (dev->last_residency_ns == 0) {
		menu_update_intervals(data, UINT_MAX);
	}

	typical_us = get_typical_interval(data);
	predicted_ns = (u64)typical_us * NSEC_PER_USEC;

	if (predicted_ns > RESIDENCY_THRESHOLD_NS) {
		unsigned int correction;
		unsigned int timer_us;
		u64 numerator;
		ktime_t delta;

		delta = tick_nohz_get_sleep_length(&delta_tick);
		if (unlikely(delta < 0)) {
			delta = 0;
			delta_tick = 0;
		}

		next_timer_ns = (u64)delta;
		bucket = (unsigned int)which_bucket(next_timer_ns);
		correction = data->correction_factor[bucket];

		data->next_timer_ns = next_timer_ns;
		data->bucket = bucket;

		numerator = PREDICTION_FACTOR_DIVISOR / 2 +
			    next_timer_ns * correction;
		timer_us = (unsigned int)div_u64(numerator,
						 PREDICTION_FACTOR_DIVISOR);

		predicted_ns = min((u64)timer_us * NSEC_PER_USEC, predicted_ns);
	} else {
		next_timer_ns = KTIME_MAX;
		delta_tick = TICK_NSEC / 2;
		bucket = BUCKETS - 1;
		data->next_timer_ns = next_timer_ns;
		data->bucket = bucket;
	}

	if (unlikely(drv->state_count <= 1 || latency_req == 0) ||
	    ((next_timer_ns < drv->states[1].target_residency_ns ||
	      latency_req < drv->states[1].exit_latency_ns) &&
	     !dev->states_usage[0].disable)) {
		*stop_tick = !(drv->states[0].flags & CPUIDLE_FLAG_POLLING);
		return 0;
	}

	if (tick_nohz_tick_stopped() && predicted_ns < TICK_NSEC) {
		u64 margin = predicted_ns >> 2;

		predicted_ns = min(predicted_ns + margin, next_timer_ns);
	}

	idx = -1;
	for (i = 0; i < drv->state_count; i++) {
		struct cpuidle_state *s = &drv->states[i];

		if (dev->states_usage[i].disable)
			continue;

		if (idx == -1)
			idx = i;

		if (s->exit_latency_ns > latency_req)
			break;

		if (s->target_residency_ns <= predicted_ns) {
			idx = i;
			continue;
		}

		if ((drv->states[idx].flags & CPUIDLE_FLAG_POLLING) &&
		    s->target_residency_ns <= next_timer_ns &&
		    s->exit_latency_ns <= predicted_ns) {
			predicted_ns = s->target_residency_ns;
			idx = i;
			break;
		}

		if (predicted_ns < TICK_NSEC)
			break;

		if (!tick_nohz_tick_stopped()) {
			predicted_ns = drv->states[idx].target_residency_ns;
			break;
		}

		if (drv->states[idx].target_residency_ns < TICK_NSEC &&
		    s->target_residency_ns <= delta_tick)
			idx = i;

		return idx;
	}

	if (idx == -1)
		idx = 0;

	if (((drv->states[idx].flags & CPUIDLE_FLAG_POLLING) ||
	     predicted_ns < TICK_NSEC) && !tick_nohz_tick_stopped()) {
		*stop_tick = false;

		if (idx > 0 &&
		    drv->states[idx].target_residency_ns > delta_tick) {
			for (i = idx - 1; i >= 0; i--) {
				if (dev->states_usage[i].disable)
					continue;

				idx = i;
				if (drv->states[i].target_residency_ns <=
				    delta_tick)
					break;
			}
		}
	}

	return idx;
}

static void menu_reflect(struct cpuidle_device *dev, int index)
{
	struct menu_device *data = this_cpu_ptr(&menu_devices);

	dev->last_state_idx = index;
	data->needs_update = 1;
	data->tick_wakeup = tick_nohz_idle_got_tick();
}

static void menu_update(struct cpuidle_driver *drv, struct cpuidle_device *dev)
{
	struct menu_device *data = this_cpu_ptr(&menu_devices);
	int last_idx = dev->last_state_idx;
	struct cpuidle_state *target = &drv->states[last_idx];
	u64 next_timer_ns = data->next_timer_ns;
	unsigned int bucket = data->bucket;
	unsigned int new_factor;
	u64 measured_ns;

	if (data->tick_wakeup && next_timer_ns > TICK_NSEC) {
		measured_ns = 9 * MAX_INTERESTING / 10;
	} else if ((drv->states[last_idx].flags & CPUIDLE_FLAG_POLLING) &&
		   dev->poll_time_limit) {
		measured_ns = next_timer_ns;
	} else {
		measured_ns = dev->last_residency_ns;

		if (measured_ns > 2 * target->exit_latency_ns)
			measured_ns -= target->exit_latency_ns;
		else
			measured_ns /= 2;
	}

	if (measured_ns > next_timer_ns)
		measured_ns = next_timer_ns;

	new_factor = data->correction_factor[bucket];
	new_factor -= new_factor / DECAY;

	if (next_timer_ns > 0 && measured_ns < MAX_INTERESTING) {
		new_factor += (unsigned int)div64_u64(RESOLUTION * measured_ns,
						      next_timer_ns);
	} else {
		new_factor += RESOLUTION;
	}

	if (DECAY == 1 && unlikely(new_factor == 0))
		new_factor = 1;

	data->correction_factor[bucket] = new_factor;

	menu_update_intervals(data, (unsigned int)ktime_to_us(measured_ns));
}

static int menu_enable_device(struct cpuidle_driver *drv,
			      struct cpuidle_device *dev)
{
	struct menu_device *data = &per_cpu(menu_devices, dev->cpu);
	unsigned int i;

	memset(data, 0, sizeof(*data));

	for (i = 0; i < BUCKETS; i++)
		data->correction_factor[i] = RESOLUTION * DECAY;

	return 0;
}

static struct cpuidle_governor menu_governor = {
	.name		= "menu",
	.rating		= 20,
	.enable		= menu_enable_device,
	.select		= menu_select,
	.reflect	= menu_reflect,
};

static int __init init_menu(void)
{
	return cpuidle_register_governor(&menu_governor);
}

postcore_initcall(init_menu);
