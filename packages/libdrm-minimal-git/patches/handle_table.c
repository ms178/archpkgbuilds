/*
 * Copyright 2018 Advanced Micro Devices, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE COPYRIGHT HOLDER(S) OR AUTHOR(S) BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 */

#include <errno.h>
#include <limits.h>
#include <pthread.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "handle_table.h"

#ifndef likely
#define likely(x)   __builtin_expect(!!(x), 1)
#endif
#ifndef unlikely
#define unlikely(x) __builtin_expect(!!(x), 0)
#endif

static pthread_once_t handle_table_alignment_once = PTHREAD_ONCE_INIT;
static uint32_t handle_table_alignment = 512;

static void handle_table_init_alignment(void)
{
	long page_size = sysconf(_SC_PAGESIZE);
	uint32_t alignment;

	if (page_size <= 0)
		page_size = 4096;

	alignment = (uint32_t)((unsigned long)page_size / sizeof(void *));
	if (!alignment)
		alignment = 1;

	/* sysconf(_SC_PAGESIZE) is expected to be power-of-two on Linux, but
	 * keep the table arithmetic correct on odd hosts as well.
	 */
	if (alignment & (alignment - 1)) {
		uint32_t pow2 = 1;

		while (pow2 < alignment && pow2 <= UINT32_MAX / 2)
			pow2 <<= 1;
		alignment = pow2;
	}

	handle_table_alignment = alignment;
}

drm_private int handle_table_insert(struct handle_table *table, uint32_t key,
				    void *value)
{
	if (unlikely(!table))
		return -EINVAL;

	if (key >= table->max_key) {
		uint64_t new_max;
		uint64_t mask;
		uint64_t bytes;
		void **values;

		pthread_once(&handle_table_alignment_once,
			     handle_table_init_alignment);
		mask = (uint64_t)handle_table_alignment - 1;
		new_max = ((uint64_t)key + 1 + mask) & ~mask;

		if (unlikely(new_max > UINT32_MAX ||
			     new_max > SIZE_MAX / sizeof(void *)))
			return -ENOMEM;

		bytes = new_max * sizeof(void *);
		values = realloc(table->values, (size_t)bytes);
		if (unlikely(!values))
			return -ENOMEM;

		memset(values + table->max_key, 0,
		       (size_t)(new_max - table->max_key) * sizeof(void *));

		table->max_key = (uint32_t)new_max;
		table->values = values;
	}

	table->values[key] = value;
	return 0;
}

drm_private void handle_table_remove(struct handle_table *table, uint32_t key)
{
	if (likely(table && key < table->max_key))
		table->values[key] = NULL;
}

drm_private void *handle_table_lookup(struct handle_table *table, uint32_t key)
{
	if (likely(table && key < table->max_key))
		return table->values[key];
	else
		return NULL;
}

drm_private void handle_table_fini(struct handle_table *table)
{
	if (unlikely(!table))
		return;

	free(table->values);
	table->max_key = 0;
	table->values = NULL;
}
