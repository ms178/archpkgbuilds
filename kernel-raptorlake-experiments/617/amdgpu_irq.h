/*
 * Copyright 2014 Advanced Micro Devices, Inc.
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

#ifndef __AMDGPU_IRQ_H__
#define __AMDGPU_IRQ_H__

#include <linux/irqdomain.h>
#include "soc15_ih_clientid.h"
#include "amdgpu_ih.h"

#define AMDGPU_MAX_IRQ_SRC_ID		0x100
#define AMDGPU_MAX_IRQ_CLIENT_ID	0x100

#define AMDGPU_IRQ_CLIENTID_LEGACY	0
#define AMDGPU_IRQ_CLIENTID_MAX		SOC15_IH_CLIENTID_MAX

#define AMDGPU_IRQ_SRC_DATA_MAX_SIZE_DW	4

struct amdgpu_device;

enum amdgpu_interrupt_state {
	AMDGPU_IRQ_STATE_DISABLE,
	AMDGPU_IRQ_STATE_ENABLE,
};

/**
 * struct amdgpu_iv_entry - Interrupt vector entry
 *
 * Layout optimized for 64-byte cache line locality in dispatch hot path.
 * Hot fields (ih, iv_entry, timestamp, client_id, src_id) packed into
 * first cache line. Total size: 72 bytes.
 */
struct amdgpu_iv_entry {
	struct amdgpu_ih_ring	*ih;
	const u32		*iv_entry;
	u64			timestamp;
	u32			client_id;
	u32			src_id;
	u32			ring_id;
	u32			vmid;
	u32			vmid_src;
	u32			timestamp_src;
	u32			pasid;
	u32			node_id;
	u32			src_data[AMDGPU_IRQ_SRC_DATA_MAX_SIZE_DW];
};

/**
 * struct amdgpu_irq_src - IRQ source descriptor
 *
 * Funcs pointer first for optimal cache behavior when dereferencing
 * in process() and set() hot paths.
 */
struct amdgpu_irq_src {
	const struct amdgpu_irq_src_funcs	*funcs;
	atomic_t				*enabled_types;
	u32					num_types;
};

struct amdgpu_irq_client {
	struct amdgpu_irq_src **sources;
};

struct amdgpu_irq_src_funcs {
	int (*set)(struct amdgpu_device *adev,
		   struct amdgpu_irq_src *source,
		   unsigned int type,
		   enum amdgpu_interrupt_state state);

	int (*process)(struct amdgpu_device *adev,
		       struct amdgpu_irq_src *source,
		       struct amdgpu_iv_entry *entry);
};

/**
 * struct amdgpu_irq - Main IRQ state
 *
 * Layout optimized for dispatch and interrupt handling hot paths.
 * Frequently accessed fields grouped at structure start.
 * Large arrays (client[], virq[]) placed at end.
 */
struct amdgpu_irq {
	spinlock_t			lock;
	u32				irq;
	bool				installed;
	bool				msi_enabled;
	bool				retry_cam_enabled;
	u8				_pad0;
	u32				srbm_soft_reset;
	u32				retry_cam_doorbell_index;
	const struct amdgpu_ih_funcs	*ih_funcs;
	struct irq_domain		*domain;

	struct amdgpu_ih_ring		ih;
	struct amdgpu_ih_ring		ih1;
	struct amdgpu_ih_ring		ih2;
	struct amdgpu_ih_ring		ih_soft;

	struct work_struct		ih1_work;
	struct work_struct		ih2_work;
	struct work_struct		ih_soft_work;

	struct amdgpu_irq_src		self_irq;

	struct amdgpu_irq_client	client[AMDGPU_IRQ_CLIENTID_MAX];
	u32				virq[AMDGPU_MAX_IRQ_SRC_ID];
};

enum interrupt_node_id_per_aid {
	AID0_NODEID = 0,
	XCD0_NODEID = 1,
	XCD1_NODEID = 2,
	AID1_NODEID = 4,
	XCD2_NODEID = 5,
	XCD3_NODEID = 6,
	AID2_NODEID = 8,
	XCD4_NODEID = 9,
	XCD5_NODEID = 10,
	AID3_NODEID = 12,
	XCD6_NODEID = 13,
	XCD7_NODEID = 14,
	NODEID_MAX,
};

extern const int node_id_to_phys_map[NODEID_MAX];

void amdgpu_irq_disable_all(struct amdgpu_device *adev);

int amdgpu_irq_init(struct amdgpu_device *adev);
void amdgpu_irq_fini_sw(struct amdgpu_device *adev);
void amdgpu_irq_fini_hw(struct amdgpu_device *adev);
int amdgpu_irq_add_id(struct amdgpu_device *adev,
		      unsigned int client_id,
		      unsigned int src_id,
		      struct amdgpu_irq_src *source);
void amdgpu_irq_dispatch(struct amdgpu_device *adev,
			 struct amdgpu_ih_ring *ih);
void amdgpu_irq_delegate(struct amdgpu_device *adev,
			 struct amdgpu_iv_entry *entry,
			 unsigned int num_dw);
int amdgpu_irq_update(struct amdgpu_device *adev,
		      struct amdgpu_irq_src *src,
		      unsigned int type);
int amdgpu_irq_get(struct amdgpu_device *adev,
		   struct amdgpu_irq_src *src,
		   unsigned int type);
int amdgpu_irq_put(struct amdgpu_device *adev,
		   struct amdgpu_irq_src *src,
		   unsigned int type);
bool amdgpu_irq_enabled(struct amdgpu_device *adev,
			struct amdgpu_irq_src *src,
			unsigned int type);
void amdgpu_irq_gpu_reset_resume_helper(struct amdgpu_device *adev);

int amdgpu_irq_add_domain(struct amdgpu_device *adev);
void amdgpu_irq_remove_domain(struct amdgpu_device *adev);
unsigned int amdgpu_irq_create_mapping(struct amdgpu_device *adev,
				       unsigned int src_id);
void amdgpu_restore_msix(struct amdgpu_device *adev);

#endif
