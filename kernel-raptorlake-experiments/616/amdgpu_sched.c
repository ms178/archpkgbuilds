/*
 * Copyright 2017 Valve Corporation
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
 * Authors: Andres Rodriguez <andresx7@gmail.com>
 */

#include <linux/file.h>
#include <linux/pid.h>

#include <drm/amdgpu_drm.h>

#include "amdgpu.h"
#include "amdgpu_sched.h"
#include "amdgpu_vm.h"

static int amdgpu_sched_process_priority_override(struct amdgpu_device *adev,
						  int fd,
						  int32_t priority)
{
	struct file *filp;
	struct amdgpu_fpriv *fpriv;
	struct amdgpu_ctx_mgr *mgr;
	struct amdgpu_ctx *ctx;
	unsigned int id;
	int r = 0;

	/* Silence unused parameter warning (adev is not used here). */
	(void)adev;

	filp = fget(fd);
	if (!filp)
		return -EBADF;

	r = amdgpu_file_to_fpriv(filp, &fpriv);
	if (r) {
		fput(filp);
		return r;
	}

	mgr = &fpriv->ctx_mgr;
	mutex_lock(&mgr->lock);
	idr_for_each_entry(&mgr->ctx_handles, ctx, id) {
		amdgpu_ctx_priority_override(ctx, priority);
	}
	mutex_unlock(&mgr->lock);

	fput(filp);
	return 0;
}

static int amdgpu_sched_context_priority_override(struct amdgpu_device *adev,
						  int fd,
						  unsigned int ctx_id,
						  int32_t priority)
{
	struct file *filp;
	struct amdgpu_fpriv *fpriv;
	struct amdgpu_ctx *ctx;
	int r = 0;

	/* Silence unused parameter warning (adev is not used here). */
	(void)adev;

	filp = fget(fd);
	if (!filp)
		return -EBADF;

	r = amdgpu_file_to_fpriv(filp, &fpriv);
	if (r) {
		fput(filp);
		return r;
	}

	ctx = amdgpu_ctx_get(fpriv, ctx_id);
	if (!ctx) {
		fput(filp);
		return -EINVAL;
	}

	amdgpu_ctx_priority_override(ctx, priority);
	amdgpu_ctx_put(ctx);
	fput(filp);
	return 0;
}

int amdgpu_sched_ioctl(struct drm_device *dev, void *data,
		       struct drm_file *filp)
{
	union drm_amdgpu_sched *args = data;
	struct amdgpu_device *adev = drm_to_adev(dev);
	int r;

	/* Validate op first. */
	switch (args->in.op) {
	case AMDGPU_SCHED_OP_PROCESS_PRIORITY_OVERRIDE:
	case AMDGPU_SCHED_OP_CONTEXT_PRIORITY_OVERRIDE:
		break;
	default:
		DRM_ERROR("Invalid sched op specified: %d\n", args->in.op);
		return -EINVAL;
	}

	/* Validate priority. */
	if (!amdgpu_ctx_priority_is_valid(args->in.priority)) {
		WARN(1, "Invalid context priority %d\n", args->in.priority);
		return -EINVAL;
	}

	/* Execute the requested operation. */
	switch (args->in.op) {
	case AMDGPU_SCHED_OP_PROCESS_PRIORITY_OVERRIDE:
		r = amdgpu_sched_process_priority_override(adev,
							   args->in.fd,
							   args->in.priority);
		break;
	case AMDGPU_SCHED_OP_CONTEXT_PRIORITY_OVERRIDE:
		r = amdgpu_sched_context_priority_override(adev,
							   args->in.fd,
							   args->in.ctx_id,
							   args->in.priority);
		break;
	default:
		/* Should be unreachable. */
		r = -EINVAL;
		break;
	}

	return r;
}
