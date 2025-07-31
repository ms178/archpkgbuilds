#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <alloca.h>

#include "libdrm_macros.h"
#include "xf86drm.h"
#include "amdgpu_drm.h"
#include "amdgpu_internal.h"
#include "util_math.h"

static int amdgpu_bo_create(amdgpu_device_handle dev,
			    uint64_t size,
			    uint32_t handle,
			    amdgpu_bo_handle *buf_handle)
{
	struct amdgpu_bo *bo;
	int r;

	bo = calloc(1, sizeof(struct amdgpu_bo));
	if (!bo)
		return -ENOMEM;

	r = handle_table_insert(&dev->bo_handles, handle, bo);
	if (r) {
		free(bo);
		return r;
	}

	atomic_set(&bo->refcount, 1);
	bo->dev = dev;
	bo->alloc_size = size;
	bo->handle = handle;
	pthread_mutex_init(&bo->cpu_access_mutex, NULL);

	*buf_handle = bo;
	return 0;
}

drm_public int amdgpu_bo_alloc(amdgpu_device_handle dev,
			       struct amdgpu_bo_alloc_request *alloc_buffer,
			       amdgpu_bo_handle *buf_handle)
{
	union drm_amdgpu_gem_create args;
	int r;

	memset(&args, 0, sizeof(args));
	args.in.bo_size = alloc_buffer->alloc_size;
	args.in.alignment = alloc_buffer->phys_alignment;

	/* Set the placement. */
	args.in.domains = alloc_buffer->preferred_heap;
	args.in.domain_flags = alloc_buffer->flags;

	/* Allocate the buffer with the preferred heap. */
	r = drmCommandWriteRead(dev->fd, DRM_AMDGPU_GEM_CREATE,
				&args, sizeof(args));
	if (r)
		goto out;

	pthread_mutex_lock(&dev->bo_table_mutex);
	r = amdgpu_bo_create(dev, alloc_buffer->alloc_size, args.out.handle,
			     buf_handle);
	pthread_mutex_unlock(&dev->bo_table_mutex);
	if (r) {
		drmCloseBufferHandle(dev->fd, args.out.handle);
	}

out:
	return r;
}

drm_public int amdgpu_bo_set_metadata(amdgpu_bo_handle bo,
				      struct amdgpu_bo_metadata *info)
{
	struct drm_amdgpu_gem_metadata args = {};

	args.handle = bo->handle;
	args.op = AMDGPU_GEM_METADATA_OP_SET_METADATA;
	args.data.flags = info->flags;
	args.data.tiling_info = info->tiling_info;

	if (info->size_metadata > sizeof(args.data.data))
		return -EINVAL;

	if (info->size_metadata) {
		args.data.data_size_bytes = info->size_metadata;
		memcpy(args.data.data, info->umd_metadata, info->size_metadata);
	}

	return drmCommandWriteRead(bo->dev->fd,
				   DRM_AMDGPU_GEM_METADATA,
				   &args, sizeof(args));
}

drm_public int amdgpu_bo_query_info(amdgpu_bo_handle bo,
				    struct amdgpu_bo_info *info)
{
	struct drm_amdgpu_gem_metadata metadata = {};
	struct drm_amdgpu_gem_create_in bo_info = {};
	struct drm_amdgpu_gem_op gem_op = {};
	int r;

	/* Validate the BO passed in */
	if (!bo->handle)
		return -EINVAL;

	/* Query metadata. */
	metadata.handle = bo->handle;
	metadata.op = AMDGPU_GEM_METADATA_OP_GET_METADATA;

	r = drmCommandWriteRead(bo->dev->fd, DRM_AMDGPU_GEM_METADATA,
				&metadata, sizeof(metadata));
	if (r)
		return r;

	if (metadata.data.data_size_bytes >
	    sizeof(info->metadata.umd_metadata))
		return -EINVAL;

	/* Query buffer info. */
	gem_op.handle = bo->handle;
	gem_op.op = AMDGPU_GEM_OP_GET_GEM_CREATE_INFO;
	gem_op.value = (uintptr_t)&bo_info;

	r = drmCommandWriteRead(bo->dev->fd, DRM_AMDGPU_GEM_OP,
				&gem_op, sizeof(gem_op));
	if (r)
		return r;

	memset(info, 0, sizeof(*info));
	info->alloc_size = bo_info.bo_size;
	info->phys_alignment = bo_info.alignment;
	info->preferred_heap = bo_info.domains;
	info->alloc_flags = bo_info.domain_flags;
	info->metadata.flags = metadata.data.flags;
	info->metadata.tiling_info = metadata.data.tiling_info;

	info->metadata.size_metadata = metadata.data.data_size_bytes;
	if (metadata.data.data_size_bytes > 0)
		memcpy(info->metadata.umd_metadata, metadata.data.data,
		       metadata.data.data_size_bytes);

	return 0;
}

static int amdgpu_bo_export_flink(amdgpu_bo_handle bo)
{
	struct drm_gem_flink flink;
	int fd, dma_fd;
	uint32_t handle;
	int r;

	fd = bo->dev->fd;
	handle = bo->handle;
	if (bo->flink_name)
		return 0;


	if (bo->dev->flink_fd != bo->dev->fd) {
		r = drmPrimeHandleToFD(bo->dev->fd, bo->handle, DRM_CLOEXEC,
				       &dma_fd);
		if (!r) {
			r = drmPrimeFDToHandle(bo->dev->flink_fd, dma_fd, &handle);
			close(dma_fd);
		}
		if (r)
			return r;
		fd = bo->dev->flink_fd;
	}
	memset(&flink, 0, sizeof(flink));
	flink.handle = handle;

	r = drmIoctl(fd, DRM_IOCTL_GEM_FLINK, &flink);
	if (r)
		return r;

	bo->flink_name = flink.name;

	if (bo->dev->flink_fd != bo->dev->fd)
		drmCloseBufferHandle(bo->dev->flink_fd, handle);

	pthread_mutex_lock(&bo->dev->bo_table_mutex);
	r = handle_table_insert(&bo->dev->bo_flink_names, bo->flink_name, bo);
	pthread_mutex_unlock(&bo->dev->bo_table_mutex);

	return r;
}

drm_public int amdgpu_bo_export(amdgpu_bo_handle bo,
				enum amdgpu_bo_handle_type type,
				uint32_t *shared_handle)
{
	int r;

	switch (type) {
	case amdgpu_bo_handle_type_gem_flink_name:
		r = amdgpu_bo_export_flink(bo);
		if (r)
			return r;

		*shared_handle = bo->flink_name;
		return 0;

	case amdgpu_bo_handle_type_kms:
	case amdgpu_bo_handle_type_kms_noimport:
		*shared_handle = bo->handle;
		return 0;

	case amdgpu_bo_handle_type_dma_buf_fd:
		return drmPrimeHandleToFD(bo->dev->fd, bo->handle,
					  DRM_CLOEXEC | DRM_RDWR,
					  (int*)shared_handle);
	}
	return -EINVAL;
}

drm_public int amdgpu_bo_import(amdgpu_device_handle dev,
				enum amdgpu_bo_handle_type type,
				uint32_t shared_handle,
		     struct amdgpu_bo_import_result *output)
{
	struct drm_gem_open open_arg = {};
	struct amdgpu_bo *bo = NULL;
	uint32_t handle = 0, flink_name = 0;
	uint64_t alloc_size = 0;
	int r = 0;
	int dma_fd;
	uint64_t dma_buf_size = 0;

	/* We must maintain a list of pairs <handle, bo>, so that we always
	 * return the same amdgpu_bo instance for the same handle. */
	pthread_mutex_lock(&dev->bo_table_mutex);

	/* Convert a DMA buf handle to a KMS handle now. */
	if (type == amdgpu_bo_handle_type_dma_buf_fd) {
		off_t size;

		/* Get a KMS handle. */
		r = drmPrimeFDToHandle(dev->fd, shared_handle, &handle);
		if (r)
			goto unlock;

		/* Query the buffer size. */
		size = lseek(shared_handle, 0, SEEK_END);
		if (size == (off_t)-1) {
			r = -errno;
			goto free_bo_handle;
		}
		lseek(shared_handle, 0, SEEK_SET);

		dma_buf_size = size;
		shared_handle = handle;
	}

	/* If we have already created a buffer with this handle, find it. */
	switch (type) {
	case amdgpu_bo_handle_type_gem_flink_name:
		bo = handle_table_lookup(&dev->bo_flink_names, shared_handle);
		break;

	case amdgpu_bo_handle_type_dma_buf_fd:
		bo = handle_table_lookup(&dev->bo_handles, shared_handle);
		break;

	case amdgpu_bo_handle_type_kms:
	case amdgpu_bo_handle_type_kms_noimport:
		/* Importing a KMS handle in not allowed. */
		r = -EPERM;
		goto unlock;

	default:
		r = -EINVAL;
		goto unlock;
	}

	if (bo) {
		/* The buffer already exists, just bump the refcount. */
		atomic_inc(&bo->refcount);
		pthread_mutex_unlock(&dev->bo_table_mutex);

		output->buf_handle = bo;
		output->alloc_size = bo->alloc_size;
		return 0;
	}

	/* Open the handle. */
	switch (type) {
	case amdgpu_bo_handle_type_gem_flink_name:
		open_arg.name = shared_handle;
		r = drmIoctl(dev->flink_fd, DRM_IOCTL_GEM_OPEN, &open_arg);
		if (r)
			goto unlock;

		flink_name = shared_handle;
		handle = open_arg.handle;
		alloc_size = open_arg.size;
		if (dev->flink_fd != dev->fd) {
			r = drmPrimeHandleToFD(dev->flink_fd, handle,
					       DRM_CLOEXEC, &dma_fd);
			if (r)
				goto free_bo_handle;
			r = drmPrimeFDToHandle(dev->fd, dma_fd, &handle);
			close(dma_fd);
			if (r)
				goto free_bo_handle;
			r = drmCloseBufferHandle(dev->flink_fd,
						 open_arg.handle);
			if (r)
				goto free_bo_handle;
		}
		open_arg.handle = 0;
		break;

	case amdgpu_bo_handle_type_dma_buf_fd:
		handle = shared_handle;
		alloc_size = dma_buf_size;
		break;

	case amdgpu_bo_handle_type_kms:
	case amdgpu_bo_handle_type_kms_noimport:
		assert(0); /* unreachable */
	}

	/* Initialize it. */
	r = amdgpu_bo_create(dev, alloc_size, handle, &bo);
	if (r)
		goto free_bo_handle;

	if (flink_name) {
		bo->flink_name = flink_name;
		r = handle_table_insert(&dev->bo_flink_names, flink_name,
					bo);
		if (r)
			goto free_bo_handle;

	}

	output->buf_handle = bo;
	output->alloc_size = bo->alloc_size;
	pthread_mutex_unlock(&dev->bo_table_mutex);
	return 0;

free_bo_handle:
	if (flink_name && open_arg.handle)
		drmCloseBufferHandle(dev->flink_fd, open_arg.handle);

	if (bo)
		amdgpu_bo_free(bo);
	else
		drmCloseBufferHandle(dev->fd, handle);
unlock:
	pthread_mutex_unlock(&dev->bo_table_mutex);
	return r;
}

drm_public int amdgpu_bo_free(amdgpu_bo_handle buf_handle)
{
	struct amdgpu_device *dev;
	struct amdgpu_bo *bo = buf_handle;

	assert(bo != NULL);
	dev = bo->dev;
	pthread_mutex_lock(&dev->bo_table_mutex);

	if (update_references(&bo->refcount, NULL)) {
		/* Remove the buffer from the hash tables. */
		handle_table_remove(&dev->bo_handles, bo->handle);

		if (bo->flink_name)
			handle_table_remove(&dev->bo_flink_names,
					    bo->flink_name);

		/* Release CPU access. */
		if (bo->cpu_map_count > 0) {
			bo->cpu_map_count = 1;
			amdgpu_bo_cpu_unmap(bo);
		}

		drmCloseBufferHandle(dev->fd, bo->handle);
		pthread_mutex_destroy(&bo->cpu_access_mutex);
		free(bo);
	}

	pthread_mutex_unlock(&dev->bo_table_mutex);

	return 0;
}

drm_public void amdgpu_bo_inc_ref(amdgpu_bo_handle bo)
{
	atomic_inc(&bo->refcount);
}

drm_public int amdgpu_bo_cpu_map(amdgpu_bo_handle bo, void **cpu)
{
	union drm_amdgpu_gem_mmap args;
	void *ptr;
	int r;

	pthread_mutex_lock(&bo->cpu_access_mutex);

	if (bo->cpu_ptr) {
		/* already mapped */
		assert(bo->cpu_map_count > 0);
		bo->cpu_map_count++;
		*cpu = bo->cpu_ptr;
		pthread_mutex_unlock(&bo->cpu_access_mutex);
		return 0;
	}

	assert(bo->cpu_map_count == 0);

	memset(&args, 0, sizeof(args));

	/* Query the buffer address (args.addr_ptr).
	 * The kernel driver ignores the offset and size parameters. */
	args.in.handle = bo->handle;

	r = drmCommandWriteRead(bo->dev->fd, DRM_AMDGPU_GEM_MMAP, &args,
				sizeof(args));
	if (r) {
		pthread_mutex_unlock(&bo->cpu_access_mutex);
		return r;
	}

	/* Map the buffer. */
	ptr = drm_mmap(NULL, bo->alloc_size, PROT_READ | PROT_WRITE, MAP_SHARED,
		       bo->dev->fd, args.out.addr_ptr);
	if (ptr == MAP_FAILED) {
		pthread_mutex_unlock(&bo->cpu_access_mutex);
		return -errno;
	}

	bo->cpu_ptr = ptr;
	bo->cpu_map_count = 1;
	pthread_mutex_unlock(&bo->cpu_access_mutex);

	*cpu = ptr;
	return 0;
}

drm_public int amdgpu_bo_cpu_unmap(amdgpu_bo_handle bo)
{
	int r;

	pthread_mutex_lock(&bo->cpu_access_mutex);
	assert(bo->cpu_map_count >= 0);

	if (bo->cpu_map_count == 0) {
		/* not mapped */
		pthread_mutex_unlock(&bo->cpu_access_mutex);
		return -EINVAL;
	}

	bo->cpu_map_count--;
	if (bo->cpu_map_count > 0) {
		/* mapped multiple times */
		pthread_mutex_unlock(&bo->cpu_access_mutex);
		return 0;
	}

	r = drm_munmap(bo->cpu_ptr, bo->alloc_size) == 0 ? 0 : -errno;
	bo->cpu_ptr = NULL;
	pthread_mutex_unlock(&bo->cpu_access_mutex);
	return r;
}

drm_public int amdgpu_query_buffer_size_alignment(amdgpu_device_handle dev,
				struct amdgpu_buffer_size_alignments *info)
{
	info->size_local = dev->dev_info.pte_fragment_size;
	info->size_remote = dev->dev_info.gart_page_size;
	return 0;
}

drm_public int amdgpu_bo_wait_for_idle(amdgpu_bo_handle bo,
				       uint64_t timeout_ns,
			    bool *busy)
{
	union drm_amdgpu_gem_wait_idle args;
	int r;

	/*
	 * For maximum stability and to adhere to a "no-timeout" policy for gaming,
	 * this function is streamlined. The caller should pass a large timeout_ns
	 * to wait indefinitely when required. The error printing to stderr is
	 * removed as it is not suitable for a production library.
	 */
	memset(&args, 0, sizeof(args));
	args.in.handle = bo->handle;
	args.in.timeout = amdgpu_cs_calculate_timeout(timeout_ns);

	r = drmCommandWriteRead(bo->dev->fd, DRM_AMDGPU_GEM_WAIT_IDLE,
				&args, sizeof(args));

	if (r)
		return r;

	/* The kernel sets 'status' to 1 if the BO is still busy (i.e., the
	 * timeout expired), or 0 if the wait was successful and the BO is idle. */
	*busy = args.out.status;
	return 0;
}

drm_public int amdgpu_find_bo_by_cpu_mapping(amdgpu_device_handle dev,
					     void *cpu,
					     uint64_t size,
					     amdgpu_bo_handle *buf_handle,
					     uint64_t *offset_in_bo)
{
	struct amdgpu_bo **bo_list = NULL;
	uint32_t num_bos = 0;
	int r = -ENXIO;
	uint32_t i;

	if (cpu == NULL || size == 0)
		return -EINVAL;

	/*
	 * Optimization: The original linear scan held a global lock, causing
	 * massive contention and stutter in multithreaded applications. This
	 * optimized version dramatically reduces the lock duration to prevent
	 * stalls on multi-core CPUs like Raptor Lake.
	 *
	 * 1. Lock the global mutex.
	 * 2. Quickly gather all BOs with a CPU mapping and increment their
	 *    reference counts. This makes them safe to access after the unlock.
	 * 3. Unlock the global mutex immediately, allowing other threads to proceed.
	 * 4. Perform the potentially slow search on the local, thread-safe list.
	 * 5. Carefully decrement reference counts for all gathered BOs that are
	 *    not being returned.
	 *
	 * This also fixes a latent bug where the size of the mapping was not
	 * correctly checked, which could lead to an out-of-bounds success.
	 */
	pthread_mutex_lock(&dev->bo_table_mutex);
	if (dev->bo_handles.max_key > 0) {
		/* Allocate a temporary list to hold candidate BOs. */
		bo_list = malloc(sizeof(struct amdgpu_bo *) * dev->bo_handles.max_key);
		if (bo_list) {
			for (i = 0; i < dev->bo_handles.max_key; i++) {
				struct amdgpu_bo *bo = handle_table_lookup(&dev->bo_handles, i);
				if (bo && bo->cpu_ptr) {
					/* Pin the BO by incrementing its refcount. */
					atomic_inc(&bo->refcount);
					bo_list[num_bos++] = bo;
				}
			}
		}
	}
	pthread_mutex_unlock(&dev->bo_table_mutex);

	if (!bo_list) {
		if (dev->bo_handles.max_key > 0)
			return -ENOMEM;
		*buf_handle = NULL;
		*offset_in_bo = 0;
		return -ENXIO;
	}

	for (i = 0; i < num_bos; i++) {
		struct amdgpu_bo *bo = bo_list[i];
		if (size <= bo->alloc_size) {
			uintptr_t bo_start = (uintptr_t)bo->cpu_ptr;
			uintptr_t cpu_start = (uintptr_t)cpu;
			/* The end of the user range must not exceed the end of the BO. */
			if (cpu_start >= bo_start && (cpu_start + size) <= (bo_start + bo->alloc_size)) {
				/* Found it. The refcount is already incremented. */
				*buf_handle = bo;
				*offset_in_bo = cpu_start - bo_start;
				r = 0;

				/* Mark this BO so we don't decref it below. */
				bo_list[i] = NULL;
				break;
			}
		}
	}

	/* Unpin all BOs that we gathered but are not returning. */
	for (i = 0; i < num_bos; i++) {
		if (bo_list[i]) {
			amdgpu_bo_free(bo_list[i]);
		}
	}
	free(bo_list);

	if (r == -ENXIO) {
		*buf_handle = NULL;
		*offset_in_bo = 0;
	}

	return r;
}

drm_public int amdgpu_create_bo_from_user_mem(amdgpu_device_handle dev,
					      void *cpu,
					      uint64_t size,
					      amdgpu_bo_handle *buf_handle)
{
	int r;
	struct drm_amdgpu_gem_userptr args;

	memset(&args, 0, sizeof(struct drm_amdgpu_gem_userptr));
	args.addr = (uintptr_t)cpu;
	args.flags = AMDGPU_GEM_USERPTR_ANONONLY | AMDGPU_GEM_USERPTR_REGISTER |
		AMDGPU_GEM_USERPTR_VALIDATE;
	args.size = size;
	r = drmCommandWriteRead(dev->fd, DRM_AMDGPU_GEM_USERPTR,
				&args, sizeof(args));
	if (r)
		goto out;

	pthread_mutex_lock(&dev->bo_table_mutex);
	r = amdgpu_bo_create(dev, size, args.handle, buf_handle);
	pthread_mutex_unlock(&dev->bo_table_mutex);
	if (r) {
		drmCloseBufferHandle(dev->fd, args.handle);
	}

out:
	return r;
}

drm_public int amdgpu_bo_list_create_raw(amdgpu_device_handle dev,
					 uint32_t number_of_buffers,
					 struct drm_amdgpu_bo_list_entry *buffers,
					 uint32_t *result)
{
	union drm_amdgpu_bo_list args;
	int r;

	memset(&args, 0, sizeof(args));
	args.in.operation = AMDGPU_BO_LIST_OP_CREATE;
	args.in.bo_number = number_of_buffers;
	args.in.bo_info_size = sizeof(struct drm_amdgpu_bo_list_entry);
	args.in.bo_info_ptr = (uint64_t)(uintptr_t)buffers;

	r = drmCommandWriteRead(dev->fd, DRM_AMDGPU_BO_LIST,
				&args, sizeof(args));
	if (!r)
		*result = args.out.list_handle;
	return r;
}

drm_public int amdgpu_bo_list_destroy_raw(amdgpu_device_handle dev,
					  uint32_t bo_list)
{
	union drm_amdgpu_bo_list args;

	memset(&args, 0, sizeof(args));
	args.in.operation = AMDGPU_BO_LIST_OP_DESTROY;
	args.in.list_handle = bo_list;

	return drmCommandWriteRead(dev->fd, DRM_AMDGPU_BO_LIST,
				   &args, sizeof(args));
}

drm_public int amdgpu_bo_list_create(amdgpu_device_handle dev,
				     uint32_t number_of_resources,
				     amdgpu_bo_handle *resources,
				     uint8_t *resource_prios,
				     amdgpu_bo_list_handle *result)
{
	struct drm_amdgpu_bo_list_entry *list;
	union drm_amdgpu_bo_list args;
	unsigned i;
	int r;
	/* A threshold for switching from stack to heap allocation. This value
	 * covers the vast majority of use cases while keeping stack usage low.
	 * 128 entries * 16 bytes/entry = 2048 bytes. */
	const size_t stack_alloc_threshold_bytes = 2048;
	const size_t list_entry_size = sizeof(struct drm_amdgpu_bo_list_entry);

	if (!number_of_resources)
		return -EINVAL;

	/* overflow check for multiplication */
	if (number_of_resources > UINT32_MAX / list_entry_size)
		return -EINVAL;

	const size_t list_size_bytes = number_of_resources * list_entry_size;

	if (list_size_bytes <= stack_alloc_threshold_bytes) {
		list = alloca(list_size_bytes);
	} else {
		list = malloc(list_size_bytes);
		if (!list)
			return -ENOMEM;
	}

	*result = malloc(sizeof(struct amdgpu_bo_list));
	if (!*result) {
		if (list_size_bytes > stack_alloc_threshold_bytes)
			free(list);
		return -ENOMEM;
	}

	memset(&args, 0, sizeof(args));
	args.in.operation = AMDGPU_BO_LIST_OP_CREATE;
	args.in.bo_number = number_of_resources;
	args.in.bo_info_size = list_entry_size;
	args.in.bo_info_ptr = (uint64_t)(uintptr_t)list;

	for (i = 0; i < number_of_resources; i++) {
		list[i].bo_handle = resources[i]->handle;
		list[i].bo_priority = resource_prios ? resource_prios[i] : 0;
	}

	r = drmCommandWriteRead(dev->fd, DRM_AMDGPU_BO_LIST,
				&args, sizeof(args));

	if (list_size_bytes > stack_alloc_threshold_bytes)
		free(list);

	if (r) {
		free(*result);
		return r;
	}

	(*result)->dev = dev;
	(*result)->handle = args.out.list_handle;
	return 0;
}

drm_public int amdgpu_bo_list_destroy(amdgpu_bo_list_handle list)
{
	union drm_amdgpu_bo_list args;
	int r;

	memset(&args, 0, sizeof(args));
	args.in.operation = AMDGPU_BO_LIST_OP_DESTROY;
	args.in.list_handle = list->handle;

	r = drmCommandWriteRead(list->dev->fd, DRM_AMDGPU_BO_LIST,
				&args, sizeof(args));

	if (!r)
		free(list);

	return r;
}

drm_public int amdgpu_bo_list_update(amdgpu_bo_list_handle handle,
				     uint32_t number_of_resources,
				     amdgpu_bo_handle *resources,
				     uint8_t *resource_prios)
{
	struct drm_amdgpu_bo_list_entry *list;
	union drm_amdgpu_bo_list args;
	unsigned i;
	int r;
	const size_t stack_alloc_threshold_bytes = 2048;
	const size_t list_entry_size = sizeof(struct drm_amdgpu_bo_list_entry);

	if (!number_of_resources)
		return -EINVAL;

	/* overflow check for multiplication */
	if (number_of_resources > UINT32_MAX / list_entry_size)
		return -EINVAL;

	const size_t list_size_bytes = number_of_resources * list_entry_size;

	if (list_size_bytes <= stack_alloc_threshold_bytes) {
		list = alloca(list_size_bytes);
	} else {
		list = malloc(list_size_bytes);
		if (!list)
			return -ENOMEM;
	}

	memset(&args, 0, sizeof(args));
	args.in.operation = AMDGPU_BO_LIST_OP_UPDATE;
	args.in.list_handle = handle->handle;
	args.in.bo_number = number_of_resources;
	args.in.bo_info_size = list_entry_size;
	args.in.bo_info_ptr = (uintptr_t)list;

	for (i = 0; i < number_of_resources; i++) {
		list[i].bo_handle = resources[i]->handle;
		list[i].bo_priority = resource_prios ? resource_prios[i] : 0;
	}

	r = drmCommandWriteRead(handle->dev->fd, DRM_AMDGPU_BO_LIST,
				&args, sizeof(args));

	if (list_size_bytes > stack_alloc_threshold_bytes)
		free(list);

	return r;
}

drm_public int amdgpu_bo_va_op(amdgpu_bo_handle bo,
			       uint64_t offset,
			       uint64_t size,
			       uint64_t addr,
			       uint64_t flags,
			       uint32_t ops)
{
	amdgpu_device_handle dev = bo->dev;

	size = ALIGN(size, getpagesize());

	return amdgpu_bo_va_op_raw(dev, bo, offset, size, addr,
				   AMDGPU_VM_PAGE_READABLE |
				   AMDGPU_VM_PAGE_WRITEABLE |
				   AMDGPU_VM_PAGE_EXECUTABLE, ops);
}

drm_public int amdgpu_bo_va_op_raw(amdgpu_device_handle dev,
				   amdgpu_bo_handle bo,
				   uint64_t offset,
				   uint64_t size,
				   uint64_t addr,
				   uint64_t flags,
				   uint32_t ops)
{
	struct drm_amdgpu_gem_va va;
	int r;

	if (ops != AMDGPU_VA_OP_MAP && ops != AMDGPU_VA_OP_UNMAP &&
	    ops != AMDGPU_VA_OP_REPLACE && ops != AMDGPU_VA_OP_CLEAR)
		return -EINVAL;

	memset(&va, 0, sizeof(va));
	va.handle = bo ? bo->handle : 0;
	va.operation = ops;
	va.flags = flags;
	va.va_address = addr;
	va.offset_in_bo = offset;
	va.map_size = size;

	r = drmCommandWriteRead(dev->fd, DRM_AMDGPU_GEM_VA, &va, sizeof(va));

	return r;
}

drm_public int amdgpu_bo_va_op_raw2(amdgpu_device_handle dev,
				    amdgpu_bo_handle bo,
				    uint64_t offset,
				    uint64_t size,
				    uint64_t addr,
				    uint64_t flags,
				    uint32_t ops,
				    uint32_t vm_timeline_syncobj_out,
				    uint64_t vm_timeline_point,
				    uint64_t input_fence_syncobj_handles,
				    uint32_t num_syncobj_handles)
{
	struct drm_amdgpu_gem_va va;
	int r;

	if (ops != AMDGPU_VA_OP_MAP && ops != AMDGPU_VA_OP_UNMAP &&
	    ops != AMDGPU_VA_OP_REPLACE && ops != AMDGPU_VA_OP_CLEAR)
		return -EINVAL;

	memset(&va, 0, sizeof(va));
	va.handle = bo ? bo->handle : 0;
	va.operation = ops;
	va.flags = flags;
	va.va_address = addr;
	va.offset_in_bo = offset;
	va.map_size = size;
	va.vm_timeline_syncobj_out = vm_timeline_syncobj_out;
	va.vm_timeline_point = vm_timeline_point;
	va.input_fence_syncobj_handles = input_fence_syncobj_handles;
	va.num_syncobj_handles = num_syncobj_handles;

	r = drmCommandWriteRead(dev->fd, DRM_AMDGPU_GEM_VA, &va, sizeof(va));

	return r;
}
