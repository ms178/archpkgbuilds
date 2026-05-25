/*
 * Copyright © 2025 Valve Corporation
 *
 * SPDX-License-Identifier: MIT
 *
 */

#ifndef ANTI_LAG_LAYER_H
#define ANTI_LAG_LAYER_H

#include "util/simple_mtx.h"
#include "vulkan/vk_layer.h"
#include "vulkan/vulkan_core.h"
#include "ringbuffer.h"

#include <stdbool.h>
#include <stdint.h>

/* ------------------------------------------------------------------ */
/*  Constants                                                          */
/* ------------------------------------------------------------------ */

#define MAX_FRAMES  8
#define MAX_QUERIES 256

/* Time constants in nanoseconds */
#define ONE_MS   INT64_C(1000000)
#define ONE_S_NS INT64_C(1000000000)

/* ------------------------------------------------------------------ */
/*  Frame state machine                                                */
/* ------------------------------------------------------------------ */

enum frame_state {
   FRAME_INVALID = 0,
   FRAME_INPUT,   /* Frame is being set up on the CPU side */
   FRAME_SUBMIT,  /* QueueSubmit calls are associating with this frame */
   FRAME_PRESENT, /* Frame has been presented; timestamps can be evaluated */
};

typedef struct frame {
   uint64_t         frame_idx;       /* application-assigned frame index */
   uint64_t         gpu_start_time;  /* RAW GPU timestamp of earliest GPU work start */
   int64_t          min_delay;       /* min calibrated submission-to-GPU delay (ns) */
   enum frame_state state;
} frame;

/* ------------------------------------------------------------------ */
/*  Per-queue timestamp query state                                    */
/* ------------------------------------------------------------------ */

struct query {
   uint64_t        begin_gpu_ts;
   uint64_t        submit_cpu_ts;
   VkCommandBuffer cmdbuffer;
};

typedef struct queue_context {
   VkQueue         queue;
   uint32_t        queue_family_idx;
   _Atomic bool    latency_sensitive;     /* one-way flag, set once on first QueuePresent */
   VkCommandPool   cmdPool;
   VkQueryPool     queryPool;
   VkSemaphore     semaphore;
   uint64_t        semaphore_value;       /* monotonically incrementing timeline value */
   uint8_t         submissions_per_frame[MAX_FRAMES];
   RINGBUFFER_DECLARE(queries, struct query, MAX_QUERIES);
} queue_context;

/* ------------------------------------------------------------------ */
/*  Device context (per-VkDevice)                                      */
/* ------------------------------------------------------------------ */

typedef struct device_context {

   /* Vulkan dispatch table — all functions the layer intercepts or delegates to */
   struct DeviceDispatchTable {
#define DECLARE_HOOK(fn) PFN_vk##fn fn
      DECLARE_HOOK(GetDeviceProcAddr);
      DECLARE_HOOK(SetDeviceLoaderData);
      DECLARE_HOOK(DestroyDevice);
      DECLARE_HOOK(QueueSubmit);
      DECLARE_HOOK(QueueSubmit2);
      DECLARE_HOOK(QueueSubmit2KHR);
      DECLARE_HOOK(GetDeviceQueue);
      DECLARE_HOOK(CreateCommandPool);
      DECLARE_HOOK(DestroyCommandPool);
      DECLARE_HOOK(CreateQueryPool);
      DECLARE_HOOK(ResetQueryPool);
      DECLARE_HOOK(DestroyQueryPool);
      DECLARE_HOOK(GetQueryPoolResults);
      DECLARE_HOOK(AllocateCommandBuffers);
      DECLARE_HOOK(FreeCommandBuffers);
      DECLARE_HOOK(BeginCommandBuffer);
      DECLARE_HOOK(EndCommandBuffer);
      DECLARE_HOOK(GetCalibratedTimestampsKHR);
      DECLARE_HOOK(CmdWriteTimestamp);
      DECLARE_HOOK(CreateSemaphore);
      DECLARE_HOOK(DestroySemaphore);
      DECLARE_HOOK(GetSemaphoreCounterValue);
      DECLARE_HOOK(WaitSemaphores);
      DECLARE_HOOK(QueuePresentKHR);
#undef DECLARE_HOOK
   } vtable;

   VkDevice              device;
   VkAllocationCallbacks alloc;
   simple_mtx_t          mtx;

   /* CPU-GPU timestamp calibration state */
   struct {
      int64_t  delta;              /* CPU_ns - (GPU_tick * period)  (signed offset) */
      uint64_t recalibrate_when;   /* CPU_monotonic_ns at which to recalibrate next */
      double   timestamp_period;   /* ns per GPU tick  (double for precision) */
   } calibration;

   RINGBUFFER_DECLARE(frames, frame, MAX_FRAMES);
   frame    *active_frame;

   /* Frame-pacing EWMA state  (α = 0.5 for both) */
   int64_t  queuing_delay;        /* EWMA of submission-to-GPU-start delay (ns) */
   int64_t  frame_time;           /* EWMA of GPU frame duration (ns) — signed for safe arithmetic */
   uint64_t prev_gpu_start_time;  /* RAW GPU timestamp of previous frame's GPU start */
   uint64_t prev_input_begin;     /* CPU ns of previous input-begin event */

   unsigned      num_queues;
   queue_context queues[];
} device_context;

/* ------------------------------------------------------------------ */
/*  Compile-time assertions                                            */
/* ------------------------------------------------------------------ */

_Static_assert(MAX_QUERIES % 2 == 0,
               "MAX_QUERIES must be even for half-pool double-buffering");
_Static_assert(MAX_FRAMES > 1,
               "need at least 2 frame slots for pipelining");

/* ------------------------------------------------------------------ */
/*  Public API (entry points called by the Vulkan loader)              */
/* ------------------------------------------------------------------ */

device_context *get_device_context(const void *object);

void     anti_lag_AntiLagUpdateAMD(VkDevice device, const VkAntiLagDataAMD *pData);
VkResult anti_lag_QueueSubmit2KHR(VkQueue queue, uint32_t submitCount,
                                  const VkSubmitInfo2 *pSubmits, VkFence fence);
VkResult anti_lag_QueueSubmit2(VkQueue queue, uint32_t submitCount,
                               const VkSubmitInfo2 *pSubmits, VkFence fence);
VkResult anti_lag_QueueSubmit(VkQueue queue, uint32_t submitCount,
                              const VkSubmitInfo *pSubmits, VkFence fence);
VkResult anti_lag_QueuePresentKHR(VkQueue queue, const VkPresentInfoKHR *pPresentInfo);
VkResult anti_lag_NegotiateLoaderLayerInterfaceVersion(VkNegotiateLayerInterface *pVersionStruct);

#endif /* ANTI_LAG_LAYER_H */
