/*
 * Copyright © 2025 Valve Corporation
 *
 * SPDX-License-Identifier: MIT
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
/* Constants                                                          */
/* ------------------------------------------------------------------ */
#define MAX_FRAMES   8
#define MAX_QUERIES  256                /* must be even AND divisible by 4
                                         * (two queries per submission)        */

/* Time constants in nanoseconds */
#define ONE_MS    INT64_C(1000000)
#define ONE_S_NS  INT64_C(1000000000)

/* ------------------------------------------------------------------ */
/* Frame state machine                                                */
/* ------------------------------------------------------------------ */
enum frame_state {
   FRAME_INVALID = 0,
   FRAME_INPUT,    /* Frame is being set up on the CPU side             */
   FRAME_SUBMIT,   /* QueueSubmit calls are associating with this frame */
   FRAME_PRESENT,  /* Frame has been presented; timestamps evaluable    */
};

typedef struct frame {
   uint64_t frame_time;     /* GPU work duration, ns (intra-frame)          */
   int64_t  min_delay;      /* min calibrated submit→GPU-start delay, ns    */
   enum frame_state state;
} frame;

/* ------------------------------------------------------------------ */
/* Per-queue timestamp query state                                    */
/* ------------------------------------------------------------------ */
struct query {
   uint64_t gpu_ts;         /* device-clock ticks (calibrated in-place      *
                             * to CPU-ns for "begin" queries during eval)   */
   uint64_t submit_ts;      /* CPU monotonic ns at submission time          */
   VkCommandBuffer cmdbuffer;
};

typedef struct queue_context {
   VkQueue       queue;
   uint32_t      queue_family_idx;
   _Atomic bool  latency_sensitive;        /* one-way flag, set on 1st Present */
   VkCommandPool cmdPool;
   VkQueryPool   queryPool;
   VkSemaphore   semaphore;
   uint64_t      semaphore_value;          /* timeline counter (== #submits)   */

   /* submissions_per_frame counts QUERIES (= 2 × submits)                  */
   uint8_t       submissions_per_frame[MAX_FRAMES];

   RINGBUFFER_DECLARE(queries, struct query, MAX_QUERIES);
} queue_context;

/* ------------------------------------------------------------------ */
/* Device context (per-VkDevice)                                      */
/* ------------------------------------------------------------------ */
typedef struct device_context {
   /* Vulkan dispatch table — all functions the layer intercepts        */
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

   VkDevice             device;
   VkAllocationCallbacks alloc;
   simple_mtx_t         mtx;
   _Atomic bool         enabled;           /* set by AntiLagUpdate(!=OFF)   */

   /* CPU↔GPU timestamp calibration state */
   struct {
      int64_t   delta;                     /* CPU_ns − (GPU_tick · period)  */
      uint64_t  recalibrate_when;          /* CPU monotonic ns deadline     */
      double    timestamp_period;          /* ns per GPU tick               */
   } calibration;

   RINGBUFFER_DECLARE(frames, frame, MAX_FRAMES);
   frame   *active_frame;

   /* Frame-pacing state (upstream algorithm) */
   uint64_t avg_frame_time;                /* EWMA of GPU frame duration    */
   uint64_t delta;                         /* EWMA of |dev| (mean abs dev)  */
   uint64_t prev_input_begin;              /* CPU ns of last INPUT begin    */
   int64_t  queuing_delay;                 /* half of last frame min_delay  */

   unsigned       num_queues;
   queue_context  queues[];
} device_context;

/* ------------------------------------------------------------------ */
/* Compile-time assertions                                            */
/* ------------------------------------------------------------------ */
_Static_assert(MAX_QUERIES % 2 == 0,
               "MAX_QUERIES must be even (begin/end query pairs)");
_Static_assert((MAX_QUERIES / 2) % 2 == 0,
               "MAX_QUERIES/2 must be even (pool-half boundary must align "
               "with begin/end pair boundary)");
_Static_assert(MAX_FRAMES > 1,
               "need at least 2 frame slots for pipelining");
_Static_assert(MAX_QUERIES / 2 <= 255,
               "submissions_per_frame must fit in uint8_t");

/* ------------------------------------------------------------------ */
/* Public API                                                         */
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
