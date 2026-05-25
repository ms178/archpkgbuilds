/*
 * Copyright © 2025 Valve Corporation
 *
 * SPDX-License-Identifier: MIT
 *
 */

#include "anti_lag_layer.h"

#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <assert.h>

#include "util/os_time.h"
#include "util/simple_mtx.h"
#include "util/u_atomic.h"
#include "vulkan/vulkan_core.h"
#include "ringbuffer.h"
#include "vk_alloc.h"
#include "vk_util.h"

/* ------------------------------------------------------------------ */
/*  GNU23-compatible branch-hint macros                                */
/* ------------------------------------------------------------------ */

#define LIKELY(x)   __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)

/* ------------------------------------------------------------------ */
/*  Stack-fallback size limits for the two submit paths                */
/* ------------------------------------------------------------------ */
/* S2_* → queue_submit2 (VkSubmitInfo2); S1_* → QueueSubmit (VkSubmitInfo) */
#define S2_SUB   8
#define S2_CB   16
#define S2_SEM  16
#define S1_SUB   8
#define S1_CB   16
#define S1_SEM  16

/* ================================================================== */
/*  Internal helpers                                                   */
/* ================================================================== */

static queue_context *
get_queue_context(device_context *ctx, VkQueue queue)
{
   /* num_queues is tiny (1–4 typical); linear scan stays L1-hot. */
   for (unsigned i = 0; i < ctx->num_queues; i++) {
      if (ctx->queues[i].queue == queue)
         return &ctx->queues[i];
   }
   return NULL;
}

/* Allocate one query slot for the given frame. Caller must hold queries lock. */
static struct query *
allocate_query(queue_context *queue_ctx, uint32_t frame_idx)
{
   /* Limit: one frame may use at most half the pool. */
   if (UNLIKELY(queue_ctx->submissions_per_frame[frame_idx] >= MAX_QUERIES / 2))
      return NULL;

   /* Double-buffered pool: refuse to allocate into the half that may
    * contain live queries from a not-yet-evaluated frame.  When the
    * ringbuffer is more than half full, the next index lands at either
    * MAX_QUERIES or MAX_QUERIES/2, which marks the start of the other
    * half.  The other half must be fully consumed before we can reuse it. */
   if (queue_ctx->queries.size > MAX_QUERIES / 2) {
      struct query *last = ringbuffer_last(queue_ctx->queries);
      uint32_t next_idx = ringbuffer_index(queue_ctx->queries, last) + 1u;
      if (next_idx == MAX_QUERIES || next_idx == MAX_QUERIES / 2)
         return NULL;
   }

   return ringbuffer_alloc(queue_ctx->queries);
}

/* ================================================================== */
/*  Frame state machine                                                */
/* ================================================================== */

static void
begin_next_frame(device_context *ctx)
{
   frame *next;
   if (ctx->active_frame) {
      /* Transition current active frame from SUBMIT → PRESENT */
      ctx->active_frame->state = FRAME_PRESENT;
      next = ringbuffer_next(ctx->frames, ctx->active_frame);
   } else {
      next = ringbuffer_last(ctx->frames);
   }

   /* NULL-safe: ringbuffer could be empty during early disable. */
   if (LIKELY(next && next->state == FRAME_INPUT)) {
      next->state      = FRAME_SUBMIT;
      ctx->active_frame = next;
   } else {
      ctx->active_frame = NULL;
   }
}

static void
reset_frame(frame *f)
{
   f->frame_idx      = 0;
   f->gpu_start_time = UINT64_MAX;
   f->min_delay      = INT64_MAX;
   f->state          = FRAME_INPUT;
}

/* ================================================================== */
/*  Timestamp calibration (CPU – GPU domain offset EWMA)               */
/* ================================================================== */

static bool
calibrate_timestamps(device_context *ctx)
{
   uint64_t ts[2];
   uint64_t deviation = 0;

   const VkCalibratedTimestampInfoKHR info[2] = {
      { .sType = VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_KHR,
        .timeDomain = VK_TIME_DOMAIN_CLOCK_MONOTONIC_KHR },
      { .sType = VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_KHR,
        .timeDomain = VK_TIME_DOMAIN_DEVICE_KHR },
   };

   VkResult r = ctx->vtable.GetCalibratedTimestampsKHR(
                  ctx->device, 2, info, ts, &deviation);
   if (UNLIKELY(r != VK_SUCCESS))
      return false;

   /* delta = CPU_ns - GPU_tick * period  →  offset to add to GPU timestamps
    * to bring them into the CPU-clock domain.  Use double for the
    * multiply to preserve precision (period can be a very small fraction). */
   int64_t new_delta =
      (int64_t)ts[0] -
      (int64_t)((double)ts[1] * ctx->calibration.timestamp_period);

   if (ctx->calibration.delta == 0) {
      ctx->calibration.delta = new_delta;
   } else {
      /* EWMA with α = 1/8  →  smooths calibration jitter */
      int64_t diff = new_delta - ctx->calibration.delta;
      ctx->calibration.delta += diff / 8;
   }

   ctx->calibration.recalibrate_when = ts[0] + (uint64_t)ONE_S_NS;
   (void)deviation;  /* reserved for future filtering of high-variance samples */
   return true;
}

/* ================================================================== */
/*  Frame evaluation (once per completed frame, under ctx->mtx)        */
/* ================================================================== */

static bool
evaluate_frame(device_context *ctx, frame *f, bool force_wait)
{
   if (UNLIKELY(f->state != FRAME_PRESENT))
      return false;

   const uint32_t frame_idx = ringbuffer_index(ctx->frames, f);
   const int query_flags = VK_QUERY_RESULT_64_BIT | VK_QUERY_RESULT_WAIT_BIT;

   /* ---- Phase 1: verify every queue's timeline has reached the
    *     expected value for this frame.  In force_wait mode we issue
    *     a non-blocking WaitSemaphores (timeout=0) as a driver "poke";
    *     the real blocking happens in GetQueryPoolResults(WAIT_BIT).
    *     In non-force mode we poll with GetSemaphoreCounterValue.    ---- */
   for (unsigned i = 0; i < ctx->num_queues; i++) {
      queue_context *qctx = &ctx->queues[i];

      ringbuffer_lock(qctx->queries);
      uint64_t expected =
         qctx->semaphore_value -
         qctx->queries.size +
         qctx->submissions_per_frame[frame_idx];
      ringbuffer_unlock(qctx->queries);

      if (force_wait) {
         const VkSemaphoreWaitInfo wi = {
            .sType          = VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO,
            .semaphoreCount = 1,
            .pSemaphores    = &qctx->semaphore,
            .pValues        = &expected,
         };
         /* timeout=0 → non-blocking poke.  The real wait happens in
          * GetQueryPoolResults(WAIT_BIT) below.  Changing to UINT64_MAX
          * would add a redundant blocking wait on a path that already
          * blocks — that is incorrect (rejected from Mythos proposal). */
         ctx->vtable.WaitSemaphores(ctx->device, &wi, 0);
      } else {
         uint64_t signaled;
         ctx->vtable.GetSemaphoreCounterValue(
            ctx->device, qctx->semaphore, &signaled);
         if (signaled < expected)
            return false;
      }
   }

   /* ---- Phase 2: harvest timestamp results per queue in batches. ----
    * Strategy (lock release during blocking driver call):
    *   1) Under lock: read head pointer + dimensions.
    *   2) Release lock before the potentially-blocking GetQueryPoolResults.
    *   3) Read results (up to 2 batches, due to ringbuffer wrap).
    *   4) Re-acquire lock, process & free queries.
    *   5) Unlock.
    *   6) Deferred pool-half reset (outside lock).
    *
    * This maximizes concurrency: QueueSubmit threads can append to the
    * tail while we are blocked on the driver call. */
   for (unsigned i = 0; i < ctx->num_queues; i++) {
      queue_context *qctx = &ctx->queues[i];

      ringbuffer_lock(qctx->queries);
      uint32_t remaining = qctx->submissions_per_frame[frame_idx];
      if (UNLIKELY(remaining == 0)) {
         ringbuffer_unlock(qctx->queries);
         continue;
      }

      /* Snapshot head state under lock */
      struct query *first_q = ringbuffer_first(qctx->queries);
      uint32_t base_idx     = ringbuffer_index(qctx->queries, first_q);
      uint32_t contiguous   = (remaining <= MAX_QUERIES - base_idx)
                                 ? remaining
                                 : (MAX_QUERIES - base_idx);
      uint32_t wrapped      = remaining - contiguous;

      /* Release lock during the blocking driver call */
      ringbuffer_unlock(qctx->queries);

      /* Batch 1: contiguous portion (may wrap at MAX_QUERIES) */
      ctx->vtable.GetQueryPoolResults(
         ctx->device, qctx->queryPool,
         base_idx, contiguous,
         sizeof(struct query) * contiguous,
         &first_q->begin_gpu_ts,
         sizeof(struct query),
         query_flags);

      /* Batch 2: wrapped portion (if any) */
      if (UNLIKELY(wrapped > 0)) {
         struct query *wrap_base = (struct query *)qctx->queries.data;
         ctx->vtable.GetQueryPoolResults(
            ctx->device, qctx->queryPool,
            0, wrapped,
            sizeof(struct query) * wrapped,
            &wrap_base->begin_gpu_ts,
            sizeof(struct query),
            query_flags);
      }

      /* Re-acquire lock for processing */
      ringbuffer_lock(qctx->queries);

      bool reset_first  = false;
      bool reset_second = false;

      for (uint32_t j = 0; j < remaining; j++) {
         struct query *q = ringbuffer_first(qctx->queries);

         /* Track earliest RAW GPU start (BEFORE calibration)
          * for GPU frame-time computation in get_wait_time(). */
         if (q->begin_gpu_ts < f->gpu_start_time)
            f->gpu_start_time = q->begin_gpu_ts;

         /* Calibrate: RAW tick → CPU-clock ns */
         q->begin_gpu_ts =
            (uint64_t)ctx->calibration.delta +
            (uint64_t)((double)q->begin_gpu_ts *
                       ctx->calibration.timestamp_period);

         /* Submission-to-GPU delay; may be slightly negative due to
          * clock domain noise — clamp to positive for min tracking. */
         int64_t submission_delay =
            (int64_t)(q->begin_gpu_ts - q->submit_cpu_ts);
         if (LIKELY(submission_delay > 0))
            f->min_delay = (f->min_delay < submission_delay)
                              ? f->min_delay
                              : submission_delay;

         /* Track pool-half boundaries for deferred reset */
         uint32_t idx      = ringbuffer_index(qctx->queries, q);
         uint32_t next_idx = idx + 1u;

         ringbuffer_free(qctx->queries, q);

         if (UNLIKELY(next_idx == MAX_QUERIES))
            reset_second = true;
         else if (UNLIKELY(next_idx == MAX_QUERIES / 2u))
            reset_first  = true;
      }

      /* Single store — all remaining queries for this frame are consumed */
      qctx->submissions_per_frame[frame_idx] = 0;
      ringbuffer_unlock(qctx->queries);

      /* Deferred pool-half resets (outside the lock) */
      if (UNLIKELY(reset_first))
         ctx->vtable.ResetQueryPool(ctx->device, qctx->queryPool,
                                    0, MAX_QUERIES / 2u);
      if (UNLIKELY(reset_second))
         ctx->vtable.ResetQueryPool(ctx->device, qctx->queryPool,
                                    MAX_QUERIES / 2u, MAX_QUERIES / 2u);
   }

   /* Guard: if no submission had a positive delay (clock-domain noise),
    * ensure min_delay is a valid non-negative value. */
   if (UNLIKELY(f->min_delay == INT64_MAX))
      f->min_delay = 0;

   /* If gpu_start_time was never written (no submissions), wrap from
    * UINT64_MAX → 0 to signal "no data" to get_wait_time(). */
   if (UNLIKELY(++f->gpu_start_time == 0))
      f->min_delay = 0;

   return true;
}

/* ================================================================== */
/*  Frame pacing: compute CPU-side sleep delay for next input          */
/* ================================================================== */

static uint64_t
get_wait_time(device_context *ctx)
{
   /* --- Determine force-wait conditions under frames lock --- */
   ringbuffer_lock(ctx->frames);

   bool force_wait = (ctx->frames.size == MAX_FRAMES);
   frame *next = ringbuffer_first(ctx->frames);

   if (UNLIKELY(force_wait && next && next->state != FRAME_PRESENT))
      begin_next_frame(ctx);

   /* If the frame after the next one is already in PRESENT, we're
    * falling behind and must force-wait. */
   if (next) {
      frame *after = ringbuffer_next(ctx->frames, next);
      if (after && after->state == FRAME_PRESENT)
         force_wait = true;
   }

   ringbuffer_unlock(ctx->frames);

   /* --- Evaluate and consume completed frames --- */
   while (next && evaluate_frame(ctx, next, force_wait)) {
      /* EWMA of submission-to-GPU-start delay (α = 0.5) */
      ctx->queuing_delay = (ctx->queuing_delay + next->min_delay) / 2;

      /* If we have two consecutive RAW GPU start timestamps, compute
       * frame duration.  gpu_start_time == 0 means "no data" (washed
       * through the UINT64_MAX-wrap in evaluate_frame). */
      if (LIKELY(next->gpu_start_time != 0 &&
                 ctx->prev_gpu_start_time != 0)) {
         double tick_delta =
            (double)(next->gpu_start_time - ctx->prev_gpu_start_time);
         int64_t ft_ns = (int64_t)(tick_delta * ctx->calibration.timestamp_period);

         /* EWMA with α = 0.5 */
         ctx->frame_time = (ctx->frame_time + ft_ns) / 2;
      }

      ctx->prev_gpu_start_time = next->gpu_start_time;
      force_wait = false;

      /* Consume this frame */
      ringbuffer_lock(ctx->frames);
      {
         next->state = FRAME_INVALID;
         ringbuffer_free(ctx->frames, next);
         next = ringbuffer_first(ctx->frames);

         /* Stop evaluating once the second-next frame is NOT in PRESENT,
          * to avoid consuming too many frames and disrupting pacing. */
         bool stop_eval = true;
         if (next) {
            frame *after = ringbuffer_next(ctx->frames, next);
            stop_eval = (after == NULL || after->state != FRAME_PRESENT);
         }
         if (stop_eval) {
            ringbuffer_unlock(ctx->frames);
            break;
         }
      }
      ringbuffer_unlock(ctx->frames);
   }

   /* --- Compute delay ---
    *   delay = frame_time + queuing_delay - slack
    *   slack = frame_time / 8 + 1 ms
    *
    * This targets the next input starting such that GPU work arrives
    * just as the previous frame completes, minus a safety margin.
    *
    * frame_time is int64_t (signed) so this arithmetic cannot wrap
    * if frame_time < queuing_delay, producing a clean negative result
    * that we clamp to 0 below. */
   int64_t delay = ctx->frame_time +
                   ctx->queuing_delay -
                   ctx->frame_time / 8 -
                   ONE_MS;

   /* Sanity: >100 ms implies <10 FPS or calibration drift. */
   if (UNLIKELY(delay > ONE_MS * 100)) {
      calibrate_timestamps(ctx);
      ctx->queuing_delay = 0;
      ctx->frame_time    = 0;
      delay = 0;
   }

   return (uint64_t)(delay < 0 ? 0 : delay);
}

/* ================================================================== */
/*  Public entry: AntiLagUpdateAMD                                     */
/* ================================================================== */

static void
anti_lag_disable(device_context *ctx)
{
   /* Reset pacing state so the next enable starts fresh */
   ctx->queuing_delay       = 0;
   ctx->frame_time          = 0;
   ctx->prev_gpu_start_time = 0;
   ctx->prev_input_begin    = 0;

   ringbuffer_lock(ctx->frames);
   while (ctx->frames.size > 0) {
      begin_next_frame(ctx);
      frame *f = ringbuffer_first(ctx->frames);
      if (f)
         evaluate_frame(ctx, f, true);
      if (f) {
         f->state = FRAME_INVALID;
         ringbuffer_free(ctx->frames, f);
      }
   }
   ctx->active_frame = NULL;
   ringbuffer_unlock(ctx->frames);
}

VKAPI_ATTR void VKAPI_CALL
anti_lag_AntiLagUpdateAMD(VkDevice device, const VkAntiLagDataAMD *pData)
{
   if (UNLIKELY(pData == NULL))
      return;

   device_context *ctx = get_device_context(device);

   /* --- OFF mode: drain and disable --- */
   if (UNLIKELY(pData->mode == VK_ANTI_LAG_MODE_OFF_AMD)) {
      simple_mtx_lock(&ctx->mtx);
      anti_lag_disable(ctx);
      simple_mtx_unlock(&ctx->mtx);
      return;
   }

   /* --- PRESENT stage: advance frame state machine --- */
   if (pData->pPresentationInfo &&
       pData->pPresentationInfo->stage == VK_ANTI_LAG_STAGE_PRESENT_AMD) {
      uint64_t frame_idx = pData->pPresentationInfo->frameIndex;
      ringbuffer_lock(ctx->frames);
      while (ctx->active_frame &&
             ctx->active_frame->frame_idx <= frame_idx) {
         begin_next_frame(ctx);
      }
      ringbuffer_unlock(ctx->frames);
      return;
   }

   /* --- INPUT stage: compute delay, create frame, sleep --- */
   simple_mtx_lock(&ctx->mtx);

   uint64_t delay = get_wait_time(ctx);

   /* Clamp to honor maxFPS if set */
   if (UNLIKELY(pData->maxFPS > 0)) {
      uint64_t frametime_min = (uint64_t)ONE_S_NS / (uint64_t)pData->maxFPS;
      if (delay < frametime_min)
         delay = frametime_min;
   }

   uint64_t next_deadline = ctx->prev_input_begin + delay;

   /* Single clock sample for this entire call chain */
   uint64_t now = os_time_get_nano();

   /* Periodically recalibrate */
   if (UNLIKELY(next_deadline > ctx->calibration.recalibrate_when))
      calibrate_timestamps(ctx);

   /* --- Allocate the new frame BEFORE sleeping ---
    * This is critical: by making the frame available in the ringbuffer
    * before we sleep, QueueSubmit calls on OTHER threads can proceed
    * while this thread sleeps, improving CPU–GPU overlap. */
   ringbuffer_lock(ctx->frames);
   {
      frame *new_frame = ringbuffer_alloc(ctx->frames);
      if (LIKELY(new_frame != NULL)) {
         reset_frame(new_frame);
         new_frame->frame_idx = pData->pPresentationInfo
                                   ? pData->pPresentationInfo->frameIndex
                                   : 0u;
         if (UNLIKELY(ctx->active_frame == NULL))
            begin_next_frame(ctx);
      }
   }
   ringbuffer_unlock(ctx->frames);

   /* Sleep until deadline (CPU-side delay, typically 2–6 ms).
    * NOTE: we still hold ctx->mtx during sleep.  This is intentional:
    * it prevents a concurrent Update call from observing partial state.
    * The sleep is the dominant latency anyway and the mutex is only
    * contended by 1–2 callers per frame. */
   os_time_nanosleep_until(next_deadline);

   ctx->prev_input_begin = (next_deadline > now) ? next_deadline : now;

   simple_mtx_unlock(&ctx->mtx);
}

/* ================================================================== */
/*  QueueSubmit interceptor helpers                                    */
/* ================================================================== */

static bool
get_commandbuffer(device_context *ctx,
                  queue_context *queue_ctx,
                  VkCommandBuffer *cmdbuffer,
                  bool has_command_buffer,
                  bool has_wait_before_cmdbuffer,
                  bool *early_submit,
                  uint64_t now /* pre-sampled timestamp */)
{
   /* Lock order: frames → queries (must not deadlock).  Hold both
    * to atomically check active_frame and allocate from query pool. */
   ringbuffer_lock(ctx->frames);
   ringbuffer_lock(queue_ctx->queries);

   /* latency_sensitive is a one-way flag (false→true, never cleared).
    * Relaxed load is safe: a transient miss skips one timestamp query,
    * which is harmless.  The release-store in QueuePresentKHR ensures
    * that the flag's visibility is ordered with prior queue init.
    *
    * We cast to (const bool *) for __atomic_load_n compatibility with
    * the _Atomic bool type in queue_context. */
   bool need_query = (ctx->active_frame != NULL) &&
                     __atomic_load_n((const bool *)&queue_ctx->latency_sensitive,
                                     __ATOMIC_RELAXED);

   if (UNLIKELY(!need_query)) {
      ringbuffer_unlock(queue_ctx->queries);
      ringbuffer_unlock(ctx->frames);
      return false;
   }

   const uint32_t frame_idx = ringbuffer_index(ctx->frames, ctx->active_frame);

   /* Early submit: if this is the FIRST submission of a frame AND
    * there are semaphore waits before any command buffer, we inject
    * a bare timestamp command buffer pre-wait to detect queue idle. */
   *early_submit = has_wait_before_cmdbuffer &&
                   queue_ctx->submissions_per_frame[frame_idx] == 0;

   struct query *q = NULL;
   if (LIKELY(has_command_buffer || *early_submit))
      q = allocate_query(queue_ctx, frame_idx);

   if (UNLIKELY(q == NULL)) {
      ringbuffer_unlock(queue_ctx->queries);
      ringbuffer_unlock(ctx->frames);
      return false;
   }

   q->submit_cpu_ts = now;
   *cmdbuffer = q->cmdbuffer;

   queue_ctx->semaphore_value++;
   queue_ctx->submissions_per_frame[frame_idx]++;

   ringbuffer_unlock(queue_ctx->queries);
   ringbuffer_unlock(ctx->frames);
   return true;
}

/* ================================================================== */
/*  QueueSubmit2 (VkSubmitInfo2) — zero-alloc in hot case              */
/* ================================================================== */

static VkResult
queue_submit2_impl(device_context *ctx,
                   VkQueue queue,
                   uint32_t submitCount,
                   const VkSubmitInfo2 *pSubmits,
                   VkFence fence,
                   PFN_vkQueueSubmit2 fp)
{
   queue_context *qctx = get_queue_context(ctx, queue);
   if (UNLIKELY(!ctx->active_frame || !qctx || submitCount == 0))
      return fp(queue, submitCount, pSubmits, fence);

   /* --- Find first submit with command buffers --- */
   bool has_wait = false;
   int first = -1;
   for (uint32_t i = 0; i < submitCount; i++) {
      if (pSubmits[i].waitSemaphoreInfoCount != 0)
         has_wait = true;
      if (pSubmits[i].commandBufferInfoCount > 0) {
         first = (int)i;
         break;
      }
   }

   /* Sample clock once for this entire call */
   uint64_t now = os_time_get_nano();

   VkCommandBuffer ts_cb;
   bool early = false;
   if (UNLIKELY(!get_commandbuffer(ctx, qctx, &ts_cb,
                                   first >= 0, has_wait,
                                   &early, now)))
      return fp(queue, submitCount, pSubmits, fence);

   /* --- Compute required sizes --- */
   uint32_t total_sub, total_cb, total_sem;

   if (early) {
      first = 0;
      total_sub = submitCount + 1u;
      total_cb  = 1;
      total_sem = 1;
   } else if (UNLIKELY(first < 0)) {
      first = 0;
      total_sub = submitCount;
      total_cb  = 1;
      total_sem = 1;
   } else {
      total_sub = submitCount;
      total_cb  = 1u + pSubmits[first].commandBufferInfoCount;
      total_sem = 1u + pSubmits[first].signalSemaphoreInfoCount;
   }

   /* --- Stack-preferring allocation ---
    * Stack arrays at function scope (NEVER inside if/else blocks)
    * to avoid use-after-scope.  Heap fallback for pathological cases. */
   VkSubmitInfo2             subs_s[S2_SUB];
   VkCommandBufferSubmitInfo cbs_s[S2_CB];
   VkSemaphoreSubmitInfo     sems_s[S2_SEM];

   VkSubmitInfo2             *subs;
   VkCommandBufferSubmitInfo *cbs;
   VkSemaphoreSubmitInfo     *sems;
   void                      *heap = NULL;

   if (LIKELY(total_sub <= S2_SUB && total_cb <= S2_CB && total_sem <= S2_SEM)) {
      subs = subs_s; cbs = cbs_s; sems = sems_s;
      memset(subs_s, 0, sizeof(subs_s));
   } else {
      VK_MULTIALLOC(ma);
      vk_multialloc_add(&ma, &subs, VkSubmitInfo2,             total_sub);
      vk_multialloc_add(&ma, &cbs,  VkCommandBufferSubmitInfo, total_cb);
      vk_multialloc_add(&ma, &sems, VkSemaphoreSubmitInfo,     total_sem);
      heap = vk_multialloc_zalloc(&ma, &ctx->alloc,
                                  VK_SYSTEM_ALLOCATION_SCOPE_COMMAND);
      if (UNLIKELY(!heap))
         return VK_ERROR_OUT_OF_HOST_MEMORY;
   }

   /* --- Copy and modify submits --- */
   if (early) {
      memcpy(subs + 1, pSubmits, sizeof(VkSubmitInfo2) * submitCount);
      subs[0] = (VkSubmitInfo2){ .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO_2 };
   } else {
      memcpy(subs, pSubmits, sizeof(VkSubmitInfo2) * submitCount);
   }

   VkSubmitInfo2 *si = &subs[first];

   /* Prepend timestamp command buffer */
   cbs[0] = (VkCommandBufferSubmitInfo){
      .sType         = VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO,
      .commandBuffer = ts_cb,
   };
   memcpy(cbs + 1, si->pCommandBufferInfos,
          sizeof(VkCommandBufferSubmitInfo) * si->commandBufferInfoCount);
   si->pCommandBufferInfos     = cbs;
   si->commandBufferInfoCount += 1;

   /* Append timeline semaphore signal */
   memcpy(sems, si->pSignalSemaphoreInfos,
          sizeof(VkSemaphoreSubmitInfo) * si->signalSemaphoreInfoCount);
   sems[si->signalSemaphoreInfoCount] = (VkSemaphoreSubmitInfo){
      .sType     = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO,
      .semaphore = qctx->semaphore,
      .value     = qctx->semaphore_value,
      .stageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT,
   };
   si->pSignalSemaphoreInfos     = sems;
   si->signalSemaphoreInfoCount += 1;

   /* --- Submit --- */
   uint32_t final_count = early ? submitCount + 1u : submitCount;
   VkResult r = fp(queue, final_count, subs, fence);
   if (UNLIKELY(heap))
      vk_free(&ctx->alloc, heap);
   return r;
}

VKAPI_ATTR VkResult VKAPI_CALL
anti_lag_QueueSubmit2KHR(VkQueue queue, uint32_t submitCount,
                         const VkSubmitInfo2 *pSubmits, VkFence fence)
{
   device_context *ctx = get_device_context(queue);
   return queue_submit2_impl(ctx, queue, submitCount, pSubmits, fence,
                             ctx->vtable.QueueSubmit2KHR);
}

VKAPI_ATTR VkResult VKAPI_CALL
anti_lag_QueueSubmit2(VkQueue queue, uint32_t submitCount,
                      const VkSubmitInfo2 *pSubmits, VkFence fence)
{
   device_context *ctx = get_device_context(queue);
   return queue_submit2_impl(ctx, queue, submitCount, pSubmits, fence,
                             ctx->vtable.QueueSubmit2);
}

/* ================================================================== */
/*  QueueSubmit (VkSubmitInfo, Vulkan 1.0)  — zero-alloc in hot case  */
/* ================================================================== */

VKAPI_ATTR VkResult VKAPI_CALL
anti_lag_QueueSubmit(VkQueue queue, uint32_t submitCount,
                     const VkSubmitInfo *pSubmits, VkFence fence)
{
   device_context *ctx  = get_device_context(queue);
   queue_context  *qctx = get_queue_context(ctx, queue);
   if (UNLIKELY(!ctx->active_frame || !qctx || submitCount == 0))
      return ctx->vtable.QueueSubmit(queue, submitCount, pSubmits, fence);

   /* --- Find first submit with command buffers --- */
   bool has_wait = false;
   int first = -1;
   for (uint32_t i = 0; i < submitCount; i++) {
      if (pSubmits[i].waitSemaphoreCount != 0)
         has_wait = true;
      if (pSubmits[i].commandBufferCount > 0) {
         first = (int)i;
         break;
      }
   }

   uint64_t now = os_time_get_nano();

   VkCommandBuffer ts_cb;
   bool early = false;
   if (UNLIKELY(!get_commandbuffer(ctx, qctx, &ts_cb,
                                   first >= 0, has_wait,
                                   &early, now)))
      return ctx->vtable.QueueSubmit(queue, submitCount, pSubmits, fence);

   /* --- Compute required sizes --- */
   uint32_t total_sub, total_cb, total_sem;

   if (early) {
      first = 0;
      total_sub = submitCount + 1u;
      total_cb  = 1;
      total_sem = 1;
   } else if (UNLIKELY(first < 0)) {
      first = 0;
      total_sub = submitCount;
      total_cb  = 1;
      total_sem = 1;
   } else {
      total_sub = submitCount;
      total_cb  = 1u + pSubmits[first].commandBufferCount;
      total_sem = 1u + pSubmits[first].signalSemaphoreCount;
   }

   /* --- Stack-preferring allocation ---
    * Stack arrays at function scope to avoid use-after-scope.
    * Sizes: 8 submits, 16 cmdbufs, 16 semas, 1 tlssi copy, 17 sema values. */
   VkSubmitInfo                  subs_s[S1_SUB];
   VkCommandBuffer               cbs_s[S1_CB];
   VkSemaphore                   sems_s[S1_SEM];
   VkTimelineSemaphoreSubmitInfo tlssi_s;
   uint64_t                      vals_s[S1_SEM + 1]; /* +1 for our sema value */

   VkSubmitInfo                  *subs;
   VkCommandBuffer               *cbs;
   VkSemaphore                   *sems;
   VkTimelineSemaphoreSubmitInfo *tlssi_copy;
   uint64_t                      *vals;
   void                          *heap = NULL;

   if (LIKELY(total_sub <= S1_SUB && total_cb <= S1_CB && total_sem <= S1_SEM)) {
      subs = subs_s; cbs = cbs_s; sems = sems_s;
      tlssi_copy = &tlssi_s;
      vals       = vals_s;
      memset(subs_s,  0, sizeof(subs_s));
      memset(&tlssi_s, 0, sizeof(tlssi_s));
      memset(vals_s,   0, sizeof(vals_s));
   } else {
      VK_MULTIALLOC(ma);
      vk_multialloc_add(&ma, &subs,        VkSubmitInfo,                  total_sub);
      vk_multialloc_add(&ma, &cbs,         VkCommandBuffer,               total_cb);
      vk_multialloc_add(&ma, &sems,        VkSemaphore,                   total_sem);
      vk_multialloc_add(&ma, &tlssi_copy,  VkTimelineSemaphoreSubmitInfo, 1);
      vk_multialloc_add(&ma, &vals,        uint64_t,                      total_sem);
      heap = vk_multialloc_zalloc(&ma, &ctx->alloc,
                                  VK_SYSTEM_ALLOCATION_SCOPE_COMMAND);
      if (UNLIKELY(!heap))
         return VK_ERROR_OUT_OF_HOST_MEMORY;
   }

   /* --- Copy and modify submits --- */
   if (early) {
      memcpy(subs + 1, pSubmits, sizeof(VkSubmitInfo) * submitCount);
      subs[0] = (VkSubmitInfo){ .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO };
   } else {
      memcpy(subs, pSubmits, sizeof(VkSubmitInfo) * submitCount);
   }

   VkSubmitInfo *si = &subs[first];

   /* Prepend timestamp command buffer */
   cbs[0] = ts_cb;
   memcpy(cbs + 1, si->pCommandBuffers,
          sizeof(VkCommandBuffer) * si->commandBufferCount);
   si->pCommandBuffers     = cbs;
   si->commandBufferCount += 1;

   /* ---- Timeline semaphore chain extension ----
    *
    * We inject our anti-lag timeline semaphore into the signal list.
    * The VkSubmitInfo may already carry a VkTimelineSemaphoreSubmitInfo
    * (TLSSI) in its pNext chain.
    *
    * STRATEGY (fixes original UB: NO writes through const pointers —
    *          Mythos version retains this bug):
    *
    *   (a) Save si->pNext BEFORE any modification (const-original pNext).
    *   (b) If original TLSSI exists: DEEP-COPY it into tlssi_copy,
    *       splice out the original, link in our copy.
    *   (c) If no original: create a fresh TLSSI in tlssi_copy.
    *   (d) After submit: restore si->pNext = orig_pNext.
    *
    * We NEVER write through tlssi_orig (const pointer from
    * vk_find_struct_const).  Only our local copy is modified.
    * The orig_pNext is const void *const to avoid -Wdiscarded-qualifiers
    * (this was the warning from the Mesa build).                 --- */
   const void *const orig_pNext = si->pNext;

   const VkTimelineSemaphoreSubmitInfo *tlssi_orig =
      vk_find_struct_const(orig_pNext, TIMELINE_SEMAPHORE_SUBMIT_INFO);

   /* Fill signal arrays: our semaphore first, then originals */
   sems[0] = qctx->semaphore;
   if (LIKELY(si->signalSemaphoreCount > 0))
      memcpy(sems + 1, si->pSignalSemaphores,
             sizeof(VkSemaphore) * si->signalSemaphoreCount);
   si->pSignalSemaphores     = sems;
   si->signalSemaphoreCount += 1;

   vals[0] = qctx->semaphore_value;

   if (tlssi_orig) {
      /* Deep copy the original TLSSI into our local buffer.
       * After this, tlssi_copy is independent of the application's chain. */
      *tlssi_copy = *tlssi_orig;

      /* Append original semaphore values after ours */
      if (tlssi_orig->signalSemaphoreValueCount > 0)
         memcpy(vals + 1,
                tlssi_orig->pSignalSemaphoreValues,
                sizeof(uint64_t) * tlssi_orig->signalSemaphoreValueCount);

      /* Update our copy with the extended arrays */
      tlssi_copy->signalSemaphoreValueCount = si->signalSemaphoreCount;
      tlssi_copy->pSignalSemaphoreValues    = vals;

      /* Splice in our copy: pNext → (rest of chain after original).
       * The original is bypassed; we never modified it.
       * No cast needed: both tlssi_copy->pNext and tlssi_orig->pNext
       * are const void* (matching the VkTimelineSemaphoreSubmitInfo
       * struct definition).  This avoids -Wdiscarded-qualifiers. */
      tlssi_copy->pNext = tlssi_orig->pNext;
      si->pNext = tlssi_copy;
   } else {
      /* No original TLSSI — create fresh.  Inherits si's original pNext. */
      *tlssi_copy = (VkTimelineSemaphoreSubmitInfo){
         .sType                     = VK_STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO,
         .pNext                     = orig_pNext,
         .signalSemaphoreValueCount = si->signalSemaphoreCount,
         .pSignalSemaphoreValues    = vals,
      };
      si->pNext = tlssi_copy;
   }

   /* --- Submit --- */
   uint32_t final_count = early ? submitCount + 1u : submitCount;
   VkResult r = ctx->vtable.QueueSubmit(queue, final_count, subs, fence);

   /* Restore application's pNext chain.
    * We only modified si->pNext (our COPY of the submit info);
    * the original structures are untouched. */
   si->pNext = orig_pNext;

   if (UNLIKELY(heap))
      vk_free(&ctx->alloc, heap);

   return r;
}

/* ================================================================== */
/*  QueuePresentKHR interceptor                                        */
/* ================================================================== */

VKAPI_ATTR VkResult VKAPI_CALL
anti_lag_QueuePresentKHR(VkQueue queue, const VkPresentInfoKHR *pPresentInfo)
{
   /* Mark this queue as latency-sensitive.  Once set, get_commandbuffer
    * will record timestamp queries for this queue's submissions.
    *
    * Store with release ordering: paired with the relaxed load in
    * get_commandbuffer.  On x86-64 (TSO), this compiles to a plain
    * mov + compiler barrier, but formally documents the ordering. */
   device_context *ctx  = get_device_context(queue);
   queue_context  *qctx = get_queue_context(ctx, queue);
   if (LIKELY(qctx))
      __atomic_store_n(&qctx->latency_sensitive, true, __ATOMIC_RELEASE);

   return ctx->vtable.QueuePresentKHR(queue, pPresentInfo);
}
