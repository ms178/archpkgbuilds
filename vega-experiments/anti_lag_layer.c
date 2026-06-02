/*
 * Copyright © 2025 Valve Corporation
 *
 * SPDX-License-Identifier: MIT
 */

#include "anti_lag_layer.h"
#include <string.h>
#include "util/os_time.h"
#include "util/simple_mtx.h"
#include "util/u_atomic.h"
#include "vulkan/vulkan_core.h"
#include "ringbuffer.h"
#include "vk_alloc.h"
#include "vk_util.h"

/* ------------------------------------------------------------------ */
/* GNU23-compatible branch-hint macros                                */
/* ------------------------------------------------------------------ */
#define LIKELY(x)   __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)

/* ------------------------------------------------------------------ */
/* Stack-fallback size limits for the two submit paths                */
/* ------------------------------------------------------------------ */
/* These bound the *common* case so we never call vk_alloc per submit.
 * Real-world games stay well under: 1-3 user submits per Submit call,
 * 1-4 CBs per submit, 0-2 signal semaphores.                          */
#define S2_SUB 8
#define S2_CB  32   /* holds both begin- and end-segment CB infos      */
#define S2_SEM 16

#define S1_SUB 8
#define S1_CB  32
#define S1_SEM 16

/* ================================================================== */
/* Internal helpers                                                   */
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

/* Allocate a (begin,end) query pair atomically.  We check capacity
 * up-front so the second allocation cannot fail — this avoids any
 * dependence on tail-free semantics (Mesa's ringbuffer is FIFO,
 * supporting head-free only).  Caller MUST hold the queries lock.    *
 *
 *   - submissions_per_frame counts QUERIES (2× submits), so the per-
 *     frame cap is MAX_QUERIES/2 queries (= MAX_QUERIES/4 submits per
 *     frame).  Reserving 2 needs (count + 2) ≤ MAX_QUERIES/2.
 *   - The ringbuffer itself must have ≥2 free slots.                  */
static bool
allocate_query_pair(queue_context *queue_ctx, uint32_t frame_idx,
                    struct query **out_begin, struct query **out_end)
{
   if (UNLIKELY((uint32_t)queue_ctx->submissions_per_frame[frame_idx] + 2u
                > (uint32_t)(MAX_QUERIES / 2u)))
      return false;
   if (UNLIKELY((uint32_t)queue_ctx->queries.size + 2u > (uint32_t)MAX_QUERIES))
      return false;

   struct query *b = ringbuffer_alloc(queue_ctx->queries);
   struct query *e = ringbuffer_alloc(queue_ctx->queries);
   /* Capacity guaranteed above; runtime asserts catch ringbuffer bugs. */
   assert(b && e);
   *out_begin = b;
   *out_end   = e;
   return true;
}

/* ================================================================== */
/* Frame state machine                                                */
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
      next->state = FRAME_SUBMIT;
      ctx->active_frame = next;
   } else {
      ctx->active_frame = NULL;
   }
}

static void
reset_frame(frame *f)
{
   /* assert is best-effort; do not rely on it in release builds.      */
   assert(f->state == FRAME_INVALID);
   f->frame_time = 0;
   f->min_delay  = 0;
   f->state      = FRAME_INPUT;
}

/* ================================================================== */
/* Timestamp calibration (CPU – GPU domain offset EWMA)               */
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

   /* delta = CPU_ns − GPU_tick · period → offset that, added to a GPU
    * timestamp, lifts it into the CPU-clock domain.  Use double for
    * the multiply to preserve precision (period is a small fraction). */
   int64_t new_delta = (int64_t)ts[0]
      - (int64_t)((double)ts[1] * ctx->calibration.timestamp_period);

   if (ctx->calibration.delta == 0) {
      ctx->calibration.delta = new_delta;
   } else {
      /* EWMA with α = 1/8 → smooths calibration jitter */
      int64_t diff = new_delta - ctx->calibration.delta;
      ctx->calibration.delta += diff / 8;
   }
   ctx->calibration.recalibrate_when = ts[0] + (uint64_t)ONE_S_NS;

   (void)deviation; /* reserved for future filtering of noisy samples */
   return true;
}

/* ================================================================== */
/* Frame evaluation (once per completed frame, under ctx->mtx)        */
/* ================================================================== */

static bool
evaluate_frame(device_context *ctx, frame *f, bool force_wait)
{
   if (UNLIKELY(f->state != FRAME_PRESENT))
      return false;

   const uint32_t frame_idx = ringbuffer_index(ctx->frames, f);
   const int query_flags = VK_QUERY_RESULT_64_BIT | VK_QUERY_RESULT_WAIT_BIT;

   /* ---- Phase 1: verify every queue's timeline has reached the
    *      expected value for this frame.  In force_wait mode we just
    *      poke (timeout=0); GetQueryPoolResults(WAIT_BIT) does the
    *      real blocking.  In non-force mode we poll cheaply.       ---- */
   for (unsigned i = 0; i < ctx->num_queues; i++) {
      queue_context *qctx = &ctx->queues[i];

      ringbuffer_lock(qctx->queries);
      /* queries.size and submissions_per_frame count QUERIES (=2×submits)
       * but the semaphore is signalled once per submit, so /2 to map.   */
      uint64_t expected = qctx->semaphore_value
                        - (uint64_t)(qctx->queries.size / 2u)
                        + (uint64_t)(qctx->submissions_per_frame[frame_idx] / 2u);
      ringbuffer_unlock(qctx->queries);

      if (force_wait) {
         const VkSemaphoreWaitInfo wi = {
            .sType          = VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO,
            .semaphoreCount = 1,
            .pSemaphores    = &qctx->semaphore,
            .pValues        = &expected,
         };
         /* timeout=0 → non-blocking poke.  Real wait is in WAIT_BIT
          * below; doubling the wait would be redundant.               */
         ctx->vtable.WaitSemaphores(ctx->device, &wi, 0);
      } else {
         uint64_t signaled;
         ctx->vtable.GetSemaphoreCounterValue(
            ctx->device, qctx->semaphore, &signaled);
         if (signaled < expected)
            return false;
      }
   }

   /* ---- Phase 2: read back timestamps and update statistics.    ---- */
   uint64_t raw_gpu_start = UINT64_MAX;     /* in GPU ticks       */
   uint64_t raw_gpu_end   = 0;              /* in GPU ticks       */
   int64_t  min_delay     = INT64_MAX;

   for (unsigned i = 0; i < ctx->num_queues; i++) {
      queue_context *qctx = &ctx->queues[i];

      ringbuffer_lock(qctx->queries);
      uint32_t remaining = qctx->submissions_per_frame[frame_idx];
      if (UNLIKELY(remaining == 0)) {
         ringbuffer_unlock(qctx->queries);
         continue;
      }

      /* Snapshot head state */
      struct query *first_q = ringbuffer_first(qctx->queries);
      uint32_t base_idx     = ringbuffer_index(qctx->queries, first_q);
      uint32_t contiguous   = (remaining < MAX_QUERIES - base_idx)
                                ? remaining : MAX_QUERIES - base_idx;
      uint32_t wrapped      = remaining - contiguous;

      /* Batch 1: contiguous portion (may stop at pool wrap) */
      ctx->vtable.GetQueryPoolResults(
         ctx->device, qctx->queryPool, base_idx, contiguous,
         sizeof(struct query) * contiguous,
         &first_q->gpu_ts, sizeof(struct query),
         query_flags);

      /* Batch 2: wrapped portion (if any) */
      if (UNLIKELY(wrapped > 0)) {
         struct query *wrap_base = (struct query *)qctx->queries.data;
         ctx->vtable.GetQueryPoolResults(
            ctx->device, qctx->queryPool, 0, wrapped,
            sizeof(struct query) * wrapped,
            &wrap_base->gpu_ts, sizeof(struct query),
            query_flags);
      }

      /* Process each query: even = begin, odd = end of a submission. */
      bool reset_first  = false;
      bool reset_second = false;
      for (uint32_t j = 0; j < remaining; j++) {
         struct query *q = ringbuffer_first(qctx->queries);

         if ((j & 1u) == 0u) {
            /* Begin timestamp. */
            uint64_t raw = q->gpu_ts;
            if (raw < raw_gpu_start)
               raw_gpu_start = raw;

            /* Calibrate: raw → CPU-ns.  Use double for precision. */
            int64_t cal = ctx->calibration.delta
                        + (int64_t)((double)raw * ctx->calibration.timestamp_period);
            q->gpu_ts = (uint64_t)cal;

            /* Submission-to-GPU delay; may be negative due to clock
             * domain noise.                                          */
            int64_t submission_delay = cal - (int64_t)q->submit_ts;
            if (submission_delay < min_delay)
               min_delay = submission_delay;
         } else {
            /* End timestamp (raw ticks). */
            if (q->gpu_ts > raw_gpu_end)
               raw_gpu_end = q->gpu_ts;
         }

         /* Track pool-half boundaries to allow ResetQueryPool in
          * larger batches.                                          */
         uint32_t idx = ringbuffer_index(qctx->queries, q);
         uint32_t next_idx = idx + 1u;
         ringbuffer_free(qctx->queries, q);

         if (UNLIKELY(next_idx == MAX_QUERIES))
            reset_second = true;
         else if (UNLIKELY(next_idx == MAX_QUERIES / 2u))
            reset_first = true;
      }

      /* Single store — this frame's queries are fully consumed. */
      qctx->submissions_per_frame[frame_idx] = (uint8_t)0;

      /* Reset the just-vacated half(s) under the queries lock to
       * respect VkQueryPool external synchronisation.  Hot in
       * benchmarks: only fires at most twice per ringbuffer cycle. */
      if (UNLIKELY(reset_first))
         ctx->vtable.ResetQueryPool(ctx->device, qctx->queryPool,
                                    0, MAX_QUERIES / 2u);
      if (UNLIKELY(reset_second))
         ctx->vtable.ResetQueryPool(ctx->device, qctx->queryPool,
                                    MAX_QUERIES / 2u, MAX_QUERIES / 2u);
      ringbuffer_unlock(qctx->queries);
   }

   /* Commit statistics into the frame. */
   if (LIKELY(raw_gpu_end > 0 && raw_gpu_end >= raw_gpu_start)) {
      f->frame_time = (uint64_t)((double)(raw_gpu_end - raw_gpu_start)
                                 * ctx->calibration.timestamp_period);
      f->min_delay  = (min_delay == INT64_MAX) ? 0 : min_delay;
   } else {
      /* No usable samples (no submissions, or malformed timestamps). */
      f->frame_time = 0;
      f->min_delay  = 0;
   }

   return true;
}

/* ================================================================== */
/* Frame pacing — compute CPU-side sleep delay for next input         */
/* ================================================================== */

static uint64_t
get_wait_time(device_context *ctx)
{
   ringbuffer_lock(ctx->frames);
   frame *next        = ringbuffer_first(ctx->frames);
   /* If we have at least 3 frames queued, then at least 2 frames have
    * already been submitted to the GPU; it is safe to block-wait on
    * the oldest.                                                     */
   bool   force_wait  = (ctx->frames.size >= 3u);
   ringbuffer_unlock(ctx->frames);

   while (next && evaluate_frame(ctx, next, force_wait)) {

      if (LIKELY(next->frame_time != 0u)) {
         /* delta = |target − actual|, EWMA(α=1/8) of mean absolute
          * deviation.  target = queuing_delay + avg_frame_time.      *
          * All math is performed in int64_t to keep -Wconversion happy
          * and to clamp the EWMA to a non-negative magnitude.        */
         int64_t target    = ctx->queuing_delay + (int64_t)ctx->avg_frame_time;
         int64_t actual    = next->min_delay + (int64_t)next->frame_time;
         int64_t d         = target - actual;
         if (d < 0) d = -d;
         int64_t cur_delta = (int64_t)ctx->delta;
         int64_t diff      = d - cur_delta;
         int64_t new_delta = cur_delta + diff / 8;
         if (new_delta < 0) new_delta = 0;
         ctx->delta        = (uint64_t)new_delta;

         /* avg_frame_time: EWMA(α=0.5) of GPU frame duration. */
         ctx->avg_frame_time = (ctx->avg_frame_time + next->frame_time) / 2u;
      }

      /* queuing_delay: aim to halve the observed sub→GPU-start delay. */
      ctx->queuing_delay = next->min_delay / 2;

      /* Consume the frame. */
      ringbuffer_lock(ctx->frames);
      next->state = FRAME_INVALID;
      ringbuffer_free(ctx->frames, next);
      next = ringbuffer_first(ctx->frames);
      ringbuffer_unlock(ctx->frames);

      /* Don't force-wait the next iteration; only opportunistically
       * evaluate, and only while we still have ≥2 frames in flight.  */
      force_wait = false;
      if (ctx->frames.size <= 2u)
         break;
   }

   /* delay = avg_frame_time + queuing_delay − slack
    * slack = min(delta, 1 ms)                                        */
   int64_t slack = (int64_t)ctx->delta;
   if (slack > ONE_MS) slack = ONE_MS;
   int64_t delay = (int64_t)ctx->avg_frame_time + ctx->queuing_delay - slack;

   /* Sanity: >100 ms ⇒ <10 FPS, likely a measurement glitch. */
   if (UNLIKELY(delay > ONE_MS * 100)) {
      calibrate_timestamps(ctx);
      ctx->avg_frame_time = 0;
      ctx->delta          = 0;
      ctx->queuing_delay  = 0;
      delay               = 0;
   }

   return (uint64_t)(delay < 0 ? 0 : delay);
}

/* ================================================================== */
/* anti_lag_disable — drain everything safely                         */
/* ================================================================== */

static void
anti_lag_disable(device_context *ctx)
{
   __atomic_store_n(&ctx->enabled, false, __ATOMIC_RELEASE);
   ctx->avg_frame_time = (uint64_t)ONE_MS;
   ctx->delta          = 0;
   ctx->queuing_delay  = 0;
   ctx->prev_input_begin = 0;

   ringbuffer_lock(ctx->frames);
   while (ctx->frames.size > 0u) {
      /* Force-wait so every pending timestamp query completes. */
      begin_next_frame(ctx);
      frame *f = ringbuffer_first(ctx->frames);
      if (LIKELY(f != NULL)) {
         evaluate_frame(ctx, f, true);
         f->state = FRAME_INVALID;
         ringbuffer_free(ctx->frames, f);
      } else {
         break;       /* defensive — should not happen */
      }
   }
   ctx->active_frame = NULL;
   ringbuffer_unlock(ctx->frames);
}

/* ================================================================== */
/* AntiLagUpdateAMD — pace the simulation thread                      */
/* ================================================================== */

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

   /* Latch the enabled flag (release: any future load with acquire
    * sees a fully-initialised context).                             */
   __atomic_store_n(&ctx->enabled, true, __ATOMIC_RELEASE);

   /* --- PRESENT stage: no-op in the new design --- *
    * The simulation-time accounting that *could* be derived here is
    * not implemented; frame allocation now happens in QueuePresentKHR. */
   if (pData->pPresentationInfo &&
       pData->pPresentationInfo->stage == VK_ANTI_LAG_STAGE_PRESENT_AMD)
      return;

   /* --- INPUT stage: compute delay, sleep until next deadline --- */
   simple_mtx_lock(&ctx->mtx);

   uint64_t delay = get_wait_time(ctx);

   /* Clamp delay to honour an application-requested maxFPS. */
   if (UNLIKELY(pData->maxFPS > 0)) {
      uint64_t frametime_min = (uint64_t)ONE_S_NS / (uint64_t)pData->maxFPS;
      if (delay < frametime_min)
         delay = frametime_min;
   }

   uint64_t next_deadline = ctx->prev_input_begin + delay;
   uint64_t now           = os_time_get_nano();

   /* Periodically recalibrate. */
   if (UNLIKELY(next_deadline > ctx->calibration.recalibrate_when))
      calibrate_timestamps(ctx);

   /* Sleep until the deadline.  We retain ctx->mtx during sleep on
    * purpose: this prevents a concurrent Update call from observing
    * partial pacing state.  Only the input thread contends here.   */
   os_time_nanosleep_until(next_deadline);

   ctx->prev_input_begin = (next_deadline > now) ? next_deadline : now;
   simple_mtx_unlock(&ctx->mtx);
}

/* ================================================================== */
/* QueueSubmit interceptor helpers                                    */
/* ================================================================== */

/* Allocate (begin, end) query-pair for the next submission on this
 * queue.  Returns false (and a no-op delegation to the underlying
 * implementation) when the pool is exhausted or the queue is not yet
 * latency-sensitive.  *out_early is true when the caller must inject
 * an extra zero-CB submit to capture queue-acquire time before the
 * application's first wait.                                          */
static bool
get_commandbuffer(device_context *ctx, queue_context *queue_ctx,
                  VkCommandBuffer *ts_begin, VkCommandBuffer *ts_end,
                  bool has_command_buffer, bool has_wait_before_cmdbuffer,
                  bool *early_submit, uint64_t now)
{
   /* Lock order: frames → queries (must not deadlock). */
   ringbuffer_lock(ctx->frames);
   ringbuffer_lock(queue_ctx->queries);

   /* latency_sensitive is a one-way flag; relaxed load is safe.
    * __atomic_load_n accepts a pointer to an _Atomic-qualified type
    * directly, so no cast is needed (avoids -Wcast-qual).            */
   bool need_query = (ctx->active_frame != NULL) &&
      __atomic_load_n(&queue_ctx->latency_sensitive, __ATOMIC_RELAXED);
   if (UNLIKELY(!need_query)) {
      ringbuffer_unlock(queue_ctx->queries);
      ringbuffer_unlock(ctx->frames);
      return false;
   }

   const uint32_t frame_idx = ringbuffer_index(ctx->frames, ctx->active_frame);

   *early_submit = has_wait_before_cmdbuffer &&
                   queue_ctx->submissions_per_frame[frame_idx] == 0u;

   struct query *qb = NULL, *qe = NULL;
   if (LIKELY(has_command_buffer || *early_submit)) {
      if (UNLIKELY(!allocate_query_pair(queue_ctx, frame_idx, &qb, &qe))) {
         ringbuffer_unlock(queue_ctx->queries);
         ringbuffer_unlock(ctx->frames);
         return false;
      }
   } else {
      ringbuffer_unlock(queue_ctx->queries);
      ringbuffer_unlock(ctx->frames);
      return false;
   }

   qb->submit_ts = now;
   qe->submit_ts = now;
   *ts_begin = qb->cmdbuffer;
   *ts_end   = qe->cmdbuffer;

   queue_ctx->semaphore_value += 1u;                          /* one signal per submit  */
   /* Explicit narrow keeps -Wconversion happy (uint8_t array element). */
   queue_ctx->submissions_per_frame[frame_idx] =
      (uint8_t)(queue_ctx->submissions_per_frame[frame_idx] + 2u);

   ringbuffer_unlock(queue_ctx->queries);
   ringbuffer_unlock(ctx->frames);
   return true;
}

/* ================================================================== */
/* QueueSubmit2 (VkSubmitInfo2) — zero-alloc in hot case              */
/* ================================================================== */

static VkResult
queue_submit2_impl(device_context *ctx, VkQueue queue, uint32_t submitCount,
                   const VkSubmitInfo2 *pSubmits, VkFence fence,
                   PFN_vkQueueSubmit2 fp)
{
   queue_context *qctx = get_queue_context(ctx, queue);
   if (UNLIKELY(!ctx->active_frame || !qctx || submitCount == 0u))
      return fp(queue, submitCount, pSubmits, fence);

   /* Find the first submission carrying command buffers; track any
    * earlier wait so we know whether to issue an early submit.    */
   bool has_wait = false;
   int  first    = -1;
   for (uint32_t i = 0; i < submitCount; i++) {
      if (pSubmits[i].waitSemaphoreInfoCount != 0u)
         has_wait = true;
      if (pSubmits[i].commandBufferInfoCount > 0u) {
         first = (int)i;
         break;
      }
   }

   uint64_t now = os_time_get_nano();
   VkCommandBuffer ts_begin, ts_end;
   bool early = false;
   if (UNLIKELY(!get_commandbuffer(ctx, qctx, &ts_begin, &ts_end,
                                   first >= 0, has_wait, &early, now)))
      return fp(queue, submitCount, pSubmits, fence);

   const uint32_t last_idx = submitCount - 1u;

   /* --- Compute layout --- */
   uint32_t total_sub;
   uint32_t total_cb_begin;        /* CB infos for the *begin*-bearing sub  */
   uint32_t total_cb_end;          /* CB infos for the *end*-bearing  sub   */
   uint32_t total_sem;             /* extended signal-sem list for last sub */
   bool     merged;                /* both ts in the same submission        */
   uint32_t last_logical;          /* index of the end-bearing sub in subs[]*/

   if (early) {
      first         = 0;
      total_sub     = submitCount + 1u;
      total_cb_begin = 1u;                                          /* ts_begin alone   */
      total_cb_end   = pSubmits[last_idx].commandBufferInfoCount + 1u;
      merged        = false;
      last_logical  = submitCount;                                  /* shifted by +1    */
   } else if ((uint32_t)first == last_idx) {
      total_sub     = submitCount;
      total_cb_begin = pSubmits[first].commandBufferInfoCount + 2u; /* ts_begin+user+ts_end */
      total_cb_end   = 0u;
      merged        = true;
      last_logical  = last_idx;
   } else {
      total_sub     = submitCount;
      total_cb_begin = pSubmits[first].commandBufferInfoCount + 1u;
      total_cb_end   = pSubmits[last_idx].commandBufferInfoCount + 1u;
      merged        = false;
      last_logical  = last_idx;
   }
   total_sem = pSubmits[last_idx].signalSemaphoreInfoCount + 1u;

   /* --- Stack-fallback allocation --- */
   VkSubmitInfo2             sub_stack[S2_SUB];
   VkCommandBufferSubmitInfo cb_stack[S2_CB];
   VkSemaphoreSubmitInfo     sem_stack[S2_SEM];

   VkSubmitInfo2             *subs;
   VkCommandBufferSubmitInfo *cbs_begin;
   VkCommandBufferSubmitInfo *cbs_end;
   VkSemaphoreSubmitInfo     *sems;
   void                      *heap = NULL;

   uint32_t cb_total = total_cb_begin + total_cb_end;
   if (LIKELY(total_sub <= S2_SUB && cb_total <= S2_CB && total_sem <= S2_SEM)) {
      subs       = sub_stack;
      cbs_begin  = cb_stack;
      cbs_end    = merged ? NULL : (cb_stack + total_cb_begin);
      sems       = sem_stack;
   } else {
      VK_MULTIALLOC(ma);
      vk_multialloc_add(&ma, &subs,      VkSubmitInfo2,             total_sub);
      vk_multialloc_add(&ma, &cbs_begin, VkCommandBufferSubmitInfo, total_cb_begin);
      if (!merged)
         vk_multialloc_add(&ma, &cbs_end, VkCommandBufferSubmitInfo, total_cb_end);
      vk_multialloc_add(&ma, &sems,      VkSemaphoreSubmitInfo,     total_sem);
      heap = vk_multialloc_zalloc(&ma, &ctx->alloc,
                                  VK_SYSTEM_ALLOCATION_SCOPE_COMMAND);
      if (UNLIKELY(!heap))
         return VK_ERROR_OUT_OF_HOST_MEMORY;
      if (merged)
         cbs_end = NULL;
   }

   /* --- Copy submits --- */
   if (early) {
      memcpy(subs + 1, pSubmits, sizeof(VkSubmitInfo2) * submitCount);
      subs[0] = (VkSubmitInfo2){ .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO_2 };
   } else {
      memcpy(subs, pSubmits, sizeof(VkSubmitInfo2) * submitCount);
   }

   /* --- Inject begin timestamp into subs[first] --- */
   {
      VkSubmitInfo2 *si = &subs[first];
      cbs_begin[0] = (VkCommandBufferSubmitInfo){
         .sType         = VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO,
         .commandBuffer = ts_begin,
      };
      if (si->commandBufferInfoCount > 0u)
         memcpy(cbs_begin + 1, si->pCommandBufferInfos,
                sizeof(VkCommandBufferSubmitInfo) * si->commandBufferInfoCount);
      si->pCommandBufferInfos    = cbs_begin;
      si->commandBufferInfoCount = si->commandBufferInfoCount + 1u;
   }

   /* --- Inject end timestamp into subs[last_logical] --- */
   {
      VkSubmitInfo2 *si = &subs[last_logical];
      if (merged) {
         /* commandBufferInfoCount already incremented by +1 above. */
         cbs_begin[si->commandBufferInfoCount] = (VkCommandBufferSubmitInfo){
            .sType         = VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO,
            .commandBuffer = ts_end,
         };
      } else {
         if (si->commandBufferInfoCount > 0u)
            memcpy(cbs_end, si->pCommandBufferInfos,
                   sizeof(VkCommandBufferSubmitInfo) * si->commandBufferInfoCount);
         cbs_end[si->commandBufferInfoCount] = (VkCommandBufferSubmitInfo){
            .sType         = VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO,
            .commandBuffer = ts_end,
         };
         si->pCommandBufferInfos = cbs_end;
      }
      si->commandBufferInfoCount = si->commandBufferInfoCount + 1u;

      /* --- Append timeline semaphore signal --- */
      if (si->signalSemaphoreInfoCount > 0u)
         memcpy(sems, si->pSignalSemaphoreInfos,
                sizeof(VkSemaphoreSubmitInfo) * si->signalSemaphoreInfoCount);
      sems[si->signalSemaphoreInfoCount] = (VkSemaphoreSubmitInfo){
         .sType     = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO,
         .semaphore = qctx->semaphore,
         .value     = qctx->semaphore_value,
         .stageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT,
      };
      si->pSignalSemaphoreInfos    = sems;
      si->signalSemaphoreInfoCount = si->signalSemaphoreInfoCount + 1u;
   }

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
/* QueueSubmit (VkSubmitInfo, Vulkan 1.0) — zero-alloc in hot case    */
/* ================================================================== */

VKAPI_ATTR VkResult VKAPI_CALL
anti_lag_QueueSubmit(VkQueue queue, uint32_t submitCount,
                     const VkSubmitInfo *pSubmits, VkFence fence)
{
   device_context *ctx  = get_device_context(queue);
   queue_context  *qctx = get_queue_context(ctx, queue);
   if (UNLIKELY(!ctx->active_frame || !qctx || submitCount == 0u))
      return ctx->vtable.QueueSubmit(queue, submitCount, pSubmits, fence);

   bool has_wait = false;
   int  first    = -1;
   for (uint32_t i = 0; i < submitCount; i++) {
      if (pSubmits[i].waitSemaphoreCount != 0u)
         has_wait = true;
      if (pSubmits[i].commandBufferCount > 0u) {
         first = (int)i;
         break;
      }
   }

   uint64_t now = os_time_get_nano();
   VkCommandBuffer ts_begin, ts_end;
   bool early = false;
   if (UNLIKELY(!get_commandbuffer(ctx, qctx, &ts_begin, &ts_end,
                                   first >= 0, has_wait, &early, now)))
      return ctx->vtable.QueueSubmit(queue, submitCount, pSubmits, fence);

   const uint32_t last_idx = submitCount - 1u;

   /* --- Compute layout --- */
   uint32_t total_sub, total_cb_begin, total_cb_end, total_sem;
   bool     merged;
   uint32_t last_logical;

   if (early) {
      first          = 0;
      total_sub      = submitCount + 1u;
      total_cb_begin = 1u;
      total_cb_end   = pSubmits[last_idx].commandBufferCount + 1u;
      merged         = false;
      last_logical   = submitCount;
   } else if ((uint32_t)first == last_idx) {
      total_sub      = submitCount;
      total_cb_begin = pSubmits[first].commandBufferCount + 2u;
      total_cb_end   = 0u;
      merged         = true;
      last_logical   = last_idx;
   } else {
      total_sub      = submitCount;
      total_cb_begin = pSubmits[first].commandBufferCount + 1u;
      total_cb_end   = pSubmits[last_idx].commandBufferCount + 1u;
      merged         = false;
      last_logical   = last_idx;
   }
   total_sem = pSubmits[last_idx].signalSemaphoreCount + 1u;

   /* --- Stack-fallback allocation --- */
   VkSubmitInfo                     sub_stack[S1_SUB];
   VkCommandBuffer                  cb_stack[S1_CB];
   VkSemaphore                      sem_stack[S1_SEM];
   uint64_t                         val_stack[S1_SEM];
   VkTimelineSemaphoreSubmitInfo    tlssi_stack;

   VkSubmitInfo    *subs;
   VkCommandBuffer *cbs_begin;
   VkCommandBuffer *cbs_end;
   VkSemaphore     *sems;
   uint64_t        *vals;
   VkTimelineSemaphoreSubmitInfo *tlssi_copy;
   void *heap = NULL;

   uint32_t cb_total = total_cb_begin + total_cb_end;
   if (LIKELY(total_sub <= S1_SUB && cb_total <= S1_CB && total_sem <= S1_SEM)) {
      subs       = sub_stack;
      cbs_begin  = cb_stack;
      cbs_end    = merged ? NULL : (cb_stack + total_cb_begin);
      sems       = sem_stack;
      vals       = val_stack;
      tlssi_copy = &tlssi_stack;
   } else {
      VK_MULTIALLOC(ma);
      vk_multialloc_add(&ma, &subs,       VkSubmitInfo,    total_sub);
      vk_multialloc_add(&ma, &cbs_begin,  VkCommandBuffer, total_cb_begin);
      if (!merged)
         vk_multialloc_add(&ma, &cbs_end, VkCommandBuffer, total_cb_end);
      vk_multialloc_add(&ma, &sems,       VkSemaphore,     total_sem);
      vk_multialloc_add(&ma, &vals,       uint64_t,        total_sem);
      vk_multialloc_add(&ma, &tlssi_copy, VkTimelineSemaphoreSubmitInfo, 1);
      heap = vk_multialloc_zalloc(&ma, &ctx->alloc,
                                  VK_SYSTEM_ALLOCATION_SCOPE_COMMAND);
      if (UNLIKELY(!heap))
         return VK_ERROR_OUT_OF_HOST_MEMORY;
      if (merged)
         cbs_end = NULL;
   }

   /* --- Copy submits --- */
   if (early) {
      memcpy(subs + 1, pSubmits, sizeof(VkSubmitInfo) * submitCount);
      subs[0] = (VkSubmitInfo){ .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO };
   } else {
      memcpy(subs, pSubmits, sizeof(VkSubmitInfo) * submitCount);
   }

   /* --- Inject begin timestamp into subs[first] --- */
   {
      VkSubmitInfo *si = &subs[first];
      cbs_begin[0] = ts_begin;
      if (si->commandBufferCount > 0u)
         memcpy(cbs_begin + 1, si->pCommandBuffers,
                sizeof(VkCommandBuffer) * si->commandBufferCount);
      si->pCommandBuffers   = cbs_begin;
      si->commandBufferCount = si->commandBufferCount + 1u;
   }

   /* --- Inject end timestamp into subs[last_logical] + timeline sema --- */
   VkSubmitInfo *si_last = &subs[last_logical];
   if (merged) {
      cbs_begin[si_last->commandBufferCount] = ts_end;
   } else {
      if (si_last->commandBufferCount > 0u)
         memcpy(cbs_end, si_last->pCommandBuffers,
                sizeof(VkCommandBuffer) * si_last->commandBufferCount);
      cbs_end[si_last->commandBufferCount] = ts_end;
      si_last->pCommandBuffers = cbs_end;
   }
   si_last->commandBufferCount = si_last->commandBufferCount + 1u;

   /* --- Build the extended (user_sems..., our_sem) signal list --- *
    *
    * STRATEGY (preserves const correctness of the application's
    * VkTimelineSemaphoreSubmitInfo and never writes through const
    * pointers; matches RADV's vk_find_struct first-match search):
    *   1. Copy user's signal semaphores to indices [0..N-1].
    *   2. Ours at index N.
    *   3. Mirror values: user's at [0..N-1] (or zero), ours at N.
    *   4. Splice a freshly-built TLSSI at the HEAD of si_last->pNext.
    */
   const uint32_t orig_sig_count = si_last->signalSemaphoreCount;
   if (orig_sig_count > 0u)
      memcpy(sems, si_last->pSignalSemaphores,
             sizeof(VkSemaphore) * orig_sig_count);
   sems[orig_sig_count]              = qctx->semaphore;
   si_last->pSignalSemaphores        = sems;
   si_last->signalSemaphoreCount     = orig_sig_count + 1u;

   const void *orig_pNext = si_last->pNext;
   const VkTimelineSemaphoreSubmitInfo *tlssi_orig =
      vk_find_struct_const(orig_pNext, TIMELINE_SEMAPHORE_SUBMIT_INFO);

   /* Fill the values array.  Indices [0..orig_sig_count-1] mirror user's
    * signal semaphores (zero for non-timeline entries that have no value). */
   if (tlssi_orig && tlssi_orig->signalSemaphoreValueCount > 0u) {
      uint32_t n = tlssi_orig->signalSemaphoreValueCount;
      if (n > orig_sig_count) n = orig_sig_count;
      memcpy(vals, tlssi_orig->pSignalSemaphoreValues, sizeof(uint64_t) * n);
      for (uint32_t k = n; k < orig_sig_count; k++) vals[k] = 0u;
   } else {
      for (uint32_t k = 0; k < orig_sig_count; k++) vals[k] = 0u;
   }
   vals[orig_sig_count] = qctx->semaphore_value;

   if (tlssi_orig) {
      *tlssi_copy = *tlssi_orig;
      tlssi_copy->pNext                       = tlssi_orig->pNext;
      tlssi_copy->signalSemaphoreValueCount   = orig_sig_count + 1u;
      tlssi_copy->pSignalSemaphoreValues      = vals;
      si_last->pNext = tlssi_copy;
   } else {
      *tlssi_copy = (VkTimelineSemaphoreSubmitInfo){
         .sType                       = VK_STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO,
         .pNext                       = orig_pNext,
         .signalSemaphoreValueCount   = orig_sig_count + 1u,
         .pSignalSemaphoreValues      = vals,
      };
      si_last->pNext = tlssi_copy;
   }

   uint32_t final_count = early ? submitCount + 1u : submitCount;
   VkResult r = ctx->vtable.QueueSubmit(queue, final_count, subs, fence);

   /* Restore our local copy's pNext so heap-free is safe (no-op for
    * stack), and so any post-hoc inspection sees clean state.       */
   si_last->pNext = orig_pNext;

   if (UNLIKELY(heap))
      vk_free(&ctx->alloc, heap);
   return r;
}

/* ================================================================== */
/* QueuePresentKHR interceptor — frame allocation pivot               */
/* ================================================================== */

VKAPI_ATTR VkResult VKAPI_CALL
anti_lag_QueuePresentKHR(VkQueue queue, const VkPresentInfoKHR *pPresentInfo)
{
   /* When multiple queues are in flight, the min-delay approach has
    * problems.  An async compute queue could be submitted to with very
    * low delay while the main graphics queue would be swamped with
    * work.  We therefore only treat a queue as latency-sensitive once
    * the application has actually presented from it.  */
   device_context *ctx = get_device_context(queue);

   /* Acquire the enabled flag with acquire ordering — pairs with the
    * release store in AntiLagUpdateAMD.                              */
   if (__atomic_load_n(&ctx->enabled, __ATOMIC_ACQUIRE)) {
      queue_context *qctx = get_queue_context(ctx, queue);
      if (LIKELY(qctx))
         __atomic_store_n(&qctx->latency_sensitive, true, __ATOMIC_RELEASE);

      /* Allocate the next frame slot and advance the state machine.  */
      ringbuffer_lock(ctx->frames);
      /* Double-check under lock — concurrent OFF could have cleared it. */
      if (LIKELY(__atomic_load_n(&ctx->enabled, __ATOMIC_RELAXED))) {
         frame *new_frame = ringbuffer_alloc(ctx->frames);
         if (LIKELY(new_frame != NULL)) {
            new_frame->state = FRAME_INVALID;   /* satisfy reset_frame assert */
            reset_frame(new_frame);
            begin_next_frame(ctx);
         }
      }
      ringbuffer_unlock(ctx->frames);
   }

   return ctx->vtable.QueuePresentKHR(queue, pPresentInfo);
}
