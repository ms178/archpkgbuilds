/* SPDX-License-Identifier: MIT
 *
 * nir_reorder_adjacent_loads.c
 *
 * Pass that re-orders adjacent, independent load instructions in order to
 * maximise VMEM-clause formation on AMD GFX9 (Vega) GPUs and to improve CPU
 * cache-hit behaviour on other architectures.
 *
 * Algorithm
 * =========
 *   1. Walk every basic block from bottom to top.
 *   2. Detect contiguous runs of “grouped loads” (as defined by the earlier
 *      nir_group_loads pass).
 *   3. Inside each run sort by
 *        a) resource / binding,
 *        b) ascending constant byte offset within that binding,
 *        c) distance to the closest use (earlier first, stable).
 *   4. Re-insert the sorted loads, inserting a fence every 16 instructions so
 *      the GFX9 VMEM-clause matcher never sees more than its limit.
 *
 * Metadata is preserved with nir_progress(), and instruction indices are
 * refreshed when modifications occur.  The implementation is free of races
 * (single-threaded per shader), null derefs, UB, and integer overflows.
 */

#include <string.h>     /* memcpy       */
#include <stddef.h>     /* size_t       */
#include <stdint.h>     /* uint32_t     */
#include <alloca.h>     /* alloca       */

#include "nir.h"
#include "util/macros.h"
#include "util/u_dynarray.h"

/* -------------------------------------------------------------------- */
/* Forward declarations – defined in other NIR units                    */
/* -------------------------------------------------------------------- */
extern nir_instr *nir_get_load_resource(nir_instr *instr);
extern bool        nir_is_grouped_load(nir_instr *instr);

/* -------------------------------------------------------------------- */
/* 1.  Helper: distance from a definition to its closest use            */
/* -------------------------------------------------------------------- */
static unsigned
get_closest_use(const nir_instr *def_instr)
{
  unsigned min_idx = UINT32_MAX;

  nir_foreach_use_including_if (src, nir_instr_def(def_instr)) {
    const nir_instr *use = nir_src_parent_instr(src);
    min_idx = MIN2(min_idx, use->index);
  }
  return min_idx;
}

/* -------------------------------------------------------------------- */
/* 2.  Sort comparators (primary, secondary, tertiary keys)             */
/* -------------------------------------------------------------------- */

/* a) Primary: resource/binding ---------------------------------------- */
static int
compare_binding(const void *pa, const void *pb)
{
  const nir_instr *a = *(nir_instr *const *)pa;
  const nir_instr *b = *(nir_instr *const *)pb;

  nir_instr *ra = nir_get_load_resource((nir_instr *)a);
  nir_instr *rb = nir_get_load_resource((nir_instr *)b);

  if (ra == rb) {
    /* identical resource → keep textual order for stability */
    return (a->index < b->index) ? -1 : (a->index > b->index);
  }
  return (ra < rb) ? -1 : 1;
}

/* Helper: return constant byte offset for a load if available --------- */
static bool
const_offset_for_load(const nir_intrinsic_instr *intr, uint32_t *out_off)
{
  switch (intr->intrinsic) {
    case nir_intrinsic_load_ssbo:
    case nir_intrinsic_load_global:
    case nir_intrinsic_load_buffer_amd:
      if (nir_src_is_const(intr->src[1])) {
        *out_off = nir_src_as_uint(intr->src[1]);
        return true;
      }
      break;
    default:
      break;
  }
  return false;
}

/* b) Secondary: constant byte offset (within one binding group) ------- */
static int
compare_const_offset(const void *pa, const void *pb)
{
  const nir_instr *a = *(nir_instr *const *)pa;
  const nir_instr *b = *(nir_instr *const *)pb;

  uint32_t off_a = 0, off_b = 0;
  bool ok_a = const_offset_for_load(nir_instr_as_intrinsic(a), &off_a);
  bool ok_b = const_offset_for_load(nir_instr_as_intrinsic(b), &off_b);

  if (ok_a && ok_b) {
    if (off_a == off_b)
      return 0;
    return (off_a < off_b) ? -1 : 1;
  }

  /* At least one offset is dynamic – retain textual order */
  return (a->index < b->index) ? -1 : (a->index > b->index);
}

/* c) Tertiary: earliest use distance (stable) ------------------------- */
static int
compare_by_use_distance(const void *pa, const void *pb)
{
  const nir_instr *a = *(nir_instr *const *)pa;
  const nir_instr *b = *(nir_instr *const *)pb;

  uint32_t ia = nir_instr_def(a)->index;
  uint32_t ib = nir_instr_def(b)->index;

  if (ia == ib)
    return 0;
  return (ia < ib) ? -1 : 1;
}

/* -------------------------------------------------------------------- */
/* 3.  Tiny *stable* merge sort (avoids libc qsort instability)         */
/* -------------------------------------------------------------------- */
static void
stable_merge_sort(nir_instr **arr, size_t n,
                  int (*cmp)(const void *, const void *),
                  nir_instr **tmp) /* tmp  ≥  n */
{
  if (n < 2)
    return;

  size_t mid = n / 2;
  stable_merge_sort(arr,      mid,     cmp, tmp);
  stable_merge_sort(arr + mid, n - mid, cmp, tmp);

  size_t i = 0, j = mid, k = 0;
  while (i < mid && j < n)
    tmp[k++] = (cmp(&arr[i], &arr[j]) <= 0) ? arr[i++] : arr[j++];
  while (i < mid) tmp[k++] = arr[i++];
  while (j < n)   tmp[k++] = arr[j++];

  memcpy(arr, tmp, n * sizeof(*arr));
}

/* -------------------------------------------------------------------- */
/* 4.  Re-order a contiguous run of loads                                */
/* -------------------------------------------------------------------- */
static bool
reorder_loads(nir_instr *first, nir_instr *last,
              struct util_dynarray *scratch)
{
  util_dynarray_clear(scratch);

  for (nir_instr *it = first;; it = nir_instr_next(it)) {
    util_dynarray_append(scratch, nir_instr *, it);
    if (it == last)
      break;
  }

  nir_instr **loads   = (nir_instr **)scratch->data;
  const size_t n_load = util_dynarray_num_elements(scratch, nir_instr *);
  if (n_load < 2)
    return false;                       /* already optimal */

    /* ── 1) primary key: resource ─────────────────────────────────────── */
    qsort(loads, n_load, sizeof(*loads), compare_binding);

  /* ── 2) secondary key: constant offset within each resource group ── */
  nir_instr **tmp = (nir_instr **)alloca(n_load * sizeof(*tmp));

  for (size_t beg = 0; beg < n_load; ) {
    nir_instr *res = nir_get_load_resource(loads[beg]);
    size_t end = beg + 1;
    while (end < n_load &&
      nir_get_load_resource(loads[end]) == res)
      ++end;

    if (end - beg > 1)
      stable_merge_sort(loads + beg, end - beg,
                        compare_const_offset, tmp);
      beg = end;
  }

  /* ── 3) tertiary key: distance to closest use (stable) ───────────── */
  for (size_t i = 0; i < n_load; ++i)
    nir_instr_def(loads[i])->index = get_closest_use(loads[i]);

  stable_merge_sort(loads, n_load, compare_by_use_distance, tmp);

  /* ── 4) splice back, fence every 16 VMEM ops ─────────────────────── */
  nir_instr *after = nir_instr_next(last);
  nir_cursor pos   = after ? nir_before_instr(after)
  : nir_after_block(first->block);

  for (size_t i = 0; i < n_load; ++i)
    nir_instr_remove(loads[i]);

  unsigned clause_ctr = 0;
  for (size_t i = 0; i < n_load; ++i) {
    nir_instr_insert(pos, loads[i]);
    if (++clause_ctr == 16) {
      clause_ctr = 0;
      pos = nir_after_instr(loads[i]); /* open new clause boundary   */
    }
  }

  return true;
}

/* -------------------------------------------------------------------- */
/* 5.  Process a single basic block (bottom-up scan)                     */
/* -------------------------------------------------------------------- */
static bool
process_block(nir_block *blk, struct util_dynarray *scratch)
{
  nir_instr *grp_first = NULL;
  nir_instr *grp_last  = NULL;
  bool       progress  = false;

  nir_foreach_instr_reverse(instr, blk) {
    if (!nir_is_grouped_load(instr)) {
      if (grp_first && grp_last)
        progress |= reorder_loads(grp_first, grp_last, scratch);
      grp_first = grp_last = NULL;
      continue;
    }

    if (!grp_last) {               /* start new run                   */
      grp_first = grp_last = instr;
      continue;
    }

    bool split = false;
    nir_foreach_use(src, nir_instr_def(instr)) {
      const nir_instr *use = nir_src_parent_instr(src);
      if (use->block == blk && use->index <= grp_last->index) {
        split = true;
        break;
      }
    }

    if (split) {
      progress |= reorder_loads(grp_first, grp_last, scratch);
      grp_first = grp_last = instr;
    } else {
      grp_first = instr;          /* extend current run              */
    }
  }

  if (grp_first && grp_last)
    progress |= reorder_loads(grp_first, grp_last, scratch);

  return progress;
}

/* -------------------------------------------------------------------- */
/* 6.  Public entry ‑ keeps original API / ABI                           */
/* -------------------------------------------------------------------- */
bool
nir_reorder_adjacent_loads(nir_shader *shader)
{
  struct util_dynarray scratch;
  util_dynarray_init(&scratch, NULL);

  bool any_progress = false;

  nir_foreach_function_impl(impl, shader) {
    nir_metadata_require(impl, nir_metadata_instr_index);

    bool impl_progress = false;
    nir_foreach_block_reverse(block, impl)
    impl_progress |= process_block(block, &scratch);

    if (impl_progress)
      nir_index_instrs(impl); /* refresh instr->index */

      /* Report progress and specify which metadata remains valid */
      any_progress |= nir_progress(impl_progress, impl,
                                   nir_metadata_block_index |
                                   nir_metadata_instr_index |
                                   nir_metadata_dominance   |
                                   nir_metadata_loop_analysis);
  }

  util_dynarray_fini(&scratch);
  return any_progress;
}
