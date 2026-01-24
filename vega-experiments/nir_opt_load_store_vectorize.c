/*
 * Copyright © 2019 Valve Corporation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */
#include "util/u_dynarray.h"
#include "nir.h"
#include "nir_builder.h"
#include "nir_deref.h"
#include "nir_range_analysis.h"
#include "nir_worklist.h"

#include <stdlib.h>

#define XXH_INLINE_ALL
#include "util/xxhash.h"

struct intrinsic_info {
   nir_variable_mode mode;
   nir_intrinsic_op op;
   bool is_unvectorizable;
   int resource_src;
   int base_src;
   int deref_src;
   int value_src;
   unsigned offset_scale;
};

static const struct intrinsic_info *
get_info(nir_intrinsic_op op)
{
   switch (op) {
#define INFO(mode, op, unvectorizable, res, base, deref, val, scale)                                                     \
   case nir_intrinsic_##op: {                                                                                            \
      static const struct intrinsic_info op##_info = { mode, nir_intrinsic_##op, unvectorizable, res, base, deref, val, scale }; \
      return &op##_info;                                                                                                 \
   }
#define LOAD(mode, op, res, base, deref, scale)       INFO(mode, load_##op, false, res, base, deref, -1, scale)
#define STORE(mode, op, res, base, deref, val, scale) INFO(mode, store_##op, false, res, base, deref, val, scale)
#define ATOMIC(mode, type, res, base, deref, val, scale)         \
   INFO(mode, type##_atomic, true, res, base, deref, val, scale) \
   INFO(mode, type##_atomic_swap, true, res, base, deref, val, scale)

      LOAD(nir_var_mem_push_const, push_constant, -1, 0, -1, 1)
      LOAD(nir_var_mem_ubo, ubo, 0, 1, -1, 1)
      LOAD(nir_var_mem_ssbo, ssbo, 0, 1, -1, 1)
      STORE(nir_var_mem_ssbo, ssbo, 1, 2, -1, 0, 1)
      LOAD(0, deref, -1, -1, 0, 1)
      STORE(0, deref, -1, -1, 0, 1, 1)
      LOAD(nir_var_mem_shared, shared, -1, 0, -1, 1)
      STORE(nir_var_mem_shared, shared, -1, 1, -1, 0, 1)
      INFO(nir_var_mem_shared, load_shared2_amd, true, -1, 0, -1, -1, 1);
      INFO(nir_var_mem_shared, store_shared2_amd, true, -1, 1, -1, 0, 1)
      LOAD(nir_var_mem_global, global, -1, 0, -1, 1)
      STORE(nir_var_mem_global, global, -1, 1, -1, 0, 1)
      LOAD(nir_var_mem_global, global_constant, -1, 0, -1, 1)
      LOAD(nir_var_mem_task_payload, task_payload, -1, 0, -1, 1)
      STORE(nir_var_mem_task_payload, task_payload, -1, 1, -1, 0, 1)
      ATOMIC(nir_var_mem_ssbo, ssbo, 0, 1, -1, 2, 1)
      ATOMIC(0, deref, -1, -1, 0, 1, 1)
      ATOMIC(nir_var_mem_shared, shared, -1, 0, -1, 1, 1)
      ATOMIC(nir_var_mem_global, global, -1, 0, -1, 1, 1)
      ATOMIC(nir_var_mem_task_payload, task_payload, -1, 0, -1, 1, 1)
      LOAD(nir_var_shader_temp, stack, -1, -1, -1, 1)
      STORE(nir_var_shader_temp, stack, -1, -1, -1, 0, 1)
      LOAD(nir_var_shader_temp, scratch, -1, 0, -1, 1)
      STORE(nir_var_shader_temp, scratch, -1, 1, -1, 0, 1)
      LOAD(nir_var_mem_ubo, ubo_uniform_block_intel, 0, 1, -1, 1)
      LOAD(nir_var_mem_ssbo, ssbo_uniform_block_intel, 0, 1, -1, 1)
      LOAD(nir_var_mem_ssbo, ssbo_intel, 0, 1, -1, 1)
      STORE(nir_var_mem_ssbo, ssbo_intel, 1, 2, -1, 0, 1)
      LOAD(nir_var_mem_shared, shared_uniform_block_intel, -1, 0, -1, 1)
      LOAD(nir_var_mem_global, global_constant_uniform_block_intel, -1, 0, -1, 1)
      INFO(nir_var_mem_ubo, ldc_nv, false, 0, 1, -1, -1, 1)
      INFO(nir_var_mem_ubo, ldcx_nv, false, 0, 1, -1, -1, 1)
      LOAD(nir_var_uniform, const_ir3, -1, 0, -1, 4)
      STORE(nir_var_uniform, const_ir3, -1, -1, -1, 0, 4)
      INFO(nir_var_mem_shared, shared_append_amd, true, -1, -1, -1, -1, 1)
      INFO(nir_var_mem_shared, shared_consume_amd, true, -1, -1, -1, -1, 1)
      LOAD(0, buffer_amd, 0, 1, -1, 1)
      STORE(0, buffer_amd, 1, 2, -1, 0, 1)
   default:
      break;
#undef ATOMIC
#undef STORE
#undef LOAD
#undef INFO
   }
   return NULL;
}

struct offset_term {
   nir_scalar s;
   uint64_t mul;
   uint64_t add32;
};

#define MAX_INLINE_OFFSET_DEFS 4

struct entry_key {
   nir_def *resource;
   nir_variable *var;
   unsigned offset_def_count;
   nir_scalar inline_offset_defs[MAX_INLINE_OFFSET_DEFS];
   uint64_t inline_offset_defs_mul[MAX_INLINE_OFFSET_DEFS];
   uint8_t inline_offset_def_num_lsbz[MAX_INLINE_OFFSET_DEFS];
   nir_scalar *offset_defs;
   uint64_t *offset_defs_mul;
   uint8_t *offset_def_num_lsbz;
};

struct entry {
   struct list_head head;
   unsigned index;
   struct entry_key *key;
   union {
      uint64_t offset;
      int64_t offset_signed;
   };
   uint64_t total_add32;
   nir_instr *instr;
   nir_intrinsic_instr *intrin;
   const struct intrinsic_info *info;
   nir_deref_instr *deref;
   unsigned num_components;
   uint32_t align_mul;
   uint32_t align_offset;
   enum gl_access_qualifier access;
   bool is_store;
};

struct vectorize_ctx {
   nir_shader *shader;
   const nir_load_store_vectorize_options *options;
   struct hash_table *numlsb_ht;
   struct list_head entries[nir_num_variable_modes];
   struct hash_table *loads[nir_num_variable_modes];
   struct hash_table *stores[nir_num_variable_modes];
};

static unsigned
get_offset_scale(struct entry *entry)
{
   if (nir_intrinsic_has_offset_shift(entry->intrin)) {
      assert(entry->info->offset_scale == 1);
      return 1u << nir_intrinsic_offset_shift(entry->intrin);
   }
   return entry->info->offset_scale;
}

static uint32_t
hash_entry_key(const void *key_)
{
   const struct entry_key *key = (const struct entry_key *)key_;

   if (key->offset_def_count == 0) {
      uint32_t hash = (key->resource ? key->resource->index : 0u);
      hash = hash * 31u + (key->var ? key->var->index : 0u);
      hash = hash * 31u + (key->var ? (uint32_t)key->var->data.mode : 0u);
      return hash;
   }

   if (key->offset_def_count <= 2) {
      uint32_t hash = 2166136261u;
      hash ^= (key->resource ? key->resource->index : 0u);
      hash *= 16777619u;
      hash ^= (key->var ? key->var->index : 0u);
      hash *= 16777619u;
      hash ^= (key->var ? (uint32_t)key->var->data.mode : 0u);
      hash *= 16777619u;

      for (unsigned i = 0; i < key->offset_def_count; i++) {
         hash ^= key->offset_defs[i].def->index;
         hash *= 16777619u;
         hash ^= key->offset_defs[i].comp;
         hash *= 16777619u;
         hash ^= (uint32_t)key->offset_defs_mul[i];
         hash *= 16777619u;
         hash ^= (uint32_t)(key->offset_defs_mul[i] >> 32);
         hash *= 16777619u;
      }
      return hash;
   }

   if (key->offset_def_count <= MAX_INLINE_OFFSET_DEFS) {
      struct {
         uint32_t resource_index;
         uint32_t var_index;
         uint32_t var_mode;
         uint32_t offset_def_count;
         struct {
            uint32_t def_index;
            uint32_t comp;
            uint64_t mul;
         } defs[MAX_INLINE_OFFSET_DEFS];
      } hash_data;

      hash_data.resource_index = key->resource ? key->resource->index : 0u;
      hash_data.var_index = key->var ? key->var->index : 0u;
      hash_data.var_mode = key->var ? (uint32_t)key->var->data.mode : 0u;
      hash_data.offset_def_count = key->offset_def_count;

      for (unsigned i = 0; i < key->offset_def_count; i++) {
         hash_data.defs[i].def_index = key->offset_defs[i].def->index;
         hash_data.defs[i].comp = key->offset_defs[i].comp;
         hash_data.defs[i].mul = key->offset_defs_mul[i];
      }

      size_t total_size = 16u + key->offset_def_count * sizeof(hash_data.defs[0]);
      return XXH32(&hash_data, total_size, 0);
   }

   uint32_t hash = XXH32(&(uint32_t){key->resource ? key->resource->index : 0u}, 4, 0);
   hash = XXH32(&(uint32_t){key->var ? key->var->index : 0u}, 4, hash);
   hash = XXH32(&(uint32_t){key->var ? (uint32_t)key->var->data.mode : 0u}, 4, hash);
   hash = XXH32(&key->offset_def_count, 4, hash);

   for (unsigned i = 0; i < key->offset_def_count; i++) {
      hash = XXH32(&key->offset_defs[i].def->index, 4, hash);
      hash = XXH32(&key->offset_defs[i].comp, 4, hash);
      hash = XXH32(&key->offset_defs_mul[i], 8, hash);
   }

   return hash;
}

static bool
entry_key_equals(const void *a_, const void *b_)
{
   const struct entry_key *a = (const struct entry_key *)a_;
   const struct entry_key *b = (const struct entry_key *)b_;

   if (a->resource != b->resource || a->var != b->var ||
       a->offset_def_count != b->offset_def_count) {
      return false;
   }

   if (a->offset_def_count == 0) {
      return true;
   }

   if (a->offset_def_count <= 2) {
      for (unsigned i = 0; i < a->offset_def_count; i++) {
         if (!nir_scalar_equal(a->offset_defs[i], b->offset_defs[i]) ||
             a->offset_defs_mul[i] != b->offset_defs_mul[i]) {
            return false;
         }
      }
      return true;
   }

   for (unsigned i = 0; i < a->offset_def_count; i++) {
      if (!nir_scalar_equal(a->offset_defs[i], b->offset_defs[i])) {
         return false;
      }
   }

   return memcmp(a->offset_defs_mul, b->offset_defs_mul,
                 a->offset_def_count * sizeof(uint64_t)) == 0;
}

static void
delete_entry_dynarray(struct hash_entry *entry)
{
   struct util_dynarray *arr = (struct util_dynarray *)entry->data;
   ralloc_free(arr);
}

static int64_t
get_offset_diff(struct entry *a, struct entry *b)
{
   assert(entry_key_equals(a->key, b->key));

   int64_t diff = b->offset_signed - a->offset_signed;
   int64_t add_diff = (int64_t)(b->total_add32 - a->total_add32);

   if ((diff > 0 && add_diff > INT64_MAX - diff) ||
       (diff < 0 && add_diff < INT64_MIN - diff)) {
      return 0;
   }

   return diff + add_diff;
}

static int
sort_entries(const void *a_, const void *b_)
{
   struct entry *a = *(struct entry *const *)a_;
   struct entry *b = *(struct entry *const *)b_;

   int64_t diff = get_offset_diff(b, a);
   if (diff > 0) {
      return 1;
   } else if (diff < 0) {
      return -1;
   }

   if (a->index > b->index) {
      return 1;
   } else if (a->index < b->index) {
      return -1;
   }

   return 0;
}

static void
sort_entries_specialized(struct entry **entries, unsigned count)
{
   if (count <= 1) {
      return;
   }

   if (count <= 16) {
      for (unsigned i = 1; i < count; i++) {
         struct entry *key = entries[i];
         int j = (int)i - 1;

         while (j >= 0 && sort_entries(&entries[j], &key) > 0) {
            entries[j + 1] = entries[j];
            j--;
         }
         entries[j + 1] = key;
      }
      return;
   }

   qsort(entries, count, sizeof(struct entry *), &sort_entries);
}

static unsigned
get_bit_size(struct entry *entry)
{
   unsigned size = entry->info->value_src >= 0
                      ? entry->intrin->src[entry->info->value_src].ssa->bit_size
                      : entry->intrin->def.bit_size;
   return size == 1 ? 32u : size;
}

static unsigned
get_write_mask(const nir_intrinsic_instr *intrin)
{
   if (nir_intrinsic_has_write_mask(intrin)) {
      return nir_intrinsic_write_mask(intrin);
   }

   const struct intrinsic_info *info = get_info(intrin->intrinsic);
   assert(info->value_src >= 0);
   return nir_component_mask(intrin->src[info->value_src].ssa->num_components);
}

static nir_op
get_effective_alu_op(nir_scalar scalar)
{
   nir_op op = nir_scalar_alu_op(scalar);

   if (op == nir_op_amul) {
      return nir_op_imul;
   } else {
      return op;
   }
}

static bool
parse_alu(nir_scalar *def, nir_op op, uint64_t *c, bool require_nuw)
{
   if (!nir_scalar_is_alu(*def) || get_effective_alu_op(*def) != op) {
      return false;
   }

   if (require_nuw && !nir_def_as_alu(def->def)->no_unsigned_wrap) {
      return false;
   }

   nir_scalar src0 = nir_scalar_chase_alu_src(*def, 0);
   nir_scalar src1 = nir_scalar_chase_alu_src(*def, 1);
   if (op != nir_op_ishl && nir_scalar_is_const(src0)) {
      *c = nir_scalar_as_uint(src0);
      *def = src1;
   } else if (nir_scalar_is_const(src1)) {
      *c = nir_scalar_as_uint(src1);
      *def = src0;
   } else {
      return false;
   }
   return true;
}

static struct offset_term
parse_offset(nir_scalar base, uint64_t *offset)
{
   struct offset_term term;
   term.s = nir_get_scalar(NULL, 0);
   term.mul = 0;
   term.add32 = 0;

   if (nir_scalar_is_const(base)) {
      *offset = nir_scalar_as_uint(base);
      return term;
   }

   uint64_t mul = 1;
   uint64_t add = 0;
   bool require_nuw = false;
   uint64_t uub = u_uintN_max(base.def->bit_size);

   for (unsigned depth = 0; depth < 32; depth++) {
      uint64_t mul2 = 1;
      uint64_t add2 = 0;
      bool made_progress = false;

      if (parse_alu(&base, nir_op_imul, &mul2, require_nuw)) {
         uub = mul2 ? uub / mul2 : 0;
         mul *= mul2;
         made_progress = true;
      } else if (parse_alu(&base, nir_op_ishl, &mul2, require_nuw)) {
         mul2 &= (base.def->bit_size - 1);
         uub >>= mul2;
         mul <<= mul2;
         made_progress = true;
      } else if (parse_alu(&base, nir_op_iadd, &add2, require_nuw)) {
         uub = u_uintN_max(base.def->bit_size);
         add += add2 * mul;
         made_progress = true;
      } else if (nir_scalar_is_alu(base)) {
         nir_op op = nir_scalar_alu_op(base);
         if (op == nir_op_mov) {
            base = nir_scalar_chase_alu_src(base, 0);
            made_progress = true;
         } else if (op == nir_op_u2u64) {
            base = nir_scalar_chase_alu_src(base, 0);
            require_nuw = true;
            uub = u_uintN_max(base.def->bit_size);
            made_progress = true;
         }
      }

      if (!made_progress) {
         break;
      }
   }

   nir_scalar base32 = base;
   uint64_t add32 = 0;
   if (require_nuw && parse_alu(&base32, nir_op_iadd, &add32, false)) {
      uint32_t uint_max = u_uintN_max(base32.def->bit_size);
      uint32_t ovfl_start = uint_max - (uint32_t)add32;
      uint32_t noovfl_end = add32 <= uub ? (uint32_t)(uub - add32) : 0;
      uint32_t mid = ((uint64_t)uint_max + 1) / 2u;
      if (ovfl_start >= (mid - 1) && noovfl_end < mid) {
         base = base32;
      } else {
         add32 = 0;
      }
   }

   if (base.def && nir_def_is_intrinsic(base.def)) {
      nir_intrinsic_instr *intrin = nir_def_as_intrinsic(base.def);
      if (intrin->intrinsic == nir_intrinsic_load_vulkan_descriptor) {
         base.def = NULL;
      }
   }

   *offset = add;
   term.s = base;
   term.mul = mul;
   term.add32 = add32;
   return term;
}

static unsigned
type_scalar_size_bytes(const struct glsl_type *type)
{
   assert(glsl_type_is_vector_or_scalar(type) ||
          glsl_type_is_matrix(type));
   return glsl_type_is_boolean(type) ? 4u : glsl_get_bit_size(type) / 8u;
}

static bool
cmp_term(struct offset_term a, struct offset_term b)
{
   if (a.s.def != b.s.def) {
      return a.s.def->index > b.s.def->index;
   }

   if (a.s.comp != b.s.comp) {
      return a.s.comp > b.s.comp;
   }

   return a.add32 > b.add32;
}

static unsigned
add_to_entry_key(struct offset_term *terms, unsigned count, struct offset_term term)
{
   for (unsigned i = 0; i <= count; i++) {
      if (i == count || cmp_term(term, terms[i])) {
         memmove(terms + i + 1, terms + i,
                 (count - i) * sizeof(struct offset_term));
         terms[i] = term;
         return 1;
      } else if (nir_scalar_equal(term.s, terms[i].s) && !terms[i].add32 && !term.add32) {
         terms[i].mul += term.mul;
         return 0;
      }
   }
   UNREACHABLE("Unreachable.");
   return 0;
}

static void
fill_in_offset_defs(struct vectorize_ctx *ctx, struct entry *entry,
                    unsigned count, struct offset_term *terms)
{
   struct entry_key *key = entry->key;
   key->offset_def_count = count;

   if (count <= MAX_INLINE_OFFSET_DEFS) {
      key->offset_defs = key->inline_offset_defs;
      key->offset_defs_mul = key->inline_offset_defs_mul;
      key->offset_def_num_lsbz = key->inline_offset_def_num_lsbz;
   } else {
      key->offset_defs = ralloc_array(entry, nir_scalar, count);
      key->offset_defs_mul = ralloc_array(entry, uint64_t, count);
      key->offset_def_num_lsbz = ralloc_array(entry, uint8_t, count);
      if (unlikely(!key->offset_defs || !key->offset_defs_mul || !key->offset_def_num_lsbz)) {
         key->offset_def_count = 0;
         return;
      }
   }

   for (unsigned i = 0; i < count; i++) {
      key->offset_defs[i] = terms[i].s;
      key->offset_defs_mul[i] = terms[i].mul;

      unsigned lsb_zero = nir_def_num_lsb_zero(ctx->numlsb_ht, terms[i].s);
      if (terms[i].add32) {
         lsb_zero = MIN2(lsb_zero, ffsll(terms[i].add32) - 1);
      }
      key->offset_def_num_lsbz[i] = (uint8_t)lsb_zero;

      entry->total_add32 += terms[i].add32 * terms[i].mul;
   }
}

static void
create_entry_key_from_deref(struct vectorize_ctx *ctx, struct entry *entry,
                            nir_deref_path *path)
{
   unsigned path_len = 0;
   while (path->path[path_len]) {
      path_len++;
   }

   struct offset_term term_stack[32];
   struct offset_term *terms = term_stack;
   if (path_len > 32) {
      terms = ralloc_array(ctx, struct offset_term, path_len);
      if (unlikely(terms == NULL)) {
         terms = term_stack;
      }
   }
   unsigned term_count = 0;

   struct entry_key *key = ralloc(entry, struct entry_key);
   if (unlikely(key == NULL)) {
      if (terms != term_stack) {
         ralloc_free(terms);
      }
      return;
   }
   key->resource = NULL;
   key->var = NULL;

   for (unsigned i = 0; i < path_len; i++) {
      nir_deref_instr *parent = i ? path->path[i - 1] : NULL;
      nir_deref_instr *deref = path->path[i];

      switch (deref->deref_type) {
      case nir_deref_type_var: {
         assert(!parent);
         key->var = deref->var;
         break;
      }
      case nir_deref_type_array:
      case nir_deref_type_ptr_as_array: {
         assert(parent);
         nir_def *index = deref->arr.index.ssa;
         uint32_t stride = nir_deref_instr_array_stride(deref);

         uint64_t offset = 0;
         struct offset_term term = parse_offset(nir_get_scalar(index, 0), &offset);
         offset = util_mask_sign_extend(offset, index->bit_size);

         entry->offset += (int64_t)offset * (int64_t)stride;
         if (term.s.def) {
            term.mul *= stride;
            term.mul = util_mask_sign_extend(term.mul, index->bit_size);
            term_count += add_to_entry_key(terms, term_count, term);
         }
         break;
      }
      case nir_deref_type_struct: {
         assert(parent);
         int offset = glsl_get_struct_field_offset(parent->type, deref->strct.index);
         entry->offset += offset;
         break;
      }
      case nir_deref_type_cast: {
         if (!parent) {
            key->resource = deref->parent.ssa;
         }
         break;
      }
      default:
         UNREACHABLE("Unhandled deref type");
      }
   }

   entry->key = key;
   fill_in_offset_defs(ctx, entry, term_count, terms);

   if (terms != term_stack) {
      ralloc_free(terms);
   }
}

static unsigned
parse_entry_key_from_offset(struct offset_term *terms, unsigned size, unsigned left,
                            nir_scalar base, uint64_t base_mul, uint64_t *offset)
{
   uint64_t new_offset;
   struct offset_term term = parse_offset(base, &new_offset);
   *offset += new_offset * base_mul;

   if (!term.s.def) {
      return 0;
   }

   term.mul *= base_mul;

   assert(left >= 1);

   if (left >= 2 && base.def->bit_size == term.s.def->bit_size) {
      if (nir_scalar_is_alu(term.s) && nir_scalar_alu_op(term.s) == nir_op_iadd) {
         nir_scalar src0 = nir_scalar_chase_alu_src(term.s, 0);
         nir_scalar src1 = nir_scalar_chase_alu_src(term.s, 1);
         unsigned amount = parse_entry_key_from_offset(terms, size, left - 1, src0, term.mul, offset);
         amount += parse_entry_key_from_offset(terms, size + amount, left - amount, src1, term.mul, offset);
         return amount;
      }
   }

   term.mul = util_mask_sign_extend(term.mul, base.def->bit_size);
   return add_to_entry_key(terms, size, term);
}

static void
create_entry_key_from_offset(struct vectorize_ctx *ctx, struct entry *entry,
                             nir_def *base, uint64_t base_mul)
{
   struct entry_key *key = ralloc(entry, struct entry_key);
   if (unlikely(key == NULL)) {
      return;
   }
   key->resource = NULL;
   key->var = NULL;
   key->offset_defs = NULL;
   key->offset_defs_mul = NULL;
   key->offset_def_count = 0;
   entry->key = key;

   struct offset_term terms[32];
   if (base) {
      nir_scalar scalar = { .def = base, .comp = 0 };
      uint64_t offset = 0;
      key->offset_def_count = parse_entry_key_from_offset(terms, 0, 32, scalar, base_mul, &offset);
      entry->offset += (int64_t)offset;
   }

   if (!key->offset_def_count) {
      return;
   }

   fill_in_offset_defs(ctx, entry, key->offset_def_count, terms);
}

static nir_variable_mode
get_variable_mode(struct entry *entry)
{
   if (nir_intrinsic_has_memory_modes(entry->intrin)) {
      return nir_intrinsic_memory_modes(entry->intrin);
   } else if (entry->info->mode) {
      return entry->info->mode;
   }
   assert(entry->deref && util_bitcount(entry->deref->modes) == 1);
   return entry->deref->modes;
}

static unsigned
mode_to_index(nir_variable_mode mode)
{
   assert(util_bitcount(mode) == 1);

   if (mode == nir_var_mem_global) {
      mode = nir_var_mem_ssbo;
   }

   return ffs(mode) - 1;
}

static nir_variable_mode
aliasing_modes(nir_variable_mode modes)
{
   if (modes & (nir_var_mem_ssbo | nir_var_mem_global)) {
      modes |= nir_var_mem_ssbo | nir_var_mem_global;
   }
   return modes;
}

static void
calc_alignment(struct entry *entry)
{
   if (entry->intrin->intrinsic == nir_intrinsic_load_buffer_amd ||
       entry->intrin->intrinsic == nir_intrinsic_store_buffer_amd) {
      entry->align_mul = nir_intrinsic_align_mul(entry->intrin);
      entry->align_offset = nir_intrinsic_align_offset(entry->intrin);
      return;
   }

   uint32_t align_mul = 31;
   for (unsigned i = 0; i < entry->key->offset_def_count; i++) {
      unsigned lsb_zero = entry->key->offset_def_num_lsbz[i];
      if (lsb_zero == 64) {
         continue;
      }
      uint64_t stride = entry->key->offset_defs_mul[i] << lsb_zero;
      if (stride) {
         align_mul = MIN2(align_mul, ffsll(stride));
      }
   }

   entry->align_mul = 1u << (align_mul - 1);

   if (entry->align_mul == 0) {
      entry->align_mul = 1;
      entry->align_offset = 0;
      return;
   }

   bool has_align = nir_intrinsic_infos[entry->intrin->intrinsic].index_map[NIR_INTRINSIC_ALIGN_MUL];
   if (!has_align || entry->align_mul >= nir_intrinsic_align_mul(entry->intrin)) {
      entry->align_offset = (uint32_t)(entry->offset % entry->align_mul);
   } else {
      entry->align_mul = nir_intrinsic_align_mul(entry->intrin);
      entry->align_offset = nir_intrinsic_align_offset(entry->intrin);
   }
}

static struct entry *
create_entry(void *mem_ctx, struct vectorize_ctx *ctx,
             const struct intrinsic_info *info,
             nir_intrinsic_instr *intrin)
{
   bool is_shared2 = intrin->intrinsic == nir_intrinsic_load_shared2_amd ||
                     intrin->intrinsic == nir_intrinsic_store_shared2_amd;

   bool is_shared_append = intrin->intrinsic == nir_intrinsic_shared_append_amd ||
                           intrin->intrinsic == nir_intrinsic_shared_consume_amd;

   struct entry *entry = rzalloc(mem_ctx, struct entry);
   if (unlikely(entry == NULL)) {
      return NULL;
   }
   entry->intrin = intrin;
   entry->instr = &intrin->instr;
   entry->info = info;
   entry->is_store = entry->info->value_src >= 0 || is_shared_append;

   entry->num_components =
      entry->is_store ? intrin->num_components : nir_def_last_component_read(&intrin->def) + 1;
   if (entry->num_components == 0 || is_shared2) {
      assert(entry->info->is_unvectorizable || !entry->is_store);
      entry->num_components = 1;
   }

   if (entry->info->deref_src >= 0) {
      entry->deref = nir_src_as_deref(intrin->src[entry->info->deref_src]);
      nir_deref_path path;
      nir_deref_path_init(&path, entry->deref, NULL);
      create_entry_key_from_deref(ctx, entry, &path);
      nir_deref_path_finish(&path);
   } else {
      nir_def *base = entry->info->base_src >= 0 ? intrin->src[entry->info->base_src].ssa : NULL;
      unsigned offset_scale = get_offset_scale(entry);
      if (nir_intrinsic_has_base(intrin)) {
         entry->offset = (int64_t)nir_intrinsic_base(intrin) * (int64_t)offset_scale;
      }
      create_entry_key_from_offset(ctx, entry, base, offset_scale);

      if (base) {
         entry->offset = util_mask_sign_extend(entry->offset, base->bit_size);
      }
   }

   if (!entry->key) {
      ralloc_free(entry);
      return NULL;
   }

   if (entry->info->resource_src >= 0) {
      entry->key->resource = intrin->src[entry->info->resource_src].ssa;
   }

   if (nir_intrinsic_has_access(intrin)) {
      entry->access = nir_intrinsic_access(intrin);
   } else if (entry->key->var) {
      entry->access = entry->key->var->data.access;
   }

   if (nir_intrinsic_can_reorder(intrin)) {
      entry->access |= ACCESS_CAN_REORDER;
   }

   uint32_t restrict_modes = nir_var_shader_in | nir_var_shader_out;
   restrict_modes |= nir_var_shader_temp | nir_var_function_temp;
   restrict_modes |= nir_var_uniform | nir_var_mem_push_const;
   restrict_modes |= nir_var_system_value | nir_var_mem_shared;
   restrict_modes |= nir_var_mem_task_payload;
   if (get_variable_mode(entry) & restrict_modes) {
      entry->access |= ACCESS_RESTRICT;
   }

   calc_alignment(entry);

   return entry;
}

static nir_deref_instr *
cast_deref(nir_builder *b, unsigned num_components, unsigned bit_size, nir_deref_instr *deref)
{
   if (glsl_get_components(deref->type) == num_components &&
       type_scalar_size_bytes(deref->type) * 8u == bit_size) {
      return deref;
   }

   enum glsl_base_type types[] = {
      GLSL_TYPE_UINT8, GLSL_TYPE_UINT16, GLSL_TYPE_UINT, GLSL_TYPE_UINT64
   };
   enum glsl_base_type base = types[ffs(bit_size / 8u) - 1u];
   const struct glsl_type *type = glsl_vector_type(base, num_components);

   if (deref->type == type) {
      return deref;
   }

   return nir_build_deref_cast(b, &deref->def, deref->modes, type, 0);
}

static bool
new_bitsize_acceptable(struct vectorize_ctx *ctx, unsigned new_bit_size,
                       struct entry *low, struct entry *high, unsigned size)
{
   if (size % new_bit_size != 0) {
      return false;
   }

   unsigned new_num_components = size / new_bit_size;

   if (low->is_store) {
      if (!nir_num_components_valid(new_num_components)) {
         return false;
      }
   } else {
      if (new_num_components > NIR_MAX_VEC_COMPONENTS) {
         return false;
      }
   }

   int64_t high_offset_signed = get_offset_diff(low, high);
   if (high_offset_signed < 0 || high_offset_signed > UINT32_MAX) {
      return false;
   }
   unsigned high_offset = (unsigned)high_offset_signed;

   if (high_offset % (new_bit_size / 8) != 0) {
      return false;
   }

   unsigned common_bit_size = MIN2(get_bit_size(low), get_bit_size(high));
   common_bit_size = MIN2(common_bit_size, new_bit_size);
   if (high_offset > 0) {
      common_bit_size = MIN2(common_bit_size, (1u << (ffs(high_offset * 8) - 1)));
   }
   if (new_bit_size / common_bit_size > NIR_MAX_VEC_COMPONENTS) {
      return false;
   }

   unsigned low_size = low->intrin->num_components * get_bit_size(low) / 8;
   int64_t hole_size = (int64_t)high_offset - (int64_t)low_size;

   if (!ctx->options->callback(low->align_mul,
                               low->align_offset,
                               new_bit_size, new_num_components, hole_size,
                               low->intrin, high->intrin,
                               ctx->options->cb_data)) {
      return false;
   }

   if (low->is_store) {
      unsigned low_size_bits = low->num_components * get_bit_size(low);
      unsigned high_size_bits = high->num_components * get_bit_size(high);

      if (low_size_bits % new_bit_size != 0) {
         return false;
      }
      if (high_size_bits % new_bit_size != 0) {
         return false;
      }

      unsigned write_mask = get_write_mask(low->intrin);
      if (!nir_component_mask_can_reinterpret(write_mask, get_bit_size(low), new_bit_size)) {
         return false;
      }

      write_mask = get_write_mask(high->intrin);
      if (!nir_component_mask_can_reinterpret(write_mask, get_bit_size(high), new_bit_size)) {
         return false;
      }
   }

   return true;
}

static nir_deref_instr *
subtract_deref(nir_builder *b, nir_deref_instr *deref, int64_t offset)
{
   if (deref->deref_type == nir_deref_type_ptr_as_array &&
       nir_src_is_const(deref->arr.index) &&
       offset % nir_deref_instr_array_stride(deref) == 0) {
      unsigned stride = nir_deref_instr_array_stride(deref);
      assert(stride != 0);
      nir_def *index = nir_imm_intN_t(b, nir_src_as_int(deref->arr.index) - offset / (int64_t)stride,
                                      deref->def.bit_size);
      return nir_build_deref_ptr_as_array(b, nir_deref_instr_parent(deref), index);
   }

   if (deref->deref_type == nir_deref_type_array &&
       nir_src_is_const(deref->arr.index)) {
      nir_deref_instr *parent = nir_deref_instr_parent(deref);
      unsigned stride = nir_deref_instr_array_stride(deref);
      assert(stride != 0);
      if (offset % (int64_t)stride == 0) {
         return nir_build_deref_array_imm(
            b, parent, nir_src_as_int(deref->arr.index) - offset / (int64_t)stride);
      }
   }

   deref = nir_build_deref_cast(b, &deref->def, deref->modes,
                                glsl_scalar_type(GLSL_TYPE_UINT8), 1);
   return nir_build_deref_ptr_as_array(
      b, deref, nir_imm_intN_t(b, -offset, deref->def.bit_size));
}

static void
hoist_base_addr(nir_instr *instr, nir_instr *to_hoist)
{
   if (to_hoist->block != instr->block || to_hoist->index <= instr->index) {
      return;
   }

   assert(to_hoist->type == nir_instr_type_load_const ||
          to_hoist->type == nir_instr_type_alu);

   if (to_hoist->type == nir_instr_type_alu) {
      nir_alu_instr *alu = nir_instr_as_alu(to_hoist);
      for (unsigned i = 0; i < nir_op_infos[alu->op].num_inputs; i++) {
         hoist_base_addr(instr, nir_def_instr(alu->src[i].src.ssa));
      }
   }

   nir_instr_move(nir_before_instr(instr), to_hoist);
   to_hoist->index = instr->index;
   return;
}

static void
vectorize_loads(nir_builder *b, struct vectorize_ctx *ctx,
                struct entry *low, struct entry *high,
                struct entry *first, struct entry *second,
                unsigned new_bit_size, unsigned new_num_components,
                unsigned high_start)
{
   unsigned old_low_bit_size = get_bit_size(low);
   unsigned old_high_bit_size = get_bit_size(high);
   unsigned old_low_num_components = low->intrin->num_components;
   unsigned old_high_num_components = high->intrin->num_components;
   bool low_bool = low->intrin->def.bit_size == 1;
   bool high_bool = high->intrin->def.bit_size == 1;
   nir_def *data = &first->intrin->def;

   b->cursor = nir_after_instr(first->instr);

   new_num_components = nir_round_up_components(new_num_components);
   new_num_components = MAX2(new_num_components, 1);

   data->num_components = new_num_components;
   data->bit_size = new_bit_size;

   nir_def *low_undef = nir_undef(b, old_low_num_components, old_low_bit_size);
   nir_def *high_undef = nir_undef(b, old_high_num_components, old_high_bit_size);

   nir_def *low_def = nir_extract_bits(
      b, (nir_def *[]){ data, low_undef }, 2, 0, old_low_num_components,
      old_low_bit_size);

   nir_def *high_def = nir_extract_bits(
      b, (nir_def *[]){ data, high_undef }, 2, high_start,
      old_high_num_components, old_high_bit_size);

   low_def = low_bool ? nir_i2b(b, low_def) : nir_mov(b, low_def);
   high_def = high_bool ? nir_i2b(b, high_def) : nir_mov(b, high_def);

   if (first == low) {
      nir_def_rewrite_uses_after_instr(&low->intrin->def, low_def,
                                       nir_def_instr(high_def));
      nir_def_rewrite_uses(&high->intrin->def, high_def);
   } else {
      nir_def_rewrite_uses(&low->intrin->def, low_def);
      nir_def_rewrite_uses_after(&high->intrin->def, high_def);
   }

   first->intrin->num_components = new_num_components;
   first->num_components = nir_def_last_component_read(data) + 1;

   const struct intrinsic_info *info = first->info;

   if (first != low && info->base_src >= 0) {
      b->cursor = nir_before_instr(first->instr);

      nir_def *base = low->intrin->src[info->base_src].ssa;
      hoist_base_addr(first->instr, nir_def_instr(base));
      nir_src_rewrite(&first->intrin->src[info->base_src], base);

      if (nir_intrinsic_has_offset_shift(first->intrin)) {
         nir_intrinsic_set_offset_shift(first->intrin,
                                        nir_intrinsic_offset_shift(low->intrin));
      }
   }

   if (info->deref_src >= 0) {
      b->cursor = nir_before_instr(first->instr);

      nir_deref_instr *deref = nir_src_as_deref(first->intrin->src[info->deref_src]);
      if (first != low && high_start != 0) {
         deref = subtract_deref(b, deref, (int64_t)high_start / 8 / (int64_t)get_offset_scale(first));
      }
      first->deref = cast_deref(b, new_num_components, new_bit_size, deref);

      nir_src_rewrite(&first->intrin->src[info->deref_src],
                      &first->deref->def);
   }

   if (nir_intrinsic_has_range_base(first->intrin)) {
      uint32_t old_low_range_base = nir_intrinsic_range_base(low->intrin);
      uint32_t old_high_range_base = nir_intrinsic_range_base(high->intrin);
      uint32_t old_low_range_end = old_low_range_base + nir_intrinsic_range(low->intrin);
      uint32_t old_high_range_end = old_high_range_base + nir_intrinsic_range(high->intrin);

      uint32_t old_low_size = old_low_num_components * old_low_bit_size / 8;
      uint32_t old_high_size = old_high_num_components * old_high_bit_size / 8;
      uint32_t old_combined_size_up_to_high = high_start / 8u + old_high_size;
      uint32_t old_combined_size = MAX2(old_low_size, old_combined_size_up_to_high);
      uint32_t old_combined_range = MAX2(old_low_range_end, old_high_range_end) -
                                    old_low_range_base;
      uint32_t new_size = new_num_components * new_bit_size / 8;

      int size_change = (int)new_size - (int)old_combined_size;
      uint32_t range = (uint32_t)((int)old_combined_range + size_change);
      assert(range);

      nir_intrinsic_set_range_base(first->intrin, old_low_range_base);
      nir_intrinsic_set_range(first->intrin, range);
   } else if (nir_intrinsic_has_base(first->intrin) && info->deref_src == -1) {
      nir_intrinsic_set_base(first->intrin, nir_intrinsic_base(low->intrin));
   }

   first->key = low->key;
   first->offset = low->offset;
   first->total_add32 = low->total_add32;

   first->align_mul = low->align_mul;
   first->align_offset = low->align_offset;

   nir_instr_remove(second->instr);
}

static void
vectorize_stores(nir_builder *b, struct vectorize_ctx *ctx,
                 struct entry *low, struct entry *high,
                 struct entry *first, struct entry *second,
                 unsigned new_bit_size, unsigned new_num_components,
                 unsigned high_start)
{
   ASSERTED unsigned low_size = low->num_components * get_bit_size(low);
   assert(low_size % new_bit_size == 0);

   b->cursor = nir_before_instr(second->instr);

   uint32_t low_write_mask = get_write_mask(low->intrin);
   uint32_t high_write_mask = get_write_mask(high->intrin);
   low_write_mask = nir_component_mask_reinterpret(low_write_mask,
                                                   get_bit_size(low),
                                                   new_bit_size);
   high_write_mask = nir_component_mask_reinterpret(high_write_mask,
                                                    get_bit_size(high),
                                                    new_bit_size);
   high_write_mask <<= high_start / new_bit_size;

   uint32_t write_mask = low_write_mask | high_write_mask;

   nir_def *low_val = low->intrin->src[low->info->value_src].ssa;
   nir_def *high_val = high->intrin->src[high->info->value_src].ssa;
   low_val = low_val->bit_size == 1 ? nir_b2iN(b, low_val, 32) : low_val;
   high_val = high_val->bit_size == 1 ? nir_b2iN(b, high_val, 32) : high_val;

   nir_def *data_channels[NIR_MAX_VEC_COMPONENTS];
   for (unsigned i = 0; i < new_num_components; i++) {
      bool set_low = low_write_mask & (1 << i);
      bool set_high = high_write_mask & (1 << i);

      if (set_low && (!set_high || low == second)) {
         unsigned offset = i * new_bit_size;
         data_channels[i] = nir_extract_bits(b, &low_val, 1, offset, 1, new_bit_size);
      } else if (set_high) {
         assert(!set_low || high == second);
         unsigned offset = i * new_bit_size - high_start;
         data_channels[i] = nir_extract_bits(b, &high_val, 1, offset, 1, new_bit_size);
      } else {
         data_channels[i] = nir_undef(b, 1, new_bit_size);
      }
   }
   nir_def *data = nir_vec(b, data_channels, new_num_components);

   if (nir_intrinsic_has_write_mask(second->intrin)) {
      nir_intrinsic_set_write_mask(second->intrin, write_mask);
   }
   second->intrin->num_components = data->num_components;
   second->num_components = data->num_components;

   const struct intrinsic_info *info = second->info;
   assert(info->value_src >= 0);
   nir_src_rewrite(&second->intrin->src[info->value_src], data);

   if (second != low && info->base_src >= 0) {
      nir_src_rewrite(&second->intrin->src[info->base_src],
                      low->intrin->src[info->base_src].ssa);
   }

   if (info->deref_src >= 0) {
      b->cursor = nir_before_instr(second->instr);
      second->deref = cast_deref(b, new_num_components, new_bit_size,
                                 nir_src_as_deref(low->intrin->src[info->deref_src]));
      nir_src_rewrite(&second->intrin->src[info->deref_src],
                      &second->deref->def);
   }

   if (second != low && nir_intrinsic_has_base(second->intrin)) {
      nir_intrinsic_set_base(second->intrin, nir_intrinsic_base(low->intrin));
   }

   if (second != low && nir_intrinsic_has_offset_shift(second->intrin)) {
      nir_intrinsic_set_offset_shift(second->intrin,
                                     nir_intrinsic_offset_shift(low->intrin));
   }

   second->key = low->key;
   second->offset = low->offset;
   second->total_add32 = low->total_add32;

   second->align_mul = low->align_mul;
   second->align_offset = low->align_offset;

   list_del(&first->head);
   nir_instr_remove(first->instr);
}

static bool
bindings_different_restrict(nir_shader *shader, struct entry *a, struct entry *b)
{
   bool different_bindings = false;
   nir_variable *a_var = NULL, *b_var = NULL;
   if (a->key->resource && b->key->resource) {
      nir_binding a_res = nir_chase_binding(nir_src_for_ssa(a->key->resource));
      nir_binding b_res = nir_chase_binding(nir_src_for_ssa(b->key->resource));
      if (!a_res.success || !b_res.success) {
         return false;
      }

      if (a_res.num_indices != b_res.num_indices ||
          a_res.desc_set != b_res.desc_set ||
          a_res.binding != b_res.binding) {
         different_bindings = true;
      }

      for (unsigned i = 0; i < a_res.num_indices; i++) {
         if (nir_src_is_const(a_res.indices[i]) && nir_src_is_const(b_res.indices[i]) &&
             nir_src_as_uint(a_res.indices[i]) != nir_src_as_uint(b_res.indices[i])) {
            different_bindings = true;
         }
      }

      if (different_bindings) {
         a_var = nir_get_binding_variable(shader, a_res);
         b_var = nir_get_binding_variable(shader, b_res);
      }
   } else if (a->key->var && b->key->var) {
      a_var = a->key->var;
      b_var = b->key->var;
      different_bindings = a_var != b_var;
   } else if (!!a->key->resource != !!b->key->resource) {
      different_bindings = true;

      if (a->key->resource) {
         nir_binding a_res = nir_chase_binding(nir_src_for_ssa(a->key->resource));
         a_var = nir_get_binding_variable(shader, a_res);
      }

      if (b->key->resource) {
         nir_binding b_res = nir_chase_binding(nir_src_for_ssa(b->key->resource));
         b_var = nir_get_binding_variable(shader, b_res);
      }
   } else {
      return false;
   }

   unsigned a_access = a->access | (a_var ? a_var->data.access : 0);
   unsigned b_access = b->access | (b_var ? b_var->data.access : 0);

   return different_bindings &&
          ((a_access | b_access) & ACCESS_RESTRICT);
}

static bool
may_alias_internal(struct entry *a, struct entry *b, uint32_t a_offset, uint32_t b_offset)
{
   if (!entry_key_equals(a->key, b->key)) {
      return true;
   }

   int64_t diff = get_offset_diff(a, b) + (int64_t)b_offset - (int64_t)a_offset;

   struct entry *first = diff < 0 ? b : a;
   unsigned size = get_bit_size(first) / 8u * first->num_components;
   return llabs(diff) < (int64_t)size;
}

static unsigned
parse_shared2_offsets(struct entry *entry, uint32_t offsets[2])
{
   if (entry->intrin->intrinsic != nir_intrinsic_load_shared2_amd &&
       entry->intrin->intrinsic != nir_intrinsic_store_shared2_amd) {
      offsets[0] = 0;
      return 1;
   }

   uint32_t stride = get_bit_size(entry) / 8u;
   if (nir_intrinsic_st64(entry->intrin)) {
      stride *= 64;
   }
   offsets[0] = nir_intrinsic_offset0(entry->intrin) * stride;
   offsets[1] = nir_intrinsic_offset1(entry->intrin) * stride;
   return 2;
}

static bool
may_alias(nir_shader *shader, struct entry *a, struct entry *b)
{
   assert(mode_to_index(get_variable_mode(a)) ==
          mode_to_index(get_variable_mode(b)));

   if ((a->access | b->access) & ACCESS_CAN_REORDER) {
      return false;
   }

   if (bindings_different_restrict(shader, a, b)) {
      return false;
   }

   if (a->key->var != b->key->var || a->key->resource != b->key->resource) {
      return true;
   }

   bool is_a_buffer_amd = a->intrin->intrinsic == nir_intrinsic_load_buffer_amd ||
                          a->intrin->intrinsic == nir_intrinsic_store_buffer_amd;
   bool is_b_buffer_amd = b->intrin->intrinsic == nir_intrinsic_load_buffer_amd ||
                          b->intrin->intrinsic == nir_intrinsic_store_buffer_amd;
   if (is_a_buffer_amd || is_b_buffer_amd) {
      if (is_a_buffer_amd != is_b_buffer_amd) {
         return true;
      }
      if ((a->access | b->access) & ACCESS_USES_FORMAT_AMD) {
         return true;
      }
      bool a_store = a->intrin->intrinsic == nir_intrinsic_store_buffer_amd;
      bool b_store = b->intrin->intrinsic == nir_intrinsic_store_buffer_amd;
      if (!nir_srcs_equal(a->intrin->src[2 + a_store], b->intrin->src[2 + b_store])) {
         return true;
      }
      if (!nir_srcs_equal(a->intrin->src[3 + a_store], b->intrin->src[3 + b_store])) {
         return true;
      }
   }

   uint32_t a_offsets[2], b_offsets[2] = { 0, 0 };
   unsigned a_count = parse_shared2_offsets(a, a_offsets);
   unsigned b_count = parse_shared2_offsets(b, b_offsets);
   for (unsigned i = 0; i < a_count; i++) {
      for (unsigned j = 0; j < b_count; j++) {
         if (may_alias_internal(a, b, a_offsets[i], b_offsets[j])) {
            return true;
         }
      }
   }

   return false;
}

static bool
check_for_aliasing(struct vectorize_ctx *ctx, struct entry *first, struct entry *second)
{
   nir_variable_mode mode = get_variable_mode(first);
   if (mode & (nir_var_uniform | nir_var_system_value |
               nir_var_mem_push_const | nir_var_mem_ubo)) {
      return false;
   }

   unsigned mode_index = mode_to_index(mode);
   if (first->is_store) {
      list_for_each_entry_from(struct entry, next, first, &ctx->entries[mode_index], head) {
         if (next == first) {
            continue;
         }
         if (next == second) {
            return false;
         }
         if (may_alias(ctx->shader, first, next)) {
            return true;
         }
      }
   } else {
      list_for_each_entry_from_rev(struct entry, prev, second, &ctx->entries[mode_index], head) {
         if (prev == second) {
            continue;
         }
         if (prev == first) {
            return false;
         }
         if (prev->is_store && may_alias(ctx->shader, second, prev)) {
            return true;
         }
      }
   }

   return false;
}

static uint64_t
calc_gcd(uint64_t a, uint64_t b)
{
   while (b != 0) {
      uint64_t tmp_a = a;
      a = b;
      b = tmp_a % b;
   }
   return a;
}

static uint64_t
round_down(uint64_t a, uint64_t b)
{
   return a / b * b;
}

static bool
addition_wraps(uint64_t a, uint64_t b, unsigned bits)
{
   uint64_t mask = BITFIELD64_MASK(bits);
   return ((a + b) & mask) < (a & mask);
}

static bool
check_for_robustness(struct vectorize_ctx *ctx, struct entry *low, uint64_t high_offset)
{
   nir_variable_mode mode = get_variable_mode(low);
   if (!(mode & ctx->options->robust_modes)) {
      return false;
   }

   uint64_t max_low = round_down(UINT64_MAX, low->align_mul) + low->align_offset;
   if (!addition_wraps(max_low, high_offset, 64)) {
      return false;
   }

   if (low->info->base_src < 0) {
      return true;
   }

   uint64_t stride = 0;
   for (unsigned i = 0; i < low->key->offset_def_count; i++) {
      unsigned lsb_zero = low->key->offset_def_num_lsbz[i];
      if (lsb_zero != 64) {
         stride = calc_gcd(low->key->offset_defs_mul[i] << lsb_zero, stride);
      }
   }

   unsigned addition_bits = low->intrin->src[low->info->base_src].ssa->bit_size;
   max_low = low->offset;
   if (stride) {
      max_low = round_down(BITFIELD64_MASK(addition_bits), stride) + (low->offset % stride);
   }
   return addition_wraps(max_low, high_offset, addition_bits);
}

static bool
is_strided_vector(const struct glsl_type *type)
{
   if (glsl_type_is_vector(type)) {
      unsigned explicit_stride = glsl_get_explicit_stride(type);
      return explicit_stride != 0 && explicit_stride !=
                                        type_scalar_size_bytes(glsl_get_array_element(type));
   } else {
      return false;
   }
}

static bool
can_vectorize(struct vectorize_ctx *ctx, struct entry *first, struct entry *second)
{
   if ((first->access | second->access) & ACCESS_KEEP_SCALAR) {
      return false;
   }

   if (!(get_variable_mode(first) & ctx->options->modes) ||
       !(get_variable_mode(second) & ctx->options->modes)) {
      return false;
   }

   if (check_for_aliasing(ctx, first, second)) {
      return false;
   }

   if (first->info != second->info || first->access != second->access ||
       (first->access & ACCESS_VOLATILE) || first->info->is_unvectorizable) {
      return false;
   }

   if ((first->access & ACCESS_ATOMIC) && get_bit_size(first) != get_bit_size(second)) {
      return false;
   }

   if (first->intrin->intrinsic == nir_intrinsic_load_buffer_amd ||
       first->intrin->intrinsic == nir_intrinsic_store_buffer_amd) {
      if (first->access & ACCESS_USES_FORMAT_AMD) {
         return false;
      }
      if (nir_intrinsic_memory_modes(first->intrin) != nir_intrinsic_memory_modes(second->intrin)) {
         return false;
      }
      bool store = first->intrin->intrinsic == nir_intrinsic_store_buffer_amd;
      if (!nir_srcs_equal(first->intrin->src[2 + store], second->intrin->src[2 + store])) {
         return false;
      }
      if (!nir_srcs_equal(first->intrin->src[3 + store], second->intrin->src[3 + store])) {
         return false;
      }
   }

   return true;
}

static bool
try_vectorize(nir_function_impl *impl, struct vectorize_ctx *ctx,
              struct entry *low, struct entry *high,
              struct entry *first, struct entry *second)
{
   if (!can_vectorize(ctx, first, second)) {
      return false;
   }

   int64_t diff_signed = get_offset_diff(low, high);
   if (diff_signed < 0 || diff_signed > UINT32_MAX) {
      return false;
   }
   uint64_t diff = (uint64_t)diff_signed;

   if (check_for_robustness(ctx, low, diff)) {
      return false;
   }

   if (first->deref) {
      const struct glsl_type *first_type = first->deref->type;
      const struct glsl_type *second_type = second->deref->type;
      if (is_strided_vector(first_type) || is_strided_vector(second_type)) {
         return false;
      }
   }

   unsigned low_bit_size = get_bit_size(low);
   unsigned high_bit_size = get_bit_size(high);
   unsigned low_size = low->num_components * low_bit_size;
   unsigned high_size = high->num_components * high_bit_size;
   unsigned new_size = MAX2(diff * 8u + high_size, low_size);

   unsigned new_bit_size = 0;
   if (new_bitsize_acceptable(ctx, low_bit_size, low, high, new_size)) {
      new_bit_size = low_bit_size;
   } else if (low_bit_size != high_bit_size &&
              new_bitsize_acceptable(ctx, high_bit_size, low, high, new_size)) {
      new_bit_size = high_bit_size;
   } else if (!(first->access & ACCESS_ATOMIC)) {
      new_bit_size = 64;
      for (; new_bit_size >= 8; new_bit_size /= 2) {
         if (new_bit_size == low_bit_size || new_bit_size == high_bit_size) {
            continue;
         }
         if (new_bitsize_acceptable(ctx, new_bit_size, low, high, new_size)) {
            break;
         }
      }
      if (new_bit_size < 8) {
         return false;
      }
   } else {
      return false;
   }
   unsigned new_num_components = new_size / new_bit_size;

   nir_builder b = nir_builder_create(impl);

   if (first->is_store) {
      vectorize_stores(&b, ctx, low, high, first, second,
                       new_bit_size, new_num_components, (unsigned)diff * 8u);
   } else {
      vectorize_loads(&b, ctx, low, high, first, second,
                      new_bit_size, new_num_components, (unsigned)diff * 8u);
   }

   return true;
}

static bool
try_vectorize_shared2(struct vectorize_ctx *ctx,
                      struct entry *low, struct entry *high,
                      struct entry *first, struct entry *second)
{
   unsigned low_bit_size = get_bit_size(low);
   unsigned high_bit_size = get_bit_size(high);
   unsigned low_size = low->num_components * low_bit_size / 8;
   unsigned high_size = high->num_components * high_bit_size / 8;
   if (low_size != high_size) {
      return false;
   }
   if (!(low_size == 4 || (low->is_store && low_size == 8))) {
      return false;
   }
   if (low->align_mul % low_size || low->align_offset % low_size) {
      return false;
   }
   if (high->align_mul % low_size || high->align_offset % low_size) {
      return false;
   }

   if (!can_vectorize(ctx, first, second) || first->deref)
      return false;

   int64_t diff_signed = get_offset_diff(low, high);
   if (diff_signed < 0 || diff_signed > UINT32_MAX) {
      return false;
   }
   uint64_t diff = (uint64_t)diff_signed;

   bool st64 = diff % (64 * low_size) == 0;
   unsigned stride = st64 ? 64 * low_size : low_size;
   if (diff % stride || diff > 255 * stride) {
      return false;
   }

   if (first->is_store) {
      if (get_write_mask(low->intrin) != BITFIELD_MASK(low->num_components)) {
         return false;
      }
      if (get_write_mask(high->intrin) != BITFIELD_MASK(high->num_components)) {
         return false;
      }
   }

   nir_def *offset = low->intrin->src[first->is_store].ssa;
   if (first != low) {
      hoist_base_addr(&first->intrin->instr, nir_def_instr(offset));
   }
   nir_builder b = nir_builder_at(nir_after_instr(first->is_store ? second->instr : first->instr));
   offset = nir_iadd_imm(&b, offset, nir_intrinsic_base(low->intrin));

   uint32_t access = nir_intrinsic_access(first->intrin);
   if (first->is_store) {
      nir_def *low_val = low->intrin->src[low->info->value_src].ssa;
      nir_def *high_val = high->intrin->src[high->info->value_src].ssa;
      nir_def *val = nir_vec2(&b, nir_bitcast_vector(&b, low_val, low_size * 8u),
                              nir_bitcast_vector(&b, high_val, low_size * 8u));
      nir_store_shared2_amd(&b, val, offset, .offset1 = (unsigned)diff / stride, .st64 = st64, .access = access);
   } else {
      nir_def *new_def = nir_load_shared2_amd(&b, low_size * 8u, offset, .offset1 = (unsigned)diff / stride,
                                              .st64 = st64, .access = access);
      nir_def_rewrite_uses(&low->intrin->def,
                           nir_bitcast_vector(&b, nir_channel(&b, new_def, 0), low_bit_size));
      nir_def_rewrite_uses(&high->intrin->def,
                           nir_bitcast_vector(&b, nir_channel(&b, new_def, 1), high_bit_size));
   }

   nir_instr_remove(first->instr);
   nir_instr_remove(second->instr);

   return true;
}

static bool
update_align(struct entry *entry)
{
   if (nir_intrinsic_has_align_mul(entry->intrin) &&
       (entry->align_mul != nir_intrinsic_align_mul(entry->intrin) ||
        entry->align_offset != nir_intrinsic_align_offset(entry->intrin))) {
      nir_intrinsic_set_align(entry->intrin, entry->align_mul, entry->align_offset);
      return true;
   }
   return false;
}

static bool
vectorize_sorted_entries(struct vectorize_ctx *ctx, nir_function_impl *impl,
                         struct util_dynarray *arr)
{
   unsigned num_entries = util_dynarray_num_elements(arr, struct entry *);

   bool progress = false;
   for (unsigned first_idx = 0; first_idx < num_entries; first_idx++) {
      struct entry *low = *util_dynarray_element(arr, struct entry *, first_idx);
      if (!low) {
         continue;
      }

      for (unsigned second_idx = first_idx + 1; second_idx < num_entries; second_idx++) {
         struct entry *high = *util_dynarray_element(arr, struct entry *, second_idx);
         if (!high) {
            continue;
         }

         struct entry *first = low->index < high->index ? low : high;
         struct entry *second = low->index < high->index ? high : low;

         int64_t diff_signed = get_offset_diff(low, high);
         if (diff_signed < 0 || diff_signed > UINT32_MAX) {
            continue;
         }
         uint64_t diff = (uint64_t)diff_signed;

         unsigned max_hole = first->is_store ? 0 : 28;
         unsigned low_size = get_bit_size(low) / 8u * low->num_components;
         bool separate = diff > max_hole + low_size;
         if (separate) {
            continue;
         }

         if (try_vectorize(impl, ctx, low, high, first, second)) {
            low = low->is_store ? second : first;
            *util_dynarray_element(arr, struct entry *, second_idx) = NULL;
            progress = true;
         }
      }
      *util_dynarray_element(arr, struct entry *, first_idx) = low;
   }

   if (!ctx->options->has_shared2_amd) {
      return progress;
   }

   for (unsigned first_idx = 0; first_idx < num_entries; first_idx++) {
      struct entry *low = *util_dynarray_element(arr, struct entry *, first_idx);
      if (!low || get_variable_mode(low) != nir_var_mem_shared) {
         continue;
      }

      for (unsigned second_idx = first_idx + 1; second_idx < num_entries; second_idx++) {
         struct entry *high = *util_dynarray_element(arr, struct entry *, second_idx);
         if (!high || get_variable_mode(high) != nir_var_mem_shared) {
            continue;
         }

         struct entry *first = low->index < high->index ? low : high;
         struct entry *second = low->index < high->index ? high : low;
         if (try_vectorize_shared2(ctx, low, high, first, second)) {
            low = NULL;
            *util_dynarray_element(arr, struct entry *, second_idx) = NULL;
            progress = true;
            break;
         }
      }

      *util_dynarray_element(arr, struct entry *, first_idx) = low;
   }

   return progress;
}

static bool
vectorize_entries(struct vectorize_ctx *ctx, nir_function_impl *impl, struct hash_table *ht)
{
   if (!ht) {
      return false;
   }

   bool progress = false;
   hash_table_foreach(ht, entry) {
      struct util_dynarray *arr = entry->data;
      if (!arr->size) {
         continue;
      }

      sort_entries_specialized((struct entry **)util_dynarray_begin(arr),
                              util_dynarray_num_elements(arr, struct entry *));

      while (vectorize_sorted_entries(ctx, impl, arr)) {
         progress = true;
      }

      util_dynarray_foreach(arr, struct entry *, elem) {
         if (*elem) {
            progress |= update_align(*elem);
         }
      }
   }

   _mesa_hash_table_clear(ht, delete_entry_dynarray);

   return progress;
}

static bool
handle_barrier(struct vectorize_ctx *ctx, bool *progress, nir_function_impl *impl, nir_instr *instr)
{
   unsigned modes = 0;
   bool acquire = true;
   bool release = true;
   if (instr->type == nir_instr_type_intrinsic) {
      nir_intrinsic_instr *intrin = nir_instr_as_intrinsic(instr);
      switch (intrin->intrinsic) {
      case nir_intrinsic_terminate_if:
      case nir_intrinsic_terminate:
      case nir_intrinsic_launch_mesh_workgroups:
         modes = nir_var_all;
         break;
      case nir_intrinsic_demote_if:
      case nir_intrinsic_demote:
         acquire = false;
         modes = nir_var_all;
         break;
      case nir_intrinsic_barrier:
         if (nir_intrinsic_memory_scope(intrin) == SCOPE_NONE) {
            break;
         }

         modes = nir_intrinsic_memory_modes(intrin) & (nir_var_mem_ssbo |
                                                       nir_var_mem_shared |
                                                       nir_var_mem_global |
                                                       nir_var_mem_task_payload);
         acquire = nir_intrinsic_memory_semantics(intrin) & (NIR_MEMORY_ACQUIRE | NIR_MEMORY_MAKE_VISIBLE);
         release = nir_intrinsic_memory_semantics(intrin) & (NIR_MEMORY_RELEASE | NIR_MEMORY_MAKE_AVAILABLE);
         switch (nir_intrinsic_memory_scope(intrin)) {
         case SCOPE_INVOCATION:
            modes = 0;
            break;
         default:
            break;
         }
         break;
      default:
         return false;
      }
   } else if (instr->type == nir_instr_type_call) {
      modes = nir_var_all;
   } else {
      return false;
   }

   while (modes) {
      unsigned mode_index = u_bit_scan(&modes);
      if ((1 << mode_index) == nir_var_mem_global) {
         assert(list_is_empty(&ctx->entries[mode_index]));
         assert(ctx->loads[mode_index] == NULL);
         assert(ctx->stores[mode_index] == NULL);
         continue;
      }

      if (acquire) {
         *progress |= vectorize_entries(ctx, impl, ctx->loads[mode_index]);
      }
      if (release) {
         *progress |= vectorize_entries(ctx, impl, ctx->stores[mode_index]);
      }
   }

   return true;
}

static bool
process_block(nir_function_impl *impl, struct vectorize_ctx *ctx, nir_block *block)
{
   bool progress = false;

   for (unsigned i = 0; i < nir_num_variable_modes; i++) {
      list_inithead(&ctx->entries[i]);
      if (ctx->loads[i]) {
         _mesa_hash_table_clear(ctx->loads[i], delete_entry_dynarray);
      }
      if (ctx->stores[i]) {
         _mesa_hash_table_clear(ctx->stores[i], delete_entry_dynarray);
      }
   }

   unsigned next_index = 0;

   nir_foreach_instr_safe(instr, block) {
      instr->index = next_index++;

      if (handle_barrier(ctx, &progress, impl, instr)) {
         continue;
      }

      if (instr->type != nir_instr_type_intrinsic) {
         continue;
      }
      nir_intrinsic_instr *intrin = nir_instr_as_intrinsic(instr);

      const struct intrinsic_info *info = get_info(intrin->intrinsic);
      if (!info) {
         continue;
      }

      nir_variable_mode mode = info->mode;
      if (nir_intrinsic_has_memory_modes(intrin)) {
         mode = nir_intrinsic_memory_modes(intrin);
      } else if (!mode) {
         mode = nir_src_as_deref(intrin->src[info->deref_src])->modes;
      }
      if (!(mode & aliasing_modes(ctx->options->modes))) {
         continue;
      }
      unsigned mode_index = mode_to_index(mode);

      struct entry *entry = create_entry(ctx, ctx, info, intrin);
      if (unlikely(entry == NULL)) {
         continue;
      }
      entry->index = next_index;

      list_addtail(&entry->head, &ctx->entries[mode_index]);

      struct hash_table *adj_ht = NULL;
      if (entry->is_store) {
         if (!ctx->stores[mode_index]) {
            ctx->stores[mode_index] = _mesa_hash_table_create(ctx, &hash_entry_key, &entry_key_equals);
         }
         adj_ht = ctx->stores[mode_index];
      } else {
         if (!ctx->loads[mode_index]) {
            ctx->loads[mode_index] = _mesa_hash_table_create(ctx, &hash_entry_key, &entry_key_equals);
         }
         adj_ht = ctx->loads[mode_index];
      }

      uint32_t key_hash = hash_entry_key(entry->key);
      struct hash_entry *adj_entry = _mesa_hash_table_search_pre_hashed(adj_ht, key_hash, entry->key);
      struct util_dynarray *arr;
      if (adj_entry && adj_entry->data) {
         arr = (struct util_dynarray *)adj_entry->data;
      } else {
         arr = ralloc(ctx, struct util_dynarray);
         if (unlikely(arr == NULL)) {
            continue;
         }
         util_dynarray_init(arr, arr);
         _mesa_hash_table_insert_pre_hashed(adj_ht, key_hash, entry->key, arr);
      }
      util_dynarray_append(arr, entry);
   }

   for (unsigned i = 0; i < nir_num_variable_modes; i++) {
      progress |= vectorize_entries(ctx, impl, ctx->loads[i]);
      progress |= vectorize_entries(ctx, impl, ctx->stores[i]);
   }

   return progress;
}

bool
nir_opt_load_store_vectorize(nir_shader *shader, const nir_load_store_vectorize_options *options)
{
   bool progress = false;

   struct vectorize_ctx *ctx = rzalloc(NULL, struct vectorize_ctx);
   if (unlikely(ctx == NULL)) {
      return false;
   }
   ctx->shader = shader;
   ctx->numlsb_ht = _mesa_pointer_hash_table_create(ctx);
   ctx->options = options;

   nir_shader_index_vars(shader, options->modes);

   nir_foreach_function_impl(impl, shader) {
      if (options->modes & nir_var_function_temp) {
         nir_function_impl_index_vars(impl);
      }

      nir_foreach_block(block, impl) {
         progress |= process_block(impl, ctx, block);
      }

      nir_progress(true, impl,
                   nir_metadata_control_flow | nir_metadata_live_defs);
   }

   ralloc_free(ctx);
   return progress;
}

static bool
opt_load_store_update_alignments_callback(struct nir_builder *b,
                                          nir_intrinsic_instr *intrin,
                                          void *s)
{
   if (!nir_intrinsic_has_align_mul(intrin)) {
      return false;
   }

   const struct intrinsic_info *info = get_info(intrin->intrinsic);
   if (!info) {
      return false;
   }

   struct vectorize_ctx* ctx = (struct vectorize_ctx*)s;
   struct entry *entry = create_entry(NULL, ctx, info, intrin);
   if (unlikely(entry == NULL)) {
      return false;
   }
   const bool progress = update_align(entry);
   ralloc_free(entry);

   return progress;
}

bool
nir_opt_load_store_update_alignments(nir_shader *shader)
{
   struct vectorize_ctx ctx;
   ctx.numlsb_ht = _mesa_pointer_hash_table_create(NULL);
   bool progress = nir_shader_intrinsics_pass(shader,
                                              opt_load_store_update_alignments_callback,
                                              nir_metadata_control_flow |
                                                 nir_metadata_live_defs |
                                                 nir_metadata_instr_index,
                                              &ctx);
   ralloc_free(ctx.numlsb_ht);
   return progress;
}
