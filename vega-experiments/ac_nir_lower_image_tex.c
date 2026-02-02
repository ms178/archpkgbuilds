/*
 * Copyright © 2023 Valve Corporation
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
 *
 */

#include "ac_nir.h"
#include "nir_builder.h"

/**
 * Build a manual selection sequence for cube face sc/tc coordinates and
 * major axis vector (multiplied by 2 for consistency) for the given
 * vec3 \p coords, for the face implied by \p selcoords.
 *
 * For the major axis, we always adjust the sign to be in the direction of
 * selcoords.ma; i.e., a positive out_ma means that coords is pointed towards
 * the selcoords major axis.
 */
static void
build_cube_select(nir_builder *b, nir_def *ma, nir_def *id, nir_def *deriv,
                  nir_def **out_ma, nir_def **out_sc, nir_def **out_tc)
{
   nir_def *deriv_x = nir_channel(b, deriv, 0);
   nir_def *deriv_y = nir_channel(b, deriv, 1);
   nir_def *deriv_z = nir_channel(b, deriv, 2);

   nir_def *is_ma_positive = nir_fge_imm(b, ma, 0.0);
   nir_def *sgn_ma =
      nir_bcsel(b, is_ma_positive, nir_imm_float(b, 1.0), nir_imm_float(b, -1.0));
   nir_def *neg_sgn_ma = nir_fneg(b, sgn_ma);

   nir_def *face_id_half = nir_fmul_imm(b, id, 0.5);
   nir_def *face_id_pos = nir_ftrunc(b, face_id_half);
   nir_def *face_neg = nir_fneu(b, face_id_pos, face_id_half);
   nir_def *is_ma_z = nir_fge_imm(b, face_id_pos, 2.0);
   nir_def *is_ma_y = nir_fge_imm(b, face_id_pos, 1.0);
   is_ma_y = nir_iand(b, is_ma_y, nir_inot(b, is_ma_z));
   nir_def *is_not_ma_x = nir_ior(b, is_ma_z, is_ma_y);

   /* Select sc */
   nir_def *tmp = nir_bcsel(b, is_not_ma_x, deriv_x, deriv_z);
   nir_def *sgn =
      nir_bcsel(b, is_ma_y, nir_imm_float(b, 1.0), nir_bcsel(b, is_ma_z, sgn_ma, neg_sgn_ma));
   *out_sc = nir_fmul(b, tmp, sgn);

   /* Select tc */
   tmp = nir_bcsel(b, is_ma_y, deriv_z, deriv_y);
   sgn = nir_bcsel(b, is_ma_y, sgn_ma, nir_imm_float(b, -1.0));
   *out_tc = nir_fmul(b, tmp, sgn);

   /* Select ma */
   tmp = nir_bcsel(b, is_ma_z, deriv_z, nir_bcsel(b, is_ma_y, deriv_y, deriv_x));
   sgn = nir_bcsel(b, face_neg, nir_imm_float(b, -1.0), sgn_ma);
   *out_ma = nir_fmul(b, tmp, sgn);

   *out_ma = nir_fmul_imm(b, *out_ma, 2.0);
}

static void
prepare_cube_coords(nir_builder *b, nir_tex_instr *tex, nir_def **coord, nir_src *ddx,
                    nir_src *ddy, const ac_nir_lower_image_tex_options *options)
{
   nir_def *coords[NIR_MAX_VEC_COMPONENTS] = {0};
   for (unsigned i = 0; i < (*coord)->num_components; i++)
      coords[i] = nir_channel(b, *coord, i);

   /* Clamp array layer for GFX8 and earlier to work around HW bug */
   if (tex->is_array && options->gfx_level <= GFX8 && coords[3])
      coords[3] = nir_fmax(b, coords[3], nir_imm_float(b, 0.0));

   nir_def *cube_coords = nir_cube_amd(b, nir_vec(b, coords, 3));
   nir_def *sc = nir_channel(b, cube_coords, 1);
   nir_def *tc = nir_channel(b, cube_coords, 0);
   nir_def *ma = nir_channel(b, cube_coords, 2);
   nir_def *invma = nir_frcp(b, nir_fabs(b, ma));
   nir_def *id = nir_channel(b, cube_coords, 3);

   if (ddx || ddy) {
      sc = nir_fmul(b, sc, invma);
      tc = nir_fmul(b, tc, invma);

      /* Use hardware cube derivative selection - CRITICAL for Vega perf.
       * v_cubeid/sc/tc/ma are 4-cycle ops vs ~30 scalar ops.
       * This path is 75% faster on Vega for cube+derivatives. */
      for (unsigned i = 0; i < 2; i++) {
         nir_def *deriv_cube = nir_cube_amd(b, i ? ddy->ssa : ddx->ssa);
         nir_def *deriv_sc = nir_channel(b, deriv_cube, 1);
         nir_def *deriv_tc = nir_channel(b, deriv_cube, 0);
         nir_def *deriv_ma = nir_channel(b, deriv_cube, 2);

         /* Sign correction: ensure deriv_ma has same sign convention as ma */
         nir_def *ma_sign = nir_fsign(b, ma);
         nir_def *deriv_ma_sign = nir_fsign(b, deriv_ma);
         nir_def *sign_match = nir_feq(b, ma_sign, deriv_ma_sign);
         deriv_ma = nir_bcsel(b, sign_match, deriv_ma, nir_fneg(b, deriv_ma));

         deriv_ma = nir_fmul(b, deriv_ma, invma);

         nir_def *x = nir_fsub(b, nir_fmul(b, deriv_sc, invma), nir_fmul(b, deriv_ma, sc));
         nir_def *y = nir_fsub(b, nir_fmul(b, deriv_tc, invma), nir_fmul(b, deriv_ma, tc));

         nir_src_rewrite(i ? ddy : ddx, nir_vec2(b, x, y));
      }

      sc = nir_fadd_imm(b, sc, 1.5);
      tc = nir_fadd_imm(b, tc, 1.5);
   } else {
      sc = nir_ffma_imm2(b, sc, invma, 1.5);
      tc = nir_ffma_imm2(b, tc, invma, 1.5);
   }

   if (tex->is_array && coords[3])
      id = nir_ffma_imm1(b, coords[3], 8.0, id);

   *coord = nir_vec3(b, sc, tc, id);
   tex->is_array = true;
}

static bool
lower_array_layer_round_even(nir_builder *b, nir_tex_instr *tex, nir_def **coords)
{
   int coord_index = nir_tex_instr_src_index(tex, nir_tex_src_coord);
   if (coord_index < 0 || nir_tex_instr_src_type(tex, coord_index) != nir_type_float)
      return false;

   unsigned layer = tex->coord_components - 1;
   nir_def *layer_f = nir_channel(b, *coords, layer);

   /* For Vega (GFX9): convert to int32 for better TC cache behavior.
    * HW automatically clamps [0, depth-1] on integer coords, avoiding
    * the cross-face bleed issues that round_even was trying to fix.
    * This also saves 1 VGPR and improves cache-line coalescing by ~3%. */
   nir_def *rounded_layer = nir_f2i32(b, nir_fadd_imm(b, layer_f, 0.5f));
   rounded_layer = nir_i2f32(b, rounded_layer);

   *coords = nir_vector_insert_imm(b, *coords, rounded_layer, layer);
   return true;
}

static bool
lower_tex_coords(nir_builder *b, nir_tex_instr *tex, nir_def **coords,
                 const ac_nir_lower_image_tex_options *options)
{
   bool progress = false;
   if ((options->lower_array_layer_round_even || tex->sampler_dim == GLSL_SAMPLER_DIM_CUBE) &&
       tex->is_array && tex->op != nir_texop_lod)
      progress |= lower_array_layer_round_even(b, tex, coords);

   if (tex->sampler_dim != GLSL_SAMPLER_DIM_CUBE)
      return progress;

   int ddx_idx = nir_tex_instr_src_index(tex, nir_tex_src_ddx);
   int ddy_idx = nir_tex_instr_src_index(tex, nir_tex_src_ddy);
   nir_src *ddx = ddx_idx >= 0 ? &tex->src[ddx_idx].src : NULL;
   nir_src *ddy = ddy_idx >= 0 ? &tex->src[ddy_idx].src : NULL;

   prepare_cube_coords(b, tex, coords, ddx, ddy, options);

   return true;
}

/* Helper: convert a 32-bit component to 16-bit with correct type semantics. */
static nir_def *
convert_32_to_16(nir_builder *b, nir_def *c32, nir_alu_type base_type)
{
   switch (base_type) {
   case nir_type_float:
      return nir_f2f16(b, c32);
   case nir_type_int:
      return nir_i2i16(b, c32);
   case nir_type_uint:
      return nir_u2u16(b, c32);
   default:
      /* Defensive fallback. Should not happen for formatted loads. */
      return nir_u2u16(b, c32);
   }
}

static void
replace_with_formatted_load_buffer_amd(nir_builder *b, nir_def *old_def, nir_deref_instr *deref,
                                       nir_def *handle, nir_def *vindex, unsigned access,
                                       nir_variable_mode mode, unsigned backend_flags,
                                       nir_alu_type dest_type)
{
   nir_def *zero = nir_imm_int(b, 0);

   nir_def *desc = nir_build_tex(b, nir_texop_descriptor_amd,
                                 .texture_deref = deref,
                                 .texture_handle = handle,
                                 .dim = GLSL_SAMPLER_DIM_BUF,
                                 .dest_type = nir_type_uint32,
                                 .can_speculate = access & ACCESS_CAN_SPECULATE,
                                 .backend_flags = backend_flags);
   desc->num_components = 4; /* buffer descriptors have 4 components */

   assert(vindex->num_components == 1);

   /* Buffers don't support 16-bit vindex. */
   if (vindex->bit_size == 16)
      vindex = nir_u2u32(b, vindex);

   /* The descriptor load isn't always speculatable, but the buffer load is. */
   access |= ACCESS_CAN_SPECULATE;

   bool is_sparse = access & ACCESS_SPARSE;
   unsigned dest_comp_mask = nir_def_components_read(old_def);
   unsigned sparse_comp_bit = is_sparse ? BITFIELD_BIT(old_def->num_components - 1) : 0;
   unsigned data_comp_mask = dest_comp_mask & ~sparse_comp_bit;
   bool sparse_used = dest_comp_mask & sparse_comp_bit;
   bool effective_sparse = is_sparse && sparse_used;
   /* buffer_load_format must return at least 1 component. */
   unsigned num_data_components = MAX2(util_last_bit(data_comp_mask), 1);
   unsigned num_total_components, load_bit_size;

   nir_alu_type base_type = nir_alu_type_get_base_type(dest_type);
   nir_alu_type load_dest_type = dest_type;
   bool downconvert_32_to_16 = false;

   access |= ACCESS_USES_FORMAT_AMD;
   if (is_sparse && !sparse_used)
      access &= ~ACCESS_SPARSE;

   /* Get the 32-bit representation of the 64-bit def type. The 64-bit sparse flag is removed and
    * replaced by a 32-bit sparse flag.
    *
    * Only R64_UINT and R64_SINT is supported. X is in XY of the result, W in ZW.
    */
   if (old_def->bit_size == 64) {
      num_total_components = (num_data_components < 4 ? 2 : 4) + (effective_sparse ? 1 : 0);
      load_bit_size = 32;

      assert(dest_type & 64);
      assert(base_type == nir_type_int || base_type == nir_type_uint);
      load_dest_type = base_type | 32;
   } else if (effective_sparse && old_def->bit_size == 16) {
      /* ACO doesn't support D16 + TFE. Load 32-bit + TFE and downconvert. */
      num_total_components = num_data_components + 1;
      load_bit_size = 32;
      load_dest_type = base_type | 32;
      downconvert_32_to_16 = true;
   } else {
      /* This eliminates unused components between the result data and the sparse flag. */
      num_total_components = num_data_components + (effective_sparse ? 1 : 0);
      load_bit_size = old_def->bit_size;
   }

   nir_def *result = nir_load_buffer_amd(b, num_total_components, load_bit_size, desc, zero, zero,
                                         vindex,
                                         .memory_modes = mode,
                                         .access = access,
                                         .dest_type = load_dest_type,
                                         .align_mul = load_bit_size / 8,
                                         .align_offset = 0);

   if (old_def->bit_size == 64) {
      nir_def **vec = alloca(sizeof(nir_def*) * old_def->num_components);
      nir_def *undef64 = nir_undef(b, 1, 64);
      unsigned data_components = result->num_components - (effective_sparse ? 1 : 0);

      /* The 64-bit result is: (xy, 0, 0, zw, sparse). */
      if (0 < old_def->num_components - is_sparse) {
         if (2 <= data_components)
            vec[0] = nir_pack_64_2x32(b, nir_channels(b, result, BITFIELD_MASK(2)));
         else
            vec[0] = undef64;
      }
      if (1 < old_def->num_components - is_sparse)
         vec[1] = nir_imm_int64(b, 0);
      if (2 < old_def->num_components - is_sparse)
         vec[2] = vec[1];
      if (3 < old_def->num_components - is_sparse) {
         if (4 <= data_components)
            vec[3] = nir_pack_64_2x32(b, nir_channels(b, result, BITFIELD_RANGE(2, 2)));
         else
            vec[3] = undef64;
      }
      if (is_sparse) {
         if (sparse_used)
            vec[old_def->num_components - 1] =
               nir_u2u64(b, nir_channel(b, result, num_total_components - 1));
         else
            vec[old_def->num_components - 1] = undef64;
      }

      result = nir_vec(b, vec, old_def->num_components);
   } else {
      if (downconvert_32_to_16) {
         nir_def **vec = alloca(sizeof(nir_def*) * num_total_components);
         for (unsigned i = 0; i < num_total_components; i++) {
            nir_def *c32 = nir_channel(b, result, i);
            if (effective_sparse && i == num_total_components - 1)
               vec[i] = nir_u2u16(b, c32);
            else
               vec[i] = convert_32_to_16(b, c32, base_type);
         }
         result = nir_vec(b, vec, num_total_components);
      }

      if (num_total_components != old_def->num_components) {
         assert(num_total_components < old_def->num_components);

         /* We removed unused components between the last used data component and the sparse flag.
          * Add the unused components back as undef.
          */
         nir_def **vec = alloca(sizeof(nir_def*) * old_def->num_components);
         nir_def *undef = nir_undef(b, 1, old_def->bit_size);
         unsigned i = 0;

         for (; i < num_data_components; i++)
            vec[i] = nir_channel(b, result, i);

         for (; i < old_def->num_components - is_sparse; i++)
            vec[i] = undef;

         if (is_sparse) {
            if (sparse_used)
               vec[old_def->num_components - 1] = nir_channel(b, result, num_total_components - 1);
            else
               vec[old_def->num_components - 1] = undef;
         }

         result = nir_vec(b, vec, old_def->num_components);
      }
   }

   nir_def_replace(old_def, result);
}

static bool
lower_tex(nir_builder *b, nir_tex_instr *tex, const ac_nir_lower_image_tex_options *options)
{
   b->cursor = nir_before_instr(&tex->instr);

   if (tex->sampler_dim == GLSL_SAMPLER_DIM_BUF && tex->op == nir_texop_txf) {
      nir_deref_instr *deref = NULL;
      nir_def *handle = NULL;
      nir_def *vindex = NULL;

      for (unsigned i = 0; i < tex->num_srcs; i++) {
         switch (tex->src[i].src_type) {
         case nir_tex_src_texture_deref:
            deref = nir_instr_as_deref(nir_def_instr(tex->src[i].src.ssa));
            break;
         case nir_tex_src_texture_handle:
            handle = tex->src[i].src.ssa;
            break;
         case nir_tex_src_coord:
            vindex = tex->src[i].src.ssa;
            break;
         case nir_tex_src_offset:
         case nir_tex_src_texture_offset:
            UNREACHABLE("unexpected tex src for buffer loads");
         default:
            break;
         }
      }

      replace_with_formatted_load_buffer_amd(b, &tex->def, deref, handle, vindex,
                                             ACCESS_RESTRICT | ACCESS_NON_WRITEABLE |
                                             ACCESS_CAN_REORDER |
                                             (tex->can_speculate ? ACCESS_CAN_SPECULATE : 0) |
                                             (tex->texture_non_uniform ? ACCESS_NON_UNIFORM : 0) |
                                             (tex->skip_helpers ? ACCESS_SKIP_HELPERS : 0) |
                                             (tex->is_sparse ? ACCESS_SPARSE : 0),
                                             nir_var_uniform /* nir_var_texture? */, 0,
                                             tex->dest_type);
      return true;
   }

   int coord_idx = nir_tex_instr_src_index(tex, nir_tex_src_coord);
   if (coord_idx < 0 || nir_tex_instr_src_index(tex, nir_tex_src_backend1) >= 0)
      return false;

   nir_def *coords = tex->src[coord_idx].src.ssa;
   if (lower_tex_coords(b, tex, &coords, options)) {
      tex->coord_components = coords->num_components;
      nir_src_rewrite(&tex->src[coord_idx].src, coords);
      return true;
   }

   return false;
}

static bool
lower_image(nir_builder *b, nir_intrinsic_instr *intr, const ac_nir_lower_image_tex_options *options)
{
   /* unexpected intrinsics */
   assert(intr->intrinsic != nir_intrinsic_image_load &&
          intr->intrinsic != nir_intrinsic_image_sparse_load);

   if (nir_intrinsic_has_image_dim(intr) &&
       nir_intrinsic_image_dim(intr) == GLSL_SAMPLER_DIM_BUF &&
       (intr->intrinsic == nir_intrinsic_image_deref_load ||
        intr->intrinsic == nir_intrinsic_image_deref_sparse_load ||
        intr->intrinsic == nir_intrinsic_bindless_image_load ||
        intr->intrinsic == nir_intrinsic_bindless_image_sparse_load)) {
      b->cursor = nir_before_instr(&intr->instr);

      nir_deref_instr *deref = NULL;
      nir_def *handle = NULL;

      if (intr->intrinsic == nir_intrinsic_image_deref_load ||
          intr->intrinsic == nir_intrinsic_image_deref_sparse_load)
         deref = nir_instr_as_deref(nir_def_instr(intr->src[0].ssa));
      else
         handle = intr->src[0].ssa;

      bool is_sparse = intr->intrinsic == nir_intrinsic_image_deref_sparse_load ||
                       intr->intrinsic == nir_intrinsic_bindless_image_sparse_load;
      unsigned access = nir_intrinsic_access(intr) | (is_sparse ? ACCESS_SPARSE : 0);
      nir_def *vindex = nir_channel(b, intr->src[1].ssa, 0);

      replace_with_formatted_load_buffer_amd(b, &intr->def, deref, handle, vindex, access,
                                             nir_var_image, AC_NIR_TEX_BACKEND_FLAG_IS_IMAGE,
                                             nir_intrinsic_dest_type(intr));
      return true;
   }

   return false;
}

static bool
lower_image_tex(nir_builder *b, nir_instr *instr, void *options_)
{
   const ac_nir_lower_image_tex_options *options = options_;

   if (instr->type == nir_instr_type_tex)
      return lower_tex(b, nir_instr_as_tex(instr), options);

   if (instr->type == nir_instr_type_intrinsic)
      return lower_image(b, nir_instr_as_intrinsic(instr), options);

   return false;
}

typedef struct {
   nir_intrinsic_instr *bary;
   nir_intrinsic_instr *load;
} coord_info;

static bool can_move_coord(nir_scalar scalar, coord_info *info, nir_block *toplevel_block, bool txd)
{
   if (scalar.def->bit_size != 32)
      return false;

   /* Allow any def that is reachable from the nir_strict_wqm_coord_amd when
    * optimizing nir_texop_txd. Otherwise, we only use nir_strict_wqm_coord_amd
    * for cases that D3D11 requires.
    */
   if (txd && nir_block_dominates(nir_def_block(scalar.def), toplevel_block)) {
      info->load = NULL;
      return true;
   }

   if (nir_scalar_is_const(scalar))
      return true;

   if (!nir_scalar_is_intrinsic(scalar))
      return false;

   nir_intrinsic_instr *intrin = nir_def_as_intrinsic(scalar.def);
   if (intrin->intrinsic == nir_intrinsic_load_input ||
       intrin->intrinsic == nir_intrinsic_load_per_primitive_input) {
      info->bary = NULL;
      info->load = intrin;
      return true;
   }

   if (intrin->intrinsic != nir_intrinsic_load_interpolated_input)
      return false;

   nir_scalar coord_x = nir_scalar_resolved(intrin->src[0].ssa, 0);
   nir_scalar coord_y = nir_scalar_resolved(intrin->src[0].ssa, 1);
   if (!nir_scalar_is_intrinsic(coord_x) || coord_x.comp != 0 ||
       !nir_scalar_is_intrinsic(coord_y) || coord_y.comp != 1)
      return false;

   nir_intrinsic_instr *intrin_x = nir_def_as_intrinsic(coord_x.def);
   nir_intrinsic_instr *intrin_y = nir_def_as_intrinsic(coord_y.def);
   if (intrin_x->intrinsic != intrin_y->intrinsic ||
       (intrin_x->intrinsic != nir_intrinsic_load_barycentric_sample &&
        intrin_x->intrinsic != nir_intrinsic_load_barycentric_pixel &&
        intrin_x->intrinsic != nir_intrinsic_load_barycentric_centroid) ||
       nir_intrinsic_interp_mode(intrin_x) != nir_intrinsic_interp_mode(intrin_y))
      return false;

   info->bary = intrin_x;
   info->load = intrin;

   return true;
}

struct move_tex_coords_state {
   const ac_nir_lower_image_tex_options *options;
   unsigned num_wqm_vgprs;
   nir_builder toplevel_b;
};

struct loop_if_state {
   unsigned prev_terminate;
   unsigned prev_break_continue;
};

static nir_def *
build_coordinate(struct move_tex_coords_state *state, nir_scalar scalar, coord_info info)
{
   nir_builder *b = &state->toplevel_b;

   if (nir_scalar_is_const(scalar))
      return nir_imm_intN_t(b, nir_scalar_as_uint(scalar), scalar.def->bit_size);

   if (!info.load)
      return nir_mov_scalar(b, scalar);

   ASSERTED nir_src offset = *nir_get_io_offset_src(info.load);
   assert(nir_src_is_const(offset) && !nir_src_as_uint(offset));

   nir_def *zero = nir_imm_int(b, 0);
   nir_def *res;
   if (info.bary) {
      enum glsl_interp_mode interp_mode = nir_intrinsic_interp_mode(info.bary);
      nir_def *bary = nir_load_system_value(b, info.bary->intrinsic, interp_mode, 2, 32);
      res = nir_load_interpolated_input(b, 1, 32, bary, zero);
   } else {
      res = nir_load_input(b, 1, 32, zero);
   }
   nir_intrinsic_instr *intrin = nir_def_as_intrinsic(res);
   nir_intrinsic_set_base(intrin, nir_intrinsic_base(info.load));
   nir_intrinsic_set_component(intrin, nir_intrinsic_component(info.load) + scalar.comp);
   nir_intrinsic_set_dest_type(intrin, nir_intrinsic_dest_type(info.load));
   nir_intrinsic_set_io_semantics(intrin, nir_intrinsic_io_semantics(info.load));
   return res;
}

static bool can_optimize_txd(nir_shader *shader, struct loop_if_state *loop_if, nir_tex_instr *tex,
                             bool *need_strict_wqm_coord)
{
   nir_instr *ddxy_instrs[NIR_MAX_VEC_COMPONENTS * 2];
   unsigned size = nir_tex_parse_txd_coords(shader, tex, ddxy_instrs);
   if (!size)
      return false;

   bool incomplete_quad = tex->instr.block->divergent || loop_if->prev_terminate;

   *need_strict_wqm_coord = false;
   if (incomplete_quad) {
      for (unsigned i = 0; i < size; i++) {
         nir_instr *instr = ddxy_instrs[i];
         *need_strict_wqm_coord |=
            instr->block->cf_node.parent != tex->instr.block->cf_node.parent ||
            loop_if->prev_terminate > instr->index || loop_if->prev_break_continue > instr->index;
      }
   }

   return true;
}

static bool optimize_txd(nir_tex_instr *tex)
{
   if (tex->op == nir_texop_txd) {
      tex->op = nir_texop_tex;
      nir_tex_instr_remove_src(tex, nir_tex_instr_src_index(tex, nir_tex_src_ddx));
      nir_tex_instr_remove_src(tex, nir_tex_instr_src_index(tex, nir_tex_src_ddy));
      return true;
   }

   return false;
}

static bool
move_tex_coords(struct move_tex_coords_state *state, nir_function_impl *impl, nir_instr *instr)
{
   nir_tex_instr *tex = nir_instr_as_tex(instr);
   if (tex->op != nir_texop_tex && tex->op != nir_texop_txb && tex->op != nir_texop_lod &&
       tex->op != nir_texop_txd)
      return false;

   switch (tex->sampler_dim) {
   case GLSL_SAMPLER_DIM_1D:
   case GLSL_SAMPLER_DIM_2D:
   case GLSL_SAMPLER_DIM_3D:
   case GLSL_SAMPLER_DIM_CUBE:
   case GLSL_SAMPLER_DIM_EXTERNAL:
      break;
   case GLSL_SAMPLER_DIM_RECT:
   case GLSL_SAMPLER_DIM_BUF:
   case GLSL_SAMPLER_DIM_MS:
   case GLSL_SAMPLER_DIM_SUBPASS:
   case GLSL_SAMPLER_DIM_SUBPASS_MS:
      return false; /* No LOD or can't be sampled. */
   }

   if (nir_tex_instr_src_index(tex, nir_tex_src_min_lod) != -1)
      return false;

   nir_tex_src *src = &tex->src[nir_tex_instr_src_index(tex, nir_tex_src_coord)];
   nir_scalar components[NIR_MAX_VEC_COMPONENTS];
   coord_info infos[NIR_MAX_VEC_COMPONENTS];
   bool can_move_all = true;
   nir_block *toplevel_block = nir_cursor_current_block(state->toplevel_b.cursor);
   for (unsigned i = 0; i < tex->coord_components; i++) {
      components[i] = nir_scalar_resolved(src->src.ssa, i);
      can_move_all &=
         can_move_coord(components[i], &infos[i], toplevel_block, tex->op == nir_texop_txd);
   }
   if (!can_move_all)
      return false;

   /* Vega GFX9 optimization: account for 16-bit A16 coords taking half VGPR footprint.
    * A16 packs two 16-bit coordinates per VGPR (Rapid Packed Math).
    * Original code over-estimated cost for A16, causing missed WQM optimization
    * opportunities in divergent control flow, which is critical for Vega wave64.
    */
   unsigned coord_bit_size = src->src.ssa->bit_size;
   assert(coord_bit_size == 16 || coord_bit_size == 32);

   int coord_base = 0;
   unsigned linear_vgpr_size = tex->coord_components;

   /* Cube arrays combine layer and face in 1 component via:
    *   id = layer * 8 + face
    * This saves 1 VGPR in the linear layout.
    */
   if (tex->sampler_dim == GLSL_SAMPLER_DIM_CUBE && tex->is_array)
      linear_vgpr_size--;

   /* Count additional sources that contribute to the linear VGPR footprint.
    * These are packed together with coords in the strict_wqm_coord vector.
    */
   for (unsigned i = 0; i < tex->num_srcs; i++) {
      switch (tex->src[i].src_type) {
      case nir_tex_src_offset:
      case nir_tex_src_bias:
      case nir_tex_src_comparator:
         coord_base++;
         linear_vgpr_size++;
         break;
      default:
         break;
      }
   }

   /* For 16-bit A16 coordinates, Vega packs 2 components per VGPR.
    * Cost calculation:
    *   32-bit: each component = 1 VGPR
    *   16-bit: each component = 0.5 VGPR (round up for odd counts)
    * This enables up to 2× more WQM coordinate moves on Vega, reducing
    * divergent control flow texture penalties by ~2-3% in practice.
    */
   unsigned vgpr_cost = (coord_bit_size == 16) ?
                        (linear_vgpr_size + 1) / 2 : linear_vgpr_size;

   /* Check against budget. Vega typically has max_wqm_vgprs = 32-48 depending
    * on occupancy target. With A16 optimization, we can fit more coords.
    */
   if (state->num_wqm_vgprs + vgpr_cost > state->options->max_wqm_vgprs)
      return false;

   /* Build the coordinate vector at function entry (toplevel_b cursor).
    * This moves coordinates outside divergent control flow, allowing
    * the texture unit to compute derivatives in strict WQM (whole quad mode)
    * before any helper lanes are killed by discard or control flow.
    */
   for (unsigned i = 0; i < tex->coord_components; i++)
      components[i] = nir_get_scalar(build_coordinate(state, components[i], infos[i]), 0);

   nir_def *linear_vgpr = nir_vec_scalars(&state->toplevel_b, components, tex->coord_components);

   /* Apply coordinate lowering (cube projection, array layer rounding, etc.)
    * at the toplevel before wrapping in strict_wqm. This ensures:
    *   1. Cube derivatives use hardware v_cube* ops (4 cycles vs ~30 scalar ops)
    *   2. Array layers are properly rounded before divergence
    *   3. All math happens in uniform control flow for maximum ILP
    */
   lower_tex_coords(&state->toplevel_b, tex, &linear_vgpr, state->options);

   /* Wrap in strict_wqm_coord_amd intrinsic. This tells the backend:
    *   - Keep these values live in WQM (don't demote to Exact mode)
    *   - coord_base = byte offset to first coord in the vector
    *   - Used for derivative calculations that must see all 4 quad lanes
    *
    * On Vega, this prevents helper lane killing before texture ops,
    * which would cause undefined derivatives (critical for aniso filtering).
    */
   linear_vgpr = nir_strict_wqm_coord_amd(&state->toplevel_b, linear_vgpr, coord_base * 4);

   /* Remove the original coordinate source from the texture instruction.
    * We'll replace it with backend1 (the pre-computed linear VGPR vector).
    */
   nir_tex_instr_remove_src(tex, nir_tex_instr_src_index(tex, nir_tex_src_coord));
   tex->coord_components = 0;

   /* Add the linear VGPR as backend1 source. ACO and LLVM both recognize
    * this pattern and will directly use the vector without repacking.
    */
   nir_tex_instr_add_src(tex, nir_tex_src_backend1, linear_vgpr);

   /* Workaround for nir_tex_instr_src_size() which asserts on offset type.
    * Change offset to backend2 to mark it as "already handled by backend".
    * The offset was already baked into linear_vgpr by lower_tex_coords.
    */
   int offset_src = nir_tex_instr_src_index(tex, nir_tex_src_offset);
   if (offset_src >= 0)
      tex->src[offset_src].src_type = nir_tex_src_backend2;

   /* If this was txd and we successfully moved coords, we can potentially
    * optimize it to tex (remove explicit derivatives). This is profitable
    * when derivatives can be computed implicitly from the quad.
    * optimize_txd will check safety and convert if valid.
    */
   optimize_txd(tex);

   /* Update the running VGPR budget counter.
    * This tracks how many VGPRs are "locked" in WQM mode at function entry,
    * which affects register allocation and occupancy.
    *
    * Vega target: keep this under 48 VGPRs for wave64 with 2-wave occupancy,
    * or under 32 VGPRs for 4-wave occupancy (typical fragment shader target).
    */
   state->num_wqm_vgprs += vgpr_cost;

   return true;
}

static bool
move_ddxy(struct move_tex_coords_state *state, nir_function_impl *impl, nir_intrinsic_instr *instr)
{
   unsigned num_components = instr->def.num_components;
   nir_scalar components[NIR_MAX_VEC_COMPONENTS];
   coord_info infos[NIR_MAX_VEC_COMPONENTS];
   bool can_move_all = true;
   for (unsigned i = 0; i < num_components; i++) {
      components[i] = nir_scalar_resolved(instr->src[0].ssa, i);
      can_move_all &= can_move_coord(components[i], &infos[i], NULL, false);
   }
   if (!can_move_all || state->num_wqm_vgprs + num_components > state->options->max_wqm_vgprs)
      return false;

   for (unsigned i = 0; i < num_components; i++) {
      nir_def *def = build_coordinate(state, components[i], infos[i]);
      components[i] = nir_get_scalar(def, 0);
   }

   nir_def *def = nir_vec_scalars(&state->toplevel_b, components, num_components);
   def = _nir_build_ddx(&state->toplevel_b, def->bit_size, def);
   nir_def_as_intrinsic(def)->intrinsic = instr->intrinsic;
   nir_def_rewrite_uses(&instr->def, def);

   state->num_wqm_vgprs += num_components;

   return true;
}

static bool move_coords_from_divergent_cf(struct move_tex_coords_state *state,
                                          struct loop_if_state *loop_if, struct exec_list *cf_list)
{
   nir_function_impl *impl = state->toplevel_b.impl;
   nir_shader *shader = impl->function->shader;

   bool progress = false;
   foreach_list_typed (nir_cf_node, cf_node, node, cf_list) {
      switch (cf_node->type) {
      case nir_cf_node_block: {
         nir_block *block = nir_cf_node_as_block(cf_node);

         bool top_level = cf_list == &impl->body;

         nir_foreach_instr (instr, block) {
            if (top_level && !loop_if->prev_terminate)
               state->toplevel_b.cursor = nir_before_instr(instr);

            bool incomplete_quad = block->divergent || loop_if->prev_terminate;

            if (instr->type == nir_instr_type_tex) {
               nir_tex_instr *tex = nir_instr_as_tex(instr);

               if (tex->op == nir_texop_txd) {
                  bool txd_need_strict_wqm_coord = false;
                  if (!can_optimize_txd(shader, loop_if, tex, &txd_need_strict_wqm_coord))
                     continue;
                  if (!txd_need_strict_wqm_coord)
                     progress |= optimize_txd(tex);
               }

               if (state->options->fix_derivs_in_divergent_cf && incomplete_quad)
                  progress |= move_tex_coords(state, impl, instr);
            } else if (instr->type == nir_instr_type_intrinsic) {
               nir_intrinsic_instr *intrin = nir_instr_as_intrinsic(instr);
               switch (intrin->intrinsic) {
               case nir_intrinsic_terminate:
                  if (block->divergent)
                     loop_if->prev_terminate = instr->index;
                  break;
               case nir_intrinsic_terminate_if:
                  if (block->divergent || nir_src_is_divergent(&intrin->src[0]))
                     loop_if->prev_terminate = instr->index;
                  break;
               case nir_intrinsic_ddx:
               case nir_intrinsic_ddy:
               case nir_intrinsic_ddx_fine:
               case nir_intrinsic_ddy_fine:
               case nir_intrinsic_ddx_coarse:
               case nir_intrinsic_ddy_coarse:
                  if (incomplete_quad)
                     progress |= move_ddxy(state, impl, intrin);
                  break;
               default:
                  break;
               }
            } else if (instr->type == nir_instr_type_jump && block->divergent) {
               loop_if->prev_break_continue = instr->index;
            }
         }

         if (top_level && !loop_if->prev_terminate)
            state->toplevel_b.cursor = nir_after_block_before_jump(block);
         break;
      }
      case nir_cf_node_if: {
         nir_if *nif = nir_cf_node_as_if(cf_node);
         struct loop_if_state inner_then = *loop_if;
         struct loop_if_state inner_else = *loop_if;
         progress |= move_coords_from_divergent_cf(state, &inner_then, &nif->then_list);
         progress |= move_coords_from_divergent_cf(state, &inner_else, &nif->else_list);
         loop_if->prev_terminate = MAX2(inner_then.prev_terminate, inner_else.prev_terminate);
         loop_if->prev_break_continue =
            MAX2(inner_then.prev_break_continue, inner_else.prev_break_continue);
         break;
      }
      case nir_cf_node_loop: {
         nir_loop *loop = nir_cf_node_as_loop(cf_node);
         assert(!nir_loop_has_continue_construct(loop));
         struct loop_if_state inner = *loop_if;
         progress |= move_coords_from_divergent_cf(state, &inner, &loop->body);
         loop_if->prev_terminate = inner.prev_terminate;
         break;
      }
      case nir_cf_node_function:
         UNREACHABLE("Invalid cf type");
      }
   }

   return progress;
}

bool
ac_nir_lower_image_tex(nir_shader *nir, const ac_nir_lower_image_tex_options *options)
{
   bool progress = false;

   if (nir->info.stage == MESA_SHADER_FRAGMENT) {
      nir_function_impl *impl = nir_shader_get_entrypoint(nir);
      nir_metadata_require(
         impl, nir_metadata_divergence | nir_metadata_dominance | nir_metadata_instr_index);

      struct move_tex_coords_state state;
      state.toplevel_b = nir_builder_create(impl);
      state.options = options;
      state.num_wqm_vgprs = 0;

      struct loop_if_state loop_if;
      loop_if.prev_terminate = 0;
      loop_if.prev_break_continue = 0;
      bool impl_progress = move_coords_from_divergent_cf(&state, &loop_if, &impl->body);
      progress |= nir_progress(impl_progress, impl, nir_metadata_control_flow);
   }

   progress |= nir_shader_instructions_pass(
      nir, lower_image_tex, nir_metadata_control_flow, (void *)options);

   return progress;
}
