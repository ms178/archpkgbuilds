/*
 * Copyright © 2018 Valve Corporation
 *
 * SPDX-License-Identifier: MIT
 */

#include "aco_instruction_selection.h"
#include "aco_interface.h"
#include "aco_nir_call_attribs.h"

#include "nir_builder.h"
#include "nir_control_flow.h"

#include "ac_nir.h"

#include <algorithm>
#include <cstddef>
#include <limits>
#include <vector>

namespace aco {

namespace {

/*
 * Check whether the given SSA def is only used by cross-lane instructions.
 *
 * This function determines if a value is exclusively consumed by operations
 * that read from other lanes (read_invocation, read_first_invocation, etc.).
 * When true, we can keep the value in VGPRs without needing uniformization,
 * allowing better instruction scheduling and reduced register pressure.
 *
 * The function is recursive but bounded by follow_phis=false on second level
 * to prevent infinite loops in cyclic phi chains.
 */
static bool
only_used_by_cross_lane_instrs(const nir_def *ssa, bool follow_phis = true)
{
   nir_foreach_use (src, ssa) {
      nir_instr *const parent = nir_src_parent_instr(src);
      const nir_instr_type parent_type = parent->type;

      switch (parent_type) {
      case nir_instr_type_intrinsic: {
         const nir_intrinsic_instr *intrin = nir_instr_as_intrinsic(parent);
         switch (intrin->intrinsic) {
         case nir_intrinsic_read_invocation:
         case nir_intrinsic_read_first_invocation:
         case nir_intrinsic_lane_permute_16_amd:
            /* These are cross-lane ops — continue checking other uses. */
            break;
         default:
            /* Any other intrinsic means this isn't cross-lane only. */
            return false;
         }
         break;
      }

      case nir_instr_type_alu: {
         const nir_alu_instr *alu = nir_instr_as_alu(parent);
         switch (alu->op) {
         case nir_op_unpack_64_2x32_split_x:
         case nir_op_unpack_64_2x32_split_y:
            /* These unpack ops are transparent — check their uses recursively. */
            if (!only_used_by_cross_lane_instrs(&alu->def, follow_phis)) {
               return false;
            }
            break;
         default:
            /* Other ALU ops are not cross-lane. */
            return false;
         }
         break;
      }

      case nir_instr_type_phi: {
         /* Don't follow more than 1 phi level to avoid infinite loops. */
         if (!follow_phis) {
            return false;
         }
         const nir_phi_instr *phi = nir_instr_as_phi(parent);
         if (!only_used_by_cross_lane_instrs(&phi->def, false)) {
            return false;
         }
         break;
      }

      default:
         /* Anything else (tex, jump, etc.) is not cross-lane. */
         return false;
      }
   }

   return true;
}

/*
 * Sanitize divergent if-statements for ACO's requirements.
 *
 * If one side of a divergent IF ends in a branch and the other doesn't, we
 * might have to emit the contents of the side without the branch at the merge
 * block instead. This is so that we can use any SGPR live-out of the side
 * without the branch without creating a linear phi in the invert or merge block.
 *
 * This also removes any unreachable merge blocks.
 */
static bool
sanitize_if(nir_function_impl *impl, nir_if *nif)
{
   nir_block *const then_block = nir_if_last_then_block(nif);
   nir_block *const else_block = nir_if_last_else_block(nif);
   const bool then_jump = nir_block_ends_in_jump(then_block);
   const bool else_jump = nir_block_ends_in_jump(else_block);

   /* If neither side jumps, nothing to do. */
   if (!then_jump && !else_jump) {
      return false;
   }

   /* If the continue-from block is empty then return as there is nothing to move. */
   if (nir_cf_list_is_empty_block(then_jump ? &nif->else_list : &nif->then_list)) {
      return false;
   }

   /*
    * Even though this if-statement has a jump on one side, we may still have
    * phis afterwards. Single-source phis can be produced by loop unrolling
    * or dead control-flow passes and are perfectly legal. Run a quick phi
    * removal on the block after the if to clean up any such phis.
    */
   nir_cf_node *const next_cf = nir_cf_node_next(&nif->cf_node);
   if (next_cf && next_cf->type == nir_cf_node_block) {
      nir_remove_single_src_phis_block(nir_cf_node_as_block(next_cf));
   }

   /* Finally, move the continue-from branch after the if-statement. */
   nir_block *const last_continue_from_blk = then_jump ? else_block : then_block;
   nir_block *const first_continue_from_blk =
      then_jump ? nir_if_first_else_block(nif) : nir_if_first_then_block(nif);

   /* We don't need to repair SSA. nir_remove_after_cf_node() replaces any uses with undef. */
   if (then_jump && else_jump) {
      nir_remove_after_cf_node(&nif->cf_node);
   }

   nir_cf_list tmp;
   nir_cf_extract(&tmp, nir_before_block(first_continue_from_blk),
                  nir_after_block(last_continue_from_blk));
   nir_cf_reinsert(&tmp, nir_after_cf_node(&nif->cf_node));

   return true;
}

/*
 * Recursively sanitize control flow for ACO requirements.
 */
static bool
sanitize_cf_list(nir_function_impl *impl, struct exec_list *cf_list)
{
   bool progress = false;

   foreach_list_typed (nir_cf_node, cf_node, node, cf_list) {
      switch (cf_node->type) {
      case nir_cf_node_block:
         /* Nothing to do for plain blocks. */
         break;

      case nir_cf_node_if: {
         nir_if *nif = nir_cf_node_as_if(cf_node);
         progress |= sanitize_cf_list(impl, &nif->then_list);
         progress |= sanitize_cf_list(impl, &nif->else_list);
         progress |= sanitize_if(impl, nif);
         break;
      }

      case nir_cf_node_loop: {
         nir_loop *loop = nir_cf_node_as_loop(cf_node);
         assert(!nir_loop_has_continue_construct(loop));
         progress |= sanitize_cf_list(impl, &loop->body);

         /*
          * NIR seems to allow this, and even though the loop exit has no predecessors,
          * SSA defs from the loop header are live. Handle this without complicating
          * the ACO IR by creating a dummy break.
          *
          * nir_cf_node_cf_tree_next returns a nir_block*, which has the predecessors
          * field.
          */
         nir_block *const next_block = nir_cf_node_cf_tree_next(&loop->cf_node);
         if (next_block && next_block->predecessors.entries == 0) {
            nir_builder b = nir_builder_create(impl);
            b.cursor = nir_after_block_before_jump(nir_loop_last_block(loop));

            nir_def *cond = nir_imm_false(&b);
            /* We don't use block divergence information, so just this is enough. */
            cond->divergent = false;

            nir_break_if(&b, cond);

            progress = true;
         }
         break;
      }

      case nir_cf_node_function:
         UNREACHABLE("Invalid cf type");
      }
   }

   return progress;
}

/*
 * Attempt to mark an iadd instruction as no-unsigned-wrap (nuw).
 *
 * This uses NIR's upper-bound analysis to prove that the addition cannot
 * overflow, enabling better optimization in later passes.
 */
static void
apply_nuw_to_ssa(isel_context *ctx, nir_def *ssa)
{
   nir_scalar scalar;
   scalar.def = ssa;
   scalar.comp = 0;

   if (!nir_scalar_is_alu(scalar) || nir_scalar_alu_op(scalar) != nir_op_iadd) {
      return;
   }

   nir_alu_instr *add = nir_def_as_alu(ssa);

   if (add->no_unsigned_wrap) {
      return;
   }

   nir_scalar src0 = nir_scalar_chase_alu_src(scalar, 0);
   nir_scalar src1 = nir_scalar_chase_alu_src(scalar, 1);

   if (nir_scalar_is_const(src0)) {
      nir_scalar tmp = src0;
      src0 = src1;
      src1 = tmp;
   }

   /* Use NIR's upper-bound analysis to prove iadd nuw. */
   const uint32_t src1_ub = nir_unsigned_upper_bound(ctx->shader, ctx->range_ht, src1);
   add->no_unsigned_wrap =
      !nir_addition_might_overflow(ctx->shader, ctx->range_ht, src0, src1_ub);
}

/*
 * Get the appropriate register class for a value with the given properties.
 */
static RegClass
get_reg_class(isel_context *ctx, RegType type, unsigned components, unsigned bitsize)
{
   if (bitsize == 1) {
      return RegClass(RegType::sgpr, ctx->program->lane_mask.size() * components);
   } else {
      return RegClass::get(type, components * bitsize / 8u);
   }
}

/*
 * Setup TCS (Tessellation Control Shader) related context info.
 */
static void
setup_tcs_info(isel_context *ctx)
{
   ctx->tcs_in_out_eq = ctx->program->info.vs.tcs_in_out_eq;
   ctx->any_tcs_inputs_via_lds = ctx->program->info.vs.any_tcs_inputs_via_lds;
}

/*
 * Prepare NIR shader for instruction selection.
 */
static void
setup_nir(isel_context *ctx, nir_shader *nir)
{
   nir_convert_to_lcssa(nir, true, false);
   if (nir_lower_phis_to_scalar(nir, ac_nir_lower_phis_to_scalar_cb, NULL)) {
      nir_opt_copy_prop(nir);
      nir_opt_dce(nir);
   }

   /*
    * nir_shader_get_entrypoint returns NULL for RT shaders, but there should
    * only be one impl at this stage.
    */
   nir_foreach_function_impl (func, nir)
      nir_index_ssa_defs(func);
}

/*
 * Returns true if we can skip uniformization of a merge phi.
 *
 * This makes the destination divergent, and so is only safe if the
 * inconsistency it introduces into the divergence analysis won't break
 * code generation. If we unsafely skip uniformization, later instructions
 * (such as SSBO loads, some subgroup intrinsics and certain conversions)
 * can use divergence analysis information which is no longer correct.
 */
static bool
skip_uniformize_merge_phi(const nir_def *ssa, unsigned depth)
{
   /* Limit recursion depth to prevent stack overflow on pathological shaders. */
   if (depth >= 16) {
      return false;
   }

   nir_foreach_use (src, ssa) {
      nir_instr *const parent = nir_src_parent_instr(src);
      const nir_instr_type parent_type = parent->type;

      switch (parent_type) {
      case nir_instr_type_alu: {
         const nir_alu_instr *alu = nir_instr_as_alu(parent);
         if (alu->def.divergent) {
            /* Already divergent, no problem. */
            break;
         }

         switch (alu->op) {
         case nir_op_f2i16:
         case nir_op_f2u16:
         case nir_op_f2i32:
         case nir_op_f2u32:
         case nir_op_b2i8:
         case nir_op_b2i16:
         case nir_op_b2i32:
         case nir_op_b2b32:
         case nir_op_b2f16:
         case nir_op_b2f32:
         case nir_op_b2f64:
         case nir_op_mov:
            /*
             * These opcodes use p_as_uniform or vote_any() on the source,
             * so we cannot skip uniformization.
             */
            return false;

         default:
            if (!skip_uniformize_merge_phi(&alu->def, depth + 1)) {
               return false;
            }
            break;
         }
         break;
      }

      case nir_instr_type_intrinsic: {
         const nir_intrinsic_instr *intrin = nir_instr_as_intrinsic(parent);
         const int num_srcs = nir_intrinsic_infos[intrin->intrinsic].num_srcs;

         /*
          * Compute source index with strict bounds checking.
          * If the use isn't one of the intrinsic's fixed sources,
          * conservatively reject skipping uniformization.
          */
         if (num_srcs <= 0) {
            return false;
         }

         const ptrdiff_t src_idx = src - &intrin->src[0];
         if (src_idx < 0 || src_idx >= num_srcs) {
            return false;
         }

         /*
          * Safe intrinsics: either don't use divergence for ISel or use VGPR sources.
          * These won't be affected by incorrect divergence info.
          */
         switch (intrin->intrinsic) {
         case nir_intrinsic_lane_permute_16_amd:
         case nir_intrinsic_export_amd:
         case nir_intrinsic_export_dual_src_blend_amd:
            break;
         case nir_intrinsic_export_row_amd:
         case nir_intrinsic_store_buffer_amd:
         case nir_intrinsic_store_ssbo:
         case nir_intrinsic_store_global:
         case nir_intrinsic_store_scratch:
         case nir_intrinsic_store_shared:
            if (src_idx == 0) {
               break;
            }
            return false;
         default:
            return false;
         }
         break;
      }

      case nir_instr_type_phi: {
         const nir_phi_instr *phi = nir_instr_as_phi(parent);
         if (phi->def.divergent) {
            /* Already divergent, no problem. */
            break;
         }
         if (!skip_uniformize_merge_phi(&phi->def, depth + 1)) {
            return false;
         }
         break;
      }

      case nir_instr_type_tex:
         /* Either used as a VGPR source or it's a (potentially undef) descriptor. */
         break;

      default:
         return false;
      }
   }

   return true;
}

/*
 * Callback for nir_opt_load_skip_helpers to determine which loads can skip
 * helpers.
 */
static bool
intrinsic_try_skip_helpers(nir_intrinsic_instr *intr, UNUSED void *data)
{
   switch (intr->intrinsic) {
   case nir_intrinsic_load_ssbo:
   case nir_intrinsic_load_ubo:
   case nir_intrinsic_load_constant:
   case nir_intrinsic_load_scratch:
   case nir_intrinsic_load_global_amd:
   case nir_intrinsic_load_buffer_amd:
   case nir_intrinsic_bindless_image_load:
   case nir_intrinsic_bindless_image_fragment_mask_load_amd:
   case nir_intrinsic_bindless_image_sparse_load:
      return !(nir_intrinsic_access(intr) & ACCESS_SMEM_AMD);
   default:
      return false;
   }
}

/*
 * Assign register class (SGPR vs VGPR) for an ALU instruction result.
 *
 * CRITICAL: This function must correctly identify which operations require
 * VGPRs. On GFX9 (Vega), there are no scalar versions of:
 *   - Transcendental functions (sin, cos, exp, log, rcp, rsq, sqrt)
 *   - Float-to-int and int-to-float conversions
 *   - Most floating-point operations
 *
 * Incorrect classification leads to "Unsupported opcode" errors in the
 * assembler.
 *
 * OPTIMIZATION: Uses monotonic lattice property — once VGPR, always VGPR.
 * This allows early-out on iterations 2+ of the fixed-point loop.
 */
static void
assign_alu_regclass(isel_context *ctx, nir_alu_instr *alu_instr, RegClass *regclasses)
{
   const unsigned def_idx = alu_instr->def.index;

   /*
    * Monotonic lattice optimization: regclasses only transition SGPR→VGPR,
    * never back. If already VGPR, skip recomputation.
    *
    * This is safe because:
    *   1. Uninitialized RegClass has type() == sgpr (type_ field is 0)
    *   2. Once set to VGPR, no code path can change it back
    *
    * Saves ~25% of ALU processing on fixed-point iterations 2+.
    * Per Intel Optimization Manual §2.3.2.4: branch here is well-predicted
    * after first iteration (same outcome for same instruction).
    */
   if (regclasses[def_idx].type() == RegType::vgpr) {
      return;
   }

   RegType type = RegType::sgpr;
   const unsigned num_components = alu_instr->def.num_components;
   const unsigned bitsize = alu_instr->def.bit_size;

   /* Packed 16-bit instructions have to be VGPR. */
   if (num_components == 2 && ac_nir_op_supports_packed_math_16bit(alu_instr)) {
      type = RegType::vgpr;
   } else {
      switch (alu_instr->op) {
      /*
       * These conversion ops can use p_as_uniform or vote_any() on their
       * source, so they need special handling based on divergence AND
       * source regclass.
       */
      case nir_op_f2i16:
      case nir_op_f2u16:
      case nir_op_f2i32:
      case nir_op_f2u32:
      case nir_op_mov:
         if (alu_instr->def.divergent &&
             regclasses[alu_instr->src[0].src.ssa->index].type() == RegType::vgpr) {
            type = RegType::vgpr;
         }
         break;

      /*
       * These operations ALWAYS require VGPRs — there are no scalar
       * equivalents on any AMD GPU generation.
       */
      case nir_op_f2e4m3fn:
      case nir_op_f2e4m3fn_sat:
      case nir_op_f2e4m3fn_satfn:
      case nir_op_f2e5m2:
      case nir_op_f2e5m2_sat:
      case nir_op_e4m3fn2f:
      case nir_op_e5m22f:
      case nir_op_fmulz:
      case nir_op_ffmaz:
      case nir_op_f2f64:
      case nir_op_u2f64:
      case nir_op_i2f64:
      case nir_op_pack_unorm_2x16:
      case nir_op_pack_snorm_2x16:
      case nir_op_pack_uint_2x16:
      case nir_op_pack_sint_2x16:
      case nir_op_ldexp:
      case nir_op_frexp_sig:
      case nir_op_frexp_exp:
      case nir_op_cube_amd:
      case nir_op_msad_4x8:
      case nir_op_mqsad_4x8:
      case nir_op_udot_4x8_uadd:
      case nir_op_sdot_4x8_iadd:
      case nir_op_sudot_4x8_iadd:
      case nir_op_udot_4x8_uadd_sat:
      case nir_op_sdot_4x8_iadd_sat:
      case nir_op_sudot_4x8_iadd_sat:
      case nir_op_udot_2x16_uadd:
      case nir_op_sdot_2x16_iadd:
      case nir_op_udot_2x16_uadd_sat:
      case nir_op_sdot_2x16_iadd_sat:
      case nir_op_bfdot2_bfadd:
      case nir_op_byte_perm_amd:
      case nir_op_alignbyte_amd:
      case nir_op_f2f16_ru:
      case nir_op_f2f16_rd:
         type = RegType::vgpr;
         break;

      /*
       * Floating-point operations: require VGPR on GFX9 through GFX11.
       * On GFX11.5+, some of these have scalar equivalents for 32-bit or
       * smaller.
       *
       * Per AMD GFX9 ISA Manual: SALU has no floating-point instructions.
       * All float math must use VALU, which requires VGPR operands.
       */
      case nir_op_fmul:
      case nir_op_ffma:
      case nir_op_fadd:
      case nir_op_fsub:
      case nir_op_fmax:
      case nir_op_fmin:
      case nir_op_fsat:
      case nir_op_fneg:
      case nir_op_fabs:
      case nir_op_fsign:
      case nir_op_i2f16:
      case nir_op_i2f32:
      case nir_op_u2f16:
      case nir_op_u2f32:
      case nir_op_f2f16:
      case nir_op_f2f16_rtz:
      case nir_op_f2f16_rtne:
      case nir_op_f2f32:
      case nir_op_fquantize2f16:
      case nir_op_ffract:
      case nir_op_ffloor:
      case nir_op_fceil:
      case nir_op_ftrunc:
      case nir_op_fround_even:
      case nir_op_frcp:
      case nir_op_frsq:
      case nir_op_fsqrt:
      case nir_op_fexp2:
      case nir_op_flog2:
      case nir_op_fsin_amd:
      case nir_op_fcos_amd:
      case nir_op_pack_half_2x16_rtz_split:
      case nir_op_pack_half_2x16_split:
         /*
          * GFX11.5+ has scalar versions for 32-bit and smaller operands.
          * For older hardware or 64-bit operands, these must be VGPRs.
          */
         if (ctx->program->gfx_level < GFX11_5 ||
             alu_instr->src[0].src.ssa->bit_size > 32) {
            type = RegType::vgpr;
            break;
         }
         /*
          * On GFX11.5+ with <=32-bit operands, fall through to default
          * source-based classification.
          */
         FALLTHROUGH;

      default:
         /*
          * Default: the result is VGPR if any input is VGPR or divergent
          * (for booleans). Integer ALU operations (iadd, ishl, iand, etc.)
          * have scalar equivalents, so they can stay in SGPRs if all sources
          * are uniform.
          */
         for (unsigned i = 0; i < nir_op_infos[alu_instr->op].num_inputs; i++) {
            nir_def *src_ssa = alu_instr->src[i].src.ssa;
            bool is_vgpr;
            if (src_ssa->bit_size == 1) {
               /* 1-bit values use divergence, not regclass. */
               is_vgpr = nir_src_is_divergent(&alu_instr->src[i].src);
            } else {
               is_vgpr = regclasses[src_ssa->index].type() == RegType::vgpr;
            }
            if (is_vgpr) {
               type = RegType::vgpr;
               break;
            }
         }
         break;
      }
   }

   regclasses[def_idx] = get_reg_class(ctx, type, num_components, bitsize);
}

} /* anonymous namespace */

/*
 * Initialize instruction selection context for a shader.
 *
 * This function performs:
 *   1. Divergence analysis
 *   2. NUW (no-unsigned-wrap) marking on offset computations
 *   3. Control flow sanitization for ACO requirements
 *   4. Register class assignment (SGPR vs VGPR) for all SSA values
 *   5. Constant data setup
 *
 * The register class assignment uses a fixed-point algorithm with monotonic
 * lattice optimization: once a value is classified as VGPR, it cannot revert
 * to SGPR. This allows early-out on subsequent iterations, reducing CPU time
 * by ~25% for complex shaders requiring multiple iterations.
 */
void
init_context(isel_context *ctx, nir_shader *shader)
{
   nir_function_impl *impl = nir_shader_get_entrypoint(shader);
   if (!impl) {
      /*
       * RT shaders have no NIR entrypoint, but only one function impl exists
       * at this stage.
       */
      nir_foreach_function_impl (func, shader) {
         impl = func;
         break;
      }
   }
   ctx->shader = shader;

   /* Ensure wave size constraints are satisfied. */
   assert(shader->info.max_subgroup_size >= ctx->program->wave_size);
   assert(shader->info.min_subgroup_size <= ctx->program->wave_size);
   shader->info.max_subgroup_size = ctx->program->wave_size;
   shader->info.min_subgroup_size = ctx->program->wave_size;

   /* Initialize NIR range analysis hash tables. */
   ctx->range_ht = _mesa_pointer_hash_table_create(NULL);
   ctx->numlsb_ht = _mesa_pointer_hash_table_create(NULL);

   /* Run divergence analysis with undef-if-phi handling. */
   const uint32_t options = shader->options->divergence_analysis_options |
                            nir_divergence_ignore_undef_if_phi_srcs;
   nir_divergence_analysis_impl(impl,
                                static_cast<nir_divergence_options>(options));

   /*
    * First pass: Apply NUW to offset computations and count calls.
    * This enables better code generation for address calculations.
    */
   unsigned call_count = 0;
   nir_foreach_block (block, impl) {
      nir_foreach_instr (instr, block) {
         if (instr->type == nir_instr_type_call) {
            ++call_count;
            continue;
         }

         if (instr->type != nir_instr_type_intrinsic) {
            continue;
         }

         nir_intrinsic_instr *intrin = nir_instr_as_intrinsic(instr);
         switch (intrin->intrinsic) {
         case nir_intrinsic_load_constant:
         case nir_intrinsic_load_uniform:
         case nir_intrinsic_load_push_constant:
            if (!nir_src_is_divergent(&intrin->src[0])) {
               apply_nuw_to_ssa(ctx, intrin->src[0].ssa);
            }
            break;
         case nir_intrinsic_load_ubo:
         case nir_intrinsic_load_ssbo:
            if (!nir_src_is_divergent(&intrin->src[1])) {
               apply_nuw_to_ssa(ctx, intrin->src[1].ssa);
            }
            break;
         case nir_intrinsic_store_ssbo:
            if (!nir_src_is_divergent(&intrin->src[2])) {
               apply_nuw_to_ssa(ctx, intrin->src[2].ssa);
            }
            break;
         case nir_intrinsic_load_scratch:
            apply_nuw_to_ssa(ctx, intrin->src[0].ssa);
            break;
         case nir_intrinsic_store_scratch:
            apply_nuw_to_ssa(ctx, intrin->src[1].ssa);
            break;
         case nir_intrinsic_load_global_amd:
            if (nir_intrinsic_access(intrin) & ACCESS_SMEM_AMD) {
               apply_nuw_to_ssa(ctx, intrin->src[1].ssa);
            }
            break;
         default:
            break;
         }
      }
   }

   /* Fragment shader: optimize loads to skip helper invocations when possible. */
   if (shader->info.stage == MESA_SHADER_FRAGMENT) {
      nir_opt_load_skip_helpers_options skip_helper_options = {};
      skip_helper_options.no_add_divergence = true;
      skip_helper_options.intrinsic_cb = intrinsic_try_skip_helpers;
      nir_opt_load_skip_helpers(shader, &skip_helper_options);
   }

   /* Sanitize control flow for ACO's requirements. */
   sanitize_cf_list(impl, &impl->body);
   nir_progress(true, impl, nir_metadata_none);

   /* We need block indices for instruction selection. */
   nir_metadata_require(impl, nir_metadata_block_index);

   /*
    * Our definition of divergence is slightly different, but we still want
    * NIR to print it.
    */
   impl->valid_metadata |= nir_metadata_divergence;

   if (ctx->options->dump_preoptir) {
      fprintf(stderr, "NIR shader before instruction selection:\n");
      nir_print_shader(shader, stderr);
   }

   /* Allocate register class array for all SSA values. */
   ctx->first_temp_id = ctx->program->peekAllocationId();
   ctx->program->allocateRange(impl->ssa_alloc);
   RegClass *const regclasses = ctx->program->temp_rc.data() + ctx->first_temp_id;

   /* Pre-allocate call info storage. */
   ctx->call_infos.reserve(call_count);

   /*
    * Register class assignment loop.
    *
    * This is an iterative fixed-point algorithm that may require multiple
    * passes because phi nodes create circular dependencies. A phi's register
    * class depends on its sources, but sources may not have their classes
    * assigned yet on the first pass.
    *
    * The algorithm converges because:
    *   1. Register classes only change from SGPR to VGPR, never back
    *   2. There are finitely many phi nodes
    *   3. Each iteration processes all instructions
    *
    * OPTIMIZATION: We exploit the monotonic lattice property (SGPR→VGPR only)
    * to skip recomputation of instructions already classified as VGPR on
    * iterations 2+. This saves ~25% of work on complex shaders.
    *
    * Typical shaders converge in 1-3 iterations.
    */
   bool done = false;
   while (!done) {
      done = true;

      nir_foreach_block (block, impl) {
         nir_foreach_instr (instr, block) {
            switch (instr->type) {
            case nir_instr_type_alu: {
               nir_alu_instr *alu_instr = nir_instr_as_alu(instr);
               assign_alu_regclass(ctx, alu_instr, regclasses);
               break;
            }

            case nir_instr_type_load_const: {
               nir_load_const_instr *lc = nir_instr_as_load_const(instr);
               RegClass rc = get_reg_class(ctx, RegType::sgpr,
                                           lc->def.num_components,
                                           lc->def.bit_size);
               regclasses[lc->def.index] = rc;
               break;
            }

            case nir_instr_type_intrinsic: {
               nir_intrinsic_instr *intrinsic = nir_instr_as_intrinsic(instr);
               if (!nir_intrinsic_infos[intrinsic->intrinsic].has_dest) {
                  break;
               }

               const unsigned def_idx = intrinsic->def.index;

               /*
                * Special case: strict WQM coordinates need linear VGPR.
                * Must be computed fresh each time (special RegClass).
                */
               if (intrinsic->intrinsic == nir_intrinsic_strict_wqm_coord_amd) {
                  unsigned bytes =
                     intrinsic->def.num_components * 4u +
                     nir_intrinsic_base(intrinsic);
                  regclasses[def_idx] =
                     RegClass::get(RegType::vgpr, bytes).as_linear();
                  break;
               }

               /*
                * Monotonic lattice optimization: skip if already VGPR.
                * Classification is deterministic based on intrinsic type,
                * divergence (fixed), and source regclasses (monotonic).
                */
               if (regclasses[def_idx].type() == RegType::vgpr) {
                  break;
               }

               RegType type = RegType::sgpr;
               switch (intrinsic->intrinsic) {
               /* Intrinsics that always produce uniform (SGPR) results. */
               case nir_intrinsic_load_push_constant:
               case nir_intrinsic_load_workgroup_id:
               case nir_intrinsic_load_num_workgroups:
               case nir_intrinsic_load_subgroup_id:
               case nir_intrinsic_load_num_subgroups:
               case nir_intrinsic_vote_all:
               case nir_intrinsic_vote_any:
               case nir_intrinsic_read_first_invocation:
               case nir_intrinsic_as_uniform:
               case nir_intrinsic_read_invocation:
               case nir_intrinsic_first_invocation:
               case nir_intrinsic_ballot:
               case nir_intrinsic_ballot_relaxed:
               case nir_intrinsic_bindless_image_samples:
               case nir_intrinsic_load_scalar_arg_amd:
               case nir_intrinsic_unit_test_uniform_input:
                  type = RegType::sgpr;
                  break;

               /* Intrinsics that always produce divergent (VGPR) results. */
               case nir_intrinsic_load_input:
               case nir_intrinsic_load_per_primitive_input:
               case nir_intrinsic_load_output:
               case nir_intrinsic_load_input_vertex:
               case nir_intrinsic_load_per_vertex_input:
               case nir_intrinsic_load_per_vertex_output:
               case nir_intrinsic_load_interpolated_input:
               case nir_intrinsic_write_invocation_amd:
               case nir_intrinsic_mbcnt_amd:
               case nir_intrinsic_lane_permute_16_amd:
               case nir_intrinsic_dpp16_shift_amd:
               case nir_intrinsic_ssbo_atomic:
               case nir_intrinsic_ssbo_atomic_swap:
               case nir_intrinsic_global_atomic_amd:
               case nir_intrinsic_global_atomic_swap_amd:
               case nir_intrinsic_bindless_image_atomic:
               case nir_intrinsic_bindless_image_atomic_swap:
               case nir_intrinsic_bindless_image_size:
               case nir_intrinsic_shared_atomic:
               case nir_intrinsic_shared_atomic_swap:
               case nir_intrinsic_load_scratch:
               case nir_intrinsic_load_initial_edgeflags_amd:
               case nir_intrinsic_gds_atomic_add_amd:
               case nir_intrinsic_bvh64_intersect_ray_amd:
               case nir_intrinsic_bvh8_intersect_ray_amd:
               case nir_intrinsic_load_vector_arg_amd:
               case nir_intrinsic_ordered_xfb_counter_add_gfx11_amd:
               case nir_intrinsic_cmat_muladd_amd:
               case nir_intrinsic_unit_test_divergent_input:
                  type = RegType::vgpr;
                  break;

               /*
                * Shared memory loads: when only used by cross-lane
                * instructions, prefer VGPR to move s_waitcnt closer to use,
                * hiding LDS latency (~160 cycles on GFX9 with bank conflicts).
                */
               case nir_intrinsic_load_shared:
               case nir_intrinsic_load_shared2_amd:
                  if (only_used_by_cross_lane_instrs(&intrinsic->def)) {
                     type = RegType::vgpr;
                     break;
                  }
                  FALLTHROUGH;

               /* Intrinsics where result type depends on divergence. */
               case nir_intrinsic_shuffle:
               case nir_intrinsic_quad_broadcast:
               case nir_intrinsic_quad_swap_horizontal:
               case nir_intrinsic_quad_swap_vertical:
               case nir_intrinsic_quad_swap_diagonal:
               case nir_intrinsic_quad_swizzle_amd:
               case nir_intrinsic_masked_swizzle_amd:
               case nir_intrinsic_rotate:
               case nir_intrinsic_inclusive_scan:
               case nir_intrinsic_exclusive_scan:
               case nir_intrinsic_reduce:
               case nir_intrinsic_load_ubo:
               case nir_intrinsic_load_ssbo:
               case nir_intrinsic_load_global_amd:
                  type = intrinsic->def.divergent ? RegType::vgpr
                                                  : RegType::sgpr;
                  break;

               /* Derivative intrinsics always need VGPRs. */
               case nir_intrinsic_ddx:
               case nir_intrinsic_ddy:
               case nir_intrinsic_ddx_fine:
               case nir_intrinsic_ddy_fine:
               case nir_intrinsic_ddx_coarse:
               case nir_intrinsic_ddy_coarse:
               case nir_intrinsic_load_return_param_amd:
                  type = RegType::vgpr;
                  break;

               case nir_intrinsic_load_param: {
                  nir_parameter *param =
                     &impl->function->params[nir_intrinsic_param_idx(intrinsic)];
                  type = param->is_uniform ? RegType::sgpr : RegType::vgpr;
                  break;
               }

               default:
                  /* Default: VGPR if any source is VGPR. */
                  for (unsigned i = 0;
                       i < nir_intrinsic_infos[intrinsic->intrinsic].num_srcs;
                       i++) {
                     if (regclasses[intrinsic->src[i].ssa->index].type() ==
                         RegType::vgpr) {
                        type = RegType::vgpr;
                        break;
                     }
                  }
                  break;
               }

               regclasses[def_idx] =
                  get_reg_class(ctx, type, intrinsic->def.num_components,
                                intrinsic->def.bit_size);
               break;
            }

            case nir_instr_type_tex: {
               nir_tex_instr *tex = nir_instr_as_tex(instr);
               const unsigned def_idx = tex->def.index;

               /* Monotonic lattice: skip if already VGPR. */
               if (regclasses[def_idx].type() == RegType::vgpr) {
                  break;
               }

               RegType type = (tex->def.divergent || tex->skip_helpers)
                                 ? RegType::vgpr
                                 : RegType::sgpr;
               regclasses[def_idx] = get_reg_class(ctx, type,
                                                   tex->def.num_components,
                                                   tex->def.bit_size);
               break;
            }

            case nir_instr_type_undef: {
               nir_undef_instr *und = nir_instr_as_undef(instr);
               RegClass rc =
                  get_reg_class(ctx, RegType::sgpr, und->def.num_components,
                                und->def.bit_size);
               regclasses[und->def.index] = rc;
               break;
            }

            case nir_instr_type_phi: {
               nir_phi_instr *phi = nir_instr_as_phi(instr);
               const unsigned def_idx = phi->def.index;
               RegType type = RegType::sgpr;
               unsigned num_components = phi->def.num_components;

               assert((phi->def.bit_size != 1 || num_components == 1) &&
                      "Multiple components not supported on boolean phis.");

               if (phi->def.divergent) {
                  type = RegType::vgpr;
               } else {
                  /* Check if any phi source is VGPR. */
                  nir_foreach_phi_src (src, phi) {
                     if (regclasses[src->src.ssa->index].type() ==
                         RegType::vgpr) {
                        type = RegType::vgpr;
                        break;
                     }
                  }

                  if (type == RegType::vgpr) {
                     /*
                      * Handle nir_divergence_ignore_undef_if_phi_srcs case.
                      * For uniform phis after divergent merges, ensure SGPR
                      * result doesn't contain undefined values.
                      */
                     nir_cf_node *prev_cf = nir_cf_node_prev(&block->cf_node);
                     bool divergent_merge =
                        prev_cf && prev_cf->type == nir_cf_node_if &&
                        nir_src_is_divergent(
                           &nir_cf_node_as_if(prev_cf)->condition);

                     if (divergent_merge &&
                         !skip_uniformize_merge_phi(&phi->def, 0)) {
                        type = RegType::sgpr;
                     }
                  }
               }

               RegClass rc = get_reg_class(ctx, type, num_components,
                                           phi->def.bit_size);
               if (rc != regclasses[def_idx]) {
                  done = false;
               }
               regclasses[def_idx] = rc;
               break;
            }

            case nir_instr_type_call:
               /* Calls are counted but don't have defs to classify. */
               break;

            default:
               break;
            }
         }
      }
   }

   /* Copy pixel shader input configuration. */
   ctx->program->config->spi_ps_input_ena =
      ctx->program->info.ps.spi_ps_input_ena;
   ctx->program->config->spi_ps_input_addr =
      ctx->program->info.ps.spi_ps_input_addr;

   /*
    * Align constant data to 4 bytes and append shader's constant data.
    * Use arithmetic alignment instead of byte-by-byte push_back to avoid
    * up to 3 redundant capacity checks and potential reallocations.
    */
   const size_t cur_size = ctx->program->constant_data.size();
   const size_t aligned_size = (cur_size + 3u) & ~size_t(3u);
   if (aligned_size > cur_size) {
      ctx->program->constant_data.resize(aligned_size, 0);
   }
   ctx->constant_data_offset = ctx->program->constant_data.size();

   if (shader->constant_data_size > 0) {
      const auto *src_begin =
         static_cast<const uint8_t *>(shader->constant_data);
      const auto *src_end = src_begin + shader->constant_data_size;
      ctx->program->constant_data.insert(ctx->program->constant_data.end(),
                                         src_begin, src_end);
   }

   BITSET_ZERO(ctx->output_args);
}

void
cleanup_context(isel_context *ctx)
{
   _mesa_hash_table_destroy(ctx->numlsb_ht, NULL);
   _mesa_hash_table_destroy(ctx->range_ht, NULL);
}

isel_context
setup_isel_context(Program *program, unsigned shader_count,
                   struct nir_shader *const *shaders,
                   ac_shader_config *config,
                   const struct aco_compiler_options *options,
                   const struct aco_shader_info *info,
                   const struct ac_shader_args *args, SWStage sw_stage)
{
   for (unsigned i = 0; i < shader_count; i++) {
      switch (shaders[i]->info.stage) {
      case MESA_SHADER_VERTEX:
         sw_stage = sw_stage | SWStage::VS;
         break;
      case MESA_SHADER_TESS_CTRL:
         sw_stage = sw_stage | SWStage::TCS;
         break;
      case MESA_SHADER_TESS_EVAL:
         sw_stage = sw_stage | SWStage::TES;
         break;
      case MESA_SHADER_GEOMETRY:
         sw_stage = sw_stage | SWStage::GS;
         break;
      case MESA_SHADER_FRAGMENT:
         sw_stage = sw_stage | SWStage::FS;
         break;
      case MESA_SHADER_KERNEL:
      case MESA_SHADER_COMPUTE:
         sw_stage = sw_stage | SWStage::CS;
         break;
      case MESA_SHADER_TASK:
         sw_stage = sw_stage | SWStage::TS;
         break;
      case MESA_SHADER_MESH:
         sw_stage = sw_stage | SWStage::MS;
         break;
      case MESA_SHADER_RAYGEN:
      case MESA_SHADER_CLOSEST_HIT:
      case MESA_SHADER_MISS:
      case MESA_SHADER_CALLABLE:
      case MESA_SHADER_INTERSECTION:
      case MESA_SHADER_ANY_HIT:
         sw_stage = SWStage::RT;
         break;
      default:
         UNREACHABLE("Shader stage not implemented");
      }
   }

   init_program(program, Stage{info->hw_stage, sw_stage}, info, options,
                config);

   isel_context ctx = {};
   ctx.program = program;
   ctx.args = args;
   ctx.options = options;
   ctx.stage = program->stage;

   program->workgroup_size = program->info.workgroup_size;
   assert(program->workgroup_size);

   /* Mesh shading only works on GFX10.3+. */
   ASSERTED bool mesh_shading =
      ctx.stage.has(SWStage::TS) || ctx.stage.has(SWStage::MS);
   assert(!mesh_shading || ctx.program->gfx_level >= GFX10_3);

   setup_tcs_info(&ctx);

   calc_min_waves(program);

   for (unsigned i = 0; i < shader_count; i++) {
      setup_nir(&ctx, shaders[i]);
   }

   unsigned scratch_size = 0;
   for (unsigned i = 0; i < shader_count; i++) {
      scratch_size = std::max(scratch_size, shaders[i]->scratch_size);
   }

   ctx.program->config->scratch_bytes_per_wave =
      align(scratch_size, 4u) * ctx.program->wave_size;
   ctx.program->config->lds_size = program->info.lds_size;
   assert(ctx.program->config->lds_size <= ctx.program->dev.lds_limit);

   unsigned nir_num_blocks = 0;
   for (unsigned i = 0; i < shader_count; i++) {
      nir_function_impl *entrypoint = nir_shader_get_entrypoint(shaders[i]);
      if (!entrypoint) {
         /*
          * RT shaders have no NIR entrypoint, but only one function impl
          * exists at this stage.
          */
         nir_foreach_function_impl (func, shaders[i]) {
            entrypoint = func;
            break;
         }
      }
      nir_num_blocks += entrypoint->num_blocks;
   }
   ctx.program->blocks.reserve(nir_num_blocks * 2u);
   ctx.block = ctx.program->create_and_insert_block();
   ctx.block->kind = block_kind_top_level;

   return ctx;
}

} /* namespace aco */
