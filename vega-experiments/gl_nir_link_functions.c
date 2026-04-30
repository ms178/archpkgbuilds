/*
 * Copyright © 2024 Valve Corporation
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
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#include "gl_nir_linker.h"
#include "linker_util.h"
#include "program/symbol_table.h"
#include "util/hash_table.h"
#include "main/shader_types.h"
#include "nir.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

struct function_sig {
   nir_function *func;

   struct list_head node;
};

typedef enum {
   PARAMETER_LIST_NO_MATCH,
   PARAMETER_LIST_EXACT_MATCH,
   PARAMETER_LIST_INEXACT_MATCH /* Match requires implicit conversion. */
} parameter_list_match_t;

/**
 * Check if two parameter lists match.
 *
 * list_a Parameters of the function definition.
 * list_b Actual parameters passed to the function.
 */
static parameter_list_match_t
parameter_lists_match(bool has_implicit_conversions,
                      bool has_implicit_int_to_uint_conversion,
                      nir_parameter *list_a, unsigned num_params_a,
                      nir_parameter *list_b, unsigned num_params_b)
{
   if (num_params_a != num_params_b)
      return PARAMETER_LIST_NO_MATCH;

   const bool has_any_implicit =
      has_implicit_conversions || has_implicit_int_to_uint_conversion;
   bool inexact_match = false;

   for (unsigned i = 0u; i < num_params_a; ++i) {
      nir_parameter *param_a = &list_a[i];
      nir_parameter *param_b = &list_b[i];

      if (param_a->type == param_b->type)
         continue;

      inexact_match = true;
      if (!has_any_implicit)
         return PARAMETER_LIST_NO_MATCH;

      switch (param_a->mode) {
      case nir_var_function_in:
         if (param_a->implicit_conversion_prohibited ||
             !_mesa_glsl_can_implicitly_convert(param_b->type,
                                                param_a->type,
                                                has_implicit_conversions,
                                                has_implicit_int_to_uint_conversion))
            return PARAMETER_LIST_NO_MATCH;
         break;

      case nir_var_function_out:
         if (!_mesa_glsl_can_implicitly_convert(param_a->type,
                                                param_b->type,
                                                has_implicit_conversions,
                                                has_implicit_int_to_uint_conversion))
            return PARAMETER_LIST_NO_MATCH;
         break;

      case nir_var_function_inout:
         return PARAMETER_LIST_NO_MATCH;

      default:
         assert(false);
         return PARAMETER_LIST_NO_MATCH;
      }
   }

   return inexact_match ? PARAMETER_LIST_INEXACT_MATCH
                        : PARAMETER_LIST_EXACT_MATCH;
}

/* Classes of parameter match, sorted (mostly) best matches first.
 * See is_better_parameter_match() below for the exceptions.
 */
typedef enum {
   PARAMETER_EXACT_MATCH,
   PARAMETER_FLOAT_TO_DOUBLE,
   PARAMETER_INT_TO_FLOAT,
   PARAMETER_INT_TO_DOUBLE,
   PARAMETER_OTHER_CONVERSION,
} parameter_match_t;

static parameter_match_t
get_parameter_match_type(const nir_parameter *param,
                         const nir_parameter *actual)
{
   const struct glsl_type *from_type;
   const struct glsl_type *to_type;

   if (param->mode == nir_var_function_out) {
      from_type = param->type;
      to_type = actual->type;
   } else {
      from_type = actual->type;
      to_type = param->type;
   }

   if (from_type == to_type)
      return PARAMETER_EXACT_MATCH;

   if (glsl_type_is_double(to_type)) {
      if (glsl_type_is_float(from_type))
         return PARAMETER_FLOAT_TO_DOUBLE;
      return PARAMETER_INT_TO_DOUBLE;
   }

   if (glsl_type_is_float(to_type))
      return PARAMETER_INT_TO_FLOAT;

   return PARAMETER_OTHER_CONVERSION;
}

static bool
is_better_parameter_match(parameter_match_t a_match,
                          parameter_match_t b_match)
{
   if (a_match >= PARAMETER_INT_TO_FLOAT &&
       b_match == PARAMETER_OTHER_CONVERSION)
      return false;

   return a_match < b_match;
}

static bool
is_best_inexact_overload(nir_parameter *actual_parameters,
                         unsigned num_parameters,
                         nir_function **matches, unsigned num_matches,
                         nir_function *sig)
{
   for (nir_function **other = matches; other < matches + num_matches; ++other) {
      if (*other == sig)
         continue;

      nir_parameter *node_a = sig->params;
      nir_parameter *node_b = (*other)->params;
      bool better_for_some_parameter = false;

      for (unsigned i = 0u; i < num_parameters; ++i) {
         const parameter_match_t a_match =
            get_parameter_match_type(&node_a[i], &actual_parameters[i]);
         const parameter_match_t b_match =
            get_parameter_match_type(&node_b[i], &actual_parameters[i]);

         if (is_better_parameter_match(a_match, b_match))
            better_for_some_parameter = true;

         if (is_better_parameter_match(b_match, a_match))
            return false;
      }

      if (!better_for_some_parameter)
         return false;
   }

   return true;
}

static nir_function *
choose_best_inexact_overload(nir_parameter *actual_parameters,
                             unsigned num_parameters,
                             nir_function **matches, unsigned num_matches,
                             bool has_choose_best_inexact_overload)
{
   if (num_matches == 0u)
      return NULL;

   if (num_matches == 1u)
      return *matches;

   if (!has_choose_best_inexact_overload)
      return NULL;

   for (nir_function **sig = matches; sig < matches + num_matches; ++sig) {
      if (is_best_inexact_overload(actual_parameters,
                                   num_parameters,
                                   matches,
                                   num_matches,
                                   *sig))
         return *sig;
   }

   return NULL;
}

static nir_function *
find_matching_signature(struct list_head *f_list,
                        nir_parameter *parameters,
                        unsigned num_parameters,
                        bool has_implicit_conversions,
                        bool has_implicit_int_to_uint_conversion)
{
   unsigned inexact_count = 0u;

   list_for_each_entry(struct function_sig, sig, f_list, node) {
      const parameter_list_match_t match =
         parameter_lists_match(has_implicit_conversions,
                               has_implicit_int_to_uint_conversion,
                               sig->func->params,
                               sig->func->num_params,
                               parameters,
                               num_parameters);
      if (match == PARAMETER_LIST_EXACT_MATCH)
         return sig->func;

      if (match == PARAMETER_LIST_INEXACT_MATCH && !sig->func->is_subroutine)
         ++inexact_count;
   }

   if (inexact_count == 0u)
      return NULL;

   nir_function *small_matches[8];
   nir_function **inexact_matches = small_matches;
   if (inexact_count > ARRAY_SIZE(small_matches)) {
      inexact_matches = malloc(sizeof(*inexact_matches) * inexact_count);
      if (!inexact_matches)
         return NULL;
   }

   unsigned idx = 0u;
   list_for_each_entry(struct function_sig, sig, f_list, node) {
      const parameter_list_match_t match =
         parameter_lists_match(has_implicit_conversions,
                               has_implicit_int_to_uint_conversion,
                               sig->func->params,
                               sig->func->num_params,
                               parameters,
                               num_parameters);
      if (match == PARAMETER_LIST_INEXACT_MATCH && !sig->func->is_subroutine)
         inexact_matches[idx++] = sig->func;
   }

   nir_function *result = choose_best_inexact_overload(parameters,
                                                       num_parameters,
                                                       inexact_matches,
                                                       idx,
                                                       has_implicit_int_to_uint_conversion);

   if (inexact_matches != small_matches)
      free(inexact_matches);

   return result;
}

/**
 * Resolve the function calls reachable from the shader entrypoint.
 * Remove unreachable functions. This avoids unresolved-reference errors
 * for dead helper functions.
 */
static bool
resolve_calls_and_remove_unreachable(struct gl_shader_program *prog,
                                     const struct gl_shader *main,
                                     struct gl_linked_shader *linked_sh,
                                     struct hash_table *func_lookup)
{
   nir_shader *shader = linked_sh->Program->nir;
   nir_function_impl *entry = nir_shader_get_entrypoint(shader);
   if (!entry)
      return true;

   nir_foreach_function(func, shader)
      func->pass_flags = 0u;

   unsigned stack_cap = 64u;
   unsigned stack_count = 0u;
   nir_function_impl **stack = calloc(stack_cap, sizeof(*stack));
   if (!stack)
      return false;

   stack[stack_count++] = entry;
   bool success = true;

   while (stack_count > 0u) {
      nir_function_impl *impl = stack[--stack_count];
      nir_function *owner = impl->function;
      if (owner->pass_flags)
         continue;

      owner->pass_flags = 1u;

      nir_foreach_block(block, impl) {
         nir_foreach_instr(instr, block) {
            if (instr->type != nir_instr_type_call)
               continue;

            nir_call_instr *call = nir_instr_as_call(instr);

            if (!call->callee->impl) {
               struct hash_entry *e = _mesa_hash_table_search(func_lookup,
                                                              call->callee->name);
               if (e) {
                  struct list_head *f_list = (struct list_head *)e->data;
                  nir_function *f = find_matching_signature(
                     f_list,
                     call->callee->params,
                     call->callee->num_params,
                     main->has_implicit_conversions,
                     main->has_implicit_int_to_uint_conversion);
                  if (f)
                     call->callee = f;
               }
            }

            if (!call->callee->impl) {
               linker_error(prog,
                            "unresolved reference to function `%s'\n",
                            call->callee->name);
               success = false;
               goto out;
            }

            if (stack_count == stack_cap) {
               const unsigned grown = stack_cap < 1024u
                                       ? stack_cap * 2u
                                       : stack_cap + 1024u;
               nir_function_impl **new_stack = realloc(stack,
                                                       sizeof(*stack) * grown);
               if (!new_stack) {
                  success = false;
                  goto out;
               }

               stack = new_stack;
               stack_cap = grown;
            }

            stack[stack_count++] = call->callee->impl;
         }
      }
   }

out:
   free(stack);

   nir_foreach_function_safe(func, shader) {
      if (func->impl && !func->pass_flags &&
          strstr(func->name, "gl_mesa_tmp") == NULL)
         exec_node_remove(&func->node);
   }

   return success;
}

static nir_function *
clone_function(struct hash_table *remap_table,
               const nir_function *fxn, nir_shader *ns)
{
   nir_function *nfxn = nir_function_clone(ns, fxn);
   _mesa_hash_table_insert(remap_table, fxn, nfxn);
   return nfxn;
}

static struct list_head *
get_or_create_func_list(struct hash_table *func_lookup,
                        void *mem_ctx,
                        const char *name)
{
   struct hash_entry *e = _mesa_hash_table_search(func_lookup, name);
   if (e)
      return (struct list_head *)e->data;

   struct list_head *func_list = ralloc(mem_ctx, struct list_head);
   if (!func_list)
      return NULL;

   list_inithead(func_list);
   _mesa_hash_table_insert(func_lookup, name, func_list);
   return func_list;
}

bool
gl_nir_link_function_calls(struct gl_shader_program *prog,
                           struct gl_shader *main,
                           struct gl_linked_shader *linked_sh,
                           struct gl_shader **shader_list,
                           unsigned num_shaders)
{
   void *mem_ctx = ralloc_context(NULL);
   struct hash_table *var_lookup = _mesa_string_hash_table_create(mem_ctx);
   struct hash_table *func_lookup = _mesa_string_hash_table_create(mem_ctx);
   struct hash_table *remap_table = _mesa_pointer_hash_table_create(mem_ctx);

   if (!mem_ctx || !var_lookup || !func_lookup || !remap_table) {
      ralloc_free(mem_ctx);
      return false;
   }

   nir_foreach_variable_in_shader(var, linked_sh->Program->nir) {
      _mesa_hash_table_insert(var_lookup, var->name, var);
   }

   nir_foreach_function(func, linked_sh->Program->nir) {
      if (!func->impl)
         continue;

      struct list_head *f_list = get_or_create_func_list(func_lookup,
                                                         mem_ctx,
                                                         func->name);
      if (!f_list) {
         ralloc_free(mem_ctx);
         return false;
      }

      nir_function *f = find_matching_signature(f_list,
                                                func->params,
                                                func->num_params,
                                                main->has_implicit_conversions,
                                                main->has_implicit_int_to_uint_conversion);
      if (!f) {
         struct function_sig *func_sig = ralloc(mem_ctx, struct function_sig);
         if (!func_sig) {
            ralloc_free(mem_ctx);
            return false;
         }

         func_sig->func = func;
         list_add(&func_sig->node, f_list);
      }
   }

   for (unsigned i = 0u; i < num_shaders; ++i) {
      if (main == shader_list[i])
         continue;

      nir_foreach_variable_in_shader(var, shader_list[i]->nir) {
         struct hash_entry *e = _mesa_hash_table_search(var_lookup, var->name);
         if (e) {
            _mesa_hash_table_insert(remap_table, var, e->data);

            nir_variable *m_var = (nir_variable *)e->data;
            if (glsl_type_is_array(var->type)) {
               m_var->data.max_array_access = MAX2(var->data.max_array_access,
                                                   m_var->data.max_array_access);

               if (glsl_array_size(m_var->type) == 0 &&
                   glsl_array_size(var->type) != 0)
                  m_var->type = var->type;
            }

            if (glsl_without_array(var->type) == var->interface_type) {
               int *linked_max_ifc_array_access = m_var->max_ifc_array_access;
               int *ir_max_ifc_array_access = var->max_ifc_array_access;

               assert(linked_max_ifc_array_access != NULL);
               assert(ir_max_ifc_array_access != NULL);

               for (unsigned j = 0u; j < var->interface_type->length; ++j) {
                  linked_max_ifc_array_access[j] =
                     MAX2(linked_max_ifc_array_access[j],
                          ir_max_ifc_array_access[j]);
               }
            }
         } else {
            nir_variable *nvar = nir_variable_clone(var, linked_sh->Program->nir);
            if (!nvar) {
               ralloc_free(mem_ctx);
               return false;
            }

            _mesa_hash_table_insert(remap_table, var, nvar);
            nir_shader_add_variable(linked_sh->Program->nir, nvar);
            _mesa_hash_table_insert(var_lookup, var->name, nvar);
         }
      }

      nir_foreach_function(func, shader_list[i]->nir) {
         nir_function *f = NULL;
         struct list_head *f_list = get_or_create_func_list(func_lookup,
                                                            mem_ctx,
                                                            func->name);
         if (!f_list) {
            ralloc_free(mem_ctx);
            return false;
         }

         f = find_matching_signature(f_list,
                                     func->params,
                                     func->num_params,
                                     false,
                                     false);
         if (!f) {
            struct function_sig *func_sig = ralloc(mem_ctx, struct function_sig);
            if (!func_sig) {
               ralloc_free(mem_ctx);
               return false;
            }

            f = clone_function(remap_table, func, linked_sh->Program->nir);
            if (!f) {
               ralloc_free(mem_ctx);
               return false;
            }

            func_sig->func = f;
            if (func->impl)
               list_add(&func_sig->node, f_list);
         } else {
            _mesa_hash_table_insert(remap_table, func, f);
         }
      }

      nir_foreach_function(func, shader_list[i]->nir) {
         if (func->impl) {
            nir_function_impl *f_impl =
               nir_function_impl_clone_remap_globals(linked_sh->Program->nir,
                                                     func->impl,
                                                     remap_table);

            struct hash_entry *e = _mesa_hash_table_search(remap_table, func);
            assert(e);

            nir_function *f = (nir_function *)e->data;
            assert(!f->impl);
            nir_function_set_impl(f, f_impl);
         }
      }
   }

   if (!resolve_calls_and_remove_unreachable(prog, main, linked_sh, func_lookup)) {
      ralloc_free(mem_ctx);
      return false;
   }

   if (linked_sh->Stage != MESA_SHADER_FRAGMENT) {
      for (unsigned i = 0u; i < num_shaders; ++i) {
         if (main == shader_list[i])
            continue;

         nir_foreach_shader_out_variable(var, shader_list[i]->nir) {
            struct hash_entry *e = _mesa_hash_table_search(var_lookup, var->name);
            if (e)
               continue;

            nir_variable *nvar = nir_variable_clone(var, linked_sh->Program->nir);
            if (!nvar) {
               ralloc_free(mem_ctx);
               return false;
            }

            nir_shader_add_variable(linked_sh->Program->nir, nvar);
            _mesa_hash_table_insert(var_lookup, nvar->name, nvar);
         }
      }
   }

   nir_fixup_deref_types(linked_sh->Program->nir);

   ralloc_free(mem_ctx);
   return true;
}
