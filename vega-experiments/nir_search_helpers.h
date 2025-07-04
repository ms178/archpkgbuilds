/*
 * Copyright © 2016 Red Hat
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
 * Authors:
 *    Rob Clark <robclark@freedesktop.org>
 */

#ifndef _NIR_SEARCH_HELPERS_
#define _NIR_SEARCH_HELPERS_

#include <math.h>
#include "util/bitscan.h"
#include "util/u_math.h"
#include "nir.h"
#include "nir_range_analysis.h"
#include <stdbool.h>
#include <stdint.h>

#ifndef likely
#  define likely(x)   __builtin_expect(!!(x), 1)
#endif
#ifndef unlikely
#  define unlikely(x) __builtin_expect(!!(x), 0)
#endif

/**
 * @brief Check if an integer constant can be an inline constant on GFX9+.
 *
 * GFX9 (Vega) and later architectures can encode certain small integer and
 * floating-point constants directly into an instruction, avoiding a VGPR load.
 * This is a significant performance and power saving. This helper is for
 * identifying integer constants that can be used as instruction modifiers or
 * folded into operations.
 *
 * Per the "Vega" ISA Reference Guide, Table 9 (Scalar Operands, pg. 36),
 * the hardware directly supports integer encodings for:
 * - Signed integers from -16 to 64 (inclusive).
 *
 * Additionally, backends can often optimize multiplications with powers of two
 * into shifts. This helper also checks for this common optimization pattern.
 *
 * @param ht Unused, for API compatibility.
 * @param instr The ALU instruction whose source is being checked.
 * @param src The index of the source operand.
 * @param num_components The number of components to check.
 * @param swizzle The swizzle for the source operand.
 * @return True if all checked components are valid GFX9+ inline constants.
 */
static inline bool
is_gfx9_inline_const(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
                     unsigned src, unsigned num_components, const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      const unsigned bit_size = instr->src[src].src.ssa->bit_size;
      if (bit_size != 16 && bit_size != 32 && bit_size != 64) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const int64_t val = nir_src_comp_as_int(instr->src[src].src, swizzle[i]);

            /* Check for the most common case first: small integers directly
             * supported by the ISA.
             */
            if (val >= -16 && val <= 64) {
                  continue;
            }

            /* Check for powers of two, a common target for strength reduction.
             * The ISA supports up to 2^15 for some operations.
             */
            const int64_t abs_val = (val > 0) ? val : -val;
            if (val != 0 && util_is_power_of_two_or_zero64(abs_val) && abs_val <= 32768) {
                  continue;
            }

            /* If it's not in any of the allowed categories, it's not an inline const. */
            return false;
      }
      return true;
}

/**
 * @brief Check if an integer constant is exactly 3.
 *
 * Used for strength reduction: imul(a, 3) -> iadd(ishl(a, 1), a).
 */
static inline bool
is_imm_3(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
         unsigned src, unsigned num_components,
         const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv))
            return false;

      for (unsigned i = 0; i < num_components; i++) {
            if (nir_src_comp_as_int(instr->src[src].src, swizzle[i]) != 3)
                  return false;
      }
      return true;
}

/**
 * @brief Check if an integer constant is exactly 5.
 *
 * Used for strength reduction: imul(a, 5) -> iadd(ishl(a, 2), a).
 */
static inline bool
is_imm_5(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
         unsigned src, unsigned num_components,
         const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv))
            return false;

      for (unsigned i = 0; i < num_components; i++) {
            if (nir_src_comp_as_int(instr->src[src].src, swizzle[i]) != 5)
                  return false;
      }
      return true;
}

/**
 * @brief Check if an integer constant is exactly 9.
 *
 * Used for strength reduction: imul(a, 9) -> iadd(ishl(a, 3), a).
 */
static inline bool
is_imm_9(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
         unsigned src, unsigned num_components,
         const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv))
            return false;

      for (unsigned i = 0; i < num_components; i++) {
            if (nir_src_comp_as_int(instr->src[src].src, swizzle[i]) != 9)
                  return false;
      }
      return true;
}

/**
 * @brief Check if a 16-bit float is exactly 0.0 or 1.0.
 *
 * These values can be encoded as inline constants on GFX9+, saving a
 * register and a literal fetch. This is a subset of the full inline float
 * constants but covers very common cases for MAD/FMA patterns.
 *
 * @return True if all checked components are 0.0 or 1.0.
 */
static inline bool
is_imm_fp16_zero_or_one(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
                        unsigned src, unsigned num_components,
                        const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      if (instr->src[src].src.ssa->bit_size != 16) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const uint16_t bits = nir_src_comp_as_uint(instr->src[src].src, swizzle[i]);

            /* Check for exact bit patterns:
             * 0x0000 = +0.0f16
             * 0x8000 = -0.0f16 (also accepted as zero)
             * 0x3c00 = 1.0f16
             */
            if (bits != 0x0000 && bits != 0x8000 && bits != 0x3c00) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if a 16-bit float value is a Not-a-Number (NaN).
 *
 * Based on IEEE 754 half-precision format:
 * - Sign bit: any (bit 15)
 * - Exponent: all ones (bits 14-10 = 0x1f)
 * - Mantissa: non-zero (bits 9-0)
 *
 * @return True if all checked components are NaN.
 */
static inline bool
is_fp16_nan(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
            unsigned src, unsigned num_components, const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      if (instr->src[src].src.ssa->bit_size != 16) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const uint16_t bits = nir_src_comp_as_uint(instr->src[src].src, swizzle[i]);

            /* Extract exponent (bits 14-10) and mantissa (bits 9-0) */
            const uint16_t exp_bits = (bits >> 10) & 0x1f;
            const uint16_t mantissa = bits & 0x3ff;

            /* NaN requires: exponent = 0x1f (all ones) AND mantissa != 0 */
            if (exp_bits != 0x1f || mantissa == 0) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if an integer value is a positive power of two.
 *
 * Used for strength reduction: imul(x, pow2) -> ishl(x, log2(pow2)).
 * This saves cycles on GFX9+ where shifts are often faster than multiplies.
 *
 * @return True if all checked components are positive powers of two.
 */
static inline bool
is_pos_power_of_two(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
                    unsigned src, unsigned num_components,
                    const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      const nir_alu_type type = nir_op_infos[instr->op].input_types[src];
      const nir_alu_type base_type = nir_alu_type_get_base_type(type);

      for (unsigned i = 0; i < num_components; i++) {
            switch (base_type) {
                  case nir_type_int: {
                        const int64_t val = nir_src_comp_as_int(instr->src[src].src, swizzle[i]);
                        if (val <= 0 || !util_is_power_of_two_or_zero64(val)) {
                              return false;
                        }
                        break;
                  }
                  case nir_type_uint: {
                        const uint64_t val = nir_src_comp_as_uint(instr->src[src].src, swizzle[i]);
                        /* A value of 0 is not a positive power of two. */
                        if (val == 0 || !util_is_power_of_two_or_zero64(val)) {
                              return false;
                        }
                        break;
                  }
                  default:
                        return false;
            }
      }
      return true;
}

/**
 * @brief Check if an integer value is a negative power of two.
 *
 * Used for patterns like: imul(x, -pow2) -> ineg(ishl(x, log2(pow2))).
 * This is a critical optimization for integer arithmetic.
 *
 * @return True if all checked components are negative powers of two.
 */
static inline bool
is_neg_power_of_two(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
                    unsigned src, unsigned num_components,
                    const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      /* Only signed integers can be negative. */
      const nir_alu_type type = nir_op_infos[instr->op].input_types[src];
      if (nir_alu_type_get_base_type(type) != nir_type_int) {
            return false;
      }

      const unsigned bit_size = instr->src[src].src.ssa->bit_size;
      const int64_t int_min = u_intN_min(bit_size);

      for (unsigned i = 0; i < num_components; i++) {
            const int64_t val = nir_src_comp_as_int(instr->src[src].src, swizzle[i]);

            /* A value cannot be positive or zero. */
            if (val >= 0) {
                  return false;
            }

            /* INT_MIN is technically a power of two, but negating it overflows.
             * This is a critical edge case to handle correctly.
             */
            if (val == int_min) {
                  return false;
            }

            /* Check if the positive counterpart is a power of two. */
            if (!util_is_power_of_two_or_zero64(-val)) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if an integer constant has exactly two bits set.
 *
 * Used for recognizing patterns like `iadd(ishl(x, a), ishl(x, b))`
 * which can sometimes be optimized.
 *
 * @return True if all components have a popcount of 2.
 */
static inline bool
is_bitcount2(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
             unsigned src, unsigned num_components,
             const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const uint64_t val = nir_src_comp_as_uint(instr->src[src].src, swizzle[i]);
            if (util_bitcount64(val) != 2) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if a floating-point constant is Not-a-Number (NaN).
 *
 * This is a generic, bit-size-agnostic version that correctly handles
 * fp16, fp32, and fp64.
 *
 * @return True if all components are NaN.
 */
static inline bool
is_nan(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
       unsigned src, unsigned num_components, const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            if (!isnan(nir_src_comp_as_float(instr->src[src].src, swizzle[i]))) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if a floating-point constant is negative zero.
 *
 * This check is bit-size aware and verifies the exact bit pattern for
 * -0.0, which is important for certain floating point identities. This
 * implementation is correct for fp16, fp32, and fp64.
 *
 * @return True if all components are negative zero.
 */
static inline bool
is_negative_zero(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
                 unsigned src, unsigned num_components, const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      const unsigned bit_size = instr->src[src].src.ssa->bit_size;
      const uint64_t neg_zero_pattern = 1ULL << (bit_size - 1);

      if (bit_size != 16 && bit_size != 32 && bit_size != 64) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const uint64_t val = nir_src_comp_as_uint(instr->src[src].src, swizzle[i]);
            if (val != neg_zero_pattern) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if any component of a floating-point constant is NaN.
 *
 * @return True if at least one component is NaN.
 */
static inline bool
is_any_comp_nan(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
                unsigned src, unsigned num_components, const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            if (isnan(nir_src_comp_as_float(instr->src[src].src, swizzle[i]))) {
                  return true;
            }
      }

      return false;
}

/**
 * @brief Check if an unsigned integer constant is a multiple of 2.
 */
static inline bool
is_unsigned_multiple_of_2(UNUSED struct hash_table *ht,
                          const nir_alu_instr *instr,
                          unsigned src, unsigned num_components,
                          const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const uint64_t val = nir_src_comp_as_uint(instr->src[src].src, swizzle[i]);
            if ((val & 1) != 0) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if an unsigned integer constant is a multiple of 4.
 */
static inline bool
is_unsigned_multiple_of_4(UNUSED struct hash_table *ht,
                          const nir_alu_instr *instr,
                          unsigned src, unsigned num_components,
                          const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const uint64_t val = nir_src_comp_as_uint(instr->src[src].src, swizzle[i]);
            if ((val & 3) != 0) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if an unsigned integer constant is a multiple of 8.
 */
static inline bool
is_unsigned_multiple_of_8(UNUSED struct hash_table *ht,
                          const nir_alu_instr *instr,
                          unsigned src, unsigned num_components,
                          const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const uint64_t val = nir_src_comp_as_uint(instr->src[src].src, swizzle[i]);
            if ((val & 7) != 0) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if an unsigned integer constant is a multiple of 16.
 */
static inline bool
is_unsigned_multiple_of_16(UNUSED struct hash_table *ht,
                           const nir_alu_instr *instr,
                           unsigned src, unsigned num_components,
                           const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const uint64_t val = nir_src_comp_as_uint(instr->src[src].src, swizzle[i]);
            if ((val & 15) != 0) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if an unsigned integer constant is a multiple of 32.
 */
static inline bool
is_unsigned_multiple_of_32(UNUSED struct hash_table *ht,
                           const nir_alu_instr *instr,
                           unsigned src, unsigned num_components,
                           const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const uint64_t val = nir_src_comp_as_uint(instr->src[src].src, swizzle[i]);
            if ((val & 31) != 0) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if an unsigned integer constant is a multiple of 64.
 */
static inline bool
is_unsigned_multiple_of_64(UNUSED struct hash_table *ht,
                           const nir_alu_instr *instr,
                           unsigned src, unsigned num_components,
                           const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const uint64_t val = nir_src_comp_as_uint(instr->src[src].src, swizzle[i]);
            if ((val & 63) != 0) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if a float constant is in the inclusive range [0.0, 1.0].
 *
 * Useful for fsat-related optimizations.
 *
 * @return True if all components are in [0.0, 1.0].
 */
static inline bool
is_zero_to_one(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
               unsigned src, unsigned num_components,
               const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      const nir_alu_type type = nir_op_infos[instr->op].input_types[src];
      if (nir_alu_type_get_base_type(type) != nir_type_float) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const double val = nir_src_comp_as_float(instr->src[src].src, swizzle[i]);
            if (isnan(val) || val < 0.0 || val > 1.0) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if a float constant is in the exclusive range (0.0, 1.0).
 *
 * This differs from `is_zero_to_one` by excluding the endpoints.
 *
 * @return True if all components are in (0.0, 1.0).
 */
static inline bool
is_gt_0_and_lt_1(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
                 unsigned src, unsigned num_components,
                 const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      const nir_alu_type type = nir_op_infos[instr->op].input_types[src];
      if (nir_alu_type_get_base_type(type) != nir_type_float) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const double val = nir_src_comp_as_float(instr->src[src].src, swizzle[i]);
            if (isnan(val) || val <= 0.0 || val >= 1.0) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if an integer constant is odd (i.e., its LSB is 1).
 *
 * @return True if all components are odd.
 */
static inline bool
is_odd(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
       unsigned src, unsigned num_components,
       const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const nir_alu_type type = nir_op_infos[instr->op].input_types[src];
            switch (nir_alu_type_get_base_type(type)) {
                  case nir_type_int:
                  case nir_type_uint: {
                        if ((nir_src_comp_as_uint(instr->src[src].src, swizzle[i]) & 1) == 0) {
                              return false;
                        }
                        break;
                  }
                  default:
                        return false;
            }
      }

      return true;
}

/**
 * @brief Check if a source is not a constant zero.
 *
 * This provides a weak guarantee. It returns true if the source is not a
 * constant, or if it is a constant that is not zero. It is useful in
 * cases where an optimization can proceed unless the value is provably a
 * constant zero.
 *
 * @return False only if the source is a constant zero. True otherwise.
 */
static inline bool
is_not_const_zero(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
                  unsigned src, unsigned num_components,
                  const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (!cv) {
            return true; /* Not a constant, so not a constant zero */
      }

      for (unsigned i = 0; i < num_components; i++) {
            const nir_alu_type type = nir_op_infos[instr->op].input_types[src];
            switch (nir_alu_type_get_base_type(type)) {
                  case nir_type_float:
                        if (nir_src_comp_as_float(instr->src[src].src, swizzle[i]) == 0.0) {
                              return false;
                        }
                        break;
                  case nir_type_bool:
                  case nir_type_int:
                  case nir_type_uint:
                        if (nir_src_comp_as_uint(instr->src[src].src, swizzle[i]) == 0) {
                              return false;
                        }
                        break;
                  default:
                        /* Should not be reached with valid NIR */
                        return false;
            }
      }

      return true;
}

/**
 * @brief Helper to check if a constant's components are all less than a limit.
 */
static inline bool
is_ult(const nir_alu_instr *instr, unsigned src, unsigned num_components,
       const uint8_t *swizzle, uint64_t limit)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const uint64_t val = nir_src_comp_as_uint(instr->src[src].src, swizzle[i]);
            if (val >= limit) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if an unsigned constant is less than 32.
 *
 * Used for shift amount validation on 32-bit operations to ensure
 * the shift amount is not out of range, which is undefined behavior in some APIs.
 *
 * @return True if all components are in [0, 31].
 */
static inline bool
is_ult_32(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
          unsigned src, unsigned num_components,
          const uint8_t *swizzle)
{
      return is_ult(instr, src, num_components, swizzle, 32);
}

/**
 * @brief Check if an unsigned constant is less than 0xfffc07fc.
 *
 * This specific magic number is used in certain graphics algorithms,
 * particularly in older D3D9-era pixel shader calculations.
 *
 * @return True if all components are < 0xfffc07fc.
 */
static inline bool
is_ult_0xfffc07fc(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
                  unsigned src, unsigned num_components,
                  const uint8_t *swizzle)
{
      return is_ult(instr, src, num_components, swizzle, 0xfffc07fcU);
}

/**
 * @brief Check if the 5 least significant bits of a value are >= 2.
 *
 * Used for specific bitfield and shift-related optimizations.
 *
 * @return True if `(val & 0x1f) >= 2` for all components.
 */
static inline bool
is_first_5_bits_uge_2(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
                      unsigned src, unsigned num_components,
                      const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const unsigned val = nir_src_comp_as_uint(instr->src[src].src, swizzle[i]);
            if ((val & 0x1f) < 2) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if a constant fits in 16 bits after applying a scale factor.
 *
 * This is a powerful helper for optimizations that convert 32-bit arithmetic
 * to 16-bit. It checks if all components of a vector constant, when scaled,
 * can be represented by a single 16-bit type (either all signed or all
 * unsigned). For example, if one component becomes -1 and another becomes
 * 40000, it's impossible, as -1 requires int16_t and 40000 requires
 * uint16_t.
 *
 * @param scale The integer scale factor to apply before checking.
 * @return True if the scaled constant can be represented in 16 bits.
 */
static inline bool
is_16_bits_with_scale(const nir_alu_instr *instr,
                      unsigned src, unsigned num_components,
                      const uint8_t *swizzle, int scale)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      /* All elements must be representable as int16_t or uint16_t. */
      bool must_be_signed = false;
      bool must_be_unsigned = false;

      for (unsigned i = 0; i < num_components; i++) {
            const int64_t val =
            scale * nir_src_comp_as_int(instr->src[src].src, swizzle[i]);

            /* Check if the value is outside the union of int16/uint16 ranges. */
            if (val > 0xffff || val < -0x8000) {
                  return false;
            }

            /* If a value is negative, all values must fit in int16_t. */
            if (val < 0) {
                  if (must_be_unsigned) {
                        return false; /* Conflict: another component required uint16_t. */
                  }
                  must_be_signed = true;
            }

            /* If a value is > 32767, all values must fit in uint16_t. */
            if (val > 0x7fff) {
                  if (must_be_signed) {
                        return false; /* Conflict: another component required int16_t. */
                  }
                  must_be_unsigned = true;
            }
      }

      return true;
}

/**
 * @brief Check if a constant can be represented as int16_t or uint16_t.
 */
static inline bool
is_16_bits(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
           unsigned src, unsigned num_components,
           const uint8_t *swizzle)
{
      return is_16_bits_with_scale(instr, src, num_components, swizzle, 1);
}

/**
 * @brief Check if `2 * constant` can be represented as int16_t or uint16_t.
 */
static inline bool
is_2x_16_bits(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
              unsigned src, unsigned num_components,
              const uint8_t *swizzle)
{
      return is_16_bits_with_scale(instr, src, num_components, swizzle, 2);
}

/**
 * @brief Check if `-2 * constant` can be represented as int16_t or uint16_t.
 */
static inline bool
is_neg2x_16_bits(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
                 unsigned src, unsigned num_components,
                 const uint8_t *swizzle)
{
      return is_16_bits_with_scale(instr, src, num_components, swizzle, -2);
}

/**
 * @brief Check if a source is not a compile-time constant.
 */
static inline bool
is_not_const(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
             unsigned src, UNUSED unsigned num_components,
             UNUSED const uint8_t *swizzle)
{
      return !nir_src_is_const(instr->src[src].src);
}

/**
 * @brief Helper to check if a source is an fmul, looking through an fneg.
 */
static inline bool
is_fmul_impl(const nir_alu_instr *instr, unsigned src)
{
      nir_instr *src_instr = nir_src_parent_instr(&instr->src[src].src);
      if (!src_instr || src_instr->type != nir_instr_type_alu) {
            return false;
      }

      nir_alu_instr *src_alu = nir_instr_as_alu(src_instr);
      if (src_alu->op == nir_op_fneg) {
            return is_fmul_impl(src_alu, 0);
      }

      return src_alu->op == nir_op_fmul || src_alu->op == nir_op_fmulz;
}

/**
 * @brief Check if a source is an fmul or fneg(fmul).
 *
 * Useful for identifying opportunities to form an FMA/MAD instruction,
 * e.g., `fadd(fmul(a,b), c)` or `fadd(fneg(fmul(a,b)), c)`.
 */
static inline bool
is_fmul(UNUSED struct hash_table *ht, const nir_alu_instr *instr, unsigned src,
        UNUSED unsigned num_components, UNUSED const uint8_t *swizzle)
{
      return is_fmul_impl(instr, src);
}

/**
 * @brief Check if a source is NOT an fmul or fneg(fmul).
 */
static inline bool
is_not_fmul(UNUSED struct hash_table *ht, const nir_alu_instr *instr, unsigned src,
            UNUSED unsigned num_components, UNUSED const uint8_t *swizzle)
{
      return !is_fmul_impl(instr, src);
}

/**
 * @brief Check if a source is an fsign, looking through a potential fneg.
 */
static inline bool
is_fsign(const nir_alu_instr *instr, unsigned src,
         UNUSED unsigned num_components, UNUSED const uint8_t *swizzle)
{
      nir_instr *src_instr = nir_src_parent_instr(&instr->src[src].src);
      if (!src_instr || src_instr->type != nir_instr_type_alu) {
            return false;
      }

      nir_alu_instr *src_alu = nir_instr_as_alu(src_instr);
      if (src_alu->op == nir_op_fneg) {
            src_instr = nir_src_parent_instr(&src_alu->src[0].src);
            if (!src_instr || src_instr->type != nir_instr_type_alu) {
                  return false;
            }
            src_alu = nir_instr_as_alu(src_instr);
      }

      return src_alu->op == nir_op_fsign;
}

/**
 * @brief Check if a source is neither a constant nor an fsign.
 */
static inline bool
is_not_const_and_not_fsign(struct hash_table *ht, const nir_alu_instr *instr,
                           unsigned src, unsigned num_components,
                           const uint8_t *swizzle)
{
      return is_not_const(ht, instr, src, num_components, swizzle) &&
      !is_fsign(instr, src, num_components, swizzle);
}

/**
 * @brief Check if an SSA definition has more than one use.
 *
 * Crucial for deciding if an instruction can be folded into a user without
 * duplicating computation. The signature is for nir_search compatibility.
 */
static inline bool
has_multiple_uses(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
                  UNUSED unsigned src, UNUSED unsigned num_components,
                  UNUSED const uint8_t *swizzle)
{
      return !list_is_singular(&instr->def.uses);
}

/**
 * @brief Check if an SSA definition has exactly one use.
 */
static inline bool
is_used_once(const nir_alu_instr *instr)
{
      return list_is_singular(&instr->def.uses);
}

/**
 * @brief Check if an SSA definition is used by an if condition.
 */
static inline bool
is_used_by_if(const nir_alu_instr *instr)
{
      return nir_def_used_by_if(&instr->def);
}

/**
 * @brief Check if an SSA definition is NOT used by an if condition.
 */
static inline bool
is_not_used_by_if(const nir_alu_instr *instr)
{
      return !nir_def_used_by_if(&instr->def);
}

/**
 * @brief Check if an SSA definition's only use is in an if condition.
 */
static inline bool
is_only_used_by_if(const nir_alu_instr *instr)
{
      return nir_def_only_used_by_if(&instr->def);
}

/**
 * @brief Check if an SSA definition has at least one user that is not an fsat.
 */
static inline bool
is_used_by_non_fsat(const nir_alu_instr *instr)
{
      nir_foreach_use(src, &instr->def) {
            const nir_instr *const user_instr = nir_src_parent_instr(src);

            if (user_instr->type != nir_instr_type_alu) {
                  return true;
            }

            const nir_alu_instr *const user_alu = nir_instr_as_alu(user_instr);
            if (user_alu->op != nir_op_fsat) {
                  return true;
            }
      }

      return false;
}

/**
 * @brief Check if an SSA definition has at least one user that is not ldc_nv.
 */
static inline bool
is_used_by_non_ldc_nv(const nir_alu_instr *instr)
{
      nir_foreach_use(src, &instr->def) {
            const nir_instr *const user_instr = nir_src_parent_instr(src);

            if (user_instr->type != nir_instr_type_intrinsic) {
                  return true;
            }

            const nir_intrinsic_instr *const user_intrin = nir_instr_as_intrinsic(user_instr);
            if (user_intrin->intrinsic != nir_intrinsic_ldc_nv) {
                  return true;
            }
      }

      return false;
}

/**
 * @brief Recursive implementation for is_only_used_as_float.
 */
static inline bool
is_only_used_as_float_impl(const nir_alu_instr *instr, unsigned depth)
{
      nir_foreach_use(src, &instr->def) {
            const nir_instr *const user_instr = nir_src_parent_instr(src);

            if (user_instr->type == nir_instr_type_alu) {
                  const nir_alu_instr *const user_alu = nir_instr_as_alu(user_instr);
                  const unsigned index = (nir_alu_src *)container_of(src, nir_alu_src, src) - user_alu->src;

                  /* Recurse through moves and bcsel (if not the condition) to find the
                   * true use. A depth limit prevents stack overflow on pathological shaders.
                   */
                  const bool is_mov_like = (user_alu->op == nir_op_bcsel && index != 0) ||
                  nir_op_is_vec_or_mov(user_alu->op);

                  if (is_mov_like && depth < 8) {
                        if (is_only_used_as_float_impl(user_alu, depth + 1)) {
                              continue;
                        }
                  }

                  const nir_alu_type type = nir_op_infos[user_alu->op].input_types[index];
                  if (nir_alu_type_get_base_type(type) != nir_type_float) {
                        return false;
                  }
            } else if (user_instr->type == nir_instr_type_intrinsic) {
                  /* Check for intrinsics that consume floats. */
                  switch (nir_instr_as_intrinsic(user_instr)->intrinsic) {
                        case nir_intrinsic_ddx:
                        case nir_intrinsic_ddy:
                        case nir_intrinsic_ddx_fine:
                        case nir_intrinsic_ddy_fine:
                        case nir_intrinsic_ddx_coarse:
                        case nir_intrinsic_ddy_coarse:
                              continue; /* These are float operations. */
                        default:
                              return false; /* Not a known float-consuming intrinsic. */
                  }
            } else if (user_instr->type == nir_instr_type_tex) {
                  /* Check texture instruction source types. */
                  const nir_tex_instr *tex = nir_instr_as_tex(user_instr);
                  const nir_tex_src *tex_src = container_of(src, nir_tex_src, src);
                  const unsigned idx = tex_src - tex->src;

                  /* Backend-specific sources have unknown types, be conservative. */
                  if (tex_src->src_type == nir_tex_src_backend1 ||
                        tex_src->src_type == nir_tex_src_backend2) {
                        return false;
                        }

                        if (nir_tex_instr_src_type(tex, idx) != nir_type_float) {
                              return false;
                        }
            } else {
                  /* Any other instruction type is assumed to not be a float use. */
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if an SSA definition is only used by float operations.
 *
 * This is critical for optimizations like `f2i(i2f(x)) -> x`, which is only
 * valid if the intermediate float value is not bitcast or used as an integer.
 * This function recursively checks through mov-like operations to find the
 * true consumers.
 *
 * @return True if all transitive uses are float operations.
 */
static inline bool
is_only_used_as_float(const nir_alu_instr *instr)
{
      return is_only_used_as_float_impl(instr, 0);
}

/**
 * @brief Check if an SSA definition is only used by fadd, fabs, or fneg.
 *
 * Recursively checks through a chain of `fabs` and `fneg` to see if the
 * ultimate consumer is an `fadd`. This is useful for FMA/MAD patterns.
 *
 * @return True if the value is only consumed by a chain leading to fadd.
 */
static inline bool
is_only_used_by_fadd(const nir_alu_instr *instr)
{
      if (list_is_empty(&instr->def.uses))
            return false;

      /* An instruction must have only a single use to be a candidate for
       * fusing into an FMA, otherwise we would be duplicating code or
       * incorrectly altering the value for other users.
       */
      if (!list_is_singular(&instr->def.uses))
            return false;

      nir_foreach_use(src, &instr->def) {
            const nir_instr *user_instr = nir_src_parent_instr(src);
            if (user_instr->type != nir_instr_type_alu) {
                  return false;
            }

            const nir_alu_instr *user_alu = nir_instr_as_alu(user_instr);

            if (user_alu->op == nir_op_fneg || user_alu->op == nir_op_fabs) {
                  /* Recurse through free modifiers, which must also be singly-used. */
                  if (!is_only_used_by_fadd(user_alu)) {
                        return false;
                  }
            } else if (user_alu->op != nir_op_fadd) {
                  return false;
            }
      }
      return true;
}

/**
 * @brief Generic helper to check if a value is only used by a specific ALU op.
 */
static inline bool
is_only_used_by_alu_op(const nir_alu_instr *instr, nir_op op)
{
      if (list_is_empty(&instr->def.uses)) {
            return false; /* A value with no uses is not used by the op. */
      }

      nir_foreach_use(src, &instr->def) {
            const nir_instr *const user_instr = nir_src_parent_instr(src);
            if (user_instr->type != nir_instr_type_alu) {
                  return false;
            }

            const nir_alu_instr *const user_alu = nir_instr_as_alu(user_instr);
            if (user_alu->op != op) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if an SSA definition is only used by iadd.
 */
static inline bool
is_only_used_by_iadd(const nir_alu_instr *instr)
{
      return is_only_used_by_alu_op(instr, nir_op_iadd);
}

/**
 * @brief Check if an SSA definition is only used by iand.
 */
static inline bool
is_only_used_by_iand(const nir_alu_instr *instr)
{
      return is_only_used_by_alu_op(instr, nir_op_iand);
}

/**
 * @brief Check if an SSA definition is only used by ior.
 */
static inline bool
is_only_used_by_ior(const nir_alu_instr *instr)
{
      return is_only_used_by_alu_op(instr, nir_op_ior);
}

/**
 * @brief Check if only the lower 8 bits of an SSA definition are ever used.
 *
 * Relies on `nir_dead_cf` and `nir_opt_undef` having been run to compute
 * the `bits_used` mask.
 */
static inline bool
only_lower_8_bits_used(const nir_alu_instr *instr)
{
      return (nir_def_bits_used(&instr->def) & ~0xffULL) == 0;
}

/**
 * @brief Check if only the lower 16 bits of an SSA definition are ever used.
 *
 * Relies on `nir_dead_cf` and `nir_opt_undef` having been run to compute
 * the `bits_used` mask.
 */
static inline bool
only_lower_16_bits_used(const nir_alu_instr *instr)
{
      return (nir_def_bits_used(&instr->def) & ~0xffffULL) == 0;
}

/**
 * @brief Returns true if the upper half of a 32/64-bit constant is all zeros.
 */
static inline bool
is_upper_half_zero(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
                   unsigned src, unsigned num_components,
                   const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const unsigned half_bit_size = nir_src_bit_size(instr->src[src].src) / 2;
            const uint64_t high_bits = u_bit_consecutive64(half_bit_size, half_bit_size);
            if ((nir_src_comp_as_uint(instr->src[src].src, swizzle[i]) & high_bits) != 0) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Returns true if the lower half of a 32/64-bit constant is all zeros.
 */
static inline bool
is_lower_half_zero(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
                   unsigned src, unsigned num_components,
                   const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const unsigned half_bit_size = nir_src_bit_size(instr->src[src].src) / 2;
            const uint64_t low_bits = BITFIELD64_MASK(half_bit_size);
            if ((nir_src_comp_as_uint(instr->src[src].src, swizzle[i]) & low_bits) != 0) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Returns true if the upper half of a 32/64-bit constant is all ones.
 */
static inline bool
is_upper_half_negative_one(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
                           unsigned src, unsigned num_components,
                           const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const unsigned half_bit_size = nir_src_bit_size(instr->src[src].src) / 2;
            const uint64_t high_bits = u_bit_consecutive64(half_bit_size, half_bit_size);
            if ((nir_src_comp_as_uint(instr->src[src].src, swizzle[i]) & high_bits) != high_bits) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Returns true if the lower half of a 32/64-bit constant is all ones.
 */
static inline bool
is_lower_half_negative_one(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
                           unsigned src, unsigned num_components,
                           const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const unsigned half_bit_size = nir_src_bit_size(instr->src[src].src) / 2;
            const uint64_t low_bits = BITFIELD64_MASK(half_bit_size);
            if ((nir_src_comp_as_uint(instr->src[src].src, swizzle[i]) & low_bits) != low_bits) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if a constant is a bitmask of consecutive LSBs (e.g., 0x00FF).
 *
 * This checks for numbers of the form `2^n - 1`. Such constants can often
 * be generated more efficiently. The all-ones case is excluded as it's
 * usually handled separately.
 *
 * @return True if the constant is a bitmask.
 */
static inline bool
is_const_bitmask(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
                 unsigned src, unsigned num_components,
                 const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const unsigned bit_size = instr->src[src].src.ssa->bit_size;
            const uint64_t c = nir_src_comp_as_uint(instr->src[src].src, swizzle[i]);
            const unsigned num_bits = util_bitcount64(c);

            /* A bitmask must have its lowest `num_bits` set, and nothing else.
             * Also exclude the all-ones value.
             */
            if (c == 0 || c != BITFIELD64_MASK(num_bits) || num_bits == bit_size) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if a constant can be created by a bitfield move (BFM).
 *
 * This checks for non-zero constants that have a consecutive run of set
 * bits anywhere in the value (e.g., 0x0FF0). These can be generated by
 * `bfm` on GFX hardware.
 *
 * @return True if the constant is a field of set bits.
 */
static inline bool
is_const_bfm(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
             unsigned src, unsigned num_components,
             const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const unsigned bit_size = instr->src[src].src.ssa->bit_size;
            const uint64_t c = nir_src_comp_as_uint(instr->src[src].src, swizzle[i]);

            if (c == 0) {
                  return false;
            }

            const unsigned num_bits = util_bitcount64(c);
            const unsigned offset = ffsll(c) - 1;

            /* Check if the bits are consecutive. Exclude all-ones. */
            if (c != (BITFIELD64_MASK(num_bits) << offset) || num_bits == bit_size) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if the 5 least significant bits of a constant are non-zero.
 *
 * Used for bit manipulation patterns where the low 5 bits often represent
 * a shift or mask amount that cannot be zero.
 *
 * @return True if `(val & 0x1f) != 0` for all components.
 */
static inline bool
is_5lsb_not_zero(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
                 unsigned src, unsigned num_components,
                 const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false;
      }

      for (unsigned i = 0; i < num_components; i++) {
            const uint32_t val = nir_src_comp_as_uint(instr->src[src].src, swizzle[i]);
            if ((val & 0x1f) == 0) {
                  return false;
            }
      }

      return true;
}

/**
 * @brief Check if a value is not the maximum for its unsigned bit size.
 *
 * Used to ensure safe increment operations (`iadd(x, 1)`) won't overflow
 * and wrap around, which could break optimizations. If the value is not a
 * constant, we conservatively assume it's not UINT_MAX.
 *
 * @return True if the value is not provably UINT_MAX.
 */
static inline bool
is_not_uint_max(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
                unsigned src, unsigned num_components,
                const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return true; /* Not a constant, assume it might not be max. */
      }

      const uint64_t uint_max = u_uintN_max(instr->src[src].src.ssa->bit_size);

      for (unsigned i = 0; i < num_components; i++) {
            const uint64_t val = nir_src_comp_as_uint(instr->src[src].src, swizzle[i]);
            if (val == uint_max) {
                  return false;
            }
      }
      return true;
}

/**
 * @brief Check if the instruction has the `no_signed_wrap` flag set.
 */
static inline bool
no_signed_wrap(const nir_alu_instr *instr)
{
      return instr->no_signed_wrap;
}

/**
 * @brief Check if the instruction has the `no_unsigned_wrap` flag set.
 */
static inline bool
no_unsigned_wrap(const nir_alu_instr *instr)
{
      return instr->no_unsigned_wrap;
}

/**
 * @brief Use range analysis to check if a value is known to be integral.
 */
static inline bool
is_integral(struct hash_table *ht, const nir_alu_instr *instr, unsigned src,
            UNUSED unsigned num_components, UNUSED const uint8_t *swizzle)
{
      const struct ssa_result_range r = nir_analyze_range(ht, instr, src);
      return r.is_integral;
}

/**
 * @brief Use range analysis to check if a value is known to be finite.
 */
static inline bool
is_finite(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
          unsigned src, UNUSED unsigned num_components,
          UNUSED const uint8_t *swizzle)
{
      const struct ssa_result_range v = nir_analyze_range(ht, instr, src);
      return v.is_finite;
}

/**
 * @brief Use range analysis to check if a value is finite and not zero.
 */
static inline bool
is_finite_not_zero(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
                   unsigned src, UNUSED unsigned num_components,
                   UNUSED const uint8_t *swizzle)
{
      const struct ssa_result_range v = nir_analyze_range(ht, instr, src);
      return v.is_finite &&
      (v.range == lt_zero || v.range == gt_zero || v.range == ne_zero);
}

#define RELATION(r)                                                        \
static inline bool                                                      \
is_##r(struct hash_table *ht, const nir_alu_instr *instr,            \
unsigned src, UNUSED unsigned num_components,                 \
UNUSED const uint8_t *swizzle)                                \
{                                                                       \
      const struct ssa_result_range v = nir_analyze_range(ht, instr, src); \
      return v.range == r;                                                 \
}                                                                       \
\
static inline bool                                                      \
is_a_number_##r(struct hash_table *ht, const nir_alu_instr *instr,   \
unsigned src, UNUSED unsigned num_components,        \
UNUSED const uint8_t *swizzle)                       \
{                                                                       \
      const struct ssa_result_range v = nir_analyze_range(ht, instr, src); \
      return v.is_a_number && v.range == r;                                \
}

RELATION(lt_zero)
RELATION(le_zero)
RELATION(gt_zero)
RELATION(ge_zero)
RELATION(ne_zero)

#undef RELATION

/**
 * @brief Use range analysis to check if a value is not negative (>= 0).
 */
static inline bool
is_not_negative(struct hash_table *ht, const nir_alu_instr *instr, unsigned src,
                UNUSED unsigned num_components, UNUSED const uint8_t *swizzle)
{
      const struct ssa_result_range v = nir_analyze_range(ht, instr, src);
      return v.range == ge_zero || v.range == gt_zero || v.range == eq_zero;
}

/**
 * @brief Use range analysis to check if a value is a number and not negative.
 */
static inline bool
is_a_number_not_negative(struct hash_table *ht, const nir_alu_instr *instr,
                         unsigned src, UNUSED unsigned num_components,
                         UNUSED const uint8_t *swizzle)
{
      const struct ssa_result_range v = nir_analyze_range(ht, instr, src);
      return v.is_a_number &&
      (v.range == ge_zero || v.range == gt_zero || v.range == eq_zero);
}

/**
 * @brief Use range analysis to check if a value is not positive (<= 0).
 */
static inline bool
is_not_positive(struct hash_table *ht, const nir_alu_instr *instr, unsigned src,
                UNUSED unsigned num_components, UNUSED const uint8_t *swizzle)
{
      const struct ssa_result_range v = nir_analyze_range(ht, instr, src);
      return v.range == le_zero || v.range == lt_zero || v.range == eq_zero;
}

/**
 * @brief Use range analysis to check if a value is a number and not positive.
 */
static inline bool
is_a_number_not_positive(struct hash_table *ht, const nir_alu_instr *instr,
                         unsigned src, UNUSED unsigned num_components,
                         UNUSED const uint8_t *swizzle)
{
      const struct ssa_result_range v = nir_analyze_range(ht, instr, src);
      return v.is_a_number &&
      (v.range == le_zero || v.range == lt_zero || v.range == eq_zero);
}

/**
 * @brief Check if a constant value is not zero.
 *
 * This provides a strong guarantee: it only returns true if the source is a
 * constant and all its components are non-zero. It returns false if the
 * source is not a constant or if any component is zero.
 */
static inline bool
is_not_zero(UNUSED struct hash_table *ht, const nir_alu_instr *instr,
            unsigned src, unsigned num_components,
            const uint8_t *swizzle)
{
      const nir_const_value *cv = nir_src_as_const_value(instr->src[src].src);
      if (unlikely(!cv)) {
            return false; /* Not a constant, can't guarantee non-zero. */
      }

      for (unsigned i = 0; i < num_components; i++) {
            if (nir_src_comp_as_uint(instr->src[src].src, swizzle[i]) == 0) {
                  return false;
            }
      }
      return true;
}

/**
 * @brief Use range analysis to check if a value is a number and not zero.
 */
static inline bool
is_a_number_not_zero(struct hash_table *ht, const nir_alu_instr *instr,
                     unsigned src, UNUSED unsigned num_components,
                     UNUSED const uint8_t *swizzle)
{
      const struct ssa_result_range v = nir_analyze_range(ht, instr, src);
      return v.is_a_number &&
      (v.range == lt_zero || v.range == gt_zero || v.range == ne_zero);
}

/**
 * @brief Use range analysis to check if a value is a number (not Inf or NaN).
 */
static inline bool
is_a_number(struct hash_table *ht, const nir_alu_instr *instr, unsigned src,
            UNUSED unsigned num_components, UNUSED const uint8_t *swizzle)
{
      const struct ssa_result_range v = nir_analyze_range(ht, instr, src);
      return v.is_a_number;
}

#endif /* _NIR_SEARCH_HELPERS_ */
