import argparse
from collections import OrderedDict
import itertools
import struct
from math import pi
import math

import nir_algebraic
from nir_algebraic import TestStatus
from nir_opcodes import type_sizes

a = 'a'
b = 'b'
c = 'c'
d = 'd'
e = 'e'
NAN = math.nan

has_fmulz = ('(options->has_fmulz || '
             '(options->has_fmulz_no_denorms && '
             '!nir_is_denorm_preserve(info->float_controls_execution_mode, 32)))')

denorm_ftz_16 = 'nir_is_denorm_flush_to_zero(info->float_controls_execution_mode, 16)'
denorm_ftz_32 = 'nir_is_denorm_flush_to_zero(info->float_controls_execution_mode, 32)'
denorm_ftz_64 = 'nir_is_denorm_flush_to_zero(info->float_controls_execution_mode, 64)'

def lowered_sincos(c):
    """
    Fast sine/cosine approximation using parabolic curve fitting.
    Maximum error ~0.001 for typical shader inputs.
    Reference: devmaster.net fast sine/cosine approximation.
    """
    x = ('fsub', ('fmul', 2.0, ('ffract', ('fadd', ('fmul', 0.5 / pi, a), c))), 1.0)
    x = ('fmul', ('fsub', x, ('fmul', x, ('fabs', x))), 4.0)
    return ('ffma', ('ffma', x, ('fabs', x), ('fneg', x)), 0.225, x)


def intBitsToFloat(i):
    """Reinterpret 32-bit integer as IEEE 754 float."""
    return struct.unpack('!f', struct.pack('!I', i))[0]


def add_fabs_fneg(pattern, replacements, commutative=True):
    """
    Generate pattern permutations with fabs/fneg modifiers applied to operands.

    For each operand in replacements dict, generates all combinations of:
    - fneg(fabs(x))
    - fabs(x)
    - fneg(x)
    - x (unmodified)

    Args:
        pattern: Tuple of (search_expr, replace_expr, optional_condition)
        replacements: Dict mapping placeholder names to base variable names
        commutative: If True, use combinations (fewer patterns); else use product

    Returns:
        List of pattern tuples with all modifier permutations
    """
    def to_list(pattern):
        return [to_list(i) if isinstance(i, tuple) else i for i in pattern]

    def to_tuple(pattern):
        return tuple(to_tuple(i) if isinstance(i, list) else i for i in pattern)

    def replace_variable(pattern, search, replace):
        for i in range(len(pattern)):
            if pattern[i] == search:
                pattern[i] = replace
            elif isinstance(pattern[i], list):
                replace_variable(pattern[i], search, replace)

    if commutative:
        perms = itertools.combinations_with_replacement(range(4), len(replacements))
    else:
        perms = itertools.product(range(4), repeat=len(replacements))

    result = []

    for perm in perms:
        curr = to_list(pattern)

        for i, (search, base) in enumerate(replacements.items()):
            if perm[i] == 0:
                replace = ['fneg', ['fabs', base]]
            elif perm[i] == 1:
                replace = ['fabs', base]
            elif perm[i] == 2:
                replace = ['fneg', base]
            else:
                replace = base

            replace_variable(curr, search, replace)

        result.append(to_tuple(curr))

    return result


optimize_fcanonicalize = [
    (('fcanonicalize', 'a@16'), a, '!' + denorm_ftz_16),
    (('fcanonicalize', 'a@32'), a, '!' + denorm_ftz_32),
    (('fcanonicalize', 'a@64'), a, '!' + denorm_ftz_64),
    (('fcanonicalize(is_only_used_as_float)', a), a),
    (('fcanonicalize', 'a(is_created_as_float)'), a, 'true', TestStatus.UNSUPPORTED),
    (('fcanonicalize', 'a(is_integral)'), a),
]

optimizations = [
    (('fgeu', a, b), ('inot', ('flt', a, b))),
    (('fltu', a, b), ('inot', ('fge', a, b))),
    (('fneo', 0.0, a), ('flt', 0.0, ('fabs', a))),
    (('fequ', 0.0, a), ('inot', ('flt', 0.0, ('fabs', a)))),

    (('imul', a, '#b(is_pos_power_of_two)'), ('ishl', a, ('find_lsb', b)), '!options->lower_bitops'),
    (('imul', 'a@8', 0x80), ('ishl', a, 7), '!options->lower_bitops'),
    (('imul', 'a@16', 0x8000), ('ishl', a, 15), '!options->lower_bitops'),
    (('imul', 'a@32', 0x80000000), ('ishl', a, 31), '!options->lower_bitops'),
    (('imul', 'a@64', 0x8000000000000000), ('ishl', a, 63), '!options->lower_bitops'),
    (('imul', a, '#b(is_neg_power_of_two)'), ('ineg', ('ishl', a, ('find_lsb', ('iabs', b)))), '!options->lower_bitops'),
    (('ishl', a, '#b'), ('imul', a, ('ishl', 1, b)), 'options->lower_bitops'),

    (('imul@64', a, '#b(is_bitcount2)'), ('iadd', ('ishl', a, ('ufind_msb', b)), ('ishl', a, ('find_lsb', b))),
     '!options->lower_bitops && (options->lower_int64_options & (nir_lower_imul64 | nir_lower_shift64)) == nir_lower_imul64'),

    (('unpack_64_2x32_split_x', ('imul_2x32_64(is_used_once)', a, b)), ('imul', a, b)),
    (('unpack_64_2x32_split_x', ('umul_2x32_64(is_used_once)', a, b)), ('imul', a, b)),
    (('imul_2x32_64', a, b), ('pack_64_2x32_split', ('imul', a, b), ('imul_high', a, b)), 'options->lower_mul_2x32_64'),
    (('umul_2x32_64', a, b), ('pack_64_2x32_split', ('imul', a, b), ('umul_high', a, b)), 'options->lower_mul_2x32_64'),

    (('udiv', a, 1), a),
    (('idiv', a, 1), a),
    (('umod', a, 1), 0),
    (('imod', a, 1), 0),
    (('imod', a, -1), 0),
    (('irem', a, 1), 0),
    (('irem', a, -1), 0),

    (('udiv', a, '#b(is_pos_power_of_two)'), ('ushr', a, ('find_lsb', b)), '!options->lower_bitops'),
    (('idiv', a, '#b(is_pos_power_of_two)'), ('imul', ('isign', a), ('ushr', ('iabs', a), ('find_lsb', b))), '!options->lower_bitops'),
    (('idiv', a, '#b(is_neg_power_of_two)'), ('ineg', ('imul', ('isign', a), ('ushr', ('iabs', a), ('find_lsb', ('iabs', b))))), '!options->lower_bitops'),
    (('umod', a, '#b(is_pos_power_of_two)'), ('iand', a, ('isub', b, 1)), '!options->lower_bitops'),
    (('imod', a, '#b(is_pos_power_of_two)'), ('iand', a, ('isub', b, 1)), '!options->lower_bitops'),
    (('imod', a, '#b(is_neg_power_of_two)'), ('bcsel', ('ieq', ('ior', a, b), b), 0, ('ior', a, b)), '!options->lower_bitops'),
    (('irem', a, '#b(is_pos_power_of_two)'),
     ('isub', a, ('iand', ('bcsel', ('ilt', a, 0), ('iadd', a, ('isub', b, 1)), a), ('ineg', b))),
     '!options->lower_bitops'),
    (('irem', a, '#b(is_neg_power_of_two)'), ('irem', a, ('iabs', b)), '!options->lower_bitops'),

    (('~fmul', ('fsign', a), ('ffloor', ('fadd', ('fabs', a), 0.5))),
     ('ftrunc', ('fadd', a, ('fmul', ('fsign', a), 0.5))),
     '!options->lower_ftrunc || options->lower_ffloor'),

    (('fneg', ('fneg', a)), ('fcanonicalize', a)),
    (('ineg', ('ineg', a)), a),
    (('fabs', ('fneg', a)), ('fabs', a)),
    (('fabs', ('u2f', a)), ('u2f', a)),
    (('iabs', ('iabs', a)), ('iabs', a)),
    (('iabs', ('ineg', a)), ('iabs', a)),

    (('fadd(nsz)', a, 0.0), ('fcanonicalize', a)),
    (('fadd', a, -0.0), ('fcanonicalize', a)),
    (('iadd', a, 0), a),
    (('iadd_sat', a, 0), a),
    (('isub_sat', a, 0), a),
    (('uadd_sat', a, 0), a),
    (('usub_sat', a, 0), a),
    (('usadd_4x8_vc4', a, 0), a),
    (('usadd_4x8_vc4', a, ~0), ~0),

    (('~fadd', ('fmul', a, b), ('fmul', a, c)), ('fmul', a, ('fadd', b, c))),
    (('~fadd', ('fmulz', a, b), ('fmulz', a, c)), ('fmulz', a, ('fadd', b, c))),
    (('~ffma', a, b, ('ffma(is_used_once)', a, c, d)), ('ffma', a, ('fadd', b, c), d)),
    (('~ffma', a, b, ('fmul(is_used_once)', a, c)), ('fmul', a, ('fadd', b, c))),
    (('~fadd', ('fmul(is_used_once)', a, b), ('ffma(is_used_once)', a, c, d)), ('ffma', a, ('fadd', b, c), d)),
    (('~ffma', a, ('fmul(is_used_once)', b, c), ('fmul(is_used_once)', b, d)), ('fmul', b, ('ffma', a, c, d))),
    (('~ffmaz', a, b, ('ffmaz(is_used_once)', a, c, d)), ('ffmaz', a, ('fadd', b, c), d)),
    (('~ffmaz', a, b, ('fmulz(is_used_once)', a, c)), ('fmulz', a, ('fadd', b, c))),
    (('~fadd', ('fmulz(is_used_once)', a, b), ('ffmaz(is_used_once)', a, c, d)), ('ffmaz', a, ('fadd', b, c), d)),
    (('~ffmaz', a, ('fmulz(is_used_once)', b, c), ('fmulz(is_used_once)', b, d)), ('fmulz', b, ('ffmaz', a, c, d))),

    (('iadd', ('imul', a, b), ('imul', a, c)), ('imul', a, ('iadd', b, c))),
    (('iadd', ('ishl', b, a), ('ishl', c, a)), ('ishl', ('iadd', b, c), a)),
    (('iand', ('iand', a, b), ('iand(is_used_once)', a, c)), ('iand', ('iand', a, b), c)),
    (('ior', ('ior', a, b), ('ior(is_used_once)', a, c)), ('ior', ('ior', a, b), c)),
    (('iand', ('ior(is_used_once)', a, b), ('ior(is_used_once)', a, c)), ('ior', a, ('iand', b, c))),
    (('ior', ('iand(is_used_once)', a, b), ('iand(is_used_once)', a, c)), ('iand', a, ('ior', b, c))),
    (('ior', ('iand', a, b), ('ior', a, c)), ('ior', a, c)),
    (('iand', ('iand', a, b), ('ior', a, c)), ('iand', a, b)),

    (('ieq', ('iand', a, '#b(is_pos_power_of_two)'), b), ('ine', ('iand', a, b), 0)),
    (('ine', ('iand', a, '#b(is_pos_power_of_two)'), b), ('ieq', ('iand', a, b), 0)),
    (('uge', ('iand', a, '#b(is_pos_power_of_two)'), b), ('ine', ('iand', a, b), 0)),
    (('ult', ('iand', a, '#b(is_pos_power_of_two)'), b), ('ieq', ('iand', a, b), 0)),
    (('ige', ('iand', a, b), '#b(is_pos_power_of_two)'), ('ine', ('iand', a, b), 0)),
    (('ilt', ('iand', a, b), '#b(is_pos_power_of_two)'), ('ieq', ('iand', a, b), 0)),
    (('ieq', ('ushr(is_used_once)', a, '#b'), 0), ('ult', a, ('ishl', 1, b))),
    (('ine', ('ushr(is_used_once)', a, '#b'), 0), ('uge', a, ('ishl', 1, b))),

    (('~fadd', ('fneg', a), a), 0.0),
    (('iadd', ('ineg', a), a), 0),
    (('iadd', ('ineg', a), ('iadd', a, b)), b),
    (('iadd', a, ('iadd', ('ineg', a), b)), b),
    (('~fadd', ('fneg', a), ('fadd', a, b)), ('fcanonicalize', b)),
    (('~fadd', a, ('fadd', ('fneg', a), b)), ('fcanonicalize', b)),
    (('fadd', ('fsat', a), ('fsat', ('fneg', a))), ('fsat', ('fabs', a))),

    (('fadd', a, a), ('fmul', a, 2.0)),
    (('fadd(contract)', a, ('fadd(is_used_once)', a, b)), ('fadd', b, ('fmul', a, 2.0))),

    (('~fmul', a, 0.0), 0.0),
    (('~fmul', a, -0.0), 0.0),
    (('fmul(nsz,nnan)', 'a', 0.0), 0.0),
    (('fmul(nsz,nnan)', 'a', -0.0), 0.0),
    (('fmulz', a, 0.0), 0.0),
    (('fmulz', a, -0.0), 0.0),
    (('fmulz(nsz)', a, 'b(is_finite_not_zero)'), ('fmul', a, b)),
    (('fmulz', 'a(is_finite)', 'b(is_finite)'), ('fmul', a, b), 'true', TestStatus.XFAIL),
    (('fmulz', a, a), ('fmul', a, a)),
    (('ffmaz(nsz)', a, 'b(is_finite_not_zero)', c), ('ffma', a, b, c)),
    (('ffmaz', 'a(is_finite)', 'b(is_finite)', c), ('ffma', a, b, c)),
    (('ffmaz', a, a, b), ('ffma', a, a, b)),

    (('imul', a, 0), 0),
    (('umul_unorm_4x8_vc4', a, 0), 0),
    (('umul_unorm_4x8_vc4', a, ~0), a),
    (('fmul', a, 1.0), ('fcanonicalize', a)),
    (('imul', a, 1), a),
    (('fmul', a, -1.0), ('fneg', a)),
    (('imul', a, -1), ('ineg', a)),

    (('fmul', ('fsign', a), ('fmul', a, a)), ('fmul', ('fabs', a), a)),
    (('fmul', ('fmul', ('fsign', a), a), a), ('fmul', ('fabs', a), a)),

    (('ffma(nsz,nnan)', 0.0, a, b), ('fcanonicalize', b)),
    (('ffma(nsz,nnan)', -0.0, a, b), ('fcanonicalize', b)),
    (('ffmaz', 0.0, a, b), ('fadd', 0.0, b)),
    (('ffmaz', -0.0, a, b), ('fadd', 0.0, b)),
    (('ffma(nsz)', a, b, 0.0), ('fmul', a, b)),
    (('ffmaz(nsz)', a, b, 0.0), ('fmulz', a, b)),
    (('ffma', a, b, -0.0), ('fmul', a, b)),
    (('ffmaz', a, b, -0.0), ('fmulz', a, b)),
    (('ffma', 1.0, a, b), ('fadd', a, b)),
    (('ffmaz(nsz)', 1.0, a, b), ('fadd', a, b)),
    (('ffma', -1.0, a, b), ('fadd', ('fneg', a), b)),
    (('ffmaz(nsz)', -1.0, a, b), ('fadd', ('fneg', a), b)),
    (('~ffma', '#a', '#b', c), ('fadd', ('fmul', a, b), c)),
    (('~ffmaz', '#a', '#b', c), ('fadd', ('fmulz', a, b), c)),

    (('flrp(nnan,nsz)', a, b, 0.0), ('fcanonicalize', a)),
    (('flrp(nnan,nsz)', a, b, -0.0), ('fcanonicalize', a)),
    (('flrp(nnan,nsz)', a, b, 1.0), ('fcanonicalize', b)),
    (('~flrp', a, a, b), ('fcanonicalize', a)),
    (('flrp(nnan,nsz)', 0.0, a, b), ('fmul', a, b)),
    (('flrp(nnan,nsz)', -0.0, a, b), ('fmul', a, b)),
    (('~flrp', a, ('fadd(is_used_once)', a, b), c), ('fadd', ('fmul', b, c), a)),

    (('sdot_4x8_iadd', a, 0, b), b),
    (('udot_4x8_uadd', a, 0, b), b),
    (('sdot_4x8_iadd_sat', a, 0, b), b),
    (('udot_4x8_uadd_sat', a, 0, b), b),
    (('sdot_2x16_iadd', a, 0, b), b),
    (('udot_2x16_uadd', a, 0, b), b),
    (('sdot_2x16_iadd_sat', a, 0, b), b),
    (('udot_2x16_uadd_sat', a, 0, b), b),

    (('sudot_4x8_iadd', a, 0, b), b),
    (('sudot_4x8_iadd', 0, a, b), b),
    (('sudot_4x8_iadd_sat', a, 0, b), b),
    (('sudot_4x8_iadd_sat', 0, a, b), b),

    (('iadd', ('sdot_4x8_iadd(is_used_once)', a, b, '#c'), '#d'), ('sdot_4x8_iadd', a, b, ('iadd', c, d))),
    (('iadd', ('udot_4x8_uadd(is_used_once)', a, b, '#c'), '#d'), ('udot_4x8_uadd', a, b, ('iadd', c, d))),
    (('iadd', ('sudot_4x8_iadd(is_used_once)', a, b, '#c'), '#d'), ('sudot_4x8_iadd', a, b, ('iadd', c, d))),
    (('iadd', ('sdot_2x16_iadd(is_used_once)', a, b, '#c'), '#d'), ('sdot_2x16_iadd', a, b, ('iadd', c, d))),
    (('iadd', ('udot_2x16_uadd(is_used_once)', a, b, '#c'), '#d'), ('udot_2x16_uadd', a, b, ('iadd', c, d))),

    (('iadd', ('sdot_4x8_iadd', 'a(is_not_const)', b, 0), c), ('sdot_4x8_iadd', a, b, c)),
    (('iadd', ('udot_4x8_uadd', 'a(is_not_const)', b, 0), c), ('udot_4x8_uadd', a, b, c)),
    (('iadd', ('sudot_4x8_iadd', 'a(is_not_const)', b, 0), c), ('sudot_4x8_iadd', a, b, c)),
    (('iadd', ('sudot_4x8_iadd', a, 'b(is_not_const)', 0), c), ('sudot_4x8_iadd', a, b, c)),
    (('iadd', ('sdot_2x16_iadd', 'a(is_not_const)', b, 0), c), ('sdot_2x16_iadd', a, b, c)),
    (('iadd', ('udot_2x16_uadd', 'a(is_not_const)', b, 0), c), ('udot_2x16_uadd', a, b, c)),
    (('sdot_4x8_iadd', '#a', '#b', 'c(is_not_const)'), ('iadd', ('sdot_4x8_iadd', a, b, 0), c)),
    (('udot_4x8_uadd', '#a', '#b', 'c(is_not_const)'), ('iadd', ('udot_4x8_uadd', a, b, 0), c)),
    (('sudot_4x8_iadd', '#a', '#b', 'c(is_not_const)'), ('iadd', ('sudot_4x8_iadd', a, b, 0), c)),
    (('sdot_2x16_iadd', '#a', '#b', 'c(is_not_const)'), ('iadd', ('sdot_2x16_iadd', a, b, 0), c)),
    (('udot_2x16_uadd', '#a', '#b', 'c(is_not_const)'), ('iadd', ('udot_2x16_uadd', a, b, 0), c)),
    (('sdot_4x8_iadd_sat', '#a', '#b', 'c(is_not_const)'), ('iadd_sat', ('sdot_4x8_iadd', a, b, 0), c), '!options->lower_iadd_sat'),
    (('udot_4x8_uadd_sat', '#a', '#b', 'c(is_not_const)'), ('uadd_sat', ('udot_4x8_uadd', a, b, 0), c), '!options->lower_uadd_sat'),
    (('sudot_4x8_iadd_sat', '#a', '#b', 'c(is_not_const)'), ('iadd_sat', ('sudot_4x8_iadd', a, b, 0), c), '!options->lower_iadd_sat'),
    (('sdot_2x16_iadd_sat', '#a', '#b', 'c(is_not_const)'), ('iadd_sat', ('sdot_2x16_iadd', a, b, 0), c), '!options->lower_iadd_sat'),
    (('udot_2x16_uadd_sat', '#a', '#b', 'c(is_not_const)'), ('uadd_sat', ('udot_2x16_uadd', a, b, 0), c), '!options->lower_uadd_sat'),

    *add_fabs_fneg((('fmul@32(nsz)', ('bcsel', ('feq(ignore_exact)', b, 0.0), 0.0, 'ma'), ('bcsel', ('feq(ignore_exact)', a, 0.0), 0.0, 'mb')),
     ('fmulz', 'ma', 'mb'), has_fmulz), {'ma': a, 'mb': b}),
    *add_fabs_fneg((('fmul@32(nsz)', ('bcsel', ('fneu(ignore_exact)', b, 0.0), 'ma', 0.0), ('bcsel', ('feq(ignore_exact)', a, 0.0), 0.0, 'mb')),
     ('fmulz', 'ma', 'mb'), has_fmulz), {'ma': a, 'mb': b}),
    *add_fabs_fneg((('fmul@32(nsz)', ('bcsel', ('fneu(ignore_exact)', b, 0.0), 'ma', 0.0), ('bcsel', ('fneu(ignore_exact)', a, 0.0), 'mb', 0.0)),
     ('fmulz', 'ma', 'mb'), has_fmulz), {'ma': a, 'mb': b}),

    *add_fabs_fneg((('bcsel', ('feq', ('fmin', ('fabs', a), ('fabs', b)), 0.0), 0.0, ('fmul@32', 'ma', 'mb')),
     ('fmulz', 'ma', 'mb'), has_fmulz), {'ma': a, 'mb': b}),

    *add_fabs_fneg((('fmul@32(nsz)', 'ma', ('bcsel', ('feq(ignore_exact)', a, 0.0), 0.0, '#b(is_not_const_zero)')),
     ('fmulz', 'ma', b), has_fmulz), {'ma': a}),

    *add_fabs_fneg((('ffma@32(nsz)', ('bcsel', ('feq(ignore_exact)', b, 0.0), 0.0, 'ma'), ('bcsel', ('feq(ignore_exact)', a, 0.0), 0.0, 'mb'), c),
     ('ffmaz', 'ma', 'mb', c), has_fmulz), {'ma': a, 'mb': b}),
    *add_fabs_fneg((('ffma@32(nsz)', 'ma', ('bcsel', ('feq(ignore_exact)', a, 0.0), 0.0, '#b(is_not_const_zero)'), c),
     ('ffmaz', 'ma', b, c), has_fmulz), {'ma': a}),

    *add_fabs_fneg((('bcsel(nsz,nnan,ninf)', ('feq(ignore_exact)', b, 0.0), 1.0, ('fexp2', ('fmul@32', a, 'mb'))),
     ('fexp2', ('fmulz', a, 'mb')),
     has_fmulz), {'mb': b}),
    *add_fabs_fneg((('bcsel', ('feq(ignore_exact)', b, 0.0), 1.0, ('fexp2', ('fmulz', a, 'mb'))),
     ('fexp2', ('fmulz', a, 'mb'))), {'mb': b}),
]

for sz in (16, 32, 64):
    sign_bit = 1 << (sz - 1)
    optimizations.extend([
        (('iand(is_only_used_as_float)', f'a@{sz}', sign_bit - 1), ('fabs', a)),
        (('ixor(is_only_used_as_float)', f'a@{sz}', sign_bit), ('fneg', a)),
        (('ior(is_only_used_as_float)', f'a@{sz}', sign_bit), ('fneg', ('fabs', a))),
    ])

sdot_4x8_a_b = ('iadd', ('iadd', ('imul', ('extract_i8', a, 0), ('extract_i8', b, 0)),
                                 ('imul', ('extract_i8', a, 1), ('extract_i8', b, 1))),
                        ('iadd', ('imul', ('extract_i8', a, 2), ('extract_i8', b, 2)),
                                 ('imul', ('extract_i8', a, 3), ('extract_i8', b, 3))))
udot_4x8_a_b = ('iadd', ('iadd', ('imul', ('extract_u8', a, 0), ('extract_u8', b, 0)),
                                 ('imul', ('extract_u8', a, 1), ('extract_u8', b, 1))),
                        ('iadd', ('imul', ('extract_u8', a, 2), ('extract_u8', b, 2)),
                                 ('imul', ('extract_u8', a, 3), ('extract_u8', b, 3))))
sudot_4x8_a_b = ('iadd', ('iadd', ('imul', ('extract_i8', a, 0), ('extract_u8', b, 0)),
                                  ('imul', ('extract_i8', a, 1), ('extract_u8', b, 1))),
                         ('iadd', ('imul', ('extract_i8', a, 2), ('extract_u8', b, 2)),
                                  ('imul', ('extract_i8', a, 3), ('extract_u8', b, 3))))
sdot_2x16_a_b = ('iadd', ('imul', ('extract_i16', a, 0), ('extract_i16', b, 0)),
                         ('imul', ('extract_i16', a, 1), ('extract_i16', b, 1)))
udot_2x16_a_b = ('iadd', ('imul', ('extract_u16', a, 0), ('extract_u16', b, 0)),
                         ('imul', ('extract_u16', a, 1), ('extract_u16', b, 1)))

optimizations.extend([
    (('sdot_4x8_iadd', a, b, c), ('iadd', sdot_4x8_a_b, c), '!options->has_sdot_4x8'),
    (('udot_4x8_uadd', a, b, c), ('iadd', udot_4x8_a_b, c), '!options->has_udot_4x8'),
    (('sudot_4x8_iadd', a, b, c), ('iadd', sudot_4x8_a_b, c), '!options->has_sudot_4x8'),
    (('sdot_2x16_iadd', a, b, c), ('iadd', sdot_2x16_a_b, c), '!options->has_dot_2x16'),
    (('udot_2x16_uadd', a, b, c), ('iadd', udot_2x16_a_b, c), '!options->has_dot_2x16'),

    (('udot_4x8_uadd_sat', a, b, '#c(is_ult_0xfffc07fc)'), ('udot_4x8_uadd', a, b, c)),
    (('udot_4x8_uadd_sat', a, b, c), ('uadd_sat', ('udot_4x8_uadd', a, b, 0), c), '!options->has_udot_4x8_sat'),
    (('sdot_4x8_iadd_sat', a, b, c), ('iadd_sat', ('sdot_4x8_iadd', a, b, 0), c), '!options->has_sdot_4x8_sat'),
    (('sudot_4x8_iadd_sat', a, b, c), ('iadd_sat', ('sudot_4x8_iadd', a, b, 0), c), '!options->has_sudot_4x8_sat'),
    (('udot_2x16_uadd_sat', a, b, c), ('uadd_sat', udot_2x16_a_b, c), '!options->has_dot_2x16'),
    (('sdot_2x16_iadd_sat', a, b, c), ('iadd_sat', sdot_2x16_a_b, c), '!options->has_dot_2x16'),
])

for s in [16, 32, 64]:
    optimizations.extend([
        (('~flrp@{}'.format(s), a, b, ('b2f', 'c@1')), ('bcsel', c, ('fcanonicalize', b), ('fcanonicalize', a)), 'options->lower_flrp{}'.format(s)),
        (('~flrp@{}'.format(s), a, ('fadd', a, b), c), ('fadd', ('fmul', b, c), a), 'options->lower_flrp{}'.format(s)),
        (('~flrp@{}'.format(s), ('fadd(is_used_once)', a, b), ('fadd(is_used_once)', a, c), d), ('fadd', ('flrp', b, c, d), a), 'options->lower_flrp{}'.format(s)),
        (('~flrp@{}'.format(s), a, ('fmul(is_used_once)', a, b), c), ('fmul', ('flrp', 1.0, b, c), a), 'options->lower_flrp{}'.format(s)),
        (('~fadd@{}'.format(s), ('fmul', a, ('fadd', 1.0, ('fneg', c))), ('fmul', b, c)), ('flrp', a, b, c), '!options->lower_flrp{}'.format(s)),
        (('~fadd@{}'.format(s), ('fmul', a, ('fsat', ('fadd', 1.0, ('fneg', c)))), ('fmul', b, ('fsat', c))), ('flrp', a, b, ('fsat', c)), '!options->lower_flrp{}'.format(s)),
        (('~fadd@{}'.format(s), a, ('fmul', c, ('fadd', b, ('fneg', a)))), ('flrp', a, b, c), '!options->lower_flrp{}'.format(s)),
        (('~fadd@{}'.format(s), ('fmul', a, ('fadd', 1.0, ('fneg', ('b2f', 'c@1')))), ('fmul', b, ('b2f', c))), ('bcsel', c, b, a), 'options->lower_flrp{}'.format(s)),
        (('~fadd@{}'.format(s), a, ('fmul', ('b2f', 'c@1'), ('fadd', b, ('fneg', a)))), ('bcsel', c, ('fcanonicalize', b), ('fcanonicalize', a)), 'options->lower_flrp{}'.format(s)),
        (('~ffma@{}'.format(s), a, ('fadd', 1.0, ('fneg', ('b2f', 'c@1'))), ('fmul', b, ('b2f', 'c@1'))), ('bcsel', c, ('fcanonicalize', b), ('fcanonicalize', a))),
        (('~ffma@{}'.format(s), b, ('b2f', 'c@1'), ('ffma', ('fneg', a), ('b2f', 'c@1'), a)), ('bcsel', c, ('fcanonicalize', b), ('fcanonicalize', a))),
        (('~ffma@{}'.format(s), ('b2f', 'c@1'), ('fadd', b, ('fneg', a)), a), ('bcsel', c, ('fcanonicalize', b), ('fcanonicalize', a))),
        (('~ffma@{}'.format(s), ('b2f', 'c@1'), ('ffma', ('fneg', a), b, d), ('fmul', a, b)), ('bcsel', c, ('fcanonicalize', d), ('fmul', a, b))),
        (('~fadd@{}'.format(s), 1.0, ('fneg', ('fmul', ('fadd', 1.0, ('fneg', a)), ('fadd', 1.0, ('fneg', b))))), ('flrp', b, 1.0, a), '!options->lower_flrp{}'.format(s)),
    ])

optimizations.extend([
    (('~flrp', ('fmul(is_used_once)', a, b), ('fmul(is_used_once)', a, c), d), ('fmul', ('flrp', b, c, d), a)),
    (('~flrp', a, 0.0, c), ('fadd', ('fmul', ('fneg', a), c), a)),

    (('fadd', ('ffloor', a), ('b2f', ('iand', ('flt', a, 0), ('flt', ('fneg', ('ffract', a)), ('ffract', a))))), ('ftrunc', ('fadd', a, 0))),
    (('fadd', ('ffloor', a), ('b2f', ('inot', ('fge', 0, ('fmin', ('fneg', a), ('ffract', a)))))), ('ftrunc', ('fadd', a, 0))),
    (('fadd', ('ffloor', a), ('b2f', ('flt', 0, ('fmin', ('fneg', a), ('ffract', a))))), ('ftrunc', ('fadd', a, 0))),

    (('fadd(nnan,nsz)', a, ('ffract', ('fneg', a))), ('fceil', a), '!options->lower_fceil'),

    (('ftrunc@16', a), ('bcsel', ('flt', a, 0.0), ('fneg', ('ffloor', ('fabs', a))), ('ffloor', ('fabs', a))), 'options->lower_ftrunc'),
    (('ftrunc@32', a), ('bcsel', ('flt', a, 0.0), ('fneg', ('ffloor', ('fabs', a))), ('ffloor', ('fabs', a))), 'options->lower_ftrunc'),
    (('ftrunc@64', a), ('bcsel', ('flt', a, 0.0), ('fneg', ('ffloor', ('fabs', a))), ('ffloor', ('fabs', a))),
     '(options->lower_ftrunc || (options->lower_doubles_options & nir_lower_dtrunc)) && (!(options->lower_doubles_options & nir_lower_dfloor) || !(options->lower_doubles_options & nir_lower_dfract))'),

    (('ffloor@16', a), ('fsub', a, ('ffract', a)), 'options->lower_ffloor'),
    (('ffloor@32', a), ('fsub', a, ('ffract', a)), 'options->lower_ffloor'),
    (('ffloor@64', a), ('fsub', a, ('ffract', a)), '(options->lower_ffloor || (options->lower_doubles_options & nir_lower_dfloor)) && !(options->lower_doubles_options & nir_lower_dfract)'),
    (('fadd@16', a, ('fadd@16', b, ('fneg', ('ffract', a)))), ('fadd@16', b, ('ffloor', a)), '!options->lower_ffloor'),
    (('fadd@32', a, ('fadd@32', b, ('fneg', ('ffract', a)))), ('fadd@32', b, ('ffloor', a)), '!options->lower_ffloor'),
    (('fadd@64', a, ('fadd@64', b, ('fneg', ('ffract', a)))), ('fadd@64', b, ('ffloor', a)), '!options->lower_ffloor && !(options->lower_doubles_options & nir_lower_dfloor)'),
    (('fadd@16(nnan)', a, ('fneg', ('ffract', a))), ('ffloor', a), '!options->lower_ffloor'),
    (('fadd@32(nnan)', a, ('fneg', ('ffract', a))), ('ffloor', a), '!options->lower_ffloor'),
    (('fadd@64(nnan)', a, ('fneg', ('ffract', a))), ('ffloor', a), '!options->lower_ffloor && !(options->lower_doubles_options & nir_lower_dfloor)'),
    (('ffract@16', a), ('fsub', a, ('ffloor', a)), 'options->lower_ffract'),
    (('ffract@32', a), ('fsub', a, ('ffloor', a)), 'options->lower_ffract'),
    (('ffract@64', a), ('fsub', a, ('ffloor', a)),
     '(options->lower_ffract || (options->lower_doubles_options & nir_lower_dfract)) && !(options->lower_doubles_options & nir_lower_dfloor)'),
    (('fadd@16', a, ('fneg(is_used_once)', ('ffloor(is_used_once)', a))), ('ffract', a), '!options->lower_ffract'),
    (('fadd@32', a, ('fneg(is_used_once)', ('ffloor(is_used_once)', a))), ('ffract', a), '!options->lower_ffract'),
    (('fadd@64', a, ('fneg(is_used_once)', ('ffloor(is_used_once)', a))), ('ffract', a), '!options->lower_ffract && !(options->lower_doubles_options & nir_lower_dfract)'),
    (('fceil', a), ('fneg', ('ffloor', ('fneg', a))), 'options->lower_fceil'),

    (('ffma@16', a, b, c), ('fadd', ('fmul', a, b), c), 'options->lower_ffma16'),
    (('ffma@32', a, b, c), ('fadd', ('fmul', a, b), c), 'options->lower_ffma32'),
    (('ffma@64', a, b, c), ('fadd', ('fmul', a, b), c), 'options->lower_ffma64'),
    (('ffmaz', a, b, c), ('fadd', ('fmulz', a, b), c), 'options->lower_ffma32'),
    (('ffma@16(contract)', a, b, c), ('fadd', ('fmul', a, b), c), 'options->fuse_ffma16'),
    (('ffma@32(contract)', a, b, c), ('fadd', ('fmul', a, b), c), 'options->fuse_ffma32'),
    (('ffma@64(contract)', a, b, c), ('fadd', ('fmul', a, b), c), 'options->fuse_ffma64'),
    (('ffmaz(contract)', a, b, c), ('fadd', ('fmulz', a, b), c), 'options->fuse_ffma32'),

    (('~fmul', ('fadd', ('bcsel', a, ('fmul', b, c), 0), '#d'), '#e'),
     ('bcsel', a, ('fmul', ('fadd', ('fmul', b, c), d), e), ('fmul', d, e))),

    (('fdph', a, b), ('fdot4', ('vec4', 'a.x', 'a.y', 'a.z', 1.0), b), 'options->lower_fdph'),

    (('fdot4', a, 0.0), 0.0, 'true', TestStatus.XFAIL),
    (('fdot3', a, 0.0), 0.0, 'true', TestStatus.XFAIL),
    (('fdot2', a, 0.0), 0.0, 'true', TestStatus.XFAIL),

    (('fdot4', ('vec4', a, b, c, 1.0), d), ('fdph', ('vec3', a, b, c), d), '!options->lower_fdph'),
    (('fdot4', ('vec4', a, 0.0, 0.0, 0.0), b), ('fmul', a, 'b.x'), 'true', TestStatus.XFAIL),
    (('fdot4', ('vec4', a, b, 0.0, 0.0), c), ('fdot2', ('vec2', a, b), c), 'true', TestStatus.XFAIL),
    (('fdot4', ('vec4', a, b, c, 0.0), d), ('fdot3', ('vec3', a, b, c), d), 'true', TestStatus.XFAIL),
    (('fdot4', 'a(w_is_zero)', b), ('fdot3', 'a.xyz', 'b.xyz'), 'true', TestStatus.XFAIL),
    (('fdot4', 'a(z_is_zero)', b), ('fdot3', 'a.xyw', 'b.xyw'), 'true', TestStatus.XFAIL),
    (('fdot4', 'a(y_is_zero)', b), ('fdot3', 'a.xzw', 'b.xzw'), 'true', TestStatus.XFAIL),
    (('fdot4', 'a(x_is_zero)', b), ('fdot3', 'a.yzw', 'b.yzw'), 'true', TestStatus.XFAIL),

    (('fdot3', ('vec3', a, 0.0, 0.0), b), ('fmul', a, 'b.x'), 'true', TestStatus.XFAIL),
    (('fdot3', ('vec3', a, b, 0.0), c), ('fdot2', ('vec2', a, b), c), 'true', TestStatus.XFAIL),
    (('fdot3', 'a(x_is_zero)', b), ('fdot2', 'a.yz', 'b.yz'), 'true', TestStatus.XFAIL),
    (('fdot3', 'a(y_is_zero)', b), ('fdot2', 'a.xz', 'b.xz'), 'true', TestStatus.XFAIL),
    (('fdot3', 'a(z_is_zero)', b), ('fdot2', 'a.xy', 'b.xy'), 'true', TestStatus.XFAIL),

    (('fdot2', ('vec2', a, 0.0), b), ('fmul', a, 'b.x'), 'true', TestStatus.XFAIL),
    (('fdot2', 'a(x_is_zero)', b), ('fmul', 'a.y', 'b.y'), 'true', TestStatus.XFAIL),
    (('fdot2', 'a(y_is_zero)', b), ('fmul', 'a.x', 'b.x'), 'true', TestStatus.XFAIL),
    (('fdot2', a, 1.0), ('fadd', 'a.x', 'a.y')),

    (('~fadd', ('fneg(is_used_once)', ('fsat(is_used_once)', 'a(is_not_fmul)')), 1.0), ('fsat', ('fadd', 1.0, ('fneg', a)))),

    (('ishl', ('iadd', ('imul', a, '#b'), '#c'), '#d'),
     ('iadd', ('imul', a, ('ishl', b, d)), ('ishl', c, d))),
    (('ishl', ('imul', a, '#b'), '#c'), ('imul', a, ('ishl', b, c))),
    (('imul', ('ishl', a, '#b'), '#c'), ('imul', a, ('ishl', c, b))),
])

optimizations.extend(optimize_fcanonicalize)

# Care must be taken here. Shifts in NIR uses only the lower log2(bitsize)
# bits of the second source. These replacements must correctly handle the
# case where (b % bitsize) + (c % bitsize) >= bitsize.
for s in (8, 16, 32, 64):
    mask = s - 1

    ishl = f"ishl@{s}"
    ishr = f"ishr@{s}"
    ushr = f"ushr@{s}"

    in_bounds = ('ult', ('iadd', ('iand', b, mask), ('iand', c, mask)), s)

    optimizations.extend([
        ((ishl, (ishl, a, '#b'), '#c'),
         ('bcsel', in_bounds, (ishl, a, ('iadd', b, c)), 0)),
        ((ushr, (ushr, a, '#b'), '#c'),
         ('bcsel', in_bounds, (ushr, a, ('iadd', b, c)), 0)),

        # To get -1 for large shifts of negative values, ishr must instead
        # clamp the shift count to the maximum value.
        ((ishr, (ishr, a, '#b'), '#c'),
         (ishr, a, ('imin', ('iadd', ('iand', b, mask), ('iand', c, mask)), s - 1))),
    ])


# Optimize a pattern of address calculation created by DXVK where the offset is
# divided by 4 and then multipled by 4. This can be turned into an iand and the
# additions before can be reassociated to CSE the iand instruction.
for size, full_mask in ((8, 0xff), (16, 0xffff), (32, 0xffffffff), (64, 0xffffffffffffffff)):
    a_sz = f'a@{size}'

    optimizations.extend([
        # a >> b << b -> a & ~((1 << b) - 1)
        (('ishl', ('ushr', a_sz, '#b'), b), ('iand', a, ('ishl', full_mask, b))),
        (('ishl', ('ishr', a_sz, '#b'), b), ('iand', a, ('ishl', full_mask, b))),

        # This does not trivially work with ishr.
        (('ushr', ('ishl', a_sz, '#b'), b), ('iand', a, ('ushr', full_mask, b))),
    ])


def ubfe_ubfe(a, b, c, d, e):
    inner_offset = ('iand', b, 0x1f)
    inner_bits = ('umin', ('iand', c, 0x1f), ('isub', 32, inner_offset))
    outer_offset = ('iand', d, 0x1f)
    outer_bits = ('iand', e, 0x1f)

    offset = ('iadd', inner_offset, outer_offset)
    bits = ('umin', outer_bits, ('imax', ('isub', inner_bits, outer_offset), 0))
    collapsed = ('ubfe', a, offset, bits)
    offset_out_of_range = ('ilt', 31, offset)

    return ('bcsel', offset_out_of_range, 0, collapsed)


optimizations.extend([
    # Create bitfield extract from right-shift + and pattern.
    (('iand@32', ('ushr@32(is_used_once)', a, b), '#c(is_const_bitmask)'),
     ('ubfe', a, b, ('bit_count', c)),
     'options->has_bfe && !options->avoid_ternary_with_two_constants'),

    (('iand@32', ('ushr@32', a, b), ('bfm', c, 0)),
     ('ubfe', a, b, c),
     'options->has_bfe'),

    (('ushr', ('iand', a, ('bfm', c, b)), b),
     ('ubfe', a, b, c),
     'options->has_bfe'),

    (('ushr@32', ('iand(is_used_once)', a, '#b(is_const_bfm)'), '#c'),
     ('bcsel',
      ('ilt', ('find_lsb', b), ('iand', c, 0x1f)),
      ('ushr',
       ('ubfe', a, ('find_lsb', b), ('bit_count', b)),
       ('isub', c, ('find_lsb', b))),
      ('ishl',
       ('ubfe', a, ('find_lsb', b), ('bit_count', b)),
       ('isub', ('find_lsb', b), c))),
     'options->has_bfe && !options->avoid_ternary_with_two_constants'),

    # Collapse two bitfield extracts with constant operands into a single one.
    (('ubfe', ('ubfe', a, '#b', '#c'), '#d', '#e'),
     ubfe_ubfe(a, b, c, d, e)),

    # Collapse non-zero right-shift into bitfield extract.
    (('ushr@32', ('ubfe', a, '#b', '#c'), '#d(is_5lsb_not_zero)'),
     ubfe_ubfe(a, b, c, d, 31)),

    (('iand', 'a(is_unsigned_multiple_of_4)', -4), a),
])


for log2 in range(1, 7):  # powers of two from 2 to 64
    v = 1 << log2
    mask = 0xffffffff & ~(v - 1)
    b_is_multiple = f'b(is_unsigned_multiple_of_{v})'

    optimizations.extend([
        # Reassociate for improved CSE
        (('iand@32', ('iadd@32', a, b_is_multiple), mask),
         ('iadd', ('iand', a, mask), b)),
    ])


# To save space in the state tables, reduce to the set that is known to help.
for i in (1, 2, 16, 24):
    lo_mask = 0xffffffff >> i
    hi_mask = (0xffffffff << i) & 0xffffffff

    optimizations.extend([
        (('ishl@32', ('iand', 'a@32', lo_mask), i), ('ishl', a, i)),

        (('iand', ('ishl', 'a@32', i), hi_mask), ('ishl', a, i)),
        (('iand', ('ushr', 'a@32', i), lo_mask), ('ushr', a, i)),
    ])

optimizations.extend([
   # This is common for address calculations. Reassociating may enable the
   # 'a<<c' to be CSE'd. It also helps architectures that have an ISHLADD
   # instruction or a constant offset field for in load / store instructions.
   (('ishl', ('iadd', a, '#b'), '#c'), ('iadd', ('ishl', a, c), ('ishl', b, c))),
   (('ishl', ('iadd(is_used_once)', ('iadd', a, '#b'), c), '#d'),
    ('iadd', ('ishl', ('iadd', a, c), d), ('ishl', b, d))),

   # (a + #b) * #c => (a * #c) + (#b * #c)
   (('imul', ('iadd(is_used_once)', a, '#b'), '#c'), ('iadd', ('imul', a, c), ('imul', b, c))),

   # ((a + #b) + c) * #d => ((a + c) * #d) + (#b * #d)
   (('imul', ('iadd(is_used_once)', ('iadd(is_used_once)', a, '#b'), c), '#d'),
    ('iadd', ('imul', ('iadd', a, c), d), ('imul', b, d))),
   (('ishl', ('iadd(is_used_once)', ('iadd(is_used_once)', a, '#b'), c), '#d'),
    ('iadd', ('ishl', ('iadd', a, c), d), ('ishl', b, d))),

   # Comparison simplifications
   (('inot', ('flt(is_used_once)', 'a(is_a_number)', 'b(is_a_number)')), ('fge', a, b)),
   (('inot', ('fge(is_used_once)', 'a(is_a_number)', 'b(is_a_number)')), ('flt', a, b)),
   (('inot', ('feq(is_used_once)', a, b)), ('fneu', a, b)),
   (('inot', ('fneu(is_used_once)', a, b)), ('feq', a, b)),
   (('inot', ('ilt(is_used_once)', a, b)), ('ige', a, b)),
   (('inot', ('ult(is_used_once)', a, b)), ('uge', a, b)),
   (('inot', ('ige(is_used_once)', a, b)), ('ilt', a, b)),
   (('inot', ('uge(is_used_once)', a, b)), ('ult', a, b)),
   (('inot', ('ieq(is_used_once)', a, b)), ('ine', a, b)),
   (('inot', ('ine(is_used_once)', a, b)), ('ieq', a, b)),

   (('iand', ('feq', a, b), ('fneu', a, b)), False),
   (('iand', ('flt', a, b), ('flt', b, a)), False),
   (('iand', ('ieq', a, b), ('ine', a, b)), False),
   (('iand', ('ilt', a, b), ('ilt', b, a)), False),
   (('iand', ('ult', a, b), ('ult', b, a)), False),

   # Help CSE and canonicalization for negated compares.
   (('flt', ('fneg', a), ('fneg', b)), ('flt', b, a)),
   (('fge', ('fneg', a), ('fneg', b)), ('fge', b, a)),
   (('feq', ('fneg', a), ('fneg', b)), ('feq', b, a)),
   (('fneu', ('fneg', a), ('fneg', b)), ('fneu', b, a)),
   (('flt', ('fneg', 'a(is_not_const)'), '#b'), ('flt', ('fneg', b), a)),
   (('flt', '#b', ('fneg', 'a(is_not_const)')), ('flt', a, ('fneg', b))),
   (('fge', ('fneg', 'a(is_not_const)'), '#b'), ('fge', ('fneg', b), a)),
   (('fge', '#b', ('fneg', 'a(is_not_const)')), ('fge', a, ('fneg', b))),
   (('fneu', ('fneg', 'a(is_not_const)'), '#b'), ('fneu', ('fneg', b), a)),
   (('feq', '#b', ('fneg', 'a(is_not_const)')), ('feq', a, ('fneg', b))),

   (('flt', a, -0.0), ('flt', a, 0.0)),
   (('flt', -0.0, a), ('flt', 0.0, a)),
   (('fge', a, -0.0), ('fge', a, 0.0)),
   (('fge', -0.0, a), ('fge', 0.0, a)),
   (('fneu', a, -0.0), ('fneu', 0.0, a)),
   (('feq', -0.0, a), ('feq', a, 0.0)),

   (('ieq', ('ineg', 'a(is_not_const)'), '#b'),  ('ieq', a, ('ineg', b))),
   (('ine', ('ineg', 'a(is_not_const)'), '#b'),  ('ine', a, ('ineg', b))),
   (('ieq', ('inot', 'a(is_not_const)'), '#b'),  ('ieq', a, ('inot', b))),
   (('ine', ('inot', 'a(is_not_const)'), '#b'),  ('ine', a, ('inot', b))),
   (('ieq', ('iabs', a), 0),  ('ieq', a, 0)),
   (('ine', ('iabs', a), 0),  ('ine', a, 0)),

   (('ieq', ('bit_count(is_used_once)', a), 0),  ('ieq', a, 0)),
   (('ine', ('bit_count(is_used_once)', a), 0),  ('ine', a, 0)),

   (('ieq', ('ineg', a), ('ineg', b)), ('ieq', a, b)),
   (('ine', ('ineg', a), ('ineg', b)), ('ine', a, b)),
   (('ieq', ('inot', a), ('inot', b)), ('ieq', a, b)),
   (('ine', ('inot', a), ('inot', b)), ('ine', a, b)),

   (('fneu', ('fabs', a), 0.0), ('fneu', a, 0.0)),
   (('feq', ('fabs', a), 0.0), ('feq', a, 0.0)),
   (('fneu', ('fabs', a), ('fabs', a)), ('fneu', a, a)),
   (('feq', ('fabs', a), ('fabs', a)), ('feq', a, a)),

   (('flt', '#b(is_gt_0_and_lt_1)', ('fsat(is_used_once)', a)), ('flt', b, a)),
   (('fge', ('fsat(is_used_once)', a), '#b(is_gt_0_and_lt_1)'), ('fge', a, b)),
   (('feq', ('fsat(is_used_once)', a), '#b(is_gt_0_and_lt_1)'), ('feq', a, b)),
   (('fneu', ('fsat(is_used_once)', a), '#b(is_gt_0_and_lt_1)'), ('fneu', a, b)),
   (('fge', ('fsat(is_used_once)', a), 1.0), ('fge', a, 1.0)),
   (('flt', 0.0, ('fsat(is_used_once)', a)), ('flt', 0.0, a)),

   (('bcsel(is_only_used_as_float)', ('feq', a, 'b(is_not_zero)'), b, a), a),
   (('bcsel(is_only_used_as_float)', ('fneu', a, 'b(is_not_zero)'), a, b), a),

   (('bcsel', ('feq(ignore_exact)', a, 0), 0, ('fsat', ('fmul', a, 'b(is_a_number)'))), ('!fsat', ('fmul', a, b))),
   (('bcsel', ('fneu(ignore_exact)', a, 0), ('fsat', ('fmul', a, 'b(is_a_number)')), 0), ('!fsat', ('fmul', a, b))),
   (('bcsel', ('feq(ignore_exact)', a, 0), b, ('fadd', a, 'b(is_not_zero)')), ('fadd', a, b)),
   (('bcsel', ('fneu(ignore_exact)', a, 0), ('fadd', a, 'b(is_not_zero)'), b), ('fadd', a, b)),

   (('fge', 0.0, ('b2f', 'a@1')), ('inot', a)),
   (('fge', ('fneg', ('b2f', 'a@1')), 0.0), ('inot', a)),

   (('fneu', ('fadd', ('b2f', 'a@1'), ('b2f', 'b@1')), 0.0), ('ior', a, b)),
   (('fneu', ('b2f', 'a@1'), ('fneg', ('b2f', 'b@1'))),      ('ior', a, b)),
   (('fneu', ('fadd', ('b2f', 'a@1'), ('fneg', ('b2f', 'b@1'))), 0.0), ('ixor', a, b)),
   (('fneu',          ('b2f', 'a@1'),           ('b2f', 'b@1')),      ('ixor', a, b)),
   (('fneu', ('fneg', ('b2f', 'a@1')), ('fneg', ('b2f', 'b@1'))),     ('ixor', a, b)),
   (('feq',  ('fadd', ('b2f', 'a@1'), ('b2f', 'b@1')), 0.0),          ('inot', ('ior', a, b))),
   (('feq',  ('b2f', 'a@1'), ('fneg', ('b2f', 'b@1'))),               ('inot', ('ior', a, b))),
   (('feq',  ('fadd', ('b2f', 'a@1'), ('fneg', ('b2f', 'b@1'))), 0.0), ('ieq', a, b)),
   (('feq',           ('b2f', 'a@1'),           ('b2f', 'b@1')),       ('ieq', a, b)),
   (('feq',  ('fneg', ('b2f', 'a@1')), ('fneg', ('b2f', 'b@1'))),      ('ieq', a, b)),

   (('flt', ('fneg', ('fadd', ('b2f', 'a@1'), ('b2f', 'b@1'))), 0.0), ('ior', a, b)),
   (('flt', 0.0, ('fadd', ('b2f', 'a@1'), ('b2f', 'b@1'))), ('ior', a, b)),

   (('fge', ('fneg', ('fadd', ('b2f', 'a@1'), ('b2f', 'b@1'))), 0.0), ('inot', ('ior', a, b))),
   (('fge', 0.0, ('fadd', ('b2f', 'a@1'), ('b2f', 'b@1'))), ('inot', ('ior', a, b))),

   (('flt', a, ('fneg', a)), ('flt', a, 0.0)),
   (('fge', a, ('fneg', a)), ('fge', a, 0.0)),

   (('flt', ('fmin', c, ('fneg', ('fadd', ('b2f', 'a@1'), ('b2f', 'b@1')))), 0.0),
    ('ior', ('flt', c, 0.0), ('ior', a, b))),

   (('~flt', ('fadd', a, b), a), ('flt', b, 0.0)),
   (('~fge', ('fadd', a, b), a), ('fge', b, 0.0)),
   (('~feq', ('fadd', a, b), a), ('feq', b, 0.0)),
   (('~fneu', ('fadd', a, b), a), ('fneu', b, 0.0)),

   (('~flt', ('fadd(is_used_once)', a, '#b'),  '#c'), ('flt', a, ('fadd', c, ('fneg', b)))),
   (('~flt', ('fneg(is_used_once)', ('fadd(is_used_once)', a, '#b')), '#c'), ('flt', ('fneg', ('fadd', c, b)), a)),
   (('~fge', ('fadd(is_used_once)', a, '#b'),  '#c'), ('fge', a, ('fadd', c, ('fneg', b)))),
   (('~fge', ('fneg(is_used_once)', ('fadd(is_used_once)', a, '#b')), '#c'), ('fge', ('fneg', ('fadd', c, b)), a)),
   (('~feq', ('fadd(is_used_once)', a, '#b'),  '#c'), ('feq', a, ('fadd', c, ('fneg', b)))),
   (('~feq', ('fneg(is_used_once)', ('fadd(is_used_once)', a, '#b')), '#c'), ('feq', ('fneg', ('fadd', c, b)), a)),
   (('~fneu', ('fadd(is_used_once)', a, '#b'),  '#c'), ('fneu', a, ('fadd', c, ('fneg', b)))),
   (('~fneu', ('fneg(is_used_once)', ('fadd(is_used_once)', a, '#b')), '#c'), ('fneu', ('fneg', ('fadd', c, b)), a)),

   (('ieq', ('iadd', a, b), a), ('ieq', b, 0)),
   (('ine', ('iadd', a, b), a), ('ine', b, 0)),

   (('flt',  0.0, ('b2f', 'a@1')), a),
   (('ieq', 'a@1', False), ('inot', a)),
   (('ieq', 'a@1', True), a),
   (('ine', 'a@1', False), a),
   (('ine', 'a@1', True), ('inot', a)),

   (('fneu', ('u2f', a), 0.0), ('ine', a, 0)),
   (('feq', ('u2f', a), 0.0), ('ieq', a, 0)),
   (('fge', ('u2f', a), 0.0), True),
   (('fge', 0.0, ('u2f', a)), ('ieq', 0, a)),
   (('flt', ('u2f', a), 0.0), False),
   (('flt', 0.0, ('u2f', a)), ('ine', 0, a)),

   (('fneu', ('i2f', a), 0.0), ('ine', a, 0)),
   (('feq', ('i2f', a), 0.0), ('ieq', a, 0)),
   (('fge', ('i2f', a), 0.0), ('ige', a, 0)),
   (('fge', 0.0, ('i2f', a)), ('ige', 0, a)),
   (('flt', ('i2f', a), 0.0), ('ilt', a, 0)),
   (('flt', 0.0, ('i2f', a)), ('ilt', 0, a)),

   (('~flt', 0.0, ('fabs', a)), ('fneu', a, 0.0)),
   (('~flt', ('fneg', ('fabs', a)), 0.0), ('fneu', a, 0.0)),
   (('fge', 0.0, ('fabs', a)), ('feq', a, 0.0)),
   (('fge', ('fneg', ('fabs', a)), 0.0), ('feq', a, 0.0)),

   (('iand', ('fge', a, 0.0), ('fge', 1.0, a)), ('feq', a, ('fsat', a)), '!options->lower_fsat'),

   (('fmax', ('b2f(is_used_once)', 'a@1'), ('b2f', 'b@1')), ('b2f', ('ior', a, b))),
   (('fmax', ('fneg(is_used_once)', ('b2f(is_used_once)', 'a@1')), ('fneg', ('b2f', 'b@1'))), ('fneg', ('b2f', ('iand', a, b)))),
   (('fmin', ('b2f(is_used_once)', 'a@1'), ('b2f', 'b@1')), ('b2f', ('iand', a, b))),
   (('fmin', ('fneg(is_used_once)', ('b2f(is_used_once)', 'a@1')), ('fneg', ('b2f', 'b@1'))), ('fneg', ('b2f', ('ior', a, b)))),

   (('bcsel', a, ('b2f', 'b@1'), 0), ('b2f', ('bcsel', a, b, False))),
   (('bcsel', a, ('b2f', 'b@1'), 1.0), ('b2f', ('bcsel', a, b, True))),
   (('bcsel', a, 0, ('b2f', 'b@1')), ('b2f', ('bcsel', a, False, b))),
   (('bcsel', a, 1.0, ('b2f', 'b@1')), ('b2f', ('bcsel', a, True, b))),

   (('fmin', ('b2f', 'a@1'), '#b'), ('bcsel', a, ('fmin', b, 1.0), ('fmin', b, 0.0))),

   (('flt', ('fadd(is_used_once)', a, ('fneg', b)), 0.0), ('flt', a, b)),

   (('fge', ('fneg', ('fabs', a)), 0.0), ('feq', a, 0.0)),

   (('~bcsel', ('flt', b, a), b, a), ('fmin', a, b)),
   (('~bcsel', ('flt', a, b), b, a), ('fmax', a, b)),
   (('~bcsel', ('fge', a, b), b, a), ('fmin', a, b)),
   (('~bcsel', ('fge', b, a), b, a), ('fmax', a, b)),

   (('bcsel', ('inot', a), b, c), ('bcsel', a, c, b)),
   (('bcsel', a, ('bcsel', a, b, c), d), ('bcsel', a, b, d)),
   (('bcsel', a, b, ('bcsel', a, c, d)), ('bcsel', a, b, d)),
   (('bcsel', a, ('bcsel', b, c, d), ('bcsel(is_used_once)', b, c, 'e')), ('bcsel', b, c, ('bcsel', a, d, 'e'))),
   (('bcsel', a, ('bcsel(is_used_once)', b, c, d), ('bcsel', b, c, 'e')), ('bcsel', b, c, ('bcsel', a, d, 'e'))),
   (('bcsel', a, ('bcsel', b, c, d), ('bcsel(is_used_once)', b, 'e', d)), ('bcsel', b, ('bcsel', a, c, 'e'), d)),
   (('bcsel', a, ('bcsel(is_used_once)', b, c, d), ('bcsel', b, 'e', d)), ('bcsel', b, ('bcsel', a, c, 'e'), d)),

   (('bcsel', a, True, b), ('ior', a, b)),
   (('bcsel', a, a, b), ('ior', a, b)),
   (('bcsel', a, b, False), ('iand', a, b)),
   (('bcsel', a, b, a), ('iand', a, b)),
   (('bcsel', a, b, True), ('ior', ('inot', a), b)),
   (('bcsel', a, False, b), ('iand', ('inot', a), b)),

   (('fmin', a, a), ('fcanonicalize', a)),
   (('fmax', a, a), ('fcanonicalize', a)),
   (('imin', a, a), a),
   (('imax', a, a), a),
   (('umin', a, a), a),
   (('umin', a, 0), 0),
   (('umin', a, -1), a),
   (('umax', a, a), a),
   (('umax', a, 0), a),
   (('umax', a, -1), -1),

   (('fmax', ('fmax', a, b), b), ('fmax', a, b)),
   (('umax', ('umax', a, b), b), ('umax', a, b)),
   (('imax', ('imax', a, b), b), ('imax', a, b)),
   (('fmin', ('fmin', a, b), b), ('fmin', a, b)),
   (('umin', ('umin', a, b), b), ('umin', a, b)),
   (('imin', ('imin', a, b), b), ('imin', a, b)),

   (('fmax', ('fmax', ('fmax', a, b), c), a), ('fmax', ('fmax', a, b), c)),
   (('umax', ('umax', ('umax', a, b), c), a), ('umax', ('umax', a, b), c)),
   (('imax', ('imax', ('imax', a, b), c), a), ('imax', ('imax', a, b), c)),
   (('fmin', ('fmin', ('fmin', a, b), c), a), ('fmin', ('fmin', a, b), c)),
   (('umin', ('umin', ('umin', a, b), c), a), ('umin', ('umin', a, b), c)),
   (('imin', ('imin', ('imin', a, b), c), a), ('imin', ('imin', a, b), c)),

   (('fmin', ('fmax', 'a(is_finite)', b), a), ('fmul', 1.0, a)),
   (('fmax', ('fmin', 'a(is_finite)', b), a), ('fmul', 1.0, a)),

   (('umin', ('umax', a, b), a), a),
   (('umax', ('umin', a, b), a), a),
   (('imin', ('imax', a, b), a), a),
   (('imax', ('imin', a, b), a), a),

   (('fmax(nsz)', 'a(is_a_number_not_negative)', 'b(is_not_positive)'), ('fcanonicalize', a)),
   (('fmin(nsz)', 'a(is_a_number_not_positive)', 'b(is_not_negative)'), ('fcanonicalize', a)),
   (('fmax', 'a(is_a_number_not_negative)', 'b(is_lt_zero)'), ('fcanonicalize', a)),
   (('fmin', 'a(is_a_number_not_positive)', 'b(is_gt_zero)'), ('fcanonicalize', a)),

   (('fneg', ('fmax(is_used_once)', ('fneg', a), ('fneg', b))), ('fmin', a, b)),
   (('fneg', ('fmin(is_used_once)', ('fneg', a), ('fneg', b))), ('fmax', a, b)),
   (('fneg', ('fmax(is_used_once)', ('fneg', a), '#b')), ('fmin', a, ('fneg', b))),
   (('fneg', ('fmin(is_used_once)', ('fneg', a), '#b')), ('fmax', a, ('fneg', b))),

   (('fmin(nsz)', a, -0.0), ('fmin', a, 0.0)),
   (('fmax(nsz)', a, -0.0), ('fmax', a, 0.0)),
])


for op in ('ine', 'ieq', 'ilt', 'ige', 'ult', 'uge', 'bitz', 'bitnz',
           'fneu', 'feq', 'flt', 'fge', 'fneo', 'fequ'):
    optimizations.extend([
        ((op, ('bcsel(is_used_once)', a, b, '#c'), '#d'),
         ('bcsel', a, (op, b, d), (op, c, d))),
        ((op, ('bcsel(is_used_once)', a, '#b', c), '#d'),
         ('bcsel', a, (op, b, d), (op, c, d))),
        ((op, '#d', ('bcsel(is_used_once)', a, b, '#c')),
         ('bcsel', a, (op, d, b), (op, d, c))),
        ((op, '#d', ('bcsel(is_used_once)', a, '#b', c)),
         ('bcsel', a, (op, d, b), (op, d, c))),
    ])


for N in (8, 16, 32, 64):
    b2iN = f'b2i{N}'
    optimizations.extend([
        (('ieq', (b2iN, 'a@1'), (b2iN, 'b@1')), ('ieq', a, b)),
        (('ine', (b2iN, 'a@1'), (b2iN, 'b@1')), ('ine', a, b)),
    ])


for N in (16, 32, 64):
    b2fN = f'b2f{N}'
    optimizations.extend([
        (('feq', (b2fN, 'a@1'), (b2fN, 'b@1')), ('ieq', a, b)),
        (('fneu', (b2fN, 'a@1'), (b2fN, 'b@1')), ('ine', a, b)),
    ])


for s in (8, 16, 32, 64):
    optimizations.extend([
       (('iand@{}'.format(s), a, ('inot', ('ishr', a, s - 1))), ('imax', a, 0)),

       (('ieq', ('iand', f'a@{s}', 1 << (s - 1)), 0),            ('ige', a, 0)),
       (('ine', ('iand', f'a@{s}', 1 << (s - 1)), 1 << (s - 1)), ('ige', a, 0)),
       (('ine', ('iand', f'a@{s}', 1 << (s - 1)), 0),            ('ilt', a, 0)),
       (('ieq', ('iand', f'a@{s}', 1 << (s - 1)), 1 << (s - 1)), ('ilt', a, 0)),
       (('ine', ('ushr', f'a@{s}', s - 1), 0), ('ilt', a, 0)),
       (('ieq', ('ushr', f'a@{s}', s - 1), 0), ('ige', a, 0)),
       (('ieq', ('ushr', f'a@{s}', s - 1), 1), ('ilt', a, 0)),
       (('ine', ('ushr', f'a@{s}', s - 1), 1), ('ige', a, 0)),
       (('ine', ('ishr', f'a@{s}', s - 1), 0), ('ilt', a, 0)),
       (('ieq', ('ishr', f'a@{s}', s - 1), 0), ('ige', a, 0)),
       (('ieq', ('ishr', f'a@{s}', s - 1), -1), ('ilt', a, 0)),
       (('ine', ('ishr', f'a@{s}', s - 1), -1), ('ige', a, 0)),
    ])


optimizations.extend([
   (('fmin', a, ('fneg', a)), ('fneg', ('fabs', a))),
   (('imin', a, ('ineg', a)), ('ineg', ('iabs', a))),
   (('fmin', a, ('fneg', ('fabs', a))), ('fneg', ('fabs', a))),
   (('imin', a, ('ineg', ('iabs', a))), ('ineg', ('iabs', a))),
   (('fmin', a, ('fabs', a)), ('fcanonicalize', a)),
   (('imin', a, ('iabs', a)), a),
   (('fmax', a, ('fneg', ('fabs', a))), ('fcanonicalize', a)),
   (('imax', a, ('ineg', ('iabs', a))), a),
   (('fmax', a, ('fabs', a)), ('fabs', a)),
   (('imax', a, ('iabs', a)), ('iabs', a)),
   (('fmax', a, ('fneg', a)), ('fabs', a)),
   (('imax', a, ('ineg', a)), ('iabs', a), '!options->lower_iabs'),
   (('~fmax', ('fabs', a), 0.0), ('fabs', a)),

   (('fmin', ('fmax', a, 0.0), 1.0), ('fsat', a), '!options->lower_fsat'),
   (('~fmax', ('fmin', a, 1.0), 0.0), ('fsat', a), '!options->lower_fsat'),
   (('~fmin', ('fmax', a, -1.0), 0.0), ('fneg', ('fsat', ('fneg', a))), '!options->lower_fsat'),
   (('~fmax', ('fmin', a, 0.0), -1.0), ('fneg', ('fsat', ('fneg', a))), '!options->lower_fsat'),
   (('fmax', ('fmin', 'a(is_a_number)', 1.0), 0.0), ('fsat', a), '!options->lower_fsat'),
   (('fmin', ('fmax', 'a(is_a_number)', 0.0), 1.0), ('fsat', a), '!options->lower_fsat'),

   (('fsat', ('fsign', a)), ('b2f', ('!flt', 0.0, a))),
   (('fsat', ('b2f', a)), ('b2f', a)),
   (('fsat', a), ('fmin', ('fmax', a, 0.0), 1.0), 'options->lower_fsat'),
   (('fsat', ('fsat', a)), ('fsat', a)),

   (('fsat', ('fneg(is_used_once)', ('fadd(is_used_once)', a, b))), ('fsat', ('fadd', ('fneg', a), ('fneg', b))), '!options->lower_fsat'),
   (('fsat', ('fneg(is_used_once)', ('fmul(is_used_once)', a, b))), ('fsat', ('fmul', ('fneg', a), b)), '!options->lower_fsat'),
   (('fsat(nsz)', ('fneg(is_used_once)', ('fmulz(is_used_once)', a, b))), ('fsat', ('fmulz', ('fneg', a), b)), '!options->lower_fsat'),
   (('fsat', ('fabs(is_used_once)', ('fmul(is_used_once)', a, b))), ('fsat', ('fmul', ('fabs', a), ('fabs', b))), '!options->lower_fsat'),

   (('fmin', ('fmax', ('fmin', ('fmax', a, b), c), b), c), ('fmin', ('fmax', a, b), c)),
   (('imin', ('imax', ('imin', ('imax', a, b), c), b), c), ('imin', ('imax', a, b), c)),
   (('umin', ('umax', ('umin', ('umax', a, b), c), b), c), ('umin', ('umax', a, b), c)),

   (('fmax', ('fsat', a), '#b(is_zero_to_one)'), ('fsat', ('fmax', a, b))),
   (('fmax', ('fsat(is_used_once)', a), ('fsat(is_used_once)', b)), ('fsat', ('fmax', a, b))),
   (('~fmin', ('fsat', a), '#b(is_zero_to_one)'), ('fsat', ('fmin', a, b))),

   (('~fsat', ('fadd', 1.0, 'a(is_not_negative)')), 1.0),
   (('fsat', ('fadd', 1.0, 'a(is_a_number_not_negative)')), 1.0),

   (('fneg', ('bcsel(is_used_once)', a, '#b', '#c')), ('bcsel', a, ('fneg', b), ('fneg', c))),

   (('fmax', ('fneg', ('fmin', b, a)), b), ('fmax', ('fabs', b), ('fneg', a))),
   (('fmin', ('fneg', ('fmax', b, a)), b), ('fmin', ('fneg', ('fabs', b)), ('fneg', a))),

   (('fmax', ('fadd(is_used_once)', 'a(is_not_positive)', '#b(is_zero_to_one)'), 0.0),
    ('fsat', ('fadd', a, b)), '!options->lower_fsat'),

   (('fmax', ('ffma(is_used_once)', 'a(is_not_positive)', 'b(is_not_negative)', '#c(is_zero_to_one)'), 0.0),
    ('fsat', ('ffma', a, b, c)), '!options->lower_fsat'),
   (('fmax', ('ffma(is_used_once)', 'a', ('fneg', a), '#b(is_zero_to_one)'), 0.0),
    ('fsat', ('ffma', a, ('fneg', a), b)), '!options->lower_fsat'),

   (('fsat', ('fmax', a, 'b(is_not_positive)')), ('fsat', a)),

   (('fsat', ('bcsel(is_used_once)', a, b, '#c')), ('bcsel', a, ('fsat', b), ('fsat', c))),
   (('fsat', ('bcsel(is_used_once)', a, '#b', c)), ('bcsel', a, ('fsat', b), ('fsat', c))),

   (('extract_u8', ('imin', ('imax', a, 0), 0xff), 0), ('imin', ('imax', a, 0), 0xff)),
])

optimizations.extend([
   (('ior', ('flt(is_used_once)', a, b), ('flt', a, c)), ('flt', a, ('!fmax', b, c))),
   (('ior', ('flt(is_used_once)', a, c), ('flt', b, c)), ('flt', ('!fmin', a, b), c)),
   (('ior', ('fge(is_used_once)', a, b), ('fge', a, c)), ('fge', a, ('!fmin', b, c))),
   (('ior', ('fge(is_used_once)', a, c), ('fge', b, c)), ('fge', ('!fmax', a, b), c)),

   (('ior', ('flt', a, '#b'), ('flt', a, '#c')), ('flt', a, ('!fmax', b, c))),
   (('ior', ('flt', '#a', c), ('flt', '#b', c)), ('flt', ('!fmin', a, b), c)),
   (('ior', ('fge', a, '#b'), ('fge', a, '#c')), ('fge', a, ('!fmin', b, c))),
   (('ior', ('fge', '#a', c), ('fge', '#b', c)), ('fge', ('!fmax', a, b), c)),

   (('~iand', ('flt(is_used_once)', a, b), ('flt', a, c)), ('flt', a, ('fmin', b, c))),
   (('~iand', ('flt(is_used_once)', a, c), ('flt', b, c)), ('flt', ('fmax', a, b), c)),
   (('~iand', ('fge(is_used_once)', a, b), ('fge', a, c)), ('fge', a, ('fmax', b, c))),
   (('~iand', ('fge(is_used_once)', a, c), ('fge', b, c)), ('fge', ('fmin', a, b), c)),

   (('iand', ('flt', a, '#b(is_a_number)'), ('flt', a, '#c(is_a_number)')), ('flt', a, ('fmin', b, c))),
   (('iand', ('flt', '#a(is_a_number)', c), ('flt', '#b(is_a_number)', c)), ('flt', ('fmax', a, b), c)),
   (('iand', ('fge', a, '#b(is_a_number)'), ('fge', a, '#c(is_a_number)')), ('fge', a, ('fmax', b, c))),
   (('iand', ('fge', '#a(is_a_number)', c), ('fge', '#b(is_a_number)', c)), ('fge', ('fmin', a, b), c)),

   (('iand', ('uge', a, b), ('ult', a, b)), False),

   (('ior', ('ior(is_used_once)', ('flt(is_used_once)', a, c), d), ('flt', b, c)),
    ('ior', ('flt', ('!fmin', a, b), c), d)),
   (('ior', ('ior(is_used_once)', ('flt', a, c), d), ('flt(is_used_once)', b, c)),
    ('ior', ('flt', ('!fmin', a, b), c), d)),
   (('ior', ('ior(is_used_once)', ('flt(is_used_once)', a, b), d), ('flt', a, c)),
    ('ior', ('flt', a, ('!fmax', b, c)), d)),
   (('ior', ('ior(is_used_once)', ('flt', a, b), d), ('flt(is_used_once)', a, c)),
    ('ior', ('flt', a, ('!fmax', b, c)), d)),

   (('ior', ('flt', 'a(is_a_number)', 'b(is_a_number)'), ('flt', b, a)), ('fneu', a, b)),

   (('umin', ('iand', a, '#b(is_pos_power_of_two)'), ('iand', c, b)),
    ('iand', ('iand', a, b), ('iand', c, b))),
])

for s in (16, 32, 64):
    if s == 64:
        match_fsign_cond = "!options->lower_fsign & !(options->lower_doubles_options & nir_lower_dsign)"
    else:
        match_fsign_cond = "!options->lower_fsign"

    optimizations.extend([
        (('ior', ('flt(is_used_once)', 0.0, f'a@{s}'), ('flt', f'b@{s}', 0.0)),
         ('flt', 0.0, ('fmax', a, ('fneg', b)))),
        (('ior', ('flt', 0.0, f'a@{s}'), ('flt(is_used_once)', f'b@{s}', 0.0)),
         ('flt', 0.0, ('fmax', a, ('fneg', b)))),

        (('ior', ('fge(is_used_once)', 0.0, f'a@{s}'), ('fge', f'b@{s}', 0.0)),
         ('fge', 0.0, ('fmin', a, ('fneg', b)))),
        (('ior', ('fge', 0.0, f'a@{s}'), ('fge(is_used_once)', f'b@{s}', 0.0)),
         ('fge', 0.0, ('fmin', a, ('fneg', b)))),

        (('~iand', ('flt(is_used_once)', 0.0, f'a@{s}'), ('flt', f'b@{s}', 0.0)),
         ('flt', 0.0, ('fmin', a, ('fneg', b)))),
        (('~iand', ('flt', 0.0, f'a@{s}'), ('flt(is_used_once)', f'b@{s}', 0.0)),
         ('flt', 0.0, ('fmin', a, ('fneg', b)))),

        (('~iand', ('fge(is_used_once)', 0.0, f'a@{s}'), ('fge', f'b@{s}', 0.0)),
         ('fge', 0.0, ('fmax', a, ('fneg', b)))),
        (('~iand', ('fge', 0.0, f'a@{s}'), ('fge(is_used_once)', f'b@{s}', 0.0)),
         ('fge', 0.0, ('fmax', a, ('fneg', b)))),

        (('ior', ('feq(is_used_once)', f'a@{s}', 0.0), ('feq', f'b@{s}', 0.0)),
         ('feq', ('fmin', ('fabs', a), ('fabs', b)), 0.0)),
        (('ior', ('fneu(is_used_once)', f'a@{s}', 0.0), ('fneu', f'b@{s}', 0.0)),
         ('fneu', ('fadd', ('fabs', a), ('fabs', b)), 0.0)),
        (('iand', ('feq(is_used_once)', f'a@{s}', 0.0), ('feq', f'b@{s}', 0.0)),
         ('feq', ('fadd', ('fabs', a), ('fabs', b)), 0.0)),
        (('iand', ('fneu(is_used_once)', f'a@{s}', 0.0), ('fneu', f'b@{s}', 0.0)),
         ('fneu', ('fmin', ('fabs', a), ('fabs', b)), 0.0)),

        (('bcsel@{}'.format(s),
          ('feq', a, 0.0), 1.0,
          ('i2f{}'.format(s),
           ('iadd',
            ('b2i{}'.format(s), ('flt', 0.0, f'a@{s}')),
            ('ineg', ('b2i{}'.format(s), ('flt', f'a@{s}', 0.0)))))),
         ('i2f{}'.format(s),
          ('iadd',
           ('b2i32', ('!fge', a, 0.0)),
           ('ineg', ('b2i32', ('!flt', a, 0.0)))))),

        (('fmul',
          ('fexp2', ('fmul', ('flog2', ('fabs', a)), b)),
          ('i2f',
           ('iadd',
            ('b2i', ('flt', 0.0, a)),
            ('ineg', ('b2i', ('flt', a, 0.0)))))),
         ('bcsel',
          ('!flt', a, 0.0),
          ('fneg', ('fexp2', ('fmul', ('flog2', ('fabs', a)), b))),
          ('fexp2', ('fmul', ('flog2', ('fabs', a)), b))),
         'true', TestStatus.XFAIL),

        (('~i2f{}'.format(s), ('f2i', f'a@{s}')), ('ftrunc', a)),

        (('~f2i{}'.format(s), ('i2f', f'a@{s}')), a),
        (('~f2i{}'.format(s), ('u2f', f'a@{s}')), a),
        (('~f2u{}'.format(s), ('i2f', f'a@{s}')), a),
        (('~f2u{}'.format(s), ('u2f', f'a@{s}')), a),

        (('fadd', ('b2f{}'.format(s), ('flt', 0.0, f'a@{s}')),
          ('fneg', ('b2f{}'.format(s), ('flt', f'a@{s}', 0.0)))),
         ('fsign', a), match_fsign_cond),
        (('iadd', ('b2i{}'.format(s), ('flt', 0, f'a@{s}')),
          ('ineg', ('b2i{}'.format(s), ('flt', f'a@{s}', 0)))),
         ('f2i{}'.format(s), ('fsign', a)), match_fsign_cond),

        (('~f2f{}'.format(s), ('f2f', a)), ('f2f{}'.format(s), a)),

        (('~f2f{}'.format(s), ('u2f', a)), ('u2f{}'.format(s), a)),
        (('~f2f{}'.format(s), ('i2f', a)), ('i2f{}'.format(s), a)),

        (('~f2u{}'.format(s), ('f2f', a)), ('f2u{}'.format(s), a)),
        (('~f2i{}'.format(s), ('f2f', a)), ('f2i{}'.format(s), a)),

        (('i2f{}'.format(s), ('f2i', ('fsign', f'a@{s}'))), ('fsign', a)),
    ])

    if s < 64:
        optimizations.extend([
            (('bcsel', a, ('b2f(is_used_once)', f'b@{s}'), ('b2f', f'c@{s}')),
             ('b2f', ('bcsel', a, b, c))),
        ])

    for B in (32, 64):
        if s < B:
            optimizations.extend([
                (('f2f{}'.format(s), ('f2f{}'.format(B), f'a@{s}')), ('fcanonicalize', a)),

                (('f2u{}'.format(B), ('f2f{}'.format(B), f'a@{s}')), ('f2u{}'.format(B), a)),
                (('f2i{}'.format(B), ('f2f{}'.format(B), f'a@{s}')), ('f2i{}'.format(B), a)),

                (('f2f{}'.format(s), ('u2f{}'.format(B), a)), ('u2f{}'.format(s), a)),
                (('f2f{}'.format(s), ('i2f{}'.format(B), a)), ('i2f{}'.format(s), a)),
            ])

for S in (1, 8, 16, 32):
    for B in (8, 16, 32, 64):
        if B <= S:
            continue

        optimizations.extend([
            (('i2i{}'.format(S), ('i2i{}'.format(B), f'a@{S}')), a),
            (('u2u{}'.format(S), ('u2u{}'.format(B), f'a@{S}')), a),
        ])

        if B < 16:
            continue

        for C in (8, 16, 32, 64):
            if C <= S:
                continue

            optimizations.extend([
                (('u2f{}'.format(B), ('u2u{}'.format(C), f'a@{S}')), ('u2f{}'.format(B), a)),
                (('i2f{}'.format(B), ('i2i{}'.format(C), f'a@{S}')), ('i2f{}'.format(B), a)),
            ])


optimizations.extend([
    (('f2fmp', ('u2f32', 'a@32')), ('u2fmp', a), 'true', TestStatus.UNSUPPORTED),
    (('f2fmp', ('i2f32', 'a@32')), ('i2fmp', a), 'true', TestStatus.UNSUPPORTED),

    (('f2u16', ('f2fmp', 'a@32')), ('f2u16', a), 'true', TestStatus.UNSUPPORTED),
    (('f2i16', ('f2fmp', 'a@32')), ('f2i16', a), 'true', TestStatus.UNSUPPORTED),

    (('i2imp', ('f2u32', 'a@32')), ('f2ump', a), 'true', TestStatus.UNSUPPORTED),
    (('i2imp', ('f2i32', 'a@32')), ('f2imp', a), 'true', TestStatus.UNSUPPORTED),

    (('u2f16', ('i2imp', 'a@32')), ('u2f16', a), 'true', TestStatus.UNSUPPORTED),
    (('i2f16', ('i2imp', 'a@32')), ('i2f16', a), 'true', TestStatus.UNSUPPORTED),
])

optimizations.extend([
    (('iand', ('u2u16', ('u2u8', 'a@16')), '#b'), ('iand', a, ('iand', b, 0xff))),
    (('u2u16', ('u2u8(is_used_once)', ('iand', 'a@16', '#b'))), ('iand', a, ('iand', b, 0xff))),
])

for op in ('iand', 'ior', 'ixor'):
    optimizations.extend([
        (('u2u8', (op, ('u2u16', ('u2u8', 'a@16')), ('u2u16', ('u2u8', 'b@16')))), ('u2u8', (op, a, b))),
        (('u2u8', (op, ('u2u16', ('u2u8', 'a@32')), ('u2u16', ('u2u8', 'b@32')))), ('u2u8', (op, a, b))),

        ((op, ('extract_i8', a, '#b'), ('extract_i8', c, b)), ('extract_i8', (op, a, c), b)),
        ((op, ('extract_u8', a, '#b'), ('extract_u8', c, b)), ('extract_u8', (op, a, c), b)),
        ((op, ('extract_i16', a, '#b'), ('extract_i16', c, b)), ('extract_i16', (op, a, c), b)),
        ((op, ('extract_u16', a, '#b'), ('extract_u16', c, b)), ('extract_u16', (op, a, c), b)),

        ((op, ('ushr(is_used_once)', a, '#b'), ('ushr', c, b)), ('ushr', (op, a, c), b)),
        ((op, ('ishr(is_used_once)', a, '#b'), ('ishr', c, b)), ('ishr', (op, a, c), b)),
        ((op, ('ishl(is_used_once)', a, '#b'), ('ishl', c, b)), ('ishl', (op, a, c), b)),
    ])

for s in (8, 16, 32, 64):
    amount_bits = s.bit_length() - 1

    lower_umin = 'options->lower_umin'
    lower_umax = 'options->lower_umax'
    lower_imin = 'false'
    lower_imax = 'false'
    lower_ior = 'options->lower_bitops'
    if s == 64:
        lower_umin = '(options->lower_umin || (options->lower_int64_options & nir_lower_minmax64) != 0)'
        lower_umax = '(options->lower_umax || (options->lower_int64_options & nir_lower_minmax64) != 0)'
        lower_imin = '((options->lower_int64_options & nir_lower_minmax64) != 0)'
        lower_imax = '((options->lower_int64_options & nir_lower_minmax64) != 0)'
        lower_ior = '(options->lower_bitops || (options->lower_int64_options & nir_lower_logic64) != 0)'

    optimizations.extend([
        (('iand', ('ieq', f'a@{s}', 0), ('ieq', f'b@{s}', 0)),
         ('ieq', ('ior', a, b), 0),
         f'{lower_umax} && !{lower_ior}'),
        (('ior',  ('ine', f'a@{s}', 0), ('ine', f'b@{s}', 0)),
         ('ine', ('ior', a, b), 0),
         f'{lower_umin} && !{lower_ior}'),
        (('iand', ('ieq', f'a@{s}', 0), ('ieq', f'b@{s}', 0)),
         ('ieq', ('umax', a, b), 0),
         f'!{lower_umax}'),
        (('ior',  ('ieq', f'a@{s}', 0), ('ieq', f'b@{s}', 0)),
         ('ieq', ('umin', a, b), 0),
         f'!{lower_umin}'),
        (('iand', ('ine', f'a@{s}', 0), ('ine', f'b@{s}', 0)),
         ('ine', ('umin', a, b), 0),
         f'!{lower_umin}'),
        (('ior',  ('ine', f'a@{s}', 0), ('ine', f'b@{s}', 0)),
         ('ine', ('umax', a, b), 0),
         f'!{lower_umax}'),

        (('bcsel', ('ult', f'b@{s}', a), b, a), ('umin', a, b), f'!{lower_umin}'),
        (('bcsel', ('ult', f'a@{s}', b), b, a), ('umax', a, b), f'!{lower_umax}'),
        (('bcsel', ('uge', f'a@{s}', b), b, a), ('umin', a, b), f'!{lower_umin}'),
        (('bcsel', ('uge', f'b@{s}', a), b, a), ('umax', a, b), f'!{lower_umax}'),
        (('bcsel', ('ilt', f'b@{s}', a), b, a), ('imin', a, b), f'!{lower_imin}'),
        (('bcsel', ('ilt', f'a@{s}', b), b, a), ('imax', a, b), f'!{lower_imax}'),
        (('bcsel', ('ige', f'a@{s}', b), b, a), ('imin', a, b), f'!{lower_imin}'),
        (('bcsel', ('ige', f'b@{s}', a), b, a), ('imax', a, b), f'!{lower_imax}'),

        (('ishl', f'a@{s}', ('iand', s - 1, b)), ('ishl', a, b)),
        (('ishr', f'a@{s}', ('iand', s - 1, b)), ('ishr', a, b)),
        (('ushr', f'a@{s}', ('iand', s - 1, b)), ('ushr', a, b)),
        (('ushr', f'a@{s}', ('ishl(is_used_once)', ('iand', b, 1), amount_bits - 1)),
         ('ushr', a, ('ishl', b, amount_bits - 1))),
        (('ushr', f'a@{s}', ('ishl(is_used_once)', ('iand', b, 3), amount_bits - 2)),
         ('ushr', a, ('ishl', b, amount_bits - 2))),

        (('ior', ('ilt(is_used_once)', f'a@{s}', b), ('ilt', a, c)),
         ('ilt', a, ('imax', b, c)), f'!{lower_imax}'),
        (('ior', ('ilt(is_used_once)', f'a@{s}', c), ('ilt', b, c)),
         ('ilt', ('imin', a, b), c), f'!{lower_imin}'),
        (('ior', ('ige(is_used_once)', f'a@{s}', b), ('ige', a, c)),
         ('ige', a, ('imin', b, c)), f'!{lower_imin}'),
        (('ior', ('ige(is_used_once)', f'a@{s}', c), ('ige', b, c)),
         ('ige', ('imax', a, b), c), f'!{lower_imax}'),

        (('ior', ('ult(is_used_once)', f'a@{s}', b), ('ult', a, c)),
         ('ult', a, ('umax', b, c)), f'!{lower_umax}'),
        (('ior', ('ult(is_used_once)', f'a@{s}', c), ('ult', b, c)),
         ('ult', ('umin', a, b), c), f'!{lower_umin}'),
        (('ior', ('uge(is_used_once)', f'a@{s}', b), ('uge', a, c)),
         ('uge', a, ('umin', b, c)), f'!{lower_umin}'),
        (('ior', ('uge(is_used_once)', f'a@{s}', c), ('uge', b, c)),
         ('uge', ('umax', a, b), c), f'!{lower_umax}'),

        (('iand', ('ilt(is_used_once)', f'a@{s}', b), ('ilt', a, c)),
         ('ilt', a, ('imin', b, c)), f'!{lower_imin}'),
        (('iand', ('ilt(is_used_once)', f'a@{s}', c), ('ilt', b, c)),
         ('ilt', ('imax', a, b), c), f'!{lower_imax}'),
        (('iand', ('ige(is_used_once)', f'a@{s}', b), ('ige', a, c)),
         ('ige', a, ('imax', b, c)), f'!{lower_imax}'),
        (('iand', ('ige(is_used_once)', f'a@{s}', c), ('ige', b, c)),
         ('ige', ('imin', a, b), c), f'!{lower_imin}'),

        (('iand', ('ult(is_used_once)', f'a@{s}', b), ('ult', a, c)),
         ('ult', a, ('umin', b, c)), f'!{lower_umin}'),
        (('iand', ('ult(is_used_once)', f'a@{s}', c), ('ult', b, c)),
         ('ult', ('umax', a, b), c), f'!{lower_umax}'),
        (('iand', ('uge(is_used_once)', f'a@{s}', b), ('uge', a, c)),
         ('uge', a, ('umax', b, c)), f'!{lower_umax}'),
        (('iand', ('uge(is_used_once)', f'a@{s}', c), ('uge', b, c)),
         ('uge', ('umin', a, b), c), f'!{lower_umin}'),
    ])

    if s < 64:
        optimizations.append((('ineg', (f'b2i{s}', f'a@{s}')), a))


optimizations.extend([
    (('ior', ('ieq', a, 0), ('ieq', a, 1)), ('uge', 1, a)),
    (('ior', ('uge', 1, a), ('ieq', a, 2)), ('uge', 2, a)),
    (('ior', ('uge', 2, a), ('ieq', a, 3)), ('uge', 3, a)),

    (('ior', ('ieq', a, 0), ('ior', ('ieq', a, 1), b)), ('ior', ('uge', 1, a), b)),
    (('ior', ('uge', 1, a), ('ior', ('ieq', a, 2), b)), ('ior', ('uge', 2, a), b)),
    (('ior', ('uge', 2, a), ('ior', ('ieq', a, 3), b)), ('ior', ('uge', 3, a), b)),

    (('ior', a, ('ieq', a, False)), True),

    (('uge', a, 1), ('ine', a, 0)),
    (('ult', a, 1), ('ieq', a, 0)),
    (('uge', 0, a), ('ieq', a, 0)),
    (('ult', 0, a), ('ine', a, 0)),

    (('b2i', ('ine', 'a@1', 'b@1')), ('b2i', ('ixor', a, b))),

    (('ishl', ('b2i32', ('ine', ('iand', 'a@32', '#b(is_pos_power_of_two)'), 0)), '#c'),
     ('bcsel',
      ('ige', ('iand', c, 31), ('find_lsb', b)),
      ('ishl', ('iand', a, b), ('iadd', ('iand', c, 31), ('ineg', ('find_lsb', b)))),
      ('ushr', ('iand', a, b), ('iadd', ('ineg', ('iand', c, 31)), ('find_lsb', b))))),

    (('b2i32', ('ine', ('iand', 'a@32', '#b(is_pos_power_of_two)'), 0)),
     ('ushr', ('iand', a, b), ('find_lsb', b)),
     '!options->lower_bitops'),

    (('ior', ('b2i', a), ('iand', b, 1)), ('iand', ('ior', ('b2i', a), b), 1)),
    (('iand', ('b2i', a), ('iand', b, 1)), ('iand', ('b2i', a), b)),

    (('iand', ('inot', ('iand', ('ior', ('ieq', a, 0), b), c)), ('ilt', a, 0)),
     ('iand', ('inot', ('iand', b, c)), ('ilt', a, 0))),
    (('iand', ('inot', ('iand', ('ieq', ('umin', a, b), 0), c)), ('ilt', a, 0)),
     ('iand', ('inot', ('iand', ('ieq', b, 0), c)), ('ilt', a, 0))),

    (('flt', a, ('fmax', b, a)), ('flt', a, b)),
    (('flt', ('fmin', a, b), a), ('flt', b, a)),
    (('~fge', a, ('fmin', b, a)), True),
    (('~fge', ('fmax', a, b), a), True),
    (('flt', a, ('fmin', b, a)), False),
    (('flt', ('fmax', a, b), a), False),
    (('~fge', a, ('fmax', b, a)), ('fge', a, b)),
    (('~fge', ('fmin', a, b), a), ('fge', b, a)),

    (('ilt', a, ('imax', b, a)), ('ilt', a, b)),
    (('ilt', ('imin', a, b), a), ('ilt', b, a)),
    (('ige', a, ('imin', b, a)), True),
    (('ige', ('imax', a, b), a), True),

    (('ult', a, ('umax', b, a)), ('ult', a, b)),
    (('ult', ('umin', a, b), a), ('ult', b, a)),
    (('uge', a, ('umin', b, a)), True),
    (('uge', ('umax', a, b), a), True),
    (('ilt', a, ('imin', b, a)), False),
    (('ilt', ('imax', a, b), a), False),
    (('ige', a, ('imax', b, a)), ('ige', a, b)),
    (('ige', ('imin', a, b), a), ('ige', b, a)),

    (('ult', a, ('umin', b, a)), False),
    (('ult', ('umax', a, b), a), False),
    (('uge', a, ('umax', b, a)), ('uge', a, b)),
    (('uge', ('umin', a, b), a), ('uge', b, a)),
    (('ult', a, ('iand', b, a)), False),
    (('ult', ('ior', a, b), a), False),
    (('uge', a, ('iand', b, a)), True),
    (('uge', ('ior', a, b), a), True),

    (('ilt', '#a', ('imax', '#b', c)), ('ior', ('ilt', a, b), ('ilt', a, c))),
    (('ilt', ('imin', '#a', b), '#c'), ('ior', ('ilt', a, c), ('ilt', b, c))),
    (('ige', '#a', ('imin', '#b', c)), ('ior', ('ige', a, b), ('ige', a, c))),
    (('ige', ('imax', '#a', b), '#c'), ('ior', ('ige', a, c), ('ige', b, c))),
    (('ult', '#a', ('umax', '#b', c)), ('ior', ('ult', a, b), ('ult', a, c))),
    (('ult', ('umin', '#a', b), '#c'), ('ior', ('ult', a, c), ('ult', b, c))),
    (('uge', '#a', ('umin', '#b', c)), ('ior', ('uge', a, b), ('uge', a, c))),
    (('uge', ('umax', '#a', b), '#c'), ('ior', ('uge', a, c), ('uge', b, c))),

    (('ilt', '#a', ('imin', '#b', c)), ('iand', ('ilt', a, b), ('ilt', a, c))),
    (('ilt', ('imax', '#a', b), '#c'), ('iand', ('ilt', a, c), ('ilt', b, c))),
    (('ige', '#a', ('imax', '#b', c)), ('iand', ('ige', a, b), ('ige', a, c))),
    (('ige', ('imin', '#a', b), '#c'), ('iand', ('ige', a, c), ('ige', b, c))),
    (('ult', '#a', ('umin', '#b', c)), ('iand', ('ult', a, b), ('ult', a, c))),
    (('ult', ('umax', '#a', b), '#c'), ('iand', ('ult', a, c), ('ult', b, c))),
    (('uge', '#a', ('umax', '#b', c)), ('iand', ('uge', a, b), ('uge', a, c))),
    (('uge', ('umin', '#a', b), '#c'), ('iand', ('uge', a, c), ('uge', b, c))),

    (('ieq', ('umin', '#a', b), 0), ('ior', ('ieq', a, 0), ('ieq', b, 0))),
    (('ine', ('umax', '#a', b), 0), ('ior', ('ine', a, 0), ('ine', b, 0))),
    (('ieq', ('umax', '#a', b), 0), ('iand', ('ieq', a, 0), ('ieq', b, 0))),
    (('ine', ('umin', '#a', b), 0), ('iand', ('ine', a, 0), ('ine', b, 0))),

    (('bcsel', ('ilt', a, 0), ('ineg', ('ishr', a, b)), ('ishr', a, b)),
     ('iabs', ('ishr', a, b))),
    (('iabs', ('ishr', ('iabs', a), b)), ('ushr', ('iabs', a), b)),
    (('iabs', ('ushr', ('iabs', a), b)), ('ushr', ('iabs', a), b)),
])

optimizations.extend([
   (('fabs', ('slt', a, b)), ('slt', a, b)),
   (('fabs', ('sge', a, b)), ('sge', a, b)),
   (('fabs', ('seq', a, b)), ('seq', a, b)),
   (('fabs', ('sne', a, b)), ('sne', a, b)),

   (('slt', a, b), ('b2f', ('flt', a, b)), 'options->lower_scmp'),
   (('sge', a, b), ('b2f', ('fge', a, b)), 'options->lower_scmp'),
   (('seq', a, b), ('b2f', ('feq', a, b)), 'options->lower_scmp'),
   (('sne', a, b), ('b2f', ('fneu', a, b)), 'options->lower_scmp'),

   (('slt', a, -0.0), ('slt', a, 0.0)),
   (('slt', -0.0, a), ('slt', 0.0, a)),
   (('sge', a, -0.0), ('sge', a, 0.0)),
   (('sge', -0.0, a), ('sge', 0.0, a)),
   (('seq', a, -0.0), ('seq', a, 0.0)),
   (('sne', a, -0.0), ('sne', a, 0.0)),

   (('seq', ('seq', a, b), 1.0), ('seq', a, b)),
   (('seq', ('sne', a, b), 1.0), ('sne', a, b)),
   (('seq', ('slt', a, b), 1.0), ('slt', a, b)),
   (('seq', ('sge', a, b), 1.0), ('sge', a, b)),

   (('sne', ('seq', a, b), 0.0), ('seq', a, b)),
   (('sne', ('sne', a, b), 0.0), ('sne', a, b)),
   (('sne', ('slt', a, b), 0.0), ('slt', a, b)),
   (('sne', ('sge', a, b), 0.0), ('sge', a, b)),

   (('seq', ('seq', a, b), 0.0), ('sne', a, b)),
   (('seq', ('sne', a, b), 0.0), ('seq', a, b)),
   (('seq', ('slt', a, b), 0.0), ('sge', a, b), 'true', TestStatus.XFAIL),
   (('seq', ('sge', a, b), 0.0), ('slt', a, b), 'true', TestStatus.XFAIL),

   (('sne', ('seq', a, b), 1.0), ('sne', a, b)),
   (('sne', ('sne', a, b), 1.0), ('seq', a, b)),
   (('sne', ('slt', a, b), 1.0), ('sge', a, b), 'true', TestStatus.XFAIL),
   (('sne', ('sge', a, b), 1.0), ('slt', a, b), 'true', TestStatus.XFAIL),

   (('fall_equal2', a, b), ('fmin', ('seq', 'a.x', 'b.x'), ('seq', 'a.y', 'b.y')), 'options->lower_vector_cmp'),
   (('fall_equal3', a, b), ('seq', ('fany_nequal3', a, b), 0.0), 'options->lower_vector_cmp'),
   (('fall_equal4', a, b), ('seq', ('fany_nequal4', a, b), 0.0), 'options->lower_vector_cmp'),
   (('fall_equal8', a, b), ('seq', ('fany_nequal8', a, b), 0.0), 'options->lower_vector_cmp'),
   (('fall_equal16', a, b), ('seq', ('fany_nequal16', a, b), 0.0), 'options->lower_vector_cmp', TestStatus.UNSUPPORTED),

   (('fany_nequal2', a, b), ('fmax', ('sne', 'a.x', 'b.x'), ('sne', 'a.y', 'b.y')), 'options->lower_vector_cmp'),
   (('fany_nequal3', a, b), ('fsat', ('fdot3', ('sne', a, b), ('sne', a, b))), 'options->lower_vector_cmp'),
   (('fany_nequal4', a, b), ('fsat', ('fdot4', ('sne', a, b), ('sne', a, b))), 'options->lower_vector_cmp'),
   (('fany_nequal8', a, b), ('fsat', ('fdot8', ('sne', a, b), ('sne', a, b))), 'options->lower_vector_cmp'),
   (('fany_nequal16', a, b), ('fsat', ('fdot16', ('sne', a, b), ('sne', a, b))), 'options->lower_vector_cmp', TestStatus.UNSUPPORTED),

   (('f2bf', a),
    ('bcsel', ('!fneu', a, a), -1, ('unpack_32_2x16_split_y', a)),
    'options->lower_bfloat16_conversions', TestStatus.UNSUPPORTED),
   (('bf2f', a), ('pack_32_2x16', ('vec2', 0, a)), 'options->lower_bfloat16_conversions'),
])


def vector_cmp(reduce_op, cmp_op, comps):
    if len(comps) == 1:
        c0 = comps[0]
        return (cmp_op, 'a.' + c0, 'b.' + c0)
    mid = len(comps) // 2
    return (reduce_op,
            vector_cmp(reduce_op, cmp_op, comps[:mid]),
            vector_cmp(reduce_op, cmp_op, comps[mid:]))


for op0, op1, op2 in (
    ('ball_iequal', 'ieq', 'iand'),
    ('ball_fequal', 'feq', 'iand'),
    ('bany_inequal', 'ine', 'ior'),
    ('bany_fnequal', 'fneu', 'ior'),
):
    optimizations.extend([
        ((op0 + '2', a, b), vector_cmp(op2, op1, 'xy'), 'options->lower_vector_cmp'),
        ((op0 + '3', a, b), vector_cmp(op2, op1, 'xyz'), 'options->lower_vector_cmp'),
        ((op0 + '4', a, b), vector_cmp(op2, op1, 'xyzw'), 'options->lower_vector_cmp', TestStatus.UNSUPPORTED),
        ((op0 + '8', a, b), vector_cmp(op2, op1, 'abcdefgh'), 'options->lower_vector_cmp', TestStatus.UNSUPPORTED),
        ((op0 + '16', a, b), vector_cmp(op2, op1, 'abcdefghijklmnop'), 'options->lower_vector_cmp', TestStatus.UNSUPPORTED),
    ])


for s in (8, 16, 32, 64):
    cond = 'true'
    if s == 64:
        cond = '!(options->lower_int64_options & nir_lower_conv64)'

    optimizations.extend([
        (('bcsel@{}'.format(s), a, -1, 0), ('ineg', ('b2i', 'a@1')), cond),
        (('bcsel@{}'.format(s), a, 0, -1), ('ineg', ('b2i', ('inot', a))), cond),
        (('bcsel@{}'.format(s), a, 1, 0), ('b2i', 'a@1'), cond),
        (('bcsel@{}'.format(s), a, 0, 1), ('b2i', ('inot', a)), cond),
    ])


optimizations.extend([
    (('iand', ('ineg', ('b2i', 'a@1')), b), ('bcsel', a, b, 0)),
    (('ior', ('ineg', ('b2i', 'a@1')), ('ineg', ('b2i', 'b@1'))), ('ineg', ('b2i', ('ior', a, b)))),
    (('ige', ('ineg', ('b2i', 'a@1')), 0), ('inot', a)),
    (('ilt', ('ineg', ('b2i', 'a@1')), 0), a),

    (('bcsel', a, ('b2i', 'b@1'), ('b2i', 'c@1')), ('b2i', ('bcsel', a, b, c))),
    (('bcsel', a, ('b2i', 'b@1'), 0), ('b2i', ('bcsel', a, b, False))),
    (('bcsel', a, ('b2i', 'b@1'), 1), ('b2i', ('bcsel', a, b, True))),
    (('bcsel', a, 0, ('b2i', 'b@1')), ('b2i', ('bcsel', a, False, b))),
    (('bcsel', a, 1, ('b2i', 'b@1')), ('b2i', ('bcsel', a, True, b))),

    (('bcsel', a, ('ineg', ('b2i', 'b@1')), ('ineg', ('b2i', 'c@1'))), ('ineg', ('b2i', ('bcsel', a, b, c)))),
    (('bcsel', a, ('ineg', ('b2i', 'b@1')), 0), ('ineg', ('b2i', ('bcsel', a, b, False)))),
    (('bcsel', a, ('ineg', ('b2i', 'b@1')), -1), ('ineg', ('b2i', ('bcsel', a, b, True)))),
    (('bcsel', a, 0, ('ineg', ('b2i', 'b@1'))), ('ineg', ('b2i', ('bcsel', a, False, b)))),
    (('bcsel', a, -1, ('ineg', ('b2i', 'b@1'))), ('ineg', ('b2i', ('bcsel', a, True, b)))),

    (('inot', ('ineg', ('b2i', a))), ('ineg', ('b2i', ('inot', a)))),

    (('ishl', ('ineg', ('b2i', a)), '#b'), ('iand', ('ishl', -1, b), ('ineg', ('b2i', a)))),
    (('ushr', ('ineg', ('b2i', a)), '#b'), ('iand', ('ushr', -1, b), ('ineg', ('b2i', a)))),

    (('ine', ('b2i', 'a@1'), ('ineg', ('b2i', 'b@1'))), ('ior', a, b)),
    (('ieq', ('b2i', 'a@1'), ('ineg', ('b2i', 'b@1'))), ('inot', ('ior', a, b))),
])


for op in ('ior', 'iand', 'ixor'):
    optimizations.extend([
        ((op, ('b2i', 'a@1'), ('b2i', 'b@1')), ('b2i', (op, a, b))),
        ((op, ('ineg', ('b2i', 'a@1')), ('ineg', ('b2i', 'b@1'))), ('ineg', ('b2i', (op, a, b)))),

        (('iand', (op, ('b2i', 'a@1'), ('ineg', ('b2i', 'b@1'))), 1), ('b2i', (op, a, b))),
    ])


optimizations.extend([
    (('feq', ('seq', a, b), 1.0), ('feq', a, b)),
    (('feq', ('sne', a, b), 1.0), ('fneu', a, b)),
    (('feq', ('slt', a, b), 1.0), ('flt', a, b)),
    (('feq', ('sge', a, b), 1.0), ('fge', a, b)),

    (('fneu', ('seq', a, b), 0.0), ('feq', a, b)),
    (('fneu', ('sne', a, b), 0.0), ('fneu', a, b)),
    (('fneu', ('slt', a, b), 0.0), ('flt', a, b)),
    (('fneu', ('sge', a, b), 0.0), ('fge', a, b)),

    (('feq', ('seq', a, b), 0.0), ('fneu', a, b)),
    (('feq', ('sne', a, b), 0.0), ('feq', a, b)),
    (('feq', ('slt', a, b), 0.0), ('fge', a, b), 'true', TestStatus.XFAIL),
    (('feq', ('sge', a, b), 0.0), ('flt', a, b), 'true', TestStatus.XFAIL),

    (('fneu', ('seq', a, b), 1.0), ('fneu', a, b)),
    (('fneu', ('sne', a, b), 1.0), ('feq', a, b)),
    (('fneu', ('slt', a, b), 1.0), ('fge', a, b), 'true', TestStatus.XFAIL),
    (('fneu', ('sge', a, b), 1.0), ('flt', a, b), 'true', TestStatus.XFAIL),

    (('fneu', ('fneg', a), a), ('fneu', a, 0.0)),
    (('feq', ('fneg', a), a), ('feq', a, 0.0)),

    (('imul', ('b2i', 'a@1'), ('b2i', 'b@1')), ('b2i', ('iand', a, b))),
    (('iand', ('b2i', 'a@1'), ('b2i', 'b@1')), ('b2i', ('iand', a, b))),
    (('ior',  ('b2i', 'a@1'), ('b2i', 'b@1')), ('b2i', ('ior', a, b))),
    (('fmul', ('b2f', 'a@1'), ('b2f', 'b@1')), ('b2f', ('iand', a, b))),
    (('ffma', ('b2f', 'a@1'), ('b2f', 'b@1'), c), ('fadd', ('b2f', ('iand', a, b)), c)),
    (('fmul', ('b2f', ('fneu', a, 0)), a), ('fmul', 1.0, a)),
    (('ffma', ('b2f', ('fneu', a, 0)), a, b), ('fadd', a, b)),
    (('fsat', ('fadd', ('b2f', 'a@1'), ('b2f', 'b@1'))), ('b2f', ('ior', a, b))),

    (('iand', 'a@bool16', 1.0), ('b2f', a)),
    (('iand', 'a@bool32', 1.0), ('b2f', a)),

    (('flt', ('fneg', ('b2f', 'a@1')), 0), a),

    (('ilt', a, a), False),
    (('ige', a, a), True),
    (('ieq', a, a), True),
    (('ine', a, a), False),
    (('ult', a, a), False),
    (('uge', a, a), True),

    (('flt', a, a), False),
    (('fge', 'a(is_a_number)', a), True),
    (('feq', 'a(is_a_number)', a), True),
    (('fneu', 'a(is_a_number)', a), False),

    (('iand', a, a), a),
    (('iand', a, 0), 0),
    (('iand', a, -1), a),
    (('iand', a, ('inot', a)), 0),

    (('ior', a, a), a),
    (('ior', a, 0), a),
    (('ior', a, -1), -1),
    (('ior', a, ('inot', a)), -1),

    (('ixor', a, a), 0),
    (('ixor', a, 0), a),
    (('ixor', a, ('ixor', a, b)), b),
    (('ixor', a, -1), ('inot', a)),

    (('inot', ('inot', a)), a),

    (('ior', ('iand', a, b), b), b),
    (('ior', ('ior', a, b), b), ('ior', a, b)),
    (('iand', ('ior', a, b), b), b),
    (('iand', ('iand', a, b), b), ('iand', a, b)),

    (('iand', ('iand(is_used_once)', a, b), ('iand(is_used_once)', a, c)), ('iand', a, ('iand', b, c))),
])

optimizations.extend([
   (('iand@64', a, '#b(is_lower_half_zero)'),
    ('pack_64_2x32_split',
     0,
     ('iand',
      ('unpack_64_2x32_split_y', a),
      ('unpack_64_2x32_split_y', b))),
    '!options->lower_pack_64_2x32_split'),
   (('iand@64', a, '#b(is_upper_half_zero)'),
    ('pack_64_2x32_split',
     ('iand',
      ('unpack_64_2x32_split_x', a),
      ('unpack_64_2x32_split_x', b)),
     0),
    '!options->lower_pack_64_2x32_split'),
   (('iand@64', a, '#b(is_lower_half_negative_one)'),
    ('pack_64_2x32_split',
     ('unpack_64_2x32_split_x', a),
     ('iand',
      ('unpack_64_2x32_split_y', a),
      ('unpack_64_2x32_split_y', b))),
    '!options->lower_pack_64_2x32_split'),
   (('iand@64', a, '#b(is_upper_half_negative_one)'),
    ('pack_64_2x32_split',
     ('iand',
      ('unpack_64_2x32_split_x', a),
      ('unpack_64_2x32_split_x', b)),
     ('unpack_64_2x32_split_y', a)),
    '!options->lower_pack_64_2x32_split'),

   (('ior@64', a, '#b(is_lower_half_zero)'),
    ('pack_64_2x32_split',
     ('unpack_64_2x32_split_x', a),
     ('ior',
      ('unpack_64_2x32_split_y', a),
      ('unpack_64_2x32_split_y', b))),
    '!options->lower_pack_64_2x32_split'),
   (('ior@64', a, '#b(is_upper_half_zero)'),
    ('pack_64_2x32_split',
     ('ior',
      ('unpack_64_2x32_split_x', a),
      ('unpack_64_2x32_split_x', b)),
     ('unpack_64_2x32_split_y', a)),
    '!options->lower_pack_64_2x32_split'),
   (('ior@64', a, '#b(is_lower_half_negative_one)'),
    ('pack_64_2x32_split',
     -1,
     ('ior',
      ('unpack_64_2x32_split_y', a),
      ('unpack_64_2x32_split_y', b))),
    '!options->lower_pack_64_2x32_split'),
   (('ior@64', a, '#b(is_upper_half_negative_one)'),
    ('pack_64_2x32_split',
     ('ior',
      ('unpack_64_2x32_split_x', a),
      ('unpack_64_2x32_split_x', b)),
     -1),
    '!options->lower_pack_64_2x32_split'),

   (('ixor@64', a, '#b(is_lower_half_zero)'),
    ('pack_64_2x32_split',
     ('unpack_64_2x32_split_x', a),
     ('ixor',
      ('unpack_64_2x32_split_y', a),
      ('unpack_64_2x32_split_y', b))),
    '!options->lower_pack_64_2x32_split'),
   (('ixor@64', a, '#b(is_upper_half_zero)'),
    ('pack_64_2x32_split',
     ('ixor',
      ('unpack_64_2x32_split_x', a),
      ('unpack_64_2x32_split_x', b)),
     ('unpack_64_2x32_split_y', a)),
    '!options->lower_pack_64_2x32_split'),

   (('iand', ('inot', a), ('inot', b)), ('inot', ('ior', a, b))),
   (('ior',  ('inot', a), ('inot', b)), ('inot', ('iand', a, b))),

   (('ishl', 0, a), 0),
   (('ishl', a, 0), a),
   (('ishr', 0, a), 0),
   (('ishr', -1, a), -1),
   (('ishr', a, 0), a),
   (('ushr', 0, a), 0),
   (('ushr', a, 0), a),

   (('bcsel', ('ieq', b, 0), a, ('ushr', a, b)), ('ushr', a, b)),
   (('bcsel', ('ieq', b, 0), a, ('ishr', a, b)), ('ishr', a, b)),
   (('bcsel', ('ieq', b, 0), a, ('ishl', a, b)), ('ishl', a, b)),
   (('bcsel', ('ine', b, 0), ('ushr', a, b), a), ('ushr', a, b)),
   (('bcsel', ('ine', b, 0), ('ishr', a, b), a), ('ishr', a, b)),
   (('bcsel', ('ine', b, 0), ('ishl', a, b), a), ('ishl', a, b)),

   (('ior', ('ishl@16', a, b), ('ushr@16', a, ('iadd', 16, ('ineg', b)))), ('urol', a, b), 'options->has_rotate16'),
   (('ior', ('ishl@16', a, b), ('ushr@16', a, ('isub', 16, b))), ('urol', a, b), 'options->has_rotate16'),
   (('ior', ('ishl@32', a, b), ('ushr@32', a, ('iadd', 32, ('ineg', b)))), ('urol', a, b), 'options->has_rotate32'),
   (('ior', ('ishl@32', a, b), ('ushr@32', a, ('isub', 32, b))), ('urol', a, b), 'options->has_rotate32'),

   (('ior', ('ushr@16', a, b), ('ishl@16', a, ('iadd', 16, ('ineg', b)))), ('uror', a, b), 'options->has_rotate16'),
   (('ior', ('ushr@16', a, b), ('ishl@16', a, ('isub', 16, b))), ('uror', a, b), 'options->has_rotate16'),
   (('ior', ('ushr@32', a, b), ('ishl@32', a, ('iadd', 32, ('ineg', b)))), ('uror', a, b), 'options->has_rotate32'),
   (('ior', ('ushr@32', a, b), ('ishl@32', a, ('isub', 32, b))), ('uror', a, b), 'options->has_rotate32'),

   (('urol@8',  a, b), ('ior', ('ishl', a, b), ('ushr', a, ('isub', 8, b))), '!options->has_rotate8'),
   (('urol@16', a, b), ('ior', ('ishl', a, b), ('ushr', a, ('isub', 16, b))), '!options->has_rotate16'),
   (('urol@32', a, b), ('ior', ('ishl', a, b), ('ushr', a, ('isub', 32, b))), '!options->has_rotate32'),
   (('urol@64', a, b), ('ior', ('ishl', a, b), ('ushr', a, ('isub', 64, b)))),

   (('uror@8',  a, b), ('ior', ('ushr', a, b), ('ishl', a, ('isub', 8, b))), '!options->has_rotate8'),
   (('uror@16', a, b), ('ior', ('ushr', a, b), ('ishl', a, ('isub', 16, b))), '!options->has_rotate16'),
   (('uror@32', a, b), ('ior', ('ushr', a, b), ('ishl', a, ('isub', 32, b))), '!options->has_rotate32'),
   (('uror@64', a, b), ('ior', ('ushr', a, b), ('ishl', a, ('isub', 64, b)))),

   (('bitfield_select', 0xff000000, ('ishl', 'b@32', 24), ('ushr', a, 8)), ('shfr', b, a, 8), 'options->has_shfr32'),
   (('bitfield_select', 0xffff0000, ('ishl', 'b@32', 16), ('extract_u16', a, 1)), ('shfr', b, a, 16), 'options->has_shfr32'),
   (('bitfield_select', 0xffffff00, ('ishl', 'b@32', 8), ('extract_u8', a, 3)), ('shfr', b, a, 24), 'options->has_shfr32'),

   (('ior', ('ishl', 'b@32', 24), ('ushr', a, 8)), ('shfr', b, a, 8), 'options->has_shfr32'),
   (('ior', ('ishl', 'b@32', 16), ('extract_u16', a, 1)), ('shfr', b, a, 16), 'options->has_shfr32'),
   (('ior', ('ishl', 'b@32', 8), ('extract_u8', a, 3)), ('shfr', b, a, 24), 'options->has_shfr32'),

   (('bcsel',
     ('ieq', c, 0),
     a,
     ('ior', ('ishl', 'b@32', ('iadd', 32, ('ineg', c))), ('ushr@32', a, c))),
    ('shfr', b, a, c),
    'options->has_shfr32', TestStatus.XFAIL),
   (('bcsel',
     ('ine', c, 0),
     ('ior', ('ishl', 'b@32', ('iadd', 32, ('ineg', c))), ('ushr@32', a, c)),
     a),
    ('shfr', b, a, c),
    'options->has_shfr32', TestStatus.XFAIL),

   (('ior', ('ishl', 'a@32', ('iadd', 32, ('ineg', b))), ('ushr@32', a, b)),
    ('shfr', a, a, b),
    'options->has_shfr32 && !options->has_rotate32'),

   (('bfi', 0xffffffff, a, b), a),
   (('bfi', 0x00000000, a, b), b),

   (('bfi', ('ineg', ('b2i', 'a@1')), b, c), ('bcsel', a, b, c)),

   (('bfi', a, 0, b), ('iand', ('inot', a), b)),
   (('bfi', '#a(is_odd)', b, b), b),
   (('bfi', '#a(is_odd)', a, b), ('ior', a, b)),
   (('bfi', '#a(is_odd)', b, 0), ('iand', a, b)),

   (('bfi', '#a(is_odd)', b, c), ('bitfield_select', a, b, c), 'options->has_bitfield_select'),

   (('u2f32', ('bfi', '#a(is_pos_power_of_two)', b, 0)),
    ('bcsel', ('ieq', ('iand', b, 1), 0), ('iand', b, 1), ('u2f', a))),
   (('u2f', ('bfi', '#a(is_pos_power_of_two)', b, 0)),
    ('bcsel', ('ieq', ('iand', b, 1), 0), 0, ('u2f', a))),

   (('fexp2(contract)', ('flog2', a)), ('fcanonicalize', a)),
   (('flog2(contract)', ('fexp2', a)), ('fcanonicalize', a)),
   (('fpow@32', a, b), ('fexp2', ('fmulz', ('flog2', a), b)), 'options->lower_fpow && ' + has_fmulz),
   (('fpow', a, b), ('fexp2', ('fmul', ('flog2', a), b)), 'options->lower_fpow'),
   (('fexp2(contract)', ('fmul', ('flog2', a), b)), ('fpow', a, b), '!options->lower_fpow'),

   (('~fexp2', ('fadd', ('fmul', ('flog2', a), b), ('fmul', ('flog2', c), d))),
    ('~fmul', ('fpow', a, b), ('fpow', c, d)),
    '!options->lower_fpow'),

   (('fexp2(contract)', ('fmul', ('flog2', a), 0.5)), ('fsqrt', a)),
   (('fexp2(contract)', ('fmul', ('flog2', a), 2.0)), ('fmul', a, a)),
   (('fexp2(contract)', ('fmul', ('flog2', a), 3.0)), ('fmul', ('fmul', a, a), a)),
   (('fexp2(contract)', ('fmul', ('flog2', a), 4.0)), ('fmul', ('fmul', a, a), ('fmul', a, a))),
   (('fexp2(contract)', ('fmul', ('flog2', a), 5.0)), ('fmul', ('fmul', ('fmul', a, a), ('fmul', a, a)), a)),
   (('fexp2(contract)', ('fmul', ('flog2', a), 6.0)), ('fmul', ('fmul', ('fmul', a, a), ('fmul', a, a)), ('fmul', a, a))),
   (('fexp2(contract)', ('fmul', ('flog2', a), 8.0)),
    ('fmul',
     ('fmul', ('fmul', a, a), ('fmul', a, a)),
     ('fmul', ('fmul', a, a), ('fmul', a, a)))),

   (('fpow(contract)', a, 1.0), ('fcanonicalize', a)),
   (('fpow(contract)', a, 2.0), ('fmul', a, a)),
   (('~fpow', a, 3.0), ('fmul', ('fmul', a, a), a)),
   (('~fpow', a, 4.0), ('fmul', ('fmul', a, a), ('fmul', a, a))),
   (('fpow(contract)', 2.0, a), ('fexp2', a)),

   (('~fpow', ('fpow', a, 2.2), 0.454545), ('fcanonicalize', a)),
   (('~fpow', ('fabs', ('fpow', a, 2.2)), 0.454545), ('fabs', a)),

   (('fsqrt(contract)', ('fexp2', a)), ('fexp2', ('fmul', 0.5, a))),
   (('frcp(contract)', ('fexp2', a)), ('fexp2', ('fneg', a))),
   (('frsq(contract)', ('fexp2', a)), ('fexp2', ('fmul', -0.5, a))),

   (('flog2(contract)', ('fsqrt', a)), ('fmul', 0.5, ('flog2', a))),
   (('flog2(contract)', ('frcp', a)), ('fneg', ('flog2', a))),
   (('flog2(contract)', ('frsq', a)), ('fmul', -0.5, ('flog2', a))),
   (('flog2(contract)', ('fpow', a, b)), ('fmul', b, ('flog2', a))),

   (('~fmul', ('fexp2(is_used_once)', a), ('fexp2(is_used_once)', b)), ('fexp2', ('fadd', a, b))),

   (('bcsel', ('flt', a, 0.0), 0.0, ('fsqrt(nnan,nsz)', a)), ('fsqrt', ('fmax', a, 0.0))),
   (('bcsel', ('fge', 0.0, a), 0.0, ('fsqrt(nnan)', a)), ('fsqrt', ('fmax', a, 0.0))),
   (('bcsel', ('flt', 0.0, a), ('fsqrt', a), 0.0), ('fsqrt', ('fmax', a, 0.0))),
   (('bcsel', ('fge', a, 0.0), ('fsqrt(nsz)', a), 0.0), ('fsqrt', ('fmax', a, 0.0))),

   (('fmul(contract)', ('fsqrt', a), ('fsqrt', a)), ('fabs', a)),
   (('fmulz(contract)', ('fsqrt', a), ('fsqrt', a)), ('fabs', a)),

   (('fdiv(contract)', 1.0, a), ('frcp', a)),
   (('fdiv', a, b), ('fmul', a, ('frcp', b)), 'options->lower_fdiv'),
   (('frcp(contract)', ('frcp', a)), ('fcanonicalize', a)),
   (('frcp(contract)', ('fsqrt', a)), ('frsq', a)),
   (('fsqrt', a), ('frcp', ('frsq', a)), 'options->lower_fsqrt'),
   (('frcp(contract)', ('frsq', a)), ('fsqrt', a), '!options->lower_fsqrt'),

   (('fsin', a), lowered_sincos(0.5), 'options->lower_sincos'),
   (('fcos', a), lowered_sincos(0.75), 'options->lower_sincos'),

   (('ieq', a, True), a),
   (('ine(is_not_used_by_if)', a, True), ('inot', a)),
   (('ine', a, False), a),
   (('ieq(is_not_used_by_if)', a, False), ('inot', 'a')),

   (('bcsel', a, True, False), a),
   (('bcsel', a, False, True), ('inot', a)),
   (('bcsel', True, b, c), b),
   (('bcsel', False, b, c), c),

   (('bcsel@16', a, 1.0, 0.0), ('b2f', a)),
   (('bcsel@16', a, 0.0, 1.0), ('b2f', ('inot', a))),
   (('bcsel@16', a, -1.0, -0.0), ('fneg', ('b2f', a))),
   (('bcsel@16', a, -0.0, -1.0), ('fneg', ('b2f', ('inot', a)))),

   (('bcsel@32', a, 1.0, 0.0), ('b2f', a)),
   (('bcsel@32', a, 0.0, 1.0), ('b2f', ('inot', a))),
   (('bcsel@32', a, -1.0, -0.0), ('fneg', ('b2f', a))),
   (('bcsel@32', a, -0.0, -1.0), ('fneg', ('b2f', ('inot', a)))),

   (('bcsel@64', a, 1.0, 0.0), ('b2f', a), '!(options->lower_doubles_options & nir_lower_fp64_full_software)'),
   (('bcsel@64', a, 0.0, 1.0), ('b2f', ('inot', a)), '!(options->lower_doubles_options & nir_lower_fp64_full_software)'),
   (('bcsel@64', a, -1.0, -0.0), ('fneg', ('b2f', a)), '!(options->lower_doubles_options & nir_lower_fp64_full_software)'),
   (('bcsel@64', a, -0.0, -1.0), ('fneg', ('b2f', ('inot', a))), '!(options->lower_doubles_options & nir_lower_fp64_full_software)'),

   (('bcsel', a, b, b), b),
   (('fcsel', a, b, b), ('fcanonicalize', b)),
])

optimizations.extend([
   (('imax', ('ineg', ('b2i', 'a@1')), ('ineg', ('b2i', 'b@1'))), ('ineg', ('b2i', ('iand', a, b)))),
   (('imin', ('ineg', ('b2i', 'a@1')), ('ineg', ('b2i', 'b@1'))), ('ineg', ('b2i', ('ior', a, b)))),
   (('umax', ('ineg', ('b2i', 'a@1')), ('ineg', ('b2i', 'b@1'))), ('ineg', ('b2i', ('ior', a, b)))),
   (('umin', ('ineg', ('b2i', 'a@1')), ('ineg', ('b2i', 'b@1'))), ('ineg', ('b2i', ('iand', a, b)))),
   (('umax', ('b2i', 'a@1'), ('b2i', 'b@1')), ('b2i', ('ior', a, b))),
   (('umin', ('b2i', 'a@1'), ('b2i', 'b@1')), ('b2i', ('iand', a, b))),

   (('iand', ('b2i', a), 1), ('b2i', a)),

   (('ine', ('umin', ('ineg', ('b2i', 'a@1')), b), 0), ('iand', a, ('ine', b, 0))),
   (('ine', ('umax', ('ineg', ('b2i', 'a@1')), b), 0), ('ior',  a, ('ine', b, 0))),
   (('ine', ('umin', ('b2i', 'a@1'), b), 0), ('iand', a, ('ine', b, 0))),
   (('ine', ('umax', ('b2i', 'a@1'), b), 0), ('ior',  a, ('ine', b, 0))),
   (('ieq', ('umin', ('ineg', ('b2i', 'a@1')), b), 0), ('ior',  ('inot', a), ('ieq', b, 0))),
   (('ieq', ('umax', ('ineg', ('b2i', 'a@1')), b), 0), ('iand', ('inot', a), ('ieq', b, 0))),
   (('ieq', ('umin', ('b2i', 'a@1'), b), 0), ('ior',  ('inot', a), ('ieq', b, 0))),
   (('ieq', ('umax', ('b2i', 'a@1'), b), 0), ('iand', ('inot', a), ('ieq', b, 0))),

   (('f2i', ('ftrunc', a)), ('f2i', a)),
   (('f2u', ('ftrunc', 'a(is_not_negative)')), ('f2u', a)),
   (('f2i', ('ffloor', 'a(is_not_negative)')), ('f2i', a)),
   (('f2u', ('ffloor', a)), ('f2u', a)),

   (('f2u', 'a(is_not_positive)'), 0),

   (('f2fmp', ('f2f32', 'a@16')), a, 'true', TestStatus.UNSUPPORTED),
   (('i2imp', ('i2i32', 'a@16')), a, 'true', TestStatus.UNSUPPORTED),
   (('i2imp', ('u2u32', 'a@16')), a, 'true', TestStatus.UNSUPPORTED),

   (('f2imp', ('f2f32', 'a@16')), ('f2i16', a), 'true', TestStatus.UNSUPPORTED),
   (('f2ump', ('f2f32', 'a@16')), ('f2u16', a), 'true', TestStatus.UNSUPPORTED),
   (('i2fmp', ('i2i32', 'a@16')), ('i2f16', a), 'true', TestStatus.UNSUPPORTED),
   (('u2fmp', ('u2u32', 'a@16')), ('u2f16', a), 'true', TestStatus.UNSUPPORTED),

   (('f2fmp', ('b2f32', 'a@1')), ('b2f16', a), 'true', TestStatus.UNSUPPORTED),
   (('i2imp', ('b2i32', 'a@1')), ('b2i16', a), 'true', TestStatus.UNSUPPORTED),
   (('i2imp', ('b2i32', 'a@1')), ('b2i16', a), 'true', TestStatus.UNSUPPORTED),

   (('f2imp', ('b2f32', 'a@1')), ('b2i16', a), 'true', TestStatus.UNSUPPORTED),
   (('f2ump', ('b2f32', 'a@1')), ('b2i16', a), 'true', TestStatus.UNSUPPORTED),
   (('i2fmp', ('b2i32', 'a@1')), ('b2f16', a), 'true', TestStatus.UNSUPPORTED),
   (('u2fmp', ('b2i32', 'a@1')), ('b2f16', a), 'true', TestStatus.UNSUPPORTED),

   (('f2f32', ('f2fmp', 'a@32')), a, 'true', TestStatus.UNSUPPORTED),
   (('i2i32', ('i2imp', 'a@32')), a, 'true', TestStatus.UNSUPPORTED),
   (('u2u32', ('i2imp', 'a@32')), a, 'true', TestStatus.UNSUPPORTED),

   (('i2i32', ('f2imp', 'a@32')), ('f2i32', a), 'true', TestStatus.UNSUPPORTED),
   (('u2u32', ('f2ump', 'a@32')), ('f2u32', a), 'true', TestStatus.UNSUPPORTED),
   (('f2f32', ('i2fmp', 'a@32')), ('i2f32', a), 'true', TestStatus.UNSUPPORTED),
   (('f2f32', ('u2fmp', 'a@32')), ('u2f32', a), 'true', TestStatus.UNSUPPORTED),

   (('f2i32', ('f2fmp', 'a@32')), ('f2i32', a), 'true', TestStatus.UNSUPPORTED),
   (('f2u32', ('f2fmp', 'a@32')), ('f2u32', a), 'true', TestStatus.UNSUPPORTED),
   (('i2f32', ('i2imp', 'a@32')), ('i2f32', a), 'true', TestStatus.UNSUPPORTED),

   (('ffloor', 'a(is_integral)'), a),
   (('fceil', 'a(is_integral)'), a),
   (('ftrunc', 'a(is_integral)'), a),
   (('fround_even', 'a(is_integral)'), a),

   (('~ffract', 'a(is_integral)'), 0.0),
   (('ffract', ('ffract', a)), ('ffract', a)),

   (('fabs', 'a(is_not_negative)'), ('fcanonicalize', a)),
   (('iabs', 'a(is_not_negative)'), a),
   (('fsat', 'a(is_not_positive)'), 0.0),

   (('~fmin', 'a(is_not_negative)', 1.0), ('fsat', a), '!options->lower_fsat'),
   (('fmin', 'a(is_a_number_not_negative)', 1.0), ('fsat', a), '!options->lower_fsat'),

   (('flt', ('fadd', ('fmul', ('fsat', a), ('fneg', ('fsat', a))), 1.0), 0.0), False),
   (('flt', ('fadd', ('fneg', ('fmul', ('fsat', a), ('fsat', a))), 1.0), 0.0), False),
   (('fmax', ('fadd', ('fmul', ('fsat', a), ('fneg', ('fsat', a))), 1.0), 0.0),
    ('fadd', ('fmul', ('fsat', a), ('fneg', ('fsat', a))), 1.0)),
   (('fmax', ('fadd', ('fneg', ('fmul', ('fsat', a), ('fsat', a))), 1.0), 0.0),
    ('fadd', ('fneg', ('fmul', ('fsat', a), ('fsat', a))), 1.0)),

   (('fneu', 'a(is_not_zero)', 0.0), True),
   (('feq',  'a(is_not_zero)', 0.0), False),

   (('fge', 'a(is_a_number_not_negative)', 'b(is_a_number_not_positive)'), True),
   (('fge', 'a(is_not_positive)',          'b(is_gt_zero)'),               False),
   (('fge', 'a(is_lt_zero)',               'b(is_not_negative)'),          False),

   (('flt', 'a(is_not_negative)',          'b(is_not_positive)'),          False),
   (('flt', 'a(is_a_number_not_positive)', 'b(is_a_number_gt_zero)'),      True),
   (('flt', 'a(is_a_number_lt_zero)',      'b(is_a_number_not_negative)'), True),

   (('ine', 'a(is_not_zero)', 0), True),
   (('ieq', 'a(is_not_zero)', 0), False),

   (('ige', 'a(is_not_negative)', 'b(is_not_positive)'), True),
   (('ige', 'a(is_not_positive)', 'b(is_gt_zero)'),      False),
   (('ige', 'a(is_lt_zero)',      'b(is_not_negative)'), False),

   (('ilt', 'a(is_not_negative)', 'b(is_not_positive)'), False),
   (('ilt', 'a(is_not_positive)', 'b(is_gt_zero)'),      True),
   (('ilt', 'a(is_lt_zero)',      'b(is_not_negative)'), True),

   (('ult', 0, 'a(is_gt_zero)'), True),
   (('ult', a, 0), False),
])

# pack/unpack identities
for pack, bits, compbits in (('pack_64_2x32', 64, 32), ('pack_32_2x16', 32, 16)):
    unpack = 'un' + pack
    optimizations += [
        ((unpack + '_split_x', (pack + '_split', a, b)), a),
        ((unpack + '_split_y', (pack + '_split', a, b)), b),
        ((unpack + '_split_x', (pack, a)), 'a.x'),
        ((unpack + '_split_y', (pack, a)), 'a.y'),
        ((unpack + '_split_x', ('u2u' + str(bits), 'a@' + str(compbits))), a),
        ((unpack + '_split_x', ('i2i' + str(bits), 'a@' + str(compbits))), a),
        ((unpack + '_split_y', ('i2i' + str(bits) + '(is_used_once)', 'a@' + str(compbits))), ('ishr', a, compbits - 1)),
        ((unpack, (pack + '_split', a, b)), ('vec2', a, b)),
        ((unpack, (pack, a)), a),
        ((pack + '_split', (unpack + '_split_x', a), (unpack + '_split_y', a)), a),
        ((pack + '_split', (unpack, a), (unpack + '.y', a)), a),
        ((pack, ('vec2', (unpack + '_split_x', a), (unpack + '_split_y', a))), a),
        ((pack, (unpack, a)), a),
    ]

optimizations.extend([
   (('unpack_64_2x32_split_y', ('u2u64', 'a@1')), 0),
   (('unpack_64_2x32_split_y', ('u2u64', 'a@8')), 0),
   (('unpack_64_2x32_split_y', ('u2u64', 'a@16')), 0),
   (('unpack_64_2x32_split_y', ('u2u64', 'a@32')), 0),

   (('unpack_double_2x32_dxil', ('pack_double_2x32_dxil', a)), a),
   (('pack_double_2x32_dxil', ('unpack_double_2x32_dxil', a)), a),

   (('unpack_64_4x16', ('pack_64_4x16', a)), a),
   (('pack_64_4x16', ('unpack_64_4x16', a)), a),
   (('unpack_32_4x8', ('pack_32_4x8', a)), a),
   (('pack_32_4x8', ('unpack_32_4x8', a)), a),

   (('unpack_64_4x16', ('pack_64_2x32', ('vec2', ('pack_32_2x16_split', a, b), ('pack_32_2x16_split', c, d)))), ('vec4', a, b, c, d)),
   (('unpack_64_4x16', ('pack_64_2x32_split', ('pack_32_2x16_split', a, b), ('pack_32_2x16_split', c, d))), ('vec4', a, b, c, d)),

   (('pack_64_2x32_split', ('pack_32_2x16_split', a, b), ('pack_32_2x16_split', c, d)),
    ('pack_64_4x16', ('vec4', a, b, c, d)), '!options->lower_pack_64_4x16'),
   (('pack_64_2x32', ('vec2', ('pack_32_2x16_split', a, b), ('pack_32_2x16_split', c, d))),
    ('pack_64_4x16', ('vec4', a, b, c, d)), '!options->lower_pack_64_4x16'),
   (('pack_64_2x32', ('vec2', ('pack_32_2x16', ('vec2', a, b)), ('pack_32_2x16', ('vec2', c, d)))),
    ('pack_64_4x16', ('vec4', a, b, c, d)), '!options->lower_pack_64_4x16'),

   (('iand',
     ('ieq', ('unpack_32_2x16_split_x', a), '#b'),
     ('ieq', ('unpack_32_2x16_split_y', a), '#c')),
    ('ieq', a, ('pack_32_2x16_split', b, c))),
])

optimizations.extend([
   (('ushr', 'a@16', 8), ('extract_u8', a, 1), '!options->lower_extract_byte'),
   (('ushr', 'a@32', 24), ('extract_u8', a, 3), '!options->lower_extract_byte'),
   (('ushr', 'a@64', 56), ('extract_u8', a, 7), '!options->lower_extract_byte'),
   (('ishr', 'a@16', 8), ('extract_i8', a, 1), '!options->lower_extract_byte'),
   (('ishr', 'a@32', 24), ('extract_i8', a, 3), '!options->lower_extract_byte'),
   (('ishr', 'a@64', 56), ('extract_i8', a, 7), '!options->lower_extract_byte'),

   (('iand', 0xff, a), ('extract_u8', a, 0), '!options->lower_extract_byte'),
   (('ishr', ('iand', 'a@32', 0x0000ff00), 8), ('extract_u8', a, 1), '!options->lower_extract_byte'),
   (('ishr', ('iand', 'a@64', 0x0000ff00), 8), ('extract_u8', a, 1), '!options->lower_extract_byte'),
   (('ishr', ('iand', a, 0x00ff0000), 16), ('extract_u8', a, 2), '!options->lower_extract_byte'),

   (('u2u8', ('extract_u16', a, 1)), ('u2u8', ('extract_u8', a, 2)), '!options->lower_extract_byte'),
   (('u2u8', ('ushr', a, 8)), ('u2u8', ('extract_u8', a, 1)), '!options->lower_extract_byte'),

   (('i2i16', ('u2u8', ('extract_u8', a, b))), ('i2i16', ('extract_i8', a, b))),
   (('u2u16', ('u2u8', ('extract_u8', a, b))), ('u2u16', ('extract_u8', a, b))),

   (('ubfe', a, 0, 8), ('extract_u8', a, 0), '!options->lower_extract_byte'),
   (('ubfe', a, 8, 8), ('extract_u8', a, 1), '!options->lower_extract_byte'),
   (('ubfe', a, 16, 8), ('extract_u8', a, 2), '!options->lower_extract_byte'),
   (('ubfe', a, 24, 8), ('extract_u8', a, 3), '!options->lower_extract_byte'),
   (('ibfe', a, 0, 8), ('extract_i8', a, 0), '!options->lower_extract_byte'),
   (('ibfe', a, 8, 8), ('extract_i8', a, 1), '!options->lower_extract_byte'),
   (('ibfe', a, 16, 8), ('extract_i8', a, 2), '!options->lower_extract_byte'),
   (('ibfe', a, 24, 8), ('extract_i8', a, 3), '!options->lower_extract_byte'),

   (('extract_u8', ('extract_i8', a, b), 0), ('extract_u8', a, b)),
   (('extract_u8', ('extract_u8', a, b), 0), ('extract_u8', a, b)),

   (('extract_i8', ('iand', a, 0x0000ff00), 1), ('extract_i8', a, 1)),
   (('extract_i8', ('iand', a, 0x00ff0000), 2), ('extract_i8', a, 2)),
   (('extract_i8', ('iand', a, 0xff000000), 3), ('extract_i8', a, 3)),

   (('extract_u8', ('iand', a, 0x0000ff00), 1), ('extract_u8', a, 1)),
   (('extract_u8', ('iand', a, 0x00ff0000), 2), ('extract_u8', a, 2)),
   (('extract_u8', ('iand', a, 0xff000000), 3), ('extract_u8', a, 3)),
])

optimizations.extend([
   (('ior', ('bcsel', ('ieq', ('iand', a, 0x00000080), 0), 0, ~0xff), ('extract_u8', a, 0)), ('extract_i8', a, 0)),
   (('ior', ('bcsel', ('ieq', ('iand', a, 0x00008000), 0), 0, ~0xff), ('extract_u8', a, 1)), ('extract_i8', a, 1)),
   (('ior', ('bcsel', ('ieq', ('iand', a, 0x00800000), 0), 0, ~0xff), ('extract_u8', a, 2)), ('extract_i8', a, 2)),
   (('ior', ('bcsel', ('ige',          'a@32',         0), 0, ~0xff), ('extract_u8', a, 3)), ('extract_i8', a, 3)),
   (('ior', ('bcsel', ('ine', ('iand', a, 0x00000080), 0), ~0xff, 0), ('extract_u8', a, 0)), ('extract_i8', a, 0)),
   (('ior', ('bcsel', ('ine', ('iand', a, 0x00008000), 0), ~0xff, 0), ('extract_u8', a, 1)), ('extract_i8', a, 1)),
   (('ior', ('bcsel', ('ine', ('iand', a, 0x00800000), 0), ~0xff, 0), ('extract_u8', a, 2)), ('extract_i8', a, 2)),
   (('ior', ('bcsel', ('ilt',          'a@32',         0), ~0xff, 0), ('extract_u8', a, 3)), ('extract_i8', a, 3)),

   (('extract_i8', ('ushr', a, 8), 0), ('extract_i8', a, 1)),
   (('extract_i8', ('ushr', a, 8), 1), ('extract_i8', a, 2)),
   (('extract_i8', ('ushr', a, 8), 2), ('extract_i8', a, 3)),
   (('extract_u8', ('ushr', a, 8), 0), ('extract_u8', a, 1)),
   (('extract_u8', ('ushr', a, 8), 1), ('extract_u8', a, 2)),
   (('extract_u8', ('ushr', a, 8), 2), ('extract_u8', a, 3)),
   (('extract_u8', ('ushr', 'a@32', 16), 0), ('extract_u8', a, 2)),
   (('extract_u8', ('ushr', 'a@32', 16), 1), ('extract_u8', a, 3)),
   (('extract_u16', ('ushr', 'a@64', 32), 0), ('extract_u16', a, 2)),
   (('extract_u16', ('ushr', 'a@64', 32), 1), ('extract_u16', a, 3)),

   (('extract_i8', ('extract_i16', a, 1), 0), ('extract_i8', a, 2)),
   (('extract_i8', ('extract_i16', a, 1), 1), ('extract_i8', a, 3)),
   (('extract_i8', ('extract_u16', a, 1), 0), ('extract_i8', a, 2)),
   (('extract_i8', ('extract_u16', a, 1), 1), ('extract_i8', a, 3)),
   (('extract_u8', ('extract_i16', a, 1), 0), ('extract_u8', a, 2)),
   (('extract_u8', ('extract_i16', a, 1), 1), ('extract_u8', a, 3)),
   (('extract_u8', ('extract_u16', a, 1), 0), ('extract_u8', a, 2)),
   (('extract_u8', ('extract_u16', a, 1), 1), ('extract_u8', a, 3)),

   (('iand', ('extract_u8', a, 0), '#b'), ('iand', a, ('iand', b, 0x00ff))),
   (('iand', ('extract_u16', a, 0), '#b'), ('iand', a, ('iand', b, 0xffff))),

   (('ieq', ('iand', ('extract_u8', a, '#b'), '#c'), 0),
    ('ieq', ('iand', a, ('ishl', ('iand', c, 0x00ff), ('imul', ('i2i32', b), 8))), 0)),
   (('ine', ('iand', ('extract_u8', a, '#b'), '#c'), 0),
    ('ine', ('iand', a, ('ishl', ('iand', c, 0x00ff), ('imul', ('i2i32', b), 8))), 0)),
   (('ieq', ('iand', ('extract_u16(is_used_once)', a, '#b'), '#c'), 0),
    ('ieq', ('iand', a, ('ishl', ('iand', c, 0xffff), ('imul', ('i2i32', b), 16))), 0)),
   (('ine', ('iand', ('extract_u16(is_used_once)', a, '#b'), '#c'), 0),
    ('ine', ('iand', a, ('ishl', ('iand', c, 0xffff), ('imul', ('i2i32', b), 16))), 0)),

   (('ushr', ('ishl', 'a@32', 16), 16), ('extract_u16', a, 0), '!options->lower_extract_word'),
   (('ushr', 'a@32', 16), ('extract_u16', a, 1), '!options->lower_extract_word'),
   (('ishr', ('ishl', 'a@32', 16), 16), ('extract_i16', a, 0), '!options->lower_extract_word'),
   (('ishr', 'a@32', 16), ('extract_i16', a, 1), '!options->lower_extract_word'),
   (('iand', 0xffff, a), ('extract_u16', a, 0), '!options->lower_extract_word'),

   (('ubfe', a, 0, 16), ('extract_u16', a, 0), '!options->lower_extract_word'),
   (('ubfe', a, 16, 16), ('extract_u16', a, 1), '!options->lower_extract_word'),
   (('ibfe', a, 0, 16), ('extract_i16', a, 0), '!options->lower_extract_word'),
   (('ibfe', a, 16, 16), ('extract_i16', a, 1), '!options->lower_extract_word'),

   (('ior',
     ('ishl', ('u2u32', 'a@8'), 24),
     ('ior',
      ('ishl', ('u2u32', 'b@8'), 16),
      ('ior',
       ('ishl', ('u2u32', 'c@8'), 8),
       ('u2u32', 'd@8')))),
    ('pack_32_4x8', ('vec4', d, c, b, a)),
    'options->has_pack_32_4x8'),

   (('ior', ('ishl', a, 16), ('u2u32', 'b@16')),
    ('pack_32_2x16_split', b, ('u2u16', a)),
    '!options->lower_pack_32_2x16_split && !options->lower_pack_split'),

   (('ior', ('u2u16', ('unpack_32_4x8', a)), ('ishl', ('u2u16', ('unpack_32_4x8.y', a)), 8)),
    ('unpack_32_2x16_split_x', a),
    '!options->lower_unpack_32_2x16_split'),
   (('ior', ('u2u16', ('unpack_32_4x8.z', a)), ('ishl', ('u2u16', ('unpack_32_4x8.w', a)), 8)),
    ('unpack_32_2x16_split_y', a),
    '!options->lower_unpack_32_2x16_split'),

   (('i2i16', ('unpack_32_4x8(xz_components_unused).y', a)),
    ('extract_i8', ('unpack_32_2x16.x', a), 1),
    '!options->lower_extract_byte'),
   (('i2i16', ('unpack_32_4x8(xz_components_unused).w', a)),
    ('extract_i8', ('unpack_32_2x16.y', a), 1),
    '!options->lower_extract_byte'),

   (('extract_u16', ('extract_i16', a, b), 0), ('extract_u16', a, b)),
   (('extract_u16', ('extract_u16', a, b), 0), ('extract_u16', a, b)),

   (('extract_i16', ('iand', a, 0x00ff0000), 1), ('extract_u8', a, 2), '!options->lower_extract_byte'),
   (('extract_u16', ('iand', a, 0x00ff0000), 1), ('extract_u8', a, 2), '!options->lower_extract_byte'),

   (('pack_64_2x32_split', a, b),
    ('ior', ('u2u64', a), ('ishl', ('u2u64', b), 32)),
    'options->lower_pack_64_2x32_split'),
   (('pack_32_2x16_split', a, b),
    ('ior', ('u2u32', a), ('ishl', ('u2u32', b), 16)),
    'options->lower_pack_32_2x16_split || options->lower_pack_split'),
   (('pack_half_2x16_split', a, b), ('pack_half_2x16_rtz_split', a, b), 'options->has_pack_half_2x16_rtz'),

   (('unpack_64_2x32_split_x', a), ('u2u32', a), 'options->lower_unpack_64_2x32_split'),
   (('unpack_64_2x32_split_y', a), ('u2u32', ('ushr', a, 32)), 'options->lower_unpack_64_2x32_split'),
   (('unpack_32_2x16_split_x', a), ('u2u16', a), 'options->lower_unpack_32_2x16_split || options->lower_pack_split'),
   (('unpack_32_2x16_split_y', a), ('u2u16', ('ushr', a, 16)), 'options->lower_unpack_32_2x16_split || options->lower_pack_split'),

   (('unpack_64_2x32_split_x', ('ushr', a, 32)), ('unpack_64_2x32_split_y', a), '!options->lower_unpack_64_2x32_split'),
   (('u2u32', ('ushr', 'a@64', 32)), ('unpack_64_2x32_split_y', a), '!options->lower_unpack_64_2x32_split'),

   (('unpack_half_2x16_split_x', ('iand', a, 0xffff)), ('unpack_half_2x16_split_x', a)),
   (('unpack_32_2x16_split_x', ('iand', a, 0xffff)), ('unpack_32_2x16_split_x', a)),
   (('unpack_64_2x32_split_x', ('iand', a, 0xffffffff)), ('unpack_64_2x32_split_x', a)),
   (('unpack_half_2x16_split_y', ('iand', a, 0xffff0000)), ('unpack_half_2x16_split_y', a)),
   (('unpack_32_2x16_split_y', ('iand', a, 0xffff0000)), ('unpack_32_2x16_split_y', a)),
   (('unpack_64_2x32_split_y', ('iand', a, 0xffffffff00000000)), ('unpack_64_2x32_split_y', a)),

   (('unpack_half_2x16_split_x', ('extract_u16', a, 0)), ('unpack_half_2x16_split_x', a)),
   (('unpack_half_2x16_split_x', ('extract_u16', a, 1)), ('unpack_half_2x16_split_y', a)),
   (('unpack_half_2x16_split_x', ('ushr', a, 16)), ('unpack_half_2x16_split_y', a)),
   (('unpack_32_2x16_split_x', ('extract_u16', a, 0)), ('unpack_32_2x16_split_x', a)),
   (('unpack_32_2x16_split_x', ('extract_u16', a, 1)), ('unpack_32_2x16_split_y', a)),

   (('ishl', ('pack_half_2x16', ('vec2', a, 0)), 16), ('pack_half_2x16', ('vec2', 0, a))),
   (('ushr', ('pack_half_2x16', ('vec2', 0, a)), 16), ('pack_half_2x16', ('vec2', a, 0))),

   (('iadd', ('pack_half_2x16', ('vec2', a, 0)), ('pack_half_2x16', ('vec2', 0, b))),
    ('pack_half_2x16', ('vec2', a, b))),
   (('ior', ('pack_half_2x16', ('vec2', a, 0)), ('pack_half_2x16', ('vec2', 0, b))),
    ('pack_half_2x16', ('vec2', a, b))),

   (('ishl', ('pack_half_2x16_split', a, 0), 16), ('pack_half_2x16_split', 0, a)),
   (('ushr', ('pack_half_2x16_split', 0, a), 16), ('pack_half_2x16_split', a, 0)),
   (('extract_u16', ('pack_half_2x16_split', 0, a), 1), ('pack_half_2x16_split', a, 0)),

   (('ishl', ('pack_half_2x16_rtz_split', a, 0), 16), ('pack_half_2x16_rtz_split', 0, a)),
   (('ushr', ('pack_half_2x16_rtz_split', 0, a), 16), ('pack_half_2x16_rtz_split', a, 0)),
   (('extract_u16', ('pack_half_2x16_rtz_split', 0, a), 1), ('pack_half_2x16_rtz_split', a, 0)),

   (('iadd', ('pack_half_2x16_split', a, 0), ('pack_half_2x16_split', 0, b)), ('pack_half_2x16_split', a, b)),
   (('ior',  ('pack_half_2x16_split', a, 0), ('pack_half_2x16_split', 0, b)), ('pack_half_2x16_split', a, b)),

   (('iadd', ('pack_half_2x16_rtz_split', a, 0), ('pack_half_2x16_rtz_split', 0, b)), ('pack_half_2x16_rtz_split', a, b)),
   (('ior',  ('pack_half_2x16_rtz_split', a, 0), ('pack_half_2x16_rtz_split', 0, b)), ('pack_half_2x16_rtz_split', a, b)),

   (('pack_uint_2x16', ('vec2', ('pack_half_2x16_rtz_split', a, 0), ('pack_half_2x16_rtz_split', b, 0))),
    ('pack_half_2x16_rtz_split', a, b)),

   (('bfi', 0xffff0000, ('pack_half_2x16_split', a, b), ('pack_half_2x16_split', c, d)),
    ('pack_half_2x16_split', c, a)),

   (('iand', ('bfi', 0x0000000f, '#a', b), 0xfffffffc),
    ('bfi', 0x0000000f, ('iand', a, 0xfffffffc), b)),
   (('iand', ('bfi', 0x00000007, '#a', b), 0xfffffffc),
    ('bfi', 0x00000007, ('iand', a, 0xfffffffc), b)),

   (('umin', ('ishl', ('iand', a, 0xf), 3), 0x78), ('ishl', ('iand', a, 0xf), 3)),

   (('extract_i8', ('pack_32_4x8_split', a, b, c, d), 0), ('i2i', a)),
   (('extract_i8', ('pack_32_4x8_split', a, b, c, d), 1), ('i2i', b)),
   (('extract_i8', ('pack_32_4x8_split', a, b, c, d), 2), ('i2i', c)),
   (('extract_i8', ('pack_32_4x8_split', a, b, c, d), 3), ('i2i', d)),
   (('extract_u8', ('pack_32_4x8_split', a, b, c, d), 0), ('u2u', a)),
   (('extract_u8', ('pack_32_4x8_split', a, b, c, d), 1), ('u2u', b)),
   (('extract_u8', ('pack_32_4x8_split', a, b, c, d), 2), ('u2u', c)),
   (('extract_u8', ('pack_32_4x8_split', a, b, c, d), 3), ('u2u', d)),

   (('u2u32', ('iadd(is_used_once)', 'a@64', b)), ('iadd', ('u2u32', a), ('u2u32', b))),
   (('u2u32', ('imul(is_used_once)', 'a@64', b)), ('imul', ('u2u32', a), ('u2u32', b))),

   (('u2f32', ('u2u64', 'a@32')), ('u2f32', a)),

   (('ult', 0xffffffff, 'a@64'), ('ine', ('unpack_64_2x32_split_y', a), 0)),

   (('i2i16', ('u2u8', ('iand', 'a@16', 1))), ('iand', 'a@16', 1)),
   (('u2u16', ('u2u8', ('iand', 'a@16', 1))), ('iand', 'a@16', 1)),

   (('u2u32', ('ushr', ('ior', ('ishl', a, 32), ('u2u64', 'b@8')), 32)), ('u2u32', a)),
   (('u2u32', ('ushr', ('ior', ('ishl', a, 32), ('u2u64', 'b@16')), 32)), ('u2u32', a)),
   (('u2u32', ('ushr', ('ior', ('ishl', a, 32), ('u2u64', 'b@32')), 32)), ('u2u32', a)),
   (('u2u16', ('ushr', ('ior', ('ishl', a, 16), ('u2u32', 'b@8')), 16)), ('u2u16', a)),
   (('u2u16', ('ushr', ('ior', ('ishl', a, 16), ('u2u32', 'b@16')), 16)), ('u2u16', a)),
])

for op in ('ushr', 'ishr'):
    optimizations.extend([
        (('extract_u8', (op, 'a@16', 8), 0), ('extract_u8', a, 1)),
    ])
    optimizations.extend([
        (('extract_u8', (op, 'a@32', 8 * i), 0), ('extract_u8', a, i)) for i in range(1, 4)
    ])
    optimizations.extend([
        (('extract_u8', (op, 'a@64', 8 * i), 0), ('extract_u8', a, i)) for i in range(1, 8)
    ])

optimizations.extend([
    (('extract_u8', ('extract_u16', a, 1), 0), ('extract_u8', a, 2)),
])

for op in ('extract_u8', 'extract_i8'):
    optimizations.extend([
        ((op, ('ishl', 'a@16', 8), 1), (op, a, 0)),
    ])
    optimizations.extend([
        ((op, ('ishl', 'a@32', 24 - 8 * i), 3), (op, a, i)) for i in range(2, -1, -1)
    ])
    optimizations.extend([
        ((op, ('ishl', 'a@64', 56 - 8 * i), 7), (op, a, i)) for i in range(6, -1, -1)
    ])

for op, repl in (('ieq', 'ieq'), ('ine', 'ine'),
                 ('ult', 'ult'), ('ilt', 'ult'),
                 ('uge', 'uge'), ('ige', 'uge')):
    optimizations.extend([
        ((op, ('pack_64_2x32_split', a, 0), ('pack_64_2x32_split', b, 0)), (repl, a, b)),
        ((op, ('pack_64_2x32_split', a, 0), '#b(is_upper_half_zero)'),
         (repl, a, ('unpack_64_2x32_split_x', b))),
        ((op, '#a(is_upper_half_zero)', ('pack_64_2x32_split', b, 0)),
         (repl, ('unpack_64_2x32_split_x', a), b)),

        ((op, ('pack_64_2x32_split', 0, a), ('pack_64_2x32_split', 0, b)), (op, a, b)),
        ((op, ('pack_64_2x32_split', 0, a), '#b(is_lower_half_zero)'),
         (op, a, ('unpack_64_2x32_split_y', b))),
        ((op, '#a(is_lower_half_zero)', ('pack_64_2x32_split', 0, b)),
         (op, ('unpack_64_2x32_split_y', a), b)),
    ])

optimizations.extend([
   (('ussub_4x8_vc4', a, 0), a),
   (('ussub_4x8_vc4', a, ~0), 0),

   (('fsub', a, b), ('fadd', a, ('fneg', b))),
   (('isub', a, b), ('iadd', a, ('ineg', b))),

   (('uabs_usub', a, b), ('bcsel', ('ult', a, b), ('ineg', ('isub', a, b)), ('isub', a, b))),
   (('uabs_isub', a, b), ('bcsel', ('ilt', a, b), ('ineg', ('isub', a, b)), ('isub', a, b))),

   (('bitz', a, b), ('inot', ('bitnz', a, b))),

   (('fmul(is_used_by_non_fsat)', ('fneg', a), b), ('fneg', ('fmul', a, b))),
   (('fmulz(is_used_by_non_fsat,nsz)', ('fneg', a), b), ('fneg', ('fmulz', a, b))),
   (('ffma', ('fneg', a), ('fneg', b), c), ('ffma', a, b, c)),
   (('ffmaz', ('fneg', a), ('fneg', b), c), ('ffmaz', a, b, c)),
   (('imul', ('ineg', a), b), ('ineg', ('imul', a, b))),

   (('~fmul(is_used_once)', ('fmul(is_used_once)', 'a(is_not_const)', 'b(is_not_const)'), '#c'),
    ('fmul', ('fmul', a, c), b)),
   (('~fmulz(is_used_once)', ('fmulz(is_used_once)', 'a(is_not_const)', 'b(is_not_const)'), '#c'),
    ('fmulz', ('fmulz', a, c), b)),
   (('~fmul(is_used_once)', ('fmulz(is_used_once)', 'a(is_not_const)', 'b(is_not_const)'), '#c(is_finite_not_zero)'),
    ('fmulz', ('fmul', a, c), b)),
   (('imul(is_used_once)', ('imul(is_used_once)', 'a(is_not_const)', 'b(is_not_const)'), '#c'),
    ('imul', ('imul', a, c), b)),
   (('~ffma', ('fmul(is_used_once)', 'a(is_not_const)', 'b(is_not_const)'), '#c', d),
    ('ffma', ('fmul', a, c), b, d)),
   (('~ffmaz', ('fmulz(is_used_once)', 'a(is_not_const)', 'b(is_not_const)'), '#c', d),
    ('ffmaz', ('fmulz', a, c), b, d)),
   (('~ffma', ('fmulz(is_used_once)', 'a(is_not_const)', 'b(is_not_const)'), '#c(is_finite_not_zero)', d),
    ('ffmaz', ('fmul', a, c), b, d)),

   (('~fadd(is_used_once)', ('fadd(is_used_once)', 'a(is_not_const)', 'b(is_fmul)'), '#c'),
    ('fadd', ('fadd', a, c), b)),
   (('~fadd(is_used_once)', ('fadd(is_used_once)', 'a(is_not_const)', 'b(is_not_const)'), '#c'),
    ('fadd', ('fadd', a, c), b)),
   (('~fadd(is_used_once)', ('ffma(is_used_once)', 'a(is_not_const)', b, 'c(is_not_const)'), '#d'),
    ('fadd', ('ffma', a, b, d), c)),
   (('~fadd(is_used_once)', ('ffmaz(is_used_once)', 'a(is_not_const)', b, 'c(is_not_const)'), '#d'),
    ('fadd', ('ffmaz', a, b, d), c)),
   (('iadd(is_used_once)', ('iadd(is_used_once)', 'a(is_not_const)', 'b(is_not_const)'), '#c'),
    ('iadd', ('iadd', a, c), b)),

   (('~fmul', '#a', ('fmul', 'b(is_not_const)', '#c')), ('fmul', ('fmul', a, c), b)),
   (('~fmulz', '#a', ('fmulz', 'b(is_not_const)', '#c')), ('fmulz', ('fmulz', a, c), b)),
   (('~fmul', '#a(is_finite_not_zero)', ('fmulz', 'b(is_not_const)', '#c')), ('fmulz', ('fmul', a, c), b)),
   (('~ffma', '#a', ('fmul', 'b(is_not_const)', '#c'), d), ('ffma', ('fmul', a, c), b, d)),
   (('~ffmaz', '#a', ('fmulz', 'b(is_not_const)', '#c'), d), ('ffmaz', ('fmulz', a, c), b, d)),
   (('~ffmaz', '#a(is_finite_not_zero)', ('fmulz', 'b(is_not_const)', '#c'), d), ('ffmaz', ('fmul', a, c), b, d)),
   (('imul', '#a', ('imul', 'b(is_not_const)', '#c')), ('imul', ('imul', a, c), b)),

   (('~fadd', '#a',          ('fadd', 'b(is_not_const)', '#c')),  ('fadd', ('fadd', a,          c),           b)),
   (('~fadd', '#a', ('fneg', ('fadd', 'b(is_not_const)', '#c'))), ('fadd', ('fadd', a, ('fneg', c)), ('fneg', b))),
   (('~fadd', '#a',          ('ffma', 'b(is_not_const)', 'c(is_not_const)', '#d')),  ('ffma',          b,  c, ('fadd', a,          d))),
   (('~fadd', '#a', ('fneg', ('ffma', 'b(is_not_const)', 'c(is_not_const)', '#d'))), ('ffma', ('fneg', b), c, ('fadd', a, ('fneg', d)))),
   (('~fadd', '#a',          ('ffmaz', 'b(is_not_const)', 'c(is_not_const)', '#d')),  ('ffmaz',          b,  c, ('fadd', a,          d))),
   (('~fadd', '#a', ('fneg', ('ffmaz', 'b(is_not_const)', 'c(is_not_const)', '#d'))), ('ffmaz', ('fneg', b), c, ('fadd', a, ('fneg', d)))),

   (('iadd', '#a', ('iadd', 'b(is_not_const)', '#c')), ('iadd', ('iadd', a, c), b)),
   (('iand', '#a', ('iand', 'b(is_not_const)', '#c')), ('iand', ('iand', a, c), b)),
   (('ior',  '#a', ('ior',  'b(is_not_const)', '#c')), ('ior',  ('ior',  a, c), b)),
   (('ixor', '#a', ('ixor', 'b(is_not_const)', '#c')), ('ixor', ('ixor', a, c), b)),

   (('ior', ('iand', a, '#c'), ('ior', b, ('iand', a, '#d'))), ('ior', b, ('iand', a, ('ior', c, d)))),

   (('~fadd', ('fadd(is_used_once)', 'a(is_fmul)', 'b(is_fmul)'), 'c(is_not_fmul)'), ('fadd', ('fadd', a, c), b)),

   (('idiv', ('imul(no_signed_wrap)', a, b), b), a, 'true', TestStatus.UNSUPPORTED),

   (('bcsel', ('ige', ('find_lsb', a), 0), ('find_lsb', a), -1), ('find_lsb', a)),
   (('bcsel', ('ige', ('ifind_msb', a), 0), ('ifind_msb', a), -1), ('ifind_msb', a)),
   (('bcsel', ('ige', ('ufind_msb', a), 0), ('ufind_msb', a), -1), ('ufind_msb', a)),
   (('bcsel', ('ige', ('ifind_msb_rev', a), 0), ('ifind_msb_rev', a), -1), ('ifind_msb_rev', a)),
   (('bcsel', ('ige', ('ufind_msb_rev', a), 0), ('ufind_msb_rev', a), -1), ('ufind_msb_rev', a)),

   (('bcsel', ('ine', a, 0), ('find_lsb', a), -1), ('find_lsb', a)),
   (('bcsel', ('ine', a, 0), ('ifind_msb', a), -1), ('ifind_msb', a)),
   (('bcsel', ('ine', a, 0), ('ufind_msb', a), -1), ('ufind_msb', a)),
   (('bcsel', ('ine', a, 0), ('ifind_msb_rev', a), -1), ('ifind_msb_rev', a)),
   (('bcsel', ('ine', a, 0), ('ufind_msb_rev', a), -1), ('ufind_msb_rev', a)),

   (('bcsel', ('ine', a, -1), ('ifind_msb', a), -1), ('ifind_msb', a)),
   (('bcsel', ('ine', a, -1), ('ifind_msb_rev', a), -1), ('ifind_msb_rev', a)),

   (('bcsel', ('ine', ('ifind_msb', 'a@32'), -1), ('iadd', 31, ('ineg', ('ifind_msb', a))), -1), ('ifind_msb_rev', a), 'options->has_find_msb_rev'),
   (('bcsel', ('ine', ('ufind_msb', 'a@32'), -1), ('iadd', 31, ('ineg', ('ufind_msb', a))), -1), ('ufind_msb_rev', a), 'options->has_find_msb_rev'),
   (('bcsel', ('ieq', ('ifind_msb', 'a@32'), -1), -1, ('iadd', 31, ('ineg', ('ifind_msb', a)))), ('ifind_msb_rev', a), 'options->has_find_msb_rev'),
   (('bcsel', ('ieq', ('ufind_msb', 'a@32'), -1), -1, ('iadd', 31, ('ineg', ('ufind_msb', a)))), ('ufind_msb_rev', a), 'options->has_find_msb_rev'),
   (('bcsel', ('ine', ('ifind_msb', 'a@32'), -1), ('iadd', 31, ('ineg', ('ifind_msb', a))), ('ifind_msb', a)), ('ifind_msb_rev', a), 'options->has_find_msb_rev'),
   (('bcsel', ('ine', ('ufind_msb', 'a@32'), -1), ('iadd', 31, ('ineg', ('ufind_msb', a))), ('ufind_msb', a)), ('ufind_msb_rev', a), 'options->has_find_msb_rev'),
   (('bcsel', ('ieq', ('ifind_msb', 'a@32'), -1), ('ifind_msb', a), ('iadd', 31, ('ineg', ('ifind_msb', a)))), ('ifind_msb_rev', a), 'options->has_find_msb_rev'),
   (('bcsel', ('ieq', ('ufind_msb', 'a@32'), -1), ('ufind_msb', a), ('iadd', 31, ('ineg', ('ufind_msb', a)))), ('ufind_msb_rev', a), 'options->has_find_msb_rev'),
   (('bcsel', ('ine', 'a@32', 0), ('iadd', 31, ('ineg', ('ufind_msb', a))), -1), ('ufind_msb_rev', a), 'options->has_find_msb_rev'),
   (('bcsel', ('ieq', 'a@32', 0), -1, ('iadd', 31, ('ineg', ('ufind_msb', a)))), ('ufind_msb_rev', a), 'options->has_find_msb_rev'),
   (('bcsel', ('ine', 'a@32', 0), ('iadd', 31, ('ineg', ('ufind_msb', a))), ('ufind_msb', a)), ('ufind_msb_rev', a), 'options->has_find_msb_rev'),
   (('bcsel', ('ieq', 'a@32', 0), ('ufind_msb', a), ('iadd', 31, ('ineg', ('ufind_msb', a)))), ('ufind_msb_rev', a), 'options->has_find_msb_rev'),

   (('bcsel', ('ine', ('ifind_msb_rev', 'a@32'), -1), ('iadd', 31, ('ineg', ('ifind_msb_rev', a))), -1), ('ifind_msb', a), '!options->lower_ifind_msb'),
   (('bcsel', ('ine', ('ufind_msb_rev', 'a@32'), -1), ('iadd', 31, ('ineg', ('ufind_msb_rev', a))), -1), ('ufind_msb', a), '!options->lower_ufind_msb'),
   (('bcsel', ('ieq', ('ifind_msb_rev', 'a@32'), -1), -1, ('iadd', 31, ('ineg', ('ifind_msb_rev', a)))), ('ifind_msb', a), '!options->lower_ifind_msb'),
   (('bcsel', ('ieq', ('ufind_msb_rev', 'a@32'), -1), -1, ('iadd', 31, ('ineg', ('ufind_msb_rev', a)))), ('ufind_msb', a), '!options->lower_ufind_msb'),
   (('bcsel', ('ine', ('ifind_msb_rev', 'a@32'), -1), ('iadd', 31, ('ineg', ('ifind_msb_rev', a))), ('ifind_msb_rev', a)), ('ifind_msb', a), '!options->lower_ifind_msb'),
   (('bcsel', ('ine', ('ufind_msb_rev', 'a@32'), -1), ('iadd', 31, ('ineg', ('ufind_msb_rev', a))), ('ufind_msb_rev', a)), ('ufind_msb', a), '!options->lower_ufind_msb'),
   (('bcsel', ('ieq', ('ifind_msb_rev', 'a@32'), -1), ('ifind_msb_rev', a), ('iadd', 31, ('ineg', ('ifind_msb_rev', a)))), ('ifind_msb', a), '!options->lower_ifind_msb'),
   (('bcsel', ('ieq', ('ufind_msb_rev', 'a@32'), -1), ('ufind_msb_rev', a), ('iadd', 31, ('ineg', ('ufind_msb_rev', a)))), ('ufind_msb', a), '!options->lower_ufind_msb'),
   (('bcsel', ('ine', 'a@32', 0), ('iadd', 31, ('ineg', ('ufind_msb_rev', a))), -1), ('ufind_msb', a), '!options->lower_ufind_msb'),
   (('bcsel', ('ieq', 'a@32', 0), -1, ('iadd', 31, ('ineg', ('ufind_msb_rev', a)))), ('ufind_msb', a), '!options->lower_ufind_msb'),
   (('bcsel', ('ine', 'a@32', 0), ('iadd', 31, ('ineg', ('ufind_msb_rev', a))), ('ufind_msb_rev', a)), ('ufind_msb', a), '!options->lower_ufind_msb'),
   (('bcsel', ('ieq', 'a@32', 0), ('ufind_msb_rev', a), ('iadd', 31, ('ineg', ('ufind_msb_rev', a)))), ('ufind_msb', a), '!options->lower_ufind_msb'),

   (('iand', a, ('inot', ('ishl', 1, ('find_lsb', a)))), ('iand', a, ('inot', ('ineg', a)))),

   (('find_lsb', ('bitfield_reverse', 'a@32')), ('ufind_msb_rev', a), 'options->has_find_msb_rev'),
   (('ufind_msb_rev', ('bitfield_reverse', 'a@32')), ('find_lsb', a), '!options->lower_find_lsb'),

   (('ifind_msb', ('f2i32(is_used_once)', a)), ('ufind_msb', ('f2i32', ('fabs', a))), 'true', TestStatus.XFAIL),
   (('ifind_msb', ('extract_u8', a, b)),       ('ufind_msb', ('extract_u8', a, b))),
   (('ifind_msb', ('extract_u16', a, b)),      ('ufind_msb', ('extract_u16', a, b))),
   (('ifind_msb', ('imax', a, 1)),             ('ufind_msb', ('imax', a, 1))),

   (('fmul', ('bcsel(is_used_once)', c, -1.0, 1.0), b), ('bcsel', c, ('fneg', b), ('fcanonicalize', b))),
   (('fmul', ('bcsel(is_used_once)', c, 1.0, -1.0), b), ('bcsel', c, ('fcanonicalize', b), ('fneg', b))),
   (('fmulz(nsz)', ('bcsel(is_used_once)', c, -1.0, 1.0), b), ('bcsel', c, ('fneg', b), ('fcanonicalize', b))),
   (('fmulz(nsz)', ('bcsel(is_used_once)', c, 1.0, -1.0), b), ('bcsel', c, ('fcanonicalize', b), ('fneg', b))),

   (('fabs', ('bcsel(is_used_once)', b, ('fneg', a), a)), ('fabs', a)),
   (('fabs', ('bcsel(is_used_once)', b, a, ('fneg', a))), ('fabs', a)),
   (('~bcsel', ('flt', a, 0.0), ('fneg', a), a), ('fabs', a)),

   (('bcsel', a, ('bcsel(is_used_once)', b, c, d), d), ('bcsel', ('iand', a, b), c, d)),
   (('bcsel', a, ('bcsel(is_used_once)', b, d, c), d), ('bcsel', ('iand', a, ('inot', b)), c, d)),
   (('bcsel', a, b, ('bcsel(is_used_once)', c, b, d)), ('bcsel', ('ior', a, c), b, d)),
   (('bcsel', a, b, ('bcsel(is_used_once)', c, d, b)), ('bcsel', ('iand', c, ('inot', a)), d, b)),
])

optimizations.extend([
   (('fmod', a, b), ('fsub', a, ('fmul', b, ('ffloor', ('fdiv', a, b)))), 'options->lower_fmod'),
   (('frem', a, b), ('fsub', a, ('fmul', b, ('ftrunc', ('fdiv', a, b)))), 'options->lower_fmod'),

   (('uadd_carry', a, b), ('b2i', ('ult', ('iadd', a, b), a)), 'options->lower_uadd_carry'),
   (('usub_borrow', a, b), ('b2i', ('ult', a, b)), 'options->lower_usub_borrow'),

   (('ihadd', a, b), ('iadd', ('iand', a, b), ('ishr', ('ixor', a, b), 1)), 'options->lower_hadd'),
   (('uhadd', a, b), ('iadd', ('iand', a, b), ('ushr', ('ixor', a, b), 1)), 'options->lower_hadd'),
   (('irhadd', a, b), ('isub', ('ior', a, b), ('ishr', ('ixor', a, b), 1)), 'options->lower_hadd'),
   (('urhadd', a, b), ('isub', ('ior', a, b), ('ushr', ('ixor', a, b), 1)), 'options->lower_hadd'),

   (('ihadd@64', a, b), ('iadd', ('iand', a, b), ('ishr', ('ixor', a, b), 1)),
    'options->lower_hadd64 || (options->lower_int64_options & nir_lower_iadd64) != 0'),
   (('uhadd@64', a, b), ('iadd', ('iand', a, b), ('ushr', ('ixor', a, b), 1)),
    'options->lower_hadd64 || (options->lower_int64_options & nir_lower_iadd64) != 0'),
   (('irhadd@64', a, b), ('isub', ('ior', a, b), ('ishr', ('ixor', a, b), 1)),
    'options->lower_hadd64 || (options->lower_int64_options & nir_lower_iadd64) != 0'),
   (('urhadd@64', a, b), ('isub', ('ior', a, b), ('ushr', ('ixor', a, b), 1)),
    'options->lower_hadd64 || (options->lower_int64_options & nir_lower_iadd64) != 0'),

   (('imul_32x16', a, b), ('imul', a, ('extract_i16', b, 0)), 'options->lower_mul_32x16'),
   (('umul_32x16', a, b), ('imul', a, ('extract_u16', b, 0)), 'options->lower_mul_32x16'),

   (('uadd_sat@64', a, b), ('bcsel', ('ult', ('iadd', a, b), a), -1, ('iadd', a, b)),
    'options->lower_uadd_sat || (options->lower_int64_options & (nir_lower_iadd64 | nir_lower_uadd_sat64)) != 0'),
   (('uadd_sat', a, b), ('bcsel', ('ult', ('iadd', a, b), a), -1, ('iadd', a, b)), 'options->lower_uadd_sat'),
   (('usub_sat', a, b), ('bcsel', ('ult', a, b), 0, ('isub', a, b)), 'options->lower_usub_sat'),
   (('usub_sat@64', a, b), ('bcsel', ('ult', a, b), 0, ('isub', a, b)), '(options->lower_int64_options & nir_lower_usub_sat64) != 0'),
])

optimizations.extend([
   (('iadd_sat@64', a, b),
    ('bcsel',
     ('iand', ('iand', ('ilt', a, 0), ('ilt', b, 0)), ('ige', ('iadd', a, b), 0)),
     0x8000000000000000,
     ('bcsel',
      ('ior', ('ior', ('ilt', a, 0), ('ilt', b, 0)), ('ige', ('iadd', a, b), 0)),
      ('iadd', a, b),
      0x7fffffffffffffff)),
    '(options->lower_int64_options & nir_lower_iadd_sat64) != 0', TestStatus.XFAIL),

   (('isub_sat@64', a, b),
    ('bcsel',
     ('iand', ('iand', ('ilt', a, 0), ('ige', b, 0)), ('ige', ('isub', a, b), 0)),
     0x8000000000000000,
     ('bcsel',
      ('ior', ('ior', ('ilt', a, 0), ('ige', b, 0)), ('ige', ('isub', a, b), 0)),
      ('isub', a, b),
      0x7fffffffffffffff)),
    '(options->lower_int64_options & nir_lower_iadd_sat64) != 0'),

   (('ilt', ('imax(is_used_once)', 'a@64', 'b@64'), 0),
    ('ilt', ('imax', ('unpack_64_2x32_split_y', a), ('unpack_64_2x32_split_y', b)), 0),
    '(options->lower_int64_options & nir_lower_minmax64) != 0'),
   (('ilt', ('imin(is_used_once)', 'a@64', 'b@64'), 0),
    ('ilt', ('imin', ('unpack_64_2x32_split_y', a), ('unpack_64_2x32_split_y', b)), 0),
    '(options->lower_int64_options & nir_lower_minmax64) != 0'),
   (('ige', ('imax(is_used_once)', 'a@64', 'b@64'), 0),
    ('ige', ('imax', ('unpack_64_2x32_split_y', a), ('unpack_64_2x32_split_y', b)), 0),
    '(options->lower_int64_options & nir_lower_minmax64) != 0'),
   (('ige', ('imin(is_used_once)', 'a@64', 'b@64'), 0),
    ('ige', ('imin', ('unpack_64_2x32_split_y', a), ('unpack_64_2x32_split_y', b)), 0),
    '(options->lower_int64_options & nir_lower_minmax64) != 0'),

   (('ilt', 'a@64', 0), ('ilt', ('unpack_64_2x32_split_y', a), 0), '(options->lower_int64_options & nir_lower_icmp64) != 0'),
   (('ige', 'a@64', 0), ('ige', ('unpack_64_2x32_split_y', a), 0), '(options->lower_int64_options & nir_lower_icmp64) != 0'),

   (('ine', 'a@64', 0),
    ('ine', ('ior', ('unpack_64_2x32_split_x', a), ('unpack_64_2x32_split_y', a)), 0),
    '(options->lower_int64_options & nir_lower_icmp64) != 0'),
   (('ieq', 'a@64', 0),
    ('ieq', ('ior', ('unpack_64_2x32_split_x', a), ('unpack_64_2x32_split_y', a)), 0),
    '(options->lower_int64_options & nir_lower_icmp64) != 0'),
   (('ult', 0, 'a@64'),
    ('ine', ('ior', ('unpack_64_2x32_split_x', a), ('unpack_64_2x32_split_y', a)), 0),
    '(options->lower_int64_options & nir_lower_icmp64) != 0'),
])

optimizations.extend([
   (('ibitfield_extract', 'value@32', 'offset', 'bits'),
    ('bcsel', ('ult', 31, 'bits'), 'value', ('ibfe', 'value', 'offset', 'bits')),
    'options->lower_bitfield_extract && options->has_bfe'),

   (('ubitfield_extract', 'value@32', 'offset', 'bits'),
    ('bcsel', ('ult', 31, 'bits'), 'value', ('ubfe', 'value', 'offset', 'bits')),
    'options->lower_bitfield_extract && options->has_bfe'),

   (('bitfield_select', a, b, 0), ('iand', a, b)),
   (('bitfield_select', a, 0, b), ('iand', ('inot', a), b)),
   (('bitfield_select', 0, a, b), b),
   (('bitfield_select', a, b, -1), ('ior', ('inot', a), b)),
   (('bitfield_select', a, -1, b), ('ior', a, b)),
   (('bitfield_select', -1, a, b), a),
   (('bitfield_select', a, b, b), b),
   (('bitfield_select', a, ('inot', b), b), ('ixor', a, b)),
   (('bitfield_select', a, b, ('inot', b)), ('inot', ('ixor', a, b))),
   (('bitfield_select', a, ('iand', a, b), c), ('bitfield_select', a, b, c)),
   (('bitfield_select', a, b, ('iand', ('inot', a), c)), ('bitfield_select', a, b, c)),
   (('bitfield_select', ('inot', a), b, c), ('bitfield_select', a, c, b)),
   (('bitfield_select', ('ineg', ('b2i', 'a@1')), b, c), ('bcsel', a, b, c)),

   (('ior', ('iand', a, b), ('iand', ('inot', a), c)), ('bitfield_select', a, b, c), 'options->has_bitfield_select'),
   (('iadd', ('iand', a, b), ('iand', ('inot', a), c)), ('bitfield_select', a, b, c), 'options->has_bitfield_select'),
   (('ixor', ('iand', a, b), ('iand', ('inot', a), c)), ('bitfield_select', a, b, c), 'options->has_bitfield_select'),
   (('ixor', ('iand', a, ('ixor', b, c)), c), ('bitfield_select', a, b, c), 'options->has_bitfield_select'),

   (('bitfield_select@1', a, b, c), ('bcsel', a, b, c)),

   (('ubfe', 'value', 'offset', ('iand', 31, 'bits')), ('ubfe', 'value', 'offset', 'bits')),
   (('ubfe', 'value', ('iand', 31, 'offset'), 'bits'), ('ubfe', 'value', 'offset', 'bits')),
   (('ibfe', 'value', 'offset', ('iand', 31, 'bits')), ('ibfe', 'value', 'offset', 'bits')),
   (('ibfe', 'value', ('iand', 31, 'offset'), 'bits'), ('ibfe', 'value', 'offset', 'bits')),
   (('bfm', 'bits', ('iand', 31, 'offset')), ('bfm', 'bits', 'offset')),
   (('bfm', ('iand', 31, 'bits'), 'offset'), ('bfm', 'bits', 'offset')),
])

optimizations.extend([
   (('ult', a, ('umin', ('iand', a, b), c)), False),
   (('ult', 31, ('umin', '#bits(is_ult_32)', a)), False),

   (('ubfe', 'value', 'offset', ('umin', 'width', ('iadd', 32, ('ineg', ('iand', 31, 'offset'))))),
    ('ubfe', 'value', 'offset', 'width'), 'true', TestStatus.XFAIL),
   (('ibfe', 'value', 'offset', ('umin', 'width', ('iadd', 32, ('ineg', ('iand', 31, 'offset'))))),
    ('ibfe', 'value', 'offset', 'width'), 'true', TestStatus.XFAIL),
   (('bfm', ('umin', 'width', ('iadd', 32, ('ineg', ('iand', 31, 'offset')))), 'offset'),
    ('bfm', 'width', 'offset'), 'true', TestStatus.XFAIL),

   (('iadd@32', ('ishl', 1, a), -1), ('bfm', a, 0), 'options->has_bfm'),
   (('ishl', ('bfm', a, 0), b), ('bfm', a, b)),

   (('ubfe', a, b, 0), 0),
   (('ibfe', a, b, 0), 0),

   (('ubfe', a, 0, '#b'), ('iand', a, ('ushr', 0xffffffff, ('ineg', b))), 'true', TestStatus.XFAIL),

   (('b2i32', ('ine', ('ubfe', a, b, 1), 0)), ('ubfe', a, b, 1)),
   (('b2i32', ('ine', ('ibfe', a, b, 1), 0)), ('ubfe', a, b, 1)),
])

optimizations.extend([
   (('ine', ('ibfe(is_used_once)', a, '#b', '#c'), 0),
    ('ine', ('iand', a, ('ishl', ('ushr', 0xffffffff, ('ineg', c)), b)), 0), 'true', TestStatus.XFAIL),
   (('ieq', ('ibfe(is_used_once)', a, '#b', '#c'), 0),
    ('ieq', ('iand', a, ('ishl', ('ushr', 0xffffffff, ('ineg', c)), b)), 0), 'true', TestStatus.XFAIL),
   (('ine', ('ubfe(is_used_once)', a, '#b', '#c'), 0),
    ('ine', ('iand', a, ('ishl', ('ushr', 0xffffffff, ('ineg', c)), b)), 0), 'true', TestStatus.XFAIL),
   (('ieq', ('ubfe(is_used_once)', a, '#b', '#c'), 0),
    ('ieq', ('iand', a, ('ishl', ('ushr', 0xffffffff, ('ineg', c)), b)), 0), 'true', TestStatus.XFAIL),

   (('ine', ('iand(is_used_once)', ('ushr', a, '#b'), '#c'), 0), ('ine', ('iand', a, ('ishl', c, b)), 0)),
   (('ine', ('iand(is_used_once)', ('ishl', a, '#b'), '#c'), 0), ('ine', ('iand', a, ('ushr', c, b)), 0)),
   (('ieq', ('iand(is_used_once)', ('ushr', a, '#b'), '#c'), 0), ('ieq', ('iand', a, ('ishl', c, b)), 0)),
   (('ieq', ('iand(is_used_once)', ('ishl', a, '#b'), '#c'), 0), ('ieq', ('iand', a, ('ushr', c, b)), 0)),
])

optimizations.extend([
   (('ifind_msb', 'value'),
    ('ufind_msb', ('bcsel', ('ilt', 'value', 0), ('inot', 'value'), 'value')),
    'options->lower_ifind_msb && !options->has_find_msb_rev && !options->has_uclz'),

   (('ifind_msb', 'value'),
    ('bcsel',
     ('ige', ('ifind_msb_rev', 'value'), 0),
     ('isub', 31, ('ifind_msb_rev', 'value')),
     ('ifind_msb_rev', 'value')),
    'options->lower_ifind_msb && options->has_find_msb_rev'),

   (('ifind_msb', 'value'),
    ('isub', 31, ('uclz', ('ixor', 'value', ('ishr', 'value', 31)))),
    'options->lower_ifind_msb && options->has_uclz'),

   (('ufind_msb', 'value@32'),
    ('bcsel',
     ('ige', ('ufind_msb_rev', 'value'), 0),
     ('isub', 31, ('ufind_msb_rev', 'value')),
     ('ufind_msb_rev', 'value')),
    'options->lower_ufind_msb && options->has_find_msb_rev'),

   (('ufind_msb', 'value@32'), ('isub', 31, ('uclz', 'value')), 'options->lower_ufind_msb && options->has_uclz'),

   (('uclz', a), ('umin', 32, ('ufind_msb_rev', a)), '!options->has_uclz && options->has_find_msb_rev'),

   (('find_lsb', 'value@64'), ('ufind_msb', ('iand', 'value', ('ineg', 'value'))), 'options->lower_find_lsb'),
   (('find_lsb', 'value'), ('ufind_msb', ('u2u32', ('iand', 'value', ('ineg', 'value')))), 'options->lower_find_lsb'),
])

optimizations.extend([
   (('extract_i8', a, 'b@32'), ('ishr', ('ishl', a, ('imul', ('isub', 3, b), 8)), 24), 'options->lower_extract_byte'),
   (('extract_u8', a, 'b@32'), ('iand', ('ushr', a, ('imul', b, 8)), 0xff), 'options->lower_extract_byte'),
   (('extract_i16', a, 'b@32'), ('ishr', ('ishl', a, ('imul', ('isub', 1, b), 16)), 16), 'options->lower_extract_word'),
   (('extract_u16', a, 'b@32'), ('iand', ('ushr', a, ('imul', b, 16)), 0xffff), 'options->lower_extract_word'),
])

optimizations.extend([
    (('pack_unorm_2x16', 'v'),
     ('pack_uvec2_to_uint',
        ('f2u32', ('fround_even', ('fmul', ('fsat', 'v'), 65535.0)))),
     'options->lower_pack_unorm_2x16'),

    (('pack_unorm_4x8', 'v'),
     ('pack_uvec4_to_uint',
        ('f2u32', ('fround_even', ('fmul', ('fsat', 'v'), 255.0)))),
     'options->lower_pack_unorm_4x8 && !options->has_pack_32_4x8'),

    (('pack_unorm_4x8', 'v'),
     ('pack_32_4x8',
        ('f2u8', ('fround_even', ('fmul', ('fsat', 'v'), 255.0)))),
     'options->lower_pack_unorm_4x8 && options->has_pack_32_4x8'),

    (('pack_snorm_2x16', 'v'),
     ('pack_uvec2_to_uint',
        ('f2i32', ('fround_even', ('fmul', ('fmin', 1.0, ('fmax', -1.0, 'v')), 32767.0)))),
     'options->lower_pack_snorm_2x16'),

    (('pack_snorm_4x8', 'v'),
     ('pack_uvec4_to_uint',
        ('f2i32', ('fround_even', ('fmul', ('fmin', 1.0, ('fmax', -1.0, 'v')), 127.0)))),
     'options->lower_pack_snorm_4x8 && !options->has_pack_32_4x8'),

    (('pack_snorm_4x8', 'v'),
     ('pack_32_4x8',
        ('f2i8', ('fround_even', ('fmul', ('fmin', 1.0, ('fmax', -1.0, 'v')), 127.0)))),
     'options->lower_pack_snorm_4x8 && options->has_pack_32_4x8'),

    (('unpack_unorm_2x16', 'v'),
     ('fdiv', ('u2f32', ('vec2', ('extract_u16', 'v', 0),
                                ('extract_u16', 'v', 1))),
              65535.0),
     'options->lower_unpack_unorm_2x16'),

    (('unpack_unorm_4x8', 'v'),
     ('fdiv', ('u2f32', ('vec4', ('extract_u8', 'v', 0),
                                ('extract_u8', 'v', 1),
                                ('extract_u8', 'v', 2),
                                ('extract_u8', 'v', 3))),
              255.0),
     'options->lower_unpack_unorm_4x8'),

    (('unpack_snorm_2x16', 'v'),
     ('fmin', 1.0,
      ('fmax', -1.0,
       ('fdiv',
        ('i2f', ('vec2', ('extract_i16', 'v', 0),
                      ('extract_i16', 'v', 1))),
        32767.0))),
     'options->lower_unpack_snorm_2x16'),

    (('unpack_snorm_4x8', 'v'),
     ('fmin', 1.0,
      ('fmax', -1.0,
       ('fdiv',
        ('i2f', ('vec4', ('extract_i8', 'v', 0),
                      ('extract_i8', 'v', 1),
                      ('extract_i8', 'v', 2),
                      ('extract_i8', 'v', 3))),
        127.0))),
     'options->lower_unpack_snorm_4x8'),
])


def _clamp01_minmax(x):
    return ('fmin', ('fmax', x, 0.0), 1.0)


def _clamp01_maxmin(x):
    return ('fmax', ('fmin', x, 1.0), 0.0)


# Redundant saturation around pack_unorm (treated as inexact w.r.t. NaN payload / edge cases).
optimizations.extend([
    (('~pack_unorm_2x16', ('fsat', 'a@32')), ('pack_unorm_2x16', a)),
    (('~pack_unorm_4x8',  ('fsat', 'a@32')), ('pack_unorm_4x8', a)),

    (('~pack_unorm_2x16', _clamp01_minmax('a@32')), ('pack_unorm_2x16', a)),
    (('~pack_unorm_2x16', _clamp01_maxmin('a@32')), ('pack_unorm_2x16', a)),
    (('~pack_unorm_4x8',  _clamp01_minmax('a@32')), ('pack_unorm_4x8', a)),
    (('~pack_unorm_4x8',  _clamp01_maxmin('a@32')), ('pack_unorm_4x8', a)),
])

# Per-component fsat removal
for mask in range(1, 4):  # vec2
    s0 = ('fsat', 'a') if (mask & 1) else 'a'
    s1 = ('fsat', 'b') if (mask & 2) else 'b'
    optimizations.append(
        (('~pack_unorm_2x16', ('vec2', s0, s1)),
         ('pack_unorm_2x16', ('vec2', a, b)))
    )

for mask in range(1, 16):  # vec4
    s0 = ('fsat', 'a') if (mask & 1) else 'a'
    s1 = ('fsat', 'b') if (mask & 2) else 'b'
    s2 = ('fsat', 'c') if (mask & 4) else 'c'
    s3 = ('fsat', 'd') if (mask & 8) else 'd'
    optimizations.append(
        (('~pack_unorm_4x8', ('vec4', s0, s1, s2, s3)),
         ('pack_unorm_4x8', ('vec4', a, b, c, d)))
    )

# Per-component clamp-chain removal (both clamp forms)
for clamp01 in (_clamp01_minmax, _clamp01_maxmin):
    for mask in range(1, 4):  # vec2
        s0 = clamp01('a') if (mask & 1) else 'a'
        s1 = clamp01('b') if (mask & 2) else 'b'
        optimizations.append(
            (('~pack_unorm_2x16', ('vec2', s0, s1)),
             ('pack_unorm_2x16', ('vec2', a, b)))
        )

    for mask in range(1, 16):  # vec4
        s0 = clamp01('a') if (mask & 1) else 'a'
        s1 = clamp01('b') if (mask & 2) else 'b'
        s2 = clamp01('c') if (mask & 4) else 'c'
        s3 = clamp01('d') if (mask & 8) else 'd'
        optimizations.append(
            (('~pack_unorm_4x8', ('vec4', s0, s1, s2, s3)),
             ('pack_unorm_4x8', ('vec4', a, b, c, d)))
        )


optimizations.extend([
   (('pack_half_2x16_split', 'a@32', 'b@32'),
    ('ior', ('ishl', ('u2u32', ('f2f16', b)), 16), ('u2u32', ('f2f16', a))),
    'options->lower_pack_split'),

   (('unpack_half_2x16_split_x', 'a@32'),
    ('f2f32', ('u2u16', a)),
    'options->lower_pack_split && !nir_is_denorm_flush_to_zero(info->float_controls_execution_mode, 16)'),

   (('unpack_half_2x16_split_x', 'a@32'),
    ('f2f32', ('fmul', 1.0, ('u2u16', a))),
    'options->lower_pack_split && nir_is_denorm_flush_to_zero(info->float_controls_execution_mode, 16)'),

   (('unpack_half_2x16_split_y', 'a@32'),
    ('f2f32', ('u2u16', ('ushr', a, 16))),
    'options->lower_pack_split && !nir_is_denorm_flush_to_zero(info->float_controls_execution_mode, 16)'),

   (('unpack_half_2x16_split_y', 'a@32'),
    ('f2f32', ('fmul', 1.0, ('u2u16', ('ushr', a, 16)))),
    'options->lower_pack_split && nir_is_denorm_flush_to_zero(info->float_controls_execution_mode, 16)'),

   (('isign', a), ('imin', ('imax', a, -1), 1), 'options->lower_isign'),
   (('imin', ('imax', a, -1), 1), ('isign', a), '!options->lower_isign'),
   (('imax', ('imin', a, 1), -1), ('isign', a), '!options->lower_isign'),

   (('fsign', a),
    ('fsub', ('b2f', ('!flt', 0.0, a)), ('b2f', ('!flt', a, 0.0))),
    'options->lower_fsign'),
   (('fsign', 'a@64'),
    ('fsub', ('b2f', ('!flt', 0.0, a)), ('b2f', ('!flt', a, 0.0))),
    'options->lower_doubles_options & nir_lower_dsign'),

   (('amul', a, b), ('imul', a, b), '!options->has_imul24 && !options->has_amul'),

   (('amul', ('udiv_aligned_4', a), 4), a),
   (('imul', ('udiv_aligned_4', a), 4), a),

   (('umul24', a, b),
    ('imul', ('iand', a, 0xffffff), ('iand', b, 0xffffff)),
    '!options->has_umul24'),
   (('umad24', a, b, c),
    ('iadd', ('imul', ('iand', a, 0xffffff), ('iand', b, 0xffffff)), c),
    '!options->has_umad24'),

   (('imul24_relaxed', a, b), ('imul24', a, b), '!options->has_mul24_relaxed && options->has_imul24'),
   (('imul24_relaxed', a, b), ('imul', a, b), '!options->has_mul24_relaxed && !options->has_imul24'),
   (('umad24_relaxed', a, b, c), ('umad24', a, b, c),  'options->has_umad24'),
   (('umad24_relaxed', a, b, c), ('iadd', ('umul24_relaxed', a, b), c), '!options->has_umad24'),
   (('umul24_relaxed', a, b), ('umul24', a, b), '!options->has_mul24_relaxed && options->has_umul24'),
   (('umul24_relaxed', a, b), ('imul', a, b), '!options->has_mul24_relaxed && !options->has_umul24'),
])

optimizations.extend([
   (('imad24_ir3', a, b, 0), ('imul24', a, b)),
   (('imad24_ir3', a, 0, c), c),
   (('imad24_ir3', a, 1, c), ('iadd', a, c)),
   (('imad24_ir3', '#a', '#b', c), ('iadd', ('imul24', a, b), c)),

   (('imul24', a, '#b@32(is_pos_power_of_two)'), ('ishl', a, ('find_lsb', b)), '!options->lower_bitops'),
   (('imul24', a, '#b@32(is_neg_power_of_two)'), ('ineg', ('ishl', a, ('find_lsb', ('iabs', b)))), '!options->lower_bitops'),
   (('imul24', a, 0), 0),

   (('imul_high@16', a, b),
    ('i2i16', ('ishr', ('imul24_relaxed', ('i2i32', a), ('i2i32', b)), 16)),
    'options->lower_mul_high16'),
   (('umul_high@16', a, b),
    ('u2u16', ('ushr', ('umul24_relaxed', ('u2u32', a), ('u2u32', b)), 16)),
    'options->lower_mul_high16'),

   (('b2i16', ('vec2', ('ult', 'a@16', b), ('ult', 'c@16', d))),
    ('umin', 1, ('usub_sat', ('vec2', b, d), ('vec2', a, c))),
    'options->vectorize_vec2_16bit && !options->lower_usub_sat'),
   (('b2i16', ('vec2', ('uge', 'a@16', '#b(is_not_zero)'), ('uge', 'c@16', '#d(is_not_zero)'))),
    ('umin', 1, ('usub_sat', ('vec2', a, c), ('iadd', ('vec2', b, d), -1))),
    'options->vectorize_vec2_16bit && !options->lower_usub_sat'),
   (('b2i16', ('vec2', ('uge', '#a(is_not_uint_max)', 'b@16'), ('uge', '#c(is_not_uint_max)', 'd@16'))),
    ('umin', 1, ('usub_sat', ('iadd', ('vec2', a, c), 1), ('vec2', b, d))),
    'options->vectorize_vec2_16bit && !options->lower_usub_sat'),

   (('~fmin@32', ('fmax@32(is_used_once)', 'a@32', 0.0), 1.0), ('fsat@32', a), '!options->lower_fsat'),
   (('~fmax@32', ('fmin@32(is_used_once)', 'a@32', 1.0), 0.0), ('fsat@32', a), '!options->lower_fsat'),
   (('~fmin@16', ('fmax@16(is_used_once)', 'a@16', 0.0), 1.0), ('fsat@16', a), '!options->lower_fsat'),
   (('~fmax@16', ('fmin@16(is_used_once)', 'a@16', 1.0), 0.0), ('fsat@16', a), '!options->lower_fsat'),

   (('fsat', ('fabs(is_used_once)', a)), ('fsat', a)),
   (('fabs', ('fsat(is_used_once)', a)), ('fsat', a)),
   (('fsat', ('fabs', ('fsat', a))), ('fsat', a)),
   (('fsat', ('fmin', ('fmax', ('fabs', a), 0.0), 1.0)), ('fsat', a)),

   (('fabs', ('fabs', a)), ('fabs', a)),
   (('iabs', ('iabs', a)), ('iabs', a)),
])

for bit_size in (8, 16, 32, 64):
    cond = '!options->lower_uadd_sat'
    if bit_size == 64:
        cond += ' && !(options->lower_int64_options & (nir_lower_iadd64 | nir_lower_uadd_sat64))'
    add = f'iadd@{bit_size}'

    optimizations += [
        (('bcsel', ('ult', ('iadd', a, b), a), -1, (add, a, b)), ('uadd_sat', a, b), cond),
        (('bcsel', ('uge', ('iadd', a, b), a), (add, a, b), -1), ('uadd_sat', a, b), cond),
        (('bcsel', ('ieq', ('uadd_carry', a, b), 0), (add, a, b), -1), ('uadd_sat', a, b), cond),
        (('bcsel', ('ine', ('uadd_carry', a, b), 0), -1, (add, a, b)), ('uadd_sat', a, b), cond),
    ]

optimizations += [
   (('bitfield_insert', 'base@32', 'insert', 'offset', 'bits'),
    ('bcsel', ('ult', 31, 'bits'), 'insert',
              ('bitfield_select', ('bfm', 'bits', 'offset'), ('ishl', 'insert', 'offset'), 'base')),
    'options->lower_bitfield_insert && options->has_bfm && options->has_bitfield_select && !options->has_bfi'),
   (('bitfield_insert', 'base@32', 'insert', 'offset', 'bits'),
    ('bcsel', ('ult', 31, 'bits'), 'insert',
              ('bfi', ('bfm', 'bits', 'offset'), 'insert', 'base')),
    'options->lower_bitfield_insert && options->has_bfm && options->has_bfi'),
]

for bit_size in (8, 16, 32):
    bit_size_str = f'{bit_size}' if bit_size < 32 else ''
    extract_opt = f'options->lower_bitfield_extract{bit_size_str}'
    if bit_size == 32:
        extract_opt += ' && !options->has_bfe'

    optimizations += [
        (('ibitfield_extract', f'value@{bit_size}', 'offset', 'bits'),
         ('bcsel', ('ieq', 0, 'bits'),
          0,
          ('ishr',
           ('ishl', 'value', ('isub', ('isub', bit_size, 'bits'), 'offset')),
           ('isub', bit_size, 'bits'))),
         extract_opt),

        (('ubitfield_extract', f'value@{bit_size}', 'offset', 'bits'),
         ('iand',
          ('ushr', 'value', 'offset'),
          ('bcsel', ('ieq', 'bits', bit_size),
           -1,
           ('isub', ('ishl', 1, 'bits'), 1))),
         extract_opt),
    ]

for sz in (8, 16, 32, 64):
    base = f'base@{sz}'

    if sz in (8, 16):
        optimizations += [
            (('bitfield_insert', base, 'insert', 'offset', 'bits'),
             ('bitfield_select', (f'u2u{sz}', ('bfm', 'bits', 'offset')), ('ishl', 'insert', 'offset'), 'base'),
             'options->lower_bitfield_insert && options->has_bfm && options->has_bitfield_select && !options->has_bfi'),
            (('bitfield_insert', base, 'insert', 'offset', 'bits'),
             (f'u2u{sz}', ('bfi', ('bfm', 'bits', 'offset'), ('u2u32', 'insert'), ('u2u32', 'base'))),
             'options->lower_bitfield_insert && options->has_bfm && options->has_bfi'),
        ]

    optimizations += [
        (('bitfield_insert', base, 'insert', 'offset', 'bits'),
         ('bcsel', ('ult', sz - 1, 'bits'), 'insert',
          ('ior',
           ('iand', 'base', ('inot', ('ishl', ('isub', ('ishl', 1, 'bits'), 1), 'offset'))),
           ('iand', ('ishl', 'insert', 'offset'), ('ishl', ('isub', ('ishl', 1, 'bits'), 1), 'offset')))),
         'true' if sz == 64 else
         'options->lower_bitfield_insert && (!options->has_bfm || (!options->has_bfi && !options->has_bitfield_select))',
         TestStatus.XFAIL if sz == 64 else TestStatus.PASS),
    ]

for bit_size in (8, 16, 32, 64):
    cond = '!options->lower_usub_sat'
    if bit_size == 64:
        cond += ' && !(options->lower_int64_options & nir_lower_usub_sat64)'
    add = f'iadd@{bit_size}'

    optimizations += [
        (('bcsel', ('ult', a, b), 0, (add, a, ('ineg', b))), ('usub_sat', a, b), cond),
        (('bcsel', ('uge', a, b), (add, a, ('ineg', b)), 0), ('usub_sat', a, b), cond),
        (('bcsel', ('ieq', ('usub_borrow', a, b), 0), (add, a, ('ineg', b)), 0), ('usub_sat', a, b), cond),
        (('bcsel', ('ine', ('usub_borrow', a, b), 0), 0, (add, a, ('ineg', b))), ('usub_sat', a, b), cond),
    ]

for bit_size in (8, 16, 32, 64):
    intmax = (1 << (bit_size - 1)) - 1
    intmin = 1 << (bit_size - 1)

    optimizations += [
        (('iadd_sat@' + str(bit_size), a, b),
         ('bcsel',
          ('ige', b, 1),
          ('bcsel', ('ilt', ('iadd', a, b), a), intmax, ('iadd', a, b)),
          ('bcsel', ('ilt', a, ('iadd', a, b)), intmin, ('iadd', a, b))),
         'options->lower_iadd_sat',
         TestStatus.XFAIL if bit_size in (8, 64) else TestStatus.PASS),

        (('isub_sat@' + str(bit_size), a, b),
         ('bcsel',
          ('ilt', b, 0),
          ('bcsel', ('ilt', ('isub', a, b), a), intmax, ('isub', a, b)),
          ('bcsel', ('ilt', a, ('isub', a, b)), intmin, ('isub', a, b))),
         'options->lower_iadd_sat'),
    ]


invert = OrderedDict((('feq', 'fneu'), ('fneu', 'feq')))

for left, right in itertools.combinations_with_replacement(invert.keys(), 2):
    optimizations.append(
        (('inot', ('ior(is_used_once)',
                  (left + '(is_used_once)', a, b),
                  (right + '(is_used_once)', c, d))),
         ('iand', (invert[left], a, b), (invert[right], c, d)))
    )
    optimizations.append(
        (('inot', ('iand(is_used_once)',
                  (left + '(is_used_once)', a, b),
                  (right + '(is_used_once)', c, d))),
         ('ior', (invert[left], a, b), (invert[right], c, d)))
    )


for x, y in itertools.product(('f', 'u', 'i'), ('f', 'u', 'i')):
    if x != 'f' and y != 'f' and x != y:
        continue
    b2x = 'b2f' if x == 'f' else 'b2i'
    b2y = 'b2f' if y == 'f' else 'b2i'
    x2yN = f'{x}2{y}'
    optimizations.append(((x2yN, (b2x, a)), (b2y, a)))


optimizations += [
    (('f2f16_rtz', ('b2f', a)), ('b2f16', a)),
    (('f2f16_rtne', ('b2f', a)), ('b2f16', a)),
]


for t in ('int', 'uint', 'float', 'bool'):
    for N in type_sizes(t):
        x2xN = f'{t[0]}2{t[0]}{N}'
        aN = f'a@{N}'
        optimizations.append(((x2xN, aN), a))


for N, M in itertools.product(type_sizes('uint'), type_sizes('uint')):
    if N < M:
        for x, y in itertools.product(('i', 'u'), ('i', 'u')):
            x2xN = f'{x}2{x}{N}'
            y2yM = f'{y}2{y}{M}'
            y2yN = f'{y}2{y}{N}'
            optimizations.append(((x2xN, (y2yM, a)), (y2yN, a)))
    elif N > M:
        for P in type_sizes('uint'):
            if M < P:
                continue
            for x in ('i', 'u'):
                x2xN = f'{x}2{x}{N}'
                x2xM = f'{x}2{x}{M}'
                aP = f'a@{P}'
                optimizations.append(((x2xN, (x2xM, aP)), (x2xN, a)))
    else:
        pass


for t in ('i', 'u'):
    for N in (8, 16, 32):
        x2xN = f'{t}2{t}{N}'
        optimizations += [
            ((x2xN, ('pack_64_2x32_split', a, b)), (x2xN, a)),
            ((x2xN, ('pack_64_2x32', a)), (x2xN, 'a.x')),

            ((x2xN, ('pack_32_2x16_split', a, b)), (x2xN, a)),
            ((x2xN, ('pack_32_2x16', a)), (x2xN, 'a.x')),
        ]

for t in ('int', 'uint', 'float'):
    for N, M in itertools.product(type_sizes(t), repeat=2):
        if N == 1 or N >= M:
            continue

        cond = 'true'
        if N == 8:
            cond = 'options->support_8bit_alu'
        elif N == 16:
            cond = 'options->support_16bit_alu'

        x2xM = f'{t[0]}2{t[0]}{M}'
        x2xN = f'{t[0]}2{t[0]}{N}'
        aN = f'a@{N}'
        bN = f'b@{N}'

        xeq = 'feq' if t == 'float' else 'ieq'
        xne = 'fneu' if t == 'float' else 'ine'
        xge = f'{t[0]}ge'
        xlt = f'{t[0]}lt'

        for P in type_sizes(t):
            if P == 1 or P > N:
                continue

            bP = f'b@{P}'
            optimizations += [
                ((xeq, (x2xM, aN), (x2xM, bP)), (xeq, a, (x2xN, b)), cond),
                ((xne, (x2xM, aN), (x2xM, bP)), (xne, a, (x2xN, b)), cond),
                ((xge, (x2xM, aN), (x2xM, bP)), (xge, a, (x2xN, b)), cond),
                ((xlt, (x2xM, aN), (x2xM, bP)), (xlt, a, (x2xN, b)), cond),
                ((xge, (x2xM, bP), (x2xM, aN)), (xge, (x2xN, b), a), cond),
                ((xlt, (x2xM, bP), (x2xM, aN)), (xlt, (x2xN, b), a), cond),
            ]

        if t in ('int', 'uint'):
            if t == 'int':
                xN_min = -(1 << (N - 1))
                xN_max = (1 << (N - 1)) - 1
            else:
                xN_min = 0
                xN_max = (1 << N) - 1

            optimizations += [
                ((xeq, (x2xM, aN), '#b'),
                 ('iand', (xeq, a, (x2xN, b)), (xeq, (x2xM, (x2xN, b)), b)), cond),
                ((xne, (x2xM, aN), '#b'),
                 ('ior', (xne, a, (x2xN, b)), (xne, (x2xM, (x2xN, b)), b)), cond),

                ((xlt, (x2xM, aN), '#b'),
                 ('iand',
                  (xlt, xN_min, b),
                  ('ior', (xlt, xN_max, b), (xlt, a, (x2xN, b)))), cond),
                ((xlt, '#a', (x2xM, bN)),
                 ('iand',
                  (xlt, a, xN_max),
                  ('ior', (xlt, a, xN_min), (xlt, (x2xN, a), b))), cond),

                ((xge, (x2xM, aN), '#b'),
                 ('iand',
                  (xge, xN_max, b),
                  ('ior', (xge, xN_min, b), (xge, a, (x2xN, b)))), cond),
                ((xge, '#a', (x2xM, bN)),
                 ('iand',
                  (xge, a, xN_min),
                  ('ior', (xge, a, xN_max), (xge, (x2xN, a), b))), cond),
            ]


optimizations += [
    (('i2i32', ('iand', 'a@64', 0xffffffff)), ('u2u32', a)),
    (('i2i16', ('iand', 'a@32', 0xffff)), ('u2u16', a)),
    (('i2i16', ('iand', 'a@64', 0xffff)), ('u2u16', a)),
    (('i2i8', ('iand', 'a@16', 0xff)), ('u2u8', a)),
    (('i2i8', ('iand', 'a@32', 0xff)), ('u2u8', a)),
    (('i2i8', ('iand', 'a@64', 0xff)), ('u2u8', a)),
]


for f2f16 in ('f2f16', 'f2f16_rtz', 'f2f16_rtne'):
    optimizations += [
        ((f2f16, ('fmax(is_used_once)', a, '#b')), ('fmax', (f2f16, a), (f2f16, b)), 'options->support_16bit_alu'),
        ((f2f16, ('fmin(is_used_once)', a, '#b')), ('fmin', (f2f16, a), (f2f16, b)), 'options->support_16bit_alu'),

        ((f2f16, ('vec2(is_used_once)',
                  ('fmax(is_used_once)', a, '#b'),
                  ('fmax(is_used_once)', c, '#d'))),
         ('fmax', (f2f16, ('vec2', a, c)), (f2f16, ('vec2', b, d))),
         'options->support_16bit_alu'),
        ((f2f16, ('vec2(is_used_once)',
                  ('fmin(is_used_once)', a, '#b'),
                  ('fmin(is_used_once)', c, '#d'))),
         ('fmin', (f2f16, ('vec2', a, c)), (f2f16, ('vec2', b, d))),
         'options->support_16bit_alu'),

        ((f2f16, ('f2f32', 'a@16')), ('fcanonicalize', a)),
        ((f2f16, ('bcsel', a, '#b', c)), ('bcsel', a, (f2f16, b), (f2f16, c)), 'options->support_16bit_alu'),
        ((f2f16, ('bcsel', a, b, '#c')), ('bcsel', a, (f2f16, b), (f2f16, c)), 'options->support_16bit_alu'),
    ]


optimizations += [
    (('pack_half_2x16_rtz_split',
      ('fmax(is_used_once)', a, '#b'),
      ('fmax(is_used_once)', c, '#d')),
     ('pack_32_2x16',
      ('fmax',
       ('unpack_32_2x16', ('pack_half_2x16_rtz_split', a, c)),
       ('unpack_32_2x16', ('pack_half_2x16_rtz_split', b, d)))),
     'options->vectorize_vec2_16bit'),
    (('pack_half_2x16_rtz_split',
      ('fmin(is_used_once)', a, '#b'),
      ('fmin(is_used_once)', c, '#d')),
     ('pack_32_2x16',
      ('fmin',
       ('unpack_32_2x16', ('pack_half_2x16_rtz_split', a, c)),
       ('unpack_32_2x16', ('pack_half_2x16_rtz_split', b, d)))),
     'options->vectorize_vec2_16bit'),

    (('pack_half_2x16_rtz_split',
      ('fneg(is_used_once)', ('fmax(is_used_once)', a, '#b')),
      ('fneg(is_used_once)', ('fmax(is_used_once)', c, '#d'))),
     ('pack_32_2x16',
      ('fmin',
       ('fneg', ('unpack_32_2x16', ('pack_half_2x16_rtz_split', a, c))),
       ('fneg', ('unpack_32_2x16', ('pack_half_2x16_rtz_split', b, d))))),
     'options->vectorize_vec2_16bit'),
    (('pack_half_2x16_rtz_split',
      ('fneg(is_used_once)', ('fmin(is_used_once)', a, '#b')),
      ('fneg(is_used_once)', ('fmin(is_used_once)', c, '#d'))),
     ('pack_32_2x16',
      ('fmax',
       ('fneg', ('unpack_32_2x16', ('pack_half_2x16_rtz_split', a, c))),
       ('fneg', ('unpack_32_2x16', ('pack_half_2x16_rtz_split', b, d))))),
     'options->vectorize_vec2_16bit'),

    (('pack_half_2x16_rtz_split',
      ('bcsel(is_used_once)', a, b, '#c'),
      ('bcsel(is_used_once)', a, d, '#e')),
     ('bcsel', a,
      ('pack_half_2x16_rtz_split', b, d),
      ('pack_half_2x16_rtz_split', c, e))),
    (('pack_half_2x16_rtz_split',
      ('bcsel(is_used_once)', a, '#b', c),
      ('bcsel(is_used_once)', a, '#d', e)),
     ('bcsel', a,
      ('pack_half_2x16_rtz_split', b, d),
      ('pack_half_2x16_rtz_split', c, e))),

    (('pack_half_2x16_rtz_split', ('b2f', 'a@1'), ('b2f', a)), ('bcsel', a, 0x3c003c00, 0)),

    (('pack_half_2x16_rtz_split', ('f2f32', 'a@16'), b),
     ('pack_32_2x16', ('vec2', ('fcanonicalize', a), ('f2f16_rtz', b)))),
    (('pack_half_2x16_rtz_split', a, ('f2f32', 'b@16')),
     ('pack_32_2x16', ('vec2', ('f2f16_rtz', a), ('fcanonicalize', b)))),

    (('pack_32_2x16_split', 'a(is_undef)', ('bcsel', b, '#c', d)),
     ('bcsel', b,
      ('pack_32_2x16_split', 0, c),
      ('pack_32_2x16_split', a, d)),
     'true', TestStatus.UNSUPPORTED),
    (('pack_32_2x16_split', 'a(is_undef)', ('bcsel', b, c, '#d')),
     ('bcsel', b,
      ('pack_32_2x16_split', a, c),
      ('pack_32_2x16_split', 0, d)),
     'true', TestStatus.UNSUPPORTED),
]


for N in (16, 32):
    for M in (8, 16):
        if M >= N:
            continue

        aN = f'a@{N}'
        u2uM = f'u2u{M}'
        i2iM = f'i2i{M}'

        for x in ('u', 'i'):
            x2xN = f'{x}2{x}{N}'
            extract_xM = f'extract_{x}{M}'

            x2xN_M_bits = f'{x2xN}(only_lower_{M}_bits_used)'
            extract_xM_M_bits = f'{extract_xM}(only_lower_{M}_bits_used)'

            optimizations += [
                ((x2xN_M_bits, (u2uM, aN)), a),
                ((extract_xM_M_bits, aN, 0), a),
            ]

            bcsel_M_bits = f'bcsel(only_lower_{M}_bits_used)'
            optimizations += [
                ((bcsel_M_bits, c, (x2xN, (u2uM, aN)), b), ('bcsel', c, a, b)),
                ((bcsel_M_bits, c, (x2xN, (i2iM, aN)), b), ('bcsel', c, a, b)),
                ((bcsel_M_bits, c, (extract_xM, aN, 0), b), ('bcsel', c, a, b)),
            ]

            for op in ('iadd', 'imul', 'iand', 'ior', 'ixor'):
                op_M_bits = f'{op}(only_lower_{M}_bits_used)'
                optimizations += [
                    ((op_M_bits, (x2xN, (u2uM, aN)), b), (op, a, b)),
                    ((op_M_bits, (x2xN, (i2iM, aN)), b), (op, a, b)),
                    ((op_M_bits, (extract_xM, aN, 0), b), (op, a, b)),
                ]


def fexp2i(exp, bits):
    if bits == 16:
        return ('i2i16', ('ishl', ('iadd', exp, 15), 10))
    if bits == 32:
        return ('ishl', ('iadd', exp, 127), 23)
    if bits == 64:
        return ('pack_64_2x32_split', 0, ('ishl', ('iadd', exp, 1023), 20))
    raise AssertionError('bits')


def ldexp(f, exp, bits):
    if bits == 16:
        exp = ('imin', ('imax', exp, -30), 30)
    elif bits == 32:
        exp = ('imin', ('imax', exp, -254), 254)
    elif bits == 64:
        exp = ('imin', ('imax', exp, -2046), 2046)
    else:
        raise AssertionError('bits')

    e1 = ('ishr', exp, 1)
    e2 = ('isub', exp, e1)
    pow2_1 = fexp2i(e1, bits)
    pow2_2 = fexp2i(e2, bits)
    return ('!fmul', ('!fmul', f, pow2_1), pow2_2)


optimizations += [
    (('ldexp@16', 'x', 'exp'), ldexp('x', 'exp', 16), 'options->lower_ldexp', TestStatus.UNSUPPORTED),
    (('ldexp@32', 'x', 'exp'), ldexp('x', 'exp', 32), 'options->lower_ldexp', TestStatus.UNSUPPORTED),
    (('ldexp@64', 'x', 'exp'), ldexp('x', 'exp', 64), 'options->lower_ldexp', TestStatus.UNSUPPORTED),
]


def bitfield_reverse_xcom2(u):
    step1 = ('iadd', ('ishl', u, 16), ('ushr', u, 16))
    step2 = ('iadd', ('iand', ('ishl', step1, 1), 0xaaaaaaaa), ('iand', ('ushr', step1, 1), 0x55555555))
    step3 = ('iadd', ('iand', ('ishl', step2, 2), 0xcccccccc), ('iand', ('ushr', step2, 2), 0x33333333))
    step4 = ('iadd', ('iand', ('ishl', step3, 4), 0xf0f0f0f0), ('iand', ('ushr', step3, 4), 0x0f0f0f0f))
    step5 = ('iadd(many-comm-expr)', ('iand', ('ishl', step4, 8), 0xff00ff00), ('iand', ('ushr', step4, 8), 0x00ff00ff))
    return step5


def bitfield_reverse_ue4(u):
    step1 = ('ior', ('ishl', u, 16), ('ushr', u, 16))
    step2 = ('ior', ('ishl', ('iand', step1, 0x00ff00ff), 8), ('ushr', ('iand', step1, 0xff00ff00), 8))
    step3 = ('ior', ('ishl', ('iand', step2, 0x0f0f0f0f), 4), ('ushr', ('iand', step2, 0xf0f0f0f0), 4))
    step4 = ('ior', ('ishl', ('iand', step3, 0x33333333), 2), ('ushr', ('iand', step3, 0xcccccccc), 2))
    step5 = ('ior(many-comm-expr)', ('ishl', ('iand', step4, 0x55555555), 1), ('ushr', ('iand', step4, 0xaaaaaaaa), 1))
    return step5


def bitfield_reverse_cp2077(u):
    step1 = ('ior', ('ishl', u, 16), ('ushr', u, 16))
    step2 = ('ior', ('iand', ('ishl', step1, 1), 0xaaaaaaaa), ('iand', ('ushr', step1, 1), 0x55555555))
    step3 = ('ior', ('iand', ('ishl', step2, 2), 0xcccccccc), ('iand', ('ushr', step2, 2), 0x33333333))
    step4 = ('ior', ('iand', ('ishl', step3, 4), 0xf0f0f0f0), ('iand', ('ushr', step3, 4), 0x0f0f0f0f))
    step5 = ('ior(many-comm-expr)', ('iand', ('ishl', step4, 8), 0xff00ff00), ('iand', ('ushr', step4, 8), 0x00ff00ff))
    return step5


optimizations += [
    (bitfield_reverse_xcom2('x@32'), ('bitfield_reverse', 'x'), '!options->lower_bitfield_reverse'),
    (bitfield_reverse_ue4('x@32'),   ('bitfield_reverse', 'x'), '!options->lower_bitfield_reverse'),
    (bitfield_reverse_cp2077('x@32'), ('bitfield_reverse', 'x'), '!options->lower_bitfield_reverse'),
]


def vkd3d_proton_packed_f2f16_rtz_lo(a, abs_a):
    packed_half = ('pack_half_2x16_rtz_split', a, 0)
    packed_half_minus1 = ('iadd', packed_half, 0xffffffff)
    f32_was_not_inf = ('fneu', abs_a, 0x7f800000)
    f16_is_now_inf = ('ieq', ('iand', packed_half, 0x7fff), 0x7c00)
    return ('bcsel', ('iand', f32_was_not_inf, f16_is_now_inf), packed_half_minus1, packed_half)


optimizations += [
    (vkd3d_proton_packed_f2f16_rtz_lo('x', ('fabs', 'x')), ('pack_half_2x16_rtz_split', 'x', 0)),
    (vkd3d_proton_packed_f2f16_rtz_lo('x(is_not_negative)', 'x'), ('pack_half_2x16_rtz_split', 'x', 0)),
    (vkd3d_proton_packed_f2f16_rtz_lo(('fneg', 'x'), ('fabs', 'x')), ('pack_half_2x16_rtz_split', ('fneg', 'x'), 0)),
]


def vkd3d_proton_msad():
    pattern = None
    for i in range(4):
        ref = ('extract_u8', 'a@32', i)
        src = ('extract_u8', 'b@32', i)
        sad = ('iabs', ('iadd', ref, ('ineg', src)))
        msad = ('bcsel', ('ieq', ref, 0), 0, sad)
        pattern = msad if pattern is None else ('iadd', pattern, msad)
    return (pattern[0] + '(many-comm-expr)', *pattern[1:])


optimizations += [
    (vkd3d_proton_msad(), ('msad_4x8', a, b, 0), 'options->has_msad'),
    (('iadd', ('msad_4x8', a, b, 0), c), ('msad_4x8', a, b, c)),
]


def vkd3d_proton_f2e4m3_ovfl(variant, x, nan):
    if variant == 0:
        cond = ('feq', ('fabs', x), float('inf'))
    elif variant == 1:
        cond = ('feq', f'{x}(is_not_negative)', float('inf'))
    elif variant == 2:
        cond = ('feq', f'{x}(is_not_positive)', -float('inf'))
    else:
        raise AssertionError('variant')
    return ('bcsel', cond, f'#{nan}(is_nan)', x)


for var in range(3):
    optimizations += [
        (('f2e4m3fn_sat', vkd3d_proton_f2e4m3_ovfl(var, a, b)),
         ('f2e4m3fn_satfn', a),
         'options->has_f2e4m3fn_satfn', TestStatus.UNSUPPORTED),
    ]

for var0, var1 in itertools.product(range(3), repeat=2):
    optimizations += [
        (('f2e4m3fn_sat',
          ('vec2',
           vkd3d_proton_f2e4m3_ovfl(var0, a, b),
           vkd3d_proton_f2e4m3_ovfl(var1, c, d))),
         ('f2e4m3fn_satfn', ('vec2', a, c)),
         'options->has_f2e4m3fn_satfn', TestStatus.UNSUPPORTED),
    ]


for ncomp in (2, 3, 4, 8, 16):
    status = TestStatus.UNSUPPORTED if ncomp >= 4 else TestStatus.PASS
    optimizations += [
        (('ball_iequal' + str(ncomp), ('ieq', a, b), ~0), ('ball_iequal' + str(ncomp), a, b), 'true', status),
        (('ball_iequal' + str(ncomp), ('feq', a, b), ~0), ('ball_fequal' + str(ncomp), a, b), 'true', status),
        (('bany_inequal' + str(ncomp), ('ine', a, b), 0), ('bany_inequal' + str(ncomp), a, b), 'true', status),
        (('bany_inequal' + str(ncomp), ('fneu', a, b), 0), ('bany_fnequal' + str(ncomp), a, b), 'true', status),
    ]


for op in ('flt', 'fge', 'feq'):
    optimizations += [
        (('iand', ('feq', a, a), (op, a, b)), ('!' + op, a, b)),
        (('iand', ('feq', a, a), (op, b, a)), ('!' + op, b, a)),
    ]


for op in ('feq', 'fneu', 'ieq', 'ine'):
    optimizations += [
        ((op, ('bcsel', 'a', '#b', '#c'), '#d'),
         ('bcsel', 'a', (op, 'b', 'd'), (op, 'c', 'd'))),
    ]


for op in ('flt', 'fge', 'ilt', 'ige', 'ult', 'uge'):
    optimizations += [
        ((op, ('bcsel', 'a', '#b', '#c'), '#d'),
         ('bcsel', 'a', (op, 'b', 'd'), (op, 'c', 'd'))),
        ((op, '#d', ('bcsel', a, '#b', '#c')),
         ('bcsel', 'a', (op, 'd', 'b'), (op, 'd', 'c'))),
    ]


for op in ('fadd', 'fmul', 'fmulz', 'iadd', 'imul'):
    optimizations += [
        ((op, ('bcsel(is_used_once)', a, '#b', c), '#d'),
         ('bcsel', a, (op, b, d), (op, c, d))),
    ]


optimizations += [
    (('umul_16x16', '#a(is_lower_half_zero)', 'b'), 0),
    (('imadsh_mix16', '#a@32(is_lower_half_zero)', 'b@32', 'c@32'), c),
    (('imadsh_mix16', 'a@32', '#b@32(is_upper_half_zero)', 'c@32'), c),
]


for op in ('flrp',):
    optimizations += [
        (('bcsel', a, (op + '(is_used_once)', b, c, d), (op, b, c, e)), (op, b, c, ('bcsel', a, d, e))),
        (('bcsel', a, (op, b, c, d), (op + '(is_used_once)', b, c, e)), (op, b, c, ('bcsel', a, d, e))),
        (('bcsel', a, (op + '(is_used_once)', b, c, d), (op, b, e, d)), (op, b, ('bcsel', a, c, e), d)),
        (('bcsel', a, (op, b, c, d), (op + '(is_used_once)', b, e, d)), (op, b, ('bcsel', a, c, e), d)),
        (('bcsel', a, (op + '(is_used_once)', b, c, d), (op, e, c, d)), (op, ('bcsel', a, b, e), c, d)),
        (('bcsel', a, (op, b, c, d), (op + '(is_used_once)', e, c, d)), (op, ('bcsel', a, b, e), c, d)),
    ]


for op in ('fmulz', 'fmul', 'iadd', 'imul', 'iand', 'ior', 'ixor', 'fmin', 'fmax', 'imin', 'imax', 'umin', 'umax'):
    optimizations += [
        (('bcsel', a, (op + '(is_used_once)', b, c), (op, b, 'd(is_not_const)')), (op, b, ('bcsel', a, c, d))),
        (('bcsel', a, (op + '(is_used_once)', b, 'c(is_not_const)'), (op, b, d)), (op, b, ('bcsel', a, c, d))),
        (('bcsel', a, (op, b, 'c(is_not_const)'), (op + '(is_used_once)', b, d)), (op, b, ('bcsel', a, c, d))),
        (('bcsel', a, (op, b, c), (op + '(is_used_once)', b, 'd(is_not_const)')), (op, b, ('bcsel', a, c, d))),
    ]


for op in ('fpow',):
    optimizations += [
        (('bcsel', a, (op + '(is_used_once)', b, c), (op, b, d)), (op, b, ('bcsel', a, c, d))),
        (('bcsel', a, (op, b, c), (op + '(is_used_once)', b, d)), (op, b, ('bcsel', a, c, d))),
        (('bcsel', a, (op + '(is_used_once)', b, c), (op, d, c)), (op, ('bcsel', a, b, d), c)),
        (('bcsel', a, (op, b, c), (op + '(is_used_once)', d, c)), (op, ('bcsel', a, b, d), c)),
    ]


for op in (
    'frcp', 'frsq', 'fsqrt', 'fexp2', 'flog2',
    'fsign', 'fsin', 'fcos',
    'fsin_amd', 'fcos_amd',
    'fneg', 'fabs', 'fcanonicalize',
):
    optimizations += [
        (('bcsel', c, (op + '(is_used_once)', a), (op + '(is_used_once)', b)),
         (op, ('bcsel', c, a, b))),
    ]


for op in ('ineg', 'iabs', 'inot', 'isign', 'fcanonicalize'):
    optimizations += [
        ((op, ('bcsel', c, '#a', '#b')), ('bcsel', c, (op, a), (op, b))),
    ]


optimizations.extend([
    (('fisnormal', 'a@16'), ('ult', 0xfff, ('iadd', ('ishl', a, 1), 0x800)), 'options->lower_fisnormal'),
    (('fisnormal', 'a@32'), ('ult', 0x1ffffff, ('iadd', ('ishl', a, 1), 0x1000000)), 'options->lower_fisnormal'),
    (('fisnormal', 'a@64'), ('ult', 0x3fffffffffffff, ('iadd', ('ishl', a, 1), 0x20000000000000)), 'options->lower_fisnormal'),
])


optimizations.extend([
    (('fquantize2f16', 'a@32'),
     ('bcsel',
      ('!flt', ('!fabs', a), math.ldexp(1.0, -14)),
      ('iand', a, 1 << 31),
      ('!f2f32', ('!f2f16_rtne', a))),
     'options->lower_fquantize2f16'),
])


for s in range(0, 31):
    mask = 0xffffffff << s
    optimizations.extend([
        (('bfi', mask, a, '#b'),
         ('iadd', ('ishl', a, s), ('iand', b, ~mask)),
         'options->avoid_ternary_with_two_constants'),
    ])


for op in ('fadd', 'fdiv', 'fmod', 'fmul', 'fpow', 'frem', 'fsub'):
    optimizations += [((op, '#a(is_nan)', b), NAN, 'true', TestStatus.XFAIL if op == 'fpow' else TestStatus.PASS)]
    optimizations += [((op, a, '#b(is_nan)'), NAN, 'true', TestStatus.XFAIL if op == 'fpow' else TestStatus.PASS)]


for op in ('ffma', 'flrp'):
    optimizations += [((op, '#a(is_nan)', b, c), NAN)]
    optimizations += [((op, a, '#b(is_nan)', c), NAN)]
    optimizations += [((op, a, b, '#c(is_nan)'), NAN)]


for op in ('fmin', 'fmax'):
    optimizations += [((op, '#a(is_nan)', b), b)]


optimizations += [(('ldexp', '#a(is_nan)', b), NAN, 'true', TestStatus.XFAIL)]


for op in ('fdot2', 'fdot3', 'fdot4', 'fdot5', 'fdot8', 'fdot16'):
    optimizations += [((op, '#a(is_any_comp_nan)', b), NAN)]


for op in ('feq', 'fge', 'flt'):
    optimizations += [((op, '#a(is_nan)', b), False)]
    optimizations += [((op, a, '#b(is_nan)'), False)]


optimizations += [(('fneu', '#a(is_nan)', b), True)]


for op in ('seq', 'sge', 'slt'):
    optimizations += [((op, '#a(is_nan)', b), 0.0)]
    optimizations += [((op, a, '#b(is_nan)'), 0.0)]


optimizations += [(('sne', '#a(is_nan)', b), 1.0)]


for i in range(2, 5):
    for T in ('f', 'u', 'i'):
        vec_inst = ('vec' + str(i),)
        indices = ('a', 'b', 'c', 'd')
        suffix_in = tuple((indices[j] + '@32') for j in range(i))

        to_16 = f'{T}2{T}16'
        to_mp = f'{T}2{T}mp'
        out_16 = tuple((to_16, indices[j]) for j in range(i))
        out_mp = tuple((to_mp, indices[j]) for j in range(i))

        optimizations += [
            ((to_16, vec_inst + suffix_in), vec_inst + out_16, '!options->vectorize_vec2_16bit'),
        ]
        if T in ('f', 'i'):
            optimizations += [
                ((to_mp, vec_inst + suffix_in), vec_inst + out_mp, '!options->vectorize_vec2_16bit', TestStatus.UNSUPPORTED),
            ]


for b2t, xne, xeq, zero, one in (('b2i', 'ine', 'ieq', 0, 1), ('b2f', 'fneu', 'feq', 0.0, 1.0)):
    optimizations += [
        ((xeq, (b2t, 'a@1'), zero), ('inot', a)),
        ((xeq, (b2t, 'a@1'), one), a),
        ((xne, (b2t, 'a@1'), zero), a),
        ((xne, (b2t, 'a@1'), one), ('inot', a)),
        ((xeq, (b2t, 'a@1'), '#b'),
         ('bcsel', (xeq, b, zero), ('inot', a), ('bcsel', (xeq, b, one), a, False))),
        ((xne, (b2t, 'a@1'), '#b'),
         ('bcsel', (xeq, b, zero), a, ('bcsel', (xeq, b, one), ('inot', a), True))),
    ]


before_ffma_optimizations = [
    (('~fmul(is_used_once)', ('fmul(is_used_once)', 'a(is_not_const)', '#b'), 'c(is_not_const)'), ('fmul', ('fmul', a, c), b)),
    (('imul(is_used_once)', ('imul(is_used_once)', 'a(is_not_const)', '#b'), 'c(is_not_const)'), ('imul', ('imul', a, c), b)),
    (('~fadd(is_used_once)', ('fadd(is_used_once)', 'a(is_not_const)', '#b'), 'c(is_not_const)'), ('fadd', ('fadd', a, c), b)),
    (('iadd(is_used_once)', ('iadd(is_used_once)', 'a(is_not_const)', '#b'), 'c(is_not_const)'), ('iadd', ('iadd', a, c), b)),

    (('~fadd', ('fmul', a, b), ('fmul', a, c)), ('fmul', a, ('fadd', b, c))),
    (('iadd', ('imul', a, b), ('imul', a, c)), ('imul', a, ('iadd', b, c))),
    (('~fadd', ('fneg', a), a), 0.0),
    (('iadd', ('ineg', a), a), 0),
    (('iadd', ('ineg', a), ('iadd', a, b)), b),
    (('iadd', a, ('iadd', ('ineg', a), b)), b),
    (('~fadd', ('fneg', a), ('fadd', a, b)), ('fcanonicalize', b)),
    (('~fadd', a, ('fadd', ('fneg', a), b)), ('fcanonicalize', b)),

    (('~flrp', ('fadd(is_used_once)', a, -1.0), ('fadd(is_used_once)', a,  1.0), d), ('fadd', ('flrp', -1.0,  1.0, d), a)),
    (('~flrp', ('fadd(is_used_once)', a,  1.0), ('fadd(is_used_once)', a, -1.0), d), ('fadd', ('flrp',  1.0, -1.0, d), a)),
    (('~flrp', ('fadd(is_used_once)', a, '#b'), ('fadd(is_used_once)', a, '#c'), d),
     ('fadd', ('fmul', d, ('fadd', c, ('fneg', b))), ('fadd', a, b))),
]

late_optimizations = [
    (('flt', ('fadd(is_used_once)', a, b), 0.0), ('flt', a, ('fneg', b))),
    (('flt', ('fneg(is_used_once)', ('fadd(is_used_once)', a, b)), 0.0), ('flt', ('fneg', a), b)),
    (('flt', 0.0, ('fadd(is_used_once)', a, b)), ('flt', ('fneg', a), b)),
    (('flt', 0.0, ('fneg(is_used_once)', ('fadd(is_used_once)', a, b))), ('flt', a, ('fneg', b))),

    (('~fge', ('fadd(is_used_once)', a, b), 0.0), ('fge', a, ('fneg', b))),
    (('~fge', ('fneg(is_used_once)', ('fadd(is_used_once)', a, b)), 0.0), ('fge', ('fneg', a), b)),
    (('~fge', 0.0, ('fadd(is_used_once)', a, b)), ('fge', ('fneg', a), b)),
    (('~fge', 0.0, ('fneg(is_used_once)', ('fadd(is_used_once)', a, b))), ('fge', a, ('fneg', b))),

    (('~feq', ('fadd(is_used_once)', a, b), 0.0), ('feq', a, ('fneg', b))),
    (('~fneu', ('fadd(is_used_once)', a, b), 0.0), ('fneu', a, ('fneg', b))),

    (('fge', ('fadd(is_used_once)', 'a(is_finite)', b), 0.0), ('fge', a, ('fneg', b))),
    (('fge', ('fneg(is_used_once)', ('fadd(is_used_once)', 'a(is_finite)', b)), 0.0), ('fge', ('fneg', a), b)),
    (('fge', 0.0, ('fadd(is_used_once)', 'a(is_finite)', b)), ('fge', ('fneg', a), b)),
    (('fge', 0.0, ('fneg(is_used_once)', ('fadd(is_used_once)', 'a(is_finite)', b))), ('fge', a, ('fneg', b))),
    (('feq', ('fadd(is_used_once)', 'a(is_finite)', b), 0.0), ('feq', a, ('fneg', b))),
    (('fneu', ('fadd(is_used_once)', 'a(is_finite)', b), 0.0), ('fneu', a, ('fneg', b))),

    *add_fabs_fneg((('iand',
                     ('fneu', 'ma', 'mb'),
                     ('iand', ('feq', a, a), ('feq', b, b))),
                    ('ior', ('!flt', 'ma', 'mb'), ('!flt', 'mb', 'ma'))),
                   {'ma': a, 'mb': b}),
    (('iand', ('fneu', a, 0.0), ('feq', a, a)), ('!flt', 0.0, ('fabs', a))),

    *add_fabs_fneg((('ior',
                     ('feq', 'ma', 'mb'),
                     ('ior', ('fneu', a, a), ('fneu', b, b))),
                    ('inot', ('ior', ('!flt', 'ma', 'mb'), ('!flt', 'mb', 'ma')))),
                   {'ma': a, 'mb': b}),
    (('ior', ('feq', a, 0.0), ('fneu', a, a)), ('inot', ('!flt', 0.0, ('fabs', a)))),

    *add_fabs_fneg((('ior', ('flt', 'ma', 'mb'), ('ior', ('fneu', a, a), ('fneu', b, b))),
                    ('inot', ('fge', 'ma', 'mb'))),
                   {'ma': a, 'mb': b}, False),
    *add_fabs_fneg((('ior', ('fge', 'ma', 'mb'), ('ior', ('fneu', a, a), ('fneu', b, b))),
                    ('inot', ('flt', 'ma', 'mb'))),
                   {'ma': a, 'mb': b}, False),

    *add_fabs_fneg((('ior', ('flt', 'ma', 'b(is_a_number)'), ('fneu', a, a)),
                    ('inot', ('fge', 'ma', b))),
                   {'ma': a}),
    *add_fabs_fneg((('ior', ('fge', 'ma', 'b(is_a_number)'), ('fneu', a, a)),
                    ('inot', ('flt', 'ma', b))),
                   {'ma': a}),
    *add_fabs_fneg((('ior', ('flt', 'a(is_a_number)', 'mb'), ('fneu', b, b)),
                    ('inot', ('fge', a, 'mb'))),
                   {'mb': b}),
    *add_fabs_fneg((('ior', ('fge', 'a(is_a_number)', 'mb'), ('fneu', b, b)),
                    ('inot', ('flt', a, 'mb'))),
                   {'mb': b}),

    *add_fabs_fneg((('iand', ('fneu', 'ma', 'b(is_a_number)'), ('feq', a, a)),
                    ('fneo', 'ma', b),
                    'options->has_fneo_fcmpu'),
                   {'ma': a}),
    *add_fabs_fneg((('ior', ('feq', 'ma', 'b(is_a_number)'), ('fneu', a, a)),
                    ('fequ', 'ma', b),
                    'options->has_fneo_fcmpu'),
                   {'ma': a}),

    (('ior', ('flt', a, b), ('flt', b, a)), ('fneo', a, b), 'options->has_fneo_fcmpu'),
    (('flt', 0.0, ('fabs', a)), ('fneo', 0.0, a), 'options->has_fneo_fcmpu'),

    (('ior', ('fneu', 'a@16', a), ('fneu', 'b@16', b)), ('funord', a, b), 'options->has_ford_funord'),
    (('iand', ('feq', 'a@16', a), ('feq', 'b@16', b)), ('ford', a, b), 'options->has_ford_funord'),
    (('ior', ('fneu', 'a@32', a), ('fneu', 'b@32', b)), ('funord', a, b), 'options->has_ford_funord'),
    (('iand', ('feq', 'a@32', a), ('feq', 'b@32', b)), ('ford', a, b), 'options->has_ford_funord'),
    (('ior', ('fneu', 'a@64', a), ('fneu', 'b@64', b)), ('funord', a, b), 'options->has_ford_funord'),
    (('iand', ('feq', 'a@64', a), ('feq', 'b@64', b)), ('ford', a, b), 'options->has_ford_funord'),

    (('inot', ('ford(is_used_once)', a, b)), ('funord', a, b)),
    (('inot', ('funord(is_used_once)', a, b)), ('ford', a, b)),
    (('inot', ('feq(is_used_once)', a, b)), ('fneu', a, b)),
    (('inot', ('fneu(is_used_once)', a, b)), ('feq', a, b)),
    (('inot', ('fequ(is_used_once)', a, b)), ('fneo', a, b)),
    (('inot', ('fneo(is_used_once)', a, b)), ('fequ', a, b)),
    (('inot', ('flt(is_used_once)', a, b)), ('fgeu', a, b), 'options->has_fneo_fcmpu'),
    (('inot', ('fgeu(is_used_once)', a, b)), ('flt', a, b)),
    (('inot', ('fge(is_used_once)', a, b)), ('fltu', a, b), 'options->has_fneo_fcmpu'),
    (('inot', ('fltu(is_used_once)', a, b)), ('fge', a, b)),

    (('fneg(is_only_used_as_float)', ('fneg', a)), a),

    (('iadd@32', ('imul(is_only_used_by_iadd)', a, b), c), ('imad', a, b, c), 'options->has_imad32'),

    (('udiv_aligned_4', a), ('ushr', a, 2)),
]


late_optimizations.extend([
    (('fadd@16', a, ('fneg', 'b')), ('fsub', 'a', 'b'), 'options->has_fsub'),
    (('fadd@32', a, ('fneg', 'b')), ('fsub', 'a', 'b'), 'options->has_fsub'),
    (('fadd@64', a, ('fneg', 'b')), ('fsub', 'a', 'b'),
     'options->has_fsub && !(options->lower_doubles_options & nir_lower_dsub)'),

    (('fneg', a), ('fmul', a, -1.0), 'options->lower_fneg'),
    (('iadd', a, ('ineg', 'b')), ('isub', 'a', 'b'), 'options->has_isub || options->lower_ineg'),
    (('ineg', a), ('isub', 0, a), 'options->lower_ineg'),
    (('iabs', a), ('imax', a, ('ineg', a)), 'options->lower_iabs'),
])


for s in (8, 16, 32, 64):
    cond = 'options->has_iadd3'
    if s == 64:
        cond += ' && !(options->lower_int64_options & nir_lower_iadd3_64)'
    iadd = f"iadd@{s}"

    late_optimizations.extend([
        ((iadd, ('iadd(is_used_once)', 'a(is_not_const)', 'b(is_not_const)'), 'c(is_not_const)'),
         ('iadd3', a, b, c), cond),
        ((iadd, ('iadd(is_used_once)', '#a(is_16_bits)', 'b(is_not_const)'), 'c(is_not_const)'),
         ('iadd3', a, b, c), cond),
        ((iadd, ('iadd(is_used_once)', 'a(is_not_const)', 'b(is_not_const)'), '#c(is_16_bits)'),
         ('iadd3', a, b, c), cond),

        ((iadd, ('ineg', ('iadd(is_used_once)', 'a(is_not_const)', 'b(is_not_const)')), 'c(is_not_const)'),
         ('iadd3', ('ineg', a), ('ineg', b), c), cond),
        ((iadd, ('ineg', ('iadd(is_used_once)', '#a(is_16_bits)', 'b(is_not_const)')), 'c(is_not_const)'),
         ('iadd3', ('ineg', a), ('ineg', b), c), cond),
        ((iadd, ('ineg', ('iadd(is_used_once)', 'a(is_not_const)', 'b(is_not_const)')), '#c(is_16_bits)'),
         ('iadd3', ('ineg', a), ('ineg', b), c), cond),

        ((iadd, ('ishl', a, 1), 'b(is_not_const)'), ('iadd3', a, a, b), cond),
        ((iadd, ('ishl', a, 1), '#b(is_16_bits)'), ('iadd3', a, a, b), cond),
        ((iadd, ('ineg', ('ishl', a, 1)), 'b(is_not_const)'), ('iadd3', ('ineg', a), ('ineg', a), b), cond),
        ((iadd, ('ineg', ('ishl', a, 1)), '#b(is_16_bits)'), ('iadd3', ('ineg', a), ('ineg', a), b), cond),

        ((f'ishl@{s}', ('iadd', a, '#b(is_2x_16_bits)'), 1), ('iadd3', a, a, ('iadd', b, b)), cond),
        ((f'ishl@{s}', ('ineg', ('iadd', a, '#b(is_neg2x_16_bits)')), 1),
         ('iadd3', ('ineg', a), ('ineg', a), ('ineg', ('iadd', b, b))), cond),
    ])

late_optimizations.extend([
    (('vec2(is_only_used_as_float)', ('fneg@16', a), b),
     ('fmul', ('vec2', a, b), ('vec2', -1.0, 1.0)), 'options->vectorize_vec2_16bit'),
    (('vec2(is_only_used_as_float)', a, ('fneg@16', b)),
     ('fmul', ('vec2', a, b), ('vec2', 1.0, -1.0)), 'options->vectorize_vec2_16bit'),

    (('fadd@32', ('fmul@32(is_used_once)', 'a@32', 'b@32'), 'c@32'),
     ('ffma@32', 'a', 'b', 'c'), 'options->fuse_ffma32'),
    (('fadd@16', ('fmul@16(is_used_once)', 'a@16', 'b@16'), 'c@16'),
     ('ffma@16', 'a', 'b', 'c'), 'options->fuse_ffma16'),

    (('~fadd@32', ('fmul@32(is_used_once)', 'a@32', 'b@32'), 'c@32'),
     ('ffma@32', 'a', 'b', 'c'), 'options->fuse_ffma32'),
    (('~fadd@16', ('fmul@16(is_used_once)', 'a@16', 'b@16'), 'c@16'),
     ('ffma@16', 'a', 'b', 'c'), 'options->fuse_ffma16'),

    (('fadd@32', ('fmulz@32(is_used_once)', 'a@32', 'b@32'), 'c@32'),
     ('ffmaz@32', 'a', 'b', 'c'), 'options->fuse_ffma32 && ' + has_fmulz),
    (('~fadd@32', ('fmulz@32(is_used_once)', 'a@32', 'b@32'), 'c@32'),
     ('ffmaz@32', 'a', 'b', 'c'), 'options->fuse_ffma32 && ' + has_fmulz),

    (('fadd@32', 'c@32', ('fmul@32(is_used_once)', 'a@32', 'b@32')),
     ('ffma@32', 'a', 'b', 'c'),
     'options->fuse_ffma32'),

    (('fadd@16', 'c@16', ('fmul@16(is_used_once)', 'a@16', 'b@16')),
     ('ffma@16', 'a', 'b', 'c'),
     'options->fuse_ffma16'),

    (('fadd@32', 'c@32', ('fmulz@32(is_used_once)', 'a@32', 'b@32')),
     ('ffmaz@32', 'a', 'b', 'c'),
     'options->fuse_ffma32 && ' + has_fmulz),

    (('fsub@32', 'c@32', ('fmul@32(is_used_once)', 'a@32', 'b@32')),
     ('ffma@32', ('fneg', 'a'), 'b', 'c'), 'options->fuse_ffma32'),
    (('~fsub@32', 'c@32', ('fmul@32(is_used_once)', 'a@32', 'b@32')),
     ('ffma@32', ('fneg', 'a'), 'b', 'c'), 'options->fuse_ffma32'),

    (('fsub@32', 'c@32', ('fmulz@32(is_used_once)', 'a@32', 'b@32')),
     ('ffmaz@32', ('fneg', 'a'), 'b', 'c'), 'options->fuse_ffma32 && ' + has_fmulz),
    (('~fsub@32', 'c@32', ('fmulz@32(is_used_once)', 'a@32', 'b@32')),
     ('ffmaz@32', ('fneg', 'a'), 'b', 'c'), 'options->fuse_ffma32 && ' + has_fmulz),

    (('fsub@16', 'c@16', ('fmul@16(is_used_once)', 'a@16', 'b@16')),
     ('ffma@16', ('fneg', 'a'), 'b', 'c'),
     'options->fuse_ffma16'),

    (('~fsub@16', 'c@16', ('fmul@16(is_used_once)', 'a@16', 'b@16')),
     ('ffma@16', ('fneg', 'a'), 'b', 'c'),
     'options->fuse_ffma16'),

    (('flt', '#b(is_gt_0_and_lt_1)', ('fsat(is_used_once)', a)), ('flt', b, a)),
    (('fge', ('fsat(is_used_once)', a), '#b(is_gt_0_and_lt_1)'), ('fge', a, b)),
    (('feq', ('fsat(is_used_once)', a), '#b(is_gt_0_and_lt_1)'), ('feq', a, b)),
    (('fneu', ('fsat(is_used_once)', a), '#b(is_gt_0_and_lt_1)'), ('fneu', a, b)),
    (('fge', ('fsat(is_used_once)', a), 1.0), ('fge', a, 1.0)),

    (('~fge', ('fmin(is_used_once)', ('fadd(is_used_once)', a, b), ('fadd', c, d)), 0.0),
     ('iand', ('fge', a, ('fneg', b)), ('fge', c, ('fneg', d)))),

    (('flt', ('fneg', a), ('fneg', b)), ('flt', b, a)),
    (('fge', ('fneg', a), ('fneg', b)), ('fge', b, a)),
    (('feq', ('fneg', a), ('fneg', b)), ('feq', b, a)),
    (('fneu', ('fneg', a), ('fneg', b)), ('fneu', b, a)),
    (('flt', ('fneg', a), -1.0), ('flt', 1.0, a)),
    (('flt', -1.0, ('fneg', a)), ('flt', a, 1.0)),
    (('fge', ('fneg', a), -1.0), ('fge', 1.0, a)),
    (('fge', -1.0, ('fneg', a)), ('fge', a, 1.0)),
    (('fneu', ('fneg', a), -1.0), ('fneu', 1.0, a)),
    (('feq', -1.0, ('fneg', a)), ('feq', a, 1.0)),

    (('ior', a, a), a),
    (('iand', a, a), a),

    (('~fadd', ('fneg(is_used_once)', ('fsat(is_used_once)', 'a(is_not_fmul)')), 1.0),
     ('fsat', ('fadd', 1.0, ('fneg', a)))),

    (('fsqrt', ('fsat(is_used_once)', 'a(cannot_add_output_modifier)')), ('fsat', ('fsqrt', a))),

    (('fdot2', a, b), ('fdot2_replicated', a, b), 'options->fdot_replicates'),
    (('fdot3', a, b), ('fdot3_replicated', a, b), 'options->fdot_replicates'),
    (('fdot4', a, b), ('fdot4_replicated', a, b), 'options->fdot_replicates'),
    (('fdph', a, b), ('fdph_replicated', a, b), 'options->fdot_replicates'),

    (('~flrp', ('fadd(is_used_once)', a, b), ('fadd(is_used_once)', a, c), d), ('fadd', ('flrp', b, c, d), a)),

    (('fround_even', a),
     ('bcsel',
      ('feq', ('ffract', a), 0.5),
      ('fadd', ('ffloor', ('fadd', a, 0.5)), 1.0),
      ('ffloor', ('fadd', a, 0.5))),
     'options->lower_fround_even'),

    (('~ffma@32', a, 2.0, -1.0), ('flrp', -1.0, 1.0, a), '!options->lower_flrp32'),
    (('~ffma@32', a, -2.0, -1.0), ('flrp', -1.0, 1.0, ('fneg', a)), '!options->lower_flrp32'),
    (('~ffma@32', a, -2.0, 1.0), ('flrp', 1.0, -1.0, a), '!options->lower_flrp32'),
    (('~ffma@32', a, 2.0, 1.0), ('flrp', 1.0, -1.0, ('fneg', a)), '!options->lower_flrp32'),

    (('~fadd@32', ('fmul(is_used_once)', 2.0, a), -1.0), ('flrp', -1.0, 1.0, a), '!options->lower_flrp32'),
    (('~fadd@32', ('fmul(is_used_once)', -2.0, a), -1.0), ('flrp', -1.0, 1.0, ('fneg', a)), '!options->lower_flrp32'),
    (('~fadd@32', ('fmul(is_used_once)', -2.0, a), 1.0), ('flrp', 1.0, -1.0, a), '!options->lower_flrp32'),
    (('~fadd@32', ('fmul(is_used_once)', 2.0, a), 1.0), ('flrp', 1.0, -1.0, ('fneg', a)), '!options->lower_flrp32'),

    (('~ffma@32', ('fadd', b, ('fneg', a)), a, a), ('flrp', a, b, a), '!options->lower_flrp32'),
    (('~ffma@32', a, 2.0, ('fneg', ('fmul', a, a))), ('flrp', a, 1.0, a), '!options->lower_flrp32'),
    (('~ffma@32', a, 2.0, ('fmul', ('fneg', a), a)), ('flrp', a, 1.0, a), '!options->lower_flrp32'),
    (('~ffma@32', a, ('fneg', a), ('fmul', 2.0, a)), ('flrp', a, 1.0, a), '!options->lower_flrp32'),
    (('~fmul@32', a, ('fadd', 2.0, ('fneg', a))), ('flrp', a, 1.0, a), '!options->lower_flrp32'),

    (('fmin', ('fadd(is_used_once)', '#c', a), ('fadd(is_used_once)', '#c', b)), ('fadd', c, ('fmin', a, b)), 'true', TestStatus.XFAIL),
    (('fmax', ('fadd(is_used_once)', '#c', a), ('fadd(is_used_once)', '#c', b)), ('fadd', c, ('fmax', a, b)), 'true', TestStatus.XFAIL),

    (('bcsel', ('feq', ('fsqrt', 'a(is_not_negative)'), 0.0), intBitsToFloat(0x7f7fffff), ('frsq', a)),
     ('fmin', ('frsq', a), intBitsToFloat(0x7f7fffff))),

    (('~fadd',
      ('ffma(is_used_once)', a, b,
       ('ffma(is_used_once)', c, d,
        ('ffma', e, 'f',
         ('fmul(is_used_once)', 'g(is_not_const_and_not_fsign)', 'h(is_not_const_and_not_fsign)')))),
      'i(is_not_const)'),
     ('ffma', a, b, ('ffma', c, d, ('ffma', e, 'f', ('ffma', 'g', 'h', 'i')))),
     '(info->stage != MESA_SHADER_VERTEX && info->stage != MESA_SHADER_GEOMETRY) && !options->intel_vec4'),

    (('~fadd',
      ('ffma(is_used_once)', a, b,
       ('ffma', c, d,
        ('fmul(is_used_once)', 'e(is_not_const_and_not_fsign)', 'f(is_not_const_and_not_fsign)'))),
      'g(is_not_const)'),
     ('ffma', a, b, ('ffma', c, d, ('ffma', e, 'f', 'g'))),
     '(info->stage != MESA_SHADER_VERTEX && info->stage != MESA_SHADER_GEOMETRY) && !options->intel_vec4'),

    (('~fadd',
      ('ffma(is_used_once)', a, b,
       ('fmul(is_used_once)', 'c(is_not_const_and_not_fsign)', 'd(is_not_const_and_not_fsign)')),
      'e(is_not_const)'),
     ('ffma', a, b, ('ffma', c, d, e)),
     '(info->stage != MESA_SHADER_VERTEX && info->stage != MESA_SHADER_GEOMETRY) && !options->intel_vec4'),

    (('~fadd',
      ('fneg', ('ffma(is_used_once)', a, b,
               ('ffma', c, d,
                ('fmul(is_used_once)', 'e(is_not_const_and_not_fsign)', 'f(is_not_const_and_not_fsign)')))),
      'g(is_not_const)'),
     ('ffma', ('fneg', a), b, ('ffma', ('fneg', c), d, ('ffma', ('fneg', e), 'f', 'g'))),
     '(info->stage != MESA_SHADER_VERTEX && info->stage != MESA_SHADER_GEOMETRY) && !options->intel_vec4'),

    (('~fadd',
      ('ffmaz(is_used_once)', a, b,
       ('ffmaz', c, d,
        ('fmulz(is_used_once)', 'e(is_not_const_and_not_fsign)', 'f(is_not_const_and_not_fsign)'))),
      'g(is_not_const)'),
     ('ffmaz', a, b, ('ffmaz', c, d, ('ffmaz', e, 'f', 'g'))),
     '(info->stage != MESA_SHADER_VERTEX && info->stage != MESA_SHADER_GEOMETRY) && !options->intel_vec4'),

    (('~fadd',
      ('ffmaz(is_used_once)', a, b,
       ('fmulz(is_used_once)', 'c(is_not_const_and_not_fsign)', 'd(is_not_const_and_not_fsign)')),
      'e(is_not_const)'),
     ('ffmaz', a, b, ('ffmaz', c, d, e)),
     '(info->stage != MESA_SHADER_VERTEX && info->stage != MESA_SHADER_GEOMETRY) && !options->intel_vec4'),

    (('~fadd',
      ('fneg', ('ffmaz(is_used_once)', a, b,
               ('ffmaz', c, d,
                ('fmulz(is_used_once)', 'e(is_not_const_and_not_fsign)', 'f(is_not_const_and_not_fsign)')))),
      'g(is_not_const)'),
     ('ffmaz', ('fneg', a), b, ('ffmaz', ('fneg', c), d, ('ffmaz', ('fneg', e), 'f', 'g'))),
     '(info->stage != MESA_SHADER_VERTEX && info->stage != MESA_SHADER_GEOMETRY) && !options->intel_vec4'),

    (('ubfe', a, b, 0), 0),
    (('ibfe', a, b, 0), 0),

    (('ubfe', a, '#b', '#c'),
     ('iand', ('ushr', 0xffffffff, ('ineg', c)), ('ushr', a, b)),
     'options->avoid_ternary_with_two_constants', TestStatus.XFAIL),

    (('ibfe', a, '#b', '#c'),
     ('ishr', ('ishl', a, ('ineg', ('iadd', b, c))), ('ineg', c)),
     'options->avoid_ternary_with_two_constants', TestStatus.XFAIL),

    (('ishl', a, 0), a),
    (('ishl', a, -32), a),
    (('ishr', a, 0), a),
    (('ishr', a, -32), a),
    (('ushr', a, 0), a),

    (('extract_i8', ('extract_i8', a, b), 0), ('extract_i8', a, b)),
    (('extract_i8', ('extract_u8', a, b), 0), ('extract_i8', a, b)),
    (('extract_u8', ('extract_i8', a, b), 0), ('extract_u8', a, b)),
    (('extract_u8', ('extract_u8', a, b), 0), ('extract_u8', a, b)),

    (('ine', ('iand', a, '#b(is_pos_power_of_two)'), 0), ('bitnz', a, ('find_lsb', b)), 'options->has_bit_test'),
    (('ieq', ('iand', a, '#b(is_pos_power_of_two)'), 0), ('bitz', a, ('find_lsb', b)), 'options->has_bit_test'),
    (('ine', ('iand', a, '#b(is_pos_power_of_two)'), b), ('bitz', a, ('find_lsb', b)), 'options->has_bit_test'),
    (('ieq', ('iand', a, '#b(is_pos_power_of_two)'), b), ('bitnz', a, ('find_lsb', b)), 'options->has_bit_test'),
    (('ine', ('iand', a, ('ishl', 1, b)), 0), ('bitnz', a, b), 'options->has_bit_test'),
    (('ieq', ('iand', a, ('ishl', 1, b)), 0), ('bitz', a, b), 'options->has_bit_test'),
    (('ine', ('iand', a, ('ishl', 1, b)), ('ishl', 1, b)), ('bitz', a, b), 'options->has_bit_test'),
    (('ieq', ('iand', a, ('ishl', 1, b)), ('ishl', 1, b)), ('bitnz', a, b), 'options->has_bit_test'),
    (('bitz', ('ushr', a, b), 0), ('bitz', a, b)),
    (('bitz', ('ishr', a, b), 0), ('bitz', a, b)),
    (('bitnz', ('ushr', a, b), 0), ('bitnz', a, b)),
    (('bitnz', ('ishr', a, b), 0), ('bitnz', a, b)),
    (('ine', ('ubfe', a, b, 1), 0), ('bitnz', a, b), 'options->has_bit_test'),
    (('ieq', ('ubfe', a, b, 1), 0), ('bitz', a, b), 'options->has_bit_test'),
    (('ine', ('ubfe', a, b, 1), 1), ('bitz', a, b), 'options->has_bit_test'),
    (('ieq', ('ubfe', a, b, 1), 1), ('bitnz', a, b), 'options->has_bit_test'),
    (('ine', ('ibfe', a, b, 1), 0), ('bitnz', a, b), 'options->has_bit_test'),
    (('ieq', ('ibfe', a, b, 1), 0), ('bitz', a, b), 'options->has_bit_test'),
    (('ine', ('ibfe', a, b, 1), -1), ('bitz', a, b), 'options->has_bit_test'),
    (('ieq', ('ibfe', a, b, 1), -1), ('bitnz', a, b), 'options->has_bit_test'),
    (('inot', ('bitnz', a, b)), ('bitz', a, b)),
    (('inot', ('bitz', a, b)), ('bitnz', a, b)),
    (('bitnz', ('inot', a), b), ('bitz', a, b)),
    (('bitz', ('inot', a), b), ('bitnz', a, b)),
])


late_optimizations += [
    (('fcanonicalize', a), ('fmul', a, 1.0), '!options->has_fcanonicalize'),
]


for N in (16, 32):
    aN = f'a@{N}'
    for x in ('u', 'i'):
        x2xN = f'{x}2{x}{N}'
        for M, lower_opt in ((8, '!options->lower_extract_byte'), (16, '!options->lower_extract_word')):
            if M >= N:
                continue
            extract_xM = f'extract_{x}{M}'
            u2uM = f'u2u{M}'
            i2iM = f'i2i{M}'
            late_optimizations.extend([
                ((x2xN, (u2uM, aN)), (extract_xM, a, 0), lower_opt),
                ((x2xN, (i2iM, aN)), (extract_xM, a, 0), lower_opt),
            ])


late_optimizations.extend([
    *((('ishl', ('extract_u8', 'a@32', 0), 8 * i), ('insert_u8', a, i), '!options->lower_insert_byte') for i in range(1, 4)),
    *((('iand', ('ishl', 'a@32', 8 * i), 0xff << (8 * i)), ('insert_u8', a, i), '!options->lower_insert_byte') for i in range(1, 4)),
    (('ishl', 'a@32', 24), ('insert_u8', a, 3), '!options->lower_insert_byte'),

    (('ishl', 'a@32', 16), ('insert_u16', a, 1), '!options->lower_insert_word'),

    (('insert_u8', ('extract_u8', 'a', 0), b), ('insert_u8', a, b)),
    (('insert_u16', ('extract_u16', 'a', 0), b), ('insert_u16', a, b)),
])


for s in (16, 32, 64):
    late_optimizations.extend([
        (('~fadd@{}'.format(s), 1.0, ('fmul(is_used_once)', c, ('fadd', b, -1.0))),
         ('fadd', ('fadd', 1.0, ('fneg', c)), ('fmul', b, c)), f'options->lower_flrp{s}'),
        (('bcsel', a, 0, ('b2f{}'.format(s), ('inot', 'b@bool'))), ('b2f{}'.format(s), ('inot', ('ior', a, b)))),
    ])


late_optimizations += [
    (('bcsel', a, ('fadd(is_used_once)', b, c), ('fadd', b, d)), ('fadd', b, ('bcsel', a, c, d))),
    (('bcsel', a, ('fadd', b, c), ('fadd(is_used_once)', b, d)), ('fadd', b, ('bcsel', a, c, d))),
]


for op in ('ffma', 'ffmaz'):
    late_optimizations += [
        (('bcsel', a, (op + '(is_used_once)', b, c, d), (op, b, c, e)), (op, b, c, ('bcsel', a, d, e))),
        (('bcsel', a, (op, b, c, d), (op + '(is_used_once)', b, c, e)), (op, b, c, ('bcsel', a, d, e))),

        (('bcsel', a, (op + '(is_used_once)', b, c, d), (op, b, e, d)), (op, b, ('bcsel', a, c, e), d)),
        (('bcsel', a, (op, b, c, d), (op + '(is_used_once)', b, e, d)), (op, b, ('bcsel', a, c, e), d)),
    ]


late_optimizations += [
    (('fmulz@32', a, b),
     ('bcsel', ('feq', ('fmin', ('fabs', a), ('fabs', b)), 0.0), 0.0, ('fmul', a, b)),
     'options->lower_fmulz_with_abs_min'),
    (('ffmaz@32', a, b, c),
     ('bcsel', ('feq', ('fmin', ('fabs', a), ('fabs', b)), 0.0), c, ('ffma@32', a, b, c)),
     'options->lower_fmulz_with_abs_min'),
]


for op in ('fabs', 'fceil', 'fcos', 'fexp2', 'ffloor', 'ffract', 'flog2', 'fneg',
           'frcp', 'fround_even', 'frsq', 'fsat', 'fsign', 'fsin', 'fsqrt'):
    late_optimizations += [(('~f2f32', (op, ('f2fmp', a))), (op, a), 'true', TestStatus.UNSUPPORTED)]


for op in ('fadd', 'fdiv', 'fmax', 'fmin', 'fmod', 'fmul', 'fpow', 'frem'):
    late_optimizations += [(('~f2f32', (op, ('f2fmp', a), ('f2fmp', b))), (op, a, b), 'true', TestStatus.UNSUPPORTED)]


for op in ('ffma', 'flrp'):
    late_optimizations += [(('~f2f32', (op, ('f2fmp', a), ('f2fmp', b), ('f2fmp', c))), (op, a, b, c), 'true', TestStatus.UNSUPPORTED)]


for op in ('feq', 'fge', 'flt', 'fneu'):
    late_optimizations += [(('~' + op, ('f2fmp', a), ('f2fmp', b)), (op, a, b), 'true', TestStatus.UNSUPPORTED)]


for sz in (16,):
    fadd = f'fadd@{sz}'
    fmul = f'fmul@{sz}(is_used_once)'
    ffma = f'ffma@{sz}'
    option = f'options->fuse_ffma{sz}'
    late_optimizations.extend([
        ((fadd, (fmul, a, b), c), (ffma, a, b, c), option),
        ((fadd, ('fneg(is_used_once)', (fmul, a, b)), c), (ffma, ('fneg', a), b, c), option),
        ((fadd, ('fabs(is_used_once)', (fmul, a, b)), c), (ffma, ('fabs', a), ('fabs', b), c), option),
    ])


late_optimizations += [
    (('f2fmp', a), ('f2f16', a), '!options->preserve_mediump', TestStatus.UNSUPPORTED),
    (('f2imp', a), ('f2i16', a), '!options->preserve_mediump', TestStatus.UNSUPPORTED),
    (('f2ump', a), ('f2u16', a), '!options->preserve_mediump', TestStatus.UNSUPPORTED),
    (('i2imp', a), ('i2i16', a), '!options->preserve_mediump', TestStatus.UNSUPPORTED),
    (('i2fmp', a), ('i2f16', a), '!options->preserve_mediump', TestStatus.UNSUPPORTED),
    (('i2imp', a), ('u2u16', a), '!options->preserve_mediump', TestStatus.UNSUPPORTED),
    (('u2fmp', a), ('u2f16', a), '!options->preserve_mediump', TestStatus.UNSUPPORTED),

    (('fisfinite', a), ('flt', ('fabs', a), float('inf'))),

    (('f2f16', a), ('f2f16_rtz', a),
     'options->force_f2f16_rtz && !nir_is_rounding_mode_rtne(info->float_controls_execution_mode, 16)'),

    (('fcsel', ('slt', 0, a), b, c), ('fcsel_gt', a, b, c), 'options->has_fused_comp_and_csel'),
    (('fcsel', ('slt', a, 0), b, c), ('fcsel_gt', ('fneg', a), b, c), 'options->has_fused_comp_and_csel'),
    (('fcsel', ('sge', a, 0), b, c), ('fcsel_ge', a, b, c), 'options->has_fused_comp_and_csel'),
    (('fcsel', ('sge', 0, a), b, c), ('fcsel_ge', ('fneg', a), b, c), 'options->has_fused_comp_and_csel'),

    (('bcsel', ('ilt', 0, 'a@32'), 'b@32', 'c@32'), ('i32csel_gt', a, b, c),
     'options->has_fused_comp_and_csel && !options->no_integers'),
    (('bcsel', ('ilt', 'a@32', 0), 'b@32', 'c@32'), ('i32csel_ge', a, c, b),
     'options->has_fused_comp_and_csel && !options->no_integers'),
    (('bcsel', ('ige', 'a@32', 0), 'b@32', 'c@32'), ('i32csel_ge', a, b, c),
     'options->has_fused_comp_and_csel && !options->no_integers'),
    (('bcsel', ('ige', 0, 'a@32'), 'b@32', 'c@32'), ('i32csel_gt', a, c, b),
     'options->has_fused_comp_and_csel && !options->no_integers'),

    (('bcsel', ('flt', 0, 'a@32'), 'b@32', 'c@32'), ('fcsel_gt', a, b, c), 'options->has_fused_comp_and_csel'),
    (('bcsel', ('flt', 'a@32', 0), 'b@32', 'c@32'), ('fcsel_gt', ('fneg', a), b, c), 'options->has_fused_comp_and_csel'),
    (('bcsel', ('fge', 'a@32', 0), 'b@32', 'c@32'), ('fcsel_ge', a, b, c), 'options->has_fused_comp_and_csel'),
    (('bcsel', ('fge', 0, 'a@32'), 'b@32', 'c@32'), ('fcsel_ge', ('fneg', a), b, c), 'options->has_fused_comp_and_csel'),
]


for s in (16, 32, 64):
    late_optimizations.extend([
        (('bcsel@{}'.format(s), ('ieq', 0, f'a@{s}'), f'b@{s}', f'c@{s}'),
         ('icsel_eqz', a, b, c), f'options->has_icsel_eqz{s} && !options->no_integers'),
        (('bcsel@{}'.format(s), ('ine', 0, f'a@{s}'), f'b@{s}', f'c@{s}'),
         ('icsel_eqz', a, c, b), f'options->has_icsel_eqz{s} && !options->no_integers'),
    ])


late_optimizations += [
    (('f2i32', ('fround_even(is_used_once)', 'a@32')), ('f2i32_rtne', a), 'options->has_f2i32_rtne'),
]


distribute_src_mods = [
    (('fmul', ('fneg', a), ('fneg', b)), ('fmul', a, b)),
    (('ffma', ('fneg', a), ('fneg', b), c), ('ffma', a, b, c)),
    (('fdot2_replicated', ('fneg', a), ('fneg', b)), ('fdot2_replicated', a, b)),
    (('fdot3_replicated', ('fneg', a), ('fneg', b)), ('fdot3_replicated', a, b)),
    (('fdot4_replicated', ('fneg', a), ('fneg', b)), ('fdot4_replicated', a, b)),
    (('fneg(is_only_used_as_float)', ('fneg', a)), a),

    (('fneg', ('fmul(is_used_once)', a, b)), ('fmul', ('fneg', a), b)),
    (('fabs', ('fmul(is_used_once)', a, b)), ('fmul', ('fabs', a), ('fabs', b))),

    (('fneg', ('ffma(is_used_once,nsz)', a, b, c)), ('ffma', ('fneg', a), b, ('fneg', c))),
    (('fneg', ('flrp(is_used_once)', a, b, c)), ('flrp', ('fneg', a), ('fneg', b), c), 'true', TestStatus.XFAIL),
    (('fneg', ('fadd(is_used_once,nsz)', a, b)), ('fadd', ('fneg', a), ('fneg', b))),

    (('fneg', ('fmin(is_used_once)', a, b)), ('fmax', ('fneg', a), ('fneg', b))),
    (('fneg', ('fmax(is_used_once)', a, b)), ('fmin', ('fneg', a), ('fneg', b))),

    (('fneg', ('fdot2_replicated(is_used_once)', a, b)), ('fdot2_replicated', ('fneg', a), b), 'true', TestStatus.XFAIL),
    (('fneg', ('fdot3_replicated(is_used_once)', a, b)), ('fdot3_replicated', ('fneg', a), b), 'true', TestStatus.XFAIL),
    (('fneg', ('fdot4_replicated(is_used_once)', a, b)), ('fdot4_replicated', ('fneg', a), b), 'true', TestStatus.XFAIL),

    (('fneg', ('fdph_replicated(is_used_once)', a, b)), ('fdph_replicated', a, ('fneg', b)), 'true', TestStatus.XFAIL),

    (('fneg', ('fsign(is_used_once)', a)), ('fsign', ('fneg', a))),
    (('fabs', ('fsign(is_used_once)', a)), ('fsign', ('fabs', a))),
]


mat_mul_optimizations = []
for t in ('f', 'i'):
    add_first = f'~{t}add(many-comm-expr)'
    add_used_once = f'~{t}add(is_used_once)'
    add = f'~{t}add'
    mul = f'~{t}mul'

    step1 = (add_used_once, (add, (add, (mul, 'a', 'q'), (mul, 'b', 'u')), (mul, 'c', 'y')), (mul, 'd', 'cc'))
    step2 = (add_used_once, (add, (add, (mul, 'a', 'r'), (mul, 'b', 'v')), (mul, 'c', 'z')), (mul, 'd', 'dd'))
    step3 = (add_used_once, (add, (add, (mul, 'a', 's'), (mul, 'b', 'w')), (mul, 'c', 'aa')), (mul, 'd', 'ee'))
    step4 = (add_used_once, (add, (add, (mul, 'a', 't'), (mul, 'b', 'x')), (mul, 'c', 'bb')), (mul, 'd', 'ff'))

    step5 = (add, (add, (add, (mul, 'q', 'gg'), (mul, 'r', 'hh')), (mul, 's', 'ii')), (mul, 't', 'jj'))
    step6 = (add, (add, (add, (mul, 'u', 'gg'), (mul, 'v', 'hh')), (mul, 'w', 'ii')), (mul, 'x', 'jj'))
    step7 = (add, (add, (add, (mul, 'y', 'gg'), (mul, 'z', 'hh')), (mul, 'aa', 'ii')), (mul, 'bb', 'jj'))
    step8 = (add, (add, (add, (mul, 'cc', 'gg'), (mul, 'dd', 'hh')), (mul, 'ee', 'ii')), (mul, 'ff', 'jj'))

    mat_mul_optimizations += [
        ((add_first, (add, (add, (mul, step1, 'gg'), (mul, step2, 'hh')), (mul, step3, 'ii')), (mul, step4, 'jj')),
         (add, (add, (add, (mul, step5, 'a'), (mul, step6, 'b')), (mul, step7, 'c')), (mul, step8, 'd'))),
        ((add_first, (add, (add, (mul, 'gg', step1), (mul, 'hh', step2)), (mul, 'ii', step3)), (mul, 'jj', step4)),
         (add, (add, (add, (mul, step5, 'a'), (mul, step6, 'b')), (mul, step7, 'c')), (mul, step8, 'd'))),
    ]

    step5_no_w_mul = (add, (add, (add, (mul, 'q', 'gg'), (mul, 'r', 'hh')), (mul, 's', 'ii')), 't')
    step6_no_w_mul = (add, (add, (add, (mul, 'u', 'gg'), (mul, 'v', 'hh')), (mul, 'w', 'ii')), 'x')
    step7_no_w_mul = (add, (add, (add, (mul, 'y', 'gg'), (mul, 'z', 'hh')), (mul, 'aa', 'ii')), 'bb')
    step8_no_w_mul = (add, (add, (add, (mul, 'cc', 'gg'), (mul, 'dd', 'hh')), (mul, 'ee', 'ii')), 'ff')

    mat_mul_optimizations += [
        ((add_first, (add, (add, (mul, step1, 'gg'), (mul, step2, 'hh')), (mul, step3, 'ii')), step4),
         (add, (add, (add, (mul, step5_no_w_mul, 'a'), (mul, step6_no_w_mul, 'b')), (mul, step7_no_w_mul, 'c')),
          (mul, step8_no_w_mul, 'd'))),
    ]

    step5_zero_z_no_w_mul = (add, (add, (mul, 'q', 'gg'), (mul, 'r', 'hh')), 't')
    step6_zero_z_no_w_mul = (add, (add, (mul, 'u', 'gg'), (mul, 'v', 'hh')), 'x')
    step7_zero_z_no_w_mul = (add, (add, (mul, 'y', 'gg'), (mul, 'z', 'hh')), 'bb')
    step8_zero_z_no_w_mul = (add, (add, (mul, 'cc', 'gg'), (mul, 'dd', 'hh')), 'ff')

    mat_mul_optimizations += [
        ((add_first, (add, (mul, step1, 'gg'), step4), (mul, step2, 'hh')),
         (add, (add, (add, (mul, step5_zero_z_no_w_mul, 'a'), (mul, step6_zero_z_no_w_mul, 'b')),
                    (mul, step7_zero_z_no_w_mul, 'c')),
          (mul, step8_zero_z_no_w_mul, 'd'))),
    ]


before_lower_int64_optimizations = [
    (('ishl', ('i2i64', a), b),
     ('bcsel',
      ('ieq', ('iand', b, 63), 0), ('i2i64', a),
      ('bcsel',
       ('ilt', ('iand', b, 63), 32),
       ('pack_64_2x32_split',
        ('ishl', ('i2i32', a), b),
        ('ishr', ('i2i32', a), ('iadd', ('ineg', b), 32))),
       ('pack_64_2x32_split',
        0,
        ('ishl', ('i2i32', a), ('iabs', ('iadd', ('ineg', b), 32)))))),
     '(options->lower_int64_options & nir_lower_shift64) != 0', TestStatus.XFAIL),

    (('ishl', ('u2u64', a), b),
     ('bcsel',
      ('ieq', ('iand', b, 63), 0), ('u2u64', a),
      ('bcsel',
       ('ilt', ('iand', b, 63), 32),
       ('pack_64_2x32_split',
        ('ishl', ('u2u32', a), b),
        ('ushr', ('u2u32', a), ('iadd', ('ineg', b), 32))),
       ('pack_64_2x32_split',
        0,
        ('ishl', ('u2u32', a), ('iabs', ('iadd', ('ineg', b), 32)))))),
     '(options->lower_int64_options & nir_lower_shift64) != 0', TestStatus.XFAIL),

    (('iadd@64', ('ineg', a), ('ineg(is_used_once)', b)), ('isub', ('ineg', a), b),
     '(options->lower_int64_options & nir_lower_ineg64) != 0'),
    (('iadd@64', a, ('ineg', b)), ('isub', a, b), '(options->lower_int64_options & nir_lower_ineg64) != 0'),
    (('isub@64', a, ('ineg', b)), ('iadd', a, b), '(options->lower_int64_options & nir_lower_ineg64) != 0'),
    (('isub@64', ('ineg', a), ('ineg', b)), ('isub', b, a), '(options->lower_int64_options & nir_lower_ineg64) != 0'),

    (('imul@64', ('ineg', a), ('ineg', b)), ('imul', a, b)),
    (('idiv@64', ('ineg', a), ('ineg', b)), ('idiv', a, b), 'true', TestStatus.XFAIL),

    (('iadd', ('i2i64', a), ('i2i64', a)), ('ishl', ('i2i64', a), 1)),
    (('iadd', ('u2u64', a), ('u2u64', a)), ('ishl', ('u2u64', a), 1)),
]


integer_promotion_optimizations = []
for s in (8, 16):
    u2u = f'u2u{s}'
    aN = f'a@{s}'
    bN = f'b@{s}'

    for op in ('ineg', 'inot'):
        integer_promotion_optimizations.extend([
            ((u2u, (op, 'a@32')), (op, (u2u, a))),
        ])

    for op in ('iadd', 'imul', 'iand', 'ior', 'ixor'):
        integer_promotion_optimizations.extend([
            ((u2u, (op, 'a@32', 'b@32')), (op, (u2u, a), (u2u, b))),
        ])

    for op in ('idiv', 'irem'):
        integer_promotion_optimizations.extend([
            ((u2u, (op, ('i2i32', aN), ('i2i32', bN))), (op, a, b)),
        ])


parser = argparse.ArgumentParser()
parser.add_argument('--out', required=True)
parser.add_argument('--out-tests')
args = parser.parse_args()

build_tests = args.out_tests is not None

passes = [
    nir_algebraic.AlgebraicPass(
        "nir_opt_algebraic",
        optimizations,
        build_tests=build_tests,
    ),
    nir_algebraic.AlgebraicPass(
        "nir_opt_algebraic_before_ffma",
        before_ffma_optimizations,
        build_tests=build_tests,
    ),
    nir_algebraic.AlgebraicPass(
        "nir_opt_algebraic_before_lower_int64",
        before_lower_int64_optimizations,
        build_tests=build_tests,
    ),
    nir_algebraic.AlgebraicPass(
        "nir_opt_algebraic_late",
        late_optimizations,
        build_tests=build_tests,
    ),
    nir_algebraic.AlgebraicPass(
        "nir_opt_algebraic_distribute_src_mods",
        distribute_src_mods,
        build_tests=build_tests,
    ),
    nir_algebraic.AlgebraicPass(
        "nir_opt_algebraic_integer_promotion",
        integer_promotion_optimizations,
        build_tests=build_tests,
    ),
    nir_algebraic.AlgebraicPass(
        "nir_opt_reassociate_matrix_mul",
        mat_mul_optimizations,
        build_tests=build_tests,
    ),
]

if build_tests:
    with open(args.out_tests, "w", encoding="utf-8") as f:
        for p in passes:
            f.write(p.render_tests())

with open(args.out, "w", encoding="utf-8") as f:
    for p in passes:
        f.write(p.render())
