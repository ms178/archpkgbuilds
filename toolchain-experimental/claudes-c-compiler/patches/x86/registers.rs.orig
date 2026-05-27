use super::super::parser::*;

/// Register encoding (3-bit register number in ModR/M and SIB).
pub(crate) fn reg_num(name: &str) -> Option<u8> {
    match name {
        "al" | "ax" | "eax" | "rax" | "xmm0" | "st" | "st(0)" | "mm0" | "es" | "ymm0" => Some(0),
        "cl" | "cx" | "ecx" | "rcx" | "xmm1" | "st(1)" | "mm1" | "cs" | "ymm1" => Some(1),
        "dl" | "dx" | "edx" | "rdx" | "xmm2" | "st(2)" | "mm2" | "ss" | "ymm2" => Some(2),
        "bl" | "bx" | "ebx" | "rbx" | "xmm3" | "st(3)" | "mm3" | "ds" | "ymm3" => Some(3),
        "ah" | "spl" | "sp" | "esp" | "rsp" | "xmm4" | "st(4)" | "mm4" | "fs" | "ymm4" => Some(4),
        "ch" | "bpl" | "bp" | "ebp" | "rbp" | "xmm5" | "st(5)" | "mm5" | "gs" | "ymm5" => Some(5),
        "dh" | "sil" | "si" | "esi" | "rsi" | "xmm6" | "st(6)" | "mm6" | "ymm6" => Some(6),
        "bh" | "dil" | "di" | "edi" | "rdi" | "xmm7" | "st(7)" | "mm7" | "ymm7" => Some(7),
        "r8b" | "r8w" | "r8d" | "r8" | "xmm8" | "ymm8" => Some(0),
        "r9b" | "r9w" | "r9d" | "r9" | "xmm9" | "ymm9" => Some(1),
        "r10b" | "r10w" | "r10d" | "r10" | "xmm10" | "ymm10" => Some(2),
        "r11b" | "r11w" | "r11d" | "r11" | "xmm11" | "ymm11" => Some(3),
        "r12b" | "r12w" | "r12d" | "r12" | "xmm12" | "ymm12" => Some(4),
        "r13b" | "r13w" | "r13d" | "r13" | "xmm13" | "ymm13" => Some(5),
        "r14b" | "r14w" | "r14d" | "r14" | "xmm14" | "ymm14" => Some(6),
        "r15b" | "r15w" | "r15d" | "r15" | "xmm15" | "ymm15" => Some(7),
        _ => None,
    }
}

/// Is this an MMX register?
pub(crate) fn is_mmx(name: &str) -> bool {
    name.starts_with("mm") && !name.starts_with("mmx")
        && name.len() <= 3
        && name.as_bytes().get(2).is_some_and(|c| c.is_ascii_digit())
}

/// Is this a segment register?
pub(crate) fn is_segment_reg(name: &str) -> bool {
    matches!(name, "es" | "cs" | "ss" | "ds" | "fs" | "gs")
}

pub(crate) fn is_control_reg(name: &str) -> bool {
    matches!(name, "cr0" | "cr2" | "cr3" | "cr4" | "cr8")
}

pub(crate) fn control_reg_num(name: &str) -> Option<u8> {
    match name {
        "cr0" => Some(0),
        "cr2" => Some(2),
        "cr3" => Some(3),
        "cr4" => Some(4),
        "cr8" => Some(8),
        _ => None,
    }
}

pub(crate) fn is_debug_reg(name: &str) -> bool {
    matches!(name, "db0" | "db1" | "db2" | "db3" | "db4" | "db5" | "db6" | "db7"
                  | "dr0" | "dr1" | "dr2" | "dr3" | "dr4" | "dr5" | "dr6" | "dr7")
}

pub(crate) fn debug_reg_num(name: &str) -> Option<u8> {
    match name {
        "db0" | "dr0" => Some(0),
        "db1" | "dr1" => Some(1),
        "db2" | "dr2" => Some(2),
        "db3" | "dr3" => Some(3),
        "db4" | "dr4" => Some(4),
        "db5" | "dr5" => Some(5),
        "db6" | "dr6" => Some(6),
        "db7" | "dr7" => Some(7),
        _ => None,
    }
}

/// Is this a YMM register?
pub(crate) fn is_ymm(name: &str) -> bool {
    name.starts_with("ymm")
}

/// Does this register need the REX.B/R/X extension bit?
pub(crate) fn needs_rex_ext(name: &str) -> bool {
    name.starts_with("r8") || name.starts_with("r9") || name.starts_with("r10")
        || name.starts_with("r11") || name.starts_with("r12") || name.starts_with("r13")
        || name.starts_with("r14") || name.starts_with("r15")
        || name.starts_with("xmm8") || name.starts_with("xmm9")
        || name.starts_with("xmm10") || name.starts_with("xmm11")
        || name.starts_with("xmm12") || name.starts_with("xmm13")
        || name.starts_with("xmm14") || name.starts_with("xmm15")
        || name.starts_with("ymm8") || name.starts_with("ymm9")
        || name.starts_with("ymm10") || name.starts_with("ymm11")
        || name.starts_with("ymm12") || name.starts_with("ymm13")
        || name.starts_with("ymm14") || name.starts_with("ymm15")
}

/// Does this register need the VEX.B extension bit? Same as REX ext but for VEX-encoded instructions.
pub(crate) fn needs_vex_ext(name: &str) -> bool {
    needs_rex_ext(name)
}

/// Is this a 64-bit GP register?
pub(crate) fn is_reg64(name: &str) -> bool {
    matches!(name, "rax" | "rcx" | "rdx" | "rbx" | "rsp" | "rbp" | "rsi" | "rdi"
        | "r8" | "r9" | "r10" | "r11" | "r12" | "r13" | "r14" | "r15")
}

/// Is this a 32-bit GP register?
pub(crate) fn is_reg32(name: &str) -> bool {
    matches!(name, "eax" | "ecx" | "edx" | "ebx" | "esp" | "ebp" | "esi" | "edi"
        | "r8d" | "r9d" | "r10d" | "r11d" | "r12d" | "r13d" | "r14d" | "r15d")
}

/// Is this a 16-bit GP register?
pub(crate) fn is_reg16(name: &str) -> bool {
    matches!(name, "ax" | "cx" | "dx" | "bx" | "sp" | "bp" | "si" | "di"
        | "r8w" | "r9w" | "r10w" | "r11w" | "r12w" | "r13w" | "r14w" | "r15w")
}

/// Is this an 8-bit GP register?
pub(crate) fn is_reg8(name: &str) -> bool {
    matches!(name, "al" | "cl" | "dl" | "bl" | "ah" | "ch" | "dh" | "bh"
        | "spl" | "bpl" | "sil" | "dil"
        | "r8b" | "r9b" | "r10b" | "r11b" | "r12b" | "r13b" | "r14b" | "r15b")
}

/// Does this 8-bit register require REX prefix for access (spl, bpl, sil, dil)?
pub(crate) fn is_rex_required_8bit(name: &str) -> bool {
    matches!(name, "spl" | "bpl" | "sil" | "dil")
}

/// Is this an XMM register?
pub(crate) fn is_xmm(name: &str) -> bool {
    name.starts_with("xmm")
}

/// Is this an XMM or YMM register?
pub(crate) fn is_xmm_or_ymm(name: &str) -> bool {
    name.starts_with("xmm") || name.starts_with("ymm")
}

/// Get the operand-size suffix character for a register name.
/// Returns 'q' for 64-bit, 'l' for 32-bit, 'w' for 16-bit, 'b' for 8-bit.
pub(crate) fn register_size_suffix(name: &str) -> Option<char> {
    if is_reg64(name) { return Some('q'); }
    if is_reg32(name) { return Some('l'); }
    if is_reg16(name) { return Some('w'); }
    if is_reg8(name) { return Some('b'); }
    None
}

/// Set of base mnemonics that accept AT&T size suffixes (b/w/l/q).
/// Only these mnemonics will have suffixes inferred from operand types.
pub(crate) const SUFFIXABLE_MNEMONICS: &[&str] = &[
    "mov", "add", "sub", "and", "or", "xor", "cmp", "test",
    "push", "pop", "lea",
    "shl", "shr", "sar", "rol", "ror",
    "inc", "dec", "neg", "not",
    "imul", "mul", "div", "idiv",
    "adc", "sbb",
    "xchg", "cmpxchg", "xadd", "bswap",
    "bsf", "bsr",
];

/// Infer the AT&T size suffix for an unsuffixed mnemonic from its operands.
///
/// Hand-written assembly (e.g., musl's .s files) often omits the size suffix
/// when it can be inferred from register operands. For example:
///   push %rax  -> pushq %rax
///   mov %edi,%ecx -> movl %edi,%ecx
///   and $0x3f,%ecx -> andl $0x3f,%ecx
///
/// Only mnemonics in the SUFFIXABLE_MNEMONICS whitelist are candidates for
/// suffix inference. All others are returned as-is.
pub(crate) fn infer_suffix(mnemonic: &str, ops: &[Operand]) -> String {
    // Check if this is a known suffixable mnemonic
    if !SUFFIXABLE_MNEMONICS.contains(&mnemonic) {
        return mnemonic.to_string();
    }

    // For shift/rotate instructions, infer size from the *destination* (last) operand,
    // not %cl (the first operand). E.g., "shl %cl, %edx" should become "shll", not "shlb".
    let is_shift = matches!(mnemonic, "shl" | "shr" | "sar" | "rol" | "ror");
    if is_shift && ops.len() == 2 {
        if let Operand::Register(r) = &ops[1] {
            if let Some(suffix) = register_size_suffix(&r.name) {
                return format!("{}{}", mnemonic, suffix);
            }
        }
    }

    // Find the first register operand to determine size
    for op in ops {
        if let Operand::Register(r) = op {
            if let Some(suffix) = register_size_suffix(&r.name) {
                return format!("{}{}", mnemonic, suffix);
            }
        }
    }

    // No register operand found - return as-is
    mnemonic.to_string()
}

/// Get operand size from mnemonic suffix.
pub(crate) fn mnemonic_size_suffix(mnemonic: &str) -> Option<u8> {
    // Handle mnemonics that don't follow the simple suffix pattern
    match mnemonic {
        "cltq" | "cqto" | "cltd" | "cdq" | "cqo" | "ret" | "nop" | "ud2"
        | "endbr64" | "pause" | "mfence" | "lfence" | "sfence" | "clflush"
        | "syscall" | "sysenter" | "cpuid" | "rdtsc" | "rdtscp" | "rdpmc"
        | "clc" | "stc" | "cli" | "sti" | "cld" | "std" | "sahf" | "lahf" | "fninit" | "fwait" | "wait" | "fnstcw" | "fstcw"
        | "fld1" | "fldl2e" | "fldlg2" | "fldln2" | "fldz" | "fldpi" | "fldl2t"
        | "fabs" | "fsqrt" | "frndint" | "f2xm1" | "fscale" | "fpatan" | "fprem" | "fprem1"
        | "fyl2x" | "fyl2xp1" | "fptan" | "fsin" | "fcos" | "fxtract" | "fnclex" | "fxch"
        | "fadd" | "fmul" | "fsub" | "fdiv" | "fnstenv" | "fldenv" | "fnstsw"
        | "ldmxcsr" | "stmxcsr" | "wbinvd" | "invd" | "rdsspq" | "rdsspd"
        | "lmsw" | "smsw"
        | "pushf" | "pushfq" | "pushfl" | "popf" | "popfq" | "popfl" | "int3"
        | "movsq" | "stosq" | "movsw" | "stosw" | "lodsb" | "lodsw" | "lodsd" | "lodsq"
        | "scasb" | "scasw" | "scasd" | "scasq" | "cmpsb" | "cmpsw" | "cmpsd" | "cmpsq"
        | "insb" | "insw" | "insd" | "insl" | "outsb" | "outsw" | "outsd" | "outsl"
        | "inb" | "inw" | "inl" | "outb" | "outw" | "outl" => return None,
        _ => {}
    }

    let last = mnemonic.as_bytes().last()?;
    match last {
        b'b' => Some(1),
        b'w' => Some(2),
        b'l' | b'd' => Some(4),
        b'q' => Some(8),
        _ => None,
    }
}

/// Infer register size in bytes from register name.
pub(crate) fn infer_reg_size(name: &str) -> u8 {
    if is_reg64(name) { 8 }
    else if is_reg32(name) { 4 }
    else if is_reg16(name) { 2 }
    else if is_reg8(name) { 1 }
    else if is_xmm(name) { 16 }
    else if is_ymm(name) { 32 }
    else { 8 } // mmx and other registers default to 8
}

/// Infer operand size from a pair of operands for suffix-less instructions.
pub(crate) fn infer_operand_size_from_pair(op1: &Operand, op2: &Operand) -> u8 {
    // Try to infer from register operands
    for op in [op1, op2] {
        if let Operand::Register(r) = op {
            if is_segment_reg(&r.name) { continue; }
            if is_reg64(&r.name) { return 8; }
            if is_reg32(&r.name) { return 4; }
            if is_reg16(&r.name) { return 2; }
            if is_reg8(&r.name) { return 1; }
        }
    }
    // Default to 64-bit
    8
}

/// Parse x87 register number: "st(0)" -> 0, "st" -> 0, "st(1)" -> 1, etc.
pub(crate) fn parse_st_num(name: &str) -> Result<u8, String> {
    if name == "st" || name == "st(0)" {
        return Ok(0);
    }
    if name.starts_with("st(") && name.ends_with(')') {
        let n: u8 = name[3..name.len()-1].parse()
            .map_err(|_| format!("bad st register: {}", name))?;
        if n > 7 {
            return Err(format!("st register out of range: {}", name));
        }
        return Ok(n);
    }
    Err(format!("not an st register: {}", name))
}

/// Map condition code suffix to encoding.
pub(crate) fn cc_from_mnemonic(cc_str: &str) -> Result<u8, String> {
    match cc_str {
        "o" => Ok(0),
        "no" => Ok(1),
        "b" | "c" | "nae" => Ok(2),
        "nb" | "nc" | "ae" => Ok(3),
        "e" | "z" => Ok(4),
        "ne" | "nz" => Ok(5),
        "be" | "na" => Ok(6),
        "nbe" | "a" => Ok(7),
        "s" => Ok(8),
        "ns" => Ok(9),
        "p" | "pe" => Ok(10),
        "np" | "po" => Ok(11),
        "l" | "nge" => Ok(12),
        "nl" | "ge" => Ok(13),
        "le" | "ng" => Ok(14),
        "nle" | "g" => Ok(15),
        _ => Err(format!("unknown condition code: {}", cc_str)),
    }
}
