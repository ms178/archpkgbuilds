//! Register helper functions for i686 instruction encoding.

/// Register encoding (3-bit register number in ModR/M and SIB).
pub(crate) fn reg_num(name: &str) -> Option<u8> {
    match name {
        "al" | "ax" | "eax" | "xmm0" | "mm0" | "st" | "st(0)" | "ymm0" => Some(0),
        "cl" | "cx" | "ecx" | "xmm1" | "mm1" | "st(1)" | "ymm1" => Some(1),
        "dl" | "dx" | "edx" | "xmm2" | "mm2" | "st(2)" | "ymm2" => Some(2),
        "bl" | "bx" | "ebx" | "xmm3" | "mm3" | "st(3)" | "ymm3" => Some(3),
        "ah" | "sp" | "esp" | "xmm4" | "mm4" | "st(4)" | "ymm4" => Some(4),
        "ch" | "bp" | "ebp" | "xmm5" | "mm5" | "st(5)" | "ymm5" => Some(5),
        "dh" | "si" | "esi" | "xmm6" | "mm6" | "st(6)" | "ymm6" => Some(6),
        "bh" | "di" | "edi" | "xmm7" | "mm7" | "st(7)" | "ymm7" => Some(7),
        _ => None,
    }
}

/// Get segment register number (es=0, cs=1, ss=2, ds=3, fs=4, gs=5).
pub(crate) fn seg_reg_num(name: &str) -> Option<u8> {
    match name {
        "es" => Some(0),
        "cs" => Some(1),
        "ss" => Some(2),
        "ds" => Some(3),
        "fs" => Some(4),
        "gs" => Some(5),
        _ => None,
    }
}

/// Is this a segment register?
pub(crate) fn is_segment_reg(name: &str) -> bool {
    matches!(name, "es" | "cs" | "ss" | "ds" | "fs" | "gs")
}

/// Is this a control register?
pub(crate) fn is_control_reg(name: &str) -> bool {
    matches!(name, "cr0" | "cr2" | "cr3" | "cr4")
}

/// Get control register number.
pub(crate) fn control_reg_num(name: &str) -> Option<u8> {
    match name {
        "cr0" => Some(0),
        "cr2" => Some(2),
        "cr3" => Some(3),
        "cr4" => Some(4),
        _ => None,
    }
}

/// Is this an XMM register?
pub(crate) fn is_xmm(name: &str) -> bool {
    name.starts_with("xmm")
}

/// Is this an MM (MMX) register?
pub(crate) fn is_mm(name: &str) -> bool {
    name.starts_with("mm") && !name.starts_with("mmx")
}

/// Infer operand size from register name for unsuffixed instructions.
pub(crate) fn reg_size(name: &str) -> u8 {
    match name {
        "al" | "ah" | "bl" | "bh" | "cl" | "ch" | "dl" | "dh" => 1,
        "ax" | "bx" | "cx" | "dx" | "sp" | "bp" | "si" | "di" => 2,
        "es" | "cs" | "ss" | "ds" | "fs" | "gs" => 2,
        _ => 4, // eax, ebx, etc. default to 32-bit on i686
    }
}

/// Get operand size from mnemonic suffix.
pub(crate) fn mnemonic_size_suffix(mnemonic: &str) -> Option<u8> {
    match mnemonic {
        "cltd" | "cdq" | "ret" | "nop" | "ud2" | "pause"
        | "mfence" | "lfence" | "sfence" | "clflush"
        | "ldmxcsr" | "stmxcsr"
        | "syscall" | "sysenter" | "cpuid" | "rdtsc" | "rdtscp" | "xgetbv"
        // Base ALU/shift mnemonics whose last letter is NOT a size suffix
        | "sub" | "sbb" | "add" | "and" | "shl" | "rol" | "xadd"
        | "insb" | "insw" | "insl" | "outsb" | "outsw" | "outsl"
        | "outb" | "outw" | "outl" | "inb" | "inw" | "inl"
        | "verw" | "lsl" | "sgdt" | "sidt" | "lgdt" | "lidt"
        | "sgdtl" | "sidtl" | "lgdtl" | "lidtl"
        | "lmsw" | "smsw"
        | "wbinvd" | "invlpg" | "rdpmc"
        | "ljmpl" | "ljmpw" | "ljmp" | "lret" | "lretl" | "lretq" => return None,
        _ => {}
    }
    let last = mnemonic.as_bytes().last()?;
    match last {
        b'b' => Some(1),
        b'w' => Some(2),
        b'l' | b'd' => Some(4),
        // i686 shouldn't have 'q' suffix for GP instructions, but handle gracefully
        b'q' => Some(4),
        _ => None,
    }
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
