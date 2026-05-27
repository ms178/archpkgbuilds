//! x86-64 instruction encoder.
//!
//! Encodes parsed x86-64 instructions into machine code bytes.
//! Handles REX prefixes, ModR/M, SIB, and displacement encoding.


mod registers;
mod core;
mod gp_integer;
mod system;
mod sse;
mod x87_misc;
mod avx;

pub(crate) use registers::*;

pub(crate) use super::parser::*;

/// Relocation entry for the linker to resolve.
#[derive(Debug, Clone)]
pub struct Relocation {
    /// Offset within the section where the relocation applies.
    pub offset: u64,
    /// Symbol name to resolve.
    pub symbol: String,
    /// Relocation type (ELF R_X86_64_* constants).
    pub reloc_type: u32,
    /// Addend for the relocation.
    pub addend: i64,
}

// ELF x86-64 relocation types (standard constants; not all emitted by assembler yet)
#[allow(dead_code)] // ELF standard constant, defined for reference/future use
pub const R_X86_64_NONE: u32 = 0;
pub const R_X86_64_64: u32 = 1;
pub const R_X86_64_PC32: u32 = 2;
#[allow(dead_code)] // ELF standard constant, defined for reference/future use
pub const R_X86_64_GOT32: u32 = 3;
pub const R_X86_64_PLT32: u32 = 4;
pub const R_X86_64_32: u32 = 10;
pub const R_X86_64_32S: u32 = 11;
pub const R_X86_64_16: u32 = 12;
pub const R_X86_64_GOTPCREL: u32 = 9;
pub const R_X86_64_TPOFF32: u32 = 23;
pub const R_X86_64_GOTTPOFF: u32 = 22;
#[allow(dead_code)] // ELF standard constant, defined for reference/future use
pub const R_X86_64_TPOFF64: u32 = 18;
// Internal-only: 8-bit PC-relative relocation for jrcxz/loop (never emitted to ELF)
pub const R_X86_64_PC8_INTERNAL: u32 = 0x8000_0001;

/// Instruction encoding context.
pub struct InstructionEncoder {
    /// Output bytes.
    pub bytes: Vec<u8>,
    /// Relocations generated during encoding.
    pub relocations: Vec<Relocation>,
    /// Current offset within the section.
    pub offset: u64,
}

impl InstructionEncoder {
    pub fn new() -> Self {
        InstructionEncoder {
            bytes: Vec::new(),
            relocations: Vec::new(),
            offset: 0,
        }
    }

    /// Encode a single instruction and append bytes.
    pub fn encode(&mut self, instr: &Instruction) -> Result<(), String> {
        let start_len = self.bytes.len();

        // Handle prefix
        if let Some(ref prefix) = instr.prefix {
            match prefix.as_str() {
                "lock" => self.bytes.push(0xF0),
                "rep" | "repz" | "repe" => self.bytes.push(0xF3),
                "repnz" | "repne" => self.bytes.push(0xF2),
                "notrack" => self.bytes.push(0x3E),
                _ => return Err(format!("unknown prefix: {}", prefix)),
            }
        }

        let result = self.encode_mnemonic(instr);

        if result.is_ok() {
            self.offset += (self.bytes.len() - start_len) as u64;
        }

        result
    }

    /// Main mnemonic dispatch.
    fn encode_mnemonic(&mut self, instr: &Instruction) -> Result<(), String> {
        // Infer suffix for unsuffixed mnemonics (e.g., push -> pushq, mov -> movl)
        // This enables assembling hand-written .s files that omit AT&T size suffixes.
        let suffixed = infer_suffix(&instr.mnemonic, &instr.operands);
        let mnemonic = suffixed.as_str();
        let ops = &instr.operands;

        match mnemonic {
            // Data movement
            "movq" => {
                // Check if any operand is an XMM register - route to SSE movq
                let has_xmm = ops.iter().any(|op| matches!(op, Operand::Register(r) if is_xmm(&r.name)));
                let has_mmx = ops.iter().any(|op| matches!(op, Operand::Register(r) if is_mmx(&r.name)));
                let has_seg = ops.iter().any(|op| matches!(op, Operand::Register(r) if is_segment_reg(&r.name)));
                let has_cr = ops.iter().any(|op| matches!(op, Operand::Register(r) if is_control_reg(&r.name)));
                let has_dr = ops.iter().any(|op| matches!(op, Operand::Register(r) if is_debug_reg(&r.name)));
                if has_xmm {
                    self.encode_movq_xmm(ops)
                } else if has_mmx {
                    self.encode_mmx_movq(ops)
                } else if has_seg {
                    self.encode_mov_seg(ops)
                } else if has_cr {
                    self.encode_mov_cr(ops)
                } else if has_dr {
                    self.encode_mov_dr(ops)
                } else {
                    self.encode_mov(ops, 8)
                }
            }
            "movl" => {
                if ops.iter().any(|op| matches!(op, Operand::Register(r) if is_segment_reg(&r.name))) {
                    self.encode_mov_seg(ops)
                } else if ops.iter().any(|op| matches!(op, Operand::Register(r) if is_control_reg(&r.name))) {
                    self.encode_mov_cr(ops)
                } else if ops.iter().any(|op| matches!(op, Operand::Register(r) if is_debug_reg(&r.name))) {
                    self.encode_mov_dr(ops)
                } else {
                    self.encode_mov(ops, 4)
                }
            }
            "movw" => {
                if ops.iter().any(|op| matches!(op, Operand::Register(r) if is_segment_reg(&r.name))) {
                    self.encode_mov_seg(ops)
                } else {
                    self.encode_mov(ops, 2)
                }
            }
            "movb" => self.encode_mov(ops, 1),
            "movabsq" | "movabs" => self.encode_movabs(ops),
            "movslq" => self.encode_movsx(ops, 4, 8),
            "movsbq" => self.encode_movsx(ops, 1, 8),
            "movswq" => self.encode_movsx(ops, 2, 8),
            "movsbl" => self.encode_movsx(ops, 1, 4),
            "movswl" => self.encode_movsx(ops, 2, 4),
            "movzbq" | "movzbl" => self.encode_movzx(ops, 1, if mnemonic == "movzbq" { 8 } else { 4 }),
            "movzwq" | "movzwl" => self.encode_movzx(ops, 2, if mnemonic == "movzwq" { 8 } else { 4 }),

            // LEA
            "leaq" => self.encode_lea(ops, 8),
            "leal" => self.encode_lea(ops, 4),

            // Stack ops
            "pushq" | "pushl" => self.encode_push(ops),
            "popq" | "popl" => self.encode_pop(ops),

            // Arithmetic
            "addq" | "addl" | "addw" | "addb" => self.encode_alu(ops, mnemonic, 0),
            "orq" | "orl" | "orw" | "orb" => self.encode_alu(ops, mnemonic, 1),
            "adcq" | "adcl" => self.encode_alu(ops, mnemonic, 2),
            "sbbq" | "sbbl" => self.encode_alu(ops, mnemonic, 3),
            "andq" | "andl" | "andw" | "andb" => self.encode_alu(ops, mnemonic, 4),
            "subq" | "subl" | "subw" | "subb" => self.encode_alu(ops, mnemonic, 5),
            "xorq" | "xorl" | "xorw" | "xorb" => self.encode_alu(ops, mnemonic, 6),
            "cmpq" | "cmpl" | "cmpw" | "cmpb" => self.encode_alu(ops, mnemonic, 7),
            "testq" | "testl" | "testw" | "testb" => self.encode_test(ops, mnemonic),

            // Multiply/divide
            "imulq" => self.encode_imul(ops, 8),
            "imull" => self.encode_imul(ops, 4),
            "mulq" => self.encode_unary_rm(ops, 4, 8),
            "divq" => self.encode_unary_rm(ops, 6, 8),
            "divl" => self.encode_unary_rm(ops, 6, 4),
            "idivq" => self.encode_unary_rm(ops, 7, 8),
            "idivl" => self.encode_unary_rm(ops, 7, 4),

            // Unary
            "negq" => self.encode_unary_rm(ops, 3, 8),
            "negl" => self.encode_unary_rm(ops, 3, 4),
            "notq" => self.encode_unary_rm(ops, 2, 8),
            "notl" => self.encode_unary_rm(ops, 2, 4),
            "incq" => self.encode_inc_dec(ops, 0, 8),
            "incl" => self.encode_inc_dec(ops, 0, 4),
            "incw" => self.encode_inc_dec(ops, 0, 2),
            "incb" => self.encode_inc_dec(ops, 0, 1),
            "decq" => self.encode_inc_dec(ops, 1, 8),
            "decl" => self.encode_inc_dec(ops, 1, 4),
            "decw" => self.encode_inc_dec(ops, 1, 2),
            "decb" => self.encode_inc_dec(ops, 1, 1),

            // Shifts
            "shlq" | "shll" | "shlw" | "shlb" => self.encode_shift(ops, mnemonic, 4),
            "shrq" | "shrl" | "shrw" | "shrb" => self.encode_shift(ops, mnemonic, 5),
            "sarq" | "sarl" | "sarw" | "sarb" => self.encode_shift(ops, mnemonic, 7),
            "rolq" | "roll" | "rolw" | "rolb" => self.encode_shift(ops, mnemonic, 0),
            "rorq" | "rorl" | "rorw" | "rorb" => self.encode_shift(ops, mnemonic, 1),

            // Double-precision shifts
            "shldq" => self.encode_double_shift(ops, 0xA4, 8),
            "shrdq" => self.encode_double_shift(ops, 0xAC, 8),

            // Sign extension
            "cltq" => { self.bytes.extend_from_slice(&[0x48, 0x98]); Ok(()) }
            "cqto" | "cqo" => { self.bytes.extend_from_slice(&[0x48, 0x99]); Ok(()) }
            "cltd" | "cdq" => { self.bytes.push(0x99); Ok(()) }

            // Byte swap
            "bswapl" => self.encode_bswap(ops, 4),
            "bswapq" => self.encode_bswap(ops, 8),

            // Bit operations
            "lzcntl" | "lzcntq" | "lzcntw" | "tzcntl" | "tzcntq" | "tzcntw"
            | "popcntl" | "popcntq" | "popcntw" => {
                self.encode_bit_count(ops, mnemonic)
            }
            "bsfl" | "bsfq" | "bsrl" | "bsrq" => self.encode_bsf_bsr(ops, mnemonic),
            "btq" | "btl" | "btsq" | "btsl" | "btrq" | "btrl" | "btcq" | "btcl" => {
                self.encode_bt(ops, mnemonic)
            }

            // Conditional set
            "sete" | "setz" | "setne" | "setnz" | "setl" | "setle" | "setg" | "setge"
            | "setb" | "setc" | "setbe" | "seta" | "setae" | "setnc" | "setnb" | "setnp" | "setp"
            | "sets" | "setns" | "seto" | "setno"
            | "setnae" | "setnbe" | "setnge" | "setng" | "setnle" | "setnl"
            | "setpe" | "setpo" => self.encode_setcc(ops, mnemonic),

            // Conditional move
            "cmoveq" | "cmovneq" | "cmovlq" | "cmovleq" | "cmovgq" | "cmovgeq"
            | "cmovbq" | "cmovbeq" | "cmovaq" | "cmovaeq"
            | "cmovel" | "cmovnel" | "cmovll" | "cmovlel" | "cmovgl" | "cmovgel"
            | "cmovbl" | "cmovbel" | "cmoval" | "cmovael"
            | "cmovew" | "cmovnew" | "cmovlw" | "cmovlew" | "cmovgw" | "cmovgew"
            | "cmovbw" | "cmovbew" | "cmovaw" | "cmovaew" => self.encode_cmovcc(ops, mnemonic),

            // Jumps (jmpq is a common AT&T alias for jmp on x86-64)
            "jmp" | "jmpq" => self.encode_jmp(ops),
            "ljmpl" | "ljmpq" | "ljmp" | "ljmpw" => self.encode_ljmp(ops, mnemonic),
            "je" | "jz" | "jne" | "jnz" | "jl" | "jle" | "jg" | "jge"
            | "jb" | "jbe" | "ja" | "jae" | "js" | "jns" | "jo" | "jno" | "jp" | "jnp"
            | "jc" | "jnc" => {
                self.encode_jcc(ops, mnemonic)
            }

            // Call/return (callq/retq are common AT&T aliases on x86-64)
            "call" | "callq" => self.encode_call(ops),
            "ret" | "retq" => {
                if ops.is_empty() {
                    self.bytes.push(0xC3);
                } else if let Some(Operand::Immediate(ImmediateValue::Integer(val))) = ops.first() {
                    // ret $imm16 - pop return address and deallocate imm16 bytes
                    self.bytes.push(0xC2);
                    self.bytes.extend_from_slice(&(*val as u16).to_le_bytes());
                } else {
                    return Err("unsupported ret operand".to_string());
                }
                Ok(())
            }

            // Standalone prefix bytes (used when prefix and instruction are on separate
            // lines or separated by ; in inline asm, e.g., "lock ; cmpxchg %r, %m")
            "lock" if ops.is_empty() => { self.bytes.push(0xF0); Ok(()) }
            "rep" | "repe" | "repz" if ops.is_empty() => { self.bytes.push(0xF3); Ok(()) }
            "repne" | "repnz" if ops.is_empty() => { self.bytes.push(0xF2); Ok(()) }

            // System instructions
            "syscall" => { self.bytes.extend_from_slice(&[0x0F, 0x05]); Ok(()) }
            "sysretq" | "sysret" => { self.bytes.extend_from_slice(&[0x48, 0x0F, 0x07]); Ok(()) }
            "sysretl" => { self.bytes.extend_from_slice(&[0x0F, 0x07]); Ok(()) }
            "sysenter" => { self.bytes.extend_from_slice(&[0x0F, 0x34]); Ok(()) }
            "sysexitq" => { self.bytes.extend_from_slice(&[0x48, 0x0F, 0x35]); Ok(()) }
            "sysexit" => { self.bytes.extend_from_slice(&[0x0F, 0x35]); Ok(()) }
            "iretq" | "iretl" => {
                if mnemonic == "iretq" {
                    self.bytes.extend_from_slice(&[0x48, 0xCF]); // REX.W + IRET
                } else {
                    self.bytes.push(0xCF); // IRET (32-bit)
                }
                Ok(())
            }
            "lretq" | "lret" | "lretl" => {
                let is_64 = mnemonic == "lretq";
                if ops.is_empty() {
                    if is_64 {
                        self.bytes.extend_from_slice(&[0x48, 0xCB]); // REX.W + RETF
                    } else {
                        self.bytes.push(0xCB); // RETF
                    }
                } else if let Some(Operand::Immediate(ImmediateValue::Integer(val))) = ops.first() {
                    if is_64 {
                        self.bytes.push(0x48); // REX.W
                    }
                    self.bytes.push(0xCA); // RETF imm16
                    self.bytes.extend_from_slice(&(*val as u16).to_le_bytes());
                } else {
                    return Err("unsupported lret operand".to_string());
                }
                Ok(())
            }
            "cpuid" => { self.bytes.extend_from_slice(&[0x0F, 0xA2]); Ok(()) }
            "rdtsc" => { self.bytes.extend_from_slice(&[0x0F, 0x31]); Ok(()) }
            "rdtscp" => { self.bytes.extend_from_slice(&[0x0F, 0x01, 0xF9]); Ok(()) }
            "wbinvd" => { self.bytes.extend_from_slice(&[0x0F, 0x09]); Ok(()) }
            "invd" => { self.bytes.extend_from_slice(&[0x0F, 0x08]); Ok(()) }

            // VMX instructions
            "vmcall" => { self.bytes.extend_from_slice(&[0x0F, 0x01, 0xC1]); Ok(()) }
            "vmlaunch" => { self.bytes.extend_from_slice(&[0x0F, 0x01, 0xC2]); Ok(()) }
            "vmresume" => { self.bytes.extend_from_slice(&[0x0F, 0x01, 0xC3]); Ok(()) }
            "vmxoff" => { self.bytes.extend_from_slice(&[0x0F, 0x01, 0xC4]); Ok(()) }
            "vmmcall" => { self.bytes.extend_from_slice(&[0x0F, 0x01, 0xD9]); Ok(()) }
            "vmfunc" => { self.bytes.extend_from_slice(&[0x0F, 0x01, 0xD4]); Ok(()) }

            // No-ops and misc
            "nop" => { self.bytes.push(0x90); Ok(()) }
            "hlt" => { self.bytes.push(0xF4); Ok(()) }
            "leave" | "leaveq" => { self.bytes.push(0xC9); Ok(()) }
            "ud2" => { self.bytes.extend_from_slice(&[0x0F, 0x0B]); Ok(()) }
            "endbr64" => { self.bytes.extend_from_slice(&[0xF3, 0x0F, 0x1E, 0xFA]); Ok(()) }
            // CET shadow stack: rdsspq %r64, rdsspd %r32
            "rdsspq" => {
                if let Some(Operand::Register(reg)) = ops.first() {
                    let rm = reg_num(&reg.name).ok_or_else(|| format!("unknown register: {}", reg.name))?;
                    self.bytes.push(0xF3);
                    // REX.W prefix (with REX.B if needed for r8-r15)
                    let rex_b = if needs_rex_ext(&reg.name) { 1 } else { 0 };
                    self.bytes.push(0x48 | rex_b);
                    self.bytes.extend_from_slice(&[0x0F, 0x1E]);
                    self.bytes.push(0xC8 | rm);
                    Ok(())
                } else {
                    Err("rdsspq requires a 64-bit register operand".to_string())
                }
            }
            "rdsspd" => {
                if let Some(Operand::Register(reg)) = ops.first() {
                    let rm = reg_num(&reg.name).ok_or_else(|| format!("unknown register: {}", reg.name))?;
                    self.bytes.push(0xF3);
                    if needs_rex_ext(&reg.name) {
                        self.bytes.push(0x41);
                    }
                    self.bytes.extend_from_slice(&[0x0F, 0x1E]);
                    self.bytes.push(0xC8 | rm);
                    Ok(())
                } else {
                    Err("rdsspd requires a 32-bit register operand".to_string())
                }
            }
            "pause" => { self.bytes.extend_from_slice(&[0xF3, 0x90]); Ok(()) }
            "mfence" => { self.bytes.extend_from_slice(&[0x0F, 0xAE, 0xF0]); Ok(()) }
            "lfence" => { self.bytes.extend_from_slice(&[0x0F, 0xAE, 0xE8]); Ok(()) }
            "sfence" => { self.bytes.extend_from_slice(&[0x0F, 0xAE, 0xF8]); Ok(()) }
            "clflush" => self.encode_clflush(ops),

            // Direction flag
            "cld" => { self.bytes.push(0xFC); Ok(()) }
            "std" => { self.bytes.push(0xFD); Ok(()) }

            // String ops
            "movsb" => { self.bytes.push(0xA4); Ok(()) }
            "movsd" if ops.is_empty() => { self.bytes.push(0xA5); Ok(()) }
            "movsq" => { self.bytes.extend_from_slice(&[0x48, 0xA5]); Ok(()) }
            "stosb" => { self.bytes.push(0xAA); Ok(()) }
            "stosd" => { self.bytes.push(0xAB); Ok(()) }
            "stosq" => { self.bytes.extend_from_slice(&[0x48, 0xAB]); Ok(()) }
            "lodsb" => { self.bytes.push(0xAC); Ok(()) }
            "lodsd" => { self.bytes.push(0xAD); Ok(()) }
            "lodsq" => { self.bytes.extend_from_slice(&[0x48, 0xAD]); Ok(()) }
            "scasb" => { self.bytes.push(0xAE); Ok(()) }
            "scasd" => { self.bytes.push(0xAF); Ok(()) }
            "scasq" => { self.bytes.extend_from_slice(&[0x48, 0xAF]); Ok(()) }
            "cmpsb" => { self.bytes.push(0xA6); Ok(()) }
            "cmpsd" if ops.is_empty() => { self.bytes.push(0xA7); Ok(()) }
            "cmpsq" => { self.bytes.extend_from_slice(&[0x48, 0xA7]); Ok(()) }

            // I/O string ops
            "insb" => { self.bytes.push(0x6C); Ok(()) }
            "insw" => { self.bytes.extend_from_slice(&[0x66, 0x6D]); Ok(()) }
            "insd" | "insl" => { self.bytes.push(0x6D); Ok(()) }
            "outsb" => { self.bytes.push(0x6E); Ok(()) }
            "outsw" => { self.bytes.extend_from_slice(&[0x66, 0x6F]); Ok(()) }
            "outsd" | "outsl" => { self.bytes.push(0x6F); Ok(()) }

            // Port I/O instructions
            "outb" | "outw" | "outl" => self.encode_out(ops, mnemonic),
            "inb" | "inw" | "inl" => self.encode_in(ops, mnemonic),

            // Atomic exchange
            "xchgb" | "xchgw" | "xchgl" | "xchgq" => self.encode_xchg(ops, mnemonic),

            // Lock-prefixed atomics (already have the lock prefix from the prefix handling)
            "cmpxchgb" | "cmpxchgw" | "cmpxchgl" | "cmpxchgq" => self.encode_cmpxchg(ops, mnemonic),
            "xaddb" | "xaddw" | "xaddl" | "xaddq" => self.encode_xadd(ops, mnemonic),

            // SSE/SSE2 floating-point
            "movss" => self.encode_sse_rr_rm(ops, &[0xF3, 0x0F, 0x10], &[0xF3, 0x0F, 0x11]),
            "movsd" => self.encode_sse_rr_rm(ops, &[0xF2, 0x0F, 0x10], &[0xF2, 0x0F, 0x11]),
            "movd" => self.encode_movd(ops),
            "movdqu" => self.encode_sse_rr_rm(ops, &[0xF3, 0x0F, 0x6F], &[0xF3, 0x0F, 0x7F]),
            "movupd" => self.encode_sse_rr_rm(ops, &[0x66, 0x0F, 0x10], &[0x66, 0x0F, 0x11]),

            "addsd" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x58]),
            "subsd" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x5C]),
            "mulsd" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x59]),
            "divsd" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x5E]),
            "addss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x58]),
            "subss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5C]),
            "mulss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x59]),
            "divss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5E]),
            "sqrtsd" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x51]),
            "sqrtss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x51]),
            "sqrtps" => self.encode_sse_op(ops, &[0x0F, 0x51]),
            "sqrtpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x51]),
            "rsqrtss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x52]),
            "rsqrtps" => self.encode_sse_op(ops, &[0x0F, 0x52]),
            "rcpss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x53]),
            "rcpps" => self.encode_sse_op(ops, &[0x0F, 0x53]),
            "ucomisd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x2E]),
            "ucomiss" => self.encode_sse_op(ops, &[0x0F, 0x2E]),
            "xorpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x57]),
            "xorps" => self.encode_sse_op(ops, &[0x0F, 0x57]),
            "andpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x54]),
            "andps" => self.encode_sse_op(ops, &[0x0F, 0x54]),

            // SSE conversions
            "cvtsd2ss" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x5A]),
            "cvtss2sd" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5A]),
            "cvtsi2sdq" => self.encode_sse_cvt_gp_to_xmm(ops, &[0xF2, 0x0F, 0x2A], 8),
            "cvtsi2ssq" => self.encode_sse_cvt_gp_to_xmm(ops, &[0xF3, 0x0F, 0x2A], 8),
            "cvttsd2siq" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF2, 0x0F, 0x2C], 8),
            "cvttss2siq" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF3, 0x0F, 0x2C], 8),
            "cvtsd2siq" | "cvtsd2si" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF2, 0x0F, 0x2D], 8),
            "cvtss2siq" | "cvtss2si" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF3, 0x0F, 0x2D], 8),
            "cvtsd2sil" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF2, 0x0F, 0x2D], 4),
            "cvtss2sil" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF3, 0x0F, 0x2D], 4),
            "cvttsd2sil" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF2, 0x0F, 0x2C], 4),
            "cvttss2sil" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF3, 0x0F, 0x2C], 4),
            "cvtsi2sdl" => self.encode_sse_cvt_gp_to_xmm(ops, &[0xF2, 0x0F, 0x2A], 4),
            "cvtsi2ssl" => self.encode_sse_cvt_gp_to_xmm(ops, &[0xF3, 0x0F, 0x2A], 4),

            // SSE2 integer SIMD
            "paddw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xFD]),
            "psubw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xF9]),
            "paddd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xFE]),
            "psubd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xFA]),
            "pmulhw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE5]),
            "pmaddwd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xF5]),
            "pcmpgtw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x65]),
            "pcmpgtb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x64]),
            "packssdw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x6B]),
            "packuswb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x67]),
            "punpcklbw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x60]),
            "punpckhbw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x68]),
            "punpcklwd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x61]),
            "punpckhwd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x69]),
            "pmovmskb" => self.encode_sse_cvt_xmm_to_gp(ops, &[0x66, 0x0F, 0xD7], 4),

            // Additional SSE2 packed integer operations
            "pcmpeqb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x74]),
            "pcmpeqd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x76]),
            "pcmpeqw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x75]),
            "pand" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDB]),
            "pandn" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDF]),
            "por" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEB]),
            "pxor" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEF]),
            "psubusb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xD8]),
            "psubusw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xD9]),
            "psubsb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE8]),
            "psubsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE9]),
            "paddusb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDC]),
            "paddusw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDD]),
            "paddsb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEC]),
            "paddsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xED]),
            "pmuludq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xF4]),
            "pmullw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xD5]),
            "pmulld" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x40]),
            "pminub" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDA]),
            "pmaxub" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDE]),
            "pminsd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x39]),
            "pmaxsd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3D]),
            "pavgb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE0]),
            "pavgw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE3]),
            "psadbw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xF6]),
            "paddq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xD4]),
            "psubq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xFB]),
            "punpcklqdq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x6C]),
            "punpckhqdq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x6D]),
            "punpckldq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x62]),
            "punpckhdq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x6A]),

            // SSE packed float operations
            "addpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x58]),
            "subpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x5C]),
            "mulpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x59]),
            "divpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x5E]),
            "addps" => self.encode_sse_op(ops, &[0x0F, 0x58]),
            "subps" => self.encode_sse_op(ops, &[0x0F, 0x5C]),
            "mulps" => self.encode_sse_op(ops, &[0x0F, 0x59]),
            "divps" => self.encode_sse_op(ops, &[0x0F, 0x5E]),
            "orpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x56]),
            "orps" => self.encode_sse_op(ops, &[0x0F, 0x56]),
            "andnpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x55]),
            "andnps" => self.encode_sse_op(ops, &[0x0F, 0x55]),
            "haddpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x7C]),
            "hsubpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x7D]),
            "haddps" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x7C]),
            "hsubps" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x7D]),
            "addsubpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xD0]),
            "addsubps" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0xD0]),
            "minsd" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x5D]),
            "maxsd" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x5F]),
            "minss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5D]),
            "maxss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5F]),

            // Additional SSE data movement
            "movaps" => self.encode_sse_rr_rm(ops, &[0x0F, 0x28], &[0x0F, 0x29]),
            "movdqa" => self.encode_sse_rr_rm(ops, &[0x66, 0x0F, 0x6F], &[0x66, 0x0F, 0x7F]),
            "movlpd" => self.encode_sse_rr_rm(ops, &[0x66, 0x0F, 0x12], &[0x66, 0x0F, 0x13]),
            "movhpd" => self.encode_sse_rr_rm(ops, &[0x66, 0x0F, 0x16], &[0x66, 0x0F, 0x17]),
            "movlps" => self.encode_sse_rr_rm(ops, &[0x0F, 0x12], &[0x0F, 0x13]),
            "movhps" => self.encode_sse_rr_rm(ops, &[0x0F, 0x16], &[0x0F, 0x17]),

            // SSE shifts with immediate
            "psllw" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xF1], 6, &[0x66, 0x0F, 0x71]),
            "psrlw" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xD1], 2, &[0x66, 0x0F, 0x71]),
            "psraw" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xE1], 4, &[0x66, 0x0F, 0x71]),
            "pslld" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xF2], 6, &[0x66, 0x0F, 0x72]),
            "psrld" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xD2], 2, &[0x66, 0x0F, 0x72]),
            "psrad" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xE2], 4, &[0x66, 0x0F, 0x72]),
            "psllq" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xF3], 6, &[0x66, 0x0F, 0x73]),
            "psrlq" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xD3], 2, &[0x66, 0x0F, 0x73]),
            "pslldq" => self.encode_sse_shift_imm_only(ops, 7, &[0x66, 0x0F, 0x73]),
            "psrldq" => self.encode_sse_shift_imm_only(ops, 3, &[0x66, 0x0F, 0x73]),

            // SSE shuffles
            "pshufd" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x70]),
            "pshuflw" => self.encode_sse_op_imm8(ops, &[0xF2, 0x0F, 0x70]),
            "pshufhw" => self.encode_sse_op_imm8(ops, &[0xF3, 0x0F, 0x70]),

            // SSE insert/extract
            "pinsrw" => self.encode_sse_insert(ops, &[0x66, 0x0F, 0xC4], false),
            // Legacy pextrw (66 0F C5): GP reg is in the ModRM "reg" field,
            // XMM src is in the "r/m" field. This is inverted vs SSE4.1 extracts.
            // TODO: pextrw with memory dest needs SSE4.1 opcode 66 0F 3A 15, not legacy 66 0F C5
            "pextrw" => self.encode_sse_extract(ops, &[0x66, 0x0F, 0xC5], false, true),
            "pinsrd" => self.encode_sse_insert(ops, &[0x66, 0x0F, 0x3A, 0x22], false),
            "pextrd" => self.encode_sse_extract(ops, &[0x66, 0x0F, 0x3A, 0x16], false, false),
            "pinsrb" => self.encode_sse_insert(ops, &[0x66, 0x0F, 0x3A, 0x20], false),
            "pextrb" => self.encode_sse_extract(ops, &[0x66, 0x0F, 0x3A, 0x14], false, false),
            "pinsrq" => self.encode_sse_insert(ops, &[0x66, 0x0F, 0x3A, 0x22], true),
            "pextrq" => self.encode_sse_extract(ops, &[0x66, 0x0F, 0x3A, 0x16], true, false),

            // AES-NI
            "aesenc" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0xDC]),
            "aesenclast" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0xDD]),
            "aesdec" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0xDE]),
            "aesdeclast" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0xDF]),
            "aesimc" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0xDB]),
            "aeskeygenassist" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0xDF]),

            // PCLMULQDQ
            "pclmulqdq" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x44]),

            // Non-temporal stores and loads
            "movnti" | "movntil" => self.encode_movnti(ops),
            "movntiq" => self.encode_movnti_q(ops),
            "movntdq" => self.encode_sse_store(ops, &[0x66, 0x0F, 0xE7]),
            "movntdqa" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x2A]),
            "movntpd" => self.encode_sse_store(ops, &[0x66, 0x0F, 0x2B]),

            // CRC32
            "crc32b" => self.encode_crc32(ops, 1),
            "crc32w" => self.encode_crc32(ops, 2),
            "crc32l" => self.encode_crc32(ops, 4),
            "crc32q" => self.encode_crc32(ops, 8),

            // x87 FPU
            "fldt" => self.encode_x87_mem(ops, &[0xDB], 5),
            "fstpt" => self.encode_x87_mem(ops, &[0xDB], 7),
            "fldl" => self.encode_x87_mem(ops, &[0xDD], 0),
            "flds" => self.encode_x87_mem(ops, &[0xD9], 0),
            "fstpl" => self.encode_x87_mem(ops, &[0xDD], 3),
            "fstps" => self.encode_x87_mem(ops, &[0xD9], 3),
            "fildq" | "fildll" => self.encode_x87_mem(ops, &[0xDF], 5),
            "fisttpq" | "fisttpll" => self.encode_x87_mem(ops, &[0xDD], 1),
            "fistpq" | "fistpll" => self.encode_x87_mem(ops, &[0xDF], 7),
            "faddp" => { self.bytes.extend_from_slice(&[0xDE, 0xC1]); Ok(()) }
            // Note: AT&T syntax swaps the meaning of fsub/fsubr and fdiv/fdivr
            // relative to Intel mnemonics for the *p (pop) forms.
            // GAS: fsubp = DE E1, fsubrp = DE E9, fdivp = DE F1, fdivrp = DE F9
            "fsubp" => { self.bytes.extend_from_slice(&[0xDE, 0xE1]); Ok(()) }
            "fsubrp" => { self.bytes.extend_from_slice(&[0xDE, 0xE9]); Ok(()) }
            "fmulp" => { self.bytes.extend_from_slice(&[0xDE, 0xC9]); Ok(()) }
            "fdivp" => { self.bytes.extend_from_slice(&[0xDE, 0xF1]); Ok(()) }
            "fdivrp" => { self.bytes.extend_from_slice(&[0xDE, 0xF9]); Ok(()) }
            "fchs" => { self.bytes.extend_from_slice(&[0xD9, 0xE0]); Ok(()) }
            "fcomip" => self.encode_fcomip(ops),
            "fucomip" => self.encode_fucomip(ops),
            "fld" => self.encode_fld_st(ops),
            "fstp" => self.encode_fstp_st(ops),

            // ---- Flag manipulation ----
            "clc" => { self.bytes.push(0xF8); Ok(()) }
            "stc" => { self.bytes.push(0xF9); Ok(()) }
            "cli" => { self.bytes.push(0xFA); Ok(()) }
            "sti" => { self.bytes.push(0xFB); Ok(()) }
            "cmc" => { self.bytes.push(0xF5); Ok(()) }
            "sahf" => { self.bytes.push(0x9E); Ok(()) }
            "lahf" => { self.bytes.push(0x9F); Ok(()) }
            "pushf" | "pushfq" => { self.bytes.push(0x9C); Ok(()) }
            // pushfl: 32-bit pushf (needs operand-size override prefix in 64-bit mode,
            // but in .code16gcc/.code32 sections it's the native size)
            "pushfl" => { self.bytes.push(0x9C); Ok(()) }
            "popf" | "popfq" => { self.bytes.push(0x9D); Ok(()) }
            "popfl" => { self.bytes.push(0x9D); Ok(()) }

            // ---- System instructions ----
            "int3" => { self.bytes.push(0xCC); Ok(()) }
            "int" => {
                if let Some(Operand::Immediate(ImmediateValue::Integer(n))) = ops.first() {
                    if *n == 3 {
                        self.bytes.push(0xCC);
                    } else {
                        self.bytes.push(0xCD);
                        self.bytes.push(*n as u8);
                    }
                    Ok(())
                } else {
                    Err("int requires immediate operand".to_string())
                }
            }
            "sldt" => self.encode_sldt(ops),
            "str" if ops.len() == 1 => self.encode_str_insn(ops),
            "fninit" => { self.bytes.extend_from_slice(&[0xDB, 0xE3]); Ok(()) }
            "fwait" | "wait" => { self.bytes.push(0x9B); Ok(()) }
            "fnstcw" | "fstcw" => {
                // fstcw = fwait + fnstcw (wait prefix before the instruction)
                if mnemonic == "fstcw" {
                    self.bytes.push(0x9B); // FWAIT
                }
                self.encode_x87_mem(ops, &[0xD9], 7)
            }
            "fldcw" => self.encode_x87_mem(ops, &[0xD9], 5),

            // ---- String operations (additional sizes) ----
            "movsw" => { self.bytes.extend_from_slice(&[0x66, 0xA5]); Ok(()) }
            "stosw" => { self.bytes.extend_from_slice(&[0x66, 0xAB]); Ok(()) }
            "stosl" => { self.bytes.push(0xAB); Ok(()) }
            "movsl" => { self.bytes.push(0xA5); Ok(()) }
            "lodsw" => { self.bytes.extend_from_slice(&[0x66, 0xAD]); Ok(()) }
            "scasw" => { self.bytes.extend_from_slice(&[0x66, 0xAF]); Ok(()) }
            "cmpsw" => { self.bytes.extend_from_slice(&[0x66, 0xA7]); Ok(()) }

            // ---- Bit scan (16-bit) ----
            "bsfw" => self.encode_bit_scan(ops, mnemonic, 0xBC),
            "bsrw" => self.encode_bit_scan(ops, mnemonic, 0xBD),

            // ---- Unsigned multiply (single-operand) ----
            "mull" => self.encode_unary_rm(ops, 4, 4),
            "mulw" => {
                self.bytes.push(0x66);
                self.encode_unary_rm(ops, 4, 2)
            }

            // ---- Rotate through carry ----
            "rclq" | "rcll" | "rclw" | "rclb" => self.encode_shift(ops, mnemonic, 2),
            "rcrq" | "rcrl" | "rcrw" | "rcrb" => self.encode_shift(ops, mnemonic, 3),

            // ---- Bit test operations (16-bit) ----
            "btw" => self.encode_bt(ops, mnemonic),
            "btsw" => self.encode_bt(ops, mnemonic),
            "btrw" => self.encode_bt(ops, mnemonic),
            "btcw" => self.encode_bt(ops, mnemonic),

            // ---- 32-bit double shifts ----
            "shldl" => self.encode_double_shift(ops, 0xA4, 4),
            "shrdl" => self.encode_double_shift(ops, 0xAC, 4),

            // ---- Additional unary sizes ----
            "notb" => self.encode_unary_rm(ops, 2, 1),
            "notw" => { self.bytes.push(0x66); self.encode_unary_rm(ops, 2, 2) }
            "negb" => self.encode_unary_rm(ops, 3, 1),
            "negw" => { self.bytes.push(0x66); self.encode_unary_rm(ops, 3, 2) }

            // ---- Additional conditional branches ----
            "jrcxz" => {
                if ops.len() != 1 { return Err("jrcxz requires 1 operand".to_string()); }
                match &ops[0] {
                    Operand::Label(label) => {
                        // jrcxz uses short jump only (E3 rel8)
                        self.bytes.push(0xE3);
                        self.add_relocation(label, R_X86_64_PC8_INTERNAL, -1);
                        self.bytes.push(0); // placeholder for rel8
                        Ok(())
                    }
                    _ => Err("jrcxz requires label".to_string()),
                }
            }
            "loop" => {
                if ops.len() != 1 { return Err("loop requires 1 operand".to_string()); }
                match &ops[0] {
                    Operand::Label(label) => {
                        self.bytes.push(0xE2);
                        self.add_relocation(label, R_X86_64_PC8_INTERNAL, -1);
                        self.bytes.push(0); // placeholder for rel8
                        Ok(())
                    }
                    _ => Err("loop requires label".to_string()),
                }
            }

            // ---- cmpxchg16b ----
            "cmpxchg16b" => {
                if ops.len() != 1 { return Err("cmpxchg16b requires 1 operand".to_string()); }
                match &ops[0] {
                    Operand::Memory(mem) => {
                        self.emit_rex_rm(8, "", mem); // REX.W
                        self.bytes.extend_from_slice(&[0x0F, 0xC7]);
                        self.encode_modrm_mem(1, mem)
                    }
                    _ => Err("cmpxchg16b requires memory operand".to_string()),
                }
            }

            // ---- MMX instructions ----
            "emms" => { self.bytes.extend_from_slice(&[0x0F, 0x77]); Ok(()) }

            // ---- SSE: palignr, pshufb ----
            "palignr" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x0F]),
            "pshufb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x00]),

            // ---- SSE: shufps, shufpd, movapd, unpcklpd, unpckhpd, movups ----
            "shufps" => self.encode_sse_op_imm8(ops, &[0x0F, 0xC6]),
            "shufpd" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0xC6]),
            "movapd" => self.encode_sse_rr_rm(ops, &[0x66, 0x0F, 0x28], &[0x66, 0x0F, 0x29]),
            "movups" => self.encode_sse_rr_rm(ops, &[0x0F, 0x10], &[0x0F, 0x11]),
            "unpcklpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x14]),
            "unpckhpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x15]),
            "unpcklps" => self.encode_sse_op(ops, &[0x0F, 0x14]),
            "unpckhps" => self.encode_sse_op(ops, &[0x0F, 0x15]),
            "movmskpd" => self.encode_sse_cvt_xmm_to_gp(ops, &[0x66, 0x0F, 0x50], 4),
            "movmskps" => self.encode_sse_cvt_xmm_to_gp(ops, &[0x0F, 0x50], 4),
            "movntps" => self.encode_sse_store(ops, &[0x0F, 0x2B]),

            // ---- SSE4.1 ----
            // blendv instructions use xmm0 as implicit mask; 3-op form names it explicitly
            "blendvpd" => { let ops2 = if ops.len() == 3 { &ops[1..] } else { ops }; self.encode_sse_op(ops2, &[0x66, 0x0F, 0x38, 0x15]) }
            "blendvps" => { let ops2 = if ops.len() == 3 { &ops[1..] } else { ops }; self.encode_sse_op(ops2, &[0x66, 0x0F, 0x38, 0x14]) }
            "pblendvb" => { let ops2 = if ops.len() == 3 { &ops[1..] } else { ops }; self.encode_sse_op(ops2, &[0x66, 0x0F, 0x38, 0x10]) }
            "roundsd" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x0B]),
            "roundss" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x0A]),
            "roundpd" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x09]),
            "roundps" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x08]),
            "pblendw" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x0E]),
            "blendpd" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x0D]),
            "blendps" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x0C]),
            "dpps" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x40]),
            "dppd" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x41]),
            "ptest" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x17]),
            "pminsb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x38]),
            "pminuw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3A]),
            "pmaxsb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3C]),
            "pmaxuw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3E]),
            "pminud" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3B]),
            "pmaxud" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3F]),
            "pminsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEA]),
            "pmaxsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEE]),
            "phminposuw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x41]),
            "packusdw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x2B]),
            "packsswb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x63]),
            "movhlps" => self.encode_sse_op(ops, &[0x0F, 0x12]),
            "movlhps" => self.encode_sse_op(ops, &[0x0F, 0x16]),
            "movddup" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x12]),
            "movshdup" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x16]),
            "movsldup" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x12]),
            "cvtps2pd" => self.encode_sse_op(ops, &[0x0F, 0x5A]),
            "cvtpd2ps" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x5A]),
            "cvtdq2ps" => self.encode_sse_op(ops, &[0x0F, 0x5B]),
            "cvtps2dq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x5B]),
            "cvttps2dq" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5B]),
            "cvtdq2pd" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0xE6]),
            "cvtpd2dq" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0xE6]),

            // SSE4.1 zero/sign extension
            "pmovzxbw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x30]),
            "pmovzxbd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x31]),
            "pmovzxbq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x32]),
            "pmovzxwd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x33]),
            "pmovzxwq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x34]),
            "pmovzxdq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x35]),
            "pmovsxbw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x20]),
            "pmovsxbd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x21]),
            "pmovsxbq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x22]),
            "pmovsxwd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x23]),
            "pmovsxwq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x24]),
            "pmovsxdq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x25]),
            "pcmpgtq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x37]),

            // SSSE3 instructions
            "pabsb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x1C]),
            "pabsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x1D]),
            "pabsd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x1E]),
            "phaddw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x01]),
            "phaddd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x02]),
            "phsubw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x05]),
            "phsubd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x06]),
            "pmulhrsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x0B]),

            // ---- AVX instructions (VEX-encoded) ----
            "vmovdqa" => self.encode_avx_mov(ops, 0x6F, 0x7F, true),
            "vmovdqu" => self.encode_avx_mov(ops, 0x6F, 0x7F, false),
            "vmovaps" => self.encode_avx_mov_np(ops, 0x28, 0x29, false),
            "vmovapd" => self.encode_avx_mov_np(ops, 0x28, 0x29, true),
            "vmovups" => self.encode_avx_mov_np(ops, 0x10, 0x11, false),
            "vmovupd" => self.encode_avx_mov_np(ops, 0x10, 0x11, true),
            "vbroadcastss" => self.encode_avx_broadcast(ops, &[0x18]),
            "vbroadcastsd" => self.encode_avx_broadcast(ops, &[0x19]),
            "vpand" => self.encode_avx_3op(ops, 0xDB, true),
            "vpandn" => self.encode_avx_3op(ops, 0xDF, true),
            "vpor" => self.encode_avx_3op(ops, 0xEB, true),
            "vpxor" => self.encode_avx_3op(ops, 0xEF, true),
            "vpaddd" => self.encode_avx_3op(ops, 0xFE, true),
            "vpsubd" => self.encode_avx_3op(ops, 0xFA, true),
            "vpcmpeqd" => self.encode_avx_3op(ops, 0x76, true),
            "vpcmpeqb" => self.encode_avx_3op(ops, 0x74, true),
            "vpcmpeqw" => self.encode_avx_3op(ops, 0x75, true),
            "vpcmpgtb" => self.encode_avx_3op(ops, 0x64, true),
            "vpcmpgtw" => self.encode_avx_3op(ops, 0x65, true),
            "vpcmpgtd" => self.encode_avx_3op(ops, 0x66, true),
            "vpaddb" => self.encode_avx_3op(ops, 0xFC, true),
            "vpaddw" => self.encode_avx_3op(ops, 0xFD, true),
            "vpsubb" => self.encode_avx_3op(ops, 0xF8, true),
            "vpsubw" => self.encode_avx_3op(ops, 0xF9, true),
            "vaddpd" => self.encode_avx_3op(ops, 0x58, true),
            "vsubpd" => self.encode_avx_3op(ops, 0x5C, true),
            "vmulpd" => self.encode_avx_3op(ops, 0x59, true),
            "vdivpd" => self.encode_avx_3op(ops, 0x5E, true),
            // FMA3 (0F38 map, W=1 for F64, pp=66)
            "vfmadd132pd" => self.encode_avx_3op_38_w1(ops, 0x98, true),
            "vfmadd213pd" => self.encode_avx_3op_38_w1(ops, 0xA8, true),
            "vfmadd231pd" => self.encode_avx_3op_38_w1(ops, 0xB8, true),
            "vaddps" => self.encode_avx_3op_np(ops, 0x58),
            "vsubps" => self.encode_avx_3op_np(ops, 0x5C),
            "vmulps" => self.encode_avx_3op_np(ops, 0x59),
            "vdivps" => self.encode_avx_3op_np(ops, 0x5E),
            "vxorpd" => self.encode_avx_3op(ops, 0x57, true),
            "vxorps" => self.encode_avx_3op_np(ops, 0x57),
            "vandpd" => self.encode_avx_3op(ops, 0x54, true),
            "vandps" => self.encode_avx_3op_np(ops, 0x54),
            "vandnpd" => self.encode_avx_3op(ops, 0x55, true),
            "vandnps" => self.encode_avx_3op_np(ops, 0x55),
            "vorpd" => self.encode_avx_3op(ops, 0x56, true),
            "vorps" => self.encode_avx_3op_np(ops, 0x56),
            "vpshufd" => self.encode_avx_shuffle(ops, 0x70, true),
            "vpshufb" => self.encode_avx_3op_38(ops, 0x00, true),
            "vpalignr" => self.encode_avx_3op_3a_imm8(ops, 0x0F, true),
            "vblendps" => self.encode_avx_3op_3a_imm8(ops, 0x0C, true),
            "vblendpd" => self.encode_avx_3op_3a_imm8(ops, 0x0D, true),
            "vinserti128" => self.encode_avx_3op_3a_imm8(ops, 0x38, true),
            "vextracti128" => self.encode_avx_extract_imm8(ops, 0x39, true),
            "vpabsb" => self.encode_avx_2op_38(ops, 0x1C, true),
            "vpabsw" => self.encode_avx_2op_38(ops, 0x1D, true),
            "vpabsd" => self.encode_avx_2op_38(ops, 0x1E, true),
            "vpmovmskb" => self.encode_avx_extract_gp(ops, 0xD7, true),
            "vmovd" => self.encode_avx_movd(ops),
            "vmovq" => self.encode_avx_movq(ops),
            "vpunpcklbw" => self.encode_avx_3op(ops, 0x60, true),
            "vpunpckhbw" => self.encode_avx_3op(ops, 0x68, true),
            "vpunpcklwd" => self.encode_avx_3op(ops, 0x61, true),
            "vpunpckhwd" => self.encode_avx_3op(ops, 0x69, true),
            "vpunpckldq" => self.encode_avx_3op(ops, 0x62, true),
            "vpunpckhdq" => self.encode_avx_3op(ops, 0x6A, true),
            "vpunpcklqdq" => self.encode_avx_3op(ops, 0x6C, true),
            "vpunpckhqdq" => self.encode_avx_3op(ops, 0x6D, true),
            "vpmullw" => self.encode_avx_3op(ops, 0xD5, true),
            "vpmulld" => self.encode_avx_3op_38(ops, 0x40, true),
            "vpmuludq" => self.encode_avx_3op(ops, 0xF4, true),
            "vpsllw" => self.encode_avx_shift(ops, 0xF1, 6, 0x71, true),
            "vpslld" => self.encode_avx_shift(ops, 0xF2, 6, 0x72, true),
            "vpsllq" => self.encode_avx_shift(ops, 0xF3, 6, 0x73, true),
            "vpsrlw" => self.encode_avx_shift(ops, 0xD1, 2, 0x71, true),
            "vpsrld" => self.encode_avx_shift(ops, 0xD2, 2, 0x72, true),
            "vpsrlq" => self.encode_avx_shift(ops, 0xD3, 2, 0x73, true),
            "vpsraw" => self.encode_avx_shift(ops, 0xE1, 4, 0x71, true),
            "vpsrad" => self.encode_avx_shift(ops, 0xE2, 4, 0x72, true),

            // AVX2 integer comparisons (additional)
            "vpcmpeqq" => self.encode_avx_3op_38(ops, 0x29, true),
            "vpcmpgtq" => self.encode_avx_3op_38(ops, 0x37, true),

            // AVX2 broadcast
            "vpbroadcastb" => self.encode_avx_broadcast(ops, &[0x78]),
            "vpbroadcastw" => self.encode_avx_broadcast(ops, &[0x79]),
            "vpbroadcastd" => self.encode_avx_broadcast(ops, &[0x58]),
            "vpbroadcastq" => self.encode_avx_broadcast(ops, &[0x59]),

            // AVX2 permute
            "vperm2i128" => self.encode_avx_3op_3a_imm8(ops, 0x46, true),
            "vperm2f128" => self.encode_avx_3op_3a_imm8(ops, 0x06, true),
            "vpermd" => self.encode_avx_3op_38(ops, 0x36, true),
            "vpermq" => self.encode_avx_shuffle_3a_w1(ops, 0x00, true),

            // AVX blend (additional)
            "vpblendd" => self.encode_avx_3op_3a_imm8(ops, 0x02, true),
            "vblendvps" => self.encode_avx_4op_3a(ops, 0x4A, true),
            "vblendvpd" => self.encode_avx_4op_3a(ops, 0x4B, true),
            "vpblendvb" => self.encode_avx_4op_3a(ops, 0x4C, true),

            // AVX2 broadcast from 128-bit memory
            "vbroadcasti128" => self.encode_avx_broadcast(ops, &[0x5A]),

            // AVX AES-NI
            "vaesenc" => self.encode_avx_3op_38(ops, 0xDC, true),
            "vaesenclast" => self.encode_avx_3op_38(ops, 0xDD, true),
            "vaesdec" => self.encode_avx_3op_38(ops, 0xDE, true),
            "vaesdeclast" => self.encode_avx_3op_38(ops, 0xDF, true),

            // AVX PCLMULQDQ
            "vpclmulqdq" => self.encode_avx_3op_3a_imm8(ops, 0x44, true),

            // Additional AVX packed integer
            "vpminub" => self.encode_avx_3op(ops, 0xDA, true),
            "vpmaxub" => self.encode_avx_3op(ops, 0xDE, true),
            "vpminsd" => self.encode_avx_3op_38(ops, 0x39, true),
            "vpmaxsd" => self.encode_avx_3op_38(ops, 0x3D, true),
            "vpminsb" => self.encode_avx_3op_38(ops, 0x38, true),
            "vpmaxsb" => self.encode_avx_3op_38(ops, 0x3C, true),
            "vpminuw" => self.encode_avx_3op_38(ops, 0x3A, true),
            "vpmaxuw" => self.encode_avx_3op_38(ops, 0x3E, true),
            "vpminsw" => self.encode_avx_3op(ops, 0xEA, true),
            "vpmaxsw" => self.encode_avx_3op(ops, 0xEE, true),
            "vpminud" => self.encode_avx_3op_38(ops, 0x3B, true),
            "vpmaxud" => self.encode_avx_3op_38(ops, 0x3F, true),
            "vpavgb" => self.encode_avx_3op(ops, 0xE0, true),
            "vpavgw" => self.encode_avx_3op(ops, 0xE3, true),
            "vpsadbw" => self.encode_avx_3op(ops, 0xF6, true),
            "vpmaddwd" => self.encode_avx_3op(ops, 0xF5, true),
            "vpmulhw" => self.encode_avx_3op(ops, 0xE5, true),
            "vpsubusb" => self.encode_avx_3op(ops, 0xD8, true),
            "vpsubusw" => self.encode_avx_3op(ops, 0xD9, true),
            "vpackuswb" => self.encode_avx_3op(ops, 0x67, true),
            "vpackssdw" => self.encode_avx_3op(ops, 0x6B, true),
            "vpackusdw" => self.encode_avx_3op_38(ops, 0x2B, true),
            "vpacksswb" => self.encode_avx_3op(ops, 0x63, true),
            "vpaddusb" => self.encode_avx_3op(ops, 0xDC, true),
            "vpaddusw" => self.encode_avx_3op(ops, 0xDD, true),
            "vpaddsb" => self.encode_avx_3op(ops, 0xEC, true),
            "vpaddsw" => self.encode_avx_3op(ops, 0xED, true),
            "vpsubsb" => self.encode_avx_3op(ops, 0xE8, true),
            "vpsubsw" => self.encode_avx_3op(ops, 0xE9, true),

            // AVX float misc (vshufps/vshufpd are in 0F map, not 0F3A)
            "vshufps" => self.encode_avx_3op_0f_imm8(ops, 0xC6, false),
            "vshufpd" => self.encode_avx_3op_0f_imm8(ops, 0xC6, true),
            "vunpcklpd" => self.encode_avx_3op(ops, 0x14, true),
            "vunpckhpd" => self.encode_avx_3op(ops, 0x15, true),
            "vunpcklps" => self.encode_avx_3op_np(ops, 0x14),
            "vunpckhps" => self.encode_avx_3op_np(ops, 0x15),
            "vmovlhps" => self.encode_avx_3op_np(ops, 0x16),
            "vmovhlps" => self.encode_avx_3op_np(ops, 0x12),
            "vmovddup" => self.encode_avx_2op_0f(ops, 0x12, 3),  // VEX.F2.0F 12 /r
            "vmovshdup" => self.encode_avx_2op_0f(ops, 0x16, 2), // VEX.F3.0F 16 /r
            "vmovsldup" => self.encode_avx_2op_0f(ops, 0x12, 2), // VEX.F3.0F 12 /r

            // AVX comparison with immediate (vcmpps/vcmppd/vcmpss/vcmpsd)
            "vcmpps" => self.encode_avx_3op_0f_imm8(ops, 0xC2, false),
            "vcmppd" => self.encode_avx_3op_0f_imm8(ops, 0xC2, true),
            "vcmpss" => self.encode_avx_cmp_scalar(ops, 0xC2, 2), // VEX.NDS.LIG.F3.0F C2 /r ib
            "vcmpsd" => self.encode_avx_cmp_scalar(ops, 0xC2, 3), // VEX.NDS.LIG.F2.0F C2 /r ib

            // AVX scalar float operations (VEX.NDS.LIG.F3/F2.0F)
            "vmovss" => self.encode_avx_scalar_mov(ops, 0x10, 0x11, 2), // F3 prefix
            "vmovsd" if !ops.is_empty() => self.encode_avx_scalar_mov(ops, 0x10, 0x11, 3), // F2 prefix
            "vaddss" => self.encode_avx_scalar_3op(ops, 0x58, 2),   // VEX.NDS.LIG.F3.0F 58
            "vsubss" => self.encode_avx_scalar_3op(ops, 0x5C, 2),   // VEX.NDS.LIG.F3.0F 5C
            "vmulss" => self.encode_avx_scalar_3op(ops, 0x59, 2),   // VEX.NDS.LIG.F3.0F 59
            "vdivss" => self.encode_avx_scalar_3op(ops, 0x5E, 2),   // VEX.NDS.LIG.F3.0F 5E
            "vaddsd" => self.encode_avx_scalar_3op(ops, 0x58, 3),   // VEX.NDS.LIG.F2.0F 58
            "vsubsd" => self.encode_avx_scalar_3op(ops, 0x5C, 3),   // VEX.NDS.LIG.F2.0F 5C
            "vmulsd" => self.encode_avx_scalar_3op(ops, 0x59, 3),   // VEX.NDS.LIG.F2.0F 59
            "vdivsd" => self.encode_avx_scalar_3op(ops, 0x5E, 3),   // VEX.NDS.LIG.F2.0F 5E
            "vmaxps" => self.encode_avx_3op_np(ops, 0x5F),
            "vminps" => self.encode_avx_3op_np(ops, 0x5D),
            "vmaxpd" => self.encode_avx_3op(ops, 0x5F, true),
            "vminpd" => self.encode_avx_3op(ops, 0x5D, true),
            "vmaxss" => self.encode_avx_scalar_3op(ops, 0x5F, 2),
            "vminss" => self.encode_avx_scalar_3op(ops, 0x5D, 2),
            "vmaxsd" => self.encode_avx_scalar_3op(ops, 0x5F, 3),
            "vminsd" => self.encode_avx_scalar_3op(ops, 0x5D, 3),

            // AVX extract/permute
            "vextractf128" => self.encode_avx_extract_imm8(ops, 0x19, true), // VEX.256.66.0F3A 19 /r ib
            "vpermilps" => self.encode_avx_shuffle_3a(ops, 0x04, true),      // VEX.256.66.0F3A 04 /r ib (imm form)
            "vpermilpd" => self.encode_avx_shuffle_3a(ops, 0x05, true),      // VEX.256.66.0F3A 05 /r ib (imm form)

            // AVX conversions
            "vcvtps2dq" => self.encode_avx_2op_0f(ops, 0x5B, 1),        // VEX.128/256.66.0F 5B /r
            "vcvtdq2ps" => self.encode_avx_2op_0f(ops, 0x5B, 0),        // VEX.128/256.0F 5B /r
            "vcvttps2dq" => self.encode_avx_2op_0f(ops, 0x5B, 2),       // VEX.128/256.F3.0F 5B /r

            // AVX insert/extract
            "vpinsrb" => self.encode_avx_insert_gp(ops, 0x20, true),
            "vpinsrw" => self.encode_avx_insert_gp_0f(ops, 0xC4, true),
            "vpinsrd" => self.encode_avx_insert_gp(ops, 0x22, true),
            "vpinsrq" if ops.len() == 4 => {
                // Already handled above, but catch the 4-op form
                self.encode_avx_insert_gp_w1(ops, 0x22, true)
            }
            "vpextrb" => self.encode_avx_extract_byte(ops, 0x14, true),
            "vpextrd" => self.encode_avx_extract_byte(ops, 0x16, true),
            "vpextrw" => self.encode_avx_extract_byte(ops, 0x15, true),

            // ---- AVX-512 (EVEX-encoded) ----
            "vpxord" => self.encode_evex_3op(ops, 0xEF, 1, 0),  // EVEX.NDS.{128,256,512}.66.0F.W0 EF /r
            "vpxorq" => self.encode_evex_3op(ops, 0xEF, 1, 1),  // EVEX.NDS.{128,256,512}.66.0F.W1 EF /r
            "vpandd" => self.encode_evex_3op(ops, 0xDB, 1, 0),  // EVEX.NDS.66.0F.W0 DB /r
            "vpandq" => self.encode_evex_3op(ops, 0xDB, 1, 1),  // EVEX.NDS.66.0F.W1 DB /r
            "vpord" => self.encode_evex_3op(ops, 0xEB, 1, 0),   // EVEX.NDS.66.0F.W0 EB /r
            "vporq" => self.encode_evex_3op(ops, 0xEB, 1, 1),   // EVEX.NDS.66.0F.W1 EB /r
            // AVX-512 packed rotate by immediate
            "vprold" => self.encode_evex_rotate_imm(ops, 0x72, 1, 0),  // EVEX.NDS.66.0F.W0 72 /1 ib
            "vprolq" => self.encode_evex_rotate_imm(ops, 0x72, 1, 1),  // EVEX.NDS.66.0F.W1 72 /1 ib
            "vprord" => self.encode_evex_rotate_imm(ops, 0x72, 0, 0),  // EVEX.NDS.66.0F.W0 72 /0 ib
            "vprorq" => self.encode_evex_rotate_imm(ops, 0x72, 0, 1),  // EVEX.NDS.66.0F.W1 72 /0 ib

            // ---- BMI2 instructions (VEX-encoded, GPR) ----
            "shrxq" => self.encode_bmi2_shift(ops, 0xF7, 3, 1), // F2.0F38.W1
            "shrxl" => self.encode_bmi2_shift(ops, 0xF7, 3, 0), // F2.0F38.W0
            "shlxq" => self.encode_bmi2_shift(ops, 0xF7, 1, 1), // 66.0F38.W1
            "shlxl" => self.encode_bmi2_shift(ops, 0xF7, 1, 0), // 66.0F38.W0
            "sarxq" => self.encode_bmi2_shift(ops, 0xF7, 2, 1), // F3.0F38.W1
            "sarxl" => self.encode_bmi2_shift(ops, 0xF7, 2, 0), // F3.0F38.W0
            "rorxq" => self.encode_bmi2_rorx(ops, 1),            // F2.0F3A.W1 F0 /r imm8
            "rorxl" => self.encode_bmi2_rorx(ops, 0),            // F2.0F3A.W0 F0 /r imm8
            "bzhiq" => self.encode_bmi2_shift(ops, 0xF5, 0, 1), // NP.0F38.W1
            "bzhil" => self.encode_bmi2_shift(ops, 0xF5, 0, 0), // NP.0F38.W0
            "pextq" => self.encode_bmi2_shift(ops, 0xF5, 2, 1), // F3.0F38.W1
            "pextl" => self.encode_bmi2_shift(ops, 0xF5, 2, 0), // F3.0F38.W0
            "pdepq" => self.encode_bmi2_shift(ops, 0xF5, 3, 1), // F2.0F38.W1
            "pdepl" => self.encode_bmi2_shift(ops, 0xF5, 3, 0), // F2.0F38.W0
            "mulxq" => self.encode_bmi2_shift(ops, 0xF6, 3, 1), // F2.0F38.W1
            "mulxl" => self.encode_bmi2_shift(ops, 0xF6, 3, 0), // F2.0F38.W0
            "andnq" => self.encode_bmi_andn(ops, 1), // NP.0F38.W1
            "andnl" => self.encode_bmi_andn(ops, 0), // NP.0F38.W0
            "bextrl" | "bextrq" => {
                let w = if mnemonic == "bextrq" { 1 } else { 0 };
                self.encode_bmi2_shift(ops, 0xF7, 0, w)          // NP.0F38.Wx
            }
            // BMI2 suffix-less forms: infer 32/64-bit from destination register
            "shrx" => { let w = self.bmi2_infer_w(ops); self.encode_bmi2_shift(ops, 0xF7, 3, w) }
            "shlx" => { let w = self.bmi2_infer_w(ops); self.encode_bmi2_shift(ops, 0xF7, 1, w) }
            "sarx" => { let w = self.bmi2_infer_w(ops); self.encode_bmi2_shift(ops, 0xF7, 2, w) }
            "rorx" => { let w = self.bmi2_infer_w(ops); self.encode_bmi2_rorx(ops, w) }
            "bzhi" => { let w = self.bmi2_infer_w(ops); self.encode_bmi2_shift(ops, 0xF5, 0, w) }
            "pext" => { let w = self.bmi2_infer_w(ops); self.encode_bmi2_shift(ops, 0xF5, 2, w) }
            "pdep" => { let w = self.bmi2_infer_w(ops); self.encode_bmi2_shift(ops, 0xF5, 3, w) }
            "mulx" => { let w = self.bmi2_infer_w(ops); self.encode_bmi2_shift(ops, 0xF6, 3, w) }
            "andn" => { let w = self.bmi2_infer_w(ops); self.encode_bmi_andn(ops, w) }
            "bextr" => { let w = self.bmi2_infer_w(ops); self.encode_bmi2_shift(ops, 0xF7, 0, w) }

            // ---- Suffix-less forms (infer size from operands) ----
            // These are commonly emitted by inline asm
            "push" => self.encode_push(ops),
            "pop" => self.encode_pop(ops),
            "mov" => self.encode_suffixless_mov(ops),
            "add" => self.encode_suffixless_alu(ops, 0),
            "sub" => self.encode_suffixless_alu(ops, 5),
            "xor" => self.encode_suffixless_alu(ops, 6),
            "and" => self.encode_suffixless_alu(ops, 4),
            "or" => self.encode_suffixless_alu(ops, 1),
            "cmp" => self.encode_suffixless_alu(ops, 7),
            "test" => self.encode_suffixless_test(ops),
            "shr" => self.encode_suffixless_shift(ops, 5),
            "shl" | "sal" => self.encode_suffixless_shift(ops, 4),
            "sar" => self.encode_suffixless_shift(ops, 7),
            "inc" => self.encode_suffixless_unary(ops, 0),
            "dec" => self.encode_suffixless_unary(ops, 1),
            "neg" => self.encode_suffixless_unary(ops, 3),
            "not" => self.encode_suffixless_unary(ops, 2),
            "lea" => {
                // Infer size from destination register
                if ops.len() == 2 {
                    if let Operand::Register(dst) = &ops[1] {
                        let size = infer_reg_size(&dst.name);
                        return self.encode_lea(ops, size);
                    }
                }
                Err("unsupported lea operands".to_string())
            }
            "xchg" => {
                if ops.len() == 2 {
                    let size = infer_operand_size_from_pair(&ops[0], &ops[1]);
                    let suffix_mnemonic = match size {
                        1 => "xchgb", 2 => "xchgw", 4 => "xchgl", _ => "xchgq",
                    };
                    self.encode_xchg(ops, suffix_mnemonic)
                } else {
                    Err("xchg requires 2 operands".to_string())
                }
            }
            "imul" => self.encode_imul(ops, 8), // default to 64-bit for suffix-less
            "mul" => {
                let size = if let Some(Operand::Register(r)) = ops.first() { infer_reg_size(&r.name) } else { 8 };
                if size == 2 { self.bytes.push(0x66); }
                self.encode_unary_rm(ops, 4, size)
            }
            "div" => {
                let size = if let Some(Operand::Register(r)) = ops.first() { infer_reg_size(&r.name) } else { 8 };
                if size == 2 { self.bytes.push(0x66); }
                self.encode_unary_rm(ops, 6, size)
            }
            "idiv" => {
                let size = if let Some(Operand::Register(r)) = ops.first() { infer_reg_size(&r.name) } else { 8 };
                if size == 2 { self.bytes.push(0x66); }
                self.encode_unary_rm(ops, 7, size)
            }
            "bswap" => {
                let size = if let Some(Operand::Register(r)) = ops.first() {
                    infer_reg_size(&r.name)
                } else { 8 };
                self.encode_bswap(ops, size)
            }
            "bsf" => self.encode_bit_scan(ops, "bsfq", 0xBC),
            "bsr" => self.encode_bit_scan(ops, "bsrq", 0xBD),
            "cmovzq" | "cmovnzq" | "cmovsq" | "cmovnsq" | "cmovpq" | "cmovnpq" => self.encode_cmovcc(ops, mnemonic),
            "cmovzl" | "cmovnzl" | "cmovsl" | "cmovnsl" | "cmovpl" | "cmovnpl" => self.encode_cmovcc(ops, mnemonic),
            "cmovzw" | "cmovnzw" | "cmovsw" | "cmovnsw" | "cmovpw" | "cmovnpw" => self.encode_cmovcc(ops, mnemonic),

            // Suffix-less cmov (infer from operand size)
            "cmovz" | "cmovnz" | "cmovs" | "cmovns" | "cmovp" | "cmovnp"
            | "cmove" | "cmovne" | "cmovl" | "cmovle" | "cmovg" | "cmovge"
            | "cmovb" | "cmovbe" | "cmova" | "cmovae"
            | "cmovc" | "cmovnc" | "cmovno" | "cmovo"
            | "cmovna" | "cmovnb" | "cmovnbe" | "cmovnge" | "cmovng"
            | "cmovnle" | "cmovnl" | "cmovpe" | "cmovpo" | "cmovnae" => {
                if ops.len() == 2 {
                    let size = infer_operand_size_from_pair(&ops[0], &ops[1]);
                    let suffix = match size {
                        8 => "q",
                        4 => "l",
                        2 => "w",
                        _ => "l",
                    };
                    let new_mnemonic = format!("{}{}", mnemonic, suffix);
                    self.encode_cmovcc(ops, &new_mnemonic)
                } else {
                    Err("cmov requires 2 operands".to_string())
                }
            }

            // Additional cmov variants with size suffix
            "cmovcq" | "cmovncq" | "cmovnaq" | "cmovnbeq"
            | "cmovngeq" | "cmovngq" | "cmovnleq" | "cmovnlq"
            | "cmovnoq" | "cmovnaeq" | "cmovnbq" | "cmovoq" | "cmovpeq" | "cmovpoq"
            | "cmovcl" | "cmovncl" | "cmovnal" | "cmovnbel"
            | "cmovngel" | "cmovngl" | "cmovnlel" | "cmovnll"
            | "cmovnol" | "cmovnael" | "cmovnbl" | "cmovol" | "cmovpel" | "cmovpol"
            | "cmovcw" | "cmovncw" | "cmovnaw" | "cmovnbew"
            | "cmovngew" | "cmovngw" | "cmovnlew" | "cmovnlw"
            | "cmovnow" | "cmovnaew" | "cmovnbw" | "cmovow" | "cmovpew" | "cmovpow" => self.encode_cmovcc(ops, mnemonic),

            // Additional set instructions
            "setcc" => self.encode_setcc(ops, "setc"),

            // ---- imul with suffix forms we haven't caught ----
            "imulw" => self.encode_imul(ops, 2),

            // ---- Additional ADC/SBB sizes ----
            "adcw" | "adcb" => self.encode_alu(ops, mnemonic, 2),
            "sbbw" | "sbbb" => self.encode_alu(ops, mnemonic, 3),

            // ---- divw, divb, idivw, idivb, mulb ----
            "divw" => { self.bytes.push(0x66); self.encode_unary_rm(ops, 6, 2) }
            "divb" => self.encode_unary_rm(ops, 6, 1),
            "idivw" => { self.bytes.push(0x66); self.encode_unary_rm(ops, 7, 2) }
            "idivb" => self.encode_unary_rm(ops, 7, 1),
            "mulb" => self.encode_unary_rm(ops, 4, 1),

            // ---- CBW/CWDE/CWD ----
            "cbw" => { self.bytes.extend_from_slice(&[0x66, 0x98]); Ok(()) }
            "cwd" => { self.bytes.extend_from_slice(&[0x66, 0x99]); Ok(()) }

            // ---- AVX: vzeroupper ----
            "vzeroupper" => {
                // VEX.128.0F.WIG 77
                self.emit_vex(false, false, false, 1, 0, 0, 0, 0);
                self.bytes.push(0x77);
                Ok(())
            }
            "vzeroall" => {
                // VEX.256.0F.WIG 77
                self.emit_vex(false, false, false, 1, 0, 0, 1, 0);
                self.bytes.push(0x77);
                Ok(())
            }

            // Additional AVX operations
            "vpaddq" => self.encode_avx_3op(ops, 0xD4, true),
            "vpsubq" => self.encode_avx_3op(ops, 0xFB, true),
            "vpsrldq" => {
                // VEX.NDD.128/256.66.0F 73 /3 ib
                if ops.len() != 3 { return Err("vpsrldq requires 3 operands".to_string()); }
                match (&ops[0], &ops[1], &ops[2]) {
                    (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                        let src_num = reg_num(&src.name).ok_or("bad register")?;
                        let l = if is_ymm(&src.name) || is_ymm(&dst.name) { 1 } else { 0 };
                        let b = needs_vex_ext(&src.name);
                        let dst_num_full = reg_num(&dst.name).ok_or("bad register")? | (if needs_vex_ext(&dst.name) { 8 } else { 0 });
                        self.emit_vex(false, false, b, 1, 0, dst_num_full, l, 1);
                        self.bytes.push(0x73);
                        self.bytes.push(self.modrm(3, 3, src_num));
                        self.bytes.push(*imm as u8);
                        Ok(())
                    }
                    _ => Err("unsupported vpsrldq operands".to_string()),
                }
            }
            "vpslldq" => {
                if ops.len() != 3 { return Err("vpslldq requires 3 operands".to_string()); }
                match (&ops[0], &ops[1], &ops[2]) {
                    (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                        let src_num = reg_num(&src.name).ok_or("bad register")?;
                        let l = if is_ymm(&src.name) || is_ymm(&dst.name) { 1 } else { 0 };
                        let b = needs_vex_ext(&src.name);
                        let dst_num_full = reg_num(&dst.name).ok_or("bad register")? | (if needs_vex_ext(&dst.name) { 8 } else { 0 });
                        self.emit_vex(false, false, b, 1, 0, dst_num_full, l, 1);
                        self.bytes.push(0x73);
                        self.bytes.push(self.modrm(3, 7, src_num));
                        self.bytes.push(*imm as u8);
                        Ok(())
                    }
                    _ => Err("unsupported vpslldq operands".to_string()),
                }
            }
            "vptest" => {
                // VEX.128.66.0F38 17 /r
                if ops.len() != 2 { return Err("vptest requires 2 operands".to_string()); }
                match (&ops[0], &ops[1]) {
                    (Operand::Register(src), Operand::Register(dst)) => {
                        let src_num = reg_num(&src.name).ok_or("bad register")?;
                        let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                        let l = if is_ymm(&src.name) || is_ymm(&dst.name) { 1 } else { 0 };
                        let r = needs_vex_ext(&dst.name);
                        let b = needs_vex_ext(&src.name);
                        self.emit_vex(r, false, b, 2, 0, 0, l, 1);
                        self.bytes.push(0x17);
                        self.bytes.push(self.modrm(3, dst_num, src_num));
                        Ok(())
                    }
                    _ => Err("unsupported vptest operands".to_string()),
                }
            }
            "vpextrq" => {
                // VEX.128.66.0F3A.W1 16 /r ib
                if ops.len() != 3 { return Err("vpextrq requires 3 operands".to_string()); }
                match (&ops[0], &ops[1], &ops[2]) {
                    (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                        let src_num = reg_num(&src.name).ok_or("bad register")?;
                        let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                        let r = needs_vex_ext(&src.name);
                        let b = needs_vex_ext(&dst.name);
                        self.emit_vex(r, false, b, 3, 1, 0, 0, 1);
                        self.bytes.push(0x16);
                        self.bytes.push(self.modrm(3, src_num, dst_num));
                        self.bytes.push(*imm as u8);
                        Ok(())
                    }
                    _ => Err("unsupported vpextrq operands".to_string()),
                }
            }
            "vpinsrq" => {
                // VEX.128.66.0F3A.W1 22 /r ib
                if ops.len() != 4 { return Err("vpinsrq requires 4 operands".to_string()); }
                match (&ops[0], &ops[1], &ops[2], &ops[3]) {
                    (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(vvvv), Operand::Register(dst)) => {
                        let src_num = reg_num(&src.name).ok_or("bad register")?;
                        let vvvv_num = reg_num(&vvvv.name).ok_or("bad register")?;
                        let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                        let r = needs_vex_ext(&dst.name);
                        let b = needs_vex_ext(&src.name);
                        let vvvv_enc = vvvv_num | (if needs_vex_ext(&vvvv.name) { 8 } else { 0 });
                        self.emit_vex(r, false, b, 3, 1, vvvv_enc, 0, 1);
                        self.bytes.push(0x22);
                        self.bytes.push(self.modrm(3, dst_num, src_num));
                        self.bytes.push(*imm as u8);
                        Ok(())
                    }
                    _ => Err("unsupported vpinsrq operands".to_string()),
                }
            }

            // ---- Suffix-less shrd/shld ----
            "shrd" => {
                if ops.len() == 3 {
                    let size = match &ops[2] {
                        Operand::Register(r) => infer_reg_size(&r.name),
                        _ => 8,
                    };
                    let opcode = 0xACu8;
                    self.encode_double_shift(ops, opcode, size)
                } else {
                    Err("shrd requires 3 operands".to_string())
                }
            }
            "shld" => {
                if ops.len() == 3 {
                    let size = match &ops[2] {
                        Operand::Register(r) => infer_reg_size(&r.name),
                        _ => 8,
                    };
                    let opcode = 0xA4u8;
                    self.encode_double_shift(ops, opcode, size)
                } else {
                    Err("shld requires 3 operands".to_string())
                }
            }

            // ---- Suffix-less adc/sbb ----
            "adc" => self.encode_suffixless_alu(ops, 2),
            "sbb" => self.encode_suffixless_alu(ops, 3),

            // ---- Additional x87 ----
            "fistpl" => self.encode_x87_mem(ops, &[0xDB], 3),
            "fistl" => self.encode_x87_mem(ops, &[0xDB], 2),
            "fildl" => self.encode_x87_mem(ops, &[0xDB], 0),
            "filds" => self.encode_x87_mem(ops, &[0xDF], 0),
            "fistps" => self.encode_x87_mem(ops, &[0xDF], 3),
            "fisttpl" => self.encode_x87_mem(ops, &[0xDB], 1),
            "fdivl" => self.encode_x87_mem(ops, &[0xDC], 6),
            "fdivs" => self.encode_x87_mem(ops, &[0xD8], 6),
            "fmull" => self.encode_x87_mem(ops, &[0xDC], 1),
            "fmuls" => self.encode_x87_mem(ops, &[0xD8], 1),
            "fsubl" => self.encode_x87_mem(ops, &[0xDC], 4),
            "fsubs" => self.encode_x87_mem(ops, &[0xD8], 4),
            "faddl" => self.encode_x87_mem(ops, &[0xDC], 0),
            "fadds" => self.encode_x87_mem(ops, &[0xD8], 0),
            // fdivrp with 2 operands is handled by the no-operand form above

            // ---- lock cmpxchg8b ----
            "cmpxchg8b" => {
                if ops.len() != 1 { return Err("cmpxchg8b requires 1 operand".to_string()); }
                match &ops[0] {
                    Operand::Memory(mem) => {
                        self.emit_rex_rm(0, "", mem);
                        self.bytes.extend_from_slice(&[0x0F, 0xC7]);
                        self.encode_modrm_mem(1, mem)
                    }
                    _ => Err("cmpxchg8b requires memory operand".to_string()),
                }
            }

            // ---- MMX paddb ----
            "paddb" if ops.iter().any(|op| matches!(op, Operand::Register(r) if is_mmx(&r.name))) => {
                // MMX form: 0F FC /r
                if ops.len() != 2 { return Err("paddb requires 2 operands".to_string()); }
                match (&ops[0], &ops[1]) {
                    (Operand::Register(src), Operand::Register(dst)) if is_mmx(&src.name) && is_mmx(&dst.name) => {
                        let src_num = reg_num(&src.name).ok_or("bad register")?;
                        let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                        self.bytes.extend_from_slice(&[0x0F, 0xFC]);
                        self.bytes.push(self.modrm(3, dst_num, src_num));
                        Ok(())
                    }
                    (Operand::Memory(mem), Operand::Register(dst)) if is_mmx(&dst.name) => {
                        let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                        self.bytes.extend_from_slice(&[0x0F, 0xFC]);
                        self.encode_modrm_mem(dst_num, mem)
                    }
                    _ => Err("unsupported mmx paddb operands".to_string()),
                }
            }
            "paddb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xFC]),

            // ---- Additional suffixless forms ----
            "bt" => self.encode_bt(ops, "btq"),
            "bts" => self.encode_bt(ops, "btsq"),
            "btr" => self.encode_bt(ops, "btrq"),
            "btc" => self.encode_bt(ops, "btcq"),
            "rcl" => self.encode_suffixless_shift(ops, 2),
            "rcr" => self.encode_suffixless_shift(ops, 3),
            "rol" => self.encode_suffixless_shift(ops, 0),
            "ror" => self.encode_suffixless_shift(ops, 1),

            // ---- rep/lock with nothing or partial ----
            "rep" => {
                // rep; by itself just emits the prefix (for "rep; nop" = pause)
                self.bytes.push(0xF3);
                Ok(())
            }
            "lock" => {
                // lock; by itself just emits the prefix byte
                self.bytes.push(0xF0);
                Ok(())
            }

            // 1-operand shift forms are now handled in encode_shift directly

            // ---- Additional x87 forms from upstream ----
            "fsts" => self.encode_x87_mem(ops, &[0xD9], 2),

            // x87 constants
            "fld1" => { self.bytes.extend_from_slice(&[0xD9, 0xE8]); Ok(()) }
            "fldl2e" => { self.bytes.extend_from_slice(&[0xD9, 0xEA]); Ok(()) }
            "fldlg2" => { self.bytes.extend_from_slice(&[0xD9, 0xEC]); Ok(()) }
            "fldln2" => { self.bytes.extend_from_slice(&[0xD9, 0xED]); Ok(()) }
            "fldz" => { self.bytes.extend_from_slice(&[0xD9, 0xEE]); Ok(()) }
            "fldpi" => { self.bytes.extend_from_slice(&[0xD9, 0xEB]); Ok(()) }
            "fldl2t" => { self.bytes.extend_from_slice(&[0xD9, 0xE9]); Ok(()) }

            // x87 arithmetic (zero-operand forms operate on st(0),st(1))
            "fabs" => { self.bytes.extend_from_slice(&[0xD9, 0xE1]); Ok(()) }
            "fsqrt" => { self.bytes.extend_from_slice(&[0xD9, 0xFA]); Ok(()) }
            "frndint" => { self.bytes.extend_from_slice(&[0xD9, 0xFC]); Ok(()) }
            "f2xm1" => { self.bytes.extend_from_slice(&[0xD9, 0xF0]); Ok(()) }
            "fscale" => { self.bytes.extend_from_slice(&[0xD9, 0xFD]); Ok(()) }
            "fpatan" => { self.bytes.extend_from_slice(&[0xD9, 0xF3]); Ok(()) }
            "fprem" => { self.bytes.extend_from_slice(&[0xD9, 0xF8]); Ok(()) }
            "fprem1" => { self.bytes.extend_from_slice(&[0xD9, 0xF5]); Ok(()) }
            "fyl2x" => { self.bytes.extend_from_slice(&[0xD9, 0xF1]); Ok(()) }
            "fyl2xp1" => { self.bytes.extend_from_slice(&[0xD9, 0xF9]); Ok(()) }
            "fptan" => { self.bytes.extend_from_slice(&[0xD9, 0xF2]); Ok(()) }
            "fsin" => { self.bytes.extend_from_slice(&[0xD9, 0xFE]); Ok(()) }
            "fcos" => { self.bytes.extend_from_slice(&[0xD9, 0xFF]); Ok(()) }
            "fxtract" => { self.bytes.extend_from_slice(&[0xD9, 0xF4]); Ok(()) }

            // x87 register-register operations
            "fadd" => self.encode_x87_arith_reg(ops, 0xD8, 0xDC, 0xC0),
            "fmul" => self.encode_x87_arith_reg(ops, 0xD8, 0xDC, 0xC8),
            "fsub" => self.encode_x87_arith_reg(ops, 0xD8, 0xDC, 0xE0),
            "fdiv" => self.encode_x87_arith_reg(ops, 0xD8, 0xDC, 0xF0),

            // x87 exchange
            "fxch" => self.encode_fxch(ops),

            // x87 control word / environment
            "fnclex" => { self.bytes.extend_from_slice(&[0xDB, 0xE2]); Ok(()) }
            "fnstenv" => self.encode_x87_mem(ops, &[0xD9], 6),
            "fldenv" => self.encode_x87_mem(ops, &[0xD9], 4),
            "fnstsw" => {
                // fnstsw %ax -> DF E0
                // fnstsw mem -> DD /7
                if let Some(Operand::Register(r)) = ops.first() {
                    if r.name == "ax" {
                        self.bytes.extend_from_slice(&[0xDF, 0xE0]);
                        return Ok(());
                    }
                }
                self.encode_x87_mem(ops, &[0xDD], 7)
            }

            // SSE MXCSR control register
            "ldmxcsr" => self.encode_sse_mem_only(ops, &[0x0F, 0xAE], 2),
            "stmxcsr" => self.encode_sse_mem_only(ops, &[0x0F, 0xAE], 3),

            // Prefetch instructions (0F 18 /0-3)
            "prefetcht0" => self.encode_sse_mem_only(ops, &[0x0F, 0x18], 1),
            "prefetcht1" => self.encode_sse_mem_only(ops, &[0x0F, 0x18], 2),
            "prefetcht2" => self.encode_sse_mem_only(ops, &[0x0F, 0x18], 3),
            "prefetchnta" => self.encode_sse_mem_only(ops, &[0x0F, 0x18], 0),
            "prefetchw" => self.encode_sse_mem_only(ops, &[0x0F, 0x0D], 1),

            // RDRAND/RDSEED
            "rdrand" | "rdrandl" | "rdrandq" => self.encode_rdrand(ops, mnemonic, 0xC7, 6),
            "rdseed" | "rdseedl" | "rdseedq" => self.encode_rdrand(ops, mnemonic, 0xC7, 7),

            // ---- Instructions from i686 encoder for cohesion ----

            // SSE ordered compare (comisd/comiss)
            "comisd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x2F]),
            "comiss" => self.encode_sse_op(ops, &[0x0F, 0x2F]),

            // SSE compare with immediate (cmpsd/cmpss/cmppd/cmpps as SSE ops)
            "cmpsd" => self.encode_sse_op_imm8(ops, &[0xF2, 0x0F, 0xC2]),
            "cmpss" => self.encode_sse_op_imm8(ops, &[0xF3, 0x0F, 0xC2]),
            "cmppd" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0xC2]),
            "cmpps" => self.encode_sse_op_imm8(ops, &[0x0F, 0xC2]),

            // x87 additional from i686
            "ftst" => { self.bytes.extend_from_slice(&[0xD9, 0xE4]); Ok(()) }
            "fxam" => { self.bytes.extend_from_slice(&[0xD9, 0xE5]); Ok(()) }
            "fucomi" => self.encode_fucomi(ops),
            "fstl" => self.encode_x87_mem(ops, &[0xDD], 2),

            // x87 reverse memory arithmetic
            "fsubrl" => self.encode_x87_mem(ops, &[0xDC], 5),
            "fdivrl" => self.encode_x87_mem(ops, &[0xDC], 7),
            "fsubrs" => self.encode_x87_mem(ops, &[0xD8], 5),
            "fdivrs" => self.encode_x87_mem(ops, &[0xD8], 7),

            // Data movement: byte-to-word extensions
            "movsbw" => self.encode_movsx(ops, 1, 2),
            "movzbw" => self.encode_movzx(ops, 1, 2),

            // SSE packed integer (missing from x86)
            "pcmpgtd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x66]),
            "pmulhuw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE4]),
            "psubb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xF8]),

            // System instructions
            "rdmsr" => { self.bytes.extend_from_slice(&[0x0F, 0x32]); Ok(()) }
            "wrmsr" => { self.bytes.extend_from_slice(&[0x0F, 0x30]); Ok(()) }
            "xgetbv" => { self.bytes.extend_from_slice(&[0x0F, 0x01, 0xD0]); Ok(()) }
            "xsetbv" => { self.bytes.extend_from_slice(&[0x0F, 0x01, 0xD1]); Ok(()) }
            "rdpmc" => { self.bytes.extend_from_slice(&[0x0F, 0x33]); Ok(()) }
            "swapgs" => { self.bytes.extend_from_slice(&[0x0F, 0x01, 0xF8]); Ok(()) }
            "clts" => { self.bytes.extend_from_slice(&[0x0F, 0x06]); Ok(()) }
            "verw" => self.encode_verw(ops),
            "lsl" => self.encode_lsl(ops),
            "lar" => self.encode_lar(ops),
            "lldt" => self.encode_system_reg(ops, &[0x0F, 0x00], 2),  // 0F 00 /2
            "ltr" => self.encode_system_reg(ops, &[0x0F, 0x00], 3),   // 0F 00 /3
            "str" => self.encode_system_reg(ops, &[0x0F, 0x00], 1),   // 0F 00 /1
            "invlpg" => self.encode_mem_only(ops, &[0x0F, 0x01], 7),  // 0F 01 /7
            "invpcid" => self.encode_invpcid(ops),
            "rdgsbase" | "wrgsbase" | "rdfsbase" | "wrfsbase" => self.encode_fsgsbase(ops, mnemonic),
            "fxsave" => self.encode_mem_only(ops, &[0x0F, 0xAE], 0),   // 0F AE /0
            "fxrstor" => self.encode_mem_only(ops, &[0x0F, 0xAE], 1), // 0F AE /1
            "fxsaveq" | "fxsave64" => self.encode_fxsaveq(ops),   // REX.W + 0F AE /0
            "fxrstorq" | "fxrstor64" => self.encode_fxrstorq(ops),  // REX.W + 0F AE /1

            // System table instructions
            "sgdt" | "sidt" | "lgdt" | "lidt"
            | "sgdtl" | "sidtl" | "lgdtl" | "lidtl"
            | "sgdtq" | "sidtq" | "lgdtq" | "lidtq" => self.encode_system_table(ops, mnemonic),
            "lmsw" => self.encode_lmsw(ops),
            "smsw" => self.encode_smsw(ops),

            _ if mnemonic.starts_with("set") => {
                // Handle setXXb forms (e.g., setcb = setc with byte suffix)
                self.encode_setcc(ops, mnemonic)
            }

            // SSE comparison pseudo-ops: cmpXXps/pd/ss/sd -> cmpps/pd/ss/sd $imm
            _ if self.try_encode_sse_cmp_pseudo(ops, mnemonic)?.is_some() => Ok(()),

            // AVX comparison pseudo-ops: vcmpXXps/pd/ss/sd -> vcmpps/pd/ss/sd $imm
            _ if self.try_encode_avx_cmp_pseudo(ops, mnemonic)?.is_some() => Ok(()),

            _ => {
                // TODO: many more instructions to handle
                Err(format!("unhandled instruction: {} {:?}", mnemonic, ops))
            }
        }
    }
}
