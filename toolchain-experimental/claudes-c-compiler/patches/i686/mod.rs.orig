//! i686 (32-bit x86) instruction encoder.
//!
//! Encodes parsed i686 instructions into machine code bytes.
//! Similar to the x86-64 encoder but without REX prefixes and with
//! 32-bit default operand size. Uses R_386_* relocation types.

mod registers;
mod core;
mod gp_integer;
mod sse;
mod x87;
mod system;

pub(crate) use registers::*;

use crate::backend::x86::assembler::parser::*;

/// Split a label string like `"pa_tr_efer + 4"` or `"symbol-8"` into (symbol, addend).
/// Returns the original string with addend 0 if no offset is found.
fn split_label_offset(label: &str) -> (&str, i64) {
    // Scan for '+' or '-' that separates the symbol from the offset.
    // Skip the first character to avoid splitting on leading sign/dot.
    for (i, c) in label.char_indices().skip(1) {
        if c == '+' || c == '-' {
            let left = label[..i].trim();
            if left.is_empty() {
                continue;
            }
            // Extract the numeric part (including sign for '-')
            let num_str = if c == '+' {
                label[i + 1..].trim()
            } else {
                // Keep the '-' sign
                label[i..].trim()
            };
            if let Ok(offset) = num_str.parse::<i64>() {
                return (left, offset);
            }
        }
    }
    (label, 0)
}

/// Relocation entry for the linker to resolve.
#[derive(Debug, Clone)]
pub struct Relocation {
    /// Offset within the section where the relocation applies.
    pub offset: u64,
    /// Symbol name to resolve.
    pub symbol: String,
    /// Relocation type (ELF R_386_* constants).
    pub reloc_type: u32,
    /// Addend for the relocation (used in RELA; for REL format, embedded in instruction).
    pub addend: i64,
    /// For symbol difference expressions (A - B): the subtracted symbol.
    pub diff_symbol: Option<String>,
}

// ELF i386 relocation types
pub const R_386_32: u32 = 1;
pub const R_386_PC32: u32 = 2;
pub const R_386_GOT32: u32 = 3;
pub const R_386_PLT32: u32 = 4;
pub const R_386_GOTOFF: u32 = 9;
pub const R_386_GOTPC: u32 = 10;
pub const R_386_TLS_LE_32: u32 = 37;
pub const R_386_TLS_IE: u32 = 15;
pub const R_386_TLS_GD: u32 = 18;
pub const R_386_TLS_LDM: u32 = 19;
pub const R_386_TLS_LDO_32: u32 = 32;
#[allow(dead_code)] // ELF standard constant; not yet emitted by assembler but used by linker
pub const R_386_TLS_GOTIE: u32 = 16;
pub const R_386_32S: u32 = 38; // R_386_TLS_LE (negative offset from TP)

/// Instruction encoding context for i686.
pub struct InstructionEncoder {
    /// Output bytes.
    pub bytes: Vec<u8>,
    /// Relocations generated during encoding.
    pub relocations: Vec<Relocation>,
    /// Current offset within the section.
    pub offset: u64,
    /// Whether we are in .code16gcc mode (16-bit real mode with 32-bit instructions).
    /// Currently .code16gcc is handled at the assembly text level (prepended to asm output);
    /// this field is infrastructure for future per-instruction operand size overrides.
    #[allow(dead_code)]
    pub code16gcc: bool,
}

impl InstructionEncoder {
    pub fn new() -> Self {
        InstructionEncoder {
            bytes: Vec::new(),
            relocations: Vec::new(),
            offset: 0,
            code16gcc: false,
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
        let mnemonic = instr.mnemonic.as_str();
        let ops = &instr.operands;

        match mnemonic {
            // Data movement
            "movl" => self.encode_mov(ops, 4),
            "movw" => self.encode_mov(ops, 2),
            "movb" => self.encode_mov(ops, 1),
            // Unsuffixed mov from inline asm - infer size from operands
            "mov" => self.encode_mov_infer_size(ops),
            "movsbl" => self.encode_movsx(ops, 1, 4),
            "movswl" => self.encode_movsx(ops, 2, 4),
            "movsbw" => self.encode_movsx(ops, 1, 2),
            "movzbl" => self.encode_movzx(ops, 1, 4),
            "movzwl" => self.encode_movzx(ops, 2, 4),
            "movzbw" => self.encode_movzx(ops, 1, 2),

            // LEA
            "leal" | "lea" => self.encode_lea(ops, 4),

            // Stack ops (32-bit default)
            "pushl" | "push" => self.encode_push(ops),
            "popl" | "pop" => self.encode_pop(ops),
            // Also handle pushw/popw for 16-bit variants
            "pushw" => self.encode_push16(ops),
            "popw" => self.encode_pop16(ops),

            // Arithmetic
            "addl" | "addw" | "addb" | "add" => self.encode_alu(ops, mnemonic, 0),
            "orl" | "orw" | "orb" | "or" => self.encode_alu(ops, mnemonic, 1),
            "adcl" | "adcw" | "adcb" | "adc" => self.encode_alu(ops, mnemonic, 2),
            "sbbl" | "sbbw" | "sbbb" | "sbb" => self.encode_alu(ops, mnemonic, 3),
            "andl" | "andw" | "andb" | "and" => self.encode_alu(ops, mnemonic, 4),
            "subl" | "subw" | "subb" | "sub" => self.encode_alu(ops, mnemonic, 5),
            "xorl" | "xorw" | "xorb" | "xor" => self.encode_alu(ops, mnemonic, 6),
            "cmpl" | "cmpw" | "cmpb" | "cmp" => self.encode_alu(ops, mnemonic, 7),
            "testl" | "testw" | "testb" | "test" => self.encode_test(ops, mnemonic),

            // Multiply/divide
            "imull" | "imul" => self.encode_imul(ops, 4),
            "mull" | "mul" => self.encode_unary_rm(ops, 4, 4),
            "divl" | "div" => self.encode_unary_rm(ops, 6, 4),
            "idivl" | "idiv" => self.encode_unary_rm(ops, 7, 4),

            // Unary
            "negl" | "neg" => self.encode_unary_rm(ops, 3, 4),
            "negw" => self.encode_unary_rm(ops, 3, 2),
            "negb" => self.encode_unary_rm(ops, 3, 1),
            "notl" | "not" => self.encode_unary_rm(ops, 2, 4),
            "notw" => self.encode_unary_rm(ops, 2, 2),
            "notb" => self.encode_unary_rm(ops, 2, 1),
            "incl" | "inc" => self.encode_inc_dec(ops, 0, 4),
            "incw" => self.encode_inc_dec(ops, 0, 2),
            "incb" => self.encode_inc_dec(ops, 0, 1),
            "decl" | "dec" => self.encode_inc_dec(ops, 1, 4),
            "decw" => self.encode_inc_dec(ops, 1, 2),
            "decb" => self.encode_inc_dec(ops, 1, 1),

            // Shifts
            "shll" | "shlw" | "shlb" | "shl" => self.encode_shift(ops, mnemonic, 4),
            "shrl" | "shrw" | "shrb" | "shr" => self.encode_shift(ops, mnemonic, 5),
            "sarl" | "sarw" | "sarb" | "sar" => self.encode_shift(ops, mnemonic, 7),
            "roll" | "rolw" | "rolb" | "rol" => self.encode_shift(ops, mnemonic, 0),
            "rorl" | "rorw" | "rorb" | "ror" => self.encode_shift(ops, mnemonic, 1),

            // Double-precision shifts
            "shldl" | "shld" => self.encode_double_shift(ops, 0xA4, 4),
            "shrdl" | "shrd" => self.encode_double_shift(ops, 0xAC, 4),

            // Sign extension
            "cltd" | "cdq" => { self.bytes.push(0x99); Ok(()) }
            "cwtl" | "cwde" => { self.bytes.push(0x98); Ok(()) }
            "cbtw" | "cbw" => { self.bytes.extend_from_slice(&[0x66, 0x98]); Ok(()) }
            "cwtd" | "cwd" => { self.bytes.extend_from_slice(&[0x66, 0x99]); Ok(()) }

            // Byte swap
            "bswapl" | "bswap" => self.encode_bswap(ops),

            // Bit operations
            "lzcntl" | "tzcntl" | "popcntl" => self.encode_bit_count(ops, mnemonic),
            "bsrl" | "bsfl" | "bsr" | "bsf" => self.encode_bsr_bsf(ops, mnemonic),
            "bsrw" | "bsfw" => self.encode_bsr_bsf_16(ops, mnemonic),
            "btl" | "btsl" | "btrl" | "btcl" | "bt" | "bts" | "btr" | "btc" => self.encode_bt(ops, mnemonic),

            // Conditional set
            "sete" | "setz" | "setne" | "setnz" | "setl" | "setle" | "setg" | "setge"
            | "setb" | "setc" | "setbe" | "seta" | "setae" | "setnc" | "setnp" | "setp"
            | "sets" | "setns" | "seto" | "setno" => self.encode_setcc(ops, mnemonic),

            // Conditional move
            "cmovel" | "cmovnel" | "cmovll" | "cmovlel" | "cmovgl" | "cmovgel"
            | "cmovbl" | "cmovbel" | "cmoval" | "cmovael"
            | "cmovsl" | "cmovnsl" | "cmovzl" | "cmovnzl" | "cmovpl" | "cmovnpl"
            | "cmovol" | "cmovnol" | "cmovcl" | "cmovncl"
            | "cmovew" | "cmovnew" | "cmovlw" | "cmovlew" | "cmovgw" | "cmovgew"
            | "cmovbw" | "cmovbew" | "cmovaw" | "cmovaew"
            | "cmovsw" | "cmovnsw" | "cmovzw" | "cmovnzw" | "cmovpw" | "cmovnpw"
            | "cmovow" | "cmovnow" | "cmovcw" | "cmovncw"
            | "cmove" | "cmovne" | "cmovl" | "cmovle" | "cmovg" | "cmovge"
            | "cmovb" | "cmovbe" | "cmova" | "cmovae"
            | "cmovs" | "cmovns" | "cmovz" | "cmovnz" | "cmovp" | "cmovnp"
            | "cmovo" | "cmovno" | "cmovc" | "cmovnc" => self.encode_cmovcc(ops, mnemonic),

            // Jumps
            "jmp" => self.encode_jmp(ops),
            "je" | "jz" | "jne" | "jnz" | "jl" | "jle" | "jg" | "jge"
            | "jb" | "jbe" | "ja" | "jae" | "js" | "jns" | "jo" | "jno" | "jp" | "jnp"
            | "jc" | "jnc" => {
                self.encode_jcc(ops, mnemonic)
            }
            // jecxz/jcxz - short jump only (no long form)
            "jecxz" | "jcxz" => {
                if ops.len() != 1 {
                    return Err("jecxz requires 1 operand".to_string());
                }
                match &ops[0] {
                    Operand::Label(_) => {
                        // E3 cb - Jump short if ECX register is 0
                        self.bytes.extend_from_slice(&[0xE3, 0x00]);
                        Ok(())
                    }
                    _ => Err("jecxz requires label operand".to_string()),
                }
            }
            // loop - short jump only (dec ECX, jump if non-zero)
            "loop" => {
                if ops.len() != 1 {
                    return Err("loop requires 1 operand".to_string());
                }
                match &ops[0] {
                    Operand::Label(_) => {
                        // E2 cb - Dec ECX; jump short if ECX != 0
                        self.bytes.extend_from_slice(&[0xE2, 0x00]);
                        Ok(())
                    }
                    _ => Err("loop requires label operand".to_string()),
                }
            }

            // Call/return
            "call" => self.encode_call(ops),
            "ret" => {
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
            // Far jump
            "ljmpl" | "ljmpw" | "ljmp" => self.encode_ljmp(ops),
            // Far return
            "lret" | "lretl" => {
                if ops.is_empty() {
                    self.bytes.push(0xCB);
                } else if let Some(Operand::Immediate(ImmediateValue::Integer(val))) = ops.first() {
                    self.bytes.push(0xCA);
                    self.bytes.extend_from_slice(&(*val as u16).to_le_bytes());
                } else {
                    return Err("unsupported lret operand".to_string());
                }
                Ok(())
            }
            // 64-bit far return (in .code64 sections, encoded with REX.W prefix)
            "lretq" => {
                self.bytes.extend_from_slice(&[0x48, 0xCB]); // REX.W + lret
                Ok(())
            }

            // No-ops and misc
            "nop" => { self.bytes.push(0x90); Ok(()) }
            "ud2" => { self.bytes.extend_from_slice(&[0x0F, 0x0B]); Ok(()) }
            "pause" => { self.bytes.extend_from_slice(&[0xF3, 0x90]); Ok(()) }
            "mfence" => { self.bytes.extend_from_slice(&[0x0F, 0xAE, 0xF0]); Ok(()) }
            "lfence" => { self.bytes.extend_from_slice(&[0x0F, 0xAE, 0xE8]); Ok(()) }
            "sfence" => { self.bytes.extend_from_slice(&[0x0F, 0xAE, 0xF8]); Ok(()) }
            "clflush" => self.encode_clflush(ops),
            "ldmxcsr" => self.encode_sse_mem_only(ops, 2),
            "stmxcsr" => self.encode_sse_mem_only(ops, 3),
            "int" => self.encode_int(ops),
            "cpuid" => { self.bytes.extend_from_slice(&[0x0F, 0xA2]); Ok(()) }
            "rdtsc" => { self.bytes.extend_from_slice(&[0x0F, 0x31]); Ok(()) }
            "rdtscp" => { self.bytes.extend_from_slice(&[0x0F, 0x01, 0xF9]); Ok(()) }
            "xgetbv" => { self.bytes.extend_from_slice(&[0x0F, 0x01, 0xD0]); Ok(()) }
            "syscall" => { self.bytes.extend_from_slice(&[0x0F, 0x05]); Ok(()) }
            "sysenter" => { self.bytes.extend_from_slice(&[0x0F, 0x34]); Ok(()) }
            "hlt" => { self.bytes.push(0xF4); Ok(()) }
            "emms" => { self.bytes.extend_from_slice(&[0x0F, 0x77]); Ok(()) }
            "cmpxchg8b" => self.encode_cmpxchg8b(ops),
            "rdmsr" => { self.bytes.extend_from_slice(&[0x0F, 0x32]); Ok(()) }
            "wrmsr" => { self.bytes.extend_from_slice(&[0x0F, 0x30]); Ok(()) }
            "rdpmc" => { self.bytes.extend_from_slice(&[0x0F, 0x33]); Ok(()) }
            "wbinvd" => { self.bytes.extend_from_slice(&[0x0F, 0x09]); Ok(()) }
            "invlpg" => self.encode_invlpg(ops),
            "verw" => self.encode_verw(ops),
            "lsl" => self.encode_lsl(ops),
            "sgdt" | "sgdtl" | "sidt" | "sidtl" | "lgdt" | "lgdtl" | "lidt" | "lidtl" => self.encode_system_table(ops, mnemonic),
            "lmsw" => self.encode_lmsw(ops),
            "smsw" => self.encode_smsw(ops),

            // Standalone prefix mnemonics (e.g. from "rep; nop" split on semicolon)
            "lock" if ops.is_empty() => { self.bytes.push(0xF0); Ok(()) }
            "rep" | "repe" | "repz" if ops.is_empty() => { self.bytes.push(0xF3); Ok(()) }
            "repnz" | "repne" if ops.is_empty() => { self.bytes.push(0xF2); Ok(()) }

            // String ops
            "movsb" => { self.bytes.push(0xA4); Ok(()) }
            "movsl" if ops.is_empty() => { self.bytes.push(0xA5); Ok(()) }
            "stosb" => { self.bytes.push(0xAA); Ok(()) }
            "stosl" => { self.bytes.push(0xAB); Ok(()) }
            "cmpsb" => { self.bytes.push(0xA6); Ok(()) }
            "cmpsl" => { self.bytes.push(0xA7); Ok(()) }
            "scasb" => { self.bytes.push(0xAE); Ok(()) }
            "scasl" => { self.bytes.push(0xAF); Ok(()) }
            "lodsb" => { self.bytes.push(0xAC); Ok(()) }
            "lodsl" => { self.bytes.push(0xAD); Ok(()) }

            // I/O string ops
            "insb" => { self.bytes.push(0x6C); Ok(()) }
            "insw" => { self.bytes.extend_from_slice(&[0x66, 0x6D]); Ok(()) }
            "insl" => { self.bytes.push(0x6D); Ok(()) }
            "outsb" => { self.bytes.push(0x6E); Ok(()) }
            "outsw" => { self.bytes.extend_from_slice(&[0x66, 0x6F]); Ok(()) }
            "outsl" => { self.bytes.push(0x6F); Ok(()) }

            // Port I/O instructions
            "outb" | "outw" | "outl" => self.encode_out(ops, mnemonic),
            "inb" | "inw" | "inl" => self.encode_in(ops, mnemonic),

            // Atomic exchange
            "xchgb" | "xchgw" | "xchgl" | "xchg" => self.encode_xchg(ops, mnemonic),

            // Lock-prefixed atomics
            "cmpxchgb" | "cmpxchgw" | "cmpxchgl" | "cmpxchg" => self.encode_cmpxchg(ops, mnemonic),
            "xaddb" | "xaddw" | "xaddl" | "xadd" => self.encode_xadd(ops, mnemonic),

            // SSE/SSE2 floating-point
            "movss" => self.encode_sse_rr_rm(ops, &[0xF3, 0x0F, 0x10], &[0xF3, 0x0F, 0x11]),
            "movsd" => self.encode_sse_rr_rm(ops, &[0xF2, 0x0F, 0x10], &[0xF2, 0x0F, 0x11]),
            "movd" => self.encode_movd(ops),
            "movq" => self.encode_movq(ops),
            "movdqu" => self.encode_sse_rr_rm(ops, &[0xF3, 0x0F, 0x6F], &[0xF3, 0x0F, 0x7F]),
            "movupd" => self.encode_sse_rr_rm(ops, &[0x66, 0x0F, 0x10], &[0x66, 0x0F, 0x11]),
            "movups" => self.encode_sse_rr_rm(ops, &[0x0F, 0x10], &[0x0F, 0x11]),
            "movaps" => self.encode_sse_rr_rm(ops, &[0x0F, 0x28], &[0x0F, 0x29]),
            "movdqa" => self.encode_sse_rr_rm(ops, &[0x66, 0x0F, 0x6F], &[0x66, 0x0F, 0x7F]),
            "movlps" => self.encode_sse_op(ops, &[0x0F, 0x12]),
            "movhps" => self.encode_sse_op(ops, &[0x0F, 0x16]),
            "movlpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x12]),
            "movhpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x16]),

            // Non-temporal stores
            "movnti" | "movntil" => self.encode_movnti(ops),
            "movntdq" => self.encode_sse_store_only(ops, &[0x66, 0x0F, 0xE7]),

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
            "maxsd" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x5F]),
            "maxss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5F]),
            "minsd" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x5D]),
            "minss" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5D]),
            "ucomisd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x2E]),
            "ucomiss" => self.encode_sse_op(ops, &[0x0F, 0x2E]),
            "comisd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x2F]),
            "comiss" => self.encode_sse_op(ops, &[0x0F, 0x2F]),
            "xorpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x57]),
            "xorps" => self.encode_sse_op(ops, &[0x0F, 0x57]),
            "andpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x54]),
            "andps" => self.encode_sse_op(ops, &[0x0F, 0x54]),
            "andnpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x55]),
            "andnps" => self.encode_sse_op(ops, &[0x0F, 0x55]),
            "orpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x56]),
            "orps" => self.encode_sse_op(ops, &[0x0F, 0x56]),
            "unpcklps" => self.encode_sse_op(ops, &[0x0F, 0x14]),
            "unpckhps" => self.encode_sse_op(ops, &[0x0F, 0x15]),
            "unpcklpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x14]),
            "unpckhpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x15]),
            "shufps" => self.encode_sse_op_imm8(ops, &[0x0F, 0xC6]),
            "shufpd" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0xC6]),
            "cmpsd" => self.encode_sse_op_imm8(ops, &[0xF2, 0x0F, 0xC2]),
            "cmpss" => self.encode_sse_op_imm8(ops, &[0xF3, 0x0F, 0xC2]),
            "cmppd" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0xC2]),
            "cmpps" => self.encode_sse_op_imm8(ops, &[0x0F, 0xC2]),
            "pclmulqdq" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x44]),

            // AES-NI
            "aesenc" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0xDC]),
            "aesenclast" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0xDD]),
            "aesdec" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0xDE]),
            "aesdeclast" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0xDF]),
            "aesimc" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0xDB]),
            "aeskeygenassist" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0xDF]),

            // SSE conversions (32-bit integer operand size for i686)
            "cvtsd2ss" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x5A]),
            "cvtss2sd" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5A]),
            "cvtsi2sdl" | "cvtsi2sd" => self.encode_sse_cvt_gp_to_xmm(ops, &[0xF2, 0x0F, 0x2A]),
            "cvtsi2ssl" | "cvtsi2ss" => self.encode_sse_cvt_gp_to_xmm(ops, &[0xF3, 0x0F, 0x2A]),
            "cvttsd2sil" | "cvttsd2si" | "cvtsd2sil" | "cvtsd2si" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF2, 0x0F, 0x2C]),
            "cvttss2sil" | "cvttss2si" | "cvtss2sil" | "cvtss2si" => self.encode_sse_cvt_xmm_to_gp(ops, &[0xF3, 0x0F, 0x2C]),
            "cvtps2pd" => self.encode_sse_op(ops, &[0x0F, 0x5A]),
            "cvtpd2ps" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x5A]),
            "cvtdq2ps" => self.encode_sse_op(ops, &[0x0F, 0x5B]),
            "cvtps2dq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x5B]),
            "cvttps2dq" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x5B]),
            "cvtdq2pd" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0xE6]),
            "cvtpd2dq" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0xE6]),

            // SSE packed integer
            "pshufd" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x70]),
            "pshuflw" => self.encode_sse_op_imm8(ops, &[0xF2, 0x0F, 0x70]),
            "pshufhw" => self.encode_sse_op_imm8(ops, &[0xF3, 0x0F, 0x70]),
            "pxor" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEF]),
            "pand" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDB]),
            "por" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEB]),
            "pandn" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDF]),
            "pcmpeqb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x74]),
            "pcmpeqd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x76]),
            "pcmpeqw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x75]),
            "pcmpgtb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x64]),
            "pcmpgtd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x66]),
            "pcmpgtw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x65]),
            "pmovmskb" => self.encode_sse_cvt_xmm_to_gp(ops, &[0x66, 0x0F, 0xD7]),
            "movmskps" => self.encode_sse_cvt_xmm_to_gp(ops, &[0x0F, 0x50]),
            "movmskpd" => self.encode_sse_cvt_xmm_to_gp(ops, &[0x66, 0x0F, 0x50]),

            // SSE packed arithmetic
            "paddb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xFC]),
            "paddw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xFD]),
            "paddd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xFE]),
            "paddq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xD4]),
            "psubb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xF8]),
            "psubw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xF9]),
            "psubd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xFA]),
            "psubq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xFB]),
            "pmullw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xD5]),
            "pmulld" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x40]),
            "pmulhw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE5]),
            "pmulhuw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE4]),
            "pmuludq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xF4]),
            "paddusb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDC]),
            "paddusw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDD]),
            "psubusb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xD8]),
            "psubusw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xD9]),
            "paddsb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEC]),
            "paddsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xED]),
            "psubsb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE8]),
            "psubsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE9]),
            "pmaxub" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDE]),
            "pmaxsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEE]),
            "pminub" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xDA]),
            "pminsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xEA]),
            "pavgb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE0]),
            "pavgw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xE3]),
            "psadbw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xF6]),
            "pmaddwd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xF5]),
            "pabsb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x1C]),
            "pabsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x1D]),
            "pabsd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x1E]),

            // SSE pack/unpack
            "punpcklbw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x60]),
            "punpcklwd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x61]),
            "punpckldq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x62]),
            "punpcklqdq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x6C]),
            "punpckhbw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x68]),
            "punpckhwd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x69]),
            "punpckhdq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x6A]),
            "punpckhqdq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x6D]),
            "packsswb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x63]),
            "packssdw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x6B]),
            "packuswb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x67]),
            "packusdw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x2B]),

            // SSE insert/extract
            "pextrw" => self.encode_pextrw(ops),
            "pinsrw" => self.encode_pinsrw(ops),

            // SSE shifts
            "pslld" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xF2], 6, &[0x66, 0x0F, 0x72]),
            "psrld" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xD2], 2, &[0x66, 0x0F, 0x72]),
            "psrad" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xE2], 4, &[0x66, 0x0F, 0x72]),
            "psllq" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xF3], 6, &[0x66, 0x0F, 0x73]),
            "psrlq" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xD3], 2, &[0x66, 0x0F, 0x73]),
            "psllw" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xF1], 6, &[0x66, 0x0F, 0x71]),
            "psrlw" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xD1], 2, &[0x66, 0x0F, 0x71]),
            "psraw" => self.encode_sse_shift(ops, &[0x66, 0x0F, 0xE1], 4, &[0x66, 0x0F, 0x71]),
            // pslldq/psrldq: byte shifts (only immediate form)
            "pslldq" => self.encode_sse_byte_shift(ops, 7),
            "psrldq" => self.encode_sse_byte_shift(ops, 3),

            // x87 FPU
            "fldt" => self.encode_x87_mem(ops, &[0xDB], 5),
            "fstpt" => self.encode_x87_mem(ops, &[0xDB], 7),
            "fldl" => self.encode_x87_mem(ops, &[0xDD], 0),
            "flds" => self.encode_x87_mem(ops, &[0xD9], 0),
            "fstpl" => self.encode_x87_mem(ops, &[0xDD], 3),
            "fstps" => self.encode_x87_mem(ops, &[0xD9], 3),
            "fstl" => self.encode_x87_mem(ops, &[0xDD], 2),
            "fsts" => self.encode_x87_mem(ops, &[0xD9], 2),
            "fildl" => self.encode_x87_mem(ops, &[0xDB], 0),
            "fildq" => self.encode_x87_mem(ops, &[0xDF], 5),
            "filds" => self.encode_x87_mem(ops, &[0xDF], 0),
            "fistpl" => self.encode_x87_mem(ops, &[0xDB], 3),
            "fistpq" => self.encode_x87_mem(ops, &[0xDF], 7),
            "fisttpq" => self.encode_x87_mem(ops, &[0xDD], 1),
            "fisttpl" => self.encode_x87_mem(ops, &[0xDB], 1),
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
            "fabs" => { self.bytes.extend_from_slice(&[0xD9, 0xE1]); Ok(()) }
            "fsqrt" => { self.bytes.extend_from_slice(&[0xD9, 0xFA]); Ok(()) }
            "fsin" => { self.bytes.extend_from_slice(&[0xD9, 0xFE]); Ok(()) }
            "fcos" => { self.bytes.extend_from_slice(&[0xD9, 0xFF]); Ok(()) }
            "fpatan" => { self.bytes.extend_from_slice(&[0xD9, 0xF3]); Ok(()) }
            "fptan" => { self.bytes.extend_from_slice(&[0xD9, 0xF2]); Ok(()) }
            "fprem" => { self.bytes.extend_from_slice(&[0xD9, 0xF8]); Ok(()) }
            "fprem1" => { self.bytes.extend_from_slice(&[0xD9, 0xF5]); Ok(()) }
            "frndint" => { self.bytes.extend_from_slice(&[0xD9, 0xFC]); Ok(()) }
            "fscale" => { self.bytes.extend_from_slice(&[0xD9, 0xFD]); Ok(()) }
            "f2xm1" => { self.bytes.extend_from_slice(&[0xD9, 0xF0]); Ok(()) }
            "fyl2x" => { self.bytes.extend_from_slice(&[0xD9, 0xF1]); Ok(()) }
            "fyl2xp1" => { self.bytes.extend_from_slice(&[0xD9, 0xF9]); Ok(()) }
            "fld1" => { self.bytes.extend_from_slice(&[0xD9, 0xE8]); Ok(()) }
            "fldl2e" => { self.bytes.extend_from_slice(&[0xD9, 0xEA]); Ok(()) }
            "fldl2t" => { self.bytes.extend_from_slice(&[0xD9, 0xE9]); Ok(()) }
            "fldlg2" => { self.bytes.extend_from_slice(&[0xD9, 0xEC]); Ok(()) }
            "fldln2" => { self.bytes.extend_from_slice(&[0xD9, 0xED]); Ok(()) }
            "fldpi" => { self.bytes.extend_from_slice(&[0xD9, 0xEB]); Ok(()) }
            "fldz" => { self.bytes.extend_from_slice(&[0xD9, 0xEE]); Ok(()) }
            "fnstsw" => self.encode_fnstsw(ops),
            "fnstcw" => self.encode_x87_mem(ops, &[0xD9], 7),
            "fldcw" => self.encode_x87_mem(ops, &[0xD9], 5),
            "fwait" | "wait" => { self.bytes.push(0x9B); Ok(()) }
            "fnclex" => { self.bytes.extend_from_slice(&[0xDB, 0xE2]); Ok(()) }
            "fninit" => { self.bytes.extend_from_slice(&[0xDB, 0xE3]); Ok(()) }
            "ftst" => { self.bytes.extend_from_slice(&[0xD9, 0xE4]); Ok(()) }
            "fxam" => { self.bytes.extend_from_slice(&[0xD9, 0xE5]); Ok(()) }
            "fcomip" => self.encode_fcomip(ops),
            "fucomip" => self.encode_fucomip(ops),
            "fucomi" => self.encode_fucomi(ops),
            "fucomp" => self.encode_fucomp(ops),
            "fucom" => self.encode_fucom(ops),
            "fld" => self.encode_fld_st(ops),
            "fstp" => self.encode_fstp_st(ops),
            "fxch" => self.encode_fxch(ops),
            "faddl" => self.encode_x87_mem(ops, &[0xDC], 0),
            "fadds" => self.encode_x87_mem(ops, &[0xD8], 0),
            "fsubl" => self.encode_x87_mem(ops, &[0xDC], 4),
            "fsubs" => self.encode_x87_mem(ops, &[0xD8], 4),
            "fmull" => self.encode_x87_mem(ops, &[0xDC], 1),
            "fmuls" => self.encode_x87_mem(ops, &[0xD8], 1),
            "fdivl" => self.encode_x87_mem(ops, &[0xDC], 6),
            "fdivs" => self.encode_x87_mem(ops, &[0xD8], 6),
            "fsubrl" => self.encode_x87_mem(ops, &[0xDC], 5),
            "fdivrl" => self.encode_x87_mem(ops, &[0xDC], 7),
            "fsubrs" => self.encode_x87_mem(ops, &[0xD8], 5),
            "fdivrs" => self.encode_x87_mem(ops, &[0xD8], 7),

            // x87 register-register arithmetic (fadd/fmul/fsub/fdiv with st(i) operands)
            "fadd" => self.encode_x87_arith_reg(ops, 0xD8, 0xDC, 0xC0),
            "fmul" => self.encode_x87_arith_reg(ops, 0xD8, 0xDC, 0xC8),
            "fsub" => self.encode_x87_arith_reg(ops, 0xD8, 0xDC, 0xE0),
            "fdiv" => self.encode_x87_arith_reg(ops, 0xD8, 0xDC, 0xF0),

            // x87 additional
            "fxtract" => { self.bytes.extend_from_slice(&[0xD9, 0xF4]); Ok(()) }
            "fnstenv" => self.encode_x87_mem(ops, &[0xD9], 6),
            "fldenv" => self.encode_x87_mem(ops, &[0xD9], 4),
            "fistl" => self.encode_x87_mem(ops, &[0xDB], 2),
            "fistps" => self.encode_x87_mem(ops, &[0xDF], 3),
            "fildll" => self.encode_x87_mem(ops, &[0xDF], 5),
            "fisttpll" => self.encode_x87_mem(ops, &[0xDD], 1),
            "fistpll" => self.encode_x87_mem(ops, &[0xDF], 7),
            "fstcw" => {
                // fstcw = fwait + fnstcw
                self.bytes.push(0x9B); // FWAIT
                self.encode_x87_mem(ops, &[0xD9], 7)
            }

            // Flag manipulation
            "cld" => { self.bytes.push(0xFC); Ok(()) }
            "std" => { self.bytes.push(0xFD); Ok(()) }
            "clc" => { self.bytes.push(0xF8); Ok(()) }
            "stc" => { self.bytes.push(0xF9); Ok(()) }
            "cmc" => { self.bytes.push(0xF5); Ok(()) }
            "cli" => { self.bytes.push(0xFA); Ok(()) }
            "sti" => { self.bytes.push(0xFB); Ok(()) }
            "sahf" => { self.bytes.push(0x9E); Ok(()) }
            "lahf" => { self.bytes.push(0x9F); Ok(()) }
            "pushf" | "pushfl" => { self.bytes.push(0x9C); Ok(()) }
            "popf" | "popfl" => { self.bytes.push(0x9D); Ok(()) }

            // Leave (stack frame teardown)
            "leave" => { self.bytes.push(0xC9); Ok(()) }

            // int3 (explicit breakpoint mnemonic)
            "int3" => { self.bytes.push(0xCC); Ok(()) }

            // Endbr32 (CET)
            "endbr32" => { self.bytes.extend_from_slice(&[0xF3, 0x0F, 0x1E, 0xFB]); Ok(()) }

            // SSE packed float arithmetic
            "addpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x58]),
            "subpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x5C]),
            "mulpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x59]),
            "divpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x5E]),
            "addps" => self.encode_sse_op(ops, &[0x0F, 0x58]),
            "subps" => self.encode_sse_op(ops, &[0x0F, 0x5C]),
            "mulps" => self.encode_sse_op(ops, &[0x0F, 0x59]),
            "divps" => self.encode_sse_op(ops, &[0x0F, 0x5E]),

            // SSE3 horizontal operations
            "haddpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x7C]),
            "hsubpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x7D]),
            "haddps" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x7C]),
            "hsubps" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x7D]),
            "addsubpd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0xD0]),
            "addsubps" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0xD0]),

            // SSSE3
            "palignr" => self.encode_sse_op_imm8(ops, &[0x66, 0x0F, 0x3A, 0x0F]),
            "pshufb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x00]),
            "phaddw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x01]),
            "phaddd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x02]),
            "phsubw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x05]),
            "phsubd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x06]),
            "pmulhrsw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x0B]),

            // SSE4.1 blend
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

            // SSE4.1 test / min-max
            "ptest" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x17]),
            "pminsb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x38]),
            "pmaxsb" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3C]),
            "pminuw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3A]),
            "pmaxuw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3E]),
            "pminud" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3B]),
            "pmaxud" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3F]),
            "pminsd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x39]),
            "pmaxsd" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x3D]),
            "phminposuw" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x41]),

            // SSE4.1 insert/extract (32-bit and byte)
            "pinsrd" => self.encode_sse_insert(ops, &[0x66, 0x0F, 0x3A, 0x22]),
            "pextrd" => self.encode_sse_extract(ops, &[0x66, 0x0F, 0x3A, 0x16]),
            "pinsrb" => self.encode_sse_insert(ops, &[0x66, 0x0F, 0x3A, 0x20]),
            "pextrb" => self.encode_sse_extract(ops, &[0x66, 0x0F, 0x3A, 0x14]),

            // SSE4.1/SSE4.2 packed integer extensions
            "pcmpgtq" => self.encode_sse_op(ops, &[0x66, 0x0F, 0x38, 0x37]),

            // SSE4.1 zero/sign extend
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

            // SSE data movement (missing from i686)
            "movapd" => self.encode_sse_rr_rm(ops, &[0x66, 0x0F, 0x28], &[0x66, 0x0F, 0x29]),
            "movhlps" => self.encode_sse_op(ops, &[0x0F, 0x12]),
            "movlhps" => self.encode_sse_op(ops, &[0x0F, 0x16]),
            "movddup" => self.encode_sse_op(ops, &[0xF2, 0x0F, 0x12]),
            "movshdup" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x16]),
            "movsldup" => self.encode_sse_op(ops, &[0xF3, 0x0F, 0x12]),
            "movntps" => self.encode_sse_store_only(ops, &[0x0F, 0x2B]),
            "movntpd" => self.encode_sse_store_only(ops, &[0x66, 0x0F, 0x2B]),

            // Prefetch instructions
            "prefetcht0" => self.encode_prefetch(ops, 1),
            "prefetcht1" => self.encode_prefetch(ops, 2),
            "prefetcht2" => self.encode_prefetch(ops, 3),
            "prefetchnta" => self.encode_prefetch(ops, 0),
            "prefetchw" => self.encode_prefetch_0f0d(ops, 1),

            // Rotate through carry
            "rclb" | "rclw" | "rcll" | "rcl" => self.encode_shift(ops, mnemonic, 2),
            "rcrb" | "rcrw" | "rcrl" | "rcr" => self.encode_shift(ops, mnemonic, 3),

            // 16-bit string operations
            "movsw" => { self.bytes.extend_from_slice(&[0x66, 0xA5]); Ok(()) }
            "stosw" => { self.bytes.extend_from_slice(&[0x66, 0xAB]); Ok(()) }
            "lodsw" => { self.bytes.extend_from_slice(&[0x66, 0xAD]); Ok(()) }
            "scasw" => { self.bytes.extend_from_slice(&[0x66, 0xAF]); Ok(()) }
            "cmpsw" => { self.bytes.extend_from_slice(&[0x66, 0xA7]); Ok(()) }

            // Additional multiply/divide sizes
            "mulb" => self.encode_unary_rm(ops, 4, 1),
            "mulw" => { self.bytes.push(0x66); self.encode_unary_rm(ops, 4, 2) }
            "divw" => { self.bytes.push(0x66); self.encode_unary_rm(ops, 6, 2) }
            "divb" => self.encode_unary_rm(ops, 6, 1),
            "idivw" => { self.bytes.push(0x66); self.encode_unary_rm(ops, 7, 2) }
            "idivb" => self.encode_unary_rm(ops, 7, 1),
            "imulw" => self.encode_imul(ops, 2),

            _ => {
                Err(format!("unhandled i686 instruction: {} {:?}", mnemonic, ops))
            }
        }
    }
}
