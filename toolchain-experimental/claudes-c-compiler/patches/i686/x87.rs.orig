//! x87 FPU instruction encoders for i686.
//!
//! Handles x87 floating-point load/store, arithmetic, comparison,
//! and control instructions.

use super::*;

impl super::InstructionEncoder {
    // ---- x87 FPU encoding (identical to x86-64, no REX needed) ----

    pub(super) fn encode_x87_mem(&mut self, ops: &[Operand], opcode: &[u8], ext: u8) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("x87 mem op requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Memory(mem) => {
                self.bytes.extend_from_slice(opcode);
                self.encode_modrm_mem(ext, mem)
            }
            _ => Err("x87 mem op requires memory operand".to_string()),
        }
    }

    pub(super) fn encode_fcomip(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() == 2 {
            match &ops[0] {
                Operand::Register(reg) => {
                    let n = parse_st_num(&reg.name)?;
                    self.bytes.extend_from_slice(&[0xDF, 0xF0 + n]);
                    Ok(())
                }
                _ => Err("fcomip requires st register".to_string()),
            }
        } else if ops.is_empty() {
            self.bytes.extend_from_slice(&[0xDF, 0xF1]);
            Ok(())
        } else {
            Err("fcomip requires 0 or 2 operands".to_string())
        }
    }

    pub(super) fn encode_fucomip(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() == 2 {
            match &ops[0] {
                Operand::Register(reg) => {
                    let n = parse_st_num(&reg.name)?;
                    self.bytes.extend_from_slice(&[0xDF, 0xE8 + n]);
                    Ok(())
                }
                _ => Err("fucomip requires st register".to_string()),
            }
        } else if ops.is_empty() {
            self.bytes.extend_from_slice(&[0xDF, 0xE9]);
            Ok(())
        } else {
            Err("fucomip requires 0 or 2 operands".to_string())
        }
    }

    pub(super) fn encode_fld_st(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("fld requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Register(reg) => {
                let n = parse_st_num(&reg.name)?;
                self.bytes.extend_from_slice(&[0xD9, 0xC0 + n]);
                Ok(())
            }
            _ => Err("fld requires st register".to_string()),
        }
    }

    pub(super) fn encode_fstp_st(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("fstp requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Register(reg) => {
                let n = parse_st_num(&reg.name)?;
                self.bytes.extend_from_slice(&[0xDD, 0xD8 + n]);
                Ok(())
            }
            _ => Err("fstp requires st register".to_string()),
        }
    }

    pub(super) fn encode_fxch(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.is_empty() {
            // fxch with no operand defaults to st(1)
            self.bytes.extend_from_slice(&[0xD9, 0xC9]);
            return Ok(());
        }
        if ops.len() == 1 || ops.len() == 2 {
            // With 1 operand: fxch %st(i)
            // With 2 operands: fxch %st(i), %st (AT&T syntax)
            match &ops[0] {
                Operand::Register(reg) => {
                    let n = parse_st_num(&reg.name)?;
                    self.bytes.extend_from_slice(&[0xD9, 0xC8 + n]);
                    Ok(())
                }
                _ => Err("fxch requires st register".to_string()),
            }
        } else {
            Err("fxch requires 0, 1 or 2 operands".to_string())
        }
    }

    /// Encode fnstsw (store FPU status word).
    pub(super) fn encode_fnstsw(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.is_empty() {
            // fnstsw with no operand defaults to %ax
            self.bytes.extend_from_slice(&[0xDF, 0xE0]);
            return Ok(());
        }
        if ops.len() != 1 {
            return Err("fnstsw requires 0 or 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Register(reg) if reg.name == "ax" => {
                self.bytes.extend_from_slice(&[0xDF, 0xE0]);
                Ok(())
            }
            Operand::Memory(mem) => {
                self.bytes.push(0xDD);
                self.encode_modrm_mem(7, mem)
            }
            _ => Err("fnstsw requires %ax or memory operand".to_string()),
        }
    }

    /// Encode fucomi (unordered compare and set EFLAGS).
    pub(super) fn encode_fucomi(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() == 2 {
            match &ops[0] {
                Operand::Register(reg) => {
                    let n = parse_st_num(&reg.name)?;
                    self.bytes.extend_from_slice(&[0xDB, 0xE8 + n]);
                    Ok(())
                }
                _ => Err("fucomi requires st register".to_string()),
            }
        } else if ops.is_empty() {
            self.bytes.extend_from_slice(&[0xDB, 0xE9]);
            Ok(())
        } else {
            Err("fucomi requires 0 or 2 operands".to_string())
        }
    }

    /// Encode fucomp (unordered compare and pop, sets FPU status word).
    pub(super) fn encode_fucomp(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() == 1 {
            match &ops[0] {
                Operand::Register(reg) => {
                    let n = parse_st_num(&reg.name)?;
                    self.bytes.extend_from_slice(&[0xDD, 0xE8 + n]);
                    Ok(())
                }
                _ => Err("fucomp requires st register".to_string()),
            }
        } else if ops.len() == 2 {
            // AT&T syntax: fucomp %st(1), %st  — first operand is the source
            match &ops[0] {
                Operand::Register(reg) => {
                    let n = parse_st_num(&reg.name)?;
                    self.bytes.extend_from_slice(&[0xDD, 0xE8 + n]);
                    Ok(())
                }
                _ => Err("fucomp requires st register".to_string()),
            }
        } else if ops.is_empty() {
            // Default: fucomp %st(1)
            self.bytes.extend_from_slice(&[0xDD, 0xE9]);
            Ok(())
        } else {
            Err("fucomp requires 0, 1 or 2 operands".to_string())
        }
    }

    /// Encode fucom (unordered compare, sets FPU status word, no pop).
    pub(super) fn encode_fucom(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() == 1 {
            match &ops[0] {
                Operand::Register(reg) => {
                    let n = parse_st_num(&reg.name)?;
                    self.bytes.extend_from_slice(&[0xDD, 0xE0 + n]);
                    Ok(())
                }
                _ => Err("fucom requires st register".to_string()),
            }
        } else if ops.len() == 2 {
            // AT&T syntax: fucom %st(1), %st — first operand is the source
            match &ops[0] {
                Operand::Register(reg) => {
                    let n = parse_st_num(&reg.name)?;
                    self.bytes.extend_from_slice(&[0xDD, 0xE0 + n]);
                    Ok(())
                }
                _ => Err("fucom requires st register".to_string()),
            }
        } else if ops.is_empty() {
            // Default: fucom %st(1)
            self.bytes.extend_from_slice(&[0xDD, 0xE1]);
            Ok(())
        } else {
            Err("fucom requires 0, 1 or 2 operands".to_string())
        }
    }

    /// Map TLS modifier string to relocation type.
    pub(super) fn tls_reloc_type(&self, modifier: &str) -> u32 {
        match modifier {
            "NTPOFF" => R_386_TLS_LE_32,
            "TPOFF" => R_386_32S,
            "TLSGD" => R_386_TLS_GD,
            "TLSLDM" => R_386_TLS_LDM,
            "DTPOFF" => R_386_TLS_LDO_32,
            "GOT" => R_386_GOT32,
            "GOTOFF" => R_386_GOTOFF,
            "PLT" => R_386_PLT32,
            "GOTPC" => R_386_GOTPC,
            "GOTNTPOFF" | "INDNTPOFF" => R_386_TLS_IE,
            _ => R_386_32,
        }
    }

    /// Encode x87 register-register arithmetic (fadd/fmul/fsub/fdiv with st(i) operands).
    pub(super) fn encode_x87_arith_reg(&mut self, ops: &[Operand], opcode_st0: u8, opcode_sti: u8, base_modrm: u8) -> Result<(), String> {
        match ops.len() {
            0 => {
                // Default: fadd %st(1), %st (i.e., st(0) = st(0) op st(1))
                self.bytes.extend_from_slice(&[opcode_st0, base_modrm + 1]);
                Ok(())
            }
            1 => {
                // fadd %st(i) -> st(0) = st(0) op st(i)
                match &ops[0] {
                    Operand::Register(reg) => {
                        let n = parse_st_num(&reg.name)?;
                        self.bytes.extend_from_slice(&[opcode_st0, base_modrm + n]);
                        Ok(())
                    }
                    _ => Err("x87 arith requires st register operand".to_string()),
                }
            }
            2 => {
                // Two operands: fadd %st(i), %st or fadd %st, %st(i)
                match (&ops[0], &ops[1]) {
                    (Operand::Register(src), Operand::Register(dst)) => {
                        let src_n = parse_st_num(&src.name)?;
                        let dst_n = parse_st_num(&dst.name)?;
                        if dst_n == 0 {
                            // fadd %st(i), %st -> D8 (base + i)
                            self.bytes.extend_from_slice(&[opcode_st0, base_modrm + src_n]);
                        } else if src_n == 0 {
                            // fadd %st, %st(i) -> DC (base + i)
                            // Note: fsub/fdiv swap in DC encoding
                            let dc_modrm = match base_modrm {
                                0xC0 => 0xC0, // fadd
                                0xC8 => 0xC8, // fmul
                                0xE0 => 0xE8, // fsub -> fsubr encoding in DC
                                0xF0 => 0xF8, // fdiv -> fdivr encoding in DC
                                _ => base_modrm,
                            };
                            self.bytes.extend_from_slice(&[opcode_sti, dc_modrm + dst_n]);
                        } else {
                            return Err("x87 arith: one operand must be st(0)".to_string());
                        }
                        Ok(())
                    }
                    _ => Err("x87 arith requires st register operands".to_string()),
                }
            }
            _ => Err("x87 arith requires 0-2 operands".to_string()),
        }
    }
}
