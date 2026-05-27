use super::*;

impl super::InstructionEncoder {
    // ---- BT/BTS/BTR/BTC encoding ----

    /// Encode bit test instructions (bt, bts, btr, btc).
    pub(crate) fn encode_bt(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        if ops.len() != 2 {
            return Err(format!("{} requires 2 operands", mnemonic));
        }
        let size = mnemonic_size_suffix(mnemonic).unwrap_or(8);
        // Determine which bt variant
        let base = &mnemonic[..mnemonic.len()-1]; // strip size suffix
        let (reg_opcode, imm_ext) = match base {
            "bt"  => (0xA3u8, 4u8),
            "bts" => (0xAB, 5),
            "btr" => (0xB3, 6),
            "btc" => (0xBB, 7),
            _ => return Err(format!("unknown bt variant: {}", mnemonic)),
        };

        match (&ops[0], &ops[1]) {
            // bt $imm, reg/mem
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.emit_rex_unary(size, &dst.name);
                self.bytes.extend_from_slice(&[0x0F, 0xBA]);
                self.bytes.push(self.modrm(3, imm_ext, dst_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Memory(mem)) => {
                self.emit_segment_prefix(mem)?;
                self.emit_rex_rm(size, "", mem);
                let rc = self.relocations.len();
                self.bytes.extend_from_slice(&[0x0F, 0xBA]);
                self.encode_modrm_mem(imm_ext, mem)?;
                self.bytes.push(*imm as u8);
                self.adjust_rip_reloc_addend(rc, 1);
                Ok(())
            }
            // bt %reg, reg/mem
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                self.emit_rex_rr(size, &src.name, &dst.name);
                self.bytes.extend_from_slice(&[0x0F, reg_opcode]);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                Ok(())
            }
            (Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                self.emit_segment_prefix(mem)?;
                self.emit_rex_rm(size, &src.name, mem);
                self.bytes.extend_from_slice(&[0x0F, reg_opcode]);
                self.encode_modrm_mem(src_num, mem)
            }
            // bt $imm, label (treat label as RIP-relative memory)
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Label(name)) => {
                let mem = MemoryOperand {
                    segment: None,
                    displacement: Displacement::Symbol(name.clone()),
                    base: None,
                    index: None,
                    scale: None,
                };
                self.emit_rex_rm(size, "", &mem);
                let rc = self.relocations.len();
                self.bytes.extend_from_slice(&[0x0F, 0xBA]);
                self.encode_modrm_mem(imm_ext, &mem)?;
                self.bytes.push(*imm as u8);
                self.adjust_rip_reloc_addend(rc, 1);
                Ok(())
            }
            _ => Err(format!("unsupported {} operands", mnemonic)),
        }
    }

    // ---- x87 FPU encoding ----

    pub(crate) fn encode_x87_mem(&mut self, ops: &[Operand], opcode: &[u8], ext: u8) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("x87 mem op requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Memory(mem) => {
                // x87 instructions don't use REX.W
                self.emit_rex_rm(0, "", mem);
                self.bytes.extend_from_slice(opcode);
                self.encode_modrm_mem(ext, mem)
            }
            _ => Err("x87 mem op requires memory operand".to_string()),
        }
    }

    pub(crate) fn encode_fcomip(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() == 2 {
            // fcomip %st(N), %st
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

    pub(crate) fn encode_fucomi(&mut self, ops: &[Operand]) -> Result<(), String> {
        // fucomi %st(i), %st -> DB E8+i (0 or 2 operands; 1-operand also accepted)
        if ops.len() == 2 || ops.len() == 1 {
            match &ops[0] {
                Operand::Register(reg) => {
                    let n = parse_st_num(&reg.name)?;
                    self.bytes.extend_from_slice(&[0xDB, 0xE8 + n]);
                    Ok(())
                }
                _ => Err("fucomi requires st register".to_string()),
            }
        } else if ops.is_empty() {
            self.bytes.extend_from_slice(&[0xDB, 0xE9]); // default st(1)
            Ok(())
        } else {
            Err("fucomi requires 0-2 operands".to_string())
        }
    }

    pub(crate) fn encode_fucomip(&mut self, ops: &[Operand]) -> Result<(), String> {
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

    pub(crate) fn encode_fld_st(&mut self, ops: &[Operand]) -> Result<(), String> {
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

    pub(crate) fn encode_fstp_st(&mut self, ops: &[Operand]) -> Result<(), String> {
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

    // ---- Bit scan (BSF/BSR) ----

    pub(crate) fn encode_bit_scan(&mut self, ops: &[Operand], mnemonic: &str, opcode2: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err(format!("{} requires 2 operands", mnemonic));
        }
        let size = mnemonic_size_suffix(mnemonic).unwrap_or(8);
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rr(size, &dst.name, &src.name);
                self.bytes.extend_from_slice(&[0x0F, opcode2]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_rm(size, &dst.name, mem);
                self.bytes.extend_from_slice(&[0x0F, opcode2]);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err(format!("unsupported {} operands", mnemonic)),
        }
    }


    // ---- Segment register moves ----

    pub(crate) fn encode_mov_seg(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("mov seg requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            // mov %seg, %reg
            (Operand::Register(src), Operand::Register(dst)) if is_segment_reg(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad seg register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                // 8C /r - MOV r/m16, Sreg
                if is_reg64(&dst.name) {
                    self.emit_rex_unary(8, &dst.name);
                } else if needs_rex_ext(&dst.name) {
                    self.bytes.push(self.rex(false, false, false, true));
                }
                self.bytes.push(0x8C);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                Ok(())
            }
            // mov %reg, %seg
            (Operand::Register(src), Operand::Register(dst)) if is_segment_reg(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad seg register")?;
                // 8E /r - MOV Sreg, r/m16
                if needs_rex_ext(&src.name) {
                    self.bytes.push(self.rex(false, false, false, true));
                }
                self.bytes.push(0x8E);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            // mov %seg, mem
            (Operand::Register(src), Operand::Memory(mem)) if is_segment_reg(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad seg register")?;
                // 8C /r - MOV r/m16, Sreg (store segment to memory)
                self.emit_rex_rm(0, "", mem);
                self.bytes.push(0x8C);
                self.encode_modrm_mem(src_num, mem)
            }
            // mov mem, %seg
            (Operand::Memory(mem), Operand::Register(dst)) if is_segment_reg(&dst.name) => {
                let dst_num = reg_num(&dst.name).ok_or("bad seg register")?;
                // 8E /r - MOV Sreg, r/m16 (load segment from memory)
                self.emit_rex_rm(0, "", mem);
                self.bytes.push(0x8E);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err("unsupported mov seg operands".to_string()),
        }
    }

    // ---- MMX movq ----

    pub(crate) fn encode_mmx_movq(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("mmx movq requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            // movq %mm, %mm
            (Operand::Register(src), Operand::Register(dst)) if is_mmx(&src.name) && is_mmx(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.extend_from_slice(&[0x0F, 0x6F]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            // movq mem, %mm
            (Operand::Memory(mem), Operand::Register(dst)) if is_mmx(&dst.name) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.emit_rex_rm(0, "", mem);
                self.bytes.extend_from_slice(&[0x0F, 0x6F]);
                self.encode_modrm_mem(dst_num, mem)
            }
            // movq %mm, mem
            (Operand::Register(src), Operand::Memory(mem)) if is_mmx(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                self.emit_rex_rm(0, "", mem);
                self.bytes.extend_from_slice(&[0x0F, 0x7F]);
                self.encode_modrm_mem(src_num, mem)
            }
            // movq %gp64, %mm -> 0F 6E (with REX.W)
            (Operand::Register(src), Operand::Register(dst)) if !is_mmx(&src.name) && is_mmx(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let b = needs_rex_ext(&src.name);
                self.bytes.push(self.rex(true, false, false, b));
                self.bytes.extend_from_slice(&[0x0F, 0x6E]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            // movq %mm, %gp64 -> 0F 7E (with REX.W)
            (Operand::Register(src), Operand::Register(dst)) if is_mmx(&src.name) && !is_mmx(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let b = needs_rex_ext(&dst.name);
                self.bytes.push(self.rex(true, false, false, b));
                self.bytes.extend_from_slice(&[0x0F, 0x7E]);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                Ok(())
            }
            _ => Err("unsupported mmx movq operands".to_string()),
        }
    }

    // ---- Suffix-less instruction helpers ----

    pub(crate) fn encode_suffixless_mov(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("mov requires 2 operands".to_string());
        }
        // Check for segment registers
        if ops.iter().any(|op| matches!(op, Operand::Register(r) if is_segment_reg(&r.name))) {
            return self.encode_mov_seg(ops);
        }
        // Check for control registers
        if ops.iter().any(|op| matches!(op, Operand::Register(r) if is_control_reg(&r.name))) {
            return self.encode_mov_cr(ops);
        }
        // Check for debug registers
        if ops.iter().any(|op| matches!(op, Operand::Register(r) if is_debug_reg(&r.name))) {
            return self.encode_mov_dr(ops);
        }
        let size = infer_operand_size_from_pair(&ops[0], &ops[1]);
        self.encode_mov(ops, size)
    }

    pub(crate) fn encode_suffixless_alu(&mut self, ops: &[Operand], alu_op: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("ALU op requires 2 operands".to_string());
        }
        let size = infer_operand_size_from_pair(&ops[0], &ops[1]);
        let suffix = match size { 1 => "b", 2 => "w", 4 => "l", _ => "q" };
        let op_name = match alu_op { 0 => "add", 1 => "or", 2 => "adc", 3 => "sbb", 4 => "and", 5 => "sub", 6 => "xor", 7 => "cmp", _ => "?" };
        let mnemonic = format!("{}{}", op_name, suffix);
        self.encode_alu(ops, &mnemonic, alu_op)
    }

    pub(crate) fn encode_suffixless_test(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("test requires 2 operands".to_string());
        }
        let size = infer_operand_size_from_pair(&ops[0], &ops[1]);
        let suffix = match size { 1 => "b", 2 => "w", 4 => "l", _ => "q" };
        let mnemonic = format!("test{}", suffix);
        self.encode_test(ops, &mnemonic)
    }

    pub(crate) fn encode_suffixless_shift(&mut self, ops: &[Operand], shift_op: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("shift requires 2 operands".to_string());
        }
        // Size comes from dst (second) operand
        let size = match &ops[1] {
            Operand::Register(r) => infer_reg_size(&r.name),
            _ => 8,
        };
        let op_name = match shift_op { 4 => "shl", 5 => "shr", 7 => "sar", 0 => "rol", 1 => "ror", 2 => "rcl", 3 => "rcr", _ => "?" };
        let suffix = match size { 1 => "b", 2 => "w", 4 => "l", _ => "q" };
        let mnemonic = format!("{}{}", op_name, suffix);
        self.encode_shift(ops, &mnemonic, shift_op)
    }

    pub(crate) fn encode_suffixless_unary(&mut self, ops: &[Operand], op_ext: u8) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("unary op requires 1 operand".to_string());
        }
        let size = match &ops[0] {
            Operand::Register(r) => infer_reg_size(&r.name),
            _ => 8,
        };
        if size == 2 { self.bytes.push(0x66); }
        self.encode_unary_rm(ops, op_ext, size)
    }

}
