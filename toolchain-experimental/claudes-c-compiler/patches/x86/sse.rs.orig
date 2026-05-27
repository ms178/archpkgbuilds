use super::*;

impl super::InstructionEncoder {
    // ---- SSE/SSE2 encoding helpers ----

    /// Encode SSE instruction: load (xmm<-xmm/mem) or store (xmm->mem).
    pub(crate) fn encode_sse_rr_rm(&mut self, ops: &[Operand], load_opcode: &[u8], store_opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("SSE mov requires 2 operands".to_string());
        }

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&src.name) && is_xmm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                // Emit mandatory prefix bytes (e.g. 0x66, 0xF2, 0xF3) before REX.
                // The prefix ends where the 0x0F escape byte begins.
                let prefix_len = load_opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &load_opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                self.emit_rex_rr(0, &dst.name, &src.name);
                // Emit remaining opcode bytes (0x0F + opcode byte)
                self.bytes.extend_from_slice(&load_opcode[prefix_len..]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) if is_xmm(&dst.name) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let prefix_len = load_opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &load_opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                self.emit_rex_rm(0, &dst.name, mem);
                self.bytes.extend_from_slice(&load_opcode[prefix_len..]);
                self.encode_modrm_mem(dst_num, mem)
            }
            (Operand::Register(src), Operand::Memory(mem)) if is_xmm(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let prefix_len = store_opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &store_opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                self.emit_rex_rm(0, &src.name, mem);
                self.bytes.extend_from_slice(&store_opcode[prefix_len..]);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("unsupported SSE mov operands".to_string()),
        }
    }

    /// Encode a standard SSE operation: op xmm/mem, xmm
    pub(crate) fn encode_sse_op(&mut self, ops: &[Operand], opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("SSE op requires 2 operands".to_string());
        }

        // Check if operands are MMX registers - if so, skip the 0x66 prefix
        let use_mmx = match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => is_mmx(&src.name) || is_mmx(&dst.name),
            (Operand::Memory(_), Operand::Register(dst)) => is_mmx(&dst.name),
            (Operand::Register(src), Operand::Memory(_)) => is_mmx(&src.name),
            _ => false,
        };

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                if !use_mmx {
                    for &b in &opcode[..prefix_len] {
                        self.bytes.push(b);
                    }
                }
                if !use_mmx {
                    self.emit_rex_rr(0, &dst.name, &src.name);
                }
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                if !use_mmx {
                    for &b in &opcode[..prefix_len] {
                        self.bytes.push(b);
                    }
                }
                if !use_mmx {
                    self.emit_rex_rm(0, &dst.name, mem);
                }
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err("unsupported SSE op operands".to_string()),
        }
    }

    pub(crate) fn encode_sse_store(&mut self, ops: &[Operand], opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("SSE store requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Memory(mem)) if is_xmm(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                self.emit_rex_rm(0, &src.name, mem);
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("unsupported SSE store operands".to_string()),
        }
    }

    pub(crate) fn encode_sse_op_imm8(&mut self, ops: &[Operand], opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 3 {
            return Err("SSE op+imm8 requires 3 operands".to_string());
        }
        match (&ops[0], &ops[1], &ops[2]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                self.emit_rex_rr(0, &dst.name, &src.name);
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("unsupported SSE op+imm8 operands".to_string()),
        }
    }

    pub(crate) fn encode_movd(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("movd requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&dst.name) => {
                // GP -> XMM: 66 0F 6E /r
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.push(0x66);
                self.emit_rex_rr(0, &dst.name, &src.name);
                self.bytes.extend_from_slice(&[0x0F, 0x6E]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&src.name) => {
                // XMM -> GP: 66 0F 7E /r
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.push(0x66);
                self.emit_rex_rr(0, &src.name, &dst.name);
                self.bytes.extend_from_slice(&[0x0F, 0x7E]);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) if is_xmm(&dst.name) => {
                // mem -> XMM: 66 0F 6E /r
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.push(0x66);
                self.emit_rex_rm(0, &dst.name, mem);
                self.bytes.extend_from_slice(&[0x0F, 0x6E]);
                self.encode_modrm_mem(dst_num, mem)
            }
            (Operand::Register(src), Operand::Memory(mem)) if is_xmm(&src.name) => {
                // XMM -> mem: 66 0F 7E /r
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                self.bytes.push(0x66);
                self.emit_rex_rm(0, &src.name, mem);
                self.bytes.extend_from_slice(&[0x0F, 0x7E]);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("unsupported movd operands".to_string()),
        }
    }

    pub(crate) fn encode_movnti(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("movnti requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let size = if is_reg64(&src.name) { 8 } else { 4 };
                self.emit_rex_rm(size, &src.name, mem);
                self.bytes.extend_from_slice(&[0x0F, 0xC3]);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("unsupported movnti operands".to_string()),
        }
    }

    pub(crate) fn encode_movnti_q(&mut self, ops: &[Operand]) -> Result<(), String> {
        // movntiq: 0F C3 /r with REX.W (always 64-bit)
        if ops.len() != 2 {
            return Err("movntiq requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                self.emit_rex_rm(8, &src.name, mem);
                self.bytes.extend_from_slice(&[0x0F, 0xC3]);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("unsupported movntiq operands".to_string()),
        }
    }

    pub(crate) fn encode_rdrand(&mut self, ops: &[Operand], mnemonic: &str, opcode2: u8, reg_ext: u8) -> Result<(), String> {
        if ops.len() != 1 { return Err("rdrand requires 1 operand".to_string()); }
        match &ops[0] {
            Operand::Register(reg) => {
                let num = reg_num(&reg.name).ok_or("bad register")?;
                let size = if mnemonic.ends_with('q') { 8 } else if mnemonic.ends_with('l') { 4 } else { infer_reg_size(&reg.name) };
                if size == 2 { self.bytes.push(0x66); }
                self.emit_rex_unary(size, &reg.name);
                self.bytes.extend_from_slice(&[0x0F, opcode2]);
                self.bytes.push(self.modrm(3, reg_ext, num));
                Ok(())
            }
            _ => Err("rdrand requires register operand".to_string()),
        }
    }

    pub(crate) fn encode_sldt(&mut self, ops: &[Operand]) -> Result<(), String> {
        // sldt %reg: 0F 00 /0
        if ops.len() != 1 { return Err("sldt requires 1 operand".to_string()); }
        match &ops[0] {
            Operand::Register(reg) => {
                let num = reg_num(&reg.name).ok_or("bad register")?;
                let size = infer_reg_size(&reg.name);
                if size == 2 { self.bytes.push(0x66); }
                if size == 8 {
                    self.bytes.push(self.rex(true, false, false, needs_rex_ext(&reg.name)));
                } else if needs_rex_ext(&reg.name) {
                    self.bytes.push(self.rex(false, false, false, true));
                }
                self.bytes.extend_from_slice(&[0x0F, 0x00]);
                self.bytes.push(self.modrm(3, 0, num));
                Ok(())
            }
            Operand::Memory(mem) => {
                self.emit_rex_rm(0, "", mem);
                self.bytes.extend_from_slice(&[0x0F, 0x00]);
                self.encode_modrm_mem(0, mem)
            }
            _ => Err("unsupported sldt operand".to_string()),
        }
    }

    pub(crate) fn encode_str_insn(&mut self, ops: &[Operand]) -> Result<(), String> {
        // str %reg: 0F 00 /1
        if ops.len() != 1 { return Err("str requires 1 operand".to_string()); }
        match &ops[0] {
            Operand::Register(reg) => {
                let num = reg_num(&reg.name).ok_or("bad register")?;
                let size = infer_reg_size(&reg.name);
                if size == 2 { self.bytes.push(0x66); }
                if size == 8 {
                    self.bytes.push(self.rex(true, false, false, needs_rex_ext(&reg.name)));
                } else if needs_rex_ext(&reg.name) {
                    self.bytes.push(self.rex(false, false, false, true));
                }
                self.bytes.extend_from_slice(&[0x0F, 0x00]);
                self.bytes.push(self.modrm(3, 1, num));
                Ok(())
            }
            _ => Err("unsupported str operand".to_string()),
        }
    }

    pub(crate) fn encode_sse_cvt_gp_to_xmm(&mut self, ops: &[Operand], opcode: &[u8], gp_size: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("cvt requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                // Need REX.W for 64-bit source
                self.emit_rex_rr(gp_size, &dst.name, &src.name);
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            _ => Err("unsupported cvt operands".to_string()),
        }
    }

    pub(crate) fn encode_sse_cvt_xmm_to_gp(&mut self, ops: &[Operand], opcode: &[u8], gp_size: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("cvt requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                self.emit_rex_rr(gp_size, &dst.name, &src.name);
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            _ => Err("unsupported cvt operands".to_string()),
        }
    }

    pub(crate) fn encode_sse_shift(&mut self, ops: &[Operand], reg_opcode: &[u8], imm_ext: u8, imm_opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("SSE shift requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let prefix_len = imm_opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &imm_opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                if needs_rex_ext(&dst.name) {
                    self.bytes.push(self.rex(false, false, false, true));
                }
                self.bytes.extend_from_slice(&imm_opcode[prefix_len..]);
                self.bytes.push(self.modrm(3, imm_ext, dst_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&src.name) => {
                self.encode_sse_op(&[ops[0].clone(), ops[1].clone()], reg_opcode)
            }
            _ => Err("unsupported SSE shift operands".to_string()),
        }
    }

    pub(crate) fn encode_sse_shift_imm_only(&mut self, ops: &[Operand], imm_ext: u8, opcode: &[u8]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("SSE shift requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                if needs_rex_ext(&dst.name) {
                    self.bytes.push(self.rex(false, false, false, true));
                }
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.bytes.push(self.modrm(3, imm_ext, dst_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            _ => Err("unsupported SSE shift operands".to_string()),
        }
    }

    pub(crate) fn encode_sse_insert(&mut self, ops: &[Operand], opcode: &[u8], rex_w: bool) -> Result<(), String> {
        if ops.len() != 3 {
            return Err("pinsrX requires 3 operands".to_string());
        }
        // pinsrX $imm, r/m, xmm
        let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
        match (&ops[0], &ops[1], &ops[2]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                let size = if rex_w { 8 } else { 0 };
                self.emit_rex_rr(size, &dst.name, &src.name);
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                self.bytes.push(*imm as u8);
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                let size = if rex_w { 8 } else { 0 };
                self.emit_rex_rm(size, &dst.name, mem);
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                let rc = self.relocations.len();
                self.encode_modrm_mem(dst_num, mem)?;
                self.bytes.push(*imm as u8);
                self.adjust_rip_reloc_addend(rc, 1);
                Ok(())
            }
            _ => Err("unsupported pinsrX operands".to_string()),
        }
    }

    /// Encode SSE/SSE4.1 extract instructions (pextrw, pextrd, pextrb, pextrq).
    /// `swap_reg_rm`: if true, the GP register goes in the ModRM "reg" field and the
    /// XMM register goes in "r/m". This is needed for legacy pextrw (66 0F C5) which
    /// uses inverted field assignment compared to SSE4.1 extracts (66 0F 3A xx).
    pub(crate) fn encode_sse_extract(&mut self, ops: &[Operand], opcode: &[u8], rex_w: bool, swap_reg_rm: bool) -> Result<(), String> {
        if ops.len() != 3 {
            return Err("pextrX requires 3 operands".to_string());
        }
        // pextrX $imm, xmm, r/m
        let prefix_len = opcode.iter().position(|&b| b == 0x0F).unwrap_or(0);
        match (&ops[0], &ops[1], &ops[2]) {
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                let size = if rex_w { 8 } else { 0 };
                if swap_reg_rm {
                    // Legacy pextrw: GP in reg field, XMM in r/m field
                    self.emit_rex_rr(size, &dst.name, &src.name);
                    self.bytes.extend_from_slice(&opcode[prefix_len..]);
                    self.bytes.push(self.modrm(3, dst_num, src_num));
                } else {
                    // SSE4.1: XMM in reg field, GP in r/m field
                    self.emit_rex_rr(size, &src.name, &dst.name);
                    self.bytes.extend_from_slice(&opcode[prefix_len..]);
                    self.bytes.push(self.modrm(3, src_num, dst_num));
                }
                self.bytes.push(*imm as u8);
                Ok(())
            }
            (Operand::Immediate(ImmediateValue::Integer(imm)), Operand::Register(src), Operand::Memory(mem)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                for &b in &opcode[..prefix_len] {
                    self.bytes.push(b);
                }
                let size = if rex_w { 8 } else { 0 };
                self.emit_rex_rm(size, &src.name, mem);
                self.bytes.extend_from_slice(&opcode[prefix_len..]);
                let rc = self.relocations.len();
                self.encode_modrm_mem(src_num, mem)?;
                self.bytes.push(*imm as u8);
                self.adjust_rip_reloc_addend(rc, 1);
                Ok(())
            }
            _ => Err("unsupported pextrX operands".to_string()),
        }
    }

    // ---- MOVQ with XMM registers ----

    /// Encode movq with XMM register operands.
    /// Forms:
    ///   movq %gp64, %xmm  -> 66 REX.W 0F 6E /r (GP -> XMM)
    ///   movq %xmm, %gp64  -> 66 REX.W 0F 7E /r (XMM -> GP)
    ///   movq mem, %xmm    -> F3 0F 7E /r (load 64-bit from memory)
    ///   movq %xmm, mem    -> 66 0F D6 /r (store 64-bit to memory)
    ///   movq %xmm, %xmm   -> F3 0F 7E /r (xmm-to-xmm)
    pub(crate) fn encode_movq_xmm(&mut self, ops: &[Operand]) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("movq requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            // movq %gp64, %xmm -> 66 REX.W 0F 6E /r
            (Operand::Register(src), Operand::Register(dst)) if !is_xmm(&src.name) && is_xmm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.push(0x66);
                // REX.W with proper R (dst xmm ext) and B (src gp ext)
                let r = needs_rex_ext(&dst.name);
                let b = needs_rex_ext(&src.name);
                self.bytes.push(self.rex(true, r, false, b));
                self.bytes.extend_from_slice(&[0x0F, 0x6E]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            // movq %xmm, %gp64 -> 66 REX.W 0F 7E /r
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&src.name) && !is_xmm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.push(0x66);
                // REX.W with proper R (src xmm ext) and B (dst gp ext)
                let r = needs_rex_ext(&src.name);
                let b = needs_rex_ext(&dst.name);
                self.bytes.push(self.rex(true, r, false, b));
                self.bytes.extend_from_slice(&[0x0F, 0x7E]);
                self.bytes.push(self.modrm(3, src_num, dst_num));
                Ok(())
            }
            // movq %xmm, %xmm -> F3 0F 7E /r
            (Operand::Register(src), Operand::Register(dst)) if is_xmm(&src.name) && is_xmm(&dst.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.push(0xF3);
                self.emit_rex_rr(0, &dst.name, &src.name);
                self.bytes.extend_from_slice(&[0x0F, 0x7E]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            // movq mem, %xmm -> F3 0F 7E /r (load)
            (Operand::Memory(mem), Operand::Register(dst)) if is_xmm(&dst.name) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;
                self.bytes.push(0xF3);
                self.emit_rex_rm(0, &dst.name, mem);
                self.bytes.extend_from_slice(&[0x0F, 0x7E]);
                self.encode_modrm_mem(dst_num, mem)
            }
            // movq %xmm, mem -> 66 0F D6 /r (store)
            (Operand::Register(src), Operand::Memory(mem)) if is_xmm(&src.name) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                self.bytes.push(0x66);
                self.emit_rex_rm(0, &src.name, mem);
                self.bytes.extend_from_slice(&[0x0F, 0xD6]);
                self.encode_modrm_mem(src_num, mem)
            }
            _ => Err("unsupported movq operand combination".to_string()),
        }
    }

    // ---- CRC32 encoding ----

    /// Encode CRC32 instruction.
    /// crc32b: F2 0F 38 F0 /r (8-bit source)
    /// crc32w: 66 F2 0F 38 F1 /r (16-bit source)
    /// crc32l: F2 0F 38 F1 /r (32-bit source)
    /// crc32q: F2 REX.W 0F 38 F1 /r (64-bit source)
    pub(crate) fn encode_crc32(&mut self, ops: &[Operand], src_size: u8) -> Result<(), String> {
        if ops.len() != 2 {
            return Err("crc32 requires 2 operands".to_string());
        }
        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;

                if src_size == 2 {
                    self.bytes.push(0x66); // operand size prefix for 16-bit
                }
                self.bytes.push(0xF2); // mandatory prefix

                // REX prefix
                let w = src_size == 8;
                let r = needs_rex_ext(&dst.name);
                let b = needs_rex_ext(&src.name);
                let need_rex = w || r || b || (src_size == 1 && is_rex_required_8bit(&src.name));
                if need_rex {
                    self.bytes.push(self.rex(w, r, false, b));
                }

                self.bytes.extend_from_slice(&[0x0F, 0x38]);
                if src_size == 1 {
                    self.bytes.push(0xF0);
                } else {
                    self.bytes.push(0xF1);
                }
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad register")?;

                if src_size == 2 {
                    self.bytes.push(0x66);
                }
                self.bytes.push(0xF2);

                let w = src_size == 8;
                let r = needs_rex_ext(&dst.name);
                let b_ext = mem.base.as_ref().is_some_and(|b| needs_rex_ext(&b.name));
                let x = mem.index.as_ref().is_some_and(|i| needs_rex_ext(&i.name));
                let need_rex = w || r || b_ext || x;
                if need_rex {
                    self.bytes.push(self.rex(w, r, x, b_ext));
                }

                self.bytes.extend_from_slice(&[0x0F, 0x38]);
                if src_size == 1 {
                    self.bytes.push(0xF0);
                } else {
                    self.bytes.push(0xF1);
                }
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err("unsupported crc32 operands".to_string()),
        }
    }

    // ---- SSE memory-only encoding (ldmxcsr, stmxcsr) ----

    /// Encode an SSE instruction with a single memory operand and a ModR/M extension.
    /// Format: [opcode bytes] ModR/M(ext, mem)
    /// Used for ldmxcsr (0F AE /2) and stmxcsr (0F AE /3).
    pub(crate) fn encode_sse_mem_only(&mut self, ops: &[Operand], opcode: &[u8], ext: u8) -> Result<(), String> {
        if ops.len() != 1 {
            return Err("SSE mem-only op requires 1 operand".to_string());
        }
        match &ops[0] {
            Operand::Memory(mem) => {
                self.emit_segment_prefix(mem)?;
                // No REX.W needed for 32-bit memory operations
                self.emit_rex_rm(0, "", mem);
                self.bytes.extend_from_slice(opcode);
                self.encode_modrm_mem(ext, mem)
            }
            _ => Err("SSE mem-only op requires memory operand".to_string()),
        }
    }

    // ---- BSF/BSR encoding ----

    /// Encode BSF (Bit Scan Forward) or BSR (Bit Scan Reverse).
    /// bsfl/bsfq: 0F BC r/m -> reg
    /// bsrl/bsrq: 0F BD r/m -> reg
    pub(crate) fn encode_bsf_bsr(&mut self, ops: &[Operand], mnemonic: &str) -> Result<(), String> {
        if ops.len() != 2 {
            return Err(format!("{} requires 2 operands", mnemonic));
        }
        let size = mnemonic_size_suffix(mnemonic).unwrap_or(8);
        let opcode_byte = if mnemonic.starts_with("bsf") { 0xBC } else { 0xBD };

        match (&ops[0], &ops[1]) {
            (Operand::Register(src), Operand::Register(dst)) => {
                let src_num = reg_num(&src.name).ok_or("bad src register")?;
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                self.emit_rex_rr(size, &dst.name, &src.name);
                self.bytes.extend_from_slice(&[0x0F, opcode_byte]);
                self.bytes.push(self.modrm(3, dst_num, src_num));
                Ok(())
            }
            (Operand::Memory(mem), Operand::Register(dst)) => {
                let dst_num = reg_num(&dst.name).ok_or("bad dst register")?;
                self.emit_segment_prefix(mem)?;
                self.emit_rex_rm(size, &dst.name, mem);
                self.bytes.extend_from_slice(&[0x0F, opcode_byte]);
                self.encode_modrm_mem(dst_num, mem)
            }
            _ => Err(format!("unsupported {} operands", mnemonic)),
        }
    }

}
