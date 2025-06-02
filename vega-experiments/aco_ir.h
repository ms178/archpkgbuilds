/*
 * Copyright © 2018 Valve Corporation
 *
 * SPDX-License-Identifier: MIT
 */

#ifndef ACO_IR_H
#define ACO_IR_H

#include "aco_opcodes.h"
#include "aco_shader_info.h"
#include "aco_util.h"

#include "util/compiler.h"

#include "ac_binary.h"
#include "ac_hw_stage.h"
#include "ac_shader_debug_info.h"
#include "ac_shader_util.h"
#include "amd_family.h"
#include <algorithm>
#include <bitset>
#include <memory>
#include <vector>
#include <set>

typedef struct nir_shader nir_shader;

namespace aco {

      extern uint64_t debug_flags;

      enum {
            DEBUG_VALIDATE_IR = 0x1,
            DEBUG_VALIDATE_RA = 0x2,
            DEBUG_VALIDATE_LIVE_VARS = 0x4,
            DEBUG_FORCE_WAITCNT = 0x8,
            DEBUG_NO_VN = 0x10,
            DEBUG_NO_OPT = 0x20,
            DEBUG_NO_SCHED = 0x40,
            DEBUG_PERF_INFO = 0x80,
            DEBUG_LIVE_INFO = 0x100,
            DEBUG_FORCE_WAITDEPS = 0x200,
            DEBUG_NO_VALIDATE = 0x400,
            DEBUG_NO_SCHED_ILP = 0x800,
            DEBUG_NO_SCHED_VOPD = 0x1000,
            DEBUG_VALIDATE_OPT = 0x2000,
      };

      enum storage_class : uint8_t {
            storage_none = 0x0,
            storage_buffer = 0x1,
            storage_gds = 0x2,
            storage_image = 0x4,
            storage_shared = 0x8,
            storage_vmem_output = 0x10,
            storage_task_payload = 0x20,
            storage_scratch = 0x40,
            storage_vgpr_spill = 0x80,
            storage_count = 8,
      };

      enum memory_semantics : uint8_t {
            semantic_none = 0x0,
            semantic_acquire = 0x1,
            semantic_release = 0x2,
            semantic_volatile = 0x4,
            semantic_private = 0x8,
            semantic_can_reorder = 0x10,
            semantic_atomic = 0x20,
            semantic_rmw = 0x40,
            semantic_acqrel = semantic_acquire | semantic_release,
            semantic_atomicrmw = semantic_volatile | semantic_atomic | semantic_rmw,
      };

      enum sync_scope : uint8_t {
            scope_invocation = 0,
            scope_subgroup = 1,
            scope_workgroup = 2,
            scope_queuefamily = 3,
            scope_device = 4,
      };

      struct memory_sync_info {
            memory_sync_info() : storage(storage_none), semantics(semantic_none), scope(scope_invocation) {}
            memory_sync_info(int storage_, int semantics_ = 0, sync_scope scope_ = scope_invocation)
            : storage((storage_class)storage_), semantics((memory_semantics)semantics_), scope(scope_)
            {}

            storage_class storage : 8;
            memory_semantics semantics : 8;
            sync_scope scope : 8;

            bool operator==(const memory_sync_info& rhs) const
            {
                  return storage == rhs.storage && semantics == rhs.semantics && scope == rhs.scope;
            }

            bool can_reorder() const
            {
                  if (semantics & semantic_acqrel)
                        return false;
                  return (!storage || (semantics & semantic_can_reorder)) && !(semantics & semantic_volatile);
            }
      };
      static_assert(sizeof(memory_sync_info) == 3, "Unexpected padding");

      enum fp_round {
            fp_round_ne = 0,
            fp_round_pi = 1,
            fp_round_ni = 2,
            fp_round_tz = 3,
      };

      enum fp_denorm {
            fp_denorm_flush = 0x0,
            fp_denorm_keep_in = 0x1,
            fp_denorm_keep_out = 0x2,
            fp_denorm_keep = 0x3,
      };

      struct float_mode {
            union {
                  struct {
                        fp_round round32 : 2;
                        fp_round round16_64 : 2;
                        unsigned denorm32 : 2;
                        unsigned denorm16_64 : 2;
                  };
                  struct {
                        uint8_t round : 4;
                        uint8_t denorm : 4;
                  };
                  uint8_t val = 0;
            };
            bool must_flush_denorms32 : 1;
            bool must_flush_denorms16_64 : 1;
            bool care_about_round32 : 1;
            bool care_about_round16_64 : 1;

            bool canReplace(float_mode other) const noexcept
            {
                  return val == other.val && (must_flush_denorms32 || !other.must_flush_denorms32) &&
                  (must_flush_denorms16_64 || !other.must_flush_denorms16_64) &&
                  (care_about_round32 || !other.care_about_round32) &&
                  (care_about_round16_64 || !other.care_about_round16_64);
            }
      };

      enum wait_type {
            wait_type_exp = 0,
            wait_type_lgkm = 1,
            wait_type_vm = 2,
            wait_type_vs = 3,
            wait_type_sample = 4,
            wait_type_bvh = 5,
            wait_type_km = 6,
            wait_type_num = 7,
      };

      struct Instruction;
      class Builder;

      struct wait_imm {
            static const uint8_t unset_counter = 0xff;

            uint8_t exp;
            uint8_t lgkm;
            uint8_t vm;
            uint8_t vs;
            uint8_t sample;
            uint8_t bvh;
            uint8_t km;

            wait_imm();
            wait_imm(uint16_t vm_, uint16_t exp_, uint16_t lgkm_, uint16_t vs_);

            uint16_t pack(enum amd_gfx_level chip) const;

            static wait_imm max(enum amd_gfx_level gfx_level);

            bool unpack(enum amd_gfx_level gfx_level, const Instruction* instr);

            bool combine(const wait_imm& other);

            bool empty() const;

            void print(FILE* output) const;

            void build_waitcnt(Builder& bld);

            uint8_t& operator[](size_t i)
            {
                  assert(i < wait_type_num);
                  return *((uint8_t*)this + i);
            }

            const uint8_t& operator[](size_t i) const
            {
                  assert(i < wait_type_num);
                  return *((uint8_t*)this + i);
            }
      };
      static_assert(offsetof(wait_imm, exp) == wait_type_exp);
      static_assert(offsetof(wait_imm, lgkm) == wait_type_lgkm);
      static_assert(offsetof(wait_imm, vm) == wait_type_vm);
      static_assert(offsetof(wait_imm, vs) == wait_type_vs);
      static_assert(offsetof(wait_imm, sample) == wait_type_sample);
      static_assert(offsetof(wait_imm, bvh) == wait_type_bvh);
      static_assert(offsetof(wait_imm, km) == wait_type_km);

      enum wait_event_imm : uint16_t {
            wait_event_imm_dont_wait_export_ready_gfx11 = 0x1,
            wait_event_imm_wait_export_ready_gfx12 = 0x2,
      };

      constexpr Format
      asVOP3(Format format)
      {
            return (Format)((uint32_t)Format::VOP3 | (uint32_t)format);
      };

      constexpr Format
      asSDWA(Format format)
      {
            assert(format == Format::VOP1 || format == Format::VOP2 || format == Format::VOPC);
            return (Format)((uint32_t)Format::SDWA | (uint32_t)format);
      }

      constexpr Format
      withoutDPP(Format format)
      {
            return (Format)((uint32_t)format & ~((uint32_t)Format::DPP16 | (uint32_t)Format::DPP8));
      }

      constexpr Format
      withoutVOP3(Format format)
      {
            return (Format)((uint32_t)format & ~((uint32_t)Format::VOP3));
      }

      enum class RegType {
            sgpr,
            vgpr,
      };

      struct RegClass {

            enum RC : uint8_t {
                  s1 = 1,
                  s2 = 2,
                  s3 = 3,
                  s4 = 4,
                  s6 = 6,
                  s8 = 8,
                  s16 = 16,
                  v1 = s1 | (1 << 5),
                  v2 = s2 | (1 << 5),
                  v3 = s3 | (1 << 5),
                  v4 = s4 | (1 << 5),
                  v5 = 5 | (1 << 5),
                  v6 = 6 | (1 << 5),
                  v7 = 7 | (1 << 5),
                  v8 = 8 | (1 << 5),
                  v10 = 10 | (1 << 5),
                  v1b = v1 | (1 << 7),
                  v2b = v2 | (1 << 7),
                  v3b = v3 | (1 << 7),
                  v4b = v4 | (1 << 7),
                  v6b = v6 | (1 << 7),
                  v8b = v8 | (1 << 7),
                  v1_linear = v1 | (1 << 6),
                  v2_linear = v2 | (1 << 6),
            };

            RegClass() = default;
            constexpr RegClass(RC rc_) : rc(rc_) {}
            constexpr RegClass(RegType type, unsigned size)
            : rc((RC)((type == RegType::vgpr ? 1 << 5 : 0) | size))
            {}

            constexpr operator RC() const { return rc; }
            explicit operator bool() = delete;

            constexpr RegType type() const { return rc <= RC::s16 ? RegType::sgpr : RegType::vgpr; }
            constexpr bool is_linear_vgpr() const { return rc & (1 << 6); };
            constexpr bool is_subdword() const { return rc & (1 << 7); }
            constexpr unsigned bytes() const { return ((unsigned)rc & 0x1F) * (is_subdword() ? 1 : 4); }
            constexpr unsigned size() const { return (bytes() + 3) >> 2; }
            constexpr bool is_linear() const { return rc <= RC::s16 || is_linear_vgpr(); }
            constexpr RegClass as_linear() const { return RegClass((RC)(rc | (1 << 6))); }
            constexpr RegClass as_subdword() const { return RegClass((RC)(rc | 1 << 7)); }

            static constexpr RegClass get(RegType type, unsigned bytes)
            {
                  if (type == RegType::sgpr) {
                        return RegClass(type, DIV_ROUND_UP(bytes, 4u));
                  } else {
                        return bytes % 4u ? RegClass(type, bytes).as_subdword() : RegClass(type, bytes / 4u);
                  }
            }

            constexpr RegClass resize(unsigned bytes) const
            {
                  if (is_linear_vgpr()) {
                        assert(bytes % 4u == 0);
                        return get(RegType::vgpr, bytes).as_linear();
                  }
                  return get(type(), bytes);
            }

      private:
            RC rc;
      };

      static constexpr RegClass s1{RegClass::s1};
      static constexpr RegClass s2{RegClass::s2};
      static constexpr RegClass s3{RegClass::s3};
      static constexpr RegClass s4{RegClass::s4};
      static constexpr RegClass s8{RegClass::s8};
      static constexpr RegClass s16{RegClass::s16};
      static constexpr RegClass v1{RegClass::v1};
      static constexpr RegClass v2{RegClass::v2};
      static constexpr RegClass v3{RegClass::v3};
      static constexpr RegClass v4{RegClass::v4};
      static constexpr RegClass v5{RegClass::v5};
      static constexpr RegClass v6{RegClass::v6};
      static constexpr RegClass v7{RegClass::v7};
      static constexpr RegClass v8{RegClass::v8};
      static constexpr RegClass v10{RegClass::v10};
      static constexpr RegClass v1b{RegClass::v1b};
      static constexpr RegClass v2b{RegClass::v2b};
      static constexpr RegClass v3b{RegClass::v3b};
      static constexpr RegClass v4b{RegClass::v4b};
      static constexpr RegClass v6b{RegClass::v6b};
      static constexpr RegClass v8b{RegClass::v8b};

      struct Temp {
            Temp() noexcept : id_(0), reg_class(0) {}
            constexpr Temp(uint32_t id, RegClass cls) noexcept : id_(id), reg_class(uint8_t(cls)) {}

            constexpr uint32_t id() const noexcept { return id_; }
            constexpr RegClass regClass() const noexcept { return (RegClass::RC)reg_class; }

            constexpr unsigned bytes() const noexcept { return regClass().bytes(); }
            constexpr unsigned size() const noexcept { return regClass().size(); }
            constexpr RegType type() const noexcept { return regClass().type(); }
            constexpr bool is_linear() const noexcept { return regClass().is_linear(); }

            constexpr bool operator<(Temp other) const noexcept { return id() < other.id(); }
            constexpr bool operator==(Temp other) const noexcept { return id() == other.id(); }
            constexpr bool operator!=(Temp other) const noexcept { return id() != other.id(); }

      private:
            uint32_t id_ : 24;
            uint32_t reg_class : 8;
      };

      struct PhysReg {
            constexpr PhysReg() = default;
            explicit constexpr PhysReg(unsigned r) : reg_b(r << 2) {}
            constexpr unsigned reg() const { return reg_b >> 2; }
            constexpr unsigned byte() const { return reg_b & 0x3; }
            constexpr operator unsigned() const { return reg(); }
            constexpr bool operator==(PhysReg other) const { return reg_b == other.reg_b; }
            constexpr bool operator!=(PhysReg other) const { return reg_b != other.reg_b; }
            constexpr bool operator<(PhysReg other) const { return reg_b < other.reg_b; }
            constexpr PhysReg advance(int bytes) const
            {
                  PhysReg res = *this;
                  res.reg_b += bytes;
                  return res;
            }

            uint16_t reg_b = 0;
      };

      static constexpr PhysReg m0{124};
      static constexpr PhysReg flat_scr_lo{102};
      static constexpr PhysReg flat_scr_hi{103};
      static constexpr PhysReg vcc{106};
      static constexpr PhysReg vcc_hi{107};
      static constexpr PhysReg tba_lo{108};
      static constexpr PhysReg tba_hi{109};
      static constexpr PhysReg tma_lo{110};
      static constexpr PhysReg tma_hi{111};
      static constexpr PhysReg ttmp0{112};
      static constexpr PhysReg ttmp1{113};
      static constexpr PhysReg ttmp2{114};
      static constexpr PhysReg ttmp3{115};
      static constexpr PhysReg ttmp4{116};
      static constexpr PhysReg ttmp5{117};
      static constexpr PhysReg ttmp6{118};
      static constexpr PhysReg ttmp7{119};
      static constexpr PhysReg ttmp8{120};
      static constexpr PhysReg ttmp9{121};
      static constexpr PhysReg ttmp10{122};
      static constexpr PhysReg ttmp11{123};
      static constexpr PhysReg sgpr_null{125};
      static constexpr PhysReg exec{126};
      static constexpr PhysReg exec_lo{126};
      static constexpr PhysReg exec_hi{127};
      static constexpr PhysReg pops_exiting_wave_id{239};
      static constexpr PhysReg scc{253};

      class Operand final {
      public:
            constexpr Operand() noexcept
            {
                  isUndef_ = true;
                  setFixed(PhysReg{128});
            }

            explicit Operand(Temp r) noexcept
            {
                  data_.temp = r;
                  if (r.id()) {
                        isTemp_ = true;
                  } else {
                        isUndef_ = true;
                        setFixed(PhysReg{128});
                  }
            };
            explicit Operand(Temp r, PhysReg reg) noexcept
            {
                  assert(r.id());
                  data_.temp = r;
                  isTemp_ = true;
                  setPrecolored(reg);
            };

            static Operand c8(uint8_t v) noexcept
            {
                  Operand op;
                  op.control_ = 0;
                  op.data_.i = v;
                  op.isConstant_ = true;
                  op.constSize = 0;
                  op.setFixed(PhysReg{0u});
                  return op;
            };

            static Operand c16(uint16_t v) noexcept
            {
                  Operand op;
                  op.control_ = 0;
                  op.data_.i = v;
                  op.isConstant_ = true;
                  op.constSize = 1;
                  if (v <= 64)
                        op.setFixed(PhysReg{128u + v});
                  else if (v >= 0xFFF0)
                        op.setFixed(PhysReg{(unsigned)(192 - (int16_t)v)});
                  else if (v == 0x3800)
                        op.setFixed(PhysReg{240});
                  else if (v == 0xB800)
                        op.setFixed(PhysReg{241});
                  else if (v == 0x3C00)
                        op.setFixed(PhysReg{242});
                  else if (v == 0xBC00)
                        op.setFixed(PhysReg{243});
                  else if (v == 0x4000)
                        op.setFixed(PhysReg{244});
                  else if (v == 0xC000)
                        op.setFixed(PhysReg{245});
                  else if (v == 0x4400)
                        op.setFixed(PhysReg{246});
                  else if (v == 0xC400)
                        op.setFixed(PhysReg{247});
                  else if (v == 0x3118)
                        op.setFixed(PhysReg{248});
                  else
                        op.setFixed(PhysReg{255});
                  return op;
            }

            static Operand c32(uint32_t v) noexcept { return c32_or_c64(v, false); }

            static Operand c64(uint64_t v) noexcept
            {
                  Operand op;
                  op.control_ = 0;
                  op.isConstant_ = true;
                  op.constSize = 3;
                  if (v <= 64) {
                        op.data_.i = (uint32_t)v;
                        op.setFixed(PhysReg{128 + (uint32_t)v});
                  } else if (v >= 0xFFFFFFFFFFFFFFF0) {
                        op.data_.i = (uint32_t)v;
                        op.setFixed(PhysReg{192 - (uint32_t)v});
                  } else if (v == 0x3FE0000000000000) {
                        op.data_.i = 0x3f000000;
                        op.setFixed(PhysReg{240});
                  } else if (v == 0xBFE0000000000000) {
                        op.data_.i = 0xbf000000;
                        op.setFixed(PhysReg{241});
                  } else if (v == 0x3FF0000000000000) {
                        op.data_.i = 0x3f800000;
                        op.setFixed(PhysReg{242});
                  } else if (v == 0xBFF0000000000000) {
                        op.data_.i = 0xbf800000;
                        op.setFixed(PhysReg{243});
                  } else if (v == 0x4000000000000000) {
                        op.data_.i = 0x40000000;
                        op.setFixed(PhysReg{244});
                  } else if (v == 0xC000000000000000) {
                        op.data_.i = 0xc0000000;
                        op.setFixed(PhysReg{245});
                  } else if (v == 0x4010000000000000) {
                        op.data_.i = 0x40800000;
                        op.setFixed(PhysReg{246});
                  } else if (v == 0xC010000000000000) {
                        op.data_.i = 0xc0800000;
                        op.setFixed(PhysReg{247});
                  } else {
                        op.signext = v >> 63;
                        op.data_.i = v & 0xffffffffu;
                        op.setFixed(PhysReg{255});
                        assert(op.constantValue64() == v &&
                        "attempt to create a unrepresentable 64-bit literal constant");
                  }
                  return op;
            }

            static Operand c32_or_c64(uint32_t v, bool is64bit) noexcept
            {
                  Operand op;
                  op.control_ = 0;
                  op.data_.i = v;
                  op.isConstant_ = true;
                  op.constSize = is64bit ? 3 : 2;
                  if (v <= 64)
                        op.setFixed(PhysReg{128 + v});
                  else if (v >= 0xFFFFFFF0)
                        op.setFixed(PhysReg{192 - v});
                  else if (v == 0x3f000000)
                        op.setFixed(PhysReg{240});
                  else if (v == 0xbf000000)
                        op.setFixed(PhysReg{241});
                  else if (v == 0x3f800000)
                        op.setFixed(PhysReg{242});
                  else if (v == 0xbf800000)
                        op.setFixed(PhysReg{243});
                  else if (v == 0x40000000)
                        op.setFixed(PhysReg{244});
                  else if (v == 0xc0000000)
                        op.setFixed(PhysReg{245});
                  else if (v == 0x40800000)
                        op.setFixed(PhysReg{246});
                  else if (v == 0xc0800000)
                        op.setFixed(PhysReg{247});
                  else {
                        assert(!is64bit && "attempt to create a 64-bit literal constant");
                        op.setFixed(PhysReg{255});
                  }
                  return op;
            }

            static Operand literal32(uint32_t v) noexcept
            {
                  Operand op;
                  op.control_ = 0;
                  op.data_.i = v;
                  op.isConstant_ = true;
                  op.constSize = 2;
                  op.setFixed(PhysReg{255});
                  return op;
            }

            explicit Operand(RegClass type) noexcept
            {
                  isUndef_ = true;
                  data_.temp = Temp(0, type);
                  setFixed(PhysReg{128});
            };
            explicit Operand(PhysReg reg, RegClass type) noexcept
            {
                  data_.temp = Temp(0, type);
                  setFixed(reg);
            }

            static Operand zero(unsigned bytes = 4) noexcept
            {
                  if (bytes == 8)
                        return Operand::c64(0);
                  else if (bytes == 4)
                        return Operand::c32(0);
                  else if (bytes == 2)
                        return Operand::c16(0);
                  assert(bytes == 1);
                  return Operand::c8(0);
            }

            static Operand get_const(enum amd_gfx_level chip, uint64_t val, unsigned bytes)
            {
                  if (val == 0x3e22f983 && bytes == 4 && chip >= GFX8) {
                        Operand op = Operand::c32(val);
                        op.setFixed(PhysReg{248});
                        return op;
                  }

                  if (bytes == 8)
                        return Operand::c64(val);
                  else if (bytes == 4)
                        return Operand::c32(val);
                  else if (bytes == 2)
                        return Operand::c16(val);
                  assert(bytes == 1);
                  return Operand::c8(val);
            }

            static bool is_constant_representable(uint64_t val, unsigned bytes, bool zext = false,
                                                  bool sext = false)
            {
                  if (bytes <= 4)
                        return true;

                  if (zext && (val & 0xFFFFFFFF00000000) == 0x0000000000000000)
                        return true;
                  uint64_t upper33 = val & 0xFFFFFFFF80000000;
                  if (sext && (upper33 == 0xFFFFFFFF80000000 || upper33 == 0))
                        return true;

                  return val >= 0xFFFFFFFFFFFFFFF0 || val <= 64 ||
                  val == 0x3FE0000000000000 ||
                  val == 0xBFE0000000000000 ||
                  val == 0x3FF0000000000000 ||
                  val == 0xBFF0000000000000 ||
                  val == 0x4000000000000000 ||
                  val == 0xC000000000000000 ||
                  val == 0x4010000000000000 ||
                  val == 0xC010000000000000;
            }

            constexpr bool isTemp() const noexcept { return isTemp_; }

            constexpr void setTemp(Temp t) noexcept
            {
                  assert(!isConstant_);
                  if (t.id() != 0)
                        isTemp_ = true;
                  data_.temp = t;
            }

            constexpr Temp getTemp() const noexcept { return data_.temp; }

            constexpr uint32_t tempId() const noexcept { return data_.temp.id(); }

            constexpr bool hasRegClass() const noexcept { return !isConstant(); }

            constexpr RegClass regClass() const noexcept { return data_.temp.regClass(); }

            constexpr unsigned bytes() const noexcept
            {
                  if (isConstant())
                        return 1 << constSize;
                  else
                        return data_.temp.bytes();
            }

            constexpr unsigned size() const noexcept
            {
                  if (isConstant())
                        return constSize > 2 ? 2 : 1;
                  else
                        return data_.temp.size();
            }

            constexpr bool isFixed() const noexcept { return isFixed_; }

            constexpr PhysReg physReg() const noexcept { return reg_; }

            constexpr void setFixed(PhysReg reg) noexcept
            {
                  isFixed_ = reg != unsigned(-1);
                  reg_ = reg;
            }

            constexpr bool isPrecolored() const noexcept { return isPrecolored_; }
            constexpr void setPrecolored(PhysReg reg) noexcept
            {
                  setFixed(reg);
                  isPrecolored_ = isFixed_;
            }

            constexpr bool isConstant() const noexcept { return isConstant_; }

            constexpr bool isLiteral() const noexcept { return isConstant() && reg_ == 255; }

            constexpr bool isUndefined() const noexcept { return isUndef_; }

            constexpr uint32_t constantValue() const noexcept { return data_.i; }

            constexpr bool constantEquals(uint32_t cmp) const noexcept
            {
                  return isConstant() && constantValue() == cmp;
            }

            constexpr uint64_t constantValue64() const noexcept
            {
                  if (constSize == 3) {
                        if (reg_ <= 192)
                              return reg_ - 128;
                        else if (reg_ <= 208)
                              return 0xFFFFFFFFFFFFFFFF - (reg_ - 193);

                        switch (reg_) {
                              case 240: return 0x3FE0000000000000;
                              case 241: return 0xBFE0000000000000;
                              case 242: return 0x3FF0000000000000;
                              case 243: return 0xBFF0000000000000;
                              case 244: return 0x4000000000000000;
                              case 245: return 0xC000000000000000;
                              case 246: return 0x4010000000000000;
                              case 247: return 0xC010000000000000;
                              case 255:
                                    return (signext && (data_.i & 0x80000000u) ? 0xffffffff00000000ull : 0ull) | data_.i;
                        }
                        unreachable("invalid register for 64-bit constant");
                  } else {
                        return data_.i;
                  }
            }

            constexpr uint16_t constantValue16(bool opsel) const noexcept
            {
                  assert(bytes() == 2 || bytes() == 4);
                  if (opsel) {
                        if (bytes() == 2 && int16_t(data_.i) >= -16 && int16_t(data_.i) <= 64 && !isLiteral())
                              return int16_t(data_.i) >>
                              16;
                        else
                              return data_.i >> 16;
                  }
                  return data_.i;
            }

            constexpr bool isOfType(RegType type) const noexcept
            {
                  return hasRegClass() && regClass().type() == type;
            }

            constexpr void setLateKill(bool flag) noexcept { isLateKill_ = flag; }

            constexpr bool isLateKill() const noexcept { return isLateKill_; }

            constexpr void setClobbered(bool flag) noexcept { isClobbered_ = flag; }
            constexpr bool isClobbered() const noexcept { return isClobbered_; }

            constexpr void setCopyKill(bool flag) noexcept
            {
                  isCopyKill_ = flag;
                  if (flag)
                        setKill(flag);
            }
            constexpr bool isCopyKill() const noexcept { return isCopyKill_; }

            constexpr void setKill(bool flag) noexcept
            {
                  isKill_ = flag;
                  if (!flag) {
                        setFirstKill(false);
                        setCopyKill(false);
                  }
            }

            constexpr bool isKill() const noexcept { return isKill_ || isFirstKill(); }

            constexpr void setFirstKill(bool flag) noexcept
            {
                  isFirstKill_ = flag;
                  if (flag)
                        setKill(flag);
            }

            constexpr bool isFirstKill() const noexcept { return isFirstKill_; }

            constexpr bool isKillBeforeDef() const noexcept { return isKill() && !isLateKill(); }

            constexpr bool isFirstKillBeforeDef() const noexcept { return isFirstKill() && !isLateKill(); }

            constexpr void setVectorAligned(bool flag) noexcept { isVectorAligned_ = flag; }
            constexpr bool isVectorAligned() const noexcept { return isVectorAligned_; }

            constexpr bool operator==(Operand other) const noexcept
            {
                  if (other.bytes() != bytes())
                        return false;
                  if (isFixed() != other.isFixed() || isKillBeforeDef() != other.isKillBeforeDef())
                        return false;
                  if (isFixed() && physReg() != other.physReg())
                        return false;
                  if (hasRegClass() && (!other.hasRegClass() || other.regClass() != regClass()))
                        return false;

                  if (isConstant())
                        return other.isConstant() && other.constantValue64() == constantValue64();
                  else if (isUndefined())
                        return other.isUndefined();
                  else if (isTemp())
                        return other.isTemp() && other.getTemp() == getTemp();
                  else
                        return true;
            }

            constexpr bool operator!=(Operand other) const noexcept { return !operator==(other); }

            constexpr void set16bit(bool flag) noexcept { is16bit_ = flag; }

            constexpr bool is16bit() const noexcept { return is16bit_; }

            constexpr void set24bit(bool flag) noexcept { is24bit_ = flag; }

            constexpr bool is24bit() const noexcept { return is24bit_; }

      private:
            union {
                  Temp temp;
                  uint32_t i;
                  float f;
            } data_ = {Temp(0, s1)};
            PhysReg reg_;
            union {
                  struct {
                        uint8_t isTemp_ : 1;
                        uint8_t isFixed_ : 1;
                        uint8_t isPrecolored_ : 1;
                        uint8_t isConstant_ : 1;
                        uint8_t isKill_ : 1;
                        uint8_t isUndef_ : 1;
                        uint8_t isFirstKill_ : 1;
                        uint8_t isLateKill_ : 1;
                        uint8_t isClobbered_ : 1;
                        uint8_t isCopyKill_ : 1;
                        uint8_t isVectorAligned_ : 1;
                        uint8_t is16bit_ : 1;
                        uint8_t is24bit_ : 1;
                        uint8_t signext : 1;
                        uint8_t constSize : 2;
                  };
                  uint16_t control_ = 0;
            };
      };

      class Definition final {
      public:
            constexpr Definition()
            : temp(Temp(0, s1)), reg_(0), isFixed_(0), isPrecolored_(0), isKill_(0), isPrecise_(0),
            isInfPreserve_(0), isNaNPreserve_(0), isSZPreserve_(0), isNUW_(0), isNoCSE_(0)
            {}
            explicit Definition(Temp tmp) noexcept : temp(tmp) {}
            explicit Definition(PhysReg reg, RegClass type) noexcept : temp(Temp(0, type)) { setFixed(reg); }
            explicit Definition(Temp tmp, PhysReg reg) noexcept : temp(tmp) { setPrecolored(reg); }

            constexpr bool isTemp() const noexcept { return tempId() > 0; }

            constexpr Temp getTemp() const noexcept { return temp; }

            constexpr uint32_t tempId() const noexcept { return temp.id(); }

            constexpr void setTemp(Temp t) noexcept { temp = t; }

            void swapTemp(Definition& other) noexcept { std::swap(temp, other.temp); }

            constexpr RegClass regClass() const noexcept { return temp.regClass(); }

            constexpr unsigned bytes() const noexcept { return temp.bytes(); }

            constexpr unsigned size() const noexcept { return temp.size(); }

            constexpr bool isFixed() const noexcept { return isFixed_; }

            constexpr PhysReg physReg() const noexcept { return reg_; }

            constexpr void setFixed(PhysReg reg) noexcept
            {
                  isFixed_ = 1;
                  reg_ = reg;
            }

            constexpr bool isPrecolored() const noexcept { return isPrecolored_; }
            constexpr void setPrecolored(PhysReg reg) noexcept
            {
                  setFixed(reg);
                  isPrecolored_ = isFixed_;
            }

            constexpr void setKill(bool flag) noexcept { isKill_ = flag; }

            constexpr bool isKill() const noexcept { return isKill_; }

            constexpr void setPrecise(bool precise) noexcept { isPrecise_ = precise; }

            constexpr bool isPrecise() const noexcept { return isPrecise_; }

            constexpr void setInfPreserve(bool inf_preserve) noexcept { isInfPreserve_ = inf_preserve; }

            constexpr bool isInfPreserve() const noexcept { return isInfPreserve_; }

            constexpr void setNaNPreserve(bool nan_preserve) noexcept { isNaNPreserve_ = nan_preserve; }

            constexpr bool isNaNPreserve() const noexcept { return isNaNPreserve_; }

            constexpr void setSZPreserve(bool sz_preserve) noexcept { isSZPreserve_ = sz_preserve; }

            constexpr bool isSZPreserve() const noexcept { return isSZPreserve_; }

            constexpr void setNUW(bool nuw) noexcept { isNUW_ = nuw; }

            constexpr bool isNUW() const noexcept { return isNUW_; }

            constexpr void setNoCSE(bool noCSE) noexcept { isNoCSE_ = noCSE; }

            constexpr bool isNoCSE() const noexcept { return isNoCSE_; }

      private:
            Temp temp = Temp(0, s1);
            PhysReg reg_;
            union {
                  struct {
                        uint8_t isFixed_ : 1;
                        uint8_t isPrecolored_ : 1;
                        uint8_t isKill_ : 1;
                        uint8_t isPrecise_ : 1;
                        uint8_t isInfPreserve_ : 1;
                        uint8_t isNaNPreserve_ : 1;
                        uint8_t isSZPreserve_ : 1;
                        uint8_t isNUW_ : 1;
                        uint8_t isNoCSE_ : 1;
                  };
                  uint16_t control_ = 0;
            };
      };

      struct RegisterDemand {
            constexpr RegisterDemand() = default;
            constexpr RegisterDemand(const int16_t v, const int16_t s) noexcept : vgpr{v}, sgpr{s} {}
            constexpr RegisterDemand(Temp t) noexcept
            {
                  if (t.regClass().type() == RegType::sgpr)
                        sgpr = t.size();
                  else
                        vgpr = t.size();
            }
            int16_t vgpr = 0;
            int16_t sgpr = 0;

            constexpr friend bool operator==(const RegisterDemand a, const RegisterDemand b) noexcept
            {
                  return a.vgpr == b.vgpr && a.sgpr == b.sgpr;
            }

            constexpr bool exceeds(const RegisterDemand other) const noexcept
            {
                  return vgpr > other.vgpr || sgpr > other.sgpr;
            }

            constexpr RegisterDemand operator+(const Temp t) const noexcept
            {
                  if (t.type() == RegType::sgpr)
                        return RegisterDemand(vgpr, sgpr + t.size());
                  else
                        return RegisterDemand(vgpr + t.size(), sgpr);
            }

            constexpr RegisterDemand operator+(const RegisterDemand other) const noexcept
            {
                  return RegisterDemand(vgpr + other.vgpr, sgpr + other.sgpr);
            }

            constexpr RegisterDemand operator-(const RegisterDemand other) const noexcept
            {
                  return RegisterDemand(vgpr - other.vgpr, sgpr - other.sgpr);
            }

            constexpr RegisterDemand& operator+=(const RegisterDemand other) noexcept
            {
                  vgpr += other.vgpr;
                  sgpr += other.sgpr;
                  return *this;
            }

            constexpr RegisterDemand& operator-=(const RegisterDemand other) noexcept
            {
                  vgpr -= other.vgpr;
                  sgpr -= other.sgpr;
                  return *this;
            }

            constexpr RegisterDemand& operator+=(const Temp t) noexcept
            {
                  if (t.type() == RegType::sgpr)
                        sgpr += t.size();
                  else
                        vgpr += t.size();
                  return *this;
            }

            constexpr RegisterDemand& operator-=(const Temp t) noexcept
            {
                  if (t.type() == RegType::sgpr)
                        sgpr -= t.size();
                  else
                        vgpr -= t.size();
                  return *this;
            }

            constexpr void update(const RegisterDemand other) noexcept
            {
                  vgpr = std::max(vgpr, other.vgpr);
                  sgpr = std::max(sgpr, other.sgpr);
            }
      };

      struct Block;
      struct Instruction;
      struct Pseudo_instruction;
      struct SALU_instruction;
      struct SMEM_instruction;
      struct DS_instruction;
      struct LDSDIR_instruction;
      struct MTBUF_instruction;
      struct MUBUF_instruction;
      struct MIMG_instruction;
      struct Export_instruction;
      struct FLAT_instruction;
      struct Pseudo_branch_instruction;
      struct Pseudo_barrier_instruction;
      struct Pseudo_reduction_instruction;
      struct VALU_instruction;
      struct VINTERP_inreg_instruction;
      struct VINTRP_instruction;
      struct VOPD_instruction;
      struct DPP16_instruction;
      struct DPP8_instruction;
      struct SDWA_instruction;

      struct Instruction {
            aco_opcode opcode;
            Format format;
            union {
                  uint32_t pass_flags;
                  RegisterDemand register_demand;
            };

            aco::span<Operand> operands;
            aco::span<Definition> definitions;

            constexpr bool usesModifiers() const noexcept;

            constexpr bool reads_exec() const noexcept
            {
                  for (const Operand& op : operands) {
                        if (op.isFixed() && (op.physReg() == exec_lo || op.physReg() == exec_hi))
                              return true;
                  }
                  return false;
            }

            constexpr bool writes_exec() const noexcept
            {
                  for (const Definition& def : definitions) {
                        if (def.isFixed() && (def.physReg() == exec_lo || def.physReg() == exec_hi))
                              return true;
                  }
                  return false;
            }

            Pseudo_instruction& pseudo() noexcept
            {
                  assert(isPseudo());
                  return *(Pseudo_instruction*)this;
            }
            const Pseudo_instruction& pseudo() const noexcept
            {
                  assert(isPseudo());
                  return *(Pseudo_instruction*)this;
            }
            constexpr bool isPseudo() const noexcept { return format == Format::PSEUDO; }

            constexpr bool isSOP1() const noexcept { return format == Format::SOP1; }
            constexpr bool isSOP2() const noexcept { return format == Format::SOP2; }
            constexpr bool isSOPK() const noexcept { return format == Format::SOPK; }
            constexpr bool isSOPP() const noexcept { return format == Format::SOPP; }
            constexpr bool isSOPC() const noexcept { return format == Format::SOPC; }

            SMEM_instruction& smem() noexcept
            {
                  assert(isSMEM());
                  return *(SMEM_instruction*)this;
            }
            const SMEM_instruction& smem() const noexcept
            {
                  assert(isSMEM());
                  return *(SMEM_instruction*)this;
            }
            constexpr bool isSMEM() const noexcept { return format == Format::SMEM; }
            DS_instruction& ds() noexcept
            {
                  assert(isDS());
                  return *(DS_instruction*)this;
            }
            const DS_instruction& ds() const noexcept
            {
                  assert(isDS());
                  return *(DS_instruction*)this;
            }
            constexpr bool isDS() const noexcept { return format == Format::DS; }
            LDSDIR_instruction& ldsdir() noexcept
            {
                  assert(isLDSDIR());
                  return *(LDSDIR_instruction*)this;
            }
            const LDSDIR_instruction& ldsdir() const noexcept
            {
                  assert(isLDSDIR());
                  return *(LDSDIR_instruction*)this;
            }
            constexpr bool isLDSDIR() const noexcept { return format == Format::LDSDIR; }
            MTBUF_instruction& mtbuf() noexcept
            {
                  assert(isMTBUF());
                  return *(MTBUF_instruction*)this;
            }
            const MTBUF_instruction& mtbuf() const noexcept
            {
                  assert(isMTBUF());
                  return *(MTBUF_instruction*)this;
            }
            constexpr bool isMTBUF() const noexcept { return format == Format::MTBUF; }
            MUBUF_instruction& mubuf() noexcept
            {
                  assert(isMUBUF());
                  return *(MUBUF_instruction*)this;
            }
            const MUBUF_instruction& mubuf() const noexcept
            {
                  assert(isMUBUF());
                  return *(MUBUF_instruction*)this;
            }
            constexpr bool isMUBUF() const noexcept { return format == Format::MUBUF; }
            MIMG_instruction& mimg() noexcept
            {
                  assert(isMIMG());
                  return *(MIMG_instruction*)this;
            }
            const MIMG_instruction& mimg() const noexcept
            {
                  assert(isMIMG());
                  return *(MIMG_instruction*)this;
            }
            constexpr bool isMIMG() const noexcept { return format == Format::MIMG; }
            Export_instruction& exp() noexcept
            {
                  assert(isEXP());
                  return *(Export_instruction*)this;
            }
            const Export_instruction& exp() const noexcept
            {
                  assert(isEXP());
                  return *(Export_instruction*)this;
            }
            constexpr bool isEXP() const noexcept { return format == Format::EXP; }
            FLAT_instruction& flat() noexcept
            {
                  assert(isFlat());
                  return *(FLAT_instruction*)this;
            }
            const FLAT_instruction& flat() const noexcept
            {
                  assert(isFlat());
                  return *(FLAT_instruction*)this;
            }
            constexpr bool isFlat() const noexcept { return format == Format::FLAT; }
            FLAT_instruction& global() noexcept
            {
                  assert(isGlobal());
                  return *(FLAT_instruction*)this;
            }
            const FLAT_instruction& global() const noexcept
            {
                  assert(isGlobal());
                  return *(FLAT_instruction*)this;
            }
            constexpr bool isGlobal() const noexcept { return format == Format::GLOBAL; }
            FLAT_instruction& scratch() noexcept
            {
                  assert(isScratch());
                  return *(FLAT_instruction*)this;
            }
            const FLAT_instruction& scratch() const noexcept
            {
                  assert(isScratch());
                  return *(FLAT_instruction*)this;
            }
            constexpr bool isScratch() const noexcept { return format == Format::SCRATCH; }
            Pseudo_branch_instruction& branch() noexcept
            {
                  assert(isBranch());
                  return *(Pseudo_branch_instruction*)this;
            }
            const Pseudo_branch_instruction& branch() const noexcept
            {
                  assert(isBranch());
                  return *(Pseudo_branch_instruction*)this;
            }
            constexpr bool isBranch() const noexcept { return format == Format::PSEUDO_BRANCH; }
            Pseudo_barrier_instruction& barrier() noexcept
            {
                  assert(isBarrier());
                  return *(Pseudo_barrier_instruction*)this;
            }
            const Pseudo_barrier_instruction& barrier() const noexcept
            {
                  assert(isBarrier());
                  return *(Pseudo_barrier_instruction*)this;
            }
            constexpr bool isBarrier() const noexcept { return format == Format::PSEUDO_BARRIER; }
            Pseudo_reduction_instruction& reduction() noexcept
            {
                  assert(isReduction());
                  return *(Pseudo_reduction_instruction*)this;
            }
            const Pseudo_reduction_instruction& reduction() const noexcept
            {
                  assert(isReduction());
                  return *(Pseudo_reduction_instruction*)this;
            }
            constexpr bool isReduction() const noexcept { return format == Format::PSEUDO_REDUCTION; }
            constexpr bool isVOP3P() const noexcept { return (uint16_t)format & (uint16_t)Format::VOP3P; }
            VINTERP_inreg_instruction& vinterp_inreg() noexcept
            {
                  assert(isVINTERP_INREG());
                  return *(VINTERP_inreg_instruction*)this;
            }
            const VINTERP_inreg_instruction& vinterp_inreg() const noexcept
            {
                  assert(isVINTERP_INREG());
                  return *(VINTERP_inreg_instruction*)this;
            }
            constexpr bool isVINTERP_INREG() const noexcept { return format == Format::VINTERP_INREG; }
            VOPD_instruction& vopd() noexcept
            {
                  assert(isVOPD());
                  return *(VOPD_instruction*)this;
            }
            const VOPD_instruction& vopd() const noexcept
            {
                  assert(isVOPD());
                  return *(VOPD_instruction*)this;
            }
            constexpr bool isVOPD() const noexcept { return format == Format::VOPD; }
            constexpr bool isVOP1() const noexcept { return (uint16_t)format & (uint16_t)Format::VOP1; }
            constexpr bool isVOP2() const noexcept { return (uint16_t)format & (uint16_t)Format::VOP2; }
            constexpr bool isVOPC() const noexcept { return (uint16_t)format & (uint16_t)Format::VOPC; }
            constexpr bool isVOP3() const noexcept { return (uint16_t)format & (uint16_t)Format::VOP3; }
            VINTRP_instruction& vintrp() noexcept
            {
                  assert(isVINTRP());
                  return *(VINTRP_instruction*)this;
            }
            const VINTRP_instruction& vintrp() const noexcept
            {
                  assert(isVINTRP());
                  return *(VINTRP_instruction*)this;
            }
            constexpr bool isVINTRP() const noexcept { return format == Format::VINTRP; }
            DPP16_instruction& dpp16() noexcept
            {
                  assert(isDPP16());
                  return *(DPP16_instruction*)this;
            }
            const DPP16_instruction& dpp16() const noexcept
            {
                  assert(isDPP16());
                  return *(DPP16_instruction*)this;
            }
            constexpr bool isDPP16() const noexcept { return (uint16_t)format & (uint16_t)Format::DPP16; }
            DPP8_instruction& dpp8() noexcept
            {
                  assert(isDPP8());
                  return *(DPP8_instruction*)this;
            }
            const DPP8_instruction& dpp8() const noexcept
            {
                  assert(isDPP8());
                  return *(DPP8_instruction*)this;
            }
            constexpr bool isDPP8() const noexcept { return (uint16_t)format & (uint16_t)Format::DPP8; } // Added this definition
            constexpr bool isDPP() const noexcept { return isDPP16() || isDPP8(); }
            SDWA_instruction& sdwa() noexcept
            {
                  assert(isSDWA());
                  return *(SDWA_instruction*)this;
            }
            const SDWA_instruction& sdwa() const noexcept
            {
                  assert(isSDWA());
                  return *(SDWA_instruction*)this;
            }
            constexpr bool isSDWA() const noexcept { return (uint16_t)format & (uint16_t)Format::SDWA; }

            FLAT_instruction& flatlike() { return *(FLAT_instruction*)this; }

            const FLAT_instruction& flatlike() const { return *(FLAT_instruction*)this; }

            constexpr bool isFlatLike() const noexcept { return isFlat() || isGlobal() || isScratch(); }

            VALU_instruction& valu() noexcept
            {
                  assert(isVALU());
                  return *(VALU_instruction*)this;
            }
            const VALU_instruction& valu() const noexcept
            {
                  assert(isVALU());
                  return *(VALU_instruction*)this;
            }
            constexpr bool isVALU() const noexcept
            {
                  return isVOP1() || isVOP2() || isVOPC() || isVOP3() || isVOP3P() || isVINTERP_INREG() ||
                  isVOPD();
            }

            SALU_instruction& salu() noexcept
            {
                  assert(isSALU());
                  return *(SALU_instruction*)this;
            }
            const SALU_instruction& salu() const noexcept
            {
                  assert(isSALU());
                  return *(SALU_instruction*)this;
            }
            constexpr bool isSALU() const noexcept
            {
                  return isSOP1() || isSOP2() || isSOPC() || isSOPK() || isSOPP();
            }

            constexpr bool isVMEM() const noexcept { return isMTBUF() || isMUBUF() || isMIMG(); }

            bool accessesLDS() const noexcept;
            bool isTrans() const noexcept;
      };
      static_assert(sizeof(Instruction) == 16, "Unexpected padding");

      struct SALU_instruction : public Instruction {
            uint32_t imm;
      };
      static_assert(sizeof(SALU_instruction) == sizeof(Instruction) + 4, "Unexpected padding");

      struct SMEM_instruction : public Instruction {
            memory_sync_info sync;
            ac_hw_cache_flags cache;
      };
      static_assert(sizeof(SMEM_instruction) == sizeof(Instruction) + 4, "Unexpected padding");

      struct VALU_instruction : public Instruction {
            union {
                  bitfield_array8<uint32_t, 0, 3> neg;
                  bitfield_array8<uint32_t, 0, 3> neg_lo;

                  bitfield_array8<uint32_t, 3, 3> abs;
                  bitfield_array8<uint32_t, 3, 3> neg_hi;

                  bitfield_array8<uint32_t, 6, 4> opsel;
                  bitfield_uint8<uint32_t, 10, 2> omod;
                  bitfield_array8<uint32_t, 12, 3> opsel_lo;
                  bitfield_array8<uint32_t, 15, 3> opsel_hi;
                  bitfield_bool<uint32_t, 18> clamp;
            };

            void swapOperands(unsigned idx0, unsigned idx1);
      };
      static_assert(sizeof(VALU_instruction) == sizeof(Instruction) + 4, "Unexpected padding");

      struct VINTERP_inreg_instruction : public VALU_instruction {
            uint8_t wait_exp : 3;
            uint8_t padding3 : 5;
            uint8_t padding4;
            uint8_t padding5;
            uint8_t padding6;
      };
      static_assert(sizeof(VINTERP_inreg_instruction) == sizeof(VALU_instruction) + 4,
                    "Unexpected padding");

      struct VOPD_instruction : public VALU_instruction {
            aco_opcode opy;
            uint16_t padding;
      };
      static_assert(sizeof(VOPD_instruction) == sizeof(VALU_instruction) + 4, "Unexpected padding");

      struct DPP16_instruction : public VALU_instruction {
            uint16_t dpp_ctrl;
            uint8_t row_mask : 4;
            uint8_t bank_mask : 4;
            bool bound_ctrl : 1;
            uint8_t fetch_inactive : 1;
            uint8_t padding3 : 6;
      };
      static_assert(sizeof(DPP16_instruction) == sizeof(VALU_instruction) + 4, "Unexpected padding");

      struct DPP8_instruction : public VALU_instruction {
            uint32_t lane_sel : 24;
            uint32_t fetch_inactive : 1;
            uint32_t padding : 7;
      };
      static_assert(sizeof(DPP8_instruction) == sizeof(VALU_instruction) + 4, "Unexpected padding");

      struct SubdwordSel {
            enum sdwa_sel : uint8_t {
                  ubyte = 0x4,
                  uword = 0x8,
                  dword = 0x10,
                  sext = 0x20,
                  sbyte = ubyte | sext,
                  sword = uword | sext,

                  ubyte0 = ubyte,
                  ubyte1 = ubyte | 1,
                  ubyte2 = ubyte | 2,
                  ubyte3 = ubyte | 3,
                  sbyte0 = sbyte,
                  sbyte1 = sbyte | 1,
                  sbyte2 = sbyte | 2,
                  sbyte3 = sbyte | 3,
                  uword0 = uword,
                  uword1 = uword | 2,
                  sword0 = sword,
                  sword1 = sword | 2,
            };

            SubdwordSel() : sel((sdwa_sel)0) {}
            constexpr SubdwordSel(sdwa_sel sel_) : sel(sel_) {}
            constexpr SubdwordSel(unsigned size, unsigned offset, bool sign_extend)
            : sel((sdwa_sel)((sign_extend ? sext : 0) | size << 2 | offset))
            {}
            constexpr operator sdwa_sel() const { return sel; }
            explicit operator bool() const { return sel != 0; }

            constexpr unsigned size() const { return (sel >> 2) & 0x7; }
            constexpr unsigned offset() const { return sel & 0x3; }
            constexpr bool sign_extend() const { return sel & sext; }
            constexpr unsigned to_sdwa_sel(unsigned reg_byte_offset) const
            {
                  reg_byte_offset += offset();
                  if (size() == 1)
                        return reg_byte_offset;
                  else if (size() == 2)
                        return 4 + (reg_byte_offset >> 1);
                  else
                        return 6;
            }

      private:
            sdwa_sel sel;
      };

      struct SDWA_instruction : public VALU_instruction {
            SubdwordSel sel[2];
            SubdwordSel dst_sel;
            uint8_t padding3;
      };
      static_assert(sizeof(SDWA_instruction) == sizeof(VALU_instruction) + 4, "Unexpected padding");

      struct VINTRP_instruction : public Instruction {
            uint8_t attribute;
            uint8_t component;
            bool high_16bits;
            uint8_t padding;
      };
      static_assert(sizeof(VINTRP_instruction) == sizeof(Instruction) + 4, "Unexpected padding");

      struct DS_instruction : public Instruction {
            memory_sync_info sync;
            bool gds;
            uint16_t offset0;
            uint8_t offset1;
            uint8_t padding;
      };
      static_assert(sizeof(DS_instruction) == sizeof(Instruction) + 8, "Unexpected padding");

      struct LDSDIR_instruction : public Instruction {
            memory_sync_info sync;
            uint8_t attr : 6;
            uint8_t attr_chan : 2;
            uint32_t wait_vdst : 4;
            uint32_t wait_vsrc : 1;
            uint32_t padding : 27;
      };
      static_assert(sizeof(LDSDIR_instruction) == sizeof(Instruction) + 8, "Unexpected padding");

      struct MUBUF_instruction : public Instruction {
            memory_sync_info sync;
            ac_hw_cache_flags cache;
            uint32_t offset : 23;
            uint32_t offen : 1;
            uint32_t idxen : 1;
            uint32_t addr64 : 1;
            uint32_t tfe : 1;
            uint32_t lds : 1;
            uint32_t disable_wqm : 1;
            uint32_t padding : 3;
      };
      static_assert(sizeof(MUBUF_instruction) == sizeof(Instruction) + 8, "Unexpected padding");

      struct MTBUF_instruction : public Instruction {
            memory_sync_info sync;
            ac_hw_cache_flags cache;
            uint8_t dfmt : 4;
            uint8_t nfmt : 3;
            bool offen : 1;
            bool idxen : 1;
            bool tfe : 1;
            bool disable_wqm : 1;
            uint8_t padding0 : 5;
            uint16_t padding1;
            uint32_t offset : 23;
            uint32_t padding2 : 9;
      };
      static_assert(sizeof(MTBUF_instruction) == sizeof(Instruction) + 12, "Unexpected padding");

      struct MIMG_instruction : public Instruction {
            memory_sync_info sync;
            ac_hw_cache_flags cache;
            uint8_t dmask;
            uint8_t dim : 3;
            bool unrm : 1;
            bool tfe : 1;
            bool da : 1;
            bool lwe : 1;
            bool r128 : 1;
            bool a16 : 1;
            bool d16 : 1;
            bool disable_wqm : 1;
            bool strict_wqm : 1;
            uint8_t padding0 : 4;
            uint8_t padding1;
      };
      static_assert(sizeof(MIMG_instruction) == sizeof(Instruction) + 8, "Unexpected padding");

      struct FLAT_instruction : public Instruction {
            memory_sync_info sync;
            ac_hw_cache_flags cache;
            int32_t offset : 24;
            uint32_t lds : 1;
            uint32_t nv : 1;
            uint32_t disable_wqm : 1;
            uint32_t padding0 : 5;
      };
      static_assert(sizeof(FLAT_instruction) == sizeof(Instruction) + 8, "Unexpected padding");

      struct Export_instruction : public Instruction {
            uint8_t enabled_mask;
            uint8_t dest;
            bool compressed : 1;
            bool done : 1;
            bool valid_mask : 1;
            bool row_en : 1;
            uint8_t padding0 : 4;
            uint8_t padding1;
      };
      static_assert(sizeof(Export_instruction) == sizeof(Instruction) + 4, "Unexpected padding");

      struct Pseudo_instruction : public Instruction {
            PhysReg scratch_sgpr;
            bool needs_scratch_reg;
      };
      static_assert(sizeof(Pseudo_instruction) == sizeof(Instruction) + 4, "Unexpected padding");

      struct Pseudo_branch_instruction : public Instruction {
            uint32_t target[2];
            bool rarely_taken;
            bool never_taken;
            uint16_t padding;
      };
      static_assert(sizeof(Pseudo_branch_instruction) == sizeof(Instruction) + 12, "Unexpected padding");

      struct Pseudo_barrier_instruction : public Instruction {
            memory_sync_info sync;
            sync_scope exec_scope;
      };
      static_assert(sizeof(Pseudo_barrier_instruction) == sizeof(Instruction) + 4, "Unexpected padding");

      enum ReduceOp : uint16_t {
            iadd8, iadd16, iadd32, iadd64,
            imul8, imul16, imul32, imul64,
            fadd16, fadd32, fadd64,
            fmul16, fmul32, fmul64,
            imin8, imin16, imin32, imin64,
            imax8, imax16, imax32, imax64,
            umin8, umin16, umin32, umin64,
            umax8, umax16, umax32, umax64,
            fmin16, fmin32, fmin64,
            fmax16, fmax32, fmax64,
            iand8, iand16, iand32, iand64,
            ior8, ior16, ior32, ior64,
            ixor8, ixor16, ixor32, ixor64,
            num_reduce_ops,
      };

      struct Pseudo_reduction_instruction : public Instruction {
            ReduceOp reduce_op;
            uint16_t cluster_size;
      };
      static_assert(sizeof(Pseudo_reduction_instruction) == sizeof(Instruction) + 4,
                    "Unexpected padding");

      inline bool
      Instruction::accessesLDS() const noexcept
      {
            return (isDS() && !ds().gds) || isLDSDIR() || isVINTRP();
      }

      inline void
      VALU_instruction::swapOperands(unsigned idx0, unsigned idx1)
      {
            if (this->isSDWA() && idx0 != idx1) {
                  assert(idx0 < 2 && idx1 < 2);
                  std::swap(this->sdwa().sel[0], this->sdwa().sel[1]);
            }
            assert(idx0 < 3 && idx1 < 3);
            std::swap(this->operands[idx0], this->operands[idx1]);
            this->neg[idx0].swap(this->neg[idx1]);
            this->abs[idx0].swap(this->abs[idx1]);
            this->opsel[idx0].swap(this->opsel[idx1]);
            this->opsel_lo[idx0].swap(this->opsel_lo[idx1]);
            this->opsel_hi[idx0].swap(this->opsel_hi[idx1]);
      }

      struct instr_deleter_functor {
            void operator()(void* p) { return; }
      };

      template <typename T> using aco_ptr = std::unique_ptr<T, instr_deleter_functor>;

      size_t get_instr_data_size(Format format);

      Instruction* create_instruction(aco_opcode opcode, Format format, uint32_t num_operands,
                                      uint32_t num_definitions);

      constexpr bool
      Instruction::usesModifiers() const noexcept
      {
            if (isDPP() || isSDWA())
                  return true;

            if (isVOP3P()) {
                  const VALU_instruction& vop3p = this->valu();
                  return vop3p.opsel_lo || vop3p.clamp || vop3p.neg_lo || vop3p.neg_hi ||
                  (vop3p.opsel_hi & BITFIELD_MASK(operands.size())) != BITFIELD_MASK(operands.size());
            } else if (isVALU()) {
                  const VALU_instruction& vop3 = this->valu();
                  return vop3.opsel || vop3.clamp || vop3.omod || vop3.abs || vop3.neg;
            }
            return false;
      }

      constexpr bool
      is_phi(Instruction* instr)
      {
            return instr->opcode == aco_opcode::p_phi || instr->opcode == aco_opcode::p_linear_phi;
      }

      static inline bool
      is_phi(aco_ptr<Instruction>& instr)
      {
            return is_phi(instr.get());
      }

      bool is_wait_export_ready(amd_gfx_level gfx_level, const Instruction* instr);
      memory_sync_info get_sync_info(const Instruction* instr);

      inline bool
      is_dead(const std::vector<uint16_t>& uses, const Instruction* instr)
      {
            if (instr->definitions.empty() || instr->isBranch() || instr->opcode == aco_opcode::p_startpgm ||
                  instr->opcode == aco_opcode::p_init_scratch ||
                  instr->opcode == aco_opcode::p_dual_src_export_gfx11)
                  return false;

            if (std::any_of(instr->definitions.begin(), instr->definitions.end(),
                  [&uses](const Definition& def) { return !def.isTemp() || uses[def.tempId()]; }))
                  return false;

            return !(get_sync_info(instr).semantics & (semantic_volatile | semantic_acqrel));
      }

      bool can_use_input_modifiers(amd_gfx_level gfx_level, aco_opcode op, int idx);
      bool can_use_opsel(amd_gfx_level gfx_level, aco_opcode op, int idx);
      bool instr_is_16bit(amd_gfx_level gfx_level, aco_opcode op);
      uint8_t get_gfx11_true16_mask(aco_opcode op);
      bool can_use_SDWA(amd_gfx_level gfx_level, const aco_ptr<Instruction>& instr, bool pre_ra);
      bool can_use_DPP(amd_gfx_level gfx_level, const aco_ptr<Instruction>& instr, bool dpp8);
      bool can_write_m0(const aco_ptr<Instruction>& instr);
      aco_ptr<Instruction> convert_to_SDWA(amd_gfx_level gfx_level, aco_ptr<Instruction>& instr);
      aco_ptr<Instruction> convert_to_DPP(amd_gfx_level gfx_level, aco_ptr<Instruction>& instr,
                                          bool dpp8);
      bool needs_exec_mask(const Instruction* instr);

      aco_opcode get_vcmp_inverse(aco_opcode op);
      aco_opcode get_vcmp_swapped(aco_opcode op);
      aco_opcode get_vcmpx(aco_opcode op);
      bool is_cmpx(aco_opcode op);

      aco_opcode get_swapped_opcode(aco_opcode opcode, unsigned idx0, unsigned idx1);
      bool can_swap_operands(aco_ptr<Instruction>& instr, aco_opcode* new_op, unsigned idx0 = 0,
                             unsigned idx1 = 1);

      uint32_t get_reduction_identity(ReduceOp op, unsigned idx);

      unsigned get_mimg_nsa_dwords(const Instruction* instr);

      unsigned get_vopd_opy_start(const Instruction* instr);

      bool should_form_clause(const Instruction* a, const Instruction* b);

      enum vmem_type : uint8_t {
            vmem_nosampler = 1 << 0,
            vmem_sampler = 1 << 1,
            vmem_bvh = 1 << 2,
      };

      uint8_t get_vmem_type(enum amd_gfx_level gfx_level, Instruction* instr);

      struct depctr_wait {
            union {
                  struct {
                        unsigned va_vdst : 4;
                        unsigned va_sdst : 3;
                        unsigned va_ssrc : 1;
                        unsigned hold_cnt : 1;
                        unsigned vm_vsrc : 3;
                        unsigned va_vcc : 1;
                        unsigned sa_sdst : 1;
                        unsigned va_exec : 1;
                        unsigned sa_exec : 1;
                  };
                  unsigned packed = -1;
            };
      };

      depctr_wait parse_depctr_wait(const Instruction* instr);

      enum block_kind {
            block_kind_uniform = 1 << 0,
            block_kind_top_level = 1 << 1,
            block_kind_loop_preheader = 1 << 2,
            block_kind_loop_header = 1 << 3,
            block_kind_loop_exit = 1 << 4,
            block_kind_continue = 1 << 5,
            block_kind_break = 1 << 6,
            block_kind_branch = 1 << 7,
            block_kind_merge = 1 << 8,
            block_kind_invert = 1 << 9,
            block_kind_discard_early_exit = 1 << 10,
            block_kind_uses_discard = 1 << 11,
            block_kind_resume = 1 << 12,
            block_kind_export_end = 1 << 13,
            block_kind_end_with_regs = 1 << 14,
      };

      struct Block {
            using edge_vec = small_vec<uint32_t, 2>;

            float_mode fp_mode;
            unsigned index;
            unsigned offset = 0;
            std::vector<aco_ptr<Instruction>> instructions;
            edge_vec logical_preds;
            edge_vec linear_preds;
            edge_vec logical_succs;
            edge_vec linear_succs;
            RegisterDemand register_demand = RegisterDemand();
            RegisterDemand live_in_demand = RegisterDemand();
            uint32_t kind = 0;
            int32_t logical_idom = -1;
            int32_t linear_idom = -1;

            uint32_t logical_dom_pre_index = 0;
            uint32_t logical_dom_post_index = 0;
            uint32_t linear_dom_pre_index = 0;
            uint32_t linear_dom_post_index = 0;

            uint16_t loop_nest_depth = 0;
            uint16_t divergent_if_logical_depth = 0;
            uint16_t uniform_if_depth = 0;

            Block() : index(0) {}
      };

      enum class SWStage : uint16_t {
            None = 0,
            VS = 1 << 0,
            GS = 1 << 1,
            TCS = 1 << 2,
            TES = 1 << 3,
            FS = 1 << 4,
            CS = 1 << 5,
            TS = 1 << 6,
            MS = 1 << 7,
            RT = 1 << 8,
            VS_GS = VS | GS,
            VS_TCS = VS | TCS,
            TES_GS = TES | GS,
      };

      constexpr SWStage
      operator|(SWStage a, SWStage b)
      {
            return static_cast<SWStage>(static_cast<uint16_t>(a) | static_cast<uint16_t>(b));
      }

      struct Stage {
            constexpr Stage() = default;

            explicit constexpr Stage(ac_hw_stage hw_, SWStage sw_) : sw(sw_), hw(hw_) {}

            constexpr bool has(SWStage stage) const
            {
                  return (static_cast<uint16_t>(sw) & static_cast<uint16_t>(stage));
            }

            unsigned num_sw_stages() const { return util_bitcount(static_cast<uint16_t>(sw)); }

            constexpr bool operator==(const Stage& other) const { return sw == other.sw && hw == other.hw; }

            constexpr bool operator!=(const Stage& other) const { return sw != other.sw || hw != other.hw; }

            SWStage sw = SWStage::None;
            ac_hw_stage hw{};
      };

      static constexpr Stage vertex_vs(AC_HW_VERTEX_SHADER, SWStage::VS);
      static constexpr Stage fragment_fs(AC_HW_PIXEL_SHADER, SWStage::FS);
      static constexpr Stage compute_cs(AC_HW_COMPUTE_SHADER, SWStage::CS);
      static constexpr Stage tess_eval_vs(AC_HW_VERTEX_SHADER, SWStage::TES);
      static constexpr Stage task_cs(AC_HW_COMPUTE_SHADER, SWStage::TS);
      static constexpr Stage mesh_ngg(AC_HW_NEXT_GEN_GEOMETRY_SHADER, SWStage::MS);
      static constexpr Stage vertex_ngg(AC_HW_NEXT_GEN_GEOMETRY_SHADER, SWStage::VS);
      static constexpr Stage vertex_geometry_ngg(AC_HW_NEXT_GEN_GEOMETRY_SHADER, SWStage::VS_GS);
      static constexpr Stage tess_eval_ngg(AC_HW_NEXT_GEN_GEOMETRY_SHADER, SWStage::TES);
      static constexpr Stage tess_eval_geometry_ngg(AC_HW_NEXT_GEN_GEOMETRY_SHADER, SWStage::TES_GS);
      static constexpr Stage vertex_geometry_gs(AC_HW_LEGACY_GEOMETRY_SHADER, SWStage::VS_GS);
      static constexpr Stage vertex_tess_control_hs(AC_HW_HULL_SHADER, SWStage::VS_TCS);
      static constexpr Stage tess_eval_geometry_gs(AC_HW_LEGACY_GEOMETRY_SHADER, SWStage::TES_GS);
      static constexpr Stage vertex_ls(AC_HW_LOCAL_SHADER,
                                       SWStage::VS);
      static constexpr Stage vertex_es(AC_HW_EXPORT_SHADER, SWStage::VS);
      static constexpr Stage tess_control_hs(AC_HW_HULL_SHADER, SWStage::TCS);
      static constexpr Stage tess_eval_es(AC_HW_EXPORT_SHADER,
                                          SWStage::TES);
      static constexpr Stage geometry_gs(AC_HW_LEGACY_GEOMETRY_SHADER, SWStage::GS);
      static constexpr Stage raytracing_cs(AC_HW_COMPUTE_SHADER, SWStage::RT);

      struct DeviceInfo {
            uint16_t lds_encoding_granule;
            uint16_t lds_alloc_granule;
            uint32_t lds_limit;
            bool has_16bank_lds;
            uint16_t physical_sgprs;
            uint16_t physical_vgprs;
            uint16_t vgpr_limit;
            uint16_t sgpr_limit;
            uint16_t sgpr_alloc_granule;
            uint16_t vgpr_alloc_granule;
            unsigned scratch_alloc_granule;
            uint16_t max_waves_per_simd;
            unsigned simd_per_cu;
            bool has_fast_fma32 = false;
            bool has_mac_legacy32 = false;
            bool has_fmac_legacy32 = false;
            bool fused_mad_mix = false;
            bool xnack_enabled = false;
            bool sram_ecc_enabled = false;

            int32_t scratch_global_offset_min;
            int32_t scratch_global_offset_max;
            unsigned max_nsa_vgprs;

            uint32_t buf_offset_max;
            uint32_t smem_offset_max;
      };

      enum class CompilationProgress {
            after_isel,
            after_spilling,
            after_ra,
            after_lower_to_hw,
      };

      class Program final {
      public:
            aco::monotonic_buffer_resource m{65536};
            std::vector<Block> blocks;
            std::vector<RegClass> temp_rc = {s1};
            RegisterDemand max_reg_demand = RegisterDemand();
            ac_shader_config* config;
            struct aco_shader_info info;
            enum amd_gfx_level gfx_level;
            enum radeon_family family;
            DeviceInfo dev;
            unsigned wave_size;
            RegClass lane_mask;
            Stage stage;
            bool needs_exact = false;
            bool needs_wqm = false;
            bool has_smem_buffer_or_global_loads = false;
            bool has_pops_overlapped_waves_wait = false;
            bool has_color_exports = false;
            bool is_prolog = false;
            bool is_epilog = false;

            std::vector<ac_shader_debug_info> debug_info;

            std::vector<uint8_t> constant_data;
            aco::small_vec<Temp, 2> private_segment_buffers;
            aco::small_vec<Temp, 2> scratch_offsets;

            uint16_t num_waves = 0;
            uint16_t min_waves = 0;
            unsigned workgroup_size;
            bool wgp_mode;

            bool needs_vcc = false;

            CompilationProgress progress;

            bool collect_statistics = false;
            uint32_t statistics[aco_num_statistics];

            float_mode next_fp_mode;
            unsigned next_loop_depth = 0;
            unsigned next_divergent_if_logical_depth = 0;
            unsigned next_uniform_if_depth = 0;

            std::vector<Definition> args_pending_vmem;

            bool pending_lds_access = false;

            bool should_repair_ssa = false;

            struct LiveInfo {
                  monotonic_buffer_resource memory;
                  std::vector<IDSet> live_in;
                  IDSet vcc_live_blocks;

                  LiveInfo() : vcc_live_blocks(memory) {}
                  LiveInfo(const LiveInfo& other) : memory(), live_in(other.live_in), vcc_live_blocks(other.vcc_live_blocks, memory) {}
                  LiveInfo& operator=(const LiveInfo& other) {
                        if (this != &other) {
                              memory.release();
                              new (&memory) monotonic_buffer_resource();
                              live_in = other.live_in;
                              vcc_live_blocks = IDSet(other.vcc_live_blocks, memory);
                        }
                        return *this;
                  }
                  LiveInfo(LiveInfo&& other) noexcept :
                  memory(std::move(other.memory)),
                  live_in(std::move(other.live_in)),
                  vcc_live_blocks(std::move(other.vcc_live_blocks), memory) // Re-parent IDSet
                  {
                        // Ensure other.vcc_live_blocks is in a valid state if it was moved from
                        new (&other.vcc_live_blocks) IDSet(other.memory);
                  }

                  LiveInfo& operator=(LiveInfo&& other) noexcept {
                        if (this != &other) {
                              memory = std::move(other.memory);
                              live_in = std::move(other.live_in);
                              vcc_live_blocks = IDSet(std::move(other.vcc_live_blocks), memory); // Re-parent IDSet
                              new (&other.vcc_live_blocks) IDSet(other.memory);
                        }
                        return *this;
                  }

            } live;


            struct {
                  FILE* output = stderr;
                  bool shorten_messages = false;
                  void (*func)(void* private_data, enum aco_compiler_debug_level level, const char* message);
                  void* private_data;
            } debug;

            Program() {
                  // Initialize IDSet with the program's live.memory resource
                  // This is needed because Program is now complex enough that the default constructor for LiveInfo won't be trivial.
                  new (&live.vcc_live_blocks) IDSet(live.memory);
            }


            void allocateRange(unsigned amount)
            {
                  assert(temp_rc.size() + amount <= 16777216);
                  temp_rc.resize(temp_rc.size() + amount);
            }

            Temp allocateTmp(RegClass rc) { return Temp(allocateId(rc), rc); }

            uint32_t peekAllocationId() { return temp_rc.size(); }

            Block* create_and_insert_block()
            {
                  Block block;
                  return insert_block(std::move(block));
            }

            Block* insert_block(Block&& block)
            {
                  block.index = blocks.size();
                  block.fp_mode = next_fp_mode;
                  block.loop_nest_depth = next_loop_depth;
                  block.divergent_if_logical_depth = next_divergent_if_logical_depth;
                  block.uniform_if_depth = next_uniform_if_depth;
                  blocks.emplace_back(std::move(block));
                  return &blocks.back();
            }

      private:
            uint32_t allocateId(RegClass rc)
            {
                  assert(temp_rc.size() <= 16777215);
                  temp_rc.push_back(rc);
                  return temp_rc.size() - 1;
            }
      };

      struct ra_test_policy {
            bool skip_optimistic_path = false;
            bool use_compact_relocate = false;
      };

      void init();

      void init_program(Program* program, Stage stage, const struct aco_shader_info* info,
                        enum amd_gfx_level gfx_level, enum radeon_family family, bool wgp_mode,
                        ac_shader_config* config);

      void select_program(Program* program, unsigned shader_count, struct nir_shader* const* shaders,
                          ac_shader_config* config, const struct aco_compiler_options* options,
                          const struct aco_shader_info* info, const struct ac_shader_args* args);
      void select_trap_handler_shader(Program* program, ac_shader_config* config,
                                      const struct aco_compiler_options* options,
                                      const struct aco_shader_info* info,
                                      const struct ac_shader_args* args);
      void select_rt_prolog(Program* program, ac_shader_config* config,
                            const struct aco_compiler_options* options,
                            const struct aco_shader_info* info, const struct ac_shader_args* in_args,
                            const struct ac_shader_args* out_args);
      void select_vs_prolog(Program* program, const struct aco_vs_prolog_info* pinfo,
                            ac_shader_config* config, const struct aco_compiler_options* options,
                            const struct aco_shader_info* info, const struct ac_shader_args* args);

      void select_ps_epilog(Program* program, void* pinfo, ac_shader_config* config,
                            const struct aco_compiler_options* options,
                            const struct aco_shader_info* info, const struct ac_shader_args* args);

      void select_ps_prolog(Program* program, void* pinfo, ac_shader_config* config,
                            const struct aco_compiler_options* options,
                            const struct aco_shader_info* info, const struct ac_shader_args* args);

      bool repair_ssa(Program* program);
      void lower_phis(Program* program);
      void lower_subdword(Program* program);
      void calc_min_waves(Program* program);
      void update_vgpr_sgpr_demand(Program* program, const RegisterDemand new_demand);
      void live_var_analysis(Program* program);
      std::vector<uint16_t> dead_code_analysis(Program* program);
      void dominator_tree(Program* program);
      void insert_exec_mask(Program* program);
      void value_numbering(Program* program);
      void optimize(Program* program);
      void optimize_postRA(Program* program);
      void lower_branches(Program* program);
      void setup_reduce_temp(Program* program);
      void lower_to_cssa(Program* program);
      void register_allocation(Program* program, ra_test_policy = {});
      void reindex_ssa(Program* program);
      void ssa_elimination(Program* program);
      void lower_to_hw_instr(Program* program);
      void schedule_program(Program* program);
      void schedule_ilp(Program* program);
      void schedule_vopd(Program* program);
      void spill(Program* program);
      void insert_waitcnt(Program* program);
      void insert_delay_alu(Program* program);
      void combine_delay_alu(Program* program);
      bool dealloc_vgprs(Program* program);
      void insert_NOPs(Program* program);
      void form_hard_clauses(Program* program);
      unsigned emit_program(Program* program, std::vector<uint32_t>& code,
                            std::vector<struct aco_symbol>* symbols = NULL, bool append_endpgm = true);
      bool check_print_asm_support(Program* program);
      bool print_asm(Program* program, std::vector<uint32_t>& binary, unsigned exec_size, FILE* output);
      bool validate_ir(Program* program);
      bool validate_cfg(Program* program);
      bool validate_ra(Program* program);
      bool validate_live_vars(Program* program);

      void collect_presched_stats(Program* program);
      void collect_preasm_stats(Program* program);
      void collect_postasm_stats(Program* program, const std::vector<uint32_t>& code);

      struct Instruction_cycle_info {
            unsigned latency;
            unsigned issue_cycles;
      };

      Instruction_cycle_info get_cycle_info(const Program& program, const Instruction& instr);

      enum print_flags {
            print_no_ssa = 0x1,
            print_perf_info = 0x2,
            print_kill = 0x4,
            print_live_vars = 0x8,
      };

      void aco_print_operand(const Operand* operand, FILE* output, unsigned flags = 0);
      void aco_print_instr(enum amd_gfx_level gfx_level, const Instruction* instr, FILE* output,
                           unsigned flags = 0);
      void aco_print_program(const Program* program, FILE* output, unsigned flags = 0);

      void _aco_err(Program* program, const char* file, unsigned line, const char* fmt, ...);

      #define aco_err(program, ...)      _aco_err(program, __FILE__, __LINE__, __VA_ARGS__)

      aco::small_vec<uint32_t, 2> get_tied_defs(Instruction* instr);

      RegisterDemand get_live_changes(Instruction* instr);
      RegisterDemand get_temp_registers(Instruction* instr);
      RegisterDemand get_temp_reg_changes(Instruction* instr);

      uint16_t max_suitable_waves(Program* program, uint16_t waves);

      uint16_t get_sgpr_alloc(Program* program, uint16_t addressable_sgprs);
      uint16_t get_vgpr_alloc(Program* program, uint16_t addressable_vgprs);

      RegisterDemand get_addr_regs_from_waves(Program* program, uint16_t waves);

      bool uses_scratch(Program* program);

      inline bool
      dominates_logical(const Block& parent, const Block& child)
      {
            return child.logical_dom_pre_index >= parent.logical_dom_pre_index &&
            child.logical_dom_post_index <= parent.logical_dom_post_index;
      }

      inline bool
      dominates_linear(const Block& parent, const Block& child)
      {
            return child.linear_dom_pre_index >= parent.linear_dom_pre_index &&
            child.linear_dom_post_index <= parent.linear_dom_post_index;
      }

      struct aco_type {
            aco_base_type base_type : 4;
            uint8_t num_components : 4;
            uint8_t bit_size;

            inline unsigned bytes() const { return (bit_size * num_components) / 8; }
            inline unsigned dwords() const { return DIV_ROUND_UP(bytes(), 4); }

            inline unsigned constant_bits() const
            {
                  switch (base_type) {
                        case aco_base_type_bfloat:
                        case aco_base_type_none:
                        case aco_base_type_lanemask: return 0;
                        case aco_base_type_float:
                              if (bit_size == 16 && (num_components == 1 || num_components == 2))
                                    return 16;
                        else if (bit_size == 32 && num_components == 1)
                              return 32;
                        else if (bit_size == 64 && num_components == 1)
                              return 64;
                        return 0;
                        case aco_base_type_uint:
                              if (bit_size == 16 && (num_components == 1 || num_components == 2))
                                    return 32;
                        else if (bit_size == 32 && num_components == 1)
                              return 32;
                        else if (bit_size == 64 && num_components == 1)
                              return 64;
                        return 0;
                        case aco_base_type_int: assert(bit_size == 64 && num_components == 1); return 64;
                  }
                  return 0;
            }
      };

      aco_type get_operand_type(aco_ptr<Instruction>& alu, unsigned index);

      struct aco_alu_opcode_info {
            uint8_t num_operands : 3;
            uint8_t num_defs : 2;
            uint8_t input_modifiers : 3;
            uint8_t output_modifiers : 1;
            aco_type op_types[4];
            aco_type def_types[3];
            fixed_reg op_fixed_reg[4];
            fixed_reg def_fixed_reg[3];
      };

      typedef struct {
            const int16_t opcode_gfx7[static_cast<int>(aco_opcode::num_opcodes)];
            const int16_t opcode_gfx9[static_cast<int>(aco_opcode::num_opcodes)];
            const int16_t opcode_gfx10[static_cast<int>(aco_opcode::num_opcodes)];
            const int16_t opcode_gfx11[static_cast<int>(aco_opcode::num_opcodes)];
            const int16_t opcode_gfx12[static_cast<int>(aco_opcode::num_opcodes)];
            const std::bitset<static_cast<int>(aco_opcode::num_opcodes)> is_atomic;
            const char* name[static_cast<int>(aco_opcode::num_opcodes)];
            const aco::Format format[static_cast<int>(aco_opcode::num_opcodes)];
            const instr_class classes[static_cast<int>(aco_opcode::num_opcodes)];
            const aco_alu_opcode_info alu_opcode_infos[static_cast<int>(aco_opcode::num_opcodes)];
      } Info;

      extern const Info instr_info;

} // namespace aco

#endif /* ACO_IR_H */
