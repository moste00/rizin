#ifndef RISCV_IL_BASE_H
#define RISCV_IL_BASE_H

#include "riscv.h"
#include "rz_types.h"
#include <rz_analysis.h>

#include "riscv_il.h"

#include <rz_il/rz_il_opbuilder_begin.h>

static const char *riscv_register_names[] = {
	[RISCV_REG_X1] = "ra",
	[RISCV_REG_X2] = "sp",
	[RISCV_REG_X3] = "gp",
	[RISCV_REG_X4] = "tp",
	[RISCV_REG_X5] = "t0",
	[RISCV_REG_X6] = "t1",
	[RISCV_REG_X7] = "t2",
	[RISCV_REG_X8] = "s0",
	[RISCV_REG_X9] = "s1",
	[RISCV_REG_X10] = "a0",
	[RISCV_REG_X11] = "a1",
	[RISCV_REG_X12] = "a2",
	[RISCV_REG_X13] = "a3",
	[RISCV_REG_X14] = "a4",
	[RISCV_REG_X15] = "a5",
	[RISCV_REG_X16] = "a6",
	[RISCV_REG_X17] = "a7",
	[RISCV_REG_X18] = "s2",
	[RISCV_REG_X19] = "s3",
	[RISCV_REG_X20] = "s4",
	[RISCV_REG_X21] = "s5",
	[RISCV_REG_X22] = "s6",
	[RISCV_REG_X23] = "s7",
	[RISCV_REG_X24] = "s8",
	[RISCV_REG_X25] = "s9",
	[RISCV_REG_X26] = "s10",
	[RISCV_REG_X27] = "s11",
	[RISCV_REG_X28] = "t3",
	[RISCV_REG_X29] = "t4",
	[RISCV_REG_X30] = "t5",
	[RISCV_REG_X31] = "t6",
};

#define RISCV_GET_REG(reg)    (((reg) != RISCV_REG_X0) ? (VARG(riscv_register_names[reg])) : UN(analysis->bits, 0))
#define RISCV_SET_REG(reg, r) (((reg) != RISCV_REG_X0) ? (SETG(riscv_register_names[reg], r)) : ((rz_il_op_pure_free(r), NOP())))

#define DEFINE_LIFTER(name, decoder, result) \
	static RzILOpEffect *rz_riscv_lift_##name(RZ_BORROW RZ_NONNULL RzAnalysis *analysis, \
		RZ_NONNULL RzAnalysisOp *op, RZ_NONNULL cs_insn *insn, ut64 current_addr, int size) { \
		decoder(analysis, insn); \
		return RISCV_SET_REG(rd, result); \
	}

// rd = result, then post_effect (e.g. jump: set return address, then redirect PC)
#define DEFINE_LIFTER_WITH_POST_EFFECT(name, decoder, result, post_effect) \
	static RzILOpEffect *rz_riscv_lift_##name(RZ_BORROW RZ_NONNULL RzAnalysis *analysis, \
		RZ_NONNULL RzAnalysisOp *op, RZ_NONNULL cs_insn *insn, ut64 current_addr, int size) { \
		decoder(analysis, insn); \
		return SEQ2( \
			RISCV_SET_REG(rd, result), \
			post_effect); \
	}

// pre_effect, then rd = result (e.g. store-conditional: store first, then write success flag)
#define DEFINE_LIFTER_WITH_PRE_EFFECT(name, decoder, pre_effect, result) \
	static RzILOpEffect *rz_riscv_lift_##name(RZ_BORROW RZ_NONNULL RzAnalysis *analysis, \
		RZ_NONNULL RzAnalysisOp *op, RZ_NONNULL cs_insn *insn, ut64 current_addr, int size) { \
		decoder(analysis, insn); \
		return SEQ2( \
			pre_effect, \
			RISCV_SET_REG(rd, result)); \
	}

#define DEFINE_LIFTER_WITH_EFFECT(name, decoder, effect) \
	static RzILOpEffect *rz_riscv_lift_##name(RZ_BORROW RZ_NONNULL RzAnalysis *analysis, \
		RZ_NONNULL RzAnalysisOp *op, RZ_NONNULL cs_insn *insn, ut64 current_addr, int size) { \
		decoder(analysis, insn); \
		return effect; \
	}

// by default, a RISC-V jump both sets a destination and sets the PC (i.e., jumps)
#define DEFINE_LIFTER_FOR_JUMP         DEFINE_LIFTER_WITH_POST_EFFECT
// oneway jumps are those that don't have a destination register
#define DEFINE_LIFTER_FOR_ONEWAY_JUMP  DEFINE_LIFTER_WITH_EFFECT

#define DEFINE_ALIAS_LIFTER(alias, name) static const RiscvInstructionLifter rz_riscv_lift_##alias = rz_riscv_lift_##name;

#define TWICE_FOR(name1, name2, def_lifter, ...) \
	def_lifter(name1, __VA_ARGS__) \
		def_lifter(name2, __VA_ARGS__)

#define THRICE_FOR(name1, name2, name3, def_lifter, ...) \
	TWICE_FOR(name1, name2, def_lifter, __VA_ARGS__) \
	def_lifter(name3, __VA_ARGS__)

#define FOR_4(name1, name2, name3, name4, def_lifter, ...) \
	TWICE_FOR(name1, name2, def_lifter, __VA_ARGS__) \
	TWICE_FOR(name3, name4, def_lifter, __VA_ARGS__)

#if RZ_CHECKS_LEVEL > 0
#define REQUIRE_OP(idx, t) \
	if (insn->detail->riscv.operands[idx].type != (t) || insn->detail->riscv.operands[idx].type == RISCV_OP_INVALID) { \
		RZ_LOG_ERROR("[%s (%d) @ 0x%08x] Expected type %d (%s) at index %d, found type %d instead\n", insn->mnemonic, insn->id, current_addr, t, #t, idx, insn->detail->riscv.operands[idx].type); \
		RZ_LOG_ERROR("op_str: %s\n", insn->op_str); \
		RZ_LOG_ERROR("need_effective_addr: %d\n", insn->detail->riscv.need_effective_addr); \
		RZ_LOG_ERROR("op_count: %u\n", insn->detail->riscv.op_count); \
		for (int _i = 0; _i < insn->detail->riscv.op_count; _i++) { \
			RZ_LOG_ERROR("operands[%d].type: %d\n", _i, insn->detail->riscv.operands[_i].type); \
			if (insn->detail->riscv.operands[_i].type == RISCV_OP_REG) { \
				RZ_LOG_ERROR("  REG = %d\n", insn->detail->riscv.operands[_i].reg); \
			} else if (insn->detail->riscv.operands[_i].type == RISCV_OP_IMM) { \
				RZ_LOG_ERROR("  IMM = 0x%" PFMT64x "\n", (ut64)insn->detail->riscv.operands[_i].imm); \
			} else if (insn->detail->riscv.operands[_i].type == RISCV_OP_MEM) { \
				RZ_LOG_ERROR("  MEM base = %d, disp = 0x%" PFMT64x "\n", insn->detail->riscv.operands[_i].mem.base, (ut64)insn->detail->riscv.operands[_i].mem.disp); \
			} \
		} \
		exit(-1); \
	}

#define REQUIRE_64_BIT(analysis) \
	if (analysis->bits != 64) { \
		RZ_LOG_ERROR("[%s (%d)] Expected 64-bit analysis, found %d bits\n", insn->mnemonic, insn->id, analysis->bits); \
		exit(-1); \
	}
#else
#define REQUIRE_OP(idx, t) \
	do { \
	} while (0)
#define REQUIRE_64_BIT(analysis) \
	do { \
	} while (0)
#endif

// Decoders, every instruction defines how its own transformation of capstone operands to IL operands

#define DECODE_RD_RS_IMM(analysis, insn) \
	REQUIRE_OP(0, RISCV_OP_REG); \
	REQUIRE_OP(1, RISCV_OP_REG); \
	REQUIRE_OP(2, RISCV_OP_IMM); \
	uint32_t rd = insn->detail->riscv.operands[0].reg; \
	RzILOpBitVector *rs = RISCV_GET_REG(insn->detail->riscv.operands[1].reg); \
	RzILOpBitVector *imm = SN(analysis->bits, insn->detail->riscv.operands[2].imm);

#define DECODE_RD_RS_RS(analysis, insn) \
	REQUIRE_OP(0, RISCV_OP_REG); \
	REQUIRE_OP(1, RISCV_OP_REG); \
	REQUIRE_OP(2, RISCV_OP_REG); \
	uint32_t rd = insn->detail->riscv.operands[0].reg; \
	RzILOpBitVector *rs1 = RISCV_GET_REG(insn->detail->riscv.operands[1].reg); \
	RzILOpBitVector *rs2 = RISCV_GET_REG(insn->detail->riscv.operands[2].reg);

#define DECODE_RS_IMM(analysis, insn) \
	REQUIRE_OP(0, RISCV_OP_REG); \
	REQUIRE_OP(1, RISCV_OP_IMM); \
	RzILOpBitVector *rs = RISCV_GET_REG(insn->detail->riscv.operands[0].reg); \
	RzILOpBitVector *imm = SN(analysis->bits, insn->detail->riscv.operands[1].imm);

#define DECODE_RS_RS_IMM(analysis, insn) \
	REQUIRE_OP(0, RISCV_OP_REG); \
	REQUIRE_OP(1, RISCV_OP_REG); \
	REQUIRE_OP(2, RISCV_OP_IMM); \
	RzILOpBitVector *rs1 = RISCV_GET_REG(insn->detail->riscv.operands[0].reg); \
	RzILOpBitVector *rs2 = RISCV_GET_REG(insn->detail->riscv.operands[1].reg); \
	RzILOpBitVector *imm = SN(analysis->bits, insn->detail->riscv.operands[2].imm);

#define DECODE_RD_RS(analysis, insn) \
	REQUIRE_OP(0, RISCV_OP_REG); \
	REQUIRE_OP(1, RISCV_OP_REG); \
	uint32_t rd = insn->detail->riscv.operands[0].reg; \
	RzILOpBitVector *rs = RISCV_GET_REG(insn->detail->riscv.operands[1].reg);

#define DECODE_IMM(analysis, insn) \
	REQUIRE_OP(0, RISCV_OP_IMM); \
	RzILOpBitVector *imm = SN(analysis->bits, insn->detail->riscv.operands[0].imm);

#define DECODE_RD_IMM(analysis, insn) \
	REQUIRE_OP(0, RISCV_OP_REG); \
	REQUIRE_OP(1, RISCV_OP_IMM); \
	uint32_t rd = insn->detail->riscv.operands[0].reg; \
	RzILOpBitVector *imm = SN(analysis->bits, insn->detail->riscv.operands[1].imm);

#define DECODE_RS_RS_IMM_MEM(analysis, insn) \
	REQUIRE_OP(0, RISCV_OP_REG); \
	REQUIRE_OP(1, RISCV_OP_MEM); \
	RzILOpBitVector *rs1 = RISCV_GET_REG(insn->detail->riscv.operands[0].reg); \
	RzILOpBitVector *rs2 = RISCV_GET_REG(insn->detail->riscv.operands[1].mem.base); \
	RzILOpBitVector *imm = SN(analysis->bits, insn->detail->riscv.operands[1].mem.disp);

#define DECODE_RD_RS_IMM_MEM(analysis, insn) \
	REQUIRE_OP(0, RISCV_OP_REG); \
	REQUIRE_OP(1, RISCV_OP_MEM); \
	uint32_t rd = insn->detail->riscv.operands[0].reg; \
	RzILOpBitVector *rs = RISCV_GET_REG(insn->detail->riscv.operands[1].mem.base); \
	RzILOpBitVector *imm = SN(analysis->bits, insn->detail->riscv.operands[1].mem.disp);

// used for *w instructions in RV64 that truncate the operands to 32 bits then does the operation
#define DECODE_RD_RS_RS_TRUNCATE32(analysis, insn) \
	REQUIRE_64_BIT(analysis); \
	REQUIRE_OP(0, RISCV_OP_REG); \
	REQUIRE_OP(1, RISCV_OP_REG); \
	REQUIRE_OP(2, RISCV_OP_REG); \
	uint32_t rd = insn->detail->riscv.operands[0].reg; \
	RzILOpBitVector *rs1 = CAST(32, IL_FALSE, RISCV_GET_REG(insn->detail->riscv.operands[1].reg)); \
	RzILOpBitVector *rs2 = CAST(32, IL_FALSE, RISCV_GET_REG(insn->detail->riscv.operands[2].reg));

#define DECODE_RD_RS_IMM_TRUNCATE32(analysis, insn) \
	REQUIRE_64_BIT(analysis); \
	REQUIRE_OP(0, RISCV_OP_REG); \
	REQUIRE_OP(1, RISCV_OP_REG); \
	REQUIRE_OP(2, RISCV_OP_IMM); \
	uint32_t rd = insn->detail->riscv.operands[0].reg; \
	RzILOpBitVector *rs = CAST(32, IL_FALSE, RISCV_GET_REG(insn->detail->riscv.operands[1].reg)); \
	RzILOpBitVector *imm = SN(32, insn->detail->riscv.operands[2].imm);

#define DECODE_RD_IMM_TRUNCATE32(analysis, insn) \
	REQUIRE_64_BIT(analysis); \
	REQUIRE_OP(0, RISCV_OP_REG); \
	REQUIRE_OP(1, RISCV_OP_IMM); \
	uint32_t rd = insn->detail->riscv.operands[0].reg; \
	RzILOpBitVector *imm = SN(32, insn->detail->riscv.operands[1].imm);

#define DECODE_NONE(analysis, insn) \
	(void)analysis; \
	(void)insn;

#define USE_LIFTER(name, uppername) [RISCV_INS_##uppername] = rz_riscv_lift_##name

#include <rz_il/rz_il_opbuilder_end.h>

#endif // RISCV_IL_BASE_H