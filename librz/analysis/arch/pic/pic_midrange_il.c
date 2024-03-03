// SPDX-FileCopyrightText: 2023 Siddharth Mishra <admin@brightprogrammer.in>
// SPDX-License-Identifier: LGPL-3.0-only

#include <string.h>
#include <stdlib.h>

#include "pic_il.h"
#include "../../asm/arch/pic/pic_midrange.h"

#include <rz_il/rz_il_opbuilder_begin.h>

// HELPER DEFINES & TYPEDEFS

typedef RzILOpEffect *(*FN_PicILUplifter)(PicMidrangeCPUState *, ut16);
#define IL_LIFTER(op)      pic_midrange_##op##_il_lifter
#define IL_LIFTER_IMPL(op) static RzILOpEffect *pic_midrange_##op##_il_lifter( \
	RZ_NONNULL PicMidrangeCPUState *cpu_state, ut16 instr)

// REGISTER DECLARATIONS & DEFINITIONS
#include "pic16f_memmaps/memmaps.h"
#define GET_REG_NAME(reg_type) pic_midrange_regname(reg_type)
#define GET_WREG()             VARG("wreg")
#define GET_FREG(idx)          VARG("freg" #idx)
// idx is kept with name in order to differentiate between same registers of different banks
#define GET_SPREG(name, bank) VARG(name)
#define BANK_SIZE             ((ut32)0x80)
#define BANK_COMMON_MAP_LOW   cpu_state->selected_bank *BANK_SIZE + 0X70
#define BANK_COMMON_MAP_HIGH  cpu_state->selected_bank *BANK_SIZE + 0X7F

// fields inside status register
const char *pic_midrange_status_flags[] = {
	"IRP", "RP1", "RP0", "TO", "PD", "Z", "DC", "C"
};

#define IRP       7
#define RP1       6
#define RP0       5
#define TO        4
#define PD        3
#define Z         2
#define DC        1
#define C         0
#define STATUS(x) pic_midrange_status_flags[x]

// device to register schema map
PicMidrangeRegType *pic_midrange_device_reg_map[] = {
	[PIC16F882] = pic16f882_reg_map,
	[PIC16F883] = pic16f883_reg_map,
	//	[PIC16F884] = pic16f884_reg_map,
	[PIC16F886] = pic16f886_reg_map,
	//	[PIC16F887] = pic16f887_reg_map,
};

/**
 * Get PicMidrangeRegType corresponding to give register index
 *
 * \param cpu_state Device CPU state.
 * \param reg Register index inside given bank of given device type.
 *
 * \return PicMidrangeRegType
 * */
PicMidrangeRegType pic_midrange_il_get_reg_type(RZ_NONNULL PicMidrangeCPUState *cpu_state, ut8 reg) {
	rz_return_val_if_fail(cpu_state, REG_INVALID);

	// compute linear register address
	ut32 addr = cpu_state->selected_bank * BANK_SIZE + reg;
	return pic_midrange_device_reg_map[cpu_state->device_type][addr];
}

/**
 * Get RzILOpPure corresponding to given register index of given device type.
 *
 * \param cpu_state Device CPU state.
 * \paramm regidx Register index in given memory bank of given device type.
 *
 * \return valid RzILOpPure on success, NULL otherwise.
 * */
RzILOpPure *pic_midrange_il_get_reg(RZ_NONNULL PicMidrangeCPUState *cpu_state, ut8 regidx) {
	rz_return_val_if_fail(cpu_state, NULL);

	// compute linear address
	ut32 addr = cpu_state->selected_bank * BANK_SIZE + regidx;
	PicMidrangeRegType reg_type = pic_midrange_device_reg_map[cpu_state->device_type][addr];

	// return register for given type
	if (reg_type == REG_FREG) {
		// last 16 registers in higher banks are mapped to bank 0
		if (addr >= BANK_COMMON_MAP_LOW && addr <= BANK_COMMON_MAP_HIGH) {
			addr = regidx;
		}
		return GET_FREG(addr);
	} else if (reg_type == REG_UNIMPLEMENTED || reg_type == REG_RESERVED) {
		return GET_SPREG(GET_REG_NAME(reg_type), cpu_state->selected_bank);
	} else {
		// for other special registers we need to append their bank index at the end
		// in order to avoid confusions
		// IDK any better way to do this atm.
		// allocating 4 extra bytes at the end to make sure devices with 128 and more
		// # of banks will also be supported.
		size_t regnamesz = strlen(GET_REG_NAME(reg_type)) + 4;
		const char *regname = malloc(regnamesz);
		memset(regname, 0, regnamesz);
		strcpy(regname, GET_REG_NAME(reg_type));
		char *bankstr = rz_num_as_string(NULL, cpu_state->selected_bank, true);
		if (!bankstr) {
			RZ_LOG_ERROR("RzIL : FAILED TO CONVERT NUMBER TO STRING");
			return NULL;
		}
		strcat(regname, bankstr);
		free(bankstr);
	}
}

// use only in IL_LIFTER_IMPL functions
#define GET_REG_TYPE(idx) pic_midrange_il_get_reg_type(cpu_state, idx)
#define GET_REG(idx)      pic_midrange_il_get_reg(cpu_state, idx);

#define GET_REG_7F(varname) \
	RzILOpPure *varname = NULL; \
	do { \
		ut8 regidx = PIC_MIDRANGE_OP_ARGS_7F_GET_F(instr); \
		ut8 addr = cpu_state->selected_bank * BANK_SIZE + regidx; \
		PicMidrangeRegType reg_type = GET_REG_TYPE(addr); \
		if (reg_type == REG_UNIMPLEMENTED || reg_type == REG_RESERVED) { \
			freg = U8(0); \
		} else { \
			freg = GET_REG(regidx); \
		} \
	} while (0)

#define BITN(x, n) IS_ZERO(LOGAND(SHIFTR0(x, U32(n)), U32(1)))
// overflow is not used in status register but just keeping this for future "maybe" use
#define CHECK_OVERFLOW(x, y, res)     AND(XOR(MSB(x), MSB(res)), XOR(MSB(y), MSB(res)))
#define CHECK_CARRY(x, y, res)        OR(AND(MSB(x), MSB(y)), AND(OR(MSB(x), MSB(y)), INV(MSB(res))))
#define CHECK_BORROW(x, y, res)       OR(OR(AND(INV(MSB(x)), MSB(y)), AND(INV(MSB(x)), MSB(res))), AND(MSB(x), AND(MSB(y), MSB(res))))
#define CHECK_DIGIT_CARRY(x, y, res)  OR(AND(BITN(x, 3), BITN(y, 3)), AND(OR(BITN(x, 3), BITN(y, 3)), INV(BITN(res, 3))))
#define CHECK_DIGIT_BORROW(x, y, res) OR( \
	OR(AND(INV(BITN(x, 3)), BITN(y, 3)), AND(INV(BITN(x, 3)), BITN(res, 3))), \
	AND(BITN(x, 3), AND(BITN(y, 3), BITN(res, 3))))

/**
 * Handle C, DC & Z flags for the previous operation.
 * To be used after an arithmetic operation.
 * Order of operands must be preserved for subtraction
 * operations, i.e `add = false`
 *
 * \param x First operand
 * \param y Second operand
 * \param res Result of last performed operation that affected the flag.
 * \param add Was this an add operation?
 *
 * \return \c RzILOpEffect containing set of steps to set status flags.
 * */
RzILOpEffect *pic_midrange_il_set_arithmetic_flags(RZ_BORROW RzILOpPure *x, RZ_BORROW RzILOpPure *y, RZ_BORROW RzILOpPure *res, bool add) {
	// get carry flag
	RzILOpBool *cf = NULL;
	RzILOpBool *dcf = NULL;
	if (add) {
		cf = CHECK_CARRY(x, y, res);
		dcf = CHECK_DIGIT_CARRY(x, y, res);
	} else { // sub
		cf = CHECK_BORROW(x, y, res);
		dcf = CHECK_DIGIT_BORROW(x, y, res);
	}

	// get zero flag
	RzILOpBool *zf = IS_ZERO(res);

	return SEQ3(SETG(STATUS(C), cf),
		SETG(STATUS(DC), dcf),
		SETG(STATUS(Z), zf));
}

#define SET_STATUS_ADD(x, y, r) pic_midrange_il_set_arithmetic_flags(x, y, r, true)
#define SET_STATUS_SUB(x, y, r) pic_midrange_il_set_arithmetic_flags(x, y, r, false)

/**
 * NOP
 * Operation: No Operation.
 * Operands: NONE
 * Status affected : NONE
 * */
IL_LIFTER_IMPL(NOP) {
	return NOP();
}

IL_LIFTER_IMPL(RETURN) {}
IL_LIFTER_IMPL(RETFIE) {}
IL_LIFTER_IMPL(OPTION) {}
IL_LIFTER_IMPL(SLEEP) {}
IL_LIFTER_IMPL(CLRWDT) {}
IL_LIFTER_IMPL(TRIS) {}
IL_LIFTER_IMPL(MOVWF) {}
IL_LIFTER_IMPL(CLR) {}

/**
 * SUBWF
 * Operation: Subtract freg from wreg.
 * Operands: f, d
 * Status affected : C, DC, Z
 * */
IL_LIFTER_IMPL(SUBWF) {
	GET_REG_7F(freg);
	RzILOpPure *wreg = GET_WREG();

	// if d bit is enabled then result will go in freg else wreg
	// TODO:
	bool reg_dest = PIC_MIDRANGE_OP_ARGS_7F_GET_F(instr);
	RzILOpPure *dest = reg_dest ? freg : wreg;

	// create a copy of current value of wreg because it's going to change
	RzILOpEffect *wreg_old = SETL("wreg_old", wreg);
	RzILOpEffect *sub_op = SETG(dest, SUB(wreg, freg));
	RzILOpEffect *set_status_op = SET_STATUS_SUB(VARL("wreg_old"), freg, wreg);
	return SEQ3(wreg_old, sub_op, set_status_op);
}

IL_LIFTER_IMPL(DECF) {}
IL_LIFTER_IMPL(IORWF) {}

/**
 * ANDWF
 * Operation: Take logical AND of freg and wreg.
 * Operands: f, d
 * Status affected : Z
 * */
IL_LIFTER_IMPL(ANDWF) {
	GET_REG_7F(freg);

	// if d bit is enabled then result will go in freg else wreg
	// TODO:
	bool reg_dest = PIC_MIDRANGE_OP_ARGS_7F_GET_F(instr);
	RzILOpPure *dest = reg_dest ? freg : GET_WREG();

	// create a copy of current value of wreg because it's going to change
	RzILOpEffect *and_op = SETG(dest, LOGAND(GET_WREG(), freg));
	RzILOpEffect *set_status_op = IS_ZERO(GET_WREG());
	return SEQ2(and_op, set_status_op);
}

/**
 * ANDWF
 * Operation: Take logical AND of freg and wreg.
 * Operands: f, d
 * Status affected : Z
 * */
IL_LIFTER_IMPL(XORWF) {
	GET_REG_7F(freg);
	RzILOpPure *wreg = GET_WREG();

	// if d bit is enabled then result will go in freg else wreg
	// TODO:
	bool reg_dest = PIC_MIDRANGE_OP_ARGS_7F_GET_F(instr);
	RzILOpPure *dest = reg_dest ? freg : wreg;

	// create a copy of current value of wreg because it's going to change
	RzILOpEffect *and_op = SETG(dest, LOGAND(wreg, freg));
	RzILOpEffect *set_status_op = IS_ZERO(wreg);
	return SEQ2(and_op, set_status_op);
}

/**
 * ADDWF
 * Operation: Add freg to wreg.
 * Operands: f, d
 * Status affected : C, DC, Z
 * */
IL_LIFTER_IMPL(ADDWF) {
	GET_REG_7F(freg);
	RzILOpPure *wreg = GET_WREG();

	// if d bit is enabled then result will go in freg else wreg
	// TODO:
	bool reg_dest = PIC_MIDRANGE_OP_ARGS_7F_GET_F(instr);
	RzILOpPure *dest = reg_dest ? freg : wreg;

	// create a copy of current value of wreg because it's going to change
	RzILOpEffect *wreg_old = SETL("wreg_old", wreg);
	RzILOpEffect *add_op = SETG(dest, ADD(wreg, freg));
	RzILOpEffect *set_status_op = SET_STATUS_ADD(VARL("wreg_old"), freg, wreg);
	return SEQ3(wreg_old, add_op, set_status_op);
}

IL_LIFTER_IMPL(MOVF) {}
IL_LIFTER_IMPL(COMF) {}
IL_LIFTER_IMPL(INCF) {}
IL_LIFTER_IMPL(DECFSZ) {}
IL_LIFTER_IMPL(RRF) {}
IL_LIFTER_IMPL(RLF) {}
IL_LIFTER_IMPL(SWAPF) {}
IL_LIFTER_IMPL(INCFSZ) {}
IL_LIFTER_IMPL(BCF) {}
IL_LIFTER_IMPL(BSF) {}
IL_LIFTER_IMPL(BTFSC) {}
IL_LIFTER_IMPL(BTFSS) {}
IL_LIFTER_IMPL(CALL) {}
IL_LIFTER_IMPL(GOTO) {}
IL_LIFTER_IMPL(MOVLW) {}
IL_LIFTER_IMPL(RETLW) {}

IL_LIFTER_IMPL(IORLW) {}

static RzILOpEffect *SET(const char *flag, RzILOpBool *pPure) {
	return SETG(flag, pPure);
}

IL_LIFTER_IMPL(ANDLW) {
	ut8 literal = PIC_MIDRANGE_OP_ARGS_8K_GET_K(instr);
	RzILOpPure *wreg = GET_WREG();
	RzILOpEffect *and_op = SETG("wreg", LOGAND(wreg, U8(literal)));
	RzILOpEffect *set_status_op = SET(STATUS(Z), IS_ZERO(wreg));
	return SEQ2(and_op, set_status_op);
}

/**
 * XORLW.
 * Operation: Take logical XOR between literal and wreg
 * Operands: Literal (k)
 * Status affected : Z
 * */
IL_LIFTER_IMPL(XORLW) {
	ut8 literal = PIC_MIDRANGE_OP_ARGS_8K_GET_K(instr);
	RzILOpPure *wreg = GET_WREG();
	RzILOpEffect *xor_op = SETG("wreg", LOGXOR(wreg, U8(literal)));
	RzILOpEffect *set_status_op = SET(STATUS(Z), IS_ZERO(wreg));
	return SEQ2(xor_op, set_status_op);
}

/**
 * SUBLW.
 * Operation: Subtract Literal From wreg
 * Operands: Literal (k)
 * Status affected : C, DC, Z
 * */
IL_LIFTER_IMPL(SUBLW) {
	ut8 literal = PIC_MIDRANGE_OP_ARGS_8K_GET_K(instr);
	RzILOpPure *wreg = GET_WREG();
	RzILOpEffect *wreg_old = SETL("wreg_old", wreg);
	RzILOpEffect *sub_op = SETG("wreg", SUB(wreg, U8(literal)));
	RzILOpEffect *set_status_op = SET_STATUS_SUB(VARL("wreg_old"), U8(literal), wreg);
	return SEQ3(wreg_old, sub_op, set_status_op);
}

/**
 * ADDLW.
 * Operation: Add Literal To wreg
 * Operands: Literal (k)
 * Status affected : C, DC, Z
 * */
IL_LIFTER_IMPL(ADDLW) {
	ut8 literal = PIC_MIDRANGE_OP_ARGS_8K_GET_K(instr);
	RzILOpPure *wreg = GET_WREG();
	RzILOpEffect *wreg_old = SETL("wreg_old", wreg);
	RzILOpEffect *add_op = SETG("wreg", ADD(wreg, U8(literal)));
	RzILOpEffect *set_status_op = SET_STATUS_ADD(VARL("wreg_old"), U8(literal), wreg);
	return SEQ3(wreg_old, add_op, set_status_op);
}

IL_LIFTER_IMPL(RESET) {}
IL_LIFTER_IMPL(CALLW) {}
IL_LIFTER_IMPL(BRW) {}
IL_LIFTER_IMPL(MOVIW_1) {}
IL_LIFTER_IMPL(MOVWI_1) {}
IL_LIFTER_IMPL(MOVLB) {}
IL_LIFTER_IMPL(LSLF) {}
IL_LIFTER_IMPL(LSRF) {}
IL_LIFTER_IMPL(ASRF) {}
IL_LIFTER_IMPL(SUBWFB) {}
IL_LIFTER_IMPL(ADDWFC) {}
IL_LIFTER_IMPL(ADDFSR) {}
IL_LIFTER_IMPL(MOVLP) {}
IL_LIFTER_IMPL(BRA) {}
IL_LIFTER_IMPL(MOVIW_2) {}
IL_LIFTER_IMPL(MOVWI_2) {}

// INSTRUCTIONS LOOKUP-TABLE
static const FN_PicILUplifter pic_midrange_il_uplifters[] = {
	[PIC_MIDRANGE_OPCODE_NOP] = IL_LIFTER(NOP),
	[PIC_MIDRANGE_OPCODE_RETURN] = IL_LIFTER(RETURN),
	[PIC_MIDRANGE_OPCODE_RETFIE] = IL_LIFTER(RETFIE),
	[PIC_MIDRANGE_OPCODE_OPTION] = IL_LIFTER(OPTION),
	[PIC_MIDRANGE_OPCODE_SLEEP] = IL_LIFTER(SLEEP),
	[PIC_MIDRANGE_OPCODE_CLRWDT] = IL_LIFTER(CLRWDT),
	[PIC_MIDRANGE_OPCODE_TRIS] = IL_LIFTER(TRIS),
	[PIC_MIDRANGE_OPCODE_MOVWF] = IL_LIFTER(MOVWF),
	[PIC_MIDRANGE_OPCODE_CLR] = IL_LIFTER(CLR),
	[PIC_MIDRANGE_OPCODE_SUBWF] = IL_LIFTER(SUBWF),
	[PIC_MIDRANGE_OPCODE_DECF] = IL_LIFTER(DECF),
	[PIC_MIDRANGE_OPCODE_IORWF] = IL_LIFTER(IORWF),
	[PIC_MIDRANGE_OPCODE_ANDWF] = IL_LIFTER(ANDWF),
	[PIC_MIDRANGE_OPCODE_XORWF] = IL_LIFTER(XORWF),
	[PIC_MIDRANGE_OPCODE_ADDWF] = IL_LIFTER(ADDWF),
	[PIC_MIDRANGE_OPCODE_MOVF] = IL_LIFTER(MOVF),
	[PIC_MIDRANGE_OPCODE_COMF] = IL_LIFTER(COMF),
	[PIC_MIDRANGE_OPCODE_INCF] = IL_LIFTER(INCF),
	[PIC_MIDRANGE_OPCODE_DECFSZ] = IL_LIFTER(DECFSZ),
	[PIC_MIDRANGE_OPCODE_RRF] = IL_LIFTER(RRF),
	[PIC_MIDRANGE_OPCODE_RLF] = IL_LIFTER(RLF),
	[PIC_MIDRANGE_OPCODE_SWAPF] = IL_LIFTER(SWAPF),
	[PIC_MIDRANGE_OPCODE_INCFSZ] = IL_LIFTER(INCFSZ),
	[PIC_MIDRANGE_OPCODE_BCF] = IL_LIFTER(BCF),
	[PIC_MIDRANGE_OPCODE_BSF] = IL_LIFTER(BSF),
	[PIC_MIDRANGE_OPCODE_BTFSC] = IL_LIFTER(BTFSC),
	[PIC_MIDRANGE_OPCODE_BTFSS] = IL_LIFTER(BTFSS),
	[PIC_MIDRANGE_OPCODE_CALL] = IL_LIFTER(CALL),
	[PIC_MIDRANGE_OPCODE_GOTO] = IL_LIFTER(GOTO),
	[PIC_MIDRANGE_OPCODE_MOVLW] = IL_LIFTER(MOVLW),
	[PIC_MIDRANGE_OPCODE_RETLW] = IL_LIFTER(RETLW),
	[PIC_MIDRANGE_OPCODE_IORLW] = IL_LIFTER(IORLW),
	[PIC_MIDRANGE_OPCODE_ANDLW] = IL_LIFTER(ANDLW),
	[PIC_MIDRANGE_OPCODE_XORLW] = IL_LIFTER(XORLW),
	[PIC_MIDRANGE_OPCODE_SUBLW] = IL_LIFTER(SUBLW),
	[PIC_MIDRANGE_OPCODE_ADDLW] = IL_LIFTER(ADDLW),
	[PIC_MIDRANGE_OPCODE_RESET] = IL_LIFTER(RESET),
	[PIC_MIDRANGE_OPCODE_CALLW] = IL_LIFTER(CALLW),
	[PIC_MIDRANGE_OPCODE_BRW] = IL_LIFTER(BRW),
	[PIC_MIDRANGE_OPCODE_MOVIW_1] = IL_LIFTER(MOVIW_1),
	[PIC_MIDRANGE_OPCODE_MOVWI_1] = IL_LIFTER(MOVWI_1),
	[PIC_MIDRANGE_OPCODE_MOVLB] = IL_LIFTER(MOVLB),
	[PIC_MIDRANGE_OPCODE_LSLF] = IL_LIFTER(LSLF),
	[PIC_MIDRANGE_OPCODE_LSRF] = IL_LIFTER(LSRF),
	[PIC_MIDRANGE_OPCODE_ASRF] = IL_LIFTER(ASRF),
	[PIC_MIDRANGE_OPCODE_SUBWFB] = IL_LIFTER(SUBWFB),
	[PIC_MIDRANGE_OPCODE_ADDWFC] = IL_LIFTER(ADDWFC),
	[PIC_MIDRANGE_OPCODE_ADDFSR] = IL_LIFTER(ADDFSR),
	[PIC_MIDRANGE_OPCODE_MOVLP] = IL_LIFTER(MOVLP),
	[PIC_MIDRANGE_OPCODE_BRA] = IL_LIFTER(BRA),
	[PIC_MIDRANGE_OPCODE_MOVIW_2] = IL_LIFTER(MOVIW_2),
	[PIC_MIDRANGE_OPCODE_MOVWI_2] = IL_LIFTER(MOVWI_2),
	[PIC_MIDRANGE_OPCODE_INVALID] = NULL
};

/**
 * Create new Mid-Range device CPU state.
 *
 * \param device_type Device to to initialize CPU state for.
 *
 * \return Valid ptr to PicMidrangeCPUState on success, NULL otherwise.
 * */
RZ_IPI RZ_OWN PicMidrangeCPUState *rz_pic_midrange_new_cpu_state(PicMidrangeDeviceType device_type) {
	if (device_type >= PIC_MIDRANGE_SUPPORTED_DEVICE_NUM) {
		RZ_LOG_ERROR("RzIL : Invalid PIC Mid-Range device type provided");
	}

	PicMidrangeCPUState *cpu_state = malloc(sizeof(PicMidrangeCPUState));
	if (!cpu_state) {
		return NULL;
	}

	cpu_state->device_type = device_type;
	cpu_state->selected_bank = 0; // initially bank is 0
	cpu_state->selected_page = 0; // initially page is 0
}

RZ_IPI RzILOpEffect *rz_midrange_il_op(
	RZ_NONNULL RzAnalysis *analysis, RZ_NONNULL RZ_BORROW RzAnalysisOp *op, RZ_NONNULL RZ_BORROW PicMidrangeCPUState *cpu_state, ut16 instr) {
	// get opcode
	PicMidrangeOpcode opcode = pic_midrange_get_opcode(instr);
	if (opcode == PIC_MIDRANGE_OPCODE_INVALID) {
		return NULL;
	}
	//
	//	PicMidrangeOpArgs opargs = pic_midrange_get_opargs(instr);

	FN_PicILUplifter uplifter = pic_midrange_il_uplifters[opcode];
	if (!uplifter) {
		return NULL;
	}

	return uplifter(cpu_state, instr);
}

/**
 * \brief Returns IL VM config for given PIC Mid-Range device type.
 *
 * \param analysis \c RzAnalysis instance.
 * \param device_type Device type in PIC16F family.
 *
 * \return valid ptr to RzAnalysisILConfig on success, NULL otherwise.
 * */
RZ_IPI RzAnalysisILConfig *rz_midrange_il_vm_config(RZ_NONNULL RzAnalysis *analysis, PicMidrangeDeviceType device_type) {
}