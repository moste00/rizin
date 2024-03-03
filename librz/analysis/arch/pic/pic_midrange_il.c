// SPDX-FileCopyrightText: 2023 Siddharth Mishra <admin@brightprogrammer.in>
// SPDX-License-Identifier: LGPL-3.0-only

#include <string.h>
#include <stdlib.h>

#include "pic_il.h"
#include "../../asm/arch/pic/pic_midrange.h"

#include <rz_il/rz_il_opbuilder_begin.h>

// HELPER DEFINES & TYPEDEFS

#define IL_LIFTER(op)      pic_midrange_##op##_il_lifter
#define IL_LIFTER_IMPL(op) static RzILOpEffect *pic_midrange_##op##_il_lifter( \
	RZ_NONNULL PicMidrangeILContext *ctx, ut16 instr)

// REGISTER DECLARATIONS & DEFINITIONS
#include "pic16f_memmaps/memmaps.h"

#define BANK_SIZE            ((ut32)0x80)
#define BANK_COMMON_MAP_LOW  cpu_state->selected_bank *BANK_SIZE + 0X70
#define BANK_COMMON_MAP_HIGH cpu_state->selected_bank *BANK_SIZE + 0X7F

// fields inside status register
const char *pic_midrange_status_flags[] = {
	"IRP", "RP1", "RP0", "TO", "PD", "Z", "DC", "C"
};

#define STATUS(x) pic_midrange_status_flags[x]
#define K         (ctx->args.k)
#define D         (ctx->args.d)
#define F         (ctx->args.f)
#define B         (ctx->args.b)
#define W         "w"
#define WF_sel    (D ? pic_midrange_regname(F) : "w")

// device to register schema map
PicMidrangeRegType *pic_midrange_device_reg_map[] = {
	[PIC16F882] = pic16f882_reg_map,
	[PIC16F883] = pic16f883_reg_map,
	//	[PIC16F884] = pic16f884_reg_map,
	[PIC16F886] = pic16f886_reg_map,
	//	[PIC16F887] = pic16f887_reg_map,
};

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
RzILOpEffect *pic_midrange_il_set_arithmetic_flags(
	RZ_BORROW RzILOpPure *x, RZ_BORROW RzILOpPure *y, RZ_BORROW RzILOpPure *res, bool add) {
	//	// get carry flag
	//	RzILOpBool *cf = NULL;
	//	RzILOpBool *dcf = NULL;
	//	if (add) {
	//		cf = CHECK_CARRY(x, y, res);
	//		dcf = CHECK_DIGIT_CARRY(x, y, res);
	//	} else { // sub
	//		cf = CHECK_BORROW(x, y, res);
	//		dcf = CHECK_DIGIT_BORROW(x, y, res);
	//	}
	//
	//	// get zero flag
	//	RzILOpBool *zf = IS_ZERO(res);
	//
	//	return SEQ3(SETG(STATUS(C), cf),
	//		SETG(STATUS(DC), dcf),
	//		SETG(STATUS(Z), zf));
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
}

IL_LIFTER_IMPL(DECF) {}
IL_LIFTER_IMPL(IORWF) {}

/**
 * ANDWF
 * Operation: Take logical AND of freg and wreg.
 * Operands: f, d
 * Status affected : Z
 * */
IL_LIFTER_IMPL(XORWF) {
}

IL_LIFTER_IMPL(MOVF) {}
IL_LIFTER_IMPL(COMF) {}
IL_LIFTER_IMPL(INCF) {}
IL_LIFTER_IMPL(DECFSZ) {}
IL_LIFTER_IMPL(RRF) {}
IL_LIFTER_IMPL(RLF) {}
IL_LIFTER_IMPL(SWAPF) {}
IL_LIFTER_IMPL(INCFSZ) {}
IL_LIFTER_IMPL(CALL) {}
IL_LIFTER_IMPL(GOTO) {}
IL_LIFTER_IMPL(MOVLW) {}
IL_LIFTER_IMPL(RETLW) {}

IL_LIFTER_IMPL(IORLW) {}

static RzILOpEffect *SET(const char *flag, RzILOpBool *pPure) {
	return SETG(flag, pPure);
}

/**
 * XORLW.
 * Operation: Take logical XOR between literal and wreg
 * Operands: Literal (k)
 * Status affected : Z
 * */
IL_LIFTER_IMPL(XORLW) {
}

/**
 * SUBLW.
 * Operation: Subtract Literal From wreg
 * Operands: Literal (k)
 * Status affected : C, DC, Z
 * */
IL_LIFTER_IMPL(SUBLW) {
	//	ut8 literal = PIC_MIDRANGE_OP_ARGS_8K_GET_K(instr);
	//	RzILOpEffect *set_status_op = SET_STATUS_SUB(VARL("wreg_old"), U8(literal), wreg);
	//	return SEQ3(wreg_old, sub_op, set_status_op);
}

/**
 * ADDLW.
 * Operation: Add Literal To wreg
 * Operands: Literal (k)
 * Status affected : C, DC, Z
 * */
IL_LIFTER_IMPL(ADDLW) {
	RzILOpEffect *add_op = SETG("w", ADD(VARG("w"), U16(K)));
	RzILOpEffect *set_status_op =
		SET_STATUS_ADD(VARL("_1"), U16(K), VARG("w"));
	return SEQ3(SETL("_1", VARG("w")),
		add_op,
		set_status_op);
}

/**
 * ADDWF
 * Operation: Add freg to wreg.
 * Operands: f, d
 * Status affected : C, DC, Z
 * */
IL_LIFTER_IMPL(ADDWF) {
	RzILOpEffect *add_op = SETG(WF_sel, ADD(VARG(W), U16(K)));
	RzILOpEffect *set_status_op =
		SET_STATUS_ADD(VARL("_1"), U16(K), VARG(WF_sel));
	return SEQ3(SETL("_1", VARG(W)),
		add_op,
		set_status_op);
}

IL_LIFTER_IMPL(ANDLW) {
	// TODO: set status Z
	return SETG(W, LOGAND(VARG(W), U16(K)));
}

/**
 * ANDWF
 * Operation: Take logical AND of freg and wreg.
 * Operands: f, d
 * Status affected : Z
 * */
IL_LIFTER_IMPL(ANDWF) {
	// TODO: set status Z
	return SETG(WF_sel, LOGAND(VARG(W), U16(K)));
}

static RzILOpEffect *bit_set(const char *reg, ut32 b, bool x) {
}

IL_LIFTER_IMPL(BCF) {
	return bit_set(pic_midrange_regname(F), B, 0);
}

IL_LIFTER_IMPL(BSF) {
	return bit_set(pic_midrange_regname(F), B, 1);
}
IL_LIFTER_IMPL(BTFSC) {}
IL_LIFTER_IMPL(BTFSS) {}

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

/**
 * Create new Mid-Range device CPU state.
 *
 * */
RZ_IPI bool rz_pic_midrange_cpu_state_setup(
	PicMidrangeCPUState *state,
	PicMidrangeDeviceType device_type) {
	rz_return_val_if_fail(state, NULL);
	if (device_type >= PIC_MIDRANGE_SUPPORTED_DEVICE_NUM) {
		RZ_LOG_ERROR("RzIL : Invalid PIC Mid-Range device type provided");
		return false;
	}

	state->device_type = device_type;
	state->selected_bank = 0; // initially bank is 0
	state->selected_page = 0; // initially page is 0
	return true;
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