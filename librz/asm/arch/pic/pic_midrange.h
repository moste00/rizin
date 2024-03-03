// SPDX-FileCopyrightText: 2018 courk <courk@courk.cc>
// SPDX-License-Identifier: LGPL-3.0-only

#ifndef PIC_MIDRANGE_H
#define PIC_MIDRANGE_H

#include <rz_asm.h>
#include <rz_types.h>

typedef enum {
	PIC_MIDRANGE_OP_ARGS_NONE = 0,
	PIC_MIDRANGE_OP_ARGS_2F,
	PIC_MIDRANGE_OP_ARGS_7F,
	PIC_MIDRANGE_OP_ARGS_1D_7F,
	PIC_MIDRANGE_OP_ARGS_1N_6K,
	PIC_MIDRANGE_OP_ARGS_3B_7F,
	PIC_MIDRANGE_OP_ARGS_4K,
	PIC_MIDRANGE_OP_ARGS_8K,
	PIC_MIDRANGE_OP_ARGS_9K,
	PIC_MIDRANGE_OP_ARGS_11K,
	PIC_MIDRANGE_OP_ARGS_1N_2M
} PicMidrangeOpArgs;

#define PIC_MIDRANGE_OP_ARGS_2F_MASK_F    0x3
#define PIC_MIDRANGE_OP_ARGS_7F_MASK_F    0x7f
#define PIC_MIDRANGE_OP_ARGS_1D_7F_MASK_D (1 << 7)
#define PIC_MIDRANGE_OP_ARGS_1D_7F_MASK_F 0x7f
#define PIC_MIDRANGE_OP_ARGS_1N_6K_MASK_N (1 << 6)
#define PIC_MIDRANGE_OP_ARGS_1N_6K_MASK_K 0x3f
#define PIC_MIDRANGE_OP_ARGS_3B_7F_MASK_B (0x7 << 7)
#define PIC_MIDRANGE_OP_ARGS_3B_7F_MASK_F 0x7f
#define PIC_MIDRANGE_OP_ARGS_4K_MASK_K    0xf
#define PIC_MIDRANGE_OP_ARGS_8K_MASK_K    0xff
#define PIC_MIDRANGE_OP_ARGS_9K_MASK_K    0x1ff
#define PIC_MIDRANGE_OP_ARGS_11K_MASK_K   0x7ff
#define PIC_MIDRANGE_OP_ARGS_1N_2M_MASK_N (1 << 2)
#define PIC_MIDRANGE_OP_ARGS_1N_2M_MASK_M 0x3

/* use the defines instead of polluting the disassembly code with & operations */
#define PIC_MIDRANGE_OP_ARGS_2F_GET_F(instr)    instr &PIC_MIDRANGE_OP_ARGS_2F_MASK_F
#define PIC_MIDRANGE_OP_ARGS_7F_GET_F(instr)    instr &PIC_MIDRANGE_OP_ARGS_7F_MASK_F
#define PIC_MIDRANGE_OP_ARGS_1D_7F_GET_D(instr) instr &PIC_MIDRANGE_OP_ARGS_1D_7F_MASK_D
#define PIC_MIDRANGE_OP_ARGS_1D_7F_GET_F(instr) instr &PIC_MIDRANGE_OP_ARGS_1D_7F_MASK_F
#define PIC_MIDRANGE_OP_ARGS_1N_6K_GET_N(instr) instr &PIC_MIDRANGE_OP_ARGS_1N_6K_MASK_N
#define PIC_MIDRANGE_OP_ARGS_1N_6K_GET_K(instr) instr &PIC_MIDRANGE_OP_ARGS_1N_6K_MASK_K
#define PIC_MIDRANGE_OP_ARGS_3B_7F_GET_B(instr) instr &PIC_MIDRANGE_OP_ARGS_3B_7F_MASK_B
#define PIC_MIDRANGE_OP_ARGS_3B_7F_GET_F(instr) instr &PIC_MIDRANGE_OP_ARGS_3B_7F_MASK_F
#define PIC_MIDRANGE_OP_ARGS_4K_GET_K(instr)    instr &PIC_MIDRANGE_OP_ARGS_4K_MASK_K
#define PIC_MIDRANGE_OP_ARGS_8K_GET_K(instr)    instr &PIC_MIDRANGE_OP_ARGS_8K_MASK_K
#define PIC_MIDRANGE_OP_ARGS_9K_GET_K(instr)    instr &PIC_MIDRANGE_OP_ARGS_9K_MASK_K
#define PIC_MIDRANGE_OP_ARGS_11K_GET_K(instr)   instr &PIC_MIDRANGE_OP_ARGS_11K_MASK_K
#define PIC_MIDRANGE_OP_ARGS_1N_2M_GET_N(instr) instr &PIC_MIDRANGE_OP_ARGS_1N_2M_MASK_N
#define PIC_MIDRANGE_OP_ARGS_1N_2M_GET_M(instr) instr &PIC_MIDRANGE_OP_ARGS_1N_2M_MASK_M

typedef struct _pic_midrange_op {
	const char *mnemonic;
	PicMidrangeOpArgs args;
} PicMidrangeOpAsmInfo;

typedef enum {
	PIC_MIDRANGE_OPCODE_NOP = 0,
	PIC_MIDRANGE_OPCODE_RETURN,
	PIC_MIDRANGE_OPCODE_RETFIE,
	PIC_MIDRANGE_OPCODE_OPTION,
	PIC_MIDRANGE_OPCODE_SLEEP,
	PIC_MIDRANGE_OPCODE_CLRWDT,
	PIC_MIDRANGE_OPCODE_TRIS,
	PIC_MIDRANGE_OPCODE_MOVWF,
	PIC_MIDRANGE_OPCODE_CLR,
	PIC_MIDRANGE_OPCODE_SUBWF,
	PIC_MIDRANGE_OPCODE_DECF,
	PIC_MIDRANGE_OPCODE_IORWF,
	PIC_MIDRANGE_OPCODE_ANDWF,
	PIC_MIDRANGE_OPCODE_XORWF,
	PIC_MIDRANGE_OPCODE_ADDWF,
	PIC_MIDRANGE_OPCODE_MOVF,
	PIC_MIDRANGE_OPCODE_COMF,
	PIC_MIDRANGE_OPCODE_INCF,
	PIC_MIDRANGE_OPCODE_DECFSZ,
	PIC_MIDRANGE_OPCODE_RRF,
	PIC_MIDRANGE_OPCODE_RLF,
	PIC_MIDRANGE_OPCODE_SWAPF,
	PIC_MIDRANGE_OPCODE_INCFSZ,
	PIC_MIDRANGE_OPCODE_BCF,
	PIC_MIDRANGE_OPCODE_BSF,
	PIC_MIDRANGE_OPCODE_BTFSC,
	PIC_MIDRANGE_OPCODE_BTFSS,
	PIC_MIDRANGE_OPCODE_CALL,
	PIC_MIDRANGE_OPCODE_GOTO,
	PIC_MIDRANGE_OPCODE_MOVLW,
	PIC_MIDRANGE_OPCODE_RETLW,
	PIC_MIDRANGE_OPCODE_IORLW,
	PIC_MIDRANGE_OPCODE_ANDLW,
	PIC_MIDRANGE_OPCODE_XORLW,
	PIC_MIDRANGE_OPCODE_SUBLW,
	PIC_MIDRANGE_OPCODE_ADDLW,
	PIC_MIDRANGE_OPCODE_RESET,
	PIC_MIDRANGE_OPCODE_CALLW,
	PIC_MIDRANGE_OPCODE_BRW,
	PIC_MIDRANGE_OPCODE_MOVIW_1,
	PIC_MIDRANGE_OPCODE_MOVWI_1,
	PIC_MIDRANGE_OPCODE_MOVLB,
	PIC_MIDRANGE_OPCODE_LSLF,
	PIC_MIDRANGE_OPCODE_LSRF,
	PIC_MIDRANGE_OPCODE_ASRF,
	PIC_MIDRANGE_OPCODE_SUBWFB,
	PIC_MIDRANGE_OPCODE_ADDWFC,
	PIC_MIDRANGE_OPCODE_ADDFSR,
	PIC_MIDRANGE_OPCODE_MOVLP,
	PIC_MIDRANGE_OPCODE_BRA,
	PIC_MIDRANGE_OPCODE_MOVIW_2,
	PIC_MIDRANGE_OPCODE_MOVWI_2,
	PIC_MIDRANGE_OPCODE_INVALID
} PicMidrangeOpcode;



PicMidrangeOpcode pic_midrange_get_opcode(ut16 instr);
PicMidrangeOpArgs pic_midrange_get_opargs(PicMidrangeOpcode opcode);
const PicMidrangeOpAsmInfo *pic_midrange_get_op_info(PicMidrangeOpcode opcode);
int pic_midrange_disassemble(RzAsmOp *op, const ut8 *b, int l);

#endif // PIC_MIDRANGE_H
