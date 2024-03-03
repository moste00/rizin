// SPDX-FileCopyrightText: 2023 Siddharth Mishra <admin@brightprogrammer.in>
// SPDX-License-Identifier: LGPL-3.0-only

/**
 * Defines the various reigster types supported by the PIC16F family
 * and a lookup table to map from these register types to their string names.
 * */

#ifndef RZ_PIC_MIDRANGE_PIC_REGTYPES_H
#define RZ_PIC_MIDRANGE_PIC_REGTYPES_H

// clang-format off

/**
 * Instead of storing strings in file register map,
 * it's better to store enums in order to save memory.
 * */
typedef enum pic_midrange_reg_type_t {
    REG_INDF,
    REG_TMR0,
    REG_PCL, /* lower byte of PC register */
    REG_STATUS, /* status register */
    REG_FSR,
    REG_PORTA,
    REG_PORTB,
    REG_PORTC,
    REG_PORTD,
    REG_PORTE,
    REG_PORTF,
    REG_PORTG,
    REG_PCLATH, /* PCLATH is used to control PCH (higher part of PC reg)*/
    REG_INTCON,
    REG_PIR1,
    REG_PIR2,
    REG_TMR1L,
    REG_TMR1H,
    REG_T1CON,
    REG_TMR2,
    REG_T2CON,
    REG_SSPBUF,
    REG_SSPCON,
    REG_SSPCON2,
    REG_CCPR1L,
    REG_CCPR1H,
    REG_CCP1CON,
    REG_RCSTA,
    REG_TXREG,
    REG_RCREG,
    REG_CCPR2L,
    REG_CCPR2H,
    REG_CCP2CON,
    REG_ADRESH,
    REG_ADRESL,
    REG_ADCON0,
    REG_ADCON,
    REG_OPTION_REG,
    REG_TRISA,
    REG_TRISB,
    REG_TRISC,
    REG_TRISD,
    REG_TRISE,
    REG_TRISF,
    REG_TRISG,
    REG_PIE1,
    REG_PIE2,
    REG_PCON,
    REG_OSCCON,
    REG_OSCCAL,
    REG_PR2,
    REG_SSPADD,
    REG_SSPSTAT,
    REG_TXSTA,
    REG_SPBRG,
    REG_SPBRGH,
    REG_ADDCON1,
    REG_OSCTUNE,
    REG_WPUB,
    REG_IOCB,
    REG_VRCON,
    REG_PWM1CON,
    REG_ECCPAS,
    REG_PSTRCON,
    REG_ADCON1,
    REG_WDTCON,
    REG_CM1CON0,
    REG_CM2CON0,
    REG_CM2CON1,
    REG_EEDAT,
    REG_EEADR,
    REG_EEDATH,
    REG_EEADRH,
    REG_SRCON,
    REG_BAUDCTL,
    REG_ANSEL,
    REG_ANSELH,
    REG_EECON1,
    REG_EECON2,
    REG_RESERVED,
    REG_FREG, /* normal indexed file register */
    REG_UNIMPLEMENTED, /* unimplemented registers are read as 0 */
    REG_INVALID /* can be used when a function fails and want to return an invalid value */
} PicMidrangeRegType;

/**
 * Map from reg enums to reg names
 * */
const char* pic_midrange_il_regnames[REG_UNIMPLEMENTED + 1] = {
    [REG_INDF]          = "indf",
    [REG_TMR0]          = "tmr0",
    [REG_PCL]           = "pcl",
    [REG_STATUS]        = "status",
    [REG_FSR]           = "fsr",
    [REG_PORTA]         = "porta",
    [REG_PORTB]         = "portb",
    [REG_PORTC]         = "portc",
    [REG_PORTD]         = "portd",
    [REG_PORTE]         = "porte",
    [REG_PORTF]         = "portf",
    [REG_PORTG]         = "portg",
    [REG_PCLATH]        = "pclath",
    [REG_INTCON]        = "intcon",
    [REG_PIR1]          = "pir1",
    [REG_PIR2]          = "pir2",
    [REG_TMR1L]         = "tmr1l",
    [REG_TMR1H]         = "tmr1h",
    [REG_T1CON]         = "t1con",
    [REG_TMR2]          = "tmr2",
    [REG_T2CON]         = "t2con",
    [REG_SSPBUF]        = "sspbuf",
    [REG_SSPCON]        = "sspcon",
    [REG_SSPCON2]       = "sspcon2",
    [REG_CCPR1L]        = "ccpr1l",
    [REG_CCPR1H]        = "ccpr1h",
    [REG_CCP1CON]       = "ccp1con",
    [REG_RCSTA]         = "rcsta",
    [REG_TXREG]         = "txreg",
    [REG_RCREG]         = "rcreg",
    [REG_CCPR2L]        = "ccpr2l",
    [REG_CCPR2H]        = "ccpr2h",
    [REG_CCP2CON]       = "ccp2con",
    [REG_ADRESH]        = "adresh",
    [REG_ADRESL]        = "adresl",
    [REG_ADCON0]        = "adcon0",
    [REG_ADCON]         = "adcon",
    [REG_OPTION_REG]    = "option_reg",
    [REG_TRISA]         = "trisa",
    [REG_TRISB]         = "trisb",
    [REG_TRISC]         = "trisc",
    [REG_TRISD]         = "trisd",
    [REG_TRISE]         = "trise",
    [REG_TRISF]         = "trisf",
    [REG_TRISG]         = "trisg",
    [REG_PIE1]          = "pie1",
    [REG_PIE2]          = "pie2",
    [REG_PCON]          = "pcon",
    [REG_OSCCON]        = "osccon",
    [REG_OSCCAL]        = "osccal",
    [REG_PR2]           = "pr2",
    [REG_SSPADD]        = "sspadd",
    [REG_SSPSTAT]       = "sspstat",
    [REG_TXSTA]         = "txsta",
    [REG_SPBRG]         = "spbrg",
    [REG_SPBRGH]        = "spbrgh",
    [REG_ADDCON1]       = "addcon1",
    [REG_OSCTUNE]       = "osctune",
    [REG_WPUB]          = "wpub",
    [REG_IOCB]          = "iocb",
    [REG_VRCON]         = "vrcon",
    [REG_PWM1CON]       = "pwm1con",
    [REG_ECCPAS]        = "eccpas",
    [REG_PSTRCON]       = "pstrcon",
    [REG_ADCON1]        = "adcon1",
    [REG_WDTCON]        = "wdtcon",
    [REG_CM1CON0]       = "cm1con0",
    [REG_CM2CON0]       = "cm2con0",
    [REG_CM2CON1]       = "cm2con1",
    [REG_EEDAT]         = "eedat",
    [REG_EEADR]         = "eeadr",
    [REG_EEDATH]        = "eedath",
    [REG_EEADRH]        = "eeadrh",
    [REG_SRCON]         = "srcon",
    [REG_BAUDCTL]       = "baudctl",
    [REG_ANSEL]         = "ansel",
    [REG_ANSELH]        = "anselh",
    [REG_EECON1]        = "eecon1",
    [REG_EECON2]        = "eecon2",
    [REG_RESERVED]      = "reserved",
    [REG_FREG]          = "freg",
    [REG_UNIMPLEMENTED] = "unimplemented",
};
// clang-format on

static inline const char *pic_midrange_regname(ut32 reg) {
	if (reg >= RZ_ARRAY_SIZE(pic_midrange_il_regnames)) {
		return NULL;
	}
	return pic_midrange_il_regnames[reg];
}

#endif // RZ_PIC_MIDRANGE_PIC_REGTYPES_H
