// SPDX-FileCopyrightText: 2023 Siddharth Mishra <admin@brightprogrammer.in>
// SPDX-License-Identifier: LGPL-3.0-only

/**
 * This file describes the memory map of PICF16882 device in PIC16F family.
 * */

#ifndef RZ_PIC_MIDRANGE_PIC_MEMMAP_PIC16F882_H
#define RZ_PIC_MIDRANGE_PIC_MEMMAP_PIC16F882_H

#include "regtypes.h"

// clang-format off
PicMidrangeRegType pic16f882_reg_map[] = {
    /* BANK 0 */
    REG_INDF,                   /* 0x00 */
    REG_TMR0,                   /* 0x01 */
    REG_PCL,                    /* 0x02 */
    REG_STATUS,                 /* 0x03 */
    REG_FSR,                    /* 0x04 */
    REG_PORTA,                  /* 0x05 */
    REG_PORTB,                  /* 0x06 */
    REG_PORTC,                  /* 0x07 */
    REG_UNIMPLEMENTED,          /* 0x08 */
    REG_PORTE,                  /* 0x09 */
    REG_PCLATH,                 /* 0x0a */
    REG_INTCON,                 /* 0x0b */
    REG_PIR1,                   /* 0x0c */
    REG_PIR2,                   /* 0x0d */
    REG_TMR1L,                  /* 0x0e */
    REG_TMR1H,                  /* 0x0f */
    REG_T1CON,                  /* 0x10 */
    REG_TMR2,                   /* 0x11 */
    REG_T2CON,                  /* 0x12 */
    REG_SSPBUF,                 /* 0x13 */
    REG_SSPCON,                 /* 0x14 */
    REG_CCPR1L,                 /* 0x15 */
    REG_CCPR1H,                 /* 0x16 */
    REG_CCP1CON,                /* 0x17 */
    REG_RCSTA,                  /* 0x18 */
    REG_TXREG,                  /* 0x19 */
    REG_RCREG,                  /* 0x1a */
    REG_CCPR2L,                 /* 0x1b */
    REG_CCPR2H,                 /* 0x1c */
    REG_CCP2CON,                /* 0x1d */
    REG_ADRESH,                 /* 0x1e */
    REG_ADCON0,                 /* 0x1f */
    REG_FREG,                   /* 0x20 */
    REG_FREG,                   /* 0x21 */
    REG_FREG,                   /* 0x22 */
    REG_FREG,                   /* 0x23 */
    REG_FREG,                   /* 0x24 */
    REG_FREG,                   /* 0x25 */
    REG_FREG,                   /* 0x26 */
    REG_FREG,                   /* 0x27 */
    REG_FREG,                   /* 0x28 */
    REG_FREG,                   /* 0x29 */
    REG_FREG,                   /* 0x2a */
    REG_FREG,                   /* 0x2b */
    REG_FREG,                   /* 0x2c */
    REG_FREG,                   /* 0x2d */
    REG_FREG,                   /* 0x2e */
    REG_FREG,                   /* 0x2f */
    REG_FREG,                   /* 0x30 */
    REG_FREG,                   /* 0x31 */
    REG_FREG,                   /* 0x32 */
    REG_FREG,                   /* 0x33 */
    REG_FREG,                   /* 0x34 */
    REG_FREG,                   /* 0x35 */
    REG_FREG,                   /* 0x36 */
    REG_FREG,                   /* 0x37 */
    REG_FREG,                   /* 0x38 */
    REG_FREG,                   /* 0x39 */
    REG_FREG,                   /* 0x3a */
    REG_FREG,                   /* 0x3b */
    REG_FREG,                   /* 0x3c */
    REG_FREG,                   /* 0x3d */
    REG_FREG,                   /* 0x3e */
    REG_FREG,                   /* 0x3f */
    REG_FREG,                   /* 0x40 */
    REG_FREG,                   /* 0x41 */
    REG_FREG,                   /* 0x42 */
    REG_FREG,                   /* 0x43 */
    REG_FREG,                   /* 0x44 */
    REG_FREG,                   /* 0x45 */
    REG_FREG,                   /* 0x46 */
    REG_FREG,                   /* 0x47 */
    REG_FREG,                   /* 0x48 */
    REG_FREG,                   /* 0x49 */
    REG_FREG,                   /* 0x4a */
    REG_FREG,                   /* 0x4b */
    REG_FREG,                   /* 0x4c */
    REG_FREG,                   /* 0x4d */
    REG_FREG,                   /* 0x4e */
    REG_FREG,                   /* 0x4f */
    REG_FREG,                   /* 0x50 */
    REG_FREG,                   /* 0x51 */
    REG_FREG,                   /* 0x52 */
    REG_FREG,                   /* 0x53 */
    REG_FREG,                   /* 0x54 */
    REG_FREG,                   /* 0x55 */
    REG_FREG,                   /* 0x56 */
    REG_FREG,                   /* 0x57 */
    REG_FREG,                   /* 0x58 */
    REG_FREG,                   /* 0x59 */
    REG_FREG,                   /* 0x5a */
    REG_FREG,                   /* 0x5b */
    REG_FREG,                   /* 0x5c */
    REG_FREG,                   /* 0x5d */
    REG_FREG,                   /* 0x5e */
    REG_FREG,                   /* 0x5f */
    REG_FREG,                   /* 0x60 */
    REG_FREG,                   /* 0x61 */
    REG_FREG,                   /* 0x62 */
    REG_FREG,                   /* 0x63 */
    REG_FREG,                   /* 0x64 */
    REG_FREG,                   /* 0x65 */
    REG_FREG,                   /* 0x66 */
    REG_FREG,                   /* 0x67 */
    REG_FREG,                   /* 0x68 */
    REG_FREG,                   /* 0x69 */
    REG_FREG,                   /* 0x6a */
    REG_FREG,                   /* 0x6b */
    REG_FREG,                   /* 0x6c */
    REG_FREG,                   /* 0x6d */
    REG_FREG,                   /* 0x6e */
    REG_FREG,                   /* 0x6f */
    REG_FREG,                   /* 0x70 */
    REG_FREG,                   /* 0x71 */
    REG_FREG,                   /* 0x72 */
    REG_FREG,                   /* 0x73 */
    REG_FREG,                   /* 0x74 */
    REG_FREG,                   /* 0x75 */
    REG_FREG,                   /* 0x76 */
    REG_FREG,                   /* 0x77 */
    REG_FREG,                   /* 0x78 */
    REG_FREG,                   /* 0x79 */
    REG_FREG,                   /* 0x7a */
    REG_FREG,                   /* 0x7b */
    REG_FREG,                   /* 0x7c */
    REG_FREG,                   /* 0x7d */
    REG_FREG,                   /* 0x7e */
    REG_FREG,                   /* 0x7f */

    /* BANK1 */

    REG_INDF,                   /* 0x80 */
    REG_OPTION_REG,             /* 0x81 */
    REG_PCL,                    /* 0x82 */
    REG_STATUS,                 /* 0x83 */
    REG_FSR,                    /* 0x84 */
    REG_TRISA,                  /* 0x85 */
    REG_TRISB,                  /* 0x86 */
    REG_TRISC,                  /* 0x87 */
    REG_UNIMPLEMENTED,          /* 0x88 */
    REG_TRISE,                  /* 0x89 */
    REG_PCLATH,                 /* 0x8a */
    REG_INTCON,                 /* 0x8b */
    REG_PIE1,                   /* 0x8c */
    REG_PIE2,                   /* 0x8d */
    REG_PCON,                   /* 0x8e */
    REG_OSCCON,                 /* 0x8f */
    REG_OSCTUNE,                /* 0x90 */
    REG_SSPCON2,                /* 0x91 */
    REG_PR2,                    /* 0x92 */
    REG_SSPADD,                 /* 0x93 */
    REG_SSPSTAT,                /* 0x94 */
    REG_WPUB,                   /* 0x95 */
    REG_IOCB,                   /* 0x96 */
    REG_VRCON,                  /* 0x97 */
    REG_RCSTA,                  /* 0x98 */
    REG_TXSTA,                  /* 0x99 */
    REG_SPBRG,                  /* 0x9a */
    REG_SPBRGH,                 /* 0x9b */
    REG_PWM1CON,                /* 0x9c */
    REG_ECCPAS,                 /* 0x9d */
    REG_ADRESL,                 /* 0x9e */
    REG_ADCON1,                 /* 0x9f */
    REG_FREG,                   /* 0xa0 */
    REG_FREG,                   /* 0xa1 */
    REG_FREG,                   /* 0xa2 */
    REG_FREG,                   /* 0xa3 */
    REG_FREG,                   /* 0xa4 */
    REG_FREG,                   /* 0xa5 */
    REG_FREG,                   /* 0xa6 */
    REG_FREG,                   /* 0xa7 */
    REG_FREG,                   /* 0xa8 */
    REG_FREG,                   /* 0xa9 */
    REG_FREG,                   /* 0xaa */
    REG_FREG,                   /* 0xab */
    REG_FREG,                   /* 0xac */
    REG_FREG,                   /* 0xad */
    REG_FREG,                   /* 0xae */
    REG_FREG,                   /* 0xaf */
    REG_FREG,                   /* 0xb0 */
    REG_FREG,                   /* 0xb1 */
    REG_FREG,                   /* 0xb2 */
    REG_FREG,                   /* 0xb3 */
    REG_FREG,                   /* 0xb4 */
    REG_FREG,                   /* 0xb5 */
    REG_FREG,                   /* 0xb6 */
    REG_FREG,                   /* 0xb7 */
    REG_FREG,                   /* 0xb8 */
    REG_FREG,                   /* 0xb9 */
    REG_FREG,                   /* 0xba */
    REG_FREG,                   /* 0xbb */
    REG_FREG,                   /* 0xbc */
    REG_FREG,                   /* 0xbd */
    REG_FREG,                   /* 0xbe */
    REG_FREG,                   /* 0xbf */
    REG_UNIMPLEMENTED,          /* 0xc0 */
    REG_UNIMPLEMENTED,          /* 0xc1 */
    REG_UNIMPLEMENTED,          /* 0xc2 */
    REG_UNIMPLEMENTED,          /* 0xc3 */
    REG_UNIMPLEMENTED,          /* 0xc4 */
    REG_UNIMPLEMENTED,          /* 0xc5 */
    REG_UNIMPLEMENTED,          /* 0xc6 */
    REG_UNIMPLEMENTED,          /* 0xc7 */
    REG_UNIMPLEMENTED,          /* 0xc8 */
    REG_UNIMPLEMENTED,          /* 0xc9 */
    REG_UNIMPLEMENTED,          /* 0xca */
    REG_UNIMPLEMENTED,          /* 0xcb */
    REG_UNIMPLEMENTED,          /* 0xcc */
    REG_UNIMPLEMENTED,          /* 0xcd */
    REG_UNIMPLEMENTED,          /* 0xce */
    REG_UNIMPLEMENTED,          /* 0xcf */
    REG_UNIMPLEMENTED,          /* 0xd0 */
    REG_UNIMPLEMENTED,          /* 0xd1 */
    REG_UNIMPLEMENTED,          /* 0xd2 */
    REG_UNIMPLEMENTED,          /* 0xd3 */
    REG_UNIMPLEMENTED,          /* 0xd4 */
    REG_UNIMPLEMENTED,          /* 0xd5 */
    REG_UNIMPLEMENTED,          /* 0xd6 */
    REG_UNIMPLEMENTED,          /* 0xd7 */
    REG_UNIMPLEMENTED,          /* 0xd8 */
    REG_UNIMPLEMENTED,          /* 0xd9 */
    REG_UNIMPLEMENTED,          /* 0xda */
    REG_UNIMPLEMENTED,          /* 0xdb */
    REG_UNIMPLEMENTED,          /* 0xdc */
    REG_UNIMPLEMENTED,          /* 0xdd */
    REG_UNIMPLEMENTED,          /* 0xde */
    REG_UNIMPLEMENTED,          /* 0xdf */
    REG_UNIMPLEMENTED,          /* 0xe0 */
    REG_UNIMPLEMENTED,          /* 0xe1 */
    REG_UNIMPLEMENTED,          /* 0xe2 */
    REG_UNIMPLEMENTED,          /* 0xe3 */
    REG_UNIMPLEMENTED,          /* 0xe4 */
    REG_UNIMPLEMENTED,          /* 0xe5 */
    REG_UNIMPLEMENTED,          /* 0xe6 */
    REG_UNIMPLEMENTED,          /* 0xe7 */
    REG_UNIMPLEMENTED,          /* 0xe8 */
    REG_UNIMPLEMENTED,          /* 0xe9 */
    REG_UNIMPLEMENTED,          /* 0xea */
    REG_UNIMPLEMENTED,          /* 0xeb */
    REG_UNIMPLEMENTED,          /* 0xec */
    REG_UNIMPLEMENTED,          /* 0xed */
    REG_UNIMPLEMENTED,          /* 0xee */
    REG_UNIMPLEMENTED,          /* 0xef */
    REG_FREG,                   /* 0xf0 */
    REG_FREG,                   /* 0xf1 */
    REG_FREG,                   /* 0xf2 */
    REG_FREG,                   /* 0xf3 */
    REG_FREG,                   /* 0xf4 */
    REG_FREG,                   /* 0xf5 */
    REG_FREG,                   /* 0xf6 */
    REG_FREG,                   /* 0xf7 */
    REG_FREG,                   /* 0xf8 */
    REG_FREG,                   /* 0xf9 */
    REG_FREG,                   /* 0xfa */
    REG_FREG,                   /* 0xfb */
    REG_FREG,                   /* 0xfc */
    REG_FREG,                   /* 0xfd */
    REG_FREG,                   /* 0xfe */
    REG_FREG,                   /* 0xff */

    /* BANK 2 */

    REG_INDF,                   /* 0x100 */
    REG_TMR0,                   /* 0x101 */
    REG_PCL,                    /* 0x102 */
    REG_STATUS,                 /* 0x103 */
    REG_FSR,                    /* 0x104 */
    REG_WDTCON,                 /* 0x105 */
    REG_PORTB,                  /* 0x106 */
    REG_CM1CON0,                /* 0x107 */
    REG_CM2CON0,                /* 0x108 */
    REG_CM2CON1,                /* 0x109 */
    REG_PCLATH,                 /* 0x10a */
    REG_INTCON,                 /* 0x10b */
    REG_EEDAT,                  /* 0x10c */
    REG_EEADR,                  /* 0x10d */
    REG_EEDATH,                 /* 0x10e */
    REG_EEADRH,                 /* 0x10f */
    REG_UNIMPLEMENTED,          /* 0x110 */
    REG_UNIMPLEMENTED,          /* 0x111 */
    REG_UNIMPLEMENTED,          /* 0x112 */
    REG_UNIMPLEMENTED,          /* 0x113 */
    REG_UNIMPLEMENTED,          /* 0x114 */
    REG_UNIMPLEMENTED,          /* 0x115 */
    REG_UNIMPLEMENTED,          /* 0x116 */
    REG_UNIMPLEMENTED,          /* 0x117 */
    REG_UNIMPLEMENTED,          /* 0x118 */
    REG_UNIMPLEMENTED,          /* 0x119 */
    REG_UNIMPLEMENTED,          /* 0x11a */
    REG_UNIMPLEMENTED,          /* 0x11b */
    REG_UNIMPLEMENTED,          /* 0x11c */
    REG_UNIMPLEMENTED,          /* 0x11d */
    REG_UNIMPLEMENTED,          /* 0x11e */
    REG_UNIMPLEMENTED,          /* 0x11f */
    REG_UNIMPLEMENTED,          /* 0x120 */
    REG_UNIMPLEMENTED,          /* 0x121 */
    REG_UNIMPLEMENTED,          /* 0x122 */
    REG_UNIMPLEMENTED,          /* 0x123 */
    REG_UNIMPLEMENTED,          /* 0x124 */
    REG_UNIMPLEMENTED,          /* 0x125 */
    REG_UNIMPLEMENTED,          /* 0x126 */
    REG_UNIMPLEMENTED,          /* 0x127 */
    REG_UNIMPLEMENTED,          /* 0x128 */
    REG_UNIMPLEMENTED,          /* 0x129 */
    REG_UNIMPLEMENTED,          /* 0x12a */
    REG_UNIMPLEMENTED,          /* 0x12b */
    REG_UNIMPLEMENTED,          /* 0x12c */
    REG_UNIMPLEMENTED,          /* 0x12d */
    REG_UNIMPLEMENTED,          /* 0x12e */
    REG_UNIMPLEMENTED,          /* 0x12f */
    REG_UNIMPLEMENTED,          /* 0x130 */
    REG_UNIMPLEMENTED,          /* 0x131 */
    REG_UNIMPLEMENTED,          /* 0x132 */
    REG_UNIMPLEMENTED,          /* 0x133 */
    REG_UNIMPLEMENTED,          /* 0x134 */
    REG_UNIMPLEMENTED,          /* 0x135 */
    REG_UNIMPLEMENTED,          /* 0x136 */
    REG_UNIMPLEMENTED,          /* 0x137 */
    REG_UNIMPLEMENTED,          /* 0x138 */
    REG_UNIMPLEMENTED,          /* 0x139 */
    REG_UNIMPLEMENTED,          /* 0x13a */
    REG_UNIMPLEMENTED,          /* 0x13b */
    REG_UNIMPLEMENTED,          /* 0x13c */
    REG_UNIMPLEMENTED,          /* 0x13d */
    REG_UNIMPLEMENTED,          /* 0x13e */
    REG_UNIMPLEMENTED,          /* 0x13f */
    REG_UNIMPLEMENTED,          /* 0x140 */
    REG_UNIMPLEMENTED,          /* 0x141 */
    REG_UNIMPLEMENTED,          /* 0x142 */
    REG_UNIMPLEMENTED,          /* 0x143 */
    REG_UNIMPLEMENTED,          /* 0x144 */
    REG_UNIMPLEMENTED,          /* 0x145 */
    REG_UNIMPLEMENTED,          /* 0x146 */
    REG_UNIMPLEMENTED,          /* 0x147 */
    REG_UNIMPLEMENTED,          /* 0x148 */
    REG_UNIMPLEMENTED,          /* 0x149 */
    REG_UNIMPLEMENTED,          /* 0x14a */
    REG_UNIMPLEMENTED,          /* 0x14b */
    REG_UNIMPLEMENTED,          /* 0x14c */
    REG_UNIMPLEMENTED,          /* 0x14d */
    REG_UNIMPLEMENTED,          /* 0x14e */
    REG_UNIMPLEMENTED,          /* 0x14f */
    REG_UNIMPLEMENTED,          /* 0x150 */
    REG_UNIMPLEMENTED,          /* 0x151 */
    REG_UNIMPLEMENTED,          /* 0x152 */
    REG_UNIMPLEMENTED,          /* 0x153 */
    REG_UNIMPLEMENTED,          /* 0x154 */
    REG_UNIMPLEMENTED,          /* 0x155 */
    REG_UNIMPLEMENTED,          /* 0x156 */
    REG_UNIMPLEMENTED,          /* 0x157 */
    REG_UNIMPLEMENTED,          /* 0x158 */
    REG_UNIMPLEMENTED,          /* 0x159 */
    REG_UNIMPLEMENTED,          /* 0x15a */
    REG_UNIMPLEMENTED,          /* 0x15b */
    REG_UNIMPLEMENTED,          /* 0x15c */
    REG_UNIMPLEMENTED,          /* 0x15d */
    REG_UNIMPLEMENTED,          /* 0x15e */
    REG_UNIMPLEMENTED,          /* 0x15f */
    REG_UNIMPLEMENTED,          /* 0x160 */
    REG_UNIMPLEMENTED,          /* 0x161 */
    REG_UNIMPLEMENTED,          /* 0x162 */
    REG_UNIMPLEMENTED,          /* 0x163 */
    REG_UNIMPLEMENTED,          /* 0x164 */
    REG_UNIMPLEMENTED,          /* 0x165 */
    REG_UNIMPLEMENTED,          /* 0x166 */
    REG_UNIMPLEMENTED,          /* 0x167 */
    REG_UNIMPLEMENTED,          /* 0x168 */
    REG_UNIMPLEMENTED,          /* 0x169 */
    REG_UNIMPLEMENTED,          /* 0x16a */
    REG_UNIMPLEMENTED,          /* 0x16b */
    REG_UNIMPLEMENTED,          /* 0x16c */
    REG_UNIMPLEMENTED,          /* 0x16d */
    REG_UNIMPLEMENTED,          /* 0x16e */
    REG_UNIMPLEMENTED,          /* 0x16f */
    REG_FREG,                   /* 0x170 */
    REG_FREG,                   /* 0x171 */
    REG_FREG,                   /* 0x172 */
    REG_FREG,                   /* 0x173 */
    REG_FREG,                   /* 0x174 */
    REG_FREG,                   /* 0x175 */
    REG_FREG,                   /* 0x176 */
    REG_FREG,                   /* 0x177 */
    REG_FREG,                   /* 0x178 */
    REG_FREG,                   /* 0x179 */
    REG_FREG,                   /* 0x17a */
    REG_FREG,                   /* 0x17b */
    REG_FREG,                   /* 0x17c */
    REG_FREG,                   /* 0x17d */
    REG_FREG,                   /* 0x17e */
    REG_FREG,                   /* 0x17f */

    /* BANK 3 */

    REG_INDF,                   /* 0x180 */
    REG_OPTION_REG,             /* 0x181 */
    REG_PCL,                    /* 0x182 */
    REG_STATUS,                 /* 0x183 */
    REG_FSR,                    /* 0x184 */
    REG_SRCON,                  /* 0x185 */
    REG_TRISB,                  /* 0x186 */
    REG_BAUDCTL,                /* 0x187 */
    REG_ANSEL,                  /* 0x188 */
    REG_ANSELH,                 /* 0x189 */
    REG_PCLATH,                 /* 0x18a */
    REG_INTCON,                 /* 0x18b */
    REG_EECON1,                 /* 0x18c */
    REG_EECON2,                 /* 0x18d */
    REG_RESERVED,               /* 0x18e */
    REG_RESERVED,               /* 0x18f */
    REG_UNIMPLEMENTED,          /* 0x190 */
    REG_UNIMPLEMENTED,          /* 0x191 */
    REG_UNIMPLEMENTED,          /* 0x192 */
    REG_UNIMPLEMENTED,          /* 0x193 */
    REG_UNIMPLEMENTED,          /* 0x194 */
    REG_UNIMPLEMENTED,          /* 0x195 */
    REG_UNIMPLEMENTED,          /* 0x196 */
    REG_UNIMPLEMENTED,          /* 0x197 */
    REG_UNIMPLEMENTED,          /* 0x198 */
    REG_UNIMPLEMENTED,          /* 0x199 */
    REG_UNIMPLEMENTED,          /* 0x19a */
    REG_UNIMPLEMENTED,          /* 0x19b */
    REG_UNIMPLEMENTED,          /* 0x19c */
    REG_UNIMPLEMENTED,          /* 0x19d */
    REG_UNIMPLEMENTED,          /* 0x19e */
    REG_UNIMPLEMENTED,          /* 0x19f */
    REG_UNIMPLEMENTED,          /* 0x1a0 */
    REG_UNIMPLEMENTED,          /* 0x1a1 */
    REG_UNIMPLEMENTED,          /* 0x1a2 */
    REG_UNIMPLEMENTED,          /* 0x1a3 */
    REG_UNIMPLEMENTED,          /* 0x1a4 */
    REG_UNIMPLEMENTED,          /* 0x1a5 */
    REG_UNIMPLEMENTED,          /* 0x1a6 */
    REG_UNIMPLEMENTED,          /* 0x1a7 */
    REG_UNIMPLEMENTED,          /* 0x1a8 */
    REG_UNIMPLEMENTED,          /* 0x1a9 */
    REG_UNIMPLEMENTED,          /* 0x1aa */
    REG_UNIMPLEMENTED,          /* 0x1ab */
    REG_UNIMPLEMENTED,          /* 0x1ac */
    REG_UNIMPLEMENTED,          /* 0x1ad */
    REG_UNIMPLEMENTED,          /* 0x1ae */
    REG_UNIMPLEMENTED,          /* 0x1af */
    REG_UNIMPLEMENTED,          /* 0x1b0 */
    REG_UNIMPLEMENTED,          /* 0x1b1 */
    REG_UNIMPLEMENTED,          /* 0x1b2 */
    REG_UNIMPLEMENTED,          /* 0x1b3 */
    REG_UNIMPLEMENTED,          /* 0x1b4 */
    REG_UNIMPLEMENTED,          /* 0x1b5 */
    REG_UNIMPLEMENTED,          /* 0x1b6 */
    REG_UNIMPLEMENTED,          /* 0x1b7 */
    REG_UNIMPLEMENTED,          /* 0x1b8 */
    REG_UNIMPLEMENTED,          /* 0x1b9 */
    REG_UNIMPLEMENTED,          /* 0x1ba */
    REG_UNIMPLEMENTED,          /* 0x1bb */
    REG_UNIMPLEMENTED,          /* 0x1bc */
    REG_UNIMPLEMENTED,          /* 0x1bd */
    REG_UNIMPLEMENTED,          /* 0x1be */
    REG_UNIMPLEMENTED,          /* 0x1bf */
    REG_UNIMPLEMENTED,          /* 0x1c0 */
    REG_UNIMPLEMENTED,          /* 0x1c1 */
    REG_UNIMPLEMENTED,          /* 0x1c2 */
    REG_UNIMPLEMENTED,          /* 0x1c3 */
    REG_UNIMPLEMENTED,          /* 0x1c4 */
    REG_UNIMPLEMENTED,          /* 0x1c5 */
    REG_UNIMPLEMENTED,          /* 0x1c6 */
    REG_UNIMPLEMENTED,          /* 0x1c7 */
    REG_UNIMPLEMENTED,          /* 0x1c8 */
    REG_UNIMPLEMENTED,          /* 0x1c9 */
    REG_UNIMPLEMENTED,          /* 0x1ca */
    REG_UNIMPLEMENTED,          /* 0x1cb */
    REG_UNIMPLEMENTED,          /* 0x1cc */
    REG_UNIMPLEMENTED,          /* 0x1cd */
    REG_UNIMPLEMENTED,          /* 0x1ce */
    REG_UNIMPLEMENTED,          /* 0x1cf */
    REG_UNIMPLEMENTED,          /* 0x1d0 */
    REG_UNIMPLEMENTED,          /* 0x1d1 */
    REG_UNIMPLEMENTED,          /* 0x1d2 */
    REG_UNIMPLEMENTED,          /* 0x1d3 */
    REG_UNIMPLEMENTED,          /* 0x1d4 */
    REG_UNIMPLEMENTED,          /* 0x1d5 */
    REG_UNIMPLEMENTED,          /* 0x1d6 */
    REG_UNIMPLEMENTED,          /* 0x1d7 */
    REG_UNIMPLEMENTED,          /* 0x1d8 */
    REG_UNIMPLEMENTED,          /* 0x1d9 */
    REG_UNIMPLEMENTED,          /* 0x1da */
    REG_UNIMPLEMENTED,          /* 0x1db */
    REG_UNIMPLEMENTED,          /* 0x1dc */
    REG_UNIMPLEMENTED,          /* 0x1dd */
    REG_UNIMPLEMENTED,          /* 0x1de */
    REG_UNIMPLEMENTED,          /* 0x1df */
    REG_UNIMPLEMENTED,          /* 0x1e0 */
    REG_UNIMPLEMENTED,          /* 0x1e1 */
    REG_UNIMPLEMENTED,          /* 0x1e2 */
    REG_UNIMPLEMENTED,          /* 0x1e3 */
    REG_UNIMPLEMENTED,          /* 0x1e4 */
    REG_UNIMPLEMENTED,          /* 0x1e5 */
    REG_UNIMPLEMENTED,          /* 0x1e6 */
    REG_UNIMPLEMENTED,          /* 0x1e7 */
    REG_UNIMPLEMENTED,          /* 0x1e8 */
    REG_UNIMPLEMENTED,          /* 0x1e9 */
    REG_UNIMPLEMENTED,          /* 0x1ea */
    REG_UNIMPLEMENTED,          /* 0x1eb */
    REG_UNIMPLEMENTED,          /* 0x1ec */
    REG_UNIMPLEMENTED,          /* 0x1ed */
    REG_UNIMPLEMENTED,          /* 0x1ee */
    REG_UNIMPLEMENTED,          /* 0x1ef */
    REG_FREG,                   /* 0x1f0 */
    REG_FREG,                   /* 0x1f1 */
    REG_FREG,                   /* 0x1f2 */
    REG_FREG,                   /* 0x1f3 */
    REG_FREG,                   /* 0x1f4 */
    REG_FREG,                   /* 0x1f5 */
    REG_FREG,                   /* 0x1f6 */
    REG_FREG,                   /* 0x1f7 */
    REG_FREG,                   /* 0x1f8 */
    REG_FREG,                   /* 0x1f9 */
    REG_FREG,                   /* 0x1fa */
    REG_FREG,                   /* 0x1fb */
    REG_FREG,                   /* 0x1fc */
    REG_FREG,                   /* 0x1fd */
    REG_FREG,                   /* 0x1fe */
    REG_FREG,                   /* 0x1ff */
};
// clang-format on

#endif // RZ_PIC_MIDRANGE_PIC_MEMMAP_PIC16F882_H
