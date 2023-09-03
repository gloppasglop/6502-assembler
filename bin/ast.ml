type bop =
  | Add
  | Mult

type address =
  | Accumulator
  | Implied
  | Immediate of int
  | Absolute of int
  | Zeropage of int
  | AbsoluteX of int
  | AbsoluteY of int
  | ZeropageX of int
  | ZeropageY of int
  | Indirect of int
  | PreIndexIndirect of int (* X-indexed, indirect.*)
  | PostIndexIndirect of int (* indirect, Y-indexed.*)
  | Relative of int

type instruction =
  | Adc
  | And
  | Asl
  | Bcc
  | Bcs
  | Beq
  | Bit
  | Bmi
  | Bne
  | Bpl
  | Brk
  | Bvc
  | Bvs
  | Clc
  | Cld
  | Cli
  | Clv
  | Cmp
  | Cpx
  | Cpy
  | Dec
  | Dex
  | Dey
  | Eor
  | Inc
  | Inx
  | Iny
  | Jmp
  | Jsr
  | Lda
  | Ldx
  | Ldy
  | Lsr
  | Nop
  | Ora
  | Pha
  | Php
  | Pla
  | Plp
  | Rol
  | Ror
  | Rti
  | Rts
  | Sbc
  | Sec
  | Sed
  | Sei
  | Sta
  | Stx
  | Sty
  | Tax
  | Tay
  | Tsx
  | Txa
  | Txs
  | Tya

type expr =
  | Int of int
  | Instruction of instruction * address
  | Binop of bop * expr * expr
