type bop =
  | Add
  | Mult

type unop =
  | Highbyte
  | Lowbyte

type symbol = Symbol of string

type value_expr =
  | Int of int
  | Binop of bop * value_expr * value_expr
  | Unop of unop * value_expr

(*
   type value_expr =
   | Arithm of arithm_expr
   | Unop of unop * arithm_expr
*)

type mnemonic =
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

type opcode = int
type loc = Lexing.position

open Instructions

type instruction = Instructions.instruction

type asm_expr =
  | Instruction of loc * instruction * value_expr option
  | Assign of symbol * value_expr
  | Label of string
