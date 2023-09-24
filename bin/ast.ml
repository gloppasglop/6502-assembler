type bop =
  | Add
  | Mult

type unop =
  | Highbyte
  | Lowbyte

type identifier = Var of string

type value_expr =
  | Int of int
  | Binop of bop * value_expr * value_expr
  | Unop of unop * value_expr
  | Var of string

(*
   type operand =
   | Var of string
   | Operand of value_expr
*)

open Instructions

type instruction = Instructions.instruction

type line =
  | Assign of identifier * value_expr
  | Instruction of instruction * value_expr option
  | Label of identifier

type asm_line = Instruction of instruction * value_expr option
