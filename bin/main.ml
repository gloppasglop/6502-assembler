open Ast
module Env = Map.Make (String)

let empty_env = Env.empty

type env = int Env.t

let pp_env e = Env.iter (fun k v -> Printf.printf "Env: %s = %d\n" k v) e

let parse (s : string) : line list =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.program Lexer.read lexbuf in
  ast
;;

(* Check if operand of Absolute, AboluteX and AbsoluteY is smaller than 255
   In that case, we have a ZeroPage
   For AbsoluteY, we only have ZeroPageY for LDX and STX
*)
open Instructions

let fix_zeropage (instruction : asm_line) : asm_line =
  match instruction with
  | Instruction (inst, v) ->
    (match v with
     | Some v' ->
       (match v' with
        | Int i ->
          (match inst.addressing with
           | Absolute ->
             (match inst.mnemonic with
              | JSR | JMP -> Instruction ({ inst with addressing = Absolute }, v)
              | _ ->
                if i <= 255
                then Instruction (Instructions.get_instruction inst.mnemonic Zeropage, v)
                else Instruction ({ inst with addressing = Absolute }, v))
           | AbsoluteX ->
             if i <= 255
             then Instruction (Instructions.get_instruction inst.mnemonic ZeropageX, v)
             else Instruction (Instructions.get_instruction inst.mnemonic AbsoluteX, v)
           | AbsoluteY ->
             (match inst.mnemonic with
              | LDX | STX ->
                if i <= 255
                then Instruction (Instructions.get_instruction inst.mnemonic ZeropageY, v)
                else Instruction (Instructions.get_instruction inst.mnemonic AbsoluteY, v)
              | _ -> Instruction (Instructions.get_instruction inst.mnemonic AbsoluteY, v))
           | _ -> instruction)
        | _ -> instruction)
     | None -> Instruction (inst, v))
;;

type loc = int
type address = int
type evaluated_pgm_line = loc * address * line

let rec eval_pgm
  (environment : env)
  (evaluated_pgm : evaluated_pgm_line list)
  (pgm : line list)
  : env * evaluated_pgm_line list
  =
  match pgm with
  | [] -> environment, evaluated_pgm
  | head :: tail ->
    let environment, loc, address =
      match evaluated_pgm with
      | [] -> environment, 0, 0
      | (loc, address, line') :: _ ->
        (match line' with
         | Assign (_, _) -> environment, loc + 1, address
         | Instruction (i, _) -> environment, loc + 1, address + i.bytes
         | Label (Var l) -> Env.add l address environment, loc + 1, address)
    in
    let env', _, line = eval_asm environment head in
    eval_pgm env' ((loc, address, line) :: evaluated_pgm) tail

and eval_asm (environment : env) line =
  match line with
  | Label (Var _) -> environment, 0, line
  | Assign (s, v) ->
    let env', line' = eval_assign environment s v in
    env', 0, line'
  | Instruction (inst, Some v) ->
    let _, v' = eval_value environment v in
    let (Instruction (inst', v')) = fix_zeropage (Instruction (inst, Some v')) in
    environment, inst'.bytes, Instruction (inst', v')
  | Instruction (inst, None) -> environment, inst.bytes, line

and eval_value (environment : env) v =
  match v with
  | Int i -> environment, Int i
  | Binop (op, e1, e2) -> eval_binop environment op e1 e2
  | Unop (op, e1) -> eval_unop environment op e1
  | Var x -> eval_var environment x

and eval_var (environment : env) (x : string) : env * value_expr =
  try environment, Int (Env.find x environment) with
  | Not_found ->
    Printf.printf "Warning: Unbound variable %s\n" x;
    environment, Var x

and eval_binop env op e1 e2 =
  let _, v1 = eval_value env e1 in
  let _, v2 = eval_value env e2 in
  match op, v1, v2 with
  | Add, Int i1, Int i2 -> env, Int (i1 + i2)
  | Mult, Int i1, Int i2 -> env, Int (i1 * i2)
  | _ -> env, Binop (op, v1, v2)

and eval_unop env op e1 =
  let _, v1 = eval_value env e1 in
  match op, v1 with
  | Highbyte, Int v -> env, Int ((v land 0xFF00) lsr 8)
  | Lowbyte, Int v -> env, Int (v land 0xFF)
  | _ -> env, Unop (op, v1)

and eval_assign (environment : env) (s : identifier) (e1 : value_expr) : env * line =
  let _, e1' = eval_value environment e1 in
  match s with
  | Var sym ->
    Printf.printf "EVAL_ASSIGN %s\n" sym;
    (match e1' with
     | Int i -> Env.add sym i environment, Assign (s, e1')
     | _ -> Env.add sym 0xFFFF environment, Assign (s, e1))
;;

(*
   let () = List.iter (fun pgm -> Printf.printf "%s - %s\n" pgm (interp pgm)) pgms
*)
let test_pgm =
  {|
  NOP
  NOP
  TATA = TITI
LABEL3:
  LDA TOTO
  TOTO = $812
LABEL1
  LDA TOTO
  TITI = TOTO+TOTO
  LDA [1+TOTO+TOTO]
  ASL TITI
LABEL2  LDA #12
  JSR $FFAB
  JSR LABEL1
  LDA $AA
  LDA $ABCD
  JMP LABEL2
|}
;;

let pp_var (var : identifier) =
  match var with
  | Var name -> Printf.sprintf "%s" name
;;

let pp_value (v : value_expr) =
  match v with
  | Int i -> Printf.sprintf "%X" i
  | _ -> "Not yet evaluated"
;;

let machine_code = function
  | Instruction (inst, None) -> Some inst.opcode
  | Instruction (inst, Some v) ->
    (match v with
     | Int i ->
       if i > 0xFFFF
       then failwith "Value too large"
       else if i <= 0xFF
       then Some ((inst.opcode lsl 8) lor i)
       else
         Some
           ((inst.opcode lsl 16) lor ((i land 0xFF00) lsr 8) lor ((i land 0x00FF) lsl 8))
     | _ -> None)
;;

let pp_machine_code = function
  | Some i ->
    if i <= 0xFF
    then Printf.sprintf "%02X" i
    else if i <= 0xFFFF
    then Printf.sprintf "%02X %02X" ((i land 0xFF00) lsr 8) (i land 0x00FF)
    else
      Printf.sprintf
        "%02X %02X %02X"
        ((i land 0xFF0000) lsr 16)
        ((i land 0x00FF00) lsr 8)
        (i land 0x0000FF)
  | None -> "Impossible"
;;

let pp_line = function
  | loc, address, Label (Var l) ->
    Printf.sprintf "%4d  %04X               %s" loc address l
  | loc, _, Assign (var, v) ->
    Printf.sprintf
      "%4d                                  %s = %s"
      loc
      (pp_var var)
      (pp_value v)
  | loc, address, Instruction (inst, Some v) ->
    let mcode = pp_machine_code (machine_code (Instruction (inst, Some v))) in
    (match inst.addressing with
     | Immediate ->
       Printf.sprintf
         "%4d  %04X  %9s                 %s #%s"
         loc
         address
         mcode
         inst.pp
         (pp_value v)
     | Accumulator -> Printf.sprintf "IMPOSSIBLE %s" inst.pp
     | Absolute ->
       Printf.sprintf
         "%4d  %04X  %9s                 %s %s"
         loc
         address
         mcode
         inst.pp
         (pp_value v)
     | AbsoluteX ->
       Printf.sprintf
         "%4d  %04X  %9s                 %s %s,X"
         loc
         address
         mcode
         inst.pp
         (pp_value v)
     | AbsoluteY ->
       Printf.sprintf
         "%4d  %04X  %9s                 %s %s,Y"
         loc
         address
         mcode
         inst.pp
         (pp_value v)
     | Zeropage ->
       Printf.sprintf
         "%4d  %04X  %9s                 %s %s"
         loc
         address
         mcode
         inst.pp
         (pp_value v)
     | ZeropageX ->
       Printf.sprintf
         "%4d  %04X  %9s                 %s %s,X"
         loc
         address
         mcode
         inst.pp
         (pp_value v)
     | ZeropageY ->
       Printf.sprintf
         "%4d  %04X  %9s                 %s %s,Y"
         loc
         address
         mcode
         inst.pp
         (pp_value v)
     | PreIndexIndirect ->
       Printf.sprintf
         "%4d  %04X  %9s                 %s (%s,X)"
         loc
         address
         mcode
         inst.pp
         (pp_value v)
     | PostIndexIndirect ->
       Printf.sprintf
         "%4d  %04X  %9s                 %s (%s),Y"
         loc
         address
         mcode
         inst.pp
         (pp_value v)
     | Indirect ->
       Printf.sprintf
         "%4d  %04X  %9s                 %s (%s)"
         loc
         address
         mcode
         inst.pp
         (pp_value v)
     | Relative ->
       Printf.sprintf
         "%4d  %04X  %9s                 %s %s"
         loc
         address
         mcode
         inst.pp
         (pp_value v)
     | Implied -> Printf.sprintf "IMPOSSIBLE %s" inst.pp)
  | loc, address, Instruction (inst, None) ->
    let mcode = pp_machine_code (machine_code (Instruction (inst, None))) in
    (match inst.addressing with
     | Immediate -> Printf.sprintf "IMPOSSIBLE %s %s" inst.pp "Immediate"
     | Accumulator ->
       Printf.sprintf "%4d  %04X  %9s                 %s" loc address mcode inst.pp
     | Absolute -> Printf.sprintf "IMPOSSIBLE %s %s" inst.pp "Absolute"
     | AbsoluteX -> Printf.sprintf "IMPOSSIBLE %s %s,X" inst.pp "AbsoluteX"
     | AbsoluteY -> Printf.sprintf "IMPOSSIBLE %s %s,Y" inst.pp "AbsoluteY"
     | Zeropage -> Printf.sprintf "IMPOSSIBLE %s %s" inst.pp "ZeroPage"
     | ZeropageX -> Printf.sprintf "IMPOSSIBLE %s %s,X" inst.pp "ZeroPageX"
     | ZeropageY -> Printf.sprintf "IMPOSSIBLE %s %s,Y" inst.pp "ZeroPageY"
     | PreIndexIndirect ->
       Printf.sprintf "IMPOSSIBLE %s (%s,X)" inst.pp "PreIndexIndirect"
     | PostIndexIndirect ->
       Printf.sprintf "IMPOSSIBLE %s (%s),Y" inst.pp "PostIndexIndirect"
     | Indirect -> Printf.sprintf "IMPOSSIBLE %s (%s)" inst.pp "Indirect"
     | Relative -> Printf.sprintf "IMPOSSIBLE %s %s" inst.pp "Relative"
     | Implied ->
       Printf.sprintf "%4d  %04X  %9s                 %s" loc address mcode inst.pp)
;;

let pp_result = function
  | env, pgm ->
    List.iter (fun line -> Printf.printf "%s\n" (pp_line line)) pgm;
    pp_env env
;;

let interp environment s = s |> parse |> eval_pgm environment []
let environment, first_pass = interp empty_env test_pgm
let () = pp_result (environment, List.rev first_pass)

(* TODO : Use different types for values *)
let fixed_pgm =
  List.map
    (fun (eval_line : evaluated_pgm_line) : line ->
      match eval_line with
      | _, _, line ->
        (match line with
         | Label _ -> line
         | Instruction (inst, v) ->
           let (Instruction (inst', v')) = fix_zeropage (Instruction (inst, v)) in
           Instruction (inst', v')
         | Assign (_, _) -> line))
    first_pass
;;

let environment, second_pass = eval_pgm environment [] (List.rev fixed_pgm)
let () = pp_result (environment, List.rev second_pass)
