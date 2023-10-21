open Ast
open Stdio
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

let fix_zeropage (instruction : asm_line) : instruction * value_expr option =
  match instruction with
  | Instruction (inst, v) ->
    (match v with
     | Some v' ->
       (match v' with
        | Int i ->
          (match inst.addressing with
           | Absolute ->
             (match inst.mnemonic with
              | JSR | JMP -> { inst with addressing = Absolute }, v
              | _ ->
                if i <= 255
                then Instructions.get_instruction inst.mnemonic Zeropage, v
                else { inst with addressing = Absolute }, v)
           | AbsoluteX ->
             if i <= 255
             then Instructions.get_instruction inst.mnemonic ZeropageX, v
             else Instructions.get_instruction inst.mnemonic AbsoluteX, v
           | AbsoluteY ->
             (match inst.mnemonic with
              | LDX | STX ->
                if i <= 255
                then Instructions.get_instruction inst.mnemonic ZeropageY, v
                else Instructions.get_instruction inst.mnemonic AbsoluteY, v
              | _ -> Instructions.get_instruction inst.mnemonic AbsoluteY, v)
           | _ -> inst, v)
        | _ -> inst, v)
     | None -> inst, v)
;;

type loc = int
type address = int
type evaluated_pgm_line = loc * address * line

type pass =
  | First
  | Second

let rec eval_pgm
  (pass : pass)
  (environment : env)
  (loc : loc)
  (address : address)
  (evaluated_pgm : evaluated_pgm_line list)
  (pgm : line list)
  : env * evaluated_pgm_line list
  =
  match pgm with
  | [] -> environment, evaluated_pgm
  | head :: tail ->
    let environment', offset, line = eval_asm pass environment address head in
    eval_pgm
      pass
      environment'
      (loc + 1)
      (address + offset)
      ((loc, address, line) :: evaluated_pgm)
      tail

and eval_asm pass (environment : env) address line =
  match line with
  | Label (Var l) -> Env.add l address environment, 0, line
  | Assign (s, v) ->
    let env', line' = eval_assign environment s v in
    let pc =
      match line' with
      | Assign (_, Int pc') -> pc'
      | _ -> failwith "Impossible: Should be Assign"
    in
    env', pc - address, line'
  | Bytes l -> environment, List.length l, line
  | Instruction (inst, sv) ->
    (match sv with
     | Some v ->
       let _, v' = eval_operand_value environment v in
       let inst', v'' =
         match inst.addressing with
         | Accumulator | Implied ->
           failwith "Accumulator or Implied should not have operand"
         (* two bytes operand *)
         | Absolute -> fix_zeropage (Instruction (inst, Some v'))
         | AbsoluteX -> fix_zeropage (Instruction (inst, Some v'))
         | AbsoluteY -> fix_zeropage (Instruction (inst, Some v'))
         | Indirect -> inst, Some v'
         (* Single byte operand *)
         | Immediate -> inst, Some v'
         | Zeropage -> inst, Some v'
         | ZeropageX -> inst, Some v'
         | ZeropageY -> inst, Some v'
         | PreIndexIndirect -> inst, Some v'
         | PostIndexIndirect -> inst, Some v'
         | Relative ->
           (match pass with
            | First ->
              (match v' with
               (* Check relative jump lower than +/- 127 *)
               | Int i -> inst, Some (Int ((i - address - 2) land 0xFF))
               | _ -> inst, Some v')
            | Second ->
              (match v with
               | Int _ -> inst, Some v
               | _ ->
                 (match v' with
                  | Int i -> inst, Some (Int ((i - address - 2) land 0xFF))
                  | _ -> failwith "Should already be avaluated to a value")))
       in
       environment, inst'.bytes, Instruction (inst', v'')
     | None -> environment, inst.bytes, line)

and eval_operand_value (environment : env) v =
  let tmp_expr = eval_value environment v in
  match tmp_expr with
  | _, Int i -> if i < 0 then environment, Int (i land 0xFFFF) else tmp_expr
  | _, _ -> tmp_expr

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
  | Minus, Int i1, Int i2 -> env, Int (i1 - i2)
  | Mult, Int i1, Int i2 -> env, Int (i1 * i2)
  | Div, Int i1, Int i2 -> env, Int (i1 / i2)
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
  | loc, address, Bytes l ->
    let bytes_as_string = List.map (fun i -> Printf.sprintf "%02X " i) l in
    let pp_bytes = List.fold_left ( ^ ) "" bytes_as_string in
    Printf.sprintf "%4d  %04X      %s" loc address pp_bytes
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
       Printf.sprintf "%4d  %04X  %6s                    %s" loc address mcode inst.pp
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
       Printf.sprintf "%4d  %04X  %6s                    %s" loc address mcode inst.pp)
;;

let pp_result = function
  | env, pgm ->
    List.iter (fun line -> Printf.printf "%s\n" (pp_line line)) pgm;
    pp_env env
;;

let value_to_int_list = function
  | Int i ->
    let i' = 0xFFFF land i in
    if i' <= 0xFF then [ i' ] else [ i' land 0xFF; (i' land 0xFF00) lsr 8 ]
  | _ -> failwith "Impossible"
;;

let line_to_bytes (line : address * address * line) : (int * int list) option =
  match line with
  | _, address, Bytes l -> Some (address, l)
  | _, _, Label _ -> None
  | _, _, Assign _ -> None
  | _, address, Instruction (inst, Some v) ->
    (match inst.addressing with
     | Accumulator | Implied -> failwith "Accumulator Impossible"
     | _ -> Some (address, inst.opcode :: value_to_int_list v))
  | _, address, Instruction (inst, None) ->
    (match inst.addressing with
     | Accumulator | Implied -> Some (address, [ inst.opcode ])
     | _ -> failwith "Impossible")
;;

let compile pgm = List.map line_to_bytes pgm
let usage_msg = "append [-verbose] <file1> [<file2>] ... -o <output>"
let input_file = ref ""
let output_file = ref ""
let origin = ref 0
let anon_fun _ = ()

let speclist =
  [ "-f", Arg.Set_string input_file, "Input file name"
  ; "-o", Arg.Set_int origin, "Origin"
  ; "-s", Arg.Set_string output_file, "Output file name"
  ]
;;

let () = Arg.parse speclist anon_fun usage_msg
let () = Printf.printf "Program: %s\n" !input_file
let pgm = In_channel.read_all !input_file
let interp environment s = s |> parse |> eval_pgm First environment 0 !origin []
let environment, first_pass = interp empty_env pgm
let () = pp_result (environment, List.rev first_pass)

(* TODO : Use different types for values *)
let fixed_pgm =
  List.map
    (fun (eval_line : evaluated_pgm_line) : line ->
      match eval_line with
      | _, _, line ->
        (match line with
         | Bytes _ -> line
         | Label _ -> line
         | Instruction (inst, v) ->
           Instruction (inst, v)
           (*
              let (Instruction (inst', v')) = fix_zeropage (Instruction (inst, v)) in
              Instruction (inst', v')
           *)
         | Assign (_, _) -> line))
    first_pass
;;

let environment, second_pass =
  eval_pgm Second environment 0 !origin [] (List.rev fixed_pgm)
;;

let () = pp_result (environment, List.rev second_pass)
let compiled = List.filter_map (fun x -> x) (compile (List.rev second_pass))

(* merge consecutive bytes *)
let merge pgm =
  let rec merge_aux acc pgm =
    match pgm with
    | [] -> acc
    | (current_addr, current_bytes) :: rest_pgm ->
      (match acc with
       | [] -> merge_aux [ current_addr, current_bytes ] rest_pgm
       | (previous_addr, previous_bytes) :: rest_acc ->
         if current_addr = previous_addr + List.length previous_bytes
         then
           merge_aux
             ((previous_addr, previous_bytes @ current_bytes) :: rest_acc)
             rest_pgm
         else merge_aux ((current_addr, current_bytes) :: acc) rest_pgm)
  in
  merge_aux [] pgm
;;

let () =
  List.iter
    (fun l ->
      match l with
      | addr, bytes ->
        let bytes_str =
          List.fold_left (fun acc s -> acc ^ Printf.sprintf "%02X" s) "" bytes
        in
        Printf.printf "%04X %s\n" addr bytes_str)
    (merge compiled |> List.rev)
;;

let outc = Out_channel.create ~append:false !output_file

let () =
  List.iter
    (fun hex_record ->
      Printf.fprintf outc "%s\n" (Intelhex.string_of_hex_record hex_record))
    (Intelhex.mem_to_hex 16 (merge compiled))
;;

let () = Out_channel.close outc
