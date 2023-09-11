open Ast
open Instructions

let parse (s : string) : asm_expr list =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.program Lexer.read lexbuf in
  ast
;;

(* TODO: Do not generate exception*)
let is_value e =
  match e with
  | Int i -> if i <= 0xFFFF then true else failwith "Integer should be 32bits"
  | Binop _ -> false
  | Unop _ -> false
;;

(*
   let is_asm_expr_value e =
   match e with
   | Instruction (_, _, v) ->
   (match v with
   | Some v -> is_value v
   | None -> true)
   | Assign (_, v) -> is_value v
   ;;
*)

let pp_addressing_mode a i =
  match a with
  | Instructions.Accumulator -> Printf.sprintf "A ; ACCUMULATOR"
  | Implied -> Printf.sprintf " ; IMPLIED"
  | Immediate -> Printf.sprintf "#$%X ; IMMEDIATE" i
  | Absolute -> Printf.sprintf "$%X ; ABSOLUTE" i
  | Zeropage -> Printf.sprintf "$%X ; ZEROPAGE" i
  | AbsoluteX -> Printf.sprintf "$%X,X ; ABSOLUTE,X" i
  | ZeropageX -> Printf.sprintf "$%X,X ; ZEROPAGE,X" i
  | AbsoluteY -> Printf.sprintf "$%X,Y ; ABSOLUTE,Y" i
  | ZeropageY -> Printf.sprintf "$%X,Y ; ZEROPAGE,Y" i
  | PreIndexIndirect -> Printf.sprintf "($%X,X) ; (INDIRECT,X)" i
  | PostIndexIndirect -> Printf.sprintf "($%X),Y; (INDIRECT),Y" i
  (* | Implied -> Printf.sprintf " ; IMPLIED" *)
  | Indirect -> Printf.sprintf "($%X) ; INDIRECT" i
  | Relative -> Printf.sprintf "$%X ; RELATIVE" i
;;

let pp_instruction (i : Instructions.instruction) : string = i.mnemonic

let int_of_val e =
  match e with
  | Int i -> i land 0xFFFF
  | Binop _ -> failwith "Got Binop. Must be a value"
  | Unop _ -> failwith "Got Unop. Must be a value"
;;

(* TODO: Cleanup, should not return 0 for None*)
let int_of_opt_val e =
  match e with
  | Some v -> int_of_val v
  | None -> 0
;;

(* TODO: Move all pp_ into instructions module*)
let string_of_instr inst =
  match inst with
  | Instruction (loc, inst, v) ->
    Printf.sprintf
      "(%d) %s %s"
      loc.pos_lnum
      (pp_instruction inst)
      (pp_addressing_mode inst.addressing (int_of_opt_val v))
  | Assign (Symbol id, v) -> Printf.sprintf "%s = $%X\n" id (int_of_val v)
  | Label l -> Printf.sprintf "%s:\n" l
;;

let rec step_value e =
  match e with
  | Int _ -> failwith "No more steps"
  | Binop (op, e1, e2) when is_value e1 && is_value e2 -> step_binop op e1 e2
  | Binop (op, e1, e2) when is_value e1 -> Binop (op, e1, step_value e2)
  | Binop (op, e1, e2) -> Binop (op, step_value e1, e2)
  | Unop (op, e) when is_value e -> step_unop op e
  | Unop (op, e) -> Unop (op, step_value e)

and step_binop bop v1 v2 =
  match bop, v1, v2 with
  | Add, Int i1, Int i2 -> Int (i1 + i2)
  | Add, _, _ -> failwith "Step add add should only work with values"
  | Mult, Int i1, Int i2 -> Int (i1 * i2)
  | Mult, _, _ -> failwith "Step add add should only work with values"

and step_unop op v =
  match op, v with
  | Highbyte, Int i -> Int ((i land 0xFF00) lsr 8)
  | Highbyte, _ -> failwith "Step add add should only work with values"
  | Lowbyte, Int i -> Int (i land 0xFF)
  | Lowbyte, _ -> failwith "Step add add should only work with values"
;;

(* Check if operand of Absolute, AboluteX and AbsoluteY is smaller than 255
   In that case, we have a ZeroPage
   For AbsoluteY, we only have ZeroPageY for LDX and STX
*)
let fix_zeropage instruction =
  match instruction with
  | Instruction (loc, inst, v) ->
    (match v with
     | Some i ->
       if is_value i
       then (
         match inst.addressing with
         | Absolute ->
           if int_of_opt_val v <= 255
           then Instruction (loc, { inst with addressing = Zeropage }, v)
           else Instruction (loc, { inst with addressing = Absolute }, v)
         | AbsoluteX ->
           if int_of_opt_val v <= 255
           then Instruction (loc, { inst with addressing = ZeropageX }, v)
           else Instruction (loc, { inst with addressing = AbsoluteX }, v)
         | AbsoluteY ->
           (match inst.mnemonic with
            | "LDX" | "STX" ->
              if int_of_opt_val v <= 255
              then Instruction (loc, { inst with addressing = ZeropageY }, v)
              else Instruction (loc, { inst with addressing = AbsoluteY }, v)
            | _ -> Instruction (loc, { inst with addressing = AbsoluteY }, v))
         | _ -> Instruction (loc, inst, v))
       else Instruction (loc, inst, v)
     | None -> Instruction (loc, inst, v))
  | _ -> failwith "Impossible"
;;

let rec step e =
  match e with
  | Instruction (loc, inst, Some v) ->
    if is_value v
    then fix_zeropage e
    else step (Instruction (loc, inst, Some (step_value v)))
  | Instruction (_, _, None) -> e
  | Assign (s, v) -> if is_value v then e else step (Assign (s, step_value v))
  | Label _ -> e
;;

let eval e = string_of_instr e

let interp s =
  let exprs = s |> parse in
  List.map (fun expr -> expr |> step |> eval) exprs
;;

let pgm =
  {|
    ; TEsting NOP
    NOP
    LDA #12
    LDA #0b11111111
    LDA #$12
    LDA $12
    LDA $1212
    LDA $12,X
    LDA $1212,X
    LDA $12,Y
    LDA $1212,Y
    LDA ($12,X)
    LDA ($12),Y
    ADC #12
    ADC #0b11111111
    ADC #$12
    ADC $12
    ADC $1212
    ADC $12,X
    ADC $1212,X
    ADC $12,Y
    ADC $1212,Y
    ADC ($12,X)
    ADC ($12),Y
    AND #12
    AND #0b11111111
    AND #$12
    AND $12
    AND $1212
    AND $12,X
    AND $1212,X
    AND $12,Y
    AND $1212,Y
    AND ($12,X)
    AND ($12),Y
    ASL
    ASL A
    ASL $12
    ASL $1212
    ASL $12,X
    ASL $1212,X
    BCC $1212
    BCS $1212
    BEQ $1212
    BIT $12
    BIT $1212
    BMI $1212
    BNE $1212
    BPL $1212
    BRK
    BVC $1212
    BVS $1212
    CLC
    CLD
    CLI
    CLV
    CMP #12
    CMP #0b11111111
    CMP #$12
    CMP $12
    CMP $1212
    CMP $12,X
    CMP $1212,X
    CMP $12,Y
    CMP $1212,Y
    CMP ($12,X)
    CMP ($12),Y
    CPX #$12
    CPX $12
    CPX $1212
    CPY #$12
    CPY $12
    CPY $1212
    DEC $12
    DEC $1212
    DEC $12,X
    DEC $1212,X
    DEX
    DEY
    EOR #12
    EOR #0b11111111
    EOR #$12
    EOR $12
    EOR $1212
    EOR $12,X
    EOR $1212,X
    EOR $12,Y
    EOR $1212,Y
    EOR ($12,X)
    EOR ($12),Y
    INC $12
    INC $1212
    INC $12,X
    INC $1212,X
    INX
    INY
    JMP $1212
    JMP ($1212)
    JSR $1212
    LDX #12
    LDX #0b11111111
    LDX #$12
    LDX $12
    LDX $1212
    LDX $12,Y
    LDX $1212,Y
    LDY #12
    LDY #0b11111111
    LDY #$12
    LDY $12
    LDY $1212
    LDY $12,X
    LDY $1212,X
    LSR
    LSR A
    LSR $12
    LSR $1212
    LSR $12,X
    LSR $1212,X
    ORA #12
    ORA #0b11111111
    ORA #$12
    ORA $12
    ORA $1212
    ORA $12,X
    ORA $1212,X
    ORA $12,Y
    ORA $1212,Y
    ORA ($12,X)
    ORA ($12),Y
    PHA
    PHP
    PLA
    PLP
    ROL
    ROL A
    ROL $12
    ROL $1212
    ROL $12,X
    ROL $1212,X
    ROR
    ROR A
    ROR $12
    ROR $1212
    ROR $12,X
    ROR $1212,X
    RTI
    RTS
    SBC #12
    SBC #0b11111111
    SBC #$12
    SBC $12
    SBC $1212
    SBC $12,X
    SBC $1212,X
    SBC $12,Y
    SBC $1212,Y
    SBC ($12,X)
    SBC ($12),Y
    SEC
    SED
    SEI
    STA #12
    STA #0b11111111
    STA #$12
    STA $12
    STA $1212
    STA $12,X
    STA $1212,X
    STA $12,Y
    STA $1212,Y
    STA ($12,X)
    STA ($12),Y
    STX $12
    STX $1212
    STX $12,Y
    STX $1212,Y
    STY $12
    STY $1212
    STY $12,X
    STY $1212,X
    TAX
    TAY
    TSX
    TXA
    TXS
    TYA
    LDA #[1+2]
    LDA #1+2
    LDA $1+12
    LDA $1+$12
    LDA $A+$12
    LDA $A+12
    LDA ([12+13],X)
    JMP [1200+$12]
    JMP [1200+12]
    JMP [$1200+12]
    JMP ($1212)
    JMP ([$1200+$12])
    LDA #<$12EF
    LDA #>$AABB
    LDA #>$FFFF
    LDA [2*4+3]
    TOTO = $1212
    TOTO = $12+$12
    TOTO = [2*4+3]
    TOTO = 3*2+2
    TOTO = 3+2*2
LABEL1:
  LDA $1212
LABEL2
  JMP ($FFAA)
LABEL3: NOP
LABEL4  PHP
LABEL5: TOTO = 1+1
LDA TOTO
|}
;;

(*
   let () = List.iter (fun pgm -> Printf.printf "%s - %s\n" pgm (interp pgm)) pgms
*)
let test_pgm =
  {|
      NOP
      ; Comment
      ; dsdsd
      TOTO:
      LDA $12
      NOP
      NOP
|}
;;

let () =
  let interpreted_pgm = interp test_pgm in
  List.iter (Printf.printf "%s\n") interpreted_pgm
;;

let () =
  let interpreted_pgm = interp pgm in
  List.iter (Printf.printf "%s\n") interpreted_pgm
;;
