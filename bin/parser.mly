%{
open Instructions
open Ast
%}

%token <int> INT
%token <string> ID
%token DCB
(*
%token <int> BYTE
%token <int> ADDRESS
*)
%token PLUS
%token MINUS
%token MULT 
%token DIV 
%token EQUAL
%token COLON
%token LPAREN
%token RPAREN
%token LT
%token GT
%token LBRACKET
%token RBRACKET
%token ADC
%token AND
%token ASL
%token BCC
%token BCS
%token BEQ
%token BIT
%token BMI
%token BNE
%token BPL
%token BRK
%token BVC
%token BVS
%token CLC
%token CLD
%token CLI
%token CLV
%token CMP
%token CPX
%token CPY
%token DEC
%token DEX
%token DEY
%token EOR
%token INC
%token INX
%token INY
%token JMP
%token JSR
%token LDA
%token LDX
%token LDY
%token LSR
%token NOP
%token ORA
%token PHA
%token PHP
%token PLA
%token PLP
%token ROL
%token ROR
%token RTI
%token RTS
%token SBC
%token SEC
%token SED
%token SEI
%token STA
%token STX
%token STY
%token TAX
%token TAY
%token TSX
%token TXA
%token TXS
%token TYA
%token POUND
%token COMMA
%token X
%token Y
%token A
%token EOF


%left PLUS MINUS
%left MULT DIV
%nonassoc GT LT

%nonassoc POUND
%nonassoc ADC AND ASL BCC BCS BEQ BIT BPL BVS CPX CPY DEC BVC
%nonassoc EOR INC JMP JSR LDA LDX LDY STA STX STY BMI BNE LSR
%nonassoc ORA ROR ROL CMP SBC 
%nonassoc unary_mult


%start program

%type <Ast.line list> program
%type <Ast.identifier> identifier
%type <Ast.value_expr> value_expr
(*)
%type <Ast.asm_expr> asm_expr
%type <Ast.arithm_expr> arithm_expr
*)

%%

program:
	| p = line*; EOF { p }
	;

line:
	| d = definition { d }
	| s = statement { s }
	| id = ID { Label (Var id ) }
	| id = ID; COLON { Label (Var id ) }
	| DCB; l = separated_list(COMMA,INT) { Bytes l }
	;

identifier:
| id = ID { Var id }

definition:
  | MULT EQUAL  v = value_expr %prec unary_mult { Assign ( Var "__PC__", v ) } 
  | id = ID  EQUAL  v = value_expr %prec unary_mult { Assign ( Var id, v ) }
	;

%inline binop:
| PLUS { Add }
| MINUS { Minus }
| MULT { Mult }
| DIV { Div }

%inline unop:
| GT { Highbyte }
| LT { Lowbyte }

value_expr: 
	| i = INT { Int i } 
	| id = ID { Var id }
	| e1 = value_expr; op=binop ; e2 = value_expr { Binop (op, e1, e2)}
	| op = unop; e = value_expr  { Unop ( op, e)}
	| LBRACKET ; e = value_expr ; RBRACKET { e } 
	;


statement: 
	| a = asm_expr { a }

asm_expr:
	(* Accumulator *)
	(*)
	| ASL; { Instruction( Instructions.asl_acc, None)}
	| LSR { Instruction( Instructions.lsr_acc, None)}
	| ROL { Instruction( Instructions.rol_acc, None)}
	| ROR { Instruction( Instructions.ror_acc, None)}
	*)
	| ASL; A { Instruction( Instructions.get_instruction ASL Accumulator,None)}
	| LSR; A { Instruction( Instructions.get_instruction LSR Accumulator, None)}
	| ROL; A { Instruction( Instructions.get_instruction ROL Accumulator, None)}
	| ROR; A { Instruction( Instructions.get_instruction ROR Accumulator, None)}
	(* Immediate *)
	| ADC; POUND; v = value_expr { Instruction( Instructions.get_instruction ADC Immediate, Some v) }
	| AND; POUND; v = value_expr { Instruction( Instructions.get_instruction AND Immediate, Some v) }
	| CMP; POUND; v = value_expr { Instruction( Instructions.get_instruction CMP Immediate, Some v) }
	| CPX; POUND; v = value_expr { Instruction( Instructions.get_instruction CPX Immediate, Some v) }
	| CPY; POUND; v = value_expr { Instruction( Instructions.get_instruction CPY Immediate, Some v) }
	| EOR; POUND; v = value_expr { Instruction( Instructions.get_instruction EOR Immediate, Some v) }
	| LDA; POUND; v = value_expr { Instruction( Instructions.get_instruction LDA Immediate, Some v) }
	| LDX; POUND; v = value_expr { Instruction( Instructions.get_instruction LDX Immediate, Some v) }
	| LDY; POUND; v = value_expr { Instruction( Instructions.get_instruction LDY Immediate, Some v) }
	| ORA; POUND; v = value_expr { Instruction( Instructions.get_instruction ORA Immediate, Some v) }
	| SBC; POUND; v = value_expr { Instruction( Instructions.get_instruction SBC Immediate, Some v) }
	(* Absolute *)
	| JMP; v = value_expr { Instruction( Instructions.get_instruction JMP Absolute, Some v)}
	| JSR; v = value_expr { Instruction( Instructions.get_instruction JSR Absolute, Some v)}
	(* Absolute or Zero Page*)
	| ADC; v = value_expr { Instruction( Instructions.get_instruction ADC Absolute, Some v)}
	| AND; v = value_expr { Instruction( Instructions.get_instruction AND Absolute, Some v)}
	| ASL; v = value_expr { Instruction( Instructions.get_instruction ASL Absolute, Some v)}
	| BIT; v = value_expr { Instruction( Instructions.get_instruction BIT Absolute, Some v)}
	| CMP; v = value_expr { Instruction( Instructions.get_instruction CMP Absolute, Some v)}
	| CPX; v = value_expr { Instruction( Instructions.get_instruction CPX Absolute, Some v)}
	| CPY; v = value_expr { Instruction( Instructions.get_instruction CPY Absolute, Some v)}
	| DEC; v = value_expr { Instruction( Instructions.get_instruction DEC Absolute, Some v)}
	| EOR; v = value_expr { Instruction( Instructions.get_instruction EOR Absolute, Some v)}
	| INC; v = value_expr { Instruction( Instructions.get_instruction INC Absolute, Some v)}
	| LDA; v = value_expr { Instruction( Instructions.get_instruction LDA Absolute, Some v)}
	| LDX; v = value_expr { Instruction( Instructions.get_instruction LDX Absolute, Some v)}
	| LDY; v = value_expr { Instruction( Instructions.get_instruction LDY Absolute, Some v)}
	| LSR; v = value_expr { Instruction( Instructions.get_instruction LSR Absolute, Some v)}
	| ORA; v = value_expr { Instruction( Instructions.get_instruction ORA Absolute, Some v)}
	| ROL; v = value_expr { Instruction( Instructions.get_instruction ROL Absolute, Some v)}
	| ROR; v = value_expr { Instruction( Instructions.get_instruction ROR Absolute, Some v)}
	| SBC; v = value_expr { Instruction( Instructions.get_instruction SBC Absolute, Some v)}
	| STA; v = value_expr { Instruction( Instructions.get_instruction STA Absolute, Some v)}
	| STX; v = value_expr { Instruction( Instructions.get_instruction STX Absolute, Some v)}
	| STY; v = value_expr { Instruction( Instructions.get_instruction STY Absolute, Some v)}
	(* zeropageX *)
	(* TODO: Check how to raise error during parsing if value > 255*)
	| STY; v = value_expr; COMMA; X { Instruction( Instructions.get_instruction STY ZeropageX, Some v) }
	(* AbsoluteX or zeropage,X *)
	| ADC; v = value_expr; COMMA; X { Instruction( Instructions.get_instruction ADC AbsoluteX, Some v) }
	| AND; v = value_expr; COMMA; X { Instruction( Instructions.get_instruction AND AbsoluteX, Some v) }
	| ASL; v = value_expr; COMMA; X { Instruction( Instructions.get_instruction ASL AbsoluteX, Some v) }
	| CMP; v = value_expr; COMMA; X { Instruction( Instructions.get_instruction CMP AbsoluteX, Some v) }
	| DEC; v = value_expr; COMMA; X { Instruction( Instructions.get_instruction DEC AbsoluteX, Some v) }
	| EOR; v = value_expr; COMMA; X { Instruction( Instructions.get_instruction EOR AbsoluteX, Some v) }
	| INC; v = value_expr; COMMA; X { Instruction( Instructions.get_instruction INC AbsoluteX, Some v) }
	| LDA; v = value_expr; COMMA; X { Instruction( Instructions.get_instruction LDA AbsoluteX, Some v) }
	| LDY; v = value_expr; COMMA; X { Instruction( Instructions.get_instruction LDY AbsoluteX, Some v) }
	| LSR; v = value_expr; COMMA; X { Instruction( Instructions.get_instruction LSR AbsoluteX, Some v) }
	| ORA; v = value_expr; COMMA; X { Instruction( Instructions.get_instruction ORA AbsoluteX, Some v) }
	| ROL; v = value_expr; COMMA; X { Instruction( Instructions.get_instruction ROL AbsoluteX, Some v) }
	| ROR; v = value_expr; COMMA; X { Instruction( Instructions.get_instruction ROR AbsoluteX, Some v) }
	| SBC; v = value_expr; COMMA; X { Instruction( Instructions.get_instruction SBC AbsoluteX, Some v) }
	| STA; v = value_expr; COMMA; X { Instruction( Instructions.get_instruction STA AbsoluteX, Some v) }
	(* zeropageY *)
	(* TODO: Check how to raise error during parsing if value > 255*)
	(* AbsoluteY *)
	| LDA; v = value_expr; COMMA; Y { Instruction( Instructions.get_instruction LDA AbsoluteY, Some v) }
	| ADC; v = value_expr; COMMA; Y { Instruction( Instructions.get_instruction ADC AbsoluteY, Some v) }
	| AND; v = value_expr; COMMA; Y { Instruction( Instructions.get_instruction AND AbsoluteY, Some v) }
	| CMP; v = value_expr; COMMA; Y { Instruction( Instructions.get_instruction CMP AbsoluteY, Some v) }
	| EOR; v = value_expr; COMMA; Y { Instruction( Instructions.get_instruction EOR AbsoluteY, Some v) }
	| ORA; v = value_expr; COMMA; Y { Instruction( Instructions.get_instruction ORA AbsoluteY, Some v) }
	| SBC; v = value_expr; COMMA; Y { Instruction( Instructions.get_instruction SBC AbsoluteY, Some v) }
	| STA; v = value_expr; COMMA; Y { Instruction( Instructions.get_instruction STA AbsoluteY, Some v) }
	(* AbsoluteY or zeropage,Y *)
	| LDX; v = value_expr; COMMA; Y { Instruction( Instructions.get_instruction LDX AbsoluteY, Some v) }
	| STX; v = value_expr; COMMA; Y { Instruction( Instructions.get_instruction STX ZeropageY, Some v) }

	| LDA; LPAREN; v = value_expr; COMMA; X; RPAREN { Instruction( Instructions.get_instruction LDA PreIndexIndirect, Some v) }
	| ADC; LPAREN; v = value_expr; COMMA; X; RPAREN { Instruction( Instructions.get_instruction ADC PreIndexIndirect, Some v) }
	| AND; LPAREN; v = value_expr; COMMA; X; RPAREN { Instruction( Instructions.get_instruction AND PreIndexIndirect, Some v) }
	| CMP; LPAREN; v = value_expr; COMMA; X; RPAREN { Instruction( Instructions.get_instruction CMP PreIndexIndirect, Some v) }
	| EOR; LPAREN; v = value_expr; COMMA; X; RPAREN { Instruction( Instructions.get_instruction EOR PreIndexIndirect, Some v) }
	| ORA; LPAREN; v = value_expr; COMMA; X; RPAREN { Instruction( Instructions.get_instruction ORA PreIndexIndirect, Some v) }
	| SBC; LPAREN; v = value_expr; COMMA; X; RPAREN { Instruction( Instructions.get_instruction SBC PreIndexIndirect, Some v) }
	| STA; LPAREN; v = value_expr; COMMA; X; RPAREN { Instruction( Instructions.get_instruction STA PreIndexIndirect, Some v) }
	(* indirect, Y indexed*)
	| LDA; LPAREN; v = value_expr; RPAREN; COMMA; Y {  Instruction( Instructions.get_instruction LDA PostIndexIndirect, Some v)}
	| ADC; LPAREN; v = value_expr; RPAREN; COMMA; Y {  Instruction( Instructions.get_instruction ADC PostIndexIndirect, Some v)}
	| AND; LPAREN; v = value_expr; RPAREN; COMMA; Y {  Instruction( Instructions.get_instruction AND PostIndexIndirect, Some v)}
	| CMP; LPAREN; v = value_expr; RPAREN; COMMA; Y {  Instruction( Instructions.get_instruction CMP PostIndexIndirect, Some v)}
	| EOR; LPAREN; v = value_expr; RPAREN; COMMA; Y {  Instruction( Instructions.get_instruction EOR PostIndexIndirect, Some v)}
	| ORA; LPAREN; v = value_expr; RPAREN; COMMA; Y {  Instruction( Instructions.get_instruction ORA PostIndexIndirect, Some v)}
	| SBC; LPAREN; v = value_expr; RPAREN; COMMA; Y {  Instruction( Instructions.get_instruction SBC PostIndexIndirect, Some v)}
	| STA; LPAREN; v = value_expr; RPAREN; COMMA; Y {  Instruction( Instructions.get_instruction STA PostIndexIndirect, Some v)}
	(* Relative *)
	| BCC; v = value_expr { Instruction( Instructions.get_instruction BCC Relative, Some v)}
	| BCS; v = value_expr { Instruction( Instructions.get_instruction BCS Relative, Some v)}
	| BEQ; v = value_expr { Instruction( Instructions.get_instruction BEQ Relative, Some v)}
	| BMI; v = value_expr { Instruction( Instructions.get_instruction BMI Relative, Some v)}
	| BNE; v = value_expr { Instruction( Instructions.get_instruction BNE Relative, Some v)}
	| BPL; v = value_expr { Instruction( Instructions.get_instruction BPL Relative, Some v)}
	| BVC; v = value_expr { Instruction( Instructions.get_instruction BVC Relative, Some v)}
	| BVS; v = value_expr { Instruction( Instructions.get_instruction BVS Relative, Some v)}
	(* Implied *)
	| BRK { Instruction( Instructions.get_instruction BRK Implied, None)}
	| CLC { Instruction( Instructions.get_instruction CLC Implied, None)}
	| CLD { Instruction( Instructions.get_instruction CLD Implied, None)}
	| CLI { Instruction( Instructions.get_instruction CLI Implied, None)}
	| CLV { Instruction( Instructions.get_instruction CLV Implied, None)}
	| DEX { Instruction( Instructions.get_instruction DEX Implied, None)}
	| DEY { Instruction( Instructions.get_instruction DEY Implied, None)}
	| INX { Instruction( Instructions.get_instruction INX Implied, None)}
	| INY { Instruction( Instructions.get_instruction INY Implied, None)}
	| NOP { Instruction( Instructions.get_instruction NOP Implied, None)}
	| PHA { Instruction( Instructions.get_instruction PHA Implied, None)}
	| PHP { Instruction( Instructions.get_instruction PHP Implied, None)}
	| PLA { Instruction( Instructions.get_instruction PLA Implied, None)}
	| PLP { Instruction( Instructions.get_instruction PLP Implied, None)}
	| RTI { Instruction( Instructions.get_instruction RTI Implied, None)}
	| RTS { Instruction( Instructions.get_instruction RTS Implied, None)}
	| SEC { Instruction( Instructions.get_instruction SEC Implied, None)}
	| SED { Instruction( Instructions.get_instruction SED Implied, None)}
	| SEI { Instruction( Instructions.get_instruction SEI Implied, None)}
	| TAX { Instruction( Instructions.get_instruction TAX Implied, None)}
	| TAY { Instruction( Instructions.get_instruction TAY Implied, None)}
	| TSX { Instruction( Instructions.get_instruction TSX Implied, None)}
	| TXA { Instruction( Instructions.get_instruction TXA Implied, None)}
	| TXS { Instruction( Instructions.get_instruction TXS Implied, None)}
	| TYA { Instruction( Instructions.get_instruction TYA Implied, None)}
	(* Indirect *)
	| JMP; LPAREN; v = value_expr; RPAREN { Instruction (Instructions.get_instruction JMP Indirect, Some v)}
	;
%%