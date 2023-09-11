%{
open Instructions
%}

%token <int> INT
%token <string> ID
(*
%token <int> BYTE
%token <int> ADDRESS
*)
%token PLUS
%token MULT 
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


%left GT
%left LT
%left PLUS
%left MULT

%start program

%type <Ast.asm_expr list> program
%type <Ast.value_expr> value_expr
(*)
%type <Ast.asm_expr> asm_expr
%type <Ast.arithm_expr> arithm_expr
*)

%%


(*)
arithm_expr:
*)

value_expr: 
	| i = INT { Int i }
	| e1 = value_expr; PLUS ; e2 = value_expr { Binop (Add, e1, e2)}
	| e1 = value_expr; MULT ; e2 = value_expr { Binop (Mult, e1, e2)}
	| LT; e = value_expr  { Unop ( Lowbyte, e)}
	| GT; e = value_expr  { Unop ( Highbyte, e)}
	| LBRACKET ; e = value_expr ; RBRACKET { e } 
	;

asm_expr:
  | id = ID ; EQUAL; assigned_expr = value_expr { Assign (Symbol id,assigned_expr)}
	| id = ID; COLON; { Label id }
	| id = ID  { Label id }
	(* Accumulator *)
	| ASL; { Instruction( $startpos, Instructions.asl_acc, None)}
	| ASL; A { Instruction( $startpos, Instructions.asl_acc,None)}
	| LSR { Instruction( $startpos, Instructions.lsr_acc, None)}
	| LSR; A { Instruction( $startpos, Instructions.lsr_acc, None)}
	| ROL { Instruction( $startpos, Instructions.rol_acc, None)}
	| ROL; A { Instruction( $startpos, Instructions.rol_acc, None)}
	| ROR { Instruction( $startpos, Instructions.ror_acc, None)}
	| ROR; A { Instruction( $startpos, Instructions.ror_acc, None)}
	(* Immediate *)
	| ADC; POUND; v = option(value_expr) { Instruction( $startpos, Instructions.adc_imm, v) }
	| AND; POUND; v = value_expr { Instruction( $startpos, Instructions.and_imm, Some v) }
	| CMP; POUND; v = value_expr { Instruction( $startpos, Instructions.cmp_imm, Some v) }
	| CPX; POUND; v = value_expr { Instruction( $startpos, Instructions.cpx_imm, Some v) }
	| CPY; POUND; v = value_expr { Instruction( $startpos, Instructions.cpy_imm, Some v) }
	| EOR; POUND; v = value_expr { Instruction( $startpos, Instructions.eor_imm, Some v) }
	| LDA; POUND; v = value_expr { Instruction( $startpos, Instructions.lda_imm, Some v) }
	| LDX; POUND; v = value_expr { Instruction( $startpos, Instructions.ldx_imm, Some v) }
	| LDY; POUND; v = value_expr { Instruction( $startpos, Instructions.ldy_imm, Some v) }
	| ORA; POUND; v = value_expr { Instruction( $startpos, Instructions.ora_imm, Some v) }
	| SBC; POUND; v = value_expr { Instruction( $startpos, Instructions.sbc_imm, Some v) }
	(* Absolute *)
	| JMP; v = value_expr { Instruction( $startpos, Instructions.jmp_abs, Some v)}
	| JSR; v = value_expr { Instruction( $startpos, Instructions.jsr_abs, Some v)}
	(* Absolute or Zero Page*)
	| ADC; v = value_expr { Instruction( $startpos, Instructions.adc_abs, Some v)}
	| AND; v = value_expr { Instruction( $startpos, Instructions.and_abs, Some v)}
	| ASL; v = value_expr { Instruction( $startpos, Instructions.asl_abs, Some v)}
	| BIT; v = value_expr { Instruction( $startpos, Instructions.bit_abs, Some v)}
	| CMP; v = value_expr { Instruction( $startpos, Instructions.cmp_abs, Some v)}
	| CPX; v = value_expr { Instruction( $startpos, Instructions.cpx_abs, Some v)}
	| CPY; v = value_expr { Instruction( $startpos, Instructions.cpy_abs, Some v)}
	| DEC; v = value_expr { Instruction( $startpos, Instructions.dec_abs, Some v)}
	| EOR; v = value_expr { Instruction( $startpos, Instructions.eor_abs, Some v)}
	| INC; v = value_expr { Instruction( $startpos, Instructions.inc_abs, Some v)}
	| LDA; v = value_expr { Instruction( $startpos, Instructions.lda_abs, Some v)}
	| LDX; v = value_expr { Instruction( $startpos, Instructions.ldx_abs, Some v)}
	| LDY; v = value_expr { Instruction( $startpos, Instructions.ldy_abs, Some v)}
	| LSR; v = value_expr { Instruction( $startpos, Instructions.lsr_abs, Some v)}
	| ORA; v = value_expr { Instruction( $startpos, Instructions.ora_abs, Some v)}
	| ROL; v = value_expr { Instruction( $startpos, Instructions.rol_abs, Some v)}
	| ROR; v = value_expr { Instruction( $startpos, Instructions.ror_abs, Some v)}
	| SBC; v = value_expr { Instruction( $startpos, Instructions.sbc_abs, Some v)}
	| STA; v = value_expr { Instruction( $startpos, Instructions.sta_abs, Some v)}
	| STX; v = value_expr { Instruction( $startpos, Instructions.stx_abs, Some v)}
	| STY; v = value_expr { Instruction( $startpos, Instructions.sty_abs, Some v)}
	(* zeropageX *)
	(* TODO: Check how to raise error during parsing if value > 255*)
	| STY; v = value_expr; COMMA; X { Instruction( $startpos, Instructions.sty_zpx, Some v) }
	(* AbsoluteX or zeropage,X *)
	| ADC; v = value_expr; COMMA; X { Instruction( $startpos, Instructions.adc_absx, Some v) }
	| AND; v = value_expr; COMMA; X { Instruction( $startpos, Instructions.and_absx, Some v) }
	| ASL; v = value_expr; COMMA; X { Instruction( $startpos, Instructions.asl_absx, Some v) }
	| CMP; v = value_expr; COMMA; X { Instruction( $startpos, Instructions.cmp_absx, Some v) }
	| DEC; v = value_expr; COMMA; X { Instruction( $startpos, Instructions.dec_absx, Some v) }
	| EOR; v = value_expr; COMMA; X { Instruction( $startpos, Instructions.eor_absx, Some v) }
	| INC; v = value_expr; COMMA; X { Instruction( $startpos, Instructions.inc_absx, Some v) }
	| LDA; v = value_expr; COMMA; X { Instruction( $startpos, Instructions.lda_absx, Some v) }
	| LDY; v = value_expr; COMMA; X { Instruction( $startpos, Instructions.ldy_absx, Some v) }
	| LSR; v = value_expr; COMMA; X { Instruction( $startpos, Instructions.lsr_absx, Some v) }
	| ORA; v = value_expr; COMMA; X { Instruction( $startpos, Instructions.ora_absx, Some v) }
	| ROL; v = value_expr; COMMA; X { Instruction( $startpos, Instructions.rol_absx, Some v) }
	| ROR; v = value_expr; COMMA; X { Instruction( $startpos, Instructions.ror_absx, Some v) }
	| SBC; v = value_expr; COMMA; X { Instruction( $startpos, Instructions.sbc_absx, Some v) }
	| STA; v = value_expr; COMMA; X { Instruction( $startpos, Instructions.sta_absx, Some v) }
	(* zeropageY *)
	(* TODO: Check how to raise error during parsing if value > 255*)
	(* AbsoluteY *)
	| LDA; v = value_expr; COMMA; Y { Instruction( $startpos, Instructions.lda_absy, Some v) }
	| ADC; v = value_expr; COMMA; Y { Instruction( $startpos, Instructions.adc_absy, Some v) }
	| AND; v = value_expr; COMMA; Y { Instruction( $startpos, Instructions.and_absy, Some v) }
	| CMP; v = value_expr; COMMA; Y { Instruction( $startpos, Instructions.cmp_absy, Some v) }
	| EOR; v = value_expr; COMMA; Y { Instruction( $startpos, Instructions.eor_absy, Some v) }
	| ORA; v = value_expr; COMMA; Y { Instruction( $startpos, Instructions.ora_absy, Some v) }
	| SBC; v = value_expr; COMMA; Y { Instruction( $startpos, Instructions.sbc_absy, Some v) }
	| STA; v = value_expr; COMMA; Y { Instruction( $startpos, Instructions.sta_absy, Some v) }
	(* AbsoluteY or zeropage,Y *)
	| LDX; v = value_expr; COMMA; Y { Instruction( $startpos, Instructions.ldx_absy, Some v) }
	| STX; v = value_expr; COMMA; Y { Instruction( $startpos, Instructions.stx_zpy, Some v) }

	| LDA; LPAREN; v = value_expr; COMMA; X; RPAREN { Instruction( $startpos, Instructions.lda_xind, Some v) }
	| ADC; LPAREN; v = value_expr; COMMA; X; RPAREN { Instruction( $startpos, Instructions.adc_xind, Some v) }
	| AND; LPAREN; v = value_expr; COMMA; X; RPAREN { Instruction( $startpos, Instructions.and_xind, Some v) }
	| CMP; LPAREN; v = value_expr; COMMA; X; RPAREN { Instruction( $startpos, Instructions.cmp_xind, Some v) }
	| EOR; LPAREN; v = value_expr; COMMA; X; RPAREN { Instruction( $startpos, Instructions.eor_xind, Some v) }
	| ORA; LPAREN; v = value_expr; COMMA; X; RPAREN { Instruction( $startpos, Instructions.ora_xind, Some v) }
	| SBC; LPAREN; v = value_expr; COMMA; X; RPAREN { Instruction( $startpos, Instructions.sbc_xind, Some v) }
	| STA; LPAREN; v = value_expr; COMMA; X; RPAREN { Instruction( $startpos, Instructions.sta_xind, Some v) }
	(* indirect, Y indexed*)
	| LDA; LPAREN; v = value_expr; RPAREN; COMMA; Y {  Instruction( $startpos, Instructions.lda_indy, Some v)}
	| ADC; LPAREN; v = value_expr; RPAREN; COMMA; Y {  Instruction( $startpos, Instructions.adc_indy, Some v)}
	| AND; LPAREN; v = value_expr; RPAREN; COMMA; Y {  Instruction( $startpos, Instructions.and_indy, Some v)}
	| CMP; LPAREN; v = value_expr; RPAREN; COMMA; Y {  Instruction( $startpos, Instructions.cmp_indy, Some v)}
	| EOR; LPAREN; v = value_expr; RPAREN; COMMA; Y {  Instruction( $startpos, Instructions.eor_indy, Some v)}
	| ORA; LPAREN; v = value_expr; RPAREN; COMMA; Y {  Instruction( $startpos, Instructions.ora_indy, Some v)}
	| SBC; LPAREN; v = value_expr; RPAREN; COMMA; Y {  Instruction( $startpos, Instructions.sbc_indy, Some v)}
	| STA; LPAREN; v = value_expr; RPAREN; COMMA; Y {  Instruction( $startpos, Instructions.sta_indy, Some v)}
	(* Relative *)
	| BCC; v = value_expr { Instruction( $startpos, Instructions.bcc_rel, Some v)}
	| BCS; v = value_expr { Instruction( $startpos, Instructions.bcs_rel, Some v)}
	| BEQ; v = value_expr { Instruction( $startpos, Instructions.beq_rel, Some v)}
	| BMI; v = value_expr { Instruction( $startpos, Instructions.bmi_rel, Some v)}
	| BNE; v = value_expr { Instruction( $startpos, Instructions.bne_rel, Some v)}
	| BPL; v = value_expr { Instruction( $startpos, Instructions.bpl_rel, Some v)}
	| BVC; v = value_expr { Instruction( $startpos, Instructions.bvc_rel, Some v)}
	| BVS; v = value_expr { Instruction( $startpos, Instructions.bvs_rel, Some v)}
	(* Implied *)
	| BRK { Instruction( $startpos, Instructions.brk_imp, None)}
	| CLC { Instruction( $startpos, Instructions.clc_imp, None)}
	| CLD { Instruction( $startpos, Instructions.cld_imp, None)}
	| CLI { Instruction( $startpos, Instructions.cli_imp, None)}
	| CLV { Instruction( $startpos, Instructions.clv_imp, None)}
	| DEX { Instruction( $startpos, Instructions.dex_imp, None)}
	| DEY { Instruction( $startpos, Instructions.dey_imp, None)}
	| INX { Instruction( $startpos, Instructions.inx_imp, None)}
	| INY { Instruction( $startpos, Instructions.iny_imp, None)}
	| NOP { Instruction( $startpos, Instructions.nop_imp, None)}
	| PHA { Instruction( $startpos, Instructions.pha_imp, None)}
	| PHP { Instruction( $startpos, Instructions.php_imp, None)}
	| PLA { Instruction( $startpos, Instructions.pla_imp, None)}
	| PLP { Instruction( $startpos, Instructions.plp_imp, None)}
	| RTI { Instruction( $startpos, Instructions.rti_imp, None)}
	| RTS { Instruction( $startpos, Instructions.rts_imp, None)}
	| SEC { Instruction( $startpos, Instructions.sec_imp, None)}
	| SED { Instruction( $startpos, Instructions.sed_imp, None)}
	| SEI { Instruction( $startpos, Instructions.sei_imp, None)}
	| TAX { Instruction( $startpos, Instructions.tax_imp, None)}
	| TAY { Instruction( $startpos, Instructions.tay_imp, None)}
	| TSX { Instruction( $startpos, Instructions.tsx_imp, None)}
	| TXA { Instruction( $startpos, Instructions.txa_imp, None)}
	| TXS { Instruction( $startpos, Instructions.txs_imp, None)}
	| TYA { Instruction( $startpos, Instructions.tya_imp, None)}
	(* Indirect *)
	| JMP; LPAREN; v = value_expr; RPAREN { Instruction( $startpos, Instructions.jmp_ind, Some v)}
	;

program:
	| p = list(asm_expr); EOF { p }
	;

(*
operand:
| POUND; v = value_expr{ Operand (Immediate, Some i) }
| v = value_expr { Operand (Absolute, Some i ) }
| v = value_expr; COMMA; X { Operand (AbsoluteX, Some i) }
| v = value_expr; COMMA; Y { Operand (AbsoluteY, Some i) }
| LPAREN; _i = INT; COMMA; X; RPAREN { Operand (Indirect, Some i) }
*)

%%