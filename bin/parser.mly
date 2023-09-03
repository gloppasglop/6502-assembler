%token <int> INT
(*
%token <int> BYTE
%token <int> ADDRESS
*)
%token PLUS
%token MULT 
%token LPAREN
%token RPAREN
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


%left PLUS
%left MULT

%start program
%type <Ast.expr> program

%%

program:
	| e = expr; EOF { e }
	;

expr:
	| i = INT { Int i }
	| e1 = expr; PLUS ; e2 = expr { Binop (Add, e1, e2)}
	| e1 = expr; MULT ; e2 = expr { Binop (Mult, e1, e2)}
	| LPAREN ; e = expr ; RPAREN { e } 
	(* Accumulator *)
	| ASL; { Instruction (Asl, Accumulator)}
	| ASL; A { Instruction (Asl, Accumulator)}
	| LSR { Instruction (Lsr, Accumulator)}
	| LSR; A { Instruction (Lsr, Accumulator)}
	| ROL { Instruction (Rol, Accumulator)}
	| ROL; A { Instruction (Rol, Accumulator)}
	| ROR { Instruction (Ror, Accumulator)}
	| ROR; A { Instruction (Ror, Accumulator)}
	(* Immediate *)
	| ADC; POUND; i = INT  { Instruction (Adc,Immediate(Int i)) }
	| AND; POUND; i = INT  { Instruction (And,Immediate(i)) }
	| CMP; POUND; i = INT  { Instruction (Cmp,Immediate(i)) }
	| CPX; POUND; i = INT  { Instruction (Cpx,Immediate(i)) }
	| CPY; POUND; i = INT  { Instruction (Cpy,Immediate(i)) }
	| EOR; POUND; i = INT  { Instruction (Eor,Immediate(i)) }
	| LDA; POUND; i = INT  { Instruction (Lda,Immediate(i)) }
	| LDX; POUND; i = INT  { Instruction (Ldx,Immediate(i)) }
	| LDY; POUND; i = INT  { Instruction (Ldy,Immediate(i)) }
	| ORA; POUND; i = INT  { Instruction (Ora,Immediate(i)) }
	| SBC; POUND; i = INT  { Instruction (Sbc,Immediate(i)) }
	| STA; POUND; i = INT  { Instruction (Sta,Immediate(i)) }
	(* Absolute *)
	| JMP; i = INT  { Instruction (Jmp, Absolute(i))}
	| JSR; i = INT  { Instruction (Jsr, Absolute(i))}
	(* Absolute or Zero Page*)
	| ADC; i = INT  { if ( i <= 255) then Instruction (Adc, Zeropage(i)) else  Instruction (Adc, Absolute(i))}
	| AND; i = INT  { if ( i <= 255) then Instruction (And, Zeropage(i)) else  Instruction (And, Absolute(i))}
	| ASL; i = INT  { if ( i <= 255) then Instruction (Asl, Zeropage(i)) else  Instruction (Asl, Absolute(i))}
	| BIT; i = INT  { if ( i <= 255) then Instruction (Bit, Zeropage(i)) else  Instruction (Bit, Absolute(i))}
	| CMP; i = INT  { if ( i <= 255) then Instruction (Cmp, Zeropage(i)) else  Instruction (Cmp, Absolute(i))}
	| CPX; i = INT  { if ( i <= 255) then Instruction (Cmp, Zeropage(i)) else  Instruction (Cpx, Absolute(i))}
	| CPY; i = INT  { if ( i <= 255) then Instruction (Cmp, Zeropage(i)) else  Instruction (Cpy, Absolute(i))}
	| DEC; i = INT  { if ( i <= 255) then Instruction (Dec, Zeropage(i)) else  Instruction (Dec, Absolute(i))}
	| EOR; i = INT  { if ( i <= 255) then Instruction (Eor, Zeropage(i)) else  Instruction (Eor, Absolute(i))}
	| INC; i = INT  { if ( i <= 255) then Instruction (Inc, Zeropage(i)) else  Instruction (Inc, Absolute(i))}
	| LDA; i = INT  { if ( i <= 255) then Instruction (Lda, Zeropage(i)) else  Instruction (Lda, Absolute(i))}
	| LDX; i = INT  { if ( i <= 255) then Instruction (Ldx, Zeropage(i)) else  Instruction (Ldx, Absolute(i))}
	| LDY; i = INT  { if ( i <= 255) then Instruction (Ldy, Zeropage(i)) else  Instruction (Ldy, Absolute(i))}
	| LSR; i = INT  { if ( i <= 255) then Instruction (Lsr, Zeropage(i)) else  Instruction (Lsr, Absolute(i))}
	| ORA; i = INT  { if ( i <= 255) then Instruction (Ora, Zeropage(i)) else  Instruction (Ora, Absolute(i))}
	| ROL; i = INT  { if ( i <= 255) then Instruction (Rol, Zeropage(i)) else  Instruction (Rol, Absolute(i))}
	| ROR; i = INT  { if ( i <= 255) then Instruction (Ror, Zeropage(i)) else  Instruction (Ror, Absolute(i))}
	| SBC; i = INT  { if ( i <= 255) then Instruction (Sbc, Zeropage(i)) else  Instruction (Sbc, Absolute(i))}
	| STA; i = INT  { if ( i <= 255) then Instruction (Sta, Zeropage(i)) else  Instruction (Sta, Absolute(i))}
	| STX; i = INT  { if ( i <= 255) then Instruction (Stx, Zeropage(i)) else  Instruction (Stx, Absolute(i))}
	| STY; i = INT  { if ( i <= 255) then Instruction (Sty, Zeropage(i)) else  Instruction (Sty, Absolute(i))}
	(* zeropageX *)
	(* TODO: Check how to raise error during parsing if value > 255*)
	| STY; i = INT; COMMA; X { Instruction (Sty, ZeropageX(i)) }
	(* AbsoluteX or zeropage,X *)
	| ADC; i = INT; COMMA; X { if (i <= 255) then Instruction (Adc, ZeropageX(i)) else Instruction (Adc, AbsoluteX(i)) }
	| AND; i = INT; COMMA; X { if (i <= 255) then Instruction (And, ZeropageX(i)) else Instruction (And, AbsoluteX(i)) }
	| ASL; i = INT; COMMA; X { if (i <= 255) then Instruction (Asl, ZeropageX(i)) else Instruction (Asl, AbsoluteX(i)) }
	| CMP; i = INT; COMMA; X { if (i <= 255) then Instruction (Cmp, ZeropageX(i)) else Instruction (Cmp, AbsoluteX(i)) }
	| DEC; i = INT; COMMA; X { if (i <= 255) then Instruction (Dec, ZeropageX(i)) else Instruction (Dec, AbsoluteX(i)) }
	| EOR; i = INT; COMMA; X { if (i <= 255) then Instruction (Eor, ZeropageX(i)) else Instruction (Eor, AbsoluteX(i)) }
	| INC; i = INT; COMMA; X { if (i <= 255) then Instruction (Inc, ZeropageX(i)) else Instruction (Inc, AbsoluteX(i)) }
	| LDA; i = INT; COMMA; X { if (i <= 255) then Instruction (Lda, ZeropageX(i)) else Instruction (Lda, AbsoluteX(i)) }
	| LDY; i = INT; COMMA; X { if (i <= 255) then Instruction (Ldy, ZeropageX(i)) else Instruction (Ldy, AbsoluteX(i)) }
	| LSR; i = INT; COMMA; X { if (i <= 255) then Instruction (Lsr, ZeropageX(i)) else Instruction (Lsr, AbsoluteX(i)) }
	| ORA; i = INT; COMMA; X { if (i <= 255) then Instruction (Ora, ZeropageX(i)) else Instruction (Ora, AbsoluteX(i)) }
	| ROL; i = INT; COMMA; X { if (i <= 255) then Instruction (Rol, ZeropageX(i)) else Instruction (Rol, AbsoluteX(i)) }
	| ROR; i = INT; COMMA; X { if (i <= 255) then Instruction (Ror, ZeropageX(i)) else Instruction (Ror, AbsoluteX(i)) }
	| SBC; i = INT; COMMA; X { if (i <= 255) then Instruction (Sbc, ZeropageX(i)) else Instruction (Sbc, AbsoluteX(i)) }
	| STA; i = INT; COMMA; X { if (i <= 255) then Instruction (Sta, ZeropageX(i)) else Instruction (Sta, AbsoluteX(i)) }
	(* zeropageY *)
	(* TODO: Check how to raise error during parsing if value > 255*)
	| STX; i = INT; COMMA; Y { Instruction (Stx, ZeropageY(i)) }
	(* AbsoluteY *)
	| LDA; i = INT; COMMA; Y { Instruction (Lda , AbsoluteY(i)) }
	| ADC; i = INT; COMMA; Y { Instruction (Adc , AbsoluteY(i)) }
	| AND; i = INT; COMMA; Y { Instruction (And , AbsoluteY(i)) }
	| CMP; i = INT; COMMA; Y { Instruction (Cmp , AbsoluteY(i)) }
	| EOR; i = INT; COMMA; Y { Instruction (Eor , AbsoluteY(i)) }
	| ORA; i = INT; COMMA; Y { Instruction (Ora , AbsoluteY(i)) }
	| SBC; i = INT; COMMA; Y { Instruction (Sbc , AbsoluteY(i)) }
	| STA; i = INT; COMMA; Y { Instruction (Sta , AbsoluteY(i)) }
	(* AbsoluteY or zeropage,Y *)
	| LDX; i = INT; COMMA; Y { if (i <= 255) then Instruction (Ldx, ZeropageY(i)) else Instruction (Ldx, AbsoluteY(i)) }
	| STX; i = INT; COMMA; Y { if (i <= 255) then Instruction (Stx, ZeropageY(i)) else Instruction (Stx, AbsoluteY(i)) }
	(* Indirect *)
	| JMP; LPAREN; i = INT ; RPAREN { Instruction ( Jmp, Indirect(i))}
	(* X-Indexed, indirect*)
	| LDA; LPAREN; i = INT; COMMA; X; RPAREN { Instruction (Lda, PreIndexIndirect (i)) }
	| ADC; LPAREN; i = INT; COMMA; X; RPAREN { Instruction (Adc, PreIndexIndirect (i)) }
	| AND; LPAREN; i = INT; COMMA; X; RPAREN { Instruction (And, PreIndexIndirect (i)) }
	| CMP; LPAREN; i = INT; COMMA; X; RPAREN { Instruction (Cmp, PreIndexIndirect (i)) }
	| EOR; LPAREN; i = INT; COMMA; X; RPAREN { Instruction (Eor, PreIndexIndirect (i)) }
	| ORA; LPAREN; i = INT; COMMA; X; RPAREN { Instruction (Ora, PreIndexIndirect (i)) }
	| SBC; LPAREN; i = INT; COMMA; X; RPAREN { Instruction (Sbc, PreIndexIndirect (i)) }
	| STA; LPAREN; i = INT; COMMA; X; RPAREN { Instruction (Sta, PreIndexIndirect (i)) }
	(* indirect, Y indexed*)
	| LDA; LPAREN; i = INT; RPAREN; COMMA; Y {  Instruction (Lda , PostIndexIndirect ( i ))}
	| ADC; LPAREN; i = INT; RPAREN; COMMA; Y {  Instruction (Adc , PostIndexIndirect ( i ))}
	| AND; LPAREN; i = INT; RPAREN; COMMA; Y {  Instruction (And , PostIndexIndirect ( i ))}
	| CMP; LPAREN; i = INT; RPAREN; COMMA; Y {  Instruction (Cmp , PostIndexIndirect ( i ))}
	| EOR; LPAREN; i = INT; RPAREN; COMMA; Y {  Instruction (Eor , PostIndexIndirect ( i ))}
	| ORA; LPAREN; i = INT; RPAREN; COMMA; Y {  Instruction (Ora , PostIndexIndirect ( i ))}
	| SBC; LPAREN; i = INT; RPAREN; COMMA; Y {  Instruction (Sbc , PostIndexIndirect ( i ))}
	| STA; LPAREN; i = INT; RPAREN; COMMA; Y {  Instruction (Sta , PostIndexIndirect ( i ))}
	(* Relative *)
	| BCC; i = INT  { Instruction (Bcc, Relative(i))}
	| BCS; i = INT  { Instruction (Bcs, Relative(i))}
	| BEQ; i = INT  { Instruction (Beq, Relative(i))}
	| BMI; i = INT  { Instruction (Bmi, Relative(i))}
	| BNE; i = INT  { Instruction (Bne, Relative(i))}
	| BPL; i = INT  { Instruction (Bpl, Relative(i))}
	| BVC; i = INT  { Instruction (Bvc, Relative(i))}
	| BVS; i = INT  { Instruction (Bvs, Relative(i))}
	(* Implied *)
	| BRK { Instruction (Brk, Implied)}
	| CLC { Instruction (Clc, Implied)}
	| CLD { Instruction (Cld, Implied)}
	| CLI { Instruction (Cli, Implied)}
	| CLV { Instruction (Clv, Implied)}
	| DEX { Instruction (Dex, Implied)}
	| DEY { Instruction (Dey, Implied)}
	| INX { Instruction (Inx, Implied)}
	| INY { Instruction (Iny, Implied)}
	| NOP { Instruction (Nop,Implied)}
	| PHA { Instruction (Pha,Implied)}
	| PHP { Instruction (Php,Implied)}
	| PLA { Instruction (Pla,Implied)}
	| PLP { Instruction (Plp,Implied)}
	| RTI { Instruction (Rti,Implied)}
	| RTS { Instruction (Rts,Implied)}
	| SEC { Instruction (Sec,Implied)}
	| SED { Instruction (Sed,Implied)}
	| SEI { Instruction (Sei,Implied)}
	| TAX { Instruction (Tax,Implied)}
	| TAY { Instruction (Tay,Implied)}
	| TSX { Instruction (Tsx,Implied)}
	| TXA { Instruction (Txa,Implied)}
	| TXS { Instruction (Txs,Implied)}
	| TYA { Instruction (Tya,Implied)}
	;

(*
operand:
| POUND; i = INT { Operand (Immediate, Some i) }
| i = INT  { Operand (Absolute, Some i ) }
| i = INT; COMMA; X { Operand (AbsoluteX, Some i) }
| i = INT; COMMA; Y { Operand (AbsoluteY, Some i) }
| LPAREN; _i = INT; COMMA; X; RPAREN { Operand (Indirect, Some i) }
*)

%%