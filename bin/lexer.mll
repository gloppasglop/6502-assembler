{
open Parser
open Lexing

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let newline = '\n' | "\r\n"
let digit = ['0'-'9']
let int = '-'? digit+
let wsp = [' ' '\t']+
let alpha = ['a'-'z' 'A'-'Z']

let id = (alpha) (alpha|digit|'_')*


let hexprefix = "0x" | '$' | '&'
let hexdigit = ['0'-'9' 'A'-'F']
let hexint = hexprefix hexdigit+
let hexint8 = hexprefix hexdigit | hexprefix hexdigit hexdigit

let octalprefix = "0o" | '@' | "0"
let octaldigit = ['0'-'7']
let octalint = octalprefix octaldigit+

let binaryprefix = "0b" | '%'
let binarydigit = ['0'-'1']
let binaryint = binaryprefix binarydigit+


rule read = 
	parse
	| wsp { read lexbuf}
  | newline { next_line lexbuf; read lexbuf }
	| "+" { PLUS }
	| "*" { MULT }
	| "=" { EQUAL }
	| ":" { COLON }
	| "(" { LPAREN }
	| ")" { RPAREN }
	| ">" { GT }
	| "<" { LT }
	| "[" { LBRACKET }
	| "]" { RBRACKET }
	| "#" { POUND }
	| "," { COMMA }
	| "X" { X }
	| "Y" { Y }
	| "A" { A }
	| "ADC" | "adc" { ADC }
	| "AND" | "and" { AND }
	| "ASL" | "asl" { ASL }
	| "BCC" | "bcc" { BCC }
	| "BCS" | "bcs" { BCS }
	| "BEQ" | "beq" { BEQ }
	| "BIT" | "bit" { BIT }
	| "BMI" | "bmi" { BMI }
	| "BNE" | "bne" { BNE }
	| "BPL" | "bpl" { BPL }
	| "BRK" | "brk" { BRK }
	| "BVC" | "bnv" { BVC }
	| "BVS" | "bvs" { BVS }
	| "CLC" | "clc" { CLC }
	| "CLD" | "cld" { CLD }
	| "CLI" | "cli" { CLI }
	| "CLV" | "clv" { CLV }
	| "CMP" | "cmp" { CMP }
	| "CPX" | "cpx" { CPX }
	| "CPY" | "cpy" { CPY }
	| "DEC" | "dec" { DEC }
	| "DEX" | "dex" { DEX }
	| "DEY" | "dey" { DEY }
	| "EOR" | "eor" { EOR }
	| "INC" |"inc" { INC }
	| "INX" | "inx" { INX }
	| "INY" | "iny" { INY }
	| "JMP" | "jmp" { JMP }
	| "JSR" | "jsr" { JSR }
	| "LDA" | "lda" { LDA }
	| "LDX" | "ldx" { LDX }
	| "LDY" | "ldy" { LDY }
	| "LSR" | "lsr" { LSR }
	| "NOP" | "nop" { NOP }
	| "ORA" | "ora" { ORA }
	| "PHA" | "pha" { PHA }
	| "PHP" | "php" { PHP }
	| "PLA" | "pla" { PLA }
	| "PLP" | "plp" { PLP }
	| "ROL" | "rol" { ROL }
	| "ROR" | "ror" { ROR }
	| "RTI" | "rti" { RTI }
	| "RTS" | "rts" { RTS }
	| "SBC" | "sbc" { SBC }
	| "SEC" | "sec" { SEC }
	| "SED" | "sed" { SED }
	| "SEI" | "sei" { SEI }
	| "STA" | "sta" { STA }
	| "STX" | "stx" { STX }
	| "STY" | "sty" { STY }
	| "TAX" | "tax" {TAX }
	| "TAY" | "tay" { TAY }
	| "TSX" | "tsx" { TSX }
	| "TXA" | "txa" { TXA }
	| "TXS" | "txs" { TXS }
	| "TYA" | "tya" { TYA }
	| int { INT ( int_of_string (Lexing.lexeme lexbuf)) }
	| hexint { INT ( Utils.int_of_hexstring (Lexing.lexeme lexbuf)) }
	| octalint { INT ( Utils.int_of_octalstring (Lexing.lexeme lexbuf)) }
	| binaryint { INT ( Utils.int_of_binarystring (Lexing.lexeme lexbuf)) }
	| id { ID ( Lexing.lexeme lexbuf)}
	| ";" { read_comment lexbuf }
	| eof { EOF }
and read_comment = 
parse
	| newline { next_line lexbuf; read lexbuf }
	| eof { EOF}
	| _ { read_comment lexbuf}