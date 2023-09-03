{
open Parser
}

let digit = ['0'-'9']
let int = '-'? digit+
let wsp = [' ' '\t']+

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
	| "+" { PLUS }
	| "*" { MULT }
	| "(" { LPAREN }
	| ")" { RPAREN }
	| "#" { POUND }
	| "," { COMMA }
	| "X" { X }
	| "Y" { Y }
	| "A" { A }
	| "ADC" { ADC }
	| "AND" { AND }
	| "ASL" { ASL }
	| "BCC" { BCC }
	| "BCS" { BCS }
	| "BEQ" { BEQ }
	| "BIT" { BIT }
	| "BMI" { BMI }
	| "BNE" { BNE }
	| "BPL" { BPL }
	| "BRK" { BRK }
	| "BVC" { BVC }
	| "BVS" { BVS }
	| "CLC" { CLC }
	| "CLD" { CLD }
	| "CLI" { CLI }
	| "CLV" { CLV }
	| "CMP" { CMP }
	| "CPX" { CPX }
	| "CPY" { CPY }
	| "DEC" { DEC }
	| "DEX" { DEX }
	| "DEY" { DEY }
	| "EOR" { EOR }
	| "INC" { INC }
	| "INX" { INX }
	| "INY" { INY }
	| "JMP" { JMP }
	| "JSR" { JSR }
	| "LDA" { LDA }
	| "LDX" { LDX }
	| "LDY" { LDY }
	| "LSR" { LSR }
	| "NOP" { NOP }
	| "ORA" { ORA }
	| "PHA" { PHA }
	| "PHP" { PHP }
	| "PLA" { PLA }
	| "PLP" { PLP }
	| "ROL" { ROL }
	| "ROR" { ROR }
	| "RTI" { RTI }
	| "RTS" { RTS }
	| "SBC" { SBC }
	| "SEC" { SEC }
	| "SED" { SED }
	| "SEI" { SEI }
	| "STA" { STA }
	| "STX" { STX }
	| "STY" { STY }
	| "TAX" { TAX }
	| "TAY" { TAY }
	| "TSX" { TSX }
	| "TXA" { TXA }
	| "TXS" { TXS }
	| "TYA" { TYA }
	| int { INT ( int_of_string (Lexing.lexeme lexbuf)) }
	| hexint { INT ( Utils.int_of_hexstring (Lexing.lexeme lexbuf)) }
	| octalint { INT ( Utils.int_of_octalstring (Lexing.lexeme lexbuf)) }
	| binaryint { INT ( Utils.int_of_binarystring (Lexing.lexeme lexbuf)) }
	| eof { EOF }