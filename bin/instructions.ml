module Instructions = struct
  type addressing_mode =
    | Accumulator
    | Implied
    | Immediate
    | Absolute
    | Zeropage
    | AbsoluteX
    | AbsoluteY
    | ZeropageX
    | ZeropageY
    | Indirect
    | PreIndexIndirect (* X-indexed, indirect.*)
    | PostIndexIndirect (* indirect, Y-indexed.*)
    | Relative

  type mnemonic =
    | ADC (* add with carry *)
    | AND (* and (with accumulator) *)
    | ASL (* arithmetic shift left *)
    | BCC (* branch on carry clear *)
    | BCS (* branch on carry set *)
    | BEQ (* branch on equal (zero set) *)
    | BIT (* bit test *)
    | BMI (* branch on minus (negative set) *)
    | BNE (* branch on not equal (zero clear) *)
    | BPL (* branch on plus (negative clear) *)
    | BRK (* break / interrupt *)
    | BVC (* branch on overflow clear *)
    | BVS (* branch on overflow set *)
    | CLC (* clear carry *)
    | CLD (* clear decimal *)
    | CLI (* clear interrupt disable *)
    | CLV (* clear overflow *)
    | CMP (* compare (with accumulator) *)
    | CPX (* compare with X *)
    | CPY (* compare with Y *)
    | DEC (* decrement *)
    | DEX (* decrement X *)
    | DEY (* decrement Y *)
    | EOR (* exclusive or (with accumulator) *)
    | INC (* increment *)
    | INX (* increment X *)
    | INY (* increment Y *)
    | JMP (* jump *)
    | JSR (* jump subroutine *)
    | LDA (* load accumulator *)
    | LDX (* load X *)
    | LDY (* load Y *)
    | LSR (* logical shift right *)
    | NOP (* no operation *)
    | ORA (* or with accumulator *)
    | PHA (* push accumulator *)
    | PHP (* push processor status (SR) *)
    | PLA (* pull accumulator *)
    | PLP (* pull processor status (SR) *)
    | ROL (* rotate left *)
    | ROR (* rotate right *)
    | RTI (* return from interrupt *)
    | RTS (* return from subroutine *)
    | SBC (* subtract with carry *)
    | SEC (* set carry *)
    | SED (* set decimal *)
    | SEI (* set interrupt disable *)
    | STA (* store accumulator *)
    | STX (* store X *)
    | STY (* store Y *)
    | TAX (* transfer accumulator to X *)
    | TAY (* transfer accumulator to Y *)
    | TSX (* transfer stack pointer to X *)
    | TXA (* transfer X to accumulator *)
    | TXS (* transfer X to stack pointer *)
    | TYA (* transfer Y to accumulator *)

  type instruction =
    { mnemonic : mnemonic
    ; addressing : addressing_mode
    ; opcode : int
    ; bytes : int
    ; cycles : int
    ; pp : string
    }

  let get_instruction mnemonic addressing_mode =
    match addressing_mode with
    (* Accumulator *)
    | Accumulator ->
      (match mnemonic with
       | ASL ->
         { mnemonic
         ; addressing = Accumulator
         ; opcode = 0x0A
         ; bytes = 1
         ; cycles = 2
         ; pp = "ASL"
         }
       | LSR ->
         { mnemonic
         ; addressing = Accumulator
         ; opcode = 0x4A
         ; bytes = 1
         ; cycles = 2
         ; pp = "LSR"
         }
       | ROL ->
         { mnemonic
         ; addressing = Accumulator
         ; opcode = 0x2A
         ; bytes = 1
         ; cycles = 2
         ; pp = "ROL"
         }
       | ROR ->
         { mnemonic
         ; addressing = Accumulator
         ; opcode = 0x6A
         ; bytes = 1
         ; cycles = 2
         ; pp = "ROR"
         }
       | _ -> failwith "Invalid Accumulator instruction")
    (* Immediate *)
    | Immediate ->
      (match mnemonic with
       | ADC ->
         { mnemonic
         ; addressing = Immediate
         ; opcode = 0x69
         ; bytes = 2
         ; cycles = 2
         ; pp = "ADC"
         }
       | AND ->
         { mnemonic
         ; addressing = Immediate
         ; opcode = 0x29
         ; bytes = 2
         ; cycles = 2
         ; pp = "AND"
         }
       | CMP ->
         { mnemonic
         ; addressing = Immediate
         ; opcode = 0xC9
         ; bytes = 2
         ; cycles = 2
         ; pp = "CMP"
         }
       | CPX ->
         { mnemonic
         ; addressing = Immediate
         ; opcode = 0xE0
         ; bytes = 2
         ; cycles = 2
         ; pp = "CPX"
         }
       | CPY ->
         { mnemonic
         ; addressing = Immediate
         ; opcode = 0xC0
         ; bytes = 2
         ; cycles = 2
         ; pp = "CPY"
         }
       | EOR ->
         { mnemonic
         ; addressing = Immediate
         ; opcode = 0x49
         ; bytes = 2
         ; cycles = 2
         ; pp = "EOR"
         }
       | LDA ->
         { mnemonic
         ; addressing = Immediate
         ; opcode = 0xA9
         ; bytes = 2
         ; cycles = 2
         ; pp = "LDA"
         }
       | LDX ->
         { mnemonic
         ; addressing = Immediate
         ; opcode = 0xA2
         ; bytes = 2
         ; cycles = 2
         ; pp = "LDX"
         }
       | LDY ->
         { mnemonic
         ; addressing = Immediate
         ; opcode = 0xA0
         ; bytes = 2
         ; cycles = 2
         ; pp = "LDY"
         }
       | ORA ->
         { mnemonic
         ; addressing = Immediate
         ; opcode = 0x09
         ; bytes = 2
         ; cycles = 2
         ; pp = "ORA"
         }
       | SBC ->
         { mnemonic
         ; addressing = Immediate
         ; opcode = 0xE9
         ; bytes = 2
         ; cycles = 2
         ; pp = "SBC"
         }
       | _ -> failwith "Invalid Immediate instruction")
    (* Absolute *)
    | Absolute ->
      (match mnemonic with
       | JMP ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0x4C
         ; bytes = 3
         ; cycles = 3
         ; pp = "JMP"
         }
       | JSR ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0x20
         ; bytes = 3
         ; cycles = 6
         ; pp = "JSR"
         }
       (* Absolute or Zero Page*)
       | ADC ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0x6D
         ; bytes = 3
         ; cycles = 4
         ; pp = "ADC"
         }
       | AND ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0x2D
         ; bytes = 3
         ; cycles = 4
         ; pp = "AND"
         }
       | ASL ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0x0E
         ; bytes = 3
         ; cycles = 6
         ; pp = "ASL"
         }
       | BIT ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0x2C
         ; bytes = 3
         ; cycles = 4
         ; pp = "BIT"
         }
       | CMP ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0xCD
         ; bytes = 3
         ; cycles = 4
         ; pp = "CMP"
         }
       | CPX ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0xEC
         ; bytes = 3
         ; cycles = 4
         ; pp = "CPX"
         }
       | CPY ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0xCC
         ; bytes = 3
         ; cycles = 4
         ; pp = "CPY"
         }
       | DEC ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0xCE
         ; bytes = 3
         ; cycles = 6
         ; pp = "DEC"
         }
       | EOR ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0x4D
         ; bytes = 3
         ; cycles = 4
         ; pp = "EOR"
         }
       | INC ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0xEE
         ; bytes = 3
         ; cycles = 6
         ; pp = "INC"
         }
       | LDA ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0xAD
         ; bytes = 3
         ; cycles = 4
         ; pp = "LDA"
         }
       | LDX ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0xAE
         ; bytes = 3
         ; cycles = 4
         ; pp = "LDX"
         }
       | LDY ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0xAC
         ; bytes = 3
         ; cycles = 4
         ; pp = "LDY"
         }
       | LSR ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0x4E
         ; bytes = 3
         ; cycles = 6
         ; pp = "LSR"
         }
       | ORA ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0x0D
         ; bytes = 3
         ; cycles = 4
         ; pp = "ORA"
         }
       | ROL ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0x2E
         ; bytes = 3
         ; cycles = 6
         ; pp = "ROL"
         }
       | ROR ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0x6E
         ; bytes = 3
         ; cycles = 6
         ; pp = "ROR"
         }
       | SBC ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0xED
         ; bytes = 3
         ; cycles = 4
         ; pp = "SBC"
         }
       | STA ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0x8D
         ; bytes = 3
         ; cycles = 4
         ; pp = "STA"
         }
       | STX ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0x8E
         ; bytes = 3
         ; cycles = 4
         ; pp = "STX"
         }
       | STY ->
         { mnemonic
         ; addressing = Absolute
         ; opcode = 0x8C
         ; bytes = 3
         ; cycles = 4
         ; pp = "STY"
         }
       | _ -> failwith "Invalid Absolute instruction")
      (* Zero Page*)
    | Zeropage ->
      (match mnemonic with
       | ADC ->
         { mnemonic
         ; addressing = Zeropage
         ; opcode = 0x65
         ; bytes = 2
         ; cycles = 3
         ; pp = "ADC"
         }
       | AND ->
         { mnemonic
         ; addressing = Zeropage
         ; opcode = 0x25
         ; bytes = 2
         ; cycles = 3
         ; pp = "AND"
         }
       | ASL ->
         { mnemonic
         ; addressing = Zeropage
         ; opcode = 0x06
         ; bytes = 2
         ; cycles = 5
         ; pp = "ASL"
         }
       | BIT ->
         { mnemonic
         ; addressing = Zeropage
         ; opcode = 0x24
         ; bytes = 2
         ; cycles = 3
         ; pp = "BIT"
         }
       | CMP ->
         { mnemonic
         ; addressing = Zeropage
         ; opcode = 0xC5
         ; bytes = 2
         ; cycles = 3
         ; pp = "CMP"
         }
       | CPX ->
         { mnemonic
         ; addressing = Zeropage
         ; opcode = 0xE4
         ; bytes = 2
         ; cycles = 3
         ; pp = "CPX"
         }
       | CPY ->
         { mnemonic
         ; addressing = Zeropage
         ; opcode = 0xC4
         ; bytes = 2
         ; cycles = 3
         ; pp = "CPY"
         }
       | DEC ->
         { mnemonic
         ; addressing = Zeropage
         ; opcode = 0xC6
         ; bytes = 2
         ; cycles = 5
         ; pp = "DEC"
         }
       | EOR ->
         { mnemonic
         ; addressing = Zeropage
         ; opcode = 0x45
         ; bytes = 2
         ; cycles = 3
         ; pp = "EOR"
         }
       | INC ->
         { mnemonic
         ; addressing = Zeropage
         ; opcode = 0xE6
         ; bytes = 2
         ; cycles = 5
         ; pp = "INC"
         }
       | LDA ->
         { mnemonic
         ; addressing = Zeropage
         ; opcode = 0xA5
         ; bytes = 2
         ; cycles = 3
         ; pp = "LDA"
         }
       | LDX ->
         { mnemonic
         ; addressing = Zeropage
         ; opcode = 0xA6
         ; bytes = 2
         ; cycles = 3
         ; pp = "LDX"
         }
       | LDY ->
         { mnemonic
         ; addressing = Zeropage
         ; opcode = 0xA4
         ; bytes = 2
         ; cycles = 3
         ; pp = "LDY"
         }
       | LSR ->
         { mnemonic
         ; addressing = Zeropage
         ; opcode = 0x46
         ; bytes = 2
         ; cycles = 5
         ; pp = "LRS"
         }
       | ORA ->
         { mnemonic
         ; addressing = Zeropage
         ; opcode = 0x05
         ; bytes = 2
         ; cycles = 3
         ; pp = "ORA"
         }
       | ROL ->
         { mnemonic
         ; addressing = Zeropage
         ; opcode = 0x26
         ; bytes = 2
         ; cycles = 5
         ; pp = "ROL"
         }
       | ROR ->
         { mnemonic
         ; addressing = Zeropage
         ; opcode = 0x66
         ; bytes = 2
         ; cycles = 5
         ; pp = "ROR"
         }
       | SBC ->
         { mnemonic
         ; addressing = Zeropage
         ; opcode = 0xE5
         ; bytes = 2
         ; cycles = 3
         ; pp = "SBC"
         }
       | STA ->
         { mnemonic
         ; addressing = Zeropage
         ; opcode = 0x85
         ; bytes = 2
         ; cycles = 3
         ; pp = "STA"
         }
       | STX ->
         { mnemonic
         ; addressing = Zeropage
         ; opcode = 0x86
         ; bytes = 2
         ; cycles = 3
         ; pp = "STX"
         }
       | STY ->
         { mnemonic
         ; addressing = Zeropage
         ; opcode = 0x84
         ; bytes = 2
         ; cycles = 3
         ; pp = "STY"
         }
       | _ -> failwith "Invalid Zeropage instruction")
    (* AbsoluteX *)
    | AbsoluteX ->
      (match mnemonic with
       | ADC ->
         { mnemonic
         ; addressing = AbsoluteX
         ; opcode = 0x7D
         ; bytes = 3
         ; cycles = 4
         ; pp = "ADC"
         }
       | AND ->
         { mnemonic
         ; addressing = AbsoluteX
         ; opcode = 0x3D
         ; bytes = 3
         ; cycles = 4
         ; pp = "AND"
         }
       | ASL ->
         { mnemonic
         ; addressing = AbsoluteX
         ; opcode = 0x1E
         ; bytes = 3
         ; cycles = 7
         ; pp = "ASL"
         }
       | CMP ->
         { mnemonic
         ; addressing = AbsoluteX
         ; opcode = 0xDD
         ; bytes = 3
         ; cycles = 4
         ; pp = "CMP"
         }
       | DEC ->
         { mnemonic
         ; addressing = AbsoluteX
         ; opcode = 0xDE
         ; bytes = 3
         ; cycles = 7
         ; pp = "DEC"
         }
       | EOR ->
         { mnemonic
         ; addressing = AbsoluteX
         ; opcode = 0x5D
         ; bytes = 3
         ; cycles = 4
         ; pp = "EOR"
         }
       | INC ->
         { mnemonic
         ; addressing = AbsoluteX
         ; opcode = 0xFE
         ; bytes = 3
         ; cycles = 7
         ; pp = "INC"
         }
       | LDA ->
         { mnemonic
         ; addressing = AbsoluteX
         ; opcode = 0xBD
         ; bytes = 3
         ; cycles = 4
         ; pp = "LDA"
         }
       | LDY ->
         { mnemonic
         ; addressing = AbsoluteX
         ; opcode = 0xBC
         ; bytes = 3
         ; cycles = 4
         ; pp = "LDY"
         }
       | LSR ->
         { mnemonic
         ; addressing = AbsoluteX
         ; opcode = 0x5E
         ; bytes = 3
         ; cycles = 7
         ; pp = "LSR"
         }
       | ORA ->
         { mnemonic
         ; addressing = AbsoluteX
         ; opcode = 0x1D
         ; bytes = 3
         ; cycles = 4
         ; pp = "ORA"
         }
       | ROL ->
         { mnemonic
         ; addressing = AbsoluteX
         ; opcode = 0x3E
         ; bytes = 3
         ; cycles = 7
         ; pp = "ROL"
         }
       | ROR ->
         { mnemonic
         ; addressing = AbsoluteX
         ; opcode = 0x7E
         ; bytes = 3
         ; cycles = 7
         ; pp = "ROR"
         }
       | SBC ->
         { mnemonic
         ; addressing = AbsoluteX
         ; opcode = 0xFD
         ; bytes = 3
         ; cycles = 4
         ; pp = "SBC"
         }
       | STA ->
         { mnemonic
         ; addressing = AbsoluteX
         ; opcode = 0x9D
         ; bytes = 3
         ; cycles = 5
         ; pp = "STA"
         }
       | _ -> failwith "Invalid AbsoluteX instruction")
    (* zeropage *)
    | ZeropageX ->
      (match mnemonic with
       | ADC ->
         { mnemonic
         ; addressing = ZeropageX
         ; opcode = 0x75
         ; bytes = 2
         ; cycles = 4
         ; pp = "ADC"
         }
       | AND ->
         { mnemonic
         ; addressing = ZeropageX
         ; opcode = 0x35
         ; bytes = 2
         ; cycles = 4
         ; pp = "AND"
         }
       | ASL ->
         { mnemonic
         ; addressing = ZeropageX
         ; opcode = 0x16
         ; bytes = 2
         ; cycles = 6
         ; pp = "ASL"
         }
       | CMP ->
         { mnemonic
         ; addressing = ZeropageX
         ; opcode = 0xD5
         ; bytes = 2
         ; cycles = 4
         ; pp = "CMP"
         }
       | DEC ->
         { mnemonic
         ; addressing = ZeropageX
         ; opcode = 0xD6
         ; bytes = 2
         ; cycles = 6
         ; pp = "DEC"
         }
       | EOR ->
         { mnemonic
         ; addressing = ZeropageX
         ; opcode = 0x55
         ; bytes = 2
         ; cycles = 4
         ; pp = "EOR"
         }
       | INC ->
         { mnemonic
         ; addressing = ZeropageX
         ; opcode = 0xF6
         ; bytes = 2
         ; cycles = 6
         ; pp = "INC"
         }
       | LDA ->
         { mnemonic
         ; addressing = ZeropageX
         ; opcode = 0xB5
         ; bytes = 2
         ; cycles = 4
         ; pp = "LDA"
         }
       | LDY ->
         { mnemonic
         ; addressing = ZeropageX
         ; opcode = 0xB4
         ; bytes = 2
         ; cycles = 4
         ; pp = "LDY"
         }
       | LSR ->
         { mnemonic
         ; addressing = ZeropageX
         ; opcode = 0x56
         ; bytes = 2
         ; cycles = 6
         ; pp = "LSR"
         }
       | ORA ->
         { mnemonic
         ; addressing = ZeropageX
         ; opcode = 0x15
         ; bytes = 2
         ; cycles = 4
         ; pp = "ORA"
         }
       | ROL ->
         { mnemonic
         ; addressing = ZeropageX
         ; opcode = 0x36
         ; bytes = 2
         ; cycles = 6
         ; pp = "ROL"
         }
       | ROR ->
         { mnemonic
         ; addressing = ZeropageX
         ; opcode = 0x76
         ; bytes = 2
         ; cycles = 6
         ; pp = "ROR"
         }
       | SBC ->
         { mnemonic
         ; addressing = ZeropageX
         ; opcode = 0xF5
         ; bytes = 2
         ; cycles = 4
         ; pp = "SBC"
         }
       | STA ->
         { mnemonic
         ; addressing = ZeropageX
         ; opcode = 0x95
         ; bytes = 2
         ; cycles = 4
         ; pp = "STA"
         }
       | STY ->
         { mnemonic
         ; addressing = ZeropageX
         ; opcode = 0x94
         ; bytes = 2
         ; cycles = 4
         ; pp = "STY"
         }
       | _ -> failwith "Invalid ZeropageX Instruction")
    | AbsoluteY ->
      (match mnemonic with
       | LDA ->
         { mnemonic
         ; addressing = AbsoluteY
         ; opcode = 0xB9
         ; bytes = 3
         ; cycles = 4
         ; pp = "LDA"
         }
       | ADC ->
         { mnemonic
         ; addressing = AbsoluteY
         ; opcode = 0x79
         ; bytes = 3
         ; cycles = 4
         ; pp = "ADC"
         }
       | AND ->
         { mnemonic
         ; addressing = AbsoluteY
         ; opcode = 0x39
         ; bytes = 3
         ; cycles = 4
         ; pp = "AND"
         }
       | CMP ->
         { mnemonic
         ; addressing = AbsoluteY
         ; opcode = 0xD9
         ; bytes = 3
         ; cycles = 4
         ; pp = "CMP"
         }
       | EOR ->
         { mnemonic
         ; addressing = AbsoluteY
         ; opcode = 0x59
         ; bytes = 3
         ; cycles = 4
         ; pp = "EOR"
         }
       | ORA ->
         { mnemonic
         ; addressing = AbsoluteY
         ; opcode = 0x19
         ; bytes = 3
         ; cycles = 4
         ; pp = "ORA"
         }
       | SBC ->
         { mnemonic
         ; addressing = AbsoluteY
         ; opcode = 0xF9
         ; bytes = 3
         ; cycles = 4
         ; pp = "SBC"
         }
       | STA ->
         { mnemonic
         ; addressing = AbsoluteY
         ; opcode = 0x99
         ; bytes = 3
         ; cycles = 5
         ; pp = "STA"
         }
       | LDX ->
         { mnemonic
         ; addressing = AbsoluteY
         ; opcode = 0xBE
         ; bytes = 3
         ; cycles = 4
         ; pp = "LDX"
         }
       | _ -> failwith "Invalid AbsoluteY instruction")
    (* ZeropageY *)
    | ZeropageY ->
      (match mnemonic with
       | LDX ->
         { mnemonic
         ; addressing = ZeropageY
         ; opcode = 0xB6
         ; bytes = 2
         ; cycles = 4
         ; pp = "LDX"
         }
       | STX ->
         { mnemonic
         ; addressing = ZeropageY
         ; opcode = 0x96
         ; bytes = 2
         ; cycles = 4
         ; pp = "STX"
         }
       | _ -> failwith "Invalid ZeropageY instruction")
    | PreIndexIndirect ->
      (match mnemonic with
       | LDA ->
         { mnemonic
         ; addressing = PreIndexIndirect
         ; opcode = 0xA1
         ; bytes = 2
         ; cycles = 6
         ; pp = "LDA"
         }
       | ADC ->
         { mnemonic
         ; addressing = PreIndexIndirect
         ; opcode = 0x61
         ; bytes = 2
         ; cycles = 6
         ; pp = "ADC"
         }
       | AND ->
         { mnemonic
         ; addressing = PreIndexIndirect
         ; opcode = 0x21
         ; bytes = 2
         ; cycles = 6
         ; pp = "AND"
         }
       | CMP ->
         { mnemonic
         ; addressing = PreIndexIndirect
         ; opcode = 0xC1
         ; bytes = 2
         ; cycles = 6
         ; pp = "CMP"
         }
       | EOR ->
         { mnemonic
         ; addressing = PreIndexIndirect
         ; opcode = 0x41
         ; bytes = 2
         ; cycles = 6
         ; pp = "EOR"
         }
       | ORA ->
         { mnemonic
         ; addressing = PreIndexIndirect
         ; opcode = 0x01
         ; bytes = 2
         ; cycles = 6
         ; pp = "ORA"
         }
       | SBC ->
         { mnemonic
         ; addressing = PreIndexIndirect
         ; opcode = 0xE1
         ; bytes = 2
         ; cycles = 6
         ; pp = "SBC"
         }
       | STA ->
         { mnemonic
         ; addressing = PreIndexIndirect
         ; opcode = 0x81
         ; bytes = 2
         ; cycles = 4
         ; pp = "STA"
         }
       | _ -> failwith "Invalid PreIndexIndirect instrution")
    (* indirect, Y indexed*)
    | PostIndexIndirect ->
      (match mnemonic with
       | LDA ->
         { mnemonic
         ; addressing = PostIndexIndirect
         ; opcode = 0xB1
         ; bytes = 2
         ; cycles = 5
         ; pp = "LDA"
         }
       | ADC ->
         { mnemonic
         ; addressing = PostIndexIndirect
         ; opcode = 0x71
         ; bytes = 2
         ; cycles = 5
         ; pp = "ADC"
         }
       | AND ->
         { mnemonic
         ; addressing = PostIndexIndirect
         ; opcode = 0x31
         ; bytes = 2
         ; cycles = 5
         ; pp = "AND"
         }
       | CMP ->
         { mnemonic
         ; addressing = PostIndexIndirect
         ; opcode = 0xD1
         ; bytes = 2
         ; cycles = 5
         ; pp = "CMP"
         }
       | EOR ->
         { mnemonic
         ; addressing = PostIndexIndirect
         ; opcode = 0x51
         ; bytes = 2
         ; cycles = 5
         ; pp = "EOR"
         }
       | ORA ->
         { mnemonic
         ; addressing = PostIndexIndirect
         ; opcode = 0x11
         ; bytes = 2
         ; cycles = 5
         ; pp = "ORA"
         }
       | SBC ->
         { mnemonic
         ; addressing = PostIndexIndirect
         ; opcode = 0xF1
         ; bytes = 2
         ; cycles = 5
         ; pp = "SBC"
         }
       | STA ->
         { mnemonic
         ; addressing = PostIndexIndirect
         ; opcode = 0x91
         ; bytes = 2
         ; cycles = 6
         ; pp = "STA"
         }
       | _ -> failwith "Invalid PostIndexIndirect instruction")
    (* Relative *)
    | Relative ->
      (match mnemonic with
       | BCC ->
         { mnemonic
         ; addressing = Relative
         ; opcode = 0x90
         ; bytes = 2
         ; cycles = 2
         ; pp = "BCC"
         }
       | BCS ->
         { mnemonic
         ; addressing = Relative
         ; opcode = 0xB0
         ; bytes = 2
         ; cycles = 2
         ; pp = "BCS"
         }
       | BEQ ->
         { mnemonic
         ; addressing = Relative
         ; opcode = 0xF0
         ; bytes = 2
         ; cycles = 2
         ; pp = "BEQ"
         }
       | BMI ->
         { mnemonic
         ; addressing = Relative
         ; opcode = 0x30
         ; bytes = 2
         ; cycles = 2
         ; pp = "BMI"
         }
       | BNE ->
         { mnemonic
         ; addressing = Relative
         ; opcode = 0xD0
         ; bytes = 2
         ; cycles = 2
         ; pp = "BNE"
         }
       | BPL ->
         { mnemonic
         ; addressing = Relative
         ; opcode = 0x10
         ; bytes = 2
         ; cycles = 2
         ; pp = "BPL"
         }
       | BVC ->
         { mnemonic
         ; addressing = Relative
         ; opcode = 0x50
         ; bytes = 2
         ; cycles = 2
         ; pp = "BVC"
         }
       | BVS ->
         { mnemonic
         ; addressing = Relative
         ; opcode = 0x70
         ; bytes = 2
         ; cycles = 2
         ; pp = "BVS"
         }
       | _ -> failwith "Invalid Relative instruction")
    (* Implied *)
    | Implied ->
      (match mnemonic with
       | BRK ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0x00
         ; bytes = 1
         ; cycles = 7
         ; pp = "BRK"
         }
       | CLC ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0x18
         ; bytes = 1
         ; cycles = 2
         ; pp = "CLC"
         }
       | CLD ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0xD8
         ; bytes = 1
         ; cycles = 2
         ; pp = "CLD"
         }
       | CLI ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0x58
         ; bytes = 1
         ; cycles = 2
         ; pp = "CLI"
         }
       | CLV ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0xB8
         ; bytes = 1
         ; cycles = 2
         ; pp = "CLV"
         }
       | DEX ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0xCA
         ; bytes = 1
         ; cycles = 2
         ; pp = "DEX"
         }
       | DEY ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0x88
         ; bytes = 1
         ; cycles = 2
         ; pp = "DEY"
         }
       | INX ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0xE8
         ; bytes = 1
         ; cycles = 2
         ; pp = "INX"
         }
       | INY ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0xC8
         ; bytes = 1
         ; cycles = 2
         ; pp = "INY"
         }
       | NOP ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0xEA
         ; bytes = 1
         ; cycles = 2
         ; pp = "NOP"
         }
       | PHA ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0x48
         ; bytes = 1
         ; cycles = 3
         ; pp = "PHA"
         }
       | PHP ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0x08
         ; bytes = 1
         ; cycles = 3
         ; pp = "PHP"
         }
       | PLA ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0x68
         ; bytes = 1
         ; cycles = 4
         ; pp = "PLA"
         }
       | PLP ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0x28
         ; bytes = 1
         ; cycles = 4
         ; pp = "PLP"
         }
       | RTI ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0x40
         ; bytes = 1
         ; cycles = 6
         ; pp = "RTI"
         }
       | RTS ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0x60
         ; bytes = 1
         ; cycles = 6
         ; pp = "RTS"
         }
       | SEC ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0x38
         ; bytes = 1
         ; cycles = 2
         ; pp = "SEC"
         }
       | SED ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0xF8
         ; bytes = 1
         ; cycles = 2
         ; pp = "SED"
         }
       | SEI ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0x78
         ; bytes = 1
         ; cycles = 2
         ; pp = "SEI"
         }
       | TAX ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0xAA
         ; bytes = 1
         ; cycles = 2
         ; pp = "TAX"
         }
       | TAY ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0xA8
         ; bytes = 1
         ; cycles = 2
         ; pp = "TAY"
         }
       | TSX ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0xBA
         ; bytes = 1
         ; cycles = 2
         ; pp = "TSX"
         }
       | TXA ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0x8A
         ; bytes = 1
         ; cycles = 2
         ; pp = "TXA"
         }
       | TXS ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0x9A
         ; bytes = 1
         ; cycles = 2
         ; pp = "TXS"
         }
       | TYA ->
         { mnemonic
         ; addressing = Implied
         ; opcode = 0x98
         ; bytes = 1
         ; cycles = 2
         ; pp = "TYA"
         }
       | _ -> failwith "Invalid Implied instruction")
      (* Indirect *)
    | Indirect ->
      (match mnemonic with
       | JMP ->
         { mnemonic
         ; addressing = Indirect
         ; opcode = 0x6C
         ; bytes = 3
         ; cycles = 5
         ; pp = "JMP"
         }
       | _ -> failwith "Invalid Indirect instruction")
  ;;
end
