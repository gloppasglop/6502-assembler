      lda #$00
      sta $10
      lda #$02
      sta $11
      
      ldx #$06
      
      ldy #$00

      lda #$07

loop: sta ($10),y
      iny
      bne loop
      
      inc $11
      cpx $11
      bne loop
      
      brk          ; done - return to debugger