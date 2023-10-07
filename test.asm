* = $1000
      asl
     lda #$00
      sta $10
      lda #$02
      sta $11
      
      ldx #$06
      
      ldy #$00

      lda #$07

* = $2000
loop: sta ($10),y
      iny
      bne loop
      
      inc $11
      cpx $12                   ; Some comments
      bne loop

      sta 12+2
      sta [$12*$2]
      sta 12/5
      sta <$ABCD
      sta >$ABCD
      
      brk          ; done - return to debugger

DCB $99 $84 $F3 $1F

      nop
DCB $FF
      nop