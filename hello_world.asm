; Tell Relaunch64 editor what start address there is for the program
; start=$1000

*=$0801

  !byte $0c, $08, $0a, $00, $9e, $20
  !byte $34, $30, $39, $36, $00, $00
  !byte $00

*=$1000

  ldx #$00
  
loop 
  lda message,x
  and #$3f
  sta $0400,x
  inx
  cpx #$0c
  bne loop
  
  rts
  
message
    !pet "Hello World!"