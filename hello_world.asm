// Tell Relaunch64 editor what start address there is for the program
// start=$1000

*=$0801 "basic"
    .byte $0C, $08
    .byte $0A, $00, $9E, $20, $32, $30, $36, $34, $00        // BASIC: 10 SYS 2064      // #2064 = $0810
    .byte $00, $00                                           // end of BASIC program

*=$0810 "main"
    jsr $e544 // clear screen

    ldx #$00
loop:
    lda message,x
    sta $0400,x
    inx
    cpx #$0c
    bne loop

    rts // return to basic

message:
    .encoding "screencode_upper"
    .text "HELLO WORLD!"