; Tell Relaunch64 editor what start address there is for the program
; start=$1000

*=$0801

  !byte $0c, $08, $0a, $00, $9e, $20
  !byte $34, $30, $39, $36, $00, $00
  !byte $00

*=$1000

.Init	
        LDA #0          ; Set character colors in horizontal bars on the screen 

        LDA #%01111111
        STA $DC0D	      ; "Switch off" interrupts signals from CIA-1

        ;LDA $D01A      ; enable raster irq
        ;ORA #$01
        ;STA $D01A

        LDA $D011
        AND #%01111111
        STA $D011	      ; Clear most significant bit in VIC's raster register
        
        LDA #92
        STA $D012       ; Set the raster line number where interrupt should occur
        
        LDA #<.Irq
        STA $0314
        LDA #>.Irq
        STA $0315	      ; Set the interrupt vector to point to interrupt service routine below
        
        LDA #%00000001
        STA $D01A	      ; Enable raster interrupt signals from VIC
        
        CLI
        
        RTS             ; Initialization done; return to BASIC

.Irq	
        LDA #7
        STA $D020	      ; Set screen border color to yellow
        STA $D021	      ; Set screen color to yellow
        
        LDA #8
        STA $D020	      ; Set screen border color to yellow
        STA $D021	      ; Set screen color to yellow
        
        LDX #10
.Pause	
        DEX
        BNE .Pause     ; Empty loop that "does nothing" for a little under a half millisecond
        
        LDA #0
        STA $D020	      ; Set screen border color to black
        STA $D021	      ; Set screen color to black
        
        ASL $D019	      ; "Acknowledge" the interrupt by clearing the VIC's interrupt flag.
        
        JMP $EA31	      ; Jump into KERNAL's standard interrupt service routine to handle keyboard scan, cursor display etc.

.message
    !pet "         Damn, that's smooth!           "
    
.charactercolors
    !byte $00, $00