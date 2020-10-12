
;==============================================================================
; ACME_TEMPLATE - copy(l)eft 2018 http://harald.ist.org/
;==============================================================================

;;!cpu 6510                     ; Enable illegal opcodes
!to "../acme_template cbm       ; Output file
!sl "../acme_template.sym               ; Symbols table
!convtab PET                    ; Character constants are PETSCII

!macro S_PROGRAM_VERSION .dummy {
        !pet "v0.0a"
}

;==============================================================================
!zone BASIC_START_PROGRAM
;==============================================================================
*= $0801                        ; Load point $0801 (BASIC START)
_FSTART                         ; This binary must begin with the bytes
                                ; representing the BASIC program: 0 SYS2061
BASIC_program
        !byte $0b,$08           ; $0801 Pointer to next line
        !byte $00,$00           ; $0803 Line number (0)
        !byte $9e               ; $0805 SYS
!byte 48+(entry_point/1000)%10  ; Decimal address of program entry point
!byte 48+(entry_point/100)%10
!byte 48+(entry_point/10)%10
!byte 48+(entry_point/1)%10
        !byte $00               ; $080a End of BASIC line
        !byte $00,$00           ; $080b End of BASIC program
entry_point     ;JMP boot       ; $080d First byte after the BASIC program


;==============================================================================
!zone BOOT
;==============================================================================
boot            JSR init
                JSR main
                JSR exit
                RTS                     ; Return to BASIC


;==============================================================================
!zone INIT
;==============================================================================

init
;==============================================================================
                JSR clear_heap
                JSR save_zeropage

                LDA #$00                ; Black
                STA $d021               ; Border
                STA $d020               ; Background

                LDA #$0e                ; Lower case font
                JSR $ffd2               ; BSOUT
                LDA #$08                ; Font case toggle disabled
                JSR $ffd2               ; BSOUT

                RTS


;==============================================================================
!zone MAIN
;==============================================================================

main
;------------------------------------------------------------------------------
                RTS


;==============================================================================
!zone EXIT
;==============================================================================

exit
;------------------------------------------------------------------------------
                LDA #$8e                ; Upper case font
                JSR $ffd2               ; BSOUT
                LDA #$09                ; Font case toggle enable
                JSR $ffd2               ; BSOUT

                LDA #$0e                ; Light blue
                STA $0286               ; Text color
                JSR $e544               ; CLEAR HOME

                LDA #$0e                ; Blue
                STA $d020               ; Background
                LDA #$06                ; Light blue
                STA $d021               ; Border

                ;LDX #<s_exit_message
                ;LDA #>s_exit_message
                ;JSR cputs
;------------------------------------------------------------------------------
.loop           LDA $cb                 ; Currently pressed key
                CMP #$40                ; $40 = no key pressed
                BNE .loop               ; Wait until key released
                LDA #$00
                STA $c6                 ; Nr. keys in input buffer

                JSR restore_zeropage    ; Clean up the mess, we created

                LDY # 5
                LDX # 0
                STY $d6                 ; Store x-pos
                STX $d3                 ; Store y-pos
                JSR $e56c               ; Set cursor


                RTS


;==============================================================================
!zone SAVE_RESTORE_ZEROPAGE
;==============================================================================
; Save/restore $0000..$00ff to/from heap. Enables return to BASIC.


save_zeropage
;------------------------------------------------------------------------------
                LDY #$00
.save_loop      LDA $00,Y
                STA zp_backup,Y
                INY
                BNE .save_loop
                RTS


restore_zeropage
;------------------------------------------------------------------------------
                TYA
                PHA

                LDY #$00
.restore_loop   LDA zp_backup,Y
                STA $00,Y
                INY
                BNE .restore_loop

                PLA
                TAY
                RTS


;==============================================================================
!zone CLEAR_HEAP
;==============================================================================

        .tptr = $f7

clear_heap
;------------------------------------------------------------------------------
                ; Clear heap variables

                LDA .tptr
                PHA
                LDA .tptr+1
                PHA

                LDY #<heap_vars_start

                LDA #>heap_vars_start
                STA .tptr+1
                LDA # 0
                STA .tptr

.loop           STA (.tptr),Y
                INY
                BNE .loop

                INC .tptr+1

                LDX #>heap_vars_end
                CPX .tptr+1
                BCS .loop

                PLA
                STA .tptr+1
                PLA
                STA .tptr

                RTS


;==============================================================================
!zone STATIC_DATA
;==============================================================================

s_exit_message  !pet petscii_home, petscii_lblue, petscii_rvs_off, 13
                !pet "asm-template", 13
                !pet "------------------------", 13
                !pet "http://harald.ist.org/", 13
                !pet "copy(l)eft 2019"
                !pet 0


;==============================================================================
!zone HEAP
;==============================================================================
heap_vars_start
zp_backup       *=zp_backup     + 256
heap_vars_end


;==============================================================================
!eof