; Tell Relaunch64 editor what start address there is for the program
; start=$0810

COLOR_BLACK         = $00
COLOR_WHITE         = $01
COLOR_RED           = $02
COLOR_CYAN          = $03
COLOR_PURPLE        = $04
COLOR_GREEN         = $05
COLOR_BLUE          = $06
COLOR_YELLOW        = $07
COLOR_ORANGE        = $08
COLOR_BROWN         = $09
COLOR_PINK          = $0A
COLOR_DARK_GREY     = $0B
COLOR_GREY          = $0C
COLOR_LIGHT_GREEN   = $0D
COLOR_LIGHT_BLUE    = $0E
COLOR_LIGHT_GREY    = $0F

!addr   SYSTEM_IRQ_HANDLER = $ea81

!addr   address_font = $3800 ; Last 2k of bank 3
!addr   address_font_pointer = $D018

!addr   address_sid_music_init = $1000
!addr   address_sid_music_play = address_sid_music_init + 3

constant_columns_per_line = 40

constant_scroll_line_index = 23 ; Scroll double line text message with it's top on this line (zero indexed from top) [0-23]
!addr   address_scroll_line_top = $0400 + constant_columns_per_line * constant_scroll_line_index
!addr   address_scroll_line_bottom = address_scroll_line_top + constant_columns_per_line 

constant_static_text_line_index = 0 ; Static text message with it's top on this line (zero indexed from top) [0-23]
!addr   address_static_message_text_scr_pos = $0400 + constant_columns_per_line * constant_static_text_line_index

constant_color_yellow = 7
constant_color_black = 0

!addr   address_border_color = $D020
!addr   address_screen_color = $D021

!addr   address_horizontal_scroll_register = $D016
!addr   address_horizontal_scroll_value = $03            ; Current horizontal pixel scroll value. Stored at zero page

constant_horizontal_scroll_delay = 0                     ; Scroll delay value constant [0-128]  (unit: rendered frames, 1/50th of a second)
!addr	address_horizontal_scroll_delay_counter = $04   ; Scroll delay counter. Stored at zero page

!addr	address_scroll_message_character_offset = $05   ; Character offset in scroll message. Stored at zero page

constant_text_color_scroll_delay = 1                    ; Scroll delay value constant [0-255] (unit: rendered frames, 1/50th of a second)
!addr   address_text_color_scroll_delay_counter = $06   ; Text color scroll delay counter. Stored at zero page
!addr   address_text_color_scroll_offset = $07          ; Offset in text color list. Stored at zero page
!addr   address_text_color_scroll_line_top = $D800 + constant_columns_per_line * constant_scroll_line_index
!addr   address_text_color_scroll_line_bottom = address_text_color_scroll_line_top + constant_columns_per_line 

*=$0801

        ;!byte $01, $08                                          ; "File header" (?)
        !byte $0C, $08                                                          
        !byte $0A, $00, $9E, $20, $32, $30, $36, $34, $00        ; BASIC: 10 SYS 2064      ; #2064 = $0810
        !byte $00, $00                                           ; end of BASIC program

*=$0810

Init    
        
        LDY #0 - 1
        STY address_scroll_message_character_offset ; Reset scroll message to first character position
        STY address_text_color_scroll_offset        ; Reset text color scroll offset to first color position
	
        LDY #7        
        STY address_horizontal_scroll_value         ; Set initial horizontal screen pixel scroll value
        
        LDY #constant_horizontal_scroll_delay
        STY address_horizontal_scroll_delay_counter ; Reset scroll delay counter to delay max value
        
        ; Set character font pointer to point to demo font
        LDA address_font_pointer
        ORA #$0e                    ; set chars location to $3800 for displaying the custom font
        STA address_font_pointer    ; Bits 1-3 ($400+512bytes * low nibble value) of $d018 sets char location $400 + $200*$0E = $3800
        
        JSR address_sid_music_init
        
        ; Set 38 column mode
        LDA address_horizontal_scroll_register
        AND #%11110111
        STA address_horizontal_scroll_register
        
        JSR clear_screen

        jsr print_static_message

        SEI
        LDA #%01111111
        STA $DC0D	      ; "Switch off" interrupts signals from CIA-1

        ;LDA $D01A      ; enable VIC-II Raster Beam IRQ
        ;ORA #$01
        ;STA $D01A

        LDA #%00000001
        STA $D01A	      ; Enable raster interrupt signals from VIC

        LDA $D011
        AND #%01111111
        STA $D011	      ; Clear most significant bit in VIC's raster register (for raster interrupt on upper part of screen, above line #256)
        
        LDA #$00        ; Interrupt on this raster line
        STA $D012       ; Set the raster line number where interrupt should occur
        
        LDA #<irq1
        STA $0314
        LDA #>irq1
        STA $0315	      ; Set the interrupt vector to point to interrupt service routine below
        
        CLI
        
        ;RTS             ; Initialization done; return to BASIC
no_exit        
    
        jmp no_exit

irq1	
        ; TODO fill with good shit!
        
        ; DEBUG: set border color for now        
        LDA #COLOR_DARK_GREY       
        STA $D020	      ; Set screen border color 
        ;STA $D021	      ; Set screen color 

        ; Turn off (reset) any horizontal scroll        
        LDA address_horizontal_scroll_register   
        AND #%11111000                          ; h-scroll is the lower 3 bits, so mask them off to zero's
        CLC                             
        ADC #$07                                ; add xscroll value (value always fits inside the lower 3 bits)
        STA address_horizontal_scroll_register    
        
        ; Actual logic (raster bar)
        LDA #COLOR_DARK_GREY                         
        JSR delay_rasterline
        LDA #COLOR_GREY                         
        JSR delay_rasterline
        LDA #COLOR_DARK_GREY                         
        JSR delay_rasterline
        LDA #COLOR_GREY                         
        JSR delay_rasterline
        LDA #COLOR_LIGHT_GREY                         
        JSR delay_rasterline
        LDA #COLOR_GREY                         
        JSR delay_rasterline
        LDA #COLOR_LIGHT_GREY                         
        JSR delay_rasterline
        LDA #COLOR_WHITE                         
        JSR delay_rasterline
        LDA #COLOR_WHITE                         
        JSR delay_rasterline
        LDA #COLOR_LIGHT_GREY                         
        JSR delay_rasterline
        LDA #COLOR_GREY                         
        JSR delay_rasterline
        LDA #COLOR_DARK_GREY                         
        JSR delay_rasterline
        LDA #COLOR_BLACK                         
        JSR delay_rasterline

        
        ; Set next raster interrupt (raster interrupt irq2)
        lda #<irq2
        sta $0314
        lda #>irq2
        sta $0315
        
                
        lda #160 ; Create raster interrupt at line 160
        sta $d012

        ASL $D019	      ; "Acknowledge" (ASL do both read and write to memory location) the interrupt by clearing the VIC's interrupt flag.
        ;JMP $EA31	      ; Jump into KERNAL's standard interrupt service routine to handle keyboard scan, cursor display etc.
        JMP SYSTEM_IRQ_HANDLER

irq2	
        ; DEBUG: set border color for now        
        LDA #COLOR_BLACK       
        STA $D020	      ; Set screen border color 
        ;STA $D021	      ; Set screen color 

        jsr rasterline_start

        jsr scroll_text
        
        jsr text_color_scroll
        
        jsr address_sid_music_play

        jsr rasterline_end

        ; Set next raster interrupt (back to raster interrupt irq1)
        lda #<irq1
        sta $0314
        lda #>irq1
        sta $0315

        LDA #49         ; Interrupt on this raster line
        STA $D012       ; Set the raster line number where interrupt should occur

        ASL $D019	      ; "Acknowledge" (ASL do both read and write to memory location) the interrupt by clearing the VIC's interrupt flag.
        ;JMP $EA31	      ; Jump into KERNAL's standard interrupt service routine to handle keyboard scan, cursor display etc.
        JMP SYSTEM_IRQ_HANDLER

; ---------------------------------------------        
; Paint a raster line                            
; ---------------------------------------------        
delay_rasterline
    STA address_border_color  ; Set border background color to what's inside ACK
    STA address_screen_color  ; Set screen background color to what's inside ACK
    
    LDX #8
.delay_raster_line_loop
        NOP
        DEX
        CPX #$00
        BNE .delay_raster_line_loop
        RTS
        
; ---------------------------------------------        
; Paint a raster line                            
; ---------------------------------------------        
rasterline_start

        ; rasterline start paint        
        LDA #constant_color_yellow       
        ;STA $D020	      ; Set screen border color to yellow
        STA $D021	      ; Set screen color to yellow
        
;        LDX #10
;.rasterline_pause_loop	
;        DEX
;        BNE .rasterline_pause_loop      ; Empty loop that "does nothing" for a little under a half millisecond
        
        RTS
         
rasterline_end
        ; rasterline stop paint        
        LDA #constant_color_black
        STA $D020	      ; Set screen border color to black
        STA $D021	      ; Set screen color to black
        
        RTS
        
        
; ---------------------------------------------        
; Clear screen subroutine                            
; ---------------------------------------------        
clear_screen                                    
        LDA #$20 ; "Space" character code
        ;LDA #$41 ; DEBUG remove: A character code
        LDX #0
.clear_screen_loop   
        sta $0400,x
        sta $0500,x
        sta $0600,x
        sta $0700,x
        dex
        bne .clear_screen_loop
        
        rts


; ---------------------------------------------        
; Print static text message                            
; ---------------------------------------------        
print_static_message        
        ldx #$00
        
.print_static_message_loop 
        lda static_message_text, x
        CMP #$00  ; Have we encountered text message null termination
        BEQ .print_static_message_loop_end 
        
        sta $0400 + constant_columns_per_line * constant_static_text_line_index, x ; print static message text data to screen character location 
        ADC #$40 - 1
        sta $0400 + constant_columns_per_line * constant_static_text_line_index + constant_columns_per_line, x ; print static message text data to screen character location 
        
        LDA #COLOR_BLACK
        sta $D800 + constant_columns_per_line * constant_static_text_line_index, x ; print static message text data to screen character location 
        sta $D800 + constant_columns_per_line * constant_static_text_line_index + constant_columns_per_line, x ; print static message text data to screen character location 
 
        inx
        jmp .print_static_message_loop
        
.print_static_message_loop_end        
        rts


; ---------------------------------------------        
; Scroll screen text content a pixel                            
; ---------------------------------------------        
scroll_text

.scrolldelaycheck
        LDX address_horizontal_scroll_delay_counter   
        DEX
        BPL .nopixelscroll

        LDX #constant_horizontal_scroll_delay
        STX address_horizontal_scroll_delay_counter
        JMP .pixelscroll

.nopixelscroll
        STX address_horizontal_scroll_delay_counter
        JMP .endscroll
               
.pixelscroll        
        LDA address_horizontal_scroll_register   
        AND #%11111000                          ; h-scroll is the lower 3 bits, so mask them off to zero's
        CLC                             
        ADC address_horizontal_scroll_value     ; add xscroll value (value always fits inside the lower 3 bits)
        STA address_horizontal_scroll_register    
        
        LDY address_horizontal_scroll_value
        CPY #7
        BNE .no_charpos_scroll
        
        JSR scroll_message_one_char ; We are on our right most pixel scroll position, add a message character to the right most position  
        
        JSR add_next_rightmost_scroll_message_char ; Add a new character at the very right of the line (to scroll in to the left)
        
.no_charpos_scroll
        DEY 
        BMI .pixelscrollreset
        STY address_horizontal_scroll_value
        JMP .endscroll
        
.pixelscrollreset        
        LDY #7 ; Move horizontal scroll of screen 7 pixels to the right         
        STY address_horizontal_scroll_value
        
        JMP .endscroll
        
.endscroll        
        RTS   


; ---------------------------------------------        
; Scroll text color of text message line 1 character position                           
; ---------------------------------------------        
text_color_scroll
.text_color_scroll_delaycheck
        LDX address_text_color_scroll_delay_counter   
        DEX
        BPL .no_text_color_scroll

        LDX #constant_text_color_scroll_delay
        STX address_text_color_scroll_delay_counter
        JMP .text_color_scroll

.no_text_color_scroll
        STX address_text_color_scroll_delay_counter
        JMP .end_text_color_scroll
               
.text_color_scroll        
        
        LDY #$FF
.text_color_scroll_loop        
        INY
        LDA address_text_color_scroll_line_top + 1, Y
        STA address_text_color_scroll_line_top, Y
        LDA address_text_color_scroll_line_bottom + 1, Y
        STA address_text_color_scroll_line_bottom, Y
        CPY #constant_columns_per_line - 2
        BNE .text_color_scroll_loop   

.text_color_scroll_new_rightmost_color
        LDX address_text_color_scroll_offset
        INX
        
        LDA scroll_text_colors, X
        CMP #$FF
        BNE .skip_text_color_scroll_offset_reset
        
        LDX #$00
        
.skip_text_color_scroll_offset_reset        
        LDA scroll_text_colors, X
        STA address_text_color_scroll_line_top + constant_columns_per_line - 1
        LDA scroll_text_colors, X
        STA address_text_color_scroll_line_bottom + constant_columns_per_line - 1

        STX address_text_color_scroll_offset
        
.end_text_color_scroll        
        RTS

; ---------------------------------------------        
; Scroll message line 1 character position                           
; ---------------------------------------------        
add_next_rightmost_scroll_message_char
        LDX address_scroll_message_character_offset
        INX
        
        LDA scroll_text_top_line, x     ; Peek at next character to put on screen (top line) 

        CMP #$00 ; Scroll message text is null terminated
        BNE .skip_scroll_message_character_offset_reset
        
        LDX #$00 ; Reset scroll message character offset

.skip_scroll_message_character_offset_reset       
        LDA scroll_text_top_line, x 
        STA address_scroll_line_top + constant_columns_per_line - 1
        
        ADC #$40 - 1 ; Offset in font between upper part and lower part of large letters
        STA address_scroll_line_bottom + constant_columns_per_line - 1
        
        STX address_scroll_message_character_offset ; Save scroll message offset register x 
        
        RTS

; ---------------------------------------------        
; Scroll message text line 1 character position to the left                           
; ---------------------------------------------        
scroll_message_one_char
        LDX #$FF
.scroll_line_char_loop        
        INX
        LDA address_scroll_line_top + 1, x
        STA address_scroll_line_top, x
        LDA address_scroll_line_bottom + 1, x
        STA address_scroll_line_bottom, x
        CPX #constant_columns_per_line - 2
        BNE .scroll_line_char_loop   
  
        RTS
        
        
scroll_text_colors        
    !byte COLOR_DARK_GREY,  COLOR_DARK_GREY 
    !byte COLOR_DARK_GREY,  COLOR_DARK_GREY 
    !byte COLOR_GREY,       COLOR_GREY
    !byte COLOR_DARK_GREY,  COLOR_DARK_GREY 
    !byte COLOR_DARK_GREY,  COLOR_DARK_GREY 
    !byte COLOR_GREY,       COLOR_GREY
    !byte COLOR_DARK_GREY,  COLOR_DARK_GREY 
    !byte COLOR_DARK_GREY,  COLOR_DARK_GREY 
    !byte COLOR_DARK_GREY,  COLOR_DARK_GREY 
    !byte COLOR_DARK_GREY,  COLOR_DARK_GREY 
    !byte COLOR_DARK_GREY,  COLOR_DARK_GREY 
    !byte COLOR_DARK_GREY,  COLOR_DARK_GREY 
    !byte COLOR_GREY,       COLOR_GREY
    !byte COLOR_LIGHT_GREY, COLOR_LIGHT_GREY 
    !byte COLOR_WHITE,      COLOR_WHITE         
    ;!byte COLOR_YELLOW,     COLOR_YELLOW         
    !byte COLOR_WHITE,      COLOR_WHITE         
    !byte COLOR_LIGHT_GREY, COLOR_LIGHT_GREY 
    !byte COLOR_GREY,       COLOR_GREY         
    !byte $FF ; Color list termination value

    
; ---------------------------------------------        
; Static message text (null terminated)                            
; ---------------------------------------------        
static_message_text    
;   !scr "1234567890123456789012345678901234567890"                       
    !scr "        rollin' down the river!         "                       
    !byte $00 ; Static message text is null terminated
    
    
; ---------------------------------------------        
; Scroll message text (null terminated)                            
; ---------------------------------------------        
scroll_text_top_line
    !scr "rollin' down the river! "                       
    !scr "1234567890 " 
    !byte $00 ; Scroll message text is null terminated

    
; ---------------------------------------------        
; SID Music                            
; ---------------------------------------------        
* = address_sid_music_init    
        ; Player size$0DE0 (3552 bytes)
        ; Load address$1000 (4096)
        ; Init address$1000 (4096)
        ; Play address$1003 (4099)
        ; Default subtune1 / 1
    !bin "Great_Giana_Sisters.sid",,$7C + 2


; ---------------------------------------------        
; Text font                            
; ---------------------------------------------        
; More fonts at http://kofler.dot.at/c64/
; Load font to last 2k block of bank 3    
* = address_font    
    ;!bin "giana_sisters.font.64c",,2
    ;!bin "devils_collection_25_y.64c",,2
    !bin "double_char_font.bin"
