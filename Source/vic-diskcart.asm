; VIC 20 Disk utility Cartridge - Version 1 (r7a)
; Leif Bloomquist - August 3, 2005

; Thanks to everyone on the Denial forums
; http://www.sleepingelephant.com/denial/

  processor 6502 ;VIC20
  
  ; Note that the cart can also be placed at $6000 (Block 3). 
  ; So whenever a defined address is given, the Block 3 address is also shown 
  ; afterwards as a comment.

  org $a000,0 ;$6000   (Fill value=0)

; ==============================================================
; Startup
; ==============================================================
  dc.w START   ; Entry point for power up
  dc.w RESTORE ; Entry point for warm start (RESTORE)
  
  dc.b "A0",$C3,$C2,$CD	; 'A0CBM' boot string
	
START  
  ;Kernel Init
  jsr $fd8d ; RAMTAS - Initialise System Constants
  jsr $fd52 ; Init Vectors
  jsr $fdf9 ; Init I/O
  jsr $e518 ; Init I/O 
  
  ;BASIC Init (Partial)
  jsr $e45b ; Init Vectors  
  jsr $e3a4 ; BASIC RAM
  jsr $e404 ; INIT Message (needed so keycheck routine below works)
  
  ; Force startup with device #8 (disk)
  lda #$08
  sta $BA

CHECK
; Check which control keys are pressed.
  lda $028d

; if C=, go straight to BASIC.
  cmp #$02
  bne SHIFT
  jmp BASIC
  
; if SHIFT, go straight to BASIC with wedge enabled.
SHIFT
  cmp #$01
  beq STARTWEDGE
  
; ==============================================================
; Default Startup - show init screen
; ==============================================================
SSCREEN
  lda #$6e
  sta $900f ; Blue Screen
  
  ldx #$00
MORESSCREEN
  lda STARTUPSCREEN,x
  beq SETUPTIMEOUT
  jsr $ffd2
  inx
  jmp MORESSCREEN

SETUPTIMEOUT
  ;Initialize Timer
  ldy $a1 ; part of TI clock, updated every 256/60 = 4.2 seconds 
  iny
  iny
  iny     ;Jump ahead three 'ticks' = 12.6 seconds

; Check for timeout
TIMEOUT
  cpy $a1
  bne KEYS
  jmp STARTWEDGE

; Check keys
KEYS
  jsr $ffe4
  cmp #$00
  beq TIMEOUT
  
DENIAL
  cmp #$44   ;D
  bne F1
  jmp CREDITS
  
F1 
  cmp #$85  ;F1
  bne DEL
  jmp HELP

DEL
  cmp #$14  ;DEL
  bne F3
  jmp STARTWEDGE
  
F3
  cmp #$86  ;F3
  bne F4
  jmp XMENU

F4
  cmp #$8a  ;F4
  bne F5
  jmp SCRUBBER

F5
  cmp #$87  ;F5
  bne PLUS
  lda #$93
  jsr $ffd2
  jmp UNNEW
  
PLUS
  cmp #$2b  ;+
  bne MINUS 
  jmp INCDRIVE
  
MINUS
  cmp #$2D  ;-
  bne F7
  jmp DECDRIVE
  
F7
  cmp #$88  ;F7
  bne F8
  jmp STARTWEDGE

F8
  cmp #$8C
  bne TIMEOUT
  jmp BASIC
  

; ==============================================================
; Start Wedge and return to BASIC
; ==============================================================
STARTWEDGE 
  lda #$06
  sta $0286 ; Restore blue text 
  jsr $e404 ; INIT Message
  jsr TEXT
  jsr WEDGE
  jmp BASIC2
  
; ==============================================================
; Return to BASIC
; ==============================================================
BASIC
  lda #$06
  sta $0286 ; Restore blue text 
  jsr $e404 ; INIT Message
  
BASIC2
  ;Clear keyboard buffer
  lda #$00
  sta $c6  
  
  ; Restore normal startup colors
  lda #$1b
  sta $900f 
  
  ; Complete BASIC Warm Start (since init is done above)
  jmp $e467 


; ==============================================================
; Handle the RESTORE key  
; ==============================================================
RESTORE
  jmp $fec7   ; Continue as if no cartridge installed

;Don't use $fea9!
;$fea9 goes to ($0318) which goes to $fead which ends up with a jmp ($a002)...


; ==============================================================
; Show Wedge start-up string
; ==============================================================
TEXT 
  ldx #$00
MORETEXT
  lda WEDGEMESSAGE,x
  beq TEXTDONE
  jsr $ffd2
  inx
  jmp MORETEXT
TEXTDONE
  rts
  
; ==============================================================
; Initialize wedge
; ==============================================================
WEDGE
  ldx #$AB
WEDGE2
  lda #$4C
  ldy #$00  
  sta $73
  sty $74
  stx $75
  rts

; ==============================================================
; Copy XMENU to BASIC area and start it.  By Anders Carlsson.
; ==============================================================
XMENU
   lda #$1e
   sta $900f ; Screen colors
   
   LDA #$A2  ; src high byte  ($A200)
COPYXMENU
   STA $FC
   LDA $2C  ; dest high byte = Start of BASIC no matter which memory map
   STA $FE
   LDA #$00 ; src/dest low byte
   STA $FB
   STA $FD
   TAY
   LDX #$04 ; number of zero bytes for end of program - 4 for XMENU (was 3)
A: LDA ($FB),Y
   STA ($FD),Y
   BNE B
   DEX  ; if zero byte, decrease counter
   BEQ C
   BNE D
B: LDX #$04 ; non-zero data, reset counter
D: INY
   BNE A
   INC $FC
   INC $FE
   
   ;JMP A 
   ;the above isn't relocatable, so cheat and use BCC (branch on carry clear)
   clc
   bcc A
   
   ; no boundary checks, rely on three zero bytes (LB -> four)
   ; This was originally a single 'INY,' had to change it for XMENU
C: dey
   dey
   dey

   STY $2D
   STY $2F
   STY $31
   LDA $FE ; dest high byte
   STA $2E
   STA $30
   STA $32

; ------------------------
RUNBASIC
   
   JSR $C659  ;CLR
   jsr $c533  ;Relinks BASIC Program from and to any address... 
   JMP $C7AE
   
; ==============================================================
; List all the Denial Helpers
; ==============================================================
CREDITS
  lda #$1e
  sta $900f ; Screen colors
  ldx #$00
MORECREDITS
  lda THECREDITS,x
  beq WAITSPACE
  jsr $ffd2
  inx
  jmp MORECREDITS

; ==============================================================
; Wait here for space bar
; ==============================================================

WAITSPACE
  jsr $ffe4
  cmp #$20 ;SPACE
  bne WAITSPACE
  jmp SSCREEN

; ==============================================================
; Help Screen
; ==============================================================
HELP
  ldx #$00
MOREHELP
  lda HELPSCREEN,x
  beq WAITSPACE
  jsr $ffd2
  inx
  jmp MOREHELP


; ==============================================================
; Launch Scrubber
; ==============================================================
SCRUBBER
   lda #$A8    ; High byte of where it will be
  
COPYSCRUBBER
    STA $FC
    LDA $2C  ; dest high byte = Start of BASIC no matter which memory map
    STA $FE
    LDA #$00 ; src/dest low byte
    STA $FB
    STA $FD
    TAY
    LDX #$04 ; number of zero bytes for end of program (was 3)
A1: LDA ($FB),Y
    STA ($FD),Y
    BNE B1
    DEX  ; if zero byte, decrease counter
    BEQ C1
    BNE D1
B1: LDX #$04 ; non-zero data, reset counter
D1: INY
    BNE A1
    INC $FC
    INC $FE
   
    ;the above isn't relocatable, so cheat and use BCC (branch on carry clear)
    clc
    bcc A1
   
   ; no boundary checks, rely on three zero bytes (LB -> four)
C1: INY
    STY $2D
    STY $2F
    STY $31
    LDA $FE ; dest high byte
    STA $2E
    STA $30
    STA $32

    JSR $C659  ;CLR
    jsr $c533  ;Relinks BASIC Program from and to any address...
    
    lda #$1e
    sta $900f ; Screen colors  
    JMP $C7AE


; ==============================================================
; Special entry points and code for $6000 (Block 3) use.
; ==============================================================

; ============================
; Display Help - Block 3
; ============================  
  org $a1a8  ;$61a8 - equals 25000 decimal, easy to remember!
  
SHOWHELP3
  ldx #$00
MOREHELP3
  lda HELPSCREEN3-$4000,x ; Trick, jump to where it *should* be
  beq HELPDONE3
  jsr $ffd2
  inx
  jmp MOREHELP3-$4000
HELPDONE3
  rts
  
; ============================
; Run XMENU - Block 3
; ============================
  org $a1bc  ;$61bc - equals 25020 decimal, easy to remember!
  jmp RUNXMENU3-$4000
  
  
; ============================
; Show Credits - Block 3 (Entry)
; ============================
  org $a1c1  ;$61c1 - equals 25025 decimal, easy to remember!
  jmp CREDITS3-$4000
  
  
; ============================
; Run Scrubber - Block 3
; ============================
  org $a1c6  ;$61c6 - equals 25030 decimal, easy to remember!
  
  lda #$68   ;High byte of where it will be in Block 3
  jmp COPYSCRUBBER-$4000
  
; ============================
; Display credits - Block 3
; ============================  
CREDITS3
  ldx #$00
MORECREDITS3
  lda THECREDITS-$4000,x
  beq CREDITSDONE3
  jsr $ffd2
  inx
  jmp MORECREDITS3-$4000
CREDITSDONE3
  rts

; ============================
; Initialize Wedge - Block 3
; ============================ 
  org $a1da  ;$61da - equals 25050 decimal

; Show Wedge start-up string
TEXT3
  ldx #$00
MORETEXT3
  lda WEDGEMESSAGE-$4000,x
  beq TEXTDONE3
  jsr $ffd2
  inx
  jmp MORETEXT3-$4000
TEXTDONE3

; Default to drive #8
  lda #$08
  sta $BA

; Initialize wedge
WEDGE3
  LDX #$6B ; <-Note
  jmp WEDGE2-$4000
  rts

; ============================
; UNNEW - Block 3
; ============================  
  org $a1f8 ;$61f8 - equals 25080 decimal
    
  jmp UNNEW-$4000   ; Returns to BASIC directly

; ==============================================================
; Include the XMenu16 binary at $A200
; Note that first two bytes must be removed+replaced with a 0 byte!
; ==============================================================

  org $a200
  incbin "x-menu16.bin"
  
; ==============================================================
; Include the Scrubber binary at $A800
; Note that first two bytes must be removed+replaced with a 0 byte!
; ==============================================================

  org $a800
  incbin "scrubber.bin"


; ==============================================================
; Un-New Routine. By Daniel Kahlin.
; ==============================================================

  org $aa00

UNNEW
   LDY #$01
   TYA
   STA ($2B),Y   ; store a non zero in the MSB of the first link addr
   JSR $C533     ; relinker
   LDA $22       ; set end of basic/start of variables to the address of the last '0' found + 2
   CLC
   ADC #$02
   STA $2D
   LDA $23
   ADC #$00
   STA $2E
   jsr $C659    ; CLR
   
  ;White Screen with green border
  lda #$1d
  sta $900f 
  
  ; Restore blue text 
  lda #$06
  sta $0286 
  
  ; Complete BASIC Warm Start (since init is done above)
  jmp $e467 

; ==============================================================
; Increment/Decrement the drive#
; ==============================================================

INCDRIVE
  inc $BA
  jmp CHECKDRIVE
  
DECDRIVE
  dec $BA

; Make sure drive is valid - 8 to 15
CHECKDRIVE
  clc
CHECKLOW
  lda $BA
  sbc #$08
  bcs CHECKHIGH  
  lda #$08
  sta $BA
CHECKHIGH
  clc
  lda $BA
  sbc #$0F
  bcc SHOWDRIVE
  lda #$0F
  sta $BA

SHOWDRIVE
  lda #$01
  sta $0286  ;White text

  ;Move Cursor
  clc
  ldx #$12
  ldy #$0b
  jsr $e50a

  lda $BA
  cmp #$08
  beq DIGIT
  cmp #$09
  beq DIGIT
  jmp DIGITS
  
DIGIT
  adc #$2F  ;convert to ascii
  jsr $ffd2
  lda #$20
  jsr $ffd2 ;append a space  
  jmp SETUPTIMEOUT

DIGITS
  lda #$31  ;'1'
  jsr $ffd2
  lda $BA
  adc #$26
  jsr $ffd2
  jmp SETUPTIMEOUT


; ==========================================================================
; Moved here because of space constraints.
; Check that drive# is at least 8, then run XMENU (Block 3)
; ==========================================================================

RUNXMENU3
  clc
  lda $BA
  sbc #$08
  bcs OK
  lda #$08
  sta $BA

OK
  lda #$62   ;High byte of where it will be in Block 3
  jmp COPYXMENU-$4000


; ==============================================================
; Include the Wedge binary at $AB00 - no load address in file
; Wedge2 has all LDA #$08's replaced with LDA $BA instead
; ==============================================================

  org $ab00
  incbin "wedge2.bin"
  

; ==============================================================
; Define some common PETSCII codes 
; http://sta.c64.org/cbm64petkey.html
; ==============================================================

CLRHOME = $93
RVSON   = $12
RVSOFF  = $92
CR      = $0D
BLACK   = $90
WHITE   = $05
RED     = $1C
BLUE    = $1F
PURPLE  = $9C
YELLOW  = $9E
AT      = $40

; ==============================================================
; Help screen
; ==============================================================

  org $ad00

HELPSCREEN
  dc.b WHITE
HELPSCREEN3
  dc.b CLRHOME, RVSON, "WEDGE COMMANDS", CR, CR  ;RVSOFF Not Needed
  dc.b AT, "$ DIRECTORY", CR
  dc.b AT, "  STATUS/COMMAND", CR
  dc.b    "/  LOAD", CR,CR
  dc.b    "POKE186,#  DRIVE#", CR, CR, CR
  dc.b RVSON, "BLOCK 3 COMMANDS", CR, CR  ;RVSOFF Not Needed
  dc.b "SYS25000 HELP", CR
  dc.b "SYS25020 XMENU*", CR
  dc.b "SYS25030 SCRUBBER*", CR
  dc.b "SYS25050 START WEDGE", CR
  dc.b "SYS25080 BASIC UN-NEW", CR, CR
  dc.b "*DESTROYS BASIC PRG!", CR
  dc.b $00

; ==============================================================
; Startup screen
; ==============================================================

  org $ae00

STARTUPSCREEN
  dc.b CLRHOME, WHITE, CR, CR, RVSON, "DISK UTILITY CARTRIDGE", CR, CR
  dc.b "F1. HELP", CR, CR
  dc.b "F3. X-MENU UTILITY", CR, CR
  dc.b "F4. HEAD SCRUBBER", CR, CR
  dc.b "F5. BASIC UN-NEW", CR, CR
  dc.b "F7. BASIC (WEDGE)", CR, CR
  dc.b "F8. BASIC (NORMAL)", CR, CR, CR
  dc.b "+/- DRIVE #8",CR
  dc.b BLUE, $00

; ==============================================================
; Wedge Message
; ==============================================================

WEDGEMESSAGE
  dc.b $11, $1C, "WEDGE ACTIVE",$11, $1F, 0

; ==============================================================
; The credits!
; ==============================================================

  org $af00 ;$6f00

THECREDITS
  dc.b CLRHOME, YELLOW, "V1 08/2005", CR, CR
  dc.b RED, "CREATED BY:", CR, CR
  dc.b BLACK
  dc.b "LEIF BLOOMQUIST", CR
  dc.b "ANDERS PERSSON", CR
  dc.b "ANDERS CARLSSON", CR
  dc.b "CHRISTOPHER PREST", CR
  dc.b "BRIAN LYONS", CR
  dc.b "LEE DAVIDSON", CR
  dc.b "SCHLOWSKI", CR
  dc.b "VIPERSAN", CR
  dc.b "DANIEL KAHLIN", CR
  dc.b "JEFF DANIELS", CR
  dc.b "MICHAEL KLEIN", CR
  dc.b "DAVID A. HOOK", CR, CR
  dc.b PURPLE
  dc.b "WWW.SLEEPINGELEPHANT", CR
  dc.b "        .COM/DENIAL/", CR
  dc.b BLUE  
  dc.b $00


;Pad to end to create valid cart image
  org $afff
  dc.b #$00