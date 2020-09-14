;== Include MemoryMap, HeaderInfo, and interrupt Vector table ==
.INCLUDE "header.inc"

;== Include Library Routines ==
.INCLUDE "InitSNES.asm"
.INCLUDE "LoadGraphics.asm"

;WRAM Usage:
;	$0000 - Character X
;	$0001 - Character Y
;	$0002 - Tilemap X
;	$0003 - Character X Pos. Within Tile
;	$0004 - Tilemap Y
;	$0005 - Character Y Pos. Within Tile
;	$0006 - Water X Scroll
;	$0007 - Water Y Scroll
;	$0008 - Scroll Timer
;   $0009 and $000A - TEMP
;   $000B and $000C - Collided tile
;   $000D - Thing

;==========================================
; Main Code
;==========================================

.BANK 0 SLOT 0
.ORG 0
.SECTION "MainCode"

Main:
    InitializeSNES							;Initialize the SNES

    rep #$10								;Set X/Y to 16-bit
    sep #$20								;Set A to 8-bit

	;jsl UploadAndExecuteSoundEngine

    LoadPalette BGPalette, 0, 12			;Load the background palette

	LoadPalette BGPalette, 8*16, 12			;Load the sprite palette

    LoadBlockToVRAM TileData, $0000, TileDataEnd-TileData	;Load the background tiles
	LoadBlockToVRAM LevelData, $0800, $0800					;Load the map to VRAM

	jsl PlaceHud							;Place the text on BG3
	jsl PlaceWater

	lda #$20
	sta $2126
	sta $2127
	lda #$AA

    jsr SetupVideo							;Setup video

	lda #$00
	sta $4016

	lda #$81
	sta $4200								;Enable NMI and Joypad Auto-Read

	lda #%11000000	;have the automatic read of the SNES read the first pair of JoyPads
	sta $4201		;IO Port Write Register

	lda #$0F
    sta $2100      							; Turn on screen, full Brightness

	jsr MosaicEffect						; Do the mosaic effect

	lda #20
	sta.b $0000
	lda #40
	sta.b $0001
	
Infinity:
	sep #$20

	;jsl InitHDMA

@LoopUntilJoyReadDone: ;Loop until the auto joypad-read is complete
	lda $4212
	and #$01
	bne @LoopUntilJoyReadDone
	
	lda $4219
	and #$08
	bne @SkipUp
	lda $0001
	inc a
	sta $0001
	lda $0003
	dec a
	sta $0003
@SkipUp:

	lda $4219
	and #$04
	bne @SkipDown
	lda $0001
	dec a
	sta $0001
	lda $0003
	inc a
	sta $0003
@SkipDown:

	lda $4219
	and #$02
	bne @SkipLeft
	lda $0000
	inc a
	sta $0000
	lda $0002
	dec a
	sta $0002
@SkipLeft:

	lda $4219
	and #$01
	bne @SkipRight
	lda $0000
	dec a
	sta $0000
	lda $0002
	inc a
	sta $0002
@SkipRight:

	jsl CollideCharacter

	lda $0008
	cmp #$04
	bne @SkipScroll
	lda $0006
	inc a
	sta $0006
	lda $0007
	inc a
	sta $0007
	lda #$00
	sta $0008
	jmp @ScrollDone
@SkipScroll:
	lda $0008
	inc a
	sta $0008
@ScrollDone:

	rep #$20

	wai				;Wait for interrupt
    jmp Infinity	;Jump to Infinity

;============================================================================
VBlank:
    rep #$30        ; A/mem=16 bits, X/Y=16 bits (to push all 16 bits)
    phb
	pha
	phx
	phy
	phd
	
	sep #$20		;Set A to 8-bit
	rep #$10		;Set X/Y to 16-bit

    lda $4210       ; Clear NMI flag
	
	;Neo Jr. Sprite Code Start
	ldx #$0000
	stx $2102

	;Write the X position
	lda $0000
	clc
	sbc #$04
	sta $2104

	;Write the Y position
	lda $0001
	clc
	sbc #$10
	sta $2104

	;Write the character
	lda #$31
	sta $2104

	;Write the attributes
	lda #$30
	sta $2104

	;Write the X position
	lda $0002
	asl a
	asl a
	asl a
	sta $2104

	;Write the Y position
	lda $0004
	asl a
	asl a
	asl a
	sta $2104

	;Write the character
	lda #$31
	sta $2104

	;Write the attributes
	lda #$30
	sta $2104

	ldx #$0100
	stx $2102

	lda #$FF
	sta $2104
	;Neo Jr. Sprite Code End

	ldx #$0000

	lda $0006
	sta $210F
	stx $210F
	
	lda $0007
	sta $2110
	stx $2110

	rep #$20		;Set A to 16-bit

    pld 
	ply 
	plx 
	pla 
	plb 

    sep #$20
    rti
;============================================================================

MosaicEffect:
	lda #$FF
	sta $2106
Loop:
	sbc #$10
	sta $2106
	wai
	wai
	wai
	wai
	wai
	wai
	wai
	wai
	cmp #$0F
	bne Loop
	rts

PlaceHud:
	rep #$10
	ldx #$1000
	ldy #$0000
@PlaceHudLoop:
	stx $2116
	lda Text, y
	cmp #$FF
	beq @DonePlacingHud
	sta $2118
	lda #$20
	sta $2119
	inx
	iny
	jmp @PlaceHudLoop
@DonePlacingHud:
	rtl

Text: .db "                                "
	  .db " USE THE D-PAD TO MOVE THE      "
	  .db "  CHARACTER                     "
	  .db "                                "
	  .db " TIME TO MAKE MAJOR CHANGES!!!  "
	  .db "                                "
	  .db "                                "
	  .db "                                "
	  .db "                                "
	  .db "                                "
	  .db "                                "
	  .db "                                "
	  .db "                                "
	  .db "                                "
	  .db "                                "
	  .db "                                "
	  .db "                                "
	  .db "                                "
	  .db "                                "
	  .db "                                "
	  .db "                                "
	  .db "                                "
	  .db "                                "
	  .db "                                "
	  .db "                                "
	  .db "                                "
	  .db "     PROGRAMMED BY ATM959       "
	  .db $FF

PlaceWater:
	ldx #$0C00
	ldy #$0000
@PlaceWaterLoop:
	stx $2116
	lda #$0F
	sta $2118
	iny
	tya
	cmp (32*32)
	beq @DonePlacingWater
	inx
	jmp @PlaceWaterLoop
@DonePlacingWater:
	rtl

CollideCharacter:
	
	;Get the tile X
	lda $0000
	lsr a
	lsr a
	lsr a
	sta $0002

	;Get the X position within the tile
	lda $0000
	and #$07
	sta $0003

	;Get the tile Y
	lda $0001
	lsr a
	lsr a
	lsr a
	sta $0004

	;Get the Y position within the tile
	lda $0001
	and #$07
	sta $0005

	rep #$30 ;Set A to 16-bit
	;Get the tile the character is on by doing (ty * 32) + tx
	lda $0004 ;Load the tile Y into A
	sta $4202 ;Store A into the multiplicand register
	lda #32   ;Load 32 into A
	sta $4203 ;Store A into the multiplier register
	nop;\
	nop; \ Wait 8
	nop; / cycles
	nop;/
	lda $2134 ;Load 16 bits of the multiplication result into A
	sta $0009 ;Store A into $0009 and $000A
	lda $0002 ;Load the tile X into A
	clc ;Clear the carry flag
	adc $0009 ;Add $0009 and $000A to A
	sta $4202 ;Store A into the multiplicand register
	lda #2 ;Load 2 into A
	sta $4203 ;Store A into the multiplier register
	nop;\
	nop; \ Wait 8
	nop; / cycles
	nop;/
	ldy $2134 ;Load the result into X
	sep #$20 ;Set A to 8-bit

	;ldx LevelData, y ;Load LevelData + x into A
	stx $000B

	sep #$10

	rtl

;============================================================================
; SetupVideo -- Sets up the video mode and tile-related registers
;----------------------------------------------------------------------------
; In: None
;----------------------------------------------------------------------------
; Out: None
;----------------------------------------------------------------------------
SetupVideo:

    lda #$09
    sta $2105      ; Set Video mode 1 and BG3 priority bit

    lda #$08       
    sta $2107	   ; Set BG1's Tile Map offset to $0800 (Word address)
	lda #$0C
	sta $2108	   ; Set BG2's Tile Map offset to $0C00
	lda #$10
	sta $2109	   ; Set BG3's Tile Map offset to $1000

    lda #$15       ; Enable BG1, BG2, BG3, and OBJ
    sta $212C

	lda #$02
	sta $212D

	lda #$02
	sta $2130

	lda #%01100001
    sta $2131       ; Color addition on BG1 and Backdrop color

    rts

;UploadAndExecuteSoundEngine:
;	sei			;Disable interrupts
;
;@WaitReady1:
;	lda $2140
;	cmp #$AA
;	bne @WaitReady1
;@WaitReady2:
;	lda $2141
;	cmp #$BB
;	bne @WaitReady2	;"Is the SPC700 ready?"
;
;	lda #$01
;	sta $2141
;	lda #$00
;	sta $2142
;	lda #$05
;	sta $2143
;	lda #$CC
;	sta $2140		;Send the transfer begin bytes
;
;@WaitEcho1:
;	lda $2140
;	cmp #$CC
;	bne @WaitEcho1	;Wait for PORT0 to echo
;
;	ldx SoundEngineEnd-SoundEngine
;	ldy #$0000
;@TransferFirst:
;	lda SoundEngine, y
;	sta $2141		;Write the first byte to PORT1
;
;	lda #$00
;	sta $2140		;Write 0 to PORT0
;
;@WaitEcho2:
;	lda $2140
;	cmp #$00
;	bne @WaitEcho2	;Wait for PORT0 to echo
;
;	dex
;	iny
;
;@TransferRest:
;	lda SoundEngine, y
;	sta $2141		;Write the next byte to PORT1
;
;	lda $2140
;	inc a
;	sta $2140		;Increase the byte in PORT0 and send it back
;	sta $0000
;
;@WaitEcho3:
;	lda $2140
;	cmp $0000
;	bne @WaitEcho3
;
;	dex
;	iny
;
;	txa
;	cmp #$00
;	bne @TransferRest
;
;	lda #$00
;	sta $2141	;0 in PORT1
;	lda #$00
;	sta $2142
;	lda #$05
;	sta $2143	;Execute address in PORT2 and PORT3
;	lda $2140
;	clc
;	adc #$02
;	sta $2140	;Increase PORT0 by 2 and store it in PORT0
;
;	stz $0000
;	lda #$00
;	ldx #$0000
;	ldy #$0000
;
;	cli			;Enable interrupts
;	rtl

.ENDS
;============================================================================

.BANK 1 SLOT 0
.ORG 0
.SECTION "DataStuff"
TileData:
	.incbin "tiles.chr"
TileDataEnd:

BGPalette:
	;    gggrrrrr   ?bbbbbgg
	.db %00000000, %00000000
	.db %00010000, %01000010
	.db %00001000, %00100001
	.db %11111111, %01111111

	.db %00000000, %00000000
	.db %11111111, %01111111
	.db %00000000, %00000000
	.db %00011111, %00000000

	.db %00000000, %00000000
	.db %11010110, %01111110
	.db %00010000, %01111110
	.db %00001000, %01111101
.ENDS

.SECTION "MapData"
LevelData: .incbin "level1.bin"
.ENDS

SoundEngine: .incbin "soundEngine.bin"
SoundEngineEnd: