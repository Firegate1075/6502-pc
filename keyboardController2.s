PORTB	= $6000
PORTA	= $6001
DDRB	= $6002
DDRA	= $6003
PCR 	= $600c
IFR	= $600d
IER	= $600e

KEYBOARD_BUFFER 	= $0200
KEYBOARD_readptr 	= $0000
KEYBOARD_writeptr 	= $0001
KEYBOARD_flags		= $0002 ; Keyboard modifiers: %xxxxrcas, r:release, c:ctrl, a:alt, s:shift

LCD_readDataBuffer	= $0010


KEYBOARD_super		= %01000000
KEYBOARD_altgr		= %00100000
KEYBOARD_ext		= %00010000
KEYBOARD_release 	= %00001000
KEYBOARD_ctrl		= %00000100
KEYBOARD_alt		= %00000010
KEYBOARD_shift		= %00000001


E=%00001000
RW=%00000100
RS=%00000010


	.org $8000
reset:
	sei

	ldx #$ff
	txs				; initialize stack pointer to $ff

	lda #$00
	sta KEYBOARD_readptr
	sta KEYBOARD_writeptr
	sta KEYBOARD_flags
	sta LCD_readDataBuffer


; 65C22 VIA setup ------------------------------------------------------

	lda #$ff			; init PORT B as outputs and...
	sta DDRB

	lda #00
	sta DDRA			; ...PORT A as inputs

	lda #$01 			; INT on rising edge of pin A1
	sta PCR

	lda #$7f			; Enable PC-INT A1
	sta IER
	lda #$82
	sta IER

; -----------------------------------------------------------------------

; setup of 16x2 character HD44780 LCD on PORTB

	jsr lcd_init			; set 4-bit mode
	lda #$28			; 4-bit mode, 2-lines, 5x8 Font
	jsr lcd_sendInstruction
	lda #$0e			; Display on, cursor on, blink off 
	jsr lcd_sendInstruction
	lda #$06 			; Left-to-right, shift off
	jsr lcd_sendInstruction
	lda #$01			; clear display
	jsr lcd_sendInstruction

; Main Program --------------------------------------------------------------

	cli				; enable interrupts



	;jsr printString


loop:
	ldx KEYBOARD_readptr
	cpx KEYBOARD_writeptr
	beq loop

	lda KEYBOARD_BUFFER, x
	and #%01100000
	beq controlChar_handler

	lda KEYBOARD_BUFFER, x
	jsr lcd_sendChar
	inc KEYBOARD_readptr

	jmp loop

controlChar_handler:
	lda KEYBOARD_BUFFER, x
	
	inc KEYBOARD_readptr
	cmp #$0a
	beq enter
	cmp #$08
	beq backspace
	cmp #$1b
	beq esc

	jmp loop

enter:
	jsr lcd_readData
	and #$40
	bne enter_clear

	lda #$c0
	jsr lcd_sendInstruction
	jmp loop

enter_clear:
	lda #1
	jsr lcd_sendInstruction
	jmp loop



backspace:

	jsr lcd_readData

	ora #%10000000			; Turn cursor-position into set-cursor instruction
	pha
	and #$40
	bne backspace_line2
	pla

	cmp #$80			; check for left screen border
	beq backspace_done
	jmp backspace_removeChar

backspace_line2:
	pla
	cmp #$c0			; check for left screen border
	beq backspace_done

backspace_removeChar:
	dec				; move cursor to the left
	pha
	jsr lcd_sendInstruction		; set mode to right-to-left

	lda #" "
	jsr lcd_sendChar		; overwrite last character with a space

	pla
	jsr lcd_sendInstruction		; move cursor back to the left
backspace_done:

	jmp loop




esc:
	lda #$01
	jsr lcd_sendInstruction
	jmp loop








;LCD-Subroutines


lcd_printString:
	ldx #0

lcd_printString_printChar:
;	lda text, x
	lda #$00
	beq done

	jsr lcd_sendChar
	inx
	jmp lcd_printString_printChar

done:
	rts


lcd_init:
	lda #$20
	sta PORTB
	ora #E
	sta PORTB
	eor #E
	sta PORTB

	lda #$80 	; Add $8 for compatibility with 4-bit mode, instruction ignored in 8-bit mode
	sta PORTB
	ora #E
	sta PORTB
	eor #E
	sta PORTB

	rts



lcd_wait:
	pha

lcd_busy:
	jsr lcd_readData
	and #%10000000
	bne lcd_busy

	pla
	rts

lcd_readData:
	lda #%00001111	; Data pins as inputs, control signals as outputs
	sta DDRB

	lda #RW
	sta PORTB
	ora #E
	sta PORTB

	lda PORTB
	and #%11110000
	sta LCD_readDataBuffer

	lda #RW
	sta PORTB
	ora #E
	sta PORTB

	lda PORTB
	and #%11110000
	lsr
	lsr
	lsr
	lsr
	ora LCD_readDataBuffer
	pha

	lda #RW
	sta PORTB
	lda #$ff
	sta DDRB

	pla

	rts


lcd_sendInstruction:
	jsr lcd_wait
	pha

	and #%11110000

	sta PORTB
	ora #E
	sta PORTB
	eor #E
	sta PORTB

	pla

	asl
	asl
	asl
	asl

	sta PORTB
	ora #E
	sta PORTB
	eor #E
	sta PORTB


	rts

lcd_sendChar:
	jsr lcd_wait
	pha

	and #%11110000
	ora #RS
	sta PORTB
	ora #E
	sta PORTB
	eor #E
	sta PORTB

	pla

	asl
	asl
	asl
	asl
	ora #RS
	sta PORTB
	ora #E
	sta PORTB
	eor #E
	sta PORTB

	rts


; Keyboard Handler

irq:
	pha
	txa
	pha


	ldx PORTA 		; read scan code and reset IRQ

	lda KEYBOARD_flags
	and #KEYBOARD_release	; check if scan code comes from a key release
	bne irq_releasekey_jump

	lda KEYBOARD_flags
	cpx #$f0		; if the scan code is $f0 (release) set the release flag
	beq irq_setRelease


	lda KEYBOARD_flags	; check if scan code comes from an ext-key
	and #KEYBOARD_ext
	bne irq_extendedkey_jump

	lda KEYBOARD_flags
	cpx #$e0
	beq irq_setExtended	; if the scan code is the extend-keycode, set the extended flag


	lda KEYBOARD_flags	; Preload the A-reg with the Keyboard-flags

	cpx #$12		; if the pressed key is shift, set the shift flag
	beq irq_setShift
	cpx #$59
	beq irq_setShift

	cpx #$14		; if the pressed key is ctrl, set the ctrl flag
	beq irq_setCtrl

	cpx #$e1		; if the scan code is e1 (pause/break) just skip it
	beq irq_done



	lda KEYBOARD_flags
	and #KEYBOARD_ctrl
	beq irq_charLookup
	cpx #$77
	bne irq_charLookup	; if the key is pause/break
	lda #$99
	ldx KEYBOARD_writeptr
	sta KEYBOARD_BUFFER, x
	inc KEYBOARD_writeptr
	jmp irq_done

; else, look up the character by scancode

irq_charLookup:
	lda KEYBOARD_flags
	and #KEYBOARD_shift	; if shift is pressed, use the shifted keymap
	bne irq_keyshifted

	lda KEYBOARD_flags	; if altGr is pressed, use that keymap
	and #KEYBOARD_altgr
	bne irq_keyAltgr

	lda scancodetable, x	; else, use the normal keymap

irq_storeChar:
	beq irq_done		; skip $00

	ldx KEYBOARD_writeptr
	sta KEYBOARD_BUFFER, x

	inc KEYBOARD_writeptr

irq_done:
	pla
	tax
	pla
	rti

irq_extendedkey_jump:
	jmp irq_extendedkey
irq_releasekey_jump:
	jmp irq_releasekey

irq_keyshifted:
	lda scancodetable_shift, x
	jmp irq_storeChar

irq_keyAltgr:
	lda scancodetable_altgr, x
	jmp irq_storeChar

; setting of flags

irq_setRelease:
	ora #KEYBOARD_release
	sta KEYBOARD_flags
	jmp irq_done
irq_setExtended:
	ora #KEYBOARD_ext
	sta KEYBOARD_flags
	jmp irq_done
irq_setShift:
	ora #KEYBOARD_shift
	sta KEYBOARD_flags
	jmp irq_done
irq_setCtrl:
	ora #KEYBOARD_ctrl
	sta KEYBOARD_flags
	jmp irq_done



irq_releasekey:				; handler for key releases
	lda KEYBOARD_flags
	and #(~KEYBOARD_release)	; turn off the release flag
	sta KEYBOARD_flags

	and #KEYBOARD_ext		; check for an extended key-code
	bne irq_releaseExtKey

	lda KEYBOARD_flags
	cpx #$12
	beq irq_releaseshift
	cpx #$59
	beq irq_releaseshift

	cpx #$14
	beq irq_releasectrl

	jmp irq_done



irq_extendedkey:			; handler for keys with extended keycodes (E0)
	lda KEYBOARD_flags		; turn off the ext-flag
	and #(~KEYBOARD_ext)
	sta KEYBOARD_flags

	lda KEYBOARD_flags		; check for modifier-keys
	cpx #$1f
	beq irq_extendedkey_setSuper
	cpx #$27
	beq irq_extendedkey_setSuper

	cpx #$11
	beq irq_extendedkey_setAltgr

	cpx #$14
	beq irq_setCtrl

	lda scancodetable_ext, x
	jmp irq_storeChar

irq_releaseExtKey:			; Handler for the release of ext-keycodes
	lda KEYBOARD_flags
	and #(~KEYBOARD_ext & ~KEYBOARD_release)
	sta KEYBOARD_flags

	cpx #$1f
	beq irq_releaseExtKey_super
	cpx #$27
	beq irq_releaseExtKey_super

	cpx #$14
	beq irq_releasectrl

	cpx #$11
	beq irq_releaseExtKey_altgr

	jmp irq_done

;flags release

irq_releaseshift:
	and #(~KEYBOARD_shift)
	sta KEYBOARD_flags
	jmp irq_done

irq_releasectrl:
	and #(~KEYBOARD_ctrl)
	sta KEYBOARD_flags
	jmp irq_done


irq_extendedkey_setSuper:
	ora #KEYBOARD_super
	sta KEYBOARD_flags
	jmp irq_done
irq_releaseExtKey_super:
	and #(~KEYBOARD_super)
	sta KEYBOARD_flags
	jmp irq_done


irq_extendedkey_setAltgr:
	ora #KEYBOARD_altgr
	sta KEYBOARD_flags
	jmp irq_done
irq_releaseExtKey_altgr:
	and #(~KEYBOARD_altgr)
	sta KEYBOARD_flags
	jmp irq_done

; F-keys from 0x80 to 0x8b, caps 0x8c, menu 0x8d, arrow uldr 0x8e-0x91, einfg 0x92, pos1 0x93, pgUp 0x94, del 0x95, ende 0x96, pgDn 0x97, drck 0x98
; pause 0x99, rollen 0x9a

scancodetable:
	.byte 	$00,	$88, 	$00,	$84,	$82,	$80,	$81,	$8b,	$00,	$89,	$87,	$85,	$83,	"\t",	"^",	$00
	.byte 	$00,	$00, 	$00,	$00,	$00,	"q",	"1",	$00,	$00,	$00,	"y",	"s",	"a",	"w",	"2",	$00
	.byte 	$00,	"c", 	"x",	"d",	"e",	"4",	"3",	$00,	$00,	" ",	"v",	"f",	"t",	"r",	"5",	$00
	.byte 	$00,	"n", 	"b",	"h",	"g",	"z",	"6",	$00,	$00,	$00,	"m",	"j",	"u",	"7",	"8",	$00
	.byte 	$00,	",", 	"k",	"i",	"o",	"0",	"9",	$00,	$00,	".",	"-",	"l",	"ö",	"p",	"ß",	$00
	.byte 	$00,	$00, 	"ä",	$00,	"ü",	$b4,	$00,	$00,	$8c,	$00,	$0a,	"+",	$00,	"#",	$00,	$00
	.byte 	$00,	"<", 	$00,	$00,	$00,	$00,	$08,	$00,	$00,	"1",	$00,	"4",	"7",	$00,	$00,	$00
	.byte 	"0",	",", 	"2",	"5",	"6",	"8",	$1b,	$00,	$8a,	"+",	"3",	"-",	"*",	"9",	$9a,	$00
	.byte 	$00,	$00,	$00,	$86,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00

scancodetable_shift:
	.byte 	$00,	$88, 	$00,	$84,	$82,	$80,	$81,	$91,	$00,	$89,	$87,	$85,	$83,	"\t",	"°",	$00
	.byte 	$00,	$00, 	$00,	$00,	$00,	"Q",	"!",	$00,	$00,	$00,	"Y",	"S",	"A",	"W",	"\"",	$00
	.byte 	$00,	"C", 	"X",	"D",	"E",	"$",	"§",	$00,	$00,	" ",	"V",	"F",	"T",	"R",	"%",	$00
	.byte 	$00,	"N", 	"B",	"H",	"G",	"Z",	"&",	$00,	$00,	$00,	"M",	"J",	"U",	"/",	"(",	$00
	.byte 	$00,	";", 	"K",	"I",	"O",	"=",	")",	$00,	$00,	":",	"_",	"L",	"Ö",	"P",	"?",	$00
	.byte 	$00,	$00, 	"Ä",	$00,	"Ü",	"`",	$00,	$00,	$8c,	$00,	$0a,	"*",	$00,	"'",	$00,	$00
	.byte 	$00,	">", 	$00,	$00,	$00,	$00,	$08,	$00,	$00,	"1",	$00,	"4",	"7",	$00,	$00,	$00
	.byte 	"0",	",", 	"2",	"5",	"6",	"8",	$1b,	$00,	$8a,	"+",	"3",	"-",	"*",	"9",	$00,	$00
	.byte 	$00,	$00,	$00,	$86,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00

scancodetable_altgr:
	.byte 	$00,	$88, 	$00,	$84,	$82,	$80,	$81,	$91,	$00,	$89,	$87,	$85,	$83,	"\t",	$00,	$00
	.byte 	$00,	$00, 	$00,	$00,	$00,	"@",	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	"²",	$00
	.byte 	$00,	$00, 	$00,	$00,	"¤",	$00,	"³",	$00,	$00,	" ",	$00,	$00,	$00,	$00,	$00,	$00
	.byte 	$00,	$00, 	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	"µ",	$00,	$00,	"{",	"[",	$00
	.byte 	$00,	$00, 	$00,	$00,	$00,	"}",	"]",	$00,	$00,	$00,	$00,	$00,	$00,	$00,	"\\",	$00
	.byte 	$00,	$00, 	$00,	$00,	$00,	$00,	$00,	$00,	$8c,	$00,	$0a,	"~",	$00,	$00,	$00,	$00
	.byte 	$00,	"|", 	$00,	$00,	$00,	$00,	$08,	$00,	$00,	$96,	$00,	$00,	$93,	$00,	$00,	$00
	.byte 	$92,	$95, 	$00,	$00,	$00,	$00,	$1b,	$00,	$8a,	$00,	$97,	$00,	$00,	$94,	$00,	$00
	.byte 	$00,	$00,	$00,	$86,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00

scancodetable_ext:
	.byte 	$00,	$00, 	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00
	.byte 	$00,	$00, 	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00
	.byte 	$00,	$00, 	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$93
	.byte 	$00,	$00, 	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00
	.byte 	$00,	$00, 	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	"/",	$00,	$00,	$00,	$00,	$00
	.byte 	$00,	$00, 	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$0a,	$00,	$00,	$00,	$00,	$00
	.byte 	$00,	$00, 	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$96,	$00,	$8f,	$93,	$00,	$00,	$00
	.byte 	$92,	$95, 	$90,	$00,	$91,	$8e,	$00,	$00,	$00,	$00,	$97,	$00,	$98,	$94,	$00,	$00
	.byte 	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00

nmi:
	rti

	.org $fffa
	.word nmi
	.word reset
	.word irq
