SCREEN	= $4000 ; 80x32-character VGA-screen

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

SCREEN_xptr		= $0010
SCREEN_yptr		= $0011
SCREEN_addressBuf	= $0014 ; 2 byte buffer
SCREEN_charBuf		= $0016

KEYBOARD_super		= %01000000
KEYBOARD_altgr		= %00100000
KEYBOARD_ext		= %00010000
KEYBOARD_release 	= %00001000
KEYBOARD_ctrl		= %00000100
KEYBOARD_alt		= %00000010
KEYBOARD_shift		= %00000001



	.org $8000
reset:
	sei

	ldx #$ff
	txs				; initialize stack pointer to $ff

	lda #$00
	sta KEYBOARD_readptr
	sta KEYBOARD_writeptr
	sta KEYBOARD_flags
	sta SCREEN_xptr
	sta SCREEN_yptr
	sta SCREEN_addressBuf
	sta SCREEN_addressBuf + 1
	sta SCREEN_charBuf

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


; Main Program --------------------------------------------------------------

	cli				; enable interrupts
	lda #$4a
	sta SCREEN_addressBuf + 1
	lda #$05
	sta SCREEN_addressBuf

;	jsr screen_printString

loop:

; handle keyboard inputs
	ldx KEYBOARD_readptr	; get the position of the last char read from the KEYBOARD Buffer
	cpx KEYBOARD_writeptr	; check if the there are unchecked chars in the buffer
	beq loop		; if not: loop

	lda KEYBOARD_BUFFER, x
	and #%01100000
	beq controlChar_handler	; if the character is non-printable, go to the controlChar handler

	lda KEYBOARD_BUFFER, x	; else print the char on screen

	jsr screen_computeAddress
	sta (SCREEN_addressBuf)

	inc SCREEN_xptr



	inc KEYBOARD_readptr

	jmp loop

controlChar_handler:
	lda KEYBOARD_BUFFER, x

	inc KEYBOARD_readptr
	cmp #$0a
	beq enter
	cmp #$0b
	beq tab
	cmp #$08
	beq backspace
	cmp #$1b
	beq esc
	cmp #$8e
	beq arrowUp
	cmp #$8f
	beq arrowLeft
	cmp #$90
	beq arrowDown
	cmp #$91
	beq arrowRight



	jmp loop

enter:
	inc SCREEN_yptr

	lda #00
	sta SCREEN_xptr

	jmp loop

tab:
	lda #" "	; just do 4 spaces

	jsr screen_computeAddress
	sta (SCREEN_addressBuf)
	inc SCREEN_xptr

	jsr screen_computeAddress
	sta (SCREEN_addressBuf)
	inc SCREEN_xptr

	jsr screen_computeAddress
	sta (SCREEN_addressBuf)
	inc SCREEN_xptr

	jsr screen_computeAddress
	sta (SCREEN_addressBuf)
	inc SCREEN_xptr

	jmp loop



backspace:
	lda SCREEN_xptr
	beq loop		; skip if on the left border

	dec SCREEN_xptr

	jsr screen_computeAddress
	lda #" "
	sta (SCREEN_addressBuf)

	jmp loop




esc:
	ldy #$1f
esc_yLoop:
	ldx #$7f
esc_xLoop:
	sty SCREEN_yptr
	stx SCREEN_xptr

	jsr screen_computeAddress

	lda #" "
	sta (SCREEN_addressBuf)


	dex
	cpx #$ff
	bne esc_xLoop

	dey
	cpy #$ff
	bne esc_yLoop

	jmp loop

arrowUp:
	lda SCREEN_yptr
	beq arrow_done		; skip if at upper border of screen

	dec SCREEN_yptr
	jmp loop

arrowLeft:
	lda SCREEN_xptr
	beq arrow_done

	dec SCREEN_xptr
	jmp loop

arrowDown:
	lda SCREEN_yptr
	cmp #$1f
	beq arrow_done

	inc SCREEN_yptr
	jmp loop

arrowRight:
	lda SCREEN_xptr
	cmp #$4f
	beq arrow_done

	inc SCREEN_xptr
	jmp loop

arrow_done:
	jmp loop


text:
	.string "Hello world!"


screen_printString:
	ldx #00
screen_printString_printChar:
	lda text, x
	beq screen_printString_done

	sta SCREEN, x
	inx
	jmp screen_printString_printChar

screen_printString_done:
	rts

screen_computeAddress:
	pha

	lda SCREEN_yptr
	lsr
	and #$0f
	ora #$40
	sta SCREEN_addressBuf+1

	lda SCREEN_yptr
	and #1
	asl
	asl
	asl
	asl
	asl
	asl
	asl
	ora SCREEN_xptr
	sta SCREEN_addressBuf

	pla
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
