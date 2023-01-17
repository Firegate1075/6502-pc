PORTB=$6000
PORTA=$6001
DDRB=$6002
DDRA=$6003

E=%00001000
RW=%00000100
RS=%00000010


	.org $8000
reset:
	lda #$ff
	sta DDRB
	ldx #$ff
	txs				; initialize stack pointer to $ff

	jsr lcd_init			; set 4-bi mode
	lda #$28			; 4-bit mode, 2-lines, 5x8 Font
	jsr lcd_sendInstruction
	lda #$0e			; Display on, cursor on, blink off 
	jsr lcd_sendInstruction
	lda #$06 			; Left-to-right, shift off
	jsr lcd_sendInstruction
	lda #$01			; clear display
	jsr lcd_sendInstruction

	jsr printString

loop:
	jmp loop

text: 	.byte "Linus M"
	.byte $f5
	.string "ller"


printString:
	ldx #0

printChar:
	lda text, x
	beq done

	jsr lcd_sendChar
	inx
	jmp printChar

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

	lda #%00001111 ; Data-pins as inputs, control signals as outputs
	sta DDRB


lcd_busy:
	lda #RW		; read from display
	sta PORTB
	ora #E
	sta PORTB

	lda PORTB
	pha

	lda #RW
	sta PORTB
	eor #E
	sta PORTB

	pla
	and #%10000000
	bne lcd_busy

	lda #RW
	sta PORTB
	lda #$ff
	sta DDRB	; PORT B as outputs

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

	.org $fffc
	.word reset
	.word $ffff
