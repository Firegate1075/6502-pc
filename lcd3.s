PORTB=$6000
PORTA=$6001
DDRB=$6002
DDRA=$6003

E=%10000000
RW=%01000000
RS=%00100000


	.org $8000
reset:
	lda #$ff
	sta DDRB
	sta DDRA

	lda #$01			; clear display
	jsr lcd_sendInstruction
	lda #$06			; Left-to-right, shift off
	jsr lcd_sendInstruction
	lda #$0e 			; Display on, cursor on, blink off
	jsr lcd_sendInstruction
	lda #$38			; 8-bit mode, 2-lines, 5x8 Font
	jsr lcd_sendInstruction

	jsr printString

loop:
	jmp loop

text: .string "Hello World"


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

lcd_wait:
	pha
	lda #0
	sta DDRB 	; PORT B as inputs


lcd_busy:
	lda #RW		; read from display
	sta PORTA

	lda #(RW|E)
	sta PORTA

	lda PORTB
	and #%10000000
	bne lcd_busy

	lda #0
	sta PORTA

	lda #$ff
	sta DDRB	; PORT B as outputs

	pla
	rts


lcd_sendInstruction:
	jsr lcd_wait
	sta PORTB

	lda #0
	sta PORTA
	lda #E
	sta PORTA
	lda #0
	sta PORTA

	rts

lcd_sendChar:
	jsr lcd_wait
	sta PORTB

	lda #0
	sta PORTA

	lda #(E|RS)
	sta PORTA

	lda #0
	sta PORTA

	rts

	.org $fffc
	.word reset
	.word $ffff
