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

	lda #$01
	sta PORTB
	
	lda #E
	sta PORTA
	lda #0
	sta PORTA

;	lda #0
;	sta DDRB
;wait1:
;	lda PORTB
;	and #%10000000
;	bne wait1
	
;	lda #$ff
;	sta DDRB

	lda #$06
	sta PORTB

	lda #E
	sta PORTA
	lda #0
	sta PORTA

	lda #$0e ; Display on, cursor on, blink off
	sta PORTB

	lda #E
	sta PORTA
	lda #0
	sta PORTA

	lda #$38
	sta PORTB

	lda #E
	sta PORTA
	lda #0
	sta PORTA

	lda #"L"
	sta PORTB

	lda #(E|RS)
	sta PORTA
	lda #0
	sta PORTA

	lda #"i"
	sta PORTB

	lda #(E|RS)
	sta PORTA
	lda #0
	sta PORTA

	lda #"n"
	sta PORTB

	lda #(E|RS)
	sta PORTA
	lda #0
	sta PORTA

	lda #"u"
	sta PORTB

	lda #(E|RS)
	sta PORTA
	lda #0
	sta PORTA

	lda #"s"
	sta PORTB

	lda #(E|RS)
	sta PORTA
	lda #0
	sta PORTA

loop:
	jmp loop

	.org $fffc
	.word reset
	.word $ffff
