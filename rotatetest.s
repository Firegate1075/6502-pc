	.org $8000
reset:
	lda #$ff
	sta $6002

	clc
	lda #$1
loop:
	ror
	sta $6000
	jmp loop 


	.org $FFFC
	.word reset
	.word $0000
