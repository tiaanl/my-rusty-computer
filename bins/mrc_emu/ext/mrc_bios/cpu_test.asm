cpu_test:
	mov	si, .msg_1
	call	print_message

	; Begin FLAG test of CPU
	xor	ax, ax
	jb	.fail
	jo	.fail
	js	.fail
	jnz	.fail
	jpo	.fail
	add	ax, 1
	jz	.fail
	jpe	.fail
	sub	ax, 0x8002
	js	.fail
	inc	ax
	jno	.fail
	shl	ax, 1
	jnb	.fail
	jnz	.fail
	shl	ax, 1
	jb	.fail

	; Begin REGISTER test of CPU
	mov	bx, 0b0101010101010101
.cpu_test:
	mov	bp, bx
	mov	cx, bp
	mov	sp, cx
	mov	dx, sp
	mov	ss, dx
	mov	si, ss
	mov	es, si
	mov	di, es
	mov	ds, di
	mov	ax, ds
	cmp	ax, 0b0101010101010101
	jnz	.cpu_1
	not	ax
	mov	bx, ax
	jmp	.cpu_test

.cpu_1:
	xor	ax, 0b1010101010101010
	jnz	.fail
	jmp	.cpu_ok

.fail:
	stc
	jmp	.done

.cpu_ok:
	clc
	jmp	.done

.msg_1		db	"Phase 1...",0

.done:
