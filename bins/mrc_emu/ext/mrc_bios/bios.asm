	BITS 16
	ORG 0xFFFF0000

boot:
	cli
	cld

	mov	ax, cs
	mov	es, ax
	mov	ds, ax

	; point the stack to 0000:0900
	xor	ax, ax
	mov	ss, ax
	mov	bp, 0x900
	mov	sp, bp

	mov	ax, 0xF0F0
	push	ax
	pop	bx
	cmp	bx, 0xF0F0
	jz	.stack_works

	; if the stack doesn't work, we print a character to the screen; we can't use call here, because it uses the
	; supposedly broken stack.
	mov	ax, 0xB800
	mov	es, ax
	xor	di, di
	mov 	al, 0x7F
	stosb
	hlt	; the stack does not work :(

.stack_works

	; Set cursor position to (0, 0)
	mov	ah, 0x02
	xor	bx, bx
	xor	dx, dx
	int	0x10

	mov	si, msg_welcome_string
	call	print_message

	hlt

print_message:
	; DS:SI = address of null terminated message
	mov	ah, 0x0E
	xor	bx, bx

.loop:
	lodsb
	int	0x10
	cmp	al, 0
	jz	.done
	jmp	.loop
.done:
	ret

msg_stack_not_working	db	"Stack not working!",0
msg_welcome_string	db	"Ok!",0

	; pad the file so that the reset_vector starts at F000:E000
	TIMES   0xFFF0-($-$$) DB 0xFF

reset_vector:  ; F000:E000
	jmp	boot

	TIMES	0x10000-($-$$) db 0xFF
