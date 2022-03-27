        BITS 16
        ORG 0xFE000

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

.stack_works:

	; Set cursor position to (0, 0)
	mov	ah, 0x02
	xor	bx, bx
	xor	dx, dx
	int	0x10

%include "cpu_test.asm"
	jnc	.continue_1
	mov	si, msg_cpu_test_failed
	call	print_message
	hlt

.continue_1:
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

init_pic:
	cli
	; init master pic
	mov al, 0x1D
	out 0x20, al	; icw1
	mov al, 0x6B
	out 0x21, al	; icw2
	mov al, 0x08
	out 0x21, al	; icw3
	mov al, 0x6B
	out 0x21, al	; icw4

	; init slave pic
	mov	al, 0x1D
	out	0xA0, al	; icw1
	mov	al, 0x6B
	out	0xA1, al	; icw2
	mov	al, 0x03
	out	0xA1, al	; icw3

	mov	al, 0x03
	out	0xA0, al
	sti

msg_stack_not_working	db	"Stack not working!",0
msg_cpu_test_failed	db	"CPU test failed!",0
msg_welcome_string	db	"Ok!",0

	; pad the file so that the reset_vector starts at F000:E000
	TIMES   0x1FF0-($-$$) DB 0xFF

reset_vector:  ; F000:E000
	jmp	boot

	TIMES	0x2000-($-$$) db 0xFF
