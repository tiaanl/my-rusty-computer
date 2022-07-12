start:
	mov ax, cs
	mov ds, ax			; use the code segment for the data segment as well

	mov ax, 3501h			; get interrupt vector (1)
	int 21h
	mov [cs:old_timer_int_seg], es	; store the old timer int segment
	mov [cs:old_timer_int_off], bx	; store the old timer int offset

	mov dx, timer_int
	mov ax, 2501h



timer_int:
	iret

old_timer_int_seg: dw 0x0000
old_timer_int_off: dw 0x0000
