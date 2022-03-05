    mov ah, 0x00
next_outer:
    mov cx, 0x10        ; set the count to 8 dots
    mov dx, 0x00        ; set the port number we will write the color to
    mov al, ah          ; set the color index
next:
    out dx, al          ; write the color to the port
    inc dx              ; increment the port
    inc al              ; increment the color
    loop next

    inc ah

    jmp next_outer
    hlt
