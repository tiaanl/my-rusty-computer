    ; set up the timer to have a frequency of 100Hz.

    cli

    mov     al, 0b00110110      ; sc = 00, rw = 11, m = 011, bcd = 0
    out     0x43, al            ; write control register to pit

    mov     ax, 11931           ; clock frequency / divisor = 100Hz, divisor = 1193182 / 100
    out     0x40, al            ; send low byte
    mov     al, ah
    out     0x40, al            ; send the high byte

    sti

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
