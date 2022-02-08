        BITS    16
        ORG     100h

PSP     equ     80h

start:
        xor     bx, bx
        mov     bl, [PSP]
        cmp     bl, 7Eh
        ja      exit

        mov     word [current_position], 081h    ; start processing after the "length" byte

process_char:
        xor     ax, ax                      ; reset the value
        xor     bx, bx                      ; reset the multiplier
        xor     cx, cx                      ; reset the total
        mov     bl, 1h

    .next:
        mov     di, [current_position]      ; start at the current position

        mov     al, [di]                    ; move the character into the accumulator

        cmp     al, ' '                     ; check if the character is a space
        je      .skip                       ; if it is a space, skip the character

        sub     al, '0'                     ; subtract '0' from it to get a decimal value
        cmp     al, 09h                     ; check if it is above 9
        jg      exit                        ; invalid character? exit program

        mul     bx                          ; multiply the value with the decimal multiplier
        add     cx, ax                      ; add the new character value to the total
        add     bx, 10                      ; increase the decimal multiplier

        xor     dx, dx
        mov     dl, [PSP]
        add     dx, di
        cmp     dx, [current_position]
        jge     .done

    .skip:
        inc     di
        mov     [current_position], di
        jmp     process_char

    .done:

print_arguments:
        mov     byte [bx+PSP+1h], '$'
        mov     ah, 9h
        mov     dx, 82h     ; start printing after the initial space between the command and the arguments.
        int     21h

exit:
        mov     ax, 4C00h
        int     21h

; DATA

average_result      dw 0000h
current_position    dw  0000h
