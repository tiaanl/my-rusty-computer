        BITS    16
        ORG     0x100

start:
        mov     ax, cs
        mov     ds, ax
        call    add_one
        call    add_two

add_one:
        inc     byte [count]            ; increase the count
        ret

add_two:
        mov     al, [count]
        add     al, 2
        mov     [count], al
        ret

exit:
        mov     ax, 0x4C00
        int     0x21

count:  db      0x01                    ; stores a sample count
