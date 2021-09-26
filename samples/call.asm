        BITS    16
        ORG     0x100

start:
        mov     ax, cs
        mov     ds, ax
        call    add_one
        call    add_two

add_one:
        inc     byte [count]
        ret

add_two:
        mov     al, [count]
        add     al, 2
        mov     [count], al
        ret

exit:
        mov     ax, 4C00h
        int     21h

count:
        db      0x01
