        BITS    16
        ORG     0x100

start:
        call    add_one
        call    add_two

add_one:
        inc byte [count]
        ret

add_two:
        mov al, [count]
        add al, 2
        mov [count], al
        ret

count:
        db      0x01
