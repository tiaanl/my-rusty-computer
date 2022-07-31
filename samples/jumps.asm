    jmp start
    times 512 db 0
middle:
    times 512 db 0
start:
    nop
    jmp middle
