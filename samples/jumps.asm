    jmp start
    times 1024 - 64 nop

middle:
    times 64 nop

start:
    nop
    jmp middle
    loop middle
    loopne middle
    loope middle
    loopnz middle
    loopz middle
