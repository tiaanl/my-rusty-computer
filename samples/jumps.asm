top:
    jmp start
    times 1024 - 64 nop

middle:
    times 64 nop

start:
    times 3 jz middle
    jz top
