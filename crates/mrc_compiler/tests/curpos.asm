main:
    jmp main

; pad some bytes so the signature ends up in the last two bytes of the 512 sector.
times 510 - ($ - $$) db 0x00

db 0x55, 0xaa
