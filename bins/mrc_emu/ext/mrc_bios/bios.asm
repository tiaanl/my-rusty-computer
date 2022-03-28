BITS 16
ORG 0xFE000

start:
    hlt

; Pad the binary with 0's until we put the reset vector jump.
TIMES 0x1FF0 - ($ - $$) DB 0x00

reset_vector:  ; F000:E000
    jmp     start

; Pad to the end of the file.
TIMES 0x2000 - ($ - $$) DB 0x00
