org 0x100

mov	dx, message
mov	ah, 0x09
int	0x21

mov     ax, 0x4c00
int	0x21

message: db 'Hello, World!',0Dh,0Ah,'$'

