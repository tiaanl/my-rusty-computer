mov ax, [bx + si]
mov ax, [bx + di]
mov ax, [bp + si]
mov ax, [bp + di]
mov ax, [si]
mov ax, [di]
mov ax, [0x1234]
mov bx, [0x1234]
mov ax, [bx]

mov ax, [bx + si + 0x12]
mov ax, [bx + di + 0x12]
mov ax, [bp + si + 0x12]
mov ax, [bp + di + 0x12]
mov ax, [si + 0x12]
mov ax, [di + 0x12]
mov ax, [bp + 0x12]
mov ax, [bx + 0x12]

mov ax, [bx + si + 0x1234]
mov ax, [bx + di + 0x1234]
mov ax, [bp + si + 0x1234]
mov ax, [bp + di + 0x1234]
mov ax, [si + 0x1234]
mov ax, [di + 0x1234]
mov ax, [bp + 0x1234]
mov ax, [bx + 0x1234]

mov al, al
mov al, ah
mov al, bl
mov al, bh
mov al, cl
mov al, ch
mov al, dl
mov al, dh

mov ax, ax
mov ax, cx
mov ax, dx
mov ax, bx
mov ax, sp
mov ax, bp
mov ax, si
mov ax, di
