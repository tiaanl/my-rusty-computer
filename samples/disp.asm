mov word [value1], 5
mov word [cs:value1], 5
mov [cs:value1],al

add di, 320

data:
    times 10 db 0

value1 db 0
