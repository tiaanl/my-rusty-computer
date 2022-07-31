add al, 3
add ax, 0x1234
add bl, 3
add bx, 0x1234
add byte [bx], 3
add byte [bx+si], 4
add byte [bx], 0x12
add word [0x1234], 0x1234
add word [bx], -3
add word [0x1234], 0x1234
add cl, bl
add cx, bx
add [bx], cl
add [bx], cx
add cl, [bx]
add cx, [bx]

adc cl, bl
and cl, bl
cmp cl, bl
or cl, bl
sub cl, bl
sbb cl, bl
xor cl, bl

sub word [0x100], -3
