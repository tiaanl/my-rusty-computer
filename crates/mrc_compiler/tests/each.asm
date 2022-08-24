aaa
aad
aam
aas
cbw
clc
cld
cli
cmc
cmpsb
cmpsw
cwd
daa
das
hlt
in al,0x20
int 0x20
into
iret
jnz many
many:
lahf
loop alot
alot:
movsb
movsw
nop
out 0x20,al
popf
pushf

; nasm compiles there all as REP ?!
rep movsb
rep cmpsb
rep stosb
rep scasb
repne movsb
repne cmpsb
repne stosb
repne scasb
repnz movsb
repnz cmpsb
repnz stosb
repnz scasb

ret
sahf
movsb
cmpsb
stosb
scasb
movsw
cmpsw
stosw
scasw
clc
stc
cld
std
cli
sti
;wait
xlat
