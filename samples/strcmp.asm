        push    bx
        push    cx
        push    dx
        push    si
        push    di

loop_strcmp_loop1:
        mov     al, byte cs:[si]
        inc     si
        cmp     al, bl
        jg      loop_strcmp_great
        cmp     al, bl
        jl      loop_strcmp_less
        cmp     al, '$'
        je      loop_strcmp_quit
        cmp     bl, '$'
        je      loop_strcmp_quit
        jmp     loop_strcmp_loop1

loop_strcmp_quit:
        mov     al, 1
        jmp     done

loop_strcmp_great:
        mov     al, 2
        jmp     done

loop_strcmp_less:
        mov     al, 0
        jmp     done

done:
        pop     di
        pop     si
        pop     dx
        pop     cx
        pop     bx
        ret
