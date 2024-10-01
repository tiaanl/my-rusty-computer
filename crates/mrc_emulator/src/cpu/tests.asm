; Test lahf and sahf for future tests.
        mov ah, 0xFF            ; Try to set all flags
        sahf                    ; Store AH into the flags register
        lahf                    ; Load all the flags back into AH
        cmp ah, 0xD5            ; Check if we have all the expected flags set
        jne error               ; ...if not, error

; Test conditional jumps on flags.
        mov ah, 0xD5
        sahf
        jnc error               ; bit 0
        jnp error               ; bit 2
        jnz error               ; bit 6
        jns error               ; bit 7

        not ah
        sahf
        jc error                ; bit 0
        jp error                ; bit 2
        jz error                ; bit 6
        js error                ; bit 7

test_add:
        mov ax, 0
        add ax, 1
        jc error
        jp error
        jz error
        js error
        jo error

        cmp ax, 1
        jnz error

success:
        jmp 0xF000:0xFFFF

error:
        hlt
