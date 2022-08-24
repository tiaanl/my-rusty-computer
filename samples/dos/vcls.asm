; Adapted from https://github.com/skissane/dos-assembly/blob/master/DOS/VCLS.ASM

start:
    call bios_clear_screen
    call bios_home_cursor

    mov ax, 0x4C00
    int 0x21

bios_home_cursor:
    xor dx, dx
    call bios_set_cursor_pos
    ret

; in:
; - dh = row
; - dl = column
bios_set_cursor_pos:
    push dx
    call bios_get_video_mode ; sets bh="active page" which int 0x10,0x02 requires
    pop dx
    mov ah, 2
    int 0x10
    ret

; out:
; - al = video mode
; - ah = columns per line
; - bh = active display page
bios_get_video_mode:
    mov ah, 0x0F
    int 0x10
    ret

bios_clear_screen:
    call bda_get_columns
    push ax
    call bda_get_rows
    mov dh, al ; ch = row of lower right corner of rectangle
    pop ax
    mov dl, al ; cl = column of lower right corner of rectangle

    mov bh, 0x07 ; bh = video attribyte (7 = white on black)
    mov al, 0x00 ; al = number of lines to scroll (0 = blank rectangle)
    mov cx, 0x00 ; dh,dl: row,col of upper left; (0,0) = origin
    mov ah, 0x06 ; function 6: scroll up / clear rectangle
    int 0x10
    ret

; in:
;   di: offset
; out:
;   al: value at this offset
get_bda_byte:
    push ds
    mov ax, 0x40
    push ax
    pop ds
    xor ax, ax
    mov al, [di]
    pop ds
    ret

; out:
;   al = columns of screen
bda_get_columns:
    mov di, 0x4A
    call get_bda_byte
    ret

; out:
;   al = rows of screen
bda_get_rows:
    mov di, 0x84
    call get_bda_byte
    ret

; %include "libzutl.inc"
;
; start:
;         call BiosClearScreen
;         BiosHomeCursor
;         putz mScreenCleared
;         exit 0
; mScreenCleared:
;         db "Screen cleared", crlf,0
