    .export memcpy
    .export memset

    ; arguments for normal calls on frame B
    .const frameb_arg1 = 0xc800 + 8 * 0
    .const frameb_arg2 = 0xc800 + 8 * 1
    .const frameb_arg3 = 0xc800 + 8 * 2

    .const dst_from = 0xc800 + 8 * 0
    .const src_from = 0xc800 + 8 * 1

    .const src_to = 0xc800 + 8 * 3

    .const ret_addr = 0xd000 - 8 * 1

    .global __cc_ret

    .section text.memcpy
    ; void *memcpy(void *dst, const void *src, int n)
memcpy:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(ret_addr)
    ldi ph, hi(ret_addr)
    st b
    inc pl
    st a

    ldi pl, lo(frameb_arg1)
    ldi ph, hi(frameb_arg1)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(__cc_ret)
    ldi ph, hi(__cc_ret)
    st  a
    inc pl
    st  b

    ldi pl, lo(frameb_arg3)
    ldi ph, hi(frameb_arg3)
    ld a
    inc pl
    ld b
    ldi pl, lo(frameb_arg2)
    ld pl
    add a, pl
    ldi pl, lo(frameb_arg2 + 1)
    ld ph
    mov pl, a
    mov a, b
    adc a, ph
    mov b, a
    mov a, pl
    ldi pl, lo(src_to)
    ldi ph, hi(src_to)
    st a
    inc pl
    st b

; copy [src_from, src_to) into [dst_from, ...)
copy:
copy_loop:
    ldi ph, hi(src_from)
    ldi pl, lo(src_from)
    ld b
    ldi pl, lo(src_to)
    ld a
    sub b, a
    ldi pl, lo(copy_loop_neq)
    ldi ph, hi(copy_loop_neq)
    jnz
    ldi ph, hi(src_from + 1)
    ldi pl, lo(src_from + 1)
    ld b
    ldi pl, lo(src_to + 1)
    ld a
    sub b, a
    ldi pl, lo(memcpy_exit)
    ldi ph, hi(memcpy_exit)
    jz ; src_to == src_from
copy_loop_neq:
    ; *dst_from := *src_from
    ldi pl, lo(src_from)
    ldi ph, hi(src_from)
    ld a
    inc pl
    ld ph
    mov pl, a
    ld b
    ldi ph, hi(dst_from)
    ldi pl, lo(dst_from)
    ld a
    inc pl
    ld ph
    mov pl, a
    st b
    ; src_from += 1
    ldi pl, lo(src_from)
    ldi ph, hi(src_from)
    ld b
    inc pl
    ld a
    inc b
    adc a, 0
    st a
    dec pl
    st b
    ; dst_from += 1
    ldi pl, lo(dst_from)
    ld b
    inc pl
    ld a
    inc b
    adc a, 0
    st a
    dec pl
    st b

    ldi pl, lo(copy_loop)
    ldi ph, hi(copy_loop)
    jmp

memcpy_exit:
    ldi pl, lo(ret_addr)
    ldi ph, hi(ret_addr)
    ld a
    inc pl
    ld ph
    mov pl, a
    jmp

    .section text.memset
; void *memset(void *b, int c, size_t len);
memset:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(ret_addr)
    ldi ph, hi(ret_addr)
    st b
    inc pl
    st a

    ldi pl, lo(frameb_arg1)
    ldi ph, hi(frameb_arg1)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(__cc_ret)
    ldi ph, hi(__cc_ret)
    st  a
    inc pl
    st  b

memset_loop:
    ; len -= 1
    ldi pl, lo(frameb_arg3)
    ldi ph, hi(frameb_arg3)
    ld  b
    inc pl
    ld  a
    dec b
    sbb a, 0
    st  a
    ldi pl, lo(frameb_arg3)
    st  b
    ldi pl, lo(memset_exit)
    ldi ph, hi(memset_exit)
    jc

    ; *b = c
    ldi pl, lo(frameb_arg2)
    ldi ph, hi(frameb_arg2)
    ld  b
    ldi pl, lo(frameb_arg1 + 1)
    ldi ph, hi(frameb_arg1 + 1)
    ld  a
    dec pl
    ld  pl
    mov ph, a
    st  b

    ; b += 1
    ldi pl, lo(frameb_arg1)
    ldi ph, hi(frameb_arg1)
    ld  b
    ; hi byte is already in a
    inc b
    adc a, 0
    st  b
    inc pl
    st  a

    ldi pl, lo(memset_loop)
    ldi ph, hi(memset_loop)
    jmp

memset_exit:
    ldi pl, lo(ret_addr)
    ldi ph, hi(ret_addr)
    ld a
    inc pl
    ld ph
    mov pl, a
    jmp
