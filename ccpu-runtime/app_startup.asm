    ; Startup code from programs loaded into lo RAM

    ; provided by the linker
    .global __seg_bss_begin
    .global __seg_bss_end

    .global main

    ; start-up code
    .section init
    .align 0x10000 ; make sure this is at address 0
    ; save return address
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(ret)
    ldi ph, hi(ret)
    st b
    inc pl
    st a

    ; enable memory segments a-b, lo RAM
    ldi ph, 0xff
    ldi pl, 0x02
    ldi a, 0x1f
    st a

    ; enable hardware stack
    ldi pl, lo(0xFC03)
    ldi ph, hi(0xFC03)
    ldi a, 1
    st a
    ; set SP1 to 1, SP0 to 0
    ; stack grows upwards
    ldi pl, lo(0xFC01)
    st a
    dec pl
    dec a
    st a

    ; initialize BSS
    ldi pl, lo(tmp)
    ldi ph, hi(tmp)
    ldi a, lo(__seg_bss_begin)
    st  a
    inc pl
    ldi a, hi(__seg_bss_begin)
    st  a
bss_loop:
    ldi pl, lo(tmp + 1)
    ldi ph, hi(tmp)
    ld  a
    ldi b, hi(__seg_bss_end)
    sub b, a
    dec pl
    ld  a
    ldi pl, lo(__seg_bss_end)
    sub a, pl
    or  a, b
    ldi pl, lo(bss_loop_end)
    ldi ph, hi(bss_loop_end)
    jz
    ldi pl, lo(tmp)
    ldi ph, hi(tmp)
    ld  a
    inc pl
    ld  ph
    mov pl, a
    mov a, 0
    st  a
    inc pl
    adc a, ph
    mov b, a
    mov a, pl
    ldi pl, lo(tmp)
    ldi ph, hi(tmp)
    st  a
    inc pl
    st  b
    ldi pl, lo(bss_loop)
    ldi ph, hi(bss_loop)
    jmp
bss_loop_end:


    ; call main
    ldi pl, lo(main)
    ldi ph, hi(main)
    jmp

    ; disable hardware stack
    ldi pl, lo(0xFC03)
    ldi ph, hi(0xFC03)
    mov a, 0
    st a

    ; enable memory segments a-c, lo ram
    ldi ph, 0xff
    ldi pl, 0x02
    ldi a, 0x3f
    st a

    ldi pl, lo(ret)
    ldi ph, hi(ret)
    ld a
    inc pl
    ld ph
    mov pl, a
    jmp

    .section bss
    .align 2
ret: res 2

    .const tmp = 0xc000

