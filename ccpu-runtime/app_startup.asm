    ; Startup code from programs loaded into lo RAM

    ; provided by the linker
    .global __seg_bss_begin
    .global __seg_bss_end

    .global main

    ; start-up code
    .section init
    .align 0x10000 ; make sure this is at address 0

    ; enable memory segments a-b, d-e lo RAM
    ldi ph, 0xff
    ldi pl, 0x02
    ldi a, 0xdf
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

    ; syscall number 12 - restart
    ldi a, 12
    ldi ph, 0xc8
    ldi pl, 0
    st  a
    ldi ph, 0xb0
    jmp

    .const tmp = 0xc800

