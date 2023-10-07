    .flags HW_STACK
    ; Startup code from programs in ROM with hardware stack

    ; provided by the linker
    .global __seg_ramtext_begin
    .global __seg_ramtext_end
    .global __seg_ramtext_origin_begin
    .global __seg_ramtext_origin_end
    .global __seg_data_begin
    .global __seg_data_end
    .global __seg_data_origin_begin
    .global __seg_data_origin_end

    .global __seg_bss_begin
    .global __seg_bss_end

    .global memcpy
    ; arguments for normal calls on frame B
    .const frameb_arg1 = 0xc800 + 8 * 0
    .const frameb_arg2 = 0xc800 + 8 * 1
    .const frameb_arg3 = 0xc800 + 8 * 2

    .global main

    ; start-up code
    .section init
    .align 0x10000 ; make sure this is at address 0
    nop
    ; enable memory segments a-b
    ldi ph, 0xff
    ldi pl, 0x02
    ldi a, 0x1e
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

    ; populate ramtext secion
    ldi pl, lo(frameb_arg1)
    ldi ph, hi(frameb_arg1)
    ldi a, lo(__seg_ramtext_begin)
    ldi b, hi(__seg_ramtext_begin)
    st a
    inc pl
    st b

    ldi pl, lo(frameb_arg2)
    ldi ph, hi(frameb_arg2)
    ldi a, lo(__seg_ramtext_origin_begin)
    ldi b, hi(__seg_ramtext_origin_begin)
    st a
    inc pl
    st b

    ldi pl, lo(frameb_arg3)
    ldi ph, hi(frameb_arg3)
    ldi a, lo(__seg_ramtext_end - __seg_ramtext_begin)
    ldi b, hi(__seg_ramtext_end - __seg_ramtext_begin)
    st a
    inc pl
    st b

    ldi pl, lo(memcpy)
    ldi ph, hi(memcpy)
    jmp

    ; populate data secion
    ldi pl, lo(frameb_arg1)
    ldi ph, hi(frameb_arg1)
    ldi a, lo(__seg_data_begin)
    ldi b, hi(__seg_data_begin)
    st a
    inc pl
    st b

    ldi pl, lo(frameb_arg2)
    ldi ph, hi(frameb_arg2)
    ldi a, lo(__seg_data_origin_begin)
    ldi b, hi(__seg_data_origin_begin)
    st a
    inc pl
    st b

    ldi pl, lo(frameb_arg3)
    ldi ph, hi(frameb_arg3)
    ldi a, lo(__seg_data_end - __seg_data_begin)
    ldi b, hi(__seg_data_end - __seg_data_begin)
    st a
    inc pl
    st b

    ldi pl, lo(memcpy)
    ldi ph, hi(memcpy)
    jmp

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
main_exit:
    ldi pl, lo(main_exit)
    ldi ph, hi(main_exit)
    jmp

    .const tmp = 0xc000
