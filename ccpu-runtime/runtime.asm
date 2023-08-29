    .export __cc_asr_word
    .export __cc_lsr_dword
    .export __cc_asl_word
    .export __cc_asl_dword
    .export __cc_lsr_word
    .export __cc_mul_byte
    .export __cc_mul_word
    .export __cc_mul_dword
    .export __cc_div_byte
    .export __cc_div_word
    .export __cc_mod_byte
    .export __cc_mod_word
    .export __cc_udiv_byte
    .export __cc_udiv_word
    .export __cc_umod_byte
    .export __cc_umod_word

    .export __cc_ret

    ; arguments for intrinsic calls
    .const intrin_arg1 = 0xc800 - 8 * 4
    .const intrin_arg2 = 0xc800 - 8 * 5
    .const intrin_arg3 = 0xc800 - 8 * 6
    .const intrin_result = 0xc800 - 8 * 3

    .const shift_rhs = 0xc800 - 8 * 5

    ; bit shifts of word values
    ; rhs (bit count) is one byte
    .section text.__cc_asr_word
__cc_asr_word:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(int_ret)
    ldi ph, hi(int_ret)
    st b
    inc pl
    st a

    ldi pl, lo(intrin_arg1)
    ldi ph, hi(intrin_arg2)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(shift_lhs)
    ldi ph, hi(shift_lhs)
    st  a
    inc pl
    st  b

    ldi ph, hi(shift_rhs)
    ldi pl, lo(shift_rhs)
    ld b
    ldi pl, lo(return_val_sign)
    ldi ph, hi(return_val_sign)
    ldi a, 15
    sub a, b ; 15 - count
    jc ; 15 < count

    ; shift_lhs = shift_lhs >> shift_rhs
    ldi a, 8
    sub b, a ; lo(count) - 8
    ldi pl, lo(__cc_asr_count_lt_8)
    ldi ph, hi(__cc_asr_count_lt_8)
    jc ; lo(count) < 8
    sub b, a
    ; b = count - 16

    ; lo(val) := hi(val)
    ; hi(val) := sign(val)
    ldi pl, lo(shift_lhs + 1)
    ldi ph, hi(shift_lhs)
    ld a
    dec pl
    st a
    inc pl
    shl a ; sign -> carry
    exp a
    st a
__cc_asr_count_lt_8:
    ; b = count - 8

    ldi a, 8
    add b, a
    ldi pl, lo(shift_exit)
    ldi ph, hi(shift_exit)
    jz ; count == 0

    ; b = count
__cc_asr_loop:
    ldi ph, hi(shift_lhs)
    ldi pl, lo(shift_lhs)
    ld a
    shr a
    st a
    inc pl
    ld a
    sar a
    st a
    ldi pl, lo(__cc_asr_loop_end)
    ldi ph, hi(__cc_asr_loop_end)
    jnc
    ldi ph, hi(shift_lhs)
    ldi pl, lo(shift_lhs)
    ld a
    ldi pl, 0x80
    or a, pl
    ldi pl, lo(shift_lhs)
    st a
__cc_asr_loop_end:
    ldi pl, lo(__cc_asr_loop)
    ldi ph, hi(__cc_asr_loop)
    dec b
    jnz ; count != 0

    ldi pl, lo(shift_exit)
    ldi ph, hi(shift_exit)
    jmp

return_val_sign:
    ldi pl, lo(intrin_result + 1)
    ldi ph, hi(intrin_result)
    ld a
    shl a ; sign -> carry
    exp a
    st a
    dec pl
    st a
    ldi pl, lo(exit)
    ldi ph, hi(exit)
    jmp

    .section text.__cc_lsr_word
__cc_lsr_word:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(int_ret)
    ldi ph, hi(int_ret)
    st b
    inc pl
    st a

    ldi pl, lo(intrin_arg1)
    ldi ph, hi(intrin_arg2)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(shift_lhs)
    ldi ph, hi(shift_lhs)
    st  a
    inc pl
    st  b

    ldi ph, hi(shift_rhs)
    ldi pl, lo(shift_rhs)
    ld b
    ldi pl, lo(return_0)
    ldi ph, hi(return_0)
    ldi a, 15
    sub a, b ; 15 - count
    jc ; 15 < count

    ; shift_lhs = shift_lhs >> shift_rhs
    ldi a, 8
    sub b, a ; lo(count) - 8
    ldi pl, lo(__cc_lsr_count_lt_8)
    ldi ph, hi(__cc_lsr_count_lt_8)
    jc ; lo(count) < 8
    sub b, a
    ; b = count - 16

    ; lo(val) := hi(val)
    ; hi(val) := 0
    ldi pl, lo(shift_lhs + 1)
    ldi ph, hi(shift_lhs)
    ld a
    dec pl
    st a
    inc pl
    mov a, 0
    st a
__cc_lsr_count_lt_8:
    ; b = count - 8

    ldi a, 8
    add b, a
    ldi pl, lo(shift_exit)
    ldi ph, hi(shift_exit)
    jz ; count == 0

    ; b = count
__cc_lsr_loop:
    ldi ph, hi(shift_lhs)
    ldi pl, lo(shift_lhs)
    ld a
    shr a
    st a
    inc pl
    ld a
    shr a
    st a
    ldi pl, lo(__cc_lsr_loop_end)
    ldi ph, hi(__cc_lsr_loop_end)
    jnc
    ldi ph, hi(shift_lhs)
    ldi pl, lo(shift_lhs)
    ld a
    ldi pl, 0x80
    or a, pl
    ldi pl, lo(shift_lhs)
    st a
__cc_lsr_loop_end:
    ldi pl, lo(__cc_asr_loop)
    ldi ph, hi(__cc_asr_loop)
    dec b
    jnz ; count != 0

    ldi pl, lo(shift_exit)
    ldi ph, hi(shift_exit)
    jmp


    ; shift_lhs = shift_lhs << shift_rhs
    .section text.__cc_asl_word
__cc_asl_word:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(int_ret)
    ldi ph, hi(int_ret)
    st b
    inc pl
    st a

    ldi pl, lo(intrin_arg1)
    ldi ph, hi(intrin_arg2)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(shift_lhs)
    ldi ph, hi(shift_lhs)
    st  a
    inc pl
    st  b

    ldi ph, hi(shift_rhs)
    ldi pl, lo(shift_rhs)
    ld b
    ldi pl, lo(return_0)
    ldi ph, hi(return_0)
    ldi a, 15
    sub a, b ; 15 - count
    jc ; 15 < count

    ldi a, 8
    sub b, a ; lo(count) - 8
    ldi pl, lo(__cc_asl_count_lt_8)
    ldi ph, hi(__cc_asl_count_lt_8)
    jc ; lo(count) < 8
    sub b, a
    ; b = count - 16

    ; hi(val) := lo(val)
    ; lo(val) := 0
    ldi pl, lo(shift_lhs)
    ldi ph, hi(shift_lhs)
    ld a
    inc pl
    st a
    dec pl
    mov a, 0
    st a
__cc_asl_count_lt_8:
    ; b = count - 8

    ldi a, 8
    add b, a
    ldi pl, lo(shift_exit)
    ldi ph, hi(shift_exit)
    jz ; count == 0

    ; b = count
__cc_asl_loop:
    ldi ph, hi(shift_lhs)
    ldi pl, lo(shift_lhs + 1)
    ld a
    shl a
    st a
    dec pl
    ld a
    shl a
    st a
    ldi pl, lo(__cc_asl_loop_end)
    ldi ph, hi(__cc_asl_loop_end)
    jnc
    ldi ph, hi(shift_lhs)
    ldi pl, lo(shift_lhs + 1)
    ld a
    ldi pl, 0x01
    or a, pl
    ldi pl, lo(shift_lhs + 1)
    st a
__cc_asl_loop_end:
    ldi pl, lo(__cc_asl_loop)
    ldi ph, hi(__cc_asl_loop)
    dec b
    jnz ; count != 0

    ldi pl, lo(shift_exit)
    ldi ph, hi(shift_exit)
    jmp

    .section text
return_0:
    ldi pl, lo(intrin_result)
    ldi ph, hi(intrin_result)
    mov a, 0
    st a
    inc pl
    st a
    ldi pl, lo(exit)
    ldi ph, hi(exit)
    jmp
shift_exit:
    ldi pl, lo(shift_lhs)
    ldi ph, hi(shift_lhs)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(intrin_result)
    ldi ph, hi(intrin_result)
    st  a
    inc pl
    st  b
exit:
    ldi pl, lo(int_ret)
    ldi ph, hi(int_ret)
    ld a
    inc pl
    ld ph
    mov pl, a
    jmp

return_0_dword:
    ldi pl, lo(intrin_result)
    ldi ph, hi(intrin_result)
    mov a, 0
    st a
    inc pl
    st a
    inc pl
    st a
    inc pl
    st a
    ldi pl, lo(exit)
    ldi ph, hi(exit)
    jmp
shift_dword_exit:
    ldi pl, lo(shift_lhs)
    ldi ph, hi(shift_lhs)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(intrin_result)
    ldi ph, hi(intrin_result)
    st  a
    inc pl
    st  b
    ldi pl, lo(shift_lhs + 2)
    ldi ph, hi(shift_lhs)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(intrin_result + 2)
    ldi ph, hi(intrin_result)
    st  a
    inc pl
    st  b
    ldi pl, lo(exit)
    ldi ph, hi(exit)
    jmp

; shift_lhs = shift_lhs << shift_rhs
    ; .section text.__cc_asl_dword
__cc_asl_dword:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(int_ret)
    ldi ph, hi(int_ret)
    st b
    inc pl
    st a

    ldi ph, hi(intrin_arg1)
    ldi pl, lo(intrin_arg1)
    ld  a
    inc pl
    ld  b
    ldi ph, hi(shift_lhs)
    ldi pl, lo(shift_lhs)
    st  a
    inc pl
    st  b
    ldi ph, hi(intrin_arg1)
    ldi pl, lo(intrin_arg1 + 2)
    ld  a
    inc pl
    ld  b
    ldi ph, hi(shift_lhs)
    ldi pl, lo(shift_lhs + 2)
    st  a
    inc pl
    st  b

    ldi ph, hi(shift_rhs)
    ldi pl, lo(shift_rhs)
    ld  a
    ldi pl, lo(return_0_dword)
    ldi ph, hi(return_0_dword)
    ldi b, 31
    sub b, a ; 31 - count
    jc ; 31 < count
    ldi pl, lo(shift_dword_exit)
    ldi ph, hi(shift_dword_exit)
    add a, 0
    jz

    mov b, a
    ldi pl, 0x7
    and a, pl
    shr b
    shr b
    shr b
    ; a = bit shift, b = byte shift
    ldi pl, lo(bit_shift)
    ldi ph, hi(bit_shift)
    st a

    ; there are three zero bytes in memory before shift_lhs
    mov a, b

    ldi pl, lo(shift_lhs + 3)
    sub pl, a
    ld b
    add pl, a
    st b

    dec pl
    sub pl, a
    ld b
    add pl, a
    st b

    dec pl
    sub pl, a
    ld b
    add pl, a
    st b

    dec pl
    sub pl, a
    ld b
    add pl, a
    st b

asl_dword_loop:
        ldi pl, lo(bit_shift)
        ldi ph, hi(bit_shift)
        ld a
        dec a
        st a
        ldi pl, lo(shift_dword_exit)
        ldi ph, hi(shift_dword_exit)
        jc

        ldi ph, hi(shift_lhs)
        ldi pl, lo(shift_lhs + 3)
        ld a
        dec pl
        ld b
        shl a
        shl b
        adc a, 0
        inc pl
        st a
        mov a, b
        ldi pl, lo(shift_lhs + 1)
        ld b
        shl b
        adc a, 0
        inc pl
        st a
        mov a, b
        ldi pl, lo(shift_lhs)
        ld b
        shl b
        adc a, 0
        st b
        inc pl
        st a

        ldi pl, lo(asl_dword_loop)
        ldi ph, hi(asl_dword_loop)
        jmp

; shift_lhs = shift_lhs >> shift_rhs
    ; .section text.__cc_lsr_dword
__cc_lsr_dword:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(int_ret)
    ldi ph, hi(int_ret)
    st b
    inc pl
    st a

    ldi ph, hi(intrin_arg1)
    ldi pl, lo(intrin_arg1)
    ld  a
    inc pl
    ld  b
    ldi ph, hi(shift_lhs)
    ldi pl, lo(shift_lhs)
    st  a
    inc pl
    st  b
    ldi ph, hi(intrin_arg1)
    ldi pl, lo(intrin_arg1 + 2)
    ld  a
    inc pl
    ld  b
    ldi ph, hi(shift_lhs)
    ldi pl, lo(shift_lhs + 2)
    st  a
    inc pl
    st  b

    ldi pl, lo(shift_rhs)
    ldi ph, hi(shift_rhs)
    ld a
    ldi pl, lo(return_0_dword)
    ldi ph, hi(return_0_dword)
    ldi b, 31
    sub b, a ; 31 - count
    jc ; 31 < count
    ldi pl, lo(shift_dword_exit)
    ldi ph, hi(shift_dword_exit)
    add a, 0
    jz

    mov b, a
    ldi pl, 0x7
    and a, pl
    shr b
    shr b
    shr b
    ; a = bit shift, b = byte shift
    ldi pl, lo(bit_shift)
    ldi ph, hi(bit_shift)
    st a

    ; there are three zero bytes in memory after shift_lhs
    mov a, b

    ldi pl, lo(shift_lhs)
    add pl, a
    ld b
    sub pl, a
    st b

    inc pl
    add pl, a
    ld b
    sub pl, a
    st b

    inc pl
    add pl, a
    ld b
    sub pl, a
    st b

    inc pl
    add pl, a
    ld b
    sub pl, a
    st b

lsr_dword_loop:
        ldi pl, lo(bit_shift)
        ldi ph, hi(bit_shift)
        ld a
        dec a
        st a
        ldi pl, lo(shift_dword_exit)
        ldi ph, hi(shift_dword_exit)
        jc

        ldi ph, hi(shift_lhs)
        ldi pl, lo(shift_lhs)
        ld b
        shr b
        inc pl
        ld a
        shr a
        exp a
        ldi pl, 0x80
        and a, pl
        or b, a
        ldi pl, lo(shift_lhs)
        st b

        inc pl
        ld b
        inc pl
        ld a
        shr b
        shr a
        exp a
        ldi pl, 0x80
        and a, pl
        or b, a
        ldi pl, lo(shift_lhs + 1)
        st b

        inc pl
        ld b
        inc pl
        ld a
        shr b
        shr a
        exp a
        ldi pl, 0x80
        and a, pl
        or b, a
        ldi pl, lo(shift_lhs + 2)
        st b

        inc pl
        ld b
        shr b
        st b

        ldi pl, lo(lsr_dword_loop)
        ldi ph, hi(lsr_dword_loop)
        jmp

; multiply A and B, intrin_result into R
    .section text.__cc_mul_byte
__cc_mul_byte:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(int_ret)
    ldi ph, hi(int_ret)
    st b
    inc pl
    st a

; R := 0
    ldi pl, lo(intrin_result)
    ldi ph, hi(intrin_result)
    mov a, 0
    st a

__cc_mul_byte_loop:
    ; B >>= 1
    ldi pl, lo(intrin_arg2)
    ldi ph, hi(intrin_arg2)
    ld a
    shr a
    st a
    ldi pl, lo(__cc_mul_byte_added)
    ldi ph, hi(__cc_mul_byte_added)
    jnc ; no need to add
    ; R += A
    ldi pl, lo(intrin_arg1)
    ldi ph, hi(intrin_arg1)
    ld a
    ldi pl, lo(intrin_result)
    ld b
    add b, a
    st b
__cc_mul_byte_added:
    ; A <<= 1
    ldi ph, hi(intrin_arg1)
    ldi pl, lo(intrin_arg1)
    ld a
    shl a
    st a

    ; A | B == 0?
    ; a = A
    ldi pl, lo(intrin_arg2)
    ld b
    or a, b
    ldi pl, lo(__cc_mul_byte_loop)
    ldi ph, hi(__cc_mul_byte_loop)
    jnz ; A | B != 0

    ldi pl, lo(exit)
    ldi ph, hi(exit)
    jmp

; multiply A and B, intrin_result into R
    .section text.__cc_mul_word
__cc_mul_word:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(int_ret)
    ldi ph, hi(int_ret)
    st b
    inc pl
    st a

    ; R := 0
    ldi pl, lo(intrin_result)
    ldi ph, hi(intrin_result)
    mov a, 0
    st a
    inc pl
    st a

__cc_mul_word_loop:
    ; lo(B) >>= 1
    ldi pl, lo(intrin_arg2)
    ldi ph, hi(intrin_arg2)
    ld a
    shr a
    st a
    ldi pl, lo(__cc_mul_word_added)
    ldi ph, hi(__cc_mul_word_added)
    jnc ; no need to add
    ; R += A
    ldi pl, lo(intrin_arg1)
    ldi ph, hi(intrin_arg1)
    ld a
    ldi pl, lo(intrin_result)
    ld b
    add b, a
    st b
    ldi pl, lo(intrin_arg1 + 1)
    ld a
    ldi pl, lo(intrin_result + 1)
    ld b
    adc b, a
    st b
__cc_mul_word_added:
    ; hi(B) >>= 1
    ldi pl, lo(intrin_arg2 + 1)
    ldi ph, hi(intrin_arg2)
    ld b
    shr b
    st b
    exp b
    ldi a, 0x80
    and a, b
    ; lo(B) |= c ? 0x80 : 0
    dec pl
    ld b
    or a, b
    st a
    ; A <<= 1
    ldi pl, lo(intrin_arg1)
    ld a
    shl a
    st a
    exp b
    ldi a, 0x01
    and b, a
    inc pl
    ld a
    shl a
    or a, b
    st a

    ; A | B == 0?
    ; a = hi(A)
    dec pl
    ld b
    or a, b
    ldi pl, lo(intrin_arg2)
    ld b
    or a, b
    inc pl
    ld b
    or a, b
    ldi pl, lo(__cc_mul_word_loop)
    ldi ph, hi(__cc_mul_word_loop)
    jnz ; A | B != 0

    ldi pl, lo(exit)
    ldi ph, hi(exit)
    jmp

; multiply A and B, intrin_result into R
    .section text.__cc_mul_dword
__cc_mul_dword:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(int_ret)
    ldi ph, hi(int_ret)
    st b
    inc pl
    st a

    ; R := 0
    ldi pl, lo(intrin_result)
    ldi ph, hi(intrin_result)
    mov a, 0
    st a
    inc pl
    st a
    inc pl
    st a
    inc pl
    st a

__cc_mul_dword_loop:
    ; lo(B) >>= 1
    ldi pl, lo(intrin_arg2)
    ldi ph, hi(intrin_arg2)
    ld a
    shr a
    st a
    ldi pl, lo(__cc_mul_dword_added)
    ldi ph, hi(__cc_mul_dword_added)
    jnc ; no need to add
    ; R += A
    ldi pl, lo(intrin_arg1)
    ldi ph, hi(intrin_arg1)
    ld a
    ldi pl, lo(intrin_result)
    ld b
    add b, a
    st b
    ldi pl, lo(intrin_arg1 + 1)
    ld a
    ldi pl, lo(intrin_result + 1)
    ld b
    adc b, a
    st b
    ldi pl, lo(intrin_arg1 + 2)
    ld a
    ldi pl, lo(intrin_result + 2)
    ld b
    adc b, a
    st b
    ldi pl, lo(intrin_arg1 + 3)
    ld a
    ldi pl, lo(intrin_result + 3)
    ld b
    adc b, a
    st b
__cc_mul_dword_added:
    ; B[1] >>= 1
    ldi pl, lo(intrin_arg2 + 1)
    ldi ph, hi(intrin_arg2)
    ld b
    shr b
    st b
    exp b
    ldi a, 0x80
    and a, b
    ; B[0] |= c ? 0x80 : 0
    dec pl
    ld b
    or a, b
    st a

    ; B[2] >>= 1
    ldi pl, lo(intrin_arg2 + 2)
    ldi ph, hi(intrin_arg2)
    ld b
    shr b
    st b
    exp b
    ldi a, 0x80
    and a, b
    ; B[1] |= c ? 0x80 : 0
    dec pl
    ld b
    or a, b
    st a

    ; B[3] >>= 1
    ldi pl, lo(intrin_arg2 + 3)
    ldi ph, hi(intrin_arg2)
    ld b
    shr b
    st b
    exp b
    ldi a, 0x80
    and a, b
    ; B[2] |= c ? 0x80 : 0
    dec pl
    ld b
    or a, b
    st a

    ; A <<= 1
    ldi pl, lo(intrin_arg1)
    ld a
    shl a
    st a
    mov a, 0
    adc a, 0

    inc pl
    ld pl
    shl pl
    exp b
    or a, pl
    ldi pl, lo(intrin_arg1 + 1)
    st a
    ldi a, 1
    and a, b

    inc pl
    ld pl
    shl pl
    exp b
    or a, pl
    ldi pl, lo(intrin_arg1 + 2)
    st a
    ldi a, 1
    and a, b

    inc pl
    ld b
    shl b
    or a, b
    st a

    ; A | B == 0?
    ; a = A[3]
    dec pl
    ld b
    or a, b
    dec pl
    ld b
    or a, b
    dec pl
    ld b
    or a, b
    ldi pl, lo(intrin_arg2)
    ld b
    or a, b
    inc pl
    ld b
    or a, b
    inc pl
    ld b
    or a, b
    inc pl
    ld b
    or a, b
    ldi pl, lo(__cc_mul_dword_loop)
    ldi ph, hi(__cc_mul_dword_loop)
    jnz ; A | B != 0

    ldi pl, lo(exit)
    ldi ph, hi(exit)
    jmp

    .section text.__cc_div_word
__cc_div_word:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(wrapper_ret)
    ldi ph, hi(wrapper_ret)
    st b
    inc pl
    st a

    ldi pl, lo(intrin_arg1)
    ldi ph, hi(intrin_arg1)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(numerator)
    ldi ph, hi(numerator)
    st  a
    inc pl
    st  b

    ldi pl, lo(intrin_arg2)
    ldi ph, hi(intrin_arg2)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(denominator)
    ldi ph, hi(denominator)
    st  a
    inc pl
    st  b

    ldi pl, lo(divide_word_signed)
    ldi ph, hi(divide_word_signed)
    jmp

    ldi pl, lo(quotient)
    ldi ph, hi(quotient)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(intrin_result)
    ldi ph, hi(intrin_result)
    st  a
    inc pl
    st  b

    ldi pl, lo(wrapper_ret)
    ldi ph, hi(wrapper_ret)
    ld  a
    inc pl
    ld  ph
    mov pl, a
    jmp

    .section text.__cc_mod_word
__cc_mod_word:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(wrapper_ret)
    ldi ph, hi(wrapper_ret)
    st b
    inc pl
    st a

    ldi pl, lo(intrin_arg1)
    ldi ph, hi(intrin_arg1)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(numerator)
    ldi ph, hi(numerator)
    st  a
    inc pl
    st  b

    ldi pl, lo(intrin_arg2)
    ldi ph, hi(intrin_arg2)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(denominator)
    ldi ph, hi(denominator)
    st  a
    inc pl
    st  b

    ldi pl, lo(divide_word_signed)
    ldi ph, hi(divide_word_signed)
    jmp

    ldi pl, lo(remainder)
    ldi ph, hi(remainder)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(intrin_result)
    ldi ph, hi(intrin_result)
    st  a
    inc pl
    st  b

    ldi pl, lo(wrapper_ret)
    ldi ph, hi(wrapper_ret)
    ld  a
    inc pl
    ld  ph
    mov pl, a
    jmp


; A / B
; intrin_arg1 / intrin_arg2 -> intrin_result, remainder
    .section text.__cc_div_word
divide_word_signed:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(int_ret)
    ldi ph, hi(int_ret)
    st b
    inc pl
    st a

    ; tmp & 1 = numerator was negative
    ; tmp & 2 = denominator was negative
    ; tmp := 0
    ldi pl, lo(tmp)
    ldi ph, hi(tmp)
    mov a, 0
    st a

    ; test D
    ldi pl, lo(denominator + 1)
    ld a
    add a, 0
    ldi pl, lo(__cc_div_word_neg_denom)
    ldi ph, hi(__cc_div_word_neg_denom)
    js ; D < 0

__cc_div_word_test_sec:
    ; D >= 0
    ; test N
    ldi pl, lo(numerator + 1)
    ldi ph, hi(numerator)
    ld a
    add a, 0
    ldi pl, lo(__cc_div_word_neg_nom)
    ldi ph, hi(__cc_div_word_neg_nom)
    js ; N < 0

__cc_div_word_positive:
    ; D >= 0
    ; N >= 0
    ; actually divide
    ldi pl, lo(divide_word)
    ldi ph, hi(divide_word)
    jmp

    ldi pl, lo(tmp)
    ldi ph, hi(tmp)
    ld a
    shr a
    st a
    ldi pl, lo(__cc_div_word_result_num_positive)
    ldi ph, hi(__cc_div_word_result_num_positive)
    jnc

    ; numerator was negative

    ; R = -R
    ldi pl, lo(remainder)
    ldi ph, hi(remainder)
    ld  b
    inc pl
    ld  a
    not a
    not b
    inc b
    adc a, 0
    st  a
    dec pl
    st  b

    ldi pl, lo(tmp)
    ldi ph, hi(tmp)
    ld a
    shr a
    ldi pl, lo(__cc_div_negate_quotient)
    ldi ph, hi(__cc_div_negate_quotient)
    jnc ; num -, den +

    ; num -, den -
    ldi pl, lo(exit)
    ldi ph, hi(exit)
    jmp

__cc_div_word_result_num_positive:
    ldi pl, lo(tmp)
    ldi ph, hi(tmp)
    ld a
    shr a
    ldi pl, lo(exit)
    ldi ph, hi(exit)
    jnc ; num +, den +, nothing to negate
    ; num +, den -
__cc_div_negate_quotient:
    ; Q = -Q
    ldi pl, lo(quotient)
    ldi ph, hi(quotient)
    ld  b
    inc pl
    ld  a
    not a
    not b
    inc b
    adc a, 0
    st  a
    dec pl
    st  b

    ldi pl, lo(exit)
    ldi ph, hi(exit)
    jmp

__cc_div_word_neg_denom:
    ; D = -D
    ldi ph, hi(denominator)
    ldi pl, lo(denominator)
    ld b
    inc pl
    ld a
    not a
    not b
    inc b
    adc a, 0
    st a
    dec pl
    st b

    ldi pl, lo(tmp)
    ldi a, 0x02 ; negative denominator
    st a

    ldi pl, lo(__cc_div_word_test_sec)
    ldi ph, hi(__cc_div_word_test_sec)
    jmp

__cc_div_word_neg_nom:
    ; N = -N
    ldi ph, hi(numerator)
    ldi pl, lo(numerator)
    ld b
    inc pl
    ld a
    not a
    not b
    inc b
    adc a, 0
    st a
    dec pl
    st b

    ldi pl, lo(tmp)
    ld a
    ldi b, 0x01 ; negative numerator
    or a, b
    st a

    ldi pl, lo(__cc_div_word_positive)
    ldi ph, hi(__cc_div_word_positive)
    jmp

    .section text.__cc_udiv_word
__cc_udiv_word:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(wrapper_ret)
    ldi ph, hi(wrapper_ret)
    st b
    inc pl
    st a

    ldi pl, lo(intrin_arg1)
    ldi ph, hi(intrin_arg1)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(numerator)
    ldi ph, hi(numerator)
    st  a
    inc pl
    st  b

    ldi pl, lo(intrin_arg2)
    ldi ph, hi(intrin_arg2)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(denominator)
    ldi ph, hi(denominator)
    st  a
    inc pl
    st  b

    ldi pl, lo(divide_word)
    ldi ph, hi(divide_word)
    jmp

    ldi pl, lo(quotient)
    ldi ph, hi(quotient)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(intrin_result)
    ldi ph, hi(intrin_result)
    st  a
    inc pl
    st  b

    ldi pl, lo(wrapper_ret)
    ldi ph, hi(wrapper_ret)
    ld  a
    inc pl
    ld  ph
    mov pl, a
    jmp

    .section text.__cc_umod_word
__cc_umod_word:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(wrapper_ret)
    ldi ph, hi(wrapper_ret)
    st b
    inc pl
    st a

    ldi pl, lo(intrin_arg1)
    ldi ph, hi(intrin_arg1)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(numerator)
    ldi ph, hi(numerator)
    st  a
    inc pl
    st  b

    ldi pl, lo(intrin_arg2)
    ldi ph, hi(intrin_arg2)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(denominator)
    ldi ph, hi(denominator)
    st  a
    inc pl
    st  b

    ldi pl, lo(divide_word)
    ldi ph, hi(divide_word)
    jmp

    ldi pl, lo(remainder)
    ldi ph, hi(remainder)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(intrin_result)
    ldi ph, hi(intrin_result)
    st  a
    inc pl
    st  b

    ldi pl, lo(wrapper_ret)
    ldi ph, hi(wrapper_ret)
    ld  a
    inc pl
    ld  ph
    mov pl, a
    jmp

    .section text.divide_word
    ; unsigned N / D
    ; preserve D
divide_word:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(div_ret)
    ldi ph, hi(div_ret)
    st b
    inc pl
    st a

    ; D == 0?
    ldi ph, hi(denominator)
    ldi pl, lo(denominator)
    ld a
    inc pl
    ld b
    or a, b
    ldi pl, lo(__cc_div_zero_trap)
    ldi ph, hi(__cc_div_zero_trap)
    jz ; D == 0

    ; Q := 0
    ldi ph, hi(quotient)
    ldi pl, lo(quotient)
    mov a, 0
    st a
    inc pl
    st a
    ; R := 0
    ldi pl, lo(remainder)
    st a
    inc pl
    st a

    ; first half:

    ; qbit := 0x80
    ldi pl, lo(qbit)
    ldi a, 0x80
    st a

divide_word_loop_1:
    ; R := (R << 1) | msb(N)
    ; N <<= 1
    ldi ph, hi(remainder)
    ldi pl, lo(remainder)
    ld a
    shl a
    ldi pl, lo(numerator + 1)
    ld b
    shl b
    st b
    adc a, 0
    ldi pl, lo(remainder)
    st a

    ; R >= D?
    ; hi(R) is still 0
    ldi pl, lo(denominator + 1)
    ld a
    add a, 0
    ldi pl, lo(divide_word_loop_1_r_lt_d)
    ldi ph, hi(divide_word_loop_1_r_lt_d)
    jnz ; hi(D) != 0

    ldi ph, hi(remainder)
    ldi pl, lo(remainder)
    ld a
    ldi pl, lo(denominator)
    ld b
    sub a, b ; lo(R) - lo(D)
    ldi pl, lo(divide_word_loop_1_r_lt_d)
    ldi ph, hi(divide_word_loop_1_r_lt_d)
    jc ; lo(R) < lo(D)

    ; R >= D
    ; R -= D
    ; hi(R) == 0, hi(D) == 0, overflow isn't possible
    ldi ph, hi(denominator)
    ldi pl, lo(denominator)
    ld a
    ldi pl, lo(remainder)
    ld b
    sub b, a
    st b
    ; Q |= qbit
    ldi pl, lo(qbit)
    ld a
    ldi pl, lo(quotient + 1)
    ld b
    or b, a
    st b

divide_word_loop_1_r_lt_d:
    ; qbit >>= 1
    ldi ph, hi(qbit)
    ldi pl, lo(qbit)
    ld a
    shr a
    st a
    ldi ph, hi(divide_word_loop_1)
    ldi pl, lo(divide_word_loop_1)
    jnc

    ; second half:

    ; qbit := 0x80
    ldi ph, hi(qbit)
    ldi pl, lo(qbit)
    ldi a, 0x80
    st a

divide_word_loop_2:
    ; R := (R << 1) | msb(N)
    ; N <<= 1
    ldi ph, hi(remainder)
    ldi pl, lo(remainder)
    ld b
    inc pl
    ld a
    shl a
    shl b
    adc a, 0
    st a
    mov a, b
    ldi pl, lo(numerator)
    ld b
    shl b
    st b
    adc a, 0
    ldi pl, lo(remainder)
    st a

    ; R >= D?
    ldi pl, lo(denominator + 1)
    ld a
    ldi pl, lo(remainder + 1)
    ld b
    sub b, a
    ldi pl, lo(divide_word_loop_2_r_lt_d)
    ldi ph, hi(divide_word_loop_2_r_lt_d)
    jc ; hi(R) < hi(D)
    ldi pl, lo(divide_word_loop_2_r_gt_d)
    ldi ph, hi(divide_word_loop_2_r_gt_d)
    jnz

    ; hi(R) == hi(D)
    ldi ph, hi(remainder)
    ldi pl, lo(remainder)
    ld a
    ldi pl, lo(denominator)
    ld b
    sub a, b ; lo(R) - lo(D)
    ldi pl, lo(divide_word_loop_2_r_lt_d)
    ldi ph, hi(divide_word_loop_2_r_lt_d)
    jc ; lo(R) < lo(D)

divide_word_loop_2_r_gt_d:
    ; R >= D
    ; R -= D
    ldi ph, hi(denominator)
    ldi pl, lo(denominator)
    ld a
    ldi pl, lo(remainder)
    ld b
    sub b, a
    st b
    ldi pl, lo(denominator + 1)
    ld a
    ldi pl, lo(remainder + 1)
    ld b
    sbb b, a
    st b
    ; Q |= qbit
    ldi pl, lo(qbit)
    ld a
    ldi pl, lo(quotient)
    ld b
    or b, a
    st b

divide_word_loop_2_r_lt_d:
    ; qbit >>= 1
    ldi ph, hi(qbit)
    ldi pl, lo(qbit)
    ld a
    shr a
    st a
    ldi ph, hi(divide_word_loop_2)
    ldi pl, lo(divide_word_loop_2)
    jnc

    ldi pl, lo(div_ret)
    ldi ph, hi(div_ret)
    ld a
    inc pl
    ld ph
    mov pl, a
    jmp

    .section text.__cc_div_byte
__cc_div_byte:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(wrapper_ret)
    ldi ph, hi(wrapper_ret)
    st b
    inc pl
    st a

    ldi pl, lo(intrin_arg1)
    ldi ph, hi(intrin_arg1)
    ld  a
    ldi pl, lo(intrin_arg2)
    ld  b
    ldi pl, lo(numerator)
    ldi ph, hi(numerator)
    st  a
    ldi pl, lo(denominator)
    st  b

    ldi pl, lo(divide_byte_signed)
    ldi ph, hi(divide_byte_signed)
    jmp

    ldi pl, lo(quotient)
    ldi ph, hi(quotient)
    ld  a
    ldi pl, lo(intrin_result)
    ldi ph, hi(intrin_result)
    st  a

    ldi pl, lo(wrapper_ret)
    ldi ph, hi(wrapper_ret)
    ld  a
    inc pl
    ld  ph
    mov pl, a
    jmp

    .section text.__cc_mod_byte
__cc_mod_byte:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(wrapper_ret)
    ldi ph, hi(wrapper_ret)
    st b
    inc pl
    st a

    ldi pl, lo(intrin_arg1)
    ldi ph, hi(intrin_arg1)
    ld  a
    ldi pl, lo(intrin_arg2)
    ld  b
    ldi pl, lo(numerator)
    ldi ph, hi(numerator)
    st  a
    ldi pl, lo(denominator)
    st  b

    ldi pl, lo(divide_byte_signed)
    ldi ph, hi(divide_byte_signed)
    jmp

    ldi pl, lo(remainder)
    ldi ph, hi(remainder)
    ld  a
    ldi pl, lo(intrin_result)
    ldi ph, hi(intrin_result)
    st  a

    ldi pl, lo(wrapper_ret)
    ldi ph, hi(wrapper_ret)
    ld  a
    inc pl
    ld  ph
    mov pl, a
    jmp

    ; A / B
    ; intrin_arg1 / intrin_arg2 -> __cc_r_quotient, __cc_r_remainder
    .section text.divide_byte_signed
divide_byte_signed:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(int_ret)
    ldi ph, hi(int_ret)
    st b
    inc pl
    st a

    ; tmp & 1 = numerator was negative
    ; tmp & 2 = denominator was negative
    ; tmp := 0
    ldi pl, lo(tmp)
    ldi ph, hi(tmp)
    mov a, 0
    st a

    ; test D
    ldi pl, lo(denominator)
    ld a
    add a, 0
    ldi pl, lo(__cc_div_byte_neg_denom)
    ldi ph, hi(__cc_div_byte_neg_denom)
    js ; D < 0

__cc_div_byte_test_sec:
    ; D >= 0
    ; test N
    ldi pl, lo(numerator)
    ldi ph, hi(numerator)
    ld a
    add a, 0
    ldi pl, lo(__cc_div_byte_neg_nom)
    ldi ph, hi(__cc_div_byte_neg_nom)
    js ; N < 0

__cc_div_byte_positive:
    ; D >= 0
    ; N >= 0
    ; actually divide
    ldi pl, lo(divide_byte)
    ldi ph, hi(divide_byte)
    jmp

    ldi pl, lo(tmp)
    ldi ph, hi(tmp)
    ld a
    shr a
    st a
    ldi pl, lo(__cc_div_byte_result_nom_positive)
    ldi ph, hi(__cc_div_byte_result_nom_positive)
    jnc

    ; numerator was negative

    ; Q = ~Q
    ldi pl, lo(quotient)
    ldi ph, hi(quotient)
    ld a
    not a
    st a

    ; R == 0?
    ldi pl, lo(remainder)
    ld a
    add a, 0
    ldi pl, lo(__cc_div_byte_d_minus_r)
    ldi ph, hi(__cc_div_byte_d_minus_r)
    jnz

    ; "return -Q, 0"
    ; R == 0
    ; Q += 1 - finish the negation
    ldi pl, lo(quotient)
    ldi ph, hi(quotient)
    ld b
    inc b
    st b

    ldi pl, lo(__cc_div_byte_result_nom_positive)
    ldi ph, hi(__cc_div_byte_result_nom_positive)
    jmp

__cc_div_byte_d_minus_r:
    ; "return -Q - 1, D - R"
    ; Q is already that
    ; R := D - R
    ldi ph, hi(denominator)
    ldi pl, lo(denominator)
    ld b
    ldi pl, lo(remainder)
    ld a
    sub b, a
    st b

__cc_div_byte_result_nom_positive:
    ldi pl, lo(tmp)
    ldi ph, hi(tmp)
    ld a
    shr a
    ldi pl, lo(exit)
    ldi ph, hi(exit)
    jnc
    ; denominator was negative
    ; "return -Q, R"
    ; Q := -Q
    ldi pl, lo(quotient)
    ldi ph, hi(quotient)
    ld b
    neg b
    st b

    ldi pl, lo(exit)
    ldi ph, hi(exit)
    jmp

__cc_div_byte_neg_denom:
    ; D = -D
    ldi ph, hi(denominator)
    ldi pl, lo(denominator)
    ld b
    neg b
    st b

    ldi pl, lo(tmp)
    ldi a, 0x02 ; negative denominator
    st a

    ldi pl, lo(__cc_div_byte_test_sec)
    ldi ph, hi(__cc_div_byte_test_sec)
    jmp

__cc_div_byte_neg_nom:
    ; N = -N
    ldi ph, hi(numerator)
    ldi pl, lo(numerator)
    ld b
    neg b
    st b

    ldi pl, lo(tmp)
    ld a
    ldi b, 0x01 ; negative numerator
    or a, b
    st a

    ldi pl, lo(__cc_div_byte_positive)
    ldi ph, hi(__cc_div_byte_positive)
    jmp


    .section text.__cc_udiv_byte
__cc_udiv_byte:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(wrapper_ret)
    ldi ph, hi(wrapper_ret)
    st b
    inc pl
    st a

    ldi pl, lo(intrin_arg1)
    ldi ph, hi(intrin_arg1)
    ld  a
    ldi pl, lo(intrin_arg2)
    ld  b
    ldi pl, lo(numerator)
    ldi ph, hi(numerator)
    st  a
    ldi pl, lo(denominator)
    st  b

    ldi pl, lo(divide_byte)
    ldi ph, hi(divide_byte)
    jmp

    ldi pl, lo(quotient)
    ldi ph, hi(quotient)
    ld  a
    ldi pl, lo(intrin_result)
    ldi ph, hi(intrin_result)
    st  a

    ldi pl, lo(wrapper_ret)
    ldi ph, hi(wrapper_ret)
    ld  a
    inc pl
    ld  ph
    mov pl, a
    jmp

    .section text.__cc_umod_byte
__cc_umod_byte:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(wrapper_ret)
    ldi ph, hi(wrapper_ret)
    st b
    inc pl
    st a

    ldi pl, lo(intrin_arg1)
    ldi ph, hi(intrin_arg1)
    ld  a
    ldi pl, lo(intrin_arg2)
    ld  b
    ldi pl, lo(numerator)
    ldi ph, hi(numerator)
    st  a
    ldi pl, lo(denominator)
    st  b

    ldi pl, lo(divide_byte)
    ldi ph, hi(divide_byte)
    jmp

    ldi pl, lo(remainder)
    ldi ph, hi(remainder)
    ld  a
    ldi pl, lo(intrin_result)
    ldi ph, hi(intrin_result)
    st  a

    ldi pl, lo(wrapper_ret)
    ldi ph, hi(wrapper_ret)
    ld  a
    inc pl
    ld  ph
    mov pl, a
    jmp


    ; A (=N) / B (=D)
    ; preserve B
    .section text.__cc_divide_byte
divide_byte:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(div_ret)
    ldi ph, hi(div_ret)
    st b
    inc pl
    st a

    ; D == 0?
    ldi ph, hi(denominator)
    ldi pl, lo(denominator)
    ld a
    add a, 0
    ldi pl, lo(__cc_div_zero_trap)
    ldi ph, hi(__cc_div_zero_trap)
    jz ; D == 0

    ; Q := 0
    ldi ph, hi(quotient)
    ldi pl, lo(quotient)
    mov a, 0
    st a
    ; R := 0
    ldi pl, lo(remainder)
    st a

    ; qbit := 0x80
    ldi pl, lo(qbit)
    ldi a, 0x80
    st a

divide_byte_loop:
    ; R := (R << 1) | msb(N)
    ; N <<= 1
    ldi ph, hi(remainder)
    ldi pl, lo(remainder)
    ld a
    shl a
    ldi pl, lo(numerator)
    ld b
    shl b
    st b
    adc a, 0
    ldi pl, lo(remainder)
    st a

    ; R >= D?
    ldi pl, lo(denominator)
    ld b
    sub a, b
    ldi pl, lo(divide_byte_loop_r_lt_d)
    ldi ph, hi(divide_byte_loop_r_lt_d)
    jc ; R < D

    ; R >= D
    ; R -= D
    ldi ph, hi(remainder)
    ldi pl, lo(remainder)
    st a ; a is already R - D after comparison

    ; Q |= qbit
    ldi pl, lo(qbit)
    ld a
    ldi pl, lo(quotient)
    ld b
    or b, a
    st b

divide_byte_loop_r_lt_d:
    ; qbit >>= 1
    ldi ph, hi(qbit)
    ldi pl, lo(qbit)
    ld a
    shr a
    st a
    ldi ph, hi(divide_byte_loop)
    ldi pl, lo(divide_byte_loop)
    jnc

    ldi pl, lo(div_ret)
    ldi ph, hi(div_ret)
    ld a
    inc pl
    ld ph
    mov pl, a
    jmp

__cc_div_zero_trap:
    ldi pl, lo(__cc_div_zero_trap)
    ldi ph, hi(__cc_div_zero_trap)
    jmp

    .section bss.natrix_runtime
    .align 64 ; all internal data have the same hi byte
__cc_ret: res 8
    res 3 ; zeroes before shift_lhs
shift_lhs: res 4
    res 3 ; zeroes after shift_lhs

numerator: res 4
denominator: res 4
int_ret: res 2
tmp: res 2
bit_shift: res 1
byte_shift: res 1
quotient: res 2
remainder: res 2
div_ret: res 2
wrapper_ret: res 2
qbit: res 2
memcpy_ret:
memcpy_arg2: res 2
