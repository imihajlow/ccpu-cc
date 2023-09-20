    .export __cc_udiv_dword
    .export __cc_umod_dword
    .export __cc_div_dword
    .export __cc_mod_dword

    .global __cc_ret
    .global __cc_div_zero_trap

    ; arguments for intrinsic calls
    .const intrin_arg1 = 0xc800 - 8 * 4
    .const intrin_arg2 = 0xc800 - 8 * 5
    .const intrin_arg3 = 0xc800 - 8 * 6
    .const intrin_result = 0xc800 - 8 * 3

    .const numerator = 0xc800 - 8 * 4
    .const denominator = 0xc800 - 8 * 5

    .section text.__cc_udiv_dword
__cc_udiv_dword:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(outer_ret)
    ldi ph, hi(outer_ret)
    st b
    inc pl
    st a

    ldi pl, lo(divide_dword)
    ldi ph, hi(divide_dword)
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
    ldi pl, lo(quotient + 2)
    ldi ph, hi(quotient + 2)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(intrin_result + 2)
    ldi ph, hi(intrin_result + 2)
    st  a
    inc pl
    st  b

    ldi ph, hi(outer_ret)
    ldi pl, lo(outer_ret)
    ld  a
    inc pl
    ld  ph
    mov pl, a
    jmp

    .section text.__cc_umod_dword
__cc_umod_dword:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(outer_ret)
    ldi ph, hi(outer_ret)
    st b
    inc pl
    st a

    ldi pl, lo(divide_dword)
    ldi ph, hi(divide_dword)
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
    ldi pl, lo(remainder + 2)
    ldi ph, hi(remainder + 2)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(intrin_result + 2)
    ldi ph, hi(intrin_result + 2)
    st  a
    inc pl
    st  b

    ldi ph, hi(outer_ret)
    ldi pl, lo(outer_ret)
    ld  a
    inc pl
    ld  ph
    mov pl, a
    jmp

    .section text.__cc_div_dword
__cc_div_dword:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(outer_ret)
    ldi ph, hi(outer_ret)
    st b
    inc pl
    st a

    ; Division result should be negated if N and D have different signs

    ldi ph, hi(numerator)
    ldi pl, lo(numerator + 3)
    ld b
    mov a, 0
    shl b
    adc a, 0
    ldi pl, lo(denominator + 3)
    ld b
    shl b
    adc a, 0
    ldi ph, hi(negate)
    ldi pl, lo(negate)
    st  a ; a & 1 indicates that the result must be negated

    ldi ph, hi(numerator)
    ldi pl, lo(numerator + 3)
    ld  b
    shl b
    ldi ph, hi(__cc_div_dword_num_positive)
    ldi pl, lo(__cc_div_dword_num_positive)
    jnc

    ; numerator is negative - negate it
    ldi ph, hi(numerator)
    ldi pl, lo(numerator + 3)
    ld  a
    not a
    st  a
    dec pl
    ld  a
    not a
    st  a
    dec pl
    ld  a
    not a
    st  a
    dec pl
    ld  a
    not a
    inc a
    st  a
    ldi pl, lo(numerator + 1)
    ld  a
    adc a, 0
    st  a
    ldi pl, lo(numerator + 2)
    ld  a
    adc a, 0
    st  a
    ldi pl, lo(numerator + 3)
    ld  a
    adc a, 0
    st  a

__cc_div_dword_num_positive:
    ldi ph, hi(denominator)
    ldi pl, lo(denominator + 3)
    ld  b
    shl b
    ldi ph, hi(__cc_div_dword_den_positive)
    ldi pl, lo(__cc_div_dword_den_positive)
    jnc

    ; denominator is negative - negate it
    ldi ph, hi(denominator)
    ldi pl, lo(denominator + 3)
    ld  a
    not a
    st  a
    dec pl
    ld  a
    not a
    st  a
    dec pl
    ld  a
    not a
    st  a
    dec pl
    ld  a
    not a
    inc a
    st  a
    ldi pl, lo(denominator + 1)
    ld  a
    adc a, 0
    st  a
    ldi pl, lo(denominator + 2)
    ld  a
    adc a, 0
    st  a
    ldi pl, lo(denominator + 3)
    ld  a
    adc a, 0
    st  a

__cc_div_dword_den_positive:
    ldi pl, lo(divide_dword)
    ldi ph, hi(divide_dword)
    jmp

    ldi ph, hi(negate)
    ldi pl, lo(negate)
    ld  a
    shr a
    ldi ph, hi(__cc_div_dword_neg_result)
    ldi pl, lo(__cc_div_dword_neg_result)
    jc

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
    ldi pl, lo(quotient + 2)
    ldi ph, hi(quotient + 2)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(intrin_result + 2)
    ldi ph, hi(intrin_result + 2)
    st  a
    inc pl
    st  b

    ldi ph, hi(outer_ret)
    ldi pl, lo(outer_ret)
    ld  a
    inc pl
    ld  ph
    mov pl, a
    jmp

__cc_div_dword_neg_result:
    ldi pl, lo(quotient + 2)
    ldi ph, hi(quotient + 2)
    ld  a
    not a
    inc pl
    ld  b
    not b
    ldi pl, lo(intrin_result + 2)
    ldi ph, hi(intrin_result + 2)
    st  a
    inc pl
    st  b
    ldi pl, lo(quotient)
    ldi ph, hi(quotient)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(intrin_result + 1)
    ldi ph, hi(intrin_result)
    not b
    st  b
    dec pl
    not a
    inc a
    st  a
    ldi pl, lo(intrin_result + 1)
    ld  a
    adc a, 0
    st  a
    ldi pl, lo(intrin_result + 2)
    ld  a
    adc a, 0
    st  a
    ldi pl, lo(intrin_result + 3)
    ld  a
    adc a, 0
    st  a

    ldi ph, hi(outer_ret)
    ldi pl, lo(outer_ret)
    ld  a
    inc pl
    ld  ph
    mov pl, a
    jmp

    .section text.__cc_mod_dword
__cc_mod_dword:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(outer_ret)
    ldi ph, hi(outer_ret)
    st b
    inc pl
    st a

    ; Remainder result should be negated if N is negative

    ldi ph, hi(numerator)
    ldi pl, lo(numerator + 3)
    ld b
    mov a, 0
    shl b
    adc a, 0
    ldi ph, hi(negate)
    ldi pl, lo(negate)
    st  a ; a & 1 indicates that the result must be negated

    ldi ph, hi(numerator)
    ldi pl, lo(numerator + 3)
    ld  b
    shl b
    ldi ph, hi(__cc_mod_dword_num_positive)
    ldi pl, lo(__cc_mod_dword_num_positive)
    jnc

    ; numerator is negative - negate it
    ldi ph, hi(numerator)
    ldi pl, lo(numerator + 3)
    ld  a
    not a
    st  a
    dec pl
    ld  a
    not a
    st  a
    dec pl
    ld  a
    not a
    st  a
    dec pl
    ld  a
    not a
    inc a
    st  a
    ldi pl, lo(numerator + 1)
    ld  a
    adc a, 0
    st  a
    ldi pl, lo(numerator + 2)
    ld  a
    adc a, 0
    st  a
    ldi pl, lo(numerator + 3)
    ld  a
    adc a, 0
    st  a

__cc_mod_dword_num_positive:
    ldi ph, hi(denominator)
    ldi pl, lo(denominator + 3)
    ld  b
    shl b
    ldi ph, hi(__cc_mod_dword_den_positive)
    ldi pl, lo(__cc_mod_dword_den_positive)
    jnc

    ; denominator is negative - negate it
    ldi ph, hi(denominator)
    ldi pl, lo(denominator + 3)
    ld  a
    not a
    st  a
    dec pl
    ld  a
    not a
    st  a
    dec pl
    ld  a
    not a
    st  a
    dec pl
    ld  a
    not a
    inc a
    st  a
    ldi pl, lo(denominator + 1)
    ld  a
    adc a, 0
    st  a
    ldi pl, lo(denominator + 2)
    ld  a
    adc a, 0
    st  a
    ldi pl, lo(denominator + 3)
    ld  a
    adc a, 0
    st  a

__cc_mod_dword_den_positive:
    ldi pl, lo(divide_dword)
    ldi ph, hi(divide_dword)
    jmp

    ldi ph, hi(negate)
    ldi pl, lo(negate)
    ld  a
    shr a
    ldi ph, hi(__cc_mod_dword_neg_result)
    ldi pl, lo(__cc_mod_dword_neg_result)
    jc

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
    ldi pl, lo(remainder + 2)
    ldi ph, hi(remainder + 2)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(intrin_result + 2)
    ldi ph, hi(intrin_result + 2)
    st  a
    inc pl
    st  b

    ldi ph, hi(outer_ret)
    ldi pl, lo(outer_ret)
    ld  a
    inc pl
    ld  ph
    mov pl, a
    jmp

__cc_mod_dword_neg_result:
    ldi pl, lo(remainder + 2)
    ldi ph, hi(remainder + 2)
    ld  a
    not a
    inc pl
    ld  b
    not b
    ldi pl, lo(intrin_result + 2)
    ldi ph, hi(intrin_result + 2)
    st  a
    inc pl
    st  b
    ldi pl, lo(remainder)
    ldi ph, hi(remainder)
    ld  a
    inc pl
    ld  b
    ldi pl, lo(intrin_result + 1)
    ldi ph, hi(intrin_result)
    not b
    st  b
    dec pl
    not a
    inc a
    st  a
    ldi pl, lo(intrin_result + 1)
    ld  a
    adc a, 0
    st  a
    ldi pl, lo(intrin_result + 2)
    ld  a
    adc a, 0
    st  a
    ldi pl, lo(intrin_result + 3)
    ld  a
    adc a, 0
    st  a

    ldi ph, hi(outer_ret)
    ldi pl, lo(outer_ret)
    ld  a
    inc pl
    ld  ph
    mov pl, a
    jmp

    .section text.__cc_divide_dword
divide_dword:
    mov a, pl
    mov b, a
    mov a, ph
    ldi pl, lo(inner_ret)
    ldi ph, hi(inner_ret)
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
    inc pl
    ld b
    or a, b
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
    st  a
    inc pl
    st  a
    inc pl
    st  a
    inc pl
    st  a
    ; R := 0
    inc pl
    st  a
    inc pl
    st  a
    inc pl
    st  a
    inc pl
    st  a

    ; PART 1: bits 31 to 24 of quotient
    ; qbit := 0x80
    ldi pl, lo(qbit)
    ldi a, 0x80
    st a

divide_dword_loop_1:
    ; R := (R << 1) | msb(N)
    ; N <<= 1
    ldi ph, hi(remainder)
    ldi pl, lo(remainder)
    ld a
    shl a
    ldi ph, hi(numerator + 3)
    ldi pl, lo(numerator + 3)
    ld b
    shl b
    st b
    adc a, 0
    ldi ph, hi(remainder)
    ldi pl, lo(remainder)
    st a

    ; R >= D?
    ; R[3..1] is still 0
    ldi ph, hi(denominator + 1)
    ldi pl, lo(denominator + 1)
    ld a
    inc pl
    ld b
    or a, b
    inc pl
    ld b
    or a, b
    ldi pl, lo(divide_dword_loop_1_r_lt_d)
    ldi ph, hi(divide_dword_loop_1_r_lt_d)
    jnz ; hi(D) != 0

    ldi ph, hi(remainder)
    ldi pl, lo(remainder)
    ld a
    ldi ph, hi(denominator)
    ldi pl, lo(denominator)
    ld b
    sub a, b ; lo(R) - lo(D)
    ldi pl, lo(divide_dword_loop_1_r_lt_d)
    ldi ph, hi(divide_dword_loop_1_r_lt_d)
    jc ; lo(R) < lo(D)

    ; R >= D
    ; R -= D
    ; hi(R) == 0, hi(D) == 0, overflow isn't possible
    ldi ph, hi(denominator)
    ldi pl, lo(denominator)
    ld a
    ldi ph, hi(remainder)
    ldi pl, lo(remainder)
    ld b
    sub b, a
    st b
    ; Q |= qbit
    ldi pl, lo(qbit)
    ld a
    ldi ph, hi(quotient + 3)
    ldi pl, lo(quotient + 3)
    ld b
    or b, a
    st b

divide_dword_loop_1_r_lt_d:
    ; qbit >>= 1
    ldi ph, hi(qbit)
    ldi pl, lo(qbit)
    ld a
    shr a
    st a
    ldi ph, hi(divide_dword_loop_1)
    ldi pl, lo(divide_dword_loop_1)
    jnc


    ; PART 2: bits 23 to 16 of quotient
    ; qbit := 0x80
    ldi ph, hi(qbit)
    ldi pl, lo(qbit)
    ldi a, 0x80
    st a

divide_dword_loop_2:
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
    ldi ph, hi(numerator + 2)
    ldi pl, lo(numerator + 2)
    ld b
    shl b
    st b
    adc a, 0
    ldi ph, hi(remainder)
    ldi pl, lo(remainder)
    st a

    ; R >= D?
    ; R[3..2] is still 0
    ldi ph, hi(denominator + 2)
    ldi pl, lo(denominator + 2)
    ld a
    inc pl
    ld b
    or a, b
    ldi pl, lo(divide_dword_loop_2_r_lt_d)
    ldi ph, hi(divide_dword_loop_2_r_lt_d)
    jnz ; D[3..2] != 0

    ldi ph, hi(denominator + 1)
    ldi pl, lo(denominator + 1)
    ld a
    ldi ph, hi(remainder + 1)
    ldi pl, lo(remainder + 1)
    ld b
    sub b, a
    ldi pl, lo(divide_dword_loop_2_r_lt_d)
    ldi ph, hi(divide_dword_loop_2_r_lt_d)
    jc ; R[1] < D[1]
    ldi pl, lo(divide_dword_loop_2_r_gt_d)
    ldi ph, hi(divide_dword_loop_2_r_gt_d)
    jnz ; R[1] != D[1]

    ; R[1] == D[1]
    ldi ph, hi(remainder)
    ldi pl, lo(remainder)
    ld a
    ldi ph, hi(denominator)
    ldi pl, lo(denominator)
    ld b
    sub a, b ; R[0] - D[0]
    ldi pl, lo(divide_dword_loop_2_r_lt_d)
    ldi ph, hi(divide_dword_loop_2_r_lt_d)
    jc ; R[0] < D[0]

divide_dword_loop_2_r_gt_d:
    ; R >= D
    ; R -= D
    ldi ph, hi(denominator)
    ldi pl, lo(denominator)
    ld a
    ldi ph, hi(remainder)
    ldi pl, lo(remainder)
    ld b
    sub b, a
    st b
    ldi ph, hi(denominator + 1)
    ldi pl, lo(denominator + 1)
    ld a
    ldi ph, hi(remainder + 1)
    ldi pl, lo(remainder + 1)
    ld b
    sbb b, a
    st b
    ; Q |= qbit
    ldi pl, lo(qbit)
    ld a
    ldi pl, lo(quotient + 2)
    ld b
    or b, a
    st b

divide_dword_loop_2_r_lt_d:
    ; qbit >>= 1
    ldi ph, hi(qbit)
    ldi pl, lo(qbit)
    ld a
    shr a
    st a
    ldi ph, hi(divide_dword_loop_2)
    ldi pl, lo(divide_dword_loop_2)
    jnc


    ; PART 3: bits 15 to 8 of quotient
    ; qbit := 0x80
    ldi ph, hi(qbit)
    ldi pl, lo(qbit)
    ldi a, 0x80
    st a

divide_dword_loop_3:
    ; R := (R << 1) | msb(N)
    ; N <<= 1
    ldi ph, hi(remainder + 1)
    ldi pl, lo(remainder + 1)
    ld b
    inc pl
    ld a
    shl a
    shl b
    adc a, 0
    st a
    mov a, b
    ldi pl, lo(remainder + 0)
    ld  b
    shl b
    adc a, 0
    inc pl
    st  a
    mov a, b
    ldi ph, hi(numerator + 1)
    ldi pl, lo(numerator + 1)
    ld b
    shl b
    st b
    adc a, 0
    ldi ph, hi(remainder)
    ldi pl, lo(remainder)
    st a

    ; R >= D?
    ; R[3] is still 0
    ldi ph, hi(denominator + 3)
    ldi pl, lo(denominator + 3)
    ld a
    add a, 0
    ldi pl, lo(divide_dword_loop_3_r_lt_d)
    ldi ph, hi(divide_dword_loop_3_r_lt_d)
    jnz ; D[3] != 0

    ldi ph, hi(denominator + 2)
    ldi pl, lo(denominator + 2)
    ld a
    ldi ph, hi(remainder + 2)
    ldi pl, lo(remainder + 2)
    ld b
    sub b, a
    ldi pl, lo(divide_dword_loop_3_r_lt_d)
    ldi ph, hi(divide_dword_loop_3_r_lt_d)
    jc ; R[2] < D[2]
    ldi pl, lo(divide_dword_loop_3_r_gt_d)
    ldi ph, hi(divide_dword_loop_3_r_gt_d)
    jnz ; R[2] != D[2]

    ; R[2] == D[2]
    ldi ph, hi(denominator + 1)
    ldi pl, lo(denominator + 1)
    ld a
    ldi ph, hi(remainder + 1)
    ldi pl, lo(remainder + 1)
    ld b
    sub b, a
    ldi pl, lo(divide_dword_loop_3_r_lt_d)
    ldi ph, hi(divide_dword_loop_3_r_lt_d)
    jc ; R[1] < D[1]
    ldi pl, lo(divide_dword_loop_3_r_gt_d)
    ldi ph, hi(divide_dword_loop_3_r_gt_d)
    jnz ; R[1] != D[1]

    ; R[1] == D[1]
    ldi ph, hi(remainder)
    ldi pl, lo(remainder)
    ld a
    ldi ph, hi(denominator)
    ldi pl, lo(denominator)
    ld b
    sub a, b ; R[0] - D[0]
    ldi pl, lo(divide_dword_loop_3_r_lt_d)
    ldi ph, hi(divide_dword_loop_3_r_lt_d)
    jc ; R[0] < D[0]

divide_dword_loop_3_r_gt_d:
    ; R >= D
    ; R -= D
    ldi ph, hi(denominator)
    ldi pl, lo(denominator)
    ld a
    ldi ph, hi(remainder)
    ldi pl, lo(remainder)
    ld b
    sub b, a
    st b
    ldi ph, hi(denominator + 1)
    ldi pl, lo(denominator + 1)
    ld a
    ldi ph, hi(remainder + 1)
    ldi pl, lo(remainder + 1)
    ld b
    sbb b, a
    st b
    ldi ph, hi(denominator + 2)
    ldi pl, lo(denominator + 2)
    ld a
    ldi ph, hi(remainder + 2)
    ldi pl, lo(remainder + 2)
    ld b
    sbb b, a
    st b
    ; Q |= qbit
    ldi pl, lo(qbit)
    ld a
    ldi pl, lo(quotient + 1)
    ld b
    or b, a
    st b

divide_dword_loop_3_r_lt_d:
    ; qbit >>= 1
    ldi ph, hi(qbit)
    ldi pl, lo(qbit)
    ld a
    shr a
    st a
    ldi ph, hi(divide_dword_loop_3)
    ldi pl, lo(divide_dword_loop_3)
    jnc

    ; PART 4: bits 7 to 0 of quotient
    ; qbit := 0x80
    ldi ph, hi(qbit)
    ldi pl, lo(qbit)
    ldi a, 0x80
    st a

divide_dword_loop_4:
    ; R := (R << 1) | msb(N)
    ; N <<= 1
    ldi ph, hi(remainder + 2)
    ldi pl, lo(remainder + 2)
    ld b
    inc pl
    ld a
    shl a
    shl b
    adc a, 0
    st a
    mov a, b
    ldi pl, lo(remainder + 1)
    ld  b
    shl b
    adc a, 0
    inc pl
    st  a
    mov a, b
    ldi pl, lo(remainder + 0)
    ld  b
    shl b
    adc a, 0
    inc pl
    st  a
    mov a, b
    ldi ph, hi(numerator)
    ldi pl, lo(numerator)
    ld b
    shl b
    st b
    adc a, 0
    ldi ph, hi(remainder)
    ldi pl, lo(remainder)
    st a

    ; R >= D?
    ldi ph, hi(denominator + 3)
    ldi pl, lo(denominator + 3)
    ld a
    ldi ph, hi(remainder + 3)
    ldi pl, lo(remainder + 3)
    ld b
    sub b, a
    ldi pl, lo(divide_dword_loop_4_r_lt_d)
    ldi ph, hi(divide_dword_loop_4_r_lt_d)
    jc ; R[3] < D[3]
    ldi pl, lo(divide_dword_loop_4_r_gt_d)
    ldi ph, hi(divide_dword_loop_4_r_gt_d)
    jnz ; R[3] != D[3]

    ; R[3] == D[3]
    ldi ph, hi(denominator + 2)
    ldi pl, lo(denominator + 2)
    ld a
    ldi ph, hi(remainder + 2)
    ldi pl, lo(remainder + 2)
    ld b
    sub b, a
    ldi pl, lo(divide_dword_loop_4_r_lt_d)
    ldi ph, hi(divide_dword_loop_4_r_lt_d)
    jc ; R[2] < D[2]
    ldi pl, lo(divide_dword_loop_4_r_gt_d)
    ldi ph, hi(divide_dword_loop_4_r_gt_d)
    jnz ; R[2] != D[2]

    ; R[2] == D[2]
    ldi ph, hi(denominator + 1)
    ldi pl, lo(denominator + 1)
    ld a
    ldi ph, hi(remainder + 1)
    ldi pl, lo(remainder + 1)
    ld b
    sub b, a
    ldi pl, lo(divide_dword_loop_4_r_lt_d)
    ldi ph, hi(divide_dword_loop_4_r_lt_d)
    jc ; R[1] < D[1]
    ldi pl, lo(divide_dword_loop_4_r_gt_d)
    ldi ph, hi(divide_dword_loop_4_r_gt_d)
    jnz ; R[1] != D[1]

    ; R[1] == D[1]
    ldi ph, hi(remainder)
    ldi pl, lo(remainder)
    ld a
    ldi ph, hi(denominator)
    ldi pl, lo(denominator)
    ld b
    sub a, b ; R[0] - D[0]
    ldi pl, lo(divide_dword_loop_4_r_lt_d)
    ldi ph, hi(divide_dword_loop_4_r_lt_d)
    jc ; R[0] < D[0]

divide_dword_loop_4_r_gt_d:
    ; R >= D
    ; R -= D
    ldi ph, hi(denominator)
    ldi pl, lo(denominator)
    ld a
    ldi ph, hi(remainder)
    ldi pl, lo(remainder)
    ld b
    sub b, a
    st b
    ldi ph, hi(denominator + 1)
    ldi pl, lo(denominator + 1)
    ld a
    ldi ph, hi(remainder + 1)
    ldi pl, lo(remainder + 1)
    ld b
    sbb b, a
    st b
    ldi ph, hi(denominator + 2)
    ldi pl, lo(denominator + 2)
    ld a
    ldi ph, hi(remainder + 2)
    ldi pl, lo(remainder + 2)
    ld b
    sbb b, a
    st b
    ldi ph, hi(denominator + 3)
    ldi pl, lo(denominator + 3)
    ld a
    ldi ph, hi(remainder + 3)
    ldi pl, lo(remainder + 3)
    ld b
    sbb b, a
    st b
    ; Q |= qbit
    ldi pl, lo(qbit)
    ld a
    ldi pl, lo(quotient + 0)
    ld b
    or b, a
    st b

divide_dword_loop_4_r_lt_d:
    ; qbit >>= 1
    ldi ph, hi(qbit)
    ldi pl, lo(qbit)
    ld a
    shr a
    st a
    ldi ph, hi(divide_dword_loop_4)
    ldi pl, lo(divide_dword_loop_4)
    jnc

    ldi pl, lo(inner_ret)
    ldi ph, hi(inner_ret)
    ld  a
    inc pl
    ld  ph
    mov pl, a
    jmp

    .section bss.__cc_udiv_dword
    .align 32
    ; remainder must directly follow quotient
quotient:  res 4
remainder: res 4
inner_ret: res 2
outer_ret: res 2
qbit:      res 1
negate:    res 1
