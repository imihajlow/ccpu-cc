use lang_c::driver::Flavor;
use static_assertions::const_assert;

use crate::machine;

const_assert!(!machine::CHAR_SIGNED);
const_assert!(machine::INT_SIZE == 2);
const_assert!(machine::SHORT_SIZE == 2);
const_assert!(machine::LONG_SIZE == 4);
const_assert!(machine::LLONG_SIZE == 8);
const_assert!(machine::PTR_SIZE == 2);

pub fn get_config(
    flavor: Flavor,
    defines: Vec<String>,
    include: Vec<String>,
    isystem: Vec<String>,
    iquote: Vec<String>,
) -> lang_c::driver::Config {
    let mut options = Vec::new();
    options.push("-E".to_string());
    options.push("-std=c11".to_string());
    options.push("-undef".to_string());
    options.push("-nostdinc".to_string());

    options.push("-D__CHAR_UNSIGNED__".to_string());
    options.push("-D__USER_LABEL_PREFIX__=".to_string());

    options.push("-D__SIZE_TYPE__=unsigned int".to_string());
    options.push("-D__PTRDIFF_TYPE__=int".to_string());
    options.push("-D__WCHAR_TYPE__=int".to_string());
    options.push("-D__WINT_TYPE__=int".to_string());
    options.push("-D__INTMAX_TYPE__=long long int".to_string());
    options.push("-D__UINTMAX_TYPE__=unsigned long long int".to_string());
    options.push("-D__SIG_ATOMIC_TYPE__=int".to_string());
    options.push("-D__INT8_TYPE__=signed char".to_string());
    options.push("-D__INT16_TYPE__=int".to_string());
    options.push("-D__INT32_TYPE__=long".to_string());
    options.push("-D__INT64_TYPE__=long long".to_string());
    options.push("-D__UINT8_TYPE__=unsigned char".to_string());
    options.push("-D__UINT16_TYPE__=unsigned int".to_string());
    options.push("-D__UINT32_TYPE__=unsigned long".to_string());
    options.push("-D__UINT64_TYPE__=unsigned long long".to_string());
    options.push("-D__INT_LEAST8_TYPE__=signed char".to_string());
    options.push("-D__INT_LEAST16_TYPE__=int".to_string());
    options.push("-D__INT_LEAST32_TYPE__=long".to_string());
    options.push("-D__INT_LEAST64_TYPE__=long long".to_string());
    options.push("-D__UINT_LEAST8_TYPE__=unsigned char".to_string());
    options.push("-D__UINT_LEAST16_TYPE__=unsigned int".to_string());
    options.push("-D__UINT_LEAST32_TYPE__=unsigned long".to_string());
    options.push("-D__UINT_LEAST64_TYPE__=unsigned long long".to_string());
    options.push("-D__INT_FAST8_TYPE__=signed char".to_string());
    options.push("-D__INT_FAST16_TYPE__=int".to_string());
    options.push("-D__INT_FAST32_TYPE__=long".to_string());
    options.push("-D__INT_FAST64_TYPE__=long long".to_string());
    options.push("-D__UINT_FAST8_TYPE__=unsigned char".to_string());
    options.push("-D__UINT_FAST16_TYPE__=unsigned int".to_string());
    options.push("-D__UINT_FAST32_TYPE__=unsigned long".to_string());
    options.push("-D__UINT_FAST64_TYPE__=unsigned long long".to_string());
    options.push("-D__INTPTR_TYPE__=int".to_string());
    options.push("-D__UINTPTR_TYPE__=unsigned int".to_string());

    options.push("-D__CHAR_BIT__=8".to_string());

    options.push("-D__SCHAR_MAX__=0x7f".to_string());
    options.push("-D__WCHAR_MAX__=0x7fff".to_string());
    options.push("-D__SHRT_MAX__=0x7fff".to_string());
    options.push("-D__INT_MAX__=0x7fff".to_string());
    options.push("-D__LONG_MAX__=0x7fffffffL".to_string());
    options.push("-D__LONG_LONG_MAX__=0x7fffffffffffffffLL".to_string());
    options.push("-D__WINT_MAX__=0x7fff".to_string());
    options.push("-D__SIZE_MAX__=0xffff".to_string());
    options.push("-D__PTRDIFF_MAX__=0x7fff".to_string());
    options.push("-D__INTMAX_MAX__=0x7fffffffffffffffLL".to_string());
    options.push("-D__UINTMAX_MAX__=0xffffffffffffffffULL".to_string());
    options.push("-D__SIG_ATOMIC_MAX__=0x7fff".to_string());
    options.push("-D__INT8_MAX__=0x7f".to_string());
    options.push("-D__INT16_MAX__=0x7fff".to_string());
    options.push("-D__INT32_MAX__=0x7fffffffL".to_string());
    options.push("-D__INT64_MAX__=0x7fffffffffffffffLL".to_string());
    options.push("-D__UINT8_MAX__=0xff".to_string());
    options.push("-D__UINT16_MAX__=0xffff".to_string());
    options.push("-D__UINT32_MAX__=0xffffffffUL".to_string());
    options.push("-D__UINT64_MAX__=0xffffffffffffffffULL".to_string());
    options.push("-D__INT_LEAST8_MAX__=0x7f".to_string());
    options.push("-D__INT_LEAST16_MAX__=0x7fff".to_string());
    options.push("-D__INT_LEAST32_MAX__=0x7fffffffL".to_string());
    options.push("-D__INT_LEAST64_MAX__=0x7fffffffffffffffLL".to_string());
    options.push("-D__UINT_LEAST8_MAX__=0xff".to_string());
    options.push("-D__UINT_LEAST16_MAX__=0xffff".to_string());
    options.push("-D__UINT_LEAST32_MAX__=0xffffffffUL".to_string());
    options.push("-D__UINT_LEAST64_MAX__=0xffffffffffffffffULL".to_string());
    options.push("-D__INT_FAST8_MAX__=0x7f".to_string());
    options.push("-D__INT_FAST16_MAX__=0x7fff".to_string());
    options.push("-D__INT_FAST32_MAX__=0x7fffffffL".to_string());
    options.push("-D__INT_FAST64_MAX__=0x7fffffffffffffffLL".to_string());
    options.push("-D__UINT_FAST8_MAX__=0xff".to_string());
    options.push("-D__UINT_FAST16_MAX__=0xffff".to_string());
    options.push("-D__UINT_FAST32_MAX__=0xffffffffUL".to_string());
    options.push("-D__UINT_FAST64_MAX__=0xffffffffffffffffULL".to_string());
    options.push("-D__INTPTR_MAX__=0x7fff".to_string());
    options.push("-D__UINTPTR_MAX__=0xffff".to_string());
    options.push("-D__WCHAR_MIN__=-32768".to_string());
    options.push("-D__WINT_MIN__=-32768".to_string());
    options.push("-D__SIG_ATOMIC_MIN__=-32768".to_string());

    options.push("-D__SCHAR_WIDTH__=8".to_string());
    options.push("-D__SHRT_WIDTH__=16".to_string());
    options.push("-D__INT_WIDTH__=16".to_string());
    options.push("-D__LONG_WIDTH__=32".to_string());
    options.push("-D__LONG_LONG_WIDTH__=64".to_string());
    options.push("-D__PTRDIFF_WIDTH__=16".to_string());
    options.push("-D__SIG_ATOMIC_WIDTH__=16".to_string());
    options.push("-D__SIZE_WIDTH__=16".to_string());
    options.push("-D__WCHAR_WIDTH__=16".to_string());
    options.push("-D__WINT_WIDTH__=16".to_string());
    options.push("-D__INT_LEAST8_WIDTH__=8".to_string());
    options.push("-D__INT_LEAST16_WIDTH__=16".to_string());
    options.push("-D__INT_LEAST32_WIDTH__=32".to_string());
    options.push("-D__INT_LEAST64_WIDTH__=64".to_string());
    options.push("-D__INT_FAST8_WIDTH__=8".to_string());
    options.push("-D__INT_FAST16_WIDTH__=16".to_string());
    options.push("-D__INT_FAST32_WIDTH__=32".to_string());
    options.push("-D__INT_FAST64_WIDTH__=64".to_string());
    options.push("-D__INTPTR_WIDTH__=16".to_string());
    options.push("-D__INTMAX_WIDTH__=64".to_string());

    options.push("-D__SIZEOF_INT__=2".to_string());
    options.push("-D__SIZEOF_LONG__=4".to_string());
    options.push("-D__SIZEOF_LONG_LONG__=8".to_string());
    options.push("-D__SIZEOF_SHORT__=2".to_string());
    options.push("-D__SIZEOF_POINTER__=2".to_string());
    options.push("-D__SIZEOF_FLOAT__=4".to_string());
    options.push("-D__SIZEOF_DOUBLE__=4".to_string());
    options.push("-D__SIZEOF_LONG_DOUBLE__=4".to_string());
    options.push("-D__SIZEOF_SIZE_T__=2".to_string());
    options.push("-D__SIZEOF_WCHAR_T__=2".to_string());
    options.push("-D__SIZEOF_WINT_T__=2".to_string());
    options.push("-D__SIZEOF_PTRDIFF_T__=2".to_string());

    options.push("-D__ORDER_BIG_ENDIAN__=4321".to_string());
    options.push("-D__ORDER_LITTLE_ENDIAN__=1234".to_string());
    options.push("-D__ORDER_PDP_ENDIAN__=3412".to_string());
    options.push("-D__BYTE_ORDER__=__ORDER_LITTLE_ENDIAN__".to_string());
    options.push("-D__FLOAT_WORD_ORDER__=__ORDER_LITTLE_ENDIAN__".to_string());

    for s in defines {
        options.push(format!("-D{}", s));
    }

    for s in include {
        options.push(format!("-I{}", s));
    }

    for s in isystem {
        options.push(format!("-isystem {}", s));
    }

    for s in iquote {
        options.push(format!("-iquote {}", s));
    }

    lang_c::driver::Config {
        cpp_command: "clang".to_string(),
        cpp_options: options,
        flavor,
    }
}
