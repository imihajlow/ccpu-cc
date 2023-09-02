pub const CHAR_SIGNED: bool = false;
pub const SHORT_SIZE: u8 = 2;
pub const INT_SIZE: u8 = 2;
pub const LONG_SIZE: u8 = 4;
pub const LLONG_SIZE: u8 = 8;
pub const PTR_SIZE: u8 = 2;
pub const BOOL_SIZE: u8 = 1;
pub const VA_LIST_SIZE: u8 = 4;

pub const SHORT_ALIGN: u32 = 2;
pub const INT_ALIGN: u32 = 2;
pub const LONG_ALIGN: u32 = 4;
pub const LLONG_ALIGN: u32 = 8;
pub const PTR_ALIGN: u32 = 2;
pub const BOOL_ALIGN: u32 = 1;

const_assert!(SHORT_SIZE >= 2);
const_assert!(INT_SIZE >= 2);
const_assert!(LONG_SIZE >= 4);
const_assert!(LLONG_SIZE >= 8);

const_assert!(SHORT_SIZE <= INT_SIZE);
const_assert!(INT_SIZE <= LONG_SIZE);
const_assert!(LONG_SIZE <= LLONG_SIZE);
