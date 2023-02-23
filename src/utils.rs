pub fn align<T: num_traits::int::PrimInt>(addr: T, alignment: T) -> T {
    let rem = addr % alignment;
    if !rem.is_zero() {
        addr + alignment - rem
    } else {
        addr
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_align() {
        assert_eq!(align(4, 4), 4);
        assert_eq!(align(5, 4), 8);
    }
}
