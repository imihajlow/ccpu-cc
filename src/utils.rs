pub fn align<T: num_traits::int::PrimInt>(addr: T, alignment: T) -> T {
    let rem = addr % alignment;
    if !rem.is_zero() {
        addr + alignment - rem
    } else {
        addr
    }
}

pub fn factorial(n: usize) -> Option<usize> {
    let mut r: usize = 1;
    for i in 2..n {
        r = r.checked_mul(i)?;
    }
    Some(r)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_align() {
        assert_eq!(align(4, 4), 4);
        assert_eq!(align(5, 4), 8);
    }

    fn test_factorial() {
        assert_eq!(factorial(0), Some(1));
        assert_eq!(factorial(1), Some(1));
        assert_eq!(factorial(5), Some(120));
        assert_eq!(factorial(1000), None);
    }
}
