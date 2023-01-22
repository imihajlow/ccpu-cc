use std::{iter::Peekable, str::Chars};

#[derive(Debug, PartialEq)]
pub enum StringParseError {
    UnexpectedEOL,
    BadChar(char),
    BadEscape(char),
    BadDigit(char),
    BadCode(u32),
}

#[derive(Clone, Copy)]
enum CharType {
    Default,
    Char16T,
    Char32T,
}

fn parse_single_char(
    it: &mut Peekable<Chars>,
    char_type: CharType,
    quote: char,
) -> Result<u32, StringParseError> {
    enum State {
        Start,
        Escape,
        Code(u8, u32, u32), // remaining, value, base
    }
    let mut state = State::Start;
    loop {
        state = match state {
            State::Start => match it.peek() {
                None => return Err(StringParseError::UnexpectedEOL),
                Some(c) if *c == quote => return Err(StringParseError::UnexpectedEOL),
                Some('\\') => {
                    it.next();
                    State::Escape
                }
                Some(c) => {
                    let result = check_char(*c, char_type)?;
                    it.next();
                    return Ok(result);
                }
            },
            State::Escape => match it.peek() {
                None => return Err(StringParseError::UnexpectedEOL),
                Some(c) => match c {
                    '\'' | '"' | '\\' | '?' => {
                        let result = check_char(*c, char_type)?;
                        it.next();
                        return Ok(result);
                    }
                    'a' => {
                        let result = check_char(char::from_u32(7).unwrap(), char_type)?;
                        it.next();
                        return Ok(result);
                    }
                    'b' => {
                        let result = check_char(char::from_u32(8).unwrap(), char_type)?;
                        it.next();
                        return Ok(result);
                    }
                    'f' => {
                        let result = check_char(char::from_u32(0xc).unwrap(), char_type)?;
                        it.next();
                        return Ok(result);
                    }
                    'n' => {
                        let result = check_char(char::from_u32(0xa).unwrap(), char_type)?;
                        it.next();
                        return Ok(result);
                    }
                    'r' => {
                        let result = check_char(char::from_u32(0xd).unwrap(), char_type)?;
                        it.next();
                        return Ok(result);
                    }
                    't' => {
                        let result = check_char(char::from_u32(9).unwrap(), char_type)?;
                        it.next();
                        return Ok(result);
                    }
                    'v' => {
                        let result = check_char(char::from_u32(0xb).unwrap(), char_type)?;
                        it.next();
                        return Ok(result);
                    }
                    'x' => {
                        it.next();
                        State::Code(2, 0, 16)
                    }
                    'u' => {
                        it.next();
                        State::Code(4, 0, 16)
                    }
                    'U' => {
                        it.next();
                        State::Code(8, 0, 16)
                    }
                    '0'..='7' => {
                        State::Code(3, 0, 8)
                    }
                    _ => return Err(StringParseError::BadEscape(*c)),
                },
            }
            State::Code(0, value, _) => {
                let c = match char::from_u32(value) {
                    Some(c) => c,
                    None => return Err(StringParseError::BadCode(value))
                };
                let result = check_char(c, char_type)?;
                return Ok(result);
            }
            State::Code(count, value, base) => {
                match it.peek() {
                    None => return Err(StringParseError::UnexpectedEOL),
                    Some(c) if *c == quote => return Err(StringParseError::UnexpectedEOL),
                    Some(c) if c.is_digit(base) => {
                        let d = c.to_digit(base).unwrap();
                        it.next();
                        State::Code(count - 1, (value * base) + d, base)
                    }
                    Some(c) => return Err(StringParseError::BadDigit(*c))
                }
            }
        }
    }
}

fn check_char(c: char, char_type: CharType) -> Result<u32, StringParseError> {
    match char_type {
        CharType::Default if (c as u32) < 128 => Ok(c as u32),
        CharType::Char16T if is_utf16_single(c) => Ok(c as u32),
        CharType::Char32T => Ok(c as u32),
        _ => Err(StringParseError::BadChar(c)),
    }
}

fn is_utf16_single(c: char) -> bool {
    let code = c as u32;
    code <= 0xd7ff || (code >= 0xe000 && code <= 0xffff)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_utf16() {
        assert!(is_utf16_single('b'));
        assert!(is_utf16_single('ф'));
        assert!(is_utf16_single('貓'));
        assert!(!is_utf16_single('🍌'));
    }

    #[test]
    fn test_char_1() {
        let s = "h'";
        let mut iter = s.chars().peekable();
        let r = parse_single_char(&mut iter, CharType::Default, '\'').unwrap();
        assert_eq!(r, 'h' as u32);
        assert_eq!(*iter.peek().unwrap(), '\'');
    }

    #[test]
    fn test_char_2() {
        let s = "ф'";
        let mut iter = s.chars().peekable();
        let r = parse_single_char(&mut iter, CharType::Char16T, '\'').unwrap();
        assert_eq!(r, 'ф' as u32);
        assert_eq!(*iter.peek().unwrap(), '\'');
    }

    #[test]
    fn test_char_3() {
        let s = "🍌'";
        let mut iter = s.chars().peekable();
        let r = parse_single_char(&mut iter, CharType::Char32T, '\'').unwrap();
        assert_eq!(r, '🍌' as u32);
        assert_eq!(*iter.peek().unwrap(), '\'');
    }

    #[test]
    fn test_char_4() {
        let s = "🍌'";
        let mut iter = s.chars().peekable();
        let r = parse_single_char(&mut iter, CharType::Char16T, '\'');
        assert_eq!(r, Err(StringParseError::BadChar('🍌')));
        assert_eq!(*iter.peek().unwrap(), '🍌');
    }

    #[test]
    fn test_char_5() {
        let s = "\\040'";
        let mut iter = s.chars().peekable();
        let r = parse_single_char(&mut iter, CharType::Char32T, '\'').unwrap();
        assert_eq!(r, ' ' as u32);
        assert_eq!(*iter.peek().unwrap(), '\'');
    }

    #[test]
    fn test_char_6() {
        let s = "\\x20'";
        let mut iter = s.chars().peekable();
        let r = parse_single_char(&mut iter, CharType::Default, '\'').unwrap();
        assert_eq!(r, ' ' as u32);
        assert_eq!(*iter.peek().unwrap(), '\'');
    }

    #[test]
    fn test_char_7() {
        let s = "\\u1234'";
        let mut iter = s.chars().peekable();
        let r = parse_single_char(&mut iter, CharType::Char16T, '\'').unwrap();
        assert_eq!(r, 0x1234);
        assert_eq!(*iter.peek().unwrap(), '\'');
    }

    #[test]
    fn test_char_8() {
        let s = "\\U0001f34c'";
        let mut iter = s.chars().peekable();
        let r = parse_single_char(&mut iter, CharType::Char32T, '\'').unwrap();
        assert_eq!(r, 0x1f34c);
        assert_eq!(*iter.peek().unwrap(), '\'');
    }
}
