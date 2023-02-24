use std::fmt::Formatter;
use std::{iter::Peekable, str::Chars};

use crate::ctype;
use crate::initializer::TypedConstant;

#[derive(Debug, PartialEq)]
pub enum StringParseError {
    UnexpectedEOL,
    BadChar(char),
    BadEscape(char),
    BadDigit(char),
    BadCode(u32),
    BadPrefix(char),
    BadQuote(char),
    MulticharacterOverlap,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum CharType {
    Default,
    Char16T,
    Char32T,
}

pub fn parse_char_literal_typed(lit: &str) -> Result<TypedConstant, StringParseError> {
    let (val, t) = parse_char_literal(lit)?;
    let t = match t {
        CharType::Default => ctype::INT_TYPE,
        CharType::Char16T => ctype::CHAR16_TYPE,
        CharType::Char32T => ctype::CHAR32_TYPE,
    };
    Ok(TypedConstant::new_integer(val.into(), t))
}

fn parse_char_literal(lit: &str) -> Result<(u32, CharType), StringParseError> {
    enum State {
        PrefixQuote,
        Quote(CharType),
        Char(CharType, Option<u32>),
        End(CharType, u32),
    }
    let mut iter = lit.chars().peekable();
    let mut state = State::PrefixQuote;
    loop {
        state = match state {
            State::PrefixQuote => match iter.next() {
                None => return Err(StringParseError::UnexpectedEOL),
                Some('\'') => State::Char(CharType::Default, None),
                Some('L') => State::Quote(CharType::Char16T), // wchar_t is 2 bytes
                Some('u') => State::Quote(CharType::Char16T),
                Some('U') => State::Quote(CharType::Char32T),
                Some(c) => return Err(StringParseError::BadPrefix(c)),
            },
            State::Quote(t) => match iter.next() {
                None => return Err(StringParseError::UnexpectedEOL),
                Some('\'') => State::Char(t, None),
                Some(c) => return Err(StringParseError::BadQuote(c)),
            },
            State::Char(t, None) => match parse_single_char(&mut iter, t, '\'')? {
                None => return Err(StringParseError::UnexpectedEOL),
                Some(x) => match t {
                    CharType::Default if x < 256 => State::Char(t, Some(x)),
                    CharType::Char16T if x < 65536 => State::Char(t, Some(x)),
                    CharType::Char32T => State::Char(t, Some(x)),
                    _ => State::End(t, x),
                },
            },
            State::Char(t, Some(x)) => match parse_single_char(&mut iter, t, '\'')? {
                None => return Ok((x, t)),
                Some(y) => match t {
                    CharType::Default if x < 256 => State::Char(t, Some((x << 8) + y)),
                    CharType::Char16T if x < 65536 => State::Char(t, Some((x << 16) + y)),
                    CharType::Char32T => State::Char(t, Some(y)),
                    _ => return Err(StringParseError::MulticharacterOverlap),
                },
            },
            State::End(t, x) => {
                if let Some('\'') = iter.next() {
                    return Ok((x, t));
                } else {
                    return Err(StringParseError::MulticharacterOverlap);
                }
            }
        }
    }
}

fn parse_single_char(
    it: &mut Peekable<Chars>,
    char_type: CharType,
    quote: char,
) -> Result<Option<u32>, StringParseError> {
    enum State {
        Start,
        Escape,
        Code(u8, u32, u32), // bytes remaining, value, base
    }
    let mut state = State::Start;
    loop {
        state = match state {
            State::Start => match it.peek() {
                None => return Err(StringParseError::UnexpectedEOL),
                Some(c) if *c == quote => return Ok(None),
                Some('\\') => {
                    it.next();
                    State::Escape
                }
                Some(c) => {
                    let result = check_char(*c, char_type)?;
                    it.next();
                    return Ok(Some(result));
                }
            },
            State::Escape => match it.peek() {
                None => return Err(StringParseError::UnexpectedEOL),
                Some(c) => match c {
                    '\'' | '"' | '\\' | '?' => {
                        let result = check_char(*c, char_type)?;
                        it.next();
                        return Ok(Some(result));
                    }
                    'a' => {
                        let result = check_char(char::from_u32(7).unwrap(), char_type)?;
                        it.next();
                        return Ok(Some(result));
                    }
                    'b' => {
                        let result = check_char(char::from_u32(8).unwrap(), char_type)?;
                        it.next();
                        return Ok(Some(result));
                    }
                    'f' => {
                        let result = check_char(char::from_u32(0xc).unwrap(), char_type)?;
                        it.next();
                        return Ok(Some(result));
                    }
                    'n' => {
                        let result = check_char(char::from_u32(0xa).unwrap(), char_type)?;
                        it.next();
                        return Ok(Some(result));
                    }
                    'r' => {
                        let result = check_char(char::from_u32(0xd).unwrap(), char_type)?;
                        it.next();
                        return Ok(Some(result));
                    }
                    't' => {
                        let result = check_char(char::from_u32(9).unwrap(), char_type)?;
                        it.next();
                        return Ok(Some(result));
                    }
                    'v' => {
                        let result = check_char(char::from_u32(0xb).unwrap(), char_type)?;
                        it.next();
                        return Ok(Some(result));
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
                    '0'..='7' => State::Code(3, 0, 8),
                    _ => return Err(StringParseError::BadEscape(*c)),
                },
            },
            State::Code(0, value, _) => {
                let c = match char::from_u32(value) {
                    Some(c) => c,
                    None => return Err(StringParseError::BadCode(value)),
                };
                let result = check_char(c, char_type)?;
                return Ok(Some(result));
            }
            State::Code(count, value, base) => match it.peek() {
                None => return Err(StringParseError::UnexpectedEOL),
                Some(c) if *c == quote => return Err(StringParseError::UnexpectedEOL),
                Some(c) if c.is_digit(base) => {
                    let d = c.to_digit(base).unwrap();
                    it.next();
                    State::Code(count - 1, (value * base) + d, base)
                }
                Some(c) => return Err(StringParseError::BadDigit(*c)),
            },
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

impl std::fmt::Display for StringParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            StringParseError::UnexpectedEOL => write!(f, "unexpected end of input"),
            StringParseError::BadChar(c) => write!(f, "bad character `{}' for the given type", c),
            StringParseError::BadEscape(c) => write!(f, "bad escape sequence `\\{}'", c),
            StringParseError::BadDigit(c) => write!(f, "bad digit `{}'", c),
            StringParseError::BadCode(x) => write!(f, "bad numeric character code {}", x),
            StringParseError::BadPrefix(c) => write!(f, "bad character literal prefix `{}'", c),
            StringParseError::BadQuote(c) => write!(f, "bad quote {}", c),
            StringParseError::MulticharacterOverlap => {
                write!(f, "character values overlap in a multi-char literal")
            }
        }
    }
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
        let r = parse_single_char(&mut iter, CharType::Default, '\'')
            .unwrap()
            .unwrap();
        assert_eq!(r, 'h' as u32);
        assert_eq!(*iter.peek().unwrap(), '\'');
    }

    #[test]
    fn test_char_2() {
        let s = "ф'";
        let mut iter = s.chars().peekable();
        let r = parse_single_char(&mut iter, CharType::Char16T, '\'')
            .unwrap()
            .unwrap();
        assert_eq!(r, 'ф' as u32);
        assert_eq!(*iter.peek().unwrap(), '\'');
    }

    #[test]
    fn test_char_3() {
        let s = "🍌'";
        let mut iter = s.chars().peekable();
        let r = parse_single_char(&mut iter, CharType::Char32T, '\'')
            .unwrap()
            .unwrap();
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
        let r = parse_single_char(&mut iter, CharType::Char32T, '\'')
            .unwrap()
            .unwrap();
        assert_eq!(r, ' ' as u32);
        assert_eq!(*iter.peek().unwrap(), '\'');
    }

    #[test]
    fn test_char_6() {
        let s = "\\x20'";
        let mut iter = s.chars().peekable();
        let r = parse_single_char(&mut iter, CharType::Default, '\'')
            .unwrap()
            .unwrap();
        assert_eq!(r, ' ' as u32);
        assert_eq!(*iter.peek().unwrap(), '\'');
    }

    #[test]
    fn test_char_7() {
        let s = "\\u1234'";
        let mut iter = s.chars().peekable();
        let r = parse_single_char(&mut iter, CharType::Char16T, '\'')
            .unwrap()
            .unwrap();
        assert_eq!(r, 0x1234);
        assert_eq!(*iter.peek().unwrap(), '\'');
    }

    #[test]
    fn test_char_8() {
        let s = "\\U0001f34c'";
        let mut iter = s.chars().peekable();
        let r = parse_single_char(&mut iter, CharType::Char32T, '\'')
            .unwrap()
            .unwrap();
        assert_eq!(r, 0x1f34c);
        assert_eq!(*iter.peek().unwrap(), '\'');
    }

    #[test]
    fn test_char_lit_1() {
        let val = parse_char_literal("'x'").unwrap();
        assert_eq!(val, ('x' as u32, CharType::Default));
    }

    #[test]
    fn test_char_lit_2() {
        let val = parse_char_literal("u'щ'").unwrap();
        assert_eq!(val, ('щ' as u32, CharType::Char16T));
    }

    #[test]
    fn test_char_lit_3() {
        let val = parse_char_literal("'ab'").unwrap();
        assert_eq!(val, (0x6162, CharType::Default));
    }
}
