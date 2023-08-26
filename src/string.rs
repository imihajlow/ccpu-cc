use std::fmt::Formatter;
use std::{iter::Peekable, str::Chars};

use crate::ctype::{self, CType};
use crate::initializer::TypedConstant;

#[derive(Debug, PartialEq)]
pub enum StringParseError {
    UnexpectedEOL,
    BadChar(char),
    BadEscape(char),
    BadDigit(char),
    BadCode(u32),
    BadPrefix(String),
    BadQuote(char),
    MulticharacterOverlap,
    UnsupportedConcat,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum StringEncoding {
    Default,
    Wchar,
    Utf8,
    Utf16,
    Utf32,
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

pub fn parse_string_literal(lit: Vec<String>) -> Result<(CType, Vec<u8>), StringParseError> {
    assert!(!lit.is_empty());
    let parts: Result<Vec<_>, _> = lit.into_iter().map(parse_string_literal_single).collect();
    let parts = parts?;
    let common_encoding = {
        let mut com = StringEncoding::Default;
        for (e, _) in &parts {
            com = match (com, *e) {
                (StringEncoding::Default, e) => e,
                (e, StringEncoding::Default) => e,
                (e1, e2) if e1 == e2 => e1,
                _ => return Err(StringParseError::UnsupportedConcat),
            }
        }
        com
    };

    let mut result = Vec::new();
    for (_, part) in parts {
        for c in part {
            encode_char(&mut result, c, common_encoding)?;
        }
    }
    result.push(0);

    let ctype = match common_encoding {
        StringEncoding::Default => ctype::CHAR_TYPE,
        StringEncoding::Wchar => ctype::WCHAR_TYPE,
        StringEncoding::Utf8 => ctype::CHAR_TYPE,
        StringEncoding::Utf16 => ctype::CHAR16_TYPE,
        StringEncoding::Utf32 => ctype::CHAR32_TYPE,
    };
    Ok((ctype, result))
}

fn parse_string_literal_single(
    lit: String,
) -> Result<(StringEncoding, Vec<char>), StringParseError> {
    enum State {
        PrefixQuote,
        Quote8,
        Quote(StringEncoding),
        Char(StringEncoding),
    }
    let mut iter = lit.chars().peekable();
    let mut state = State::PrefixQuote;
    let mut result = Vec::new();
    loop {
        state = match state {
            State::PrefixQuote => match iter.next() {
                None => return Err(StringParseError::UnexpectedEOL),
                Some('"') => State::Char(StringEncoding::Default),
                Some('L') => State::Quote(StringEncoding::Wchar),
                Some('u') => State::Quote8,
                Some('U') => State::Quote(StringEncoding::Utf32),
                Some(c) => return Err(StringParseError::BadPrefix(c.to_string())),
            },
            State::Quote8 => match iter.next() {
                None => return Err(StringParseError::UnexpectedEOL),
                Some('8') => State::Quote(StringEncoding::Utf8),
                Some('"') => State::Char(StringEncoding::Utf16),
                Some(c) => return Err(StringParseError::BadPrefix(format!("u{}", c))),
            },
            State::Quote(t) => match iter.next() {
                None => return Err(StringParseError::UnexpectedEOL),
                Some('"') => State::Char(t),
                Some(c) => return Err(StringParseError::BadQuote(c)),
            },
            State::Char(e) => match parse_single_char(&mut iter, '"') {
                Ok(None) => {
                    // quote being last is ensured by lang_c parser
                    return Ok((e, result));
                }
                Ok(Some(c)) => {
                    result.push(c);
                    State::Char(e)
                }
                Err(e) => return Err(e),
            },
        }
    }
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
                Some(c) => return Err(StringParseError::BadPrefix(c.to_string())),
            },
            State::Quote(t) => match iter.next() {
                None => return Err(StringParseError::UnexpectedEOL),
                Some('\'') => State::Char(t, None),
                Some(c) => return Err(StringParseError::BadQuote(c)),
            },
            State::Char(t, None) => match parse_single_char(&mut iter, '\'')? {
                None => return Err(StringParseError::UnexpectedEOL),
                Some(c) => {
                    let x = check_char(c, t)?;
                    match t {
                        CharType::Default if x < 256 => State::Char(t, Some(x)),
                        CharType::Char16T if x < 65536 => State::Char(t, Some(x)),
                        CharType::Char32T => State::Char(t, Some(x)),
                        _ => State::End(t, x),
                    }
                }
            },
            State::Char(t, Some(x)) => match parse_single_char(&mut iter, '\'')? {
                None => return Ok((x, t)),
                Some(c) => {
                    let y = check_char(c, t)?;
                    match t {
                        CharType::Default if x < 256 => State::Char(t, Some((x << 8) + y)),
                        CharType::Char16T if x < 65536 => State::Char(t, Some((x << 16) + y)),
                        CharType::Char32T => State::Char(t, Some(y)),
                        _ => return Err(StringParseError::MulticharacterOverlap),
                    }
                }
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
    quote: char,
) -> Result<Option<char>, StringParseError> {
    enum State {
        Start,
        Escape,
        Code(u8, u32, u32), // bytes remaining, value, base
    }
    let mut state = State::Start;
    loop {
        state = match state {
            State::Start => match it.next() {
                None => return Err(StringParseError::UnexpectedEOL),
                Some(c) if c == quote => return Ok(None),
                Some('\\') => State::Escape,
                Some(c) => {
                    return Ok(Some(c));
                }
            },
            State::Escape => match it.peek() {
                None => return Err(StringParseError::UnexpectedEOL),
                Some(c) => match c {
                    '\'' | '"' | '\\' | '?' => {
                        let r = *c;
                        it.next();
                        return Ok(Some(r));
                    }
                    'a' => {
                        it.next();
                        return Ok(Some(char::from_u32(7).unwrap()));
                    }
                    'b' => {
                        it.next();
                        return Ok(Some(char::from_u32(8).unwrap()));
                    }
                    'f' => {
                        it.next();
                        return Ok(Some(char::from_u32(0xc).unwrap()));
                    }
                    'n' => {
                        it.next();
                        return Ok(Some(char::from_u32(0xa).unwrap()));
                    }
                    'r' => {
                        it.next();
                        return Ok(Some(char::from_u32(0xd).unwrap()));
                    }
                    't' => {
                        it.next();
                        return Ok(Some(char::from_u32(9).unwrap()));
                    }
                    'v' => {
                        it.next();
                        return Ok(Some(char::from_u32(0xb).unwrap()));
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
                return match char::from_u32(value) {
                    Some(c) => Ok(Some(c)),
                    None => Err(StringParseError::BadCode(value)),
                };
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

fn encode_char(buf: &mut Vec<u8>, c: char, enc: StringEncoding) -> Result<(), StringParseError> {
    match enc {
        StringEncoding::Default => {
            let x = c as u32;
            if x < 128 {
                buf.push(x as u8);
                Ok(())
            } else {
                Err(StringParseError::BadChar(c))
            }
        }
        StringEncoding::Wchar => {
            if is_utf16_single(c) {
                let x = c as u16;
                buf.extend_from_slice(&x.to_le_bytes());
                Ok(())
            } else {
                Err(StringParseError::BadChar(c))
            }
        }
        StringEncoding::Utf8 => {
            let mut tmp = [0; 4];
            buf.extend_from_slice(c.encode_utf8(&mut tmp).as_bytes());
            Ok(())
        }
        StringEncoding::Utf16 => {
            let mut tmp = [0; 2];
            let a = c.encode_utf16(&mut tmp);
            for w in a {
                buf.extend_from_slice(&w.to_le_bytes());
            }
            Ok(())
        }
        StringEncoding::Utf32 => {
            let x = c as u32;
            buf.extend_from_slice(&x.to_le_bytes());
            Ok(())
        }
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
            StringParseError::UnsupportedConcat => write!(
                f,
                "unsupported non-standard concatenation of string literals"
            ),
        }
    }
}

#[cfg(test)]
mod test {
    use std::assert_eq;

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
        let r = parse_single_char(&mut iter, '\'').unwrap().unwrap();
        assert_eq!(r, 'h');
        assert_eq!(*iter.peek().unwrap(), '\'');
    }

    #[test]
    fn test_char_2() {
        let s = "ф'";
        let mut iter = s.chars().peekable();
        let r = parse_single_char(&mut iter, '\'').unwrap().unwrap();
        assert_eq!(r, 'ф');
        assert_eq!(*iter.peek().unwrap(), '\'');
    }

    #[test]
    fn test_char_3() {
        let s = "🍌'";
        let mut iter = s.chars().peekable();
        let r = parse_single_char(&mut iter, '\'').unwrap().unwrap();
        assert_eq!(r, '🍌');
        assert_eq!(*iter.peek().unwrap(), '\'');
    }

    #[test]
    fn test_char_4() {
        assert_eq!(
            check_char('🍌', CharType::Char16T),
            Err(StringParseError::BadChar('🍌'))
        );
    }

    #[test]
    fn test_char_5() {
        let s = "\\040'";
        let mut iter = s.chars().peekable();
        let r = parse_single_char(&mut iter, '\'').unwrap().unwrap();
        assert_eq!(r, ' ');
        assert_eq!(*iter.peek().unwrap(), '\'');
    }

    #[test]
    fn test_char_6() {
        let s = "\\x20'";
        let mut iter = s.chars().peekable();
        let r = parse_single_char(&mut iter, '\'').unwrap().unwrap();
        assert_eq!(r, ' ');
        assert_eq!(*iter.peek().unwrap(), '\'');
    }

    #[test]
    fn test_char_7() {
        let s = "\\u1234'";
        let mut iter = s.chars().peekable();
        let r = parse_single_char(&mut iter, '\'').unwrap().unwrap();
        assert_eq!(r, char::from_u32(0x1234).unwrap());
        assert_eq!(*iter.peek().unwrap(), '\'');
    }

    #[test]
    fn test_char_8() {
        let s = "\\U0001f34c'";
        let mut iter = s.chars().peekable();
        let r = parse_single_char(&mut iter, '\'').unwrap().unwrap();
        assert_eq!(r, char::from_u32(0x1f34c).unwrap());
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

    #[test]
    fn test_string_lit_1() {
        let (e, v) = parse_string_literal_single("\"hello\"".to_string()).unwrap();
        assert_eq!(e, StringEncoding::Default);
        assert_eq!(v, ['h', 'e', 'l', 'l', 'o']);
    }

    #[test]
    fn test_string_lit_2() {
        let (e, v) = parse_string_literal_single("U\"he\\u1234l\"".to_string()).unwrap();
        assert_eq!(e, StringEncoding::Utf32);
        assert_eq!(v, ['h', 'e', char::from_u32(0x1234).unwrap(), 'l']);
    }

    #[test]
    fn test_string_lit_3() {
        let (e, v) = parse_string_literal_single("u\"he\\\"\\nl\"".to_string()).unwrap();
        assert_eq!(e, StringEncoding::Utf16);
        assert_eq!(v, ['h', 'e', '"', '\n', 'l']);
    }

    #[test]
    fn test_string_lit_4() {
        let (e, v) = parse_string_literal_single("u8\"he'\\\\l\"".to_string()).unwrap();
        assert_eq!(e, StringEncoding::Utf8);
        assert_eq!(v, ['h', 'e', '\'', '\\', 'l']);
    }

    #[test]
    fn test_string_lit_5() {
        let (e, v) = parse_string_literal_single("L\"he\\'\"".to_string()).unwrap();
        assert_eq!(e, StringEncoding::Wchar);
        assert_eq!(v, ['h', 'e', '\'']);
    }

    #[test]
    fn test_string_lit_6() {
        let r = parse_string_literal_single("u9\"he'\\\\l\"".to_string());
        assert_eq!(r, Err(StringParseError::BadPrefix("u9".to_string())));
    }

    #[test]
    fn test_string_lit_7() {
        let r = parse_string_literal_single("U8\"he'\\\\l\"".to_string());
        assert_eq!(r, Err(StringParseError::BadQuote('8')));
    }

    #[test]
    fn test_encode_char_1() {
        let mut buf = Vec::new();
        encode_char(&mut buf, 'A', StringEncoding::Default).unwrap();
        assert_eq!(buf, vec!['A' as u8]);
    }

    #[test]
    fn test_encode_char_2() {
        let mut buf = Vec::new();
        assert!(encode_char(&mut buf, 'ф', StringEncoding::Default).is_err());
    }

    #[test]
    fn test_encode_char_3() {
        let mut buf = Vec::new();
        encode_char(&mut buf, 'ф', StringEncoding::Utf8).unwrap();
        assert_eq!(buf, vec![0xd1, 0x84]);
    }

    #[test]
    fn test_encode_char_4() {
        let mut buf = Vec::new();
        encode_char(&mut buf, 'ф', StringEncoding::Utf16).unwrap();
        assert_eq!(buf, vec![0x44, 0x04]);
    }

    #[test]
    fn test_encode_char_5() {
        let mut buf = Vec::new();
        encode_char(&mut buf, 'ф', StringEncoding::Utf32).unwrap();
        assert_eq!(buf, vec![0x44, 0x04, 0x00, 0x00]);
    }

    #[test]
    fn test_encode_char_6() {
        let mut buf = Vec::new();
        encode_char(&mut buf, '🍌', StringEncoding::Utf16).unwrap();
        assert_eq!(buf, vec![0x3c, 0xd8, 0x4c, 0xdf]);
    }

    #[test]
    fn test_encode_char_7() {
        let mut buf = Vec::new();
        encode_char(&mut buf, '🍌', StringEncoding::Utf32).unwrap();
        assert_eq!(buf, vec![0x4c, 0xf3, 0x01, 0x00]);
    }

    #[test]
    fn test_encode_char_8() {
        let mut buf = Vec::new();
        encode_char(&mut buf, '🍌', StringEncoding::Utf8).unwrap();
        assert_eq!(buf, vec![0xf0, 0x9f, 0x8d, 0x8c]);
    }

    #[test]
    fn test_encode_char_9() {
        let mut buf = Vec::new();
        assert!(encode_char(&mut buf, '🍌', StringEncoding::Wchar).is_err());
    }

    #[test]
    fn test_parse_string_1() {
        let (t, v) =
            parse_string_literal(vec!["\"ф \"".to_string(), "u8\"hello\"".to_string()]).unwrap();
        assert_eq!(t, ctype::CHAR_TYPE);
        assert_eq!(
            v,
            vec![0xd1, 0x84, 0x20, 0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x00]
        );
    }
}
