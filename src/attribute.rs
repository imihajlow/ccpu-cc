use crate::error::CompileError;
use crate::error::CompileWarning;
use crate::error::ErrorCollector;
use crate::string::parse_string_literal;
use lang_c::ast::Expression;
use lang_c::span::Span;

#[derive(Debug, Clone)]
pub enum Attribute {
    Section(String),
    Packed,
}

#[derive(Debug, Clone)]
pub struct Attributes {
    section: Option<String>,
    packed: bool,
}

impl Attribute {
    pub fn parse(
        mut attr: lang_c::ast::Attribute,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<Option<Self>, ()> {
        match &attr.name.node[..] {
            "packed" => Ok(Some(Attribute::Packed)),
            "section" => {
                if attr.arguments.len() != 1 {
                    ec.record_warning(
                        CompileWarning::WrongAttributeParameters(
                            attr.name.node.to_string(),
                            "must be 1 parameter".to_string(),
                        ),
                        span,
                    )?;
                    return Ok(None);
                }
                let arg = attr.arguments.pop().unwrap();
                if let Expression::StringLiteral(s) = arg.node {
                    let (_, name) = parse_string_literal(s.node).map_err(|e| {
                        ec.record_error(CompileError::StringParseError(e), s.span)
                            .err()
                            .unwrap()
                    })?;
                    let name = match String::from_utf8(name) {
                        Ok(s) => s,
                        Err(e) => {
                            ec.record_error(CompileError::FromUtf8Error(e), s.span)?;
                            unreachable!();
                        }
                    };
                    Ok(Some(Attribute::Section(name)))
                } else {
                    ec.record_warning(
                        CompileWarning::WrongAttributeParameters(
                            attr.name.node.to_string(),
                            "not a string literal".to_string(),
                        ),
                        span,
                    )?;
                    return Ok(None);
                }
            }
            x => {
                ec.record_warning(
                    CompileWarning::Unimplemented(format!("attribute {}", x)),
                    span,
                )?;
                Ok(None)
            }
        }
    }
}

impl Attributes {
    pub fn new() -> Self {
        Self {
            section: None,
            packed: false,
        }
    }

    pub fn is_packed(&self) -> bool {
        self.packed
    }

    pub fn get_section(&self) -> Option<&str> {
        self.section.as_ref().map(|x| x.as_str())
    }

    pub fn add_attribute(
        &mut self,
        attr: Attribute,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        match attr {
            Attribute::Packed => self.packed = true,
            Attribute::Section(new_section) => {
                if self.section.is_some() {
                    ec.record_warning(CompileWarning::SectionOverridden, span)?;
                }
                self.section = Some(new_section)
            }
        }
        Ok(())
    }

    pub fn update(
        &mut self,
        new_attrs: Attributes,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        self.packed |= new_attrs.packed;
        self.section = match (self.section.take(), new_attrs.section) {
            (None, x) => x,
            (Some(s), None) => Some(s),
            (Some(old), Some(new)) if old == new => Some(old),
            (Some(_), Some(new)) => {
                ec.record_warning(CompileWarning::SectionOverridden, span)?;
                Some(new)
            }
        };
        Ok(())
    }
}
