use crate::constant::compute_constant_expr;
use crate::error::CompileError;
use crate::error::CompileWarning;
use crate::error::ErrorCollector;
use crate::machine;
use crate::name_scope::NameScope;
use crate::string::parse_string_literal;
use lang_c::ast::Expression;
use lang_c::span::Span;

#[derive(Debug, Clone)]
pub enum Attribute {
    Section(String),
    Packed,
    Aligned(Option<u32>),
}

#[derive(Debug, Clone)]
pub struct Attributes {
    section: Option<String>,
    packed: bool,
    aligned: Option<u32>,
}

impl Attribute {
    pub fn parse(
        mut attr: lang_c::ast::Attribute,
        scope: &mut NameScope,
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
                    let (_, mut name) = parse_string_literal(s.node).map_err(|e| {
                        ec.record_error(CompileError::StringParseError(e), s.span)
                            .err()
                            .unwrap()
                    })?;
                    name.pop(); // discard terminating zero
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
            "aligned" => {
                if attr.arguments.is_empty() {
                    return Ok(Some(Attribute::Aligned(None)));
                }
                if attr.arguments.len() != 1 {
                    ec.record_warning(
                        CompileWarning::WrongAttributeParameters(
                            attr.name.node.to_string(),
                            "must be 0 or 1 parameter".to_string(),
                        ),
                        span,
                    )?;
                    return Ok(None);
                }
                let arg = attr.arguments.pop().unwrap();
                let arg_span = arg.span;
                let r = compute_constant_expr(arg, true, scope, ec)?;
                if !r.t.t.is_integer() {
                    ec.record_error(CompileError::IntegerTypeRequired, arg_span)?;
                    unreachable!();
                }
                Ok(Some(Attribute::Aligned(Some(r.unwrap_integer() as u32))))
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
            aligned: None,
        }
    }

    pub fn is_packed(&self) -> bool {
        self.packed
    }

    pub fn get_section(&self) -> Option<&str> {
        self.section.as_ref().map(|x| x.as_str())
    }

    pub fn get_align(&self, default: u32) -> u32 {
        self.aligned.unwrap_or(default)
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
            Attribute::Aligned(None) => {
                self.aligned = Some(machine::LLONG_ALIGN);
            }
            Attribute::Aligned(Some(x)) => {
                self.aligned = Some(x);
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
