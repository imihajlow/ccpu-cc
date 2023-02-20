use lang_c::span::Span;

use crate::ctype::QualifiedType;
use crate::ctype::TaggedTypeKind;
use crate::error::CompileError;
use crate::error::ErrorCollector;
use crate::machine;
use crate::name_scope::NameScope;

#[derive(Clone)]
pub enum Tagged {
    Enum(Enum),
    Struct(StructUnion),
    Union(StructUnion),
}

#[derive(Clone)]
pub struct Enum {
    values: Option<Vec<(String, i128)>>,
}

#[derive(Clone)]
pub struct StructUnion {
    members: Option<Vec<(String, QualifiedType)>>,
}

impl Tagged {
    pub fn new_struct(members: Option<Vec<(String, QualifiedType)>>) -> Self {
        Self::Struct(StructUnion { members })
    }

    pub fn new_union(members: Option<Vec<(String, QualifiedType)>>) -> Self {
        Self::Union(StructUnion { members })
    }

    pub fn new_enum(values: Option<Vec<(String, i128)>>) -> Self {
        Self::Enum(Enum { values })
    }

    pub fn is_complete(&self) -> bool {
        match self {
            Tagged::Enum(e) => e.values.is_some(),
            Tagged::Struct(s) | Tagged::Union(s) => s.members.is_some(),
        }
    }

    pub fn is_same_tag_as(&self, other: &Self) -> bool {
        match (self, other) {
            (Tagged::Enum(_), Tagged::Enum(_))
            | (Tagged::Struct(_), Tagged::Struct(_))
            | (Tagged::Union(_), Tagged::Union(_)) => true,
            _ => false,
        }
    }

    pub fn get_kind(&self) -> TaggedTypeKind {
        match self {
            Tagged::Enum(_) => TaggedTypeKind::Enum,
            Tagged::Struct(_) => TaggedTypeKind::Struct,
            Tagged::Union(_) => TaggedTypeKind::Union,
        }
    }

    pub fn alignof(
        &self,
        scope: &NameScope,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<u32, ()> {
        match self {
            Tagged::Enum(_) => Ok(machine::INT_ALIGN),
            Tagged::Struct(su) => alignof_struct(su, scope, span, ec),
            Tagged::Union(su) => alignof_union(su, scope, span, ec),
        }
    }

    pub fn sizeof(
        &self,
        scope: &NameScope,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<u32, ()> {
        match self {
            Tagged::Enum(_) => Ok(machine::INT_SIZE as u32),
            Tagged::Struct(su) => sizeof_struct(su, scope, span, ec),
            Tagged::Union(su) => sizeof_union(su, scope, span, ec),
        }
    }
}

fn alignof_struct(
    su: &StructUnion,
    scope: &NameScope,
    span: Span,
    ec: &mut ErrorCollector,
) -> Result<u32, ()> {
    let members = su
        .members
        .as_ref()
        .expect("complete types only at this point");
    members
        .first()
        .map(|(_, t)| t.t.alignof(scope, span, ec))
        .unwrap_or(Ok(0))
}

fn alignof_union(
    su: &StructUnion,
    scope: &NameScope,
    span: Span,
    ec: &mut ErrorCollector,
) -> Result<u32, ()> {
    let members = su
        .members
        .as_ref()
        .expect("complete types only at this point");
    let mut align = 1;
    for (_, t) in members {
        align = num::integer::lcm(align, t.t.alignof(scope, span, ec)?);
    }
    Ok(align)
}

fn sizeof_struct(
    su: &StructUnion,
    scope: &NameScope,
    span: Span,
    ec: &mut ErrorCollector,
) -> Result<u32, ()> {
    let members = su
        .members
        .as_ref()
        .expect("complete types only at this point");
    let mut size = 0;
    for (_, t) in members {
        let align = t.t.alignof(scope, span, ec)?;
        let rem = size % align;
        if rem != 0 {
            size += align - rem;
        }
        size += t.t.sizeof(scope, span, ec)?;
    }
    Ok(size)
}

fn sizeof_union(
    su: &StructUnion,
    scope: &NameScope,
    span: Span,
    ec: &mut ErrorCollector,
) -> Result<u32, ()> {
    let members = su
        .members
        .as_ref()
        .expect("complete types only at this point");
    let mut size = 0;
    for (_, t) in members {
        size = std::cmp::max(size, t.t.sizeof(scope, span, ec)?);
    }
    Ok(size)
}

#[cfg(test)]
mod test {
    use crate::ctype::{CType, Qualifiers};

    use super::*;

    #[test]
    fn test_align() {
        let su = StructUnion {
            members: Some(vec![
                (
                    "x".to_string(),
                    QualifiedType {
                        t: CType::Int(2, true),
                        qualifiers: Qualifiers::empty(),
                    },
                ),
                (
                    "y".to_string(),
                    QualifiedType {
                        t: CType::Int(4, true),
                        qualifiers: Qualifiers::empty(),
                    },
                ),
            ]),
        };
        let st = Tagged::Struct(su.clone()); // struct { int x; long y; };
        let un = Tagged::Union(su); // union { int x; long y; };
        let scope = NameScope::new();
        let mut ec = &mut ErrorCollector::new();
        assert_eq!(st.alignof(&scope, Span::none(), &mut ec), Ok(2));
        assert_eq!(un.alignof(&scope, Span::none(), &mut ec), Ok(4));
    }

    #[test]
    fn test_sizeof() {
        let su = StructUnion {
            members: Some(vec![
                (
                    "x".to_string(),
                    QualifiedType {
                        t: CType::Int(2, true),
                        qualifiers: Qualifiers::empty(),
                    },
                ),
                (
                    "y".to_string(),
                    QualifiedType {
                        t: CType::Int(4, true),
                        qualifiers: Qualifiers::empty(),
                    },
                ),
            ]),
        };
        let st = Tagged::Struct(su.clone()); // struct { int x; long y; };
        let un = Tagged::Union(su); // union { int x; long y; };
        let scope = NameScope::new();
        let mut ec = &mut ErrorCollector::new();
        assert_eq!(st.sizeof(&scope, Span::none(), &mut ec), Ok(8));
        assert_eq!(un.sizeof(&scope, Span::none(), &mut ec), Ok(4));
    }
}
