use std::collections::HashSet;

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
    members: Option<Vec<(Option<String>, QualifiedType)>>,
}

impl Tagged {
    pub fn new_struct(members: Option<Vec<(Option<String>, QualifiedType)>>) -> Self {
        Self::Struct(StructUnion { members })
    }

    pub fn new_union(members: Option<Vec<(Option<String>, QualifiedType)>>) -> Self {
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

    /**
     * For a struct or a union, make a set of member names.
     * Names from nested anonymous structs/unions are included recursively.
     */
    pub fn collect_member_names(&self, scope: &NameScope) -> HashSet<String> {
        let mut result = HashSet::new();
        match self {
            Tagged::Struct(su) | Tagged::Union(su) => {
                if let Some(members) = su.members.as_ref() {
                    for (name, t) in members {
                        // names are guaranteed by TypeBuilder to be unique
                        if let Some(name) = name.as_ref() {
                            result.insert(name.to_string());
                        } else if let Some(tti) = t.t.get_anon_struct_or_union_id() {
                            let inner_names =
                                scope.get_tagged_type(tti).collect_member_names(scope);
                            result.extend(inner_names.into_iter());
                        }
                    }
                }
            }
            _ => (),
        }
        result
    }

    /**
     * For a struct or a union, find a field by its name.
     *
     * Returns field offset and its type.
     */
    pub fn get_field(
        &self,
        name: &str,
        scope: &NameScope,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<(u32, QualifiedType), ()> {
        match self {
            Tagged::Struct(su) | Tagged::Union(su) => {
            }
            Tagged::Enum(_) => {

            }
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
                    Some("x".to_string()),
                    QualifiedType {
                        t: CType::Int(2, true),
                        qualifiers: Qualifiers::empty(),
                    },
                ),
                (
                    Some("y".to_string()),
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
                    Some("x".to_string()),
                    QualifiedType {
                        t: CType::Int(2, true),
                        qualifiers: Qualifiers::empty(),
                    },
                ),
                (
                    Some("y".to_string()),
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
