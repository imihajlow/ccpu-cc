use std::collections::HashSet;

use lang_c::span::Span;

use crate::ctype::QualifiedType;
use crate::ctype::StructUnionKind;
use crate::error::{CompileError, ErrorCollector};
use crate::name_scope::NameScope;
use crate::utils;

#[derive(Clone)]
pub struct StructUnion {
    kind: StructUnionKind,
    members: Option<Vec<(Option<String>, QualifiedType)>>,
}

impl StructUnion {
    pub fn new_struct(members: Option<Vec<(Option<String>, QualifiedType)>>) -> Self {
        Self {
            members,
            kind: StructUnionKind::Struct,
        }
    }

    pub fn new_union(members: Option<Vec<(Option<String>, QualifiedType)>>) -> Self {
        Self {
            members,
            kind: StructUnionKind::Union,
        }
    }

    pub fn alignof(
        &self,
        scope: &NameScope,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<u32, ()> {
        match self.kind {
            StructUnionKind::Struct => self.alignof_struct(scope, span, ec),
            StructUnionKind::Union => self.alignof_union(scope, span, ec),
        }
    }

    pub fn sizeof(
        &self,
        scope: &NameScope,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<u32, ()> {
        match self.kind {
            StructUnionKind::Struct => self.sizeof_struct(scope, span, ec),
            StructUnionKind::Union => self.sizeof_union(scope, span, ec),
        }
    }

    /**
     * Find a field by its name.
     *
     * Returns field offset and its type.
     * Returns `Ok(None)` if not found.
     * Returns `Err(())` on error (e.g. incomplete types).
     */
    pub fn get_field(
        &self,
        name: &str,
        scope: &NameScope,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<Option<(u32, QualifiedType)>, ()> {
        assert!(self.is_complete());
        match self.kind {
            StructUnionKind::Struct => self.get_field_struct(name, scope, span, ec),
            StructUnionKind::Union => self.get_field_union(name, scope, span, ec),
        }
    }

    pub fn get_kind(&self) -> StructUnionKind {
        self.kind
    }

    pub fn is_complete(&self) -> bool {
        self.members.is_some()
    }

    /**
     * Make a set of member names.
     * Names from nested anonymous structs/unions are included recursively.
     */
    pub fn collect_member_names(&self, scope: &NameScope) -> HashSet<String> {
        let mut result = HashSet::new();
        if let Some(members) = self.members.as_ref() {
            for (name, t) in members {
                // names are guaranteed by TypeBuilder to be unique
                if let Some(name) = name.as_ref() {
                    result.insert(name.to_string());
                } else if let Some(tti) = t.t.get_anon_struct_or_union_id() {
                    let inner_names = scope.get_struct_union(tti).collect_member_names(scope);
                    result.extend(inner_names.into_iter());
                }
            }
        }
        result
    }

    fn alignof_struct(
        &self,
        scope: &NameScope,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<u32, ()> {
        let members = self
            .members
            .as_ref()
            .expect("complete types only at this point");
        members
            .first()
            .map(|(_, t)| t.t.alignof(scope, span, ec))
            .unwrap_or(Ok(0))
    }

    fn alignof_union(
        &self,
        scope: &NameScope,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<u32, ()> {
        let members = self
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
        &self,
        scope: &NameScope,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<u32, ()> {
        let members = self
            .members
            .as_ref()
            .expect("complete types only at this point");
        let mut size = 0;
        for (_, t) in members {
            let align = t.t.alignof(scope, span, ec)?;
            size = utils::align(size, align);
            size += t.t.sizeof(scope, span, ec)?;
        }
        Ok(size)
    }

    fn sizeof_union(
        &self,
        scope: &NameScope,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<u32, ()> {
        let members = self
            .members
            .as_ref()
            .expect("complete types only at this point");
        let mut size = 0;
        for (_, t) in members {
            size = std::cmp::max(size, t.t.sizeof(scope, span, ec)?);
        }
        Ok(size)
    }

    fn get_field_struct(
        &self,
        name: &str,
        scope: &NameScope,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<Option<(u32, QualifiedType)>, ()> {
        let members = self
            .members
            .as_ref()
            .expect("complete types only at this point");
        let mut offset = 0;
        for (member_name, t) in members {
            let align = t.t.alignof(scope, span, ec)?;
            offset = utils::align(offset, align);
            if let Some(member_name) = member_name.as_ref() {
                if member_name == name {
                    return Ok(Some((offset, t.clone())));
                }
            } else if let Some(id) = t.t.get_anon_struct_or_union_id() {
                let nested = scope.get_struct_union(id);
                if let Some((nested_offset, t)) = nested.get_field(name, scope, span, ec)? {
                    return Ok(Some((offset + nested_offset, t)));
                }
            }
            offset += t.t.sizeof(scope, span, ec)?;
        }
        Ok(None)
    }

    fn get_field_union(
        &self,
        name: &str,
        scope: &NameScope,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<Option<(u32, QualifiedType)>, ()> {
        let members = self
            .members
            .as_ref()
            .expect("complete types only at this point");
        for (member_name, t) in members {
            if let Some(member_name) = member_name.as_ref() {
                if member_name == name {
                    return Ok(Some((0, t.clone())));
                }
            } else if let Some(id) = t.t.get_anon_struct_or_union_id() {
                let nested = scope.get_struct_union(id);
                if let Some(r) = nested.get_field(name, scope, span, ec)? {
                    return Ok(Some(r));
                }
            }
        }
        Ok(None)
    }
}

#[cfg(test)]
mod test {
    use crate::ctype::{CType, Qualifiers};

    use super::*;

    #[test]
    fn test_align() {
        let mut su = StructUnion {
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
            kind: StructUnionKind::Struct,
        };
        let mut ec = &mut ErrorCollector::new();
        let scope = NameScope::new();
        assert_eq!(su.alignof(&scope, Span::none(), &mut ec), Ok(2)); // struct { int x; long y; };
        su.kind = StructUnionKind::Union;
        assert_eq!(su.alignof(&scope, Span::none(), &mut ec), Ok(4)); // union { int x; long y; };
    }

    #[test]
    fn test_sizeof() {
        let mut su = StructUnion {
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
            kind: StructUnionKind::Struct,
        };
        let mut ec = &mut ErrorCollector::new();
        let scope = NameScope::new();
        assert_eq!(su.sizeof(&scope, Span::none(), &mut ec), Ok(8)); // struct { int x; long y; };
        su.kind = StructUnionKind::Union;
        assert_eq!(su.sizeof(&scope, Span::none(), &mut ec), Ok(4)); // union { int x; long y; };
    }

    #[test]
    fn test_field_1() {
        let mut su = StructUnion {
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
            kind: StructUnionKind::Struct,
        };
        let mut ec = &mut ErrorCollector::new();
        let scope = NameScope::new();
        // struct { int x; long y; };
        assert_eq!(
            su.get_field("x", &scope, Span::none(), &mut ec).unwrap(),
            Some((
                0,
                QualifiedType {
                    t: CType::Int(2, true),
                    qualifiers: Qualifiers::empty(),
                }
            ))
        );
        assert_eq!(
            su.get_field("y", &scope, Span::none(), &mut ec).unwrap(),
            Some((
                4,
                QualifiedType {
                    t: CType::Int(4, true),
                    qualifiers: Qualifiers::empty(),
                }
            ))
        );
        assert_eq!(su.get_field("z", &scope, Span::none(), ec), Ok(None));

        // union { int x; long y; };
        su.kind = StructUnionKind::Union;
        assert_eq!(
            su.get_field("x", &scope, Span::none(), &mut ec).unwrap(),
            Some((
                0,
                QualifiedType {
                    t: CType::Int(2, true),
                    qualifiers: Qualifiers::empty(),
                }
            ))
        );
        assert_eq!(
            su.get_field("y", &scope, Span::none(), &mut ec).unwrap(),
            Some((
                0,
                QualifiedType {
                    t: CType::Int(4, true),
                    qualifiers: Qualifiers::empty(),
                }
            ))
        );
        assert_eq!(su.get_field("z", &scope, Span::none(), ec), Ok(None));
    }
}
