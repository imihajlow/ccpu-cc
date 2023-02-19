use crate::ctype::QualifiedType;
use crate::ctype::TaggedTypeKind;

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
        Self::Struct(StructUnion { members })
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
}
