use std::collections::HashMap;

use crate::ctype::QualifiedType;

#[derive(Debug, Clone)]
pub enum Value {
    Void,
    Int(i128),
    Struct(HashMap<String, Value>),
    Array(Vec<Value>),
}

#[derive(Clone)]
pub struct TypedValue {
    pub t: QualifiedType,
    pub val: Value,
}
