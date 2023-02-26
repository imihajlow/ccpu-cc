use lang_c::{
    ast::StorageClassSpecifier,
    span::{Node, Span},
};
use std::collections::{HashMap, HashSet};

use crate::{
    ctype::{EnumIdentifier, FunctionArgs, QualifiedType, StructUnionIdentifier, StructUnionKind},
    enums::Enum,
    error::{CompileError, CompileWarning, ErrorCollector},
    ir,
    lvalue::{LValue, TypedLValue},
    object_location::ObjectLocation,
    rvalue::{RValue, TypedRValue},
    struct_union::StructUnion,
    utils,
};
use crate::{
    initializer::TypedConstant,
    ir::{GlobalVarId, Reg, VarLocation},
};

/**
 * Keep track of symbols across the whole translation unit.
 * Manages values of symbols and builds the export and import lists.
 */
#[derive(Clone)]
pub struct NameScope {
    last_reg: Reg,
    last_static_id: u32,
    defs: Vec<Scope>,
    static_initializers: HashMap<GlobalVarId, TypedConstant>,
    fixed_regs: HashSet<ir::Reg>,
    tagged_types: Vec<(Tagged, Span)>,
    frame_size: u32,
    return_type: Option<QualifiedType>,
}

#[derive(Clone)]
pub enum Value {
    Type(QualifiedType),
    AutoVar(QualifiedType, Reg),
    Arg(QualifiedType, usize),
    StaticVar(
        QualifiedType,
        GlobalVarId,
        GlobalStorageClass,
        Option<TypedConstant>,
    ),
    Object(QualifiedType, u32),
}

#[derive(Clone)]
struct Scope {
    default: HashMap<String, (Value, Span)>,
    tagged: HashMap<String, usize>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum GlobalStorageClass {
    Default,
    Static,
    Extern,
}

#[derive(Clone)]
enum Tagged {
    Enum(Enum),
    StructUnion(StructUnion),
}

impl NameScope {
    /**
     * All declarations added to a new NameScope are at global level.
     */
    pub fn new() -> Self {
        Self {
            last_reg: 0,
            last_static_id: 0,
            defs: vec![Scope::new()],
            static_initializers: HashMap::new(),
            fixed_regs: HashSet::new(),
            tagged_types: Vec::new(),
            frame_size: 0,
            return_type: None,
        }
    }

    /**
     * Go down one level.
     */
    pub fn push(&mut self) {
        self.defs.push(Scope::new());
    }

    /**
     * Go up one level. Panics if poped above the global level.
     * All initializers for the static variables at the popped level are collected.
     */
    pub fn pop_and_collect_initializers(&mut self) {
        let mut m = self.defs.pop().unwrap();
        if self.defs.len() == 0 {
            panic!("NameScope is popped above the global level");
        }
        for (_key, (val, _span)) in m.default.drain() {
            if let Value::StaticVar(t, id, _, initializer) = val {
                let initializer = initializer.unwrap_or_else(|| TypedConstant::new_default(t));
                self.static_initializers.insert(id, initializer);
            }
        }
    }

    /**
     * Push scope, declare function arguments and save return type.
     */
    pub fn start_function(&mut self, args: &FunctionArgs, return_type: &QualifiedType) {
        let mut defs = HashMap::new();
        if let FunctionArgs::List(l) = args {
            for (i, (t, name, span)) in l.iter().enumerate() {
                if let Some(name) = name {
                    defs.insert(name.to_string(), (Value::Arg(t.clone(), i), *span));
                }
            }
        }
        self.defs.push(Scope::new_with_defs(defs));
        self.return_type = Some(return_type.clone());
    }

    /**
     * Resets frame size to 0 and returns previous size.
     */
    pub fn reset_frame_size(&mut self) -> u32 {
        let r = self.frame_size;
        self.frame_size = 0;
        r
    }

    /**
     * Allocate a temporary variable.
     */
    pub fn alloc_temp(&mut self) -> VarLocation {
        VarLocation::Local(self.alloc_reg())
    }

    /**
     * Get return type of current function.
     */
    pub fn get_return_type(&self) -> QualifiedType {
        self.return_type.as_ref().unwrap().clone()
    }

    /**
     * Declare a global var, a local var or a type alias.
     *
     * The initializer, if given, is cast to the target type.
     */
    pub fn declare(
        &mut self,
        name: &str,
        t: QualifiedType,
        storage_class: &Option<Node<StorageClassSpecifier>>,
        initializer: Option<TypedConstant>,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        let (storage_class, storage_class_span) = if let Some(stc) = storage_class.as_ref() {
            (Some(&stc.node), Some(stc.span))
        } else {
            (None, None)
        };

        if let Some(StorageClassSpecifier::Typedef) = storage_class {
            if initializer.is_some() {
                return ec.record_error(CompileError::TypedefInitialized, span);
            }
            if let Some((old_val, _)) = self.remove_at_same_level(name) {
                if !old_val.is_type() {
                    return ec.record_error(
                        CompileError::ConflictingStorageClass(name.to_string()),
                        span,
                    );
                }
                if &t != old_val.get_type() {
                    return ec.record_error(CompileError::TypeRedefinition(name.to_string()), span);
                }
            }
            self.insert(name, Value::Type(t), span);
        } else if self.is_at_global_level() {
            let storage_class = match GlobalStorageClass::from_ast_storage_class(storage_class) {
                Ok(class) => class,
                Err(_) => return ec.record_error(CompileError::WrongStorageClass, span),
            };
            let initializer = if let Some(tv) = initializer {
                Some(tv.implicit_cast(&t, span, ec)?)
            } else {
                None
            };
            if let Some((old_val, _)) = self.remove_at_same_level(name) {
                // check that both definitions are variables
                if !old_val.is_var() {
                    return ec.record_error(
                        CompileError::ConflictingStorageClass(name.to_string()),
                        span,
                    );
                }

                // check storage classes compatibility
                let old_storage_class = old_val.unwrap_storage_class();
                let mut combined_storage_class =
                    if let Some(class) = match_storage_classes(old_storage_class, storage_class) {
                        class
                    } else {
                        return ec.record_error(
                            CompileError::ConflictingStorageClass(name.to_string()),
                            span,
                        );
                    };

                // check types
                if !t.is_compatible_to(&old_val.get_type(), true) {
                    return ec.record_error(CompileError::ConflictingTypes(name.to_string()), span);
                }

                // check initializers
                let (old_var_id, old_initializer) =
                    if let Value::StaticVar(_, id, _, stclass) = old_val {
                        (id, stclass)
                    } else {
                        unreachable!()
                    };
                if old_initializer.is_some() && initializer.is_some() {
                    return ec.record_error(CompileError::VarRedefinition(name.to_string()), span);
                }
                if initializer.is_some() && combined_storage_class == GlobalStorageClass::Extern {
                    ec.record_warning(
                        CompileWarning::ExternVarInitialized(name.to_string()),
                        span,
                    )?;
                    combined_storage_class = GlobalStorageClass::Default;
                }
                let combined_initializer = initializer.or(old_initializer);

                self.insert(
                    name,
                    Value::StaticVar(t, old_var_id, combined_storage_class, combined_initializer),
                    span,
                );
            } else {
                let var_id = self.alloc_global_var_id(name);
                self.insert(
                    name,
                    Value::StaticVar(t, var_id, storage_class, initializer),
                    span,
                );
            }
        } else {
            // local variable
            if self.exists_at_any_level(name).is_some() {
                ec.record_warning(CompileWarning::LocalVarShadow(name.to_string()), span)?;
            }

            if self.remove_at_same_level(name).is_some() {
                return ec.record_error(CompileError::VarRedefinition(name.to_string()), span);
            }

            match storage_class {
                Some(StorageClassSpecifier::Static) => {
                    if t.t.is_scalar() {
                        let initializer = if let Some(tv) = initializer {
                            Some(tv.implicit_cast(&t, span, ec)?)
                        } else {
                            None
                        };
                        let id = self.alloc_global_var_id(name);
                        self.insert(
                            name,
                            Value::StaticVar(t, id, GlobalStorageClass::Static, initializer),
                            span,
                        );
                    } else {
                        todo!()
                    }
                }
                None
                | Some(StorageClassSpecifier::Auto)
                | Some(StorageClassSpecifier::Register) => {
                    assert!(initializer.is_none());
                    if t.t.is_scalar() {
                        let id = self.alloc_reg();
                        self.insert(name, Value::AutoVar(t, id), span);
                    } else {
                        let size = t.t.sizeof(self, span, ec)?;
                        let align = t.t.alignof(self, span, ec)?;
                        let offset = self.alloc_frame(size, align);
                        self.insert(name, Value::Object(t, offset), span);
                    }
                }
                Some(StorageClassSpecifier::Extern) => {
                    return ec
                        .record_error(CompileError::WrongStorageClass, storage_class_span.unwrap())
                }
                Some(StorageClassSpecifier::ThreadLocal) => unimplemented!(),
                Some(StorageClassSpecifier::Typedef) => unreachable!(), // handled above
            }
        }
        Ok(())
    }

    pub fn declare_struct(
        &mut self,
        name: Option<String>,
        members: Option<Vec<(Option<String>, QualifiedType)>>,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<StructUnionIdentifier, ()> {
        let tagged = Tagged::new_struct(members);
        let id = self.declare_tagged(name.clone(), tagged, span, ec)?;
        Ok(StructUnionIdentifier {
            id,
            name,
            kind: StructUnionKind::Struct,
        })
    }

    pub fn declare_union(
        &mut self,
        name: Option<String>,
        members: Option<Vec<(Option<String>, QualifiedType)>>,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<StructUnionIdentifier, ()> {
        let tagged = Tagged::new_union(members);
        let id = self.declare_tagged(name.clone(), tagged, span, ec)?;
        Ok(StructUnionIdentifier {
            id,
            name,
            kind: StructUnionKind::Union,
        })
    }

    pub fn declare_enum(
        &mut self,
        name: Option<String>,
        values: Option<Vec<(String, i128)>>,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<EnumIdentifier, ()> {
        let tagged = Tagged::new_enum(values);
        let id = self.declare_tagged(name.clone(), tagged, span, ec)?;
        Ok(EnumIdentifier { id, name })
    }

    pub fn get_type_alias(
        &self,
        name: &str,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<&QualifiedType, ()> {
        if let Some(val) = self.get(name) {
            if val.is_type() {
                return Ok(val.get_type());
            } else {
                ec.record_error(CompileError::NotAType(name.to_string()), span)?;
                unreachable!();
            }
        }
        ec.record_error(CompileError::UnknownIdentifier(name.to_string()), span)?;
        unreachable!();
    }

    pub fn get_static_const(
        &self,
        name: &str,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<&TypedConstant, ()> {
        if let Some(val) = self.get(name) {
            if val.is_var() {
                if let Value::StaticVar(t, _, _, initializer) = val {
                    if !t.is_const() || initializer.is_none() {
                        ec.record_error(CompileError::NonConstInConstExpr, span)?;
                        unreachable!();
                    }
                    return Ok(initializer.as_ref().unwrap());
                } else {
                    ec.record_error(CompileError::NonConstInConstExpr, span)?;
                    unreachable!();
                }
            } else {
                ec.record_error(CompileError::NotAVar(name.to_string()), span)?;
                unreachable!();
            }
        }
        ec.record_error(CompileError::UnknownIdentifier(name.to_string()), span)?;
        unreachable!();
    }

    pub fn get_rvalue(
        &self,
        name: &str,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<TypedRValue, ()> {
        if let Some(val) = self.get(name) {
            match val {
                Value::AutoVar(t, r) => Ok(TypedRValue {
                    t: t.clone(),
                    src: RValue::new_var(VarLocation::Local(*r)),
                }),
                Value::Arg(t, n) => Ok(TypedRValue {
                    t: t.clone(),
                    src: RValue::new_var(VarLocation::Arg(*n)),
                }),
                Value::StaticVar(t, id, _, _) => Ok(TypedRValue {
                    t: t.clone(),
                    src: RValue::new_var(VarLocation::Global(id.clone())),
                }),
                Value::Type(_) => {
                    ec.record_error(CompileError::NotAVar(name.to_string()), span)?;
                    unreachable!();
                }
                Value::Object(t, p) => Ok(TypedRValue {
                    t: t.clone(),
                    src: RValue::new_object(ObjectLocation::PointedBy(ir::Scalar::FrameOffset(*p))),
                }),
            }
        } else {
            ec.record_error(CompileError::UnknownIdentifier(name.to_string()), span)?;
            unreachable!();
        }
    }

    pub fn get_lvalue(
        &self,
        name: &str,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<TypedLValue, ()> {
        if let Some(val) = self.get(name) {
            match val {
                Value::AutoVar(t, r) => Ok(TypedLValue {
                    t: t.clone(),
                    lv: LValue::Var(VarLocation::Local(*r)),
                }),
                Value::Arg(t, n) => Ok(TypedLValue {
                    t: t.clone(),
                    lv: LValue::Var(VarLocation::Arg(*n)),
                }),
                Value::StaticVar(t, id, _, _) => Ok(TypedLValue {
                    t: t.clone(),
                    lv: LValue::Var(VarLocation::Global(id.clone())),
                }),
                Value::Type(_) => {
                    ec.record_error(CompileError::NotAVar(name.to_string()), span)?;
                    unreachable!();
                }
                Value::Object(t, p) => Ok(TypedLValue {
                    t: t.clone(),
                    lv: LValue::Object(ObjectLocation::PointedBy(ir::Scalar::FrameOffset(*p))),
                }),
            }
        } else {
            ec.record_error(CompileError::UnknownIdentifier(name.to_string()), span)?;
            unreachable!();
        }
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        for m in self.defs.iter().rev() {
            if let Some((val, _)) = m.default.get(name) {
                return Some(val);
            }
        }
        return None;
    }

    pub fn get_struct_union(&self, id: &StructUnionIdentifier) -> &StructUnion {
        if let Tagged::StructUnion(su) = self.get_tagged_type(id.id) {
            assert_eq!(id.kind, su.get_kind());
            su
        } else {
            panic!("wrong struct/union id")
        }
    }

    pub fn get_enum(&self, id: &EnumIdentifier) -> &Enum {
        if let Tagged::Enum(en) = self.get_tagged_type(id.id) {
            en
        } else {
            panic!("wrong struct/union id")
        }
    }

    pub fn fix_in_memory(&mut self, var: &VarLocation) {
        match var {
            VarLocation::Global(_) => (),
            VarLocation::Local(n) => {
                self.fixed_regs.insert(*n);
            }
            VarLocation::Arg(_) => (),
            VarLocation::Frame(_) => (),
            VarLocation::Return => (),
        }
    }

    #[cfg(test)]
    pub fn get_fixed_regs(&self) -> &HashSet<ir::Reg> {
        &self.fixed_regs
    }

    fn get_tagged_type(&self, id: usize) -> &Tagged {
        let result = &self.tagged_types[id].0;
        result
    }

    fn alloc_reg(&mut self) -> Reg {
        let r = self.last_reg;
        self.last_reg += 1;
        r
    }

    fn alloc_global_var_id(&mut self, name: &str) -> GlobalVarId {
        let id = self.last_static_id;
        self.last_static_id += 1;
        GlobalVarId(name.to_string(), id)
    }

    fn alloc_frame(&mut self, size: u32, align: u32) -> u32 {
        self.frame_size = utils::align(self.frame_size, align);
        let r = self.frame_size;
        self.frame_size += size;
        r
    }

    fn is_at_global_level(&self) -> bool {
        self.defs.len() == 1
    }

    fn remove_at_same_level(&mut self, key: &str) -> Option<(Value, Span)> {
        self.defs.last_mut().unwrap().default.remove(key)
    }

    fn exists_at_any_level(&self, key: &str) -> Option<Span> {
        for m in self.defs.iter().rev() {
            if let Some((_val, span)) = m.default.get(key) {
                return Some(*span);
            }
        }
        None
    }

    fn insert(&mut self, key: &str, val: Value, span: Span) {
        self.defs
            .last_mut()
            .unwrap()
            .default
            .insert(key.to_string(), (val, span));
    }

    fn push_tagged(&mut self, name: Option<String>, val: Tagged, span: Span) -> usize {
        self.tagged_types.push((val, span));
        let id = self.tagged_types.len() - 1;
        if let Some(name) = name {
            self.defs.last_mut().unwrap().tagged.insert(name, id);
        }
        id
    }

    fn declare_tagged(
        &mut self,
        name: Option<String>,
        val: Tagged,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<usize, ()> {
        let id = if let Some(name) = &name {
            if let Some((id, lvl)) = self.find_tagged_id_and_level(&name) {
                let prev = &self.tagged_types[id].0;
                if !prev.is_same_tag_as(&val) {
                    ec.record_error(
                        CompileError::RedefinitionWithDifferentTag(name.to_string()),
                        span,
                    )?;
                    unreachable!()
                }
                if !val.is_complete() {
                    id
                } else if prev.is_complete() {
                    if lvl == 0 {
                        // two same types on the same level -> error
                        ec.record_error(
                            CompileError::TaggedTypedRedefinition(name.to_string()),
                            span,
                        )?;
                        unreachable!();
                    } else {
                        // distinct new type
                        self.push_tagged(Some(name.to_string()), val, span)
                    }
                } else {
                    self.tagged_types[id] = (val, span);
                    id
                }
            } else {
                self.push_tagged(Some(name.to_string()), val, span)
            }
        } else {
            self.push_tagged(None, val, span)
        };
        Ok(id)
    }

    /**
     * If name has been declared, return id and level where level 0 is current level.
     */
    fn find_tagged_id_and_level(&self, name: &str) -> Option<(usize, usize)> {
        for (lvl, m) in self.defs.iter().rev().enumerate() {
            if let Some(id) = m.tagged.get(name) {
                return Some((*id, lvl));
            }
        }
        None
    }

    fn find_tagged_id(&self, name: &str) -> Option<usize> {
        self.find_tagged_id_and_level(name).map(|(id, _)| id)
    }
}

impl Value {
    pub fn is_var(&self) -> bool {
        match self {
            Value::Type(_) => false,
            _ => true,
        }
    }

    pub fn is_type(&self) -> bool {
        !self.is_var()
    }

    pub fn get_type(&self) -> &QualifiedType {
        match self {
            Value::Type(t) => t,
            Value::AutoVar(t, _) => t,
            Value::Arg(t, _) => t,
            Value::StaticVar(t, _, _, _) => t,
            Value::Object(t, _) => t,
        }
    }

    pub fn unwrap_storage_class(&self) -> GlobalStorageClass {
        if let Value::StaticVar(_, _, class, _) = self {
            *class
        } else {
            panic!("Can only unwrap storage class for a static variable");
        }
    }

    #[cfg(test)]
    pub fn unwrap_static_var(
        &self,
    ) -> (
        &QualifiedType,
        &GlobalVarId,
        &GlobalStorageClass,
        &Option<TypedConstant>,
    ) {
        if let Value::StaticVar(t, id, stcl, init) = self {
            (t, id, stcl, init)
        } else {
            panic!("Not a static var")
        }
    }
}

impl GlobalStorageClass {
    pub fn from_ast_storage_class(class: Option<&StorageClassSpecifier>) -> Result<Self, ()> {
        match class {
            None => Ok(GlobalStorageClass::Default),
            Some(StorageClassSpecifier::Extern) => Ok(GlobalStorageClass::Extern),
            Some(StorageClassSpecifier::Static) => Ok(GlobalStorageClass::Static),
            Some(_) => Err(()),
        }
    }
}

impl Scope {
    fn new() -> Self {
        Self {
            default: HashMap::new(),
            tagged: HashMap::new(),
        }
    }

    fn new_with_defs(defs: HashMap<String, (Value, Span)>) -> Self {
        Self {
            default: defs,
            tagged: HashMap::new(),
        }
    }
}

impl Tagged {
    fn new_struct(members: Option<Vec<(Option<String>, QualifiedType)>>) -> Self {
        Self::StructUnion(StructUnion::new_struct(members))
    }

    fn new_union(members: Option<Vec<(Option<String>, QualifiedType)>>) -> Self {
        Self::StructUnion(StructUnion::new_union(members))
    }

    fn new_enum(values: Option<Vec<(String, i128)>>) -> Self {
        Self::Enum(Enum::new(values))
    }

    fn is_complete(&self) -> bool {
        match self {
            Tagged::Enum(e) => e.is_complete(),
            Tagged::StructUnion(s) => s.is_complete(),
        }
    }

    fn is_same_tag_as(&self, other: &Self) -> bool {
        match (self, other) {
            (Tagged::Enum(_), Tagged::Enum(_)) => true,
            (Tagged::StructUnion(s), Tagged::StructUnion(o)) => s.get_kind() == o.get_kind(),
            _ => false,
        }
    }
}

fn match_storage_classes(
    old: GlobalStorageClass,
    new: GlobalStorageClass,
) -> Option<GlobalStorageClass> {
    use GlobalStorageClass::*;
    // same classes match
    // static may not follow non-static
    // extern may follow static
    match (old, new) {
        (Static, Static) => Some(Static),
        (Extern, Extern) => Some(Extern),
        (_, Static) => None,
        (Static, Extern) => Some(Static),
        (Static, Default) => None,
        (Default, _) => Some(Default),
        (_, Default) => Some(Default),
    }
}

#[cfg(test)]
mod test {
    use crate::translation_unit::TranslationUnit;

    use super::*;

    fn compile(code: &str) -> (TranslationUnit, ErrorCollector) {
        use lang_c::driver::{parse_preprocessed, Config, Flavor};
        let mut cfg = Config::default();
        cfg.flavor = Flavor::StdC11;
        let p = parse_preprocessed(&cfg, code.to_string()).unwrap();
        let mut ec = ErrorCollector::new();
        let tu = TranslationUnit::translate(p.unit, &mut ec).unwrap();
        assert_eq!(ec.get_error_count(), 0);
        (tu, ec)
    }

    fn get_first_body(tu: &TranslationUnit) -> &Vec<ir::Block> {
        tu.functions[0].get_body()
    }

    #[test]
    fn test_struct_1() {
        let (tu, ec) =
            compile("struct X { int x; }; void foo(void) { struct X x; struct X *p = &x; }");
        ec.print_issues();
        assert_eq!(ec.get_warning_count(), 0);
        let body = get_first_body(&tu);
        assert_eq!(body.len(), 1);
    }
}
