use lang_c::{
    ast::StorageClassSpecifier,
    span::{Node, Span},
};
use std::collections::{HashMap, HashSet};

use crate::{
    ctype::{FunctionArgs, QualifiedType},
    error::{CompileError, CompileWarning, ErrorCollector},
    ir,
};
use crate::{
    initializer::TypedValue,
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
    defs: Vec<HashMap<Namespace, (Value, Span)>>,
    static_initializers: HashMap<GlobalVarId, TypedValue>,
    fixed_regs: HashSet<ir::Reg>,
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
        Option<TypedValue>,
    ),
}

#[derive(Eq, Hash, PartialEq, Clone)]
enum Namespace {
    Default(String),
    Tag(String),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum GlobalStorageClass {
    Default,
    Static,
    Extern,
}

impl NameScope {
    /**
     * All declarations added to a new NameScope are at global level.
     */
    pub fn new() -> Self {
        Self {
            last_reg: 0,
            last_static_id: 0,
            defs: vec![HashMap::new()],
            static_initializers: HashMap::new(),
            fixed_regs: HashSet::new(),
        }
    }

    /**
     * Go down one level.
     */
    pub fn push(&mut self) {
        self.defs.push(HashMap::new());
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
        for (_key, (val, _span)) in m.drain() {
            if let Value::StaticVar(t, id, _, initializer) = val {
                let initializer = initializer.unwrap_or_else(|| TypedValue::new_default(t));
                self.static_initializers.insert(id, initializer);
            }
        }
    }

    /**
     * Push scope and declare function arguments.
     */
    pub fn start_function(&mut self, args: &FunctionArgs) {
        let mut defs = HashMap::new();
        if let FunctionArgs::List(l) = args {
            for (i, (t, name, span)) in l.iter().enumerate() {
                if let Some(name) = name {
                    defs.insert(
                        Namespace::Default(name.to_string()),
                        (Value::Arg(t.clone(), i), *span),
                    );
                }
            }
        }
        self.defs.push(defs);
    }

    /**
     * Allocate a temporary variable.
     */
    pub fn alloc_temp(&mut self) -> VarLocation {
        VarLocation::Local(self.alloc_reg())
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
        initializer: Option<TypedValue>,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        let (storage_class, storage_class_span) = if let Some(stc) = storage_class.as_ref() {
            (Some(&stc.node), Some(stc.span))
        } else {
            (None, None)
        };

        let key = Namespace::Default(name.to_string());
        if let Some(StorageClassSpecifier::Typedef) = storage_class {
            if initializer.is_some() {
                return ec.record_error(CompileError::TypedefInitialized, span);
            }
            if let Some((old_val, _)) = self.remove_at_same_level(&key) {
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
            self.insert(key, Value::Type(t), span);
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
            if let Some((old_val, _)) = self.remove_at_same_level(&key) {
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
                    key,
                    Value::StaticVar(t, old_var_id, combined_storage_class, combined_initializer),
                    span,
                );
            } else {
                let var_id = self.alloc_global_var_id(name);
                self.insert(
                    key,
                    Value::StaticVar(t, var_id, storage_class, initializer),
                    span,
                );
            }
        } else {
            // local variable
            if self.exists_at_any_level(&key).is_some() {
                ec.record_warning(CompileWarning::LocalVarShadow(name.to_string()), span)?;
            }

            if self.remove_at_same_level(&key).is_some() {
                return ec.record_error(CompileError::VarRedefinition(name.to_string()), span);
            }

            match storage_class {
                Some(StorageClassSpecifier::Static) => {
                    let initializer = if let Some(tv) = initializer {
                        Some(tv.implicit_cast(&t, span, ec)?)
                    } else {
                        None
                    };
                    let id = self.alloc_global_var_id(name);
                    self.insert(
                        key,
                        Value::StaticVar(t, id, GlobalStorageClass::Static, initializer),
                        span,
                    );
                }
                None
                | Some(StorageClassSpecifier::Auto)
                | Some(StorageClassSpecifier::Register) => {
                    assert!(initializer.is_none());
                    let id = self.alloc_reg();
                    self.insert(key, Value::AutoVar(t, id), span);
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
    ) -> Result<&TypedValue, ()> {
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

    pub fn get_var(
        &self,
        name: &str,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<(&QualifiedType, VarLocation), ()> {
        if let Some(val) = self.get(name) {
            match val {
                Value::AutoVar(t, r) => Ok((t, VarLocation::Local(*r))),
                Value::Arg(t, n) => Ok((t, VarLocation::Arg(*n))),
                Value::StaticVar(t, id, _, _) => Ok((t, VarLocation::Global(id.clone()))),
                Value::Type(_) => {
                    ec.record_error(CompileError::NotAVar(name.to_string()), span)?;
                    unreachable!();
                }
            }
        } else {
            ec.record_error(CompileError::UnknownIdentifier(name.to_string()), span)?;
            unreachable!();
        }
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        let key = Namespace::Default(name.to_string());
        for m in self.defs.iter().rev() {
            if let Some((val, _)) = m.get(&key) {
                return Some(val);
            }
        }
        return None;
    }

    pub fn fix_in_memory(&mut self, var: &VarLocation) {
        match var {
            VarLocation::Global(_) => (),
            VarLocation::Local(n) => {
                self.fixed_regs.insert(*n);
            }
            VarLocation::Arg(_) => (),
        }
    }

    #[cfg(test)]
    pub fn get_fixed_regs(&self) -> &HashSet<ir::Reg> {
        &self.fixed_regs
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

    fn is_at_global_level(&self) -> bool {
        self.defs.len() == 1
    }

    fn remove_at_same_level(&mut self, ns: &Namespace) -> Option<(Value, Span)> {
        self.defs.last_mut().unwrap().remove(ns)
    }

    fn exists_at_any_level(&self, ns: &Namespace) -> Option<Span> {
        for m in self.defs.iter().rev() {
            if let Some((_val, span)) = m.get(ns) {
                return Some(*span);
            }
        }
        None
    }

    fn insert(&mut self, key: Namespace, val: Value, span: Span) {
        self.defs.last_mut().unwrap().insert(key, (val, span));
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
        &Option<TypedValue>,
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
