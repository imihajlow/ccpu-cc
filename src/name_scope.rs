use lang_c::{
    ast::StorageClassSpecifier,
    span::{Node, Span},
};

use std::collections::HashMap;

use crate::{
    attribute::Attributes,
    builtin::{get_builtin_function, is_builtin_name, BuiltinFunction},
    ctype::{EnumIdentifier, FunctionArgs, QualifiedType, StructUnionIdentifier, StructUnionKind},
    enums::Enum,
    error::{CompileError, CompileWarning, ErrorCollector},
    generic_ir::Scalar,
    ir::{self, Width},
    lvalue::{LValue, TypedLValue},
    object_location::ObjectLocation,
    rvalue::{RValue, TypedRValue},
    struct_union::StructUnion,
    utils,
};
use crate::{
    initializer::TypedConstant,
    ir::{GlobalVarId, VarLocation, VirtualReg},
};

/**
 * Keep track of symbols across the whole translation unit.
 * Manages values of symbols and builds the export and import lists.
 *
 * If an address of a local variable is needed, it's "fixed in memory".
 * Each fixed variable is allocated on the function frame alongside with bigger objects.
 * Addresses of fixed variables are stored in address variables for faster access in case of software stack.
 */
#[derive(Clone)]
pub struct NameScope {
    last_reg: VirtualReg,
    last_static_id: u32,
    defs: Vec<Scope>,
    pub static_initializers: HashMap<GlobalVarId, (TypedConstant, Attributes, Span)>,
    tagged_types: Vec<(Tagged, Span)>,
    function_frame: Option<FunctionFrame>,
    local_statics: Vec<GlobalVarId>,
    defined_functions: HashMap<String, ExportClass>,
    next_id: usize,
    pub literals: Vec<Vec<u8>>,
}

#[derive(Clone, Debug)]
pub enum Value {
    Type(QualifiedType),
    AutoVar(QualifiedType, VirtualReg),
    StaticVar {
        t: QualifiedType,
        id: GlobalVarId,
        storage_class: GlobalStorageClass,
        initializer: Option<TypedConstant>,
        attrs: Attributes,
    },
    Object(QualifiedType, ir::Scalar),
    Builtin(QualifiedType, BuiltinFunction),
    IntConstant(QualifiedType, i128),
}

#[derive(Clone)]
pub struct FunctionFrame {
    name: String,
    frame_size: u32,
    fixed_regs: HashMap<ir::VirtualReg, (ir::VirtualReg, ir::Width)>, // reg -> address reg
    address_regs: Vec<(ir::VirtualReg, u32)>,
    frame_pointer_reg: ir::VirtualReg,
    return_type: QualifiedType,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExportClass {
    Normal,
    Weak,
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
            tagged_types: Vec::new(),
            function_frame: None,
            local_statics: Vec::new(),
            defined_functions: HashMap::new(),
            next_id: 0,
            literals: Vec::new(),
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
        let m = self.defs.pop().unwrap();
        if self.defs.len() == 0 {
            panic!("NameScope is popped above the global level");
        }
        for (_key, (val, span)) in m.default {
            if let Value::StaticVar {
                t,
                id,
                initializer,
                attrs,
                ..
            } = val
            {
                let initializer = initializer.unwrap_or_else(|| TypedConstant::new_default(t));
                self.static_initializers
                    .insert(id, (initializer, attrs, span));
            }
        }
    }

    /**
     * Collect top-level initializers.
     */
    pub fn finalize_initializers(&mut self) {
        assert_eq!(self.defs.len(), 1);
        for (_key, (val, span)) in &self.defs.first().unwrap().default {
            match val {
                Value::StaticVar {
                    t,
                    id,
                    storage_class,
                    initializer,
                    attrs,
                    ..
                } => match storage_class {
                    GlobalStorageClass::Default | GlobalStorageClass::Static => {
                        if !t.t.is_function() {
                            let initializer = initializer
                                .clone()
                                .unwrap_or_else(|| TypedConstant::new_default(t.clone()));
                            self.static_initializers
                                .insert(id.clone(), (initializer, attrs.clone(), *span));
                        }
                    }
                    GlobalStorageClass::Extern => (),
                },
                Value::Object(_, _) => {
                    unreachable!("No objects at top level");
                }
                Value::AutoVar(_, _) => {
                    unreachable!("No auto vars at top level");
                }
                Value::Type(_) | Value::Builtin(_, _) | Value::IntConstant(_, _) => (),
            }
        }
    }

    /**
     * Push scope, declare function arguments and frame pointer and save return type.
     */
    pub fn start_function(
        &mut self,
        name: &str,
        args: &FunctionArgs,
        return_type: &QualifiedType,
        is_inline: bool,
    ) -> Vec<ir::Op> {
        let export_class = if is_inline {
            ExportClass::Weak
        } else {
            ExportClass::Normal
        };
        self.defined_functions
            .insert(name.to_string(), export_class);
        let mut defs = HashMap::new();
        let mut init_instructions = if let FunctionArgs::List(l) = args {
            let mut ops = Vec::new();
            for (i, (t, name, span)) in l.iter().enumerate() {
                if let Some(name) = name {
                    if t.t.is_scalar() || t.t.is_valist() {
                        let reg = self.alloc_reg();
                        defs.insert(name.to_string(), (Value::AutoVar(t.clone(), reg), *span));
                        ops.push(ir::Op::Arg(ir::ArgOp {
                            dst_reg: reg,
                            arg_number: i,
                            width: t.t.get_scalar_width().unwrap(),
                        }))
                    } else if t.t.is_object() {
                        // Passing struct by value - actually passing the address of the struct
                        let reg = self.alloc_reg();
                        defs.insert(
                            name.to_string(),
                            (
                                Value::Object(t.clone(), Scalar::Var(VarLocation::Local(reg))),
                                *span,
                            ),
                        );
                        ops.push(ir::Op::Arg(ir::ArgOp {
                            dst_reg: reg,
                            arg_number: i,
                            width: Width::PTR_WIDTH,
                        }))
                    } else {
                        todo!("array passing in parameters")
                    }
                }
            }
            ops
        } else {
            Vec::new()
        };
        let fp_reg = self.alloc_reg();
        init_instructions.push(ir::Op::FramePointer(fp_reg));
        self.defs.push(Scope::new_with_defs(defs));
        assert!(self.function_frame.is_none());
        self.function_frame = Some(FunctionFrame::new(name, return_type, fp_reg));
        init_instructions
    }

    /**
     * End a function.
     */
    pub fn end_function(&mut self) -> FunctionFrame {
        self.pop_and_collect_initializers();
        self.function_frame.take().unwrap()
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
        self.function_frame.as_ref().unwrap().return_type.clone()
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
        attrs: Attributes,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        if is_builtin_name(name) {
            return ec.record_error(CompileError::BuiltinRedefinition(name.to_string()), span);
        }
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
            if !t.t.is_complete(self) {
                return ec.record_error(CompileError::IncompleteType(t), span);
            }
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
                let (old_var_id, old_initializer, mut old_attrs) = if let Value::StaticVar {
                    id,
                    initializer,
                    attrs,
                    ..
                } = old_val
                {
                    (id, initializer, attrs)
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
                old_attrs.update(attrs, span, ec)?;

                self.insert(
                    name,
                    Value::StaticVar {
                        t,
                        id: old_var_id,
                        storage_class: combined_storage_class,
                        initializer: combined_initializer,
                        attrs: old_attrs,
                    },
                    span,
                );
            } else {
                let var_id = match storage_class {
                    GlobalStorageClass::Static => GlobalVarId::Static(name.to_string()),
                    GlobalStorageClass::Default | GlobalStorageClass::Extern => {
                        GlobalVarId::Global(name.to_string())
                    }
                };
                self.insert(
                    name,
                    Value::StaticVar {
                        t,
                        id: var_id,
                        storage_class,
                        initializer,
                        attrs,
                    },
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

            if !t.t.is_complete(self) {
                return ec.record_error(CompileError::IncompleteType(t), span);
            }

            match storage_class {
                Some(StorageClassSpecifier::Static) => {
                    let id = GlobalVarId::LocalStatic {
                        name: name.to_string(),
                        function_name: self.function_frame.as_ref().unwrap().name.clone(),
                        index: self.next_id,
                    };
                    self.next_id += 1;
                    if t.t.is_scalar() {
                        let initializer = if let Some(tv) = initializer {
                            Some(tv.implicit_cast(&t, span, ec)?)
                        } else {
                            None
                        };
                        self.local_statics.push(id.clone());
                        self.insert(
                            name,
                            Value::StaticVar {
                                t,
                                id,
                                storage_class: GlobalStorageClass::Static,
                                initializer,
                                attrs,
                            },
                            span,
                        );
                    } else if t.t.is_object() || t.t.is_array() {
                        self.local_statics.push(id.clone());
                        self.insert(
                            name,
                            Value::StaticVar {
                                t,
                                id,
                                storage_class: GlobalStorageClass::Static,
                                initializer,
                                attrs,
                            },
                            span,
                        );
                    } else if t.t.is_valist() {
                        ec.record_error(
                            CompileError::Unimplemented("static va_list".to_string()),
                            span,
                        )?;
                        unreachable!()
                    } else {
                        unreachable!()
                    }
                }
                None
                | Some(StorageClassSpecifier::Auto)
                | Some(StorageClassSpecifier::Register) => {
                    assert!(initializer.is_none());
                    if t.t.is_scalar() | t.t.is_valist() {
                        let id = self.alloc_reg();
                        self.insert(name, Value::AutoVar(t, id), span);
                    } else if t.t.is_object() || t.t.is_array() {
                        let size = t.t.sizeof(self, span, ec)?;
                        let align = t.t.alignof(self, span, ec)?;
                        let offset = self.alloc_frame(size, align);
                        let address_reg = self.alloc_reg();
                        self.function_frame
                            .as_mut()
                            .unwrap()
                            .address_regs
                            .push((address_reg, offset));
                        self.insert(
                            name,
                            Value::Object(t, ir::Scalar::Var(ir::VarLocation::Local(address_reg))),
                            span,
                        );
                    } else {
                        unreachable!()
                    }
                }
                Some(StorageClassSpecifier::Extern) => {
                    return ec
                        .record_error(CompileError::WrongStorageClass, storage_class_span.unwrap())
                }
                Some(StorageClassSpecifier::ThreadLocal) => {
                    ec.record_error(
                        CompileError::Unimplemented("thread locals".to_string()),
                        span,
                    )?;
                    unreachable!()
                }
                Some(StorageClassSpecifier::Typedef) => unreachable!(), // handled above
            }
        }
        Ok(())
    }

    pub fn declare_int_constant(
        &mut self,
        name: &str,
        t: QualifiedType,
        value: i128,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<(), ()> {
        if self.exists_at_any_level(name).is_some() {
            ec.record_warning(CompileWarning::LocalVarShadow(name.to_string()), span)?;
        }
        if self.remove_at_same_level(name).is_some() {
            return ec.record_error(CompileError::VarRedefinition(name.to_string()), span);
        }
        self.insert(name, Value::IntConstant(t, value), span);
        Ok(())
    }

    pub fn declare_struct(
        &mut self,
        name: Option<String>,
        members: Option<Vec<(Option<String>, QualifiedType)>>,
        packed: bool,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<StructUnionIdentifier, ()> {
        let tagged = Tagged::new_struct(members, packed);
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
        packed: bool,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<StructUnionIdentifier, ()> {
        let tagged = Tagged::new_union(members, packed);
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

    pub fn add_literal(&mut self, data: Vec<u8>) -> GlobalVarId {
        self.literals.push(data);
        GlobalVarId::Literal(self.literals.len() - 1)
    }

    pub fn get_type_alias(
        &self,
        name: &str,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<QualifiedType, ()> {
        if let Some(val) = self.get(name) {
            if val.is_type() {
                return Ok(val.take_type());
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
    ) -> Result<TypedConstant, ()> {
        if let Some(val) = self.get(name) {
            if !val.is_type() {
                match val {
                    Value::StaticVar { t, initializer, .. } => {
                        if !t.is_const() || initializer.is_none() {
                            ec.record_error(CompileError::NonConstInConstExpr, span)?;
                            unreachable!();
                        }
                        return Ok(initializer.unwrap());
                    }
                    Value::IntConstant(t, c) => {
                        return Ok(TypedConstant {
                            t,
                            val: crate::initializer::Constant::Int(c),
                        })
                    }
                    _ => {
                        ec.record_error(CompileError::NonConstInConstExpr, span)?;
                        unreachable!();
                    }
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
                    t,
                    src: RValue::new_var(VarLocation::Local(r)),
                }),
                Value::StaticVar { t, id, .. } => {
                    if t.t.is_object() || t.t.is_function() || t.t.is_array() {
                        Ok(TypedRValue {
                            t,
                            src: RValue::Object(ObjectLocation::PointedBy(Scalar::SymbolOffset(
                                id, 0,
                            ))),
                        })
                    } else {
                        Ok(TypedRValue {
                            t,
                            src: RValue::new_var(VarLocation::Global(id)),
                        })
                    }
                }
                Value::Type(_) => {
                    ec.record_error(CompileError::NotAVar(name.to_string()), span)?;
                    unreachable!();
                }
                Value::Object(t, p) => Ok(TypedRValue {
                    t,
                    src: RValue::new_object(ObjectLocation::PointedBy(p)),
                }),
                Value::Builtin(t, b) => Ok(TypedRValue {
                    t,
                    src: RValue::Builtin(b),
                }),
                Value::IntConstant(t, v) => Ok(TypedRValue {
                    src: RValue::new_const(v as u64),
                    t,
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
                    t,
                    lv: LValue::Var(VarLocation::Local(r)),
                }),
                Value::StaticVar { t, id, .. } => {
                    if t.t.is_object() || t.t.is_array() || t.t.is_function() {
                        Ok(TypedLValue {
                            t,
                            lv: LValue::Object(ObjectLocation::PointedBy(Scalar::SymbolOffset(
                                id, 0,
                            ))),
                        })
                    } else {
                        Ok(TypedLValue {
                            t,
                            lv: LValue::Var(VarLocation::Global(id)),
                        })
                    }
                }
                Value::Type(_) | Value::Builtin(_, _) | Value::IntConstant(_, _) => {
                    ec.record_error(CompileError::NotAVar(name.to_string()), span)?;
                    unreachable!();
                }
                Value::Object(t, p) => Ok(TypedLValue {
                    t,
                    lv: LValue::Object(ObjectLocation::PointedBy(p)),
                }),
            }
        } else {
            ec.record_error(CompileError::UnknownIdentifier(name.to_string()), span)?;
            unreachable!();
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some((builtin_fn, t)) = get_builtin_function(name) {
            return Some(Value::Builtin(t, builtin_fn));
        }
        for m in self.defs.iter().rev() {
            if let Some((val, _)) = m.default.get(name) {
                return Some(val.clone());
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

    /**
     * Fix a variable in memory. Return its address.
     */
    pub fn fix_in_memory(&mut self, var: &VarLocation, width: ir::Width) -> ir::Scalar {
        match var {
            VarLocation::Global(id) => ir::Scalar::SymbolOffset(id.clone(), 0),
            VarLocation::Local(n) => {
                if let Some((reg, old_width)) =
                    self.function_frame.as_ref().unwrap().fixed_regs.get(n)
                {
                    assert_eq!(width, *old_width);
                    ir::Scalar::Var(ir::VarLocation::Local(*reg))
                } else {
                    let offset = self.alloc_frame(8, 8); // TODO const?
                    let address_reg = self.alloc_reg();
                    self.function_frame
                        .as_mut()
                        .unwrap()
                        .fixed_regs
                        .insert(*n, (address_reg, width));
                    self.function_frame
                        .as_mut()
                        .unwrap()
                        .address_regs
                        .push((address_reg, offset));
                    ir::Scalar::Var(ir::VarLocation::Local(address_reg))
                }
            }
            VarLocation::Return => unreachable!("address of a return value"),
        }
    }

    pub fn alloc_reg(&mut self) -> VirtualReg {
        let r = self.last_reg;
        self.last_reg += 1;
        r
    }

    pub fn get_frame_pointer_reg(&self) -> VirtualReg {
        self.function_frame.as_ref().unwrap().frame_pointer_reg
    }

    pub fn get_import_symbols(&self) -> Vec<GlobalVarId> {
        let mut result = Vec::new();
        for (name, (val, _)) in &self.defs[0].default {
            if val.is_var() {
                if val.get_type().t.is_function() {
                    match val.unwrap_storage_class() {
                        GlobalStorageClass::Static => (),
                        GlobalStorageClass::Default | GlobalStorageClass::Extern => {
                            if !self.defined_functions.contains_key(name) {
                                result.push(GlobalVarId::Global(name.clone()));
                            }
                        }
                    }
                } else if let GlobalStorageClass::Extern = val.unwrap_storage_class() {
                    result.push(GlobalVarId::Global(name.clone()));
                }
            }
        }
        result
    }

    pub fn get_export_symbols(&self) -> Vec<(GlobalVarId, ExportClass)> {
        let mut result = Vec::new();
        for (name, (val, _)) in &self.defs[0].default {
            if val.is_var() {
                if val.get_type().t.is_function() {
                    match val.unwrap_storage_class() {
                        GlobalStorageClass::Static => (),
                        GlobalStorageClass::Default | GlobalStorageClass::Extern => {
                            if let Some(class) = self.defined_functions.get(name) {
                                result.push((GlobalVarId::Global(name.clone()), *class));
                            }
                        }
                    }
                } else if let GlobalStorageClass::Default = val.unwrap_storage_class() {
                    result.push((GlobalVarId::Global(name.clone()), ExportClass::Normal));
                }
            }
        }
        result
    }

    /// Allocate space for an object on the function frame.
    pub fn alloc_temp_object(
        &mut self,
        t: QualifiedType,
        ec: &mut ErrorCollector,
        span: Span,
    ) -> Result<TypedLValue, ()> {
        assert!(t.t.is_object());
        let size = t.t.sizeof(self, span, ec)?;
        let align = t.t.alignof(self, span, ec)?;
        let offset = self.alloc_frame(size, align);
        let address_reg = self.alloc_reg();
        self.function_frame
            .as_mut()
            .unwrap()
            .address_regs
            .push((address_reg, offset));
        Ok(TypedLValue {
            t,
            lv: LValue::Object(ObjectLocation::PointedBy(Scalar::Var(VarLocation::Local(
                address_reg,
            )))),
        })
    }

    fn get_tagged_type(&self, id: usize) -> &Tagged {
        let result = &self.tagged_types[id].0;
        result
    }

    fn alloc_frame(&mut self, size: u32, align: u32) -> u32 {
        self.function_frame.as_mut().unwrap().alloc(size, align)
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
            Value::AutoVar(_, _) => true,
            Value::StaticVar { .. } => true,
            Value::Object(_, _) => true,
            Value::Builtin(_, _) => true,
            Value::IntConstant(_, _) => false,
        }
    }

    pub fn is_type(&self) -> bool {
        match self {
            Value::Type(_) => true,
            Value::AutoVar(_, _) => false,
            Value::StaticVar { .. } => false,
            Value::Object(_, _) => false,
            Value::Builtin(_, _) => false,
            Value::IntConstant(_, _) => false,
        }
    }

    pub fn get_type(&self) -> &QualifiedType {
        match self {
            Value::Type(t) => t,
            Value::AutoVar(t, _) => t,
            Value::StaticVar { t, .. } => t,
            Value::Object(t, _) => t,
            Value::Builtin(t, _) => t,
            Value::IntConstant(t, _) => t,
        }
    }

    pub fn take_type(self) -> QualifiedType {
        match self {
            Value::Type(t) => t,
            Value::AutoVar(t, _) => t,
            Value::StaticVar { t, .. } => t,
            Value::Object(t, _) => t,
            Value::Builtin(t, _) => t,
            Value::IntConstant(t, _) => t,
        }
    }

    pub fn unwrap_storage_class(&self) -> GlobalStorageClass {
        if let Value::StaticVar { storage_class, .. } = self {
            *storage_class
        } else {
            panic!("Can only unwrap storage class for a static variable");
        }
    }

    #[cfg(test)]
    pub fn unwrap_static_var(
        self,
    ) -> (
        QualifiedType,
        GlobalVarId,
        GlobalStorageClass,
        Option<TypedConstant>,
        Attributes,
    ) {
        if let Value::StaticVar {
            t,
            id,
            storage_class,
            initializer,
            attrs,
        } = self
        {
            (t, id, storage_class, initializer, attrs)
        } else {
            panic!("Not a static var")
        }
    }
}

impl FunctionFrame {
    pub fn get_size(&self) -> u32 {
        self.frame_size
    }

    pub fn get_fixed_reg_addr_var(&self, var: &VarLocation) -> Option<VarLocation> {
        if let VarLocation::Local(n) = var {
            self.fixed_regs
                .get(n)
                .map(|(reg, _)| ir::VarLocation::Local(*reg))
        } else {
            None
        }
    }

    pub fn address_regs_iter(&self) -> impl Iterator<Item = (u32, ir::VarLocation)> + '_ {
        self.address_regs
            .iter()
            .map(|(reg, offset)| (*offset, ir::VarLocation::Local(*reg)))
    }

    pub fn fixed_regs_iter(
        &self,
    ) -> impl Iterator<Item = (ir::VirtualReg, ir::VirtualReg, ir::Width)> + '_ {
        self.fixed_regs
            .iter()
            .map(|(fixed_reg, (address_reg, width))| (*fixed_reg, *address_reg, *width))
    }

    pub fn get_frame_pointer_reg(&self) -> ir::VirtualReg {
        self.frame_pointer_reg
    }

    #[cfg(test)]
    pub fn get_fixed_regs(&self) -> &HashMap<ir::VirtualReg, (ir::VirtualReg, ir::Width)> {
        &self.fixed_regs
    }

    fn new(name: &str, return_type: &QualifiedType, fp_reg: ir::VirtualReg) -> Self {
        FunctionFrame {
            name: name.to_string(),
            frame_size: 0,
            fixed_regs: HashMap::new(),
            address_regs: Vec::new(),
            frame_pointer_reg: fp_reg,
            return_type: return_type.clone(),
        }
    }

    fn alloc(&mut self, size: u32, align: u32) -> u32 {
        self.frame_size = utils::align(self.frame_size, align);
        let r = self.frame_size;
        self.frame_size += size;
        r
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
    fn new_struct(members: Option<Vec<(Option<String>, QualifiedType)>>, packed: bool) -> Self {
        Self::StructUnion(StructUnion::new_struct(members, packed))
    }

    fn new_union(members: Option<Vec<(Option<String>, QualifiedType)>>, packed: bool) -> Self {
        Self::StructUnion(StructUnion::new_union(members, packed))
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
