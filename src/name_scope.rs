use crate::ir::{VarLocation, Reg};
use std::collections::HashMap;

use lang_c::span::Span;

use crate::{
    ctype::QualifiedType,
    error::{CompileError, CompileWarning, ErrorCollector},
    translation_unit::GlobalDeclaration,
};

pub struct NameScope<'globals> {
    last_reg: Reg,
    globals: &'globals HashMap<String, GlobalDeclaration>,
    locals: Vec<HashMap<String, (QualifiedType, VarLocation)>>,
}

impl<'globals> NameScope<'globals> {
    pub fn new(globals: &'globals HashMap<String, GlobalDeclaration>) -> Self {
        Self {
            last_reg: 0,
            globals,
            locals: vec![HashMap::new()],
        }
    }

    pub fn push(&mut self) {
        self.locals.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.locals
            .pop()
            .expect("name scope is popped beyond upper level");
    }

    pub fn alloc_temp(&mut self) -> VarLocation {
        VarLocation::Local(self.alloc_reg())
    }

    pub fn declare_local_var(
        &mut self,
        name: &str,
        t: QualifiedType,
        is_static: bool,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<VarLocation, ()> {
        if self.find(name, false).is_some() {
            ec.record_warning(CompileWarning::LocalVarShadow(name.to_string()), span)?;
        }
        let loc = if is_static {
            VarLocation::Static(name.to_string()) // TODO mangle local static names
        } else {
            VarLocation::Local(self.alloc_reg())
        };
        if self
            .locals
            .last_mut()
            .expect("empty name scope: too many pops")
            .insert(name.to_string(), (t, loc.clone()))
            .is_some()
        {
            ec.record_error(CompileError::VarRedefinition(name.to_string()), span)?;
        }
        Ok(loc)
    }

    pub fn get(
        &self,
        name: &str,
        span: Span,
        ec: &mut ErrorCollector,
    ) -> Result<(QualifiedType, VarLocation), ()> {
        match self.find(name, true) {
            Some(x) => Ok(x),
            None => {
                ec.record_error(CompileError::UnknownIdentifier(name.to_string()), span)?;
                unreachable!();
            }
        }
    }

    // pub fn get_and_update_register(
    //     &mut self,
    //     name: &str,
    //     span: Span,
    //     ec: &mut ErrorCollector,
    // ) -> Result<(QualifiedType, VarLocation), ()> {
    //     match self.find_mut(name) {
    //         Some(v) => match v.1 {
    //             VarLocation::Static(_) => Ok(v.clone()),
    //             VarLocation::Local(_) => {
    //                 let new_reg = self.alloc_reg();
    //                 v.1 = VarLocation::Local(new_reg);
    //                 Ok(v.clone())
    //             }
    //         },
    //         None => {
    //             if let Some(gd) = self.globals.get(name) {
    //                 Ok((gd.t.clone(), VarLocation::Static(name.to_string())))
    //             } else {
    //                 ec.record_error(CompileError::UnknownIdentifier(name.to_string()), span)?;
    //                 unreachable!()
    //             }
    //         }
    //     }
    // }

    fn alloc_reg(&mut self) -> Reg {
        let r = self.last_reg;
        self.last_reg += 1;
        r
    }

    fn find(&self, name: &str, globals: bool) -> Option<(QualifiedType, VarLocation)> {
        for m in self.locals.iter().rev() {
            if let Some(r) = m.get(name) {
                return Some(r.clone());
            }
        }
        if globals {
            if let Some(gd) = self.globals.get(name) {
                Some((gd.t.clone(), VarLocation::Static(name.to_owned())))
            } else {
                None
            }
        } else {
            None
        }
    }

    fn find_mut(&mut self, name: &str) -> Option<&mut (QualifiedType, VarLocation)> {
        for m in self.locals.iter_mut().rev() {
            if let Some(r) = m.get_mut(name) {
                return Some(r);
            }
        }
        None
    }
}
