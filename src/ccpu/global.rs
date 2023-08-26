use crate::generic_ir::GlobalVarId;

pub const RET_VALUE_REG_SYMBOL: &str = "__cc_ret";
pub const DYN_FRAME_POINTER_SYMBOL: &str = "__cc_fp";

pub fn get_global_var_label(id: &GlobalVarId) -> String {
    match id {
        GlobalVarId::Global(s) => s.to_string(),
        GlobalVarId::Static(s) => format!("__static_{}", s),
        GlobalVarId::LocalStatic {
            name,
            function_name,
            index,
        } => format!("__static_{}_{}_{}", function_name, index, name),
        GlobalVarId::CompilerInternal(s) => format!("__cc_{}", s),
        GlobalVarId::Literal(id) => format!("__lit_{}", id),
    }
}

pub fn get_static_frame_symbol(fn_name: &str) -> GlobalVarId {
    GlobalVarId::CompilerInternal(format!("{}_frame", fn_name))
}
