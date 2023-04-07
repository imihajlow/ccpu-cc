use crate::generic_ir::GlobalVarId;

pub const RET_VALUE_REG_SYMBOL: &str = "__cc_ret";
pub const DYN_FRAME_POINTER_SYMBOL: &str = "__cc_fp";

pub fn get_global_var_label(id: &GlobalVarId) -> String {
    format!("{}_{}", id.0, id.1)
}

pub fn get_static_frame_symbol(fn_name: &str) -> String {
    format!("__cc_{}_frame", fn_name)
}
