use crate::generic_ir::GlobalVarId;

pub const RET_VALUE_REG_SYMBOL: &str = "__cc_ret";

pub fn get_global_var_label(id: &GlobalVarId) -> String {
    format!("{}_{}", id.0, id.1)
}
