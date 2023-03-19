use std::collections::HashMap;

use crate::ir;

pub fn deconstruct_ssa(body: &mut [ir::Block], map: &mut HashMap<ir::Reg, ir::Reg>) {
    // Rename registers in body and tail
    for block in body.iter_mut() {
        for op in block.ops.iter_mut() {
            op.remap_regs(map, None);
        }
        block.tail.remap_regs(map);
    }

    // Convert phi nodes to copies
    for block in body.iter_mut() {
        todo!()
    }
}
