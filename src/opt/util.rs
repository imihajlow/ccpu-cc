use std::collections::{HashMap, HashSet};

use crate::ir;

pub fn count_reg_usages(blocks: &Vec<ir::Block>) -> HashMap<ir::VirtualReg, u32> {
    let mut m = HashMap::new();
    for block in blocks {
        for (_, (_, srcs)) in &block.phi.srcs {
            for (_, s) in srcs {
                if let Some(r) = s.get_reg() {
                    count(&mut m, r);
                }
            }
        }
        for op in &block.ops {
            let mut set = HashSet::new();
            op.collect_read_regs(&mut set);
            for r in set {
                count(&mut m, r);
            }
        }
        {
            let mut set = HashSet::new();
            block.tail.collect_read_regs(&mut set);
            for r in set {
                count(&mut m, r);
            }
        }
    }
    m
}

fn count(map: &mut HashMap<ir::VirtualReg, u32>, r: ir::VirtualReg) {
    if let Some(c) = map.get_mut(&r) {
        *c += 1;
    } else {
        map.insert(r, 1);
    }
}
