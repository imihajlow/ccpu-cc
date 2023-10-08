use crate::{
    generic_ir::{JumpCondition, Op, Tail},
    ir,
    opt::util::count_reg_usages,
};

pub fn optimize_conditions(blocks: &mut Vec<ir::Block>) -> bool {
    let usages = count_reg_usages(blocks);
    let mut modified = false;
    for block in blocks {
        if let Tail::Cond(cond, t, f) = &mut block.tail {
            match cond {
                JumpCondition::StrictBool(s) => {
                    if let Some(reg) = s.get_reg() {
                        if usages.get(&reg).copied().unwrap_or(0) <= 1 {
                            if let Some(origin_op) = find_reg_origin(&block.ops, reg) {
                                match origin_op {
                                    Op::Bool(op) => {
                                        *cond =
                                            JumpCondition::RelaxedBool(op.src.clone(), op.width);
                                        modified = true;
                                    }
                                    Op::BoolInv(op) => {
                                        *cond =
                                            JumpCondition::RelaxedBool(op.src.clone(), op.width);
                                        (*t, *f) = (*f, *t);
                                        modified = true;
                                    }
                                    _ => (),
                                }
                            }
                        }
                    }
                }
                JumpCondition::RelaxedBool(s, _) => {
                    if let Some(reg) = s.get_reg() {
                        if usages.get(&reg).copied().unwrap_or(0) <= 1 {
                            if let Some(origin_op) = find_reg_origin(&block.ops, reg) {
                                match origin_op {
                                    Op::Bool(op) => {
                                        *cond =
                                            JumpCondition::RelaxedBool(op.src.clone(), op.width);
                                        modified = true;
                                    }
                                    Op::BoolInv(op) => {
                                        *cond =
                                            JumpCondition::RelaxedBool(op.src.clone(), op.width);
                                        (*t, *f) = (*f, *t);
                                        modified = true;
                                    }
                                    Op::Compare(op) => {
                                        *cond = JumpCondition::Compare(op.desc.clone());
                                        modified = true;
                                    }
                                    _ => (),
                                }
                            }
                        }
                    }
                }
                JumpCondition::Compare(_) => (),
            }
        }
    }
    modified
}

fn find_reg_origin(ops: &Vec<ir::Op>, reg: ir::VirtualReg) -> Option<&ir::Op> {
    for op in ops {
        if op.get_dst_reg() == Some(reg) {
            return Some(&op);
        }
    }
    None
}
