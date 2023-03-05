use std::assert_matches::assert_matches;

use crate::ir;
use crate::name_scope::FunctionFrame;

/**
 * Insert instructions to flush fixed registers to memory and to load them from memory.
 */
pub fn insert_flush_instructions(body: &mut Vec<ir::Block>, frame: &FunctionFrame) {
    for (fixed_reg, address_reg, width) in frame.fixed_regs_iter() {
        let mut states = vec![FlushState::Unknown; body.len()];
        insert_mem_ops(
            0,
            FlushState::Clean,
            body,
            &mut states,
            fixed_reg,
            address_reg,
            width,
        );
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum FlushState {
    Unknown,
    Clean,
    RegDirty,
    MemDirty,
}

/**
 * Insert load and store instructions for given fixed reg starting from given block number.
 */
fn insert_mem_ops(
    block_number: usize,
    mut state: FlushState,
    body: &mut Vec<ir::Block>,
    states: &mut Vec<FlushState>,
    fixed_reg: ir::Reg,
    address_reg: ir::Reg,
    width: ir::Width,
) {
    assert_eq!(body.len(), states.len());
    assert_ne!(state, FlushState::Unknown);
    assert_eq!(states[block_number], FlushState::Unknown);

    states[block_number] = state;

    let mut i = 0;
    {
        let ops = &mut body[block_number].ops;
        while i != ops.len() {
            match state {
                FlushState::Clean => {
                    if ops[i].is_write_to_register(fixed_reg) && ops[i].is_memory_write() {
                        assert_matches!(ops[i], ir::Op::Call(_));
                        state = FlushState::RegDirty;
                    } else if ops[i].is_write_to_register(fixed_reg) {
                        state = FlushState::RegDirty;
                    } else if ops[i].is_memory_write() {
                        state = FlushState::MemDirty;
                    }
                }
                FlushState::RegDirty => {
                    if ops[i].is_memory_read() | ops[i].is_memory_write() {
                        ops.insert(
                            i,
                            ir::Op::Store(ir::StoreOp {
                                dst_addr: ir::Scalar::Var(ir::VarLocation::Local(address_reg)),
                                src: ir::Scalar::Var(ir::VarLocation::Local(fixed_reg)),
                                width,
                            }),
                        );
                        state = FlushState::Clean;
                    }
                }
                FlushState::MemDirty => {
                    if ops[i].is_read_from_register(fixed_reg) {
                        ops.insert(
                            i,
                            ir::Op::Load(ir::LoadOp {
                                src_addr: ir::Scalar::Var(ir::VarLocation::Local(address_reg)),
                                dst: ir::VarLocation::Local(fixed_reg),
                                width,
                            }),
                        );
                        state = FlushState::Clean;
                    } else if ops[i].is_write_to_register(fixed_reg) {
                        state = FlushState::RegDirty;
                    }
                }
                FlushState::Unknown => unreachable!(),
            }
            i += 1;
        }
    }
    let to_visit = body[block_number].tail.get_connections();
    let mut extra_load = false;
    let mut extra_store = false;
    for n in &to_visit {
        if states[*n] != FlushState::Unknown && states[*n] != state {
            if state == FlushState::RegDirty {
                extra_store = true;
            }
            if state == FlushState::MemDirty {
                extra_load = true;
            }
        }
    }
    if extra_store {
        body[block_number].ops.push(ir::Op::Store(ir::StoreOp {
            dst_addr: ir::Scalar::Var(ir::VarLocation::Local(address_reg)),
            src: ir::Scalar::Var(ir::VarLocation::Local(fixed_reg)),
            width,
        }));
        state = FlushState::Clean;
    }
    if extra_load {
        body[block_number].ops.push(ir::Op::Load(ir::LoadOp {
            src_addr: ir::Scalar::Var(ir::VarLocation::Local(address_reg)),
            dst: ir::VarLocation::Local(fixed_reg),
            width,
        }));
        state = FlushState::Clean;
    }
    for n in to_visit {
        if states[n] == FlushState::Unknown {
            insert_mem_ops(n, state, body, states, fixed_reg, address_reg, width);
        }
    }
}
