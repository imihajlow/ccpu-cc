use crate::ir;
use crate::name_scope::FunctionFrame;

/**
 * Insert instructions to flush fixed registers to memory and to load them from memory.
 */
pub fn insert_flush_instructions(body: &mut Vec<ir::Block>, frame: &FunctionFrame) {
    for (fixed_reg, address_reg, width) in frame.fixed_regs_iter() {
        let mut states = vec![FlushState::Unknown; body.len()];
        insert_stores(
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
    Dirty,
}

/**
 * Insert store instructions for given fixed reg starting from given block number.
 */
fn insert_stores(
    block_number: usize,
    state: FlushState,
    body: &mut Vec<ir::Block>,
    states: &mut Vec<FlushState>,
    fixed_reg: ir::Reg,
    address_reg: ir::Reg,
    width: ir::Width,
) {
    assert_eq!(body.len(), states.len());
    assert_ne!(state, FlushState::Unknown);

    if state == states[block_number] {
        // This block has already been visited, all necessary store instructions have been added.
        return;
    }
    let stop_on_write = match states[block_number] {
        FlushState::Dirty => {
            // This block has already been visited as Dirty, now state is Clean, no new instructions can be added.
            return;
        }
        FlushState::Clean => {
            // This block has already been visited as Clean, but now state is Dirty.
            // New instructions are possible, but we have to stop after first write to the variable.
            true
        }
        FlushState::Unknown => {
            // This block has never been visited.
            false
        }
    };
    states[block_number] = state;
    let mut dirty = match state {
        FlushState::Clean => false,
        FlushState::Dirty => true,
        FlushState::Unknown => unreachable!(),
    };
    let mut i = 0;
    {
        let ops = &mut body[block_number].ops;
        while i != ops.len() {
            if dirty && ops[i].is_memory_read() {
                ops.insert(
                    i,
                    ir::Op::Store(ir::StoreOp {
                        dst_addr: ir::Scalar::Var(ir::VarLocation::Local(address_reg)),
                        src: ir::Scalar::Var(ir::VarLocation::Local(fixed_reg)),
                        width,
                    }),
                );
                i += 1;
                dirty = false;
            }
            if ops[i].is_write_to_register(fixed_reg) {
                if stop_on_write {
                    return;
                }
                dirty = true;
            }
            i += 1;
        }
    }
    let state = if dirty {
        FlushState::Dirty
    } else {
        FlushState::Clean
    };
    let mut to_visit = Vec::new();
    match &body[block_number].tail {
        ir::Tail::Ret => (),
        ir::Tail::Jump(n) => to_visit.push(*n),
        ir::Tail::Cond(_, n, m) => {
            to_visit.push(*n);
            to_visit.push(*m);
        }
        ir::Tail::Switch(_, _, cases, default) => {
            for (_, n) in cases.iter() {
                to_visit.push(*n);
            }
            to_visit.push(*default);
        }
    };
    for n in to_visit {
        insert_stores(n, state, body, states, fixed_reg, address_reg, width);
    }
}
