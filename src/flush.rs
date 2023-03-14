use std::assert_matches::assert_matches;

use crate::graph::{CycleLength, Graph};
use crate::ir;
use crate::name_scope::FunctionFrame;

/**
 * Insert instructions to flush fixed registers to memory and to load them from memory.
 */
pub fn insert_flush_instructions(body: &mut Vec<ir::Block>, frame: &FunctionFrame) {
    let g = build_graph(body);
    let gt = g.clone().transposed();
    let exit_nodes: Vec<usize> = body
        .iter()
        .enumerate()
        .filter_map(|(index, block)| {
            if block.tail.is_return() {
                Some(index)
            } else {
                None
            }
        })
        .collect();
    for (fixed_reg, address_reg, width) in frame.fixed_regs_iter() {
        insert_flush_instructions_for_reg(
            body,
            fixed_reg,
            address_reg,
            width,
            &g,
            &gt,
            &exit_nodes,
        );
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum FlushState {
    Clean,
    RegDirty,
    MemDirty,
    BothDirty,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum EntryEffect {
    None,
    RegRead,
    RegWrite,
    MemAccess,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ExitEffect {
    None,
    RegRead,
    RegWrite,
    MemWrite,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum BlockPosition {
    Front,
    Back,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum FlushInstruction {
    Store,
    Load,
}

fn insert_flush_instructions_for_reg(
    body: &mut Vec<ir::Block>,
    fixed_reg: ir::Reg,
    address_reg: ir::Reg,
    width: ir::Width,
    g: &Graph,
    gt: &Graph,
    exit_nodes: &[usize],
) {
    let mut effects = Vec::with_capacity(body.len());
    for block in body.iter_mut() {
        effects.push(insert_inner_flush_instructions(
            block,
            fixed_reg,
            address_reg,
            width,
        ));
    }
    println!("reg = {} @ {}", fixed_reg, address_reg);
    println!("effects = {:#?}", effects);
    let fwd_states = build_forward_states(g, &effects);
    println!("fwd_states = {:#?}", fwd_states);
    println!("exit_nodes = {:?}", exit_nodes);
    let bwd_states = build_backward_states(gt, exit_nodes, &effects);
    println!("bwd_states = {:#?}", bwd_states);
    let fps = find_possible_flush_positions(&fwd_states, &bwd_states);
    println!("flush_positions = {:?}", fps);
}

fn build_forward_states(
    g: &Graph,
    effects: &[(EntryEffect, ExitEffect)],
) -> Vec<(FlushState, FlushState)> {
    assert_eq!(g.get_node_count(), effects.len());
    assert!(g.get_node_count() > 0);
    let mut result = vec![(None, None); g.get_node_count()];
    fn dfs(
        start: usize,
        state: FlushState,
        g: &Graph,
        effects: &[(EntryEffect, ExitEffect)],
        result: &mut Vec<(Option<FlushState>, Option<FlushState>)>,
    ) {
        let (_, exit_effect) = effects[start];
        let old_entry_state = result[start].0;
        if let Some(old_entry_state) = old_entry_state {
            // This node is already visited
            if old_entry_state != state {
                // Entry state is updated
                let new_exit_state = match old_entry_state {
                    FlushState::Clean => {
                        result[start].0 = Some(state);
                        transform_state(state, exit_effect)
                    }
                    _ => {
                        result[start].0 = Some(FlushState::BothDirty);
                        transform_state(FlushState::BothDirty, exit_effect)
                    }
                };
                if Some(new_exit_state) != result[start].1 {
                    // Exit state updated - visit children
                    result[start].1 = Some(new_exit_state);
                    for child in g.get_children(start) {
                        dfs(*child, new_exit_state, g, effects, result);
                    }
                }
            }
        } else {
            // First time visiting this node
            result[start].0 = Some(state);
            let new_state = transform_state(state, exit_effect);
            result[start].1 = Some(new_state);
            for child in g.get_children(start) {
                dfs(*child, new_state, g, effects, result);
            }
        }
    }
    dfs(0, FlushState::Clean, g, effects, &mut result);
    result
        .into_iter()
        .map(|(entry, exit)| {
            (
                entry.unwrap_or(FlushState::Clean),
                exit.unwrap_or(FlushState::Clean),
            )
        })
        .collect()
}

fn build_backward_states(
    gt: &Graph,
    exit_nodes: &[usize],
    effects: &[(EntryEffect, ExitEffect)],
) -> Vec<(FlushState, FlushState)> {
    assert_eq!(gt.get_node_count(), effects.len());
    assert!(gt.get_node_count() > 0);
    assert!(exit_nodes.len() > 0);
    let mut result = vec![(None, None); gt.get_node_count()];
    fn dfs(
        start: usize,
        state: FlushState,
        gt: &Graph,
        effects: &[(EntryEffect, ExitEffect)],
        result: &mut Vec<(Option<FlushState>, Option<FlushState>)>,
    ) {
        let (entry_effect, _) = effects[start];
        let old_exit_state = result[start].1;
        if let Some(old_exit_state) = old_exit_state {
            // This node is already visited
            if old_exit_state != state {
                // Exit state is updated
                let new_entry_state = match old_exit_state {
                    FlushState::Clean => {
                        result[start].1 = Some(state);
                        transform_state_bwd(state, entry_effect)
                    }
                    _ => {
                        result[start].1 = Some(FlushState::BothDirty);
                        transform_state_bwd(FlushState::BothDirty, entry_effect)
                    }
                };
                if Some(new_entry_state) != result[start].0 {
                    // Entry state updated - visit children
                    result[start].0 = Some(new_entry_state);
                    for child in gt.get_children(start) {
                        dfs(*child, new_entry_state, gt, effects, result);
                    }
                }
            }
        } else {
            // First time visiting this node
            result[start].1 = Some(state);
            let new_state = transform_state_bwd(state, entry_effect);
            result[start].0 = Some(new_state);
            for child in gt.get_children(start) {
                dfs(*child, new_state, gt, effects, result);
            }
        }
    }
    for node in exit_nodes {
        dfs(*node, FlushState::Clean, gt, effects, &mut result);
    }
    result
        .into_iter()
        .map(|(entry, exit)| {
            (
                entry.unwrap(), // entry.unwrap_or(FlushState::Clean),
                exit.unwrap(),  // exit.unwrap_or(FlushState::Clean),
            )
        })
        .collect()
}

fn find_possible_flush_positions(
    fwd_states: &[(FlushState, FlushState)],
    bwd_states: &[(FlushState, FlushState)],
) -> Vec<(usize, BlockPosition, FlushInstruction)> {
    assert_eq!(fwd_states.len(), bwd_states.len());
    let mut result = Vec::new();
    for i in 0..fwd_states.len() {
        if let Some(instr) = get_flush_instruction(fwd_states[i].0, bwd_states[i].0) {
            result.push((i, BlockPosition::Front, instr))
        }
        if let Some(instr) = get_flush_instruction(fwd_states[i].1, bwd_states[i].1) {
            result.push((i, BlockPosition::Back, instr))
        }
    }
    result
}

fn get_flush_instruction(fwd_state: FlushState, bwd_state: FlushState) -> Option<FlushInstruction> {
    match fwd_state {
        FlushState::BothDirty => None,
        FlushState::Clean => None,
        FlushState::RegDirty => match bwd_state {
            FlushState::MemDirty | FlushState::BothDirty => Some(FlushInstruction::Store),
            FlushState::Clean | FlushState::RegDirty => None,
        },
        FlushState::MemDirty => match bwd_state {
            FlushState::RegDirty | FlushState::BothDirty => Some(FlushInstruction::Load),
            FlushState::Clean | FlushState::MemDirty => None,
        },
    }
}

fn insert_inner_flush_instructions(
    block: &mut ir::Block,
    fixed_reg: ir::Reg,
    address_reg: ir::Reg,
    width: ir::Width,
) -> (EntryEffect, ExitEffect) {
    let mut entry_effect = EntryEffect::None;
    let mut exit_effect = ExitEffect::None;
    let mut flush_state = FlushState::Clean;
    let mut i = 0;
    while i != block.ops.len() {
        let (local_entry_effect, local_exit_effect) =
            get_instruction_effects(&block.ops[i], fixed_reg);
        if entry_effect == EntryEffect::None {
            entry_effect = local_entry_effect;
        }
        if local_exit_effect != ExitEffect::None {
            exit_effect = local_exit_effect;
        }
        flush_state = match (flush_state, local_entry_effect) {
            (FlushState::Clean, _) => exit_effect_to_flush_state(local_exit_effect),
            (state, EntryEffect::None) => state,
            (FlushState::RegDirty, EntryEffect::RegRead) => FlushState::RegDirty,
            (FlushState::RegDirty, EntryEffect::RegWrite) => FlushState::RegDirty,
            (FlushState::MemDirty, EntryEffect::RegWrite) => FlushState::RegDirty,
            (FlushState::MemDirty, EntryEffect::MemAccess) => FlushState::MemDirty,
            (FlushState::RegDirty, EntryEffect::MemAccess) => {
                block.ops.insert(
                    i,
                    ir::Op::Store(ir::StoreOp {
                        dst_addr: ir::Scalar::Var(ir::VarLocation::Local(address_reg)),
                        src: ir::Scalar::Var(ir::VarLocation::Local(fixed_reg)),
                        width,
                    }),
                );
                FlushState::Clean
            }
            (FlushState::MemDirty, EntryEffect::RegRead) => {
                block.ops.insert(
                    i,
                    ir::Op::Load(ir::LoadOp {
                        src_addr: ir::Scalar::Var(ir::VarLocation::Local(address_reg)),
                        dst: ir::VarLocation::Local(fixed_reg),
                        width,
                    }),
                );
                FlushState::Clean
            }
            (FlushState::BothDirty, _) => unreachable!(),
        };
        i += 1;
    }
    if block.tail.is_read_from_register(fixed_reg) {
        if flush_state == FlushState::MemDirty {
            block.ops.push(ir::Op::Load(ir::LoadOp {
                src_addr: ir::Scalar::Var(ir::VarLocation::Local(address_reg)),
                dst: ir::VarLocation::Local(fixed_reg),
                width,
            }));
        }
        exit_effect = ExitEffect::RegRead;
        if entry_effect == EntryEffect::None {
            entry_effect = EntryEffect::RegRead;
        }
    }
    (entry_effect, exit_effect)
}

fn get_instruction_effects(op: &ir::Op, fixed_reg: ir::Reg) -> (EntryEffect, ExitEffect) {
    let entry_effect = if op.is_read_from_register(fixed_reg) {
        EntryEffect::RegRead
    } else if op.is_memory_read() || op.is_memory_write() {
        EntryEffect::MemAccess
    } else if op.is_write_to_register(fixed_reg) {
        EntryEffect::RegWrite
    } else {
        EntryEffect::None
    };
    let exit_effect = if op.is_write_to_register(fixed_reg) {
        ExitEffect::RegWrite
    } else if op.is_memory_write() {
        ExitEffect::MemWrite
    } else if op.is_read_from_register(fixed_reg) {
        ExitEffect::RegRead
    } else {
        ExitEffect::None
    };
    (entry_effect, exit_effect)
}

fn exit_effect_to_flush_state(effect: ExitEffect) -> FlushState {
    match effect {
        ExitEffect::None => FlushState::Clean,
        ExitEffect::RegRead => FlushState::Clean,
        ExitEffect::RegWrite => FlushState::RegDirty,
        ExitEffect::MemWrite => FlushState::MemDirty,
    }
}

fn transform_state(state: FlushState, effect: ExitEffect) -> FlushState {
    match effect {
        ExitEffect::None => state,
        ExitEffect::RegRead => FlushState::Clean,
        ExitEffect::RegWrite => FlushState::RegDirty,
        ExitEffect::MemWrite => FlushState::MemDirty,
    }
}

fn transform_state_bwd(state: FlushState, effect: EntryEffect) -> FlushState {
    match effect {
        EntryEffect::None => state,
        EntryEffect::RegRead => FlushState::RegDirty,
        EntryEffect::RegWrite => FlushState::Clean,
        EntryEffect::MemAccess => FlushState::MemDirty,
    }
}

fn build_graph(blocks: &Vec<ir::Block>) -> Graph {
    let mut g = Graph::with_node_count(blocks.len());
    for (i, block) in blocks.iter().enumerate() {
        for j in block.tail.get_connections() {
            g.add_edge_unique(i, j);
        }
    }
    g
}
