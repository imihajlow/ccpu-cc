use std::mem;

use bitflags::bitflags;

use crate::graph::Graph;
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
    let mut flush_instructions = Vec::new();
    for (fixed_reg, address_reg, width) in frame.fixed_regs_iter() {
        let instrs = construct_flush_instructions_for_reg(
            body,
            fixed_reg,
            address_reg,
            width,
            &g,
            &gt,
            &exit_nodes,
        );
        flush_instructions.extend(instrs.into_iter().map(|(block_index, pos, instr)| {
            (block_index, pos, instr, fixed_reg, address_reg, width)
        }));
    }
    for (block_index, pos, instr, fixed_reg, address_reg, width) in flush_instructions.into_iter() {
        let op = construct_flush_instruction(instr, fixed_reg, address_reg, width);
        let block = &mut body[block_index].ops;
        match pos {
            BlockPosition::Front => block.insert(0, op),
            BlockPosition::Back => block.push(op),
        }
    }
}

bitflags! {
    #[derive(Default)]
    struct Device: u32 {
        const REG = 1 << 0;
        const MEM = 1 << 1;
    }
}

#[derive(Debug, Clone, Copy)]
struct BlockDescription {
    /// These devices are expected to contain actual values on block entry.
    expects_synced: Device,
    /// These devices might be out of sync after this block.
    desyncs: Device,
    /// During execution of this block, these devices are reset to new values.
    provides: Device,
    /// The sync expectation of these devices don't propagate back through this block.
    sync_expectation_barrier: Device,
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

fn construct_flush_instructions_for_reg(
    body: &mut Vec<ir::Block>,
    fixed_reg: ir::VirtualReg,
    address_reg: ir::VirtualReg,
    width: ir::Width,
    g: &Graph,
    gt: &Graph,
    exit_nodes: &[usize],
) -> Vec<(usize, BlockPosition, FlushInstruction)> {
    let mut descriptions = Vec::with_capacity(body.len());
    for block in body.iter_mut() {
        descriptions.push(insert_inner_flush_instructions(
            block,
            fixed_reg,
            address_reg,
            width,
        ));
    }
    // println!("reg = {} @ {}", fixed_reg, address_reg);
    // println!("descriptions = {:#?}", descriptions);

    let mut modifications = Vec::new();
    loop {
        let desynced = propagate_desynced(g, &descriptions);
        // println!("desynced = {:#?}", desynced);
        // println!("exit_nodes = {:?}", exit_nodes);
        let expected = propagate_expected(gt, exit_nodes, &descriptions);
        // println!("expected = {:#?}", expected);
        if is_consistent(&desynced, &expected) {
            break;
        }
        let fps = find_possible_flush_positions(&desynced, &expected);
        // println!("flush_positions = {:?}", fps);

        let pos = fps
            .iter()
            .min_by(|(i, _, _), (j, _, _)| body[*i].loop_depth.cmp(&body[*j].loop_depth))
            .unwrap();
        // println!("use pos {}", pos.0);
        modifications.push(pos.clone());
        descriptions[pos.0].update_with_instruction(pos.1, pos.2);
    }
    modifications
}

fn propagate_desynced(g: &Graph, descriptions: &[BlockDescription]) -> Vec<(Device, Device)> {
    assert_eq!(g.get_node_count(), descriptions.len());
    assert!(g.get_node_count() > 0);
    let mut result = vec![(None, None); g.get_node_count()];
    fn dfs(
        start: usize,
        desynced_on_start: Device,
        g: &Graph,
        descriptions: &[BlockDescription],
        result: &mut Vec<(Option<Device>, Option<Device>)>,
    ) {
        let descr = descriptions[start];
        let last_desynced_on_start = result[start].0;
        if let Some(last_desynced_on_start) = last_desynced_on_start {
            // This node is already visited
            if last_desynced_on_start != desynced_on_start {
                // Entry state is updated
                let new_desynced_on_start = desynced_on_start | last_desynced_on_start;
                result[start].0 = Some(new_desynced_on_start);
                let new_desynced_on_exit = descr.propagate_desynced(new_desynced_on_start);
                if Some(new_desynced_on_exit) != result[start].1 {
                    // Exit state updated - visit children
                    result[start].1 = Some(new_desynced_on_exit);
                    for child in g.get_children(start) {
                        dfs(*child, new_desynced_on_exit, g, descriptions, result);
                    }
                }
            }
        } else {
            // First time visiting this node
            result[start].0 = Some(desynced_on_start);
            let new_desynced_on_exit = descr.propagate_desynced(desynced_on_start);
            result[start].1 = Some(new_desynced_on_exit);
            for child in g.get_children(start) {
                dfs(*child, new_desynced_on_exit, g, descriptions, result);
            }
        }
    }
    dfs(0, Device::empty(), g, descriptions, &mut result);
    result
        .into_iter()
        .map(|(entry, exit)| (entry.unwrap_or_default(), exit.unwrap_or_default()))
        .collect()
}

fn propagate_expected(
    gt: &Graph,
    exit_nodes: &[usize],
    descriptions: &[BlockDescription],
) -> Vec<(Device, Device)> {
    assert_eq!(gt.get_node_count(), descriptions.len());
    assert!(gt.get_node_count() > 0);
    assert!(exit_nodes.len() > 0);
    let mut result = vec![(None, None); gt.get_node_count()];
    fn dfs(
        start: usize,
        expected_on_exit: Device,
        gt: &Graph,
        descriptions: &[BlockDescription],
        result: &mut Vec<(Option<Device>, Option<Device>)>,
    ) {
        let descr = descriptions[start];
        let last_expected_on_exit = result[start].1;
        if let Some(last_expected_on_exit) = last_expected_on_exit {
            // This node is already visited
            if last_expected_on_exit != expected_on_exit {
                // Exit state is updated
                let new_expected_on_exit = expected_on_exit | last_expected_on_exit;
                result[start].1 = Some(new_expected_on_exit);
                let new_expected_on_entry = descr.propagate_expected(new_expected_on_exit);
                if Some(new_expected_on_entry) != result[start].0 {
                    // Entry state updated - visit children
                    result[start].0 = Some(new_expected_on_entry);
                    for child in gt.get_children(start) {
                        dfs(*child, new_expected_on_entry, gt, descriptions, result);
                    }
                }
            }
        } else {
            // First time visiting this node
            result[start].1 = Some(expected_on_exit);
            let expected_on_entry = descr.propagate_expected(expected_on_exit);
            result[start].0 = Some(expected_on_entry);
            for child in gt.get_children(start) {
                dfs(*child, expected_on_entry, gt, descriptions, result);
            }
        }
    }
    for node in exit_nodes {
        dfs(*node, Device::empty(), gt, descriptions, &mut result);
    }
    result
        .into_iter()
        .map(|(entry, exit)| (entry.unwrap(), exit.unwrap()))
        .collect()
}

fn is_consistent(desynced: &[(Device, Device)], expected: &[(Device, Device)]) -> bool {
    assert_eq!(desynced.len(), expected.len());
    for i in 0..desynced.len() {
        if desynced[i].0.is_all() || desynced[i].1.is_all() {
            return false;
        }
        if !(expected[i].0 & desynced[i].0).is_empty() {
            return false;
        }
        if !(expected[i].1 & desynced[i].1).is_empty() {
            return false;
        }
    }
    true
}

fn find_possible_flush_positions(
    desynced: &[(Device, Device)],
    expected: &[(Device, Device)],
) -> Vec<(usize, BlockPosition, FlushInstruction)> {
    assert_eq!(desynced.len(), expected.len());
    let mut result = Vec::new();
    for i in 0..desynced.len() {
        if let Some(instr) = get_flush_instruction(desynced[i].0, expected[i].0) {
            result.push((i, BlockPosition::Front, instr))
        }
        if let Some(instr) = get_flush_instruction(desynced[i].1, expected[i].1) {
            result.push((i, BlockPosition::Back, instr))
        }
    }
    result
}

fn get_flush_instruction(desynced: Device, expected: Device) -> Option<FlushInstruction> {
    if !desynced.is_all() {
        let device = desynced & expected;
        if device == Device::REG {
            Some(FlushInstruction::Load)
        } else if device == Device::MEM {
            Some(FlushInstruction::Store)
        } else {
            None
        }
    } else {
        None
    }
}

fn insert_inner_flush_instructions(
    block: &mut ir::Block,
    fixed_reg: ir::VirtualReg,
    address_reg: ir::VirtualReg,
    width: ir::Width,
) -> BlockDescription {
    enum Operation {
        Op(ir::Op),
        Flush(FlushInstruction),
    }

    let ops = mem::replace(&mut block.ops, Vec::new());

    // Devices which are expected to have actual values at the beginning of the block
    let mut expected = if block.tail.is_read_from_register(fixed_reg) {
        Device::REG
    } else {
        Device::empty()
    };
    let mut new_rev_ops = Vec::with_capacity(ops.len());
    for op in ops.into_iter().rev() {
        if expected.contains(Device::MEM) && op.is_write_to_register(fixed_reg) {
            new_rev_ops.push(Operation::Flush(FlushInstruction::Store));
            expected = Device::empty();
        } else if expected.contains(Device::REG) && op.is_memory_write() {
            new_rev_ops.push(Operation::Flush(FlushInstruction::Load));
            expected = Device::empty();
        }
        if op.is_write_to_register(fixed_reg) {
            expected &= !Device::REG;
        }
        if op.is_read_from_register(fixed_reg) {
            expected |= Device::REG;
        }
        if op.is_memory_write() || op.is_memory_read() {
            expected |= Device::MEM;
        }
        new_rev_ops.push(Operation::Op(op))
    }

    // Devices whose values are provied by this block
    let mut provides = Device::empty();
    for op in new_rev_ops.iter() {
        match op {
            Operation::Flush(FlushInstruction::Load) => provides |= Device::REG,
            Operation::Flush(FlushInstruction::Store) => provides |= Device::MEM,
            Operation::Op(op) if op.is_write_to_register(fixed_reg) => provides |= Device::REG,
            _ => (),
        }
    }

    // Devices whose values may be out of sync after this block
    let mut desynced = Device::empty();
    for op in new_rev_ops.iter().rev() {
        match op {
            Operation::Flush(FlushInstruction::Load) => desynced = Device::empty(),
            Operation::Flush(FlushInstruction::Store) => desynced = Device::empty(),
            Operation::Op(op) if op.is_write_to_register(fixed_reg) => desynced = Device::MEM,
            Operation::Op(op) if op.is_memory_write() => desynced = Device::REG,
            _ => (),
        }
    }
    assert_ne!(desynced, Device::all());

    // Devices which are expected to be synchronized before and after this block independently.
    let mut sync_expectation_barrier = Device::empty();
    for op in new_rev_ops.iter().rev() {
        match op {
            Operation::Flush(FlushInstruction::Load) => sync_expectation_barrier = Device::all(),
            Operation::Flush(FlushInstruction::Store) => sync_expectation_barrier = Device::all(),
            Operation::Op(op) if op.is_write_to_register(fixed_reg) => {
                sync_expectation_barrier = Device::all()
            }
            Operation::Op(op) if op.is_read_from_register(fixed_reg) => {
                sync_expectation_barrier |= Device::REG
            }
            Operation::Op(op) if op.is_memory_write() => sync_expectation_barrier = Device::all(),
            Operation::Op(op) if op.is_memory_read() => sync_expectation_barrier |= Device::MEM,
            _ => (),
        }
    }

    block.ops = new_rev_ops
        .into_iter()
        .rev()
        .map(|op| match op {
            Operation::Op(op) => op,
            Operation::Flush(instr) => {
                construct_flush_instruction(instr, fixed_reg, address_reg, width)
            }
        })
        .collect();
    BlockDescription {
        expects_synced: expected,
        desyncs: desynced,
        provides,
        sync_expectation_barrier,
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

fn construct_flush_instruction(
    instr: FlushInstruction,
    fixed_reg: ir::VirtualReg,
    address_reg: ir::VirtualReg,
    width: ir::Width,
) -> ir::Op {
    match instr {
        FlushInstruction::Store => ir::Op::Store(ir::StoreOp {
            dst_addr: ir::Scalar::Var(ir::VarLocation::Local(address_reg)),
            src: ir::Scalar::Var(ir::VarLocation::Local(fixed_reg)),
            width,
        }),
        FlushInstruction::Load => ir::Op::Load(ir::LoadOp {
            src_addr: ir::Scalar::Var(ir::VarLocation::Local(address_reg)),
            dst: ir::VarLocation::Local(fixed_reg),
            width,
        }),
    }
}

impl BlockDescription {
    fn propagate_desynced(&self, desynced_on_start: Device) -> Device {
        (desynced_on_start & !self.provides) | self.desyncs
    }

    fn propagate_expected(&self, expected_on_exit: Device) -> Device {
        (expected_on_exit & !self.sync_expectation_barrier) | self.expects_synced
    }

    fn update_with_instruction(&mut self, pos: BlockPosition, instr: FlushInstruction) {
        match (pos, instr) {
            (BlockPosition::Front, FlushInstruction::Store) => {
                self.expects_synced &= !Device::MEM;
                self.provides |= Device::MEM;
            }
            (BlockPosition::Front, FlushInstruction::Load) => {
                self.expects_synced &= !Device::REG;
                self.provides |= Device::REG;
            }
            (BlockPosition::Back, FlushInstruction::Store) => {
                self.desyncs = Device::empty();
                self.expects_synced =
                    (Device::REG & !self.sync_expectation_barrier) | self.expects_synced;
                self.provides |= Device::MEM;
            }
            (BlockPosition::Back, FlushInstruction::Load) => {
                self.desyncs = Device::empty();
                self.expects_synced =
                    (Device::MEM & !self.sync_expectation_barrier) | self.expects_synced;
                self.provides |= Device::REG;
            }
        }
        self.sync_expectation_barrier = Device::all();
    }
}
