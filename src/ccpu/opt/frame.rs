use crate::ccpu::global::get_static_frame_symbol;
use crate::ir;

pub fn resolve_frame_pointer(blocks: &mut [ir::Block], fn_name: &str, is_reentrant: bool) -> bool {
    // the FramePointer operation only happens once in the first block
    for op in &mut blocks.first_mut().unwrap().ops {
        if let ir::Op::FramePointer(reg) = op {
            if is_reentrant {
                todo!()
            } else {
                *op = ir::Op::Copy(ir::UnaryUnsignedOp {
                    dst: ir::VarLocation::Local(*reg),
                    src: ir::Scalar::SymbolOffset(get_static_frame_symbol(fn_name), 0),
                    width: ir::Width::PTR_WIDTH,
                });
                return true;
            }
        }
    }
    false
}
