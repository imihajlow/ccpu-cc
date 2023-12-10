use std::collections::HashMap;

use crate::generic_ir::Width;

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum InstructionClass {
    Other,
    Copy(Width),
    Bool(Width),
    BoolInv(Width),
    Add {
        w: Width,
        has_const: bool,
    },
    Sub {
        w: Width,
        lhs_const: bool,
        rhs_const: bool,
    },
    Mul {
        w: Width,
        has_const: bool,
    },
    Div {
        w: Width,
        lhs_const: bool,
        rhs_const: bool,
    },
    Mod {
        w: Width,
        lhs_const: bool,
        rhs_const: bool,
    },
    BAnd {
        w: Width,
        has_const: bool,
    },
    BOr {
        w: Width,
        has_const: bool,
    },
    BXor {
        w: Width,
        has_const: bool,
    },
    LShift {
        w: Width,
        lhs_const: bool,
        rhs_const: bool,
    },
    RShift {
        w: Width,
        lhs_const: bool,
        rhs_const: bool,
    },
    Neg(Width),
    Not(Width),
    Compare {
        w: Width,
        lhs_const: bool,
        rhs_const: bool,
    },
    Conv {
        from: Width,
        to: Width,
        sign: bool,
    },
    Store {
        addr_const: bool,
        val_const: bool,
        w: Width,
    },
    Load {
        addr_const: bool,
        w: Width,
    },
    Call,
    Memcpy,
    IntrinCall,
    ByteSwap(Width),
}

pub struct Stats {
    m: HashMap<InstructionClass, Record>,
}

struct Record {
    ir_count: usize,
    bytes_count: usize,
}

impl Stats {
    pub fn new() -> Self {
        Self { m: HashMap::new() }
    }

    pub fn rec(&mut self, c: InstructionClass, len: usize) {
        if let Some(cnt) = self.m.get_mut(&c) {
            cnt.add(len);
        } else {
            self.m.insert(c, Record::new(len));
        }
    }

    pub fn print_stats(&self) {
        let mut sorted = self.m.iter().collect::<Vec<_>>();
        sorted.sort_by(|(_, v1), (_, v2)| v2.bytes_count.cmp(&v1.bytes_count));
        println!("{:30} {:>6} {:>6} {:>6}", "class", "count", "bytes", "bpi");
        println!("===================================================");
        for (k, v) in sorted {
            let bpi = if v.ir_count > 0 {
                v.bytes_count as f32 / v.ir_count as f32
            } else {
                0.0
            };
            println!(
                "{:<30} {:>6} {:>6} {:>6.1}",
                k, v.ir_count, v.bytes_count, bpi
            );
        }
    }
}

impl Record {
    fn new(bytes: usize) -> Self {
        Record {
            ir_count: 1,
            bytes_count: bytes,
        }
    }

    fn add(&mut self, bytes: usize) {
        self.ir_count += 1;
        self.bytes_count += bytes;
    }
}

fn get_const_spec(c: &bool) -> &'static str {
    if *c {
        "const"
    } else {
        "var"
    }
}

impl std::fmt::Display for InstructionClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        use InstructionClass::*;
        let s = match self {
            Other => format!("other"),
            Copy(w) => format!("copy({})", w),
            Bool(w) => format!("bool({})", w),
            BoolInv(w) => format!("boolinv({})", w),
            Add { w, has_const } => format!("add {}({})", get_const_spec(has_const), w),
            Sub {
                w,
                lhs_const,
                rhs_const,
            } => format!(
                "sub {} {}({})",
                get_const_spec(lhs_const),
                get_const_spec(rhs_const),
                w
            ),
            Mul { w, has_const } => format!("mul {}({})", get_const_spec(has_const), w),
            Div {
                w,
                lhs_const,
                rhs_const,
            } => format!(
                "div {} {}({})",
                get_const_spec(lhs_const),
                get_const_spec(rhs_const),
                w
            ),
            Mod {
                w,
                lhs_const,
                rhs_const,
            } => format!(
                "mod {} {}({})",
                get_const_spec(lhs_const),
                get_const_spec(rhs_const),
                w
            ),
            BAnd { w, has_const } => format!("band {}({})", get_const_spec(has_const), w),
            BOr { w, has_const } => format!("bor {}({})", get_const_spec(has_const), w),
            BXor { w, has_const } => format!("bxor {}({})", get_const_spec(has_const), w),
            LShift {
                w,
                lhs_const,
                rhs_const,
            } => format!(
                "lshift {} {}({})",
                get_const_spec(lhs_const),
                get_const_spec(rhs_const),
                w
            ),
            RShift {
                w,
                lhs_const,
                rhs_const,
            } => format!(
                "rshift {} {}({})",
                get_const_spec(lhs_const),
                get_const_spec(rhs_const),
                w
            ),
            Neg(w) => format!("neg({})", w),
            Not(w) => format!("not({})", w),
            Compare {
                w,
                lhs_const,
                rhs_const,
            } => format!(
                "cmp {} {}({})",
                get_const_spec(lhs_const),
                get_const_spec(rhs_const),
                w
            ),
            Conv { from, to, sign } => format!(
                "conv from {} to {}, {}",
                from,
                to,
                if *sign { "signed" } else { "unsigned" }
            ),
            Store {
                addr_const,
                val_const,
                w,
            } => format!(
                "store {} <- {}({})",
                get_const_spec(addr_const),
                get_const_spec(val_const),
                w
            ),
            Load { addr_const, w } => format!("load from {}({})", get_const_spec(addr_const), w),
            Call => "call".to_string(),
            Memcpy => "memcpy".to_string(),
            IntrinCall => "intrin_call".to_string(),
            ByteSwap(w) => format!("byte_swap({})", w),
        };
        if let Some(width) = f.width() {
            write!(f, "{:1$}", s, width)
        } else {
            write!(f, "{}", s)
        }
    }
}
