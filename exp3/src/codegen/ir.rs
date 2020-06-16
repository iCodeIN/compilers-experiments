use std::fmt::{Display, Formatter, Error};
use indexmap::map::IndexMap;

pub type Ident = String;
pub type Label = String;

#[derive(Clone, Debug, Default)]
pub struct Segment {
    seg: IndexMap<Label, Vec<Inst>>,
}

#[derive(Clone, Debug)]
pub struct IR {
    pub body: Segment,
}

#[derive(Clone, Debug)]
pub enum Inst {
    /// x := op y
    Assign(Ident, Op),
    /// goto
    Goto(Label),
    /// if val goto label
    Test(Val, Label),
    /// return
    Ret,
}

#[derive(Clone, Debug)]
pub enum Op {
    Val(Val),

    Add(Val, Val),
    Sub(Val, Val),
    Mul(Val, Val),
    Div(Val, Val),

    LT(Val, Val),
    LE(Val, Val),
    GT(Val, Val),
    GE(Val, Val),
    EQ(Val, Val),
    NE(Val, Val),

    And(Val, Val),
    Or(Val, Val),
    Not(Val),

    Call(Ident, Vec<Val>),
}

#[derive(Clone, Debug)]
pub enum Val {
    Num(i64),
    Var(Ident),
}

pub fn gen_ident() -> Ident {
    static mut ID: u32 = 0;
    let name = format!("t{}", unsafe { ID });
    unsafe {
        ID += 1;
    }
    name
}

pub fn gen_label(label: &str) -> Label {
    static mut ID: u32 = 0;
    let new_label = format!("{}{}", label, unsafe { ID });
    unsafe {
        ID += 1;
    }
    new_label
}

impl Segment {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn append(mut self, rhs: Self) -> Self {
        for inst in rhs.seg {
            self.seg.insert(inst.0, inst.1);
        }
        self
    }

    pub fn insert_inst(&mut self, inst: Inst) {
        if let Some((k, mut last_seg)) = self.seg.pop() {
            last_seg.push(inst);
            self.seg.insert(k, last_seg);
        }
    }

    pub fn insert_label(&mut self, label: Label) {
        self.seg.insert(label, vec![]);
    }
}

impl Display for Val {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Val::Num(n) => write!(f, "{}", n),
            Val::Var(id) => write!(f, "{}", id),
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Op::Val(v) => write!(f, "{}", v),
            Op::Add(e1, e2) => write!(f, "{} + {}", e1, e2),
            Op::Sub(e1, e2) => write!(f, "{} - {}", e1, e2),
            Op::Mul(e1, e2) => write!(f, "{} * {}", e1, e2),
            Op::Div(e1, e2) => write!(f, "{} / {}", e1, e2),
            Op::LT(e1, e2) => write!(f, "{} < {}", e1, e2),
            Op::LE(e1, e2) => write!(f, "{} <= {}", e1, e2),
            Op::GT(e1, e2) => write!(f, "{} > {}", e1, e2),
            Op::GE(e1, e2) => write!(f, "{} >= {}", e1, e2),
            Op::EQ(e1, e2) => write!(f, "{} == {}", e1, e2),
            Op::NE(e1, e2) => write!(f, "{} != {}", e1, e2),
            Op::And(e1, e2) => write!(f, "{} && {}", e1, e2),
            Op::Or(e1, e2) => write!(f, "{} || {}", e1, e2),
            Op::Not(e) => write!(f, "!{}", e),
            Op::Call(id, args) => {
                write!(f, "{}(", id)?;
                args.first()
                    .map(|arg| write!(f, "{}", arg))
                    .unwrap_or_else(|| Ok(()))?;
                for arg in args.iter().skip(1) {
                    write!(f, ", {}", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl Display for Inst {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Inst::Assign(id, op) => write!(f, "{} = {}", id, op),
            Inst::Goto(label) => write!(f, "goto {}", label),
            Inst::Test(v, label) => write!(f, "test {} {}", v, label),
            Inst::Ret => write!(f, "return"),
        }
    }
}

impl Display for IR {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        writeln!(f, "proc")?;
        for (label, seg) in &self.body.seg {
            writeln!(f, "{}:", label)?;
            for inst in seg {
                writeln!(f, "    {}", inst)?;
            }
        }
        Ok(())
    }
}
