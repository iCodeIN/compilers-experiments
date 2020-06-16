use crate::parse::lex::{Ident, Lit};
use psc::Pos;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct StxInfo {
    pos: Pos,
    /*
        ctx: HashMap<Ident, Type>,
        ...
    */
}

impl StxInfo {
    pub fn new(pos: Pos) -> Self {
        Self { pos }
    }
}

/*
program:
VAR <vars>: INTEGER;
BEGIN <stmts> END;
*/
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Program {
    decl: Vec<Ident>,
    body: Vec<Stmt>,

    info: StxInfo,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Stmt {
    /*
    assigment:
    <ident> := <alg_exp>
    */
    Assign {
        ident: Ident,
        exp: AlgExp,
        info: StxInfo,
    },
    /*
    condition:
    IF <bool_exp> THEN <stmt> ELSE <stmt>
    */
    Cond {
        cond: BoolExp,
        csq: Box<Stmt>,
        alt: Box<Stmt>,
        info: StxInfo,
    },

    /*
    while-loop
    WHILE <bool_exp> DO <stmt>
    */
    While {
        cond: BoolExp,
        body: Box<Stmt>,
        info: StxInfo,
    },

    /*
    sub-procedure:
    BEGIN <stmts> END
    */
    SubProc {
        body: Vec<Stmt>,
        info: StxInfo,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AlgExp {
    Var(Ident, StxInfo),
    Lit(Lit, StxInfo),
    Add(Box<AlgExp>, Box<AlgExp>, StxInfo),
    Sub(Box<AlgExp>, Box<AlgExp>, StxInfo),
    Mul(Box<AlgExp>, Box<AlgExp>, StxInfo),
    Div(Box<AlgExp>, Box<AlgExp>, StxInfo),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum BoolExp {
    LT(AlgExp, AlgExp, StxInfo),
    LE(AlgExp, AlgExp, StxInfo),
    EQ(AlgExp, AlgExp, StxInfo),
    GT(AlgExp, AlgExp, StxInfo),
    GE(AlgExp, AlgExp, StxInfo),
    NE(AlgExp, AlgExp, StxInfo),
}

/*
some factors
*/
impl Program {
    pub fn new(info: StxInfo, decl: Vec<Ident>, body: Vec<Stmt>) -> Self {
        Self { decl, body, info }
    }
}

impl Stmt {
    pub fn assign(info: StxInfo, ident: Ident, exp: AlgExp) -> Self {
        Self::Assign { ident, exp, info }
    }

    pub fn cond(info: StxInfo, cond: BoolExp, csq: Stmt, alt: Stmt) -> Self {
        Self::Cond {
            cond,
            csq: Box::new(csq),
            alt: Box::new(alt),
            info,
        }
    }

    pub fn while_loop(info: StxInfo, cond: BoolExp, body: Stmt) -> Self {
        Self::While {
            cond,
            body: Box::new(body),
            info,
        }
    }

    pub fn sub_proc(info: StxInfo, body: Vec<Stmt>) -> Self {
        Self::SubProc { body, info }
    }
}

impl AlgExp {
    pub fn info(&self) -> StxInfo {
        match self {
            AlgExp::Var(_, info)
            | AlgExp::Lit(_, info)
            | AlgExp::Add(_, _, info)
            | AlgExp::Sub(_, _, info)
            | AlgExp::Mul(_, _, info)
            | AlgExp::Div(_, _, info) => *info,
        }
    }

    pub fn var(info: StxInfo, ident: Ident) -> Self {
        Self::Var(ident, info)
    }

    pub fn lit(info: StxInfo, lit: Lit) -> Self {
        Self::Lit(lit, info)
    }

    pub fn add(info: StxInfo, lhs: Self, rhs: Self) -> Self {
        Self::Add(Box::new(lhs), Box::new(rhs), info)
    }

    pub fn sub(info: StxInfo, lhs: Self, rhs: Self) -> Self {
        Self::Sub(Box::new(lhs), Box::new(rhs), info)
    }

    pub fn mul(info: StxInfo, lhs: Self, rhs: Self) -> Self {
        Self::Mul(Box::new(lhs), Box::new(rhs), info)
    }

    pub fn div(info: StxInfo, lhs: Self, rhs: Self) -> Self {
        Self::Div(Box::new(lhs), Box::new(rhs), info)
    }
}

impl BoolExp {
    pub fn le(info: StxInfo, lhs: AlgExp, rhs: AlgExp) -> Self {
        Self::LE(lhs, rhs, info)
    }

    pub fn lt(info: StxInfo, lhs: AlgExp, rhs: AlgExp) -> Self {
        Self::LT(lhs, rhs, info)
    }

    pub fn ge(info: StxInfo, lhs: AlgExp, rhs: AlgExp) -> Self {
        Self::GE(lhs, rhs, info)
    }

    pub fn gt(info: StxInfo, lhs: AlgExp, rhs: AlgExp) -> Self {
        Self::GT(lhs, rhs, info)
    }

    pub fn eq(info: StxInfo, lhs: AlgExp, rhs: AlgExp) -> Self {
        Self::EQ(lhs, rhs, info)
    }

    pub fn ne(info: StxInfo, lhs: AlgExp, rhs: AlgExp) -> Self {
        Self::NE(lhs, rhs, info)
    }
}
