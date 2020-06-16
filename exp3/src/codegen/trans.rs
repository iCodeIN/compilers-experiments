use crate::codegen::ir::{Segment, IR, gen_label, Ident, Op, gen_ident};
use crate::parse::ast::{Program, AlgExp, BoolExp, Stmt};
use crate::codegen::ir::Inst::{Ret, Assign, Test, Goto};
use crate::codegen::ir::Val::{Var, Num};

pub fn trans(ast: Program) -> IR {
    let mut seg = Segment::new();
    let entry = gen_label("entry");
    seg.insert_label(entry);
    for stm in ast.body {
        trans_stm(stm, &mut seg);
    }
    seg.insert_inst(Ret);
    IR {
        body: seg,
    }
}

pub fn trans_stm(stm: Stmt, seg: &mut Segment) {
    match stm {
        Stmt::Assign { ident, exp, .. } => {
            trans_alg(exp, ident.0, seg);
        }
        Stmt::Cond { cond, csq, alt, .. } => {
            /*
                <cond to t>
                test t if_csq
                <alt>
                goto after_if
            if_csq:
                <csq>
            after_if:
            */
            let temp = gen_ident();
            let if_csq = gen_label("if_csq");
            let after_if = gen_label("after_if");

            trans_bool(cond, temp.clone(), seg);
            seg.insert_inst(Test(Var(temp), if_csq.clone()));
            trans_stm(*alt, seg);
            seg.insert_inst(Goto(after_if.clone()));
            seg.insert_label(if_csq);
            trans_stm(*csq, seg);
            seg.insert_label(after_if);
        }
        Stmt::While { cond, body, .. } => {
            /*
            while_entry:
                <cond to t>
                test t while_body
                goto after_while
            while_body:
                <body>
                goto while_entry
            after_while:
            */
            let temp = gen_ident();
            let while_entry = gen_label("while_entry");
            let while_body = gen_label("while_body");
            let after_while = gen_label("after_while");

            seg.insert_label(while_entry.clone());
            trans_bool(cond, temp.clone(), seg);
            seg.insert_inst(Test(Var(temp), while_body.clone()));
            seg.insert_inst(Goto(after_while.clone()));
            seg.insert_label(while_body);
            trans_stm(*body, seg);
            seg.insert_inst(Goto(while_entry));
            seg.insert_label(after_while);
        }
        Stmt::SubProc { body, .. } => {
            body.into_iter().for_each(|stm| trans_stm(stm, seg));
        }
    }
}



pub fn trans_alg(expr: AlgExp, target: Ident, seg: &mut Segment) {

    /*
    <e1 to t1>
    <e2 to t2>
    target = e1 op e2
    */
    let (op, e1, e2): (fn(_, _) -> _, _, _) = match expr {
        AlgExp::Add(e1, e2, _) => (Op::Add, *e1, *e2),
        AlgExp::Sub(e1, e2, _) => (Op::Sub, *e1, *e2),
        AlgExp::Mul(e1, e2, _) => (Op::Mul, *e1, *e2),
        AlgExp::Div(e1, e2, _) => (Op::Div, *e1, *e2),
        AlgExp::Lit(crate::parse::lex::Lit::Unsigned(n), _) => {
            seg.insert_inst(Assign(target, Op::Val(Num(n as i64))));
            return;
        }
        AlgExp::Var(id, _) => {
            seg.insert_inst(Assign(target, Op::Val(Var(id.0))));
            return;
        }
    };

    let temp1 = gen_ident();
    let temp2 = gen_ident();
    trans_alg(e1, temp1.clone(), seg);
    trans_alg(e2, temp2.clone(), seg);
    seg.insert_inst(Assign(target, op(Var(temp1), Var(temp2))));
}

pub fn trans_bool(expr: BoolExp, target: Ident, seg: &mut Segment) {
    /*
    <e1 to t1>
    <e2 to t2>
    target = e1 op e2
    */
    let (op, e1, e2): (fn(_, _) -> _, _, _) = match expr {
        BoolExp::GT(e1, e2, _) => (Op::GT, e1, e2),
        BoolExp::GE(e1, e2, _) => (Op::GE, e1, e2),
        BoolExp::LT(e1, e2, _) => (Op::LT, e1, e2),
        BoolExp::LE(e1, e2, _) => (Op::LE, e1, e2),
        BoolExp::EQ(e1, e2, _) => (Op::EQ, e1, e2),
        BoolExp::NE(e1, e2, _) => (Op::NE, e1, e2),
    };

    let temp1 = gen_ident();
    let temp2 = gen_ident();
    trans_alg(e1, temp1.clone(), seg);
    trans_alg(e2, temp2.clone(), seg);
    seg.insert_inst(Assign(target, op(Var(temp1), Var(temp2))));
}
