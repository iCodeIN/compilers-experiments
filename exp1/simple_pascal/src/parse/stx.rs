use crate::parse::ast::{AlgExp, BoolExp, Program, Stmt, StxInfo};
use crate::parse::lex::{tok_ident, tok_keyword, tok_lit, tok_op, Ident, Keyword, Op, TokenStream};
use psc::{
    char, empty, lexeme, pos, pure, wrap, ParseFn, ParseLogger, ParseState, Parser, ParserExt, Pos,
};
use std::iter::once;

/*
<program> ::= <decl> <sub_proc>
*/
pub fn program() -> impl Parser<TokenStream, Target = Program> {
    info().map3(decl(), sub_proc(), Program::new)
}

/*
<decl>      ::= VAR <var_table>: INTEGER | eps
<var_table> ::= <var> | <var>, <var_table>
*/
fn decl<'a>() -> impl Parser<TokenStream, Target = Vec<Ident>> {
    let var = wrap(tok_keyword().filter(|(_, k)| k == &Keyword::Var)) << ' ';
    let ids = (wrap(tok_ident()) << lexeme(','))
        .many()
        .chain(tok_ident().map(once))
        .map(|ch| ch.map(|t| t.1))
        .map(Iterator::collect::<Vec<_>>); // (var,)* var
    let integer = tok_keyword().filter(|(_, k)| k == &Keyword::Integer);

    (var >> ids << lexeme(':') << integer << lexeme(';')) | pure(|| vec![])
}

/*
<sub_proc> ::= BEGIN <stmts> END
*/
fn sub_proc<'a>() -> impl Parser<TokenStream, Target = Vec<Stmt>> {
    let begin = wrap(tok_keyword().filter(|(_, k)| k == &Keyword::Begin));
    let stmts = (wrap(stmt()) << ';')
        .many()
        .chain(stmt().map(once))
        .map(Iterator::collect::<Vec<_>>);
    let end = tok_keyword().filter(|(_, k)| k == &Keyword::End);

    begin >> stmts << end
}

/*
<stmt>          ::= <assign> | <cond> | <while_loop> | <sub_proc>
<assign>        ::= <var> := <alg_exp>
<cond>          ::= IF <bool_exp> THEN <stmt> ELSE <stmt>
<while_loop>    ::= WHILE <bool_exp> DO <stmt>
*/
fn stmt<'a>() -> impl Parser<TokenStream, Target = Stmt> {
    fn parse_assign(stream: &mut TokenStream, logger: &mut ParseLogger) -> Option<Stmt> {
        let info = info().parse(stream, logger)?;

        let ident = tok_ident().parse(stream, logger)?.1;
        tok_op()
            .filter(|(_, k)| k == &Op::Assign)
            .parse(stream, logger)?;
        let exp = alg_exp().parse(stream, logger)?;

        Some(Stmt::assign(info, ident, exp))
    }

    fn parse_cond(stream: &mut TokenStream, logger: &mut ParseLogger) -> Option<Stmt> {
        let info = info().parse(stream, logger)?;

        tok_keyword()
            .filter(|(_, k)| k == &Keyword::If)
            .parse(stream, logger)?;
        char(' ').parse(stream, logger)?;
        let cond = relation().parse(stream, logger)?;
        tok_keyword()
            .filter(|(_, k)| k == &Keyword::Then)
            .parse(stream, logger)?;
        char(' ').parse(stream, logger)?;
        let csq = stmt().parse(stream, logger)?;
        tok_keyword()
            .filter(|(_, k)| k == &Keyword::Else)
            .parse(stream, logger)?;
        char(' ').parse(stream, logger)?;
        let alt = stmt().parse(stream, logger)?;

        Some(Stmt::cond(info, cond, csq, alt))
    }

    fn parse_while(stream: &mut TokenStream, logger: &mut ParseLogger) -> Option<Stmt> {
        let info = info().parse(stream, logger)?;

        tok_keyword()
            .filter(|(_, k)| k == &Keyword::While)
            .parse(stream, logger)?;
        char(' ').parse(stream, logger)?;
        let cond = relation().parse(stream, logger)?;
        char(' ').parse(stream, logger)?;
        tok_keyword()
            .filter(|(_, k)| k == &Keyword::Do)
            .parse(stream, logger)?;
        let body = stmt().parse(stream, logger)?;

        Some(Stmt::while_loop(info, cond, body))
    }

    fn parse_sub_proc(stream: &mut TokenStream, logger: &mut ParseLogger) -> Option<Stmt> {
        info()
            .map2(sub_proc(), Stmt::sub_proc)
            .parse(stream, logger)
    }

    wrap(ParseFn(parse_assign))
        | ParseFn(parse_cond)
        | ParseFn(parse_while)
        | ParseFn(parse_sub_proc)
}

/*
<alg_exp>   ::= <term> <alg_exp_>
<alg_exp_>  ::= + <term> <alg_exp_> | - <term> <alg_exp_> | eps
<term>      ::= <val> <term_>
<term_>     ::= * <val> <term_> | / <val> <term_> | eps
<val>       ::= var | unsigned | ( alg_exp )
*/
fn alg_exp<'a>() -> impl Parser<TokenStream, Target = AlgExp> {
    fn parse_alg_exp(stream: &mut ParseState, logger: &mut ParseLogger) -> Option<AlgExp> {
        let e1 = term().parse(stream, logger)?;
        let e2 = alg_exp_().parse(stream, logger)?;
        Some(match e2 {
            None => e1,
            Some(f) => f(e1),
        })
    }
    ParseFn(parse_alg_exp)
}

fn alg_exp_<'a>() -> impl Parser<TokenStream, Target = Option<Box<dyn FnOnce(AlgExp) -> AlgExp>>>
{
    fn parse_add(
        stream: &mut ParseState,
        logger: &mut ParseLogger,
    ) -> Option<Option<Box<dyn FnOnce(AlgExp) -> AlgExp>>> {
        tok_op()
            .filter(|(_, k)| k == &Op::Add)
            .parse(stream, logger)?;
        let e1 = term().parse(stream, logger)?;
        let e2 = alg_exp_().parse(stream, logger)?;
        Some(Some(match e2 {
            None => {
                Box::new(move |e: AlgExp| AlgExp::add(e.info(), e, e1)) as Box<dyn FnOnce(_) -> _>
            }
            Some(f) => Box::new(move |e| f(AlgExp::add(e.info(), e, e1))),
        }))
    }

    fn parse_sub(
        stream: &mut TokenStream,
        logger: &mut ParseLogger,
    ) -> Option<Option<Box<dyn FnOnce(AlgExp) -> AlgExp>>> {
        tok_op()
            .filter(|(_, k)| k == &Op::Sub)
            .parse(stream, logger)?;
        let e1 = term().parse(stream, logger)?;
        let e2 = alg_exp_().parse(stream, logger)?;
        Some(Some(match e2 {
            None => {
                Box::new(move |e: AlgExp| AlgExp::sub(e.info(), e, e1)) as Box<dyn FnOnce(_) -> _>
            }
            Some(f) => Box::new(move |e| f(AlgExp::sub(e.info(), e, e1))),
        }))
    }

    wrap(ParseFn(parse_add)) | ParseFn(parse_sub) | pure(|| None)
}

fn term<'a>() -> impl Parser<TokenStream, Target = AlgExp> {
    fn parse_term(stream: &mut ParseState, logger: &mut ParseLogger) -> Option<AlgExp> {
        let e1 = val().parse(stream, logger)?;
        let e2 = term_().parse(stream, logger)?;
        Some(match e2 {
            None => e1,
            Some(f) => f(e1),
        })
    }
    ParseFn(parse_term)
}

fn term_<'a>() -> impl Parser<TokenStream, Target = Option<Box<dyn FnOnce(AlgExp) -> AlgExp>>> {
    fn parse_mul(
        stream: &mut ParseState,
        logger: &mut ParseLogger,
    ) -> Option<Option<Box<dyn FnOnce(AlgExp) -> AlgExp>>> {
        tok_op()
            .filter(|(_, k)| k == &Op::Mul)
            .parse(stream, logger)?;
        let e1 = val().parse(stream, logger)?;
        let e2 = term_().parse(stream, logger)?;
        Some(Some(match e2 {
            None => {
                Box::new(move |e: AlgExp| AlgExp::mul(e.info(), e, e1)) as Box<dyn FnOnce(_) -> _>
            }
            Some(f) => Box::new(move |e| f(AlgExp::mul(e.info(), e, e1))),
        }))
    }

    fn parse_div(
        stream: &mut TokenStream,
        logger: &mut ParseLogger,
    ) -> Option<Option<Box<dyn FnOnce(AlgExp) -> AlgExp>>> {
        tok_op()
            .filter(|(_, k)| k == &Op::Div)
            .parse(stream, logger)?;
        let e1 = val().parse(stream, logger)?;
        let e2 = term_().parse(stream, logger)?;
        Some(Some(match e2 {
            None => {
                Box::new(move |e: AlgExp| AlgExp::div(e.info(), e, e1)) as Box<dyn FnOnce(_) -> _>
            }
            Some(f) => Box::new(move |e| f(AlgExp::div(e.info(), e, e1))),
        }))
    }

    wrap(ParseFn(parse_mul)) | ParseFn(parse_div) | pure(|| None)
}

fn val<'a>() -> impl Parser<TokenStream, Target = AlgExp> {
    let var = info().map2(tok_ident(), |info, (_, id)| AlgExp::var(info, id));
    let lit = info().map2(tok_lit(), |info, (_, lit)| AlgExp::lit(info, lit));
    fn parse_paren(stream: &mut ParseState, logger: &mut ParseLogger) -> Option<AlgExp> {
        (wrap('(') >> alg_exp() << ')').parse(stream, logger)
    }

    wrap(var) | lit | ParseFn(parse_paren)
}

/*
<relation>      ::= <alg_exp> <relation_op> <alg_exp>
<relation_op>   ::= < | <= | = | >= | > | <>
*/
fn relation<'a>() -> impl Parser<TokenStream, Target = BoolExp> {
    let op = tok_op()
        .filter(|(_, op)| {
            use Op::*;
            match op {
                LT | LE | EQ | GT | GE | NE => true,
                _ => false,
            }
        })
        .map(|t| t.1);

    let op = op.map(|op: Op| {
        move |info, lhs, rhs| match op {
            Op::LT => BoolExp::lt(info, lhs, rhs),
            Op::LE => BoolExp::le(info, lhs, rhs),
            Op::EQ => BoolExp::eq(info, lhs, rhs),
            Op::GE => BoolExp::ge(info, lhs, rhs),
            Op::GT => BoolExp::gt(info, lhs, rhs),
            Op::NE => BoolExp::ne(info, lhs, rhs),
            _ => unreachable!(),
        }
    });

    info().map4(alg_exp(), op, alg_exp(), |info, lhs, f, rhs| {
        f(info, lhs, rhs)
    })
}

fn info<'a>() -> impl Parser<TokenStream, Target = StxInfo> {
    pos().map(StxInfo::new)
}
