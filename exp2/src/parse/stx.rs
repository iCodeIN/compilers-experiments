use crate::parse::ast::{AlgExp, BoolExp, Program, Stmt, StxInfo};
use crate::parse::lex::{Ident, Keyword, Lit, Op, Token, TokenStream};
use psc::core::traits::covert::IntoParser;
use psc::{pure, wrap, Msg, ParseFn, ParseLogger, Parser, ParserExt};
use std::iter::once;

pub fn parser() -> impl Parser<TokenStream, Target = Program> {
    program()
}

/**
<program> ::= <decl> <sub_proc>
*/
fn program() -> impl Parser<TokenStream, Target = Program> {
    info().map3(decl(), sub_proc(), Program::new)
}

/**
<decl>      ::= VAR <var_table>: INTEGER | eps
<var_table> ::= <var> | <var>, <var_table>
*/
fn decl() -> impl Parser<TokenStream, Target = Vec<Ident>> {
    let var = wrap(Token::Keyword(Keyword::Var));
    let ids = (wrap(IdentParser) << Token::Comma)
        .many()
        .chain(IdentParser.map(once))
        .map(Iterator::collect::<Vec<_>>); // (var,)* var
    let integer = wrap(Token::Keyword(Keyword::Integer));

    (var >> ids << Token::Colon << integer << Token::Semi) | pure(|| vec![])
}

/*
<sub_proc> ::= BEGIN <stmts> END
*/
fn sub_proc() -> impl Parser<TokenStream, Target = Vec<Stmt>> {
    let begin = wrap(Token::Keyword(Keyword::Begin));
    let stmts = stmt().some();
    let end = Token::Keyword(Keyword::End);

    begin >> stmts << end
}

/**
<stmt>          ::= <assign> | <cond> | <while_loop> | <sub_proc>
<assign>        ::= <var> := <alg_exp>;
<cond>          ::= IF <bool_exp> THEN <stmt> ELSE <stmt>
<while_loop>    ::= WHILE <bool_exp> DO <stmt>
*/
fn stmt() -> impl Parser<TokenStream, Target = Stmt> {
    fn parse_assign(stream: &mut TokenStream, logger: &mut ParseLogger) -> Option<Stmt> {
        let info = info().parse(stream, logger)?;

        let ident = IdentParser.parse(stream, logger)?;
        wrap(Token::Op(Op::Assign)).parse(stream, logger)?;
        let exp = alg_exp().parse(stream, logger)?;
        wrap(Token::Semi).parse(stream, logger)?;
        Some(Stmt::assign(info, ident, exp))
    }

    fn parse_cond(stream: &mut TokenStream, logger: &mut ParseLogger) -> Option<Stmt> {
        let info = info().parse(stream, logger)?;

        wrap(Token::Keyword(Keyword::If)).parse(stream, logger)?;
        let cond = relation().parse(stream, logger)?;
        wrap(Token::Keyword(Keyword::Then)).parse(stream, logger)?;
        let csq = stmt().parse(stream, logger)?;
        wrap(Token::Keyword(Keyword::Else)).parse(stream, logger)?;
        let alt = stmt().parse(stream, logger)?;

        Some(Stmt::cond(info, cond, csq, alt))
    }

    fn parse_while(stream: &mut TokenStream, logger: &mut ParseLogger) -> Option<Stmt> {
        let info = info().parse(stream, logger)?;

        wrap(Token::Keyword(Keyword::While)).parse(stream, logger)?;
        let cond = relation().parse(stream, logger)?;
        wrap(Token::Keyword(Keyword::Do)).parse(stream, logger)?;
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

/**
<alg_exp>   ::= <term> <alg_exp_>
<alg_exp_>  ::= + <term> <alg_exp_> | - <term> <alg_exp_> | eps
<term>      ::= <val> <term_>
<term_>     ::= * <val> <term_> | / <val> <term_> | eps
<val>       ::= var | unsigned | ( alg_exp )
*/
pub fn alg_exp() -> impl Parser<TokenStream, Target = AlgExp> {
    fn parse_alg_exp(stream: &mut TokenStream, logger: &mut ParseLogger) -> Option<AlgExp> {
        let e1 = term().parse(stream, logger)?;
        let e2 = alg_exp_().parse(stream, logger)?;

        Some(match e2 {
            None => e1,
            Some(f) => f(e1),
        })
    }
    ParseFn(parse_alg_exp)
}

fn alg_exp_() -> impl Parser<TokenStream, Target = Option<Box<dyn FnOnce(AlgExp) -> AlgExp>>> {
    fn parse_add(
        stream: &mut TokenStream,
        logger: &mut ParseLogger,
    ) -> Option<Option<Box<dyn FnOnce(AlgExp) -> AlgExp>>> {
        wrap(Token::Op(Op::Add)).parse(stream, logger)?;
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
        wrap(Token::Op(Op::Sub)).parse(stream, logger)?;
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

fn term() -> impl Parser<TokenStream, Target = AlgExp> {
    fn parse_term(stream: &mut TokenStream, logger: &mut ParseLogger) -> Option<AlgExp> {
        let e1 = val().parse(stream, logger)?;
        let e2 = term_().parse(stream, logger)?;
        Some(match e2 {
            None => e1,
            Some(f) => f(e1),
        })
    }
    ParseFn(parse_term)
}

fn term_() -> impl Parser<TokenStream, Target = Option<Box<dyn FnOnce(AlgExp) -> AlgExp>>> {
    fn parse_mul(
        stream: &mut TokenStream,
        logger: &mut ParseLogger,
    ) -> Option<Option<Box<dyn FnOnce(AlgExp) -> AlgExp>>> {
        wrap(Token::Op(Op::Mul)).parse(stream, logger)?;
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
        wrap(Token::Op(Op::Div)).parse(stream, logger)?;
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

fn val() -> impl Parser<TokenStream, Target = AlgExp> {
    let var = info().map2(IdentParser, AlgExp::var);
    let lit = info().map2(LitParser, AlgExp::lit);
    fn parse_paren(stream: &mut TokenStream, logger: &mut ParseLogger) -> Option<AlgExp> {
        (wrap(Token::LPar) >> alg_exp() << Token::RPar).parse(stream, logger)
    }

    wrap(var) | lit | ParseFn(parse_paren)
}

/**
<relation>      ::= <alg_exp> <relation_op> <alg_exp>
<relation_op>   ::= < | <= | = | >= | > | <>
*/
fn relation() -> impl Parser<TokenStream, Target = BoolExp> {
    let op = OpParser.filter(Op::is_rela).map(|op: Op| {
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

fn info() -> impl Parser<TokenStream, Target = StxInfo> {
    Info
}

#[derive(Copy, Clone, Debug)]
struct Info;

impl Parser<TokenStream> for Info {
    type Target = StxInfo;

    fn parse(&self, stream: &mut TokenStream, _: &mut ParseLogger) -> Option<Self::Target> {
        stream.clone().next().map(|t| t.0).map(StxInfo::new)
    }
}
#[derive(Clone, Debug)]
pub struct TokenParser(Token);

impl Parser<TokenStream> for TokenParser {
    type Target = Token;

    fn parse(&self, stream: &mut TokenStream, logger: &mut ParseLogger) -> Option<Self::Target> {
        stream.next().and_then(|(pos, tok)| {
            if tok == self.0 {
                Some(tok)
            } else {
                logger.with(Msg::Err(format!(
                    "expected {:?}, unexpected `{:?}` at {:?}",
                    self.0, tok, pos
                )));
                None
            }
        })
    }
}

impl IntoParser<TokenStream> for Token {
    type Target = Token;
    type Parser = TokenParser;

    fn into_parser(self) -> Self::Parser {
        TokenParser(self)
    }
}

#[derive(Copy, Clone, Debug)]
struct OpParser;

impl Parser<TokenStream> for OpParser {
    type Target = Op;

    fn parse(&self, stream: &mut TokenStream, logger: &mut ParseLogger) -> Option<Self::Target> {
        stream.next().and_then(|(pos, tok)| match tok {
            Token::Op(op) => Some(op),
            tok => {
                logger.with(Msg::Err(format!(
                    "expected operator, unexpected `{:?}` at {:?}",
                    tok, pos
                )));
                None
            }
        })
    }
}

#[derive(Copy, Clone, Debug)]
struct IdentParser;

impl Parser<TokenStream> for IdentParser {
    type Target = Ident;

    fn parse(&self, stream: &mut TokenStream, logger: &mut ParseLogger) -> Option<Self::Target> {
        stream.next().and_then(|(pos, tok)| match tok {
            Token::Ident(id) => Some(Ident(id)),
            tok => {
                logger.with(Msg::Err(format!(
                    "expected identifier, unexpected `{:?}` at {:?}",
                    tok, pos
                )));
                None
            }
        })
    }
}

#[derive(Copy, Clone, Debug)]
struct LitParser;

impl Parser<TokenStream> for LitParser {
    type Target = Lit;

    fn parse(&self, stream: &mut TokenStream, logger: &mut ParseLogger) -> Option<Self::Target> {
        stream.next().and_then(|(pos, tok)| match tok {
            Token::Lit(lit) => Some(lit),
            tok => {
                logger.with(Msg::Err(format!(
                    "expected literal, unexpected `{:?}` at {:?}",
                    tok, pos
                )));
                None
            }
        })
    }
}

#[derive(Copy, Clone, Debug)]
struct KeywordParser;

impl Parser<TokenStream> for KeywordParser {
    type Target = Keyword;

    fn parse(&self, stream: &mut TokenStream, logger: &mut ParseLogger) -> Option<Self::Target> {
        match stream.next() {
            Some((_, Token::Keyword(k))) => Some(k),
            Some((pos, tok)) => {
                logger.with(Msg::Err(format!(
                    "error at {:?}, expected keyword, unexpected `{:?}`",
                    pos, tok
                )));
                None
            }
            None => {
                logger.with(Msg::Err("input exhaustive.".to_string()));
                None
            }
        }
    }
}
