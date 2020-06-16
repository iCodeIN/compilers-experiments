use psc::{lexeme, pos, pure, reg, wrap, ParseState, Parser, ParserExt, Pos};
use std::vec::IntoIter;

pub type TokenStream = IntoIter<(Pos, Token)>;

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Token {
    Keyword(Keyword),
    Op(Op),
    Lit(Lit),
    Ident(String),
    LPar,
    RPar,
    Semi,
    Colon,
    Comma,
}

static KEYWORDS: [&str; 9] = [
    "VAR", "INTEGER", "BEGIN", "END", "IF", "THEN", "ELSE", "WHILE", "DO",
];

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Keyword {
    /* declare */
    Var,
    Integer,
    /* sub-procedure */
    Begin,
    End,
    /* condition */
    If,
    Then,
    Else,
    /* while-loop */
    While,
    Do,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Op {
    /* assignment */
    Assign, // :=
    /* algorithm */
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    /* relation */
    LT, // <
    LE, // <=
    EQ, // =
    GE, // >=
    GT, // >
    NE, // <>
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum Lit {
    Unsigned(u64),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Ident(pub String);

impl Op {
    pub fn is_rela(&self) -> bool {
        use Op::*;
        match self {
            LT | LE | EQ | GT | GE | NE => true,
            _ => false,
        }
    }
}

pub fn tokenizer<'a>() -> impl Parser<ParseState<'a>, Target = TokenStream> {
    use Token::*;
    let semi = wrap(';') >> pure(|| Semi);
    let lp = wrap('(') >> pure(|| LPar);
    let rp = wrap(')') >> pure(|| RPar);
    let colon = wrap(':') >> pure(|| Colon);
    let comma = wrap(',') >> pure(|| Comma);

    let single = wrap(tok_op().map(Op))
        | tok_lit().map(Lit)
        | tok_ident().map(|id| Ident(id.0))
        | tok_keyword().map(Keyword)
        | semi
        | lp
        | rp
        | colon
        | comma;

    let single = pos().map2(single, |pos, tok| (pos, tok));

    lexeme(single).many().map(Vec::into_iter)
}

pub fn tok_keyword<'a>() -> impl Parser<ParseState<'a>, Target = Keyword> {
    use Keyword::*;

    //          token               |            semantic
    /* declare */
    (wrap("VAR")                >> pure(|| Var))
        | (wrap("INTEGER")      >> pure(|| Integer))
        /* sub-procedure */
        | (wrap("BEGIN")        >> pure(|| Begin))
        | (wrap("END")          >> pure(|| End))
        /* sub-procedure */
        | (wrap("IF")           >> pure(|| If))
        | (wrap("THEN")         >> pure(|| Then))
        | (wrap("ELSE")         >> pure(|| Else))
        /* while-loop */
        | (wrap("WHILE")        >> pure(|| While))
        | (wrap("DO")           >> pure(|| Do))
}

pub fn tok_op<'a>() -> impl Parser<ParseState<'a>, Target = Op> {
    use Op::*;
    //          token               |            semantic
    /* assignment */
    (wrap(":=")                 >> pure(|| Assign))
        /* algorithm */
        | (wrap("+")            >> pure(|| Add))
        | (wrap("-")            >> pure(|| Sub))
        | (wrap("*")            >> pure(|| Mul))
        | (wrap("/")            >> pure(|| Div))
        /* relation */
        | (wrap("<")            >> pure(|| LT))
        | (wrap("<=")           >> pure(|| LE))
        | (wrap("=")            >> pure(|| EQ))
        | (wrap(">")            >> pure(|| GT))
        | (wrap(">=")           >> pure(|| GE))
        | (wrap("<>")           >> pure(|| NE))
}

pub fn tok_lit<'a>() -> impl Parser<ParseState<'a>, Target = Lit> {
    reg("\\d+")
        .map(str::parse::<u64>)
        .map(Result::unwrap)
        .map(Lit::Unsigned)
}

pub fn tok_ident<'a>() -> impl Parser<ParseState<'a>, Target = Ident> {
    reg("[A-Z][0-9A-Z]*")
        .filter(|s| !KEYWORDS.contains(s))
        .map(str::to_owned)
        .map(Ident)
}
