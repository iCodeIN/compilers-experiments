use psc::{ParseLogger, ParseState, Parser};
use simple_pascal::parse::lex::tokenizer;
use simple_pascal::parse::stx::{alg_exp, parser};

static PASCAL: &str = r"
VAR X, Y: INTEGER;
BEGIN
    X := 1;
    WHILE X < 100 DO
    BEGIN
        IF X < 50 THEN
        BEGIN
            X := 2*(X + 1);
        END
        ELSE
            X := 2*X + 1;
        END
END
";

fn main() {
    println!("input:");
    println!("{}", PASCAL);

    let mut logger = ParseLogger::new();
    let mut stream = ParseState::new(PASCAL);
    let mut tok_stream = tokenizer().parse(&mut stream, &mut logger).unwrap();
    println!("token stream: ");
    for tok in tok_stream.clone() {
        println!("{:?}", tok);
    }

    let program = parser().parse(&mut tok_stream, &mut logger);
    println!("abstract syntax tree:");
    println!("{:?}", program);
}
