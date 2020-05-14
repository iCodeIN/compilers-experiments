use psc::{ParseLogger, ParseState, Parser};
use simple_pascal::parse::lex::tokenizer;

static PASCAL: &str = r"
VAR X: INTEGER;
BEGIN
    X = 0;
    WHILE X < 100 DO
    BEGIN
        IF X < 50 THEN
        BEGIN
            X = 2*(X + 1);
        END
        ELSE
            X = 2*X + 1;
    END
END
";

fn main() {
    let mut logger = ParseLogger::new();
    let mut stream = ParseState::new(PASCAL);
    let tok_stream = tokenizer().parse(&mut stream, &mut logger).unwrap();
    for tok in tok_stream {
        println!("{:?}", tok);
    }
}
