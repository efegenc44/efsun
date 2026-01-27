mod interner;
mod lexer;
mod token;
mod location;
mod parser;
mod expression;

fn main() {
    let source = "f (f x) (\\x \\y x)";
    let mut interner = interner::Interner::new();
    let lexer = lexer::Lexer::new(source, &mut interner);
    let mut parser = parser::Parser::new(lexer);

    let expression = parser.expression().unwrap();
    expression.data().print(&interner, 0);
}