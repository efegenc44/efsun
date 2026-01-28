mod interner;
mod lexer;
mod token;
mod location;
mod parser;
mod expression;
mod resolver;

fn main() {
    let source = r"\x \y x (\x y x)";
    let mut interner = interner::Interner::new();
    let lexer = lexer::Lexer::new(source, &mut interner);
    let mut parser = parser::Parser::new(lexer);
    let mut resolver = resolver::Resolver::new();

    let mut expression = parser.expression().unwrap();
    resolver.expression(&mut expression).unwrap();
    expression.data().print(&interner, 0);
}