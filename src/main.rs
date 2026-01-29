mod interner;
mod lexer;
mod token;
mod location;
mod parser;
mod expression;
mod resolver;
mod checker;
mod typ;

fn main() {
    let source = r"\x \y \z y (z x)";
    let mut interner = interner::Interner::new();
    let lexer = lexer::Lexer::new(source, &mut interner);
    let mut parser = parser::Parser::new(lexer);
    let mut resolver = resolver::Resolver::new();
    let mut checker = checker::TypeChecker::new();

    let expression = parser.expression().unwrap();
    let expression = resolver.expression(expression).unwrap();
    expression.data().print(&interner, 0);
    let t = checker.infer(&expression);
    println!("{t}");
}