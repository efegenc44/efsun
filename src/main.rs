mod interner;
mod lexer;
mod token;
mod location;

fn main() {
    let source = "f (f x)";
    let mut interner = interner::Interner::new();
    let lexer = lexer::Lexer::new(source, &mut interner);

    for token in lexer {
        println!("{}", token.unwrap());
    }
}