use crate::{
    expression::{
        ApplicationExpression, Expression, IdentifierExpression,
        Unresolved
    },
    interner::{InternId, Interner},
    location::{Located, SourceLocation},
    resolver::Capture
};

#[derive(Clone)]
pub enum ANF {
    Let(InternId, Atom, Box<ANF>),
    LetApp(InternId, Atom, Atom, Box<ANF>),
    Atom(Atom)
}

#[derive(Clone)]
pub enum Atom {
    String(InternId),
    Identifier(IdentifierExpression<Unresolved>),
    Lambda(InternId, Box<ANF>)
}

fn go(e: Expression<Unresolved>, k: Box<dyn FnOnce(Atom) -> ANF + '_>) -> ANF {
    match e {
        Expression::String(id) => k(Atom::String(id)),
        Expression::Identifier(identifier) => k(Atom::Identifier(identifier)),
        Expression::Application(application) => {
            let (function, argument) = application.destruct();

            go(argument.destruct().0, Box::new(|argument| {
                go(function.data().clone(), Box::new(|function| {
                    let t = InternId::dummy();
                    ANF::LetApp(t, function, argument.clone(),
                        Box::new(k(Atom::Identifier(IdentifierExpression::new(Located::new(t, SourceLocation::eof(), SourceLocation::eof()))))))
                }))
            }))
        },
        Expression::Lambda(lambda) => {
            let (variable, expression) = lambda.destruct();

            let anf = Box::new(anf(expression.destruct().0));
            k(Atom::Lambda(*variable.data(), anf))
        },
        Expression::Let(letin) => {
            let (variable, variable_expression, return_expression) = letin.destruct();

            go(variable_expression.destruct().0, Box::new(|vexpr| {
                ANF::Let(*variable.data(), vexpr, Box::new(go(return_expression.data().clone(), k)))
            }))
        },
    }
}

pub fn anf(e: Expression<Unresolved>) -> ANF {
    go(e, Box::new(ANF::Atom))
}

pub fn print_anf(anf: &ANF, depth: usize, interner: &Interner) {
    let indent = 2*depth;

    match anf {
        ANF::Let(id, atom, anf) => {
            println!("{:indent$}let {}", "", interner.lookup(*id));
            print_atom(atom, depth + 1, interner);
            println!("{:indent$}in", "");
            print_anf(anf, depth + 1, interner);
        },
        ANF::LetApp(id, func, argm, expr) => {
            println!("{:indent$}letapp {id}", "");
            print_atom(func, depth + 1, interner);
            print_atom(argm, depth + 1, interner);
            println!("{:indent$}in", "");
            print_anf(expr, depth + 1, interner);
        },
        ANF::Atom(atom) => print_atom(atom, depth, interner),
    }
}

fn print_atom(atom: &Atom, depth: usize, interner: &Interner) {
    let indent = 2*depth;

    match atom {
        Atom::String(id) => println!("{:indent$}\"{}\"", "", interner.lookup(*id)),
        Atom::Identifier(identifier) => println!("{:indent$}{}", "", interner.lookup(*identifier.identifier().data())),
        Atom::Lambda(id, anf) => {
            println!("{:indent$}\\{}", "", interner.lookup(*id));
            print_anf(anf, depth + 1, interner);
        },
    }
}