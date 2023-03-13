//! Parse source files into untyped, sugared AST

use super::ast;
use super::ir;

/// Nom is *sooo* namespaced that this makes it a little
/// easier to interact with the stuff I need
mod nom_wrapper {
    pub use nom::character::complete::*;
    pub use nom::character::*;

    pub use nom::branch::*;
    pub use nom::combinator::*;
    pub use nom::multi::*;
    pub use nom::sequence::*;

    pub use nom::*;

    pub use nom::{
        bytes::streaming::tag, // This works for str too I guess?
        error::ParseError,
    };
}
use nom_wrapper as nom;
use nom_wrapper::Parser;

/// Taken from <https://github.com/rust-bakery/nom/blob/main/doc/nom_recipes.md>
fn ws<'a, F, O, E: nom::ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> nom::IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> nom::IResult<&'a str, O, E>,
{
    //nom::delimited(nom::multispace0, inner, nom::multispace0)
    nom::preceded(nom::multispace0, inner)
}

fn non_line_ws<'a, F, O, E: nom::ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> nom::IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> nom::IResult<&'a str, O, E>,
{
    //nom::delimited(nom::space0, inner, nom::space0)
    nom::preceded(nom::space0, inner)
}

fn integer(input: &str) -> nom::IResult<&str, i64> {
    nom::i64(input)
}

fn identifier(input: &str) -> nom::IResult<&str, String> {
    let (input, leading) = nom::alt((nom::char('_'), nom::satisfy(char::is_alphabetic)))(input)?;
    let (input, mut ident) = nom::many0(nom::alt((
        nom::char('_'),
        nom::satisfy(char::is_alphanumeric),
    )))
    .map(|v| v.into_iter().collect::<String>())
    .parse(input)?;

    ident.insert(0, leading);
    Ok((input, ident))
}

fn true_(input: &str) -> nom::IResult<&str, ()> {
    nom::tag("true")(input).map(|(i, _)| (i, ()))
}

fn false_(input: &str) -> nom::IResult<&str, ()> {
    nom::tag("false")(input).map(|(i, _)| (i, ()))
}

fn atom(input: &str) -> nom::IResult<&str, ast::Expression> {
    nom::alt((
        nom::map(ws(true_), |_| {
            ast::Expression::Literal(ast::Literal::Bool(true))
        }),
        nom::map(ws(false_), |_| {
            ast::Expression::Literal(ast::Literal::Bool(false))
        }),
        nom::map(ws(integer), |i| {
            ast::Expression::Literal(ast::Literal::Int(i))
        }),
        nom::map(ws(identifier), ast::Expression::Variable),
        nom::delimited(ws(nom::char('(')), expression, ws(nom::char(')'))),
    ))(input)
}

fn arguments(input: &str) -> nom::IResult<&str, Vec<ast::Expression>> {
    nom::separated_list0(ws(nom::char(',')), expression).parse(input)
}

fn function_call(input: &str) -> nom::IResult<&str, ast::Expression> {
    nom::pair(
        ws(identifier),
        nom::delimited(ws(nom::char('(')), arguments, ws(nom::char(')'))),
    )
    .map(|(name, arguments)| {
        ast::Expression::FunctionCall(
            None,
            name,
            arguments.into_iter().map(|expr| (None, expr)).collect(),
        )
    })
    .parse(input)
}

/// TODO(Brooke): Clean this up, it's probably more complicated than it needs to be.
macro_rules! left_assoc_operator {
    ($name:ident, $sep:expr, $sub: expr) => {
        fn $name(input: &str) -> nom::IResult<&str, ast::Expression> {
            let (i, list) = {
                let mut list = Vec::new();
                let (mut i, start) = $sub.parse(input)?;
                list.push((None, start));
                loop {
                    match nom::pair($sep, $sub).map(|(a, b)| (Some(a), b)).parse(i) {
                        Ok((i_, pair)) => {
                            i = i_;
                            list.push(pair);
                        }
                        Err(..) => break,
                    }
                }
                (i, list)
            };
            let mut iter = list.into_iter();
            let mut expr = iter.next().unwrap().1;
            for (op, id) in iter {
                expr = ast::Expression::Operation(
                    op.unwrap(),
                    Box::new((None, expr)),
                    Box::new((None, id)),
                );
            }
            Ok((i, expr))
        }
    };
}

left_assoc_operator! {
    multiplicative_operators,
    nom::alt((
        ws(nom::char('*')).map(|_| ir::Operation::Multiply),
        ws(nom::char('/')).map(|_| ir::Operation::Divide)
    )),
    nom::alt((function_call, atom))
}

left_assoc_operator! {
    additive_operators,
    nom::alt((
        ws(nom::char('+')).map(|_| ir::Operation::Add),
        ws(nom::char('-')).map(|_| ir::Operation::Subtract)
    )),
    multiplicative_operators
}

left_assoc_operator! {
    comparative_operators,
    nom::alt((
        ws(nom::tag("<=")).map(|_| ir::Operation::LessThanOrEqualTo),
        ws(nom::tag(">=")).map(|_| ir::Operation::GreaterThanOrEqualTo),
        ws(nom::char('<')).map(|_| ir::Operation::LessThan),
        ws(nom::char('>')).map(|_| ir::Operation::GreaterThan),
    )),
    additive_operators
}

left_assoc_operator! {
    equality_operators,
    nom::alt((
        ws(nom::tag("==")).map(|_| ir::Operation::Equals),
        ws(nom::tag("!=")).map(|_| ir::Operation::NotEquals),
    )),
    comparative_operators
}

fn expression(input: &str) -> nom::IResult<&str, ast::Expression> {
    equality_operators.parse(input)
}

fn type_annotation(input: &str) -> nom::IResult<&str, ast::Type> {
    nom::alt((
        ws(nom::tag("int")).map(|_| ast::Type::Int),
        ws(nom::tag("bool")).map(|_| ast::Type::Bool),
    ))
    .parse(input)
}

fn var_decl(input: &str) -> nom::IResult<&str, ast::Statement> {
    nom::tuple((
        ws(nom::tag("var")),
        ws(identifier),
        ws(nom::char(':')),
        type_annotation,
        ws(nom::char('=')),
        expression,
    ))
    .map(|(_var, name, _colon, type_, _equals, expression)| {
        ast::Statement::MakeVariable(ast::MakeVariable {
            type_,
            lhs: name,
            rhs: expression,
        })
    })
    .parse(input)
}

fn return_(input: &str) -> nom::IResult<&str, ast::Statement> {
    nom::tuple((ws(nom::tag("return")), expression))
        .map(|(_ret, expression)| ast::Statement::Return((None, expression)))
        .parse(input)
}

fn if_(input: &str) -> nom::IResult<&str, ast::Statement> {
    nom::tuple((ws(nom::tag("if")), expression, body))
        .map(|(_if, condition, body)| {
            ast::Statement::If(ast::If {
                condition: (None, condition),
                body,
            })
        })
        .parse(input)
}

fn print_(input: &str) -> nom::IResult<&str, ast::Statement> {
    nom::preceded(
        ws(nom::tag("print")),
        nom::delimited(ws(nom::char('(')), expression, ws(nom::char(')'))),
    )
    .map(|expression| ast::Statement::Print((None, expression)))
    .parse(input)
}

fn loop_(input: &str) -> nom::IResult<&str, ast::Statement> {
    nom::preceded(ws(nom::tag("loop")), body)
        .map(|body| ast::Statement::Loop(body))
        .parse(input)
}

fn break_(input: &str) -> nom::IResult<&str, ast::Statement> {
    ws(nom::tag("break"))
        .map(|_break| ast::Statement::Break)
        .parse(input)
}

fn var_set(input: &str) -> nom::IResult<&str, ast::Statement> {
    nom::tuple((ws(identifier), ws(nom::char('=')), expression))
        .map(|(ident, _equals, expression)| {
            ast::Statement::SetVariable(ast::SetVariable {
                lhs: ident,
                rhs: (None, expression),
            })
        })
        .parse(input)
}

fn statement(input: &str) -> nom::IResult<&str, ast::Statement> {
    nom::alt((var_decl, return_, if_, loop_, break_, print_, var_set)).parse(input)
}

fn body(input: &str) -> nom::IResult<&str, Vec<ast::Statement>> {
    nom::delimited(
        ws(nom::char('{')),
        nom::separated_list0(non_line_ws(nom::line_ending), statement),
        ws(nom::char('}')),
    )
    .parse(input)
}

fn parameters(input: &str) -> nom::IResult<&str, Vec<(ast::Type, String)>> {
    nom::separated_list0(
        ws(nom::char(',')),
        nom::tuple((ws(identifier), ws(nom::char(':')), ws(type_annotation))),
    )
    .map(|vec| {
        vec.into_iter()
            .map(|(ident, _colon, type_)| (type_, ident))
            .collect()
    })
    .parse(input)
}

pub fn function(input: &str) -> nom::IResult<&str, ast::Function> {
    nom::tuple((
        ws(nom::tag("function")),
        ws(identifier),
        nom::delimited(ws(nom::char('(')), parameters, ws(nom::char(')'))),
        ws(nom::tag("->")),
        ws(type_annotation),
        body,
    ))
    .map(
        |(_function, name, parameters, _arrow, returns, body)| ast::Function {
            name,
            parameters,
            returns,
            body,
        },
    )
    .parse(input)
}

pub fn file(mut input: &str) -> nom::IResult<&str, ast::File> {
    let mut functions = Vec::new();
    loop {
        let (i, function) = match function.parse(input) {
            Ok(res) => res,
            // Kind of a hack!!
            Err(e) => {
                if e.is_incomplete()
                    && ws(nom::eof::<&str, nom::error::Error<&str>>)
                        .parse(input)
                        .is_ok()
                {
                    break;
                } else {
                    return Err(e);
                }
            }
        };
        functions.push(function);
        input = i
    }
    Ok((input, ast::File { functions }))
}

#[test]
fn test_parse() {
    println!();
    println!(
        "{:#?}",
        file(
            "\
function main() -> int {
    return 0
}
function foo(x: int) -> bool {
    return x > 0
}
"
        )
    );
}
