//! Parse source files into untyped, sugared AST

use super::ast;
use super::internal_error::*;
use super::ir;

use std::cell::RefCell;
use std::rc::Rc;

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

pub type Location<'a> = nom_locate::LocatedSpan<&'a str, Rc<String>>;

#[derive(Debug, Clone)]
pub struct SpanInfo {
    pub eof: bool,
    pub line: Option<usize>,
    pub column: Option<usize>,
    pub filename: Rc<String>,
}

#[derive(Debug, Clone)]
pub struct Span<T> {
    pub inner: T,
    pub info: Option<SpanInfo>,
}

impl SpanInfo {
    fn eof(filename: Rc<String>) -> SpanInfo {
        SpanInfo {
            eof: true,
            line: None,
            column: None,
            filename: filename,
        }
    }
}

impl SpanInfo {
    pub fn from_err(
        filename: Rc<String>,
        err: nom::Err<nom::error::Error<Location<'_>>>,
    ) -> SpanInfo {
        let err = match err {
            nom::Err::Incomplete(..) => {
                return SpanInfo::eof(filename);
            }
            nom::Err::Error(e) => e,
            nom::Err::Failure(e) => e,
        };
        let (input, position) =
            nom_locate::position::<Location, nom::error::Error<Location>>(err.input).unwrap();
        SpanInfo {
            eof: false,
            line: Some(position.location_line() as usize),
            column: Some(position.get_utf8_column()),
            filename: position.extra.clone(),
        }
    }
}

impl<T> Span<T> {
    pub fn new(inner: T, info: Option<SpanInfo>) -> Span<T> {
        Span { inner, info }
    }
    pub fn generated(inner: T) -> Span<T> {
        Span { inner, info: None }
    }
    pub fn get(self) -> T {
        self.inner
    }
    pub fn nil(&self) -> Span<()> {
        Span {
            inner: (),
            info: self.info.clone(),
        }
    }
}

impl<T> std::ops::Deref for Span<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.inner
    }
}

impl<T> std::ops::DerefMut for Span<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.inner
    }
}

/// Taken from <https://github.com/rust-bakery/nom/blob/main/doc/nom_recipes.md>
fn ws<'a, F, O, E: nom::ParseError<Location<'a>>>(
    inner: F,
) -> impl FnMut(Location<'a>) -> nom::IResult<Location<'a>, O, E>
where
    F: FnMut(Location<'a>) -> nom::IResult<Location<'a>, O, E>,
{
    //nom::delimited(nom::multispace0, inner, nom::multispace0)
    nom::preceded(nom::multispace0, inner)
}

fn non_line_ws<'a, F, O, E: nom::ParseError<Location<'a>>>(
    inner: F,
) -> impl FnMut(Location<'a>) -> nom::IResult<Location<'a>, O, E>
where
    F: FnMut(Location<'a>) -> nom::IResult<Location<'a>, O, E>,
{
    //nom::delimited(nom::space0, inner, nom::space0)
    nom::preceded(nom::space0, inner)
}

fn integer(input: Location) -> nom::IResult<Location, i64> {
    nom::i64(input)
}

fn identifier(input: Location) -> nom::IResult<Location, String> {
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

fn true_(input: Location) -> nom::IResult<Location, ()> {
    nom::tag("true")(input).map(|(i, _)| (i, ()))
}

fn false_(input: Location) -> nom::IResult<Location, ()> {
    nom::tag("false")(input).map(|(i, _)| (i, ()))
}

fn atom(input: Location) -> nom::IResult<Location, ast::Expression> {
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

fn arguments(input: Location) -> nom::IResult<Location, Vec<ast::Expression>> {
    nom::separated_list0(ws(nom::char(',')), expression).parse(input)
}

fn alloc_(input: Location) -> nom::IResult<Location, ast::Expression> {
    nom::preceded(
        ws(nom::tag("alloc")),
        nom::delimited(ws(nom::char('(')), expression, ws(nom::char(')'))),
    )
    .map(|expression| ast::Expression::Allocate(Box::new((None, expression))))
    .parse(input)
}

fn function_call(input: Location) -> nom::IResult<Location, ast::Expression> {
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
        fn $name(input: Location) -> nom::IResult<Location, ast::Expression> {
            let (i, list) = {
                let mut list = Vec::new();
                let (mut i, start) = $sub.parse(input)?;
                list.push((None, start));
                loop {
                    match nom::pair($sep, $sub)
                        .map(|(a, b)| (Some(a), b))
                        .parse(i.clone())
                    {
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

fn dereference(input: Location) -> nom::IResult<Location, ast::Expression> {
    nom::pair(
        nom::many0_count(ws(nom::tag("at"))),
        nom::alt((alloc_, function_call, atom)),
    )
    .map(|(count, expression)| {
        if count == 0 {
            expression
        } else {
            ast::Expression::Dereference(count, Rc::new(RefCell::new(expression)))
        }
    })
    .parse(input)
}

left_assoc_operator! {
    multiplicative_operators,
    nom::alt((
        ws(nom::char('*')).map(|_| ir::Operation::Multiply),
        ws(nom::char('/')).map(|_| ir::Operation::Divide)
    )),
    dereference
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

fn expression(input: Location) -> nom::IResult<Location, ast::Expression> {
    equality_operators.parse(input)
}

fn function_type(input: Location) -> nom::IResult<Location, ast::Type> {
    nom::tuple((
        ws(nom::tag("func")),
        nom::delimited(
            ws(nom::char('(')),
            nom::separated_list0(ws(nom::char(',')), type_annotation),
            ws(nom::char(')')),
        ),
        nom::opt(nom::preceded(ws(nom::tag("->")), type_annotation)),
    ))
    .map(|(_func, args, returns)| {
        ast::Type::Function(Rc::new((returns.unwrap_or(ast::Type::Unit), args)))
    })
    .parse(input)
}

fn type_annotation(input: Location) -> nom::IResult<Location, ast::Type> {
    nom::pair(
        nom::many0_count(ws(nom::char('*'))),
        nom::alt((
            ws(nom::tag("int")).map(|_| ast::Type::Int),
            ws(nom::tag("bool")).map(|_| ast::Type::Bool),
            function_type,
        )),
    )
    .map(|(ptr_count, inner_type)| {
        let mut type_ = inner_type;
        for _ in 0..ptr_count {
            type_ = ast::Type::Pointer(Box::new(type_))
        }
        type_
    })
    .parse(input)
}

fn var_decl(input: Location) -> nom::IResult<Location, ast::Statement> {
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

fn return_(input: Location) -> nom::IResult<Location, ast::Statement> {
    nom::tuple((ws(nom::tag("return")), nom::opt(expression)))
        .map(|(_ret, expression)| {
            ast::Statement::Return((None, expression.unwrap_or(ast::Expression::Unit)))
        })
        .parse(input)
}

fn if_(input: Location) -> nom::IResult<Location, ast::Statement> {
    nom::tuple((ws(nom::tag("if")), expression, body))
        .map(|(_if, condition, body)| {
            ast::Statement::If(ast::If {
                condition: (None, condition),
                body,
            })
        })
        .parse(input)
}

fn print_(input: Location) -> nom::IResult<Location, ast::Statement> {
    nom::preceded(
        ws(nom::tag("print")),
        nom::delimited(ws(nom::char('(')), expression, ws(nom::char(')'))),
    )
    .map(|expression| ast::Statement::Print((None, expression)))
    .parse(input)
}

fn loop_(input: Location) -> nom::IResult<Location, ast::Statement> {
    nom::preceded(ws(nom::tag("loop")), body)
        .map(|body| ast::Statement::Loop(body))
        .parse(input)
}

fn break_(input: Location) -> nom::IResult<Location, ast::Statement> {
    ws(nom::tag("break"))
        .map(|_break| ast::Statement::Break)
        .parse(input)
}

fn lvalue(input: Location) -> nom::IResult<Location, ast::LValue> {
    nom::pair(nom::many0_count(ws(nom::tag("at"))), ws(identifier))
        .map(|(count, interior)| ast::LValue {
            name: interior,
            derefs: count,
        })
        .parse(input)
}

fn var_set(input: Location) -> nom::IResult<Location, ast::Statement> {
    nom::tuple((lvalue, ws(nom::char('=')), expression))
        .map(|(lhs, _equals, expression)| {
            ast::Statement::SetVariable(ast::SetVariable {
                lhs,
                rhs: (None, expression),
            })
        })
        .parse(input)
}

fn bare_expression(input: Location) -> nom::IResult<Location, ast::Statement> {
    expression
        .map(|expr| ast::Statement::BareExpression((None, expr)))
        .parse(input)
}

fn statement(input: Location) -> nom::IResult<Location, Span<ast::Statement>> {
    let (input, _) = nom::multispace0::<Location, nom::error::Error<Location>>(input).unwrap();
    let (input, start_location) =
        nom_locate::position::<Location, nom::error::Error<Location>>(input).unwrap();
    let span_info = SpanInfo {
        eof: false,
        line: Some(start_location.location_line() as usize),
        column: Some(start_location.get_utf8_column()),
        filename: start_location.extra.clone(),
    };
    let (input, stmt) = nom::alt((
        var_decl,
        return_,
        if_,
        loop_,
        break_,
        print_,
        var_set,
        bare_expression,
    ))
    .parse(input)?;
    Ok((input, Span::new(stmt, Some(span_info))))
}

fn body(input: Location) -> nom::IResult<Location, Vec<Span<ast::Statement>>> {
    nom::delimited(
        ws(nom::char('{')),
        nom::separated_list0(non_line_ws(nom::line_ending), statement),
        ws(nom::char('}')),
    )
    .parse(input)
}

fn parameters(input: Location) -> nom::IResult<Location, Vec<(ast::Type, String)>> {
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

pub fn function(input: Location) -> nom::IResult<Location, ast::Function> {
    nom::tuple((
        ws(nom::tag("function")),
        ws(identifier),
        nom::delimited(ws(nom::char('(')), parameters, ws(nom::char(')'))),
        nom::opt(nom::pair(ws(nom::tag("->")), ws(type_annotation)).map(|(_arrow, type_)| type_)),
        body,
    ))
    .map(
        |(_function, name, parameters, returns, body)| ast::Function {
            name,
            parameters,
            returns: returns.unwrap_or(ast::Type::Unit),
            body,
        },
    )
    .parse(input)
}

pub fn file(mut input: Location) -> nom::IResult<Location, ast::File> {
    let mut functions = Vec::new();
    loop {
        let (i, function) = match function.parse(input.clone()) {
            Ok(res) => res,
            // Kind of a hack!!
            Err(e) => {
                if e.is_incomplete()
                    && ws(nom::eof::<Location, nom::error::Error<Location>>)
                        .parse(input.clone())
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
        body(Location::new_extra(
            "{
    var ptr: int = 5
    *ptr = 5
    print(*ptr)

    return 0
}",
            Rc::new(String::from("<main>"))
        ))
    );
}
