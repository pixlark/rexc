use std::{cell::RefCell, collections::VecDeque, rc::Rc};

use super::ast::*;
use super::ir::Operation;
use super::parse;

#[derive(Debug, Clone)]
pub enum Span {
    Eof {
        filename: Rc<String>,
    },
    Char {
        filename: Rc<String>,
        pos: usize,
    },
    Range {
        filename: Rc<String>,
        start: usize,
        end: usize,
    },
}

#[derive(Debug, Clone)]
enum TokenKind {
    Eof,
    Newline,
    // Literals
    IntegerLiteral(i64),
    // Identifiers
    Identifier(String),
    // Keywords
    Alloc,
    At,
    Bool,
    Break,
    False,
    Func,
    Function,
    If,
    Int,
    Loop,
    Print,
    Return,
    True,
    Var,
    // Symbols
    Comma,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Colon,
    RightArrow,
    AssignEquals,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanOrEqualTo,
    GreaterThanOrEqualTo,
    Plus,
    Minus,
    Asterisk,
    ForwardSlash,
}

#[derive(Debug, Clone)]
struct Token {
    kind: TokenKind,
    span: Span,
}

#[derive(Debug)]
enum ParseErrorKind {
    UnrecognizedCharacter(char),
    IntegerLiteralTooBig,
    ParserExpected {
        expected: Vec<&'static str>,
        got: Option<TokenKind>,
    },
}

#[derive(Debug)]
struct ParseError {
    kind: ParseErrorKind,
    span: Span,
}

#[derive(Debug)]
struct Lexer {
    source: Rc<String>,
    chars: Vec<char>,
    cursor: usize,
    u8_cursor: usize,
    filename: Rc<String>,

    buffer: VecDeque<Option<Token>>,
}

#[derive(Debug)]
struct Parser {
    lexer: Lexer,
}

macro_rules! match_single_chars {
    ( $c:expr, $span:expr, $( $ch:expr, $kind:expr ),* ) => {
        match $c {
            $(
                $ch => return Ok(Some(Token {
                    kind: $kind,
                    span: $span,
                })),
            )*
            _ => {}
        }
    }
}

impl Lexer {
    fn new(source: Rc<String>, filename: Rc<String>) -> Result<Lexer, ParseError> {
        let source = source.clone();
        let chars = source.chars().collect::<Vec<char>>();

        let mut lexer = Lexer {
            source: source.clone(),
            chars,
            cursor: 0,
            u8_cursor: 0,
            filename: filename.clone(),

            buffer: VecDeque::new(),
        };

        // Fill first token
        let tok = lexer.next_token()?;
        lexer.buffer.push_back(tok);
        Ok(lexer)
    }
    fn consume(&mut self) -> Result<Option<Token>, ParseError> {
        let tok = self.buffer.pop_front().unwrap();
        if self.buffer.is_empty() {
            let next_tok = self.next_token()?;
            self.buffer.push_back(next_tok);
        }
        Ok(tok)
    }
    fn current(&mut self) -> &Option<Token> {
        self.buffer.front().unwrap()
    }
    fn lookahead(&mut self, n: usize) -> Result<&Option<Token>, ParseError> {
        let mut current_lookahead = self.buffer.len();

        while n + 1 > current_lookahead {
            let next_tok = self.next_token()?;
            self.buffer.push_back(next_tok);
            current_lookahead += 1;
        }

        Ok(self.buffer.iter().nth(n).unwrap())
    }
    fn next_char(&mut self) -> Option<char> {
        self.advance_char();
        self.current_char()
    }
    fn advance_char(&mut self) {
        let c = self.current_char();
        self.cursor += if c.is_some() { 1 } else { 0 };
        self.u8_cursor += c.map(|c| c.len_utf8()).unwrap_or(0);
    }
    fn retreat_char(&mut self) {
        if self.cursor >= 1 {
            self.cursor -= 1;
            let c = self.current_char();
            self.u8_cursor -= c.map(|c| c.len_utf8()).unwrap();
        }
    }
    fn current_char(&self) -> Option<char> {
        self.chars.get(self.cursor).map(|c| *c)
    }
    fn span_range(&self, start: usize, end: usize) -> Span {
        Span::Range {
            filename: self.source.clone(),
            start,
            end,
        }
    }
    fn span_char(&self, pos: usize) -> Span {
        Span::Char {
            filename: self.source.clone(),
            pos,
        }
    }
    fn span_eof(&self) -> Span {
        Span::Eof {
            filename: self.source.clone(),
        }
    }
    fn next_token(&mut self) -> Result<Option<Token>, ParseError> {
        let mut c = match self.current_char() {
            Some(c) => c,
            None => return Ok(None),
        };

        // Skip whitespace
        while c != '\n' && c.is_ascii_whitespace() {
            c = match self.next_char() {
                Some(c) => c,
                None => return Ok(None),
            }
        }

        // Integer
        let mut integer_sign: Option<char> = None;
        if c == '-' || c == '+' {
            match self.next_char() {
                Some(ch) if ch.is_ascii_digit() => {
                    integer_sign = Some(c);
                    c = ch;
                }
                _ => {
                    self.retreat_char();
                }
            }
        }

        if integer_sign.is_some() || c.is_ascii_digit() {
            let mut buf = Vec::new();

            if let Some(sign) = integer_sign {
                buf.push(sign);
            }

            let tok_start = self.u8_cursor;
            while c.is_ascii_digit() {
                buf.push(c);
                c = match self.next_char() {
                    Some(c) => c,
                    None => break,
                }
            }
            let tok_end = self.u8_cursor;
            let span = self.span_range(tok_start, tok_end);

            let s = buf.into_iter().collect::<String>();
            if let Ok(i) = s.parse::<i64>() {
                return Ok(Some(Token {
                    kind: TokenKind::IntegerLiteral(i),
                    span,
                }));
            } else {
                return Err(ParseError {
                    kind: ParseErrorKind::IntegerLiteralTooBig,
                    span,
                });
            }
        }

        // Keyword/Identifier
        if c.is_alphabetic() || c == '_' {
            let mut buf = Vec::new();

            let tok_start = self.u8_cursor;
            while c.is_alphanumeric() || c == '_' {
                buf.push(c);
                c = match self.next_char() {
                    Some(c) => c,
                    None => break,
                }
            }
            let tok_end = self.u8_cursor;
            let span = self.span_range(tok_start, tok_end);

            let ident = buf.into_iter().collect::<String>();
            let kw = match ident.as_str() {
                "alloc" => Some(TokenKind::Alloc),
                "at" => Some(TokenKind::At),
                "bool" => Some(TokenKind::Bool),
                "break" => Some(TokenKind::Break),
                "false" => Some(TokenKind::False),
                "func" => Some(TokenKind::Func),
                "function" => Some(TokenKind::Function),
                "if" => Some(TokenKind::If),
                "int" => Some(TokenKind::Int),
                "loop" => Some(TokenKind::Loop),
                "print" => Some(TokenKind::Print),
                "return" => Some(TokenKind::Return),
                "true" => Some(TokenKind::True),
                "var" => Some(TokenKind::Var),
                _ => None,
            };
            return Ok(Some(if let Some(kw) = kw {
                Token { kind: kw, span }
            } else {
                Token {
                    kind: TokenKind::Identifier(ident),
                    span,
                }
            }));
        }

        // Symbols
        let symbol_span = self.span_char(self.u8_cursor);
        self.advance_char();

        match_single_chars! {
            c, symbol_span,
            '\n', TokenKind::Newline,
            ',', TokenKind::Comma,
            '(', TokenKind::OpenParen,
            ')', TokenKind::CloseParen,
            '{', TokenKind::OpenBrace,
            '}', TokenKind::CloseBrace,
            ':', TokenKind::Colon,
            '+', TokenKind::Plus,
            '*', TokenKind::Asterisk,
            '/', TokenKind::ForwardSlash
        }

        match c {
            '-' => {
                let next_c = self.current_char();
                return Ok(Some(Token {
                    kind: match next_c {
                        Some('>') => {
                            self.advance_char();
                            TokenKind::RightArrow
                        }
                        None | Some(_) => TokenKind::Minus,
                    },
                    span: symbol_span,
                }));
            }
            '=' => {
                let next_c = self.current_char();
                return Ok(Some(Token {
                    kind: match next_c {
                        Some('=') => {
                            self.advance_char();
                            TokenKind::Equals
                        }
                        None | Some(_) => TokenKind::AssignEquals,
                    },
                    span: symbol_span,
                }));
            }
            '!' => {
                let next_c = self.current_char();
                if let Some('=') = next_c {
                    self.advance_char();
                    return Ok(Some(Token {
                        kind: TokenKind::NotEquals,
                        span: symbol_span,
                    }));
                }
            }
            '<' => {
                let next_c = self.current_char();
                return Ok(Some(Token {
                    kind: match next_c {
                        Some('=') => {
                            self.advance_char();
                            TokenKind::LessThanOrEqualTo
                        }
                        None | Some(_) => TokenKind::LessThan,
                    },
                    span: symbol_span,
                }));
            }
            '>' => {
                let next_c = self.current_char();
                return Ok(Some(Token {
                    kind: match next_c {
                        Some('=') => {
                            self.advance_char();
                            TokenKind::GreaterThanOrEqualTo
                        }
                        None | Some(_) => TokenKind::GreaterThan,
                    },
                    span: symbol_span,
                }));
            }
            _ => {}
        }

        let unrecognized_span = self.span_char(self.u8_cursor);
        Err(ParseError {
            kind: ParseErrorKind::UnrecognizedCharacter(c),
            span: unrecognized_span,
        })
    }
}

macro_rules! left_assoc_operator {
    (fn $name:ident, $subparser:ident, $operator_list:expr) => {
        fn $name(&mut self) -> Result<Expression, ParseError> {
            let operator_list = $operator_list;
            let mut lhs = self.$subparser()?;
            loop {
                if let Some(tok) = self.lexer.current() {
                    let op = operator_list.iter().find(|(k, _)| {
                        std::mem::discriminant(k) == std::mem::discriminant(&tok.kind)
                    });
                    if let Some(op) = op {
                        self.lexer.consume()?;
                        let rhs = self.$subparser()?;
                        lhs = Expression::Operation(
                            op.1,
                            Box::new((None, lhs)),
                            Box::new((None, rhs)),
                        );
                        continue;
                    }
                }
                break;
            }
            Ok(lhs)
        }
    };
}

macro_rules! separated {
    ($self:expr, $subparser:ident, by $separator:path, terminated_by $terminator:path) => {{
        let mut vec = Vec::new();
        loop {
            let current = $self.lexer.current();
            if let Some(Token {
                kind: $terminator, ..
            }) = current
            {
                break;
            }

            vec.push($self.$subparser()?);

            let current = $self.lexer.current();
            if let Some(Token {
                kind: $separator, ..
            }) = current
            {
                $self.lexer.consume()?;
            } else {
                break;
            }
        }
        vec
    }};
}

macro_rules! expect {
    ($self:expr, expect $pat:pat, take $closure:expr, error $expect_list:expr) => {{
        let tok = $self.lexer.consume()?;
        if let Some(Token { kind: $pat, .. }) = tok {
            $closure()
        } else {
            return $self.error_expected($expect_list, tok);
        }
    }};
}

impl Parser {
    fn new(lexer: Lexer) -> Parser {
        Parser { lexer }
    }
    fn error_expected<T>(
        &self,
        expected: Vec<&'static str>,
        got: Option<Token>,
    ) -> Result<T, ParseError> {
        Err(ParseError {
            kind: ParseErrorKind::ParserExpected {
                expected,
                got: got.clone().map(|t| t.kind),
            },
            span: got.clone().map(|t| t.span).unwrap_or(self.lexer.span_eof()),
        })
    }
    fn atom(&mut self) -> Result<Expression, ParseError> {
        let tok = self.lexer.consume()?;
        match tok {
            Some(Token {
                kind: TokenKind::IntegerLiteral(i),
                span: _span,
            }) => Ok(Expression::Literal(Literal::Int(i))),
            Some(Token {
                kind: TokenKind::True,
                span: _span,
            }) => Ok(Expression::Literal(Literal::Bool(true))),
            Some(Token {
                kind: TokenKind::False,
                span: _span,
            }) => Ok(Expression::Literal(Literal::Bool(false))),
            Some(Token {
                kind: TokenKind::Identifier(name),
                span: _span,
            }) => Ok(Expression::Variable(name)),
            Some(Token {
                kind: TokenKind::OpenParen,
                span: _span,
            }) => {
                let inner = self.expression()?;
                let tok = self.lexer.consume()?;
                if matches!(
                    tok,
                    Some(Token {
                        kind: TokenKind::CloseParen,
                        ..
                    })
                ) {
                    Ok(inner)
                } else {
                    self.error_expected(vec!["')'"], tok)
                }
            }
            _ => self.error_expected(vec!["literal", "variable", "'('"], tok),
        }
    }
    fn function_call(&mut self) -> Result<Expression, ParseError> {
        let current = self.lexer.current();

        match current {
            Some(Token {
                kind: TokenKind::Identifier(..),
                ..
            }) => {
                let peek = self.lexer.lookahead(1)?;
                if let Some(Token {
                    kind: TokenKind::OpenParen,
                    ..
                }) = peek
                {
                    let ident_tok = self.lexer.consume().unwrap();
                    self.lexer.consume()?;

                    let name = match ident_tok {
                        Some(Token {
                            kind: TokenKind::Identifier(name),
                            ..
                        }) => name,
                        _ => unreachable!(),
                    };

                    let interior = separated!(self, expression, by TokenKind::Comma, terminated_by TokenKind::CloseParen);
                    let end = self.lexer.consume()?;
                    if !matches!(
                        end,
                        Some(Token {
                            kind: TokenKind::CloseParen,
                            ..
                        })
                    ) {
                        return self.error_expected(vec!["')'"], end);
                    }
                    return Ok(Expression::FunctionCall(
                        None,
                        name,
                        interior.into_iter().map(|e| (None, e)).collect(),
                    ));
                }
            }
            Some(Token {
                kind: TokenKind::Alloc,
                ..
            }) => {
                let peek = self.lexer.lookahead(1)?;
                if let Some(Token {
                    kind: TokenKind::OpenParen,
                    ..
                }) = peek
                {
                    self.lexer.consume().unwrap();
                    self.lexer.consume().unwrap();

                    let interior = self.expression()?;
                    let end = self.lexer.consume()?;
                    if !matches!(
                        end,
                        Some(Token {
                            kind: TokenKind::CloseParen,
                            ..
                        })
                    ) {
                        return self.error_expected(vec!["')'"], end);
                    }
                    return Ok(Expression::Allocate(Box::new((None, interior))));
                }
            }
            _ => {}
        }
        self.atom()
    }
    fn dereference(&mut self) -> Result<Expression, ParseError> {
        let mut deref_count = 0;
        while matches!(
            self.lexer.current(),
            Some(Token {
                kind: TokenKind::At,
                ..
            })
        ) {
            deref_count += 1;
            self.lexer.consume()?;
        }
        let mut interior = self.function_call()?;
        if deref_count > 0 {
            interior = Expression::Dereference(deref_count, Rc::new(RefCell::new(interior)));
        }
        Ok(interior)
    }
    left_assoc_operator! {
        fn multiplicative_operators,
        dereference,
        vec![
            (TokenKind::Asterisk, Operation::Multiply),
            (TokenKind::ForwardSlash, Operation::Divide),
        ]
    }
    left_assoc_operator! {
        fn additive_operators,
        multiplicative_operators,
        vec![
            (TokenKind::Plus, Operation::Add),
            (TokenKind::Minus, Operation::Subtract)
        ]
    }
    left_assoc_operator! {
        fn comparative_operators,
        additive_operators,
        vec![
            (TokenKind::LessThan, Operation::LessThan),
            (TokenKind::GreaterThan, Operation::GreaterThan),
            (TokenKind::LessThanOrEqualTo, Operation::LessThanOrEqualTo),
            (TokenKind::GreaterThanOrEqualTo, Operation::GreaterThanOrEqualTo)
        ]
    }
    left_assoc_operator! {
        fn equality_operators,
        comparative_operators,
        vec![
            (TokenKind::Equals, Operation::Equals),
            (TokenKind::NotEquals, Operation::NotEquals),
        ]
    }
    fn expression(&mut self) -> Result<Expression, ParseError> {
        self.equality_operators()
    }
    fn type_annotation(&mut self) -> Result<Type, ParseError> {
        let tok = self.lexer.consume()?;
        match tok {
            Some(Token {
                kind: TokenKind::Asterisk,
                ..
            }) => Ok(Type::Pointer(Box::new(self.type_annotation()?))),
            Some(Token {
                kind: TokenKind::Int,
                ..
            }) => Ok(Type::Int),
            Some(Token {
                kind: TokenKind::Bool,
                ..
            }) => Ok(Type::Bool),
            Some(Token {
                kind: TokenKind::Func,
                ..
            }) => unimplemented!(),
            _ => self.error_expected(vec!["int", "bool", "func"], tok),
        }
    }
    fn make_variable(&mut self) -> Result<Statement, ParseError> {
        self.lexer.consume().unwrap(); // `var`

        let name = expect!(
            self,
            expect TokenKind::Identifier(name),
            take move || name,
            error vec!["ident"]
        );

        expect!(
            self,
            expect TokenKind::Colon,
            take move || (),
            error vec!["':'"]
        );

        let type_ = self.type_annotation()?;

        expect!(
            self,
            expect TokenKind::AssignEquals,
            take move || (),
            error vec!["'='"]
        );

        let expr = self.expression()?;

        Ok(Statement::MakeVariable(MakeVariable {
            type_,
            lhs: name,
            rhs: expr,
        }))
    }
    fn return_(&mut self) -> Result<Statement, ParseError> {
        self.lexer.consume().unwrap();

        if let Some(Token {
            kind: TokenKind::Newline,
            ..
        }) = self.lexer.current()
        {
            return Ok(Statement::Return((None, Expression::Unit)));
        }

        let inner = self.expression()?;

        Ok(Statement::Return((None, inner)))
    }
    fn print_(&mut self) -> Result<Statement, ParseError> {
        self.lexer.consume().unwrap();

        expect!(
            self,
            expect TokenKind::OpenParen,
            take move || (),
            error vec!["'('"]
        );

        let inner = self.expression()?;

        let end = self.lexer.consume()?;
        match end {
            Some(Token {
                kind: TokenKind::CloseParen,
                ..
            }) => Ok(Statement::Print((None, inner))),
            _ => self.error_expected(vec!["')'"], end),
        }
    }
    fn if_(&mut self) -> Result<Statement, ParseError> {
        self.lexer.consume().unwrap();

        let condition = self.expression()?;

        let body = self.body()?;

        Ok(Statement::If(If {
            condition: (None, condition),
            body: body
                .into_iter()
                .map(|s| parse::Span::new(s, None))
                .collect(),
        }))
    }
    fn loop_(&mut self) -> Result<Statement, ParseError> {
        self.lexer.consume().unwrap();

        let body = self.body()?;

        Ok(Statement::Loop(
            body.into_iter()
                .map(|s| parse::Span::new(s, None))
                .collect(),
        ))
    }
    fn lvalue(&mut self) -> Result<LValue, ParseError> {
        unimplemented!()
    }
    fn assignment(&mut self) -> Result<Statement, ParseError> {
        let mut current = self.lexer.current();
        let mut deref_count = 0;
        while let Some(Token {
            kind: TokenKind::At,
            ..
        }) = current
        {
            deref_count += 1;
            self.lexer.consume()?;
            current = self.lexer.current();
        }

        let name = expect!(
            self,
            expect TokenKind::Identifier(name),
            take move || name,
            error vec!["ident"]
        );

        expect!(
            self,
            expect TokenKind::AssignEquals,
            take move || (),
            error vec!["'='"]
        );

        let expr = self.expression()?;

        Ok(Statement::SetVariable(SetVariable {
            lhs: LValue {
                name,
                derefs: deref_count,
            },
            rhs: (None, expr),
        }))
    }
    fn statement(&mut self) -> Result<Statement, ParseError> {
        let current = self.lexer.current();
        //
        // Zero-lookahead statements
        //
        match current {
            Some(Token {
                kind: TokenKind::Var,
                ..
            }) => return self.make_variable(),
            Some(Token {
                kind: TokenKind::Return,
                ..
            }) => return self.return_(),
            Some(Token {
                kind: TokenKind::If,
                ..
            }) => return self.if_(),
            Some(Token {
                kind: TokenKind::Loop,
                ..
            }) => return self.loop_(),
            Some(Token {
                kind: TokenKind::Break,
                ..
            }) => {
                self.lexer.consume().unwrap();
                return Ok(Statement::Break);
            }
            Some(Token {
                kind: TokenKind::Print,
                ..
            }) => return self.print_(),
            _ => {}
        }
        //
        // Assignment (max lookahead 1)
        //
        match current {
            Some(Token {
                kind: TokenKind::At,
                ..
            }) => return self.assignment(),
            Some(Token {
                kind: TokenKind::Identifier(..),
                ..
            }) => {
                let peek = self.lexer.lookahead(1)?;

                if let Some(Token {
                    kind: TokenKind::AssignEquals,
                    ..
                }) = peek
                {
                    return self.assignment();
                }
            }
            _ => {}
        }
        //
        // Bare expression (assumed if none of the above)
        //
        Ok(Statement::BareExpression((None, self.expression()?)))
    }
    fn body(&mut self) -> Result<Vec<Statement>, ParseError> {
        expect!(
            self,
            expect TokenKind::OpenBrace,
            take move || (),
            error vec!["'{'"]
        );

        let mut body = Vec::new();

        loop {
            let peek = self.lexer.current();
            if matches!(
                peek,
                Some(Token {
                    kind: TokenKind::CloseBrace,
                    ..
                })
            ) {
                self.lexer.consume().unwrap();
                break;
            }
            let stmt = self.statement()?;
            expect!(
                self,
                expect TokenKind::Newline,
                take move || (),
                error vec!["newline"]
            );
            body.push(stmt);
        }

        Ok(body)
    }
    fn parameter(&mut self) -> Result<(Type, String), ParseError> {
        let name = expect!(
            self,
            expect TokenKind::Identifier(name),
            take move || name,
            error vec!["ident"]
        );
        expect!(
            self,
            expect TokenKind::Colon,
            take move || (),
            error vec!["':'"]
        );
        let type_ = self.type_annotation()?;
        Ok((type_, name))
    }
    fn function(&mut self) -> Result<Function, ParseError> {
        self.lexer.consume().unwrap();

        let name = expect!(
            self,
            expect TokenKind::Identifier(name),
            take move || name,
            error vec!["ident"]
        );

        expect!(
            self,
            expect TokenKind::OpenParen,
            take move || (),
            error vec!["'('"]
        );

        let parameters = separated!(
            self,
            parameter,
            by TokenKind::Comma,
            terminated_by TokenKind::CloseParen
        );

        expect!(
            self,
            expect TokenKind::CloseParen,
            take move || (),
            error vec!["')'"]
        );

        let current = self.lexer.current();
        let returns = if let Some(Token {
            kind: TokenKind::RightArrow,
            ..
        }) = current
        {
            self.lexer.consume()?;
            self.type_annotation()?
        } else {
            Type::Unit
        };

        let body = self.body()?;

        Ok(Function {
            name,
            parameters,
            returns,
            body: body
                .into_iter()
                .map(|stmt| parse::Span::new(stmt, None))
                .collect(),
        })
    }
    fn file(&mut self) -> Result<File, ParseError> {
        let mut functions = Vec::new();

        loop {
            let current = self.lexer.current();
            match current {
                Some(Token {
                    kind: TokenKind::Function,
                    ..
                }) => {
                    let f = self.function()?;
                    expect!(
                        self,
                        expect TokenKind::Newline,
                        take move || (),
                        error vec!["newline"]
                    );
                    functions.push(f);
                }
                _ => break,
            }
        }

        let bad_token = self.lexer.consume()?;
        if let None = bad_token {
            Ok(File { functions })
        } else {
            self.error_expected(vec!["'function'"], bad_token)
        }
    }
}

#[test]
fn test_new_parser() {
    let lexer = Lexer::new(
        Rc::new(String::from(
            "\
function baz() {
    return false
}
",
        )),
        Rc::new(String::from("<main>")),
    )
    .unwrap();
    let mut parser = Parser::new(lexer);
    let expr = parser.file();
    println!("{:#?}", expr);
}
