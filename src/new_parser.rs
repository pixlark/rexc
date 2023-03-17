use std::{cell::RefCell, collections::VecDeque, rc::Rc};

use super::ast::*;
use super::ir::Operation;

#[derive(Debug, Clone)]
pub enum Span {
    Empty,
    Eof {
        source: Rc<String>,
        filename: Rc<String>,
    },
    Char {
        source: Rc<String>,
        filename: Rc<String>,
        pos: usize,
    },
    Range {
        source: Rc<String>,
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
pub enum ParseErrorKind {
    UnrecognizedCharacter(char),
    IntegerLiteralTooBig,
    ParserExpected {
        expected: Vec<&'static str>,
        got: Option<TokenKind>,
    },
}

#[derive(Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct Lexer {
    source: Rc<String>,
    chars: Vec<char>,
    cursor: usize,
    u8_cursor: usize,
    filename: Rc<String>,

    buffer: VecDeque<Option<Token>>,
}

#[derive(Debug)]
pub struct Parser {
    pub lexer: Lexer,
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
    pub fn new(source: Rc<String>, filename: Rc<String>) -> Result<Lexer, ParseError> {
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
    /// Does not do newline handling. Use `.consume()` instead.
    fn base_consume(&mut self) -> Result<Option<Token>, ParseError> {
        let tok = self.buffer.pop_front().unwrap();
        if self.buffer.is_empty() {
            let next_tok = self.next_token()?;
            self.buffer.push_back(next_tok);
        }
        Ok(tok)
    }
    /// Does not do newline handling. Use `.current()` instead.
    fn base_current(&mut self) -> &Option<Token> {
        self.buffer.front().unwrap()
    }
    /// Does not do newline handling. Use `.lookahead(n)` instead.
    fn base_lookahead(&mut self, n: usize) -> Result<&Option<Token>, ParseError> {
        let mut current_lookahead = self.buffer.len();

        while n + 1 > current_lookahead {
            let next_tok = self.next_token()?;
            self.buffer.push_back(next_tok);
            current_lookahead += 1;
        }

        Ok(self.buffer.iter().nth(n).unwrap())
    }
    fn consume(&mut self) -> Result<Option<Token>, ParseError> {
        self.consume_newline()?;
        self.base_consume()
    }
    fn current(&mut self) -> Result<&Option<Token>, ParseError> {
        if self.is_newline() {
            let mut n = 1usize;
            loop {
                if !matches!(
                    self.base_lookahead(n)?,
                    Some(Token {
                        kind: TokenKind::Newline,
                        ..
                    })
                ) {
                    break;
                }
                n += 1;
            }
            self.base_lookahead(n)
        } else {
            Ok(self.buffer.front().unwrap())
        }
    }
    fn real_lookahead_count(&self) -> (usize, usize) {
        let mut count = 0usize;
        let mut cursor = 0usize;
        for tok in self.buffer.iter() {
            cursor += 1;
            if !matches!(
                tok,
                Some(Token {
                    kind: TokenKind::Newline,
                    ..
                })
            ) {
                count += 1;
            }
        }
        return (count, cursor - 1);
    }
    fn lookahead(&mut self, n: usize) -> Result<&Option<Token>, ParseError> {
        let (mut real_lookahead_count, mut cursor) = self.real_lookahead_count();

        while n + 1 > real_lookahead_count {
            let next_tok = self.next_token()?;
            cursor += 1;
            if !matches!(
                &next_tok,
                Some(Token {
                    kind: TokenKind::Newline,
                    ..
                })
            ) {
                real_lookahead_count += 1;
            }
            self.buffer.push_back(next_tok);
        }

        Ok(self.buffer.iter().nth(cursor).unwrap())
    }
    fn is_newline(&self) -> bool {
        matches!(
            self.buffer.front().unwrap(),
            Some(Token {
                kind: TokenKind::Newline,
                ..
            })
        )
    }
    fn consume_newline(&mut self) -> Result<bool, ParseError> {
        let mut any = false;
        while self.is_newline() {
            self.base_consume()?;
            any = true;
        }
        Ok(any)
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
            source: self.source.clone(),
            filename: self.filename.clone(),
            start,
            end,
        }
    }
    fn span_char(&self, pos: usize) -> Span {
        Span::Char {
            source: self.source.clone(),
            filename: self.filename.clone(),
            pos,
        }
    }
    fn span_eof(&self) -> Span {
        Span::Eof {
            source: self.source.clone(),
            filename: self.filename.clone(),
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

        if c == '#' {
            while c != '\n' {
                self.advance_char();
                c = match self.current_char() {
                    Some(c) => c,
                    None => return Ok(None),
                }
            }
            return self.next_token();
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
                let current = self.lexer.current()?;
                if let Some(tok) = current {
                    let tok_span = tok.span.clone();
                    let op = operator_list.iter().find(|(k, _)| {
                        std::mem::discriminant(k) == std::mem::discriminant(&tok.kind)
                    });
                    if let Some(op) = op {
                        self.lexer.consume()?;
                        let rhs = self.$subparser()?;
                        lhs = Expression {
                            kind: ExpressionKind::Operation(
                                op.1,
                                Box::new((None, lhs)),
                                Box::new((None, rhs)),
                            ),
                            span: tok_span,
                        };
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
            let current = $self.lexer.current()?;
            if let Some(Token {
                kind: $terminator, ..
            }) = current
            {
                break;
            }

            vec.push($self.$subparser()?);

            let current = $self.lexer.current()?;
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
        expect!($self, expect $pat, take $closure, error $expect_list, span)
    }};
    ($self:expr, expect $pat:pat, take $closure:expr, error $expect_list:expr, $span:ident) => {{
        let tok = $self.lexer.consume()?;
        if let Some(Token { kind: $pat, $span }) = tok {
            $closure()
        } else {
            return $self.error_expected($expect_list, tok);
        }
    }};
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        Parser { lexer }
    }
    fn expect_newline(&mut self) -> Result<(), ParseError> {
        if self.lexer.consume_newline()? {
            Ok(())
        } else {
            let tok = self.lexer.consume().unwrap();
            self.error_expected(vec!["newline"], tok)
        }
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
                span,
            }) => Ok(Expression::new(
                ExpressionKind::Literal(Literal::Int(i)),
                span,
            )),
            Some(Token {
                kind: TokenKind::True,
                span,
            }) => Ok(Expression::new(
                ExpressionKind::Literal(Literal::Bool(true)),
                span,
            )),
            Some(Token {
                kind: TokenKind::False,
                span,
            }) => Ok(Expression::new(
                ExpressionKind::Literal(Literal::Bool(false)),
                span,
            )),
            Some(Token {
                kind: TokenKind::Identifier(name),
                span,
            }) => Ok(Expression::new(ExpressionKind::Variable(name), span)),
            Some(Token {
                kind: TokenKind::OpenParen,
                span,
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
        let current = self.lexer.current()?;

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

                    let (name, span) = match ident_tok {
                        Some(Token {
                            kind: TokenKind::Identifier(name),
                            span,
                        }) => (name, span),
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
                    return Ok(Expression::new(
                        ExpressionKind::FunctionCall(
                            None,
                            name,
                            interior.into_iter().map(|e| (None, e)).collect(),
                        ),
                        span,
                    ));
                }
            }
            Some(Token {
                kind: TokenKind::Alloc,
                span,
            }) => {
                let span = span.clone();
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
                    return Ok(Expression::new(
                        ExpressionKind::Allocate(Box::new((None, interior))),
                        span,
                    ));
                }
            }
            _ => {}
        }
        self.atom()
    }
    fn dereference(&mut self) -> Result<Expression, ParseError> {
        let mut deref_count = 0;
        while matches!(
            self.lexer.current()?,
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
            let span = interior.span.clone();
            interior = Expression::new(
                ExpressionKind::Dereference(deref_count, Rc::new(RefCell::new(interior))),
                span,
            );
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
            }) => {
                expect!(
                    self,
                    expect TokenKind::OpenParen,
                    take move || (),
                    error vec!["'('"]
                );
                let params = separated!(
                    self,
                    type_annotation,
                    by TokenKind::Comma,
                    terminated_by TokenKind::CloseParen
                );
                expect!(
                    self,
                    expect TokenKind::CloseParen,
                    take move || (),
                    error vec!["')'"]
                );

                let tok = self.lexer.current()?;
                let returns = if let Some(Token {
                    kind: TokenKind::RightArrow,
                    ..
                }) = tok
                {
                    self.lexer.consume().unwrap();
                    Some(self.type_annotation()?)
                } else {
                    None
                };

                Ok(Type::Function(Rc::new((
                    returns.unwrap_or(Type::Unit),
                    params,
                ))))
            }
            _ => self.error_expected(vec!["int", "bool", "func"], tok),
        }
    }
    fn make_variable(&mut self) -> Result<Statement, ParseError> {
        let span = self.lexer.consume().unwrap().unwrap().span; // `var`

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

        Ok(Statement::new(
            StatementKind::MakeVariable(MakeVariable {
                type_,
                lhs: name,
                rhs: expr,
            }),
            span,
        ))
    }
    fn return_(&mut self) -> Result<Statement, ParseError> {
        let span = self.lexer.consume().unwrap().unwrap().span;

        if self.lexer.is_newline() {
            return Ok(Statement::new(
                StatementKind::Return((None, Expression::new(ExpressionKind::Unit, span.clone()))),
                span,
            ));
        }

        let inner = self.expression()?;

        Ok(Statement::new(StatementKind::Return((None, inner)), span))
    }
    fn print_(&mut self) -> Result<Statement, ParseError> {
        let span = self.lexer.consume().unwrap().unwrap().span;

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
            }) => Ok(Statement::new(StatementKind::Print((None, inner)), span)),
            _ => self.error_expected(vec!["')'"], end),
        }
    }
    fn if_(&mut self) -> Result<Statement, ParseError> {
        let span = self.lexer.consume().unwrap().unwrap().span;

        let condition = self.expression()?;

        let body = self.body()?;

        Ok(Statement::new(
            StatementKind::If(If {
                condition: (None, condition),
                body,
            }),
            span,
        ))
    }
    fn loop_(&mut self) -> Result<Statement, ParseError> {
        let span = self.lexer.consume().unwrap().unwrap().span;

        let body = self.body()?;

        Ok(Statement::new(StatementKind::Loop(body), span))
    }
    fn lvalue(&mut self) -> Result<LValue, ParseError> {
        unimplemented!()
    }
    fn assignment(&mut self) -> Result<Statement, ParseError> {
        let mut current = self.lexer.current()?;
        let mut deref_count = 0;
        while let Some(Token {
            kind: TokenKind::At,
            ..
        }) = current
        {
            deref_count += 1;
            self.lexer.consume()?;
            current = self.lexer.current()?;
        }

        let name = expect!(
            self,
            expect TokenKind::Identifier(name),
            take move || name,
            error vec!["ident"]
        );

        let span = expect!(
            self,
            expect TokenKind::AssignEquals,
            take move || span,
            error vec!["'='"],
            span
        );

        let expr = self.expression()?;

        Ok(Statement::new(
            StatementKind::SetVariable(SetVariable {
                lhs: LValue {
                    name,
                    derefs: deref_count,
                },
                rhs: (None, expr),
            }),
            span,
        ))
    }
    fn statement(&mut self) -> Result<Statement, ParseError> {
        let current = self.lexer.current()?;
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
                span,
            }) => {
                let span = span.clone();
                self.lexer.consume().unwrap();
                return Ok(Statement::new(StatementKind::Break, span));
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
        let expr = self.expression()?;
        let span = expr.span.clone();
        Ok(Statement::new(
            StatementKind::BareExpression((None, expr)),
            span,
        ))
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
            let peek = self.lexer.current()?;
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
            self.expect_newline()?;
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

        let current = self.lexer.current()?;
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
            body,
        })
    }
    pub fn file(&mut self) -> Result<File, ParseError> {
        let mut functions = Vec::new();

        loop {
            let current = self.lexer.current()?;
            match current {
                Some(Token {
                    kind: TokenKind::Function,
                    ..
                }) => {
                    let f = self.function()?;
                    self.expect_newline()?;
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
function foo() {
    return
}
function bar(x: int) -> int {
    x = x + 1
    return x
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
