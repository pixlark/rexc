use std::{collections::VecDeque, rc::Rc};

#[derive(Debug)]
pub struct Span {
    pub source: Rc<String>,
    pub start: usize,
    pub end: Option<usize>,
}

#[derive(Debug)]
enum TokenKind {
    Eof,
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

#[derive(Debug)]
struct Token {
    kind: TokenKind,
    span: Span,
}

#[derive(Debug)]
enum LexErrorKind {
    UnrecognizedCharacter(char),
    IntegerLiteralTooBig,
}

#[derive(Debug)]
struct LexError {
    kind: LexErrorKind,
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

impl Lexer {
    fn new(source: Rc<String>, filename: Rc<String>) -> Result<Lexer, LexError> {
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
    fn consume(&mut self) -> Result<Option<Token>, LexError> {
        let tok = self.buffer.pop_front().unwrap();
        if self.buffer.is_empty() {
            let next_tok = self.next_token()?;
            self.buffer.push_back(next_tok);
        }
        Ok(tok)
    }
    fn current(&self) -> &Option<Token> {
        self.buffer.front().unwrap()
    }
    fn lookahead(&mut self, n: usize) -> Result<&Option<Token>, LexError> {
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
    fn span(&self, start: usize, end: usize) -> Span {
        Span {
            source: self.source.clone(),
            start,
            end: Some(end),
        }
    }
    fn half_span(&self, start: usize) -> Span {
        Span {
            source: self.source.clone(),
            start,
            end: None,
        }
    }
    fn next_token(&mut self) -> Result<Option<Token>, LexError> {
        let mut c = match self.current_char() {
            Some(c) => c,
            None => return Ok(None),
        };

        // Skip whitespace
        while c.is_whitespace() {
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

        if integer_sign.is_some() || c.is_alphanumeric() {
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
            let span = self.span(tok_start, tok_end);

            let s = buf.into_iter().collect::<String>();
            if let Ok(i) = s.parse::<i64>() {
                return Ok(Some(Token {
                    kind: TokenKind::IntegerLiteral(i),
                    span,
                }));
            } else {
                return Err(LexError {
                    kind: LexErrorKind::IntegerLiteralTooBig,
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
            let span = self.span(tok_start, tok_end);

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
        let symbol_span = self.half_span(self.u8_cursor);
        self.advance_char();
        match c {
            '(' => {
                return Ok(Some(Token {
                    kind: TokenKind::OpenParen,
                    span: symbol_span,
                }))
            }
            ')' => {
                return Ok(Some(Token {
                    kind: TokenKind::CloseParen,
                    span: symbol_span,
                }))
            }
            '{' => {
                return Ok(Some(Token {
                    kind: TokenKind::OpenBrace,
                    span: symbol_span,
                }))
            }
            '}' => {
                return Ok(Some(Token {
                    kind: TokenKind::CloseBrace,
                    span: symbol_span,
                }))
            }
            ':' => {
                return Ok(Some(Token {
                    kind: TokenKind::Colon,
                    span: symbol_span,
                }))
            }
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
            '+' => {
                return Ok(Some(Token {
                    kind: TokenKind::Plus,
                    span: symbol_span,
                }))
            }
            '*' => {
                return Ok(Some(Token {
                    kind: TokenKind::Asterisk,
                    span: symbol_span,
                }))
            }
            '/' => {
                return Ok(Some(Token {
                    kind: TokenKind::ForwardSlash,
                    span: symbol_span,
                }))
            }
            _ => {}
        }

        let unrecognized_span = self.half_span(self.u8_cursor);
        Err(LexError {
            kind: LexErrorKind::UnrecognizedCharacter(c),
            span: unrecognized_span,
        })
    }
}

#[test]
fn test_new_parser() {
    let mut lexer = Lexer::new(
        Rc::new(String::from("--15")),
        Rc::new(String::from("<main>")),
    )
    .unwrap();
    loop {
        match lexer.consume().unwrap() {
            Some(t) => println!("{:?}", t),
            None => break,
        }
    }
}
