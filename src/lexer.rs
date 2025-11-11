use std::{arch::x86_64, collections::HashMap, hash::Hash, str};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    PLUS,
    MINUS,
    SLASH,
    STAR,

    FloatLiteral,
    INTLIteral,
    StringLit,
    True,
    False,
    CharLit,

    GT,
    LT,
    GTE,
    LTE,

    BitwiseAnd,
    BitwiseOr,
    AND,
    OR,

    Equal,
    EqualEquals,
    BangEqual,
    Bang,

    LPAREN,
    RPAREN,

    LBrace,
    RBrace,
    EOF,

    Identifier,
    Fn,
    Return,
    IF,
    ELSE,

    INT,
    Char,
    Float,
    Boolean,
    Str,
    Void,
    Null,

    Comma,
    Semicolon,
}
#[derive(Debug, Clone)]
pub struct Token {
    pub lexme: Option<String>,
    pub token_type: TokenType,
    line: usize,
}

pub struct Lexer {
    stream: Vec<char>,
    start: usize,
    current: usize,
    tokens: Vec<Token>,
    line: usize,
    keywords: HashMap<String, TokenType>,
}

impl Lexer {
    pub fn new(source: String) -> Self {
        let chars: Vec<char> = source.chars().collect();
        let mut keywords = HashMap::new();
        keywords.insert("int".to_string(), TokenType::INT);
        keywords.insert("float".to_string(), TokenType::Float);
        keywords.insert("fn".to_string(), TokenType::Fn);
        keywords.insert("return".to_string(), TokenType::Return);
        keywords.insert("bool".to_string(), TokenType::Boolean);
        keywords.insert("Str".to_string(), TokenType::Str);
        keywords.insert("void".to_string(), TokenType::Void);
        keywords.insert("null".to_string(), TokenType::Null);
        keywords.insert("true".to_string(), TokenType::True);
        keywords.insert("false".to_string(), TokenType::False);
        keywords.insert("char".to_string(), TokenType::Char);
        
        keywords.insert("if".to_string(), TokenType::IF);
        keywords.insert("else".to_string(), TokenType::ELSE);
        Self {
            stream: chars,
            start: 0,
            current: 0,
            tokens: vec![],
            keywords,
            line: 1,
        }
    }

    fn peek(&self) -> char {
        if self.eof() {
            return '\0';
        }
        self.stream[self.current]
    }

    fn advance(&mut self) -> char {
        let ch = self.stream[self.current];
        self.current += 1;
        ch
    }

    #[inline]
    fn eof(&self) -> bool {
        self.current >= self.stream.len()
    }

    pub fn lexe(&mut self) -> &[Token] {
        while !self.eof() {
            self.start = self.current;
            self.scan_tokens();
        }
        self.push_token(TokenType::EOF);
        &self.tokens
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.stream.len() {
            return '\0';
        }
        self.stream[self.current + 1]
    }
    fn add_double_check(&mut self, ch: char, t1: TokenType, t2: TokenType) {
        if ch == self.peek() {
            self.push_token(t1);
            self.advance();
        } else {
            self.push_token(t2);
        }
    }
    fn parse_string(&mut self) {
        while !self.eof() && self.peek() != '"' {
            self.advance();
        }
        self.advance();
        let string = self.stream[self.start + 1..self.current - 1]
            .iter()
            .collect::<String>();
        self.push_token_2(TokenType::StringLit, string);
    }
    fn parse_char(&mut self) {
        while !self.eof() && self.peek() != '\'' {
            self.advance();
        }
        self.advance();
        let string = self.stream[self.start + 1..self.current - 1]
            .iter()
            .collect::<String>();
        self.push_token_2(TokenType::CharLit, string);
    }

    fn scan_tokens(&mut self) {
        let c = self.advance();
        match c {
            ' ' | '\r' | '\t' => {}
            '\n' => {
                self.line += 1;
            }
            '{' => self.push_token(TokenType::LBrace),
            '}' => self.push_token(TokenType::RBrace),
            ';' => self.push_token(TokenType::Semicolon),
            ',' => self.push_token(TokenType::Comma),
            '(' => self.push_token(TokenType::LPAREN),
            ')' => self.push_token(TokenType::RPAREN),
            '-' => self.push_token(TokenType::MINUS),
            '*' => self.push_token(TokenType::STAR),
            '+' => self.push_token(TokenType::PLUS),
            '/' => self.push_token(TokenType::SLASH),
            '!' => self.add_double_check('=', TokenType::BangEqual, TokenType::Bang),
            '=' => self.add_double_check('=', TokenType::EqualEquals, TokenType::Equal),
            '>' => self.add_double_check('=', TokenType::GTE, TokenType::GT),
            '<' => self.add_double_check('=', TokenType::LTE, TokenType::LT),
            '|' => self.add_double_check('|', TokenType::OR, TokenType::BitwiseOr),
            '&' => self.add_double_check('&', TokenType::AND, TokenType::BitwiseAnd),
            '\'' => self.parse_char(),
            '"' => self.parse_string(),
            a if a.is_ascii_digit() => {
                while self.peek().is_ascii_digit() {
                    self.advance();
                }
                let mut is_float = false;
                if self.peek() == '.' && self.peek_next().is_ascii_digit() {
                    self.advance();
                    is_float = true;
                    while self.peek().is_ascii_digit() {
                        self.advance();
                    }
                }
                let num = self.stream[self.start..self.current]
                    .to_vec()
                    .iter()
                    .collect::<String>();
                if is_float {
                    self.push_token_2(TokenType::FloatLiteral, num);
                } else {
                    self.push_token_2(TokenType::INTLIteral, num);
                }
            }
            a if a.is_ascii_alphabetic() => {
                while self.peek().is_ascii_alphanumeric() {
                    self.advance();
                }

                let word = self.stream[self.start..self.current]
                    .to_vec()
                    .iter()
                    .collect::<String>();
                let val = self.keywords.get(&word);
                if self.keywords.contains_key(&word) {
                    self.push_token(*val.unwrap());
                } else {
                    self.push_token_2(TokenType::Identifier, word);
                }
            }
            _ => panic!("error while lexing {:?}", c),
        }
    }
    fn push_token_2(&mut self, token_type: TokenType, lexme: String) {
        let token = Token {
            lexme: Some(lexme),
            token_type,
            line: self.line,
        };
        self.tokens.push(token);
    }

    fn push_token(&mut self, token_type: TokenType) {
        let token = Token {
            lexme: None,
            token_type,
            line: self.line,
        };
        self.tokens.push(token);
    }
}
