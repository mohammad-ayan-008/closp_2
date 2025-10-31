#![allow(dead_code, non_camel_case_types)]
use std::vec;
use crate::expressions::*;
use crate::statements::*;
use crate::lexer::{Token, TokenType};

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn parse_program(&mut self) -> Result<Vec<Item>, String> {
        let mut items = vec![];
        while !self.is_eof() {
            match self.parse_items() {
                Ok(a) => items.push(a),
                Err(a) => panic!("{a}"),
            }
        }
        Ok(items)
    }

    fn parse_items(&mut self) -> Result<Item, String> {
        match self.peek().token_type {
            TokenType::Fn => Ok(Item::Function(self.parse_function()?)),
            _ => Err("Expected Function at high level code".to_string()),
        }
    }

    fn parse_function(&mut self) -> Result<Function, String> {
        self.consume(TokenType::Fn)?;
        let type_ = self.parse_type()?;
        let name = self.consume(TokenType::Identifier)?;
        self.consume(TokenType::LPAREN)?;
        let params = self.parse_parameters()?;
        let block = self.parse_block()?;
        Ok(Function {
            name: name.lexme.unwrap(),
            return_type: type_,
            params,
            body: block,
        })
    }

    pub fn parse_block(&mut self) -> Result<Block, String> {
        self.consume(TokenType::LBrace)?;
        let mut statements = Vec::new();
        while !self.match_(TokenType::RBrace) && !self.is_eof() {
            match self.parse_statements() {
                Ok(a) => statements.push(a),
                Err(a) => {
                    return Err(a);
                }
            }
        }

        self.consume(TokenType::RBrace)?;
        Ok(Block { statements })
    }
    fn parse_statements(&mut self) -> Result<Statement, String> {
        match self.peek().token_type {
            TokenType::Return => self.return_statement(),
            TokenType::Identifier=>self.parse_expression_statement(),
            TokenType::LBrace=>{
                Ok(Statement::Block(self.parse_block()?))
            }
            _ => Err(format!("expected return found {:?} ", self.peek())),
        }
    }
    pub fn parse_expression_statement(&mut self)->Result<Statement,String>{
        let exp = self.parse_expression()?;
        self.consume(TokenType::Semicolon)?;
        Ok(Statement::ExpressionStatement(exp))
    }

    fn return_statement(&mut self) -> Result<Statement, String> {
        self.consume(TokenType::Return)?;
        let mut exp = None;
        if !self.match_(TokenType::Semicolon) {
            let exps = self.parse_expression()?;
            exp = Some(exps);
        }
        self.consume(TokenType::Semicolon)?;
        Ok(Statement::Return(exp))
    }
    
    pub fn match_(&mut self, type_: TokenType) -> bool {
        self.peek().token_type == type_
    }

    pub fn match_consume(&mut self, tk: TokenType) -> bool {
        if self.peek().token_type == tk {
            self.advance();
            true
        } else {
            false
        }
    }

    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, String> {
        let mut params = vec![];
        while !self.match_(TokenType::RPAREN) {
            let type_ = self.parse_type()?;
            let ident = self.consume(TokenType::Identifier)?;
            params.push(Parameter {
                name: ident.lexme.unwrap(),
                type_,
            });

            if !self.match_consume(TokenType::Comma) {
                break;
            }
        }
        self.consume(TokenType::RPAREN)?;
        Ok(params)
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        let type_=match self.peek().token_type {
            TokenType::INT => {
                self.advance();
                Type::Int
            },
            TokenType::Float=> {
                self.advance();
                Type::Float
            },
            TokenType::Str=> {
                self.advance();
                Type::Str
            },
            TokenType::Boolean => {
                self.advance();
                Type::Boolean
            },
            TokenType::Void =>{
                self.advance();
                Type::Void
            }
            _ => {
                return Err(format!("found {:?} expected datatytpe", self.peek()));
            }
        };
        self.parse_pointer(type_)
    }
    fn parse_pointer(&mut self,mut type_:Type)->Result<Type,String>{
        while self.match_(TokenType::STAR){
            self.advance();
            type_ = Type::Pointer(Box::new(type_));
        }
        Ok(type_)
    }

    pub fn new(tokens: &[Token]) -> Self {
        Self {
            tokens: tokens.to_vec(),
            current: 0,
        }
    }
    fn advance(&mut self) -> Token {
        let tk = &self.tokens[self.current];
        self.current += 1;
        tk.clone()
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn is_eof(&self) -> bool {
        self.tokens[self.current].token_type == TokenType::EOF
    }

    pub fn parse_expression(&mut self) -> Result<Expression, String> {
        self.parse_expression_precidence(0)
    }

    fn parse_expression_precidence(&mut self, min_prec: u8) -> Result<Expression, String> {
        let mut left = self.parse_cast()?;
        while let Some(a) = self.get_binary_op() {
            let (prec, assoc) = self.get_precidence(&a);

            if prec < min_prec {
                break;
            }

            let _op_token = self.advance();
            let right = self.parse_expression_precidence(assoc)?;
            left = Expression::Binary {
                lhs: Box::new(left),
                op: a,
                rhs: Box::new(right),
            }
        }
        Ok(left)
    }

    fn parse_cast(&mut self)->Result<Expression,String>{
        if self.match_(TokenType::LPAREN){
            let pos = self.current;
            let _tk = self.advance();

            if let Ok(a) = self.parse_type() && self.match_(TokenType::RPAREN){
                    let _tk = self.advance();
                    let expr = self.parse_cast()?;
                    return Ok(Expression::Cast { expected: a, expr: Box::new(expr) }); 
            }
            self.current = pos;
        }
        self.parse_unary()
    }

    fn parse_unary(&mut self) -> Result<Expression, String> {
        match self.peek().token_type {
            TokenType::MINUS => {
                let _token = self.advance();
                let exp = self.parse_cast()?;
                Ok(Expression::Unary {
                    token:UnaryOP::Negate,
                    exp: Box::new(exp),
                })
            }
            TokenType::Bang => {
                let _token = self.advance();
                let exp = self.parse_cast()?;
                Ok(Expression::Unary {
                    token:UnaryOP::Not,
                    exp: Box::new(exp),
                })
            }
            _ => self.parse_fn_call(),
        }
    }
    pub fn parse_arguments(&mut self)->Result<Vec<Expression>,String>{
        let mut args= vec![];
        while !self.match_(TokenType::RPAREN){
            let arg = self.parse_expression()?;
            args.push(arg);
            if !self.match_consume(TokenType::Comma){
                break;
            }
        }
        self.consume(TokenType::RPAREN)?;
        Ok(args)
    }
    pub fn parse_fn_call(&mut self)->Result<Expression,String>{
        let mut primary = self.parse_primary()?;
        if self.match_(TokenType::LPAREN){
            if let Expression::Identifier(a) = &primary{
                self.consume(TokenType::LPAREN)?;
                let args = self.parse_arguments()?;
                primary = Expression::FunctionCall { name: a.clone(), args };
            }else {
                return Err("Expected function name".to_string());
            }
        }
        Ok(primary)
    }

    fn parse_primary(&mut self) -> Result<Expression, String> {
        let tk = self.advance();
        match tk.token_type {
            TokenType::INTLIteral => {
                let num = tk.lexme.unwrap().parse::<i64>().unwrap();
                Ok(Expression::Int_Literal(num))
            },
            TokenType::FloatLiteral => {
                let num = tk.lexme.unwrap().parse::<f64>().unwrap();
                Ok(Expression::Float_Literal(num))
            },
            TokenType::StringLit => Ok(Expression::String_Literal(tk.lexme.unwrap())),
            TokenType::LPAREN => {
                let expr = self.parse_expression()?;
                self.consume(TokenType::RPAREN)?;
                Ok(expr)
            },
            TokenType::Identifier => Ok(Expression::Identifier(tk.lexme.unwrap())),
            TokenType::Null=>Ok(Expression::Null),
            _ => Err(format!("found {:?} previous to {:?}", self.peek(),self.previous())),
        }
    }

    fn previous(&self)->&Token{
        &self.tokens[self.current -1]
    }
    fn consume(&mut self, token: TokenType) -> Result<Token, String> {
        if self.peek().token_type == token {
            return Ok(self.advance());
        }
        Err(format!("Expected {:?} found {:?}", token, self.peek()))
    }
    fn get_binary_op(&mut self) -> Option<Binaryop> {
        if self.is_eof() {
            return None;
        }
        match &self.peek().token_type {
            TokenType::PLUS => Some(Binaryop::ADD),
            TokenType::MINUS => Some(Binaryop::SUB),
            TokenType::STAR => Some(Binaryop::MUL),
            TokenType::SLASH => Some(Binaryop::DIV),
            TokenType::BangEqual => Some(Binaryop::NotEq),
            TokenType::EqualEquals => Some(Binaryop::EqualEqual),
            TokenType::GT => Some(Binaryop::GT),
            TokenType::GTE => Some(Binaryop::GTE),
            TokenType::LT => Some(Binaryop::LT),
            TokenType::LTE => Some(Binaryop::LTE),
            TokenType::BitwiseAnd => Some(Binaryop::BITAND),
            TokenType::BitwiseOr => Some(Binaryop::BITOR),
            TokenType::AND => Some(Binaryop::And),
            TokenType::OR => Some(Binaryop::Or),
            _ => None,
        }
    }

    fn get_precidence(&mut self, op: &Binaryop) -> (u8, u8) {
        match op {
            Binaryop::BITOR => (1, 2),
            Binaryop::BITAND => (3, 4),
            Binaryop::Or => (5, 6),
            Binaryop::And => (7, 8),
            Binaryop::NotEq | Binaryop::EqualEqual => (9, 10),
            Binaryop::GT | Binaryop::LT | Binaryop::LTE | Binaryop::GTE => (11, 12),
            Binaryop::SUB | Binaryop::ADD => (13, 14),
            Binaryop::DIV | Binaryop::MUL => (15, 16),
            // right associatove so high right bp
        }
    }
}
