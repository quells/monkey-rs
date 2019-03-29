use std::collections::HashSet;
use std::error::Error;
use std::str::FromStr;

use crate::lex::{Token, TokenKind};

#[cfg(test)]
mod test;

#[macro_use]
mod helpers;
use parse::helpers::*;

mod ast;
use parse::ast::*;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Vec<TokenKind>, Token),
    IllegalToken(Token),
    InvalidInteger(Token),
    UnexpectedEndOfTokens,
}

impl Error for ParseError {}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let msg = match self {
            ParseError::UnexpectedToken(e, t) => format!(
                "unexpected token at {}:{} : expected one of {:?} found {:?}",
                t.line, t.character, e, t
            ),
            ParseError::UnexpectedEndOfTokens => "unexpected end of token stream".to_owned(),
            ParseError::IllegalToken(t) => format!("found illegal token {:?}", t),
            ParseError::InvalidInteger(t) => format!("unable to parse `{}` as integer", t.literal),
        };
        write!(f, "{}", msg)
    }
}

impl ParseError {
    #[allow(dead_code)]
    fn is_equivalent_to(&self, other: &ParseError) -> bool {
        match (self, other) {
            (
                ParseError::UnexpectedToken(expected_l, found_l),
                ParseError::UnexpectedToken(expected_r, found_r),
            ) => {
                if !found_l.is_equivalent_to(found_r) {
                    return false;
                }

                let hash_l: HashSet<_> = expected_l.iter().collect();
                let hash_r: HashSet<_> = expected_r.iter().collect();
                let diff: HashSet<_> = hash_l.symmetric_difference(&hash_r).collect();

                diff.is_empty()
            }
            (ParseError::UnexpectedEndOfTokens, ParseError::UnexpectedEndOfTokens) => true,
            (ParseError::IllegalToken(token_l), ParseError::IllegalToken(token_r)) => {
                token_l.is_equivalent_to(token_r)
            }
            (ParseError::InvalidInteger(token_l), ParseError::InvalidInteger(token_r)) => {
                token_l.is_equivalent_to(token_r)
            }
            _ => false,
        }
    }
}

struct Parser {
    tokens: Vec<Token>,
    len: usize,
    cursor: usize,
}

impl Parser {
    fn new(tokens: &[Token]) -> Parser {
        let ts = tokens.to_vec();
        let len = ts.len();
        Parser {
            tokens: ts,
            len,
            cursor: 0,
        }
    }

    fn next(&mut self) -> Option<Token> {
        if self.cursor < self.len {
            let next = self.tokens[self.cursor].clone();
            self.cursor += 1;
            Some(next)
        } else {
            None
        }
    }

    fn peek(&self) -> Option<Token> {
        if self.cursor < self.len {
            let next = self.tokens[self.cursor].clone();
            Some(next)
        } else {
            None
        }
    }

    fn eat(&mut self, expected: TokenKind) -> Result<Token, ParseError> {
        match self.next() {
            Some(next) => {
                if next.kind == expected {
                    Ok(next)
                } else {
                    Err(ParseError::UnexpectedToken(vec![expected], next))
                }
            }
            None => Err(ParseError::UnexpectedEndOfTokens),
        }
    }

    fn next_statement(&mut self) -> Result<Statement, ParseError> {
        let first_token = match self.peek() {
            Some(t) => t,
            None => {
                return Err(ParseError::UnexpectedEndOfTokens);
            }
        };

        match first_token.kind {
            TokenKind::Let => {
                let _let = self.eat(TokenKind::Let)?;
                let id = self.eat(TokenKind::Identifier)?;
                self.eat(TokenKind::Assign)?;
                let expr = self.next_expression()?;
                self.eat(TokenKind::Semicolon)?;
                Ok(Statement::Assignment(_let, Identifier(id), expr))
            }
            TokenKind::Return => {
                let _return = self.eat(TokenKind::Return)?;
                let value = self.next_expression()?;
                self.eat(TokenKind::Semicolon)?;
                Ok(Statement::Return(_return, value))
            }
            _ => Err(ParseError::UnexpectedToken(
                vec![TokenKind::Let],
                first_token,
            )),
        }
    }

    fn next_expression(&mut self) -> Result<Expression, ParseError> {
        let first_token = match self.peek() {
            Some(t) => t,
            None => {
                return Err(ParseError::UnexpectedEndOfTokens);
            }
        };

        match self.next_equality_expr(first_token) {
            Ok(expr) => Ok(Box::new(expr)),
            Err(e) => Err(e),
        }
    }

    fn next_equality_expr(&mut self, first_token: Token) -> Result<EqualityExpr, ParseError> {
        let lhs = self.next_relational_expr(first_token);
        // FIXME: check for equality operator
        match lhs {
            Ok(expr) => Ok(EqualityExpr::Wrapped(expr)),
            Err(e) => Err(e),
        }
    }

    fn next_relational_expr(&mut self, first_token: Token) -> Result<RelationalExpr, ParseError> {
        let lhs = self.next_additive_expr(first_token);
        // FIXME: check for relational operator
        match lhs {
            Ok(expr) => Ok(RelationalExpr::Wrapped(expr)),
            Err(e) => Err(e),
        }
    }

    impl_next_binop!(next_additive_expr; AdditiveExpr; next_term; AdditiveBinOp;
        Plus, Add;
        Minus, Subtract
    );

    impl_next_binop!(next_term; Term; next_factor; TermBinOp;
        Asterisk, Multiply;
        Slash, Divide
    );

    fn next_factor(&mut self, first_token: Token) -> Result<Factor, ParseError> {
        match first_token.kind {
            TokenKind::Integer => {
                let int = self.eat(TokenKind::Integer)?;
                let parsed = match isize::from_str(&int.literal) {
                    Ok(x) => x,
                    Err(_) => return Err(ParseError::InvalidInteger(int)),
                };
                Ok(Factor::Integer(int, parsed))
            }
            TokenKind::Identifier => {
                let id = self.eat(TokenKind::Identifier)?;
                let default = Factor::Identifier(Identifier(id.clone()));
                let next_token = match self.peek() {
                    Some(t) => t,
                    None => return Ok(default),
                };
                match next_token.kind {
                    TokenKind::LParen => {
                        self.eat(TokenKind::LParen)?;
                        let mut params = Vec::new();
                        loop {
                            // Exit loop on `)` or EOF
                            let eof = Err(ParseError::UnexpectedEndOfTokens);
                            match self.peek() {
                                Some(t) => match t.kind {
                                    TokenKind::RParen => {
                                        self.eat(TokenKind::RParen)?;
                                        break;
                                    }
                                    TokenKind::EOF => return eof,
                                    _ => (),
                                },
                                None => return eof,
                            };

                            if !params.is_empty() {
                                self.eat(TokenKind::Comma)?;
                            }
                            params.push(self.next_expression()?);
                        }
                        Ok(Factor::FunctionCall(id, params))
                    }
                    _ => Ok(default),
                }
            }
            _ => Err(ParseError::UnexpectedToken(
                vec![TokenKind::Integer, TokenKind::Identifier],
                first_token,
            )),
        }
    }
}

pub fn parse(tokens: &[Token]) -> Result<Program, ParseError> {
    let mut statements = Vec::new();
    let mut parser = Parser::new(&tokens);

    while let Some(next_token) = parser.peek() {
        match next_token.kind {
            TokenKind::EOF => break,
            TokenKind::Semicolon => {
                parser.eat(TokenKind::Semicolon)?;
                continue;
            }
            _ => (),
        }

        let statement = parser.next_statement()?;
        statements.push(statement);
    }

    parser.eat(TokenKind::EOF)?;
    match parser.peek() {
        Some(t) => Err(ParseError::IllegalToken(t)),
        None => Ok(Program::new(&statements)),
    }
}
