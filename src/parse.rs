use crate::lex::{Token, TokenKind};

pub trait Node {
    fn token(&self) -> Option<Token>;
}

#[derive(Debug)]
pub struct Program {
    statements: Vec<Statement>,
}

impl Node for Program {
    fn token(&self) -> Option<Token> {
        match (*self.statements).into_iter().next() {
            Some(s) => s.token(),
            None => None,
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    InitialAssignment(Token, Expression, Expression),
}

impl Node for Statement {
    fn token(&self) -> Option<Token> {
        let t = match self {
            Statement::InitialAssignment(t, _, _) => t.clone(),
        };
        Some(t)
    }
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Token),
    Integer(Token),
}

impl Node for Expression {
    fn token(&self) -> Option<Token> {
        let t = match self {
            Expression::Identifier(t) => t.clone(),
            Expression::Integer(t) => t.clone(),
        };
        Some(t)
    }
}

use std::error::Error;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(TokenKind, Token),
    UnexpectedEndOfTokens,
    IllegalToken(Token),
}

impl Error for ParseError {}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let msg = match self {
            ParseError::UnexpectedToken(e, t) => {
                format!("unexpected token at {}:{} : expected {:?} found {:?}", t.line, t.character, e, t)
            },
            ParseError::UnexpectedEndOfTokens => "unexpected end of token stream".to_owned(),
            ParseError::IllegalToken(t) => {
                format!("found illegal token {:?}", t)
            }
        };
        write!(f, "{}", msg)
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
            len: len,
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
                    Err(ParseError::UnexpectedToken(expected, next))
                }
            },
            None => {
                Err(ParseError::UnexpectedEndOfTokens)
            }
        }
    }
}

pub fn parse(tokens: &[Token]) -> Result<Program, ParseError> {
    let parser = Parser::new(&tokens);
    match tokens.len() {
        0 => Err(ParseError::UnexpectedEndOfTokens),
        _ => Err(ParseError::IllegalToken(tokens[0].clone())),
    }
}