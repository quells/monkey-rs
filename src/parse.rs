use crate::lex::{Token, TokenKind};

pub trait Node {
    fn token(&self) -> Option<Token>;
}

#[derive(Debug)]
pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    fn new(statements: &[Statement]) -> Program {
        Program {
            statements: statements.to_vec(),
        }
    }
}

impl Node for Program {
    fn token(&self) -> Option<Token> {
        match (*self.statements).into_iter().next() {
            Some(s) => s.token(),
            None => None,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Statement {
    InitialAssignment(Token, Expression, Expression),
}

impl Statement {
    #[allow(dead_code)]
    fn is_equivalent_to(&self, other: &Statement) -> bool {
        match (self, other) {
            (Statement::InitialAssignment(let_l, id_l, expr_l), Statement::InitialAssignment(let_r, id_r, expr_r)) => {
                let_l.is_equivalent_to(let_r) && id_l.is_equivalent_to(id_r) && expr_l.is_equivalent_to(expr_r)
            },
        }
    }
}

impl Node for Statement {
    fn token(&self) -> Option<Token> {
        let t = match self {
            Statement::InitialAssignment(t, _, _) => t.clone(),
        };
        Some(t)
    }
}

#[derive(Clone, Debug)]
pub enum Expression {
    Identifier(Token),
    Integer(Token),
}

impl Expression {
    fn is_equivalent_to(&self, other: &Expression) -> bool {
        match (self, other) {
            (Expression::Identifier(l), Expression::Identifier(r)) => l.is_equivalent_to(r),
            (Expression::Integer(l), Expression::Integer(r)) => l.is_equivalent_to(r),
            _ => false,
        }
    }
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
    UnexpectedToken(Vec<TokenKind>, Token),
    UnexpectedEndOfTokens,
    IllegalToken(Token),
}

impl Error for ParseError {}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let msg = match self {
            ParseError::UnexpectedToken(e, t) => {
                format!("unexpected token at {}:{} : expected one of {:?} found {:?}", t.line, t.character, e, t)
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
                    Err(ParseError::UnexpectedToken(vec![expected], next))
                }
            },
            None => {
                Err(ParseError::UnexpectedEndOfTokens)
            }
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
                Ok(Statement::InitialAssignment(_let, Expression::Identifier(id), expr))
            },
            _ => {
                return Err(ParseError::UnexpectedToken(vec![TokenKind::Let], first_token));
            }
        }
    }

    fn next_expression(&mut self) -> Result<Expression, ParseError> {
        let first_token = match self.peek() {
            Some(t) => t,
            None => {
                return Err(ParseError::UnexpectedEndOfTokens);
            }
        };

        match first_token.kind {
            TokenKind::Integer => {
                let int = self.eat(TokenKind::Integer)?;
                Ok(Expression::Integer(int))
            },
            TokenKind::Identifier => {
                let id = self.eat(TokenKind::Identifier)?;
                Ok(Expression::Identifier(id))
            },
            _ => {
                return Err(ParseError::UnexpectedToken(vec![TokenKind::Integer, TokenKind::Identifier], first_token));
            }
        }
    }
}

pub fn parse(tokens: &[Token]) -> Result<Program, ParseError> {
    let mut statements = Vec::new();
    let mut parser = Parser::new(&tokens);
    loop {
        let next_token = match parser.peek() {
            Some(t) => t,
            None => break,
        };

        match next_token.kind {
            TokenKind::EOF => break,
            TokenKind::Semicolon => {
                parser.eat(TokenKind::Semicolon)?;
                continue;
            },
            _ => (),
        }
        
        let statement = parser.next_statement()?;
        statements.push(statement);
    }
    Ok(Program::new(&statements))
}

#[cfg(test)]
mod test {
    use crate::lex::lex;
    use crate::lex::{Token, TokenKind};
    use crate::parse::parse;
    use crate::parse::{Statement, Expression};

    #[test]
    fn initial_assignment() {
        let valid_src = r#"
            let abc = 123;
            let xyz = abc;
        "#;

        let tokens = lex(&valid_src);
        let parsed = parse(&tokens);
        
        assert!(parsed.is_ok(), "could not parse program: {}", parsed.unwrap_err());
        let program = parsed.unwrap();

        assert_eq!(2, program.statements.len());
        let mut statements = (&program.statements).into_iter();
        
        let first_expected = Statement::InitialAssignment(
            Token::basic("let", TokenKind::Let), 
            Expression::Identifier(Token::basic("abc", TokenKind::Identifier)),
            Expression::Integer(Token::basic("123", TokenKind::Integer)),
        );
        let first_actual = statements.next().unwrap();
        assert!(first_actual.is_equivalent_to(&first_expected));
        
        let second_expected = Statement::InitialAssignment(
            Token::basic("let", TokenKind::Let), 
            Expression::Identifier(Token::basic("xyz", TokenKind::Identifier)),
            Expression::Identifier(Token::basic("abc", TokenKind::Identifier)),
        );
        let second_actual = statements.next().unwrap();
        assert!(second_actual.is_equivalent_to(&second_expected));
    }

}
