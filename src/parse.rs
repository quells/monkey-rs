use std::str::FromStr;

use crate::lex::{Token, TokenKind};

pub trait Node {
    fn token(&self) -> Option<Token>;
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
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

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let msg = match self {
            Statement::InitialAssignment(_, id, value) => {
                format!("let {} = {};", id, value)
            },
        };
        write!(f, "{}", msg)
    }
}

#[derive(Clone, Debug)]
pub enum Expression {
    Identifier(Token),
    Integer(Token, isize),
    FunctionCall(Token, Vec<Expression>),
}

impl Expression {
    fn is_equivalent_to(&self, other: &Expression) -> bool {
        match (self, other) {
            (Expression::Identifier(l), Expression::Identifier(r)) => l.is_equivalent_to(r),
            (Expression::Integer(l, _), Expression::Integer(r, _)) => l.is_equivalent_to(r),
            (Expression::FunctionCall(id_l, params_l), Expression::FunctionCall(id_r, params_r)) => {
                if params_l.len() != params_r.len() {
                    return false;
                }
                let param_acc = params_l.into_iter()
                    .zip(params_r)
                    .map(|(l, r)| l.is_equivalent_to(r))
                    .fold(true, |a, b| a && b);
                id_l.is_equivalent_to(id_r) && param_acc
            }
            _ => false,
        }
    }
}

impl Node for Expression {
    fn token(&self) -> Option<Token> {
        let t = match self {
            Expression::Identifier(t) => t.clone(),
            Expression::Integer(t, _) => t.clone(),
            Expression::FunctionCall(t, _) => t.clone(),
        };
        Some(t)
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let msg: String = match self {
            Expression::Identifier(t) => t.literal.clone(),
            Expression::Integer(t, _) => t.literal.clone(),
            Expression::FunctionCall(id, params) => {
                let param_strs = match params.split_first() {
                    Some((first, rest)) => {
                        rest.into_iter()
                            .fold(format!("{}", first), |a, b| format!("{}, {}", a, b))
                    },
                    None => "".to_owned(),
                };
                format!("{}({})", id.literal, param_strs)
            },
        };
        write!(f, "{}", msg)
    }
}

use std::error::Error;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Vec<TokenKind>, Token),
    UnexpectedEndOfTokens,
    IllegalToken(Token),
    InvalidInteger(Token),
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
            },
            ParseError::InvalidInteger(t) => {
                format!("unable to parse `{}` as integer", t.literal)
            },
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
                let parsed = match isize::from_str(&int.literal) {
                    Ok(x) => x,
                    Err(_) => return Err(ParseError::InvalidInteger(int)),
                };
                Ok(Expression::Integer(int, parsed))
            },
            TokenKind::Identifier => {
                let id = self.eat(TokenKind::Identifier)?;
                let default = Ok(Expression::Identifier(id.clone()));
                let next_token = match self.peek() {
                    Some(t) => t,
                    None => return default,
                };
                match next_token.kind {
                    TokenKind::LParen => {
                        self.eat(TokenKind::LParen)?;
                        let mut params = Vec::new();
                        loop {
                            let maybe_close = match self.peek() {
                                Some(t) => t,
                                None => return Err(ParseError::UnexpectedEndOfTokens),
                            };
                            if maybe_close.kind == TokenKind::RParen {
                                self.eat(TokenKind::RParen)?;
                                break;
                            }

                            if params.len() > 0 {
                                self.eat(TokenKind::Comma)?;
                            }
                            let expr = self.next_expression()?;
                            params.push(expr);
                        }
                        Ok(Expression::FunctionCall(id, params))
                    },
                    _ => default,
                }
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
    use crate::parse::{Program, Statement, Expression};

    fn setup(src: &str, expected_statement_count: usize) -> Program {
        let tokens = lex(&src);
        let parsed = parse(&tokens);

        assert!(parsed.is_ok(), "could not parse program: {}", parsed.unwrap_err());
        let program = parsed.unwrap();

        assert_eq!(expected_statement_count, program.statements.len());
        program
    }

    #[test]
    fn assign_int() {
        let src = "let abc = 123;";
        let actual = setup(&src, 1).statements.into_iter().next().unwrap();

        let expected = Statement::InitialAssignment(
            Token::basic("let", TokenKind::Let), 
            Expression::Identifier(Token::basic("abc", TokenKind::Identifier)),
            Expression::Integer(Token::basic("123", TokenKind::Integer), 123),
        );

        assert!(actual.is_equivalent_to(&expected));
    }

    #[test]
    fn assign_identifier() {
        let src = "let xyz = abc;";
        let actual = setup(&src, 1).statements.into_iter().next().unwrap();

        let expected = Statement::InitialAssignment(
            Token::basic("let", TokenKind::Let), 
            Expression::Identifier(Token::basic("xyz", TokenKind::Identifier)),
            Expression::Identifier(Token::basic("abc", TokenKind::Identifier)),
        );

        assert!(actual.is_equivalent_to(&expected));
    }

    #[test]
    fn assign_fn_call() {
        let src = "let sum = add(1, abc);";
        let actual = setup(&src, 1).statements.into_iter().next().unwrap();

        let expected = Statement::InitialAssignment(
            Token::basic("let", TokenKind::Let),
            Expression::Identifier(Token::basic("sum", TokenKind::Identifier)),
            Expression::FunctionCall(
                Token::basic("add", TokenKind::Identifier),
                vec![
                    Expression::Integer(Token::basic("1", TokenKind::Integer), 0),
                    Expression::Identifier(Token::basic("abc", TokenKind::Identifier)),
                ]
            )
        );

        assert!(actual.is_equivalent_to(&expected));
    }

    #[test]
    fn assign_nested_fn_call() {
        let src = "let sum = add(1, add(2, 3));";
        let actual = setup(&src, 1).statements.into_iter().next().unwrap();

        let expected = Statement::InitialAssignment(
            Token::basic("let", TokenKind::Let),
            Expression::Identifier(Token::basic("sum", TokenKind::Identifier)),
            Expression::FunctionCall(
                Token::basic("add", TokenKind::Identifier),
                vec![
                    Expression::Integer(Token::basic("1", TokenKind::Integer), 0),
                    Expression::FunctionCall(
                        Token::basic("add", TokenKind::Identifier),
                        vec![
                            Expression::Integer(Token::basic("2", TokenKind::Integer), 0),
                            Expression::Integer(Token::basic("3", TokenKind::Integer), 0),
                        ]
                    )
                ]
            )
        );

        assert!(actual.is_equivalent_to(&expected));
    }
}
