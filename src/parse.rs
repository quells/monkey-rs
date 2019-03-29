use std::collections::HashSet;
use std::str::FromStr;

use crate::lex::{Token, TokenKind};

trait EquivalentTo {
    fn is_equivalent_to(&self, other: &Self) -> bool;
}

pub trait ToExpression {
    fn to_expression(&self) -> Expression;
}

pub trait ToChild<T> {
    fn to_child(&self) -> T;
}

pub trait AsParent<T> {
    fn as_parent(&self) -> Option<T>;
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

#[derive(Clone, Debug)]
pub enum Statement {
    Assignment(Token, Identifier, Expression), // let `a` = `b`;
    Return(Token, Expression),                 // return `a`;
}

impl EquivalentTo for Statement {
    fn is_equivalent_to(&self, other: &Statement) -> bool {
        match (self, other) {
            (
                Statement::Assignment(let_l, id_l, expr_l),
                Statement::Assignment(let_r, id_r, expr_r),
            ) => {
                let_l.is_equivalent_to(let_r)
                    && id_l.is_equivalent_to(id_r)
                    && expr_l.is_equivalent_to(expr_r)
            }
            (Statement::Return(return_l, value_l), Statement::Return(return_r, value_r)) => {
                return_l.is_equivalent_to(return_r) && value_l.is_equivalent_to(value_r)
            }
            _ => false,
        }
    }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let msg = match self {
            Statement::Assignment(_, id, value) => format!("let {} = {};", id, value),
            Statement::Return(_, value) => format!("return {};", value),
        };
        write!(f, "{}", msg)
    }
}

macro_rules! impl_equivalent_to_binop {
    ($this:ident) => {
        impl EquivalentTo for $this {
            fn is_equivalent_to(&self, other: &Self) -> bool {
                match (self, other) {
                    ($this::Wrapped(l), $this::Wrapped(r)) => l.is_equivalent_to(r),
                    ($this::$this(ll, lop, lr), $this::$this(rl, rop, rr)) => {
                        lop == rop && ll.is_equivalent_to(rl) && lr.is_equivalent_to(rr)
                    }
                    ($this::Wrapped(l), $this::$this(_, _, _)) => match l.as_parent() {
                        Some(l) => l.is_equivalent_to(other),
                        None => false,
                    },
                    ($this::$this(_, _, _), $this::Wrapped(r)) => match r.as_parent() {
                        Some(r) => r.is_equivalent_to(self),
                        None => false,
                    },
                }
            }
        }
    };
}

macro_rules! impl_display_binop_node {
    ($this:ident, $child:ident) => {
        impl std::fmt::Display for $this {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match self {
                    $this::Wrapped(w) => w.fmt(f),
                    $this::$child(l, op, r) => write!(f, "{} {} {}", l, op, r),
                }
            }
        }
    };
}

macro_rules! impl_display_binop {
    ($this:ident; $( $op:tt, $literal:expr );+ ) => {
        impl std::fmt::Display for $this {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match self {
                    $(
                        $this::$op => write!(f, "{}", $literal)
                    ),*
                }
            }
        }
    }
}

macro_rules! impl_next_binop {
    ($fn:tt; $this:ident; $next:tt; $opkind:tt; $( $token:tt, $op:tt );+ ) => {
        fn $fn(&mut self, first_token: Token) -> Result<$this, ParseError> {
            // left associative
            let mut lhs = self.$next(first_token)?;

            loop {
                let op_token = match self.peek() {
                    Some(t) => t,
                    None => return Err(ParseError::UnexpectedEndOfTokens),
                };
                let op = match op_token.kind {
                    $(
                        TokenKind::$token => $opkind::$op
                    ),*,
                    _ => break,
                };
                self.eat(op_token.kind)?;

                let next_token = match self.peek() {
                    Some(t) => t,
                    None => return Err(ParseError::UnexpectedEndOfTokens),
                };
                let rhs = self.$next(next_token)?;

                lhs = $this::$this(lhs.clone(), op, rhs).to_child();
            }

            Ok($this::Wrapped(lhs))
        }
    }
}

macro_rules! wrapped_or_return_none {
    ($matcher:tt, $this:ident) => {
        match $matcher {
            $this::Wrapped(w) => w,
            _ => return None,
        }
    };
}

type Expression = Box<EqualityExpr>;

#[derive(Clone, Debug)]
pub enum EqualityExpr {
    Wrapped(RelationalExpr),
    EqualityExpr(RelationalExpr, EqualityBinOp, RelationalExpr),
}

#[derive(Clone, Debug, PartialEq)]
pub enum EqualityBinOp {
    Equal,
    NotEqual,
}

impl_equivalent_to_binop!(EqualityExpr);
impl_display_binop_node!(EqualityExpr, EqualityExpr);
impl_display_binop!(
    EqualityBinOp;
    Equal, "==";
    NotEqual, "!="
);

impl AsParent<Factor> for EqualityExpr {
    fn as_parent(&self) -> Option<Factor> {
        let relational = wrapped_or_return_none!(self, EqualityExpr);
        let additive = wrapped_or_return_none!(relational, RelationalExpr);
        let term = wrapped_or_return_none!(additive, AdditiveExpr);
        let factor = wrapped_or_return_none!(term, Term);
        match factor {
            Factor::Wrapped(e) => e.as_parent(),
            _ => Some(factor.clone()),
        }
    }
}

impl ToExpression for EqualityExpr {
    fn to_expression(&self) -> Expression {
        Box::new(self.clone())
    }
}

impl ToChild<RelationalExpr> for EqualityExpr {
    fn to_child(&self) -> RelationalExpr {
        RelationalExpr::Wrapped(AdditiveExpr::Wrapped(Term::Wrapped(Factor::Wrapped(
            self.to_expression(),
        ))))
    }
}

#[derive(Clone, Debug)]
pub enum RelationalExpr {
    Wrapped(AdditiveExpr),
    RelationalExpr(AdditiveExpr, RelationalBinOp, AdditiveExpr),
}

#[derive(Clone, Debug, PartialEq)]
pub enum RelationalBinOp {
    LessThan,
    LessThanEqual,
    GreaterThanEqual,
    GreaterThan,
}

impl_equivalent_to_binop!(RelationalExpr);
impl_display_binop_node!(RelationalExpr, RelationalExpr);
impl_display_binop!(
    RelationalBinOp;
    LessThan, "<";
    LessThanEqual, "<=";
    GreaterThan, ">";
    GreaterThanEqual, ">="
);

impl AsParent<EqualityExpr> for RelationalExpr {
    fn as_parent(&self) -> Option<EqualityExpr> {
        let additive = wrapped_or_return_none!(self, RelationalExpr);
        let term = wrapped_or_return_none!(additive, AdditiveExpr);
        let factor = wrapped_or_return_none!(term, Term);
        let equality = wrapped_or_return_none!(factor, Factor);
        let unboxed = *(*equality).clone();
        Some(unboxed)
    }
}

impl ToExpression for RelationalExpr {
    fn to_expression(&self) -> Expression {
        Box::new(EqualityExpr::Wrapped(self.clone()))
    }
}

impl ToChild<AdditiveExpr> for RelationalExpr {
    fn to_child(&self) -> AdditiveExpr {
        AdditiveExpr::Wrapped(Term::Wrapped(Factor::Wrapped(self.to_expression())))
    }
}

#[derive(Clone, Debug)]
pub enum AdditiveExpr {
    Wrapped(Term),
    AdditiveExpr(Term, AdditiveBinOp, Term),
}

#[derive(Clone, Debug, PartialEq)]
pub enum AdditiveBinOp {
    Add,
    Subtract,
}

impl_equivalent_to_binop!(AdditiveExpr);
impl_display_binop_node!(AdditiveExpr, AdditiveExpr);
impl_display_binop!(
    AdditiveBinOp;
    Add, "+";
    Subtract, "-"
);

impl AsParent<RelationalExpr> for AdditiveExpr {
    fn as_parent(&self) -> Option<RelationalExpr> {
        let term = wrapped_or_return_none!(self, AdditiveExpr);
        let factor = wrapped_or_return_none!(term, Term);
        let equality = wrapped_or_return_none!(factor, Factor);
        let unboxed = *(*equality).clone();
        let relational = wrapped_or_return_none!(unboxed, EqualityExpr);
        Some(relational)
    }
}

impl ToExpression for AdditiveExpr {
    fn to_expression(&self) -> Expression {
        Box::new(EqualityExpr::Wrapped(RelationalExpr::Wrapped(self.clone())))
    }
}

impl ToChild<Term> for AdditiveExpr {
    fn to_child(&self) -> Term {
        Term::Wrapped(Factor::Wrapped(self.to_expression()))
    }
}

#[derive(Clone, Debug)]
pub enum Term {
    Wrapped(Factor),
    Term(Factor, TermBinOp, Factor),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TermBinOp {
    Multiply,
    Divide,
}

impl_equivalent_to_binop!(Term);
impl_display_binop_node!(Term, Term);
impl_display_binop!(
    TermBinOp;
    Multiply, "*";
    Divide, "/"
);

impl AsParent<AdditiveExpr> for Term {
    fn as_parent(&self) -> Option<AdditiveExpr> {
        let factor = wrapped_or_return_none!(self, Term);
        let equality = wrapped_or_return_none!(factor, Factor);
        let unboxed = *(*equality).clone();
        let relational = wrapped_or_return_none!(unboxed, EqualityExpr);
        let additive = wrapped_or_return_none!(relational, RelationalExpr);
        Some(additive)
    }
}

impl ToExpression for Term {
    fn to_expression(&self) -> Expression {
        Box::new(EqualityExpr::Wrapped(RelationalExpr::Wrapped(
            AdditiveExpr::Wrapped(self.clone()),
        )))
    }
}

impl ToChild<Factor> for Term {
    fn to_child(&self) -> Factor {
        Factor::Wrapped(self.to_expression())
    }
}

#[derive(Clone, Debug)]
pub enum Factor {
    Wrapped(Expression),
    Identifier(Identifier),
    Integer(Token, isize),
    FunctionCall(Token, Vec<Expression>),
}

impl AsParent<Term> for Factor {
    fn as_parent(&self) -> Option<Term> {
        let equality = wrapped_or_return_none!(self, Factor);
        let unboxed = *(*equality).clone();
        let relational = wrapped_or_return_none!(unboxed, EqualityExpr);
        let additive = wrapped_or_return_none!(relational, RelationalExpr);
        let term = wrapped_or_return_none!(additive, AdditiveExpr);
        Some(term)
    }
}

impl EquivalentTo for Factor {
    fn is_equivalent_to(&self, other: &Factor) -> bool {
        match (self, other) {
            (Factor::Wrapped(l), Factor::Wrapped(r)) => l.is_equivalent_to(r),
            (Factor::Identifier(l), Factor::Identifier(r)) => l.is_equivalent_to(r),
            (Factor::Integer(literal_l, parsed_l), Factor::Integer(literal_r, parsed_r)) => {
                literal_l.is_equivalent_to(literal_r) && parsed_l == parsed_r
            }
            (Factor::FunctionCall(id_l, params_l), Factor::FunctionCall(id_r, params_r)) => {
                if params_l.len() != params_r.len() {
                    return false;
                }
                let param_acc = params_l
                    .iter()
                    .zip(params_r)
                    .map(|(l, r)| l.is_equivalent_to(r))
                    .all(|b| b);
                id_l.is_equivalent_to(id_r) && param_acc
            }
            (Factor::Wrapped(l), _) => match l.as_parent() {
                Some(l) => l.is_equivalent_to(other),
                None => false,
            },
            (_, Factor::Wrapped(r)) => match r.as_parent() {
                Some(r) => r.is_equivalent_to(self),
                None => false,
            },
            _ => false,
        }
    }
}

impl ToExpression for Factor {
    fn to_expression(&self) -> Expression {
        let wrapped = |v: &Self| {
            Box::new(EqualityExpr::Wrapped(RelationalExpr::Wrapped(
                AdditiveExpr::Wrapped(Term::Wrapped(v.clone())),
            )))
        };

        match self {
            Factor::Wrapped(e) => e.clone(),
            Factor::Identifier(_) => wrapped(self),
            Factor::Integer(_, _) => wrapped(self),
            Factor::FunctionCall(_, _) => wrapped(self),
        }
    }
}

impl std::fmt::Display for Factor {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let msg: String = match self {
            Factor::Wrapped(e) => format!("{}", e),
            Factor::Identifier(t) => t.0.literal.clone(),
            Factor::Integer(t, _) => t.literal.clone(),
            Factor::FunctionCall(id, params) => {
                let param_strs = match params.split_first() {
                    Some((first, rest)) => rest
                        .iter()
                        .fold(format!("{}", first), |a, b| format!("{}, {}", a, b)),
                    None => "".to_owned(),
                };
                format!("{}({})", id.literal, param_strs)
            }
        };
        write!(f, "{}", msg)
    }
}

#[derive(Clone, Debug)]
pub struct Identifier(Token);

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0.literal)
    }
}

impl EquivalentTo for Identifier {
    fn is_equivalent_to(&self, other: &Self) -> bool {
        self.0.is_equivalent_to(&other.0)
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

#[cfg(test)]
mod test {
    use crate::lex::lex;
    use crate::lex::{Token, TokenKind};
    use crate::parse::parse;
    use crate::parse::*;

    fn setup(src: &str, expected_statement_count: usize) -> Program {
        let tokens = lex(&src);
        let parsed = parse(&tokens);

        assert!(
            parsed.is_ok(),
            "could not parse program: {}",
            parsed.unwrap_err()
        );
        let program = parsed.unwrap();

        assert_eq!(expected_statement_count, program.statements.len());
        program
    }

    #[test]
    fn assign_int() {
        let src = "let abc = 123;";
        let actual = setup(&src, 1).statements.into_iter().next().unwrap();

        let expected = Statement::Assignment(
            Token::basic("let", TokenKind::Let),
            Identifier(Token::basic("abc", TokenKind::Identifier)),
            Factor::Integer(Token::basic("123", TokenKind::Integer), 123).to_expression(),
        );

        assert!(actual.is_equivalent_to(&expected));
    }

    #[test]
    fn assign_identifier() {
        let src = "let xyz = abc;";
        let actual = setup(&src, 1).statements.into_iter().next().unwrap();

        let expected = Statement::Assignment(
            Token::basic("let", TokenKind::Let),
            Identifier(Token::basic("xyz", TokenKind::Identifier)),
            Factor::Identifier(Identifier(Token::basic("abc", TokenKind::Identifier)))
                .to_expression(),
        );

        assert!(actual.is_equivalent_to(&expected));
    }

    #[test]
    fn assign_fn_call() {
        let src = "let sum = add(1, abc);";
        let actual = setup(&src, 1).statements.into_iter().next().unwrap();

        let expected = Statement::Assignment(
            Token::basic("let", TokenKind::Let),
            Identifier(Token::basic("sum", TokenKind::Identifier)),
            Factor::FunctionCall(
                Token::basic("add", TokenKind::Identifier),
                vec![
                    Factor::Integer(Token::basic("1", TokenKind::Integer), 1).to_expression(),
                    Factor::Identifier(Identifier(Token::basic("abc", TokenKind::Identifier)))
                        .to_expression(),
                ],
            )
            .to_expression(),
        );

        assert!(actual.is_equivalent_to(&expected));
    }

    #[test]
    fn assign_nested_fn_call() {
        let src = "let sum = add(1, add(2, 3));";
        let actual = setup(&src, 1).statements.into_iter().next().unwrap();

        let expected = Statement::Assignment(
            Token::basic("let", TokenKind::Let),
            Identifier(Token::basic("sum", TokenKind::Identifier)),
            Factor::FunctionCall(
                Token::basic("add", TokenKind::Identifier),
                vec![
                    Factor::Integer(Token::basic("1", TokenKind::Integer), 1).to_expression(),
                    Factor::FunctionCall(
                        Token::basic("add", TokenKind::Identifier),
                        vec![
                            Factor::Integer(Token::basic("2", TokenKind::Integer), 2)
                                .to_expression(),
                            Factor::Integer(Token::basic("3", TokenKind::Integer), 3)
                                .to_expression(),
                        ],
                    )
                    .to_expression(),
                ],
            )
            .to_expression(),
        );

        assert!(actual.is_equivalent_to(&expected));
    }

    #[test]
    fn assign_missing_token() {
        let mut src = "let abc = 123";
        let mut tokens = lex(&src);
        let mut parsed = parse(&tokens);

        assert!(parsed.is_err());
        let mut actual = parsed.err().unwrap();
        let mut expected = ParseError::UnexpectedToken(
            vec![TokenKind::Semicolon],
            Token::basic("", TokenKind::EOF),
        );
        assert!(actual.is_equivalent_to(&expected));

        src = "let abc 123;";
        tokens = lex(&src);
        parsed = parse(&tokens);

        assert!(parsed.is_err());
        actual = parsed.err().unwrap();
        expected = ParseError::UnexpectedToken(
            vec![TokenKind::Assign],
            Token::basic("123", TokenKind::Integer),
        );
        assert!(actual.is_equivalent_to(&expected));

        src = "let abc =;";
        tokens = lex(&src);
        parsed = parse(&tokens);

        assert!(parsed.is_err());
        actual = parsed.err().unwrap();
        expected = ParseError::UnexpectedToken(
            vec![TokenKind::Integer, TokenKind::Identifier],
            Token::basic(";", TokenKind::Semicolon),
        );
        println!("{:?}", actual);
        assert!(actual.is_equivalent_to(&expected));

        src = "let = 123;";
        tokens = lex(&src);
        parsed = parse(&tokens);

        assert!(parsed.is_err());
        actual = parsed.err().unwrap();
        expected = ParseError::UnexpectedToken(
            vec![TokenKind::Identifier],
            Token::basic("=", TokenKind::Assign),
        );
        assert!(actual.is_equivalent_to(&expected));
    }

    #[test]
    fn return_statement() {
        let mut src = "return 1;";
        let mut actual = setup(&src, 1).statements.into_iter().next().unwrap();
        let mut expected = Statement::Return(
            Token::basic("return", TokenKind::Return),
            Factor::Integer(Token::basic("1", TokenKind::Integer), 1).to_expression(),
        );
        assert!(actual.is_equivalent_to(&expected));

        src = "return abc;";
        actual = setup(&src, 1).statements.into_iter().next().unwrap();
        expected = Statement::Return(
            Token::basic("return", TokenKind::Return),
            Factor::Identifier(Identifier(Token::basic("abc", TokenKind::Identifier)))
                .to_expression(),
        );
        assert!(actual.is_equivalent_to(&expected));

        src = "return add(2, 3);";
        actual = setup(&src, 1).statements.into_iter().next().unwrap();
        expected = Statement::Return(
            Token::basic("return", TokenKind::Return),
            Factor::FunctionCall(
                Token::basic("add", TokenKind::Identifier),
                vec![
                    Factor::Integer(Token::basic("2", TokenKind::Integer), 2).to_expression(),
                    Factor::Integer(Token::basic("3", TokenKind::Integer), 3).to_expression(),
                ],
            )
            .to_expression(),
        );
        assert!(actual.is_equivalent_to(&expected));
    }

    #[test]
    fn multiplication() {
        let src = "let a = 2 * b;";
        let actual = setup(&src, 1).statements.into_iter().next().unwrap();
        let expected = Statement::Assignment(
            Token::basic("let", TokenKind::Let),
            Identifier(Token::basic("a", TokenKind::Identifier)),
            Term::Term(
                Factor::Integer(Token::basic("2", TokenKind::Integer), 2),
                TermBinOp::Multiply,
                Factor::Identifier(Identifier(Token::basic("b", TokenKind::Identifier))),
            )
            .to_expression(),
        );
        assert!(actual.is_equivalent_to(&expected));
    }

    #[test]
    fn division() {
        let src = "let a = b / 2;";
        let actual = setup(&src, 1).statements.into_iter().next().unwrap();
        let expected = Statement::Assignment(
            Token::basic("let", TokenKind::Let),
            Identifier(Token::basic("a", TokenKind::Identifier)),
            Term::Term(
                Factor::Identifier(Identifier(Token::basic("b", TokenKind::Identifier))),
                TermBinOp::Divide,
                Factor::Integer(Token::basic("2", TokenKind::Integer), 2),
            )
            .to_expression(),
        );
        assert!(actual.is_equivalent_to(&expected));
    }

    #[test]
    fn repeated_term() {
        let mut src = "let a = 2 * 2 * 2;";
        let mut actual = setup(&src, 1).statements.into_iter().next().unwrap();
        let mut expected = Statement::Assignment(
            Token::basic("let", TokenKind::Let),
            Identifier(Token::basic("a", TokenKind::Identifier)),
            Term::Term(
                Factor::Wrapped(
                    Term::Term(
                        Factor::Integer(Token::basic("2", TokenKind::Integer), 2),
                        TermBinOp::Multiply,
                        Factor::Integer(Token::basic("2", TokenKind::Integer), 2),
                    )
                    .to_expression(),
                ),
                TermBinOp::Multiply,
                Factor::Integer(Token::basic("2", TokenKind::Integer), 2),
            )
            .to_expression(),
        );
        assert!(actual.is_equivalent_to(&expected));
    }

    #[test]
    fn addition() {
        let src = "let a = 2 + b;";
        let actual = setup(&src, 1).statements.into_iter().next().unwrap();
        let expected = Statement::Assignment(
            Token::basic("let", TokenKind::Let),
            Identifier(Token::basic("a", TokenKind::Identifier)),
            AdditiveExpr::AdditiveExpr(
                Term::Wrapped(Factor::Integer(Token::basic("2", TokenKind::Integer), 2)),
                AdditiveBinOp::Add,
                Term::Wrapped(Factor::Identifier(Identifier(Token::basic(
                    "b",
                    TokenKind::Identifier,
                )))),
            )
            .to_expression(),
        );
        assert!(actual.is_equivalent_to(&expected));
    }

    #[test]
    fn subtraction() {
        let src = "let a = 2 - b;";
        let actual = setup(&src, 1).statements.into_iter().next().unwrap();
        let expected = Statement::Assignment(
            Token::basic("let", TokenKind::Let),
            Identifier(Token::basic("a", TokenKind::Identifier)),
            AdditiveExpr::AdditiveExpr(
                Term::Wrapped(Factor::Integer(Token::basic("2", TokenKind::Integer), 2)),
                AdditiveBinOp::Subtract,
                Term::Wrapped(Factor::Identifier(Identifier(Token::basic(
                    "b",
                    TokenKind::Identifier,
                )))),
            )
            .to_expression(),
        );
        assert!(actual.is_equivalent_to(&expected));
    }
}
