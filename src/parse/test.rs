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
        Factor::Identifier(Identifier(Token::basic("abc", TokenKind::Identifier))).to_expression(),
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
                        Factor::Integer(Token::basic("2", TokenKind::Integer), 2).to_expression(),
                        Factor::Integer(Token::basic("3", TokenKind::Integer), 3).to_expression(),
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
    let mut expected =
        ParseError::UnexpectedToken(vec![TokenKind::Semicolon], Token::basic("", TokenKind::EOF));
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
        Factor::Identifier(Identifier(Token::basic("abc", TokenKind::Identifier))).to_expression(),
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
            Box::new(Factor::Integer(Token::basic("2", TokenKind::Integer), 2)),
            TermBinOp::Multiply,
            Box::new(Factor::Identifier(Identifier(Token::basic(
                "b",
                TokenKind::Identifier,
            )))),
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
            Box::new(Factor::Identifier(Identifier(Token::basic(
                "b",
                TokenKind::Identifier,
            )))),
            TermBinOp::Divide,
            Box::new(Factor::Integer(Token::basic("2", TokenKind::Integer), 2)),
        )
        .to_expression(),
    );
    assert!(actual.is_equivalent_to(&expected));
}

#[test]
fn repeated_term() {
    let src = "let a = 2 * 2 * 2;";
    let actual = setup(&src, 1).statements.into_iter().next().unwrap();
    let expected = Statement::Assignment(
        Token::basic("let", TokenKind::Let),
        Identifier(Token::basic("a", TokenKind::Identifier)),
        Term::Term(
            Box::new(Factor::Wrapped(
                Term::Term(
                    Box::new(Factor::Integer(Token::basic("2", TokenKind::Integer), 2)),
                    TermBinOp::Multiply,
                    Box::new(Factor::Integer(Token::basic("2", TokenKind::Integer), 2)),
                )
                .to_expression(),
            )),
            TermBinOp::Multiply,
            Box::new(Factor::Integer(Token::basic("2", TokenKind::Integer), 2)),
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
            Box::new(Term::Wrapped(Factor::Integer(
                Token::basic("2", TokenKind::Integer),
                2,
            ))),
            AdditiveBinOp::Add,
            Box::new(Term::Wrapped(Factor::Identifier(Identifier(Token::basic(
                "b",
                TokenKind::Identifier,
            ))))),
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
            Box::new(Term::Wrapped(Factor::Integer(
                Token::basic("2", TokenKind::Integer),
                2,
            ))),
            AdditiveBinOp::Subtract,
            Box::new(Term::Wrapped(Factor::Identifier(Identifier(Token::basic(
                "b",
                TokenKind::Identifier,
            ))))),
        )
        .to_expression(),
    );
    assert!(actual.is_equivalent_to(&expected));
}

#[test]
fn equal() {
    let src = "return a == b;";
    let actual = setup(&src, 1).statements.into_iter().next().unwrap();
    let lhs = RelationalExpr::Wrapped(AdditiveExpr::Wrapped(Term::Wrapped(Factor::Identifier(
        Identifier(Token::basic("a", TokenKind::Identifier)),
    ))));
    let rhs = RelationalExpr::Wrapped(AdditiveExpr::Wrapped(Term::Wrapped(Factor::Identifier(
        Identifier(Token::basic("b", TokenKind::Identifier)),
    ))));
    let expected = Statement::Return(
        Token::basic("return", TokenKind::Return),
        Box::new(EqualityExpr::EqualityExpr(
            Box::new(lhs),
            EqualityBinOp::Equal,
            Box::new(rhs),
        )),
    );
    assert!(actual.is_equivalent_to(&expected));
}

#[test]
fn not_equal() {
    let src = "return a != b;";
    let actual = setup(&src, 1).statements.into_iter().next().unwrap();
    let lhs = RelationalExpr::Wrapped(AdditiveExpr::Wrapped(Term::Wrapped(Factor::Identifier(
        Identifier(Token::basic("a", TokenKind::Identifier)),
    ))));
    let rhs = RelationalExpr::Wrapped(AdditiveExpr::Wrapped(Term::Wrapped(Factor::Identifier(
        Identifier(Token::basic("b", TokenKind::Identifier)),
    ))));
    let expected = Statement::Return(
        Token::basic("return", TokenKind::Return),
        Box::new(EqualityExpr::EqualityExpr(
            Box::new(lhs),
            EqualityBinOp::NotEqual,
            Box::new(rhs),
        )),
    );
    assert!(actual.is_equivalent_to(&expected));
}

#[test]
fn less_than() {
    let src = "return a < b;";
    let actual = setup(&src, 1).statements.into_iter().next().unwrap();
    let lhs = AdditiveExpr::Wrapped(Term::Wrapped(Factor::Identifier(
        Identifier(Token::basic("a", TokenKind::Identifier)),
    )));
    let rhs = AdditiveExpr::Wrapped(Term::Wrapped(Factor::Identifier(
        Identifier(Token::basic("b", TokenKind::Identifier)),
    )));
    let expected = Statement::Return(
        Token::basic("return", TokenKind::Return),
        Box::new(EqualityExpr::Wrapped(RelationalExpr::RelationalExpr(
            Box::new(lhs),
            RelationalBinOp::LessThan,
            Box::new(rhs),
        ))),
    );
    assert!(actual.is_equivalent_to(&expected));
}

#[test]
fn less_than_or_equal() {
    let src = "return a <= b;";
    let actual = setup(&src, 1).statements.into_iter().next().unwrap();
    let lhs = AdditiveExpr::Wrapped(Term::Wrapped(Factor::Identifier(
        Identifier(Token::basic("a", TokenKind::Identifier)),
    )));
    let rhs = AdditiveExpr::Wrapped(Term::Wrapped(Factor::Identifier(
        Identifier(Token::basic("b", TokenKind::Identifier)),
    )));
    let expected = Statement::Return(
        Token::basic("return", TokenKind::Return),
        Box::new(EqualityExpr::Wrapped(RelationalExpr::RelationalExpr(
            Box::new(lhs),
            RelationalBinOp::LessThanEqual,
            Box::new(rhs),
        ))),
    );
    assert!(actual.is_equivalent_to(&expected));
}

#[test]
fn greater_than() {
    let src = "return a > b;";
    let actual = setup(&src, 1).statements.into_iter().next().unwrap();
    let lhs = AdditiveExpr::Wrapped(Term::Wrapped(Factor::Identifier(
        Identifier(Token::basic("a", TokenKind::Identifier)),
    )));
    let rhs = AdditiveExpr::Wrapped(Term::Wrapped(Factor::Identifier(
        Identifier(Token::basic("b", TokenKind::Identifier)),
    )));
    let expected = Statement::Return(
        Token::basic("return", TokenKind::Return),
        Box::new(EqualityExpr::Wrapped(RelationalExpr::RelationalExpr(
            Box::new(lhs),
            RelationalBinOp::GreaterThan,
            Box::new(rhs),
        ))),
    );
    assert!(actual.is_equivalent_to(&expected));
}

#[test]
fn greater_than_or_equal() {
    let src = "return a >= b;";
    let actual = setup(&src, 1).statements.into_iter().next().unwrap();
    let lhs = AdditiveExpr::Wrapped(Term::Wrapped(Factor::Identifier(
        Identifier(Token::basic("a", TokenKind::Identifier)),
    )));
    let rhs = AdditiveExpr::Wrapped(Term::Wrapped(Factor::Identifier(
        Identifier(Token::basic("b", TokenKind::Identifier)),
    )));
    let expected = Statement::Return(
        Token::basic("return", TokenKind::Return),
        Box::new(EqualityExpr::Wrapped(RelationalExpr::RelationalExpr(
            Box::new(lhs),
            RelationalBinOp::GreaterThanEqual,
            Box::new(rhs),
        ))),
    );
    assert!(actual.is_equivalent_to(&expected));
}
