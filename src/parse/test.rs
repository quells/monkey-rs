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
fn assign_wrapped() {
    let src = "let a = (123);";

    let actual = setup(&src, 1).statements.into_iter().next().unwrap();

    let expected = Statement::Assignment(
        Token::basic("let", TokenKind::Let),
        Identifier(Token::basic("a", TokenKind::Identifier)),
        Factor::Integer(Token::basic("123", TokenKind::Integer), 123).to_expression(),
    );

    let ok = actual.is_equivalent_to(&expected);
    assert!(ok);
}

#[test]
fn assign_nested_wrapped() {
    let src = "let a = ((123));";

    let actual = setup(&src, 1).statements.into_iter().next().unwrap();

    let expected = Statement::Assignment(
        Token::basic("let", TokenKind::Let),
        Identifier(Token::basic("a", TokenKind::Identifier)),
        Factor::Integer(Token::basic("123", TokenKind::Integer), 123).to_expression(),
    );

    let ok = actual.is_equivalent_to(&expected);
    assert!(ok);
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
        vec![
            TokenKind::LParen,
            TokenKind::Integer,
            TokenKind::True,
            TokenKind::False,
            TokenKind::Identifier,
        ],
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
            Box::new(PrefixExpr::Wrapped(Factor::Integer(
                Token::basic("2", TokenKind::Integer),
                2,
            ))),
            TermBinOp::Multiply,
            Box::new(PrefixExpr::Wrapped(Factor::Identifier(Identifier(
                Token::basic("b", TokenKind::Identifier),
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
            Box::new(PrefixExpr::Wrapped(Factor::Identifier(Identifier(
                Token::basic("b", TokenKind::Identifier),
            )))),
            TermBinOp::Divide,
            Box::new(PrefixExpr::Wrapped(Factor::Integer(
                Token::basic("2", TokenKind::Integer),
                2,
            ))),
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
            Box::new(PrefixExpr::Wrapped(Factor::Wrapped(
                Term::Term(
                    Box::new(PrefixExpr::Wrapped(Factor::Integer(
                        Token::basic("2", TokenKind::Integer),
                        2,
                    ))),
                    TermBinOp::Multiply,
                    Box::new(PrefixExpr::Wrapped(Factor::Integer(
                        Token::basic("2", TokenKind::Integer),
                        2,
                    ))),
                )
                .to_expression(),
            ))),
            TermBinOp::Multiply,
            Box::new(PrefixExpr::Wrapped(Factor::Integer(
                Token::basic("2", TokenKind::Integer),
                2,
            ))),
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
            Box::new(Term::Wrapped(PrefixExpr::Wrapped(Factor::Integer(
                Token::basic("2", TokenKind::Integer),
                2,
            )))),
            AdditiveBinOp::Add,
            Box::new(Term::Wrapped(PrefixExpr::Wrapped(Factor::Identifier(
                Identifier(Token::basic("b", TokenKind::Identifier)),
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
            Box::new(Term::Wrapped(PrefixExpr::Wrapped(Factor::Integer(
                Token::basic("2", TokenKind::Integer),
                2,
            )))),
            AdditiveBinOp::Subtract,
            Box::new(Term::Wrapped(PrefixExpr::Wrapped(Factor::Identifier(
                Identifier(Token::basic("b", TokenKind::Identifier)),
            )))),
        )
        .to_expression(),
    );
    assert!(actual.is_equivalent_to(&expected));
}

#[test]
fn equal() {
    let src = "return a == b;";
    let actual = setup(&src, 1).statements.into_iter().next().unwrap();
    let lhs = RelationalExpr::Wrapped(AdditiveExpr::Wrapped(Term::Wrapped(PrefixExpr::Wrapped(
        Factor::Identifier(Identifier(Token::basic("a", TokenKind::Identifier))),
    ))));
    let rhs = RelationalExpr::Wrapped(AdditiveExpr::Wrapped(Term::Wrapped(PrefixExpr::Wrapped(
        Factor::Identifier(Identifier(Token::basic("b", TokenKind::Identifier))),
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
    let lhs = RelationalExpr::Wrapped(AdditiveExpr::Wrapped(Term::Wrapped(PrefixExpr::Wrapped(
        Factor::Identifier(Identifier(Token::basic("a", TokenKind::Identifier))),
    ))));
    let rhs = RelationalExpr::Wrapped(AdditiveExpr::Wrapped(Term::Wrapped(PrefixExpr::Wrapped(
        Factor::Identifier(Identifier(Token::basic("b", TokenKind::Identifier))),
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
    let lhs = AdditiveExpr::Wrapped(Term::Wrapped(PrefixExpr::Wrapped(Factor::Identifier(
        Identifier(Token::basic("a", TokenKind::Identifier)),
    ))));
    let rhs = AdditiveExpr::Wrapped(Term::Wrapped(PrefixExpr::Wrapped(Factor::Identifier(
        Identifier(Token::basic("b", TokenKind::Identifier)),
    ))));
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
    let lhs = AdditiveExpr::Wrapped(Term::Wrapped(PrefixExpr::Wrapped(Factor::Identifier(
        Identifier(Token::basic("a", TokenKind::Identifier)),
    ))));
    let rhs = AdditiveExpr::Wrapped(Term::Wrapped(PrefixExpr::Wrapped(Factor::Identifier(
        Identifier(Token::basic("b", TokenKind::Identifier)),
    ))));
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
    let lhs = AdditiveExpr::Wrapped(Term::Wrapped(PrefixExpr::Wrapped(Factor::Identifier(
        Identifier(Token::basic("a", TokenKind::Identifier)),
    ))));
    let rhs = AdditiveExpr::Wrapped(Term::Wrapped(PrefixExpr::Wrapped(Factor::Identifier(
        Identifier(Token::basic("b", TokenKind::Identifier)),
    ))));
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
    let lhs = AdditiveExpr::Wrapped(Term::Wrapped(PrefixExpr::Wrapped(Factor::Identifier(
        Identifier(Token::basic("a", TokenKind::Identifier)),
    ))));
    let rhs = AdditiveExpr::Wrapped(Term::Wrapped(PrefixExpr::Wrapped(Factor::Identifier(
        Identifier(Token::basic("b", TokenKind::Identifier)),
    ))));
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

#[test]
fn inversion() {
    let src = "return !a;";
    let actual = setup(&src, 1).statements.into_iter().next().unwrap();
    let expected = Statement::Return(
        Token::basic("return", TokenKind::Return),
        Box::new(EqualityExpr::Wrapped(RelationalExpr::Wrapped(
            AdditiveExpr::Wrapped(Term::Wrapped(PrefixExpr::Invert(Factor::Identifier(
                Identifier(Token::basic("a", TokenKind::Identifier)),
            )))),
        ))),
    );
    assert!(actual.is_equivalent_to(&expected));
}

#[test]
fn negation() {
    let src = "return -1;";
    let actual = setup(&src, 1).statements.into_iter().next().unwrap();
    let expected = Statement::Return(
        Token::basic("return", TokenKind::Return),
        Box::new(EqualityExpr::Wrapped(RelationalExpr::Wrapped(
            AdditiveExpr::Wrapped(Term::Wrapped(PrefixExpr::Negate(Factor::Integer(
                Token::basic("1", TokenKind::Integer),
                1,
            )))),
        ))),
    );
    assert!(actual.is_equivalent_to(&expected));
}

#[test]
fn return_true() {
    let src = "return true;";
    let actual = setup(&src, 1).statements.into_iter().next().unwrap();
    let expected = Statement::Return(
        Token::basic("return", TokenKind::Return),
        Box::new(EqualityExpr::Wrapped(RelationalExpr::Wrapped(
            AdditiveExpr::Wrapped(Term::Wrapped(PrefixExpr::Wrapped(Factor::Boolean(
                Token::basic("true", TokenKind::True),
                true,
            )))),
        ))),
    );
    assert!(actual.is_equivalent_to(&expected));
}

#[test]
fn return_false() {
    let src = "return false;";
    let actual = setup(&src, 1).statements.into_iter().next().unwrap();
    let expected = Statement::Return(
        Token::basic("return", TokenKind::Return),
        Box::new(EqualityExpr::Wrapped(RelationalExpr::Wrapped(
            AdditiveExpr::Wrapped(Term::Wrapped(PrefixExpr::Wrapped(Factor::Boolean(
                Token::basic("false", TokenKind::False),
                false,
            )))),
        ))),
    );
    assert!(actual.is_equivalent_to(&expected));
}

#[test]
fn operator_precedence() {
    for (implicit_original, explicit) in [
        ("let a = 1 + 2 + 3;", "let a = (1 + 2) + 3;"),
        ("let a = 1 + 2 - 3;", "let a = ((1 + 2) - 3);"),
        ("let a = 1 * 2 * 3;", "let a = ((1 * 2) * 3);"),
        ("let x = a * b / c;", "let x = ((a * b) / c);"),
        ("let x = a + b / c;", "let x = (a + (b / c));"),
        (
            "return a + b * c + d / e - f;",
            "return (((a + (b * c)) + (d / e)) - f);",
        ),
        ("return -a * b;", "return ((-a) * b);"),
        ("return !-a;", "return (!(-a));"),
        ("return 5 > 4 == 3 < 4;", "return (5 > 4) == (3 < 4);"),
        ("return 5 > 4 != 3 < 4;", "return (5 > 4) != (3 < 4);"),
        (
            "return 3 + 4 * 5 == 3*1 + 4*5;",
            "return (3 + (4 * 5)) == ((3*1) + (4*5));",
        ),
        ("return 3 > 5 == false;", "return ((3 > 5) == false);"),
    ]
    .iter()
    {
        let implicit = lex(implicit_original);
        let implicit = parse(&implicit).unwrap();
        let implicit = &implicit.statements[0];

        let explicit = lex(explicit);
        let explicit = parse(&explicit).unwrap();
        let explicit = &explicit.statements[0];

        assert!(implicit.is_equivalent_to(&explicit), implicit_original);
    }
}
