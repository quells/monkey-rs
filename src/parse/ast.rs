use std::fmt::{Formatter, Result};

use crate::lex::Token;
use parse::helpers::*;

pub type Expression = Box<EqualityExpr>;

#[derive(Clone, Debug)]
pub struct Identifier(pub Token);

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.0.literal.fmt(f)
    }
}

impl EquivalentTo for Identifier {
    fn is_equivalent_to(&self, other: &Self) -> bool {
        self.0.is_equivalent_to(&other.0)
    }
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new(statements: &[Statement]) -> Program {
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
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Statement::Assignment(_, id, value) => format!("let {} = {};", id, value),
            Statement::Return(_, value) => format!("return {};", value),
        }.fmt(f)
    }
}

#[derive(Clone, Debug)]
pub enum EqualityExpr {
    Wrapped(RelationalExpr),
    EqualityExpr(Box<RelationalExpr>, EqualityBinOp, Box<RelationalExpr>),
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
        let prefix = wrapped_or_return_none!(term, Term);
        let factor = wrapped_or_return_none!(prefix, PrefixExpr);
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
        RelationalExpr::Wrapped(AdditiveExpr::Wrapped(Term::Wrapped(PrefixExpr::Wrapped(
            Factor::Wrapped(self.to_expression()),
        ))))
    }
}

#[derive(Clone, Debug)]
pub enum RelationalExpr {
    Wrapped(AdditiveExpr),
    RelationalExpr(Box<AdditiveExpr>, RelationalBinOp, Box<AdditiveExpr>),
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
        let prefix = wrapped_or_return_none!(term, Term);
        let factor = wrapped_or_return_none!(prefix, PrefixExpr);
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
        AdditiveExpr::Wrapped(Term::Wrapped(PrefixExpr::Wrapped(Factor::Wrapped(
            self.to_expression(),
        ))))
    }
}

#[derive(Clone, Debug)]
pub enum AdditiveExpr {
    Wrapped(Term),
    AdditiveExpr(Box<Term>, AdditiveBinOp, Box<Term>),
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
        let prefix = wrapped_or_return_none!(term, Term);
        let factor = wrapped_or_return_none!(prefix, PrefixExpr);
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
        Term::Wrapped(PrefixExpr::Wrapped(Factor::Wrapped(self.to_expression())))
    }
}

#[derive(Clone, Debug)]
pub enum Term {
    Wrapped(PrefixExpr),
    Term(Box<PrefixExpr>, TermBinOp, Box<PrefixExpr>),
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
        let prefix = wrapped_or_return_none!(self, Term);
        let factor = wrapped_or_return_none!(prefix, PrefixExpr);
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

impl ToChild<PrefixExpr> for Term {
    fn to_child(&self) -> PrefixExpr {
        PrefixExpr::Wrapped(Factor::Wrapped(self.to_expression()))
    }
}

#[derive(Clone, Debug)]
pub enum PrefixExpr {
    Wrapped(Factor),
    Invert(Factor),
    Negate(Factor),
}

impl AsParent<Term> for PrefixExpr {
    fn as_parent(&self) -> Option<Term> {
        let factor = wrapped_or_return_none!(self, PrefixExpr);
        let equality = wrapped_or_return_none!(factor, Factor);
        let unboxed = *(*equality).clone();
        let relational = wrapped_or_return_none!(unboxed, EqualityExpr);
        let additive = wrapped_or_return_none!(relational, RelationalExpr);
        let term = wrapped_or_return_none!(additive, AdditiveExpr);
        Some(term)
    }
}

impl_equivalent_to_unop!(PrefixExpr; Invert, Negate);

impl ToExpression for PrefixExpr {
    fn to_expression(&self) -> Expression {
        Box::new(EqualityExpr::Wrapped(RelationalExpr::Wrapped(
            AdditiveExpr::Wrapped(Term::Wrapped(self.clone())),
        )))
    }
}

impl ToChild<Factor> for PrefixExpr {
    fn to_child(&self) -> Factor {
        Factor::Wrapped(self.to_expression())
    }
}

impl std::fmt::Display for PrefixExpr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let (prefix, factor) = match self {
            PrefixExpr::Wrapped(factor) => return factor.fmt(f),
            PrefixExpr::Invert(factor) => ("!", factor),
            PrefixExpr::Negate(factor) => ("-", factor),
        };
        prefix.fmt(f)?;
        factor.fmt(f)
    }
}

#[derive(Clone, Debug)]
pub enum Factor {
    Wrapped(Expression),
    Identifier(Identifier),
    Integer(Token, isize),
    FunctionCall(Token, Vec<Expression>),
}

impl AsParent<PrefixExpr> for Factor {
    fn as_parent(&self) -> Option<PrefixExpr> {
        let equality = wrapped_or_return_none!(self, Factor);
        let unboxed = *(*equality).clone();
        let relational = wrapped_or_return_none!(unboxed, EqualityExpr);
        let additive = wrapped_or_return_none!(relational, RelationalExpr);
        let term = wrapped_or_return_none!(additive, AdditiveExpr);
        let prefix = wrapped_or_return_none!(term, Term);
        Some(prefix)
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
                AdditiveExpr::Wrapped(Term::Wrapped(PrefixExpr::Wrapped(v.clone()))),
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
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Factor::Wrapped(e) => e.fmt(f),
            Factor::Identifier(t) => t.0.literal.fmt(f),
            Factor::Integer(t, _) => t.literal.fmt(f),
            Factor::FunctionCall(id, params) => {
                let param_strs = match params.split_first() {
                    Some((first, rest)) => rest
                        .iter()
                        .fold(format!("{}", first), |a, b| format!("{}, {}", a, b)),
                    None => String::new(),
                };
                id.literal.fmt(f)?;
                "(".fmt(f)?;
                param_strs.fmt(f)?;
                ")".fmt(f)
            }
        }
    }
}
