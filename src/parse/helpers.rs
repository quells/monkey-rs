use parse::ast::*;

pub trait EquivalentTo {
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

                lhs = $this::$this(Box::new(lhs.clone()), op, Box::new(rhs)).to_child();
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