use std::fmt;

use arbitrary::{Arbitrary, Unstructured};

use crate::bitvecto::Bitvector;
use crate::draw::Pixel;

#[derive(Arbitrary, Clone, Debug)]
pub enum Expr {
    X,
    Y,

    Constant(#[arbitrary(with = |u: &mut Unstructured| u.int_in_range(0..=64))] usize),

    Mod(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),

    Add(Box<Expr>, Box<Expr>),

    Not(Box<Expr>),
    Xor(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Rol(Box<Expr>, Box<Expr>),
    Ror(Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn create_arbitrary<const BITS: u8>(data: &[u8]) -> Self {
        fn limit<const BITS: u8>(expr: Expr) -> Expr {
            match expr {
                ex @ (Expr::X | Expr::Y) => ex,

                Expr::Constant(value) => Expr::Constant(Bitvector::<BITS>::from(value).into()),

                Expr::Mod(a, b) => {
                    let a = Box::new(limit::<BITS>(*a));
                    let b = Box::new(limit::<BITS>(*b));
                    Expr::Mod(a, b)
                }

                Expr::Mul(a, b) => {
                    let a = Box::new(limit::<BITS>(*a));
                    let b = Box::new(limit::<BITS>(*b));
                    Expr::Mul(a, b)
                }

                Expr::Add(a, b) => {
                    let a = Box::new(limit::<BITS>(*a));
                    let b = Box::new(limit::<BITS>(*b));
                    Expr::Add(a, b)
                }

                Expr::Not(inner) => {
                    let inner = Box::new(limit::<BITS>(*inner));
                    Expr::Not(inner)
                }

                Expr::Xor(a, b) => {
                    let a = Box::new(limit::<BITS>(*a));
                    let b = Box::new(limit::<BITS>(*b));
                    Expr::Xor(a, b)
                }

                Expr::And(a, b) => {
                    let a = Box::new(limit::<BITS>(*a));
                    let b = Box::new(limit::<BITS>(*b));
                    Expr::And(a, b)
                }

                Expr::Or(a, b) => {
                    let a = Box::new(limit::<BITS>(*a));
                    let b = Box::new(limit::<BITS>(*b));
                    Expr::Or(a, b)
                }

                Expr::Rol(a, b) => {
                    let a = Box::new(limit::<BITS>(*a));
                    let b = Box::new(limit::<BITS>(*b));
                    Expr::Rol(a, b)
                }

                Expr::Ror(a, b) => {
                    let a = Box::new(limit::<BITS>(*a));
                    let b = Box::new(limit::<BITS>(*b));
                    Expr::Ror(a, b)
                }
            }
        }

        let expr = Self::arbitrary(&mut Unstructured::new(data)).unwrap();
        limit::<BITS>(expr)
    }

    fn evaluate<const BITS: u8>(&self, x: usize, y: usize) -> Bitvector<BITS> {
        match self {
            Self::X => x.into(),
            Self::Y => y.into(),
            Self::Constant(v) => (*v).into(),
            Self::Not(a) => !a.evaluate(x, y),
            Self::Mod(a, b) => a.evaluate(x, y) % b.evaluate(x, y),
            Self::Mul(a, b) => a.evaluate(x, y) * (b.evaluate(x, y)),
            Self::Add(a, b) => a.evaluate(x, y) + (b.evaluate(x, y)),
            Self::Xor(a, b) => a.evaluate(x, y) ^ b.evaluate(x, y),
            Self::And(a, b) => a.evaluate(x, y) & b.evaluate(x, y),
            Self::Or(a, b) => a.evaluate(x, y) | b.evaluate(x, y),
            Self::Rol(a, b) => a.evaluate(x, y).rotate_left(b.evaluate(x, y)),
            Self::Ror(a, b) => a.evaluate(x, y).rotate_right(b.evaluate(x, y)),
        }
    }

    fn precedence(&self) -> usize {
        match self {
            Self::X | Self::Y | Self::Constant(_) => 0,
            Self::Not(_) => 1,

            Self::Mod(..)
            | Self::Mul(..)
            | Self::Add(..)
            | Self::Xor(..)
            | Self::And(..)
            | Self::Or(..)
            | Self::Rol(..)
            | Self::Ror(..) => 2,
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let this = self.precedence();

        let (op, a, b) = match self {
            Self::X => return write!(f, "x"),
            Self::Y => return write!(f, "y"),
            Self::Constant(v) => return write!(f, "{v}"),
            Self::Not(other) => {
                return if other.precedence() <= this {
                    write!(f, "~{other}")
                } else {
                    write!(f, "~({other})")
                };
            }

            Self::Mod(a, b) => ("%", a, b),
            Self::Mul(a, b) => ("*", a, b),

            Self::Add(a, b) => ("+", a, b),

            Self::Xor(a, b) => ("^", a, b),
            Self::And(a, b) => ("&", a, b),
            Self::Or(a, b) => ("|", a, b),
            Self::Rol(a, b) => ("<<", a, b),
            Self::Ror(a, b) => (">>", a, b),
        };

        if a.precedence() < this {
            write!(f, "{a}")?;
        } else {
            write!(f, "({a})")?;
        }

        write!(f, " {op} ")?;

        if b.precedence() < this {
            write!(f, "{b}")?;
        } else {
            write!(f, "({b})")?;
        }

        Ok(())
    }
}

impl Pixel for Expr {
    fn get<const BITS: u8>(&self, x: usize, y: usize) -> u8 {
        let value: usize = self.evaluate::<BITS>(x, y).into();
        value as u8
    }
}
