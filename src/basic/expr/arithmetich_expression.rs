use ordered_float::OrderedFloat;
use rand::{Rng, SeedableRng};

use std::fmt::Display;

use super::operator::Operator;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArithmeticExpression {
    Int(i32),
    Float(OrderedFloat<f32>),
    BinaryOp {
        left: Box<ArithmeticExpression>,
        op: Operator,
        right: Box<ArithmeticExpression>,
    },
}

impl ArithmeticExpression {
    /// Check if this expression is primarily an integer type
    pub fn is_int(&self) -> bool {
        match self {
            ArithmeticExpression::Int(_) => true,
            ArithmeticExpression::Float(_) => false,
            ArithmeticExpression::BinaryOp { left, right, .. } => {
                // For binary operations, if both operands are int, result is int
                // Otherwise, result is float
                left.is_int() && right.is_int()
            }
        }
    }

    /// Check if this expression is primarily a float type
    pub fn is_float(&self) -> bool {
        !self.is_int()
    }

    pub fn generate_random_expression<T: Rng + SeedableRng>(max_depth: usize, rng: &mut T) -> Self {
        if max_depth == 0 {
            return match rng.random_range(0..=1) {
                0 => ArithmeticExpression::Int(rng.random_range(-100..=100)),
                1 => ArithmeticExpression::Float(OrderedFloat::from(rng.random::<f32>() * 100.0)),
                _ => unreachable!(),
            };
        }

        match rng.random_range(0..=2) {
            0 => ArithmeticExpression::Int(rng.random_range(-100..=100)),
            1 => ArithmeticExpression::Float(OrderedFloat::from(rng.random::<f32>() * 100.0)),
            2 => ArithmeticExpression::BinaryOp {
                left: Box::new(Self::generate_random_expression(max_depth - 1, rng)),
                op: Operator::generate_random_operator(rng),
                right: Box::new(Self::generate_random_expression(max_depth - 1, rng)),
            },
            _ => unreachable!(),
        }
    }
}

impl Display for ArithmeticExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArithmeticExpression::Int(n) => {
                if *n < 0 {
                    write!(f, "({})", n)
                } else {
                    write!(f, "{}", n)
                }
            }
            ArithmeticExpression::Float(x) => {
                if x.into_inner() < 0.0 {
                    write!(f, "({})", x)
                } else {
                    write!(f, "{}", x)
                }
            }
            ArithmeticExpression::BinaryOp { left, op, right } => {
                let left_str = format!("{left}");
                let right_str = format!("{right}");

                let left_needs_paren = match (&**left, op) {
                    (ArithmeticExpression::BinaryOp { op: left_op, .. }, op) => {
                        left_op.get_precedence() < op.get_precedence()
                    }
                    _ => false,
                };

                let right_needs_paren = match (&**right, op) {
                    (ArithmeticExpression::BinaryOp { op: right_op, .. }, op) => {
                        right_op.get_precedence() <= op.get_precedence()
                    }
                    _ => false,
                };

                write!(
                    f,
                    "{}{} {} {}{}",
                    if left_needs_paren { "(" } else { "" },
                    left_str,
                    op,
                    if right_needs_paren { "(" } else { "" },
                    right_str
                )?;

                if left_needs_paren {
                    write!(f, ")")?;
                }
                if right_needs_paren {
                    write!(f, ")")?;
                }

                Ok(())
            }
        }
    }
}
