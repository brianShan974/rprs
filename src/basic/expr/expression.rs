use derive_more::Display;
use rand::{Rng, SeedableRng};

use super::arithmetich_expression::ArithmeticExpression;
use super::boolean_expression::BooleanExpression;

#[derive(Clone, Debug, Display, PartialEq, Eq, Hash)]
#[display("{}", _0)]
pub enum Expression {
    Arithmetic(ArithmeticExpression),
    Boolean(BooleanExpression),
}

impl Expression {
    pub fn generate_random_expression<T: Rng + SeedableRng>(max_depth: usize, rng: &mut T) -> Self {
        // 70% chance to generate arithmetic expression, 30% chance to generate boolean expression
        if rng.random_range(0..10) < 7 {
            Self::Arithmetic(ArithmeticExpression::generate_random_expression(
                max_depth, rng,
            ))
        } else {
            Self::Boolean(BooleanExpression::generate_random_boolean_expression(
                max_depth, rng,
            ))
        }
    }

    pub fn is_arithmetic(&self) -> bool {
        matches!(self, Self::Arithmetic(_))
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, Self::Boolean(_))
    }

    /// Check if this expression is primarily an integer type
    pub fn is_int(&self) -> bool {
        match self {
            Self::Arithmetic(arith) => arith.is_int(),
            Self::Boolean(_) => false,
        }
    }

    /// Check if this expression is primarily a float type
    pub fn is_float(&self) -> bool {
        match self {
            Self::Arithmetic(arith) => arith.is_float(),
            Self::Boolean(_) => false,
        }
    }
}
