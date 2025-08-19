use super::arithmetich_expression::ArithmeticExpression;
use rand::{Rng, SeedableRng};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BooleanExpression {
    Literal(bool),
    Comparison {
        left: ArithmeticExpression,
        op: ComparisonOperator,
        right: ArithmeticExpression,
    },
    LogicalOp {
        left: Box<BooleanExpression>,
        op: LogicalOperator,
        right: Box<BooleanExpression>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ComparisonOperator {
    Equal,        // ==
    NotEqual,     // !=
    LessThan,     // <
    LessEqual,    // <=
    GreaterThan,  // >
    GreaterEqual, // >=
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LogicalOperator {
    And, // &&
    Or,  // ||
}

impl BooleanExpression {
    pub fn generate_random_boolean_expression<T: Rng + SeedableRng>(
        max_depth: usize,
        rng: &mut T,
    ) -> Self {
        if max_depth == 0 {
            // At leaf level, generate boolean literal
            return BooleanExpression::Literal(rng.random_range(0..2) == 0);
        }

        match rng.random_range(0..3) {
            0 => {
                // Generate boolean literal
                BooleanExpression::Literal(rng.random_range(0..2) == 0)
            }
            1 => {
                // Generate comparison expression
                let left = ArithmeticExpression::generate_random_expression(1, rng);
                let right = ArithmeticExpression::generate_random_expression(1, rng);
                let op = ComparisonOperator::generate_random_comparison_operator(rng);
                BooleanExpression::Comparison { left, op, right }
            }
            _ => {
                // Generate logical operation
                let left = Box::new(Self::generate_random_boolean_expression(max_depth - 1, rng));
                let right = Box::new(Self::generate_random_boolean_expression(max_depth - 1, rng));
                let op = LogicalOperator::generate_random_logical_operator(rng);
                BooleanExpression::LogicalOp { left, op, right }
            }
        }
    }
}

impl ComparisonOperator {
    pub fn generate_random_comparison_operator<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        match rng.random_range(0..6) {
            0 => ComparisonOperator::Equal,
            1 => ComparisonOperator::NotEqual,
            2 => ComparisonOperator::LessThan,
            3 => ComparisonOperator::LessEqual,
            4 => ComparisonOperator::GreaterThan,
            _ => ComparisonOperator::GreaterEqual,
        }
    }
}

impl LogicalOperator {
    pub fn generate_random_logical_operator<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        if rng.random_range(0..2) == 0 {
            LogicalOperator::And
        } else {
            LogicalOperator::Or
        }
    }
}

impl Display for BooleanExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BooleanExpression::Literal(b) => {
                write!(f, "{}", b)
            }
            BooleanExpression::Comparison { left, op, right } => {
                write!(f, "{} {} {}", left, op, right)
            }
            BooleanExpression::LogicalOp { left, op, right } => {
                write!(f, "{} {} {}", left, op, right)
            }
        }
    }
}

impl Display for ComparisonOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ComparisonOperator::Equal => write!(f, "=="),
            ComparisonOperator::NotEqual => write!(f, "!="),
            ComparisonOperator::LessThan => write!(f, "<"),
            ComparisonOperator::LessEqual => write!(f, "<="),
            ComparisonOperator::GreaterThan => write!(f, ">"),
            ComparisonOperator::GreaterEqual => write!(f, ">="),
        }
    }
}

impl Display for LogicalOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicalOperator::And => write!(f, "&&"),
            LogicalOperator::Or => write!(f, "||"),
        }
    }
}
