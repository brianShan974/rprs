use std::fmt::Display;

use rand::Rng;

use super::operator::Operator;

#[derive(Debug, Clone)]
pub enum ArithmeticExpression {
    Int(i32),
    Float(f32),
    BinaryOp {
        left: Box<ArithmeticExpression>,
        op: Operator,
        right: Box<ArithmeticExpression>,
    },
}

impl ArithmeticExpression {
    pub fn generate_random_expression(max_depth: usize) -> Self {
        let mut rng = rand::rng();

        if max_depth == 0 {
            return match rng.random_range(0..=1) {
                0 => ArithmeticExpression::Int(rng.random_range(-100..=100)),
                1 => ArithmeticExpression::Float(rng.random::<f32>() * 100.0),
                _ => unreachable!(),
            };
        }

        match rng.random_range(0..=2) {
            0 => ArithmeticExpression::Int(rng.random_range(-100..=100)),
            1 => ArithmeticExpression::Float(rng.random::<f32>() * 100.0),
            2 => ArithmeticExpression::BinaryOp {
                left: Box::new(Self::generate_random_expression(max_depth - 1)),
                op: Operator::generate_random_operator(),
                right: Box::new(Self::generate_random_expression(max_depth - 1)),
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
                if *x < 0.0 {
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
