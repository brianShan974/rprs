use ordered_float::OrderedFloat;
use rand::{Rng, SeedableRng};
use std::fmt::Display;

use super::operator::Operator;
use crate::basic::body::fun::function::Function;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArithmeticExpression {
    Int(i32),
    Float(OrderedFloat<f32>),
    BinaryOp {
        left: Box<ArithmeticExpression>,
        op: Operator,
        right: Box<ArithmeticExpression>,
    },
    FunctionCall(String, Vec<super::expression::Expression>),
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
            ArithmeticExpression::FunctionCall(_, _) => false,
        }
    }

    /// Check if this expression is primarily a float type
    pub fn is_float(&self) -> bool {
        !self.is_int()
    }

    pub fn generate_random_expression<T: Rng + SeedableRng>(
        max_depth: usize,
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
        rng: &mut T,
    ) -> Self {
        // For now, keep the existing logic but we'll need to modify this to use typed generation
        Self::generate_random_expression_untyped(max_depth, external_functions, rng)
    }

    pub fn generate_random_expression_untyped<T: Rng + SeedableRng>(
        max_depth: usize,
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
        rng: &mut T,
    ) -> Self {
        if max_depth == 0 {
            return match rng.random_range(0..=1) {
                0 => ArithmeticExpression::Int(rng.random_range(-100..=100)),
                1 => ArithmeticExpression::Float(OrderedFloat::from(rng.random::<f32>() * 100.0)),
                _ => unreachable!(),
            };
        }

        match rng.random_range(0..=5) {
            0 => ArithmeticExpression::Int(rng.random_range(-100..=100)),
            1 => ArithmeticExpression::Float(OrderedFloat::from(rng.random::<f32>() * 100.0)),
            2 => ArithmeticExpression::BinaryOp {
                left: Box::new(Self::generate_random_expression(
                    max_depth - 1,
                    external_functions.clone(),
                    rng,
                )),
                op: Operator::generate_random_operator(rng),
                right: Box::new(Self::generate_random_expression(
                    max_depth - 1,
                    external_functions,
                    rng,
                )),
            },
            3..=4 => {
                // Generate function call if external_functions is provided and not empty
                if let Some(functions) = external_functions {
                    let functions_borrowed = functions.borrow();
                    if !functions_borrowed.is_empty() {
                        let function =
                            &functions_borrowed[rng.random_range(0..functions_borrowed.len())];
                        let function_name = function.get_name().to_string();

                        // Generate random arguments (0-2 expressions)
                        let num_args = rng.random_range(0..=2);
                        let mut args = Vec::with_capacity(num_args);
                        for _ in 0..num_args {
                            // Allow recursive function calls in arguments
                            args.push(super::expression::Expression::generate_random_expression(
                                max_depth.saturating_sub(1),
                                Some(functions.clone()),
                                rng,
                            ));
                        }

                        return ArithmeticExpression::FunctionCall(function_name, args);
                    }
                }

                // Fallback to simple arithmetic expression
                ArithmeticExpression::Int(rng.random_range(-100..=100))
            }
            5 => {
                // Generate binary operation with function call as one operand
                if let Some(functions) = external_functions {
                    let functions_borrowed = functions.borrow();
                    if !functions_borrowed.is_empty() {
                        let function =
                            &functions_borrowed[rng.random_range(0..functions_borrowed.len())];
                        let function_name = function.get_name().to_string();

                        // Generate random arguments (0-2 expressions)
                        let num_args = rng.random_range(0..=2);
                        let mut args = Vec::with_capacity(num_args);
                        for _ in 0..num_args {
                            // Allow recursive function calls in arguments
                            args.push(super::expression::Expression::generate_random_expression(
                                max_depth.saturating_sub(1),
                                Some(functions.clone()),
                                rng,
                            ));
                        }

                        let function_call = ArithmeticExpression::FunctionCall(function_name, args);

                        // Decide whether function call should be left or right operand
                        if rng.random_range(0..=1) == 0 {
                            ArithmeticExpression::BinaryOp {
                                left: Box::new(function_call),
                                op: Operator::generate_random_operator(rng),
                                right: Box::new(Self::generate_random_expression(
                                    max_depth - 1,
                                    Some(functions.clone()),
                                    rng,
                                )),
                            }
                        } else {
                            ArithmeticExpression::BinaryOp {
                                left: Box::new(Self::generate_random_expression(
                                    max_depth - 1,
                                    Some(functions.clone()),
                                    rng,
                                )),
                                op: Operator::generate_random_operator(rng),
                                right: Box::new(function_call),
                            }
                        }
                    } else {
                        // Fallback to simple arithmetic expression
                        ArithmeticExpression::Int(rng.random_range(-100..=100))
                    }
                } else {
                    // Fallback to simple arithmetic expression
                    ArithmeticExpression::Int(rng.random_range(-100..=100))
                }
            }
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
            ArithmeticExpression::FunctionCall(name, args) => {
                let args_str = args
                    .iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{}({})", name, args_str)?;
                Ok(())
            }
        }
    }
}
