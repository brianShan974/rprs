use rand::{Rng, SeedableRng};

use std::fmt::Display;

use super::arithmetic_expression::ArithmeticExpression;

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
    FunctionCall(String, Vec<crate::basic::expr::expression::Expression>),
    VariableReference(String), // Direct boolean variable reference
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
        external_variables: Option<&[crate::basic::var::variable::Variable]>,
        rng: &mut T,
    ) -> Self {
        if max_depth == 0 {
            // At leaf level, try to generate boolean variable reference first, then boolean literal
            if let Some(variables) = external_variables {
                let boolean_variables: Vec<_> = variables
                    .iter()
                    .filter(|var| {
                        if let Some(var_type) = var.get_type() {
                            matches!(
                                var_type,
                                crate::basic::cls::class::Class::Basic(
                                    crate::basic::cls::basic_type::BasicType::Boolean
                                )
                            )
                        } else {
                            false
                        }
                    })
                    .collect();

                if !boolean_variables.is_empty() && rng.random_range(0..3) < 2 {
                    // 67% chance to use boolean variable at leaf level
                    let bool_var = boolean_variables[rng.random_range(0..boolean_variables.len())];
                    return BooleanExpression::VariableReference(bool_var.get_name().to_string());
                }
            }
            // Fallback to boolean literal
            return BooleanExpression::Literal(rng.random_range(0..2) == 0);
        }

        match rng.random_range(0..10) {
            0..=1 => {
                // Generate boolean literal (20% probability)
                BooleanExpression::Literal(rng.random_range(0..2) == 0)
            }
            2..=6 => {
                // Generate comparison expression (50% probability - increased for more variable usage)
                let left = ArithmeticExpression::generate_random_expression(
                    1,
                    None,
                    external_variables,
                    rng,
                );
                let right = ArithmeticExpression::generate_random_expression(
                    1,
                    None,
                    external_variables,
                    rng,
                );
                let op = ComparisonOperator::generate_random_comparison_operator(rng);
                BooleanExpression::Comparison { left, op, right }
            }
            7..=8 => {
                // Generate logical operation (20% probability)
                let left = Box::new(Self::generate_random_boolean_expression(
                    max_depth - 1,
                    external_variables,
                    rng,
                ));
                let right = Box::new(Self::generate_random_boolean_expression(
                    max_depth - 1,
                    external_variables,
                    rng,
                ));
                let op = LogicalOperator::generate_random_logical_operator(rng);
                BooleanExpression::LogicalOp { left, op, right }
            }
            _ => {
                // Generate direct boolean variable reference (10% probability - new!)
                if let Some(variables) = external_variables {
                    let boolean_variables: Vec<_> = variables
                        .iter()
                        .filter(|var| {
                            if let Some(var_type) = var.get_type() {
                                matches!(
                                    var_type,
                                    crate::basic::cls::class::Class::Basic(
                                        crate::basic::cls::basic_type::BasicType::Boolean
                                    )
                                )
                            } else {
                                false
                            }
                        })
                        .collect();

                    if !boolean_variables.is_empty() {
                        // Create a direct boolean variable reference
                        let bool_var =
                            boolean_variables[rng.random_range(0..boolean_variables.len())];
                        BooleanExpression::VariableReference(bool_var.get_name().to_string())
                    } else {
                        // Fallback to boolean literal if no boolean variables available
                        BooleanExpression::Literal(rng.random_range(0..2) == 0)
                    }
                } else {
                    // Fallback to boolean literal if no variables available
                    BooleanExpression::Literal(rng.random_range(0..2) == 0)
                }
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
            BooleanExpression::FunctionCall(name, args) => {
                let args_str = args
                    .iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{}({})", name, args_str)
            }
            BooleanExpression::VariableReference(var_name) => {
                write!(f, "{}", var_name)
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
