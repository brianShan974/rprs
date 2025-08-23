use rand::seq::IndexedRandom;
use rand::{Rng, SeedableRng};

use std::fmt::Display;

use super::arithmetic_expression::ArithmeticExpression;
use crate::basic::body::fun::function::Function;
use crate::basic::expr::expression::Expression;
use crate::basic::expr::operator::Operator;
use crate::basic::var::variable::Variable;
use std::cell::RefCell;
use std::rc::Rc;

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
    FunctionCall(String, Vec<Expression>),
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
    /// Generate a boolean expression that prioritizes variables over literals
    /// Used for logical operations to ensure minimal literal usage
    pub fn generate_variable_prioritized_boolean_expression<T: Rng + SeedableRng>(
        max_depth: usize,
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
        external_variables: Option<&[Variable]>,
        rng: &mut T,
    ) -> Self {
        if max_depth == 0 {
            // At max depth, FORCE boolean variable reference if available
            if let Some(variables) = external_variables {
                let boolean_variables: Vec<_> =
                    variables.iter().filter(|var| var.is_boolean()).collect();

                if !boolean_variables.is_empty() {
                    let variable = boolean_variables.choose(rng).unwrap();
                    return Self::VariableReference(variable.get_name().to_string());
                }
            }
            // Only fallback to literal if absolutely no boolean variables
            return Self::generate_random_boolean_literal(rng);
        }

        // For logical operations, HEAVILY prioritize variable-based expressions
        match rng.random_range(0..10) {
            0..=3 => {
                // Generate direct boolean variable reference (40% probability - MAXIMIZED)
                if let Some(variables) = external_variables {
                    let boolean_variables: Vec<_> =
                        variables.iter().filter(|var| var.is_boolean()).collect();

                    if !boolean_variables.is_empty() {
                        let bool_var = boolean_variables.choose(rng).unwrap();
                        return Self::VariableReference(bool_var.get_name().to_string());
                    }
                }
                // Fallback to simple variable comparison if no boolean variables
                if let Some(variables) = external_variables {
                    let numeric_variables: Vec<_> =
                        variables.iter().filter(|var| var.is_numeric()).collect();

                    if numeric_variables.len() >= 2 {
                        // Generate variable vs variable comparison
                        let left_var = numeric_variables.choose(rng).unwrap();
                        let right_var = numeric_variables.choose(rng).unwrap();
                        let op = ComparisonOperator::generate_random_comparison_operator(rng);
                        return Self::Comparison {
                            left: ArithmeticExpression::VariableReference(
                                left_var.get_name().to_string(),
                            ),
                            op,
                            right: ArithmeticExpression::VariableReference(
                                right_var.get_name().to_string(),
                            ),
                        };
                    }
                }
                // Last resort: boolean literal
                Self::generate_random_boolean_literal(rng)
            }
            4..=8 => {
                // Generate variable-based comparison (50% probability)
                if let Some(variables) = external_variables {
                    let numeric_variables: Vec<_> =
                        variables.iter().filter(|var| var.is_numeric()).collect();

                    if !numeric_variables.is_empty() {
                        // At least one operand MUST be a variable
                        let left_var = numeric_variables.choose(rng).unwrap();
                        let left = ArithmeticExpression::VariableReference(
                            left_var.get_name().to_string(),
                        );

                        let right = if !numeric_variables.is_empty() && rng.random_bool(2.0 / 3.0) {
                            // 67% chance for another variable
                            let right_var = numeric_variables.choose(rng).unwrap();
                            ArithmeticExpression::VariableReference(
                                right_var.get_name().to_string(),
                            )
                        } else {
                            // 33% chance for simple literal
                            if rng.random_bool(1.0 / 2.0) {
                                ArithmeticExpression::generate_random_int_literal(rng)
                            } else {
                                ArithmeticExpression::generate_random_float_literal(rng)
                            }
                        };

                        let op = ComparisonOperator::generate_random_comparison_operator(rng);
                        return Self::Comparison { left, op, right };
                    }
                }
                // Fallback to boolean literal
                Self::generate_random_boolean_literal(rng)
            }
            9 => {
                // Generate function call that returns boolean (10% probability - for logical operations)
                if let Some(functions) = external_functions {
                    let functions_borrowed = functions.borrow();
                    if !functions_borrowed.is_empty() {
                        // Filter functions that return boolean types and are NOT class methods
                        let boolean_functions: Vec<_> = functions_borrowed
                            .iter()
                            .filter(|func| func.is_boolean_function() && !func.is_method())
                            .collect();

                        if !boolean_functions.is_empty() {
                            let function = boolean_functions.choose(rng).unwrap();
                            let function_name = function.get_name().to_string();

                            // Generate arguments that match function parameter types
                            let mut args = Vec::with_capacity(function.get_parameters().len());
                            for param in function.get_parameters() {
                                let param_type = param.get_type();
                                let arg = if let Some(variables) = external_variables {
                                    // Try to find a variable of matching type first
                                    let matching_vars: Vec<_> = variables
                                        .iter()
                                        .filter(|var| var.get_type() == Some(param_type))
                                        .collect();

                                    if !matching_vars.is_empty() && rng.random_bool(4.0 / 5.0) {
                                        // 80% chance to use matching variable
                                        let variable = matching_vars.choose(rng).unwrap();
                                        Expression::VariableReference(
                                            variable.get_name().to_string(),
                                        )
                                    } else {
                                        // Generate expression of matching type
                                        Expression::generate_expression_for_type(
                                            param_type,
                                            max_depth.saturating_sub(1),
                                            Some(functions.clone()),
                                            external_variables,
                                            rng,
                                        )
                                    }
                                } else {
                                    // Generate expression of matching type
                                    Expression::generate_expression_for_type(
                                        param_type,
                                        max_depth.saturating_sub(1),
                                        Some(functions.clone()),
                                        external_variables,
                                        rng,
                                    )
                                };
                                args.push(arg);
                            }

                            return Self::FunctionCall(function_name, args);
                        }
                    }
                }

                // Fallback to boolean literal if no suitable functions available
                Self::generate_random_boolean_literal(rng)
            }
            _ => {
                // Generate boolean literal (10% probability - minimized)
                Self::generate_random_boolean_literal(rng)
            }
        }
    }

    pub fn generate_random_boolean_expression<T: Rng + SeedableRng>(
        max_depth: usize,
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
        external_variables: Option<&[Variable]>,
        rng: &mut T,
    ) -> Self {
        if max_depth == 0 {
            // At max depth, try to generate boolean variable reference first if available (high probability)
            if let Some(variables) = external_variables {
                let boolean_variables: Vec<_> =
                    variables.iter().filter(|var| var.is_boolean()).collect();

                if !boolean_variables.is_empty() && rng.random_bool(3.0 / 4.0) {
                    // 75% chance to generate boolean variable reference (increased)
                    let variable = boolean_variables.choose(rng).unwrap();
                    return Self::VariableReference(variable.get_name().to_string());
                }
            }
            // Fallback to boolean literal
            return Self::generate_random_boolean_literal(rng);
        }

        match rng.random_range(0..10) {
            0 => {
                // Generate boolean literal (10% probability - reduced)
                Self::generate_random_boolean_literal(rng)
            }
            1..=5 => {
                // Generate comparison expression (50% probability - reduced to make room for function calls)
                // Ensure at least one operand is a variable to reduce literal comparisons
                let left_is_variable = rng.random_bool(9.0 / 10.0); // 90% chance for variable (significantly increased)
                let right_is_variable = rng.random_bool(3.0 / 4.0); // 75% chance for variable (increased)

                let left = if left_is_variable {
                    // FORCE variable reference first - prioritize direct variable usage
                    if let Some(variables) = external_variables {
                        let numeric_variables: Vec<_> =
                            variables.iter().filter(|var| var.is_numeric()).collect();

                        if !numeric_variables.is_empty() {
                            let variable = numeric_variables.choose(rng).unwrap();
                            ArithmeticExpression::VariableReference(variable.get_name().to_string())
                        } else {
                            // If no numeric variables, force simple literal instead of complex expression
                            if rng.random_bool(1.0 / 2.0) {
                                ArithmeticExpression::generate_random_int_literal(rng)
                            } else {
                                ArithmeticExpression::generate_random_float_literal(rng)
                            }
                        }
                    } else {
                        // If no variables available, force simple literal
                        if rng.random_bool(1.0 / 2.0) {
                            ArithmeticExpression::generate_random_int_literal(rng)
                        } else {
                            ArithmeticExpression::generate_random_float_literal(rng)
                        }
                    }
                } else {
                    // Generate simple arithmetic expression or literal - avoid too much complexity
                    if rng.random_bool(1.0 / 3.0) {
                        // 33% chance for simple binary operation
                        let op_left = if rng.random_bool(1.0 / 2.0) {
                            ArithmeticExpression::generate_random_int_literal(rng)
                        } else {
                            ArithmeticExpression::generate_random_float_literal(rng)
                        };
                        let op_right = if rng.random_bool(1.0 / 2.0) {
                            ArithmeticExpression::generate_random_int_literal(rng)
                        } else {
                            ArithmeticExpression::generate_random_float_literal(rng)
                        };
                        ArithmeticExpression::BinaryOp {
                            left: Box::new(op_left),
                            op: Operator::generate_random_operator(rng),
                            right: Box::new(op_right),
                        }
                    } else {
                        // 67% chance for simple literal
                        if rng.random_bool(1.0 / 2.0) {
                            ArithmeticExpression::generate_random_int_literal(rng)
                        } else {
                            ArithmeticExpression::generate_random_float_literal(rng)
                        }
                    }
                };

                let right = if right_is_variable {
                    if let Some(variables) = external_variables {
                        let numeric_variables: Vec<_> =
                            variables.iter().filter(|var| var.is_numeric()).collect();

                        if !numeric_variables.is_empty() {
                            let variable = numeric_variables.choose(rng).unwrap();
                            ArithmeticExpression::VariableReference(variable.get_name().to_string())
                        } else if rng.random_bool(1.0 / 2.0) {
                            ArithmeticExpression::generate_random_int_literal(rng)
                        } else {
                            ArithmeticExpression::generate_random_float_literal(rng)
                        }
                    } else if rng.random_bool(1.0 / 2.0) {
                        ArithmeticExpression::generate_random_int_literal(rng)
                    } else {
                        ArithmeticExpression::generate_random_float_literal(rng)
                    }
                } else {
                    // Generate simple arithmetic expression or literal - avoid too much complexity
                    if rng.random_bool(1.0 / 3.0) {
                        // 33% chance for simple binary operation
                        let op_left = if rng.random_bool(1.0 / 2.0) {
                            ArithmeticExpression::generate_random_int_literal(rng)
                        } else {
                            ArithmeticExpression::generate_random_float_literal(rng)
                        };
                        let op_right = if rng.random_bool(1.0 / 2.0) {
                            ArithmeticExpression::generate_random_int_literal(rng)
                        } else {
                            ArithmeticExpression::generate_random_float_literal(rng)
                        };
                        ArithmeticExpression::BinaryOp {
                            left: Box::new(op_left),
                            op: Operator::generate_random_operator(rng),
                            right: Box::new(op_right),
                        }
                    } else {
                        // 67% chance for simple literal
                        if rng.random_bool(1.0 / 2.0) {
                            ArithmeticExpression::generate_random_int_literal(rng)
                        } else {
                            ArithmeticExpression::generate_random_float_literal(rng)
                        }
                    }
                };

                let op = ComparisonOperator::generate_random_comparison_operator(rng);
                Self::Comparison { left, op, right }
            }
            6 => {
                // Generate logical operation (10% probability - reduced)
                // PRIORITIZE variable-based boolean expressions for complex logical operations

                let left = Box::new(Self::generate_variable_prioritized_boolean_expression(
                    max_depth - 1,
                    external_functions.clone(),
                    external_variables,
                    rng,
                ));
                let right = Box::new(Self::generate_variable_prioritized_boolean_expression(
                    max_depth - 1,
                    external_functions,
                    external_variables,
                    rng,
                ));
                let op = LogicalOperator::generate_random_logical_operator(rng);
                Self::LogicalOp { left, op, right }
            }
            7..=8 => {
                if let Some(variables) = external_variables {
                    let boolean_variables: Vec<_> =
                        variables.iter().filter(|var| var.is_boolean()).collect();

                    if !boolean_variables.is_empty() {
                        // Create a direct boolean variable reference
                        let bool_var = boolean_variables.choose(rng).unwrap();
                        Self::VariableReference(bool_var.get_name().to_string())
                    } else {
                        // Fallback to boolean literal if no boolean variables available
                        Self::generate_random_boolean_literal(rng)
                    }
                } else {
                    // Fallback to boolean literal if no variables available
                    Self::generate_random_boolean_literal(rng)
                }
            }
            9 => {
                if let Some(functions) = external_functions {
                    let functions_borrowed = functions.borrow();
                    if !functions_borrowed.is_empty() {
                        let boolean_functions: Vec<_> = functions_borrowed
                            .iter()
                            .filter(|func| func.is_boolean_function() && !func.is_method())
                            .collect();

                        if !boolean_functions.is_empty() {
                            let function = boolean_functions.choose(rng).unwrap();
                            let function_name = function.get_name().to_string();

                            // Generate arguments that match function parameter types
                            let mut args = Vec::with_capacity(function.get_parameters().len());
                            for param in function.get_parameters() {
                                let param_type = param.get_type();
                                let arg = if let Some(variables) = external_variables {
                                    let matching_vars: Vec<_> = variables
                                        .iter()
                                        .filter(|var| var.get_type() == Some(param_type))
                                        .collect();

                                    if !matching_vars.is_empty() && rng.random_bool(4.0 / 5.0) {
                                        // 80% chance to use matching variable
                                        let variable = matching_vars.choose(rng).unwrap();
                                        Expression::VariableReference(
                                            variable.get_name().to_string(),
                                        )
                                    } else {
                                        // Generate expression of matching type
                                        Expression::generate_expression_for_type(
                                            param_type,
                                            max_depth.saturating_sub(1),
                                            Some(functions.clone()),
                                            external_variables,
                                            rng,
                                        )
                                    }
                                } else {
                                    // Generate expression of matching type
                                    Expression::generate_expression_for_type(
                                        param_type,
                                        max_depth.saturating_sub(1),
                                        Some(functions.clone()),
                                        external_variables,
                                        rng,
                                    )
                                };
                                args.push(arg);
                            }

                            return Self::FunctionCall(function_name, args);
                        }
                    }
                }

                // Fallback to boolean literal if no suitable functions available
                Self::generate_random_boolean_literal(rng)
            }
            _ => {
                // Fallback to boolean literal for any other case
                Self::generate_random_boolean_literal(rng)
            }
        }
    }

    pub fn generate_random_boolean_literal<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        Self::Literal(rng.random())
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
        if rng.random_bool(1.0 / 2.0) {
            LogicalOperator::And
        } else {
            LogicalOperator::Or
        }
    }
}

impl Display for BooleanExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(b) => {
                write!(f, "{}", b)
            }
            Self::Comparison { left, op, right } => {
                write!(f, "{} {} {}", left, op, right)
            }
            Self::LogicalOp { left, op, right } => {
                write!(f, "{} {} {}", left, op, right)
            }
            Self::FunctionCall(name, args) => {
                let args_str = args
                    .iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{}({})", name, args_str)
            }
            Self::VariableReference(var_name) => {
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
