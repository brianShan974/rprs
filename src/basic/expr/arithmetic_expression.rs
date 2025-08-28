use ordered_float::OrderedFloat;
use rand::seq::IndexedRandom;
use rand::{Rng, SeedableRng};
use std::fmt::Display;

use super::operator::Operator;
use crate::basic::body::fun::function::Function;
use crate::basic::expr::expression::Expression;
use crate::basic::var::variable::Variable;
use std::cell::RefCell;
use std::rc::Rc;

// Probability constants for expression generation
const PROBABILITY_CONST_VARIABLE_USE: f64 = 1.0 / 3.0;
const PROBABILITY_VARIABLE_USE_AT_MAX_DEPTH: f64 = 9.0 / 10.0;
const PROBABILITY_USE_MATCHING_VARIABLE: f64 = 2.0 / 3.0;
const PROBABILITY_INT_VS_FLOAT_LITERAL: f64 = 1.0 / 2.0;
const PROBABILITY_FUNCTION_CALL_ON_LHS: f64 = 1.0 / 2.0;

// Range constants for random generation
const MAX_INT_LITERAL: i32 = 100;
const MIN_INT_LITERAL: i32 = -MAX_INT_LITERAL;
const FLOAT_LITERAL_RANGE: f32 = MAX_INT_LITERAL as f32;

// Expression generation strategy constants
const NUM_BINARY_OP_STRATEGIES: usize = 10;
const MAX_EXPRESSION_RANGE: usize = 19;
const MAX_COMPILE_TIME_CONST_RANGE: usize = 3;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArithmeticExpression {
    Int(i32),
    Float(OrderedFloat<f32>),
    BinaryOp {
        left: Box<ArithmeticExpression>,
        op: Operator,
        right: Box<ArithmeticExpression>,
    },
    FunctionCall(String, Vec<Expression>),
    VariableReference(String),
}

impl ArithmeticExpression {
    /// Check if this expression is primarily an integer type
    pub fn is_int(&self, external_variables: Option<&[Variable]>) -> bool {
        match self {
            ArithmeticExpression::Int(_) => true,
            ArithmeticExpression::Float(_) => false,
            ArithmeticExpression::BinaryOp { left, right, .. } => {
                // For binary operations, if both operands are int, result is int
                // Otherwise, result is float
                left.is_int(external_variables) && right.is_int(external_variables)
            }
            ArithmeticExpression::FunctionCall(_, _) => false,
            ArithmeticExpression::VariableReference(var_name) => {
                // Look up the variable type from external_variables
                if let Some(variables) = external_variables
                    && let Some(variable) = variables.iter().find(|v| v.get_name() == var_name)
                    && let Some(var_type) = variable.get_class()
                {
                    return var_type.is_integer_type();
                }
                false // Default to false if variable not found or type unknown
            }
        }
    }

    /// Check if this expression is primarily a float type
    pub fn is_float(&self, external_variables: Option<&[Variable]>) -> bool {
        !self.is_int(external_variables)
    }

    /// Generate an arithmetic expression that is guaranteed to be a compile-time constant
    pub fn generate_compile_time_constant_expression<T: Rng + SeedableRng>(
        max_depth: usize,
        target_is_int: bool, // true for int, false for float
        external_variables: Option<&[Variable]>,
        rng: &mut T,
    ) -> Self {
        if max_depth == 0 {
            // At max depth, try to generate const val variable reference first
            if let Some(variables) = external_variables {
                let const_vars: Vec<_> = variables
                    .iter()
                    .filter(|var| {
                        // Only const val variables are compile-time constants
                        var.is_const()
                            && var.is_numeric()
                            && var
                                .get_class()
                                .is_some_and(|ty| ty.is_integer_type() == target_is_int)
                    })
                    .collect();

                if !const_vars.is_empty() && rng.random_bool(PROBABILITY_CONST_VARIABLE_USE) {
                    let variable = const_vars.choose(rng).unwrap();
                    return ArithmeticExpression::VariableReference(
                        variable.get_name().to_string(),
                    );
                }
            }
            // Generate literal of target type
            return if target_is_int {
                ArithmeticExpression::generate_random_int_literal(rng)
            } else {
                ArithmeticExpression::generate_random_float_literal(rng)
            };
        }

        match rng.random_range(0..=MAX_COMPILE_TIME_CONST_RANGE) {
            0 => {
                // Generate literal of target type
                if target_is_int {
                    ArithmeticExpression::generate_random_int_literal(rng)
                } else {
                    ArithmeticExpression::generate_random_float_literal(rng)
                }
            }
            1 => {
                // Generate const val variable reference if available
                if let Some(variables) = external_variables {
                    let const_vars: Vec<_> = variables
                        .iter()
                        .filter(|var| {
                            // Only const val variables are compile-time constants
                            var.is_const()
                                && var.is_numeric()
                                && var
                                    .get_class()
                                    .is_some_and(|ty| ty.is_integer_type() == target_is_int)
                        })
                        .collect();

                    if !const_vars.is_empty() {
                        let variable = const_vars.choose(rng).unwrap();
                        return ArithmeticExpression::VariableReference(
                            variable.get_name().to_string(),
                        );
                    }
                }
                // Fallback to literal if no const variables available
                if target_is_int {
                    ArithmeticExpression::generate_random_int_literal(rng)
                } else {
                    ArithmeticExpression::generate_random_float_literal(rng)
                }
            }
            2..=3 => {
                // Generate binary operation of target type (only with compile-time constants)
                ArithmeticExpression::BinaryOp {
                    left: Box::new(Self::generate_compile_time_constant_expression(
                        max_depth - 1,
                        target_is_int,
                        external_variables,
                        rng,
                    )),
                    op: Operator::generate_random_operator(rng),
                    right: Box::new(Self::generate_compile_time_constant_expression(
                        max_depth - 1,
                        target_is_int,
                        external_variables,
                        rng,
                    )),
                }
            }
            _ => {
                // Fallback to literal
                if target_is_int {
                    ArithmeticExpression::generate_random_int_literal(rng)
                } else {
                    ArithmeticExpression::generate_random_float_literal(rng)
                }
            }
        }
    }

    pub fn generate_random_expression<T: Rng + SeedableRng>(
        max_depth: usize,
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
        external_variables: Option<&[Variable]>,
        rng: &mut T,
    ) -> Self {
        // For now, keep the existing logic but we'll need to modify this to use typed generation
        Self::generate_random_expression_untyped(
            max_depth,
            external_functions,
            external_variables,
            rng,
        )
    }

    /// Generate an arithmetic expression of a specific type (integer or float)
    pub fn generate_typed_expression<T: Rng + SeedableRng>(
        max_depth: usize,
        target_is_int: bool, // true for int, false for float
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
        external_variables: Option<&[Variable]>,
        rng: &mut T,
    ) -> Self {
        if max_depth == 0 {
            // At max depth, try to generate variable reference of matching type first
            if let Some(variables) = external_variables {
                let matching_vars: Vec<_> = variables
                    .iter()
                    .filter(|var| {
                        var.is_numeric()
                            && if target_is_int {
                                var.get_class().is_some_and(|t| t.is_integer_type())
                            } else {
                                var.get_class().is_some_and(|t| t.is_float_type())
                            }
                    })
                    .collect();

                if !matching_vars.is_empty()
                    && rng.random_bool(PROBABILITY_VARIABLE_USE_AT_MAX_DEPTH)
                {
                    let variable = matching_vars.choose(rng).unwrap();
                    return ArithmeticExpression::VariableReference(
                        variable.get_name().to_string(),
                    );
                }
            }
            // Generate literal of target type
            return if target_is_int {
                ArithmeticExpression::generate_random_int_literal(rng)
            } else {
                ArithmeticExpression::generate_random_float_literal(rng)
            };
        }

        match rng.random_range(0..=MAX_EXPRESSION_RANGE) {
            0..=1 => {
                // Generate literal of target type (10% probability - reduced for more complex expressions)
                if target_is_int {
                    ArithmeticExpression::generate_random_int_literal(rng)
                } else {
                    ArithmeticExpression::generate_random_float_literal(rng)
                }
            }
            2..=9 => {
                // Generate variable reference of matching type if available (40% probability - increased from 30%)
                if let Some(variables) = external_variables {
                    let matching_vars: Vec<_> = variables
                        .iter()
                        .filter(|var| {
                            var.is_numeric()
                                && if target_is_int {
                                    var.get_class().is_some_and(|t| t.is_integer_type())
                                } else {
                                    var.get_class().is_some_and(|t| t.is_float_type())
                                }
                        })
                        .collect();

                    if !matching_vars.is_empty() {
                        let variable = matching_vars.choose(rng).unwrap();
                        return ArithmeticExpression::VariableReference(
                            variable.get_name().to_string(),
                        );
                    }
                }
                // Fallback to literal of target type (removed aggressive variable usage)
                // Last resort fallback to literal
                if target_is_int {
                    ArithmeticExpression::generate_random_int_literal(rng)
                } else {
                    ArithmeticExpression::generate_random_float_literal(rng)
                }
            }
            10..=15 => {
                // Generate binary operation of target type with controlled operand types (30% probability - reduced from 40% to make room for more variables)
                // Ensure a good mix of variable + literal combinations
                // Use different strategies to create more variable + literal combinations
                // Increase variable + literal combinations to 85% probability
                let strategy = rng.random_range(0..NUM_BINARY_OP_STRATEGIES);
                let (left_is_variable, right_is_variable) = match strategy {
                    0..=3 => (true, false), // variable + literal (40%)
                    4..=7 => (false, true), // literal + variable (40%)
                    8 => (true, true),      // variable + variable (10%)
                    9 => (false, false), // literal + literal (10%) - but we'll still try to use variables
                    _ => (true, false),  // fallback to variable + literal
                };

                let left = if left_is_variable {
                    // Try to generate variable reference of matching type
                    if let Some(variables) = external_variables {
                        let matching_vars: Vec<_> = variables
                            .iter()
                            .filter(|var| {
                                var.is_numeric()
                                    && if target_is_int {
                                        var.get_class().is_some_and(|t| t.is_integer_type())
                                    } else {
                                        var.get_class().is_some_and(|t| t.is_float_type())
                                    }
                            })
                            .collect();

                        if !matching_vars.is_empty() {
                            let variable = matching_vars.choose(rng).unwrap();
                            ArithmeticExpression::VariableReference(variable.get_name().to_string())
                        } else {
                            // Fallback to literal of target type
                            if target_is_int {
                                ArithmeticExpression::generate_random_int_literal(rng)
                            } else {
                                ArithmeticExpression::generate_random_float_literal(rng)
                            }
                        }
                    } else {
                        // Fallback to literal of target type
                        if target_is_int {
                            ArithmeticExpression::generate_random_int_literal(rng)
                        } else {
                            ArithmeticExpression::generate_random_float_literal(rng)
                        }
                    }
                } else {
                    // Generate literal of target type
                    if target_is_int {
                        ArithmeticExpression::generate_random_int_literal(rng)
                    } else {
                        ArithmeticExpression::generate_random_float_literal(rng)
                    }
                };

                let right = if right_is_variable {
                    // Try to generate variable reference of matching type
                    if let Some(variables) = external_variables {
                        let matching_vars: Vec<_> = variables
                            .iter()
                            .filter(|v| {
                                v.is_numeric()
                                    && if target_is_int {
                                        v.get_class().is_some_and(|t| t.is_integer_type())
                                    } else {
                                        v.get_class().is_some_and(|t| t.is_float_type())
                                    }
                            })
                            .collect();

                        if !matching_vars.is_empty() {
                            let variable = matching_vars.choose(rng).unwrap();
                            ArithmeticExpression::VariableReference(variable.get_name().to_string())
                        } else {
                            // Fallback to literal of target type
                            if target_is_int {
                                ArithmeticExpression::generate_random_int_literal(rng)
                            } else {
                                ArithmeticExpression::generate_random_float_literal(rng)
                            }
                        }
                    } else {
                        // Fallback to literal of target type
                        if target_is_int {
                            ArithmeticExpression::generate_random_int_literal(rng)
                        } else {
                            ArithmeticExpression::generate_random_float_literal(rng)
                        }
                    }
                } else {
                    // Generate literal of target type
                    if target_is_int {
                        ArithmeticExpression::generate_random_int_literal(rng)
                    } else {
                        ArithmeticExpression::generate_random_float_literal(rng)
                    }
                };

                ArithmeticExpression::BinaryOp {
                    left: Box::new(left),
                    op: Operator::generate_random_operator(rng),
                    right: Box::new(right),
                }
            }

            16..=17 => {
                // Generate function call if external_functions is provided and not empty (10% probability)
                // Only use functions that return numeric types
                if let Some(functions) = external_functions {
                    let functions_borrowed = functions.borrow();
                    if !functions_borrowed.is_empty() {
                        // Filter functions that return numeric types and are NOT class methods
                        let numeric_functions: Vec<_> = functions_borrowed
                            .iter()
                            .filter(|func| func.is_numeric_function() && !func.is_method())
                            .collect();

                        if !numeric_functions.is_empty() {
                            let function = numeric_functions.choose(rng).unwrap();
                            let function_name = function.get_name().to_string();

                            // Generate arguments that match function parameter types
                            let mut args = Vec::with_capacity(function.get_parameters().len());
                            for param in function.get_parameters() {
                                let param_type = param.get_type();
                                let arg = if let Some(variables) = external_variables {
                                    // Try to find a variable of matching type first
                                    let matching_vars: Vec<_> = variables
                                        .iter()
                                        .filter(|var| var.get_class() == Some(param_type))
                                        .collect();

                                    if !matching_vars.is_empty()
                                        && rng.random_bool(PROBABILITY_USE_MATCHING_VARIABLE)
                                    {
                                        // 67% chance to use matching variable
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

                            return ArithmeticExpression::FunctionCall(function_name, args);
                        }
                    }
                }

                // Fallback to literal if no suitable functions available
                if target_is_int {
                    ArithmeticExpression::generate_random_int_literal(rng)
                } else {
                    ArithmeticExpression::generate_random_float_literal(rng)
                }
            }
            18..=19 => {
                // Generate nested binary operation with function call as one operand (10% probability - for complex nested expressions)
                // Only use functions that return numeric types
                if let Some(functions) = external_functions {
                    let functions_borrowed = functions.borrow();
                    if !functions_borrowed.is_empty() {
                        // Filter functions that return numeric types and are NOT class methods
                        let numeric_functions: Vec<_> = functions_borrowed
                            .iter()
                            .filter(|func| func.is_numeric_function() && !func.is_method())
                            .collect();

                        if !numeric_functions.is_empty() {
                            let function = numeric_functions.choose(rng).unwrap();
                            let function_name = function.get_name().to_string();

                            // Generate arguments that match function parameter types
                            let mut args = Vec::with_capacity(function.get_parameters().len());
                            for param in function.get_parameters() {
                                let param_type = param.get_type();
                                let arg = if let Some(variables) = external_variables {
                                    // Try to find a variable of matching type first
                                    let matching_vars: Vec<_> = variables
                                        .iter()
                                        .filter(|var| var.get_class() == Some(param_type))
                                        .collect();

                                    if !matching_vars.is_empty()
                                        && rng.random_bool(PROBABILITY_USE_MATCHING_VARIABLE)
                                    {
                                        // 67% chance to use matching variable
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

                            let function_call =
                                ArithmeticExpression::FunctionCall(function_name, args);

                            // Decide whether function call should be left or right operand
                            if rng.random_bool(PROBABILITY_FUNCTION_CALL_ON_LHS) {
                                ArithmeticExpression::BinaryOp {
                                    left: Box::new(function_call),
                                    op: Operator::generate_random_operator(rng),
                                    right: Box::new(Self::generate_typed_expression(
                                        max_depth - 1,
                                        target_is_int,
                                        Some(functions.clone()),
                                        external_variables,
                                        rng,
                                    )),
                                }
                            } else {
                                ArithmeticExpression::BinaryOp {
                                    left: Box::new(Self::generate_typed_expression(
                                        max_depth - 1,
                                        target_is_int,
                                        Some(functions.clone()),
                                        external_variables,
                                        rng,
                                    )),
                                    op: Operator::generate_random_operator(rng),
                                    right: Box::new(function_call),
                                }
                            }
                        } else {
                            // No numeric functions available, fallback to simple binary operation
                            let left = if target_is_int {
                                ArithmeticExpression::generate_random_int_literal(rng)
                            } else {
                                ArithmeticExpression::generate_random_float_literal(rng)
                            };
                            let right = if target_is_int {
                                ArithmeticExpression::generate_random_int_literal(rng)
                            } else {
                                ArithmeticExpression::generate_random_float_literal(rng)
                            };
                            ArithmeticExpression::BinaryOp {
                                left: Box::new(left),
                                op: Operator::generate_random_operator(rng),
                                right: Box::new(right),
                            }
                        }
                    } else {
                        // Fallback to simple binary operation
                        let left = if target_is_int {
                            ArithmeticExpression::generate_random_int_literal(rng)
                        } else {
                            ArithmeticExpression::generate_random_float_literal(rng)
                        };
                        let right = if target_is_int {
                            ArithmeticExpression::generate_random_int_literal(rng)
                        } else {
                            ArithmeticExpression::generate_random_float_literal(rng)
                        };
                        ArithmeticExpression::BinaryOp {
                            left: Box::new(left),
                            op: Operator::generate_random_operator(rng),
                            right: Box::new(right),
                        }
                    }
                } else {
                    // Fallback to simple binary operation
                    let left = if target_is_int {
                        ArithmeticExpression::generate_random_int_literal(rng)
                    } else {
                        ArithmeticExpression::generate_random_float_literal(rng)
                    };
                    let right = if target_is_int {
                        ArithmeticExpression::generate_random_int_literal(rng)
                    } else {
                        ArithmeticExpression::generate_random_float_literal(rng)
                    };
                    ArithmeticExpression::BinaryOp {
                        left: Box::new(left),
                        op: Operator::generate_random_operator(rng),
                        right: Box::new(right),
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn generate_random_expression_untyped<T: Rng + SeedableRng>(
        max_depth: usize,
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
        external_variables: Option<&[Variable]>,
        rng: &mut T,
    ) -> Self {
        if max_depth == 0 {
            // At max depth, try to generate variable reference first if available (high probability)
            // Only use numeric variables for arithmetic expressions
            if let Some(variables) = external_variables {
                let numeric_variables: Vec<_> =
                    variables.iter().filter(|var| var.is_numeric()).collect();

                if !numeric_variables.is_empty()
                    && rng.random_bool(PROBABILITY_VARIABLE_USE_AT_MAX_DEPTH)
                {
                    let variable = numeric_variables.choose(rng).unwrap();
                    return ArithmeticExpression::VariableReference(
                        variable.get_name().to_string(),
                    );
                }
            }
            return match rng.random_range(0..=1) {
                0 => ArithmeticExpression::generate_random_int_literal(rng),
                1 => ArithmeticExpression::generate_random_float_literal(rng),
                _ => unreachable!(),
            };
        }

        match rng.random_range(0..=19) {
            0..=1 => {
                // Generate literal (10% probability - reduced)
                if rng.random_bool(PROBABILITY_INT_VS_FLOAT_LITERAL) {
                    ArithmeticExpression::generate_random_int_literal(rng)
                } else {
                    ArithmeticExpression::generate_random_float_literal(rng)
                }
            }
            2..=5 => {
                // Generate variable reference if available (20% probability - reduced)
                // Only use numeric variables for arithmetic expressions
                if let Some(variables) = external_variables {
                    let numeric_variables: Vec<_> =
                        variables.iter().filter(|var| var.is_numeric()).collect();

                    if !numeric_variables.is_empty() {
                        let variable = numeric_variables.choose(rng).unwrap();
                        return ArithmeticExpression::VariableReference(
                            variable.get_name().to_string(),
                        );
                    }
                }
                // Fallback to literal (removed aggressive variable usage)
                // Last resort fallback to literal
                ArithmeticExpression::generate_random_int_literal(rng)
            }
            6..=15 => {
                // Generate binary operation with controlled operand types (50% probability - MAXIMIZED for complex expressions)
                // Ensure a good mix of variable + literal combinations
                // Use different strategies to create more variable + literal combinations
                // Increase variable + literal combinations to 80% probability
                let strategy = rng.random_range(0..10);
                let (left_is_variable, right_is_variable) = match strategy {
                    0..=3 => (true, false), // variable + literal (40%)
                    4..=7 => (false, true), // literal + variable (40%)
                    8 => (true, true),      // variable + variable (10%)
                    9 => (false, false), // literal + literal (10%) - but we'll still try to use variables
                    _ => (true, false),  // fallback to variable + literal
                };

                let left = if left_is_variable {
                    // Try to generate variable reference
                    if let Some(variables) = external_variables {
                        let numeric_variables: Vec<_> =
                            variables.iter().filter(|var| var.is_numeric()).collect();

                        if !numeric_variables.is_empty() {
                            let variable = numeric_variables.choose(rng).unwrap();
                            ArithmeticExpression::VariableReference(variable.get_name().to_string())
                        } else {
                            // Fallback to literal if no variables available
                            if rng.random_bool(PROBABILITY_INT_VS_FLOAT_LITERAL) {
                                ArithmeticExpression::generate_random_int_literal(rng)
                            } else {
                                ArithmeticExpression::generate_random_float_literal(rng)
                            }
                        }
                    } else {
                        // Fallback to literal if no variables available
                        if rng.random_bool(PROBABILITY_INT_VS_FLOAT_LITERAL) {
                            ArithmeticExpression::generate_random_int_literal(rng)
                        } else {
                            ArithmeticExpression::generate_random_float_literal(rng)
                        }
                    }
                } else {
                    // Generate literal
                    if rng.random_bool(PROBABILITY_INT_VS_FLOAT_LITERAL) {
                        ArithmeticExpression::generate_random_int_literal(rng)
                    } else {
                        ArithmeticExpression::generate_random_float_literal(rng)
                    }
                };

                let right = if right_is_variable {
                    // Try to generate variable reference
                    if let Some(variables) = external_variables {
                        let numeric_variables: Vec<_> =
                            variables.iter().filter(|var| var.is_numeric()).collect();

                        if !numeric_variables.is_empty() {
                            let variable = numeric_variables.choose(rng).unwrap();
                            ArithmeticExpression::VariableReference(variable.get_name().to_string())
                        } else {
                            // Fallback to literal if no variables available
                            if rng.random_bool(PROBABILITY_INT_VS_FLOAT_LITERAL) {
                                ArithmeticExpression::generate_random_int_literal(rng)
                            } else {
                                ArithmeticExpression::generate_random_float_literal(rng)
                            }
                        }
                    } else {
                        // Fallback to literal if no variables available
                        if rng.random_bool(PROBABILITY_INT_VS_FLOAT_LITERAL) {
                            ArithmeticExpression::generate_random_int_literal(rng)
                        } else {
                            ArithmeticExpression::generate_random_float_literal(rng)
                        }
                    }
                } else {
                    // Generate literal
                    if rng.random_bool(PROBABILITY_INT_VS_FLOAT_LITERAL) {
                        ArithmeticExpression::generate_random_int_literal(rng)
                    } else {
                        ArithmeticExpression::generate_random_float_literal(rng)
                    }
                };

                ArithmeticExpression::BinaryOp {
                    left: Box::new(left),
                    op: Operator::generate_random_operator(rng),
                    right: Box::new(right),
                }
            }
            16..=17 => {
                // Generate function call if external_functions is provided and not empty (10% probability)
                // Only use functions that return numeric types
                if let Some(functions) = external_functions {
                    let functions_borrowed = functions.borrow();
                    if !functions_borrowed.is_empty() {
                        // Filter functions that return numeric types and are NOT class methods
                        let numeric_functions: Vec<_> = functions_borrowed
                            .iter()
                            .filter(|func| func.is_numeric_function() && !func.is_method())
                            .collect();

                        if !numeric_functions.is_empty() {
                            let function = numeric_functions.choose(rng).unwrap();
                            let function_name = function.get_name().to_string();

                            // Generate arguments that match function parameter types
                            let mut args = Vec::with_capacity(function.get_parameters().len());
                            for param in function.get_parameters() {
                                let param_type = param.get_type();
                                let arg = if let Some(variables) = external_variables {
                                    // Try to find a variable of matching type first
                                    let matching_vars: Vec<_> = variables
                                        .iter()
                                        .filter(|var| var.get_class() == Some(param_type))
                                        .collect();

                                    if !matching_vars.is_empty()
                                        && rng.random_bool(PROBABILITY_USE_MATCHING_VARIABLE)
                                    {
                                        // 67% chance to use matching variable
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

                            return ArithmeticExpression::FunctionCall(function_name, args);
                        }
                    }
                }

                // Fallback to simple arithmetic expression
                ArithmeticExpression::generate_random_int_literal(rng)
            }
            18..=19 => {
                // Generate nested binary operation with function call as one operand (10% probability - for complex nested expressions)
                // Only use functions that return numeric types
                if let Some(functions) = external_functions {
                    let functions_borrowed = functions.borrow();
                    if !functions_borrowed.is_empty() {
                        // Filter functions that return numeric types and are NOT class methods
                        let numeric_functions: Vec<_> = functions_borrowed
                            .iter()
                            .filter(|func| func.is_numeric_function() && !func.is_method())
                            .collect();

                        if !numeric_functions.is_empty() {
                            let function = numeric_functions.choose(rng).unwrap();
                            let function_name = function.get_name().to_string();

                            // Generate arguments that match function parameter types
                            let mut args = Vec::with_capacity(function.get_parameters().len());
                            for param in function.get_parameters() {
                                let param_type = param.get_type();
                                let arg = if let Some(variables) = external_variables {
                                    // Try to find a variable of matching type first
                                    let matching_vars: Vec<_> = variables
                                        .iter()
                                        .filter(|var| var.get_class() == Some(param_type))
                                        .collect();

                                    if !matching_vars.is_empty()
                                        && rng.random_bool(PROBABILITY_USE_MATCHING_VARIABLE)
                                    {
                                        // 67% chance to use matching variable
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

                            let function_call =
                                ArithmeticExpression::FunctionCall(function_name, args);

                            // Decide whether function call should be left or right operand
                            if rng.random_bool(PROBABILITY_FUNCTION_CALL_ON_LHS) {
                                ArithmeticExpression::BinaryOp {
                                    left: Box::new(function_call),
                                    op: Operator::generate_random_operator(rng),
                                    right: Box::new(Self::generate_random_expression(
                                        max_depth - 1,
                                        Some(functions.clone()),
                                        external_variables,
                                        rng,
                                    )),
                                }
                            } else {
                                ArithmeticExpression::BinaryOp {
                                    left: Box::new(Self::generate_random_expression(
                                        max_depth - 1,
                                        Some(functions.clone()),
                                        external_variables,
                                        rng,
                                    )),
                                    op: Operator::generate_random_operator(rng),
                                    right: Box::new(function_call),
                                }
                            }
                        } else {
                            // No numeric functions available, fallback to simple arithmetic expression
                            ArithmeticExpression::generate_random_int_literal(rng)
                        }
                    } else {
                        // Fallback to simple arithmetic expression
                        ArithmeticExpression::generate_random_int_literal(rng)
                    }
                } else {
                    // Fallback to simple arithmetic expression
                    ArithmeticExpression::generate_random_int_literal(rng)
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn generate_random_int_literal<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        Self::Int(rng.random_range(MIN_INT_LITERAL..=MAX_INT_LITERAL))
    }

    pub fn generate_random_float_literal<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        Self::Float(OrderedFloat::from(
            rng.random::<f32>() * FLOAT_LITERAL_RANGE,
        ))
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
            ArithmeticExpression::VariableReference(var_name) => {
                write!(f, "{}", var_name)?;
                Ok(())
            }
        }
    }
}
