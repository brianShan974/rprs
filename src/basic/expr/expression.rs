use rand::{Rng, SeedableRng};

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use super::arithmetic_expression::ArithmeticExpression;
use super::boolean_expression::BooleanExpression;
use crate::basic::body::fun::function::Function;
use crate::basic::var::variable::Variable;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expression {
    Arithmetic(ArithmeticExpression),
    Boolean(BooleanExpression),
    FunctionCall(String, Vec<Expression>),
    VariableReference(String),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Arithmetic(arith) => write!(f, "{}", arith),
            Expression::Boolean(boolean) => write!(f, "{}", boolean),
            Expression::FunctionCall(name, args) => {
                if args.is_empty() {
                    write!(f, "{}()", name)
                } else {
                    let args_str = args
                        .iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "{}({})", name, args_str)
                }
            }
            Expression::VariableReference(var_name) => write!(f, "{}", var_name),
        }
    }
}

impl Expression {
    pub fn generate_random_expression<T: Rng + SeedableRng>(
        max_depth: usize,
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
        external_variables: Option<&[Variable]>,
        rng: &mut T,
    ) -> Self {
        // 10% arithmetic, 10% boolean, 25% function call, 45% variable reference, 10% fallback
        match rng.random_range(0..20) {
            0..=1 => Self::Arithmetic(ArithmeticExpression::generate_random_expression(
                max_depth,
                external_functions.clone(),
                external_variables,
                rng,
            )),
            3..=4 => Self::Boolean(BooleanExpression::generate_random_boolean_expression(
                max_depth, rng,
            )),
            5..=9 => {
                // Generate function call if external_functions is provided and not empty
                if let Some(functions) = external_functions {
                    let functions_borrowed = functions.borrow();
                    if !functions_borrowed.is_empty() {
                        let function =
                            &functions_borrowed[rng.random_range(0..functions_borrowed.len())];
                        let function_name = function.get_name().to_string();

                        // Generate random arguments (0-3 expressions)
                        let num_args = rng.random_range(0..=3);
                        let mut args = Vec::with_capacity(num_args);
                        for _ in 0..num_args {
                            args.push(Self::generate_random_expression(
                                max_depth.saturating_sub(1),
                                Some(functions.clone()),
                                external_variables,
                                rng,
                            ));
                        }

                        return Self::FunctionCall(function_name, args);
                    }
                }
                // Fallback to arithmetic expression if no functions available
                Self::Arithmetic(ArithmeticExpression::generate_random_expression(
                    max_depth,
                    None,
                    external_variables,
                    rng,
                ))
            }
            10..=18 => {
                // Generate variable reference if external_variables is provided and not empty
                if let Some(variables) = external_variables {
                    if !variables.is_empty() {
                        let variable = &variables[rng.random_range(0..variables.len())];
                        return Self::VariableReference(variable.get_name().to_string());
                    }
                }
                // Fallback to arithmetic expression if no variables available
                Self::Arithmetic(ArithmeticExpression::generate_random_expression(
                    max_depth,
                    None,
                    external_variables,
                    rng,
                ))
            }
            _ => {
                // Fallback to arithmetic expression
                Self::Arithmetic(ArithmeticExpression::generate_random_expression(
                    max_depth,
                    None,
                    external_variables,
                    rng,
                ))
            }
        }
    }

    pub fn is_arithmetic(&self) -> bool {
        matches!(self, Self::Arithmetic(_))
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, Self::Boolean(_))
    }

    pub fn is_function_call(&self) -> bool {
        matches!(self, Self::FunctionCall(_, _))
    }

    /// Check if this expression is primarily an integer type
    pub fn is_int(
        &self,
        external_variables: Option<&[crate::basic::var::variable::Variable]>,
    ) -> bool {
        match self {
            Self::Arithmetic(arith) => arith.is_int(external_variables),
            Self::Boolean(_) => false,
            Self::FunctionCall(_, _) => false, // Function calls are not considered int by default
            Self::VariableReference(var_name) => {
                // Look up the variable type from external_variables
                if let Some(variables) = external_variables {
                    if let Some(variable) = variables.iter().find(|v| v.get_name() == var_name) {
                        if let Some(var_type) = variable.get_type() {
                            return var_type.is_integer_type();
                        }
                    }
                }
                false // Default to false if variable not found or type unknown
            }
        }
    }

    /// Check if this expression is primarily a float type
    pub fn is_float(
        &self,
        external_variables: Option<&[crate::basic::var::variable::Variable]>,
    ) -> bool {
        match self {
            Self::Arithmetic(arith) => arith.is_float(external_variables),
            Self::Boolean(_) => false,
            Self::FunctionCall(_, _) => false, // Function calls are not considered float by default
            Self::VariableReference(var_name) => {
                // Look up the variable type from external_variables
                if let Some(variables) = external_variables {
                    if let Some(variable) = variables.iter().find(|v| v.get_name() == var_name) {
                        if let Some(var_type) = variable.get_type() {
                            return !var_type.is_integer_type(); // Float if not integer
                        }
                    }
                }
                false // Default to false if variable not found or type unknown
            }
        }
    }

    /// Check if this expression is a compile-time constant
    pub fn is_compile_time_constant(
        &self,
        external_variables: Option<&[crate::basic::var::variable::Variable]>,
    ) -> bool {
        match self {
            Self::Arithmetic(arith) => arith.is_compile_time_constant(external_variables),
            Self::Boolean(_) => true, // Boolean literals are compile-time constants
            Self::FunctionCall(_, _) => false, // Function calls are not compile-time constants
            Self::VariableReference(var_name) => {
                // Only const val variables are compile-time constants
                if let Some(variables) = external_variables {
                    if let Some(variable) = variables.iter().find(|v| v.get_name() == var_name) {
                        // Check if the variable is a const val
                        return variable.get_prefix().get_init().is_const();
                    }
                }
                false // Default to false if variable not found
            }
        }
    }
}
