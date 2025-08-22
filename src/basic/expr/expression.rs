use rand::{Rng, SeedableRng};

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use super::arithmetic_expression::ArithmeticExpression;
use super::boolean_expression::BooleanExpression;
use crate::basic::body::fun::function::Function;
use crate::basic::cls::basic_type::BasicType;
use crate::basic::cls::class::Class;
use crate::basic::cls::number_types::number::NumberType;
use crate::basic::var::variable::Variable;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expression {
    Arithmetic(ArithmeticExpression),
    Boolean(BooleanExpression),
    StringLiteral(String),
    FunctionCall(String, Vec<Expression>),
    VariableReference(String),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Arithmetic(arith) => write!(f, "{}", arith),
            Expression::Boolean(boolean) => write!(f, "{}", boolean),
            Expression::StringLiteral(s) => write!(f, "\"{}\"", s),
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
        // 5% arithmetic, 5% boolean, 3% string literal, 20% function call, 55% variable reference, 12% fallback
        match rng.random_range(0..20) {
            0 => Self::Arithmetic(ArithmeticExpression::generate_random_expression(
                max_depth,
                external_functions.clone(),
                external_variables,
                rng,
            )),
            1 => Self::generate_random_string_literal(rng),
            2..=3 => Self::Boolean(BooleanExpression::generate_random_boolean_expression(
                max_depth,
                external_functions.clone(),
                external_variables,
                rng,
            )),
            4..=7 => {
                // Generate function call if external_functions is provided and not empty
                if let Some(functions) = external_functions {
                    let functions_borrowed = functions.borrow();
                    if !functions_borrowed.is_empty() {
                        // Only allow top-level functions to avoid cross-class method calls
                        let available_functions: Vec<_> = functions_borrowed
                            .iter()
                            .filter(|func| !func.is_class_method())
                            .collect();

                        if !available_functions.is_empty() {
                            let function = &available_functions
                                [rng.random_range(0..available_functions.len())];
                            let function_name = function.get_name().to_string();

                            // Generate arguments that match function parameter types
                            let mut args = Vec::with_capacity(function.get_parameters().len());
                            for param in function.get_parameters() {
                                let param_type = param.get_type();
                                let arg = if let Some(variables) = external_variables {
                                    // Try to find a variable of matching type first
                                    let matching_vars: Vec<_> = variables
                                        .iter()
                                        .filter(|var| {
                                            if let Some(var_type) = var.get_type() {
                                                var_type == param_type
                                            } else {
                                                false
                                            }
                                        })
                                        .collect();

                                    if !matching_vars.is_empty() && rng.random_range(0..3) < 2 {
                                        // 67% chance to use matching variable
                                        let variable = &matching_vars
                                            [rng.random_range(0..matching_vars.len())];
                                        Self::VariableReference(variable.get_name().to_string())
                                    } else {
                                        // Generate expression of matching type
                                        Self::generate_expression_for_type(
                                            param_type,
                                            max_depth.saturating_sub(1),
                                            Some(functions.clone()),
                                            external_variables,
                                            rng,
                                        )
                                    }
                                } else {
                                    // Generate expression of matching type
                                    Self::generate_expression_for_type(
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
                // Fallback to arithmetic expression if no functions available
                Self::Arithmetic(ArithmeticExpression::generate_random_expression(
                    max_depth,
                    None,
                    external_variables,
                    rng,
                ))
            }
            8..=14 => {
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

    /// Generate a random string literal
    pub fn generate_random_string_literal<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        // Generate a random string with 3-10 characters
        let length = rng.random_range(3..=10);
        let mut chars = Vec::with_capacity(length);

        // Define safe characters to use (excluding problematic ones)
        // Exclude: " (34), $ (36), \ (92), ` (96), { (123), } (125), and some other problematic chars
        let safe_chars = [
            // Letters (a-z, A-Z)
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q',
            'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
            'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y',
            'Z', // Numbers (0-9)
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', // Safe symbols
            ' ', '!', '#', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<',
            '=', '>', '?', '@', '[', ']', '^', '_', '|', '~',
        ];

        // Generate random characters from the safe set
        for _ in 0..length {
            let char_index = rng.random_range(0..safe_chars.len());
            chars.push(safe_chars[char_index]);
        }

        let string = chars.into_iter().collect::<String>();
        Self::StringLiteral(string)
    }

    /// Check if this expression is primarily an integer type
    pub fn is_int(
        &self,
        external_variables: Option<&[crate::basic::var::variable::Variable]>,
        external_functions: Option<&Rc<RefCell<Vec<Function>>>>,
    ) -> bool {
        match self {
            Self::Arithmetic(arith) => arith.is_int(external_variables),
            Self::Boolean(_) => false,
            Self::StringLiteral(_) => false,
            Self::FunctionCall(func_name, _) => {
                // For function calls, look up the actual function return type
                if let Some(functions) = external_functions {
                    if let Some(func) = functions
                        .borrow()
                        .iter()
                        .find(|f| f.get_name() == func_name)
                    {
                        if let Some(return_type) = func.get_return_type() {
                            return return_type.is_integer_type();
                        }
                    }
                }
                false // Default to false if function not found or has no return type
            }
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
        external_functions: Option<&Rc<RefCell<Vec<Function>>>>,
    ) -> bool {
        match self {
            Self::Arithmetic(arith) => arith.is_float(external_variables),
            Self::Boolean(_) => false,
            Self::StringLiteral(_) => false,
            Self::FunctionCall(func_name, _) => {
                // For function calls, look up the actual function return type
                if let Some(functions) = external_functions {
                    if let Some(func) = functions
                        .borrow()
                        .iter()
                        .find(|f| f.get_name() == func_name)
                    {
                        if let Some(return_type) = func.get_return_type() {
                            // Float type if it's a floating point number type
                            return matches!(
                                return_type,
                                crate::basic::cls::class::Class::Basic(
                                    crate::basic::cls::basic_type::BasicType::Number(
                                        crate::basic::cls::number_types::number::NumberType::FloatingPoint(_)
                                    )
                                )
                            );
                        }
                    }
                }
                false // Default to false if function not found or has no return type
            }
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
            Self::StringLiteral(_) => true, // String literals are compile-time constants
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

    /// Generate an expression that matches a specific type
    pub fn generate_expression_for_type<T: Rng + SeedableRng>(
        target_type: &Class,
        max_depth: usize,
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
        external_variables: Option<&[Variable]>,
        rng: &mut T,
    ) -> Self {
        match target_type {
            Class::Basic(BasicType::Number(NumberType::SignedInteger(_))) => {
                // Generate integer arithmetic expression
                Expression::Arithmetic(ArithmeticExpression::generate_typed_expression(
                    max_depth,
                    true, // target_is_int = true
                    external_functions,
                    external_variables,
                    rng,
                ))
            }
            Class::Basic(BasicType::Number(NumberType::FloatingPoint(_))) => {
                // Generate float arithmetic expression
                Expression::Arithmetic(ArithmeticExpression::generate_typed_expression(
                    max_depth,
                    false, // target_is_int = false
                    external_functions,
                    external_variables,
                    rng,
                ))
            }
            Class::Basic(BasicType::Boolean) => {
                // Generate boolean expression
                Expression::Boolean(BooleanExpression::generate_random_boolean_expression(
                    max_depth,
                    external_functions,
                    external_variables,
                    rng,
                ))
            }
            Class::Basic(BasicType::String) => {
                // Generate string expression
                Expression::generate_random_string_literal(rng)
            }
            Class::Basic(BasicType::Char) => {
                // Generate char expression (for now, generate a random char)
                Expression::StringLiteral(format!(
                    "'{}'",
                    (rng.random_range(32..127) as u8) as char
                ))
            }
            _ => {
                // Fallback to integer expression for unknown types
                Expression::Arithmetic(ArithmeticExpression::generate_typed_expression(
                    max_depth,
                    true, // target_is_int = true
                    external_functions,
                    external_variables,
                    rng,
                ))
            }
        }
    }
}
