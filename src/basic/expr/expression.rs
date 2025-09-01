use rand::seq::IndexedRandom;
use rand::{Rng, SeedableRng};

use std::cell::RefCell;
use std::fmt::{self, Display};
use std::rc::Rc;

use super::arithmetic_expression::ArithmeticExpression;
use super::boolean_expression::BooleanExpression;
use crate::basic::body::fun::function::Function;
use crate::basic::body::fun::parameter::Parameter;
use crate::basic::cls::basic_type::BasicType;
use crate::basic::cls::class::Class;
use crate::basic::cls::number_types::number::NumberType;
use crate::basic::utils::{format_function_call, format_method_call, format_property_access};
use crate::basic::var::variable::Variable;

pub const SAFE_CHARS: &[char] = &[
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
    'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '0', '1', '2', '3', '4',
    '5', '6', '7', '8', '9', ' ', '!', '#', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/',
    ':', ';', '<', '=', '>', '?', '@', '[', ']', '^', '_', '|', '~',
];

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expression {
    Arithmetic(ArithmeticExpression),
    Boolean(BooleanExpression),
    StringLiteral(String),
    FunctionCall(String, Vec<Expression>),
    VariableReference(String),
    ClassInstantiation(String),
    PropertyAccess(String, String),
    MethodCall(String, String, Vec<Expression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Arithmetic(arith) => write!(f, "{}", arith),
            Self::Boolean(boolean) => write!(f, "{}", boolean),
            Self::StringLiteral(s) => write!(f, "\"{}\"", s),
            Self::FunctionCall(name, args) => {
                write!(f, "{}", format_function_call(name, args))
            }
            Self::VariableReference(var_name) => write!(f, "{}", var_name),
            Self::ClassInstantiation(class_name) => write!(f, "{}()", class_name),
            Self::PropertyAccess(object, property) => {
                write!(f, "{}", format_property_access(object, property))
            }
            Self::MethodCall(object, method, args) => {
                write!(f, "{}", format_method_call(object, method, args))
            }
        }
    }
}

impl Expression {
    pub fn generate_random_expression<T: Rng + SeedableRng>(
        max_depth: usize,
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
        external_variables: Option<&[Variable]>,
        defined_classes: Option<&[Class]>,
        rng: &mut T,
    ) -> Self {
        // Generate different types of expressions based on probability
        match rng.random_range(0..=9) {
            0..=2 => {
                // Arithmetic expressions (30% probability)
                Self::Arithmetic(ArithmeticExpression::generate_random_expression_untyped(
                    max_depth,
                    external_functions,
                    external_variables,
                    rng,
                ))
            }
            3 => {
                // Boolean expressions (10% probability)
                Self::Boolean(BooleanExpression::generate_random_boolean_expression(
                    max_depth,
                    external_functions,
                    external_variables,
                    rng,
                ))
            }
            4 => {
                // String expressions (10% probability)
                Self::generate_random_string_literal(rng)
            }
            5 => {
                // Function calls (10% probability)
                if let Some(functions) = &external_functions {
                    let functions = functions.borrow();
                    if !functions.is_empty() {
                        let function = functions.choose(rng).unwrap();
                        let args = Self::generate_function_call_args(
                            function.get_parameters(),
                            max_depth.saturating_sub(1),
                            external_functions.clone(),
                            external_variables,
                            rng,
                        );
                        Self::FunctionCall(function.get_name().to_string(), args)
                    } else {
                        Self::generate_random_string_literal(rng)
                    }
                } else {
                    Self::generate_random_string_literal(rng)
                }
            }
            6..=8 => {
                // Method calls (10% probability) - if we have classes and methods
                if let Some(classes) = defined_classes {
                    let class_methods: Vec<_> = classes
                        .iter()
                        .filter_map(|class| {
                            if let Class::Custom(custom_class) = class {
                                Some(custom_class.get_methods())
                            } else {
                                None
                            }
                        })
                        .flatten()
                        .collect();

                    if !class_methods.is_empty() {
                        let method = class_methods.choose(rng).unwrap();
                        let args = Self::generate_function_call_args(
                            method.get_parameters(),
                            max_depth.saturating_sub(1),
                            external_functions.clone(),
                            external_variables,
                            rng,
                        );
                        // Use a simple object name for method calls
                        if let Some(variables) = external_variables {
                            let objects: Vec<_> = variables
                                .iter()
                                .filter(|v| {
                                    v.get_class().is_some_and(|c| matches!(c, Class::Custom(_)))
                                })
                                .collect();
                            if !objects.is_empty() {
                                let object_name =
                                    objects.choose(rng).unwrap().get_name().to_string();
                                Self::MethodCall(object_name, method.get_name().to_string(), args)
                            } else {
                                // No objects available, fallback to string literal
                                Self::generate_random_string_literal(rng)
                            }
                        } else {
                            // No external variables, fallback to string literal
                            Self::generate_random_string_literal(rng)
                        }
                    } else {
                        Self::generate_random_string_literal(rng)
                    }
                } else {
                    Self::generate_random_string_literal(rng)
                }
            }
            9 => {
                // Property access (10% probability)
                if let Some(variables) = external_variables {
                    let objects: Vec<_> = variables
                        .iter()
                        .filter(|v| v.get_class().is_some_and(|c| matches!(c, Class::Custom(_))))
                        .collect();
                    if !objects.is_empty() {
                        let object = objects.choose(rng).unwrap();

                        // Find the class of this object to get its real properties
                        let object_class = object.get_class();
                        if let Some(Class::Custom(custom_class)) = object_class {
                            // Get real properties from the class - only public properties
                            let public_properties: Vec<_> = custom_class
                                .properties
                                .iter()
                                .filter(|prop| prop.get_prefix().get_visibility().is_public())
                                .collect();
                            if !public_properties.is_empty() {
                                let property = public_properties.choose(rng).unwrap();
                                let property_name = property.get_name().to_string();
                                Self::PropertyAccess(object.get_name().to_string(), property_name)
                            } else {
                                Self::generate_random_string_literal(rng)
                            }
                        } else {
                            Self::generate_random_string_literal(rng)
                        }
                    } else {
                        Self::generate_random_string_literal(rng)
                    }
                } else {
                    Self::generate_random_string_literal(rng)
                }
            }
            _ => unreachable!(),
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
        let length = rng.random_range(3..=10);
        let mut chars = Vec::with_capacity(length);
        for _ in 0..length {
            chars.push(SAFE_CHARS.choose(rng).unwrap());
        }
        Self::StringLiteral(chars.into_iter().collect::<String>())
    }

    /// Generate function call arguments based on parameter types
    pub fn generate_function_call_args<T: Rng + SeedableRng>(
        parameters: &[Parameter],
        max_depth: usize,
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
        external_variables: Option<&[Variable]>,
        rng: &mut T,
    ) -> Vec<Self> {
        parameters
            .iter()
            .map(|param| {
                Self::generate_expression_for_type(
                    param.get_type(),
                    max_depth,
                    external_functions.clone(),
                    external_variables,
                    rng,
                )
            })
            .collect()
    }

    /// Check if this expression is primarily an integer type
    pub fn is_int(
        &self,
        external_variables: Option<&[Variable]>,
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
    ) -> bool {
        match self {
            Self::Arithmetic(arith) => arith.is_int(external_variables),
            Self::Boolean(_) => false,
            Self::StringLiteral(_) => false,
            Self::ClassInstantiation(_) => false,
            Self::PropertyAccess(_, _) => false,
            Self::MethodCall(_, _, _) => false,
            Self::FunctionCall(func_name, _) => {
                // For function calls, look up the actual function return type
                if let Some(functions) = external_functions
                    && let Some(func) = functions
                        .borrow()
                        .iter()
                        .find(|f| f.get_name() == func_name)
                    && let Some(return_type) = func.get_return_type()
                {
                    return return_type.is_integer_type();
                }
                false
            }
            Self::VariableReference(var_name) => {
                // Look up the variable type from external_variables
                if let Some(variables) = external_variables
                    && let Some(variable) = variables.iter().find(|v| v.get_name() == var_name)
                    && let Some(var_type) = variable.get_class()
                {
                    return var_type.is_integer_type();
                }
                false
            }
        }
    }

    /// Check if this expression is primarily a float type
    pub fn is_float(
        &self,
        external_variables: Option<&[Variable]>,
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
    ) -> bool {
        match self {
            Self::Arithmetic(arith) => arith.is_float(external_variables),
            Self::Boolean(_) => false,
            Self::StringLiteral(_) => false,
            Self::ClassInstantiation(_) => false,
            Self::PropertyAccess(_, _) => false,
            Self::MethodCall(_, _, _) => false,
            Self::FunctionCall(func_name, _) => {
                // For function calls, look up the actual function return type
                if let Some(functions) = external_functions
                    && let Some(func) = functions
                        .borrow()
                        .iter()
                        .find(|f| f.get_name() == func_name)
                    && let Some(return_type) = func.get_return_type()
                {
                    // Float type if it's a floating point number type
                    return return_type.is_float_type();
                }
                false
            }
            Self::VariableReference(var_name) => {
                // Look up the variable type from external_variables
                if let Some(variables) = external_variables
                    && let Some(variable) = variables.iter().find(|v| v.get_name() == var_name)
                    && let Some(var_type) = variable.get_class()
                {
                    return !var_type.is_integer_type(); // Float if not integer
                }
                false
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
                Self::Arithmetic(ArithmeticExpression::generate_typed_expression(
                    max_depth,
                    true,
                    external_functions,
                    external_variables,
                    rng,
                ))
            }
            Class::Basic(BasicType::Number(NumberType::FloatingPoint(_))) => {
                // Generate float arithmetic expression
                Self::Arithmetic(ArithmeticExpression::generate_typed_expression(
                    max_depth,
                    false,
                    external_functions,
                    external_variables,
                    rng,
                ))
            }
            Class::Basic(BasicType::Boolean) => {
                // Generate boolean expression
                Self::Boolean(BooleanExpression::generate_random_boolean_expression(
                    max_depth,
                    external_functions,
                    external_variables,
                    rng,
                ))
            }
            Class::Basic(BasicType::String) => {
                // Generate string expression
                Self::generate_random_string_literal(rng)
            }
            Class::Basic(BasicType::Char) => {
                // Generate char expression as arithmetic expression with char literal
                let char_value = rng.random_range(32..127) as u8 as char;
                Self::Arithmetic(ArithmeticExpression::Char(char_value))
            }
            Class::Custom(custom_class) => {
                // Generate class instantiation for custom classes
                Self::ClassInstantiation(custom_class.get_name())
            }
            _ => {
                // Fallback to integer expression for unknown types
                Self::Arithmetic(ArithmeticExpression::generate_typed_expression(
                    max_depth,
                    true,
                    external_functions,
                    external_variables,
                    rng,
                ))
            }
        }
    }

    pub fn generate_random_int_literal<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        Self::Arithmetic(ArithmeticExpression::generate_random_int_literal(rng))
    }

    pub fn generate_random_float_literal<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        Self::Arithmetic(ArithmeticExpression::generate_random_float_literal(rng))
    }

    pub fn generate_random_double_literal<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        Self::Arithmetic(ArithmeticExpression::generate_random_double_literal(rng))
    }

    pub fn generate_random_boolean_literal<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        Self::Boolean(BooleanExpression::generate_random_boolean_literal(rng))
    }
}
