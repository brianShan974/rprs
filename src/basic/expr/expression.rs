use rand::seq::IndexedRandom;
use rand::{Rng, SeedableRng};

use std::cell::RefCell;
use std::fmt::{self, Display};
use std::rc::Rc;

use super::arithmetic_expression::ArithmeticExpression;
use super::boolean_expression::BooleanExpression;
use crate::basic::body::fun::function::Function;
use crate::basic::cls::basic_type::BasicType;
use crate::basic::cls::class::Class;
use crate::basic::cls::number_types::number::NumberType;
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
    ClassInstantiation(String),     // 类实例化，如 A()
    PropertyAccess(String, String), // 属性访问，如 obj.property
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Arithmetic(arith) => write!(f, "{}", arith),
            Self::Boolean(boolean) => write!(f, "{}", boolean),
            Self::StringLiteral(s) => write!(f, "\"{}\"", s),
            Self::FunctionCall(name, args) => {
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
            Self::VariableReference(var_name) => write!(f, "{}", var_name),
            Self::ClassInstantiation(class_name) => write!(f, "{}()", class_name),
            Self::PropertyAccess(object, property) => write!(f, "{}.{}", object, property),
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
        // 3% arithmetic, 3% boolean, 2% string literal, 7% function call, 2% variable reference, 8% class instantiation, 75% property access
        match rng.random_range(0..20) {
            0 => Self::Arithmetic(ArithmeticExpression::generate_random_expression(
                max_depth,
                external_functions.clone(),
                external_variables,
                rng,
            )),
            1 => Self::generate_random_string_literal(rng),
            2 => Self::Boolean(BooleanExpression::generate_random_boolean_expression(
                max_depth,
                external_functions.clone(),
                external_variables,
                rng,
            )),
            3 => {
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
                            let function = available_functions.choose(rng).unwrap();
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

                                    if !matching_vars.is_empty() && rng.random_bool(2.0 / 3.0) {
                                        // 67% chance to use matching variable
                                        let variable = matching_vars.choose(rng).unwrap();
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
            4 => {
                // Generate class instantiation if defined_classes is provided and not empty
                if let Some(classes) = defined_classes
                    && !classes.is_empty()
                {
                    // Filter for custom classes only
                    let custom_classes: Vec<_> = classes
                        .iter()
                        .filter(|class| matches!(class, Class::Custom(_)))
                        .collect();

                    if !custom_classes.is_empty() {
                        let class = custom_classes.choose(rng).unwrap();
                        if let Class::Custom(custom_class) = class {
                            return Self::ClassInstantiation(custom_class.get_name());
                        }
                    }
                }
                // Fallback to arithmetic expression if no custom classes available
                Self::Arithmetic(ArithmeticExpression::generate_random_expression(
                    max_depth,
                    None,
                    external_variables,
                    rng,
                ))
            }
            6..=7 => {
                // Generate variable reference if external_variables is provided and not empty
                if let Some(variables) = external_variables
                    && !variables.is_empty()
                {
                    let variable = variables.choose(rng).unwrap();
                    return Self::VariableReference(variable.get_name().to_string());
                }
                // Fallback to arithmetic expression if no variables available
                Self::Arithmetic(ArithmeticExpression::generate_random_expression(
                    max_depth,
                    None,
                    external_variables,
                    rng,
                ))
            }
            5..=19 => {
                // Generate property access if we have custom classes and variables
                if let (Some(classes), Some(variables)) = (defined_classes, external_variables)
                    && !classes.is_empty()
                    && !variables.is_empty()
                {
                    // Find custom classes
                    let custom_classes: Vec<_> = classes
                        .iter()
                        .filter(|class| matches!(class, Class::Custom(_)))
                        .collect();

                    if !custom_classes.is_empty() {
                        // Find variables that are custom class instances
                        let custom_vars: Vec<_> = variables
                            .iter()
                            .filter(|var| {
                                if let Some(var_type) = var.get_class() {
                                    matches!(var_type, Class::Custom(_))
                                } else {
                                    false
                                }
                            })
                            .collect();

                        if !custom_vars.is_empty() {
                            let object_var = custom_vars.choose(rng).unwrap();
                            let object_type = object_var.get_class().unwrap();

                            if let Class::Custom(custom_class) = object_type {
                                // Find public properties of this class only
                                let public_properties: Vec<_> = custom_class
                                    .properties
                                    .iter()
                                    .filter(|prop| {
                                        // Check if property is public
                                        prop.get_prefix().get_visibility().is_public()
                                    })
                                    .collect();

                                if !public_properties.is_empty() {
                                    let property = public_properties.choose(rng).unwrap();
                                    return Self::PropertyAccess(
                                        object_var.get_name().to_string(),
                                        property.get_name().to_string(),
                                    );
                                }
                            }
                        }
                    }
                }
                // Fallback to variable reference if no property access possible
                if let Some(variables) = external_variables
                    && !variables.is_empty()
                {
                    let variable = variables.choose(rng).unwrap();
                    return Self::VariableReference(variable.get_name().to_string());
                }
                // Final fallback to arithmetic expression
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

        // Generate random characters from the safe set
        for _ in 0..length {
            chars.push(SAFE_CHARS.choose(rng).unwrap());
        }

        let string = chars.into_iter().collect::<String>();
        Self::StringLiteral(string)
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
            Self::ClassInstantiation(_) => false, // 类实例化不是整数类型
            Self::PropertyAccess(_, _) => false,  // 属性访问需要根据属性类型判断
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
            Self::ClassInstantiation(_) => false, // 类实例化不是浮点类型
            Self::PropertyAccess(_, _) => false,  // 属性访问需要根据属性类型判断
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
                    true, // target_is_int = true
                    external_functions,
                    external_variables,
                    rng,
                ))
            }
            Class::Basic(BasicType::Number(NumberType::FloatingPoint(_))) => {
                // Generate float arithmetic expression
                Self::Arithmetic(ArithmeticExpression::generate_typed_expression(
                    max_depth,
                    false, // target_is_int = false
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
                // Generate char expression (for now, generate a random char)
                Self::StringLiteral(format!("'{}'", (rng.random_range(32..127) as u8) as char))
            }
            Class::Custom(custom_class) => {
                // Generate class instantiation for custom classes
                Self::ClassInstantiation(custom_class.get_name())
            }
            _ => {
                // Fallback to integer expression for unknown types
                Self::Arithmetic(ArithmeticExpression::generate_typed_expression(
                    max_depth,
                    true, // target_is_int = true
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
