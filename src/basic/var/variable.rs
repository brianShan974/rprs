use std::cell::RefCell;
use std::rc::Rc;

use derive_more::Constructor;
use rand::{Rng, SeedableRng, seq::IndexedRandom};

use super::prefix::var_prefix::VariablePrefix;
use crate::basic::body::fun::function::Function;
use crate::basic::cls::basic_type::BasicType;
use crate::basic::cls::class::{BOOLEAN, Class, DOUBLE, FLOAT, INT, STRING};
use crate::basic::cls::custom_class::CustomClass;
use crate::basic::cls::number_types::number::NumberType;
use crate::basic::expr::arithmetic_expression::ArithmeticExpression;
use crate::basic::expr::boolean_expression::BooleanExpression;
use crate::basic::expr::expression::Expression;
use crate::basic::utils::generate_random_identifier;

pub type VarRef = Rc<Variable>;

#[derive(Constructor, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Variable {
    prefix: VariablePrefix,
    name: String,
    value: Option<Expression>,
    ty: Option<Rc<Class>>,
}

impl Variable {
    pub fn output_declaration(&self) -> String {
        match &self.ty {
            Some(ty) => format!("{} {}: {}", self.prefix, self.name, ty.get_name()),
            _ => format!("{} {}", self.prefix, self.name),
        }
    }

    pub fn output_assignment(&self) -> Option<String> {
        self.value
            .as_ref()
            .map(|value| format!("{} = {}", self.name, value))
    }

    pub fn output_init(&self) -> Option<String> {
        self.value.as_ref().map(|value| match &self.ty {
            Some(ty) => format!(
                "{} {}: {} = {}",
                self.prefix,
                self.name,
                ty.get_name(),
                value
            ),
            _ => format!("{} {} = {}", self.prefix, self.name, value),
        })
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn is_mutable(&self) -> bool {
        self.prefix.is_mutable()
    }

    pub fn get_prefix(&self) -> &VariablePrefix {
        &self.prefix
    }

    pub fn get_class(&self) -> Option<&Class> {
        self.ty.as_ref().map(|rc| rc.as_ref())
    }

    pub fn get_value(&self) -> Option<&Expression> {
        self.value.as_ref()
    }

    pub fn generate_random_variable<T: Rng + SeedableRng>(
        is_member: bool,
        with_initial_value: bool,
        external_variables: Option<&[Variable]>,
        rng: &mut T,
    ) -> Self {
        Self::generate_random_variable_with_const_control(
            is_member,
            with_initial_value,
            external_variables,
            true,
            rng,
        )
    }

    pub fn generate_random_variable_with_const_control<T: Rng + SeedableRng>(
        is_member: bool,
        with_initial_value: bool,
        external_variables: Option<&[Variable]>,
        allow_const: bool,
        rng: &mut T,
    ) -> Self {
        Self::generate_random_variable_with_const_control_and_functions(
            is_member,
            with_initial_value,
            external_variables,
            None, // No external functions for backward compatibility
            allow_const,
            None, // No defined classes for backward compatibility
            rng,
        )
    }

    pub fn generate_random_variable_with_const_control_and_functions<T: Rng + SeedableRng>(
        is_member: bool,
        with_initial_value: bool,
        external_variables: Option<&[Variable]>,
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
        allow_const: bool,
        defined_classes: Option<&[Class]>,
        rng: &mut T,
    ) -> Self {
        let prefix =
            VariablePrefix::generate_random_prefix_with_const_control(is_member, allow_const, rng);
        let name = generate_random_identifier(rng);
        // First determine the type, then generate matching expression
        let ty = if with_initial_value {
            // Choose type with adjusted probabilities, prioritizing custom classes
            if let Some(classes) = defined_classes {
                if !classes.is_empty() {
                    // 60% chance for custom classes, 40% for basic types
                    if rng.random_bool(0.6) {
                        // Choose a random custom class
                        let custom_class = classes.choose(rng).unwrap();
                        Some(custom_class.clone())
                    } else {
                        // Choose from basic types
                        let basic_types = [
                            Some(INT.clone()),
                            Some(INT.clone()),
                            Some(INT.clone()),
                            Some(INT.clone()),
                            Some(INT.clone()),
                            Some(INT.clone()),
                            Some(INT.clone()), // 35% Int
                            Some(FLOAT.clone()),
                            Some(FLOAT.clone()),
                            Some(FLOAT.clone()),
                            Some(FLOAT.clone()),
                            Some(FLOAT.clone()),
                            Some(FLOAT.clone()),
                            Some(FLOAT.clone()),   // 35% Float
                            Some(BOOLEAN.clone()), // 10% Boolean
                            Some(STRING.clone()),  // 10% String
                            Some(INT.clone()),
                            Some(INT.clone()),
                            Some(INT.clone()),
                            Some(INT.clone()), // 10% Int (fallback)
                        ];
                        basic_types[rng.random_range(0..20)].clone()
                    }
                } else {
                    // No custom classes available, use basic types
                    let basic_types = [
                        Some(INT.clone()),
                        Some(INT.clone()),
                        Some(INT.clone()),
                        Some(INT.clone()),
                        Some(INT.clone()),
                        Some(INT.clone()),
                        Some(INT.clone()), // 35% Int
                        Some(FLOAT.clone()),
                        Some(FLOAT.clone()),
                        Some(FLOAT.clone()),
                        Some(FLOAT.clone()),
                        Some(FLOAT.clone()),
                        Some(FLOAT.clone()),
                        Some(FLOAT.clone()),   // 35% Float
                        Some(BOOLEAN.clone()), // 10% Boolean
                        Some(STRING.clone()),  // 10% String
                        Some(INT.clone()),
                        Some(INT.clone()),
                        Some(INT.clone()),
                        Some(INT.clone()), // 10% Int (fallback)
                    ];
                    basic_types[rng.random_range(0..20)].clone()
                }
            } else {
                // No defined classes provided, use basic types
                let basic_types = [
                    Some(INT.clone()),
                    Some(INT.clone()),
                    Some(INT.clone()),
                    Some(INT.clone()),
                    Some(INT.clone()),
                    Some(INT.clone()),
                    Some(INT.clone()), // 35% Int
                    Some(FLOAT.clone()),
                    Some(FLOAT.clone()),
                    Some(FLOAT.clone()),
                    Some(FLOAT.clone()),
                    Some(FLOAT.clone()),
                    Some(FLOAT.clone()),
                    Some(FLOAT.clone()),   // 35% Float
                    Some(BOOLEAN.clone()), // 10% Boolean
                    Some(STRING.clone()),  // 10% String
                    Some(INT.clone()),
                    Some(INT.clone()),
                    Some(INT.clone()),
                    Some(INT.clone()), // 10% Int (fallback)
                ];
                basic_types[rng.random_range(0..20)].clone()
            }
        } else {
            None
        };

        let value = if with_initial_value {
            // Generate expression that matches the determined type
            match &ty {
                Some(Class::Basic(BasicType::Number(NumberType::SignedInteger(_)))) => {
                    // Generate integer arithmetic expression
                    Some(Expression::Arithmetic(
                        ArithmeticExpression::generate_typed_expression(
                            2,                          // Allow some complexity
                            true,                       // target_is_int = true
                            external_functions.clone(), // Use external functions if available
                            external_variables,
                            rng,
                        ),
                    ))
                }
                Some(FLOAT) | Some(DOUBLE) => {
                    // Generate float arithmetic expression
                    Some(Expression::Arithmetic(
                        ArithmeticExpression::generate_typed_expression(
                            2,                          // Allow some complexity
                            false,                      // target_is_int = false
                            external_functions.clone(), // Use external functions if available
                            external_variables,
                            rng,
                        ),
                    ))
                }
                Some(BOOLEAN) => {
                    // Generate boolean expression
                    Some(Expression::Boolean(BooleanExpression::Literal(
                        rng.random(),
                    )))
                }
                Some(STRING) => {
                    // Generate string expression
                    Some(Expression::generate_random_string_literal(rng))
                }
                _ => {
                    // Handle custom class types
                    if let Some(Class::Custom(custom_class)) = &ty {
                        // Generate various expressions for custom class types
                        Some(Self::generate_custom_type_expression(
                            custom_class,
                            external_variables,
                            external_functions.clone(),
                            rng,
                        ))
                    } else {
                        // Fallback to integer expression for unknown types
                        Some(Expression::Arithmetic(
                            ArithmeticExpression::generate_typed_expression(
                                2,                          // Allow some complexity
                                true,                       // target_is_int = true
                                external_functions.clone(), // Use external functions if available
                                external_variables,
                                rng,
                            ),
                        ))
                    }
                }
            }
        } else {
            None
        };

        Self {
            prefix,
            name,
            value,
            ty: ty.map(|t| Rc::new(t)),
        }
    }

    /// Generate a variable with a specific type
    pub fn generate_random_variable_with_type<T: Rng + SeedableRng>(
        is_member: bool,
        with_initial_value: bool,
        target_type: Option<Class>,
        external_variables: Option<&[Variable]>,
        rng: &mut T,
    ) -> Self {
        let prefix = VariablePrefix::generate_random_prefix(is_member, rng);
        let name = generate_random_identifier(rng);

        let (value, ty) = if with_initial_value {
            match &target_type {
                Some(Class::Basic(BasicType::Number(NumberType::SignedInteger(_)))) => {
                    // Generate integer arithmetic expression that can include variable references
                    let expr = if prefix.get_init().is_const() {
                        // For const val, only generate compile-time constants
                        Expression::Arithmetic(
                            ArithmeticExpression::generate_compile_time_constant_expression(
                                2,    // Allow some complexity
                                true, // target_is_int = true
                                external_variables,
                                rng,
                            ),
                        )
                    } else {
                        // For var/val, can generate any expression
                        Expression::Arithmetic(ArithmeticExpression::generate_typed_expression(
                            2,    // Allow some complexity
                            true, // target_is_int = true
                            None, // No external functions for variable initialization
                            external_variables,
                            rng,
                        ))
                    };
                    (Some(expr), target_type.clone().map(|t| Rc::new(t)))
                }
                Some(FLOAT) | Some(DOUBLE) => {
                    // Generate float arithmetic expression that can include variable references
                    let expr = if prefix.get_init().is_const() {
                        // For const val, only generate compile-time constants
                        Expression::Arithmetic(
                            ArithmeticExpression::generate_compile_time_constant_expression(
                                2,     // Allow some complexity
                                false, // target_is_int = false
                                external_variables,
                                rng,
                            ),
                        )
                    } else {
                        // For var/val, can generate any expression
                        Expression::Arithmetic(ArithmeticExpression::generate_typed_expression(
                            2,     // Allow some complexity
                            false, // target_is_int = false
                            None,  // No external functions for variable initialization
                            external_variables,
                            rng,
                        ))
                    };
                    (Some(expr), target_type.clone().map(|t| Rc::new(t)))
                }
                Some(BOOLEAN) => {
                    // Generate boolean expression
                    let expr = if prefix.get_init().is_const() {
                        // For const val, only generate boolean literals (compile-time constants)
                        Expression::Boolean(BooleanExpression::Literal(rng.random()))
                    } else {
                        // For var/val, can generate any boolean expression
                        Expression::Boolean(BooleanExpression::Literal(rng.random()))
                    };
                    (Some(expr), target_type.clone().map(|t| Rc::new(t)))
                }
                Some(STRING) => {
                    // Generate string literal
                    let expr = Expression::generate_random_string_literal(rng);
                    (Some(expr), target_type.clone().map(|t| Rc::new(t)))
                }
                Some(Class::Custom(custom_class)) => {
                    // Generate custom class instantiation
                    let expr = Expression::ClassInstantiation(custom_class.get_name());
                    (Some(expr), target_type.clone().map(|t| Rc::new(t)))
                }
                _ => {
                    // Default to integer arithmetic expression with possible variable references
                    let expr =
                        Expression::Arithmetic(ArithmeticExpression::generate_typed_expression(
                            2,    // Allow some complexity
                            true, // target_is_int = true (default to INT)
                            None, // No external functions for variable initialization
                            external_variables,
                            rng,
                        ));
                    let inferred_type = Some(Rc::new(INT));
                    (Some(expr), inferred_type)
                }
            }
        } else {
            (None, target_type.map(|t| Rc::new(t)))
        };

        Self {
            prefix,
            name,
            value,
            ty,
        }
    }

    pub fn is_numeric(&self) -> bool {
        self.ty.as_ref().is_some_and(|ty| ty.is_numeric_type())
    }

    pub fn is_integer(&self) -> bool {
        self.ty.as_ref().is_some_and(|ty| ty.is_integer_type())
    }

    pub fn is_float(&self) -> bool {
        self.ty.as_ref().is_some_and(|ty| ty.is_float_type())
    }

    pub fn is_boolean(&self) -> bool {
        self.ty.as_ref().is_some_and(|ty| ty.is_boolean_type())
    }

    pub fn is_string(&self) -> bool {
        self.ty.as_ref().is_some_and(|ty| ty.is_string_type())
    }

    pub fn is_const(&self) -> bool {
        self.prefix.get_init().is_const()
    }

    /// Generate various expressions for custom class types
    fn generate_custom_type_expression<T: Rng + SeedableRng>(
        custom_class: &CustomClass,
        external_variables: Option<&[Variable]>,
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
        rng: &mut T,
    ) -> Expression {
        // Choose between different ways to generate custom type expressions
        match rng.random_range(0..3) {
            0 => {
                // Constructor call (e.g., cn3())
                Expression::ClassInstantiation(custom_class.get_name())
            }
            1 => {
                // Function call that returns the same type
                if let Some(functions) = external_functions {
                    let functions = functions.borrow();
                    // Find functions that return the same custom type
                    let matching_functions: Vec<_> = functions
                        .iter()
                        .filter(|f| {
                            f.get_return_type()
                                .as_ref()
                                .map(|rt| rt.get_name() == custom_class.get_name())
                                .unwrap_or(false)
                        })
                        .collect();

                    if !matching_functions.is_empty() {
                        let chosen_function = matching_functions.choose(rng).unwrap();
                        let args = Self::generate_function_call_args(
                            chosen_function,
                            external_variables,
                            rng,
                        );
                        Expression::FunctionCall(chosen_function.get_name().to_string(), args)
                    } else {
                        // Fallback to constructor if no matching functions
                        Expression::ClassInstantiation(custom_class.get_name())
                    }
                } else {
                    // Fallback to constructor if no external functions
                    Expression::ClassInstantiation(custom_class.get_name())
                }
            }
            _ => {
                // Variable reference of the same type
                if let Some(variables) = external_variables {
                    // Find variables of the same custom type
                    let matching_variables: Vec<_> = variables
                        .iter()
                        .filter(|v| {
                            v.get_class()
                                .as_ref()
                                .map(|c| c.get_name() == custom_class.get_name())
                                .unwrap_or(false)
                        })
                        .collect();

                    if !matching_variables.is_empty() {
                        let chosen_variable = matching_variables.choose(rng).unwrap();
                        Expression::VariableReference(chosen_variable.get_name().to_string())
                    } else {
                        // Fallback to constructor if no matching variables
                        Expression::ClassInstantiation(custom_class.get_name())
                    }
                } else {
                    // Fallback to constructor if no external variables
                    Expression::ClassInstantiation(custom_class.get_name())
                }
            }
        }
    }

    /// Generate function call arguments for a given function
    fn generate_function_call_args<T: Rng + SeedableRng>(
        function: &Function,
        external_variables: Option<&[Variable]>,
        rng: &mut T,
    ) -> Vec<Expression> {
        let parameters = function.get_parameters();
        let mut args = Vec::new();

        for parameter in parameters {
            let param_type = parameter.get_type();
            let arg =
                Self::generate_expression_for_parameter_type(param_type, external_variables, rng);
            args.push(arg);
        }

        args
    }

    /// Generate expression for a specific parameter type
    fn generate_expression_for_parameter_type<T: Rng + SeedableRng>(
        param_type: &Class,
        external_variables: Option<&[Variable]>,
        rng: &mut T,
    ) -> Expression {
        match param_type {
            Class::Basic(BasicType::Number(NumberType::SignedInteger(_))) => {
                Expression::Arithmetic(ArithmeticExpression::generate_typed_expression(
                    1,    // Simple expressions for function arguments
                    true, // target_is_int = true
                    None, // No external functions for arguments
                    external_variables,
                    rng,
                ))
            }
            Class::Basic(BasicType::Number(NumberType::UnsignedInteger(_))) => {
                Expression::Arithmetic(ArithmeticExpression::generate_typed_expression(
                    1,    // Simple expressions for function arguments
                    true, // target_is_int = true
                    None, // No external functions for arguments
                    external_variables,
                    rng,
                ))
            }
            Class::Basic(BasicType::Number(NumberType::FloatingPoint(_))) => {
                Expression::Arithmetic(ArithmeticExpression::generate_typed_expression(
                    1,     // Simple expressions for function arguments
                    false, // target_is_int = false
                    None,  // No external functions for arguments
                    external_variables,
                    rng,
                ))
            }
            Class::Basic(BasicType::Boolean) => {
                Expression::Boolean(BooleanExpression::Literal(rng.random()))
            }
            Class::Basic(BasicType::String) => Expression::generate_random_string_literal(rng),
            Class::Basic(BasicType::Char) => {
                Expression::Arithmetic(ArithmeticExpression::Char(rng.random_range('a'..='z')))
            }
            Class::Custom(custom_class) => {
                // For custom type parameters, try to find matching variables first
                if let Some(variables) = external_variables {
                    let matching_variables: Vec<_> = variables
                        .iter()
                        .filter(|v| {
                            v.get_class()
                                .as_ref()
                                .map(|c| c.get_name() == custom_class.get_name())
                                .unwrap_or(false)
                        })
                        .collect();

                    if !matching_variables.is_empty() {
                        let chosen_variable = matching_variables.choose(rng).unwrap();
                        Expression::VariableReference(chosen_variable.get_name().to_string())
                    } else {
                        Expression::ClassInstantiation(custom_class.get_name())
                    }
                } else {
                    Expression::ClassInstantiation(custom_class.get_name())
                }
            }
        }
    }
}
