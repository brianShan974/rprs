use std::rc::Rc;

use derive_more::Constructor;
use rand::{Rng, SeedableRng};

use super::prefix::var_prefix::VariablePrefix;
use crate::basic::cls::basic_type::BasicType;
use crate::basic::cls::class::{BOOLEAN, Class, DOUBLE, FLOAT, INT, STRING};
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
    ty: Option<Class>,
}

impl Variable {
    pub fn output_declaration(&self) -> String {
        match self.ty {
            Some(ref ty) => format!("{} {}: {}", self.prefix, self.name, ty.get_name()),
            _ => format!("{} {}", self.prefix, self.name),
        }
    }

    pub fn output_assignment(&self) -> Option<String> {
        self.value
            .as_ref()
            .map(|value| format!("{} = {}", self.name, value))
    }

    pub fn output_init(&self) -> Option<String> {
        self.value.as_ref().map(|value| match self.ty {
            Some(ref ty) => format!(
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
        self.ty.as_ref()
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
        let prefix =
            VariablePrefix::generate_random_prefix_with_const_control(is_member, allow_const, rng);
        let name = generate_random_identifier(rng);
        // First determine the type, then generate matching expression
        let ty = if with_initial_value {
            // Choose type with adjusted probabilities
            match rng.random_range(0..20) {
                0..=6 => Some(INT),    // 35% probability for Int
                7..=13 => Some(FLOAT), // 35% probability for Float
                14 => Some(BOOLEAN),   // 5% probability for Boolean
                15 => Some(STRING),    // 5% probability for String
                16..=19 => {
                    // 20% probability for custom class
                    Some(INT) // We'll handle custom class in value generation
                }
                _ => unreachable!(),
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
                            2,    // Allow some complexity
                            true, // target_is_int = true
                            None, // No external functions for variable initialization
                            external_variables,
                            rng,
                        ),
                    ))
                }
                Some(FLOAT) | Some(DOUBLE) => {
                    // Generate float arithmetic expression
                    Some(Expression::Arithmetic(
                        ArithmeticExpression::generate_typed_expression(
                            2,     // Allow some complexity
                            false, // target_is_int = false
                            None,  // No external functions for variable initialization
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
                    // Check if we should generate a custom class instantiation
                    if rng.random_bool(0.3) {
                        // 30% chance to try to generate a custom class instantiation
                        // We'll need to find a custom class from somewhere
                        // For now, fallback to integer expression
                        Some(Expression::Arithmetic(
                            ArithmeticExpression::generate_typed_expression(
                                2,    // Allow some complexity
                                true, // target_is_int = true
                                None, // No external functions for variable initialization
                                external_variables,
                                rng,
                            ),
                        ))
                    } else {
                        // Fallback to integer expression
                        Some(Expression::Arithmetic(
                            ArithmeticExpression::generate_typed_expression(
                                2,    // Allow some complexity
                                true, // target_is_int = true
                                None, // No external functions for variable initialization
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
            ty,
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
                    (Some(expr), target_type.clone())
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
                    (Some(expr), target_type.clone())
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
                    (Some(expr), target_type.clone())
                }
                Some(STRING) => {
                    // For now, generate integer arithmetic expression (string literals not implemented)
                    let expr =
                        Expression::Arithmetic(ArithmeticExpression::generate_typed_expression(
                            2,    // Allow some complexity
                            true, // target_is_int = true (fallback to INT)
                            None, // No external functions for variable initialization
                            external_variables,
                            rng,
                        ));
                    (Some(expr), Some(INT))
                }
                Some(Class::Custom(custom_class)) => {
                    // Generate custom class instantiation
                    let expr = Expression::ClassInstantiation(custom_class.get_name());
                    (Some(expr), target_type.clone())
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
                    let inferred_type = Some(INT);
                    (Some(expr), inferred_type)
                }
            }
        } else {
            (None, target_type)
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
}
