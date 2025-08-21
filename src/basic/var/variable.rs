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

    pub fn get_type(&self) -> Option<&Class> {
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
        let value = if with_initial_value {
            // Generate expression based on random type choice
            match rng.random_range(0..4) {
                0 => {
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
                1 => {
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
                2 => {
                    // Generate boolean expression
                    Some(Expression::Boolean(BooleanExpression::Literal(
                        rng.random(),
                    )))
                }
                3 => {
                    // Generate string expression
                    Some(Expression::generate_random_string_literal(rng))
                }
                _ => unreachable!(),
            }
        } else {
            None
        };
        let ty = if let Some(value) = &value {
            // Infer type from expression
            match value {
                Expression::Arithmetic(arith_expr) => {
                    // Check if it's an integer or float expression
                    if arith_expr.is_int(external_variables) {
                        Some(INT) // Use INT for integer expressions
                    } else {
                        Some(FLOAT) // Use FLOAT for float expressions
                    }
                }
                Expression::Boolean(_) => Some(BOOLEAN),
                Expression::StringLiteral(_) => Some(STRING),
                Expression::FunctionCall(_, _) => Some(FLOAT), // Function calls default to float for now
                Expression::VariableReference(var_name) => {
                    // Look up the variable type from external_variables
                    if let Some(variables) = external_variables {
                        if let Some(variable) = variables.iter().find(|v| v.get_name() == var_name)
                        {
                            variable.get_type().cloned()
                        } else {
                            None
                        }
                    } else {
                        None
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
}
