use derive_more::Constructor;
use rand::{Rng, SeedableRng};

use super::prefix::var_prefix::VariablePrefix;
use crate::basic::cls::basic_types::{BasicType, FloatingPointType, NumberType};
use crate::basic::cls::class::Class;
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
            Some(ref ty) => format!("{}{}: {}", self.prefix, self.name, ty),
            _ => format!("{}{}", self.prefix, self.name),
        }
    }

    pub fn output_assignment(&self) -> Option<String> {
        self.value
            .as_ref()
            .map(|value| format!("{} = {}", self.name, value))
    }

    pub fn output_init(&self) -> Option<String> {
        self.value
            .as_ref()
            .map(|value| format!("{}{} = {}", self.prefix, self.name, value))
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn is_mutable(&self) -> bool {
        self.prefix.is_mutable()
    }

    pub fn get_type(&self) -> Option<&Class> {
        self.ty.as_ref()
    }

    pub fn get_value(&self) -> Option<&Expression> {
        self.value.as_ref()
    }

    pub fn generate_random_variable<T: Rng + SeedableRng>(
        is_stack: bool,
        with_initial_value: bool,
        rng: &mut T,
    ) -> Self {
        let prefix = VariablePrefix::generate_random_prefix(is_stack);
        let name = generate_random_identifier(rng);
        let value = if with_initial_value {
            Some(Expression::generate_random_expression(5, rng))
        } else {
            None
        };
        let ty = if let Some(value) = &value
            && value.is_arithmetic()
        {
            Some(Class::Basic(BasicType::Number(NumberType::FloatingPoint(
                FloatingPointType::Float,
            ))))
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
        is_stack: bool,
        with_initial_value: bool,
        target_type: Option<Class>,
        rng: &mut T,
    ) -> Self {
        let prefix = VariablePrefix::generate_random_prefix(is_stack);
        let name = generate_random_identifier(rng);

        let (value, ty) = if with_initial_value {
            match &target_type {
                Some(Class::Basic(BasicType::Number(NumberType::SignedInteger(_)))) => {
                    // Generate integer expression
                    let expr = Expression::generate_random_expression(3, rng);
                    (Some(expr), target_type.clone())
                }
                Some(Class::Basic(BasicType::Number(NumberType::FloatingPoint(_)))) => {
                    // Generate float expression
                    let expr = Expression::generate_random_expression(3, rng);
                    (Some(expr), target_type.clone())
                }
                Some(Class::Basic(BasicType::Boolean)) => {
                    // Generate boolean expression (comparison or literal)
                    let expr = Expression::generate_random_expression(2, rng);
                    (Some(expr), target_type.clone())
                }
                Some(Class::Basic(BasicType::String)) => {
                    // For now, generate arithmetic expression (string literals not implemented)
                    let expr = Expression::generate_random_expression(2, rng);
                    (
                        Some(expr),
                        Some(Class::Basic(BasicType::Number(NumberType::FloatingPoint(
                            FloatingPointType::Float,
                        )))),
                    )
                }
                _ => {
                    // Default to arithmetic expression
                    let expr = Expression::generate_random_expression(3, rng);
                    let inferred_type = Some(Class::Basic(BasicType::Number(
                        NumberType::FloatingPoint(FloatingPointType::Float),
                    )));
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
