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
use ordered_float::OrderedFloat;

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
        rng: &mut T,
    ) -> Self {
        let prefix = VariablePrefix::generate_random_prefix(is_member, rng);
        let name = generate_random_identifier(rng);
        let value = if with_initial_value {
            Some(Expression::generate_random_expression(5, None, rng))
        } else {
            None
        };
        let ty = if let Some(value) = &value {
            // Infer type from expression
            match value {
                Expression::Arithmetic(arith_expr) => {
                    // Check if it's an integer or float expression
                    if arith_expr.is_int() {
                        Some(INT) // Use INT for integer expressions
                    } else {
                        Some(FLOAT) // Use FLOAT for float expressions
                    }
                }
                Expression::Boolean(_) => Some(BOOLEAN),
                Expression::FunctionCall(_, _) => Some(FLOAT), // Function calls default to float for now
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
        rng: &mut T,
    ) -> Self {
        let prefix = VariablePrefix::generate_random_prefix(is_member, rng);
        let name = generate_random_identifier(rng);

        let (value, ty) = if with_initial_value {
            match &target_type {
                Some(Class::Basic(BasicType::Number(NumberType::SignedInteger(_)))) => {
                    // Generate integer expression
                    let expr = Expression::Arithmetic(ArithmeticExpression::Int(
                        rng.random_range(-100..100),
                    ));
                    (Some(expr), target_type.clone())
                }
                Some(FLOAT) | Some(DOUBLE) => {
                    // Generate float expression
                    let expr = Expression::Arithmetic(ArithmeticExpression::Float(
                        OrderedFloat::from(rng.random::<f32>() * 100.0),
                    ));
                    (Some(expr), target_type.clone())
                }
                Some(BOOLEAN) => {
                    // Generate boolean expression
                    let expr = Expression::Boolean(BooleanExpression::Literal(rng.random()));
                    (Some(expr), target_type.clone())
                }
                Some(STRING) => {
                    // For now, generate arithmetic expression (string literals not implemented)
                    let expr = Expression::Arithmetic(ArithmeticExpression::Int(
                        rng.random_range(-100..100),
                    ));
                    (Some(expr), Some(INT))
                }
                _ => {
                    // Default to arithmetic expression
                    let expr = Expression::Arithmetic(ArithmeticExpression::Int(
                        rng.random_range(-100..100),
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
