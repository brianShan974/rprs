use rand::{Rng, SeedableRng};
use std::fmt;

use crate::basic::{
    cls::custom_class::CustomClass, expr::expression::Expression, var::variable::Variable,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ObjectInstance {
    pub class_name: String,
    pub variable_name: String,
    pub properties: Vec<ObjectProperty>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ObjectProperty {
    pub name: String,
    pub value: Expression,
}

impl ObjectInstance {
    pub fn new(class_name: String, variable_name: String) -> Self {
        Self {
            class_name,
            variable_name,
            properties: Vec::new(),
        }
    }

    pub fn add_property(&mut self, property: ObjectProperty) {
        self.properties.push(property);
    }

    pub fn generate_random_object_instance<T: Rng + SeedableRng>(
        custom_class: &CustomClass,
        rng: &mut T,
    ) -> Self {
        let variable_name = Self::generate_random_variable_name(rng);
        let mut object = Self::new(custom_class.get_name().to_string(), variable_name);

        // Generate property values for each class property
        for class_property in &custom_class.properties {
            let property_value = Self::generate_property_value(class_property, rng);
            object.add_property(ObjectProperty {
                name: class_property.get_name().to_string(),
                value: property_value,
            });
        }

        object
    }

    fn generate_random_variable_name<T: Rng + SeedableRng>(rng: &mut T) -> String {
        // Generate a completely random variable name
        let length = rng.random_range(3..=8);
        let mut name = String::with_capacity(length);

        // First character should be lowercase
        name.push((rng.random_range(97..=122) as u8) as char);

        // Rest of the characters can be lowercase letters
        for _ in 1..length {
            name.push((rng.random_range(97..=122) as u8) as char);
        }

        name
    }

    fn generate_property_value<T: Rng + SeedableRng>(
        class_property: &Variable,
        rng: &mut T,
    ) -> Expression {
        // For now, generate simple expressions based on property type
        // In a full implementation, this would use the typed generation system
        match class_property.get_type() {
            Some(crate::basic::cls::class::Class::Basic(
                crate::basic::cls::basic_types::BasicType::Number(number_type),
            )) => match number_type {
                crate::basic::cls::basic_types::NumberType::SignedInteger(_) => {
                    Expression::Arithmetic(
                        crate::basic::expr::arithmetich_expression::ArithmeticExpression::Int(
                            rng.random_range(-100..=100),
                        ),
                    )
                }
                crate::basic::cls::basic_types::NumberType::UnsignedInteger(_) => {
                    Expression::Arithmetic(
                        crate::basic::expr::arithmetich_expression::ArithmeticExpression::Int(
                            rng.random_range(0..=100),
                        ),
                    )
                }
                crate::basic::cls::basic_types::NumberType::FloatingPoint(_) => {
                    Expression::Arithmetic(
                        crate::basic::expr::arithmetich_expression::ArithmeticExpression::Float(
                            ordered_float::OrderedFloat::from(rng.random::<f32>() * 100.0),
                        ),
                    )
                }
            },
            Some(crate::basic::cls::class::Class::Basic(
                crate::basic::cls::basic_types::BasicType::Boolean,
            )) => Expression::Boolean(
                crate::basic::expr::boolean_expression::BooleanExpression::Literal(
                    rng.random_range(0..=1) == 0,
                ),
            ),
            Some(crate::basic::cls::class::Class::Basic(
                crate::basic::cls::basic_types::BasicType::String,
            )) => {
                // For now, generate a simple string literal
                Expression::Arithmetic(
                    crate::basic::expr::arithmetich_expression::ArithmeticExpression::Int(
                        rng.random_range(0..=100),
                    ),
                )
            }
            Some(crate::basic::cls::class::Class::Basic(
                crate::basic::cls::basic_types::BasicType::Char,
            )) => Expression::Arithmetic(
                crate::basic::expr::arithmetich_expression::ArithmeticExpression::Int(
                    rng.random_range(0..=100),
                ),
            ),
            _ => Expression::Arithmetic(
                crate::basic::expr::arithmetich_expression::ArithmeticExpression::Int(
                    rng.random_range(0..=100),
                ),
            ),
        }
    }

    pub fn get_class_name(&self) -> &str {
        &self.class_name
    }

    pub fn get_variable_name(&self) -> &str {
        &self.variable_name
    }
}

impl fmt::Display for ObjectInstance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.variable_name)
    }
}

impl ObjectProperty {
    pub fn new(name: String, value: Expression) -> Self {
        Self { name, value }
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_value(&self) -> &Expression {
        &self.value
    }
}

impl fmt::Display for ObjectProperty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.name, self.value)
    }
}
