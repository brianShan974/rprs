use rand::{Rng, SeedableRng};
use std::fmt;

use crate::basic::cls::basic_types::{BasicType, FloatingPointType, NumberType, SignedIntegerType};
use crate::basic::cls::custom_class::CustomClass;

pub const BASIC_TYPES: &[Class] = &[
    Class::Basic(BasicType::Number(NumberType::SignedInteger(
        SignedIntegerType::Byte,
    ))),
    Class::Basic(BasicType::Number(NumberType::SignedInteger(
        SignedIntegerType::Short,
    ))),
    Class::Basic(BasicType::Number(NumberType::SignedInteger(
        SignedIntegerType::Int,
    ))),
    Class::Basic(BasicType::Number(NumberType::SignedInteger(
        SignedIntegerType::Long,
    ))),
    Class::Basic(BasicType::Number(NumberType::FloatingPoint(
        FloatingPointType::Float,
    ))),
    Class::Basic(BasicType::Number(NumberType::FloatingPoint(
        FloatingPointType::Double,
    ))),
    Class::Basic(BasicType::Boolean),
];

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Class {
    Basic(BasicType),
    Custom(CustomClass),
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Class::Basic(basic_type) => write!(f, "{}", basic_type),
            Class::Custom(custom_class) => write!(f, "{}", custom_class.get_name()),
        }
    }
}

impl Class {
    pub fn generate_random_class<T: Rng + SeedableRng>(
        rng: &mut T,
        defined_classes: Option<&mut Vec<Class>>,
        current_indentation_layer: Option<usize>,
    ) -> Self {
        // 70% chance for basic types, 30% chance for custom types
        match rng.random_range(0..10) {
            0..=6 => Class::Basic(BasicType::generate_random_basic_type(rng)),
            _ => Class::Custom(CustomClass::generate_random_custom_class(
                rng,
                defined_classes,
                current_indentation_layer,
            )),
        }
    }

    pub fn get_name(&self) -> String {
        match self {
            Class::Basic(basic_type) => basic_type.to_string(),
            Class::Custom(custom_class) => custom_class.get_name(),
        }
    }
}
