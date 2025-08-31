use derive_more::Display;
use rand::{Rng, SeedableRng};

use crate::basic::cls::basic_type::BasicType;
use crate::basic::cls::custom_class::CustomClass;
use crate::basic::cls::number_types::floating_point::FloatingPointType;
use crate::basic::cls::number_types::number::NumberType;
use crate::basic::cls::number_types::signed_integer::SignedIntegerType;

pub const BYTE: Class = Class::Basic(BasicType::Number(NumberType::SignedInteger(
    SignedIntegerType::Byte,
)));
pub const SHORT: Class = Class::Basic(BasicType::Number(NumberType::SignedInteger(
    SignedIntegerType::Short,
)));
pub const INT: Class = Class::Basic(BasicType::Number(NumberType::SignedInteger(
    SignedIntegerType::Int,
)));
pub const LONG: Class = Class::Basic(BasicType::Number(NumberType::SignedInteger(
    SignedIntegerType::Long,
)));
pub const FLOAT: Class = Class::Basic(BasicType::Number(NumberType::FloatingPoint(
    FloatingPointType::Float,
)));
pub const DOUBLE: Class = Class::Basic(BasicType::Number(NumberType::FloatingPoint(
    FloatingPointType::Double,
)));
pub const BOOLEAN: Class = Class::Basic(BasicType::Boolean);
pub const CHAR: Class = Class::Basic(BasicType::Char);
pub const STRING: Class = Class::Basic(BasicType::String);

pub const BASIC_TYPES: &[Class] = &[BYTE, SHORT, INT, LONG, FLOAT, DOUBLE, BOOLEAN, CHAR, STRING];

#[derive(Clone, Debug, PartialEq, Eq, Hash, Display)]
pub enum Class {
    #[display("{_0}")]
    Basic(BasicType),
    #[display("{}", _0.get_name())]
    Custom(CustomClass),
}

impl Class {
    pub fn generate_random_class<T: Rng + SeedableRng>(
        rng: &mut T,
        defined_classes: Option<&mut Vec<Self>>,
        current_indentation_layer: Option<usize>,
    ) -> Self {
        // 70% chance for basic types, 30% chance for custom types
        if rng.random_range(0..10) <= 6 {
            Self::Basic(BasicType::generate_random_basic_type(rng))
        } else {
            Self::Custom(CustomClass::generate_random_custom_class(
                rng,
                defined_classes,
                current_indentation_layer,
                None, // existing_names - use default
            ))
        }
    }

    pub fn get_name(&self) -> String {
        match self {
            Self::Basic(basic_type) => basic_type.to_string(),
            Self::Custom(custom_class) => custom_class.get_name(),
        }
    }

    pub fn is_integer_type(&self) -> bool {
        use BasicType::Number;
        use NumberType::SignedInteger;
        use NumberType::UnsignedInteger;
        matches!(
            self,
            Self::Basic(Number(SignedInteger(_))) | Self::Basic(Number(UnsignedInteger(_)))
        )
    }

    pub fn is_signed_integer_type(&self) -> bool {
        use BasicType::Number;
        use NumberType::SignedInteger;
        matches!(self, Self::Basic(Number(SignedInteger(_))))
    }

    pub fn is_numeric_type(&self) -> bool {
        use BasicType::Number;
        matches!(self, Self::Basic(Number(_)))
    }

    pub fn is_float_type(&self) -> bool {
        self == &FLOAT || self == &DOUBLE
    }

    pub fn is_boolean_type(&self) -> bool {
        self == &BOOLEAN
    }

    pub fn is_string_type(&self) -> bool {
        self == &STRING
    }
}
