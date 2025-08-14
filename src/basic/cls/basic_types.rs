use derive_more::Display;
use rand::Rng;

#[derive(Clone, Debug, Display)]
pub enum BasicType {
    Number(NumberType),
    #[display("Boolean")]
    Boolean,
    #[display("Char")]
    Char,
    #[display("String")]
    String,
}

impl BasicType {
    pub fn generate_random_basic_type() -> Self {
        // let mut rng = rand::rng();

        // match rng.random_range(0..4) {
        //     0..=1 => BasicType::Number(NumberType::generate_random_number_type()),
        //     2 => BasicType::Boolean,
        //     3 => BasicType::Char,
        //     _ => BasicType::String,
        // }
        BasicType::Number(NumberType::generate_random_number_type())
    }
}

#[derive(Clone, Debug, Display)]
#[display("{}", _0)]
pub enum NumberType {
    SignedInteger(SignedIntegerType),
    UnsignedInteger(UnsignedIntegerType),
    FloatingPoint(FloatingPointType),
}

impl NumberType {
    pub fn generate_random_number_type() -> Self {
        let mut rng = rand::rng();

        match rng.random_range(0..3) {
            0 => {
                NumberType::SignedInteger(SignedIntegerType::generate_random_signed_integer_type())
            }
            1 => NumberType::UnsignedInteger(
                UnsignedIntegerType::generate_random_unsigned_integer_type(),
            ),
            _ => {
                NumberType::FloatingPoint(FloatingPointType::generate_random_floating_point_type())
            }
        }
    }
}

#[derive(Clone, Debug, Display)]
pub enum SignedIntegerType {
    #[display("Byte")]
    Byte,
    #[display("Short")]
    Short,
    #[display("Int")]
    Int,
    #[display("Long")]
    Long,
}

impl SignedIntegerType {
    pub fn generate_random_signed_integer_type() -> Self {
        let mut rng = rand::rng();

        match rng.random_range(0..4) {
            0 => SignedIntegerType::Byte,
            1 => SignedIntegerType::Short,
            2 => SignedIntegerType::Int,
            _ => SignedIntegerType::Long,
        }
    }
}

#[derive(Clone, Debug, Display)]
pub enum UnsignedIntegerType {
    #[display("UByte")]
    UByte,
    #[display("UShort")]
    UShort,
    #[display("UInt")]
    UInt,
    ULong,
}

impl UnsignedIntegerType {
    pub fn generate_random_unsigned_integer_type() -> Self {
        let mut rng = rand::rng();

        match rng.random_range(0..4) {
            0 => UnsignedIntegerType::UByte,
            1 => UnsignedIntegerType::UShort,
            2 => UnsignedIntegerType::UInt,
            _ => UnsignedIntegerType::ULong,
        }
    }
}

#[derive(Clone, Debug, Display)]
pub enum FloatingPointType {
    #[display("Float")]
    Float,
    #[display("Double")]
    Double,
}

impl FloatingPointType {
    pub fn generate_random_floating_point_type() -> Self {
        let mut rng = rand::rng();

        match rng.random_range(0..2) {
            0 => FloatingPointType::Float,
            _ => FloatingPointType::Double,
        }
    }
}
