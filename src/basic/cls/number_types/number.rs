use derive_more::Display;
use rand::{Rng, SeedableRng};

use crate::basic::cls::number_types::{
    floating_point::FloatingPointType, signed_integer::SignedIntegerType,
    unsigned_integer::UnsignedIntegerType,
};

#[derive(Clone, Debug, Display, PartialEq, Eq, Hash)]
#[display("{}", _0)]
pub enum NumberType {
    SignedInteger(SignedIntegerType),
    UnsignedInteger(UnsignedIntegerType),
    FloatingPoint(FloatingPointType),
}

impl NumberType {
    pub fn generate_random_number_type<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        // match rng.random_range(0..3) {
        //     0 => {
        //         NumberType::SignedInteger(SignedIntegerType::generate_random_signed_integer_type())
        //     }
        //     1 => NumberType::UnsignedInteger(
        //         UnsignedIntegerType::generate_random_unsigned_integer_type(),
        //     ),
        //     _ => {
        //         NumberType::FloatingPoint(FloatingPointType::generate_random_floating_point_type())
        //     }
        // }

        match rng.random_range(0..=1) {
            0 => NumberType::SignedInteger(SignedIntegerType::generate_random_signed_integer_type(
                rng,
            )),
            _ => NumberType::FloatingPoint(FloatingPointType::generate_random_floating_point_type(
                rng,
            )),
        }
    }
}
