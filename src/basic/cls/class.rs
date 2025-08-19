use derive_more::Display;
use rand::{Rng, SeedableRng};

use crate::basic::cls::basic_types::BasicType;
use crate::basic::cls::custom_class::CustomClass;

#[derive(Clone, Debug, Display, PartialEq, Eq, Hash)]
#[display("{}", _0)]
pub enum Class {
    Basic(BasicType),
    Custom(CustomClass),
}

impl Class {
    pub fn generate_random_class<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        // // 80% chance for basic types, 20% chance for custom types
        // match rng.random_range(0..10) {
        //     0..7 => Class::Basic(BasicType::generate_random_basic_type()),
        //     _ => Class::Custom(CustomClass),
        // }
        Class::Basic(BasicType::generate_random_basic_type(rng))
    }
}
