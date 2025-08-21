use derive_more::Display;
use rand::{Rng, SeedableRng};

use crate::basic::cls::number_types::number::NumberType;

#[derive(Clone, Debug, Display, PartialEq, Eq, Hash)]
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
    pub fn generate_random_basic_type<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        // match rng.random_range(0..4) {
        //     0..=1 => BasicType::Number(NumberType::generate_random_number_type()),
        //     2 => BasicType::Boolean,
        //     3 => BasicType::Char,
        //     _ => BasicType::String,
        // }
        BasicType::Number(NumberType::generate_random_number_type(rng))
    }
}
