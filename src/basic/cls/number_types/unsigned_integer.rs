use derive_more::Display;
use rand::{Rng, SeedableRng};

#[derive(Clone, Debug, Display, PartialEq, Eq, Hash)]
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
    pub fn generate_random_unsigned_integer_type<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        match rng.random_range(0..4) {
            0 => UnsignedIntegerType::UByte,
            1 => UnsignedIntegerType::UShort,
            2 => UnsignedIntegerType::UInt,
            _ => UnsignedIntegerType::ULong,
        }
    }
}
