use derive_more::Display;
use rand::{Rng, SeedableRng};

#[derive(Clone, Debug, Display, PartialEq, Eq, Hash)]
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
    pub fn generate_random_signed_integer_type<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        match rng.random_range(0..4) {
            0 => SignedIntegerType::Byte,
            1 => SignedIntegerType::Short,
            2 => SignedIntegerType::Int,
            _ => SignedIntegerType::Long,
        }
    }
}
