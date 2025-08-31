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
        let types = [Self::Byte, Self::Short, Self::Int, Self::Long];
        types[rng.random_range(0..4)].clone()
    }
}
