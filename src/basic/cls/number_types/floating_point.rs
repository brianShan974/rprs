use derive_more::Display;
use rand::{Rng, SeedableRng};

#[derive(Clone, Debug, Display, PartialEq, Eq, Hash)]
pub enum FloatingPointType {
    #[display("Float")]
    Float,
    #[display("Double")]
    Double,
}

impl FloatingPointType {
    pub fn generate_random_floating_point_type<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        match rng.random_range(0..2) {
            0 => FloatingPointType::Float,
            _ => FloatingPointType::Double,
        }
    }
}
