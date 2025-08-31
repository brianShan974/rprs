use rand::{Rng, SeedableRng};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::basic::utils::select_enum_variant_with_probability;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumIter)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

impl Operator {
    pub fn generate_random_operator<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        let operators: Vec<_> = Operator::iter().collect();
        // Probability distribution: Add and Sub more common than Mul and Div
        let probabilities = [0.35, 0.35, 0.15, 0.15]; // Add, Sub, Mul, Div
        select_enum_variant_with_probability(&operators, &probabilities, rng)
            .unwrap_or(&Operator::Add)
            .clone()
    }

    pub fn get_precedence(&self) -> u8 {
        match self {
            Operator::Mul | Operator::Div => 1,
            Operator::Add | Operator::Sub => 2,
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
        }
    }
}
