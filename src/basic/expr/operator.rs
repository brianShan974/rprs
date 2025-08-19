use std::fmt::Display;

use rand::{Rng, SeedableRng};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

impl Operator {
    pub fn generate_random_operator<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        match rng.random_range(0..4) {
            0 => Self::Add,
            1 => Self::Sub,
            2 => Self::Mul,
            _ => Self::Div,
        }
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
