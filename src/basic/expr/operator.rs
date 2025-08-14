use std::fmt::Display;

use rand::random_range;

#[derive(Debug, Clone)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

impl Operator {
    pub fn generate_random_operator() -> Self {
        match random_range(0..4) {
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
