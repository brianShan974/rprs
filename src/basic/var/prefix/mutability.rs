use super::init::VariableInit;
use rand::Rng;
use std::fmt::Display;

#[derive(Clone, Debug, Default)]
pub enum VariableMutability {
    #[default]
    Val,

    Var,
}

impl Display for VariableMutability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VariableMutability::Val => write!(f, "{} ", Self::VAL_REPR),
            VariableMutability::Var => write!(f, "{} ", Self::VAR_REPR),
        }
    }
}

impl VariableMutability {
    pub const VAL_REPR: &str = "val";
    pub const VAR_REPR: &str = "var";

    pub fn generate_random_variable_mutability(is_lateinit: bool, init: &VariableInit) -> Self {
        if is_lateinit {
            return Self::Var;
        }

        // If init is Const, mutability must be Val
        if matches!(init, VariableInit::Const) {
            return Self::Val;
        }

        let mut rng = rand::rng();

        match rng.random_range(0..=1) {
            0 => Self::Val,
            1 => Self::Var,
            _ => unreachable!(),
        }
    }
}
