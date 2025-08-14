use super::init::VariableInit;
use derive_more::Display;
use rand::Rng;

#[derive(Clone, Debug, Default, Display)]
pub enum VariableMutability {
    #[display("{} ", Self::VAL_REPR)]
    #[default]
    Val,

    #[display("{} ", Self::VAR_REPR)]
    Var,
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
