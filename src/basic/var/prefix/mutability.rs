use derive_more::Display;
use rand::{Rng, SeedableRng};

use super::init::VariableInit;

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Display)]
pub enum VariableMutability {
    #[default]
    #[display("val")]
    Val,
    #[display("var")]
    Var,
}

impl VariableMutability {
    pub fn generate_random_variable_mutability<T: Rng + SeedableRng>(
        init: &VariableInit,
        rng: &mut T,
    ) -> Self {
        if matches!(init, VariableInit::LateInit) {
            return Self::Var;
        }

        // If init is Const, mutability must be Val
        if matches!(init, VariableInit::Const) {
            return Self::Val;
        }

        match rng.random_range(0..=1) {
            0 => Self::Val,
            1 => Self::Var,
            _ => unreachable!(),
        }
    }
}
