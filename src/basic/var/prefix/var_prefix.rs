use std::fmt::Display;

use derive_more::Constructor;
use rand::{Rng, SeedableRng};

use super::{init::VariableInit, mutability::VariableMutability, visibility::Visibility};

#[derive(Constructor, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct VariablePrefix {
    visibility: Visibility,
    init: VariableInit,
    mutability: VariableMutability,
}

impl VariablePrefix {
    pub fn generate_random_prefix<T: Rng + SeedableRng>(is_member: bool, rng: &mut T) -> Self {
        let visibility = Visibility::generate_random_visibility(is_member, rng);
        let init = VariableInit::generate_random_variable_init(is_member, rng);
        let mutability = VariableMutability::generate_random_variable_mutability(&init, rng);

        Self {
            visibility,
            init,
            mutability,
        }
    }

    pub fn is_mutable(&self) -> bool {
        matches!(self.mutability, VariableMutability::Var)
    }

    pub fn is_stack(&self) -> bool {
        self.visibility.is_default()
    }

    pub fn get_init(&self) -> &VariableInit {
        &self.init
    }

    pub fn is_val(&self) -> bool {
        matches!(self.mutability, VariableMutability::Val)
    }

    pub fn is_var(&self) -> bool {
        matches!(self.mutability, VariableMutability::Var)
    }
}

impl Display for VariablePrefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.is_stack() {
            write!(f, "{} ", self.visibility)?;
        }

        if !self.init.is_default() {
            write!(f, "{} ", self.init)?;
        }

        write!(f, "{}", self.mutability)?;

        Ok(())
    }
}
