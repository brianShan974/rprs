use derive_more::Display;

use super::{init::VariableInit, mutability::VariableMutability, scope::VariableScope};

#[derive(Clone, Debug, Default, Display)]
#[display("{scope}{init}{mutability}")]
pub struct VariablePrefix {
    scope: VariableScope,
    init: VariableInit,
    mutability: VariableMutability,
}

impl VariablePrefix {
    pub fn generate_random_prefix(is_stack: bool) -> Self {
        let scope = VariableScope::generate_random_scope(is_stack);
        let init = VariableInit::generate_random_variable_init(is_stack);
        let mutability =
            VariableMutability::generate_random_variable_mutability(init.is_lateinit(), &init);

        Self {
            scope,
            init,
            mutability,
        }
    }

    pub fn is_mutable(&self) -> bool {
        use super::mutability::VariableMutability;
        matches!(self.mutability, VariableMutability::Var)
    }
}
