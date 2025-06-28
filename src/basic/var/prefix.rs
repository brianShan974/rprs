use std::fmt::Display;

use super::{init::VariableInit, mutability::VariableMutability, scope::VariableScope};

#[derive(Debug, Default)]
pub struct VariablePrefix {
    scope: VariableScope,
    init: VariableInit,
    mutability: VariableMutability,
}

impl Display for VariablePrefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}{}", self.scope, self.init, self.mutability)
    }
}
