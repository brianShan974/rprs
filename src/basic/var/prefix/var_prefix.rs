use derive_more::Display;

use super::{init::VariableInit, mutability::VariableMutability, scope::VariableScope};

#[derive(Debug, Default, Display)]
#[display("{scope}{init}{mutability}")]
pub struct VariablePrefix {
    scope: VariableScope,
    init: VariableInit,
    mutability: VariableMutability,
}
