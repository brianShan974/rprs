use std::fmt::Display;

#[derive(Debug, Default)]
pub enum VariableMutability {
    #[default]
    Val,
    Var,
}

impl VariableMutability {
    pub const VAL_REPR: &str = "val";
    pub const VAR_REPR: &str = "var";
}

impl Display for VariableMutability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Val => write!(f, "{} ", Self::VAL_REPR),
            Self::Var => write!(f, "{} ", Self::VAR_REPR),
        }
    }
}

impl From<VariableMutability> for String {
    fn from(value: VariableMutability) -> Self {
        match value {
            VariableMutability::Val => VariableMutability::VAL_REPR,
            VariableMutability::Var => VariableMutability::VAR_REPR,
        }
        .to_string()
    }
}
