use std::fmt::Display;

#[derive(Debug, Default)]
pub enum VariableScope {
    #[default]
    Stack,
    Internal,
    Public,
    Private,
    Protected,
}

impl VariableScope {
    pub const STACK_REPR: &str = "";
    pub const INTERNAL_REPR: &str = "internal";
    pub const PUBLIC_REPR: &str = "public";
    pub const PRIVATE_REPR: &str = "private";
    pub const PROTECTED_REPR: &str = "protected";
}

impl Display for VariableScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stack => write!(f, ""),
            Self::Internal => write!(f, "{} ", Self::INTERNAL_REPR),
            Self::Public => write!(f, "{} ", Self::PUBLIC_REPR),
            Self::Private => write!(f, "{} ", Self::PRIVATE_REPR),
            Self::Protected => write!(f, "{} ", Self::PROTECTED_REPR),
        }
    }
}

impl From<VariableScope> for String {
    fn from(value: VariableScope) -> Self {
        match value {
            VariableScope::Stack => VariableScope::STACK_REPR,
            VariableScope::Internal => VariableScope::INTERNAL_REPR,
            VariableScope::Public => VariableScope::PUBLIC_REPR,
            VariableScope::Private => VariableScope::PRIVATE_REPR,
            VariableScope::Protected => VariableScope::PROTECTED_REPR,
        }
        .to_string()
    }
}
