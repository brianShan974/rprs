use rand::Rng;
use std::fmt::Display;

#[derive(Clone, Debug, Default)]
pub enum VariableScope {
    #[default]
    Stack,

    Internal,

    Public,

    Private,

    Protected,
}

impl Display for VariableScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VariableScope::Stack => write!(f, "{}", Self::STACK_REPR),
            VariableScope::Internal => write!(f, "{} ", Self::INTERNAL_REPR),
            VariableScope::Public => write!(f, "{} ", Self::PUBLIC_REPR),
            VariableScope::Private => write!(f, "{} ", Self::PRIVATE_REPR),
            VariableScope::Protected => write!(f, "{} ", Self::PROTECTED_REPR),
        }
    }
}

impl VariableScope {
    pub const STACK_REPR: &str = "";
    pub const INTERNAL_REPR: &str = "internal";
    pub const PUBLIC_REPR: &str = "public";
    pub const PRIVATE_REPR: &str = "private";
    pub const PROTECTED_REPR: &str = "protected";

    pub fn generate_random_scope(is_stack: bool) -> Self {
        if is_stack {
            return Self::Stack;
        }

        let mut rng = rand::rng();

        match rng.random_range(1..=4) {
            1 => Self::Internal,
            2 => Self::Public,
            3 => Self::Private,
            4 => Self::Protected,
            _ => unreachable!(),
        }
    }
}
