use derive_more::Display;
use rand::Rng;

#[derive(Clone, Debug, Default, Display)]
pub enum VariableScope {
    #[display("{}", Self::STACK_REPR)]
    #[default]
    Stack,

    #[display("{} ", Self::INTERNAL_REPR)]
    Internal,

    #[display("{} ", Self::PUBLIC_REPR)]
    Public,

    #[display("{} ", Self::PRIVATE_REPR)]
    Private,

    #[display("{} ", Self::PROTECTED_REPR)]
    Protected,
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
