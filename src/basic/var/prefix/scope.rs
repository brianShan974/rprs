use derive_more::Display;

#[derive(Debug, Default, Display)]
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
}
