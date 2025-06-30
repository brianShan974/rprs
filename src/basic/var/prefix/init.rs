use derive_more::Display;

#[derive(Debug, Default, Display)]
#[display("{_variant} ")]
pub enum VariableInit {
    #[display("{}", Self::DEFAULT_REPR)]
    #[default]
    Default,

    #[display("{} ", Self::CONST_REPR)]
    Const,

    #[display("{} ", Self::LATEINIT_REPR)]
    LateInit,
}

impl VariableInit {
    pub const DEFAULT_REPR: &str = "";
    pub const CONST_REPR: &str = "const";
    pub const LATEINIT_REPR: &str = "lateinit";
}
