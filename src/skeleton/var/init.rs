use std::fmt::Display;

#[derive(Debug, Default)]
pub enum VariableInit {
    #[default]
    Default,
    Const,
    LateInit,
}

impl VariableInit {
    pub const DEFAULT_REPR: &str = "";
    pub const CONST_REPR: &str = "const";
    pub const LATEINIT_REPR: &str = "lateinit";
}

impl Display for VariableInit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Default => write!(f, ""),
            Self::Const => write!(f, "{} ", Self::CONST_REPR),
            Self::LateInit => write!(f, "{} ", Self::LATEINIT_REPR),
        }
    }
}

impl From<VariableInit> for String {
    fn from(value: VariableInit) -> Self {
        match value {
            VariableInit::Default => VariableInit::DEFAULT_REPR,
            VariableInit::Const => VariableInit::CONST_REPR,
            VariableInit::LateInit => VariableInit::LATEINIT_REPR,
        }
        .to_string()
    }
}
