use rand::Rng;
use std::fmt::Display;

#[derive(Clone, Debug, Default)]
pub enum VariableInit {
    #[default]
    Default,

    Const,

    LateInit,
}

impl Display for VariableInit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VariableInit::Default => write!(f, "{}", Self::DEFAULT_REPR),
            VariableInit::Const => write!(f, "{} ", Self::CONST_REPR),
            VariableInit::LateInit => write!(f, "{} ", Self::LATEINIT_REPR),
        }
    }
}

impl VariableInit {
    pub const DEFAULT_REPR: &str = "";
    pub const CONST_REPR: &str = "const";
    pub const LATEINIT_REPR: &str = "lateinit";

    pub fn generate_random_variable_init(_is_stack: bool) -> Self {
        let mut rng = rand::rng();

        match rng.random_range(0..=1) {
            0 => Self::Default,
            1 => Self::Const,
            _ => unreachable!(),
        }
    }

    pub fn is_lateinit(&self) -> bool {
        matches!(self, Self::LateInit)
    }
}
