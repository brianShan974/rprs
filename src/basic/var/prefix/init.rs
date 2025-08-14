use derive_more::Display;
use rand::Rng;

#[derive(Clone, Debug, Default, Display)]
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
