use derive_more::Display;
use rand::{Rng, SeedableRng};

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Display)]
pub enum VariableInit {
    #[default]
    #[display("")]
    Default,
    #[display("const")]
    Const,
    #[display("lateinit")]
    LateInit,
}

impl VariableInit {
    pub fn generate_random_variable_init<T: Rng + SeedableRng>(
        is_member: bool,
        rng: &mut T,
    ) -> Self {
        if is_member {
            return Self::Default;
        }

        match rng.random_range(0..=1) {
            0 => Self::Default,
            1 => Self::Const,
            _ => unreachable!(),
        }
    }

    pub fn is_default(&self) -> bool {
        matches!(self, Self::Default)
    }

    pub fn is_const(&self) -> bool {
        matches!(self, Self::Const)
    }
}
