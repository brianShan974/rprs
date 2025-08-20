use derive_more::Display;
use rand::{Rng, SeedableRng};

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Display)]
pub enum Visibility {
    #[default]
    #[display("")]
    Default,
    #[display("internal")]
    Internal,
    #[display("public")]
    Public,
    #[display("private")]
    Private,
    #[display("protected")]
    Protected,
}

impl Visibility {
    pub fn generate_random_visibility<T: Rng + SeedableRng>(is_member: bool, rng: &mut T) -> Self {
        if !is_member {
            return Self::Default;
        }

        match rng.random_range(1..=4) {
            1 => Self::Internal,
            2 => Self::Public,
            3 => Self::Private,
            4 => Self::Protected,
            _ => unreachable!(),
        }
    }

    pub fn is_default(&self) -> bool {
        matches!(self, Self::Default)
    }
}
