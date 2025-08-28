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

        match rng.random_range(1..=10) {
            1..=6 => Self::Public,     // 60% chance for public
            7 => Self::Internal,       // 10% chance for internal
            8 => Self::Private,        // 10% chance for private
            9..=10 => Self::Protected, // 20% chance for protected
            _ => unreachable!(),
        }
    }

    pub fn is_default(&self) -> bool {
        matches!(self, Self::Default)
    }

    pub fn is_public(&self) -> bool {
        matches!(self, Self::Public)
    }
}
