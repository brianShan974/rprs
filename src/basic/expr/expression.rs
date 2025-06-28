use std::fmt::Display;

use derive_more::Constructor;

#[derive(Debug, Constructor, Clone)]
pub struct Expression;

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}
