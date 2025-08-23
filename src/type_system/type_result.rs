use std::fmt::Display;

use crate::type_system::Type;

/// Type error information
#[derive(Clone, Debug)]
pub struct TypeError {
    pub message: String,
    pub location: String,
    pub expected: Option<Type>,
    pub found: Option<Type>,
}

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Type error at {}: {}", self.location, self.message)?;
        if let (Some(expected), Some(found)) = (&self.expected, &self.found) {
            write!(f, " (expected {}, found {})", expected, found)?;
        }
        Ok(())
    }
}

/// Result type for type checking operations
pub type TypeResult<T> = Result<T, TypeError>;
