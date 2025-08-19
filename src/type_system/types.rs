use rand::{Rng, SeedableRng};

use std::collections::HashMap;
use std::fmt::Display;

use crate::basic::cls::{basic_types::BasicType, class::Class};

/// Represents a type in the type system
#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    /// Basic types (Int, Float, Boolean, etc.)
    Basic(Class),
    /// Function type: parameters -> return_type
    Function(Vec<Type>, Box<Type>),
    /// Generic type with type parameters
    Generic(String, Vec<Type>),
    /// Union type (A | B)
    Union(Vec<Type>),
    /// Nullable type (Type?)
    Nullable(Box<Type>),
    /// Type variable for type inference
    Variable(String),
    /// Unknown type (used during type inference)
    Unknown,
    /// Error type (used for type errors)
    Error,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Basic(class) => write!(f, "{}", class),
            Type::Function(params, ret) => {
                let params_str = params
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "({}) -> {}", params_str, ret)
            }
            Type::Generic(name, params) => {
                if params.is_empty() {
                    write!(f, "{}", name)
                } else {
                    let params_str = params
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "{}<{}>", name, params_str)
                }
            }
            Type::Union(types) => {
                let types_str = types
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" | ");
                write!(f, "{}", types_str)
            }
            Type::Nullable(inner) => write!(f, "{}?", inner),
            Type::Variable(name) => write!(f, "{}", name),
            Type::Unknown => write!(f, "?"),
            Type::Error => write!(f, "Error"),
        }
    }
}

impl Type {
    /// Check if this type is a numeric type
    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::Basic(_)) // All basic types are numeric for now
    }

    /// Check if this type is a boolean type
    pub fn is_boolean(&self) -> bool {
        matches!(self, Type::Basic(Class::Basic(BasicType::Boolean)))
    }

    /// Check if this type is a function type
    pub fn is_function(&self) -> bool {
        matches!(self, Type::Function(_, _))
    }

    /// Check if this type is nullable
    pub fn is_nullable(&self) -> bool {
        matches!(self, Type::Nullable(_))
    }

    /// Get the inner type of a nullable type
    pub fn inner_type(&self) -> Option<&Type> {
        match self {
            Type::Nullable(inner) => Some(inner),
            _ => None,
        }
    }

    /// Check if two types are compatible for assignment
    pub fn is_assignable_to(&self, target: &Type) -> bool {
        if self == target {
            return true;
        }

        match (self, target) {
            // Nullable types can be assigned to their non-nullable counterparts
            (Type::Nullable(inner), target) => inner.is_assignable_to(target),

            // Any type can be assigned to a nullable version of itself
            (inner, Type::Nullable(target_inner)) => inner.is_assignable_to(target_inner),

            // Numeric types can be assigned to wider numeric types
            (Type::Basic(_), Type::Basic(_)) => true, // Simplified for now

            // Function types must match exactly
            (Type::Function(params1, ret1), Type::Function(params2, ret2)) => {
                if params1.len() != params2.len() {
                    return false;
                }
                params1.iter().zip(params2.iter()).all(|(p1, p2)| p1 == p2)
                    && ret1.is_assignable_to(ret2)
            }

            _ => false,
        }
    }

    /// Get the most specific common type between two types
    pub fn common_type<T: Rng + SeedableRng>(&self, other: &Type, rng: &mut T) -> Type {
        if self == other {
            return self.clone();
        }

        match (self, other) {
            // If one is nullable and the other isn't, return nullable
            (Type::Nullable(inner), other) | (other, Type::Nullable(inner)) => {
                if inner.as_ref() == other {
                    Type::Nullable(Box::new(other.clone()))
                } else {
                    Type::Unknown
                }
            }

            // For basic types, return the wider type
            (Type::Basic(_), Type::Basic(_)) => Type::Basic(Class::generate_random_class(rng)),

            _ => Type::Unknown,
        }
    }
}

/// Type context for storing type information
#[derive(Clone, Debug)]
pub struct TypeContext {
    /// Variable types
    variables: HashMap<String, Type>,
    /// Function signatures
    functions: HashMap<String, Type>,
    /// Type aliases
    aliases: HashMap<String, Type>,
}

impl TypeContext {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            aliases: HashMap::new(),
        }
    }

    /// Add a variable with its type
    pub fn add_variable(&mut self, name: String, ty: Type) {
        self.variables.insert(name, ty);
    }

    /// Get the type of a variable
    pub fn get_variable_type(&self, name: &str) -> Option<&Type> {
        self.variables.get(name)
    }

    /// Add a function signature
    pub fn add_function(&mut self, name: String, ty: Type) {
        self.functions.insert(name, ty);
    }

    /// Get the type of a function
    pub fn get_function_type(&self, name: &str) -> Option<&Type> {
        self.functions.get(name)
    }

    /// Add a type alias
    pub fn add_alias(&mut self, name: String, ty: Type) {
        self.aliases.insert(name, ty);
    }

    /// Resolve a type alias
    pub fn resolve_alias(&self, name: &str) -> Option<&Type> {
        self.aliases.get(name)
    }

    /// Check if a variable exists
    pub fn has_variable(&self, name: &str) -> bool {
        self.variables.contains_key(name)
    }

    /// Check if a function exists
    pub fn has_function(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }

    /// Get all variable names
    pub fn get_variable_names(&self) -> Vec<String> {
        self.variables.keys().cloned().collect()
    }

    /// Get all function names
    pub fn get_function_names(&self) -> Vec<String> {
        self.functions.keys().cloned().collect()
    }
}

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
