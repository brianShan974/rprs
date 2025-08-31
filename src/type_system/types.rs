use std::fmt::Display;

use crate::basic::{
    cls::{
        basic_type::BasicType,
        class::{BOOLEAN, Class, DOUBLE},
        number_types::{floating_point::FloatingPointType, number::NumberType},
    },
    utils::map_collect_join,
};

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

impl Type {
    /// Check if this type is a numeric type
    pub fn is_numeric(&self) -> bool {
        if let Self::Basic(cls) = self {
            cls.is_numeric_type()
        } else {
            false
        }
    }

    /// Check if this type is a boolean type
    pub fn is_boolean(&self) -> bool {
        matches!(self, Type::Basic(BOOLEAN))
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
            (Type::Basic(class1), Type::Basic(class2)) => {
                class1 == class2
                    || class1.is_numeric_type() && class2.is_numeric_type() &&
                    // Numeric type promotion: Int -> Float -> Double
                    matches!(
                        (class1, class2),
                        (
                            Class::Basic(BasicType::Number(NumberType::SignedInteger(_))),
                            Class::Basic(BasicType::Number(NumberType::FloatingPoint(
                                FloatingPointType::Float,
                            ))),
                        ) | (
                            Class::Basic(BasicType::Number(NumberType::SignedInteger(_))),
                            Class::Basic(BasicType::Number(NumberType::FloatingPoint(
                                FloatingPointType::Double,
                            ))),
                        ) | (
                            Class::Basic(BasicType::Number(NumberType::FloatingPoint(
                                FloatingPointType::Float,
                            ))),
                            Class::Basic(BasicType::Number(NumberType::FloatingPoint(
                                FloatingPointType::Double,
                            ))),
                        )
                    )
            }

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
    pub fn common_type(&self, other: &Type) -> Type {
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
            (Type::Basic(class1), Type::Basic(class2)) => {
                if class1 == class2 {
                    Type::Basic(class1.clone())
                } else {
                    Type::Basic(DOUBLE)
                }
            }

            _ => Type::Unknown,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Basic(class) => write!(f, "{}", class),
            Type::Function(params, ret) => {
                let params_str = map_collect_join(params, |t| t.to_string(), ", ");
                write!(f, "({}) -> {}", params_str, ret)
            }
            Type::Generic(name, params) => {
                if params.is_empty() {
                    write!(f, "{}", name)
                } else {
                    let params_str = map_collect_join(params, |t| t.to_string(), ", ");
                    write!(f, "{}<{}>", name, params_str)
                }
            }
            Type::Union(types) => {
                let types_str = map_collect_join(types, |t| t.to_string(), " | ");
                write!(f, "{}", types_str)
            }
            Type::Nullable(inner) => write!(f, "{}?", inner),
            Type::Variable(name) => write!(f, "{}", name),
            Type::Unknown => write!(f, "?"),
            Type::Error => write!(f, "Error"),
        }
    }
}
