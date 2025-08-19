use std::collections::HashMap;

use crate::{
    basic::cls::{
        basic_types::{BasicType, FloatingPointType, NumberType, SignedIntegerType},
        class::Class,
    },
    type_system::{Type, TypeContext, TypeError, TypeResult},
};

/// Simplified type inference engine
pub struct TypeInference {
    context: TypeContext,
    type_variables: HashMap<String, Type>,
    next_var_id: u32,
}

impl TypeInference {
    pub fn new() -> Self {
        Self {
            context: TypeContext::new(),
            type_variables: HashMap::new(),
            next_var_id: 0,
        }
    }

    pub fn with_context(context: TypeContext) -> Self {
        Self {
            context,
            type_variables: HashMap::new(),
            next_var_id: 0,
        }
    }

    /// Generate a new type variable
    pub fn new_type_variable(&mut self) -> Type {
        let var_name = format!("T{}", self.next_var_id);
        self.next_var_id += 1;
        let var_type = Type::Variable(var_name.clone());
        self.type_variables.insert(var_name, var_type.clone());
        var_type
    }

    /// Infer a basic type from a literal value
    pub fn infer_literal_type(&self, value: &str) -> TypeResult<Type> {
        // Simple type inference based on string representation
        if value.parse::<i32>().is_ok() {
            Ok(Type::Basic(Class::Basic(BasicType::Number(
                NumberType::SignedInteger(SignedIntegerType::Int),
            ))))
        } else if value.parse::<f32>().is_ok() {
            Ok(Type::Basic(Class::Basic(BasicType::Number(
                NumberType::FloatingPoint(FloatingPointType::Float),
            ))))
        } else if value == "true" || value == "false" {
            Ok(Type::Basic(Class::Basic(BasicType::Boolean)))
        } else if value.starts_with('"') && value.ends_with('"') {
            Ok(Type::Basic(Class::Basic(BasicType::String)))
        } else {
            Err(TypeError {
                message: format!("Cannot infer type for literal: {}", value),
                location: "type inference".to_string(),
                expected: None,
                found: None,
            })
        }
    }

    /// Infer type from variable name
    pub fn infer_variable_type(&self, var_name: &str) -> TypeResult<Type> {
        self.context
            .get_variable_type(var_name)
            .cloned()
            .ok_or_else(|| TypeError {
                message: format!("Variable '{}' not found", var_name),
                location: "type inference".to_string(),
                expected: None,
                found: None,
            })
    }

    /// Unify two types (find the most specific common type)
    pub fn unify_types(&mut self, t1: &Type, t2: &Type) -> TypeResult<Type> {
        if t1 == t2 {
            return Ok(t1.clone());
        }

        match (t1, t2) {
            // If one is unknown, use the other
            (Type::Unknown, other) | (other, Type::Unknown) => Ok(other.clone()),

            // If both are basic types, find common type
            (Type::Basic(_), Type::Basic(_)) => {
                // For simplicity, return the first type
                Ok(t1.clone())
            }

            // If one is nullable and the other isn't, make both nullable
            (Type::Nullable(inner), other) | (other, Type::Nullable(inner)) => {
                if inner.as_ref() == other {
                    Ok(Type::Nullable(Box::new(other.clone())))
                } else {
                    Err(TypeError {
                        message: format!("Cannot unify types {} and {}", t1, t2),
                        location: "type unification".to_string(),
                        expected: None,
                        found: None,
                    })
                }
            }

            // For other cases, return error
            _ => Err(TypeError {
                message: format!("Cannot unify types {} and {}", t1, t2),
                location: "type unification".to_string(),
                expected: None,
                found: None,
            }),
        }
    }

    /// Infer function type from parameters and return type
    pub fn infer_function_type(&mut self, param_types: Vec<Type>, return_type: Type) -> Type {
        Type::Function(param_types, Box::new(return_type))
    }

    /// Get the current type context
    pub fn get_context(&self) -> &TypeContext {
        &self.context
    }

    /// Get a mutable reference to the type context
    pub fn get_context_mut(&mut self) -> &mut TypeContext {
        &mut self.context
    }

    /// Add a type constraint
    pub fn add_constraint(&mut self, var_name: &str, constraint: Type) {
        self.type_variables.insert(var_name.to_string(), constraint);
    }

    /// Solve type constraints and return the final types
    pub fn solve_constraints(&self) -> HashMap<String, Type> {
        self.type_variables.clone()
    }
}
