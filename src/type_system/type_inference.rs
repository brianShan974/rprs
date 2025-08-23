use std::collections::HashMap;

use crate::{
    basic::cls::class::{BOOLEAN, FLOAT, INT, STRING},
    type_system::{Type, type_context::TypeContext, type_result::TypeResult},
};

/// Simplified type inference engine
#[derive(Default)]
pub struct TypeInference {
    context: TypeContext,
    type_variables: HashMap<String, Type>,
    next_var_id: u32,
}

impl TypeInference {
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
            Some(Type::Basic(INT))
        } else if value.parse::<f32>().is_ok() {
            Some(Type::Basic(FLOAT))
        } else if value == "true" || value == "false" {
            Some(Type::Basic(BOOLEAN))
        } else if value.starts_with('"') && value.ends_with('"') {
            Some(Type::Basic(STRING))
        } else {
            None
        }
    }

    /// Infer type from variable name
    pub fn infer_variable_type(&self, var_name: &str) -> TypeResult<Type> {
        self.context.get_variable_type(var_name).cloned()
    }

    /// Unify two types (find the most specific common type)
    pub fn unify_types(&mut self, t1: &Type, t2: &Type) -> TypeResult<Type> {
        if t1 == t2 {
            return Some(t1.clone());
        }

        match (t1, t2) {
            // If one is unknown, use the other
            (Type::Unknown, other) | (other, Type::Unknown) => Some(other.clone()),

            // If both are basic types, find common type
            (Type::Basic(_), Type::Basic(_)) => {
                // For simplicity, return the first type
                Some(t1.clone())
            }

            // If one is nullable and the other isn't, make both nullable
            (Type::Nullable(inner), other) | (other, Type::Nullable(inner)) => {
                if inner.as_ref() == other {
                    Some(Type::Nullable(Box::new(other.clone())))
                } else {
                    None
                }
            }

            // For other cases, return error
            _ => None,
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
