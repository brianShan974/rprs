use crate::type_system::{Type, TypeContext, TypeError, TypeResult};

/// Simplified type checker for validating type safety
pub struct TypeChecker {
    context: TypeContext,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            context: TypeContext::new(),
        }
    }

    pub fn with_context(context: TypeContext) -> Self {
        Self { context }
    }

    /// Check if a type is valid
    pub fn check_type(&self, ty: &Type) -> TypeResult<()> {
        match ty {
            Type::Basic(_) => Ok(()),
            Type::Function(params, ret) => {
                for param in params {
                    self.check_type(param)?;
                }
                self.check_type(ret)
            }
            Type::Generic(_, params) => {
                for param in params {
                    self.check_type(param)?;
                }
                Ok(())
            }
            Type::Union(types) => {
                for ty in types {
                    self.check_type(ty)?;
                }
                Ok(())
            }
            Type::Nullable(inner) => self.check_type(inner),
            Type::Variable(_) => Ok(()),
            Type::Unknown => Ok(()),
            Type::Error => Err(TypeError {
                message: "Error type is not valid".to_string(),
                location: "type check".to_string(),
                expected: None,
                found: Some(ty.clone()),
            }),
        }
    }

    /// Check type compatibility
    pub fn check_compatibility(&self, source: &Type, target: &Type) -> TypeResult<()> {
        if source.is_assignable_to(target) {
            Ok(())
        } else {
            Err(TypeError {
                message: format!("Type {} is not assignable to {}", source, target),
                location: "type compatibility".to_string(),
                expected: Some(target.clone()),
                found: Some(source.clone()),
            })
        }
    }

    /// Add a variable to the context
    pub fn add_variable(&mut self, name: String, ty: Type) -> TypeResult<()> {
        self.check_type(&ty)?;
        self.context.add_variable(name, ty);
        Ok(())
    }

    /// Add a function to the context
    pub fn add_function(&mut self, name: String, ty: Type) -> TypeResult<()> {
        self.check_type(&ty)?;
        self.context.add_function(name, ty);
        Ok(())
    }

    /// Get the type of a variable
    pub fn get_variable_type(&self, name: &str) -> Option<&Type> {
        self.context.get_variable_type(name)
    }

    /// Get the type of a function
    pub fn get_function_type(&self, name: &str) -> Option<&Type> {
        self.context.get_function_type(name)
    }

    /// Check if a variable exists
    pub fn has_variable(&self, name: &str) -> bool {
        self.context.has_variable(name)
    }

    /// Check if a function exists
    pub fn has_function(&self, name: &str) -> bool {
        self.context.has_function(name)
    }

    /// Get the current type context
    pub fn get_context(&self) -> &TypeContext {
        &self.context
    }

    /// Get a mutable reference to the type context
    pub fn get_context_mut(&mut self) -> &mut TypeContext {
        &mut self.context
    }
}
