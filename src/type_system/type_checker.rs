use crate::type_system::{Type, type_context::TypeContext, type_result::TypeResult};

/// Simplified type checker for validating type safety
#[derive(Default)]
pub struct TypeChecker {
    context: TypeContext,
}

impl TypeChecker {
    pub fn with_context(context: TypeContext) -> Self {
        Self { context }
    }

    /// Check if a type is valid
    pub fn check_type(ty: &Type) -> TypeResult<()> {
        match ty {
            Type::Basic(_) => Some(()),
            Type::Function(params, ret) => {
                for param in params {
                    Self::check_type(param)?;
                }
                Self::check_type(ret)
            }
            Type::Generic(_, params) => {
                for param in params {
                    Self::check_type(param)?;
                }
                Some(())
            }
            Type::Union(types) => {
                for ty in types {
                    Self::check_type(ty)?;
                }
                Some(())
            }
            Type::Nullable(inner) => Self::check_type(inner),
            Type::Variable(_) => Some(()),
            Type::Unknown => Some(()),
            Type::Error => None,
        }
    }

    /// Check type compatibility
    pub fn check_compatibility(&self, source: &Type, target: &Type) -> TypeResult<()> {
        if source.is_assignable_to(target) {
            Some(())
        } else {
            None
        }
    }

    /// Add a variable to the context
    pub fn add_variable(&mut self, name: String, ty: Type) -> TypeResult<()> {
        Self::check_type(&ty)?;
        self.context.add_variable(name, ty);
        Some(())
    }

    /// Add a function to the context
    pub fn add_function(&mut self, name: String, ty: Type) -> TypeResult<()> {
        Self::check_type(&ty)?;
        self.context.add_function(name, ty);
        Some(())
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
