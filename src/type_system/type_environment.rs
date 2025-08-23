use crate::type_system::{Type, type_context::TypeContext};

/// Type environment for managing scoped type information
pub struct TypeEnvironment {
    /// Stack of type contexts (for nested scopes)
    contexts: Vec<TypeContext>,
    /// Global type context
    global_context: TypeContext,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        Self {
            contexts: vec![TypeContext::default()],
            global_context: TypeContext::default(),
        }
    }

    /// Enter a new scope
    pub fn enter_scope(&mut self) {
        self.contexts.push(TypeContext::default());
    }

    /// Exit the current scope
    pub fn exit_scope(&mut self) -> Option<TypeContext> {
        self.contexts.pop()
    }

    /// Get the current scope context
    pub fn current_context(&self) -> &TypeContext {
        self.contexts.last().unwrap_or(&self.global_context)
    }

    /// Get a mutable reference to the current scope context
    pub fn current_context_mut(&mut self) -> &mut TypeContext {
        self.contexts.last_mut().unwrap_or(&mut self.global_context)
    }

    /// Add a variable to the current scope
    pub fn add_variable(&mut self, name: String, ty: Type) {
        self.current_context_mut().add_variable(name, ty);
    }

    /// Get the type of a variable (search from current scope to global)
    pub fn get_variable_type(&self, name: &str) -> Option<&Type> {
        // Search from innermost scope to outermost
        for context in self.contexts.iter().rev() {
            if let Some(ty) = context.get_variable_type(name) {
                return Some(ty);
            }
        }

        // Check global context
        self.global_context.get_variable_type(name)
    }

    /// Add a function to the global context
    pub fn add_function(&mut self, name: String, ty: Type) {
        self.global_context.add_function(name, ty);
    }

    /// Get the type of a function from global context
    pub fn get_function_type(&self, name: &str) -> Option<&Type> {
        self.global_context.get_function_type(name)
    }

    /// Check if a variable exists in any scope
    pub fn has_variable(&self, name: &str) -> bool {
        self.get_variable_type(name).is_some()
    }

    /// Check if a function exists
    pub fn has_function(&self, name: &str) -> bool {
        self.global_context.has_function(name)
    }

    /// Get all variable names in the current scope
    pub fn get_current_scope_variables(&self) -> Vec<String> {
        self.current_context().get_variable_names()
    }

    /// Get all function names
    pub fn get_function_names(&self) -> Vec<String> {
        self.global_context.get_function_names()
    }

    /// Get the depth of the scope stack
    pub fn scope_depth(&self) -> usize {
        self.contexts.len()
    }

    /// Check if we're in the global scope
    pub fn is_global_scope(&self) -> bool {
        self.contexts.len() == 1
    }

    /// Get a snapshot of the current type environment
    pub fn snapshot(&self) -> TypeEnvironment {
        TypeEnvironment {
            contexts: self.contexts.clone(),
            global_context: self.global_context.clone(),
        }
    }

    /// Restore from a snapshot
    pub fn restore(&mut self, snapshot: TypeEnvironment) {
        self.contexts = snapshot.contexts;
        self.global_context = snapshot.global_context;
    }
}

impl Default for TypeEnvironment {
    fn default() -> Self {
        Self::new()
    }
}
