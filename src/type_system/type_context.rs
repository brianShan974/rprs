use std::collections::HashMap;

use crate::type_system::Type;

/// Type context for storing type information
#[derive(Clone, Debug, Default)]
pub struct TypeContext {
    /// Variable types
    variables: HashMap<String, Type>,
    /// Function signatures
    functions: HashMap<String, Type>,
    /// Type aliases
    aliases: HashMap<String, Type>,
}

impl TypeContext {
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
