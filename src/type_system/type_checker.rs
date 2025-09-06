use crate::basic::cls::{class::Class, custom_class::CustomClass};
use crate::basic::var::prefix::visibility::Visibility;
use crate::type_system::{Type, type_context::TypeContext, type_result::TypeResult};
use std::collections::HashMap;

/// Simplified type checker for validating type safety
#[derive(Default)]
pub struct TypeChecker {
    context: TypeContext,
    /// Map of class names to their definitions for inheritance checking
    class_definitions: HashMap<String, CustomClass>,
}

impl TypeChecker {
    pub fn with_context(context: TypeContext) -> Self {
        Self {
            context,
            class_definitions: HashMap::new(),
        }
    }

    /// Add a class definition to the type checker
    pub fn add_class_definition(&mut self, class: CustomClass) {
        self.class_definitions.insert(class.get_base_name(), class);
    }

    /// Get a class definition by name
    pub fn get_class_definition(&self, name: &str) -> Option<&CustomClass> {
        self.class_definitions.get(name)
    }

    /// Check if a class is a subclass of another class
    pub fn is_subclass_of(&self, subclass_name: &str, superclass_name: &str) -> bool {
        if subclass_name == superclass_name {
            return true;
        }

        if let Some(subclass) = self.get_class_definition(subclass_name)
            && let Some(parent_class) = subclass.get_parent_class()
        {
            return self.is_subclass_of(&parent_class.get_name(), superclass_name);
        }
        false
    }

    /// Get all accessible members (including inherited) for a class
    pub fn get_accessible_members(
        &self,
        class_name: &str,
        current_class: Option<&str>,
    ) -> Vec<(String, String, String)> {
        let mut members = Vec::new();

        if let Some(class) = self.get_class_definition(class_name) {
            // Add properties
            for property in &class.properties {
                let visibility = property.get_prefix().get_visibility();
                let property_name = property.get_name().to_string();
                let property_type = property
                    .get_class()
                    .map(|c| c.to_string())
                    .unwrap_or_else(|| "Unknown".to_string());

                // Check if member is accessible
                if self.is_member_accessible(visibility, current_class, Some(class_name)) {
                    members.push(("property".to_string(), property_name, property_type));
                }
            }

            // Add methods
            for method in &class.methods {
                let method_name = method.get_name().to_string();
                let return_type = method
                    .get_return_type()
                    .map(|t| t.to_string())
                    .unwrap_or_else(|| "Unit".to_string());

                // For now, assume all methods are public (we'd need to add visibility to Function)
                members.push(("method".to_string(), method_name, return_type));
            }

            // Add inherited members
            if let Some(parent_class) = class.get_parent_class() {
                let inherited_members =
                    self.get_accessible_members(&parent_class.get_name(), current_class);
                members.extend(inherited_members);
            }
        }

        members
    }

    /// Check if a member is accessible based on visibility rules
    fn is_member_accessible(
        &self,
        visibility: &Visibility,
        current_class: Option<&str>,
        member_class: Option<&str>,
    ) -> bool {
        match visibility {
            Visibility::Public => true,
            Visibility::Protected => {
                // Protected members are accessible within the same class or subclasses
                if let (Some(current), Some(member)) = (current_class, member_class) {
                    current == member || self.is_subclass_of(current, member)
                } else {
                    false
                }
            }
            Visibility::Private => {
                // Private members are only accessible within the same class
                if let (Some(current), Some(member)) = (current_class, member_class) {
                    current == member
                } else {
                    false
                }
            }
            Visibility::Internal => {
                // Internal members are accessible within the same module (simplified: always true)
                true
            }
            Visibility::Default => {
                // Default visibility (package-private in Kotlin)
                true
            }
        }
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

    /// Check type compatibility with inheritance support
    pub fn check_compatibility(&self, source: &Type, target: &Type) -> TypeResult<()> {
        if self.is_assignable_to_with_inheritance(source, target) {
            Some(())
        } else {
            None
        }
    }

    /// Check if source type is assignable to target type, considering inheritance
    pub fn is_assignable_to_with_inheritance(&self, source: &Type, target: &Type) -> bool {
        // First try the basic type compatibility
        if source.is_assignable_to(target) {
            return true;
        }

        // Check for inheritance-based assignment
        match (source, target) {
            (
                Type::Basic(Class::Custom(source_class)),
                Type::Basic(Class::Custom(target_class)),
            ) => {
                // Check if source class is a subclass of target class
                self.is_subclass_of(&source_class.get_base_name(), &target_class.get_base_name())
            }
            (
                Type::Basic(Class::Generic(source_generic)),
                Type::Basic(Class::Generic(target_generic)),
            ) => {
                // Handle generic types - check if base types are compatible
                match (&source_generic.base_type, &target_generic.base_type) {
                    (Class::Custom(source_custom), Class::Custom(target_custom)) => self
                        .is_subclass_of(
                            &source_custom.get_base_name(),
                            &target_custom.get_base_name(),
                        ),
                    _ => false,
                }
            }
            _ => false,
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
