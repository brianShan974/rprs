use rand::{Rng, SeedableRng};
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::basic::body::block::{INDENT_SIZE, SPACE};
use crate::basic::body::fun::function::Function;
use crate::basic::body::fun::parameter::Parameter;
use crate::basic::cls::class::{Class, FLOAT};
use crate::basic::utils::generate_random_identifier;
use crate::basic::var::variable::Variable;
use crate::type_system::TypedGenerationContext;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CustomClass {
    pub name: String,
    pub properties: Vec<Variable>,
    pub methods: Vec<Function>,
    pub current_indentation_layer: usize,
}

impl CustomClass {
    pub const MAX_PROPERTIES: usize = 5;
    pub const MAX_METHODS: usize = 5;

    pub fn new(name: String, current_indentation_layer: usize) -> Self {
        Self {
            name,
            properties: Vec::new(),
            methods: Vec::new(),
            current_indentation_layer,
        }
    }

    pub fn get_name(&self) -> String {
        self.name.clone()
    }

    pub fn add_property(&mut self, property: Variable) {
        self.properties.push(property);
    }

    pub fn add_method(&mut self, method: Function) {
        self.methods.push(method);
    }

    pub fn generate_random_custom_class<T: Rng + SeedableRng>(
        rng: &mut T,
        defined_classes: Option<&mut Vec<Class>>,
        current_indentation_layer: Option<usize>,
    ) -> Self {
        let name = generate_random_identifier(rng);
        let current_indentation_layer = current_indentation_layer.unwrap_or(0);
        let mut custom_class = Self::new(name, current_indentation_layer);

        // Generate 1-4 properties
        let num_properties = rng.random_range(1..=Self::MAX_PROPERTIES);
        for _ in 0..num_properties {
            custom_class.add_property(Variable::generate_random_variable_with_const_control(
                true, true, None, false, rng,
            ));
        }

        // Generate 1-3 methods using type-safe function generation
        let num_methods = rng.random_range(1..=Self::MAX_METHODS);
        for _ in 0..num_methods {
            // Convert class properties to parameters for method generation
            let external_variables: Vec<_> = custom_class
                .properties
                .iter()
                .map(|var| {
                    Parameter::new(
                        var.get_name().to_string(),
                        var.get_type().cloned().unwrap_or(FLOAT),
                    )
                })
                .collect();

            // Create a typed context for method generation
            let external_functions = Rc::new(RefCell::new(Vec::new()));
            let mut typed_context = TypedGenerationContext::new(external_functions);

            // Generate a type-safe method
            if let Some(method) = Function::generate_type_safe_function(
                &external_variables,
                typed_context.get_external_functions(),
                defined_classes.as_ref().map(|classes| classes.as_slice()),
                Some(custom_class.current_indentation_layer + 1), // Indentation level for class methods
                None,                                             // Max depth for class methods
                true,                                             // Is method
                &mut typed_context,
                rng,
            ) {
                custom_class.add_method(method);
            }
        }

        if let Some(defined_classes) = defined_classes {
            defined_classes.push(Class::Custom(custom_class.clone()));
        }

        custom_class
    }

    /// Generate a type-safe custom class using typed generation context
    pub fn generate_type_safe_custom_class<T: Rng + SeedableRng>(
        rng: &mut T,
        typed_context: &mut TypedGenerationContext,
        current_indentation_layer: Option<usize>,
    ) -> Self {
        let name = generate_random_identifier(rng);
        let current_indentation_layer = current_indentation_layer.unwrap_or(0);
        let mut custom_class = Self::new(name, current_indentation_layer);

        // Generate 1-4 properties
        let num_properties = rng.random_range(1..=Self::MAX_PROPERTIES);
        for _ in 0..num_properties {
            custom_class.add_property(Variable::generate_random_variable_with_const_control(
                true, true, None, false, rng,
            ));
        }

        // Generate 1-3 methods using type-safe function generation
        let num_methods = rng.random_range(1..=Self::MAX_METHODS);
        for _ in 0..num_methods {
            // Convert class properties to parameters for method generation
            let external_variables: Vec<_> = custom_class
                .properties
                .iter()
                .map(|var| {
                    Parameter::new(
                        var.get_name().to_string(),
                        var.get_type().cloned().unwrap_or(FLOAT),
                    )
                })
                .collect();

            // Create a separate typed context for each method to avoid recursion
            let mut method_typed_context =
                TypedGenerationContext::new(typed_context.get_external_functions());

            // Generate a type-safe method with explicit return type
            if let Some(method) = Self::generate_method_with_return_type(
                &external_variables,
                &mut method_typed_context,
                custom_class.current_indentation_layer,
                rng,
            ) {
                // Add the method to the main typed context so other methods can call it
                typed_context.add_function(&method).ok();
                custom_class.add_method(method);
            }
        }

        custom_class
    }

    /// Generate a method with explicit return type
    fn generate_method_with_return_type<T: Rng + SeedableRng>(
        external_variables: &[Parameter],
        typed_context: &mut TypedGenerationContext,
        current_indentation_layer: usize,
        rng: &mut T,
    ) -> Option<Function> {
        // Generate a type-safe method
        // Function::generate_type_safe_function will decide the return type internally
        Function::generate_type_safe_function(
            external_variables,
            typed_context.get_external_functions(),
            None,                                // defined_classes - use default basic types
            Some(current_indentation_layer + 1), // Indentation level for class methods
            None,
            true, // Is method
            typed_context,
            rng,
        )
    }
}

impl fmt::Display for CustomClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Calculate indentation for class level
        let outer_indentation = SPACE.repeat(self.current_indentation_layer * INDENT_SIZE);
        let inner_indentation = SPACE.repeat(INDENT_SIZE);

        // Start class declaration
        writeln!(f, "{}class {} {{", outer_indentation, self.name)?;

        for property in &self.properties {
            writeln!(
                f,
                "{outer_indentation}{inner_indentation}{} {}: {} = {}",
                property.get_prefix(),
                property.get_name(),
                property
                    .get_type()
                    .map(|c| c.get_name())
                    .unwrap_or_else(|| "Unknown".to_string()),
                property
                    .get_value()
                    .map(|v| v.to_string())
                    .unwrap_or_else(|| "null".to_string())
            )?;
        }

        if !self.methods.is_empty() {
            writeln!(f)?;
        }

        for method in &self.methods {
            writeln!(f, "{}", method)?;
        }

        writeln!(f, "{}}}", outer_indentation)?;

        Ok(())
    }
}
