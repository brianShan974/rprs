use rand::{Rng, SeedableRng};
use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;

use crate::basic::body::block::{INDENT_SIZE, SPACE};
use crate::basic::body::fun::function::Function;
use crate::basic::body::fun::parameter::Parameter;
use crate::basic::cls::class::{Class, FLOAT};
use crate::basic::utils::{GenerationConfig, generate_unique_identifier};
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

    pub fn get_methods(&self) -> &[Function] {
        &self.methods
    }

    pub fn generate_random_custom_class<T: Rng + SeedableRng>(
        rng: &mut T,
        defined_classes: Option<&mut Vec<Class>>,
        current_indentation_layer: Option<usize>,
        existing_names: Option<&mut HashSet<String>>,
    ) -> Self {
        let mut existing_names = existing_names.unwrap_or(&mut HashSet::new()).clone();
        let name = generate_unique_identifier(rng, &mut existing_names);
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
                        var.get_class().cloned().unwrap_or(FLOAT),
                    )
                })
                .collect();

            // Create a typed context for method generation
            let external_functions = Rc::new(RefCell::new(Vec::new()));
            let mut typed_context = TypedGenerationContext::new(external_functions);

            // Generate a type-safe method
            let mut method_config = GenerationConfig::new(
                external_variables
                    .iter()
                    .map(|p| p.clone().into())
                    .collect(),
                typed_context.get_external_functions(),
                defined_classes.as_ref().map(|classes| classes.to_vec()),
                custom_class.current_indentation_layer + 1,
                5, // Max depth for class methods
            );

            if let Some(method) = Function::generate_type_safe_function(
                &mut method_config,
                &external_variables,
                true, // Is method
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
        existing_names: Option<&mut HashSet<String>>,
    ) -> Self {
        let mut existing_names = existing_names.unwrap_or(&mut HashSet::new()).clone();
        let name = generate_unique_identifier(rng, &mut existing_names);
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
                        var.get_class().cloned().unwrap_or(FLOAT),
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
                typed_context.add_function(&method);
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
        let mut method_config = GenerationConfig::new(
            external_variables
                .iter()
                .map(|p| p.clone().into())
                .collect(),
            typed_context.get_external_functions(),
            Some(typed_context.get_defined_classes().to_vec()), // Pass defined classes for method generation
            current_indentation_layer + 1,
            5, // Max depth for class methods
        );

        Function::generate_type_safe_function(
            &mut method_config,
            external_variables,
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
                    .get_class()
                    .map(|c| c.get_name())
                    .unwrap_or_else(|| "Unknown".to_string()),
                property
                    .get_value()
                    .map(|v| v.to_string())
                    .unwrap_or_else(|| "null".to_string())
            )?;
        }

        for method in &self.methods {
            writeln!(f, "\n{}", method)?;
        }

        write!(f, "{}}}", outer_indentation)?;

        Ok(())
    }
}
