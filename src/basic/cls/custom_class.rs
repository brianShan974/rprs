use rand::{Rng, SeedableRng};
use std::fmt;

use crate::basic::body::block::{INDENT_SIZE, SPACE};
use crate::basic::body::fun::function::Function;
use crate::basic::cls::class::Class;
use crate::basic::utils::generate_random_identifier;
use crate::basic::var::variable::Variable;

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
            custom_class.add_property(Variable::generate_random_variable(true, true, rng));
        }

        // Generate 1-3 methods
        let num_methods = rng.random_range(1..=Self::MAX_METHODS);
        for _ in 0..num_methods {
            // Generate a method as a Function
            if let Some(method) = Function::generate_random_function(
                custom_class.properties.clone(), // External variables for class methods
                std::rc::Rc::new(std::cell::RefCell::new(Vec::new())), // Empty external functions
                defined_classes.as_ref().map(|classes| classes.as_slice()),
                Some(custom_class.current_indentation_layer + 1), // Indentation level for class methods
                Some(3),                                          // Max depth for class methods
                true,
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
                property.get_type().unwrap(),
                property.get_value().unwrap()
            )?;
        }

        for method in &self.methods {
            writeln!(f, "{outer_indentation}{inner_indentation}{}", method)?;
        }

        writeln!(f, "{}}}", outer_indentation)?;

        Ok(())
    }
}
