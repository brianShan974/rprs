use rand::{Rng, SeedableRng, seq::IndexedRandom};

use std::fmt::Display;
use std::rc::Rc;

use crate::basic::{
    cls::{
        class::{BASIC_TYPES, Class},
        custom_class::CustomClass,
    },
    utils::generate_random_identifier,
    var::{prefix::var_prefix::VariablePrefix, variable::Variable},
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Parameter {
    name: String,
    ty: Rc<Class>,
}

impl Parameter {
    pub const MAX_COUNT: usize = 4;

    pub fn new(name: String, ty: Rc<Class>) -> Self {
        Self { name, ty }
    }

    pub fn generate_random_parameter<T: Rng + SeedableRng>(
        rng: &mut T,
        defined_classes: Option<&[Class]>,
    ) -> Self {
        let defined_classes = defined_classes.unwrap_or(BASIC_TYPES);
        if defined_classes.is_empty() {
            // Fallback to a basic type if no classes are available
            Self {
                name: generate_random_identifier(rng),
                ty: Rc::new(BASIC_TYPES.choose(rng).unwrap().clone()),
            }
        } else {
            // Bias towards basic types for method/function parameters
            let chosen_class = if rng.random_bool(0.7) {
                // 70% chance to choose from basic types
                BASIC_TYPES.choose(rng).unwrap().clone()
            } else {
                // 30% chance to choose from all defined classes
                defined_classes.choose(rng).unwrap().clone()
            };

            // For method parameters, we want concrete types instead of generic parameters
            let concrete_type = if let Class::Custom(custom_class) = &chosen_class {
                if !custom_class.get_generic_parameters().is_empty() {
                    // Generate concrete type arguments for generic classes
                    Self::generate_concrete_type_for_generic_class(custom_class, rng)
                } else {
                    chosen_class.clone()
                }
            } else {
                chosen_class.clone()
            };

            Self {
                name: generate_random_identifier(rng),
                ty: Rc::new(concrete_type),
            }
        }
    }

    /// Generate a concrete type for a generic class by replacing type parameters with concrete types
    fn generate_concrete_type_for_generic_class<T: Rng + SeedableRng>(
        custom_class: &CustomClass,
        rng: &mut T,
    ) -> Class {
        let base_name = custom_class.get_base_name();
        let generic_params = custom_class.get_generic_parameters();

        if generic_params.is_empty() {
            // Non-generic class, return as is
            Class::Custom(custom_class.clone())
        } else {
            // Generate concrete type arguments
            let concrete_type_args: Vec<String> = generic_params
                .iter()
                .map(|_| {
                    // Generate random basic types for type arguments
                    let basic_types = ["String", "Int", "Float", "Boolean"];
                    basic_types.choose(rng).unwrap().to_string()
                })
                .collect();

            // Create a new custom class with concrete type arguments
            // For method parameters, we want concrete types like cu<Float, Boolean> instead of cu<T, T>
            let concrete_class_name = format!("{}<{}>", base_name, concrete_type_args.join(", "));

            // Create a new custom class with the concrete name but no generic parameters
            let mut concrete_class = custom_class.clone();
            concrete_class.name = concrete_class_name;
            concrete_class.generic_parameters.clear(); // Remove generic parameters since we're using concrete types

            Class::Custom(concrete_class)
        }
    }

    pub fn generate_random_parameters<T: Rng + SeedableRng>(
        rng: &mut T,
        defined_classes: Option<&[Class]>,
    ) -> Vec<Self> {
        let num_params = rng.random_range(0..=3);
        let mut parameters = Vec::with_capacity(num_params);

        for _ in 0..num_params {
            parameters.push(Self::generate_random_parameter(rng, defined_classes));
        }

        parameters
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_type(&self) -> &Class {
        self.ty.as_ref()
    }
}

impl Display for Parameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}

impl From<Parameter> for Variable {
    fn from(parameter: Parameter) -> Self {
        Variable::new(
            VariablePrefix::default(),
            parameter.name,
            None,
            Some(parameter.ty),
        )
    }
}
