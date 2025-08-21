use rand::{Rng, SeedableRng, seq::IndexedRandom};

use std::fmt::Display;

use crate::basic::{
    cls::class::{BASIC_TYPES, Class},
    utils::generate_random_identifier,
    var::{prefix::var_prefix::VariablePrefix, variable::Variable},
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Parameter {
    name: String,
    ty: Class,
}

impl Parameter {
    pub const MAX_COUNT: usize = 4;

    pub fn new(name: String, ty: Class) -> Self {
        Self { name, ty }
    }

    pub fn generate_random_parameter<T: Rng + SeedableRng>(
        rng: &mut T,
        defined_classes: Option<&[Class]>,
    ) -> Self {
        let defined_classes = defined_classes.unwrap_or(BASIC_TYPES);
        Self {
            name: generate_random_identifier(rng),
            ty: defined_classes.choose(rng).unwrap().clone(),
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
        &self.ty
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
            Some(parameter.ty.clone()),
        )
    }
}
