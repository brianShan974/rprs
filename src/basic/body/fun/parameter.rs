use std::fmt::Display;

use rand::Rng;

use crate::basic::{
    cls::class::Class,
    utils::generate_random_identifier,
    var::{prefix::var_prefix::VariablePrefix, variable::Variable},
};

#[derive(Clone, Debug)]
pub struct Parameter {
    name: String,
    ty: Class,
}

impl Parameter {
    pub const MAX_COUNT: usize = 4;

    pub fn generate_random_parameter() -> Self {
        Self {
            name: generate_random_identifier(),
            ty: Class::generate_random_class(),
        }
    }

    pub fn generate_random_parameters() -> Vec<Self> {
        let mut rng = rand::rng();

        let count = rng.random_range(0..=Self::MAX_COUNT);

        (0..count)
            .map(|_| Self::generate_random_parameter())
            .collect()
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
