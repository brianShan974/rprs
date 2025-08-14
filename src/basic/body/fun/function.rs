use std::fmt::Display;

use crate::basic::{
    body::{block::Block, fun::parameter::Parameter},
    cls::class::Class,
    utils::generate_random_identifier,
    var::variable::Variable,
};

pub struct Function {
    name: String,
    parameters: Vec<Parameter>,
    return_type: Option<Class>,
    body: Block,
}

impl Function {
    pub fn generate_random_function(external_variables: Vec<Parameter>) -> Self {
        let parameters = Parameter::generate_random_parameters();
        let all_identifiers: Vec<Variable> = external_variables
            .into_iter()
            .chain(parameters.clone().into_iter())
            .map(|p| p.into())
            .collect();

        Self {
            name: generate_random_identifier(),
            parameters,
            return_type: None,
            body: Block::generate_random_block(all_identifiers, 0),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.return_type {
            Some(ty) => writeln!(
                f,
                "fun {}({}): {} {{",
                self.name,
                self.parameters
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                ty
            )?,
            None => writeln!(
                f,
                "fun {}({}) {{",
                self.name,
                self.parameters
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
            )?,
        }
        writeln!(f, "{}", self.body)?;
        writeln!(f, "}}")?;

        Ok(())
    }
}
