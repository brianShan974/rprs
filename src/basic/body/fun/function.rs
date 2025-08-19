use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

use crate::basic::{
    body::{
        block::{Block, INDENT_SIZE, SPACE},
        fun::parameter::Parameter,
    },
    cls::class::Class,
    utils::generate_random_identifier,
    var::variable::Variable,
};

#[derive(Clone)]
pub struct Function {
    name: String,
    parameters: Vec<Parameter>,
    return_type: Option<Class>,
    body: Block,
    current_indentation_layer: usize,
}

impl Function {
    pub const MAX_DEPTH: usize = 5;

    pub fn generate_random_function(
        external_variables: Vec<Parameter>,
        external_functions: Rc<RefCell<Vec<Function>>>,
        current_indentation_layer: Option<usize>,
        max_depth: Option<usize>,
    ) -> Option<Self> {
        if matches!(max_depth, Some(0)) {
            return None;
        }

        let max_depth = max_depth.unwrap_or(Self::MAX_DEPTH);
        let current_indentation_layer = current_indentation_layer.unwrap_or(0);
        let parameters = Parameter::generate_random_parameters();
        let all_identifiers: Vec<Variable> = external_variables
            .into_iter()
            .chain(parameters.clone().into_iter())
            .map(|p| p.into())
            .collect();

        let function = Self {
            name: generate_random_identifier(),
            parameters,
            return_type: None,
            body: Block::generate_random_block(
                all_identifiers,
                external_functions.clone(),
                current_indentation_layer,
                false,
                max_depth,
            )?,
            current_indentation_layer,
        };

        // Add the generated function to external_functions
        external_functions.borrow_mut().push(function.clone());

        Some(function)
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let indentation = SPACE.repeat(self.current_indentation_layer * INDENT_SIZE);
        match &self.return_type {
            Some(ty) => writeln!(
                f,
                "{indentation}fun {}({}): {} {}",
                self.name,
                self.parameters
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                ty,
                self.body,
            )?,
            None => writeln!(
                f,
                "{indentation}fun {}({}) {}",
                self.name,
                self.parameters
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                self.body,
            )?,
        }

        Ok(())
    }
}
