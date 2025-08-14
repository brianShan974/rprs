use crate::basic::{
    body::block::Block, cls::class::Class, utils::generate_random_identifier,
    var::variable::Variable,
};

pub struct Function {
    name: String,
    parameters: Vec<Variable>,
    return_type: Option<Class>,
    body: Block,
}

impl Function {
    pub fn generate_random_function(external_variables: Vec<Variable>) -> Self {
        Self {
            name: generate_random_identifier(),
            parameters: Vec::new(),
            return_type: None,
            body: Block::generate_random_block(external_variables, 0),
        }
    }
}
