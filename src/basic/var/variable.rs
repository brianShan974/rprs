use derive_more::Constructor;

use crate::basic::expr::expression::Expression;

use super::prefix::var_prefix::VariablePrefix;

#[derive(Constructor, Debug)]
pub struct Variable {
    prefix: VariablePrefix,
    name: String,
    value: Option<Expression>,
}

impl Variable {
    pub fn output_declaration(&self) -> String {
        format!("{}{}", self.prefix, self.name)
    }

    pub fn output_assignment(&self) -> Option<String> {
        self.value
            .as_ref()
            .map(|value| format!("{} = {}", self.name, value))
    }

    pub fn output_init(&self) -> Option<String> {
        self.value
            .as_ref()
            .map(|value| format!("{}{} = {}", self.prefix, self.name, value))
    }
}
