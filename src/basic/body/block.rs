use crate::basic::var::variable::Variable;

use super::stmt::statement::Statement;

pub struct Block {
    statements: Vec<Statement>,
    external_variables: Vec<Variable>,
    new_variables: Vec<Variable>,
}
