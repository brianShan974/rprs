use derive_more::Display;

use crate::basic::body::stmt::{
    for_statement::ForStatement, if_statement::IfStatement, single_statement::SingleStatement,
    when_statement::WhenStatement, while_statement::WhileStatement,
};
use crate::basic::var::variable::Variable;
use rand::Rng;

#[derive(Display)]
#[display("{}", _0)]
pub enum Statement {
    Single(SingleStatement),
    If(IfStatement),
    For(ForStatement),
    While(WhileStatement),
    When(WhenStatement),
}

impl Statement {
    pub const MAX_DEPTH: usize = 5;

    pub fn generate_random_statement(
        external_variables: Vec<Variable>,
        current_indentation_layer: usize,
        max_depth: Option<usize>,
    ) -> Option<Self> {
        if matches!(max_depth, Some(0)) {
            return None;
        }

        let max_depth = max_depth.unwrap_or(Self::MAX_DEPTH);
        let mut rng = rand::rng();

        // If depth is 1, only generate simple statements
        if max_depth == 1 {
            return Some(Statement::Single(
                SingleStatement::generate_random_single_statement(external_variables),
            ));
        }

        // Randomly select statement type, prefer simple statements at shallow depth
        Some(match rng.random_range(0..10) {
            0..=5 => Statement::Single(SingleStatement::generate_random_single_statement(
                external_variables,
            )),
            6..=7 => Statement::If(IfStatement::generate_random_if_statement(
                external_variables,
                current_indentation_layer,
                max_depth - 1,
            )?),
            8 => Statement::For(ForStatement::generate_random_for_statement(
                external_variables,
                current_indentation_layer,
                max_depth - 1,
            )?),
            9 => Statement::While(WhileStatement::generate_random_while_statement(
                external_variables,
                current_indentation_layer,
                max_depth - 1,
            )?),
            _ => Statement::When(WhenStatement::generate_random_when_statement(
                external_variables,
                current_indentation_layer,
                max_depth - 1,
            )?),
        })
    }
}
