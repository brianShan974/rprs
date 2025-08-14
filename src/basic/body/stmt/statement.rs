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
    pub fn generate_random_statement(external_variables: Vec<Variable>) -> Self {
        Self::generate_random_statement_with_depth(external_variables, 5)
    }

    pub fn generate_random_statement_with_depth(
        external_variables: Vec<Variable>,
        max_depth: usize,
    ) -> Self {
        let mut rng = rand::rng();

        // If depth is 0, only generate simple statements
        if max_depth == 0 {
            return Statement::Single(
                SingleStatement::generate_random_single_statement_with_variables(
                    external_variables,
                ),
            );
        }

        // Randomly select statement type, prefer simple statements at shallow depth
        match rng.random_range(0..10) {
            0..=5 => Statement::Single(
                SingleStatement::generate_random_single_statement_with_variables(
                    external_variables,
                ),
            ),
            6..=7 => Statement::If(IfStatement::generate_random_if_statement(
                external_variables,
                0,
                Some(max_depth - 1),
            )),
            8 => Statement::For(ForStatement::generate_random_for_statement(
                external_variables,
                0,
            )),
            9 => Statement::While(WhileStatement::generate_random_while_statement(
                external_variables,
                0,
            )),
            _ => Statement::When(WhenStatement::generate_random_when_statement(
                external_variables,
                0,
            )),
        }
    }
}
