use crate::basic::var::variable::Variable;
use rand::Rng;
use std::fmt::Display;

use super::stmt::statement::Statement;

const MAX_NUM_STATEMENTS: usize = 3;
const MAX_NUM_NEW_VARS: usize = 1;

const INDENTATION: usize = 4;

pub struct Block {
    statements: Vec<Statement>,
    current_indentation_layer: usize,
    new_variables: Vec<Variable>,
    external_variables: Vec<Variable>,
}

impl Block {
    pub fn generate_random_block(
        external_variables: Vec<Variable>,
        current_indentation_layer: usize,
    ) -> Self {
        Self::generate_random_block_with_depth(external_variables, current_indentation_layer, 3)
    }

    pub fn generate_random_block_with_depth(
        external_variables: Vec<Variable>,
        current_indentation_layer: usize,
        max_depth: usize,
    ) -> Self {
        let mut rng = rand::rng();

        let num_new_vars = rng.random_range(0..=MAX_NUM_NEW_VARS);
        let mut new_variables = Vec::with_capacity(num_new_vars);

        // Generate new variables (but don't add them to external_variables)
        for _ in 0..num_new_vars {
            let new_var = Variable::generate_random_variable(true, true);
            new_variables.push(new_var);
        }

        // Create combined external variables for child blocks
        let mut combined_external_variables = external_variables.clone();
        combined_external_variables.extend(new_variables.clone());

        let num_statements = rng.random_range(1..=MAX_NUM_STATEMENTS);
        let mut statements = Vec::with_capacity(num_statements);

        // Generate random statements with depth limit
        for _ in 0..num_statements {
            statements.push(Statement::generate_random_statement_with_depth(
                combined_external_variables.clone(),
                max_depth,
            ));
        }

        Block {
            statements,
            current_indentation_layer,
            new_variables,
            external_variables,
        }
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let space = " ";
        let outer_indentation = space.repeat(self.current_indentation_layer * INDENTATION);
        let inner_indentation = space.repeat((self.current_indentation_layer + 1) * INDENTATION);
        writeln!(f, "{outer_indentation}{{")?;
        for stmt in self.statements.iter() {
            writeln!(f, "{inner_indentation}{stmt}")?
        }
        writeln!(f, "{outer_indentation}}}")?;

        Ok(())
    }
}
