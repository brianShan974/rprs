use crate::basic::var::variable::Variable;
use rand::Rng;
use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

use super::stmt::statement::Statement;

pub const SPACE: &str = " ";
pub const INDENT_SIZE: usize = 4;

#[derive(Clone)]
pub struct Block {
    is_independent: bool,
    statements: Vec<Statement>,
    current_indentation_layer: usize,
}

impl Block {
    pub const MAX_DEPTH: usize = 5;

    pub const MAX_NUM_STATEMENTS: usize = 3;
    pub const MAX_NUM_NEW_VARS: usize = 1;

    pub fn generate_random_block(
        external_variables: Vec<Variable>,
        external_functions: Rc<RefCell<Vec<crate::basic::body::fun::function::Function>>>,
        current_indentation_layer: usize,
        is_independent: bool,
        max_depth: usize,
    ) -> Option<Self> {
        let mut rng = rand::rng();

        let num_new_vars = rng.random_range(0..=Self::MAX_NUM_NEW_VARS);
        let mut new_variables = Vec::with_capacity(num_new_vars);

        // Generate new variables (but don't add them to external_variables)
        for _ in 0..num_new_vars {
            let new_var = Variable::generate_random_variable(true, true);
            new_variables.push(new_var);
        }

        // Create combined external variables for child blocks
        let mut combined_external_variables = external_variables.clone();
        combined_external_variables.extend(new_variables.clone());

        let num_statements = rng.random_range(1..=Self::MAX_NUM_STATEMENTS);
        let mut statements = Vec::with_capacity(num_statements);

        // Generate random statements with depth limit
        for _ in 0..num_statements {
            statements.push(Statement::generate_random_statement(
                combined_external_variables.clone(),
                external_functions.clone(),
                current_indentation_layer + 1,
                Some(max_depth - 1),
            )?);
        }

        Some(Block {
            is_independent,
            statements,
            current_indentation_layer,
        })
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let outer_indentation = SPACE.repeat(self.current_indentation_layer * INDENT_SIZE);
        let inner_indentation = SPACE.repeat(INDENT_SIZE);

        if self.is_independent {
            write!(f, "{outer_indentation}")?;
        }
        writeln!(f, "{{")?;
        for stmt in self.statements.iter() {
            match stmt {
                Statement::Single(single_stmt) => {
                    writeln!(f, "{outer_indentation}{inner_indentation}{single_stmt}")?;
                }
                _ => {
                    writeln!(f, "{stmt}")?;
                }
            }
        }
        write!(f, "{outer_indentation}}}")?;

        Ok(())
    }
}
