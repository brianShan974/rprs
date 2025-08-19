use crate::basic::{
    body::{fun::function::Function, stmt::statement::Statement},
    var::variable::Variable,
};
use crate::type_system::{Type, TypedGenerationContext};
use rand::{Rng, SeedableRng};
use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

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

    /// Create a new block with statements
    pub fn new(statements: Vec<Statement>, current_indentation_layer: usize) -> Self {
        Self {
            is_independent: false,
            statements,
            current_indentation_layer,
        }
    }

    pub fn generate_random_block<T: Rng + SeedableRng>(
        external_variables: Vec<Variable>,
        external_functions: Rc<RefCell<Vec<Function>>>,
        current_indentation_layer: usize,
        is_independent: bool,
        max_depth: usize,
        rng: &mut T,
    ) -> Option<Self> {
        let num_new_vars = rng.random_range(0..=Self::MAX_NUM_NEW_VARS);
        let mut new_variables = Vec::with_capacity(num_new_vars);

        // Generate new variables (but don't add them to external_variables)
        for _ in 0..num_new_vars {
            let new_var = Variable::generate_random_variable(true, true, rng);
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
                rng,
            )?);
        }

        Some(Block {
            is_independent,
            statements,
            current_indentation_layer,
        })
    }

    pub fn get_statements(&self) -> &Vec<Statement> {
        &self.statements
    }

    pub fn get_statements_mut(&mut self) -> &mut Vec<Statement> {
        &mut self.statements
    }

    pub fn is_independent(&self) -> bool {
        self.is_independent
    }

    pub fn get_current_indentation_layer(&self) -> usize {
        self.current_indentation_layer
    }

    /// Generate a type-safe block using typed generation context
    pub fn generate_type_safe_block<T: Rng + SeedableRng>(
        external_variables: Vec<Variable>,
        external_functions: Rc<RefCell<Vec<Function>>>,
        current_indentation_layer: usize,
        is_independent: bool,
        max_depth: usize,
        typed_context: &mut TypedGenerationContext,
        rng: &mut T,
    ) -> Option<Self> {
        Self::generate_type_safe_block_with_return_type(
            external_variables,
            external_functions,
            current_indentation_layer,
            is_independent,
            max_depth,
            typed_context,
            None,
            rng,
        )
    }

    /// Generate a type-safe block with expected return type
    pub fn generate_type_safe_block_with_return_type<T: Rng + SeedableRng>(
        external_variables: Vec<Variable>,
        external_functions: Rc<RefCell<Vec<Function>>>,
        current_indentation_layer: usize,
        is_independent: bool,
        max_depth: usize,
        typed_context: &mut TypedGenerationContext,
        expected_return_type: Option<&Type>,
        rng: &mut T,
    ) -> Option<Self> {
        let num_new_vars = rng.random_range(0..=Self::MAX_NUM_NEW_VARS);
        let mut new_variables = Vec::with_capacity(num_new_vars);

        // Generate new type-compatible variables
        for _ in 0..num_new_vars {
            let new_var = typed_context.generate_type_compatible_variable(rng);
            let _ = typed_context.add_variable(&new_var);
            new_variables.push(new_var);
        }

        // Create combined external variables for child blocks
        let mut combined_external_variables = external_variables.clone();
        combined_external_variables.extend(new_variables.clone());

        let num_statements = rng.random_range(1..=Self::MAX_NUM_STATEMENTS);
        let mut statements = Vec::with_capacity(num_statements);

        // Generate type-safe statements with return type awareness
        for _ in 0..num_statements {
            statements.push(Statement::generate_type_safe_statement_with_return_type(
                combined_external_variables.clone(),
                external_functions.clone(),
                current_indentation_layer + 1,
                Some(max_depth - 1),
                typed_context,
                expected_return_type,
                rng,
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
