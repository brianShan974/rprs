use crate::basic::{
    body::{
        fun::function::Function,
        stmt::{single_statement::SingleStatement, statement::Statement},
    },
    utils::GenerationConfig,
    var::variable::Variable,
};
use crate::type_system::{Type, TypedGenerationContext};
use rand::{Rng, SeedableRng};
use std::borrow::Cow;
use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

pub const SPACE: &str = " ";
pub const INDENT_SIZE: usize = 4;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Block {
    is_independent: bool,
    statements: Vec<Statement>,
    current_indentation_layer: usize,
}

impl Block {
    pub const MAX_DEPTH: usize = 5;

    pub const MAX_NUM_STATEMENTS: usize = 6; // Reduced from 8 to 6 for fewer statements
    pub const MAX_NUM_NEW_VARS: usize = Self::MAX_NUM_STATEMENTS / 2; // Increased from 1 to 3 for more variables

    /// Create a new block with statements
    pub fn new(statements: Vec<Statement>, current_indentation_layer: usize) -> Self {
        Self {
            is_independent: false,
            statements,
            current_indentation_layer,
        }
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
        external_variables: &[Variable],
        external_functions: Rc<RefCell<Vec<Function>>>,
        current_indentation_layer: usize,
        is_independent: bool,
        max_depth: usize,
        typed_context: &mut TypedGenerationContext,
        rng: &mut T,
    ) -> Option<Self> {
        Self::generate_type_safe_block_with_config(
            &mut GenerationConfig::new(
                external_variables.to_vec(),
                external_functions,
                Some(
                    typed_context
                        .get_defined_classes()
                        .iter()
                        .map(|rc| rc.as_ref().clone())
                        .collect(),
                ), // Pass defined classes from typed context
                current_indentation_layer,
                max_depth,
            ),
            external_variables,
            is_independent,
            typed_context,
            None, // No expected return type for this block
            rng,
        )
    }

    /// Generate a type-safe block with expected return type (using GenerationConfig)
    pub fn generate_type_safe_block_with_config<T: Rng + SeedableRng>(
        config: &mut GenerationConfig,
        external_variables: &[Variable],
        is_independent: bool,
        typed_context: &mut TypedGenerationContext,
        expected_return_type: Option<&Type>,
        rng: &mut T,
    ) -> Option<Self> {
        if config.max_depth == 0 {
            return None;
        }

        // Create a child context for this block to ensure proper variable scoping
        // Variables declared in this block won't affect the parent context
        let mut block_context = typed_context.create_child_context();

        // Ensure the block context has access to defined classes for variable generation
        if let Some(classes) = &config.defined_classes {
            block_context.set_defined_classes(classes.clone());
        }

        let num_new_vars = rng.random_range(0..=Self::MAX_NUM_NEW_VARS);
        let mut new_variables = Vec::with_capacity(num_new_vars);

        // Generate new type-compatible variables in the block context
        for _ in 0..num_new_vars {
            let new_var = block_context.generate_type_compatible_variable_no_const(false, rng);
            let _ = block_context.add_variable(&new_var);
            new_variables.push(new_var);
        }

        // Create combined external variables for child blocks - use Cow to avoid unnecessary cloning
        let combined_external_variables: Cow<[Variable]> = if new_variables.is_empty() {
            Cow::Borrowed(external_variables)
        } else {
            let mut combined = Vec::with_capacity(external_variables.len() + new_variables.len());
            combined.extend_from_slice(external_variables);
            combined.extend_from_slice(&new_variables);
            Cow::Owned(combined)
        };

        let num_statements = rng.random_range(1..=Self::MAX_NUM_STATEMENTS);
        let mut statements = Vec::with_capacity(num_statements + new_variables.len());

        // Add variable declaration statements for new variables
        for new_var in &new_variables {
            statements.push(Statement::Single(SingleStatement::VariableDeclaration(
                new_var.clone(),
            )));
        }

        // Generate type-safe statements with return type awareness using the block context
        for _ in 0..num_statements {
            statements.push(Statement::generate_type_safe_statement_with_return_type(
                &mut GenerationConfig::new(
                    combined_external_variables.to_vec(), // Only clone when necessary
                    config.external_functions.clone(),
                    config
                        .defined_classes
                        .as_ref()
                        .map(|classes| classes.to_vec()), // TODO: optimize to avoid cloning
                    config.current_indentation_layer + 1,
                    config.max_depth - 1,
                ),
                &combined_external_variables,
                Some(config.max_depth - 1),
                &mut block_context, // Use the block-specific context
                expected_return_type,
                rng,
            )?);
        }

        Some(Block {
            is_independent,
            statements,
            current_indentation_layer: config.current_indentation_layer,
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
