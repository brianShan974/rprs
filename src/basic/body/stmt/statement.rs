use derive_more::Display;
use rand::{Rng, SeedableRng};

use std::cell::RefCell;
use std::rc::Rc;

use crate::basic::body::fun::function::Function;
use crate::basic::body::stmt::{
    for_statement::ForStatement, if_statement::IfStatement, single_statement::SingleStatement,
    when_statement::WhenStatement, while_statement::WhileStatement,
};
use crate::basic::var::variable::Variable;
use crate::type_system::{Type, TypedGenerationContext};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Display)]
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

    pub fn generate_random_statement<T: Rng + SeedableRng>(
        external_variables: &[Variable],
        external_functions: Rc<RefCell<Vec<Function>>>,
        current_indentation_layer: usize,
        max_depth: Option<usize>,
        rng: &mut T,
    ) -> Option<Self> {
        if matches!(max_depth, Some(0)) {
            return None;
        }

        let max_depth = max_depth.unwrap_or(Self::MAX_DEPTH);

        // If depth is 1, only generate simple statements
        if max_depth == 1 {
            return Some(Statement::Single(
                SingleStatement::generate_random_single_statement(
                    external_variables,
                    external_functions,
                    rng,
                ),
            ));
        }

        // Randomly select statement type, prefer simple statements at shallow depth
        Some(match rng.random_range(0..10) {
            0..=5 => Statement::Single(SingleStatement::generate_random_single_statement(
                external_variables,
                external_functions.clone(),
                rng,
            )),
            6..=7 => Statement::If(IfStatement::generate_random_if_statement(
                external_variables,
                external_functions,
                current_indentation_layer,
                max_depth - 1,
                rng,
            )?),
            8 => Statement::For(ForStatement::generate_random_for_statement(
                external_variables,
                external_functions,
                current_indentation_layer,
                max_depth - 1,
                rng,
            )?),
            9 => Statement::While(WhileStatement::generate_random_while_statement(
                external_variables,
                external_functions,
                current_indentation_layer,
                max_depth - 1,
                rng,
            )?),
            _ => Statement::When(WhenStatement::generate_random_when_statement(
                external_variables,
                external_functions,
                current_indentation_layer,
                max_depth - 1,
                rng,
            )?),
        })
    }

    /// Generate a type-safe statement using typed generation context
    pub fn generate_type_safe_statement<T: Rng + SeedableRng>(
        external_variables: &[Variable],
        external_functions: Rc<RefCell<Vec<Function>>>,
        current_indentation_layer: usize,
        max_depth: Option<usize>,
        typed_context: &mut TypedGenerationContext,
        rng: &mut T,
    ) -> Option<Self> {
        Self::generate_type_safe_statement_with_return_type(
            external_variables,
            external_functions,
            current_indentation_layer,
            max_depth,
            typed_context,
            None,
            rng,
        )
    }

    /// Generate a type-safe statement with expected return type
    pub fn generate_type_safe_statement_with_return_type<T: Rng + SeedableRng>(
        external_variables: &[Variable],
        external_functions: Rc<RefCell<Vec<Function>>>,
        current_indentation_layer: usize,
        max_depth: Option<usize>,
        typed_context: &mut TypedGenerationContext,
        expected_return_type: Option<&Type>,
        rng: &mut T,
    ) -> Option<Self> {
        if matches!(max_depth, Some(0)) {
            return None;
        }

        let max_depth = max_depth.unwrap_or(Self::MAX_DEPTH);

        // If depth is 1, only generate simple statements
        if max_depth == 1 {
            return Some(Statement::Single(
                SingleStatement::generate_type_safe_single_statement_with_return_type(
                    external_variables,
                    external_functions,
                    typed_context,
                    expected_return_type,
                    rng,
                ),
            ));
        }

        // For now, prefer simple statements to avoid complex nested type checking
        // This can be enhanced later with type-safe compound statements
        Some(match rng.random_range(0..10) {
            0..=7 => Statement::Single(
                SingleStatement::generate_type_safe_single_statement_with_return_type(
                    external_variables,
                    external_functions.clone(),
                    typed_context,
                    expected_return_type,
                    rng,
                ),
            ),
            8 => Statement::If(IfStatement::generate_type_safe_if_statement(
                external_variables,
                external_functions,
                current_indentation_layer,
                max_depth - 1,
                typed_context,
                expected_return_type,
                rng,
            )?),
            9 => Statement::For(ForStatement::generate_type_safe_for_statement(
                external_variables,
                external_functions,
                current_indentation_layer,
                max_depth - 1,
                typed_context,
                expected_return_type,
                rng,
            )?),
            _ => Statement::While(WhileStatement::generate_type_safe_while_statement(
                external_variables,
                external_functions,
                current_indentation_layer,
                max_depth - 1,
                typed_context,
                expected_return_type,
                rng,
            )?),
        })
    }
}
