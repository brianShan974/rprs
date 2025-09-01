use derive_more::Display;
use rand::{Rng, SeedableRng};

use std::cell::RefCell;
use std::rc::Rc;

use crate::basic::body::fun::function::Function;
use crate::basic::body::stmt::{
    for_statement::ForStatement, if_statement::IfStatement, single_statement::SingleStatement,
    when_statement::WhenStatement, while_statement::WhileStatement,
};
use std::borrow::Cow;

use crate::basic::utils::GenerationConfig;
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

    /// Generate a type-safe statement using typed generation context
    pub fn generate_type_safe_statement<T: Rng + SeedableRng>(
        external_variables: &[Variable],
        external_functions: Rc<RefCell<Vec<Function>>>,
        current_indentation_layer: usize,
        max_depth: Option<usize>,
        typed_context: &mut TypedGenerationContext,
        rng: &mut T,
    ) -> Option<Self> {
        // Create a shared config to avoid cloning - use Cow for external variables
        let external_vars_cow: Cow<[Variable]> = Cow::Borrowed(external_variables);

        let mut shared_config = GenerationConfig::new(
            external_vars_cow.to_vec(), // Only clone when necessary
            external_functions,
            Some(typed_context.get_defined_classes().iter().map(|rc| rc.as_ref().clone()).collect()), // TODO: optimize to avoid cloning
            current_indentation_layer,
            max_depth.unwrap_or(Self::MAX_DEPTH),
        );

        Self::generate_type_safe_statement_with_return_type(
            &mut shared_config,
            external_variables,
            max_depth,
            typed_context,
            None,
            rng,
        )
    }

    /// Generate a type-safe statement with expected return type
    pub fn generate_type_safe_statement_with_return_type<T: Rng + SeedableRng>(
        config: &mut GenerationConfig,
        external_variables: &[Variable],
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
                    config.external_functions.clone(),
                    typed_context,
                    expected_return_type,
                    config.defined_classes.as_deref(), // Pass defined classes to single statement generation
                    rng,
                ),
            ));
        }

        // Generate statements with balanced distribution for better code variety
        Some(match rng.random_range(0..10) {
            0..=4 => Statement::Single(
                SingleStatement::generate_type_safe_single_statement_with_return_type(
                    external_variables,
                    config.external_functions.clone(),
                    typed_context,
                    expected_return_type,
                    config.defined_classes.as_deref(), // Pass defined classes to single statement generation
                    rng,
                ),
            ),
            5 => Statement::If(IfStatement::generate_type_safe_if_statement(
                config,
                external_variables,
                typed_context,
                expected_return_type,
                rng,
            )?),
            6 => Statement::For(ForStatement::generate_type_safe_for_statement(
                config,
                external_variables,
                typed_context,
                expected_return_type,
                rng,
            )?),
            7 => Statement::While(WhileStatement::generate_type_safe_while_statement(
                config,
                external_variables,
                typed_context,
                expected_return_type,
                rng,
            )?),
            8..=9 => Statement::Single(
                SingleStatement::generate_type_safe_single_statement_with_return_type(
                    external_variables,
                    config.external_functions.clone(),
                    typed_context,
                    expected_return_type,
                    config.defined_classes.as_deref(), // Fallback to single statement for when statements
                    rng,
                ),
            ),
            _ => unreachable!(),
        })
    }
}
