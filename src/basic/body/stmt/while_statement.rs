use rand::{Rng, SeedableRng};

use std::fmt::Display;

use crate::basic::body::block::{Block, INDENT_SIZE, SPACE};
use crate::basic::expr::boolean_expression::BooleanExpression;
use crate::basic::expr::expression::Expression;
use crate::basic::utils::GenerationConfig;
use crate::basic::var::variable::Variable;
use crate::type_system::{Type, TypedGenerationContext};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct WhileStatement {
    current_indentation_layer: usize,
    condition: Expression,
    block: Block,
}

impl Display for WhileStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let indentation = SPACE.repeat(self.current_indentation_layer * INDENT_SIZE);
        write!(
            f,
            "{}while ({}) {}",
            indentation, self.condition, self.block
        )?;

        Ok(())
    }
}

impl WhileStatement {
    pub fn get_condition(&self) -> &Expression {
        &self.condition
    }

    pub fn get_block(&self) -> &Block {
        &self.block
    }

    /// Generate a type-safe while statement with expected return type
    pub fn generate_type_safe_while_statement<T: Rng + SeedableRng>(
        config: &mut GenerationConfig,
        external_variables: &[Variable],
        typed_context: &mut TypedGenerationContext,
        expected_return_type: Option<&Type>,
        rng: &mut T,
    ) -> Option<Self> {
        if config.max_depth == 0 {
            return None;
        }

        // Generate condition - must be Boolean type for while loop
        let condition = Expression::Boolean(BooleanExpression::generate_random_boolean_expression(
            3,
            Some(config.external_functions.clone()), // Pass available external functions
            Some(external_variables),
            rng,
        ));

        // Create a child context for the loop body to ensure proper variable scoping
        let mut loop_context = typed_context.create_child_context();

        // Enter loop context for break/continue support
        loop_context.enter_loop(crate::type_system::typed_generator::LoopType::While);

        // Generate block with return type awareness using loop context
        let block = Block::generate_type_safe_block_with_config(
            &mut GenerationConfig::new(
                external_variables.to_vec(),
                config.external_functions.clone(),
                config
                    .defined_classes
                    .as_ref()
                    .map(|classes| classes.to_vec()),
                config.current_indentation_layer,
                config.max_depth - 1,
            ),
            external_variables,
            false,
            &mut loop_context,
            expected_return_type,
            rng,
        )?;

        Some(Self {
            current_indentation_layer: config.current_indentation_layer,
            condition,
            block,
        })
    }
}
