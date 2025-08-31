use rand::{Rng, SeedableRng};

use std::fmt::Display;

use crate::basic::body::block::{Block, INDENT_SIZE, SPACE};

use crate::basic::expr::boolean_expression::BooleanExpression;
use crate::basic::utils::GenerationConfig;
use crate::basic::var::variable::Variable;
use crate::type_system::{Type, TypedGenerationContext};

const MAX_ELSEIF_BLOCKS: usize = 2;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IfStatement {
    current_indentation_layer: usize,
    condition: BooleanExpression,
    if_block: Block,
    elseif_blocks: Vec<(BooleanExpression, Block)>,
    else_block: Option<Block>,
}

impl IfStatement {
    pub const MAX_DEPTH: usize = 5;

    pub fn get_condition(&self) -> &BooleanExpression {
        &self.condition
    }

    pub fn get_if_block(&self) -> &Block {
        &self.if_block
    }

    pub fn get_elseif_blocks(&self) -> &Vec<(BooleanExpression, Block)> {
        &self.elseif_blocks
    }

    pub fn get_else_block(&self) -> Option<&Block> {
        self.else_block.as_ref()
    }

    /// Generate a type-safe if statement with expected return type
    pub fn generate_type_safe_if_statement<T: Rng + SeedableRng>(
        config: &mut GenerationConfig,
        external_variables: &[Variable],
        typed_context: &mut TypedGenerationContext,
        expected_return_type: Option<&Type>,
        rng: &mut T,
    ) -> Option<Self> {
        if config.max_depth == 0 {
            return None;
        }

        // Generate condition
        let condition = BooleanExpression::generate_random_boolean_expression(
            3,
            Some(config.external_functions.clone()),
            Some(external_variables),
            rng,
        );

        // Generate if block with return type awareness
        let if_block = Block::generate_type_safe_block_with_config(
            config,
            external_variables,
            false,
            typed_context,
            expected_return_type,
            rng,
        )?;

        // Generate else if blocks with return type awareness
        let num_elseif_blocks = rng.random_range(0..=MAX_ELSEIF_BLOCKS);
        let mut elseif_blocks = Vec::with_capacity(num_elseif_blocks);

        for _ in 0..num_elseif_blocks {
            let elseif_condition = BooleanExpression::generate_random_boolean_expression(
                3,
                Some(config.external_functions.clone()),
                Some(external_variables),
                rng,
            );
            if let Some(elseif_block) = Block::generate_type_safe_block_with_config(
                config,
                external_variables,
                false,
                typed_context,
                expected_return_type,
                rng,
            ) {
                elseif_blocks.push((elseif_condition, elseif_block));
            };
        }

        // Generate else block with return type awareness (50% chance)
        let else_block = if rng.random_bool(1.0 / 2.0) {
            Some(Block::generate_type_safe_block_with_config(
                config,
                external_variables,
                false,
                typed_context,
                expected_return_type,
                rng,
            )?)
        } else {
            None
        };

        Some(Self {
            current_indentation_layer: config.current_indentation_layer,
            condition,
            if_block,
            elseif_blocks,
            else_block,
        })
    }
}

impl Display for IfStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let indentation = SPACE.repeat(self.current_indentation_layer * INDENT_SIZE);
        // Write the if statement
        write!(f, "{indentation}if ({}) {}", self.condition, self.if_block)?;

        // Write else if blocks
        for (condition, block) in &self.elseif_blocks {
            write!(f, " else if ({condition}) {block}")?;
        }

        // Write else block
        if let Some(else_block) = &self.else_block {
            write!(f, " else {else_block}")?;
        }

        Ok(())
    }
}
