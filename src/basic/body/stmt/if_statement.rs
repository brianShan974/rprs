use crate::basic::body::block::Block;
use crate::basic::expr::expression::Expression;
use crate::basic::var::variable::Variable;
use rand::Rng;
use std::fmt::Display;

const MAX_ELSEIF_BLOCKS: usize = 2;

pub struct IfStatement {
    condition: Expression,
    if_block: Block,
    elseif_blocks: Vec<(Expression, Block)>,
    else_block: Option<Block>,
}

impl Display for IfStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Write the if statement
        write!(f, "if ({}) {}", self.condition, self.if_block)?;

        // Write else if blocks
        for (condition, block) in &self.elseif_blocks {
            write!(f, " else if ({}) {}", condition, block)?;
        }

        // Write else block
        if let Some(else_block) = &self.else_block {
            write!(f, " else {}", else_block)?;
        }

        Ok(())
    }
}

impl IfStatement {
    pub fn generate_random_if_statement(
        external_variables: Vec<Variable>,
        current_indentation_layer: usize,
        max_depth: Option<usize>,
    ) -> Self {
        let mut rng = rand::rng();

        // Generate condition
        let condition = Expression::generate_random_expression(3);

        // Generate if block
        let block_depth = max_depth.unwrap_or(2).saturating_sub(1);
        let if_block = Block::generate_random_block_with_depth(
            external_variables.clone(),
            current_indentation_layer + 1,
            block_depth,
        );

        // Generate else if blocks
        let num_elseif_blocks = rng.random_range(0..=MAX_ELSEIF_BLOCKS);
        let mut elseif_blocks = Vec::with_capacity(num_elseif_blocks);

        for _ in 0..num_elseif_blocks {
            let elseif_condition = Expression::generate_random_expression(3);
            let elseif_block = Block::generate_random_block_with_depth(
                external_variables.clone(),
                current_indentation_layer + 1,
                block_depth,
            );
            elseif_blocks.push((elseif_condition, elseif_block));
        }

        // Generate else block (50% chance)
        let else_block = if rng.random() {
            Some(Block::generate_random_block_with_depth(
                external_variables,
                current_indentation_layer + 1,
                block_depth,
            ))
        } else {
            None
        };

        Self {
            condition,
            if_block,
            elseif_blocks,
            else_block,
        }
    }
}
