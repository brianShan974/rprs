use crate::basic::body::block::{Block, INDENT_SIZE, SPACE};
use crate::basic::expr::expression::Expression;
use crate::basic::var::variable::Variable;
use rand::Rng;
use std::fmt::Display;

const MAX_ELSEIF_BLOCKS: usize = 2;

pub struct IfStatement {
    current_indentation_layer: usize,
    condition: Expression,
    if_block: Block,
    elseif_blocks: Vec<(Expression, Block)>,
    else_block: Option<Block>,
}

impl IfStatement {
    pub const MAX_DEPTH: usize = 5;

    pub fn generate_random_if_statement(
        external_variables: Vec<Variable>,
        current_indentation_layer: usize,
        max_depth: usize,
    ) -> Option<Self> {
        if max_depth == 0 {
            return None;
        }

        let mut rng = rand::rng();

        // Generate condition
        let condition = Expression::generate_random_expression(3);

        // Generate if block
        let if_block = Block::generate_random_block(
            external_variables.clone(),
            current_indentation_layer,
            false,
            max_depth - 1,
        )?;

        // Generate else if blocks
        let num_elseif_blocks = rng.random_range(0..=MAX_ELSEIF_BLOCKS);
        let mut elseif_blocks = Vec::with_capacity(num_elseif_blocks);

        for _ in 0..num_elseif_blocks {
            let elseif_condition = Expression::generate_random_expression(3);
            if let Some(elseif_block) = Block::generate_random_block(
                external_variables.clone(),
                current_indentation_layer,
                false,
                max_depth - 1,
            ) {
                elseif_blocks.push((elseif_condition, elseif_block));
            };
        }

        // Generate else block (50% chance)
        let else_block = if rng.random() {
            Some(Block::generate_random_block(
                external_variables,
                current_indentation_layer,
                false,
                max_depth - 1,
            )?)
        } else {
            None
        };

        Some(Self {
            current_indentation_layer,
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
            writeln!(f, " else {else_block}")?;
        } else {
            writeln!(f)?;
        }

        Ok(())
    }
}
