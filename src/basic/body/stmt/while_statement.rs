use crate::basic::body::block::{Block, INDENT_SIZE, SPACE};
use crate::basic::expr::expression::Expression;
use crate::basic::var::variable::Variable;
use std::fmt::Display;

pub struct WhileStatement {
    current_indentation_layer: usize,
    condition: Expression,
    block: Block,
}

impl Display for WhileStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let indentation = SPACE.repeat(self.current_indentation_layer * INDENT_SIZE);
        writeln!(
            f,
            "{}while ({}) {}",
            indentation, self.condition, self.block
        )?;

        Ok(())
    }
}

impl WhileStatement {
    pub fn generate_random_while_statement(
        external_variables: Vec<Variable>,
        current_indentation_layer: usize,
        max_depth: usize,
    ) -> Option<Self> {
        if max_depth == 0 {
            return None;
        }

        let condition = Expression::generate_random_expression(3);
        let block = Block::generate_random_block(
            external_variables,
            current_indentation_layer,
            false,
            max_depth - 1,
        );

        Some(Self {
            current_indentation_layer,
            condition,
            block: block?,
        })
    }
}
