use crate::basic::body::block::Block;
use crate::basic::expr::expression::Expression;
use crate::basic::var::variable::Variable;
use std::fmt::Display;

pub struct WhileStatement {
    condition: Expression,
    block: Block,
}

impl Display for WhileStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "while ({}) {}", self.condition, self.block)?;

        Ok(())
    }
}

impl WhileStatement {
    pub fn generate_random_while_statement(
        external_variables: Vec<Variable>,
        current_indentation_layer: usize,
    ) -> Self {
        let condition = Expression::generate_random_expression(3);
        let block = Block::generate_random_block_with_depth(
            external_variables,
            current_indentation_layer,
            2,
        );

        Self { condition, block }
    }
}
