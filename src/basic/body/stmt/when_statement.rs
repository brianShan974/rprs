use crate::basic::body::block::Block;
use crate::basic::expr::expression::Expression;
use crate::basic::var::variable::Variable;
use rand::Rng;
use std::fmt::Display;

pub struct WhenStatement {
    subject: Variable,
    arms: Vec<(Expression, Block)>,
    else_arm: Block,
}

impl Display for WhenStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "when ({}) {{", self.subject.output_declaration())?;

        for (condition, block) in &self.arms {
            writeln!(f, " {} -> {}", condition, block)?;
        }

        writeln!(f, " else -> {}", self.else_arm)?;
        write!(f, " }}")?;

        Ok(())
    }
}

impl WhenStatement {
    pub fn generate_random_when_statement(
        external_variables: Vec<Variable>,
        current_indentation_layer: usize,
    ) -> Self {
        let mut rng = rand::rng();

        // Generate subject variable
        let subject = Variable::generate_random_variable(true, false);

        // Generate arms
        let num_arms = rng.random_range(1..=2);
        let mut arms = Vec::with_capacity(num_arms);

        for _ in 0..num_arms {
            let condition = Expression::generate_random_expression(3);
            let block = Block::generate_random_block_with_depth(
                external_variables.clone(),
                current_indentation_layer + 1,
                2, // Use smaller depth limit
            );
            arms.push((condition, block));
        }

        // Generate else arm
        let else_arm = Block::generate_random_block_with_depth(
            external_variables,
            current_indentation_layer + 1,
            2, // Use smaller depth limit
        );

        Self {
            subject,
            arms,
            else_arm,
        }
    }
}
