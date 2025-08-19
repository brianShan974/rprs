use crate::basic::body::block::{Block, INDENT_SIZE, SPACE};
use crate::basic::expr::expression::Expression;
use crate::basic::var::variable::Variable;
use rand::Rng;
use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

#[derive(Clone)]
pub struct WhenStatement {
    current_indentation_layer: usize,
    subject: Variable,
    arms: Vec<(Expression, Block)>,
    else_arm: Block,
}

impl Display for WhenStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let indentation = SPACE.repeat(self.current_indentation_layer * INDENT_SIZE);
        writeln!(f, "{indentation}when ({}) {{", self.subject.get_name())?;

        for (condition, block) in &self.arms {
            writeln!(f, "{indentation}{} -> {}", condition, block)?;
        }

        writeln!(f, "{indentation}else -> {}", self.else_arm)?;

        Ok(())
    }
}

impl WhenStatement {
    pub const MAX_DEPTH: usize = 5;

    pub fn generate_random_when_statement(
        external_variables: Vec<Variable>,
        external_functions: Rc<RefCell<Vec<crate::basic::body::fun::function::Function>>>,
        current_indentation_layer: usize,
        max_depth: usize,
    ) -> Option<Self> {
        if max_depth == 0 {
            return None;
        }

        let mut rng = rand::rng();

        // Generate subject variable
        let subject = Variable::generate_random_variable(true, false);

        // Generate arms
        let num_arms = rng.random_range(1..=2);
        let mut arms = Vec::with_capacity(num_arms);

        for _ in 0..num_arms {
            let condition = Expression::generate_random_expression(3);
            let block = Block::generate_random_block(
                external_variables.clone(),
                external_functions.clone(),
                current_indentation_layer,
                false,
                max_depth - 1,
            );
            arms.push((condition, block?));
        }

        // Generate else arm
        let else_arm = Block::generate_random_block(
            external_variables,
            external_functions,
            current_indentation_layer,
            false,
            max_depth - 1,
        );

        Some(Self {
            current_indentation_layer,
            subject,
            arms,
            else_arm: else_arm?,
        })
    }
}
