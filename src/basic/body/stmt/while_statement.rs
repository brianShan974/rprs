use rand::{Rng, SeedableRng};

use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

use crate::basic::body::block::{Block, INDENT_SIZE, SPACE};
use crate::basic::body::fun::function::Function;
use crate::basic::expr::expression::Expression;
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
        writeln!(
            f,
            "{}while ({}) {}",
            indentation, self.condition, self.block
        )?;

        Ok(())
    }
}

impl WhileStatement {
    pub fn generate_random_while_statement<T: Rng + SeedableRng>(
        external_variables: Vec<Variable>,
        external_functions: Rc<RefCell<Vec<Function>>>,
        current_indentation_layer: usize,
        max_depth: usize,
        rng: &mut T,
    ) -> Option<Self> {
        if max_depth == 0 {
            return None;
        }

        let condition = Expression::generate_random_expression(3, None, rng);
        let block = Block::generate_random_block(
            external_variables,
            external_functions,
            current_indentation_layer,
            false,
            max_depth - 1,
            rng,
        );

        Some(Self {
            current_indentation_layer,
            condition,
            block: block?,
        })
    }

    pub fn get_condition(&self) -> &Expression {
        &self.condition
    }

    pub fn get_block(&self) -> &Block {
        &self.block
    }

    /// Generate a type-safe while statement with expected return type
    pub fn generate_type_safe_while_statement<T: Rng + SeedableRng>(
        external_variables: Vec<Variable>,
        external_functions: Rc<RefCell<Vec<Function>>>,
        current_indentation_layer: usize,
        max_depth: usize,
        typed_context: &mut TypedGenerationContext,
        expected_return_type: Option<&Type>,
        rng: &mut T,
    ) -> Option<Self> {
        if max_depth == 0 {
            return None;
        }

        // Generate condition
        let condition = Expression::generate_random_expression(3, None, rng);

        // Generate block with return type awareness
        let block = Block::generate_type_safe_block_with_return_type(
            external_variables,
            external_functions,
            current_indentation_layer,
            false,
            max_depth - 1,
            typed_context,
            expected_return_type,
            rng,
        )?;

        Some(Self {
            current_indentation_layer,
            condition,
            block,
        })
    }
}
