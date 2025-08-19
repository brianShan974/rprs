use crate::basic::body::block::{Block, INDENT_SIZE, SPACE};
use crate::basic::utils::generate_random_identifier;
use crate::basic::var::variable::Variable;
use rand::Rng;
use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

#[derive(Clone, Debug)]
enum ForLoopType {
    RangeLoop {
        start: i32,
        end: i32,
        step: Option<i32>,
    },
}

#[derive(Clone)]
pub struct ForStatement {
    current_indentation_layer: usize,
    loop_variable_name: String,
    loop_block: Block,
    loop_type: ForLoopType,
}

impl ForStatement {
    pub const MAX_DEPTH: usize = 5;

    pub fn generate_random_for_statement(
        external_variables: Vec<Variable>,
        external_functions: Rc<RefCell<Vec<crate::basic::body::fun::function::Function>>>,
        current_indentation_layer: usize,
        max_depth: usize,
    ) -> Option<Self> {
        if max_depth == 0 {
            return None;
        }

        let mut rng = rand::rng();

        // Generate loop variable name
        let loop_variable_name = generate_random_identifier();

        // Only generate range loop
        let loop_type = ForLoopType::RangeLoop {
            start: rng.random_range(0..10),
            end: rng.random_range(10..50),
            step: if rng.random() {
                Some(rng.random_range(1..5))
            } else {
                None
            },
        };

        // Generate loop body with smaller depth limit
        let loop_block = Block::generate_random_block(
            external_variables,
            external_functions,
            current_indentation_layer,
            false,
            max_depth - 1,
        )?;

        Some(Self {
            current_indentation_layer,
            loop_variable_name,
            loop_block,
            loop_type,
        })
    }
}

impl Display for ForStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let indentation = SPACE.repeat(self.current_indentation_layer * INDENT_SIZE);
        match &self.loop_type {
            ForLoopType::RangeLoop { start, end, step } => {
                write!(
                    f,
                    "{indentation}for ({} in {}..{}) ",
                    self.loop_variable_name, start, end
                )?;
                if let Some(step_val) = step {
                    write!(f, "step {} ", step_val)?;
                }
                write!(f, "{}", self.loop_block)?;
            }
        }
        Ok(())
    }
}
