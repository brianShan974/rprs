use crate::basic::body::block::Block;
use crate::basic::utils::generate_random_identifier;
use crate::basic::var::variable::Variable;
use rand::Rng;
use std::fmt::Display;

#[derive(Debug)]
enum ForLoopType {
    RangeLoop {
        start: i32,
        end: i32,
        step: Option<i32>,
    },
}

pub struct ForStatement {
    loop_variable_name: String,
    loop_block: Block,
    loop_type: ForLoopType,
}

impl ForStatement {
    pub fn generate_random_for_statement(
        external_variables: Vec<Variable>,
        current_indentation_layer: usize,
    ) -> Self {
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
        let loop_block = Block::generate_random_block_with_depth(
            external_variables,
            current_indentation_layer,
            2, // Use smaller depth limit
        );

        Self {
            loop_variable_name,
            loop_block,
            loop_type,
        }
    }
}

impl Display for ForStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.loop_type {
            ForLoopType::RangeLoop { start, end, step } => {
                write!(f, "for ({} in {}..{})", self.loop_variable_name, start, end)?;
                if let Some(step_val) = step {
                    write!(f, " step {}", step_val)?;
                }
                writeln!(f, " {}", self.loop_block)?;
            }
        }
        Ok(())
    }
}
