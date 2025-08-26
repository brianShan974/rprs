use rand::{Rng, SeedableRng};

use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

use crate::basic::body::block::{Block, INDENT_SIZE, SPACE};
use crate::basic::body::fun::function::Function;
use crate::basic::cls::class::INT;
use crate::basic::utils::generate_random_identifier;
use crate::basic::var::prefix::var_prefix::VariablePrefix;
use crate::basic::var::variable::Variable;
use crate::type_system::{Type, TypedGenerationContext};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum ForLoopType {
    RangeLoop {
        start: i32,
        end: i32,
        step: Option<i32>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ForStatement {
    current_indentation_layer: usize,
    loop_variable_name: String,
    loop_block: Block,
    loop_type: ForLoopType,
}

impl ForStatement {
    pub const MAX_DEPTH: usize = 5;

    pub fn generate_random_for_statement<T: Rng + SeedableRng>(
        external_variables: &[Variable],
        external_functions: Rc<RefCell<Vec<Function>>>,
        current_indentation_layer: usize,
        max_depth: usize,
        rng: &mut T,
    ) -> Option<Self> {
        if max_depth == 0 {
            return None;
        }

        // Generate loop variable name
        let loop_variable_name = generate_random_identifier(rng);

        // Create loop variable (but don't add it to external variables)
        let loop_variable = Variable::new(
            VariablePrefix::default(),
            loop_variable_name.clone(),
            None,
            Some(INT),
        );

        // Only generate range loop
        let loop_type = ForLoopType::RangeLoop {
            start: rng.random_range(0..10),
            end: rng.random_range(10..50),
            step: if rng.random_bool(1.0 / 2.0) {
                Some(rng.random_range(1..5))
            } else {
                None
            },
        };

        // Generate loop body with smaller depth limit
        // Create a combined list that includes the loop variable for the loop body scope
        let loop_body_variables: Vec<Variable> = external_variables
            .iter()
            .map(|v| v.to_owned())
            .chain(std::iter::once(loop_variable.clone()))
            .collect();

        let loop_block = Block::generate_random_block(
            &loop_body_variables, // Include loop variable in loop body scope
            external_functions,
            current_indentation_layer,
            false,
            max_depth - 1,
            rng,
        )?;

        Some(Self {
            current_indentation_layer,
            loop_variable_name,
            loop_block,
            loop_type,
        })
    }

    pub fn get_loop_block(&self) -> &Block {
        &self.loop_block
    }

    pub fn get_loop_variable_name(&self) -> &str {
        &self.loop_variable_name
    }

    /// Generate a type-safe for statement with expected return type
    pub fn generate_type_safe_for_statement<T: Rng + SeedableRng>(
        external_variables: &[Variable],
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

        // Generate loop variable name
        let loop_variable_name = generate_random_identifier(rng);

        // Create loop variable (but don't add it to the main context)
        let loop_variable = Variable::new(
            VariablePrefix::default(),
            loop_variable_name.clone(),
            None,
            Some(INT),
        );

        // Create a child context for the loop body to ensure proper variable scoping
        let mut loop_context = typed_context.create_child_context();

        // Add the loop variable to the child context only
        let _ = loop_context.add_variable(&loop_variable);

        // Only generate range loop
        let loop_type = ForLoopType::RangeLoop {
            start: rng.random_range(0..10),
            end: rng.random_range(10..50),
            step: if rng.random_bool(1.0 / 2.0) {
                Some(rng.random_range(1..5))
            } else {
                None
            },
        };

        // Generate loop body with return type awareness using child context
        let loop_block = Block::generate_type_safe_block_with_return_type(
            external_variables, // Use original external variables, not including loop variable
            external_functions,
            current_indentation_layer,
            false,
            max_depth - 1,
            &mut loop_context, // Use the child context
            expected_return_type,
            None, // defined_classes
            rng,
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
