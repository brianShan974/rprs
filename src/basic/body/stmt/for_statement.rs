use rand::{Rng, SeedableRng};

use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

use crate::basic::body::block::{Block, INDENT_SIZE, SPACE};
use crate::basic::body::fun::function::Function;
use crate::basic::cls::basic_types::{BasicType, NumberType, SignedIntegerType};
use crate::basic::cls::class::Class;
use crate::basic::utils::generate_random_identifier;
use crate::basic::var::prefix::var_prefix::VariablePrefix;
use crate::basic::var::variable::Variable;
use crate::type_system::{Type, TypedGenerationContext};

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

    pub fn generate_random_for_statement<T: Rng + SeedableRng>(
        external_variables: Vec<Variable>,
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

        // Create loop variable and add it to external variables for the loop body
        let loop_variable = Variable::new(
            VariablePrefix::default(),
            loop_variable_name.clone(),
            None,
            Some(Class::Basic(BasicType::Number(NumberType::SignedInteger(
                SignedIntegerType::Int,
            )))),
        );
        let mut loop_body_variables = external_variables.clone();
        loop_body_variables.push(loop_variable);

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
            loop_body_variables,
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

    /// Generate a type-safe for statement with expected return type
    pub fn generate_type_safe_for_statement<T: Rng + SeedableRng>(
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

        // Generate loop variable name
        let loop_variable_name = generate_random_identifier(rng);

        // Create loop variable and add it to the context
        let loop_variable = Variable::new(
            VariablePrefix::default(),
            loop_variable_name.clone(),
            None,
            Some(Class::Basic(BasicType::Number(NumberType::SignedInteger(
                SignedIntegerType::Int,
            )))),
        );
        let _ = typed_context.add_variable(&loop_variable);

        // Add loop variable to external variables for the loop body
        let mut loop_body_variables = external_variables.clone();
        loop_body_variables.push(loop_variable);

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

        // Generate loop body with return type awareness
        let loop_block = Block::generate_type_safe_block_with_return_type(
            loop_body_variables,
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
