use rand::{Rng, SeedableRng};

use std::fmt::Display;

use crate::basic::body::block::{Block, INDENT_SIZE, SPACE};
use crate::basic::cls::class::INT;
use crate::basic::utils::{GenerationConfig, generate_random_identifier};
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

    pub fn get_loop_block(&self) -> &Block {
        &self.loop_block
    }

    pub fn get_loop_variable_name(&self) -> &str {
        &self.loop_variable_name
    }

    /// Generate a type-safe for statement with expected return type
    pub fn generate_type_safe_for_statement<T: Rng + SeedableRng>(
        config: &mut GenerationConfig,
        external_variables: &[Variable],
        typed_context: &mut TypedGenerationContext,
        expected_return_type: Option<&Type>,
        rng: &mut T,
    ) -> Option<Self> {
        if config.max_depth == 0 {
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
        let loop_block = Block::generate_type_safe_block_with_config(
            &mut GenerationConfig::new(
                external_variables.to_vec(),
                config.external_functions.clone(),
                config
                    .defined_classes
                    .as_ref()
                    .map(|classes| classes.to_vec()),
                config.current_indentation_layer,
                config.max_depth - 1,
            ),
            external_variables, // Use original external variables, not including loop variable
            false,
            &mut loop_context, // Use the child context
            expected_return_type,
            rng,
        )?;

        Some(Self {
            current_indentation_layer: config.current_indentation_layer,
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
