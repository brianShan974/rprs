use std::fmt::Display;

use crate::basic::body::block::{Block, INDENT_SIZE, SPACE};
use crate::basic::expr::expression::Expression;
use crate::basic::var::variable::Variable;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

    pub fn get_subject(&self) -> &Variable {
        &self.subject
    }

    pub fn get_arms(&self) -> &Vec<(Expression, Block)> {
        &self.arms
    }

    pub fn get_else_arm(&self) -> &Block {
        &self.else_arm
    }
}
