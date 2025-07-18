use crate::basic::{body::block::Block, expr::expression::Expression, var::variable::Variable};

pub struct WhenStatement {
    subject: Variable,
    arms: Vec<(Expression, Block)>,
    else_arm: Block,
}
