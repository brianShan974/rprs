use crate::basic::{body::block::Block, expr::expression::Expression};

pub struct WhileStatement {
    condition: Expression,
    block: Block,
}
