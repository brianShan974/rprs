use crate::basic::{body::block::Block, expr::expression::Expression};

pub struct IfStatement {
    condition: Expression,
    if_block: Block,
    elseif_blocks: Vec<(Expression, Block)>,
    else_block: Block,
}
