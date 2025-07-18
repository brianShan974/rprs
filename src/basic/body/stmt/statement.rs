use super::{
    for_statement::ForStatement, if_statement::IfStatement, single_statement::SingleStatement,
    when_statement::WhenStatement, while_statement::WhileStatement,
};

pub enum Statement {
    Single(SingleStatement),
    If(IfStatement),
    For(ForStatement),
    While(WhileStatement),
    When(WhenStatement),
}
