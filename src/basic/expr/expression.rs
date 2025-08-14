use super::arithmetich_expression::ArithmeticExpression;
use derive_more::Display;

#[derive(Clone, Debug, Display)]
#[display("{}", _0)]
pub enum Expression {
    Arithmetic(ArithmeticExpression),
}

impl Expression {
    pub fn generate_random_expression(max_depth: usize) -> Self {
        Self::Arithmetic(ArithmeticExpression::generate_random_expression(max_depth))
    }

    pub fn is_arithmetic(&self) -> bool {
        matches!(self, Self::Arithmetic(_))
    }
}
