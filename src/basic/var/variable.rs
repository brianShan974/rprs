use super::prefix::var_prefix::VariablePrefix;
use crate::basic::cls::basic_types::{BasicType, FloatingPointType, NumberType};
use crate::basic::cls::class::Class;
use crate::basic::expr::expression::Expression;
use crate::basic::utils::generate_random_identifier;
use derive_more::Constructor;

#[derive(Constructor, Clone, Debug)]
pub struct Variable {
    prefix: VariablePrefix,
    name: String,
    value: Option<Expression>,
    ty: Option<Class>,
}

impl Variable {
    pub fn output_declaration(&self) -> String {
        match self.ty {
            Some(ref ty) => format!("{}{}: {}", self.prefix, self.name, ty),
            _ => format!("{}{}", self.prefix, self.name),
        }
    }

    pub fn output_assignment(&self) -> Option<String> {
        self.value
            .as_ref()
            .map(|value| format!("{} = {}", self.name, value))
    }

    pub fn output_init(&self) -> Option<String> {
        self.value
            .as_ref()
            .map(|value| format!("{}{} = {}", self.prefix, self.name, value))
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn is_mutable(&self) -> bool {
        self.prefix.is_mutable()
    }

    pub fn generate_random_variable(is_stack: bool, with_initial_value: bool) -> Self {
        let prefix = VariablePrefix::generate_random_prefix(is_stack);
        let name = generate_random_identifier();
        let value = if with_initial_value {
            Some(Expression::generate_random_expression(5))
        } else {
            None
        };
        let ty = if let Some(value) = &value
            && value.is_arithmetic()
        {
            Some(Class::Basic(BasicType::Number(NumberType::FloatingPoint(
                FloatingPointType::Float,
            ))))
        } else {
            None
        };

        Self {
            prefix,
            name,
            value,
            ty,
        }
    }
}
