use crate::basic::expr::expression::Expression;
use crate::basic::var::variable::Variable;
use rand::Rng;

pub enum SingleStatement {
    VariableDeclaration(Variable),
    Assignment(String, Expression),
    Expression(Expression),
    Return(Option<Expression>),
}

impl SingleStatement {
    pub fn generate_random_single_statement(external_variables: Vec<Variable>) -> Self {
        let mut rng = rand::rng();

        match rng.random_range(0..4) {
            0 => {
                let var = Variable::generate_random_variable(true, true);
                // Don't add the new variable to external_variables
                SingleStatement::VariableDeclaration(var)
            }
            1 => {
                // Get available mutable variables
                let available_vars = external_variables
                    .iter()
                    .filter(|v| v.is_mutable()) // Only mutable variables
                    .map(|v| v.get_name().to_string())
                    .collect::<Vec<_>>();

                if available_vars.is_empty() {
                    // If no mutable variables available, generate a new variable declaration instead
                    let var = Variable::generate_random_variable(true, true);
                    // Don't add the new variable to external_variables
                    SingleStatement::VariableDeclaration(var)
                } else {
                    // Choose a random mutable variable
                    let var_name =
                        available_vars[rng.random_range(0..available_vars.len())].clone();
                    let expr = Expression::generate_random_expression(3);
                    SingleStatement::Assignment(var_name, expr)
                }
            }
            2 => SingleStatement::Expression(Expression::generate_random_expression(3)),
            _ => {
                if rng.random() {
                    SingleStatement::Return(Some(Expression::generate_random_expression(3)))
                } else {
                    SingleStatement::Return(None)
                }
            }
        }
    }
}

impl std::fmt::Display for SingleStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SingleStatement::VariableDeclaration(var) => {
                if let Some(init) = var.output_init() {
                    write!(f, "{}", init)
                } else {
                    write!(f, "{}", var.output_declaration())
                }
            }
            SingleStatement::Assignment(var_name, expr) => {
                write!(f, "{} = {}", var_name, expr)
            }
            SingleStatement::Expression(expr) => {
                write!(f, "{}", expr)
            }
            SingleStatement::Return(expr) => {
                if let Some(expr) = expr {
                    write!(f, "return {}", expr)
                } else {
                    write!(f, "return")
                }
            }
        }
    }
}
