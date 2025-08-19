use crate::basic::expr::expression::Expression;
use crate::basic::var::variable::Variable;
use rand::Rng;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone)]
pub enum SingleStatement {
    VariableDeclaration(Variable),
    Assignment(String, Expression),
    FunctionCall(String, Vec<Expression>),
    Return(Option<Expression>),
}

impl SingleStatement {
    pub fn generate_random_single_statement(
        external_variables: Vec<Variable>,
        external_functions: Rc<RefCell<Vec<crate::basic::body::fun::function::Function>>>,
    ) -> Self {
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
            2 => {
                // Generate function call
                let functions = external_functions.borrow();
                if functions.is_empty() {
                    // If no functions available, generate a variable declaration instead
                    let var = Variable::generate_random_variable(true, true);
                    SingleStatement::VariableDeclaration(var)
                } else {
                    // Choose a random function
                    let function = &functions[rng.random_range(0..functions.len())];
                    let function_name = function.get_name().to_string();

                    // Generate random arguments (0-3 expressions)
                    let num_args = rng.random_range(0..=3);
                    let mut args = Vec::with_capacity(num_args);
                    for _ in 0..num_args {
                        args.push(Expression::generate_random_expression(2));
                    }

                    SingleStatement::FunctionCall(function_name, args)
                }
            }
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
            SingleStatement::FunctionCall(function_name, args) => {
                let args_str = args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{}({})", function_name, args_str)
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
