use crate::basic::body::fun::function::Function;
use crate::basic::expr::expression::Expression;
use crate::basic::var::variable::Variable;
use crate::type_system::{Type, TypedGenerationContext};
use rand::{Rng, SeedableRng};
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
    pub fn generate_random_single_statement<T: Rng + SeedableRng>(
        external_variables: Vec<Variable>,
        external_functions: Rc<RefCell<Vec<Function>>>,
        rng: &mut T,
    ) -> Self {
        match rng.random_range(0..4) {
            0 => {
                let var = Variable::generate_random_variable(true, true, rng);
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
                    let var = Variable::generate_random_variable(true, true, rng);
                    // Don't add the new variable to external_variables
                    SingleStatement::VariableDeclaration(var)
                } else {
                    // Choose a random mutable variable
                    let var_name =
                        available_vars[rng.random_range(0..available_vars.len())].clone();
                    let expr = Expression::generate_random_expression(3, rng);
                    SingleStatement::Assignment(var_name, expr)
                }
            }
            2 => {
                // Generate function call
                let functions = external_functions.borrow();
                if functions.is_empty() {
                    // If no functions available, generate a variable declaration instead
                    let var = Variable::generate_random_variable(true, true, rng);
                    SingleStatement::VariableDeclaration(var)
                } else {
                    // Choose a random function
                    let function = &functions[rng.random_range(0..functions.len())];
                    let function_name = function.get_name().to_string();

                    // Generate random arguments (0-3 expressions)
                    let num_args = rng.random_range(0..=3);
                    let mut args = Vec::with_capacity(num_args);
                    for _ in 0..num_args {
                        args.push(Expression::generate_random_expression(2, rng));
                    }

                    SingleStatement::FunctionCall(function_name, args)
                }
            }
            _ => {
                if rng.random() {
                    SingleStatement::Return(Some(Expression::generate_random_expression(3, rng)))
                } else {
                    SingleStatement::Return(None)
                }
            }
        }
    }

    /// Generate a type-safe single statement using typed generation context
    pub fn generate_type_safe_single_statement<T: Rng + SeedableRng>(
        external_variables: Vec<Variable>,
        external_functions: Rc<RefCell<Vec<Function>>>,
        typed_context: &mut TypedGenerationContext,
        rng: &mut T,
    ) -> Self {
        Self::generate_type_safe_single_statement_with_return_type(
            external_variables,
            external_functions,
            typed_context,
            None,
            rng,
        )
    }

    /// Generate a type-safe single statement with expected return type
    pub fn generate_type_safe_single_statement_with_return_type<T: Rng + SeedableRng>(
        external_variables: Vec<Variable>,
        external_functions: Rc<RefCell<Vec<Function>>>,
        typed_context: &mut TypedGenerationContext,
        expected_return_type: Option<&Type>,
        rng: &mut T,
    ) -> Self {
        // Add external variables to typed context
        for var in &external_variables {
            let _ = typed_context.add_variable(var);
        }

        // Add external functions to typed context
        {
            let functions = external_functions.borrow();
            for func in functions.iter() {
                let _ = typed_context.add_function(func);
            }
        }

        match rng.random_range(0..4) {
            0 => {
                // Generate a type-compatible variable
                let var = typed_context.generate_type_compatible_variable(rng);
                // Add the new variable to context
                let _ = typed_context.add_variable(&var);
                SingleStatement::VariableDeclaration(var)
            }
            1 => {
                // Generate type-safe assignment
                let mutable_vars = typed_context.get_mutable_variables(&external_variables);
                if !mutable_vars.is_empty() {
                    let var_name = &mutable_vars[rng.random_range(0..mutable_vars.len())];
                    match typed_context.generate_type_safe_assignment(var_name, rng) {
                        Ok(assignment) => assignment,
                        Err(_) => {
                            // Fallback to variable declaration
                            let var = typed_context.generate_type_compatible_variable(rng);
                            let _ = typed_context.add_variable(&var);
                            SingleStatement::VariableDeclaration(var)
                        }
                    }
                } else {
                    // No mutable variables, generate new variable
                    let var = typed_context.generate_type_compatible_variable(rng);
                    let _ = typed_context.add_variable(&var);
                    SingleStatement::VariableDeclaration(var)
                }
            }
            2 => {
                // Generate type-safe function call
                let available_funcs = typed_context.get_available_functions();
                if !available_funcs.is_empty() {
                    let func_name = &available_funcs[rng.random_range(0..available_funcs.len())];
                    match typed_context.generate_type_safe_function_call(func_name, rng) {
                        Ok(call) => call,
                        Err(_) => {
                            // Fallback to variable declaration
                            let var = typed_context.generate_type_compatible_variable(rng);
                            let _ = typed_context.add_variable(&var);
                            SingleStatement::VariableDeclaration(var)
                        }
                    }
                } else {
                    // No functions available, generate variable
                    let var = typed_context.generate_type_compatible_variable(rng);
                    let _ = typed_context.add_variable(&var);
                    SingleStatement::VariableDeclaration(var)
                }
            }
            _ => {
                // Generate type-aware return statement with expected return type
                typed_context
                    .generate_type_safe_return_statement_with_type(expected_return_type, rng)
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
