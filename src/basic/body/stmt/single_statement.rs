use rand::seq::IndexedRandom;
use rand::{Rng, SeedableRng};

use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

use crate::basic::body::fun::function::Function;
use crate::basic::cls::class::Class;
use crate::basic::expr::arithmetic_expression::ArithmeticExpression;
use crate::basic::expr::expression::Expression;
use crate::basic::obj::object_instance::ObjectInstance;
use crate::basic::var::variable::Variable;
use crate::type_system::{Type, TypedGenerationContext};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CompoundAssignmentOperator {
    AddAssign,      // +=
    SubtractAssign, // -=
    MultiplyAssign, // *=
    DivideAssign,   // /=
}

impl CompoundAssignmentOperator {
    pub fn generate_random_compound_assignment_operator<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        match rng.random_range(0..4) {
            0 => CompoundAssignmentOperator::AddAssign,
            1 => CompoundAssignmentOperator::SubtractAssign,
            2 => CompoundAssignmentOperator::MultiplyAssign,
            _ => CompoundAssignmentOperator::DivideAssign,
        }
    }
}

impl Display for CompoundAssignmentOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompoundAssignmentOperator::AddAssign => write!(f, "+="),
            CompoundAssignmentOperator::SubtractAssign => write!(f, "-="),
            CompoundAssignmentOperator::MultiplyAssign => write!(f, "*="),
            CompoundAssignmentOperator::DivideAssign => write!(f, "/="),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SingleStatement {
    VariableDeclaration(Variable),
    Assignment(String, Expression),
    CompoundAssignment(String, CompoundAssignmentOperator, Expression),
    FunctionCall(String, Vec<Expression>),
    ObjectCreation(ObjectInstance),
    Return(Option<Expression>),
}

impl SingleStatement {
    pub fn generate_random_single_statement<T: Rng + SeedableRng>(
        external_variables: &[Variable],
        external_functions: Rc<RefCell<Vec<Function>>>,
        defined_classes: Option<&[Class]>,
        rng: &mut T,
    ) -> Self {
        match rng.random_range(0..10) as u32 {
            0 => {
                let var = Variable::generate_random_variable_with_const_control(
                    false,
                    true,
                    Some(external_variables),
                    false,
                    rng,
                );
                // Don't add the new variable to external_variables
                SingleStatement::VariableDeclaration(var)
            }
            1 => {
                // Get available mutable variables
                let available_vars = external_variables
                    .iter()
                    .filter(|v| v.is_mutable())
                    .map(|v| v.get_name().to_string())
                    .collect::<Vec<_>>();

                if available_vars.is_empty() {
                    // If no mutable variables available, generate a new variable declaration instead
                    let var = Variable::generate_random_variable_with_const_control(
                        false,
                        true,
                        Some(external_variables),
                        false,
                        rng,
                    );
                    // Don't add the new variable to external_variables
                    SingleStatement::VariableDeclaration(var)
                } else {
                    // Choose a random mutable variable
                    let var_name = available_vars.choose(rng).unwrap().clone();

                    // Find the variable to get its type
                    let var_type = external_variables
                        .iter()
                        .find(|v| v.get_name() == var_name)
                        .and_then(|v| v.get_class());

                    let expr = if let Some(target_type) = var_type {
                        // Generate expression of matching type
                        Expression::generate_expression_for_type(
                            target_type,
                            3,
                            Some(external_functions.clone()),
                            Some(external_variables),
                            rng,
                        )
                    } else {
                        // Fallback to random expression if type not found
                        Expression::generate_random_expression(
                            3,
                            Some(external_functions.clone()),
                            Some(external_variables),
                            defined_classes, // Pass defined classes to expression generation
                            rng,
                        )
                    };
                    SingleStatement::Assignment(var_name, expr)
                }
            }
            2 => {
                // Generate function call
                let functions = external_functions.borrow();
                if functions.is_empty() {
                    // If no functions available, generate a variable declaration instead
                    let var = Variable::generate_random_variable_with_const_control(
                        false,
                        true,
                        Some(external_variables),
                        false,
                        rng,
                    );
                    SingleStatement::VariableDeclaration(var)
                } else {
                    // Only allow top-level functions to avoid cross-class method calls
                    let available_functions: Vec<_> = functions
                        .iter()
                        .filter(|func| !func.is_class_method())
                        .collect();

                    if available_functions.is_empty() {
                        // If no functions available, generate a variable declaration instead
                        let var = Variable::generate_random_variable_with_const_control(
                            false,
                            true,
                            Some(external_variables),
                            false,
                            rng,
                        );
                        return SingleStatement::VariableDeclaration(var);
                    }

                    // Choose a random function
                    let function = available_functions.choose(rng).unwrap();
                    let function_name = function.get_name().to_string();

                    // Generate arguments that match function parameter types
                    let mut args = Vec::with_capacity(function.get_parameters().len());
                    for param in function.get_parameters() {
                        let param_type = param.get_type();
                        let arg = if !external_variables.is_empty() {
                            // Try to find a variable of matching type first
                            let matching_vars: Vec<_> = external_variables
                                .iter()
                                .filter(|var| var.get_class() == Some(param_type))
                                .collect();

                            if !matching_vars.is_empty() && rng.random_bool(2.0 / 3.0) {
                                // 67% chance to use matching variable
                                let variable = matching_vars.choose(rng).unwrap();
                                Expression::VariableReference(variable.get_name().to_string())
                            } else {
                                // Generate expression of matching type
                                Expression::generate_expression_for_type(
                                    param_type,
                                    2,
                                    Some(external_functions.clone()),
                                    Some(external_variables),
                                    rng,
                                )
                            }
                        } else {
                            // Generate expression of matching type
                            Expression::generate_expression_for_type(
                                param_type,
                                2,
                                Some(external_functions.clone()),
                                Some(external_variables),
                                rng,
                            )
                        };
                        args.push(arg);
                    }

                    SingleStatement::FunctionCall(function_name, args)
                }
            }
            3..=7 => {
                // Generate compound assignment (+=, -=, *=, /=)
                // Get available mutable variables (simplified for testing)
                let available_vars = external_variables
                    .iter()
                    .filter(|v| v.is_mutable())
                    .map(|v| v.get_name().to_string())
                    .collect::<Vec<_>>();

                if available_vars.is_empty() {
                    // If no mutable numeric variables available, generate a new variable declaration instead
                    let var = Variable::generate_random_variable_with_const_control(
                        false,
                        true,
                        Some(external_variables),
                        false,
                        rng,
                    );
                    SingleStatement::VariableDeclaration(var)
                } else {
                    // Choose a random mutable variable
                    let var_name = available_vars.choose(rng).unwrap().clone();
                    let op =
                        CompoundAssignmentOperator::generate_random_compound_assignment_operator(
                            rng,
                        );

                    // Generate numeric expression for the right side
                    let expr = if !external_variables.is_empty() && rng.random_bool(1.0 / 2.0) {
                        // 50% chance to generate variable reference
                        // Only use numeric variables for compound assignment
                        let numeric_variables: Vec<_> = external_variables
                            .iter()
                            .filter(|var| var.is_numeric())
                            .collect();

                        if !numeric_variables.is_empty() {
                            let variable = numeric_variables.choose(rng).unwrap();
                            Expression::VariableReference(variable.get_name().to_string())
                        } else {
                            // Fallback to random numeric expression if no numeric variables available
                            Expression::generate_random_expression(
                                3,
                                Some(external_functions.clone()),
                                Some(external_variables),
                                defined_classes, // Pass defined classes to expression generation
                                rng,
                            )
                        }
                    } else {
                        // Otherwise generate random numeric expression
                        Expression::generate_random_expression(
                            3,
                            Some(external_functions.clone()),
                            Some(external_variables),
                            defined_classes, // Pass defined classes to expression generation
                            rng,
                        )
                    };

                    SingleStatement::CompoundAssignment(var_name, op, expr)
                }
            }
            8 => {
                // Generate object creation (20% chance)
                if rng.random_bool(1.0 / 5.0) {
                    // For now, generate a simple object creation
                    // In a full implementation, this would use custom classes from the context
                    let var = Variable::generate_random_variable_with_const_control(
                        false,
                        true,
                        Some(external_variables),
                        false,
                        rng,
                    );
                    SingleStatement::VariableDeclaration(var)
                } else {
                    // Fallback to variable declaration
                    let var = Variable::generate_random_variable_with_const_control(
                        false,
                        true,
                        Some(external_variables),
                        false,
                        rng,
                    );
                    SingleStatement::VariableDeclaration(var)
                }
            }
            9 => {
                if rng.random_bool(1.0 / 2.0) {
                    SingleStatement::Return(Some(Expression::generate_random_expression(
                        3,
                        Some(external_functions.clone()),
                        Some(external_variables),
                        defined_classes, // Pass defined classes to expression generation
                        rng,
                    )))
                } else {
                    SingleStatement::Return(None)
                }
            }
            _ => {
                // Fallback to variable declaration for any unexpected values
                let var = Variable::generate_random_variable_with_const_control(
                    false,
                    true,
                    Some(external_variables),
                    false,
                    rng,
                );
                SingleStatement::VariableDeclaration(var)
            }
        }
    }

    /// Generate a type-safe single statement using typed generation context
    pub fn generate_type_safe_single_statement<T: Rng + SeedableRng>(
        external_variables: &[Variable],
        external_functions: Rc<RefCell<Vec<Function>>>,
        typed_context: &mut TypedGenerationContext,
        rng: &mut T,
    ) -> Self {
        Self::generate_type_safe_single_statement_with_return_type(
            external_variables,
            external_functions,
            typed_context,
            None,
            None, // defined_classes
            rng,
        )
    }

    /// Generate a type-safe single statement with expected return type
    pub fn generate_type_safe_single_statement_with_return_type<T: Rng + SeedableRng>(
        external_variables: &[Variable],
        external_functions: Rc<RefCell<Vec<Function>>>,
        typed_context: &mut TypedGenerationContext,
        expected_return_type: Option<&Type>,
        defined_classes: Option<&[Class]>,
        rng: &mut T,
    ) -> Self {
        // Add external variables to typed context
        for var in external_variables {
            let _ = typed_context.add_variable(var);
        }

        // Add external functions to typed context
        {
            let functions = external_functions.borrow();
            for func in functions.iter() {
                let _ = typed_context.add_function(func);
            }
        }

        match rng.random_range(0..5) {
            0 => {
                // Generate a type-compatible variable
                let var = typed_context.generate_type_compatible_variable_no_const(false, rng);
                // Add the new variable to context
                let _ = typed_context.add_variable(&var);
                SingleStatement::VariableDeclaration(var)
            }
            1 => {
                // Generate type-safe assignment
                let mutable_vars = typed_context.get_mutable_variables();
                if !mutable_vars.is_empty() {
                    let var = mutable_vars.choose(rng).unwrap();
                    if let Some(assignment) = typed_context.generate_type_safe_assignment(var, rng)
                    {
                        assignment
                    } else {
                        let var =
                            typed_context.generate_type_compatible_variable_no_const(false, rng);
                        let _ = typed_context.add_variable(&var);
                        SingleStatement::VariableDeclaration(var)
                    }
                } else {
                    // No mutable variables, generate new variable
                    let var = typed_context.generate_type_compatible_variable_no_const(false, rng);
                    let _ = typed_context.add_variable(&var);
                    SingleStatement::VariableDeclaration(var)
                }
            }
            2 => {
                // Generate type-safe function call
                let available_funcs = typed_context.get_available_functions();
                if !available_funcs.is_empty() {
                    let func_name = available_funcs.choose(rng).unwrap();
                    if let Some(call) =
                        typed_context.generate_type_safe_function_call(func_name, rng)
                    {
                        call
                    } else {
                        // Fallback to variable declaration
                        let var =
                            typed_context.generate_type_compatible_variable_no_const(false, rng);
                        let _ = typed_context.add_variable(&var);
                        SingleStatement::VariableDeclaration(var)
                    }
                } else {
                    // No functions available, generate variable
                    let var = typed_context.generate_type_compatible_variable(false, rng);
                    let _ = typed_context.add_variable(&var);
                    SingleStatement::VariableDeclaration(var)
                }
            }
            3 => {
                // Generate type-safe compound assignment
                // Only select numeric mutable variables for compound assignment
                let all_mutable_vars = typed_context.get_mutable_variables();
                let numeric_mutable_vars: Vec<&Variable> = all_mutable_vars
                    .iter()
                    .filter(|var| var.is_numeric())
                    .collect();

                if !numeric_mutable_vars.is_empty() {
                    let var = numeric_mutable_vars.choose(rng).unwrap();
                    let op =
                        CompoundAssignmentOperator::generate_random_compound_assignment_operator(
                            rng,
                        );

                    // Generate numeric expression for the right side - can be variable, function call, or literal
                    let expr = match rng.random_range(0..3) {
                        0 => {
                            // 33% chance: Generate numeric variable reference if available
                            let numeric_vars: Vec<_> = all_mutable_vars
                                .iter()
                                .filter(|var| var.is_numeric())
                                .collect();

                            if !numeric_vars.is_empty() {
                                let ref_var = numeric_vars.choose(rng).unwrap();
                                Expression::VariableReference(ref_var.get_name().to_string())
                            } else {
                                Expression::Arithmetic(ArithmeticExpression::Int(
                                    rng.random_range(1..=10),
                                ))
                            }
                        }
                        1 => {
                            // 33% chance: Generate function call with proper parameters by delegating to Expression
                            Expression::generate_random_expression(
                                2,
                                Some(typed_context.get_external_functions()),
                                Some(&typed_context.get_mutable_variables()),
                                defined_classes, // Pass defined classes to expression generation
                                rng,
                            )
                        }
                        _ => {
                            // 33% chance: Generate numeric literal
                            Expression::Arithmetic(ArithmeticExpression::Int(
                                rng.random_range(1..=10),
                            ))
                        }
                    };

                    SingleStatement::CompoundAssignment(var.get_name().to_string(), op, expr)
                } else {
                    // No mutable variables, generate new variable
                    let var = typed_context.generate_type_compatible_variable_no_const(false, rng);
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

impl Display for SingleStatement {
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
            SingleStatement::CompoundAssignment(var_name, op, expr) => {
                write!(f, "{} {} {}", var_name, op, expr)
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
            SingleStatement::ObjectCreation(object) => {
                write!(
                    f,
                    "val {} = {}({})",
                    object.get_variable_name(),
                    object.get_class_name(),
                    object
                        .properties
                        .iter()
                        .map(|prop| format!("{} = {}", prop.get_name(), prop.get_value()))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}
