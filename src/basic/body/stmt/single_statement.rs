use rand::seq::IndexedRandom;
use rand::{Rng, SeedableRng};

use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

use crate::basic::body::fun::function::Function;
use crate::basic::cls::class::Class;
use crate::basic::expr::arithmetic_expression::ArithmeticExpression;
use crate::basic::expr::expression::{CallerRelationship, Expression};
use crate::basic::obj::object_instance::ObjectInstance;
use crate::basic::var::prefix::visibility::Visibility;
use crate::basic::var::variable::Variable;
use crate::type_system::{Type, TypedGenerationContext};

use crate::basic::utils::{
    format_function_call, format_method_call, map_collect_join,
    select_enum_variant_with_probability,
};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Clone, Debug, PartialEq, Eq, Hash, EnumIter)]
pub enum CompoundAssignmentOperator {
    AddAssign,      // +=
    SubtractAssign, // -=
    MultiplyAssign, // *=
    DivideAssign,   // /=
}

impl CompoundAssignmentOperator {
    pub fn generate_random_compound_assignment_operator<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        let operators: Vec<_> = CompoundAssignmentOperator::iter().collect();
        // Probability distribution: AddAssign and SubtractAssign more common than MultiplyAssign and DivideAssign
        let probabilities = [0.4, 0.4, 0.1, 0.1]; // AddAssign, SubtractAssign, MultiplyAssign, DivideAssign
        select_enum_variant_with_probability(&operators, &probabilities, rng)
            .unwrap_or(&CompoundAssignmentOperator::AddAssign)
            .clone()
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
    PropertyCompoundAssignment(String, String, CompoundAssignmentOperator, Expression), // object_name, property_name, operator, expression
    FunctionCall(String, Vec<Expression>),
    MethodCall(String, String, Vec<Expression>), // object_name, method_name, arguments
    ObjectCreation(ObjectInstance),
    Return(Option<Expression>),
    Break,
    Continue,
}

impl SingleStatement {
    /// Generate a type-safe single statement using typed generation context
    pub fn generate_type_safe_single_statement<T: Rng + SeedableRng>(
        external_variables: &[Variable],
        external_functions: Rc<RefCell<Vec<Function>>>,
        typed_context: &mut TypedGenerationContext,
        rng: &mut T,
    ) -> Self {
        // Get defined classes before calling the function to avoid borrow conflicts
        let defined_classes = typed_context.get_defined_classes().to_vec();
        Self::generate_type_safe_single_statement_with_return_type(
            external_variables,
            external_functions,
            typed_context,
            None,
            Some(
                &defined_classes
                    .iter()
                    .map(|rc| rc.as_ref().clone())
                    .collect::<Vec<_>>(),
            ), // Use defined classes from typed context
            rng,
        )
    }

    /// Generate a type-safe single statement with expected return type
    pub fn generate_type_safe_single_statement_with_return_type<T: Rng + SeedableRng>(
        external_variables: &[Variable],
        external_functions: Rc<RefCell<Vec<Function>>>,
        typed_context: &mut TypedGenerationContext,
        expected_return_type: Option<&Type>,
        _defined_classes: Option<&[Class]>,
        rng: &mut T,
    ) -> Self {
        // Add external variables to typed context
        for var in external_variables {
            let _ = typed_context.add_variable(var);
        }

        // Add external functions to typed context (but not class methods)
        {
            let functions = external_functions.borrow();
            for func in functions.iter() {
                // Only add functions that are not class methods
                if !func.is_method() {
                    let _ = typed_context.add_function(func);
                }
            }
        }

        // Check if we're inside a loop for break/continue generation
        let is_inside_loop = typed_context.is_inside_loop();

        match rng.random_range(0..12) {
            0 => {
                // Generate a type-compatible variable (8% probability - further reduced)
                let var = typed_context.generate_type_compatible_variable_no_const(false, rng);
                // Add the new variable to context
                let _ = typed_context.add_variable(&var);
                SingleStatement::VariableDeclaration(var)
            }
            1 => {
                // Generate type-safe assignment (10% probability - reduced)
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
            2..=3 => {
                // Generate type-safe function call (20% probability - increased)
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
            4..=5 => {
                // Generate type-safe compound assignment (20% probability - increased)
                // 70% chance for variable compound assignment, 30% chance for property compound assignment
                if rng.random_bool(0.7) {
                    // Generate variable compound assignment - use efficient filtering without cloning
                    let all_mutable_vars = typed_context.get_mutable_variables();
                    let numeric_mutable_vars: Vec<&Variable> = all_mutable_vars
                        .iter()
                        .filter(|v| v.is_numeric())
                        .copied()
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
                                // 33% chance: Generate type-compatible variable reference if available
                                let compatible_vars: Vec<_> = all_mutable_vars
                                    .iter()
                                    .filter(|v| {
                                        // Only use variables of the same type as the target variable
                                        if var.is_float() {
                                            v.is_float()
                                        } else if var.is_integer() {
                                            v.is_integer()
                                        } else {
                                            // For other types, use any variable
                                            true
                                        }
                                    })
                                    .collect();

                                if !compatible_vars.is_empty() {
                                    let ref_var = compatible_vars.choose(rng).unwrap();
                                    Expression::VariableReference(ref_var.get_name().to_string())
                                } else {
                                    // Fallback to type-compatible literal
                                    if var.is_float() {
                                        Expression::Arithmetic(ArithmeticExpression::Float(
                                            ordered_float::OrderedFloat(
                                                rng.random_range(1.0..=10.0),
                                            ),
                                        ))
                                    } else {
                                        Expression::Arithmetic(ArithmeticExpression::Int(
                                            rng.random_range(1..=10),
                                        ))
                                    }
                                }
                            }
                            1 => {
                                // 33% chance: Generate type-safe expression that matches the variable type
                                if let Some(_var_type) = typed_context.get_variable_type(var) {
                                    // Generate a simple expression that matches the variable type
                                    // Generate expression based on variable type to avoid implicit conversion
                                    if var.is_float() {
                                        // For float variables, generate float literal
                                        Expression::Arithmetic(ArithmeticExpression::Float(
                                            ordered_float::OrderedFloat(
                                                rng.random_range(1.0..=10.0),
                                            ),
                                        ))
                                    } else if var.is_integer() {
                                        // For integer variables, generate integer literal
                                        Expression::Arithmetic(ArithmeticExpression::Int(
                                            rng.random_range(1..=10),
                                        ))
                                    } else {
                                        // For other types, generate integer literal as fallback
                                        Expression::Arithmetic(ArithmeticExpression::Int(
                                            rng.random_range(1..=10),
                                        ))
                                    }
                                } else {
                                    // Fallback to numeric literal if variable type not found
                                    Expression::Arithmetic(ArithmeticExpression::Int(
                                        rng.random_range(1..=10),
                                    ))
                                }
                            }
                            _ => {
                                // 33% chance: Generate type-compatible numeric literal
                                if var.is_float() {
                                    Expression::Arithmetic(ArithmeticExpression::Float(
                                        ordered_float::OrderedFloat(rng.random_range(1.0..=10.0)),
                                    ))
                                } else {
                                    Expression::Arithmetic(ArithmeticExpression::Int(
                                        rng.random_range(1..=10),
                                    ))
                                }
                            }
                        };

                        SingleStatement::CompoundAssignment(var.get_name().to_string(), op, expr)
                    } else {
                        // No mutable variables, generate new variable
                        let var =
                            typed_context.generate_type_compatible_variable_no_const(false, rng);
                        let _ = typed_context.add_variable(&var);
                        SingleStatement::VariableDeclaration(var)
                    }
                } else {
                    // Generate property compound assignment (30% chance)
                    if let Some(_defined_classes) = _defined_classes {
                        let class_objects: Vec<_> = external_variables
                            .iter()
                            .filter(|v| {
                                v.get_class().is_some_and(|c| matches!(c, Class::Custom(_)))
                            })
                            .collect();

                        if !class_objects.is_empty() {
                            let object = class_objects.choose(rng).unwrap();
                            let object_name = object.get_name().to_string();

                            // Find the class of this object to get its real properties
                            let object_class = object.get_class();
                            if let Some(Class::Custom(custom_class)) = object_class {
                                // Determine the relationship between caller and target class
                                let relationship = Expression::determine_caller_relationship(
                                    typed_context.get_current_class_name(),
                                    custom_class,
                                    Some(_defined_classes),
                                );

                                // Get real properties from the class based on visibility rules
                                let mutable_numeric_properties: Vec<_> = custom_class
                                    .properties
                                    .iter()
                                    .filter(|prop| {
                                        if !prop.is_numeric() || !prop.is_mutable() {
                                            return false;
                                        }

                                        let visibility = prop.get_prefix().get_visibility();
                                        match relationship {
                                            CallerRelationship::SameClass => {
                                                // Same class - can access all properties
                                                true
                                            }
                                            CallerRelationship::Subclass => {
                                                // Subclass - can access public and protected properties
                                                visibility.is_public()
                                                    || matches!(visibility, Visibility::Protected)
                                            }
                                            CallerRelationship::Unrelated => {
                                                // Unrelated class - only public properties
                                                visibility.is_public()
                                            }
                                        }
                                    })
                                    .collect();

                                if !mutable_numeric_properties.is_empty() {
                                    // Choose a real numeric property from the class
                                    let property = mutable_numeric_properties.choose(rng).unwrap();
                                    let property_name = property.get_name().to_string();

                                    let op = CompoundAssignmentOperator::generate_random_compound_assignment_operator(rng);

                                    // Generate a simple numeric expression for the right side
                                    let expr = if rng.random_bool(0.5) {
                                        Expression::Arithmetic(ArithmeticExpression::Int(
                                            rng.random_range(1..=10),
                                        ))
                                    } else {
                                        Expression::Arithmetic(ArithmeticExpression::Float(
                                            ordered_float::OrderedFloat(
                                                rng.random_range(1.0..=10.0),
                                            ),
                                        ))
                                    };

                                    SingleStatement::PropertyCompoundAssignment(
                                        object_name,
                                        property_name,
                                        op,
                                        expr,
                                    )
                                } else {
                                    // No numeric properties available, fallback to variable declaration
                                    let var = typed_context
                                        .generate_type_compatible_variable_no_const(false, rng);
                                    let _ = typed_context.add_variable(&var);
                                    SingleStatement::VariableDeclaration(var)
                                }
                            } else {
                                // Object is not a custom class, fallback to variable declaration
                                let var = typed_context
                                    .generate_type_compatible_variable_no_const(false, rng);
                                let _ = typed_context.add_variable(&var);
                                SingleStatement::VariableDeclaration(var)
                            }
                        } else {
                            // No class objects available, fallback to variable declaration
                            let var = typed_context
                                .generate_type_compatible_variable_no_const(false, rng);
                            let _ = typed_context.add_variable(&var);
                            SingleStatement::VariableDeclaration(var)
                        }
                    } else {
                        // No defined classes, fallback to variable declaration
                        let var =
                            typed_context.generate_type_compatible_variable_no_const(false, rng);
                        let _ = typed_context.add_variable(&var);
                        SingleStatement::VariableDeclaration(var)
                    }
                }
            }
            6..=7 => {
                // Generate method call (20% probability - new)
                if let Some(defined_classes) = _defined_classes {
                    let mut available_methods = Vec::new();

                    // Get current class name from typed context
                    let current_class_name = typed_context.get_current_class_name();

                    for class in defined_classes.iter() {
                        if let Class::Custom(custom_class) = class {
                            // Determine the relationship between caller and target class
                            let relationship = Expression::determine_caller_relationship(
                                current_class_name,
                                custom_class,
                                Some(defined_classes),
                            );

                            match relationship {
                                CallerRelationship::SameClass => {
                                    // Same class - can call all methods (private, protected, internal, public)
                                    for method in custom_class.get_methods() {
                                        available_methods.push(method);
                                    }
                                }
                                CallerRelationship::Subclass => {
                                    // Subclass - can call public and protected methods
                                    for method in custom_class.get_methods() {
                                        let visibility = method.get_visibility();
                                        if visibility.is_public()
                                            || matches!(visibility, Visibility::Protected)
                                        {
                                            available_methods.push(method);
                                        }
                                    }
                                }
                                CallerRelationship::Unrelated => {
                                    // Unrelated class - can call public and internal methods (same module)
                                    for method in custom_class.get_methods() {
                                        let visibility = method.get_visibility();
                                        if visibility.is_public()
                                            || matches!(visibility, Visibility::Internal)
                                        {
                                            available_methods.push(method);
                                        }
                                    }
                                }
                            }
                        }
                    }

                    let class_methods = available_methods;

                    if !class_methods.is_empty() {
                        let method = class_methods.choose(rng).unwrap();
                        let args = Expression::generate_function_call_args(
                            method.get_parameters(),
                            2, // Max depth for method call arguments
                            Some(external_functions.clone()),
                            Some(external_variables),
                            rng,
                        );

                        // Use a simple object name for method calls
                        if !external_variables.is_empty() {
                            // Find objects that can actually call this method
                            let compatible_objects: Vec<_> = external_variables
                                .iter()
                                .filter(|v| {
                                    if let Some(Class::Custom(custom_class)) = v.get_class() {
                                        // Check if this object's class can call the method
                                        let relationship =
                                            Expression::determine_caller_relationship(
                                                current_class_name,
                                                custom_class,
                                                Some(defined_classes),
                                            );

                                        match relationship {
                                            CallerRelationship::SameClass => {
                                                // Same class - can call all methods
                                                custom_class
                                                    .get_methods()
                                                    .iter()
                                                    .any(|m| m.get_name() == method.get_name())
                                            }
                                            CallerRelationship::Subclass => {
                                                // Subclass - can call public and protected methods
                                                let visibility = method.get_visibility();
                                                if visibility.is_public()
                                                    || matches!(visibility, Visibility::Protected)
                                                {
                                                    custom_class
                                                        .get_methods()
                                                        .iter()
                                                        .any(|m| m.get_name() == method.get_name())
                                                } else {
                                                    false
                                                }
                                            }
                                            CallerRelationship::Unrelated => {
                                                // Unrelated class - can call public and internal methods (same module)
                                                let visibility = method.get_visibility();
                                                if visibility.is_public()
                                                    || matches!(visibility, Visibility::Internal)
                                                {
                                                    custom_class
                                                        .get_methods()
                                                        .iter()
                                                        .any(|m| m.get_name() == method.get_name())
                                                } else {
                                                    false
                                                }
                                            }
                                        }
                                    } else {
                                        false
                                    }
                                })
                                .collect();

                            if !compatible_objects.is_empty() {
                                let object_name = compatible_objects
                                    .choose(rng)
                                    .unwrap()
                                    .get_name()
                                    .to_string();
                                SingleStatement::MethodCall(
                                    object_name,
                                    method.get_name().to_string(),
                                    args,
                                )
                            } else {
                                // No objects available, fallback to variable declaration
                                let var = typed_context
                                    .generate_type_compatible_variable_no_const(false, rng);
                                let _ = typed_context.add_variable(&var);
                                SingleStatement::VariableDeclaration(var)
                            }
                        } else {
                            // No external variables, fallback to variable declaration
                            let var = typed_context
                                .generate_type_compatible_variable_no_const(false, rng);
                            let _ = typed_context.add_variable(&var);
                            SingleStatement::VariableDeclaration(var)
                        }
                    } else {
                        // Fallback to variable declaration
                        let var =
                            typed_context.generate_type_compatible_variable_no_const(false, rng);
                        let _ = typed_context.add_variable(&var);
                        SingleStatement::VariableDeclaration(var)
                    }
                } else {
                    // No defined classes, fallback to variable declaration
                    let var = typed_context.generate_type_compatible_variable_no_const(false, rng);
                    let _ = typed_context.add_variable(&var);
                    SingleStatement::VariableDeclaration(var)
                }
            }
            8 => {
                // Generate type-aware return statement (9% probability)
                if let Some(return_stmt) = typed_context
                    .generate_type_safe_return_statement_with_type(expected_return_type, rng)
                {
                    return_stmt
                } else {
                    // If we cannot generate a compatible return statement, fallback to variable assignment
                    let var = typed_context.generate_type_compatible_variable_no_const(false, rng);
                    let _ = typed_context.add_variable(&var);
                    SingleStatement::VariableDeclaration(var)
                }
            }
            9 => {
                // Generate break/continue statements (8% probability, only inside loops)
                if is_inside_loop {
                    if rng.random_bool(0.6) {
                        // 60% chance for break, 40% chance for continue
                        SingleStatement::Break
                    } else {
                        SingleStatement::Continue
                    }
                } else {
                    // Fallback to variable declaration if not in loop
                    let var = typed_context.generate_type_compatible_variable_no_const(false, rng);
                    let _ = typed_context.add_variable(&var);
                    SingleStatement::VariableDeclaration(var)
                }
            }
            10 => {
                // Additional break/continue generation opportunity (8% probability, only inside loops)
                if is_inside_loop {
                    if rng.random_bool(0.5) {
                        // 50% chance for break, 50% chance for continue
                        SingleStatement::Break
                    } else {
                        SingleStatement::Continue
                    }
                } else {
                    // Fallback to variable declaration if not in loop
                    let var = typed_context.generate_type_compatible_variable_no_const(false, rng);
                    let _ = typed_context.add_variable(&var);
                    SingleStatement::VariableDeclaration(var)
                }
            }
            _ => {
                // Fallback to variable declaration (8% probability)
                let var = typed_context.generate_type_compatible_variable_no_const(false, rng);
                let _ = typed_context.add_variable(&var);
                SingleStatement::VariableDeclaration(var)
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
            SingleStatement::PropertyCompoundAssignment(object_name, property_name, op, expr) => {
                write!(f, "{}.{} {} {}", object_name, property_name, op, expr)
            }
            SingleStatement::FunctionCall(function_name, args) => {
                write!(f, "{}", format_function_call(function_name, args))
            }
            SingleStatement::MethodCall(object_name, method_name, args) => {
                write!(f, "{}", format_method_call(object_name, method_name, args))
            }
            SingleStatement::Return(expr) => {
                if let Some(expr) = expr {
                    write!(f, "return {}", expr)
                } else {
                    write!(f, "return")
                }
            }
            SingleStatement::ObjectCreation(object) => {
                let props_str = map_collect_join(
                    &object.properties,
                    |prop| format!("{} = {}", prop.get_name(), prop.get_value()),
                    ", ",
                );
                write!(
                    f,
                    "val {} = {}({})",
                    object.get_variable_name(),
                    object.get_class_name(),
                    props_str
                )
            }
            SingleStatement::Break => write!(f, "break"),
            SingleStatement::Continue => write!(f, "continue"),
        }
    }
}
