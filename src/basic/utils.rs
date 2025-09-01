use rand::seq::IteratorRandom;
use rand::{Rng, SeedableRng};
use std::cell::RefCell;

use std::rc::Rc;

use crate::basic::body::fun::function::Function;
use crate::basic::cls::class::Class;
use crate::basic::expr::arithmetic_expression::ArithmeticExpression;
use crate::basic::expr::expression::Expression;
use crate::basic::var::variable::Variable;
use crate::type_system::TypedGenerationContext;

// Common probability constants to avoid duplication
pub const PROBABILITY_USE_MATCHING_VARIABLE: f64 = 2.0 / 3.0;
pub const PROBABILITY_INT_VS_FLOAT_LITERAL: f64 = 1.0 / 2.0;
pub const PROBABILITY_INT_LITERAL: f64 = 1.0 / 2.0; // Same as PROBABILITY_INT_VS_FLOAT_LITERAL
pub const PROBABILITY_HALF: f64 = 1.0 / 2.0; // Generic 50% probability

pub const MIN_IDENTIFIER_LENGTH: usize = 1;
pub const MAX_IDENTIFIER_LENGTH: usize = 7;

pub const IDENTIFIER_CHARS: &str = "abcdefghijklmnopqrstuvwxyz0123456789_";
pub const IDENTIFIER_FIRST_CHARS: &str = "abcdefghijklmnopqrstuvwxyz_";

/// Generate a random identifier
pub fn generate_random_identifier<T: Rng + SeedableRng>(rng: &mut T) -> String {
    let length = rng.random_range(MIN_IDENTIFIER_LENGTH..=MAX_IDENTIFIER_LENGTH);
    let mut identifier = String::new();

    // First character must be a letter or underscore
    let first_char = IDENTIFIER_FIRST_CHARS.chars().choose(rng).unwrap();
    identifier.push(first_char);

    // Rest can be letters, digits, or underscore (equal probability for each)
    for _ in 1..length {
        let next_char = IDENTIFIER_CHARS.chars().choose(rng).unwrap();
        identifier.push(next_char);
    }

    identifier
}

/// Generate a unique identifier that doesn't exist in the given set
pub fn generate_unique_identifier<T: Rng + SeedableRng>(
    rng: &mut T,
    existing_names: &mut Vec<String>,
) -> String {
    let base_identifier = generate_random_identifier(rng);
    let mut identifier = base_identifier.clone();
    let mut counter = 1;

    // Keep adding suffix until we find a unique name
    while existing_names.contains(&identifier) {
        identifier = format!("{}_{}", base_identifier, counter);
        counter += 1;
    }

    // Add the unique identifier to the vector
    existing_names.push(identifier.clone());
    identifier
}

/// Generic function to filter and collect items
pub fn filter_collect<T, F>(items: &[T], filter: F) -> Vec<&T>
where
    F: Fn(&T) -> bool,
{
    items.iter().filter(|item| filter(item)).collect()
}

/// Generic function to map, collect and join items
pub fn map_collect_join<T, F>(items: &[T], map: F, separator: &str) -> String
where
    F: Fn(&T) -> String,
{
    items.iter().map(map).collect::<Vec<_>>().join(separator)
}

/// Filter numeric variables
pub fn filter_numeric_variables(variables: &[Variable]) -> Vec<&Variable> {
    filter_collect(variables, |var| var.is_numeric())
}

/// Filter const numeric variables
pub fn filter_const_numeric_variables(variables: &[Variable]) -> Vec<&Variable> {
    filter_collect(variables, |var| var.is_const() && var.is_numeric())
}

/// Filter numeric functions (non-methods)
pub fn filter_numeric_functions(functions: &[Function]) -> Vec<&Function> {
    filter_collect(functions, |func| {
        func.is_numeric_function() && !func.is_method()
    })
}

/// Filter boolean functions (non-methods)
pub fn filter_boolean_functions(functions: &[Function]) -> Vec<&Function> {
    filter_collect(functions, |func| {
        func.is_boolean_function() && !func.is_method()
    })
}

/// Generate a random expression with default parameters
pub fn generate_random_expression_default<T: Rng + SeedableRng>(
    max_depth: usize,
    external_variables: Option<&[Variable]>,
    external_functions: Option<Rc<RefCell<Vec<Function>>>>,
    defined_classes: Option<&[Class]>,
    rng: &mut T,
) -> Expression {
    Expression::generate_random_expression(
        max_depth,
        external_functions,
        external_variables,
        defined_classes,
        rng,
    )
}

/// Generate a random expression with functions
pub fn generate_random_expression_with_functions<T: Rng + SeedableRng>(
    max_depth: usize,
    external_functions: Rc<RefCell<Vec<Function>>>,
    external_variables: Option<&[Variable]>,
    defined_classes: Option<&[Class]>,
    rng: &mut T,
) -> Expression {
    Expression::generate_random_expression(
        max_depth,
        Some(external_functions),
        external_variables,
        defined_classes,
        rng,
    )
}

/// Generate a fallback int literal expression
pub fn generate_fallback_int_literal<T: Rng + SeedableRng>(rng: &mut T) -> Expression {
    Expression::generate_random_int_literal(rng)
}

/// Generate a fallback float literal expression
pub fn generate_fallback_float_literal<T: Rng + SeedableRng>(rng: &mut T) -> Expression {
    Expression::generate_random_float_literal(rng)
}

/// Generate a literal based on target type (int or float)
pub fn generate_literal_by_type<T: Rng + SeedableRng>(
    target_is_int: bool,
    rng: &mut T,
) -> ArithmeticExpression {
    if target_is_int {
        ArithmeticExpression::generate_random_int_literal(rng)
    } else {
        ArithmeticExpression::generate_random_float_literal(rng)
    }
}

/// Generate an expression literal based on target type (int or float)
pub fn generate_expression_literal_by_type<T: Rng + SeedableRng>(
    target_is_int: bool,
    rng: &mut T,
) -> Expression {
    if target_is_int {
        Expression::generate_random_int_literal(rng)
    } else {
        Expression::generate_random_float_literal(rng)
    }
}

/// Select a random enum variant based on probability distribution
pub fn select_enum_variant_with_probability<'a, T, R>(
    variants: &'a [T],
    probabilities: &[f64],
    rng: &mut R,
) -> Option<&'a T>
where
    R: Rng + SeedableRng,
{
    if variants.len() != probabilities.len() || variants.is_empty() {
        return None;
    }

    let random_value = rng.random::<f64>();
    let mut cumulative_prob = 0.0;

    for (i, &prob) in probabilities.iter().enumerate() {
        cumulative_prob += prob;
        if random_value <= cumulative_prob {
            return Some(&variants[i]);
        }
    }

    // Fallback to last variant if rounding errors occur
    variants.last()
}

/// Choose a random item from a slice with fallback
pub fn choose_random_item<'a, T, R>(items: &'a [T], rng: &mut R) -> Option<&'a T>
where
    R: Rng + SeedableRng,
{
    if items.is_empty() {
        None
    } else {
        Some(&items[rng.random_range(0..items.len())])
    }
}

/// Choose a random item from a slice, panicking if empty
pub fn choose_random_item_unwrap<'a, T, R>(items: &'a [T], rng: &mut R) -> &'a T
where
    R: Rng + SeedableRng,
{
    &items[rng.random_range(0..items.len())]
}

/// Format expressions as comma-separated string
fn format_expressions(args: &[Expression]) -> String {
    map_collect_join(args, |arg| arg.to_string(), ", ")
}

/// Format function call with arguments
pub fn format_function_call(name: &str, args: &[Expression]) -> String {
    if args.is_empty() {
        format!("{}()", name)
    } else {
        format!("{}({})", name, format_expressions(args))
    }
}

/// Format method call with arguments
pub fn format_method_call(object: &str, method: &str, args: &[Expression]) -> String {
    if args.is_empty() {
        format!("{}.{}()", object, method)
    } else {
        format!("{}.{}({})", object, method, format_expressions(args))
    }
}

/// Format property access
pub fn format_property_access(object: &str, property: &str) -> String {
    format!("{}.{}", object, property)
}

/// Configuration for code generation to reduce function parameter count
#[derive(Clone)]
pub struct GenerationConfig {
    pub external_variables: Vec<Variable>,
    pub external_functions: Rc<RefCell<Vec<Function>>>,
    pub defined_classes: Option<Vec<Class>>,
    pub current_indentation_layer: usize,
    pub max_depth: usize,
    pub typed_context: Option<Rc<RefCell<TypedGenerationContext>>>,
    pub existing_names: Vec<String>,
}

impl GenerationConfig {
    pub fn new(
        external_variables: Vec<Variable>,
        external_functions: Rc<RefCell<Vec<Function>>>,
        defined_classes: Option<Vec<Class>>,
        current_indentation_layer: usize,
        max_depth: usize,
    ) -> Self {
        Self {
            external_variables,
            external_functions,
            defined_classes,
            current_indentation_layer,
            max_depth,
            typed_context: None,
            existing_names: Vec::new(),
        }
    }

    pub fn with_typed_context(
        mut self,
        typed_context: Rc<RefCell<TypedGenerationContext>>,
    ) -> Self {
        self.typed_context = Some(typed_context);
        self
    }

    pub fn with_existing_names(mut self, existing_names: Vec<String>) -> Self {
        self.existing_names = existing_names;
        self
    }
}
