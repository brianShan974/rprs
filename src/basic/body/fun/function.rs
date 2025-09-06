use rand::{Rng, SeedableRng, seq::IndexedRandom};

use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

use crate::basic::{
    body::{
        block::{Block, INDENT_SIZE, SPACE},
        fun::parameter::Parameter,
        stmt::{single_statement::SingleStatement, statement::Statement},
    },
    cls::{
        basic_type::BasicType,
        class::{BOOLEAN, Class, FLOAT, INT},
        custom_class::CustomClass,
        generic_type::{GenericType, GenericTypeParameter},
    },
    expr::expression::Expression,
    utils::{GenerationConfig, generate_unique_identifier, map_collect_join},
    var::{prefix::visibility::Visibility, variable::Variable},
};
use crate::type_system::{Type, TypedGenerationContext};

pub type FuncRef = Rc<Function>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Function {
    name: String,
    parameters: Vec<Parameter>,
    return_type: Option<Class>,
    body: Block,
    current_indentation_layer: usize,
    visibility: Visibility,
    is_method: bool, // Whether this function is a class method
}

impl Function {
    pub const MAX_DEPTH: usize = 5;

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_parameters(&self) -> &Vec<Parameter> {
        &self.parameters
    }

    pub fn get_body(&self) -> &Block {
        &self.body
    }

    pub fn get_return_type(&self) -> Option<&Class> {
        self.return_type.as_ref()
    }

    pub fn get_visibility(&self) -> &Visibility {
        &self.visibility
    }

    pub fn get_current_indentation_layer(&self) -> usize {
        self.current_indentation_layer
    }

    pub fn is_class_method(&self) -> bool {
        self.is_method
    }

    /// Generate only the function signature (name, parameters, return type) without full implementation
    pub fn generate_signature_only<T: Rng + SeedableRng>(
        rng: &mut T,
        name: String,
        current_indentation_layer: usize,
    ) -> Self {
        let visibility = Visibility::generate_random_visibility(false, rng);
        // For signature-only functions, we don't have access to defined classes yet
        // Use empty vectors as placeholders
        let empty_classes: Vec<Class> = Vec::new();
        let parameters = Parameter::generate_random_parameters(rng, Some(&empty_classes));
        let return_type = Self::decide_return_type_with_custom_types(rng, Some(&empty_classes));

        // Create an empty body for signature-only functions
        let body = Block::new(Vec::new(), current_indentation_layer + 1);

        Self {
            name,
            parameters,
            body,
            return_type,
            visibility,
            current_indentation_layer,
            is_method: false,
        }
    }

    /// Generate function signature with access to defined classes for proper type generation
    pub fn generate_signature_only_with_classes<T: Rng + SeedableRng>(
        rng: &mut T,
        name: String,
        current_indentation_layer: usize,
        class_skeletons: &[CustomClass],
    ) -> Self {
        let visibility = Visibility::generate_random_visibility(false, rng);

        // Convert class skeletons to Class enum for parameter generation
        let defined_classes: Vec<Class> = class_skeletons
            .iter()
            .map(|c| Class::Custom(c.clone()))
            .collect();

        let parameters = Parameter::generate_random_parameters(rng, Some(&defined_classes));
        let return_type = Self::decide_return_type_with_custom_types(rng, Some(&defined_classes));

        // Create an empty body for signature-only functions
        let body = Block::new(Vec::new(), current_indentation_layer + 1);

        Self {
            name,
            parameters,
            body,
            return_type,
            visibility,
            current_indentation_layer,
            is_method: false,
        }
    }

    /// Generate a type-safe function using typed generation context
    pub fn generate_type_safe_function<T: Rng + SeedableRng>(
        config: &mut GenerationConfig,
        external_variables: &[Parameter],
        is_method: bool,
        typed_context: &mut TypedGenerationContext,
        rng: &mut T,
    ) -> Option<Self> {
        if matches!(config.max_depth, 0) {
            return None;
        }

        let parameters =
            Parameter::generate_random_parameters(rng, config.defined_classes.as_deref());
        let all_identifiers: Vec<Variable> = external_variables
            .iter()
            .chain(parameters.iter())
            .map(|p| p.to_owned().into())
            .collect();

        // Decide on return type first (before generating body)
        let return_type = if is_method {
            // For methods, we need to check if we're in a generic class context
            let generic_params = if let Some(classes) = config.defined_classes.as_deref() {
                // Find the current class (if any) and get its generic parameters
                classes.iter().find_map(|class| {
                    if let Class::Custom(custom_class) = class {
                        if !custom_class.get_generic_parameters().is_empty() {
                            Some(custom_class.get_generic_parameters())
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
            } else {
                None
            };

            Self::decide_method_return_type_with_custom_types(
                rng,
                config.defined_classes.as_deref(),
                generic_params,
            )
        } else {
            Self::decide_return_type_with_custom_types(rng, config.defined_classes.as_deref())
        };

        // Convert return type to Type system type for context
        let return_type_system = return_type.as_ref().map(|ty| Type::Basic(ty.clone()));

        // Generate function body with expected return type
        let body = Block::generate_type_safe_block_with_config(
            config,
            &all_identifiers,
            false,
            typed_context,
            return_type_system.as_ref(),
            rng,
        )?;

        // Step 1: Ensure function body has return statement if return type is specified
        let body_with_returns = if return_type.is_some() && !Self::all_paths_have_return(&body) {
            // If function has return type but not all paths have return statements, add one at the end
            Self::add_return_statement_to_body(
                body,
                return_type_system.as_ref(),
                typed_context,
                rng,
            )?
        } else {
            body
        };

        // Step 2: Ensure function has return type if body has return statement AND all paths have return
        let final_return_type = if return_type.is_none()
            && Self::has_return_statement(&body_with_returns)
            && Self::all_paths_have_return(&body_with_returns)
        {
            // If function has no return type but body has return statement AND all paths have return, infer return type
            Self::infer_return_type_from_body(&body_with_returns, &config.external_functions)
        } else {
            return_type
        };

        let function = Self {
            name: generate_unique_identifier(rng, &mut config.existing_names),
            parameters,
            return_type: final_return_type,
            body: body_with_returns,
            current_indentation_layer: config.current_indentation_layer,
            visibility: Visibility::generate_random_visibility(is_method, rng),
            is_method,
        };

        // Add the generated function to external_functions
        config
            .external_functions
            .borrow_mut()
            .push(function.clone());

        // Add the function to the typed context for type checking
        let _ = typed_context.add_function(&function);

        Some(function)
    }

    /// Generate a concrete type for a generic class by replacing type parameters with concrete types
    fn generate_concrete_type_for_generic_class<T: Rng + SeedableRng>(
        custom_class: &CustomClass,
        rng: &mut T,
    ) -> Class {
        let base_name = custom_class.get_base_name();
        let generic_params = custom_class.get_generic_parameters();

        if generic_params.is_empty() {
            // Non-generic class, return as is
            Class::Custom(custom_class.clone())
        } else {
            // Generate concrete type arguments
            let concrete_type_args: Vec<String> = generic_params
                .iter()
                .map(|_| {
                    // Generate random basic types for type arguments
                    let basic_types = ["String", "Int", "Float", "Boolean"];
                    basic_types.choose(rng).unwrap().to_string()
                })
                .collect();

            // Create a new custom class with concrete type arguments
            // For method return types, we want concrete types like cu<Float, Boolean> instead of cu<T, T>
            let concrete_class_name = format!("{}<{}>", base_name, concrete_type_args.join(", "));

            // Create a new custom class with the concrete name but no generic parameters
            let mut concrete_class = custom_class.clone();
            concrete_class.name = concrete_class_name;
            concrete_class.generic_parameters.clear(); // Remove generic parameters since we're using concrete types

            Class::Custom(concrete_class)
        }
    }

    /// Generate a type-safe function with access to all classes and function signatures
    pub fn generate_type_safe_function_with_signatures<T: Rng + SeedableRng>(
        rng: &mut T,
        typed_context: &mut TypedGenerationContext,
        classes: &[CustomClass],
        function_signatures: &[Self],
        current_signature: &Self,
        current_indentation_layer: Option<usize>,
        _existing_names: Option<&mut Vec<String>>,
    ) -> Option<Self> {
        let current_indentation_layer = current_indentation_layer.unwrap_or(0);

        // Start with the current signature
        let mut function = current_signature.clone();

        // Create a separate typed context for this function
        let mut function_typed_context =
            TypedGenerationContext::new(typed_context.get_external_functions());

        // Convert classes to Class enum for the function context
        let defined_classes: Vec<Class> =
            classes.iter().map(|c| Class::Custom(c.clone())).collect();
        function_typed_context.set_defined_classes(defined_classes.clone());

        // Add function signatures to the typed context so functions can call other functions
        for function_signature in function_signatures {
            function_typed_context.add_function(function_signature);
        }

        // Convert class properties to parameters for function generation
        let external_variables: Vec<_> = classes
            .iter()
            .flat_map(|class| {
                class.properties.iter().map(|var| {
                    Parameter::new(
                        format!("other_{}_{}", class.get_base_name(), var.get_name()),
                        var.get_class()
                            .map(|c| Rc::new(c.clone()))
                            .unwrap_or_else(|| Rc::new(FLOAT.clone())),
                    )
                })
            })
            .collect();

        // Generate function body with access to all classes and functions
        let external_vars: Vec<Variable> = external_variables
            .iter()
            .map(|p| p.clone().into())
            .collect();
        let body = Block::generate_type_safe_block_with_config(
            &mut GenerationConfig::new(
                external_vars.clone(),
                typed_context.get_external_functions(),
                Some(defined_classes),
                current_indentation_layer,
                Self::MAX_DEPTH,
            ),
            &external_vars,
            false,
            &mut function_typed_context,
            function
                .return_type
                .as_ref()
                .map(|ty| Type::Basic(ty.clone()))
                .as_ref(),
            rng,
        )?;

        // Update the function with the generated body
        function.body = body;

        Some(function)
    }

    /// Decide on a return type for the function
    fn decide_return_type<T: Rng + SeedableRng>(rng: &mut T) -> Option<Class> {
        match rng.random_range(0..4) {
            0 => None, // No return type (Unit)
            1 => Some(BOOLEAN),
            2 => Some(FLOAT),
            _ => Some(INT),
        }
    }

    /// Decide on a return type for the function with support for custom types
    fn decide_return_type_with_custom_types<T: Rng + SeedableRng>(
        rng: &mut T,
        defined_classes: Option<&[Class]>,
    ) -> Option<Class> {
        // If we have defined classes, include them in the selection
        if let Some(classes) = defined_classes {
            if !classes.is_empty() {
                // 50% chance for basic types, 50% chance for custom types
                match rng.random_range(0..10) {
                    0..=4 => {
                        // Basic types
                        match rng.random_range(0..4) {
                            0 => None, // No return type (Unit)
                            1 => Some(BOOLEAN),
                            2 => Some(FLOAT),
                            _ => Some(INT),
                        }
                    }
                    5..=9 => {
                        // Custom types - select from available custom classes
                        let custom_classes: Vec<&Class> = classes
                            .iter()
                            .filter(|c| matches!(c, Class::Custom(_)))
                            .collect();

                        if !custom_classes.is_empty() {
                            let chosen_class = (*custom_classes.choose(rng).unwrap()).clone();

                            // For method return types, we want concrete types instead of generic parameters
                            if let Class::Custom(custom_class) = &chosen_class {
                                if !custom_class.get_generic_parameters().is_empty() {
                                    // Generate concrete type arguments for generic classes
                                    Some(Self::generate_concrete_type_for_generic_class(
                                        custom_class,
                                        rng,
                                    ))
                                } else {
                                    Some(chosen_class)
                                }
                            } else {
                                Some(chosen_class)
                            }
                        } else {
                            // Fallback to basic types if no custom classes available
                            match rng.random_range(0..4) {
                                0 => None, // No return type (Unit)
                                1 => Some(BOOLEAN),
                                2 => Some(FLOAT),
                                _ => Some(INT),
                            }
                        }
                    }
                    _ => unreachable!(),
                }
            } else {
                // No defined classes, use basic types only
                Self::decide_return_type(rng)
            }
        } else {
            // No defined classes provided, use basic types only
            Self::decide_return_type(rng)
        }
    }

    /// Decide on a return type for a method with support for custom types and generic parameters
    fn decide_method_return_type_with_custom_types<T: Rng + SeedableRng>(
        rng: &mut T,
        defined_classes: Option<&[Class]>,
        generic_parameters: Option<&[GenericTypeParameter]>,
    ) -> Option<Class> {
        // 20% chance to return a generic type parameter if available
        if let Some(params) = generic_parameters
            && !params.is_empty()
            && rng.random_bool(0.2)
        {
            // Filter parameters that can be return types (covariant or invariant)
            let returnable_params: Vec<_> = params
                .iter()
                .filter(|param| param.can_be_return_type())
                .collect();

            if !returnable_params.is_empty() {
                let _chosen_param = returnable_params.choose(rng).unwrap();
                // Return the generic type parameter as a Class::Generic
                let generic_type = GenericType::new(
                    Class::Basic(BasicType::String),
                    vec![Class::Basic(BasicType::String)],
                );
                return Some(Class::Generic(Box::new(generic_type)));
            }
        }

        // Fallback to normal custom type selection
        Self::decide_return_type_with_custom_types(rng, defined_classes)
    }

    /// Check if function body has any return statement (recursively)
    fn has_return_statement(body: &Block) -> bool {
        let statements = body.get_statements();

        for statement in statements {
            match statement {
                Statement::Single(single_stmt) => {
                    if let SingleStatement::Return(_) = single_stmt {
                        return true;
                    }
                }
                Statement::If(if_stmt) => {
                    // Check if block
                    if Self::has_return_statement(if_stmt.get_if_block()) {
                        return true;
                    }
                    // Check else if blocks
                    for (_, elseif_block) in if_stmt.get_elseif_blocks() {
                        if Self::has_return_statement(elseif_block) {
                            return true;
                        }
                    }
                    // Check else block
                    if let Some(else_block) = if_stmt.get_else_block()
                        && Self::has_return_statement(else_block)
                    {
                        return true;
                    }
                }
                Statement::For(for_stmt) => {
                    // Check loop block
                    if Self::has_return_statement(for_stmt.get_loop_block()) {
                        return true;
                    }
                }
                Statement::While(while_stmt) => {
                    // Check while block
                    if Self::has_return_statement(while_stmt.get_block()) {
                        return true;
                    }
                }
                Statement::When(when_stmt) => {
                    // Check all arms
                    for (_, arm_block) in when_stmt.get_arms() {
                        if Self::has_return_statement(arm_block) {
                            return true;
                        }
                    }
                    // Check else arm
                    if Self::has_return_statement(when_stmt.get_else_arm()) {
                        return true;
                    }
                }
            }
        }
        false
    }

    /// Check if all execution paths in function body have return statements (recursively)
    fn all_paths_have_return(body: &Block) -> bool {
        let statements = body.get_statements();

        for statement in statements {
            match statement {
                Statement::Single(single_stmt) => {
                    if let SingleStatement::Return(_) = single_stmt {
                        return true; // Found return, all paths from here have return
                    }
                }
                Statement::If(if_stmt) => {
                    // For if statements, we need to check if all execution paths lead to a return
                    // Check if any branch has a return statement
                    let if_has_return = Self::all_paths_have_return(if_stmt.get_if_block());

                    let mut any_elseif_has_return = false;
                    for (_, elseif_block) in if_stmt.get_elseif_blocks() {
                        if Self::all_paths_have_return(elseif_block) {
                            any_elseif_has_return = true;
                        }
                    }

                    let else_has_return = if let Some(else_block) = if_stmt.get_else_block() {
                        Self::all_paths_have_return(else_block)
                    } else {
                        false
                    };

                    // If any branch has a return, we need to check if the remaining code also has a return
                    // to ensure all paths have return
                    if if_has_return || any_elseif_has_return || else_has_return {
                        // Some branches have return, continue checking the rest
                        continue;
                    }
                    // No branch has return, continue checking
                }
                Statement::For(_for_stmt) => {
                    // For loops, we can't guarantee all paths have return
                    // (loop might not execute), so we don't consider them
                }
                Statement::While(_while_stmt) => {
                    // For while loops, we can't guarantee all paths have return
                    // (loop might not execute), so we don't consider them
                }
                Statement::When(when_stmt) => {
                    // For when statements, all arms must have return statements
                    for (_, arm_block) in when_stmt.get_arms() {
                        if !Self::all_paths_have_return(arm_block) {
                            return false;
                        }
                    }
                    // Else arm must also have return
                    if !Self::all_paths_have_return(when_stmt.get_else_arm()) {
                        return false;
                    }
                }
            }
        }
        false // No return statement found in this block
    }

    /// Check if function body has any empty return statement (recursively)
    fn has_empty_return_in_body(body: &Block) -> bool {
        let statements = body.get_statements();

        for statement in statements {
            match statement {
                Statement::Single(single_stmt) => {
                    if let SingleStatement::Return(expr) = single_stmt
                        && expr.is_none()
                    {
                        return true; // Found empty return
                    }
                }
                Statement::If(if_stmt) => {
                    // Check if block
                    if Self::has_empty_return_in_body(if_stmt.get_if_block()) {
                        return true;
                    }
                    // Check else if blocks
                    for (_, elseif_block) in if_stmt.get_elseif_blocks() {
                        if Self::has_empty_return_in_body(elseif_block) {
                            return true;
                        }
                    }
                    // Check else block
                    if let Some(else_block) = if_stmt.get_else_block()
                        && Self::has_empty_return_in_body(else_block)
                    {
                        return true;
                    }
                }
                Statement::For(for_stmt) => {
                    // Check loop block
                    if Self::has_empty_return_in_body(for_stmt.get_loop_block()) {
                        return true;
                    }
                }
                Statement::While(while_stmt) => {
                    // Check while block
                    if Self::has_empty_return_in_body(while_stmt.get_block()) {
                        return true;
                    }
                }
                Statement::When(when_stmt) => {
                    // Check all arms
                    for (_, arm_block) in when_stmt.get_arms() {
                        if Self::has_empty_return_in_body(arm_block) {
                            return true;
                        }
                    }
                    // Check else arm
                    if Self::has_empty_return_in_body(when_stmt.get_else_arm()) {
                        return true;
                    }
                }
            }
        }
        false
    }

    /// Add a return statement to function body
    fn add_return_statement_to_body<T: Rng + SeedableRng>(
        mut body: Block,
        expected_return_type: Option<&Type>,
        typed_context: &mut TypedGenerationContext,
        rng: &mut T,
    ) -> Option<Block> {
        // Create a return statement based on expected return type
        let return_statement = Statement::Single(
            typed_context.generate_type_safe_return_statement_with_type(expected_return_type, rng),
        );

        // Add return statement to the end of function body
        body.get_statements_mut().push(return_statement);

        // Create new block with return statement
        Some(body)
    }

    /// Infer return type from function body by analyzing return statements (recursively)
    fn infer_return_type_from_body(
        body: &Block,
        external_functions: &Rc<RefCell<Vec<Function>>>,
    ) -> Option<Class> {
        let statements = body.get_statements();
        let mut return_types = Vec::new();
        let mut has_empty_return = false;

        // Collect all return statement types (recursively)
        for statement in statements {
            match statement {
                Statement::Single(single_stmt) => {
                    if let SingleStatement::Return(expr) = single_stmt {
                        match expr {
                            Some(expr) => {
                                // If there's a return expression, infer its type based on the expression
                                let return_type = if expr.is_boolean() {
                                    // Boolean expression
                                    BOOLEAN
                                } else if expr.is_int(None, Some(external_functions.clone())) {
                                    // Integer expression
                                    INT
                                } else if expr.is_float(None, Some(external_functions.clone())) {
                                    // Float expression
                                    FLOAT
                                } else if let Expression::FunctionCall(func_name, _) = expr {
                                    // For function calls, look up the actual function return type
                                    if let Some(func) = external_functions
                                        .borrow()
                                        .iter()
                                        .find(|f| f.get_name() == func_name)
                                    {
                                        if let Some(func_return_type) = func.get_return_type() {
                                            func_return_type.clone()
                                        } else {
                                            // Function has no return type (Unit), so this return statement should be empty
                                            // This indicates a type mismatch - function call returns Unit but we're trying to return it
                                            // We should avoid this case by better generation logic
                                            // Return None to indicate this is an invalid return statement
                                            return None;
                                        }
                                    } else {
                                        // Function not found, return None to indicate invalid return
                                        return None;
                                    }
                                } else {
                                    // Default to Float for unknown expressions
                                    FLOAT
                                };
                                return_types.push(return_type);
                            }
                            None => {
                                // Return without value - Unit type
                                // If there's any empty return, function should not have return type annotation
                                has_empty_return = true;
                            }
                        }
                    }
                }
                Statement::If(if_stmt) => {
                    // Check if block for empty returns
                    if Self::has_empty_return_in_body(if_stmt.get_if_block()) {
                        has_empty_return = true;
                    }
                    if let Some(ty) = Self::infer_return_type_from_body(
                        if_stmt.get_if_block(),
                        external_functions,
                    ) {
                        return_types.push(ty);
                    }
                    // Check else if blocks
                    for (_, elseif_block) in if_stmt.get_elseif_blocks() {
                        if Self::has_empty_return_in_body(elseif_block) {
                            has_empty_return = true;
                        }
                        if let Some(ty) =
                            Self::infer_return_type_from_body(elseif_block, external_functions)
                        {
                            return_types.push(ty);
                        }
                    }
                    // Check else block
                    if let Some(else_block) = if_stmt.get_else_block() {
                        if Self::has_empty_return_in_body(else_block) {
                            has_empty_return = true;
                        }
                        if let Some(ty) =
                            Self::infer_return_type_from_body(else_block, external_functions)
                        {
                            return_types.push(ty);
                        }
                    }
                }
                Statement::For(for_stmt) => {
                    // Check loop block for empty returns
                    if Self::has_empty_return_in_body(for_stmt.get_loop_block()) {
                        has_empty_return = true;
                    }
                    if let Some(ty) = Self::infer_return_type_from_body(
                        for_stmt.get_loop_block(),
                        external_functions,
                    ) {
                        return_types.push(ty);
                    }
                }
                Statement::While(while_stmt) => {
                    // Check while block for empty returns
                    if Self::has_empty_return_in_body(while_stmt.get_block()) {
                        has_empty_return = true;
                    }
                    if let Some(ty) = Self::infer_return_type_from_body(
                        while_stmt.get_block(),
                        external_functions,
                    ) {
                        return_types.push(ty);
                    }
                }
                Statement::When(when_stmt) => {
                    // Check all arms for empty returns
                    for (_, arm_block) in when_stmt.get_arms() {
                        if Self::has_empty_return_in_body(arm_block) {
                            has_empty_return = true;
                        }
                        if let Some(ty) =
                            Self::infer_return_type_from_body(arm_block, external_functions)
                        {
                            return_types.push(ty);
                        }
                    }
                    // Check else arm for empty returns
                    if Self::has_empty_return_in_body(when_stmt.get_else_arm()) {
                        has_empty_return = true;
                    }
                    if let Some(ty) = Self::infer_return_type_from_body(
                        when_stmt.get_else_arm(),
                        external_functions,
                    ) {
                        return_types.push(ty);
                    }
                }
            }
        }

        // If there are empty returns, function should not have return type annotation
        if has_empty_return {
            return None;
        }

        // If no return statements found, no return type annotation
        if return_types.is_empty() {
            return None;
        }

        // Check for type consistency
        let first_type = &return_types[0];
        let all_same_type = return_types.iter().all(|ty| ty == first_type);

        if all_same_type {
            // All return statements have the same type, use that type
            Some(first_type.clone())
        } else {
            // Mixed types - we need to find a common type or choose the most appropriate one
            let has_boolean = return_types.contains(&BOOLEAN);
            let has_int = return_types.iter().any(|ty| ty.is_signed_integer_type());
            let has_float = return_types.iter().any(|ty| ty.is_float_type());

            if has_boolean && (has_int || has_float) {
                // Mixed Boolean and Number types - this is problematic
                // We should avoid this case by better generation logic
                // For now, we'll choose the most common type by counting occurrences
                let mut boolean_count = 0;
                let mut int_count = 0;
                let mut float_count = 0;

                for ty in &return_types {
                    if ty == &BOOLEAN {
                        boolean_count += 1;
                    } else if ty.is_signed_integer_type() {
                        int_count += 1;
                    } else if ty.is_float_type() {
                        float_count += 1;
                    }
                }

                // Return the most common type
                if boolean_count >= int_count && boolean_count >= float_count {
                    Some(BOOLEAN)
                } else if int_count >= float_count {
                    Some(INT)
                } else {
                    Some(FLOAT)
                }
            } else if has_int && has_float {
                // Mixed Int and Float - promote to Float (common type promotion)
                Some(FLOAT)
            } else if has_int {
                // Only Int types
                Some(INT)
            } else if has_float {
                // Only Float types
                Some(FLOAT)
            } else {
                // Only Boolean types
                Some(BOOLEAN)
            }
        }
    }

    pub fn is_method(&self) -> bool {
        self.is_method
    }

    pub fn is_boolean_function(&self) -> bool {
        self.get_return_type()
            .is_some_and(|ty| ty.is_boolean_type())
    }

    pub fn is_numeric_function(&self) -> bool {
        self.get_return_type()
            .is_some_and(|ty| ty.is_numeric_type())
    }

    pub fn is_unit_function(&self) -> bool {
        self.get_return_type().is_none()
    }

    pub fn is_integer_function(&self) -> bool {
        self.get_return_type()
            .is_some_and(|ty| ty.is_integer_type())
    }

    pub fn is_float_function(&self) -> bool {
        self.get_return_type().is_some_and(|ty| ty.is_float_type())
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let indentation = SPACE.repeat(self.current_indentation_layer * INDENT_SIZE);
        let visibility = if self.is_method() {
            format!("{} ", self.visibility)
        } else {
            "".to_string()
        };
        let return_type: String = match &self.return_type {
            Some(ty) => format!(": {}", ty),
            None => "".to_string(),
        };
        let params_str = map_collect_join(&self.parameters, |p| p.to_string(), ", ");
        write!(
            f,
            "{indentation}{visibility}fun {}({}){return_type} {}",
            self.name, params_str, self.body,
        )?;

        Ok(())
    }
}
