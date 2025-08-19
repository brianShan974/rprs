use rand::{Rng, SeedableRng};

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
        basic_types::{BasicType, FloatingPointType, NumberType, SignedIntegerType},
        class::Class,
    },
    utils::generate_random_identifier,
    var::variable::Variable,
};
use crate::type_system::{Type, TypedGenerationContext};

#[derive(Clone)]
pub struct Function {
    name: String,
    parameters: Vec<Parameter>,
    return_type: Option<Class>,
    body: Block,
    current_indentation_layer: usize,
}

impl Function {
    pub const MAX_DEPTH: usize = 5;

    pub fn generate_random_function<T: Rng + SeedableRng>(
        external_variables: Vec<Parameter>,
        external_functions: Rc<RefCell<Vec<Function>>>,
        current_indentation_layer: Option<usize>,
        max_depth: Option<usize>,
        rng: &mut T,
    ) -> Option<Self> {
        if matches!(max_depth, Some(0)) {
            return None;
        }

        let max_depth = max_depth.unwrap_or(Self::MAX_DEPTH);
        let current_indentation_layer = current_indentation_layer.unwrap_or(0);
        let parameters = Parameter::generate_random_parameters(rng);
        let all_identifiers: Vec<Variable> = external_variables
            .into_iter()
            .chain(parameters.clone().into_iter())
            .map(|p| p.into())
            .collect();

        let function = Self {
            name: generate_random_identifier(rng),
            parameters,
            return_type: None,
            body: Block::generate_random_block(
                all_identifiers,
                external_functions.clone(),
                current_indentation_layer,
                false,
                max_depth,
                rng,
            )?,
            current_indentation_layer,
        };

        // Add the generated function to external_functions
        external_functions.borrow_mut().push(function.clone());

        Some(function)
    }

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

    /// Generate a type-safe function using typed generation context
    pub fn generate_type_safe_function<T: Rng + SeedableRng>(
        external_variables: Vec<Parameter>,
        external_functions: Rc<RefCell<Vec<Function>>>,
        current_indentation_layer: Option<usize>,
        max_depth: Option<usize>,
        typed_context: &mut TypedGenerationContext,
        rng: &mut T,
    ) -> Option<Self> {
        if matches!(max_depth, Some(0)) {
            return None;
        }

        let max_depth = max_depth.unwrap_or(Self::MAX_DEPTH);
        let current_indentation_layer = current_indentation_layer.unwrap_or(0);
        let parameters = Parameter::generate_random_parameters(rng);
        let all_identifiers: Vec<Variable> = external_variables
            .into_iter()
            .chain(parameters.clone().into_iter())
            .map(|p| p.into())
            .collect();

        // Decide on return type first (before generating body)
        let return_type = Self::decide_return_type(rng);

        // Convert return type to Type system type for context
        let return_type_system = return_type.as_ref().map(|ty| Type::Basic(ty.clone()));

        // Generate function body with expected return type
        let body = Block::generate_type_safe_block_with_return_type(
            all_identifiers,
            external_functions.clone(),
            current_indentation_layer,
            false,
            max_depth,
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
            Self::infer_return_type_from_body(&body_with_returns)
        } else {
            return_type
        };

        let function = Self {
            name: generate_random_identifier(rng),
            parameters,
            return_type: final_return_type,
            body: body_with_returns,
            current_indentation_layer,
        };

        // Add the generated function to external_functions
        external_functions.borrow_mut().push(function.clone());

        Some(function)
    }

    /// Decide on a return type for the function
    fn decide_return_type<T: Rng + SeedableRng>(rng: &mut T) -> Option<Class> {
        match rng.random_range(0..4) {
            0 => None, // No return type (Unit)
            1 => Some(Class::Basic(BasicType::Boolean)),
            2 => Some(Class::Basic(BasicType::Number(NumberType::FloatingPoint(
                FloatingPointType::Float,
            )))),
            _ => Some(Class::Basic(BasicType::Number(NumberType::SignedInteger(
                SignedIntegerType::Int,
            )))),
        }
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
                    if let Some(else_block) = if_stmt.get_else_block() {
                        if Self::has_return_statement(else_block) {
                            return true;
                        }
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
                    if let SingleStatement::Return(expr) = single_stmt {
                        if expr.is_none() {
                            return true; // Found empty return
                        }
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
                    if let Some(else_block) = if_stmt.get_else_block() {
                        if Self::has_empty_return_in_body(else_block) {
                            return true;
                        }
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
    fn infer_return_type_from_body(body: &Block) -> Option<Class> {
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
                                    Class::Basic(BasicType::Boolean)
                                } else if expr.is_int() {
                                    // Integer expression
                                    Class::Basic(BasicType::Number(NumberType::SignedInteger(
                                        SignedIntegerType::Int,
                                    )))
                                } else if expr.is_float() {
                                    // Float expression
                                    Class::Basic(BasicType::Number(NumberType::FloatingPoint(
                                        FloatingPointType::Float,
                                    )))
                                } else {
                                    // Default to Float for unknown expressions
                                    Class::Basic(BasicType::Number(NumberType::FloatingPoint(
                                        FloatingPointType::Float,
                                    )))
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
                    if let Some(ty) = Self::infer_return_type_from_body(if_stmt.get_if_block()) {
                        return_types.push(ty);
                    }
                    // Check else if blocks
                    for (_, elseif_block) in if_stmt.get_elseif_blocks() {
                        if Self::has_empty_return_in_body(elseif_block) {
                            has_empty_return = true;
                        }
                        if let Some(ty) = Self::infer_return_type_from_body(elseif_block) {
                            return_types.push(ty);
                        }
                    }
                    // Check else block
                    if let Some(else_block) = if_stmt.get_else_block() {
                        if Self::has_empty_return_in_body(else_block) {
                            has_empty_return = true;
                        }
                        if let Some(ty) = Self::infer_return_type_from_body(else_block) {
                            return_types.push(ty);
                        }
                    }
                }
                Statement::For(for_stmt) => {
                    // Check loop block for empty returns
                    if Self::has_empty_return_in_body(for_stmt.get_loop_block()) {
                        has_empty_return = true;
                    }
                    if let Some(ty) = Self::infer_return_type_from_body(for_stmt.get_loop_block()) {
                        return_types.push(ty);
                    }
                }
                Statement::While(while_stmt) => {
                    // Check while block for empty returns
                    if Self::has_empty_return_in_body(while_stmt.get_block()) {
                        has_empty_return = true;
                    }
                    if let Some(ty) = Self::infer_return_type_from_body(while_stmt.get_block()) {
                        return_types.push(ty);
                    }
                }
                Statement::When(when_stmt) => {
                    // Check all arms for empty returns
                    for (_, arm_block) in when_stmt.get_arms() {
                        if Self::has_empty_return_in_body(arm_block) {
                            has_empty_return = true;
                        }
                        if let Some(ty) = Self::infer_return_type_from_body(arm_block) {
                            return_types.push(ty);
                        }
                    }
                    // Check else arm for empty returns
                    if Self::has_empty_return_in_body(when_stmt.get_else_arm()) {
                        has_empty_return = true;
                    }
                    if let Some(ty) = Self::infer_return_type_from_body(when_stmt.get_else_arm()) {
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
            let has_boolean = return_types
                .iter()
                .any(|ty| matches!(ty, Class::Basic(BasicType::Boolean)));
            let has_int = return_types.iter().any(|ty| {
                matches!(
                    ty,
                    Class::Basic(BasicType::Number(NumberType::SignedInteger(_)))
                )
            });
            let has_float = return_types.iter().any(|ty| {
                matches!(
                    ty,
                    Class::Basic(BasicType::Number(NumberType::FloatingPoint(_)))
                )
            });

            if has_boolean && (has_int || has_float) {
                // Mixed Boolean and Number types - this is problematic
                // We should avoid this case by better generation logic
                // For now, we'll choose the first type found
                Some(first_type.clone())
            } else if has_int && has_float {
                // Mixed Int and Float - promote to Float (common type promotion)
                Some(Class::Basic(BasicType::Number(NumberType::FloatingPoint(
                    FloatingPointType::Float,
                ))))
            } else if has_int {
                // Only Int types
                Some(Class::Basic(BasicType::Number(NumberType::SignedInteger(
                    SignedIntegerType::Int,
                ))))
            } else if has_float {
                // Only Float types
                Some(Class::Basic(BasicType::Number(NumberType::FloatingPoint(
                    FloatingPointType::Float,
                ))))
            } else {
                // Only Boolean types
                Some(Class::Basic(BasicType::Boolean))
            }
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let indentation = SPACE.repeat(self.current_indentation_layer * INDENT_SIZE);
        match &self.return_type {
            Some(ty) => writeln!(
                f,
                "{indentation}fun {}({}): {} {}",
                self.name,
                self.parameters
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                ty,
                self.body,
            )?,
            None => writeln!(
                f,
                "{indentation}fun {}({}) {}",
                self.name,
                self.parameters
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                self.body,
            )?,
        }

        Ok(())
    }
}
