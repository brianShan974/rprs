use rand::{Rng, SeedableRng, seq::IndexedRandom};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::type_system::{Type, TypeChecker};
use crate::{
    basic::{
        body::{
            fun::function::Function,
            stmt::{single_statement::SingleStatement, statement::Statement},
        },
        cls::{
            basic_type::BasicType,
            class::{BOOLEAN, Class, FLOAT, INT, STRING},
            number_types::{
                floating_point::FloatingPointType, number::NumberType,
                signed_integer::SignedIntegerType, unsigned_integer::UnsignedIntegerType,
            },
        },
        expr::{
            arithmetic_expression::ArithmeticExpression, boolean_expression::BooleanExpression,
            expression::Expression, operator::Operator,
        },
        var::variable::Variable,
    },
    type_system::type_result::TypeResult,
};

/// Type-aware code generation context
pub struct TypedGenerationContext {
    /// Type checker for validation
    type_checker: TypeChecker,
    /// Current variable types in scope
    variable_types: HashMap<Variable, Type>,
    /// Available functions with their signatures
    function_signatures: HashMap<String, Type>,
    /// External functions reference
    external_functions: Rc<RefCell<Vec<Function>>>,
    /// Current class methods (if we're inside a class method)
    current_class_methods: Option<Vec<Function>>,
}

impl TypedGenerationContext {
    pub fn new(external_functions: Rc<RefCell<Vec<Function>>>) -> Self {
        Self {
            type_checker: TypeChecker::default(),
            variable_types: HashMap::new(),
            function_signatures: HashMap::new(),
            external_functions,
            current_class_methods: None,
        }
    }

    /// Create a new child context that inherits the parent's variables and functions
    /// This is used for block scoping - variables added to the child won't affect the parent
    pub fn create_child_context(&self) -> Self {
        Self {
            type_checker: TypeChecker::default(),
            variable_types: self.variable_types.clone(),
            function_signatures: self.function_signatures.clone(),
            external_functions: self.external_functions.clone(),
            current_class_methods: self.current_class_methods.clone(),
        }
    }

    /// Add a variable to the context with type tracking
    pub fn add_variable(&mut self, var: &Variable) -> TypeResult<()> {
        let var_type = self.infer_variable_type(var)?;
        let type_system_type = self.class_to_type_system_type(var_type);

        self.variable_types
            .insert(var.clone(), type_system_type.clone());
        self.type_checker
            .add_variable(var.get_name().to_string(), type_system_type)?;

        Some(())
    }

    /// Add a function to the context with type tracking
    pub fn add_function(&mut self, func: &Function) -> TypeResult<()> {
        let func_type = self.infer_function_type(func);

        self.function_signatures
            .insert(func.get_name().to_string(), func_type.clone());
        self.type_checker
            .add_function(func.get_name().to_string(), func_type)?;

        Some(())
    }

    pub fn get_external_functions(&self) -> Rc<RefCell<Vec<Function>>> {
        self.external_functions.clone()
    }

    /// Set the current class methods (used when generating code inside a class method)
    pub fn set_current_class_methods(&mut self, methods: Vec<Function>) {
        self.current_class_methods = Some(methods);
    }

    /// Get the current class methods (if we're inside a class method)
    pub fn get_current_class_methods(&self) -> Option<&Vec<Function>> {
        self.current_class_methods.as_ref()
    }

    /// Check if we're currently inside a class method
    pub fn is_inside_class_method(&self) -> bool {
        self.current_class_methods.is_some()
    }

    /// Generate a type-safe variable assignment
    pub fn generate_type_safe_assignment<T: Rng + SeedableRng>(
        &self,
        var: &Variable,
        rng: &mut T,
    ) -> TypeResult<SingleStatement> {
        if let Some(var_type) = self.variable_types.get(var) {
            // Generate an expression that strictly matches the variable's type
            let expr = self.generate_expression_for_type(
                var_type,
                Some(self.external_functions.clone()),
                rng,
            )?;

            // Validate that the generated expression type matches the variable type
            let expr_type = self.infer_expression_type(&expr)?;
            self.type_checker
                .check_compatibility(&expr_type, var_type)?;

            Some(SingleStatement::Assignment(
                var.get_name().to_string(),
                expr,
            ))
        } else {
            None
        }
    }

    /// Generate a type-safe function call
    pub fn generate_type_safe_function_call<T: Rng + SeedableRng>(
        &self,
        func_name: &str,
        rng: &mut T,
    ) -> TypeResult<SingleStatement> {
        if let Some(func_type) = self.function_signatures.get(func_name) {
            match func_type {
                Type::Function(param_types, _return_type) => {
                    let mut args = Vec::new();

                    // Generate arguments that match the parameter types
                    for param_type in param_types {
                        let arg = self.generate_expression_for_type(
                            param_type,
                            Some(self.external_functions.clone()),
                            rng,
                        )?;
                        args.push(arg);
                    }

                    Some(SingleStatement::FunctionCall(func_name.to_string(), args))
                }
                _ => None,
            }
        } else {
            None
        }
    }

    /// Generate a type-safe function call expression that returns a specific type
    pub fn generate_type_safe_function_call_expression<T: Rng + SeedableRng>(
        &self,
        target_type: &Type,
        rng: &mut T,
    ) -> TypeResult<Expression> {
        self.generate_type_safe_function_call_expression_with_depth(target_type, 0, rng)
    }

    /// Generate a type-safe function call expression with depth limit
    fn generate_type_safe_function_call_expression_with_depth<T: Rng + SeedableRng>(
        &self,
        target_type: &Type,
        depth: usize,
        rng: &mut T,
    ) -> TypeResult<Expression> {
        let functions = &self.external_functions;
        let functions_borrowed = functions.borrow();

        // Find functions that return the target type, but exclude the current function being generated
        let compatible_functions: Vec<_> = functions_borrowed
            .iter()
            .filter(|func| {
                if let Some(Type::Function(_, return_type)) =
                    self.function_signatures.get(func.get_name())
                {
                    // Check if return type is compatible with target type
                    self.type_checker
                        .check_compatibility(return_type, target_type)
                        .is_some()
                } else {
                    false
                }
            })
            .collect();

        if compatible_functions.is_empty() {
            // Fallback to generating a simple expression of the target type
            return self.generate_expression_for_type_with_depth(
                target_type,
                Some(functions.clone()),
                depth + 1,
                rng,
            );
        }

        // Randomly select a compatible function
        let function = compatible_functions.choose(rng).unwrap();
        let func_name = function.get_name();

        if let Some(Type::Function(param_types, _)) = self.function_signatures.get(func_name) {
            let mut args = Vec::new();

            // Generate arguments that match the parameter types
            for param_type in param_types {
                let arg = self.generate_expression_for_type_with_depth(
                    param_type,
                    Some(functions.clone()),
                    depth + 1,
                    rng,
                )?;
                args.push(arg);
            }

            // Return the appropriate expression type based on target type
            match target_type {
                Type::Basic(Class::Basic(BasicType::Number(_))) => {
                    // For numeric types, return ArithmeticExpression
                    Some(Expression::Arithmetic(ArithmeticExpression::FunctionCall(
                        func_name.to_string(),
                        args,
                    )))
                }
                Type::Basic(BOOLEAN) => {
                    // For boolean type, return BooleanExpression with function call
                    Some(Expression::Boolean(BooleanExpression::FunctionCall(
                        func_name.to_string(),
                        args,
                    )))
                }
                Type::Basic(STRING) => {
                    // For string type, return Expression::FunctionCall
                    Some(Expression::FunctionCall(func_name.to_string(), args))
                }
                _ => {
                    // For other types, return Expression::FunctionCall
                    Some(Expression::FunctionCall(func_name.to_string(), args))
                }
            }
        } else {
            // Fallback
            self.generate_expression_for_type_with_depth(
                target_type,
                Some(functions.clone()),
                depth + 1,
                rng,
            )
        }
    }

    /// Generate a type-safe arithmetic expression
    pub fn generate_type_safe_arithmetic_expression<T: Rng + SeedableRng>(
        &self,
        target_type: &Type,
        max_depth: usize,
        rng: &mut T,
    ) -> TypeResult<ArithmeticExpression> {
        if max_depth == 0 {
            // Generate simple literal based on target type
            match target_type {
                Type::Basic(Class::Basic(BasicType::Number(NumberType::SignedInteger(_)))) => {
                    Some(ArithmeticExpression::generate_random_int_literal(rng))
                }
                Type::Basic(Class::Basic(BasicType::Number(NumberType::FloatingPoint(_)))) => {
                    Some(ArithmeticExpression::generate_random_float_literal(rng))
                }
                _ => {
                    // Default to int for other types
                    Some(ArithmeticExpression::generate_random_int_literal(rng))
                }
            }
        } else {
            // 40% chance for function call, 30% chance for binary op, 30% chance for literal
            match rng.random_range(0..10) {
                0..=3 => {
                    // Generate type-safe function call
                    match self.generate_type_safe_function_call_expression(target_type, rng) {
                        Some(Expression::Arithmetic(arith_expr)) => Some(arith_expr),
                        _ => {
                            // Fallback to literal
                            match target_type {
                                Type::Basic(Class::Basic(BasicType::Number(
                                    NumberType::SignedInteger(_),
                                ))) => Some(ArithmeticExpression::generate_random_int_literal(rng)),
                                Type::Basic(Class::Basic(BasicType::Number(
                                    NumberType::FloatingPoint(_),
                                ))) => {
                                    Some(ArithmeticExpression::generate_random_float_literal(rng))
                                }
                                _ => Some(ArithmeticExpression::generate_random_int_literal(rng)),
                            }
                        }
                    }
                }
                4..=6 => {
                    // Generate binary operation
                    let left = self.generate_type_safe_arithmetic_expression(
                        target_type,
                        max_depth - 1,
                        rng,
                    )?;
                    let right = self.generate_type_safe_arithmetic_expression(
                        target_type,
                        max_depth - 1,
                        rng,
                    )?;
                    let op = Operator::generate_random_operator(rng);
                    Some(ArithmeticExpression::BinaryOp {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                    })
                }
                7..=9 => {
                    // Generate literal
                    match target_type {
                        Type::Basic(Class::Basic(BasicType::Number(
                            NumberType::SignedInteger(_),
                        ))) => Some(ArithmeticExpression::generate_random_int_literal(rng)),
                        Type::Basic(Class::Basic(BasicType::Number(
                            NumberType::FloatingPoint(_),
                        ))) => Some(ArithmeticExpression::generate_random_float_literal(rng)),
                        _ => Some(ArithmeticExpression::generate_random_int_literal(rng)),
                    }
                }
                _ => unreachable!(),
            }
        }
    }

    /// Generate a variable with a compatible type for the current context
    pub fn generate_type_compatible_variable<T: Rng + SeedableRng>(
        &self,
        is_member: bool,
        rng: &mut T,
    ) -> Variable {
        // Generate a variable with a common numeric type for compatibility
        let target_type = Some(FLOAT);

        let variables: Vec<Variable> = self.variable_types.keys().cloned().collect();
        Variable::generate_random_variable_with_type(
            is_member,
            true,
            target_type,
            Some(&variables),
            rng,
        )
    }

    /// Generate a variable with a compatible type for the current context (no const)
    pub fn generate_type_compatible_variable_no_const<T: Rng + SeedableRng>(
        &self,
        is_member: bool,
        rng: &mut T,
    ) -> Variable {
        // Generate a variable with a common numeric type for compatibility
        let variables: Vec<Variable> = self.variable_types.keys().cloned().collect();
        Variable::generate_random_variable_with_const_control(
            is_member,
            true,
            Some(&variables),
            false, // Don't allow const
            rng,
        )
    }

    /// Get available mutable variables for assignment
    pub fn get_mutable_variables(&self) -> Vec<Variable> {
        self.variable_types
            .keys()
            .filter(|var| var.is_mutable())
            .cloned()
            .collect()
    }

    /// Get available functions for calling
    pub fn get_available_functions(&self) -> Vec<String> {
        let functions = self.external_functions.borrow();
        functions.iter().map(|f| f.get_name().to_string()).collect()
    }

    /// Validate a statement for type safety
    pub fn validate_statement(&mut self, statement: &Statement) -> TypeResult<()> {
        match statement {
            Statement::Single(single) => self.validate_single_statement(single),
            // For now, just validate single statements
            _ => Some(()),
        }
    }

    /// Validate a single statement for type safety
    fn validate_single_statement(&mut self, statement: &SingleStatement) -> TypeResult<()> {
        match statement {
            SingleStatement::VariableDeclaration(var) => {
                self.add_variable(var)?;
                Some(())
            }
            SingleStatement::Assignment(var_name, expr) => {
                // Find the variable in our context by name
                let var = self
                    .variable_types
                    .keys()
                    .find(|v| v.get_name() == var_name);
                if let Some(var) = var {
                    let var_type = self.variable_types.get(var).unwrap();
                    let expr_type = self.infer_expression_type(expr)?;
                    self.type_checker.check_compatibility(&expr_type, var_type)
                } else {
                    None
                }
            }
            SingleStatement::FunctionCall(func_name, args) => {
                if let Some(func_type) = self.function_signatures.get(func_name) {
                    match func_type {
                        Type::Function(param_types, _) => {
                            if args.len() != param_types.len() {
                                return None;
                            }

                            for (arg, expected_type) in args.iter().zip(param_types.iter()) {
                                let arg_type = self.infer_expression_type(arg)?;
                                self.type_checker
                                    .check_compatibility(&arg_type, expected_type)?;
                            }
                            Some(())
                        }
                        _ => None,
                    }
                } else {
                    // Function not in context, but might be external - allow for now
                    Some(())
                }
            }
            SingleStatement::Return(_) => Some(()),
            SingleStatement::ObjectCreation(_) => Some(()), // For now, just accept object creation
            SingleStatement::CompoundAssignment(var_name, _, expr) => {
                // Find the variable in our context by name
                let var = self
                    .variable_types
                    .keys()
                    .find(|v| v.get_name() == var_name);
                if let Some(var) = var {
                    let var_type = self.variable_types.get(var).unwrap();
                    let expr_type = self.infer_expression_type(expr)?;
                    // For compound assignment, both the variable and expression should be numeric
                    if matches!(var_type, Type::Basic(Class::Basic(BasicType::Number(_)))) {
                        self.type_checker.check_compatibility(&expr_type, var_type)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        }
    }

    /// Convert Class to Type system Type
    fn class_to_type_system_type(&self, class: Option<Class>) -> Type {
        match class {
            Some(class) => Type::Basic(class),
            None => Type::Unknown,
        }
    }

    /// Infer the type of a variable
    fn infer_variable_type(&self, var: &Variable) -> TypeResult<Option<Class>> {
        Some(var.get_type().cloned())
    }

    /// Infer the type of a function
    fn infer_function_type(&self, func: &Function) -> Type {
        // Infer parameter types from function parameters
        let param_types: Vec<Type> = func
            .get_parameters()
            .iter()
            .map(|p| Type::Basic(p.get_type().clone()))
            .collect();

        // Infer return type from function body
        let return_type = self.infer_function_return_type(func);

        Type::Function(param_types, Box::new(return_type))
    }

    /// Infer the return type of a function by analyzing its body
    fn infer_function_return_type(&self, func: &Function) -> Type {
        // Analyze the function body to find return statements
        let statements = func.get_body().get_statements();

        for statement in statements {
            if let Statement::Single(single_stmt) = statement
                && let SingleStatement::Return(expr) = single_stmt
            {
                return match expr {
                    Some(expr) => {
                        // If there's a return expression, infer its type
                        if let Some(ty) = self.infer_expression_type(expr) {
                            ty
                        } else {
                            Type::Basic(FLOAT)
                        }
                    }
                    None => {
                        // Return without value - Unit type
                        Type::Basic(BOOLEAN)
                    }
                };
            }
        }

        // If no return statement found, assume Unit type
        Type::Basic(BOOLEAN)
    }

    /// Infer the type of an expression
    fn infer_expression_type(&self, expr: &Expression) -> TypeResult<Type> {
        match expr {
            Expression::Arithmetic(arith) => {
                match arith {
                    ArithmeticExpression::Int(_) => Some(Type::Basic(INT)),
                    ArithmeticExpression::Float(_) => Some(Type::Basic(FLOAT)),
                    ArithmeticExpression::BinaryOp { left, right, .. } => {
                        // For binary operations, if both operands are int, result is int
                        // Otherwise, result is float
                        let left_type = self.infer_arithmetic_expression_type(left)?;
                        let right_type = self.infer_arithmetic_expression_type(right)?;
                        if matches!(
                            left_type,
                            Type::Basic(Class::Basic(BasicType::Number(
                                NumberType::SignedInteger(_)
                            )))
                        ) && matches!(
                            right_type,
                            Type::Basic(Class::Basic(BasicType::Number(
                                NumberType::SignedInteger(_)
                            )))
                        ) {
                            Some(Type::Basic(INT))
                        } else {
                            Some(Type::Basic(FLOAT))
                        }
                    }
                    ArithmeticExpression::FunctionCall(func_name, _) => {
                        // Look up function return type
                        if let Some(Type::Function(_, return_type)) =
                            self.function_signatures.get(func_name)
                        {
                            Some(*return_type.clone())
                        } else {
                            Some(Type::Basic(FLOAT)) // Fallback
                        }
                    }
                    ArithmeticExpression::VariableReference(var_name) => {
                        // Look up variable type
                        for (var, var_type) in &self.variable_types {
                            if var.get_name() == var_name {
                                return Some(var_type.clone());
                            }
                        }
                        Some(Type::Basic(FLOAT)) // Fallback
                    }
                }
            }
            Expression::Boolean(_) => Some(Type::Basic(BOOLEAN)),
            Expression::StringLiteral(_) => Some(Type::Basic(STRING)),
            Expression::ClassInstantiation(_) => None, // TODO: Handle class instantiation properly
            Expression::FunctionCall(func_name, _) => {
                // Look up function return type
                if let Some(Type::Function(_, return_type)) =
                    self.function_signatures.get(func_name)
                {
                    Some(*return_type.clone())
                } else {
                    Some(Type::Unknown) // Fallback
                }
            }
            Expression::VariableReference(var_name) => {
                // Look up variable type
                for (var, var_type) in &self.variable_types {
                    if var.get_name() == var_name {
                        return Some(var_type.clone());
                    }
                }
                Some(Type::Unknown) // Fallback
            }
        }
    }

    /// Infer the type of an arithmetic expression
    fn infer_arithmetic_expression_type(&self, arith: &ArithmeticExpression) -> TypeResult<Type> {
        match arith {
            ArithmeticExpression::Int(_) => Some(Type::Basic(INT)),
            ArithmeticExpression::Float(_) => Some(Type::Basic(FLOAT)),
            ArithmeticExpression::BinaryOp { left, right, .. } => {
                // For binary operations, if both operands are int, result is int
                // Otherwise, result is float
                let left_type = self.infer_arithmetic_expression_type(left)?;
                let right_type = self.infer_arithmetic_expression_type(right)?;
                if matches!(
                    left_type,
                    Type::Basic(Class::Basic(BasicType::Number(NumberType::SignedInteger(
                        _
                    ))))
                ) && matches!(
                    right_type,
                    Type::Basic(Class::Basic(BasicType::Number(NumberType::SignedInteger(
                        _
                    ))))
                ) {
                    Some(Type::Basic(INT))
                } else {
                    Some(Type::Basic(FLOAT))
                }
            }
            ArithmeticExpression::FunctionCall(func_name, _) => {
                // Look up function return type
                if let Some(Type::Function(_, return_type)) =
                    self.function_signatures.get(func_name)
                {
                    Some(*return_type.clone())
                } else {
                    Some(Type::Basic(FLOAT)) // Fallback
                }
            }
            ArithmeticExpression::VariableReference(var_name) => {
                // Look up variable type
                for (var, var_type) in &self.variable_types {
                    if var.get_name() == var_name {
                        return Some(var_type.clone());
                    }
                }
                Some(Type::Basic(FLOAT)) // Fallback
            }
        }
    }

    /// Generate a type-safe return statement (empty return only)
    pub fn generate_type_safe_return_statement(&self) -> SingleStatement {
        // Always generate empty return when no type context is provided
        // Functions should have consistent return behavior based on their type annotation
        SingleStatement::Return(None)
    }

    /// Generate a type-safe return statement with specific return type
    pub fn generate_type_safe_return_statement_with_type<T: Rng + SeedableRng>(
        &self,
        expected_return_type: Option<&Type>,
        rng: &mut T,
    ) -> SingleStatement {
        match expected_return_type {
            Some(return_type) => {
                // If we have an expected return type, generate appropriate return
                match return_type {
                    Type::Basic(Class::Basic(BasicType::Number(_))) => {
                        // Numeric return type - generate return with expression that strictly matches the type
                        let expr = self
                            .generate_expression_for_type(
                                return_type,
                                Some(self.external_functions.clone()),
                                rng,
                            )
                            .unwrap_or(
                                // Fallback: generate type-specific literal
                                match return_type {
                                    Type::Basic(Class::Basic(BasicType::Number(
                                        NumberType::FloatingPoint(_),
                                    ))) => Expression::generate_random_float_literal(rng),
                                    Type::Basic(Class::Basic(BasicType::Number(
                                        NumberType::SignedInteger(_),
                                    ))) => Expression::generate_random_int_literal(rng),
                                    _ => Expression::generate_random_int_literal(rng),
                                },
                            );
                        SingleStatement::Return(Some(expr))
                    }
                    Type::Basic(BOOLEAN) => {
                        // Boolean return type - generate boolean expression
                        let expr = self
                            .generate_expression_for_type(
                                return_type,
                                Some(self.external_functions.clone()),
                                rng,
                            )
                            .unwrap_or(self.generate_boolean_expression(rng));
                        SingleStatement::Return(Some(expr))
                    }
                    Type::Basic(STRING) => {
                        // String return type - generate string expression
                        let expr = self
                            .generate_expression_for_type(
                                return_type,
                                Some(self.external_functions.clone()),
                                rng,
                            )
                            .unwrap_or(Expression::generate_random_string_literal(rng));
                        SingleStatement::Return(Some(expr))
                    }
                    _ => {
                        // Other types - try to generate appropriate expression or fallback to empty return
                        if let Some(expr) = self.generate_expression_for_type(
                            return_type,
                            Some(self.external_functions.clone()),
                            rng,
                        ) {
                            SingleStatement::Return(Some(expr))
                        } else {
                            SingleStatement::Return(None)
                        }
                    }
                }
            }
            None => {
                // No expected return type - only generate empty return
                // Functions without return type annotation should only have empty returns
                SingleStatement::Return(None)
            }
        }
    }

    /// Generate an expression compatible with the given type
    fn generate_expression_for_type<T: Rng + SeedableRng>(
        &self,
        target_type: &Type,
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
        rng: &mut T,
    ) -> TypeResult<Expression> {
        self.generate_expression_for_type_with_depth(target_type, external_functions, 0, rng)
    }

    /// Generate an expression compatible with the given type with depth limit
    fn generate_expression_for_type_with_depth<T: Rng + SeedableRng>(
        &self,
        target_type: &Type,
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
        depth: usize,
        rng: &mut T,
    ) -> TypeResult<Expression> {
        if depth > 5 {
            // Prevent infinite recursion by generating simple literals
            return match target_type {
                Type::Basic(Class::Basic(BasicType::Number(NumberType::SignedInteger(_)))) => {
                    Some(Expression::generate_random_int_literal(rng))
                }
                Type::Basic(Class::Basic(BasicType::Number(NumberType::FloatingPoint(_)))) => {
                    Some(Expression::generate_random_float_literal(rng))
                }
                Type::Basic(BOOLEAN) => Some(Expression::generate_random_boolean_literal(rng)),
                _ => Some(Expression::generate_random_int_literal(rng)),
            };
        }

        match target_type {
            Type::Basic(Class::Basic(BasicType::Number(number_type))) => {
                match number_type {
                    NumberType::SignedInteger(SignedIntegerType::Byte) => {
                        // Generate small integer expression for Byte (-128 to 127)
                        Some(self.generate_int_expression_with_depth(depth + 1, rng))
                    }
                    NumberType::SignedInteger(SignedIntegerType::Short) => {
                        // Generate medium integer expression for Short
                        Some(self.generate_int_expression_with_depth(depth + 1, rng))
                    }
                    NumberType::SignedInteger(SignedIntegerType::Int) => {
                        // Generate integer expression for Int
                        Some(self.generate_int_expression_with_depth(depth + 1, rng))
                    }
                    NumberType::SignedInteger(SignedIntegerType::Long) => {
                        // Generate large integer expression for Long
                        Some(self.generate_int_expression_with_depth(depth + 1, rng))
                    }
                    NumberType::UnsignedInteger(UnsignedIntegerType::UByte) => {
                        // Generate small positive integer expression for UByte
                        Some(self.generate_int_expression_with_depth(depth + 1, rng))
                    }
                    NumberType::UnsignedInteger(UnsignedIntegerType::UShort) => {
                        // Generate medium positive integer expression for UShort
                        Some(self.generate_int_expression_with_depth(depth + 1, rng))
                    }
                    NumberType::UnsignedInteger(UnsignedIntegerType::UInt) => {
                        // Generate positive integer expression for UInt
                        Some(self.generate_int_expression_with_depth(depth + 1, rng))
                    }
                    NumberType::UnsignedInteger(UnsignedIntegerType::ULong) => {
                        // Generate large positive integer expression for ULong
                        Some(self.generate_int_expression_with_depth(depth + 1, rng))
                    }
                    NumberType::FloatingPoint(FloatingPointType::Float) => {
                        // Generate float expression specifically
                        Some(self.generate_float_expression_with_depth(depth + 1, rng))
                    }
                    NumberType::FloatingPoint(FloatingPointType::Double) => {
                        // Generate double expression specifically
                        Some(self.generate_double_expression(rng))
                    }
                }
            }
            Type::Basic(BOOLEAN) => {
                // Generate boolean expression specifically
                Some(self.generate_boolean_expression(rng))
            }
            Type::Basic(STRING) => {
                // Generate string expression with higher probability for string literals
                let variables: Vec<Variable> = self.variable_types.keys().cloned().collect();

                // 70% chance for string literal, 30% chance for variable reference or function call
                if rng.random_bool(7.0 / 10.0) {
                    Some(Expression::generate_random_string_literal(rng))
                } else {
                    Some(Expression::generate_random_expression(
                        1,
                        external_functions,
                        Some(&variables), // Pass available variables
                        None,
                        rng,
                    ))
                }
            }
            _ => {
                // Default to simple arithmetic expression
                // Convert variable_types keys to Vec<Variable> for external_variables
                let variables: Vec<Variable> = self.variable_types.keys().cloned().collect();
                Some(Expression::generate_random_expression(
                    2,
                    external_functions,
                    Some(&variables), // Pass available variables
                    None,
                    rng,
                ))
            }
        }
    }

    /// Generate a float expression specifically with depth limit
    fn generate_float_expression_with_depth<T: Rng + SeedableRng>(
        &self,
        depth: usize,
        rng: &mut T,
    ) -> Expression {
        // 25% chance for float literal, 15% for float arithmetic, 30% for function call, 30% for variable reference
        match rng.random_range(0..10) {
            0..=2 => {
                // Generate float literal
                Expression::generate_random_float_literal(rng)
            }
            3 => {
                // Generate float arithmetic expression
                let left = ArithmeticExpression::generate_random_float_literal(rng);
                let right = ArithmeticExpression::generate_random_float_literal(rng);
                let op = Operator::generate_random_operator(rng);
                Expression::Arithmetic(ArithmeticExpression::BinaryOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                })
            }
            4..=6 => {
                // Generate variable reference for float type
                let float_vars: Vec<_> = self
                    .variable_types
                    .keys()
                    .filter(|var| var.is_float())
                    .collect();

                if !float_vars.is_empty() {
                    let var = float_vars.choose(rng).unwrap();
                    Expression::VariableReference(var.get_name().to_string())
                } else {
                    // Fallback to float literal if no float variables available
                    Expression::generate_random_float_literal(rng)
                }
            }
            7..=9 => {
                // Generate type-safe function call that returns float
                let float_type = Type::Basic(FLOAT);

                if let Some(expr) = self.generate_type_safe_function_call_expression_with_depth(
                    &float_type,
                    depth + 1,
                    rng,
                ) {
                    expr
                } else {
                    Expression::generate_random_float_literal(rng)
                }
            }
            _ => unreachable!(),
        }
    }

    /// Generate a double expression specifically
    fn generate_double_expression<T: Rng + SeedableRng>(&self, rng: &mut T) -> Expression {
        // 70% chance to generate double literal, 30% chance to generate double arithmetic
        if rng.random_bool(7.0 / 10.0) {
            // Generate double literal (using float for now, but with decimal)
            Expression::generate_random_float_literal(rng)
        } else {
            // Generate double arithmetic expression
            let left = ArithmeticExpression::generate_random_float_literal(rng);
            let right = ArithmeticExpression::generate_random_float_literal(rng);
            let op = Operator::generate_random_operator(rng);
            Expression::Arithmetic(ArithmeticExpression::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            })
        }
    }

    /// Generate an integer expression specifically with depth limit
    fn generate_int_expression_with_depth<T: Rng + SeedableRng>(
        &self,
        depth: usize,
        rng: &mut T,
    ) -> Expression {
        // 25% chance for integer literal, 15% for integer arithmetic, 30% for function call, 30% for variable reference
        match rng.random_range(0..10) {
            0..=2 => {
                // Generate integer literal
                Expression::generate_random_int_literal(rng)
            }
            3 => {
                // Generate integer arithmetic expression
                let left = ArithmeticExpression::generate_random_int_literal(rng);
                let right = ArithmeticExpression::generate_random_int_literal(rng);
                let op = Operator::generate_random_operator(rng);
                Expression::Arithmetic(ArithmeticExpression::BinaryOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                })
            }
            4..=6 => {
                // Generate variable reference for integer type
                let int_vars: Vec<&Variable> = self
                    .variable_types
                    .keys()
                    .filter(|var| var.is_integer())
                    .collect();

                if !int_vars.is_empty() {
                    let var = int_vars.choose(rng).unwrap();
                    Expression::VariableReference(var.get_name().to_string())
                } else {
                    // Fallback to integer literal if no integer variables available
                    Expression::generate_random_int_literal(rng)
                }
            }
            7..=9 => {
                // Generate type-safe function call that returns int
                let int_type = Type::Basic(INT);

                if let Some(expr) = self.generate_type_safe_function_call_expression_with_depth(
                    &int_type,
                    depth + 1,
                    rng,
                ) {
                    expr
                } else {
                    Expression::generate_random_int_literal(rng)
                }
            }
            _ => unreachable!(),
        }
    }

    /// Generate a boolean expression specifically
    fn generate_boolean_expression<T: Rng + SeedableRng>(&self, rng: &mut T) -> Expression {
        // Generate boolean expression with depth 2
        Expression::Boolean(BooleanExpression::generate_random_boolean_expression(
            2, None, None, rng,
        ))
    }
}
