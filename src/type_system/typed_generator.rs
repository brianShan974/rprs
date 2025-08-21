use ordered_float::OrderedFloat;
use rand::{Rng, SeedableRng};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::basic::{
    body::{
        fun::function::Function,
        stmt::{single_statement::SingleStatement, statement::Statement},
    },
    cls::{
        basic_type::BasicType,
        class::Class,
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
};
use crate::type_system::{Type, TypeChecker, TypeError, TypeResult};

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
}

impl TypedGenerationContext {
    pub fn new(external_functions: Rc<RefCell<Vec<Function>>>) -> Self {
        Self {
            type_checker: TypeChecker::new(),
            variable_types: HashMap::new(),
            function_signatures: HashMap::new(),
            external_functions,
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

        Ok(())
    }

    /// Add a function to the context with type tracking
    pub fn add_function(&mut self, func: &Function) -> TypeResult<()> {
        let func_type = self.infer_function_type(func);

        self.function_signatures
            .insert(func.get_name().to_string(), func_type.clone());
        self.type_checker
            .add_function(func.get_name().to_string(), func_type)?;

        Ok(())
    }

    pub fn get_external_functions(&self) -> Rc<RefCell<Vec<Function>>> {
        self.external_functions.clone()
    }

    /// Generate a type-safe variable assignment
    pub fn generate_type_safe_assignment<T: Rng + SeedableRng>(
        &self,
        var: &Variable,
        rng: &mut T,
    ) -> TypeResult<SingleStatement> {
        if let Some(var_type) = self.variable_types.get(var) {
            // Check if variable is mutable by looking it up in external variables
            // For now, generate a compatible expression
            let expr = self.generate_expression_for_type(
                var_type,
                Some(self.external_functions.clone()),
                rng,
            )?;
            Ok(SingleStatement::Assignment(
                var.get_name().to_string(),
                expr,
            ))
        } else {
            Err(TypeError {
                message: format!("Variable '{}' not found in context", var.get_name()),
                location: "typed assignment generation".to_string(),
                expected: None,
                found: None,
            })
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

                    Ok(SingleStatement::FunctionCall(func_name.to_string(), args))
                }
                _ => Err(TypeError {
                    message: format!("'{}' is not a function", func_name),
                    location: "typed function call generation".to_string(),
                    expected: None,
                    found: None,
                }),
            }
        } else {
            Err(TypeError {
                message: format!("Function '{}' not found in context", func_name),
                location: "typed function call generation".to_string(),
                expected: None,
                found: None,
            })
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
                if let Some(func_type) = self.function_signatures.get(func.get_name()) {
                    if let Type::Function(_, return_type) = func_type {
                        // Check if return type is compatible with target type
                        self.type_checker
                            .check_compatibility(return_type, target_type)
                            .is_ok()
                    } else {
                        false
                    }
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
        let function = &compatible_functions[rng.random_range(0..compatible_functions.len())];
        let func_name = function.get_name();

        if let Some(func_type) = self.function_signatures.get(func_name) {
            if let Type::Function(param_types, _) = func_type {
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

                Ok(Expression::Arithmetic(ArithmeticExpression::FunctionCall(
                    func_name.to_string(),
                    args,
                )))
            } else {
                // Fallback
                self.generate_expression_for_type_with_depth(
                    target_type,
                    Some(functions.clone()),
                    depth + 1,
                    rng,
                )
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
                    Ok(ArithmeticExpression::Int(rng.random_range(-100..=100)))
                }
                Type::Basic(Class::Basic(BasicType::Number(NumberType::FloatingPoint(_)))) => Ok(
                    ArithmeticExpression::Float(OrderedFloat::from(rng.random::<f32>() * 100.0)),
                ),
                _ => {
                    // Default to int for other types
                    Ok(ArithmeticExpression::Int(rng.random_range(-100..=100)))
                }
            }
        } else {
            // 40% chance for function call, 30% chance for binary op, 30% chance for literal
            match rng.random_range(0..10) {
                0..=3 => {
                    // Generate type-safe function call
                    match self.generate_type_safe_function_call_expression(target_type, rng) {
                        Ok(Expression::Arithmetic(arith_expr)) => Ok(arith_expr),
                        _ => {
                            // Fallback to literal
                            match target_type {
                                Type::Basic(Class::Basic(BasicType::Number(
                                    NumberType::SignedInteger(_),
                                ))) => Ok(ArithmeticExpression::Int(rng.random_range(-100..=100))),
                                Type::Basic(Class::Basic(BasicType::Number(
                                    NumberType::FloatingPoint(_),
                                ))) => Ok(ArithmeticExpression::Float(OrderedFloat::from(
                                    rng.random::<f32>() * 100.0,
                                ))),
                                _ => Ok(ArithmeticExpression::Int(rng.random_range(-100..=100))),
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
                    Ok(ArithmeticExpression::BinaryOp {
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
                        ))) => Ok(ArithmeticExpression::Int(rng.random_range(-100..=100))),
                        Type::Basic(Class::Basic(BasicType::Number(
                            NumberType::FloatingPoint(_),
                        ))) => Ok(ArithmeticExpression::Float(OrderedFloat::from(
                            rng.random::<f32>() * 100.0,
                        ))),
                        _ => Ok(ArithmeticExpression::Int(rng.random_range(-100..=100))),
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
        let target_type = Some(Class::Basic(BasicType::Number(NumberType::FloatingPoint(
            FloatingPointType::Float,
        ))));

        Variable::generate_random_variable_with_type(is_member, true, target_type, rng)
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
            _ => Ok(()),
        }
    }

    /// Validate a single statement for type safety
    fn validate_single_statement(&mut self, statement: &SingleStatement) -> TypeResult<()> {
        match statement {
            SingleStatement::VariableDeclaration(var) => {
                self.add_variable(var)?;
                Ok(())
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
                    Err(TypeError {
                        message: format!("Variable '{}' not found", var_name),
                        location: "assignment validation".to_string(),
                        expected: None,
                        found: None,
                    })
                }
            }
            SingleStatement::FunctionCall(func_name, args) => {
                if let Some(func_type) = self.function_signatures.get(func_name) {
                    match func_type {
                        Type::Function(param_types, _) => {
                            if args.len() != param_types.len() {
                                return Err(TypeError {
                                    message: format!(
                                        "Function '{}' expects {} arguments, got {}",
                                        func_name,
                                        param_types.len(),
                                        args.len()
                                    ),
                                    location: "function call validation".to_string(),
                                    expected: None,
                                    found: None,
                                });
                            }

                            for (arg, expected_type) in args.iter().zip(param_types.iter()) {
                                let arg_type = self.infer_expression_type(arg)?;
                                self.type_checker
                                    .check_compatibility(&arg_type, expected_type)?;
                            }
                            Ok(())
                        }
                        _ => Err(TypeError {
                            message: format!("'{}' is not a function", func_name),
                            location: "function call validation".to_string(),
                            expected: None,
                            found: None,
                        }),
                    }
                } else {
                    // Function not in context, but might be external - allow for now
                    Ok(())
                }
            }
            SingleStatement::Return(_) => Ok(()),
            SingleStatement::ObjectCreation(_) => Ok(()), // For now, just accept object creation
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
        Ok(var.get_type().cloned())
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
            if let Statement::Single(single_stmt) = statement {
                if let SingleStatement::Return(expr) = single_stmt {
                    return match expr {
                        Some(expr) => {
                            // If there's a return expression, infer its type
                            match self.infer_expression_type(expr) {
                                Ok(ty) => ty,
                                Err(_) => Type::Basic(Class::Basic(BasicType::Number(
                                    NumberType::FloatingPoint(FloatingPointType::Float),
                                ))),
                            }
                        }
                        None => {
                            // Return without value - Unit type
                            Type::Basic(Class::Basic(BasicType::Boolean))
                        }
                    };
                }
            }
        }

        // If no return statement found, assume Unit type
        Type::Basic(Class::Basic(BasicType::Boolean))
    }

    /// Infer the type of an expression
    fn infer_expression_type(&self, _expr: &Expression) -> TypeResult<Type> {
        // For now, assume all expressions are Float (arithmetic expressions)
        Ok(Type::Basic(Class::Basic(BasicType::Number(
            NumberType::FloatingPoint(FloatingPointType::Float),
        ))))
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
                        // Numeric return type - generate return with expression
                        let expr = self
                            .generate_expression_for_type(
                                return_type,
                                Some(self.external_functions.clone()),
                                rng,
                            )
                            .unwrap_or_else(|_| {
                                Expression::generate_random_expression(
                                    2,
                                    Some(self.external_functions.clone()),
                                    rng,
                                )
                            });
                        SingleStatement::Return(Some(expr))
                    }
                    Type::Basic(Class::Basic(BasicType::Boolean)) => {
                        // Boolean return type - generate boolean expression
                        let expr = self
                            .generate_expression_for_type(
                                return_type,
                                Some(self.external_functions.clone()),
                                rng,
                            )
                            .unwrap_or_else(|_| self.generate_boolean_expression(rng));
                        SingleStatement::Return(Some(expr))
                    }
                    _ => {
                        // Other types - default to return without value for safety
                        SingleStatement::Return(None)
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
                Type::Basic(Class::Basic(BasicType::Number(NumberType::SignedInteger(_)))) => Ok(
                    Expression::Arithmetic(ArithmeticExpression::Int(rng.random_range(-100..100))),
                ),
                Type::Basic(Class::Basic(BasicType::Number(NumberType::FloatingPoint(_)))) => {
                    Ok(Expression::Arithmetic(ArithmeticExpression::Float(
                        OrderedFloat::from(rng.random::<f32>() * 100.0),
                    )))
                }
                Type::Basic(Class::Basic(BasicType::Boolean)) => Ok(Expression::Boolean(
                    BooleanExpression::Literal(rng.random()),
                )),
                _ => Ok(Expression::Arithmetic(ArithmeticExpression::Int(
                    rng.random_range(-100..100),
                ))),
            };
        }

        match target_type {
            Type::Basic(Class::Basic(BasicType::Number(number_type))) => {
                match number_type {
                    NumberType::SignedInteger(SignedIntegerType::Byte) => {
                        // Generate small integer expression for Byte (-128 to 127)
                        Ok(self.generate_int_expression_with_depth(depth + 1, rng))
                    }
                    NumberType::SignedInteger(SignedIntegerType::Short) => {
                        // Generate medium integer expression for Short
                        Ok(self.generate_int_expression_with_depth(depth + 1, rng))
                    }
                    NumberType::SignedInteger(SignedIntegerType::Int) => {
                        // Generate integer expression for Int
                        Ok(self.generate_int_expression_with_depth(depth + 1, rng))
                    }
                    NumberType::SignedInteger(SignedIntegerType::Long) => {
                        // Generate large integer expression for Long
                        Ok(self.generate_int_expression_with_depth(depth + 1, rng))
                    }
                    NumberType::UnsignedInteger(UnsignedIntegerType::UByte) => {
                        // Generate small positive integer expression for UByte
                        Ok(self.generate_int_expression_with_depth(depth + 1, rng))
                    }
                    NumberType::UnsignedInteger(UnsignedIntegerType::UShort) => {
                        // Generate medium positive integer expression for UShort
                        Ok(self.generate_int_expression_with_depth(depth + 1, rng))
                    }
                    NumberType::UnsignedInteger(UnsignedIntegerType::UInt) => {
                        // Generate positive integer expression for UInt
                        Ok(self.generate_int_expression_with_depth(depth + 1, rng))
                    }
                    NumberType::UnsignedInteger(UnsignedIntegerType::ULong) => {
                        // Generate large positive integer expression for ULong
                        Ok(self.generate_int_expression_with_depth(depth + 1, rng))
                    }
                    NumberType::FloatingPoint(FloatingPointType::Float) => {
                        // Generate float expression specifically
                        Ok(self.generate_float_expression_with_depth(depth + 1, rng))
                    }
                    NumberType::FloatingPoint(FloatingPointType::Double) => {
                        // Generate double expression specifically
                        Ok(self.generate_double_expression(rng))
                    }
                }
            }
            Type::Basic(Class::Basic(BasicType::Boolean)) => {
                // Generate boolean expression specifically
                Ok(self.generate_boolean_expression(rng))
            }
            Type::Basic(Class::Basic(BasicType::String)) => {
                // For string, generate a simple expression (string literals not implemented yet)
                Ok(Expression::generate_random_expression(
                    1,
                    external_functions,
                    rng,
                ))
            }
            _ => {
                // Default to simple arithmetic expression
                Ok(Expression::generate_random_expression(
                    2,
                    external_functions,
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
        // 40% chance to generate float literal, 25% chance to generate float arithmetic, 35% chance to generate function call
        match rng.random_range(0..10) {
            0..=3 => {
                // Generate float literal
                let float_value = rng.random::<f32>() * 100.0;
                Expression::Arithmetic(ArithmeticExpression::Float(OrderedFloat::from(float_value)))
            }
            4..=5 => {
                // Generate float arithmetic expression
                let left =
                    ArithmeticExpression::Float(OrderedFloat::from(rng.random::<f32>() * 50.0));
                let right =
                    ArithmeticExpression::Float(OrderedFloat::from(rng.random::<f32>() * 50.0));
                let op = Operator::generate_random_operator(rng);
                Expression::Arithmetic(ArithmeticExpression::BinaryOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                })
            }
            6..=9 => {
                // Generate type-safe function call that returns float
                let float_type = Type::Basic(Class::Basic(BasicType::Number(
                    NumberType::FloatingPoint(FloatingPointType::Float),
                )));

                match self.generate_type_safe_function_call_expression_with_depth(
                    &float_type,
                    depth + 1,
                    rng,
                ) {
                    Ok(expr) => expr,
                    Err(_) => {
                        // Fallback to float literal
                        let float_value = rng.random::<f32>() * 100.0;
                        Expression::Arithmetic(ArithmeticExpression::Float(OrderedFloat::from(
                            float_value,
                        )))
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    /// Generate a double expression specifically
    fn generate_double_expression<T: Rng + SeedableRng>(&self, rng: &mut T) -> Expression {
        // 70% chance to generate double literal, 30% chance to generate double arithmetic
        if rng.random_range(0..10) < 7 {
            // Generate double literal (using float for now, but with decimal)
            let double_value = rng.random::<f32>() * 100.0;
            Expression::Arithmetic(ArithmeticExpression::Float(OrderedFloat::from(
                double_value,
            )))
        } else {
            // Generate double arithmetic expression
            let left = ArithmeticExpression::Float(OrderedFloat::from(rng.random::<f32>() * 50.0));
            let right = ArithmeticExpression::Float(OrderedFloat::from(rng.random::<f32>() * 50.0));
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
        // 40% chance to generate integer literal, 20% chance to generate integer arithmetic, 40% chance to generate function call
        match rng.random_range(0..10) {
            0..=3 => {
                // Generate integer literal
                let int_value = rng.random_range(-100..100);
                Expression::Arithmetic(ArithmeticExpression::Int(int_value))
            }
            4..=5 => {
                // Generate integer arithmetic expression
                let left = ArithmeticExpression::Int(rng.random_range(-50..50));
                let right = ArithmeticExpression::Int(rng.random_range(-50..50));
                let op = Operator::generate_random_operator(rng);
                Expression::Arithmetic(ArithmeticExpression::BinaryOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                })
            }
            6..=9 => {
                // Generate type-safe function call that returns int
                let int_type = Type::Basic(Class::Basic(BasicType::Number(
                    NumberType::SignedInteger(SignedIntegerType::Int),
                )));

                match self.generate_type_safe_function_call_expression_with_depth(
                    &int_type,
                    depth + 1,
                    rng,
                ) {
                    Ok(expr) => expr,
                    Err(_) => {
                        // Fallback to integer literal
                        let int_value = rng.random_range(-100..100);
                        Expression::Arithmetic(ArithmeticExpression::Int(int_value))
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    /// Generate a boolean expression specifically
    fn generate_boolean_expression<T: Rng + SeedableRng>(&self, rng: &mut T) -> Expression {
        // Generate boolean expression with depth 2
        Expression::Boolean(BooleanExpression::generate_random_boolean_expression(
            2, rng,
        ))
    }
}
