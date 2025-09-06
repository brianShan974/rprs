use rand::seq::IndexedRandom;
use rand::{Rng, SeedableRng};

use std::cell::RefCell;
use std::fmt::{self, Display};
use std::rc::Rc;

use super::arithmetic_expression::ArithmeticExpression;
use super::boolean_expression::BooleanExpression;
use crate::basic::body::fun::function::Function;
use crate::basic::body::fun::parameter::Parameter;
use crate::basic::cls::basic_type::BasicType;
use crate::basic::cls::class::Class;
use crate::basic::cls::custom_class::CustomClass;
use crate::basic::cls::number_types::number::NumberType;
use crate::basic::utils::{format_function_call, format_method_call, format_property_access};
use crate::basic::var::prefix::visibility::Visibility;
use crate::basic::var::variable::Variable;
use crate::type_system::typed_generator::TypedGenerationContext;

/// Represents the relationship between the caller and the target class
#[derive(Debug, Clone, PartialEq)]
pub enum CallerRelationship {
    /// Same class - can call all methods (private, protected, internal, public)
    SameClass,
    /// Subclass - can call public and protected methods
    Subclass,
    /// Unrelated class - can only call public methods
    Unrelated,
}

pub const SAFE_CHARS: &[char] = &[
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
    'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '0', '1', '2', '3', '4',
    '5', '6', '7', '8', '9', ' ', '!', '#', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/',
    ':', ';', '<', '=', '>', '?', '@', '[', ']', '^', '_', '|', '~',
];

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expression {
    Arithmetic(ArithmeticExpression),
    Boolean(BooleanExpression),
    StringLiteral(String),
    FunctionCall(String, Vec<Expression>),
    VariableReference(String),
    ClassInstantiation(String),
    PropertyAccess(String, String),
    MethodCall(String, String, Vec<Expression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Arithmetic(arith) => write!(f, "{}", arith),
            Self::Boolean(boolean) => write!(f, "{}", boolean),
            Self::StringLiteral(s) => write!(f, "\"{}\"", s),
            Self::FunctionCall(name, args) => {
                write!(f, "{}", format_function_call(name, args))
            }
            Self::VariableReference(var_name) => write!(f, "{}", var_name),
            Self::ClassInstantiation(class_name) => write!(f, "{}", class_name),
            Self::PropertyAccess(object, property) => {
                write!(f, "{}", format_property_access(object, property))
            }
            Self::MethodCall(object, method, args) => {
                write!(f, "{}", format_method_call(object, method, args))
            }
        }
    }
}

impl Expression {
    /// Determine the relationship between the caller class and the target class
    pub fn determine_caller_relationship(
        current_class_name: Option<&str>,
        target_class: &CustomClass,
        defined_classes: Option<&[Class]>,
    ) -> CallerRelationship {
        let current_class_name = match current_class_name {
            Some(name) => name,
            None => return CallerRelationship::Unrelated,
        };

        // Check if it's the same class
        if current_class_name == target_class.get_name() {
            return CallerRelationship::SameClass;
        }

        // Check if current class is a subclass of target class (including multi-level inheritance)
        if let Some(classes) = defined_classes {
            if Self::is_subclass_of_by_name(current_class_name, &target_class.get_name(), classes) {
                return CallerRelationship::Subclass;
            }
            // Also check if target class is a subclass of current class (reverse relationship)
            // In this case, current class is a superclass of target class
            if Self::is_subclass_of_by_name(&target_class.get_name(), current_class_name, classes) {
                // Current class is the superclass, target class is the subclass
                // Superclass can access subclass's public methods, but not protected/private
                return CallerRelationship::Unrelated;
            }
        }

        CallerRelationship::Unrelated
    }

    /// Helper function to check if a class is a subclass of another class by name (including multi-level inheritance)
    fn is_subclass_of_by_name(
        subclass_name: &str,
        superclass_name: &str,
        defined_classes: &[Class],
    ) -> bool {
        // Find the subclass
        for class in defined_classes.iter() {
            if let Class::Custom(custom_class) = class
                && custom_class.get_name() == subclass_name
            {
                // Check direct parent
                if let Some(parent_class) = custom_class.get_parent_class() {
                    if let Class::Custom(parent_custom_class) = parent_class.as_ref()
                        && parent_custom_class.get_name() == superclass_name
                    {
                        return true;
                    }
                    // Recursively check parent's inheritance chain
                    if Self::is_subclass_of_by_name(
                        &parent_class.get_name(),
                        superclass_name,
                        defined_classes,
                    ) {
                        return true;
                    }
                }
                break;
            }
        }
        false
    }

    pub fn generate_random_expression<T: Rng + SeedableRng>(
        max_depth: usize,
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
        external_variables: Option<&[Variable]>,
        _defined_classes: Option<&[Class]>,
        current_class: Option<&str>, // 当前正在生成的类名
        rng: &mut T,
    ) -> Self {
        // Generate different types of expressions based on probability
        match rng.random_range(0..=9) {
            0..=2 => {
                // Arithmetic expressions (30% probability)
                Self::Arithmetic(ArithmeticExpression::generate_random_expression_untyped(
                    max_depth,
                    external_functions,
                    external_variables,
                    rng,
                ))
            }
            3 => {
                // Boolean expressions (10% probability)
                Self::Boolean(BooleanExpression::generate_random_boolean_expression(
                    max_depth,
                    external_functions,
                    external_variables,
                    rng,
                ))
            }
            4 => {
                // String expressions (10% probability)
                Self::generate_random_string_literal(rng)
            }
            5 => {
                // Function calls (10% probability) - only call global functions, not class methods
                if let Some(functions) = &external_functions {
                    let functions = functions.borrow();
                    // Filter out class methods - only include functions that are not methods of any class
                    let global_functions: Vec<_> = functions
                        .iter()
                        .filter(|func| {
                            // Check if this function is a method of any defined class
                            if let Some(classes) = _defined_classes {
                                !classes.iter().any(|class| {
                                    if let Class::Custom(custom_class) = class {
                                        custom_class
                                            .get_methods()
                                            .iter()
                                            .any(|method| method.get_name() == func.get_name())
                                    } else {
                                        false
                                    }
                                })
                            } else {
                                true // If no classes defined, treat all as global functions
                            }
                        })
                        .collect();

                    if !global_functions.is_empty() {
                        let function = global_functions.choose(rng).unwrap();
                        let args = Self::generate_function_call_args(
                            function.get_parameters(),
                            max_depth.saturating_sub(1),
                            external_functions.clone(),
                            external_variables,
                            rng,
                        );
                        Self::FunctionCall(function.get_name().to_string(), args)
                    } else {
                        Self::generate_random_string_literal(rng)
                    }
                } else {
                    Self::generate_random_string_literal(rng)
                }
            }
            6..9 => {
                // Method calls (40% probability) - use appropriate methods based on caller relationship
                if let Some(variables) = external_variables {
                    let mut available_methods = Vec::new();

                    for variable in variables.iter() {
                        if let Some(Class::Custom(custom_class)) = variable.get_class() {
                            // Determine the relationship between caller and target class
                            let relationship = Self::determine_caller_relationship(
                                current_class,
                                custom_class,
                                _defined_classes,
                            );

                            match relationship {
                                CallerRelationship::SameClass => {
                                    // Same class - can call all methods (private, protected, internal, public)
                                    for method in custom_class.get_methods() {
                                        available_methods
                                            .push((variable.get_name().to_string(), method));
                                    }
                                }
                                CallerRelationship::Subclass => {
                                    // Subclass - can call public and protected methods
                                    for method in custom_class.get_methods() {
                                        let visibility = method.get_visibility();
                                        if visibility.is_public()
                                            || matches!(visibility, Visibility::Protected)
                                        {
                                            available_methods
                                                .push((variable.get_name().to_string(), method));
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
                                            available_methods
                                                .push((variable.get_name().to_string(), method));
                                        }
                                    }
                                }
                            }
                        }
                    }

                    if !available_methods.is_empty() {
                        // Choose a random available method
                        let (object_name, method) = available_methods.choose(rng).unwrap();
                        let args = Self::generate_function_call_args(
                            method.get_parameters(),
                            max_depth.saturating_sub(1),
                            external_functions.clone(),
                            external_variables,
                            rng,
                        );
                        Self::MethodCall(object_name.clone(), method.get_name().to_string(), args)
                    } else {
                        // No available methods, fall back to different expression type
                        Self::generate_random_expression(
                            max_depth.saturating_sub(1),
                            external_functions.clone(),
                            external_variables,
                            _defined_classes,
                            current_class,
                            rng,
                        )
                    }
                } else {
                    Self::generate_random_string_literal(rng)
                }
            }
            9 => {
                // Property access (10% probability) - enhanced with inheritance support
                if let Some(variables) = external_variables {
                    let objects: Vec<_> = variables
                        .iter()
                        .filter(|v| v.get_class().is_some_and(|c| matches!(c, Class::Custom(_))))
                        .collect();
                    if !objects.is_empty() {
                        let object = objects.choose(rng).unwrap();

                        // Find the class of this object to get its real properties
                        let object_class = object.get_class();
                        if let Some(Class::Custom(custom_class)) = object_class {
                            // Get all properties including inherited ones
                            let mut all_properties = Vec::new();

                            // Determine the relationship between caller and target class
                            let relationship = Self::determine_caller_relationship(
                                current_class,
                                custom_class,
                                _defined_classes,
                            );

                            // Add properties based on caller relationship
                            for prop in &custom_class.properties {
                                let visibility = prop.get_prefix().get_visibility();

                                match relationship {
                                    CallerRelationship::SameClass => {
                                        // Same class - can access all properties (private, protected, internal, public)
                                        all_properties.push(prop);
                                    }
                                    CallerRelationship::Subclass => {
                                        // Subclass - can access public and protected properties
                                        if visibility.is_public()
                                            || matches!(visibility, Visibility::Protected)
                                        {
                                            all_properties.push(prop);
                                        }
                                    }
                                    CallerRelationship::Unrelated => {
                                        // Unrelated class - only public properties
                                        if visibility.is_public() {
                                            all_properties.push(prop);
                                        }
                                    }
                                }
                            }

                            // Add inherited properties (simplified - in real implementation we'd traverse the inheritance chain)
                            if let Some(_parent_name) = custom_class.get_parent_class() {
                                // For now, we'll just use the current class properties
                                // In a full implementation, we'd recursively get parent class properties
                            }

                            if !all_properties.is_empty() {
                                let property = all_properties.choose(rng).unwrap();
                                let property_name = property.get_name().to_string();
                                Self::PropertyAccess(object.get_name().to_string(), property_name)
                            } else {
                                Self::generate_random_string_literal(rng)
                            }
                        } else {
                            Self::generate_random_string_literal(rng)
                        }
                    } else {
                        Self::generate_random_string_literal(rng)
                    }
                } else {
                    Self::generate_random_string_literal(rng)
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn is_arithmetic(&self) -> bool {
        matches!(self, Self::Arithmetic(_))
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, Self::Boolean(_))
    }

    pub fn is_function_call(&self) -> bool {
        matches!(self, Self::FunctionCall(_, _))
    }

    /// Generate a random string literal
    pub fn generate_random_string_literal<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        let length = rng.random_range(3..=10);
        let mut chars = Vec::with_capacity(length);
        for _ in 0..length {
            chars.push(SAFE_CHARS.choose(rng).unwrap());
        }
        Self::StringLiteral(chars.into_iter().collect::<String>())
    }

    /// Generate function call arguments based on parameter types
    pub fn generate_function_call_args<T: Rng + SeedableRng>(
        parameters: &[Parameter],
        max_depth: usize,
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
        external_variables: Option<&[Variable]>,
        rng: &mut T,
    ) -> Vec<Self> {
        parameters
            .iter()
            .map(|param| {
                Self::generate_expression_for_type(
                    param.get_type(),
                    max_depth,
                    external_functions.clone(),
                    external_variables,
                    rng,
                )
            })
            .collect()
    }

    /// Check if this expression is primarily an integer type
    pub fn is_int(
        &self,
        external_variables: Option<&[Variable]>,
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
    ) -> bool {
        match self {
            Self::Arithmetic(arith) => arith.is_int(external_variables),
            Self::Boolean(_) => false,
            Self::StringLiteral(_) => false,
            Self::ClassInstantiation(_) => false,
            Self::PropertyAccess(_, _) => false,
            Self::MethodCall(_, _, _) => false,
            Self::FunctionCall(func_name, _) => {
                // For function calls, look up the actual function return type
                if let Some(functions) = external_functions
                    && let Some(func) = functions
                        .borrow()
                        .iter()
                        .find(|f| f.get_name() == func_name)
                    && let Some(return_type) = func.get_return_type()
                {
                    return return_type.is_integer_type();
                }
                false
            }
            Self::VariableReference(var_name) => {
                // Look up the variable type from external_variables
                if let Some(variables) = external_variables
                    && let Some(variable) = variables.iter().find(|v| v.get_name() == var_name)
                    && let Some(var_type) = variable.get_class()
                {
                    return var_type.is_integer_type();
                }
                false
            }
        }
    }

    /// Check if this expression is primarily a float type
    pub fn is_float(
        &self,
        external_variables: Option<&[Variable]>,
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
    ) -> bool {
        match self {
            Self::Arithmetic(arith) => arith.is_float(external_variables),
            Self::Boolean(_) => false,
            Self::StringLiteral(_) => false,
            Self::ClassInstantiation(_) => false,
            Self::PropertyAccess(_, _) => false,
            Self::MethodCall(_, _, _) => false,
            Self::FunctionCall(func_name, _) => {
                // For function calls, look up the actual function return type
                if let Some(functions) = external_functions
                    && let Some(func) = functions
                        .borrow()
                        .iter()
                        .find(|f| f.get_name() == func_name)
                    && let Some(return_type) = func.get_return_type()
                {
                    // Float type if it's a floating point number type
                    return return_type.is_float_type();
                }
                false
            }
            Self::VariableReference(var_name) => {
                // Look up the variable type from external_variables
                if let Some(variables) = external_variables
                    && let Some(variable) = variables.iter().find(|v| v.get_name() == var_name)
                    && let Some(var_type) = variable.get_class()
                {
                    return !var_type.is_integer_type(); // Float if not integer
                }
                false
            }
        }
    }

    /// Generate an expression that matches a specific type
    pub fn generate_expression_for_type<T: Rng + SeedableRng>(
        target_type: &Class,
        max_depth: usize,
        external_functions: Option<Rc<RefCell<Vec<Function>>>>,
        external_variables: Option<&[Variable]>,
        rng: &mut T,
    ) -> Self {
        match target_type {
            Class::Basic(BasicType::Number(NumberType::SignedInteger(_))) => {
                // Generate integer arithmetic expression
                Self::Arithmetic(ArithmeticExpression::generate_typed_expression(
                    max_depth,
                    true,
                    external_functions,
                    external_variables,
                    rng,
                ))
            }
            Class::Basic(BasicType::Number(NumberType::FloatingPoint(_))) => {
                // Generate float arithmetic expression
                Self::Arithmetic(ArithmeticExpression::generate_typed_expression(
                    max_depth,
                    false,
                    external_functions,
                    external_variables,
                    rng,
                ))
            }
            Class::Basic(BasicType::Boolean) => {
                // Generate boolean expression
                Self::Boolean(BooleanExpression::generate_random_boolean_expression(
                    max_depth,
                    external_functions,
                    external_variables,
                    rng,
                ))
            }
            Class::Basic(BasicType::String) => {
                // Generate string expression
                Self::generate_random_string_literal(rng)
            }
            Class::Basic(BasicType::Char) => {
                // Generate char expression as arithmetic expression with char literal
                let char_value = rng.random_range(32..127) as u8 as char;
                Self::Arithmetic(ArithmeticExpression::Char(char_value))
            }
            Class::Custom(custom_class) => {
                // Generate class instantiation for custom classes, considering subclasses
                // First, try to find existing variables of this class or its subclasses
                if let Some(variables) = external_variables {
                    let compatible_variables: Vec<_> = variables
                        .iter()
                        .filter(|v| {
                            if let Some(Class::Custom(var_class)) = v.get_class() {
                                // Check if variable's class is the same or a subclass
                                var_class.get_base_name() == custom_class.get_base_name()
                                    || Self::is_subclass_of(var_class, custom_class)
                            } else {
                                false
                            }
                        })
                        .collect();

                    if !compatible_variables.is_empty() {
                        let chosen_variable = compatible_variables.choose(rng).unwrap();
                        return Self::VariableReference(chosen_variable.get_name().to_string());
                    }
                }

                // If no compatible variables found, generate class instantiation
                if custom_class.get_generic_parameters().is_empty() {
                    // Non-generic class
                    Self::ClassInstantiation(format!("{}()", custom_class.get_name()))
                } else {
                    // Generic class - use concrete types for instantiation
                    let type_args = custom_class
                        .get_generic_parameters()
                        .iter()
                        .map(|_| {
                            // Generate random basic types for type arguments
                            let basic_types = ["String", "Int", "Float", "Boolean"];
                            basic_types.choose(rng).unwrap().to_string()
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    let instantiation =
                        format!("{}<{}>()", custom_class.get_base_name(), type_args);
                    Self::ClassInstantiation(instantiation)
                }
            }
            Class::Generic(generic_type) => {
                // For generic types, use the base type for expression generation
                Self::generate_expression_for_type(
                    generic_type.get_base_type(),
                    max_depth,
                    external_functions,
                    external_variables,
                    rng,
                )
            }
            _ => {
                // Fallback to integer expression for unknown types
                Self::Arithmetic(ArithmeticExpression::generate_typed_expression(
                    max_depth,
                    true,
                    external_functions,
                    external_variables,
                    rng,
                ))
            }
        }
    }

    pub fn generate_random_int_literal<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        Self::Arithmetic(ArithmeticExpression::generate_random_int_literal(rng))
    }

    pub fn generate_random_float_literal<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        Self::Arithmetic(ArithmeticExpression::generate_random_float_literal(rng))
    }

    pub fn generate_random_double_literal<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        Self::Arithmetic(ArithmeticExpression::generate_random_double_literal(rng))
    }

    pub fn generate_random_boolean_literal<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        Self::Boolean(BooleanExpression::generate_random_boolean_literal(rng))
    }

    /// Generate property access with inheritance support
    pub fn generate_inheritance_aware_property_access<T: Rng + SeedableRng>(
        external_variables: Option<&[Variable]>,
        typed_context: &TypedGenerationContext,
        rng: &mut T,
    ) -> Self {
        if let Some(variables) = external_variables {
            let objects: Vec<_> = variables
                .iter()
                .filter(|v| v.get_class().is_some_and(|c| matches!(c, Class::Custom(_))))
                .collect();

            if !objects.is_empty() {
                let object = objects.choose(rng).unwrap();
                let object_class = object.get_class();

                if let Some(Class::Custom(custom_class)) = object_class {
                    // Get accessible members (including inherited) using the type checker
                    let accessible_members =
                        typed_context.get_accessible_members(&custom_class.get_base_name());

                    // Filter for properties only
                    let accessible_properties: Vec<_> = accessible_members
                        .iter()
                        .filter(|(member_type, _, _)| member_type == "property")
                        .collect();

                    if !accessible_properties.is_empty() {
                        let (_, property_name, _) = accessible_properties.choose(rng).unwrap();
                        return Self::PropertyAccess(
                            object.get_name().to_string(),
                            property_name.clone(),
                        );
                    }
                }
            }
        }

        // Fallback to string literal if no accessible properties found
        Self::generate_random_string_literal(rng)
    }

    /// Check if a class is a subclass of another class
    fn is_subclass_of(subclass: &CustomClass, superclass: &CustomClass) -> bool {
        if subclass.get_base_name() == superclass.get_base_name() {
            return true;
        }

        // Check inheritance chain
        if let Some(parent_class) = subclass.get_parent_class()
            && let Class::Custom(parent_custom_class) = parent_class.as_ref()
        {
            if parent_custom_class.get_base_name() == superclass.get_base_name() {
                return true;
            }
            // Recursively check parent's inheritance chain
            return Self::is_subclass_of(parent_custom_class, superclass);
        }
        false
    }
}
