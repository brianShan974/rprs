use rand::prelude::IndexedRandom;
use rand::{Rng, SeedableRng};
use std::cell::RefCell;

use std::fmt;
use std::rc::Rc;

use crate::basic::body::block::{INDENT_SIZE, SPACE};
use crate::basic::body::fun::function::Function;
use crate::basic::body::fun::parameter::Parameter;
use crate::basic::cls::class::{Class, FLOAT};
use crate::basic::cls::generic_type::GenericParameterContext;
use crate::basic::cls::generic_type::GenericTypeParameter;
use crate::basic::utils::{GenerationConfig, generate_unique_identifier};
use crate::basic::var::variable::Variable;
use crate::type_system::TypedGenerationContext;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CustomClass {
    pub name: String,
    pub generic_parameters: Vec<GenericTypeParameter>,
    pub properties: Vec<Rc<Variable>>,
    pub methods: Vec<Function>,
    pub current_indentation_layer: usize,
    pub parent_class: Option<Rc<Class>>, // Parent class for inheritance
    pub is_open: bool,                   // Whether the class is marked as 'open' (can be inherited)
}

impl CustomClass {
    pub const MAX_PROPERTIES: usize = 5;
    pub const MAX_METHODS: usize = 5;

    pub fn new(name: String, current_indentation_layer: usize) -> Self {
        Self {
            name,
            generic_parameters: Vec::new(),
            properties: Vec::new(),
            methods: Vec::new(),
            current_indentation_layer,
            parent_class: None,
            is_open: false, // Default to not open
        }
    }

    pub fn get_name(&self) -> String {
        if self.generic_parameters.is_empty() {
            self.name.clone()
        } else {
            let params = self
                .generic_parameters
                .iter()
                .map(|p| p.get_name())
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}<{}>", self.name, params)
        }
    }

    pub fn add_property(&mut self, property: Variable) {
        self.properties.push(Rc::new(property));
    }

    pub fn add_method(&mut self, method: Function) {
        self.methods.push(method);
    }

    pub fn get_methods(&self) -> &[Function] {
        &self.methods
    }

    /// Get only public methods from this class
    pub fn get_public_methods(&self) -> Vec<&Function> {
        self.methods
            .iter()
            .filter(|method| method.get_visibility().is_public())
            .collect()
    }

    /// Get public and protected methods from this class (for subclasses)
    pub fn get_public_and_protected_methods(&self) -> Vec<&Function> {
        self.methods
            .iter()
            .filter(|method| {
                let visibility = method.get_visibility();
                visibility.is_public()
                    || matches!(
                        visibility,
                        crate::basic::var::prefix::visibility::Visibility::Protected
                    )
            })
            .collect()
    }

    /// Get public and internal methods from this class (for same module)
    pub fn get_public_and_internal_methods(&self) -> Vec<&Function> {
        self.methods
            .iter()
            .filter(|method| {
                let visibility = method.get_visibility();
                visibility.is_public()
                    || matches!(
                        visibility,
                        crate::basic::var::prefix::visibility::Visibility::Internal
                    )
            })
            .collect()
    }

    pub fn get_generic_parameters(&self) -> &[GenericTypeParameter] {
        &self.generic_parameters
    }

    pub fn get_base_name(&self) -> String {
        self.name.clone()
    }

    /// Get the parent class if this class inherits from another class
    pub fn get_parent_class(&self) -> Option<&Rc<Class>> {
        self.parent_class.as_ref()
    }

    /// Set the parent class for inheritance
    pub fn set_parent_class(&mut self, parent_class: Rc<Class>) {
        self.parent_class = Some(parent_class);
    }

    /// Set whether the class is open (can be inherited)
    pub fn set_open(&mut self, is_open: bool) {
        self.is_open = is_open;
    }

    /// Check if the class is open (can be inherited)
    pub fn is_open(&self) -> bool {
        self.is_open
    }

    /// Check if this class inherits from another class
    pub fn has_parent(&self) -> bool {
        self.parent_class.is_some()
    }

    /// Check if this class can inherit from the given parent class
    /// Rules:
    /// 1. Parent class must be marked as 'open'
    /// 2. Generic classes can only inherit from generic classes with the same type parameters
    /// 3. Non-generic classes can inherit from non-generic classes or partially specialized generic classes
    pub fn can_inherit_from(&self, parent_class: &Class) -> bool {
        match parent_class {
            Class::Custom(parent_custom_class) => {
                // First check if parent class is open
                if !parent_custom_class.is_open() {
                    return false; // Cannot inherit from non-open class
                }

                // Check if parent is generic
                if !parent_custom_class.generic_parameters.is_empty() {
                    // Parent is generic - child must also be generic with same parameters
                    if self.generic_parameters.is_empty() {
                        return false; // Non-generic class cannot inherit from generic class
                    }

                    // Check if generic parameters match
                    if self.generic_parameters.len() != parent_custom_class.generic_parameters.len()
                    {
                        return false;
                    }

                    // Check if parameter names match (they should be identical)
                    for (child_param, parent_param) in self
                        .generic_parameters
                        .iter()
                        .zip(parent_custom_class.generic_parameters.iter())
                    {
                        if child_param.get_name() != parent_param.get_name() {
                            return false;
                        }
                    }
                }
                // If parent is non-generic and open, any class can inherit from it
                true
            }
            _ => false, // Can only inherit from custom classes
        }
    }

    pub fn generate_random_custom_class<T: Rng + SeedableRng>(
        rng: &mut T,
        defined_classes: Option<&mut Vec<Class>>,
        current_indentation_layer: Option<usize>,
        existing_names: Option<&mut Vec<String>>,
    ) -> Self {
        let mut existing_names = existing_names.unwrap_or(&mut Vec::new()).clone();
        let name = generate_unique_identifier(rng, &mut existing_names);
        let current_indentation_layer = current_indentation_layer.unwrap_or(0);
        let mut custom_class = Self::new(name.clone(), current_indentation_layer);

        // Randomly decide if the class should be open (30% chance)
        // Open classes can be inherited from
        custom_class.set_open(rng.random_bool(0.3));

        // Determine class purpose based on name and properties
        let class_purpose = Self::infer_class_purpose(&name, rng);

        // Create context for smart generic parameter generation
        let available_types = defined_classes
            .as_ref()
            .map(|classes| classes.to_vec())
            .unwrap_or_default();

        let context = GenericParameterContext::new(class_purpose.clone(), available_types);

        // Generate inheritance relationship (30% chance)
        if let Some(classes) = defined_classes.as_ref()
            && !classes.is_empty()
            && rng.random_bool(0.3)
        {
            // 30% chance to inherit from an existing class
            let available_parents: Vec<_> = classes
                .iter()
                .filter(|class| {
                    if let Class::Custom(existing_class) = class {
                        existing_class.get_base_name() != custom_class.get_base_name()
                            && custom_class.can_inherit_from(class)
                    } else {
                        false
                    }
                })
                .cloned()
                .collect();

            if !available_parents.is_empty() {
                let parent_class = Rc::new(available_parents.choose(rng).unwrap().clone());
                custom_class.set_parent_class(parent_class);
            }
        }

        // Generate 0-3 generic parameters with improved logic
        let num_generic_params = if rng.random_bool(0.3) {
            // 30% chance for generic class
            rng.random_range(1..=3)
        } else {
            0
        };

        for _i in 0..num_generic_params {
            // Use smart generation for better constraints
            let param = if rng.random_bool(0.7) {
                // 70% chance to use smart generation
                GenericTypeParameter::generate_smart_generic_parameter(
                    rng,
                    Some(&mut existing_names),
                    &context,
                )
            } else {
                GenericTypeParameter::generate_random_generic_parameter(
                    rng,
                    Some(&mut existing_names),
                )
            };

            custom_class.generic_parameters.push(param);
        }

        // Generate 1-4 properties
        let num_properties = rng.random_range(1..=Self::MAX_PROPERTIES);

        // Build external variables for property generation
        let external_variables: Vec<Variable> = Vec::new();

        for _ in 0..num_properties {
            custom_class.add_property(Variable::generate_random_variable_with_const_control(
                true,
                true,
                Some(&external_variables),
                false,
                rng,
            ));
        }

        // Generate 1-3 methods using type-safe function generation
        let num_methods = rng.random_range(1..=Self::MAX_METHODS);
        for _ in 0..num_methods {
            // Convert class properties to parameters for method generation
            let external_variables: Vec<_> = custom_class
                .properties
                .iter()
                .map(|var| {
                    Parameter::new(
                        var.get_name().to_string(),
                        var.get_class()
                            .map(|c| Rc::new(c.clone()))
                            .unwrap_or_else(|| Rc::new(FLOAT.clone())),
                    )
                })
                .collect();

            // Create a typed context for method generation
            let external_functions = Rc::new(RefCell::new(Vec::new()));
            let mut typed_context = TypedGenerationContext::new(external_functions);

            // Generate a type-safe method
            let mut method_config = GenerationConfig::new(
                external_variables
                    .iter()
                    .map(|p| p.clone().into())
                    .collect(),
                typed_context.get_external_functions(),
                None, // No defined classes for method generation in this context
                custom_class.current_indentation_layer + 1,
                5, // Max depth for class methods
            );

            if let Some(method) = Function::generate_type_safe_function(
                &mut method_config,
                &external_variables,
                true, // Is method
                &mut typed_context,
                rng,
            ) {
                custom_class.add_method(method);
            }
        }

        if let Some(defined_classes) = defined_classes {
            defined_classes.push(Class::Custom(custom_class.clone()));
        }

        custom_class
    }

    /// Infer the purpose of a class based on its name and characteristics
    fn infer_class_purpose<T: Rng + SeedableRng>(name: &str, rng: &mut T) -> String {
        let name_lower = name.to_lowercase();

        // Check for common naming patterns
        if name_lower.contains("list")
            || name_lower.contains("array")
            || name_lower.contains("collection")
        {
            "collection".to_string()
        } else if name_lower.contains("map")
            || name_lower.contains("dict")
            || name_lower.contains("hash")
        {
            "map".to_string()
        } else if name_lower.contains("set") || name_lower.contains("unique") {
            "set".to_string()
        } else if name_lower.contains("queue") || name_lower.contains("stack") {
            "collection".to_string()
        } else if name_lower.contains("calc")
            || name_lower.contains("math")
            || name_lower.contains("number")
        {
            "numeric".to_string()
        } else if name_lower.contains("text")
            || name_lower.contains("string")
            || name_lower.contains("format")
        {
            "text".to_string()
        } else if name_lower.contains("sort")
            || name_lower.contains("compare")
            || name_lower.contains("order")
        {
            "comparison".to_string()
        } else if name_lower.contains("factory")
            || name_lower.contains("builder")
            || name_lower.contains("create")
        {
            "producer".to_string()
        } else if name_lower.contains("processor")
            || name_lower.contains("handler")
            || name_lower.contains("consumer")
        {
            "consumer".to_string()
        } else if name_lower.contains("mutable")
            || name_lower.contains("state")
            || name_lower.contains("var")
        {
            "mutable".to_string()
        } else {
            // Random purpose for generic classes
            let purposes = [
                "collection",
                "numeric",
                "text",
                "comparison",
                "producer",
                "consumer",
                "mutable",
            ];
            purposes[rng.random_range(0..purposes.len())].to_string()
        }
    }

    /// Generate class skeleton (properties and method signatures) without full implementation
    pub fn generate_class_skeleton_only<T: Rng + SeedableRng>(
        rng: &mut T,
        name: String,
        existing_names: &mut Vec<String>,
    ) -> Self {
        let current_indentation_layer = 0;
        let mut custom_class = Self::new(name, current_indentation_layer);

        // Generate 0-3 generic parameters (20% chance for each)
        let mut existing_generic_names = Vec::new();
        for _ in 0..3 {
            if rng.random_bool(0.2) {
                let param = GenericTypeParameter::generate_random_generic_parameter(
                    rng,
                    Some(&mut existing_generic_names),
                );
                custom_class.generic_parameters.push(param);
            }
        }

        // Generate 1-4 properties (only types, no initial values)
        let num_properties = rng.random_range(1..=Self::MAX_PROPERTIES);
        for _ in 0..num_properties {
            let property_name = generate_unique_identifier(rng, existing_names);
            // For skeleton generation, we don't have access to defined classes yet
            // Use empty vector as placeholder
            let mut empty_classes: Vec<Class> = Vec::new();
            let property_type = Class::generate_random_class(rng, Some(&mut empty_classes), None);
            let property = Variable::new_with_type_only(property_name, property_type);
            custom_class.add_property(property);
        }

        // Generate method signatures only (without full implementation)
        let num_methods = rng.random_range(1..=Self::MAX_METHODS);
        for _ in 0..num_methods {
            let method_name = generate_unique_identifier(rng, existing_names);
            let method =
                Function::generate_signature_only(rng, method_name, current_indentation_layer + 1);
            custom_class.methods.push(method);
        }

        custom_class
    }

    /// Generate a type-safe custom class with access to all class skeletons
    pub fn generate_type_safe_custom_class_with_skeletons<T: Rng + SeedableRng>(
        rng: &mut T,
        typed_context: &mut TypedGenerationContext,
        class_skeletons: &[Self],
        current_skeleton: &Self,
        current_indentation_layer: Option<usize>,
        _existing_names: Option<&mut Vec<String>>,
    ) -> Self {
        let current_indentation_layer = current_indentation_layer.unwrap_or(0);

        // Start with the current skeleton
        let mut custom_class = current_skeleton.clone();
        custom_class.current_indentation_layer = current_indentation_layer;

        // Update properties with initial values while preserving original property information
        let mut updated_properties = Vec::new();

        // Build external variables for property generation (similar to method generation)
        let external_variables: Vec<Variable> = custom_class
            .properties
            .iter()
            .map(|var| {
                Variable::new_with_type_only(
                    var.get_name().to_string(),
                    var.get_class().unwrap_or(&FLOAT).clone(),
                )
            })
            .collect();

        for _property in &custom_class.properties {
            // Create a new variable with the same name and type but with an initial value
            let updated_property = Variable::generate_random_variable_with_const_control(
                true,                      // is_member
                true,                      // with_initial_value
                Some(&external_variables), // Use built external variables
                false,                     // allow_const
                rng,
            );
            updated_properties.push(Rc::new(updated_property));
        }
        custom_class.properties = updated_properties;

        // Get the number of methods from the skeleton
        let skeleton_method_count = custom_class.methods.len();

        // Clear existing methods (which have empty bodies from skeleton generation)
        custom_class.methods.clear();

        // Generate methods with full implementation for all skeleton methods
        let num_methods = skeleton_method_count.max(1); // At least 1 method
        for _ in 0..num_methods {
            // Convert class properties to parameters for method generation
            let external_variables: Vec<_> = custom_class
                .properties
                .iter()
                .map(|var| {
                    Parameter::new(
                        var.get_name().to_string(),
                        var.get_class()
                            .map(|c| Rc::new(c.clone()))
                            .unwrap_or_else(|| Rc::new(FLOAT.clone())),
                    )
                })
                .collect();

            // Create a separate typed context for each method to avoid recursion
            let mut method_typed_context =
                TypedGenerationContext::new(typed_context.get_external_functions());

            // Convert class skeletons to Class enum for the method context
            let defined_classes: Vec<Class> = class_skeletons
                .iter()
                .map(|c| Class::Custom(c.clone()))
                .collect();
            method_typed_context.set_defined_classes(defined_classes);

            // Also add the current class to the defined classes if it's not already there
            let current_class = Class::Custom(custom_class.clone());
            if !method_typed_context
                .get_defined_classes()
                .iter()
                .any(|c| c.get_name() == current_class.get_name())
            {
                let mut all_classes = method_typed_context.get_defined_classes().to_vec();
                all_classes.push(Rc::new(current_class));
                method_typed_context.set_defined_classes(
                    all_classes.iter().map(|rc| rc.as_ref().clone()).collect(),
                );
            }

            // Generate a type-safe method with explicit return type
            if let Some(method) = Self::generate_method_with_return_type(
                &external_variables,
                &mut method_typed_context,
                custom_class.current_indentation_layer,
                rng,
                Some(&custom_class.generic_parameters),
            ) {
                // Don't add methods to global context to prevent direct calls from other classes
                // typed_context.add_function(&method);
                custom_class.add_method(method);
            }
        }

        custom_class
    }

    /// Generate a type-safe custom class with access to all class skeletons and function signatures
    pub fn generate_type_safe_custom_class_with_skeletons_and_functions<T: Rng + SeedableRng>(
        rng: &mut T,
        typed_context: &mut TypedGenerationContext,
        class_skeletons: &[Self],
        function_signatures: &[Function],
        current_skeleton: &Self,
        current_indentation_layer: Option<usize>,
        _existing_names: Option<&mut Vec<String>>,
    ) -> Self {
        let current_indentation_layer = current_indentation_layer.unwrap_or(0);

        // Start with the current skeleton
        let mut custom_class = current_skeleton.clone();
        custom_class.current_indentation_layer = current_indentation_layer;

        // Ensure the class has the correct open status (30% chance)
        custom_class.set_open(rng.random_bool(0.3));

        // Generate inheritance relationship (30% chance) if not already set
        if !custom_class.has_parent() && !class_skeletons.is_empty() && rng.random_bool(0.3) {
            // 30% chance to inherit from an existing class skeleton
            let available_parents: Vec<_> = class_skeletons
                .iter()
                .filter(|skeleton| {
                    skeleton.get_name() != custom_class.get_name()
                        && custom_class.can_inherit_from(&Class::Custom((*skeleton).clone()))
                })
                .map(|skeleton| Rc::new(Class::Custom(skeleton.clone())))
                .collect();

            if !available_parents.is_empty() {
                let parent_class = available_parents.choose(rng).unwrap().clone();
                custom_class.set_parent_class(parent_class);
            }
        }

        // Update properties with initial values while preserving original property information
        let mut updated_properties = Vec::new();

        // Build external variables for property generation (similar to method generation)
        let external_variables: Vec<Variable> = custom_class
            .properties
            .iter()
            .map(|var| {
                Variable::new_with_type_only(
                    var.get_name().to_string(),
                    var.get_class().unwrap_or(&FLOAT).clone(),
                )
            })
            .collect();

        for _property in &custom_class.properties {
            // Create a new variable with the same name and type but with an initial value
            let updated_property = Variable::generate_random_variable_with_const_control(
                true,                      // is_member
                true,                      // with_initial_value
                Some(&external_variables), // Use built external variables
                false,                     // allow_const
                rng,
            );
            updated_properties.push(Rc::new(updated_property));
        }
        custom_class.properties = updated_properties;

        // Get the number of methods from the skeleton
        let skeleton_method_count = custom_class.methods.len();

        // Clear existing methods (which have empty bodies from skeleton generation)
        custom_class.methods.clear();

        // Generate methods with full implementation for all skeleton methods
        let num_methods = skeleton_method_count.max(1); // At least 1 method
        for _ in 0..num_methods {
            // Convert class properties to parameters for method generation
            let external_variables: Vec<_> = custom_class
                .properties
                .iter()
                .map(|var| {
                    Parameter::new(
                        var.get_name().to_string(),
                        var.get_class()
                            .map(|c| Rc::new(c.clone()))
                            .unwrap_or_else(|| Rc::new(FLOAT.clone())),
                    )
                })
                .collect();

            // Create a separate typed context for each method to avoid recursion
            let mut method_typed_context =
                TypedGenerationContext::new(typed_context.get_external_functions());

            // Convert class skeletons to Class enum for the method context
            let defined_classes: Vec<Class> = class_skeletons
                .iter()
                .map(|c| Class::Custom(c.clone()))
                .collect();
            method_typed_context.set_defined_classes(defined_classes);

            // Add function signatures to the typed context so methods can call functions
            for function_signature in function_signatures {
                method_typed_context.add_function(function_signature);
            }

            // Also add the current class to the defined classes if it's not already there
            let current_class = Class::Custom(custom_class.clone());
            if !method_typed_context
                .get_defined_classes()
                .iter()
                .any(|c| c.get_name() == current_class.get_name())
            {
                let mut all_classes = method_typed_context.get_defined_classes().to_vec();
                all_classes.push(Rc::new(current_class));
                method_typed_context.set_defined_classes(
                    all_classes.iter().map(|rc| rc.as_ref().clone()).collect(),
                );
            }

            // Generate a type-safe method with explicit return type
            if let Some(method) = Self::generate_method_with_return_type(
                &external_variables,
                &mut method_typed_context,
                custom_class.current_indentation_layer,
                rng,
                Some(&custom_class.generic_parameters),
            ) {
                // Don't add methods to global context to prevent direct calls from other classes
                // typed_context.add_function(&method);
                custom_class.add_method(method);
            }
        }

        custom_class
    }

    /// Generate a method with explicit return type
    fn generate_method_with_return_type<T: Rng + SeedableRng>(
        external_variables: &[Parameter],
        typed_context: &mut TypedGenerationContext,
        current_indentation_layer: usize,
        rng: &mut T,
        generic_parameters: Option<&[GenericTypeParameter]>,
    ) -> Option<Function> {
        // Generate a type-safe method
        // Function::generate_type_safe_function will decide the return type internally
        let mut method_config = GenerationConfig::new(
            external_variables
                .iter()
                .map(|p| p.clone().into())
                .collect(),
            typed_context.get_external_functions(),
            Some(
                typed_context
                    .get_defined_classes()
                    .iter()
                    .map(|rc| rc.as_ref().clone())
                    .collect(),
            ), // Pass defined classes for method generation
            current_indentation_layer + 1,
            5, // Max depth for class methods
        )
        .with_generic_parameters(
            generic_parameters
                .map(|params| params.to_vec())
                .unwrap_or_default(),
        );

        // Ensure the typed_context has the defined_classes set
        if typed_context.get_defined_classes().is_empty() {
            // If empty, we need to set it from the config
            if let Some(classes) = &method_config.defined_classes {
                typed_context.set_defined_classes(classes.clone());
            }
        }

        Function::generate_type_safe_function(
            &mut method_config,
            external_variables,
            true, // Is method
            typed_context,
            rng,
        )
    }
}

impl fmt::Display for CustomClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Calculate indentation for class level
        let outer_indentation = SPACE.repeat(self.current_indentation_layer * INDENT_SIZE);
        let inner_indentation = SPACE.repeat(INDENT_SIZE);

        // Start class declaration with generic parameters and inheritance
        let inheritance_part = if let Some(parent) = &self.parent_class {
            format!(" : {}", parent)
        } else {
            String::new()
        };

        // Add 'open' modifier if the class is open
        let open_modifier = if self.is_open { "open " } else { "" };

        if self.generic_parameters.is_empty() {
            writeln!(
                f,
                "{}{}class {}{} {{",
                outer_indentation, open_modifier, self.name, inheritance_part
            )?;
        } else {
            let params = self
                .generic_parameters
                .iter()
                .map(|p| p.get_name())
                .collect::<Vec<_>>()
                .join(", ");
            writeln!(
                f,
                "{}{}class {}<{}>{} {{",
                outer_indentation, open_modifier, self.name, params, inheritance_part
            )?;
        }

        for property in &self.properties {
            writeln!(
                f,
                "{outer_indentation}{inner_indentation}{} {}: {} = {}",
                property.get_prefix(),
                property.get_name(),
                property
                    .get_class()
                    .map(|c| c.get_name())
                    .unwrap_or_else(|| "Unknown".to_string()),
                property
                    .get_value()
                    .map(|v| v.to_string())
                    .unwrap_or_else(|| "TODO_INITIALIZE".to_string())
            )?;
        }

        for method in &self.methods {
            writeln!(f, "\n{}", method)?;
        }

        write!(f, "{}}}", outer_indentation)?;

        Ok(())
    }
}
