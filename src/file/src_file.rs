use rand::{Rng, SeedableRng};

use std::borrow::Cow;
use std::cell::RefCell;

use std::fmt::Display;
use std::rc::Rc;

use crate::basic::body::fun::function::Function;
use crate::basic::cls::class::Class;
use crate::basic::cls::custom_class::CustomClass;
use crate::basic::utils::{GenerationConfig, generate_random_identifier};
use crate::basic::var::variable::Variable;
use crate::type_system::TypedGenerationContext;

pub struct File {
    name: String,
    top_level_constants: Vec<Variable>,
    functions: Vec<Function>,
    classes: Vec<CustomClass>,
}

impl File {
    pub const MAX_FUNCTIONS: usize = 15;
    pub const MAX_CLASSES: usize = 10;

    pub fn generate_random_file<T: Rng + SeedableRng>(
        rng: &mut T,
        num_classes: Option<usize>,
        num_functions: Option<usize>,
    ) -> Self {
        // Generate random file name
        let name = Self::generate_random_filename(rng);

        // Generate top-level constants
        let num_constants = rng.random_range(0..=5);
        let mut top_level_constants = Vec::with_capacity(num_constants);

        // For top-level constants, we don't have external variables yet, so we'll use an empty vector
        let external_variables: Vec<Variable> = Vec::new();

        for _ in 0..num_constants {
            let constant = Variable::generate_random_variable(
                false,                     // not a member variable
                true,                      // with initial value
                Some(&external_variables), // Use empty external variables for now
                rng,
            );
            top_level_constants.push(constant);
        }

        // Generate classes FIRST
        let num_classes = num_classes.unwrap_or_else(|| rng.random_range(0..=Self::MAX_CLASSES));
        let mut classes = Vec::with_capacity(num_classes);
        let mut existing_names = Vec::new();

        for _ in 0..num_classes {
            let class = CustomClass::generate_random_custom_class(
                rng,
                None,
                Some(0),
                Some(&mut existing_names),
            );
            classes.push(class);
        }

        // Convert classes to Class enum for expression generation
        let defined_classes: Vec<Class> = classes
            .iter()
            .map(|class| Class::Custom(class.clone()))
            .collect();

        // Generate type-safe functions with access to defined classes
        let num_functions =
            num_functions.unwrap_or_else(|| rng.random_range(1..=Self::MAX_FUNCTIONS));
        let mut functions = Vec::with_capacity(num_functions);
        let external_functions = Rc::new(RefCell::new(Vec::new()));

        for _ in 0..num_functions {
            // Create a new typed context for each function to avoid variable scope pollution
            let mut typed_context = TypedGenerationContext::new(external_functions.clone());
            let mut function_config = GenerationConfig::new(
                Vec::new(),
                external_functions.clone(),
                Some(defined_classes.clone()), // TODO: optimize to use Cow
                0,
                5,
            );

            if let Some(function) = Function::generate_type_safe_function(
                &mut function_config,
                &Vec::new(),
                false,
                &mut typed_context,
                rng,
            ) {
                functions.push(function);
            }
        }

        Self {
            name,
            top_level_constants,
            functions,
            classes,
        }
    }

    /// Generate a type-safe file using typed generation context
    pub fn generate_type_safe_file<T: Rng + SeedableRng>(
        rng: &mut T,
        max_constants: usize,
        max_classes: Option<usize>,
        max_functions: Option<usize>,
        num_classes: Option<usize>,
        num_functions: Option<usize>,
    ) -> Self {
        // Generate random file name
        let name = Self::generate_random_filename(rng);

        // Generate top-level constants
        let num_constants = rng.random_range(0..=max_constants);
        let mut top_level_constants = Vec::with_capacity(num_constants);

        // For top-level constants, we don't have external variables yet, so we'll use an empty vector
        let external_variables: Vec<Variable> = Vec::new();

        for _ in 0..num_constants {
            let constant = Variable::generate_random_variable(
                false,                     // not a member variable
                true,                      // with initial value
                Some(&external_variables), // Use empty external variables for now
                rng,
            );
            top_level_constants.push(constant);
        }

        // Generate type-safe classes and functions
        let num_classes = num_classes
            .unwrap_or_else(|| rng.random_range(1..=max_classes.unwrap_or(Self::MAX_CLASSES)));
        let num_functions = num_functions
            .unwrap_or_else(|| rng.random_range(1..=max_functions.unwrap_or(Self::MAX_FUNCTIONS)));
        let mut classes = Vec::with_capacity(num_classes);
        let mut functions = Vec::with_capacity(num_functions);
        let mut existing_names = Vec::new();

        // Create a shared typed context for all classes
        let shared_external_functions = Rc::new(RefCell::new(Vec::new()));
        let mut shared_typed_context = TypedGenerationContext::new(shared_external_functions);

        // First pass: Generate all class names only
        // TODO: rewrite this with iterators
        let mut class_names = Vec::with_capacity(num_classes);
        for _ in 0..num_classes {
            let class_name = generate_random_identifier(rng);
            existing_names.push(class_name.clone());
            class_names.push(class_name);
        }

        // Second pass: Generate all function names only
        // TODO: rewrite this with iterators
        let mut function_names = Vec::with_capacity(num_functions);
        for _ in 0..num_functions {
            let function_name = generate_random_identifier(rng);
            existing_names.push(function_name.clone());
            function_names.push(function_name);
        }

        // Third pass: Generate class skeletons (properties and method signatures)
        let mut class_skeletons = Vec::with_capacity(num_classes);
        for class_name in &class_names {
            let class_skeleton = CustomClass::generate_class_skeleton_only(
                rng,
                class_name.clone(),
                &mut existing_names,
            );
            class_skeletons.push(class_skeleton);
        }

        // Fourth pass: Generate function signatures only
        let mut function_signatures = Vec::with_capacity(num_functions);
        for function_name in &function_names {
            let function_signature = Function::generate_signature_only_with_classes(
                rng,
                function_name.clone(),
                0,
                &class_skeletons,
            );
            function_signatures.push(function_signature);
        }

        // Fifth pass: Generate full class implementations with access to all skeletons and function signatures
        for class_skeleton in &class_skeletons {
            let class = CustomClass::generate_type_safe_custom_class_with_skeletons_and_functions(
                rng,
                &mut shared_typed_context,
                &class_skeletons,
                &function_signatures,
                class_skeleton,
                None,
                Some(&mut existing_names),
            );
            classes.push(class);
        }

        // Sixth pass: Generate full function implementations with access to all classes and function signatures
        for function_signature in &function_signatures {
            let function = Function::generate_type_safe_function_with_signatures(
                rng,
                &mut shared_typed_context,
                &classes,
                &function_signatures,
                function_signature,
                None,
                Some(&mut existing_names),
            );
            if let Some(func) = function {
                functions.push(func);
            }
        }

        // Convert classes to Class enum for expression generation
        let defined_classes: Vec<Class> = classes
            .iter()
            .map(|class| Class::Custom(class.clone()))
            .collect();

        // Set the defined classes in the shared typed context so classes can access each other
        shared_typed_context.set_defined_classes(defined_classes.clone());

        // Generate type-safe functions with access to defined classes
        let num_functions = rng.random_range(1..=max_functions.unwrap_or(Self::MAX_FUNCTIONS));
        let mut functions = Vec::with_capacity(num_functions);
        let external_functions = Rc::new(RefCell::new(Vec::new()));
        let mut all_variables = Vec::new(); // Collect variables from all functions

        for _ in 0..num_functions {
            // Create a new typed context for each function to avoid variable scope pollution
            let mut typed_context = TypedGenerationContext::new(external_functions.clone());
            // Set the defined classes for variable generation
            typed_context.set_defined_classes(defined_classes.clone());

            // Use Cow to avoid unnecessary cloning
            let all_vars_cow: Cow<[Variable]> = if all_variables.is_empty() {
                Cow::Borrowed(&[])
            } else {
                Cow::Borrowed(&all_variables)
            };

            let mut function_config = GenerationConfig::new(
                all_vars_cow.to_vec(), // Only clone when necessary
                external_functions.clone(),
                Some(defined_classes.clone()), // TODO: optimize to use Cow
                0,
                5,
            )
            .with_existing_names(existing_names.clone());

            if let Some(function) = Function::generate_type_safe_function(
                &mut function_config,
                &Vec::new(),
                false,
                &mut typed_context,
                rng,
            ) {
                existing_names.push(function.get_name().to_string());
                functions.push(function);

                // Extract variables from the generated function and add them to all_variables
                // This allows subsequent functions to access variables from previous functions
                let function_variables = typed_context.get_mutable_variables();
                all_variables.extend(function_variables.iter().map(|v| (*v).clone()));
            }
        }

        Self {
            name,
            top_level_constants,
            functions,
            classes,
        }
    }

    fn generate_random_filename<T: Rng + SeedableRng>(rng: &mut T) -> String {
        format!("{}.kt", generate_random_identifier(rng))
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_functions(&self) -> &[Function] {
        &self.functions
    }

    pub fn get_classes(&self) -> &[CustomClass] {
        &self.classes
    }
}

impl Display for File {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Write top-level constants first
        for constant in self.top_level_constants.iter() {
            if let Some(init) = constant.output_init() {
                writeln!(f, "\n{}", init)?;
            } else {
                writeln!(f, "\n{}", constant.output_declaration())?;
            }
        }

        if !self.classes.is_empty() {
            writeln!(f)?;
        }

        // Write classes
        let mut class_iter = self.classes.iter();
        if let Some(class) = class_iter.next() {
            writeln!(f, "{}", class)?;
        }
        for class in class_iter {
            writeln!(f, "\n{}", class)?;
        }

        if !self.functions.is_empty() {
            writeln!(f)?;
        }

        // Write functions
        let mut function_iter = self.functions.iter();
        if let Some(function) = function_iter.next() {
            writeln!(f, "{}", function)?;
        }
        for function in function_iter {
            writeln!(f, "\n{}", function)?;
        }

        Ok(())
    }
}
