use rand::{Rng, SeedableRng};

use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

use crate::basic::body::fun::function::Function;
use crate::basic::cls::custom_class::CustomClass;
use crate::basic::utils::generate_random_identifier;
use crate::basic::var::variable::Variable;
use crate::type_system::TypedGenerationContext;

pub struct File {
    name: String,
    top_level_constants: Vec<Variable>,
    functions: Vec<Function>,
    classes: Vec<CustomClass>,
}

impl File {
    pub const MAX_FUNCTIONS: usize = 10;
    pub const MAX_CLASSES: usize = 10;

    pub fn generate_random_file<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        // Generate random file name
        let name = Self::generate_random_filename(rng);

        // Generate top-level constants
        let num_constants = rng.random_range(0..=5);
        let mut top_level_constants = Vec::with_capacity(num_constants);
        for _ in 0..num_constants {
            let constant = Variable::generate_random_variable(
                false, // not a member variable
                true,  // with initial value
                None,  // no external variables for top-level constants
                rng,
            );
            top_level_constants.push(constant);
        }

        // Generate type-safe functions (using the same method as type-safe file)
        let num_functions = rng.random_range(1..=Self::MAX_FUNCTIONS);
        let mut functions = Vec::with_capacity(num_functions);
        let external_functions = Rc::new(RefCell::new(Vec::new()));

        for _ in 0..num_functions {
            // Create a new typed context for each function to avoid variable scope pollution
            let mut typed_context = TypedGenerationContext::new(external_functions.clone());
            if let Some(function) = Function::generate_type_safe_function(
                &Vec::new(),
                external_functions.clone(),
                None,
                None,
                None,
                false,
                &mut typed_context,
                rng,
            ) {
                functions.push(function);
            }
        }

        // Generate classes
        let num_classes = rng.random_range(0..=Self::MAX_CLASSES);
        let mut classes = Vec::with_capacity(num_classes);

        for _ in 0..num_classes {
            let class = CustomClass::generate_random_custom_class(rng, None, None);
            classes.push(class);
        }

        Self {
            name,
            top_level_constants,
            functions,
            classes,
        }
    }

    /// Generate a type-safe file using typed generation context
    pub fn generate_type_safe_file<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        // Generate random file name
        let name = Self::generate_random_filename(rng);

        // Generate top-level constants
        let num_constants = rng.random_range(0..=5);
        let mut top_level_constants = Vec::with_capacity(num_constants);
        for _ in 0..num_constants {
            let constant = Variable::generate_random_variable(
                false, // not a member variable
                true,  // with initial value
                None,  // no external variables for top-level constants
                rng,
            );
            top_level_constants.push(constant);
        }

        // Generate type-safe classes FIRST
        let num_classes = rng.random_range(1..=Self::MAX_CLASSES); // 确保至少生成一个类
        let mut classes = Vec::with_capacity(num_classes);

        for _ in 0..num_classes {
            let mut typed_context = TypedGenerationContext::new(Rc::new(RefCell::new(Vec::new())));
            let class = CustomClass::generate_type_safe_custom_class(rng, &mut typed_context, None);
            classes.push(class);
        }

        // Convert classes to Class enum for expression generation
        let defined_classes: Vec<crate::basic::cls::class::Class> = classes
            .iter()
            .map(|class| crate::basic::cls::class::Class::Custom(class.clone()))
            .collect();

        // Generate type-safe functions with access to defined classes
        let num_functions = rng.random_range(1..=Self::MAX_FUNCTIONS);
        let mut functions = Vec::with_capacity(num_functions);
        let external_functions = Rc::new(RefCell::new(Vec::new()));

        for _ in 0..num_functions {
            // Create a new typed context for each function to avoid variable scope pollution
            let mut typed_context = TypedGenerationContext::new(external_functions.clone());
            if let Some(function) = Function::generate_type_safe_function(
                &Vec::new(),
                external_functions.clone(),
                Some(&defined_classes), // Pass defined classes to function generation
                None,
                None,
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
        let mut constants = self.top_level_constants.iter();
        if let Some(constant) = constants.next() {
            if let Some(init) = constant.output_init() {
                writeln!(f, "{}", init)?;
            } else {
                writeln!(f, "{}", constant.output_declaration())?;
            }
        }
        for constant in constants {
            if let Some(init) = constant.output_init() {
                write!(f, "\n{}", init)?;
            } else {
                write!(f, "\n{}", constant.output_declaration())?;
            }
        }

        // Write classes
        for class in self.classes.iter() {
            write!(f, "\n\n{}", class)?;
        }

        // Write functions
        for function in self.functions.iter() {
            write!(f, "\n\n{}", function)?;
        }

        Ok(())
    }
}
