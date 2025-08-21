use rand::{Rng, SeedableRng};

use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

use crate::basic::body::fun::function::Function;
use crate::basic::cls::custom_class::CustomClass;
use crate::basic::utils::generate_random_identifier;
use crate::type_system::TypedGenerationContext;

pub struct File {
    name: String,
    functions: Vec<Function>,
    classes: Vec<CustomClass>,
}

impl File {
    pub const MAX_FUNCTIONS: usize = 10;
    pub const MAX_CLASSES: usize = 10;

    pub fn generate_random_file<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        // Generate random file name
        let name = Self::generate_random_filename(rng);

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
                Some(3), // Max depth for functions (reduced to avoid complexity)
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
            functions,
            classes,
        }
    }

    /// Generate a type-safe file using typed generation context
    pub fn generate_type_safe_file<T: Rng + SeedableRng>(rng: &mut T) -> Self {
        // Generate random file name
        let name = Self::generate_random_filename(rng);

        // Generate type-safe functions
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
                Some(3), // Max depth for functions (reduced to avoid complexity)
                false,
                &mut typed_context,
                rng,
            ) {
                functions.push(function);
            }
        }

        // Generate type-safe classes
        let num_classes = rng.random_range(0..=Self::MAX_CLASSES);
        let mut classes = Vec::with_capacity(num_classes);

        for _ in 0..num_classes {
            let mut typed_context = TypedGenerationContext::new(external_functions.clone());
            let class = CustomClass::generate_type_safe_custom_class(rng, &mut typed_context, None);
            classes.push(class);
        }

        Self {
            name,
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
        // Write classes first
        for (i, class) in self.classes.iter().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }
            write!(f, "{}", class)?;
        }

        // Write functions
        for (i, function) in self.functions.iter().enumerate() {
            if self.classes.is_empty() && i == 0 {
                // No extra newline if no classes
            } else {
                writeln!(f)?;
            }
            write!(f, "{}", function)?;
        }

        Ok(())
    }
}
