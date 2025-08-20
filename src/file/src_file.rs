use rand::{Rng, SeedableRng};

use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

use crate::basic::body::fun::function::Function;
use crate::basic::utils::generate_random_identifier;
use crate::type_system::TypedGenerationContext;

pub struct File {
    name: String,
    functions: Vec<Function>,
}

impl File {
    pub const MAX_FUNCTIONS: usize = 5;

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
                Vec::new(),
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

        Self { name, functions }
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
                Vec::new(),
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

        Self { name, functions }
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
}

impl Display for File {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Write functions
        for (i, function) in self.functions.iter().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }
            write!(f, "{}", function)?;
        }

        Ok(())
    }
}
