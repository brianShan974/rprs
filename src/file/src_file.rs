use crate::basic::body::fun::function::Function;
use rand::Rng;
use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

pub struct File {
    name: String,
    functions: Vec<Function>,
}

impl File {
    pub const MAX_FUNCTIONS: usize = 5;

    pub fn generate_random_file() -> Self {
        let mut rng = rand::rng();

        // Generate random file name
        let name = Self::generate_random_filename();

        // Generate random functions
        let num_functions = rng.random_range(1..=Self::MAX_FUNCTIONS);
        let mut functions = Vec::with_capacity(num_functions);
        let external_functions = Rc::new(RefCell::new(Vec::new()));

        for _ in 0..num_functions {
            if let Some(function) = Function::generate_random_function(
                Vec::new(),
                external_functions.clone(),
                None,
                None,
            ) {
                functions.push(function);
            }
        }

        Self { name, functions }
    }

    fn generate_random_filename() -> String {
        let name = crate::basic::utils::generate_random_identifier();
        format!("{}.kt", name)
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
