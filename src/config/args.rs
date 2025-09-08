use clap::Parser;

#[derive(Parser, Debug)]
#[command(
    name = "randprog_rs",
    about = "A multi-threaded Kotlin code generator",
    version,
    long_about = "Generates type-safe Kotlin code files using multiple CPU cores for maximum performance."
)]
pub struct Args {
    /// Total number of files to generate, defaults to 1
    #[arg(short = 'n', long)]
    pub total_files: Option<usize>,

    /// Seed for deterministic generation
    #[arg(short = 's', long)]
    pub seed: Option<usize>,

    /// Number of threads to use
    #[arg(short = 't', long)]
    pub threads: Option<usize>,

    /// Output directory for generated files
    #[arg(short, long)]
    pub output_dir: Option<String>,

    /// Maximum number of classes per file
    #[arg(long)]
    pub max_classes: Option<usize>,

    /// Maximum number of functions per file
    #[arg(long)]
    pub max_functions: Option<usize>,

    /// The number of classes per file, overrides max-classes
    #[arg(short, long)]
    pub functions: Option<usize>,

    /// The number of functions per file, overrides max-functions
    #[arg(short, long)]
    pub classes: Option<usize>,

    /// Maximum number of constants per file
    #[arg(long, default_value = "5")]
    pub max_constants: usize,

    /// Outputs the config of the program
    #[arg(short, long)]
    pub verbose: bool,
}

impl Args {
    /// Get the total number of tasks to generate
    pub fn total_tasks(&self) -> usize {
        self.total_files.unwrap_or(1)
    }

    /// Get the number of threads to use
    pub fn thread_count(&self) -> usize {
        self.threads.unwrap_or_else(num_cpus::get)
    }

    /// Validate arguments
    pub fn validate(&self) -> Result<(), String> {
        if let Some(0) = self.total_files {
            return Err("total_files must be greater than 0".to_string());
        }

        if self.max_classes == Some(0) {
            return Err("max_classes must be greater than 0".to_string());
        }

        if self.max_functions == Some(0) {
            return Err("max_functions must be greater than 0".to_string());
        }

        if let Some(threads) = self.threads
            && threads == 0
        {
            return Err("threads must be greater than 0".to_string());
        }

        Ok(())
    }
}
