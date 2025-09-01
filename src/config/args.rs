use clap::Parser;

#[derive(Parser, Debug)]
#[command(
    name = "randprog_rs",
    about = "A multi-threaded Kotlin code generator",
    version,
    long_about = "Generates type-safe Kotlin code files using multiple CPU cores for maximum performance."
)]
pub struct Args {
    /// Total number of files to generate
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
    #[arg(long, default_value = "10")]
    pub max_classes: usize,

    /// Maximum number of functions per file
    #[arg(long, default_value = "15")]
    pub max_functions: usize,

    /// Maximum number of constants per file
    #[arg(long, default_value = "5")]
    pub max_constants: usize,
}

impl Args {
    /// Get the total number of tasks to generate
    pub fn total_tasks(&self) -> usize {
        // If total_files is specified, use it directly
        if let Some(total_files) = self.total_files {
            return total_files;
        }

        // Otherwise, return the number of threads
        let num_cores = num_cpus::get();
        let threads = self.threads.unwrap_or(num_cores);
        threads
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

        if self.max_classes == 0 {
            return Err("max_classes must be greater than 0".to_string());
        }

        if self.max_functions == 0 {
            return Err("max_functions must be greater than 0".to_string());
        }

        if let Some(threads) = self.threads {
            if threads == 0 {
                return Err("threads must be greater than 0".to_string());
            }
        }

        Ok(())
    }
}
