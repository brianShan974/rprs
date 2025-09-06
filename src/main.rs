#[cfg(feature = "jemalloc")]
use jemallocator::Jemalloc;

use rand::{Rng, rng};
use rand::{SeedableRng, rngs::StdRng};
use rayon::prelude::*;

use clap::Parser;
use randprog_rs::config::Args;
use randprog_rs::file::src_file::File;

#[cfg(feature = "jemalloc")]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

pub const SEED_GAP: usize = 42;

fn main() {
    // Parse command line arguments
    let args = Args::parse();

    eprintln!("Starting multi-threaded Kotlin code generator");
    eprintln!("Configuration:");
    eprintln!("Threads: {}", args.thread_count());
    eprintln!("Total files: {}", args.total_tasks());
    if let Some(classes) = args.classes {
        eprintln!("Number of classes: {}", classes);
    } else if let Some(max_classes) = args.max_classes {
        eprintln!("Max classes: {}", max_classes);
    }
    if let Some(functions) = args.functions {
        eprintln!("Number of functions: {}", functions);
    } else if let Some(max_functions) = args.max_functions {
        eprintln!("Max functions: {}", max_functions);
    }
    if let Some(ref output_dir) = args.output_dir {
        eprintln!("Output directory: {}", output_dir);
    }
    eprintln!();

    // Configure rayon thread pool for optimal performance
    rayon::ThreadPoolBuilder::new()
        .num_threads(args.thread_count())
        .build_global()
        .unwrap();

    let seed = args
        .seed
        .unwrap_or_else(|| rng().random_range(0..=usize::MAX));

    // Create all task seeds upfront
    let total_tasks = args.total_tasks();
    let seeds: Vec<_> = (0..total_tasks).map(|i| i * SEED_GAP + seed).collect();

    // Use rayon's work-stealing scheduler for producer-consumer pattern
    // Each thread will steal work from others when it finishes its current task
    let _all_results: Vec<String> = seeds
        .par_iter()
        .map(|&seed| {
            let mut rng = StdRng::seed_from_u64(seed as u64);
            let file = File::generate_type_safe_file(
                &mut rng,
                args.max_constants,
                args.max_classes,
                args.max_functions,
                args.classes,
                args.functions,
            );
            file.to_string()
        })
        .collect();

    eprintln!(
        "Generated {} file{} successfully",
        total_tasks,
        if total_tasks == 1 { "" } else { "s" }
    );
    for result in _all_results {
        println!("{}", result);
    }
}
