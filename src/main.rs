#[cfg(feature = "jemalloc")]
use jemallocator::Jemalloc;

use rand::{SeedableRng, rngs::StdRng};
use rayon::prelude::*;

use randprog_rs::file::src_file::File;

#[cfg(feature = "jemalloc")]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

pub const SEED_GAP: u64 = 42;
pub const TASKS_PER_CORE: usize = 2;

fn main() {
    // Get the number of CPU cores
    let num_cores = num_cpus::get();

    // Configure rayon thread pool for optimal performance
    rayon::ThreadPoolBuilder::new()
        .num_threads(num_cores)
        .build_global()
        .unwrap();

    // Create seeds for parallel generation
    let seeds: Vec<u64> = (0..num_cores as u64 * TASKS_PER_CORE as u64)
        .map(|i| i * SEED_GAP)
        .collect();

    // Generate files in parallel using rayon
    let _results: Vec<String> = seeds
        .par_iter()
        .map(|&seed| {
            // Create a separate RNG for each thread
            let mut rng = StdRng::seed_from_u64(seed);

            // Generate a type-safe file
            let file = File::generate_type_safe_file(&mut rng);
            file.to_string()
        })
        .collect();
}
