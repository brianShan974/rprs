use rand::{SeedableRng, rngs::StdRng};

use randprog_rs::file::src_file::File;

fn main() {
    // Create a shared RNG context
    let mut rng = StdRng::from_seed([0; 32]);

    // Generate a type-safe file
    let type_safe_file = File::generate_type_safe_file(&mut rng);
    println!("Generated Type-Safe File: {}", type_safe_file.get_name());
    println!("{}", type_safe_file);

    // Generate a regular file for comparison
    let regular_file = File::generate_random_file(&mut rng);
    println!("\nGenerated Regular File: {}", regular_file.get_name());
    println!("{}", regular_file);
}
