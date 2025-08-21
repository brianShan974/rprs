use rand::{SeedableRng, rngs::StdRng};

use randprog_rs::file::src_file::File;

fn main() {
    // Create a shared RNG context
    let mut rng = StdRng::from_os_rng();

    println!("Starting file generation...");

    // Generate a type-safe file
    println!("Generating type-safe file...");
    let type_safe_file = File::generate_type_safe_file(&mut rng);
    println!("Generated Type-Safe File: {}", type_safe_file.get_name());
    println!("{}", type_safe_file);
}
