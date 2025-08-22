use rand::{SeedableRng, rngs::StdRng};

use randprog_rs::file::src_file::File;

fn main() {
    // Create a shared RNG context
    let mut rng = StdRng::from_os_rng();

    // Generate a type-safe file
    let type_safe_file = File::generate_type_safe_file(&mut rng);
    println!("{}", type_safe_file);
}
