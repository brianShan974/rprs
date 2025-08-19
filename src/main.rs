use randprog_rs::file::src_file::File;

fn main() {
    println!("=== Random Kotlin File Generation ===\n");

    // Generate multiple random files
    for i in 1..=3 {
        let file = File::generate_random_file();
        println!("File #{}: {}", i, file.get_name());
        println!("{}", file);
        println!("{}", "=".repeat(80));
        println!();
    }
}
