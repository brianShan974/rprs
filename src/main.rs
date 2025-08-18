use randprog_rs::basic::body::fun::function::Function;

fn main() {
    println!("=== Function Generation with Proper Indentation ===\n");

    // Generate multiple functions to test indentation
    for i in 1..=3 {
        let function = Function::generate_random_function(Vec::new(), None, None).unwrap();
        println!("Function #{}:", i);
        println!("{}", function);
        println!();
    }
}
