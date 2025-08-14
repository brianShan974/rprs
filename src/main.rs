use randprog_rs::basic::body::fun::function::Function;

fn main() {
    let function = Function::generate_random_function(Vec::new());
    println!("{}", function);
}
