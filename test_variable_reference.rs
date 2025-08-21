use rand::{SeedableRng, rngs::StdRng};
use randprog_rs::basic::expr::expression::Expression;
use randprog_rs::basic::var::variable::Variable;
use randprog_rs::basic::var::prefix::var_prefix::VariablePrefix;
use randprog_rs::basic::cls::class::{FLOAT, INT, BOOLEAN};

fn main() {
    let mut rng = StdRng::from_os_rng();
    
    // Create some test variables
    let var1 = Variable::new(
        VariablePrefix::default(),
        "testVar1".to_string(),
        None,
        Some(INT),
    );
    let var2 = Variable::new(
        VariablePrefix::default(),
        "testVar2".to_string(),
        None,
        Some(FLOAT),
    );
    let var3 = Variable::new(
        VariablePrefix::default(),
        "testVar3".to_string(),
        None,
        Some(BOOLEAN),
    );
    
    let external_variables = vec![var1, var2, var3];
    
    println!("Testing variable reference generation:");
    println!("Available variables: testVar1, testVar2, testVar3");
    println!();
    
    // Generate 10 expressions to see if we get variable references
    for i in 0..10 {
        let expr = Expression::generate_random_expression(
            3,
            None,
            Some(&external_variables),
            &mut rng,
        );
        println!("Expression {}: {}", i + 1, expr);
    }
}
