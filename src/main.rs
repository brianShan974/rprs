use randprog_rs::basic::body::stmt::{for_statement::ForStatement, statement::Statement};
use randprog_rs::basic::var::variable::Variable;

fn main() {
    println!("=== Kotlin For Loop Random Generator ===\n");

    // Create some initial external variables
    let mut external_variables = Vec::new();
    external_variables.push(Variable::generate_random_variable(true, true)); // var
    external_variables.push(Variable::generate_random_variable(true, true)); // var
    external_variables.push(Variable::generate_random_variable(true, false)); // val

    println!(
        "Initial external variables: {:?}",
        external_variables
            .iter()
            .map(|v| format!("{} ({})", v.get_name(), v.output_declaration()))
            .collect::<Vec<_>>()
    );

    // Generate 3 random for loops
    for i in 1..=3 {
        println!("For Loop #{}:", i);
        let for_stmt = ForStatement::generate_random_for_statement(external_variables.clone(), 0);

        // Check variables before displaying
        println!(
            "External variables before loop {} display: {:?}",
            i,
            external_variables
                .iter()
                .map(|v| format!("{} ({})", v.get_name(), v.output_declaration()))
                .collect::<Vec<_>>()
        );

        println!("{}", for_stmt);

        // Print current external variables after display
        println!(
            "External variables after loop {} display: {:?}",
            i,
            external_variables
                .iter()
                .map(|v| format!("{} ({})", v.get_name(), v.output_declaration()))
                .collect::<Vec<_>>()
        );
        println!();
    }

    println!("=== If Statement Generator ===\n");

    // Generate 3 random if statements
    for i in 1..=3 {
        println!("If Statement #{}:", i);
        let if_stmt = Statement::generate_random_statement(external_variables.clone());

        // Check variables before displaying
        println!(
            "External variables before if {} display: {:?}",
            i,
            external_variables
                .iter()
                .map(|v| format!("{} ({})", v.get_name(), v.output_declaration()))
                .collect::<Vec<_>>()
        );

        println!("{}", if_stmt);

        // Print current external variables after display
        println!(
            "External variables after if {} display: {:?}",
            i,
            external_variables
                .iter()
                .map(|v| format!("{} ({})", v.get_name(), v.output_declaration()))
                .collect::<Vec<_>>()
        );
        println!();
    }
}
