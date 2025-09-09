use rand::SeedableRng;
use rand::rngs::StdRng;

use crate::basic::cls::basic_type::BasicType;
use crate::basic::cls::class::Class;
use crate::basic::cls::generic_type::{
    GenericType, GenericTypeContext, GenericTypeParameter, TypeBound, Variance,
};
use crate::basic::cls::number_types::number::NumberType;
use crate::basic::cls::number_types::signed_integer::SignedIntegerType;

#[test]
fn test_generic_type_parameter_creation() {
    let param = GenericTypeParameter::new("T".to_string());
    assert_eq!(param.name, "T");
    assert!(param.bounds.is_empty());
    assert!(matches!(param.variance, Variance::Invariant));
}

#[test]
fn test_generic_type_parameter_with_bounds() {
    let int_type = Class::Basic(BasicType::Number(NumberType::SignedInteger(
        SignedIntegerType::Int,
    )));
    let bounds = vec![TypeBound::Class(int_type.clone())];
    let param = GenericTypeParameter::with_bounds("K".to_string(), bounds.clone());

    assert_eq!(param.name, "K");
    assert_eq!(param.bounds.len(), 1);
    assert_eq!(param.bounds, bounds);
}

#[test]
fn test_generic_type_parameter_with_variance() {
    let param = GenericTypeParameter::with_variance("V".to_string(), Variance::Covariant);
    assert_eq!(param.name, "V");
    assert!(param.bounds.is_empty());
    assert!(matches!(param.variance, Variance::Covariant));
}

#[test]
fn test_type_constraint_validation() {
    let int_type = Class::Basic(BasicType::Number(NumberType::SignedInteger(
        SignedIntegerType::Int,
    )));
    let string_type = Class::Basic(BasicType::String);

    let param = GenericTypeParameter::with_bounds(
        "T".to_string(),
        vec![TypeBound::Class(int_type.clone())],
    );

    // int_type should satisfy the constraint
    assert!(param.validate_type_constraints(&int_type));

    // string_type should not satisfy the constraint
    assert!(!param.validate_type_constraints(&string_type));
}

#[test]
fn test_generic_type_creation() {
    let base_type = Class::Basic(BasicType::String);
    let type_args = vec![
        Class::Basic(BasicType::Number(NumberType::SignedInteger(
            SignedIntegerType::Int,
        ))),
        Class::Basic(BasicType::Boolean),
    ];

    let generic_type = GenericType::new(base_type.clone(), type_args.clone());

    assert_eq!(*generic_type.get_base_type(), base_type);
    assert_eq!(generic_type.get_type_arguments(), type_args.as_slice());
}

#[test]
fn test_generic_type_instantiation() {
    let base_type = Class::Basic(BasicType::String);
    let type_args = vec![
        Class::Basic(BasicType::Number(NumberType::SignedInteger(
            SignedIntegerType::Int,
        ))),
        Class::Basic(BasicType::Boolean),
    ];

    let generic_type = GenericType::new(base_type.clone(), type_args.clone());

    // Create generic parameters
    let generic_params = vec![
        GenericTypeParameter::new("T".to_string()),
        GenericTypeParameter::new("K".to_string()),
    ];

    // Instantiate with concrete types
    let result = generic_type.instantiate_with_types(&generic_params, &type_args);
    assert!(result.is_ok());

    let instantiated = result.unwrap();
    match instantiated {
        Class::Generic(instantiated_generic) => {
            assert_eq!(*instantiated_generic.get_base_type(), base_type);
            assert_eq!(
                instantiated_generic.get_type_arguments(),
                type_args.as_slice()
            );
        }
        _ => panic!("Expected generic type"),
    }
}

#[test]
fn test_generic_type_context() {
    let generic_params = vec![
        GenericTypeParameter::new("T".to_string()),
        GenericTypeParameter::new("K".to_string()),
    ];

    let available_types = vec![
        Class::Basic(BasicType::String),
        Class::Basic(BasicType::Number(NumberType::SignedInteger(
            SignedIntegerType::Int,
        ))),
        Class::Basic(BasicType::Boolean),
    ];

    let mut context = GenericTypeContext::new(generic_params.clone(), available_types.clone());

    // Add a type mapping
    context.add_type_mapping("T", Class::Basic(BasicType::String));

    // Test type inference
    let inferred_type = context.infer_type_for_parameter(&generic_params[0]);
    assert!(inferred_type.is_some());
    assert_eq!(inferred_type.unwrap(), Class::Basic(BasicType::String));

    // Test compatible types
    let compatible_types = context.get_compatible_types(&generic_params[1]);
    assert!(!compatible_types.is_empty());
}

#[test]
fn test_generic_type_arity() {
    let base_type = Class::Basic(BasicType::String);
    let type_args = vec![
        Class::Basic(BasicType::Number(NumberType::SignedInteger(
            SignedIntegerType::Int,
        ))),
        Class::Basic(BasicType::Boolean),
        Class::Basic(BasicType::Char),
    ];

    let generic_type = GenericType::new(base_type, type_args);
    assert_eq!(generic_type.get_arity(), 3);
}

#[test]
fn test_generic_type_constraint_checking() {
    let base_type = Class::Basic(BasicType::String);
    let type_args = vec![
        Class::Basic(BasicType::Number(NumberType::SignedInteger(
            SignedIntegerType::Int,
        ))),
        Class::Basic(BasicType::Boolean),
    ];

    let generic_type = GenericType::new(base_type, type_args);

    let generic_params = vec![
        GenericTypeParameter::new("T".to_string()),
        GenericTypeParameter::new("K".to_string()),
    ];

    let concrete_types = vec![
        Class::Basic(BasicType::Number(NumberType::SignedInteger(
            SignedIntegerType::Int,
        ))),
        Class::Basic(BasicType::Boolean),
    ];

    assert!(generic_type.can_be_instantiated_with(&generic_params, &concrete_types));

    // Test with wrong number of arguments
    let wrong_types = vec![Class::Basic(BasicType::String)];
    assert!(!generic_type.can_be_instantiated_with(&generic_params, &wrong_types));
}

#[test]
fn test_random_generic_parameter_generation() {
    let mut rng = StdRng::seed_from_u64(42);
    let param = GenericTypeParameter::generate_random_generic_parameter(&mut rng, None);

    assert!(!param.name.is_empty());
    // Bounds and variance are randomly generated, so we just check they exist
    assert!(param.bounds.len() <= 2); // Max 2 bounds
}
