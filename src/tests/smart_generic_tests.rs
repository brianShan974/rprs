use rand::SeedableRng;
use rand::rngs::StdRng;

use crate::basic::cls::basic_type::BasicType;
use crate::basic::cls::class::Class;
use crate::basic::cls::generic_type::{
    GenericParameterContext, GenericTypeParameter, TypeBound, Variance,
};
use crate::basic::cls::number_types::number::NumberType;
use crate::basic::cls::number_types::signed_integer::SignedIntegerType;

#[test]
fn test_generic_parameter_context_creation() {
    let available_types = vec![
        Class::Basic(BasicType::String),
        Class::Basic(BasicType::Number(NumberType::SignedInteger(
            SignedIntegerType::Int,
        ))),
        Class::Basic(BasicType::Boolean),
    ];

    let context = GenericParameterContext::new("collection".to_string(), available_types.clone());

    assert_eq!(context.class_purpose, "collection");
    assert_eq!(context.available_types, available_types);
    assert_eq!(context.complexity_level, 1);
}

#[test]
fn test_context_purpose_detection() {
    let available_types = vec![];

    let collection_context =
        GenericParameterContext::new("collection".to_string(), available_types.clone());
    let numeric_context =
        GenericParameterContext::new("numeric".to_string(), available_types.clone());
    let text_context = GenericParameterContext::new("text".to_string(), available_types.clone());

    assert!(collection_context.is_collection_type());
    assert!(numeric_context.is_numeric_type());
    assert!(text_context.is_text_type());

    assert!(!collection_context.is_numeric_type());
    assert!(!numeric_context.is_text_type());
    assert!(!text_context.is_collection_type());
}

#[test]
fn test_smart_generic_parameter_generation() {
    let mut rng = StdRng::seed_from_u64(42);

    let available_types = vec![
        Class::Basic(BasicType::String),
        Class::Basic(BasicType::Number(NumberType::SignedInteger(
            SignedIntegerType::Int,
        ))),
    ];

    let context = GenericParameterContext::new("collection".to_string(), available_types);

    let param = GenericTypeParameter::generate_smart_generic_parameter(&mut rng, None, &context);

    // Should have a valid name
    assert!(!param.name.is_empty());

    // Should have appropriate constraints for collection type
    if !param.bounds.is_empty() {
        let has_valid_constraint = param.bounds.iter().any(|bound| match bound {
            TypeBound::Class(Class::Basic(BasicType::String)) => true,
            TypeBound::Class(Class::Basic(BasicType::Number(_))) => true,
            _ => false,
        });
        assert!(
            has_valid_constraint,
            "Collection type should have appropriate constraints"
        );
    }
}

#[test]
fn test_smart_variance_generation() {
    let mut rng = StdRng::seed_from_u64(42);

    let available_types = vec![];

    // Test collection type (should be covariant)
    let collection_context =
        GenericParameterContext::new("collection".to_string(), available_types.clone());
    let param =
        GenericTypeParameter::generate_smart_generic_parameter(&mut rng, None, &collection_context);

    // Collections are often covariant, but we allow some variance
    assert!(matches!(
        param.variance,
        Variance::Covariant | Variance::Invariant
    ));

    // Test producer type (should be covariant)
    let producer_context =
        GenericParameterContext::new("producer".to_string(), available_types.clone());
    let param =
        GenericTypeParameter::generate_smart_generic_parameter(&mut rng, None, &producer_context);

    assert_eq!(param.variance, Variance::Covariant);

    // Test consumer type (should be contravariant)
    let consumer_context =
        GenericParameterContext::new("consumer".to_string(), available_types.clone());
    let param =
        GenericTypeParameter::generate_smart_generic_parameter(&mut rng, None, &consumer_context);

    assert_eq!(param.variance, Variance::Contravariant);
}

#[test]
fn test_constraint_relationship_validation() {
    let mut rng = StdRng::seed_from_u64(42);

    let available_types = vec![
        Class::Basic(BasicType::String),
        Class::Basic(BasicType::Number(NumberType::SignedInteger(
            SignedIntegerType::Int,
        ))),
    ];

    let context = GenericParameterContext::new("numeric".to_string(), available_types);

    // Generate multiple parameters to test constraint validation
    let param1 = GenericTypeParameter::generate_smart_generic_parameter(&mut rng, None, &context);
    let _param2 = GenericTypeParameter::generate_smart_generic_parameter(&mut rng, None, &context);

    // Both parameters should have valid constraint relationships
    assert!(GenericTypeParameter::validate_constraint_relationships(
        &param1.bounds
    ));
}

#[test]
fn test_advanced_constraints_generation() {
    let mut rng = StdRng::seed_from_u64(42);

    let available_types = vec![
        Class::Basic(BasicType::String),
        Class::Basic(BasicType::Number(NumberType::SignedInteger(
            SignedIntegerType::Int,
        ))),
    ];

    let context = GenericParameterContext::new("collection".to_string(), available_types);

    let advanced_constraints =
        GenericTypeParameter::generate_advanced_constraints(&mut rng, &context);

    // Advanced constraints should be valid
    if !advanced_constraints.is_empty() {
        assert!(GenericTypeParameter::validate_constraint_relationships(
            &advanced_constraints
        ));
    }
}

#[test]
fn test_context_aware_constraint_generation() {
    let mut rng = StdRng::seed_from_u64(42);

    let available_types = vec![
        Class::Basic(BasicType::String),
        Class::Basic(BasicType::Number(NumberType::SignedInteger(
            SignedIntegerType::Int,
        ))),
    ];

    // Test numeric context
    let numeric_context =
        GenericParameterContext::new("numeric".to_string(), available_types.clone());
    let numeric_param =
        GenericTypeParameter::generate_smart_generic_parameter(&mut rng, None, &numeric_context);

    // Numeric types should have number constraints
    if !numeric_param.bounds.is_empty() {
        let has_numeric_constraint = numeric_param.bounds.iter().any(|bound| match bound {
            TypeBound::Class(Class::Basic(BasicType::Number(_))) => true,
            _ => false,
        });
        assert!(
            has_numeric_constraint,
            "Numeric type should have number constraints"
        );
    }

    // Test text context
    let text_context = GenericParameterContext::new("text".to_string(), available_types.clone());
    let text_param =
        GenericTypeParameter::generate_smart_generic_parameter(&mut rng, None, &text_context);

    // Text types should have string constraints
    if !text_param.bounds.is_empty() {
        let has_text_constraint = text_param.bounds.iter().any(|bound| match bound {
            TypeBound::Class(Class::Basic(BasicType::String)) => true,
            _ => false,
        });
        assert!(
            has_text_constraint,
            "Text type should have string constraints"
        );
    }
}

#[test]
fn test_complexity_level_management() {
    let available_types = vec![];

    let context =
        GenericParameterContext::new("collection".to_string(), available_types).with_complexity(3);

    assert_eq!(context.complexity_level, 3);
}
