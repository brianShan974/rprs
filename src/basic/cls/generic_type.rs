use rand::prelude::IndexedRandom;
use rand::{Rng, SeedableRng};
use std::collections::HashMap;
use std::fmt;

use crate::basic::cls::basic_type::BasicType;
use crate::basic::cls::class::Class;
use crate::basic::cls::number_types::floating_point::FloatingPointType;
use crate::basic::cls::number_types::number::NumberType;
use crate::basic::cls::number_types::signed_integer::SignedIntegerType;

/// Completeness issue types
#[derive(Debug, Clone)]
pub enum CompletenessIssue {
    Success(String),
    Warning(String),
    Error(String),
    Info(String),
}

/// Completeness report for generic type system
#[derive(Debug, Clone)]
pub struct CompletenessReport {
    pub is_complete: bool,
    pub issues: Vec<CompletenessIssue>,
    pub summary_text: String,
}

impl CompletenessReport {
    pub fn new() -> Self {
        Self {
            is_complete: true,
            issues: Vec::new(),
            summary_text: String::new(),
        }
    }

    pub fn is_complete(&self) -> bool {
        self.is_complete
    }

    pub fn add_issue(&mut self, issue: CompletenessIssue) {
        match &issue {
            CompletenessIssue::Error(_) => self.is_complete = false,
            _ => {}
        }
        self.issues.push(issue);
    }

    pub fn summary(&self) -> String {
        if self.summary_text.is_empty() {
            let success_count = self
                .issues
                .iter()
                .filter(|i| matches!(i, CompletenessIssue::Success(_)))
                .count();
            let warning_count = self
                .issues
                .iter()
                .filter(|i| matches!(i, CompletenessIssue::Warning(_)))
                .count();
            let error_count = self
                .issues
                .iter()
                .filter(|i| matches!(i, CompletenessIssue::Error(_)))
                .count();

            format!(
                "Completeness: {}% ({} success, {} warnings, {} errors)",
                100 - (error_count * 10).min(100),
                success_count,
                warning_count,
                error_count
            )
        } else {
            self.summary_text.clone()
        }
    }
}

/// Context for generic parameter generation with purpose and available types
#[derive(Clone, Debug)]
pub struct GenericParameterContext {
    pub class_purpose: String,
    pub available_types: Vec<Class>,
    pub complexity_level: u8,
}

impl GenericParameterContext {
    pub fn new(class_purpose: String, available_types: Vec<Class>) -> Self {
        Self {
            class_purpose,
            available_types,
            complexity_level: 1,
        }
    }

    pub fn with_complexity(mut self, complexity_level: u8) -> Self {
        self.complexity_level = complexity_level;
        self
    }

    pub fn is_collection_type(&self) -> bool {
        matches!(
            self.class_purpose.as_str(),
            "collection" | "list" | "set" | "map" | "queue" | "stack"
        )
    }

    pub fn is_numeric_type(&self) -> bool {
        matches!(
            self.class_purpose.as_str(),
            "numeric" | "math" | "calculation" | "arithmetic"
        )
    }

    pub fn is_text_type(&self) -> bool {
        matches!(
            self.class_purpose.as_str(),
            "text" | "string" | "formatting" | "parsing"
        )
    }
}

/// Context for generic type inference and instantiation
#[derive(Clone, Debug)]
pub struct GenericTypeContext {
    pub generic_parameters: Vec<GenericTypeParameter>,
    pub type_mappings: std::collections::HashMap<String, Class>,
    pub available_types: Vec<Class>,
}

impl GenericTypeContext {
    pub fn new(generic_parameters: Vec<GenericTypeParameter>, available_types: Vec<Class>) -> Self {
        Self {
            generic_parameters,
            type_mappings: std::collections::HashMap::new(),
            available_types,
        }
    }

    /// Try to infer the type for a specific generic parameter
    pub fn infer_type_for_parameter(&self, param: &GenericTypeParameter) -> Option<Class> {
        // First check if we have a direct mapping
        if let Some(mapped_type) = self.type_mappings.get(&param.name) {
            return Some(mapped_type.clone());
        }

        // Try to infer from available types based on constraints
        for available_type in &self.available_types {
            if param.validate_type_constraints(available_type) {
                return Some(available_type.clone());
            }
        }

        None
    }

    /// Add a type mapping for a generic parameter
    pub fn add_type_mapping(&mut self, param_name: &str, concrete_type: Class) {
        self.type_mappings
            .insert(param_name.to_string(), concrete_type);
    }

    /// Get all available types that satisfy a parameter's constraints
    pub fn get_compatible_types(&self, param: &GenericTypeParameter) -> Vec<Class> {
        self.available_types
            .iter()
            .filter(|&t| param.validate_type_constraints(t))
            .cloned()
            .collect()
    }
}

/// Represents a generic type parameter (e.g., T, K, V)
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenericTypeParameter {
    pub name: String,
    pub bounds: Vec<TypeBound>,
    pub variance: Variance,
}

/// Represents type bounds for generic parameters
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeBound {
    Class(Class),
    Interface(String), // For future interface support
    Multiple(Vec<TypeBound>),
}

/// Represents variance of generic type parameters
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Variance {
    Invariant,     // T
    Covariant,     // out T
    Contravariant, // in T
}

impl GenericTypeParameter {
    pub fn new(name: String) -> Self {
        Self {
            name,
            bounds: Vec::new(),
            variance: Variance::Invariant,
        }
    }

    pub fn with_bounds(name: String, bounds: Vec<TypeBound>) -> Self {
        Self {
            name,
            bounds,
            variance: Variance::Invariant,
        }
    }

    pub fn with_variance(name: String, variance: Variance) -> Self {
        Self {
            name,
            bounds: Vec::new(),
            variance,
        }
    }

    pub fn get_name(&self) -> String {
        self.name.clone()
    }

    pub fn get_bounds(&self) -> &[TypeBound] {
        &self.bounds
    }

    pub fn get_variance(&self) -> &Variance {
        &self.variance
    }

    /// Check if this parameter can be used as a return type
    pub fn can_be_return_type(&self) -> bool {
        // Covariant and invariant parameters can be return types
        matches!(self.variance, Variance::Covariant | Variance::Invariant)
    }

    /// Validate if a concrete type satisfies the type constraints
    pub fn validate_type_constraints(&self, concrete_type: &Class) -> bool {
        if self.bounds.is_empty() {
            // No constraints means any type is valid
            return true;
        }

        // Check if the concrete type satisfies all bounds
        for bound in &self.bounds {
            if !Self::satisfies_type_bound(concrete_type, bound) {
                return false;
            }
        }
        true
    }

    /// Check if two constraints conflict with each other
    fn constraints_conflict(a: &TypeBound, b: &TypeBound) -> bool {
        match (a, b) {
            (TypeBound::Class(class_a), TypeBound::Class(class_b)) => {
                // Check if classes are fundamentally incompatible
                match (class_a, class_b) {
                    (Class::Basic(_), Class::Basic(_)) => {
                        // Basic types are generally compatible
                        false
                    }
                    (Class::Custom(_), Class::Custom(_)) => {
                        // Custom types might conflict, but we'll assume they're compatible
                        false
                    }
                    (Class::Generic(_), Class::Generic(_)) => {
                        // Generic types might conflict, but we'll assume they're compatible
                        false
                    }
                    _ => {
                        // Different type categories might conflict
                        false
                    }
                }
            }
            (TypeBound::Interface(_), TypeBound::Interface(_)) => {
                // Interface constraints are generally compatible
                false
            }
            (TypeBound::Multiple(_), _) | (_, TypeBound::Multiple(_)) => {
                // Multiple bounds need more sophisticated checking
                false
            }
            _ => {
                // Different constraint types are generally compatible
                false
            }
        }
    }

    /// Validate if a concrete type can be used for this generic parameter
    pub fn can_accept_concrete_type(&self, concrete_type: &Class) -> bool {
        // If no bounds, any type is acceptable
        if self.bounds.is_empty() {
            return true;
        }

        // Check if the concrete type satisfies all bounds
        for bound in &self.bounds {
            if !Self::satisfies_type_bound(concrete_type, bound) {
                return false;
            }
        }

        true
    }

    /// Check if a concrete type satisfies a specific type bound
    pub fn satisfies_type_bound(concrete_type: &Class, bound: &TypeBound) -> bool {
        match bound {
            TypeBound::Class(bound_class) => {
                // For now, we'll do a simple type compatibility check
                // In a more sophisticated system, this would check inheritance, interfaces, etc.
                match (concrete_type, bound_class) {
                    (Class::Basic(_), Class::Basic(_)) => {
                        // Basic types are compatible if they're the same or related
                        concrete_type == bound_class
                            || Self::are_basic_types_compatible(concrete_type, bound_class)
                    }
                    (Class::Custom(_), Class::Custom(_)) => {
                        // Custom types are compatible if they're the same
                        concrete_type == bound_class
                    }
                    (Class::Generic(_), Class::Generic(_)) => {
                        // Generic types need more sophisticated checking
                        // For now, we'll assume they're compatible
                        true
                    }
                    _ => {
                        // Different type categories are generally not compatible
                        false
                    }
                }
            }
            TypeBound::Interface(interface_name) => {
                // Check if the concrete type implements the interface
                // For now, we'll do a simple check based on type characteristics
                match interface_name.as_str() {
                    "Comparable" => {
                        // String and Number types are generally comparable
                        matches!(
                            concrete_type,
                            Class::Basic(BasicType::String) | Class::Basic(BasicType::Number(_))
                        )
                    }
                    "Serializable" => {
                        // Most types are serializable
                        true
                    }
                    _ => {
                        // Unknown interface, assume compatible
                        true
                    }
                }
            }
            TypeBound::Multiple(bounds) => {
                // For multiple bounds, the type must satisfy ALL bounds
                bounds
                    .iter()
                    .all(|bound| Self::satisfies_type_bound(concrete_type, bound))
            }
        }
    }

    /// Check if two basic types are compatible for generic constraints
    fn are_basic_types_compatible(a: &Class, b: &Class) -> bool {
        match (a, b) {
            (Class::Basic(BasicType::Number(_)), Class::Basic(BasicType::Number(_))) => {
                // All number types are compatible with each other
                true
            }
            (Class::Basic(BasicType::String), Class::Basic(BasicType::String)) => {
                // String types are compatible
                true
            }
            (Class::Basic(BasicType::Boolean), Class::Basic(BasicType::Boolean)) => {
                // Boolean types are compatible
                true
            }
            _ => {
                // Different basic type categories are not compatible
                false
            }
        }
    }

    /// Generate a random generic type parameter
    pub fn generate_random_generic_parameter<T: Rng + SeedableRng>(
        rng: &mut T,
        existing_names: Option<&mut Vec<String>>,
    ) -> Self {
        let mut existing_names = existing_names.unwrap_or(&mut Vec::new()).clone();
        let name = Self::generate_unique_parameter_name(rng, &mut existing_names);

        // 30% chance to have bounds
        let bounds = if rng.random_bool(0.3) {
            let num_bounds = rng.random_range(1..=2);
            let mut bounds = Vec::new();
            for _ in 0..num_bounds {
                let bound = Self::generate_random_type_bound(rng);
                bounds.push(bound);
            }
            bounds
        } else {
            Vec::new()
        };

        // 20% chance to have variance
        let variance = if rng.random_bool(0.2) {
            match rng.random_range(0..3) {
                0 => Variance::Covariant,
                1 => Variance::Contravariant,
                _ => Variance::Invariant,
            }
        } else {
            Variance::Invariant
        };

        Self {
            name,
            bounds,
            variance,
        }
    }

    /// Generate a smart generic parameter with context-aware constraints
    pub fn generate_smart_generic_parameter<T: Rng + SeedableRng>(
        rng: &mut T,
        existing_names: Option<&mut Vec<String>>,
        context: &GenericParameterContext,
    ) -> Self {
        let mut existing_names = existing_names.unwrap_or(&mut Vec::new()).clone();
        let name = Self::generate_unique_parameter_name(rng, &mut existing_names);

        // Generate context-aware bounds
        let bounds = Self::generate_smart_constraints(rng, context);

        // Generate context-aware variance
        let variance = Self::generate_smart_variance(rng, context);

        Self {
            name,
            bounds,
            variance,
        }
    }

    /// Generate smart constraints based on context with no depth limit
    fn generate_smart_constraints<T: Rng + SeedableRng>(
        rng: &mut T,
        context: &GenericParameterContext,
    ) -> Vec<TypeBound> {
        // Use complexity level to determine constraint sophistication
        let has_bounds_probability = match context.complexity_level {
            0 | 1 => 0.3,       // 30% chance for simple types
            2 => 0.5,           // 50% chance for medium complexity
            3..=u8::MAX => 0.7, // 70% chance for complex types
        };

        if !rng.random_bool(has_bounds_probability) {
            return Vec::new();
        }

        // Generate variable number of constraints based on complexity
        let max_bounds = match context.complexity_level {
            0 | 1 => 2,       // Simple: 1-2 constraints
            2 => 4,           // Medium: 1-4 constraints
            3..=u8::MAX => 8, // Complex: 1-8 constraints
        };

        let num_bounds = rng.random_range(1..=max_bounds);
        let mut bounds = Vec::new();

        for _ in 0..num_bounds {
            let bound = Self::generate_smart_type_bound(rng, context);
            bounds.push(bound);
        }

        // Validate constraint relationships
        if !Self::validate_constraint_relationships(&bounds) {
            // If constraints are invalid, fall back to simple bounds
            bounds = vec![Self::generate_random_type_bound(rng)];
        }

        bounds
    }

    /// Generate intelligent type bound based on context
    fn generate_smart_type_bound<T: Rng + SeedableRng>(
        rng: &mut T,
        context: &GenericParameterContext,
    ) -> TypeBound {
        // Use context information to generate appropriate constraints
        if context.is_collection_type() {
            // For collections, prefer element types
            if !context.available_types.is_empty() && rng.random_bool(0.7) {
                let available_type = context.available_types.choose(rng).unwrap().clone();
                return TypeBound::Class(available_type);
            }
            // Fallback to common collection element types
            let collection_types = vec![
                Class::Basic(BasicType::String),
                Class::Basic(BasicType::Number(NumberType::SignedInteger(
                    SignedIntegerType::Int,
                ))),
                Class::Basic(BasicType::Boolean),
            ];
            let chosen_type = collection_types.choose(rng).unwrap().clone();
            return TypeBound::Class(chosen_type);
        }

        if context.is_numeric_type() {
            // For numeric types, prefer numeric constraints
            let numeric_types = vec![
                Class::Basic(BasicType::Number(NumberType::SignedInteger(
                    SignedIntegerType::Int,
                ))),
                Class::Basic(BasicType::Number(NumberType::SignedInteger(
                    SignedIntegerType::Long,
                ))),
                Class::Basic(BasicType::Number(NumberType::FloatingPoint(
                    FloatingPointType::Float,
                ))),
                Class::Basic(BasicType::Number(NumberType::FloatingPoint(
                    FloatingPointType::Double,
                ))),
            ];
            let chosen_type = numeric_types.choose(rng).unwrap().clone();
            return TypeBound::Class(chosen_type);
        }

        if context.is_text_type() {
            // For text types, prefer string-related types
            let text_types = vec![
                Class::Basic(BasicType::String),
                Class::Basic(BasicType::Char),
            ];
            let chosen_type = text_types.choose(rng).unwrap().clone();
            return TypeBound::Class(chosen_type);
        }

        // For other types, use available types or generate complex types
        if !context.available_types.is_empty() && rng.random_bool(0.6) {
            let available_type = context.available_types.choose(rng).unwrap().clone();
            return TypeBound::Class(available_type);
        }

        // Generate complex types based on complexity level
        match context.complexity_level {
            0 | 1 => {
                // Simple: basic types only
                let basic_types = vec![
                    Class::Basic(BasicType::String),
                    Class::Basic(BasicType::Number(NumberType::SignedInteger(
                        SignedIntegerType::Int,
                    ))),
                    Class::Basic(BasicType::Number(NumberType::FloatingPoint(
                        FloatingPointType::Float,
                    ))),
                    Class::Basic(BasicType::Boolean),
                ];
                let chosen_type = basic_types.choose(rng).unwrap().clone();
                TypeBound::Class(chosen_type)
            }
            2 => {
                // Medium: mix of basic and custom types
                if rng.random_bool(0.3) {
                    // 30% chance for custom types
                    TypeBound::Class(Class::generate_random_class(rng, None, None))
                } else {
                    // 70% chance for basic types
                    let basic_types = vec![
                        Class::Basic(BasicType::String),
                        Class::Basic(BasicType::Number(NumberType::SignedInteger(
                            SignedIntegerType::Int,
                        ))),
                        Class::Basic(BasicType::Number(NumberType::FloatingPoint(
                            FloatingPointType::Float,
                        ))),
                        Class::Basic(BasicType::Boolean),
                    ];
                    let chosen_type = basic_types.choose(rng).unwrap().clone();
                    TypeBound::Class(chosen_type)
                }
            }
            3..=u8::MAX => {
                // Complex: full range of types including nested generics
                match rng.random_range(0..4) {
                    0 => {
                        // Basic types
                        let basic_types = vec![
                            Class::Basic(BasicType::String),
                            Class::Basic(BasicType::Number(NumberType::SignedInteger(
                                SignedIntegerType::Int,
                            ))),
                            Class::Basic(BasicType::Number(NumberType::FloatingPoint(
                                FloatingPointType::Float,
                            ))),
                            Class::Basic(BasicType::Boolean),
                        ];
                        let chosen_type = basic_types.choose(rng).unwrap().clone();
                        TypeBound::Class(chosen_type)
                    }
                    1 => {
                        // Custom types
                        TypeBound::Class(Class::generate_random_class(rng, None, None))
                    }
                    2 => {
                        // Nested generic types
                        let generic_type = GenericType::generate_random_generic_type(rng, None);
                        TypeBound::Class(Class::Generic(Box::new(generic_type)))
                    }
                    3 => {
                        // Interface types
                        let interfaces = vec![
                            "Comparable",
                            "Serializable",
                            "Cloneable",
                            "Iterable",
                            "Collection",
                            "List",
                            "Set",
                            "Map",
                            "Iterator",
                            "Stream",
                        ];
                        let interface_name = interfaces.choose(rng).unwrap().to_string();
                        TypeBound::Interface(interface_name)
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    /// Generate intelligent random bound with context awareness
    fn generate_smart_random_bound<T: Rng + SeedableRng>(
        rng: &mut T,
        context: &GenericParameterContext,
    ) -> TypeBound {
        // Use complexity level to determine constraint sophistication
        match context.complexity_level {
            0 | 1 => {
                // Simple: just basic types
                let basic_types = vec![
                    Class::Basic(BasicType::String),
                    Class::Basic(BasicType::Number(NumberType::SignedInteger(
                        SignedIntegerType::Int,
                    ))),
                    Class::Basic(BasicType::Boolean),
                ];
                let chosen_type = basic_types.choose(rng).unwrap().clone();
                TypeBound::Class(chosen_type)
            }
            2 => {
                // Medium: use context-aware generation
                Self::generate_smart_type_bound(rng, context)
            }
            3..=u8::MAX => {
                // Complex: generate multiple constraints or advanced types
                if rng.random_bool(0.3) {
                    // Generate multiple bounds
                    let mut bounds = Vec::new();
                    let num_bounds = rng.random_range(2..=3);
                    for _ in 0..num_bounds {
                        bounds.push(Self::generate_smart_type_bound(rng, context));
                    }
                    TypeBound::Multiple(bounds)
                } else {
                    // Generate advanced single bound
                    Self::generate_smart_type_bound(rng, context)
                }
            }
        }
    }

    /// Generate smart variance based on context
    fn generate_smart_variance<T: Rng + SeedableRng>(
        rng: &mut T,
        context: &GenericParameterContext,
    ) -> Variance {
        // Generate variance based on context and purpose
        if context.is_collection_type() {
            // Collections are typically covariant for read operations
            match rng.random_range(0..3) {
                0 => Variance::Covariant,     // 40% - most common for collections
                1 => Variance::Invariant,     // 30% - for mutable collections
                _ => Variance::Contravariant, // 30% - for write operations
            }
        } else if context.is_numeric_type() {
            // Numeric types are typically invariant
            match rng.random_range(0..3) {
                0 => Variance::Invariant,     // 50% - most common for numeric
                1 => Variance::Covariant,     // 30% - for numeric operations
                _ => Variance::Contravariant, // 20% - less common
            }
        } else if context.is_text_type() {
            // Text types can be covariant (immutable) or invariant (mutable)
            match rng.random_range(0..3) {
                0 => Variance::Covariant,     // 40% - for immutable strings
                1 => Variance::Invariant,     // 40% - for mutable strings
                _ => Variance::Contravariant, // 20% - less common
            }
        } else {
            // For other types, use complexity-based generation
            match context.complexity_level {
                0 | 1 => Variance::Invariant, // Simple: always invariant
                2 => {
                    // Medium: prefer invariant and covariant
                    match rng.random_range(0..3) {
                        0 => Variance::Invariant,     // 50%
                        1 => Variance::Covariant,     // 30%
                        _ => Variance::Contravariant, // 20%
                    }
                }
                3..=u8::MAX => {
                    // Complex: all variances equally likely
                    match rng.random_range(0..3) {
                        0 => Variance::Invariant,
                        1 => Variance::Covariant,
                        _ => Variance::Contravariant,
                    }
                }
            }
        }
    }

    /// Validate constraint relationships for logical consistency
    pub fn validate_constraint_relationships(constraints: &[TypeBound]) -> bool {
        if constraints.len() <= 1 {
            return true;
        }

        // Check for conflicting constraints
        for i in 0..constraints.len() {
            for j in (i + 1)..constraints.len() {
                if Self::constraints_conflict(&constraints[i], &constraints[j]) {
                    return false;
                }
            }
        }

        true
    }

    /// Generate advanced constraints with complex relationships
    pub fn generate_advanced_constraints<T: Rng + SeedableRng>(
        rng: &mut T,
        context: &GenericParameterContext,
    ) -> Vec<TypeBound> {
        if !rng.random_bool(0.3) {
            // 30% chance for advanced constraints
            return Vec::new();
        }

        let constraint_type = rng.random_range(0..4);
        match constraint_type {
            0 => {
                // Single class constraint
                vec![Self::generate_smart_type_bound(rng, context)]
            }
            1 => {
                // Multiple class constraints
                let num_constraints = rng.random_range(2..=3);
                let mut constraints = Vec::new();
                for _ in 0..num_constraints {
                    constraints.push(Self::generate_smart_type_bound(rng, context));
                }
                constraints
            }
            2 => {
                // Interface constraint (for future use)
                vec![TypeBound::Interface("Comparable".to_string())]
            }
            3 => {
                // Mixed constraints
                let mut constraints = Vec::new();
                constraints.push(Self::generate_smart_type_bound(rng, context));
                if rng.random_bool(0.5) {
                    constraints.push(TypeBound::Interface("Serializable".to_string()));
                }
                constraints
            }
            _ => unreachable!(),
        }
    }

    /// Generate a random type bound
    fn generate_random_type_bound<T: Rng + SeedableRng>(rng: &mut T) -> TypeBound {
        // Generate complex type bounds with no depth limit
        match rng.random_range(0..4) {
            0 => {
                // Basic types
                let basic_types = vec![
                    Class::Basic(BasicType::Number(NumberType::SignedInteger(
                        SignedIntegerType::generate_random_signed_integer_type(rng),
                    ))),
                    Class::Basic(BasicType::Number(NumberType::FloatingPoint(
                        FloatingPointType::generate_random_floating_point_type(rng),
                    ))),
                    Class::Basic(BasicType::Boolean),
                    Class::Basic(BasicType::String),
                ];
                let chosen_type = basic_types.choose(rng).unwrap().clone();
                TypeBound::Class(chosen_type)
            }
            1 => {
                // Custom types (if available)
                TypeBound::Class(Class::generate_random_class(rng, None, None))
            }
            2 => {
                // Generic types - recursive generation
                let generic_type = GenericType::generate_random_generic_type(rng, None);
                TypeBound::Class(Class::Generic(Box::new(generic_type)))
            }
            3 => {
                // Interface bounds
                let interfaces = vec![
                    "Comparable",
                    "Serializable",
                    "Cloneable",
                    "Iterable",
                    "Collection",
                    "List",
                    "Set",
                    "Map",
                    "Iterator",
                ];
                let interface_name = interfaces.choose(rng).unwrap().to_string();
                TypeBound::Interface(interface_name)
            }
            _ => unreachable!(),
        }
    }

    /// Generate a unique parameter name
    fn generate_unique_parameter_name<T: Rng + SeedableRng>(
        rng: &mut T,
        existing_names: &mut Vec<String>,
    ) -> String {
        let common_names = vec!["T", "K", "V", "E", "R", "U", "S", "A", "B", "C"];

        // Try common names first
        for name in &common_names {
            if !existing_names.contains(&name.to_string()) {
                existing_names.push(name.to_string());
                return name.to_string();
            }
        }

        // Generate a random name if all common names are taken
        loop {
            let name = format!("T{}", rng.random_range(0..1000));
            if !existing_names.contains(&name) {
                existing_names.push(name.clone());
                return name;
            }
        }
    }
}

impl fmt::Display for GenericTypeParameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Display variance if not invariant
        match &self.variance {
            Variance::Covariant => write!(f, "out ")?,
            Variance::Contravariant => write!(f, "in ")?,
            Variance::Invariant => {}
        }

        write!(f, "{}", self.name)?;

        // Display bounds if any
        if !self.bounds.is_empty() {
            write!(f, " : ")?;
            for (i, bound) in self.bounds.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                match bound {
                    TypeBound::Class(class) => write!(f, "{}", class.get_name())?,
                    TypeBound::Interface(interface) => write!(f, "{}", interface)?,
                    TypeBound::Multiple(bounds) => {
                        for (j, bound) in bounds.iter().enumerate() {
                            if j > 0 {
                                write!(f, " & ")?;
                            }
                            match bound {
                                TypeBound::Class(class) => write!(f, "{}", class.get_name())?,
                                TypeBound::Interface(interface) => write!(f, "{}", interface)?,
                                TypeBound::Multiple(_) => write!(f, "ComplexBound")?,
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }
}

impl fmt::Display for TypeBound {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeBound::Class(class) => write!(f, "{}", class.get_name()),
            TypeBound::Interface(interface) => write!(f, "{}", interface),
            TypeBound::Multiple(bounds) => {
                for (i, bound) in bounds.iter().enumerate() {
                    if i > 0 {
                        write!(f, " & ")?;
                    }
                    write!(f, "{}", bound)?;
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for Variance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variance::Invariant => write!(f, ""),
            Variance::Covariant => write!(f, "out "),
            Variance::Contravariant => write!(f, "in "),
        }
    }
}

/// Represents a generic type (e.g., List<String>, Map<K, V>)
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenericType {
    pub base_type: Class,
    pub type_arguments: Vec<Class>,
}

impl GenericType {
    pub fn new(base_type: Class, type_arguments: Vec<Class>) -> Self {
        Self {
            base_type,
            type_arguments,
        }
    }

    pub fn get_base_type(&self) -> &Class {
        &self.base_type
    }

    pub fn get_type_arguments(&self) -> &[Class] {
        &self.type_arguments
    }

    /// Instantiate a generic type by replacing formal type parameters with concrete types
    pub fn instantiate_with_types(
        &self,
        generic_parameters: &[GenericTypeParameter],
        concrete_types: &[Class],
    ) -> Result<Class, String> {
        // Check if we have the right number of type arguments
        if concrete_types.len() != generic_parameters.len() {
            return Err(format!(
                "Expected {} type arguments, got {}",
                generic_parameters.len(),
                concrete_types.len()
            ));
        }

        // Validate that concrete types satisfy the constraints
        for (param, concrete_type) in generic_parameters.iter().zip(concrete_types.iter()) {
            if !param.validate_type_constraints(concrete_type) {
                return Err(format!(
                    "Type {} does not satisfy constraints for parameter {}",
                    concrete_type.get_name(),
                    param.get_name()
                ));
            }
        }

        // Create a new generic type with concrete type arguments
        let instantiated_type = GenericType::new(self.base_type.clone(), concrete_types.to_vec());

        Ok(Class::Generic(Box::new(instantiated_type)))
    }

    /// Infer generic types from a context (e.g., method parameters, return types)
    pub fn infer_generic_types(&self, context: &GenericTypeContext) -> Result<Vec<Class>, String> {
        let mut inferred_types = Vec::new();

        // For each generic parameter, try to infer its type from context
        for param in &context.generic_parameters {
            if let Some(inferred_type) = context.infer_type_for_parameter(param) {
                inferred_types.push(inferred_type);
            } else {
                // If we can't infer the type, we need to generate a default
                inferred_types.push(Self::generate_default_type_for_parameter(param));
            }
        }

        Ok(inferred_types)
    }

    /// Generate a default type for a generic parameter when inference fails
    fn generate_default_type_for_parameter(param: &GenericTypeParameter) -> Class {
        // Generate a basic type as default
        // In a more sophisticated system, this could use the parameter's bounds
        if param.bounds.is_empty() {
            // No bounds, use a common basic type
            Class::Basic(BasicType::String)
        } else {
            // Use the first bound as default
            match &param.bounds[0] {
                TypeBound::Class(class) => class.clone(),
                TypeBound::Interface(_) => Class::Basic(BasicType::String),
                TypeBound::Multiple(bounds) => {
                    if let Some(first_bound) = bounds.first() {
                        match first_bound {
                            TypeBound::Class(class) => class.clone(),
                            _ => Class::Basic(BasicType::String),
                        }
                    } else {
                        Class::Basic(BasicType::String)
                    }
                }
            }
        }
    }

    /// Check if this generic type can be instantiated with the given types
    pub fn can_be_instantiated_with(
        &self,
        generic_parameters: &[GenericTypeParameter],
        concrete_types: &[Class],
    ) -> bool {
        // Check number of arguments
        if concrete_types.len() != generic_parameters.len() {
            return false;
        }

        // Check constraints
        generic_parameters
            .iter()
            .zip(concrete_types.iter())
            .all(|(param, concrete_type)| param.validate_type_constraints(concrete_type))
    }

    /// Get the arity (number of type parameters) of this generic type
    pub fn get_arity(&self) -> usize {
        self.type_arguments.len()
    }

    /// Check if this is a raw generic type (has formal type parameters)
    pub fn is_raw_generic(&self) -> bool {
        // Check if any type argument is a formal type parameter
        self.type_arguments
            .iter()
            .any(|arg| matches!(arg, Class::FormalTypeParameter(_)))
    }

    /// Generate a random generic type
    pub fn generate_random_generic_type<T: Rng + SeedableRng>(
        rng: &mut T,
        defined_classes: Option<&[Class]>,
    ) -> Self {
        // Choose a base type (prefer custom classes for generics)
        let base_type = if let Some(classes) = defined_classes {
            if !classes.is_empty() && rng.random_bool(0.7) {
                classes.choose(rng).unwrap().clone()
            } else {
                Class::generate_random_class(rng, Some(&mut classes.to_vec()), None)
            }
        } else {
            // No defined classes available, use basic types only
            Class::Basic(BasicType::generate_random_basic_type(rng))
        };

        // Generate 1-3 type arguments
        let num_args = rng.random_range(1..=3);
        let mut type_arguments = Vec::new();

        for _ in 0..num_args {
            let arg_type = if let Some(classes) = defined_classes {
                if !classes.is_empty() && rng.random_bool(0.6) {
                    classes.choose(rng).unwrap().clone()
                } else {
                    Class::generate_random_class(rng, Some(&mut classes.to_vec()), None)
                }
            } else {
                // No defined classes available, use basic types only
                Class::Basic(BasicType::generate_random_basic_type(rng))
            };
            type_arguments.push(arg_type);
        }

        Self::new(base_type, type_arguments)
    }

    /// Check if this generic type is fully instantiated
    pub fn is_fully_instantiated(&self) -> bool {
        !self
            .type_arguments
            .iter()
            .any(|arg| matches!(arg, Class::FormalTypeParameter(_)))
    }

    /// Get all formal type parameters from this generic type
    pub fn get_formal_type_parameters(&self) -> Vec<String> {
        let mut params = Vec::new();
        for arg in &self.type_arguments {
            if let Class::FormalTypeParameter(name) = arg {
                params.push(name.to_string());
            }
        }
        params
    }

    /// Check if this generic type has recursive generic structure
    pub fn has_recursive_generic_structure(&self) -> bool {
        self.check_recursive_structure(&mut HashMap::new())
    }

    /// Helper method to check for recursive structure
    fn check_recursive_structure(&self, visited: &mut HashMap<String, bool>) -> bool {
        let type_name = self.base_type.get_name();

        if visited.contains_key(&type_name) {
            return true;
        }

        visited.insert(type_name.clone(), true);

        for arg in &self.type_arguments {
            if let Class::Generic(generic_arg) = arg {
                if generic_arg.check_recursive_structure(visited) {
                    return true;
                }
            }
        }

        visited.remove(&type_name);
        false
    }

    /// Get the common supertype of all type arguments
    pub fn get_common_supertype(&self) -> Option<Class> {
        if self.type_arguments.is_empty() {
            return None;
        }

        if self.type_arguments.len() == 1 {
            return Some(self.type_arguments[0].clone());
        }

        let mut common_type = self.type_arguments[0].clone();

        for arg in &self.type_arguments[1..] {
            if let Some(supertype) = Self::find_common_supertype(&common_type, arg) {
                common_type = supertype;
            } else {
                return Some(Class::Basic(BasicType::String));
            }
        }

        Some(common_type)
    }

    /// Find common supertype between two types
    fn find_common_supertype(type1: &Class, type2: &Class) -> Option<Class> {
        if type1 == type2 {
            return Some(type1.clone());
        }

        if let (Class::Basic(basic1), Class::Basic(basic2)) = (type1, type2) {
            return Self::find_common_basic_type(basic1, basic2);
        }

        // Use String as common type instead of Object
        Some(Class::Basic(BasicType::String))
    }

    /// Find common basic type between two basic types
    fn find_common_basic_type(basic1: &BasicType, basic2: &BasicType) -> Option<Class> {
        match (basic1, basic2) {
            (BasicType::Number(_), BasicType::Number(_)) => Some(Class::Basic(BasicType::Number(
                NumberType::FloatingPoint(FloatingPointType::Double),
            ))),
            (BasicType::String, _) | (_, BasicType::String) => {
                Some(Class::Basic(BasicType::String))
            }
            (BasicType::Boolean, _) | (_, BasicType::Boolean) => {
                Some(Class::Basic(BasicType::String))
            }
            _ => Some(Class::Basic(BasicType::String)),
        }
    }

    /// Check if this generic type can unify with another generic type
    pub fn can_unify_with(&self, other: &GenericType) -> bool {
        if self.base_type != other.base_type {
            return false;
        }

        if self.type_arguments.len() != other.type_arguments.len() {
            return false;
        }

        for (arg1, arg2) in self.type_arguments.iter().zip(other.type_arguments.iter()) {
            if !Self::can_unify_type_arguments(arg1, arg2) {
                return false;
            }
        }

        true
    }

    /// Check if two type arguments can unify
    fn can_unify_type_arguments(arg1: &Class, arg2: &Class) -> bool {
        match (arg1, arg2) {
            (a, b) if a == b => true,
            (Class::FormalTypeParameter(_), _) | (_, Class::FormalTypeParameter(_)) => true,
            (Class::Basic(basic1), Class::Basic(basic2)) => {
                Self::basic_types_compatible(basic1, basic2)
            }
            (Class::Generic(gen1), Class::Generic(gen2)) => gen1.can_unify_with(gen2),
            _ => false,
        }
    }

    /// Check if two basic types are compatible for unification
    fn basic_types_compatible(basic1: &BasicType, basic2: &BasicType) -> bool {
        match (basic1, basic2) {
            (BasicType::Number(_), BasicType::Number(_)) => true,
            (BasicType::String, _) | (_, BasicType::String) => true,
            (BasicType::Boolean, _) | (_, BasicType::Boolean) => true,
            _ => false,
        }
    }

    /// Get the most general unifier (MGU) between this type and another
    pub fn get_most_general_unifier(&self, other: &GenericType) -> Option<HashMap<String, Class>> {
        if !self.can_unify_with(other) {
            return None;
        }

        let mut unifier = HashMap::new();

        for (arg1, arg2) in self.type_arguments.iter().zip(other.type_arguments.iter()) {
            if let Some(substitution) = Self::build_substitution(arg1, arg2) {
                unifier.extend(substitution);
            }
        }

        Some(unifier)
    }

    /// Build substitution mapping between two type arguments
    fn build_substitution(arg1: &Class, arg2: &Class) -> Option<HashMap<String, Class>> {
        let mut substitution = HashMap::new();

        match (arg1, arg2) {
            (Class::FormalTypeParameter(name), concrete_type) => {
                substitution.insert(name.to_string(), concrete_type.clone());
            }
            (concrete_type, Class::FormalTypeParameter(name)) => {
                substitution.insert(name.to_string(), concrete_type.clone());
            }
            (a, b) if a == b => {}
            (Class::Generic(gen1), Class::Generic(gen2)) => {
                if let Some(recursive_sub) = gen1.get_most_general_unifier(gen2) {
                    substitution.extend(recursive_sub);
                }
            }
            _ => return None,
        }

        Some(substitution)
    }

    /// Validate all constraints for a set of generic parameters
    pub fn validate_all_constraints(
        &self,
        generic_parameters: &[GenericTypeParameter],
    ) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();

        if self.type_arguments.len() != generic_parameters.len() {
            errors.push(format!(
                "Expected {} generic parameters, got {} type arguments",
                generic_parameters.len(),
                self.type_arguments.len()
            ));
        }

        for (arg, param) in self.type_arguments.iter().zip(generic_parameters.iter()) {
            if !param.validate_type_constraints(arg) {
                errors.push(format!(
                    "Type argument {} does not satisfy constraints for parameter {}",
                    arg.get_name(),
                    param.get_name()
                ));
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Check system completeness for this generic type
    pub fn check_system_completeness(&self) -> CompletenessReport {
        let mut report = CompletenessReport::new();

        if self.is_fully_instantiated() {
            report.add_issue(CompletenessIssue::Success(
                "Generic type is fully instantiated".to_string(),
            ));
        } else {
            report.add_issue(CompletenessIssue::Info(
                "Generic type has formal type parameters".to_string(),
            ));
        }

        if self.has_recursive_generic_structure() {
            report.add_issue(CompletenessIssue::Warning(
                "Generic type has recursive structure".to_string(),
            ));
        } else {
            report.add_issue(CompletenessIssue::Success(
                "No recursive structure detected".to_string(),
            ));
        }

        if !self.type_arguments.is_empty() {
            report.add_issue(CompletenessIssue::Success(format!(
                "Generic type has {} type arguments",
                self.type_arguments.len()
            )));
        }

        // Check base type validity - use String instead of Object
        if !matches!(self.base_type, Class::Basic(BasicType::String)) {
            report.add_issue(CompletenessIssue::Success("Base type is valid".to_string()));
        }

        report
    }
}

impl fmt::Display for GenericType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}<{}>",
            self.base_type.get_name(),
            self.type_arguments
                .iter()
                .map(|t| t.get_name())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}
