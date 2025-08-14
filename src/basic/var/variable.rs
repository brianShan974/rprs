use super::prefix::var_prefix::VariablePrefix;
use crate::basic::cls::basic_types::{BasicType, FloatingPointType, NumberType};
use crate::basic::cls::class::Class;
use crate::basic::expr::expression::Expression;
use derive_more::Constructor;
use rand::{Rng, seq::IteratorRandom};

const LETTERS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
const LETTERS_AND_DIGITS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";

const HARD_KEYWORDS: &[&str] = &[
    "as",
    "break",
    "class",
    "continue",
    "do",
    "else",
    "false",
    "for",
    "fun",
    "if",
    "in",
    "interface",
    "is",
    "null",
    "object",
    "package",
    "return",
    "super",
    "this",
    "throw",
    "true",
    "try",
    "typealias",
    "typeof",
    "val",
    "var",
    "when",
    "while",
];
const SOFT_KEYWORDS: &[&str] = &[
    "by",
    "catch",
    "constructor",
    "delegate",
    "dynamic",
    "field",
    "file",
    "finally",
    "get",
    "import",
    "init",
    "param",
    "property",
    "receiver",
    "set",
    "setparam",
    "value",
    "where",
];
const MODIFIER_KEYWORDS: &[&str] = &[
    "abstract",
    "actual",
    "annotation",
    "companion",
    "const",
    "crossinline",
    "data",
    "enum",
    "expect",
    "external",
    "final",
    "infix",
    "inline",
    "inner",
    "internal",
    "lateinit",
    "noinline",
    "open",
    "operator",
    "out",
    "override",
    "private",
    "protected",
    "public",
    "reified",
    "sealed",
    "suspend",
    "tailrec",
    "vararg",
];

#[derive(Constructor, Clone, Debug)]
pub struct Variable {
    prefix: VariablePrefix,
    name: String,
    value: Option<Expression>,
    ty: Option<Class>,
}

impl Variable {
    pub fn output_declaration(&self) -> String {
        format!("{}{}", self.prefix, self.name)
    }

    pub fn output_assignment(&self) -> Option<String> {
        self.value
            .as_ref()
            .map(|value| format!("{} = {}", self.name, value))
    }

    pub fn output_init(&self) -> Option<String> {
        self.value
            .as_ref()
            .map(|value| format!("{}{} = {}", self.prefix, self.name, value))
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn is_mutable(&self) -> bool {
        self.prefix.is_mutable()
    }

    pub fn generate_random_variable(is_stack: bool, with_initial_value: bool) -> Self {
        let prefix = VariablePrefix::generate_random_prefix(is_stack);
        let name = Self::generate_random_variable_name();
        let value = if with_initial_value {
            Some(Expression::generate_random_expression(5))
        } else {
            None
        };
        let ty = if let Some(value) = &value
            && value.is_arithmetic()
        {
            Some(Class::Basic(BasicType::Number(NumberType::FloatingPoint(
                FloatingPointType::Float,
            ))))
        } else {
            None
        };

        Self {
            prefix,
            name,
            value,
            ty,
        }
    }

    pub fn generate_random_variable_name() -> String {
        let mut rng = rand::rng();

        // First character: letter or underscore
        let first_char = LETTERS.chars().choose(&mut rng).unwrap();

        // Remaining characters: letters, digits or underscores
        let remaining_chars: String = (0..rng.random_range(0..10))
            .map(|_| LETTERS_AND_DIGITS.chars().choose(&mut rng).unwrap())
            .collect();

        let name = format!("{first_char}{remaining_chars}");

        // Check if it's a reserved keyword (extensible list)
        if is_reserved_keyword(&name) {
            Self::generate_random_variable_name()
        } else {
            name
        }
    }
}

pub fn is_reserved_keyword(name: &str) -> bool {
    HARD_KEYWORDS.contains(&name)
        || SOFT_KEYWORDS.contains(&name)
        || MODIFIER_KEYWORDS.contains(&name)
}
