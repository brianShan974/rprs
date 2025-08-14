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

pub fn generate_random_identifier() -> String {
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
        generate_random_identifier()
    } else {
        name
    }
}

pub fn is_reserved_keyword(name: &str) -> bool {
    HARD_KEYWORDS.contains(&name)
        || SOFT_KEYWORDS.contains(&name)
        || MODIFIER_KEYWORDS.contains(&name)
}
