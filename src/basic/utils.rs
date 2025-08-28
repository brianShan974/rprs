use rand::{Rng, SeedableRng};

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

pub fn generate_random_identifier<T: Rng + SeedableRng>(rng: &mut T) -> String {
    // First character: letter or underscore
    let first_char = LETTERS
        .chars()
        .nth(rng.random_range(0..LETTERS.len()))
        .unwrap();

    // Remaining characters: letters, digits or underscores
    let remaining_chars: String = (0..rng.random_range(0..10))
        .map(|_| {
            LETTERS_AND_DIGITS
                .chars()
                .nth(rng.random_range(0..LETTERS_AND_DIGITS.len()))
                .unwrap()
        })
        .collect();

    let name = format!("{first_char}{remaining_chars}");

    // Check if it's a reserved keyword (extensible list)
    if is_reserved_keyword(&name) {
        generate_random_identifier(rng)
    } else {
        name
    }
}

pub fn is_reserved_keyword(name: &str) -> bool {
    HARD_KEYWORDS.contains(&name)
        || SOFT_KEYWORDS.contains(&name)
        || MODIFIER_KEYWORDS.contains(&name)
}

pub fn generate_unique_identifier<T: Rng + SeedableRng>(
    rng: &mut T,
    existing_names: &[String],
) -> String {
    let mut attempts = 0;
    const MAX_ATTEMPTS: usize = 100; // Prevent infinite loops

    loop {
        let name = generate_random_identifier(rng);

        // Check if the name already exists
        if !existing_names.contains(&name) {
            return name;
        }

        attempts += 1;
        if attempts >= MAX_ATTEMPTS {
            // If we can't find a unique name after many attempts,
            // append a random number to make it unique
            let random_suffix = rng.random_range(1000..9999);
            return format!("{}_{}", generate_random_identifier(rng), random_suffix);
        }
    }
}
