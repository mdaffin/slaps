/*
 * shlex.rs is part of slaps.
 * Copyright (C) 2020 rd <slaps@megane.space>
 * License: WTFPL
 */
use lazy_static::lazy_static;
use regex::Regex;
use std::borrow::Cow;
use std::borrow::Cow::Borrowed;
use std::error::Error;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::ops::Range;

// From: https://github.com/jimmycuadra/rust-shellwords
lazy_static! {
    static ref MAIN_PATTERN: Regex =
        Regex::new(r#"(?m:\s*(?:([^\s\\'"]+)|'([^']*)'|"((?:[^"\\]|\\.)*)"|(\\.?)|(\S))(\s|\z)?)"#)
            .unwrap();
    static ref ESCAPE_PATTERN: Regex = Regex::new(r#"\\(.)"#).unwrap();
    static ref METACHAR_PATTERN: Regex = Regex::new(r#"\\([$`"\\\n])"#).unwrap();
}

/// Single word, multiple words surrounded by quotes, or an argument with an optional value.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Field<'a> {
    /// Escaped value with any surrounding quotes and escape characters removed.
    ///
    /// Includes the leading dashes for argument names. Try: [`argument_name_and_value`]
    ///
    /// [`argument_name_and_value`]: `Field::argument_name_and_value`
    pub parsed: Cow<'a, str>,
    /// Unescaped value as it appeared in the original input.
    pub original: &'a str,
    /// Position within the original input.
    pub position: Range<usize>,
    /// Field type.
    pub kind: FieldType,
}

impl<'a> Field<'a> {
    #[inline]
    fn new(original: &'a str, offset: usize, parsed: Cow<'a, str>) -> Self {
        let kind = FieldType::from(&parsed);
        Field {
            parsed,
            original,
            position: offset..offset + original.len(),
            kind,
        }
    }

    fn push_str(&mut self, parsed_append: &str, new_original: &'a str) {
        self.parsed.to_mut().push_str(&parsed_append);
        self.kind = FieldType::from(&self.parsed);
        self.original = new_original;
        self.position.end = self.position.start + new_original.len();
    }

    /// Returns true if the [`Field`] is either a short or a long argument.
    ///
    /// # Examples
    /// ```
    /// # use slaps::shlex::{split, Field, FieldType};
    /// let fields = split("--abc -d e -- --fgh").unwrap();
    ///
    /// assert!(fields[0].is_argument());
    /// assert!(fields[1].is_argument());
    /// assert!(!fields[2].is_argument());
    /// assert!(!fields[3].is_argument());
    /// assert!(!fields[4].is_argument()); // after a "--", all arguments are treated as values
    /// ```
    #[allow(clippy::must_use_candidate)]
    pub fn is_argument(&self) -> bool {
        use FieldType::{ArgLong, ArgShort};
        self.kind == ArgShort || self.kind == ArgLong
    }

    /// If the field is an argument, returns a tuple of its name and optional value.
    ///
    /// The name has its leading `--` characters removed.
    ///
    /// The value has to be part of the same field, separated by a `=` character (`--name=value`).
    ///
    /// # Examples
    /// ```
    /// # use slaps::shlex::{split, Field, FieldType};
    /// let fields = split("--a=123 -d zeee").unwrap();
    ///
    /// assert_eq!(fields.len(), 3);
    /// assert!(matches!(fields[0].argument_name_and_value(), (Some("a"), Some("123"))));
    /// assert!(matches!(fields[1].argument_name_and_value(), (Some("d"), None)));
    /// assert!(matches!(fields[2].argument_name_and_value(), (None, None)));
    /// ```
    #[allow(clippy::must_use_candidate)]
    pub fn argument_name_and_value(&self) -> (Option<&str>, Option<&str>) {
        use FieldType::{ArgLong, ArgShort};
        if self.is_argument() {
            let name_offset = match self.kind {
                ArgShort => 1,
                ArgLong => 2,
                _ => unreachable!(),
            };

            if let Some(value_pos) = self.parsed.find('=') {
                let name = Some(&self.parsed[name_offset..value_pos]);
                let value = Some(&self.parsed[value_pos + 1..]);
                return (name, value);
            }
            return (Some(&self.parsed[name_offset..]), None);
        }
        (None, None)
    }
}

/// Helper for differentiating between arguments and values when destructuring [`Field`]s.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum FieldType {
    /// Standalone value for an argument, either named or positional.
    Value,
    /// Short argument, with an optional value separated by a `=` character.
    ArgShort,
    /// Long argument, with an optional value separated by a `=` character.
    ArgLong,
    /// Standalone `--` argument without a name.
    ///
    /// Indicates that any `Field`s starting with a `-` past this one will instead be interpreted
    /// as `Value`s.
    ArgDoubleDash,
}

impl<T: AsRef<str>> From<T> for FieldType {
    #[inline]
    fn from(s: T) -> Self {
        if !s.as_ref().starts_with('-') || s.as_ref().starts_with("---") {
            FieldType::Value
        } else if s.as_ref() == "--" {
            FieldType::ArgDoubleDash
        } else if s.as_ref().starts_with("--") {
            FieldType::ArgLong
        } else {
            FieldType::ArgShort
        }
    }
}

/// Error returned by [`split`] when the input has mismatched quote characters.
///
/// Contains the position of the first mismatched quote within the original input.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct MismatchedQuotes(usize);

impl AsRef<usize> for MismatchedQuotes {
    fn as_ref(&self) -> &usize {
        &self.0
    }
}

impl AsMut<usize> for MismatchedQuotes {
    fn as_mut(&mut self) -> &mut usize {
        &mut self.0
    }
}

impl Display for MismatchedQuotes {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Error for MismatchedQuotes {}

impl From<usize> for MismatchedQuotes {
    fn from(pos: usize) -> Self {
        MismatchedQuotes(pos)
    }
}

/// Splits an input line grouping quoted words together into [`Field`]s and removing any escape
/// characters.
///
/// An annotated version of what [`std::env::args`] usually gives us.
///
/// # Errors
/// - [`MismatchedQuotes`] - Parsing the input failed due to mismatched quotes.
///
/// # Examples
/// ```
/// # use slaps::shlex::split;
/// let fields = split(r#"foo "bar\" abc" 'bcd aaa'"#).unwrap();
/// let mismatched_quotes = r#"foo 'bar"#;
/// let mismatched_position = split(mismatched_quotes).unwrap_err();
///
/// assert_eq!(fields.len(), 3);
/// assert_eq!(fields[0].parsed, "foo");
/// assert_eq!(fields[1].parsed, "bar\" abc");
/// assert_eq!(fields[2].parsed, "bcd aaa");
/// assert_eq!(*mismatched_position.as_ref(), 4);
/// assert_eq!(split(&mismatched_quotes[0..4]).unwrap().len(), 1); // this is guaranteed to work.
/// ```
pub fn split(input: &str) -> Result<Vec<Field>, MismatchedQuotes> {
    let mut fields = Vec::with_capacity(input.split_whitespace().count());
    if fields.capacity() == 0 {
        return Ok(fields);
    }

    let mut new_field: Option<Field> = None;
    let mut past_double_dash = false;

    for capture in MAIN_PATTERN.captures_iter(input) {
        if let Some(word) = capture.get(1) {
            if let Some(field) = &mut new_field {
                field.push_str(&word.as_str(), &input[field.position.start..word.end()]);
            } else {
                new_field = Some(Field::new(
                    word.as_str(),
                    word.start(),
                    Borrowed(word.as_str()),
                ));
            }
        } else if let Some(single_quoted_word) = capture.get(2) {
            if let Some(field) = &mut new_field {
                field.push_str(
                    &single_quoted_word.as_str(),
                    &input[field.position.start..=single_quoted_word.end()],
                );
            } else {
                let range = single_quoted_word.start() - 1..=single_quoted_word.end();
                let start = *range.start();
                new_field = Some(Field::new(
                    &input[range],
                    start,
                    Borrowed(single_quoted_word.as_str()),
                ));
            }
        } else if let Some(double_quoted_word) = capture.get(3) {
            let escaped = METACHAR_PATTERN.replace_all(double_quoted_word.as_str(), "$1");
            if let Some(field) = &mut new_field {
                field.push_str(
                    &escaped,
                    &input[field.position.start..=double_quoted_word.end()],
                );
            } else {
                let range = double_quoted_word.start() - 1..=double_quoted_word.end();
                let start = *range.start();
                new_field = Some(Field::new(&input[range.clone()], start, escaped));
            }
        } else if let Some(to_escape) = capture.get(4) {
            let escaped = ESCAPE_PATTERN.replace_all(to_escape.as_str(), "$1");
            if let Some(field) = &mut new_field {
                field.push_str(&escaped, &input[field.position.start..to_escape.end()]);
            } else {
                new_field = Some(Field::new(to_escape.as_str(), to_escape.start(), escaped));
            }
        } else if let Some(mismatched_quote) = capture.get(5) {
            return Err(MismatchedQuotes::from(mismatched_quote.start()));
        }

        if capture.get(6).is_some() {
            if let Some(mut field) = new_field {
                if past_double_dash {
                    field.kind = FieldType::Value;
                } else if field.kind == FieldType::ArgDoubleDash {
                    past_double_dash = true;
                }

                fields.push(field);
                new_field = None;
            }
        }
    }

    Ok(fields)
}

#[cfg(test)]
mod tests {
    use crate::shlex::FieldType::{ArgDoubleDash, ArgLong, ArgShort, Value};
    use crate::shlex::{split, MismatchedQuotes};
    use std::borrow::Cow::{Borrowed, Owned};

    #[test]
    fn splits_simple() {
        let fields = split("foo bar").unwrap();

        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].original, r#"foo"#);
        assert_eq!(fields[0].position.start, 0);
        assert_eq!(fields[0].position.len(), fields[0].original.len());
        assert_eq!(fields[0].kind, Value);
        assert_eq!(fields[0].parsed, Borrowed(r#"foo"#));
        assert!(matches!(fields[0].parsed, Borrowed(_)));
        assert_eq!(fields[1].original, r#"bar"#);
        assert_eq!(fields[1].position.start, fields[0].position.end + 1);
        assert_eq!(fields[1].position.len(), fields[1].original.len());
        assert_eq!(fields[1].kind, Value);
        assert_eq!(fields[1].parsed, Borrowed(r#"bar"#));
        assert!(matches!(fields[1].parsed, Borrowed(_)));
    }

    #[test]
    fn splits_removes_escape() {
        let fields = split(r#"\ foo b\"ar a\\rb"#).unwrap();

        assert_eq!(fields.len(), 3);
        assert_eq!(fields[0].original, r#"\ foo"#);
        assert_eq!(fields[0].position.start, 0);
        assert_eq!(fields[0].position.len(), fields[0].original.len());
        assert_eq!(fields[0].kind, Value);
        assert_eq!(fields[0].parsed, Borrowed(r#" foo"#));
        assert!(matches!(fields[0].parsed, Owned(_)));
        assert_eq!(fields[1].original, r#"b\"ar"#);
        assert_eq!(fields[1].position.start, fields[0].position.end + 1);
        assert_eq!(fields[1].position.len(), fields[1].original.len());
        assert_eq!(fields[1].kind, Value);
        assert_eq!(fields[1].parsed, Borrowed(r#"b"ar"#));
        assert!(matches!(fields[1].parsed, Owned(_)));
        assert_eq!(fields[2].original, r#"a\\rb"#);
        assert_eq!(fields[2].position.start, fields[1].position.end + 1);
        assert_eq!(fields[2].position.len(), fields[2].original.len());
        assert_eq!(fields[2].kind, Value);
        assert_eq!(fields[2].parsed, Borrowed(r#"a\rb"#));
        assert!(matches!(fields[2].parsed, Owned(_)));
    }

    #[test]
    fn splits_single_quoted() {
        let fields = split("'f  oo' b'a r' 'a r'b").unwrap();

        assert_eq!(fields.len(), 3);
        assert_eq!(fields[0].original, r#"'f  oo'"#);
        assert_eq!(fields[0].position.start, 0);
        assert_eq!(fields[0].position.len(), fields[0].original.len());
        assert_eq!(fields[0].kind, Value);
        assert_eq!(fields[0].parsed, Borrowed(r#"f  oo"#));
        assert!(matches!(fields[0].parsed, Borrowed(_)));
        assert_eq!(fields[1].original, r#"b'a r'"#);
        assert_eq!(fields[1].position.start, fields[0].position.end + 1);
        assert_eq!(fields[1].position.len(), fields[1].original.len());
        assert_eq!(fields[1].kind, Value);
        assert_eq!(fields[1].parsed, Borrowed(r#"ba r"#));
        assert!(matches!(fields[1].parsed, Owned(_)));
        assert_eq!(fields[2].original, r#"'a r'b"#);
        assert_eq!(fields[2].position.start, fields[1].position.end + 1);
        assert_eq!(fields[2].position.len(), fields[2].original.len());
        assert_eq!(fields[2].kind, Value);
        assert_eq!(fields[2].parsed, Borrowed(r#"a rb"#));
        assert!(matches!(fields[2].parsed, Owned(_)));
    }

    #[test]
    fn splits_double_quoted() {
        let fields = split(r#""f  oo" b"a r" "a r"b"#).unwrap();

        assert_eq!(fields.len(), 3);
        assert_eq!(fields[0].original, r#""f  oo""#);
        assert_eq!(fields[0].position.start, 0);
        assert_eq!(fields[0].position.len(), fields[0].original.len());
        assert_eq!(fields[0].kind, Value);
        assert_eq!(fields[0].parsed, Borrowed(r#"f  oo"#));
        assert!(matches!(fields[0].parsed, Borrowed(_)));
        assert_eq!(fields[1].original, r#"b"a r""#);
        assert_eq!(fields[1].position.start, fields[0].position.end + 1);
        assert_eq!(fields[1].position.len(), fields[1].original.len());
        assert_eq!(fields[1].kind, Value);
        assert_eq!(fields[1].parsed, Borrowed(r#"ba r"#));
        assert!(matches!(fields[1].parsed, Owned(_)));
        assert_eq!(fields[2].original, r#""a r"b"#);
        assert_eq!(fields[2].position.start, fields[1].position.end + 1);
        assert_eq!(fields[2].position.len(), fields[2].original.len());
        assert_eq!(fields[2].kind, Value);
        assert_eq!(fields[2].parsed, Borrowed(r#"a rb"#));
        assert!(matches!(fields[2].parsed, Owned(_)));
    }

    #[test]
    fn splits_double_quoted_removes_escape() {
        let fields = split(r#""f oo" b"a\" r" "a r"b"#).unwrap();

        assert_eq!(fields.len(), 3);
        assert_eq!(fields[0].original, r#""f oo""#);
        assert_eq!(fields[0].position.start, 0);
        assert_eq!(fields[0].position.len(), fields[0].original.len());
        assert_eq!(fields[0].kind, Value);
        assert_eq!(fields[0].parsed, Borrowed(r#"f oo"#));
        assert!(matches!(fields[0].parsed, Borrowed(_)));
        assert_eq!(fields[1].original, r#"b"a\" r""#);
        assert_eq!(fields[1].position.start, fields[0].position.end + 1);
        assert_eq!(fields[1].position.len(), fields[1].original.len());
        assert_eq!(fields[1].kind, Value);
        assert_eq!(fields[1].parsed, Borrowed(r#"ba" r"#));
        assert!(matches!(fields[1].parsed, Owned(_)));
        assert_eq!(fields[2].original, r#""a r"b"#);
        assert_eq!(fields[2].position.start, fields[1].position.end + 1);
        assert_eq!(fields[2].position.len(), fields[2].original.len());
        assert_eq!(fields[2].kind, Value);
        assert_eq!(fields[2].parsed, Borrowed(r#"a rb"#));
        assert!(matches!(fields[2].parsed, Owned(_)));
    }

    #[test]
    fn splits_args() {
        let fields = split(r#"'--foo=abc'   "-s" " --long" ---invalid"#).unwrap();

        assert_eq!(fields.len(), 4);
        assert_eq!(fields[0].original, r#"'--foo=abc'"#);
        assert_eq!(fields[0].position.start, 0);
        assert_eq!(fields[0].position.len(), fields[0].original.len());
        assert_eq!(fields[0].kind, ArgLong);
        assert_eq!(fields[0].parsed, Borrowed(r#"--foo=abc"#));
        assert_eq!(fields[0].argument_name_and_value().0, Some("foo"));
        assert_eq!(fields[0].argument_name_and_value().1, Some("abc"));
        assert!(matches!(fields[0].parsed, Borrowed(_)));
        assert_eq!(fields[1].original, r#""-s""#);
        assert_eq!(fields[1].position.start, fields[0].position.end + 3);
        assert_eq!(fields[1].position.len(), fields[1].original.len());
        assert_eq!(fields[1].kind, ArgShort);
        assert_eq!(fields[1].parsed, Borrowed(r#"-s"#));
        assert!(matches!(fields[1].parsed, Borrowed(_)));
        assert_eq!(fields[2].original, r#"" --long""#);
        assert_eq!(fields[2].position.start, fields[1].position.end + 1);
        assert_eq!(fields[2].position.len(), fields[2].original.len());
        assert_eq!(fields[2].kind, Value);
        assert_eq!(fields[2].parsed, Borrowed(r#" --long"#));
        assert!(matches!(fields[2].parsed, Borrowed(_)));
        assert_eq!(fields[3].original, r#"---invalid"#);
        assert_eq!(fields[3].position.start, fields[2].position.end + 1);
        assert_eq!(fields[3].position.len(), fields[3].original.len());
        assert_eq!(fields[3].kind, Value);
        assert_eq!(fields[3].parsed, Borrowed(r#"---invalid"#));
        assert!(matches!(fields[3].parsed, Borrowed(_)));
    }

    #[test]
    fn splits_mismatched_quotes() {
        assert_eq!(split(r#"foo    ' aaa"#).unwrap_err().0, 7);
        assert_eq!(split(r#"foo    " aaa"#).unwrap_err().0, 7);
        assert_eq!(split(r#"foo'   '  " aaaa"#).unwrap_err().0, 10);
        assert_eq!(split(r#"foo"   "  '     "#).unwrap_err().0, 10);

        let mut mismatched = MismatchedQuotes(10);
        assert_eq!(format!("{}", mismatched), format!("{}", 10 as usize));

        assert_eq!(mismatched.0, 10);
        *mismatched.as_mut() += 10;
        assert_eq!(mismatched.0, 20);
        assert_eq!(*mismatched.as_ref(), 20);
        assert_eq!(MismatchedQuotes(20), MismatchedQuotes(20));
        assert_ne!(MismatchedQuotes(20), MismatchedQuotes(10));
        assert!(MismatchedQuotes(20) > MismatchedQuotes(10));
        assert!(MismatchedQuotes(10) < MismatchedQuotes(20));
    }

    #[test]
    fn splits_double_dash() {
        let fields = split(r#"--foo '--'  -z   --aaa"#).unwrap();

        assert_eq!(fields.len(), 4);
        assert_eq!(fields[0].original, r#"--foo"#);
        assert_eq!(fields[0].position.start, 0);
        assert_eq!(fields[0].position.len(), fields[0].original.len());
        assert_eq!(fields[0].kind, ArgLong);
        assert_eq!(fields[0].parsed, Borrowed(r#"--foo"#));
        assert!(matches!(fields[0].parsed, Borrowed(_)));
        assert_eq!(fields[1].original, r#"'--'"#);
        assert_eq!(fields[1].position.start, fields[0].position.end + 1);
        assert_eq!(fields[1].position.len(), fields[1].original.len());
        assert_eq!(fields[1].kind, ArgDoubleDash);
        assert_eq!(fields[1].parsed, Borrowed(r#"--"#));
        assert!(matches!(fields[1].parsed, Borrowed(_)));
        assert_eq!(fields[2].original, r#"-z"#);
        assert_eq!(fields[2].position.start, fields[1].position.end + 2);
        assert_eq!(fields[2].position.len(), fields[2].original.len());
        assert_eq!(fields[2].kind, Value);
        assert_eq!(fields[2].parsed, Borrowed(r#"-z"#));
        assert!(matches!(fields[2].parsed, Borrowed(_)));
        assert_eq!(fields[3].original, r#"--aaa"#);
        assert_eq!(fields[3].position.start, fields[2].position.end + 3);
        assert_eq!(fields[3].position.len(), fields[3].original.len());
        assert_eq!(fields[3].kind, Value);
        assert_eq!(fields[3].parsed, Borrowed(r#"--aaa"#));
        assert!(matches!(fields[3].parsed, Borrowed(_)));
    }

    #[test]
    fn test_splits_empty() {
        let fields = split(r#""#).unwrap();
        assert_eq!(fields.len(), 0);
        let fields = split(r#"   "#).unwrap();
        assert_eq!(fields.len(), 0);
    }

    #[test]
    fn test_field_equals() {
        let fields = split(r#"--foo --foo --bar  "#).unwrap();
        assert_eq!(fields[0], fields[0]);
        assert_ne!(fields[0], fields[1]);
        assert_ne!(fields[0], fields[2]);
        assert_eq!(fields[1], fields[1]);
        assert_ne!(fields[1], fields[0]);
        assert_ne!(fields[1], fields[2]);
        assert_eq!(fields[2], fields[2]);
        assert_ne!(fields[2], fields[0]);
        assert_ne!(fields[2], fields[1]);
    }
}
