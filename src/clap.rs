/*
 * clap.rs is part of slaps.
 * Copyright (C) 2020 rd <slaps@megane.space>
 * License: WTFPL
 */

use crate::shlex::{split, Field, FieldMatcher, FieldType};
use crate::{Config, ExecutionError, MismatchedQuotes};
use clap::ArgSettings;
pub use clap::{App, AppSettings, ArgMatches, ErrorKind};
use itertools::Itertools;
use rustyline::completion::Completer;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::Context;
use std::borrow::Cow;
use std::borrow::Cow::Borrowed;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::fmt::Write;
use std::fmt::{Debug, Display, Formatter};

// Clap settings suited for interactive usage. Set for all subcommands.
const GLOBAL_CLAP_SETTINGS: &[AppSettings] = &[
    #[cfg(test)]
    AppSettings::ColorNever,
    AppSettings::DisableVersion,
    AppSettings::DisableHelpFlags,
    AppSettings::InferSubcommands,
    AppSettings::SubcommandsNegateReqs,
    AppSettings::VersionlessSubcommands,
];

/// Handler function for a command.
type Handler<'a> = Box<dyn 'a + Fn(&clap::ArgMatches) -> Result<(), ExecutionError> + Send + Sync>;

/// Uses `clap` to parse arguments for registered commands and provide completions.
pub(crate) struct Matcher<'a, 'b> {
    clap: App<'a, 'b>,
    config: Config<'b>,
    handlers: HashMap<String, Handler<'b>>,
}

impl<'a, 'b> Matcher<'a, 'b> {
    /// Initialises clap with settings suitable for interactive usage.
    #[must_use]
    pub fn with_name_and_config(name: &str, config: Config<'b>) -> Self {
        let mut m = Matcher {
            clap: App::new(name)
                .global_settings(GLOBAL_CLAP_SETTINGS)
                .setting(AppSettings::NoBinaryName)
                .template(config.help_template),
            config,
            handlers: HashMap::new(),
        };

        m.register_command_with_handler(
            App::new("quit").alias("exit"),
            Box::new(|_| Err(ExecutionError::GracefulExit)),
        );
        m.clap.p.create_help_and_version();
        m.clap.p.set(AppSettings::DisableHelpSubcommand);

        m
    }

    /// Registers a subcommand with clap.
    #[cfg(test)]
    pub fn register_command(&mut self, mut subcommand: App<'a, 'b>) {
        // Override settings to make suitable for interactive use.
        self.apply_global_settings(&mut subcommand);
        // Register with clap.
        self.clap.p.add_subcommand(subcommand);
    }

    pub fn register_command_with_handler(
        &mut self,
        mut subcommand: App<'a, 'b>,
        handler: Handler<'b>,
    ) {
        self.handlers
            .insert(subcommand.get_name().to_string(), handler);
        // Override settings to make suitable for interactive use.
        self.apply_global_settings(&mut subcommand);
        // Register with clap.
        self.clap.p.add_subcommand(subcommand);
    }

    /// Parses the given word slice using clap to return an `ArgMatches` object.
    pub fn get_matches<T: AsRef<OsStr>>(&mut self, input: &[T]) -> Result<ArgMatches, Error> {
        Ok(self.clap.get_matches_from_safe_borrow(input)?)
    }

    // Parses the given word slice and executes the matched command's handler.
    pub fn handle_matches<T: AsRef<OsStr>>(&mut self, input: &[T]) -> Result<(), crate::Error> {
        let matches = self.clap.get_matches_from_safe_borrow(input)?;
        if let Some(subcommand) = matches.subcommand_name() {
            if let (Some(handler), Some(subcommand_matches)) = (
                self.handlers.get_mut(subcommand),
                matches.subcommand_matches(subcommand),
            ) {
                handler(subcommand_matches)?;
            }
        }
        unreachable!()
    }

    pub fn command_from_input<'c>(
        &self,
        input: &'c [Field<'c>],
    ) -> (&clap::App, Option<&'c Field<'c>>, Option<&str>) {
        let mut cmd = &self.clap;
        let mut cmd_field = None;
        let mut fuzzy_match = None;

        for f in input.iter().take_while(|&f| !f.is_argument()) {
            if let Some((subcmd, fuzzy)) = cmd.p.subcommands.iter().find_map(|c| {
                let name = c.p.meta.name.as_str();
                if name == f.parsed {
                    Some((c, name))
                } else if let Some((alias, _)) =
                    c.p.meta
                        .aliases
                        .iter()
                        .flatten()
                        .find(|(s, _)| *s == f.parsed)
                {
                    Some((c, alias))
                } else {
                    None
                }
            }) {
                cmd = subcmd;
                cmd_field = Some(f);
                fuzzy_match = Some(fuzzy);
                continue;
            } else {
                let (fuzzy_cmd, fuzzy_count) = {
                    let mut i = cmd.p.subcommands.iter().filter_map(|c| {
                        let name = c.p.meta.name.as_str();
                        if name.starts_with(f.parsed.as_ref()) {
                            Some((c, name))
                        } else if let Some((alias, _)) =
                            c.p.meta
                                .aliases
                                .iter()
                                .flatten()
                                .find(|(s, _)| s.starts_with(f.parsed.as_ref()))
                        {
                            Some((c, alias))
                        } else {
                            None
                        }
                    });
                    (i.next(), i.take(1).count() == 0)
                };

                match (fuzzy_cmd, fuzzy_count) {
                    (Some((fuzzy_cmd, s)), true) => {
                        cmd = fuzzy_cmd;
                        cmd_field = Some(f);
                        fuzzy_match = Some(s);
                        continue;
                    }
                    _ => break,
                }
            }
        }

        (cmd, cmd_field, fuzzy_match)
    }

    /// Recursively applies global settings to all subcommands of a command.
    fn apply_global_settings(&self, command: &mut clap::App<'a, 'b>) {
        GLOBAL_CLAP_SETTINGS.iter().for_each(|&s| command.p.set(s));

        if command.p.has_subcommands() || command.p.has_args() {
            command.p.meta.template = Some(self.config.subcommand_help_template);
        } else {
            command.p.meta.template = Some(self.config.subcommand_help_template_no_args);
        }

        command.p.create_help_and_version();
        command.p.set(AppSettings::DisableHelpSubcommand);

        if command.p.has_subcommands() {
            command
                .p
                .subcommands
                .iter_mut()
                .for_each(|c| self.apply_global_settings(c))
        }
    }

    fn complete_fields(&self, line: &[Field], pos: usize) -> (usize, Vec<String>) {
        // Split off fields before, at and after cursor.
        let (before_cursor, at_cursor, _) = line.split_at_pos(pos);

        // If cursor past a double dash, short circuit.
        if before_cursor.has_double_dash() {
            return (1, Vec::with_capacity(0));
        }

        // If cursor inside a quoted string, short circuit
        if at_cursor.iter().any(|&f| f.is_quoted()) {
            return (2, Vec::with_capacity(0));
        }

        // Enable special handling for the help command.
        let help_field = before_cursor.match_first("help");

        // Get current subcommand for input.
        // Don't complete anything else for the help command.
        let (subcommand, subcommand_field, subcommand_fuzzy) = if help_field.is_some() {
            self.command_from_input(&line[1..])
        } else {
            self.command_from_input(&line)
        };

        // Split off words after subcommand
        let after_subcommand = if let Some(sf) = subcommand_field {
            if pos <= sf.position.start {
                // Cursor before current subcommand, short circuit.
                return (3, Vec::with_capacity(0));
            } else if Some(sf) == at_cursor {
                // Cursor at current subcommand, complete fuzzy match.
                return (
                    sf.position.start,
                    vec![String::from(subcommand_fuzzy.unwrap())],
                );
            }

            let (_, _, after) = line.split_at_pos(sf.position.end);
            after
        } else {
            &line
        };

        if subcommand.p.has_subcommands() {
            // If no fields after subcommand, or partial subcommand under cursor as first field,
            // suggest subcommands.
            if after_subcommand.first() == at_cursor {
                return if subcommand_field.is_none() && at_cursor.is_some() {
                    // Completing argument/option for unknown command
                    (5, Vec::with_capacity(0))
                } else {
                    // Complete empty subcommand.
                    let completions = get_subcommands_names(subcommand).collect();

                    let insert_pos = match (help_field, subcommand_field) {
                        (Some(help), None) => help.position.end + 1,
                        (_, Some(sub)) => sub.position.end + 1,
                        _ => 0,
                    };

                    (insert_pos, completions)
                };
            }
        }

        // Don't complete anything else for the help command.
        if help_field.is_some() {
            return (6, Vec::with_capacity(0));
        }

        // Try to auto-complete word under cursor. arguments > options
        if let Some(arg_at_cursor) = at_cursor.filter(|&f| f.is_argument() || f.parsed == "-") {
            if arg_at_cursor.kind == FieldType::ArgShort
                && !arg_at_cursor
                    .argument_name_and_value()
                    .0
                    .unwrap_or_default()
                    .is_empty()
            {
                // Complete short arg.
                return (7, Vec::with_capacity(0));
            }

            let completions =
                get_subcommand_options(subcommand, Some(arg_at_cursor), after_subcommand, false)
                    .chain(get_subcommand_flags(
                        subcommand,
                        Some(arg_at_cursor),
                        after_subcommand,
                    ))
                    .collect();

            return (arg_at_cursor.position.start, completions);
        }

        if let Some(last_before_cursor) = before_cursor.last() {
            if get_subcommand_option_value_expected(subcommand, last_before_cursor) {
                return (8, Vec::with_capacity(0));
            } else if at_cursor.is_none() {
                let completions =
                    get_subcommand_options(subcommand, None, after_subcommand, true).collect();
                return (last_before_cursor.position.end + 1, completions);
            }
        }

        (9, Vec::with_capacity(0))
    }
}

impl Debug for Matcher<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "clap::Matcher {{name = {:?}, config = {:?}, commands = {:?}}}",
            self.clap.get_name(),
            self.config,
            self.handlers.keys()
        )
    }
}

impl Completer for Matcher<'_, '_> {
    type Candidate = String;

    #[allow(clippy::too_many_lines)]
    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> Result<(usize, Vec<Self::Candidate>), rustyline::error::ReadlineError> {
        // Parse entire input into words.
        // If there were mismatched quotes, short-circuit.
        let fields = match split(line) {
            Ok(f) => f,
            Err(MismatchedQuotes(quotes_pos)) => {
                return if pos == line.len() {
                    let candidate = line.chars().nth(quotes_pos).unwrap().to_string();
                    Ok((pos, vec![candidate]))
                } else {
                    Ok((0, Vec::with_capacity(0)))
                }
            }
        };

        Ok(self.complete_fields(&fields, pos))
    }
}

impl Hinter for Matcher<'_, '_> {
    fn hint(&self, line: &str, pos: usize, _ctx: &Context<'_>) -> Option<String> {
        // Don't show hints if the cursor is not at the end of the string.
        if pos != line.len() {
            return None;
        }

        // Try to auto-complete. If only one completion available, display hint.
        match split(line) {
            Ok(fields) => {
                let (_, completions) = self.complete_fields(&fields, pos);
                if completions.len() == 1 {
                    return if let (_, Some(at_cursor), _) = fields.split_at_pos(pos) {
                        Some(completions[0][at_cursor.parsed.len()..].to_string())
                    } else {
                        Some(completions.into_iter().next().unwrap())
                    };
                }
            }
            Err(MismatchedQuotes(quotes_pos)) => {
                return Some(line.chars().nth(quotes_pos).unwrap().to_string());
            }
        }
        None
    }
}

impl Highlighter for Matcher<'_, '_> {
    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        let (line, fields) = match split(&line) {
            Ok(f) => (Borrowed(line), f),
            Err(MismatchedQuotes(quote_pos)) => (
                {
                    let quote_char = line.chars().nth(quote_pos).unwrap().to_string();
                    let (quote_style, value_style) =
                        if self.config.highlighter_underline_word_under_cursor && pos > quote_pos {
                            (
                                self.config.highlighter_style_mismatched_quotes.underline(),
                                self.config.highlighter_style_value.underline(),
                            )
                        } else {
                            (
                                self.config.highlighter_style_mismatched_quotes,
                                self.config.highlighter_style_value,
                            )
                        };
                    Cow::from(format!(
                        "{}{}{}",
                        &line[..quote_pos],
                        &quote_style.paint(&quote_char),
                        &value_style.paint(&line[quote_pos + 1..])
                    ))
                },
                split(&line[..quote_pos]).unwrap(),
            ),
        };

        let (_, subcommand_word, _) = self.command_from_input(&fields);
        let (_, at_cursor, _) = fields.split_at_pos(pos);
        let mut highlighted = String::new();
        let mut highlighted_pos = 0;

        for field in &fields {
            // Insert whitespace.
            while highlighted_pos < field.position.start {
                highlighted.push(' ');
                highlighted_pos += 1;
            }
            let mut color = None;

            // Highlight commands.
            if let Some(subcommand_word) = subcommand_word {
                if subcommand_word.position.start >= field.position.start {
                    color = Some(self.config.highlighter_style_command);
                }
            }
            // Highlight arguments.
            if color.is_none() && field.is_argument() {
                if let Some(equals_pos) = field.original.find('=') {
                    let (before_equals, after_equals) = field.original.split_at(equals_pos);
                    let (arg_style, value_style) =
                        if self.config.highlighter_underline_word_under_cursor
                            && Some(field) == at_cursor
                        {
                            (
                                self.config.highlighter_style_argument.underline(),
                                self.config.highlighter_style_value.underline(),
                            )
                        } else {
                            (
                                self.config.highlighter_style_argument,
                                self.config.highlighter_style_value,
                            )
                        };
                    write!(
                        highlighted,
                        "{}{}",
                        arg_style.paint(before_equals),
                        value_style.paint(after_equals)
                    )
                    .unwrap();
                    highlighted_pos += field.original.len();
                    continue;
                } else {
                    color = Some(self.config.highlighter_style_argument);
                }
            }
            // Highlight values
            if color.is_none() {
                color = Some(self.config.highlighter_style_value);
            }

            // Underline word at cursor.
            if self.config.highlighter_underline_word_under_cursor && Some(field) == at_cursor {
                color = Some(color.unwrap().underline());
            }
            write!(highlighted, "{}", color.unwrap().paint(field.original)).unwrap();
            highlighted_pos += field.original.len();
        }

        if highlighted.is_empty() {
            line
        } else {
            // Add remaining un-highlighted substring.
            highlighted.push_str(&line[highlighted_pos..]);
            Cow::from(highlighted)
        }
    }
}

fn get_subcommands_names(app: &clap::App) -> impl Iterator<Item = String> {
    app.p
        .subcommands
        .iter()
        .map(|s| s.p.meta.name.clone())
        .sorted_by(|a, b| {
            if b == "quit" || b == "help" {
                Ordering::Less
            } else if a == "help" {
                Ordering::Greater
            } else {
                a.cmp(&b)
            }
        })
}

fn get_subcommand_flags<'a>(
    app: &'a clap::App,
    current: Option<&Field>,
    missing: &[Field],
) -> impl Iterator<Item = String> + 'a {
    app.p
        .flags
        .iter()
        .filter(|a| {
            (match (a.s.short, a.s.long, current) {
                (None, None, _) => false,
                (_, _, None) => true,
                (_, l, Some(f)) => match (l, f.kind) {
                    (_, FieldType::ArgShort) => f.argument_name_and_value().0.unwrap().is_empty(),
                    (Some(l), FieldType::ArgLong) => {
                        l.starts_with(f.argument_name_and_value().0.unwrap())
                    }
                    (Some(_), FieldType::ArgDoubleDash) => true,
                    _ => false,
                },
            }) && (a.b.is_set(ArgSettings::Multiple)
                || !missing.iter().any(|f| {
                    match (f.kind, f.argument_name_and_value(), a.s.short, a.s.long) {
                        (FieldType::ArgShort, (Some(n), _), Some(s), _) => {
                            !n.is_empty() && n.chars().next().unwrap() == s
                        }
                        (FieldType::ArgLong, (Some(n), _), _, Some(l)) => l == n,
                        _ => false,
                    }
                }))
        })
        .sorted_by(|a, b| {
            if !b.b.is_set(ArgSettings::Multiple) && a.b.is_set(ArgSettings::Multiple) {
                Ordering::Greater
            } else {
                match (a.s.short, a.s.long) {
                    (_, Some(l)) => match (b.s.short, b.s.long) {
                        (_, Some(b)) => l.cmp(b),
                        //(Some(_), _) => unreachable!(),
                        _ => unreachable!(),
                    },
                    (Some(s), _) => match (b.s.short, b.s.long) {
                        (_, Some(_)) => Ordering::Greater,
                        (Some(ref b), _) => s.cmp(b),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }
            }
        })
        .map(move |a| match (a.s.short, a.s.long) {
            (_, Some(l)) => format!("--{}", l),
            (Some(s), _) => format!("-{}", s),
            _ => unreachable!(),
        })
}

fn get_subcommand_option_value_expected(app: &clap::App, before_cursor: &Field) -> bool {
    if !before_cursor.is_argument() {
        return false;
    }

    app.p.opts.iter().any(|a| {
        match (
            before_cursor.kind,
            before_cursor.argument_name_and_value(),
            a.s.short,
            a.s.long,
        ) {
            (FieldType::ArgShort, (Some(n), _), Some(s), _) => n.chars().next().unwrap() == s,
            (FieldType::ArgLong, (Some(n), _), _, Some(l)) => l == n,
            _ => unreachable!(),
        }
    })
}

fn get_subcommand_options<'a>(
    app: &'a clap::App,
    current: Option<&Field>,
    missing: &[Field],
    required: bool,
) -> impl Iterator<Item = String> + 'a {
    app.p
        .opts
        .iter()
        .filter(|a| {
            (match (a.s.short, a.s.long, current) {
                (None, None, _) => false,
                (_, _, None) => true,
                (_, l, Some(f)) => match (l, f.kind) {
                    (_, FieldType::ArgShort) => f.argument_name_and_value().0.unwrap().is_empty(),
                    (Some(l), FieldType::ArgLong) => {
                        l.starts_with(f.argument_name_and_value().0.unwrap())
                    }
                    (Some(_), FieldType::ArgDoubleDash) => true,
                    _ => unreachable!(),
                },
            }) && (!required || a.b.is_set(ArgSettings::Required))
                && (a.b.is_set(ArgSettings::Multiple)
                    || !missing.iter().any(|f| {
                        match (f.kind, f.argument_name_and_value(), a.s.short, a.s.long) {
                            (FieldType::ArgShort, (Some(n), _), Some(s), _) => {
                                !n.is_empty() && n.chars().next().unwrap() == s
                            }
                            (FieldType::ArgLong, (Some(n), _), _, Some(l)) => l == n,
                            _ => false,
                        }
                    }))
        })
        .sorted_by(|a, b| {
            if !b.b.is_set(ArgSettings::Multiple) && a.b.is_set(ArgSettings::Multiple)
                || b.b.is_set(ArgSettings::Required) && !a.b.is_set(ArgSettings::Required)
            {
                Ordering::Greater
            } else {
                match (a.s.short, a.s.long) {
                    (_, Some(l)) => match (b.s.short, b.s.long) {
                        (_, Some(b)) => l.cmp(b),
                        //(Some(_), _) => unreachable!()
                        _ => unreachable!(),
                    },
                    (Some(s), _) => match (b.s.short, b.s.long) {
                        (_, Some(_)) => Ordering::Greater,
                        (Some(ref b), _) => s.cmp(b),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }
            }
        })
        .map(move |a| match (a.s.short, a.s.long) {
            (_, Some(l)) => format!("--{}", l),
            (Some(s), _) => format!("-{}", s),
            _ => unreachable!(),
        })
}

/// Command Line Argument Parser Error
#[derive(Debug, Clone)]
pub struct Error {
    /// The type of error
    pub kind: ErrorKind,
    /// Formatted error message.
    pub message: String,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for Error {}

impl From<clap::Error> for crate::Error {
    fn from(err: clap::Error) -> Self {
        Self::ClapError(Error::from(err))
    }
}

impl From<clap::Error> for Error {
    fn from(err: clap::Error) -> Self {
        use ErrorKind::{
            ArgumentConflict, ArgumentNotFound, EmptyValue, Format, HelpDisplayed,
            InvalidSubcommand, InvalidUtf8, InvalidValue, Io, MissingArgumentOrSubcommand,
            MissingRequiredArgument, MissingSubcommand, TooFewValues, TooManyValues,
            UnexpectedMultipleUsage, UnknownArgument, UnrecognizedSubcommand, ValueValidation,
            VersionDisplayed, WrongNumberOfValues,
        };

        let clap::Error {
            kind, mut message, ..
        } = err;

        // Make the messages a little less verbose...
        if let Some((trim_at, _)) = match kind {
            MissingRequiredArgument | InvalidValue => {
                message = message
                    .lines()
                    .take_while(|&l| !l.is_empty())
                    .map(str::trim)
                    .join(" ");
                None
            }
            InvalidSubcommand => {
                message = message
                    .lines()
                    .take_while(|&l| !l.is_empty())
                    .map(str::trim)
                    .join(". ");
                None
            }
            UnknownArgument
            | UnrecognizedSubcommand
            | EmptyValue
            | ValueValidation
            | TooManyValues
            | WrongNumberOfValues
            | TooFewValues
            | ArgumentConflict
            | UnexpectedMultipleUsage
            | MissingSubcommand => message.match_indices('\n').next(),
            HelpDisplayed
            | VersionDisplayed
            | MissingArgumentOrSubcommand
            | ArgumentNotFound
            | InvalidUtf8
            | Io
            | Format => None,
        } {
            message.truncate(trim_at);
        }

        Error { kind, message }
    }
}

#[cfg(test)]
mod tests {
    use crate::clap::{Matcher, GLOBAL_CLAP_SETTINGS};
    use crate::shlex::split;
    use crate::{Config, ExecutionError};
    use clap::{App, AppSettings, Arg, ErrorKind, SubCommand};
    use rustyline::completion::Completer;
    use rustyline::highlight::Highlighter;
    use rustyline::hint::Hinter;
    use rustyline::history::History;
    use rustyline::Context;
    use std::borrow::Cow;
    use std::ffi::OsStr;

    fn get_test_app() -> App<'static, 'static> {
        App::new("slap")
            .subcommand(App::new("that").arg(Arg::with_name("arg").long("arg")))
            .subcommand(App::new("this"))
    }

    fn get_test_matcher() -> Matcher<'static, 'static> {
        let mut matcher = Matcher::with_name_and_config("slaps", Config::default());
        matcher.register_command(get_test_app());
        matcher
    }

    #[test]
    fn test_sets_clap_settings() {
        let config = Config::default();
        let matcher = Matcher::with_name_and_config("slaps", config);
        let global_settings = matcher.clap.p.g_settings;

        assert_eq!(matcher.clap.get_name(), "slaps");
        assert_eq!(matcher.clap.p.meta.template, Some(config.help_template));

        GLOBAL_CLAP_SETTINGS
            .iter()
            .for_each(|&s| assert!(global_settings.is_set(s)));
        assert!(matcher.clap.p.is_set(AppSettings::NoBinaryName));
    }

    #[test]
    fn test_debug_prints_name() {
        let config = Config::default();
        let matcher = Matcher::with_name_and_config("slaps", config);

        assert!(format!("{:?}", matcher).starts_with(r#"clap::Matcher {name = "slaps""#),);
    }

    #[test]
    fn test_applies_global_settings_recursively() {
        let config = Config::default();
        let subcommand = get_test_app();

        let matcher = {
            let mut matcher = Matcher::with_name_and_config("slaps", config);
            matcher.register_command(subcommand);
            matcher
        };

        GLOBAL_CLAP_SETTINGS.iter().for_each(|&s| {
            assert!(matcher.clap.p.subcommands[2].p.is_set(s));
            assert!(matcher.clap.p.subcommands[2].p.subcommands[0].p.is_set(s));
            assert!(matcher.clap.p.subcommands[2].p.subcommands[1].p.is_set(s));
        });

        assert_eq!(
            matcher.clap.p.subcommands[2].p.meta.template,
            Some(config.subcommand_help_template)
        );

        assert_eq!(
            matcher.clap.p.subcommands[2].p.subcommands[0]
                .p
                .meta
                .template,
            Some(config.subcommand_help_template)
        );
        assert_eq!(
            matcher.clap.p.subcommands[2].p.subcommands[1]
                .p
                .meta
                .template,
            Some(config.subcommand_help_template_no_args)
        );
    }

    #[test]
    fn test_get_matches_from_words() {
        let words = split("slap that --arg").unwrap();
        let mut matcher = get_test_matcher();
        let m = matcher.get_matches(&words).unwrap();

        assert_eq!(m.subcommand_name(), Some("slap"));
        assert_eq!(
            m.subcommand_matches("slap").unwrap().subcommand_name(),
            Some("that")
        );
        assert_eq!(
            m.subcommand_matches("slap")
                .unwrap()
                .subcommand_matches("that")
                .unwrap()
                .occurrences_of("arg"),
            1
        );
    }

    #[allow(clippy::redundant_clone)]
    fn test_clap_error_helper<T: AsRef<OsStr>>(
        app: clap::App,
        input: &[T],
        expected_kind: ErrorKind,
        expected_message: Option<&'static str>,
    ) {
        let mut matcher = get_test_matcher();
        matcher.register_command(app);
        let result = matcher.get_matches(&input).unwrap_err();

        let expected_message: Cow<'static, str> = match expected_message {
            Some(s) => s.into(),
            None => matcher
                .clap
                .get_matches_from_safe_borrow(&*input)
                .unwrap_err()
                .message
                .into(),
        };

        assert_eq!(result.clone().kind, expected_kind);
        assert_eq!(format!("{}", result.clone()).as_str(), &expected_message);
    }

    #[test]
    fn test_clap_error_missing_required_argument() {
        test_clap_error_helper(
            clap::App::new("prog").arg(Arg::with_name("debug").required(true)),
            &split("prog").unwrap(),
            ErrorKind::MissingRequiredArgument,
            Some("error: The following required arguments were not provided: <debug>"),
        );
    }

    #[test]
    fn test_clap_error_invalid_value() {
        test_clap_error_helper(
            App::new("prog").arg(
                Arg::with_name("speed")
                    .possible_value("fast")
                    .possible_value("slow"),
            ),
            &split("prog other").unwrap(),
            ErrorKind::InvalidValue,
            Some("error: 'other' isn't a valid value for '<speed>' [possible values: fast, slow]"),
        );
    }

    #[test]
    fn test_clap_error_invalid_subcommand() {
        test_clap_error_helper(
            App::new("prog").subcommand(
                SubCommand::with_name("config")
                    .about("Used for configuration")
                    .arg(
                        Arg::with_name("config_file")
                            .help("The configuration file to use")
                            .index(1),
                    ),
            ),
            &split("prog confug").unwrap(),
            ErrorKind::InvalidSubcommand,
            Some("error: The subcommand 'confug' wasn't recognized. Did you mean 'config'?"),
        );
    }

    #[test]
    fn test_clap_error_unknown_argument() {
        test_clap_error_helper(
            App::new("prog").arg(Arg::from_usage("--flag 'some flag'")),
            &split("prog --other").unwrap(),
            ErrorKind::UnknownArgument,
            Some("error: Found argument '--other' which wasn't expected, or isn't valid in this context"),
        );
    }

    #[test]
    fn test_clap_error_unrecognized_subcommand() {
        test_clap_error_helper(
            App::new("prog").subcommand(
                SubCommand::with_name("config")
                    .about("Used for configuration")
                    .arg(
                        Arg::with_name("config_file")
                            .help("The configuration file to use")
                            .index(1),
                    ),
            ),
            &split("prog help nothing").unwrap(),
            ErrorKind::UnrecognizedSubcommand,
            Some("error: The subcommand 'nothing' wasn't recognized"),
        );
    }

    #[test]
    fn test_clap_error_empty_value() {
        test_clap_error_helper(
            App::new("prog").arg(Arg::with_name("color").long("color").empty_values(false)),
            &split("prog --color").unwrap(),
            ErrorKind::EmptyValue,
            Some("error: The argument '--color <color>' requires a value but none was supplied"),
        );
    }

    #[test]
    fn test_clap_error_value_validation() {
        #[allow(clippy::needless_pass_by_value)]
        fn is_numeric(val: String) -> Result<(), String> {
            match val.parse::<i64>() {
                Ok(..) => Ok(()),
                Err(..) => Err(String::from("Value wasn't a number!")),
            }
        }
        test_clap_error_helper(
            App::new("prog").arg(Arg::with_name("num").validator(is_numeric)),
            &split("prog NotANumber").unwrap(),
            ErrorKind::ValueValidation,
            Some("error: Invalid value for '<num>': Value wasn't a number!"),
        );
    }

    #[test]
    fn test_clap_error_too_many_values() {
        test_clap_error_helper(
            App::new("prog").arg(Arg::with_name("arg").multiple(true).max_values(2)),
            &split("prog too many values").unwrap(),
            ErrorKind::TooManyValues,
            Some("error: The value 'values' was provided to '<arg>...', but it wasn't expecting any more values"),
        );
    }

    #[test]
    fn test_clap_error_too_few_values() {
        test_clap_error_helper(
            App::new("prog").arg(Arg::with_name("some_opt").long("opt").min_values(3)),
            &split("prog --opt too few").unwrap(),
            ErrorKind::TooFewValues,
            Some("error: The argument '--opt <some_opt>' requires at least 3 values, but only 2 were provided"),
        );
    }

    #[test]
    fn test_clap_error_argument_conflict() {
        test_clap_error_helper(
            App::new("prog")
                .arg(
                    Arg::with_name("debug")
                        .long("debug")
                        .conflicts_with("color"),
                )
                .arg(Arg::with_name("color").long("color")),
            &split("prog --debug --color").unwrap(),
            ErrorKind::ArgumentConflict,
            Some("error: The argument '--color' cannot be used with '--debug'"),
        );
    }

    #[test]
    fn test_clap_error_subcommand_required() {
        test_clap_error_helper(
            clap::App::new("prog")
                .setting(clap::AppSettings::SubcommandRequired)
                .subcommand(clap::SubCommand::with_name("test")),
            &split("prog").unwrap(),
            ErrorKind::MissingSubcommand,
            Some("error: 'prog' requires a subcommand, but one was not provided"),
        );
    }

    #[test]
    fn test_clap_error_missing_argument_or_subcommand() {
        test_clap_error_helper(
            clap::App::new("prog")
                .about("test")
                .setting(clap::AppSettings::ArgRequiredElseHelp)
                .subcommand(
                    clap::SubCommand::with_name("config")
                        .about("Used for configuration")
                        .arg(Arg::with_name("config_file").help("The configuration file to use")),
                ),
            &split("prog").unwrap(),
            ErrorKind::MissingArgumentOrSubcommand,
            None,
        );
    }

    #[test]
    fn test_clap_error_unexpected_multiple_usage() {
        test_clap_error_helper(
            clap::App::new("prog").arg(Arg::with_name("debug").long("debug").multiple(false)),
            &split("prog --debug --debug").unwrap(),
            ErrorKind::UnexpectedMultipleUsage,
            Some("error: The argument '--debug' was provided more than once, but cannot be used multiple times"),
        );
    }

    #[test]
    fn test_clap_error_help_displayed() {
        test_clap_error_helper(
            clap::App::new("prog"),
            &split("help prog").unwrap(),
            ErrorKind::HelpDisplayed,
            None,
        );
    }

    #[test]
    fn test_clap_completion_short_circuits_on_mismatched_quotes() {
        let app = clap::App::new("prog");
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("pr \"", 2, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 0);
        assert_eq!(result.1.capacity(), 0);
    }

    #[test]
    fn test_clap_completion_matches_mismatched_double_quotes() {
        let app = clap::App::new("prog");
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("pr \"", 4, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 4);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "\"");
    }

    #[test]
    fn test_clap_completion_matches_mismatched_single_quotes() {
        let app = clap::App::new("prog");
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("pr '", 4, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 4);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "'");
    }

    #[test]
    fn test_clap_completion_short_circuits_on_double_dash() {
        let app = clap::App::new("prog");
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("-- pr", 5, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 1);
        assert_eq!(result.1.capacity(), 0);
    }

    #[test]
    fn test_clap_completion_short_circuits_inside_quotes() {
        let app = clap::App::new("prog");
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("\"pr\"", 3, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 2);
        assert_eq!(result.1.capacity(), 0);
    }

    #[test]
    fn test_clap_completion_match_help_subcommand() {
        let app = clap::App::new("prog");
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("help pr", 7, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 5);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "prog");
    }

    #[test]
    fn test_clap_completion_short_circuits_cursor_before_subcommand() {
        let app = clap::App::new("prog");
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("prog", 0, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 3);
        assert_eq!(result.1.capacity(), 0);
    }

    #[test]
    fn test_clap_completions_short_circuits_not_a_subcommand() {
        let app = clap::App::new("prog");
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("prog slaps", 10, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 9);
        assert_eq!(result.1.capacity(), 0);
    }

    #[test]
    fn test_clap_completion_matches_command() {
        let app = clap::App::new("prog");
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("pr", 2, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 0);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "prog");
    }

    #[test]
    fn test_clap_completion_short_circuits_on_help_subcommand() {
        let app = clap::App::new("prog").subcommand(clap::App::new("slaps"));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("prog help ", 10, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 10);
        assert_eq!(result.1.len(), 0);
    }

    #[test]
    fn test_clap_completion_matches_empty_subcommand() {
        let app = clap::App::new("prog").subcommand(clap::App::new("slaps").alias("that"));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("prog ", 5, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 5);
        assert_eq!(result.1.len(), 2);
        assert_eq!(result.1[0], "slaps");
    }

    #[test]
    fn test_clap_completion_matches_subcommand_empty_input() {
        let app = clap::App::new("prog").subcommand(clap::App::new("slaps").alias("that"));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete(" ", 1, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 0);
        assert_eq!(result.1.len(), 3);
        assert_eq!(result.1[0], "prog");
    }

    #[test]
    fn test_clap_completion_matches_partial_subcommand() {
        let app = clap::App::new("prog").subcommand(clap::App::new("slaps").alias("that"));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("prog s", 6, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 5);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "slaps");
    }

    #[test]
    fn test_clap_completion_matches_partial_command() {
        let app = clap::App::new("prog").subcommand(clap::App::new("slaps").alias("that"));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("p", 1, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 0);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "prog");
    }

    #[test]
    fn test_clap_completion_matches_partial_command_alias() {
        let app = clap::App::new("prog").alias("slaps");
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("s", 1, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 0);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "slaps");
    }

    #[test]
    fn test_clap_completion_matches_partial_subcommand_alias() {
        let app = clap::App::new("prog").subcommand(clap::App::new("slaps").alias("that"));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("prog t", 6, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 5);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "that");
    }

    #[test]
    fn test_clap_completion_matches_arg_for_partial_subcommand_alias() {
        let app = clap::App::new("prog").subcommand(
            clap::App::new("slaps")
                .alias("that")
                .arg(Arg::with_name("arg").long("arg").takes_value(true)),
        );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("prog t --", 9, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 7);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "--arg");
    }

    #[test]
    fn test_clap_completion_matches_arg_for_subcommand_alias() {
        let app = clap::App::new("prog").subcommand(
            clap::App::new("slaps")
                .alias("that")
                .arg(Arg::with_name("arg").long("arg").takes_value(true)),
        );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("prog that --", 12, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 10);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "--arg");
    }

    #[test]
    fn test_clap_completion_short_circuits_empty_command() {
        let app = clap::App::new("prog").subcommand(clap::App::new("slaps").alias("that"));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("--a", 3, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 5);
        assert_eq!(result.1.capacity(), 0);
    }

    #[test]
    fn test_clap_completion_dont_complete_args_for_help() {
        let app = clap::App::new("prog")
            .subcommand(clap::App::new("slaps").arg(Arg::with_name("that").long("that")));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("help prog slaps --t", 19, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 6);
        assert_eq!(result.1.capacity(), 0);
    }

    #[test]
    fn test_clap_completion_short_circuits_complete_short_arg() {
        let app = clap::App::new("prog")
            .subcommand(clap::App::new("slaps").arg(Arg::with_name("that").short("t")));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("prog slaps -t", 13, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 7);
        assert_eq!(result.1.capacity(), 0);
    }

    #[test]
    #[allow(clippy::shadow_unrelated)]
    fn test_clap_completion_matches_long_opt_under_cursor() {
        let app = clap::App::new("prog").subcommand(
            clap::App::new("slaps").arg(Arg::with_name("that").long("that").takes_value(true)),
        );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("prog slaps -", 12, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 11);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "--that");

        let result = slaps
            .complete("prog slaps --", 13, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 11);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "--that");

        let result = slaps
            .complete("prog slaps --t", 14, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 11);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "--that");
    }

    #[test]
    fn test_clap_completion_matches_short_opt_under_cursor() {
        let app = clap::App::new("prog").subcommand(
            clap::App::new("slaps").arg(Arg::with_name("that").short("t").takes_value(true)),
        );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("prog slaps -", 12, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 11);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "-t");
    }

    #[test]
    #[allow(clippy::shadow_unrelated)]
    fn test_clap_completion_matches_long_flag_under_cursor() {
        let app = clap::App::new("prog").subcommand(
            clap::App::new("slaps").arg(Arg::with_name("that").long("that").takes_value(false)),
        );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("prog slaps -", 12, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 11);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "--that");

        let result = slaps
            .complete("prog slaps --", 13, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 11);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "--that");

        let result = slaps
            .complete("prog slaps --t", 14, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 11);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "--that");
    }

    #[test]
    #[allow(clippy::shadow_unrelated)]
    fn test_clap_completion_matches_short_flag_under_cursor() {
        let app = clap::App::new("prog").subcommand(
            clap::App::new("slaps").arg(Arg::with_name("that").short("t").takes_value(false)),
        );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("prog slaps -", 12, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 11);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "-t");

        let result = slaps
            .complete("prog slaps --", 13, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 11);
        assert_eq!(result.1.len(), 0);
    }

    #[test]
    fn test_clap_completion_short_circuits_long_arg_value_expected() {
        let app = clap::App::new("prog").subcommand(
            clap::App::new("slaps").arg(Arg::with_name("that").long("that").takes_value(true)),
        );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("prog slaps --that ", 18, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 8);
        assert_eq!(result.1.capacity(), 0);
    }

    #[test]
    fn test_clap_completion_short_circuits_short_arg_value_expected() {
        let app = clap::App::new("prog").subcommand(
            clap::App::new("slaps").arg(Arg::with_name("that").short("t").takes_value(true)),
        );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("prog slaps -t ", 14, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 8);
        assert_eq!(result.1.capacity(), 0);
    }

    #[test]
    fn test_clap_completion_matches_required_short_arg() {
        let app = clap::App::new("prog").subcommand(
            clap::App::new("slaps").arg(
                Arg::with_name("that")
                    .short("t")
                    .required(true)
                    .takes_value(true),
            ),
        );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("prog slaps ", 11, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 11);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "-t");
    }

    #[test]
    fn test_clap_completion_matches_required_long_arg() {
        let app = clap::App::new("prog").subcommand(
            clap::App::new("slaps").arg(
                Arg::with_name("that")
                    .long("that")
                    .required(true)
                    .takes_value(true),
            ),
        );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("prog slaps ", 11, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 11);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "--that");
    }

    #[test]
    fn test_clap_completion_nothing_to_complete() {
        let app = clap::App::new("prog").subcommand(
            clap::App::new("slaps").arg(
                Arg::with_name("that")
                    .long("that")
                    .required(true)
                    .takes_value(true),
            ),
        );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete(
                "prog slaps --that arg a",
                23,
                &Context::new(&History::new()),
            )
            .unwrap();

        assert_eq!(result.0, 9);
        assert_eq!(result.1.len(), 0);
    }

    #[test]
    fn test_clap_completion_quit_command_last() {
        let app = clap::App::new("prog");
        let slap = clap::App::new("slaps");
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(slap);
        slaps.register_command(app);

        let result = slaps
            .complete("", 0, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 0);
        assert_eq!(result.1.len(), 4);
        assert_eq!(result.1[0], "prog");
        assert_eq!(result.1[1], "slaps");
        assert_eq!(result.1[2], "help");
        assert_eq!(result.1[3], "quit");
    }

    #[test]
    fn test_clap_completion_flags_exclude_duplicates_short() {
        let app = clap::App::new("slaps").arg(Arg::with_name("verbose").short("v").multiple(false));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps -v -", 10, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.1.len(), 0);
    }

    #[test]
    fn test_clap_completion_flags_exclude_duplicates_long() {
        let app =
            clap::App::new("slaps").arg(Arg::with_name("verbose").long("verbose").multiple(false));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps --verbose --", 18, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.1.len(), 0);
    }

    #[test]
    fn test_clap_completion_flags_exclude_duplicates_mixed() {
        let app = clap::App::new("slaps").arg(
            Arg::with_name("verbose")
                .long("verbose")
                .short("v")
                .multiple(false),
        );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps -v --", 11, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.1.len(), 0);
    }

    #[test]
    fn test_clap_completion_flags_exclude_duplicates_except_multiple_short() {
        let app = clap::App::new("slaps").arg(Arg::with_name("verbose").short("v").multiple(true));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps -v -", 10, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 9);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "-v");
    }

    #[test]
    fn test_clap_completion_flags_exclude_duplicates_except_multiple_long() {
        let app =
            clap::App::new("slaps").arg(Arg::with_name("verbose").long("verbose").multiple(true));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps --verbose -", 17, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 16);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "--verbose");
    }

    #[test]
    fn test_clap_completion_flags_exclude_duplicates_except_multiple_mixed() {
        let app = clap::App::new("slaps").arg(
            Arg::with_name("verbose")
                .long("verbose")
                .short("v")
                .multiple(true),
        );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps -v -", 10, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 9);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "--verbose");
    }

    #[test]
    fn test_clap_completion_flags_sorted_short() {
        let app = clap::App::new("slaps")
            .arg(Arg::with_name("this").short("t"))
            .arg(Arg::with_name("that").short("a"));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps -", 7, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 6);
        assert_eq!(result.1.len(), 2);
        assert_eq!(result.1[0], "-a");
        assert_eq!(result.1[1], "-t");
    }

    #[test]
    fn test_clap_completion_flags_sorted_long() {
        let app = clap::App::new("slaps")
            .arg(Arg::with_name("this").long("this"))
            .arg(Arg::with_name("that").long("that"));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps -", 7, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 6);
        assert_eq!(result.1.len(), 2);
        assert_eq!(result.1[0], "--that");
        assert_eq!(result.1[1], "--this");
    }

    #[test]
    fn test_clap_completion_flags_sorted_long_before_short() {
        let app = clap::App::new("slaps")
            .arg(Arg::with_name("this").long("this"))
            .arg(Arg::with_name("that").short("a"));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps -", 7, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 6);
        assert_eq!(result.1.len(), 2);
        assert_eq!(result.1[0], "--this");
        assert_eq!(result.1[1], "-a");
    }

    #[test]
    fn test_clap_completion_flags_sorted_single_before_multiple() {
        let app = clap::App::new("slaps")
            .arg(Arg::with_name("this").long("this"))
            .arg(Arg::with_name("that").long("that").multiple(true));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps -", 7, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 6);
        assert_eq!(result.1.len(), 2);
        assert_eq!(result.1[0], "--this");
        assert_eq!(result.1[1], "--that");
    }

    #[test]
    fn test_clap_completion_opts_sorted_short() {
        let app = clap::App::new("slaps")
            .arg(Arg::with_name("this").short("t").takes_value(true))
            .arg(Arg::with_name("that").short("a").takes_value(true));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps -", 7, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 6);
        assert_eq!(result.1.len(), 2);
        assert_eq!(result.1[0], "-a");
        assert_eq!(result.1[1], "-t");
    }

    #[test]
    fn test_clap_completion_opts_sorted_long() {
        let app = clap::App::new("slaps")
            .arg(Arg::with_name("this").long("this").takes_value(true))
            .arg(Arg::with_name("that").long("that").takes_value(true));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps -", 7, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 6);
        assert_eq!(result.1.len(), 2);
        assert_eq!(result.1[0], "--that");
        assert_eq!(result.1[1], "--this");
    }

    #[test]
    fn test_clap_completion_opts_sorted_long_before_short() {
        let app = clap::App::new("slaps")
            .arg(Arg::with_name("this").long("this").takes_value(true))
            .arg(Arg::with_name("that").short("a").takes_value(true));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps -", 7, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 6);
        assert_eq!(result.1.len(), 2);
        assert_eq!(result.1[0], "--this");
        assert_eq!(result.1[1], "-a");
    }

    #[test]
    fn test_clap_completion_opts_sorted_single_before_multiple() {
        let app = clap::App::new("slaps")
            .arg(Arg::with_name("this").long("this").takes_value(true))
            .arg(
                Arg::with_name("that")
                    .long("that")
                    .takes_value(true)
                    .multiple(true),
            );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps -", 7, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 6);
        assert_eq!(result.1.len(), 2);
        assert_eq!(result.1[0], "--this");
        assert_eq!(result.1[1], "--that");
    }

    #[test]
    fn test_clap_completion_opts_exclude_duplicates_short() {
        let app = clap::App::new("slaps").arg(
            Arg::with_name("verbose")
                .short("v")
                .multiple(false)
                .takes_value(true),
        );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps -v 1 -", 12, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.1.len(), 0);
    }

    #[test]
    fn test_clap_completion_opts_exclude_duplicates_long() {
        let app = clap::App::new("slaps").arg(
            Arg::with_name("verbose")
                .long("verbose")
                .multiple(false)
                .takes_value(true),
        );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps --verbose 1 --", 20, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.1.len(), 0);
    }

    #[test]
    fn test_clap_completion_opts_exclude_duplicates_mixed() {
        let app = clap::App::new("slaps").arg(
            Arg::with_name("verbose")
                .long("verbose")
                .short("v")
                .takes_value(true)
                .multiple(false),
        );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps -v 1 --", 13, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.1.len(), 0);
    }

    #[test]
    fn test_clap_completion_opts_exclude_duplicates_short_except_multiple() {
        let app = clap::App::new("slaps").arg(
            Arg::with_name("verbose")
                .short("v")
                .multiple(true)
                .takes_value(true),
        );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps -v 1 -", 12, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 11);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "-v");
    }

    #[test]
    fn test_clap_completion_opts_exclude_duplicates_long_except_multiple() {
        let app = clap::App::new("slaps").arg(
            Arg::with_name("verbose")
                .long("verbose")
                .multiple(true)
                .takes_value(true),
        );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps --verbose 1 -", 19, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 18);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "--verbose");
    }

    #[test]
    fn test_clap_completion_opts_exclude_duplicates_mixed_except_multiple() {
        let app = clap::App::new("slaps").arg(
            Arg::with_name("verbose")
                .long("verbose")
                .short("v")
                .takes_value(true)
                .multiple(true),
        );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps -v 1 -", 12, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 11);
        assert_eq!(result.1.len(), 1);
        assert_eq!(result.1[0], "--verbose");
    }

    #[test]
    fn test_clap_completion_opts_required_before_optional() {
        let app = clap::App::new("slaps")
            .arg(
                Arg::with_name("this")
                    .long("this")
                    .takes_value(true)
                    .required(true),
            )
            .arg(
                Arg::with_name("that")
                    .long("that")
                    .takes_value(true)
                    .required(false),
            );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps -", 7, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 6);
        assert_eq!(result.1.len(), 2);
        assert_eq!(result.1[0], "--this");
        assert_eq!(result.1[1], "--that");
    }

    #[test]
    fn test_clap_completion_opts_before_flags() {
        let app = clap::App::new("slaps")
            .arg(Arg::with_name("this").long("this").takes_value(true))
            .arg(Arg::with_name("that").long("that").takes_value(false));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps
            .complete("slaps -", 7, &Context::new(&History::new()))
            .unwrap();

        assert_eq!(result.0, 6);
        assert_eq!(result.1.len(), 2);
        assert_eq!(result.1[0], "--this");
        assert_eq!(result.1[1], "--that");
    }

    #[test]
    fn test_clap_hinter_single_completion_only() {
        let app = clap::App::new("slaps")
            .arg(Arg::with_name("this").long("this").takes_value(true))
            .arg(Arg::with_name("that").long("that").takes_value(false));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps.hint("slaps --t", 9, &Context::new(&History::new()));

        assert_eq!(result, None);
    }

    #[test]
    fn test_clap_hinter_mismatched_quotes_single() {
        let app = clap::App::new("slaps")
            .arg(Arg::with_name("this").long("this").takes_value(true))
            .arg(Arg::with_name("that").long("that").takes_value(false));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps.hint("slaps '", 7, &Context::new(&History::new()));

        assert_eq!(result.as_deref(), Some("'"));
    }

    #[test]
    fn test_clap_hinter_mismatched_quotes_double() {
        let app = clap::App::new("slaps")
            .arg(Arg::with_name("this").long("this").takes_value(true))
            .arg(Arg::with_name("that").long("that").takes_value(false));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps.hint("slaps \"", 7, &Context::new(&History::new()));

        assert_eq!(result.as_deref(), Some("\""));
    }

    #[test]
    fn test_clap_hinter_cursor_at_end_of_input_only() {
        let app = clap::App::new("slaps")
            .arg(Arg::with_name("this").long("this").takes_value(true))
            .arg(Arg::with_name("that").long("that").takes_value(false));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps.hint("slaps '", 6, &Context::new(&History::new()));

        assert_eq!(result.as_deref(), None);
    }

    #[test]
    fn test_clap_hinter_truncates_start_of_suggestion() {
        let app = clap::App::new("slaps")
            .arg(Arg::with_name("this").long("this").takes_value(true))
            .arg(Arg::with_name("that").long("that").takes_value(false));
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps.hint("slaps --thi", 11, &Context::new(&History::new()));

        assert_eq!(result.as_deref(), Some("s"));
    }

    #[test]
    fn test_clap_hinter_from_empty() {
        let app = clap::App::new("slaps").arg(
            Arg::with_name("this")
                .long("this")
                .takes_value(true)
                .required(true),
        );
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps.hint("slaps ", 6, &Context::new(&History::new()));

        assert_eq!(result.as_deref(), Some("--this"));
    }

    #[test]
    fn test_clap_highlighter_reconstructs_whitespace() {
        let app = clap::App::new("slaps").arg(
            Arg::with_name("this")
                .long("this")
                .takes_value(true)
                .required(true),
        );
        let config = Config::default();
        let mut slaps = Matcher::with_name_and_config("slaps", Config::default());
        slaps.register_command(app);

        let result = slaps.highlight("slaps   --this", 6);

        assert_eq!(
            result,
            format!(
                "{}   {}",
                &config.highlighter_style_command.paint("slaps"),
                &config.highlighter_style_argument.paint("--this")
            )
        );
    }

    #[test]
    fn test_clap_highlighter_highlights_mismatched_quote() {
        let app = clap::App::new("slaps").arg(
            Arg::with_name("this")
                .long("this")
                .takes_value(true)
                .required(true),
        );
        let config = Config::default().highlighter_underline_word_under_cursor(false);
        let mut slaps = Matcher::with_name_and_config("slaps", config);
        slaps.register_command(app);

        let result = slaps.highlight(" \"--this", 0);

        assert_eq!(
            result,
            format!(
                " {}{}",
                &config.highlighter_style_mismatched_quotes.paint("\""),
                &config.highlighter_style_value.paint("--this")
            )
        );
    }

    #[test]
    fn test_clap_highlighter_highlights_mismatched_quote_underline() {
        let app = clap::App::new("slaps").arg(
            Arg::with_name("this")
                .long("this")
                .takes_value(true)
                .required(true),
        );
        let config = Config::default().highlighter_underline_word_under_cursor(true);
        let mut slaps = Matcher::with_name_and_config("slaps", config);
        slaps.register_command(app);

        let result = slaps.highlight("\"--this", 7);

        assert_eq!(
            result,
            format!(
                "{}{}",
                &config
                    .highlighter_style_mismatched_quotes
                    .underline()
                    .paint("\""),
                &config.highlighter_style_value.underline().paint("--this")
            )
        );
    }

    #[test]
    fn test_clap_highlighter_highlights_before_mismatched_quote() {
        let app = clap::App::new("slaps").arg(
            Arg::with_name("this")
                .long("this")
                .takes_value(true)
                .required(true),
        );
        let config = Config::default().highlighter_underline_word_under_cursor(false);
        let mut slaps = Matcher::with_name_and_config("slaps", config);
        slaps.register_command(app);

        let result = slaps.highlight("slaps \"--this", 6);

        assert_eq!(
            result,
            format!(
                "{} {}{}",
                &config.highlighter_style_command.paint("slaps"),
                &config.highlighter_style_mismatched_quotes.paint("\""),
                &config.highlighter_style_value.paint("--this")
            )
        );
    }

    #[test]
    fn test_clap_highlighter_highlights_all_values_after_double_dash() {
        let app = clap::App::new("slaps").arg(
            Arg::with_name("this")
                .long("this")
                .takes_value(true)
                .required(true),
        );
        let config = Config::default().highlighter_underline_word_under_cursor(false);
        let mut slaps = Matcher::with_name_and_config("slaps", config);
        slaps.register_command(app);

        let result = slaps.highlight("slaps -- --this", 6);

        assert_eq!(
            result,
            format!(
                "{} {} {}",
                &config.highlighter_style_command.paint("slaps"),
                &config.highlighter_style_argument.paint("--"),
                &config.highlighter_style_value.paint("--this")
            )
        );
    }

    #[test]
    fn test_clap_highlighter_highlights_command() {
        let app = clap::App::new("slaps").arg(
            Arg::with_name("this")
                .long("this")
                .takes_value(true)
                .required(true),
        );
        let config = Config::default().highlighter_underline_word_under_cursor(false);
        let mut slaps = Matcher::with_name_and_config("slaps", config);
        slaps.register_command(app);

        let result = slaps.highlight("slaps", 5);

        assert_eq!(
            result,
            format!("{}", &config.highlighter_style_command.paint("slaps"),)
        );
    }

    #[test]
    fn test_clap_highlighter_highlights_argument() {
        let app = clap::App::new("slaps").arg(
            Arg::with_name("this")
                .long("this")
                .takes_value(true)
                .required(true),
        );
        let config = Config::default().highlighter_underline_word_under_cursor(false);
        let mut slaps = Matcher::with_name_and_config("slaps", config);
        slaps.register_command(app);

        let result = slaps.highlight("slaps --this", 0);

        assert_eq!(
            result,
            format!(
                "{} {}",
                &config.highlighter_style_command.paint("slaps"),
                &config.highlighter_style_argument.paint("--this"),
            )
        );
    }

    #[test]
    fn test_clap_highlighter_highlights_argument_with_inline_value() {
        let app = clap::App::new("slaps").arg(
            Arg::with_name("this")
                .long("this")
                .takes_value(true)
                .required(true),
        );
        let config = Config::default().highlighter_underline_word_under_cursor(false);
        let mut slaps = Matcher::with_name_and_config("slaps", config);
        slaps.register_command(app);

        let result = slaps.highlight("slaps --this=arg", 16);

        assert_eq!(
            result,
            format!(
                "{} {}{}",
                &config.highlighter_style_command.paint("slaps"),
                &config.highlighter_style_argument.paint("--this"),
                &config.highlighter_style_value.paint("=arg"),
            )
        );
    }

    #[test]
    fn test_clap_highlighter_underlines_argument_with_inline_value() {
        let app = clap::App::new("slaps").arg(
            Arg::with_name("this")
                .long("this")
                .takes_value(true)
                .required(true),
        );
        let config = Config::default().highlighter_underline_word_under_cursor(true);
        let mut slaps = Matcher::with_name_and_config("slaps", config);
        slaps.register_command(app);

        let result = slaps.highlight("slaps --this=arg", 16);

        assert_eq!(
            result,
            format!(
                "{} {}{}",
                &config.highlighter_style_command.paint("slaps"),
                &config
                    .highlighter_style_argument
                    .underline()
                    .paint("--this"),
                &config.highlighter_style_value.underline().paint("=arg"),
            )
        );
    }

    #[test]
    fn test_clap_highlighter_does_not_underline_word_under_cursor_if_disabled() {
        let app = clap::App::new("slaps").arg(
            Arg::with_name("this")
                .long("this")
                .takes_value(true)
                .required(true),
        );
        let config = Config::default().highlighter_underline_word_under_cursor(true);
        let mut slaps = Matcher::with_name_and_config("slaps", config);
        slaps.register_command(app);

        let result = slaps.highlight("slaps", 5);

        assert_eq!(
            result,
            format!(
                "{}",
                &config.highlighter_style_command.underline().paint("slaps"),
            )
        );
    }

    #[test]
    fn test_clap_executes_handler() {
        let app = clap::App::new("slaps").arg(
            Arg::with_name("this")
                .long("this")
                .takes_value(true)
                .required(true),
        );
        let config = Config::default().highlighter_underline_word_under_cursor(true);
        let mut slaps = Matcher::with_name_and_config("slaps", config);
        slaps.register_command_with_handler(
            app,
            Box::new(|args| {
                assert_eq!(args.value_of("this"), Some("arg"));
                Err(ExecutionError::GracefulExit)
            }),
        );

        let result = slaps.handle_matches(&split("slaps --this arg").unwrap());

        assert!(result.is_err());
        assert!(matches!(
            result,
            Err(crate::Error::ExecutionError(ExecutionError::GracefulExit))
        ));
    }

    #[test]
    fn test_clap_handler_propagates_clap_errors() {
        let app = clap::App::new("slaps").arg(
            Arg::with_name("this")
                .long("this")
                .takes_value(true)
                .required(true),
        );
        let config = Config::default().highlighter_underline_word_under_cursor(true);
        let mut slaps = Matcher::with_name_and_config("slaps", config);
        slaps.register_command_with_handler(app, Box::new(|_| unreachable!()));

        let result = slaps.handle_matches(&split("slaps --that arg").unwrap());

        assert!(result.is_err());
        assert!(matches!(result, Err(crate::Error::ClapError(_))));
    }
}
