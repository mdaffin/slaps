/*
 * lib.rs is part of slaps.
 * Copyright (C) 2020 rd <slaps@megane.space>
 * License: WTFPL
 */

//! Add an interactive shell mode to your Rust command-line application using your
//! existing [`clap`]/[`structopt`] configuration.
//!
//! [`Clap`]: https://crates.io/crates/clap
//! [`structopt`]: https://crates.io/crates/structopt

// Deny unsafe code and enforce strict linting.
#![deny(unsafe_code)]
#![warn(clippy::all)]
#![warn(clippy::pedantic)]
#![warn(missing_docs)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

mod clap;
mod config;
mod error;
#[doc(hidden)]
pub mod shlex;

pub use config::{ColorMode, CompletionType, Config};
pub use error::{ClapError, Error, ErrorKind, ExecutionError, MismatchedQuotes, ReadlineError};
use rustyline::Editor;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};

/// Handler function for a command.
type Handler<'a> = Box<dyn 'a + Fn(&clap::ArgMatches) -> Result<(), ExecutionError> + Send + Sync>;

/// Interactive shell mode using a `clap`/`structopt` configuration.
pub struct Slaps<'a, 'b> {
    config: Config<'b>,
    editor: Editor<clap::Matcher<'a, 'b>>,
    handlers: HashMap<u64, Handler<'b>>,
}

impl<'a, 'b> Slaps<'a, 'b> {
    /// Creates a new interactive app using the default configuration.
    ///
    /// # Examples
    /// ```
    /// use slaps::Slaps;
    ///
    /// let slaps = Slaps::with_name("slaps");
    /// ```
    #[must_use]
    pub fn with_name(name: &str) -> Self {
        Self::with_name_and_config(name, Config::default())
    }

    /// Creates a new interactive app with the given `Config`.
    ///
    /// # Examples
    /// ```
    /// use slaps::{Config, ColorMode, Slaps};
    ///
    /// let slaps = Slaps::with_name_and_config("slaps", Config::default()
    ///     .color_mode(ColorMode::Disabled));
    /// ```
    #[must_use]
    pub fn with_name_and_config(name: &str, config: Config<'b>) -> Slaps<'a, 'b> {
        use ::clap::App;

        let mut editor = Editor::with_config(config.into());
        editor.set_helper(Some(clap::Matcher::with_name_and_config(name, config)));

        Slaps {
            config,
            editor,
            handlers: HashMap::new(),
        }
        .command(
            App::new("quit").alias("exit").about("Exits the program"),
            |_| Err(ExecutionError::GracefulExit),
        )
    }

    /// Registers a Clap subcommand configuration with Slaps.
    ///
    /// # Examples
    /// ```
    /// use clap::App;
    /// use slaps::Slaps;
    ///
    /// let slaps = Slaps::with_name("slaps")
    ///     .command(App::new("slap"), |matches| {println!("{:?}", matches); Ok(())} );
    /// ```
    #[must_use]
    pub fn command<T: 'b>(mut self, command: clap::App<'a, 'b>, handler: T) -> Self
    where
        T: Fn(&clap::ArgMatches) -> Result<(), ExecutionError> + Send + Sync,
    {
        self.handlers
            .insert(calculate_hash(&command.p.meta.name), Box::new(handler));
        self.editor.helper_mut().unwrap().register_command(command);
        self
    }

    /// Prompts the user for a `String` using the default prompt.
    ///
    /// If prompt is None, uses the default from the [`Config`].
    ///
    /// # Errors
    /// May return a [`ReadlineError`] from the internal readline implementation.
    pub fn prompt_line(&mut self, prompt: Option<&str>) -> Result<String, ReadlineError> {
        Ok(self.editor.readline(prompt.unwrap_or(self.config.prompt))?)
    }

    /// Prompts the user for a password.
    ///
    /// If prompt is None, uses the default from the [`Config`].
    ///
    /// # Errors
    /// May return a [`ReadlineError`] from the internal readline implementation.
    pub fn prompt_password(&self, prompt: Option<&str>) -> Result<String, ReadlineError> {
        Ok(rpassword::prompt_password_stderr(
            prompt.unwrap_or(self.config.password_prompt),
        )?)
    }

    /// Parses a string directly into a [`clap::ArgMatches`] object.
    ///
    /// # Errors
    /// - `MismatchedQuote` parsing failed due to mismatched quotation marks within the input.
    ///
    /// # Examples
    /// ```
    /// use clap::{App, Arg};
    /// use slaps::Slaps;
    ///
    /// let command = App::new("slaps").arg(Arg::with_name("that").long("that").takes_value(true));
    /// let mut slaps = Slaps::with_name("slaps").command(command, |args| Ok(()));
    /// let matches = slaps.get_matches("slaps --that arg").unwrap();
    ///
    /// assert_eq!(matches.subcommand_name(), Some("slaps"));
    /// assert_eq!(matches.subcommand_matches("slaps").unwrap().value_of("that"), Some("arg"));
    /// ```
    pub fn get_matches<T: AsRef<str>>(&mut self, input: T) -> Result<clap::ArgMatches, Error> {
        let words = shlex::split(input.as_ref())?;
        Ok(self.editor.helper_mut().unwrap().get_matches(&words)?)
    }

    /// Starts the main interactive loop, prompting the user for input repeatedly.
    ///
    /// # Errors
    /// Will return an error when it fails to read more input.
    pub fn run(mut self) -> Result<(), Error> {
        use rustyline::error::ReadlineError as RustylineError;
        loop {
            let input = match self.editor.readline(self.config.prompt) {
                Ok(i) => i,
                Err(e) => {
                    return match e {
                        RustylineError::Eof | RustylineError::Interrupted => {
                            Err(ExecutionError::GracefulExit.into())
                        }
                        _ => Err(e.into()),
                    }
                }
            };

            let fields = match shlex::split(&input) {
                Ok(f) => f,
                Err(e) => {
                    eprintln!("{}", e);
                    continue;
                }
            };

            match self.handle_matches(&fields) {
                Err(Error::ClapError(e)) => match e.kind {
                    clap::ErrorKind::Io | ErrorKind::Format => return Err(Error::ClapError(e)),
                    _ => eprintln!("{}", e),
                },
                Ok(r) => eprintln!("t {:?}", r),
                Err(e) => eprintln!("{}", e),
            };
        }
    }

    /// Matches a command by parsing command-line arguments from the given input, and executes its
    /// associated handler.
    ///
    /// Useful for handling commands from `std::env::args` before switching to interactive mode.
    ///
    /// # Errors
    /// - [`ClapError`] - the input did not specify a command or failed to parse arguments.
    /// - [`ExecutionError`] - the handler returned an error.
    ///
    /// # Examples
    /// ```
    /// use slaps::{Config, ExecutionError, Slaps, Error};
    /// use clap::{App, Arg, ErrorKind};
    ///
    /// let command = App::new("slaps").arg(
    /// Arg::with_name("this")
    ///     .long("this")
    ///     .takes_value(true)
    ///     .required(true),
    /// );
    ///
    /// let mut slaps = Slaps::with_name("slaps").command(command, |args| {
    /// # assert_eq!(args.value_of("this"), Some("arg"));
    ///     Err(ExecutionError::GracefulExit)
    /// });
    /// let args = ["prog", "slaps", "--this", "arg"];
    /// let result = slaps.handle_matches(&args[1..]);
    ///
    /// match result {
    ///     Err(Error::ClapError(ref e)) => match e.kind {
    ///         ErrorKind::Io | ErrorKind::Format => eprintln!("I/O error: {}", e),
    ///         _ => eprintln!("Error while parsing arguments: {}", e),
    ///     },
    ///     Ok(_) => eprintln!("Command succeeded."),
    ///     Err(ref e) => eprintln!("Command handler failed to execute: {}", e)
    /// }
    ///
    /// # assert!(result.is_err());
    /// # assert!(matches!(
    /// #        result,
    /// #        Err(Error::ExecutionError(ExecutionError::GracefulExit))
    /// #    ));
    /// ```
    pub fn handle_matches<A: AsRef<OsStr>>(&mut self, input: &[A]) -> Result<(), Error> {
        let matches = self.editor.helper_mut().unwrap().get_matches(input)?;
        if let Some(subcommand) = matches.subcommand_name() {
            if let (Some(handler), Some(subcommand_matches)) = (
                self.handlers.get_mut(&calculate_hash(&subcommand)),
                matches.subcommand_matches(subcommand),
            ) {
                handler(subcommand_matches)?;
            }
        }
        Ok(())
    }
}

impl Debug for Slaps<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Slaps {{config = {:?}, handlers = {:?}}}",
            self.config,
            self.handlers.keys()
        )
    }
}

// Hack to avoid copying Strings into the handlers hashmap.
fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

#[cfg(test)]
mod tests {
    use crate::config::ColorMode::Disabled;
    use crate::config::CompletionType::List;
    use crate::shlex::split;
    use crate::{Config, Slaps};

    #[test]
    fn test_sets_rustyline_config() {
        use rustyline::config::Configurer;
        use rustyline::config::OutputStreamType;
        use rustyline::ColorMode;
        use rustyline::CompletionType;

        let config = Config::default().color_mode(Disabled).completion_type(List);

        let mut slaps = Slaps::with_name_and_config("slaps", config);
        let readline_config = slaps.editor.config_mut();
        assert_eq!(readline_config.color_mode(), ColorMode::Disabled);
        assert_eq!(readline_config.completion_type(), CompletionType::List);
        assert_eq!(readline_config.auto_add_history(), config.history_auto_add);
        assert_eq!(readline_config.tab_stop(), 4);
        assert_eq!(readline_config.output_stream(), OutputStreamType::Stderr);
    }

    #[test]
    fn test_propagates_clap_errors() {
        let app = ::clap::App::new("slaps").arg(
            ::clap::Arg::with_name("this")
                .long("this")
                .takes_value(true)
                .required(true),
        );
        let config = Config::default().highlighter_underline_word_under_cursor(true);
        let mut slaps =
            Slaps::with_name_and_config("slaps", config).command(app, |_| unreachable!());
        let result = slaps.handle_matches(&split("slaps --that arg").unwrap());

        assert!(result.is_err());
        assert!(matches!(result, Err(crate::Error::ClapError(_))));
    }
}
