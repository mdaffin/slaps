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
mod readline;
#[doc(hidden)]
pub mod shlex;

use crate::readline::Readline;
pub use config::{ColorMode, CompletionType, Config};
pub use error::{ClapError, Error, ErrorKind, MismatchedQuotes, ReadlineError};

/// Interactive shell mode using a `clap`/`structopt` configuration.
#[derive(Debug)]
pub struct Slaps<'a, 'b> {
    config: Config<'b>,
    matcher: clap::Matcher<'a, 'b>,
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
        Slaps {
            config,
            matcher: clap::Matcher::with_name_and_config(name, config),
        }
    }

    /// Registers a Clap subcommand configuration with Slaps.
    ///
    /// # Examples
    /// ```
    /// use clap::App;
    /// use slaps::Slaps;
    ///
    /// let slaps = Slaps::with_name("slaps").subcommand(App::new("slap"));
    /// ```
    #[must_use]
    pub fn subcommand(mut self, command: clap::App<'a, 'a>) -> Self {
        self.matcher.register_command(command);

        self
    }

    /// Prompts the user for a `String` using the default prompt.
    ///
    /// If prompt is None, uses the default from the [`Config`].
    ///
    /// # Errors
    /// May return a [`ReadlineError`] from the internal readline implementation.
    pub fn prompt_line(&self, prompt: Option<&str>) -> Result<String, ReadlineError> {
        readline::Readline::new(self.config).prompt_line(prompt.unwrap_or(self.config.prompt))
    }

    /// Prompts the user for a password.
    ///
    /// If prompt is None, uses the default from the [`Config`].
    ///
    /// # Errors
    /// May return a [`ReadlineError`] from the internal readline implementation.
    pub fn prompt_password(&self, prompt: Option<&str>) -> Result<String, ReadlineError> {
        readline::Readline::prompt_password(prompt.unwrap_or(self.config.password_prompt))
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
    /// let mut slaps = Slaps::with_name("slaps").subcommand(command);
    /// let matches = slaps.get_matches("slaps --that arg").unwrap();
    ///
    /// assert_eq!(matches.subcommand_name(), Some("slaps"));
    /// assert_eq!(matches.subcommand_matches("slaps").unwrap().value_of("that"), Some("arg"));
    /// ```
    pub fn get_matches<T: AsRef<str>>(&mut self, input: T) -> Result<clap::ArgMatches, Error> {
        let words = shlex::split(input.as_ref())?;
        Ok(self.matcher.get_matches(&words)?)
    }

    /// Starts the main interactive loop, prompting the user for input repeatedly.
    ///
    /// # Errors
    /// Will return an error when it fails to read more input.
    pub fn run(mut self) -> Result<(), Error> {
        loop {
            let input = match Readline::with_helper(
                self.config,
                &self.matcher,
                &self.matcher,
                &self.matcher,
            )
            .prompt_line(self.config.prompt)
            {
                Ok(i) => i,
                Err(e) => return Err(Error::ReadlineError(e)),
            };

            match self.get_matches(&input) {
                Err(Error::ClapError(e)) => match e.kind {
                    clap::ErrorKind::Io | ErrorKind::Format => return Err(Error::ClapError(e)),
                    _ => eprintln!("{}", e),
                },
                Ok(r) => eprintln!("t {:?}", r),
                Err(e) => eprintln!("{}", e),
            };
        }
    }
}
