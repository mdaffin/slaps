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
mod readline;
/// Splitting strings into fields in accordance with Unix shell rules.
pub mod shlex;

use crate::readline::Readline;
pub use config::{ColorMode, CompletionType, Config};
pub use readline::Error as ReadlineError;

/// Interactive shell mode using a `clap`/`structopt` configuration.
#[derive(Debug)]
pub struct Slaps<'a> {
    config: Config<'a>,
    readline: readline::Readline,
}

impl<'a> Slaps<'a> {
    /// Creates a new interactive app using the default configuration.
    ///
    /// # Examples
    /// ```
    /// use slaps::Slaps;
    ///
    /// let slaps = Slaps::new();
    /// ```
    #[must_use]
    pub fn new() -> Self {
        let config = Config::default();
        let readline = Readline::with_config(&config);

        Slaps { config, readline }
    }

    /// Creates a new interactive app with the given `Config`.
    ///
    /// # Examples
    /// ```
    /// use slaps::{Config, ColorMode, Slaps};
    ///
    /// let slaps = Slaps::with_config(Config::default().color_mode(ColorMode::Disabled));
    /// ```
    #[must_use]
    pub fn with_config(config: Config<'a>) -> Self {
        let readline = Readline::with_config(&config);

        Slaps { config, readline }
    }

    /// Prompts the user for input using the default prompt.
    ///
    /// # Errors
    /// May return a [`ReadlineError`] from the internal readline implementation.
    pub fn prompt_line(&mut self) -> Result<String, readline::Error> {
        self.readline.prompt_line(self.config.prompt)
    }

    /// Prompts the user for a password using the default password prompt.
    ///
    /// # Errors
    /// May return a [`ReadlineError`] from the internal readline implementation.
    pub fn prompt_password(&mut self) -> Result<String, readline::Error> {
        Readline::prompt_password(self.config.password_prompt)
    }

    /// Prompts the user for a password twice.
    ///
    /// Does not verify if the passwords match.
    ///
    /// # Errors
    /// May return a [`ReadlineError`] from the internal readline implementation.
    pub fn prompt_new_password(&mut self) -> Result<(String, String), readline::Error> {
        // Does IO.
        Ok((
            Readline::prompt_password(self.config.new_password_prompt)?,
            Readline::prompt_password(self.config.new_password_retype_prompt)?,
        ))
    }
}

impl Default for Slaps<'_> {
    fn default() -> Self {
        Slaps::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::Slaps;

    #[test]
    fn test_default_same_config_as_new() {
        let slaps_new = Slaps::new();
        let slaps_default = Slaps::default();

        assert_eq!(slaps_new.config, slaps_default.config);
    }
}
