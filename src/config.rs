/*
 * config.rs is part of slaps.
 * Copyright (C) 2020 rd <slaps@megane.space>
 * License: WTFPL
 */

/// Configuration for the internal sub-components of slaps.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Config<'a> {
    /// Colorization mode.
    pub color_mode: ColorMode,
    /// Tab-completion type.
    pub completion_type: CompletionType,
    /// Prompt for input.
    pub prompt: &'a str,
    /// Prompt for passwords.
    pub password_prompt: &'a str,
    /// Template for the root help command.
    pub help_template: &'a str,
    /// Help template for subcommands with arguments.
    pub subcommand_help_template: &'a str,
    /// Help template for subcommands without arguments.
    pub subcommand_help_template_no_args: &'a str,
}

impl<'a> Config<'a> {
    /// Forces colorization on or off.
    ///
    /// By default, colorization is on except if stdout is not a TTY.
    ///
    /// # Examples
    /// ```
    /// use slaps::{Config, ColorMode};
    ///
    /// let config = Config::default().color_mode(ColorMode::Disabled);
    /// # assert_eq!(config.color_mode, ColorMode::Disabled);
    /// ```
    #[must_use]
    pub fn color_mode(mut self, color_mode: ColorMode) -> Self {
        self.color_mode = color_mode;
        self
    }

    /// Sets the tab completion type.
    ///
    /// # Examples
    /// ```
    /// use slaps::{Config, CompletionType};
    ///
    /// let config = Config::default().completion_type(CompletionType::List);
    /// # assert_eq!(config.completion_type, CompletionType::List);
    /// ```
    #[must_use]
    pub fn completion_type(mut self, completion_mode: CompletionType) -> Self {
        self.completion_type = completion_mode;
        self
    }

    /// Sets the string used to prompt for input.
    ///
    /// # Examples
    /// ```
    /// use slaps::Config;
    ///
    /// let config = Config::default().set_prompt(" >>> ");
    /// # assert_eq!(config.prompt, " >>> ");
    /// ```
    #[must_use]
    pub fn set_prompt(mut self, prompt: &'a str) -> Self {
        self.prompt = prompt;
        self
    }

    /// Sets the string used to prompt for passwords.
    ///
    /// # Examples
    /// ```
    /// use slaps::Config;
    ///
    /// let config = Config::default().set_password_prompt("Password? ");
    /// # assert_eq!(config.password_prompt, "Password? ");
    /// ```
    #[must_use]
    pub fn set_password_prompt(mut self, prompt: &'a str) -> Self {
        self.password_prompt = prompt;
        self
    }

    /// Sets the [template] for the root help command.
    ///
    /// # Examples
    /// ```
    /// use slaps::Config;
    ///
    /// let config = Config::default().help_template("Subcommands: {subcommands}");
    /// # assert_eq!(config.help_template, "Subcommands: {subcommands}");
    /// ```
    ///
    /// [template]: clap::App::template
    #[must_use]
    pub fn help_template(mut self, template: &'a str) -> Self {
        self.help_template = template;
        self
    }

    /// Sets the help [template] for subcommands with arguments.
    ///
    /// # Examples
    /// ```
    /// use slaps::Config;
    ///
    /// let config = Config::default().subcommand_help_template("Usage: {usage}");
    /// # assert_eq!(config.subcommand_help_template, "Usage: {usage}");
    /// ```
    ///
    /// [template]: clap::App::template
    #[must_use]
    pub fn subcommand_help_template(mut self, template: &'a str) -> Self {
        self.subcommand_help_template = template;
        self
    }

    /// Sets the help [template] for subcommands without arguments.
    ///
    /// # Examples
    /// ```
    /// use slaps::Config;
    ///
    /// let config = Config::default().subcommand_help_template_no_args("About: {about}");
    /// # assert_eq!(config.subcommand_help_template_no_args, "About: {about}");
    /// ```
    ///
    /// [template]: clap::App::template
    #[must_use]
    pub fn subcommand_help_template_no_args(mut self, template: &'a str) -> Self {
        self.subcommand_help_template_no_args = template;
        self
    }
}

impl Default for Config<'_> {
    fn default() -> Self {
        Config {
            color_mode: ColorMode::default(),
            completion_type: CompletionType::default(),
            prompt: "> ",
            password_prompt: "Password: ",
            help_template: "{subcommands}",
            subcommand_help_template: "{usage}\n{about}\n\n{all-args}",
            subcommand_help_template_no_args: "{about}",
        }
    }
}

/// Colorization mode.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ColorMode {
    /// Activate highlighting even if platform is not supported (windows < 10).
    Forced,
    /// Activate highlighting if platform/terminal is supported.
    Enabled,
    /// Deactivate highlighting even if platform/terminal is supported.
    Disabled,
}

impl Default for ColorMode {
    fn default() -> Self {
        ColorMode::Enabled
    }
}

/// Tab completion style.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum CompletionType {
    /// Complete the next full match (like in Vim by default).
    Circular,
    /// Complete till longest match.
    /// When more than one match, list all matches
    /// (like in Bash/Readline).
    List,
}

impl Default for CompletionType {
    fn default() -> Self {
        CompletionType::Circular
    }
}

#[cfg(test)]
mod tests {
    use crate::{ColorMode, CompletionType, Config};

    #[test]
    fn test_default_config() {
        assert_eq!(
            Config::default(),
            Config {
                color_mode: ColorMode::Enabled,
                completion_type: CompletionType::Circular,
                prompt: "> ",
                password_prompt: "Password: ",
                help_template: "{subcommands}",
                subcommand_help_template: "{usage}\n{about}\n\n{all-args}",
                subcommand_help_template_no_args: "{about}"
            }
        );
    }
}