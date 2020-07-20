/*
 * config.rs is part of slaps.
 * Copyright (C) 2020 rd <slaps@megane.space>
 * License: WTFPL
 */

/// Used to configure and provide abstraction for the internal sub-components of slaps.
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
    /// Prompt for new passwords.
    pub new_password_prompt: &'a str,
    /// Prompt for retyping new passwords.
    pub new_password_retype_prompt: &'a str,
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

    /// Sets the string used to prompt for new passwords.
    ///
    /// # Examples
    /// ```
    /// use slaps::Config;
    ///
    /// let config = Config::default().new_password_prompt("New password? ");
    /// # assert_eq!(config.new_password_prompt, "New password? ");
    /// ```
    #[must_use]
    pub fn new_password_prompt(mut self, prompt: &'a str) -> Self {
        self.new_password_prompt = prompt;
        self
    }

    /// Sets the string used to prompt to retype new passwords.
    ///
    /// # Examples
    /// ```
    /// use slaps::Config;
    ///
    /// let config = Config::default().new_password_retype_prompt("Retype password? ");
    /// # assert_eq!(config.new_password_retype_prompt, "Retype password? ");
    /// ```
    #[must_use]
    pub fn new_password_retype_prompt(mut self, prompt: &'a str) -> Self {
        self.new_password_retype_prompt = prompt;
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
            new_password_prompt: "Enter new password: ",
            new_password_retype_prompt: "Retype new password: ",
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

/// Tab completion style
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum CompletionType {
    /// Complete the next full match (like in Vim by default)
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
                new_password_prompt: "Enter new password: ",
                new_password_retype_prompt: "Retype new password: ",
            }
        );
    }
}
