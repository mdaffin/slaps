/*
 * readline.rs is part of slaps.
 * Copyright (C) 2020 rd <slaps@megane.space>
 * License: WTFPL
 */

use crate::{ColorMode, CompletionType, Config};
use rustyline::completion::{Candidate, Completer};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::line_buffer::LineBuffer;
use rustyline::validate::Validator;
use rustyline::{Context, Editor};
use std::borrow::Cow;
use std::borrow::Cow::Borrowed;
use std::fmt::{Display, Formatter};
use std::io;

/// Rustyline-based readline implementation.
#[derive(Debug)]
pub struct Readline<'a> {
    editor: Editor<Helper<'a>>,
}

impl<'a> Readline<'a> {
    pub fn new(config: Config) -> Self {
        Readline {
            editor: Editor::with_config((&config).into()),
        }
    }

    pub fn with_helper(
        config: Config,
        completer: &'a (dyn Completer<Candidate = CompletionCandidate> + 'a),
        hinter: &'a dyn Hinter,
        highlighter: &'a dyn Highlighter,
    ) -> Self {
        let mut readline = Self::new(config);
        readline.editor.set_helper(Some(Helper {
            completer,
            hinter,
            highlighter,
        }));
        readline
    }

    pub fn prompt_line(&mut self, prompt: &str) -> Result<String, Error> {
        Ok(self.editor.readline(prompt)?)
    }

    pub fn prompt_password(prompt: &str) -> Result<String, Error> {
        Ok(rpassword::prompt_password_stderr(prompt)?)
    }
}

/// The error type for Readline errors that can arise from
/// I/O related errors or Errno when using the nix-rust library
#[derive(Debug)]
#[allow(clippy::pub_enum_variant_names)]
#[non_exhaustive]
pub enum Error {
    /// I/O Error
    Io(io::Error),
    /// EOF (Ctrl-D)
    Eof,
    /// Ctrl-C
    Interrupted,
    /// Chars Error
    #[cfg(unix)]
    Utf8Error,
    /// Unix Error from syscall
    #[cfg(unix)]
    Errno(nix::Error),
    /// Error generated on WINDOW_BUFFER_SIZE_EVENT to mimic unix SIGWINCH
    /// signal
    #[cfg(windows)]
    WindowResize,
    /// Like Utf8Error on unix
    #[cfg(windows)]
    Decode(char::DecodeUtf16Error),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Io(ref err) => err.fmt(f),
            Error::Eof => write!(f, "EOF"),
            Error::Interrupted => write!(f, "Interrupted"),
            #[cfg(unix)]
            Error::Utf8Error => write!(f, "invalid utf-8: corrupt contents"),
            #[cfg(unix)]
            Error::Errno(ref err) => err.fmt(f),
            #[cfg(windows)]
            Error::WindowResize => write!(f, "WindowResize"),
            #[cfg(windows)]
            Error::Decode(ref err) => err.fmt(f),
        }
    }
}

impl std::error::Error for Error {}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Self::Io(e)
    }
}

impl From<rustyline::error::ReadlineError> for Error {
    fn from(e: rustyline::error::ReadlineError) -> Self {
        match e {
            ReadlineError::Io(e) => Self::Io(e),
            ReadlineError::Eof => Self::Eof,
            ReadlineError::Interrupted => Self::Interrupted,
            ReadlineError::Utf8Error => Self::Utf8Error,
            #[cfg(unix)]
            ReadlineError::Errno(e) => Self::Errno(e),
            #[cfg(windows)]
            ReadlineError::WindowResize => Self::WindowResize,
            #[cfg(windows)]
            ReadlineError::Decode(e) => Self::Decode(e),
            _ => unreachable!(),
        }
    }
}

impl Into<rustyline::Config> for &Config<'_> {
    fn into(self) -> rustyline::Config {
        use rustyline::config::OutputStreamType::Stderr;
        rustyline::Config::builder()
            .color_mode(self.color_mode.into())
            .completion_type(self.completion_type.into())
            .output_stream(Stderr)
            .tab_stop(4)
            .build()
    }
}

impl Into<rustyline::ColorMode> for ColorMode {
    fn into(self) -> rustyline::ColorMode {
        use rustyline::config::ColorMode::{Disabled, Enabled, Forced};
        match self {
            ColorMode::Forced => Forced,
            ColorMode::Enabled => Enabled,
            ColorMode::Disabled => Disabled,
        }
    }
}

impl Into<rustyline::CompletionType> for CompletionType {
    fn into(self) -> rustyline::CompletionType {
        use rustyline::CompletionType::{Circular, List};

        match self {
            CompletionType::Circular => Circular,
            CompletionType::List => List,
        }
    }
}

#[derive(Clone, Copy)]
pub struct Helper<'a> {
    completer: &'a dyn Completer<Candidate = CompletionCandidate>,
    hinter: &'a dyn Hinter,
    highlighter: &'a dyn Highlighter,
}

impl Completer for Helper<'_> {
    type Candidate = CompletionCandidate;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        ctx: &Context<'_>,
    ) -> Result<(usize, Vec<Self::Candidate>), rustyline::error::ReadlineError> {
        self.completer.complete(line, pos, ctx)
    }

    fn update(&self, line: &mut LineBuffer, start: usize, elected: &str) {
        self.completer.update(line, start, elected)
    }
}

impl Hinter for Helper<'_> {
    fn hint(&self, line: &str, pos: usize, ctx: &Context<'_>) -> Option<String> {
        self.hinter.hint(line, pos, ctx)
    }
}

impl Validator for Helper<'_> {}

impl Highlighter for Helper<'_> {
    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        _default: bool,
    ) -> Cow<'b, str> {
        Borrowed(prompt)
    }
    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Cow::from(format!("{}", ansi_term::Color::White.dimmed().paint(hint)))
    }
    fn highlight_char(&self, _line: &str, _pos: usize) -> bool {
        true
    }
}

impl rustyline::Helper for Helper<'_> {}

#[derive(Debug, Clone)]
pub struct CompletionCandidate {
    pub replacement: String,
    pub kind: CandidateType,
}

impl Candidate for CompletionCandidate {
    fn display(&self) -> &str {
        &self.replacement
    }

    fn replacement(&self) -> &str {
        &self.replacement
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum CandidateType {
    MismatchedQuote,
    Value,
    Command,
    Argument,
}

#[cfg(test)]
mod test {
    use crate::config::ColorMode::Disabled;
    use crate::config::CompletionType::List;
    use crate::readline::Readline;
    use crate::Config;
    use std::io;

    #[test]
    fn test_sets_rustyline_config() {
        use rustyline::config::Configurer;
        use rustyline::config::OutputStreamType;
        use rustyline::ColorMode;
        use rustyline::CompletionType;

        let config = Config::default().color_mode(Disabled).completion_type(List);

        let mut readline = Readline::new(config);
        let readline_config = readline.editor.config_mut();
        assert_eq!(readline_config.color_mode(), ColorMode::Disabled);
        assert_eq!(readline_config.completion_type(), CompletionType::List);
        assert_eq!(readline_config.tab_stop(), 4);
        assert_eq!(readline_config.output_stream(), OutputStreamType::Stderr);
    }

    #[test]
    fn test_error_conversions() {
        use super::Error;
        use rustyline::error::ReadlineError;
        #[cfg(unix)]
        let io_error = io::Error::from_raw_os_error(22);
        #[cfg(unix)]
        let io_error_two = io::Error::from_raw_os_error(22);
        #[cfg(windows)]
        let io_error = io::Error::from_raw_os_error(10022);
        #[cfg(windows)]
        let io_error_two = io::Error::from_raw_os_error(10022);
        let io_error_display = io_error.to_string();

        let readline_io_error = Error::from(ReadlineError::Io(io_error));
        assert!(matches!(readline_io_error, Error::Io(_)));
        assert_eq!(readline_io_error.to_string(), io_error_display);

        let readline_io_error = Error::from(io_error_two);
        assert!(matches!(readline_io_error, Error::Io(_)));
        assert_eq!(readline_io_error.to_string(), io_error_display);

        let error = Error::from(ReadlineError::Interrupted);
        assert!(matches!(error, Error::Interrupted));
        assert_eq!(error.to_string(), ReadlineError::Interrupted.to_string());

        let error = Error::from(ReadlineError::Eof);
        assert!(matches!(error, Error::Eof));
        assert_eq!(error.to_string(), ReadlineError::Eof.to_string());

        let error = Error::from(ReadlineError::Utf8Error);
        assert!(matches!(error, Error::Utf8Error));
        assert_eq!(error.to_string(), ReadlineError::Utf8Error.to_string());

        if cfg!(unix) {
            use nix::errno::Errno;
            let error = Error::from(ReadlineError::Errno(nix::Error::Sys(Errno::E2BIG)));
            assert!(matches!(error, Error::Errno(_)));
            assert_eq!(
                error.to_string(),
                ReadlineError::Errno(nix::Error::Sys(Errno::E2BIG)).to_string()
            );
        }
    }
}
