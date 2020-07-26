/*
 * error.rs is part of slaps.
 * Copyright (C) 2020 rd <slaps@megane.space>
 * License: WTFPL
 */
pub use crate::clap::{Error as ClapError, ErrorKind};
pub use crate::shlex::MismatchedQuotes;
use std::fmt::{Display, Formatter};
use std::{error, fmt, io};

/// Variant of all sub-types of errors returned by the library.
#[allow(clippy::pub_enum_variant_names)]
#[derive(Debug)]
pub enum Error {
    /// Error while parsing the input using clap.
    ClapError(ClapError),
    /// Error while reading a line from standard input.
    ReadlineError(ReadlineError),
    /// Mismatched quotes while parsing the input.
    ///
    /// Contains the position of the first mismatched quote within the original input.
    MismatchedQuotes(usize),
    /// Error returned by a command handler.
    ExecutionError(ExecutionError),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::ClapError(e) => e.fmt(f),
            Error::ReadlineError(e) => e.fmt(f),
            Error::MismatchedQuotes(_) => write!(f, "Mismatched quotes"),
            Error::ExecutionError(e) => e.fmt(f),
        }
    }
}

impl error::Error for Error {}

impl From<ClapError> for Error {
    fn from(e: ClapError) -> Self {
        Error::ClapError(e)
    }
}

impl From<ReadlineError> for Error {
    fn from(e: ReadlineError) -> Self {
        Error::ReadlineError(e)
    }
}

impl From<MismatchedQuotes> for Error {
    fn from(e: MismatchedQuotes) -> Self {
        Error::MismatchedQuotes(e.0)
    }
}

impl From<ExecutionError> for Error {
    fn from(e: ExecutionError) -> Self {
        Error::ExecutionError(e)
    }
}

/// The error type for Readline errors that can arise from
/// I/O related errors or Errno when using the nix-rust library
#[derive(Debug)]
#[allow(clippy::pub_enum_variant_names)]
#[allow(clippy::module_name_repetitions)]
#[non_exhaustive]
pub enum ReadlineError {
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
    Decode(std::char::DecodeUtf16Error),
}

impl Display for ReadlineError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ReadlineError::Io(ref err) => err.fmt(f),
            ReadlineError::Eof => write!(f, "EOF"),
            ReadlineError::Interrupted => write!(f, "Interrupted"),
            #[cfg(unix)]
            ReadlineError::Utf8Error => write!(f, "invalid utf-8: corrupt contents"),
            #[cfg(unix)]
            ReadlineError::Errno(ref err) => err.fmt(f),
            #[cfg(windows)]
            ReadlineError::WindowResize => write!(f, "WindowResize"),
            #[cfg(windows)]
            ReadlineError::Decode(ref err) => err.fmt(f),
        }
    }
}

impl std::error::Error for ReadlineError {}

impl From<io::Error> for ReadlineError {
    fn from(e: io::Error) -> Self {
        Self::Io(e)
    }
}

impl From<rustyline::error::ReadlineError> for ReadlineError {
    fn from(e: rustyline::error::ReadlineError) -> Self {
        use rustyline::error::ReadlineError as RustylineError;
        match e {
            RustylineError::Io(e) => Self::Io(e),
            RustylineError::Eof => Self::Eof,
            RustylineError::Interrupted => Self::Interrupted,
            #[cfg(unix)]
            RustylineError::Utf8Error => Self::Utf8Error,
            #[cfg(unix)]
            RustylineError::Errno(e) => Self::Errno(e),
            #[cfg(windows)]
            RustylineError::WindowResize => Self::WindowResize,
            #[cfg(windows)]
            RustylineError::Decode(e) => Self::Decode(e),
            _ => unreachable!(),
        }
    }
}

impl From<rustyline::error::ReadlineError> for Error {
    fn from(e: rustyline::error::ReadlineError) -> Self {
        ReadlineError::from(e).into()
    }
}

/// Error variant for command handlers.
#[derive(Debug)]
#[allow(clippy::module_name_repetitions)]
pub enum ExecutionError {
    /// Program should exit due to a fatal error encountered while executing the command.
    FatalExit(Box<dyn error::Error>),
    /// Program should exit gracefully.
    GracefulExit,
}

impl fmt::Display for ExecutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExecutionError::FatalExit(e) => write!(f, "{}", e),
            ExecutionError::GracefulExit => write!(f, "Success"),
        }
    }
}

impl error::Error for ExecutionError {}

#[cfg(test)]
mod tests {
    use std::io;

    #[test]
    fn test_readline_error_conversions() {
        use super::ReadlineError;
        use rustyline::error::ReadlineError as RustylineError;
        #[cfg(unix)]
        let io_error = io::Error::from_raw_os_error(22);
        #[cfg(unix)]
        let io_error_two = io::Error::from_raw_os_error(22);
        #[cfg(windows)]
        let io_error = io::Error::from_raw_os_error(10022);
        #[cfg(windows)]
        let io_error_two = io::Error::from_raw_os_error(10022);
        let io_error_display = io_error.to_string();

        let readline_io_error = ReadlineError::from(RustylineError::Io(io_error));
        assert!(matches!(readline_io_error, ReadlineError::Io(_)));
        assert_eq!(readline_io_error.to_string(), io_error_display);

        let readline_io_error = ReadlineError::from(io_error_two);
        assert!(matches!(readline_io_error, ReadlineError::Io(_)));
        assert_eq!(readline_io_error.to_string(), io_error_display);

        let error = ReadlineError::from(RustylineError::Interrupted);
        assert!(matches!(error, ReadlineError::Interrupted));
        assert_eq!(error.to_string(), RustylineError::Interrupted.to_string());

        let error = ReadlineError::from(RustylineError::Eof);
        assert!(matches!(error, ReadlineError::Eof));
        assert_eq!(error.to_string(), RustylineError::Eof.to_string());

        #[cfg(unix)]
        if cfg!(unix) {
            use nix::errno::Errno;
            let error = ReadlineError::from(RustylineError::Utf8Error);
            assert!(matches!(error, ReadlineError::Utf8Error));
            assert_eq!(error.to_string(), RustylineError::Utf8Error.to_string());

            let error = ReadlineError::from(RustylineError::Errno(nix::Error::Sys(Errno::E2BIG)));
            assert!(matches!(error, ReadlineError::Errno(_)));
            assert_eq!(
                error.to_string(),
                RustylineError::Errno(nix::Error::Sys(Errno::E2BIG)).to_string()
            );
        }
    }
}
