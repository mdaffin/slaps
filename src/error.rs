/*
 * error.rs is part of slaps.
 * Copyright (C) 2020 rd <slaps@megane.space>
 * License: WTFPL
 */
pub use crate::clap::{Error as ClapError, ErrorKind};
pub use crate::readline::Error as ReadlineError;
pub use crate::shlex::MismatchedQuotes;
use std::fmt::{Display, Formatter};

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
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl std::error::Error for Error {}

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
