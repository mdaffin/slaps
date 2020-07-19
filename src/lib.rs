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
