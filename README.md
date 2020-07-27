[![Crates.io](https://img.shields.io/crates/v/slaps)](https://crates.io/crates/slaps)
[![Documentation](https://docs.rs/slaps/badge.svg)](https://docs.rs/slaps)
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/tjg1/slaps/Continuous%20integration)](https://github.com/tjg1/slaps/actions?query=workflow%3A%22Continuous+integration%22)
[![Coveralls github](https://img.shields.io/coveralls/github/tjg1/slaps)](https://coveralls.io/github/tjg1/slaps)

Slaps adds an interactive CLI mode to your Rust command-line applications 
using your existing [Clap](https://github.com/clap-rs/clap) configuration.

It combines Clap with [rustyline](https://github.com/kkawakam/rustyline) to provide line editing, 
auto-completion and syntax highlighting.

Example
-------

This:

```rust
use slaps::Slaps;

// Using structopt
Slaps::with_name("slaps")
    .command(RekeyCommand::clap(), |args| Ok(()))
    .command(AccountCommand::clap(), |args| Ok(()))
    .command(UndoCommand::clap(), |args| Ok(()))
    .run()?;
```

Gets you this:

![Terminal screenshot](https://files.catbox.moe/rc4ch7.png)

Zero extra configuration needed.

Notes
-----

This project is currently in early stages of development and any changes are motivated by it being part of my personal 
toolkit to quickly build Rust command-line applications.
Most of it was extracted wholesale from the first project I built while learning Rust.
It abuses the hell out of Clap's `doc(hidden)` internals to minimise the overhead of the glue code holding everything 
together.
This makes things likely to break with future versions of Clap.
You have been warned, your warranty is now void, etc.
