/*
 * readline.rs is part of slaps.
 * Copyright (C) 2020 rd <slaps@megane.space>
 * License: WTFPL
 */

use slaps::Slaps;

fn main() {
    let slaps = Slaps::with_name("slaps");

    eprintln!("{:?}", slaps);

    let line = slaps.prompt_line(None).unwrap();
    println!("Hello, {}", line);

    let password = slaps.prompt_password(None).unwrap();
    println!("Password was {} characters long!", password.len());
}
