/*
 * readline.rs is part of slaps.
 * Copyright (C) 2020 rd <slaps@megane.space>
 * License: WTFPL
 */

use slaps::Slaps;

fn main() {
    let mut slaps = Slaps::new();

    eprintln!("{:?}", slaps);

    let line = slaps.prompt_line().unwrap();
    println!("Hello, {}", line);

    let password = slaps.prompt_password().unwrap();
    println!("Password was {} characters long!", password.len());

    let (new_password, new_password_confirm) = slaps.prompt_new_password().unwrap();
    if new_password.is_empty() && new_password_confirm.is_empty() {
        println!("You did not enter a password!")
    } else if new_password == new_password_confirm {
        println!("Passwords match!");
    } else {
        println!("Passwords do not match!")
    }
}
