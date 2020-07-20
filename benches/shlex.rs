/*
 * shlex.rs is part of slaps.
 * Copyright (C) 2020 rd <slaps@megane.space>
 * License: WTFPL
 */
#![feature(test)]

extern crate test;

use slaps::shlex::split;
use test::Bencher;

#[bench]
fn bench_splits_simple(b: &mut Bencher) {
    b.iter(|| split("foo bar"));
}

#[bench]
fn bench_splits_removes_escape(b: &mut Bencher) {
    b.iter(|| split(r#"\ foo b\"ar a\\rb"#));
}

#[bench]
fn bench_splits_single_quoted(b: &mut Bencher) {
    b.iter(|| split(r#"'f  oo' b'a r' 'a r'"#));
}

#[bench]
fn bench_splits_double_quoted(b: &mut Bencher) {
    b.iter(|| split(r#"f  oo" b"a r" "a r"b"#));
}

#[bench]
fn bench_splits_quoted_removes_escape(b: &mut Bencher) {
    b.iter(|| split(r#""f oo" b"a\" r" "a r"b"#));
}

#[bench]
fn bench_splits_args(b: &mut Bencher) {
    b.iter(|| split(r#"'--foo=abc'   "-s" " --long""#));
}

#[bench]
fn bench_splits_mismatched_quotes(b: &mut Bencher) {
    b.iter(|| split(r#"foo    ' aaa"#));
}

#[bench]
fn bench_splits_double_dash(b: &mut Bencher) {
    b.iter(|| split(r#"--foo '--'  -z   --aaa"#));
}

#[bench]
fn bench_is_argument(b: &mut Bencher) {
    let fields = split("--abc -d e -- --fgh").unwrap();

    b.iter(|| fields[0].is_argument());
    b.iter(|| fields[1].is_argument());
    b.iter(|| fields[2].is_argument());
    b.iter(|| fields[3].is_argument());
    b.iter(|| fields[4].is_argument());
}

#[bench]
fn bench_argument_name_and_value(b: &mut Bencher) {
    let fields = split("--a=123 -d zeee").unwrap();

    b.iter(|| fields[0].argument_name_and_value());
    b.iter(|| fields[1].argument_name_and_value());
    b.iter(|| fields[2].argument_name_and_value());
}

#[bench]
fn bench_empty(b: &mut Bencher) {
    b.iter(|| split(r#""#));
}

#[bench]
fn bench_all_whitespace(b: &mut Bencher) {
    b.iter(|| split(r#"   "#));
}
