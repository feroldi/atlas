#![cfg(test)]

use lazy_static::lazy_static;
use proptest::prelude::*;
use proptest::string::string_regex;
use regex::escape;

mod char_stream_tests;
mod constant_parser_tests;
mod preprocessor_tests;
mod scanner_tests;
mod source_map_tests;

lazy_static! {
    static ref SOURCE_CHAR_PATTERN: String = {
        // C17 [5.2.1] Character sets
        let upper_alpha = &('A'..='Z').collect::<String>();
        let lower_alpha = &('a'..='z').collect::<String>();
        let digits = &('0'..='9').collect::<String>();
        let graphic_chars = r###"!"#%&'()*+,-./:;<=>?[\]^_{|}~"###;
        let spaces = "\x20\t\n\r\x0b\x0c";

        [upper_alpha, lower_alpha, digits, graphic_chars, spaces].join("")
    };

    static ref PRINTABLE_CHAR_PATTERN: String = {
        ('\x20'..='\x7e').collect::<_>()
    };
}

pub fn source_char() -> impl Strategy<Value = String> {
    string_regex(&format!("[{}]", escape(&SOURCE_CHAR_PATTERN))).unwrap()
}

pub fn source_chars() -> impl Strategy<Value = String> {
    string_regex(&format!("[{}]+", escape(&SOURCE_CHAR_PATTERN))).unwrap()
}

pub fn source_chars_except(excluded_chars: &[char]) -> impl Strategy<Value = String> {
    string_regex(&format!(
        "[{}]+",
        escape(&SOURCE_CHAR_PATTERN.replace(excluded_chars, ""))
    ))
    .unwrap()
}

pub fn non_source_char() -> impl Strategy<Value = String> {
    string_regex(&format!("[^{}\0]", escape(&SOURCE_CHAR_PATTERN))).unwrap()
}

pub fn printable_chars() -> impl Strategy<Value = String> {
    printable_chars_except(&[])
}

pub fn printable_chars_except(excluded_chars: &[char]) -> impl Strategy<Value = String> {
    string_regex(&format!(
        "[{}]+",
        &escape(&PRINTABLE_CHAR_PATTERN.replace(excluded_chars, ""))
    ))
    .unwrap()
}

pub fn whitespace() -> impl Strategy<Value = String> {
    let spaces = "\x20\t\n\r\x0b\x0c";

    string_regex(&format!("[{}]+", spaces)).unwrap()
}

pub fn newline() -> impl Strategy<Value = String> {
    string_regex("\n|\r|\n\r|\r\n").unwrap()
}

pub fn source_punctuation() -> impl Strategy<Value = String> {
    string_regex(&format!("[{}]", escape(r"!#%&()*+,-./:;<=>?[]^{|}~"))).unwrap()
}

pub fn is_start_of_prefixed_char_const_or_str_lit(text: &str) -> bool {
    use regex::Regex;

    Regex::new(r#"^[uUL]'|(u8|[uUL])""#).unwrap().is_match(text)
}
