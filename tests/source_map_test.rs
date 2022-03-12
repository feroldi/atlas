#![feature(assert_matches)]

use atlas::source_map::{BytePos, Pos, SourceFile, Span, Spanned};
use proptest::prelude::*;
use proptest::string::string_regex;
use std::assert_matches::assert_matches;

proptest! {
    #[test]
    fn convert_byte_pos_to_and_from_usize(value: usize) {
        let byte_pos = <BytePos as Pos>::from_usize(value);
        assert_eq!(<BytePos as Pos>::to_usize(byte_pos), value);
    }
}

proptest! {
    #[test]
    fn add_two_byte_poses(a: usize, b: usize) {
        prop_assume!(a.checked_add(b).is_some());

        let p1 = BytePos::from_usize(a);
        let p2 = BytePos::from_usize(b);

        assert_eq!(p1 + p2, BytePos::from_usize(a + b));
    }
}

proptest! {
    #[test]
    fn add_assign_two_byte_poses(a: usize, b: usize) {
        prop_assume!(a.checked_add(b).is_some());

        let mut p1 = BytePos::from_usize(a);
        let p2 = BytePos::from_usize(b);

        p1 += p2;

        assert_eq!(p1, BytePos::from_usize(a + b));
    }
}

proptest! {
    #[test]
    fn span_from_usizes_should_create_a_span_of_byte_poses(start: usize, end: usize) {
        prop_assume!(start <= end);

        let span = Span::from_usizes(start, end);

        assert_eq!(
            span,
            Span {
                start: BytePos::from_usize(start),
                end: BytePos::from_usize(end),
            }
        );
    }
}

proptest! {
    #[test]
    fn span_from_usizes_should_not_allow_start_greater_than_end(start: usize, end: usize) {
        prop_assume!(start > end);

        let result = std::panic::catch_unwind(|| Span::from_usizes(start, end));

        assert_matches!(result, Err(_));
    }
}

fn valid_span() -> impl Strategy<Value = Span> {
    any::<usize>()
        .prop_flat_map(|end| (0..end, Just(end)))
        .prop_map(|(start, end)| Span {
            start: start.into(),
            end: end.into(),
        })
}

proptest! {
    #[test]
    fn span_from_spanned(span in valid_span()) {
        struct S;

        let spanned = Spanned::new(S, span);

        assert_eq!(Span::from(spanned), span);
    }
}

proptest! {
    #[test]
    fn spanned_new_should_create_a_spanned_with_a_value_and_a_span(span in valid_span()) {
        #[derive(PartialEq, Debug)]
        struct S;

        let spanned = Spanned::new(S, span);

        assert_eq!(spanned.value, S);
        assert_eq!(spanned.span, span);
    }
}

#[test]
fn spanned_new_should_accept_a_dummy_span() {
    #[derive(PartialEq, Debug)]
    struct S;

    let spanned = Spanned::new(S, Span::DUMMY);

    assert_eq!(spanned.value, S);
    assert_eq!(spanned.span, Span::DUMMY);
}

#[test]
fn spanned_with_dummy_span_should_create_a_spanned_with_a_value_and_dummy_span() {
    #[derive(PartialEq, Debug)]
    struct S;

    let spanned = Spanned::with_dummy_span(S);

    assert_eq!(spanned.value, S);
    assert_eq!(spanned.span, Span::DUMMY);
}

proptest! {
    #[test]
    fn spanned_should_deref_to_its_internal_value(span in valid_span()) {
        #[derive(PartialEq, Debug)]
        struct S {
            i: i32,
        }

        let spanned = Spanned::new(S { i: 42 }, span);

        assert_eq!(*spanned, S { i: 42 });
        assert_eq!(spanned.i, 42);
    }
}

proptest! {
    #[test]
    fn usize_should_be_convertible_to_byte_pos(value: usize) {
        let bp = <usize as Into<BytePos>>::into(value);

        assert_eq!(bp, BytePos::from_usize(value));
    }
}

proptest! {
    #[test]
    fn get_text_snippet_should_return_empty_string_for_dummy_span(text in ".*") {
        let sf = SourceFile::new(&text);

        assert_eq!(sf.get_text_snippet(Span::DUMMY), "");
    }
}

fn c_lang_charset() -> impl Strategy<Value = String> {
    // C17 [5.2.1] Character sets
    let upper_alpha = "A-Z";
    let lower_alpha = "a-z";
    let digits = "0-9";
    let graphic_chars = r###"!"#%&'()*+,-./:;<=>?[\\]^_{|}~"###;
    let spaces = "\x20\t\n\r\x0b\x0c";

    // TODO(feroldi): Check if there are any missing control characters from the standard.
    let charset = [upper_alpha, lower_alpha, digits, graphic_chars, spaces].join("");

    string_regex(&format!("[{}]+", charset)).unwrap()
}

fn text_and_inbounds_byte_pos() -> impl Strategy<Value = (String, BytePos)> {
    c_lang_charset()
        .prop_flat_map(|text| (0..text.len(), Just(text)))
        .prop_map(|(index, text)| (text, index.into()))
}

proptest! {
    #[test]
    fn get_text_snippet_should_return_empty_string_for_empty_span_and_non_empty_text(
        (text, byte_pos) in text_and_inbounds_byte_pos()
    ) {
        let sf = SourceFile::new(&text);

        let empty_span = Span {
            start: byte_pos,
            end: byte_pos
        };

        assert_eq!(sf.get_text_snippet(empty_span), "");
    }
}

proptest! {
    #[test]
    fn get_text_snippet_should_panic_if_span_is_non_dummy_and_text_is_empty(
        span in valid_span().prop_filter("no dummy", |&span| span != Span::DUMMY)
    ) {
        let sf = SourceFile::new("");
        let result = std::panic::catch_unwind(|| sf.get_text_snippet(span));

        assert_matches!(result, Err(_));
    }
}

proptest! {
    #[test]
    fn get_text_snippet_should_panic_on_fully_out_of_bounds_span(
        text in c_lang_charset(),
        span in valid_span()
    ) {
        prop_assume!(span.start.to_usize() > text.len());

        let sf = SourceFile::new(&text);
        let result = std::panic::catch_unwind(|| sf.get_text_snippet(span));

        assert_matches!(result, Err(_));
    }
}

fn text_and_partially_out_of_bounds_span() -> impl Strategy<Value = (String, Span)> {
    c_lang_charset()
        .prop_flat_map(|text| (0..text.len(), text.len().., Just(text)))
        .prop_map(|(inbounds_index, outbounds_index, text)| {
            (text, Span::from_usizes(inbounds_index, outbounds_index))
        })
}

proptest! {
    #[test]
    fn get_text_snippet_should_panic_on_partially_out_of_bounds_span(
        (text, span) in text_and_partially_out_of_bounds_span()
    ) {
        let sf = SourceFile::new(&text);
        let result = std::panic::catch_unwind(|| sf.get_text_snippet(span));

        assert_matches!(result, Err(_));
    }
}

proptest! {
    #[test]
    fn get_text_snippet_should_return_substring_for_the_exclusive_range_of_a_non_dummy_span(
        prefix in ".*",
        infix in ".*",
        suffix in ".*",
    ) {
        let start = prefix.len();
        let end = start + infix.len();
        let span = Span::from_usizes(start, end);

        let expected_snippet = infix.clone();

        let text = [prefix, infix, suffix].concat();
        let sf = SourceFile::new(&text);

        assert_eq!(sf.get_text_snippet(span), &expected_snippet);
    }
}

proptest! {
    #[test]
    fn get_text_snippet_should_return_whole_text_if_span_covers_it(
        text in ".*",
    ) {
        let sf = SourceFile::new(&text);

        let span = Span::from_usizes(0, text.len());

        assert_eq!(sf.get_text_snippet(span), &text);
    }
}

#[test]
fn get_text_snippet_should_work_with_utf8_text() {
    let sf = SourceFile::new("🗻∈🌏");

    assert_eq!(sf.get_text_snippet(Span::from_usizes(0, 4)), "🗻");
    assert_eq!(sf.get_text_snippet(Span::from_usizes(4, 7)), "∈");
    assert_eq!(sf.get_text_snippet(Span::from_usizes(7, 11)), "🌏");
}

#[test]
fn get_text_snippet_should_panic_if_span_is_broken_utf8() {
    let sf = SourceFile::new("\u{0800}");

    let span = Span::from_usizes(0, 1);
    let result = std::panic::catch_unwind(|| sf.get_text_snippet(span));

    assert_matches!(result, Err(_));
}

#[test]
fn get_text_snippet_should_panic_if_empty_span_start_is_not_a_utf_8_char_boundary() {
    let sf = SourceFile::new("\u{0800}");

    let span = Span::from_usizes(1, 1);
    let result = std::panic::catch_unwind(|| sf.get_text_snippet(span));

    assert_matches!(result, Err(_));
}
