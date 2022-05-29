use std::ops::{Add, AddAssign};

pub struct SourceFile<'a> {
    source_text: &'a str,
}

impl<'a> SourceFile<'a> {
    pub fn new(source_text: &str) -> SourceFile {
        SourceFile { source_text }
    }

    pub fn get_text_snippet(&self, span: impl Into<Span>) -> &'a str {
        let span = span.into();
        let (start_idx, end_idx) = (span.start.to_usize(), span.end.to_usize());
        &self.source_text[start_idx..end_idx]
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Spanned<T> {
        Spanned { value, span }
    }

    pub const fn with_dummy_span(value: T) -> Spanned<T> {
        Spanned {
            value,
            span: Span::DUMMY,
        }
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> From<Spanned<T>> for Span {
    fn from(spanned: Spanned<T>) -> Span {
        spanned.span
    }
}

/// This is an exclusive text range as in [start, end).
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Span {
    pub start: BytePos,
    pub end: BytePos,
}

impl Span {
    pub const DUMMY: Span = Span {
        start: BytePos::from_usize(0),
        end: BytePos::from_usize(0),
    };

    pub fn from_usizes(start: usize, end: usize) -> Span {
        debug_assert!(start <= end, "cannot have start greater than end");

        Span {
            start: BytePos::from_usize(start),
            end: BytePos::from_usize(end),
        }
    }
}

pub trait Pos: Sized + Add + AddAssign {
    fn from_usize(value: usize) -> Self;
    fn to_usize(self) -> usize;
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct BytePos(usize);

impl const Pos for BytePos {
    fn from_usize(value: usize) -> BytePos {
        BytePos(value)
    }

    fn to_usize(self) -> usize {
        self.0
    }
}

impl Add for BytePos {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Self::from_usize(self.to_usize() + rhs.to_usize())
    }
}

impl AddAssign for BytePos {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs
    }
}

impl std::convert::From<usize> for BytePos {
    fn from(value: usize) -> BytePos {
        BytePos::from_usize(value)
    }
}

#[allow(dead_code)]
fn calc_lines_positions(source_text: &str) -> Vec<BytePos> {
    std::iter::once(0usize)
        .chain(source_text.match_indices('\n').map(|(idx, _)| idx + 1))
        .filter(|&pos| pos < source_text.len())
        .map(BytePos)
        .collect()
}

#[allow(dead_code)]
fn lookup_line_index(lines_pos: &[BytePos], pos: BytePos) -> Option<usize> {
    lines_pos
        .iter()
        .rev()
        .position(|lines_pos| lines_pos.0 <= pos.0)
        .map(|line_index| lines_pos.len() - line_index - 1)
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use proptest::prelude::*;

    use crate::source_map::{BytePos, Pos, SourceFile, Span, Spanned};
    use crate::test_util::*;

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

    fn text_and_inbounds_byte_pos() -> impl Strategy<Value = (String, BytePos)> {
        source_chars()
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
            text in source_chars(),
            span in valid_span()
        ) {
            prop_assume!(span.start.to_usize() > text.len());

            let sf = SourceFile::new(&text);
            let result = std::panic::catch_unwind(|| sf.get_text_snippet(span));

            assert_matches!(result, Err(_));
        }
    }

    fn text_and_partially_out_of_bounds_span() -> impl Strategy<Value = (String, Span)> {
        source_chars()
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
}

#[cfg(test)]
mod legacy_tests {
    // These tests are here temporarily, because they don't have any use right
    // now, so they aren't part of the API right now, but in the future,
    // we'll have a diagnostics system that surely will make use of these
    // functions, so we keep them here as is for now.
    mod calc_lines_positions {
        use crate::source_map::{calc_lines_positions, BytePos};

        #[test]
        fn empty_text() {
            let source_text = "";
            let source_lines_pos = calc_lines_positions(source_text);
            assert_eq!(source_lines_pos, vec![]);
        }

        #[test]
        fn text_without_newline() {
            let source_text = "some text without newline";
            let source_lines_pos = calc_lines_positions(source_text);
            assert_eq!(source_lines_pos, vec![BytePos(0usize)]);
        }

        #[test]
        fn text_with_newline_at_the_end() {
            let source_text = "abc\n";
            let source_lines_pos = calc_lines_positions(source_text);
            assert_eq!(source_lines_pos, vec![BytePos(0usize)]);
        }

        #[test]
        fn text_with_newline_in_the_middle() {
            let source_text = "abc\ndef";
            let source_lines_pos = calc_lines_positions(source_text);
            assert_eq!(source_lines_pos, vec![BytePos(0usize), BytePos(4usize)]);
        }

        #[test]
        fn text_with_newline_at_the_start() {
            let source_text = "\nabc";
            let source_lines_pos = calc_lines_positions(source_text);
            assert_eq!(source_lines_pos, vec![BytePos(0usize), BytePos(1usize)]);
        }

        #[test]
        fn text_with_various_newlines_at_the_start() {
            let source_text = "\n\n\nabc";
            let source_lines_pos = calc_lines_positions(source_text);
            assert_eq!(
                source_lines_pos,
                vec![
                    BytePos(0usize),
                    BytePos(1usize),
                    BytePos(2usize),
                    BytePos(3usize),
                ]
            );
        }

        #[test]
        fn text_with_various_newlines_at_the_end() {
            let source_text = "abc\n\n\n";
            let source_lines_pos = calc_lines_positions(source_text);
            assert_eq!(
                source_lines_pos,
                vec![BytePos(0usize), BytePos(4usize), BytePos(5usize)]
            );
        }
    }

    mod lookup_line_index {
        use crate::source_map::{lookup_line_index, BytePos};

        #[test]
        fn empty_start_pos_of_lines() {
            let start_pos_of_lines = Vec::<BytePos>::new();
            let line_index = lookup_line_index(&start_pos_of_lines, BytePos(0));
            assert_eq!(line_index, None::<usize>);
        }

        #[test]
        fn one_line() {
            let start_pos_of_lines = vec![BytePos(0)];

            let line_index = lookup_line_index(&start_pos_of_lines, BytePos(0));
            assert_eq!(line_index, Some(0usize));

            let line_index = lookup_line_index(&start_pos_of_lines, BytePos(1));
            assert_eq!(line_index, Some(0usize));

            let line_index = lookup_line_index(&start_pos_of_lines, BytePos(100));
            assert_eq!(line_index, Some(0usize));

            let line_index = lookup_line_index(&start_pos_of_lines, BytePos(5000));
            assert_eq!(line_index, Some(0usize));
        }

        #[test]
        fn should_return_index_of_the_first_line_whose_pos_is_less_or_equal_to_input_pos() {
            let start_pos_of_lines = vec![BytePos(0), BytePos(5), BytePos(10)];

            let line_index = lookup_line_index(&start_pos_of_lines, BytePos(0));
            assert_eq!(line_index, Some(0usize));
            let line_index = lookup_line_index(&start_pos_of_lines, BytePos(4));
            assert_eq!(line_index, Some(0usize));

            let line_index = lookup_line_index(&start_pos_of_lines, BytePos(5));
            assert_eq!(line_index, Some(1usize));
            let line_index = lookup_line_index(&start_pos_of_lines, BytePos(9));
            assert_eq!(line_index, Some(1usize));

            let line_index = lookup_line_index(&start_pos_of_lines, BytePos(10));
            assert_eq!(line_index, Some(2usize));
            let line_index = lookup_line_index(&start_pos_of_lines, BytePos(50));
            assert_eq!(line_index, Some(2usize));
        }

        #[test]
        fn contiguous_lines() {
            let start_pos_of_lines = vec![BytePos(0), BytePos(1), BytePos(2)];

            let line_index = lookup_line_index(&start_pos_of_lines, BytePos(0));
            assert_eq!(line_index, Some(0usize));

            let line_index = lookup_line_index(&start_pos_of_lines, BytePos(1));
            assert_eq!(line_index, Some(1usize));

            let line_index = lookup_line_index(&start_pos_of_lines, BytePos(2));
            assert_eq!(line_index, Some(2usize));
        }
    }
}
