use std::assert_matches::debug_assert_matches;
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

        debug_assert_matches!(
            {
                let source_bytes = self.source_text.as_bytes();
                let snippet_bytes = &source_bytes[start_idx..end_idx];
                std::str::from_utf8(snippet_bytes)
            },
            Ok(_),
            "span is an invalid UTF-8 sequence"
        );

        // SAFETY: Span is always a valid utf-8 snippet.
        unsafe { self.source_text.get_unchecked(start_idx..end_idx) }
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

// TODO: Test.
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
    // TODO: Should we try to normalize spans with start == end to a dummy?
    pub const DUMMY: Span = Span {
        start: BytePos(0),
        end: BytePos(0),
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

impl Pos for BytePos {
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
    // These tests are here temporarily, because they don't have any use right now, so they aren't
    // part of the API right now, but in the future, we'll have a diagnostics system that surely will
    // make use of these functions, so we keep them here as is for now.
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
