use std::ops::{Add, AddAssign};

/// This is an exclusive text range as in [start, end).
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Span {
    pub(crate) start: BytePos,
    pub(crate) end: BytePos,
}

impl Span {
    const EMPTY: Span = Span {
        start: BytePos::EMPTY,
        end: BytePos::EMPTY,
    };

    #[cfg(test)]
    pub(crate) fn from_raw_pos(start: usize, end: usize) -> Span {
        debug_assert!(start <= end, "cannot have start greater than end");

        Span {
            start: BytePos::from_usize(start),
            end: BytePos::from_usize(end),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Spanned<T> {
    value: T,
    pub(crate) span: Span,
}

impl<T> Spanned<T> {
    pub(crate) fn new(value: T, span: Span) -> Spanned<T> {
        Spanned { value, span }
    }

    pub(crate) const fn with_empty_span(value: T) -> Spanned<T> {
        Spanned {
            value,
            span: Span::EMPTY,
        }
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

pub trait Pos: Sized + Add + AddAssign {
    fn from_usize(value: usize) -> Self;
    fn to_usize(self) -> usize;
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct BytePos(usize);

impl BytePos {
    const EMPTY: BytePos = BytePos(usize::MAX);
}

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

// TODO: Write tests.
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
    mod impl_pos_for_byte_pos {
        use crate::source_map::{BytePos, Pos};

        #[test]
        fn create_byte_pos_from_usize() {
            let byte_pos = <BytePos as Pos>::from_usize(42usize);
            assert_eq!(byte_pos, BytePos(42));
        }

        #[test]
        fn convert_byte_pos_to_usize() {
            let byte_pos = BytePos(42);
            assert_eq!(<BytePos as Pos>::to_usize(byte_pos), 42usize);
        }
    }

    mod addition_of_byte_pos {
        use crate::source_map::{BytePos, Pos};

        #[test]
        fn add_two_byte_poses() {
            let p1 = BytePos::from_usize(2);
            let p2 = BytePos::from_usize(3);

            assert_eq!(p1 + p2, BytePos::from_usize(5));
        }

        #[test]
        fn add_assign_two_byte_poses() {
            let mut p1 = BytePos::from_usize(2);
            let p2 = BytePos::from_usize(3);

            p1 += p2;

            assert_eq!(p1, BytePos::from_usize(5));
        }
    }

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

    #[test]
    fn span_from_raw_pos_should_create_a_span_of_byte_poses() {
        use super::{BytePos, Span};

        assert_eq!(
            Span::from_raw_pos(123, 2949),
            Span {
                start: BytePos(123),
                end: BytePos(2949),
            }
        );

        assert_eq!(
            Span::from_raw_pos(3, 4),
            Span {
                start: BytePos(3),
                end: BytePos(4),
            }
        );

        assert_eq!(
            Span::from_raw_pos(0, 0),
            Span {
                start: BytePos(0),
                end: BytePos(0),
            }
        );
    }

    #[test]
    #[should_panic(expected = "cannot have start greater than end")]
    fn span_from_raw_pos_should_not_allow_start_greater_than_end() {
        use super::Span;
        let _ = Span::from_raw_pos(3, 2);
    }

    #[test]
    fn spanned_new_should_create_a_spanned_with_a_value_and_a_span() {
        use super::{Span, Spanned};

        let spanned = Spanned::new('a', Span::from_raw_pos(34, 53));

        assert_eq!(spanned.value, 'a');
        assert_eq!(spanned.span, Span::from_raw_pos(34, 53));

        let spanned = Spanned::new(42, Span::from_raw_pos(0, 3));

        assert_eq!(spanned.value, 42);
        assert_eq!(spanned.span, Span::from_raw_pos(0, 3));

        let spanned = Spanned::new("abc", Span::EMPTY);

        assert_eq!(spanned.value, "abc");
        assert_eq!(spanned.span, Span::EMPTY);
    }

    #[test]
    fn spanned_with_empty_span_should_create_a_spanned_with_a_value_and_empty_span() {
        use super::{Span, Spanned};

        let spanned = Spanned::with_empty_span('a');

        assert_eq!(spanned.value, 'a');
        assert_eq!(spanned.span, Span::EMPTY);

        let spanned = Spanned::with_empty_span(42);

        assert_eq!(spanned.value, 42);
        assert_eq!(spanned.span, Span::EMPTY);

        let spanned = Spanned::with_empty_span("abc");

        assert_eq!(spanned.value, "abc");
        assert_eq!(spanned.span, Span::EMPTY);
    }

    #[test]
    fn spanned_should_deref_to_its_internal_value() {
        use super::{Span, Spanned};

        #[derive(PartialEq, Debug)]
        struct S {
            i: i32,
        }

        let spanned = Spanned::new(S { i: 42 }, Span::from_raw_pos(34, 53));

        assert_eq!(*spanned, S { i: 42 });
        assert_eq!(spanned.i, 42);
    }
}
