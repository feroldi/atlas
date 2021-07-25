#[derive(Debug, PartialEq)]
struct BytePos(usize);

fn calc_lines_start_positions(source_text: &str) -> Vec<BytePos> {
    if source_text.is_empty() {
        vec![]
    } else {
        std::iter::once(0usize)
            .chain(source_text.match_indices('\n').map(|(idx, _)| idx + 1))
            .filter(|&pos| pos < source_text.len())
            .map(BytePos)
            .collect()
    }
}

fn lookup_line_index(lines_start_pos: &[BytePos], pos: BytePos) -> Option<usize> {
    lines_start_pos
        .iter()
        .rev()
        .position(|line_pos| line_pos.0 <= pos.0)
        .map(|line_index| lines_start_pos.len() - line_index - 1)
}

#[cfg(test)]
mod tests {
    mod calc_lines_start_positions {
        use crate::source_map::{calc_lines_start_positions, BytePos};

        #[test]
        fn empty_text() {
            let source_text = "";
            let source_line_pos = calc_lines_start_positions(source_text);
            assert_eq!(source_line_pos, vec![]);
        }

        #[test]
        fn text_without_newline() {
            let source_text = "some text without newline";
            let source_line_pos = calc_lines_start_positions(source_text);
            assert_eq!(source_line_pos, vec![BytePos(0usize)]);
        }

        #[test]
        fn text_with_newline_at_the_end() {
            let source_text = "abc\n";
            let source_line_pos = calc_lines_start_positions(source_text);
            assert_eq!(source_line_pos, vec![BytePos(0usize)]);
        }

        #[test]
        fn text_with_newline_in_the_middle() {
            let source_text = "abc\ndef";
            let source_line_pos = calc_lines_start_positions(source_text);
            assert_eq!(source_line_pos, vec![BytePos(0usize), BytePos(4usize)]);
        }

        #[test]
        fn text_with_newline_at_the_start() {
            let source_text = "\nabc";
            let source_line_pos = calc_lines_start_positions(source_text);
            assert_eq!(source_line_pos, vec![BytePos(0usize), BytePos(1usize)]);
        }

        #[test]
        fn text_with_various_newlines_at_the_start() {
            let source_text = "\n\n\nabc";
            let source_line_pos = calc_lines_start_positions(source_text);
            assert_eq!(
                source_line_pos,
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
            let source_line_pos = calc_lines_start_positions(source_text);
            assert_eq!(
                source_line_pos,
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
