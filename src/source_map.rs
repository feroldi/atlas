#[derive(Debug, PartialEq)]
struct BytePos(usize);

fn parse_start_pos_of_source_lines(source_text: &str) -> Vec<BytePos> {
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

fn lookup_line_index(_: &[BytePos], _: BytePos) -> Option<usize> {
    None
}

#[cfg(test)]
mod tests {
    mod parse_start_pos_of_source_lines {
        use crate::source_map::{parse_start_pos_of_source_lines, BytePos};

        #[test]
        fn empty_text() {
            let source_text = "";
            let source_line_pos = parse_start_pos_of_source_lines(source_text);
            assert_eq!(source_line_pos, vec![]);
        }

        #[test]
        fn text_without_newline() {
            let source_text = "some text without newline";
            let source_line_pos = parse_start_pos_of_source_lines(source_text);
            assert_eq!(source_line_pos, vec![BytePos(0usize)]);
        }

        #[test]
        fn text_with_newline_at_the_end() {
            let source_text = "abc\n";
            let source_line_pos = parse_start_pos_of_source_lines(source_text);
            assert_eq!(source_line_pos, vec![BytePos(0usize)]);
        }

        #[test]
        fn text_with_newline_in_the_middle() {
            let source_text = "abc\ndef";
            let source_line_pos = parse_start_pos_of_source_lines(source_text);
            assert_eq!(source_line_pos, vec![BytePos(0usize), BytePos(4usize)]);
        }

        #[test]
        fn text_with_newline_at_the_start() {
            let source_text = "\nabc";
            let source_line_pos = parse_start_pos_of_source_lines(source_text);
            assert_eq!(source_line_pos, vec![BytePos(0usize), BytePos(1usize)]);
        }

        #[test]
        fn text_with_various_newlines_at_the_start() {
            let source_text = "\n\n\nabc";
            let source_line_pos = parse_start_pos_of_source_lines(source_text);
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
            let source_line_pos = parse_start_pos_of_source_lines(source_text);
            assert_eq!(
                source_line_pos,
                vec![BytePos(0usize), BytePos(4usize), BytePos(5usize)]
            );
        }
    }

    mod lookup_line_index_tests {
        use crate::source_map::{lookup_line_index, BytePos};

        #[test]
        fn empty_start_pos_of_lines() {
            let start_pos_of_lines = Vec::<BytePos>::new();
            let line_start_pos = lookup_line_index(&start_pos_of_lines, BytePos(0));
            assert_eq!(line_start_pos, None::<usize>);
        }
    }
}
