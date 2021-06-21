#[derive(Debug, PartialEq)]
struct BytePos {
    raw: usize,
}

fn parse_start_pos_of_source_lines(source_text: &str) -> Vec<BytePos> {
    let mut start_pos_of_lines = if source_text.is_empty() {
        vec![]
    } else {
        vec![BytePos { raw: 0 }]
    };

    if source_text.ends_with('\n') {
        start_pos_of_lines.push(BytePos {
            raw: source_text.len() + 1,
        });
    }

    start_pos_of_lines
}

#[cfg(test)]
mod tests {
    use super::{parse_start_pos_of_source_lines, BytePos};

    #[test]
    fn parse_start_pos_of_source_lines_from_empty_text() {
        let source_text = "";
        let source_line_pos = parse_start_pos_of_source_lines(source_text);
        assert_eq!(source_line_pos, vec![]);
    }

    #[test]
    fn parse_start_pos_of_source_lines_from_some_text_without_newline() {
        let source_text = "some text without newline";
        let source_line_pos = parse_start_pos_of_source_lines(source_text);
        assert_eq!(source_line_pos, vec![BytePos { raw: 0usize }]);
    }

    #[test]
    fn parse_start_pos_of_source_lines_from_some_text_with_one_newline_at_the_end() {
        let source_text = "abc\n";
        let source_line_pos = parse_start_pos_of_source_lines(source_text);
        assert_eq!(
            source_line_pos,
            vec![
                BytePos { raw: 0usize },
                BytePos {
                    raw: source_text.len() + 1
                }
            ]
        );
    }
}
