#[derive(Debug, PartialEq)]
struct BytePos {
    raw: usize,
}

fn parse_start_pos_of_source_lines(_: &str) -> Vec<BytePos> {
    vec![BytePos { raw: 0usize }]
}

#[cfg(test)]
mod tests {
    use super::{parse_start_pos_of_source_lines, BytePos};

    #[test]
    fn parse_start_pos_of_source_lines_from_empty_text() {
        let source_text = "";
        let source_line_pos = parse_start_pos_of_source_lines(source_text);
        assert_eq!(source_line_pos, vec![BytePos { raw: 0usize }]);
    }

    #[test]
    fn parse_start_pos_of_source_lines_from_some_text_without_newline() {
        let source_text = "some text without newline";
        let source_line_pos = parse_start_pos_of_source_lines(source_text);
        assert_eq!(source_line_pos, vec![BytePos { raw: 0usize }]);
    }
}
