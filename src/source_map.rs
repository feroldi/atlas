#[derive(Debug, PartialEq)]
struct BytePos {
    raw: usize,
}

fn parse_start_pos_of_source_lines(source_text: &str) -> Vec<BytePos> {
    std::iter::once(if source_text.is_empty() {
        None
    } else {
        Some(BytePos { raw: 0 })
    })
    .chain(source_text.bytes().enumerate().map(|(pos, byte)| {
        if byte == b'\n' && pos + 1 < source_text.len() {
            Some(BytePos { raw: pos + 1 })
        } else {
            None
        }
    }))
    .flatten()
    .collect()
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
    fn parse_start_pos_of_source_lines_from_text_without_newline() {
        let source_text = "some text without newline";
        let source_line_pos = parse_start_pos_of_source_lines(source_text);
        assert_eq!(source_line_pos, vec![BytePos { raw: 0usize }]);
    }

    #[test]
    fn parse_start_pos_of_source_lines_from_text_with_newline_at_the_end() {
        let source_text = "abc\n";
        let source_line_pos = parse_start_pos_of_source_lines(source_text);
        assert_eq!(source_line_pos, vec![BytePos { raw: 0usize }]);
    }

    #[test]
    fn parse_start_pos_of_source_lines_from_text_with_newline_in_the_middle() {
        let source_text = "abc\ndef";
        let source_line_pos = parse_start_pos_of_source_lines(source_text);
        assert_eq!(
            source_line_pos,
            vec![BytePos { raw: 0usize }, BytePos { raw: 4usize }]
        );
    }
}
