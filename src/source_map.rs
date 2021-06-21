#[derive(Debug, PartialEq)]
struct BytePos {
    raw: usize,
}

fn parse_start_pos_of_source_lines(_: &str) -> Vec<BytePos> {
    Vec::new()
}

#[cfg(test)]
mod tests {
    use super::{parse_start_pos_of_source_lines, BytePos};

    #[test]
    fn parse_start_pos_of_source_lines_from_empty_text() {
        let input = "";
        let source_line_pos = parse_start_pos_of_source_lines(input);
        assert_eq!(source_line_pos, Vec::<BytePos>::new());
    }
}
