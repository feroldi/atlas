#[derive(Debug, PartialEq)]
struct BytePos {
    raw: usize,
}

fn parse_source_line_positions(_: &str) -> Vec<BytePos> {
    Vec::new()
}

#[cfg(test)]
mod tests {
    use super::{BytePos, parse_source_line_positions};

    #[test]
    fn parse_source_line_positions_from_empty_text() {
        let input = "";
        let source_line_pos = parse_source_line_positions(input);
        assert_eq!(source_line_pos, Vec::<BytePos>::new());
    }
}
