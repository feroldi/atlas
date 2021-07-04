struct CharBumper {}

// TODO: peek
// TODO: peek_is
// TODO: bump
// TODO: bump_if

impl CharBumper {
    fn peek(&self) -> Option<char> {
        None
    }
}

#[cfg(test)]
mod tests {
    mod char_bumper_peek {
        use crate::char_bumper::CharBumper;

        #[test]
        fn should_return_none_when_input_is_empty() {
            let char_bumper = CharBumper {};
            assert_eq!(char_bumper.peek(), None::<char>);
        }
    }
}
