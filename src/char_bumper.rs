use std::str::Chars;

struct CharBumper<'chars> {
    chars: Chars<'chars>,
    peek_char: Option<char>,
}

// TODO: peek_is
// TODO: bump_if

impl<'a> From<&'a str> for CharBumper<'a> {
    fn from(source: &'a str) -> CharBumper<'a> {
        let mut chars = source.chars();
        let peek_char = chars.next();
        CharBumper { chars, peek_char }
    }
}

impl CharBumper<'_> {
    fn peek(&self) -> Option<char> {
        self.peek_char
    }

    fn bump(&mut self) -> Option<char> {
        let old_peek = self.peek();
        self.peek_char = self.chars.next();
        old_peek
    }
}

#[cfg(test)]
mod tests {
    use crate::char_bumper::CharBumper;

    #[test]
    fn peek_should_return_none_when_input_is_empty() {
        let char_bumper = CharBumper::from("");
        assert_eq!(char_bumper.peek(), None::<char>);
    }

    #[test]
    fn peek_should_return_first_char_from_input_when_it_is_not_empty() {
        let char_bumper = CharBumper::from("abc");
        assert_eq!(char_bumper.peek(), Some('a'));
    }

    #[test]
    fn multiple_calls_to_peek_should_return_the_same_char() {
        let char_bumper = CharBumper::from("abc");

        assert_eq!(char_bumper.peek(), Some('a'));
        assert_eq!(char_bumper.peek(), Some('a'));
        assert_eq!(char_bumper.peek(), Some('a'));
    }

    #[test]
    fn bump_should_return_none_when_input_is_empty() {
        let mut char_bumper = CharBumper::from("");
        assert_eq!(char_bumper.bump(), None::<char>);
    }

    #[test]
    fn bump_should_return_first_char_from_input_when_it_is_not_empty() {
        let mut char_bumper = CharBumper::from("abc");
        assert_eq!(char_bumper.bump(), Some('a'));
    }

    #[test]
    fn bump_should_advance_the_peeking_character_to_the_next_char() {
        let mut char_bumper = CharBumper::from("abc");
        let _ = char_bumper.bump();
        assert_eq!(char_bumper.peek(), Some('b'));
    }

    #[test]
    fn bump_should_return_none_when_all_chars_have_been_bumped() {
        let mut char_bumper = CharBumper::from("abc");
        let _ = char_bumper.bump();
        let _ = char_bumper.bump();
        let _ = char_bumper.bump();
        assert_eq!(char_bumper.bump(), None);
    }

    #[test]
    fn peek_should_return_none_when_all_chars_have_been_bumped() {
        let mut char_bumper = CharBumper::from("abc");
        let _ = char_bumper.bump();
        let _ = char_bumper.bump();
        let _ = char_bumper.bump();
        assert_eq!(char_bumper.peek(), None);
    }
}
