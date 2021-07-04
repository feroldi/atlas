use std::marker::PhantomData;

struct CharBumper<'str> {
    source: &'str str,
}

// TODO: peek_is
// TODO: bump
// TODO: bump_if

impl<'a> From<&'a str> for CharBumper<'a> {
    fn from(value: &'a str) -> CharBumper<'a> {
        CharBumper { source: value }
    }
}

impl CharBumper<'_> {
    fn peek(&self) -> Option<char> {
        self.source.chars().next()
    }

    fn bump(&self) -> Option<char> {
        self.peek()
    }
}

#[cfg(test)]
mod tests {
    mod peek {
        use crate::char_bumper::CharBumper;

        #[test]
        fn should_return_none_when_input_is_empty() {
            let char_bumper = CharBumper::from("");
            assert_eq!(char_bumper.peek(), None::<char>);
        }

        #[test]
        fn should_return_first_char_from_input_when_it_is_not_empty() {
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
    }

    mod bump {
        use crate::char_bumper::CharBumper;

        #[test]
        fn should_return_none_when_input_is_empty() {
            let char_bumper = CharBumper::from("");
            assert_eq!(char_bumper.bump(), None::<char>);
        }

        #[test]
        fn should_return_first_char_from_input_when_it_is_not_empty() {
            let char_bumper = CharBumper::from("abc");
            assert_eq!(char_bumper.bump(), Some('a'));
        }
    }
}
