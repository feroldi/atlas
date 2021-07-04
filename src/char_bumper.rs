use std::marker::PhantomData;

struct CharBumper<'str> {
    _phantom: PhantomData<&'str str>,
}

// TODO: peek_is
// TODO: bump
// TODO: bump_if

impl<'a> From<&'a str> for CharBumper<'a> {
    fn from(_: &'a str) -> CharBumper<'a> {
        CharBumper {
            _phantom: PhantomData,
        }
    }
}

impl CharBumper<'_> {
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
            let char_bumper = CharBumper::from("");
            assert_eq!(char_bumper.peek(), None::<char>);
        }
    }
}
