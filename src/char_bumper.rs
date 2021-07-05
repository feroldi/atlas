use std::str::Chars;

/// A string consumer that allows peeking and bumping characters.
///
/// # Examples
/// ```
/// use atlas::char_bumper::CharBumper;
///
/// let mut char_bumper = CharBumper::from("ab");
///
/// assert_eq!(char_bumper.peek(), Some('a'));
/// assert!(char_bumper.peek_is('a'));
///
/// assert_eq!(char_bumper.bump(), Some('a'));
/// assert!(char_bumper.peek_is('b'));
///
/// assert!(!char_bumper.bump_if('c'));
/// assert!(char_bumper.peek_is('b'));
///
/// assert!(char_bumper.bump_if('b'));
/// assert_eq!(char_bumper.peek(), None);
/// ```
pub struct CharBumper<'chars> {
    chars: Chars<'chars>,
    peek_char: Option<char>,
}

impl<'a> From<&'a str> for CharBumper<'a> {
    fn from(source: &'a str) -> CharBumper<'a> {
        let mut chars = source.chars();
        let peek_char = chars.next();
        CharBumper { chars, peek_char }
    }
}

impl CharBumper<'_> {
    /// Returns the current character.
    ///
    /// # Examples
    /// ```
    /// use atlas::char_bumper::CharBumper;
    ///
    /// let cb = CharBumper::from("abc");
    /// assert_eq!(cb.peek(), Some('a'));
    ///
    /// let cb = CharBumper::from("");
    /// assert_eq!(cb.peek(), None);
    /// ```
    pub fn peek(&self) -> Option<char> {
        self.peek_char
    }

    /// Returns the peeking character, and advances it to the next char.
    ///
    /// # Examples
    /// ```
    /// use atlas::char_bumper::CharBumper;
    ///
    /// let mut cb = CharBumper::from("abc");
    ///
    /// assert_eq!(cb.bump(), Some('a'));
    /// assert_eq!(cb.peek(), Some('b'));
    ///
    /// assert_eq!(cb.bump(), Some('b'));
    /// assert_eq!(cb.peek(), Some('c'));
    ///
    /// assert_eq!(cb.bump(), Some('c'));
    /// assert_eq!(cb.peek(), None);
    ///
    /// assert_eq!(cb.bump(), None);
    /// assert_eq!(cb.bump(), None);
    /// assert_eq!(cb.bump(), None);
    /// ```
    pub fn bump(&mut self) -> Option<char> {
        let old_peek = self.peek();
        self.peek_char = self.chars.next();
        old_peek
    }

    /// Returns whether the peeking character is equal to `ch`.
    ///
    /// # Examples
    /// ```
    /// use atlas::char_bumper::CharBumper;
    ///
    /// let cb = CharBumper::from("abc");
    ///
    /// assert!(cb.peek_is('a'));
    /// assert!(!cb.peek_is('b'));
    /// ```
    pub fn peek_is(&self, ch: char) -> bool {
        self.peek() == Some(ch)
    }

    /// Bumps the peeking character if it is equal to `ch`.
    ///
    /// Returns `true` when it bumps the peeking character, and `false` otherwise.
    ///
    /// # Examples
    /// ```
    /// use atlas::char_bumper::CharBumper;
    ///
    /// let mut cb = CharBumper::from("abc");
    ///
    /// assert!(!cb.bump_if('b'));
    /// assert!(cb.peek_is('a'));
    ///
    /// assert!(cb.bump_if('a'));
    /// assert!(cb.peek_is('b'));
    /// ```
    pub fn bump_if(&mut self, ch: char) -> bool {
        let is_ch_peek = self.peek_is(ch);
        if is_ch_peek {
            self.bump();
        }
        is_ch_peek
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

    #[test]
    fn peek_is_should_return_true_when_arg_equals_peek_char() {
        let char_bumper = CharBumper::from("abc");
        assert!(char_bumper.peek_is('a'));
    }

    #[test]
    fn peek_is_should_return_false_when_arg_differs_from_peek_char() {
        let char_bumper = CharBumper::from("abc");
        assert!(!char_bumper.peek_is('b'));
    }

    #[test]
    fn peek_is_should_not_advance_peeking_char() {
        let char_bumper = CharBumper::from("abc");
        let peek_before = char_bumper.peek();
        let _ = char_bumper.peek_is('a');
        let _ = char_bumper.peek_is('b');
        let peek_after = char_bumper.peek();
        assert_eq!(peek_before, peek_after);
    }

    #[test]
    fn bump_if_should_return_true_when_arg_equals_peek_char() {
        let mut char_bumper = CharBumper::from("abc");
        assert!(char_bumper.bump_if('a'));
    }

    #[test]
    fn bump_if_should_return_false_when_arg_differs_from_peek_char() {
        let mut char_bumper = CharBumper::from("abc");
        assert!(!char_bumper.bump_if('b'));
    }

    #[test]
    fn bump_if_should_advance_peeking_char_if_it_returns_true() {
        let mut char_bumper = CharBumper::from("abc");
        assert_eq!(char_bumper.peek(), Some('a'));
        assert!(char_bumper.bump_if('a'));
        assert_eq!(char_bumper.peek(), Some('b'));
    }

    #[test]
    fn bump_if_should_not_advance_peeking_char_if_it_returns_false() {
        let mut char_bumper = CharBumper::from("abc");
        assert_eq!(char_bumper.peek(), Some('a'));
        assert!(!char_bumper.bump_if('b'));
        assert_eq!(char_bumper.peek(), Some('a'));
    }
}
