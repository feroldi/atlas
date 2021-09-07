use crate::source_map::{BytePos, Pos};
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
    peek_char_byte_pos: BytePos,
}

impl<'a> From<&'a str> for CharBumper<'a> {
    fn from(source: &'a str) -> CharBumper<'a> {
        let mut chars = source.chars();
        let peek_char = chars.next();
        CharBumper {
            chars,
            peek_char,
            peek_char_byte_pos: BytePos::from_usize(0),
        }
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
        if let Some(ch) = old_peek {
            self.peek_char_byte_pos += BytePos::from_usize(ch.len_utf8());
        }
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

    /// Returns the byte position of the peeking character.
    ///
    /// As [`bump`][bump] is called, the byte position of the peeking character is updated. At each
    /// update, the UTF-8 byte length of the former peeking character is taken into account, as to
    /// correctly point to the next character.
    ///
    /// # Examples
    ///
    /// The following code shows the byte position being advanced by one byte at a time for an
    /// input made of ASCII-only characters.
    ///
    /// ```
    /// # use atlas::{char_bumper::CharBumper, source_map::{BytePos, Pos}};
    /// let mut cb = CharBumper::from("abc");
    ///
    /// assert_eq!(cb.get_byte_pos_of_peeking_char(), BytePos::from_usize(0));
    ///
    /// cb.bump();
    /// assert_eq!(cb.get_byte_pos_of_peeking_char(), BytePos::from_usize(1));
    ///
    /// cb.bump();
    /// assert_eq!(cb.get_byte_pos_of_peeking_char(), BytePos::from_usize(2));
    /// ```
    ///
    /// When the input string contains UTF-8 characters, the byte position is updated according to
    /// the byte length of said characters.
    ///
    /// ```
    /// # use atlas::{char_bumper::CharBumper, source_map::{BytePos, Pos}};
    /// // PILE OF POO is 4-byte long.
    /// let mut cb = CharBumper::from("💩");
    ///
    /// assert_eq!(cb.get_byte_pos_of_peeking_char(), BytePos::from_usize(0));
    ///
    /// cb.bump();
    /// assert_eq!(cb.get_byte_pos_of_peeking_char(), BytePos::from_usize(4));
    /// ```
    ///
    /// [bump]: crate::char_bumper::CharBumper::bump
    pub fn get_byte_pos_of_peeking_char(&self) -> BytePos {
        self.peek_char_byte_pos
    }
}

#[cfg(test)]
mod tests {
    use crate::char_bumper::CharBumper;
    use crate::source_map::{BytePos, Pos};

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

    #[test]
    fn current_peek_pos_is_initially_zero() {
        let char_bumper = CharBumper::from("abc");
        assert_eq!(
            char_bumper.get_byte_pos_of_peeking_char(),
            BytePos::from_usize(0)
        );
    }

    #[test]
    fn current_peek_pos_remains_the_same_after_peeking_a_char() {
        let char_bumper = CharBumper::from("abc");

        let before_byte_pos = char_bumper.get_byte_pos_of_peeking_char();
        let _ = char_bumper.peek();
        let after_byte_pos = char_bumper.get_byte_pos_of_peeking_char();

        assert_eq!(before_byte_pos, after_byte_pos);
    }

    #[test]
    fn current_peek_pos_advances_by_one_after_bumping_an_ascii_char() {
        let mut char_bumper = CharBumper::from("abc");

        let first_byte_pos = char_bumper.get_byte_pos_of_peeking_char();

        let _ = char_bumper.bump();
        let second_byte_pos = char_bumper.get_byte_pos_of_peeking_char();

        assert_eq!(second_byte_pos, first_byte_pos + BytePos::from_usize(1));

        let _ = char_bumper.bump();
        let third_byte_pos = char_bumper.get_byte_pos_of_peeking_char();

        assert_eq!(third_byte_pos, first_byte_pos + BytePos::from_usize(2));
    }

    #[test]
    fn current_peek_pos_advances_by_the_char_length_after_bumping_a_utf8_char() {
        let source_input_and_length = [("\u{80}", 2), ("\u{800}", 3), ("\u{10000}", 4)];

        for (source, byte_length) in source_input_and_length {
            let mut char_bumper = CharBumper::from(source);

            let first_byte_pos = char_bumper.get_byte_pos_of_peeking_char();
            let _ = char_bumper.bump();
            let second_byte_pos = char_bumper.get_byte_pos_of_peeking_char();

            assert_eq!(
                second_byte_pos,
                first_byte_pos + BytePos::from_usize(byte_length)
            );
        }
    }

    #[test]
    fn current_peek_pos_doesnt_advance_when_bumping_past_the_last_char() {
        let mut char_bumper = CharBumper::from("abc");

        let initial_byte_pos = char_bumper.get_byte_pos_of_peeking_char();

        assert_eq!(char_bumper.bump(), Some('a'));
        assert_eq!(char_bumper.bump(), Some('b'));
        assert_eq!(char_bumper.bump(), Some('c'));

        let last_byte_pos = char_bumper.get_byte_pos_of_peeking_char();
        assert_eq!(last_byte_pos, initial_byte_pos + BytePos::from_usize(3));

        assert_eq!(char_bumper.bump(), None);

        let past_last_byte_pos = char_bumper.get_byte_pos_of_peeking_char();
        assert_eq!(
            past_last_byte_pos,
            initial_byte_pos + BytePos::from_usize(3)
        );
    }
}
