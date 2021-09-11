use crate::source_map::{BytePos, Pos};
use std::str::Chars;

pub struct Scanner<'chars> {
    chars: Chars<'chars>,
    peek_char: Option<char>,
    byte_pos_of_peeking_char: BytePos,
}

impl Scanner<'_> {
    pub fn with_input(source: &str) -> Scanner {
        let mut chars = source.chars();
        let peek_char = chars.next();
        Scanner {
            chars,
            peek_char,
            byte_pos_of_peeking_char: BytePos::from_usize(0),
        }
    }

    pub fn peek_char(&self) -> Option<char> {
        self.peek_char
    }

    pub fn consume_char(&mut self) -> Option<char> {
        let old_peek = self.peek_char();
        self.peek_char = self.chars.next();
        if let Some(ch) = old_peek {
            self.byte_pos_of_peeking_char += BytePos::from_usize(ch.len_utf8());
        }
        old_peek
    }

    pub fn peek_char_is(&self, ch: char) -> bool {
        self.peek_char() == Some(ch)
    }

    pub fn consume_char_if(&mut self, ch: char) -> bool {
        let is_ch_peek = self.peek_char_is(ch);
        if is_ch_peek {
            self.consume_char();
        }
        is_ch_peek
    }
}

#[cfg(test)]
mod tests {
    use super::Scanner;
    use crate::source_map::{BytePos, Pos};

    #[test]
    fn peek_should_return_none_when_input_is_empty() {
        let scanner = Scanner::with_input("");
        assert_eq!(scanner.peek_char(), None::<char>);
    }

    #[test]
    fn peek_should_return_first_char_from_input_when_it_is_not_empty() {
        let scanner = Scanner::with_input("abc");
        assert_eq!(scanner.peek_char(), Some('a'));
    }

    #[test]
    fn multiple_calls_to_peek_should_return_the_same_char() {
        let scanner = Scanner::with_input("abc");

        assert_eq!(scanner.peek_char(), Some('a'));
        assert_eq!(scanner.peek_char(), Some('a'));
        assert_eq!(scanner.peek_char(), Some('a'));
    }

    #[test]
    fn consume_char_should_return_none_when_input_is_empty() {
        let mut scanner = Scanner::with_input("");
        assert_eq!(scanner.consume_char(), None::<char>);
    }

    #[test]
    fn consume_char_should_return_first_char_from_input_when_it_is_not_empty() {
        let mut scanner = Scanner::with_input("abc");
        assert_eq!(scanner.consume_char(), Some('a'));
    }

    #[test]
    fn consume_char_should_advance_the_peeking_character_to_the_next_char() {
        let mut scanner = Scanner::with_input("abc");
        let _ = scanner.consume_char();
        assert_eq!(scanner.peek_char(), Some('b'));
    }

    #[test]
    fn consume_char_should_return_none_when_all_chars_have_been_consume_chared() {
        let mut scanner = Scanner::with_input("abc");
        let _ = scanner.consume_char();
        let _ = scanner.consume_char();
        let _ = scanner.consume_char();
        assert_eq!(scanner.consume_char(), None);
    }

    #[test]
    fn peek_should_return_none_when_all_chars_have_been_consume_chared() {
        let mut scanner = Scanner::with_input("abc");
        let _ = scanner.consume_char();
        let _ = scanner.consume_char();
        let _ = scanner.consume_char();
        assert_eq!(scanner.peek_char(), None);
    }

    #[test]
    fn peek_is_should_return_true_when_arg_equals_peek_char() {
        let scanner = Scanner::with_input("abc");
        assert!(scanner.peek_char_is('a'));
    }

    #[test]
    fn peek_is_should_return_false_when_arg_differs_from_peek_char() {
        let scanner = Scanner::with_input("abc");
        assert!(!scanner.peek_char_is('b'));
    }

    #[test]
    fn peek_is_should_not_advance_peeking_char() {
        let scanner = Scanner::with_input("abc");
        let peek_before = scanner.peek_char();
        let _ = scanner.peek_char_is('a');
        let _ = scanner.peek_char_is('b');
        let peek_after = scanner.peek_char();
        assert_eq!(peek_before, peek_after);
    }

    #[test]
    fn consume_char_if_should_return_true_when_arg_equals_peek_char() {
        let mut scanner = Scanner::with_input("abc");
        assert!(scanner.consume_char_if('a'));
    }

    #[test]
    fn consume_char_if_should_return_false_when_arg_differs_from_peek_char() {
        let mut scanner = Scanner::with_input("abc");
        assert!(!scanner.consume_char_if('b'));
    }

    #[test]
    fn consume_char_if_should_advance_peeking_char_if_it_returns_true() {
        let mut scanner = Scanner::with_input("abc");
        assert_eq!(scanner.peek_char(), Some('a'));
        assert!(scanner.consume_char_if('a'));
        assert_eq!(scanner.peek_char(), Some('b'));
    }

    #[test]
    fn consume_char_if_should_not_advance_peeking_char_if_it_returns_false() {
        let mut scanner = Scanner::with_input("abc");
        assert_eq!(scanner.peek_char(), Some('a'));
        assert!(!scanner.consume_char_if('b'));
        assert_eq!(scanner.peek_char(), Some('a'));
    }

    #[test]
    fn current_peek_pos_is_initially_zero() {
        let scanner = Scanner::with_input("abc");
        assert_eq!(scanner.byte_pos_of_peeking_char, BytePos::from_usize(0));
    }

    #[test]
    fn current_peek_pos_remains_the_same_after_peeking_a_char() {
        let scanner = Scanner::with_input("abc");

        let before_byte_pos = scanner.byte_pos_of_peeking_char;
        let _ = scanner.peek_char();
        let after_byte_pos = scanner.byte_pos_of_peeking_char;

        assert_eq!(before_byte_pos, after_byte_pos);
    }

    #[test]
    fn current_peek_pos_advances_by_one_after_consuming_an_ascii_char() {
        let mut scanner = Scanner::with_input("abc");

        let first_byte_pos = scanner.byte_pos_of_peeking_char;

        let _ = scanner.consume_char();
        let second_byte_pos = scanner.byte_pos_of_peeking_char;

        assert_eq!(second_byte_pos, first_byte_pos + BytePos::from_usize(1));

        let _ = scanner.consume_char();
        let third_byte_pos = scanner.byte_pos_of_peeking_char;

        assert_eq!(third_byte_pos, first_byte_pos + BytePos::from_usize(2));
    }

    #[test]
    fn current_peek_pos_advances_by_the_char_length_after_consuming_a_utf8_char() {
        let source_input_and_length = [("\u{80}", 2), ("\u{800}", 3), ("\u{10000}", 4)];

        for (source, byte_length) in source_input_and_length {
            let mut scanner = Scanner::with_input(source);

            let first_byte_pos = scanner.byte_pos_of_peeking_char;
            let _ = scanner.consume_char();
            let second_byte_pos = scanner.byte_pos_of_peeking_char;

            assert_eq!(
                second_byte_pos,
                first_byte_pos + BytePos::from_usize(byte_length)
            );
        }
    }

    #[test]
    fn current_peek_pos_doesnt_advance_when_consuming_past_the_last_char() {
        let mut scanner = Scanner::with_input("abc");

        let initial_byte_pos = scanner.byte_pos_of_peeking_char;

        assert_eq!(scanner.consume_char(), Some('a'));
        assert_eq!(scanner.consume_char(), Some('b'));
        assert_eq!(scanner.consume_char(), Some('c'));

        let last_byte_pos = scanner.byte_pos_of_peeking_char;
        assert_eq!(last_byte_pos, initial_byte_pos + BytePos::from_usize(3));

        assert_eq!(scanner.consume_char(), None);

        let past_last_byte_pos = scanner.byte_pos_of_peeking_char;
        assert_eq!(
            past_last_byte_pos,
            initial_byte_pos + BytePos::from_usize(3)
        );
    }
}
