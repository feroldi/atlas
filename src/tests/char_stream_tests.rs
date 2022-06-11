#![cfg(test)]

use crate::char_stream::CharStream;
use crate::source_map::{BytePos, Pos};

#[test]
fn peek_should_return_eof_when_char_stream_is_empty() {
    let char_stream = CharStream::with_text("");
    assert_eq!(char_stream.peek(), CharStream::EOF_CHAR);
}

#[test]
fn peek_should_return_first_char_from_stream_when_it_is_not_empty() {
    let char_stream = CharStream::with_text("abc");
    assert_eq!(char_stream.peek(), 'a');
}

#[test]
fn multiple_calls_to_peek_should_return_the_same_char() {
    let char_stream = CharStream::with_text("abc");

    assert_eq!(char_stream.peek(), 'a');
    assert_eq!(char_stream.peek(), 'a');
    assert_eq!(char_stream.peek(), 'a');
}

#[test]
fn lookahead_should_return_eof_when_char_stream_is_empty_no_matter_the_value_of_n() {
    let char_stream = CharStream::with_text("");

    assert_eq!(char_stream.lookahead(0), CharStream::EOF_CHAR);
    assert_eq!(char_stream.lookahead(1), CharStream::EOF_CHAR);
    assert_eq!(char_stream.lookahead(2), CharStream::EOF_CHAR);
}

#[test]
fn lookahead_should_return_eof_when_n_is_greater_or_equal_to_the_char_stream_length() {
    let char_stream = CharStream::with_text("abc");

    assert_ne!(char_stream.lookahead(2), CharStream::EOF_CHAR);

    assert_eq!(char_stream.lookahead(3), CharStream::EOF_CHAR);
}

#[test]
fn lookahead_should_return_first_char_from_stream_when_n_is_zero() {
    let char_stream = CharStream::with_text("abc");
    assert_eq!(char_stream.lookahead(0), 'a');
}

#[test]
fn lookahead_should_return_nth_char_from_stream_when_n_is_nonzero() {
    let char_stream = CharStream::with_text("abcdefg");

    assert_eq!(char_stream.lookahead(1), 'b');
    assert_eq!(char_stream.lookahead(4), 'e');
}

#[test]
fn lookahead_can_look_at_most_8_chars_ahead() {
    let char_stream = CharStream::with_text("the brown fox jumped over the lazy dog");
    assert_eq!(char_stream.lookahead(8), 'n');
}

#[test]
#[should_panic(expected = "cannot look further than 8 chars ahead")]
fn lookahead_cannot_look_further_than_8_chars_ahead() {
    let char_stream = CharStream::with_text("the brown fox jumped over the lazy dog");
    char_stream.lookahead(9);
}

#[test]
fn lookahead_should_not_consume_the_char_stream() {
    let char_stream = CharStream::with_text("abc");

    char_stream.lookahead(3);

    assert_eq!(char_stream.peek(), 'a');
}

#[test]
fn consume_should_return_eof_when_char_stream_is_empty() {
    let mut char_stream = CharStream::with_text("");
    assert_eq!(char_stream.consume(), CharStream::EOF_CHAR);
}

#[test]
fn consume_should_return_the_then_peeked_char() {
    let mut char_stream = CharStream::with_text("abc");
    assert_eq!(char_stream.consume(), 'a');
}

#[test]
fn consume_should_advance_the_peeked_char_to_the_next_char() {
    let mut char_stream = CharStream::with_text("abc");

    char_stream.consume();

    assert_eq!(char_stream.peek(), 'b');

    assert_eq!(char_stream.lookahead(0), 'b');
    assert_eq!(char_stream.lookahead(1), 'c');
}

#[test]
fn consume_should_return_eof_after_the_char_stream_is_completely_consumed() {
    let mut char_stream = CharStream::with_text("abc");

    char_stream.consume();
    char_stream.consume();
    char_stream.consume();

    assert_eq!(char_stream.consume(), CharStream::EOF_CHAR);
}

#[test]
fn consume_should_return_eof_forever_after_encountering_a_null_char_in_the_middle_of_the_input() {
    let mut char_stream = CharStream::with_text("abc\0def");

    char_stream.consume();
    char_stream.consume();
    char_stream.consume();

    assert_eq!(char_stream.consume(), CharStream::EOF_CHAR);
    assert_eq!(char_stream.consume(), CharStream::EOF_CHAR);
    assert_eq!(char_stream.consume(), CharStream::EOF_CHAR);
    assert_eq!(char_stream.consume(), CharStream::EOF_CHAR);
}

#[test]
fn try_consume_should_not_consume_char_stream_if_ch_is_not_equal_to_peek() {
    let mut char_stream = CharStream::with_text("abc");

    char_stream.try_consume('b');

    assert_eq!(char_stream.peek(), 'a');
}

#[test]
fn try_consume_should_return_false_if_ch_is_not_equal_to_peek() {
    let mut char_stream = CharStream::with_text("abc");

    assert!(!char_stream.try_consume('b'));
}

#[test]
fn try_consume_should_consume_char_stream_if_ch_is_equal_to_peek() {
    let mut char_stream = CharStream::with_text("abc");

    char_stream.try_consume('a');

    assert_eq!(char_stream.peek(), 'b');
}

#[test]
fn try_consume_should_return_true_if_ch_is_equal_to_peek() {
    let mut char_stream = CharStream::with_text("abc");

    assert!(char_stream.try_consume('a'));
}

#[test]
#[should_panic(expected = "cannot consume an EOF char")]
fn try_consume_should_panic_if_ch_is_eof() {
    let mut char_stream = CharStream::with_text("abc");
    char_stream.try_consume(CharStream::EOF_CHAR);
}

#[test]
fn peek_and_lookahead_should_return_eof_after_the_char_stream_is_completely_consumed() {
    let mut char_stream = CharStream::with_text("abc");

    char_stream.consume();
    char_stream.consume();
    char_stream.consume();

    assert_eq!(char_stream.peek(), CharStream::EOF_CHAR);

    assert_eq!(char_stream.lookahead(0), CharStream::EOF_CHAR);
    assert_eq!(char_stream.lookahead(1), CharStream::EOF_CHAR);
}

#[test]
fn peek_byte_pos_should_return_zero_for_the_first_peeked_char_whether_char_stream_is_empty_or_not()
{
    assert_eq!(
        CharStream::with_text("").peek_byte_pos(),
        BytePos::from_usize(0)
    );

    assert_eq!(
        CharStream::with_text("abc").peek_byte_pos(),
        BytePos::from_usize(0)
    );
}

#[test]
fn peek_byte_pos_should_not_advance_after_peeking_a_char() {
    let char_stream = CharStream::with_text("abc");

    let before_byte_pos = char_stream.peek_byte_pos();
    let _ = char_stream.peek();
    let after_byte_pos = char_stream.peek_byte_pos();

    assert_eq!(before_byte_pos, after_byte_pos);
}

#[test]
fn peek_byte_pos_should_not_advance_after_lookahead() {
    let char_stream = CharStream::with_text("abc");

    let before_byte_pos = char_stream.peek_byte_pos();
    let _ = char_stream.lookahead(2);
    let after_byte_pos = char_stream.peek_byte_pos();

    assert_eq!(before_byte_pos, after_byte_pos);
}

#[test]
fn peek_byte_pos_should_not_advance_after_consuming_an_eof_char() {
    let input_text = "abc";
    let mut char_stream = CharStream::with_text(input_text);

    let first_byte_pos = char_stream.peek_byte_pos();

    for _ in 0..input_text.len() {
        char_stream.consume();
    }

    let last_byte_pos = char_stream.peek_byte_pos();

    assert_ne!(first_byte_pos, last_byte_pos);

    let eof_char = char_stream.consume();

    assert_eq!(eof_char, CharStream::EOF_CHAR);
    assert_eq!(char_stream.peek_byte_pos(), last_byte_pos);
}

#[test]
fn peek_byte_pos_should_advance_by_one_after_consuming_an_ascii_char() {
    let mut char_stream = CharStream::with_text("abc");

    let first_byte_pos = char_stream.peek_byte_pos();

    let _ = char_stream.consume();
    let second_byte_pos = char_stream.peek_byte_pos();

    assert_eq!(second_byte_pos, first_byte_pos + BytePos::from_usize(1));

    let _ = char_stream.consume();
    let third_byte_pos = char_stream.peek_byte_pos();

    assert_eq!(third_byte_pos, first_byte_pos + BytePos::from_usize(2));
}

#[test]
fn peek_byte_pos_should_advance_by_the_char_utf8_byte_length_after_consuming_a_utf8_char() {
    let text_input_with_varying_char_lengths = "\u{80}\u{800}\u{10000}";
    let mut char_stream = CharStream::with_text(text_input_with_varying_char_lengths);

    let expected_peek_byte_poses = [
        BytePos::from_usize(2), // 2-byte length
        BytePos::from_usize(5), // 3-byte length
        BytePos::from_usize(9), // 4-byte length
    ];

    for expected_peek_byte_pos in expected_peek_byte_poses {
        char_stream.consume();
        assert_eq!(char_stream.peek_byte_pos(), expected_peek_byte_pos);
    }
}
