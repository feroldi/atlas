use crate::source_map::{BytePos, Pos};
use std::str::Chars;

#[derive(Clone)]
pub struct CharStream<'chars> {
    chars: Chars<'chars>,
    peeked_char: char,
    byte_pos_of_peeked_char: BytePos,
}

impl CharStream<'_> {
    pub const EOF_CHAR: char = '\0';
    const LOOKAHEAD_LIMIT: usize = 8;

    pub fn with_text(text: &str) -> CharStream {
        let mut char_stream = CharStream {
            chars: text.chars(),
            peeked_char: CharStream::EOF_CHAR,
            byte_pos_of_peeked_char: BytePos::from_usize(0),
        };

        // Consumes the first char by the C rules, making it available
        // as the first peek-char.
        let _ = char_stream.consume();

        char_stream
    }

    pub fn peek(&self) -> char {
        self.peeked_char
    }

    pub fn peek_byte_pos(&self) -> BytePos {
        self.byte_pos_of_peeked_char
    }

    pub fn lookahead(&self, n: usize) -> char {
        assert!(
            n <= CharStream::LOOKAHEAD_LIMIT,
            "cannot look further than {} chars ahead",
            CharStream::LOOKAHEAD_LIMIT,
        );

        if n == 0 {
            self.peek()
        } else {
            let mut cloned_self = self.clone();
            for _ in 0..n {
                let _ = cloned_self.consume();
            }
            cloned_self.peek()
        }
    }

    pub fn consume(&mut self) -> char {
        let old_peek = self.peek();

        if let Some(next_char) = self.chars.next() {
            if next_char == CharStream::EOF_CHAR {
                // Consumes the entire iterator until the end. A NUL char marks the end
                // of the input.
                for _ in &mut self.chars {}
            }
            self.peeked_char = next_char;
        } else {
            self.peeked_char = CharStream::EOF_CHAR;
        }

        if old_peek != CharStream::EOF_CHAR {
            self.byte_pos_of_peeked_char += BytePos::from_usize(old_peek.len_utf8());
        }

        old_peek
    }

    pub fn try_consume(&mut self, ch: char) -> bool {
        assert!(ch != CharStream::EOF_CHAR, "cannot consume an EOF char");

        let is_ch_peek = self.peek() == ch;
        if is_ch_peek {
            self.consume();
        }
        is_ch_peek
    }
}
