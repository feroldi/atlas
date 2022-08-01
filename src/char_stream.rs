use std::str::Chars;

use crate::source_map::{BytePos, Pos};

#[derive(Clone)]
pub struct CharStream<'chars> {
    chars: Chars<'chars>,
    cached_peek: Option<PeekChar>,
    byte_pos_of_peeked_char: BytePos,
}

#[derive(Clone, Copy)]
struct PeekChar {
    ch: char,
    size: usize,
}

impl CharStream<'_> {
    pub const EOF_CHAR: char = '\0';
    const LOOKAHEAD_LIMIT: usize = 8;

    pub fn with_text(text: &str) -> CharStream {
        CharStream {
            chars: text.chars(),
            cached_peek: None,
            byte_pos_of_peeked_char: BytePos::from_usize(0),
        }
    }

    pub fn peek(&mut self) -> char {
        match self.cached_peek {
            Some(peek) => peek.ch,
            _ => {
                let peeked = self.get_next_char_and_size();
                self.cached_peek = Some(peeked);
                peeked.ch
            }
        }
    }

    fn get_next_char_and_size(&mut self) -> PeekChar {
        let next_char = self.chars.next();
        let lookahead_char = self.chars.clone().next();

        if is_newline(next_char) && is_newline(lookahead_char) && next_char != lookahead_char {
            self.chars.next();
            return PeekChar { ch: '\n', size: 2 };
        }

        match next_char {
            Some(ch) => PeekChar {
                ch,
                size: ch.len_utf8(),
            },
            _ => PeekChar {
                ch: CharStream::EOF_CHAR,
                size: 0,
            },
        }
    }

    pub fn peek_byte_pos(&self) -> BytePos {
        self.byte_pos_of_peeked_char
    }

    pub fn lookahead(&mut self, n: usize) -> char {
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
        // Guarantees that `self.cached_peek` has a peeked char.
        self.peek();
        debug_assert!(self.cached_peek.is_some());

        let peeked = self.cached_peek.unwrap();

        if peeked.ch != CharStream::EOF_CHAR {
            let peeked = self.cached_peek.unwrap();
            self.byte_pos_of_peeked_char += BytePos::from_usize(peeked.size);
            self.cached_peek = None;
        }

        peeked.ch
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

fn is_newline(ch: impl Into<Option<char>>) -> bool {
    matches!(ch.into(), Some('\n' | '\r'))
}
