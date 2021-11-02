use crate::source_map::{BytePos, Pos};
use std::str::Chars;

#[derive(PartialEq, Debug)]
pub enum Bracket {
    Round,
    Square,
    Curly,
}

#[derive(PartialEq, Debug)]
pub enum TokenKind {
    Open(Bracket),
    Closed(Bracket),
    Period,
    Arrow,
    PlusPlus,
    MinusMinus,
    Ampersand,
    Star,
    Plus,
    Minus,
    Tilde,
    Exclamation,
    Slash,
    Percent,
    LessLess,
    GreaterGreater,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    EqualEqual,
    ExclaEqual,
    Caret,
    Pipe,
    AmpAmp,
    PipePipe,
    Question,
    Colon,
    Semicolon,
    Ellipsis,
    Equal,
    StarEqual,
    SlashEqual,
    PercentEqual,
    PlusEqual,
    MinusEqual,
    LessLessEqual,
    GreaterGreaterEqual,
    AmpEqual,
    CaretEqual,
    PipeEqual,
    Comma,
    Hash,
    HashHash,
    Eof,
}

#[derive(PartialEq, Debug)]
pub struct Token {
    pub kind: TokenKind,
}

impl Token {
    fn eof() -> Token {
        Token {
            kind: TokenKind::Eof,
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum ScanError {}

pub struct Scanner<'chars> {
    chars: Chars<'chars>,
    peeked_char: char,
    byte_pos_of_peeking_char: BytePos,
}

impl Scanner<'_> {
    pub fn with_input(source: &str) -> Scanner {
        let mut scanner = Scanner {
            chars: source.chars(),
            peeked_char: '\0',
            byte_pos_of_peeking_char: BytePos::from_usize(0),
        };

        // Consumes the first char by the C rules, making it available
        // as the peeking char.
        let _ = scanner.consume_char();

        scanner
    }

    pub fn scan_next_token(&mut self) -> Result<Token, ScanError> {
        let token_kind = match self.consume_char() {
            '(' => TokenKind::Open(Bracket::Round),
            ')' => TokenKind::Closed(Bracket::Round),
            '[' => TokenKind::Open(Bracket::Square),
            ']' => TokenKind::Closed(Bracket::Square),
            '{' => TokenKind::Open(Bracket::Curly),
            '}' => TokenKind::Closed(Bracket::Curly),
            '.' => {
                if self.consume_char_if('.') {
                    if self.consume_char_if('.') {
                        TokenKind::Ellipsis
                    } else {
                        todo!("diagnose error")
                    }
                } else {
                    TokenKind::Period
                }
            }
            '-' => {
                if self.consume_char_if('>') {
                    TokenKind::Arrow
                } else if self.consume_char_if('-') {
                    TokenKind::MinusMinus
                } else if self.consume_char_if('=') {
                    TokenKind::MinusEqual
                } else {
                    TokenKind::Minus
                }
            }
            '+' => {
                if self.consume_char_if('+') {
                    TokenKind::PlusPlus
                } else if self.consume_char_if('=') {
                    TokenKind::PlusEqual
                } else {
                    TokenKind::Plus
                }
            }
            '&' => {
                if self.consume_char_if('&') {
                    TokenKind::AmpAmp
                } else if self.consume_char_if('=') {
                    TokenKind::AmpEqual
                } else {
                    TokenKind::Ampersand
                }
            }
            '*' => {
                if self.consume_char_if('=') {
                    TokenKind::StarEqual
                } else {
                    TokenKind::Star
                }
            }
            '~' => TokenKind::Tilde,
            '!' => {
                if self.consume_char_if('=') {
                    TokenKind::ExclaEqual
                } else {
                    TokenKind::Exclamation
                }
            }
            '/' => {
                if self.consume_char_if('=') {
                    TokenKind::SlashEqual
                } else {
                    TokenKind::Slash
                }
            }
            '%' => {
                if self.consume_char_if('=') {
                    TokenKind::PercentEqual
                } else {
                    TokenKind::Percent
                }
            }
            '<' => {
                if self.consume_char_if('<') {
                    if self.consume_char_if('=') {
                        TokenKind::LessLessEqual
                    } else {
                        TokenKind::LessLess
                    }
                } else if self.consume_char_if('=') {
                    TokenKind::LessEqual
                } else {
                    TokenKind::Less
                }
            }
            '>' => {
                if self.consume_char_if('>') {
                    if self.consume_char_if('=') {
                        TokenKind::GreaterGreaterEqual
                    } else {
                        TokenKind::GreaterGreater
                    }
                } else if self.consume_char_if('=') {
                    TokenKind::GreaterEqual
                } else {
                    TokenKind::Greater
                }
            }
            '=' => {
                if self.consume_char_if('=') {
                    TokenKind::EqualEqual
                } else {
                    TokenKind::Equal
                }
            }
            '^' => {
                if self.consume_char_if('=') {
                    TokenKind::CaretEqual
                } else {
                    TokenKind::Caret
                }
            }
            '|' => {
                if self.consume_char_if('|') {
                    TokenKind::PipePipe
                } else if self.consume_char_if('=') {
                    TokenKind::PipeEqual
                } else {
                    TokenKind::Pipe
                }
            }
            '?' => TokenKind::Question,
            ':' => TokenKind::Colon,
            ';' => TokenKind::Semicolon,
            ',' => TokenKind::Comma,
            '#' => {
                if self.consume_char_if('#') {
                    TokenKind::HashHash
                } else {
                    TokenKind::Hash
                }
            }
            // End of input.
            '\0' => return Ok(Token::eof()),
            _ => unimplemented!(),
        };

        Ok(Token { kind: token_kind })
    }

    fn peek_char(&self) -> char {
        self.peeked_char
    }

    fn consume_char(&mut self) -> char {
        let old_peek = self.peek_char();
        self.peeked_char = self.chars.next().unwrap_or('\0');
        if old_peek != '\0' {
            self.byte_pos_of_peeking_char += BytePos::from_usize(old_peek.len_utf8());
        }
        old_peek
    }

    fn peek_char_is(&self, ch: char) -> bool {
        self.peek_char() == ch
    }

    fn consume_char_if(&mut self, ch: char) -> bool {
        debug_assert!(ch != '\0', "cannot expect NUL char");
        let is_ch_peek = self.peek_char_is(ch);
        if is_ch_peek {
            self.consume_char();
        }
        is_ch_peek
    }
}

#[cfg(test)]
mod tests {
    use super::{Bracket, Scanner, Token, TokenKind};
    use crate::source_map::{BytePos, Pos};

    #[test]
    fn peek_char_should_return_nul_when_input_is_empty() {
        let scanner = Scanner::with_input("");
        assert_eq!(scanner.peek_char(), '\0');
    }

    #[test]
    fn peek_char_should_return_first_char_from_input_when_it_is_not_empty() {
        let scanner = Scanner::with_input("abc");
        assert_eq!(scanner.peek_char(), 'a');
    }

    #[test]
    fn multiple_calls_to_peek_char_should_return_the_same_char() {
        let scanner = Scanner::with_input("abc");

        assert_eq!(scanner.peek_char(), 'a');
        assert_eq!(scanner.peek_char(), 'a');
        assert_eq!(scanner.peek_char(), 'a');
    }

    #[test]
    fn consume_char_should_return_nul_when_input_is_empty() {
        let mut scanner = Scanner::with_input("");
        assert_eq!(scanner.consume_char(), '\0');
    }

    #[test]
    fn consume_char_should_return_first_char_from_input_when_it_is_not_empty() {
        let mut scanner = Scanner::with_input("abc");
        assert_eq!(scanner.consume_char(), 'a');
    }

    #[test]
    fn consume_char_should_advance_the_peeking_character_to_the_next_char() {
        let mut scanner = Scanner::with_input("abc");
        let _ = scanner.consume_char();
        assert_eq!(scanner.peek_char(), 'b');
    }

    #[test]
    fn consume_char_should_return_nul_when_all_chars_have_been_consume_chared() {
        let mut scanner = Scanner::with_input("abc");
        let _ = scanner.consume_char();
        let _ = scanner.consume_char();
        let _ = scanner.consume_char();
        assert_eq!(scanner.consume_char(), '\0');
    }

    #[test]
    fn peek_char_should_return_nul_when_all_chars_have_been_consume_chared() {
        let mut scanner = Scanner::with_input("abc");
        let _ = scanner.consume_char();
        let _ = scanner.consume_char();
        let _ = scanner.consume_char();
        assert_eq!(scanner.peek_char(), '\0');
    }

    #[test]
    fn peek_char_is_should_return_true_when_arg_equals_peek_char() {
        let scanner = Scanner::with_input("abc");
        assert!(scanner.peek_char_is('a'));
    }

    #[test]
    fn peek_char_is_should_return_false_when_arg_differs_from_peek_char() {
        let scanner = Scanner::with_input("abc");
        assert!(!scanner.peek_char_is('b'));
    }

    #[test]
    fn peek_char_is_should_not_advance_peeking_char() {
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
    #[should_panic(expected = "cannot expect NUL char")]
    fn consume_char_if_should_panic_when_expected_char_is_nul() {
        let mut scanner = Scanner::with_input("abc");
        let _ = scanner.consume_char_if('\0');
    }

    #[test]
    fn consume_char_if_should_advance_peeking_char_if_it_returns_true() {
        let mut scanner = Scanner::with_input("abc");
        assert_eq!(scanner.peek_char(), 'a');
        assert!(scanner.consume_char_if('a'));
        assert_eq!(scanner.peek_char(), 'b');
    }

    #[test]
    fn consume_char_if_should_not_advance_peeking_char_if_it_returns_false() {
        let mut scanner = Scanner::with_input("abc");
        assert_eq!(scanner.peek_char(), 'a');
        assert!(!scanner.consume_char_if('b'));
        assert_eq!(scanner.peek_char(), 'a');
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

        assert_eq!(scanner.consume_char(), 'a');
        assert_eq!(scanner.consume_char(), 'b');
        assert_eq!(scanner.consume_char(), 'c');

        let last_byte_pos = scanner.byte_pos_of_peeking_char;
        assert_eq!(last_byte_pos, initial_byte_pos + BytePos::from_usize(3));

        assert_eq!(scanner.consume_char(), '\0');

        let past_last_byte_pos = scanner.byte_pos_of_peeking_char;
        assert_eq!(
            past_last_byte_pos,
            initial_byte_pos + BytePos::from_usize(3)
        );
    }

    #[test]
    fn scanning_an_empty_input_returns_an_eof_token() {
        let mut scanner = Scanner::with_input("");

        let tok = scanner.scan_next_token();

        assert_eq!(tok, Ok(Token::eof()));
    }

    macro_rules! test_token_kind {
        ( $( $test_name:ident : $input:literal => $expected_kind:expr ,)* ) => {
            $(
                #[test]
                fn $test_name() {
                    let mut scanner = Scanner::with_input($input);
                    let tok = scanner.scan_next_token();
                    assert_eq!(tok, Ok(Token { kind: $expected_kind }));
                    assert_eq!(scanner.peek_char(), '\0');
                }
            )*
        }
    }

    test_token_kind! {
        scan_open_round_bracket: "(" => TokenKind::Open(Bracket::Round),
        scan_closed_round_bracket: ")" => TokenKind::Closed(Bracket::Round),
        scan_open_square_bracket: "[" => TokenKind::Open(Bracket::Square),
        scan_closed_square_bracket: "]" => TokenKind::Closed(Bracket::Square),
        scan_open_curly_bracket: "{" => TokenKind::Open(Bracket::Curly),
        scan_closed_curly_bracket: "}" => TokenKind::Closed(Bracket::Curly),
        scan_period: "." => TokenKind::Period,
        scan_arrow: "->" => TokenKind::Arrow,
        scan_plus_plus: "++" => TokenKind::PlusPlus,
        scan_minus_minus: "--" => TokenKind::MinusMinus,
        scan_ampersand: "&" => TokenKind::Ampersand,
        scan_star: "*" => TokenKind::Star,
        scan_plus: "+" => TokenKind::Plus,
        scan_minus: "-" => TokenKind::Minus,
        scan_tilde: "~" => TokenKind::Tilde,
        scan_exclamation: "!" => TokenKind::Exclamation,
        scan_slash: "/" => TokenKind::Slash,
        scan_percent: "%" => TokenKind::Percent,
        scan_less_less: "<<" => TokenKind::LessLess,
        scan_greater_greater: ">>" => TokenKind::GreaterGreater,
        scan_less: "<" => TokenKind::Less,
        scan_greater: ">" => TokenKind::Greater,
        scan_less_equal: "<=" => TokenKind::LessEqual,
        scan_greater_equal: ">=" => TokenKind::GreaterEqual,
        scan_equal_equal: "==" => TokenKind::EqualEqual,
        scan_excla_equal: "!=" => TokenKind::ExclaEqual,
        scan_caret: "^" => TokenKind::Caret,
        scan_pipe: "|" => TokenKind::Pipe,
        scan_amp_amp: "&&" => TokenKind::AmpAmp,
        scan_pipe_pipe: "||" => TokenKind::PipePipe,
        scan_question: "?" => TokenKind::Question,
        scan_colon: ":" => TokenKind::Colon,
        scan_semicolon: ";" => TokenKind::Semicolon,
        scan_ellipsis: "..." => TokenKind::Ellipsis,
        scan_equal: "=" => TokenKind::Equal,
        scan_star_equal: "*=" => TokenKind::StarEqual,
        scan_slash_equal: "/=" => TokenKind::SlashEqual,
        scan_percent_equal: "%=" => TokenKind::PercentEqual,
        scan_plus_equal: "+=" => TokenKind::PlusEqual,
        scan_minus_equal: "-=" => TokenKind::MinusEqual,
        scan_less_less_equal: "<<=" => TokenKind::LessLessEqual,
        scan_greater_greater_equal: ">>=" => TokenKind::GreaterGreaterEqual,
        scan_amp_equal: "&=" => TokenKind::AmpEqual,
        scan_caret_equal: "^=" => TokenKind::CaretEqual,
        scan_pipe_equal: "|=" => TokenKind::PipeEqual,
        scan_comma: "," => TokenKind::Comma,
        scan_hash: "#" => TokenKind::Hash,
        scan_hash_hash: "##" => TokenKind::HashHash,
    }
}
