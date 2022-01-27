use crate::char_stream::CharStream;

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
pub enum ScanDiag {}

pub struct Scanner<'chars> {
    chars: CharStream<'chars>,
}

impl Scanner<'_> {
    pub fn with_input(source_text: &str) -> Scanner {
        Scanner {
            chars: CharStream::with_text(source_text),
        }
    }

    pub fn scan_next_token(&mut self) -> Result<Token, ScanDiag> {
        let token_kind = match self.chars.consume() {
            '(' => TokenKind::Open(Bracket::Round),
            ')' => TokenKind::Closed(Bracket::Round),
            '[' => TokenKind::Open(Bracket::Square),
            ']' => TokenKind::Closed(Bracket::Square),
            '{' => TokenKind::Open(Bracket::Curly),
            '}' => TokenKind::Closed(Bracket::Curly),
            '.' => {
                if self.chars.peek() == '.' && self.chars.lookahead(1) == '.' {
                    self.chars.consume();
                    self.chars.consume();
                    TokenKind::Ellipsis
                } else {
                    TokenKind::Period
                }
            }
            '-' => {
                if self.chars.try_consume('>') {
                    TokenKind::Arrow
                } else if self.chars.try_consume('-') {
                    TokenKind::MinusMinus
                } else if self.chars.try_consume('=') {
                    TokenKind::MinusEqual
                } else {
                    TokenKind::Minus
                }
            }
            '+' => {
                if self.chars.try_consume('+') {
                    TokenKind::PlusPlus
                } else if self.chars.try_consume('=') {
                    TokenKind::PlusEqual
                } else {
                    TokenKind::Plus
                }
            }
            '&' => {
                if self.chars.try_consume('&') {
                    TokenKind::AmpAmp
                } else if self.chars.try_consume('=') {
                    TokenKind::AmpEqual
                } else {
                    TokenKind::Ampersand
                }
            }
            '*' => {
                if self.chars.try_consume('=') {
                    TokenKind::StarEqual
                } else {
                    TokenKind::Star
                }
            }
            '~' => TokenKind::Tilde,
            '!' => {
                if self.chars.try_consume('=') {
                    TokenKind::ExclaEqual
                } else {
                    TokenKind::Exclamation
                }
            }
            '/' => {
                if self.chars.try_consume('=') {
                    TokenKind::SlashEqual
                } else {
                    TokenKind::Slash
                }
            }
            '%' => {
                if self.chars.try_consume('=') {
                    TokenKind::PercentEqual
                } else {
                    TokenKind::Percent
                }
            }
            '<' => {
                if self.chars.try_consume('<') {
                    if self.chars.try_consume('=') {
                        TokenKind::LessLessEqual
                    } else {
                        TokenKind::LessLess
                    }
                } else if self.chars.try_consume('=') {
                    TokenKind::LessEqual
                } else {
                    TokenKind::Less
                }
            }
            '>' => {
                if self.chars.try_consume('>') {
                    if self.chars.try_consume('=') {
                        TokenKind::GreaterGreaterEqual
                    } else {
                        TokenKind::GreaterGreater
                    }
                } else if self.chars.try_consume('=') {
                    TokenKind::GreaterEqual
                } else {
                    TokenKind::Greater
                }
            }
            '=' => {
                if self.chars.try_consume('=') {
                    TokenKind::EqualEqual
                } else {
                    TokenKind::Equal
                }
            }
            '^' => {
                if self.chars.try_consume('=') {
                    TokenKind::CaretEqual
                } else {
                    TokenKind::Caret
                }
            }
            '|' => {
                if self.chars.try_consume('|') {
                    TokenKind::PipePipe
                } else if self.chars.try_consume('=') {
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
                if self.chars.try_consume('#') {
                    TokenKind::HashHash
                } else {
                    TokenKind::Hash
                }
            }
            '\0' => return Ok(Token::eof()),
            _ => unimplemented!(),
        };

        Ok(Token { kind: token_kind })
    }
}

#[cfg(test)]
mod tests {
    use super::{Bracket, Scanner, Token, TokenKind};
    use crate::char_stream::CharStream;

    #[test]
    fn scanning_an_empty_input_should_return_an_eof_token() {
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
                    assert_eq!(scanner.chars.peek(), CharStream::EOF_CHAR);
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

    #[test]
    fn two_adjacent_period_chars_should_be_scanned_as_two_separate_period_operators() {
        let mut scanner = Scanner::with_input("..");

        assert_eq!(
            scanner.scan_next_token(),
            Ok(Token {
                kind: TokenKind::Period
            })
        );

        assert_eq!(
            scanner.scan_next_token(),
            Ok(Token {
                kind: TokenKind::Period
            })
        );

        assert_eq!(scanner.scan_next_token(), Ok(Token::eof()));
    }
}
