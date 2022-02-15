use crate::char_stream::CharStream;
use crate::source_map::{Span, Spanned};
use std::assert_matches::debug_assert_matches;

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
    Identifier,
    KwAuto,
    KwBreak,
    KwCase,
    KwChar,
    KwConst,
    KwContinue,
    KwDefault,
    KwDo,
    KwDouble,
    KwElse,
    KwEnum,
    KwExtern,
    KwFloat,
    KwFor,
    KwGoto,
    KwIf,
    KwInline,
    KwInt,
    KwLong,
    KwRegister,
    KwRestrict,
    KwReturn,
    KwShort,
    KwSigned,
    KwSizeof,
    KwStatic,
    KwStruct,
    KwSwitch,
    KwTypedef,
    KwUnion,
    KwUnsigned,
    KwVoid,
    KwVolatile,
    KwWhile,
    KwAlignas,
    KwAlignof,
    KwAtomic,
    KwBool,
    KwComplex,
    KwGeneric,
    KwImaginary,
    KwNoreturn,
    KwStaticAssert,
    KwThreadLocal,
    NumericConstant,
    Eof,
}

#[derive(PartialEq, Debug)]
pub struct Token {
    pub kind: TokenKind,
}

impl Token {
    const EOF: Spanned<Token> = Spanned::with_dummy_span(Token {
        kind: TokenKind::Eof,
    });
}

// TODO: Should be SyntaxDiag
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

    pub fn scan_next_token(&mut self) -> Result<Spanned<Token>, ScanDiag> {
        while is_whitespace_char(self.chars.peek()) {
            self.chars.consume();
        }

        let span_start = self.chars.peek_byte_pos();

        let token_kind = match self.chars.consume() {
            CharStream::EOF_CHAR => return Ok(Token::EOF),
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
            ch if ch.is_ascii_digit() => {
                // TODO: Refactor this section into a function.
                let mut prev_peek = ch;

                while self.chars.peek().is_ascii_alphanumeric() {
                    prev_peek = self.chars.peek();
                    self.chars.consume();
                }

                if matches!(
                    (prev_peek, self.chars.peek()),
                    ('p' | 'P' | 'e' | 'E', '+' | '-')
                ) {
                    let sign_char = self.chars.consume();
                    debug_assert_matches!(sign_char, '+' | '-');

                    while self.chars.peek().is_ascii_alphanumeric() {
                        self.chars.consume();
                    }
                }

                TokenKind::NumericConstant
            }
            ch if is_identifier_head(ch) => self.scan_identifier_or_keyword(ch),
            unrecognized_char => {
                unimplemented!("character not recognized: `{}`", unrecognized_char)
            }
        };

        let span_end = self.chars.peek_byte_pos();
        let token_span = Span {
            start: span_start,
            end: span_end,
        };

        Ok(Spanned::new(Token { kind: token_kind }, token_span))
    }

    fn scan_identifier_or_keyword(&mut self, ident_head: char) -> TokenKind {
        debug_assert!(is_identifier_head(ident_head), "char: `{}`", ident_head);

        let mut lexeme_buffer = String::with_capacity(16);
        lexeme_buffer.push(ident_head);

        while is_identifier_tail(self.chars.peek()) {
            lexeme_buffer.push(self.chars.consume());
        }

        match lexeme_buffer.as_ref() {
            "auto" => TokenKind::KwAuto,
            "break" => TokenKind::KwBreak,
            "case" => TokenKind::KwCase,
            "char" => TokenKind::KwChar,
            "const" => TokenKind::KwConst,
            "continue" => TokenKind::KwContinue,
            "default" => TokenKind::KwDefault,
            "do" => TokenKind::KwDo,
            "double" => TokenKind::KwDouble,
            "else" => TokenKind::KwElse,
            "enum" => TokenKind::KwEnum,
            "extern" => TokenKind::KwExtern,
            "float" => TokenKind::KwFloat,
            "for" => TokenKind::KwFor,
            "goto" => TokenKind::KwGoto,
            "if" => TokenKind::KwIf,
            "inline" => TokenKind::KwInline,
            "int" => TokenKind::KwInt,
            "long" => TokenKind::KwLong,
            "register" => TokenKind::KwRegister,
            "restrict" => TokenKind::KwRestrict,
            "return" => TokenKind::KwReturn,
            "short" => TokenKind::KwShort,
            "signed" => TokenKind::KwSigned,
            "sizeof" => TokenKind::KwSizeof,
            "static" => TokenKind::KwStatic,
            "struct" => TokenKind::KwStruct,
            "switch" => TokenKind::KwSwitch,
            "typedef" => TokenKind::KwTypedef,
            "union" => TokenKind::KwUnion,
            "unsigned" => TokenKind::KwUnsigned,
            "void" => TokenKind::KwVoid,
            "volatile" => TokenKind::KwVolatile,
            "while" => TokenKind::KwWhile,
            "_Alignas" => TokenKind::KwAlignas,
            "_Alignof" => TokenKind::KwAlignof,
            "_Atomic" => TokenKind::KwAtomic,
            "_Bool" => TokenKind::KwBool,
            "_Complex" => TokenKind::KwComplex,
            "_Generic" => TokenKind::KwGeneric,
            "_Imaginary" => TokenKind::KwImaginary,
            "_Noreturn" => TokenKind::KwNoreturn,
            "_Static_assert" => TokenKind::KwStaticAssert,
            "_Thread_local" => TokenKind::KwThreadLocal,
            _ => TokenKind::Identifier,
        }
    }
}

fn is_whitespace_char(ch: char) -> bool {
    const SPACE: char = ' ';
    const TAB: char = '\t';
    const LINE_FEED: char = '\n';
    const CARRIAGE_RETURN: char = '\r';
    const VERTICAL_TAB: char = '\x0b';
    const FORM_FEED: char = '\x0c';

    matches!(
        ch,
        SPACE | TAB | LINE_FEED | CARRIAGE_RETURN | VERTICAL_TAB | FORM_FEED,
    )
}

// TODO(feroldi): Test.
fn is_identifier_head(ch: char) -> bool {
    matches!(ch, 'a'..='z' | 'A'..='Z' | '_')
}

// TODO(feroldi): Test.
fn is_identifier_tail(ch: char) -> bool {
    matches!(ch, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9')
}

#[cfg(test)]
mod tests {
    use super::{Bracket, Scanner, Token, TokenKind};
    use crate::source_map::{Span, Spanned};

    #[test]
    fn scanning_an_empty_input_should_return_an_eof_token() {
        let mut scanner = Scanner::with_input("");

        let token = scanner.scan_next_token();

        assert_eq!(token, Ok(Token::EOF));
    }

    macro_rules! test_token_kind {
        ( $( $test_name:ident : $input_text:literal => $expected_kind:expr ,)* ) => {
            $(
                #[test]
                fn $test_name() {
                    let input_text_with_newline = format!("{}\n", $input_text);
                    let mut scanner = Scanner::with_input(&input_text_with_newline);

                    let token = scanner.scan_next_token();

                    let expected_token = Token { kind: $expected_kind };
                    let expected_span = Span::from_raw_pos(0, $input_text.len());

                    assert_eq!(token, Ok(Spanned::new(expected_token, expected_span)));
                    assert_eq!(scanner.chars.peek(), '\n');
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
        scan_keyword_auto: "auto" => TokenKind::KwAuto,
        scan_keyword_break: "break" => TokenKind::KwBreak,
        scan_keyword_case: "case" => TokenKind::KwCase,
        scan_keyword_char: "char" => TokenKind::KwChar,
        scan_keyword_const: "const" => TokenKind::KwConst,
        scan_keyword_continue: "continue" => TokenKind::KwContinue,
        scan_keyword_default: "default" => TokenKind::KwDefault,
        scan_keyword_do: "do" => TokenKind::KwDo,
        scan_keyword_double: "double" => TokenKind::KwDouble,
        scan_keyword_else: "else" => TokenKind::KwElse,
        scan_keyword_enum: "enum" => TokenKind::KwEnum,
        scan_keyword_extern: "extern" => TokenKind::KwExtern,
        scan_keyword_float: "float" => TokenKind::KwFloat,
        scan_keyword_for: "for" => TokenKind::KwFor,
        scan_keyword_goto: "goto" => TokenKind::KwGoto,
        scan_keyword_if: "if" => TokenKind::KwIf,
        scan_keyword_inline: "inline" => TokenKind::KwInline,
        scan_keyword_int: "int" => TokenKind::KwInt,
        scan_keyword_long: "long" => TokenKind::KwLong,
        scan_keyword_register: "register" => TokenKind::KwRegister,
        scan_keyword_restrict: "restrict" => TokenKind::KwRestrict,
        scan_keyword_return: "return" => TokenKind::KwReturn,
        scan_keyword_short: "short" => TokenKind::KwShort,
        scan_keyword_signed: "signed" => TokenKind::KwSigned,
        scan_keyword_sizeof: "sizeof" => TokenKind::KwSizeof,
        scan_keyword_static: "static" => TokenKind::KwStatic,
        scan_keyword_struct: "struct" => TokenKind::KwStruct,
        scan_keyword_switch: "switch" => TokenKind::KwSwitch,
        scan_keyword_typedef: "typedef" => TokenKind::KwTypedef,
        scan_keyword_union: "union" => TokenKind::KwUnion,
        scan_keyword_unsigned: "unsigned" => TokenKind::KwUnsigned,
        scan_keyword_void: "void" => TokenKind::KwVoid,
        scan_keyword_volatile: "volatile" => TokenKind::KwVolatile,
        scan_keyword_while: "while" => TokenKind::KwWhile,
        scan_keyword_alignas: "_Alignas" => TokenKind::KwAlignas,
        scan_keyword_alignof: "_Alignof" => TokenKind::KwAlignof,
        scan_keyword_atomic: "_Atomic" => TokenKind::KwAtomic,
        scan_keyword_bool: "_Bool" => TokenKind::KwBool,
        scan_keyword_complex: "_Complex" => TokenKind::KwComplex,
        scan_keyword_generic: "_Generic" => TokenKind::KwGeneric,
        scan_keyword_imaginary: "_Imaginary" => TokenKind::KwImaginary,
        scan_keyword_noreturn: "_Noreturn" => TokenKind::KwNoreturn,
        scan_keyword_static_assert: "_Static_assert" => TokenKind::KwStaticAssert,
        scan_keyword_thread_local: "_Thread_local" => TokenKind::KwThreadLocal,
    }

    #[test]
    fn two_adjacent_period_chars_should_be_scanned_as_two_separate_period_operators() {
        let mut scanner = Scanner::with_input("..");

        assert_eq!(
            scanner.scan_next_token().unwrap(),
            Spanned::new(
                Token {
                    kind: TokenKind::Period
                },
                Span::from_raw_pos(0, 1),
            )
        );

        assert_eq!(
            scanner.scan_next_token().unwrap(),
            Spanned::new(
                Token {
                    kind: TokenKind::Period
                },
                Span::from_raw_pos(1, 2),
            )
        );

        assert_eq!(scanner.scan_next_token(), Ok(Token::EOF));
    }

    #[test]
    fn sequence_of_nondigit_identifier_chars_should_be_scanned_as_identifier() {
        let mut scanner =
            Scanner::with_input("_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
        let num_nondigits = 1 + 26 * 2;

        let token = scanner.scan_next_token().unwrap();

        assert_eq!(
            token,
            Spanned::new(
                Token {
                    kind: TokenKind::Identifier
                },
                Span::from_raw_pos(0, num_nondigits),
            )
        );
    }

    #[test]
    fn identifiers_can_be_made_of_only_one_nondigit_identifier_char() {
        let nondigit_chars = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".chars();

        for nondigit_char in nondigit_chars {
            let text_input = format!("{}", nondigit_char);
            let mut scanner = Scanner::with_input(&text_input);

            let token = scanner.scan_next_token();

            let expected_token = Spanned::new(
                Token {
                    kind: TokenKind::Identifier,
                },
                Span::from_raw_pos(0, 1),
            );

            assert_eq!(
                token,
                Ok(expected_token),
                "scanned input: `{}`",
                nondigit_char
            );
        }
    }

    #[test]
    fn nondigit_char_followed_by_digit_chars_should_be_scanned_as_identifier() {
        let nondigit_chars = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".chars();
        let digits = "0123456789";

        for nondigit_char in nondigit_chars {
            // Note that the last whitespace in the input is important to make this
            // test check that the scanner indeed consumes digits, otherwise it could
            // just scan until EOF, which isn't what we want.
            let text_input = format!("{}{} ", nondigit_char, digits);
            let mut scanner = Scanner::with_input(&text_input);

            let token = scanner.scan_next_token();

            let expected_token = Spanned::new(
                Token {
                    kind: TokenKind::Identifier,
                },
                Span::from_raw_pos(0, 1 + digits.len()),
            );

            assert_eq!(token, Ok(expected_token), "scanned input: `{}`", text_input);
        }
    }

    #[test]
    fn scanning_of_identifiers_should_stop_at_a_non_identifier_char() {
        let mut scanner = Scanner::with_input("foo1 bar2");

        let token_foo = scanner.scan_next_token().unwrap();

        assert_eq!(
            token_foo,
            Spanned::new(
                Token {
                    kind: TokenKind::Identifier
                },
                Span::from_raw_pos(0, 4)
            )
        );

        let token_bar = scanner.scan_next_token().unwrap();

        assert_eq!(
            token_bar,
            Spanned::new(
                Token {
                    kind: TokenKind::Identifier
                },
                Span::from_raw_pos(5, 9)
            )
        );
    }

    // TODO: Test that multiple calls to scan_next_token ignores whitespace.
    #[test]
    fn whitespace_at_the_start_of_the_input_should_be_ignored_when_scanned() {
        let space = ' ';
        let tab = '\t';
        let line_feed = '\n';
        let carriage_return = '\r';
        let vertical_tab = '\x0b';
        let form_feed = '\x0c';

        let input_text = format!(
            "{}{}{}{}{}{}foo",
            space, tab, line_feed, carriage_return, vertical_tab, form_feed
        );
        let mut scanner = Scanner::with_input(&input_text);

        let token = scanner.scan_next_token().unwrap();

        assert_eq!(
            token,
            Spanned::new(
                Token {
                    kind: TokenKind::Identifier
                },
                Span::from_raw_pos(6, 9)
            )
        );
    }

    #[test]
    fn is_whitespace_char_should_return_true_for_c_lang_whitespace_chars_and_false_otherwise() {
        use super::is_whitespace_char;

        assert!(is_whitespace_char(' '));
        assert!(is_whitespace_char('\t'));
        assert!(is_whitespace_char('\n'));
        assert!(is_whitespace_char('\r'));
        assert!(is_whitespace_char('\x0b'));
        assert!(is_whitespace_char('\x0c'));

        let all_ascii_non_whitespaces =
            (0u8..128).filter(|ch| !matches!(ch, b' ' | b'\t' | b'\n' | b'\r' | b'\x0b' | b'\x0c'));

        for ch in all_ascii_non_whitespaces {
            assert!(!is_whitespace_char(ch as char), "ASCII code is: `{}`", ch);
        }
    }

    #[test]
    fn sequence_of_one_or_more_ascii_digits_should_be_scanned_as_numeric_constant() {
        utils::assert_numeric_constants(&[
            "1234567890",
            "2340056",
            "352",
            "4402",
            "562307",
            "629",
            "70001",
            "81",
            "93903458062390497540956",
            "1000000000000000000000000000000000000000000000000000000",
            "000",
            "000123",
            "0",
            "1",
            "2",
            "3",
            "4",
            "5",
            "6",
            "7",
            "8",
            "9",
        ]);
    }

    #[test]
    fn digits_with_alphanumeric_chars_mixed_in_should_be_scanned_as_a_numeric_constant() {
        utils::assert_numeric_constants(&[
            "123abc",
            "1a",
            "000i",
            "0xFFFFF",
            "1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
            "123abc456",
        ]);
    }

    #[test]
    fn digits_with_scientific_notation_should_be_scanned_as_a_numeric_constant() {
        utils::assert_numeric_constants(&[
            "123e0456",
            "123e+0456",
            "123e-0456",
            "123p0456",
            "123p+0456",
            "123p-0456",
        ]);
    }

    #[test]
    fn numeric_constants_stop_being_scanned_after_reaching_a_punctuation() {
        let mut scanner = Scanner::with_input("12345+6789-567");

        // TODO: Refactor these assertion sequences into a asserting function
        // that scans the next token, and compares the token's kind and span.
        let token = scanner.scan_next_token();
        let expected_token = Spanned::new(
            Token {
                kind: TokenKind::NumericConstant,
            },
            Span::from_raw_pos(0, 5),
        );
        assert_eq!(token, Ok(expected_token));

        let token = scanner.scan_next_token();
        let expected_token = Spanned::new(
            Token {
                kind: TokenKind::Plus,
            },
            Span::from_raw_pos(5, 6),
        );
        assert_eq!(token, Ok(expected_token));

        let token = scanner.scan_next_token();
        let expected_token = Spanned::new(
            Token {
                kind: TokenKind::NumericConstant,
            },
            Span::from_raw_pos(6, 10),
        );
        assert_eq!(token, Ok(expected_token));

        let token = scanner.scan_next_token();
        let expected_token = Spanned::new(
            Token {
                kind: TokenKind::Minus,
            },
            Span::from_raw_pos(10, 11),
        );
        assert_eq!(token, Ok(expected_token));

        let token = scanner.scan_next_token();
        let expected_token = Spanned::new(
            Token {
                kind: TokenKind::NumericConstant,
            },
            Span::from_raw_pos(11, 14),
        );
        assert_eq!(token, Ok(expected_token));

        assert_eq!(scanner.scan_next_token(), Ok(Token::EOF));
    }

    mod utils {
        use super::*;

        pub(super) fn assert_numeric_constants(numeric_constants: &[&str]) {
            let input_text = format!("{}\n", numeric_constants.join(" "));
            let mut scanner = Scanner::with_input(&input_text);

            let mut cursor = 0;

            for decimal_digit_seq in numeric_constants {
                let end_cursor = cursor + decimal_digit_seq.len();
                let expected_span = Span::from_raw_pos(cursor, end_cursor);

                let token = scanner.scan_next_token();
                let expected_token = Token {
                    kind: TokenKind::NumericConstant,
                };

                assert_eq!(
                    token,
                    Ok(Spanned::new(expected_token, expected_span)),
                    "numeric constant: `{}`",
                    decimal_digit_seq
                );

                // Plus one to account for the whitespace between the numbers.
                cursor = end_cursor + 1;
            }
        }
    }
}
