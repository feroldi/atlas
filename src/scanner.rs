use crate::char_stream::CharStream;
use crate::source_map::{Span, Spanned};

pub struct Scanner<'input> {
    chars: CharStream<'input>,
}

impl<'input> Scanner<'input> {
    pub fn with_input(source_text: &'input str) -> Scanner<'input> {
        Scanner {
            chars: CharStream::with_text(source_text),
        }
    }

    pub fn scan_next_token(&mut self) -> Result<Spanned<Token>, Diag> {
        while is_space(self.peek()) {
            self.consume();
        }

        let span_start = self.chars.peek_byte_pos();

        let token_kind = match self.consume() {
            CharStream::EOF_CHAR => return Ok(Token::EOF),
            '(' => TokenKind::Open(Bracket::Round),
            ')' => TokenKind::Closed(Bracket::Round),
            '[' => TokenKind::Open(Bracket::Square),
            ']' => TokenKind::Closed(Bracket::Square),
            '{' => TokenKind::Open(Bracket::Curly),
            '}' => TokenKind::Closed(Bracket::Curly),
            '.' => {
                if is_digit(self.peek()) {
                    let first_digit = self.consume();
                    self.scan_numeric_constant(first_digit)
                } else if self.peek() == '.' && self.lookahead(1) == '.' {
                    self.consume();
                    self.consume();
                    TokenKind::Ellipsis
                } else {
                    TokenKind::Period
                }
            }
            '-' => {
                if self.try_consume('>') {
                    TokenKind::Arrow
                } else if self.try_consume('-') {
                    TokenKind::MinusMinus
                } else if self.try_consume('=') {
                    TokenKind::MinusEqual
                } else {
                    TokenKind::Minus
                }
            }
            '+' => {
                if self.try_consume('+') {
                    TokenKind::PlusPlus
                } else if self.try_consume('=') {
                    TokenKind::PlusEqual
                } else {
                    TokenKind::Plus
                }
            }
            '&' => {
                if self.try_consume('&') {
                    TokenKind::AmpAmp
                } else if self.try_consume('=') {
                    TokenKind::AmpEqual
                } else {
                    TokenKind::Ampersand
                }
            }
            '*' => {
                if self.try_consume('=') {
                    TokenKind::StarEqual
                } else {
                    TokenKind::Star
                }
            }
            '~' => TokenKind::Tilde,
            '!' => {
                if self.try_consume('=') {
                    TokenKind::ExclaEqual
                } else {
                    TokenKind::Exclamation
                }
            }
            '/' => {
                if self.try_consume('=') {
                    TokenKind::SlashEqual
                } else {
                    TokenKind::Slash
                }
            }
            '%' => {
                if self.try_consume('=') {
                    TokenKind::PercentEqual
                } else {
                    TokenKind::Percent
                }
            }
            '<' => {
                if self.try_consume('<') {
                    if self.try_consume('=') {
                        TokenKind::LessLessEqual
                    } else {
                        TokenKind::LessLess
                    }
                } else if self.try_consume('=') {
                    TokenKind::LessEqual
                } else {
                    TokenKind::Less
                }
            }
            '>' => {
                if self.try_consume('>') {
                    if self.try_consume('=') {
                        TokenKind::GreaterGreaterEqual
                    } else {
                        TokenKind::GreaterGreater
                    }
                } else if self.try_consume('=') {
                    TokenKind::GreaterEqual
                } else {
                    TokenKind::Greater
                }
            }
            '=' => {
                if self.try_consume('=') {
                    TokenKind::EqualEqual
                } else {
                    TokenKind::Equal
                }
            }
            '^' => {
                if self.try_consume('=') {
                    TokenKind::CaretEqual
                } else {
                    TokenKind::Caret
                }
            }
            '|' => {
                if self.try_consume('|') {
                    TokenKind::PipePipe
                } else if self.try_consume('=') {
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
                if self.try_consume('#') {
                    TokenKind::HashHash
                } else {
                    TokenKind::Hash
                }
            }
            '\'' => self.scan_character_constant(CharEncoding::Byte)?,
            prefix @ ('L' | 'u' | 'U') if self.peek() == '\'' => {
                self.consume();

                let char_encoding = match prefix {
                    'L' => CharEncoding::Wide,
                    'u' => CharEncoding::Utf16,
                    'U' => CharEncoding::Utf32,
                    _ => unreachable!("invalid character constant prefix '{}'!", prefix),
                };

                self.scan_character_constant(char_encoding)?
            }
            ch if is_digit(ch) => self.scan_numeric_constant(ch),
            ch if is_start_of_identifier(ch) => self.scan_identifier_or_keyword(ch),
            unrecognized_char => return Err(Diag::UnrecognizedChar(unrecognized_char)),
        };

        let span_end = self.chars.peek_byte_pos();

        let token_span = Span {
            start: span_start,
            end: span_end,
        };

        Ok(Spanned::new(Token { kind: token_kind }, token_span))
    }

    fn scan_character_constant(&mut self, encoding: CharEncoding) -> Result<TokenKind, Diag> {
        if self.peek() == '\'' {
            self.consume();
            return Err(Diag::EmptyCharacterConstant);
        }

        while self.peek() != '\'' {
            if is_newline(self.peek()) || self.peek() == CharStream::EOF_CHAR {
                return Err(Diag::UnterminatedCharacterConstant);
            }

            // Skips backslashes. This effectively escapes single-quotes. Validation of
            // escape sequences occurs later on during parsing, which means
            // character-constant tokens may be semantically invalid. Such situation is
            // similar to numeric-constant tokens.
            // NOTE: Review this during implementation of escaping newlines in code.
            // @escape-newline.
            if self.peek() == '\\' {
                self.consume();
            }

            self.consume();
        }

        let terminating_quote = self.consume();
        debug_assert_eq!(terminating_quote, '\'');

        Ok(TokenKind::CharacterConstant { encoding })
    }

    fn scan_numeric_constant(&mut self, first_digit: char) -> TokenKind {
        debug_assert!(is_numeric_constant_char(first_digit));

        let mut prev_peek = first_digit;

        // Intentionally scans a more general case of a numeric constant, which may turn
        // out to be ill-formed as per the standard. This means a numeric constant token
        // is not guaranteed to be correct.
        //
        // A more strict and conforming scanning is done during parsing, which is when a
        // ill-formed numeric constant is diagnosed. This takes some burden away
        // from the scanner, and also makes the parser's job somewhat simpler, as
        // the character set to be considered during the parsing of a numeric
        // constant token is greatly reduced.
        loop {
            while is_numeric_constant_char(self.peek()) {
                prev_peek = self.consume();
            }

            if matches!(self.peek(), '+' | '-') && matches!(prev_peek, 'e' | 'E' | 'p' | 'P') {
                self.consume();
                continue;
            }

            break;
        }

        TokenKind::NumericConstant
    }

    fn scan_identifier_or_keyword(&mut self, ident_start: char) -> TokenKind {
        debug_assert!(is_start_of_identifier(ident_start));

        let mut lexeme_buffer = String::with_capacity(16);
        lexeme_buffer.push(ident_start);

        while is_remaining_of_identifier(self.peek()) {
            lexeme_buffer.push(self.consume());
        }

        get_keyword_kind_for_lexeme(&lexeme_buffer).unwrap_or(TokenKind::Identifier)
    }

    fn peek(&self) -> char {
        self.chars.peek()
    }

    fn lookahead(&self, n: usize) -> char {
        self.chars.lookahead(n)
    }

    fn consume(&mut self) -> char {
        self.chars.consume()
    }

    fn try_consume(&mut self, expected_char: char) -> bool {
        self.chars.try_consume(expected_char)
    }
}

// TODO(feroldi): Add `is_eof()` function.
#[derive(PartialEq, Debug, Copy, Clone)]
pub struct Token {
    pub kind: TokenKind,
}

impl Token {
    pub const EOF: Spanned<Token> = Spanned::with_dummy_span(Token {
        kind: TokenKind::Eof,
    });
}

#[derive(PartialEq, Debug, Copy, Clone)]
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
    CharacterConstant { encoding: CharEncoding },
    Eof,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Bracket {
    Round,
    Square,
    Curly,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum CharEncoding {
    Byte,
    Wide,
    Utf16,
    Utf32,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Diag {
    UnrecognizedChar(char),
    EmptyCharacterConstant,
    UnterminatedCharacterConstant,
}

fn get_keyword_kind_for_lexeme(lexeme: &str) -> Option<TokenKind> {
    let keyword_kind = match lexeme {
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
        _ => return None,
    };

    Some(keyword_kind)
}

const fn is_newline(ch: char) -> bool {
    matches!(ch, '\n' | '\r')
}

const fn is_numeric_constant_char(ch: char) -> bool {
    is_alpha(ch) || is_digit(ch) || ch == '.'
}

const fn is_start_of_identifier(ch: char) -> bool {
    is_alpha(ch) || ch == '_'
}

const fn is_remaining_of_identifier(ch: char) -> bool {
    is_start_of_identifier(ch) || is_digit(ch)
}

const fn is_space(ch: char) -> bool {
    matches!(ch, '\x20' | '\t' | '\n' | '\r' | '\x0b' | '\x0c')
}

const fn is_digit(ch: char) -> bool {
    matches!(ch, '0'..='9')
}

const fn is_alpha(ch: char) -> bool {
    matches!(ch, 'A'..='Z' | 'a'..='z')
}
