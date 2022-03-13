use crate::char_stream::CharStream;
use crate::source_map::{Span, Spanned};
use std::assert_matches::debug_assert_matches;

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Bracket {
    Round,
    Square,
    Curly,
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
    Eof,
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

    // TODO: Refactor this into free functions that scan a specific set of token categories. For
    // example, have this check if peek is ascii punctuation, then call the punctuation scanning
    // function passing in the char-stream.
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
                // TODO(feroldi): Refactor with numeric_constant scanning.
                if self.chars.peek().is_ascii_digit() {
                    while self.chars.peek().is_ascii_digit() {
                        self.chars.consume();
                    }
                    TokenKind::NumericConstant
                } else if self.chars.peek() == '.' && self.chars.lookahead(1) == '.' {
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
            // TODO: Define our own functions for these chars matching.
            ch if ch.is_ascii_digit() => {
                // TODO: Refactor this section into a function.
                let mut prev_peek = ch;

                while self.chars.peek().is_ascii_alphanumeric() || self.chars.peek() == '.' {
                    prev_peek = self.chars.consume();
                }

                // TODO: Improve this code's readability.
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

        get_keyword_kind_for_lexeme(&lexeme_buffer).unwrap_or(TokenKind::Identifier)
    }
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

// TODO(feroldi): @charset Refactor this characters set into a module.
// TODO(feroldi): Rename to is_whitespace
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

fn is_identifier_head(ch: char) -> bool {
    matches!(ch, 'a'..='z' | 'A'..='Z' | '_')
}

fn is_identifier_tail(ch: char) -> bool {
    matches!(ch, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9')
}
