use crate::char_stream::CharStream;
use crate::source_map::{Span, Spanned};

pub struct Scanner<'chars> {
    chars: CharStream<'chars>,
}

impl Scanner<'_> {
    pub fn with_input(source_text: &str) -> Scanner {
        Scanner {
            chars: CharStream::with_text(source_text),
        }
    }

    // TODO: Refactor this into free functions that scan a specific set of token
    // categories. For example, have this check if peek is ascii punctuation,
    // then call the punctuation scanning function passing in the char-stream.
    pub fn scan_next_token(&mut self) -> Result<Spanned<Token>, Diag> {
        while is_whitespace_char(self.peek()) {
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
            ch if is_digit(ch) => self.scan_numeric_constant(ch),
            ch if is_identifier_head(ch) => self.scan_identifier_or_keyword(ch),
            unrecognized_char => return Err(Diag::UnrecognizedChar(unrecognized_char)),
        };

        let span_end = self.chars.peek_byte_pos();
        let token_span = Span {
            start: span_start,
            end: span_end,
        };

        Ok(Spanned::new(Token { kind: token_kind }, token_span))
    }

    fn scan_numeric_constant(&mut self, first_digit: char) -> TokenKind {
        debug_assert!(is_numeric_constant_char(first_digit));

        let mut prev_peek = first_digit;

        // We consume a possibly ill-formed numeric constant, because we
        // delegate the diagnosing to the parser. This is so that diagnosing
        // becomes easier, as the scope of possible characters to analyse is
        // the numeric constant's. We also guarantee that a num-const token
        // doesn't start with adjacent periods, and any exponent found in it is
        // always correct regarding the position of the sign.
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

    fn scan_identifier_or_keyword(&mut self, ident_head: char) -> TokenKind {
        debug_assert!(is_identifier_head(ident_head), "char: `{}`", ident_head);

        let mut lexeme_buffer = String::with_capacity(16);
        lexeme_buffer.push(ident_head);

        while is_identifier_body(self.peek()) {
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
    Eof,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Bracket {
    Round,
    Square,
    Curly,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Diag {
    UnrecognizedChar(char),
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

fn is_numeric_constant_char(ch: char) -> bool {
    matches!(ch, '.' | '0'..='9' | 'a'..='z' | 'A'..='Z')
}

fn is_digit(ch: char) -> bool {
    matches!(ch, '0'..='9')
}

fn is_identifier_head(ch: char) -> bool {
    matches!(ch, 'a'..='z' | 'A'..='Z' | '_')
}

fn is_identifier_body(ch: char) -> bool {
    matches!(ch, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9')
}
