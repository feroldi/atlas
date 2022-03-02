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

#[derive(PartialEq, Debug, Copy, Clone)]
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
            // TODO: Define our own functions for these chars matching.
            ch if ch.is_ascii_digit() => {
                // TODO: Refactor this section into a function.
                let mut prev_peek = ch;

                while self.chars.peek().is_ascii_alphanumeric() {
                    prev_peek = self.chars.peek();
                    self.chars.consume();
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

// TODO: Test.
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
    use crate::source_map::SourceFile;
    use proptest::prelude::*;

    #[test]
    fn scanning_an_empty_input_should_return_an_eof_token() {
        let mut scanner = Scanner::with_input("");

        assert_eq!(scanner.scan_next_token(), Ok(Token::EOF));
    }

    #[test]
    fn scan_punctuations() {
        assert_eq!(
            scan_first_token(")"),
            (TokenKind::Closed(Bracket::Round), ")")
        );
        assert_eq!(
            scan_first_token("["),
            (TokenKind::Open(Bracket::Square), "[")
        );
        assert_eq!(
            scan_first_token("]"),
            (TokenKind::Closed(Bracket::Square), "]")
        );
        assert_eq!(
            scan_first_token("{"),
            (TokenKind::Open(Bracket::Curly), "{")
        );
        assert_eq!(
            scan_first_token("}"),
            (TokenKind::Closed(Bracket::Curly), "}")
        );
        assert_eq!(scan_first_token("."), (TokenKind::Period, "."));
        assert_eq!(scan_first_token("->"), (TokenKind::Arrow, "->"));
        assert_eq!(scan_first_token("++"), (TokenKind::PlusPlus, "++"));
        assert_eq!(scan_first_token("--"), (TokenKind::MinusMinus, "--"));
        assert_eq!(scan_first_token("&"), (TokenKind::Ampersand, "&"));
        assert_eq!(scan_first_token("*"), (TokenKind::Star, "*"));
        assert_eq!(scan_first_token("+"), (TokenKind::Plus, "+"));
        assert_eq!(scan_first_token("-"), (TokenKind::Minus, "-"));
        assert_eq!(scan_first_token("~"), (TokenKind::Tilde, "~"));
        assert_eq!(scan_first_token("!"), (TokenKind::Exclamation, "!"));
        assert_eq!(scan_first_token("/"), (TokenKind::Slash, "/"));
        assert_eq!(scan_first_token("%"), (TokenKind::Percent, "%"));
        assert_eq!(scan_first_token("<<"), (TokenKind::LessLess, "<<"));
        assert_eq!(scan_first_token(">>"), (TokenKind::GreaterGreater, ">>"));
        assert_eq!(scan_first_token("<"), (TokenKind::Less, "<"));
        assert_eq!(scan_first_token(">"), (TokenKind::Greater, ">"));
        assert_eq!(scan_first_token("<="), (TokenKind::LessEqual, "<="));
        assert_eq!(scan_first_token(">="), (TokenKind::GreaterEqual, ">="));
        assert_eq!(scan_first_token("=="), (TokenKind::EqualEqual, "=="));
        assert_eq!(scan_first_token("!="), (TokenKind::ExclaEqual, "!="));
        assert_eq!(scan_first_token("^"), (TokenKind::Caret, "^"));
        assert_eq!(scan_first_token("|"), (TokenKind::Pipe, "|"));
        assert_eq!(scan_first_token("&&"), (TokenKind::AmpAmp, "&&"));
        assert_eq!(scan_first_token("||"), (TokenKind::PipePipe, "||"));
        assert_eq!(scan_first_token("?"), (TokenKind::Question, "?"));
        assert_eq!(scan_first_token(":"), (TokenKind::Colon, ":"));
        assert_eq!(scan_first_token(";"), (TokenKind::Semicolon, ";"));
        assert_eq!(scan_first_token("..."), (TokenKind::Ellipsis, "..."));
        assert_eq!(scan_first_token("="), (TokenKind::Equal, "="));
        assert_eq!(scan_first_token("*="), (TokenKind::StarEqual, "*="));
        assert_eq!(scan_first_token("/="), (TokenKind::SlashEqual, "/="));
        assert_eq!(scan_first_token("%="), (TokenKind::PercentEqual, "%="));
        assert_eq!(scan_first_token("+="), (TokenKind::PlusEqual, "+="));
        assert_eq!(scan_first_token("-="), (TokenKind::MinusEqual, "-="));
        assert_eq!(scan_first_token("<<="), (TokenKind::LessLessEqual, "<<="));
        assert_eq!(
            scan_first_token(">>="),
            (TokenKind::GreaterGreaterEqual, ">>=")
        );
        assert_eq!(scan_first_token("&="), (TokenKind::AmpEqual, "&="));
        assert_eq!(scan_first_token("^="), (TokenKind::CaretEqual, "^="));
        assert_eq!(scan_first_token("|="), (TokenKind::PipeEqual, "|="));
        assert_eq!(scan_first_token(","), (TokenKind::Comma, ","));
        assert_eq!(scan_first_token("#"), (TokenKind::Hash, "#"));
        assert_eq!(scan_first_token("##"), (TokenKind::HashHash, "##"));
    }

    #[test]
    fn scan_keywords() {
        assert_eq!(scan_first_token("auto"), (TokenKind::KwAuto, "auto"));
        assert_eq!(scan_first_token("break"), (TokenKind::KwBreak, "break"));
        assert_eq!(scan_first_token("case"), (TokenKind::KwCase, "case"));
        assert_eq!(scan_first_token("char"), (TokenKind::KwChar, "char"));
        assert_eq!(scan_first_token("const"), (TokenKind::KwConst, "const"));
        assert_eq!(
            scan_first_token("continue"),
            (TokenKind::KwContinue, "continue")
        );
        assert_eq!(
            scan_first_token("default"),
            (TokenKind::KwDefault, "default")
        );
        assert_eq!(scan_first_token("do"), (TokenKind::KwDo, "do"));
        assert_eq!(scan_first_token("double"), (TokenKind::KwDouble, "double"));
        assert_eq!(scan_first_token("else"), (TokenKind::KwElse, "else"));
        assert_eq!(scan_first_token("enum"), (TokenKind::KwEnum, "enum"));
        assert_eq!(scan_first_token("extern"), (TokenKind::KwExtern, "extern"));
        assert_eq!(scan_first_token("float"), (TokenKind::KwFloat, "float"));
        assert_eq!(scan_first_token("for"), (TokenKind::KwFor, "for"));
        assert_eq!(scan_first_token("goto"), (TokenKind::KwGoto, "goto"));
        assert_eq!(scan_first_token("if"), (TokenKind::KwIf, "if"));
        assert_eq!(scan_first_token("inline"), (TokenKind::KwInline, "inline"));
        assert_eq!(scan_first_token("int"), (TokenKind::KwInt, "int"));
        assert_eq!(scan_first_token("long"), (TokenKind::KwLong, "long"));
        assert_eq!(
            scan_first_token("register"),
            (TokenKind::KwRegister, "register")
        );
        assert_eq!(
            scan_first_token("restrict"),
            (TokenKind::KwRestrict, "restrict")
        );
        assert_eq!(scan_first_token("return"), (TokenKind::KwReturn, "return"));
        assert_eq!(scan_first_token("short"), (TokenKind::KwShort, "short"));
        assert_eq!(scan_first_token("signed"), (TokenKind::KwSigned, "signed"));
        assert_eq!(scan_first_token("sizeof"), (TokenKind::KwSizeof, "sizeof"));
        assert_eq!(scan_first_token("static"), (TokenKind::KwStatic, "static"));
        assert_eq!(scan_first_token("struct"), (TokenKind::KwStruct, "struct"));
        assert_eq!(scan_first_token("switch"), (TokenKind::KwSwitch, "switch"));
        assert_eq!(
            scan_first_token("typedef"),
            (TokenKind::KwTypedef, "typedef")
        );
        assert_eq!(scan_first_token("union"), (TokenKind::KwUnion, "union"));
        assert_eq!(
            scan_first_token("unsigned"),
            (TokenKind::KwUnsigned, "unsigned")
        );
        assert_eq!(scan_first_token("void"), (TokenKind::KwVoid, "void"));
        assert_eq!(
            scan_first_token("volatile"),
            (TokenKind::KwVolatile, "volatile")
        );
        assert_eq!(scan_first_token("while"), (TokenKind::KwWhile, "while"));
        assert_eq!(
            scan_first_token("_Alignas"),
            (TokenKind::KwAlignas, "_Alignas")
        );
        assert_eq!(
            scan_first_token("_Alignof"),
            (TokenKind::KwAlignof, "_Alignof")
        );
        assert_eq!(
            scan_first_token("_Atomic"),
            (TokenKind::KwAtomic, "_Atomic")
        );
        assert_eq!(scan_first_token("_Bool"), (TokenKind::KwBool, "_Bool"));
        assert_eq!(
            scan_first_token("_Complex"),
            (TokenKind::KwComplex, "_Complex")
        );
        assert_eq!(
            scan_first_token("_Generic"),
            (TokenKind::KwGeneric, "_Generic")
        );
        assert_eq!(
            scan_first_token("_Imaginary"),
            (TokenKind::KwImaginary, "_Imaginary")
        );
        assert_eq!(
            scan_first_token("_Noreturn"),
            (TokenKind::KwNoreturn, "_Noreturn")
        );
        assert_eq!(
            scan_first_token("_Static_assert"),
            (TokenKind::KwStaticAssert, "_Static_assert")
        );
        assert_eq!(
            scan_first_token("_Thread_local"),
            (TokenKind::KwThreadLocal, "_Thread_local")
        );
    }

    #[test]
    fn scan_two_adjacent_period_chars_as_two_separate_period_punctuations() {
        assert_eq!(
            scan_tokens(".."),
            [(TokenKind::Period, "."), (TokenKind::Period, ".")]
        );
    }

    #[test]
    fn scan_single_nondigit_char_as_identifier() {
        use std::iter::once;

        let nondigit_chars = once('_').chain('a'..='z').chain('A'..='Z');

        for nondigit_char in nondigit_chars {
            let input_text = format!("{}", nondigit_char);
            let token = scan_first_token(&input_text);

            assert_eq!(token, (TokenKind::Identifier, &*input_text));
        }
    }

    fn identifier() -> impl Strategy<Value = String> {
        use super::get_keyword_kind_for_lexeme;

        let ident = proptest::string::string_regex("[_a-zA-Z][_0-9a-zA-Z]*").unwrap();

        ident.prop_filter("must not be a keyword", |ident| {
            get_keyword_kind_for_lexeme(ident).is_none()
        })
    }

    prop_compose! {
        fn non_identifier_chars()(non_ident in "[^_0-9a-zA-Z]+") -> String {
            non_ident
        }
    }

    proptest! {
        #[test]
        fn scan_valid_identifier(input_text in identifier()) {
            let input_text = input_text.as_str();
            let tokens = scan_tokens(input_text);

            assert_eq!(tokens, [(TokenKind::Identifier, input_text)]);
        }
    }

    proptest! {
        #[test]
        fn scan_identifier_until_it_reaches_a_non_identifier_char(
            ident in identifier(),
            non_ident in non_identifier_chars()
        ) {
            let input_text = format!("{}{}", ident, non_ident);
            let token = scan_first_token(&input_text);

            assert_eq!(token, (TokenKind::Identifier, &*ident));
        }
    }

    // TODO: Remove.
    #[test]
    fn scanning_of_identifiers_should_stop_at_a_non_identifier_char() {
        assert_eq!(
            scan_tokens("foo1 bar2"),
            [
                (TokenKind::Identifier, "foo1"),
                (TokenKind::Identifier, "bar2"),
            ]
        );
    }

    #[test]
    fn whitespace_at_the_start_of_the_input_should_be_ignored_when_scanned() {
        assert_eq!(
            scan_tokens(" \t\n\r\x0b\x0cfoo \t\n\rbar\x0b\x0c"),
            [
                (TokenKind::Identifier, "foo"),
                (TokenKind::Identifier, "bar"),
            ]
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
        assert_numeric_constants(&[
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
        assert_numeric_constants(&[
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
        assert_numeric_constants(&[
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
        assert_eq!(
            scan_tokens("12345+6789-567"),
            [
                (TokenKind::NumericConstant, "12345"),
                (TokenKind::Plus, "+"),
                (TokenKind::NumericConstant, "6789"),
                (TokenKind::Minus, "-"),
                (TokenKind::NumericConstant, "567"),
            ]
        );
    }

    fn assert_numeric_constants(numeric_constants: &[&str]) {
        let input_text = format!("{}\n", numeric_constants.join(" "));

        let mut scanner = Scanner::with_input(&input_text);
        let source_file = SourceFile::new(&input_text);

        for &decimal_digit_seq in numeric_constants {
            let token = scanner.scan_next_token().unwrap();

            let expected_token = Token {
                kind: TokenKind::NumericConstant,
            };

            assert_eq!(
                *token, expected_token,
                "numeric constant: `{}`",
                decimal_digit_seq
            );

            assert_eq!(
                source_file.get_text_snippet(token),
                decimal_digit_seq,
                "numeric constant: `{}`",
                decimal_digit_seq
            );
        }
    }

    struct TokenKindAndLexemeIter<'input> {
        scanner: Scanner<'input>,
        source_file: SourceFile<'input>,
    }

    impl<'input> TokenKindAndLexemeIter<'input> {
        fn new(input_text: &'input str) -> TokenKindAndLexemeIter<'input> {
            TokenKindAndLexemeIter {
                scanner: Scanner::with_input(input_text),
                source_file: SourceFile::new(input_text),
            }
        }
    }

    impl<'input> Iterator for TokenKindAndLexemeIter<'input> {
        type Item = (TokenKind, &'input str);

        fn next(&mut self) -> Option<(TokenKind, &'input str)> {
            // TODO(feroldi): What to do when not Ok(_)?
            if let Ok(token) = self.scanner.scan_next_token() {
                if token != Token::EOF {
                    let lexeme = self.source_file.get_text_snippet(token);
                    return Some((token.kind, lexeme));
                }
            }

            None
        }
    }

    fn scan_tokens(input_text: &str) -> Vec<(TokenKind, &str)> {
        TokenKindAndLexemeIter::new(input_text).collect::<Vec<_>>()
    }

    fn scan_first_token(input_text: &str) -> (TokenKind, &str) {
        TokenKindAndLexemeIter::new(input_text).next().unwrap()
    }
}
