use atlas::scanner::{Bracket, Diag, Scanner, Token, TokenKind};
use atlas::source_map::SourceFile;
use proptest::prelude::*;
use proptest::string::string_regex;
use util::*;

mod util;

#[test]
fn scanning_an_empty_input_should_return_an_eof_token() {
    let mut scanner = Scanner::with_input("");

    assert_eq!(scanner.scan_next_token(), Ok(Token::EOF));
}

#[test]
fn scan_punctuations() {
    assert_eq!(scan_first("("), (TokenKind::Open(Bracket::Round), "("));
    assert_eq!(scan_first(")"), (TokenKind::Closed(Bracket::Round), ")"));
    assert_eq!(scan_first("["), (TokenKind::Open(Bracket::Square), "["));
    assert_eq!(scan_first("]"), (TokenKind::Closed(Bracket::Square), "]"));
    assert_eq!(scan_first("{"), (TokenKind::Open(Bracket::Curly), "{"));
    assert_eq!(scan_first("}"), (TokenKind::Closed(Bracket::Curly), "}"));
    assert_eq!(scan_first("."), (TokenKind::Period, "."));
    assert_eq!(scan_first("->"), (TokenKind::Arrow, "->"));
    assert_eq!(scan_first("++"), (TokenKind::PlusPlus, "++"));
    assert_eq!(scan_first("--"), (TokenKind::MinusMinus, "--"));
    assert_eq!(scan_first("&"), (TokenKind::Ampersand, "&"));
    assert_eq!(scan_first("*"), (TokenKind::Star, "*"));
    assert_eq!(scan_first("+"), (TokenKind::Plus, "+"));
    assert_eq!(scan_first("-"), (TokenKind::Minus, "-"));
    assert_eq!(scan_first("~"), (TokenKind::Tilde, "~"));
    assert_eq!(scan_first("!"), (TokenKind::Exclamation, "!"));
    assert_eq!(scan_first("/"), (TokenKind::Slash, "/"));
    assert_eq!(scan_first("%"), (TokenKind::Percent, "%"));
    assert_eq!(scan_first("<<"), (TokenKind::LessLess, "<<"));
    assert_eq!(scan_first(">>"), (TokenKind::GreaterGreater, ">>"));
    assert_eq!(scan_first("<"), (TokenKind::Less, "<"));
    assert_eq!(scan_first(">"), (TokenKind::Greater, ">"));
    assert_eq!(scan_first("<="), (TokenKind::LessEqual, "<="));
    assert_eq!(scan_first(">="), (TokenKind::GreaterEqual, ">="));
    assert_eq!(scan_first("=="), (TokenKind::EqualEqual, "=="));
    assert_eq!(scan_first("!="), (TokenKind::ExclaEqual, "!="));
    assert_eq!(scan_first("^"), (TokenKind::Caret, "^"));
    assert_eq!(scan_first("|"), (TokenKind::Pipe, "|"));
    assert_eq!(scan_first("&&"), (TokenKind::AmpAmp, "&&"));
    assert_eq!(scan_first("||"), (TokenKind::PipePipe, "||"));
    assert_eq!(scan_first("?"), (TokenKind::Question, "?"));
    assert_eq!(scan_first(":"), (TokenKind::Colon, ":"));
    assert_eq!(scan_first(";"), (TokenKind::Semicolon, ";"));
    assert_eq!(scan_first("..."), (TokenKind::Ellipsis, "..."));
    assert_eq!(scan_first("="), (TokenKind::Equal, "="));
    assert_eq!(scan_first("*="), (TokenKind::StarEqual, "*="));
    assert_eq!(scan_first("/="), (TokenKind::SlashEqual, "/="));
    assert_eq!(scan_first("%="), (TokenKind::PercentEqual, "%="));
    assert_eq!(scan_first("+="), (TokenKind::PlusEqual, "+="));
    assert_eq!(scan_first("-="), (TokenKind::MinusEqual, "-="));
    assert_eq!(scan_first("<<="), (TokenKind::LessLessEqual, "<<="));
    assert_eq!(scan_first(">>="), (TokenKind::GreaterGreaterEqual, ">>="));
    assert_eq!(scan_first("&="), (TokenKind::AmpEqual, "&="));
    assert_eq!(scan_first("^="), (TokenKind::CaretEqual, "^="));
    assert_eq!(scan_first("|="), (TokenKind::PipeEqual, "|="));
    assert_eq!(scan_first(","), (TokenKind::Comma, ","));
    assert_eq!(scan_first("#"), (TokenKind::Hash, "#"));
    assert_eq!(scan_first("##"), (TokenKind::HashHash, "##"));
}

#[test]
fn scan_keywords() {
    assert_eq!(scan_first("auto"), (TokenKind::KwAuto, "auto"));
    assert_eq!(scan_first("break"), (TokenKind::KwBreak, "break"));
    assert_eq!(scan_first("case"), (TokenKind::KwCase, "case"));
    assert_eq!(scan_first("char"), (TokenKind::KwChar, "char"));
    assert_eq!(scan_first("const"), (TokenKind::KwConst, "const"));
    assert_eq!(scan_first("continue"), (TokenKind::KwContinue, "continue"));
    assert_eq!(scan_first("default"), (TokenKind::KwDefault, "default"));
    assert_eq!(scan_first("do"), (TokenKind::KwDo, "do"));
    assert_eq!(scan_first("double"), (TokenKind::KwDouble, "double"));
    assert_eq!(scan_first("else"), (TokenKind::KwElse, "else"));
    assert_eq!(scan_first("enum"), (TokenKind::KwEnum, "enum"));
    assert_eq!(scan_first("extern"), (TokenKind::KwExtern, "extern"));
    assert_eq!(scan_first("float"), (TokenKind::KwFloat, "float"));
    assert_eq!(scan_first("for"), (TokenKind::KwFor, "for"));
    assert_eq!(scan_first("goto"), (TokenKind::KwGoto, "goto"));
    assert_eq!(scan_first("if"), (TokenKind::KwIf, "if"));
    assert_eq!(scan_first("inline"), (TokenKind::KwInline, "inline"));
    assert_eq!(scan_first("int"), (TokenKind::KwInt, "int"));
    assert_eq!(scan_first("long"), (TokenKind::KwLong, "long"));
    assert_eq!(scan_first("register"), (TokenKind::KwRegister, "register"));
    assert_eq!(scan_first("restrict"), (TokenKind::KwRestrict, "restrict"));
    assert_eq!(scan_first("return"), (TokenKind::KwReturn, "return"));
    assert_eq!(scan_first("short"), (TokenKind::KwShort, "short"));
    assert_eq!(scan_first("signed"), (TokenKind::KwSigned, "signed"));
    assert_eq!(scan_first("sizeof"), (TokenKind::KwSizeof, "sizeof"));
    assert_eq!(scan_first("static"), (TokenKind::KwStatic, "static"));
    assert_eq!(scan_first("struct"), (TokenKind::KwStruct, "struct"));
    assert_eq!(scan_first("switch"), (TokenKind::KwSwitch, "switch"));
    assert_eq!(scan_first("typedef"), (TokenKind::KwTypedef, "typedef"));
    assert_eq!(scan_first("union"), (TokenKind::KwUnion, "union"));
    assert_eq!(scan_first("unsigned"), (TokenKind::KwUnsigned, "unsigned"));
    assert_eq!(scan_first("void"), (TokenKind::KwVoid, "void"));
    assert_eq!(scan_first("volatile"), (TokenKind::KwVolatile, "volatile"));
    assert_eq!(scan_first("while"), (TokenKind::KwWhile, "while"));
    assert_eq!(scan_first("_Alignas"), (TokenKind::KwAlignas, "_Alignas"));
    assert_eq!(scan_first("_Alignof"), (TokenKind::KwAlignof, "_Alignof"));
    assert_eq!(scan_first("_Atomic"), (TokenKind::KwAtomic, "_Atomic"));
    assert_eq!(scan_first("_Bool"), (TokenKind::KwBool, "_Bool"));
    assert_eq!(scan_first("_Complex"), (TokenKind::KwComplex, "_Complex"));
    assert_eq!(scan_first("_Generic"), (TokenKind::KwGeneric, "_Generic"));
    assert_eq!(
        scan_first("_Imaginary"),
        (TokenKind::KwImaginary, "_Imaginary")
    );
    assert_eq!(
        scan_first("_Noreturn"),
        (TokenKind::KwNoreturn, "_Noreturn")
    );
    assert_eq!(
        scan_first("_Static_assert"),
        (TokenKind::KwStaticAssert, "_Static_assert")
    );
    assert_eq!(
        scan_first("_Thread_local"),
        (TokenKind::KwThreadLocal, "_Thread_local")
    );
}

#[test]
fn scan_two_adjacent_period_chars_as_two_separate_period_punctuations() {
    assert_eq!(
        scan_all(".."),
        [(TokenKind::Period, "."), (TokenKind::Period, ".")]
    );
}

#[test]
fn scan_single_nondigit_char_as_identifier() {
    use std::iter::once;

    let nondigit_chars = once('_').chain('a'..='z').chain('A'..='Z');

    for nondigit_char in nondigit_chars {
        let input_text = format!("{}", nondigit_char);
        let token = scan_first(&input_text);

        assert_eq!(token, (TokenKind::Identifier, &*input_text));
    }
}

// TODO(feroldi): @charset Refactor this characters set into a module.
fn identifier() -> impl Strategy<Value = String> {
    string_regex("[_a-zA-Z][_0-9a-zA-Z]*")
        .unwrap()
        .prop_filter("must not be a keyword", |ident| !is_keyword(ident))
}

proptest! {
    #[test]
    fn scan_valid_identifier(input_text in identifier()) {
        assert_eq!(
            scan_first(&input_text),
            (TokenKind::Identifier, &*input_text)
        );
    }
}

// TODO(feroldi): @charset Refactor this characters set into a module.
fn non_identifier_chars() -> impl Strategy<Value = String> {
    string_regex("[^_0-9a-zA-Z]+").unwrap()
}

proptest! {
    #[test]
    fn scan_identifier_until_it_reaches_a_non_identifier_char(
        ident in identifier(),
        non_ident in non_identifier_chars()
    ) {
        let input_text = format!("{}{}", ident, non_ident);

        assert_eq!(
            scan_first(&input_text),
            (TokenKind::Identifier, &*ident)
        );
    }
}

proptest! {
    #[test]
    fn whitespace_at_the_start_of_the_input_should_be_ignored_when_scanned(
        ws in whitespace(),
        ident in identifier(),
    ) {
        let input_text = format!("{}{}", ws, ident);

        assert_eq!(scan_first(&input_text), (TokenKind::Identifier, &*ident));
    }
}

proptest! {
    #[test]
    fn scan_decimal_digits_as_numeric_constant(decimal_digits in "[0-9]+") {
        assert_eq!(
            scan_first(&decimal_digits),
            (TokenKind::NumericConstant, &*decimal_digits)
        );
    }
}

#[test]
fn scan_single_decimal_digit_as_numeric_constant() {
    assert_eq!(scan_first("0"), (TokenKind::NumericConstant, "0"));
    assert_eq!(scan_first("1"), (TokenKind::NumericConstant, "1"));
    assert_eq!(scan_first("2"), (TokenKind::NumericConstant, "2"));
    assert_eq!(scan_first("3"), (TokenKind::NumericConstant, "3"));
    assert_eq!(scan_first("4"), (TokenKind::NumericConstant, "4"));
    assert_eq!(scan_first("5"), (TokenKind::NumericConstant, "5"));
    assert_eq!(scan_first("6"), (TokenKind::NumericConstant, "6"));
    assert_eq!(scan_first("7"), (TokenKind::NumericConstant, "7"));
    assert_eq!(scan_first("8"), (TokenKind::NumericConstant, "8"));
    assert_eq!(scan_first("9"), (TokenKind::NumericConstant, "9"));
}

fn stop_char_for_num_const() -> impl Strategy<Value = String> {
    string_regex("[^0-9a-zA-Z.]").unwrap()
}

proptest! {
    #[test]
    fn numeric_constant_can_have_a_period_punctuation_in_the_middle(
        num_const in "[0-9]+[.][0-9]+",
        stop_char in stop_char_for_num_const(),
    ) {
        let input_text = format!("{}{}", num_const, stop_char);
        assert_eq!(
            scan_first(&input_text),
            (TokenKind::NumericConstant, &*num_const)
        );
    }
}

proptest! {
    #[test]
    fn numeric_constant_can_end_with_a_period_punctuation(
        num_const in "[0-9]+[.]",
        stop_char in stop_char_for_num_const(),
    ) {
        let input_text = format!("{}{}", num_const, stop_char);
        assert_eq!(
            scan_first(&input_text),
            (TokenKind::NumericConstant, &*num_const)
        );
    }
}

proptest! {
    #[test]
    fn numeric_constant_can_start_with_a_period_punctuation(
        num_const in "[.][0-9]+",
        stop_char in stop_char_for_num_const(),
    ) {
        let input_text = format!("{}{}", num_const, stop_char);
        assert_eq!(
            scan_first(&input_text),
            (TokenKind::NumericConstant, &*num_const)
        );
    }
}

proptest! {
    #[test]
    fn numeric_constant_cannot_start_with_more_than_one_period_punctuation(
        input_text in r"\.\.+[0-9]",
    ) {
        assert_ne!(scan_first(&input_text).0, TokenKind::NumericConstant);
    }
}

proptest! {
    #[test]
    fn numeric_constant_can_have_decimal_and_binary_exponent(
        num_const in "[0-9]+[eEpP][+-]?[0-9]+",
        stop_char in stop_char_for_num_const(),
    ) {
        let input_text = format!("{}{}", num_const, stop_char);
        assert_eq!(
            scan_first(&input_text),
            (TokenKind::NumericConstant, &*num_const)
        );
    }
}

proptest! {
    #[test]
    fn numeric_constants_should_not_contain_plus_or_minus_if_it_is_not_an_exponent(
        num_const_without_exponent in "[0-9]+[0-9a-dA-Df-oF-Oq-zQ-Z]+",
        incorrect_exponent in "[+-][0-9]+",
        stop_char in stop_char_for_num_const(),
    ) {
        let input_text = format!(
            "{}{}{}",
            num_const_without_exponent, incorrect_exponent, stop_char
        );
        assert_eq!(
            scan_first(&input_text),
            (TokenKind::NumericConstant, &*num_const_without_exponent)
        );
    }
}

proptest! {
    #[test]
    fn numeric_constant_can_have_various_decimal_or_binary_exponents(
        num_const in "[0-9]+([eEpP][+-]?[0-9]+)+",
        stop_char in stop_char_for_num_const(),
    ) {
        let input_text = format!("{}{}", num_const, stop_char);
        assert_eq!(
            scan_first(&input_text),
            (TokenKind::NumericConstant, &*num_const)
        );
    }
}

proptest! {
    #[test]
    fn character_constant_is_wrapped_in_single_quotes(
        c_char_seq in c_char_sequence(),
        stop_char in source_char()
    ) {
        let char_const = format!("'{}'", c_char_seq);
        let input_text = format!("{}{}", char_const, stop_char);

        assert_eq!(
            scan_first(&input_text),
            (TokenKind::CharacterConstant, &*char_const)
        );
    }
}

#[test]
fn character_constant_cannot_be_empty() {
    let mut scanner = Scanner::with_input("''");

    assert_eq!(scanner.scan_next_token(), Err(Diag::EmptyCharacterConstant));
    assert_eq!(scanner.scan_next_token(), Ok(Token::EOF));
}

proptest! {
    #[test]
    fn character_constant_cannot_end_in_newline_or_nul(
        c_char_seq in c_char_sequence()
    ) {
        for newline_or_nul in ['\n', '\r', '\0'] {
            let input_text = format!("'{}{}", c_char_seq, newline_or_nul);
            let mut scanner = Scanner::with_input(&input_text);

            assert_eq!(
                scanner.scan_next_token(),
                Err(Diag::UnterminatedCharacterConstant)
            );
            assert_eq!(scanner.scan_next_token(), Ok(Token::EOF));
        }
    }
}

#[test]
fn character_constant_cannot_abruptly_end_in_newline_or_nul() {
    for newline_or_nul in ['\n', '\r', '\0'] {
        let input_text = format!("'{}", newline_or_nul);
        let mut scanner = Scanner::with_input(&input_text);

        assert_eq!(
            scanner.scan_next_token(),
            Err(Diag::UnterminatedCharacterConstant)
        );
        assert_eq!(scanner.scan_next_token(), Ok(Token::EOF));
    }
}

#[test]
fn character_constant_token_may_contain_a_backslash() {
    assert_eq!(scan_first(r"'\'"), (TokenKind::CharacterConstant, r"'\'"));
}

fn c_char_sequence() -> impl Strategy<Value = String> {
    source_chars_except(&['\'', '\\', '\n', '\r'])
}

proptest! {
    #[test]
    fn scanner_should_diagnose_characters_not_in_source_charset(
        non_source_char in non_source_char()
    ) {
        let mut scanner = Scanner::with_input(&non_source_char);
        let unrec_char = non_source_char.chars().next().unwrap();

        assert_eq!(scanner.scan_next_token(), Err(Diag::UnrecognizedChar(unrec_char)));
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
        let token = self.scanner.scan_next_token().unwrap();

        if token != Token::EOF {
            let lexeme = self.source_file.get_text_snippet(token);
            Some((token.kind, lexeme))
        } else {
            None
        }
    }
}

fn scan_all(input_text: &str) -> Vec<(TokenKind, &str)> {
    TokenKindAndLexemeIter::new(input_text).collect::<Vec<_>>()
}

fn scan_first(input_text: &str) -> (TokenKind, &str) {
    TokenKindAndLexemeIter::new(input_text).next().unwrap()
}

fn is_keyword(lexeme: &str) -> bool {
    matches!(
        lexeme,
        "auto"
            | "break"
            | "case"
            | "char"
            | "const"
            | "continue"
            | "default"
            | "do"
            | "double"
            | "else"
            | "enum"
            | "extern"
            | "float"
            | "for"
            | "goto"
            | "if"
            | "inline"
            | "int"
            | "long"
            | "register"
            | "restrict"
            | "return"
            | "short"
            | "signed"
            | "sizeof"
            | "static"
            | "struct"
            | "switch"
            | "typedef"
            | "union"
            | "unsigned"
            | "void"
            | "volatile"
            | "while"
            | "_Alignas"
            | "_Alignof"
            | "_Atomic"
            | "_Bool"
            | "_Complex"
            | "_Generic"
            | "_Imaginary"
            | "_Noreturn"
            | "_Static_assert"
            | "_Thread_local",
    )
}
