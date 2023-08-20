use crate::scanner::TokenKind;

#[derive(PartialEq, Debug, Copy, Clone)]
pub(crate) enum Diag {
    UnrecognizedChar(char),
    EmptyCharacterConstant,
    UnterminatedCharacterConstant,
    UnterminatedStringLiteral,
    UnterminatedBlockComment,
    EmptyTranslationUnit,
    ExpectedButGot { expected: TokenKind, got: TokenKind },
    MissingClosingParen,
}
