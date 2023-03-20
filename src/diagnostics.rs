#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub(crate) enum Diag {
    UnrecognizedChar(char),
    EmptyCharacterConstant,
    UnterminatedCharacterConstant,
    UnterminatedStringLiteral,
    UnterminatedBlockComment,
    EmptyTranslationUnit,
}
