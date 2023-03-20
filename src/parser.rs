use crate::ast::{BuiltinTypeKind, ExternalDecl, TranslationUnit, Type, VarDecl};
use crate::diagnostics::Diag;
use crate::scanner::{Scanner, Token};
use crate::source_map::{SourceFile, Spanned};

pub(crate) struct Parser<'src> {
    scanner: Scanner<'src>,
    source_file: SourceFile<'src>,
}

impl<'src> Parser<'src> {
    pub(crate) fn new(scanner: Scanner<'src>, source_file: SourceFile<'src>) -> Parser<'src> {
        Parser {
            scanner,
            source_file,
        }
    }

    pub(crate) fn parse_translation_unit(&mut self) -> Result<TranslationUnit, Vec<Diag>> {
        let tok = self.consume_tok().unwrap();

        if tok == Token::EOF {
            Err(vec![Diag::EmptyTranslationUnit])
        } else {
            let ident_tok = self.consume_tok().unwrap();
            let lexeme = self.source_file.get_text_snippet(ident_tok);

            let var_decl = VarDecl {
                type_specifier: Type::BuiltinType(BuiltinTypeKind::Int),
                identifier: lexeme.to_owned(),
            };

            let translation_unit = TranslationUnit {
                external_decls: vec![ExternalDecl::VarDecl(var_decl)],
            };

            Ok(translation_unit)
        }
    }

    fn consume_tok(&mut self) -> Result<Spanned<Token>, Diag> {
        self.scanner.scan_next_token()
    }
}
