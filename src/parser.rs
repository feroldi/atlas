use crate::ast::{BuiltinTypeKind, ExternalDecl, IntegerLiteral, TranslationUnit, Type, VarDecl};
use crate::constant_parser::{parse_numeric_constant, NumConst};
use crate::diagnostics::Diag;
use crate::scanner::{Scanner, Token, TokenKind};
use crate::source_map::{SourceFile, Spanned};

pub(crate) struct Parser<'src> {
    scanner: Scanner<'src>,
    source_file: SourceFile<'src>,
    cached_peek_tok: Option<Spanned<Token>>,
}

impl<'src> Parser<'src> {
    pub(crate) fn new(scanner: Scanner<'src>, source_file: SourceFile<'src>) -> Parser<'src> {
        Parser {
            scanner,
            source_file,
            cached_peek_tok: None,
        }
    }

    pub(crate) fn parse_translation_unit(&mut self) -> Result<TranslationUnit, Vec<Diag>> {
        let tok = self.consume_tok().unwrap();

        if tok == Token::EOF {
            Err(vec![Diag::EmptyTranslationUnit])
        } else {
            debug_assert!(tok.kind == TokenKind::KwInt);

            let ident_tok = self.consume_tok().unwrap();
            debug_assert!(ident_tok.kind == TokenKind::Identifier);

            let initializer = if self.peek_tok().unwrap().kind == TokenKind::Equal {
                self.consume_tok().unwrap();

                let init_tok = self.consume_tok().unwrap();
                debug_assert!(init_tok.kind == TokenKind::NumericConstant);

                let parse_result =
                    parse_numeric_constant(self.source_file.get_text_snippet(init_tok));
                debug_assert!(!parse_result.has_overflowed);

                match parse_result.num_const {
                    NumConst::Int(int_const) => {
                        debug_assert!(!int_const.is_unsigned);

                        Some(IntegerLiteral {
                            value: int_const.value,
                            ty: Type::BuiltinType(BuiltinTypeKind::Int),
                        })
                    }
                }
            } else {
                None
            };

            let lexeme = self.source_file.get_text_snippet(ident_tok);

            let var_decl = VarDecl {
                type_specifier: Type::BuiltinType(BuiltinTypeKind::Int),
                identifier: lexeme.to_owned(),
                initializer,
            };

            let translation_unit = TranslationUnit {
                external_decls: vec![ExternalDecl::VarDecl(var_decl)],
            };

            Ok(translation_unit)
        }
    }

    fn consume_tok(&mut self) -> Result<Spanned<Token>, Diag> {
        // Guarantees that `self.cached_peek_tok` has a peeked token.
        self.peek_tok()?;
        debug_assert!(self.cached_peek_tok.is_some());

        let peeked_tok = self.cached_peek_tok.unwrap();

        if peeked_tok != Token::EOF {
            self.cached_peek_tok = None;
        }

        Ok(peeked_tok)
    }

    fn peek_tok(&mut self) -> Result<Spanned<Token>, Diag> {
        match self.cached_peek_tok {
            Some(peeked_tok) => Ok(peeked_tok),
            _ => {
                let peeked_tok = self.scanner.scan_next_token()?;
                self.cached_peek_tok = Some(peeked_tok);

                Ok(peeked_tok)
            }
        }
    }
}
