use crate::ast::{
    BuiltinTypeKind, Decl, FuncDecl, IntegerLiteral, Param, TranslationUnit, Type, VarDecl,
};
use crate::constant_parser::{parse_numeric_constant, NumConst};
use crate::diagnostics::Diag;
use crate::scanner::{Bracket, Scanner, Token, TokenKind};
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

        if tok == Token::eof() {
            Err(vec![Diag::EmptyTranslationUnit])
        } else {
            let mut external_decls = vec![];
            let mut diagnostics = vec![];

            match self.parse_decl(*tok) {
                Ok(decl) => external_decls.push(decl),
                Err(decl_diags) => diagnostics.extend(decl_diags),
            }

            if diagnostics.is_empty() {
                let translation_unit = TranslationUnit { external_decls };

                Ok(translation_unit)
            } else {
                Err(diagnostics)
            }
        }
    }

    fn parse_decl(&mut self, type_spec_tok: Token) -> Result<Decl, Vec<Diag>> {
        debug_assert!(
            type_spec_tok.kind == TokenKind::KwInt || type_spec_tok.kind == TokenKind::KwLong
        );
        let type_spec = map_token_kind_to_type(type_spec_tok.kind);

        let ident_tok = self.consume_tok().unwrap();
        debug_assert!(ident_tok.kind == TokenKind::Identifier);

        let lexeme = self.source_file.get_text_snippet(ident_tok);
        let next_tok = self.consume_tok().unwrap();

        if next_tok.kind == TokenKind::Open(Bracket::Round) {
            let mut parameters = vec![];
            let mut decl_diags = vec![];

            loop {
                let tok = self.consume_tok().unwrap();

                match tok.kind {
                    TokenKind::Closed(Bracket::Round) => break,
                    TokenKind::Semicolon => {
                        decl_diags.push(Diag::MissingClosingParen);
                        break;
                    }
                    TokenKind::KwInt => {
                        let identifier = if self.peek_tok().unwrap().kind == TokenKind::Identifier {
                            let ident_tok = self.consume_tok().unwrap();
                            let lexeme = self.source_file.get_text_snippet(ident_tok);

                            Some(lexeme.to_owned())
                        } else {
                            None
                        };

                        parameters.push(Param {
                            type_specifier: Type::BuiltinType(BuiltinTypeKind::Int),
                            identifier,
                        });
                    }
                    unknown_kind => {
                        decl_diags.push(Diag::ExpectedButGot {
                            expected: TokenKind::KwInt,
                            got: unknown_kind,
                        });
                        continue;
                    }
                }

                let next_tok = self.peek_tok().unwrap();

                if next_tok.kind != TokenKind::Closed(Bracket::Round) {
                    if next_tok.kind == TokenKind::Comma {
                        self.consume_tok().unwrap();
                    } else {
                        decl_diags.push(Diag::ExpectedButGot {
                            expected: TokenKind::Comma,
                            got: next_tok.kind,
                        });
                    }
                }
            }

            if decl_diags.is_empty() {
                Ok(Decl::Func(FuncDecl {
                    ret_type_specifier: type_spec,
                    identifier: lexeme.to_owned(),
                    parameters,
                }))
            } else {
                Err(decl_diags)
            }
        } else {
            let initializer = if next_tok.kind == TokenKind::Equal {
                Some(self.parse_initializer())
            } else {
                None
            };

            Ok(Decl::Var(VarDecl {
                type_specifier: type_spec,
                identifier: lexeme.to_owned(),
                initializer,
            }))
        }
    }

    fn parse_initializer(&mut self) -> IntegerLiteral {
        let init_tok = self.consume_tok().unwrap();
        debug_assert!(init_tok.kind == TokenKind::NumericConstant);

        let parse_result = parse_numeric_constant(self.source_file.get_text_snippet(init_tok));
        debug_assert!(!parse_result.has_overflowed);

        match parse_result.num_const {
            NumConst::Int(int_const) => {
                debug_assert!(!int_const.is_unsigned);

                IntegerLiteral {
                    value: int_const.value,
                    ty: Type::BuiltinType(BuiltinTypeKind::Int),
                }
            }
        }
    }

    fn consume_tok(&mut self) -> Result<Spanned<Token>, Diag> {
        // Guarantees that `self.cached_peek_tok` has a peeked token.
        self.peek_tok()?;
        debug_assert!(self.cached_peek_tok.is_some());

        let peeked_tok = self.cached_peek_tok.unwrap();

        if peeked_tok != Token::eof() {
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

fn map_token_kind_to_type(tok_kind: TokenKind) -> Type {
    match tok_kind {
        TokenKind::KwInt => Type::BuiltinType(BuiltinTypeKind::Int),
        TokenKind::KwLong => Type::BuiltinType(BuiltinTypeKind::Long),
        _ => unimplemented!("missing impl of other types"),
    }
}
