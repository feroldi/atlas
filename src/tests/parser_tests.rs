#![cfg(test)]

use crate::ast::{BuiltinTypeKind, ExternalDecl, IntegerLiteral, TranslationUnit, Type, VarDecl};
use crate::diagnostics::Diag;
use crate::parser::Parser;

#[test]
fn parse_empty_translation_unit_should_issue_an_error() {
    let result = parse("");

    assert_eq!(result, Err(vec![Diag::EmptyTranslationUnit]));
}

#[test]
fn parse_basic_top_level_variable_declaration() {
    let result = parse(
        r#"
        int x;
        "#,
    );

    let var_decl = VarDecl {
        type_specifier: Type::BuiltinType(BuiltinTypeKind::Int),
        identifier: "x".to_owned(),
        initializer: None,
    };

    let expected = TranslationUnit {
        external_decls: vec![ExternalDecl::VarDecl(var_decl)],
    };

    assert_eq!(result, Ok(expected));
}

#[test]
fn parse_basic_top_level_variable_declaration_with_initializer() {
    let result = parse(
        r#"
        int x = 42;
        "#,
    );

    let initializer = IntegerLiteral {
        value: 42,
        ty: Type::BuiltinType(BuiltinTypeKind::Int),
    };

    let var_decl = VarDecl {
        type_specifier: Type::BuiltinType(BuiltinTypeKind::Int),
        identifier: "x".to_owned(),
        initializer: Some(initializer),
    };

    let expected = TranslationUnit {
        external_decls: vec![ExternalDecl::VarDecl(var_decl)],
    };

    assert_eq!(result, Ok(expected));
}

fn parse(source_text: &str) -> Result<TranslationUnit, Vec<Diag>> {
    use crate::scanner::Scanner;
    use crate::source_map::SourceFile;

    let scanner = Scanner::with_input(source_text);
    let source_file = SourceFile::new(source_text);

    let mut parser = Parser::new(scanner, source_file);

    parser.parse_translation_unit()
}
