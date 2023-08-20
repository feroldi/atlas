#![cfg(test)]

use proptest::prelude::*;

use crate::ast::{BuiltinTypeKind, ExternalDecl, IntegerLiteral, TranslationUnit, Type, VarDecl};
use crate::diagnostics::Diag;
use crate::parser::Parser;
use crate::tests::*;

#[test]
fn parse_empty_translation_unit_should_issue_an_error() {
    let result = parse("");

    assert_eq!(result, Err(vec![Diag::EmptyTranslationUnit]));
}

proptest! {
    #[test]
    fn parse_basic_top_level_variable_declaration(
        type_spec in "int|long",
        ident_name in identifier(),
    ) {
        let result = parse(
            &format!(
                r#"
                {type_spec} {ident_name};
                "#,
                type_spec=type_spec,
                ident_name=ident_name,
            )
        );

        let expected_type_kind = match type_spec.as_ref() {
            "int" => BuiltinTypeKind::Int,
            "long" => BuiltinTypeKind::Long,
            _ => panic!("forgot to cover extra types"),
        };

        let var_decl = VarDecl {
            type_specifier: Type::BuiltinType(expected_type_kind),
            identifier: ident_name,
            initializer: None,
        };

        let expected = TranslationUnit {
            external_decls: vec![ExternalDecl::VarDecl(var_decl)],
        };

        assert_eq!(result, Ok(expected));
    }
}

proptest! {
#[test]
    fn parse_basic_top_level_variable_declaration_with_initializer(
        type_spec in "int|long",
        ident_name in identifier(),
    ) {
        let result = parse(
            &format!(
                r#"
                {type_spec} {ident_name} = 42;
                "#,
                type_spec=type_spec,
                ident_name=ident_name,
            )
        );

        let expected_type_kind = match type_spec.as_ref() {
            "int" => BuiltinTypeKind::Int,
            "long" => BuiltinTypeKind::Long,
            _ => panic!("forgot to cover extra types"),
        };

        let initializer = IntegerLiteral {
            value: 42,
            ty: Type::BuiltinType(BuiltinTypeKind::Int),
        };

        let var_decl = VarDecl {
            type_specifier: Type::BuiltinType(expected_type_kind),
            identifier: ident_name,
            initializer: Some(initializer),
        };

        let expected = TranslationUnit {
            external_decls: vec![ExternalDecl::VarDecl(var_decl)],
        };

        assert_eq!(result, Ok(expected));
    }
}

fn parse(source_text: &str) -> Result<TranslationUnit, Vec<Diag>> {
    use crate::scanner::Scanner;
    use crate::source_map::SourceFile;

    let scanner = Scanner::with_input(source_text);
    let source_file = SourceFile::new(source_text);

    let mut parser = Parser::new(scanner, source_file);

    parser.parse_translation_unit()
}
