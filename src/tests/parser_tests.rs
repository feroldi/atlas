#![cfg(test)]

use proptest::prelude::*;

use crate::ast::{
    BuiltinTypeKind, Decl, FuncDecl, IntegerLiteral, Param, TranslationUnit, Type, VarDecl,
};
use crate::diagnostics::Diag;
use crate::parser::Parser;
use crate::scanner::TokenKind;
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
            external_decls: vec![Decl::Var(var_decl)],
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
            external_decls: vec![Decl::Var(var_decl)],
        };

        assert_eq!(result, Ok(expected));
    }
}

#[test]
fn parse_basic_top_level_function_declaration() {
    let result = parse(
        r#"
        int foo();
        "#,
    );

    let func_decl = FuncDecl {
        ret_type_specifier: Type::BuiltinType(BuiltinTypeKind::Int),
        identifier: "foo".to_owned(),
        parameters: vec![],
    };

    let expected = TranslationUnit {
        external_decls: vec![Decl::Func(func_decl)],
    };

    assert_eq!(result, Ok(expected));
}

#[test]
fn parse_basic_top_level_function_declaration_with_unnamed_parameter() {
    let result = parse(
        r#"
        int foo(int);
        "#,
    );

    let func_decl = FuncDecl {
        ret_type_specifier: Type::BuiltinType(BuiltinTypeKind::Int),
        identifier: "foo".to_owned(),
        parameters: vec![Param {
            type_specifier: Type::BuiltinType(BuiltinTypeKind::Int),
            identifier: None,
        }],
    };

    let expected = TranslationUnit {
        external_decls: vec![Decl::Func(func_decl)],
    };

    assert_eq!(result, Ok(expected));
}

#[test]
fn parse_basic_top_level_function_declaration_with_named_parameter() {
    let result = parse(
        r#"
        int foo(int bar);
        "#,
    );

    let func_decl = FuncDecl {
        ret_type_specifier: Type::BuiltinType(BuiltinTypeKind::Int),
        identifier: "foo".to_owned(),
        parameters: vec![Param {
            type_specifier: Type::BuiltinType(BuiltinTypeKind::Int),
            identifier: Some("bar".to_owned()),
        }],
    };

    let expected = TranslationUnit {
        external_decls: vec![Decl::Func(func_decl)],
    };

    assert_eq!(result, Ok(expected));
}

#[test]
fn parse_basic_top_level_function_declaration_with_many_unnamed_parameters() {
    let result = parse(
        r#"
        int foo(int, int, int);
        "#,
    );

    let func_decl = FuncDecl {
        ret_type_specifier: Type::BuiltinType(BuiltinTypeKind::Int),
        identifier: "foo".to_owned(),
        parameters: vec![
            Param {
                type_specifier: Type::BuiltinType(BuiltinTypeKind::Int),
                identifier: None,
            },
            Param {
                type_specifier: Type::BuiltinType(BuiltinTypeKind::Int),
                identifier: None,
            },
            Param {
                type_specifier: Type::BuiltinType(BuiltinTypeKind::Int),
                identifier: None,
            },
        ],
    };

    let expected = TranslationUnit {
        external_decls: vec![Decl::Func(func_decl)],
    };

    assert_eq!(result, Ok(expected));
}

#[test]
fn parse_basic_top_level_function_declaration_with_many_named_parameters() {
    let result = parse(
        r#"
        int foo(int x, int y, int z);
        "#,
    );

    let func_decl = FuncDecl {
        ret_type_specifier: Type::BuiltinType(BuiltinTypeKind::Int),
        identifier: "foo".to_owned(),
        parameters: vec![
            Param {
                type_specifier: Type::BuiltinType(BuiltinTypeKind::Int),
                identifier: Some("x".to_owned()),
            },
            Param {
                type_specifier: Type::BuiltinType(BuiltinTypeKind::Int),
                identifier: Some("y".to_owned()),
            },
            Param {
                type_specifier: Type::BuiltinType(BuiltinTypeKind::Int),
                identifier: Some("z".to_owned()),
            },
        ],
    };

    let expected = TranslationUnit {
        external_decls: vec![Decl::Func(func_decl)],
    };

    assert_eq!(result, Ok(expected));
}

#[test]
fn parse_error_basic_top_level_function_declaration_with_many_parameters_missing_comma() {
    let result = parse(
        r#"
        int foo(int a int int);
        "#,
    );

    let expected = vec![
        Diag::ExpectedButGot {
            expected: TokenKind::Comma,
            got: TokenKind::KwInt,
        },
        Diag::ExpectedButGot {
            expected: TokenKind::Comma,
            got: TokenKind::KwInt,
        },
    ];

    assert_eq!(result, Err(expected));
}

#[test]
fn parse_error_basic_top_level_function_declaration_missing_closing_paren() {
    let result = parse(
        r#"
        int foo(;
        "#,
    );

    let expected = vec![Diag::MissingClosingParen];

    assert_eq!(result, Err(expected));
}

#[test]
fn parse_error_basic_top_level_function_declaration_unexpected_token() {
    let result = parse(
        r#"
        int foo(+);
        "#,
    );

    let expected = vec![Diag::ExpectedButGot {
        expected: TokenKind::KwInt,
        got: TokenKind::Plus,
    }];

    assert_eq!(result, Err(expected));
}

#[test]
fn parse_error_basic_top_level_function_declaration_unexpected_token_and_missing_closing_paren() {
    let result = parse(
        r#"
        int foo(+;
        "#,
    );

    let expected = vec![
        Diag::ExpectedButGot {
            expected: TokenKind::KwInt,
            got: TokenKind::Plus,
        },
        Diag::MissingClosingParen,
    ];

    assert_eq!(result, Err(expected));
}

fn parse(source_text: &str) -> Result<TranslationUnit, Vec<Diag>> {
    use crate::scanner::Scanner;
    use crate::source_map::SourceFile;

    let scanner = Scanner::with_input(source_text);
    let source_file = SourceFile::new(source_text);

    let mut parser = Parser::new(scanner, source_file);

    parser.parse_translation_unit()
}
