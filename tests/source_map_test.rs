use atlas::source_map::{BytePos, Pos, SourceFile, Span, Spanned};

// TODO: Convert to property based test.
#[test]
fn convert_byte_pos_to_and_from_usize() {
    let byte_pos = <BytePos as Pos>::from_usize(42usize);
    assert_eq!(<BytePos as Pos>::to_usize(byte_pos), 42usize);
}

// TODO: Convert to property based test.
#[test]
fn add_two_byte_poses() {
    let p1 = BytePos::from_usize(2);
    let p2 = BytePos::from_usize(3);

    assert_eq!(p1 + p2, BytePos::from_usize(5));
}

// TODO: Convert to property based test.
#[test]
fn add_assign_two_byte_poses() {
    let mut p1 = BytePos::from_usize(2);
    let p2 = BytePos::from_usize(3);

    p1 += p2;

    assert_eq!(p1, BytePos::from_usize(5));
}

// TODO: Convert to property based test.
#[test]
fn span_from_usizes_should_create_a_span_of_byte_poses() {
    assert_eq!(
        Span::from_usizes(123, 2949),
        Span {
            start: BytePos::from_usize(123),
            end: BytePos::from_usize(2949),
        }
    );

    assert_eq!(
        Span::from_usizes(3, 4),
        Span {
            start: BytePos::from_usize(3),
            end: BytePos::from_usize(4),
        }
    );

    assert_eq!(
        Span::from_usizes(0, 0),
        Span {
            start: BytePos::from_usize(0),
            end: BytePos::from_usize(0),
        }
    );
}

// TODO: Convert to property based test.
#[test]
#[should_panic(expected = "cannot have start greater than end")]
fn span_from_usizes_should_not_allow_start_greater_than_end() {
    let _ = Span::from_usizes(3, 2);
}

#[test]
fn span_from_spanned() {
    struct S;

    let span = Span::from_usizes(2, 7);
    let spanned = Spanned::new(S, span);

    assert_eq!(Span::from(spanned), span);
}

// TODO: Convert to property based test.
#[test]
fn spanned_new_should_create_a_spanned_with_a_value_and_a_span() {
    let spanned = Spanned::new('a', Span::from_usizes(34, 53));

    assert_eq!(spanned.value, 'a');
    assert_eq!(spanned.span, Span::from_usizes(34, 53));

    let spanned = Spanned::new(42, Span::from_usizes(0, 3));

    assert_eq!(spanned.value, 42);
    assert_eq!(spanned.span, Span::from_usizes(0, 3));

    let spanned = Spanned::new("abc", Span::DUMMY);

    assert_eq!(spanned.value, "abc");
    assert_eq!(spanned.span, Span::DUMMY);
}

// TODO: Convert to property based test.
#[test]
fn spanned_with_dummy_span_should_create_a_spanned_with_a_value_and_dummy_span() {
    let spanned = Spanned::with_dummy_span('a');

    assert_eq!(spanned.value, 'a');
    assert_eq!(spanned.span, Span::DUMMY);

    let spanned = Spanned::with_dummy_span(42);

    assert_eq!(spanned.value, 42);
    assert_eq!(spanned.span, Span::DUMMY);

    let spanned = Spanned::with_dummy_span("abc");

    assert_eq!(spanned.value, "abc");
    assert_eq!(spanned.span, Span::DUMMY);
}

// TODO: Convert to property based test.
#[test]
fn spanned_should_deref_to_its_internal_value() {
    #[derive(PartialEq, Debug)]
    struct S {
        i: i32,
    }

    let spanned = Spanned::new(S { i: 42 }, Span::from_usizes(34, 53));

    assert_eq!(*spanned, S { i: 42 });
    assert_eq!(spanned.i, 42);
}

// TODO: Convert to property based test.
#[test]
fn usize_should_be_convertible_to_byte_pos() {
    let bp: BytePos = 42usize.into();

    assert_eq!(bp, BytePos::from_usize(42));
}

// TODO: Convert to property based test.
#[test]
fn get_text_snippet_should_return_empty_string_for_dummy_span() {
    let sc = SourceFile::new("hello world!\n");

    assert_eq!(sc.get_text_snippet(Span::DUMMY), "");
}

// TODO: Convert to property based test.
#[test]
fn get_text_snippet_should_return_empty_string_for_empty_span() {
    let sc = SourceFile::new("hello world!\n");

    assert_eq!(sc.get_text_snippet(Span::from_usizes(2, 2)), "");
    assert_eq!(sc.get_text_snippet(Span::from_usizes(3, 3)), "");
    assert_eq!(sc.get_text_snippet(Span::from_usizes(5, 5)), "");
}

// TODO: Convert to property based test.
#[test]
#[should_panic(expected = "range end index 14 out of range for slice of length 13")]
fn get_text_snippet_should_panic_on_out_of_bounds_spans() {
    let sc = SourceFile::new("hello world!\n");

    let _ = sc.get_text_snippet(Span::from_usizes(14, 14));
}

// TODO: Should convert to property based test?
#[test]
fn get_text_snippet_should_return_substring_for_the_exclusive_range_of_a_non_dummy_span() {
    let sc = SourceFile::new("hello world!\n");

    assert_eq!(
        sc.get_text_snippet(Span::from_usizes(0, 13)),
        "hello world!\n"
    );
    assert_eq!(sc.get_text_snippet(Span::from_usizes(0, 1)), "h");
    assert_eq!(sc.get_text_snippet(Span::from_usizes(1, 2)), "e");
    assert_eq!(sc.get_text_snippet(Span::from_usizes(0, 5)), "hello");
    assert_eq!(sc.get_text_snippet(Span::from_usizes(6, 11)), "world");
}

#[test]
fn get_text_snippet_should_work_with_utf8_text() {
    let sc = SourceFile::new("🗻∈🌏");

    assert_eq!(sc.get_text_snippet(Span::from_usizes(0, 4)), "🗻");
    assert_eq!(sc.get_text_snippet(Span::from_usizes(4, 7)), "∈");
    assert_eq!(sc.get_text_snippet(Span::from_usizes(7, 11)), "🌏");
}

#[test]
#[should_panic(expected = "span is an invalid UTF-8 sequence")]
fn get_text_snippet_should_panic_if_span_is_broken_utf8() {
    let sc = SourceFile::new("\u{0800}");
    let _ = sc.get_text_snippet(Span::from_usizes(0, 1));
}
