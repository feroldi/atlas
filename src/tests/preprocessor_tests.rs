#![cfg(test)]

use crate::preprocessor::Preprocessor;

#[test]
fn pp_should_return_text_as_is_if_no_directive_is_found() {
    let source_text = r#"
        int main(void) {
            return 0;
        }
    "#
    .to_owned();

    let pp = Preprocessor::with_text(source_text.clone());

    assert_eq!(pp.parse(), source_text);
}
