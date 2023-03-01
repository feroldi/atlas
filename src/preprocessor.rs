// TODO: Expand the whole text, including inner includes, and construct a
// Lisp-like list of chunks so that we can correctly compute the byte-pos of
// tokens when reporting diagnostics. Also, keep both touched and untouched
// versions of the source text, so that we can report the original code lines.
//
// CharStream should take care of eating the preprocessor's output so that the
// scanner doesn't have to care about it.

pub struct Preprocessor {
    text: String,
}

impl Preprocessor {
    pub fn with_text(text: String) -> Preprocessor {
        Preprocessor { text }
    }

    pub fn parse(&self) -> String {
        self.text.clone()
    }
}
