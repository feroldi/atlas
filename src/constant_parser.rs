use std::iter::Peekable;

pub(crate) fn parse_numeric_constant(token_lexeme: &str) -> ParseResult {
    let attrs = parse_num_const_attributes(token_lexeme);
    let (value, has_overflowed) = eval_integer_constant(attrs);

    ParseResult {
        num_const: NumConst::Int(value),
        has_overflowed,
    }
}

#[derive(PartialEq, Eq, Debug)]
pub(crate) struct ParseResult {
    pub(crate) num_const: NumConst,
    pub(crate) has_overflowed: bool,
}

#[derive(PartialEq, Eq, Debug)]
pub(crate) enum NumConst {
    Int(IntConst),
}


#[derive(PartialEq, Eq, Debug)]
pub(crate) enum IntConst {
    // TODO: Labels for suffixes.
    value: u64,
}


fn parse_num_const_attributes(token_lexeme: &str) -> NumConstAttrs {
    let mut seq = Seq::new(token_lexeme);

    let radix;
    let digits_start;
    let digits_end;

    if seq.peek() != b'0' {
        radix = 10;

        while seq.peek().is_ascii_digit() {
            seq.bump();
        }

        (digits_start, digits_end) = (0, seq.pos);
        // TODO: parse suffix.
    } else if token_lexeme.len() == 1 {
        radix = 10;
        (digits_start, digits_end) = (0, 1);
    } else {
        seq.bump();

        if matches!(seq.peek(), b'x' | b'X') {
            seq.bump();

            radix = 16;
            digits_start = 2;

            while seq.peek().is_ascii_hexdigit() {
                seq.bump();
            }
        } else {
            radix = 8;
            digits_start = 1;

            while seq.peek().is_ascii_octdigit() {
                seq.bump();
            }
        }

        digits_end = seq.pos;
    }

    NumConstAttrs {
        radix,
        digits: &token_lexeme.as_bytes()[digits_start..digits_end],
    }
}

fn eval_integer_constant(attrs: NumConstAttrs) -> (u64, bool) {
    let mut value = 0u64;
    let mut has_overflowed = false;

    for &digit in attrs.digits {
        let (low_mul, overflowed) = value.overflowing_mul(attrs.radix);
        has_overflowed |= overflowed;
        value = low_mul;

        let (low_add, overflowed) = value.overflowing_add(convert_digit_to_int(digit));
        has_overflowed |= overflowed;
        value = low_add;
    }

    (value, has_overflowed)
}

struct NumConstAttrs<'a> {
    radix: u64,
    digits: &'a [u8],
}

struct Seq<'a> {
    iter: Peekable<std::slice::Iter<'a, u8>>,
    pos: usize,
}

impl Seq<'_> {
    fn new(token_lexeme: &str) -> Seq {
        Seq {
            iter: token_lexeme.as_bytes().iter().peekable(),
            pos: 0,
        }
    }

    fn peek(&mut self) -> u8 {
        self.iter.peek().map(|&&c| c).unwrap_or(b'\0')
    }

    fn bump(&mut self) -> u8 {
        match self.iter.next() {
            Some(&ch) => {
                self.pos += 1;
                ch
            }
            None => b'\0',
        }
    }
}

fn convert_digit_to_int(digit: u8) -> u64 {
    match digit {
        b'0'..=b'9' => (digit - b'0') as u64,
        b'a'..=b'f' => (digit - b'a') as u64 + 10,
        b'A'..=b'F' => (digit - b'A') as u64 + 10,
        _ => unreachable!("should not convert '{}'", digit),
    }
}
