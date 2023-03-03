#[derive(PartialEq, Eq, Debug)]
pub(crate) struct NumericConstant {
    pub(crate) value: NumConstVal,
    pub(crate) has_overflowed: bool,
}

#[derive(PartialEq, Eq, Debug)]
pub(crate) enum NumConstVal {
    // TODO: Labels for suffixes.
    Int(u64),
}

pub(crate) fn parse_numeric_constant(input: &str) -> NumericConstant {
    if input == "0" {
        return NumericConstant {
            value: NumConstVal::Int(0),
            has_overflowed: false,
        };
    }

    let (input, radix) = if input.starts_with("0x") || input.starts_with("0X") {
        (&input[2..], 16)
    } else if let Some(stripped) = input.strip_prefix('0') {
        (stripped, 8)
    } else {
        (input, 10)
    };

    let mut value = 0u64;
    let mut has_overflowed = false;

    for &digit in input.as_bytes() {
        let (low_mul, overflowed) = value.overflowing_mul(radix);
        has_overflowed |= overflowed;
        value = low_mul;

        let (low_add, overflowed) = value.overflowing_add(convert_digit_to_int(digit));
        has_overflowed |= overflowed;
        value = low_add;
    }

    NumericConstant {
        value: NumConstVal::Int(value),
        has_overflowed,
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
