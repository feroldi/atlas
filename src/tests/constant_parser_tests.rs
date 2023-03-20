#![cfg(test)]

use proptest::prelude::*;

use crate::constant_parser::{parse_numeric_constant, IntegerConst, NumConst, ParseResult};

#[test]
fn single_digits_should_parse_into_integer_constant() {
    let inputs = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"];

    for input in inputs {
        let integer = IntegerConst {
            value: input.parse().unwrap(),
            is_unsigned: false,
        };

        let expected = ParseResult {
            num_const: NumConst::Int(integer),
            has_overflowed: false,
        };

        assert_eq!(parse_numeric_constant(input), expected);
    }
}

proptest! {
    #[test]
    fn constants_that_start_with_something_other_than_zero_should_parse_as_decimal_integer_constants(
        input in "[1-9][0-9]{1,18}"
    ) {
        // Using at most 19 digits for this test, as that is the maximum number of digits that
        // guarantees it will never overflow 64 bits.

        let integer = IntegerConst {
            value: input.parse().unwrap(),
            is_unsigned: false,
        };

        let expected = ParseResult {
            num_const: NumConst::Int(integer),
            has_overflowed: false,
        };

        assert_eq!(parse_numeric_constant(&input), expected);
    }
}

proptest! {
    #[test]
    fn decimal_intenger_constants_that_go_over_64_bits_should_be_flagged_as_overflowed(
        input in "[1-9][0-9]{20,37}"
    ) {
        let truncated_value = input.parse::<u128>().unwrap() as u64;

        let integer = IntegerConst {
            value: truncated_value,
            is_unsigned: false,
        };

        let expected = ParseResult {
            num_const: NumConst::Int(integer),
            has_overflowed: true,
        };

        assert_eq!(parse_numeric_constant(&input), expected);
    }
}

#[test]
fn decimal_intenger_constant_that_goes_a_bit_over_64_bits_should_be_flagged_as_overflowed() {
    let valid_input = "18446744073709551615";

    assert_eq!(
        parse_numeric_constant(valid_input),
        ParseResult {
            num_const: NumConst::Int(IntegerConst {
                value: u64::MAX,
                is_unsigned: false
            }),
            has_overflowed: false,
        }
    );

    let invalid_input = "18446744073709551616";

    assert_eq!(
        parse_numeric_constant(invalid_input),
        ParseResult {
            num_const: NumConst::Int(IntegerConst {
                value: 0,
                is_unsigned: false
            }),
            has_overflowed: true,
        }
    );
}

proptest! {
    #[test]
    fn constants_that_start_with_zero_should_parse_as_octal_integer_constants(
        input in "0[1-7]{1,21}"
    ) {
        // Using at most 21 digits for this test, as that is the maximum number of digits that
        // guarantees it will never overflow 64 bits.

        let raw_value = u64::from_str_radix(&input[1..], 8).unwrap();

        let integer = IntegerConst {
            value: raw_value,
            is_unsigned: false,
        };

        let expected = ParseResult {
            num_const: NumConst::Int(integer),
            has_overflowed: false,
        };

        assert_eq!(parse_numeric_constant(&input), expected);
    }
}

proptest! {
    #[test]
    fn octal_intenger_constants_that_go_over_64_bits_should_be_flagged_as_overflowed(
        input in "0[1-7]{23,42}"
    ) {
        let truncated_value = u128::from_str_radix(&input[1..], 8).unwrap() as u64;

        let integer = IntegerConst {
            value: truncated_value,
            is_unsigned: false,
        };


        let expected = ParseResult {
            num_const: NumConst::Int(integer),
            has_overflowed: true,
        };

        assert_eq!(parse_numeric_constant(&input), expected);
    }
}

#[test]
fn octal_intenger_constant_that_goes_a_bit_over_64_bits_should_be_flagged_as_overflowed() {
    let valid_input = "01777777777777777777777";

    assert_eq!(
        parse_numeric_constant(valid_input),
        ParseResult {
            num_const: NumConst::Int(IntegerConst {
                value: u64::MAX,
                is_unsigned: false
            }),
            has_overflowed: false,
        }
    );

    let invalid_input = "02000000000000000000000";

    assert_eq!(
        parse_numeric_constant(invalid_input),
        ParseResult {
            num_const: NumConst::Int(IntegerConst {
                value: 0,
                is_unsigned: false
            }),
            has_overflowed: true,
        }
    );
}

proptest! {
    #[test]
    fn constants_that_start_with_zero_and_x_should_parse_as_hexadecimal_integer_constants(
        input in "0[xX][1-9a-fA-F]{1,16}"
    ) {
        // Using at most 16 digits for this test, as that is the maximum number of digits that
        // guarantees it will never overflow 64 bits.

        let raw_value = u64::from_str_radix(&input[2..], 16).unwrap();

        let integer = IntegerConst {
            value: raw_value,
            is_unsigned: false,
        };

        let expected = ParseResult {
            num_const: NumConst::Int(integer),
            has_overflowed: false,
        };

        assert_eq!(parse_numeric_constant(&input), expected);
    }
}

proptest! {
    #[test]
    fn hexadecimal_intenger_constants_that_go_over_64_bits_should_be_flagged_as_overflowed(
        input in "0[xX][1-9a-fA-F]{17,32}"
    ) {
        let truncated_value = u128::from_str_radix(&input[2..], 16).unwrap() as u64;

        let integer = IntegerConst {
            value: truncated_value,
            is_unsigned: false,
        };

        let expected = ParseResult {
            num_const: NumConst::Int(integer),
            has_overflowed: true,
        };

        assert_eq!(parse_numeric_constant(&input), expected);
    }
}

#[test]
fn hexadecimal_intenger_constant_that_goes_a_bit_over_64_bits_should_be_flagged_as_overflowed() {
    let valid_input = "0xffffffffffffffff";

    assert_eq!(
        parse_numeric_constant(valid_input),
        ParseResult {
            num_const: NumConst::Int(IntegerConst {
                value: u64::MAX,
                is_unsigned: false
            }),
            has_overflowed: false,
        }
    );

    let invalid_input = "0x10000000000000000";

    assert_eq!(
        parse_numeric_constant(invalid_input),
        ParseResult {
            num_const: NumConst::Int(IntegerConst {
                value: 0,
                is_unsigned: false
            }),
            has_overflowed: true,
        }
    );
}
