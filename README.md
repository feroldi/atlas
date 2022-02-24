# Atlas

[![build](https://github.com/feroldi/atlas/actions/workflows/rust.yml/badge.svg)](https://github.com/feroldi/atlas/actions/workflows/rust.yml)
[![coverage](https://feroldi.github.io/atlas-coverage/badges/flat.svg)](https://feroldi.github.io/atlas-coverage)

This is an experimental project of a C compiler written in Rust. More
specifically, the implementation tries to follow the ISO/IEC 9899:2011
standard (i.e., C11) to a certain extent. It's not a production compiler,
and probably will never be. The main purpose of this project is to
teach myself compiler data structures, language design and optimization
techniques.

## Building

Before building, make sure that all tests pass. Doing so guarentees a well working compiler.

```bash
$ cargo test
```

Once tests are all green, build Atlas like so:

```bash
$ cargo build
```

## FAQ

### Why choose to write a compiler for C11?

C11 is a great, challenging language to make a compiler for. It's also
true that one can learn a lot by writing a compiler. That being so,
C11 seems to be an option that gets the most out of the experience.
Just imagine being able to compile whole big projects out there :)

## License

This project is licensed under the MIT license. See LICENSE.
