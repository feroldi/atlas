# Atlas

[![build](https://github.com/feroldi/atlas/actions/workflows/build.yml/badge.svg)](https://github.com/feroldi/atlas/actions/workflows/build.yml)
[![coverage](https://feroldi.github.io/atlas-coverage/badges/flat.svg)](https://feroldi.github.io/atlas-coverage)

This is an experimental project of a C compiler written in Rust. More
specifically, the implementation tries to follow the ISO/IEC 9899:2018
standard (i.e., C17) to a certain extent. It's not a production compiler,
and probably will never be. The main purpose of this project is to
teach myself compiler data structures, language design and optimization
techniques.

## Building

In order to build this project, you need to install [`cargo`](https://github.com/rust-lang/cargo).

Once `cargo` is installed, make sure that all tests are passing.
Doing so guarentees a well working compiler.
You can do that by running the following command:

```bash
$ cargo test
```

If all tests pass, then you're good to go.
Build the compiler and other tools by running the following command:

```bash
$ cargo build --release
```

Binaries can be found under the `target/release/` directory.

## FAQ

### Why choose to write a compiler for C17?

C17 is a great, challenging language to make a compiler for. It's also
true that one can learn a lot by writing a compiler. That being so,
C17 seems to be an option that gets the most out of the experience.
Just imagine being able to compile whole big projects out there :)

## License

This project is licensed under the MIT license. See LICENSE.
