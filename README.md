# Atlas

[![ci](https://github.com/feroldi/atlas/actions/workflows/ci.yml/badge.svg)](https://github.com/feroldi/atlas/actions/workflows/ci.yml)
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

## Documentation

Code documentation can be accessed by running the following command:

```bash
$ cargo doc --open
```

There still isn't any other kind of documentation yet, but it's in the radar.

## FAQ

### Why choose to write a compiler for C17?

C17 is a great, challenging language to make a compiler for. It's also
true that one can learn a lot by writing a compiler. That being so,
C17 seems to be an option that gets the most out of the experience.
Just imagine being able to compile whole big projects out there :)

### How much of a standard compliance are we talking about?

People that went through the journey of writing a C compiler failed almost
exclusively.  There are just a few examples of successful and compliant
C compilers out there.  So, that means this project might never have the
glory that I wish it'd have.  Either way, full compliance is just a hard
to achieve dream, and as long as I'm around, I'll be pushing commits to
this repository.

### Why is it written in Rust?

This project was formely built with C++ under the name [`cci`][cci],
which I have discontinued in favor of this one. The reason? I was
just tired of writing C++ code, and, in my opinion, Rust is what C++
reasonably aims to become, but that's a long way. So, why not pick the
better tool for the job that's available right now? Also, it's easier
to write tests, easier to design APIs, easier to manage dependencies,
easier to write documentation. The list goes on.

[cci]: https://github.com/feroldi/cci/

## License

This project is licensed under the MIT license. See LICENSE.
