# Contributing to Dialectic

Glad to see you reading this! We really appreciate the help. For simple bug fixes, just submit a PR
with the fix and we can discuss the fix directly in the PR. If the fix is more complex, start with an
issue.

## Getting started

We keep the internals of Dialectic very well-documented. Here are a few places to start:

- The [Dialectic docs themselves](https://docs.rs/dialectic) contain documentation which covers some
  of the internals.
- The session macro compiler implementation (`dialectic-compiler`) has its own
  [readme](https://github.com/boltlabs-inc/dialectic/tree/main/dialectic-compiler/README.md)
  explaining the compilation pipeline.
- For resources on how to implement a backend, you can check out the existing
  [`dialectic-tokio-mpsc`](https://github.com/boltlabs-inc/dialectic/tree/main/dialectic-tokio-mpsc)
  and
  [`dialectic-tokio-serde`](https://github.com/boltlabs-inc/dialectic/tree/main/dialectic-tokio-serde)
  crates, which are pretty readable and well-documented.

## Testing

Testing differs among the various crates. When it comes to the `dialectic` crate itself, "tests"
consist of the examples (`dialectic/examples`), benchmarks (`dialectic/benches`), and a lot of
doc-tests. Since `dialectic` itself contains mostly compile-time complexity, the vast majority of
necessary tests are basically compile-pass. There are exceptions to this, which mostly use the null
backend (the `micro` benchmark) and the [tokio MPSC
backend](https://github.com/boltlabs-inc/dialectic/tree/main/dialectic-tokio-mpsc), which is the
backend used for doc-tests/examples that actually do something. The [tokio serde/bincode
backend](https://github.com/boltlabs-inc/dialectic/tree/main/dialectic-tokio-serde-bincode) is also
used for the examples in the `dialectic/examples` directory.

For the `dialectic-macro` crate, proc macros are tested both inside `dialectic-macro` *and* for the
`Session!` macro *specifically* within the `dialectic-compiler` crate. The difference between these
tests are that the `dialectic-macro` tests cover invocations of the `Session!` macro and other proc
macros, ensuring that they compile; and the `dialectic-compiler` tests cover internals and exposed
machinery of the session macro compiler which is used internally in `dialectic-macro`. This allows
things like the `dialectic-compiler` parser to be rigorously tested without having to go through the
Rust compiler.

## Coding conventions

Please use `rustfmt` to format your code. :)
We have an empty `rustfmt.toml` inside the workspace root; this is intentional, so that your
`rustfmt` install will use the default options (our preference.)

Most dialectic crates contain a number of linting options defined at the crate level, usually
something like this:

```rust
#![warn(missing_docs)]
#![warn(missing_copy_implementations, missing_debug_implementations)]
#![warn(unused_qualifications, unused_results)]
#![warn(future_incompatible)]
#![warn(unused)]
// Documentation configuration
#![forbid(broken_intra_doc_links)]
```

Note that warnings are denied during our CI process, so if you are missing documentation or such,
your PR *will* fail CI. We also use `clippy` in our CI and disallow warnings there as well.

Thank you again for contributing, we really appreciate it! If you have further questions, please
feel free to contact the maintainers: Kenny ([@kwf](https://github.com/kwf) on GitHub / [kwf@boltlabs.io]) or Shea
([@sdleffler](https://github.com/sdleffler) on GitHub / [shea@errno.com]).

## Code of Conduct

Please be kind, courteous, and respectful. This project, although not formally affiliated with the
Rust project, supports the [Rust Code of Conduct](https://www.rust-lang.org/policies/code-of-conduct).
Please report any violations of this code of conduct to
[conduct@boltlabs.io](mailto:conduct@boltlabs.io).
