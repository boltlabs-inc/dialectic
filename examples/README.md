# Session-Typed Example Programs Using [Dialectic](https://crates.io/crates/dialectic)

**See also: [Dialectic's online documentation](https://docs.rs/dialectic), which includes a
step-by-step tutorial.**

## What are these?

Each example is a demonstration of a particular different session type and how the two ends of that
session might be implemented using Dialectic. The examples in this repository are all of a
client/server architecture using TCP sockets and binary serialization for communication.

## Reading and editing the examples

The examples live in the `examples` directory, one file per example. Both the server and the client
for that example are defined in the same file, as the functions `server` and `client`. Currently,
the list of examples, in rough order of increasing sophistication, is:

- [`template`](examples/template.rs): The shortest session-typed program: this session performs no
  communication.
- [`hello`](examples/hello.rs): Hello world in session types: a send followed by a receive.
- [`tally`](examples/tally.rs): A more complex example: the server tallies input numbers either by
  summing them or taking their product. This example demonstrates nested loops, offering and
  choosing different protocol options, abstracting out subroutines, and control flow operations.
- [`stack`](examples/stack.rs): A context-free session type: a server which stores a stack of
  strings and pops them when requested, but uppercase. This example demonstrates how session types
  can be used to make it a type error to pop from the empty stack.

## Running the examples

You need a recent Rust compiler to build and run the examples. [Installation instructions for Rust
can be found here](https://www.rust-lang.org/learn/get-started).

Each example builds a single binary which can run either the server or the client for that example
depending on whether you invoke its `server` or `client` subcommand from the shell. To run an
example, first start its server:

```bash
cargo run --quiet --features="bincode" --example some_particular_example -- server
```

Then, in another terminal window, run the client for that example:

```bash
cargo run --quiet --features="bincode" --example some_particular_example -- client
```

You can quit the server by pressing `^C`.

Both the client and server default to connecting to / serving on `127.0.0.1:5000`. This can be
altered using the `--address` and `--port` flags. In particular, if you want to expose the server to
another machine, you'll need to specify the address to bind.

⚠️ **These examples have been built for pedagogy, not engineered for robustness against malicious
input. Running them connected to the open internet might be a bad idea!**

## Creating a new example

Run the script `./new-example` from the project root, using some new example name for
`my_new_example`:

```bash
./new-example my_new_example
```

This will create `examples/my_new_example.rs` by copying `examples/template.rs`.

## Contributing

Pull requests adding new examples are absolutely welcome! Your PR should also modify this
`README.md` file to update the list of examples with a brief description of your new example.

Also encouraged are issues explaining a protocol you'd like to see encoded with session types, even
if you can't figure out how to encode it. We can figure it out together!
