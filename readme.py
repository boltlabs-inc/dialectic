#! /bin/env python3

# This file automatically generates the README.md file for this repository based upon the first
# section of the crate documentation.

main_readme_links = """
[`dialectic-tokio-mpsc`]: https://crates.io/crates/dialectic-tokio-mpsc
[`dialectic-tokio-serde`]: https://crates.io/crates/dialectic-tokio-serde
[`dialectic-tokio-serde-bincode`]: https://crates.io/crates/dialectic-tokio-serde-bincode
[`dialectic-tokio-serde-json`]: https://crates.io/crates/dialectic-tokio-serde-json
[`bincode`]: https://crates.io/crates/bincode
[`serde_json`]: https://crates.io/crates/serde_json
[tutorial-style tour of the crate]: https://docs.rs/dialectic/latest/dialectic/tutorial/index.html
[quick reference]: https://docs.rs/dialectic/latest/dialectic/#quick-reference
[reference documentation]: https://docs.rs/dialectic
[`types`]: https://docs.rs/dialectic/latest/dialectic/types/index.html
[`Chan`]: https://docs.rs/dialectic/latest/dialectic/struct.Chan.html
[`Transmit`]: https://docs.rs/dialectic/latest/dialectic/backend/trait.Transmit.html
[`Receive`]: https://docs.rs/dialectic/latest/dialectic/backend/trait.Receive.html
[`backend`]: https://docs.rs/dialectic/latest/dialectic/backend/index.html
[`Session!`]: https://docs.rs/dialectic/latest/dialectic/macro.Session.html
"""

with open("README.md", "w") as readme:
    readme.write("# Dialectic\n")
    with open("dialectic/src/lib.rs", "r") as lib:
        for line in lib:

            if line.startswith("/*!"):
                line = line[3:]

            if line.endswith("<!-- snip -->\n"):
                break

            readme.write(line)
        readme.write(main_readme_links)

with open("dialectic-compiler/README.md", "w") as readme:
    readme.write("# Dialectic session type macro compiler\n")
    with open("dialectic-compiler/src/lib.rs", "r") as lib:
        for line in lib:
            if line.startswith("/*!"):
                line = line[3:]

            if line.startswith("```text"):
                line = line.replace("```text", "```rust")

            if line.endswith("<!-- snip -->\n"):
                break

            readme.write(line)
