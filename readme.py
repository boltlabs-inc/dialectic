#! /bin/env python3

# This file automatically generates the README.md file for this repository based upon the first
# section of the crate documentation.

with open("README.md", "w") as readme:
    readme.write("# Dialectic\n\n")
    with open("dialectic/src/lib.rs", "r") as lib:
        for line in lib:
            if line.startswith("/*!"):
                line = line[3:]
            if line.endswith("<!-- snip -->\n"):
                break
            readme.write(line)

with open("dialectic-compiler/README.md", "w") as readme:
    readme.write("# Dialectic session type macro compiler\n\n")
    with open("dialectic-compiler/src/lib.rs", "r") as lib:
        for line in lib:
            if line.startswith("/*!"):
                line = line[3:]

            if line.startswith("```text"):
                line = line.replace("```text", "```rust")

            if line.endswith("<!-- snip -->\n"):
                break

            readme.write(line)
