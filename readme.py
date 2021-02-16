#! /bin/env python3

# This file automatically generates the README.md file for this repository based upon the first
# section of the crate documentation.

with open("README.md", "w") as readme:
    readme.write("# Dialectic\n\n")
    with open("dialectic/src/lib.rs", "r") as lib:
        for line in lib:
            if len(line) > 4:
                line = line[4:]
            else:
                line = "\n"
            if line.endswith("<!-- snip -->\n"):
                break
            readme.write(line)
