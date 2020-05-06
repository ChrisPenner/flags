#!/bin/bash

testFiles=( simple )
for src in "${testFiles[@]}" ; do
    flags build "$src.sh" -f "$src.yaml" -o "$src"
    chmod +x "$src"
done

testHelpGeneration() {
  stderr=$(./simple --help 2>&1)

read -rd '' expected <<-EOF
Usage:
  ./simple <command>

More info:
  ./simple [command] --help

Commands:
  ./simple add todo...
  ./simple list  [-r|--reverse] [-q|--query=<query>]
EOF

  assertEquals "$stderr" "$expected"
}

testListHelp() {
  stderr=$(./simple list --help 2>&1)

read -rd '' expected <<-EOF
Usage:
  ./simple list  [-r|--reverse] [-q|--query=<query>]

Flags:
  -r, --reverse: Reverse the TODO list
  -q, --query: List only TODOs containing this text
EOF

  assertEquals "$stderr" "$expected"
}

testTooManyArgs() {
  stderr=$(./simple list arg 2>&1)

read -rd '' expected <<-EOF
Expected 0 arguments but got 1

Usage:
  ./simple <command>

More info:
  ./simple [command] --help

Commands:
  ./simple add todo...
  ./simple list  [-r|--reverse] [-q|--query=<query>]
EOF

  assertEquals "$stderr" "$expected"
}

testTooFewArgs() {
  stderr=$(./simple add 2>&1)

read -rd '' expected <<-EOF
Argument "todo" is required

Usage:
  ./simple <command>

More info:
  ./simple [command] --help

Commands:
  ./simple add todo...
  ./simple list  [-r|--reverse] [-q|--query=<query>]
EOF

  assertEquals "$stderr" "$expected"
}

source ./shunit2
