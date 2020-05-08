#!/bin/bash

testFiles=( simple single )
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
  ./simple show [arg]
  ./simple show-flag  [-f|--flag=<flag>]
  ./simple multiple arg... -f|--flag=<flag>
EOF

  assertEquals "$expected" "$stderr"
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

  assertEquals "$expected" "$stderr" 
}

testTooManyArgs() {
  stderr=$(./simple list arg 2>&1)

read -rd '' msg <<-EOF
Expected 0 arguments but got 1
EOF

  assertContains "$stderr" "$msg" 
}

testTooFewArgs() {
  stderr=$(./simple add 2>&1)

read -rd '' msg <<-EOF
Argument "todo" is required
EOF

  assertContains "$stderr" "$msg" 
}

testPassthroughArgs() {
  stdout=$(./simple show -- --pass)

  expected="--pass"

  assertEquals "$expected" "$stdout" 
}

testDefaultArgs() {
  stdout=$(./simple show)
  expected="default-arg"
  assertEquals "$expected" "$stdout" 
}

testDefaultFlags() {
  stdout=$(./simple show-flag)
  expected="default-flag"
  assertEquals "$expected" "$stdout" 
}

testShortFlag() {
  stdout=$(./simple show-flag -f val)
  expected="val"
  assertEquals "$expected" "$stdout" 
}

testLongFlag() {
  stdout=$(./simple show-flag --flag val)
  expected="val"
  assertEquals "$expected" "$stdout" 
}

testSingleHelp() {
  stderr=$(./single -h  2>&1)

read -rd '' msg <<-EOF
Usage:
  ./single single arg [-f|--flag]

Args:
  arg: An arg
Flags:
  -f, --flag: A flag
EOF

  assertEquals "$msg" "$stderr" 
}








source ./shunit2
