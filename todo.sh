#!/bin/bash

LIST_LOCATION="$HOME/.todos"

# Define our 'add' sub command
function add {
  for todo in "$@" ; do
    echo "$todo" >> "$LIST_LOCATION"
  done
}

# Define our 'list' sub command
function list {
    if [[ -n "$reverse" ]]; then
        reverser="tac"
    else
        reverser="cat"
    fi

    if [[ -n "$query" ]]; then
      filterer="grep $query"
    else
      filterer="cat"
    fi

  cat -n "$LIST_LOCATION" | \
    $filterer | \
    $reverser
}


source <( (spago run -q <<-'EOF'
- name: add
  description: "Add a todo to the list"
  args:
    - name: todo
      description: "The todo you'd like to add"
      multiple: true
      validators: []
  flags: []
- name: "list"
  description: "List out your existing TODOs"
  args: []
  flags:
    - longName: "reverse"
      shortName: "r"
      description: "Reverse the TODO list"
      multiple: false
      hasArg: false
      validators: []
    - longName: "query"
      shortName: "q"
      description: "List only TODOs containing this text"
      multiple: false
      hasArg: true
      validators: []
EOF
) | tee "out.sh" )
