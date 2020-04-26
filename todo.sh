#!/bin/bash

LIST_LOCATION="$HOME/.todos"

function add {
  echo "$1" >> "$LIST_LOCATION"
}

function list {
    if [[ -n "$reverse" ]]; then
        cat_cmd="tac"
    else
        cat_cmd="cat"
    fi


    if [[ -n "$query" ]]; then
        $cat_cmd -n "$LIST_LOCATION" | grep "$query"
    else
        $cat_cmd cat -n "$LIST_LOCATION"
    fi
}

flags <<'EOF'
add:
  template: 'add %todo'
  help: "Add a todo to the list"
  args:
    todo: "The todo you'd like to add"

list:
  template: 'list -r|--reverse [-q|--query=%query]'
  help: "List out your existing TODOs"
  flags:
    reverse: "Reverse the TODO list"
    query: "List only TODOs containing this text"
EOF
