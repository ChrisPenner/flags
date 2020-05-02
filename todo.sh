#!/usr/local/bin/bash

# set -x
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
