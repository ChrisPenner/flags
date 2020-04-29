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

tmpfile="$(mktemp)"
flags config.yaml > "$tmpfile"
source "$tmpfile"

# https://stackoverflow.com/questions/32596123/why-source-command-doesnt-work-with-process-substitution-in-bash-3-2
# source <( flags config.yaml )
