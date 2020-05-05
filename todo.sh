#!/usr/local/bin/flags shebang

LIST_LOCATION="$HOME/.todos"

# Define our 'add' sub command
# Arguments will be provided as expected in "$@"
function add {
  for todo in "$@" ; do
    echo "$todo" >> "$LIST_LOCATION"
  done
}

# Define our 'list' sub command
# Our flags for this sub-command will be parsed and provided as environment variables
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
