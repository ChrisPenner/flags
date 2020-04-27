# Flags


<!-- toc GFM -->

* [Magic](#magic)
* [Examples](#examples)

<!-- tocstop -->

Flags solves the "how the heck do I parse command line args" problem.

What makes it different from the other 10000 solutions?

The biggest difference is that you can use it with bash scripts and plugins
without requiring ANY dependencies from your users!

Some other bonuses:

- [x] Supports Sub-commands (think 'git log', 'git rebase', etc.)
- [x] Configuration is entirely declarative, no fiddling with args yourself!
- [x] Supports both short and long flags (at the same time) 
- [x] Allows passing multiple of the same flag

Upcoming features:

- [ ] Automatically generates the proper 'help' messages.
- [ ] Yaml config
- [ ] Validate argument types (string, number, file, etc.)
- [ ] Required vs optional arguments
- [ ] Includes easy "include" command for shipping bash scripts
- [ ] Npm install

How do we achieve all this ✨magic✨ you ask?

Easy! Flags is effectively a bash compiler!

## Magic

During development of your script or plugin you'll need the `flags` program locally, 
it parses your configuration and helps you run your program.
When you're ready to ship your script or plugin, you run it in compiler mode,
bundle the generated bash file with your plugin or script (or literally copy-paste it into your script)
then just `source` the compiled bash!

## Examples

Let's say we're writing a todo app, as all programmers are legally obligated to do every 3 months.

Here's how you'd use it:

```bash
$ todo add "Microwave Pizza Pops™"

$ todo add "Finish writing the README"

$ todo list
     1 Finish writing the README
     2 Microwave Pizza Pops™

# List todos in reverse using '-r' short-flag
$ todo list -r
     2 Microwave Pizza Pops™
     1 Finish writing the README

# Search todos using a 'query' long-flag
$ todo list --query Pizza
     2 Microwave Pizza Pops™

# Print help/usage info
$ todo help
```

We can see we've got two sub commands; `add` and `list`.
We've also got a flag option for `list` called `query` which takes an argument.


Here's the source:


```bash
#!/bin/bash

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
    # "reverse" will be true if specified, and blank otherwise
    if $reverse; then
        reverser="tac"
    else
        reverser="cat"
    fi

    # "query" will be populated if provided
    if [[ -n "$query" ]]; then
      filterer="grep $query"
    else
      filterer="cat"
    fi

  cat -n "$LIST_LOCATION" | \
    $filterer | \
    $reverser
}

# Here's where we specify our commands and flags, as well as their help text and descriptions
source <(spago run -q <<-'EOF'
[
   { "name": "add"
   , "description": "Add a todo to the list"
   , "args": [
      { "name": "todo"
      , "description": "The todo you'd like to add"
      , "acceptMultiple": true
      }
    ],
    "flags": []
  },
  { "name": "list"
  , "description": "List out your existing TODOs"
  , "args": []
  , "flags": 
    [ { "longName": "reverse"
      , "shortName": "r"
      , "description" : "Reverse the TODO list"
      , "acceptMultiple": false
      , "hasArg": false
      },
      { "longName": "query"
      , "shortName": "q"
      , "description": "List only TODOs containing this text"
      , "acceptMultiple": false
      , "hasArg": true
      }
    ]
  }
]
EOF
)
```

Notice the `source` command? That's where the magic happens.

`flags` will generate a command line parser from your config, then run the appropriate function
with args available as environment variables of the appropriate name!


You can find this example in [./todo.sh](./todo.sh) .
