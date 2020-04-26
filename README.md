# Flags

Flags solves the "how the heck do I parse command line args".

What makes it different from the other 10000 solutions?

The biggest difference is that you can use it with bash scripts and plugins
without requiring ANY dependencies from your users!

Some other bonuses:

* Supports short & long flags
* Supports 'sub commands' (think 'git log', 'git rebase', etc.)
* Automatically generates the proper 'help' messages.
* Configuration is entirely declarative, no fiddling with args yourself!

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
```
