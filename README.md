# Flags


<!-- toc GFM -->

* [Magic](#magic)
* [Example](#example)
* [Usage](#usage)

<!-- tocstop -->

Flags solves the "how the heck do I parse command line args" problem.

What makes it different from the other 10000 solutions?

1. `flags` parses, validates, and generates help messages for all your arguments and flags from a completely declarative configuration.
2.  You can compile your bash scripts and plugins so your users can enjoy them without ANY required dependencies!

Some other bonuses:

- [x] Supports Sub-commands (think 'git log', 'git rebase', etc.)
- [x] Configuration is entirely declarative, no fiddling with args yourself!
- [x] Supports both short and long flags (at the same time) 
- [x] Allows passing multiple of the same flag
- [x] Yaml config
- [x] Automatically generates the proper 'help' messages.
- [x] Support '--' passthrough flag
- [x] Default flags
- [x] Required vs optional arguments
- [x] Npm install
- [x] Default args
- [x] Supports custom shebang: `#!/usr/local/bin/flags run`

Upcoming features:

- [ ] Validate argument types (string, number, file, etc.)
- [ ] Tab-complete
- [ ] Config init

How do we achieve all this ✨magic✨ you ask?

Easy! Flags just generates bash for you!

## Magic

During development of your script or plugin you'll run the `flags` program locally, 
it parses your configuration and helps you run your program.

When you're ready to ship your script or plugin, simply run `flags build` to activate compiler mode.
It'll bundle generated bash with your plugin or script into a dependency-free bash script you can distribute to your users.

## Example

Let's say we're writing a todo app, as all programmers are legally obligated to do every 3 months.

First off, here's what it looks like to use the app:

```bash
$ todo add "Microwave Pizza Pops™"

$ todo add "Finish writing the README"

$ todo list
     1 Finish writing the README
     2 Microwave Pizza Pops™

# List todos in reverse using an optional '-r' short-flag
$ todo list -r
     2 Microwave Pizza Pops™
     1 Finish writing the README

# Search todos using a 'query' long-flag
$ todo list --query Pizza
     2 Microwave Pizza Pops™

# Print help/usage info
$ todo help
Usage:
  todo <command>

More info:
  todo [command] --help

Commands:
  todo add [todo...]
  todo list  [-r|--reverse] [-q|--query=<query>]
```

We can see we've got three sub commands; `add`, `list`, and `help` (which is generated for you).

We've also got some flag option for `list`. One called `query` which takes an argument, one called `reverse` with a -r short-flag


Here's the whole source:

```bash
#!/usr/local/bin/flags run

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

# We don't need to call any of these functions, 'flags' will pick the right command and run it for us.
```

And we've got a config file at `flags.yaml` which looks like this:

```yaml
- name: add
  description: "Add a todo to the list"
  args:
    - name: todo
      description: "The todos you'd like to add"
      multiple: true
- name: "list"
  description: "List out your existing TODOs"
  flags:
    - longName: "reverse"
      description: "Reverse the TODO list"
    - longName: "query"
      description: "List only TODOs containing this text"
      hasArg: true
```

That's it!

## Usage

There are two main commands in `flags`: `run` and `build`

`run` is a helper for when you're working on your script 
