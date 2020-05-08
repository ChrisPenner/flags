# Flags


<!-- toc GFM -->

* [Magic](#magic)
* [Example](#example)
* [Usage](#usage)
  * [Flags Build](#flags-build)
  * [Flags Run](#flags-run)
  * [Flags Run](#flags-run-1)
* [Config](#config)

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
- [x] Init command
- [x] Validate argument types (string, number, file, dir, path)

Upcoming features:

- [ ] Tab-complete
- [ ] Single top-level command

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
    - name: "reverse"
      description: "Reverse the TODO list"
    - name: "query"
      description: "List only TODOs containing this text"
      hasArg: true
```

That's it!

## Usage

There are three main commands in `flags`: `run`, `build`, `init`

### Flags Build

`flags build` is used to compile `flags`' argument handling logic into a simple bash script.

This is useful either when distributing your script to others where `flags` may not be installed. You may also wish to to do this for your scripts locally to gain a teensy bonus on startup time for your script (though `flags` is generally pretty fast).

To compile our `todo` cli we'd run this:

```bash
flags build todo.sh > todo-cli
```

Where `todo-cli` is the location you'd like to write the resulting bash script. You can also optionally provide a `-o todo-cli` if you prefer to write directly rather than redirecting stdout.

The resulting script will include your logic from the original script, as well as all of the argument and flag parsing logic, and will patch the shebang to be `#!/bin/bash`.

### Flags Run

`flags run` is a command for when you're working on your script. 
Pass it a script and some args and `flags` will parse the arguments and run the script.

For our todo-list example it looks like this:

```bash
flags run todo.sh -- list -r
     2 Microwave Pizza Pops™
     1 Finish writing the README
```

You can also embed `flags run` into your script as a *shebang* by add this as your script's first line:

```bash
#!/usr/local/bin/flags run
```

Where `/usr/local/bin/flags` is replaced by the result of running `which flags` on your system.

`flags run` looks for a flags.yaml config in the same directory as the script, but you can specify a config with `-f` if needed. Note that due to limitations of using a *shebang* unfortunately you can't specify any configuration options when using the *shebang* style.

### Flags Run

`flags init` will create a helpful sample `flags.yaml` in the current directory.

## Config

```yaml
# The name of the subcommand
- name: add
  # This description is printed in the help message
  description: "Add a todo to the list"
  # Argument configuration
  args:
      # The name of the argument 
    - name: todo
      # This description is printed in the help message
      description: "The todos you'd like to add"
      # (default: false) Whether multiple values can be provided for this argument
      multiple: false
      # (default: false) Whether the argument is required or optional
      required: false
      # (default: null) A default value for optional arguments
      default: null
# The name of the subcommand
- name: "list"
  # This description is printed in the help message
  description: "List out your existing TODOs"
  # Flags configuration
  flags:
      # (default: first char of long-name)
    - shortName: "r"
      # (required) Both the name of the flag, and the name of the environment variable which it will be bound to.
      # dashes will be replaced with underscores in variable names
      name: "reverse"
      # This description is printed in the help message
      description: "Reverse the TODO list"
      # (default: false) Whether the flag can be provided multiple times
      multiple: false
      # (default: false) Whether the flag takes an argument
      # Variables for flags without arguments will be unset by default
      # and "true" if the arg is provided.
      hasArg: false
    - # (required) Both the name of the flag, and the name of the environment variable which it will be bound to.
      # dashes will be replaced with underscores in variable names
      name: "query"
      # This description is printed in the help message
      description: "List only TODOs containing this text"
      # (default: false) Whether the flag takes an argument
      # Variables for flags without arguments will be unset by default
      # and "true" if the arg is provided.
      hasArg: true
      # Whether the flag is required. Only applies to flags with arguments
      required: false
      # Default value. Only applies to flags with arguments.
      default: null
```
