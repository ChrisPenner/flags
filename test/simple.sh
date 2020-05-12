#!/bin/bash

add() {
    echo add
}

list() {
    echo list
}

show() {
    echo "$arg"
}

show_flag() {
    echo "$flag"
}

multiple() {
    echo args:
    for x in "${args[@]}" ; do
            echo $x
    done
    echo flags:
    for x in "${flags[@]}" ; do
            echo $x
    done
}

required_flag() {
    echo 'hi'
}
