#!/usr/local/bin/flags shebang

function test {
    echo $a_file
    echo $a_num
    echo $a_path
}

function args {
    echo single: $single
    echo second: $second
    echo multiple: "${multiple[@]}"
}
