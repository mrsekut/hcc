#!/bin/bash

path="./test/tests"
epath="$path/execs"

try() {
    rm -r $epath
    mkdir $epath
    while read row; do
        index=$(echo ${row} | cut -d , -f1)
        input=$(echo ${row} | cut -d , -f2)
        expected=$(echo ${row} | cut -d , -f3)

        gcc -g main.c $path/tmps/$index.s -o $epath/$index
        actual=$($epath/$index)
        if [ "$actual" = "$expected" ]; then
            echo "$index: $input => $actual"
        else
            echo "$expected expected, but got $actual"
            exit 1
        fi

    done <./test/tests/tests.csv
}

try
echo OK
