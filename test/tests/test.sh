#!/bin/bash

rm -rf tmps
mkdir tmps
while read row; do
    index=$(echo "${row}" | cut -d , -f1)
    input=$(echo "${row}" | cut -d , -f2)

    echo "$index: $input"
    touch ./tmps/$index.s
    stack exec hcc-exe "$input" >./tmps/$index.s
done <tests.csv
