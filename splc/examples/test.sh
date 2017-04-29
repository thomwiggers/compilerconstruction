#!/bin/bash

echo "Making sure splc is up to date"
cabal install
if [ $? -ne 0 ]; then
    exit 1;
fi

pretty () { 
    cabal exec splc -- pretty $1
}
prettyi () { 
    cabal exec splc -- pretty -i 
}

code=0

set +e

for f in examples/syntax_pass/*.spl; do
    echo -en "Checking if pretty-printing $f is parseable \t\t"
    if pretty "$f" | prettyi > /dev/null 2>&1; then
        echo "Success"
    else
        code=1
        echo "Failed"
        echo "===== OUTPUT of pretty $f ===="
        pretty $f
        echo "=============================="
        echo

    fi
done

for f in examples/syntax_fail/*.spl; do
    echo -en "Checking if parsing $f fails: \t\t"
    if ! pretty "$f" > /dev/null 2>&1; then
        echo "Success"
    else
        code=1
        echo "Failed"
        echo "==== OUTPUT of pretty $f ====="
        pretty $f
        echo "=============================="
        echo
    fi
done

for f in examples/type_pass/*.spl; do
    echo -en "Checking if infering $f succeeds: \t\t"
    if cabal exec splc -- analysis $f > /dev/null 2>&1; then
        echo "Success"
    else
        code=1
        echo "Failed"
        echo "===== output of analysis of $f ====="
        cabal exec splc -- analysis $f
        echo "===================================="
        echo
    fi
done

for f in examples/type_fail/*.spl; do
    echo -en "Checking if infering $f fails: \t\t"
    if ! cabal exec splc -- analysis $f > /dev/null 2>&1; then
        echo "Success"
    else
        code=1
        echo "Failed"
        echo "===== output of analysis of $f ====="
        cabal exec splc -- analysis $f
        echo "===================================="
        echo
    fi
done

exit $code
