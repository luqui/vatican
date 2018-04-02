#!/usr/bin/env zsh

function runtests() {
    set -e
    make
    ./main
    valgrind --leak-check=yes --suppressions=valgrind.supp --error-exitcode=1 ./main
}

if runtests; then
    echo '\e[1;32mPASS\e[0m'
else
    echo '\e[1;31mFAIL\e[0m'
    exit 1
fi
