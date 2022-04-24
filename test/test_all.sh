#!/usr/bin/env bash

RED=$'\033[0;31m'
GREEN=$'\033[0;32m'
NOCOLOR=$'\033[0m'

dune build
exe=./_build/default/bin/main.exe
exit_status=0

for test in ./test/*.f; do
    fname_out=$(basename "$test" ".f").out
    fname_exp=$(basename "$test" ".f").exp

    $exe "$test" 2>/dev/null 1> ./test/"$fname_out"

    # To promote, uncomment:
    # cp ./test/"$fname_out" ./test/"$fname_exp"

    DIFF=$(diff ./test/"$fname_out" ./test/"$fname_exp")
    if [ "$DIFF" != "" ]; then
        echo ""
        echo "-------------------------"
        printf "%sFAIL%s: %s\n" "$RED" "$NOCOLOR" "$test"
        echo "$DIFF"
        echo "-------------------------"
        echo ""

        exit_status=1
    else
        printf "%sPASS:%s %s\n" "$GREEN" "$NOCOLOR" "$test"
    fi
done

exit $exit_status
