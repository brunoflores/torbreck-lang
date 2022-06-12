#!/usr/bin/env bash

RED=$'\033[0;31m'
GREEN=$'\033[0;32m'
NOCOLOR=$'\033[0m'

exit_status=0

dune build ../bin/light.ml
dune build ../bin/linker.ml

exe_light=../../_build/default/light/bin/light.exe
exe_linker=../../_build/default/light/bin/linker.exe

for test in "$(pwd)"/*.ml; do
    fname_out=$(basename "$test" ".ml").stdout
    fname_exp=$(basename "$test" ".ml").stdoutexp
    fname_exec=$(basename "$test" ".ml").out

    $exe_light -I ../stdlib "$test" > /dev/null
    $exe_linker "$test" > /dev/null

    ./"$fname_exec" 2> /dev/null 1> ./"$fname_out"

    # To promote, uncomment:
    # cp ./"$fname_out" ./"$fname_exp"

    DIFF=$(diff ./"$fname_out" ./"$fname_exp")
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
