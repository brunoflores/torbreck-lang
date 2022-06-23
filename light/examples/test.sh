#!/usr/bin/env bash

RED=$'\033[0;31m'
GREEN=$'\033[0;32m'
NOCOLOR=$'\033[0m'

exit_status=0

dune build ../bin/light.exe
dune build ../bin/linker.exe

exe_light=../../_build/default/light/bin/light.exe
exe_linker=../../_build/default/light/bin/linker.exe

flags="-I ../stdlib"
stdlib="io.ml exc.ml vect.ml"

# First pass: build
for test in "$(pwd)"/*.ml; do
    $exe_light $flags $test > /dev/null
    $exe_linker $flags $stdlib $test > /dev/null
done

# Second pass: execute test
for test in "$(pwd)"/single_*.sh; do
    fname_out=$(basename "$test" ".sh").stdout
    fname_err=$(basename "$test" ".sh").stderr
    fname_out_exp=$(basename "$test" ".sh").stdoutexp
    fname_err_exp=$(basename "$test" ".sh").stderrexp

    bash "$test" 2> ./"$fname_err" 1> ./"$fname_out"

    # To promote, uncomment:
    # cp ./"$fname_out" ./"$fname_out_exp"
    # cp ./"$fname_err" ./"$fname_err_exp"

    DIFF=$(diff ./"$fname_out" ./"$fname_out_exp")
    if [ "$DIFF" != "" ]; then
        echo ""
        echo "-------------------------"
        printf "%sFAIL (stdout)%s: %s\n" "$RED" "$NOCOLOR" "$test"
        echo "$DIFF"
        echo "-------------------------"
        echo ""

        exit_status=1
    else
        printf "%sPASS (stdout):%s %s\n" "$GREEN" "$NOCOLOR" "$test"
    fi

    DIFF=$(diff ./"$fname_err" ./"$fname_err_exp")
    if [ "$DIFF" != "" ]; then
        echo ""
        echo "-------------------------"
        printf "%sFAIL (stderr)%s: %s\n" "$RED" "$NOCOLOR" "$test"
        echo "$DIFF"
        echo "-------------------------"
        echo ""

        exit_status=1
    else
        printf "%sPASS (stderr):%s %s\n" "$GREEN" "$NOCOLOR" "$test"
    fi
done

exit $exit_status
