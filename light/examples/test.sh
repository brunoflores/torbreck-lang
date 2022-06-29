#!/usr/bin/env fish

set exit_status 0

dune build ../bin/light.exe
dune build ../bin/linker.exe

set exe_light ../../_build/default/light/bin/light.exe
set exe_linker ../../_build/default/light/bin/linker.exe

set flags "-I ../stdlib"
set stdlib "io.ml exc.ml vect.ml"

rm *.zo *.zi

function echo_with_color -a color header msg err
    echo
    echo -------------------------
    set_color $color; and echo -n $header; and set_color normal
    echo $msg
    echo
    echo $err
    echo -------------------------
    echo
end

function echo_error -a header msg err
    echo_with_color red $header $msg $err
    set exit_status 1
end

function echo_success -a header msg
    set_color green; and echo -n $header; and set_color normal
    echo $msg
end

for test in *.ml
    set tmpf (mktemp)

    eval $exe_light $flags $test 1>/dev/null 2>$tmpf
    if test $status -ne 0
        echo_error "DID NOT COMPILE: " $test (cat $tmpf)
    end

    eval $exe_linker $flags $stdlib $test 1>/dev/null 2>$tmpf
    if test $status -ne 0
        echo_error "DID NOT LINK: " $test (cat $tmpf)
    end
end

for test in single_*.sh
    set fname_out (path change-extension .stdout $test)
    set fname_err (path change-extension .stderr $test)
    set fname_out_exp (path change-extension .stdoutexp $test)
    set fname_err_exp (path change-extension .stderrexp $test)

    bash $test 1>$fname_out 2>$fname_err

    # To promote, uncomment:
    # cp ./"$fname_out" ./"$fname_out_exp"
    # cp ./"$fname_err" ./"$fname_err_exp"

    set diff (diff $fname_out $fname_out_exp)
    if ! test -z "$diff"
        echo_error "FAIL (stdout): " $test $diff
    else
        echo_success "PASS (stdout): " $test
    end

    set diff (diff $fname_err $fname_err_exp)
    if ! test -z "$diff"
        echo_error "FAIL (stderr): " $test $diff
    else
        echo_success "PASS (stderr): " $test
    end
end

exit $exit_status
