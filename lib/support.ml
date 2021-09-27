open Format

module Error = struct
  exception Exit of int

  type info =
    | INFO of {
        pos_fname : string;
        pos_lnum : int;
        pos_bol : int;
        pos_cnum : int;
      }
    | DUMMY

  type 'a withinfo = { i : info; v : 'a }

  let dummyinfo = DUMMY

  let createInfo fname lnum bol cnum =
    INFO { pos_fname = fname; pos_lnum = lnum; pos_bol = bol; pos_cnum = cnum }

  let printInfo (i : info) =
    match i with
    | INFO { pos_fname; pos_lnum; pos_bol; pos_cnum } ->
        Printf.printf "{ %s; %d; %d; %d }" pos_fname pos_lnum pos_bol pos_cnum
    | DUMMY -> Printf.printf "%s" "<Unknown file and line>: "

  let errf f =
    print_flush ();
    open_vbox 0;
    open_hvbox 0;
    f ();
    print_cut ();
    close_box ();
    print_newline ();
    raise (Exit 1)

  let errfAt fi f =
    errf (fun () ->
        printInfo fi;
        print_space ();
        f ())

  let err s =
    errf (fun () ->
        print_string "Error: ";
        print_string s;
        print_newline ())

  let error fi s =
    errfAt fi (fun () ->
        print_string s;
        print_newline ())

  let warning s =
    print_string "Warning: ";
    print_string s;
    print_newline ()

  let warningAt fi s =
    printInfo fi;
    print_string " Warning: ";
    print_string s;
    print_newline ()
end
