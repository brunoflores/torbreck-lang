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
  [@@deriving show]

  type 'a withinfo = { i : info; v : 'a }

  let dummyinfo = DUMMY

  let createInfo fname lnum bol cnum =
    INFO { pos_fname = fname; pos_lnum = lnum; pos_bol = bol; pos_cnum = cnum }

  let printInfo (i : info) =
    match i with
    | INFO { pos_fname; pos_lnum; pos_bol; pos_cnum } ->
        let _ = pos_fname in
        let _ = pos_bol in
        let _ = pos_cnum in
        let lines = Stdio.In_channel.read_lines pos_fname in
        let line = List.nth lines (pos_lnum - 1) in
        print_newline ();
        print_endline "  |";
        Printf.printf "%d | %s\n" pos_lnum line;
        Printf.printf "  |";
        for _ = 0 to pos_bol do
          Printf.printf " "
        done;
        Printf.printf "^";
        print_newline ()
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
